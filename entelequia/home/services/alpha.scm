(define-module (entelequia home services alpha)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix profiles)                             ; profile, concatenate-manifests
  #:use-module (gnu packages rust-apps)                    ; fd
  #:use-module (gnu packages chromium)                     ; ungoogled-chromium
  #:use-module (guix-agentic agents core)                  ; agent, agent->package/manifest-entries
  #:use-module (guix-agentic guardrails sandbox)           ; sandbox, accessors
  #:use-module (guix-agentic packages skills)              ; make-pi-skill
  #:use-module (alpha-agent agent)                         ; alpha (pinned channel)
  #:use-module (entelequia packages notebooklm-py)
  #:use-module (guix-openclaw packages node-openclaw-deps) ; pi
  #:export (alpha-home-service))

;;; alpha — rafael's personal agent (home service).
;;;
;;; Installs, into rafael's home profile:
;;;   - `pi'  (the coding-agent CLI the launcher execs), and
;;;   - `alpha' — a thin wrapper that injects the OpenRouter key the neutral
;;;     channel expects and execs the real launcher:
;;;       * OPENROUTER_API_KEY <- contents of KEY-FILE (default
;;;         /run/secrets/openrouter/rafael), where *sops-guix* has decrypted the
;;;         key at boot with owner=rafael, mode 0400.
;;;
;;; The model is NOT pinned in the wrapper or settings.json beyond a default;
;;; alpha is the trusted personal agent, so `/model' ranges freely.  The PKS
;;; durable-memory share (~/pks read-write) is folded on by the agent's
;;; with-memory composition, not here.
;;;
;;; System prerequisite: curie's system must run sops-secrets-service-type with
;;;   (sops-secret (key '("openrouter" "rafael")) (file %sops-curie) ...)
;;; so KEY-FILE exists and is readable by rafael.  See system/machines/curie.scm.

(define pi node-earendil-works-pi-coding-agent-0.78.1)

;; notebooklm-py already ships upstream's comprehensive Agent Skill.  Repackage
;; that same immutable file for guix-agentic's pi skill search path rather than
;; maintaining a divergent copy.
(define notebooklm-skill
  (make-pi-skill
   #:name "notebooklm"
   #:skill-md (file-append notebooklm-py
                           "/share/claude-skills/notebooklm/SKILL.md")
   #:synopsis "Create and manage Google NotebookLM notebooks"))

;; Add a purpose-built NotebookLM client instead of handing the LLM an
;; unrestricted GUI browser.  The CLI can create notebooks, add sources, chat,
;; and generate artifacts over NotebookLM's RPC API.  Its authenticated state
;; is produced once by `notebooklm login' on the host and shared read-write so
;; the CLI can refresh cookies.  This is bearer credential material: expose it
;; only to trusted alpha, never to the forager or household agents.
(define alpha-with-notebooklm
  (let ((sb (agent-sandbox alpha)))
    (agent
     (inherit alpha)
     (skills (cons notebooklm-skill (agent-skills alpha)))
     (extra-packages (cons* notebooklm-py
                            ungoogled-chromium
                            (agent-extra-packages alpha)))
     (sandbox
      (sandbox
       (inherit sb)
       ;; The X socket is needed only for the human-mediated login window.
       ;; NotebookLM operations after login remain HTTP-only.
       (share (append '("/home/rafael/.notebooklm"
                        "/tmp/.X11-unix")
                      (sandbox-share sb)))
       (expose (cons "/home/rafael/.Xauthority" (sandbox-expose sb)))
       (preserve
        (append '("^NOTEBOOKLM_HOME$"
                  "^PLAYWRIGHT_"
                  "^DISPLAY$"
                  "^XAUTHORITY$")
                (sandbox-preserve sb))))))))

(define alpha-launcher (agent->package alpha-with-notebooklm))

;; The agent's complete tool closure as a store profile.  Pointing
;; GUIX_ENVIRONMENT at it makes the launcher reuse it inside the L1 container,
;; putting every declared tool and skill search path on the container PATH.
;; `pi' itself is resolved from the home profile at the outer level.
(define alpha-tool-profile
  (profile
   (content
    (packages->manifest
     (cons fd (agent->manifest-entries alpha-with-notebooklm))))))

(define (alpha-wrapper key-file)
  "An executable `alpha' that reads the OpenRouter key from KEY-FILE
(sops-decrypted local file) and execs the alpha launcher."
  (program-file
   "alpha"
   #~(begin
       (use-modules (ice-9 rdelim))
       (let ((key-file #$key-file))
         ;; sops-guix decrypts the key to KEY-FILE at boot (owner = rafael,
         ;; mode 0400).  Read it at launch; it never lands anywhere else.
         (when (file-exists? key-file)
           (call-with-input-file key-file
             (lambda (p)
               (let ((k (read-line p)))
                 (when (string? k) (setenv "OPENROUTER_API_KEY" k))))))
         ;; Point denotecli's default --dirs at the PKS so a flag-less
         ;; `denotecli search' the agent runs hits ~/pks (not the upstream ~/org
         ;; default).  alpha's sandbox preserves DENOTECLI_DIRS into the L1
         ;; container; the vendored denotecli reads it (see alpha-agent
         ;; denotecli.scm snippet).
         (setenv "DENOTECLI_DIRS" "/home/rafael/pks")
         ;; guix shell requires bind-mount sources to exist before startup.
         ;; Keep NotebookLM's bearer credentials private to the user.
         (let ((notebooklm-home "/home/rafael/.notebooklm"))
           (unless (file-exists? notebooklm-home)
             (mkdir notebooklm-home #o700))
           (setenv "NOTEBOOKLM_HOME" notebooklm-home))
         ;; notebooklm-py's Playwright patch uses this Guix browser instead of
         ;; looking for Playwright's impure, CDN-downloaded browser bundle.
         (setenv "PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD" "1")
         (setenv "PLAYWRIGHT_CHROMIUM_EXECUTABLE"
                 #$(file-append ungoogled-chromium "/bin/chromium"))
         ;; Hand the launcher a ready-built tool profile to reuse inside the
         ;; container, so rg/fd/denotecli are on PATH and nothing is downloaded.
         (unless (getenv "GUIX_ENVIRONMENT")
           (setenv "GUIX_ENVIRONMENT" #$alpha-tool-profile))
         (apply execl #$(file-append alpha-launcher "/bin/alpha")
                "alpha" (cdr (command-line)))))))

(define (alpha-cli key-file)
  "A package placing the wrapper at bin/alpha (so it lands on PATH via the
home profile)."
  (package
    (name "alpha-cli")
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list #:modules '((guix build utils))
           #:builder
           #~(begin
               (use-modules (guix build utils))
               (mkdir-p (string-append #$output "/bin"))
               (copy-file #$(alpha-wrapper key-file)
                          (string-append #$output "/bin/alpha"))
               (chmod (string-append #$output "/bin/alpha") #o755))))
    (synopsis "alpha launcher wrapper (injects the OpenRouter key)")
    (description "Wrapper that injects the OpenRouter key (from the
sops-decrypted local file) then execs the alpha launcher.")
    (home-page "https://github.com/RafaelPalomar/alpha-agent")
    (license license:gpl3+)))

(define* (alpha-home-service #:key (key-file "/run/secrets/openrouter/rafael"))
  "Return the home services that deploy alpha: `pi' and the `alpha' wrapper
(reading the OpenRouter key from KEY-FILE), both into the home profile.  The
agent's own tools (including notebooklm, rg, fd, and denotecli) remain in its
private profile; the wrapper hands that profile to the sandbox via
GUIX_ENVIRONMENT (see `alpha-tool-profile')."
  (list
   (simple-service 'alpha
                   home-profile-service-type
                   (list pi (alpha-cli key-file)))))
