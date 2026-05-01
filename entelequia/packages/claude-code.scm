(define-module (entelequia packages claude-code)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages elf)
  #:use-module (nonguix build-system binary))

(define-public claude-code
  (package
    (name "claude-code")
    (version "2.1.116")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/@anthropic-ai/claude-code-linux-x64"
             "/-/claude-code-linux-x64-" version ".tgz"))
       (sha256
        (base32 "1sphicyqipc16r1734phdgsra2p94dhl54hsfms73vlcd6659phd"))))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan
       ;; Keep the binary basename `claude` — Bun SEA checks /proc/self/exe
       ;; and falls back to a plain Bun runtime if the basename doesn't match.
       '(("claude" "libexec/claude-code/claude"))
       #:validate-runpath? #f
       #:strip-binaries? #f            ; stripping corrupts Bun SEA payload
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'patch-interpreter
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/libexec/claude-code/claude"))
                    (ld (string-append (assoc-ref inputs "glibc")
                                       ,(glibc-dynamic-linker))))
               (invoke "patchelf" "--set-interpreter" ld bin))))
         (add-after 'patch-interpreter 'create-wrapper
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin-dir (string-append out "/bin"))
                    (wrapper (string-append bin-dir "/claude"))
                    (real (string-append out "/libexec/claude-code/claude")))
               (mkdir-p bin-dir)
               (call-with-output-file wrapper
                 (lambda (port)
                   (format port "#!/bin/sh~%")
                   ;; DISABLE_AUTOUPDATER stops claude from self-downloading
                   ;; into ~/.local/share/claude, which shadows this package
                   ;; via ~/.local/bin on PATH.
                   (format port "export DISABLE_AUTOUPDATER=1~%")
                   (format port "exec ~a \"$@\"~%" real)))
               (chmod wrapper #o755)))))))
    (inputs (list glibc))
    (native-inputs (list patchelf))
    (supported-systems '("x86_64-linux"))
    (home-page "https://claude.ai/code")
    (synopsis "Anthropic's official CLI for Claude")
    (description
     "Claude Code is an agentic coding tool that lives in your terminal,
understands your codebase, and helps you code faster through natural language
commands.  It integrates with the Anthropic API.")
    (license (license:non-copyleft
              "https://github.com/anthropics/claude-code/blob/main/LICENSE"))))
