(define-module (entelequia packages notebooklm-py)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system pyproject)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages markup))

;;; notebooklm-py — unofficial Python CLI/library + Claude Code skill for
;;; Google NotebookLM.
;;;
;;; Upstream: https://github.com/teng-lin/notebooklm-py
;;;
;;; The core driver is a pure-httpx RPC client against NotebookLM's internal
;;; Boq `batchexecute' endpoints; ALL operations (create / chat / sources /
;;; audio / video / slides / mindmaps / data-tables) run over httpx once a
;;; Google `storage_state.json' (session cookies) is present.
;;;
;;; AUTH ON GUIX — the upstream `[browser]' extra (Playwright) and `[cookies]'
;;; extra (rookiepy) are DELIBERATELY OMITTED:
;;;   * python-playwright is not in Guix and `playwright install chromium'
;;;     downloads a foreign prebuilt browser binary (impure; won't run on Guix
;;;     unpatched) — see the Playwright note in docs/ and the development
;;;     profile commentary.
;;;   * python-rookiepy is not in Guix (Rust/maturin extension).
;;; Neither is needed: `notebooklm login --browser-cookies firefox::none'
;;; (or `firefox::<container>') reads cookies directly from LibreWolf/Firefox's
;;; UNENCRYPTED `cookies.sqlite' via the stdlib `sqlite3' reader in
;;; cli/_firefox_containers.py, bypassing both deps.  Do the one-time Google
;;; sign-in by hand in LibreWolf (human keystroke), then extract.
;;;
;;; The SKILL.md (Claude Code agent skill) ships inside the wheel at
;;; notebooklm/data/SKILL.md and is also installed to
;;; share/claude-skills/notebooklm/SKILL.md so the desktop-suite
;;; claude-skills-files home service can link it into ~/.claude/skills/.

(define-public notebooklm-py
  ;; main HEAD at the audited tree (pyproject version 0.7.0; latest tag is
  ;; v0.6.0 — 0.7.0 is not yet tagged upstream).
  (let ((commit "ffc3dfb1a3ce8f060b3c2ab135586b035f224e22")
        (revision "0"))
    (package
      (name "notebooklm-py")
      (version (git-version "0.7.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/teng-lin/notebooklm-py")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1kvng1s4sgqc1wy36v88szrbppq5kl7i67dnnj2brlmk351gi5sy"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        ;; Test deps (pytest-httpx, vcrpy, pytest-rerunfailures, …) are not all
        ;; packaged in Guix and the e2e suite needs live Google auth.
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            ;; Expose the agent skill where the home `claude-skills-files'
            ;; service expects it (share/claude-skills/<name>/SKILL.md).
            (add-after 'install 'install-claude-skill
              (lambda _
                (let ((dst (string-append #$output
                                          "/share/claude-skills/notebooklm")))
                  (mkdir-p dst)
                  (copy-file "SKILL.md" (string-append dst "/SKILL.md"))))))))
      (native-inputs
       (list python-hatchling
             python-hatch-fancy-pypi-readme))
      ;; Runtime deps are REGULAR inputs, not propagated: the pyproject `wrap'
      ;; phase bakes them into the `notebooklm' launcher's GUIX_PYTHONPATH, so
      ;; the CLI is self-contained and NOTHING leaks into the home profile.
      ;; This is deliberate — the guix-hermes channel (hermes-agent) rebuilds
      ;; the entire python closure (its python-httpx propagates its own anyio/
      ;; certifi/httpcore/…), so a propagated stock python-httpx@0.28.1 would
      ;; collide at profile level with hermes's divergent build of the same
      ;; version.  Keeping these private sidesteps that for httpx/click/rich/…
      (inputs
       (list python-httpx
             python-click
             python-rich
             python-filelock
             ;; `[markdown]' extra: cleaner web-source ingestion. Cheap, pure
             ;; Python, already in Guix — kept so `source add' handles HTML well.
             python-markdownify))
      (home-page "https://github.com/teng-lin/notebooklm-py")
      (synopsis "Unofficial Python CLI/library and Claude skill for NotebookLM")
      (description
       "notebooklm-py provides full programmatic access to Google NotebookLM
via a reverse-engineered httpx RPC client: create and manage notebooks, add
sources (URL/PDF/YouTube/Drive), chat with source-grounded citations, and
generate studio artifacts (audio overviews, video, slides, mind maps,
infographics, data tables, reports).  It installs the @command{notebooklm}
command-line tool and a Claude Code @file{SKILL.md} agent skill.

This package omits the upstream Playwright (@code{[browser]}) and rookiepy
(@code{[cookies]}) extras, which are unpackaged on Guix; authenticate instead
with @command{notebooklm login --browser-cookies firefox::none}, which reads
session cookies from LibreWolf/Firefox via the standard-library
@code{sqlite3} reader.  Drives Google's undocumented endpoints, so usage is
subject to NotebookLM's Terms of Service and may break on upstream changes;
prefer a dedicated Google account.")
      (license license:expat))))
