(define-module (entelequia packages notebooklm-py)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system pyproject)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages markup)
  #:use-module (entelequia packages python-playwright))

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
;;; AUTH ON GUIX — two paths, neither needing the upstream `[cookies]' extra
;;; (rookiepy; a Rust/maturin ext not in Guix):
;;;
;;;   1. Interactive Playwright login (primary).  We add entelequia's
;;;      `python-playwright' as a private input (baked into the launcher's
;;;      GUIX_PYTHONPATH, NOT propagated — so no profile collision) and patch
;;;      cli/services/playwright_login.py so that when
;;;      @env{PLAYWRIGHT_CHROMIUM_EXECUTABLE} is set (desktop-suite exports it =
;;;      ungoogled-chromium) the bundled-chromium path launches THAT browser via
;;;      executable_path and skips the CDN `playwright install' download.
;;;      Upstream offers only bundled chromium (download) or a branded `channel'
;;;      (chrome/edge, hardcoded to /opt/google/chrome — never PATH), so neither
;;;      finds a Guix browser without this patch.  Then `notebooklm login' opens
;;;      ungoogled-chromium; the user signs into Google once (human keystroke).
;;;
;;;   2. Browser-cookie extraction (fallback, no Playwright):
;;;      `notebooklm login --browser-cookies firefox::none' reads Firefox's
;;;      UNENCRYPTED `cookies.sqlite' via the stdlib `sqlite3' reader in
;;;      cli/_firefox_containers.py.  Needs an actual Firefox profile on disk.
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
            ;; Teach the Playwright login to drive a Guix browser: when
            ;; PLAYWRIGHT_CHROMIUM_EXECUTABLE is set, launch it via
            ;; executable_path and skip the CDN `playwright install' download.
            (add-after 'unpack 'patch-playwright-executable-path
              (lambda _
                (let ((f "src/notebooklm/cli/services/playwright_login.py"))
                  ;; Skip the bundled-chromium download check when we supply
                  ;; our own browser.
                  (substitute* f
                    (("    if browser == \"chromium\":")
                     (string-append
                      "    if browser == \"chromium\" and not "
                      "os.environ.get(\"PLAYWRIGHT_CHROMIUM_EXECUTABLE\"):")))
                  ;; Inject executable_path for the bundled-chromium path.
                  (substitute* f
                    (("            launch_kwargs\\[\"channel\"\\] = browser\n")
                     (string-append
                      "            launch_kwargs[\"channel\"] = browser\n"
                      "        elif os.environ.get"
                      "(\"PLAYWRIGHT_CHROMIUM_EXECUTABLE\"):\n"
                      "            launch_kwargs[\"executable_path\"] = "
                      "os.environ[\"PLAYWRIGHT_CHROMIUM_EXECUTABLE\"]\n")))
                  ;; The event-based `page.wait_for_url' login wait pins its
                  ;; glob to get_base_url() (notebooklm.google.com), but after a
                  ;; successful SSO Google lands the browser on the rebranded
                  ;; host notebook.google.com (see the next substitution), so
                  ;; the wait never matches and blocks until timeout, never
                  ;; persisting storage_state.json.  NotebookLM is also a
                  ;; streaming SPA whose document is slow to reach `load'.
                  ;; Resolve `goto' on navigation commit and replace the wait
                  ;; with a `url_matches_base_host(page.url)' polling loop that
                  ;; accepts either landing host.
                  (substitute* f
                    (("page.goto\\(f\"\\{get_base_url\\(\\)\\}/\", timeout=30000\\)")
                     "page.goto(f\"{get_base_url()}/\", wait_until=\"commit\", timeout=30000)")
                    (("page.wait_for_url\\(f\"\\{get_base_url\\(\\)\\}/\\*\\*\", timeout=300_000\\)")
                     (string-append
                      "_login_deadline = time.time() + 300\n"
                      "                    while not "
                      "url_matches_base_host(page.url):\n"
                      "                        if time.time() > "
                      "_login_deadline:\n"
                      "                            raise PlaywrightTimeout("
                      "\"Login not detected within 5 minutes\")\n"
                      "                        time.sleep(1)")))
                  ;; Google rebranded NotebookLM's web UI to
                  ;; `notebook.google.com' ("Gemini Notebook"): after a
                  ;; successful SSO the browser lands there, not on
                  ;; `notebooklm.google.com'.  url_matches_base_host gates all
                  ;; three login checkpoints (already-logged-in, post-login
                  ;; detection, defense-in-depth), so broaden it to accept the
                  ;; new host -- otherwise login is never detected and
                  ;; storage_state.json is never written.  The captured cookies
                  ;; are .google.com, so the httpx RPC client keeps working.
                  (substitute* f
                    (("    return current_host == get_base_host\\(\\).lower\\(\\)")
                     (string-append
                      "    return current_host in "
                      "(get_base_host().lower(), \"notebook.google.com\")"))))))
            ;; Expose the agent skill where the home `claude-skills-files'
            ;; service expects it (share/claude-skills/<name>/SKILL.md).
            (add-after 'install 'install-claude-skill
              (lambda _
                (let ((dst (string-append #$output
                                          "/share/claude-skills/notebooklm")))
                  (mkdir-p dst)
                  (copy-file "SKILL.md" (string-append dst "/SKILL.md")))))
            ;; A stray pip-installed ~/.local/.../playwright (with the broken
            ;; vendored node) must not shadow the Guix module baked into the
            ;; launcher's GUIX_PYTHONPATH.
            (add-after 'wrap 'disable-user-site
              (lambda _
                (wrap-program (string-append #$output "/bin/notebooklm")
                  '("PYTHONNOUSERSITE" = ("1"))))))))
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
             python-markdownify
             ;; `[browser]' extra: enables interactive `notebooklm login' via
             ;; the patched executable_path path above.  Private input (baked
             ;; into the launcher, not propagated) so the playwright/node/
             ;; greenlet/pyee/typing-extensions closure never reaches the
             ;; profile — same collision-avoidance rationale as the deps above.
             python-playwright))
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
