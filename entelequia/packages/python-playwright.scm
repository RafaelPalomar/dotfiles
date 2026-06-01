(define-module (entelequia packages python-playwright)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system pyproject)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages node)
  #:use-module (gnu packages python-xyz))

;;; python-playwright — Playwright for Python on Guix.
;;;
;;; WHY A BINARY REPACKAGE (not a source build):
;;; Playwright's source build downloads a prebuilt "driver" bundle (a vendored
;;; Node.js runtime + the Playwright JS package) from a Microsoft CDN during
;;; setup — network access that Guix's build sandbox forbids.  The platform
;;; manylinux wheel already CONTAINS that bundle (~47 MB), so we install the
;;; wheel directly.
;;;
;;; NODE — the wheel's vendored `driver/node' is an ancient manylinux1 build
;;; that segfaults under modern glibc, and patchelf'ing a Node binary is
;;; fragile.  Instead we replace it with a symlink to the Guix-packaged
;;; @code{node}; Playwright's driver is plain JS (driver/package/cli.js) and
;;; runs fine on Guix's Node 22.  (Playwright also honours
;;; @env{PLAYWRIGHT_NODEJS_PATH}, but the symlink means no env var is needed.)
;;;
;;; BROWSERS — Playwright normally runs `playwright install' to download
;;; prebuilt Chromium/Firefox/WebKit binaries (foreign + impure).  We suppress
;;; that (@env{PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1}, @env{PLAYWRIGHT_BROWSERS_PATH=0})
;;; and drive the Guix-packaged @code{ungoogled-chromium}.  Because Playwright
;;; cannot auto-discover a browser it didn't install, consumer code must pass an
;;; explicit @code{executable_path} at launch, e.g.:
;;;
;;;   from playwright.sync_api import sync_playwright
;;;   import os
;;;   with sync_playwright() as p:
;;;       b = p.chromium.launch(
;;;               executable_path=os.environ["PLAYWRIGHT_CHROMIUM_EXECUTABLE"])
;;;
;;; The development home profile exports @env{PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD}
;;; and @env{PLAYWRIGHT_CHROMIUM_EXECUTABLE} (= <ungoogled-chromium>/bin/chromium)
;;; so this contract holds for interactive sessions.
;;;
;;; CAVEAT — Playwright pins an exact Chromium revision and patches it; stock
;;; ungoogled-chromium is a different build, so the newer headless mode and a
;;; few protocol features may diverge.  Adequate for scripted navigation and
;;; cookie/session capture (e.g. a NotebookLM login); not guaranteed for the
;;; full Playwright test matrix.

(define-public python-playwright
  (package
    (name "python-playwright")
    (version "1.60.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/py3/p/playwright/"
             "playwright-" version "-py3-none-manylinux1_x86_64.whl"))
       (sha256
        (base32 "0gn92kgvpb02yzgg3whna1754g9liymhwaakp02zpcsgi3kzlaqw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; The driver bundle carries prebuilt foreign binaries we don't
          ;; relink; their RUNPATH is irrelevant.
          (delete 'validate-runpath)
          ;; Source is a prebuilt wheel: drop it where the install phase looks
          ;; (dist/) instead of unpacking a tarball, and skip the build.
          (replace 'unpack
            (lambda _
              (mkdir-p "dist")
              (copy-file
               #$source
               "dist/playwright-1.60.0-py3-none-manylinux1_x86_64.whl")))
          (delete 'build)
          ;; Replace the broken vendored Node with Guix's node.
          (add-after 'install 'use-guix-node
            (lambda _
              (let ((node-file (car (find-files #$output
                                                (lambda (f _)
                                                  (string=? (basename f) "node"))))))
                (delete-file node-file)
                (symlink #$(file-append node "/bin/node") node-file))))
          ;; Bake the no-download policy into the CLI entry point.
          (add-after 'create-entrypoints 'wrap-cli
            (lambda _
              (let ((pw (string-append #$output "/bin/playwright")))
                (when (file-exists? pw)
                  (wrap-program pw
                    '("PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD" = ("1"))
                    '("PLAYWRIGHT_BROWSERS_PATH" = ("0"))))))))))
    (inputs (list node))
    (propagated-inputs
     (list python-greenlet python-pyee))
    (home-page "https://playwright.dev/python/")
    (synopsis "Playwright browser automation for Python (Guix-repackaged wheel)")
    (description
     "Playwright is a library for automating Chromium, Firefox and WebKit with a
single API.  This package installs the upstream platform wheel and runs its
JavaScript driver on the Guix @code{node} package rather than the wheel's
incompatible vendored Node binary.

Browser auto-download is disabled; drive the Guix @code{ungoogled-chromium}
package by passing @code{executable_path} at launch.  The development home
profile exports @env{PLAYWRIGHT_CHROMIUM_EXECUTABLE} for this purpose.

Use on demand rather than installing into a shared profile:
@code{guix shell python-playwright -- python script.py}.  It is kept OUT of
the development home profile because its python closure (greenlet/pyee ->
typing-extensions) collides with the guix-hermes channel's vendored python
rebuild on hosts that also run hermes-agent.")
    (supported-systems '("x86_64-linux"))
    (license license:asl2.0)))
