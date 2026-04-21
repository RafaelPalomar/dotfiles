(define-module (entelequia packages mermaid-cli)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (gnu packages base)
  #:use-module (gnu packages node)
  #:use-module (nonguix build-system binary))

(define-public mermaid-cli
  (package
    (name "mermaid-cli")
    (version "11.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/RafaelPalomar/dotfiles/releases/download/"
             "vendor-mmdc-" version "/"
             "mmdc-bundle-" version ".tar.gz"))
       (sha256
        (base32 "07xpgwfxwz7cph9apdps1qn27l4wvh5q9kbngrnixa457bi8ilaj"))))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan
       '(("node_modules" "lib/mermaid-cli/node_modules"))
       #:validate-runpath? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (invoke "tar" "-xzf" source)))
         (add-after 'install 'create-wrapper
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (node (string-append (assoc-ref inputs "node") "/bin/node"))
                    (cli (string-append
                          out
                          "/lib/mermaid-cli/node_modules"
                          "/@mermaid-js/mermaid-cli/src/cli.js")))
               (mkdir-p bin)
               (call-with-output-file (string-append bin "/mmdc")
                 (lambda (port)
                   ;; Chromium is resolved at runtime. Honor a pre-set
                   ;; PUPPETEER_EXECUTABLE_PATH; otherwise probe PATH for
                   ;; chromium, then google-chrome, then fall back to the
                   ;; system profile path. PUPPETEER_SKIP_DOWNLOAD prevents
                   ;; any attempt to fetch a bundled browser.
                   (format port
                           "#!/bin/sh~%\
: \"${PUPPETEER_EXECUTABLE_PATH:=$(command -v chromium \
|| command -v google-chrome \
|| echo /run/current-system/profile/bin/chromium)}\"~%\
export PUPPETEER_SKIP_DOWNLOAD=1 PUPPETEER_EXECUTABLE_PATH~%\
exec ~a ~a \"$@\"~%"
                           node cli)))
               (chmod (string-append bin "/mmdc") #o755)))))))
    (inputs (list glibc node))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/mermaid-js/mermaid-cli")
    (synopsis "Command-line interface for mermaid diagram rendering")
    (description
     "mermaid-cli (@command{mmdc}) renders Mermaid diagram definitions from
the command line to SVG, PNG, or PDF output.  It drives a headless Chromium
browser via Puppeteer.  Chromium is resolved at runtime from the system
profile (@file{/run/current-system/profile/bin/chromium}), so it does not
need to be a build-time input.

The vendor bundle (node_modules) is pre-packed with
@code{PUPPETEER_SKIP_DOWNLOAD=1} so no additional network access is required
at build time.")
    (license license:expat)))
