(define-module (entelequia packages claude-code)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (gnu packages base)
  #:use-module (gnu packages node)
  #:use-module (nonguix build-system binary))

(define-public claude-code
  (package
    (name "claude-code")
    (version "2.1.87")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-"
             version ".tgz"))
       (sha256
        (base32 "18lnqln4piwjdv7aky3fswsf1ccrb2hmnpm0qcaf9dr3s8jchmax"))))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan
       ;; Install the bundled CLI and vendored binaries, linux/x64 only
       '(("cli.js" "lib/claude-code/cli.js")
         ("package.json" "lib/claude-code/package.json")
         ;;("resvg.wasm" "lib/claude-code/resvg.wasm")
         ("vendor/ripgrep/x64-linux/" "lib/claude-code/vendor/ripgrep/x64-linux/")
         ;;("vendor/tree-sitter-bash/x64-linux/" "lib/claude-code/vendor/tree-sitter-bash/x64-linux/")
         )
       #:validate-runpath? #f        ; pre-built binaries, skip RUNPATH check
       #:phases
       (modify-phases %standard-phases

         (add-after 'install 'create-wrapper
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (node (string-append (assoc-ref inputs "node") "/bin/node"))
                    (cli (string-append out "/lib/claude-code/cli.js")))
               (mkdir-p bin)
               (call-with-output-file (string-append bin "/claude")
                 (lambda (port)
                   ;; DISABLE_AUTOUPDATER prevents claude from trying to
                   ;; update itself, which would fail against the immutable store.
                   (format port "#!/bin/sh~%DISABLE_AUTOUPDATER=1 exec ~a ~a \"$@\"~%"
                           node cli)))
               (chmod (string-append bin "/claude") #o755)))))))
    ;; glibc is needed so binary-build-system's patchelf phase can set the
    ;; correct ELF interpreter on the bundled ripgrep binary and ripgrep.node
    ;; native addon.
    (inputs (list glibc node))
    (supported-systems '("x86_64-linux"))
    (home-page "https://claude.ai/code")
    (synopsis "Anthropic's official CLI for Claude")
    (description
     "Claude Code is an agentic coding tool that lives in your terminal,
understands your codebase, and helps you code faster through natural language
commands.  It bundles ripgrep for code search and integrates with the
Anthropic API.")
    (license (license:non-copyleft
              "https://github.com/anthropics/claude-code/blob/main/LICENSE"))))
