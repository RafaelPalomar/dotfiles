(define-module (entelequia packages opencode)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages elf)
  #:use-module (nonguix build-system binary))

(define-public opencode
  (package
    (name "opencode")
    (version "1.15.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/opencode-linux-x64"
             "/-/opencode-linux-x64-" version ".tgz"))
       (sha256
        (base32 "0b2b2kmvf1hc6pbq8h0n6l1i058mrw5whfcmrsfalfyrqh7n511v"))))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan
       ;; Keep the binary basename `opencode` — like Claude Code, this is a
       ;; Bun single-file executable, and the Bun runtime checks
       ;; /proc/self/exe.
       '(("bin/opencode" "libexec/opencode/opencode"))
       #:validate-runpath? #f
       #:strip-binaries? #f            ; stripping corrupts the Bun SEA payload
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'patch-interpreter
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/libexec/opencode/opencode"))
                    (ld (string-append (assoc-ref inputs "glibc")
                                       ,(glibc-dynamic-linker))))
               (invoke "patchelf" "--set-interpreter" ld bin))))
         (add-after 'patch-interpreter 'create-wrapper
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin-dir (string-append out "/bin"))
                    (wrapper (string-append bin-dir "/opencode"))
                    (real (string-append out "/libexec/opencode/opencode")))
               (mkdir-p bin-dir)
               (call-with-output-file wrapper
                 (lambda (port)
                   (format port "#!/bin/sh~%")
                   ;; Stop opencode self-updating into a writable cache dir
                   ;; that would shadow this declaratively-managed package.
                   (format port "export OPENCODE_DISABLE_AUTOUPDATE=1~%")
                   (format port "export OPENCODE_ENABLE_EXA=1~%")
                   (format port "exec ~a \"$@\"~%" real)))
               (chmod wrapper #o755)))))))
    (inputs (list glibc))
    (native-inputs (list patchelf))
    (supported-systems '("x86_64-linux"))
    (home-page "https://opencode.ai")
    (synopsis "AI coding agent for the terminal")
    (description
     "opencode is an AI coding agent built for the terminal.  It provides a
TUI client for working with large language models against your codebase,
supporting multiple providers and a client/server architecture.")
    (license license:expat)))
