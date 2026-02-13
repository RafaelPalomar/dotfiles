(define-module (entelequia packages entelequia-docs)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages sphinx)
  #:use-module ((guix licenses) #:prefix license:))

(use-modules (guix build utils))

(define-public entelequia-docs
  (package
    (name "entelequia-docs")
    (version "1.0.0")
    (source (local-file "../../docs"
                        "entelequia-docs-source"
                        #:recursive? #t))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f  ; No tests for documentation
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)  ; No configure script needed
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Build Texinfo sources first
              (invoke "make" "texinfo")
              ;; Build Info manual from Texinfo
              (invoke "make" "info")
              ;; Build HTML documentation
              (invoke "make" "html")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (info-dir (string-append out "/share/info"))
                     (html-dir (string-append out "/share/doc/entelequia/html")))
                ;; Install Info manual
                (mkdir-p info-dir)
                (install-file "build/texinfo/entelequia.info" info-dir)
                ;; Register with Info directory
                (invoke "install-info"
                        "--info-dir" info-dir
                        (string-append info-dir "/entelequia.info"))
                ;; Install HTML documentation
                (mkdir-p html-dir)
                (copy-recursively "build/html" html-dir)))))))
    (native-inputs
     (list python-sphinx
           texinfo))
    (home-page "https://github.com/rafael/dotfiles")
    (synopsis "Documentation for Entelequia Guix dotfiles system")
    (description
     "Comprehensive documentation for the Entelequia declarative dotfiles
system built with GNU Guix.  Includes system architecture, configuration
patterns, security hardening, GPG infrastructure, and troubleshooting guides.
Provides both Info manual (for Emacs integration via @kbd{C-h i}) and HTML
output.")
    (license license:gpl3+)))
