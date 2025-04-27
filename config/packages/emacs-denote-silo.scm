(define-module (config packages emacs-denote-silo)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (guix download)
  #:use-module (guix build-system emacs)
  #:use-module ((gnu packages emacs-xyz) #:prefix upstream:)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-denote-silo
  (package
    (name "emacs-denote-silo")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/denote-silo-"
                           version ".tar"))
       (sha256
        (base32 "1jxr52npjiwisambwav6rasndjdxhll8x278q8cr7giq71am7c8b"))))
    (build-system emacs-build-system)
    (propagated-inputs (list upstream:emacs-denote))
    (home-page "https://github.com/protesilaos/denote-silo")
    (synopsis "Convenience functions for using Denote in multiple silos")
    (description "Package for working with different denote silos")
    (license license:gpl3+)))
