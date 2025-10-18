(define-module (config packages emacs-ob-mermaid)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((gnu packages emacs-xyz) #:prefix upstream:)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-ob-mermaid
  (package
    (name "emacs-ob-mermaid")
    (version "20250621.1655")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/arnm/ob-mermaid.git")
             (commit "372c2d91d3cdba5da9f7ac23e7bce7a0b3b46862")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bvg7xgm9ph7hbkgzm145ifxi833rg6lamanngqq564n7d0l4ng0"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/arnm/ob-mermaid")
    (synopsis "Org-babel support for mermaid evaluation")
    (description
     "Org-Babel support for evaluating mermaid diagrams.  Supported header arguments:
:file - Output file (required) :theme - Mermaid theme :width, :height - Diagram
dimensions :scale - Scale factor :background-color - Background color
:mermaid-config-file - Mermaid config file :css-file - CSS file for styling
:puppeteer-config-file - Puppeteer config file :pdf-fit - Enable PDF fit mode
:cmdline - Additional command line arguments.")
    (license #f)))
