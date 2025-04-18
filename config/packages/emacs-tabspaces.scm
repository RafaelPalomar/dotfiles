(define-module (config packages emacs-tabspaces)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((gnu packages emacs-xyz) #:prefix upstream:)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-tabspaces
  (package
    (name "emacs-tabspaces")
    (version "20250116.229")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mclear-tools/tabspaces.git")
             (commit "f552823f51f11d66492f754deb51abd709c08ed9")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "038il7nvymxh7wryskylz3ma4xl63jjvg6fvdjpa8x4ry60w4z5j"))))
    (build-system emacs-build-system)
    (propagated-inputs (list upstream:emacs-project))
    (home-page "https://github.com/mclear-tools/tabspaces")
    (synopsis "Leverage tab-bar and project for buffer-isolated workspaces")
    (description
     "This package provides several functions to facilitate a frame-based tab workflow
with one workspace per tab, integration with project.el (for project-based
workspaces) and buffer isolation per tab (i.e.  a \"tabspace\" workspace).  The
package assumes project.el and tab-bar.el are both present (they are built-in to
Emacs 27.1+).  This file is not part of GNU Emacs. ; Acknowledgements Much of
the package code is inspired by: - https://github.com/kaz-yos/emacs -
https://github.com/wamei/elscreen-separate-buffer-list/issues/8 -
https://www.rousette.org.uk/archives/using-the-tab-bar-in-emacs/ -
https://github.com/minad/consult#multiple-sources -
https://github.com/florommel/bufferlo.")
    (license #f)))
