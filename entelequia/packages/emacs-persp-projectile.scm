(define-module (config packages emacs-persp-projectile)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((gnu packages emacs-xyz) #:prefix upstream:)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-persp-projectile
  (package
    (name "emacs-persp-projectile")
    (version "20210618.708")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bbatsov/persp-projectile.git")
             (commit "4e374d7650c7e041df5af5ac280a44d4a4ec705a")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cpf1739cd6ylyaz7pspsmh1dsmvymdqfpypahca0nn169vdrzk9"))))
    (build-system emacs-build-system)
    (propagated-inputs (list upstream:emacs-perspective upstream:emacs-projectile))
    (home-page "https://github.com/bbatsov/persp-projectile")
    (synopsis "Perspective integration with Projectile")
    (description
     "This library bridges perspective mode to the awesome library Projectile.  The
idea is to create a separate perspective when switching project.  A perspective
is an independent workspace for Emacs, similar to multiple desktops in Gnome and
@code{MacOS}.  I often work on many projects at the same time, and using
perspective and projectile together allows me to easily know which project I'm
current in, and focus on files that only belong to current project when
switching buffer.  To use this library, put this file in your Emacs load path,
and call (require persp-projectile) See perspective.el on github:
https://github.com/nex3/perspective-el.")
    (license #f)))
