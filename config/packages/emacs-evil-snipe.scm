(define-module (config packages emacs-evil-snipe)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((gnu packages emacs-xyz) #:prefix upstream:)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

;; This is a copy of the original in
;; https://github.com/hiecaq/guix-config?tab=readme-ov-file#evil-snipe
(define-public emacs-evil-snipe
  (let ((commit "3ad53b8da0dd23093a3f2f0e5c13ecdb08ba8efa")
        (last-release-version "2.0.8") ;; from the el file version header
        (revision "0")
        (url "https://github.com/hiecaq/evil-snipe"))
    (package
     (name "emacs-evil-snipe")
     (version (git-version last-release-version revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url url)
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0fk9nl0h1j1ig6pvb4aix3injxi2jyw9djixchxf4aky11znivgj"))))
     (propagated-inputs
      (list upstream:emacs-evil))
     (build-system emacs-build-system)
     (home-page url)
     (synopsis "2-char searching ala vim-sneak & vim-seek, for evil-mode")
     (description "This library It provides 2-character motions for quickly
(and more accurately) jumping around text, compared to evil's built-in
f/F/t/T motions, incrementally highlighting candidate targets as you type.")
     (license license:expat))))
