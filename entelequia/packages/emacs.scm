(define-module (entelequia packages emacs)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (guix download)
  #:use-module (guix build-system emacs)
  #:use-module ((gnu packages emacs-xyz) #:prefix upstream:)
  #:use-module ((gnu packages emacs-build) #:prefix upstream:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:))

;; emacs-denote-silo is now available upstream in (gnu packages emacs-xyz)
;; at the same version (0.2.0), so the local definition was removed.  It is
;; picked up via `(use-package-modules emacs-xyz ...)` in the home service.

;; This is a copy of the original in
;; https://github.com/hiecaq/guix-config?tab=readme-ov-file#evil-snipe
(define-public emacs-evil-snipe
  (let ((commit "c07788c35cf8cd8e652a494322fdc0643e30a89f")
        (last-release-version "2.0.8") ;; from the el file version header
        (revision "1")
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
         "06zhpsmn67f2n0f2yqzcv978l10nkvdr25kkl3cwkhhj9a56x62g"))))
     (propagated-inputs
      (list upstream:emacs-evil))
     (build-system emacs-build-system)
     (home-page url)
     (synopsis "2-char searching ala vim-sneak & vim-seek, for evil-mode")
     (description "This library It provides 2-character motions for quickly
(and more accurately) jumping around text, compared to evil's built-in
f/F/t/T motions, incrementally highlighting candidate targets as you type.")
     (license license:expat))))

(define-public emacs-ob-mermaid
  (package
    (name "emacs-ob-mermaid")
    (version "20260323.0059")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/arnm/ob-mermaid.git")
             (commit "30c2da02e3d24dbec0d004d3c6dfe7b381500b05")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09mhzj1x4zvfj9yik71rcnk02rlidq2si018ah9iwy93r49hdslv"))))
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

(define-public emacs-tabspaces
  (package
    (name "emacs-tabspaces")
    (version "20260515.2051")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mclear-tools/tabspaces.git")
             (commit "ea95efb5b2ef265e3c48059801554042ab0813b3")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lwnj37xrcf7838dnprrdspp92yh5sz9ihks6xr759vjd6iahksj"))))
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

(define-public emacs-copilot
  (package
    (name "emacs-copilot")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/copilot-emacs/copilot.el")
             (commit "ab5c58bc969f52f6d75e972658f2c3381c70b4fa")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1glqr4x7r2f0wgcn0mbcvphdidirpnq1b771ig5ly4s3zaxnqxl5"))))
    (build-system emacs-build-system)
    (propagated-inputs (list upstream:emacs-editorconfig upstream:emacs-jsonrpc
                             upstream:emacs-track-changes upstream:emacs-f))
    (home-page "https://github.com/copilot-emacs/copilot.el")
    (synopsis "An unofficial Copilot plugin")
    (description "An unofficial Copilot plugin for Emacs.")
    (license #f)))

(define-public emacs-uuidgen
  ;; Runtime dependency of emacs-code-review; not yet packaged upstream in Guix.
  (let ((commit "cebbe09d27c63abe61fe8c2e2248587d90265b59")
        (revision "0"))
    (package
      (name "emacs-uuidgen")
      (version (git-version "1.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kanru/uuidgen-el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1ih6kj3inwdxypbqj2n5vnfxmc6rfrx114w8bdy60yd8klx7273d"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/kanru/uuidgen-el")
      (synopsis "UUID generation functions for Emacs Lisp")
      (description
       "Provides various UUID (Universally Unique IDentifier) generating functions
following RFC 4122 — UUID versions 1, 3, 4, and 5.")
      (license license:gpl3+))))

(define-public emacs-code-review
  ;; PR review UI (inline diff comments, approve/request-changes, suggestion
  ;; blocks) built on top of Forge.  Upstream (wandersoncferreira) is dormant
  ;; since 2022-12 and predates the emacsql 4 / closql 2 API change that
  ;; ships in current Guix; phelrine's `fix/closql-update' branch carries
  ;; the compatibility patch.  Pinned to that branch's head.
  (let ((commit "97dae6fca12d49833dcbe865460021151520c10b")
        (revision "0"))
    (package
      (name "emacs-code-review")
      (version (git-version "0.0.7" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/phelrine/code-review")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "12y2209mkk6c2p1fh8zbzbk044m52690ji1dqjb1a7s2i5yaka2p"))))
      (build-system emacs-build-system)
      (arguments
       (list #:tests? #f))                ; upstream Makefile has no 'check' target
      (propagated-inputs
       (list upstream:emacs-closql
             upstream:emacs-magit
             upstream:emacs-transient
             upstream:emacs-a
             upstream:emacs-ghub
             emacs-uuidgen
             upstream:emacs-deferred
             upstream:emacs-markdown-mode
             upstream:emacs-forge
             upstream:emacs-emojify))
      (home-page "https://github.com/wandersoncferreira/code-review")
      (synopsis "Perform code review from GitHub, GitLab, and Bitbucket Cloud")
      (description
       "Code Review lets you review pull/merge requests directly from Emacs.
It builds on Magit + Forge and adds inline diff comments, approve /
request-changes / comment review submission, suggestion blocks, and
reply-to-thread handling for GitHub, GitLab, and Bitbucket Cloud.")
      (license license:gpl3+))))
