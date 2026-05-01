(define-module (entelequia packages python-learning)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz))

;;; Python learning toolchain
;;;
;;; - thonny:        beginner-friendly Python IDE with a step-by-step
;;;                  variable visualizer (the killer feature for kids).
;;; - python-pgzero: zero-boilerplate 2D games framework on top of
;;;                  pygame; aimed at education.
;;;
;;; Pinned to thonny 4.1.7 (last setuptools-based release); 5.x switched
;;; to uv_build which Guix doesn't yet expose as a build backend.

(define-public python-pgzero
  (package
    (name "python-pgzero")
    (version "1.2.1")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "pgzero" version))
      (sha256
       (base32 "01k1iv1qdy9kyizr3iysxqfmy10w38qvjfxx1hzarjr8y0hc1bcc"))))
    (build-system python-build-system)
    (arguments
     (list #:tests? #f))    ; tests need a display
    (propagated-inputs (list python-pygame python-numpy))
    (home-page "https://github.com/lordmauve/pgzero")
    (synopsis "Zero-boilerplate 2D games framework for education")
    (description
     "Pygame Zero is a programming framework designed to make creating
2D games approachable for beginners.  Programs run with the @code{pgzrun}
launcher, which removes the boilerplate normally required by Pygame
(initialisation, the main loop, event dispatch).  Authored by Daniel
Pope to support the Python in education community.")
    (license license:lgpl3+)))

(define-public thonny
  (package
    (name "thonny")
    (version "4.1.7")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "thonny" version))
      (sha256
       (base32 "08w1lm2apb10swj3rl8dh92mc8q1my72q8qfhdi29h94yl2mcfvb"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f    ; in-tree tests require a running Tk display
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-pgzero-ast-end-lineno
            (lambda _
              ;; Backport upstream PR #3552 (Thonny 5.0): the pgzero
               ;; plugin synthesises a `pgzrun.go()' call and sets
               ;; lineno=1000000 as a sentinel but forgets end_lineno,
               ;; producing an invalid AST range (1000000, 1) and
               ;; aborting Run with `ValueError: AST node line range
               ;; (1000000, 1) is not valid'.  Single-line fix.
              (substitute* "thonny/plugins/backend/pgzero_backend.py"
                (("go\\.lineno = 1000000")
                 "go.lineno = 1000000\n    go.end_lineno = 1000000"))))
          (add-after 'install 'install-desktop-files
            (lambda _
              ;; Thonny ships .desktop, AppData, man page and icons under
              ;; packaging/ but its setup.py leaves them for distro
              ;; packagers to install.  Wire them up here so menu
              ;; launchers and icons just work after `guix home reconfigure'.
              (let* ((apps     (string-append #$output "/share/applications"))
                     (metainfo (string-append #$output "/share/metainfo"))
                     (man1     (string-append #$output "/share/man/man1"))
                     (icons    (string-append #$output "/share/icons/hicolor")))
                (mkdir-p apps)
                (copy-file "packaging/linux/org.thonny.Thonny.desktop"
                           (string-append apps "/org.thonny.Thonny.desktop"))
                ;; Resolve via PATH (the home profile puts thonny on PATH).
                (substitute* (string-append apps "/org.thonny.Thonny.desktop")
                  (("/usr/bin/thonny") "thonny"))
                (mkdir-p metainfo)
                (copy-file "packaging/linux/org.thonny.Thonny.appdata.xml"
                           (string-append metainfo
                                          "/org.thonny.Thonny.appdata.xml"))
                (mkdir-p man1)
                (copy-file "packaging/linux/thonny.1"
                           (string-append man1 "/thonny.1"))
                (for-each
                 (lambda (size)
                   (let* ((dir (string-append icons "/" size "x" size
                                              "/apps"))
                          (src (string-append "packaging/icons/thonny-"
                                              size "x" size ".png")))
                     (when (file-exists? src)
                       (mkdir-p dir)
                       (copy-file src (string-append dir "/thonny.png")))))
                 '("16" "22" "32" "48" "64" "128" "192" "256"))))))))
    (propagated-inputs
     (list `(,python "tk")  ; tkinter — Thonny's GUI toolkit
           python-asttokens
           python-docutils
           python-jedi
           python-mypy
           python-pylint
           python-pyserial
           python-send2trash
           python-setuptools
           python-wheel))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://thonny.org")
    (synopsis "Python IDE for beginners")
    (description
     "Thonny is a Python IDE designed for teaching and learning
programming.  Distinguishing features for novices include a
step-by-step debugger that visualises function calls and variable
state, an integrated package manager, a simple shell, and a faithful
representation of how Python evaluates expressions.  Used widely in
schools and on the Raspberry Pi.")
    (license license:expat)))
