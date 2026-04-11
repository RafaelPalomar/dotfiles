(define-module (entelequia home profiles development)
  #:use-module (gnu packages)
  #:use-module (entelequia packages aider)
  #:use-module (entelequia packages claude-code)
  #:use-module (entelequia packages mermaid-cli)
  #:use-module (guix-openclaw packages openclaw)
  #:use-module (systole packages claude-skills)
  #:export (development-home-packages))

;;; Development home profile
;;;
;;; Development tools and programming environments.
;;; Includes compilers, build tools, and language-specific toolchains.

(define (development-home-packages)
  (append
   (map specification->package
        '(;; C/C++ development
          "gcc-toolchain"
          "glibc"
          "make"
          "cmake"

          ;; Node.js/JavaScript
          "node"

          ;; Python
          "python"
          "python-pip"

          ;; Debugging and profiling
          "gdb"
          "valgrind"
          "strace"

          ;; Build tools
          "pkg-config"
          "autoconf"
          "automake"
          "libtool"

          ;; Secrets management
          "sops"

          ;; Note: full LaTeX/XeLaTeX toolchain is at the system level via
          ;; base-latex-packages (common-packages.scm) so xelatex is on PATH
          ;; for all processes including the Emacs shepherd daemon.
          ))
   ;; Local packages
   (list claude-code
         mermaid-cli
         python-aider-chat
         openclaw
         slicer-skill)))
