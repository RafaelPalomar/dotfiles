(define-module (entelequia home profiles development)
  #:use-module (gnu packages)
  #:use-module (entelequia packages aider)
  #:use-module (entelequia packages claude-code)
  #:use-module (guix-openclaw packages openclaw)
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

          ;; LaTeX / document authoring
          "texlive-scheme-basic"  ;; pdflatex engine + basic LaTeX
          "texlive-pgfgantt"      ;; Gantt charts (pulls in pgf/tikz)
          "texlive-standalone"))  ;; standalone class for org-babel latex blocks
   ;; AI coding assistants (local packages)
   (list claude-code
         python-aider-chat
         openclaw)))
