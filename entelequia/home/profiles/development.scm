(define-module (entelequia home profiles development)
  #:use-module (gnu packages)
  #:export (development-home-packages))

;;; Development home profile
;;;
;;; Development tools and programming environments.
;;; Includes compilers, build tools, and language-specific toolchains.

(define development-home-packages
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
         "libtool")))
