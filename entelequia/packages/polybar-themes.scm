;; Original from https://github.com/nmaupu/guix-config.git

(define-module (entelequia packages polybar-themes)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix licenses:)
  #:use-module (guix packages)
  #:use-module (guix git-download))

(define-public polybar-themes
  (package
    (name "polybar-themes")
    (version "0dfbf9fb282b594ff439ce916e7be373ba6fbd02")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/nmaupu/polybar-themes.git")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lhpg1sfi7jplg4ywssc5ihfa75235i1nwkdskhavc54c6bv66yp"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let* ((out (assoc-ref %outputs "out"))
                          (source (assoc-ref %build-inputs "source")))
                     (mkdir-p out)
                     (copy-recursively (string-append source "/simple") out)))))
    (home-page "https://github.com/nmaupu/polybar-themes.git")
    (synopsis "A huge collection of polybar themes with different styles, colors and variants.")
    (description "A huge collection of polybar themes with different styles, colors and variants.")
    (license licenses:gpl3)))
