(define-module (entelequia packages latex)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system font)
  #:use-module (guix build-system texlive)
  #:use-module (guix licenses)
  #:export (font-sciflycore-sans
            latex-nfr))

;;; Packages for the NFR proposal LaTeX class and its required fonts.
;;;
;;; The nfr class targets XeLaTeX and uses three font families:
;;;   - Dosis (Google Fonts / OFL) — available as font-dosis in Guix
;;;   - Gentium Plus (SIL / OFL)   — available as font-sil-gentium in Guix
;;;   - SciFly Sans (Flyerzone, freeware) — packaged here

(define-public font-sciflycore-sans
  (package
    (name "font-sciflycore-sans")
    (version "1.0")
    (source (local-file "nfr/SciFly-Sans.ttf"
                        "SciFly-Sans.ttf"))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (dir (string-append out "/share/fonts/truetype")))
               (mkdir-p dir)
               (copy-file (assoc-ref %build-inputs "source")
                          (string-append dir "/SciFly-Sans.ttf"))
               #t))))))
    (home-page "http://www.flyerzone.co.uk")
    (synopsis "SciFly Sans freeware typeface by Flyerzone")
    (description
     "SciFly Sans is a freeware sans-serif typeface designed by Flyerzone.
It is used by the @code{nfr} LaTeX class for title and heading decorations.")
    (license (fsdg-compatible "http://www.flyerzone.co.uk"
                             "Freeware typeface; free to use commercially or non-commercially"))))

(define-public latex-nfr
  (package
    (name "latex-nfr")
    (version "2022.10.18")
    (source (local-file "nfr/texmf"
                        "latex-nfr-texmf"
                        #:recursive? #t))
    (build-system texlive-build-system)
    (home-page "https://github.com/OUH-MESHLab")
    (synopsis "LaTeX class for NFR grant proposals (XeLaTeX)")
    (description
     "The @code{nfr} document class provides styling for Norwegian Research
Council (NFR) grant proposals.  It requires XeLaTeX and uses the Dosis,
Gentium Plus, and SciFly Sans font families.

The class sets up coloured section headings (lavender), hyperlinks, a custom
caption rule, and convenience commands for cross-referenced requirement tags.")
    (license expat)))
