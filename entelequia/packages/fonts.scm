;; Original from https://github.com/nmaupu/guix-config.git

(define-module (entelequia packages fonts)
  #:use-module (guix build-system font)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download))

(define* (%nerd-font-package #:key font-name version hash)
  (package
    (name (string-append "nerd-font-" (string-downcase font-name)))
    (version version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ryanoasis/nerd-fonts/releases/download/v"
                           version "/" font-name ".zip"))
       (sha256
        (base32 hash))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'make-files-writable
           (lambda _
             (for-each
              make-file-writable
              (find-files "." ".*\\.(otf|otc|ttf|ttc)$"))
             #t)))))
    (home-page "https://www.nerdfonts.com/")
    (synopsis "Iconic font aggregator, collection, and patcher")
    (description
     "Nerd Fonts patches developer targeted fonts with a high number
of glyphs (icons). Specifically to add a high number of extra glyphs
from popular ‘iconic fonts’ such as Font Awesome, Devicons, Octicons,
and others.")
    (license expat)))

(define-public polybar-themes-fonts
  (package
    (name "polybar-themes-fonts")
    (version "396a4c3649c2ad15c4724dd541c433b249bb0b9a")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/adi1090x/polybar-themes.git")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00pi697wl4ihfdw8sz57rg5cy9isv2zx9k6j00vbr289m7lr6db2"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'make-files-writable
           (lambda _
             (for-each
              make-file-writable
              (find-files "./fonts" ".*\\.(otf|otc|ttf|ttc)$"))
             #t)))))
    (home-page "")
    (synopsis "")
    (description "")
    (license expat)))

(define-public nerd-font-commitmono
  (%nerd-font-package #:font-name "CommitMono"
                      #:version "3.3.0"
                      #:hash "0bxk8mqalg2sf8pnm85p083imcjcnzz4h5lg0920dljqbz95w1gj"))

(define-public nerd-font-iosevka
  (%nerd-font-package #:font-name "Iosevka"
                      #:version "3.3.0"
                      #:hash "10w24pir4flr0zhm0n6v6kblgmcx7dpnqv2xkp8d0rgh3rnlwpm5"))

(define-public nerd-font-fantasque-sans-mono
  (%nerd-font-package #:font-name "FantasqueSansMono"
                      #:version "3.3.0"
                      #:hash "1r9z57k5k191lb19i85qg42zawnm1vwrrvnq7nvabywcwq66d6l4"))

(define-public nerd-font-noto
  (%nerd-font-package #:font-name "Noto"
                      #:version "3.3.0"
                      #:hash "14mna22dam0kx0axi53rjvkr97wlv161a9w2ap771890cjxnw70k"))

(define-public nerd-font-droid-sans-mono
  (%nerd-font-package #:font-name "DroidSansMono"
                      #:version "3.3.0"
                      #:hash "18hfqhrqfb8z3r4gkmjlv6l9xvgrpkd97ahk2j8fx89qbpa3mab0"))

(define-public nerd-font-terminus
  (%nerd-font-package #:font-name "Terminus"
                      #:version "3.3.0"
                      #:hash "0msqm9rnc5rzmwmfs2sx534hqz3apmfja00m6iib8627nr30n2yl"))

(define-public nerd-font-jetbrains
  (%nerd-font-package #:font-name "JetBrainsMono"
                      #:version "3.4.0"
                      #:hash "0g29gj9d6720grfr2vasnvdppzw4hycpfyd5di54d2p4mkrmzw3n"))

(define-public fonts-all
  (list nerd-font-commitmono
        nerd-font-iosevka
        nerd-font-fantasque-sans-mono
        nerd-font-noto
        nerd-font-droid-sans-mono
        nerd-font-terminus
        nerd-font-jetbrains
        polybar-themes-fonts))
(define-module (nmaupu packages fonts)
  #:use-module (guix build-system font)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download))

(define* (%nerd-font-package #:key font-name version hash)
  (package
    (name (string-append "nerd-font-" (string-downcase font-name)))
    (version version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ryanoasis/nerd-fonts/releases/download/v"
                           version "/" font-name ".zip"))
       (sha256
        (base32 hash))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'make-files-writable
           (lambda _
             (for-each
              make-file-writable
              (find-files "." ".*\\.(otf|otc|ttf|ttc)$"))
             #t)))))
    (home-page "https://www.nerdfonts.com/")
    (synopsis "Iconic font aggregator, collection, and patcher")
    (description
     "Nerd Fonts patches developer targeted fonts with a high number
of glyphs (icons). Specifically to add a high number of extra glyphs
from popular ‘iconic fonts’ such as Font Awesome, Devicons, Octicons,
and others.")
    (license expat)))

(define-public polybar-themes-fonts
  (package
    (name "polybar-themes-fonts")
    (version "396a4c3649c2ad15c4724dd541c433b249bb0b9a")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/adi1090x/polybar-themes.git")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00pi697wl4ihfdw8sz57rg5cy9isv2zx9k6j00vbr289m7lr6db2"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'make-files-writable
           (lambda _
             (for-each
              make-file-writable
              (find-files "./fonts" ".*\\.(otf|otc|ttf|ttc)$"))
             #t)))))
    (home-page "")
    (synopsis "")
    (description "")
    (license expat)))

(define-public nerd-font-commitmono
  (%nerd-font-package #:font-name "CommitMono"
                      #:version "3.3.0"
                      #:hash "0bxk8mqalg2sf8pnm85p083imcjcnzz4h5lg0920dljqbz95w1gj"))

(define-public nerd-font-iosevka
  (%nerd-font-package #:font-name "Iosevka"
                      #:version "3.3.0"
                      #:hash "10w24pir4flr0zhm0n6v6kblgmcx7dpnqv2xkp8d0rgh3rnlwpm5"))

(define-public nerd-font-fantasque-sans-mono
  (%nerd-font-package #:font-name "FantasqueSansMono"
                      #:version "3.3.0"
                      #:hash "1r9z57k5k191lb19i85qg42zawnm1vwrrvnq7nvabywcwq66d6l4"))

(define-public nerd-font-noto
  (%nerd-font-package #:font-name "Noto"
                      #:version "3.3.0"
                      #:hash "14mna22dam0kx0axi53rjvkr97wlv161a9w2ap771890cjxnw70k"))

(define-public nerd-font-droid-sans-mono
  (%nerd-font-package #:font-name "DroidSansMono"
                      #:version "3.3.0"
                      #:hash "18hfqhrqfb8z3r4gkmjlv6l9xvgrpkd97ahk2j8fx89qbpa3mab0"))

(define-public nerd-font-terminus
  (%nerd-font-package #:font-name "Terminus"
                      #:version "3.3.0"
                      #:hash "0msqm9rnc5rzmwmfs2sx534hqz3apmfja00m6iib8627nr30n2yl"))

(define-public nerd-font-jetbrains
  (%nerd-font-package #:font-name "JetBrainsMono"
                      #:version "3.4.0"
                      #:hash "0g29gj9d6720grfr2vasnvdppzw4hycpfyd5di54d2p4mkrmzw3n"))

(define-public fonts-all
  (list nerd-font-commitmono
        nerd-font-iosevka
        nerd-font-fantasque-sans-mono
        nerd-font-noto
        nerd-font-droid-sans-mono
        nerd-font-terminus
        nerd-font-jetbrains
        polybar-themes-fonts))
