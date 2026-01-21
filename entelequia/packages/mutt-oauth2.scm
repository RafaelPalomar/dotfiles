(define-module (entelequia packages mutt-oauth2)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils))

(define-public mutt-oauth2
  (package
    (name "mutt-oauth2")
    (version "0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://raw.githubusercontent.com/neomutt/neomutt/main/contrib/oauth2/mutt_oauth2.py")
        (file-name "mutt_oauth2.py")
        (sha256
          (base32 "1990c4xqaacbhsq82a3rpzd6r3gzkls4wxk5ww958xcl44m3mh9i"))))
    (build-system trivial-build-system)
    (arguments
      (list
        #:modules '((guix build utils))
        #:builder
        #~(begin
            (use-modules (guix build utils))
            (let* ((source (assoc-ref %build-inputs "source"))
                   (out (assoc-ref %outputs "out"))
                   (bin (string-append out "/bin"))
                   (script (string-append bin "/mutt_oauth2.py")))
              (mkdir-p bin)
              (copy-file source script)
              (chmod script #o555)))))
    (home-page "https://neomutt.org/")
    (synopsis "OAuth2 support script for Mutt/Neomutt")
    (description
     "This package provides the mutt-oauth2.py script from the Neomutt project,
which adds OAuth2 authentication support to Mutt/Neomutt email clients.")
    (license license:gpl3+)))
