(define-module (entelequia packages gh)
  #:use-module (guix packages)
  #:use-module ((guix licenses)  #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages syncthing)
  #:use-module (gnu packages version-control)
  #:use-module (nonguix build-system binary))

(define-public gh
  (package
    (name "gh")
    (version "2.93.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/cli/cli/releases/download/v"
                           version "/gh_" version "_linux_amd64.tar.gz"))
       (sha256
        (base32 "1w3sh2swdaw5d93p1nsx8x8mmiz14bzzy29pdy4hn3hkp872kl82"))))
    (build-system binary-build-system)
    (arguments
     '(#:install-plan '(("bin/" "/bin/")
                        ("share/" "/share/")
                        ("LICENSE" "share/doc/gh/"))))
    (home-page "https://cli.github.com/")
    (synopsis "GitHub’s official command line tool")
    (description
     "@code{gh} is GitHub on the command line. It brings pull requests, issues,
and other GitHub concepts to the terminal next to where you are already working
with @code{git} and your code.")
    (license license:expat)))
