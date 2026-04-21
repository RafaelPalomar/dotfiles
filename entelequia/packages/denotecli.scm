(define-module (entelequia packages denotecli)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages golang))

;;; denotecli — Go CLI companion for Protesilaos' Denote note system.
;;;
;;; Upstream: https://github.com/junghan0611/denotecli
;;; Go module path is nested at denotecli/denotecli, so #:unpack-path and
;;; #:import-path are set accordingly. Pure stdlib; no external Go deps.
;;; Requires Go 1.25+ per the module's go directive.

(define-public denotecli
  (let ((commit "240c9d4b43fc6b791576224ff872df7e183d797b")
        (revision "0"))
    (package
      (name "denotecli")
      (version (git-version "0.8.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/junghan0611/denotecli")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0k96gdgbps5ic1z82csp8vs4yivg9yms3j1vzfqsvp67dqg37l2d"))))
      (build-system go-build-system)
      (arguments
       (list #:go go-1.25
             #:import-path "github.com/junghan0611/denotecli/denotecli"
             #:unpack-path "github.com/junghan0611/denotecli"
             #:install-source? #f
             #:tests? #f))
      (home-page "https://github.com/junghan0611/denotecli")
      (synopsis "Command-line companion for Denote notes")
      (description
       "denotecli is a Go CLI that operates on note collections following the
Denote file-name convention @code{IDENTIFIER--TITLE__KEYWORDS.EXT}.  It offers
search, read, create, rename, graph-traversal, timeline, and keyword operations
with JSON output suitable for integration with AI agents and other tooling.
Uses the Go standard library only, with no external module dependencies.")
      (license license:asl2.0))))
