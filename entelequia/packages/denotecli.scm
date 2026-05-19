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
  (let ((commit "d1c02d07d99e6a23ae00393e01c3b487e020527f")
        (revision "2"))
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
          (base32 "0aixcmfcqvmd6qgxx6zd7p4vpy1xb82dqq3r82b0rpxgqq9k5pgm"))
         ;; Make `denotecli create --content -' read the body from stdin.
         ;; Upstream 0.8.0 treats --content's value as a literal string with
         ;; no `-' special case, which silently drops piped bodies.  Patch
         ;; the CLI layer (cmdCreate in main.go) so CreateNote stays pure.
         (snippet
          '(begin
             (use-modules (guix build utils))
             ;; Add the "io" import — needed for io.ReadAll.
             (substitute* "denotecli/main.go"
               (("\t\"fmt\"\n")
                "\t\"fmt\"\n\t\"io\"\n"))
             ;; Splice stdin-read right after the --content flag is parsed.
             (substitute* "denotecli/main.go"
               (("\tcontent := getFlag\\(args, \"--content\", \"\"\\)\n")
                (string-append
                 "\tcontent := getFlag(args, \"--content\", \"\")\n"
                 "\tif content == \"-\" {\n"
                 "\t\tdata, err := io.ReadAll(os.Stdin)\n"
                 "\t\tif err != nil {\n"
                 "\t\t\tfatal(\"read stdin: \" + err.Error())\n"
                 "\t\t}\n"
                 "\t\tcontent = string(data)\n"
                 "\t}\n")))))))
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
