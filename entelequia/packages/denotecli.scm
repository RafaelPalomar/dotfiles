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
        (revision "3"))
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
         ;; Three local patches (kept identical in the alpha-agent vendored copy,
         ;; alpha-agent/alpha-agent/denotecli.scm; the archimedes copy carries 1+3
         ;; only — it deliberately omits the DENOTECLI_DIRS default):
         ;;  1. `denotecli create --content -' reads the body from stdin.
         ;;     Upstream 0.8.0 treats --content's value as a literal string with
         ;;     no `-' special case, which silently drops piped bodies.  Patch
         ;;     the CLI layer (cmdCreate in main.go) so CreateNote stays pure.
         ;;  2. `--dirs' defaults to $DENOTECLI_DIRS (falling back to upstream
         ;;     ~/org when unset), so a flag-less command can target a configured
         ;;     store instead of ~/org.  Backward-compatible: env unset => ~/org.
         ;;  3. a new `append' subcommand: locate a note by ID and append to its
         ;;     body IN PLACE, never regenerating the filename/identifier (Denote
         ;;     IDs are load-bearing for backlinks).  Upstream has create but no
         ;;     append, so a rolling note (e.g. an agent's long-term memory) can
         ;;     only be grown by hand-editing.  Shipped as a self-contained
         ;;     append.go wired into the dispatch + usage.
         (snippet
          '(begin
             (use-modules (guix build utils))
             ;; Patch 3: write the self-contained append.go.  Authored in plain
             ;; upstream style (--dirs default "~/org"); patch 2's substitution
             ;; below rewrites that default to defaultDirs() here too.
             (call-with-output-file "denotecli/append.go"
               (lambda (port)
                 (display "\
// append.go
package main

import (
	\"fmt\"
	\"io\"
	\"os\"
	\"path/filepath\"
	\"strings\"
)

// AppendNote appends content to the body of an existing Denote note, located by
// ID, WITHOUT changing its filename or identifier.  Denote IDs are load-bearing
// for backlinks and must never be regenerated, so this rewrites the same path in
// place.  Appended text is separated from the existing body by a single blank
// line, matching the layout CreateNote produces.
func AppendNote(files []DenoteFile, id, content string) (string, error) {
	for _, f := range files {
		if f.ID == id {
			data, err := os.ReadFile(f.Path)
			if err != nil {
				return \"\", fmt.Errorf(\"read %s: %w\", f.Path, err)
			}
			nl := \"\\n\"
			body := strings.TrimRight(string(data), nl)
			body += nl + nl + strings.TrimRight(content, nl) + nl
			if err := os.WriteFile(f.Path, []byte(body), 0644); err != nil {
				return \"\", fmt.Errorf(\"write %s: %w\", f.Path, err)
			}
			return f.Path, nil
		}
	}
	return \"\", fmt.Errorf(\"not found: %s\", id)
}

func cmdAppend() {
	if len(os.Args) < 3 {
		fatal(\"usage: denotecli append <id> --content TEXT [--dirs DIR,...]\")
	}
	id := os.Args[2]
	args := os.Args[3:]
	if err := validateFlags(args, []string{\"--dirs\", \"--content\"}, nil); err != nil {
		fatal(err.Error())
	}
	dirsStr := getFlag(args, \"--dirs\", \"~/org\")
	content := getFlag(args, \"--content\", \"\")
	if content == \"-\" {
		data, err := io.ReadAll(os.Stdin)
		if err != nil {
			fatal(\"read stdin: \" + err.Error())
		}
		content = string(data)
	}
	if strings.TrimSpace(content) == \"\" {
		fatal(\"usage: denotecli append <id> --content TEXT [--dirs DIR,...] (content was empty)\")
	}
	dirs := strings.Split(dirsStr, \",\")
	files := ScanDirs(dirs)
	path, err := AppendNote(files, id, content)
	if err != nil {
		fatal(err.Error())
	}
	df, ok := ParseFilename(filepath.Base(path))
	if !ok {
		fatal(\"parse appended filename: \" + path)
	}
	df.Path = path
	printJSON(df)
}
" port)))
             ;; Patch 1: add the "io" import — needed for io.ReadAll.
             (substitute* "denotecli/main.go"
               (("\t\"fmt\"\n")
                "\t\"fmt\"\n\t\"io\"\n"))
             ;; Patch 1: splice stdin-read right after the --content flag is parsed.
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
                 "\t}\n")))
             ;; Patch 2: inject defaultDirs() and route every --dirs default through
             ;; it — in BOTH main.go and the new append.go.
             (substitute* "denotecli/main.go"
               (("func main\\(\\) \\{")
                (string-append
                 "func defaultDirs() string {\n"
                 "\tif d := os.Getenv(\"DENOTECLI_DIRS\"); d != \"\" {\n"
                 "\t\treturn d\n"
                 "\t}\n"
                 "\treturn \"~/org\"\n"
                 "}\n\n"
                 "func main() {")))
             (substitute* '("denotecli/main.go" "denotecli/append.go")
               (("getFlag\\(args, \"--dirs\", \"~/org\"\\)")
                "getFlag(args, \"--dirs\", defaultDirs())"))
             ;; Patch 3: dispatch + usage for `append'.  The usage clause anchors
             ;; on the trailing "]\n" (unique to the usage() line) so it does not
             ;; also hit the identical text in cmdCreate's double-quoted fatal().
             (substitute* "denotecli/main.go"
               (("\t\tcmdCreate\\(\\)")
                "\t\tcmdCreate()\n\tcase \"append\":\n\t\tcmdAppend()")
               (("\\[--content TEXT\\]\n")
                "[--content TEXT]\n  denotecli append <id> --content TEXT [--dirs DIR,...]\n"))))))
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
