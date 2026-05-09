(define-module (entelequia packages emacs)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (guix download)
  #:use-module (guix build-system emacs)
  #:use-module ((gnu packages emacs-xyz) #:prefix upstream:)
  #:use-module ((gnu packages emacs-build) #:prefix upstream:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:))

;; emacs-denote-silo is now available upstream in (gnu packages emacs-xyz)
;; at the same version (0.2.0), so the local definition was removed.  It is
;; picked up via `(use-package-modules emacs-xyz ...)` in the home service.

;; This is a copy of the original in
;; https://github.com/hiecaq/guix-config?tab=readme-ov-file#evil-snipe
(define-public emacs-evil-snipe
  (let ((commit "c07788c35cf8cd8e652a494322fdc0643e30a89f")
        (last-release-version "2.0.8") ;; from the el file version header
        (revision "1")
        (url "https://github.com/hiecaq/evil-snipe"))
    (package
     (name "emacs-evil-snipe")
     (version (git-version last-release-version revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url url)
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "06zhpsmn67f2n0f2yqzcv978l10nkvdr25kkl3cwkhhj9a56x62g"))))
     (propagated-inputs
      (list upstream:emacs-evil))
     (build-system emacs-build-system)
     (home-page url)
     (synopsis "2-char searching ala vim-sneak & vim-seek, for evil-mode")
     (description "This library It provides 2-character motions for quickly
(and more accurately) jumping around text, compared to evil's built-in
f/F/t/T motions, incrementally highlighting candidate targets as you type.")
     (license license:expat))))

(define-public emacs-ob-mermaid
  (package
    (name "emacs-ob-mermaid")
    (version "20260323.0059")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/arnm/ob-mermaid.git")
             (commit "30c2da02e3d24dbec0d004d3c6dfe7b381500b05")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09mhzj1x4zvfj9yik71rcnk02rlidq2si018ah9iwy93r49hdslv"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/arnm/ob-mermaid")
    (synopsis "Org-babel support for mermaid evaluation")
    (description
     "Org-Babel support for evaluating mermaid diagrams.  Supported header arguments:
:file - Output file (required) :theme - Mermaid theme :width, :height - Diagram
dimensions :scale - Scale factor :background-color - Background color
:mermaid-config-file - Mermaid config file :css-file - CSS file for styling
:puppeteer-config-file - Puppeteer config file :pdf-fit - Enable PDF fit mode
:cmdline - Additional command line arguments.")
    (license #f)))

(define-public emacs-persp-projectile
  (package
    (name "emacs-persp-projectile")
    (version "20210618.708")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bbatsov/persp-projectile.git")
             (commit "4e374d7650c7e041df5af5ac280a44d4a4ec705a")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cpf1739cd6ylyaz7pspsmh1dsmvymdqfpypahca0nn169vdrzk9"))))
    (build-system emacs-build-system)
    (propagated-inputs (list upstream:emacs-perspective upstream:emacs-projectile))
    (home-page "https://github.com/bbatsov/persp-projectile")
    (synopsis "Perspective integration with Projectile")
    (description
     "This library bridges perspective mode to the awesome library Projectile.  The
idea is to create a separate perspective when switching project.  A perspective
is an independent workspace for Emacs, similar to multiple desktops in Gnome and
@code{MacOS}.  I often work on many projects at the same time, and using
perspective and projectile together allows me to easily know which project I'm
current in, and focus on files that only belong to current project when
switching buffer.  To use this library, put this file in your Emacs load path,
and call (require persp-projectile) See perspective.el on github:
https://github.com/nex3/perspective-el.")
    (license #f)))

(define-public emacs-tabspaces
  (package
    (name "emacs-tabspaces")
    (version "20260222.1459")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mclear-tools/tabspaces.git")
             (commit "9fdbb0c9a7a153459408c1a93f9a9139d00f254a")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jwmkxjzjb7jplnh1p7n4i580hjhxz9ijyvpq9gksamwgk1zd62i"))))
    (build-system emacs-build-system)
    (propagated-inputs (list upstream:emacs-project))
    (home-page "https://github.com/mclear-tools/tabspaces")
    (synopsis "Leverage tab-bar and project for buffer-isolated workspaces")
    (description
     "This package provides several functions to facilitate a frame-based tab workflow
with one workspace per tab, integration with project.el (for project-based
workspaces) and buffer isolation per tab (i.e.  a \"tabspace\" workspace).  The
package assumes project.el and tab-bar.el are both present (they are built-in to
Emacs 27.1+).  This file is not part of GNU Emacs. ; Acknowledgements Much of
the package code is inspired by: - https://github.com/kaz-yos/emacs -
https://github.com/wamei/elscreen-separate-buffer-list/issues/8 -
https://www.rousette.org.uk/archives/using-the-tab-bar-in-emacs/ -
https://github.com/minad/consult#multiple-sources -
https://github.com/florommel/bufferlo.")
    (license #f)))

(define-public emacs-aider
  (package
   (name "emacs-aider")
   (version "0.13.2")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/tninja/aider.el")
           (commit "6f6472586413f59cce992ecaa79276e3b56212ca")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1932ghif057mxm9nz1213gf8j26ispl6ikfr90hrlag168c2f630"))))
   (build-system emacs-build-system)
   (propagated-inputs (list upstream:emacs-transient upstream:emacs-magit
                            upstream:emacs-markdown-mode upstream:emacs-s))
   (arguments
    '(#:include '("^[^/]*.el$" "^[^/]*.el.in$"
                  "^dir$"
                  "^[^/]*.info$"
                  "^[^/]*.texi$"
                  "^[^/]*.texinfo$"
                  "^doc/dir$"
                  "^doc/[^/]*.info$"
                  "^doc/[^/]*.texi$"
                  "^doc/[^/]*.texinfo$"
                  "^snippets$")
      #:exclude '("^.dir-locals.el$" "^test.el$" "^tests.el$"
                  "^[^/]*-test.el$" "^[^/]*-tests.el$")))
   (home-page "https://github.com/tninja/aider.el")
   (synopsis "AI assisted programming with Aider and LLM")
   (description
    "Boost your programming efficiency! This package + Aider (https://aider.chat/)
brings AI-assisted programming capabilities *inside* Emacs! Aider works
seamlessly with both *new* and *existing* codebases in your local Git repo,
using AI models (Claude, @code{ChatGPT}, Gemini, even local ones!) to assist
you.  It can suggest improvements, squash bugs, or even write whole new sections
of code.  Enhance your coding with AI without ever leaving your Emacs comfort
zone.  The package also supports AI-assisted Agile development workflows and
AI-assisted code reading to help you understand complex codebases faster and
more thoroughly.  To use aider.el, you need to install the Aider command line
tool: https://aider.chat/#getting-started After that, configure it with (use
sonnet as example): (use-package aider :config ;; For latest claude sonnet model
(setq aider-args (\"--model\" \"sonnet\" \"--no-auto-accept-architect\")) (setenv
\"ANTHROPIC_API_KEY\" anthropic-api-key) (global-set-key (kbd \"C-c a\")
aider-transient-menu)) For more details, see https://github.com/tninja/aider.el
If you like the feature of this package, but wish to use Claude Code / Gemini
CLI / or @code{OpenAI} Codex, please take a look at
https://github.com/tninja/ai-code-interface.el Comparing to its forked peer
(aidermacs), Aider.el has brought in lots of application-level features and
tools to enhance daily programming.  These include: - AI-assisted agile
development methodologies (like TDD, refactoring and legacy code handling based
on established software engineering books) - Code / module reading AI assistant
- Diff extraction and AI code review tools - Let aider to fix the errors
reported by flycheck - Expand context by adding current file's
dependencies/dependents - Software planning / brainstorming discussion
capabilities - Prompt candidates for code change / document change / question
ask - Run current script to validate AI's change - Code / repo evolution
analysis with git blame and git log - Utilities for bootstrapping new files and
projects. - Organize project with repo specific Aider prompt file - Snippets
from community and aider use experience and pattern Besides of that, aider.el
focus on simplicity.  It has much less configurations (transparent to aider
config), simplified menu.")
   (license #f)))

(define-public emacs-copilot
  (package
    (name "emacs-copilot")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/copilot-emacs/copilot.el")
             (commit "c8c06efaa508569e13d7191882ae33435bb14543")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xvhfwgddms0cxhi9pn75vb6qsd6gqfv8s59xjk9ilh57nvwzqfn"))))
    (build-system emacs-build-system)
    (propagated-inputs (list upstream:emacs-editorconfig upstream:emacs-jsonrpc
                             upstream:emacs-track-changes upstream:emacs-f))
    (home-page "https://github.com/copilot-emacs/copilot.el")
    (synopsis "An unofficial Copilot plugin")
    (description "An unofficial Copilot plugin for Emacs.")
    (license #f)))
