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

(define-public emacs-denote-silo
  (package
    (name "emacs-denote-silo")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/denote-silo-"
                           version ".tar"))
       (sha256
        (base32 "1jxr52npjiwisambwav6rasndjdxhll8x278q8cr7giq71am7c8b"))))
    (build-system emacs-build-system)
    (propagated-inputs (list upstream:emacs-denote))
    (home-page "https://github.com/protesilaos/denote-silo")
    (synopsis "Convenience functions for using Denote in multiple silos")
    (description "Package for working with different denote silos")
    (license license:gpl3+)))

;; This is a copy of the original in
;; https://github.com/hiecaq/guix-config?tab=readme-ov-file#evil-snipe
(define-public emacs-evil-snipe
  (let ((commit "3ad53b8da0dd23093a3f2f0e5c13ecdb08ba8efa")
        (last-release-version "2.0.8") ;; from the el file version header
        (revision "0")
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
         "0fk9nl0h1j1ig6pvb4aix3injxi2jyw9djixchxf4aky11znivgj"))))
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
    (version "20250621.1655")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/arnm/ob-mermaid.git")
             (commit "372c2d91d3cdba5da9f7ac23e7bce7a0b3b46862")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bvg7xgm9ph7hbkgzm145ifxi833rg6lamanngqq564n7d0l4ng0"))))
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
    (version "20250116.229")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mclear-tools/tabspaces.git")
             (commit "f552823f51f11d66492f754deb51abd709c08ed9")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "038il7nvymxh7wryskylz3ma4xl63jjvg6fvdjpa8x4ry60w4z5j"))))
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
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/copilot-emacs/copilot.el")
             (commit "b98754712ad05282e1bb392bb35e5918ed35c9b6")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n1kpjkr6vdfpb20x28bsg14piqv9vl0iz0jswc5qm0qgdmi23ah"))))
    (build-system emacs-build-system)
    (propagated-inputs (list upstream:emacs-editorconfig upstream:emacs-jsonrpc
                             upstream:emacs-track-changes upstream:emacs-f))
    (home-page "https://github.com/copilot-emacs/copilot.el")
    (synopsis "An unofficial Copilot plugin")
    (description "An unofficial Copilot plugin for Emacs.")
    (license #f)))
