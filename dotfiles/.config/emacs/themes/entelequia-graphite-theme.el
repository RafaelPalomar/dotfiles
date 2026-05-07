;;; entelequia-graphite-theme.el --- Graphite & Vellum -*- lexical-binding: t; -*-

;; Author: entelequia-theme · iter03
;; Maintainer: rafael
;; Keywords: faces themes
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (doom-themes "2.2.0"))

;;; Commentary:
;;
;; A tactile editorial dark theme — Graphite & Vellum.  The Emacs side of
;; the entelequia-theme design (see ~/src/entelequia-theme/, PKS
;; 20260507T172436).  Built on doom-themes for broad mode coverage,
;; reuses Dracula's design philosophy of good defaults across many modes
;; while substituting the palette to warm graphite + oxidised copper +
;; vellum cream.
;;
;; Palette
;;   ink        #1c1a18  warm graphite — bg
;;   slate      #28241f  raised paper — surface
;;   vellum     #ede4d0  warm cream — fg
;;   muted      #7a7268  stone grey — secondary
;;   copper     #a85a2c  burnt sienna — primary signal
;;   brass      #8a7437  aged brass — sparing tertiary
;;   sage       #7a8c5a  earth green — strings, success
;;   gold       #c08a3e  burnt gold — constants, numbers, warnings
;;   teal       #5c8c8c  oxidised teal — types, info
;;   mauve      #8b5a7c  printed mauve — preprocessor, unused
;;   urgent     #bf616a  alarm red — errors only

;;; Code:

(require 'doom-themes)

(defgroup entelequia-graphite-theme nil
  "Options for the entelequia-graphite theme."
  :group 'doom-themes)

(def-doom-theme entelequia-graphite
  "A tactile editorial dark theme — Graphite & Vellum."

  ;; Name        Default      256-color    16-color
  ((bg          '("#1c1a18"   "#1c1a18"    "black"))
   (bg-alt      '("#28241f"   "#28241f"    "black"))
   (base0       '("#0d0c0b"   "#0d0c0b"    "black"))
   (base1       '("#1c1a18"   "#1c1a18"    "brightblack"))
   (base2       '("#28241f"   "#28241f"    "brightblack"))
   (base3       '("#3a3530"   "#3a3530"    "brightblack"))
   (base4       '("#4d463f"   "#4d463f"    "brightblack"))
   (base5       '("#7a7268"   "#7a7268"    "brightblack"))
   (base6       '("#a6998a"   "#a6998a"    "white"))
   (base7       '("#c8b9a6"   "#c8b9a6"    "white"))
   (base8       '("#ede4d0"   "#ede4d0"    "brightwhite"))
   (fg          '("#ede4d0"   "#ede4d0"    "brightwhite"))
   (fg-alt      '("#a6998a"   "#a6998a"    "white"))

   (grey        base5)
   ;; copper occupies "orange" + "red" registers — printed-ink red
   (red         '("#bf616a"   "#bf616a"    "red"))
   (orange      '("#a85a2c"   "#a85a2c"    "brightred"))
   (green       '("#7a8c5a"   "#7a8c5a"    "green"))
   (teal        '("#5c8c8c"   "#5c8c8c"    "brightgreen"))
   (yellow      '("#c08a3e"   "#c08a3e"    "yellow"))
   (blue        '("#7395a8"   "#7395a8"    "brightblue"))
   (dark-blue   '("#5c8c8c"   "#5c8c8c"    "blue"))
   (magenta     '("#8b5a7c"   "#8b5a7c"    "magenta"))
   (violet      '("#a3819b"   "#a3819b"    "brightmagenta"))
   (cyan        '("#6f8a8a"   "#6f8a8a"    "brightcyan"))
   (dark-cyan   '("#5c8c8c"   "#5c8c8c"    "cyan"))

   ;; Face categories — doom-themes derives common faces from these slots.
   (highlight       orange)
   (vertical-bar    (doom-darken base1 0.1))
   (selection       bg-alt)
   (builtin         yellow)
   (comments        grey)
   (doc-comments    (doom-lighten grey 0.15))
   (constants       yellow)
   (functions       (doom-lighten orange 0.15))
   (keywords        orange)
   (methods         (doom-lighten orange 0.15))
   (operators       fg)
   (type            teal)
   (strings         green)
   (variables       fg)
   (numbers         yellow)
   (region          bg-alt)
   (error           red)
   (warning         yellow)
   (success         green)
   (vc-modified     yellow)
   (vc-added        green)
   (vc-deleted      red)

   ;; Custom slots used by the face overrides below
   (modeline-bg              bg-alt)
   (modeline-bg-alt          bg)
   (modeline-bg-inactive     bg)
   (modeline-bg-inactive-alt bg)
   (modeline-fg              fg)
   (modeline-fg-alt          grey)
   (-modeline-pad
    (when doom-entelequia-graphite-padded-modeline
      (if (integerp doom-entelequia-graphite-padded-modeline)
          doom-entelequia-graphite-padded-modeline
        4))))


  ;; ── Face overrides ─────────────────────────────────────────────────────
  ((cursor :background ,orange)
   (fringe :background ,bg :foreground ,grey)
   (vertical-border :foreground ,base3)
   (link :foreground ,orange :underline t)
   (link-visited :foreground ,(doom-darken orange 0.15) :underline t)

   ;; Mode line — copper bottom-rule echoes the polybar masthead's lower edge
   (mode-line
    :background ,modeline-bg :foreground ,modeline-fg
    :box (:line-width 1 :color ,orange))
   (mode-line-inactive
    :background ,modeline-bg-inactive :foreground ,modeline-fg-alt
    :box (:line-width 1 :color ,base3))
   (mode-line-emphasis :foreground ,orange)
   (mode-line-buffer-id :foreground ,orange :weight bold)

   ;; Selection / search — copper as the active assertion
   (region :background ,bg-alt :foreground nil :extend t)
   (secondary-selection :background ,base2)
   (isearch :background ,orange :foreground ,bg :weight bold)
   (lazy-highlight :background ,base3 :foreground ,yellow)

   ;; Show-paren — copper underline for matching pair
   (show-paren-match :foreground ,orange :weight bold :underline t)
   (show-paren-mismatch :foreground ,red :weight bold :underline t)

   ;; ── Org mode ────────────────────────────────────────────────────────
   (org-document-title :foreground ,orange :weight bold :height 1.4)
   (org-document-info :foreground ,grey)
   (org-document-info-keyword :foreground ,grey)
   (org-level-1 :foreground ,orange :weight bold :height 1.25 :extend t)
   (org-level-2 :foreground ,yellow :weight bold :height 1.15 :extend t)
   (org-level-3 :foreground ,green :weight bold :height 1.08 :extend t)
   (org-level-4 :foreground ,teal :weight bold :extend t)
   (org-level-5 :foreground ,magenta :weight bold :extend t)
   (org-level-6 :foreground ,fg :weight bold :extend t)
   (org-level-7 :foreground ,grey :weight bold :extend t)
   (org-level-8 :foreground ,grey :extend t)

   (org-link :foreground ,teal :underline t)
   (org-todo :foreground ,red :weight bold)
   (org-done :foreground ,green :weight bold)
   (org-headline-done :foreground ,grey :strike-through t)
   (org-checkbox :foreground ,orange)
   (org-checkbox-statistics-todo :foreground ,red :weight bold)
   (org-checkbox-statistics-done :foreground ,green :weight bold)

   (org-block            :background ,bg-alt :foreground ,fg :extend t)
   (org-block-begin-line :background ,bg-alt :foreground ,grey :slant italic :extend t)
   (org-block-end-line   :background ,bg-alt :foreground ,grey :slant italic :extend t)
   (org-meta-line        :foreground ,grey :slant italic)
   (org-tag              :foreground ,orange :weight bold)
   (org-priority         :foreground ,red :weight bold)
   (org-date             :foreground ,yellow :underline t)
   (org-special-keyword  :foreground ,orange)
   (org-table            :foreground ,fg)
   (org-formula          :foreground ,yellow)
   (org-quote            :foreground ,fg :slant italic :extend t)
   (org-verse            :slant italic)
   (org-code             :foreground ,orange)
   (org-verbatim         :foreground ,yellow)
   (org-ellipsis         :foreground ,orange)
   (org-drawer           :foreground ,grey)
   (org-special-keyword  :foreground ,orange)
   (org-warning          :foreground ,red :weight bold)

   ;; Org-agenda
   (org-agenda-structure   :foreground ,orange :weight bold)
   (org-agenda-date        :foreground ,yellow :weight bold)
   (org-agenda-date-today  :foreground ,orange :weight bold :underline t)
   (org-agenda-date-weekend :foreground ,grey)
   (org-agenda-done        :foreground ,green)
   (org-agenda-clocking    :background ,bg-alt :foreground ,orange)
   (org-scheduled          :foreground ,fg)
   (org-scheduled-today    :foreground ,orange)
   (org-scheduled-previously :foreground ,red)
   (org-time-grid          :foreground ,grey)

   ;; ── Magit ──────────────────────────────────────────────────────────
   (magit-section-heading :foreground ,orange :weight bold)
   (magit-section-heading-selection :foreground ,(doom-lighten orange 0.15) :weight bold)
   (magit-section-highlight :background ,bg-alt)
   (magit-branch-current :foreground ,orange :weight bold :box (:line-width 1 :color ,orange))
   (magit-branch-local :foreground ,teal)
   (magit-branch-remote :foreground ,green)
   (magit-tag :foreground ,yellow)
   (magit-hash :foreground ,grey)
   (magit-diff-added :background ,bg :foreground ,green :extend t)
   (magit-diff-added-highlight :background ,bg-alt :foreground ,(doom-lighten green 0.15) :extend t)
   (magit-diff-removed :background ,bg :foreground ,red :extend t)
   (magit-diff-removed-highlight :background ,bg-alt :foreground ,(doom-lighten red 0.15) :extend t)
   (magit-diff-context :foreground ,grey :extend t)
   (magit-diff-context-highlight :background ,bg-alt :foreground ,fg :extend t)
   (magit-diff-hunk-heading :foreground ,grey :extend t)
   (magit-diff-hunk-heading-highlight :background ,bg-alt :foreground ,orange :weight bold :extend t)
   (magit-diff-file-heading :foreground ,fg :weight bold)
   (magit-diff-file-heading-highlight :background ,bg-alt :foreground ,fg :weight bold)
   (magit-log-author :foreground ,yellow)
   (magit-log-date :foreground ,grey)
   (magit-log-graph :foreground ,grey)

   ;; ── Company / corfu / completion ──────────────────────────────────
   (company-tooltip :background ,bg-alt :foreground ,fg)
   (company-tooltip-selection :background ,orange :foreground ,bg)
   (company-tooltip-annotation :foreground ,grey)
   (company-tooltip-common :foreground ,orange :weight bold)
   (company-scrollbar-bg :background ,bg)
   (company-scrollbar-fg :background ,grey)
   (corfu-default :background ,bg-alt :foreground ,fg)
   (corfu-current :background ,orange :foreground ,bg)
   (corfu-bar :background ,grey)

   ;; ── Ivy / counsel (the user's completion stack) ───────────────────
   (ivy-current-match :background ,orange :foreground ,bg :weight bold :extend t)
   (ivy-minibuffer-match-face-1 :foreground ,yellow :weight bold)
   (ivy-minibuffer-match-face-2 :foreground ,green :weight bold)
   (ivy-minibuffer-match-face-3 :foreground ,teal :weight bold)
   (ivy-minibuffer-match-face-4 :foreground ,magenta :weight bold)
   (ivy-confirm-face :foreground ,green)
   (ivy-match-required-face :foreground ,red)
   (ivy-subdir :foreground ,orange)
   (ivy-modified-buffer :foreground ,yellow)
   (ivy-virtual :foreground ,grey :slant italic)
   (counsel-key-binding :foreground ,orange :weight bold)

   ;; ── Helpful / which-key ───────────────────────────────────────────
   (which-key-key-face :foreground ,orange :weight bold)
   (which-key-group-description-face :foreground ,yellow)
   (which-key-command-description-face :foreground ,fg)
   (which-key-separator-face :foreground ,grey)

   ;; ── Line numbers ──────────────────────────────────────────────────
   (line-number :foreground ,grey :background ,bg)
   (line-number-current-line :foreground ,orange :background ,bg-alt :weight bold)

   ;; ── Whitespace ────────────────────────────────────────────────────
   (whitespace-trailing :background ,red :foreground ,bg)
   (whitespace-tab :foreground ,base3)
   (whitespace-space :foreground ,base3)
   (whitespace-newline :foreground ,base3)
   (whitespace-indentation :foreground ,base3)
   (whitespace-line :background ,bg-alt)

   ;; ── Diff / ediff ──────────────────────────────────────────────────
   (diff-added :background ,bg :foreground ,green)
   (diff-removed :background ,bg :foreground ,red)
   (diff-changed :foreground ,yellow)
   (diff-context :foreground ,grey)
   (diff-hunk-header :foreground ,orange :weight bold)
   (diff-file-header :foreground ,fg :weight bold)
   (ediff-current-diff-A :background ,(doom-blend red bg 0.2))
   (ediff-current-diff-B :background ,(doom-blend green bg 0.2))
   (ediff-current-diff-C :background ,(doom-blend yellow bg 0.2))
   (ediff-fine-diff-A :background ,(doom-blend red bg 0.4))
   (ediff-fine-diff-B :background ,(doom-blend green bg 0.4))
   (ediff-fine-diff-C :background ,(doom-blend yellow bg 0.4))

   ;; ── Tree-sitter / lsp / treemacs (covered by doom-themes already,
   ;; but explicit copper-on-graphite overrides for visibility) ───────
   (tree-sitter-hl-face:keyword :foreground ,orange :weight bold)
   (tree-sitter-hl-face:function :foreground ,(doom-lighten orange 0.15))
   (tree-sitter-hl-face:type :foreground ,teal)
   (tree-sitter-hl-face:string :foreground ,green)
   (tree-sitter-hl-face:comment :foreground ,grey :slant italic)

   (lsp-face-highlight-textual :background ,bg-alt :foreground ,orange)
   (lsp-face-highlight-read :background ,bg-alt :foreground ,teal)
   (lsp-face-highlight-write :background ,bg-alt :foreground ,red)

   ;; ── Dired ─────────────────────────────────────────────────────────
   (dired-directory :foreground ,orange :weight bold)
   (dired-symlink :foreground ,teal)
   (dired-marked :foreground ,orange :weight bold)
   (dired-flagged :foreground ,red :weight bold)

   ;; ── Eshell / term ─────────────────────────────────────────────────
   (eshell-prompt :foreground ,orange :weight bold)
   (eshell-ls-directory :foreground ,orange :weight bold)
   (eshell-ls-symlink :foreground ,teal)
   (eshell-ls-executable :foreground ,green :weight bold)
   (eshell-ls-archive :foreground ,magenta)
   (eshell-ls-special :foreground ,yellow)

   ;; ── Misc ─────────────────────────────────────────────────────────
   (error :foreground ,red :weight bold)
   (warning :foreground ,yellow :weight bold)
   (success :foreground ,green :weight bold)
   (button :foreground ,orange :underline t)
   (escape-glyph :foreground ,yellow)
   (homoglyph :foreground ,yellow)
   (header-line :background ,bg-alt :foreground ,orange :weight bold)

   (font-lock-keyword-face :foreground ,orange :weight bold)
   (font-lock-comment-face :foreground ,grey :slant italic)
   (font-lock-comment-delimiter-face :foreground ,grey)
   (font-lock-doc-face :foreground ,grey :slant italic)
   (font-lock-string-face :foreground ,green)
   (font-lock-builtin-face :foreground ,yellow)
   (font-lock-constant-face :foreground ,yellow)
   (font-lock-type-face :foreground ,teal)
   (font-lock-variable-name-face :foreground ,fg)
   (font-lock-function-name-face :foreground ,(doom-lighten orange 0.15))
   (font-lock-preprocessor-face :foreground ,magenta)
   (font-lock-warning-face :foreground ,red :weight bold)
   (font-lock-negation-char-face :foreground ,red)))

(provide-theme 'entelequia-graphite)

;;; entelequia-graphite-theme.el ends here
