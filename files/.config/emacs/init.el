;; -*- lexical-binding: t; -*-

            ;;; This file is generated from the =emacs.org= file in my dotfiles repository!

            ;;; ----- Basic Configuration -----

;; Core settings
(setq ;; Flash the UI instead of beeping
 visible-bell t

 ;; Yes, this is Emacs
 inhibit-startup-message t

 ;; Instruct auto-save-mode to save to the current file, not a backup file
 auto-save-default nil

 ;; No backup files, please
 make-backup-files nil

 ;; Make it easy to cycle through previous items in the mark ring
 set-mark-command-repeat-pop t

 ;; Don't warn on large files
 large-file-warning-threshold nil

 ;; Follow symlinks to VC-controlled files without warning
 ;; TODO This goes potentially out as I'll be using magit
 ;;vc-follow-symlinks t

 ;; Don't warn on advice
 ad-redefinition-action 'accept

 ;; Revert Dired and other buffers
 global-auto-revert-non-file-buffers t

 ;; Silence compiler warnings as they can be pretty disruptive
 ;; TODO This goes potentially out
 ;; native-comp-async-report-warnings-errors nil
 )

;; Core modes
(repeat-mode 1)                ;; Enable repeating key maps
(menu-bar-mode 0)              ;; Hide the menu bar
(tool-bar-mode 0)              ;; Hide the tool bar
(savehist-mode 1)              ;; Save minibuffer history
(scroll-bar-mode 0)            ;; Hide the scroll bar
(xterm-mouse-mode 1)           ;; Enable mouse events in terminal Emacs
(display-time-mode 1)          ;; Display time in mode line / tab bar
(fido-vertical-mode 1)         ;; Improved vertical minibuffer completions
(column-number-mode 1)         ;; Show column number on mode line
(tab-bar-history-mode 1)       ;; Remember previous tab window configurations
(auto-save-visited-mode 1)     ;; Auto-save files at an interval
(global-visual-line-mode 1)    ;; Visually wrap long lines in all buffers
(global-auto-revert-mode 1)    ;; Refresh buffers with changed local files

;; Tabs to spaces
(setq-default indent-tabs-mode nil
              tab-width 2)

;; Display line numbers in programming modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Make icomplete slightly more convenient
;; TODO this potentiall goes out
;; (keymap-set icomplete-fido-mode-map "M-h" 'icomplete-fido-backward-updir)
;; (keymap-set icomplete-fido-mode-map "TAB" 'icomplete-force-complete)

;; Delete trailing whitespace before saving buffers
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Move customization settings out of init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file t))

;; Match completion substrings that may be out of order
;; TODO this poentially goes out
;; (defun dw/override-fido-completion-styles ()
;;   (setq-local completion-styles '(substring partial-completion emacs22)))

;;(add-hook 'icomplete-minibuffer-setup-hook 'dw/override-fido-completion-styles)

;;; --------- Enable Evil Mode ---------

;; This is required here for evil-collection to work
(setq evil-want-keybinding nil)

;; configure undo system
(setq evil-undo-system 'undo-fu)

(require 'evil)

(evil-mode 1)

  ;;; --------- Evil Collection ---------

(evil-commentary-mode)

    ;;; --------- Evil Collection ---------

(evil-collection-init)

    ;;; --------- Evil Surround ---------
(global-evil-surround-mode 1)

        ;;; --------- Evil Snipe ---------

;;   (require 'evil-snipe)

;;   (evil-snipe-mode +1)
;;   (evil-snipe-override-mode +1)

;;   (setq evil-snipe-scope 'whole-visible)



;; ;; Play nicely with avy
;; (define-key evil-snipe-parent-transient-map (kbd "C-;")
;;   (evilem-create 'evil-snipe-repeat
;;                  :bind ((evil-snipe-scope 'buffer)
;;                         (evil-snipe-enable-highlight)
;;                         (evil-snipe-enable-incremental-highlight))))

;;; ----- Appearance -----

(defun dw/set-terminal-title (title)
  (send-string-to-terminal (format "\e]0;%s\a" title)))

(defun dw/clear-background-color (&optional frame)
  (interactive)
  (or frame (setq frame (selected-frame)))
  "unsets the background color in terminal mode"
  (unless (display-graphic-p frame)
    ;; Set the terminal to a transparent version of the background color
    (send-string-to-terminal
     (format "\033]11;[90]%s\033\\"
             (face-attribute 'default :background)))
    (set-face-background 'default "unspecified-bg" frame)))

;; Clear the background color for transparent terminals
(unless (display-graphic-p)
  (add-hook 'after-make-frame-functions 'dw/clear-background-color)
  (add-hook 'window-setup-hook 'dw/clear-background-color)
  (add-hook 'ef-themes-post-load-hook 'dw/clear-background-color))

(when (display-graphic-p)
  (set-face-attribute 'default nil
                      :font "JetBrains Mono"
                      :weight 'normal
                      :height 140)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil
                      :font "JetBrains Mono"
                      :weight 'normal
                      :height 140)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil
                      :font "Iosevka Aile"
                      :height 120
                      :weight 'normal)

  ;; Make frames transparent
  (set-frame-parameter (selected-frame) 'alpha-background 95)
  (add-to-list 'default-frame-alist '(alpha-background . 95))
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(use-package modus-themes
  :ensure nil
  :demand t
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-common-palette-overrides
   `((bg-main "#292D3E")
     (bg-active bg-main)
     (fg-main "#EEFFFF")
     (fg-active fg-main)
     (fringe unspecified)
     (border-mode-line-active unspecified)
     (border-mode-line-inactive unspecified)
     (fg-mode-line-active "#A6Accd")
     (bg-mode-line-active "#232635")
     (fg-mode-line-inactive "#676E95")
     (bg-mode-line-inactive "#282c3d")
     (bg-tab-bar      "#242837")
     (bg-tab-current  bg-main)
     (bg-tab-other    bg-active)
     (fg-prompt "#c792ea")
     (bg-prompt unspecified)
     (bg-hover-secondary "#676E95")
     (bg-completion "#2f447f")
     (fg-completion white)
     (bg-region "#3C435E")
     (fg-region white)

     (fg-heading-0 "#82aaff")
     (fg-heading-1 "#82aaff")
     (fg-heading-2 "#c792ea")
     (fg-heading-3 "#bb80b3")
     (fg-heading-4 "#a1bfff")

     (fg-prose-verbatim "#c3e88d")
     (bg-prose-block-contents "#232635")
     (fg-prose-block-delimiter "#676E95")
     (bg-prose-block-delimiter bg-prose-block-contents)

     (accent-1 "#79a8ff")

     (keyword "#89DDFF")
     (builtin "#82aaff")
     (comment "#676E95")
     (string "#c3e88d")
     (fnname "#82aaff")
     (type "#c792ea")
     (variable "#ffcb6b")
     (docstring "#8d92af")
     (constant "#f78c6c")))
  :init
  (load-theme 'modus-vivendi-tinted t)
  (add-hook 'modus-themes-after-load-theme-hook #'dw/clear-background-color))

;; Make vertical window separators look nicer in terminal Emacs
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

;; Clean up the mode line
(setq-default mode-line-format
              '("%e" "  "
                (:propertize
                 ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote))
                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                mode-line-position
                mode-line-format-right-align
                "  "
                (project-mode-line project-mode-line-format)
                " "
                (vc-mode vc-mode)
                "  "
                mode-line-modes
                mode-line-misc-info
                "  ")
              project-mode-line t
              mode-line-buffer-identification '(" %b")
              mode-line-position-column-line-format '(" %l:%c"))

;; Move global mode string to the tab-bar and hide tab close buttons
(setq tab-bar-close-button-show nil
      tab-bar-separator " "
      tab-bar-format '(tab-bar-format-menu-bar
                       tab-bar-format-tabs-groups
                       tab-bar-separator
                       tab-bar-format-align-right
                       tab-bar-format-global))

;; Turn on the tab-bar
(tab-bar-mode 1)

;; Customize time display
(setq display-time-load-average nil
      display-time-format "%l:%M %p %b %d W%U"
      display-time-world-time-format "%a, %d %b %I:%M %p %Z"
      display-time-world-list
      '(("Etc/UTC" "UTC")
        ("Europe/Oslo" "Oslo")
        ("America/Los_Angeles" "Seattle")
        ("America/Denver" "Denver")
        ("America/Boston" "Boston")
        ("Pacific/Auckland" "Auckland")
        ("Asia/Shanghai" "Shanghai")
        ("Asia/Kolkata" "Hyderabad")))

;;; ----- Essential Org Mode Configuration -----

(setq org-ellipsis " ▾"
      org-startup-folded 'content
      org-cycle-separator-lines 2
      org-fontify-quote-and-verse-blocks t)

;; Indent org-mode buffers for readability
(add-hook 'org-mode-hook #'org-indent-mode)

;; Set up Org Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)))

;; Use org-tempo
(use-package org-tempo
  :ensure nil
  :demand t
  :config
  (dolist (item '(("sh" . "src sh")
                  ("el" . "src emacs-lisp")
                  ("li" . "src lisp")
                  ("sc" . "src scheme")
                  ("ts" . "src typescript")
                  ("py" . "src python")
                  ("yaml" . "src yaml")
                  ("json" . "src json")
                  ("einit" . "src emacs-lisp :tangle emacs/init.el")
                  ("emodule" . "src emacs-lisp :tangle emacs/modules/dw-MODULE.el")))
    (add-to-list 'org-structure-template-alist item)))

;;; ------------ Projectile setup --------------

(projectile-mode +1)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;;; ---------------- Configure cmake mode --------------------

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

;;; ---------------- General Definitions -----------------

(setq general-override-states '(insert
                                emacs
                                hybrid
                                normal
                                visual
                                motion
                                operator
                                replace))

(require 'general)

(defconst my-leader "SPC")

(general-create-definer my-leader-def
  :prefix my-leader)

(general-override-mode)

(general-define-key
 "C-=" 'text-scale-increase
 "C--" 'text-scale-decrease
 "<C-wheel-down>" 'text-scale-decrease
 "<C-wheel-up>" 'text-scale-increase)

(my-leader-def
  :states '(motion normal visual)
  :keymaps 'override ;; https://github.com/noctuid/general.el/issues/99#issuecomment-360914335

  ;; map universal argument to SPC-u
  "u" '(universal-argument :which-key "Universal argument")
  ";" '(eval-region :which-key "eval-region")
  "SPC" '(projectile-find-file :which-key "Projectile find file")
  "C-SPC" '(projectile-find-file-other-frame :which-key "Projectile find file (new frame)")
  "S-SPC" '(projectile-find-file-other-frame :which-key "Projectile find file (new frame)")

  "ff" '(find-file :which-key "Find file")
  "fs" '(save-buffer :which-key "Save buffer")
  "qq" '(evil-quit :which-key "Quit Emacs")

  "x" '(open-scratch-buffer :which-key "Open scratch buffer")
  "d" '(dired-jump :which-key "dired-jump")
  "/" '(consult-ripgrep :which-key "consult-ripgrep")
                                        ;"[" '(+tab-bar/switch-to-prev-tab :which-key "+tab-bar/switch-to-prev-tab")
                                        ;"]" '(+tab-bar/switch-to-next-tab :which-key "+tab-bar/switch-to-next-tab")
  "v" '(vterm-toggle :which-key "vterm-toggle")
  "a" '(ace-window :which-key "ace-window")
  "l" '(ace-window :which-key "ace-window")

  ;; editor
  ;; "e" '(:ignore t :which-key "Editor")
  ;; "eu" '(vundo :which-key "vundo")
  ;; "ev" '(vundo :which-key "vundo")
  ;; "er" '(query-replace :which-key "query-replace")
                                        ;"ec" '(consult-theme :which-key "consult-theme")
  "ep" '(point-to-register :which-key "point-to-register")
  "es" '(consult-register-store :which-key "consult-register-store")
  "ej" '(jump-to-register :which-key "jump-to-register")
  "ef" '(:ignore t :which-key "Fold")
  "efh" '(hs-hide-block :which-key "hs-hide-block")
  "efs" '(hs-show-block :which-key "hs-show-block")
  "efa" '(hs-show-all :which-key "hs-show-all")


  ;; consult
  "c" '(:ignore t :which-key "consult")
  "cf" '(consult-flymake :which-key "consult-flymake")
  "ct" '(consult-theme :which-key "consult-theme")
                                        ;"cg" '(:ignore t :which-key "Grep")
                                        ;"cgr" '(consult-ripgrep :which-key "consult-ripgrep")
                                        ;"cgg" '(consult-git-grep :which-key "consult-git-grep")
                                        ;"cb" '(consult-buffer :which-key "consult-buffer")


  ;; buffer
                                        ;"TAB" '(switch-to-prev-buffer :which-key "Prev buffer")
  "b" '(:ignore t :which-key "Buffer")
  "bb" '(consult-buffer :which-key "consult-buffer")
  "b[" '(previous-buffer :which-key "Previous buffer")
  "b]" '(next-buffer :which-key "Next buffer")
  "bd" '(kill-current-buffer :which-key "Kill buffer")
  "bk" '(kill-current-buffer :which-key "Kill buffer")
  "bl" '(evil-switch-to-windows-last-buffer :which-key "Switch to last buffer")
  "br" '(revert-buffer-no-confirm :which-key "Revert buffer")
  "bK" '(kill-other-buffers :which-key "Kill other buffers")


  ;; open
  "o" '(:ignore t :which-key "Open")
  "oc" '(open-init-file :which-key "Open init.el")


  ;; project
  "p" '(:ignore t :which-key "Project")
  "pp" '(projectile-switch-project :which-key "Switch Project")
  "po" '(projectile-find-other-file :which-key "projectile-find-other-file")


  ;; help
  "h" '(:ignore t :which-key "Help")
  "hf" '(helpful-callable :which-key "describe-function")
  "hk" '(helpful-key :which-key "describe-key")
  "hv" '(helpful-variable :which-key "describe-variable")
  "ho" '(helpful-symbol :which-key "describe-symbol")
  "hm" '(describe-mode :which-key "describe-mode")
  "hF" '(describe-face :which-key "describe-face")
  "hw" '(where-is :which-key "where-is")
  "h." '(display-local-help :which-key "display-local-help")


  ;; window
  "w" '(:ignore t :which-key "Window")
  "ww" '(ace-window :which-key "ace-window")
  "ws" '(evil-window-split :which-key "evil-window-split")
  "wv" '(evil-window-vsplit :which-key "evil-window-vsplit")
  "wd" '(evil-window-delete :which-key "evil-window-delete")
  "wm" '(ace-delete-window :which-key "ace-delete-window")


  ;; toggles
  "t" '(:ignore t :which-key "Toggles")
                                        ;"ta" '(corfu-mode :which-key "corfu-mode") ;; 'a' for autocomplete
  "ts" '(flyspell-mode :which-key "flyspell-mode")
  "tf" '(flyspell-mode :which-key "flyspell-mode")
  "tc" '(flymake-mode :which-key "flymake-mode")
  "tg" '(evil-goggles-mode :which-key "evil-goggles")
  "tI" '(toggle-indent-style :which-key "Indent style")
  "tv" '(visual-line-mode :which-key "visual-line-mode")


  ;; narrow
  "N" '(:ignore t :which-key "Narrow")
  "Nr" '(narrow-to-region :which-key "narrow-to-region")
  "Nw" '(widen :which-key "widen")


  ;; tabs
  "TAB" '(:ignore t :which-key "Tabs")
  "TAB TAB" '(tab-bar-switch-to-tab :which-key "tab-bar-switch-to-tab")
  "TAB [" '(+tab-bar/switch-to-prev-tab :which-key "+tab-bar/switch-to-prev-tab")
  "TAB ]" '(t+ab-bar/switch-to-next-tab :which-key "+tab-bar/switch-to-next-tab")
  "TAB n" '(+tab-bar/add-new :which-key "+tab-bar/add-new")
  "TAB k" '(+tab-bar/close-tab :which-key "+tab-bar/close-tab")
  "TAB d" '(+tab-bar/close-tab :which-key "+tab-bar/close-tab")
  "TAB K" '(+tab-bar/close-all-tabs-except-current :which-key "+tab-bar/close-all-tabs-except-current")
  "TAB r" '(tab-rename :which-key "tab-rename")

  ;; Magit
  "gg" '(magit-status :which-key "magit-status")
  "gC" '(magit-clone :which-key "magit-clone")

  ;; AVY
  "gs" '(avy-goto-char-2 :whihc-key "avy-goto-char-2")
  )

;;; ----------- AVY --------------

(require 'avy)

(setq magit-status-buffer-switch-function 'switch-to-buffer)

(require 'beacon)

(beacon-mode 1)

;;; --------- Perspective Mode -------

(require 'perspective)
(global-set-key (kbd "C-x C-b") 'persp-list-buffers)
(customize-set-variable 'persp-mode-prefix-key (kbd "C-c M-p"))
(persp-mode)
