;; -*- lexical-binding: t; -*-

      ;;; This file is generated from the =emacs.org= file in my dotfiles repository!
      ;;; ----- Basic Configuration -----

;; Increase the garbage collection threshold during startup for faster startup
(setq gc-cons-threshold most-positive-fixnum)

;; Reset garbage collection thresholds after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))  ;; 16MB
            (setq gc-cons-percentage 0.1)))

(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)

(setq network-enable-ipv6 nil)
(setq starttls-use-gnutls t
      starttls-gnutls-program "gnutls-cli")

;; Core settings
(setq visible-bell t                         ;; Flash the screen instead of beeping
      inhibit-startup-message t              ;; Suppress the startup message
      inhibit-startup-screen t               ;; Disable the startup screen
      initial-scratch-message ";; Welcome to Emacs!\n\n"  ;; Set the scratch message
      make-backup-files nil                  ;; Disable backup files
      auto-save-default nil                  ;; Disable auto-saving to backup files
      ad-redefinition-action 'accept         ;; Silence function redefinition warnings
      ring-bell-function 'ignore             ;; Disable the bell completely
      vc-follow-symlinks t                   ;; Always follow symlinks
      large-file-warning-threshold nil       ;; Disable large file warnings
      custom-file (expand-file-name "custom.el" user-emacs-directory) ;; Set custom file
      frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b")))                     ;; Show full path in frame title
      )

;; Load the custom file if it exists
(when (file-exists-p custom-file)
  (load custom-file t))

;; Set default encoding to UTF-8
(prefer-coding-system 'utf-8)

;; Set tabs to spaces and define tab width
(setq-default indent-tabs-mode nil           ;; Use spaces instead of tabs
              tab-width 2)                   ;; Set default tab width to 2

;; Simplify the interface
(menu-bar-mode -1)                           ;; Disable the menu bar
(tool-bar-mode -1)                           ;; Disable the tool bar
(scroll-bar-mode -1)                         ;; Disable the scroll bar

;; (fset 'geiser-xref-backend (lambda (&rest _) nil))

;; (with-eval-after-load 'xref
;;   (remove-hook 'xref-backend-functions #'geiser-xref-backend))

;; (with-eval-after-load 'geiser
;;   (add-hook 'geiser-mode-hook
;;             (lambda ()
;;               (setq-local xref-backend-functions
;;                           (remq 'geiser-xref-backend xref-backend-functions)))))

;; (with-eval-after-load 'geiser
;;   (add-hook 'geiser-mode-hook
;;             (lambda ()
;;               (add-hook 'xref-backend-functions #'geiser-xref-backend nil t))))

;; Core modes
(repeat-mode 1)                              ;; Enable repeat mode
(savehist-mode 1)                            ;; Save minibuffer history
(save-place-mode 1)                          ;; Remember cursor positions in files
(recentf-mode 1)                             ;; Enable recent files mode
(which-key-mode 1)                           ;; Enable which-key mode (if installed)
(column-number-mode 1)                       ;; Show column numbers
(display-time-mode 1)                        ;; Display time in mode line
(global-visual-line-mode 1)                  ;; Wrap long lines visually
(xterm-mouse-mode 1)                         ;; Enable mouse support in terminal
(auto-save-visited-mode 1)                   ;; Auto-save files at intervals
(tab-bar-history-mode 1)                     ;; Enable tab bar history
(global-auto-revert-mode 1)                  ;; Refresh buffers when files change

;; Display line numbers in programming modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Delete trailing whitespace before saving files
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Set default directory to home
(setq default-directory "~/")

;; Fix locale settings (if needed)
(when (and (not (getenv "LC_ALL"))
           (or (not (getenv "LANG"))
               (string= (getenv "LANG") "")))
  (setenv "LANG" "en_US.UTF-8"))

;; -*- lexical-binding: t; -*-

;;; This file is generated from the =emacs.org= file in my dotfiles repository!

;;; ----- Basic Configuration -----

;; Core settings
(setq visible-bell t                        ;; Flash the UI instead of beeping
      inhibit-startup-message t             ;; Yes, this is emacs
      auto-save-default nil                 ;; Save to the current file, not a backup file
      make-backup-files nil                 ;; No backup files, please
      large-file-warning-threshold nil      ;; Don't warn on large files
      ad-redefinition-action 'accept        ;; Don't warn on advice
      global-auto-revert-non-file-buffers t ;; Revert Dired and other buffers
      )

;; Tabs to spaces
(setq-default indent-tabs-mode nil
              tab-width 2)

;; Display line numbers in programming modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Delete trailing whitespace before saving buffers
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Move customization settings out of init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file t))

;; Load nano-theme without installing it via package.el
(use-package nano-theme
  :ensure nil
  :config
  ;; Use the dark variant of the theme
  (nano-dark))

(require 'ansi-color)
(defun my/compilation-ansi-colorize ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))
(add-hook 'compilation-filter-hook #'my/compilation-ansi-colorize)

;; Enable nano-modeline for a minimalist mode line
(use-package nano-modeline
  :ensure nil
  :config
  (add-hook 'prog-mode-hook            'nano-modeline-prog-mode)
  (add-hook 'text-mode-hook            'nano-modeline-text-mode)
  (add-hook 'org-mode-hook             'nano-modeline-org-mode)
  (add-hook 'pdf-view-mode-hook        'nano-modeline-pdf-mode)
  (add-hook 'mu4e-headers-mode-hook    'nano-modeline-mu4e-headers-mode)
  (add-hook 'mu4e-view-mode-hook       'nano-modeline-mu4e-message-mode)
  (add-hook 'elfeed-show-mode-hook     #'nano-modeline-elfeed-entry-mode)
  (add-hook 'elfeed-search-mode-hook   #'nano-modeline-elfeed-search-mode)
  (add-hook 'term-mode-hook            'nano-modeline-term-mode)
  (add-hook 'xwidget-webkit-mode-hook  #'nano-modeline-xwidget-mode)
  (add-hook 'messages-buffer-mode-hook 'nano-modeline-message-mode)
  (add-hook 'org-capture-mode-hook     'nano-modeline-org-capture-mode)
  (add-hook 'org-agenda-mode-hook      'nano-modeline-org-agenda-mode)
  )

;; Font configurations
(when (display-graphic-p)
  ;; Set default font for fixed-pitch (monospace) text
  (set-face-attribute 'default nil
                      :font "JetBrains Mono"
                      :weight 'normal)

  ;; Set the fixed-pitch face
  (set-face-attribute 'fixed-pitch nil
                      :inherit 'default
                      :font "Fira Code Retina"
                      :weight 'normal))

  ;; Set the variable-pitch face
  ;; (set-face-attribute 'variable-pitch nil
  ;;                     :font "Cantarell"
  ;;                     :weight 'normal))

;; Adjust settings for terminal Emacs
(unless (display-graphic-p)
  ;; Clear background color for transparent terminals
  (set-face-background 'default "unspecified-bg"))

;; Simplify the mode line
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                "%b"           ; Buffer name
                " [%*] "       ; Modification status
                "("
                mode-line-position
                ") "
                mode-line-modes
                mode-line-end-spaces))

;; Load all-the-icons without installing via package.el
(use-package all-the-icons
  :ensure nil)

;; Tweak the tab bar to match the minimalist theme
(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil
      tab-bar-separator " | "
      tab-bar-format '(tab-bar-format-tabs
                       tab-bar-separator
                       tab-bar-format-align-right))

;; Enable the tab bar
(tab-bar-mode 1)

;; Set the frame title to show the buffer or file name
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Enable Evil Mode
(use-package evil
  :ensure nil
  :init
  ;; Required for Evil Collection
  (setq evil-want-keybinding nil)
  ;; Use `undo-fu` for undo system
  (setq evil-undo-system 'undo-fu)
  :config
  ;; Enable Evil Mode
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure nil
  :config
  ;; Initialize Evil Collection for all supported modes
  (evil-collection-init))

;; Add Evil Surround
(use-package evil-surround
  :ensure nil
  :config
  (global-evil-surround-mode 1))

;; Enable Evil Commentary
(use-package evil-commentary
  :ensure nil
  :config
  (evil-commentary-mode))

;; Add Visual Feedback with Evil Goggles
(use-package evil-goggles
  :ensure nil
  :config
  (evil-goggles-mode)
  ;; Optional: Use diff-mode faces
  (evil-goggles-use-diff-faces))

;; Enable Evil Matchit
(use-package evil-matchit
  :ensure nil
  :config
  (global-evil-matchit-mode 1))

;; Use Undo-Fu for Enhanced Undo/Redo
(use-package undo-fu
  :ensure nil)

;; Integrate Evil with Paredit for Lisp Editing
(use-package evil-paredit
  :ensure nil
  :after (evil paredit)
  :hook
  ((emacs-lisp-mode lisp-mode scheme-mode) . evil-paredit-mode))

;; Adjust Evil Keybindings and Settings
(setq evil-want-C-u-scroll t             ;; Enable Vim-style C-u scrolling
      evil-want-C-d-scroll t             ;; Enable Vim-style C-d scrolling
      evil-search-module 'evil-search    ;; Use Evil's search module
      evil-want-fine-undo t              ;; More granular undo steps
      evil-kill-on-visual-paste nil)     ;; Don't replace clipboard on paste

;; Customize Cursor Appearance
(setq evil-normal-state-cursor 'box      ;; Normal mode cursor is a box
      evil-insert-state-cursor 'bar      ;; Insert mode cursor is a bar
      evil-visual-state-cursor 'hollow)  ;; Visual mode cursor is hollow

;; Evil Org Mode Integration
(use-package evil-org
  :ensure nil
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  (add-hook 'org-mode-hook #'evil-org-mode)
  (require 'evil-org-agenda)
  (evil-org-set-key-theme '(navigation insert textobjects additional))
  (evil-org-agenda-set-keys))

;; Enhance Org Mode appearance with org-modern
(use-package org-modern
  :after (org)
  :ensure nil
  :hook
  (org-mode . org-modern-mode)
  :config
  ;; Enable org-modern globally for all Org buffers
  (global-org-modern-mode))

;; Enhance text layout with visual-fill-column
(use-package visual-fill-column
  :after (org)
  :ensure nil
  :hook (org-mode . visual-fill-column-mode)
  :config
  (setq visual-fill-column-width 100  ;; Set text width
        visual-fill-column-center-text t))  ;; Center the text

;; Enable Org Indent Mode for better alignment
(add-hook 'org-mode-hook 'org-indent-mode)

;; Use org-appear to show hidden emphasis markers on cursor
(use-package org-appear
  :after (org)
  :ensure nil
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-autokeywords t
        org-appear-delay 0.5))

;; Configure Org Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)
   (latex . t)
   (C . t)
   (mermaid . t)
   (dot . t)))

;; Don't ask for confirmation before executing code blocks
(setq org-confirm-babel-evaluate nil)

;; LaTeX export settings using minted
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-minted-options
      '(("frame" "lines")
        ("fontsize" "\\scriptsize")
        ("linenos" "true")))

(setq org-capture-templates
      '(("t" "TODO workflow")
        ("tt" "Work Todo" entry (file+olp "~/org/inbox.org" "Inbox")
         "* TODO %?\nEntered on %U\n  %i\n\n")
        ("tp" "Personal Todo" entry (file+olp "~/org/inbox-personal.org" "Inbox")
         "* TODO %? :personal:\nEntered on %U\n  %i\n\n")
        ("m" "Email Workflow")
        ("mf" "Follow Up" entry (file+olp "~/org/inbox.org" "E-Mail")
         "* TODO Follow up with %:fromname on [[%:link][%:subject]]
    SCHEDULED: %t
    DEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))

    %i" :immediate-finish nil)
        ("mr" "Reply" entry (file+olp "~/org/inbox.org" "E-Mail")
         "* TODO [#A] Reply to %:fromname on [[%:link][%:subject]]
    SCHEDULED: %t
    DEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))

    %i" :immediate-finish nil)))

(setq org-agenda-files '("~/org/inbox.org"
                         "~/org/archive.org"
                         "~/org/inbox-personal.org"
                         "~/org/archive-personal.org"))

(setq-default org-refile-targets '((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path t)

(use-package org-mime
  :ensure nil
  :after (mu4e org)
  :config
  ;; Set the default mail user agent to mu4e
  (setq mail-user-agent 'mu4e-user-agent)
  (setq org-mime-preserve-inline-images t)
  ;; Optional: Set default export options for HTML emails
  (setq org-mime-export-options '(:section-numbers nil
                                                   :with-author nil
                                                   :with-toc nil)))

(setq ob-mermaid-cli-path "/home/rafael/node_modules/.bin/mmdc")

;; Enable Ivy for enhanced completion
(use-package ivy
  :ensure nil
  :demand t
  :diminish
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t       ;; Extend searching to recent files and bookmarks
        ivy-count-format "(%d/%d) "     ;; Display the current and total number of candidates
        ivy-wrap t                      ;; Allow wrapping around completion candidates
        ivy-height 15                   ;; Set the height of the Ivy completion window
        ivy-fixed-height-minibuffer t ;; Maintain the completion window height
        )
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)))

;; Use Counsel to enhance built-in Emacs commands
(use-package counsel
  :ensure nil
  :demand t
  :after ivy
  :config
  (counsel-mode 1)
  ;; Replace some default commands with counsel alternatives
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x b") 'counsel-switch-buffer)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  ;; Additional keybindings for counsel commands
  (global-set-key (kbd "C-c k") 'counsel-rg)          ;; Ripgrep search
  (global-set-key (kbd "C-c g") 'counsel-git)         ;; Git files
  (global-set-key (kbd "C-c j") 'counsel-git-grep)    ;; Git grep
  (global-set-key (kbd "C-c L") 'counsel-load-library))

;; Enable Swiper for improved in-buffer searching
(use-package swiper
  :ensure nil
  :after ivy
  :bind (("C-s" . swiper)             ;; Replace default search with swiper
         ("C-r" . swiper)))           ;; Replace reverse search

;; Enhance Ivy with ivy-rich
(use-package ivy-rich
  :ensure nil
  :after ivy
  :init
  (ivy-rich-mode 1))

;; Load general.el for keybinding management
(use-package general
:ensure nil
:config
;; Set up 'SPC' as the leader key with precedence over minor modes
(general-create-definer my/leader-keys
  :states '(normal insert visual emacs)
  :keymaps 'override                 ;; Ensure it has precedence
  :prefix "SPC"
  :non-normal-prefix "M-SPC")
;; Additional keybindings
(general-define-key
 :states '(normal insert visual emacs)
 :keymaps 'override
 "C-+" 'text-scale-increase
 "C-=" 'text-scale-increase
 "C--" 'text-scale-decrease
 "C-0" '(text-scale-set :which-key "Reset text scale")))

;; Basic keybindings
(my/leader-keys
  "SPC" '(counsel-M-x :which-key "M-x")
  "f"   '(:ignore t :which-key "Files")
  "ff"  '(counsel-find-file :which-key "Find file")
  "fs"  '(save-buffer :which-key "Save buffer")
  "fr"  '(counsel-recentf :which-key "Recent files")
  "fS"  '(write-file :which-key "Save file as...")
  "b"   '(:ignore t :which-key "Buffers")
  "bb"  '(ivy-switch-buffer :which-key "Switch buffer")
  "bk"  '(kill-current-buffer :which-key "Kill buffer")
  "br"  '(revert-buffer :which-key "Revert buffer")
  "w"   '(:ignore t :which-key "Windows")
  "wd"  '(delete-window :which-key "Delete window")
  "wo"  '(delete-other-windows :which-key "Delete other windows")
  "ws"  '(split-window-below :which-key "Split window below")
  "wv"  '(split-window-right :which-key "Split window right")
  ;;"TAB" '(:ignore t :which-key "Tabs")
  ;;"TAB" '(switch-to-prev-buffer :which-key "Previous buffer")
  "u"   '(universal-argument :which-key "Universal argument")
  "q"   '(:ignore t :which-key "Quit/Restart")
  "qq"  '(save-buffers-kill-terminal :which-key "Quit Emacs"))

;; Search and completion
(my/leader-keys
  "/"    '(swiper :which-key "Swiper search")
  "s"    '(:ignore t :which-key "Search")
  "sa"   '(swiper-all :which-key "Swiper all buffers")
  "sb"   '(swiper :which-key "Search buffer")
  "sd"   '(counsel-rg :which-key "Ripgrep search")
  "sp"   '(counsel-projectile-rg :which-key "Search project with rg")
  "sg"   '(counsel-git-grep :which-key "Search in Git repo")
  "sr"   '(ivy-resume :which-key "Resume last search")
  )

(general-define-key
 :states '(normal visual)
 :keymaps 'override
 "SPC s s" '(avy-goto-char :which-key "Avy Goto Char")
 "SPC s w" '(avy-goto-word-1 :which-key "Avy Goto Word")
 "SPC s l" '(avy-goto-line :which-key "Avy Goto Line"))

;; Ensure counsel-projectile is loaded
(use-package counsel-projectile
  :ensure nil
  :after (counsel projectile)
  :config
  (counsel-projectile-mode 1))

;; Project management
(my/leader-keys
  "p"   '(:ignore t :which-key "Project")
  "pp"  '(counsel-projectile-switch-project :which-key "Switch project")
  "pf"  '(counsel-projectile-find-file :which-key "Find file in project")
  "pb"  '(counsel-projectile-switch-to-buffer :which-key "Switch buffer in project")
  "pd"  '(projectile-dired :which-key "Project Dired")
  "ps"  '(counsel-projectile-rg :which-key "Search in project")
  "pR"  '(projectile-replace :which-key "Replace in project")
  "pD"  '(projectile-kill-buffers :which-key "Kill project buffers")
  "pc"  '(projectile-compile-project :which-key "Compile project")
  "pC"  '(projectile-configure-project :which-key "Configure project")
  )

;; Window and buffer navigation
(my/leader-keys
  "w"   '(:ignore t :which-key "Windows")
  "wh"  '(evil-window-left :which-key "Window left")
  "wl"  '(evil-window-right :which-key "Window right")
  "wk"  '(evil-window-up :which-key "Window up")
  "wj"  '(evil-window-down :which-key "Window down")
  "w/"  '(split-window-right :which-key "Split window right")
  "w-"  '(split-window-below :which-key "Split window below")
  "w="  '(balance-windows :which-key "Balance windows")
  "wm"  '(delete-other-windows :which-key "Maximize window")
  "wd"  '(delete-window :which-key "Delete window")
  "ww"  '(other-window :which-key "Other window")
  ;;"TAB" '(mode-line-other-buffer :which-key "Switch to last buffer")
  "b"   '(:ignore t :which-key "Buffers")
  "bn"  '(next-buffer :which-key "Next buffer")
  "bp"  '(previous-buffer :which-key "Previous buffer")
  "bl"  '(list-buffers :which-key "List buffers")
  )

;; Org Mode keybindings
(my/leader-keys
  "n"   '(:ignore t :which-key "Notes")
  "nn"  '(org-capture :which-key "Org Capture")
  "na"  '(org-agenda :which-key "Org Agenda")
  "nl"  '(org-store-link :which-key "Store org link")
  "nb"  '(org-switchb :which-key "Switch Org buffer")
  )

;; Git keybindings
(my/leader-keys
  "g"   '(:ignore t :which-key "Git")
  "gs"  '(magit-status :which-key "Magit Status")
  "gg"  '(magit-status :which-key "Magit Status")
  "gb"  '(magit-branch-checkout :which-key "Checkout branch")
  "gc"  '(magit-commit :which-key "Commit changes")
  "gC"  '(magit-clone :which-key "Clone repository")
  "gp"  '(magit-push-current :which-key "Push changes")
  "gl"  '(magit-log :which-key "Show log")
  )

;; Utility keybindings
(my/leader-keys
  "t"   '(:ignore t :which-key "Toggle")
  "ts"  '(flyspell-mode :which-key "Toggle Flyspell")
  "tn"  '(display-line-numbers-mode :which-key "Toggle line numbers")
  "tp"  '(visual-line-mode :which-key "Toggle Visual Line Mode")
  "a"   '(align-regexp :which-key "Align with Regexp")
  )

;; Help and documentation
(my/leader-keys
  "h"   '(:ignore t :which-key "Help")
  "hf"  '(describe-function :which-key "Describe function")
  "hv"  '(describe-variable :which-key "Describe variable")
  "hk"  '(describe-key :which-key "Describe key")
  "hm"  '(describe-mode :which-key "Describe mode")
  "ho"  '(counsel-describe-symbol :which-key "Describe symbol")
  "hi"  '(info :which-key "Info manuals")
  )

;; Code and development tools
(my/leader-keys
  "c"   '(:ignore t :which-key "Code")
  "cc"  '(compile :which-key "Compile")
  "cr"  '(recompile :which-key "Recompile")
  "cs"  '(counsel-imenu :which-key "Search symbols")
  "cd"  '(xref-find-definitions :which-key "Find definitions")
  "cD"  '(xref-find-references :which-key "Find references")
  "ca"  '(lsp-execute-code-action :which-key "Code action")
  )

(my/leader-keys
  "TAB" '(:ignore t :which-key "Tabs")
  ;; Reassign 'switch-to-prev-buffer' if desired
  "TAB TAB" '(switch-to-prev-buffer :which-key "Previous buffer")
  "TAB n" '(tab-bar-new-tab :which-key "New Tab")
  "TAB c" '(tab-bar-close-tab :which-key "Close Tab")
  "TAB o" '(tab-bar-switch-to-tab :which-key "Switch to Tab")
  "TAB [" '(tab-bar-switch-to-prev-tab :which-key "Previous Tab")
  "TAB ]" '(tab-bar-switch-to-next-tab :which-key "Next Tab")
  "TAB r" '(tab-bar-rename-tab :which-key "Rename Tab")
  )

;; Restart Emacs
(use-package restart-emacs
  :ensure nil)

(my/leader-keys
  "qr" '(restart-emacs :which-key "Restart Emacs"))

(my/leader-keys
  ;; Project bindings
  "p" '(:ignore t :which-key "project")
  "p p" '(projectile-switch-project :which-key "Switch project")
  "p f" '(projectile-find-file :which-key "Find file in project")
  "p b" '(projectile-switch-to-buffer :which-key "Switch to buffer in project")
  "p d" '(projectile-find-dir :which-key "Find directory in project")
  "p s" '(projectile-ripgrep :which-key "Search in project")
  "p g" '(projectile-vc :which-key "Open project in VC")
  "p k" '(projectile-kill-buffers :which-key "Kill project buffers"))

(my/leader-keys
  ;; Layout/Workspace bindings
  "l" '(:ignore t :which-key "layout")
  "l l" '(persp-switch :which-key "Switch workspace")
  "l n" '(persp-next :which-key "Next workspace")
  "l p" '(persp-prev :which-key "Previous workspace")
  "l r" '(persp-rename :which-key "Rename workspace")
  "l k" '(persp-kill :which-key "Kill workspace")
  "l s" '(persp-save-state-to-file :which-key "Save workspace state")
  "l L" '(persp-load-state-from-file :which-key "Load workspace state"))

(use-package projectile
  :ensure nil
  :init
  ;; Enable Projectile globally
  (projectile-mode +1)
  :config
  ;; Set Projectile to use the native indexing method
  (setq projectile-indexing-method 'native)
  ;; Enable caching for faster indexing
  (setq projectile-enable-caching t)
  ;; Set the Projectile cache file directory
  (setq projectile-cache-file (expand-file-name "projectile.cache" user-emacs-directory))

  (setq projectile-enable-caching t)

  (setq projectile-allow-remote-projects t          ;; allow TRAMP projects
        projectile-track-known-projects-automatically t
        projectile-indexing-method 'alien           ;; faster over TRAMP
        projectile-enable-caching t)

  ;; Allow remembering of remote projects
  (with-eval-after-load 'tramp
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path)))

(use-package counsel-projectile
  :after (counsel projectile)
  :ensure nil
  :config
  (counsel-projectile-mode))

(use-package perspective
  :ensure nil
  :bind
  ;; Bind perspective commands under "C-c p"
  (("C-c p s" . persp-switch)
   ("C-c p k" . persp-kill)
   ("C-c p r" . persp-rename))
  :init
  ;; Suppress the warning about missing persp-mode-prefix-key
  (setq persp-suppress-no-prefix-key-warning t)
  (persp-mode))

(use-package perspective
  :config
  ;; Integrate Perspective with Projectile
  (use-package persp-projectile
    :ensure nil
    :bind (("C-c p p" . projectile-persp-switch-project))))

;; Force early credential loading
(add-hook 'emacs-startup-hook
          (lambda ()
            (ignore-errors
              (auth-source-search :max 1 :host "api.openai.com" :user "gptel")
              (auth-source-search :max 1 :host "api.anthropic.com" :user "gptel"))))

;; Configure gptel with multiple backends
(use-package gptel
  :ensure nil
  :config
  ;; Try with curl first (remove workaround)
  ;; (setq gptel-use-curl nil)  ; Comment this out to test

  (setq gptel-default-mode 'org-mode)

  ;; Define available backends with explicit models
  (setq gptel-backends
        (list
         ;; Claude (Anthropic) - set as default
         (gptel-make-anthropic "Claude"
           :stream t
           :key (lambda ()
                  (auth-source-pick-first-password :host "api.anthropic.com")))

         ;; OpenAI GPT with models
         (gptel-make-openai "ChatGPT"
           :stream t
           :models '(gpt-5
                     gpt-5-mini
                     gpt-5-nano)
           :key (lambda ()
                  (auth-source-pick-first-password :host "api.openai.com")))))

  ;; Set default backend
  (setq gptel-backend (car gptel-backends)))

;; Helper function to switch between backends
(defun my/gptel-switch-backend ()
  "Interactively switch between configured gptel backends."
  (interactive)
  (let* ((backend-names (mapcar (lambda (b) (gptel-backend-name b)) gptel-backends))
         (choice (completing-read "Select backend: " backend-names nil t)))
    (setq gptel-backend (seq-find (lambda (b)
                                    (string= (gptel-backend-name b) choice))
                                  gptel-backends))
    (message "Switched to %s" choice)))


  ;; ;; This is a workaround https://github.com/karthink/gptel/issues/342
  ;;   (setq gptel-use-curl nil)
  ;;   (setq gptel-default-mode 'org-mode)

  ;;   ;; Configure gptel with multiple backends
  ;;   (use-package gptel
  ;;     :ensure nil
  ;;     :config
  ;;     ;; Define available backends
  ;;     (setq gptel-backends
  ;;           (list
  ;;            ;; Claude (Anthropic) - set as default
  ;;            (gptel-make-anthropic "Claude"
  ;;              :stream t
  ;;              :key (lambda ()
  ;;                     (auth-source-pick-first-password :host "api.anthropic.com")))

  ;;            ;; OpenAI GPT
  ;;            (gptel-make-openai "ChatGPT"
  ;;              :stream t
  ;;              :key (lambda ()
  ;;                     (auth-source-pick-first-password :host "api.openai.com")))))

  ;;     ;; Set Claude as the default backend
  ;;     (setq gptel-backend (car gptel-backends))

  ;;     ;; Set default Claude model
  ;;     (setq gptel-model 'claude-sonnet-4-5-20250929))

  ;;   ;; Helper function to switch between backends
  ;;   (defun my/gptel-switch-backend ()
  ;;     "Interactively switch between configured gptel backends."
  ;;     (interactive)
  ;;     (let* ((backend-names (mapcar (lambda (b) (gptel-backend-name b)) gptel-backends))
  ;;            (choice (completing-read "Select backend: " backend-names nil t)))
  ;;       (setq gptel-backend (seq-find (lambda (b)
  ;;                                       (string= (gptel-backend-name b) choice))
  ;;                                     gptel-backends))
  ;;       (message "Switched to %s" choice)))

    ;; Keybindings
    (my/leader-keys
      "o"    '(:ignore t :which-key "AI models")
      "og"   '(gptel :which-key "Invoke gptel")
      "oa"   '(gptel-abort :which-key "Abort gptel invocation")
      "om"   '(gptel-menu :which-key "gptel-menu")
      "ob"   '(my/gptel-switch-backend :which-key "Switch AI backend")
      "oc"   '(:ignore t :which-key "AI models context manipulation")
      "ocb"  '(gptel-add :which-key "Add/Remove buffer to AI context")
      "ocf"  '(gptel-context-add-file :which-key "Add file to AI context")
      "ocr"  '(gptel-context-remove-all :which-key "Remove all AI context")
      "or"   '(gptel-rewrite :which-key "AI model rewrite"))

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

(use-package mu4e
  :ensure nil  ;; mu4e is installed externally; not via package.el
  :load-path "/usr/share/emacs/site-lisp/mu4e"  ;; Adjust this path if necessary
  :commands (mu4e)  ;; Ensure it's available for 'after' directives
  :defer t
  :init
  ;; General mu4e settings that need to be set before mu4e loads
  (setq mu4e-maildir "~/.local/share/mail"
        mu4e-attachment-dir "~/Downloads"
        mu4e-get-mail-command  "guix shell -L ~/dotfiles cyrus-sasl-xoauth2 -- mbsync -a"
        mu4e-update-interval 300
        mu4e-index-cleanup t
        mu4e-index-update-error-warning t
        mu4e-hide-index-messages t
        mu4e-index-update-in-background t
        mu4e-change-filenames-when-moving t
        mu4e-index-lazy-check nil
        mu4e-confirm-quit nil
        mu4e-split-view 'single-window
        mu4e-headers-auto-update nil
        mu4e-headers-date-format "%d-%m"
        mu4e-headers-time-format "%H:%M"
        mu4e-headers-from-or-to-prefix '("" . "To ")
        mu4e-headers-include-related t
        mu4e-headers-skip-duplicates t
        sendmail-program "msmtp"
        send-mail-function 'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function 'message-send-mail-with-sendmail)
  :config
  ;; Unbind conflicting keys if necessary
  (define-key mu4e-headers-mode-map (kbd "C--") nil)
  (define-key mu4e-view-mode-map (kbd "C--") nil)
  (define-key mu4e-headers-mode-map (kbd "C-c c") 'mu4e-org-store-and-capture)
  (define-key mu4e-view-mode-map    (kbd "C-c c") 'mu4e-org-store-and-capture)

  ;; Custom keybindings with Evil
  (with-eval-after-load 'evil
    ;; Ensure 'a' is available in visual state in mu4e-view-mode
    (evil-define-key 'visual mu4e-view-mode-map (kbd "a") 'mu4e-view-action)
    ;; Similarly, for mu4e-headers-mode if needed
    (evil-define-key 'visual mu4e-headers-mode-map (kbd "a") 'mu4e-headers-mark-for-*))

  ;; Reset variables, as our configuration is based on contexts
  (setq mu4e-contexts nil
        mu4e-drafts-folder nil
        mu4e-compose-reply-to-address nil
        mu4e-compose-signature t
        mu4e-compose-signature-auto-include t
        mu4e-sent-folder nil
        mu4e-trash-folder nil)

  ;; Set mu4e signature
  (setq mu4e-compose-signature "Prof. Rafael Palomar, Ph.D.
__________________________________
Head of Medical Software Research Laboratory (MESH|Lab)
The Intervention Centre, Oslo University Hospital (OUH)
Sognsvannsveien 20 (Rikshospitalet Building D-6.3002)
N-0372 Oslo, Norway
rafael.palomar@ous-research.no
https://ivs.no

Associate Professor
Norwegian University of Science and Technology (NTNU)
Teknologiveien 22, 2815 Gjøvik, Norway
rafael.palomar@ntnu.no
https://ntnu.no
--")

  ;; Define mu4e contexts
  (setq mu4e-contexts
        (list
         ;; NTNU Account
         (make-mu4e-context
          :name "NTNU"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/rafael.palomar@ntnu.no" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address      . "rafael.palomar@ntnu.no")
                  (user-full-name         . "Rafael Palomar")
                  (mu4e-drafts-folder     . "/rafael.palomar@ntnu.no/Drafts")
                  (mu4e-sent-folder       . "/rafael.palomar@ntnu.no/Sent")
                  (mu4e-trash-folder      . "/rafael.palomar@ntnu.no/Trash")
                  (mu4e-refile-folder     . "/rafael.palomar@ntnu.no/Archive")
                  ;; Configure SMTP
                  (smtpmail-smtp-user     . "rafael.palomar@ntnu.no")
                  (smtpmail-smtp-server   . "smtp.office365.com")
                  (smtpmail-smtp-service  . 587)
                  (smtpmail-stream-type   . starttls)))
         ;; OUS-Research Account
         (make-mu4e-context
          :name "OUS-Research"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/rafael.palomar@ous-research.no" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address      . "rafael.palomar@ous-research.no")
                  (user-full-name         . "Rafael Palomar")
                  (mu4e-drafts-folder     . "/rafaelpa@ous-research.no/Drafts")
                  (mu4e-sent-folder       . "/rafaelpa@ous-research.no/Sent")
                  (mu4e-trash-folder      . "/rafaelpa@ous-research.no/Trash")
                  (mu4e-refile-folder     . "/rafaelpa@ous-research.no/Archive")
                  ;; Configure SMTP
                  (smtpmail-smtp-user     . "rafaelpa@uio.no")
                  (smtpmail-smtp-server   . "smtp.office365.com")
                  (smtpmail-smtp-service  . 587)
                  (smtpmail-stream-type   . starttls))))))


(use-package mu4e-dashboard
  :ensure nil  ;; Adjust accordingly if you install it via package.el
  :after mu4e
  :config
  (require 'svg-lib)  ;; Ensure svg-lib is loaded
  (setq mu4e-dashboard-propagate-keymap nil)

  (defun mu4e-dashboard ()
    "Open the mu4e dashboard on the left side."
    (interactive)
    (with-selected-window
        (split-window (selected-window) -34 'left)
      (find-file (expand-file-name "mu4e-dashboard.org" user-emacs-directory))
      (mu4e-dashboard-mode)
      (hl-line-mode)
      (set-window-dedicated-p nil t)
      (defvar svg-font-lock-keywords
        `(("\\!\\([\\ 0-9]+\\)\\!"
           (0 (list 'face nil 'display (svg-font-lock-tag (match-string 1)))))))
      (defun svg-font-lock-tag (label)
        (svg-lib-tag label nil
                     :stroke 0 :margin 1 :font-weight 'bold
                     :padding (max 0 (- 3 (length label)))
                     :foreground (face-foreground 'nano-popout-i)
                     :background (face-background 'nano-popout-i)))
      (push 'display font-lock-extra-managed-props)
      (font-lock-add-keywords nil svg-font-lock-keywords)
      (font-lock-flush (point-min) (point-max)))))


(my/leader-keys
  "m"    '(:ignore t :which-key "Mail")
  "mm"   '(mu4e :which-key "Open mu4e")
  "mq"   '(mu4e-quit :which-key "Quit mu4e")

  "mc"  '(:ignore t :which-key "Compose")
  "mcc" '(mu4e-compose-new :which-key "Compose new email (plain text)")
  "mcC" '(my-mu4e-compose-new-with-org-mode :which-key "Compose new email with Org-mode")
  "mr"  '(:ignore t :which-key "Reply")
  "mrr" '(mu4e-compose-reply :which-key "Reply (plain text)")
  "mrR" '(my-mu4e-compose-reply-with-org-mode :which-key "Reply with Org-mode"))

;;; --------- tramp -------

(with-eval-after-load 'tramp
  (require 'tramp-container)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; Use /bin/bash (or /bin/sh) on every remote host
  (add-to-list 'tramp-connection-properties
               '(".*" "remote-shell" "/bin/sh"))
  ;; every docker hop  → /bin/sh
  (add-to-list 'tramp-connection-properties
               '("\\`/docker:" "remote-shell" "/bin/sh"))
  ;; If you also want to force a login shell:
  (add-to-list 'tramp-connection-properties
               '(".*" "remote-shell-login" ("-l")))
  ;; (optional) keep TRAMP’s default remote path plus the usual ones
  (add-to-list 'tramp-connection-properties
               '(".*" "remote-path"
                 ("/usr/local/sbin" "/usr/local/bin" "/usr/bin" "/bin"
                  tramp-own-remote-path))))

(use-package denote
    :ensure nil
    :config
    ;; Define silos for Work and Personal notes
    (setq denote-directory "~/Notes/Work/")

    (setq denote-silo-extras-directories
          '(("personal" . "~/Notes/Personal/")))

    (defvar my-denote-to-agenda-regexp "_agenda"
      "Denote file names that are added to the agenda.
        See `my-add-denote-to-agenda'.")

    (defun my-denote-add-to-agenda ()
      "Add current file to the `org-agenda-files', if needed.
        The file's name must match the `my-denote-to-agenda-regexp'.

        Add this to the `after-save-hook' or call it interactively."
      (interactive)
      (when-let* ((file (buffer-file-name))
                  ((denote-file-is-note-p file))
                  ((string-match-p my-denote-to-agenda-regexp (buffer-file-name))))
        (add-to-list 'org-agenda-files file)))

    ;; Example to add the file automatically. Comment/Uncomment it:
    (add-hook 'after-save-hook #'my-denote-add-to-agenda)

    (defun my-denote-remove-from-agenda ()
      "Remove current file from the `org-agenda-files'.
        See `my-denote-add-to-agenda' for how to add files to the Org
        agenda."
      (interactive)
      (when-let* ((file (buffer-file-name))
                  ((string-match-p my-denote-to-agenda-regexp (buffer-file-name))))
        (setq org-agenda-files (delete file org-agenda-files)))))

(use-package denote-silo
  :ensure nil
  ;; Bind these commands to key bindings of your choice.
  :commands ( denote-silo-create-note
        denote-silo-open-or-create
        denote-silo-select-silo-then-command
        denote-silo-dired
        denote-silo-cd )
  :config
  ;; Add your silos to this list.  By default, it only includes the
  ;; value of the variable `denote-directory'.
  (setq denote-silo-directories
  (list denote-directory
        "~/Notes/Personal/")))

(use-package dashboard
  :ensure nil
  :config
  (dashboard-setup-startup-hook))

(use-package avy
  :ensure nil
  :config
  ;; Set the style for how avy displays candidates
  (setq avy-style 'at-full
        avy-all-windows t
        avy-background t))

(use-package beacon
  :ensure nil
  :config
  (beacon-mode 1))

(use-package tabspaces
  :ensure nil
  :after (persp-mode)
  :hook (after-init . tabspaces-mode)
  :init
  (setq tabspaces-use-filtered-buffers-as-default t
        tabspaces-default-tab "Main"
        tabspaces-remove-to-default t
        tabspaces-include-buffers '("*scratch*")
        tabspaces-session t)
  :config
  ;; Automatically create workspaces when switching projects
  (defun my/project-switch ()
    "Switch project and create a new tab/workspace."
    (interactive)
    (let ((project (project-prompt-project-dir)))
      (tabspaces-switch-or-create-workspace (car (last (split-string project "/" t))))
      (project-switch-project project))))

;; Geiser + Guile
(use-package geiser
  :ensure nil
  :init
  (setq geiser-active-implementations '(guile))
  :hook
  ((scheme-mode . geiser-mode))
  :config
  (setq geiser-mode-auto-p t))

(use-package geiser-guile
  :ensure nil
  :config
  ;; You already have these, just keep them consolidated here:
  (add-to-list 'geiser-guile-load-path "~/src/guix")
  (add-to-list 'geiser-guile-load-path "~/src/nonguix")
  (add-to-list 'geiser-guile-load-path "~/src/guix-systole/systole")
  (add-to-list 'geiser-guile-load-path "~/src/guix-systole/system")
  ;; Optional: pass extra -L paths to REPL on start (redundant with load-path above)
  ;; (setq geiser-guile-extra-load-path geiser-guile-load-path)
  )

;; Evil-friendly navigation in Scheme
;; (with-eval-after-load 'evil
;;   (evil-define-key 'normal scheme-mode-map (kbd "gd") #'xref-find-definitions)
;;   (evil-define-key 'normal scheme-mode-map (kbd "gD") #'xref-find-references))

;; Structural editing and readability
(use-package paredit
  :ensure nil
  :hook (scheme-mode . paredit-mode))

(use-package rainbow-delimiters
  :ensure nil
  :hook (scheme-mode . rainbow-delimiters-mode))

(use-package forge
:after magit
:ensure nil                       ;; package provided by Guix
:config
;; Where the credentials live (same as you do for GPTel etc.)
(setq auth-sources '("~/.authinfo.gpg"))

;; Register the forges you use (GitHub + GitLab examples)
(setq forge-alist
      '(("github.com" "api.github.com" "github.com" forge-github-repository)
        ("gitlab.com" "gitlab.com/api/v4" "gitlab.com" forge-gitlab-repository)))

;; Optional: fetch issues automatically when you `M-x magit-status`
(setq forge-add-default-bindings t         ; C-c C-f prefix
      forge-database-file (expand-file-name "forge-db.sqlite" user-emacs-directory))
)

(require 'pyvenv)
(pyvenv-mode 1)  ;; Enable pyvenv mode

;;; c-style-vtk.el --- Visualization ToolKit (VTK) Emacs C Style.
;;; Extracted from https://raw.githubusercontent.com/MartinNowak/elisp/6466ef96d228b496c2db8ca898ffe316caf5e765/mine/c-style-vtk.el

;; --- Register the VTK indentation style -----------------------------
(with-eval-after-load 'cc-mode
  (c-add-style
   "vtk"
   '("stroustrup"                       ; inherit, then override
     (c-basic-offset          . 2)
     (indent-tabs-mode        . nil)
     (c-comment-only-line-offset . 0)
     (c-electric-pound-behavior . (alignleft))

     (c-offsets-alist
      ;; the list below is copied from `c-set-indent-vtk`
      (case-label             . 0)
      (label                  . 0)
      (topmost-intro-cont     . 0)

      (block-open             . +)
      (block-close            . 0)

      (substatement           . +)
      (substatement-open      . +)

      (statement-block-intro  . 0)
      (access-label           . -)

      (string                 . c-lineup-dont-change)
      (c                     . c-lineup-C-comments)

      (defun-block-intro      . +)
      (member-init-intro      . +)
      (member-init-cont       . 0)

      (func-decl-cont         . +)

      (brace-list-intro       . +)
      (brace-list-entry       . 0)

      (statement-cont         . +)
      (statement-case-intro   . +)
      (statement-case-open    . +)

      (arglist-intro          . c-lineup-arglist-intro-after-paren)
      (arglist-cont          . c-lineup-arglist)
      (arglist-cont-nonempty . c-lineup-arglist)
      (arglist-close          . c-lineup-arglist)

      (stream-op              . c-lineup-streamop)
      (inclass                . +)

      (cpp-macro              . -1000)
      (cpp-macro-cont         . c-lineup-dont-change)

      (objc-method-intro      . -1000)
      (objc-method-args-cont  . c-lineup-ObjC-method-args)
      (objc-method-call-cont  . c-lineup-ObjC-method-call)

      (extern-lang-open       . 0)
      (extern-lang-close      . 0)
      (inextern-lang          . +)

      (namespace-open         . 0)
      (namespace-close        . 0)
      (innamespace            . +)

      (template-args-cont     . +)
      (inlambda               . c-lineup-inexpr-block)
      (lambda-intro-cont      . +)
      (inexpr-statement       . 0)
      (inexpr-class           . +)))))
