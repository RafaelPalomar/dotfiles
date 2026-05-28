;; -*- lexical-binding: t; -*-

;;; This file is generated from the =emacs.org= file in my dotfiles repository!
;;; ----- Basic Configuration -----

;; Performance optimizations
(setq read-process-output-max (* 1024 1024)  ;; 1MB
      process-adaptive-read-buffering nil)

;; Increase the garbage collection threshold during startup for faster startup
(setq gc-cons-threshold most-positive-fixnum)

;; Reset garbage collection thresholds after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))  ;; 16MB
            (setq gc-cons-percentage 0.1)))

;; Encoding and locale
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Network settings
(setq network-enable-ipv6 nil
      starttls-use-gnutls t
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
                 "%b"))))                     ;; Show full path in frame title

;; Load the custom file if it exists
(when (file-exists-p custom-file)
  (load custom-file t))

;; Set tabs to spaces and define tab width
(setq-default indent-tabs-mode nil           ;; Use spaces instead of tabs
              tab-width 2)                   ;; Set default tab width to 2

;; Simplify the interface
(menu-bar-mode -1)                           ;; Disable the menu bar
(tool-bar-mode -1)                           ;; Disable the tool bar
(scroll-bar-mode -1)                         ;; Disable the scroll bar

;; Add local bin to PATH
(setenv "PATH" (concat (getenv "HOME") "/.local/bin:" (getenv "PATH")))
(add-to-list 'exec-path (expand-file-name "~/.local/bin"))

;; Fix locale settings (if needed)
(when (and (not (getenv "LC_ALL"))
           (or (not (getenv "LANG"))
               (string= (getenv "LANG") "")))
  (setenv "LANG" "en_US.UTF-8"))

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

;; Set default directory to home
(setq default-directory "~/")

;; Delete trailing whitespace before saving buffers
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package doom-themes
  :ensure nil
  :config
  ;; Add our custom theme directory to the load path
  (add-to-list 'custom-theme-load-path
               (expand-file-name "themes" user-emacs-directory))

  ;; Load entelequia-graphite (built on doom-themes infrastructure)
  (load-theme 'entelequia-graphite t)

  ;; Enable doom-themes features
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification
  (doom-themes-org-config))

(require 'ansi-color)
(defun my/compilation-ansi-colorize ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))
(add-hook 'compilation-filter-hook #'my/compilation-ansi-colorize)

;; Font configurations - universal for GUI and terminal
(set-face-attribute 'default nil
                    :font "JetBrains Mono"
                    :height 110
                    :weight 'normal)

(set-face-attribute 'fixed-pitch nil
                    :font "Fira Code Retina"
                    :weight 'normal)

(set-face-attribute 'variable-pitch nil
                    :font "Cantarell"
                    :weight 'normal)

;; Ensure line numbers scale with text
(set-face-attribute 'line-number nil
                    :inherit 'default)
(set-face-attribute 'line-number-current-line nil
                    :inherit 'line-number
                    :weight 'bold)

;; Fix Org Agenda scaling
(with-eval-after-load 'org-agenda
  (set-face-attribute 'org-agenda-structure nil
                      :inherit 'default
                      :height 1.0))

;; Fix Dashboard scaling
(with-eval-after-load 'dashboard
  (set-face-attribute 'dashboard-heading nil
                      :inherit 'default
                      :weight 'bold
                      :height 1.0)
  (set-face-attribute 'dashboard-items-face nil
                      :inherit 'default))

;; Frame transparency (GUI only feature, but won't error in terminal)
(set-frame-parameter (selected-frame) 'alpha 95)
(add-to-list 'default-frame-alist '(alpha . 95))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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

;; Load all-the-icons without installing via package.el
(use-package all-the-icons
  :ensure nil)

;; Enable Evil Mode
(use-package evil
  :ensure nil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-d-scroll t
        evil-want-C-i-jump nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-fu
        evil-search-module 'evil-search
        evil-want-fine-undo t
        evil-kill-on-visual-paste nil)
  :config
  ;; Enable Evil Mode
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure nil
  :config
  ;; Exclude dired from evil-collection (dirvish provides custom keybindings)
  (setq evil-collection-mode-list (delq 'dired evil-collection-mode-list))
  ;; Initialize Evil Collection for all supported modes (except dired)
  (evil-collection-init))

(use-package evil-escape
  :ensure nil
  :after evil
  :config
  (evil-escape-mode 1)
  (setq-default evil-escape-key-sequence "jk"
                evil-escape-delay 0.15))

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

;; Customize Cursor Appearance
(setq evil-normal-state-cursor 'box      ;; Normal mode cursor is a box
      evil-insert-state-cursor 'bar      ;; Insert mode cursor is a bar
      evil-visual-state-cursor 'hollow)  ;; Visual mode cursor is hollow

;; Org Mode base configuration (Doom defaults)
(use-package org
  :ensure nil
  :config
  (setq org-startup-indented t
        org-startup-folded 'content
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t
        org-log-done 'time
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis "⤵")
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(p)" "FOLLOW-UP(f)" "|" "DONE(d)" "CANCELLED(c)"))))

;; Enhance Org Mode appearance with org-modern
(use-package org-modern
  :after (org)
  :ensure nil
  :hook
  (org-mode . org-modern-mode)
  :config
  ;; Enable org-modern globally for all Org buffers
  (global-org-modern-mode))

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

(setq org-babel-latex-pdf-svg-process "pdftocairo -svg %f %o")

(require 'tex-site nil t)
(with-eval-after-load 'tex
  (setq-default TeX-engine 'xetex)
  (add-to-list 'TeX-command-list
               '("XeLaTeX" "xelatex -synctex=1 -interaction=nonstopmode %s"
                 TeX-run-TeX nil (latex-mode) :help "Run XeLaTeX")
               t))

(setq org-capture-templates
      '(;; Mail workflow.  me/mf use the denote pattern; the
        ;; mu4e-link body (with %:subject %:fromname etc. specifiers)
        ;; is supplied via denote-org-capture-specifiers.  ma/mw append
        ;; a list item under the chosen project's * Log heading and
        ;; therefore use a buffer-positioning target function.
        ("m" "Email Workflow")
        ("me" "Email Task → fleeting" plain
         (file denote-last-path)
         (lambda ()
           (let* ((kw (delq nil (list "agenda" (my-mu4e--context-tag))))
                  (subj (or (plist-get org-store-link-plist :description)
                            (plist-get org-store-link-plist :subject)
                            "(no subject)"))
                  (denote-directory (expand-file-name "~/pks/fleeting/"))
                  (denote-use-keywords kw)
                  (denote-use-title subj)
                  (denote-org-capture-specifiers "* TODO %:subject
:PROPERTIES:
:FROM:    %:fromname <%:fromaddress>
:DATE:    %:date
:MSG-ID:  %:message-id
:END:

[[%:link][View Email]]

%?
"))
             (denote-org-capture)))
         :no-save t :jump-to-captured t :kill-buffer t)
        ("mf" "Follow Up → fleeting" plain
         (file denote-last-path)
         (lambda ()
           (let* ((kw (delq nil (list "agenda" (my-mu4e--context-tag))))
                  (subj (or (plist-get org-store-link-plist :description)
                            (plist-get org-store-link-plist :subject)
                            "(no subject)"))
                  (denote-directory (expand-file-name "~/pks/fleeting/"))
                  (denote-use-keywords kw)
                  (denote-use-title (concat "Follow up: " subj))
                  (denote-org-capture-specifiers "* TODO Follow up with %:fromname on [[%:link][%:subject]]
SCHEDULED: %t
DEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))
:PROPERTIES:
:FROM:    %:fromname <%:fromaddress>
:DATE:    %:date
:MSG-ID:  %:message-id
:END:

%?
"))
             (denote-org-capture)))
         :no-save t :jump-to-captured t :kill-buffer t)
        ("ma" "Action Item → project Log" item
         (function my-mu4e-capture-target-project-log)
         "- %<%Y-%m-%d> :: %? — from %:fromname re %:subject [[%:link][thread]]"
         :immediate-finish nil)
        ("mw" "Waiting For → project Log" item
         (function my-mu4e-capture-target-project-log)
         "- %<%Y-%m-%d> :: WAITING %? — from %:fromname re %:subject [[%:link][thread]]"
         :immediate-finish nil)))

;; Agenda files.  With the function-based PKS, projects/ notes tagged
;; _agenda are the source of truth for active project TODOs.
(defun my-pks-projects-files ()
  "All org files under ~/pks/projects/, as a refile-targets function."
  (when (file-directory-p (expand-file-name "~/pks/projects/"))
    (directory-files-recursively
     (expand-file-name "~/pks/projects/") "\\.org\\'")))

(setq org-agenda-files
      (delete-dups
       (append
        '("~/org/archive.org")
        ;; Denote notes tagged _agenda across all PKS silos.
        (when (file-directory-p (expand-file-name "~/pks/"))
          (directory-files-recursively
           (expand-file-name "~/pks/") "_agenda.*\\.org\\'")))))

(setq-default org-refile-targets
              '((nil :maxlevel . 9)
                (org-agenda-files :maxlevel . 9)
                (my-pks-projects-files :maxlevel . 5)))
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path 'file)

;; Helper function to make current frame floating in bspwm
(defun my/make-frame-floating ()
  "Make the current frame floating in bspwm."
  (when (and (display-graphic-p)
             (executable-find "bspc"))
    (call-process "bspc" nil 0 nil "node" "-t" "floating")))

;; Helper function to setup special frames (capture, dired, agenda)
(defun my/setup-special-frame ()
  "Setup the current frame as a special floating frame.
Deletes other windows and makes the frame floating."
  (delete-other-windows)
  (my/make-frame-floating))

;; Function to check if current frame is a special frame based on name
(defun my/is-special-frame-p ()
  "Check if the current frame is a special frame (capture, dired, agenda, denote)."
  (let ((frame-name (frame-parameter nil 'name)))
    (and frame-name
         (or (string-match-p "org-capture" frame-name)
             (string-match-p "dired-manager" frame-name)
             (string-match-p "org-agenda" frame-name)
             (string-match-p "denote" frame-name)))))

;; Hook for org-capture to setup special frame
(add-hook 'org-capture-mode-hook
          (lambda ()
            (when (my/is-special-frame-p)
              (my/setup-special-frame))))

;; Hook for dired to setup special frame
(add-hook 'dired-mode-hook
          (lambda ()
            (when (and (my/is-special-frame-p)
                       (= (length (window-list)) 1))
              (my/make-frame-floating))))

;; Hook for org-agenda to setup special frame
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (when (my/is-special-frame-p)
              (my/setup-special-frame))))

;; Hook for denote to setup special frame
(add-hook 'denote-create-note-hook
          (lambda ()
            (when (my/is-special-frame-p)
              (my/setup-special-frame))))

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

(setq ob-mermaid-cli-path (expand-file-name "~/.guix-home/profile/bin/mmdc"))

(with-eval-after-load 'ob-mermaid
  (setq org-babel-default-header-args:mermaid
        '((:results . "file replace")
          (:exports . "results")
          (:background-color . "transparent"))))

(use-package org-protocol
  :ensure nil
  :config
  (setq org-protocol-default-template-key "q"))

;; Org Clock Configuration
(setq org-clock-persist t
      org-clock-persist-file "~/.config/emacs/org-clock-save.el"
      org-clock-in-resume t
      org-clock-into-drawer t
      org-clock-out-remove-zero-time-clocks t
      org-clock-persist-query-resume nil
      org-clock-report-include-clocking-task t
      org-clock-idle-time 15)

(unless (daemonp)
  (org-clock-persistence-insinuate))

;; Clock notification on start/stop
(defun my/org-clock-notify (state)
  "Send notification when clocking STATE changes."
  (let ((task (org-get-heading t t t t)))
    (pcase state
      ('in (start-process "notify" nil "notify-send" "Org Clock"
                          (format "Started: %s" task) "-i" "clock"))
      ('out (start-process "notify" nil "notify-send" "Org Clock"
                           (format "Stopped: %s" task) "-i" "clock")))))

(add-hook 'org-clock-in-hook (lambda () (my/org-clock-notify 'in)))
(add-hook 'org-clock-out-hook (lambda () (my/org-clock-notify 'out)))

;; Helper functions
(defun my/org-clock-in-last ()
  "Resume last clocked task."
  (interactive)
  (org-clock-in-last))

(defun my/org-clock-goto-current ()
  "Jump to currently clocked task."
  (interactive)
  (org-clock-goto))

(defun my/org-clock-report-today ()
  "Show clock report for today."
  (interactive)
  (org-clock-report))

;; Org Super Agenda
(use-package org-super-agenda
  :ensure nil
  :after org-agenda
  :config
  (unless (daemonp)
    (org-super-agenda-mode 1))
  (setq org-super-agenda-groups
        '((:name "Today"
           :time-grid t
           :scheduled today
           :deadline today)
          (:name "Overdue"
           :deadline past
           :scheduled past)
          (:name "Due Soon"
           :deadline future
           :scheduled future)
          (:name "In Progress"
           :todo "DOING")
          (:name "Waiting"
           :todo "WAITING")
          (:name "GitHub Issues"
           :tag "github")
          (:name "AI Tasks"
           :and (:tag "ai" :not (:todo "DONE")))
          (:name "Active Projects"
           :and (:todo "PROJECT" :tag "active"))
          (:name "Personal"
           :tag "personal")
          (:name "Work"
           :tag "work")
          (:name "Research"
           :tag "research")
          (:auto-category t))))

;; Org Columns for Kanban-like view
(setq org-columns-default-format "%50ITEM(Task) %TODO %3PRIORITY %10TAGS")

(defun my/project-kanban ()
  "Show column view for current project (kanban-like)."
  (interactive)
  (org-columns))

(defun my/project-kanban-quit ()
  "Quit column view."
  (interactive)
  (org-columns-quit))

(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda "" ((org-agenda-span 'day)))
          (tags-todo "+PRIORITY=\"A\""
                     ((org-agenda-overriding-header "High Priority")))
          (todo "DOING"
                ((org-agenda-overriding-header "In Progress")))
          (todo "PROJECT"
                ((org-agenda-overriding-header "Active Projects")))))))

;; Enable Ivy for enhanced completion
(use-package ivy
  :ensure nil
  :demand t
  :diminish
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers nil
        ivy-count-format "(%d/%d) "
        ivy-wrap t
        ivy-height 15
        ivy-fixed-height-minibuffer t
        ivy-ignore-buffers nil)
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
  (global-set-key (kbd "C-c k") 'counsel-rg)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c L") 'counsel-load-library))

;; Enable Swiper for improved in-buffer searching
(use-package swiper
  :ensure nil
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

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
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")
  ;; Doom/Spacemacs-style local leader: `SPC m' for major-mode commands.
  ;; Bindings are mode-scoped via `:keymaps' at the call site, e.g.
  ;;   (my/local-leader-keys :keymaps 'org-mode-map "a" 'org-agenda)
  (general-create-definer my/local-leader-keys
    :states '(normal visual emacs motion)
    :prefix "SPC m"
    :non-normal-prefix "M-SPC m")
  ;; Additional keybindings
  (general-define-key
   :states '(normal insert visual emacs)
   :keymaps 'override
   "C-+" 'text-scale-increase
   "C-=" 'text-scale-increase
   "C--" 'text-scale-decrease
   "C-0" '(text-scale-set :which-key "Reset text scale")))

;; Basic keybindings.  Single-chord shortcuts mirror Doom's defaults:
;;   SPC SPC  M-x         SPC :    M-x (chord-friendly)
;;   SPC .    find-file   SPC ;    eval-expression
;;   SPC ,    switch buffer (workspace)
;;   SPC <    switch buffer (all)
;;   SPC '    resume last ivy
;;   SPC X    org-capture
;;   SPC RET  bookmark jump
(my/leader-keys
  "SPC" '(counsel-M-x :which-key "M-x")
  ":"   '(counsel-M-x :which-key "M-x")
  ";"   '(eval-expression :which-key "Eval expression")
  "."   '(counsel-find-file :which-key "Find file")
  ","   '(persp-ivy-switch-buffer :which-key "Switch buffer (workspace)")
  "<"   '(ivy-switch-buffer :which-key "Switch buffer (all)")
  "'"   '(ivy-resume :which-key "Resume last search")
  "X"   '(org-capture :which-key "Org capture")
  "RET" '(counsel-bookmark :which-key "Bookmark jump")
  "f"   '(:ignore t :which-key "Files")
  "ff"  '(counsel-find-file :which-key "Find file")
  "fs"  '(save-buffer :which-key "Save buffer")
  "fr"  '(counsel-recentf :which-key "Recent files")
  "fS"  '(write-file :which-key "Save file as...")
  "w"   '(:ignore t :which-key "Windows")
  "wd"  '(delete-window :which-key "Delete window")
  "wo"  '(delete-other-windows :which-key "Delete other windows")
  "ws"  '(split-window-below :which-key "Split window below")
  "wv"  '(split-window-right :which-key "Split window right")
  "u"   '(universal-argument :which-key "Universal argument")
  "m"   '(:ignore t :which-key "Major mode")
  "q"   '(:ignore t :which-key "Quit/Restart")
  "qq"  '(save-buffers-kill-terminal :which-key "Quit Emacs"))

;; Text manipulation (Doom-style)
(my/leader-keys
  "x"   '(:ignore t :which-key "text")
  "xa"  '(align-regexp :which-key "Align")
  "xl"  '(downcase-region :which-key "Downcase")
  "xu"  '(upcase-region :which-key "Upcase")
  "xc"  '(capitalize-region :which-key "Capitalize")
  "xw"  '(delete-trailing-whitespace :which-key "Delete trailing whitespace")
  "xs"  '(sort-lines :which-key "Sort lines"))

;; Toggle bindings (more complete)
(my/leader-keys
  "t"   '(:ignore t :which-key "toggle")
  "tl"  '(display-line-numbers-mode :which-key "Line numbers")
  "tf"  '(toggle-frame-fullscreen :which-key "Fullscreen")
  "tw"  '(whitespace-mode :which-key "Whitespace")
  "tt"  '(toggle-truncate-lines :which-key "Truncate lines")
  "tv"  '(visual-line-mode :which-key "Visual line mode")
  "ts"  '(flyspell-mode :which-key "Spell check"))

;; Insert bindings
(my/leader-keys
  "i"   '(:ignore t :which-key "insert")
  "iy"  '(counsel-yank-pop :which-key "From kill ring")
  "is"  '(yas-insert-snippet :which-key "Snippet")
  "iu"  '(insert-char :which-key "Unicode character"))

;; Search and completion
(my/leader-keys
  "/"    '(swiper :which-key "Swiper search")
  "s"    '(:ignore t :which-key "Search")
  "sa"   '(swiper-all :which-key "Swiper all buffers")
  "sb"   '(swiper :which-key "Search buffer")
  "sd"   '(counsel-rg :which-key "Ripgrep search")
  "sp"   '(counsel-projectile-rg :which-key "Search project with rg")
  "sg"   '(counsel-git-grep :which-key "Search in Git repo")
  "sr"   '(ivy-resume :which-key "Resume last search"))

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
  "pk"  '(my/project-kanban :which-key "Project Kanban"))

;; PKS digests
;; PKS daily/weekly review live on the `SPC n' prefix map (mirror of
;; `C-c n'); bindings added alongside the rest of the PKS map in the
;; denote section below.

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
  "b"   '(:ignore t :which-key "Buffers")
  "bn"  '(next-buffer :which-key "Next buffer")
  "bp"  '(previous-buffer :which-key "Previous buffer")
  "bl"  '(list-buffers :which-key "List buffers"))

;; Org Mode keybindings (under `SPC o'; `SPC n' is reserved for PKS).
;; Clock subcommands use `ok' to keep `oc' for capture without overload.
;; `SPC o o' duplicates the most-used Org primary (agenda) per the
;; Doom doubling convention.
(my/leader-keys
  "o"   '(:ignore t :which-key "Org")
  "oo"  '(org-agenda :which-key "Agenda")
  "oc"  '(org-capture :which-key "Capture")
  "oa"  '(org-agenda :which-key "Agenda")
  "ol"  '(org-store-link :which-key "Store link")
  "ob"  '(org-switchb :which-key "Switch Org buffer")
  "ok"  '(:ignore t :which-key "Clock")
  "oki" '(org-clock-in :which-key "Clock in")
  "oko" '(org-clock-out :which-key "Clock out")
  "okr" '(my/org-clock-in-last :which-key "Resume last clock")
  "okg" '(my/org-clock-goto-current :which-key "Go to current clock")
  "okR" '(my/org-clock-report-today :which-key "Clock report today"))

;; Git keybindings
(my/leader-keys
  "g"   '(:ignore t :which-key "Git")
  "gs"  '(magit-status :which-key "Magit Status")
  "gg"  '(magit-status :which-key "Magit Status")
  "gb"  '(magit-branch-checkout :which-key "Checkout branch")
  "gc"  '(:ignore t :which-key "Commit")
  "gcc" '(magit-commit-create :which-key "Commit")
  "gca" '(my/ai-commit-message :which-key "AI commit msg")
  "gC"  '(magit-clone :which-key "Clone repository")
  "gp"  '(magit-push-current :which-key "Push changes")
  "gl"  '(magit-log :which-key "Show log"))

;; Utility keybindings
(my/leader-keys
  "t"   '(:ignore t :which-key "Toggle")
  "ts"  '(flyspell-mode :which-key "Toggle Flyspell")
  "tn"  '(display-line-numbers-mode :which-key "Toggle line numbers")
  "tp"  '(visual-line-mode :which-key "Toggle Visual Line Mode"))

;; Help and documentation
(my/leader-keys
  "h"   '(:ignore t :which-key "Help")
  "hf"  '(describe-function :which-key "Describe function")
  "hv"  '(describe-variable :which-key "Describe variable")
  "hk"  '(describe-key :which-key "Describe key")
  "hm"  '(describe-mode :which-key "Describe mode")
  "ho"  '(counsel-describe-symbol :which-key "Describe symbol")
  "hi"  '(info :which-key "Info manuals"))

;; Code and development tools
(my/leader-keys
  "c"   '(:ignore t :which-key "Code")
  "cc"  '(compile :which-key "Compile")
  "cr"  '(recompile :which-key "Recompile")
  "cs"  '(counsel-imenu :which-key "Search symbols")
  "cd"  '(xref-find-definitions :which-key "Find definitions")
  "cD"  '(xref-find-references :which-key "Find references")
  "ca"  '(lsp-execute-code-action :which-key "Code action"))

(my/leader-keys
  "TAB" '(:ignore t :which-key "Workspace")
  "TAB TAB" '(persp-switch :which-key "Switch workspace")
  "TAB n" '(persp-switch :which-key "New/switch workspace")
  "TAB d" '(persp-kill :which-key "Delete workspace")
  "TAB r" '(persp-rename :which-key "Rename workspace")
  "TAB [" '(persp-prev :which-key "Previous workspace")
  "TAB ]" '(persp-next :which-key "Next workspace")
  "TAB b" '(persp-switch-to-buffer :which-key "Switch buffer in workspace")
  "TAB p" '(projectile-persp-switch-project :which-key "Project → workspace"))

;; Restart Emacs
(use-package restart-emacs
  :ensure nil)

(my/leader-keys
  "qr" '(restart-emacs :which-key "Restart Emacs"))

(use-package projectile
  :ensure nil
  :init
  (projectile-mode +1)
  :config
  ;; Set where projectile looks for projects
  (setq projectile-project-search-path '("~/src/" "~/projects/" "~/"))

  ;; Ensure known projects file location
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))

  ;; Auto-discover projects
  (setq projectile-track-known-projects-automatically t)

  ;; Sets the Alien indexing method
  (setq projectile-indexing-method 'alien)

  ;; Sort results by recent
  (setq projectile-sort-order 'recentf)

  ;; Refresh project list on startup
  (projectile-discover-projects-in-search-path))

(use-package counsel-projectile
  :after (counsel projectile)
  :ensure nil
  :config
  (counsel-projectile-mode))

(use-package perspective
  :ensure nil
  :demand t
  :init
  (setq persp-mode-prefix-key (kbd "C-c M-p"))
  :config
  (persp-mode)

  ;; Show perspectives in tab-bar
  (setq tab-bar-show t
        tab-bar-new-button-show nil
        tab-bar-close-button-show nil)

  ;; Sync tab-bar with perspectives
  (add-hook 'persp-switch-hook #'persp-update-tab-bar)
  (add-hook 'persp-created-hook #'persp-update-tab-bar)
  (add-hook 'persp-killed-hook #'persp-update-tab-bar)

  (defun persp-update-tab-bar ()
    "Update tab-bar to show current perspective."
    (let ((persp-names (persp-names)))
      (setq tab-bar-tabs
            (mapcar (lambda (name)
                      `(tab
                        (name . ,name)
                        (explicit-name . t)
                        (current . ,(equal name (persp-current-name)))))
                    persp-names))))

  ;; Initial update
  (persp-update-tab-bar)

  ;; Integration with Ivy/Counsel
  (setq read-buffer-function #'persp-read-buffer)
  (setq persp-show-modestring t)

  ;; Make ibuffer respect perspectives
  (add-hook 'ibuffer-hook
            (lambda ()
              (persp-ibuffer-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))

  ;; Integration with projectile
  (with-eval-after-load 'projectile
    (require 'persp-projectile)))

;; Enhance projectile to auto-create perspectives
(with-eval-after-load 'projectile
  (defun my/projectile-switch-project-action ()
    "Switch to a perspective for the project, creating if needed."
    (persp-switch (projectile-project-name))
    (projectile-find-file))

  (setq projectile-switch-project-action #'my/projectile-switch-project-action))

(my/leader-keys
  "b"   '(:ignore t :which-key "Buffers")
  "bb"  '(persp-ivy-switch-buffer :which-key "Switch buffer")
  "bk"  '(kill-current-buffer :which-key "Kill buffer")
  "bB"  '(ivy-switch-buffer :which-key "Switch buffer (all)")
  "bs"  '(persp-ibuffer :which-key "List buffers")
  "br"  '(revert-buffer :which-key "Revert buffer")
  "bn"  '(next-buffer :which-key "Next buffer")
  "bp"  '(previous-buffer :which-key "Previous buffer"))

;; Filter switch-to-buffer
(advice-add 'switch-to-buffer :around
            (lambda (orig-fun &rest args)
              (let ((persp-buffers (persp-buffers (persp-curr))))
                (apply orig-fun args))))

(with-eval-after-load 'perspective
  ;; Make switch-to-buffer only show buffers in current perspective
  (setq read-buffer-function #'persp-read-buffer)

  ;; Make ibuffer only show current perspective's buffers
  (add-hook 'ibuffer-hook
            (lambda ()
              (persp-ibuffer-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))

  (setq persp-show-modestring t))

;; Configure gptel with multiple backends
(use-package gptel
  :ensure nil
  :config
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
  (setq gptel-backend (car gptel-backends))

  (setq gptel-model 'claude-sonnet-4-5-20250929)

  ;; PKS-aware system directive.  Ensures gptel's output uses denote's
  ;; filename conventions, link syntax, and closed keyword vocabulary
  ;; when the user asks it to produce notes or link suggestions.
  (setq gptel-directives
        (append (bound-and-true-p gptel-directives)
                '((pks
                   . "You are assisting a user whose notes live in a function-based Zettelkasten at ~/pks/ managed with Denote.
Silos: fleeting/ permanent/ literature/ projects/ reference/ (+ review-queue/, library/).
Filenames: YYYYMMDDTHHMMSS--slug__kw1_kw2.org.
Closed keyword vocabulary: _research _code _learn _project _lit _perm _fleeting _ntnu _ous _agenda _moc _meeting _hub _idea _review. Warn before proposing a new keyword outside this set.
Links: [[denote:YYYYMMDDTHHMMSSID][title]].
Citations: [cite:@citekey] with bibliography at ~/pks/library/references.bib.
When drafting a note propose: Title (asserts a single claim for permanent/), Silo, Keywords.
Prefer atomic notes (one claim per permanent note).  Hubs (MOCs) live in reference/ with _moc keyword.
Do NOT regenerate denote IDs; preserve them on rename."))))

  (setq gptel--system-message
        (alist-get 'pks gptel-directives)))

;; Capture the last gptel response (or the selected region) as a
;; fleeting denote note via denotecli.  Useful after a chat where the
;; conclusion deserves to become a trackable note.
(defun my/gptel-capture-as-fleeting (beg end)
  "Capture region BEG..END as a fleeting denote note.
If no active region, use the whole buffer."
  (interactive
   (if (use-region-p) (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (let* ((text (buffer-substring-no-properties beg end))
         (title (read-string "Title: "))
         (kw (completing-read-multiple
              "Keywords (comma-separated): "
              '("fleeting" "research" "code" "learn" "ntnu" "ous" "idea")
              nil nil "fleeting")))
    (with-temp-buffer
      (insert text)
      (shell-command-on-region
       (point-min) (point-max)
       (format "denotecli create --title %s --tags %s --dir %s --content -"
               (shell-quote-argument title)
               (shell-quote-argument (string-join kw ","))
               (shell-quote-argument (expand-file-name "~/pks/fleeting/")))
       nil t))
    (message "Captured to ~/pks/fleeting/ as %s" title)))

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

(defun my/ai-continue-code ()
  "Send selected region/function with 'continue this' prompt"
  (interactive)
  (gptel-send "Continue implementing this:" :context 'buffer))

(defun my/ai-fix-error ()
  "Fix compilation/runtime error at point"
  (interactive)
  (gptel-send "Fix this error:" :context 'buffer))

;; AI Task Helpers
(defun my/org-task-to-ai-context ()
  "Export current task with properties and context to clipboard for AI."
  (interactive)
  (let* ((heading (org-get-heading t t t t))
         (props (org-entry-properties))
         (context (or (cdr (assoc "CONTEXT" props)) "No context provided"))
         (expected (or (cdr (assoc "EXPECTED_OUTPUT" props)) "Not specified"))
         (files (or (cdr (assoc "FILES" props)) "No files specified"))
         (body (org-get-entry))
         (ai-context (save-excursion
                       (when (re-search-forward "#\\+BEGIN_AI_CONTEXT"
                                                (org-entry-end-position) t)
                         (buffer-substring-no-properties
                          (point)
                          (progn (re-search-forward "#\\+END_AI_CONTEXT")
                                 (line-beginning-position)))))))
    (kill-new
     (format "# Task: %s\n\n## Context\n%s\n\n## Expected Output\n%s\n\n## Relevant Files\n%s\n\n## Details\n%s\n\n## Additional Context\n%s"
             heading context expected files body (or ai-context "None")))))

(defun my/ai-break-down-task ()
  "Ask AI to break down current task into subtasks."
  (interactive)
  (my/org-task-to-ai-context)
  (gptel-send "Break down the copied task into actionable subtasks in org-mode format (** subtask format)."))

(defun my/ai-project-plan ()
  "Ask AI to generate project plan from description."
  (interactive)
  (let ((desc (org-get-entry)))
    (gptel-send (format "Create a project plan for:\n\n%s\n\nFormat as org-mode outline with milestones and tasks." desc))))

(defun my/ai-commit-message ()
  "Generate a commit message from staged changes via gptel."
  (interactive)
  (let* ((diff (shell-command-to-string "git diff --cached"))
         (prompt (format "Write a concise commit message (conventional commits format) for:\n\n%s" diff)))
    (gptel-request
        prompt
      :callback
      (lambda (response _info)
        (when response
          (kill-new (string-trim response))
          (message "Commit message copied: %s" (string-trim response)))))))

;; AI / gptel keybindings (under `SPC a a'; `SPC a a a' opens gptel —
;; the doubling-for-primary convention).
(my/leader-keys
  "aa"   '(:ignore t :which-key "AI models")
  "aaa"  '(gptel :which-key "Invoke gptel")
  "aab"  '(my/gptel-switch-backend :which-key "Switch AI backend")
  "aac"  '(:ignore t :which-key "AI context")
  "aacb" '(gptel-add :which-key "Add/Remove buffer to AI context")
  "aacc" '(my/ai-continue-code :which-key "Continue this")
  "aacf" '(gptel-context-add-file :which-key "Add file to AI context")
  "aacr" '(gptel-context-remove-all :which-key "Remove all AI context")
  "aaf"  '(my/ai-fix-error :which-key "Fix this error")
  "aai"  '(gptel-send :which-key "Send to AI")
  "aam"  '(gptel-menu :which-key "gptel-menu")
  "aar"  '(gptel-rewrite :which-key "AI model rewrite")
  "aax"  '(gptel-abort :which-key "Abort gptel invocation"))

;; Org task AI keybindings — local leader under `SPC m a' in org buffers.
(my/local-leader-keys
 :keymaps 'org-mode-map
 "a"  '(:ignore t :which-key "AI on task")
 "ax" '(my/org-task-to-ai-context :which-key "Copy task → AI context")
 "ab" '(my/ai-break-down-task    :which-key "Break down task")
 "ap" '(my/ai-project-plan       :which-key "Project plan"))

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

(use-package mu4e
  :ensure nil
  :commands (mu4e)
  :defer t
  :init
  ;; --- Core settings (must be set before mu4e loads) --------------
  (setq mail-user-agent 'mu4e-user-agent
        mu4e-root-maildir "~/.local/share/mail"
        mu4e-attachment-dir "~/Downloads"

        ;; Fetch + index
        mu4e-get-mail-command "sync-mail"
        mu4e-update-interval 600            ; 10 min; gentler on O365
        mu4e-index-update-in-background t
        mu4e-index-update-error-warning t
        mu4e-index-lazy-check t             ; mtime-based; fast on 35k+ msgs
        mu4e-index-cleanup t
        mu4e-hide-index-messages t
        mu4e-change-filenames-when-moving t ; play well with mbsync

        ;; UI + context policy
        mu4e-confirm-quit nil
        mu4e-split-view 'single-window
        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'ask
        mu4e-compose-format-flowed t        ; soft-wrap for mobile/institutional

        ;; Headers
        mu4e-headers-auto-update nil
        mu4e-headers-date-format "%d-%m"
        mu4e-headers-time-format "%H:%M"
        mu4e-headers-from-or-to-prefix '("" . "To ")
        mu4e-search-include-related t
        mu4e-search-skip-duplicates t
        mu4e-search-threads t
        mu4e-headers-thread-connection-prefix '("├─" "│ ")
        mu4e-headers-thread-blank-prefix '("  " "  ")
        mu4e-headers-thread-single-orphan-prefix '("─>" "  ")
        mu4e-headers-thread-orphan-prefix '("┬>" "│ ")
        mu4e-headers-thread-last-child-prefix '("└>" "  ")
        mu4e-headers-thread-child-prefix '("├>" "│ ")
        mu4e-headers-fields '((:human-date . 10)
                              (:flags . 6)
                              (:from-or-to . 22)
                              (:thread-subject))

        ;; Sending: msmtp selects account from envelope From.
        sendmail-program "msmtp"
        message-send-mail-function 'message-send-mail-with-sendmail
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-kill-buffer-on-exit t)

  :config
  ;; --- Keybindings -------------------------------------------------
  ;; C-c c is bound globally to `pks-dispatch'; the dispatch surfaces
  ;; the mail-capture group conditionally on mu4e context.  No local
  ;; mu4e override needed.
  (define-key mu4e-headers-mode-map (kbd "C--") nil)
  (define-key mu4e-view-mode-map    (kbd "C--") nil)
  (define-key mu4e-headers-mode-map (kbd "C-c u") 'mu4e-update-mail-and-index)
  (define-key mu4e-view-mode-map    (kbd "C-c u") 'mu4e-update-mail-and-index)

  ;; --- Evil tweaks -------------------------------------------------
  (with-eval-after-load 'evil
    (evil-define-key 'visual mu4e-view-mode-map    (kbd "a") 'mu4e-view-action)
    (evil-define-key 'visual mu4e-headers-mode-map (kbd "a") 'mu4e-headers-action))

  ;; --- Maildir shortcuts (press `j <key>` from main/headers) ------
  (setq mu4e-maildir-shortcuts
        '((:maildir "/rafael.palomar@ous-research.no/INBOX"   :key ?o)
          (:maildir "/rafael.palomar@ntnu.no/INBOX"           :key ?n)
          (:maildir "/rafael.palomar@ous-research.no/Sent"    :key ?s)
          (:maildir "/rafael.palomar@ous-research.no/Drafts"  :key ?d)
          (:maildir "/rafael.palomar@ous-research.no/Archive" :key ?a)))

  ;; --- Capture helpers --------------------------------------------
  ;; Direct fast-paths kept as adjuncts to `pks-dispatch' for high-
  ;; frequency mail captures.  C-c c (global) → pks-dispatch covers
  ;; the discoverable surface; these are the muscle-memory shortcuts.
  (defun my/mu4e-capture-email-task ()
    "Capture email as a quick task."
    (interactive) (org-store-link nil) (org-capture nil "me"))
  (defun my/mu4e-capture-action-item ()
    "Capture email as an action item."
    (interactive) (org-store-link nil) (org-capture nil "ma"))
  (defun my/mu4e-capture-waiting ()
    "Mark email as waiting for response."
    (interactive) (org-store-link nil) (org-capture nil "mw"))

  (dolist (map (list mu4e-headers-mode-map mu4e-view-mode-map))
    (define-key map (kbd "C-c t") 'my/mu4e-capture-email-task)
    (define-key map (kbd "C-c a") 'my/mu4e-capture-action-item)
    (define-key map (kbd "C-c w") 'my/mu4e-capture-waiting))

  ;; --- Contexts (per-context From + signature + folders) ----------
  ;; mu4e-compose-signature (set per-context below) is still the
  ;; canonical way to declare a signature in mu4e 1.12; the old
  ;; `mu4e-compose-signature-auto-include' flag was obsoleted.
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "OUS-Research"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/rafael.palomar@ous-research.no"
                               (mu4e-message-field msg :maildir))))
          :vars
          `((user-mail-address       . "rafael.palomar@ous-research.no")
            (user-full-name          . "Rafael Palomar")
            (mu4e-drafts-folder      . "/rafael.palomar@ous-research.no/Drafts")
            (mu4e-sent-folder        . "/rafael.palomar@ous-research.no/Sent")
            (mu4e-trash-folder       . "/rafael.palomar@ous-research.no/Trash")
            (mu4e-refile-folder      . "/rafael.palomar@ous-research.no/Archive")
            (mu4e-compose-signature
             . ,(concat
                 "Prof. Rafael Palomar, Ph.D.\n"
                 "__________________________________\n"
                 "Head of Medical Software Research Laboratory (MESH|Lab)\n"
                 "The Intervention Centre, Oslo University Hospital (OUH)\n"
                 "Sognsvannsveien 20 (Rikshospitalet Building D-6.3002)\n"
                 "N-0372 Oslo, Norway\n"
                 "rafael.palomar@ous-research.no\n"
                 "https://ivs.no\n--"))))
         (make-mu4e-context
          :name "NTNU"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/rafael.palomar@ntnu.no"
                               (mu4e-message-field msg :maildir))))
          :vars
          `((user-mail-address       . "rafael.palomar@ntnu.no")
            (user-full-name          . "Rafael Palomar")
            (mu4e-drafts-folder      . "/rafael.palomar@ntnu.no/Drafts")
            (mu4e-sent-folder        . "/rafael.palomar@ntnu.no/Sent")
            (mu4e-trash-folder       . "/rafael.palomar@ntnu.no/Trash")
            (mu4e-refile-folder      . "/rafael.palomar@ntnu.no/Archive")
            (mu4e-compose-signature
             . ,(concat
                 "Prof. Rafael Palomar, Ph.D.\n"
                 "__________________________________\n"
                 "Associate Professor\n"
                 "Norwegian University of Science and Technology (NTNU)\n"
                 "Teknologiveien 22, 2815 Gjøvik, Norway\n"
                 "rafael.palomar@ntnu.no\n"
                 "https://ntnu.no\n--")))))))

(use-package mu4e-dashboard
  :ensure nil
  :after mu4e
  :config
  (require 'svg-lib)
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

;; mu4e-alert for desktop notifications
(use-package mu4e-alert
  :ensure nil
  :after mu4e
  :config
  (mu4e-alert-set-default-style 'libnotify)
  (mu4e-alert-enable-notifications)
  (mu4e-alert-enable-mode-line-display))

;; Org → HTML email via org-mime (replaces removed `org-mu4e`).
;; Compose in plain text/org syntax; press `C-c M-o` (or `SPC a m h`) in
;; the message buffer to convert to multipart/alternative + inline HTML
;; before sending.
(with-eval-after-load 'org-mime
  (define-key message-mode-map (kbd "C-c M-o") 'org-mime-htmlize))

;; mu4e bookmarks for quick navigation
(with-eval-after-load 'mu4e
  (setq mu4e-bookmarks
        '((:name "Unread"       :query "flag:unread AND NOT flag:trashed" :key ?u)
          (:name "Today"        :query "date:today..now"                  :key ?t)
          (:name "Last 7 days"  :query "date:7d..now" :hide-unread t      :key ?w)
          (:name "With images"  :query "mime:image/*"                     :key ?p)
          (:name "Flagged"      :query "flag:flagged"                     :key ?f)
          (:name "NTNU"         :query "maildir:/rafael.palomar@ntnu.no/*"          :key ?n)
          (:name "OUS"          :query "maildir:/rafael.palomar@ous-research.no/*"  :key ?o)
          (:name "Large (>5MB)" :query "size:5M..500M"                    :key ?l))))

;; Mail keybindings — under `SPC a m' (Applications → Mail).  `SPC m' is
;; the major-mode local leader (Doom/Spacemacs convention).
(my/leader-keys
  "am"   '(:ignore t :which-key "Mail")
  "amm"  '(mu4e :which-key "Open mu4e")
  "amq"  '(mu4e-quit :which-key "Quit mu4e")

  "amc"  '(:ignore t :which-key "Compose")
  "amcc" '(mu4e-compose-new :which-key "Compose new")
  "amh"  '(org-mime-htmlize :which-key "Htmlize compose (org → HTML)")

  "amr"  '(:ignore t :which-key "Reply")
  "amrr" '(mu4e-compose-reply :which-key "Reply")
  "amra" '(mu4e-compose-wide-reply :which-key "Reply all"))

(use-package notmuch
  :ensure nil
  :commands (notmuch notmuch-search notmuch-hello)
  :init
  (setq notmuch-show-logo nil
        notmuch-search-oldest-first nil
        notmuch-show-all-tags-list t
        notmuch-archive-tags '("-inbox" "-unread")
        notmuch-hello-sections '(notmuch-hello-insert-saved-searches
                                 notmuch-hello-insert-search
                                 notmuch-hello-insert-recent-searches)
        notmuch-saved-searches
        '((:name "inbox"   :query "tag:inbox and tag:unread"                       :key "i")
          (:name "unread"  :query "tag:unread"                                     :key "u")
          (:name "today"   :query "date:today.."                                   :key "t")
          (:name "flagged" :query "tag:flagged"                                    :key "f")
          (:name "ntnu"    :query "path:rafael.palomar@ntnu.no/**"                 :key "n")
          (:name "ous"     :query "path:rafael.palomar@ous-research.no/**"         :key "o"))))

(use-package consult-notmuch
  :ensure nil
  :after notmuch
  :commands (consult-notmuch consult-notmuch-tree))

(my/leader-keys
  "amn"  '(:ignore t :which-key "Notmuch")
  "amnn" '(notmuch :which-key "Notmuch hello")
  "amns" '(notmuch-search :which-key "Search")
  "amni" '((lambda () (interactive) (notmuch-search "tag:inbox")) :which-key "Inbox")
  "amnu" '((lambda () (interactive) (notmuch-search "tag:unread")) :which-key "Unread")
  "amnc" '(consult-notmuch :which-key "Consult search")
  "amnt" '(consult-notmuch-tree :which-key "Consult tree"))

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
  ;; (optional) keep TRAMP's default remote path plus the usual ones
  (add-to-list 'tramp-connection-properties
               '(".*" "remote-path"
                 ("/usr/local/sbin" "/usr/local/bin" "/usr/bin" "/bin"
                  tramp-own-remote-path))))

;; Icons in dired
(use-package all-the-icons-dired
  :ensure nil
  :hook (dired-mode . all-the-icons-dired-mode))

;; Dired configurations
(use-package dired
  :ensure nil
  :custom
  ;; Copy/move to other dired window
  (dired-dwim-target t)

  ;; Better listings
  (dired-listing-switches "-alh --group-directories-first")

  ;; Reuse same buffer
  (dired-kill-when-opening-new-dired-buffer t)

  :hook
  ;; Hide details by default
  (dired-mode . dired-hide-details-mode)

  :config
  ;; Enable dired-x for extra features
  (require 'dired-x)

  ;; Hide dotfiles by default (toggle with M-o)
  (setq dired-omit-files "^\\.[^.]")

  ;; Use 'a' to reuse the same buffer
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Better keybindings
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file ".."))))

;; Colorful dired
(use-package diredfl
  :ensure nil
  :hook (dired-mode . diredfl-mode))

;; Extra dired features
(use-package dired-aux
  :ensure nil
  :config
  (setq dired-create-destination-dirs 'ask
        dired-vc-rename-file t))

;; Image-dired - Thumbnail support for image files
(use-package image-dired
  :ensure nil
  :custom
  ;; Use ImageMagick for thumbnail generation (instead of vipsthumbnail)
  (image-dired-cmd-create-thumbnail-program "convert")
  (image-dired-cmd-create-thumbnail-options
   '("-size" "%wx%h" "%f[0]" "-resize" "%wx%h>" "-strip" "jpeg:%t"))
  ;; Thumbnail storage location
  (image-dired-dir (expand-file-name "image-dired" user-emacs-directory))
  (image-dired-thumbnail-storage 'standard))

;; Dirvish - Modern file manager with previews and Miller columns
(use-package dirvish
  :ensure nil
  :init
  ;; Enable dirvish globally (overrides default dired)
  (dirvish-override-dired-mode)

  ;; Show file attributes in the header line
  (dirvish-attributes '(file-size file-time))

  ;; Enable previews for various file types (excluding 'image since vipsthumbnail is not available)
  (dirvish-preview-dispatchers '(gif video audio epub archive pdf))

  ;; Cache preview images for better performance
  (dirvish-cache-dir (expand-file-name "dirvish-cache" user-emacs-directory))

  ;; Use header and mode lines for better UI
  (dirvish-use-header-line 'global)
  (dirvish-use-mode-line 'global)

  :config
  ;; Miller columns layout (3-pane view)
  (setq dirvish-default-layout '(0 0.4 0.6))  ; Parent:Current:Preview ratio

  ;; Configure external applications for specific file types
  ;; Use xdg-open for media files instead of opening in Emacs
  (setq dired-guess-shell-alist-user
        '(("\\.\\(mp4\\|mkv\\|avi\\|mov\\|wmv\\|flv\\|webm\\)\\'" "xdg-open")
          ("\\.\\(mp3\\|flac\\|wav\\|ogg\\|m4a\\)\\'" "xdg-open")
          ("\\.\\(jpg\\|jpeg\\|png\\|gif\\|bmp\\|svg\\)\\'" "xdg-open")
          ("\\.\\(pdf\\|djvu\\)\\'" "xdg-open")
          ("\\.\\(xlsx?\\|docx?\\|pptx?\\)\\'" "xdg-open")))

  ;; Make dired-find-file use external apps for media files
  (defun my/dired-open-file ()
    "In dired, open the file at point with external application if appropriate."
    (interactive)
    (let* ((file (dired-get-filename nil t))
           (ext (when file (file-name-extension file))))
      (if (and ext
               (member (downcase ext)
                       '("mp4" "mkv" "avi" "mov" "wmv" "flv" "webm"
                         "mp3" "flac" "wav" "ogg" "m4a"
                         "jpg" "jpeg" "png" "gif" "bmp")))
          (call-process "xdg-open" nil 0 nil file)
        (dired-find-file))))

  ;; Define Evil keybindings for dirvish (normal state)
  (evil-define-key 'normal dirvish-mode-map
    ;; Navigation
    (kbd "l") 'my/dired-open-file              ; Open file/directory (with external apps)
    (kbd "h") 'dired-up-directory              ; Go to parent
    (kbd "q") 'dirvish-quit                    ; Quit
    (kbd "RET") 'my/dired-open-file            ; Also bind RET for consistency
    (kbd "j") 'dired-next-line                 ; Move down
    (kbd "k") 'dired-previous-line             ; Move up
    (kbd "gg") 'evil-goto-first-line           ; Go to top
    (kbd "G") 'evil-goto-line                  ; Go to bottom

    ;; File operations
    (kbd "C") 'dired-do-copy                   ; Copy
    (kbd "R") 'dired-do-rename                 ; Rename/move
    (kbd "D") 'dired-do-delete                 ; Delete
    (kbd "d") 'dired-flag-file-deletion        ; Flag for deletion
    (kbd "x") 'dired-do-flagged-delete         ; Execute deletions
    (kbd "+") 'dired-create-directory          ; Create directory
    (kbd "Z") 'dired-do-compress               ; Compress/uncompress
    (kbd "c") 'dired-do-compress-to            ; Compress to specific file

    ;; Marking
    (kbd "m") 'dired-mark                      ; Mark file
    (kbd "u") 'dired-unmark                    ; Unmark file
    (kbd "U") 'dired-unmark-all-marks          ; Unmark all
    (kbd "t") 'dired-toggle-marks              ; Toggle marks
    (kbd "*") 'dired-mark-executables          ; Mark executables
    (kbd "%") 'dired-mark-files-regexp         ; Mark by regexp

    ;; View/Edit
    (kbd "o") 'dired-find-file-other-window    ; Open in other window
    (kbd "gr") 'revert-buffer                  ; Refresh
    (kbd "i") 'dired-maybe-insert-subdir       ; Insert subdir
    (kbd ".") 'dired-omit-mode                 ; Toggle hidden files
    (kbd "e") 'wdired-change-to-wdired-mode    ; Edit filenames inline (wdired)

    ;; Permissions & ownership
    (kbd "M") 'dired-do-chmod                  ; Change mode
    (kbd "O") 'dired-do-chown                  ; Change owner
    (kbd "gG") 'dired-do-chgrp                 ; Change group

    ;; Dirvish-specific features
    (kbd "?") 'dirvish-dispatch                ; Command menu
    (kbd "f") 'dirvish-file-info-menu          ; File info
    (kbd "y") 'dirvish-yank-menu               ; Yank/copy menu
    (kbd "s") 'dirvish-quicksort               ; Sort menu
    (kbd "TAB") 'dirvish-subtree-toggle        ; Toggle subtree
    (kbd "a") 'dirvish-quick-access            ; Quick access
    (kbd "v") 'dirvish-vc-menu                 ; Version control
    (kbd "M-l") 'dirvish-ls-switches-menu      ; Listing options
    (kbd "M-e") 'dirvish-emerge-menu           ; Batch operations
    (kbd "M-j") 'dirvish-fd-jump               ; Fast jump with fd
    (kbd "M-s") 'dirvish-setup-menu            ; Setup/config menu
    (kbd "M-n") 'dirvish-narrow                ; Narrow/filter
    (kbd "M-m") 'dirvish-mark-menu             ; Mark operations
    (kbd "M-t") 'dirvish-layout-toggle         ; Toggle layout
    (kbd "M-b") 'dirvish-history-go-backward
    (kbd "M-f") 'dirvish-history-go-forward))

;; Global keybindings for dirvish (using SPC leader)
(my/leader-keys
  "d"   '(:ignore t :which-key "Dired/Dirvish")
  "dd"  '(dirvish :which-key "Open dirvish")
  "dj"  '(dirvish-fd-jump :which-key "Jump with fd")
  "ds"  '(dirvish-side :which-key "Dirvish sidebar")
  "dh"  '(dirvish-history-jump :which-key "Jump to history")
  "da"  '(dirvish-quick-access :which-key "Quick access"))

(use-package denote
  :ensure nil
  :config
  (setq denote-directory (expand-file-name "~/pks/fleeting/"))

  (setq denote-silo-extras-directories
        '(("permanent"  . "~/pks/permanent/")
          ("literature" . "~/pks/literature/")
          ("projects"   . "~/pks/projects/")
          ("reference"  . "~/pks/reference/")
          ("review"     . "~/pks/review-queue/")
          ("archive"    . "~/pks/archive/")
          ("legacy"     . "~/Notes/Work-legacy/")))

  (setq denote-known-keywords
        '("research" "code" "learn" "project" "lit" "perm" "fleeting"
          "ntnu" "ous" "agenda" "moc" "meeting" "hub" "idea" "review"))

  (setq denote-infer-keywords t
        denote-sort-keywords t
        denote-file-type 'org
        denote-prompts '(title keywords)
        denote-date-prompt-use-org-read-date t
        denote-rename-confirmations nil
        denote-excluded-directories-regexp "legacy\\|review-queue")

  (defvar my-denote-to-agenda-regexp "_agenda"
    "Denote file names that are added to the agenda.
See `my-denote-add-to-agenda'.")

  (defun my-denote-add-to-agenda ()
    "Add current file to `org-agenda-files' if it matches the regexp."
    (interactive)
    (when-let* ((file (buffer-file-name))
                ((denote-file-is-note-p file))
                ((string-match-p my-denote-to-agenda-regexp file)))
      (add-to-list 'org-agenda-files file)))

  (defun my-denote-remove-from-agenda ()
    "Remove current file from `org-agenda-files'."
    (interactive)
    (when-let* ((file (buffer-file-name))
                ((string-match-p my-denote-to-agenda-regexp file)))
      (setq org-agenda-files (delete file org-agenda-files))))

  (add-hook 'after-save-hook #'my-denote-add-to-agenda)

  (defun my-denote-refresh-agenda ()
    "Seed `org-agenda-files' with all _agenda notes across PKS silos."
    (interactive)
    (dolist (silo '("~/pks/projects/" "~/pks/fleeting/" "~/pks/permanent/"))
      (when (file-directory-p (expand-file-name silo))
        (dolist (f (directory-files-recursively
                    (expand-file-name silo) "_agenda.*\\.org\\'"))
          (add-to-list 'org-agenda-files f)))))
  (add-hook 'after-init-hook #'my-denote-refresh-agenda)

  ;; Rescope advice for cross-silo link-following.
  ;; `denote-link-ol-follow' resolves [[denote:ID]] via `denote-get-path-by-id',
  ;; which scans `denote-directory' recursively.  With denote-directory pinned
  ;; to ~/pks/fleeting/, cross-silo links (permanent/, literature/, projects/,
  ;; reference/) fail with "Cannot open ID of unknown link type".  Widen
  ;; denote-directory to ~/pks/ when following from a PKS buffer.
  ;;
  ;; Defined here in denote's :config (not denote-org's, which is lazy via
  ;; :commands) so the advice installs eagerly at startup — otherwise the
  ;; first link-follow attempt happens before denote-org loads, and the
  ;; advice is never wired.
  ;;
  ;; ~/pks/ is a symlink into ~/Nextcloud/PKS/; resolve both sides to their
  ;; true names before comparing, otherwise file-truename on the buffer path
  ;; yields the Nextcloud path and the prefix match fails.
  (defun my-denote-rescope-link-follow (orig-fun &rest args)
    "Around-advice on `denote-link-ol-follow' to widen `denote-directory'
to the PKS root when called from a buffer inside ~/pks/."
    (let* ((file (buffer-file-name))
           (pks-link (expand-file-name "~/pks/"))
           (pks-real (file-truename pks-link))
           (inside-pks (and file
                            (let ((true (file-truename file)))
                              (or (string-prefix-p pks-link true)
                                  (string-prefix-p pks-real true)))))
           (denote-directory (if inside-pks pks-link denote-directory)))
      (apply orig-fun args)))
  (advice-add 'denote-link-ol-follow :around #'my-denote-rescope-link-follow))

(use-package denote-silo
  :ensure nil
  :commands (denote-silo-create-note
             denote-silo-open-or-create
             denote-silo-select-silo-then-command
             denote-silo-dired
             denote-silo-cd)
  :config
  (setq denote-silo-directories
        (list denote-directory
              "~/pks/permanent/"
              "~/pks/literature/"
              "~/pks/projects/"
              "~/pks/reference/"
              "~/pks/review-queue/"
              "~/Notes/Work-legacy/")))

(use-package denote-org
  :ensure nil
  :after denote
  :commands (denote-org-link-to-heading
             denote-org-extract-org-subtree
             denote-org-dblock-insert-backlinks
             denote-org-dblock-insert-links
             denote-org-dblock-insert-missing-links)
  :config
  ;; Denote-org's dblocks scan `denote-directory', which in this setup is
  ;; ~/pks/fleeting/ (the default capture silo).  Hub / MOC notes live in
  ;; reference/ and want to see links across ALL silos, so rescope the
  ;; dblock update to the PKS root.  ~/pks/ is a symlink into
  ;; ~/Nextcloud/PKS/; resolve both sides to their true names before
  ;; comparing, otherwise file-truename on the buffer path yields the
  ;; Nextcloud path and the prefix match fails.
  ;;
  ;; (The sibling `my-denote-rescope-link-follow' lives in denote's :config
  ;; so it installs eagerly — link-following must work before any dblock
  ;; command first triggers denote-org's load.)
  (defun my-denote-rescope-dblock-update (orig-fun &rest args)
    (let* ((file (buffer-file-name))
           (pks-link (expand-file-name "~/pks/"))
           (pks-real (file-truename pks-link))
           (inside-pks (and file
                            (let ((true (file-truename file)))
                              (or (string-prefix-p pks-link true)
                                  (string-prefix-p pks-real true)))))
           (denote-directory (if inside-pks pks-link denote-directory)))
      (apply orig-fun args)))
  (advice-add 'org-update-dblock     :around #'my-denote-rescope-dblock-update)
  (advice-add 'org-update-all-dblocks :around #'my-denote-rescope-dblock-update))

;; consult-denote: Vertico/Marginalia-style fuzzy search + grep across
;; the PKS corpus with live preview.  Scoped globally to ~/pks/ so one
;; search hits every silo (default denote-directory is only the
;; fleeting silo).
(use-package consult-denote
  :ensure nil
  :after denote
  :commands (consult-denote-find consult-denote-grep)
  :config
  (consult-denote-mode 1))

;; Forward-declare consult-project-function as dynamic so our let-binds
;; below don't get treated as lexical in this lexical-binding-on file
;; before consult.el's defcustom has fired.  Without this, byte-compile
;; / first eval warns "defining as dynamic and already lexical var".
(defvar consult-project-function)

;; Rescope read-side denote/consult-denote commands to ~/pks/ when
;; the buffer or denote-directory is already under PKS, so M-x and
;; embark calls reach every silo without the my-pks-* wrappers.
;; Mirrors the dblock/link-follow advice on denote-org above.
(defun my-denote-rescope-pks-read (orig-fun &rest args)
  (let* ((file (buffer-file-name))
         (pks-link (expand-file-name "~/pks/"))
         (pks-real (file-truename pks-link))
         (under-pks (lambda (path)
                      (and path
                           (let ((true (file-truename path)))
                             (or (string-prefix-p pks-link true)
                                 (string-prefix-p pks-real true))))))
         (rescope (or (funcall under-pks file)
                      (funcall under-pks denote-directory)))
         (denote-directory (if rescope pks-link denote-directory))
         ;; consult-grep / consult-find prefer
         ;; `consult-project-function' over the directory argument,
         ;; and projectile sees ~/ as a project — so without these
         ;; rebinds the consult commands jump to ~/ instead of ~/pks/.
         (default-directory (if rescope pks-link default-directory))
         (consult-project-function (if rescope nil consult-project-function)))
    (apply orig-fun args)))

(with-eval-after-load 'denote
  (advice-add 'denote-find-file :around #'my-denote-rescope-pks-read))
(with-eval-after-load 'consult-denote
  (advice-add 'consult-denote-find :around #'my-denote-rescope-pks-read)
  (advice-add 'consult-denote-grep :around #'my-denote-rescope-pks-read))

;; The completion stack is ivy/counsel (completing-read-function =
;; ivy-completing-read), not vertico — so consult-grep's async streaming
;; doesn't render through ivy's minibuffer.  Use counsel-rg for content
;; search and counsel-file-jump for filename search; both natively
;; integrate with the user's ivy UX.  Function names retain the
;; "consult" stem so the existing keybindings keep their aliases.
(defun my-pks-consult-find ()
  "Find a file across the whole PKS tree by name (counsel-file-jump)."
  (interactive)
  (let ((default-directory (expand-file-name "~/pks/")))
    (counsel-file-jump "" (expand-file-name "~/pks/"))))

(defun my-pks-consult-grep ()
  "Grep across the whole PKS tree for content (counsel-rg)."
  (interactive)
  (let ((default-directory (expand-file-name "~/pks/")))
    (counsel-rg "" (expand-file-name "~/pks/") nil "PKS rg: ")))

(defun my-pks-grep-debug (query)
  "Synchronously grep ~/pks/ for QUERY and message the result count.
Use to verify the wrapper context can actually find matches without
going through consult's async layer."
  (interactive "sQuery: ")
  (let* ((default-directory (expand-file-name "~/pks/"))
         (consult-project-function nil)
         (paths (with-temp-buffer
                  (call-process "grep" nil t nil "-rl" query
                                (expand-file-name "~/pks/"))
                  (split-string (buffer-string) "\n" t))))
    (message "my-pks-grep-debug: default-directory=%s consult-project-fn=%S | %d files match %S | first: %s"
             default-directory
             consult-project-function
             (length paths)
             query
             (or (car paths) "none"))))

(defun my-pks-consult-grep-trace (toggle)
  "Toggle :around advice that logs every make-process called by consult.
Call once to enable, again to disable.  After enabling, run
\\[my-pks-consult-grep], type a query, then check *Messages* for
DEBUG-make-process lines reporting cwd and command."
  (interactive (list (if (advice-member-p
                          #'my-pks-consult-grep--make-process-spy
                          'make-process)
                         'off
                       'on)))
  (cond
   ((eq toggle 'on)
    (advice-add 'make-process :around
                #'my-pks-consult-grep--make-process-spy)
    (message "make-process trace: ON (run C-c n g, then check *Messages*)"))
   ((eq toggle 'off)
    (advice-remove 'make-process #'my-pks-consult-grep--make-process-spy)
    (message "make-process trace: OFF"))))

(defun my-pks-consult-grep--make-process-spy (orig-fun &rest args)
  (let ((plist (car args)))
    (when (and (listp plist) (plist-get plist :command))
      (let ((cmd (plist-get plist :command))
            (name (plist-get plist :name)))
        (when (or (string-match-p "grep" (or name ""))
                  (string-match-p "consult" (or name ""))
                  (and (consp cmd) (string-match-p "grep" (or (car cmd) ""))))
          (message "DEBUG-make-process: name=%s cwd=%s cmd=%S"
                   name default-directory cmd)))))
  (apply orig-fun args))

(defun my-pks-show-daily-review ()
  "Run the pks-daily-review fallback script and visit the resulting note.
Non-LLM digest of stale fleeting, active _agenda projects, and stale
MOCs.  Idempotent per day; same-date re-runs overwrite."
  (interactive)
  (let* ((cmd (executable-find "pks-daily-review"))
         (output (and cmd (string-trim
                           (shell-command-to-string
                            (shell-quote-argument cmd))))))
    (cond
     ((not cmd)
      (user-error "pks-daily-review not found on PATH"))
     ((and output (file-exists-p output))
      (find-file output))
     (t
      (user-error "pks-daily-review produced no file: %s" output)))))

(defun my-pks-show-weekly-review ()
  "Run the pks-weekly-review fallback script and visit the resulting note.
Project-level Sunday digest: stale projects (mtime >14d), stale _agenda
notes anywhere in PKS (>14d), and stale review-queue items (>30d).
Idempotent per day; same-date re-runs overwrite."
  (interactive)
  (let* ((cmd (executable-find "pks-weekly-review"))
         (output (and cmd (string-trim
                           (shell-command-to-string
                            (shell-quote-argument cmd))))))
    (cond
     ((not cmd)
      (user-error "pks-weekly-review not found on PATH"))
     ((and output (file-exists-p output))
      (find-file output))
     (t
      (user-error "pks-weekly-review produced no file: %s" output)))))

;; C-c n prefix keymap (Protesilaos-style).  Define BEFORE any
;; `with-eval-after-load' form that references it: those bodies fire
;; immediately if denote is already loaded.
(define-prefix-command 'my-pks-prefix-map)

(with-eval-after-load 'denote
  (define-key my-pks-prefix-map (kbd ".") #'my-pks-consult-find)
  (define-key my-pks-prefix-map (kbd "?") #'my-pks-consult-grep))

;; Denote capture templates — body inserted below front-matter.
(with-eval-after-load 'denote
  (setq denote-templates
        '((fleeting
           . "* Thought\n\n")
          (literature
           . "* Source\n- Author:\n- Year:\n- URL:\n\n* Key claims\n\n* My notes\n")
          (project
           . "* Status\nBrief current state.\n\n* Next actions\n** NEXT \n\n* Log\n- %U :: \n\n* Architecture / patterns\n\n* References\n")
          (moc
           . "* Purpose\n\n* Pinned notes\n\n#+BEGIN: denote-backlinks\n#+END:\n"))))

;; Silo-routing capture helpers.  Each let-binds `denote-directory'
;; to the target silo plus the matching denote-use-keywords/template,
;; then invokes the standard `denote' command.  This is the
;; "convenience commands for note creation" pattern from the denote
;; manual (section 5.1).  `denote' opens a buffer for the new note;
;; the user fills in the body and `C-x C-s' to save (or relies on
;; `denote-save-buffers' = t — currently nil).
(defun my-pks-capture-fleeting ()
  "Create a denote fleeting note in ~/pks/fleeting/."
  (interactive)
  (let ((denote-directory (expand-file-name "~/pks/fleeting/"))
        (denote-use-keywords '("fleeting"))
        (denote-use-template 'fleeting))
    (denote)))

(defun my-pks-capture-literature ()
  "Create a denote literature note in ~/pks/literature/."
  (interactive)
  (let ((denote-directory (expand-file-name "~/pks/literature/"))
        (denote-use-keywords '("lit"))
        (denote-use-template 'literature))
    (denote)))

(defun my-pks-capture-project ()
  "Create a denote project note in ~/pks/projects/."
  (interactive)
  (let ((denote-directory (expand-file-name "~/pks/projects/"))
        (denote-use-keywords '("project" "agenda"))
        (denote-use-template 'project))
    (denote)))

(defun my-pks-capture-hub ()
  "Create a denote hub/MOC note in ~/pks/reference/."
  (interactive)
  (let ((denote-directory (expand-file-name "~/pks/reference/"))
        (denote-use-keywords '("moc" "hub"))
        (denote-use-template 'moc))
    (denote)))

;; Mail-capture targets: route mu4e thread captures into PKS instead
;; of inbox.org.  Email-Task / Follow-Up land in fleeting/ as denote
;; notes carrying the message-id property (so the thread can be
;; cross-referenced later); the closed-vocabulary tag is derived from
;; the active mu4e context (ous / ntnu).
(defun my-mu4e--context-tag ()
  "Closed-vocabulary keyword for the current mu4e context, or nil."
  (let ((name (and (boundp 'mu4e--context-current)
                   mu4e--context-current
                   (mu4e-context-name mu4e--context-current))))
    (cond ((equal name "OUS-Research") "ous")
          ((equal name "NTNU") "ntnu"))))

(defun my-mu4e-capture-target-project-log ()
  "Org-capture target: pick a project, position point under its * Log.
Prompts via `completing-read' against ~/pks/projects/*.org filenames
and jumps point to the line following the first `* Log' heading.
Used by the mail-derived Action Item and Waiting For workflows."
  (let* ((projects-dir (expand-file-name "~/pks/projects/"))
         (project-files
          (directory-files-recursively projects-dir "\\.org\\'"))
         (file-alist
          (mapcar (lambda (f) (cons (file-name-base f) f)) project-files))
         (choice (completing-read "Project: " file-alist nil t))
         (path (cdr (assoc choice file-alist))))
    (find-file path)
    (goto-char (point-min))
    (unless (re-search-forward "^\\* Log\\b" nil t)
      (user-error "No `* Log' heading in %s" path))
    (forward-line 1)
    (while (looking-at-p "^[[:space:]]*$")
      (forward-line 1))))

(defun my-pks-find-project (query)
  "Jump to a projects/ note whose filename contains QUERY."
  (interactive "sProject: ")
  (let* ((files (directory-files-recursively
                 (expand-file-name "~/pks/projects/") "\\.org\\'"))
         (match (seq-find (lambda (f) (string-match-p query f)) files)))
    (if match (find-file match)
      (user-error "No matching project note: %s" query))))

;; ─── Move / promote helpers ────────────────────────────────────────
;; Move (or promote) a denote note across PKS silos using Denote's
;; native rename API, preserving the denote ID so backlinks survive.
;; After a successful move the buffer follows the file to its new silo.
(defun my-pks--silo-path (label)
  "Return the absolute path of silo LABEL.
Includes \"fleeting\" (the default `denote-directory') plus everything
in `denote-silo-extras-directories'."
  (cond
   ((string= label "fleeting") (expand-file-name "~/pks/fleeting/"))
   (t (let ((cell (assoc label denote-silo-extras-directories)))
        (and cell (expand-file-name (cdr cell)))))))

(defun my-pks--silo-labels ()
  "All known PKS silo labels for completion."
  (cons "fleeting" (mapcar #'car denote-silo-extras-directories)))

(defun my-pks--move-current-to (silo &optional kw-vocab restrict-to-path)
  "Move the current denote note to SILO, preserving its denote ID.
KW-VOCAB seeds `completing-read-multiple' for new keywords (empty = keep
existing).  If RESTRICT-TO-PATH is non-nil, error unless the buffer's
file lives under that path.

Uses Denote's native rename API rather than denotecli, since denotecli
0.8.0 only exposes read/search/create operations."
  (let ((file (buffer-file-name)))
    (unless (and file (denote-file-is-note-p file))
      (user-error "Current buffer is not a denote note"))
    (when restrict-to-path
      (unless (string-prefix-p (expand-file-name restrict-to-path) file)
        (user-error "Current note is not under %s" restrict-to-path)))
    (let ((silo-dir (my-pks--silo-path silo)))
      (unless silo-dir
        (user-error "Unknown PKS silo: %s" silo))
      (let* ((id      (denote-retrieve-filename-identifier file))
             (title   (or (denote-retrieve-title-or-filename file 'org) ""))
             (sig     (or (denote-retrieve-filename-signature file) ""))
             (ext     (file-name-extension file t))
             (kw-in   (completing-read-multiple
                       (format "%s keywords (comma-separated, empty = keep): "
                               silo)
                       kw-vocab))
             (new-kw  (if kw-in kw-in
                        (denote-extract-keywords-from-path file)))
             (new-path (denote-format-file-name
                        (file-name-as-directory silo-dir)
                        id new-kw title ext sig)))
        (when (file-equal-p file new-path)
          (user-error "Note is already in %s with the same keywords" silo))
        (save-buffer)
        (denote-rename-file-and-buffer file new-path)
        (message "Moved %s → %s" id silo)))))

(defun my-denote-promote-fleeting-to-permanent ()
  "Promote the current fleeting note to permanent/ (preserves denote ID)."
  (interactive)
  (my-pks--move-current-to
   "permanent"
   '("perm" "research" "code" "learn" "ntnu" "ous")
   "~/pks/fleeting/"))

(defun my-denote-promote-to-reference ()
  "Promote the current note to reference/ as a hub/MOC (preserves denote ID)."
  (interactive)
  (my-pks--move-current-to
   "reference"
   '("moc" "hub" "research" "code" "learn" "ntnu" "ous")))

(defun my-denote-archive-fleeting ()
  "Archive the current fleeting note to archive/{work,personal}/ (preserves denote ID).
Cold storage for notes worth keeping but not load-bearing — light
meeting notes, ephemeral observations, traces that won't ripen into
permanent claims.  Drops the `fleeting' keyword; does NOT add an
`archive' keyword (the silo is the marker).  One-way: notes do not
move back from archive.  Asks for explicit yes-or-no confirmation."
  (interactive)
  (let ((file (buffer-file-name)))
    (unless (and file (denote-file-is-note-p file))
      (user-error "Current buffer is not a denote note"))
    (unless (string-prefix-p (expand-file-name "~/pks/fleeting/") file)
      (user-error "Current note is not in fleeting/"))
    (let* ((kws (denote-extract-keywords-from-path file))
           (default-domain (if (or (member "ous" kws) (member "ntnu" kws))
                               "work" "personal"))
           (domain (completing-read
                    (format "Archive domain (default %s): " default-domain)
                    '("work" "personal") nil t nil nil default-domain))
           (archive-dir (expand-file-name (format "~/pks/archive/%s/" domain)))
           (new-kws (remove "fleeting" kws))
           (id (denote-retrieve-filename-identifier file))
           (title (or (denote-retrieve-title-or-filename file 'org) ""))
           (sig (or (denote-retrieve-filename-signature file) ""))
           (ext (file-name-extension file t))
           (new-path (denote-format-file-name
                      (file-name-as-directory archive-dir)
                      id new-kws title ext sig)))
      (unless (yes-or-no-p
               (format "Archive %s → archive/%s/ ? (one-way; cannot un-archive) "
                       id domain))
        (user-error "Aborted"))
      (save-buffer)
      (denote-rename-file-and-buffer file new-path)
      (message "Archived %s → archive/%s" id domain))))

(defun my-pks-move-note-to-silo ()
  "Move the current denote note to a chosen PKS silo (preserves denote ID).
Generic counterpart to `my-denote-promote-fleeting-to-permanent' — pick
any target silo from `denote-silo-extras-directories' (plus fleeting)."
  (interactive)
  (let ((target (completing-read "Move to silo: "
                                 (my-pks--silo-labels) nil t)))
    (my-pks--move-current-to target denote-known-keywords)))

;; Bind the C-c n prefix once denote is available.  The prefix map
;; itself is created earlier in this file; binding it here keeps the
;; top-level binding gated on denote being usable.  We also mirror the
;; map onto `SPC n' so PKS has a single canonical leader home (the
;; previous `SPC n d/w' shortcuts now arrive via the map at `D'/`W').
(with-eval-after-load 'denote
  (global-set-key (kbd "C-c n") 'my-pks-prefix-map)
  (my/leader-keys "n" `(,my-pks-prefix-map :which-key "Notes/PKS"))
  (let ((m my-pks-prefix-map))
    (define-key m "c" #'denote)
    (define-key m "C" #'denote-open-or-create)
    (define-key m "s" #'denote-silo-create-note)
    (define-key m "S" #'denote-silo-select-silo-then-command)
    (define-key m "d" #'denote-silo-dired)
    (define-key m "n" #'my-pks-capture-fleeting)
    (define-key m "i" #'denote-link)
    (define-key m "I" #'denote-link-after-creating)
    (define-key m "b" #'denote-backlinks)
    (define-key m "B" #'denote-find-backlink)
    (define-key m "r" #'denote-rename-file)
    (define-key m "k" #'denote-rename-file-keywords)
    (define-key m "e" #'denote-org-extract-org-subtree)
    (define-key m "h" #'my-pks-capture-hub)
    (define-key m "p" #'my-pks-find-project)
    (define-key m "P" #'my-pks-capture-project)
    (define-key m "f" #'my-pks-consult-find)
    (define-key m "g" #'my-pks-consult-grep)
    (define-key m "D" #'my-pks-show-daily-review)
    (define-key m "W" #'my-pks-show-weekly-review)
    (define-key m "L" #'my-pks-capture-literature)
    (define-key m "a" #'my-denote-add-to-agenda)
    (define-key m "A" #'my-denote-remove-from-agenda)
    (define-key m "!" #'my-denote-promote-fleeting-to-permanent)
    (define-key m "^" #'my-denote-promote-to-reference)
    (define-key m "z" #'my-denote-archive-fleeting)
    (define-key m "m" #'my-pks-move-note-to-silo)
    (define-key m (kbd "l l") #'denote-org-dblock-insert-links)
    (define-key m (kbd "l b") #'denote-org-dblock-insert-backlinks)
    (define-key m (kbd "l m") #'denote-org-dblock-insert-missing-links)))

;; ─── Unified PKS dispatch (transient) ─────────────────────────────
;; Single canonical menu surface for capture, digest, and search.
;; Bound globally to C-c c.  Triggered from the OS via
;; sxhkd super+shift+c → emacsclient with a floating "denote" frame.
(require 'transient)

(with-eval-after-load 'denote
  (add-to-list 'denote-templates
               '(fleeting-todo . "* TODO \n\n")
               t)
  (add-to-list 'denote-templates
               '(meeting . "* Attendees\n\n* Agenda\n\n* Notes\n\n* Action items\n  - [ ] \n")
               t))

(defun my-pks-capture-todo ()
  "Create a denote TODO note in ~/pks/fleeting/.
Keywords are (\"agenda\" CONTEXT-TAG?); template is `fleeting-todo'
which inserts a top-level TODO heading after the front matter."
  (interactive)
  (let ((denote-directory (expand-file-name "~/pks/fleeting/"))
        (denote-use-keywords (delq nil (list "agenda" (my-mu4e--context-tag))))
        (denote-use-template 'fleeting-todo))
    (denote)))

(defun my-pks-capture-meeting ()
  "Create a denote meeting note in ~/pks/fleeting/.
Keywords are (\"meeting\" CONTEXT-TAG?); template seeds Attendees /
Agenda / Notes / Action items sections.  Promote to permanent or
extract action items to a project Log later."
  (interactive)
  (let ((denote-directory (expand-file-name "~/pks/fleeting/"))
        (denote-use-keywords (delq nil (list "meeting" (my-mu4e--context-tag))))
        (denote-use-template 'meeting))
    (denote)))

(defun my-pks-capture-mail-followup ()
  "Run the `mf' org-capture template after storing the mu4e link."
  (interactive)
  (org-store-link nil)
  (org-capture nil "mf"))

(defun my-pks--mu4e-context-p ()
  "Non-nil when in a mu4e headers or view buffer."
  (or (derived-mode-p 'mu4e-headers-mode)
      (derived-mode-p 'mu4e-view-mode)))

(defun my-pks--maybe-close-denote-frame ()
  "Delete the current frame if it was opened by sxhkd
(`super+shift+c' or `super+shift+n').  Identified by frame name
\"denote\" or \"org-capture\" set via `emacsclient -F'."
  (when (member (frame-parameter nil 'name) '("denote" "org-capture"))
    (delete-frame)))

(add-hook 'org-capture-after-finalize-hook
          #'my-pks--maybe-close-denote-frame)

(defun pks-dispatch-quit ()
  "Quit the pks-dispatch transient and close the floating frame."
  (interactive)
  (transient-quit-one)
  (my-pks--maybe-close-denote-frame))

(transient-define-prefix pks-dispatch ()
  "Unified PKS capture, digest, and search menu."
  ["PKS dispatch"
   ["Captures"
    ("f" "Fleeting note"      my-pks-capture-fleeting)
    ("l" "Literature note"    my-pks-capture-literature)
    ("P" "New project"        my-pks-capture-project)
    ("h" "Hub / MOC"          my-pks-capture-hub)
    ("t" "TODO → fleeting"    my-pks-capture-todo)
    ("M" "Meeting note"       my-pks-capture-meeting)]
   ["Mail" :if my-pks--mu4e-context-p
    ("e" "Email → fleeting"        my/mu4e-capture-email-task)
    ("F" "Follow-up → fleeting"    my-pks-capture-mail-followup)
    ("a" "Action → project Log"    my/mu4e-capture-action-item)
    ("w" "Waiting → project Log"   my/mu4e-capture-waiting)]
   ["Digests / search"
    ("D" "Daily review"   my-pks-show-daily-review)
    ("W" "Weekly review"  my-pks-show-weekly-review)
    ("g" "Grep PKS"       my-pks-consult-grep)
    ("/" "Find file"      my-pks-consult-find)]]
  [("q" "Quit" pks-dispatch-quit)])

(global-set-key (kbd "C-c c") #'pks-dispatch)

;; OS-side polished entry point.  Creates a small, centered, undecorated
;; floating frame and pops the dispatch inside.  The frame is named
;; "denote" so the existing org-capture-after-finalize-hook closes it
;; on capture finalize.  Auto-close on focus-out and ESC for non-capture
;; actions (digests, search) where the capture hook doesn't fire.
(defun pks-dispatch-floating ()
  "Open a centered floating frame and run `pks-dispatch'.
Used as the desktop entry point (sxhkd super+shift+c).  Called via
`emacsclient -e' from a non-graphical context, so we must explicitly
ask the X server (via $DISPLAY) to host the new frame — otherwise
`make-frame' falls back to the daemon's controlling terminal and
errors with \"Unknown terminal type\"."
  (interactive)
  (let* ((display (or (getenv "DISPLAY") ":0"))
         ;; Generous size: small enough to feel like a popup, large
         ;; enough that the action's flow (denote prompt, org-capture
         ;; buffer split, etc.) has room to work.
         (cols 100) (rows 35)
         (frame (make-frame-on-display
                 display
                 `((name . "denote")
                   (title . "PKS dispatch")
                   (width . ,cols)
                   (height . ,rows)
                   (undecorated . t)
                   (menu-bar-lines . 0)
                   (tool-bar-lines . 0)
                   (vertical-scroll-bars . nil)
                   (horizontal-scroll-bars . nil)
                   (left-fringe . 4)
                   (right-fringe . 4)
                   (internal-border-width . 12))))
         (mon-w (nth 3 (assq 'geometry (frame-monitor-attributes frame))))
         (mon-h (nth 4 (assq 'geometry (frame-monitor-attributes frame))))
         (px-w  (frame-pixel-width frame))
         (px-h  (frame-pixel-height frame))
         (left  (max 0 (/ (- (or mon-w 1920) px-w) 2)))
         (top   (max 0 (/ (- (or mon-h 1080) px-h) 3))))
    (set-frame-position frame left top)
    (select-frame-set-input-focus frame)
    ;; Show a dedicated empty buffer so the floating frame doesn't
    ;; reveal whatever note happened to be current in the daemon.
    (with-selected-frame frame
      (switch-to-buffer (get-buffer-create "*pks-dispatch*"))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "\n  PKS dispatch\n"
                "  ───────────────────────────\n\n"
                "  Captures · Mail · Digests / search\n\n"
                "  q to quit"))
      (setq mode-line-format nil
            cursor-type nil
            buffer-read-only t)
      (goto-char (point-min))
      (pks-dispatch))))

(use-package citar
  :ensure nil
  :after denote
  :custom
  (citar-bibliography '("~/pks/library/references.bib"))
  (citar-library-paths '("~/pks/library/papers/"
                         "~/pks/library/books/"))
  (citar-notes-paths '("~/pks/literature/"))
  (org-cite-global-bibliography '("~/pks/library/references.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar))

(use-package citar-denote
  :ensure nil
  :after citar
  :custom
  (citar-denote-file-type 'org)
  (citar-denote-keyword "lit")        ; literature notes carry _lit
  (citar-denote-subdir nil)           ; use denote-directory itself
  (citar-denote-signature nil)        ; consistent with flat-ZK choice
  :config
  (citar-denote-mode))

;; Citar bindings under the C-c n r sub-prefix.
(with-eval-after-load 'citar
  (define-prefix-command 'my-pks-citar-map)
  (define-key my-pks-prefix-map (kbd "r") 'my-pks-citar-map)
  (let ((m my-pks-citar-map))
    (define-key m "i" #'citar-insert-citation)      ; [cite:@key] in org
    (define-key m "o" #'citar-open)                 ; fuzzy-pick → PDF/note
    (define-key m "f" #'citar-open-files)           ; open associated PDF
    (define-key m "n" #'citar-open-notes)           ; open/create literature note
    (define-key m "r" #'citar-open-entry)           ; jump to .bib entry
    (define-key m "a" #'citar-add-file-to-library))) ; attach PDF to citekey

(use-package pdf-tools
  :ensure nil
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-loader-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t))

(use-package saveplace-pdf-view
  :ensure nil
  :after pdf-tools
  :config
  (save-place-mode 1))

(use-package org-noter
  :ensure nil
  :commands (org-noter)
  :custom
  (org-noter-notes-search-path '("~/pks/literature/"))
  (org-noter-always-create-frame nil)
  (org-noter-doc-split-fraction '(0.6 . 0.4))
  (org-noter-auto-save-last-location t)
  (org-noter-kill-frame-at-session-end nil))

(use-package org-pdftools
  :ensure nil
  :init
  ;; Upstream bug: org-pdftools.el calls bare cl names (`find-if',
  ;; `getf') but only does `(require 'cl-lib)', so the symbols are
  ;; void at runtime in modern Emacs.  `find-if' bites at link
  ;; abbreviation; `getf' bites following a link via C-c C-o
  ;; (`org-pdftools-open-pdftools').  Alias before invocation.
  (unless (fboundp 'find-if) (defalias 'find-if 'cl-find-if))
  (unless (fboundp 'getf)    (defalias 'getf    'cl-getf))
  :hook (org-mode . org-pdftools-setup-link))

;; org-noter-pdftools ships in the same Guix package as org-pdftools.
;; It teaches org-noter to use pdf-tools' precise (page . scroll)
;; locations instead of bare page numbers — without it, M-i writes
;; `:NOTER_PAGE: 1' as an integer and re-reads fail with
;; "wrong type argument: listp, 1".
(use-package org-noter-pdftools
  :ensure nil
  :after (org-noter org-pdftools)
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions
              #'org-noter-pdftools-jump-to-note)))

(use-package dashboard
  :ensure nil
  :config
  (setq dashboard-items '((recents . 5) (bookmarks . 5)))
  (unless (daemonp)
    (dashboard-setup-startup-hook)
    (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))))
  (add-hook 'server-after-make-frame-hook
            (lambda ()
              (unless (active-minibuffer-window)
                (dashboard-open)))))

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
  ((scheme-mode . geiser-mode)
   (scheme-mode . (lambda () (setq-local tab-width 2))))
  :config
  (setq geiser-mode-auto-p t))

(use-package geiser-guile
  :ensure nil
  :config
  (add-to-list 'geiser-guile-load-path "~/src/guix")
  (add-to-list 'geiser-guile-load-path "~/src/nonguix")
  (add-to-list 'geiser-guile-load-path "~/src/guix-systole/systole")
  (add-to-list 'geiser-guile-load-path "~/src/guix-systole/system"))

;; Structural editing and readability
(use-package paredit
  :ensure nil
  :hook (scheme-mode . paredit-mode))

(use-package rainbow-delimiters
  :ensure nil
  :hook (scheme-mode . rainbow-delimiters-mode))

(use-package forge
  :after magit
  :ensure nil
  :config
  ;; Where the credentials live
  (setq auth-sources '("~/.authinfo.gpg"))

  ;; Register the forges you use (GitHub + GitLab examples)
  (setq forge-alist
        '(("github.com" "api.github.com" "github.com" forge-github-repository)
          ("gitlab.com" "gitlab.com/api/v4" "gitlab.com" forge-gitlab-repository)))

  ;; Optional: fetch issues automatically when you `M-x magit-status`
  (setq forge-add-default-bindings t
        forge-database-file (expand-file-name "forge-db.sqlite" user-emacs-directory)))

(require 'pyvenv)
(pyvenv-mode 1)

;;; c-style-vtk.el --- Visualization ToolKit (VTK) Emacs C Style.
;;; Extracted from https://raw.githubusercontent.com/MartinNowak/elisp/6466ef96d228b496c2db8ca898ffe316caf5e765/mine/c-style-vtk.el

;; --- Register the VTK indentation style -----------------------------
(with-eval-after-load 'cc-mode
  (c-add-style
   "vtk"
   '("stroustrup"
     (c-basic-offset          . 2)
     (indent-tabs-mode        . nil)
     (c-comment-only-line-offset . 0)
     (c-electric-pound-behavior . (alignleft))

     (c-offsets-alist
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

(use-package prescient
  :ensure nil
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :ensure nil
  :after (ivy prescient)
  :config
  (ivy-prescient-mode 1)
  (setq ivy-prescient-retain-classic-highlighting t))

;; Company Mode for in-buffer completion (Doom default)
(use-package company
  :ensure nil
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-show-numbers t
        company-tooltip-align-annotations t
        company-tooltip-limit 12
        company-backends '((company-capf company-files company-keywords))
        company-global-modes '(not erc-mode message-mode help-mode)))

;; Company prescient integration
(use-package company-prescient
  :ensure nil
  :after (company prescient)
  :config
  (company-prescient-mode 1))

(use-package hl-todo
  :ensure nil
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF0000")
          ("HACK"   . "#FFA500")
          ("NOTE"   . "#00FF00")
          ("DEPRECATED" . "#808080"))))

;; Smart whitespace cleanup
(use-package ws-butler
  :ensure nil
  :hook (prog-mode . ws-butler-mode))

(use-package envrc
  :ensure nil
  :hook (after-init . envrc-global-mode))

;; ERC - Emacs IRC Client
(use-package erc
  :ensure nil
  :commands (erc erc-tls)
  :config
  ;; Basic settings
  (setq erc-server-coding-system '(utf-8 . utf-8)
        erc-interpret-mirc-color t
        erc-rename-buffers t
        erc-hide-list '("JOIN" "PART" "QUIT")
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477")
        erc-lurker-hide-list '("JOIN" "PART" "QUIT")
        erc-kill-buffer-on-part t
        erc-kill-queries-on-quit t
        erc-kill-server-buffer-on-quit t
        erc-autojoin-timing 'ident
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 22
        erc-timestamp-format "[%H:%M] "
        erc-prompt-for-nickserv-password nil))

  ;; Auto-identify with services using authinfo
  (use-package erc-services
    :ensure nil
    :config
    (erc-services-mode 1)
    (setq erc-prompt-for-nickserv-password nil
          erc-nickserv-passwords nil))

  ;; Track activity
  (use-package erc-track
    :ensure nil
    :config
    (erc-track-mode 1)
    (setq erc-track-visibility nil
          erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                    "324" "329" "332" "333" "353" "477")))

  ;; Notification support
  (use-package erc-match
    :ensure nil
    :config
    (erc-match-mode 1)
    (setq erc-keywords '("your-keywords-here")))

  ;; Logging
  (use-package erc-log
    :ensure nil
    :config
    (setq erc-log-channels-directory "~/.local/share/erc/logs/"
          erc-save-buffer-on-part nil
          erc-save-queries-on-quit nil
          erc-log-write-after-send t
          erc-log-write-after-insert t))

;; Helper function to connect to common IRC networks
(defun my/erc-connect (server port nick)
  "Connect to IRC SERVER on PORT with NICK.
Credentials should be in ~/.authinfo.gpg with format:
machine irc.libera.chat login yournick password yourpass"
  (interactive
   (list
    (read-string "Server: " "irc.libera.chat")
    (read-number "Port: " 6697)
    (read-string "Nick: " user-login-name)))
  (let* ((auth (auth-source-search :host server
                                   :user nick
                                   :require '(:secret)
                                   :max 1))
         (password (when auth
                    (funcall (plist-get (car auth) :secret)))))
    (erc-tls :server server
             :port port
             :nick nick
             :password password)))

;; Keybindings
(my/leader-keys
  "a"   '(:ignore t :which-key "Applications")
  "ai"  '(:ignore t :which-key "IRC")
  "aii" '(my/erc-connect :which-key "Connect to IRC")
  "aiq" '(erc-quit-server :which-key "Quit IRC server")
  "aib" '(erc-switch-to-buffer :which-key "Switch IRC buffer"))
