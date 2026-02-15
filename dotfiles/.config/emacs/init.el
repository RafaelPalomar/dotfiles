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
  ;; Load doom-dracula theme
  (load-theme 'doom-dracula t)

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
        org-ellipsis "⤵"))

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

(setq org-capture-templates
      '(("t" "TODO workflow")
        ("tt" "Work Todo" entry (file+olp "~/org/inbox.org" "Inbox")
         "* TODO %?\nEntered on %U\n  %i\n\n")
        ("tp" "Personal Todo" entry (file+olp "~/org/inbox-personal.org" "Inbox")
         "* TODO %? :personal:\nEntered on %U\n  %i\n\n")
        ("q" "Quick Capture" entry (file+olp "~/org/inbox.org" "Inbox")
         "* TODO %?\nEntered on %U\n" :immediate-finish t)
        ("p" "Project Idea" entry (file+olp "~/org/projects.org" "Project Ideas")
         "* PROJECT %?\n:PROPERTIES:\n:CREATED: %U\n:STATUS: idea\n:END:\n\n** Goals\n\n** Notes\n\n")
        ("a" "AI-Friendly Task" entry (file+olp "~/org/inbox.org" "Inbox")
         "* TODO %?\n:PROPERTIES:\n:AI_FRIENDLY: t\n:CONTEXT: \n:EXPECTED_OUTPUT: \n:FILES: \n:END:\n\n#+BEGIN_AI_CONTEXT\n\n#+END_AI_CONTEXT\n\n** Acceptance Criteria\n- [ ] \n")
        ("g" "GitHub Issue" entry (file+olp "~/org/github-issues.org" "Open Issues")
         "* TODO %?\n:PROPERTIES:\n:REPO: \n:ISSUE: \n:STATE: \n:GITHUB_URL: \n:LABELS: \n:END:\n\n")
        ("n" "Meeting Note" entry (file+olp "~/org/inbox.org" "Meetings")
         "* Meeting: %?\n:PROPERTIES:\n:CREATED: %U\n:ATTENDEES: \n:END:\n\n** Agenda\n\n** Notes\n\n** Action Items\n")
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
                         "~/org/inbox-personal.org"
                         "~/org/projects.org"
                         "~/org/github-issues.org"
                         "~/org/archive.org"
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

(org-clock-persistence-insinuate)

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
  (org-super-agenda-mode 1)
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

(defun my/create-project ()
  "Create new project from template."
  (interactive)
  (find-file "~/org/projects.org")
  (goto-char (point-max))
  (insert "\n* PROJECT " (read-string "Project name: ") "\n")
  (org-set-property "CREATED" (format-time-string "[%Y-%m-%d %a]"))
  (org-set-property "STATUS" "active")
  (org-set-property "PROJECT_ID" (org-id-new))
  (insert "\n** Goals\n\n** Milestones\n\n*** TODO Backlog\n\n*** TODO In Progress\n\n*** TODO Review\n\n*** TODO Done\n\n** Notes\n\n"))

(defun my/weekly-review ()
  "Open weekly review layout."
  (interactive)
  (delete-other-windows)
  (org-agenda nil "a")
  (org-agenda-week-view)
  (split-window-right)
  (other-window 1)
  (org-clock-report)
  (split-window-below)
  (other-window 1)
  (find-file "~/org/weekly-review.org"))

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
  "w"   '(:ignore t :which-key "Windows")
  "wd"  '(delete-window :which-key "Delete window")
  "wo"  '(delete-other-windows :which-key "Delete other windows")
  "ws"  '(split-window-below :which-key "Split window below")
  "wv"  '(split-window-right :which-key "Split window right")
  "u"   '(universal-argument :which-key "Universal argument")
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
  "pk"  '(my/project-kanban :which-key "Project Kanban")
  "pn"  '(my/create-project :which-key "New project"))

;; Notes and weekly review
(my/leader-keys
  "nw"  '(my/weekly-review :which-key "Weekly review"))

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

;; Org Mode keybindings
(my/leader-keys
  "n"   '(:ignore t :which-key "Notes")
  "nn"  '(org-capture :which-key "Org Capture")
  "na"  '(org-agenda :which-key "Org Agenda")
  "nl"  '(org-store-link :which-key "Store org link")
  "nb"  '(org-switchb :which-key "Switch Org buffer")
  "nc"  '(:ignore t :which-key "Clock")
  "nci" '(org-clock-in :which-key "Clock in")
  "nco" '(org-clock-out :which-key "Clock out")
  "ncr" '(my/org-clock-in-last :which-key "Resume last clock")
  "ncg" '(my/org-clock-goto-current :which-key "Go to current clock")
  "ncR" '(my/org-clock-report-today :which-key "Clock report today"))

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

  ;; Custom directives for common tasks
  ;; TODO: Fix gptel-make-directive - function doesn't exist in current version
  ;; (gptel-make-directive "code-review"
  ;;                       "Review this code for bugs, performance, and best practices.")
  ;;
  ;; (gptel-make-directive "explain"
  ;;                       "Explain this code in simple terms.")
  )

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

;; Keybindings
(my/leader-keys
  "o"    '(:ignore t :which-key "AI models")
  "oa"   '(gptel-abort :which-key "Abort gptel invocation")
  "ob"   '(my/gptel-switch-backend :which-key "Switch AI backend")
  "oc"   '(:ignore t :which-key "AI context")
  "ocb"  '(gptel-add :which-key "Add/Remove buffer to AI context")
  "occ"  '(my/ai-continue-code :which-key "Continue this")
  "ocf"  '(gptel-context-add-file :which-key "Add file to AI context")
  "ocr"  '(gptel-context-remove-all :which-key "Remove all AI context")
  "of"   '(my/ai-fix-error :which-key "Fix this error")
  "og"   '(gptel :which-key "Invoke gptel")
  "oi"   '(gptel-send :which-key "Send to AI")
  "om"   '(gptel-menu :which-key "gptel-menu")
  "or"   '(gptel-rewrite :which-key "AI model rewrite")
  "ov"   '(aider-transient-menu :which-key "Vibe code w/ Aider"))

;; Org task AI keybindings
(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 :prefix "SPC o t"
 "x" 'my/org-task-to-ai-context
 "b" 'my/ai-break-down-task
 "p" 'my/ai-project-plan)

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

(use-package mu4e
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :commands (mu4e)
  :defer t
  :init
  ;; General mu4e settings that need to be set before mu4e loads
  (setq mu4e-maildir "~/.local/share/mail"
        mu4e-attachment-dir "~/Downloads"
        mu4e-get-mail-command  "guix shell -L ~/.dotfiles cyrus-sasl-xoauth2 -- mbsync -a"
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

  :custom
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
    (kbd "l") 'my/dired-open-file              ; Open file/directory (with external apps)
    (kbd "h") 'dired-up-directory              ; Go to parent
    (kbd "q") 'dirvish-quit                    ; Quit
    (kbd "RET") 'my/dired-open-file            ; Also bind RET for consistency
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
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))))

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
  "aic" '(my/erc-connect :which-key "Connect to IRC")
  "aiq" '(erc-quit-server :which-key "Quit IRC server")
  "aib" '(erc-switch-to-buffer :which-key "Switch IRC buffer"))

;; GitHub Copilot - best for vibe coding
(use-package copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion))
  :config
  ;; Set default indentation offset
  (setq copilot-indent-offset-alist
        '((prog-mode . 2)
          (c-mode . 2)
          (c++-mode . 2)
          (python-mode . 4)
          (emacs-lisp-mode . 2)
          (scheme-mode . 2)
          (org-mode . 2)))

  ;; Suppress the warning
  (setq warning-suppress-types '((copilot copilot-no-mode-indent))))

(use-package aider
  :config
  (setq aider-program (expand-file-name "~/.local/bin/aider-wrapper.sh"))

  (setq aider-args
        (list "--model" "claude-sonnet-4-5-20250929"
              "--no-auto-commits"
              "--no-stream"))  ;; Disable streaming to prevent repetition

  ;; Better buffer configuration
  (setq aider-buffer-window-setup 'split-window-below)

  ;; Clean buffer before new output
  (defun my/aider-clean-buffer ()
    "Clean aider buffer before running new commands."
    (when-let ((buf (get-buffer "*aider*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)))))

  (advice-add 'aider :before #'my/aider-clean-buffer)

  ;; Better terminal handling
  (setq comint-scroll-to-bottom-on-input t
        comint-scroll-to-bottom-on-output t
        comint-move-point-for-output t))

(defun my/aider-setup-api-key ()
  "Set Anthropic API key before running aider."
  (unless (getenv "ANTHROPIC_API_KEY")
    (setenv "ANTHROPIC_API_KEY"
            (auth-source-pick-first-password :host "api.anthropic.com" :user "aider"))))

(advice-add 'aider-transient-menu :before #'my/aider-setup-api-key)
(advice-add 'aider :before #'my/aider-setup-api-key)

(defun my/ai-commit-message ()
  "Generate a commit message from staged changes."
  (interactive)
  (let* ((diff (shell-command-to-string "git diff --cached"))
         (prompt (format "Write a concise commit message (conventional commits format) for:\n\n%s" diff)))
    (gptel-request
        prompt
      :callback
      (lambda (response info)
        (when response
          (kill-new (string-trim response))
          (message "Commit message copied: %s" (string-trim response)))))))
