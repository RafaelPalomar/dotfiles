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


    ;;; ----- Evil Goggles -----

(evil-goggles-mode)

;; optionally use diff-mode's faces; as a result, deleted text
;; will be highlighed with `diff-removed` face which is typically
;; some red color (as defined by the color theme)
;; other faces such as `diff-added` will be used for other actions
(evil-goggles-use-diff-faces)

(require 'nano-theme)
  (setq nano-fonts-use t) ; Use theme font stack
  (nano-dark)             ; Use theme dark version
  (nano-mode)             ; Recommended settings


  (defun my/set-face (face style)
    "Reset FACE and make it inherit STYLE."
    (set-face-attribute face nil
                        :foreground 'unspecified :background 'unspecified
                        :family     'unspecified :slant      'unspecified
                        :weight     'unspecified :height     'unspecified
                        :underline  'unspecified :overline   'unspecified
                        :box        'unspecified :inherit    style))
  (my/set-face 'italic 'nano-faded)

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
                      :weight 'normal))

  ;; Make frames transparent
  (set-frame-parameter (selected-frame) 'alpha-background 95)
  (add-to-list 'default-frame-alist '(alpha-background . 95))
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

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

;; From https://emacs.stackexchange.com/questions/72572/how-to-see-color-output-when-compiling
;; this will help the compilation buffer to interpret correcly color characters as color, instead of
;; characters
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; enable org-modern globally
(with-eval-after-load 'org (global-org-modern-mode))

  ;;; ----- Essential Org Mode Configuration -----

  (setq org-startup-folded 'content
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

  ;; Follow zotero links
  ;; https://plexwave.org/blog/org-zotero-links
  ;; Pay attention to the requirements in terms of zotero, plugins, action scripts and manual modification to the action script
  (defun my-org-zotero-open (path _)
    (call-process "open" nil nil nil (concat "zotero:" path)))

  (org-link-set-parameters "zotero" :follow #'my-org-zotero-open)

(setq org-agenda-files
      '("~/org"))

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(setq org-capture-templates
      '(("m" "Email Workflow")
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

;;; ------------ Projectile setup --------------

;; https://github.com/bbatsov/projectile/issues/1649
;; This will allow remembering of remote projects
(require 'tramp)

(projectile-mode +1)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;;; ----- Guile Geiser setup -----

(require 'geiser-guile)

(with-eval-after-load 'geiser-guile
  (add-to-list 'geiser-guile-load-path "~/src/guix/guix"))

    ;;; ----- Paredit -----

(require 'paredit)

;; Make evil play nicely with paredit
(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)

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
  ;;"l" '(ace-window :which-key "ace-window")

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
  "pC" '(projectile-configure-project :which-key "projectile-configure-project")
  "pc" '(projectile-compile-project :which-key "projectile-compile-project")
  "pi" '(projectile-invalidate-cache :which-key "projectile-invalidate-cache")
  "pa" '(projectile-add-known-project :which-key "projectile-add-known-project")


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
  ;; "N" '(:ignore t :which-key "Narrow")
  ;; "Nr" '(narrow-to-region :which-key "narrow-to-region")
  ;; "Nw" '(widen :which-key "widen")


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

  ;; Denote
  "nn" '(denote-open-or-create :which-key "denote-open-or-create")

  ;; Workspaces (persp-mode commands) under "SPC l"
  "l"   '(:ignore t :which-key "Workspace")

  ;; Workspace Switching
  "ll"  '(persp-switch :which-key "Switch Workspace")
  "ln"  '(persp-next :which-key "Next Workspace")
  "lp"  '(persp-prev :which-key "Previous Workspace")
  "lr"  '(persp-rename :which-key "Rename Workspace")
  "lk"  '(persp-kill :which-key "Kill Workspace")
  "lc"  '(persp-copy :which-key "Copy Workspace")

  ;; Buffer Management within Workspaces
  "la"  '(persp-add-buffer :which-key "Add Buffer to Workspace")
  "lA"  '(persp-remove-buffer :which-key "Remove Buffer from Workspace")
  "lb"  '(persp-switch-to-buffer :which-key "Switch Buffer in Workspace")
  "li"  '(persp-import :which-key "Import Buffer to Workspace")

  ;; Workspace Persistence
  "ls"  '(persp-save-state-to-file :which-key "Save Workspaces to File")
  "lo"  '(persp-load-state-from-file :which-key "Load Workspaces from File")
  "ld"  '(persp-remove-some :which-key "Remove Some Workspaces")

  ;; Tabs under "SPC TAB"
  "TAB" '(:ignore t :which-key "Tabs")
  "TAB c" '(tab-bar-new-tab :which-key "New Tab")
  "TAB k" '(tab-bar-close-tab :which-key "Close Tab")
  "TAB n" '(tab-bar-switch-to-next-tab :which-key "Next Tab")
  "TAB p" '(tab-bar-switch-to-prev-tab :which-key "Previous Tab")
  "TAB r" '(tab-bar-rename-tab :which-key "Rename Tab")
  )

;;; ----------- mu4e general configuration --------------

(require 'mu4e)  ;; Ensure mu4e is loaded

(with-eval-after-load 'mu4e
  ;; Option 1: Unbind C--
  (define-key mu4e-headers-mode-map (kbd "C--") nil)
  (define-key mu4e-view-mode-map (kbd "C--") nil)

  ;; Option 2: Rebind C-- to text-scale-decrease
  ;; (define-key mu4e-headers-mode-map (kbd "C--") 'text-scale-decrease)
  ;; (define-key mu4e-view-mode-map (kbd "C--") 'text-scale-decrease)
  )

(setq mu4e-maildir "~/.local/share/mail"
      ;;mu4e-mu-binary "/usr/local/bin/mu"
      mu4e-attachment-dir "~/Downloads"
      mu4e-get-mail-command  "guix shell -L ~/dotfiles cyrus-sasl-xoauth2 -- mbsync -a"
      mu4e-update-interval 300            ; Update interval (seconds)
      mu4e-index-cleanup t                ; Cleanup after indexing
      mu4e-index-update-error-warning t   ; Warnings during update
      mu4e-hide-index-messages t          ; Hide indexing messages
      mu4e-index-update-in-background t   ; Background update
      mu4e-change-filenames-when-moving t ; Needed for mbsync
      mu4e-index-lazy-check nil           ; Don't be lazy, index everything

      mu4e-confirm-quit nil
      mu4e-split-view 'single-window

      mu4e-headers-auto-update nil
      mu4e-headers-date-format "%d-%m"
      mu4e-headers-time-format "%H:%M"
      mu4e-headers-from-or-to-prefix '("" . "To ")
      mu4e-headers-include-related t
      mu4e-headers-skip-duplicates t)

(setq sendmail-program "msmtp"
      send-mail-function 'smtpmail-send-it
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-send-mail-function 'message-send-mail-with-sendmail)

;; Reset variables, as our configurtion is based on contexts
(setq mu4e-contexts nil
      mu4e-drafts-folder nil
      mu4e-compose-reply-to-address nil
      mu4e-compose-signature t
      mu4e-compose-signature-auto-include t
      mu4e-sent-folder nil
      mu4e-trash-folder nil)

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
                (smtpmail-stream-type   . starttls)
                ))

       ;; UIO Account
       (make-mu4e-context
        :name "UIO"
        :match-func
        (lambda (msg)
          (when msg
            (string-prefix-p "/rafaelpa@uio.no" (mu4e-message-field msg :maildir))))
        :vars '((user-mail-address      . "rafaelpa@uio.no")
                (user-full-name         . "Rafael Palomar")
                (mu4e-drafts-folder     . "/rafaelpa@uio.no/Drafts")
                (mu4e-sent-folder       . "/rafaelpa@uio.no/Sent")
                (mu4e-trash-folder      . "/rafaelpa@uio.no/Trash")
                (mu4e-refile-folder     . "/rafaelpa@uio.no/Archive")
                ;; Configure SMTP
                (smtpmail-smtp-user     . "rafaelpa@uio.no")
                (smtpmail-smtp-server   . "smtp.office365.com")
                (smtpmail-smtp-service  . 587)
                (smtpmail-stream-type   . starttls)

                ;; Gmail Account
                ;; (make-mu4e-context
                ;;  :name "Gmail"
                ;;  :match-func
                ;;  (lambda (msg)
                ;;    (when msg
                ;;      (string-prefix-p "/rafaelpalomaravalos@gmail.com" (mu4e-message-field msg :maildir))))
                ;;  :vars '((user-mail-address      . "rafaelpalomaravalos@gmail.com")
                ;;          (user-full-name         . "Your Name")  ;; Replace with your name
                ;;          (mu4e-drafts-folder     . "/rafaelpalomaravalos@gmail.com/[Gmail]/Drafts")
                ;;          (mu4e-sent-folder       . "/rafaelpalomaravalos@gmail.com/[Gmail]/Sent Mail")
                ;;          (mu4e-trash-folder      . "/rafaelpalomaravalos@gmail.com/[Gmail]/Trash")
                ;;          (mu4e-refile-folder     . "/rafaelpalomaravalos@gmail.com/[Gmail]/All Mail")
                ;;          ;; Configure SMTP
                ;;          (smtpmail-smtp-user     . "rafaelpalomaravalos@gmail.com")
                ;;          (smtpmail-smtp-server   . "smtp.gmail.com")
                ;;          (smtpmail-smtp-service  . 587)
                ;;          (smtpmail-stream-type   . starttls)
                ;;          ;; Signature
                ;;          (mu4e-compose-signature . "Best regards,\nYour Name")))
                ;; Update your signature
                ))))

;; Fixing keybindings and other configurations
(with-eval-after-load 'mu4e
  ;; Unbind conflicting keys if necessary
  (define-key mu4e-headers-mode-map (kbd "C--") nil)
  (define-key mu4e-view-mode-map (kbd "C--") nil)

  ;; Ensure 'a' is available in visual state in mu4e-view-mode
  (evil-define-key 'visual mu4e-view-mode-map (kbd "a") 'mu4e-view-action)
  ;; Similarly, for mu4e-headers-mode if needed
  (evil-define-key 'visual mu4e-headers-mode-map (kbd "a") 'mu4e-headers-mark-for-*)

  ;; Additional mu4e configurations can go here
  )

(require 'mu4e-dashboard)
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
    (font-lock-flush (point-min) (point-max))))

;; Define quick action capture
              ;; https://systemcrafters.net/emacs-mail/email-workflow-with-org-mode/

  ;; Helper function to get the current message
  (defun efs/get-current-message ()
    "Get the current message in mu4e, whether in view or headers mode."
    (cond
     ((eq major-mode 'mu4e-view-mode)
      mu4e~view-message)
     ((eq major-mode 'mu4e-headers-mode)
      (mu4e-message-at-point))
     (t
      (mu4e-message-at-point))))


  (defun efs/capture-mail-follow-up (msg)
    "Create a follow-up task for the email message MSG."
    (interactive (list (or msg (mu4e-message-at-point))))
    (unless msg
      (error "No message found."))
    ;; Extract message details
    (let* ((from (mu4e-message-field msg :from))
           (fromname (or (cdr (car from)) (car (car from)) "[No Name]"))
           (subject (mu4e-message-field msg :subject))
           (message-id (mu4e-message-field msg :message-id))
           (link (concat "mu4e:msgid:" message-id))
           (region (when (use-region-p)
                     (buffer-substring-no-properties
                      (region-beginning) (region-end)))))
      ;; Set the org-capture variables
      (setq org-store-link-plist (list
                                  :type "mu4e"
                                  :fromname fromname
                                  :subject subject
                                  :message-id message-id
                                  :link link))
      (setq org-capture-initial region)
      ;; Mark the message as read
      (cond
       ((eq major-mode 'mu4e-view-mode)
        (mu4e-view-mark-for-read))
       ((eq major-mode 'mu4e-headers-mode)
        (mu4e-headers-mark-for-read)
        (mu4e-mark-execute-all t)))
      ;; Start the capture
      (org-capture nil "mf")))


(defun efs/capture-mail-reply (msg)
  "Create a reply task for the email message MSG."
  (interactive (list (or msg (mu4e-message-at-point))))
  (unless msg
    (error "No message found."))
  ;; Extract message details
  (let* ((from (mu4e-message-field msg :from))
         (fromname (or (cdr (car from)) (car (car from)) "[No Name]"))
         (subject (mu4e-message-field msg :subject))
         (message-id (mu4e-message-field msg :message-id))
         (link (concat "mu4e:msgid:" message-id))
         (region (when (use-region-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end)))))
    ;; Set the org-capture variables
    (setq org-store-link-plist (list
                                :type "mu4e"
                                :fromname fromname
                                :subject subject
                                :message-id message-id
                                :link link))
    (setq org-capture-initial region)
    ;; Mark the message as read
    (cond
     ((eq major-mode 'mu4e-view-mode)
      (mu4e-view-mark-for-read))
     ((eq major-mode 'mu4e-headers-mode)
      (mu4e-headers-mark-for-read)
      (mu4e-mark-execute-all t)))
    ;; Start the capture
    (org-capture nil "mr")))

              ;; Add custom actions for our capture templates
              (add-to-list 'mu4e-headers-actions
                           '("follow up" . efs/capture-mail-follow-up) t)
              (add-to-list 'mu4e-view-actions
                           '("follow up" . efs/capture-mail-follow-up) t)
              (add-to-list 'mu4e-headers-actions
                           '("reply" . efs/capture-mail-reply) t)
              (add-to-list 'mu4e-view-actions
                           '("reply" . efs/capture-mail-reply) t)

;;; ----------- AVY --------------

(require 'avy)

(setq magit-status-buffer-switch-function 'switch-to-buffer)

(require 'beacon)

(beacon-mode 1)

(setq persp-keymap-prefix (kbd "C-c M-p"))
(setq persp-auto-save-opt 0)
(persp-mode 1)

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

;;; --------- gptel -------
;; This is a workaround https://github.com/karthink/gptel/issues/342
(setq gptel-use-curl nil)
(setq gptel-default-mode 'org-mode)

;;; --------- tramp -------

(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;;; --------- dired -------
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;;; ----Enable dired-find-alternate-file------
(put 'dired-find-alternate-file 'disabled nil)

;;; --------- enable marginalia -------
(marginalia-mode)

;;; ----------- Denote Configuration -----------

;; Ensure Denote is loaded
(require 'denote)

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
    (setq org-agenda-files (delete file org-agenda-files))))

;;; ------------------- Emacs Dashboard ---------------

(require 'dashboard)
(dashboard-setup-startup-hook)
