;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; allow loading of lisp code from .doom.d
(add-to-list 'load-path "~/.config/doom")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Mermaid configuration
(setq! ob-mermaid-cli-path "/usr/local/bin/mmdc")
(org-babel-do-load-languages
    'org-babel-load-languages
    '((mermaid . t)
      (scheme . t)
      (your-other-langs . t)))

;; Set TRAMP docker to "podman"
(setq! tramp-docker-program "podman")

;; Use the gptel package with some custom configuration
;; This setup requires the file ~/.authinfo.gpg in the following form:
;; machine gptel-api-key login <login_email> password <openai_key>
(use-package! gptel
  :config
  (defun read-gptel-api-key-from-auth-source ()
    "Retrieve the GPTEL API key from auth-source."
    (let ((credentials (auth-source-search :max 1
                                           :host "gptel-api-key"
                                           :require '(:user :secret))))
      (if credentials
          (let ((key (plist-get (nth 0 credentials) :secret)))
            (if (functionp key)
                (funcall key)
              key))
        (error "API key for gptel not found"))))
  (setq! gptel-api-key #'read-gptel-api-key-from-auth-source)
  (setq! gptel-default-mode 'org-mode))

;; This is inspired by hlissner/doom.d configuration https://github.com/hlissner/.doom.d/
;; FIX format gives a warning on spelling (it seems to work ok, though)
;; (after! org-roam
;;   (setq org-roam-capture-templates
;;         `(("s" "slipbox" plain
;;            ,(format "#+title: ${title}\n" org-roam-directory)
;;            :target (file "slipbox/%<%Y%m%d%H%M%S>-${slug}.org")
;;            :unnarrowed t)
;;           ("m" "main" plain
;;            ,(format "#+title: ${title}\n" org-roam-directory)
;;            :target (file "main/%<%Y%m%d%H%M%S>-${slug}.org")
;;            :unnarrowed t)
;;           ("a" "article" plain
;;            ,(format "#+title: ${title}\n" org-roam-directory)
;;            :target (file "article/%<%Y%m%d%H%M%S>-${slug}.org")
;;            :unnarrowed t)
;;           ("g" "AI-powered" plain
;;            ,(format "#+title: ${title}\n" org-roam-directory)
;;            :target (file "ai-notes/%<%Y%m%d%H%M%S>-${slug}.org")
;;            :unnarrowed t
;;            :hook (lambda () (gptel-mode 1))))
;;         org-roam-dailies-capture-templates
;;         `(("d" "default" plain ""
;;            :target (file+head "%<%Y-%m-%d>.org" ,(format "" org-roam-directory))))))


(after! org-roam
  :ensure t
  :custom
  (setq org-roam-directory (file-truename "~/Dropbox/org"))
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n"))))
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))
  (setq org-roam-capture-templates
        '(
          ("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n<t")
           :unnarrowed t)
          ("m" "meeting" plain "%?"
           :target (file+head "meetings/%<%Y%m%d%H%M%S>-${slug}.org"
                              ":PROPERTIES:\n:project: fill\n:people: fill\n:END:\n#+title: ${title} %<%Y-%m-%d>\n#+filetags:")
           :unnarrowed t)
          ("t" "main" plain "%?"
           :target (file+head "main/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags:")
           :unnarrowed t)
          ("a" "article" plain "%?"
           :target (file+head "articles/${slug}.org"
                              "#+title: ${title}\n#+filetags: articles\n#+AUTHOR: Rafael Palomar\n#+DATE: %<%Y-%m-%d>\n#+DESCRIPTION: description")
           :unnarrowed t)
          ))
  :config
  (org-roam-db-autosync-enable))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; Use `citar' with `org-cite'
;;

(after! oc
  (setq org-cite-global-bibliography '("~/Dropbox/org/references/references.bib")))

(use-package! citar
  :after oc
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography '("~/Dropbox/org/references/references.bib"))
  (citar-org-roam-note-title-template "${author} - ${title}\npdf: ${file}")
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))


;; The following fragment is inspired on
;; https://www.riccardopinosio.com/blog/posts/zotero_notes_article.html
;; for a workflow based on Zotero and org-roam for scientific note taking
;; LINK ZOTERO NOTES TO ORG-ROAM VIA CITAR

(load "org-roam-zotero-notes")

(add-hook 'org-mode-hook
          '(lambda ()
             (setq org-file-apps
                   '((auto-mode . emacs)
                     ("\\.mm\\'" . default)
                     ("\\.x?html?\\'" . default)
                     ("\\.pdf\\'" . "evince %s")))))

;; Configure denote with citar
(setq denote-directory "~/Dropbox/Notes/Work")
(require 'denote-silo-extras)
'(denote-silo-extras-directories '("~/Dropbox/Notes/Personal"))
(add-hook 'after-init-hook #'citar-denote-mode)

