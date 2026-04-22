(define-module (entelequia home profiles email)
  #:use-module (entelequia packages cyrus-sasl-xoauth2)
  #:use-module (entelequia packages mutt-oauth2)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages mail)
  #:export (email-home-packages))

;;; Email home profile
;;;
;;; Email stack with OAuth2 support for institutional accounts.
;;; Includes:
;;;   - isync (mbsync)         Maildir sync against IMAP (O365, Gmail)
;;;   - msmtp                  Outbound SMTP
;;;   - cyrus-sasl-xoauth2     OAuth2 SASL plugin
;;;   - mutt-oauth2            Token refresh helper (neomutt script)
;;;   - notmuch + emacs-notmuch + emacs-consult-notmuch
;;;       Machine-queryable mail layer alongside mu4e: notmuch's
;;;       `--format=json` output is the interface agents / scripts use.
;;;       mu4e remains the primary human UI.

(define email-home-packages
  (list cyrus-sasl-xoauth2
        mutt-oauth2
        isync
        msmtp
        notmuch
        emacs-notmuch
        emacs-consult-notmuch))
