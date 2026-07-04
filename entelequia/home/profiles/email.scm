(define-module (entelequia home profiles email)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (entelequia packages mutt-oauth2)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages python-xyz)
  #:export (email-home-packages))

;;; Email home profile
;;;
;;; Email stack with OAuth2 support for institutional accounts.
;;; Includes:
;;;   - isync (mbsync)         Maildir sync against IMAP (O365, Gmail)
;;;   - msmtp                  Outbound SMTP
;;;   - cyrus-sasl-xoauth2     OAuth2 SASL plugin (upstream Guix package)
;;;   - cyrus-sasl             In the profile ONLY to activate its SASL_PATH
;;;                            search path: search paths are exported by the
;;;                            declaring package, and upstream
;;;                            cyrus-sasl-xoauth2 does not declare one (our
;;;                            old local recipe did).  Without this, mbsync /
;;;                            msmtp never find the xoauth2 plugin.
;;;   - mutt-oauth2            Token refresh helper (neomutt script)
;;;   - notmuch + emacs-notmuch + emacs-consult-notmuch
;;;       Machine-queryable mail layer alongside mu4e: notmuch's
;;;       `--format=json` output is the interface agents / scripts use.
;;;       mu4e remains the primary human UI.
;;;   - python-icalendar       iCal parser used by `mail-calendar` to
;;;                            extract DTSTART / RRULE / ATTENDEE / UID
;;;                            from text/calendar parts of invite mails.

(define email-home-packages
  (list cyrus-sasl
        cyrus-sasl-xoauth2
        mutt-oauth2
        isync
        msmtp
        notmuch
        emacs-notmuch
        emacs-consult-notmuch
        python-icalendar))
