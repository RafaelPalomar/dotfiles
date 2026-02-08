(define-module (entelequia home profiles email)
  #:use-module (entelequia packages cyrus-sasl-xoauth2)
  #:use-module (entelequia packages mutt-oauth2)
  #:use-module (gnu packages)
  #:use-module (gnu packages mail)
  #:export (email-home-packages))

;;; Email home profile
;;;
;;; Email stack with OAuth2 support for institutional accounts.
;;; Includes isync (mbsync), msmtp, mu (maildir utils), and OAuth2 tools.

(define email-home-packages
  (list cyrus-sasl-xoauth2
        mutt-oauth2
        isync
        msmtp
        mu))
