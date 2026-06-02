(define-module (entelequia home profiles role)
  #:use-module (entelequia home profiles email)
  #:use-module (entelequia home services tailscale-work)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:export (home-role-packages
            home-role-services))

;;; Home/work role profile  (ADR-0005)
;;;
;;; Encodes the home-vs-work domain as a single per-machine decision so the
;;; separation is STRUCTURAL, not policy.  A machine declares its role and
;;; gets exactly the matching packages + services:
;;;
;;;   'work  -> the work email stack (isync/msmtp/notmuch/OAuth) + ~/.mbsyncrc
;;;             (the two O365 accounts) + the userspace work tailscaled.
;;;   'home  -> NONE of the above.  Personal mail (rafael@palomar.no, Tuta)
;;;             has no IMAP/SMTP/bridge, so it is human-only (browser); a home
;;;             box therefore ships no mail stack, no ~/.mbsyncrc, and no work
;;;             tailnet at all.  Separation by removal, not duplication.
;;;
;;; Both roles drop a ~/.config/entelequia/role marker so userspace tooling
;;; (mail-draft, pks helpers) can branch on the domain.
;;;
;;; The work ~/.mbsyncrc is installed here from mail/mbsyncrc.work, which is
;;; kept OUT of the blanket dotfiles/ copy (home-dotfiles-service-type) — that
;;; copy pushed the work mbsyncrc onto EVERY machine, which was the real
;;; home/work leak (the email *profile* was a red herring).  A 'work machine
;;; gets it via role; a 'home machine never does.
;;;
;;; PHASE 0 status: this module is DEFINED but not yet imported by any machine,
;;; so it is behaviour-neutral.  WIRING it into curie/einstein (role 'work) and
;;; baroja (role 'home) — and removing the now-redundant dotfiles/.mbsyncrc from
;;; the blanket copy at the same time — is Phase 1+3.

(define (home-role-marker role)
  "A ~/.config/entelequia/role marker file holding the role name."
  (simple-service
   'entelequia-home-role-marker
   home-xdg-configuration-files-service-type
   (list (list "entelequia/role"
               (plain-file "entelequia-role"
                           (string-append (symbol->string role) "\n"))))))

(define (work-mail-dotfiles)
  "Install the work ~/.mbsyncrc (kept out of the blanket dotfiles/ copy)."
  (simple-service
   'entelequia-work-mail-dotfiles
   home-files-service-type
   (list (list ".mbsyncrc" (local-file "mail/mbsyncrc.work")))))

(define (home-role-packages role)
  "Role-conditional home packages.  'work pulls the email stack; 'home none."
  (case role
    ((work) email-home-packages)
    ((home) '())
    (else (error "home-role-packages: unknown role (want 'work or 'home)" role))))

(define (home-role-services role)
  "Role-conditional home services: the role marker always, plus — for 'work —
the work userspace tailscaled and the work ~/.mbsyncrc."
  (case role
    ((work)
     (list (home-role-marker 'work)
           (service home-tailscale-work-service-type)
           (work-mail-dotfiles)))
    ((home)
     (list (home-role-marker 'home)))
    (else (error "home-role-services: unknown role (want 'work or 'home)" role))))
