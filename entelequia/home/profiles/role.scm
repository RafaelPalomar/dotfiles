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
;;; The work ~/.mbsyncrc is available here from mail/mbsyncrc.work (kept OUT of
;;; the blanket dotfiles/ copy, which pushed the work mbsyncrc onto EVERY machine
;;; — including the kids' boxes — the real home/work leak; the email *profile*
;;; was a red herring).  It is installed only under #:manage-mail?, which must
;;; wait until every mail machine is tagged and the blanket dotfiles/.mbsyncrc is
;;; removed in ONE coordinated step (else the role file and the blanket file both
;;; target ~/.mbsyncrc and collide).
;;;
;;; Status: PHASE 1 wires curie+einstein as 'work (role packages + marker; curie
;;; #:work-tailnet? #t) with #:manage-mail? OFF, so the blanket .mbsyncrc still
;;; serves all 7 machines and the change is behaviour-neutral bar the new marker.
;;; The mail cut (#:manage-mail? on everywhere + drop the blanket) and baroja's
;;; flip to 'home are a later coordinated phase.

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

(define* (home-role-services role #:key (work-tailnet? #f) (manage-mail? #f))
  "Role-conditional home services.  The ~/.config/entelequia/role marker is
always installed.  For 'work, two capabilities are OPT-IN (not every work box
wants them, and the mail cut is a coordinated multi-machine step):

  #:work-tailnet?  add the userspace work tailscaled (curie only today).
  #:manage-mail?   install the work ~/.mbsyncrc from role instead of the blanket
                   dotfiles/ copy.  Leave #f until ALL mail machines are tagged
                   and dotfiles/.mbsyncrc is removed, else the role file and the
                   blanket file both target ~/.mbsyncrc and collide."
  (case role
    ((work)
     (append (list (home-role-marker 'work))
             (if work-tailnet? (list (service home-tailscale-work-service-type)) '())
             (if manage-mail?  (list (work-mail-dotfiles)) '())))
    ((home)
     (list (home-role-marker 'home)))
    (else (error "home-role-services: unknown role (want 'work or 'home)" role))))
