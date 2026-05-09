(define-module (entelequia home services librewolf-kids)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (librewolf-kids-home-service))

;;; Per-user librewolf default-search override for kids
;;;
;;; The system-wide policy at /etc/librewolf/policies/policies.json
;;; (entelequia/system/lib/librewolf-policy.scm) installs *both* SearXNG
;;; engines on every machine but can only declare ONE Default — the
;;; sensible adult choice for a multi-user box.  Librewolf, like Firefox,
;;; has no per-user policy mechanism.  This home service fills that gap
;;; for kid users (leandro@alucard, adrian@hopper) by dropping a `user.js'
;;; with `browser.search.defaultenginename' set to "SearXNG (Kids)".
;;;
;;; user.js semantics
;;; -----------------
;;; Mozilla applies `user.js' on every browser startup, copying its values
;;; into `prefs.js'.  Manual changes via about:config are reverted on the
;;; next launch — these are *enforced* defaults, not seed values.
;;;
;;; For the *active* engine, modern Librewolf stores the choice in
;;; `search.json.mozlz4' and consults `browser.search.defaultenginename'
;;; only when initialising a fresh profile (or after that file is wiped).
;;; In other words: the very first launch of a clean profile picks up our
;;; pref; subsequent launches keep whatever the user last selected from
;;; the search UI.  That's the Mozilla-supported behaviour and it's
;;; acceptable here — kids basically never edit search engines.
;;;
;;; Bootstrap behaviour
;;; -------------------
;;; We mkdir ~/.librewolf if missing, write a minimal `profiles.ini'
;;; declaring a relative `default' profile, and seed `user.js' inside it.
;;; If the user already has profiles (existing librewolf install), we
;;; additionally drop `user.js' into every profile-looking directory we
;;; find — so old profiles get the override too.  We never overwrite an
;;; existing `profiles.ini' (preserves the user's profile metadata).

(define %kids-user-js
  "// MANAGED FILE — written by entelequia librewolf-kids home service.
// Direct edits here are reverted on every `home-reconfigure'.
// To change kid defaults, edit
//   entelequia/home/services/librewolf-kids.scm
// and run `home-reconfigure'.
//
// Force the default search engine to the kid SearXNG variant.
// The engine itself is added system-wide via SearchEngines.Add in
//   /etc/librewolf/policies/policies.json
// — see entelequia/system/lib/librewolf-policy.scm.
user_pref(\"browser.search.defaultenginename\",     \"SearXNG (Kids)\");
user_pref(\"browser.search.defaultenginename.US\",  \"SearXNG (Kids)\");
user_pref(\"browser.urlbar.placeholderName\",       \"SearXNG (Kids)\");
")

(define %kids-profiles-ini
  ;; [InstallXXXX] hashes are normally computed from the install path; an
  ;; arbitrary value here is silently ignored — Librewolf falls back to
  ;; whichever [ProfileN] is marked Default=1, which is what we want.
  "[Profile0]
Name=default
IsRelative=1
Path=default
Default=1

[General]
StartWithLastProfile=1
Version=2
")

(define librewolf-kids-home-service
  (simple-service
   'librewolf-kids-prefs
   home-activation-service-type
   #~(begin
       (use-modules (ice-9 ftw)
                    (srfi srfi-1))
       (let* ((home (getenv "HOME"))
              (lw-dir (string-append home "/.librewolf"))
              (managed-name "default")
              (managed-dir (string-append lw-dir "/" managed-name))
              (profiles-ini (string-append lw-dir "/profiles.ini"))
              (user-js-content #$%kids-user-js))
         (define (write-user-js dir)
           (let ((path (string-append dir "/user.js")))
             (call-with-output-file path
               (lambda (port) (display user-js-content port)))))
         (define (looks-like-profile? dir)
           ;; A directory under ~/.librewolf is treated as a profile if it
           ;; contains any of the well-known Mozilla profile artefacts, or
           ;; if it's the managed dir we just bootstrapped.
           (or (string=? (basename dir) managed-name)
               (file-exists? (string-append dir "/prefs.js"))
               (file-exists? (string-append dir "/times.json"))
               (file-exists? (string-append dir "/extensions"))))
         (mkdir-p lw-dir)
         ;; Bootstrap profiles.ini + managed profile if absent.  Existing
         ;; profiles.ini is preserved so we don't shadow the user's other
         ;; profiles.
         (unless (file-exists? profiles-ini)
           (mkdir-p managed-dir)
           (call-with-output-file profiles-ini
             (lambda (port) (display #$%kids-profiles-ini port))))
         (mkdir-p managed-dir)
         ;; Drop user.js into the managed profile and into every existing
         ;; profile-shaped sibling so re-runs are idempotent.
         (let* ((entries (or (scandir lw-dir
                                      (lambda (e)
                                        (and (not (member e '("." "..")))
                                             (not (string=? e "Crash Reports")))))
                             '())))
           (for-each
            (lambda (name)
              (let ((path (string-append lw-dir "/" name)))
                (when (and (file-exists? path)
                           (eq? (stat:type (stat path)) 'directory)
                           (looks-like-profile? path))
                  (write-user-js path))))
            entries))))))
