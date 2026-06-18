(define-module (entelequia system lib librewolf-policy)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (make-librewolf-policy-service
            librewolf-policy-service))

;;; Librewolf managed-policy service
;;;
;;; Drops a JSON file at /etc/librewolf/policies/policies.json.  Verified via
;;; strace that the Guix `librewolf-150.0.1' build *does* probe this path
;;; (the lookup is via stat, which is why an openat-only strace misses it).
;;; Mozilla's policy code merges this file with the bundled
;;; `<install>/lib/librewolf/distribution/policies.json' (the upstream
;;; Librewolf hardening defaults — DisableTelemetry, NoDefaultBookmarks,
;;; uBlock0 normal_installed, etc.).  Keys we set here add to or override
;;; that base; we do NOT have to re-state Librewolf's hardening.
;;;
;;; What this policy adds
;;; ---------------------
;;; - Bitwarden via ExtensionSettings (force_installed) — Mozilla's AMO is
;;;   reachable from Librewolf so the install_url XPI download works
;;;   (unlike chromium, where we had to fall back to a manual drag-drop).
;;; - SearXNG (drake-karat tailnet, adult instance)
;;; - SearXNG (Kids) (searxng-kids.drake-karat.ts.net) — installed alongside
;;;   so kids on shared machines can switch in Settings → Search.
;;; - Default = whichever name the caller passed via #:default-search-name.
;;;   Mozilla's `SearchEngines.Default' sets the initial default for new
;;;   profiles; users can change it (see librewolf-kids home service for
;;;   the per-user override path on alucard/hopper).
;;; - Cookies.Allow for scratch.mit.edu — Librewolf's bundled hardening clears
;;;   cookies + site data on shutdown, which logs kids out of Scratch and
;;;   loses their saved projects between sessions.  A per-site cookie `Allow'
;;;   permission exempts just that origin from the clearing (and from cookie
;;;   blocking), so Scratch stays usable without relaxing hardening globally.
;;;
;;; Per-user override is NOT possible from this layer — Librewolf, like
;;; Firefox, only honours one system-wide policies.json.

(define %bitwarden-firefox-id
  ;; Confirmed via AMO API: addons.mozilla.org/api/v5/addons/addon/
  ;; bitwarden-password-manager/ → guid.
  "{446900e4-71c2-419f-a6a7-df9c091e268b}")

(define %searxng-adult-url
  "https://searxng.drake-karat.ts.net/search?q={searchTerms}")

(define %searxng-adult-suggest
  "https://searxng.drake-karat.ts.net/autocompleter?q={searchTerms}")

(define %searxng-adult-icon
  "https://searxng.drake-karat.ts.net/favicon.ico")

(define %searxng-kids-url
  "https://searxng-kids.drake-karat.ts.net/search?q={searchTerms}")

(define %searxng-kids-suggest
  "https://searxng-kids.drake-karat.ts.net/autocompleter?q={searchTerms}")

(define %searxng-kids-icon
  "https://searxng-kids.drake-karat.ts.net/favicon.ico")

(define (policy-json default-name)
  "Return the JSON body for /etc/librewolf/policies/policies.json with
DEFAULT-NAME picked as the SearchEngines.Default."
  (string-append
   "{
  \"policies\": {
    \"Cookies\": {
      \"Allow\": [
        \"https://scratch.mit.edu\"
      ]
    },
    \"ExtensionSettings\": {
      \"" %bitwarden-firefox-id "\": {
        \"installation_mode\": \"force_installed\",
        \"install_url\": \"https://addons.mozilla.org/firefox/downloads/latest/bitwarden-password-manager/latest.xpi\",
        \"private_browsing\": true
      }
    },
    \"SearchEngines\": {
      \"PreventInstalls\": false,
      \"Add\": [
        {
          \"Name\": \"SearXNG\",
          \"URLTemplate\": \"" %searxng-adult-url "\",
          \"SuggestURLTemplate\": \"" %searxng-adult-suggest "\",
          \"IconURL\": \"" %searxng-adult-icon "\",
          \"Method\": \"GET\",
          \"Alias\": \"s\"
        },
        {
          \"Name\": \"SearXNG (Kids)\",
          \"URLTemplate\": \"" %searxng-kids-url "\",
          \"SuggestURLTemplate\": \"" %searxng-kids-suggest "\",
          \"IconURL\": \"" %searxng-kids-icon "\",
          \"Method\": \"GET\",
          \"Alias\": \"sk\"
        }
      ],
      \"Default\": \"" default-name "\"
    }
  }
}
"))

(define* (make-librewolf-policy-service #:key (default-search-name "SearXNG"))
  "Build a service that drops /etc/librewolf/policies/policies.json with
both SearXNG variants installed and Bitwarden force-installed.
DEFAULT-SEARCH-NAME picks the system-wide default (\"SearXNG\" for
adult-default machines; \"SearXNG (Kids)\" if you want kids by default)."
  (simple-service 'librewolf-managed-policy
                  etc-service-type
                  (list `("librewolf/policies/policies.json"
                          ,(plain-file "entelequia-librewolf-policy.json"
                                       (policy-json default-search-name))))))

;; Convenience binding for the common case.  Same as
;; (make-librewolf-policy-service) — i.e. adult SearXNG default.
(define librewolf-policy-service
  (make-librewolf-policy-service))
