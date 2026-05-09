(define-module (entelequia system lib chromium-policy)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (chromium-policy-service))

;;; Chromium policy service
;;;
;;; Drops a JSON file at /etc/chromium/policies/managed/entelequia.json.
;;; The Guix `ungoogled-chromium' binary reads this directory unconditionally
;;; (verified: `strings .../lib/chromium | grep /etc/chromium/policies` →
;;; "/etc/chromium/policies").  Files under managed/ are mandatory — users
;;; cannot override them from the UI; verify in chrome://policy after deploy.
;;;
;;; The policy here pins the default search engine to the user's private
;;; SearXNG instance on the drake-karat tailnet.
;;;
;;; ExtensionInstallForcelist intentionally NOT set
;;; -----------------------------------------------
;;; ungoogled-chromium upstream rejects ExtensionInstallForcelist
;;; (issue #2523, "closed as not planned") because the Chrome Web Store
;;; integration is stripped out and the install code path can't fetch
;;; CRXs from clients2.google.com.  The chromium-web-store helper
;;; extension only re-enables *manual* Web Store browsing — it does
;;; not hook the background force-install machinery.  Including the
;;; policy regardless just creates a misleading "Installation pending"
;;; row at chrome://policy.  Bitwarden install is therefore a one-time
;;; per-profile manual step — see entelequia/home/services/chromium.scm.

(define %entelequia-chromium-policy
  ;; Keep this as a literal JSON string rather than (object->json-string ...)
  ;; — pulling in (json) at system level means another module dep with no
  ;; benefit for a hand-maintained policy file.
  "{
  \"DefaultSearchProviderEnabled\": true,
  \"DefaultSearchProviderName\": \"SearXNG\",
  \"DefaultSearchProviderKeyword\": \"s\",
  \"DefaultSearchProviderSearchURL\": \"https://searxng.drake-karat.ts.net/search?q={searchTerms}\",
  \"DefaultSearchProviderSuggestURL\": \"https://searxng.drake-karat.ts.net/autocompleter\",
  \"DefaultSearchProviderIconURL\": \"https://searxng.drake-karat.ts.net/favicon.ico\"
}
")

(define chromium-policy-service
  (simple-service 'chromium-managed-policy
                  etc-service-type
                  (list `("chromium/policies/managed/entelequia.json"
                          ,(plain-file "entelequia-chromium-policy.json"
                                       %entelequia-chromium-policy)))))
