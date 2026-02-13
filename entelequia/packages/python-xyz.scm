(define-module (entelequia packages python-xyz)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix build utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))


(define-public python-rofi-rbw
  (package
   (name "python-rofi-rbw")
   (version "1.6.0")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "rofi_rbw" version))
     (sha256
      (base32 "1sncly7cbr2g5j8brmwavyn48g076imrfjcffqcwc151sj3md17g"))))
   (build-system pyproject-build-system)
   (arguments
    (list
     #:phases
     #~(modify-phases %standard-phases
         (add-after 'unpack 'fix-wayland-detection
           (lambda _
             (substitute* "src/rofi_rbw/abstractionhelper.py"
               (("return os.environ.get\\(\"WAYLAND_DISPLAY\", False\\) is not None")
                "return bool(os.environ.get(\"WAYLAND_DISPLAY\"))"))))
         (add-after 'unpack 'fix-missing-type-field
           (lambda _
             ;; rbw 1.14.1 doesn't include 'type' field in JSON output
             ;; Default to 'Login' type for compatibility (capitalized to match EntryType.LOGIN.value)
             (substitute* "src/rofi_rbw/rbw.py"
               (("item\\[\"type\"\\]")
                "item.get(\"type\", \"Login\")"))))
         (add-after 'unpack 'fix-folder-matching
           (lambda _
             ;; Fix folder matching - normalize None to empty string
             (substitute* "src/rofi_rbw/selector/rofi.py"
               (("entry\\.folder == match\\.group\\(\"folder\"\\)")
                "(entry.folder or \"\") == (match.group(\"folder\") or \"\")")))))))
   (propagated-inputs (list python-configargparse xdotool))
   (native-inputs (list python-hatchling))
   (home-page "https://github.com/fdw/rofi-rbw")
   (synopsis "Rofi frontend for Bitwarden")
   (description "Rofi frontend for Bitwarden via rbw.")
   (license license:expat)))
