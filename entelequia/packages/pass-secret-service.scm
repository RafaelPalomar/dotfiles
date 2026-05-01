(define-module (entelequia packages pass-secret-service)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages check)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz))

(define-public python-dbus-next
  (package
    (name "python-dbus-next")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "dbus_next" version))
       (sha256
        (base32 "1996lqiq1qh8li93j5b5ngwqiw6lm3fljd8aii9dlaik15ly5spl"))))
    (build-system pyproject-build-system)
    ;; Tests need a live D-Bus session; skip them in the build sandbox.
    (arguments (list #:tests? #f))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/altdesktop/python-dbus-next")
    (synopsis "Pure-Python D-Bus library with asyncio support")
    (description
     "@code{dbus-next} is a Python library for D-Bus that supports both
the asyncio event loop and the GLib main loop.  It implements the D-Bus
specification in pure Python and is used by @code{pass-secret-service}.")
    (license license:expat)))

(define-public python-pypass
  (package
    (name "python-pypass")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pypass" version))
       (sha256
        (base32 "1nm4mj7pd7gz4ghic6b3wrnd1b59hd1f0axavdabfl79wy511l7r"))
       (snippet
        '(begin
           (use-modules (guix build utils))
           ;; enum34 is a Python-2-only backport of the stdlib enum module;
           ;; pbr unconditionally turns requirements.txt entries into
           ;; install_requires.  Drop it so the build doesn't try to fetch.
           (substitute* "requirements.txt"
             (("^enum34\n") ""))
           ;; passwordstore.py shells out to 'which gpg2' / 'which gpg' at
           ;; module import time, which fails in the build sandbox.  Hard
           ;; the binary name to "gpg" — gpg is resolved on PATH at runtime.
           (substitute* "pypass/passwordstore.py"
             (("\\['which', 'gpg2'\\]") "['true']")
             (("\\['which', 'gpg'\\]") "['true']")
             (("GPG_BIN = 'gpg2'") "GPG_BIN = 'gpg'"))))))
    (build-system pyproject-build-system)
    ;; Upstream's tests rely on a writable HOME and the pass binary; skip in
    ;; the sandbox — pass is wired in at runtime via PATH (propagated input).
    (arguments (list #:tests? #f))
    (propagated-inputs
     (list password-store
           python-click
           python-colorama
           python-pexpect))
    (native-inputs (list python-pbr python-setuptools python-wheel))
    (home-page "https://github.com/ReAzem/python-pass")
    (synopsis "Python implementation of the pass password manager")
    (description
     "@code{pypass} is a Python interface to the @code{pass} password
manager.  It can read, create, edit, and remove entries from a
GPG-encrypted password store.")
    (license license:gpl3+)))

(define-public pass-secret-service
  ;; Track the latest commit on develop; upstream tags are absent.
  (let ((commit "6335c85d9a790a6472e3de6eff87a15208caa5dc")
        (revision "0"))
    (package
      (name "pass-secret-service")
      (version (git-version "0.1a0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mdellweg/pass_secret_service")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0aj60r7piiry8q1p19vkx4y7qzg97acdvbkh2nq6nla2fbf8haa9"))))
      (build-system pyproject-build-system)
      ;; The upstream test suite spins up a real D-Bus session on a
      ;; separate bus address; skip in the sandbox.
      (arguments (list #:tests? #f))
      (propagated-inputs
       (list python-click
             python-cryptography
             python-dbus-next
             python-decorator
             python-pypass))
      (native-inputs (list python-setuptools python-wheel))
      (home-page "https://github.com/mdellweg/pass_secret_service")
      (synopsis "Secret Service D-Bus daemon backed by pass")
      (description
       "@code{pass-secret-service} exposes the freedesktop.org Secret
Service D-Bus API (@code{org.freedesktop.secrets}) using @code{pass} as
the storage backend.  Applications that use @code{libsecret} or Qt
Keychain — such as the Nextcloud desktop client or GNOME Online
Accounts — can persist credentials transparently into the GPG-encrypted
password store.")
      (license license:gpl3+))))
