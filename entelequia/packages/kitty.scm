(define-module (entelequia packages kitty)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages terminals)
  #:export (kitty-with-synergy-scroll))

;;; kitty with Synergy / Barrier / Deskflow scroll-wheel fix
;;;
;;; kitty 0.46 introduced high-resolution scroll handling via XI2 and
;;; gated the legacy Button4/5/6/7 path in glfw/x11_window.c behind
;;;
;;;   if (!_glfw.x11.xi.num_scroll_devices)
;;;
;;; The intent of that guard is to avoid double-counting on devices that
;;; would emit both XI2 smooth-scroll axes and legacy button events.  In
;;; practice no real device does that, but as soon as ANY XI2 scroll
;;; device exists (a touchpad, a real mouse), the guard becomes true
;;; and kitty silently drops every Button4/5 event that arrives — which
;;; is exactly how Synergy, Barrier, Deskflow, and Input-Leap forward
;;; the scroll wheel from the server to the client, via XTestFakeButton.
;;; Every other X11 app on the client receives the events normally; only
;;; kitty filters them out.
;;;
;;; Upstream tracking:
;;;   https://github.com/kovidgoyal/kitty/issues/9958
;;;   https://github.com/kovidgoyal/kitty/issues/10046
;;;
;;; The minimal correct fix is to drop the XI2 guard so the legacy path
;;; runs unconditionally.  Real XI2 scroll devices don't generate
;;; Button4/5 events, so this doesn't cause double scrolling.  The
;;; substitution targets exactly the four guard lines in the
;;; ButtonPress/ButtonRelease handlers; `num_scroll_devices' is not
;;; consulted anywhere else in the file for this purpose.

(define-public kitty-with-synergy-scroll
  (package
    (inherit kitty)
    (name "kitty-with-synergy-scroll")
    (arguments
     (substitute-keyword-arguments (package-arguments kitty)
       ((#:phases phases)
        #~(modify-phases #$phases
            ;; Guix master (Aug 2026) bumped docutils to 0.22, which breaks
            ;; sphinx-inline-tabs (`KeyError: 'backrefs'` in its visit()).
            ;; kitty's `setup.py linux-package` runs `make docs` (sphinx),
            ;; so the whole build fails on the docs step.  kitty SKIPS
            ;; building docs when `docs/_build/html` already exists, then
            ;; simply copies docs/_build/{html,man}; pre-create both so the
            ;; sphinx path is never taken.  We don't consume kitty's bundled
            ;; HTML docs locally.  Drop this once guix fixes the
            ;; sphinx-inline-tabs/docutils incompatibility upstream.
            (add-after 'unpack 'stub-kitty-docs
              (lambda _
                (let ((build "src/github.com/kovidgoyal/kitty/docs/_build"))
                  (mkdir-p (string-append build "/html"))
                  (mkdir-p (string-append build "/man"))
                  (call-with-output-file (string-append build "/html/index.html")
                    (lambda (port)
                      (display "<!-- docs omitted (entelequia build) -->\n"
                               port))))))
            (add-after 'unpack 'allow-xtest-scroll
              (lambda _
                ;; Match only the four bare guards by anchoring on the
                ;; trailing newline — `for (... num_scroll_devices; ...)`
                ;; and `if (!num_scroll_devices) return;` on line 543
                ;; have other text on the same line and must not match.
                ;; (`$' alone does not work here: Guix `substitute*' keeps
                ;; the newline in the haystack, so `$' would have to sit
                ;; before the `\n', and `\n$' is just longer than `\n'.)
                (substitute*
                    "src/github.com/kovidgoyal/kitty/glfw/x11_window.c"
                  (("if \\(!_glfw\\.x11\\.xi\\.num_scroll_devices\\)\n")
                   "if (1) /* entelequia: accept XTest Button4/5/6/7 */\n"))))))))))
