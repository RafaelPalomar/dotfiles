(define-module (entelequia packages xlibre-fix)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (xlibre)
  #:export (xlibre-server-no-mesa-glx))

;;; xlibre-server with the Mesa-glx OutputClass override
;;;
;;; Upstream xlibre-server >= 25.1.5 ships
;;; share/X11/xorg.conf.d/10-nvidia.conf with both `Module "glx"` and
;;; `Module "glxserver_nvidia"` in the OutputClass.  On a system with
;;; the proprietary NVIDIA driver, both modules try to register as the
;;; GLX vendor for screen 0; whichever wins makes apps using libglvnd
;;; route through it.  Mesa-glx winning means apps fall through to a
;;; Mesa "nvidia-drm" DRI driver (which does not exist) and finally to
;;; llvmpipe (software rendering).  Symptom: Luanti and other GL apps
;;; visibly slow despite nvidia-smi reporting the GPU live.
;;;
;;; A `Section "Module" / Disable "glx" / Load "glxserver_nvidia"` in
;;; the user's xserver.conf does NOT fix it because OutputClass-loaded
;;; modules override the global Disable.  See Xorg.0.log:
;;;   (II) "glx" will be loaded even though the default is to disable it.
;;;
;;; The minimal correct fix is to drop the `Module "glx"` line from the
;;; bundled OutputClass — leave glxserver_nvidia, no Mesa fallback.

(define-public xlibre-server-no-mesa-glx
  (package
    (inherit xlibre-server)
    (name "xlibre-server-no-mesa-glx")
    (arguments
     (substitute-keyword-arguments (package-arguments xlibre-server)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'install 'remove-mesa-glx-from-nvidia-conf
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((conf (string-append (assoc-ref outputs "out")
                                           "/share/X11/xorg.conf.d/"
                                           "10-nvidia.conf")))
                  (when (file-exists? conf)
                    (substitute* conf
                      (("^[ \t]*Module[ \t]+\"glx\"\n") ""))))))))))))
