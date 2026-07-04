(define-module (entelequia system lib xorg-configs)
  #:use-module (entelequia packages xlibre-fix)
  #:use-module (gnu system keyboard)
  #:use-module (nongnu packages nvidia)
  #:use-module (xlibre)
  #:export (make-xlibre-config))

;;; Per-GPU xlibre configurations
;;;
;;; One constructor keyed off machine-config-gpu-type, so machine files no
;;; longer carry duplicated xlibre-configuration blocks (einstein ≡ alucard
;;; and hopper ≡ baroja had byte-identical copies differing only in the
;;; keyboard literal).  Machines needing something unusual can still pass
;;; #:xorg-config to make-desktop-base-os.

(define (make-xlibre-config gpu-type keyboard)
  "Return an xlibre xorg-configuration for GPU-TYPE ('nvidia, 'amd, 'intel,
or anything else for a plain default), using KEYBOARD as the layout."
  (case gpu-type
    ((nvidia)
     (xlibre-configuration
      ;; Patched xlibre-server: drops `Module "glx"` from the bundled
      ;; share/X11/xorg.conf.d/10-nvidia.conf so the OutputClass only
      ;; loads NVIDIA's GLX (see entelequia/packages/xlibre-fix.scm).
      (server xlibre-server-no-mesa-glx)
      (modules (list nvidia-driver xlibre-input-libinput))
      (drivers '("nvidia"))
      (keyboard-layout keyboard)
      ;; Belt-and-suspenders: even with the OutputClass patched, X still
      ;; loads a default `glx` module that registers as the GLX vendor
      ;; for screen 0 ahead of glxserver_nvidia.  Disable it explicitly
      ;; so libglvnd routes to NVIDIA and apps get hardware acceleration.
      (extra-config
       (list "Section \"Module\""
             "  Disable \"glx\""
             "  Load \"glxserver_nvidia\""
             "EndSection"))))
    ((amd)
     ;; modesetting instead of xlibre-video-amdgpu for better pixmap
     ;; stability (per-CRTC framebuffers prevent pixmap corruption).
     ;; See: https://wiki.archlinux.org/title/AMDGPU#Xorg_configuration
     (xlibre-configuration
      (modules (list xlibre-input-libinput))
      ;; NOTE on `drivers': Guix ALWAYS emits a default `device-modesetting' +
      ;; `screen-modesetting' pair (whether `drivers' is '() or
      ;; '("modesetting")).  That auto pair would shadow the custom "AMD
      ;; Graphics" Device below — which is why TearFree/SWcursor never applied
      ;; before.  We cannot inject options into the auto device, so instead we
      ;; declare an explicit ServerLayout -> Screen -> "AMD Graphics" Device in
      ;; extra-config.  X uses the explicit ServerLayout and leaves the auto
      ;; sections unreferenced, so our Device (with its options) is the one
      ;; that actually binds.
      (drivers '())
      (keyboard-layout keyboard)
      (extra-config
       (list "Section \"Device\""
             "  Identifier \"AMD Graphics\""
             "  Driver \"modesetting\""
             "  Option \"TearFree\" \"true\""
             ;; SWcursor: the modesetting HW cursor plane is not rotated on
             ;; the amdgpu/Strix Halo path, so the pointer is invisible on a
             ;; rotated external output (e.g. a dock's portrait monitor) even
             ;; though everything else renders.  Software cursor composites
             ;; correctly on rotated CRTCs.  Negligible cost on a static
             ;; desktop.
             "  Option \"SWcursor\" \"true\""
             "EndSection"
             "Section \"Screen\""
             "  Identifier \"AMD Screen\""
             "  Device \"AMD Graphics\""
             "EndSection"
             ;; Explicit ServerLayout so X binds OUR Screen/Device instead of
             ;; the auto-generated screen-modesetting (which carries no
             ;; options).
             "Section \"ServerLayout\""
             "  Identifier \"AMD Layout\""
             "  Screen \"AMD Screen\""
             "EndSection"))))
    ((intel)
     ;; modesetting (KMS) + TearFree — works well for Intel iGPUs from
     ;; Sandy Bridge onwards and avoids the legacy xlibre-video-intel DDX.
     (xlibre-configuration
      (modules (list xlibre-input-libinput))
      (drivers '("modesetting"))
      (keyboard-layout keyboard)
      (extra-config
       (list "Section \"Device\""
             "  Identifier \"Intel Graphics\""
             "  Driver \"modesetting\""
             "  Option \"TearFree\" \"true\""
             "EndSection"))))
    (else
     (xlibre-configuration
      (modules (list xlibre-input-libinput))
      (keyboard-layout keyboard)))))
