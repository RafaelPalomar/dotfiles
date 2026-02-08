(define-module (entelequia lib records)
  #:use-module (gnu system keyboard)
  #:use-module (guix records)
  #:export (machine-config
            machine-config?
            machine-config-hostname
            machine-config-username
            machine-config-locale
            machine-config-timezone
            machine-config-keyboard
            machine-config-gpu-type
            machine-config-machine-type))

;;; Configuration record for parameterized system definitions
;;;
;;; This record type allows system configurations to be parameterized
;;; instead of having hardcoded values, making it easier to create
;;; new machine configurations and test variations in VMs.

(define-record-type* <machine-config>
  machine-config make-machine-config
  machine-config?
  ;; Machine identification
  (hostname     machine-config-hostname)              ; Required: "einstein", "curie", etc.
  (username     machine-config-username)              ; Required: "rafael", "test", etc.

  ;; Localization (with sensible defaults)
  (locale       machine-config-locale                 ; Locale setting
                (default "en_US.utf8"))
  (timezone     machine-config-timezone               ; Timezone
                (default "Europe/Oslo"))
  (keyboard     machine-config-keyboard               ; keyboard-layout object
                (default (keyboard-layout "us" "altgr-intl" #:model "thinkpad")))

  ;; Hardware classification
  (gpu-type     machine-config-gpu-type               ; 'nvidia, 'amd, 'intel, or #f
                (default 'intel))
  (machine-type machine-config-machine-type           ; 'desktop, 'laptop, 'server
                (default 'desktop)))
