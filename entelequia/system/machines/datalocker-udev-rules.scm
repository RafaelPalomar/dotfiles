(define-module (entelequia system machines datalocker-udev-rules)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (datalocker-udev-rules-service
            datalocker-udev-rules-package))

;;; DataLocker Sentry ONE udev rule
;;;
;;; Automatically triggers unlock script when DataLocker USB is inserted

;; Trigger script content
(define datalocker-trigger-script
  "#!/bin/sh
# DataLocker Udev Trigger Wrapper - runs as root from udev

LOG_FILE=\"/tmp/datalocker-udev-trigger.log\"

# Log that udev triggered us
echo \"[$(date)] Udev rule triggered\" >> \"$LOG_FILE\" 2>&1

# Run the unlock script as user rafael with X11 environment
# Use nohup to detach from udev
nohup /run/setuid-programs/sudo -u rafael \
    env DISPLAY=:0 \
        XAUTHORITY=/home/rafael/.Xauthority \
        DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus \
        HOME=/home/rafael \
    /home/rafael/.local/bin/datalocker-unlock \
    >> \"$LOG_FILE\" 2>&1 &

echo \"[$(date)] Unlock script launched (PID: $!)\" >> \"$LOG_FILE\" 2>&1
exit 0
")

(define datalocker-rule-content
  "# DataLocker Sentry ONE - Auto-unlock when inserted
# Vendor: DL (230a), Product: Sentry ONE-M (1550)

# When the UNLOCKER partition (sr device) is added, trigger unlock script
ACTION==\"add\", SUBSYSTEM==\"block\", ENV{ID_VENDOR_ID}==\"230a\", ENV{ID_MODEL_ID}==\"1550\", ENV{ID_CDROM}==\"1\", \\
  RUN+=\"/run/current-system/profile/libexec/datalocker-udev-trigger\"
")

;; Create a package that installs the udev rule in the proper directory structure
(define-public datalocker-udev-rules-package
  (package
    (name "datalocker-udev-rules")
    (version "1.0.0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list #:modules '((guix build utils))
           #:builder
           #~(begin
               (use-modules (guix build utils))
               (let* ((out (assoc-ref %outputs "out"))
                      (rules-dir (string-append out "/lib/udev/rules.d"))
                      (rule-file (string-append rules-dir "/90-datalocker.rules"))
                      (libexec-dir (string-append out "/libexec"))
                      (trigger-script (string-append libexec-dir "/datalocker-udev-trigger")))
                 ;; Install udev rule
                 (mkdir-p rules-dir)
                 (call-with-output-file rule-file
                   (lambda (port)
                     (display #$datalocker-rule-content port)))
                 ;; Install trigger script
                 (mkdir-p libexec-dir)
                 (call-with-output-file trigger-script
                   (lambda (port)
                     (display #$datalocker-trigger-script port)))
                 ;; Make trigger script executable
                 (chmod trigger-script #o755)
                 #t))))
    (home-page "https://github.com/rafaelpalomar/dotfiles")
    (synopsis "Udev rules for DataLocker Sentry ONE auto-unlock")
    (description
     "This package provides udev rules to automatically trigger the unlock
script when a DataLocker Sentry ONE encrypted USB drive is inserted.")
    (license license:gpl3+)))

(define datalocker-udev-rules-service
  (simple-service 'datalocker-udev-rules
                  udev-service-type
                  (list datalocker-udev-rules-package)))
