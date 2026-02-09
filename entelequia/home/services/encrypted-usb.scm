(define-module (entelequia home services encrypted-usb)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages linux)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (encrypted-usb-configuration
            encrypted-usb-configuration?
            encrypted-usb-configuration-name
            encrypted-usb-configuration-vendor-id
            encrypted-usb-configuration-model-id
            encrypted-usb-configuration-mount-point
            encrypted-usb-configuration-unlock-command
            encrypted-usb-configuration-lock-command
            encrypted-usb-configuration-poll-interval
            home-encrypted-usb-service-type))

;;; Encrypted USB Auto-unlock Service
;;;
;;; Generic service for monitoring and auto-unlocking encrypted USB devices
;;; Supports any device that can be identified by USB vendor/model ID

(define-record-type* <encrypted-usb-configuration>
  encrypted-usb-configuration make-encrypted-usb-configuration
  encrypted-usb-configuration?
  ;; Unique name for this device configuration
  (name            encrypted-usb-configuration-name
                   (default "encrypted-usb"))
  ;; USB identification (as shown in lsusb, e.g., "230a" for vendor, "1550" for model)
  (vendor-id       encrypted-usb-configuration-vendor-id)
  (model-id        encrypted-usb-configuration-model-id)
  ;; Expected mount point name (e.g., "UNLOCKER" for DataLocker)
  ;; Used to verify the unlock partition is mounted before running unlock command
  (mount-point     encrypted-usb-configuration-mount-point
                   (default #f))  ; If #f, don't wait for mount point
  ;; Command to unlock the device (as string or gexp)
  (unlock-command  encrypted-usb-configuration-unlock-command)
  ;; Optional command to lock the device before removal (as string or gexp)
  (lock-command    encrypted-usb-configuration-lock-command
                   (default #f))
  ;; How often to poll for device (in seconds)
  (poll-interval   encrypted-usb-configuration-poll-interval
                   (default 2)))

(define (encrypted-usb-shepherd-service config)
  "Return a shepherd service that monitors for the configured USB device."
  (let ((name (encrypted-usb-configuration-name config))
        (vendor-id (encrypted-usb-configuration-vendor-id config))
        (model-id (encrypted-usb-configuration-model-id config))
        (mount-point (encrypted-usb-configuration-mount-point config))
        (unlock-cmd (encrypted-usb-configuration-unlock-command config))
        (lock-cmd (encrypted-usb-configuration-lock-command config))
        (poll-interval (encrypted-usb-configuration-poll-interval config)))

    (list
     (shepherd-service
      (documentation (string-append "Monitor for " name " USB device and auto-unlock"))
      (provision (list (string->symbol (string-append name "-monitor"))))
      (requirement '())
      (start #~(make-forkexec-constructor
                (list #$(file-append bash "/bin/bash")
                      "-c"
                      (string-append
                       "# " #$name " Monitor Service\n"
                       "LOG=/tmp/" #$name "-monitor.log\n"
                       "VENDOR_ID=" #$vendor-id "\n"
                       "MODEL_ID=" #$model-id "\n"
                       (if #$mount-point
                           (string-append "MOUNT_POINT=" #$mount-point "\n")
                           "")
                       "UNLOCK_CMD='" #$unlock-cmd "'\n"
                       (if #$lock-cmd
                           (string-append "LOCK_CMD='" #$lock-cmd "'\n")
                           "")
                       "POLL_INTERVAL=" #$(number->string poll-interval) "\n"
                       "LAST_STATE=\"\"\n"
                       "\n"
                       "log() {\n"
                       "    echo \"[$(date '+%Y-%m-%d %H:%M:%S')] $1\" >> \"$LOG\"\n"
                       "}\n"
                       "\n"
                       "log \"Monitor started for " #$name " (${VENDOR_ID}:${MODEL_ID})\"\n"
                       "\n"
                       "while true; do\n"
                       "    # Check if device is present\n"
                       "    if lsusb | grep -q \"${VENDOR_ID}:${MODEL_ID}\"; then\n"
                       "        CURRENT_STATE=\"connected\"\n"
                       "        \n"
                       "        # Only run unlock if state changed from disconnected to connected\n"
                       "        if [ \"$LAST_STATE\" != \"connected\" ]; then\n"
                       "            log \"Device detected - triggering unlock\"\n"
                       "            \n"
                       (if #$mount-point
                           "            # Wait for mount point to appear (max 30 seconds)\n"
                           "")
                       (if #$mount-point
                           "            for i in {1..30}; do\n"
                           "")
                       (if #$mount-point
                           "                if [ -d /run/media/$USER/${MOUNT_POINT} ]; then\n"
                           "")
                       (if #$mount-point
                           "                    log \"Mount point ${MOUNT_POINT} ready\"\n"
                           "")
                       (if #$mount-point
                           "                    break\n"
                           "")
                       (if #$mount-point
                           "                fi\n"
                           "")
                       (if #$mount-point
                           "                sleep 1\n"
                           "")
                       (if #$mount-point
                           "            done\n"
                           "")
                       "            \n"
                       "            # Run unlock command\n"
                       "            log \"Executing unlock command\"\n"
                       "            eval \"$UNLOCK_CMD\" >> \"$LOG\" 2>&1 &\n"
                       "            log \"Unlock command launched (PID: $!)\"\n"
                       "        fi\n"
                       "    else\n"
                       "        CURRENT_STATE=\"disconnected\"\n"
                       "        \n"
                       "        # Run lock command if device was just disconnected and lock command is defined\n"
                       (if #$lock-cmd
                           "        if [ \"$LAST_STATE\" = \"connected\" ]; then\n"
                           "")
                       (if #$lock-cmd
                           "            log \"Device disconnected - running lock command\"\n"
                           "")
                       (if #$lock-cmd
                           "            eval \"$LOCK_CMD\" >> \"$LOG\" 2>&1\n"
                           "")
                       (if #$lock-cmd
                           "        fi\n"
                           "")
                       "    fi\n"
                       "    \n"
                       "    LAST_STATE=\"$CURRENT_STATE\"\n"
                       "    sleep \"$POLL_INTERVAL\"\n"
                       "done\n"))
                #:log-file (string-append
                            (or (getenv "XDG_STATE_HOME")
                                (string-append (getenv "HOME") "/.local/state"))
                            "/" #$name "-monitor.log")
                #:environment-variables
                (list (string-append "HOME=" (getenv "HOME"))
                      (string-append "USER=" (getenv "USER"))
                      (string-append "DISPLAY=" (or (getenv "DISPLAY") ":0"))
                      (string-append "XAUTHORITY=" (getenv "HOME") "/.Xauthority")
                      (string-append "DBUS_SESSION_BUS_ADDRESS="
                                     (or (getenv "DBUS_SESSION_BUS_ADDRESS")
                                         "unix:path=/run/user/1000/bus"))
                      (string-append "PATH=" (getenv "PATH")))))
      (stop #~(make-kill-destructor))
      (respawn? #t)))))

(define home-encrypted-usb-service-type
  (service-type
   (name 'home-encrypted-usb)
   (description "Monitor for encrypted USB devices and auto-unlock/lock")
   (extensions
    (list (service-extension
           home-shepherd-service-type
           encrypted-usb-shepherd-service)))
   (default-value #f)))
