(define-module (entelequia lib helpers)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:export (gpu-driver-packages
            gpu-kernel-arguments
            specifications->packages))

;;; Helper functions for system configuration
;;;
;;; This module provides utility functions used across system
;;; and home configurations to reduce code duplication.

;;; Package utilities

(define* (specifications->packages specs)
  "Convert a list of package specification strings to package objects."
  (map specification->package specs))

;;; GPU-related helpers

(define* (gpu-driver-packages gpu-type)
  "Return GPU-specific driver packages based on GPU type.
   GPU-TYPE should be one of: 'nvidia, 'amd, 'intel, or #f (no GPU)."
  (case gpu-type
    ((nvidia)
     '("nvidia-driver"))
    ((amd)
     '("xlibre-video-amdgpu"
       "amd-microcode"
       "amdgpu-firmware"))
    ((intel)
     '("xlibre-video-intel"
       "intel-microcode"))
    (else
     '())))

(define* (gpu-kernel-arguments gpu-type #:key (extra-args '()))
  "Return kernel arguments appropriate for the GPU type.
   EXTRA-ARGS can be provided to add machine-specific arguments."
  (let ((base-args '("quiet" "splash" "pcspkr.disable=1"))
        (gpu-args
         (case gpu-type
           ((nvidia)
            '("modprobe.blacklist=nouveau"
              "nvidia-drm.modeset=1"))
           ((amd)
            '("amd_pstate=active"
              "amdgpu.dc=1"
              "amdgpu.dpm=1"))
           ((intel)
            '())
           (else
            '()))))
    (append base-args gpu-args extra-args)))
