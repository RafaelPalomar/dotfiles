(define-module (entelequia packages gns3)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages virtualization))


(define-public gns3-server
  (package
    (name "gns3-server")
    (version "2.2.57")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/a5/b8/"
             "131b3910b4ed4a41e9576935729bd3d031e5c4e85aa2d8210aad345cbacf/"
             "gns3_server-" version ".tar.gz"))
       (sha256
        (base32 "03xbrp1afygfc9l9sh1p35yzpanmpqvgsinmbnnv26mdvgd4hdzy"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f                   ; tests require network + running GNS3 env
      #:phases
      #~(modify-phases %standard-phases
          (delete 'sanity-check)    ; platformdirs 4.x is API-compatible but fails version pin check
          (add-after 'wrap 'wrap-qemu-path
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Prepend Guix QEMU's bin dir to PATH so GNS3 finds the correct
              ;; qemu-system-* binaries without manual preference configuration.
              (wrap-program (string-append #$output "/bin/gns3server")
                `("PATH" prefix
                  (,(string-append (assoc-ref inputs "qemu") "/bin")))))))))
    (native-inputs
     (list python-setuptools))
    (inputs
     (list qemu))
    (propagated-inputs
     (list python-aiohttp
           python-aiohttp-cors
           python-aiofiles
           python-jinja2
           python-sentry-sdk
           python-psutil
           python-py-cpuinfo
           python-platformdirs
           python-distro
           python-jsonschema
           python-truststore))
    (home-page "https://github.com/GNS3/gns3-server")
    (synopsis "GNS3 network emulator server")
    (description
     "GNS3 server manages emulated network devices (QEMU VMs, Docker
containers, Dynamips, etc.) and exposes them via a REST API consumed by
the GNS3 GUI client.  The server binary is wrapped so that Guix's QEMU
binaries are on PATH by default, requiring no manual preference setup.")
    (license license:gpl3+)))


(define-public gns3-gui
  (package
    (name "gns3-gui")
    (version "2.2.57")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/95/5d/"
             "9de7c2070dd03fae8788505ddaac5cca20b3919518fe16042702ed4db440/"
             "gns3_gui-" version ".tar.gz"))
       (sha256
        (base32 "01nj15mq3h51rvj1xyxd32zmhg1r6ry92i07ywpcmnaffi4mihws"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f                   ; tests require a running GNS3 server
      #:phases
      #~(modify-phases %standard-phases
          (delete 'sanity-check)))) ; psutil/jsonschema version pins stricter than Guix provides
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list gns3-server
           python-jsonschema
           python-sentry-sdk
           python-psutil
           python-distro
           python-truststore
           python-qdarkstyle
           python-pyqt-6))
    (home-page "https://github.com/GNS3/gns3-gui")
    (synopsis "GNS3 network emulator graphical interface")
    (description
     "GNS3 is an open-source network emulator that lets you design and
run complex network topologies on your desktop.  It supports Cisco IOS
(via Dynamips), MikroTik RouterOS, and any device that can run inside
QEMU or Docker containers.  This package provides the PyQt6-based GUI
that connects to a local or remote GNS3 server.")
    (license license:gpl3+)))
