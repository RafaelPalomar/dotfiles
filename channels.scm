(define-module (channels))

(use-modules (guix channels))

(list (channel
        (name 'guix)
        (url "https://codeberg.org/guix/guix.git")
        (branch "master")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (introduction
          (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
              "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
        (name 'guix-xlibre)
        (url "https://codeberg.org/rafaelpalomar/guix-xlibre.git")
        (branch "master"))
      (channel
        (name 'tailscale)
        (url "https://codeberg.org/rafaelpalomar/guix-tailscale.git")
        (branch "main"))
      (channel
        (name 'guix-systole)
        (url "https://github.com/systoleos/guix-systole")
        (branch "main"))
      (channel
        (name 'systole-artwork)
        (url "https://github.com/systoleos/guix-systole-artwork")
        (branch "main"))
      (channel
        (name 'guix-openclaw)
        (url "https://github.com/OUH-MESHLab/guix-openclaw")
        (branch "main"))
      (channel
        (name 'sops-guix)
        (url "https://github.com/fishinthecalculator/sops-guix.git")
        (branch "main")
        (introduction
          (make-channel-introduction
            "0bbaf1fdd25266c7df790f65640aaa01e6d2dbc9"
            (openpgp-fingerprint
              "8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2")))))
