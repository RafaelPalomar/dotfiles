(list (channel
       (name 'guix)
       (url "https://codeberg.org/guix/guix.git")
       (branch "master")
       (commit "f2295f8fde56f06f0759a1cd8d927745af9e360d")
       (introduction
        (make-channel-introduction
         "9edb3f66fd807b096b48283debdcddccfea34bad"
         (openpgp-fingerprint
          "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
       (name 'nonguix)
       (url "https://gitlab.com/nonguix/nonguix")
       (branch "master")
       (commit "35a3ee8b8c7897fff288a890ebefa417eea0e0e3")
       (introduction
        (make-channel-introduction
         "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
         (openpgp-fingerprint
          "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
       (name 'guix-xlibre)
       (url "https://codeberg.org/rafaelpalomar/guix-xlibre.git")
       (branch "master")
       (commit "f0f1edbc94f1f3f47103171f628ef88464afe560"))
      (channel
       (name 'tailscale)
       (url "https://github.com/umanwizard/guix-tailscale")
       (branch "main")
       (commit "58bc8b05520b8565a3230e21388e97f00b886e4b"))
      (channel
       (name 'guix-systole)
       (url "https://github.com/systoleos/guix-systole")
       (branch "main")
       (commit "711d6fef96353c9ec4f2533eaa6665eb2f55ddc1"))
      (channel
       (name 'systole-artwork)
       (url "https://github.com/systoleos/guix-systole-artwork")
       (branch "main")
       (commit "26e4f71bf518a03c646d42d7c65ec8529f3c63a6"))
      (channel
       (name 'guix-openclaw)
       (url "https://github.com/OUH-MESHLab/guix-openclaw")
       (branch "main")
       (commit "4b8f5ed7449b937d7f17d01706c575476fa72d13"))
      (channel
       (name 'sops-guix)
       (url "https://github.com/fishinthecalculator/sops-guix.git")
       (branch "main")
       (commit "5a88726e1be11e2df0bb85b597a1f597e334e0b7")
       (introduction
        (make-channel-introduction
         "0bbaf1fdd25266c7df790f65640aaa01e6d2dbc9"
         (openpgp-fingerprint
          "8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2")))))
