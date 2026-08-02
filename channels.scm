;; Intent file: names/URLs/branches only.  guix time-machine -C loads this in
;; a clean user-module that pre-provides the (guix channels) bindings but has
;; NO module forms — neither define-module nor use-modules resolve there.
;; Keep it a bare (list (channel ...)) exactly like the generated lock.

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
        (name 'guix-hermes)
        (url "https://github.com/OUH-MESHLab/guix-hermes")
        (branch "main"))
      (channel
        (name 'guix-openclaw)
        (url "https://github.com/OUH-MESHLab/guix-openclaw")
        (branch "main"))
      (channel
        (name 'guix-agentic)
        (url "https://github.com/OUH-MESHLab/guix-agentic")
        ;; household-cooperation: coordinated feature branch (consult broker,
        ;; web + guix-host capabilities); flip back to main when it merges.
        (branch "household-cooperation"))
      (channel
        (name 'archimedes-agent)
        (url "https://github.com/RafaelPalomar/archimedes-agent")
        ;; household-cooperation: coordinated feature branch; flip back to
        ;; main when it merges.
        (branch "household-cooperation"))
      (channel
        (name 'alpha-agent)
        (url "https://github.com/RafaelPalomar/alpha-agent")
        ;; household-cooperation: coordinated feature branch (poppins durable
        ;; memory, web harness, entelequia context); flip back to master when
        ;; it merges.
        (branch "household-cooperation"))
      (channel
        (name 'mr-banks)
        (url "https://github.com/RafaelPalomar/mr-banks")
        ;; Mr. Banks, the household finance agent (pure agent code — no ledger
        ;; data, which lives in a private folder).  household-cooperation:
        ;; coordinated feature branch alongside guix-agentic/alpha-agent; flip
        ;; to main when it merges.
        (branch "household-cooperation"))
      (channel
        (name 'guix-codegraph)
        (url "https://github.com/OUH-MESHLab/guix-codegraph")
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
