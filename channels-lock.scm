(define-module (channels-lock))

(use-modules (guix channels))

(list (channel
        (name 'guix)
        (url "https://git.guix.gnu.org/guix.git")
        (branch "master")
        (commit
          "6a483ed7c607b01003edb9cb118c9f89c9d457e9")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (commit
          "f5338f63fce69622ce06f93fe02524967e1f30d4")
        (introduction
          (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
              "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
        (name 'guix-xlibre)
        (url "https://codeberg.org/rafaelpalomar/guix-xlibre.git")
        (branch "master")
        (commit
          "87b281333e079e3da034b7e01132f392bed30e51"))
      (channel
        (name 'tailscale)
        (url "https://github.com/umanwizard/guix-tailscale")
        (branch "main")
        (commit
          "58bc8b05520b8565a3230e21388e97f00b886e4b"))
      (channel
        (name 'guix-systole)
        (url "https://github.com/systoleos/guix-systole")
        (branch "main")
        (commit
          "4ba2e6d4d1d4ed5cfa499e9c63e03fe26ba8f196"))
      (channel
        (name 'systole-artwork)
        (url "https://github.com/systoleos/guix-systole-artwork")
        (branch "main")
        (commit
          "26e4f71bf518a03c646d42d7c65ec8529f3c63a6"))
      (channel
        (name 'guix-openclaw)
        (url "https://github.com/OUH-MESHLab/guix-openclaw")
        (branch "main")
        (commit
          "4b8f5ed7449b937d7f17d01706c575476fa72d13"))
      (channel
        ;; Pinned to rafaelpalomar/sops-guix fork with local sops package.
        ;; Upstream sops-guix (7d3eb60) assumes sops is available in
        ;; (gnu packages password-utils) from upstream Guix commit 24a33681
        ;; (2026-04-04), but our pinned Guix commit (2026-03) predates that.
        ;; Our fork restores the trivial binary-based sops package.
        ;; Introduction field is omitted because our patched commits are
        ;; not signed with the upstream maintainer's GPG key; the fork
        ;; is trusted because we control it.
        (name 'sops-guix)
        (url "https://github.com/RafaelPalomar/sops-guix.git")
        (branch "fix-local-sops-for-older-guix")
        (commit
          "b17655781fe1de0bcdede165b384d96ea2d311d8")))
