========
Overview
========

This is a **GNU Guix-based declarative dotfiles system** called "entelequia". It manages complete system configurations for multiple machines (desktop and laptop) using Guix's functional package management and home-environment system. The entire system state is declared in Scheme code, making it fully reproducible.

Key Concepts
============

- **Declarative System**: Everything from kernel to dotfiles is declared in Scheme
- **Layered Architecture**: Base → Desktop-Base/Server-Base → Machine-specific (composable layers)
- **Parameterization**: ``machine-config`` record eliminates hardcoded values (hostname, GPU, timezone, etc.)
- **DRY Principle**: 90%+ code deduplication through shared libraries (common-packages, common-services)
- **Multi-System Support**: Two distinct systems (``einstein`` desktop, ``curie`` laptop) share common base
- **Guix Channels**: 6 pinned channels provide reproducible package sources (guix, nonguix, guix-xlibre, tailscale, guix-systole, systole-artwork)
- **Home Environment**: Uses Guix Home (not GNU Stow) to manage user packages and dotfiles
- **Literate Configuration**: Emacs config written in org-mode for documentation alongside code
- **Testability**: VM configurations for pre-deployment testing
