;; -*- mode: scheme; -*-
;; Slicer build manifest, Qt5 variant.
;; Deployed by `slicer-init` from ~/.dotfiles/dotfiles/.local/share/slicer-templates/.

(specifications->manifest
 (list
  ;; Build tools
  "ripgrep" "cmake" "ninja" "pkg-config" "gcc-toolchain"
  "coreutils" "git" "patch" "openssl" "curl" "nss-certs"
  "bash" "perl" "make" "sed" "grep" "gawk" "findutils"
  "strace" "binutils" "file"

  ;; Core libs
  "zlib" "pcre2" "glib" "fontconfig" "freetype"

  ;; X11 / GL
  "libx11" "libxext" "libsm" "libice" "libxcb"
  "xcb-util" "xcb-util-image" "xcb-util-keysyms"
  "xcb-util-renderutil" "xcb-util-wm" "libxkbcommon"
  "libxrender" "libxfixes" "libxrandr" "mesa" "glu"

  ;; Qt 5
  "qtbase@5" "qtsvg@5" "qtmultimedia@5" "qtxmlpatterns@5"
  "qtwebengine@5" "qtx11extras@5" "qttools@5" "qtlocation@5"

  ;; Docs
  "python" "python-sphinx" "python-sphinx-rtd-theme"
  "python-sphinx-tabs" "python-sphinx-copybutton"))
