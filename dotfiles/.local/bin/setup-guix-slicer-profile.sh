#!/bin/sh

if [ $# -lt 1 ]; then
    echo "Usage: $0 PROFILE_PATH [QT_VERSION] [EXTRA_PACKAGES...]"
    exit 1
fi

PROFILE_PATH="$1"
QT_VERSION="${2:-6}"
shift 2  # Skip both PROFILE_PATH and QT_VERSION

# Remaining arguments are extra packages
EXTRA_PACKAGES="$@"

# Base packages (Qt-agnostic)
BASE_PACKAGES="ripgrep cmake ninja pkg-config zlib pcre2 glib gcc-toolchain \
               libx11 libxext libsm libice libxcb xcb-util xcb-util-image \
               xcb-util-keysyms xcb-util-renderutil xcb-util-wm libxkbcommon \
               coreutils git patch openssl curl nss-certs bash perl make sed grep \
               gawk findutils strace binutils file fontconfig freetype libxrender \
               libxfixes libxrandr mesa glu"

DOC_PACKAGES="python python-sphinx python-sphinx-rtd-theme python-sphinx-tabs \
              python-sphinx-copybutton"

# Qt version specific packages
if [ "$QT_VERSION" = "5" ]; then
    QT_PACKAGES="qtbase@5 qtsvg@5 qtmultimedia@5 qtxmlpatterns@5 \
        qtwebengine@5 qtx11extras@5 qttools@5 qtlocation@5"
else
    QT_PACKAGES="qtbase qtsvg qtmultimedia qtxmlpatterns qtwebengine \
        qtx11extras qttools qtpositioning qt5compat qtscxml"
fi

if [ ! -e "$PROFILE_PATH" ]; then
    echo "Creating Guix profile at $PROFILE_PATH with Qt${QT_VERSION}..."
    guix package -p "$PROFILE_PATH" -i \
         $BASE_PACKAGES $QT_PACKAGES $DOC_PACKAGES $EXTRA_PACKAGES

    echo "Profile created successfully at $PROFILE_PATH"
else
    echo "Profile already exists at $PROFILE_PATH"
fi
