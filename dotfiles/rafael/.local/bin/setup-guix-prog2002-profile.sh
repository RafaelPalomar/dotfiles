#!/bin/sh
#
# setup-guix-prog2002-profile.sh — create the PROG2002 Graphics Programming
# Guix profile used to build and run the labs, assignment and exam.
#
# Mirrors setup-guix-slicer-profile.sh: a single, channels-lock-pinned named
# profile (not a per-worktree direnv).  Declared in ~/.dotfiles and deployed by
# `guix home reconfigure`; then build the profile with:
#
#     setup-guix-prog2002-profile.sh [PROFILE_PATH] [EXTRA_PACKAGES...]
#
# PROFILE_PATH defaults to ~/PROG2002.  Once built, activate it in a shell with:
#
#     GUIX_PROFILE="$HOME/PROG2002" . "$GUIX_PROFILE/etc/profile"
#
# That puts CMake 4, the C++ toolchain, the GLAD generator (python+jinja2) and
# the GLFW/Mesa build+run deps on PATH/CPATH/LIBRARY_PATH/PKG_CONFIG_PATH.

PROFILE_PATH="${1:-$HOME/PROG2002}"
[ $# -ge 1 ] && shift
EXTRA_PACKAGES="$@"

# Everything needed to build the course project on the host: a C++ toolchain +
# CMake 4 + Ninja/Make, the GLAD loader generator (python + jinja2), and all the
# system deps GLFW compiles its Wayland AND X11 backends against, plus Mesa to
# run OpenGL on the GPU.
# gcc-toolchain@14 matches the shared CI image (g++-14). GCC 15 miscompiles the
# pinned GLM 1.0.3 (round() overload resolution), so keep the local toolchain in
# lock-step with CI.
PACKAGES="gcc-toolchain@14 cmake ninja make pkg-config git \
          python python-jinja2 \
          wayland wayland-protocols libxkbcommon extra-cmake-modules \
          libx11 libxrandr libxinerama libxcursor libxi libxext \
          libxfixes libxrender xorgproto \
          mesa glu mesa-utils nss-certs"

CHANNELS_LOCK="${PROG2002_CHANNELS_LOCK:-$HOME/.dotfiles/channels-lock.scm}"

if [ ! -e "$PROFILE_PATH" ]; then
    echo "Creating Guix profile at $PROFILE_PATH ..."
    echo "Pinning to channels: $CHANNELS_LOCK"
    guix time-machine -C "$CHANNELS_LOCK" -- \
         package -p "$PROFILE_PATH" -i $PACKAGES $EXTRA_PACKAGES
    echo
    echo "Profile created at $PROFILE_PATH"
    echo "Activate with: GUIX_PROFILE=\"$PROFILE_PATH\" . \"\$GUIX_PROFILE/etc/profile\""
else
    echo "Profile already exists at $PROFILE_PATH."
    echo "To update it:"
    echo "  guix time-machine -C \"$CHANNELS_LOCK\" -- package -p \"$PROFILE_PATH\" -i $PACKAGES"
fi
