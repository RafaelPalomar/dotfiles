#!/bin/sh

exec guix shell -C -N --emulate-fhs \
  uv gcc-toolchain glibc gawk grep sed bash curl \
  coreutils tar gzip zlib nss-certs python python-pip git \
  --share=$HOME/.config \
  --share=$HOME/.cache \
  --share=$HOME/.local \
  --share=$HOME/.authinfo.gpg \
  --share=$(pwd) \
  --preserve='^ANTHROPIC_API_KEY$' \
  -- env ANTHROPIC_API_KEY="$ANTHROPIC_API_KEY" \
     ~/.local/bin/aider "$@"
