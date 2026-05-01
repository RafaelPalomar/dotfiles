#!/bin/sh
# Build the Habitica container image on lovelace.
#
# Habitica has no published OCI image; this script clones HabitRPG/habitica at
# a pinned commit and runs `podman build -f Dockerfile-Dev` to produce a local
# image tagged habitica:<commit>. The pin must match %habitica-commit in
# entelequia/system/lib/server-services.scm.
#
# Run on lovelace (rootless Podman, user rafael), e.g.:
#   ssh -p 2222 rafael@lovelace.<tailnet>.ts.net /home/rafael/.dotfiles/scripts/build-habitica-image.sh
#
# Or copy + run locally:
#   scp -P 2222 scripts/build-habitica-image.sh rafael@lovelace:/tmp/
#   ssh -p 2222 rafael@lovelace /tmp/build-habitica-image.sh

set -eu

# Keep this in sync with %habitica-commit in
# entelequia/system/lib/server-services.scm.
COMMIT="${HABITICA_COMMIT:-a92999fc11a0fcfe24d74ddc952219ece5d73101}"
TAG="habitica:${COMMIT}"
WORKDIR="$(mktemp -d -t habitica-build.XXXXXX)"
trap 'rm -rf "$WORKDIR"' EXIT

echo ">> Building $TAG"
echo ">> Workdir: $WORKDIR"

# Skip if already built — `podman image exists` returns 0 if found.
if podman image exists "$TAG"; then
    echo ">> Image $TAG already present; nothing to do."
    echo ">> (Force rebuild: podman rmi $TAG && rerun.)"
    exit 0
fi

cd "$WORKDIR"
git clone --filter=blob:none https://github.com/HabitRPG/habitica.git src
cd src
git checkout "$COMMIT"

# config.json is gitignored upstream; the build (vite + gulp) reads it via nconf
# and bakes TRUSTED_DOMAINS into the client bundle.  Seed it from the example
# and override the deployment-facing keys so the SPA points at our tailnet URL.
BASE_URL="${HABITICA_BASE_URL:-https://habitica.drake-karat.ts.net}"
sed -e "s|\"BASE_URL\": \".*\"|\"BASE_URL\": \"${BASE_URL}\"|" \
    -e "s|\"TRUSTED_DOMAINS\": \".*\"|\"TRUSTED_DOMAINS\": \"${BASE_URL}\"|" \
    config.json.example > config.json

# Upstream's website/server/index.js gates @babel/register on
# NODE_ENV != production, expecting prod runs to use a pre-transpiled tree.
# In our setup we run npm start against the source tree with NODE_ENV=production,
# so under Node 20 ESM-detect the first ESM-syntax server file (libs/setupNconf.js)
# loads as a real ES module and crashes on `__dirname`.  Drop the env gate so the
# babel runtime hook is always installed and the source-tree run works.
sed -i \
    -e "s|^if (process\\.env\\.NODE_ENV !== 'production') {$|/* patched: babel/register unconditional (see build-habitica-image.sh) */ if (true) {|" \
    website/server/index.js

# npm install fetches ~700 packages and can hit EIDLETIMEOUT on slow uplinks.
# Bump fetch timeout to 10 min and allow more retries before giving up.  These
# are baked into the image as ENV so they apply to every RUN npm install layer.
sed -i '/^WORKDIR/a ENV npm_config_fetch_timeout=600000\nENV npm_config_fetch_retries=10\nENV npm_config_fetch_retry_maxtimeout=120000' Dockerfile-Dev

# Dockerfile-Dev runs `npm run client:build && gulp build:prod` so the resulting
# image's `npm start` serves the API plus the built SPA from port 3000 — no
# separate client container needed.
podman build -f Dockerfile-Dev -t "$TAG" .

echo ">> Built $TAG"
podman images "$TAG"
