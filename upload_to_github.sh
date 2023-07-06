#! /usr/bin/env nix-shell
#! nix-shell -i bash -p jq github-cli

set -e

BUILT=$(nix build .#ghc945-static-github --no-link --json | jq -r '.[0].outputs.out')
echo "Built:"
ls -lh "$BUILT"

VERSION="$(nix eval .#ghc945-static.version --raw)"

ARTIFACT_LINUX="$BUILT/rust-notebook-language-server-$VERSION-x86_64-linux"
ARTIFACT_LINUX_ARCHIVE="$ARTIFACT_LINUX.tar.gz"

# Smoke check
$ARTIFACT_LINUX --help 2>&1 > /dev/null

TAG=v"$VERSION"
echo "Tagging at $TAG"
git tag "$TAG" -f
echo ""

echo "Pushing tags"
git push --tags
echo ""

echo "Creating release $TAG"
gh release create "$TAG" \
  "$ARTIFACT_LINUX_ARCHIVE" \
  --title "$TAG" \
  --notes ""
