set -ex
rm -rf .lake
LAKE=${LAKE:-lake}
$LAKE update
$LAKE build
$LAKE lean Test.lean
$LAKE exe s
