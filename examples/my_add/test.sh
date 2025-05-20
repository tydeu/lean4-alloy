set -ex
rm -rf .lake
LAKE=${LAKE:-lake}
$LAKE update --keep-toolchain
$LAKE build
$LAKE lean Test.lean
$LAKE exe my_add
