set -ex
rm -rf .lake
LAKE=${LAKE:-lake}
$LAKE update --keep-toolchain
$LAKE build
$LAKE lean Eval.lean
$LAKE exe test
