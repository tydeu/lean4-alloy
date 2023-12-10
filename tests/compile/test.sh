set -ex
rm -rf build
LAKE=${LAKE:-lake}
$LAKE build -U
$LAKE build Eval -v
.lake/build/bin/run
