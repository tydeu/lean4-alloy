set -ex
rm -rf .lake
LAKE=${LAKE:-lake}
$LAKE build -U
$LAKE build Test -v
.lake/build/bin/s
