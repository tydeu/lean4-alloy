set -ex
rm -rf build
LAKE=${LAKE:-lake}
$LAKE build -U
$LAKE build Test -v
build/bin/my_add
