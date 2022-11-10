set -ex
rm -rf build
LAKE=${LAKE:-lake}
$LAKE build
$LAKE build Test -v
#build/bin/s # TODO: Segfaults for some reason
