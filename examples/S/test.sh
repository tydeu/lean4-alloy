set -ex
rm -rf build
LAKE=${LAKE:-lake}
$LAKE build Test
$LAKE build
#build/bin/s # TODO: Segfaults for some reason
