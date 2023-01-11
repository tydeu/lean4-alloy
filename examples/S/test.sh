set -ex
rm -rf build
LAKE=${LAKE:-lake}
$LAKE build -U
$LAKE build Test -v
#build/bin/s # TODO: Segfaults for some reason
