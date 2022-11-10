set -ex
${LAKE:-lake} build
cd examples/my_add
./test.sh
cd ../..
cd examples/S
./test.sh
cd ../..
