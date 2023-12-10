#!/usr/bin/env bash
set -exo pipefail

${LAKE:-lake} build

pushd examples/my_add
./test.sh
popd

pushd examples/S
./test.sh
popd

pushd tests/compile
./test.sh
popd

find tests/run -type f | xargs -n1 ${LAKE:-lake} env lean

echo "all done"
