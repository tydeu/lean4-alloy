#!/usr/bin/env bash
set -exo pipefail

${LAKE:-lake} build

pushd examples/my_add
./test.sh
popd

pushd examples/S
./test.sh
popd

find tests -type f -exec ${LAKE:-lake} env lean {} \;
