#!/bin/bash

entry=$(echo "$1" | tr A-Z a-z)

if [ ${#entry} -ne 1 ]; then
    echo 'Error: Enter a problem entry.'
    exit 1
fi

dune build # NOTE: Build first so that the test run time does not include the build time.

for problem in "data/$(echo $entry | tr a-z A-Z)"?; do
    time diff ${problem}.ans <(dune exec icpc_domestic2021 $entry <${problem})
done
