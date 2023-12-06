#!/bin/bash

for problem in "data/A"?; do
    time diff ${problem}.ans <(dune exec icpc_domestic2021 a <${problem})
done
