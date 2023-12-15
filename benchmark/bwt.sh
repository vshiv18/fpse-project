#!/bin/bash

for f in ./cum_fastas/*.fasta; do
    FILENAME=$(basename $f .fasta)
    IFS='_' read -ra ADDR <<<"$FILENAME"
    N=${ADDR[0]}
    mkdir -p ./out/bwt/$FILENAME
    echo "Running bwt on $f"
    gtime -v ../_build/default/src/bwt.exe -i $f --out-dir ./out/bwt/$FILENAME 2>&1 | grep -E 'Elapsed|Maximum resident set size' >./out/bwt/$FILENAME/performance.txt
done
