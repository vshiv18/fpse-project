#!/bin/bash

for f in ./cum_fastas/*.fasta; do
    FILENAME=$(basename $f .fasta)
    IFS='_' read -ra ADDR <<<"$FILENAME"
    N=${ADDR[0]}
    mkdir -p ./out/bwt/$FILENAME
    echo "Running sacak on $f"
    gtime -v ../_build/default/src/sacak_bwt.exe -i $f --out-dir ./out/sacak/$FILENAME 2>&1 | grep -E 'Elapsed|Maximum resident set size' >./out/sacak/$FILENAME/performance.txt
done
