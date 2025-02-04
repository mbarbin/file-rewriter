#!/bin/bash -e

dirs=(
    # Add new directories below:
    "lib/file_rewriter/src"
    "lib/file_rewriter/test"
    "lib/sexps_rewriter/src"
    "lib/sexps_rewriter/test"
)

for dir in "${dirs[@]}"; do
    # Apply headache to .ml files
    headache -c .headache.config -h COPYING.HEADER ${dir}/*.ml

    # Check if .mli files exist in the directory, if so apply headache
    if ls ${dir}/*.mli 1> /dev/null 2>&1; then
        headache -c .headache.config -h COPYING.HEADER ${dir}/*.mli
    fi
done

dune fmt
