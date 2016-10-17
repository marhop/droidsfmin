#!/usr/bin/env bash

sigfile_orig=$1
pattern=$2

droidsfmin "$sigfile_orig" \
    -P <(droidsfmin -l "$sigfile_orig" | grep -E "$pattern" | cut -f 1)

