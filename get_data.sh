#!/bin/sh

COOKIE="${1}"

for i in $(seq 1 25); do
    dir=$(printf './data/day%02d' "$i")
    mkdir -p "$dir"
    curl -b "$COOKIE" "https://adventofcode.com/2023/day/${i}/input" -o "${dir}/input"
done
