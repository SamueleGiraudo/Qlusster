#!/bin/bash

# Author: Samuele Giraudo
# Creation: dec. 2022
# Modifications: dec. 2022, jul. 2023

echo "Generating the QLU file..."
clock_start=$(date +%s.%N)
./aclove --file $1 --output no-rules no-shadows short-names | ./RemoveColors.sh > $1.qlu
clock_end=$(date +%s.%N)
time=$(echo "$clock_end - $clock_start" | bc)
time=$(printf "%.2f" "$time")
echo "End generating the QLU file [$time s]."
./qlusster --file $1.qlu --verbose --play

