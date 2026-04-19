#!/bin/bash

if [ $# -lt 1 ]; then
    echo "Usage: ./run_extract.sh <directory>"
    exit 1
fi

TARGET_DIR="$1"
FILE_TYPE="typeapi.tsv"
rm $FILE_TYPE

for file in "$TARGET_DIR"/*.hs; do
    echo "Processing $file ..."
    python tool/get_api.py "$file" >> $FILE_TYPE
done
