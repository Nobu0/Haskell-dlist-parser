#!/bin/bash

# src ディレクトリ内の .hs ファイルをすべて探す
find src -type f -name "*.hs" | while read file; do
  echo "=== Running: $file ==="
  cabal run myapp -- "$file"
  echo ""
done
