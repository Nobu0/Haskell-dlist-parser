#!/bin/bash

# 出力先ディレクトリ
outdir="test/log/fun"
mkdir -p "$outdir"

# src デ
ィレクトリ内の .hs ファイルをすべて探す
find test -type f -name "*.fun" | while read file; do
  echo "=== Running: $file ==="

  # src/ を取り除いて、スラッシュをアンダースコアに変換
  relpath="${file#src/}"
  safe_name="${relpath//\//_}"

  # 出力ファイル名
  outfile="$outdir/${safe_name}.txt"

  cabal run infer -- "$file" > "$outfile"
  echo ""
done
