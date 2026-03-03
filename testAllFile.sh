#!/bin/bash

# 出力先ディレクトリ
outdir="test/log"
mkdir -p "$outdir"

# src ディレクトリ内の .hs ファイルをすべて探す
find src -type f -name "*.hs" | while read file; do
  echo "=== Running: $file ==="

  # src/ を取り除いて、スラッシュをアンダースコアに変換
  relpath="${file#src/}"
  safe_name="${relpath//\//_}"

  # 出力ファイル名
  outfile="$outdir/${safe_name}.txt"

  cabal run myapp -- "$file" > "$outfile"
  echo ""
done
head $outdir/*.hs.txt > $outdir/../all_result.txt
diff $outdir/../all_result.txt $outdir/../all_result_org.txt

cabal run dlist > test/dlist_tmp.txt
diff test/dlist_log.txt test/dlist_tmp.txt
exit


#!/bin/bash

# src ディレクトリ内の .hs ファイルをすべて探す
find src -type f -name "*.hs" | while read file; do
  
  echo "=== Running: $file ==="
  cabal run myapp -- "$file" > test/log/$file.txt
  echo ""

done
