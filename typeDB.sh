#!/bin/bash

# === 設定 ===
HI_DIR="./dist-newstyle"  # .hi ファイルがあるディレクトリ
DUMP_DIR="hi-dumps"  # .hi のダンプを保存する一時ディレクトリ
OUTPUT_FILE="types.tsv"

#mkdir -p "$DUMP_DIR"
#rm -f "$DUMP_DIR"/*.dump-hi
#rm -f "$OUTPUT_FILE"

echo "[INFO] Dumping .hi files..."

# Dump all .hi files with unique names
find "$HI_DIR" -name "*.hi" | while read -r hi_file; do
  # パスをファイル名に変換（スラッシュをアンダースコアに）
  # safe_name を生成する際に、先頭のドットを避ける
  safe_name=$(echo "$hi_file" | sed 's|^'"$HI_DIR"'/||' | sed 's|[\\/]|_|g')
  dump_file="$DUMP_DIR/${safe_name}.dump-hi"
  #  safe_name=$(echo "$hi_file" | sed 's|/|_|g' | sed 's|\\|_|g')
  #  dump_file="$DUMP_DIR/${safe_name}.dump-hi"
  echo "[INFO] Dumping $hi_file -> $dump_file"
  ghc --show-iface "$hi_file" > "$dump_file"
done

# 1. ファイル一覧を一時ファイルに保存
find "$DUMP_DIR" -name "*.dump-hi" > dump_file_list.txt
cat dump_file_list.txt

export DEBUG=true

# 2. Pythonスクリプトに渡す
echo "[INFO] Extracting types..."
python tool/extract_exported_types.py @dump_file_list.txt > "$OUTPUT_FILE"

echo "[INFO] Done. Output written to $OUTPUT_FILE"

python tool/clean_tsv.py

exit
#cabal build -O0 -fno-ignore-interface-pragmas -fno-omit-interface-pragmas 

#rm "infer.json"
#rm "types.tsv"

file="./dist-newstyle/build/x86_64-windows/ghc-9.6.7/dlist-parser-0.1.0.0/x/dlist/noopt/build/dlist/dlist-tmp/Parser/Core/TokenParser.hi"
echo "Processing $file"
ghc --show-iface "$file" > "infer.json"
exit

for file in $(find . -name "*.hi"); do
  echo "Processing $file"
  ghc --show-iface "$file" > "infer.json"
  python "tool\\extract_exported_types.py" "infer.json" >> "types.tsv"
  #cabal run filejson "$file" >> "inferDB.json"
done

