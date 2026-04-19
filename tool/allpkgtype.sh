#!/bin/bash

set -e

(
  echo ":browse! Prelude"
  echo ":browse! GHC.Base"
  for m in $(ghc-pkg field base exposed-modules --simple-output | sed 's/,//g'); do
      echo ":browse! $m"
  done
) | ghci > tool/browse_all.txt

echo "[DONE] browse_all.txt created."


OUT=tool/browse_all2.txt
rm -f $OUT

# GHCi がロードできるパッケージだけ
PKGS="base ghc-prim integer-gmp array bytestring containers"

for pkg in $PKGS; do
    echo "[PACKAGE] $pkg"

    MODULES=$(ghc-pkg field $pkg exposed-modules --simple-output | sed 's/,//g')

    for m in $MODULES; do
        echo "[MODULE] $m"
        echo ":browse! $m" | ghci >> $OUT
    done
done

echo "[DONE] ${OUT} created."

exit
# base パッケージに含まれるすべてのモジュール名を表示
#!/bin/bash

for m in $(ghc-pkg field base exposed-modules --simple-output | sed 's/,//g'); do
    echo ":browse! $m"
done | cabal repl > base-types.txt

#python tool/clean_api.py

#python tool/sqlite_api.py

python tool/sqlite_class.py

exit

for m in $(ghc-pkg field base exposed-modules | sed 's/exposed-modules: //'); do
    echo ":browse $m"
done | cabal repl > base-types.txt
exit

# 1. baseパッケージから公開モジュール一覧を取得し、カンマを削除してスペース区切りにする
#MODULES=$(ghc-pkg field base exposed-modules --simple-output)
MODULES=$(ghc-pkg field base exposed-modules --simple-output | sed 's/,//g')

# 2. GHCi（cabal repl）に渡すためのコマンド群を一時ファイルに書き出す
# 各モジュールをインポートした後に :browse を実行する
cat <<EOF > .gen_ghci_script
:set -fno-print-explicit-foralls
:set -fno-print-explicit-kinds
EOF

for mod in $MODULES; do
    #echo "putStrLn \"--- Module: $mod ---\"" >> .gen_ghci_script
    echo $mod
    echo ":browse $mod" | cabal repl >> types_list.txt 2> /dev/null
done

# 3. cabal repl を起動し、生成したスクリプトを読み込ませる
# 実行結果を型情報リスト(types_list.txt)に保存する

# 一時ファイルを削除
#rm .gen_ghci_script
wc types_list.txt

echo "Done! Check types_list.txt"
exit

#!/bin/bash

FILE_TYPE="typeallapi.tsv"
rm $FILE_TYPE
ghc-pkg field base exposed-modules > tmp.txt
cat tmp.txt

exit
for file in "$TARGET_DIR"/*.hs; do
    echo "Processing $file ..."
    echo ":browse $name" | cabal repl >> functions.txt
done
