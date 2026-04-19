import os
import re
import json
import subprocess
import sys
from pathlib import Path

# --- 内部モジュールを除外するフィルタ ---
def is_external_module(module):
    internal_prefixes = [
        "AST.", "TypeInference.", "Parser.", "Lexer.", "Utils.", "Decl."
    ]
    return not any(module.startswith(p) for p in internal_prefixes)


# --- import 文からモジュール名を抽出 ---
def extract_import_modules(source_code: str):
    modules = re.findall(r"import\s+(qualified\s+)?([A-Za-z0-9_.]+)", source_code)
    return [m[1] for m in modules]


# --- GHC のパッケージ DB から .hs を探す ---
def find_hs_file(module_name):
    # モジュール名 → パス変換
    rel = module_name.replace(".", "/") + ".hs"

    # GHC のパッケージ DB を検索
    ghc_base = Path(os.environ.get("GHC_PACKAGE_PATH", ""))

    # Windows の場合、GHC の標準パスを追加
    search_paths = [
        Path("C:/ProgramData/chocolatey/lib/ghc"),
        Path("C:/Program Files/Haskell Platform"),
        Path("C:/sr"),
        ghc_base
    ]

    for base in search_paths:
        if not base.exists():
            continue
        for root, dirs, files in os.walk(base):
            p = Path(root) / rel
            if p.exists():
                return str(p)

    return None


# --- GHC に JSON を吐かせる ---
def compile_and_dump(hs_path):
    subprocess.run([
        "ghc",
        "-ddump-json",
        "-fforce-recomp",
        "-c",
        hs_path
    ])

    dump_path = hs_path.replace(".hs", ".dump-json")
    if os.path.exists(dump_path):
        return dump_path
    return None


# --- JSON からエクスポート一覧を取得 ---
def load_exports(json_path):
    with open(json_path, "r", encoding="utf-8") as f:
        data = json.load(f)

    exports = []
    if "exports" in data:
        for item in data["exports"]:
            if "name" in item and "type" in item:
                exports.append((item["name"], item["type"]))

    return exports


# --- typeDB.tsv に書き込む ---
def append_to_tsv(name, type_str, path="typeDB.tsv"):
    with open(path, "a", encoding="utf-8") as f:
        f.write(f"{name}\t{type_str}\n")


# --- メイン処理 ---
def main():
    if len(sys.argv) < 2:
        print("Usage: python get_api.py <source-file>")
        sys.exit(1)

    source_path = sys.argv[1]

    with open(source_path, "r", encoding="utf-8") as f:
        code = f.read()

    modules = extract_import_modules(code)
    modules = [m for m in modules if is_external_module(m)]

    print("Detected external modules:", modules)

    for module in modules:
        print(f"\nProcessing module: {module}")

        hs_path = find_hs_file(module)
        if not hs_path:
            print("  Could not find .hs file for module:", module)
            continue

        dump_path = compile_and_dump(hs_path)
        if not dump_path:
            print("  Could not generate JSON dump:", module)
            continue

        exports = load_exports(dump_path)
        if not exports:
            print("  No exports found.")
            continue

        for name, typ in exports:
            print(f"  {name} :: {typ}")
            append_to_tsv(name, typ)


if __name__ == "__main__":
    main()
