import sys
import json
import re
from collections import defaultdict
import os

def normalize_name(name):
    # GHCの内部名を正規化
    name = re.sub(r'^\$w+', '', name)         # $wfoo → foo
    name = re.sub(r'^\$+', '', name)          # $foo → foo
    name = re.sub(r'_[0-9]+$', '', name)      # foo_1 → foo
    return name

def is_exported(name, exports):
    norm_name = normalize_name(name)
    return any(norm_name == normalize_name(exp) for exp in exports)


#def is_exported(name, exports):
#    return any(name == exp or exp.endswith("." + name) for exp in exports)

def parse_exports_from_file(path):
    with open(path, encoding='utf-8') as f:
        lines = f.readlines()

    exports = set()
    collecting = False
    for line in lines:
        if "exports:" in line:
            print(f"[DEBUG] Found 'exports:' in {path}")
            collecting = True
            continue
        if collecting:
            if line.startswith(" ") or line.startswith("\t"):
                raw = line.strip()
                print(f"  Export: {raw}")
                # パターン1: 単一の名前
                if "{" not in raw:
                    exports.add(raw)
                else:
                    # パターン2: Name{A B C}
                    m = re.match(r"(\w+)\{(.+)\}", raw)
                    if m:
                        type_name = m.group(1)
                        constructors = m.group(2).split()
                        exports.add(type_name)
                        for ctor in constructors:
                            exports.add(ctor)
            else:
                break
    if not exports:
        print(f"[DEBUG] No exports found in {path}")
    return exports

def expand_args(args):
    expanded = []
    for arg in args:
        if arg.startswith("@"):
            list_path = arg[1:]
            with open(list_path, encoding='utf-8') as f:
                expanded.extend(line.strip() for line in f if line.strip())
        else:
            expanded.append(arg)
    return expanded

import re

def parse_type_sigs(path):
    with open(path, encoding='utf-8') as f:
        lines = f.readlines()

    sigs = {}
    valid_name = re.compile(r'^[a-zA-Z_][\w\'.]*$')
    i = 0

    while i < len(lines):
        line = lines[i]
        if "::" in line:
            parts = line.strip().split("::", 1)
            name = parts[0].strip()
            typ = parts[1].strip()
            i += 1

            # 継続行を結合（空行やインデント行を含む）
            while i < len(lines):
                next_line = lines[i]
                if next_line.strip() == "":
                    i += 1
                    continue
                if not next_line.startswith(" ") and not next_line.startswith("\t"):
                    break
                typ += " " + next_line.strip()
                i += 1

            # メタ情報を除去
            for tag in ['[LambdaFormInfo:', '[Arity:', '[Unfolding:']:
                if tag in typ:
                    typ = typ.split(tag)[0].strip()

            if valid_name.match(name):
                sigs[name] = typ
        else:
            i += 1

    print(f"[INFO] Found {len(sigs)} type signatures in {path}")
    return sigs

def main():
    print(f"[DEBUG] Files to process: {sys.argv[1:]}")
    args = expand_args(sys.argv[1:])
    #print(f"[DEBUG] Files to process: {args}")
    if not args:
        print("Usage: python extract_exported_types.py <dump-hi> [more ...]")
        return

    all_exports = set()
    all_type_sigs = {}

    # 1. すべてのファイルから exports を集める
    for path in args:
        exports = parse_exports_from_file(path)
        all_exports.update(exports)

    # 2. すべてのファイルから type signatures を集める
    for path in args:
        sigs = parse_type_sigs(path)
        for name, typ in sigs.items():
            if name not in all_type_sigs:
                all_type_sigs[name] = typ

    # 3. 照合して出力
    for name, typ in all_type_sigs.items():
        if is_exported(name, all_exports):
            print(f"[MATCH] {name} :: {typ}")
            # 出力処理へ

    # 1回目：すべての exports を収集
    #for path in args: # sys.argv[1:]:
    #    exports = parse_exports_from_file(path)
    #    all_exports.update(exports)

    if not all_exports:
        print("[WARN] No exports found in any file.")
    else:
        print(f"[INFO] Total unique exports collected: {len(all_exports)}")

    # 2回目：すべての型シグネチャを収集
    for path in args: #sys.argv[1:]:
        sigs = parse_type_sigs(path)
        for name, typ in sigs.items():
          print(f"[DEBUG] Found: {name}")
          if is_exported(name, all_exports):
              print(f"[MATCH] {name} :: {typ}")
              all_type_sigs[name] = typ
        #for name, typ in sigs.items():
        #  if is_exported(name, all_exports):
        #      print(f"[MATCH] {name} matches export")
        #  else:
        #      print(f"[SKIP] {name} not in exports")
        #for name, typ in sigs.items():
        #    if is_exported(name, all_exports) and name not in all_type_sigs:
        #        all_type_sigs[name] = typ
        #        print(f"  Matched: {name} :: {typ}")
        #for name, typ in sigs.items():
        #    if name not in all_type_sigs:
        #        all_type_sigs[name] = typ
        #        print(f"  Found: {name} :: {typ}")
    #for path in args:
    #    sigs = parse_type_sigs(path)
    #    for name, typ in sigs.items():
    #        print(f"{name}\t{typ}")

    print(f"[INFO] Total matched type signatures: {len(all_type_sigs)}")
    for name, typ in all_type_sigs.items():
        print(f"{name}\t{typ}")

if __name__ == "__main__":
    main()
