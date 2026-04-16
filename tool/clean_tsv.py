import re

def remove_meta_infox(typ):
    # メタ情報は "]" や ")" の後に続く "[" で始まる
    # つまり、型の終わりを探してそこから後ろを削除する
    for i in range(len(typ)):
        if typ[i] == '[':
            # 型の中のリストは直前が "(" か "," か " "
            # メタ情報は直前が ")" または "]"
            if i > 0 and typ[i-1] in ")]":
                return typ[:i].rstrip()
    return typ

def remove_meta_info(typ):
    tags = [
        '[HasNoCafRefs,',
        '[LambdaFormInfo:',
        '[Arity:',
        '[Unfolding:',
        '[TagSig:',
        '[Strictness:',
        '[CPR:'
    ]

    cut_pos = len(typ)
    for tag in tags:
        pos = typ.find(tag)
        if pos != -1:
            cut_pos = min(cut_pos, pos)

    return typ[:cut_pos].rstrip()

def remove_module_names(typ):
    # 文字列を1文字ずつ走査して、モジュール名を安全に削除する
    result = ""
    token = ""
    for ch in typ:
        if ch.isalnum() or ch in "._'":
            token += ch
        else:
            # トークンがモジュール名を含む場合
            if "." in token:
                token = token.split(".")[-1]
            result += token + ch
            token = ""
    # 最後のトークン処理
    if token:
        if "." in token:
            token = token.split(".")[-1]
        result += token
    return result

def simplify_type(typ):
    # --- 1. GHC のメタ情報を削除 ---
    typ = remove_meta_info(typ)

    # --- 2. モジュール名の除去 ---
    typ = remove_module_names(typ)

    # --- 3. 余計な空白を削除（行頭の空白は削除しない） ---
    typ = " ".join(typ.split())

    # --- 4. タプルの空白整形 ---
    typ = typ.replace("( ", "(").replace(" )", ")")
    typ = typ.replace(" ,", ",").replace(", ", ", ")
    typ = typ.replace("(,", "(").replace(",)", ")")

    # --- 5. リストの空白整形 ---
    typ = typ.replace("[ ", "[").replace(" [", "[")
    typ = typ.replace(" ]", "]").replace("] ", "]")

    # --- 6. 関数矢印の空白統一 ---
    typ = typ.replace("->", " -> ")

    # --- 7. 型末尾の [] を削除 ---
    if typ.endswith("[]"):
        typ = typ[:-2].strip()

    typ = " ".join(typ.split())

    return typ


def main():
    skiptags = [
        "GHC.Prim.RealWorld",
        "GHC.Prim.State#",
        "GHC.Prim.Addr#",
        "Pred",
        "=>",
        "InferM"
    ]

    with open("types.tsv", encoding="utf-8") as f:
        lines = f.readlines()

    with open("types_cleaned.tsv", "w", encoding="utf-8") as out:
        out.write("Name\tType\n")
        for line in lines:
            if "\t" not in line:
                continue
            name, typ = line.strip().split("\t", 1)

            # --- Addr# の行はスキップ ---
            skipout = False
            for tag in skiptags:
              if typ.find(tag) >= 0:
                skipout = True
                break

            if skipout:
                continue

            typ = simplify_type(typ)
            out.write(f"{name}\t{typ}\n")


if __name__ == "__main__":
    main()

"""
def simplify_type(typ):
    # メタ情報の除去（角括弧で始まるタグを分割して削除）
    for tag in ['[LambdaFormInfo:', '[Arity:', '[Unfolding:', '[TagSig:', '[Strictness:', '[CPR:']:
        if tag in typ:
            typ = typ.split(tag)[0].strip()

    # 空の [] を削除（末尾にある場合）
    if typ.endswith("[]"):
        typ = typ[:-2].rstrip()

    # GHCのモジュール名を除去（例: GHC.Base.String → String）
    patterns = [
        ('\\b(?:[\\w]+\\.)+([A-Z][\\w\']*)', r'\1'),  # 大文字識別子
        ('\\b(?:[\\w]+\\.)+([a-z][\\w\']*)', r'\1'),  # 小文字識別子
    ]

    for pattern, repl in patterns:
        typ = re.sub(pattern, repl, typ)

    return typ

def main():
    with open("types.tsv", encoding="utf-8") as f:
        lines = f.readlines()

    with open("types_cleaned.tsv", "w", encoding="utf-8") as out:
        out.write("Name\tType\n")
        for line in lines:
            if "\t" not in line:
                continue
            name, typ = line.strip().split("\t", 1)
            typ = simplify_type(typ)
            out.write(f"{name}\t{typ}\n")

if __name__ == "__main__":
    main()
"""