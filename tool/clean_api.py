import re

def parse_browse_output(path):
    """
    :browse の出力をパースして
    (module, name, type) のリストを返す。
    モジュール名が省略されている行は前のモジュール名を継承する。
    """
    pattern = re.compile(r"^(?:(?P<module>[^:]+):)?(?P<name>[^\s]+)\s+::?\s+(?P<type>.+)$")

    results = []
    current_module = ""

    with open(path, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()

            # 空行やプロンプトをスキップ
            if not line or line.startswith("*") or line.startswith("Prelude"):
                continue

            m = pattern.match(line)
            if m:
                module = m.group("module")
                name = m.group("name")
                typ = m.group("type")

                # モジュール名があれば更新
                if module:
                    current_module = module

                # モジュール名が空なら継承
                results.append((current_module, name, typ))

    return results


def save_tsv(items, out="typeDBapi.tsv"):
    with open(out, "w", encoding="utf-8") as f:
        for module, name, typ in items:
            f.write(f"{module}\t{name}\t{typ}\n")


if __name__ == "__main__":
    items = parse_browse_output("base-types.txt")
    save_tsv(items)
    print("Extracted:", len(items))
