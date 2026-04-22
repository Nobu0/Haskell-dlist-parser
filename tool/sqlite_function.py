import re
import sqlite3

DB = "tool/typeDB.sqlite"

def parse_functions(path):
    # モジュール名付き関数にも対応
    # 例: Data.Foldable.toList :: Foldable t => t a -> [a]
    pattern = re.compile(r"^([A-Za-z0-9_.']+)\s*::\s*(.+)$")

    results = []

    with open(path, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()

            m = pattern.match(line)
            if m:
                full_name = m.group(1)      # Data.Foldable.toList
                type_sig = m.group(2)

                parts = full_name.split(".")
                if len(parts) > 1:
                    module = ".".join(parts[:-1])   # Data.Foldable
                    name = parts[-1]                # toList
                else:
                    module = ""
                    name = full_name

                results.append((module, name, type_sig))

    return results


def save_functions_to_db(funcs):
    conn = sqlite3.connect(DB)
    cur = conn.cursor()

    for module, name, type_sig in funcs:
        cur.execute(
            "INSERT INTO items (module, name, type, kind) VALUES (?, ?, ?, ?)",
            (module, name, type_sig, "function")
        )

    conn.commit()
    conn.close()


if __name__ == "__main__":
    print("[FUNC] Parsing browse_fun.txt...")
    funcs = parse_functions("tool/browse_fun.txt")
    print(f"[FUNC] Found {len(funcs)} functions")

    save_functions_to_db(funcs)
    print("[FUNC] Saved to DB.")

