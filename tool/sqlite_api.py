import re
import sqlite3

DB_PATH = "typeDB.sqlite"

# SQLite 初期化
def init_db():
    conn = sqlite3.connect(DB_PATH)
    cur = conn.cursor()

    cur.execute("""
        CREATE TABLE IF NOT EXISTS items (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            module TEXT,
            name TEXT,
            type TEXT,
            kind TEXT
        )
    """)

    conn.commit()
    conn.close()


# :browse の出力をパース
def parse_browse_output(path):
    pattern = re.compile(
        r"^(?:(?P<module>[^:]+):)?(?P<name>[^\s]+)\s+::?\s+(?P<type>.+)$"
    )

    results = []
    current_module = ""

    with open(path, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()

            if not line or line.startswith("*") or line.startswith("Prelude"):
                continue

            m = pattern.match(line)
            if m:
                module = m.group("module")
                name = m.group("name")
                typ = m.group("type")

                if module:
                    current_module = module

                # 種類判定
                if typ.startswith("class "):
                    kind = "class"
                elif typ.startswith("data "):
                    kind = "data"
                elif "=>" in typ:
                    kind = "function"  # 制約つき関数
                else:
                    kind = "function"

                results.append((current_module, name, typ, kind))

    return results


# SQLite に保存
def save_to_db(items):
    conn = sqlite3.connect(DB_PATH)
    cur = conn.cursor()

    cur.executemany(
        "INSERT INTO items (module, name, type, kind) VALUES (?, ?, ?, ?)",
        items
    )

    conn.commit()
    conn.close()


if __name__ == "__main__":
    init_db()

    items = parse_browse_output("base-types.txt")
    save_to_db(items)

    print("Saved:", len(items), "items to SQLite.")
