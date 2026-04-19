import re
import sqlite3

DB = "tool/typeDB.sqlite"

def parse_classes(path):
    # class Foo a
    # class C a => Foo a
    pattern = re.compile(r"^class\s+(?:.*=>\s+)?([A-Za-z0-9_']+)\b")

    results = []

    with open(path, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()

            m = pattern.match(line)
            if m:
                class_name = m.group(1)   # Applicative, Monad, Eq, etc.
                class_def  = line
                results.append((class_name, class_def))

    return results


def save_classes_to_db(classes):
    conn = sqlite3.connect(DB)
    cur = conn.cursor()

    for name, class_def in classes:
        cur.execute(
            "INSERT INTO items (module, name, type, kind) VALUES (?, ?, ?, ?)",
            ("", name, class_def, "class")
        )

    conn.commit()
    conn.close()


if __name__ == "__main__":
    print("[CLASS] Parsing browse_all.txt...")
    classes = parse_classes("tool/browse_all.txt")
    print(f"[CLASS] Found {len(classes)} classes")

    save_classes_to_db(classes)
    print("[CLASS] Saved to DB.")
