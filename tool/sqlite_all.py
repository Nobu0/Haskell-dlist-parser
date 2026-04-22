import re
import sqlite3

DB = "tool/typeDB.sqlite"

re_skip = re.compile(r"-- imported .*")
re_module = re.compile(r"--- Package: .* \| Module: ([A-Za-z0-9_.]+) ---")
re_class_start = re.compile(r"^class\s+(?:.*=>\s+)?([A-Za-z0-9_']+)")
re_instance_start = re.compile(r"^(?:data\s+instance|newtype\s+instance|instance)\s+(.+)")
re_sig_start = re.compile(r"^([^ ]+)\s::")
re_type_xxx_start = re.compile(r"^type\s+([a-z]+)\s+([^ ]+)")
re_type_kind_start   = re.compile(r"^type\s+([^ ]+)[^:]+\s*::")
re_type_synonym_start = re.compile(r"^type\s+([^ ]+)\b")
re_data_start = re.compile(r"^data\s+([A-Za-z0-9_']+)")
re_newtype_start = re.compile(r"^newtype\s+([A-Za-z0-9_']+)")


def clean(line):
    if line.startswith("ghci>"):
        return line.replace("ghci>", "").strip()
    return line.strip()

def parse_browse_all(path):
    results = []
    current_module = ""

    with open(path, "r", encoding="utf-8") as f:
        buffer = ""
        buffer_kind = None
        buffer_name = ""

        for raw in f:
            line = clean(raw)
            if not line:
                continue

            m = re_skip.match(line)
            if m:
                continue

            # type family
            m = re_type_xxx_start.match(line)
            if m:
                if buffer:
                    results.append((current_module, buffer_name, buffer, buffer_kind))
                buffer = line
                buffer_kind = "type-" + m.group(1)
                buffer_name = m.group(2)
                continue

            # type kind signature
            m = re_type_kind_start.match(line)
            if m:
                if buffer:
                    results.append((current_module, buffer_name, buffer, buffer_kind))
                buffer = line
                buffer_kind = "type-kind"
                buffer_name = m.group(1)
                continue

            # type synonym
            m = re_type_synonym_start.match(line)
            if m:
                if buffer:
                    results.append((current_module, buffer_name, buffer, buffer_kind))
                buffer = line
                buffer_kind = "type-synonym"
                buffer_name = m.group(1)
                continue
            # data
            m = re_data_start.match(line)
            if m:
                if buffer:
                    results.append((current_module, buffer_name, buffer, buffer_kind))
                buffer = line
                buffer_kind = "data"
                buffer_name = m.group(1)
                continue

            # newtype
            m = re_newtype_start.match(line)
            if m:
                if buffer:
                    results.append((current_module, buffer_name, buffer, buffer_kind))
                buffer = line
                buffer_kind = "newtype"
                buffer_name = m.group(1)
                continue

            # モジュール行
            m = re_module.match(line)
            if m:
                if buffer:
                    results.append((current_module, buffer_name, buffer, buffer_kind))
                current_module = m.group(1)
                buffer = ""
                buffer_kind = None
                buffer_name = ""
                continue

            # class
            m = re_class_start.match(line)
            if m:
                if buffer:
                    results.append((current_module, buffer_name, buffer, buffer_kind))
                buffer = line
                buffer_kind = "class"
                buffer_name = m.group(1)
                continue

            # instance
            m = re_instance_start.match(line)
            if m:
                if buffer:
                    results.append((current_module, buffer_name, buffer, buffer_kind))
                buffer = line
                buffer_kind = "instance"
                buffer_name = m.group(1).split()[0]  # Ord, Eq, Show など
                continue

            # function
            m = re_sig_start.match(line)
            if m:
                if buffer:
                    results.append((current_module, buffer_name, buffer, buffer_kind))
                buffer = line
                buffer_kind = "function"
                full_name = m.group(1)
                buffer_name = full_name.split(".")[-1]
                continue

            # 継続行
            if buffer:
                buffer += " " + line
                continue

        # 最後のバッファ
        if buffer:
            results.append((current_module, buffer_name, buffer, buffer_kind))

    return results


def save_to_db(items):
    conn = sqlite3.connect(DB)
    cur = conn.cursor()

    for module, name, type_sig, kind in items:
        cur.execute(
            "INSERT INTO items (module, name, type, kind) VALUES (?, ?, ?, ?)",
            (module, name, type_sig, kind)
        )

    conn.commit()
    conn.close()


if __name__ == "__main__":
    print("[PARSE] Reading browse_all.txt...")
    items = parse_browse_all("tool/browse_all.txt")
    print(f"[PARSE] Found {len(items)} items")

    save_to_db(items)
    print("[DONE] Saved all items to DB.")
