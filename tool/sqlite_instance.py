import subprocess
import sqlite3
import re

DB = "tool/typeDB.sqlite"


def get_class_info(class_name):
    print(f"[INFO] :info {class_name}")

    # ghci を起動
    p = subprocess.Popen(
        ["ghci"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        universal_newlines=True
    )

    # Prelude をロードさせる
    p.stdin.write(":module +Prelude\n")

    # :info を送る
    p.stdin.write(f":info {class_name}\n")

    # 終了
    p.stdin.write(":quit\n")
    p.stdin.flush()

    out, err = p.communicate()

    if err:
        print("[WARN]", err.strip())

    return out

def get_all_classes():
    conn = sqlite3.connect(DB)
    cur = conn.cursor()
    cur.execute("SELECT DISTINCT name FROM items WHERE kind='class'")
    classes = [row[0] for row in cur.fetchall()]
    conn.close()
    return classes


def parse_instances(info_text):
    pattern = re.compile(r"instance\s+(.+)")
    return [m.group(1) for m in map(pattern.match, info_text.splitlines()) if m]


def save_instances_to_db(class_name, instances):
    conn = sqlite3.connect(DB)
    cur = conn.cursor()

    for inst in instances:
        cur.execute(
            "INSERT INTO items (module, name, type, kind) VALUES (?, ?, ?, ?)",
            ("", class_name, inst, "instance")
        )

    conn.commit()
    conn.close()


if __name__ == "__main__":
    classes = get_all_classes()
    print(f"[INSTANCE] Found {len(classes)} classes")

    for cls in classes:
        print(f"[PROCESS] {cls}")
        info = get_class_info(cls)
        instances = parse_instances(info)
        print(f"[FOUND] {len(instances)} instances")

        save_instances_to_db(cls, instances)

    print("[DONE] All instances saved.")
