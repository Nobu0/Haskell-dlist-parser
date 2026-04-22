import sqlite3

DB = "tool/typeDB.sqlite"

conn = sqlite3.connect(DB)
cur = conn.cursor()

cur.execute("DROP TABLE items")

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

print("[DB] Initialized.")
