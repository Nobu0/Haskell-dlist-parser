#!/bin/bash

set -e



python tool/sqlite_init.py

#python tool/sqlite_class.py
python tool/sqlite_all.py

python tool/sqlite_instance.py

python tool/sqlite_function.py

sqlite3 tool/typeDB.sqlite "CREATE INDEX idx_items_kind ON items(kind);"
sqlite3 tool/typeDB.sqlite "CREATE INDEX idx_items_name ON items(name);"

sqlite3 tool/typeDB.sqlite "SELECT kind,name,type,id,module FROM items ORDER BY name;" > tool/all.txt
#exit
sqlite3 tool/typeDB.sqlite "SELECT kind,name,type,id,module FROM items WHERE kind='function' ORDER BY name;" > tool/fun.txt
sqlite3 tool/typeDB.sqlite "SELECT kind,name,type,id,module FROM items WHERE kind='instance' ORDER BY name;" > tool/ins.txt
sqlite3 tool/typeDB.sqlite "SELECT kind,name,type,id,module FROM items WHERE kind='class' ORDER BY name;" > tool/cls.txt
sqlite3 tool/typeDB.sqlite "SELECT kind,name,type,id,module FROM items WHERE kind like '%type%' ORDER BY name;" > tool/typ.txt
sqlite3 tool/typeDB.sqlite "SELECT kind,name,type,id,module FROM items WHERE kind like 'data' ORDER BY name;" > tool/dat.txt

