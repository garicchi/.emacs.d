# MYSQL CHEAT SHEET
##  データ操作
- データSELECT(キャッシュなし) select SQL_NO_CACHE * from {table name}
- データINSERT insert into {table name} ({type1},{type2}) values({v1},{v2})
- データINSERT insert into {table name} values({v1},{v2})
- データUPDATE update {table name} set {col name}={value}
- データDELETE delete from {table name}

## Table操作
- テーブル作成 create table \`{table name}\`(\`id\` int not null primary key, \`name\` varchar(10))

## カラム操作
- カラム追加 alter table {table name} add {column name} {column type}
- カラム削除 alter table {table name} drop {column name}
