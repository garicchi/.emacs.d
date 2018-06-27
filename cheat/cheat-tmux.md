# TMUX CHEAT SHEET

## セッション操作
- 新規セッション開始 tmux new -s {セッション名}
- セッション一覧 tmux ls
- セッション再開 tmux a -t {セッション名}
- セッション終了 tmux kill-session -t {セッション名}
- tmux 終了 "tmux kill-server"

## セッション内操作
- セッション離脱 Ctrl+b d
- セッション一覧選択 Ctrl+b s
- セッション名前変更 Ctrl+b $
- ペイン分割 (左右) Ctrl+b %
- ペイン分割 (上下)" "Ctrl+b "
