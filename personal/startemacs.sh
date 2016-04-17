#!/bin/bash
env LANG="zh_CN.UTF-8" emacs --daemon --no-desktop # --debug-init
emacsclient -c -n
sleep 2
emacsclient -e <<EOF
"(load-file \"/home/fd3kyt/.emacs.d/personal/load/init-local-font.el\")"
EOF
