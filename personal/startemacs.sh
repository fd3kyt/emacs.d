#!/bin/bash
env LANG="zh_CN.UTF-8" emacs --daemon --no-desktop
emacsclient -c -n -e <<EOF
"(load-file \"/home/fd3kyt/.emacs.d/kyt/load/init-local-font.el\")"
EOF
