#!/bin/bash
#env PATH=/home/fd3kyt/anaconda3/bin:$PATH LANG="zh_CN.UTF-8" emacs --daemon # --no-desktop # --debug-init
LANG="zh_CN.UTF-8" emacs --daemon # --no-desktop # --debug-init
emacsclient -c -n
