#!/bin/bash
env LANG="zh_CN.UTF-8" emacs --daemon --no-desktop # --debug-init
emacsclient -c -n
