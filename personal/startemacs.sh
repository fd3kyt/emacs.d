#!/bin/bash
#env PATH=~/anaconda3/bin:$PATH LANG="zh_CN.UTF-8" emacs --daemon # --no-desktop # --debug-init

echo "@@@@ Note: if you are debugging emacs,"
echo "@@@@ run 'emacs' directly instead of using this script."
echo "@@@@ even better: ~/.emacs.d/test-startup.sh"

LOCK_PATH=~/.emacs.d/.emacs.desktop.lock
if [ -f ${LOCK_PATH} ]; then
    echo "Maybe you need to remove ${LOCK_PATH} manually."
    sleep 3
fi
LANG="zh_CN.UTF-8" emacs --daemon # --no-desktop # --debug-init
emacsclient -c -n
