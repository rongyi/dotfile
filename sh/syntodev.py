#!/usr/bin/python
# -*- coding: utf-8 -*-
import os
import sys
import subprocess
import shlex

REMOTE_HOME = '/home/rongyi'

if __name__ == '__main__':
    argc = len(sys.argv)
    if argc == 1:
        print('need file or dir to copy to remote')
        sys.exit(-1)

    local_home = os.path.expanduser('~')
    cwd = os.getcwd()
    if cwd.startswith(local_home):
        rela_cwd = cwd[len(local_home) + 1:]
        remote_path = os.path.join(REMOTE_HOME, rela_cwd)
    else:
        remote_path = cwd
    cmd_tmpl = 'scp -r %s rongyi@XXX.XXX.X.XXX:%s'
    cmds = []
    for i in xrange(argc - 1):
        cmds.append(cmd_tmpl % (sys.argv[i + 1], remote_path))

    for cmd in cmds:
        print(cmd)
        subprocess.call(shlex.split(cmd))
