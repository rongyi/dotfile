#!/usr/bin/python
# -*- coding: utf-8 -*-

"""Disable a notebook's main keyboard:
http://askubuntu.com/questions/160945/is-there-a-way-to-disable-a-laptops-internal-keyboard,
"""

import os
import sys
import shlex
import subprocess
import select


MAIN_KEYBOARD_CHARACTERISTIC = 'AT Translated Set 2 keyboard'
LIST_CMD = 'xinput list'
DISABLE_CMD_TMPL = 'xinput float %d'


def parse_list(raw):
    """parse output of list command"""
    lines = [line.strip() for line in raw.split('\n') if line.strip() and MAIN_KEYBOARD_CHARACTERISTIC in line]
    if len(lines) == 0:
        return False
    the_line = lines[0]
    id = the_line[the_line.find('id'):the_line.find('\t', the_line.find('id'))]
    if not id:
        return False
    the_id = int(id.split('=')[1])
    master = the_line[the_line.find('['):].strip('[]')
    master_lst = master.split(' ')
    master_number = int(master_lst[len(master_lst) - 1].strip('()'))
    return (the_id, master_number)


def run_cmd(cmd, live=False, readsize=10):
    # readsize = 10
    cmdargs = shlex.split(cmd)
    p = subprocess.Popen(cmdargs,
                         stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE)

    stdout = ''
    stderr = ''
    rpipes = [p.stdout, p.stderr]
    while True:
        rfd, wfd, efd = select.select(rpipes, [], rpipes, 1)

        if p.stdout in rfd:
            dat = os.read(p.stdout.fileno(), readsize)
            if live:
                sys.stdout.write(dat)
            stdout += dat
            if dat == '':
                rpipes.remove(p.stdout)
        if p.stderr in rfd:
            dat = os.read(p.stderr.fileno(), readsize)
            stderr += dat
            if live:
                sys.stdout.write(dat)
            if dat == '':
                rpipes.remove(p.stderr)
        # only break out if we've emptied the pipes, or there is nothing to
        # read from and the process has finished.
        if (not rpipes or not rfd) and p.poll() is not None:
            break
        # Calling wait while there are still pipes to read can cause a lock
        elif not rpipes and p.poll() == None:
            p.wait()

    return p.returncode, stdout, stderr


if __name__ == '__main__':
    rc, stdout, _ = run_cmd(LIST_CMD)
    if rc != 0:
        sys.stderr.write('get xinput fail')
        sys.exit(-1)
    parse_ret = parse_list(stdout)
    if not parse_ret:
        sys.stderr.write('\n')
        sys.exit(-1)
    id = parse_ret[0]
    rc, _, _ = run_cmd(DISABLE_CMD_TMPL % id)
    if rc == 0:
        sys.stdout.write('disable main keyboard success\n')
    else:
        sys.stderr.write('disable main keyboard fail\n')
        sys.exit(-1)
