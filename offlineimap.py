#! /usr/bin/env python2

from subprocess import check_output

def get_pass(account):
    return check_output("pass " + account, shell=True).splitlines()[0]

def get_user(account):
    return check_output("pass " + account, shell=True).splitlines()[1][7:]
