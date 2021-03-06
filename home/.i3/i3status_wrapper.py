#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# This script is a simple wrapper which prefixes each i3status line with custom
# information. It is a python reimplementation of:
# http://code.stapelberg.de/git/i3status/tree/contrib/wrapper.pl
#
# To use it, ensure your ~/.i3status.conf contains this line:
#     output_format = "i3bar"
# in the 'general' section.
# Then, in your ~/.i3/config, use:
#     status_command i3status | ~/i3status/contrib/wrapper.py
# In the 'bar' section.
#
# In its current version it will display the cpu frequency governor, but you
# are free to change it to display whatever you like, see the comment in the
# source code below.
#
# © 2012 Valentin Haenel <valentin.haenel@gmx.de>
#
# This program is free software. It comes without any warranty, to the extent
# permitted by applicable law. You can redistribute it and/or modify it under
# the terms of the Do What The Fuck You Want To Public License (WTFPL), Version
# 2, as published by Sam Hocevar. See http://sam.zoy.org/wtfpl/COPYING for more
# details.

import sys
import json
import subprocess


# This first stuff was by the original author.
def get_governor():
    """ Get the current governor for cpu0, assuming all CPUs use the same. """
    with open('/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor') as fp:
        return fp.readlines()[0].strip()

def print_line(message):
    """ Non-buffered printing to stdout. """
    sys.stdout.write(message + '\n')
    sys.stdout.flush()

def read_line():
    """ Interrupted respecting reader for stdin. """
    # try reading a line, removing any extra whitespace
    try:
        line = sys.stdin.readline().strip()
        # i3status sends EOF, or an empty line
        if not line:
            sys.exit(3)
        return line
    # exit on ctrl-c
    except KeyboardInterrupt:
        sys.exit()

# Stuff after this was added and modified Tyrus.
def handle_line():
    line, prefix = read_line(), ''
    # ignore comma at start of lines
    if line.startswith(','):
        line, prefix = line[1:], ','

    j = json.loads(line)
    # insert information into the start of the json, but could be anywhere
    for s in j:
        if s['name'] == 'volume':
            i = playerctl_info()
            if i:
                s['full_text'] += ' ' + i if i else ''
    # and echo back new encoded json
    print_line(prefix+json.dumps(j))


def song_info():
    cmd =  (
        "xwininfo -tree -root "
        " | grep -P '\- (?:Google Play|YouTube) Music'"
        " | sed 's/[^\"]*\"\\(.*\\) - \\(Google Play\|YouTube\\) Music.*/\\1/'"
    )
    p = subprocess.Popen(cmd, stdout=subprocess.PIPE, shell=True)
    stdout, _ = p.communicate()
    info = str.rstrip(stdout.decode('utf-8'))
    if info != "Home":
      return info
    return None


def playerctl_info():
    delim = "!tyrus!"
    format_str = delim.join(("{{artist}}", "{{title}}", "{{status}}"))
    cmd = "playerctl metadata --format '{}'".format(format_str)

    p = subprocess.Popen(cmd, stdout=subprocess.PIPE, shell=True)
    stdout, _ = p.communicate()
    info = str.rstrip(stdout.decode('utf-8'))
    if not info:
        return None
    artist, title, status = info.split(delim)
    if "- Twitch" in title:
        return None
    # Youtube Music adds "- Topic"
    artist = artist.replace(" - Topic", "")
    # Bandcamp adds the play icon when playing
    title = title.replace("▶", "").strip()
    status = "⏵" if status == "Playing" else "⏸"
    now_playing = " - ".join(filter(None, (title, artist)))
    return status + now_playing


if __name__ == '__main__':
    # Skip the first line which contains the version header.
    print_line(read_line())

    # The second line contains the start of the infinite array.
    print_line(read_line())

    while True:
        handle_line()
