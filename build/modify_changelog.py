#!/usr/bin/env python

import os, sys, re, fileinput
CHANGE_LOG = "CHANGELOG"
TMP_LOG = "tmp_log"


def word_wrapper( word_list ):
    MAX_WIDTH = 60
    wrapped_list = []
    current_line = ''
    for word in word_list:
        if len(current_line + word) < MAX_WIDTH - 1:
            current_line = "%s %s" % (current_line, word )
            continue

        if current_line:
            wrapped_list.append(current_line)

        current_line = word
    wrapped_list.append(current_line)
    
    return wrapped_list

def get_version():
    revno_re = re.compile( ' *(\d+):')
    versions = []
    for line in fileinput.input( CHANGE_LOG ):
        versions.append( revno_re.findall( line ) )
    if versions:
        version = int(max(versions)[0])
    return version
        

def main():
    version = get_version()

    log_options = "--forward --line --include-merges"
    awk_strip = "awk '$0 !~ /^$/ {print $0}'"
    log_cmd = "bzr log %s -r%s.. | %s > %s"

    log_cmd = log_cmd % (log_options, version, awk_strip, TMP_LOG)
    os.system(log_cmd)


    new_log_section = "\n"

    for line in fileinput.input(TMP_LOG):
        line = line.strip()
        split_line = line.split(' ')
        stanza = ' '.join([split_line[0], split_line[2], "<%s>" % split_line[1]])
        wrapped_comment = word_wrapper( split_line[3:] )
        stanza = stanza + "\n   " + "\n    ".join( wrapped_comment ) + '\n\n'

        new_log_section += stanza

    open(CHANGE_LOG, "a").write( new_log_section )

if __name__ == '__main__':
    main()
