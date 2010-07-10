#!/usr/bin/env python

"Prepare the content on the website based on the last build"
import yaml
import fileinput
import re
import sys

TEMPLATE = "##TEMPLATE##"

def change_log():
    "Get a data structure of the change log"
    change_data = []
    current_change_data = []
    heading_match = re.compile("(\d+)\:\s(\d+\-\d+\-\d+)\s\<(\S+)\>")
    version = 0
    date = ''
    username = ''
    for line in fileinput.input( "../build/CHANGELOG" ):
        line = line.strip()
        matches = heading_match.findall(line)
        if matches:
            if version:
                change_data.append((version, date, username, current_change_data))
            version = int(matches[0][0])
            date = matches[0][1]
            username = matches[0][2]
            current_change_data = []
        else:
            if version:
                current_change_data.append(line)

    change_data.append((version, date, username, current_change_data))
    return change_data

def check_change_date(change_data):
    "find the latest change"
    latest_record = change_data[-1]
    version, date, username, change_lines = latest_record
    return version, date 

def modify_header(filename, version, date):
    "Modify HTML header with updated info"
    new_filename = filename.rpartition(".template")[0] + ".html"
    output_lines = []
    version_string = "1.00-%s (%s)" % (version, date)
    print(version_string)
    template_found = False
    for line in fileinput.input( filename ):
        if not TEMPLATE in line:
            output_lines.append(line)
        else:   
            template_found = True
            front, middle, end = line.rpartition(TEMPLATE)
            output_lines.append( ''.join([front, version_string, end]) )
    if template_found:
        print("Writing %s..." % new_filename)
        open(new_filename, 'w').write(''.join(output_lines))
    else:
        print ("Error: TEMPLATE string not found in %s" % filename)
        sys.exit(1)

def main():
    "main method"
    change_data = change_log()
    last_version, last_date = check_change_date(change_data)
    modify_header("header.template", last_version, last_date)
    modify_header("page_outline.template", last_version, last_date)

if __name__ == "__main__":
    main()
