#!/bin/bash
set +x

cp Output/prereqSetup-$PREREQ_VERSION*.exe release
scp release/prereq-$PREREQ_VERSION*.tar.gz release/prereqSetup-$PREREQ_VERSION*.exe `cat release/server.txt`

