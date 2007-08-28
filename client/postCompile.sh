#!/bin/bash
set +x

cp Output/bombardierSetup-$BOMBARDIER_VERSION*.exe release
scp release/bombardier-$BOMBARDIER_VERSION*.tar.gz release/bombardierSetup-$BOMBARDIER_VERSION*.exe `cat release/server.txt`

