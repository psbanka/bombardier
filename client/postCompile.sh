#!/bin/bash
set +x

cp Output/bombardierSetup-0.4.exe release/bombardierSetup-0.4.exe
scp release/bombardier-0.4.tar.gz release/bombardierSetup-0.4.exe `cat release/server.txt`

