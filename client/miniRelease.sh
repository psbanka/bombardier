#!/bin/bash

./preCompile.sh
scp release/bombardier.tar.gz release/bombardier.md5 `cat release/server.txt`
