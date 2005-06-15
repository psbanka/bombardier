#!/bin/bash

./preCompile.sh
scp release/bombardier.tar.gz release/bombardier.md5 root@172.17.50.56:/var/www/deploy
