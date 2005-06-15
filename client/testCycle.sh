#!/bin/bash
./preCompile.sh
cd scratch || exit 1
rm -rf *
cp ../release/bombardier-0.4.tar.gz .
../spkgDir/rescue.py
