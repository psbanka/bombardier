#!/bin/bash

rm -rf release/bombardier-$BOMBARDIER_VERSION_STRING || cd .
mkdir release/bombardier-$BOMBARDIER_VERSION_STRING
cd release/bombardier-$BOMBARDIER_VERSION_STRING

echo "bombardierClient"
mkdir bombardierClient
cp -r ../../bombardier bombardierClient
cp ../../bombardier/setup.py bombardierClient

echo "spkg"
cp -r ../../spkgDir spkg

cd ..

echo "setting permissions on third party installers..."
chmod 777 *.exe *.msi

echo "making release tarball"

tar -cz --exclude=".svn" -f bombardier-$BOMBARDIER_VERSION_STRING.tar.gz bombardier-$BOMBARDIER_VERSION_STRING
#rm -rf bombardier-$BOMBARDIER_VERSION_STRING
