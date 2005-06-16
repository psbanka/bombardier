#!/bin/bash
rm -rf release/bombardier-0.4 || cd .
mkdir release/bombardier-0.4
cd release/bombardier-0.4

echo "PyCurl"
mkdir pycurl-ssl-7.13.2.win32-py2.4
cd pycurl-ssl-7.13.2.win32-py2.4
curl http://www.bombardierinstaller.org/downloads/pycurl-ssl-7.13.2.win32-py2.4.exe > ../../pycurl-ssl-7.13.2.win32-py2.4.exe
unzip ../../pycurl-ssl-7.13.2.win32-py2.4.exe > unzip.txt
cd ..

echo "pyyaml-46"
curl http://www.bombardierinstaller.org/downloads/pyyaml-46.tar.gz > ../pyyaml-46.tar.gz
tar -xzf ../pyyaml-46.tar.gz

echo "Windows extensions"
mkdir pywin32-204.win32-py2.4
cd pywin32-204.win32-py2.4
curl http://www.bombardierinstaller.org/downloads/pywin32-204.win32-py2.4.exe > ../../pywin32-204.win32-py2.4.exe
unzip ../../pywin32-204.win32-py2.4.exe > unzip.txt
cd ..

echo "wx windows"
curl http://www.bombardierinstaller.org/downloads/wxPython2.6-win32-unicode-2.6.0.0-py24.exe > wxPython2.6-win32-unicode-2.6.0.0-py24.exe

echo "bombardierClient"
mkdir bombardierClient
cp -r ../../bombardier bombardierClient
cp ../../bombardier/setup.py bombardierClient

echo "spkg"
cp -r ../../spkgDir spkg

echo "site-root"
cp -r ../../site-root site-root

echo "making release tarball"
cd ..
tar -cz --exclude=".svn" -f bombardier-0.4.tar.gz bombardier-0.4
#rm -rf bombardier-0.4
