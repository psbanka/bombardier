#!/bin/bash
DOWNLOADS="http://bombardierinstaller.org/downloads"
PYTHON_24=python-2.4.1
PY_CURL=pycurl-ssl-zlib-7.14.0.win32-py2.4
PY_YAML=pyyaml-46
PY_WIN32=pywin32-204.win32-py2.4
WX_PYTHON=wxPython2.6-win32-unicode-2.6.0.0-py24
DMOWEASEL=dmoweasel

if [ -e http_proxy.txt ]; then
    echo "using http_proxy.txt to set proxy"
    export http_proxy=`cat http_proxy.txt`
fi
echo "http_proxy=$http_proxy"

rm -rf release/bombardier-0.4 || cd .
mkdir release/bombardier-0.4
cd release/bombardier-0.4

if [ -e ../$PYTHON_24.msi ]; then
    echo "$PYTHON_24 has already been downloaded (skipping download)"
else
    echo "downloading $PYTHON_24..." 
    curl $DOWNLOADS/$PYTHON_24.msi > ../$PYTHON_24.msi
fi

mkdir $PY_CURL
cd $PY_CURL

if [ -e ../../$PY_CURL.tar.gz ]; then
    echo "PyCurl has already been downloaded (skipping download)"
else
    echo "downloading PyCurl..."
    curl $DOWNLOADS/$PY_CURL.tar.gz > ../../$PY_CURL.tar.gz
fi
tar -xzf ../../$PY_CURL.tar.gz
cd ..

if [ -e ../$PY_YAML.tar.gz ] ; then
    echo "$PY_YAML has already been downloaded (skipping download)"
else
    echo "downloading $PY_YAML..."
    curl $DOWNLOADS/$PY_YAML.tar.gz > ../$PY_YAML.tar.gz
fi
tar -xzf ../$PY_YAML.tar.gz

mkdir $DMOWEASEL
cd $DMOWEASEL
if [ -e ../$DMOWEASEL ]; then
    echo "DMOWeasel has already been downloaded (skipping download)"
else
    echo "Downloading dmoWeasel..."
    curl $DOWNLOADS/$DMOWEASEL.tar.gz > ../../$DMOWEASEL.tar.gz
fi
tar -xzvf ../../$DMOWEASEL.tar.gz

mkdir $PY_WIN32
cd $PY_WIN32
if [ -e ../../$PY_WIN32.exe ] ; then
    echo "Python Windows extensions have already been downloaded (skipping download)"
else
    echo "Downloading Python Windows extensions..."
    curl $DOWNLOADS/$PY_WIN32.exe > ../../$PY_WIN32.exe
fi
unzip ../../$PY_WIN32.exe > unzip.txt  2>&1
cd ..

#FIX
if [ -e ../$WX_PYTHON.exe ]; then
    echo "WX Windows has already been downloaded (skipping download)"
else
    echo "downloading WX Windows..."
    curl $DOWNLOADS/$WX_PYTHON.exe > ../$WX_PYTHON.exe
fi
cp ../$WX_PYTHON.exe .

echo "bombardierClient"
mkdir bombardierClient
cp -r ../../bombardier bombardierClient
cp ../../bombardier/setup.py bombardierClient

echo "spkg"
cp -r ../../spkgDir spkg

echo "site-root"
cp -r ../../site-root site-root

cd ..

echo "setting permissions on third party installers..."
chmod 777 *.exe *.msi

echo "making release tarball"

tar -cz --exclude=".svn" -f bombardier-0.4.tar.gz bombardier-0.4
rm -rf bombardier-0.4
