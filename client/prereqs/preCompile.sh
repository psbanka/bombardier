#!/bin/bash
DOWNLOADS="http://www.bombardierinstaller.com/downloads"
PYTHON=python-2.5.1
PY_CURL=pycurl-7.16.2.1-ssl-zlib.win32.py2.5
PY_YAML=PyYAML-3.05
PY_WIN32=pywin32-210.win32-py2.5

if [ -e http_proxy.txt ]; then
    echo "using http_proxy.txt to set proxy"
    export http_proxy=`cat http_proxy.txt`
fi
echo "http_proxy=$http_proxy"

rm -rf release/prereq-$PREREQ_VERSION_STRING || cd .
mkdir release/prereq-$PREREQ_VERSION_STRING
cd release/prereq-$PREREQ_VERSION_STRING

if [ -e ../$PYTHON.msi ]; then
    echo "$PYTHON has already been downloaded (skipping download)"
else
    echo "downloading $PYTHON..." 
    curl $DOWNLOADS/$PYTHON.msi > ../$PYTHON.msi
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

mkdir $PY_WIN32
cd $PY_WIN32
if [ -e ../../$PY_WIN32.exe ] ; then
    echo "Python Windows extensions have already been downloaded (skipping download)"
else
    echo "Downloading Python Windows extensions..."
    curl $DOWNLOADS/$PY_WIN32.exe > ../../$PY_WIN32.exe
fi
unzip ../../$PY_WIN32.exe > unzip.txt  2>&1
cd ../..

echo "setting permissions on third party installers..."
chmod 777 *.exe *.msi

echo "making release tarball"

tar -cz --exclude=".svn" -f prereq-$PREREQ_VERSION_STRING.tar.gz prereq-$PREREQ_VERSION_STRING
#rm -rf prereq-$PREREQ_VERSION_STRING
