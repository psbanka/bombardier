#!/bin/bash
DOWNLOADS="http://www.bombardierinstaller.com/downloads"
PYTHON=python-2.5.1
PY_CURL=pycurl-7.16.2.1-ssl-zlib.win32.py2.5
PY_YAML=PyYAML-3.05
PY_WIN32=pywin32-210.win32-py2.5
PY_CRYPTO=pycrypto-2.0.1.win32-py2.5
PY_CRYPTO_URL=http://www.voidspace.org.uk/cgi-bin/voidspace/downman.py?file=pycrypto-2.0.1.win32-py2.5.zip

RELEASE_DIR=$(pwd)/release
TARGET_DIR=$RELEASE_DIR/$BASE_NAME

function setupProxy()
{
    if [ -e http_proxy.txt ]; then
        echo "using http_proxy.txt to set proxy"
        export http_proxy=`cat http_proxy.txt`
    fi
    echo "http_proxy=$http_proxy"
}

function cleanTargetDir()
{
    rm -rf $TARGET_DIR || exit 1
    mkdir -p $TARGET_DIR/$BASE_NAME
}

function downloadPython()
{
    cd $RELEASE_DIR
    if [ -e $RELEASE_DIR/$PYTHON.msi ]; then
        echo "$PYTHON has already been downloaded (skipping download)"
    else
        echo "downloading $PYTHON..." 
        curl $DOWNLOADS/$PYTHON.msi > $RELEASE_DIR/$PYTHON.msi
    fi
}

function downloadPyCurl()
{
    mkdir $TARGET_DIR/$PY_CURL
    cd $TARGET_DIR/$PY_CURL

    if [ -e $RELEASE_DIR/$PY_CURL.tar.gz ]; then
        echo "PyCurl has already been downloaded (skipping download)"
    else
        echo "downloading PyCurl..."
        curl $DOWNLOADS/$PY_CURL.tar.gz > $RELEASE_DIR/$PY_CURL.tar.gz
    fi
    tar -xzf $RELEASE_DIR/$PY_CURL.tar.gz
}

function downloadPyYaml()
{
    cd $TARGET_DIR
    if [ -e $RELEASE_DIR/$PY_YAML.tar.gz ] ; then
        echo "$PY_YAML has already been downloaded (skipping download)"
    else
        echo "downloading $PY_YAML..."
        curl $DOWNLOADS/$PY_YAML.tar.gz > $RELEASE_DIR/$PY_YAML.tar.gz
    fi
    tar -xzf $RELEASE_DIR/$PY_YAML.tar.gz
}

function downloadPyCrypto()
{
    cd $RELEASE_DIR
    if [ -e $RELEASE_DIR/$PY_CRYPTO.zip ] ; then
        echo "PyCrypto has already been downloaded (skipping download)"
    else
        echo "Downloading PyCrypto..."
        curl $PY_CRYPTO_URL > $RELEASE_DIR/$PY_CRYPTO.zip
    fi
    unzip $PY_CRYPTO.zip
    mkdir $TARGET_DIR/$PY_CRYPTO
    cd $TARGET_DIR/$PY_CRYPTO
    unzip $RELEASE_DIR/$PY_CRYPTO.exe
}

function downloadPyWin32()
{
    mkdir $TARGET_DIR/$PY_WIN32
    cd $TARGET_DIR/$PY_WIN32
    if [ -e $TARGET_DIR/$PY_WIN32.exe ] ; then
        echo "Python Windows extensions have already been downloaded (skipping download)"
    else
        echo "Downloading Python Windows extensions..."
        curl $DOWNLOADS/$PY_WIN32.exe > $RELEASE_DIR/$PY_WIN32.exe
    fi
    unzip $RELEASE_DIR/$PY_WIN32.exe > unzip.txt  2>&1
}

function openPermissions()
{
    cd $RELEASE_DIR
    chmod -R 777 .
}

function makeReleaseTarball()
{
    cd $RELEASE_DIR
    echo "making release tarball"

    tar -cz --exclude=".svn" -f $BASE_NAME.tar.gz $BASE_NAME
    #rm -rf $BASE_NAME
}

function setup()
{
    setupProxy
    cleanTargetDir
}

function download()
{
    downloadPython
    downloadPyCurl
    downloadPyYaml
    downloadPyCrypto
    downloadPyWin32
}

setup
download
openPermissions
makeReleaseTarball

