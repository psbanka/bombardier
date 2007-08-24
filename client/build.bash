#!/bin/bash

PYTHON_EXE=/cygdrive/c/Python25/python.exe
export BOMBARDIER_VERSION=0.5
export BOMBARDIER_REVISION=`svn info | grep '^Revision' | sed 's/[^0-9]'//g`
export BOMBARDIER_VERSION_STRING=$BOMBARDIER_VERSION-$BOMBARDIER_REVISION
export BASE_NAME=bombardierSetup-$BOMBARDIER_VERSION_STRING

cat inno-setup2.iss | sed "s/$BOMBARDIER_VERSION/$BOMBARDIER_VERSION_STRING/g" > versionedSetup.iss
cat bombardier/staticData.py | sed "s/\(^VERSION.*\)$BOMBARDIER_VERSION\(.*\)/\1$BOMBARDIER_VERSION_STRING\2/" > versionedStaticData.py
mv versionedStaticData.py bombardier/staticData.py
echo $BOMBARDIER_VERSION_STRING
$PYTHON_EXE versioner.py

cd ../TEST || exit 1

$PYTHON_EXE unitTests.py || exit 1
cd ../client || exit 1

bash ./preCompile.sh || exit 1

/cygdrive/c/Program\ Files/Inno\ Setup\ 5/Compil32.exe /cc versionedSetup.iss || exit 1

bash ./postCompile.sh

