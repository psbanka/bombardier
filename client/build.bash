#!/bin/bash

export BOMBARDIER_VERSION=`svn info | grep '^Revision' | sed 's/[^0-9]'//g`
export BASE_NAME=bombardierSetup-0.4-$BOMBARDIER_VERSION

cat inno-setup2.iss | sed "s/bombardierSetup-0\.4/bombardierSetup-0\.4-$BOMBARDIER_VERSION/g" > versionedSetup.iss
cat bombardier/staticData.py | sed "s/\(^VERSION.*\)0\.41\(.*\)/\10\.41-$BOMBARDIER_VERSION\2/" > versionedStaticData.py
mv versionedStaticData.py bombardier/staticData.py

/cygdrive/c/Python25/python.exe versioner.py

cd ../TEST || exit 1
/cygdrive/c/Python25/python.exe unitTests.py || exit 1

cd ../client || exit 1

bash ./preCompile.sh || exit 1

/cygdrive/c/Program\ Files/Inno\ Setup\ 5/Compil32.exe /cc versionedSetup.iss || exit 1

bash ./postCompile.sh

