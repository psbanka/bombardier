#!/bin/bash

PYTHON_EXE=/cygdrive/c/Python25/python.exe
export PREREQ_VERSION=0.5
export PREREQ_REVISION=`svn info | grep '^Revision' | sed 's/[^0-9]'//g`
export PREREQ_VERSION_STRING=$PREREQ_VERSION-$PREREQ_REVISION
export BASE_NAME=prereqSetup-$PREREQ_VERSION_STRING

cat prereqsetup.iss | sed "s/$PREREQ_VERSION/$PREREQ_VERSION_STRING/g" > versionedSetup.iss
echo $PREREQ_VERSION_STRING

bash ./preCompile.sh || exit 1

/cygdrive/c/Program\ Files/Inno\ Setup\ 5/Compil32.exe /cc versionedSetup.iss || exit 1

#bash ./postCompile.sh

