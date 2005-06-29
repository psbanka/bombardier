#!/bin/bash

cd ../TEST || exit 1
/cygdrive/c/Python24/python.exe unitTests.py || exit 1

cd ../client || exit 1

bash ./preCompile.sh || exit 1

/cygdrive/c/Program\ Files/Inno\ Setup\ 5/Compil32.exe /cc inno-setup2.iss || exit 1

bash ./postCompile.sh

