#!/bin/bash

cd ../TEST || exit 1
/cygdrive/c/Python24/python.exe unitTests.py || exit 2

cd ../client || exit 3

bash ./preCompile.sh || exit 4

/cygdrive/c/Program\ Files/Inno\ Setup\ 5/Compil32.exe /cc inno-setup2.iss || exit 5

bash ./postCompile.sh
