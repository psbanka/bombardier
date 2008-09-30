#!/bin/bash

tar -xzvf svn.tar.gz
mkdir scripts
svn co file://$PWD/svn/scripts
cd scripts
svn cleanup
cd -
