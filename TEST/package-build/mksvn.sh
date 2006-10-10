#!/bin/bash

tar -xzvf svn.tar.gz
mkdir scripts
BASE_DIR=$PWD
cd scripts
svn co file://$BASE_DIR/svn/scripts