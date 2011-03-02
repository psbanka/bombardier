#!/bin/bash
function run_tests() {
    for i in *.py
      do python $i
    done
}

run_tests
cd ../lib
run_tests
cd -
