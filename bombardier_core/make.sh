#!/bin/bash

if [ $(bzr stat | awk '/^modified:/' | wc -l) != 0 ]; then
    echo "Check in and commit changes to upstream branch before building."
    exit 1
fi
if [ $(bzr info | awk '/^Checkout .*/' | wc -l) != 0 ]; then
    bzr version-info --format python > lib/_version.py
    python setup.py sdist
else
    echo "Bind and update before building."
    exit 1
fi

