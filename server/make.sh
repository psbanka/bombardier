#!/bin/bash
echo "Be sure that you have properly checked in before running a versioned build."
bzr version-info --format python > lib/_version.py
python setup.py sdist
