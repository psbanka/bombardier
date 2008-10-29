#!/bin/bash
echo "Be sure you are checked in correctly before versioning a build."
bzr version-info --format python > lib/_version.py
python setup.py sdist
