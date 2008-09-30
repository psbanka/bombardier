#!/bin/bash
bzr version-info --format python > lib/_version.py
python setup.py sdist
