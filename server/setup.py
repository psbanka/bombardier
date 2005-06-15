#!/usr/bin/env python

from distutils.core import setup
setup(name="webserver",
      version="0.1",
      packages = ['pages'],
      py_modules=["webServer", "template", "static"])
