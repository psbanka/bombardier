#!/bin/bash

sudo ./killServer.bash
sleep 2
su www-data -c python2.3 webServer.py
