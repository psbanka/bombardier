#!/bin/bash

kill `ps -ef | grep webServer.py | grep -v vim | grep -v grep | awk '{print $2}'`


