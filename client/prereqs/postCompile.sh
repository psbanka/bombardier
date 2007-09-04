#!/bin/bash
set +x

scp Output/$BASE_NAME.exe `cat release/server.txt`

