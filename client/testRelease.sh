#!/bin/bash
tar -czf testbombardier.tar.gz *.py *.pyd *.dat yaml curl
md5sum testbombardier.tar.gz > testbombardier.md5
scp testbombardier.tar.gz testbombardier.md5 root@172.17.50.56:/var/www/deploy
