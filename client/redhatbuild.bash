#!/bin/bash

export SVN_TRUNK_URL="http://172.17.50.57/bombardier/trunk/"
export REDHAT_TRUNK="/usr/src/redhat"

do_or_die( )
{
    $1 && return
    echo
    echo "============================================================"
    echo "                   FAILED = VERY YES"
    echo "============================================================"
    echo $2
    echo "============================================================"
    exit 1
}

do_or_die "rm -rf trunk"

echo Non recursively checking out trunk...
do_or_die "svn co -N $SVN_TRUNK_URL" "Could not check out trunk."

do_or_die "cd trunk" "Could not go to trunk directory"
export BOMBARDIER_VERSION=`svn info | grep '^Revision' | sed 's/[^0-9]'//g`

echo Update client...
do_or_die "svn update -r$BOMBARDIER_REVISION client" "Could not update client"

cp -f client/bombardier.spec $REDHAT_TRUNK/SPECS
mv client bombardier-$BOMBARDIER_REVISION
tar -czvf $REDHAT_TRUNK/SOURCES/bombardier-$BOMBARDIER_REVISION.tar.gz bombarider-$BOMBERDIER_REVISION

do_or_die "cd $REDHAT_TRUNK/SPECS" "Could not change directory to redhat build area"

do_or_die "rpmbuild -bb bombardier.spec" "Could not build bombardier rpm package"

