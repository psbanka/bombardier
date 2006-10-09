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
export RPM_PACKAGE_VERSION=`svn info | grep '^Revision' | sed 's/[^0-9]'//g`

echo Update client...
do_or_die "svn update -r$RPM_PACKAGE_VERSION client" "Could not update client"

cat client/bombardier.spec | sed "s/\(Version\:.*\)/\1${RPM_PACKAGE_VERSION}/g" > $REDHAT_TRUNK/SPECS/bombardier.spec
mv client bombardier-$RPM_PACKAGE_VERSION
find . -name ".svn" -exec rm -rf '{}'>/dev/null \;
tar -czvf $REDHAT_TRUNK/SOURCES/bombardier-${RPM_PACKAGE_VERSION}.tar.gz bombardier-$RPM_PACKAGE_VERSION

do_or_die "cd $REDHAT_TRUNK/SPECS" "Could not change directory to redhat build area"

do_or_die "rpmbuild -bb bombardier.spec" "Could not build bombardier rpm package"

