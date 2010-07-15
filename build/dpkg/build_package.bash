#!/bin/bash

function complain_and_exit() {
    echo $1
    exit 1
}
export DEBCHANGE_QUERY_BTS=no
[ "$DEBFULLNAME" == "" ] && complain_and_exit "DEBFULLNAME is not set"
[ "$DEBEMAIL" == "" ] &&  complain_and_exit "DEBEMAIL is not set"

start_dir=$PWD
[ "${start_dir##*/}" == "dpkg" ] || complain_and_exit "Script must run from the dpkg directory"

component=$1
version=1.00
component_name=bombardier-${component}-${version}
revision=$(ls ../../$component/dist/ | sed "s/bombardier_${component}-${version}-\(.*\).tar.gz/\1/")

[ "$revision" == "" ] && complain_and_exit "Version not found" 

old_name=bombardier_${component}-${version}

bdr_name=${component_name}-${revision}
work_dir=$start_dir/work/$component
bdr_dir=$work_dir/$bdr_name
man_dir=$bdr_dir/usr/share/man
message="$(bzr log -r${revision} | awk '$0 ~ /^  / {print substr($0,3); next}')"

rm -rf $work_dir
mkdir -p $work_dir

cd $work_dir || complain_and_exit "Could not cd to $work_dir"

orig_tarball=$start_dir/../../$component/dist/${old_name}-${revision}.tar.gz
cp  $orig_tarball $bdr_name.tar.gz
tar -xzf $bdr_name.tar.gz 
mv ${old_name}-${revision} $bdr_dir

cd $bdr_dir

dh_make -c gpl -s -b -f ../$bdr_name.tar.gz

cd $bdr_dir/debian
rm *ex *EX README.Debian dirs

cp -r $start_dir/common/* .
cp -r $start_dir/$component/* .
if [ -e $man_dir ]; then
    find $man_dir -type f -exec gzip '{}' \; || :
fi

cd ..

dch --newversion=${revision}-1 "$message"
cp debian/changelog $start_dir/$component/new_changelog

debuild -sa
