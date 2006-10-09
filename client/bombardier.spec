Distribution:	CCux
Packager:  	Peter Banka <peter.banka@ge.com>

Summary:   	An open-source package manager for Windows and Linux
Name:      	bombardier
Version:   	$BOMBARDIER_VERSION
Release:   	1
Requires: 	python pyyaml pycurl
License: 	GNU/GPL
URL:		http://www.bombardierinstaller.com/
Group:     	Development/Python
Source0: 	%{name}-%{version}.tar.gz
BuildRoot: 	%{_tmppath}/%{name}-buildroot

%description
Bombardier solves the problem of how to automatically
deliver, install, and configure software on remote systems

%prep
%setup

%build
echo "no build necessary"

%install
rm -rf $RPM_BUILD_ROOT/*
mkdir $RPM_BUILD_ROOT || true
mkdir $RPM_BUILD_ROOT/etc
cp etc/bombardier.yml $RPM_BUILD_ROOT/etc/
/usr/local/bin/python bombardier/setup.py install --root $RPM_BUILD_ROOT
cd site-root
/usr/local/bin/python setup.py install --root $RPM_BUILD_ROOT
cd ..
cd spkgDir
mkdir $RPM_BUILD_ROOT/opt || true
mkdir $RPM_BUILD_ROOT/opt/spkg || true
/usr/local/bin/python setup.py install $RPM_BUILD_ROOT/opt/spkg

%files
%defattr(-,root,root,-)
/usr/local/lib/python2.4/site-packages/bombardier
/usr/local/lib/python2.4/site-packages/FileManifest.py
/usr/local/lib/python2.4/site-packages/RegistryDict.py
/usr/local/lib/python2.4/site-packages/commonUtil.py
/usr/local/lib/python2.4/site-packages/dbutils.py
/usr/local/lib/python2.4/site-packages/installUtils.py
/usr/local/lib/python2.4/site-packages/FileManifest.pyc
/usr/local/lib/python2.4/site-packages/RegistryDict.pyc
/usr/local/lib/python2.4/site-packages/commonUtil.pyc
/usr/local/lib/python2.4/site-packages/installUtils.pyc
/usr/local/lib/python2.4/site-packages/dbutils.pyc
/etc/bombardier.yml
/opt/spkg

%clean
rm -rf $RPM_BUILD_ROOT
