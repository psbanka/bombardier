build_dir=$(PWD)/../build
staging_dir=../staging
upload_docs_dir=../bombardierinstaller/docs
launchpad_uploader=$(build_dir)/launchpad_uploader.py
modify_changelog=$(build_dir)/modify_changelog.py

MAKE_FILE=$(build_dir)/Makefile

define upload_dir
../$(subst _upload,,$@)
endef

define doc_dir
../$(subst _doc,,$@)
endef

define doc_type
$(subst _doc,,$@)
endef

define check_doc_dir
	[ -e  $(doc_dir) ] || exit 1
endef

define check_upload_dir
	[ -e  $(upload_dir) ] || exit 1
endef

define doc_target
	cd $(doc_dir) && make -f $(MAKE_FILE) doc
    cp -r $(doc_dir)/docs/.build/html $(upload_docs_dir)/$(doc_type)
endef

define target_dir
../$@
endef

define check_target_dir
	[ -e $(target_dir) ] || exit 1
endef

define upload_target
	cd $(upload_dir) && make -f $(MAKE_FILE) upload
endef

define sdist_target
	cd $(target_dir) && make -f $(MAKE_FILE) sdist
endef

define setup_target
	cd $(build_dir)/../$(subst _setup,,$@) && python setup.py install
endef

define dpkg_target
	cd $(build_dir)/dpkg && bash build_package.bash $(subst _dpkg,,$@)
    @echo
    @echo "Press Ctrl-C now to skip ppa upload"
    @echo
    @read tester
    cd $(build_dir) && dput ppa:bombardier-team/bombardier-ppa ./dpkg/work/$(subst _dpkg,,$@)/bombardier*source.changes
endef

define remove_dist_files
	echo "$$PWD" | grep -v "build"
	rm -f AUTHORS CHANGELOG INSTALL LICENSE MANIFEST MANIFEST.in README
endef

define remove_upload_dist_files
	cd $(upload_dir) && rm -f AUTHORS CHANGELOG INSTALL LICENSE MANIFEST MANIFEST.in README project_info.yml setup.py
endef

help:
	@echo "Please use \`make <target>' where <target> is one of"
	@echo "  all       to make server, client, cli, and core"
	@echo "  staging   to make all and create a staging directory"
	@echo "  <component>  to make component (server, client, cli or core)"
	@echo "  <component>_upload  to make component and upload to pypi"
	@echo "  <component>_doc  to build the generated doc html"
	@echo "  <component>_dpkg  to build the apt source package"
	@echo "  <component>_setup  to run setup.py install"
	@echo "  modify_changelog  to roughly add bzr comments to the CHANGELOG file"

all:
	make -f $(MAKE_FILE) server
	make -f $(MAKE_FILE) client
	make -f $(MAKE_FILE) cli
	make -f $(MAKE_FILE) core

staging: all
	rm -rf $(staging_dir)
	mkdir $(staging_dir)
	mv ../server/dist/*.gz $(staging_dir)
	mv ../client/dist/*.gz $(staging_dir)
	mv ../cli/dist/*.gz $(staging_dir)
	mv ../core/dist/*.gz $(staging_dir)

server:
	$(check_target_dir)
	$(sdist_target)

client:
	$(check_target_dir)
	$(sdist_target)

cli: 
	$(check_target_dir)
	$(sdist_target)

core:
	$(check_target_dir)
	$(sdist_target)

server_dpkg: server
	$(dpkg_target)

client_dpkg: client
	$(dpkg_target)

cli_dpkg:  cli
	$(dpkg_target)

core_dpkg: core
	$(dpkg_target)

server_setup: server
	$(setup_target)

client_setup: client
	$(setup_target)

cli_setup:  cli
	$(setup_target)

core_setup: core
	$(setup_target)

server_upload:
	$(check_upload_dir)
	$(upload_target) 
	$(remove_upload_dist_files)

client_upload:
	$(check_upload_dir)
	$(upload_target) 
	$(remove_upload_dist_files)

cli_upload:
	$(check_upload_dir)
	$(upload_target) 
	$(remove_upload_dist_files)

core_upload:
	$(check_upload_dir)
	$(upload_target)
	$(remove_upload_dist_files)

server_doc:
	$(check_doc_dir)
	$(doc_target) 

client_doc:
	$(check_doc_dir)
	$(doc_target) 

cli_doc:
	$(check_doc_dir)
	$(doc_target) 

core_doc:
	$(check_doc_dir)
	$(doc_target) 

clean_docs:
	rm -rf $(upload_docs_dir)
	mkdir $(upload_docs_dir)

bombardierinstaller: clean_docs server_doc client_doc cli_doc 
	@echo "Doc build complete"
	cd ../bombardierinstaller && python prep_site.py

sdist: do_sdist reclean_files

upload:	official clean_dist dist_files version_dist
	@echo "UPLOAD TO PYPI using setup.py sdist upload"
	python setup.py sdist upload
	@echo "UPLOAD TO LAUNCHPAD using $(launchpad_uploader)"
	python $(launchpad_uploader)

modify_changelog:
	python $(modify_changelog)

doc:	clean_dist dist_files version_dist reclean_files
	cd docs && make html

version_dist:
	bzr version-info --format python > lib/_version.py

clean_dist: clean_files clean_build
	rm -rf dist

clean_build: clean_files
	rm -rf build 

do_sdist: clean_dist dist_files version_dist
	python setup.py sdist

official: checked_out not_modified 

checked_out:
	@[ $$(bzr info | awk '/^Checkout .*/' | wc -l ) -ne 0 ]
	@echo "checked_out ok"
	
not_modified:
	@[ $$(bzr stat | awk '/^modified:/' | wc -l ) -eq 0 ]
	@echo "not_modified ok"

clean_files:
	$(remove_dist_files)

reclean_files:
	$(remove_dist_files)

dist_files: AUTHORS CHANGELOG README LICENSE INSTALL MANIFEST.in project_info proto_setup

AUTHORS:
	cp -f $(build_dir)/AUTHORS .
CHANGELOG:
	cp -f $(build_dir)/CHANGELOG .
README:
	cp -f $(build_dir)/README .
LICENSE:
	cp -f $(build_dir)/LICENSE .
INSTALL:
	cp -f $(build_dir)/INSTALL .
MANIFEST.in:
	cp -f $(build_dir)/MANIFEST.in .
project_info:
	cp -f $(build_dir)/project_info.yml .
proto_setup:
	cp -f $(build_dir)/proto_setup.py ./setup.py
