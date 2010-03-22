build_dir=$(PWD)/../build
staging_dir=../staging
upload_docs_dir=../bombardierinstaller/docs

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

define upload_target
	cd $(upload_dir) && make -f $(MAKE_FILE) upload
endef

define target_dir
../$@
endef

define check_target_dir
	[ -e $(target_dir) ] || exit 1
endef

define sdist_target
	cd $(target_dir) && make -f $(MAKE_FILE) sdist
endef

define remove_dist_files
	echo "$$PWD" | grep -v "build"
	rm -f AUTHORS CHANGELOG INSTALL LICENSE MANIFEST MANIFEST.in README
endef

help:
	@echo "Please use \`make <target>' where <target> is one of"
	@echo "  all       to make server, client, cli, and core"
	@echo "  staging   to make all and create a staging directory"
	@echo "  <component>  to make component (server, client, cli or core)"
	@echo "  <component>_upload  to make component and upload to pypi"

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

server_upload:
	$(check_upload_dir)
	$(upload_target) 

client_upload:
	$(check_upload_dir)
	$(upload_target) 

cli_upload:
	$(check_upload_dir)
	$(upload_target) 

core_upload:
	$(check_upload_dir)
	$(upload_target)

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

sdist: do_sdist reclean_files

upload:	official clean_dist dist_files version_dist reclean_files
	python setup.py sdist upload

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

dist_files: AUTHORS CHANGELOG README LICENSE INSTALL MANIFEST.in 

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

