build_dir=../build

MAKE_FILE=$(build_dir)/Makefile

define upload_dir
../$(subst _upload,,$@)
endef

define check_upload_dir
	[ -e  $(upload_dir) ] || exit 1
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
	$(check_target_dir)
#	$(upload_target) 

client_upload:
	$(check_target_dir)
#	$(upload_target) 

cli_upload:
	$(check_target_dir)
#	$(upload_target) 

core_upload:
	$(check_upload_dir)
	$(upload_target)

sdist: do_sdist clean_files

upload:	official clean_dist dist_files version_dist
	python setup.py sdist upload

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
	rm -f AUTHORS CHANGELOG INSTALL LICENSE MANIFEST MANIFEST.in README

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

