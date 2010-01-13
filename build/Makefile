MAKE_FILE=../build/Makefile

define check_target_dir
	[ -e ../$@ ] || exit 1
endef

define sdist_target
	cd ../$@ && make -f $(MAKE_FILE) sdist
endef

define upload_target
	$(sdist_target) upload
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
	[ -e ../core ] || exit 1
	cd ../core && make -f $(MAKE_FILE) upload

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
	cp -f ../build/AUTHORS .
CHANGELOG:
	cp -f ../build/CHANGELOG .
README:
	cp -f ../build/README .
LICENSE:
	cp -f ../build/LICENSE .
INSTALL:
	cp -f ../build/INSTALL .
MANIFEST.in:
	cp -f ../build/MANIFEST.in .

