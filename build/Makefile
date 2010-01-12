client_dirty: TARGET=client
client_dirty: dirty

server_dirty:
	cd ../server && make -f ../build/Makefile dirty

dirty: do_sdist clean

upload:	checked_out not_modified nice
	rm -rf dist build MANIFEST
	bzr version-info --format python > lib/_version.py
	python setup.py sdist upload

dist_clean:
	rm -f AUTHORS CHANGELOG INSTALL LICENSE MANIFEST MANIFEST.in README
	rm -rf build dist

clean:
	rm -f AUTHORS CHANGELOG INSTALL LICENSE MANIFEST MANIFEST.in README
	rm -rf build 

do_sdist: dist_clean nice 
	cd $$TARGET
	bzr version-info --format python > lib/_version.py
	python setup.py sdist

official_sdist: checked_out not_modified 

checked_out:
	@bzr info | awk '/^Checkout .*/' | wc -l || exit 1
	@echo "checked_out ok"
	
not_modified:
	@bzr stat | awk '/^modified:/' | wc -l && exit 1
	@echo "not_modified ok"

nice: AUTHORS CHANGELOG README LICENSE INSTALL MANIFEST.in 

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

