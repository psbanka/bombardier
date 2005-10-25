#!/cygdrive/c/Python24/python.exe

import os

BOMBARDIER_VERSION = os.environ[ 'BOMBARDIER_VERSION' ]
for dir in [ 'bombardier', 'spkgDir', 'site-root' ]:
    for f in [ os.path.join( dir, x ) for x in os.listdir(dir) if x.endswith( '.py' )]:
        fp = open( f, 'r' )
        lines = fp.readlines()
        newLines = [ lines[0], '# Version 0.41-%s\n' %BOMBARDIER_VERSION ] + lines[1:]
        fp.close()
        fp = open( f, 'w' )
        fp.write( ''.join( newLines ) )
        fp.flush()
        fp.close()
