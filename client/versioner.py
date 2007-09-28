#!/cygdrive/c/Python25/python.exe

import os

BOMBARDIER_VERSION_STRING  = os.environ[ 'BOMBARDIER_VERSION_STRING' ]
for dir in [ 'bombardier', 'spkgDir']:
    for f in [ os.path.join( dir, x ) for x in os.listdir(dir) if x.endswith( '.py' )]:
        fp = open( f, 'r' )
        lines = fp.readlines()
        versionStr = '# Version %s\n' %BOMBARDIER_VERSION_STRING
        newLines = [ lines[0], versionStr ]
        if lines[1].startswith( '# Version ' ):
            newLines += lines[2:]
        else:
            newLines += lines[1:]
        fp.close()
        fp = open( f, 'w' )
        fp.write( ''.join( newLines ) )
        fp.flush()
        fp.close()
