#!/cygdrive/c/Python24/python.exe

import os

for dir in [ 'bombardier', 'spkgDir', 'site-root', "dmoweasel", "dmoweasel/dmoweasel" ]:
    for f in [ os.path.join( dir, x ) for x in os.listdir(dir) if x.endswith( '.py' )]:
        fp = open( f, 'r' )
        lines = fp.readlines()
        newLines = [ lines[0] ]
        if lines[1].startswith( '# Version ' ):
            newLines += lines[2:]
        else:
            newLines += lines[1:]
        fp.close()
        fp = open( f, 'w' )
        fp.write( ''.join( newLines ) )
        fp.flush()
        fp.close()
