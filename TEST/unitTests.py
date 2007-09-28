#!/cygdrive/c/Python25/python.exe

import sys, os
sys.path = [os.path.join("..", "client"), os.path.join('..', 'spkgDir')] + sys.path

from bombardier.staticData import *
import unittest
import Tcommon

# Client-side tests
def clientTesting():
    import TPackage
    import TRepository
    import TConfig
    import TBombardierClass
    import TCommSocket
    import TminiUtility
    import TFilesystem
    import TFileManifest
    tcommon = Tcommon.Tcommon()
    tcommon.setForTest()
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(TBombardierClass.BombardierTest))
    suite.addTest(unittest.makeSuite(TPackage.PackageTest))
    suite.addTest(unittest.makeSuite(TRepository.RepositoryTest))
    suite.addTest(unittest.makeSuite(TConfig.ConfigTest))
    suite.addTest(unittest.makeSuite(TCommSocket.CommSocketTest))
    suite.addTest(unittest.makeSuite(TminiUtility.miniUtilityTest))
    suite.addTest(unittest.makeSuite(TFilesystem.FilesystemTest))
    suite.addTest(unittest.makeSuite(TFileManifest.FileManifestTest))
    print "Testing: client-side components"
    status = unittest.TextTestRunner(verbosity=1).run(suite)
    print "(%s)" %`status`
    tcommon.unsetForTest()
    return status


status2 = clientTesting()
if len( status2.failures ) + len( status2.errors ) == 0:
    print "ALL TESTS PASSED"
    sys.exit(0)
sys.exit(1)
