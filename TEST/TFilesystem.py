#!/cygdrive/c/Python25/python.exe

import os, sys, unittest, shutil, Tcommon
sys.path = ["../client"] + sys.path

import bombardier.Filesystem as Filesystem
from bombardier.staticData import *
import bombardier.Exceptions as Exceptions
import bombardier.miniUtility as miniUtility
import MockObjects
import socket

TF  = 'testfile'
TF2 = 'testfile2'
TD  = 'testdir'

dumbYaml = '---\nHello: there'

minigz = '\x1f\x8b\x08\x08l\xfb\x86B\x00\x03foo.txt\x00\xab\xe0\x02\x00\x1f\x08\xeaF\x02\x00\x00\x00'

smallTar1 = "\x1f\x8b\x08\x00w\xfb\x86B\x00\x03\xed\xce\xb1\r\xc20\x14\x84a\xd7L\xe1\t\xd0\xb3\xfd\x9e\x93\x01X\x81\x01R\x80D\x01\x96b#e|\x8cDJ\x1a\xa4\x84\xe6\xff\x9a\x93NW\xdc\xb5\x94c[\x9a\xdb\x92\x04\x91\xac\xda3\x05\xcb\xd9\x89D\xd5\x18z\xae\xa2\x0b\xbd\x0b6\x98\xd9{\x17u\x10\xe7e\xd3W\x1f\xcf\xda\xa6\xd9{\x17e\x14\x1dSN_v\xa7r\x9fn\x0f\x7f\xae\x97\xb9\xee\xf1k'\xcb\xe1\xdf\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbfx\x01\x1c^\xbf=\x00(\x00\x00"

class FilesystemTest(unittest.TestCase):

    def setUp(self):
        self.server = MockObjects.MockServer()
        self.startDir = os.getcwd()
        self.scratchDir = os.path.join(self.startDir, "scratch")
        if os.path.isdir(self.scratchDir):
            shutil.rmtree(self.scratchDir)
        os.makedirs(self.scratchDir)
        os.chdir(self.scratchDir)
        self.filesystem = Filesystem.Filesystem()
        open(miniUtility.getProgressPath(), 'w').write('---\n\rstatus: {}')
    def tearDown(self):
        os.chdir(self.startDir)
        shutil.rmtree(self.scratchDir)

    def testopen(self):
        self.filesystem.open(TF, 'w').write('hello\n')
        assert self.filesystem.open(TF, 'r').read() == 'hello\n'
    def testisfile(self):
        open(TF, 'w').write('x')
        assert self.filesystem.isfile(TF) == True
    def testisdir(self):
        os.mkdir("foo")
        assert self.filesystem.isdir("foo")
    def testgetcwd(self):
        assert self.filesystem.getcwd() == self.scratchDir
    def teststat(self):
        open(TF, 'w').write('x')
        assert len(self.filesystem.stat(TF)) == 10, self.filesystem.stat(TF)
    def testunlink(self):
        open(TF, 'w').write('x')
        assert os.path.isfile(TF)
        self.filesystem.unlink(TF)
        assert not os.path.isfile(TF)
    def testloadYaml(self):
        open(TF, 'w').write(dumbYaml)
        data = self.filesystem.loadYaml(TF)
        assert data== {'Hello': 'there'}, data
        exception = False
        try:
            data = self.filesystem.loadYaml("x")
        except Exceptions.NoYamlData, e:
            assert e.filename.endswith('x')
            exception = True
        assert exception
    def testgzipOpen(self):
        open(TF, 'w').write(minigz)
        data = self.filesystem.gzipOpen(TF).read()
        assert data == 'x\n', `data`
    def testtarOpen(self):
        open(TF, 'w').write(smallTar1)
        tar = self.filesystem.tarOpen(TF, 'r')
        assert tar.firstmember.name == "foo.txt"
        tar.close()
    def testcreateTar(self):
        os.mkdir('tmp')
        open(os.path.join('tmp',TF), 'w').write('x')
        self.filesystem.createTar("testfile.tar.gz", "tmp")
        data = open("testfile.tar.gz").read()
        assert 'testfile.tar' in data
    def testlistdir(self):
        os.mkdir('hello')
        open(TF, 'w').write('x')
        inodes = self.filesystem.listdir('.')
        assert 'hello' in inodes
        assert TF in inodes
    def testmkdir(self):
        self.filesystem.mkdir('x')
        assert os.path.isdir('x')
    def testcopyfile(self):
        open(TF, 'w').write('x')
        self.filesystem.copyfile(TF, TF2)
        assert os.path.isfile(TF2)
    def testrmtree(self):
        os.mkdir(TD)
        self.filesystem.rmtree(TD)
        assert not os.path.isdir(TD)
    def testchdir(self):
        assert os.getcwd() == self.scratchDir
        self.filesystem.chdir('..')
        assert os.getcwd() != self.scratchDir
    def testgetAllFromFile(self):
        open(TF, 'w').write('x')
        data = self.filesystem.getAllFromFile('.', TF)
        assert data == ['x'], data
    def testgetBinaryDataFromFilePath( self ):
        open(TF, 'wb').write(minigz)
        data = self.filesystem.getBinaryDataFromFilePath(TF)
        assert data == minigz
    def testgetStringDataFromFilepath( self ):
        open(TF, 'w').write("hello")
        data = self.filesystem.getStringDataFromFilePath(TF)
        assert data == "hello"
    def testsetLock(self):
        status = self.filesystem.setLock()
        assert os.path.isfile(os.path.join(self.startDir, INSTALL_LOCK))
    def testclearLock(self):
        open(INSTALL_LOCK, 'w').write('x')
        status = self.filesystem.clearLock()
        assert not os.path.isfile(os.path.join(self.startDir, INSTALL_LOCK))
    def testloadCurrent(self):
        current = self.filesystem.loadCurrent()
        assert type(current) == type({}), current
    def testupdateCurrentStatus(self):
        self.filesystem.updateCurrentStatus("testing", "testing currentstatus now", self.server)
        current = self.filesystem.loadCurrent()
        assert current['status']['overall'] == "testing"
        assert current['status']['main']    == "testing currentstatus now"
    def testupdateCurrentAction(self):
        self.filesystem.updateCurrentAction("unit testing", 10, self.server)
        current = self.filesystem.loadCurrent()
        assert current['status']['action']     == "unit testing"
        assert current['status']['percentage'] == 10
    def testgetCurrentAction(self):
        action = self.filesystem.updateCurrentAction("unit testing", 10, self.server)
        assert self.filesystem.getCurrentAction() == "unit testing"
    def testUpdateDict(self):
        newDict = {"a": {"b":1, "c":2}}
        oldDict = {"a": {"d":2}}
        combined = miniUtility.updateDict(newDict, oldDict)
        assert len(combined["a"].keys()) == 3
    def testappend(self):
        open(TF, 'w').write('y')
        open(TF2, 'w').write('x')
        self.filesystem.append(TF, TF2)
        data = open(TF2).read()
        assert data == 'xy', data
    def testGetProgressData(self):
        okProgress  = """---
install-progress:
    testdelaypackage-1-1:
        INSTALLED: 'Mon Jul 11 21:15:51 2005'
        UNINSTALLED: NA
        VERIFIED: 'Mon Jul 11 21:15:51 2005'
    testdelaypackage-2-1:
        INSTALLED: 'Mon Jul 11 21:15:56 2005'
        UNINSTALLED: NA
        VERIFIED: 'Mon Jul 11 21:15:56 2005'
    testdelaypackage-3-1:
        INSTALLED: 'Mon Jul 11 21:16:07 2005'
        UNINSTALLED: NA
        VERIFIED: 'Mon Jul 11 21:16:07 2005'
    testdelaypackage-4-1:
        INSTALLED: 'Mon Jul 11 21:16:02 2005'
        UNINSTALLED: NA
        VERIFIED: 'Mon Jul 11 21:16:02 2005'
    testokpackage1-1:
        INSTALLED: 'Mon Jul 11 21:15:49 2005'
        UNINSTALLED: NA
        VERIFIED: 'Mon Jul 11 21:15:49 2005'"""
        open(miniUtility.getProgressPath(), 'w').write(okProgress)
        progress = self.filesystem.getProgressData()
        assert type(progress) == type({})
        assert "testdelaypackage-1-1" in progress.keys()
        badProgress = "---- cscript-5.6**8@"
        open(miniUtility.getProgressPath(), 'w').write(badProgress)
        exceptionCaught = False
        try:
            progress = self.filesystem.getProgressData()
        except Exception, e:
            exceptionCaught = True
            assert `e` == "Invalid progress: ---- cscript-5.6**8@", "(%s)" % `e`
        assert exceptionCaught

if __name__ == "__main__":
    tcommon = Tcommon.Tcommon()
    tcommon.setForTest()
    suite = unittest.TestSuite()
    #suite.addTest(FilesystemTest("testGetProgressData"))
    #suite.addTest(FilesystemTest("testupdateCurrentStatus"))
    suite.addTest(unittest.makeSuite(FilesystemTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
    tcommon.unsetForTest()

