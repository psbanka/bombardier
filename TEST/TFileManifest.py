#!/cygdrive/c/Python25/python.exe

import os, sys, unittest, shutil, Tcommon
sys.path = ["../client"] + sys.path

from bombardier.staticData import *
from bombardier.FileManifest import FileManifest
import yaml

class FileManifestTest(unittest.TestCase):
    def setUp(self):
        self.testDir = '_manifest_test'
        self.manifestFile = os.path.join( self.testDir, 'manifest/manifest.yml' )
        self.createDirectoryStructure()
        self.fileManifest = FileManifest(self.testDir, ['foo', 'bar'], self.manifestFile)
        self.expectedManifestDict = {'foo': {'file.exe': 'b66b5b56809078def934c04cda9e791f'},
                                     'bar': {'file.txt': '', 
                                             'bar2\\file.aspx': 'dbf1f2836e1b325fcfdfa6fca6aee3c1'}}
        self.mappingDictionary = { 'bar': os.path.join( self.testDir, 'bar' ),
                                   'foo': os.path.join( self.testDir, 'foo' ) }
    
    def createDirectoryStructure(self):
        self.startDir = os.getcwd()
        os.system( 'rm -rf %s'%self.testDir )
        os.mkdir( self.testDir )
        
        os.chdir( self.testDir )
        for subdir in [ 'manifest', 'bar/bar2', 'foo' ]:
            os.makedirs( subdir )
        open( 'bar/file.txt', 'w' ).write("This is text.")
        open( 'bar/bar2/file.aspx', 'w' ).write(u"Aspx file.")
        open( 'foo/file.exe', 'w' ).write("\x45\x90\x08\x21\x05")
        os.chdir( self.startDir )

    def tearDown(self):
        os.system( 'rm -rf %s' %self.testDir )

    def testCreateManifest(self):
        self.fileManifest.createManifest()
        assert self.fileManifest.manifestDictionary == self.expectedManifestDict
        
    def testVerifyManifest(self):
        open( self.manifestFile, 'w' ).write( yaml.dump( self.expectedManifestDict ) )
        self.fileManifest.loadManifest()
        assert self.fileManifest.manifestDictionary == self.expectedManifestDict
        assert self.fileManifest.verifyManifest( self.mappingDictionary ) == []

        os.system( "rm -f %s" %os.path.join( self.testDir, 'bar/file.txt' ) )
        errorList = self.fileManifest.verifyManifest( self.mappingDictionary )  
        assert errorList != [], errorList

        open( os.path.join( self.testDir, 'bar/file.txt' ), 'w' ).write("New text.")
        errorList = self.fileManifest.verifyManifest( self.mappingDictionary )  
        assert errorList == [], errorList

        open( os.path.join( self.testDir, 'bar/bar2/file.aspx' ), 'w' ).write(u"New aspx.")
        errorList = self.fileManifest.verifyManifest( self.mappingDictionary )  
        assert errorList != [], errorList


if __name__ == "__main__":
    tcommon = Tcommon.Tcommon()
    tcommon.setForTest()
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(FileManifestTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
    tcommon.unsetForTest()

