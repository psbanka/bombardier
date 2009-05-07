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
        extension_list = ['exe', 'aspx']
        self.fileManifest = FileManifest(self.testDir, ['foo', 'bar'], self.manifestFile, extension_list )
        self.expectedManifestDict = {'foo': {'file.exe': 'b66b5b56809078def934c04cda9e791f'},
                                     'bar': {'file.txt': '', 
                                             'bar2%sfile.aspx' % os.sep: 'dbf1f2836e1b325fcfdfa6fca6aee3c1'}}
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

    def testCreateManifestFile(self):
        self.fileManifest.create_manifest()
        assert self.fileManifest.manifest_dictionary == self.expectedManifestDict, self.fileManifest.manifest_dictionary
        self.fileManifest.write_manifest_file()
        assert os.path.isfile(self.manifestFile)
        
    def testVerifyManifest(self):
        open( self.manifestFile, 'w' ).write( yaml.dump( self.expectedManifestDict ) )
        self.fileManifest.load_manifest()
        assert self.fileManifest.manifest_dictionary == self.expectedManifestDict
        assert self.fileManifest.verify_manifest( self.mappingDictionary ) == []

        os.system( "rm -f %s" %os.path.join( self.testDir, 'bar/file.txt' ) )
        errorList = self.fileManifest.verify_manifest( self.mappingDictionary )  
        assert errorList != [], errorList

        open( os.path.join( self.testDir, 'bar/file.txt' ), 'w' ).write("New text.")
        errorList = self.fileManifest.verify_manifest( self.mappingDictionary )  
        assert errorList == [], errorList

        open( os.path.join( self.testDir, 'bar/bar2/file.aspx' ), 'w' ).write(u"New aspx.")
        errorList = self.fileManifest.verify_manifest( self.mappingDictionary )  
        assert errorList != [], errorList


if __name__ == "__main__":
    tcommon = Tcommon.Tcommon()
    tcommon.setForTest()
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(FileManifestTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
    tcommon.unsetForTest()

