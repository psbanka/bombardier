#!/usr/bin/env python

import os, sys, unittest
sys.path.insert(0, "../lib")

from FileManifest import FileManifest
import yaml

class FileManifestTest(unittest.TestCase):
    def setUp(self):
        self.test_dir = '_manifest_test'
        self.manifest_file = os.path.join( self.test_dir, 'manifest/manifest.yml' )
        self.create_directory_structure()
        extension_list = ['exe', 'aspx']
        self.file_manifest = FileManifest(self.test_dir, ['foo', 'bar'], self.manifest_file, extension_list )
        self.expected_manifest_dict = {'foo': {'file.exe': 'b66b5b56809078def934c04cda9e791f'},
                                     'bar': {'file.txt': '',
                                             'bar2%sfile.aspx' % os.sep: 'dbf1f2836e1b325fcfdfa6fca6aee3c1'}}
        self.mapping_dictionary = { 'bar': os.path.join( self.test_dir, 'bar' ),
                                   'foo': os.path.join( self.test_dir, 'foo' ) }

    def create_directory_structure(self):
        self.start_dir = os.getcwd()
        os.system( 'rm -rf %s'%self.test_dir )
        os.mkdir( self.test_dir )

        os.chdir( self.test_dir )
        for subdir in [ 'manifest', 'bar/bar2', 'foo' ]:
            os.makedirs( subdir )
        open( 'bar/file.txt', 'w' ).write("This is text.")
        open( 'bar/bar2/file.aspx', 'w' ).write(u"Aspx file.")
        open( 'foo/file.exe', 'w' ).write("\x45\x90\x08\x21\x05")
        os.chdir( self.start_dir )

    def tearDown(self):
        os.system( 'rm -rf %s' %self.test_dir )

    def test_create_manifest_file(self):
        self.file_manifest.create_manifest()
        assert self.file_manifest.manifest_dictionary == self.expected_manifest_dict, self.file_manifest.manifest_dictionary
        self.file_manifest.write_manifest_file()
        assert os.path.isfile(self.manifest_file)

    def test_verify_manifest(self):
        open( self.manifest_file, 'w' ).write( yaml.dump( self.expected_manifest_dict ) )
        self.file_manifest.load_manifest()
        assert self.file_manifest.manifest_dictionary == self.expected_manifest_dict
        assert self.file_manifest.verify_manifest( self.mapping_dictionary ) == []

        os.system( "rm -f %s" %os.path.join( self.test_dir, 'bar/file.txt' ) )
        error_list = self.file_manifest.verify_manifest( self.mapping_dictionary )
        assert error_list != [], error_list

        open( os.path.join( self.test_dir, 'bar/file.txt' ), 'w' ).write("New text.")
        error_list = self.file_manifest.verify_manifest( self.mapping_dictionary )
        assert error_list == [], error_list

        open( os.path.join( self.test_dir, 'bar/bar2/file.aspx' ), 'w' ).write(u"New aspx.")
        error_list = self.file_manifest.verify_manifest( self.mapping_dictionary )
        assert error_list != [], error_list


if __name__ == "__main__":
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(FileManifestTest))
    unittest.TextTestRunner(verbosity=2).run(suite)

