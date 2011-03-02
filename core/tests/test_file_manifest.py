#!/usr/bin/env python

import os, sys, unittest
sys.path.insert(0, "../lib")

from FileManifest import FileManifest
from bombardier_core.mini_utility import get_slash_cwd, make_path
import simplejson as json

LOG_COMMAND = False


def force_remove_dir(rm_path):
    rm_cmd = 'bash -c "rm -rf %s"' % rm_path
    log_command( rm_cmd )

def force_remove_file(rm_path):
    rm_cmd = 'bash -c "rm -f %s"' % rm_path
    log_command( rm_cmd )

def log_command(cmd_str):
    if LOG_COMMAND:
        print "\n==================="
        print "CMD: ", cmd_str
    result = os.system(cmd_str) 
    if LOG_COMMAND:
        print "Result = ", result 
        print "==================="
    return result

class FileManifestTest(unittest.TestCase):
    def setUp(self):
        self.test_dir = '_manifest_test'
        self.manifest_file = make_path( self.test_dir, 'manifest/manifest.yml' )
        self.create_directory_structure()
        extension_list = ['exe', 'aspx']
        self.file_manifest = FileManifest(self.test_dir, ['foo', 'bar'], self.manifest_file, extension_list )
        self.expected_manifest_dict = {'foo': {'file.exe': 'b66b5b56809078def934c04cda9e791f'},
                                     'bar': {'file.txt': '',
                                             'bar2/file.aspx': 'dbf1f2836e1b325fcfdfa6fca6aee3c1'}}
        self.mapping_dictionary = { 'bar': make_path( self.test_dir, 'bar'),
                                   'foo': make_path( self.test_dir, 'foo') }

    def create_directory_structure(self):
        self.start_dir = get_slash_cwd()
        force_remove_dir(self.test_dir)
        log_command( 'bash -c "mkdir -p %s"' % self.test_dir )

        for subdir in [ 'manifest', make_path('bar','bar2'), 'foo' ]:
            full_path = make_path(self.test_dir, subdir)
            log_command('bash -c "mkdir -p %s"' % full_path)

        fp = open( make_path(self.test_dir, 'bar','file.txt'), 'w' )
        fp.write("This is text.")
        fp.close()
        del fp

        fp = open( make_path(self.test_dir, 'bar','bar2','file.aspx'), 'w' )
        fp.write(u"Aspx file.")
        fp.close()
        del fp

        fp = open( make_path(self.test_dir, 'foo','file.exe'), 'w' )
        fp.write("\x45\x90\x08\x21\x05")
        fp.close()
        del fp

    def tearDown(self):
        return
        force_remove_dir(self.test_dir)

    def test_create_manifest_file(self):
        self.file_manifest.create_manifest()
        if self.file_manifest.manifest_dictionary != self.expected_manifest_dict:
            print "Expected: %s\nGot    : %s" % (self.file_manifest.manifest_dictionary, self.expected_manifest_dict)
        assert self.file_manifest.manifest_dictionary == self.expected_manifest_dict, self.file_manifest.manifest_dictionary
        self.file_manifest.write_manifest_file()
        assert os.path.isfile(self.manifest_file)

    def test_verify_manifest(self):
        manifest_file_handle = open( self.manifest_file, 'w' )
        json.dump( self.expected_manifest_dict, manifest_file_handle)
        manifest_file_handle.flush()
        manifest_file_handle.close()
        self.file_manifest.load_manifest()
        assert self.file_manifest.manifest_dictionary == self.expected_manifest_dict
        assert self.file_manifest.verify_manifest( self.mapping_dictionary ) == []

        force_remove_file( make_path(self.test_dir, 'bar', 'file.txt') )
        error_list = self.file_manifest.verify_manifest( self.mapping_dictionary )
        assert error_list != [], error_list

        open( make_path(self.test_dir, 'bar', 'file.txt'), 'w' ).write("New text.")
        error_list = self.file_manifest.verify_manifest( self.mapping_dictionary )
        assert error_list == [], error_list

        open( make_path(self.test_dir, 'bar', 'bar2', 'file.aspx'), 'w' ).write(u"New aspx.")
        error_list = self.file_manifest.verify_manifest( self.mapping_dictionary )
        assert error_list != [], error_list


if __name__ == "__main__":
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(FileManifestTest))
    unittest.TextTestRunner(verbosity=2).run(suite)

