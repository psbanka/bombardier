#!/usr/bin/env python
"""FileManifest.py: This is a class for automatically creating and
   verifying file manifests for installed code."""

# BSD License
# Copyright (c) 2009, Shawn Sherwood
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the name of the GE Security nor the names of its contributors may
#   be used to endorse or promote products derived from this software without
#   specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

import os
from mini_utility import md5_sum, make_path, yaml_load, yaml_dump

class FileManifest:
    """File manifest creation and verification class"""
    def __init__( self, root_dir, sub_dirs, manifest_path, \
                  md5_extensions=None ):
        """md5_extensions is a list of file extensions that 
           are md5summed as part of the file manifest"""
        if md5_extensions == None:
            md5_extensions = []
        self.md5_extensions = md5_extensions
        self.root_dir = root_dir.replace("\\", "/")
        self.manifest_dictionary = {}
        self.sub_dirs = sub_dirs
        self.manifest_path = manifest_path

    def create_manifest( self ):
        """Loop through subdirectories, collecting manifest data"""
        for inode in os.listdir( self.root_dir ):
            if inode in self.sub_dirs:
                temp_dictionary = {}
                full_path = make_path( self.root_dir, inode )
                if os.path.isdir( full_path ):
                    self.manifest_dictionary[inode] = \
                      self.create_path_dictionary( full_path, temp_dictionary )

    def write_manifest_file(self):
        """Write out the json or yaml for the manifest to the expected path"""
        dump_string = yaml_dump( self.manifest_dictionary )
        handle = open( self.manifest_path, "w" )
        handle.write( dump_string )
        handle.close()
        
    def create_path_dictionary( self, path, work_dict ):
        """Loop through the directories gathering data into the manifest"""
        for inode in os.listdir( path ):
            full_path = make_path( path, inode )
            relative_path = self.get_relative_path(full_path)
            if os.path.isdir( full_path ):
                self.create_path_dictionary( full_path, work_dict )
            elif os.path.isfile( full_path ):
                if inode.split('.')[-1].lower() in self.md5_extensions:
                    handle = open( full_path, 'rb' )
                    data = handle.read()
                    work_dict[relative_path] = md5_sum(data)
                    handle.close()
                else:
                    work_dict[relative_path] = ''
        return work_dict

    def get_relative_path( self, full_path ):
        """Utility function that creates a relative path from a full path"""
        path_from_subdir = full_path.split( self.root_dir + '/' )[-1] 
        rel_path = '/'.join( path_from_subdir.split( '/' )[1:] ) 
        return( rel_path ) 

    def load_manifest( self ):
        """Initialize manifest_dictionary from the existing manifest_path"""
        load_string = open( self.manifest_path, 'r' ).read()
        self.manifest_dictionary = yaml_load(load_string)

    def verify_manifest( self, mapping_dict ):
        """Loop through keys in manifest file, checking for files and
           md5sums as necessary. The mapping dictionary maps directories
           from the manifest file to directories in the file system."""
        tuple_check_list = []
        for subdir in self.manifest_dictionary.keys():
            for inode in self.manifest_dictionary[subdir]:
                new_tuple = ( make_path( mapping_dict[subdir], inode ),
                              self.manifest_dictionary[subdir][inode] )
                tuple_check_list.append( new_tuple )
        error_list = self.verify_file_md5_tuples( tuple_check_list )
        return( error_list )

    @classmethod
    def verify_file_md5_tuples(cls, file_md5_tuple_list):
        "Verify that a list of filepath, md5 pairs match"
        error_list = []
        for file_tuple in file_md5_tuple_list:
            filepath = file_tuple[0]
            md5sum = file_tuple[1]
            lastslash = filepath.rfind( '/' )+1
            base = filepath[0:lastslash]
            if not os.path.isdir(base):
                err_string = "missing directory"
                error_list.append((filepath, err_string))
            elif not os.path.isfile(filepath):
                err_string = "missing file"
                error_list.append((filepath, err_string))
            elif md5sum != '':
                computed = md5_sum(open(filepath, 'rb').read())
                if md5sum != computed:
                    err_string = "invalid checksum: Actual: %s Expected %s" \
                                 % (computed, md5sum)
                    error_list.append((filepath, err_string))
        return error_list

