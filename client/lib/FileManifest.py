#!/cygdrive/c/Python24/python.exe
"""FileManifest.py: This is a class for automatically creating and
   verifying file manifests for installed code."""

# Copyright (C) 2005 Shawn Sherwood

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, 
# MA  02110-1301, USA.

import yaml, os
import md5

class FileManifest:
    """File manifest creation and verification class"""
    def __init__( self, root_dir, sub_dirs, manifest_path, \
                  md5_extensions=None ):
        """md5_extensions is a list of file extensions that 
           are md5summed as part of the file manifest"""
        if md5_extensions == None:
            md5_extensions = []
        self.md5_extensions = md5_extensions
        self.root_dir = root_dir
        self.manifest_dictionary = {}
        self.sub_dirs = sub_dirs
        self.manifest_path = manifest_path

    def create_manifest( self ):
        """Loop through subdirectories, collecting manifest data"""
        for inode in os.listdir( self.root_dir ):
            if inode in self.sub_dirs:
                temp_dictionary = {}
                full_path = os.path.join( self.root_dir, inode )
                if os.path.isdir( full_path ):
                    self.manifest_dictionary[inode] = \
                      self.create_path_dictionary( full_path, temp_dictionary )

    def write_manifest_file(self):
        """Write out the yaml for the manifest to the expected path"""
        yaml_string = yaml.dump( self.manifest_dictionary )
        handle = open( self.manifest_path, "w" )
        handle.write( yaml_string )
        handle.close()
        
    def create_path_dictionary( self, path, work_dict ):
        """Loop through the directories gathering data into the manifest"""
        for inode in os.listdir( path ):
            full_path = os.path.join( path, inode )
            relative_path = self.get_relative_path(full_path)
            if os.path.isdir( full_path ):
                self.create_path_dictionary( full_path, work_dict )
            elif os.path.isfile( full_path ):
                if inode.split('.')[-1].lower() in self.md5_extensions:
                    handle = open( full_path, 'rb' )
                    data = handle.read()
                    work_dict[relative_path] = md5.new( data ).hexdigest()
                    handle.close()
                else:
                    work_dict[relative_path] = ''
        return work_dict

    def get_relative_path( self, full_path ):
        """Utility function that creates a relative path from a full path"""
        path_from_subdir = full_path.split( self.root_dir + os.sep )[-1] 
        return( os.sep.join( path_from_subdir.split( os.sep )[1:] ) )

    def load_manifest( self ):
        """Initialize manifest_dictionary from the existing manifest_path"""
        yaml_string = open( self.manifest_path, 'r' ).read()
        self.manifest_dictionary = yaml.load(yaml_string)

    def verify_manifest( self, mapping_dict ):
        """Loop through keys in manifest file, checking for files and
           md5sums as necessary. The mapping dictionary maps directories
           from the manifest file to directories in the file system."""
        tuple_check_list = []
        for subdir in self.manifest_dictionary.keys():
            for inode in self.manifest_dictionary[subdir]:
                new_tuple = ( os.path.join( mapping_dict[subdir], inode ),
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
            lastslash = filepath.rfind( os.sep )+1
            base = filepath[0:lastslash]
            if not os.path.isdir(base):
                err_string = "missing directory"
                error_list.append((filepath, err_string))
            elif not os.path.isfile(filepath):
                err_string = "missing file"
                error_list.append((filepath, err_string))
            elif md5sum != '':
                computed = md5.new(open(filepath, 'rb').read()).hexdigest()
                if md5sum != computed:
                    err_string = "invalid checksum: Actual: %s Expected %s" \
                                 % (computed, md5sum)
                    error_list.append((filepath, err_string))
        return error_list

