#!/cygdrive/c/Python24/python.exe

# FileManifest.py: This is a class for automatically creating and
# verifying file manifests for installed code.

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
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

import yaml, os
import md5

class FileManifest:
    def __init__( self, rootDir, subDirs, manifestPath ):
        self.MD5_EXTENSIONS = ['cs', 'aspx', 'asax', 'dll',
                               'ocx', 'exe', 'HEX', 'asax',
                               'ascx', 'js', 'css', 'cab', 
                               'gif', 'jpg', 'swf', 'ttf']  
        self.rootDir = rootDir
        self.manifestDictionary = {}
        self.subDirs = subDirs
        self.manifestPath = manifestPath

    def createManifest( self ):
        for inode in os.listdir( self.rootDir ):
            if inode in self.subDirs:
                tempDictionary = {}
                fullPath = os.path.join( self.rootDir, inode )
                if os.path.isdir( fullPath ):
                    self.manifestDictionary[inode] = self.createPathDictionary( fullPath,
                                                                                tempDictionary )
        self.dumpYamlToFile()

    def dumpYaml(self):
        yamlString = yaml.dump( self.manifestDictionary )
        return( yamlString )

    def dumpYamlToFile(self):
        yamlString = self.dumpYaml()
        fp = open( self.manifestPath, "w" )
        fp.write( yamlString )
        fp.close()
        
    def createPathDictionary( self, path, dict ):
        for inode in os.listdir( path ):
            fullPath = os.path.join( path, inode )
            relativePath = self.getRelativePath(fullPath)
            if os.path.isdir( fullPath ):
                self.createPathDictionary( fullPath, dict )
            elif os.path.isfile( fullPath ):
                if inode.split('.')[-1].lower() in self.MD5_EXTENSIONS:
                    fp = open( fullPath, 'rb' )
                    dict[relativePath] = md5.new( fp.read() ).hexdigest()
                    fp.close()
                else:
                    dict[relativePath] = ''
        return dict

    def printSelf(self):
        print self.manifestDictionary

    def getRelativePath( self, fullPath ):
        pathFromSubdir = fullPath.split( self.rootDir + os.sep )[-1] 
        return( os.sep.join( pathFromSubdir.split( os.sep )[1:] ) )

    def buildPathFromList( self, list ):
        if list[0][-1] == ':':
            list[0] += os.sep
        tPath = ''
        for p in list:
            tPath = os.path.join( tPath, p )
        return tPath

    def loadManifest( self ):
        yamlParser = yaml.loadFile( self.manifestPath )
        self.manifestDictionary = yamlParser.next()

    def verifyManifest( self, mappingDict ):
        tupleCheckList = []
        for subdir in self.manifestDictionary.keys():
            for inode in self.manifestDictionary[subdir]:
               tupleCheckList.append( ( os.path.join( mappingDict[subdir], inode ),
                                        self.manifestDictionary[subdir][inode] ) )
        errorList = self.verifyFileMd5Tuples( tupleCheckList )
        return( errorList )

    def verifyFileMd5Tuples(self, fileMd5TupleList):
        errorList = []
        for fileTuple in fileMd5TupleList:
            filepath = fileTuple[0]
            md5sum = fileTuple[1]
            lastslash = filepath.rfind( os.sep )+1
            base = filepath[0:lastslash]
            if not os.path.isdir(base):
                errString = "missing directory"
                errorList.append((filepath, errString))
            elif not os.path.isfile(filepath):
                errString = "missing file"
                errorList.append((filepath, errString))
            elif md5sum != '':
                computed = md5.new(open(filepath, 'rb').read()).hexdigest()
                if md5sum != computed:
                    errString = "invalid checksum: Actual: %s Expected %s" % (computed, md5sum)
                    errorList.append((filepath, errString))
        return errorList
    
def test(): # probably doesn't work - pbanka
    f = FileManifest('.', os.path.join( os.getcwd(), ".." ), '.' )
    f.createManifest()
    return f 

def timeTester( func ):
    import time
    start = time.time()
    result = func()
    end = time.time()
    et = end - start
    print "ET: ", et
    return result

if __name__ == "__main__":
    BUILD_DIR = "/var/_build"
    STR_SUPRANET_1861 = "supranet-1.0-1.1.1861"
    SUPRANET_1861_SRC = os.path.join( BUILD_DIR, "code", "supranetBuilds", STR_SUPRANET_1861 )
    SUPRANET_SUBDIRS = ['KwmCMALeniSync', 'KwmImportSvc', 'KwmProgSvc', 'Site']
    OMSDIRS = [ 'LoginDB', 'OMSMaint', 'OMSTestClient', 'OMSUtils', 'OMS', 'OMSTcp2HTTPS', 'OMSTestSite', 'ReaderDB' ]
    supranetStagingDir = os.path.join( os.getcwd(), "staging", "supranet" ) 
    os.chdir( supranetStagingDir )
    yamlFile = os.path.join( os.getcwd(), "manifest.yml" )
    f = FileManifest( os.getcwd(), SUPRANET_SUBDIRS, yamlFile )
    f.createManifest()
    mappingDict = {}
    for inode in SUPRANET_SUBDIRS:
        mappingDict[inode] = os.path.join( supranetStagingDir, inode )
    mappingDict['KwmCMALeniSync']= os.path.join( SUPRANET_1861_SRC, 'KwmCMALeniSync' )
    mappingDict['KwmImportSvc']= os.path.join( SUPRANET_1861_SRC, 'KwmImportSvc' )
   
    print mappingDict
    f.checksumTest( mappingDict )


