#!/cygdrive/c/Python23/python.exe

import yaml, os
from os import path
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
        for dir in os.listdir( self.rootDir ):
            if dir in self.subDirs:
                tempDictionary = {}
                fullPath = os.path.join( self.rootDir, dir )
                if os.path.isdir( fullPath ):
                    self.manifestDictionary[dir] = self.createPathDictionary( fullPath, tempDictionary )
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
        for file in os.listdir( path ):
            fullPath = os.path.join( path, file )
            relativePath = self.getRelativePath(fullPath)
            if os.path.isdir( fullPath ):
                self.createPathDictionary( fullPath, dict )
            elif os.path.isfile( fullPath ):
                if file.split('.')[-1].lower() in self.MD5_EXTENSIONS:
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
        d = self.manifestDictionary.keys()
        tupleCheckList = []
        for subdir in self.manifestDictionary.keys():
            for file in self.manifestDictionary[subdir]:
               tupleCheckList.append( ( os.path.join( mappingDict[subdir], file ), self.manifestDictionary[subdir][file] ) )
        errorList = self.verifyFileMd5Tuples( tupleCheckList )
        return( errorList )

    def verifyFileMd5Tuples(self, fileMd5TupleList):
        errorList = []
        for fileTuple in fileMd5TupleList:
            file = fileTuple[0]
            md5sum = fileTuple[1]
            lastslash = file.rfind( os.sep )+1
            base = file[0:lastslash]
            filename = file[lastslash:]
            if not os.path.isdir(base):
                errString = "missing directory"
                errorList.append((file, errString))
            elif not os.path.isfile(file):
                errString = "missing file"
                errorList.append((file, errString))
            elif md5sum != '':
                computed = md5.new(open(file, 'rb').read()).hexdigest()
                if md5sum != computed:
                    errString = "invalid checksum: Actual: %s Expected %s" % (computed, md5sum)
                    errorList.append((file, errString))
        return errorList
    
def test():
    f = FileManifest(os.path.join( os.getcwd(), ".." ) )
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
    SUPRANET_1861_SRC = path.join( BUILD_DIR, "code", "supranetBuilds", STR_SUPRANET_1861 )
    SUPRANET_SUBDIRS = ['KwmCMALeniSync', 'KwmImportSvc', 'KwmProgSvc', 'Site']
    OMSDIRS = [ 'LoginDB', 'OMSMaint', 'OMSTestClient', 'OMSUtils', 'OMS', 'OMSTcp2HTTPS', 'OMSTestSite', 'ReaderDB' ]
    supranetStagingDir = os.path.join( os.getcwd(), "staging", "supranet" ) 
    os.chdir( supranetStagingDir )
    yamlFile = os.path.join( os.getcwd(), "manifest.yml" )
    f = FileManifest( os.getcwd(), SUPRANET_SUBDIRS, yamlFile )
    f.createManifest()
    mappingDict = {}
    for dir in SUPRANET_SUBDIRS:
        mappingDict[dir] = os.path.join( supranetStagingDir, dir )
    mappingDict['KwmCMALeniSync']= os.path.join( SUPRANET_1861_SRC, 'KwmCMALeniSync' )
    mappingDict['KwmImportSvc']= os.path.join( SUPRANET_1861_SRC, 'KwmImportSvc' )
   
    print mappingDict
    f.checksumTest( mappingDict )
#    f.printSelf()
#
#    f.rootDir = os.path.join( os.getcwd(), ".." )
#    f.loadManifest( yamlFile )
#    f.checksumTest()

