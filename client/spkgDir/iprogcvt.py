#!C:\Python24\python.exe

import yaml, sys, time, os
import bombardier.miniUtility
import bombardier.Logger as Logger

Logger.addStdErrLogging()

def convertProgress():
    spkgPath = bombardier.miniUtility.getSpkgPath()
    progressPath = os.path.join(spkgPath, "install-progress.yml")
    statusPath = os.path.join(spkgPath, "status.yml")
    if os.path.isfile(progressPath):
        Logger.warning("reading from file %s" % progressPath)
        progData = {}
        try:
            progData = yaml.loadFile(progressPath).next()
        except Exception, e:
            Logger.error("Cannot convert file: %s" % e)
            sys.exit(1)
        data = {"install-progress": progData,
                "status": { "action": "", "main": "",
                            "overall": "", "percentage": 0 },
                "timestamp": time.time()}
        Logger.warning("writing file %s" % statusPath)
        outputFile = open(statusPath, 'w')
        yamlString = yaml.dump(data)
        outputFile.write(yamlString)
        outputFile.close()
        os.unlink(progressPath)

if __name__ == "__main__":
    convertProgress()
    
