#!/cygdrive/c/Python24/python.exe
# Version 0.41-168

import yaml, datetime, urlparse, tarfile
import dmoweasel.dbprocess2 as dbprocess2
import bombardier.utility
from bombardier.staticData import *


def makeQueryString(args):
    queryString = ""
    if len(args.keys()) >= 1:
        for argIndex in range(0, len(args.keys())):
            key = args.keys()[argIndex]
            value = args[args.keys()[argIndex]]
            if argIndex == 0:
                queryString += "?%s=%s" % (key, value)
            else:
                queryString += "\\&%s=%s" % (key, value)
    return queryString

def getDateTime(): 
    d = datetime.datetime.today()
    dateString = "%d-%d-%d-%d-%d-%d" %(d.year, d.month, d.day, d.hour, d.minute, d.second ) 
    return( dateString )

def makeCurlCmd(targetName, config): 
    filePath = targetName+".tar.gz"
    args = {"packagename":targetName, "installscript": config["scripts"]}
    fullPath = "%s/website/service/package/" % config["destination"]["address"]
    queryString = makeQueryString(args)
    url = urlparse.urljoin(fullPath, queryString)
    print "Uploading to %s" % url
    cmd = "/bin/curl -T ../%s %s > output.txt\n" % (filePath, url)
    open("sendtar.bash", 'w').write(cmd)
    cmd = "/bin/rm -rf %s %s\n" % (filePath, targetName)
    open("cleanup.bash", 'w').write(cmd)
    return OK

def prepareDirectory(config, targetDir):
    os.makedirs(targetDir)
    os.chdir(targetDir)
    if config.has_key("svn"):
        print "Checking out %s from source control" % config["svn"]
        cmd = "c:\\cygwin\\bin\\svn co  --username %(svnusername)s --password %(svnpassword)s %(svn)s" % config
        print cmd
        status = os.system(cmd)
        print status
        if status != OK:
            print "*** Failed to export SVN data"
            sys.exit(1)
        print "*** Successful export of SVN data"
        print "Purging Script files prior to dump..."
        cmd = r"""c:\\cygwin\\bin\\find . -name "Script" -exec /bin/rm '{}' ;"""
        print cmd
        status = os.system(cmd)

def sendMessage(subject, message, me, admins, smtpServer):
    import smtplib
    from email.MIMEText import MIMEText
    msg = MIMEText(message)
    msg['Subject'] = subject
    msg['From'] = me
    msg['To'] = ','.join(admins)
    s = smtplib.SMTP()
    try:
        s.connect(host=smtpServer)
        s.sendmail(me, admins, msg.as_string())
        s.quit()
    except:
        print "Unable to send email"

def sendAdministrativeMessage(new, modified, delted, config):
    newFiles = ["new:" + fileName for fileName in new]
    modifiedFiles = ["modified:" + fileName for fileName in new]
    deletedFiles = ["deleted:" + fileName for fileName in new]
    subject  = "Changes have been made to %s schema in production" % config["target"]
    message  = "The following items have been deleted:\n"
    message += '\n'.join(newFiles) +"\n\n"
    message += "The following items have been modified:\n"
    message += '\n'.join(modifiedFiles) + "\n\n"
    message += "The following items have been added:\n"
    message += '\n'.join(newFiles) + "\n\n"
    me       = config["me"]
    admins   = config["admins"]
    smtpServer = config["smtpServer"]
    sendMessage(subject, message, me, admins, smtpServer)

def sendSvnChanges(targetDir, target, config):
    os.chdir(targetDir)
    os.chdir("Databases")
    os.system("c:\\cygwin\\bin\\svn status > status.txt")
    os.system("c:\\cygwin\\bin\\svn diff > difference.txt")
    statusData = open("status.txt", 'r').readlines()
    deleted  = []
    new      = []
    modified = []
    for line in statusData:
        status, filename = line.split()
        if status == "?":
            new.append(filename)
        elif status == "!":
            deleted.append(filename)
        elif status == "M":
            modified.append(filename)
    for filename in deleted:
        os.system("c:\\cygwin\\bin\\svn delete %s" % filename)
    for filename in new:
       os.system("c:\\cygwin\\bin\\svn add %s" % filename)
    if new != [] or modified != [] or deleted != []:
        sendAdministrativeMessage(new, modified, deleted, config)
        cmd = 'c:\\cygwin\\bin\\svn ci --non-interactive -m "%s automated pull from production"' % target
        print cmd
        os.system(cmd)

def backupTargets(fullConfig, timestamp, logger, userTarget):
    filesystem = bombardier.Filesystem.Filesystem()
    output = {}
    for target in targets.keys():
        if userTarget and userTarget != target:
            continue
        config = targets[target]
        config["svnusername"] = fullConfig["svnusername"]
        config["svnpassword"] = fullConfig["svnpassword"]
        config["tmpDir"] = fullConfig["tmpDir"]
        config["destination"] = fullConfig["destination"]
        config["me"] = fullConfig["me"]
        config["admins"] = fullConfig["admins"]
        config["smtpServer"] = fullConfig["smtpServer"]
        config["target"] = target
        #targetName = "Lenidb-pod1Structure-2005-9-27-8-23-12" # ^^^^^
        targetName       = "%s-%s-%s" % (config["database"], target, timestamp)
        targetDir        = os.path.join(config["tmpDir"], targetName)
        targetFile       = targetName + ".tar.gz"
        if config.has_key("skip"):
            skipTables = {config["database"]: config["skip"]}
        else:   
            skipTables = {config["database"]: []}
        prepareDirectory(config, targetDir)
        status = dbprocess2.backupDb([config["database"]], skipTables, config, logger)
        if status == OK:
            print "=========== BACKUP COMPLETED SUCCESSFULLY"
        else:
            print "=========== BACKUP HAD ERRORS"
        if config["username"]:
            open("__owner",'w').write(config["username"] + '\n')
            open("__passwd",'w').write("%s::%s\n" % (config["username"], config["password"]))
        status = OK
        if config.has_key("svn"):
            sendSvnChanges(targetDir, target, config)
        if status == OK:
            os.chdir(targetDir)
            tar = tarfile.open( "../%s" % targetFile, "w:gz" )
            for inode in os.listdir( '.' ):
                tar.add( inode )
            tar.close()
            makeCurlCmd(targetName, config)
            if fullConfig["upload"]:
                os.chdir(targetDir)
                status = os.system("c:\\cygwin\\bin\\bash sendtar.bash")   
                print "sending status = %s" % status
                print open("output.txt", 'r').read()
        #status = OK  # ^^
        output[config["database"]] = {"status": status, "targetDir": targetDir}
    return output
     
if __name__ == "__main__":
    target = None
    fullConfig    = yaml.loadFile("dbBackup.yml").next()
    fullConfig["upload"] = True
    if len(sys.argv) > 1:
        target = sys.argv[1]
    if len(sys.argv) > 2:
        fullConfig["upload"] = False
        print "Will upload this to the server when finished"
    targets   = fullConfig["targets"]
    logger    = bombardier.utility.Logger.Logger()
    logger.addStdErrLogging()
    timestamp = getDateTime()   
    output    = backupTargets(fullConfig, timestamp, logger, target)
    print output
