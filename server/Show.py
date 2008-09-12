#!/usr/bin/python

import sys, readline, time
import yaml
import PinshCmd, ConfigField, Integer, BomHostField, PackageField, JobNameField
from commonUtil import *
import SecureCommSocket

TERM_OVERCOUNT = 8 # For some reason, the term width seems too long...

class ShowCommand(PinshCmd.PinshCmd):
    def __init__(self, name, helpText):
        PinshCmd.PinshCmd.__init__(self, name, helpText)
        self.configField = None
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        pyChucker(slash)
        if noFlag:
            return FAIL, []
        if len(tokens) < 3:
            return FAIL, ["Incomplete command."]
        currentDict = self.configField.getSpecificData(tokens, 2)
        return OK, yaml.dump(currentDict, default_flow_style=False).split('\n')

class Merged(ShowCommand):
    def __init__(self):
        ShowCommand.__init__(self, "merged", "merged\tdisplay a merged configuration")
        self.configField = ConfigField.ConfigField()
        self.children = [self.configField]

class Client(ShowCommand):
    def __init__(self):
        ShowCommand.__init__(self, "client", "client\tshow the configuration for one client")
        self.configField = ConfigField.ConfigField(dataType=ConfigField.CLIENT)
        self.children = [self.configField]

class Include(ShowCommand):
    def __init__(self):
        ShowCommand.__init__(self, "include", "include\tshow a shared include file")
        self.configField = ConfigField.ConfigField(dataType=ConfigField.INCLUDE)
        self.children = [self.configField]

class Bom(ShowCommand):
    def __init__(self):
        ShowCommand.__init__(self, "bom", "bom\tshow a bill of materials")
        self.configField = ConfigField.ConfigField(dataType=ConfigField.BOM)
        self.children = [self.configField]

class History(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "history")
        self.helpText = "history\tdisplay the history of commands"
        self.integer  = Integer.Integer(min=1, max=1000)
        self.children = [self.integer]
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        pyChucker(noFlag, slash)
        if len(tokens) == 2 or tokens[-1].strip()=='':
            number = 20
        else:
            try:
                number = int(tokens[2])
            except:
                return FAIL, ["%s is not a number." % tokens[2]]
        hlen = readline.get_current_history_length()
        if hlen < number:
            number = hlen
        output = []
        for i in range(hlen-number, hlen):
            output.append("%4d\t%s" % (i, readline.get_history_item(i)))
        return OK, output

class Package(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "package")
        self.helpText = "package\tdisplay information about a given package"
        self.packageField = PackageField.BasicPackageField()
        self.children = [self.packageField]
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        pyChucker(noFlag, slash)
        if len(tokens) < 3:
            return FAIL, ["Incomplete command."]
        packageName = tokens[2]
        pkgData = yaml.load(open(mode.serverHome+"/deploy/packages/packages.yml").read())
        return OK, ['', packageName, "========================", '', [pkgData.get(packageName)]]

def printify(inputObject):
    textList = list(inputObject)
    textList.sort()
    output = []
    if not textList:
        return []
    maxLength = max( [ len(t) for t in textList ] )
    columns = (mode.termwidth - TERM_OVERCOUNT) / maxLength
    for i in range(0, len(textList), columns):
        newLine = ''
        for item in textList[i:i+columns]:
            newLine += item.ljust(maxLength+2)
        output.append(newLine)
    return output

class Status(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "status")
        self.helpText = "status\tstatus of a host"
        self.bomHostField = BomHostField.BomHostField()
        self.children = [self.bomHostField]
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        pyChucker(slash)
        if noFlag:
            return FAIL, []
        if len(tokens) < 3:
            return FAIL, ["Incomplete command."]
        hostName = tokens[2]
        statusFile = "status/%s.yml" % hostName
        if not os.path.isfile(statusFile):
            return FAIL, ["No status on file (%s)" % statusFile]
        installed, broken = PackageField.getNamesFromProgress(hostName, False)
        totalPackages = PackageField.getPackageNamesFromBom(hostName)
        missing = []
        accountedPackages = list(installed.union(broken))
        for item in totalPackages:
            found = False
            for packageName in accountedPackages:
                if packageName.startswith(item):
                    found = True
                    break
            if not found:
                missing.append(item)
        if installed:
            output = ["Installed:",[printify(list(installed))]]
        else:
            output = ["Installed:",[["NONE"]]]
        if broken:
            output += ["Broken:",[printify(list(broken))]]
        else:
            output += ["Broken:",[["NONE"]]]
        if missing:
            output += ["Not Installed:",[printify(list(missing))]]
        else:
            output += ["Not Installed:",[["NONE"]]]
        return OK, output

class Jobs(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "jobs")
        self.helpText = "jobs\tdisplay jobs"
        self.jobNameField = JobNameField.JobNameField()
        self.children = [self.jobNameField]
        self.level = 0
        self.cmdOwner = 1
        self.auth = ADMIN
        
    def cmd(self, tokens, noFlag, slash):
        pyChucker(slash)
        jobName = ''
        if noFlag:
            return FAIL, []
        if mode.auth != ADMIN:
            return FAIL, ["Must be enabled"]
        if len(tokens) > 2:
            jobName = tokens[2]
        c = SecureCommSocket.SecureClient(TB_CTRL_PORT, mode.password)
        try:
            jobs = c.sendSecure(TB_SHOW, [])[0]
            for job in jobs:
                if jobs[job].get("lastRun"):
                    jobs[job]["lastRun"] = time.strftime("%c", time.localtime(float(jobs[job]["lastRun"])))
            if jobName:
                return OK, jobs[jobName]
            return OK, jobs
        except ConnectionRefusedException:
            return FAIL, ["Job server is not running. Use 'scheduler start' to start it."]
    

class Show(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "show")
        self.helpText = "show\tdisplay components of the system"
        history = History()
        merged = Merged()
        client = Client()
        include = Include()
        status = Status()
        package = Package()
        bom = Bom()
        jobs = Jobs()
        self.children = [merged, client, include, bom, history, status, package, jobs]
        self.level = 0
        self.cmdOwner = 1

