#!/usr/bin/python

import sys

import PinshCmd, Expression, Configure, libUi, libCmd
from commonUtil import *

import libFirewall

DEBUG = 0

DF = "/bin/df"
IPTABLES = "/sbin/iptables"
IP = "/sbin/ip"

class Disk(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "disk")
        self.helpText = "disk\tshows disk utilization"
        self.auth = USER
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if noFlag:
            return FAIL, []
        status, output = libCmd.runcmd(DF+" -h")
        if status == FAIL:
            return FAIL, ["Unable to determine disk status."]
        else:
            return OK, [output]

class Running(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "running-config")
        self.helpText = "running-config\tshows all current configuration"
        self.auth = ADMIN
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if noFlag:
            return FAIL, []
        libUi.user("Building configuration...\n")
        return Configure.running()

class Config(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "config")
        self.helpText = "config\tshows all saved configuration"
        self.auth = ADMIN
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if noFlag:
            return FAIL, []
        libUi.user("Building configuration...\n")
        return Configure.config()

class Ip(PinshCmd.PinshCmd):

    class Address(PinshCmd.PinshCmd):
        def __init__(self):
            PinshCmd.PinshCmd.__init__(self, "address")
            self.helpText = "address\tshows IP address information"
            self.auth = USER
            self.cmdOwner = 1

        def cmd(self, tokens, noFlag, slash):
            if noFlag:
                return FAIL, []
            status, output = libCmd.runcmd(IP+" address")
            if status == FAIL:
                return FAIL, ["unable to determine ip interface information"]
            else:
                 return OK, [output.split('\n')]
        
    class Route(PinshCmd.PinshCmd):
        def __init__(self):
            PinshCmd.PinshCmd.__init__(self, "route")
            self.helpText = "route\tshows the IP route table"
            self.auth = USER
            self.cmdOwner = 1

        def cmd(self, tokens, noFlag, slash):
            if noFlag:
                return FAIL, []
            tatus, output = libCmd.runcmd(IP+" route")
            if output == FAIL:
                return FAIL, ["unable to determine ip routing information"]
            else:
                return OK, [output.split('\n')]

    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "ip")
        self.helpText = "ip\tshows IP information about this host"
        self.address = Ip.Address()
        self.route = Ip.Route()
        self.children = [self.address, self.route]
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        output = []
        if noFlag:
            return FAIL, []
        status, newOutput = self.address.cmd(tokens, noFlag, slash)
        output.append(newOutput)
        status, newOutput = self.route.cmd(tokens, noFlag, slash)
        output.append(['\n'])
        output.append(newOutput)
        return OK, output

class Firewall(PinshCmd.PinshCmd):

    class Nat(PinshCmd.PinshCmd):
        def __init__(self):
            PinshCmd.PinshCmd.__init__(self, "nat")
            self.helpText = "nat\tshows the current firewall NAT tables"
            self.cmdOwner = 1

        def cmd(self, tokens, noFlag, slash):
            if noFlag:
                return FAIL, []
            status, output = libCmd.runcmd(IPTABLES+" -t nat -nvL")
            if status == FAIL:
                return FAIL, ["unable to determine address translation"]
            else:
                return OK, ["Current NAT rules\n=======================", output]

    class Policy(PinshCmd.PinshCmd):
        def __init__(self):
            PinshCmd.PinshCmd.__init__(self, "policy")
            self.helpText = "policy\tshows the current firewall filtering policy"
            self.cmdOwner = 1

        def cmd(self, tokens, noFlag, slash):
            if noFlag:
                return FAIL, []
            status, output= libCmd.runcmd(IPTABLES+" -nvL")
            if status == FAIL:
                return FAIL, ["Unable to determine firewall filters."]
            else:
                return OK, ["Current filtering rules\n=======================", output]

    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "firewall")
        self.helpText = "firewall\tshows the current firewall policy"
        self.policy = Firewall.Policy()
        self.nat = Firewall.Nat()
        self.saved = PinshCmd.PinshCmd("saved","saved\tshow a saved firewall XML policy")
        self.pName = Expression.Expression()
        self.pName.helpText = "<name>\tfirewall policy name"
        self.children = [self.policy, self.nat, self.saved]
        self.saved.children = [self.pName]
        self.auth = ADMIN
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if noFlag:
            return FAIL, []
        if len(tokens) == 4:
            policyName = tokens[3]
            output = "Firewall policy: "+policyName+"\n"
            status, policyData = libFirewall.showPolicy(policyName)
            if status == FAIL:
                return FAIL, []
            return OK, [output, policyData]
        else:
            status1, output1 = self.policy.cmd(tokens, noFlag, slash)
            status2, output2  = self.nat.cmd(tokens, noFlag, slash)

            if status1 == OK and status2 == OK:
                return OK, [output1, output2]
            return FAIL, []

class Show(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "show")
        self.helpText = "show\tdisplay components of the system"
        disk = Disk()
        firewall = Firewall()
        ip = Ip()
        running = Running()
        config = Config()
        self.children = [disk, firewall, ip, running, config]
        self.level = 0
        self.cmdOwner = 1

if __name__ == "__main__":
    from libTest import *
    status = startTest()
    show = Show()

    import libRunningNetwork
    currentDefaultRoute = libRunningNetwork.getRunningDefaultRoute()[0]

    status = testMe2(show, "show ip", OK, '', status)
    status = testMe2(show, "show ip address", OK, '(lo:)', status)
    status = testMe2(show, "show ip route", OK, '('+currentDefaultRoute.replace('.','\.')+')', status)
    status = testMe2(show, "show disk", OK, '(/dev/hda)', status)
    status = testMe2(show, "show firewall nat", OK, '(Chain POSTROUTING)', status)

    status = testMe2(show, "show firewall pol", OK, '(Chain OUTPUT)', status)

    status = testMe2(show, "show firewall policy", OK, '(Chain OUTPUT)', status)
    status = testMe2(show, "show firewall", OK, '(Chain OUTPUT)', status)
    status = testMe2(show, "show firewall", OK, '(Chain POSTROUTING)', status)
    #status = testMe2(show, "show firewall saved test", OK, '(Firewall policy)', status)
    endTest(status)
