import cherrypy, Root

import threading, time

from static import *

import telnetlib, re


"""This thing allows queries and puts to the VLAN configuration system,
allowing a system to change its VLAN on the fly."""


class CiscoBeast(threading.Thread):
    def __init__(self, address, loginpass, enablepass):
        threading.Thread.__init__(self)
        self.loginpass  = loginpass
        self.enablepass = enablepass
        self.tn = telnetlib.Telnet(address)  
        self.macAddrTable = []
        self.macMatch = ''
        self.vlan     = ''
        self.delay    = 0

    def login(self):
        self.tn.read_until("word:")
        self.tn.write('%s\n' % self.loginpass)
        self.tn.read_until('>')
        self.tn.write('en\n')
        self.tn.read_until("word:")
        self.tn.write('%s\n' % self.enablepass)
        self.tn.read_until("#")
        self.tn.write('term len 0\n')

    def get_mac_addr_table(self):
        self.tn.write('show mac-address-table\n')
        self.tn.read_until('#')
        raw = self.tn.read_until('#')
        lines = raw.split('\r\n')

        for line in lines[5:]:
            ln = line.split()
            if ln == []:
            	break
            elif ln[2] == 'dynamic':
            	vlan, mac, port = ln[0], ln[1], ln[4]

            self.macAddrTable.append((vlan, mac, port))

    def mac2tuple(self, macMatch):
        macMatch = macMatch.lower()
        macMatch = re.sub(':', '', macMatch)
        macMatch = re.sub('-', '', macMatch)
        macMatch = re.sub('\\.', '', macMatch)

        if not self.macAddrTable:
            self.get_mac_addr_table()

        for camEntry in self.macAddrTable:
            mac = re.sub("\\.", "", camEntry[1])
            if  macMatch == mac:
            	return camEntry 

    def get_vlan(self, macMatch):
        match = self.mac2tuple(macMatch)
        if match: return match[0]

    def get_port(self, macMatch):
        match = self.mac2tuple(macMatch)
        if match: return match[2]

    def set_vlan(self, macMatch, vlan):
        port = self.get_port(macMatch)

        self.tn.write('config t\n')
        self.tn.read_until('#')

        self.tn.write('int %s\n' %port)
        self.tn.read_until('#')
        self.tn.write('switchport access vlan %s\n' %vlan)
        self.tn.read_until('#')
        self.tn.write('exit\n')
        self.tn.read_until('#')
        self.tn.write('exit\n')

    def run(self):
        time.sleep(self.delay)
        self.set_vlan(self.macMatch, self.vlan)


class Vlan(Root.Root):

    known_methods = ["GET", "PUT"]

    def GET(self, macaddress=None):
        cherrypy.response.headerMap["Content-type"] = "text/plain"
        cb = CiscoBeast(SWITCH_ADDRESS, SWITCH_LOGINPASS, SWITCH_ENABLEPASS)
        cb.login()
        output = []
        if macaddress:
            output.append("%s\n" % cb.get_vlan(macaddress))
        else:
            cb.get_mac_addr_table()
            for vlan, mac, port in cb.macAddrTable:
                output.append("%s,%s,%s" % (vlan, mac, port))
        return "\n".join(output)

    def PUT(self, macaddress):
        cherrypy.response.headerMap["Content-type"] = "text/plain"
        output = []
        vlan = cherrypy.request.body.read().strip()
        if not vlan:
            cherrypy.response.status = 400
            return "VLAN Service received a PUT with no vlan data" 
        output.append("Setting vlan %s for mac address %s" % (vlan, macaddress))
        cb = CiscoBeast(SWITCH_ADDRESS, SWITCH_LOGINPASS, SWITCH_ENABLEPASS)
        cb.login()
        currentVlan = cb.get_vlan(macaddress)
        if currentVlan == "":
            output.append("Could not look up current VLAN")
            return "\n".join(output)
        elif currentVlan == vlan:
            output.append("Already set.")
            return "\n".join(output)
        else:
            output.append("Setting new VLAN from %s to %s" % ( currentVlan, vlan ))
            cb.macMatch = macaddress
            cb.vlan     = vlan
            cb.delay    = 2
            cb.start()
            return "\n".join(output)
