#!/usr/bin/env python

import sys, telnetlib, getopt, re

class CiscoBeast:
	def __init__(self, address, loginpass, enablepass):
        self.loginpass  = loginpass
        self.enablepass = enablepass
		self.tn = telnetlib.Telnet(address)  
		self.macAddrTable = []

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

if __name__ == '__main__':

	opts, args = getopt.getopt(sys.argv[1:], "")
	cb = CiscoBeast()
	cb.login()

	if args:
		mac = args[0]

		if len(args) > 1:
			vlan = args[1]
			cb.set_vlan(mac, vlan)

		else:
			print cb.get_vlan(mac)
	else:
		cb.get_mac_addr_table()

		for vlan, mac, port in cb.macAddrTable:
			print vlan, mac, port
