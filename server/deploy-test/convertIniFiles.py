#!/cygdrive/c/Python23/python.exe

import os, shelve
import ConfigParser

def getFullName(packageName):
	files=os.listdir('.')
	candidate = ''
	for file in files:
		if not file.endswith('.spkg'):
			continue
		if not file.startswith(packageName):
			continue
		fileName = file[:file.rfind('.spkg')]
		if candidate == '':
			candidate = fileName
			continue
		curver = int(candidate.split('-')[-1])
		newver = int(fileName.split('-')[-1])
                print packageName, curver, newver
		if newver > curver:
			print "accepted:", fileName, "over", candidate
			candidate = fileName
		else:
			print "rejected:",fileName
	return candidate

files = os.listdir('.')
if os.path.isfile('packages.dat'):
	os.unlink('packages.dat')
output = shelve.open("packages.dat")
for file in files:
	if file.endswith('.ini'):
		d = {}
		config = ConfigParser.ConfigParser()
		config.read(file)
		for section in config.sections():
		        d[section] = {}
			for option in config.options(section):
				d[section][option] =  config.get(section, option)
		packageName = file[:file.rfind('.ini')]
                if not 'install' in d.keys():
                        print "bad package:",packageName
			continue
		#print 'packageName:',packageName
                d['install']['fullName'] = getFullName(packageName) 
		#print "full Name:",d['install']['fullName']
		output[packageName] = d
output.close()
