import shelve, md5
s = shelve.open("packages.dat")
print "fileName, md5sum"
for item in s.keys():
	p = s[item]
        fullName = p['install']['fullName']+".spkg"
	if fullName == ".spkg":
		print "bad file:", item
		continue
	computed = md5.new(open(fullName, 'rb').read()).hexdigest()
	print fullName, computed
	p['install']['md5sum'] = computed
	s[item] = p
s.close()
