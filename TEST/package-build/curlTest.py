import bombardier.Server as S
import bombardier.Filesystem as FS
s = S.Server(FS.Filesystem())
a = "file:///d:/mydocuments/dev/bombardier-svn/trunk/TEST/server-directory/deploy/packages"
a = "file:///d:/mydocuments/dev/bombardier-svn/trunk/TEST/server-directory//packages.yml"
s.serverData = {"address": a}
data = s.serviceRequest('packages.yml')
upload = {"a":[1,2,3]}
data = s.serviceYamlRequest('test.yml', putData=upload)
print data
