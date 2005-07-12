import os, shutil

class CADir:
    def __init__(self, directory):
        self.directory = directory 
        self.serial = os.path.join(directory, "serial")
        self.cert = os.path.join(directory, "ca-cert.pem")
        self.key = os.path.join(directory, "ca-key.pem")
        nameFile = os.path.join(self.directory, "name")

        self.confFile = os.path.join(directory, "conf", "ca-cert-conf")
        self.confFileTmpl = os.path.join(directory, 
                                           "conf", "ca-cert-conf.tmpl")

        if os.path.exists(nameFile):
            self.name = open(nameFile).read()


    def init(self, name):
        directory = self.directory
        self.name = name
        os.system("mkdir -p '%s'" %directory)
        os.mkdir(os.path.join(directory, "certs"))
        os.mkdir(os.path.join(directory, "req"))
        shutil.copytree("conf", os.path.join(directory, "conf"))

        open(self.serial, 'w').write("01")
        template = open(self.confFileTmpl).read()
        open(self.confFile, 'w').write(template % self.name)

        command = "openssl req -x509 -nodes -days 3650 -newkey rsa:1024 \
                   -keyout %s -out %s -config %s 2>/dev/null" %(self.key, 
                                                            self.cert, 
                                                            self.confFile)

        os.system(command)

    def sign(self, request):
        request = request.strip()
        command = "openssl x509 -req -days 3650 -CA %s \
                   -CAkey %s -CAserial %s" %(self.cert, 
                                             self.key,
                                             self.serial) 

        childStdin, childStdout, childStderr = os.popen3(command)
        childStdin.write(request)
        childStdin.close()
        cert = childStdout.read()
        err = childStderr.read()
        return cert
