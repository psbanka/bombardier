from datetime import datetime
from django.db import models
from django.utils.translation import gettext_lazy as _
from bombardier_core.static_data import OK
import os, glob
from commands import getstatusoutput as gso
from Exceptions import InvalidServerHome

class BadDistFile(Exception):
    def __init__(self, config_file):
        Exception.__init__(self)
        self.config_file = config_file
    def __str__(self):
        return self.config_file
    def __repr__(self):
        return self.__str__()

class BombardierModelFactory:
    def __init__(self):
        self.subdir = ''
        self.extension = 'yml'
        self.model = None
        config_entry = ServerConfig.objects.get(name="server_home")
        self.server_home = config_entry.value
        if not os.path.isdir(self.server_home):
            raise InvalidServerHome(server_home)

    def clean(self):
        config_objects = self.model.objects.all()
        for config_object in config_objects:
            config_object.delete()

    def create(self):
        # find all current files
        glob_string = os.path.join(self.server_home, self.subdir, "*.%s" % self.extension)
        config_files = glob.glob(glob_string)

        for config_file in config_files:
            base_name = config_file.split(os.path.sep)[-1]
            config_name = base_name.split('.%s' % self.extension)[0]
            # Make one
            config_object = self.model(name=config_name)
            config_object.initialize_from_file(config_file)
            if self.subdir == "dist":
                print ">>>>>>>>>",config_object.version
            config_object.save()

    def summarize(self):
        return [ object.name for object in self.model.objects.all() ]

class BomModelFactory(BombardierModelFactory):
    def __init__(self):
        BombardierModelFactory.__init__(self)
        self.subdir = "bom"
        self.model = Bom

class IncludeModelFactory(BombardierModelFactory):
    def __init__(self):
        BombardierModelFactory.__init__(self)
        self.subdir = "include"
        self.model = Include

class MachineModelFactory(BombardierModelFactory):
    def __init__(self):
        BombardierModelFactory.__init__(self)
        self.subdir = "machine"
        self.model = Machine

class PackageModelFactory(BombardierModelFactory):
    def __init__(self):
        BombardierModelFactory.__init__(self)
        self.subdir = "package"
        self.model = Package

class DistModelFactory(BombardierModelFactory):
    def __init__(self):
        BombardierModelFactory.__init__(self)
        self.subdir = "dist"
        self.extension = "tar.gz"
        self.model = Dist

class BombardierModel(models.Model):
    def initialize_from_file(self, config_file):
        pass
        #print " NOT IMPLEMENTED"

class Bom(BombardierModel):
    name = models.CharField(max_length=64)

class Include(BombardierModel):
    name = models.CharField(max_length=64)

class Machine(BombardierModel):
    name = models.CharField(max_length=64)

class Package(BombardierModel):
    name = models.CharField(max_length=64)


class Dist(BombardierModel):
    name = models.CharField(max_length=64)
    dist_name = models.CharField(max_length=64)
    version = models.CharField(max_length=10)
    desc = models.CharField(max_length=255)

    def set_desc(self, value):
        self.desc = value
        self.save()

    def initialize_from_file(self, config_file):
        status, output = gso("tar -tzf %s | grep setup.py" % config_file)
        if status != OK:
            raise BadDistFile(config_file)
        smallest_line = 99*' '
        for line in output.split('\n'):
            if len(line) < len(smallest_line):
                smallest_line = line
        status, _output = gso("cd /tmp && tar -xzvf %s %s" % (config_file, smallest_line))
        if status != OK:
            raise BadDistFile(config_file)
        setup_path = "/tmp/%s" % (smallest_line)

        _status, self.dist_name = gso("python %s --name" % setup_path)
        _status, self.version = gso("python %s --version" % setup_path)
        _status, self.desc = gso("python %s --description" % setup_path)
        _status, _output = gso('rm -f %s' % setup_path)

class ServerConfig(BombardierModel):
    name = models.CharField(max_length=64)
    value = models.CharField(max_length=64)


