from datetime import datetime
from django.db import models
from django.utils.translation import gettext_lazy as _
from bombardier_core.static_data import OK

class BadDistFile(Exception):
    def __init__(self, config_file):
        Exception.__init__(self)
        self.config_file = config_file
    def __str__(self):
        return self.config_file
    def __repr__(self):
        return self.__str__()

class BombardierModel(models.Model):
    def initialize_from_file(self, config_file):
        print dir(self)
        print " NOT IMPLEMENTED"

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
    version = models.CharField(max_length=10)
    desc = models.CharField(max_length=255)
    def initialize_from_file(self, config_file):
        status, output = gso("tar -tzf %s | grep setup.py" % config_file)
        if status != OK:
            raise BadDistFile(config_file)
        smallest_line = 99*' '
        for line in output.split('\n'):
            if line < smallest_line:
                smallest_line = line
        status, _output = gso("cd /tmp && tar -xzvf %s %s" % (config_file, smallest_line))
        if status != OK:
            raise BadDistFile(config_file)
        status, output = gso("python /tmp/%s --name" % smallest_time)
        if status != OK:
            raise BadDistFile(config_file)
        print "NAME: %s" % output
        print "HELLO WORLD!!"

class ServerConfig(BombardierModel):
    name = models.CharField(max_length=64)
    value = models.CharField(max_length=64)


