from datetime import datetime
from django.db import models
from django.utils.translation import gettext_lazy as _

class Bom(models.Model):
    name = models.CharField(max_length=64)

class Include(models.Model):
    name = models.CharField(max_length=64)

class Client(models.Model):
    name = models.CharField(max_length=64)

class Package(models.Model):
    name = models.CharField(max_length=64)

class ServerConfig(models.Model):
    name = models.CharField(max_length=64)
    value = models.CharField(max_length=64)


