from datetime import datetime
from django.db import models
from django.utils.translation import gettext_lazy as _

class Bom(models.Model):
    name = models.CharField(max_length=64)
    class Admin:
        pass
    def __str__(self):
        return self.name

class Include(models.Model):
    name = models.CharField(max_length=64)
    class Admin:
        pass
    def __str__(self):
        return self.name

class Client(models.Model):
    name = models.CharField(max_length=64)
    class Admin:
        pass
    def __str__(self):
        return self.name

