from django.db import models

class ServerConfig(models.Model):
    name = models.CharField(max_length=64)
    value = models.CharField(max_length=64)


