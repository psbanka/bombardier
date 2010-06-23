from django.db import models

class ServerConfig(models.Model):
    name = models.CharField(max_length=64)
    value = models.CharField(max_length=64)

class Comment(models.Model):
    id = models.AutoField(primary_key=True)
    time = models.DateTimeField(auto_now=False, auto_now_add=True)
    publish = models.BooleanField()
    username = models.CharField(max_length=64)
    text = models.TextField()

class CommentedJob(models.Model):
    comment_id = models.ForeignKey('Comment')
    job_name = models.CharField(max_length=64)
    username = models.CharField(max_length=64)
    machine_name = models.CharField(max_length=64)
