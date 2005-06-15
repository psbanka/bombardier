#!/cygdrive/c/Python24/python.exe
import sys, traceback, StringIO
try:
    ass += 1
except Exception:
    e = StringIO.StringIO()
    traceback.print_exc(file=e)
    e.seek(0)
    data = e.read()
    for line in data.split('\n'):
        print ">>>",line
