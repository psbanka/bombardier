"Script to run dispatcher as a daemon"
import os, sys, time
from django.core.management import setup_environ
from Dispatcher import Dispatcher
import settings, resource, yaml
import Pyro.core
from bombardier_core.static_data import OK
MAXFD = 1024
INFO_FILE = "dispatcher_info.yml"

def refork():
    "Inner fork, sets up Dispatcher"
    os.setsid()
    try:
        pid = os.fork()
    except OSError, err:
        raise Exception, "%s [%d]" % (err.strerror, err.errno)

    if (pid != 0):
        while not os.path.isfile(INFO_FILE):
            time.sleep(1)
        os._exit(0)
    else:
        setup_environ(settings)
        Pyro.core.initServer()
        daemon = Pyro.core.Daemon()
        uri = daemon.connect(Dispatcher(),"dispatcher")
        pid = os.getpid()
        info_dict = { "dispatcher_uri": str(uri), "dispatcher_pid": pid }
        open(INFO_FILE, 'w').write( yaml.dump(info_dict) )
        daemon.requestLoop()

def daemonize():
    "Create a deamon process from Dispatcher"
    if os.path.isfile(INFO_FILE):
        os.unlink(INFO_FILE)
    try:
        pid = os.fork()
    except OSError, err:
        raise Exception, "%s [%d]" % (err.strerror, err.errno)

    if (pid == 0):
        refork()
    else:
        while not os.path.isfile(INFO_FILE):
            time.sleep(1)
        return OK

    while not os.path.isfile(INFO_FILE):
        time.sleep(1)
    maxfd = resource.getrlimit(resource.RLIMIT_NOFILE)[1]
    if (maxfd == resource.RLIM_INFINITY):
        maxfd = MAXFD

    for file_descriptor in range(0, maxfd):
        try:
            os.close(file_descriptor)
        except OSError:
            pass

    os.dup2(0, 1)
    os.dup2(0, 2)

    return OK

if __name__ == "__main__":
    sys.exit(daemonize())

