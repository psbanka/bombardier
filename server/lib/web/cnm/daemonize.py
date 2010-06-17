"Script to run dispatcher as a daemon"
import os, sys, time
from django.core.management import setup_environ
from Dispatcher import Dispatcher
import settings, resource, yaml
from bombardier_core.static_data import OK
MAXFD = 1024
INFO_FILE = "dispatcher_info.yml"

def refork(parent_pid, server_home):
    "Inner fork, sets up Dispatcher"
    yaml_file = os.path.join(server_home, INFO_FILE)
    os.setsid()
    try:
        pid = os.fork()
    except OSError, err:
        raise Exception, "%s [%d]" % (err.strerror, err.errno)

    if (pid != 0):
        while not os.path.isfile(yaml_file):
            time.sleep(1)
        os._exit(0)
    else:
        setup_environ(settings)
        os.chdir(server_home)
        os.environ["PYRO_STORAGE"] = server_home
        import Pyro.core
        import Pyro.configuration
        #Pyro.configuration.PYRO_STORAGE = server_home
        data = Pyro.configuration.Pyro.config.PYRO_STORAGE
        Pyro.configuration.Pyro.config.PYRO_STORAGE = server_home

        open("/tmp/daemon_info.txt",'a').write("%s\n" % data)
        Pyro.core.initServer()
        daemon = Pyro.core.Daemon()
        uri = daemon.connect(Dispatcher(parent_pid),"dispatcher")
        pid = os.getpid()
        info_dict = { "dispatcher_uri": str(uri), "dispatcher_pid": pid }
        open(yaml_file, 'w').write( yaml.dump(info_dict) )
        daemon.requestLoop()

def daemonize(server_home):
    "Create a deamon process from Dispatcher"
    os.chdir(server_home)
    yaml_file = os.path.join(server_home, INFO_FILE)
    os.environ["PYRO_STORAGE"] = server_home
    parent_pid = os.getpid()
    if os.path.isfile(yaml_file):
        os.unlink(yaml_file)
    try:
        pid = os.fork()
    except OSError, err:
        raise Exception, "%s [%d]" % (err.strerror, err.errno)

    if (pid == 0):
        refork(parent_pid, server_home)
    else:
        while not os.path.isfile(yaml_file):
            time.sleep(1)
        return OK

    while not os.path.isfile(yaml_file):
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
    if len(sys.argv) > 1:
        settings.SERVER_HOME= sys.argv[1]
    sys.exit(daemonize(settings.SERVER_HOME))

