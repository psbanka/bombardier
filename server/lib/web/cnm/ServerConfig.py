#!/usr/bin/env python

import sys, os
import stat
import yaml, syck

GLOBAL_CONFIG_FILE = "/etc/bombardier.yml"

class ConfigFileException(Exception):
    def __init__(self, message, file_name):
        e = Exception()
        Exception.__init__(e)
        self.file_name = file_name
        self.message = message
    def __repr__(self):
        return "%% Error processing config file %s: %s" \
               %(self.file_name, self.message)
    def __str__(self):
        return self.__repr__()

class ServerConfig:
    def __init__(self):
        self.default_group = "root"
        self.debug = True
        self.global_config = {}
        self.server_home = None
        self.tmp_path = None
        self.load_config()

    def write_config(self, option, value):
        self.global_config[option] = value
        open(GLOBAL_CONFIG_FILE, 'w').write(yaml.dump(self.global_config))
        os.system("chgrp %s %s 2> /dev/null" % (self.default_group, GLOBAL_CONFIG_FILE))
        os.system("chmod 660 %s 2> /dev/null" % (GLOBAL_CONFIG_FILE))

    def load_config(self):
        try:
            if not os.path.isfile(GLOBAL_CONFIG_FILE):
                raise ConfigFileException("File not found.", GLOBAL_CONFIG_FILE)
            st = os.stat(GLOBAL_CONFIG_FILE)
            mode = st[stat.ST_MODE]
            permission = stat.S_IMODE(mode)
            if mode & stat.S_IROTH:
                raise ConfigFileException("Incorrect permissions %s." %permission, GLOBAL_CONFIG_FILE)
            self.global_config=syck.load(open(GLOBAL_CONFIG_FILE).read())
            if not self.global_config.has_key("tmp_path"):
                self.global_config["tmp_path"] = "/tmp"
            self.tmp_path = self.global_config["tmp_path"]
            if not self.global_config.has_key("default_group"):
                self.global_config["default_group"] = "root"
            self.default_group = self.global_config["default_group"]
            self.server_home = self.global_config.get("server_home")
        except ConfigFileException, e:
            print e
            sys.exit(1)
        except syck.error, e:
            print "Yaml error loading config file %s" %GLOBAL_CONFIG_FILE
            print e[0]
            sys.exit(1)
        except IOError: 
            print "%% The global Bombardier configuration file (%s) is not readable."\
                  % GLOBAL_CONFIG_FILE
            sys.exit(1)

if __name__ == "__main__":
    from libtest import starttest, runtest, endtest 
    starttest()

    #status = runtest(mode.reprompt, [], none, status)

    endTest(status)
