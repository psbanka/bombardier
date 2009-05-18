#!/usr/bin/python

import sys, os, readline, time
import yaml
import PinshCmd, ConfigField, Integer, BomHostField, PackageField
from bombardier_core.static_data import OK, FAIL

TERM_OVERCOUNT = 8 # For some reason, the term width seems too long...

class ShowCommand(PinshCmd.PinshCmd):
    def __init__(self, name, help_text):
        PinshCmd.PinshCmd.__init__(self, name, help_text)
        self.config_field = None
        self.cmd_owner = 1

    def cmd(self, tokens, no_flag, slash):
        if no_flag:
            return FAIL, []
        if len(tokens) < 3:
            return FAIL, ["Incomplete command."]
        current_dict = self.config_field.get_specific_data(tokens, 2)
        return OK, yaml.dump(current_dict, default_flow_style=False).split('\n')

class Merged(ShowCommand):
    def __init__(self):
        ShowCommand.__init__(self, "merged", "merged\tdisplay a merged configuration")
        self.config_field = ConfigField.ConfigField()
        self.children = [self.config_field]

class Client(ShowCommand):
    def __init__(self):
        ShowCommand.__init__(self, "client", "client\tshow the configuration for one client")
        self.config_field = ConfigField.ConfigField(data_type=ConfigField.CLIENT)
        self.children = [self.config_field]

class Include(ShowCommand):
    def __init__(self):
        ShowCommand.__init__(self, "include", "include\tshow a shared include file")
        self.config_field = ConfigField.ConfigField(data_type=ConfigField.INCLUDE)
        self.children = [self.config_field]

class Bom(ShowCommand):
    def __init__(self):
        ShowCommand.__init__(self, "bom", "bom\tshow a bill of materials")
        self.config_field = ConfigField.ConfigField(data_type=ConfigField.BOM)
        self.children = [self.config_field]

class Package(ShowCommand):
    def __init__(self):
        ShowCommand.__init__(self, "package", "package\tdisplay information about a given package")
        self.config_field = ConfigField.ConfigField(data_type=ConfigField.PACKAGE)
        self.children = [self.config_field]

class History(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "history")
        self.help_text = "history\tdisplay the history of commands"
        self.integer  = Integer.Integer(min=1, max=1000)
        self.children = [self.integer]
        self.level = 0
        self.cmd_owner = 1

    def cmd(self, tokens, no_flag, slash):
        if len(tokens) == 2 or tokens[-1].strip()=='':
            number = 20
        else:
            try:
                number = int(tokens[2])
            except:
                return FAIL, ["%s is not a number." % tokens[2]]
        hlen = readline.get_current_history_length()
        if hlen < number:
            number = hlen
        output = []
        for i in range(hlen-number, hlen):
            output.append("%4d\t%s" % (i, readline.get_history_item(i)))
        return OK, output

def printify(input_objects):
    text_list = list(input_objects)
    text_list.sort()
    output = []
    if not text_list:
        return []
    max_length = max( [ len(t) for t in text_list ] )
    columns = (system_state.termwidth - TERM_OVERCOUNT) / max_length
    for i in range(0, len(text_list), columns):
        newLine = ''
        for item in text_list[i:i+columns]:
            newLine += item.ljust(max_length+2)
        output.append(newLine)
    return output

class Status(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "status")
        self.help_text = "status\tstatus of a host"
        self.bom_host_field = BomHostField.BomHostField()
        self.children = [self.bom_host_field]
        self.level = 0
        self.cmd_owner = 1

    def cmd(self, tokens, no_flag, slash):
        if no_flag:
            return FAIL, []
        if len(tokens) < 3:
            return FAIL, ["Incomplete command."]
        host_name = tokens[2]
        status_file = os.path.join(system_state.server_home, "status", "%s.yml" % host_name)
        if not os.path.isfile(status_file):
            return FAIL, ["No status on file (%s)" % status_file]
        installed, broken = PackageField.getNamesFromProgress(host_name, False)
        total_packages = PackageField.getPackageNamesFromBom(host_name)
        missing = []
        accounted_packages = list(installed.union(broken))
        for item in total_packages:
            found = False
            for package_name in accounted_packages:
                if package_name.startswith(item):
                    found = True
                    break
            if not found:
                missing.append(item)
        if installed:
            output = ["Installed:",[printify(list(installed))]]
        else:
            output = ["Installed:",[["NONE"]]]
        if broken:
            output += ["Broken:",[printify(list(broken))]]
        else:
            output += ["Broken:",[["NONE"]]]
        if missing:
            output += ["Not Installed:",[printify(list(missing))]]
        else:
            output += ["Not Installed:",[["NONE"]]]
        return OK, output


class Show(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "show")
        self.help_text = "show\tdisplay components of the system"
        history = History()
        merged = Merged()
        client = Client()
        include = Include()
        status = Status()
        package = Package()
        bom = Bom()
        self.children = [merged, client, include, bom, history, status, package]
        self.level = 0
        self.cmd_owner = 1

