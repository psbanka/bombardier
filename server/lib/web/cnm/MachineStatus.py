"Package management information"

from bombardier_core.mini_utility import strip_version, get_installed_uninstalled_times
from bombardier_core.static_data import OK, FAIL
import syck, os
from MachineConfig import MachineConfig
from Exceptions import MachineStatusException, MachineConfigurationException
PROGRESS = "install-progress"
LOCAL_PACKAGES = "local-packages"

class MachineStatus:

    def __init__(self, server_home, machine_name):
        self.server_home = server_home
        self.machine_name = machine_name
        self.status_data  = {}

    def freshen(self):
        self.status_data = ''
        status_file = os.path.join(self.server_home, "status",
                                   "%s.yml" % self.machine_name)
        if os.path.isfile(status_file):
            try:
                self.status_data = syck.load(open(status_file).read())
            except Exception, exc:
                if exc[0] == "syntax error":
                    self.status_data = ''
                    msg = "Syntax error in status information for %s"
                    raise MachineStatusException(msg)
            return self.check_status_data()
        return FAIL

    def check_status_data(self):
        "Check for proper elements being present in status data"
        valid = type(self.status_data) == type({}) and \
                PROGRESS in self.status_data and \
                LOCAL_PACKAGES in self.status_data
        return valid

    def get_progress_data(self):
        self.freshen()
        return self.status_data.get("install-progress")

    def get_names_from_progress(self, stripped=False):
        progress_data = self.get_progress_data()
        if progress_data == None:
             (installed_package_names, broken_package_names) = ([],[])
        else:
            pkg_info = get_installed_uninstalled_times(progress_data)
            unstripped_inst_pkgs    = [package_name[0] for package_name in pkg_info["installed"]]
            unstripped_broken_pkgs  = [package_name[0] for package_name in pkg_info["broken_installed"]]
            unstripped_broken_pkgs += [package_name[0] for package_name in pkg_info["broken_uninstalled"]]

            if stripped:
                installed_package_names = [ strip_version(x) for x in unstripped_inst_pkgs]
                broken_package_names    = [ strip_version(x) for x in unstripped_broken_pkgs]
            else:
                installed_package_names = unstripped_inst_pkgs
                broken_package_names    = unstripped_broken_pkgs
        return (set(installed_package_names), set(broken_package_names))

    def get_package_names_from_progress(self):
        "Pull status yaml data from local file"
        status_yml = os.path.join(self.server_home, "status", \
                                  "%s.yml" % self.machine_name)
        if not os.path.isfile(status_yml):
            msg = "Cannot retrieve status (NO FILE: %s)" % status_yml
            raise MachineStatusException(msg)
        yml = syck.load( open(status_yml).read() )
        if yml == None:
            msg = "Cannot retrieve status (EMPTY FILE: %s)" % status_yml
            raise MachineStatusException(msg)
        return yml

    def get_all_package_names(self, config_packages):
        "Get package names from status and this object and union them together."
        try:
            package_list = self.get_package_names_from_progress().get(PROGRESS, {})
        except MachineStatusException:
            package_list = []
        package_names = set([strip_version(x) for x in package_list])
        package_names = package_names.union(set(config_packages))
        return list(package_names)

    def get_package_data(self, package_name):
        "Get package metadata from a local yaml file."
        yml_path = os.path.join(self.server_home, "package",
                                "%s.yml" % package_name)
        package_data = {}
        if not os.path.isfile(yml_path):
            msg = "Requested invalid package: %s" % package_name
            raise MachineConfigurationException(self.machine_name, msg)
        else:
            package_data = syck.load(open(yml_path).read())
        return package_data

    def get_package_names_from_bom(self):
        machine_config = MachineConfig(self.machine_name, '', self.server_home)
        status = machine_config.merge()
        if status == FAIL:
            print " %% Bad config file for %s." % self.machine_name
            return []
        return set(machine_config.data.get("packages", []))

    def machine_install_status(self):
        output = {"status": FAIL}
        try:
            installed, broken = self.get_names_from_progress(False)
        except IOError:
            output["error_message"] = "Cannot read status file"
            return output

        try:
            total_packages = self.get_package_names_from_bom()
        except IOError:
            output["error_message"] = "Cannot read configuration data"
            return output

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
        output["installed"] = list(installed)
        output["broken"] = list(broken)
        output["not_installed"] = list(missing)
        output["status"] = OK
        return output