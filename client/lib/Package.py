#!/cygdrive/c/Python24/python.exe

# Package.py: This guy is responsible for most of the actual work that
# gets done in Bombardier. It's responsible for getting, extracting,
# installing, verifying, etc. packages from the repository onto the
# local system.

# Copyright (C) 2005 Peter Banka

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

import os, time, datetime, glob, random
import sys, StringIO, traceback

import Spkg
import MetaData
from bombardier_core.mini_utility import evalBoolean, getPackagePath, getSpkgPath
from Exceptions import BadPackage, FeatureRemovedException, RebootRequiredException
from threading import Thread
from bombardier_core.Logger import Logger
from bombardier_core.static_data import OK, FAIL, AVERAGE, REBOOT
from bombardier_core.static_data import INSTALL, UNINSTALL, CONFIGURE, VERIFY

class JobThread(Thread):
    def __init__(self, import_string, cmd, config):
        Thread.__init__(self)
        self.import_string = import_string
        self.cmd = cmd
        self.config = config
        self.cmd_status = None

    def run(self):
        Logger.debug("Running %s..." % self.cmd)
        try:
            exec(self.import_string)
            exec("self.cmd_status = %s" % self.cmd)
        except StandardError, e:
            Logger.error( "Failed to run %s" % self.cmd )
            e = StringIO.StringIO()
            traceback.print_exc(file=e)
            e.seek(0)
            data = e.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg += "\n||>>>%s" % line
            Logger.error(ermsg)
            self.cmd_status = FAIL

class Job:
    def __init__(self, import_string, cmd, config):
        self.import_string = import_string
        self.cmd = cmd
        self.config = config
        self.start_time = None
        self.jt = None
        self.job_status = None

    def is_running(self):
        if self.jt:
            if self.jt.isAlive():
                return True
            if type(self.jt.cmd_status) == type(0) \
               or type(self.jt.cmd_status) == type('str'):
                Logger.debug("-- status: %s" % (self.jt.cmd_status))
                self.job_status = self.jt.cmd_status
            else:
                msg = "Invalid return status (type: %s)"
                Logger.error(msg % (type(self.jt.cmd_status)))
                self.job_status = FAIL
        return False

    def execute(self):
        self.start_time = time.time()
        status = FAIL
        if self.is_running():
            msg = "Refusing to run %s; it is already running" % self.cmd
            Logger.error(msg)
            return FAIL
        self.jt = JobThread(self.import_string, self.cmd, self.config)
        self.jt.start()
        Logger.info("Started job...")
        counter = 1
        while self.is_running():
            time.sleep(1)
            counter += 1
            if not counter % 100:
                msg = "Waiting for completion (%s)..."
                Logger.info(msg % time.strftime(time.ctime()))
                counter = 1
        status = self.jt.cmd_status
        return status

    def kill_thread(self, timeout=5):
        if not self.is_running():
            return OK
        self.jt.join(timeout)
        return OK

class Package:

    """This class provides an abstraction for a downloadable,
    installable, verifiable, and uninstallable thing. Each
    package is held in the repository"""

    def __init__(self, name, repository, config, filesystem,
                 operating_system, instance_name):
        self.name         = name
        self.repository   = repository
        self.filesystem   = filesystem
        self.operating_system = operating_system
        self.instance_name = instance_name
        self.action       = INSTALL
        self.installed    = False
        self.dependencies = []
        self.depends_on_me = []
        self.console      = False
        self.full_name    = ''
        self.reboot       = False
        self.package_version = 0
        self.checksum     = ''
        self.config       = config
        self.status       = OK
        self.priority     = AVERAGE
        self.meta_data    = MetaData.MetaData({})
        self.working_dir  = ''
        self.scripts_dir  = ''
        self.maint_dir    = ''
        self.downloaded   = False
        self.dependency_errors = []

    ############################################ PUBLIC METHODS

    def initialize(self):
        try:
            self.meta_data = self.repository.get_meta_data(self.name)
        except BadPackage, bp:
            self._invalidate(bp)
            return
        self._check_meta_data()
        self.checksum = self.meta_data.data['install'].get('md5sum')
        if not self.checksum:
            ermsg = "Package %s does not have a checksum field"\
                    " (not checking)" % (self.name)
            Logger.warning(ermsg)
        if self.meta_data.data['install'].get('md5list'):
            self.checksum = self.meta_data.data['install']['md5list']
        chk = self.meta_data.data["install"].get('console')
        self.console = evalBoolean(chk)
        chk = self.meta_data.data["install"].get('reboot')
        self.reboot = evalBoolean(chk)
        chk = self.meta_data.data.get("package-version")
        if type(chk) == type(1):
            self.package_version = chk
        self._eval_priority()
        self._gather_dependencies()

    def get_configuration(self):
        return self.meta_data.data.get("configuration")

    def add_dependency_error(self, dependencyName):
        errmsg = "BOM file is incomplete: should contain %s" % dependencyName
        Logger.warning(errmsg)
        self.dependency_errors.append(dependencyName)

    def install_and_verify(self, package_list=[], dry_run=False):
        if self._download() != OK:
            return FAIL
        if self.action == INSTALL:
            status = self._install(package_list, dry_run=dry_run)
            if not dry_run:
                if status == OK:
                    status = self.verify()
                self._write_progress()
            return status
        raise FeatureRemovedException(self.action)

    def configure(self):
        if self._download() != OK:
            return FAIL
        return self._find_cmd(CONFIGURE)

    def verify(self): 
        if self._download() != OK:
            return FAIL
        message = "Verifying package %s" % self.full_name
        Logger.info(message)
        self.status = self._find_cmd(VERIFY)
        if self.action != INSTALL:
            self._write_progress()
        Logger.info("Verify result for %s : %s" % (self.full_name, self.status))
        return self.status

    def uninstall(self, dry_run=False):
        self.action = UNINSTALL
        if self._download() != OK:
            return FAIL
        if self.status != OK:
            return self.status
        dry_run_string = ""
        if dry_run:
            dry_run_string = " --DRY_RUN-- "
        Logger.info("Uninstalling package %s%s" % (self.name, dry_run_string))
        self.status = self._find_cmd(UNINSTALL, dry_run=dry_run)
        if not dry_run:
            self._write_progress()
        msg = "Uninstall result for %s%s : %s"
        Logger.info(msg % (self.full_name, dry_run_string, self.status))
        return self.status

    def execute_maint_script(self, script_name):
        if self._download() != OK:
            return FAIL
        # remove old history
        output_path = os.path.join(getSpkgPath(), self.instance_name,
                                  "output", "%s-output.yml" % script_name)
        self.filesystem.rmScheduledFile(output_path)
        message = "Executing (%s) inside package (%s)"
        Logger.info(message % (script_name, self.full_name))
        script_path = "%s/%s.py" %(self.maint_dir, script_name)
        start_dir = os.getcwd()
        os.chdir(self.maint_dir)
        if not os.path.isfile(script_path):
            msg = "%s does not exist" %script_path
            raise BadPackage, (self.name, msg)
        sys.path.append(self.maint_dir )
        import_string = 'import %s' % script_name
        cmd = 'status = %s.execute(self.config, Logger)' % script_name
        job = Job(import_string, cmd, self.config)
        status = job.execute()
        sys.path.remove(self.maint_dir)
        os.chdir(start_dir)
        if status == None:
            status = OK
        if type(status) != type(1):
            msg = "Invalid status type (%s: '%s')"
            Logger.warning(msg % (type(status), status))
            status = FAIL
        return status

    ############################################ PRIVATE METHODS

    def _invalidate(self, exception_object):
        erstr = "INVALID PACKAGE: %s" % self.name
        Logger.error(erstr)
        Logger.error(str(exception_object))
        self.status = FAIL
        
    def _gather_dependencies(self):
        self.dependencies = self.meta_data.data.get("dependencies")
        if type(self.dependencies) == type(None):
            self.dependencies = []
        elif type(self.dependencies) == type('string'):
            self.dependencies = [self.dependencies]
        elif type(self.dependencies) == type([]):
            pass
        else:
            errmsg = "Unsupported dependencies data for %s. "\
                     "Must be a list. [%s] instead." % (self.name, self.dependencies)
            Logger.warning(errmsg)
            self.dependencies = []
    
    def _eval_priority(self):
        self.priority = self.meta_data.data["install"].get('priority')
        if not self.priority:
            ermsg = "Package %s does not have a priority "\
                    "(assuming %s)" % (self.name, AVERAGE)
            Logger.warning(ermsg)
            self.priority = AVERAGE
        else:
            try:
                self.priority = int(self.priority)
            except ValueError:
                ermsg = "Package %s has an invalid priority value"\
                        "(assuming %s)" % (self.name, AVERAGE)
                Logger.warning(ermsg)
                self.priority = AVERAGE

    def _check_meta_data(self):
        self.status = FAIL
        if self.meta_data.data:
            if type(self.meta_data.data) == type(dict()):
                if self.meta_data.data.get("install"):
                    if type(self.meta_data.data["install"]) == type({}):
                        self.full_name = self.meta_data.data['install'].get('fullName')
                        if not self.full_name:
                            self.full_name = self.meta_data.data['install'].get('full_name')
                        if self.full_name:
                            self.status = OK
                            return
                        else:
                            msg = "Package does not exist on the server"
                            raise BadPackage, (self.name, msg)
                    else:
                        msg = "The server's record is completely corrupt"
                        raise BadPackage, (self.name, msg)
                else:
                    msg = "server record is corrupt: no 'install' section"
                    raise BadPackage, (self.name, msg)
            else:
                msg = "server record is corrupt: not a dictionary"
                raise BadPackage, (self.name, msg)
        else:
            msg = "No metadata found for this package"
            raise BadPackage, (self.name, msg)


    def _initialize_from_filesystem(self):
        """ Expects a standard package to be extracted in the packages
        directory """
        package_path = getPackagePath(self.instance_name)
        if not self.full_name:
            raise BadPackage(self.name, "Could not find full name.")
        new_dir = os.path.join(package_path, self.full_name)
        self.scripts_dir = os.path.join(new_dir, "scripts")
        if self.package_version > 3:
            self.maint_dir = os.path.join(new_dir, "maint")
        else:
            self.maint_dir = os.path.join(self.scripts_dir, "maint")
        if not self.filesystem.isdir(self.scripts_dir):
            errmsg = "Scripts directory does not exist"
            raise BadPackage(self.name, errmsg)
        injector_dir = os.path.join(new_dir, "injector")
        if self.filesystem.isdir(injector_dir):
            self.working_dir = injector_dir
        else:
            errmsg = "The injector directory does not exist for [%s]" % self.full_name
            raise BadPackage(self.name, errmsg)

    def _find_cmd(self, action, package_list=[], dry_run=False):
        if self.package_version == 4:
            return self._find_cmd_type_4(action, package_list, dry_run=dry_run)

    def _dump_error(self, e, file_name):
        Logger.error("Error detected in %s (%s)." % (file_name, e))
        e = StringIO.StringIO()
        traceback.print_exc(file=e)
        e.seek(0)
        data = e.read()
        ermsg = ''
        for line in data.split('\n'):
            ermsg += "\n||>>>%s" % line
        Logger.error(ermsg)
        Logger.error("Error ocurred in %s" % file_name)

    def _find_cmd_type_4(self, action, package_list=[], dry_run=False):
        cwd = self.filesystem.getcwd()
        sys.path.insert(0, self.scripts_dir)
        self.filesystem.chdir(self.scripts_dir)
        files = glob.glob("*.py")
        files = [x.split('.py')[0] for x in files]
        if self.name in files:
            files = [self.name]
        else:
            vpkg_name = self.meta_data.data.get('virtualpackage')
            if vpkg_name in files:
                files = [vpkg_name]
        status = FAIL
        file_found = False
        for file_name in files:  # FIXME this is stupid
            if file_name.split('.')[0] in ["installer", "verify", "uninstaller", "configure"]:
                continue
            try:
                obj = Spkg.SpkgV4(self.config, Logger)
                self.filesystem.chdir(self.working_dir)
                letters = [ chr( x ) for x in range(65, 91) ]
                random.shuffle(letters)
                rand_string = ''.join(letters)
                exec("import %s as %s" % (file_name, rand_string)) # FIXME
                Logger.debug("This is package version %s" % self.package_version)
                if self.package_version == 2:
                    exec("obj = %s.%s(self.config)" % (rand_string, file_name))
                elif self.package_version == 3:
                    self.config["__INSTANCE__"] = self.instance_name
                    cmdString = "obj = %s.%s(self.config, package_list, Logger)"
                    exec(cmdString % (rand_string, file_name))
                elif self.package_version == 4:
                    self.config["__FUTURE_PACKAGES__"] = package_list
                    self.config["__INSTANCE__"] = self.instance_name
                    cmdString = "obj = %s.%s(self.config, Logger)"
                    exec(cmdString % (rand_string, file_name))
                else:
                    raise BadPackage( self.name, "Unknown package version %s" % self.package_version )
                file_found = True
                if not dry_run:
                    if action == INSTALL:
                        status = obj.installer()
                    elif action == VERIFY:
                        status = obj.verify()
                    elif action == UNINSTALL:
                        status = obj.uninstaller()
                    elif action == CONFIGURE:
                        status = obj.configure()
                    else:
                        raise FeatureRemovedException(action)
                else:
                    status = OK
                del rand_string
                if file_name in sys.modules:
                    sys.modules.pop(file_name)
                while self.scripts_dir in sys.path:
                    sys.path.remove(self.scripts_dir)
                break
            except ImportError:
                Logger.debug("File %s is not runnable. Looking for others" % file_name)
                continue
            except SystemExit, e:
                if e.code:
                    status = e.code
                else:
                    status = 0
                file_found = True
                del rand_string
                break
            except KeyboardInterrupt:
                Logger.error("Keyboard interrupt detected. Exiting...")
                sys.exit(10)
            except SyntaxError, e:
                self._dump_error(e, file_name)
                file_found = False
                del rand_string
                break
            except StandardError, e:
                self._dump_error(e, file_name)
                file_found = True
                status = FAIL
                del rand_string
                break
        self.filesystem.chdir(cwd)
        if not file_found:
            raise BadPackage(self.name, "Unable to find a suitable script to install.")
        if status == None:
            status = OK
        if status == REBOOT:
            raise RebootRequiredException(self.name)
        if status != OK:
            erstr = "%s: failed with status %s" % (self.full_name, status)
            Logger.error(erstr)
            return FAIL
        return OK

    def _download(self):
        if not self.downloaded:
            try:
                self.repository.get_type4_package(self.name,
                                                  checksum=self.checksum)
                self._initialize_from_filesystem()
                self.downloaded = True
            except BadPackage, bp:
                self._invalidate(bp)
                return FAIL
        return OK

    def _install(self, package_list, dry_run=False):
        dry_run_string = ""
        if dry_run:
            dry_run_string = " --DRY_RUN-- "
        if self._download() != OK:
            return FAIL
        message = "Beginning installation of (%s)%s"
        Logger.info(message % (self.full_name, dry_run_string))
        self.status = self._find_cmd(INSTALL, package_list, dry_run=dry_run)
        msg = "Install result for %s%s : %s"
        Logger.info(msg % (self.full_name, dry_run_string, self.status))
        return self.status

    def _write_progress(self):
        time_string = time.ctime()
        progress_data = self.filesystem.getProgressData(self.instance_name)
        if not progress_data.has_key(self.full_name):
            progress_data[self.full_name] = {"INSTALLED": "NA",
                                             "UNINSTALLED": "NA",
                                             "VERIFIED": "NA",
                                             "DEPENDENCY_ERRORS": []}
        if self.action == INSTALL:
            if progress_data.get(self.full_name) == None:
                progress_data[self.full_name] = {}
            if self.full_name:
                if self.status == OK:
                    progress_data[self.full_name]['INSTALLED']   = time_string
                    # FIXME: shouldn't this next line be 'NA'? -- pbanka
                    progress_data[self.full_name]['VERIFIED']    = time_string 
                    progress_data[self.full_name]['UNINSTALLED'] = 'NA'

                else:
                    progress_data[self.full_name]['INSTALLED']   = "BROKEN"
                    progress_data[self.full_name]['VERIFIED']    = 'NA'
                    progress_data[self.full_name]['UNINSTALLED'] = 'NA'
            else:
                Logger.error("Unnamed package installed: (%s)" % (self.name))
                return FAIL
    
        elif self.action == UNINSTALL:
            like_packages = [ p for p in progress_data if p.startswith(self.name) and p != self.full_name ]
            for p in like_packages:
                del progress_data[p]
            if self.status == OK:
                progress_data[self.full_name]['UNINSTALLED'] = time_string
                progress_data[self.full_name]['INSTALLED']   = 'NA'
            else:
                progress_data[self.full_name]['UNINSTALLED'] = "BROKEN"

        elif self.action == VERIFY:
            progress_data[self.full_name]['VERIFIED'] = time_string
        progress_data[self.full_name]['DEPENDENCY_ERRORS'] = self.dependency_errors
        self.filesystem.updateProgress({"install-progress":progress_data},
                                       self.instance_name, overwrite=True)
        return OK
