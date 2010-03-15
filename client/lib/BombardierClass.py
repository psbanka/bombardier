#!/usr/bin/python
# BSD License
# Copyright (c) 2009, Peter Banka et al
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the name of the GE Security nor the names of its contributors may
#   be used to endorse or promote products derived from this software without
#   specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

'''Contains those classes necessary for figuring out package installation
order and offers major actions available on a machine. Main entry
point for BC

NOTE ON NAMING CONVENTIONS: 
 - 'pkn' = a package name
 - 'pkd' = a package dictionary
 - 'pkg' = a package object
 - 'vpn' = "virtual package name"
 - 'pkl' = a package list
 - 'pdat' = a dictionary of package progress data
'''

import sets, os, copy
import PackageV4, PackageV5
import Exceptions
from bombardier_core.Logger import Logger
from bombardier_core.mini_utility import get_installed, get_progress_data
from bombardier_core.mini_utility import diff_dicts
from bombardier_core.static_data import OK, FAIL, HASH_FILE
from bombardier_core.static_data import UNINSTALL, DRY_RUN, VERIFY
from bombardier_core.static_data import INSTALL, CONFIGURE
from bombardier_core.static_data import EXECUTE, ACTION_REVERSE_LOOKUP

MAX_CHAIN_DEPTH = 50

def swap(list_obj, index1, index2):
    'swap two indexes in a list'
    new_list = copy.deepcopy(list_obj)
    new_list[index1] = list_obj[index2]
    new_list[index2] = list_obj[index1]
    return new_list

class PackageChain:
    ''' 
    A list of package names which should be installed in order. Contains
    the priority of the most important package in the chain
    '''
    def __init__(self, priority, start_pkn, pkd,
                 installed_pkns, broken_pkns, repository,
                 config, instance_name, record_errors):
        """
        priority -- How important this package chain is (higher is better)
        start_pkn -- The name of the first package in this 
                     dependency chain
        pkd -- a dictionary of package objects
        installed_pkns -- a list of packages that have been installed
        repository -- the Repository object which keeps track of packages
        config -- the object that tracks of the configuration fo the system
        instance_name -- the name of this machine (can be more than one)
        record_errors -- flag whether dependency errors should be tracked
        """
        self.depth       = 0
        self.priority    = priority
        self.pkd         = pkd
        self.instance_name = instance_name
        self.chain       = [start_pkn]
        self.repository  = repository
        self.config      = config
        self.record_errors = record_errors
        self.v_pkgs   = VirtualPackages(repository.pkg_data)
        self.installed_pkns = self.v_pkgs.resolve_vpkl( installed_pkns )
        self.broken_pkns = broken_pkns
        self._pkg_chain(start_pkn)
        self._filter_bad_pkgs()

    def _filter_bad_pkgs(self):
        '''
        Go through my chain of packages, and if there are any 'bad' packages
        in the list of dependencies, cut off the package chain at that point
        '''
        index = 0
        for index in range(0, len(self.chain)):
            pkn = self.chain[index]
            if pkn in self.broken_pkns:
                index -= 1
                msg = "Omitting packages %s due to bad package"
                msg = msg % self.chain[index:]
                Logger.warning(msg)
                break
        self.chain = self.chain[:index+1]

    def _pkg_chain(self, pkn):
        '''
        Recursive function to add to self.chain packages in a proper
        dependency order
        pkn -- the name of a package or virtual package
        '''
        self.depth += 1
        if self.depth > MAX_CHAIN_DEPTH:
            raise Exceptions.DependencyLoopException(self.chain)
        for dep_name in self.pkd[ self.get_actual_pkn( pkn ) ].dependencies:
            if dep_name not in self.installed_pkns: 
                self.chain.insert( 0, self.get_actual_pkn( dep_name ) )
            else:
                continue
            self._sync_dependencies( dep_name, pkn )
            dependency = self.pkd.get(self.get_actual_pkn( dep_name ))
            if dep_name in self.broken_pkns:
                self.priority = 0
            else:
                self.priority = max( dependency.priority, self.priority )
            self._pkg_chain( dep_name )
        self.depth -= 1

    def _sync_dependencies( self, dep_name, pkn ):
        '''
        We have discovered that the 'pkn' package is dependent upon
        the 'dep_name' package being installed. Therefore, we can't install
        pkn unless dep_name is installed. We want to make sure that
        the dep_name package is going to be installed.
        dep_name -- the name of a package which is dependent upon one
                    that should be installed
        pkn -- the name of the package that dep_name is
               dependent upon
        '''
        if dep_name not in ( self.v_pkgs.resolve_vpkl( self.installed_pkns ) + \
                            self.v_pkgs.resolve_vpkl( self.pkd.keys() ) ):
            self.pkd[dep_name] = self._get_new_pkg( dep_name )
            if self.record_errors:
                self.pkd[pkn].add_dependency_error( dep_name )

    def get_actual_pkn( self, pkn ):
        '''
        This resolves names of packages into 'actual' package names,
        not virtual names
        pkn -- the name of either a package or virtual package
        '''
        return( self.v_pkgs.get_actual_pkn( pkn, self.pkd.keys() ) )

    def _get_new_pkg( self, pkn ):
        '''
        create a package object based on pkn
        pkn -- the name of the package to create
        '''
        version = self.repository.determine_pkg_version(pkn)
        pkg = None
        if version == 4:
            pkg = PackageV4.PackageV4(pkn, self.repository,
                                          self.config,
                                          self.instance_name)
        else:
            pkg = PackageV5.PackageV5(pkn, self.repository,
                                          self.config,
                                          self.instance_name)
        pkg.initialize()
        return pkg

class VirtualPackages:
    '''A lookup class that can resolve names of virtual package names to
    actual package names'''
    def __init__( self, pkd ):
        '''
        pkd -- a dictionary of package objects
        '''
        self.pkd = pkd
        self.virtual_pkgs_dict = {}
        self.virtual_name_lookup_dict = {}
        self._init_vpkg_dictionaries()

    def _init_vpkg_dictionaries( self ):
        '''
        initialize the dictionaries that this class uses to resolve virtual
        package names into actual package names
        '''
        for pkn in self.pkd:
            pkg_data = self.pkd[pkn]
            if pkg_data.get( 'virtualpackage' ):
                vpn = pkg_data['virtualpackage'] 
                self.virtual_name_lookup_dict.setdefault( pkn, vpn )
                self.virtual_pkgs_dict.setdefault( vpn, [] ).append( pkn )

    def _get_pkn_list_from_vpn( self, vpn ):
        '''
        We are given the name of a virual package. We are to find the names
        of all the actual packages which could be used in place of that
        virtual
        vpn -- the name of a virtual package
        '''
        return( self.virtual_pkgs_dict.get( vpn, [] ) )

    def get_vpn_from_pkn( self, pkn ):
        '''
        Return the name of an actual package. If the pkn submitted
        is an actual package, return it, otherwise look up an actual one and
        substitute it.
        pkn -- the name of either an actual package or virual
               package name
        '''
        return( self.virtual_name_lookup_dict.get( pkn, pkn ) )
    
    def resolve_vpkl( self, pkl ):
        '''
        go through a list of package names, replacing any virtual package names
        with actual package names
        pkl -- a list of package names, some of which could be
               'virtual', meaning that they represent a class of
               possible packages
        '''
        return( [self.get_vpn_from_pkn( pkn ) for pkn in pkl] )

    def get_actual_pkn( self, pkn, pkns ):
        '''
        We are given a pkn, which may be actual or virtual. We are
        going to find all the actual package names which could be used in place
        of that package name. We only want to pick a package which is in our
        repository.
        pkn -- an actual or virtual package name
        pkns -- a list of actual package names
        '''
        resolved_pkns = self._get_pkn_list_from_vpn( pkn )
        installable_pkns = []
        for actual_pkn in resolved_pkns:
            if actual_pkn in pkns:
                installable_pkns.append(actual_pkn)
        installable_pkns.append(pkn)
        return( installable_pkns[0] ) 

def find_differences(pkg_config, config_diff, differences=[], chain=[]):
    '''
    Find the differences in the configuration fo the system since the last
    time the package was installed or configured
    package_config -- the configuration values required by a package
    config_diff -- the differences that were made to this system since
                   the last time this package was installed.
    differences -- differences in the configuration that are relevant
                   to this package (or any other installed package on the
                   machine, as it recurses)
    chain -- a package chain
    '''
    output = differences
    for key in pkg_config.keys():
        if type(pkg_config[key]) == type({}):
            if key not in config_diff:
                continue
            _newdif = find_differences(pkg_config[key], config_diff[key],
                                     output, chain + [key])
            continue
        if key in config_diff.keys():
            output.append("%s/%s" % ('/'.join(chain), key))
            continue
    return output

class Bombardier:
    '''Master class for managing system actions'''

    def __init__(self, repository, config, instance_name):
        '''
        repository -- object that keeps track of package data
        config -- object that keeps track of this machine's configuration
        instance_name -- the name of this machine
        '''
        self.repository = repository
        self.config     = config
        self.instance_name    = instance_name
        self.record_errors = True
        self.operation_status = OK
        self.operation_output = []

    def _get_dependency_errors(self, bom_pkns, pdat):
        '''
        Pulls dependency error data out of status.yml
        bom_pkns -- the names of packages which are on the bill of
                    materials for this machine
        pdat -- a dictionary which maintains information about what
                packages are installed, uninstalled, verified, etc.
        '''
        dependency_names = set([])
        install_progress = pdat.get('install-progress', {})
        for pkn in install_progress:
            pkd = install_progress[ pkn ]
            dep_errors = set(pkd.get('DEPENDENCY_ERRORS', []))
            dependency_names = dependency_names.union(dep_errors)
        dependency_names = list(dependency_names - set(bom_pkns))
        return dependency_names

    def _create_pkg_chains(self, pkd):
        '''
        Create package chains based on current set of packages
        pkd -- dictionary of package objects
        '''
        chains = []
        pkg_data = get_progress_data(self.instance_name, False)
        installed_pkns, broken_pkns = get_installed(pkg_data)
        for pkn in pkd.keys():
            if pkn in broken_pkns:
                Logger.warning("Skipping broken package %s" % pkn)
                continue
            if pkn not in installed_pkns:
                chain_priority = pkd[pkn].priority
            else:
                chain_priority = 0
            try:
                new_chain = PackageChain(chain_priority, pkn,
                                         pkd, installed_pkns,
                                         broken_pkns, self.repository,
                                         self.config,
                                         self.instance_name, self.record_errors)
            except Exceptions.BadPackage, err:
                errmsg = "Package %s will not be installed because it is "\
                         "dependent upon one or more broken packages"
                Logger.warning(errmsg % pkn)
                Logger.warning(str(err))
                continue
            chains.append([new_chain.priority, new_chain.chain])
        return chains

    def _get_top_priority(self, chains):
        '''
        Go through a bunch of chains and determine which is the most important
        chains -- tuples of priority / chain
        '''
        top_priority = 0
        for priority, _chain in chains:
            if priority > top_priority:
                top_priority = priority
        return top_priority

    def _find_install_order(self, pkd):
        """Returns a list of pkns in the correct installation order
        - Create chains of package dependencies, the priority of each
        chain is the package with the highest priority in it, unless
        that package is already installed.
        - A package can appear in more than one chain
        - A package chain can have a single package in it if it does not
        have any dependencies and is not dependent on others.
        pkd -- dictionary of package objects
        """

        chains = self._create_pkg_chains(pkd)
        pdat = get_progress_data(self.instance_name, False)
        installed_pkns, _broken_pkns = get_installed(pdat)
        # - Put all the packages of each chain into the installation
        # order, excluding those that have already been installed in order
        # of chain priority. If a package is already in the installation
        # List, do not put it in twice.
        install_order = []
        while chains:
            top_priority = self._get_top_priority(chains)
            for priority, chain in chains:
                if priority == top_priority:
                    for pkn in chain:
                        if pkn not in installed_pkns:
                            if pkn not in install_order:
                                install_order.append(pkn)
                    chains.remove([priority, chain])
        return install_order

    def _install_pkgs(self, add_pkd, dry_run=False):
        '''
        Provided a dictionary of packages, we will install those that need to
        be installed.
        add_pkd -- a dictionary of packages that need to be added
        dry_run -- whether or not to just 'fake it'
        '''
        making_progress = True
        pkns_left = ['initialize']
        while making_progress and pkns_left:
            making_progress = False
            install_order = self._find_install_order(add_pkd)
            pkns_left = list(install_order)
            for pkn in install_order:
                msg = "Packages remaining to install (in order):"
                Logger.info(msg)
                for tpkn in pkns_left:
                    Logger.info("  +   %s" % tpkn)
                pkns_left.remove(pkn)
                pkg = add_pkd[pkn]
                erstr = "Currently installing package priority %s [%s]"
                Logger.info(erstr % (pkg.get_priority(), pkn))
                status = pkg.install_and_verify(pkns_left)
                if not dry_run:
                    hash_path = os.path.join(pkg.get_path(), HASH_FILE)
                    self.config.save_hash(hash_path)
                if status == FAIL:
                    erstr = "Package installation failure -- re-calculating"\
                            " package installation order"
                    Logger.error(erstr)
                    break
                else:
                    making_progress = True
        if pkns_left:
            msg = "There are packages that are broken, and we have done all"\
                  " we can do. ; ;"
            Logger.error(msg)
            return FAIL
        return OK

    def _cleanup(self):
        'Some housekeeping before exiting'
        if self.operation_status == FAIL:
            log_function = Logger.error
        else:
            log_function = Logger.info
        for message in self.operation_output:
            log_function("==OUTPUT==:%s" % message)
        return self.operation_status

    def _check_configuration(self, pkg):
        '''
        Check to see if we have sufficient configuration data to mess around
        with a given package
        pkg -- a package object that we need to do something with
        '''
        if pkg.get_configuration():
            required_configs = pkg.get_configuration()
            diff = diff_dicts(required_configs, self.config.data)
            if diff != {}:
                self.operation_status = FAIL
                errmsg = "This machine does not have sufficient "\
                         "configuration data to install %s " % pkg.name
                diff_txt = ''
                for key in diff:
                    diff_txt += "key: %s; value: %s" % (key, diff[key])
                Logger.warning(errmsg)
                Logger.warning(diff_txt)
                raise Exceptions.BadPackage(pkg.name, "Bad Config")

    def _get_new_pkg(self, pkn):
        '''
        Instantiate a package object based on name
        pkn -- The name of a package to instantiate
        '''
        version = self.repository.determine_pkg_version(pkn)
        pkg = None
        if version == 4:
            pkg = PackageV4.PackageV4(pkn, self.repository,
                                          self.config,
                                          self.instance_name)
        else:
            pkg = PackageV5.PackageV5(pkn, self.repository,
                                          self.config,
                                          self.instance_name)
        pkg.initialize()
        return pkg

    def _get_add_pkd(self, add_pkns):
        '''
        create a pkd for package names that should be installed
        add_pkns -- the names of packages that we should be installing
        '''
        pkd = {}
        for pkn in add_pkns:
            try:
                pkg = self._get_new_pkg(pkn)
                self._check_configuration(pkg)
                pkd[pkn] = pkg
            except Exceptions.BadPackage, err:
                errmsg = "Skipping bad package %s: %s" % (err.pkn, err.errmsg)
                self.operation_output.append(errmsg)
                Logger.warning(errmsg)
        return pkd

    def _get_del_pkd(self, del_pkns):
        '''
        create a dictionary of package objects based on a list of names
        that we know we're supposed to remove
        del_pkns -- the names of packages that we should be removing
        '''
        pkd = {}
        for pkn in del_pkns:
            try:
                pkg = self._get_new_pkg(pkn)
                pkg.action = UNINSTALL
                pkd[pkn] = pkg
            except Exceptions.BadPackage, err:
                errmsg = "Skipping Bad package: %s" % err
                Logger.warning(errmsg)
        return pkd

    def _sort_uninstalled_pkgs(self, uninstall_order):
        '''Make sure packages get uninstalled in the right order, paying
        attention to dependencies.
        uninstall_order -- a list of package names the order of which indicates
                           which should be uninstalled first, second...
        '''
        dependency_dict = {}
        if len(uninstall_order) > 1:
            Logger.info("Determining uninstallation order...")
            for pkn in uninstall_order:
                dependency_dict[pkn] = []

            for pkn in uninstall_order:
                pkg = self._get_new_pkg(pkn)
                for other_pkn in uninstall_order:
                    if other_pkn in pkg.dependencies:
                        dependency_dict[other_pkn].append(pkn)
        else:
            return uninstall_order

        new_proper_order = copy.deepcopy(uninstall_order)
        swapped = True

        while swapped:
            proper_order = copy.deepcopy(new_proper_order)
            swapped = False
            for pkn in proper_order:
                dependency_list = dependency_dict[pkn]
                for dependency in dependency_list:
                    index1 = proper_order.index(dependency)
                    index2 = proper_order.index(pkn)
                    if index1 > index2:
                        new_proper_order = swap(proper_order, index1, index2)
                        swapped = True
                        break
                if swapped:
                    break

        Logger.info("Uninstallation Order: %s" % new_proper_order)
        return new_proper_order

    def _get_uninst_pkg_dep(self, pkd,
                            del_pkns,
                            installed_pkns):
        '''Add any packages that are installed already which are dependent
        upon those to the list as well
        pkd -- The dictionary of package objects
        del_pkns -- names of packages we want to remove
        installed_pkns -- names of packages that are installed already
        '''
        msg = "Checking dependencies of packages to be uninstalled %s..."
        msg = msg % del_pkns
        Logger.info(msg)
        v_pkgs = VirtualPackages(self.repository.pkg_data)
        uninstall_order = copy.deepcopy(del_pkns)
        while del_pkns:
            new_dependency_names = []
            del_pkns = []
            for pkn in installed_pkns:
                if pkn in pkd.keys():
                    Logger.debug("Package %s will already be deleted -- "\
                                 "ignoring" % pkn)
                    continue
                pkg = self._get_new_pkg(pkn)
                pkg.action = UNINSTALL
                for tombstoned_pkns in pkd.keys():
                    vpn = v_pkgs.get_vpn_from_pkn(tombstoned_pkns)
                    if vpn in pkg.dependencies:
                        erstr = "Adding to package removal list: %s"\
                                " (depends on %s)"
                        uninstall_order.insert(0, pkn)
                        Logger.info(erstr % (pkn, tombstoned_pkns))
                        pkd[tombstoned_pkns].depends_on_me.append(pkn)
                        if pkn not in pkd.keys():
                            if pkn not in new_dependency_names:
                                pkd[pkn] = pkg
                                new_dependency_names.append(pkn)
                del_pkns = new_dependency_names

        proper_order = self._sort_uninstalled_pkgs(uninstall_order)
        return pkd, proper_order

    def _get_pkd_to_remove(self, del_pkns):
        '''
        Given a list of package names that we want to delete, let's create a
        proper dictionary of package objects which provide complete dependency
        information and specify the proper uninstallation order
        del_pkns -- a list of package names that need to be removed
        '''
        uninstall_order = []
        pdat = get_progress_data(self.instance_name, False)
        installed_pkns, _broken_pkns = get_installed(pdat)
        del_pkd = self._get_del_pkd(del_pkns)
        if sets.Set(installed_pkns) == sets.Set(del_pkd.keys()):
            return del_pkd, del_pkd.keys()
        if del_pkns:
            del_pkd, uninstall_order = self._get_uninst_pkg_dep(del_pkd,
                                                                del_pkns,
                                                                installed_pkns)
        return del_pkd, uninstall_order

    def _examine_dependencies(self, pkns):
        """ We need to determine if there are packages installed which
        have missing dependencies. If so, they should be added to the BOM.
        pkns -- a list of package names to check
        """
        updated_pkns = pkns.copy()
        vpkgs = VirtualPackages(self.repository.pkg_data)

        for pkn in pkns:
            apkn = vpkgs.get_actual_pkn(pkn, pkns)
            pkg = self._get_new_pkg(apkn)
            pkg.initialize()
            for dep in pkg.dependencies:
                updated_pkns.update([vpkgs.get_actual_pkn(dep, pkns)])
        if updated_pkns.difference(pkns):
            updated_pkns = self._examine_dependencies(updated_pkns)
        return updated_pkns
    
    def _check_bom(self, bom_pkns):
        """ Check through what should be installed on the system and what
        is installed on the system and determine what packages aren't
        installed that should be and what packages are installed that
        shouldn't be.
        bom_pkns -- A list of package names that are intended to be
                    installed on the machine (i.e. the 'bill of
                    materials'
        """
        should_be_installed = []
        shouldnt_be_installed = []
        pdat = get_progress_data(self.instance_name, False)
        installed_pkns, broken_pkns = get_installed(pdat)
        
        all_pkns = set(installed_pkns).union(broken_pkns)
        all_plus_missing_pkns = self._examine_dependencies(all_pkns)
        missing_pkns = list(all_plus_missing_pkns - all_pkns)
        bom_pkns = bom_pkns + missing_pkns

        dependency_errors = self._get_dependency_errors(bom_pkns, pdat)
        if dependency_errors:
            errmsg = "The following packages are installed as "\
                     "dependencies %s" % dependency_errors
            Logger.debug(errmsg)
        bom_pkns += dependency_errors
        for pkn in installed_pkns:
            if pkn not in bom_pkns:
                shouldnt_be_installed.append(pkn)
        for pkn in bom_pkns:
            if pkn not in installed_pkns:
                should_be_installed.append(pkn)
        return should_be_installed, shouldnt_be_installed

    def _ck_inst_stat(self, bom_pkns):
        '''
        Given a list of packages that are supposed to be on this machine, we
        will look at the installation status and (1) make a decision on what
        packages should be removed (and in what order) and (2) decide which
        packages should be added.
        bom_pkns -- A list of package names that are intended to be
                             installed on the machine (i.e. the 'bill of
                             materials'
        '''
        if bom_pkns == []:
            bom_pkns = self.config.get_bom_pkns()
            if bom_pkns == []:
                Logger.warning("Empty Bill of Materials")
        add_pkns, del_pkns = self._check_bom(bom_pkns)
        add_pkd = self._get_add_pkd(add_pkns)
        del_pkd, uninstall_order = self._get_pkd_to_remove(del_pkns)
        return add_pkd, del_pkd, uninstall_order

    def _check_configuration_hash(self, pkn):
        '''
        We want to see if the configuration that a given package has used
        has changed since it was installed.
        pkn -- the name of the package to check
        '''
        pkg = self._get_new_pkg(pkn)
        pkg_config = pkg.get_configuration()
        config_hash_path = os.path.join(pkg.get_path(), HASH_FILE)
        config_diff = self.config.check_hash(config_hash_path)
        differences = find_differences(pkg_config, config_diff, [])
        return differences

    def _uninstall_pkgs(self, del_pkd, uninstall_order,
                            dry_run=False):
        '''
        Given a dictionary of packages and an uninstall order, make some
        changes on this machine
        del_pkd -- A dictionary of package objects that are to
                   be uninstalled
        uninstall_order -- the order with which to uninstall packages
        dry_run -- boolean flag indicating if we're serious about this or not
        '''
        status = OK
        remove_full_pkns = []
        #Logger.info("UninstallOrder: %s" % uninstall_order)
        for name in uninstall_order:
            if del_pkd[name].full_name:
                name_str = del_pkd[name].full_name
            else:
                name_str = name
            remove_full_pkns.append(name_str)
        Logger.info("Packages to remove: %s" % remove_full_pkns)
        for pkn in uninstall_order:
            uninstall_status = del_pkd[pkn].uninstall(dry_run)
            if uninstall_status == FAIL:
                return FAIL
        return status

    def check_system(self):
        '''
        After a system has had all of its packages installed and it's in
        stable-state, this is a method we can use to verify that everything
        is still kosher
        '''
        Logger.info("System-check starting...")
        bom_pkns = self.config.get_bom_pkns()
        pdat = get_progress_data(self.instance_name)
        full_pdat = get_progress_data(self.instance_name, False)
        full_installed_pkns, _full_bpkns = get_installed(full_pdat)
        msg = "Packages that are installed: %s"
        Logger.info(msg % ' '.join(full_installed_pkns))
        installed_pkns, broken_pkns = get_installed(pdat)
        should_be_installed, shouldnt_be_installed = self._check_bom(bom_pkns)
        # check the configuration for each installed package
        pkg_info = {"Packages installed properly": installed_pkns,
                    "Packages to be RECONFIGURED": [],
                    "Packages to be INSTALLED": should_be_installed,
                    "Packages to be REMOVED": shouldnt_be_installed,
                    "Packages that are BROKEN": broken_pkns,
                   }
        for pkn in shouldnt_be_installed:
            pkg_info["Packages installed properly"].remove(pkn)
        for pkn in installed_pkns:
            differences = self._check_configuration_hash(pkn)
            if differences:
                if pkn in pkg_info["Packages installed properly"]:
                    pkg_info["Packages to be RECONFIGURED"][pkn] = differences
                    pkg_info["Packages installed properly"].remove(pkn)
        for key in pkg_info:
            Logger.info("==OUTPUT==:%s: %s" % (key, pkg_info[key]))
        return pkg_info

    def use_pkg(self, pkn, action, script_name=''):
        '''
        Main entry point to the class. Performs an action using a package
        pkn -- name of the package to use
        action -- STATUS, INSTALL, UNINSTALL, CONFIGURE, VERIFY, or EXEC
        script_name -- the name of a method to run within the package
        '''
        try:
            pkg = self._get_new_pkg(pkn)
            if pkg.status == FAIL:
                self.operation_status = FAIL
                return self._cleanup()
            if action == INSTALL:
                pdat = get_progress_data(self.instance_name, False)
                installed_pkns, broken_pkns = get_installed(pdat)
                if pkn in [installed_pkns + broken_pkns]:
                    Logger.error("Package %s cannot be installed." % pkn)
                    self.operation_status = FAIL
                    return FAIL
                add_pkd = {pkn:pkg}
                status = self._install_pkgs(add_pkd)
            if action == UNINSTALL:
                pdat = get_progress_data(self.instance_name, False)
                installed_pkns, broken_pkns = get_installed(pdat)
                bom_pkns = installed_pkns
                if pkn in bom_pkns:
                    bom_pkns.remove(pkn)
                self.config.set_bom_pkgs(bom_pkns)
                add_pkd, del_pkd, uninstall_order = self._ck_inst_stat([])
                status = self._uninstall_pkgs(del_pkd, uninstall_order)
            if action == VERIFY:
                status = pkg.verify()
            if action == CONFIGURE:
                self._check_configuration(pkg)
                status = pkg.configure()
                hash_path = os.path.join(pkg.get_path(), HASH_FILE)
                msg = "Writing configuration fingerprint to %s" % hash_path
                Logger.info(msg)
                self.config.save_hash(hash_path)
            if action == EXECUTE:
                status = pkg.execute_maint_script(script_name)
                if status == FAIL:
                    self.operation_status = FAIL
            msg = "Finished %s for %s."
            msg = msg % (ACTION_REVERSE_LOOKUP[action], pkn)
            self.operation_output.append(msg)
            status = self._cleanup()
            return self._cleanup()
        except Exceptions.BadPackage, err:
            errmsg = "Cannot perform action %s on package %s: %s"
            errmsg = errmsg % (ACTION_REVERSE_LOOKUP[action], err.pkn,
                               err.errmsg)
            Logger.warning(errmsg)
            Logger.info("RETURN FAIL 1")
            return FAIL

    def reconcile_system(self, action):
        '''
        Figure out everything that needs to happen to get this machine into
        compliance with its stated configuration and do it
        action -- DRY_RUN or RECONCILE
        '''

        self.operation_status = OK
        self.operation_output = []
        pkns = []
        dry_run = False

        if action == DRY_RUN:
            Logger.info("Reconcile dry-run starting...")
            dry_run = True
        else:
            Logger.info("Reconcile starting...")

        add_pkd, del_pkd, uninstall_order = self._ck_inst_stat(pkns)
        #Logger.info("uninstall_order: %s" % uninstall_order)
        status = self._uninstall_pkgs(del_pkd, uninstall_order,
                                          dry_run)
        if status == FAIL:
            msg = "Uninstallation failed. Aborting reconcile."
            self.operation_output.append(msg)
            self.operation_status = FAIL
            return self._cleanup()

        add_pkd, del_pkd, uninstall_order = self._ck_inst_stat(pkns)
        status = self._install_pkgs(add_pkd, dry_run)
        if status != OK:
            self.operation_status = FAIL
        self.operation_output.append("Finished installing")
        return self._cleanup()

