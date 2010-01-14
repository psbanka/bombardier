#!/usr/bin/env python

import sys

import setup_tests
setup_tests.cleanup()
setup_tests.start()
sys.path.insert(0, "../lib")

import unittest, StringIO, time, os
import yaml
import BombardierClass
import Repository
from bombardier_core.Config import Config
from bombardier_core.static_data import OK, FAIL, RECONCILE, DRY_RUN
import Exceptions
import MockObjects

INSTANCE = "TEST_INSTANCE"

def _write_progress(progress_data):
    open("spkg/%s/status.yml" % INSTANCE, 'w').write(yaml.dump(progress_data))

class BombardierTest(unittest.TestCase):

    def setUp(self):
        data = {"testokpackage1":{"install": {"fullName": None}, "package-version": 4}}
        self.repository = Repository.Repository(INSTANCE, data)
        #self.repository = MockObjects.MockRepository(data)
        #self.config = MockObjects.MockConfig()
        self.config = Config(INSTANCE, {})
        self.bombardier = BombardierClass.Bombardier(self.repository, self.config, INSTANCE)

    def tearDown(self):
        pass

    def test_check_configuration(self):
        pkg1 = MockObjects.MockPackage()
        pkg1.dependencies = ["pkg2"]
        pkg1.priority = 200
        meta_data = MockObjects.MockMetaData({})
        pkg1.meta_data = meta_data
        status = self.bombardier._check_configuration(pkg1)
        assert status == OK, "System does not have proper configuration"

        data = {"configuration":
                {"section1": {"item1":"spam","item2":"eggs"},
                 "section2": {"item1":"foo"}}
                }
        meta_data = MockObjects.MockMetaData(data)
        pkg1.meta_data = meta_data
        status = self.bombardier._check_configuration(pkg1)
        assert status == FAIL, "System does not know its configuration is wrong"

        self.config.data = {"section1": {"item1": "foo", "item2": "bar", "item3": "baz"},
                            "section2": {"item1": 3},
                            "section3": {"item1": "cheeze"}}
        status = self.bombardier._check_configuration(pkg1)
        assert status == OK, "System does not know its configuration is correct"

    def test_get_packages_to_remove_1(self):
        pkg_data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "dependencies": ["pkg2"], "package-version": 4},
                    "pkg2": {"install": {"fullName":"pkg2-1"}, "package-version": 4}}
        install_progress1 = {"install-progress":
                            {"pkg1-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'},
                             "pkg2-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'}}}
        _write_progress(install_progress1)
        repository = Repository.Repository(INSTANCE, pkg_data)
        self.config.repository = repository
        self.bombardier.repository = repository
        pkd, packages = self.bombardier._get_pkd_to_remove(["pkg2"])
        test = ["pkg1", "pkg2"]
        assert set(test) == set(packages)

    def test_get_packages_to_remove(self):
        pkg_data = {"pkg1": {"install": {"fullName":"pkg1-1"}, "package-version": 4},
                    "pkg2": {"install": {"fullName":"pkg2-1"}, "package-version": 4},
                   }
        install_progress1 = {"install-progress":
                            {"pkg1-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'},
                             "pkg2-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'}}}
        _write_progress(install_progress1)
        repository = Repository.Repository(INSTANCE, pkg_data)
        self.config.repository = repository
        self.bombardier.repository = repository
        status = FAIL
        pkd, packages = self.bombardier._get_pkd_to_remove(["pkg1"])
        assert packages == ["pkg1"], packages

    def test_get_vpn_from_pkn(self):
        pkg_data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "virtualpackage":"anypackage"}, "package-version": 4,
                    "pkg2": {"install": {"fullName":"pkg2-1"}, "package-version": 4}}
        repository = Repository.Repository(INSTANCE, pkg_data)
        vp = BombardierClass.VirtualPackages(repository.pkg_data)
        vpn = vp.get_vpn_from_pkn("pkg1")
        assert vpn == "anypackage", "Got back a weird virtual name "\
               "for pkg1-1: %s" % vpn
        vpn2 = vp.get_vpn_from_pkn("pkg2")
        assert vpn2 == "pkg2", "Got back a weird virtual name for pkg2: %s" % vpn2
        vpn3 = vp.get_vpn_from_pkn("shrubbery")
        assert vpn3 == "shrubbery", "Got back a weird virtual name "\
               "for nonsense package: %s" % vpn3

    def test_get_pkn_list_from_vpn(self):
        pkg_data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "virtualpackage":"anypackage", "package-version": 4},
                    "pkg2": {"install": {"fullName":"pkg2-1"},
                         "virtualpackage":"anypackage", "package-version": 4}}
        repository = Repository.Repository(INSTANCE, pkg_data)
        vp = BombardierClass.VirtualPackages(repository.pkg_data)
        pkn_set = set(vp._get_pkn_list_from_vpn("anypackage"))
        assert pkn_set == set(["pkg1", "pkg2"]), "Got back some weird set"\
             "of real package names: %s" % pkn_set

    def test_resolve_vpkl(self):
        pkg_data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "virtualpackage":"anypackage", "package-version": 4},
                    "pkg2": {"install": {"fullName":"pkg2-1"}, "package-version": 4}}
        repository = Repository.Repository(INSTANCE, pkg_data)
        vp = BombardierClass.VirtualPackages(repository.pkg_data)
        pkn_set = set(vp.resolve_vpkl(["anypackage", "pkg2"]))
        assert pkn_set == set(["anypackage", "pkg2"]),"Error in resolveVPkgList:"\
               "%s" % pkn_set

    def test_get_actual_pkn(self):
        pkg_data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "virtualpackage":"anypackage", "package-version": 4},
                    "pkg2": {"install": {"fullName":"pkg2-1"}, "package-version": 4}}
        repository = Repository.Repository(INSTANCE, pkg_data)
        vp = BombardierClass.VirtualPackages(repository.pkg_data)
        packageName = vp.get_actual_pkn( "anypackage", ["pkg1", "pkg2"] )
        assert packageName == "pkg1", "Bad actual package name from "\
               "getActualPackageName: %s" % packageName
        
    def test_remove_virtual_package(self):
        pkg_data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "dependencies": ["anypackage"], "package-version": 4},
                    "pkg2": {"install": {"fullName":"pkg2-1"},
                             "virtualpackage":"anypackage",  "package-version": 4},
                   }
        install_progress1 = {"install-progress":
                            {"pkg1-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'},
                             "pkg2-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'}}}
        _write_progress(install_progress1)
        repository = Repository.Repository(INSTANCE, pkg_data)
        self.config.repository = repository
        self.bombardier.repository = repository
        should_be_removed = set(["pkg1", "pkg2"])

        pkd, packages = self.bombardier._get_pkd_to_remove(["pkg2"])
        assert should_be_removed == set(packages), "Uninstalling a virtual package has "\
               "failed. %s != %s" % (should_be_removed, set(packages))

    def test_check_bom(self):
        install_progress_2 = {"install-progress":
                                {"pkg1-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                            "UNINSTALLED": 'NA',
                                            "VERIFIED": 'Mon Apr 18 01:01:01 2005'},
                                 "pkg3-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                            "UNINSTALLED": 'NA',
                                            "VERIFIED": 'Mon Apr 18 01:01:01 2005'}}}
        _write_progress(install_progress_2)
        should_be_installed, shouldnt_be_installed = self.bombardier._check_bom(["pkg1", "pkg2"])
        assert ["pkg2"] == should_be_installed, should_be_installed
        assert ["pkg3"] == shouldnt_be_installed, shouldnt_be_installed
        
    def test_add_to_dependency_errors(self):
        pkg1 = MockObjects.MockPackage()

        pkg_chain = BombardierClass.PackageChain(100, "pkg1", {"pkg1": pkg1}, ["pkg3"],[],
                                                 self.repository, self.config, INSTANCE, False)
                                                           
        status = pkg_chain._sync_dependencies("pkg1", "pkg2")

    def test_package_dep(self):
        pkg_data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "dependencies": ["pkg5"], "package-version": 4},
                    "pkg5": {"install": {"fullName":"pkg5-1"}, "package-version": 4}}

        repository = Repository.Repository(INSTANCE, pkg_data)
        pkg1 = MockObjects.MockPackage()
        pkg1.dependencies = ["pkg5"]
        packages = {"pkg1": pkg1}
        broken_pkgs = []
        new_chain = BombardierClass.PackageChain(100, "pkg1", packages, ["pkg5"], broken_pkgs,
                                                 repository, self.config, INSTANCE, False)
        assert new_chain.chain == ["pkg1"], new_chain.chain 

    def test_package_chain(self):
        pkg_data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "dependencies": ["pkg5"], "package-version": 4},
                    "pkg5": {"install": {"fullName":"pkg5-1"},
                         "dependencies": ["pkg6"], "package-version": 4},
                    "pkg6": {"install": {"fullName":"pkg6-1"}, "package-version": 4}}

        repository = Repository.Repository(INSTANCE, pkg_data)
        pkg1 = MockObjects.MockPackage()
        pkg1.dependencies = ["pkg5"]
        packages = {"pkg1": pkg1}
        broken_pkgs = []
        new_chain = BombardierClass.PackageChain(100, "pkg1", packages, ["pkg2", "pkg3"],
                                                 broken_pkgs, repository, self.config, INSTANCE, False)
        assert new_chain.chain == ["pkg6", "pkg5", "pkg1"], new_chain.chain
        pcalls = pkg1.getAllCalls()
        assert len(pcalls) == 0

    def test_package_chain_with_broken(self):
        pkg_data = {"pkg1": {"install": {"fullName":"pkg1-1",
                                     "priority": '20'}, "package-version": 4,
                         "dependencies": ["pkg2"]},
                    "pkg2": {"install": {"fullName":"pkg2-1",
                                     "priority": '70'}, "package-version": 4,
                         "dependencies": ["pkg3"]},
                    "pkg3": {"install": {"fullName":"pkg3-1",
                                     "priority": '20'}, "package-version": 4,
                         "dependencies": ["pkg4"]},
                    "pkg4": {"install": {"fullName":"pkg4-1",
                                     "priority": '20'}, "package-version": 4,
                         "dependencies": ["pkg5"]},
                    "pkg5": {"install": {"fullName":"pkg5-1",
                                     "priority": '30'}, "package-version": 4,
                         "dependencies": ["pkg6"]},
                    "pkg6": {"install": {"fullName":"pkg6-1",
                                     "priority": '20'}, "package-version": 4}}

        repository = Repository.Repository(INSTANCE, pkg_data)
        pkg1 = MockObjects.MockPackage()
        pkg1.dependencies = ["pkg2"]
        pkg1.priority = 20
        pkg2 = MockObjects.MockPackage()
        pkg2.dependencies = ["pkg3"]
        pkg2.priority = 70
        pkg3 = MockObjects.MockPackage()
        pkg3.dependencies = ["pkg4"]
        pkg3.priority = 20
        pkg4 = MockObjects.MockPackage()
        pkg4.dependencies = ["pkg5"]
        pkg4.priority = 20
        pkg5 = MockObjects.MockPackage()
        pkg5.dependencies = ["pkg6"]
        pkg5.priority = 30
        pkg6 = MockObjects.MockPackage()
        pkg6.priority = 20
        packages = {"pkg1": pkg1,"pkg2": pkg2,"pkg3": pkg3,"pkg4": pkg4,"pkg5": pkg5,"pkg6": pkg6}
        broken_pkgs = ["pkg3"]
        new_chain = BombardierClass.PackageChain(0, "pkg1", packages, [],
                                                 broken_pkgs, repository, self.config, INSTANCE, False)
        assert new_chain.chain == ["pkg6", "pkg5", "pkg4"], new_chain.chain
        assert new_chain.priority == 30

    def test_create_package_chains(self):
        pkg_data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "dependencies": ["pkg5"], "package-version": 4},
                    "pkg5": {"install": {"fullName":"pkg5-1"}, "package-version": 4,
                         "dependencies": ["pkg6"]}}
        install_progress_2 = {"install-progress":
                            {"pkg1-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'},
                             "pkg3-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'}}}
        _write_progress(install_progress_2)
        repository = Repository.Repository(INSTANCE, pkg_data)
        pkg1 = MockObjects.MockPackage()
        pkg1.dependencies = ["pkg5"]
        pkg5 = MockObjects.MockPackage()
        packages = {"pkg1": pkg1, "pkg5": pkg5}
        self.bombardier.repository = repository
        chains = self.bombardier._create_pkg_chains(packages)
        assert len(chains) == 2
        pcalls = pkg1.getAllCalls()

    def test_get_top_priority(self):
        chain1 = MockObjects.MockChain()
        chain2 = MockObjects.MockChain()
        chains = [(100, chain1), (300, chain2)]
        top_priority = self.bombardier._get_top_priority(chains)
        assert top_priority == 300

    def test_find_install_order(self):
        pkg_data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "dependencies": ["pkg5"], "package-version": 4},
                    "pkg5": {"install": {"fullName":"pkg5-1"},
                         "dependencies": ["pkg6"], "package-version": 4}}
        install_progress_2 = {"install-progress":
                            {"pkg1-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'},
                             "pkg3-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'}}}
        _write_progress(install_progress_2)
        repository = Repository.Repository(INSTANCE, pkg_data)
        self.config.repository = repository
        pkg1 = MockObjects.MockPackage()
        pkg1.dependencies = ["pkg5"]
        pkg1.priority = 200
        pkg5 = MockObjects.MockPackage()
        pkg6 = MockObjects.MockPackage()
        pkg6.priority = 300
        packages = {"pkg1": pkg1, "pkg5": pkg5, "pkg6": pkg6}
        install_order  = self.bombardier._find_install_order(packages)
        assert install_order == ["pkg6", "pkg5", "pkg1"], "bad install order %s" % install_order

    def test_install_pkgs(self):
        pkg1 = MockObjects.MockPackage()
        add_pkd = {"pkg1": pkg1}
        status = self.bombardier._install_pkgs(add_pkd)
        assert status == OK, "Perfectly good package failed to install"
        assert self.repository.getAllCalls() == []

    def test_install_one_broken(self):
        pkg1 = MockObjects.MockPackage()
        pkg2 = MockObjects.MockPackage()
        pkg2.priority = 200
        pkg2.processResults = FAIL
        add_pkd = {"pkg1": pkg1, "pkg2": pkg2}
        status = self.bombardier._install_pkgs(add_pkd)
        assert status == OK, "Perfectly good package failed to install"

    def test_check_system_1(self):
        pkg_data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                        "dependencies": ["pkg5"], "package-version": 4},
                    "pkg2": {"install": {"fullName":"pkg2-1"},
                        "dependencies": ["pkg6"], "package-version": 4}}
        install_progress1 = {"install-progress":
                            {"pkg1-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'},
                             "pkg2-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'}}}
        repository = Repository.Repository(INSTANCE, pkg_data)
        self.config.repository = repository
        base = os.path.join(os.getcwd(), "packages", "pkg1-1")
        self.bombardier.repository = repository
        test_results = self.bombardier.check_system()
        assert test_results.has_key("pkg1"), test_results
        assert test_results['pkg1'] == OK, test_results
        assert test_results['pkg2'] == FAIL, test_results
        assert len(test_results.keys()) == 2, "Returned more than 2 results."

    def test_check_system_2(self):
        install_progress = {"install-progress":
                            {"pkg1-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'},
                             "pkg2-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'}}}
        pkg_data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                                "dependencies": ["pkg5"], "package-version": 4},
                       "pkg2": {"install": {"fullName":"pkg2-1"},
                                "dependencies": ["pkg6"], "package-version": 4}}
        repository = Repository.Repository(INSTANCE, pkg_data)
        self.config.repository = repository
        base1 = os.path.join(os.getcwd(), "packages", "pkg1-1")
        base2 = os.path.join(os.getcwd(), "packages", "pkg2-1")
        self.bombardier.repository = repository
        test_results = self.bombardier.check_system()
        assert test_results == {"pkg1": OK, "pkg2":OK}, test_results

    def test_verify_system_3(self): # it's not time for this package to be verified
        pkg_data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                                "dependencies": ["pkg5"], "package-version": 4}}
        install_progress_3 = {"install-progress":
                           {"pkg1-1": {"INSTALLED": time.ctime(),
                                       "UNINSTALLED": 'NA',
                                       "VERIFIED": time.ctime()}}}
        repository = Repository.Repository(INSTANCE, pkg_data)
        self.config.repository = repository
        self.bombardier.repository = repository
        test_results = self.bombardier.check_system()
        assert test_results == {}, `test_results`

    def test_verify_system_4(self): # No packages installed, error in verify.
        pkg_data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                                "dependencies": ["pkg5"], "package-version": 4}}
        install_progress_3 = {"install-progress":
                           {"pkg1-1": {"INSTALLED": time.ctime(),
                                       "UNINSTALLED": 'NA',
                                       "VERIFIED": time.ctime()}}}
        repository = Repository.Repository(INSTANCE, pkg_data)
        self.config.repository = repository
        self.bombardier.repository = repository

        test_results = self.bombardier.check_system()
        assert test_results == {}, `test_results`

    def test_reconcile_system1(self):
        self.config.data = {"bom": ["base"], "packages": ["pkg3"]}
        pkg_data = {"pkg1": {"install": {"fullName":"pkg1-1", "priority":"100"}, "package-version": 4},
                       "pkg2": {"install": {"fullName":"pkg2-1", "priority":"50"},
                                "dependencies": [ "pkg1"], "package-version": 4},
                       "pkg3": {"install": {"fullName":"pkg3-1", "priority":"3"}, "package-version": 4}}
        repository = Repository.Repository(INSTANCE, pkg_data)
        self.config.repository = repository
        base1 = os.path.join(os.getcwd(), "packages", "pkg1-1")
        base2 = os.path.join(os.getcwd(), "packages", "pkg2-1")
        base3 = os.path.join(os.getcwd(), "packages", "pkg3-1")

        self.bombardier.repository = repository
        
        try:
            self.bombardier.reconcile_system(DRY_RUN)
        except SystemExit, e:
            assert e.code == OK
        
    def test_reconcile_system_with_dependencies(self):
        self.config.data = {"bom": ["base"]}
        pkg_data = {"pkg1": {"install": {"fullName":"pkg1-1"}, "package-version": 4},
                       "pkg2": {"install": {"fullName":"pkg2-1"}, "package-version": 4}}
        repository = Repository.Repository(INSTANCE, pkg_data)
        self.config.repository = repository
        base1 = os.path.join(os.getcwd(), "packages", "pkg1-1")
        self.bombardier.repository = repository


        self.config.data = {"bom": ["base"]}
        pkg_data = {"pkg1": {"install": {"fullName":"pkg1-1"}, "package-version": 4},
                       "pkg2": {"install": {"fullName":"pkg2-1"},
                                "dependencies": [ "pkg1"], "package-version": 4}}
        repository = Repository.Repository(INSTANCE, pkg_data)
        self.config.repository = repository
        base1 = os.path.join(os.getcwd(), "packages", "pkg1-1")
        base2 = os.path.join(os.getcwd(), "packages", "pkg2-1")
        self.bombardier.repository = repository

        try:
            self.bombardier.reconcile_system(DRY_RUN)
        except SystemExit, e:
            assert e.code == OK

    def test_reconcile_system_bogus(self):
        self.config.data = {"bom": ["base"]}
        install_progress_3 = {"install-progress":
                                {"TestPackage-7-1": {"INSTALLED": time.ctime(),
                                                    "UNINSTALLED": 'NA',
                                                    "VERIFIED": time.ctime(),
                                                   },
                                },
                             }
        _write_progress(install_progress_3)
        pkg_data = {"TestPackage-7": {"install": {"fullName":"TestPackage-7"},
                                      "package-version": 4},
                   }
        repository = Repository.Repository(INSTANCE, pkg_data)
        self.config.repository = repository
        base1 = os.path.join(os.getcwd(), "packages", "TestPackage-7")
        self.bombardier.repository = repository
        try:
            self.bombardier.reconcile_system(DRY_RUN)
        except SystemExit, e:
            assert e.code == OK

    def NOtest_check_system_1(self):
        #^ get the system to believe that it has some package installed
        now = time.ctime()
        install_progress = {"install-progress":
                           {"reconfig-1": {"INSTALLED": now,
                                           "UNINSTALLED": 'NA',
                                           "VERIFIED": now},
                           "stable-1": {"INSTALLED": now,
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": now}}}
        pkg_data = {"reconfig": {"install": {"fullName":"reconfig-1"},
                                 "configuration": {"section1":"option1"}, "package-version": 4},
                    "stable": {"install": {"fullName":"stable-1"},
                               "configuration": {"section2":"option2"}, "package-version": 4}}
        _write_progress(install_progress)
        repository = Repository.Repository(INSTANCE, pkg_data)

        #^ get the system to believe it has a configuration fingerprint saved
        old_config_data = {"section1": {"option1": "spam"}, "section2": {"option2": "eggs"}}
        self.config.savedYamlData["reconfig"] = old_config_data
        self.config.savedYamlData["stable"] = old_config_data

        #^ set the current fingerprint to something different
        self.config.data = {"section1": {"option1": "foo"}, "section2": {"option2": "eggs"}}
        self.config.data["packages"] = ["reconfig", "stable"]

        #^ have the system report that the package with different config data needs attention
        self.config.repository = repository
        self.bombardier.repository = repository
        pkg_info = self.bombardier.check_system()
        assert pkg_info["reconfigure"] == {'reconfig': ['/section1']}, pkg_info
        assert pkg_info["ok"] == ["stable"], pkg_info

    def NOtest_check_system_2(self):
        now = time.ctime()
        install_progress = {"install-progress":
                           {"reconfig-1": {"INSTALLED": now,
                                           "UNINSTALLED": 'NA',
                                           "VERIFIED": now},
                           "stable-1": {"INSTALLED": now,
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": now}}}
        _write_progress(install_progress)
        pkg_data = {"reconfig": {"install": {"fullName":"reconfig-1"},
                                 "configuration": {}, "package-version": 4},
                    "stable": {"install": {"fullName":"stable-1"},
                               "configuration": {"section2":"option2"}, "package-version": 4}}
        repository = Repository.Repository(INSTANCE, pkg_data)
        old_config_data = {"section1": {"option1": "spam"}, "section2": {"option2": "eggs"}}
        self.config.savedYamlData["reconfig"] = old_config_data
        self.config.savedYamlData["stable"] = old_config_data
        self.config.data = {"section1": {"option1": "foo"}, "section2": {"option2": "eggs"}}
        self.config.data["packages"] = ["reconfig", "stable"]
        self.config.repository = repository
        self.bombardier.repository = repository
        packageData = self.bombardier.check_system()
        assert pkg_info["reconfigure"] == {}, pkg_info
        assert "reconfig" in pkg_info["ok"], pkg_info


if __name__ == "__main__":
    suite = unittest.TestSuite()
    #suite.addTest(BombardierTest("test_remove_virtual_package"))
    #suite.addTest(BombardierTest("test_reconcile_system_bogus"))
    #suite.addTest(BombardierTest("test_add_to_dependency_errors"))
    #suite.addTest(BombardierTest("test_add_to_dependency_errors"))
    #suite.addTest(BombardierTest("test_add_to_dependency_errors"))
    #suite.addTest(BombardierTest("test_bogus_dependency"))
    #suite.addTest(BombardierTest("test_check_bom"))
    #suite.addTest(BombardierTest("test_create_package_chains"))
    #suite.addTest(BombardierTest("test_get_actual_pkn"))
    #suite.addTest(BombardierTest("test_get_packages_to_remove"))
    #suite.addTest(BombardierTest("test_get_pkn_list_from_vpn"))
    #suite.addTest(BombardierTest("test_get_top_priority"))
    #suite.addTest(BombardierTest("test_get_vpn_from_pkn"))
    #suite.addTest(BombardierTest("test_find_install_order"))
    #suite.addTest(BombardierTest("test_package_chain_with_broken"))
    #suite.addTest(BombardierTest("test_package_dep"))
    #suite.addTest(BombardierTest("test_reconcile_system1"))
    #suite.addTest(BombardierTest("test_reconcile_system_with_dependencies"))
    suite.addTest(BombardierTest("test_get_packages_to_remove_1"))
    #suite.addTest(BombardierTest("test_check_system_1"))
    #suite.addTest(BombardierTest("test_check_system_2"))
    #suite.addTest(unittest.makeSuite(BombardierTest))
    status = unittest.TextTestRunner(verbosity=2).run(suite)
    errors = len(status.errors) + len(status.failures)
    if not errors:
        setup_tests.cleanup()
