import time
import yaml

basicProgress = """---
install-progress:
    foomanchoo-1:
        INSTALLED: today
"""

statusFile1 = {"status": {"overall": "installing",
                          "package": "testokpackage1",
                          "action": "downloading",
                          "percentage": 10,
                          "main": "Installing Packages"},
               "todo": ["testokpackage2, base",
                        "testdbpackage1, base",
                        "testconsolepackage, test",
                        "testrebootpackage, test"],
               "timestamp": time.time(),
               "install-progress": {"testokpackage2": {"INSTALLED": "today"},
                                    "testdbpackage1": {"INSTALLED": "today"},
                                    "testconsolepackage": {"INSTALLED": "today"},
                                    "testrebootpackage": {"INSTALLED": "today"}}}

statusFile2 = {"status": {"overall": "installing",
                          "package": "testokpackage1",
                          "action": "verifying",
                          "percentage": 20},
               "todo": ["testconsolepackage, test",
                        "testrebootpackage, test"],
               "timestamp": (time.time() - 1000),
               "install-progress": {"testokpackage2": {"INSTALLED": "today"},
                                    "testdbpackage1": {"INSTALLED": "today"},
                                    "testconsolepackage": {"INSTALLED": "today"},
                                    "testrebootpackage": {"INSTALLED": "today"}}}

currentStatus = {"status": {"overall": "installing", 
                            "package": "testokpackage1",
                            "action": "verifying",
                            "percentage": 20},
                 "todo": ["testconsolepackage, test", "testrebootpackage, test"],
                 "timestamp": 1115778289.98}

progressData = {"testbadverifypackage1-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                            "UNINSTALLED": "NA",
                                            "VERIFIED": 'Sat May 07 23:14:27 2005'},
                "testconsolepackage1-1": {"INSTALLED": 'Sat May 07 23:14:42 2005',
                                          "UNINSTALLED": "NA",
                                          "VERIFIED": 'Sat May 07 23:14:42 2005'},
                "testokpackage1-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                     "UNINSTALLED": "NA",
                                     "VERIFIED": 'Sat May 07 23:14:28 2005'}}

packageData = {'testbaduninstallpackage1': {'dependencies': {},
                                            'install': {'priority': '150',
                                                        'console': 'FALSE',
                                                        'fullName': 'testbaduninstallpackage1-1',
                                                        'autoreboot': 'FALSE',
                                                        'reboot': 'FALSE'}},
               'testbadpackage1': {'dependencies': {},
                                   'install': {'priority': '150',
                                               'console': 'FALSE',
                                               'fullName': 'testbadpackage1-1',
                                               'autoreboot': 'FALSE',
                                               'reboot': 'FALSE'}},
               'testdb-data-initial': {'dependencies': {'dep0': 'testdb-structure'},
                                       'backup': {'rootname': 'testdb-data',
                                                  'role': 'data',
                                                  'databases': 'testdb'},
                                       'install': {'priority': '150',
                                                   'console': 'FALSE',
                                                   'fullName': 'testdb-data-initial-1',
                                                   'autoreboot': 'FALSE',
                                                   'reboot': 'FALSE'}}}

configData1 = {"packageGroups": ["dkeyr4-1.0"], 
              "body":{"residence":"portland"}}

configData2 = {"packageGroups": ["dkeyr4-1.0"], 
              "body":{"residence":"portland"},
              "include":["margaret", "bill"]}

margaretData = {"body":{"residence":"Everett"},
                "system": {"ipaddress": "22.22.22.22"}}

billData = {"system": {"serviceuser": "drillbit"},
            "include": ["frank"]}

frankData = {"system": {"servicepasswd": "foomanchoo"}}
               

indexData = {"testsystem": {
             "contact": {"ownedclients": ["peter"]},
             "hardware": {"client": ["testhw"]},
             "project": {"clients":["bombardier"]}}
             }

bombardierProjectData = yaml.load("""---
clients:
    - kwmge01
    - kwmge02
    - kwmqadb01
    - kwmqadb03
    - kwmqadkey01
    - kwmqaweb01
    - kwmqaweb02
    - kwmqaweb03
    - kwmqawebtest
    - uspsupernet
contactid: bbutterfield
finish: '2005-10-30'
start: '2005-07-22'
data:
    configItem: ["foo", "bar"]
""")

testhw = yaml.load("""---
description: secure net for stuff
data:
    configItem2: ["spam", "eggs"]
""")

repodata1 = {"localhost": {"address": "http://127.0.0.1",
                           "networks": ["0.0.0.0/0"],
                           "username": "None",
                           "password": "None"}}

repodata2 = {"QA": {"address": "http://172.77.51.57",
                    "username": "None",
                    "password": "None",
                    "proxy": {"address": "None",
                              "port": "None",
                              "username": "None",
                              "password": "None"}},
             "Production": {"address": "http://172.18.4.88",
                            "username": "None",
                            "password": "None"},
             "Internet": {"address": "http://65.102.15.57",
                          "username": "outside",
                          "password": "please"}}

repoyaml1 = """
localhost:
  address: http://127.0.0.1
  networks: [0.0.0.0/0]
  username: None
  password: None"""

repoyaml2 = """
QA:
  address: http://172.77.51.57
  username: None
  password: None
  proxy:
     address: None
     port: None
     username: None
     password: None

Production:
  address: http://172.77.51.57
  username: None
  password: None
  proxy:
     address: None
     port: None
     username: None
     password: None

Internet:
  address: http://65.102.15.57
  username: outside
  password: please
  proxy:
     address: None
     port: None
     username: None
     password: None
"""

ipconfigData = """Windows 2000 IP Configuration

Ethernet adapter vpn-connection:

        Media State . . . . . . . . . . . : Cable Disconnected

Ethernet adapter eth0:

        Media State . . . . . . . . . . . : Cable Disconnected

Ethernet adapter wireless:

        Connection-specific DNS Suffix  . : e2k.ad.ge.com
        IP Address. . . . . . . . . . . . : 192.168.1.22
        Subnet Mask . . . . . . . . . . . : 255.255.255.0
        Default Gateway . . . . . . . . . : 192.168.1.100

Ethernet adapter {CB33273C-45E2-47C3-A6F9-936B6F7C02BB}:

        Connection-specific DNS Suffix  . :
        Autoconfiguration IP Address. . . : 169.254.241.150
        Subnet Mask . . . . . . . . . . . : 255.255.0.0
        Default Gateway . . . . . . . . . :
"""
