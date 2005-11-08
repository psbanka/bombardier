#!/cygdrive/c/Python24/python.exe

# SQLObjectType2.py: Handles all kinds of static data.

# Copyright (C) 2005 Seth de l'Isle, Peter Banka

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

import Exceptions

name2code = {"Alert":2109440,
    "AlertSystem":2101248,
    "AllDatabaseObjects":511,
    "AllDatabaseUserObjects":509,
    "Application":0,
    "AutoProperty":188416,
    "Backup":184320,
    "BackupDevice":139264,
    "BulkCopy":204800,
    "Category":2134016,
    "Check":49152,
    "Column":24576,
    "Configuration":159744,
    "ConfigValue":163840,
    "Database":135168,
    "DatabaseRole":225280,
    "DBFile":212992,
    "DBObject":28672,
    "DBOption":32768,
    "Default":64,
    "DistributionArticle":1134592,
    "DistributionDatabase":1118208,
    "DistributionPublication":1130496,
    "DistributionPublisher":1105920,
    "DistributionSubscription":1138688,
    "Distributor":1097728,
    "DRIDefault":53248,
    "FileGroup":208896,
    "FullTextCatalog":266240,
    "FullTextService":270336,
    "Index":16384,
    "IntegratedSecurity":45056,
    "Job":2117632,
    "JobFilter":2166784,
    "JobHistoryFilter":2170880,
    "JobSchedule":2174976,
    "JobServer":2105344,
    "JobStep":2121728,
    "Key":20480,
    "LinkedServer":233472,
    "LinkedServerLogin":262144,
    "LogFile":217088,
    "Login":143360,
    "Language":147456,
    "MergeArticle":1073152,
    "MergePublication":1069056,
    "MergePullSubscription":1081344,
    "MergeSubscription":1077248,
    "MergeSubsetFilter":1142784,
    "Operator":2113536,
    "Permission":40960,
    "ProcedureParameter":36864,
    "Publisher":1089536,
    "QueryResults":167936,
    "RegisteredServer":200704,
    "RegisteredSubscriber":1110016,
    "Registry":176128,
    "RemoteLogin":155648,
    "RemoteServer":151552,
    "Replication":1085440,
    "ReplicationDatabase":1114112,
    "ReplicationSecurity":1101824,
    "ReplicationStoredProcedure":1126400,
    "ReplicationTable":1122304,
    "Restore":229376,
    "Rule":128,
    "Schedule":2162688,
    "ServerGroup":192512,
    "ServerRole":221184,
    "SQLServer":131072,
    "StoredProcedure":16,
    "Subscriber":1093632,
    "SystemDatatype":4096,
    "SystemTable":2,
    "TargetServer":2125824,
    "TargetServerGroup":2129920,
    "TransactionLog":172032,
    "TransArticle":1056768,
    "Transfer":180224,
    "TransPublication":1069056,
    "TransPullSubscription":1064960,
    "TransSubscription":1060864,
    "Trigger":256,
    "Unknown":16384,
    "User":8192,
    "UserDefinedDatatype":4096,
    "UserDefinedFunction":1,
    "UserTable":8,
    "View":4}

code2name={}
for name, code in name2code.items():
    code2name[code] = name

def mapping(key):
    return code2name[key]

def path2database(path):
    nodes = path.split('.')
    if nodes[0] == 'Databases':
        return nodes[1]
    else:
        return None

def path2table(path):
    nodes = path.split('.')
    if nodes[2] == 'Tables':
        return nodes[3]
    else:
        return None

def type2path(path, typeName, objectName):

    database = path2database(path)
    table = path2table(path)       #FIXME: This may be good or it may be bad.

    if typeName == 'UserTable':
        return "Databases.%s.Tables.%s" %(database, objectName)

    elif typeName == 'View':
        return "Databases.%s.Views.%s" %(database, objectName)

    elif typeName == 'StoredProcedure':
        return "Databases.%s.StoredProcedures.%s" %(database, objectName)

    elif typeName == 'Index':
        return "Databases.%s.Tables.%s.Indexes.%s" %(database, table, objectName)

    elif typeName == 'UserDefinedDatatype':
        return "Databases.%s.UserDefinedDatatypes.%s" %(database, objectName)

    elif typeName == 'UserDefinedFunction':
        return "Databases.%s.UserDefinedFunctions.%s" %(database, objectName)

    else:
        raise Exceptions.UnknownType( typeName, objectName )
