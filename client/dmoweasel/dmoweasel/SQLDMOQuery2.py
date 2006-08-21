#!/cygdrive/c/Python24/python.exe
# Version 0.41-168

# SQLDMOQuery.py: Deals with handling queries to an MSSQL database.

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

class SQLDMOQuery:
    def __init__(self, com):
        self.com = com
        self.columns = com.Columns
        self.rows = com.Rows
        self.row = 1

    def keys(self):
        return self._map().keys()
        return map(lambda x: self.com.ColumnName(x),range(0,self.columns+1))

    def __getitem__(self, index):
        mapping = self._map()

        for key in mapping:
            mapping[key] = self.com.GetColumnString(index, mapping[key])

        return mapping

    def _map(self):
        mapping = {}

        for i in range(1, self.columns+1):
            key = self.com.ColumnName(i)
            mapping[key] = i

        return mapping

    def get_rows(self):
        while self.row < self.rows:
            yield self[self.row]
            self.row = self.row + 1

    def output(self):
        results = self.get_rows()
        data = []
        for line in results:
            data.append(line)
        return data
        

if __name__ == '__main__':
   
    import SQLDMOServer
    server  = SQLDMOServer.SQLDMOServer("(local)")
    query   = server.exec_query("hart4", "sp_tables")
    results = query.get_rows()
    for a in results:
        print a 
