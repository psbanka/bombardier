#!/cygdrive/c/Python24/python.exe

# MetaData.py: This is little class that acts like both a dictionary
# and a ConfigParser object for convenience sake.

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

import ConfigParser

class MetaData:
    def __init__(self, data):
        self.data = data

    def __getitem__(self, key):
        return self.data[key]

    def __setitem__(self, key, value):
        self.data[key] = value

    def has_key(self, key):
        return self.data.has_key(key)

    def get(self, section_name, option_name, default=None):
        if not self.data.has_key(section_name):
          if default != None:
            return default
          raise ConfigParser.NoSectionError(section_name)
        section = self.data[section_name]
        if not section.has_key(option_name):
          if default != None:
            return default
          raise ConfigParser.NoOptionError(section_name, option_name)
        return section[option_name]

    def has_section(self, section_name):
        if section_name in self.data.keys():
            return True
        return False

