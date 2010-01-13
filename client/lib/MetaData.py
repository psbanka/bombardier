"""This is little class that acts like both a dictionary
and a ConfigParser object for convenience sake."""

# Copyright (C) 2005 Peter Banka

import ConfigParser

class MetaData:
    "Class that provides ConfigParser and dict interface"
    def __init__(self, data):
        "data -- dictionary the class will use"
        self.data = data

    def __getitem__(self, key):
        "part of dict interface"
        return self.data[key]

    def __setitem__(self, key, value):
        "part of dict interface"
        self.data[key] = value

    def has_key(self, key):
        "part of dict interface"
        return self.data.has_key(key)

    def get(self, section_name, option_name, default=None):
        "part of ConfigParser interface"
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
        "part of ConfigParser interface"
        if not self.data.has_key(section_name):
            if section_name in self.data.keys():
                return True
            return False

