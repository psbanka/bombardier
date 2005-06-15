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

    def get(self, sectionName, optionName, default=None):
        if not self.data.has_key(sectionName):
          if default != None:
            return default
          raise ConfigParser.NoSectionError(sectionName)
        section = self.data[sectionName]
        if not section.has_key(optionName):
          if default != None:
            return default
          raise ConfigParser.NoOptionError(sectionName, optionName)
        return section[optionName]

    def has_section(self, sectionName):
        if sectionName in self.data.keys():
            return True
        return False

