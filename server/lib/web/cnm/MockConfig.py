class MockDict:
    """In order to instantiate a target installer package and discover what
    configuration items it might need, we construct a dictionary-like object
    that records all of the queries that is made to it"""
    def __init__(self, name):
        "Create the basic mock dictionary"
        self.name = name
        self.dicts = []

    def get(self, key):
        "Allows for treating the object like a dictionary"
        md = MockDict(key)
        self.dicts.append(md)
        return md

    def get_requests(self):
        "Divulges all of the requests that have been made to this object"
        output = {}
        for subdict in self.dicts:
            key = subdict.name
            if subdict.dicts:
                value = subdict.get_requests()
            else:
                value = ""
            output[key] = value
        return output

def parse_section(mydict, section_string, default=''):
    """This routine provides for data to be accessed in "dot-syntax" rather
    than using brackets (e.g. a dictionary such as obj = {'foo': {'bar: 1}}
    can be accessed with obj("foo.bar") rather than obj["foo"]["bar"] )"""

    sections = section_string.split('.')
    current_section = sections[0]
    if len(sections) > 1:
        if not mydict.has_key(current_section):
            mydict[current_section] = {}
        remaining_section_string = '.'.join(sections[1:])
        mydict[current_section] = parse_section(mydict[current_section],
                                                remaining_section_string,
                                                default)
    else:
        mydict[current_section] = default
    return mydict

class MockConfig:
    """This object mocks the bombardier.Config object in order to discover
    what configuration items are being requested"""

    def __init__(self):
        self.requests = {}
        self.data = MockDict("/")

    def string(self, section, default='', optional=False):
        """Obtain a string object from the configuration. Note that if the
        item we're looking for is OPTIONAL, then we don't record it."""
        if not optional:
            self.requests = parse_section(self.requests, section, default)
        return default

    def integer(self, section, default='', optional=False):
        "Obtain an integer from the configuration"
        if not optional:
            self.requests = parse_section(self.requests, section, default)
        return default

    def dictionary(self, section, default={}, optional=False):
        "Obtain a dictionary object from the configuration"
        if not optional:
            self.requests = parse_section(self.requests, section, default)
        return default

    def listobj(self, section, default=[], optional=False):
        "obtain a list object from the configuration"
        if not optional:
            self.requests = parse_section(self.requests, section, [])
        return default

    def get(self, section, option, default="", optional=False):
        "Allow this object to be treated like a dictionary"
        if not self.requests.has_key(section):
            self.requests[section] = {}
        if not optional:
            self.requests[section][option] = default
        return default

    def get_requests(self):
        """This is where the mock object tattles on what information was
        requested"""
        requests = self.requests
        data_requests = self.data.get_requests()
        for key in data_requests.keys():
            requests[key] = data_requests[key]
        return requests

    def get_instance(self):
        """Some packages want to know what instance they are running on.
        This is implemented in a trivial way in the mock system."""
        return "BUILD"


