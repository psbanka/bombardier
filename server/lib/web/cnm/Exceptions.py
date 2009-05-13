class InvalidServerHome(Exception):
    def __init__(self, server_home):
        Exception.__init__(self)
        self.server_home = server_home
    def __repr__(self):
        return "Server home value is not valid: %s" % self.server_home
    def __str__(self):
        return self.__repr__()
