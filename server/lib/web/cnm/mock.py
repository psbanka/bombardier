#
# (c) Dave Kirby 2001
# dkirby@bigfoot.com
#
'''
The Mock class emulates any other class for testing purposes.
All method calls are stored for later examination.
The class constructor takes a dictionary of method names and the values
they return.  Methods that are not in the dictionary will return None.
'''


class Mock:
    def __init__(self, returnValues=None ):
        self.mockCalledMethods = {}
        self.mockAllCalledMethods = []
        self.mockReturnValues = returnValues or {}
        
    def __getattr__( self, name ):
        return MockCaller( name, self )
    
    def getAllCalls(self):
        '''return a list of MockCall objects,
        representing all the methods in the order they were called'''
        return self.mockAllCalledMethods

    def getNamedCalls(self, methodName ):
        '''return a list of MockCall objects,
        representing all the calls to the named method in the order they were called'''
        return self.mockCalledMethods.get(methodName, [] )

class MockCall:
    def __init__(self, name, params, kwparams ):
        self.name = name
        self.params = params
        self.kwparams = kwparams
    def getParam( self, n ):
        if type(n) == type(1):
            return self.params[n]
        elif type(n) == type(''):
            return self.kwparams[n]
        else:
            raise IndexError, 'illegal index type for getParam'
    def getName(self):
        return self.name
    
    #pretty-print the method call
    def __str__(self):
        s = self.name + "("
        sep = ''
        for p in self.params:
            try:
                s = s + sep + repr(p)
            except:
                s = s + sep + "<UNPRINTABLE>"
            sep = ', '
        for k,v in self.kwparams.items():
            s = s + sep + k+ '='+repr(v)
            sep = ', '
        s = s + ')'
        return s
    def __repr__(self):
        return self.__str__()

class MockCaller:
    def __init__( self, name, mock):
        self.name = name
        self.mock = mock
    def __call__(self,  *params, **kwparams ):
        thisCall = MockCall( self.name, params, kwparams )
        calls = self.mock.mockCalledMethods.get(self.name, [] )
        if calls == []:
            self.mock.mockCalledMethods[self.name] = calls
        calls.append(thisCall)
        self.mock.mockAllCalledMethods.append(thisCall)
        return self.mock.mockReturnValues.get(self.name)
            
