#!/cygdrive/c/Python23/python.exe

import mock, unittest
import Tcommon
Tcommon.checkImportPath("Config.py")
import Config
from staticData import PROGRESS_FILE, OLD_PROGRESS_FILE

mockFD = mock.Mock( { 'read': 'calling read method', 
                      'write': 'calling write method' }
                  )

class Os(mock.Mock):
  def __init__(self):
    mock.Mock.__init__(self)

  class path(mock.Mock):
    def __init__(self):
      mock.Mock.__init__(self)

    def join(self,*data):
      mock.Mock.__getattr__(self, 'join')(data)
      if data[1] == PROGRESS_FILE:
        return 'test1'
      elif data[1] == OLD_PROGRESS_FILE:
        return 'test2'
      else:
        return 'test3'

    def isfile(self,file):
      mock.Mock.__getattr__(self,'isfile')(file)
      self.changingIsFile(file)

    def changingIsFile(self,file):
      pass


mockUtil   = mock.Mock( { 'convertProgressData': 'Converted' } )
mockOsPath = Os.path()

def stubOpen(file,mode='r',bytes=None):
  return mockFD

def stubIsFileTest1(file):
  if file == 'test1':
    return True
  return False

def stubIsFileTest2(file):
  if file == 'test2':
    return True
  return False

__builtins__.open = stubOpen

class ConfigTest(unittest.TestCase):
  def testGetProgressPath(self):

    Config.utility = mockUtil
    Config.os.path = mockOsPath

  # Test1: 
    Config.os.path.changingIsFile = stubIsFileTest1
    path = Config.getProgressPath()

    assert path == 'test1', 'Should always return test1'
     
    print mockFD.getAllCalls()
    print mockUtil.getAllCalls()
    print mockOsPath.getAllCalls()
    print "=" * 60

    # test 2
    Config.os.path.changingIsFile = stubIsFileTest2
    path = Config.getProgressPath()
    assert path == 'test1', 'Should always return test1'

    FDCalls = mockFD.getAllCalls()
    print type(FDCalls)
    # Should have had 2 file descriptor calls here
#    assert len(FDCalls) == 2, 'Should have called 1 read 1 write'
#    assert FDCalls[0] == 'read()'
#    assert FDCalls[1] == 'write()'

    print mockFD.getAllCalls()
    print mockUtil.getAllCalls()
    print mockOsPath.getAllCalls()

if __name__ == '__main__':
  suite = unittest.TestSuite()
  suite.addTest(ConfigTest("testGetProgressPath"))
  unittest.TextTestRunner(verbosity=2).run(suite)

