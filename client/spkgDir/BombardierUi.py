#!/cygdrive/c/Python24/python.exe

# BombardierUi.py: Main launching app for the Bombardier GUI. It's
# largely a container for a few other panels. This was built using the
# wonderful BoaConstructor.

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

#Boa:App:BoaApp

import wx
import Frame1 as Frame1
import bombardier.miniUtility, sys
from bombardier.staticData import *

modules ={'Frame1': [1, 'Main frame of Application', u'Frame1'],
 u'statusPanel': [0, '', u'wxPanel1.py'],
 u'logPanel': [0, '', u'wxPanel2.py']}

class BoaApp(wx.App):
    def OnInit(self):
        wx.InitAllImageHandlers()
        self.main = Frame1.create(None)
        self.main.Show()
        self.SetTopWindow(self.main)
        return True

def main():
    application = BoaApp(0)
    application.MainLoop()

if __name__ == '__main__':
    actionPath = os.path.join(bombardier.miniUtility.getSpkgPath(),
                              ACTION_FILE)
    if sys.argv[-1] == '-a':
        open(actionPath, 'w').write('a')
    else:
        open(actionPath, 'w').write('')
    main()
