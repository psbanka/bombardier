#!/cygdrive/c/Python24/python.exe
#Boa:App:BoaApp

import wx

import Frame1 as Frame1

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
    main()
