#Boa:FramePanel:Panel1

import wx
import threading, pywintypes, win32pipe, time

from bombardier.staticData import *
import bombardier.StatusThread as StatusThread
import bombardier.CommSocket as CommSocket
import bombardier.Filesystem as Filesystem
import bombardier.miniUtility as miniUtility

class MessageThread(threading.Thread):
    def __init__(self, commSocket):
        threading.Thread.__init__(self)
        self.commSocket = commSocket
    
    def run(self):
        while not self.commSocket.testStop():
            try:
                win32pipe.CallNamedPipe(BC_PIPE_NAME, CHECK, 256,
                                        win32pipe.NMPWAIT_WAIT_FOREVER)
                return OK
            except pywintypes.error, details:
                time.sleep(1)
                continue


[wxID_PANEL1, wxID_PANEL1APPLICATION, wxID_PANEL1CURRENTACTION, 
 wxID_PANEL1LIGHT, wxID_PANEL1MAIN, wxID_PANEL1OVERALL, wxID_PANEL1START, 
 wxID_PANEL1STATICTEXT1, wxID_PANEL1STATICTEXT2, wxID_PANEL1STATICTEXT3, 
 wxID_PANEL1STATICTEXT4, wxID_PANEL1STOP, wxID_PANEL1SUB, wxID_PANEL1TODOS, 
] = [wx.NewId() for _init_ctrls in range(14)]

class Panel1(wx.Panel):
    def _init_coll_todos_Columns(self, parent):
        # generated method, don't edit

        parent.InsertColumn(col=0, format=wx.LIST_FORMAT_LEFT,
              heading=u'Package Name', width=400)
        parent.InsertColumn(col=1, format=wx.LIST_FORMAT_LEFT, heading=u'From',
              width=150)

    def _init_ctrls(self, prnt):
        # generated method, don't edit
        wx.Panel.__init__(self, id=wxID_PANEL1, name='', parent=prnt,
              pos=wx.Point(290, 98), size=wx.Size(612, 405),
              style=wx.TAB_TRAVERSAL)
        self.SetClientSize(wx.Size(604, 378))

        self.main = wx.StaticText(id=wxID_PANEL1MAIN,
              label=u'Bombardier not started', name=u'main', parent=self,
              pos=wx.Point(48, 32), size=wx.Size(185, 20), style=0)
        self.main.SetFont(wx.Font(12, wx.SWISS, wx.NORMAL, wx.BOLD, False,
              u'Microsoft Sans Serif'))

        self.sub = wx.StaticText(id=wxID_PANEL1SUB,
              label=u'No packages to install', name=u'sub', parent=self,
              pos=wx.Point(48, 64), size=wx.Size(127, 13), style=0)
        self.sub.SetFont(wx.Font(8, wx.SWISS, wx.NORMAL, wx.BOLD, False,
              u'Microsoft Sans Serif'))

        lightPath = os.path.join(miniUtility.getSpkgPath(), OFFLIGHT)
        self.light = wx.StaticBitmap(bitmap=wx.Bitmap(lightPath, wx.BITMAP_TYPE_PNG),
                                     id=wxID_PANEL1LIGHT, name='light',
                                     parent=self, pos=wx.Point(16, 32), size=wx.Size(11, 130),
                                     style=0)

        self.staticText1 = wx.StaticText(id=wxID_PANEL1STATICTEXT1,
              label=u'Overall progress', name='staticText1', parent=self,
              pos=wx.Point(96, 88), size=wx.Size(76, 13), style=0)

        self.staticText2 = wx.StaticText(id=wxID_PANEL1STATICTEXT2,
              label=u'Current application', name='staticText2', parent=self,
              pos=wx.Point(96, 112), size=wx.Size(88, 13), style=0)

        self.staticText3 = wx.StaticText(id=wxID_PANEL1STATICTEXT3,
              label=u'Current action', name='staticText3', parent=self,
              pos=wx.Point(96, 136), size=wx.Size(66, 13), style=0)

        self.overall = wx.Gauge(id=wxID_PANEL1OVERALL, name=u'overall',
              parent=self, pos=wx.Point(208, 88), range=100, size=wx.Size(376,
              16), style=wx.GA_HORIZONTAL)

        self.application = wx.Gauge(id=wxID_PANEL1APPLICATION,
              name=u'application', parent=self, pos=wx.Point(208, 112),
              range=100, size=wx.Size(376, 16), style=wx.GA_HORIZONTAL)

        self.currentAction = wx.StaticText(id=wxID_PANEL1CURRENTACTION,
              label=u'idle', name=u'currentAction', parent=self,
              pos=wx.Point(208, 136), size=wx.Size(16, 13), style=0)

        self.todos = wx.ListCtrl(id=wxID_PANEL1TODOS, name=u'todos',
              parent=self, pos=wx.Point(16, 200), size=wx.Size(568, 128),
              style=wx.LC_REPORT | wx.VSCROLL)
        self.todos.SetLabel(u'Applications to install')
        self.todos.SetToolTipString(u'Applications left to install')
        self._init_coll_todos_Columns(self.todos)

        self.staticText4 = wx.StaticText(id=wxID_PANEL1STATICTEXT4,
              label=u'Applications remaining to install', name='staticText4',
              parent=self, pos=wx.Point(16, 176), size=wx.Size(146, 13),
              style=0)

        self.start = wx.Button(id=wxID_PANEL1START, label=u'Start',
              name=u'start', parent=self, pos=wx.Point(16, 344),
              size=wx.Size(75, 23), style=0)
        self.start.Bind(wx.EVT_BUTTON, self.OnStartButton, id=wxID_PANEL1START)

        self.stop = wx.Button(id=wxID_PANEL1STOP, label=u'Stop', name=u'stop',
              parent=self, pos=wx.Point(112, 344), size=wx.Size(75, 23),
              style=0)
        self.stop.Bind(wx.EVT_BUTTON, self.OnStopButton, id=wxID_PANEL1STOP)

    def __init__(self, parent, id, pos, size, style, name, iconControl,
                 filesystem = Filesystem.Filesystem()):
        self._init_ctrls(parent)
        self.iconControl = iconControl
        self.statusCommSocket = CommSocket.CommSocket()
        self.statusThread = StatusThread.StatusThread(self.overall.SetValue,
                                                      self.application.SetValue,
                                                      self.main.SetLabel,
                                                      self.sub.SetLabel,
                                                      self.currentAction.SetLabel,
                                                      self.setLightColor,
                                                      self.iconControl,
                                                      self.todos,
                                                      self.statusCommSocket,
                                                      filesystem)
        self.statusThread.start()
        self.messageThread = None

    def stopMessageThread(self):
        if self.messageThread:
            self.messageCommSocket.sendStop()
            self.messageThread.join()

    def stopStatusThread(self):
        self.statusCommSocket.sendStop()
        self.statusThread.join()

    def __del__(self):
        self.stopMessageThread()
        self.stopStatusThread()
        
    def setLightColor(self, color):
        filePath = os.path.join(miniUtility.getSpkgPath, color)
        self.light.SetBitmap(wx.Bitmap(filePath, wx.BITMAP_TYPE_PNG))

    def OnStartButton(self, event):
        self.messageCommSocket = CommSocket.CommSocket()
        self.messageThread = MessageThread(self.messageCommSocket)
        self.messageThread.start()

    def OnStopButton(self, event):
        if self.messageThread:
            self.messageCommSocket.sendStop()
            self.messageThread.join()
