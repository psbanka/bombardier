#Boa:FramePanel:Panel2

import wx
import LogThread as LogThread
import bombardier.CommSocket as CommSocket

[wxID_PANEL2, wxID_PANEL2LOG, 
] = [wx.NewId() for _init_ctrls in range(2)]

class Panel2(wx.Panel):
    def _init_coll_log_Columns(self, parent):
        # generated method, don't edit

        parent.InsertColumn(col=0, format=wx.LIST_FORMAT_LEFT, heading=u'Level',
              width=100)
        parent.InsertColumn(col=1, format=wx.LIST_FORMAT_LEFT,
              heading=u'Message', width=400)
        parent.InsertColumn(col=2, format=wx.LIST_FORMAT_LEFT, heading=u'Time',
              width=100)

    def _init_ctrls(self, prnt):
        # generated method, don't edit
        wx.Panel.__init__(self, id=wxID_PANEL2, name='', parent=prnt,
              pos=wx.Point(301, 197), size=wx.Size(610, 347),
              style=wx.TAB_TRAVERSAL)
        self.SetClientSize(wx.Size(602, 320))

        self.log = wx.ListCtrl(id=wxID_PANEL2LOG, name=u'log', parent=self,
              pos=wx.Point(0, 0), size=wx.Size(595, 389),
              style=wx.VSCROLL | wx.LC_REPORT)
        self._init_coll_log_Columns(self.log)

    def __init__(self, parent, id, pos, size, style, name):
        self._init_ctrls(parent)
        self.logCommSocket = CommSocket.CommSocket()
        self.logThread = LogThread.LogThread(self.log,
                                             self.logCommSocket)
        self.logThread.start()

    def __del__(self):
        self.logCommSocket.sendStop()
        self.logThread.join()

if __name__ == "__init__":
    pass
