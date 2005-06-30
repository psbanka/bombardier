#Boa:Frame:Frame1

import wx
from statusPanel import Panel1 
from logPanel import Panel2

from bombardier.staticData import *
import bombardier.miniUtility as miniUtility
 
def create(parent):
    return Frame1(parent)

[wxID_FRAME1, wxID_FRAME1NOTEBOOK1, wxID_FRAME1PANEL1, wxID_FRAME1PANEL2, 
] = [wx.NewId() for _init_ctrls in range(4)]

TB_CLOSE = wx.NewId()

class Frame1(wx.Frame):

    TBMENU_SHOW = 1000
    TBMENU_HIDE = 1001
    
    _custom_classes = {'wx.Panel': ['Panel1', 'Panel2']}
    def _init_coll_notebook1_Pages(self, parent):
        # generated method, don't edit

        parent.AddPage(imageId=-1, page=self.panel2, select=True,
              text=u'Status')
        parent.AddPage(imageId=-1, page=self.panel1, select=False, text=u'Log')

    def _init_ctrls(self, prnt):
        # generated method, don't edit
        wx.Frame.__init__(self, id=wxID_FRAME1, name='', parent=prnt,
              pos=wx.Point(318, 202), size=wx.Size(611, 442),
              style=wx.DEFAULT_FRAME_STYLE, title=u'Bombardier Client')
        self.SetClientSize(wx.Size(603, 415))
        iconPath = os.path.join(miniUtility.getSpkgPath(), 'big-icon.ico')
        self.SetIcon(wx.Icon(iconPath, wx.BITMAP_TYPE_ICO))

        self.notebook1 = wx.Notebook(id=wxID_FRAME1NOTEBOOK1, name='notebook1',
              parent=self, pos=wx.Point(0, 0), size=wx.Size(603, 415), style=0)

        self.panel1 = Panel2(id=wxID_FRAME1PANEL1, name='panel1',
              parent=self.notebook1, pos=wx.Point(0, 0), size=wx.Size(595, 389),
              style=wx.TAB_TRAVERSAL)

        self.panel2 = Panel1(id=wxID_FRAME1PANEL2, name='panel2',
              parent=self.notebook1, pos=wx.Point(0, 0), size=wx.Size(595, 389),
              style=wx.TAB_TRAVERSAL, iconControl=self.updateIcon)

        self._init_coll_notebook1_Pages(self.notebook1)

    def updateIcon(self, status):
        # update icon based on current state
        if status == OK:
            iconPath = os.path.join(miniUtility.getSpkgPath(), 'big-icon.ico')
            self.tbicon.SetIcon(wx.Icon(iconPath, wx.BITMAP_TYPE_ICO), 'Bombardier - OK')
            self.SetIcon(wx.Icon(iconPath, wx.BITMAP_TYPE_ICO))
        else:
            iconPath = os.path.join(miniUtility.getSpkgPath(), 'error-icon.ico')
            self.tbicon.SetIcon(wx.Icon(iconPath, wx.BITMAP_TYPE_ICO), 'Bombardier - ERROR')
            self.SetIcon(wx.Icon(iconPath, wx.BITMAP_TYPE_ICO))

    def trayIconsInit(self):
        # create tray icon and initialize it        
        self.tbicon = wx.TaskBarIcon()
        self.updateIcon(OK)

        # set up event handlers to catch tray icon events 
        wx.EVT_TASKBAR_LEFT_DCLICK(self.tbicon, self.restoreWindow)
        wx.EVT_TASKBAR_RIGHT_UP(self.tbicon, self.showTaskBarMenu)

        # handlers for tray icon popup menu        
        wx.EVT_MENU(self.tbicon, self.TBMENU_SHOW, self.restoreWindow)
        wx.EVT_MENU(self.tbicon, self.TBMENU_HIDE, self.hideWindow)
        self.Bind(wx.EVT_WINDOW_DESTROY, self.exitApp)

        # handler for main window minimization        
        wx.EVT_ICONIZE(self, self.onWindowMinimize)

        # start with main window showing
        self.Show(True)

    def restoreWindow(self, event):
        # show/restore main window
        self.Show(True)
        self.Iconize(False)

    def hideWindow(self, event):
        # hide main window
        self.Iconize(True)

    def showTaskBarMenu(self, event):
        # create popup menu
        menu = wx.Menu()

        # choose show/hide based on current window state        
        if self.IsIconized():
            menu.Append(self.TBMENU_SHOW, '&Show Window')
        else:
            menu.Append(self.TBMENU_HIDE, '&Hide Window')

        # pop up the menu        
        self.tbicon.PopupMenu(menu)
        menu.Destroy()
        wx.GetApp().ProcessIdle()

    def restoreWindow(self, event):
        # show/restore main window
        self.Show(True)
        self.Iconize(False)

    def onWindowMinimize(self, event):
        # minimize
        self.Iconize(True)
        # hide taskbar button        
        self.Show(False)

    def exitApp(self, event):
        self.tbicon.RemoveIcon()
        self.tbicon.Destroy()
        self.Close()

    def __init__(self, parent):
        self._init_ctrls(parent)
        self.trayIconsInit()

