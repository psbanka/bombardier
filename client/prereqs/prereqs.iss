[Setup]
AppName=BombardierPrerequisites
AppVerName=BombardierPrequisites 0.5
DefaultDirName=C:\Temp
DefaultGroupName=Bombardier
AppCopyright=Copyright 2004-2007 Peter Banka et al
Uninstallable=true
SourceDir=.
OutputBaseFileName=bombardierPrereqs
WizardImageFile=bomber.bmp
RestartIfNeededByRun=true
PrivilegesRequired=admin
DisableDirPage=true
DirExistsWarning=no
DisableProgramGroupPage=true
DisableFinishedPage=false
AlwaysShowComponentsList=false
DisableReadyPage=true
ShowLanguageDialog=no

[Dirs]
Name: {app}\scratch

[Files]
; PYTHON
Source: release\python-2.5.1.msi; DestDir: {app}\scratch

; Support tools
Source: release\bombardierPrereqs-0.5.tar.gz; DestDir: {app}\scratch
Source: rescue.py; DestDir: {app}\scratch

[Icons]

[Registry]

[InstallDelete]

[Run]
Filename: {win}\SYSTEM32\msiexec.exe; Parameters: /Ipython-2.5.1.msi ALLUSERS=1 /QN; WorkingDir: {app}\scratch; StatusMsg: Installing Python, please wait...; Flags: postinstall; Description: Install Python
Filename: {reg:HKLM\Software\Python\PythonCore\2.5\InstallPath,(Default)|C:\Python25}\python.exe; WorkingDir: {app}\scratch; Flags: postinstall; Description: Install Python modules; StatusMsg: Installing Bombardier Python Modules; Parameters: rescue.py

[UninstallDelete]
Name: {app}\scratch; Type: filesandordirs

[UninstallRun]
