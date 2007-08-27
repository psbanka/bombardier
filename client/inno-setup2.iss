[Setup]
AppName=Bombardier
AppVerName=Bombardier 0.5
DefaultDirName=C:\spkg
DefaultGroupName=Bombardier
AppCopyright=Copyright 2004-2007 Peter Banka et al
Uninstallable=true
SourceDir=.
OutputBaseFileName=bombardierSetup-0.5
WizardImageFile=graphics\bomber.bmp
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
Name: {app}\log
Name: {app}\packages; Flags: uninsneveruninstall
Name: {app}\dependencies
Name: {app}\scratch

[Files]
; PYTHON
Source: release\python-2.5.1.msi; DestDir: {app}\dependencies
Source: release\repositoryDirectory.yml; DestDir: {app}

; Support tools
Source: release\bombardier-0.5.tar.gz; DestDir: {app}\scratch
Source: spkgDir\rescue.py; DestDir: {app}\scratch

[Icons]
Name: {group}\Uninstall Bombardier; Filename: {uninstallexe}

[Registry]
Root: HKLM; Subkey: Software\GE-IT; Flags: uninsdeletekeyifempty
Root: HKLM; Subkey: Software\GE-IT\Bombardier; Flags: uninsdeletekey
Root: HKLM; Subkey: Software\GE-IT\Bombardier; ValueType: string; ValueName: InstallPath; ValueData: {app}

[InstallDelete]
Name: {app}\config.yml; Type: files
Name: {app}\bc2.py; Type: files

[Run]
Filename: {win}\SYSTEM32\msiexec.exe; Parameters: /Ipython-2.5.1.msi ALLUSERS=1 /QN; WorkingDir: {app}\dependencies; StatusMsg: Installing Python, please wait...; Flags: postinstall; Description: Install Python
Filename: {reg:HKLM\Software\Python\PythonCore\2.5\InstallPath,(Default)|C:\Python25}\python.exe; WorkingDir: {app}\scratch; Flags: postinstall; Description: Install Python modules; StatusMsg: Installing Bombardier Python Modules; Parameters: rescue.py -n

[UninstallDelete]
Name: {app}\ntrights.exe; Type: files
Name: {app}\bc.py; Type: files
Name: {app}\bc2.py; Type: files
Name: {app}\setup.py; Type: files
Name: {app}\systemtype.txt; Type: files
Name: {app}\BOM.txt; Type: files
Name: {app}\consoleCheck.txt; Type: files
Name: {app}\log; Type: filesandordirs
Name: {app}\dependencies; Type: filesandordirs
Name: {app}\bin; Type: filesandordirs
Name: {app}\scratch; Type: filesandordirs

[UninstallRun]
Filename: {win}\system32\regsvr32; Parameters: /s /u AutoItX3.dll; WorkingDir: c:\spkg
