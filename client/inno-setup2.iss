[Setup]
AppName=Bombardier
AppVerName=Bombardier 0.4-pre-1
DefaultDirName=C:\spkg
DefaultGroupName=Bombardier
AppCopyright=Copyright 2004, 2005 Peter Banka et al
Uninstallable=true
SourceDir=.
OutputBaseFileName=bombardierSetup-0.4
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
Source: release\python-2.4.1.msi; DestDir: {app}\dependencies
Source: release\repositoryDirectory.yml; DestDir: {app}

; Support tools
Source: release\bombardier-0.4.tar.gz; DestDir: {app}\scratch
Source: spkgDir\rescue.py; DestDir: {app}\scratch

[Icons]
Name: {group}\Bombardier; Filename: {app}\bc.py; WorkingDir: {app}; HotKey: ctrl+alt+b; Flags: dontcloseonexit
Name: {group}\Uninstall Bombardier; Filename: {uninstallexe}
Name: {group}\Management Interface; Filename: {app}\website.url

[Registry]
Root: HKLM; Subkey: Software\GE-IT; Flags: uninsdeletekeyifempty
Root: HKLM; Subkey: Software\GE-IT\Bombardier; Flags: uninsdeletekey
Root: HKLM; Subkey: Software\GE-IT\Bombardier; ValueType: string; ValueName: InstallPath; ValueData: {app}

[Run]
Filename: {win}\SYSTEM32\msiexec.exe; Parameters: /Ipython-2.4.1.msi ALLUSERS=1 /QN; WorkingDir: {app}\dependencies; StatusMsg: Installing Python, please wait...; Flags: postinstall; Description: Install Python
;Filename: {reg:HKLM\Software\Python\PythonCore\2.3\InstallPath,(Default)|C:\Python23}\pythonw.exe; WorkingDir: {reg:HKLM\Software\Python\PythonCore\2.3\InstallPath,(Default)|C:\Python23}\Lib\site-packages\win32com\client; Flags: runhidden postinstall; Description: Register Windows COM services; StatusMsg: Registering Windows COM services; Parameters: "makepy.py ""COM + 1.0 Admin Type Library"""
Filename: {reg:HKLM\Software\Python\PythonCore\2.4\InstallPath,(Default)|C:\Python24}\python.exe; WorkingDir: {app}\scratch; Flags: postinstall shellexec; Description: Install Python modules; StatusMsg: Installing Bombardier Python Modules; Parameters: rescue.py -g

[UninstallDelete]
Name: {app}\ntrights.exe; Type: files
Name: {app}\sql-config-orig.aut; Type: files
Name: {app}\access-config-orig.aut; Type: files
Name: {app}\bc.py; Type: files
Name: {app}\setup.py; Type: files
Name: {app}\systemtype.txt; Type: files
Name: {app}\BOM.txt; Type: files
Name: {app}\consoleCheck.txt; Type: files
Name: {reg:HKLM\Software\Python\PythonCore\2.4\InstallPath,(Default)|C:\Python24}\Lib\site-packages\bombardier.py; Type: files
Name: {reg:HKLM\Software\Python\PythonCore\2.4\InstallPath,(Default)|C:\Python24}\Lib\site-packages\BombardierClientService.py; Type: files
Name: {reg:HKLM\Software\Python\PythonCore\2.4\InstallPath,(Default)|C:\Python24}\Lib\site-packages\BombardierAgent.py; Type: files
Name: {reg:HKLM\Software\Python\PythonCore\2.4\InstallPath,(Default)|C:\Python24}\Lib\site-packages\quickPut.py; Type: files
Name: {reg:HKLM\Software\Python\PythonCore\2.4\InstallPath,(Default)|C:\Python24}\Lib\site-packages\RegistryDict.py; Type: files
Name: {reg:HKLM\Software\Python\PythonCore\2.4\InstallPath,(Default)|C:\Python24}\Lib\site-packages\Package.py; Type: files
Name: {reg:HKLM\Software\Python\PythonCore\2.4\InstallPath,(Default)|C:\Python24}\Lib\site-packages\ReconcileThread.py; Type: files
Name: {reg:HKLM\Software\Python\PythonCore\2.4\InstallPath,(Default)|C:\Python24}\Lib\site-packagesRepository.py; Type: files
Name: {reg:HKLM\Software\Python\PythonCore\2.4\InstallPath,(Default)|C:\Python24}\Lib\site-packages\Config.py; Type: files
Name: {reg:HKLM\Software\Python\PythonCore\2.4\InstallPath,(Default)|C:\Python24}\Lib\site-packages\staticData.py; Type: files
Name: {reg:HKLM\Software\Python\PythonCore\2.4\InstallPath,(Default)|C:\Python24}\Lib\site-packages\Logger.py; Type: files
Name: {reg:HKLM\Software\Python\PythonCore\2.4\InstallPath,(Default)|C:\Python24}\Lib\site-packages\utility.py; Type: files
Name: {reg:HKLM\Software\Python\PythonCore\2.4\InstallPath,(Default)|C:\Python24}\Lib\site-packages\commonUtil.py; Type: files
Name: {reg:HKLM\Software\Python\PythonCore\2.4\InstallPath,(Default)|C:\Python24}\Lib\site-packages\webops.py; Type: files
Name: {app}\log; Type: filesandordirs
Name: {app}\dependencies; Type: filesandordirs
Name: {app}\bin; Type: filesandordirs
Name: {app}\scratch; Type: filesandordirs

[UninstallRun]
Filename: {reg:HKLM\Software\Python\PythonCore\2.4\InstallPath,(Default)|C:\Python24}\pythonw.exe; WorkingDir: {reg:HKLM\Software\Python\PythonCore\2.4\InstallPath,(Default)|C:\Python24}\Lib\site-packages\bombardier; Parameters: BombardierAgent.py remove
Filename: {reg:HKLM\Software\Python\PythonCore\2.4\InstallPath,(Default)|C:\Python24}\pythonw.exe; WorkingDir: {reg:HKLM\Software\Python\PythonCore\2.4\InstallPath,(Default)|C:\Python24}\Lib\site-packages\bombardier; Parameters: BombardierClient.py remove
Filename: {win}\system32\regsvr32; Parameters: /s AutoItX3.dll; WorkingDir: c:\spkg
