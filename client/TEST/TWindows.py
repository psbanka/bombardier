#!/cygdrive/c/Python25/python.exe

    ################################
    ## Credentials stuff

    def testAutoLogin(self): #^ FIXME: broken on Sean's machine
        self.windows.credentialResults = [FAIL, OK]
        self.windows.makeUserResults = [OK]
        self.config.username = "cheezie"
        self.config.password = "pooft34dkdkdr!!"
        self.config.domain   = ""
        status = self.config.autoLogin()
        assert status == OK, "Cannot configure user to autoLogin"
        wcalls = self.windows.getAllCalls()
        assert len(wcalls) == 6, len(wcalls)
        assert `wcalls[0]`.startswith("testCredentials('cheezie', '', 'pooft34dkdkdr!!', "), `wcalls[0]`
        assert `wcalls[1]`.startswith("mkServiceUser('cheezie', 'pooft34dkdkdr!!', '', "\
                                      "'Bombardier administrative user'"), `wcalls[1]`
        assert `wcalls[2]`.startswith("testCredentials('cheezie', '', 'pooft34dkdkdr!!', "), `wcalls[2]`
        assert `wcalls[3]`.startswith(r"setKey('SOFTWARE\\Microsoft\\Windows NT\\Current"), `wcalls[3]`
        assert `wcalls[4]`.startswith(r"setKey('SOFTWARE\\Microsoft\\Windows NT\\Current"), `wcalls[4]`
        assert `wcalls[5]`.startswith(r"setKey('SOFTWARE\\Microsoft\\Windows NT\\Current"), `wcalls[5]`

    def testexecute(self):
        status = self.windows.execute('hostname', 'messed up',
                                      dieOnExit=False,
                                      captureOutput=True)
        testStr = "output:%s" % socket.gethostname().lower()
        assert status == OK

    def testCheckAutoLogin(self):
        bombardier.Config.noAutoLogin(self.windows)
        wcalls = self.windows.getAllCalls()
        assert len(wcalls) == 4, len(wcalls)
        assert `wcalls[0]`.startswith(r"setKey('SOFTWARE\\Microsoft\\Windows NT\\Current"), `wcalls[0]`
        assert `wcalls[1]`.startswith(r"setKey('SOFTWARE\\Microsoft\\Windows NT\\Current"), `wcalls[1]`
        assert `wcalls[2]`.startswith(r"setKey('SOFTWARE\\Microsoft\\Windows NT\\Current"), `wcalls[2]`
        assert `wcalls[3]`.startswith(r"setKey('SOFTWARE\\Microsoft\\Windows NT\\Current"), `wcalls[3]`
        
    def testwatchForTermination(self):
        consolePath = os.path.join(miniUtility.getSpkgPath(), CONSOLE_MONITOR)
        if os.path.isfile(consolePath):
            os.unlink(consolePath)
        status = self.filesystem.watchForTermination(sleepTime = 1.0, timeout = 1)
        assert status == FAIL