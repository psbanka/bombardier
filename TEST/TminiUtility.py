
    def testSpkgPath(self):
        self.windows.registryQueries = ["c:\\"]
        foo = bombardier.Config.getSpkgPath(self.windows)
        assert foo == "c:\\", foo
        rcalls = self.windows.getAllCalls()
        assert len(rcalls) == 1
        assert `rcalls[0]`.startswith("queryValue('"), `rcalls[0]`

    def testGetProgressPath(self):
        self.windows.registryQueries = ['','']
        self.filesystem.files = [PROGRESS_FILE]
        progressPath = miniUtility.getProgressPath()
        assert progressPath == "install-progress.yml", progressPath
