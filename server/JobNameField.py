#!/usr/bin/python

import PinshCmd, SecureCommSocket
from commonUtil import *

class JobNameField(PinshCmd.PinshCmd):
    def __init__(self, name = "jobNameField"):
        PinshCmd.PinshCmd.__init__(self, name)
        self.helpText = "<job-name>\tthe name of a scheduled job"
        self.level = 99
        self.cmdOwner = 0

    def match(self, tokens, index):
        possibleMatches = self.name(tokens, index)
        if len(possibleMatches) == 0:
            return INCOMPLETE, 1
        elif len(possibleMatches) == 1:
            return COMPLETE, 1
        else:
            return PARTIAL, 1

    def name(self, tokens, index):
        userInput = [tokens[index]]
        if mode.auth != ADMIN:
            return userInput
        c = SecureCommSocket.SecureClient(TB_CTRL_PORT, mode.password)
        jobs = c.sendSecure(TB_SHOW, [])[0]
        output = [ job for job in jobs if job.startswith(tokens[index]) ]
        if userInput != ['']:
            output += userInput
        return output

if __name__ == "__main__":
    from libTest import startTest, endTest, runTest
    import TimeBom
    mode.auth = ADMIN
    jobNameField = JobNameField()
    status = OK
    TB_CTRL_PORT = 5423
    TEST_JOB_FILE = "testJobFile.yml"

    startTest()
    tb = TimeBom.TimeBom("fooman", '.', TEST_JOB_FILE, TB_CTRL_PORT)
    tb.jobs.addJob(name='checker', bomshCmd='checkerCmd', freq=3600, user='chawn')
    tb.jobs.addJob(name='statlilap', bomshCmd='statlilapCmd', freq=5, user='chawn')
    tb.start()
    mode.password = "fooman"
    
    status = runTest(jobNameField.name, [[""], 0], ["checker", "statlilap"], status)
    status = runTest(jobNameField.name, [["c"], 0], ["checker", 'c'], status)
    status = runTest(jobNameField.name, [["w"], 0], ['w'], status)
    c = SecureCommSocket.SecureClient(TB_CTRL_PORT, mode.password)
    c.sendSecure(TB_KILL, [])
    endTest(status)
