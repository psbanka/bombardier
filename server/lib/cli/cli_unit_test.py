#!/usr/bin/env python

import unittest, StringIO, sets, yaml, sys, os



import pexpect, sys, re
PASSWORD = "abc123"
USERNAME = "TESTY"
DEBUG = True
DEBUG = False
PROMPT_CHARACTER = '\\$'
PROMPT = '\(%s\)%s' % (USERNAME, PROMPT_CHARACTER)

class CliUnitTest(unittest.TestCase):

    def setUp(self):
        self.s = pexpect.spawn("bomsh -n -u %s" % USERNAME, timeout=5000)
        if DEBUG:
            self.s.logfile = sys.stdout
        #expected_values = [pexpect.TIMEOUT, '[pP]assword: ']
        #i = self.s.expect(expected_values, timeout=30)
        #assert i == 1, "Could not start the bomsh"
        #print self.s.before
        #self.s.sendline(PASSWORD)
        #self.s.sendline('\n\r')
        self.get_prompt()

    def tearDown(self):
        output = self.run_command("\b" * 20)
        #output = self.run_command("\n\r")
        output = self.run_command("exit\n\r")
        #output = self.run_command("n\n\r")
        self.s.close(True)

    def get_prompt(self):
        expected_values = [pexpect.TIMEOUT, PROMPT ]
        i = self.s.expect(expected_values, timeout=30)
        assert i == 1, "Could not get a PROMPT"

    def read(self):
        output = []
        data = 'initialize'
        while True:
            try:
                data = self.s.read_nonblocking(timeout=0.25)
            except pexpect.EOF:
                break
            except pexpect.TIMEOUT:
                break
            output.append(data)
        return output

    def run_command(self, cmd):
        self.s.sendline(cmd+'\n')
        output = self.read()
        lines = ''.join(output).split('\r\n')
        command_output = []
        for line in lines:
            if re.compile(PROMPT).findall(line):
                continue
            if line:
                command_output.append( line.strip() )
        return command_output

    def tab_completion(self, cmd):
        self.s.send(cmd+'\t')
        output = self.read()
        line = ''.join(output)
        if re.compile(PROMPT).findall(line):
            line = line.split('>')[1]
        #print "LINE: (%s)" % line
        return line

    def double_tab_completion(self, cmd):
        self.s.send(cmd+'\t\t')
        output = self.read()
        lines = ''.join(output).split('\r\n')
        return lines[1].strip().split()

class CliHelpTest(CliUnitTest):

    def test_cli_help(self):
        command = "cli ?"
        expected_output = [
            'cli ?',
            'spammy      THIS IS A TEST',
            'spammy2     THIS IS A TEST',
            'spotty      THIS IS A TEST',
        ]
        output = self.run_command(command)
        assert output == expected_output, "(%s) != (%s)" % (output, expected_output)

    def test_slash_help(self):
        command = "?"
        expected_output = [
             '?',
             'cli            do not use this',
             'dispatcher     dispatcher control commands and status',
             'edit           modify components of the system', 
             'exit           exit current mode',
             'machine        commands that operate on a given machine',
             'set            set a configuration value',
             'show           display components of the system',
             'ssh            ssh to another host',
             'terminal       change settings for the current terminal']
        output = self.run_command(command)
        assert output == expected_output, "(%s) != (%s)" % (output, expected_output)

class CliCmdTest(CliUnitTest):

    def test_ambigous_command(self):
        command = "cli sp 22"
        err = '%  Command "sp" ambiguous -- could be [\'spammy\', \'spammy2\', \'spotty\'].'
        expected_output = [command, err]

        output = self.run_command(command)
        assert output == expected_output, "(%s) != (%s)" % (output, expected_output)

    def test_complete_but_ambigous_command(self):
        command = "cli spammy 22"
        expected_output = [command, "cli", "spammy", "22"]
        output = self.run_command(command)
        assert output == expected_output, "(%s) != (%s)" % (output, expected_output)

    def test_shortened_command(self):
        command = "cl spammy2 22"
        expected_output = [command, "cli", "spammy2", "22"]
        output = self.run_command(command)
        assert output == expected_output, "(%s) != (%s)" % (output, expected_output)

    def test_simple_command(self):
        command = "cli spammy2 22"
        expected_output = [command, "cli", "spammy2", "22"]
        output = self.run_command(command)
        assert output == expected_output, "(%s) != (%s)" % (output, expected_output)

class Scratchy(CliUnitTest):
    def test_shortened_command2(self):
        command = 'cl spotty A'
        expected_output = [command, "cli", "spotty", "AA"]
        output = self.run_command(command)
        assert output == expected_output, "(%s) != (%s)" % (output, expected_output)

class CliCompletionTest(CliUnitTest):

    def test_simple_completion(self):
        output = self.tab_completion("cl")
        expected_output = "cli "
        assert output == expected_output, "(%s) != (%s)" % (output, expected_output)

    def test_complex_completion(self):
        output = self.double_tab_completion("cli sp")
        expected_output = ["spammy", "spammy2", "spotty"]
        assert output == expected_output, "(%s) != (%s)" % (output, expected_output)

    def test_complex_completion2(self):
        output = self.double_tab_completion("cli spo ")
        expected_output = ["AA", "BB", "CC"]
        assert output == expected_output, "(%s) != (%s)" % (output, expected_output)

if __name__ == "__main__":
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(CliCmdTest))
    suite.addTest(unittest.makeSuite(Scratchy))
    suite.addTest(unittest.makeSuite(CliCompletionTest))
    suite.addTest(unittest.makeSuite(CliHelpTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
