from SystemStateSingleton import SystemState
from bombardier_core.static_data import OK, FAIL, STRING_TO_RETURN_VALUE_MAPPING
import glob, sys, StringIO, yaml
import libUi
import re
import Slash
IGNORE_MODULE_NAMES = ['tester', 'libUi', 'Slash', 'SystemStateSingleton',
                       '_version']

def get_cls(module_name, module_to_test):
    setup_test = None
    if module_name in IGNORE_MODULE_NAMES:
        return None, None
    if modules_to_test:
        if not module_name in modules_to_test:
            return None, None
    module = __import__(module_name)
    if not hasattr(module, module_name):
        return None, None
    if hasattr(module, "setup_test"):
        setup_test = module.setup_test
    cls = getattr(module, module_name)
    return cls, setup_test

class TestCmd:
    def __init__(self, command, status, output_lines):
        self.command = command
        self.status = status
        self.output_lines = output_lines

def get_tests(cls, module_name):
    tests = []
    if module_name in IGNORE_MODULE_NAMES:
        return tests
    module = __import__(module_name)
    if not hasattr(module, module_name):
        return tests
    cls = getattr(module, module_name)
    doc_str = cls.__doc__
    if not doc_str:
        return tests
    command = ''
    for line in doc_str.split('\n'):
        if command:
            #result = re.compile("(\[(OK|FAIL), \[.*\]\])").findall(line)
            result = re.compile("(\[(OK|FAIL), .*\])").findall(line)
            if result:
                yaml_str = result[0][0]
                try:
                    parsed_test = yaml.load(yaml_str)
                    status = STRING_TO_RETURN_VALUE_MAPPING[parsed_test[0]]
                    output_lines = parsed_test[1]
                    tests.append(TestCmd(command, status, output_lines))
                except yaml.parser.ParserError:
                    print "Invalid result: ", line
                    break
            command = ''
        else:
            result = re.compile('bomsh\# (.*)').findall(line)
            if result:
                command = result[0]
    return tests

def compare_expected(expected, received):
    exp_list = [ x for x in expected.split(' ') if x != '' ]
    rec_list = [ x for x in received.split(' ') if x != '' ]
    if len(exp_list) != len(rec_list):
        return False
    for i in xrange(len(exp_list)):
        exp = exp_list[i]
        rec = rec_list[i]
        if exp == rec:
            continue
        elif exp == "==UNKNOWN==":
            continue
        else:
            return False
    return True

def run_tests(cls, tests, debug):
    object = cls()
    slash = Slash.Slash([object])

    output = {}
    for test_cmd in tests:
        command = test_cmd.command
        expected_status = test_cmd.status
        expected_cmd_output = test_cmd.output_lines
        if debug:
            print "TESTING: ", cls, command
            print "Expected result:", expected_status, expected_cmd_output
        #no_flag, help_flag, tokens, comment = libUi.process_input(command)
        cmd_status, cmd_output = slash.process_command(command.strip())
        if debug:
            print "COMMAND OUTPUT:",cmd_output
        if cmd_status != expected_status:
            print "STATUS FAIL: (%s) != (%s)" % (cmd_status, expected_status)
            output[command] = "FAIL"
            continue
        if len(cmd_output) != len(expected_cmd_output):
            output[command] = "FAIL"
            print "LENGTH FAIL: len(%s) != len(%s)" % (cmd_output, expected_cmd_output)
            continue
        if type(cmd_output) == type(['list']):
            for index in range(0,len(cmd_output)):
                received_user_line = cmd_output[index].strip()
                expected_user_line = expected_cmd_output[index].strip()
                if not compare_expected(expected_user_line, received_user_line):
                    print "FAILED: (%s) != (%s)" % (expected_user_line, received_user_line)
                    output[command] = "FAIL"
        output[command] = "PASSED"
    return output

if __name__ == "__main__":
    import optparse

    parser = optparse.OptionParser("usage: %prog [-d] <Test Module>")
    parser.add_option("-d", "--debug", dest="debug", action="store_const",
                      const=True, help="Turn on debugging")
    parser.add_option("-s", "--server_home", dest="server_home", metavar = "HOME",
                      help="Set server home")

    (options, modules_to_test) = parser.parse_args()
    debug = False
    if options.debug:
        debug = True

    system_state = SystemState()
    system_state.load_config()
    libUi.login("admin", 'abc123')

    output = system_state.cnm_connector.dispatcher_control("start")
    if not debug:
        print "(suppressing output)"
        output_handle = StringIO.StringIO()
        system_state.set_output(output_handle)
    else:
        print "OUTPUT FROM DISPATCHER START", output
        print "(clearing existing connections)"
    system_state.cnm_connector.cleanup_connections()

    if options.server_home:
        system_state.cnm_connector.sync_server_home(options.server_home)

    file_names = glob.glob("*.py")
    module_names = [ x.split('.')[0] for x in file_names ]
    module_names = [ n for n in module_names if n not in IGNORE_MODULE_NAMES ]

    for module_name in module_names:
        cls, setup_test = get_cls(module_name, modules_to_test)
        if not cls:
            continue
        if setup_test:
            setup_test()
        tests = get_tests(cls, module_name)
        if not tests:
            continue

        output = run_tests(cls, tests, debug)
        for command in output:
            print "%s:  '%s' (%s)" % (output[command], command, module_name)
