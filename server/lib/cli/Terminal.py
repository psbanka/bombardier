import PinshCmd, Integer
import MultipleChoice
from bombardier_core.static_data import NO_COLOR, LIGHT, DARK, OK, FAIL, COMPLETE
from bombardier_server.cli.SystemStateSingleton import SystemState, ENABLE, USER, F0
system_state = SystemState()

class Terminal(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "terminal")
        self.help_text = "terminal\tchange settings for the current terminal"
        self.length = PinshCmd.PinshCmd("length", "length\tnumber of lines the terminal can display")
        self.width  = PinshCmd.PinshCmd("width", "width\tnumber of columns the terminal can disply")
        self.color  = PinshCmd.PinshCmd("color", "color\tset terminal color")
        help_text = ["black and white", "colors for light background", "colors for dark background"]
        color_options = MultipleChoice.MultipleChoice(choices = ["none", "light", "dark"],
                                                     help_text = help_text)
        self.children = [self.length, self.width, self.color]
        self.num = Integer.Integer(min = 0, max = 1000, name = "<int>")
        self.length.children = [self.num]
        self.width.children = [self.num]
        self.color.children = [color_options]
        self.level = 0
        self.cmd_owner = 1

    def cmd(self, tokens, no_flag, slash):
        if no_flag:
            if tokens[1].lower().startswith('c'):
                system_state.termcolor = NO_COLOR
                return OK, ["Terminal set to black and white."]
            else:
                return FAIL, ["'No' not applicable in this context"]
        if len(tokens) < 3:
            msg  = ["terminal length: %d" % system_state.termlen]
            msg += ["terminal width: %d" % system_state.termwidth]
            if system_state.termcolor != NO_COLOR:
                if system_state.termcolor == LIGHT:
                    msg += ["terminal color-scheme: light"]
                if system_state.termcolor == DARK:
                    msg += ["terminal color-scheme: dark"]
            return OK, msg
        value = tokens[2]
        if tokens[1].lower().startswith('c'):
            if value.lower().startswith('n'):
                system_state.termcolor = NO_COLOR
                return OK, ["Terminal set to black and white."]
            elif value.lower().startswith('l'):
                system_state.termcolor = LIGHT
                return OK, ["Terminal set to light background."]
            elif value.lower().startswith('d'):
                system_state.termcolor = DARK
                return OK, ["Terminal set to dark background."]
            else:
                return FAIL, ["Unknown terminal color."]

        if self.num.match([value], 0) != (COMPLETE, 1):
            return FAIL, ["Please choose a value between 0 and 1000"]

        if tokens[1].lower().startswith('l'):
            system_state.termlen = int(value)
        elif tokens[1].lower().startswith('w'):
            system_state.termwidth = int(value)
        return OK, []
