import PinshCmd
from bombardier_core.static_data import NO_MATCH, COMPLETE, PARTIAL

class MultipleChoice(PinshCmd.PinshCmd):
    def __init__(self, choices = [], help_text = []):
        PinshCmd.PinshCmd.__init__(self, "multiple")
        self.help_text = ""
        self.choices = choices
        self.level = 99
        if len(choices) != len(help_text):
            self.help_text = "error\t--This module needs attention--"
            return
        for index in range(0, len(choices)):
            self.help_text += choices[index]+'\t'+help_text[index]+'\n'
        self.help_text = self.help_text[:-1]
        self.cmd_owner = 0

    def match(self, tokens, index):
        if tokens[index] == '':
            return NO_MATCH, 1
        if tokens[index] in self.choices:
            return COMPLETE, 1
        partial_choices = []
        for choice in self.choices:
            if len(tokens[index]) < choice:
                partial_choices.append(choice[:len(tokens[index])])
        if tokens[index] in partial_choices:
            return PARTIAL, 1
        return NO_MATCH, 1

    def preferred_names(self, tokens, index):
        possible_choices = []
        if tokens[index] == '':
            return self.choices
        for choice in self.choices:
            if choice.find(tokens[index]) == 0:
                possible_choices.append(choice)
        return possible_choices

if __name__ == "__main__":
    from libTest import startTest, runTest, endTest
    multipleChoice = MultipleChoice(["gt", "lt", "eq", "ge", "le"], ["greater than", "less than", "equal to", "greater than or equal to", "less than or equal to"])
    status = startTest()
    status = runTest(multipleChoice.match, [[""], 0], (NO_MATCH, 1), status)
    status = runTest(multipleChoice.match, [["g"], 0], (PARTIAL, 1), status)
    status = runTest(multipleChoice.match, [["le"], 0], (COMPLETE, 1), status)
    status = runTest(multipleChoice.match, [["foofy"], 0], (NO_MATCH, 1), status)
    status = runTest(multipleChoice.preferred_names, [["l"], 0], ["lt", "le"], status)
    endTest(status)
