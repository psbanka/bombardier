#!/usr/bin/python

import sys

import PinshCmd
from commonUtil import *

class MultipleChoice(PinshCmd.PinshCmd):
    def __init__(self, choices = [], helpText = []):
        PinshCmd.PinshCmd.__init__(self, "multiple")
        self.helpText = ""
        self.choices = choices
        self.level = 99
        if len(choices) != len(helpText):
            self.helpText = "error\t--This module needs attention--"
            return
        for index in range(0, len(choices)):
            self.helpText += choices[index]+'\t'+helpText[index]+'\n'
        self.helpText = self.helpText[:-1]
        self.cmdOwner = 0

    def match(self, tokens, index):
        if tokens[index] == '':
            return NO_MATCH, 1
        if tokens[index] in self.choices:
            return COMPLETE, 1
        partialChoices = []
        for choice in self.choices:
            if len(tokens[index]) < choice:
                partialChoices.append(choice[:len(tokens[index])])
        if tokens[index] in partialChoices:
            return PARTIAL, 1
        return NO_MATCH, 1

    def name(self, tokens, index):
        possibleChoices = []
        if tokens[index] == '':
            return self.choices
        for choice in self.choices:
            if choice.find(tokens[index]) == 0:
                possibleChoices.append(choice)
        return possibleChoices

if __name__ == "__main__":
    from libTest import startTest, runTest, endTest
    multipleChoice = MultipleChoice(["gt", "lt", "eq", "ge", "le"], ["greater than", "less than", "equal to", "greater than or equal to", "less than or equal to"])
    status = startTest()
    status = runTest(multipleChoice.match, [[""], 0], (NO_MATCH, 1), status)
    status = runTest(multipleChoice.match, [["g"], 0], (PARTIAL, 1), status)
    status = runTest(multipleChoice.match, [["le"], 0], (COMPLETE, 1), status)
    status = runTest(multipleChoice.match, [["foofy"], 0], (NO_MATCH, 1), status)
    status = runTest(multipleChoice.name, [["l"], 0], ["lt", "le"], status)
    endTest(status)
