# BSD License
# Copyright (c) 2009, Peter Banka et al
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the name of the GE Security nor the names of its contributors may
#   be used to endorse or promote products derived from this software without
#   specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

'''A [PinshCmd] field which provides for integer matching'''

import PinshCmd
from bombardier_core.static_data import NO_MATCH, COMPLETE, PARTIAL

class Integer(PinshCmd.PinshCmd):
    'Any number between self.min_value and self.max_value'
    def __init__(self, min_value = 0, max_value = 100, name = "<integer>"):
        '''
        min_value -- the smallest number this field will match.
        max_value -- the largest number this field will match
        name -- usually '<integer>' is sufficient
        '''

        PinshCmd.PinshCmd.__init__(self, name)
        help_template = "%s\ta number between %s and %s"
        self.help_text = help_template % (name, min_value, max_value)
        self.min_value = min_value
        self.max_value = max_value
        self.cmd_owner = 0

    def match(self, tokens, index):
        '''determines if the number the user entered falls within limits'''
        if tokens[index] == '':
            return NO_MATCH, 1
        value = 0
        try:
            value = int(tokens[index])
            if value >= self.min_value and value <= self.max_value:
                return COMPLETE, 1
            else:
                return PARTIAL, 1
        except ValueError:
            return NO_MATCH, 1
        return NO_MATCH, 1

    def preferred_names(self, tokens, index):
        '''A wrapper for match'''
        if self.match(tokens, index)[0] == NO_MATCH:
            return []
        else:
            return [tokens[index]]

