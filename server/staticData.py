TB_CTRL_PORT = 7000
TB_CHECK = "C"
TB_KILL = "K"
TB_WAIT = "W"
TB_ADD  = "A"
TB_DEL  = "D"
TB_SHOW = "S"
TB_SAVE = "V"
TB_LOAD = "L"

DEBUG = 0

YAML_CHARS = [ chr(x) for x in range(ord(' '), ord('~')+1) ] + [ '\n' ]

# RESULT CODES
OK = 0
FAIL = 1
UNKNOWN = -1

ON = 1
OFF = 0

# PinshCmds
NO_MATCH  = 0
PARTIAL  = 1
COMPLETE = 2
INCOMPLETE = 3

# BOOLEANS
YES = 1
NO = 0
NEUTRAL = 2

# Authorization levels
USER = 0
ADMIN = 1

# MATCH TYPES
NO_MATCH  = 0
PARTIAL  = 1
COMPLETE = 2
