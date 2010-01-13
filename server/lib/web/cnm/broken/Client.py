import Pyro.core
import time

long = Pyro.core.getProxyForURI("PYRONAME://longy")

def loop_test():
    for i in range(1,20):
        print i, "::", long.check_in()
        time.sleep(0.5)

#set password:
long.set_password("abc123")
print long.check_password()
loop_test()

