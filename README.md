Bombardier
==========

Centralized, cross-platform package deployment, configuration and change
management.

Bombardier is a command-line system that allows system admins to manage a large
number of computers the datacenter over ssh. You can compare Bombardier to 
ControlTier (www.controltier.org) insofar as it is a push-based command-and-control type system, but with a more
secure architecture and a lot less XML. It also provides a better user
interface (i.e. command-line) that (we think) most system administrators will
prefer to web-based interfaces for daily work (which you are limited to with
the other products).

Compared to
["Promise-theory"](http://www.socallinuxexpo.org/scale11x/presentations/promise-theory-dummies)
systems such as Puppet, CFEngine, and Chef, Bombardier is a little more
old-school. It is more focused on delivering packages to remote systems and
configuring them in a top-down A-Z type way. This makes Bombardier packages
quite a bit easier to write than Puppet modules (since they are just Python),
but they are quite a bit less flexible than puppet modules which tend to "do
the right thing" no matter what the system configuration on the remote system.

For common tasks such as deploying software or making changes to system
configuration, this tool can make these tasks a lot faster and a lot more
reliable. The basic approach is to drive automated SSH sessionsover Expect.
There are three major pieces of code to this bombardier:

- A "client," which installs on the computers you want to manage. The client
  does not run as a service, so it is more accurately described as client-side
  libraries. The client software can be automatically installed by the server.

- The server, which runs under Apache and provides a ReST interface, and a
  daemon. The daemon is responsible for managing ssh connections to the
  computers in the network.

- A command-line interface (CLI), which provides an efficient Cisco-style
  command-line tool for driving the whole system.
