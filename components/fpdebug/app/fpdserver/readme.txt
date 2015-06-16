FPDebug-Server

The FPDebug-server contains a single fpdebug-thread to be able to debug one application at a time.

Multiple listeners can attach to this debug-thread so that they will receive messages from the fpdebug-thread about
the application being debugged.
The listeners can also send commands to the debug-thread, all listeners will receive information about all comands.

By default there are two listeners, one to communicate with the console. That way the debugger can be controlled from
the console. The other listener is setting up a tcp-server listening on port 9159. It's possible to connect to this
port using telnet to control the debugging.

In- and output are in a a json-format, but it is possible to register multiple formats.

The FPDebug-server could be used as a stand-alone debugger, controlled using the console. Or it could be used by other
front-ends using the tcp-listener. Multiple connections can be used to monitor what is happening.

The goal is to add a Lazarus as a front end. It can be used for remote-debugging, and it can be used to avoid the
necessity to sign the Lazarus-executable to be able to debug on OS/X. (Only the FPDebug-server has to be signed)

Joost van der Sluis, may 2015.

