
The debug server is a simple program that listens for debug messages,
and displays them in a list. The messages can be saved, cleared, it can
be paused - whatever.

It is the server part to a standard FPC unit - dbugintf. This unit
offers a simple API to send messages to a debug server (using
simpleIPC), modeled after the GExperts GDebug tool for Delphi, with
some minor enhancements.

Typical usage is as follows (I stripped actual code and {$ifdef debug}):

uses dbugintf,sysutils;

Procedure BackupFile(FN : String);

Var
   BFN : String;

begin
   SendMethodEnter('BackupFile');
   BFN:=FN+'.bak';
   SendDebug(Format('backup file "%s" exists, deleting',[BFN]));
   SendDebug(Format('Backing up "%s" to "%s"',[FN,BFN]));
   SendMethodExit('BackupFile');
end;

Procedure SaveToFile(FN : String);

begin
   SendMethodEnter('SaveToFile');
   BackupFile(FN);
   SendDebug('Saving to file '+FN);
   SendMethodExit('SaveToFile');
end;

There are some more methods as displayed here.

Output would look as in the attached screenshot. It is extremely useful
when debugging GUI code with lots of events - because you see the
messages as they are sent, in a separate window which can be kept 'on
top'. We also use it to debug our server applications.  The indentation
of the messages (by SendMethodEnter) is intentional: if an exception
occurs, then the SendMethodExit does not happen, and you see that
something is wrong visually.

