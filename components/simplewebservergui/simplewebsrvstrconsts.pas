unit SimpleWebSrvStrConsts;

{$mode objfpc}{$H+}

interface

resourcestring
  rsSWSTitle = 'Simple Web Server';
  rsSWSPathOfCompileserver = 'Path of compileserver';
  rsSWSAddress = 'Address';
  rsSWSPort = 'Port';
  rsSWSBindAny = 'Bind Any';
  rsSWSUserOrigin = 'User';
  rsSWError = 'Error';
  rsSWErrorWriting = 'Error writing "%s"';
  rsSWErrorCreatingDirectory = 'Error creating directory';
  rsSWDirectoryNotFound = 'Directory not found';
  rsSWSimpleWebServerAddress = 'Simple Web Server Address';
  rsSWSimpleWebServerPort = 'Simple Web Server Port';
  rsSWSimpleWebServerExecutable = 'Simple Web Server Executable';
  rsSWWrongCompileserverExe = 'Wrong compileserver exe: %s';
  rsSWServerExeIsNotExecutable = 'Server exe is not executable: "%s"';
  rsSWFileNotFound = 'File not found: "%s"';
  rsSWServerExecutableNotFoundInPATH = 'Server executable "%s" not found in '
    +'PATH.';
  rsSWServerDirectoryNotFound = 'Server directory "%s" not found.';
  rsSWBindingOfSocketFailed = 'Binding of socket failed';
  rsSWTheFollowingProcessAlreadyListens = 'The following process already '
    +'listens:';
  rsSWKillProcess = 'Kill process?';
  rsSWKillPID = 'Kill PID %s';
  rsSWTryAnotherPort = 'Try another port';
  rsSWMissingServerExecutable = 'Missing server executable';
  rsSWMissingLocalDirectory = 'Missing local directory';

implementation

end.

