unit SimpleWebSrvStrConsts;

{$mode objfpc}{$H+}

interface

resourcestring
  rsSWSTitle = 'Simple Web Server';
  rsSWSPathOfCompileserver = 'Path of compileserver, macro $(SWSExe)';
  rsSWSAddress = 'Address';
  rsSWSPortMacro = 'Port, macro $(SWSPort)';
  rsSWTCPPort102465535YouCanUseMacroPortForBelowParams = 'TCP Port 1024..65535'
    +' (you can use macro $(port) for below params)';
  rsSWPort = 'Port';
  rsSWCustomServer = 'Custom Server';
  rsSWExecutable = 'Executable';
  rsSWWorkingDirectory = 'Working Directory';
  rsSWOrigin = 'Origin';
  rsSWNote = 'Note';
  rsSWCopyLocation = 'Copy Location';
  rsSWCopyURL = 'Copy URL';
  rsSWCopyWorkingDirectoryPath = 'Copy Working Directory Path';
  rsSWCopyOrigin = 'Copy Origin';
  rsSWParametersPleaseUseMacroPort = 'Parameters (please use macro $(port))';
  rsSWCommandLineParameters = 'Command line parameters';
  rsSWAddCustomServer = 'Add Custom Server';
  rsSWSelectDirectory = 'Select Directory';
  rsSWServerExtraCommandLineOptionsOnePerLine = 'Server extra command-line '
    +'options (one per line)';
  rsSWBrowserToOpenHTMLPageMacroSWSBrowser = 'Browser to open HTML page, macro'
    +' $(SWSBrowser)';
  rsSWAddExtraCommandLineOptionsForTheCommandWhichStarts = 'Add extra command-'
    +'line options for the command which starts the webserver.';
  rsSWUseThisBrowserWhenOpeningTheURLOrHTMLFileOfAWebBro = 'Use this browser '
    +'when opening the URL or HTML file of a web browser project';
  rsSWSBindAny = 'Bind Any';
  rsSWSUserOrigin = 'User';
  rsSWUnableToAddLocation = 'Unable to add location:';
  rsSWMissingController = 'missing controller';
  rsSWMissingLocation = 'Missing Location.';
  rsSWAddSimpleWebServerLocation = 'Add Simple Web Server Location';
  rsSWLocation = 'Location';
  rsSWAnArbitraryNameForTheURLSubfolder = 'An arbitrary name for the URL '
    +'subfolder';
  rsSWLocalDirectory = 'Local Directory';
  rsSWWorkingDirectoryOnDiskUsuallyWhereTheServerFetches = 'Working directory '
    +'on disk, usually where the server fetches files from';
  rsSWAddLocation = 'Add Location';
  rsSWError = 'Error';
  rsSWThereIsAlreadyAServerOnPortOriginPath = 'There is already a server on '
    +'port %s:%sOrigin: %sPath: %s';
  rsSWErrorFindingFreeTCPPort = 'Error finding free TCP port:';
  rsSWErrorKillingProcess = 'Error killing process %s:';
  rsSWErrorCheckingWhichProcessUsesTCPPort = 'Error checking which process '
    +'uses TCP port %s:';
  rsSWCannotFindBrowserSee = 'Cannot find browser. See';
  rsSWInvalidMacroSee = 'Invalid macro. See';
  rsSWToolsOptionsSimpleWebServerBrowser = 'Tools / Options / Simple Web '
    +'Server / Browser';
  rsSWServerExecutableNotFound = 'Server executable not found.';
  rsSWServerExeIsNotExecutable2 = 'Server exe is not executable.';
  rsSWInvalidPort = 'Invalid Port.';
  rsSWPortAlreadyUsed = 'Port already used.';
  rsSWLocalDirectoryNotFound = 'Local directory not found:';
  rsSWLocationAlreadyUsed = 'Location already used.';
  rsSWErrorWriting = 'Error writing "%s":';
  rsSWErrorCreatingDirectory = 'Error creating directory';
  rsSWDirectoryNotFound = 'Directory not found.';
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
  rsSWMissingServerExecutable = 'Missing server executable.';
  rsSWServerExecutableNotFoundInPATH2 = 'Server executable not found in PATH.';
  rsSWMissingLocalDirectory = 'Missing local directory.';
  rsSWStartServer = 'Start Server';
  rsSWStopServer = 'Stop Server';
  rsSWDelete = 'Delete?';
  rsSWDeleteServerAt = 'Delete server at "%s"?';
  rsSWConfigure = 'Configure';
  rsSWAdd = 'Add';
  rsSWDelete2 = 'Delete';
  rsSWDeleteLocation = 'Delete location "%s"?';

implementation

end.

