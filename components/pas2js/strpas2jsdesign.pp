unit strpas2jsdesign;

{$mode objfpc}{$H+}

interface

uses FileProcs, sysutils;

Resourcestring
  // "Create new" dialog
  pjsdWebApplication = 'Web Browser Application';
  pjsdWebAppDescription = 'A pas2js program running in the browser.';
  pjsdProgressiveWebApplication = 'Progressive Web Application';
  pjsdProgressiveWebAppDescription = 'A Web Application using a service worker, a manifest, and more to be installable.';
  pjsdServiceWorker = 'Pas2JS Service Worker';
  pjsdServiceWorkerDescription = 'A pas2js program running as cache for a web app. It does not run standalone.';
  pjsdNodeJSApplication = 'Node.js Application';
  pjsdNodeJSAppDescription = 'A pas2js program running in node.js.';
  pjsdModuleApplication = 'Pas2JS Library / JavaScript module';
  pjsdModuleAppDescription = 'A pas2js library that is transpiled to a JavaScript module.';
  pjsdNewAtomPackage = 'Atom package';
  pjsdNewAtomPackageDescr = 'A pas2js program running as Atom package.';
  pjsdNewVSCodeExtension = 'Visual Studio Code extension';
  pjsdNewVSCodeExtensionDescr = 'A pas2js program running as Visual Studio Code extension.';

  // menu item
  SPasJSWebserverCaption = 'Pas2JS WebServers';

  // Static texts webservers form
  SWebserversStatus  = 'Status';
  SWebserversPort    = 'Port';
  SWebserversBaseDir = 'Root directory';
  SWebserversProject = 'Project';
  SWebserversExtra   = 'Additional info';
  SWebserversCount   = 'Number of webserver processes: %s';
  SWebserversCaption = 'Web server processes';

  // Dynamic texts webservers form
  SStatusRunning = 'Running';
  SStatusStopped = 'Stopped';
  SStatusError   = 'Error starting';

  // IDE options frame
  pjsdSelectPas2jsExecutable = 'Select pas2js executable';
  pjsdSelectXExecutable = 'Select %s executable';
  pjsdSelectNodeJSExecutable = 'Select Node.js executable';
  pjsdSelectBrowserExecutable = 'Select browser executable';
  pjsdSelectAtomTemplateDir = 'Select Atom package template directory';
  pjsdSelectVSCodeTemplateDir = 'Select Visual Studio Code extension template directory';
  pjsdYouCanUseIDEMacrosLikeMakeExeWithoutAFullPathIsSea = 'You can use IDE '
    +'macros like $MakeExe(). Without a full path, %s is searched in PATH.';
  pjsdPathOfXMacroPas2js = 'Path of %s, macro $(pas2js)';
  pjsdBrowse = 'Browse';
  pjsdPathOfXMacroPas2JSWebServer = 'Path of %s, macro $(Pas2JSWebServer)';
  pjsdPortNumberToStartAllocatingFrom = 'Port number to start allocating '
    +'from, macro $(Pas2JSWebServerPort)';
  pjsdServerInstancesWillBeStartedWithAPortStartingFromT = 'Server instances '
    +'will be started with a port starting from this number, increasing per '
    +'new project';
  pjsdBrowserToOpenHTMLPage = 'Browser to open HTML page, macro $(Pas2JSBrowser)';
  pjsdUseThisBrowserWhenOpeningTheURLOrHTMLFileOfAWebBro = 'Use this browser '
    +'when opening the URL or HTML file of a web browser project';
  pjsdPathOfNodeJsExecutable = 'Path of Node.js executable, macro $(Pas2JSNodeJS)';
  pjsdHTTPServerOptsLabelCaption = 'HTTP Server extra command-line options (one per line)';
  pjsdHTTPServerOptsLabelHint = 'Add extra command-line options for the command which starts the webserver.';
  pjsdAtomPackageTemplateDirectory = 'Atom package template directory';
  pjsdVisualStudioCodeExtensionTemplateDirectory = 'Visual Studio Code '
    +'extension template directory';

  // Project options frame
  pjsdWebProjectPas2js = 'Web Project (pas2js)';
  pjsdProjectIsAWebBrowserPas2jsProject = 'Project is a Web Browser (pas2js) '
    +'project';
  pjsdProjectHTMLPage = 'Project HTML page:';
  pjsdMaintainHTMLPage = 'Maintain HTML page';
  pjsdUseBrowserConsoleUnitToDisplayWritelnOutput = 'Use Browser Console unit '
    +'to display writeln() output';
  pjsdRunRTLWhenAllPageResourcesAreFullyLoaded = 'Run RTL when all page '
    +'resources are fully loaded';
  pjsdProjectNeedsAHTTPServer = 'Project needs a HTTP server';
  pjsdStartHTTPServerOnPort = 'Start HTTP Server on port';
  pjsdUseThisURLToStartApplication = 'Use this URL to start application';
  pjsdResetRunCommand = 'Reset Run command';
  pjsdResetCompileCommand = 'Reset Compile command';

  // New browser project options form
  pjsdPas2JSBrowserProjectOptions = 'Pas2JS Browser project options';
  pjsdCreateInitialHTMLPage = 'Create initial HTML page';
  pjsdLetRTLShowUncaughtExceptions = 'Let RTL show uncaught exceptions';
  pjsdUseBrowserApplicationObject = 'Use Browser Application object';
  pjsdUseWASIApplicationObject = 'Run WebAssembly program:';
  pjsWasiProgramFileTextHint = 'Name of your WebAssembly file';

  // New NodeJS project options form
  pjsdNodeJSProjectOptions = 'NodeJS project options';
  pjsdUseNodeJSApplicationObject = 'Use NodeJS Application object';

  // New class definition from HTML File
  rsCreateClassFromHTMLName = 'Pas2JS class definition from HTML file';
  rsCreateClassFromHTMLDescription = 'Create a Pas2JS "form" class definition from HTML file using ID attributes in the HTML file.';

  rsCreateUnitFromTypeScript = 'Pas2JS import unit from TypeScript declaration module';
  rsCreateUnitFromTypeScriptDescription = 'Create a Pas2JS import unit from a TypeScript declaration module.';
  rsDTSDidNotProduceOutput = 'The DTS2pas tool did not produce an output file.';


  // Macros names
  pjsdPas2JSExecutable = 'Pas2JS executable';
  pjsdPas2JSWebServerExe = 'Pas2JS webserver executable';
  pjsdPas2JSWebServerPort = 'Pas2JS webserver port';
  pjsdPas2JSSelectedBrowserExecutable = 'Pas2JS selected browser executable';
  pjsdPas2JSSelectedNodeJSExcutable = 'Pas2JS selected NodeJS excutable';
  pjsdPas2JSCurrentProjectURL = 'Pas2JS current project URL';

  // Error descriptions
  pjsdMissingPathToPas2js = 'missing path to pas2js';
  pjsdFileNotFound = 'file "%s" not found';
  pjsdDirectoryNotFound = 'directory "%s" not found';
  pjsdFileNotExecutable = 'file "%s" not executable';
  pjsdFileNameDoesNotStartWithPas2js = 'filename does not start with "pas2js"';
  pjsdHTMLFilter = 'HTML Files|*.html|All files|*.*';
  pjsdHTMLSourceFileNotFound = 'HTML Source not found';
  pjsdHTMLFileNotFound = 'Cannot find the HTML Source file for the class in unit %s:'+LineEnding+
                         '%s'+LineEnding+
                         'Would you like to select the file from disk?';
  pjsdBtnSelectFile = 'Select the file';
  pjsdButtonCancel = 'Cancel refresh';

  pjsRefreshClassFromHTML = 'Refresh class from HTML source';
  pjsRefreshAllClassesFromHTML = 'Refresh all classes from HTML source';
  rsHTTPRequestFailed = 'HTML request to service URL %s failed: %s';

function SafeFormat(const Fmt: String; const Args: Array of const): String;

implementation

function SafeFormat(const Fmt: String; const Args: array of const): String;
begin
  // try with translated resourcestring
  try
    Result:=Format(Fmt,Args);
    exit;
  except
    on E: Exception do
      debugln(['ERROR: SafeFormat: ',E.Message]);
  end;
  // translation didn't work
  // ToDo: find out how to get the resourcestring default value
  //ResetResourceTables;

  // use a safe fallback
  Result:=SimpleFormat(Fmt,Args);
end;

end.

