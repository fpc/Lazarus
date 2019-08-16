unit strpas2jsdesign;

{$mode objfpc}{$H+}

interface

Resourcestring
  // "Create new" dialog
  pjsdWebApplication = 'Web Browser Application';
  pjsdWebAppDescription = 'A pas2js program running in the browser';
  pjsdNodeJSApplication = 'Node.js Application';
  pjsdNodeJSAppDescription = 'A pas2js program running in node.js';

  // menu item
  SPasJSWebserversCaption = 'Pas2JS WebServers';

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

  // Options frame
  pjsdSelectPas2jsExecutable = 'Select pas2js executable';
  pjsdSelectSimpleserverExecutable = 'Select simpleserver executable';
  pjsdSelectNodeJSExecutable = 'Select Node.js executable';
  pjsdSelectBrowserExecutable = 'Select browser executable';
  pjsdPathOf = 'Path of %s';
  pjsdYouCanUseIDEMacrosLikeMakeExeWithoutAFullPathIsSea = 'You can use IDE '
    +'macros like $MakeExe(). Without a full path, %s is searched in PATH.';
  pjsdBrowse = 'Browse';
  pjsdPortNumbersToStartAllocatingFrom = 'Port numbers to start allocating '
    +'from %s';
  pjsdServerInstancesWillBeStartedWithAPortStartingFromT = 'Server instances '
    +'will be started with a port starting from this number, increasing per '
    +'new project';
  pjsdBrowserToUseWhenOpeningHTMLPage = 'Browser to use when opening HTML page';
  pjsdUseThisBrowserWhenOpeningTheURLOrHTMLFileOfAWebBro = 'Use this browser '
    +'when opening the URL or HTML file of a web browser project';
  pjsdPathOfNodeJsExecutable = 'Path of Node.js executable';


implementation

end.

