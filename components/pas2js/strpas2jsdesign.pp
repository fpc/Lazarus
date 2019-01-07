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



implementation

end.

