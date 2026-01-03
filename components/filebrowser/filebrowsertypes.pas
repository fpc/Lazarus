unit FileBrowserTypes;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TStartDir = (sdProjectDir, sdLastOpened, sdCustomDir);
  TRootDir = (rdProjectDir, rdUserDir, rdRootDir, rdCustomDir);

Const
  DefaultStartDir = sdProjectDir;
  DefaultRootDir = sdProjectDir;
  DefaultSyncCurrentEditor = False;
  DefaultSplitterPos = 150;

  SConfigFile         = 'idebrowserwin.xml';
  KeyStartDir         = 'StartDir';
  KeyRootDir          = 'RootDir';
  KeyCustomStartDir   = 'CustomDir';
  KeyCustomRootDir    = 'CustomRootDir';
  KeySplitterPos      = 'SplitterPos';
  KeySyncCurrentEditor= 'SyncCurrentEditor';

resourcestring
  SFileBrowserIDEMenuCaption = 'File Browser';

implementation

end.

