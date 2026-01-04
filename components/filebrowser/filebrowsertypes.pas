unit FileBrowserTypes;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TRootDir = (rdProjectDir, rdSystemRootDir, rdUserDir, rdCustomDir);

Const
  DefaultRootDir = rdProjectDir;
  DefaultSplitterPos = 150;
  SConfigFile         = 'idebrowserwin.xml';
  KeyRootDir          = 'RootDir';
  KeyCustomRootDir    = 'CustomRootDir';
  KeySplitterPos      = 'SplitterPos';
  KeyRememberSelDir   = 'RememberSelectedDir';
  KeySyncCurrentEditor= 'SyncCurrentEditor';

resourcestring
  SFileBrowserIDEMenuCaption = 'File Browser';
  rsConfigure = 'Configure';
  rsReload = 'Reload';
  rsShowHidden = 'Show hidden files';

implementation

end.

