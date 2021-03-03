unit LazCHMHelpRegister;

{$mode objfpc}{$H+}

{ Registers Lazarus CHM Help menu shortcuts into the IDE }
{ This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}

interface

uses
  { rtl }
  SysUtils, Classes,
  { lcl }
  LCLType, FileUtil, LResources,
  PropEdits, Controls;

procedure Register;

implementation

uses
  { lazarus }
  LazIDEIntf, MenuIntf, IdeCommands,
  { local }
  LazCHMHelp;

const
  HELP_CURRENT_NAME  = 'chpHelp';
  HELP_CATEGORY_IDECMD_NAME = 'chpFormat';

// Register package
procedure Register;
var
  Cat: TIDECommandCategory;
  {
  Key: TIDEShortCut;
  }
  CmdHelpCommand: TIDECommand;
  AChmViewer: TChmHelpViewer;
begin
  // We can't put this in an initialization section because IDEChmHelp requires
  // some IDE features, which are only available in "Register".
  AChmViewer := ChmViewerInstance();
  Cat := IDECommandList.CreateCategory(nil, HELP_CATEGORY_IDECMD_NAME,
    HELP_CATEGORY_IDECMD, IDECmdScopeSrcEditOnly);
  {
  // Assign F1 key
  Key := IDEShortCut(VK_F1, [], VK_UNKNOWN, []);
  CmdHelpCommand := RegisterIDECommand(Cat, HELP_CURRENT_NAME, HELP_CURRENT_IDECMD, Key,
    @IDECHMHelp.ShowAllHelp);
  }
  CmdHelpCommand := RegisterIDECommand(Cat, HELP_CURRENT_NAME, HELP_CURRENT_IDECMD,
    @AChmViewer.ShowAllHelp);
  RegisterIDEMenuCommand(mnuHelp, HELP_CURRENT_NAME, HELP_CURRENT_MENU,
    @AChmViewer.ShowAllHelp, nil, CmdHelpCommand);
end;

end.
