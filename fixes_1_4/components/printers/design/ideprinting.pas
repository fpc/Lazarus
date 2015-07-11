{ Copyright (C) 2006 Darius Blaszijk

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit ideprinting; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLtype;

procedure PrintFile(Sender: TObject);
procedure Register;

implementation

uses
  MenuIntf, IDECommands, Controls, Forms, Dialogs, SrcEditorIntf, SourcePrinter;

resourcestring
  SDescrFormatting  = 'Formatting';
  SDescrPFSelection = 'Print ...';
  SPFSelectionCmd   = 'Print';
  SPrintSources     = 'Print sources';
  SPrintWhat        = 'What would you like to print ?';
  SPrintFile        = 'Complete file';
  SPrintSelection   = 'Selected text';


var
  CmdFormatSelection : TIDECommand;

procedure Register;
var
  Key : TIDEShortCut;
  Cat : TIDECommandCategory;
begin
  Key:=IDEShortCut(VK_P,[SSctrl],VK_UNKNOWN,[]);
{$ifndef USECustomCategory}
  Cat:=IDECommandList.CreateCategory(nil,
                                    'PrintFormatting',
                                    SDescrFormatting,
                                    IDECmdScopeSrcEditOnly);
{$else}
  cat:=nil;
{$endif}
  CmdFormatSelection:=RegisterIDECommand(Cat,
                                         'PrintSelection',
                                         SPFSelectionCmd,
                                         Key,nil,@PrintFile);

  //file main menu item
  RegisterIDEMenuCommand(itmFileDirectories,
                         'PrintSelection',
                         SDescrPFSelection, 
                         nil,nil,CmdFormatSelection, 'menu_file_print');

  //source editor popup
  RegisterIDEMenuCommand(SrcEditMenuSectionPages,
                         'PrintSelection',
                         SDescrPFSelection, 
                         nil,nil,CmdFormatSelection, 'menu_file_print');
end;

procedure PrintFile(Sender: TObject);

var
  sp: TSourcePrinter;
  L : TStrings;
  R : TModalResult;

begin
  If Not Assigned(SourceEditorManagerIntf) or Not Assigned(SourceEditorManagerIntf.ActiveEditor) then
    Exit;
  If (SourceEditorManagerIntf.ActiveEditor.Selection='') then
    R:=mrYesToAll
  else
    R:=QuestionDlg(SPrintSources,SPrintWhat,mtInformation,[mrYesToAll,SPrintFile,mrYes,SPrintSelection,mrCancel],0);
  If R=mrCancel then
    exit;
  L:=TStringList.Create;
  try
    case R of
      mrYesToAll : L.Assign(SourceEditorManagerIntf.ActiveEditor.Lines);
      mrYes      : L.Text:=SourceEditorManagerIntf.ActiveEditor.Selection;
    end;
    sp := TSourcePrinter.Create;
    try
      sp.Execute(L);
    finally
      sp.Free;
    end;
  finally
    L.Free;
  end;
end;

end.

