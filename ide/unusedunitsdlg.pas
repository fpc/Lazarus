{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    A dialog showing the unused units of the current unit
    (at cursor in source editor).
    With the ability to remove them automatically.
}
unit UnusedUnitsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc,FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, ButtonPanel, ComCtrls,
  SrcEditorIntf, LazIDEIntf,
  CodeCache, CodeToolManager,
  LazarusIDEStrConsts;

type

  { TUnusedUnitsDialog }

  TUnusedUnitsDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    UnitsTreeView: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure OkClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  UnusedUnitsDialog: TUnusedUnitsDialog;

function ShowUnusedUnitsDialog: TModalResult;

implementation

function ShowUnusedUnitsDialog: TModalResult;
var
  SrcEdit: TSourceEditorInterface;
  Code: TCodeBuffer;
  Units: TStringList;
begin
  Result:=mrOk;
  if not LazarusIDE.BeginCodeTools then exit;

  // get cursor position
  SrcEdit:=SourceEditorWindow.ActiveEditor;
  if SrcEdit=nil then exit;
  Code:=TCodeBuffer(SrcEdit.CodeToolsBuffer);
  if Code=nil then exit;

  Units:=TStringList.Create;
  try
    if not CodeToolBoss.FindUnusedUnits(Code,Units) then begin
      DebugLn(['ShowUnusedUnitsDialog CodeToolBoss.FindUnusedUnits failed']);
      LazarusIDE.DoJumpToCodeToolBossError;
      exit(mrCancel);
    end;


  finally
    Units.Free;
  end;
end;

{ TUnusedUnitsDialog }

procedure TUnusedUnitsDialog.FormCreate(Sender: TObject);
begin
  Caption:='Unused units';

  ButtonPanel1.OKButton.Caption:='Remove selected units';
  ButtonPanel1.OKButton.OnClick:=@OkClick;
  ButtonPanel1.CancelButton.Caption:='Cancel';
end;

procedure TUnusedUnitsDialog.OkClick(Sender: TObject);
begin

end;

initialization
  {$I unusedunitsdlg.lrs}

end.

