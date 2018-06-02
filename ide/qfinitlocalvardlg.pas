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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

 Author: Mattias Gaertner

 Abstract:
   Dialog used by the quick fix "Insert Assignment var := ..." for messages
   Hint/Warning: (5036) Local variable "$1" does not seem to be initialized
   Dialog shows a list of possible statements and a list of possible insert
   positions.
}
unit QFInitLocalVarDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, contnrs,
  // LCL
  Forms, Controls, Dialogs, ButtonPanel, ExtCtrls,
  // LazUtils
  LazLoggerBase,
  // Codetools
  CodeToolManager, CodeCache, StdCodeTools,
  // IdeIntf
  LazIDEIntf, IDEDialogs,
  // IDE
  LazarusIDEStrConsts;

type

  { TQFInitLocalVarDialog }

  TQFInitLocalVarDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    Panel1: TPanel;
    ValueRadioGroup: TRadioGroup;
    WhereRadioGroup: TRadioGroup;
    procedure ButtonPanel1OKButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    Statements: TStrings;
    InsertPositions: TObjectList;
    procedure Init(TheStatements: TStrings; TheInsertPositions: TObjectList);
  end;

function QuickFixLocalVarNotInitialized(Code: TCodeBuffer; X, Y: integer): boolean;

implementation

function QuickFixLocalVarNotInitialized(Code: TCodeBuffer; X, Y: integer): boolean;
var
  Statements: TStrings;
  InsertPositions: TObjectList;
  Dlg: TQFInitLocalVarDialog;
begin
  Result:=false;
  Statements:=nil;
  InsertPositions:=nil;
  try
    if not CodeToolBoss.GetPossibleInitsForVariable(Code,X,Y,Statements,
      InsertPositions)
    then begin
      if CodeToolBoss.ErrorCode<>nil then
        LazarusIDE.DoJumpToCodeToolBossError
      else
        ShowMessage('CodeToolBoss.GetPossibleInitsForVariable failed at '+Code.Filename+'('+IntToStr(Y)+','+IntToStr(X)+')');
      exit;
    end;

    Dlg:=TQFInitLocalVarDialog.Create(nil);
    try
      Dlg.Init(Statements,InsertPositions);
      case Dlg.ShowModal of
      mrOk: Result:=true;
      mrAbort:
        if CodeToolBoss.ErrorCode<>nil then
          LazarusIDE.DoJumpToCodeToolBossError
        else
          IDEMessageDialog('Error','Unable to insert the code',mtError,[mbOk]);
      else
        // user cancel
      end;
    finally
      Dlg.Free;
    end;
  finally
    Statements.Free;
    InsertPositions.Free;
  end;
end;

{$R *.lfm}

{ TQFInitLocalVarDialog }

procedure TQFInitLocalVarDialog.FormCreate(Sender: TObject);
begin
  Caption:=lisInitializeLocalVariable;
  ButtonPanel1.OKButton.OnClick:=@ButtonPanel1OKButtonClick;
end;

procedure TQFInitLocalVarDialog.ButtonPanel1OKButtonClick(Sender: TObject);
var
  OldChange: Boolean;
begin
  OldChange:=LazarusIDE.OpenEditorsOnCodeToolChange;
  LazarusIDE.OpenEditorsOnCodeToolChange:=true;
  try
    if CodeToolBoss.InsertStatements(
      TInsertStatementPosDescription(InsertPositions[Max(0,WhereRadioGroup.ItemIndex)]),
      Statements[Max(0,ValueRadioGroup.ItemIndex)])
    then begin
      ModalResult:=mrOk;
    end else begin
      ModalResult:=mrAbort;
    end;
  finally
    LazarusIDE.OpenEditorsOnCodeToolChange:=OldChange;
  end;
end;

procedure TQFInitLocalVarDialog.Init(TheStatements: TStrings;
  TheInsertPositions: TObjectList);
var
  i: Integer;
  InsertPos: TInsertStatementPosDescription;
  sl: TStringList;
  s: String;
  CodeXY: TCodeXYPosition;
begin
  Statements:=TheStatements;
  InsertPositions:=TheInsertPositions;
  sl:=TStringList.Create;
  try
    // show possible insert positions
    for i:=0 to InsertPositions.Count-1 do begin
      InsertPos:=TInsertStatementPosDescription(InsertPositions[i]);
      CodeXY:=InsertPos.CodeXYPos;
      s:=ExtractFileName(CodeXY.Code.Filename)
         +'('+IntToStr(CodeXY.Y)+','+IntToStr(CodeXY.X)+') '
         +InsertPos.Description;
      sl.Add(s);
    end;
    WhereRadioGroup.Items.Assign(sl);
    WhereRadioGroup.ItemIndex:=0;
    WhereRadioGroup.AutoSize:=true;
    WhereRadioGroup.Caption:='Insert where:';

    // show possible statements
    sl.Clear;
    for s in Statements do
      sl.Add(DbgStr(s));
    ValueRadioGroup.Items.Assign(sl);
    ValueRadioGroup.ItemIndex:=0;
    ValueRadioGroup.Caption:='Insert what:';
  finally
    sl.Free;
  end;
end;

end.

