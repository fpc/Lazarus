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
    An IDE dialog to add a "With" block to the sourceeditor selection.
}
unit AddWithBlockDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls, Grids,
  // Codetools
  CodeCache, CodeToolManager, FileProcs, FindDeclarationTool,
  // IDEIntf
  IDEDialogs, LazIDEIntf, SrcEditorIntf, CodyStrConsts, CodyUtils;

type

  { TAddWithBlockDialog }

  TAddWithBlockDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    WithExprGroupBox: TGroupBox;
    WithExprStringGrid: TStringGrid;
    procedure ButtonPanel1OKButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    SrcEdit: TSourceEditorInterface;
    Code: TCodeBuffer;
    StartPos: TPoint;
    EndPos: TPoint;
    Candidates: TStringList;
    function Check(aSrcEdit: TSourceEditorInterface): boolean;
  end;

procedure ShowAddWithBlockDialog(Sender: TObject);

function CompareObjectThenText(List: TStringList; Index1, Index2: Integer): integer;

implementation

procedure ShowAddWithBlockDialog(Sender: TObject);
var
  AddWithBlockDialog: TAddWithBlockDialog;
  SrcEdit: TSourceEditorInterface;
  i: Integer;
  WithExpr: String;
begin
  // commit changes form source editor to codetools
  if not LazarusIDE.BeginCodeTools then exit;
  // check context at cursor
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if (SrcEdit=nil) or (SrcEdit.Selection='') then begin
    IDEMessageDialog(crsCWError,
      crsPleaseSelectSomeCodeInTheSourceEditor, mtError, [mbCancel]);
    exit;
  end;

  AddWithBlockDialog:=TAddWithBlockDialog.Create(nil);
  try
    if not AddWithBlockDialog.Check(SrcEdit) then exit;
    if AddWithBlockDialog.ShowModal<>mrOk then exit;

    with AddWithBlockDialog do begin
      i:=WithExprStringGrid.Row;
      if (i<1) then begin
        debugln(['ShowAddWithBlockDialog nothing selected']);
        exit;
      end;
      WithExpr:=WithExprStringGrid.Cells[0,i];
      if not CodeToolBoss.AddWithBlock(Code,StartPos.X,StartPos.Y,
        EndPos.X,EndPos.Y,WithExpr,nil)
      then begin
        LazarusIDE.DoJumpToCodeToolBossError;
        exit;
      end;
    end;

  finally
    AddWithBlockDialog.Free;
  end;
end;

function CompareObjectThenText(List: TStringList; Index1, Index2: Integer
  ): integer;
var
  n1: PtrUInt;
  n2: PtrUInt;
begin
  n1:=PtrUInt(List.Objects[Index1]);
  n2:=PtrUInt(List.Objects[Index2]);
  if n1>n2 then exit(-1);
  if n2>n1 then exit(1);
  Result:=CompareText(List[Index1],List[Index2]);
end;

{ TAddWithBlockDialog }

procedure TAddWithBlockDialog.FormCreate(Sender: TObject);
begin
  Caption:=crsAddWithBlock;
  WithExprGroupBox.Caption:=crsSelectExpression;
  WithExprStringGrid.Columns[0].Title.Caption:=crsExpression;
  WithExprStringGrid.Columns[1].Title.Caption:=crsCount;

  ButtonPanel1.HelpButton.Caption:=crsHelp;
  ButtonPanel1.OKButton.Caption:=crsBTNOK;
  ButtonPanel1.CancelButton.Caption:=crsBTNCancel;
end;

procedure TAddWithBlockDialog.ButtonPanel1OKButtonClick(Sender: TObject);
var
  i: Integer;
begin
  i:=WithExprStringGrid.Row;
  if (i<1) then exit;
  ModalResult:=mrOk;
end;

function TAddWithBlockDialog.Check(aSrcEdit: TSourceEditorInterface): boolean;
var
  i: Integer;
begin
  Result:=false;
  SrcEdit:=aSrcEdit;
  Code:=TCodeBuffer(SrcEdit.CodeToolsBuffer);
  StartPos:=SrcEdit.BlockBegin;
  EndPos:=SrcEdit.BlockEnd;
  Candidates:=TStringList.Create;
  try
    //debugln(['TAddWithBlockDialog.Check ',dbgs(StartPos),' ',dbgs(EndPos)]);
    if not CodeToolBoss.AddWithBlock(Code,StartPos.X,StartPos.Y,EndPos.X,EndPos.Y,
      '',Candidates)
    then begin
      debugln(['ShowAddWithBlockDialog CodeToolBoss.AddWithBlock check failed']);
      LazarusIDE.DoJumpToCodeToolBossError;
      exit;
    end;

    if Candidates.Count=0 then begin
      IDEMessageDialog(crsCWError,
        crsPleaseSelectSomeCodeInTheSourceEditorForANewWithBl,mtError,[mbCancel]);
      exit;
    end;

    Candidates.CustomSort(@CompareObjectThenText);

    WithExprStringGrid.RowCount:=Candidates.Count+1;
    for i:=0 to Candidates.Count-1 do begin
      WithExprStringGrid.Cells[0,i+1]:=Candidates[i];
      WithExprStringGrid.Cells[1,i+1]:=IntToStr(PtrUInt(Candidates.Objects[i]));
    end;

    WithExprStringGrid.Row:=1;
    WithExprStringGrid.Col:=0;
  finally
    Candidates.Free;
  end;

  Result:=true;
end;

{$R *.lfm}

end.

