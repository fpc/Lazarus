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
    Dialog for the Extract Proc feature.
    Allows user choose what kind of procedure to create and shows missing
    identifiers.
}
unit ExtractProcDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AVL_Tree, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls,
  BasicCodeTools, CodeAtom, CodeCache, CodeToolManager, ExtractProcTool,
  LazarusIDEStrConsts, IDEProcs, MiscOptions, IDEContextHelpEdit;

type

  { TExtractProcDialog }

  TExtractProcDialog = class(TForm)
    MissingIdentifiersListBox: TListBox;
    MissingIdentifiersGroupBox: TGroupBox;
    NameEdit: TEDIT;
    NameGroupbox: TGROUPBOX;
    OkButton: TBitBtn;
    CancelButton: TBitBtn;
    HelpButton: TBitBtn;
    BtnPanel: TPanel;
    TypeRadiogroup: TRADIOGROUP;
    procedure HelpButtonClick(Sender: TObject);
    procedure ExtractProcDialogCREATE(Sender: TObject);
    procedure ExtractProcDialogClose(Sender: TObject;
      var CloseAction: TCloseAction);
    procedure OkButtonCLICK(Sender: TObject);
  private
    FMethodPossible: boolean;
    FMissingIdentifiers: TAVLTree;
    FSubProcSameLvlPossible: boolean;
    procedure SetMissingIdentifiers(const AValue: TAVLTree);
  public
    procedure UpdateAvailableTypes;
    function GetProcType: TExtractProcType;
    function GetProcName: string;
    property MethodPossible: boolean read FMethodPossible write FMethodPossible;
    property SubProcSameLvlPossible: boolean read FSubProcSameLvlPossible write FSubProcSameLvlPossible;
    property MissingIdentifiers: TAVLTree read FMissingIdentifiers write SetMissingIdentifiers;
  end;

function ShowExtractProcDialog(Code: TCodeBuffer;
  const BlockBegin, BlockEnd: TPoint;
  var NewSource: TCodeBuffer;
  var NewX, NewY, NewTopLine: integer): TModalresult;

implementation

function ShowExtractProcDialog(Code: TCodeBuffer;
  const BlockBegin, BlockEnd: TPoint;
  var NewSource: TCodeBuffer;
  var NewX, NewY, NewTopLine: integer): TModalresult;
var
  ExtractProcDialog: TExtractProcDialog;
  MethodPossible: Boolean;
  SubProcSameLvlPossible: boolean;
  ProcName: String;
  ProcType: TExtractProcType;
  MissingIdentifiers: TAVLTree;
begin
  Result:=mrCancel;
  if CompareCaret(BlockBegin,BlockEnd)<=0 then begin
    MessageDlg(lisNoCodeSelected,
      lisPleaseSelectSomeCodeToExtractANewProcedureMethod,
      mtInformation,[mbCancel],0);
    exit;
  end;
  
  MissingIdentifiers:=nil;
  try
    // check if selected statements can be extracted
    if not CodeToolBoss.CheckExtractProc(Code,BlockBegin,BlockEnd,MethodPossible,
      SubProcSameLvlPossible,MissingIdentifiers)
    then begin
      if CodeToolBoss.ErrorMessage='' then begin
        MessageDlg(lisInvalidSelection,
          Format(lisThisStatementCanNotBeExtractedPleaseSelectSomeCode, [#13]),
        mtInformation,[mbCancel],0);
      end;
      exit;
    end;

    // ask user how to extract
    ExtractProcDialog:=TExtractProcDialog.Create(nil);
    try
      ExtractProcDialog.MethodPossible:=MethodPossible;
      ExtractProcDialog.SubProcSameLvlPossible:=SubProcSameLvlPossible;
      ExtractProcDialog.MissingIdentifiers:=MissingIdentifiers;
      ExtractProcDialog.UpdateAvailableTypes;
      Result:=ExtractProcDialog.ShowModal;
      if Result<>mrOk then exit;
      ProcName:=ExtractProcDialog.GetProcName;
      ProcType:=ExtractProcDialog.GetProcType;
    finally
      ExtractProcDialog.Free;
    end;

    // extract procedure/method
    if not CodeToolBoss.ExtractProc(Code,BlockBegin,BlockEnd,ProcType,ProcName,
      MissingIdentifiers,NewSource,NewX,NewY,NewTopLine)
    then begin
      Result:=mrCancel;
      exit;
    end;
    Result:=mrOk;
  finally
    CodeToolBoss.FreeTreeOfPCodeXYPosition(MissingIdentifiers);
  end;
end;

{ TExtractProcDialog }

procedure TExtractProcDialog.ExtractProcDialogCREATE(Sender: TObject);
begin
  Caption:=lisExtractProcedure;
  NameGroupbox.Caption:=lisNameOfNewProcedure;
  TypeRadiogroup.Caption:=dlgEnvType;
  NameEdit.Text:=MiscellaneousOptions.ExtractProcName;
  MissingIdentifiersGroupBox.Caption:='Missing identifiers';
  
  OkButton.Caption:=lisExtract;
  CancelButton.Caption:=dlgCancel;
end;

procedure TExtractProcDialog.HelpButtonClick(Sender: TObject);
begin
  ShowContextHelpForIDE(Self);
end;

procedure TExtractProcDialog.ExtractProcDialogClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  MiscellaneousOptions.ExtractProcName:=NameEdit.Text;
end;

procedure TExtractProcDialog.OkButtonCLICK(Sender: TObject);
var
  ProcName: String;
begin
  ProcName:=GetProcName;
  if (ProcName='') or (not IsValidIdent(ProcName)) then begin
    MessageDlg(lisInvalidProcName,
      Format(lisSVUOisNotAValidIdentifier, ['"', ProcName, '"']),
      mtError,[mbCancel],0);
    exit;
  end;
  ModalResult:=mrOk;
end;

procedure TExtractProcDialog.SetMissingIdentifiers(const AValue: TAVLTree);
var
  Node: TAVLTreeNode;
  CodePos: PCodeXYPosition;
  p: integer;
  Identifier: string;
  s: String;
begin
  if AValue=FMissingIdentifiers then exit;
  FMissingIdentifiers:=AValue;
  MissingIdentifiersListBox.Items.BeginUpdate;
  MissingIdentifiersListBox.Items.Clear;
  if FMissingIdentifiers<>nil then begin
    Node:=FMissingIdentifiers.FindLowest;
    while Node<>nil do begin
      CodePos:=PCodeXYPosition(Node.Data);
      CodePos^.Code.LineColToPosition(CodePos^.Y,CodePos^.X,p);
      if p>=1 then
        Identifier:=GetIdentifier(@CodePos^.Code.Source[p])
      else
        Identifier:='?';
      s:=Identifier+' at '+IntToStr(CodePos^.Y)+','+IntToStr(CodePos^.X);
      MissingIdentifiersListBox.Items.Add(s);
      Node:=FMissingIdentifiers.FindSuccessor(Node);
    end;
  end;
  MissingIdentifiersListBox.Items.EndUpdate;
  
  // show/hide the MissingIdentifiersGroupBox
  MissingIdentifiersGroupBox.Visible:=MissingIdentifiersListBox.Items.Count>0;
end;

procedure TExtractProcDialog.UpdateAvailableTypes;
begin
  with TypeRadiogroup.Items do begin
    BeginUpdate;
    Clear;
    if MethodPossible then begin
      Add(lisPublicMethod);
      Add(lisPrivateMethod);
      Add(lisProtectedMethod);
      Add(lisPublishedMethod);
      TypeRadiogroup.Columns:=2;
    end else begin
      TypeRadiogroup.Columns:=1;
    end;
    Add(lisProcedure);
    Add(lisProcedureWithInterface);
    Add(lisSubProcedure);
    if SubProcSameLvlPossible then
      Add(lisSubProcedureOnSameLevel);
    EndUpdate;
    TypeRadiogroup.ItemIndex:=Count-1;
  end;
end;

function TExtractProcDialog.GetProcType: TExtractProcType;
var
  Item: string;
begin
  Result:=eptSubProcedure;
  if TypeRadiogroup.ItemIndex>=0 then begin
    Item:=TypeRadiogroup.Items[TypeRadiogroup.ItemIndex];
    if Item=lisPublicMethod then Result:=eptPublicMethod
    else if Item=lisPrivateMethod then Result:=eptPrivateMethod
    else if Item=lisProtectedMethod then Result:=eptProtectedMethod
    else if Item=lisPublishedMethod then Result:=eptPublishedMethod
    else if Item=lisProcedure then Result:=eptProcedure
    else if Item=lisProcedureWithInterface then Result:=eptProcedureWithInterface
    else if Item=lisSubProcedure then Result:=eptSubProcedure
    else if Item=lisSubProcedureOnSameLevel then Result:=eptSubProcedureSameLvl;
  end;
end;

function TExtractProcDialog.GetProcName: string;
begin
  Result:=NameEdit.Text;
end;

initialization
  {$I extractprocdlg.lrs}

end.

