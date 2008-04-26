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
    A dialog showing the empty methods of the current class
    (at cursor in source editor).
    With the ability to remove them automatically.
}
unit EmptyMethodsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ButtonPanel, SynEdit, SynHighlighterPas,
  CodeToolsStructs, CodeAtom, CodeCache, CodeToolManager, PascalParserTool,
  CodeTree,
  SrcEditorIntf, LazIDEIntf,
  LazarusIDEStrConsts;

type

  { TEmptyMethodsDialog }

  TEmptyMethodsDialog = class(TForm)
    AllButton: TButton;
    PublishedButton: TButton;
    ButtonPanel1: TButtonPanel;
    PrivateCheckBox: TCheckBox;
    ProtectedCheckBox: TCheckBox;
    PublicCheckBox: TCheckBox;
    PublishedCheckBox: TCheckBox;
    SectionsGroupBox: TGroupBox;
    MethodsGroupBox: TGroupBox;
    MethodsSynEdit: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    procedure AllButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure PrivateCheckBoxChange(Sender: TObject);
    procedure PublishedButtonClick(Sender: TObject);
  private
    FCaret: TPoint;
    FCode: TCodeBuffer;
    function GetSections: TPascalClassSections;
    procedure SetCaret(const AValue: TPoint);
    procedure SetCode(const AValue: TCodeBuffer);
    procedure SetSections(const AValue: TPascalClassSections);
    procedure UpdateList;
  public
    property Sections: TPascalClassSections read GetSections write SetSections;
    property Code: TCodeBuffer read FCode write SetCode;
    property Caret: TPoint read FCaret write SetCaret;
  end;

function ShowEmptyMethodsDialog: TModalResult;


implementation


function ShowEmptyMethodsDialog: TModalResult;
var
  EmptyMethodsDialog: TEmptyMethodsDialog;
  ErrMsg: String;
  SrcEdit: TSourceEditorInterface;
  Code: TCodeBuffer;
  Caret: TPoint;
  ListOfPCodeXYPosition: TFPList;
  AllEmpty: boolean;
begin
  ListOfPCodeXYPosition:=TFPList.Create;
  try
    // init codetools
    ErrMsg:=lisSAMIDEIsBusy;
    if not LazarusIDE.BeginCodeTools then exit;

    // get cursor position
    ErrMsg:=lisSAMCursorIsNotInAClassDeclaration;
    SrcEdit:=SourceEditorWindow.ActiveEditor;
    if SrcEdit=nil then exit;
    Code:=TCodeBuffer(SrcEdit.CodeToolsBuffer);
    if Code=nil then exit;
    Caret:=SrcEdit.CursorTextXY;
    ErrMsg:='';

    // check cursor is in a class
    if not CodeToolBoss.FindEmptyMethods(Code,Caret.X,Caret.Y,
      AllPascalClassSections,ListOfPCodeXYPosition,AllEmpty)
    then begin
      DebugLn(['ShowEmptyMethodsDialog CodeToolBoss.FindEmptyMethods failed']);
      if CodeToolBoss.ErrorMessage<>'' then begin
        ErrMsg:='';
        LazarusIDE.DoJumpToCodeToolBossError;
      end;
      exit;
    end;
    CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);

    EmptyMethodsDialog:=TEmptyMethodsDialog.Create(nil);
    try
      EmptyMethodsDialog.Code:=Code;
      EmptyMethodsDialog.Caret:=Caret;
      EmptyMethodsDialog.UpdateList;
      Result:=EmptyMethodsDialog.ShowModal;
    finally
      EmptyMethodsDialog.Free;
    end;
  finally
    CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
    if ErrMsg<>'' then begin
      MessageDlg(lisCCOErrorCaption,
        'Unable to show empty methods of the current class, because'+#13
        +ErrMsg,mtError,[mbCancel],0);
    end;
  end;
end;

{ TEmptyMethodsDialog }

procedure TEmptyMethodsDialog.FormCreate(Sender: TObject);
begin
  Caption:=lisEMDEmtpyMethods;
  SectionsGroupBox.Caption:=lisEMDSearchInTheseClassSections;
  PrivateCheckBox.Caption:=lisPrivate;
  ProtectedCheckBox.Caption:=lisProtected;
  PublicCheckBox.Caption:=lisEMDPublic;
  PublishedButton.Caption:=lisEMDPublished;
  AllButton.Caption:=lisEMDAll;
  PublishedButton.Caption:=lisEMDOnlyPublished;
  MethodsGroupBox.Caption:=lisEMDFoundEmptyMethods;
  Sections:=AllPascalClassSections;
  
  ButtonPanel1.OKButton.OnClick:=@OKButtonClick;
  ButtonPanel1.OKButton.Caption:=lisEMDRemoveMethods;
end;

procedure TEmptyMethodsDialog.OKButtonClick(Sender: TObject);
var
  AllEmpty: boolean;
  RemovedProcHeads: TStrings;
begin
  DebugLn(['TEmptyMethodsDialog.OKButtonClick ']);
  RemovedProcHeads:=nil;
  try
    if (not CodeToolBoss.RemoveEmptyMethods(Code,Caret.X,Caret.Y,Sections,
      AllEmpty,[],RemovedProcHeads))
    then begin
      DebugLn(['TEmptyMethodsDialog.OKButtonClick failed']);
      exit;
    end;
  finally
    RemovedProcHeads.Free;
  end;
  ModalResult:=mrOk;
end;

procedure TEmptyMethodsDialog.PrivateCheckBoxChange(Sender: TObject);
begin
  UpdateList;
end;

procedure TEmptyMethodsDialog.PublishedButtonClick(Sender: TObject);
begin
  Sections:=[pcsPublished];
end;

procedure TEmptyMethodsDialog.SetSections(const AValue: TPascalClassSections);
begin
  PrivateCheckBox.Checked:=pcsPrivate in AValue;
  ProtectedCheckBox.Checked:=pcsProtected in AValue;
  PublicCheckBox.Checked:=pcsPublic in AValue;
  PublishedCheckBox.Checked:=pcsPublished in AValue;
end;

procedure TEmptyMethodsDialog.SetCaret(const AValue: TPoint);
begin
  FCaret:=AValue;
end;

function TEmptyMethodsDialog.GetSections: TPascalClassSections;
begin
  Result:=[];
  if PrivateCheckBox.Checked then Include(Result,pcsPrivate);
  if ProtectedCheckBox.Checked then Include(Result,pcsProtected);
  if PublicCheckBox.Checked then Include(Result,pcsPublic);
  if PublishedCheckBox.Checked then Include(Result,pcsPublished);
end;

procedure TEmptyMethodsDialog.SetCode(const AValue: TCodeBuffer);
begin
  if FCode=AValue then exit;
  FCode:=AValue;
end;

procedure TEmptyMethodsDialog.UpdateList;
var
  CurSections: TPascalClassSections;
  ListOfPCodeXYPosition: TFPList;
  i: Integer;
  CodePos: TCodeXYPosition;
  Tool: TCodeTool;
  CleanPos: integer;
  Node: TCodeTreeNode;
  NodeText: String;
  AllEmpty: boolean;
  NewTxt: String;
begin
  if (Code=nil) or (Caret.X<1) or (Caret.Y<1) then begin
    MethodsSynEdit.Text:='';
    exit;
  end;

  CurSections:=Sections;
  ListOfPCodeXYPosition:=TFPList.Create;
  try
    if (not CodeToolBoss.FindEmptyMethods(Code,Caret.X,Caret.Y,
      CurSections,ListOfPCodeXYPosition,AllEmpty))
    or (not CodeToolBoss.Explore(Code,Tool,false))
    then begin
      MethodsSynEdit.Text:='CodeToolBoss.FindEmptyMethods failed'#10
        +CodeToolBoss.ErrorMessage;
      exit;
    end;

    NewTxt:='';
    for i:=0 to ListOfPCodeXYPosition.Count-1 do begin
      CodePos:=PCodeXYPosition(ListOfPCodeXYPosition[i])^;
      //DebugLn(['TEmptyMethodsDialog.UpdateList ',i,' ',DbgsCXY(CodePos)]);
      if Tool.CaretToCleanPos(CodePos,CleanPos)<>0 then begin
        DebugLn(['TEmptyMethodsDialog.UpdateList Tool.CaretToCleanPos failed']);
        continue;
      end;
      Node:=Tool.FindDeepestNodeAtPos(CleanPos,false);
      if Node=nil then begin
        DebugLn(['TEmptyMethodsDialog.UpdateList Tool.FindDeepestNodeAtPos failed']);
        continue;
      end;
      NodeText:=Tool.ExtractProcHead(Node,[phpWithStart,phpWithParameterNames,
        phpWithVarModifiers,phpWithDefaultValues,phpWithResultType,
        phpWithCallingSpecs,phpWithProcModifiers]);
      NewTxt:=NewTxt+NodeText+#10;
    end;
    MethodsSynEdit.Text:=NewTxt;
  finally
    CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
  end;
end;

procedure TEmptyMethodsDialog.AllButtonClick(Sender: TObject);
begin
  Sections:=AllPascalClassSections;
end;

initialization
  {$I emptymethodsdlg.lrs}

end.

