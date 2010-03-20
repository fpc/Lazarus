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
    A dialog showing the abstract methods of the current class
    (at cursor in source editor).
    With the ability to implement them automatically by adding empty method
    stubs.
}
unit AbstractsMethodsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, Graphics, Dialogs,
  CheckLst, StdCtrls, ExtCtrls, Buttons,
  CodeAtom, CodeTree, PascalParserTool, CodeCache, CodeToolManager,
  LazIDEIntf, SrcEditorIntf,
  LazarusIDEStrConsts;

type

  { TAbstractMethodDlgItem }

  TAbstractMethodDlgItem = class
  public
    CodeXYPos: TCodeXYPosition;
    ProcHead: string;
    BelongsToStartClass: boolean;
  end;

  { TAbstractMethodsDialog }

  TAbstractMethodsDialog = class(TForm)
    AddAllBitBtn: TBitBtn;
    NoteLabel: TLabel;
    SelectNoneButton: TButton;
    SelectAllButton: TButton;
    CancelBitBtn: TBitBtn;
    AddFirstBitBtn: TBitBtn;
    MethodsCheckListBox: TCheckListBox;
    MethodsGroupBox: TGroupBox;
    BtnPanel: TPanel;
    procedure AddAllBitBtnClick(Sender: TObject);
    procedure AddFirstBitBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MethodsCheckListBoxClickCheck(Sender: TObject);
    procedure SelectAllButtonClick(Sender: TObject);
    procedure SelectNoneButtonClick(Sender: TObject);
  private
    CodePos: TCodeXYPosition;
    TopLine: integer;
    FItems: TFPList;// list of TAbstractMethodDlgItem
    FCheckingSelection: boolean;
    procedure ClearItems;
    procedure UpdateButtons;
    function CheckSelection: boolean;
    function AddOverrides(OnlyFirst: boolean): boolean;
  public
    NewCode: TCodeBuffer;
    NewX,NewY,NewTopLine: integer;
    procedure Init(aListOfPCodeXYPosition: TFPList; aCode: TCodeBuffer;
                   const aCaret: TPoint; aTopLine: integer);
  end;

function ShowAbstractMethodsDialog: TModalResult;

implementation

{$R *.lfm}

function ShowAbstractMethodsDialog: TModalResult;
var
  AbstractMethodsDialog: TAbstractMethodsDialog;
  SrcEdit: TSourceEditorInterface;
  Code: TCodeBuffer;
  Caret: TPoint;
  ErrMsg: String;
  ListOfPCodeXYPosition: TFPList;
begin
  Result:=mrCancel;
  ListOfPCodeXYPosition:=nil;
  try
    // init codetools
    ErrMsg:=lisSAMIDEIsBusy;
    if not LazarusIDE.BeginCodeTools then exit;
    
    // get cursor position
    ErrMsg:=lisSAMCursorIsNotInAClassDeclaration;
    SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
    if SrcEdit=nil then exit;
    Code:=TCodeBuffer(SrcEdit.CodeToolsBuffer);
    if Code=nil then exit;
    Caret:=SrcEdit.CursorTextXY;
    
    // check cursor is in a class
    if not CodeToolBoss.FindAbstractMethods(Code,Caret.X,Caret.Y,
      ListOfPCodeXYPosition,false) then
    begin
      DebugLn(['ShowAbstractMethodsDialog CodeToolBoss.FindAbstractMethods failed']);
      if CodeToolBoss.ErrorMessage<>'' then begin
        ErrMsg:='';
        LazarusIDE.DoJumpToCodeToolBossError;
      end;
      exit;
    end;

    // check if there are abstract methods left to override
    if (ListOfPCodeXYPosition=nil) or (ListOfPCodeXYPosition.Count=0) then begin
      ErrMsg:='';
      MessageDlg(lisSAMNoAbstractMethodsFound,
        lisSAMThereAreNoAbstractMethodsLeftToOverride
        ,mtConfirmation,[mbOk],0);
      Result:=mrOk;
      exit;
    end;

    ErrMsg:='';
    AbstractMethodsDialog:=TAbstractMethodsDialog.Create(nil);
    AbstractMethodsDialog.Init(ListOfPCodeXYPosition,Code,Caret,SrcEdit.TopLine);
    Result:=AbstractMethodsDialog.ShowModal;
    AbstractMethodsDialog.Free;
  finally
    CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
    if ErrMsg<>'' then begin
      MessageDlg(lisCCOErrorCaption,
        lisSAMUnableToShowAbstractMethodsOfTheCurrentClassBecaus+#13
        +ErrMsg,mtError,[mbCancel],0);
    end;
  end;
end;

{ TAbstractMethodsDialog }

procedure TAbstractMethodsDialog.FormCreate(Sender: TObject);
begin
  FItems:=TFPList.Create;

  AddFirstBitBtn.Caption:=lisSAMOverrideFirstSelected;
  AddAllBitBtn.Caption:=lisSAMOverrideAllSelected;

  SelectNoneButton.Caption:=lisSAMSelectNone;
  SelectAllButton.Caption:=lisMenuSelectAll;
  MethodsGroupBox.Caption:=lisSAMAbstractMethodsNotYetOverridden;
end;

procedure TAbstractMethodsDialog.AddFirstBitBtnClick(Sender: TObject);
begin
  if not AddOverrides(true) then exit;
  ModalResult:=mrOk;
end;

procedure TAbstractMethodsDialog.AddAllBitBtnClick(Sender: TObject);
begin
  if not AddOverrides(false) then exit;
  ModalResult:=mrOk;
end;

procedure TAbstractMethodsDialog.FormDestroy(Sender: TObject);
begin
  ClearItems;
end;

procedure TAbstractMethodsDialog.MethodsCheckListBoxClickCheck(Sender: TObject);
begin
  CheckSelection;
  UpdateButtons;
end;

procedure TAbstractMethodsDialog.SelectAllButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do
    MethodsCheckListBox.Checked[i]:=
      not TAbstractMethodDlgItem(FItems[i]).BelongsToStartClass;
end;

procedure TAbstractMethodsDialog.SelectNoneButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do
    MethodsCheckListBox.Checked[i]:=false;
end;

procedure TAbstractMethodsDialog.ClearItems;
var
  i: Integer;
begin
  if FItems=nil then exit;
  for i:=0 to FItems.Count-1 do
    TObject(FItems[i]).Free;
  FreeAndNil(FItems);
end;

procedure TAbstractMethodsDialog.UpdateButtons;
var
  i: Integer;
begin
  i:=MethodsCheckListBox.Items.Count-1;
  while (i>=0) and (not MethodsCheckListBox.Checked[i]) do dec(i);
  AddFirstBitBtn.Enabled:=i>=0;
  AddAllBitBtn.Enabled:=AddFirstBitBtn.Enabled;
end;

function TAbstractMethodsDialog.CheckSelection: boolean;
var
  i: Integer;
  Item: TAbstractMethodDlgItem;
begin
  Result:=true;
  if FCheckingSelection then exit;
  FCheckingSelection:=true;
  try
    for i:=0 to FItems.Count-1 do begin
      Item:=TAbstractMethodDlgItem(FItems[i]);
      if MethodsCheckListBox.Checked[i] and Item.BelongsToStartClass then begin
        if Result then begin
          MessageDlg(lisCCOErrorCaption,
            lisSAMThisMethodCanNotBeOverriddenBecauseItIsDefinedInTh,
            mtError,[mbCancel],0);
          Result:=false;
        end;
        MethodsCheckListBox.Checked[i]:=false;
      end;
    end;
  finally
    FCheckingSelection:=false;
  end;
end;

function TAbstractMethodsDialog.AddOverrides(OnlyFirst: boolean): boolean;
var
  i: Integer;
  NewList: TFPList;
  Item: TAbstractMethodDlgItem;
begin
  Result:=false;
  if not CheckSelection then exit;
  NewList:=nil;
  try
    for i:=0 to FItems.Count-1 do begin
      if not MethodsCheckListBox.Checked[i] then continue;
      Item:=TAbstractMethodDlgItem(FItems[i]);
      AddCodePosition(NewList,Item.CodeXYPos);
      DebugLn(['TAbstractMethodsDialog.AddOverrides ',Item.CodeXYPos.Code.Filename,' ',Item.CodeXYPos.X,',',Item.CodeXYPos.Y]);
      if OnlyFirst then break;
    end;
    
    //DebugLn(['TAbstractMethodsDialog.AddOverrides ',CodePos.Code.Filename,' ',CodePos.X,',',CodePos.Y]);
    if not CodeToolBoss.AddMethods(CodePos.Code,CodePos.X,CodePos.Y,TopLine,
      NewList,true,NewCode,NewX,NewY,NewTopLine)
    then begin
      LazarusIDE.DoJumpToCodeToolBossError;
      exit;
    end;
    
    LazarusIDE.DoOpenFileAndJumpToPos(NewCode.Filename,Point(NewX,NewY),
                                      NewTopLine,-1,-1,[]);
  finally
    CodeToolBoss.FreeListOfPCodeXYPosition(NewList);
  end;

  Result:=true;
end;

procedure TAbstractMethodsDialog.Init(aListOfPCodeXYPosition: TFPList;
  aCode: TCodeBuffer; const aCaret: TPoint; aTopLine: integer);
var
  i: Integer;
  CodeXYPos: TCodeXYPosition;
  CurTool: TCodeTool;
  ListOfPCodeXYPosition: TFPList;
  Tool: TCodeTool;
  CleanPos: integer;
  ClassNode: TCodeTreeNode;
  CurNode: TCodeTreeNode;
  ProcNode: TCodeTreeNode;
  NewItem: TAbstractMethodDlgItem;
  StartClassName: String;
  BelongsToStartClassCnt: Integer;
  NoteStr: String;
begin
  ListOfPCodeXYPosition:=aListOfPCodeXYPosition;
  if ListOfPCodeXYPosition=nil then begin
    DebugLn(['TAbstractMethodsDialog.Init ListOfPCodeXYPosition=nil']);
    exit;
  end;
  CodePos.Code:=aCode;
  CodePos.X:=aCaret.X;
  CodePos.Y:=aCaret.Y;
  TopLine:=aTopLine;

  // get Tool and ClassNode
  Tool:=CodeToolBoss.GetCodeToolForSource(CodePos.Code,true,false) as TCodeTool;
  if Tool.CaretToCleanPos(CodePos,CleanPos)<>0 then begin
    DebugLn(['TAbstractMethodsDialog.Init invalid ',CodePos.Code.Filename,' ',CodePos.X,',',CodePos.Y]);
    exit;
  end;
  ClassNode:=Tool.FindDeepestNodeAtPos(CleanPos,false);
  if ClassNode=nil then begin
    DebugLn(['TAbstractMethodsDialog.Init no node at cursor ',CodePos.Code.Filename,' ',CodePos.X,',',CodePos.Y]);
    exit;
  end;
  if ClassNode.Desc=ctnTypeDefinition then
    ClassNode:=ClassNode.FirstChild
  else if ClassNode.Desc=ctnGenericType then
    ClassNode:=ClassNode.LastChild
  else
    ClassNode:=Tool.FindClassOrInterfaceNode(ClassNode);
  if (ClassNode=nil) then begin
    DebugLn(['TAbstractMethodsDialog.Init no class node at cursor ',CodePos.Code.Filename,' ',CodePos.X,',',CodePos.Y]);
    exit;
  end;
  
  StartClassName:=Tool.ExtractClassName(ClassNode,false);
  BelongsToStartClassCnt:=0;

  // create items
  for i:=0 to ListOfPCodeXYPosition.Count-1 do begin
    CodeXYPos:=PCodeXYPosition(ListOfPCodeXYPosition[i])^;
    CurTool:=CodeToolBoss.GetCodeToolForSource(CodeXYPos.Code,true,false) as TCodeTool;
    if CurTool.CaretToCleanPos(CodeXYPos,CleanPos)<>0 then begin
      DebugLn(['TAbstractMethodsDialog.Init skipping ',CodeXYPos.Code.Filename,' ',CodeXYPos.X,',',CodeXYPos.Y]);
      continue;
    end;
    CurNode:=CurTool.FindDeepestNodeAtPos(CleanPos,false);
    if CurNode=nil then begin
      DebugLn(['TAbstractMethodsDialog.Init no node at ',CodeXYPos.Code.Filename,' ',CodeXYPos.X,',',CodeXYPos.Y]);
      continue;
    end;
    if CurNode.Desc<>ctnProcedure then begin
      DebugLn(['TAbstractMethodsDialog.Init no proc node at ',CodeXYPos.Code.Filename,' ',CodeXYPos.X,',',CodeXYPos.Y]);
      continue;
    end;
    ProcNode:=CurNode;
    NewItem:=TAbstractMethodDlgItem.Create;
    NewItem.CodeXYPos:=CodeXYPos;
    NewItem.ProcHead:=CurTool.ExtractProcHead(ProcNode,[phpAddClassname,
      phpWithStart,phpWithParameterNames,phpWithVarModifiers,
      phpWithDefaultValues,phpWithResultType,
      phpWithOfObject,phpWithCallingSpecs]);
    NewItem.BelongsToStartClass:=ProcNode.HasAsParent(ClassNode);
    if NewItem.BelongsToStartClass then
      inc(BelongsToStartClassCnt);
    FItems.Add(NewItem);
  end;
  
  MethodsCheckListBox.Clear;
  for i:=0 to FItems.Count-1 do begin
    NewItem:=TAbstractMethodDlgItem(FItems[i]);
    MethodsCheckListBox.Items.Add(NewItem.ProcHead);
    MethodsCheckListBox.Checked[i]:=not NewItem.BelongsToStartClass;
  end;

  // caption
  Caption:=Format(lisSAMAbstractMethodsOf, [StartClassName]);
  
  // note
  NoteStr:='';
  if BelongsToStartClassCnt>0 then begin
    NoteStr:=Format(lisSAMIsAnAbstractClassItHasAbstractMethods, [
      StartClassName, IntToStr(BelongsToStartClassCnt)])+#13;
  end;
  NoteStr:=NoteStr+
    Format(lisSAMThereAreAbstractMethodsToOverrideSelectTheMethodsF,
           [IntToStr(FItems.Count-BelongsToStartClassCnt), #13]);
  NoteLabel.Caption:=NoteStr;
  
  UpdateButtons;
end;

end.


