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
    Options dialog and methods for finding and renaming identifier references.
}
unit FindRenameIdentifier;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls,
  AVL_Tree, CodeAtom, CodeCache,
  CodeToolManager,
  LazarusIDEStrConsts, IDEProcs, IDEWindowIntf, MiscOptions, DialogProcs,
  InputHistory, SearchResultView;

type
  TFindRenameIdentifierDialog = class(TForm)
    CancelButton: TButton;
    CurrentGroupBox: TGroupBox;
    CurrentListBox: TListBox;
    ExtraFilesEdit: TEdit;
    ExtraFilesGroupBox: TGroupBox;
    FindOrRenameButton: TButton;
    NewEdit: TEdit;
    NewGroupBox: TGroupBox;
    RenameCheckBox: TCheckBox;
    ScopeCommentsCheckBox: TCheckBox;
    ScopeGroupBox: TGroupBox;
    ScopeRadioGroup: TRadioGroup;
    procedure FindOrRenameButtonClick(Sender: TObject);
    procedure FindRenameIdentifierDialogClose(Sender: TObject;
      var CloseAction: TCloseAction);
    procedure FindRenameIdentifierDialogCreate(Sender: TObject);
    procedure RenameCheckBoxChange(Sender: TObject);
  private
    FAllowRename: boolean;
    FIdentifierFilename: string;
    FIdentifierPosition: TPoint;
    procedure SetAllowRename(const AValue: boolean);
    procedure UpdateRename;
  public
    procedure LoadFromConfig;
    procedure SaveToConfig;
    procedure LoadFromOptions(Options: TFindRenameIdentifierOptions);
    procedure SaveToOptions(Options: TFindRenameIdentifierOptions);
    procedure SetIdentifier(const NewIdentifierFilename: string;
                            const NewIdentifierPosition: TPoint);
    property IdentifierFilename: string read FIdentifierFilename;
    property IdentifierPosition: TPoint read FIdentifierPosition;
    property AllowRename: boolean read FAllowRename write SetAllowRename;
  end;


function ShowFindRenameIdentifierDialog(const Filename: string;
  const Position: TPoint; AllowRename, SetRenameActive: boolean;
  Options: TFindRenameIdentifierOptions): TModalResult;
function GatherIdentifierReferences(Files: TStringList;
  DeclarationCode: TCodeBuffer; const DeclarationCaretXY: TPoint;
  SearchInComments: boolean;
  var TreeOfPCodeXYPosition: TAVLTree): TModalResult;
function ShowIdentifierReferences(
  DeclarationCode: TCodeBuffer; const DeclarationCaretXY: TPoint;
  TreeOfPCodeXYPosition: TAVLTree): TModalResult;
procedure AddReferencesToResultView(DeclarationCode: TCodeBuffer;
  const DeclarationCaretXY: TPoint; TargetCode: TCodeBuffer;
  TreeOfPCodeXYPosition: TAVLTree; ClearItems: boolean; SearchPageIndex: integer);
  

implementation


function ShowFindRenameIdentifierDialog(const Filename: string;
  const Position: TPoint; AllowRename, SetRenameActive: boolean;
  Options: TFindRenameIdentifierOptions): TModalResult;
var
  FindRenameIdentifierDialog: TFindRenameIdentifierDialog;
begin
  FindRenameIdentifierDialog:=TFindRenameIdentifierDialog.Create(nil);
  try
    FindRenameIdentifierDialog.LoadFromConfig;
    FindRenameIdentifierDialog.SetIdentifier(Filename,Position);
    FindRenameIdentifierDialog.AllowRename:=AllowRename;
    if SetRenameActive and AllowRename then
      FindRenameIdentifierDialog.RenameCheckBox.Checked:=true;
    Result:=FindRenameIdentifierDialog.ShowModal;
    if Result=mrOk then
      if Options<>nil then
        FindRenameIdentifierDialog.SaveToOptions(Options);
  finally
    FindRenameIdentifierDialog.Free;
  end;
end;

function GatherIdentifierReferences(Files: TStringList;
  DeclarationCode: TCodeBuffer; const DeclarationCaretXY: TPoint;
  SearchInComments: boolean;
  var TreeOfPCodeXYPosition: TAVLTree): TModalResult;
var
  i: Integer;
  LoadResult: TModalResult;
  Code: TCodeBuffer;
  ListOfPCodeXYPosition: TFPList;
begin
  Result:=mrCancel;
  ListOfPCodeXYPosition:=nil;
  TreeOfPCodeXYPosition:=nil;
  try
    // sort files
    Files.Sort;
    // remove doubles
    i:=0;
    while i<=Files.Count-2 do begin
      while (i<=Files.Count-2) and (CompareFilenames(Files[i],Files[i+1])=0) do
      begin
        Files.Delete(i+1);
      end;
      inc(i);
    end;

    // search in every file
    for i:=0 to Files.Count-1 do begin
      LoadResult:=
               LoadCodeBuffer(Code,Files[i],[lbfCheckIfText,lbfUpdateFromDisk]);
      if LoadResult=mrAbort then begin
        debugln('GatherIdentifierReferences unable to load "',Code.Filename,'"');
        exit;
      end;
      if LoadResult<>mrOk then continue;
      
      // search references
      CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
      if not CodeToolBoss.FindReferences(
        DeclarationCode,DeclarationCaretXY.X,DeclarationCaretXY.Y,
        Code, not SearchInComments, ListOfPCodeXYPosition) then
      begin
        debugln('GatherIdentifierReferences unable to FindReferences in "',Code.Filename,'"');
        Result:=mrAbort;
        exit;
      end;
      //debugln('GatherIdentifierReferences FindReferences in "',Code.Filename,'" ',dbgs(ListOfPCodeXYPosition<>nil));

      // add to tree
      if ListOfPCodeXYPosition<>nil then begin
        if TreeOfPCodeXYPosition=nil then
          TreeOfPCodeXYPosition:=CodeToolBoss.CreateTreeOfPCodeXYPosition;
        CodeToolBoss.AddListToTreeOfPCodeXYPosition(ListOfPCodeXYPosition,
                                              TreeOfPCodeXYPosition,true,false);
      end;
    end;

    Result:=mrOk;
  finally
    CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
    if Result<>mrOk then
      CodeToolBoss.FreeTreeOfPCodeXYPosition(TreeOfPCodeXYPosition);
  end;
end;

function ShowIdentifierReferences(
  DeclarationCode: TCodeBuffer; const DeclarationCaretXY: TPoint;
  TreeOfPCodeXYPosition: TAVLTree): TModalResult;
var
  Identifier: string;
  OldSearchPageIndex: LongInt;
  SearchPageIndex: LongInt;
begin
  Result:=mrCancel;
  SearchPageIndex:=-1;
  try
    // show result
    CodeToolBoss.GetIdentifierAt(DeclarationCode,
      DeclarationCaretXY.X,DeclarationCaretXY.Y,Identifier);
    // create a search result page
    SearchPageIndex:=SearchResultsView.AddSearch(
      'References of '+Identifier,
      Identifier,
      '',
      ExtractFilePath(DeclarationCode.Filename),
      '*.pas;*.pp;*.p;*.inc',
      [fifWholeWord,fifSearchDirectories]);
    if SearchPageIndex<0 then exit;

    // list results
    SearchResultsView.BeginUpdate(SearchPageIndex);
    AddReferencesToResultView(DeclarationCode,DeclarationCaretXY,
                   DeclarationCode,TreeOfPCodeXYPosition,false,SearchPageIndex);
    OldSearchPageIndex:=SearchPageIndex;
    SearchPageIndex:=-1;
    SearchResultsView.EndUpdate(OldSearchPageIndex);
    SearchResultsView.ShowOnTop;
  finally
    if SearchPageIndex>=0 then
      SearchResultsView.EndUpdate(SearchPageIndex);
  end;
end;

procedure AddReferencesToResultView(DeclarationCode: TCodeBuffer;
  const DeclarationCaretXY: TPoint; TargetCode: TCodeBuffer;
  TreeOfPCodeXYPosition: TAVLTree; ClearItems: boolean;
  SearchPageIndex: integer);
var
  Identifier: string;
  CodePos: PCodeXYPosition;
  CurLine: String;
  TrimmedLine: String;
  TrimCnt: Integer;
  ANode: TAVLTreeNode;
begin
  CodeToolBoss.GetIdentifierAt(DeclarationCode,
    DeclarationCaretXY.X,DeclarationCaretXY.Y,Identifier);

  SearchResultsView.BeginUpdate(SearchPageIndex);
  if ClearItems then
    SearchResultsView.Items[SearchPageIndex].Clear;
  if (TreeOfPCodeXYPosition<>nil) then begin
    ANode:=TreeOfPCodeXYPosition.FindHighest;
    while ANode<>nil do begin
      CodePos:=PCodeXYPosition(ANode.Data);
      CurLine:=TrimRight(CodePos^.Code.GetLine(CodePos^.Y-1));
      TrimmedLine:=Trim(CurLine);
      TrimCnt:=length(CurLine)-length(TrimmedLine);
      //debugln('ShowReferences x=',dbgs(CodePos^.x),' y=',dbgs(CodePos^.y),' ',CurLine);
      SearchResultsView.AddMatch(SearchPageIndex,
                                 CodePos^.Code.Filename,
                                 Point(CodePos^.X,CodePos^.Y),
                                 Point(CodePos^.X+length(Identifier),CodePos^.Y),
                                 TrimmedLine,
                                 CodePos^.X-TrimCnt, length(Identifier));
      ANode:=TreeOfPCodeXYPosition.FindPrecessor(ANode);
    end;
  end;
  SearchResultsView.EndUpdate(SearchPageIndex);
end;

{ TFindRenameIdentifierDialog }

procedure TFindRenameIdentifierDialog.FindRenameIdentifierDialogCreate(
  Sender: TObject);
begin
  IDEDialogLayoutList.ApplyLayout(Self,450,460);

  Caption:=lisFRIFindOrRenameIdentifier;
  CancelButton.Caption:=dlgCancel;
  CurrentGroupBox.Caption:=lisCodeToolsOptsIdentifier;
  ExtraFilesGroupBox.Caption:=lisFRIAdditionalFilesToSearchEGPathPasPath2Pp;
  FindOrRenameButton.Caption:=lisFRIFindReferences;
  NewGroupBox.Caption:=lisFRIRenameTo;
  RenameCheckBox.Caption:=lisFRIRename;
  ScopeCommentsCheckBox.Caption:=lisFRISearchInCommentsToo;
  ScopeGroupBox.Caption:=lisFRISearchWhere;
  ScopeRadioGroup.Caption:=dlgScope;
  ScopeRadioGroup.Items[0]:=lisFRIinCurrentUnit;
  ScopeRadioGroup.Items[1]:=lisFRIinMainProject;
  ScopeRadioGroup.Items[2]:=lisFRIinProjectPackageOwningCurrentUnit;
  ScopeRadioGroup.Items[3]:=lisFRIinAllOpenPackagesAndProjects;

  LoadFromConfig;
end;

procedure TFindRenameIdentifierDialog.RenameCheckBoxChange(Sender: TObject);
begin
  UpdateRename;
end;

procedure TFindRenameIdentifierDialog.UpdateRename;
begin
  RenameCheckBox.Enabled:=AllowRename;
  NewEdit.Enabled:=RenameCheckBox.Checked and RenameCheckBox.Enabled;
  if NewEdit.Enabled then
    FindOrRenameButton.Caption:=lisFRIRenameAllReferences
  else
    FindOrRenameButton.Caption:=lisFRIFindReferences;
end;

procedure TFindRenameIdentifierDialog.SetAllowRename(const AValue: boolean);
begin
  if FAllowRename=AValue then exit;
  FAllowRename:=AValue;
  UpdateRename;
end;

procedure TFindRenameIdentifierDialog.FindOrRenameButtonClick(Sender: TObject);
var
  NewIdentifier: String;
begin
  NewIdentifier:=NewEdit.Text;
  if (NewIdentifier='') or (not IsValidIdent(NewIdentifier)) then begin
    MessageDlg(lisFRIInvalidIdentifier,
      Format(lisSVUOisNotAValidIdentifier, ['"', NewIdentifier, '"']), mtError,
        [mbCancel], 0);
    exit;
  end;
  ModalResult:=mrOk;
end;

procedure TFindRenameIdentifierDialog.FindRenameIdentifierDialogClose(
  Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveToConfig;
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TFindRenameIdentifierDialog.LoadFromConfig;
begin
  LoadFromOptions(MiscellaneousOptions.FindRenameIdentifierOptions);
end;

procedure TFindRenameIdentifierDialog.SaveToConfig;
begin
  SaveToOptions(MiscellaneousOptions.FindRenameIdentifierOptions);
end;

procedure TFindRenameIdentifierDialog.LoadFromOptions(
  Options: TFindRenameIdentifierOptions);
begin
  RenameCheckBox.Checked:=Options.Rename;
  ExtraFilesEdit.Text:=StringListToText(Options.ExtraFiles,';',true);
  NewEdit.Text:=Options.RenameTo;
  ScopeCommentsCheckBox.Checked:=Options.SearchInComments;
  case Options.Scope of
  frCurrentUnit: ScopeRadioGroup.ItemIndex:=0;
  frProject: ScopeRadioGroup.ItemIndex:=1;
  frOwnerProjectPackage: ScopeRadioGroup.ItemIndex:=2;
  else
    ScopeRadioGroup.ItemIndex:=3;
  end;
  UpdateRename;
end;

procedure TFindRenameIdentifierDialog.SaveToOptions(
  Options: TFindRenameIdentifierOptions);
begin
  Options.Rename:=RenameCheckBox.Checked;
  SplitString(ExtraFilesEdit.Text,';',Options.ExtraFiles,true);
  Options.RenameTo:=NewEdit.Text;
  Options.SearchInComments:=ScopeCommentsCheckBox.Checked;
  case ScopeRadioGroup.ItemIndex of
  0: Options.Scope:=frCurrentUnit;
  1: Options.Scope:=frProject;
  2: Options.Scope:=frOwnerProjectPackage;
  else Options.Scope:=frAllOpenProjectsAndPackages;
  end;
end;

procedure TFindRenameIdentifierDialog.SetIdentifier(
  const NewIdentifierFilename: string; const NewIdentifierPosition: TPoint);
var
  s: String;
  ACodeBuffer: TCodeBuffer;
  ListOfCodeBuffer: TFPList;
  i: Integer;
  CurCode: TCodeBuffer;
  NewIdentifier: String;
begin
  FIdentifierFilename:=NewIdentifierFilename;
  FIdentifierPosition:=NewIdentifierPosition;
  CurrentListBox.Items.Clear;
  s:=IdentifierFilename
     +'('+IntToStr(IdentifierPosition.Y)+','+IntToStr(IdentifierPosition.X)+')';
  CurrentListBox.Items.Add(s);
  LoadCodeBuffer(ACodeBuffer,IdentifierFileName,[lbfCheckIfText]);
  if ACodeBuffer<>nil then begin
    CodeToolBoss.GetIncludeCodeChain(ACodeBuffer,true,ListOfCodeBuffer);
    if ListOfCodeBuffer<>nil then begin
      for i:=0 to ListOfCodeBuffer.Count-1 do begin
        CurCode:=TCodeBuffer(ListOfCodeBuffer[i]);
        if CurCode=ACodeBuffer then break;
        s:=CurCode.Filename;
        CurrentListBox.Items.Insert(0,s);
      end;
      ListOfCodeBuffer.Free;
    end;
    if CodeToolBoss.GetIdentifierAt(ACodeBuffer,
      NewIdentifierPosition.X,NewIdentifierPosition.Y,NewIdentifier) then
    begin
      CurrentGroupBox.Caption:=Format(lisFRIIdentifier, [NewIdentifier]);
      NewEdit.Text:=NewIdentifier;
    end;
  end;
end;

initialization
  {$I findrenameidentifier.lrs}

end.


