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
  CodeAtom, CodeCache, CodeToolManager,
  LazarusIDEStrConsts, IDEProcs, IDEOptionDefs, MiscOptions, DialogProcs,
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
    procedure SetIdentifier(const NewIdentifierFilename: string;
                            const NewIdentifierPosition: TPoint);
    property IdentifierFilename: string read FIdentifierFilename;
    property IdentifierPosition: TPoint read FIdentifierPosition;
    property AllowRename: boolean read FAllowRename write SetAllowRename;
  end;


function ShowFindRenameIdentifierDialog(const Filename: string;
  const Position: TPoint; AllowRename: boolean): TModalResult;
procedure ShowReferences(DeclarationCode: TCodeBuffer;
  const DeclarationCaretXY: TPoint; TargetCode: TCodeBuffer;
  ListOfPCodeXYPosition: TList);
  

implementation


function ShowFindRenameIdentifierDialog(const Filename: string;
  const Position: TPoint; AllowRename: boolean): TModalResult;
var
  FindRenameIdentifierDialog: TFindRenameIdentifierDialog;
begin
  FindRenameIdentifierDialog:=TFindRenameIdentifierDialog.Create(nil);
  try
    FindRenameIdentifierDialog.LoadFromConfig;
    FindRenameIdentifierDialog.SetIdentifier(Filename,Position);
    FindRenameIdentifierDialog.AllowRename:=AllowRename;
    Result:=FindRenameIdentifierDialog.ShowModal;
  finally
    FindRenameIdentifierDialog.Free;
  end;
end;

procedure ShowReferences(DeclarationCode: TCodeBuffer;
  const DeclarationCaretXY: TPoint; TargetCode: TCodeBuffer;
  ListOfPCodeXYPosition: TList);
var
  Identifier: string;
  SearchPageIndex: LongInt;
  i: Integer;
  CodePos: PCodeXYPosition;
  CurLine: String;
  TrimmedLine: String;
  TrimCnt: Integer;
begin
  CodeToolBoss.GetIdentifierAt(DeclarationCode,
    DeclarationCaretXY.X,DeclarationCaretXY.Y,Identifier);
  SearchPageIndex:=SearchResultsView.AddResult(
    'References of '+Identifier,
    Identifier,
    ExtractFilePath(TargetCode.Filename),
    '*.pas;*.pp;*.inc',
    [fifWholeWord,fifSearchDirectories]);

  SearchResultsView.BeginUpdate(SearchPageIndex);
  SearchResultsView.Items[SearchPageIndex].Clear;
  if (ListOfPCodeXYPosition<>nil) then
    for i:=0 to ListOfPCodeXYPosition.Count-1 do begin
      CodePos:=PCodeXYPosition(ListOfPCodeXYPosition[i]);
      CurLine:=TrimRight(CodePos^.Code.GetLine(CodePos^.Y-1));
      TrimmedLine:=Trim(CurLine);
      TrimCnt:=length(CurLine)-length(TrimmedLine);
      //debugln('ShowReferences x=',dbgs(CodePos^.x),' y=',dbgs(CodePos^.y),' ',CurLine);
      SearchResultsView.AddMatch(SearchPageIndex,
                                 TargetCode.Filename,
                                 Point(CodePos^.X,CodePos^.Y),
                                 TrimmedLine,
                                 CodePos^.X-TrimCnt, length(Identifier));
    end;
  SearchResultsView.EndUpdate(SearchPageIndex);
  SearchResultsView.ShowOnTop;
end;

{ TFindRenameIdentifierDialog }

procedure TFindRenameIdentifierDialog.FindRenameIdentifierDialogCreate(
  Sender: TObject);
begin
  IDEDialogLayoutList.ApplyLayout(Self,450,400);

  Caption:='Find or Rename Identifier';
  CancelButton.Caption:='Cancel';
  CurrentGroupBox.Caption:='Identifier';
  ExtraFilesGroupBox.Caption:=
                    'Additional files to search (e.g. /path/*.pas;/path2/*.pp)';
  FindOrRenameButton.Caption:='Find References';
  NewGroupBox.Caption:='Rename to';
  RenameCheckBox.Caption:='Rename';
  ScopeCommentsCheckBox.Caption:='Search in comments too';
  ScopeGroupBox.Caption:='Search where';
  ScopeRadioGroup.Caption:='Scope';
  
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
    FindOrRenameButton.Caption:='Rename all References'
  else
    FindOrRenameButton.Caption:='Find References';
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
    MessageDlg('Invalid Identifier',
      '"'+NewIdentifier+'" is not a valid identifier.',mtError,[mbCancel],0);
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
var
  Options: TFindRenameIdentifierOptions;
begin
  Options:=MiscellaneousOptions.FindRenameIdentifierOptions;
  RenameCheckBox.Checked:=Options.Rename;
  ExtraFilesEdit.Text:=StringListToText(Options.ExtraFiles,';',true);
  NewEdit.Text:=Options.RenameTo;
  ScopeCommentsCheckBox.Checked:=Options.SearchInComments;
  case Options.Scope of
  frCurrentUnit: ScopeRadioGroup.ItemIndex:=0;
  frCurrentProjectPackage: ScopeRadioGroup.ItemIndex:=1;
  else
    ScopeRadioGroup.ItemIndex:=2;
  end;
  UpdateRename;
end;

procedure TFindRenameIdentifierDialog.SaveToConfig;
var
  Options: TFindRenameIdentifierOptions;
  ExtraFileList: TStringList;
begin
  Options:=MiscellaneousOptions.FindRenameIdentifierOptions;
  Options.Rename:=RenameCheckBox.Checked;
  ExtraFileList:=SplitString(ExtraFilesEdit.Text,';');
  Options.ExtraFiles.Assign(ExtraFileList);
  ExtraFileList.Free;
  Options.RenameTo:=NewEdit.Text;
  Options.SearchInComments:=ScopeCommentsCheckBox.Checked;
  case ScopeRadioGroup.ItemIndex of
  0: Options.Scope:=frCurrentUnit;
  1: Options.Scope:=frCurrentProjectPackage;
  else Options.Scope:=frAllOpenProjectsAndPackages;
  end;
end;

procedure TFindRenameIdentifierDialog.SetIdentifier(
  const NewIdentifierFilename: string; const NewIdentifierPosition: TPoint);
var
  s: String;
  ACodeBuffer: TCodeBuffer;
  ListOfCodeBuffer: TList;
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
    ListOfCodeBuffer:=nil;
    CodeToolBoss.GetIncludeCodeChain(ACodeBuffer,true,ListOfCodeBuffer);
    if ListOfCodeBuffer<>nil then begin
      for i:=0 to ListOfCodeBuffer.Count-1 do begin
        CurCode:=TCodeBuffer(ListOfCodeBuffer[i]);
        if CurCode=ACodeBuffer then break;
        s:=CurCode.Filename;
        CurrentListBox.Items.Insert(0,s);
      end;
    end;
    if CodeToolBoss.GetIdentifierAt(ACodeBuffer,
      NewIdentifierPosition.X,NewIdentifierPosition.Y,NewIdentifier) then
    begin
      CurrentGroupBox.Caption:='Identifier: '+NewIdentifier;
      NewEdit.Text:=NewIdentifier;
    end;
  end;
end;

initialization
  {$I findrenameidentifier.lrs}

end.


