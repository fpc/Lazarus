{ /***************************************************************************
                     projectopts.pp  -  Lazarus IDE unit
                     -----------------------------------

 ***************************************************************************/

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
    Project options dialog

}
unit ProjectOpts;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLIntf, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls,
  LResources, Buttons, Dialogs, CodeToolManager,
  ProjectIntf, IDEWindowIntf,
  LazarusIDEStrConsts, IDEOptionDefs, Project;

type
  TProjectOptionsDialog = class(TForm)
    NoteBook: TNoteBook;

    // Application
    AppSettingsGroupBox: TGroupBox;
    TitleLabel: TLabel;
    TitleEdit: TEdit;
    OutputSettingsGroupBox: TGroupBox;
    TargetFileLabel: TLabel;
    TargetFileEdit: TEdit;

    // Forms
    FormsAutoCreatedLabel: TLabel;
    FormsAutoCreatedListBox: TListBox;
    FormsAvailFormsLabel: TLabel;
    FormsAvailFormsListBox: TListBox;
    FormsAddToAutoCreatedFormsBtn: TSpeedButton;
    FormsRemoveFromAutoCreatedFormsBtn: TSpeedButton;
    FormsMoveAutoCreatedFormUpBtn: TSpeedButton;
    FormsMoveAutoCreatedFormDownBtn: TSpeedButton;
    FormsAutoCreateNewFormsCheckBox: TCheckBox;
    
    // Misc
    SaveClosedUnitInfoCheckBox: TCheckBox;
    SaveOnlyProjectUnitInfoCheckBox: TCheckBox;
    MainUnitIsPascalSourceCheckBox: TCheckBox;
    MainUnitHasUsesSectionForAllUnitsCheckBox: TCheckBox;
    MainUnitHasCreateFormStatementsCheckBox: TCheckBox;
    MainUnitHasTitleStatementCheckBox: TCheckBox;
    RunnableCheckBox: TCheckBox;

    // buttons at bottom
    OkButton: TBitBtn;
    CancelButton: TBitBtn;
    
    procedure OnFormsPageResize(Sender: TObject);
    procedure ProjectOptionsClose(Sender: TObject;
                                  var CloseAction: TCloseAction);
    procedure ProjectOptionsResize(Sender: TObject);
    procedure FormsAddToAutoCreatedFormsBtnClick(Sender: TObject);
    procedure FormsRemoveFromAutoCreatedFormsBtnClick(Sender: TObject);
    procedure FormsMoveAutoCreatedFormUpBtnClick(Sender: TObject);
    procedure FormsMoveAutoCreatedFormDownBtnClick(Sender: TObject);
  private
    FProject: TProject;
    procedure SetProject(AProject: TProject);
    procedure SetupApplicationPage;
    procedure SetupFormsPage;
    procedure SetupMiscPage;
    procedure FillAutoCreateFormsListbox;
    procedure FillAvailFormsListBox;
    function IndexOfAutoCreateForm(FormName: string): integer;
    function FirstAutoCreateFormSelected: integer;
    function FirstAvailFormSelected: integer;
    procedure SelectOnlyThisAutoCreateForm(Index: integer);
    function GetAutoCreatedFormsList: TStrings;
    function GetProjectTitle: string;
    function SetAutoCreateForms: boolean;
    function SetProjectTitle: boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    property Project: TProject read FProject write SetProject;
  end;

function ShowProjectOptionsDialog(AProject: TProject): TModalResult;


implementation


function ShowProjectOptionsDialog(AProject: TProject): TModalResult;
begin
  with TProjectOptionsDialog.Create(nil) do try
    Project:= AProject;
    Result:= ShowModal;
  finally
    Free;
  end;
end;


{ TProjectOptionsDialog }

constructor TProjectOptionsDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Width:=430;
  Height:=375;
  Position:= poScreenCenter;
  OnResize:= @ProjectOptionsResize;
  OnClose:= @ProjectOptionsClose;
  Caption:=dlgProjectOptions;

  NoteBook:=TNoteBook.Create(Self);
  with NoteBook do begin
    Parent:=Self;
    SetBounds(0, 0, Self.ClientWidth, Self.ClientHeight - 33);
    Anchors:= [akTop, akLeft, akRight, akBottom];
    if PageCount > 0 then
      Pages[0]:=dlgPOApplication
    else
      Pages.Add(dlgPOApplication);
    Pages.Add(dlgPOFroms);
    Pages.Add(dlgPOMisc);
    PageIndex:=0;
  end;

  SetupFormsPage;
  SetupApplicationPage;
  SetupMiscPage;

  CancelButton:=TBitBtn.Create(Self);
  with CancelButton do begin
    Parent:= Self;
    Left:= Self.ClientWidth - Width - 4;
    Top:= Self.ClientHeight - Height - 4;
    Kind:= bkCancel;
    Anchors:= [akBottom, akRight];
  end;

  OkButton:=TBitBtn.Create(Self);
  with OkButton do begin
    Parent:= Self;
    Left:= CancelButton.Left - Width - 4;
    Top:= CancelButton.Top;
    Kind:= bkOk;
    Anchors:= [akBottom, akRight];
  end;

  IDEDialogLayoutList.ApplyLayout(Self, 430, 375);
end;

procedure TProjectOptionsDialog.SetupApplicationPage;
var MaxX:integer;
begin
  MaxX:=ClientWidth-5;

  AppSettingsGroupBox:=TGroupBox.Create(Self);
  with AppSettingsGroupBox do begin
    Parent:=NoteBook.Page[0];
    Left:=5;
    Top:=5;
    Width:=MaxX-2*Left;
    Height:=60;
    Anchors:= [akTop, akLeft, akRight];
    Caption:=dlgApplicationSettings;
  end;

  TitleLabel:=TLabel.Create(Self);
  with TitleLabel do begin
    Parent:=AppSettingsGroupBox;
    Left:= 5;
    Top:= 4;
    Width:= 50;
    Height:= 23;
    Caption:=dlgPOTitle;
  end;

  TitleEdit:=TEdit.Create(Self);
  with TitleEdit do begin
    Parent:=AppSettingsGroupBox;
    Left:=TitleLabel.Left+TitleLabel.Width+2;
    Top:=TitleLabel.Top - 1;
    Width:=AppSettingsGroupBox.ClientWidth-Left-10;
    Anchors:= [akTop, akLeft, akRight];
    Text:='';
  end;

  OutputSettingsGroupBox:=TGroupBox.Create(Self);
  with OutputSettingsGroupBox do begin
    Parent:=NoteBook.Page[0];
    Left:=AppSettingsGroupBox.Left;
    Top:=AppSettingsGroupBox.Top+AppSettingsGroupBox.Height+5;
    Width:=AppSettingsGroupBox.Width;
    Height:=60;
    Anchors:= [akTop, akLeft, akRight];
    Caption:=dlgPOOutputSettings;
  end;

  TargetFileLabel:=TLabel.Create(Self);
  with TargetFileLabel do begin
    Parent:=OutputSettingsGroupBox;
    Left:= 5;
    Top:= 4;
    Width:= 100;
    Height:= 23;
    Caption:=dlgPOTargetFileName;
  end;

  TargetFileEdit:=TEdit.Create(Self);
  with TargetFileEdit do begin
    Parent:=OutputSettingsGroupBox;
    Left:=TargetFileLabel.Left+TargetFileLabel.Width+5;
    Top:=TargetFileLabel.Top - 1;
    Width:=OutputSettingsGroupBox.Width-Left-10;
    Anchors:= [akTop, akLeft, akRight];
    Text:='';
  end;
end;

procedure TProjectOptionsDialog.SetupFormsPage;
begin
  FormsAutoCreatedLabel:=TLabel.Create(Self);
  with FormsAutoCreatedLabel do begin
    Parent:=NoteBook.Page[1];
    SetBounds(40,4,Width,Height);
    Caption:=dlgAutoCreateForms;
    AutoSize:=true;
  end;

  FormsAutoCreatedListBox:=TListBox.Create(Self);
  with FormsAutoCreatedListBox do begin
    Parent:=NoteBook.Page[1];
    SetBounds(40,28,Width,Height);
    MultiSelect:=true;
  end;

  FormsAvailFormsLabel:=TLabel.Create(Self);
  with FormsAvailFormsLabel do begin
    Parent:=NoteBook.Page[1];
    Left:=FormsAutoCreatedListBox.Left+FormsAutoCreatedListBox.Width+45;
    Top:=FormsAutoCreatedLabel.Top;
    Caption:=dlgAvailableForms;
    AutoSize:=true;
  end;

  FormsAvailFormsListBox:=TListBox.Create(Self);
  with FormsAvailFormsListBox do begin
    Parent:=NoteBook.Page[1];
    Left:=FormsAvailFormsLabel.Left;
    Top:=FormsAutoCreatedListBox.Top;
    Width:=FormsAutoCreatedListBox.Width;
    Height:=FormsAutoCreatedListBox.Height;
    MultiSelect:=true;
  end;

  FormsAddToAutoCreatedFormsBtn:=TSpeedButton.Create(Self);
  with FormsAddToAutoCreatedFormsBtn do begin
    Parent:=NoteBook.Page[1];
    Left:=FormsAutoCreatedListBox.Left+FormsAutoCreatedListBox.Width+10;
    Top:=FormsAutoCreatedListBox.Top+80;
    Width:=25;
    Height:=25;
    Glyph.LoadFromLazarusResource('leftarrow');
    OnClick:=@FormsAddToAutoCreatedFormsBtnClick;
  end;

  FormsRemoveFromAutoCreatedFormsBtn:=TSpeedButton.Create(Self);
  with FormsRemoveFromAutoCreatedFormsBtn do begin
    Parent:=NoteBook.Page[1];
    Left:=FormsAddToAutoCreatedFormsBtn.Left;
    Top:=FormsAddToAutoCreatedFormsBtn.Top
        +FormsAddToAutoCreatedFormsBtn.Height+10;
    Width:=25;
    Height:=25;
    Glyph.LoadFromLazarusResource('rightarrow');
    OnClick:=@FormsRemoveFromAutoCreatedFormsBtnClick;
  end;

  FormsMoveAutoCreatedFormUpBtn:=TSpeedButton.Create(Self);
  with FormsMoveAutoCreatedFormUpBtn do begin
    Parent:=NoteBook.Page[1];
    Left:=FormsAutoCreatedListBox.Left-35;
    Top:=FormsAutoCreatedListBox.Top+80;
    Width:=25;
    Height:=25;
    Glyph.LoadFromLazarusResource('uparrow');
    OnClick:=@FormsMoveAutoCreatedFormUpBtnClick;
  end;

  FormsMoveAutoCreatedFormDownBtn:=TSpeedButton.Create(Self);
  with FormsMoveAutoCreatedFormDownBtn do begin
    Parent:=NoteBook.Page[1];
    Left:=FormsMoveAutoCreatedFormUpBtn.Left;
    Top:=FormsMoveAutoCreatedFormUpBtn.Top
        +FormsMoveAutoCreatedFormUpBtn.Height+10;
    Width:=25;
    Height:=25;
    Glyph.LoadFromLazarusResource('downarrow');
    OnClick:=@FormsMoveAutoCreatedFormDownBtnClick;
  end;

  FormsAutoCreateNewFormsCheckBox:=TCheckBox.Create(Self);
  with FormsAutoCreateNewFormsCheckBox do begin
    Parent:=NoteBook.Page[1];
    Left:=FormsAutoCreatedListBox.Left+5;
    Top:=FormsAutoCreatedListBox.Top+FormsAutoCreatedListBox.Height+5;
    Caption:=dlgAutoCreateNewForms;
  end;

  NoteBook.Page[1].OnResize:=@OnFormsPageResize;
end;

procedure TProjectOptionsDialog.SetupMiscPage;
var
  w: Integer;
  x: Integer;
  y: Integer;
  NewParent: TPage;
begin
  w:=350;
  x:=10;
  y:=10;
  NewParent:=NoteBook.Page[2];

  SaveClosedUnitInfoCheckBox:=TCheckBox.Create(Self);
  with SaveClosedUnitInfoCheckBox do begin
    Parent:= NewParent;
    Caption:= dlgSaveEditorInfo;
    SetBounds(x,y,w,Height);
    inc(y,Height+5);
  end;
  
  SaveOnlyProjectUnitInfoCheckBox:=TCheckBox.Create(Self);
  with SaveOnlyProjectUnitInfoCheckBox do begin
    Parent:= NewParent;
    Caption:= dlgSaveEditorInfoProject;
    SetBounds(x,y,w,Height);
    inc(y,Height+5);
  end;

  inc(y,10);

  MainUnitIsPascalSourceCheckBox:=TCheckBox.Create(Self);
  with MainUnitIsPascalSourceCheckBox do begin
    Parent:= NewParent;
    Caption:= lisMainUnitIsPascalSource;
    SetBounds(x,y,w,Height);
    inc(y,Height+5);
  end;
  
  MainUnitHasUsesSectionForAllUnitsCheckBox:=TCheckBox.Create(Self);
  with MainUnitHasUsesSectionForAllUnitsCheckBox do begin
    Parent:= NewParent;
    Caption:= lisMainUnitHasUsesSectionContainingAllUnitsOfProject;
    SetBounds(x,y,w,Height);
    inc(y,Height+5);
  end;

  MainUnitHasCreateFormStatementsCheckBox:=TCheckBox.Create(Self);
  with MainUnitHasCreateFormStatementsCheckBox do begin
    Parent:= NewParent;
    Caption:= lisMainUnitHasApplicationCreateFormStatements;
    SetBounds(x,y,w,Height);
    inc(y,Height+5);
  end;

  MainUnitHasTitleStatementCheckBox:=TCheckBox.Create(Self);
  with MainUnitHasTitleStatementCheckBox do begin
    Parent:= NewParent;
    Caption:= lisMainUnitHasApplicationTitleStatements;
    SetBounds(x,y,w,Height);
    inc(y,Height+5);
  end;

  RunnableCheckBox:=TCheckBox.Create(Self);
  with RunnableCheckBox do begin
    Parent:= NewParent;
    Caption:= lisProjectIsRunnable;
    SetBounds(x,y,w,Height);
    inc(y,Height+5);
  end;
end;

procedure TProjectOptionsDialog.SetProject(AProject: TProject);
begin
  FProject:=AProject;
  if AProject=nil then exit;
  
  with AProject do begin
    TitleEdit.Text:=Title;
    TargetFileEdit.Text:=TargetFilename;
  end;
  FillAutoCreateFormsListbox;
  FillAvailFormsListBox;
  
  FormsAutoCreateNewFormsCheckBox.Checked:=Project.AutoCreateForms;

  SaveClosedUnitInfoCheckBox.Checked:=(pfSaveClosedUnits in AProject.Flags);
  SaveOnlyProjectUnitInfoCheckBox.Checked:=
    (pfSaveOnlyProjectUnits in AProject.Flags);

  MainUnitIsPascalSourceCheckBox.Checked:=
    (pfMainUnitIsPascalSource in AProject.Flags);
  MainUnitHasUsesSectionForAllUnitsCheckBox.Checked:=
    (pfMainUnitHasUsesSectionForAllUnits in AProject.Flags);
  MainUnitHasCreateFormStatementsCheckBox.Checked:=
    (pfMainUnitHasCreateFormStatements in AProject.Flags);
  MainUnitHasTitleStatementCheckBox.Checked:=
    (pfMainUnitHasTitleStatement in AProject.Flags);
  RunnableCheckBox.Checked:=(pfRunnable in AProject.Flags);
end;

procedure TProjectOptionsDialog.ProjectOptionsClose(Sender: TObject;
  var CloseAction: TCloseAction);
var
  NewFlags: TProjectFlags;

  procedure SetProjectFlag(AFlag: TProjectFlag; AValue: boolean);
  begin
    if AValue then
      Include(NewFlags,AFlag)
    else
      Exclude(NewFlags,AFlag);
  end;

begin
  if ModalResult = mrOk then begin  

    with Project do begin
      Title:=TitleEdit.Text;
      TargetFilename:=TargetFileEdit.Text;
    end;
  
    // flags
    NewFlags:=Project.Flags;
    SetProjectFlag(pfSaveClosedUnits,SaveClosedUnitInfoCheckBox.Checked);
    SetProjectFlag(pfSaveOnlyProjectUnits,
                   SaveOnlyProjectUnitInfoCheckBox.Checked);
    SetProjectFlag(pfMainUnitIsPascalSource,
                   MainUnitIsPascalSourceCheckBox.Checked);
    SetProjectFlag(pfMainUnitHasUsesSectionForAllUnits,
                   MainUnitHasUsesSectionForAllUnitsCheckBox.Checked);
    SetProjectFlag(pfMainUnitHasCreateFormStatements,
                   MainUnitHasCreateFormStatementsCheckBox.Checked);
    SetProjectFlag(pfMainUnitHasTitleStatement,
                   MainUnitHasTitleStatementCheckBox.Checked);
    SetProjectFlag(pfRunnable,RunnableCheckBox.Checked);
    Project.Flags:=NewFlags;
    
    Project.AutoCreateForms:=FormsAutoCreateNewFormsCheckBox.Checked;
    
    SetAutoCreateForms;
    SetProjectTitle;
  end;
    
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TProjectOptionsDialog.OnFormsPageResize(Sender: TObject);
var MaxX, MaxY, ListBoxWidth, ListBoxHeight: integer;
begin
  MaxX:=ClientWidth-8;
  MaxY:=ClientHeight-75;
  ListBoxWidth:=(MaxX-95) div 2;
  ListBoxHeight:=MaxY-70;

  with FormsAutoCreatedListBox do begin
    Left:=FormsAutoCreatedLabel.Left;
    Top:=FormsAutoCreatedLabel.Top+FormsAutoCreatedLabel.Height+3;
    Width:=ListBoxWidth;
    Height:=ListBoxHeight;
  end;

  with FormsAvailFormsLabel do begin
    Left:=FormsAutoCreatedListBox.Left+FormsAutoCreatedListBox.Width+45;
    Top:=FormsAutoCreatedLabel.Top;
    Width:=FormsAutoCreatedLabel.Width;
    Height:=FormsAutoCreatedLabel.Height;
  end;

  with FormsAvailFormsListBox do begin
    Left:=FormsAvailFormsLabel.Left;
    Top:=FormsAutoCreatedListBox.Top;
    Width:=FormsAutoCreatedListBox.Width;
    Height:=FormsAutoCreatedListBox.Height;
  end;

  with FormsAddToAutoCreatedFormsBtn do begin
    Left:=FormsAutoCreatedListBox.Left+FormsAutoCreatedListBox.Width+10;
    Top:=FormsAutoCreatedListBox.Top+80;
    Width:=25;
    Height:=25;
  end;

  with FormsRemoveFromAutoCreatedFormsBtn do begin
    Left:=FormsAddToAutoCreatedFormsBtn.Left;
    Top:=FormsAddToAutoCreatedFormsBtn.Top
        +FormsAddToAutoCreatedFormsBtn.Height+10;
    Width:=25;
    Height:=25;
  end;

  with FormsMoveAutoCreatedFormUpBtn do begin
    Left:=FormsAutoCreatedListBox.Left-35;
    Top:=FormsAutoCreatedListBox.Top+80;
    Width:=25;
    Height:=25;
  end;

  with FormsMoveAutoCreatedFormDownBtn do begin
    Left:=FormsMoveAutoCreatedFormUpBtn.Left;
    Top:=FormsMoveAutoCreatedFormUpBtn.Top
        +FormsMoveAutoCreatedFormUpBtn.Height+10;
    Width:=25;
    Height:=25;
  end;

  with FormsAutoCreateNewFormsCheckBox do begin
    Left:=FormsMoveAutoCreatedFormUpBtn.Left;
    Top:=FormsAutoCreatedListBox.Top+FormsAutoCreatedListBox.Height+5;
    Width:=Parent.ClientWidth-Left;
    Height:=25;
  end;
end;

function TProjectOptionsDialog.GetAutoCreatedFormsList: TStrings;
var i, j: integer;
begin
  if (FProject<>nil) and (FProject.MainUnitID>=0) then begin
    Result:=CodeToolBoss.ListAllCreateFormStatements(
         FProject.MainUnitInfo.Source);
    if Result<>nil then begin
      // shorten lines of type 'FormName:TFormName' to simply 'FormName'
      for i:=0 to Result.Count-1 do begin
        j:=Pos(':',Result[i]);
        if j>0 then begin
          if 't'+lowercase(copy(Result[i],1,j-1))
            =lowercase(copy(Result[i],j+1,length(Result[i])-j)) then
          begin
            Result[i]:=copy(Result[i],1,j-1);
          end;
        end;
      end;
    end;
  end else begin
    Result:=nil;
  end;  
end;

function TProjectOptionsDialog.GetProjectTitle: string;
begin
  Result:='';
  if (FProject=nil) or (FProject.MainUnitID<0) then begin
    exit;
  end;
  CodeToolBoss.GetApplicationTitleStatement(FProject.MainUnitInfo.Source,Result);
end;

procedure TProjectOptionsDialog.FillAutoCreateFormsListbox;
var sl: TStrings;
begin
  sl:= GetAutoCreatedFormsList;
  FormsAutoCreatedListBox.Items.BeginUpdate;
  FormsAutoCreatedListBox.Items.Clear;
  if sl <> nil then begin
    FormsAutoCreatedListBox.Items.Assign(sl);
    sl.Free;
  end;  
  FormsAutoCreatedListBox.Items.EndUpdate;
end;

procedure TProjectOptionsDialog.FillAvailFormsListBox;
var sl: TStringList;
  i: integer;
begin
  FormsAvailFormsListBox.Items.BeginUpdate;
  FormsAvailFormsListBox.Items.Clear;

  if (FProject<>nil) then begin
    sl:= TStringList.Create;
    try
      for i:= 0 to FProject.UnitCount - 1 do begin
        if (FProject.Units[i].IsPartOfProject)
          and (FProject.Units[i].ComponentName <> '')
          and (IndexOfAutoCreateForm(FProject.Units[i].ComponentName) < 0) then
          sl.Add(FProject.Units[i].ComponentName);
      end;
      sl.Sort;
      FormsAvailFormsListBox.Items.Assign(sl);
    finally
      sl.Free;
    end;    
  end;
  FormsAvailFormsListBox.Items.EndUpdate;
end;

function TProjectOptionsDialog.IndexOfAutoCreateForm(FormName: string): integer;
var p: integer;
begin
  p:=Pos(':',FormName);
  if p>0 then FormName:=copy(FormName,1,p-1);
  Result:=FormsAutoCreatedListBox.Items.Count-1;
  while (Result>=0) do begin
    p:=Pos(':',FormsAutoCreatedListBox.Items[Result]);
    if p<1 then p:=length(FormsAutoCreatedListBox.Items[Result])+1;
    if AnsiCompareText(copy(FormsAutoCreatedListBox.Items[Result],1,p-1)
                      ,FormName)=0 then exit;
    dec(Result);
  end;
end;

function TProjectOptionsDialog.FirstAutoCreateFormSelected: integer;
begin
  Result:=0;
  while (Result<FormsAutoCreatedListBox.Items.Count)
  and (not FormsAutoCreatedListBox.Selected[Result]) do
    inc(Result);
  if Result=FormsAutoCreatedListBox.Items.Count then Result:=-1;
end;

function TProjectOptionsDialog.FirstAvailFormSelected: integer;
begin
  Result:=0;
  while (Result<FormsAvailFormsListBox.Items.Count)
  and (not FormsAvailFormsListBox.Selected[Result]) do
    inc(Result);
  if Result=FormsAvailFormsListBox.Items.Count then Result:=-1;
end;

procedure TProjectOptionsDialog.FormsAddToAutoCreatedFormsBtnClick(
  Sender: TObject);
var i: integer;
  NewFormName: string;
begin
  FormsAutoCreatedListBox.Items.BeginUpdate;
  with FormsAvailFormsListBox do begin
    Items.BeginUpdate;
    i:=0;
    while i<Items.Count do begin
      if Selected[i] then begin
        NewFormName:=Items[i];
        Items.Delete(i);
        FormsAutoCreatedListBox.Items.Add(NewFormName);
      end else
        inc(i);
    end;
    Items.EndUpdate;
  end;
  FormsAutoCreatedListBox.Items.EndUpdate;
end;

procedure TProjectOptionsDialog.FormsRemoveFromAutoCreatedFormsBtnClick(
  Sender: TObject);
var i, NewPos, cmp: integer;
  OldFormName: string;
begin
  FormsAutoCreatedListBox.Items.BeginUpdate;
  FormsAvailFormsListBox.Items.BeginUpdate;
  i:=0;
  while i<FormsAutoCreatedListBox.Items.Count do begin
    if FormsAutoCreatedListBox.Selected[i] then begin
      OldFormName:=FormsAutoCreatedListBox.Items[i];
      FormsAutoCreatedListBox.Items.Delete(i);
      NewPos:=0;
      cmp:=1;
      while (NewPos<FormsAvailFormsListBox.Items.Count) do begin
        cmp:=AnsiCompareText(FormsAvailFormsListBox.Items[NewPos],OldFormName);
        if cmp<0 then
          inc(NewPos)
        else
          break;
      end;
      if cmp=0 then continue;
      FormsAvailFormsListBox.Items.Insert(NewPos,OldFormName);
    end else
      inc(i);
  end;
  FormsAvailFormsListBox.Items.EndUpdate;
  FormsAutoCreatedListBox.Items.EndUpdate;
end;

procedure TProjectOptionsDialog.FormsMoveAutoCreatedFormUpBtnClick(
  Sender: TObject);
var i: integer;
  h: string;
begin
  i:=FirstAutoCreateFormSelected;
  if i<1 then exit;
  with FormsAutoCreatedListBox do begin
    Items.BeginUpdate;
    h:=Items[i];
    Items[i]:=Items[i-1];
    Items[i-1]:=h;
    Items.EndUpdate;
  end;
  SelectOnlyThisAutoCreateForm(i-1);
end;

procedure TProjectOptionsDialog.FormsMoveAutoCreatedFormDownBtnClick(
  Sender: TObject);
var i: integer;
  h: string;
begin
  i:=FirstAutoCreateFormSelected;
  if (i<0) or (i>=FormsAutoCreatedListBox.Items.Count-1) then exit;
  with FormsAutoCreatedListBox do begin
    Items.BeginUpdate;
    h:=Items[i];
    Items[i]:=Items[i+1];
    Items[i+1]:=h;
    Items.EndUpdate;
  end;
  SelectOnlyThisAutoCreateForm(i+1);
end;

procedure TProjectOptionsDialog.ProjectOptionsResize(Sender: TObject);
begin
end;

procedure TProjectOptionsDialog.SelectOnlyThisAutoCreateForm(
  Index: integer);
var i: integer;
begin
  with FormsAutoCreatedListBox do begin
    for i:=0 to Items.Count-1 do
      Selected[i]:= (i = Index);
  end;
end;

function TProjectOptionsDialog.SetAutoCreateForms: boolean;
var i: integer;
    OldList: TStrings;
begin
  Result:=true;
  if (Project.MainUnitID < 0)
  or (not (pfMainUnitHasUsesSectionForAllUnits in Project.Flags)) then
    exit;
  OldList:= GetAutoCreatedFormsList;
  if (OldList = nil) then exit;
  try
    if OldList.Count = FormsAutoCreatedListBox.Items.Count then begin

      { Just exit if the form list is the same }
      i:= OldList.Count - 1;
      while (i >= 0) 
        and (AnsiCompareText(OldList[i], FormsAutoCreatedListBox.Items[i])=0) do
        dec(i);
      if i < 0 then Exit;
    end;

    if not CodeToolBoss.SetAllCreateFromStatements(
      Project.MainUnitInfo.Source, FormsAutoCreatedListBox.Items) then
    begin
      MessageDlg(lisProjOptsError,
        Format(lisProjOptsUnableToChangeTheAutoCreateFormList, [LineEnding]),
          mtWarning, [mbCancel], 0);
      Result:=false;
      exit;
    end;
  finally
    OldList.Free;
  end;
end;

function TProjectOptionsDialog.SetProjectTitle: boolean;
var
  OldTitle: String;
begin
  Result:=true;
  if (Project.MainUnitID < 0)
  or (not (pfMainUnitHasTitleStatement in Project.Flags)) then
    exit;
  OldTitle:=GetProjectTitle;
  if (OldTitle='') and Project.TitleIsDefault then exit;
  
  if (OldTitle<>Project.Title) and (not Project.TitleIsDefault) then begin
    // set Application.Title:= statement
    if not CodeToolBoss.SetApplicationTitleStatement(
      Project.MainUnitInfo.Source, Project.Title) then
    begin
      MessageDlg(lisProjOptsError,
        'Unable to change project title in source.'#13
        +CodeToolBoss.ErrorMessage,
        mtWarning, [mbCancel], 0);
      Result:=false;
    end;
  end;
  
  if (OldTitle<>'') and Project.TitleIsDefault then begin
    // delete title
    if not CodeToolBoss.RemoveApplicationTitleStatement(
      Project.MainUnitInfo.Source) then
    begin
      MessageDlg(lisProjOptsError,
        'Unable to remove project title from source.'#13
        +CodeToolBoss.ErrorMessage,
        mtWarning, [mbCancel], 0);
      Result:=false;
    end;
  end;
end;

initialization

{$I projectopts.lrs}

end.
