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
unit ProjectOpts_new;

{$mode objfpc}{$H+}

interface

uses
  Arrow, Buttons, StdCtrls, SysUtils, LCLProc, Classes, CodeToolManager,
  Controls, Dialogs, LCLIntf, LResources, ExtCtrls, Forms, Graphics, Spin,
  FileUtil, IDEContextHelpEdit,
  IDEWindowIntf, ProjectIntf, IDEDialogs,
  IDEOptionDefs, LazarusIDEStrConsts, Project, IDEProcs, W32VersionInfo,
  VersionInfoAdditionalInfo, W32Manifest,
  OptionsEditorBase;

type

  { TProjectOptionsDialog }

  TProjectOptionsDialogNew = class(TOptionsEditorForm)
    EnableI18NCheckBox: TCheckBox;
    I18NGroupBox: TGroupBox;
    PoOutDirLabel: TLabel;

    Notebook: TNotebook;
    ApplicationPage:    TPage;
    FormsPage:    TPage;
    MiscPage:    TPage;
    LazDocPage:    TPage;
    i18nPage: TPage;
    POOutDirButton: TButton;
    POOutDirEdit: TEdit;
    SavePage: TPage;
    UseAppBundleCheckBox: TCheckBox;
    UseXPManifestCheckBox: TCheckBox;
    VersionInfoPage: TPage;

    AppSettingsGroupBox: TGroupBox;
    OutputSettingsGroupBox: TGroupBox;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    TitleLabel: TLabel;
    TitleEdit: TEdit;
    TargetFileLabel: TLabel;
    TargetFileEdit: TEdit;

    // Forms
    FormsAutoCreatedLabel: TLabel;
    FormsAutoCreatedListBox: TListBox;
    FormsAvailFormsLabel: TLabel;
    FormsAvailFormsListBox: TListBox;
    FormsAddToAutoCreatedFormsBtn: TArrow;
    FormsRemoveFromAutoCreatedFormsBtn: TArrow;
    FormsMoveAutoCreatedFormUpBtn: TArrow;
    FormsMoveAutoCreatedFormDownBtn: TArrow;
    FormsAutoCreateNewFormsCheckBox: TCheckBox;
    FormsMoveAutoCreatedFormsDownBtn: TArrow;

    // Misc
    MainUnitIsPascalSourceCheckBox: TCheckBox;
    MainUnitHasUsesSectionForAllUnitsCheckBox: TCheckBox;
    MainUnitHasCreateFormStatementsCheckBox: TCheckBox;
    MainUnitHasTitleStatementCheckBox: TCheckBox;
    RunnableCheckBox: TCheckBox;
    AlwaysBuildCheckBox: TCheckBox;

    // Lazdoc settings
    LazDocBrowseButton: TButton;
    LazDocPathEdit: TEdit;
    LazDocDeletePathButton: TButton;
    LazDocAddPathButton: TButton;
    LazDocPathsGroupBox: TGroupBox;
    LazDocListBox: TListBox;

    // Session
    SaveClosedUnitInfoCheckBox: TCheckBox;
    SaveOnlyProjectUnitInfoCheckBox: TCheckBox;
    SaveSessionLocationRadioGroup: TRadioGroup;

    // VersionInfo
    UseVersionInfoCheckBox: TCheckBox;
    VersionInfoGroupBox: TGroupBox;
    VersionLabel: TLabel;
    MajorRevisionLabel: TLabel;
    MinorRevisionLabel: TLabel;
    BuildLabel: TLabel;
    VersionSpinEdit: TSpinEdit;
    MajorRevisionSpinEdit: TSpinEdit;
    MinorRevisionSpinEdit: TSpinEdit;
    BuildEdit: TEdit;
    AutomaticallyIncreaseBuildCheckBox: TCheckBox;
    LanguageSettingsGroupBox: TGroupBox;
    LanguageSelectionLabel: TLabel;
    CharacterSetLabel: TLabel;
    LanguageSelectionComboBox: TComboBox;
    CharacterSetComboBox: TComboBox;
    OtherInfoGroupBox: TGroupBox;
    AdditionalInfoButton: TButton;
    DescriptionEdit: TEdit;
    CopyrightEdit: TEdit;
    DescriptionLabel: TLabel;
    CopyrightLabel: TLabel;
    AdditionalInfoForm: TVersionInfoAdditinalInfoForm;


    procedure AdditionalInfoButtonClick(Sender: TObject);
    procedure EnableI18NCheckBoxChange(Sender: TObject);
    procedure FormsPageResize(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure LazDocAddPathButtonClick(Sender: TObject);
    procedure LazDocBrowseButtonClick(Sender: TObject);
    procedure LazDocDeletePathButtonClick(Sender: TObject);
    procedure ProjectOptionsClose(Sender: TObject;
                                  var CloseAction: TCloseAction);
    procedure FormsAddToAutoCreatedFormsBtnClick(Sender: TObject);
    procedure FormsRemoveFromAutoCreatedFormsBtnClick(Sender: TObject);
    procedure FormsMoveAutoCreatedFormUpBtnClick(Sender: TObject);
    procedure FormsMoveAutoCreatedFormDownBtnClick(Sender: TObject);
    procedure POOutDirButtonClick(Sender: TObject);
    procedure UseVersionInfoCheckBoxChange(Sender: TObject);
  private
    FProject: TProject;
    procedure SetProject(AProject: TProject);
    procedure SetupApplicationPage(PageIndex: Integer);
    procedure SetupFormsPage(PageIndex: Integer);
    procedure SetupMiscPage(PageIndex: Integer);
    procedure SetupLazDocPage(PageIndex: Integer);
    procedure SetupSavePage(PageIndex: Integer);
    procedure SetupVersionInfoPage(PageIndex: Integer);
    procedure SetupI18NPage(PageIndex: Integer);
    procedure EnableVersionInfo(UseVersionInfo: boolean);
    procedure Enablei18nInfo(Usei18n: boolean);
    procedure FillAutoCreateFormsListbox;
    procedure FillAvailFormsListBox;
    function IndexOfAutoCreateForm(FormName: String): Integer;
    function FirstAutoCreateFormSelected: Integer;
    function FirstAvailFormSelected: Integer;
    procedure SelectOnlyThisAutoCreateForm(Index: Integer);
    function GetAutoCreatedFormsList: TStrings;
    function GetProjectTitle: String;
    function SetAutoCreateForms: Boolean;
    function SetProjectTitle: Boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    property Project: TProject read FProject write SetProject;
  end;

function ShowProjectOptionsDialog(AProject: TProject): TModalResult;

function ProjectSessionStorageToLocalizedName(s: TProjectSessionStorage): string;
function LocalizedNameToProjectSessionStorage(
                                       const s: string): TProjectSessionStorage;

implementation


function ShowProjectOptionsDialog(AProject: TProject): TModalResult;
begin
  with TProjectOptionsDialogNew.Create(Nil) do
    try
      Project := AProject;
      Result  := ShowModal;
    finally
      Free;
    end;
end;

function ProjectSessionStorageToLocalizedName(s: TProjectSessionStorage
  ): string;
begin
  case s of
  pssInProjectInfo: Result:=lisPOSaveInLpiFil;
  pssInProjectDir:  Result:=lisPOSaveInLpsFileInProjectDirectory;
  pssInIDEConfig:   Result:=lisPOSaveInIDEConfigDirectory;
  pssNone:          Result:=lisPODoNotSaveAnySessionInfo;
  else
    RaiseGDBException('');
  end;
end;

function LocalizedNameToProjectSessionStorage(const s: string
  ): TProjectSessionStorage;
begin
  for Result:=Low(TProjectSessionStorage) to High(TProjectSessionStorage) do
    if ProjectSessionStorageToLocalizedName(Result)=s then exit;
  Result:=pssInProjectInfo;
end;


{ TProjectOptionsDialogNew }

constructor TProjectOptionsDialogNew.Create(TheOwner: TComponent);
var
  i: Integer;
  Title: string;
begin
  inherited Create(TheOwner);

  Caption := dlgProjectOptions;
  HelpButton.Caption:=lisMenuHelp;

  NoteBook.PageIndex := 0;

  SetupApplicationPage(0);
  SetupFormsPage(1);
  SetupMiscPage(2);
  SetupLazDocPage(3);
  SetupSavePage(4);
  SetupVersionInfoPage(5);
  SetupI18NPage(6);

  IDEDialogLayoutList.ApplyLayout(Self, 430, 375);
  
  //Indexing items
  for i:= 0 to NoteBook.PageCount-1 do
  begin
    Title:=NoteBook.Pages[i];
    ScanControlTextsForIndex(Title, NoteBook.Page[i]);
  end
end;

procedure TProjectOptionsDialogNew.SetupApplicationPage(PageIndex: Integer);
begin
  NoteBook.Page[PageIndex].Caption := dlgPOApplication;

  AppSettingsGroupBox.Caption := dlgApplicationSettings;
  TitleLabel.Caption := dlgPOTitle;
  TitleEdit.Text := '';
  OutputSettingsGroupBox.Caption := dlgPOOutputSettings;
  TargetFileLabel.Caption := dlgPOTargetFileName;
  TargetFileEdit.Text := '';
  UseAppBundleCheckBox.Caption := dlgPOUseAppBundle;
  UseAppBundleCheckBox.Checked := False;
  UseXPManifestCheckBox.Caption := dlgPOUseManifest;
  UseXPManifestCheckBox.Checked := False;
end;

procedure TProjectOptionsDialogNew.SetupLazDocPage(PageIndex: Integer);
begin
  NoteBook.Page[PageIndex].Caption := lisLazDoc;

  LazDocPathsGroupBox.Caption := lisLazDocPathsGroupBox;
  LazDocAddPathButton.Caption := lisLazDocAddPathButton;
  LazDocDeletePathButton.Caption := lisLazDocDeletePathButton;

  LazDocPathEdit.Clear;
end;

procedure TProjectOptionsDialogNew.SetupSavePage(PageIndex: Integer);
var
  s: TProjectSessionStorage;
begin
  NoteBook.Page[PageIndex].Caption := dlgPOSaveSession;

  SaveClosedUnitInfoCheckBox.Caption := dlgSaveEditorInfo;
  SaveOnlyProjectUnitInfoCheckBox.Caption := dlgSaveEditorInfoProject;
  SaveSessionLocationRadioGroup.Caption:=lisPOSaveSessionInformationIn;
  for s:=Low(TProjectSessionStorage) to High(TProjectSessionStorage) do
    SaveSessionLocationRadioGroup.Items.Add(
                                       ProjectSessionStorageToLocalizedName(s));
end;

procedure TProjectOptionsDialogNew.SetupFormsPage(PageIndex: Integer);
begin
  NoteBook.Page[PageIndex].Caption := dlgPOFroms;

  FormsAutoCreatedLabel.Caption := dlgAutoCreateForms;
  FormsAvailFormsLabel.Caption := dlgAvailableForms;
  FormsAutoCreateNewFormsCheckBox.Caption := dlgAutoCreateNewForms;
end;

procedure TProjectOptionsDialogNew.SetupMiscPage(PageIndex: Integer);
begin
  NoteBook.Page[PageIndex].Caption := dlgPOMisc;

  MainUnitIsPascalSourceCheckBox.Caption := lisMainUnitIsPascalSource;
  MainUnitHasUsesSectionForAllUnitsCheckBox.Caption := lisMainUnitHasUsesSectionContainingAllUnitsOfProject;
  MainUnitHasCreateFormStatementsCheckBox.Caption := lisMainUnitHasApplicationCreateFormStatements;
  MainUnitHasTitleStatementCheckBox.Caption := lisMainUnitHasApplicationTitleStatements;
  RunnableCheckBox.Caption := lisProjectIsRunnable;
  AlwaysBuildCheckBox.Caption := lisProjOptsAlwaysBuildEvenIfNothingChanged;
end;

procedure TProjectOptionsDialogNew.SetupVersionInfoPage(PageIndex: Integer);
begin
  NoteBook.Page[PageIndex].Caption := VersionInfoTitle;
  UseVersionInfoCheckBox.Caption := rsIncludeVersionInfoInExecutable;
  VersionInfoGroupBox.Caption := rsVersionNumbering;
  VersionLabel.Caption := rsVersion;
  MajorRevisionLabel.Caption := rsMajorRevision;
  MinorRevisionLabel.Caption := rsMinorRevision;
  BuildLabel.Caption := rsBuild;
  AutomaticallyIncreaseBuildCheckBox.Caption := rsAutomaticallyIncreaseBuildNumber;
  LanguageSettingsGroupBox.Caption := rsLanguageOptions;
  LanguageSelectionLabel.Caption := rsLanguageSelection;
  CharacterSetLabel.Caption := rsCharacterSet;
  OtherInfoGroupBox.Caption := rsOtherInfo;
  DescriptionLabel.Caption := lisCodeToolsDefsDescription;
  CopyrightLabel.Caption := rsCopyright;
  AdditionalInfoButton.Caption := rsAdditionalInfo;
end;

procedure TProjectOptionsDialogNew.SetupI18NPage(PageIndex: Integer);
begin
  NoteBook.Page[PageIndex].Caption := dlgPOI18n;

  EnableI18NCheckBox.Caption := rsEnableI18n;
  I18NGroupBox.Caption := rsI18nOptions;
  PoOutDirLabel.Caption := rsPOOutputDirectory;
end;

procedure TProjectOptionsDialogNew.EnableVersionInfo(UseVersionInfo: boolean);
begin
  VersionInfoGroupBox.Enabled := UseVersionInfo;
  VersionLabel.Enabled := UseVersionInfo;
  MajorRevisionLabel.Enabled := UseVersionInfo;
  MinorRevisionLabel.Enabled := UseVersionInfo;
  BuildLabel.Enabled := UseVersionInfo;
  VersionSpinEdit.Enabled := UseVersionInfo;
  MajorRevisionSpinEdit.Enabled := UseVersionInfo;
  MinorRevisionSpinEdit.Enabled := UseVersionInfo;
  BuildEdit.Enabled := UseVersionInfo;
  AutomaticallyIncreaseBuildCheckBox.Enabled := UseVersionInfo;

  LanguageSettingsGroupBox.Enabled := UseVersionInfo;
  LanguageSelectionLabel.Enabled := UseVersionInfo;
  CharacterSetLabel.Enabled := UseVersionInfo;
  LanguageSelectionComboBox.Enabled := UseVersionInfo;
  CharacterSetComboBox.Enabled := UseVersionInfo;

  OtherInfoGroupBox.Enabled := UseVersionInfo;
  DescriptionLabel.Enabled := UseVersionInfo;
  CopyrightLabel.Enabled := UseVersionInfo;
  DescriptionEdit.Enabled := UseVersionInfo;
  CopyrightEdit.Enabled := UseVersionInfo;
  AdditionalInfoButton.Enabled := UseVersionInfo;
end;

procedure TProjectOptionsDialogNew.Enablei18nInfo(Usei18n: boolean);
begin
  I18NGroupBox.Enabled := Usei18n;
end;

procedure TProjectOptionsDialogNew.SetProject(AProject: TProject);
var
  AFilename: String;
begin
  FProject := AProject;
  if AProject = Nil then
    exit;

  with AProject do
  begin
    TitleEdit.Text := Title;
    TargetFileEdit.Text := TargetFilename;
    UseAppBundleCheckBox.Checked := UseAppBundle;
    UseXPManifestCheckBox.Checked := XPManifest.UseManifest;
    UseVersionInfoCheckBox.Checked := VersionInfo.UseVersionInfo;
  end;
  FillAutoCreateFormsListbox;
  FillAvailFormsListBox;

  FormsAutoCreateNewFormsCheckBox.Checked := Project.AutoCreateForms;

  SaveClosedUnitInfoCheckBox.Checked := (pfSaveClosedUnits in AProject.Flags);
  SaveOnlyProjectUnitInfoCheckBox.Checked :=
    (pfSaveOnlyProjectUnits in AProject.Flags);
  SaveSessionLocationRadioGroup.ItemIndex:=ord(AProject.SessionStorage);

  MainUnitIsPascalSourceCheckBox.Checked :=
    (pfMainUnitIsPascalSource in AProject.Flags);
  MainUnitHasUsesSectionForAllUnitsCheckBox.Checked :=
    (pfMainUnitHasUsesSectionForAllUnits in AProject.Flags);
  MainUnitHasCreateFormStatementsCheckBox.Checked :=
    (pfMainUnitHasCreateFormStatements in AProject.Flags);
  MainUnitHasTitleStatementCheckBox.Checked :=
    (pfMainUnitHasTitleStatement in AProject.Flags);
  RunnableCheckBox.Checked := (pfRunnable in AProject.Flags);
  AlwaysBuildCheckBox.Checked := (pfAlwaysBuild in AProject.Flags);

  // lazdoc
  SplitString(Project.LazDocPaths,';',LazDocListBox.Items,true);
  
  // i18n
  AFilename:=Project.POOutputDirectory;
  Project.ShortenFilename(AFilename);
  POOutDirEdit.Text:=AFilename;
  EnableI18NCheckBox.Checked := Project.Enablei18n;
  Enablei18nInfo( Project.Enablei18n );

  
  // VersionInfo
  VersionSpinEdit.Value := Project.VersionInfo.VersionNr;
  MajorRevisionSpinEdit.Value := Project.VersionInfo.MajorRevNr;
  MinorRevisionSpinEdit.Value := Project.VersionInfo.MinorRevNr;
  BuildEdit.Text := IntToStr(Project.VersionInfo.BuildNr);
  
  EnableVersionInfo(Project.VersionInfo.UseVersionInfo);

  if Project.VersionInfo.AutoIncrementBuild then
    AutomaticallyIncreaseBuildCheckBox.Checked := true;
  LanguageSelectionComboBox.Items.Assign(MSLanguages);
  LanguageSelectionComboBox.ItemIndex :=
                            MSHexLanguages.IndexOf(Project.VersionInfo.HexLang);
  LanguageSelectionComboBox.Sorted := True;
  CharacterSetComboBox.Items.Assign(MSCharacterSets);
  CharacterSetComboBox.ItemIndex :=
                     MSHexCharacterSets.IndexOf(Project.VersionInfo.HexCharSet);
  CharacterSetComboBox.Sorted := True;
  DescriptionEdit.Text := Project.VersionInfo.DescriptionString;
  CopyrightEdit.Text := Project.VersionInfo.CopyrightString;
end;

procedure TProjectOptionsDialogNew.ProjectOptionsClose(Sender: TObject;
  var CloseAction: TCloseAction);
var
  NewFlags: TProjectFlags;
  AFilename: String;

  procedure SetProjectFlag(AFlag: TProjectFlag; AValue: Boolean);
  begin
    if AValue then
      Include(NewFlags, AFlag)
    else
      Exclude(NewFlags, AFlag);
  end;

begin
  if ModalResult = mrOk then
  begin

    with Project do
    begin
      Title := TitleEdit.Text;
      TargetFilename := TargetFileEdit.Text;
      UseAppBundle := UseAppBundleCheckBox.Checked;
      XPManifest.UseManifest := UseXPManifestCheckBox.Checked;
      Project.XPManifest.UpdateMainSourceFile(Project.MainFilename);
    end;

    // flags
    NewFlags := Project.Flags;
    SetProjectFlag(pfSaveClosedUnits, SaveClosedUnitInfoCheckBox.Checked);
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
    SetProjectFlag(pfRunnable, RunnableCheckBox.Checked);
    SetProjectFlag(pfAlwaysBuild, AlwaysBuildCheckBox.Checked);
    Project.Flags := NewFlags;
    
    if SaveSessionLocationRadioGroup.ItemIndex>=0 then
      Project.SessionStorage:=LocalizedNameToProjectSessionStorage(
                         SaveSessionLocationRadioGroup.Items[
                                      SaveSessionLocationRadioGroup.ItemIndex]);

    Project.AutoCreateForms := FormsAutoCreateNewFormsCheckBox.Checked;

    SetAutoCreateForms;
    SetProjectTitle;
    
    // lazdoc
    Project.LazDocPaths:=StringListToText(LazDocListBox.Items,';',true);
    
    // i18n
    AFilename:=TrimFilename(POOutDirEdit.Text);
    Project.LongenFilename(AFilename);
    Project.POOutputDirectory:=AFilename;
    Project.EnableI18N := EnableI18NCheckBox.Checked;

    // VersionInfo
    Project.VersionInfo.UseVersionInfo:=UseVersionInfoCheckBox.Checked;
    Project.VersionInfo.AutoIncrementBuild:=AutomaticallyIncreaseBuildCheckBox.Checked;
    Project.VersionInfo.VersionNr:=VersionSpinEdit.Value;
    Project.VersionInfo.MajorRevNr:=MajorRevisionSpinEdit.Value;
    Project.VersionInfo.MinorRevNr:=MinorRevisionSpinEdit.Value;
    Project.VersionInfo.BuildNr:=StrToIntDef(BuildEdit.Text,Project.VersionInfo.BuildNr);
    Project.VersionInfo.DescriptionString:=DescriptionEdit.Text;
    Project.VersionInfo.CopyrightString:=CopyrightEdit.Text;
    Project.VersionInfo.HexLang:=MSLanguageToHex(LanguageSelectionComboBox.Text);
    Project.VersionInfo.HexCharSet:=MSCharacterSetToHex(CharacterSetComboBox.Text);
    if Project.VersionInfo.Modified then
      Project.VersionInfo.UpdateMainSourceFile(Project.MainFilename);
  end;

  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TProjectOptionsDialogNew.LazDocAddPathButtonClick(Sender: TObject);
begin
  if LazDocPathEdit.Text <> '' then
    LazDocListBox.Items.Add(LazDocPathEdit.Text);
end;

procedure TProjectOptionsDialogNew.HelpButtonClick(Sender: TObject);
begin
  ShowContextHelpForIDE(Self);
end;

procedure TProjectOptionsDialogNew.FormsPageResize(Sender: TObject);
begin
  with FormsAutoCreatedListBox do
  begin
    Width  := (FormsPage.Width - Left * 2 - 6) div 2;
  end;

  with FormsAvailFormsLabel do
    Left := FormsAvailFormsListBox.Left;
end;

procedure TProjectOptionsDialogNew.AdditionalInfoButtonClick(Sender: TObject);
var
  InfoModified: Boolean;
begin
   InfoModified:=false;
   ShowVersionInfoAdditionailInfoForm(Project.VersionInfo,InfoModified);
   if InfoModified then
      Project.Modified:=InfoModified;
end;

procedure TProjectOptionsDialogNew.EnableI18NCheckBoxChange(Sender: TObject);
begin
   Enablei18nInfo(EnableI18NCheckBox.Checked);
end;

procedure TProjectOptionsDialogNew.LazDocBrowseButtonClick(Sender: TObject);
begin
  if SelectDirectoryDialog.Execute then
    LazDocPathEdit.Text := SelectDirectoryDialog.FileName;
end;

procedure TProjectOptionsDialogNew.LazDocDeletePathButtonClick(Sender: TObject);
begin
  if (LazDocListBox.ItemIndex >= 0) then
    LazDocListBox.Items.Delete(LazDocListBox.ItemIndex);
end;

function TProjectOptionsDialogNew.GetAutoCreatedFormsList: TStrings;
var
  i, j: Integer;
begin
  if (FProject <> Nil) and (FProject.MainUnitID >= 0) then
  begin
    Result := CodeToolBoss.ListAllCreateFormStatements(
                                                  FProject.MainUnitInfo.Source);
    if Result <> Nil then
      for i := 0 to Result.Count - 1 do
      begin
        j := Pos(':', Result[i]);
        if j > 0 then
          if 't' + lowercase(copy(Result[i], 1, j - 1)) = lowercase(
            copy(Result[i], j + 1, length(Result[i]) - j)) then
            Result[i] := copy(Result[i], 1, j - 1);
      end// shorten lines of type 'FormName:TFormName' to simply 'FormName'
    ;
  end
  else
    Result := Nil;
end;

function TProjectOptionsDialogNew.GetProjectTitle: String;
begin
  Result := '';
  if (FProject = Nil) or (FProject.MainUnitID < 0) then
    exit;
  CodeToolBoss.GetApplicationTitleStatement(
                                          FProject.MainUnitInfo.Source, Result);
end;

procedure TProjectOptionsDialogNew.FillAutoCreateFormsListbox;
var
  sl: TStrings;
begin
  sl := GetAutoCreatedFormsList;
  FormsAutoCreatedListBox.Items.BeginUpdate;
  FormsAutoCreatedListBox.Items.Clear;
  if sl <> Nil then
  begin
    FormsAutoCreatedListBox.Items.Assign(sl);
    sl.Free;
  end;
  FormsAutoCreatedListBox.Items.EndUpdate;
end;

procedure TProjectOptionsDialogNew.FillAvailFormsListBox;
var
  sl: TStringList;
  i:  Integer;
begin
  FormsAvailFormsListBox.Items.BeginUpdate;
  FormsAvailFormsListBox.Items.Clear;

  if (FProject <> Nil) then
  begin
    sl := TStringList.Create;
    try
      for i := 0 to FProject.UnitCount - 1 do
        if (FProject.Units[i].IsPartOfProject) and
          (FProject.Units[i].ComponentName <> '') and
          (IndexOfAutoCreateForm(FProject.Units[i].ComponentName) < 0) then
          sl.Add(FProject.Units[i].ComponentName);
      sl.Sort;
      FormsAvailFormsListBox.Items.Assign(sl);
    finally
      sl.Free;
    end;
  end;
  FormsAvailFormsListBox.Items.EndUpdate;
end;

function TProjectOptionsDialogNew.IndexOfAutoCreateForm(FormName:
  String): Integer;
var
  p: Integer;
begin
  p := Pos(':', FormName);
  if p > 0 then
    FormName := copy(FormName, 1, p - 1);
  Result := FormsAutoCreatedListBox.Items.Count - 1;
  while (Result >= 0) do
  begin
    p := Pos(':', FormsAutoCreatedListBox.Items[Result]);
    if p < 1 then
      p := length(FormsAutoCreatedListBox.Items[Result]) + 1;
    if AnsiCompareText(copy(FormsAutoCreatedListBox.Items[Result], 1, p - 1)
      , FormName) = 0 then
      exit;
    dec(Result);
  end;
end;

function TProjectOptionsDialogNew.FirstAutoCreateFormSelected: Integer;
begin
  Result := 0;
  while (Result < FormsAutoCreatedListBox.Items.Count) and
    (not FormsAutoCreatedListBox.Selected[Result]) do
    inc(Result);
  if Result = FormsAutoCreatedListBox.Items.Count then
    Result := -1;
end;

function TProjectOptionsDialogNew.FirstAvailFormSelected: Integer;
begin
  Result := 0;
  while (Result < FormsAvailFormsListBox.Items.Count) and
    (not FormsAvailFormsListBox.Selected[Result]) do
    inc(Result);
  if Result = FormsAvailFormsListBox.Items.Count then
    Result := -1;
end;

procedure TProjectOptionsDialogNew.FormsAddToAutoCreatedFormsBtnClick(
  Sender: TObject);
var
  i: Integer;
  NewFormName: String;
begin
  FormsAutoCreatedListBox.Items.BeginUpdate;
  with FormsAvailFormsListBox do
  begin
    Items.BeginUpdate;
    i := 0;
    while i < Items.Count do
      if Selected[i] then
      begin
        NewFormName := Items[i];
        Items.Delete(i);
        FormsAutoCreatedListBox.Items.Add(NewFormName);
      end
      else
        inc(i);
    Items.EndUpdate;
  end;
  FormsAutoCreatedListBox.Items.EndUpdate;
end;

procedure TProjectOptionsDialogNew.FormsRemoveFromAutoCreatedFormsBtnClick(
  Sender: TObject);
var
  i, NewPos, cmp: Integer;
  OldFormName:    String;
begin
  FormsAutoCreatedListBox.Items.BeginUpdate;
  FormsAvailFormsListBox.Items.BeginUpdate;
  i := 0;
  while i < FormsAutoCreatedListBox.Items.Count do
    if FormsAutoCreatedListBox.Selected[i] then
    begin
      OldFormName := FormsAutoCreatedListBox.Items[i];
      FormsAutoCreatedListBox.Items.Delete(i);
      NewPos := 0;
      cmp := 1;
      while (NewPos < FormsAvailFormsListBox.Items.Count) do
      begin
        cmp := AnsiCompareText(FormsAvailFormsListBox.Items[NewPos], OldFormName);
        if cmp < 0 then
          inc(NewPos)
        else
          break;
      end;
      if cmp = 0 then
        continue;
      FormsAvailFormsListBox.Items.Insert(NewPos, OldFormName);
    end
    else
      inc(i);
  FormsAvailFormsListBox.Items.EndUpdate;
  FormsAutoCreatedListBox.Items.EndUpdate;
end;

procedure TProjectOptionsDialogNew.FormsMoveAutoCreatedFormUpBtnClick(
  Sender: TObject);
var
  i: Integer;
  h: String;
begin
  i := FirstAutoCreateFormSelected;
  if i < 1 then
    exit;
  with FormsAutoCreatedListBox do
  begin
    Items.BeginUpdate;
    h := Items[i];
    Items[i] := Items[i - 1];
    Items[i - 1] := h;
    Items.EndUpdate;
  end;
  SelectOnlyThisAutoCreateForm(i - 1);
end;

procedure TProjectOptionsDialogNew.FormsMoveAutoCreatedFormDownBtnClick(
  Sender: TObject);
var
  i: Integer;
  h: String;
begin
  i := FirstAutoCreateFormSelected;
  if (i < 0) or (i >= FormsAutoCreatedListBox.Items.Count - 1) then
    exit;
  with FormsAutoCreatedListBox do
  begin
    Items.BeginUpdate;
    h := Items[i];
    Items[i] := Items[i + 1];
    Items[i + 1] := h;
    Items.EndUpdate;
  end;
  SelectOnlyThisAutoCreateForm(i + 1);
end;

procedure TProjectOptionsDialogNew.POOutDirButtonClick(Sender: TObject);
var
  NewDirectory: string;
begin
  NewDirectory:=LazSelectDirectory(lisPOChoosePoFileDirectory,
                                   Project.ProjectDirectory);
  if NewDirectory='' then exit;
  Project.ShortenFilename(NewDirectory);
  POOutDirEdit.Text:=NewDirectory;
end;

procedure TProjectOptionsDialogNew.UseVersionInfoCheckBoxChange(Sender: TObject);
begin
  EnableVersionInfo(UseVersionInfoCheckBox.Checked);
end;

procedure TProjectOptionsDialogNew.SelectOnlyThisAutoCreateForm(Index: Integer);
var
  i: Integer;
begin
  with FormsAutoCreatedListBox do
    for i := 0 to Items.Count - 1 do
      Selected[i] := (i = Index);
end;

function TProjectOptionsDialogNew.SetAutoCreateForms: Boolean;
var
  i: Integer;
  OldList: TStrings;
begin
  Result := True;
  if (Project.MainUnitID < 0) or
    (not (pfMainUnitHasUsesSectionForAllUnits in Project.Flags)) then
    exit;
  OldList := GetAutoCreatedFormsList;
  if (OldList = Nil) then
    exit;
  try
    if OldList.Count = FormsAutoCreatedListBox.Items.Count then
    begin

      { Just exit if the form list is the same }
      i := OldList.Count - 1;
      while (i >= 0) and
        (CompareText(OldList[i], FormsAutoCreatedListBox.Items[i]) = 0) do
        dec(i);
      if i < 0 then
        Exit;
    end;

    if not CodeToolBoss.SetAllCreateFromStatements(
      Project.MainUnitInfo.Source, FormsAutoCreatedListBox.Items) then
    begin
      MessageDlg(lisProjOptsError,
        Format(lisProjOptsUnableToChangeTheAutoCreateFormList, [LineEnding]),
        mtWarning, [mbCancel], 0);
      Result := False;
      exit;
    end;
  finally
    OldList.Free;
  end;
end;

function TProjectOptionsDialogNew.SetProjectTitle: Boolean;
var
  OldTitle: String;
begin
  Result := True;
  if (Project.MainUnitID < 0) or
    (not (pfMainUnitHasTitleStatement in Project.Flags)) then
    exit;
  OldTitle := GetProjectTitle;
  if (OldTitle = '') and Project.TitleIsDefault then
    exit;

  if (OldTitle <> Project.Title) and (not Project.TitleIsDefault) then
    if not CodeToolBoss.SetApplicationTitleStatement(
      Project.MainUnitInfo.Source, Project.Title) then
    begin
      MessageDlg(lisProjOptsError,
        'Unable to change project title in source.'#13 +
        CodeToolBoss.ErrorMessage,
        mtWarning, [mbCancel], 0);
      Result := False;
      exit;
    end;// set Application.Title:= statement

  if (OldTitle <> '') and Project.TitleIsDefault then
    if not CodeToolBoss.RemoveApplicationTitleStatement(
      Project.MainUnitInfo.Source) then
    begin
      MessageDlg(lisProjOptsError,
        'Unable to remove project title from source.'#13 +
        CodeToolBoss.ErrorMessage,
        mtWarning, [mbCancel], 0);
      Result := False;
      exit;
    end;// delete title
end;

initialization
  {$I projectopts_new.lrs}

end.
