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
    This unit defines a dialog for the lazarus environment options and a class to
    store the options in a xml file.

  ToDo: split this into two units - the dialog and the options.
}
unit EnvironmentOpts_new;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  Classes, SysUtils, FPCAdds, LCLProc, Forms, Controls, Buttons, GraphType,
  Graphics, ExtCtrls, StdCtrls, Spin, FileUtil, LResources, Dialogs,
  Laz_XMLCfg,
  ObjectInspector, IDEWindowIntf,
  LazarusIDEStrConsts, TransferMacros, LazConf, ExtToolDialog, IDEProcs,
  IDEOptionDefs, InputHistory, EditorOptions, IDETranslations,
  OptionsEditorBase,
  // uses EnvironmentOpts too for common declarations
  EnvironmentOpts;


  { TEnvironmentOptionsDialog }
type
  TEnvironmentOptionsDialogNew = class(TOptionsEditorForm)
    NoteBook: TNoteBook;
    FilesPage: TPage;
    DesktopPage: TPage;
    WindowsPage: TPage;
    FormEditorPage: TPage;
    ObjectInspectorPage: TPage;
    BackupPage: TPage;
    NamingPage: TPage;
    LazDocPage: TPage;
    SelectDirectoryDialog: TSelectDirectoryDialog;

    // lazdoc settings
    LazDocBrowseButton: TButton;
    LazDocPathEdit: TEdit;
    LazDocDeletePathButton: TButton;
    LazDocAddPathButton: TButton;
    LazDocPathsGroupBox: TGroupBox;
    LazDocListBox: TListBox;

    // language
    LanguageGroupBox: TGroupBox;
    LanguageComboBox: TComboBox;
    AutoSaveGroupBox: TGroupBox;
    AutoSaveEditorFilesCheckBox: TCheckBox;
    AutoSaveProjectCheckBox: TCheckBox;
    AutoSaveIntervalInSecsLabel: TLabel;
    AutoSaveIntervalInSecsComboBox: TComboBox;
    
    // desktop files
    DesktopFilesGroupBox: TGroupBox;
    SaveDesktopSettingsToFileButton: TButton;
    LoadDesktopSettingsFromFileButton: TButton;

    // hints
    CheckDiskChangesWithLoadingCheckBox: TCheckBox;
    ShowHintsForComponentPaletteCheckBox: TCheckBox;
    ShowHintsForMainSpeedButtonsCheckBox: TCheckBox;
    
    // messages view
    MsgViewDblClickJumpsCheckBox: TCheckBox;

    // window layout
    WindowPositionsGroupBox: TGroupBox;
    WindowPositionsListBox: TListBox;
    WindowPositionsBox: TIDEWindowSetupLayoutComponent;
    MinimizeAllOnMinimizeMainCheckBox: TCheckBox;
    HideIDEOnRunCheckBox: TCheckBox;

    // designer
    GridGroupBox: TGroupBox;
    ShowGridCheckBox: TCheckBox;
    ShowBorderSpaceCheckBox: TCheckBox;
    GridColorLabel: TLabel;
    GridColorButton: TColorButton;
    SnapToGridCheckBox: TCheckBox;
    GridSizeXLabel: TLabel;
    GridSizeXComboBox: TComboBox;
    GridSizeYLabel: TLabel;
    GridSizeYComboBox: TComboBox;
    GuideLinesGroupBox: TGroupBox;
    ShowGuideLinesCheckBox: TCheckBox;
    SnapToGuideLinesCheckBox: TCheckBox;
    GuideLineColorLeftTopLabel: TLabel;
    GuideLineColorLeftTopButton: TColorButton;
    GuideLineColorRightBottomLabel: TLabel;
    GuideLineColorRightBottomButton: TColorButton;
    FormEditMiscGroupBox: TGroupBox;
    ShowComponentCaptionsCheckBox: TCheckBox;
    ShowEditorHintsCheckBox: TCheckBox;
    AutoCreateFormsOnOpenCheckBox: TCheckBox;
    RightClickSelectsCheckBox: TCheckBox;
    GrabberColorLabel: TLabel;
    GrabberColorButton: TColorButton;
    MarkerColorLabel: TLabel;
    MarkerColorButton: TColorButton;
    RubberbandGroupBox: TGroupBox;
    RubberbandSelectColorLabel: TLabel;
    RubberbandSelectColorButton: TColorButton;
    RubberbandCreateColorLabel: TLabel;
    RubberbandCreateColorButton: TColorButton;
    RubberbandSelectsGrandChildsCheckBox: TCheckBox;
    DesignerPaintLazyCheckBox: TCheckBox;

    // object inspector
    ObjectInspectorColorsGroupBox: TGroupBox;
    OIBackgroundColorLabel: TLabel;
    OIBackgroundColorButton: TColorButton;

    OISubPropsColorLabel: TLabel;
    OISubPropsColorButton: TColorButton;
    OIReferencesColorLabel: TLabel;
    OIReferencesColorButton: TColorButton;
    OIValueColorLabel: TLabel;
    OIValueColorButton: TColorButton;
    OIDefaultValueColorLabel: TLabel;
    OIDefaultValueColorButton: TColorButton;
    OIPropNameColorLabel: TLabel;
    OIPropNameColorButton: TColorButton;

    OIMiscGroupBox: TGroupBox;
    OIDefaultItemHeightSpinEdit: TSpinEdit;
    OIDefaultItemHeightLabel: TLabel;
    OIShowHintCheckBox: TCheckBox;

    // Files
    MaxRecentOpenFilesLabel: TLabel;
    MaxRecentOpenFilesComboBox: TComboBox;
    MaxRecentProjectFilesLabel: TLabel;
    MaxRecentProjectFilesComboBox: TComboBox;
    OpenLastProjectAtStartCheckBox: TCheckBox;
    LazarusDirGroupBox: TGroupBox;
    LazarusDirComboBox: TComboBox;
    LazarusDirButton: TButton;
    CompilerPathGroupBox: TGroupBox;
    CompilerPathComboBox: TComboBox;
    CompilerPathButton: TButton;
    FPCSourceDirGroupBox: TGroupBox;
    FPCSourceDirComboBox: TComboBox;
    FPCSourceDirButton: TButton;
    MakePathGroupBox: TGroupBox;
    MakePathComboBox: TComboBox;
    MakePathButton: TButton;
    TestBuildDirGroupBox: TGroupBox;
    TestBuildDirComboBox: TComboBox;
    TestBuildDirButton: TButton;

    // backup
    BackupHelpLabel: TLabel;
    BackupProjectGroupBox: TGroupBox;
    BakProjTypeRadioGroup: TRadioGroup;
    BakProjAddExtLabel: TLabel;
    BakProjAddExtComboBox: TComboBox;
    BakProjMaxCounterLabel: TLabel;
    BakProjMaxCounterComboBox: TComboBox;
    BakProjSubDirLabel: TLabel;
    BakProjSubDirComboBox: TComboBox;
    BackupOtherGroupBox: TGroupBox;
    BakOtherTypeRadioGroup: TRadioGroup;
    BakOtherAddExtLabel: TLabel;
    BakOtherAddExtComboBox: TComboBox;
    BakOtherMaxCounterLabel: TLabel;
    BakOtherMaxCounterComboBox: TComboBox;
    BakOtherSubDirLabel: TLabel;
    BakOtherSubDirComboBox: TComboBox;
    
    // naming conventions
    PascalFileExtRadiogroup: TRadioGroup;
    CharCaseFileActionRadioGroup: TRadioGroup;
    AmbiguousFileActionRadioGroup: TRadioGroup;

    procedure BakTypeRadioGroupClick(Sender: TObject);
    procedure FilesButtonClick(Sender: TObject);
    procedure DirectoriesButtonClick(Sender: TObject);
    procedure FormEditorPageResize(Sender: TObject);
    procedure LazDocAddPathButtonClick(Sender: TObject);
    procedure LazDocBrowseButtonClick(Sender: TObject);
    procedure LazDocDeletePathButtonClick(Sender: TObject);
    procedure NotebookChangeBounds(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure SaveDesktopSettingsToFileButtonClick(Sender: TObject);
    procedure LoadDesktopSettingsFromFileButtonClick(Sender: TObject);
    procedure WindowPositionsListBoxMouseUp(Sender:TObject;
       Button:TMouseButton;  Shift:TShiftState;  X,Y:integer);
  private
    FOnLoadEnvironmentSettings: TOnLoadEnvironmentSettings;
    FOnSaveEnvironmentSettings: TOnSaveEnvironmentSettings;
    FLayouts: TIDEWindowLayoutList;
    FOldLazarusDir: string;
    FOldCompilerFilename: string;
    FOldFPCSourceDir: string;
    FOldMakeFilename: string;
    FOldTestDir: string;

    procedure SetCategoryPage(const AValue: TEnvOptsDialogPage);
    procedure SetupFilesPage(Page: integer);
    procedure SetupDesktopPage(Page: integer);
    procedure SetupWindowsPage(Page: integer);
    procedure SetupFormEditorPage(Page: integer);
    procedure SetupObjectInspectorPage(Page: integer);
    procedure SetupBackupPage(Page: integer);
    procedure SetupNamingPage(Page: integer);
    procedure SetupLazDocPage(Page: integer);
    procedure SetWindowPositionsItem(Index: integer);
    function CheckValues: boolean;
    function CheckLazarusDir: boolean;
    function IsFPCSourceDir: boolean;
    function CheckTestDir: boolean;
  published
    property OnSaveEnvironmentSettings: TOnSaveEnvironmentSettings
      read FOnSaveEnvironmentSettings write FOnSaveEnvironmentSettings;
    property OnLoadEnvironmentSettings: TOnLoadEnvironmentSettings
      read FOnLoadEnvironmentSettings write FOnLoadEnvironmentSettings;
    property CategoryPage: TEnvOptsDialogPage write SetCategoryPage;
  public
    procedure ReadSettings(AnEnvironmentOptions: TEnvironmentOptions);
    procedure WriteSettings(AnEnvironmentOptions: TEnvironmentOptions);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;


function DebuggerNameToType(const s: string): TDebuggerType;
function PascalExtToType(const Ext: string): TPascalExtType;
function AmbiguousFileActionNameToType(const Action: string): TAmbiguousFileAction;
function CharCaseFileActionNameToType(const Action: string): TCharCaseFileAction;

function CheckFileChanged(const OldFilename, NewFilename: string): boolean;
function CheckExecutable(const OldFilename, NewFilename: string;
  const ErrorCaption, ErrorMsg: string): boolean;
function CheckDirPathExists(const Dir,
  ErrorCaption, ErrorMsg: string): TModalResult;
function SimpleDirectoryCheck(const OldDir, NewDir,
  NotFoundErrMsg: string; var StopChecking: boolean): boolean;

procedure SetComboBoxText(AComboBox:TComboBox; const AText:AnsiString);
procedure SetComboBoxText(AComboBox:TComboBox; const AText:AnsiString;
                          MaxCount: integer);

const
  DefaultLazDocPath = '$(LazarusDir)/docs/xml/lcl';
  
implementation


const MaxComboBoxCount: integer = 20;


function DebuggerNameToType(const s: string): TDebuggerType;
begin
  for Result:=Low(TDebuggerType) to High(TDebuggerType) do
    if CompareText(DebuggerName[Result],s)=0 then exit;
  Result:=dtNone;
end;

function PascalExtToType(const Ext: string): TPascalExtType;
begin
  if Ext<>'' then
    for Result:=Low(TPascalExtType) to High(TPascalExtType) do
      if CompareFilenames(Ext,PascalExtension[Result])=0 then exit;
  Result:=petNone;
end;

function AmbiguousFileActionNameToType(
  const Action: string): TAmbiguousFileAction;
begin
  for Result:=Low(TAmbiguousFileAction) to High(TAmbiguousFileAction) do begin
    if CompareText(AmbiguousFileActionNames[Result],Action)=0 then
      exit;
  end;
  Result:=afaAsk;
end;

function CharCaseFileActionNameToType(
  const Action: string): TCharCaseFileAction;
begin
  for Result:=Low(TCharCaseFileAction) to High(TCharCaseFileAction) do begin
    if CompareText(CharCaseFileActionNames[Result],Action)=0 then
      exit;
  end;
  Result:=ccfaAutoRename;
end;


function CheckFileChanged(const OldFilename,
  NewFilename: string): boolean;
begin
  Result:=(NewFilename<>OldFilename) and (NewFilename<>'');
end;

function CheckExecutable(const OldFilename,
  NewFilename: string; const ErrorCaption, ErrorMsg: string): boolean;
begin
  Result:=true;
  if not CheckFileChanged(OldFilename,NewFilename) then exit;
  if (not FileIsExecutable(NewFilename)) then begin
    if MessageDlg(ErrorCaption,Format(ErrorMsg,[NewFilename]),
      mtWarning,[mbIgnore,mbCancel],0)=mrCancel
    then begin
      Result:=false;
    end;
  end;
end;

function CheckDirPathExists(const Dir,
  ErrorCaption, ErrorMsg: string): TModalResult;
begin
  if not DirPathExists(Dir) then begin
    Result:=MessageDlg(ErrorCaption,Format(ErrorMsg,[Dir]),mtWarning,
                       [mbIgnore,mbCancel],0);
  end else
    Result:=mrOk;
end;

function SimpleDirectoryCheck(const OldDir, NewDir,
  NotFoundErrMsg: string; var StopChecking: boolean): boolean;
var
  SubResult: TModalResult;
begin
  StopChecking:=true;
  if not CheckFileChanged(OldDir,NewDir) then begin
    Result:=true;
    exit;
  end;
  SubResult:=CheckDirPathExists(NewDir,lisEnvOptDlgDirectoryNotFound,
                                  NotFoundErrMsg);
  if SubResult=mrIgnore then begin
    Result:=true;
    exit;
  end;
  if SubResult=mrCancel then begin
    Result:=false;
    exit;
  end;
  StopChecking:=false;
  Result:=true;
end;

procedure SetComboBoxText(AComboBox:TComboBox; const AText: String);
var a:integer;
begin
  a:=AComboBox.Items.IndexOf(AText);
  if a>=0 then
    AComboBox.ItemIndex:=a
  else begin
    AComboBox.Items.Add(AText);
    AComboBox.ItemIndex:=AComboBox.Items.IndexOf(AText);
  end;
  AComboBox.Text:=AText;
end;

procedure SetComboBoxText(AComboBox:TComboBox; const AText: String;
  MaxCount: integer);
var a:integer;
begin
  a:=AComboBox.Items.IndexOf(AText);
  if a>=0 then
    AComboBox.ItemIndex:=a
  else begin
    AComboBox.Items.Insert(0,AText);
    AComboBox.ItemIndex:=AComboBox.Items.IndexOf(AText);
    if MaxCount<2 then MaxCount:=2;
    while AComboBox.Items.Count>MaxCount do
      AComboBox.Items.Delete(AComboBox.Items.Count-1);
  end;
  AComboBox.Text:=AText;
end;


const
  EnvOptsConfFileName='environmentoptions.xml';
  BakMaxCounterInfiniteTxt = 'infinite';

{ TEnvironmentOptionsDialogNew }

constructor TEnvironmentOptionsDialogNew.Create(TheOwner: TComponent);
var
  Title: string;
  i: Integer;
begin
  inherited Create(TheOwner);
  IDEDialogLayoutList.ApplyLayout(Self,Width,Height);
  Caption:=lisMenuGeneralOptions;

  NoteBook.PageIndex:=0;

  SetupFilesPage(0);
  SetupDesktopPage(1);
  SetupWindowsPage(2);
  SetupFormEditorPage(3);
  SetupObjectInspectorPage(4);
  SetupBackupPage(5);
  SetupNamingPage(6);
  SetupLazDocPage(7);

  btnCancel.Caption:=dlgCancel;
  CancelControl:=btnCancel;
  
  //Indexing item
  for i:= 0 to NoteBook.PageCount-1 do
  begin
    Title:=NoteBook.Pages[i];
    ScanControlTextsForIndex(Title, NoteBook.Page[i]);
  end
end;

destructor TEnvironmentOptionsDialogNew.Destroy;
begin
  inherited Destroy;
end;

procedure TEnvironmentOptionsDialogNew.SetupDesktopPage(Page: integer);
var
  i: Integer;
  LangID: String;
begin
  NoteBook.Page[Page].Caption := dlgDesktop;
  
  // language
  LanguageGroupBox.Caption:=dlgEnvLanguage;

  with LanguageComboBox.Items do begin
    BeginUpdate;
    for i:=0 to LazarusTranslations.Count-1 do begin
      LangID:=LazarusTranslations[i].ID;
      if LangID='' then
        //No [] if automatic
        Add(GetLazarusLanguageLocalizedName(LangID))
      else
        Add(GetLazarusLanguageLocalizedName(LangID)+' ['+LangID+']');
    end;
    EndUpdate;
  end;

  // auto save
  AutoSaveGroupBox.Caption:=dlgAutoSave;
  AutoSaveEditorFilesCheckBox.Caption:=dlgEdFiles;
  AutoSaveProjectCheckBox.Caption:=dlgEnvProject;
  AutoSaveIntervalInSecsLabel.Caption:=dlgIntvInSec;

  // desktop files
  DesktopFilesGroupBox.Caption:=dlgDesktopFiles;
  SaveDesktopSettingsToFileButton.Caption:=dlgSaveDFile;
  LoadDesktopSettingsFromFileButton.Caption:=dlgLoadDFile;

  // hints
  CheckDiskChangesWithLoadingCheckBox.Caption:=lisCheckChangesOnDiskWithLoading;
  ShowHintsForComponentPaletteCheckBox.Caption:=dlgPalHints;
  ShowHintsForMainSpeedButtonsCheckBox.Caption:=dlgSpBHints;

  // messages view
  MsgViewDblClickJumpsCheckBox.Caption:=lisEnvDoubleClickOnMessagesJumpsOtherwiseSingleClick;
end;

procedure TEnvironmentOptionsDialogNew.SetupWindowsPage(Page: integer);
begin
  NoteBook.Page[Page].Caption := dlgWindows;

  // windows
  MinimizeAllOnMinimizeMainCheckBox.Caption:=dlgMinimizeAllOnMinimizeMain;
  HideIDEOnRunCheckBox.Caption:=dlgHideIDEOnRun;

  // Window Positions
  WindowPositionsGroupBox.Caption:=dlgWinPos;
  with WindowPositionsListBox.Items do begin
    BeginUpdate;
    Add(dlgMainMenu);
    Add(dlgSrcEdit);
    Add(dlgMsgs);
    Add(dlgObjInsp);
    Add(lisMenuProjectInspector);
    Add(lisCodeExplorer);
    Add(lisMenuPackageGraph);
    Add(dlgUnitDepCaption);
    Add(lisMenuLazDoc);
    EndUpdate;
  end;
  WindowPositionsBox:=TIDEWindowSetupLayoutComponent.Create(Self);
  with WindowPositionsBox do begin
    Name:='WindowPositionsBox';
    Parent:=WindowPositionsGroupBox;
    BorderSpacing.Around:=6;
    Align:=alBottom;
    AnchorToNeighbour(akTop,6,WindowPositionsListBox);
  end;
end;

procedure TEnvironmentOptionsDialogNew.SetupBackupPage(Page: integer);
begin
  NoteBook.Page[Page].Caption := dlgEnvBckup;

  BackupHelpLabel.Caption:=dlgEnvBackupHelpNote;

  BackupProjectGroupBox.Caption:=dlgProjFiles;

  with BakProjTypeRadioGroup do begin
    Caption:=dlgEnvType;
    with Items do begin
      BeginUpdate;
      Add(lisNoBackupFiles);
      Add(dlgSmbFront);
      Add(dlgSmbBehind);
      Add(dlgSmbCounter);
      Add(dlgCustomExt);
      Add(dlgBckUpSubDir);
      EndUpdate;
    end;
  end;

  BakProjAddExtLabel.Caption:=dlgEdCustomExt;

  with BakProjAddExtComboBox.Items do begin
    BeginUpdate;
    Clear;
    Add('bak');
    Add('old');
    EndUpdate;
  end;

  BakProjMaxCounterLabel.Caption:=dlgMaxCntr;

  with BakProjMaxCounterComboBox.Items do begin
    BeginUpdate;
    Clear;
    Add('1');
    Add('2');
    Add('3');
    Add('5');
    Add('9');
    Add(BakMaxCounterInfiniteTxt);
    EndUpdate;
  end;

  BakProjSubDirLabel.Caption:=dlgEdBSubDir;

  BakProjSubDirComboBox.Text:='';
  with BakProjSubDirComboBox.Items do begin
    BeginUpdate;
    Clear;
    Add(dlgBakNoSubDirectory);
    Add('backup');
    EndUpdate;
  end;

  BackupOtherGroupBox.Caption:=dlgEnvOtherFiles;

  with BakOtherTypeRadioGroup do begin
    Caption:=dlgEnvType;
    with Items do begin
      BeginUpdate;
      Add(lisNoBackupFiles);
      Add(dlgSmbFront);
      Add(dlgSmbBehind);
      Add(dlgSmbCounter);
      Add(dlgCustomExt);
      Add(dlgBckUpSubDir);
      EndUpdate;
    end;
  end;

  BakOtherAddExtLabel.Caption:=dlgEdCustomExt;

  with BakOtherAddExtComboBox.Items do begin
    BeginUpdate;
    Add('bak');
    Add('old');
    EndUpdate;
  end;

  BakOtherMaxCounterLabel.Caption:=dlgMaxCntr;

  with BakOtherMaxCounterComboBox.Items do begin
    BeginUpdate;
    Clear;
    Add('1');
    Add('2');
    Add('3');
    Add('5');
    Add('9');
    Add(BakMaxCounterInfiniteTxt);
    EndUpdate;
  end;

  BakOtherSubDirLabel.Caption:=dlgEdBSubDir;

  with BakOtherSubDirComboBox.Items do begin
    BeginUpdate;
    Clear;
    Add(dlgBakNoSubDirectory);
    Add('backup');
    EndUpdate;
  end;
end;

procedure TEnvironmentOptionsDialogNew.SetupFilesPage(Page: integer);
begin
  NoteBook.Page[Page].Caption := dlgEnvFiles;

  MaxRecentOpenFilesLabel.Caption:=dlgMaxRecentFiles;

  MaxRecentProjectFilesLabel.Caption:=dlgMaxRecentProjs;

  OpenLastProjectAtStartCheckBox.Caption:=dlgQOpenLastPrj;

  LazarusDirGroupBox.Caption:=dlgLazarusDir;

  with LazarusDirComboBox.Items do begin
    BeginUpdate;
    Add(ProgramDirectory);
    EndUpdate;
  end;

  CompilerPathGroupBox.Caption:=Format(dlgFpcPath,[GetDefaultCompilerFilename]);

  with CompilerPathComboBox do begin
    Items.BeginUpdate;
    GetDefaultCompilerFilenames(Items);
    Items.EndUpdate;
  end;

  FPCSourceDirGroupBox.Caption:=dlgFpcSrcPath;

  MakePathGroupBox.Caption:=dlgMakePath;

  with MakePathComboBox.Items do begin
    BeginUpdate;
    Add('/usr/bin/make');
    EndUpdate;
  end;

  TestBuildDirGroupBox.Caption:=dlgTestPrjDir;

  with TestBuildDirComboBox.Items do begin
    BeginUpdate;
    Add('/tmp');
    Add('/var/tmp');
    Add('c:/tmp');
    Add('c:/windows/temp');
    EndUpdate;
  end;
end;

procedure TEnvironmentOptionsDialogNew.SetCategoryPage(
  const AValue: TEnvOptsDialogPage);
var
  p: Integer;
begin
  case AValue of
    eodpFiles: p:=0;
    eodpLanguage, eodpAutoSave, eodpDesktop, eodpMainHints,
    eodpWindowPositions: p:=2;
    eodpFormEditor: p:=3;
    eodpObjectInspector: p:=4;
    eodpBackup: p:=5;
    eodpNaming: p:=6;
  end;
  Notebook.PageIndex:=p;
end;

procedure TEnvironmentOptionsDialogNew.SetupFormEditorPage(Page: integer);

  procedure SetupGridGroupBox;
  begin
    ShowBorderSpaceCheckBox.Caption:=dlgQShowBorderSpacing;
    ShowGridCheckBox.Caption:=dlgQShowGrid;
    GridColorLabel.Caption:=dlgGridColor;
    SnapToGridCheckBox.Caption:=dlgQSnapToGrid;
    GridSizeXComboBox.Hint:=dlgGridXHint;
    GridSizeXLabel.Caption:=dlgGridX;
    GridSizeYComboBox.Hint:=dlgGridYHint;
    GridSizeYLabel.Caption:=dlgGridY;
  end;

  procedure SetupGuideLinesGroupBox;
  begin
    ShowGuideLinesCheckBox.Caption:=dlgGuideLines;
    SnapToGuideLinesCheckBox.Caption:=dlgSnapGuideLines;
    GuideLineColorLeftTopLabel.Caption:=dlgLeftTopClr;
    GuideLineColorRightBottomLabel.Caption:=dlgRightBottomClr;
  end;
  
  procedure SetupMiscGroupBox;
  begin
    ShowComponentCaptionsCheckBox.Caption:=dlgShowCaps;
    ShowEditorHintsCheckBox.Caption:=dlgShowEdrHints;
    AutoCreateFormsOnOpenCheckBox.Caption:=dlgAutoForm;
    RightClickSelectsCheckBox.Caption:=dlgRightClickSelects;
    GrabberColorLabel.Caption:=dlgGrabberColor;
    MarkerColorLabel.Caption:=dlgMarkerColor;

    with DesignerPaintLazyCheckBox do begin
      Caption:=lisFEPaintDesignerItemsOnIdle;
      Hint:=lisFEPaintDesignerItemsOnIdleReduceOverheadForSlowCompu;
    end;
  end;
  
  procedure SetupRubberbandBox;
  begin
    RubberbandSelectColorLabel.Caption:=dlgRuberbandSelectionColor;
    RubberbandCreateColorLabel.Caption:=dlgRuberbandCreationColor;
    RubberbandSelectsGrandChildsCheckBox.Caption:=dlgRubberbandSelectsGrandChilds;
  end;

begin
  // EnvironmentOptionsDialog editor page
  NoteBook.Page[Page].Caption := dlgFrmEditor;

  GridGroupBox.Caption:=dlgEnvGrid;

  SetupGridGroupBox;
  
  GuideLinesGroupBox.Caption:=dlgEnvLGuideLines;

  SetupGuideLinesGroupBox;

  RubberbandGroupBox.Caption:=dlgRubberBandGroup;

  SetupRubberbandBox;

  FormEditMiscGroupBox.Caption:=dlgEnvMisc;

  SetupMiscGroupBox;
end;

procedure TEnvironmentOptionsDialogNew.SetupNamingPage(Page: integer);
var
  pe: TPascalExtType;
begin
  NoteBook.Page[Page].Caption := dlgNaming;

  with PascalFileExtRadiogroup do begin
    Caption:=dlgPasExt;
    with Items do begin
      BeginUpdate;
      for pe:=Low(TPascalExtType) to High(TPascalExtType) do
        if pe<>petNone then
          Add(PascalExtension[pe]);
      EndUpdate;
    end;
    PascalFileExtRadiogroup.Columns:=PascalFileExtRadiogroup.Items.Count;
  end;

  with CharcaseFileActionRadioGroup do begin
    Caption:=dlgCharCaseFileAct;
    with Items do begin
      BeginUpdate;
      Add(dlgEnvAsk);
      Add(dlgAutoRen);
      Add(dlgnoAutomaticRenaming);
      EndUpdate;
    end;
  end;

  with AmbiguousFileActionRadioGroup do begin
    Caption:=dlgAmbigFileAct;
    with Items do begin
      BeginUpdate;
      Add(dlgEnvAsk);
      Add(dlgAutoDel);
      Add(dlgAutoRen);
      Add(dlgAmbigWarn);
      Add(dlgIgnoreVerb);
      EndUpdate;
    end;
  end;
end;

procedure TEnvironmentOptionsDialogNew.SetupLazDocPage(Page: integer);
begin
  NoteBook.Page[Page].Caption := lisLazDoc;
  
  LazDocPathsGroupBox.Caption := lisLazDocPathsGroupBox;
  LazDocAddPathButton.Caption := lisLazDocAddPathButton;
  LazDocDeletePathButton.Caption := lisLazDocDeletePathButton;

  LazDocPathEdit.Clear;
end;

procedure TEnvironmentOptionsDialogNew.BakTypeRadioGroupClick(Sender: TObject);
var i: integer;
begin
  i:=TRadioGroup(Sender).ItemIndex;
  if Sender=BakProjTypeRadioGroup then begin
    BakProjAddExtComboBox.Enabled:=(i=4);
    BakProjAddExtLabel.Enabled:=BakProjAddExtComboBox.Enabled;
    BakProjMaxCounterComboBox.Enabled:=(i=3);
    BakProjMaxCounterLabel.EnableD:=BakProjMaxCounterComboBox.Enabled;
  end else begin
    BakOtherAddExtComboBox.Enabled:=(i=4);
    BakOtherAddExtLabel.Enabled:=BakOtherAddExtComboBox.Enabled;
    BakOtherMaxCounterComboBox.Enabled:=(i=3);
    BakOtherMaxCounterLabel.EnableD:=BakOtherMaxCounterComboBox.Enabled;
  end;
end;

procedure TEnvironmentOptionsDialogNew.FilesButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  AFilename: string;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    // set title
    if Sender=CompilerPathButton then
      OpenDialog.Title:=
                      Format(lisChooseCompilerPath,[GetDefaultCompilerFilename])
    else if Sender=MakePathButton then
      OpenDialog.Title:=lisChooseMakePath
    else
      exit;

    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);

      if Sender=CompilerPathButton then begin
        // check compiler filename
        SetComboBoxText(CompilerPathComboBox,AFilename);
        CheckExecutable(FOldCompilerFilename,CompilerPathComboBox.Text,
          lisEnvOptDlgInvalidCompilerFilename,
          lisEnvOptDlgInvalidCompilerFilenameMsg);
      end else if Sender=MakePathButton then begin
        //check make filename
        SetComboBoxText(MakePathComboBox,AFilename);
        CheckExecutable(FOldMakeFilename,MakePathComboBox.Text,
          lisEnvOptDlgInvalidMakeFilename,
          lisEnvOptDlgInvalidMakeFilenameMsg);
      end;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TEnvironmentOptionsDialogNew.DirectoriesButtonClick(Sender: TObject);
var
  OpenDialog: TSelectDirectoryDialog;
  ADirectoryName: string;
begin
  OpenDialog:=TSelectDirectoryDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    // set title
    if Sender=LazarusDirButton then
      OpenDialog.Title:=lisChooseLazarusSourceDirectory
    else if Sender=FPCSourceDirButton then
      OpenDialog.Title:=lisChooseFPCSourceDir
    else if Sender=TestBuildDirButton then
      OpenDialog.Title:=lisChooseTestBuildDir
    else
      exit;

    if OpenDialog.Execute then begin
      ADirectoryName:=CleanAndExpandDirectory(OpenDialog.Filename);

      if Sender=LazarusDirButton then begin
        // check lazarus directory
        SetComboBoxText(LazarusDirComboBox,ADirectoryName);
        CheckLazarusDir;
      end else if Sender=FPCSourceDirButton then begin
        // check fpc source directory
        SetComboBoxText(FPCSourceDirComboBox,ADirectoryName);
        IsFPCSourceDir;
      end else if Sender=TestBuildDirButton then begin
        // check test directory
        SetComboBoxText(TestBuildDirComboBox,ADirectoryName);
        CheckTestDir;
      end;

    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TEnvironmentOptionsDialogNew.FormEditorPageResize(Sender: TObject);
var
  w: Integer;
begin
  w:=((FormEditorPage.ClientWidth-3*5)*5) div 10;
  GridGroupBox.Width:=w;
  FormEditMiscGroupBox.Width:=GridGroupBox.Width;
end;

procedure TEnvironmentOptionsDialogNew.LazDocAddPathButtonClick(Sender: TObject);
begin
  if LazDocPathEdit.Text <> '' then
    LazDocListBox.Items.Add(LazDocPathEdit.Text);
end;

procedure TEnvironmentOptionsDialogNew.LazDocBrowseButtonClick(Sender: TObject);
begin
  if SelectDirectoryDialog.Execute then
    LazDocPathEdit.Text := SelectDirectoryDialog.FileName;
end;

procedure TEnvironmentOptionsDialogNew.LazDocDeletePathButtonClick(Sender: TObject
  );
begin
  LazDocListBox.Items.Delete(LazDocListBox.ItemIndex);
end;

procedure TEnvironmentOptionsDialogNew.NotebookChangeBounds(Sender: TObject);
begin

end;

procedure TEnvironmentOptionsDialogNew.OkButtonClick(Sender: TObject);
begin
  if not CheckValues then exit;
  IDEDialogLayoutList.SaveLayout(Self);
  ModalResult:=mrOk;
end;

procedure TEnvironmentOptionsDialogNew.CancelButtonClick(Sender: TObject);
begin
  IDEDialogLayoutList.SaveLayout(Self);
  ModalResult:=mrCancel;
end;

procedure TEnvironmentOptionsDialogNew.SaveDesktopSettingsToFileButtonClick(
  Sender: TObject);
var AnEnvironmentOptions: TEnvironmentOptions;
  SaveDialog: TSaveDialog;
begin
  debugln('TEnvironmentOptionsDialogNew.SaveDesktopSettingsToFileButtonClick A');
  SaveDialog:=TSaveDialog.Create(nil);
  try
    try
      InputHistories.ApplyFileDialogSettings(SaveDialog);
      SaveDialog.Filter:=lisLazarusDesktopSettings+' (*.lds)|*.lds'
           +'|'+lisXMLFiles+' (*.xml)|*.xml'
           +'|'+dlgAllFiles+' (*.*)|' + GetAllFilesMask;
      if SaveDialog.Execute then begin
        AnEnvironmentOptions:=TEnvironmentOptions.Create;
        try
          WriteSettings(AnEnvironmentOptions);
          AnEnvironmentOptions.Filename:=SaveDialog.Filename;
          if Assigned(OnSaveEnvironmentSettings) then
            OnSaveEnvironmentSettings(Self,AnEnvironmentOptions);
          AnEnvironmentOptions.Save(true);
        finally
          AnEnvironmentOptions.Free;
        end;
      end;
      InputHistories.StoreFileDialogSettings(SaveDialog);
    except
      on E: Exception do begin
        DebugLn('ERROR: [TEnvironmentOptionsDialogNew.SaveDesktopSettingsToFileButtonClick] ',E.Message);
      end;
    end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TEnvironmentOptionsDialogNew.LoadDesktopSettingsFromFileButtonClick(
  Sender: TObject);
var AnEnvironmentOptions: TEnvironmentOptions;
  OpenDialog: TOpenDialog;
begin
  debugln('TEnvironmentOptionsDialogNew.LoadDesktopSettingsFromFileButtonClick A');
  OpenDialog:=TOpenDialog.Create(nil);
  try
    try
      InputHistories.ApplyFileDialogSettings(OpenDialog);
      OpenDialog.Filter:=lisLazarusDesktopSettings+' (*.lds)|*.lds'
           +'|'+lisXMLFiles+' (*.xml)|*.xml'
           +'|'+dlgAllFiles+' (*.*)|' + GetAllFilesMask;
      if OpenDialog.Execute then begin
        AnEnvironmentOptions:=TEnvironmentOptions.Create;
        try
          AnEnvironmentOptions.Filename:=OpenDialog.Filename;
          AnEnvironmentOptions.Load(true);
          if Assigned(OnLoadEnvironmentSettings) then
            OnLoadEnvironmentSettings(Self,AnEnvironmentOptions);
          ReadSettings(AnEnvironmentOptions);
        finally
          AnEnvironmentOptions.Free;
        end;
      end;
      InputHistories.StoreFileDialogSettings(OpenDialog);
    except
      // ToDo
      DebugLn('ERROR: [TEnvironmentOptionsDialogNew.SaveDesktopSettingsToFileButtonClick]');
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TEnvironmentOptionsDialogNew.ReadSettings(
  AnEnvironmentOptions: TEnvironmentOptions);
var i: integer;
begin
  with AnEnvironmentOptions do begin
    // language
    LanguageComboBox.ItemIndex:=LazarusTranslations.IndexOf(LanguageID);
    //debugln('TEnvironmentOptionsDialogNew.ReadSettings LanguageComboBox.ItemIndex=',dbgs(LanguageComboBox.ItemIndex),' LanguageID="',LanguageID,'"');

    // auto save
    AutoSaveEditorFilesCheckBox.Checked:=AutoSaveEditorFiles;
    AutoSaveProjectCheckBox.Checked:=AutoSaveProject;
    SetComboBoxText(AutoSaveIntervalInSecsComboBox
       ,IntToStr(AutoSaveIntervalInSecs));

    // desktop
    FLayouts:=IDEWindowLayoutList;
    SetWindowPositionsItem(0);

    // object inspector
    OIBackgroundColorButton.ButtonColor:=
       ObjectInspectorOptions.GridBackgroundColor;
    OISubPropsColorButton.ButtonColor:=
       ObjectInspectorOptions.SubPropertiesColor;
    OIReferencesColorButton.ButtonColor:=
       ObjectInspectorOptions.ReferencesColor;
    OIValueColorButton.ButtonColor:=
       ObjectInspectorOptions.ValueColor;
    OIDefaultValueColorButton.ButtonColor:=
       ObjectInspectorOptions.DefaultValueColor;
    OIPropNameColorButton.ButtonColor:=
       ObjectInspectorOptions.PropertyNameColor;

    OIDefaultItemHeightSpinEdit.Value:=ObjectInspectorOptions.DefaultItemHeight;
    OIShowHintCheckBox.Checked := ObjectInspectorOptions.ShowHints;
       
    // window minimizing and hiding
    MinimizeAllOnMinimizeMainCheckBox.Checked:=MinimizeAllOnMinimizeMain;
    HideIDEOnRunCheckBox.Checked:=HideIDEOnRun;

    // hints
    CheckDiskChangesWithLoadingCheckBox.Checked:=
      CheckDiskChangesWithLoading;
    ShowHintsForComponentPaletteCheckBox.Checked:=
      ShowHintsForComponentPalette;
    ShowHintsForMainSpeedButtonsCheckBox.Checked:=
      ShowHintsForMainSpeedButtons;
      
    // messages view
    MsgViewDblClickJumpsCheckBox.Checked:=MsgViewDblClickJumps;

    // EnvironmentOptionsDialog editor
    ShowBorderSpaceCheckBox.Checked:=ShowBorderSpacing;
    ShowGridCheckBox.Checked:=ShowGrid;
    GridColorButton.ButtonColor:=GridColor;
    SnapToGridCheckBox.Checked:=SnapToGrid;
    SetComboBoxText(GridSizeXComboBox,IntToStr(GridSizeX));
    SetComboBoxText(GridSizeYComboBox,IntToStr(GridSizeY));
    ShowGuideLinesCheckBox.Checked:=ShowGuideLines;
    SnapToGuideLinesCheckBox.Checked:=SnapToGuideLines;
    GuideLineColorLeftTopButton.ButtonColor:=GuideLineColorLeftTop;
    GuideLineColorRightBottomButton.ButtonColor:=GuideLineColorRightBottom;
    ShowComponentCaptionsCheckBox.Checked:=ShowComponentCaptions;
    ShowEditorHintsCheckBox.Checked:=ShowEditorHints;
    AutoCreateFormsOnOpenCheckBox.Checked:=AutoCreateFormsOnOpen;
    RightClickSelectsCheckBox.Checked:=RightClickSelects;
    GrabberColorButton.ButtonColor:=GrabberColor;
    MarkerColorButton.ButtonColor:=MarkerColor;
    RubberbandSelectColorButton.ButtonColor:=RubberbandSelectionColor;
    RubberbandCreateColorButton.ButtonColor:=RubberbandCreationColor;
    RubberbandSelectsGrandChildsCheckBox.Checked:=RubberbandSelectsGrandChilds;
    DesignerPaintLazyCheckBox.Checked:=DesignerPaintLazy;

    // files
    LazarusDirComboBox.Items.Assign(LazarusDirHistory);
    FOldLazarusDir:=LazarusDirectory;
    SetComboBoxText(LazarusDirComboBox,LazarusDirectory,MaxComboBoxCount);
    CompilerPathComboBox.Items.Assign(CompilerFileHistory);
    FOldCompilerFilename:=CompilerFilename;
    SetComboBoxText(CompilerPathComboBox,CompilerFilename,MaxComboBoxCount);
    FPCSourceDirComboBox.Items.Assign(FPCSourceDirHistory);
    FOldFPCSourceDir:=FPCSourceDirectory;
    SetComboBoxText(FPCSourceDirComboBox,FPCSourceDirectory,MaxComboBoxCount);
    MakePathComboBox.Items.Assign(MakeFileHistory);
    FOldMakeFilename:=MakeFilename;
    SetComboBoxText(MakePathComboBox,MakeFilename,MaxComboBoxCount);
    TestBuildDirComboBox.Items.Assign(TestBuildDirHistory);
    FOldTestDir:=TestBuildDirectory;
    SetComboBoxText(TestBuildDirComboBox,TestBuildDirectory,MaxComboBoxCount);

    // recent files and directories
    SetComboBoxText(MaxRecentOpenFilesComboBox,IntToStr(MaxRecentOpenFiles));
    SetComboBoxText(MaxRecentProjectFilesComboBox,IntToStr(MaxRecentProjectFiles));
    OpenLastProjectAtStartCheckBox.Checked:=OpenLastProjectAtStart;

    // backup
    with BackupInfoProjectFiles do begin
      case BackupType of
       bakNone:          BakProjTypeRadioGroup.ItemIndex:=0;
       bakSymbolInFront: BakProjTypeRadioGroup.ItemIndex:=1;
       bakSymbolBehind:  BakProjTypeRadioGroup.ItemIndex:=2;
       bakCounter:       BakProjTypeRadioGroup.ItemIndex:=3;
       bakUserDefinedAddExt: BakProjTypeRadioGroup.ItemIndex:=4;
       bakSameName:      BakProjTypeRadioGroup.ItemIndex:=5;
      end;
      SetComboBoxText(BakProjAddExtComboBox,AdditionalExtension);
      if MaxCounter<=0 then
        SetComboBoxText(BakProjMaxCounterComboBox,BakMaxCounterInfiniteTxt)
      else
        SetComboBoxText(BakProjMaxCounterComboBox,IntToStr(MaxCounter));
      if SubDirectory<>'' then
        SetComboBoxText(BakProjSubDirComboBox,SubDirectory)
      else
        SetComboBoxText(BakProjSubDirComboBox,dlgBakNoSubDirectory);
    end;
    BakTypeRadioGroupClick(BakProjTypeRadioGroup);
    with BackupInfoOtherFiles do begin
      case BackupType of
       bakNone:          BakOtherTypeRadioGroup.ItemIndex:=0;
       bakSymbolInFront: BakOtherTypeRadioGroup.ItemIndex:=1;
       bakSymbolBehind:  BakOtherTypeRadioGroup.ItemIndex:=2;
       bakCounter:       BakOtherTypeRadioGroup.ItemIndex:=3;
       bakUserDefinedAddExt: BakOtherTypeRadioGroup.ItemIndex:=4;
       bakSameName:      BakOtherTypeRadioGroup.ItemIndex:=5;
      end;
      SetComboBoxText(BakOtherAddExtComboBox,AdditionalExtension);
      if MaxCounter<=0 then
        SetComboBoxText(BakOtherMaxCounterComboBox,BakMaxCounterInfiniteTxt)
      else
        SetComboBoxText(BakOtherMaxCounterComboBox,IntToStr(MaxCounter));
      if SubDirectory<>'' then
        SetComboBoxText(BakOtherSubDirComboBox,SubDirectory)
      else
        SetComboBoxText(BakOtherSubDirComboBox,dlgBakNoSubDirectory);
    end;
    BakTypeRadioGroupClick(BakOtherTypeRadioGroup);
    
    // naming
    for i:=0 to PascalFileExtRadiogroup.Items.Count-1 do
      if PascalFileExtRadiogroup.Items[i]=PascalExtension[PascalFileExtension]
      then PascalFileExtRadiogroup.ItemIndex:=i;

    CharCaseFileActionRadioGroup.ItemIndex  := ord(CharCaseFileAction);
    AmbiguousFileActionRadioGroup.ItemIndex := ord(AmbiguousFileAction);
    
    //lazdoc
    SplitString(LazDocPaths,';',LazDocListBox.Items);
  end;
end;

procedure TEnvironmentOptionsDialogNew.WriteSettings(
  AnEnvironmentOptions: TEnvironmentOptions);
begin
  with AnEnvironmentOptions do begin
    // language
    if (LanguageComboBox.ItemIndex>=0)
    and (LanguageComboBox.ItemIndex<LazarusTranslations.Count) then
      LanguageID:=LazarusTranslations[LanguageComboBox.ItemIndex].ID;
    //debugln('TEnvironmentOptionsDialogNew.WriteSettings A LanguageID="',LanguageID,'" LanguageComboBox.ItemIndex=',dbgs(LanguageComboBox.ItemIndex),' ',dbgs(LanguageComboBox.HandleAllocated));

    // auto save
    AutoSaveEditorFiles:=AutoSaveEditorFilesCheckBox.Checked;
    AutoSaveProject:=AutoSaveProjectCheckBox.Checked;
    AutoSaveIntervalInSecs:=StrToIntDef(
      AutoSaveIntervalInSecsComboBox.Text,AutoSaveIntervalInSecs);

    // desktop
    WindowPositionsBox.Save;

    // object inspector
    ObjectInspectorOptions.GridBackgroundColor:=
       OIBackgroundColorButton.ButtonColor;
    ObjectInspectorOptions.SubPropertiesColor:=
       OISubPropsColorButton.ButtonColor;
    ObjectInspectorOptions.ReferencesColor:=
       OIReferencesColorButton.ButtonColor;
    ObjectInspectorOptions.ValueColor:=
       OIValueColorButton.ButtonColor;
    ObjectInspectorOptions.DefaultValueColor:=
       OIDefaultValueColorButton.ButtonColor;
    ObjectInspectorOptions.PropertyNameColor:=
       OIPropNameColorButton.ButtonColor;

    ObjectInspectorOptions.DefaultItemHeight:=
       RoundToInt(OIDefaultItemHeightSpinEdit.Value);
    ObjectInspectorOptions.ShowHints := OIShowHintCheckBox.Checked;

    // window minimizing
    MinimizeAllOnMinimizeMain:=MinimizeAllOnMinimizeMainCheckBox.Checked;
    HideIDEOnRun:=HideIDEOnRunCheckBox.Checked;

    // hints
    CheckDiskChangesWithLoading:=CheckDiskChangesWithLoadingCheckBox.Checked;
    ShowHintsForComponentPalette:=ShowHintsForComponentPaletteCheckBox.Checked;
    ShowHintsForMainSpeedButtons:=ShowHintsForMainSpeedButtonsCheckBox.Checked;
    
    // messages view
    MsgViewDblClickJumps:=MsgViewDblClickJumpsCheckBox.Checked;

    // EnvironmentOptionsDialog editor
    ShowBorderSpacing:=ShowBorderSpaceCheckBox.Checked;
    ShowGrid:=ShowGridCheckBox.Checked;
    GridColor:=GridColorButton.ButtonColor;
    SnapToGrid:=SnapToGridCheckBox.Checked;
    GridSizeX:=StrToIntDef(GridSizeXComboBox.Text,GridSizeX);
    GridSizeY:=StrToIntDef(GridSizeYComboBox.Text,GridSizeY);
    ShowGuideLines:=ShowGuideLinesCheckBox.Checked;
    SnapToGuideLines:=SnapToGuideLinesCheckBox.Checked;
    GuideLineColorLeftTop:=GuideLineColorLeftTopButton.ButtonColor;
    GuideLineColorRightBottom:=GuideLineColorRightBottomButton.ButtonColor;
    ShowComponentCaptions:=ShowComponentCaptionsCheckBox.Checked;
    ShowEditorHints:=ShowEditorHintsCheckBox.Checked;
    AutoCreateFormsOnOpen:=AutoCreateFormsOnOpenCheckBox.Checked;
    RightClickSelects:=RightClickSelectsCheckBox.Checked;
    GrabberColor:=GrabberColorButton.ButtonColor;
    MarkerColor:=MarkerColorButton.ButtonColor;
    RubberbandSelectionColor:=RubberbandSelectColorButton.ButtonColor;
    RubberbandCreationColor:=RubberbandCreateColorButton.ButtonColor;
    RubberbandSelectsGrandChilds:=RubberbandSelectsGrandChildsCheckBox.Checked;
    DesignerPaintLazy:=DesignerPaintLazyCheckBox.Checked;

    // files
    LazarusDirectory:=LazarusDirComboBox.Text;
    LazarusDirHistory.Assign(LazarusDirComboBox.Items);
    CompilerFilename:=CompilerPathComboBox.Text;
    CompilerFileHistory.Assign(CompilerPathComboBox.Items);
    FPCSourceDirectory:=FPCSourceDirComboBox.Text;
    FPCSourceDirHistory.Assign(FPCSourceDirComboBox.Items);
    MakeFilename:=MakePathComboBox.Text;
    MakeFileHistory.Assign(MakePathComboBox.Items);
    TestBuildDirHistory.Assign(TestBuildDirComboBox.Items);
    TestBuildDirectory:=TestBuildDirComboBox.Text;

    // recent files and directories
    MaxRecentOpenFiles:=StrToIntDef(
        MaxRecentOpenFilesComboBox.Text,MaxRecentOpenFiles);
    MaxRecentProjectFiles:=StrToIntDef(
        MaxRecentProjectFilesComboBox.Text,MaxRecentProjectFiles);
    OpenLastProjectAtStart:=OpenLastProjectAtStartCheckBox.Checked;

    // backup
    with BackupInfoProjectFiles do begin
      case BakProjTypeRadioGroup.ItemIndex of
       0: BackupType:=bakNone;
       1: BackupType:=bakSymbolInFront;
       2: BackupType:=bakSymbolBehind;
       3: BackupType:=bakCounter;
       4: BackupType:=bakUserDefinedAddExt;
       5: BackupType:=bakSameName;
      end;
      AdditionalExtension:=BakProjAddExtComboBox.Text;
      if BakProjMaxCounterComboBox.Text=BakMaxCounterInfiniteTxt then
        MaxCounter:=0
      else
        MaxCounter:=StrToIntDef(BakProjMaxCounterComboBox.Text,1);
      SubDirectory:=BakProjSubDirComboBox.Text;
      if SubDirectory=dlgBakNoSubDirectory then
        SubDirectory:='';
    end;
    with BackupInfoOtherFiles do begin
      case BakOtherTypeRadioGroup.ItemIndex of
       0: BackupType:=bakNone;
       1: BackupType:=bakSymbolInFront;
       2: BackupType:=bakSymbolBehind;
       3: BackupType:=bakCounter;
       4: BackupType:=bakUserDefinedAddExt;
       5: BackupType:=bakSameName;
      end;
      AdditionalExtension:=BakOtherAddExtComboBox.Text;
      if BakOtherMaxCounterComboBox.Text=BakMaxCounterInfiniteTxt then
        MaxCounter:=0
      else
        MaxCounter:=StrToIntDef(BakOtherMaxCounterComboBox.Text,1);
      if BakOtherSubDirComboBox.Text=dlgBakNoSubDirectory then
        SubDirectory:=''
      else
        SubDirectory:=BakOtherSubDirComboBox.Text;
    end;
    
    // naming
    if PascalFileExtRadiogroup.ItemIndex>=0 then
      PascalFileExtension:=PascalExtToType(
        PascalFileExtRadiogroup.Items[PascalFileExtRadiogroup.ItemIndex])
    else
      PascalFileExtension:=petPAS;

    //lazdoc
    LazDocPaths:=StringListToText(LazDocListBox.Items,';',true);

    CharcaseFileAction  := TCharCaseFileAction(CharcaseFileActionRadioGroup.ItemIndex);
    AmbiguousFileAction := TAmbiguousFileAction(AmbiguousFileActionRadioGroup.ItemIndex);
  end;
end;

procedure TEnvironmentOptionsDialogNew.SetupObjectInspectorPage(Page: integer);
begin
  NoteBook.Page[Page].Caption := dlgObjInsp;

  // object inspector
  ObjectInspectorColorsGroupBox.Caption:=dlgEnvColors;
  OIBackgroundColorLabel.Caption:=dlgBackColor;
  OISubPropsColorLabel.Caption:=dlgSubPropkColor;
  OIReferencesColorLabel.Caption:=dlgReferenceColor;
  OIValueColorLabel.Caption:=dlgValueColor;
  OIDefaultValueColorLabel.Caption:=dlgDefValueColor;
  OIPropNameColorLabel.Caption:=dlgPropNameColor;
  OIMiscGroupBox.Caption:=dlgOIMiscellaneous;
  OIDefaultItemHeightLabel.Caption:=dlgOIItemHeight;
  OIShowHintCheckBox.Caption := lisShowHintsInObjectInspector;
end;

procedure TEnvironmentOptionsDialogNew.WindowPositionsListBoxMouseUp(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  SetWindowPositionsItem(WindowPositionsListBox.ItemIndex);
end;

procedure TEnvironmentOptionsDialogNew.SetWindowPositionsItem(Index: integer);
begin
  if WindowPositionsBox.Layout<>nil then
    WindowPositionsBox.Save;
  WindowPositionsListBox.ItemIndex:=Index;
  case Index of
  0: WindowPositionsBox.Layout:=FLayouts.ItemByEnum(nmiwMainIDEName);
  1: WindowPositionsBox.Layout:=FLayouts.ItemByEnum(nmiwSourceNoteBookName);
  2: WindowPositionsBox.Layout:=FLayouts.ItemByEnum(nmiwMessagesViewName);
  3: WindowPositionsBox.Layout:=FLayouts.ItemByFormID(DefaultObjectInspectorName);
  4: WindowPositionsBox.Layout:=FLayouts.ItemByEnum(nmiwProjectInspector);
  5: WindowPositionsBox.Layout:=FLayouts.ItemByEnum(nmiwCodeExplorerName);
  6: WindowPositionsBox.Layout:=FLayouts.ItemByEnum(nmiwPkgGraphExplorer);
  7: WindowPositionsBox.Layout:=FLayouts.ItemByEnum(nmiwUnitDependenciesName);
  8: WindowPositionsBox.Layout:=FLayouts.ItemByEnum(nmiwLazDocName);
  end;
  if Index>=0 then
    WindowPositionsBox.Caption:=WindowPositionsListBox.Items[Index];
end;

function TEnvironmentOptionsDialogNew.CheckLazarusDir: boolean;
var
  NewLazarusDir: string;
  StopChecking: boolean;
begin
  NewLazarusDir:=LazarusDirComboBox.Text;
  Result:=SimpleDirectoryCheck(FOldLazarusDir,NewLazarusDir,
                              lisEnvOptDlgLazarusDirNotFoundMsg,StopChecking);
  if (not Result) or StopChecking then exit;

  // lazarus directory specific tests
  NewLazarusDir:=AppendPathDelim(NewLazarusDir);
  if not CheckLazarusDirectory(NewLazarusDir)
  then begin
    Result:=(MessageDlg(Format(lisEnvOptDlgInvalidLazarusDir,[NewLazarusDir]),
               mtWarning,[mbIgnore,mbCancel],0)=mrIgnore);
    exit;
  end;
  Result:=true;
end;

function TEnvironmentOptionsDialogNew.IsFPCSourceDir: boolean;
var
  NewFPCSrcDir: string;
  StopChecking: boolean;
begin
  NewFPCSrcDir:=FPCSourceDirComboBox.Text;
  Result:=SimpleDirectoryCheck(FOldFPCSourceDir,NewFPCSrcDir,
                               lisEnvOptDlgFPCSrcDirNotFoundMsg,StopChecking);
  if (not Result) or StopChecking then exit;

  // FPC source directory specific tests
  if not CheckFPCSourceDir(NewFPCSrcDir) then begin
    Result:=(MessageDlg(Format(lisEnvOptDlgInvalidFPCSrcDir,[NewFPCSrcDir]),
               mtWarning,[mbIgnore,mbCancel],0)=mrIgnore);
    exit;
  end;
  Result:=true;
end;

function TEnvironmentOptionsDialogNew.CheckTestDir: boolean;
var
  NewTestDir: string;
  StopChecking: boolean;
begin
  NewTestDir:=TestBuildDirComboBox.Text;
  Result:=SimpleDirectoryCheck(FOldTestDir,NewTestDir,
                               lisEnvOptDlgTestDirNotFoundMsg,StopChecking);
  if (not Result) or StopChecking then exit;
end;

function TEnvironmentOptionsDialogNew.CheckValues: boolean;
begin
  Result:=false;
  // check lazarus directory
  if not CheckLazarusDir then exit;
  // check compiler filename
  if not CheckExecutable(FOldCompilerFilename,CompilerPathComboBox.Text,
    lisEnvOptDlgInvalidCompilerFilename,lisEnvOptDlgInvalidCompilerFilenameMsg)
  then exit;
  // check fpc source directory
  if not IsFPCSourceDir then exit;
  // check make filename
  if not CheckExecutable(FOldMakeFilename,MakePathComboBox.Text,
    lisEnvOptDlgInvalidMakeFilename,lisEnvOptDlgInvalidMakeFilenameMsg)
  then exit;
  // check test directory
  if not CheckTestDir then exit;
  
  Result:=true;
end;

initialization
  {$I environmentopts_new.lrs}

end.

