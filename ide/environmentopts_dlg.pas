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
    This unit defines a dialog for the lazarus environment options.

}
unit EnvironmentOpts_Dlg;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  Classes, SysUtils, LCLProc, Forms, Controls, Buttons, GraphType,
  Graphics, ExtCtrls, StdCtrls, Spin, FileUtil, LResources, Dialogs,
  Laz_XMLCfg,
  ObjectInspector, IDEWindowIntf,
  LazarusIDEStrConsts, TransferMacros, LazConf, ExtToolDialog, IDEProcs,
  IDEOptionDefs, InputHistory, EditorOptions, IDETranslations, ButtonPanel,
  EnvironmentOpts,
  options_oi, options_files;

type
  TOnLoadEnvironmentSettings = procedure (Sender: TObject;
        EnvironmentOptions: TEnvironmentOptions) of object;
  TOnSaveEnvironmentSettings = procedure (Sender: TObject;
        EnvironmentOptions: TEnvironmentOptions) of object;

  { TEnvironmentOptionsDialog: EnvironmentOptionsDialog for environment options }
  
  TEnvOptsDialogPage = (eodpLanguage, eodpAutoSave, eodpDesktop, eodpMainHints,
    eodpWindowPositions, eodpFormEditor, eodpObjectInspector, eodpFiles,
    eodpBackup, eodpNaming);
  
  { TEnvironmentOptionsDialog }

  TEnvironmentOptionsDialog = class(TForm)
    ButtonPanel: TButtonPanel;
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
    MsgViewFocusCheckBox: TCheckBox;

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
    procedure FormEditorPageResize(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure LazDocAddPathButtonClick(Sender: TObject);
    procedure LazDocBrowseButtonClick(Sender: TObject);
    procedure LazDocDeletePathButtonClick(Sender: TObject);
    procedure NotebookChangeBounds(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure SaveDesktopSettingsToFileButtonClick(Sender: TObject);
    procedure LoadDesktopSettingsFromFileButtonClick(Sender: TObject);
    procedure WindowPositionsListBoxSelectionChange(Sender: TObject;
      User: boolean);
  private
    FOnLoadEnvironmentSettings: TOnLoadEnvironmentSettings;
    FOnSaveEnvironmentSettings: TOnSaveEnvironmentSettings;
    FLayouts: TIDEWindowLayoutList;
    OIOptionsFrame: TOIOptionsFrame;
    FilesOptionsFrame: TFilesOptionsFrame;

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
    function LangIDToCaption(const LangID: string): string;
    function CaptionToLangID(const ACaption: string): string;
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
  end;

 
implementation

uses
  IDEContextHelpEdit;

{ TEnvironmentOptions }

const
  EnvOptsConfFileName='environmentoptions.xml';
  BakMaxCounterInfiniteTxt = 'infinite';


{ TEnvironmentOptionsDialog }

constructor TEnvironmentOptionsDialog.Create(TheOwner: TComponent);
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

  ButtonPanel.OKButton.OnClick := @OKButtonClick;
  ButtonPanel.CancelButton.OnClick := @CancelButtonClick;
  ButtonPanel.HelpButton.OnClick := @HelpButtonClick;
end;

procedure TEnvironmentOptionsDialog.SetupDesktopPage(Page: integer);
var
  i: Integer;
  LangID: String;
  sl: TStringList;
begin
  NoteBook.Page[Page].Caption := dlgDesktop;
  
  // language
  LanguageGroupBox.Caption:=dlgEnvLanguage;

  // languages: first the automatic, then sorted the rest
  sl:=TStringList.Create;
  for i:=0 to LazarusTranslations.Count-1 do begin
    LangID:=LazarusTranslations[i].ID;
    if LangID<>'' then
      sl.Add(LangIDToCaption(LangID));
  end;
  sl.Sort;
  sl.Insert(0,GetLazarusLanguageLocalizedName(''));
  LanguageComboBox.Items.Assign(sl);
  sl.Free;

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
  MsgViewFocusCheckBox.Caption:=dlgEOFocusMessagesAfterCompilation;
end;

procedure TEnvironmentOptionsDialog.SetupWindowsPage(Page: integer);
begin
  NoteBook.Page[Page].Caption := dlgWindow;

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
    Add(lisMenuFPDocEditor);
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

procedure TEnvironmentOptionsDialog.SetupBackupPage(Page: integer);
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

procedure TEnvironmentOptionsDialog.SetupFilesPage(Page: integer);
begin
  FilesOptionsFrame := TFilesOptionsFrame.Create(Self);
  FilesOptionsFrame.Parent := FilesPage;

  FilesOptionsFrame.Anchors := [akLeft, akTop, akRight];
  FilesOptionsFrame.AnchorSideLeft.Control := FilesPage;
  FilesOptionsFrame.AnchorSideTop.Control := FilesPage;
  FilesOptionsFrame.AnchorSideRight.Side := asrBottom;
  FilesOptionsFrame.AnchorSideRight.Control := FilesPage;
  FilesOptionsFrame.BorderSpacing.Around := 6;

  FilesOptionsFrame.Setup;
  FilesOptionsFrame.Visible := True;

  FilesPage.Caption := FilesOptionsFrame.GetTitle;
end;

procedure TEnvironmentOptionsDialog.SetCategoryPage(
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

procedure TEnvironmentOptionsDialog.SetupFormEditorPage(Page: integer);

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

procedure TEnvironmentOptionsDialog.SetupNamingPage(Page: integer);
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

procedure TEnvironmentOptionsDialog.SetupLazDocPage(Page: integer);
begin
  NoteBook.Page[Page].Caption := lisFPDocEditor;
  
  LazDocPathsGroupBox.Caption := lisCodeHelpPathsGroupBox;
  LazDocAddPathButton.Caption := lisCodeHelpAddPathButton;
  LazDocDeletePathButton.Caption := lisCodeHelpDeletePathButton;

  LazDocPathEdit.Clear;
end;

procedure TEnvironmentOptionsDialog.BakTypeRadioGroupClick(Sender: TObject);
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

procedure TEnvironmentOptionsDialog.FormEditorPageResize(Sender: TObject);
var
  w: Integer;
begin
  w:=((FormEditorPage.ClientWidth-3*5)*5) div 10;
  GridGroupBox.Width:=w;
  FormEditMiscGroupBox.Width:=GridGroupBox.Width;
end;

procedure TEnvironmentOptionsDialog.HelpButtonClick(Sender: TObject);
begin
  ShowContextHelpForIDE(Self);
end;

procedure TEnvironmentOptionsDialog.LazDocAddPathButtonClick(Sender: TObject);
begin
  if LazDocPathEdit.Text <> '' then
    LazDocListBox.Items.Add(LazDocPathEdit.Text);
end;

procedure TEnvironmentOptionsDialog.LazDocBrowseButtonClick(Sender: TObject);
begin
  if SelectDirectoryDialog.Execute then
    LazDocPathEdit.Text := SelectDirectoryDialog.FileName;
end;

procedure TEnvironmentOptionsDialog.LazDocDeletePathButtonClick(Sender: TObject
  );
begin
  LazDocListBox.Items.Delete(LazDocListBox.ItemIndex);
end;

procedure TEnvironmentOptionsDialog.NotebookChangeBounds(Sender: TObject);
begin

end;

procedure TEnvironmentOptionsDialog.OkButtonClick(Sender: TObject);
begin
  if not CheckValues then exit;
  IDEDialogLayoutList.SaveLayout(Self);
  ModalResult:=mrOk;
end;

procedure TEnvironmentOptionsDialog.CancelButtonClick(Sender: TObject);
begin
  IDEDialogLayoutList.SaveLayout(Self);
  ModalResult:=mrCancel;
end;

procedure TEnvironmentOptionsDialog.SaveDesktopSettingsToFileButtonClick(
  Sender: TObject);
var AnEnvironmentOptions: TEnvironmentOptions;
  SaveDialog: TSaveDialog;
begin
  debugln('TEnvironmentOptionsDialog.SaveDesktopSettingsToFileButtonClick A');
  SaveDialog:=TSaveDialog.Create(nil);
  try
    try
      InputHistories.ApplyFileDialogSettings(SaveDialog);
      SaveDialog.Filter:=lisLazarusDesktopSettings+' (*.lds)|*.lds'
           +'|'+lisXMLFiles+' (*.xml)|*.xml'
           +'|'+dlgAllFiles+' ('+GetAllFilesMask+')|' + GetAllFilesMask;
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
        DebugLn('ERROR: [TEnvironmentOptionsDialog.SaveDesktopSettingsToFileButtonClick] ',E.Message);
      end;
    end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TEnvironmentOptionsDialog.LoadDesktopSettingsFromFileButtonClick(
  Sender: TObject);
var AnEnvironmentOptions: TEnvironmentOptions;
  OpenDialog: TOpenDialog;
begin
  debugln('TEnvironmentOptionsDialog.LoadDesktopSettingsFromFileButtonClick A');
  OpenDialog:=TOpenDialog.Create(nil);
  try
    try
      InputHistories.ApplyFileDialogSettings(OpenDialog);
      OpenDialog.Filter:=lisLazarusDesktopSettings+' (*.lds)|*.lds'
           +'|'+lisXMLFiles+' (*.xml)|*.xml'
           +'|'+dlgAllFiles+' ('+GetAllFilesMask+')|' + GetAllFilesMask;
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
      DebugLn('ERROR: [TEnvironmentOptionsDialog.SaveDesktopSettingsToFileButtonClick]');
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TEnvironmentOptionsDialog.ReadSettings(AnEnvironmentOptions: TEnvironmentOptions);
var 
  i: integer;
begin
  with AnEnvironmentOptions do 
  begin
    // language
    LanguageComboBox.Text:=LangIDToCaption(LanguageID);
    //debugln('TEnvironmentOptionsDialog.ReadSettings LanguageComboBox.ItemIndex=',dbgs(LanguageComboBox.ItemIndex),' LanguageID="',LanguageID,'" LanguageComboBox.Text="',LanguageComboBox.Text,'"');

    // auto save
    AutoSaveEditorFilesCheckBox.Checked:=AutoSaveEditorFiles;
    AutoSaveProjectCheckBox.Checked:=AutoSaveProject;
    SetComboBoxText(AutoSaveIntervalInSecsComboBox
       ,IntToStr(AutoSaveIntervalInSecs));

    // desktop
    FLayouts:=IDEWindowLayoutList;
    SetWindowPositionsItem(0);

    // Object inspector
    OIOptionsFrame.ReadSettings(AnEnvironmentOptions);

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
    MsgViewFocusCheckBox.Checked:=MsgViewFocus;

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
    FilesOptionsFrame.ReadSettings(AnEnvironmentOptions);

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

procedure TEnvironmentOptionsDialog.WriteSettings(AnEnvironmentOptions: TEnvironmentOptions);
begin
  with AnEnvironmentOptions do 
  begin
    // language
    LanguageID:=CaptionToLangID(LanguageComboBox.Text);
    //debugln('TEnvironmentOptionsDialog.WriteSettings A LanguageID="',LanguageID,'" LanguageComboBox.ItemIndex=',dbgs(LanguageComboBox.ItemIndex),' LanguageComboBox.Text=',LanguageComboBox.Text);

    // auto save
    AutoSaveEditorFiles:=AutoSaveEditorFilesCheckBox.Checked;
    AutoSaveProject:=AutoSaveProjectCheckBox.Checked;
    AutoSaveIntervalInSecs:=StrToIntDef(
      AutoSaveIntervalInSecsComboBox.Text,AutoSaveIntervalInSecs);

    // desktop
    WindowPositionsBox.Save;

    // Object inspector
    OIOptionsFrame.WriteSettings(AnEnvironmentOptions);

    // window minimizing
    MinimizeAllOnMinimizeMain:=MinimizeAllOnMinimizeMainCheckBox.Checked;
    HideIDEOnRun:=HideIDEOnRunCheckBox.Checked;

    // hints
    CheckDiskChangesWithLoading:=CheckDiskChangesWithLoadingCheckBox.Checked;
    ShowHintsForComponentPalette:=ShowHintsForComponentPaletteCheckBox.Checked;
    ShowHintsForMainSpeedButtons:=ShowHintsForMainSpeedButtonsCheckBox.Checked;
    
    // messages view
    MsgViewDblClickJumps:=MsgViewDblClickJumpsCheckBox.Checked;
    MsgViewFocus:=MsgViewFocusCheckBox.Checked;

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
    FilesOptionsFrame.WriteSettings(AnEnvironmentOptions);

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

procedure TEnvironmentOptionsDialog.SetupObjectInspectorPage(Page: integer);
begin
  OIOptionsFrame := TOIOptionsFrame.Create(Self);
  OIOptionsFrame.Parent := ObjectInspectorPage;

  OIOptionsFrame.Anchors := [akLeft, akTop, akRight];
  OIOptionsFrame.AnchorSideLeft.Control := ObjectInspectorPage;
  OIOptionsFrame.AnchorSideTop.Control := ObjectInspectorPage;
  OIOptionsFrame.AnchorSideRight.Side := asrBottom;
  OIOptionsFrame.AnchorSideRight.Control := ObjectInspectorPage;
  OIOptionsFrame.BorderSpacing.Around := 6;

  OIOptionsFrame.Setup;
  OIOptionsFrame.Visible := True;

  ObjectInspectorPage.Caption := OIOptionsFrame.GetTitle;
end;

procedure TEnvironmentOptionsDialog.WindowPositionsListBoxSelectionChange(
  Sender: TObject; User: boolean);
begin
  if User then
    SetWindowPositionsItem(WindowPositionsListBox.ItemIndex);
end;

procedure TEnvironmentOptionsDialog.SetWindowPositionsItem(Index: integer);
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
  8: WindowPositionsBox.Layout:=FLayouts.ItemByEnum(nmiwFPDocEditorName);
  end;
  if Index>=0 then
    WindowPositionsBox.Caption:=WindowPositionsListBox.Items[Index];
end;

function TEnvironmentOptionsDialog.LangIDToCaption(const LangID: string
  ): string;
begin
  if LangID<>'' then
    Result:=GetLazarusLanguageLocalizedName(LangID)+' ['+LangID+']'
  else
    //No [] if automatic
    Result:=GetLazarusLanguageLocalizedName(LangID);
end;

function TEnvironmentOptionsDialog.CaptionToLangID(const ACaption: string
  ): string;
var
  i: Integer;
begin
  for i:=0 to LazarusTranslations.Count-1 do begin
    Result:=LazarusTranslations[i].ID;
    if ACaption=LangIDToCaption(Result) then exit;
  end;
  Result:='';
end;

function TEnvironmentOptionsDialog.CheckValues: boolean;
begin
  Result:=false;
  if not FilesOptionsFrame.Check then
    Exit;  
  Result:=true;
end;

initialization
  {$I environmentopts_dlg.lrs}

end.

