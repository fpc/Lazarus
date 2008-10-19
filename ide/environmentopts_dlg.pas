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
  options_files, options_desktop, options_window, options_formed, options_oi;

type
  { TEnvironmentOptionsDialog: EnvironmentOptionsDialog for environment options }
  
  TEnvOptsDialogPage = (
    eodpLanguage,
    eodpAutoSave,
    eodpDesktop,
    eodpMainHints,
    eodpWindowPositions,
    eodpFormEditor,
    eodpObjectInspector,
    eodpFiles,
    eodpBackup,
    eodpNaming
  );
  
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
    procedure HelpButtonClick(Sender: TObject);
    procedure LazDocAddPathButtonClick(Sender: TObject);
    procedure LazDocBrowseButtonClick(Sender: TObject);
    procedure LazDocDeletePathButtonClick(Sender: TObject);
    procedure NotebookChangeBounds(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    FOnLoadEnvironmentSettings: TOnLoadEnvironmentSettings;
    FOnSaveEnvironmentSettings: TOnSaveEnvironmentSettings;
    FilesOptionsFrame: TFilesOptionsFrame;
    DesktopOptionsFrame: TDesktopOptionsFrame;
    WindowOptionsFrame: TWindowOptionsFrame;
    FormEditorOptionsFrame: TFormEditorOptionsFrame;
    OIOptionsFrame: TOIOptionsFrame;

    procedure SetupFrame(AFrame: TAbstractOptionsFrame; APage: TPage);

    procedure SetCategoryPage(const AValue: TEnvOptsDialogPage);
    procedure SetupFilesPage(Page: integer);
    procedure SetupDesktopPage(Page: integer);
    procedure SetupWindowsPage(Page: integer);
    procedure SetupFormEditorPage(Page: integer);
    procedure SetupObjectInspectorPage(Page: integer);
    procedure SetupBackupPage(Page: integer);
    procedure SetupNamingPage(Page: integer);
    procedure SetupLazDocPage(Page: integer);
    function CheckValues: boolean;

    procedure LoadEnvironmentSettings(Sender: TObject; AOptions: TEnvironmentOptions);
    procedure SaveEnvironmentSettings(Sender: TObject; AOptions: TEnvironmentOptions);
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
begin
  DesktopOptionsFrame := TDesktopOptionsFrame.Create(Self);
  SetupFrame(DesktopOptionsFrame, NoteBook.Page[Page]);
end;

procedure TEnvironmentOptionsDialog.SetupWindowsPage(Page: integer);
begin
  WindowOptionsFrame := TWindowOptionsFrame.Create(Self);
  SetupFrame(WindowOptionsFrame, NoteBook.Page[Page]);
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

procedure TEnvironmentOptionsDialog.SetupFrame(AFrame: TAbstractOptionsFrame; APage: TPage);
begin
  AFrame.Parent := APage;

  AFrame.Anchors := [akLeft, akTop, akRight, akBottom];
  AFrame.AnchorSideLeft.Control := APage;
  AFrame.AnchorSideTop.Control := APage;
  AFrame.AnchorSideRight.Side := asrBottom;
  AFrame.AnchorSideRight.Control := APage;
  AFrame.AnchorSideBottom.Side := asrBottom;
  AFrame.AnchorSideBottom.Control := APage;
  AFrame.BorderSpacing.Around := 6;

  AFrame.OnLoadEnvironmentSettings := @LoadEnvironmentSettings;
  AFrame.OnSaveEnvironmentSettings := @SaveEnvironmentSettings;

  AFrame.Setup;
  AFrame.Visible := True;

  APage.Caption := AFrame.GetTitle;
end;

procedure TEnvironmentOptionsDialog.SetupFilesPage(Page: integer);
begin
  FilesOptionsFrame := TFilesOptionsFrame.Create(Self);
  SetupFrame(FilesOptionsFrame, NoteBook.Page[Page]);
end;

procedure TEnvironmentOptionsDialog.SetCategoryPage(const AValue: TEnvOptsDialogPage);
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
begin
  FormEditorOptionsFrame := TFormEditorOptionsFrame.Create(Self);
  SetupFrame(FormEditorOptionsFrame, NoteBook.Page[Page]);
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

procedure TEnvironmentOptionsDialog.ReadSettings(AnEnvironmentOptions: TEnvironmentOptions);
var 
  i: integer;
begin
  with AnEnvironmentOptions do 
  begin
    // Files
    FilesOptionsFrame.ReadSettings(AnEnvironmentOptions);

    // Desktop
    DesktopOptionsFrame.ReadSettings(AnEnvironmentOptions);

    // Window
    WindowOptionsFrame.ReadSettings(AnEnvironmentOptions);

    // Object inspector
    OIOptionsFrame.ReadSettings(AnEnvironmentOptions);

    // Form editor
    FormEditorOptionsFrame.ReadSettings(AnEnvironmentOptions);

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
    // Files
    FilesOptionsFrame.WriteSettings(AnEnvironmentOptions);

    // Desktop
    DesktopOptionsFrame.WriteSettings(AnEnvironmentOptions);

    // Window
    WindowOptionsFrame.WriteSettings(AnEnvironmentOptions);

    // Object inspector
    OIOptionsFrame.WriteSettings(AnEnvironmentOptions);

    // Form editor
    FormEditorOptionsFrame.WriteSettings(AnEnvironmentOptions);

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
  SetupFrame(OIOptionsFrame, NoteBook.Page[Page]);
end;

function TEnvironmentOptionsDialog.CheckValues: boolean;
begin
  Result:=false;
  if not FilesOptionsFrame.Check then
    Exit;  
  Result:=true;
end;

procedure TEnvironmentOptionsDialog.LoadEnvironmentSettings(Sender: TObject;
  AOptions: TEnvironmentOptions);
begin
  if Assigned(OnLoadEnvironmentSettings) then
    OnLoadEnvironmentSettings(Self, AOptions);
  ReadSettings(AOptions);
end;

procedure TEnvironmentOptionsDialog.SaveEnvironmentSettings(Sender: TObject;
  AOptions: TEnvironmentOptions);
begin
  WriteSettings(AOptions);
  if Assigned(OnSaveEnvironmentSettings) then
    OnSaveEnvironmentSettings(Self, AOptions);
end;

initialization
  {$I environmentopts_dlg.lrs}

end.

