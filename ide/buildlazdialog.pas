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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner
  Converted to lfm by: Matthijs Willemstein
  Quickoptions added by: Giuliano Colla
  Then extensively modified by: Juha Manninen
   - added support for Build Profiles which extend the idea of Quick Options.
   - changed UI to be less weird and comply better with UI design norms.
   - changed object structure to keep it logical and to avoid duplicate data.

  Abstract:
    Defines settings for the "Build Lazarus" function of the IDE.
    TConfigureBuildLazarusDlg is used to edit the build options.

    The BuildLazarus function will build the lazarus parts.

    Building occurs only with options defined in the Detail Page.
    Profiles are just used to set options there. Therefore beginners can
    use default profiles and don't need to touch the Detail Page.
    Advanced users can define their own profiles for example for cross compiling.
}
unit BuildLazDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$IFDEF Windows}
  Windows,
  {$ENDIF}
  // LCL
  Forms, Controls, StdCtrls, ExtCtrls, Buttons, Dialogs,
  LCLPlatformDef, CheckLst, Menus, ComCtrls, LCLType,
  // LazUtils
  LazFileUtils, LazStringUtils, LazUTF8, LazLoggerBase, LazVersion,
  // LazControls
  DividerBevel,
  // BuildIntf
  PackageIntf,
  // IDEIntf
  IdeIntfStrConsts, IDEImagesIntf, IDEWindowIntf, IDEUtils,
  // IdeConfig
  EnvironmentOpts, LazConf, ParsedCompilerOpts, MiscOptions, IdeBuilder, IdeConfStrConsts,
  // IdePackager
  PackageSystem, PackageDefs,
  // IDE
  LazarusIDEStrConsts, MainBar, BuildProfileManager, GenericListEditor, GenericCheckList;

type

  { TConfigureBuildLazarusDlg }

  TConfigureBuildLazarusDlg = class(TForm)
    CleanAllRadioButton: TRadioButton;
    CleanAutoRadioButton: TRadioButton;
    CleanOnceCheckBox: TCheckBox;
    CommonsDividerBevel: TDividerBevel;
    ConfirmBuildCheckBox: TCheckBox;
    DefinesButton: TButton;
    DefinesLabel: TLabel;
    DefinesListBox: TCheckListBox;
    CancelButton: TBitBtn;
    CBLDBtnPanel: TPanel;
    BuildProfileComboBox: TComboBox;
    CompileButton: TBitBtn;
    CompileAdvancedButton: TBitBtn;
    InhTreeView: TTreeView;
    LCLWidgetTypeLabel: TLabel;
    LCLWidgetTypeComboBox: TComboBox;
    OptionsLabel: TLabel;
    OptionsMemo: TMemo;
    CleanUpGroupBox: TGroupBox;
    PageControl1: TPageControl;
    RestartAfterBuildCheckBox: TCheckBox;
    ShowOptsMenuItem: TMenuItem;
    DetailsPanel: TPanel;
    HelpButton: TBitBtn;
    BuildProfileLabel: TLabel;
    OptionsPopupMenu: TPopupMenu;
    Panel2: TPanel;
    SaveSettingsButton: TBitBtn;
    BuildProfileButton: TButton;
    BuildTabSheet: TTabSheet;
    InfoTabSheet: TTabSheet;
    TargetCPUComboBox: TComboBox;
    TargetCPULabel: TLabel;
    TargetDirectoryButton: TButton;
    TargetDirectoryComboBox: TComboBox;
    TargetDirectoryLabel: TLabel;
    TargetOSComboBox: TComboBox;
    TargetOSLabel: TLabel;
    UpdateRevisionIncCheckBox: TCheckBox;
    procedure BuildProfileButtonClick(Sender: TObject);
    procedure BuildProfileComboBoxSelect(Sender: TObject);
    procedure CleanRadioButtonClick(Sender: TObject);
    procedure CompileAdvancedButtonClick(Sender: TObject);
    procedure CompileButtonClick(Sender: TObject);
    procedure DefinesButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ShowOptsMenuItemClick(Sender: TObject);
    procedure SaveSettingsButtonClick(Sender: TObject);
    procedure TargetDirectoryButtonClick(Sender: TObject);
    procedure MRUComboBoxExit(Sender: TObject);
  private
    fBuilder: TLazarusBuilder;
    // Data is copied by caller before and after opening this dialog.
    fProfiles: TBuildLazarusProfiles;
    fUpdatingProfileCombo: Boolean;
    fImageIndexPackage: Integer;
    fImageIndexRequired: Integer;
    fImageIndexInherited: Integer;
    procedure SetupInfoPage;
    procedure UpdateInheritedTree;
    procedure PrepareClose;
  public
    constructor Create(TheOwner: TComponent); overload; reintroduce;
    destructor Destroy; override;
    procedure CopyProfileToUI(AProfile: TBuildLazarusProfile);
    procedure CopyUIToProfile(AProfile: TBuildLazarusProfile);
    procedure DisableCompilation;
    procedure UpdateProfileNamesUI;
  end;

  { TLazarusBuildManyDialog }

  TLazarusBuildManyDialog = class(TGenericCheckListForm)
    // nothing, just ApplyLayout() requires a specific class name
  end;

function ShowConfigBuildLazDlg(ABuilder: TLazarusBuilder;
  AProfiles: TBuildLazarusProfiles; ADisableCompilation: Boolean): TModalResult;


implementation

{$R *.lfm}

function ShowConfigBuildLazDlg(ABuilder: TLazarusBuilder;
  AProfiles: TBuildLazarusProfiles; ADisableCompilation: Boolean): TModalResult;
// mrOk=save
// mrYes=save and compile
// mrAll=save and compile all selected profiles
var
  ConfigBuildLazDlg: TConfigureBuildLazarusDlg;
begin
  Result := mrCancel;
  ConfigBuildLazDlg := TConfigureBuildLazarusDlg.Create(nil);
  try
    ConfigBuildLazDlg.fBuilder := ABuilder;
    ConfigBuildLazDlg.fProfiles.Assign(AProfiles); // Copy profiles to dialog.
    if ADisableCompilation then
      ConfigBuildLazDlg.DisableCompilation;
    Result := ConfigBuildLazDlg.ShowModal;
    if Result in [mrOk,mrYes,mrAll] then
      AProfiles.Assign(ConfigBuildLazDlg.fProfiles); // Copy profiles back from dialog.
  finally
    ConfigBuildLazDlg.Free;
  end;
end;

{ TConfigureBuildLazarusDlg }

constructor TConfigureBuildLazarusDlg.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fProfiles:=TBuildLazarusProfiles.Create;
  fUpdatingProfileCombo:=False;
end;

destructor TConfigureBuildLazarusDlg.Destroy;
begin
  FreeAndNil(fProfiles);
  inherited Destroy;
end;

procedure TConfigureBuildLazarusDlg.FormCreate(Sender: TObject);
var
  LCLInterface: TLCLPlatform;
begin
  IDEDialogLayoutList.ApplyLayout(Self,700,480);

  Caption := lisConfigureBuildLazarus;
  PageControl1.ActivePage:=BuildTabSheet;
  BuildTabSheet.Caption:=lisBuildCaption;

  // Show Build target names in combobox.
  LCLWidgetTypeLabel.Caption := lisLCLWidgetType;
  for LCLInterface:=Low(TLCLPlatform) to High(TLCLPlatform) do
    LCLWidgetTypeComboBox.Items.Add(LCLPlatformDisplayNames[LCLInterface]);

  BuildProfileLabel.Caption:=lisLazBuildProfile;
  BuildProfileButton.Hint := lisLazBuildManageProfiles2;
  BuildProfileComboBox.Hint := lisLazBuildNameOfTheActiveProfile;
  OptionsLabel.Caption := lisLazBuildOptions;
  TargetOSLabel.Caption := lisLazBuildTargetOS;
  TargetCPULabel.Caption := lisLazBuildTargetCPU;
  TargetDirectoryLabel.Caption := lisLazBuildTargetDirectory;

  DefinesListBox.Hint := lisLazBuildDefinesWithoutD;
  DefinesLabel.Caption := lisLazBuildDefines;
  DefinesButton.Caption := lisLazBuildEditDefines;
  DefinesButton.Hint := lisLazBuildEditListOfDefinesWhichCanBeUsedByAnyProfile;

  CleanUpGroupBox.Caption:=lisCleanUpMode;
  CleanAutoRadioButton.Caption:=lisAutomatically;
  CleanAllRadioButton.Caption:=lisCleanAll;
  CleanOnceCheckBox.Caption:=lisCleanOnlyOnce;
  CleanOnceCheckBox.Hint:=lisAfterCleaningUpSwitchToAutomaticClean;

  UpdateRevisionIncCheckBox.Caption := lisLazBuildUpdateRevInc;
  UpdateRevisionIncCheckBox.Hint := lisLazBuildUpdateRevisionInfoInAboutLazarusDialog;

  CommonsDividerBevel.Caption := lisLazBuildCommonSettings;
  RestartAfterBuildCheckBox.Caption := lisLazBuildRestartAfterBuild;
  RestartAfterBuildCheckBox.Hint := lisLazBuildRestartLazarusAutomatically;
  ConfirmBuildCheckBox.Caption := lisLazBuildConfirmBuild;
  ConfirmBuildCheckBox.Hint := lisLazBuildShowConfirmationDialogWhenBuilding;

  CompileButton.Caption := lisBuild;
  IDEImages.AssignImage(CompileButton, 'menu_build');
  CompileAdvancedButton.Caption := lisLazBuildBuildMany;
  IDEImages.AssignImage(CompileAdvancedButton, 'menu_build_all');
  SaveSettingsButton.Caption := lisSaveSettings;
  IDEImages.AssignImage(SaveSettingsButton, 'laz_save');
  CancelButton.Caption := lisCancel;
  HelpButton.Caption := lisMenuHelp;

  OptionsMemo.Hint := lisLazBuildOptionsPassedToCompiler;
  ShowOptsMenuItem.Caption := lisLazBuildShowOptionsAndDefinesForCommandLine;

  with TargetOSComboBox do
  begin
    with Items do begin
      Add(''); //('+rsiwpDefault+')');
      Add('Darwin');
      Add('FreeBSD');
      Add('Linux');
      Add('NetBSD');
      Add('OpenBSD');
      Add('DragonFly');
      Add('Solaris');
      Add('Win32');
      Add('Win64');
      Add('WinCE');
      Add('Go32v2');
      Add('OS2');
      Add('BeOS');
      Add('Haiku');
      Add('QNX');
      Add('NetWare');
      Add('wdosx');
      Add('emx');
      Add('Watcom');
      Add('NetwLibC');
      Add('Amiga');
      Add('AROS');
      Add('Atari');
      Add('PalmOS');
      Add('GBA');
      Add('NDS');
      Add('MacOS');
      Add('MorphOS');
      Add('Embedded');
      Add('Symbian');
      Add('MSDOS');
      Add('Wii');
      Add('iOS');
    end;
    ItemIndex:=0;
  end;

  with TargetCPUComboBox do begin
    with Items do begin
      Add(''); //('+rsiwpDefault+')');
      Add('aarch64');
      Add('arm');
      Add('i386');
      Add('loongarch64');
      Add('m68k');
      Add('powerpc');
      Add('powerpc64');
      Add('sparc');
      Add('x86_64');
    end;
    ItemIndex:=0;
  end;

  SetupInfoPage;
  BuildProfileComboBox.DropDownCount:=EnvironmentOptions.DropDownCount;
  LCLWidgetTypeComboBox.DropDownCount:=EnvironmentOptions.DropDownCount;
  TargetOSComboBox.DropDownCount:=EnvironmentOptions.DropDownCount;
  TargetCPUComboBox.DropDownCount:=EnvironmentOptions.DropDownCount;
  TargetDirectoryComboBox.DropDownCount:=EnvironmentOptions.DropDownCount;
end;

procedure TConfigureBuildLazarusDlg.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // dialog
  if (Key = VK_ESCAPE) and (Shift = []) then
  begin
    Close;
    Key := 0;
  end
  else if (Key = VK_RETURN) and (Shift = [ssCtrl]) then
  begin
    if CompileButton.Enabled then
      CompileButtonClick(Sender);
    Key := 0;
  end
  else if (Key = VK_S) and (Shift = [ssCtrl]) then
  begin
    if SaveSettingsButton.Enabled then
      SaveSettingsButtonClick(Sender);
    Key := 0;
  end

  // tabs
  else if (Key = VK_TAB) and (Shift = [ssCtrl]) then
  begin
    PageControl1.SelectNextPage(true);
    Key := 0;
  end
  else if (Key = VK_TAB) and (Shift = [ssCtrl, ssShift]) then
  begin
    PageControl1.SelectNextPage(false);
    Key := 0;
  end
end;

procedure TConfigureBuildLazarusDlg.FormResize(Sender: TObject);
begin
  LCLWidgetTypeComboBox.Width:=(OptionsMemo.Width - 12) div 3;
  TargetOSComboBox.Width:=LCLWidgetTypeComboBox.Width;
  DefinesListBox.Width:=(OptionsMemo.Width - 6) div 2;
end;

procedure TConfigureBuildLazarusDlg.FormShow(Sender: TObject);
begin
  UpdateProfileNamesUI;
end;

procedure TConfigureBuildLazarusDlg.ShowOptsMenuItemClick(Sender: TObject);
begin
  CopyUIToProfile(fProfiles.Current);
  ShowMessage(fProfiles.Current.ExtraOptions);
end;

procedure TConfigureBuildLazarusDlg.TargetDirectoryButtonClick(Sender: TObject);
var
  DirDialog: TSelectDirectoryDialog;
  lExpandedName: string;
  lDirName, lDirNameF: string;
begin
  DirDialog:=TSelectDirectoryDialog.Create(nil);
  try
    DirDialog.Options:=DirDialog.Options+[ofPathMustExist];
    DirDialog.Title:=lisLazBuildABOChooseOutputDir+'(lazarus'+
                      GetExecutableExt(fProfiles.Current.FPCTargetOS)+')';

    { Setup directory path }
    lDirName:=EnvironmentOptions.GetParsedValue(eopTestBuildDirectory, TargetDirectoryComboBox.Text);
    lExpandedName:=ChompPathDelim(CleanAndExpandDirectory(lDirName));
    lDirName:=GetValidDirectoryAndFilename(lExpandedName, lDirNameF);

    DirDialog.InitialDir:=lDirName;
    DirDialog.FileName:=lDirNameF;

    if DirDialog.Execute then begin
      lDirName:=ChompPathDelim(CleanAndExpandDirectory(DirDialog.Filename));
      if CompareFilenames(lDirName, lExpandedName)<>0 then
        SetComboBoxText(TargetDirectoryComboBox,lDirName,cstFilename,MaxComboBoxCount);
    end;
  finally
    DirDialog.Free;
  end;
end;

procedure TConfigureBuildLazarusDlg.MRUComboBoxExit(Sender: TObject);
var
  cb: TComboBox;
begin
  cb := Sender as TComboBox;
  if cb.ItemIndex < 0 then
    SetComboBoxText(cb, cb.Text, cstFilename)
  else begin
    cb.Items.Move(cb.ItemIndex, 0);
    cb.ItemIndex := 0;
  end;
end;

procedure TConfigureBuildLazarusDlg.CopyProfileToUI(AProfile: TBuildLazarusProfile);
var
  i: Integer;
begin
  CleanAutoRadioButton.OnClick:=Nil;
  CleanAllRadioButton.OnClick:=Nil;
  try
    LCLWidgetTypeComboBox.ItemIndex   :=ord(AProfile.TargetPlatform);
    UpdateRevisionIncCheckBox.Checked :=AProfile.UpdateRevisionInc;
    TargetOSComboBox.Text             :=AProfile.TargetOS;
    with AProfile do begin
      if TargetDirHistory.Count > 0 then
        TargetDirectoryComboBox.Items.Assign(TargetDirHistory);
      SetComboBoxText(TargetDirectoryComboBox,TargetDirectory,cstFilename,MaxComboBoxCount);
    end;
    TargetCPUComboBox.Text            :=AProfile.TargetCPU;
    case AProfile.IdeBuildMode of
    bmBuild: CleanAutoRadioButton.Checked:=true;
    bmCleanBuild, bmCleanAllBuild: CleanAllRadioButton.Checked:=true;
    end;
    CleanOnceCheckBox.Checked:=AProfile.CleanOnce;
    OptionsMemo.Lines.Assign(AProfile.OptionsLines);
    for i:=0 to DefinesListBox.Items.Count-1 do
      DefinesListBox.Checked[i]:=AProfile.Defines.IndexOf(DefinesListBox.Items[i]) > -1;
  finally
    CleanAutoRadioButton.OnClick:=@CleanRadioButtonClick;
    CleanAllRadioButton.OnClick:=@CleanRadioButtonClick;
  end;
end;

procedure TConfigureBuildLazarusDlg.CopyUIToProfile(AProfile: TBuildLazarusProfile);
var
  i: Integer;
begin
  AProfile.TargetPlatform    :=TLCLPlatform(LCLWidgetTypeComboBox.ItemIndex);
  AProfile.UpdateRevisionInc :=UpdateRevisionIncCheckBox.Checked;
  AProfile.TargetOS          :=TargetOSComboBox.Text;
  AProfile.TargetDirectory   :=TargetDirectoryComboBox.Text;
  AProfile.TargetDirHistory.Assign(TargetDirectoryComboBox.Items);
  AProfile.TargetCPU         :=TargetCPUComboBox.Text;
  if CleanAllRadioButton.Checked then
    AProfile.IdeBuildMode := bmCleanAllBuild
  else
    AProfile.IdeBuildMode := bmBuild;
  AProfile.CleanOnce:=CleanOnceCheckBox.Checked;
  AProfile.OptionsLines.Text := OptionsMemo.Lines.Text; // not use Assign, Lines may contain word wraps!
  AProfile.Defines.Clear;
  for i:=0 to DefinesListBox.Items.Count-1 do
    if DefinesListBox.Checked[i] then
      AProfile.Defines.Add(DefinesListBox.Items[i]);
end;

procedure TConfigureBuildLazarusDlg.DisableCompilation;
begin
  CompileButton.Enabled:=False;
  CompileAdvancedButton.Enabled:=False;
end;

procedure TConfigureBuildLazarusDlg.UpdateProfileNamesUI;
var
  i: Integer;
begin
  // List of defines to checklistbox.
  DefinesListBox.Items.Clear;
  for i:=0 to fProfiles.AllDefines.Count-1 do
    DefinesListBox.Items.Add(fProfiles.AllDefines[i]);
  // Update the Profiles ComboBox.
  fUpdatingProfileCombo:=True;
  BuildProfileComboBox.Items.BeginUpdate;
  BuildProfileComboBox.Items.Clear;
  for i:=0 to fProfiles.Count-1 do
    BuildProfileComboBox.Items.Add(fProfiles[i].TranslatedName);
  BuildProfileCombobox.ItemIndex:=fProfiles.CurrentIndex;
  CopyProfileToUI(fProfiles.Current); // Copy current selection to UI.
  BuildProfileComboBox.Items.EndUpdate;
  fUpdatingProfileCombo:=False;
  RestartAfterBuildCheckBox.Checked:=fProfiles.RestartAfterBuild;
  ConfirmBuildCheckBox.Checked     :=fProfiles.ConfirmBuild;
end;

procedure TConfigureBuildLazarusDlg.SetupInfoPage;
begin
  InfoTabSheet.Caption:=lisInformation;

  fImageIndexPackage := IDEImages.LoadImage('item_package');
  fImageIndexRequired := IDEImages.LoadImage('pkg_required');
  fImageIndexInherited := IDEImages.LoadImage('pkg_inherited');
  InhTreeView.Images := IDEImages.Images_16;

  UpdateInheritedTree;
end;

procedure TConfigureBuildLazarusDlg.UpdateInheritedTree;
var
  AncestorNode: TTreeNode;

  procedure AddChildNode(const NewNodeName, Value: string);
  var
    VisibleValue: string;
    ChildNode: TTreeNode;
  begin
    VisibleValue := UTF8Trim(Value);
    if VisibleValue = '' then
      exit;
    ChildNode := InhTreeView.Items.AddChild(AncestorNode,
      NewNodeName + ' = "' + VisibleValue + '"');
    ChildNode.ImageIndex := fImageIndexRequired;
    ChildNode.SelectedIndex := ChildNode.ImageIndex;
  end;

var
  PkgList: TFPList;
  i: Integer;
  Pkg: TLazPackage;
  AncestorOptions: TPkgAdditionalCompilerOptions;
  LazDir: String;
begin
  PkgList:=nil;
  InhTreeView.BeginUpdate;
  LazDir:=EnvironmentOptions.GetParsedLazarusDirectory;
  try
    PackageGraph.GetAllRequiredPackages(nil,
      PackageGraph.FirstInstallDependency,PkgList,[pirSkipDesignTimeOnly]);

    // add detail nodes
    if PkgList<>nil then
      for i := 0 to PkgList.Count - 1 do
      begin
        Pkg:=TLazPackage(PkgList[i]);
        AncestorOptions := Pkg.UsageOptions;
        AncestorNode := InhTreeView.Items.Add(nil, '');
        AncestorNode.Text := AncestorOptions.GetOwnerName;
        AncestorNode.ImageIndex := fImageIndexPackage;
        AncestorNode.SelectedIndex := AncestorNode.ImageIndex;
        with AncestorOptions.ParsedOpts do
        begin
          AddChildNode(lisunitPath,
            CreateRelativeSearchPath(GetParsedValue(pcosUnitPath),LazDir));
          AddChildNode(lisincludePath,
            CreateRelativeSearchPath(GetParsedValue(pcosIncludePath),LazDir));
          AddChildNode(lisobjectPath,
            CreateRelativeSearchPath(GetParsedValue(pcosObjectPath),LazDir));
          AddChildNode(lislibraryPath,
            CreateRelativeSearchPath(GetParsedValue(pcosLibraryPath),LazDir));
          AddChildNode(lislinkerOptions, GetParsedValue(pcosLinkerOptions));
          AddChildNode(liscustomOptions, GetParsedValue(pcosCustomOptions));
        end;
        AncestorNode.Expanded := True;
      end;
  finally
    InhTreeView.EndUpdate;
    PkgList.Free;
  end;
end;

procedure TConfigureBuildLazarusDlg.PrepareClose;
begin
  CopyUIToProfile(fProfiles.Current);
  fProfiles.RestartAfterBuild :=RestartAfterBuildCheckBox.Checked;
  fProfiles.ConfirmBuild      :=ConfirmBuildCheckBox.Checked;
  MainIDEBar.itmToolBuildLazarus.Caption:=
    Format(lisMenuBuildLazarusProf, [fProfiles.Current.Name]);
end;

procedure TConfigureBuildLazarusDlg.CompileAdvancedButtonClick(Sender: TObject);
// mrOk=change selected profiles. Selected profiles will be saved or discarded
// depending on the calling dialog
// mrYes=save and compile
// mrCancel=do nothing
var
  EditForm: TLazarusBuildManyDialog;
  i, ind: Integer;
begin
  PrepareClose;
  // Add a button for building all.
  EditForm:=TLazarusBuildManyDialog.CreateWithActionButton(lisBuild, 'menu_build');
  try
    EditForm.Caption:=lisLazBuildSelectProfilesToBuild;
    // Copy profile names to checkboxlist and check the previously selected ones.
    for i:=0 to fProfiles.Count-1 do begin
      ind:=EditForm.CheckListBox1.Items.Add(fProfiles[i].Name);
      if fProfiles.Selected.IndexOf(fProfiles[i].Name)>-1 then
        EditForm.CheckListBox1.Checked[ind]:=True;
    end;
    IDEDialogLayoutList.ApplyLayout(EditForm);
    // Show the form.
    EditForm.ShowModal;
    if EditForm.ModalResult in [mrOK, mrYes] then begin
      // Copy checked profile names to Selected.
      fProfiles.Selected.Clear;
      for i:=0 to fProfiles.Count-1 do begin      // fProfiles and CheckListBox1
        if EditForm.CheckListBox1.Checked[i] then // indexes match now.
          fProfiles.Selected.Add(fProfiles[i].Name);
      end;
    end;
    if EditForm.ModalResult=mrYes then
      ModalResult:=mrAll;
  finally
    IDEDialogLayoutList.SaveLayout(EditForm);
    EditForm.Free;
  end;
end;

procedure TConfigureBuildLazarusDlg.CompileButtonClick(Sender: TObject);
begin
  PrepareClose;
  ModalResult:=mrYes;
end;

procedure TConfigureBuildLazarusDlg.SaveSettingsButtonClick(Sender: TObject);
begin
  PrepareClose;
  ModalResult:=mrOk;
end;

procedure TConfigureBuildLazarusDlg.DefinesButtonClick(Sender: TObject);
var
  EditForm: TGenericListEditForm;
  i: Integer;
begin
  EditForm:=TGenericListEditForm.Create(Nil);
  try
    EditForm.Caption:=lisLazBuildEditDefines;
    EditForm.Memo1.Lines.Assign(fProfiles.AllDefines);
    if EditForm.ShowModal=mrOK then begin
      CopyUIToProfile(fProfiles.Current); // Make sure changed fields don't get lost.
      fProfiles.AllDefines.Assign(EditForm.Memo1.Lines);
      DefinesListBox.Items.Clear;
      for i:=0 to fProfiles.AllDefines.Count-1 do
        DefinesListBox.Items.Add(fProfiles.AllDefines[i]);
      for i:=0 to DefinesListBox.Items.Count-1 do // Check the right boxes again.
        DefinesListBox.Checked[i]:=fProfiles.Current.Defines.IndexOf(DefinesListBox.Items[i]) > -1;
    end;
  finally
    EditForm.Free;
  end;
end;

procedure TConfigureBuildLazarusDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TConfigureBuildLazarusDlg.BuildProfileButtonClick(Sender: TObject);
var
  Frm: TBuildProfileManagerForm;
begin
  Frm:=TBuildProfileManagerForm.Create(nil);
  try
    CopyUIToProfile(fProfiles.Current);    // Make sure changed fields get included.
    Frm.Prepare(fProfiles);                // Copy profiles to dialog.
    if Frm.ShowModal = mrOk then begin
      fProfiles.Assign(Frm.ProfsToManage); // Copy profiles back from dialog.
      UpdateProfileNamesUI;
    end;
  finally
    Frm.Free;
  end;
end;

procedure TConfigureBuildLazarusDlg.BuildProfileComboBoxSelect(Sender: TObject);
begin
  // QT binding calls this also when items are added to list. It shouldn't.
  if (fProfiles.Count=0) or fUpdatingProfileCombo then Exit;
  if (Sender as TComboBox).ItemIndex=-1 then Exit;
  CopyUIToProfile(fProfiles.Current);      // Save old selection from UI.
  fProfiles.CurrentIndex:=(Sender as TComboBox).ItemIndex;
  CopyProfileToUI(fProfiles.Current);      // Copy new selection to UI.
end;

procedure TConfigureBuildLazarusDlg.CleanRadioButtonClick(Sender: TObject);
begin
  //
end;

end.

