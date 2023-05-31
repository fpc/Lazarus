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
}
unit Desktop_Options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, StdCtrls, Dialogs, LCLProc, ExtCtrls, Spin,
  // LazUtils
  FileUtil, LazUTF8,
  // LazControls
  DividerBevel,
  // IdeIntf
  IdeIntfStrConsts, IDEOptionsIntf, IDEOptEditorIntf, IDEWindowIntf,
  IDEUtils, IDEDialogs, InputHistory,
  // IdeConfig
  EnvironmentOpts,
  // IDE
  LazarusIDEStrConsts, IDETranslations, EnvGuiOptions;

type

  { TDesktopOptionsFrame }

  TDesktopOptionsFrame = class(TAbstractIDEOptionsEditor)
    AskSavingOnlySessionCheckBox: TCheckBox;
    AutoSaveEditorFilesCheckBox: TCheckBox;
    AutoSaveIntervalInSecsComboBox: TComboBox;
    AutoSaveIntervalInSecsLabel: TLabel;
    AutoSaveProjectCheckBox: TCheckBox;
    lblDropDownCount: TLabel;
    lblComboBoxes: TDividerBevel;
    lblCheckAndAutoSave: TDividerBevel;
    lblImportExport: TDividerBevel;
    lblGlyphs: TDividerBevel;
    lblHints: TDividerBevel;
    lblLanguage: TDividerBevel;
    lblMouseAction: TDividerBevel;
    PreferDoubleClickCheckBox: TCheckBox;
    CheckDiskChangesWithLoadingCheckBox: TCheckBox;
    lblButtons: TLabel;
    lblCenter: TLabel;
    lblMenus: TLabel;
    LanguageComboBox: TComboBox;
    ImportDesktopButton: TButton;
    PanelGlyphsButtonsOptions: TPanel;
    PanelGlyphsMenusOptions: TPanel;
    rbMenuGlyphShowAlways: TRadioButton;
    rbMenuGlyphShowNever: TRadioButton;
    rbMenuGlyphShowSystem: TRadioButton;
    rbBtnGlyphShowAlways: TRadioButton;
    rbBtnGlyphShowNever: TRadioButton;
    rbBtnGlyphShowSystem: TRadioButton;
    ExportDesktopButton: TButton;
    ShowHintsForComponentPaletteCheckBox: TCheckBox;
    ShowHintsForMainSpeedButtonsCheckBox: TCheckBox;
    spDropDownCount: TSpinEdit;
    procedure ExportDesktopButtonClick(Sender: TObject);
    procedure ImportDesktopButtonClick(Sender: TObject);
  private
    function LangIDToCaption(const LangID: string): string;
    function CaptionToLangID(const ACaption: string): string;
    procedure DoLoadSettings(AOptions: TAbstractIDEOptions);
    procedure DoSaveSettings(AOptions: TAbstractIDEOptions);
  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TDesktopOptionsFrame }

function TDesktopOptionsFrame.GetTitle: String;
begin
  Result := lisGeneral;
end;

procedure TDesktopOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  i: Integer;
  LangID: String;
  sl: TStringListUTF8Fast;
begin
  // language
  lblLanguage.Caption := dlgEnvLanguage;
  LanguageComboBox.Hint := dlgEnvLanguageHint;

  // languages: first the automatic, then sorted the rest
  sl:=TStringListUTF8Fast.Create;
  for i:=0 to LazarusTranslations.Count-1 do
  begin
    LangID:=LazarusTranslations[i].ID;
    if LangID<>'' then
      sl.Add(LangIDToCaption(LangID));
  end;
  sl.Sort;
  sl.Insert(0,GetLazarusLanguageLocalizedName(''));
  LanguageComboBox.Items.Assign(sl);
  sl.Free;

  // mouse action
  lblMouseAction.Caption := dlgMouseAction;
  PreferDoubleClickCheckBox.Caption := dlgPreferDoubleClickOverSingleClick;
  PreferDoubleClickCheckBox.Hint := dlgCurrentlyRespectedByMessagesWindow;

  // hints
  lblHints.Caption := dlgDesktopHints;
  ShowHintsForMainSpeedButtonsCheckBox.Caption := dlgSpBHints;
  ShowHintsForComponentPaletteCheckBox.Caption := dlgPalHints;

  // button glyphs
  lblGlyphs.Caption := lisShowGlyphsFor;
  lblButtons.Caption := dlgDesktopButtons;
  lblMenus.Caption := dlgDesktopMenus;
  rbBtnGlyphShowAlways.Caption := lisAlways;
  rbBtnGlyphShowNever.Caption := lisNever;
  rbBtnGlyphShowSystem.Caption := lisDefault;
  rbMenuGlyphShowAlways.Caption := lisAlways;
  rbMenuGlyphShowNever.Caption := lisNever;
  rbMenuGlyphShowSystem.Caption := lisDefault;

  // check and auto save files
  lblCheckAndAutoSave.Caption := dlgCheckAndAutoSaveFiles;
  CheckDiskChangesWithLoadingCheckBox.Caption := lisCheckForDiskFileChangesViaContent;
  CheckDiskChangesWithLoadingCheckBox.Hint := lisSlowerButMoreAccurate;
  AskSavingOnlySessionCheckBox.Caption := lisAskBeforeSavingProjectSSession;
  AskSavingOnlySessionCheckBox.Hint := lisIfOnlySessionInfoChangedThenAsk;
  // The following 3 are now hidden.
  AutoSaveEditorFilesCheckBox.Caption := dlgEdFiles;
  AutoSaveProjectCheckBox.Caption := dlgProject;
  AutoSaveIntervalInSecsLabel.Caption := dlgIntvInSec;

  // desktop files
  lblImportExport.Caption := lisExportImport;
  ExportDesktopButton.Caption := lisExport;
  ImportDesktopButton.Caption := lisImport;
  ExportDesktopButton.Hint := lisExportEnvironmentOptions;
  ImportDesktopButton.Hint := lisImportEnvironmentOptions;

  // comboboxes
  lblComboBoxes.Caption := lisComboBoxes;
  lblDropDownCount.Caption := lisDropDownCount;
  spDropDownCount.Hint := lisDropDownCountHint;
end;

procedure TDesktopOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  EnvOpt: TEnvironmentOptions;
  EnvGui: TIDESubOptions;
begin
  EnvOpt := AOptions as TEnvironmentOptions;
  EnvGui := EnvOpt.GetSubConfigObj(TEnvGuiOptions);
  //Assert(Assigned(EnvGui), 'TDesktopOptionsFrame.ReadSettings: EnvGui=Nil');
  with EnvGui as TEnvGuiOptions do
  begin
    // language
    LanguageComboBox.Text:=LangIDToCaption(EnvOpt.LanguageID);
    //debugln('TEnvironmentOptionsDialog.ReadSettings LanguageComboBox.ItemIndex=',dbgs(LanguageComboBox.ItemIndex),' LanguageID="',LanguageID,'" LanguageComboBox.Text="',LanguageComboBox.Text,'"');

    // mouse action
    PreferDoubleClickCheckBox.Checked := MsgViewDblClickJumps;

    // hints
    ShowHintsForMainSpeedButtonsCheckBox.Checked:=ShowHintsForMainSpeedButtons;
    ShowHintsForComponentPaletteCheckBox.Checked:=ShowHintsForComponentPalette;

    // glyphs
    case ShowButtonGlyphs of
      sbgAlways: rbBtnGlyphShowAlways.Checked := True;
      sbgNever: rbBtnGlyphShowNever.Checked := True;
      sbgSystem: rbBtnGlyphShowSystem.Checked := True;
    end;
    case ShowMenuGlyphs of
      sbgAlways: rbMenuGlyphShowAlways.Checked := True;
      sbgNever: rbMenuGlyphShowNever.Checked := True;
      sbgSystem: rbMenuGlyphShowSystem.Checked := True;
    end;

    // comboboxes
    spDropDownCount.Value := EnvOpt.DropDownCount;

    // check and auto save files
    CheckDiskChangesWithLoadingCheckBox.Checked:=EnvOpt.CheckDiskChangesWithLoading;
    AskSavingOnlySessionCheckBox.Checked:=EnvOpt.AskSaveSessionOnly;
    AutoSaveEditorFilesCheckBox.Checked:=EnvOpt.AutoSaveEditorFiles;
    AutoSaveProjectCheckBox.Checked:=EnvOpt.AutoSaveProject;
    SetComboBoxText(AutoSaveIntervalInSecsComboBox
       ,IntToStr(EnvOpt.AutoSaveIntervalInSecs),cstCaseInsensitive);
  end;
end;

procedure TDesktopOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  EnvOpt: TEnvironmentOptions;
  EnvGui: TIDESubOptions;
begin
  EnvOpt := AOptions as TEnvironmentOptions;
  EnvGui := EnvOpt.GetSubConfigObj(TEnvGuiOptions);
  //Assert(Assigned(EnvGui), 'TDesktopOptionsFrame.ReadSettings: EnvGui=Nil');
  with EnvGui as TEnvGuiOptions do
  begin
    // language
    EnvOpt.LanguageID:=CaptionToLangID(LanguageComboBox.Text);
    //debugln('TEnvironmentOptionsDialog.WriteSettings A LanguageID="',LanguageID,'" LanguageComboBox.ItemIndex=',dbgs(LanguageComboBox.ItemIndex),' LanguageComboBox.Text=',LanguageComboBox.Text);

    // mouse action
    MsgViewDblClickJumps := PreferDoubleClickCheckBox.Checked;

    // hints
    ShowHintsForMainSpeedButtons:=ShowHintsForMainSpeedButtonsCheckBox.Checked;
    ShowHintsForComponentPalette:=ShowHintsForComponentPaletteCheckBox.Checked;

    // glyphs
    if rbBtnGlyphShowAlways.Checked then
      ShowButtonGlyphs := sbgAlways
    else
    if rbBtnGlyphShowNever.Checked then
      ShowButtonGlyphs := sbgNever
    else
      ShowButtonGlyphs := sbgSystem;
    if rbMenuGlyphShowAlways.Checked then
      ShowMenuGlyphs := sbgAlways
    else
    if rbMenuGlyphShowNever.Checked then
      ShowMenuGlyphs := sbgNever
    else
      ShowMenuGlyphs := sbgSystem;

    // check and auto save files
    EnvOpt.CheckDiskChangesWithLoading:=CheckDiskChangesWithLoadingCheckBox.Checked;
    EnvOpt.AskSaveSessionOnly:=AskSavingOnlySessionCheckBox.Checked;
    EnvOpt.AutoSaveEditorFiles:=AutoSaveEditorFilesCheckBox.Checked;
    EnvOpt.AutoSaveProject:=AutoSaveProjectCheckBox.Checked;
    EnvOpt.AutoSaveIntervalInSecs:=StrToIntDef(
      AutoSaveIntervalInSecsComboBox.Text,EnvOpt.AutoSaveIntervalInSecs);

    // comboboxes
    EnvOpt.DropDownCount := spDropDownCount.Value;
  end;
end;

procedure TDesktopOptionsFrame.ExportDesktopButtonClick(Sender: TObject);
var
  AnEnvironmentOptions: TEnvironmentOptions;
  SaveDialog: TSaveDialog;
  AFilename: String;
begin
  //debugln('TDesktopOptionsFrame.ExportDesktopButtonClick A');
  SaveDialog := IDESaveDialogClass.Create(nil);
  try
    try
      InputHistories.ApplyFileDialogSettings(SaveDialog);
      SaveDialog.Filter:=dlgFilterLazarusDesktopSettings+' (*.lds)|*.lds'
           //+'|'+dlgFilterXML+' (*.xml)|*.xml'
           +'|'+dlgFilterAll+' ('+GetAllFilesMask+')|' + GetAllFilesMask;
      if SaveDialog.Execute then
      begin
        AFilename:=SaveDialog.Filename;
        if ExtractFileExt(AFilename)='' then
          AFilename:=AFilename+'.lds';
        AnEnvironmentOptions := TEnvironmentOptions.Create(Application.ExeName);
        try
          AnEnvironmentOptions.Filename := AFilename;
          DoSaveSettings(AnEnvironmentOptions);
          AnEnvironmentOptions.Save(true);
          ShowMessageFmt(lisSuccessfullyExported, [SaveDialog.Filename]);
        finally
          AnEnvironmentOptions.Free;
        end;
      end;
      InputHistories.StoreFileDialogSettings(SaveDialog);
    except
      on E: Exception do
      begin
        DebugLn('ERROR: [TDesktopOptionsFrame.ExportDesktopButtonClick] ', E.Message);
      end;
    end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TDesktopOptionsFrame.ImportDesktopButtonClick(Sender: TObject);
var
  AnEnvironmentOptions: TEnvironmentOptions;
  OpenDialog: TOpenDialog;
begin
  //debugln('TDesktopOptionsFrame.ImportDesktopButtonClick A');
  OpenDialog := IDEOpenDialogClass.Create(nil);
  try
    try
      InputHistories.ApplyFileDialogSettings(OpenDialog);
      OpenDialog.Filter:=dlgFilterLazarusDesktopSettings+' (*.lds)|*.lds'
           //+'|'+dlgFilterXML+' (*.xml)|*.xml'
           +'|'+dlgFilterAll+' ('+GetAllFilesMask+')|' + GetAllFilesMask;
      if OpenDialog.Execute then
      begin
        AnEnvironmentOptions := TEnvironmentOptions.Create(Application.ExeName);
        try
          AnEnvironmentOptions.Filename := OpenDialog.Filename;
          AnEnvironmentOptions.Load(true);
          DoLoadSettings(AnEnvironmentOptions);
          IDEWindowCreators.RestoreSimpleLayout;
          ShowMessageFmt(lisSuccessfullyImported, [OpenDialog.Filename]);
        finally
          AnEnvironmentOptions.Free;
        end;
      end;
      InputHistories.StoreFileDialogSettings(OpenDialog);
    except
      on E: Exception do
      begin
        // ToDo
        DebugLn('ERROR: [TDesktopOptionsFrame.ImportDesktopButtonClick] ', E.Message);
      end;
    end;
  finally
    OpenDialog.Free;
  end;
end;

function TDesktopOptionsFrame.LangIDToCaption(const LangID: string): string;
begin
  if LangID <> '' then
    Result := GetLazarusLanguageLocalizedName(LangID)+' ['+LangID+']'
  else
    //No [] if automatic
    Result := GetLazarusLanguageLocalizedName(LangID);
end;

function TDesktopOptionsFrame.CaptionToLangID(const ACaption: string): string;
var
  i: Integer;
begin
  for i := 0 to LazarusTranslations.Count-1 do 
  begin
    Result := LazarusTranslations[i].ID;
    if ACaption = LangIDToCaption(Result) then 
      Exit;
  end;
  Result := '';
end;

procedure TDesktopOptionsFrame.DoLoadSettings(AOptions: TAbstractIDEOptions);
begin
  if Assigned(OnLoadIDEOptions) then
    OnLoadIDEOptions(Self, AOptions);
end;

procedure TDesktopOptionsFrame.DoSaveSettings(AOptions: TAbstractIDEOptions);
begin
  if Assigned(OnSaveIDEOptions) then
    OnSaveIDEOptions(Self, AOptions);
end;

class function TDesktopOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEnvironment, TDesktopOptionsFrame, EnvOptionsDesktop);
end.

