unit fRegistrySettings;

{ settings form for JCF notepad registy options }

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is fRegistrySettings, released May 2003.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele. 

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$mode delphi}

interface

uses
  SysUtils, Classes, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, Spin, ButtonPanel;

type

  { TfmRegistrySettings }

  TfmRegistrySettings = class(TForm)
    ButtonPanel: TButtonPanel;
    dlgOpen:         TOpenDialog;
    pgPages:         TPageControl;
    dlgSelectDirectory: TSelectDirectoryDialog;
    eMRUMaxItems:    TSpinEdit;
    tsGeneral:       TTabSheet;
    tsLogFile:       TTabSheet;
    eSettingsFile:   TEdit;
    sbFile:          TSpeedButton;
    Label1:          TLabel;
    btnClearMRU:     TButton;
    Label2:          TLabel;
    rgShowParseTree: TRadioGroup;
    sbSpecifedDir:   TSpeedButton;
    Label3:          TLabel;
    lblBackupFileExt: TLabel;
    lblOutputFileExt: TLabel;
    rgLogLevel:      TRadioGroup;
    rgLogDir:        TRadioGroup;
    btnViewLog:      TButton;
    cbViewLog:       TCheckBox;
    edtBackupExt:    TEdit;
    edtOutputExt:    TEdit;
    cbLogTime:       TCheckBox;
    tsExclusions:    TTabSheet;
    lblFilesCaption: TLabel;
    lblDirsCaption:  TLabel;
    mFiles:          TMemo;
    mDirs:           TMemo;
    rgWriteSettingsFile: TRadioGroup;
    cbCheckMultibyteChars: TCheckBox;
    tsIde:           TTabSheet;
    cbEditorIntegration: TCheckBox;
    cbFormatBeforeSave: TCheckBox;
    cbFormatAfterLoad: TCheckBox;
    procedure btnClearMRUClick(Sender: TObject);
    {$push}{$warn 5024 off}
    procedure eSettingsFileKeyUp(Sender: TObject; var Key: word;
      Shift: TShiftState);
    {$pop}
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure sbFileClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure sbSpecifedDirClick(Sender: TObject);
    procedure btnViewLogClick(Sender: TObject);
    procedure tsExclusionsResize(Sender: TObject);
    procedure cbFormatAfterLoadClick(Sender: TObject);
    procedure cbFormatBeforeSaveClick(Sender: TObject);
  private
    fsSpecifiedDirectory: string;
    procedure ShowDirs;
    procedure ReadSettings;
    procedure WriteSettings;
  public
    procedure Execute;
  end;

implementation

uses
  LCLType, ConvertTypes, JcfRegistrySettings, JcfSettings, JcfUIConsts;

  {$R *.lfm}

procedure TfmRegistrySettings.ReadSettings;
var
  lcSet: TJCFRegistrySettings;
begin
  lcSet := GetRegSettings;
  Assert(lcSet <> nil);

  { general }
  eSettingsFile.TextHint := lisHintApplicationDefault + lcSet.FormatConfigFileName;
  if lcSet.FormatConfigNameSpecified then
    eSettingsFile.Text := lcSet.FormatConfigFileName
  else
    eSettingsFile.Text := '';

  rgWriteSettingsFile.ItemIndex := Ord(lcSet.FormatFileWriteOption);

  eMRUMaxItems.Value := lcSet.MRUMaxItems;
  rgShowParseTree.ItemIndex := Ord(lcSet.ShowParseTreeOption);

  cbCheckMultibyteChars.Checked := lcSet.CheckMultiByteChars;

  { mru }
  btnClearMRU.Enabled := GetRegSettings.CanClearMRU;

  { log }
  rgLogLevel.ItemIndex := Ord(lcSet.LogLevel);
  rgLogDir.ItemIndex := Ord(lcSet.LogPlace);
  fsSpecifiedDirectory := lcSet.SpecifiedDirectory;
  cbViewLog.Checked  := lcSet.ViewLogAfterRun;
  cbLogTime.Checked  := lcSet.LogTime;

  edtBackupExt.Text := lcSet.BackupExtension;
  edtOutputExt.Text := lcSet.OutputExtension;

  { exclusions}
  mFiles.Lines.Assign(lcSet.ExclusionsFiles);
  mDirs.Lines.Assign(lcSet.ExclusionsDirs);

  ShowDirs;

  { IDE }
  cbEditorIntegration.Checked := lcSet.EditorIntegration;
  cbFormatAfterLoad.Checked  := lcSet.FormatAfterLoad;
  cbFormatBeforeSave.Checked := lcSet.FormatBeforeSave;

end;

procedure TfmRegistrySettings.WriteSettings;
var
  lcSet: TJCFRegistrySettings;
  lsSettingsFile:string;
begin
  lcSet := GetRegSettings;
  Assert(lcSet <> nil);

  lsSettingsFile:=TrimRight(eSettingsFile.Text);
  if lcSet.FormatConfigFileName <> lsSettingsFile then
  begin
    lcSet.FormatConfigFileName := lsSettingsFile;
    if lsSettingsFile <> '' then
      FormattingSettings.ReadFromFile(lsSettingsFile, True);
  end;

  lcSet.FormatFileWriteOption := TFormatFileWriteOption(rgWriteSettingsFile.ItemIndex);

  lcSet.MRUMaxItems := eMRUMaxItems.Value;
  lcSet.ShowParseTreeOption := TShowParseTreeOption(rgShowParseTree.ItemIndex);

  lcSet.CheckMultiByteChars := cbCheckMultibyteChars.Checked;

  { log files }
  lcSet.LogLevel := TLogLevel(rgLogLevel.ItemIndex);
  lcSet.LogPlace := TLogPlace(rgLogDir.ItemIndex);
  lcSet.SpecifiedDirectory := fsSpecifiedDirectory;
  lcSet.ViewLogAfterRun := cbViewLog.Checked;
  lcSet.LogTime  := cbLogTime.Checked;

  lcSet.BackupExtension := edtBackupExt.Text;
  lcSet.OutputExtension := edtOutputExt.Text;

  { exclusions }
  lcSet.ExclusionsFiles.Assign(mFiles.Lines);
  lcSet.ExclusionsDirs.Assign(mDirs.Lines);

  { IDE }
  lcSet.EditorIntegration := cbEditorIntegration.Checked;
  lcSet.FormatAfterLoad  := cbFormatAfterLoad.Checked;
  lcSet.FormatBeforeSave := cbFormatBeforeSave.Checked;

  lcSet.WriteAll;
end;

procedure TfmRegistrySettings.Execute;
begin
  ReadSettings;
  FormResize(nil);

  pgPages.ActivePage := tsGeneral;

  ShowModal;
end;

procedure TfmRegistrySettings.btnClearMRUClick(Sender: TObject);
begin
  GetRegSettings.ClearMRU;
  btnClearMRU.Enabled := False;
end;

{$push}{$warn 5024 off}
procedure TfmRegistrySettings.eSettingsFileKeyUp(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    sbFileClick(Sender);
end;
{$pop}

procedure TfmRegistrySettings.FormCreate(Sender: TObject);
begin
  Caption := lisCnfCaption;
  tsGeneral.Caption := lisCnfTabGeneral;
  tsLogFile.Caption := lisCnfTabLogFile;
  tsExclusions.Caption := lisCnfTabExclusions;
  //tsIde.Caption := lisCnfDelphiIDE;
  label1.Caption := lisCnfGenConvertSettingsFile;
  rgWriteSettingsFile.Caption := lisCnfGenWriteSettings;
  rgWriteSettingsFile.Items[0] := lisCnfGenAlways;
  rgWriteSettingsFile.Items[1] := lisCnfGenFail;
  rgWriteSettingsFile.Items[2] := lisCnfGenNever;
  rgShowParseTree.Caption := lisCnfGenShowParseTree;
  rgShowParseTree.Items[0] := lisCnfGenAlways;
  rgShowParseTree.Items[1] := lisCnfGenOnParseError;
  rgShowParseTree.Items[2] := lisCnfGenNever;
  //label2.Caption := lisCnfGenMru;
  //btnClearMRU.Caption := lisCnfGenClearMru;
  //cbCheckMultibyteChars.Caption := lisCnfGenCheckMultibyte;

  rgLogLevel.Caption := lisCnfLogDetailLevel;
  rgLogLevel.Items[0] := lisCnfLogErrorsOnly;
  rgLogLevel.Items[1] := lisCnfLogFile;
  rgLogLevel.Items[2] := lisCnfLogToken;
  rgLogDir.Caption := lisCnfLogFileDir;
  Label3.Caption := lisCnfLogSelectDir;

  cbViewLog.Caption := lisCnfLogViewLog;
  cbLogTime.Caption := lisCnfLogTime;
  btnViewLog.Caption := lisCnfLogViewLogNow;
  lblBackupFileExt.Caption := lisCnfLogBackupFileExt;
  lblOutputFileExt.Caption := lisCnfLogOutputFileExt;

  lblFilesCaption.Caption := lisCnfExcludeFiles;
  lblDirsCaption.Caption := lisCnfExcludeDirs;

  //cbEditorIntegration.Caption := lisCnfIdeIntegration;
  //cbFormatAfterLoad.Caption := lisCnfIdeFormatAfterLoad;
  //cbFormatBeforeSave.Caption := lisCnfIdeFormatBeforeSave;
end;

procedure TfmRegistrySettings.OKButtonClick(Sender: TObject);
begin
  WriteSettings;
end;

procedure TfmRegistrySettings.sbFileClick(Sender: TObject);
begin
  dlgOpen.Filter := CONFIG_FILE_FILTERS;
  dlgOpen.InitialDir := ExtractFilePath(eSettingsFile.Text);
  dlgOpen.FileName := ExtractFileName(eSettingsFile.Text);
  if dlgOpen.Execute then
    eSettingsFile.Text := dlgOpen.FileName;
end;

procedure TfmRegistrySettings.FormResize(Sender: TObject);
const
  SPACING = 8;
  SMALL_SPACE = 4;
begin
  sbFile.Left := tsGeneral.ClientWidth - (sbFile.Width + SPACING);
  eSettingsFile.Width := tsGeneral.ClientWidth -
    (eSettingsFile.Left + sbFile.Width + SPACING + SMALL_SPACE);

  rgLogDir.Width := tsGeneral.ClientWidth - (rgLogDir.Left + SPACING);

  btnViewLog.Left := tsGeneral.ClientWidth - (btnViewLog.Width + SPACING);
end;

procedure TfmRegistrySettings.sbSpecifedDirClick(Sender: TObject);
begin
  dlgSelectDirectory.InitialDir := fsSpecifiedDirectory;
  if (dlgSelectDirectory.Execute) then
  begin
    fsSpecifiedDirectory := IncludeTrailingPathDelimiter(
      dlgSelectDirectory.FileName);
    ShowDirs;
  end;
end;

procedure TfmRegistrySettings.btnViewLogClick(Sender: TObject);
begin
  GetRegSettings.ViewLog;
end;

procedure TfmRegistrySettings.ShowDirs;
begin
  rgLogDir.Items[0] := Format(lisCnfLogTemp, [GetTempDir]);
  rgLogDir.Items[1] := Format(lisCnfLogApplication, [IncludeTrailingPathDelimiter(
    ExtractFileDir(ParamStr(0)))]);
  rgLogDir.Items[2] := Format(lisCnfLogSpecified, [fsSpecifiedDirectory]);
end;

procedure TfmRegistrySettings.tsExclusionsResize(Sender: TObject);
const
  SPACING = 8;
begin
  mFiles.Left  := SPACING;
  mFiles.Width := tsExclusions.ClientWidth - (SPACING * 2);

  mDirs.Left  := SPACING;
  mDirs.Width := tsExclusions.ClientWidth - (SPACING * 2);
end;

procedure TfmRegistrySettings.cbFormatAfterLoadClick(Sender: TObject);
begin
  if cbFormatAfterLoad.Checked then
    cbEditorIntegration.Checked := True;
end;

procedure TfmRegistrySettings.cbFormatBeforeSaveClick(Sender: TObject);
begin
  if cbFormatBeforeSave.Checked then
    cbEditorIntegration.Checked := True;
end;

end.
