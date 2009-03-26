unit frBasicSettings;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frBasicSettings.pas, released April 2000.
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


{$I JcfGlobal.inc}

interface

uses
  { delphi }
  SysUtils, Classes, Controls, Forms, Dialogs,
  Buttons, StdCtrls, ExtCtrls,
  { local }
  JvMRUManager,
  ConvertTypes, frmBaseSettingsFrame, JvBaseDlg, JvBrowseFolder;

type
  TfrBasic = class(TfrSettingsFrame)
    rgFileRecurse: TRadioGroup;
    rgBackup: TRadioGroup;
    edtInput: TEdit;
    edtOutput: TEdit;
    lblOutput: TLabel;
    lblInput: TLabel;
    sbOpen:  TSpeedButton;
    dlgOpen: TOpenDialog;
    JvBrowseForFolderDialog1: TJvBrowseForFolderDialog;
    procedure rgFileRecurseClick(Sender: TObject);
    procedure rgBackupClick(Sender: TObject);
    procedure rgModeClick(Sender: TObject);
    procedure edtInputDragOver(Sender, Source: TObject; X, Y: integer;
      State: TDragState; var Accept: boolean);
    procedure edtInputDragDrop(Sender, Source: TObject; X, Y: integer);
    procedure edtInputKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure sbOpenClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    procedure AddCheckMRU(const psFile: string);

  protected

    procedure DragItemDropped(const piFormat: integer; const psItem: string); override;

  public
    // ref to main form
    mruFiles: TJvMRUManager;

    constructor Create(AOwner: TComponent); override;

    procedure Read; override;
    procedure Write; override;

    function GetCurrentBackupMode: TBackupMode;
    function GetCurrentSourceMode: TSourceMode;
    procedure SetCurrentSourceMode(const peValue: TSourceMode);

    function GetGoHint: string;

    procedure DisplayOutputFile;
    procedure DoFileOpen(const psName: string = '');
  end;

implementation

uses
  { jcl }
  jclFileUtils,
  { local }
  JcfHelp, JcfSettings, JcfRegistrySettings;

{$ifdef FPC}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

constructor TfrBasic.Create(AOwner: TComponent);
var
  lbShowFileName: boolean;
begin
  inherited;
  IsDropActive := True;

  { the filename setting etc. is not relevant to the IDE pluggin }

  {$IFDEF IDEPLUGGIN}
  lbShowFileName := False;
  {$ELSE}
  lbShowFileName := True;
  {$ENDIF}

  lblInput.Visible  := lbShowFileName;
  lblOutput.Visible := lbShowFileName;
  edtInput.Visible  := lbShowFileName;
  edtOutput.Visible := lbShowFileName;
  sbOpen.Visible    := lbShowFileName;

  rgFileRecurse.Visible := lbShowFileName;
  rgBackup.Visible      := lbShowFileName;

  fiHelpContext := HELP_BASIC_SETTINGS;
end;

procedure TfrBasic.DragItemDropped(const piFormat: integer; const psItem: string);
begin
  // can only be from the input edit box
  edtInput.Text := psItem;
  DisplayOutputFile;
end;

function TfrBasic.GetCurrentBackupMode: TBackupMode;
begin
  Result := TBackupMode(rgBackup.ItemIndex);
end;

function TfrBasic.GetCurrentSourceMode: TSourceMode;
begin
  Result := TSourceMode(rgFileRecurse.ItemIndex);
end;

procedure TfrBasic.SetCurrentSourceMode(const peValue: TSourceMode);
begin
  if GetCurrentSourceMode <> peValue then
  begin
    rgFileRecurse.ItemIndex := Ord(peValue);
    rgFileRecurseClick(nil);
  end;
end;

procedure TfrBasic.DisplayOutputFile;
{$IFNDEF IDEPLUGGIN}
var
  bShowOutput: boolean;
{$ENDIF}
begin
  case GetCurrentBackupMode of
    cmInPlace:
      lblOutput.Caption := '';
    cmInPlaceWithBackup:
      lblOutput.Caption := 'Backup file';
    cmSeparateOutput:
      lblOutput.Caption := 'Output file';
    else
      raise Exception.Create('TfrmMain.DisplayOutputFile: bad backup group index');
  end;

  {$IFNDEF IDEPLUGGIN}
  if FormatSettings = nil then
    edtOutput.Text := ''
  else
    edtOutput.Text :=
      GetRegSettings.GetOutputFileName(edtInput.Text, GetCurrentBackupMode);

  bShowOutput := (GetCurrentBackupMode <> cmInplace) and
    (GetCurrentSourceMode = fmSingleFIle);

  lblOutput.Visible := bShowOutput;
  edtOutput.Visible := bShowOutput;
  {$ENDIF}
end;

procedure TfrBasic.rgModeClick(Sender: TObject);
begin
  CallOnChange;
end;

{-------------------------------------------------------------------------------
  event handlers }

procedure TfrBasic.rgFileRecurseClick(Sender: TObject);
begin
  inherited;
  case GetCurrentSourceMode of
    fmSingleFile:
      lblInput.Caption := 'Input file';
    fmDirectory:
    begin
      lblInput.Caption := 'Directory';
      edtInput.Text    := IncludeTrailingPathDelimiter(ExtractFileDir(edtInput.Text));
    end;
    fmDirectoryRecursive:
    begin
      lblInput.Caption := 'Start directory';
      edtInput.Text    := IncludeTrailingPathDelimiter(ExtractFileDir(edtInput.Text));
    end;
  end;

  DisplayOutputFile;
  CallOnChange;
end;

procedure TfrBasic.rgBackupClick(Sender: TObject);
begin
  DisplayOutputFile;
  CallOnChange;
end;


procedure TfrBasic.Read;
var
  lcRegSet: TJCFRegistrySettings;
begin
  lcRegSet := GetRegSettings;

  rgFileRecurse.ItemIndex := Ord(lcRegSet.SourceMode);
  rgBackup.ItemIndex := Ord(lcRegSet.BackupMode);
  edtInput.Text := lcRegSet.Input;

  DisplayOutputFile;
end;

procedure TfrBasic.Write;
var
  lcRegSet: TJCFRegistrySettings;
begin
  lcRegSet := GetRegSettings;

  lcRegSet.SourceMode := GetCurrentSourceMode;
  lcRegSet.BackupMode := GetCurrentBackupMode;
  lcRegSet.Input      := edtInput.Text;
end;

function TfrBasic.GetGoHint: string;
begin
  if FormatSettings.Obfuscate.Enabled then
    Result := 'Obfuscate'
  else
    Result := 'Format';

  case GetCurrentSourceMode of
    fmSingleFile:
      Result := Result + ' file';
    fmDirectory:
      Result := Result + ' directory';
    fmDirectoryRecursive:
      Result := Result + ' directory heirarchy';
  end;

  case GetCurrentBackupMode of
    cmInPlace:
      Result := Result + ' in place';
    cmInPlaceWithBackup:
      Result := Result + ' with backup';
    cmSeparateOutput:
      Result := Result + ' to output';
  end;
end;

procedure TfrBasic.DoFileOpen(const psName: string);
var
  lsDir: string;
begin

  if psName = '' then
  begin
    // get a file name
    lsDir := IncludeTrailingPathDelimiter(ExtractFileDir(edtInput.Text));
    // strip out the dir

    dlgOpen.InitialDir := lsDir;
    dlgOpen.FileName   := extractFileName(edtInput.Text);
    dlgOpen.Filter     := SOURCE_FILE_FILTERS;

    if GetCurrentSourceMode = fmSingleFile then
    begin
      if dlgOpen.Execute then
      begin
        edtInput.Text := dlgOpen.FileName;
        AddCheckMRU(edtInput.Text);

        DisplayOutputFile;
      end;
    end
    else
    begin
      JvBrowseForFolderDialog1.Directory := edtInput.Text;
      if (JvBrowseForFolderDialog1.Execute) then
      begin
        edtInput.Text := IncludeTrailingPathDelimiter(JvBrowseForFolderDialog1.Directory);
        AddCheckMRU(edtInput.Text);
        DisplayOutputFile;
      end;
    end;
  end
  else
  begin

    // have a name. Is it a dir or a file?
    if DirectoryExists(psName) then
    begin
      edtInput.Text := IncludeTrailingPathDelimiter(psName);

      AddCheckMRU(edtInput.Text);
      DisplayOutputFile;

      if GetCurrentSourceMode = fmSingleFile then
        SetCurrentSourceMode(fmDirectory);
    end
    else if FileExists(psName) then
    begin
      edtInput.Text := psName;

      AddCheckMRU(edtInput.Text);
      DisplayOutputFile;

      if GetCurrentSourceMode <> fmSingleFile then
        SetCUrrentSourceMode(fmSingleFile);
    end;
  end;
end;


procedure TfrBasic.AddCheckMRU(const psFile: string);
var
  liIndex: integer;
begin
  liIndex := mruFiles.Strings.IndexOf(psFile);

  if (liIndex < 0) then
  begin
    mruFiles.Add(psFile, 0);
    liIndex := mruFiles.Strings.IndexOf(psFile);
  end;

  mruFiles.Strings.Move(liIndex, 0);

  while mruFiles.Strings.Count > mruFiles.Capacity do
    mruFiles.Strings.Delete(mruFiles.Strings.Count - 1);
end;

{------------------------------------------------------------------------------
  event handlers }

procedure TfrBasic.edtInputDragOver(Sender, Source: TObject; X, Y: integer;
  State: TDragState; var Accept: boolean);
begin
  Accept := True;
end;

procedure TfrBasic.edtInputDragDrop(Sender, Source: TObject; X, Y: integer);
begin
  HandleShellDragDrop(Source);
end;

procedure TfrBasic.edtInputKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  DisplayOutputFile;
end;

procedure TfrBasic.sbOpenClick(Sender: TObject);
begin
  DoFileOpen;
end;

procedure TfrBasic.FrameResize(Sender: TObject);
const
  SPACING     = 8;
  SMALL_SPACE = 2;
begin
  {inherited;
  // these fill width
  sbOpen.Left     := ClientWidth - (sbOpen.Width + SPACING);
  edtInput.Width  := (sbOpen.Left - SMALL_SPACE) - edtInput.Left;
  edtOutput.Width := (ClientWidth - SPACING) - edtOutput.Left;}
end;

end.
