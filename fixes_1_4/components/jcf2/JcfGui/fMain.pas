{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is fMain.pas, released April 2000.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved. 
Contributor(s): Michael Beck.
                        
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

unit fMain;

{ Created AFS 27 November 1999
  Main form for code formatting utility program
}


{$I ..\Include\JcfGlobal.inc}

interface

uses
  { delphi }
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls, Menus,
  ActnList, StdActns, ToolWin, ImgList, ShellAPI,
  { local }
  FileConverter, JcfSettings,  ConvertTypes,
  frBasicSettings, JvMRUManager, JvFormPlacement,
  JvMemo, frDrop, frmBaseSettingsFrame, JvComponent, JvExStdCtrls,
  JvComponentBase;

type
  TfrmMain = class(TForm)
    mnuMain: TMainMenu;
    mnuFile: TMenuItem;
    mnuGo:   TMenuItem;
    mnuClose: TMenuItem;
    mnuHelp: TMenuItem;
    mnuAbout: TMenuItem;
    tlbTop:  TToolBar;
    tbtnOpenFiles: TToolButton;
    btnGo:   TToolButton;
    btnAbout: TToolButton;
    tbtnToolButton6: TToolButton;
    btnSettings: TToolButton;
    ilStandardImages: TImageList;
    tbtnToolButton8: TToolButton;
    tbtnToolButton2: TToolButton;
    ActionList: TActionList;
    aOpenFiles: TAction;
    aOptions: TAction;
    aGo:     TAction;
    aAbout:  TAction;
    aExit:   TAction;
    tbtnToolButton4: TToolButton;
    btnClose: TToolButton;
    OpenFile1: TMenuItem;
    mnuFormatSettings: TMenuItem;
    mnuViewLog: TMenuItem;
    dlgSaveConfig: TSaveDialog;
    actHelpContents: THelpContents;
    mnuContents: TMenuItem;
    tbHelp:  TToolButton;
    mnuSettings: TMenuItem;
    mnuRegistrySettings: TMenuItem;
    mruFiles: TJvMRUManager;
    dlgOpen: TOpenDialog;
    frBasic: TfrBasic;
    N1:      TMenuItem;
    mnuSaveSettingsAs: TMenuItem;
    aSaveSettingsAs: TAction;
    JvFormStorage1: TJvFormStorage;
    mOutput: TJvMemo;
    lblLog:  TLabel;
    N2:      TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuGoClick(Sender: TObject);
    procedure mnuCloseClick(Sender: TObject);
    procedure aFormatExecute(Sender: TObject);
    procedure aAboutExecue(Sender: TObject);
    procedure aExitExecute(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure aOpenFilesExecute(Sender: TObject);
    procedure aOptionsExecute(Sender: TObject);
    procedure mnuViewLogClick(Sender: TObject);
    procedure mnuSaveSettingsAsClick(Sender: TObject);
    procedure actHelpContentsExecute(Sender: TObject);
    procedure mnuRegistrySettingsClick(Sender: TObject);
    procedure mruFilesClick(Sender: TObject; const RecentName, Caption: string;
      UserData: integer);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure frBasicsbOpenClick(Sender: TObject);
  private
    fcConverter: TFileConverter;

    procedure ShowStatusMesssage(const psFile, psMessage: string;
      const peMessageType: TStatusMessageType;
      const piY, piX: integer);

    procedure DoFormat;
    procedure ShowAbout;
    procedure SettingsChange(Sender: TObject);

  public
  end;

var
  frmMain: TfrmMain;

implementation

{$ifdef FPC}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

uses
  { deplhi }
  Windows,
  { jcl }
  JclFileUtils,
  { local }
  fAbout, fAllSettings, fRegistrySettings,
  SettingsStream, JcfHelp, JcfRegistrySettings, JcfFontSetFunctions;

function OkDialog(const psMsg: string): boolean;
begin
  Result := MessageDlg(psMsg, mtWarning, [mbYes, mbCancel], 0) = mrYes;
end;

procedure ErrorDialog(const psMsg: string);
begin
  MessageDlg(psMsg, mtError, [mbOK], 0);
end;

{------------------------------------------------------------------------------
  worker procs }


procedure TfrmMain.DoFormat;
var
  lcRegSet: TJCFRegistrySettings;
  lsSource, lsFileDesc, lsMessage: string;
begin
  frBasic.Write;

  lcRegSet := GetRegSettings;

  lsSource := lcRegSet.Input;

  if (lcRegSet.SourceMode = fmSingleFile) and (ExtractFileName(lsSource) = '') then
  begin
    ErrorDialog('No file specified in direcory');
    Exit;
  end;

  if (lsSource = '') then
  begin
    ErrorDialog('No files to format');
    exit;
  end;

  if lcRegSet.SourceMode = fmSingleFile then
    lsFileDesc := 'the file ' + ExtractFileName(lsSource)
  else
    lsFileDesc := 'the files in ' + lsSource;

  { confirm before obfuscate }
  if FormatSettings.Obfuscate.Enabled then
  begin
    lsMessage := 'Are you sure that you want to obfuscate ' + lsFileDesc;

    if lcRegSet.BackupMode = cmInPlace then
      lsMessage := lsMessage + ' without backup';

    lsMessage := lsMessage + '?';
    if not OkDialog(lsMessage) then
      exit;
  end
  else if (lcRegSet.BackupMode = cmInPlace) then
  begin
    lsMessage := 'Are you sure you want to convert ' + lsFileDesc + ' without backup?';
    if not OkDialog(lsMessage) then
      exit;
  end;

  fcConverter.Input := lsSource;
  fcConverter.BackupMode := frBasic.GetCurrentBackupMode;
  fcConverter.SourceMode := frBasic.GetCurrentSourceMode;

  fcConverter.Convert;
end;

procedure TfrmMain.ShowAbout;
var
  fAbout: TfrmAboutBox;
begin
  fAbout := TfrmAboutBox.Create(self);
  try
    fAbout.ShowModal;
  finally
    fAbout.Release;
  end;
end;



{------------------------------------------------------------------------------
  event handlers}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  SetObjectFontToSystemFont(Self);

  Application.HelpFile := GetHelpFilePath;

  Randomize;

  GetRegSettings.MRUFiles := mruFiles.Strings;
  GetRegSettings.ReadAll;

  fcConverter := TFileConverter.Create;
  fcConverter.OnStatusMessage := ShowStatusMesssage;

  frBasic.mruFiles := mruFiles;
  frBasic.Read;
  frBasic.OnChange := SettingsChange;
  SettingsChange(nil);

  FormResize(nil);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  frBasic.Write;
  FreeAndNil(fcConverter);

  GetRegSettings.WriteAll;
  GetRegSettings.MRUFiles := nil;
end;


procedure TfrmMain.ShowStatusMesssage(const psFile, psMessage: string;
  const peMessageType: TStatusMessageType;
  const piY, piX: integer);
var
  lsMessage: string;
begin
  { show the message }
  lsMessage := psMessage;
  if (piX > 0) and (piY > 0) then
    lsMessage := lsMessage + ' near line ' + IntToStr(piY) + ' col ' + IntToStr(piX);
  mOutput.Lines.Add(lsMessage);

  { scroll into view and check the srollbar }
  mOutput.CurrentLine := mOutput.Lines.Count - 1;
  if mOutput.Lines.Count > 1 then
    mOutput.ScrollBars := ssVertical
  else
    mOutput.ScrollBars := ssNone;

  Application.ProcessMessages;
end;

procedure TfrmMain.mnuGoClick(Sender: TObject);
begin
  DoFormat;
end;

procedure TfrmMain.mnuCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.aFormatExecute(Sender: TObject);
begin
  DoFormat;
end;

procedure TfrmMain.aAboutExecue(Sender: TObject);
begin
  ShowAbout;
end;

procedure TfrmMain.aExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.mnuAboutClick(Sender: TObject);
begin
  ShowAbout;
end;

procedure TfrmMain.aOpenFilesExecute(Sender: TObject);
begin
  frBasic.DoFileOpen;
end;

procedure TfrmMain.aOptionsExecute(Sender: TObject);
var
  lfSet: TFormAllSettings;
begin
  lfSet := TFormAllSettings.Create(Self);
  try
    lfSet.Execute;
    frBasic.DisplayOutputFile;
  finally
    lfSet.Release;
  end;
end;

procedure TfrmMain.SettingsChange(Sender: TObject);
begin
  btnGo.Hint := frBasic.GetGoHint;

  if frBasic.GetCurrentSourceMode = fmSingleFile then
    tbtnOpenFiles.Hint := 'Select a source file'
  else
    tbtnOpenFiles.Hint := 'Select a source directory';
end;

procedure TfrmMain.mnuViewLogClick(Sender: TObject);
begin
  GetRegSettings.ViewLog;
end;


const
  CONFIG_FILTER = 'Config files (*.cfg)|*.cfg|Text files (*.txt)|' +
    '*.txt|XML files (*.xml)|*.xml|All files (*.*)|*.*';

procedure TfrmMain.mnuSaveSettingsAsClick(Sender: TObject);
var
  lsName: string;
  dlgSaveConfig: TSaveDialog;
  lcFile: TSettingsStreamOutput;
begin
  lsName := '';

  dlgSaveConfig := TSaveDialog.Create(self);
  try
    dlgSaveConfig.FileName := 'Saved.cfg';
    dlgSaveConfig.InitialDir := ExtractFilePath(Application.ExeName);
    dlgSaveConfig.DefaultExt := '.cfg';
    dlgSaveCOnfig.Filter := CONFIG_FILTER;

    if dlgSaveConfig.Execute then
    begin
      lsName := dlgSaveCOnfig.FileName;
    end;

    if lsName = '' then
      exit;

  finally
    dlgSaveConfig.Free;
  end;

  lcFile := TSettingsStreamOutput.Create(lsName);
  try
    FormatSettings.ToStream(lcFile);
  finally
    lcFile.Free;
  end;
end;

procedure TfrmMain.actHelpContentsExecute(Sender: TObject);
begin
  try
    Application.HelpContext(HELP_MAIN);
  except
    if FileExists(Application.HelpFile) then
      ShellExecute(Handle, 'open', PChar(Application.HelpFile), nil, nil, SW_SHOWNORMAL);
  end;
end;

procedure TfrmMain.mnuRegistrySettingsClick(Sender: TObject);
var
  lfSettings: TfmRegistrySettings;
begin
  lfSettings := TfmRegistrySettings.Create(self);
  try
    lfSettings.Execute;
    frBasic.DisplayOutputFile;
  finally
    lfSettings.Release;
  end;
end;

procedure TfrmMain.mruFilesClick(Sender: TObject; const RecentName, Caption: string;
  UserData: integer);
begin
  frBasic.DoFileOpen(RecentName);
end;

procedure TfrmMain.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_F1 then
    try
      Application.HelpContext(HELP_MAIN);
    except
      if FileExists(Application.HelpFile) then
        ShellExecute(Handle, 'open', PChar(Application.HelpFile),
          nil, nil, SW_SHOWNORMAL);
    end;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  {frBasic.Left  := 0;
  frBasic.Top   := tlbTop.Top + tlbTop.Height;
  frBasic.Width := ClientWidth;

  mOutput.Left   := 2;
  mOutput.Width  := ClientWidth - 4;
  mOutput.Top    := lblLog.Top + lblLog.Height + 4;
  mOutput.Height := ClientHeight - (mOutput.Top + 4);}
end;

procedure TfrmMain.frBasicsbOpenClick(Sender: TObject);
begin
  frBasic.sbOpenClick(Sender);
end;

end.
