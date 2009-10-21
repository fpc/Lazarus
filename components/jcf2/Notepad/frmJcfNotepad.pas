unit frmJCFNotepad;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is frmJCFNotepad, released May 2003.
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
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ActnList,
  Buttons, Menus, ShellAPI,
  { JEDI }
  JvMRUManager, JvMemo, JvComponent, JvExStdCtrls, JvFormPlacement, JvComponentBase,
  { local }
  JcfRegistrySettings, Converter, ConvertTypes, JcfUnicodeFiles;

{ have to do file pos display *after* various processing }
const
  WM_SHOWFILEPOS = WM_USER + 42;

type
  TfmJCFNotepad = class(TForm)
    sb1:          TStatusBar;
    pnlTop:       TPanel;
    pcPages:      TPageControl;
    tsInput:      TTabSheet;
    tsOutput:     TTabSheet;
    mInput:       TJvMemo;
    mOutput:      TJvMemo;
    mMessages:    TJvMemo;
    lblMessages:  TLabel;
    sbLoad:       TSpeedButton;
    sbSave:       TSpeedButton;
    sbGo:         TSpeedButton;
    ActionList1:  TActionList;
    actOpen:      TAction;
    actSave:      TAction;
    actGo:        TAction;
    OpenDialog1:  TOpenDialog;
    SaveDialog1:  TSaveDialog;
    actClear:     TAction;
    sbClear:      TSpeedButton;
    MainMenu1:    TMainMenu;
    mnuFile:      TMenuItem;
    mnuFileOpen:  TMenuItem;
    mnuFileSaveOut: TMenuItem;
    mnuExit:      TMenuItem;
    mnuSettings:  TMenuItem;
    actCopy:      TAction;
    actPaste:     TAction;
    N1:           TMenuItem;
    mruFiles:     TJvMRUManager;
    mnuEdit:      TMenuItem;
    mnuEditPaste: TMenuItem;
    mnuEditCopy:  TMenuItem;
    mnuEditGo:    TMenuItem;
    mnuEditClear: TMenuItem;
    mnuEditCut:   TMenuItem;
    mnuEditCopyOutput: TMenuItem;
    mnuEditSelectAll: TMenuItem;
    mnuEditCopyMessages: TMenuItem;
    mnuFormat:    TMenuItem;
    mnuFileSaveInAs: TMenuItem;
    mnuHelp:      TMenuItem;
    mnuHelpAbout: TMenuItem;
    mnuShowRegSetting: TMenuItem;
    mnuFormatSettings: TMenuItem;
    ActCut:       TAction;
    Contents1:    TMenuItem;
    JvFormStorage1: TJvFormStorage;
    mnuFileSaveIn: TMenuItem;
    procedure FormResize(Sender: TObject);
    procedure pcPagesChange(Sender: TObject);
    procedure actGoExecute(Sender: TObject);
    procedure mInputKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure mruFilesClick(Sender: TObject; const RecentName, Caption: string;
      UserData: integer);
    procedure mnuEditCopyOutputClick(Sender: TObject);
    procedure mnuEditSelectAllClick(Sender: TObject);
    procedure mnuEditCopyMessagesClick(Sender: TObject);
    procedure mnuFileSaveInAsClick(Sender: TObject);
    procedure mnuHelpAboutClick(Sender: TObject);
    procedure mnuShowRegSettingClick(Sender: TObject);
    procedure mnuFormatSettingsClick(Sender: TObject);
    procedure ActCutExecute(Sender: TObject);
    procedure mInputMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Contents1Click(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure mOutputKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure mInputKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure mInputEnter(Sender: TObject);
    procedure mOutputEnter(Sender: TObject);
    procedure mInputClick(Sender: TObject);
    procedure mOutputClick(Sender: TObject);
    procedure mInputKeyPress(Sender: TObject; var Key: char);
    procedure mnuFileSaveInClick(Sender: TObject);
  private
    fcConvert: TConverter;
    fsLastInputFileName: string;
    feLastInputContentType: TFileContentType;

    procedure OnConvertStatusMessage(const psUnit, psMessage: string;
     const peMessageType: TStatusMessageType;
     const piY, piX: integer);

    procedure CheckInputState;
    procedure CheckCutPasteState;
    procedure DoFileOpen(const psFileName: string);
    procedure SaveInputToFile(const psFileName: string);

    procedure CheckSaveEnabled;

    procedure AddCheckMRU(const psFile: string);

    procedure ShowFilePos;
    procedure SendShowFilePos;

    procedure OnReceiveShowFilePos(var msg: TMessage); message WM_SHOWFILEPOS;
  public
  end;

var
  fmJCFNotepad: TfmJCFNotepad;

implementation

uses
  { delphi }
  ClipBrd,
  { local }
  JcfStringUtils,
  JcfHelp, fAbout, fRegistrySettings, fAllSettings, JcfFontSetFunctions;

{$ifdef FPC}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

procedure TfmJCFNotepad.CheckInputState;
begin
  actGo.Enabled := (mInput.Text <> '');
  actClear.Enabled := (mInput.Text <> '');
end;

procedure TfmJCFNotepad.CheckSaveEnabled;
begin
  mnuFileSaveIn.Enabled := (fsLastInputFileName <> '');
end;

procedure TfmJCFNotepad.CheckCutPasteState;
var
  lbHasOutput: boolean;
begin
  actPaste.Enabled := (pcPages.ActivePage = tsInput) and Clipboard.HasFormat(CF_TEXT);
  actCut.Enabled := (pcPages.ActivePage = tsInput) and (mInput.SelLength > 0);

  lbHasOutput := (pcPages.ActivePage = tsOutput) and (mOutput.Text <> '');
  actSave.Enabled := lbHasOutput;

  if pcPages.ActivePage = tsOutput then
    actCopy.Enabled := lbHasOutput
  else
    actCopy.Enabled := (mInput.Text <> '');

end;

procedure TfmJCFNotepad.DoFileOpen(const psFileName: string);
var
  lsFileContents: WideString;
begin
  if psFileName = '' then
    exit;
  if not FileExists(psFileName) then
    exit;

  GetRegSettings.InputDir := ExtractFilePath(psFileName);

  ReadTextFile(psFileName, lsFileContents, feLastInputContentType);

  mInput.Text := lsFileContents;
  sb1.Panels[1].Text := psFileName;
  AddCheckMRU(psFileName);
  fsLastInputFileName := psFileName;

  CheckInputState;
  SendShowFilePos;
  CheckSaveEnabled;
end;

procedure TfmJCFNotepad.SaveInputToFile(const psFileName: string);
begin
  if psFileName = '' then
    exit;

  // default the file type to utf-8 if it's not known
  if feLastInputContentType = eUnknown then
  begin
    feLastInputContentType := eUtf8;
  end;

  WriteTextFile(psFileName, mInput.Text, feLastInputContentType);


  sb1.Panels[1].Text := 'Saved input as ' + psFileName;
  AddCheckMRU(psFileName);

  fsLastInputFileName := psFileName;
  CheckSaveEnabled;
end;

procedure TfmJCFNotepad.AddCheckMRU(const psFile: string);
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

procedure TfmJCFNotepad.FormResize(Sender: TObject);
const
  OUTPUT_PAD = 4;
begin
  mOutput.Left := OUTPUT_PAD;
  mOutput.Top  := OUTPUT_PAD;
  mOutput.Width := tsOutput.ClientWidth - (2 * OUTPUT_PAD);

  // two thirds height
  mOutput.Height := (tsOutput.Height * 2 div 3) - (2 * OUTPUT_PAD);

  lblMessages.Left := 4;
  lblMessages.Top  := mOutput.Top + mOutput.Height + OUTPUT_PAD;

  mMessages.Top  := lblMessages.Top + lblMessages.Height + OUTPUT_PAD;
  mMessages.Height := tsOutput.ClientHeight - (lblMessages.Top +
    lblMessages.Height + (OUTPUT_PAD * 2));
  mMessages.Left := OUTPUT_PAD;
  mMessages.Width := tsOutput.ClientWidth - (2 * OUTPUT_PAD);
end;

procedure TfmJCFNotepad.pcPagesChange(Sender: TObject);
begin
  CheckCutPasteState;
  SendShowFilePos;
end;

procedure TfmJCFNotepad.actGoExecute(Sender: TObject);
begin
  actGo.Enabled := False;

  mMessages.Clear;
  fcConvert.OnStatusMessage := OnConvertStatusMessage;

  fcConvert.InputCode := mInput.Lines.Text;
  fcConvert.Convert;
  mOutput.Lines.Text := fcConvert.OutputCode;
  fcConvert.Clear;

  pcPages.ActivePage := tsOutput;
  pcPagesChange(nil);

  CheckInputState;
end;

procedure TfmJCFNotepad.mInputKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  CheckInputState;
  CheckCutPasteState;
end;


procedure TfmJCFNotepad.FormShow(Sender: TObject);
begin
  CheckInputState;
  CheckCutPasteState;
end;

procedure TfmJCFNotepad.actOpenExecute(Sender: TObject);
begin
  OpenDialog1.InitialDir := GetRegSettings.InputDir;
  OpenDialog1.Filter := SOURCE_FILE_FILTERS;

  if OpenDialog1.Execute then
  begin
    pcPages.ActivePage := tsInput;
    DoFileOpen(OpenDialog1.FileName);
  end;
end;

procedure TfmJCFNotepad.actClearExecute(Sender: TObject);
begin
  mInput.Text  := '';
  mOutput.Text := '';
  mMessages.Text := '';
  pcPages.ActivePage := tsInput;

  CheckInputState;
  CheckCutPasteState;
end;

procedure TfmJCFNotepad.actSaveExecute(Sender: TObject);
begin
  SaveDialog1.InitialDir := GetRegSettings.OutputDir;
  SaveDialog1.Title  := 'Save output file';
  SaveDialog1.Filter := SOURCE_FILE_FILTERS;


  if SaveDialog1.Execute then
  begin
    GetRegSettings.OutputDir := ExtractFilePath(SaveDialog1.FileName);
    StringToFile(SaveDialog1.FileName, AnsiString(mOutput.Text));
    sb1.Panels[1].Text := 'Saved output: ' + SaveDialog1.FileName;
    AddCheckMRU(SaveDialog1.FileName);
  end;
end;

procedure TfmJCFNotepad.mnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfmJCFNotepad.FormCreate(Sender: TObject);
var
  lsHelpFile: string;
begin
  SetObjectFontToSystemFont(Self);

  //lsHelpFile := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'CodeFormat.chm';
  //lsHelpFile := IncludeTrailingPathDelimiter(ExtractFilePath(GetModuleName(GetModuleHandle('JCFIdeD11.bpl')))) + 'CodeFormat.chm';
  lsHelpFile := GetHelpFilePath;

  if FileExists(lsHelpFile) then
    Application.HelpFile := lsHelpFile;

  fcConvert := TConverter.Create;

  GetRegSettings.MRUFiles := mruFiles.Strings;
  GetRegSettings.ReadAll;

  mruFiles.RemoveInvalid;

  pcPages.ActivePage := tsInput;

  feLastInputContentType := eUnknown;
  fsLastInputFileName := '';

  CheckSaveEnabled;
end;

procedure TfmJCFNotepad.FormDestroy(Sender: TObject);
begin
  GetRegSettings.WriteAll;
  GetRegSettings.MRUFiles := nil;

  FreeAndNil(fcConvert);
end;

procedure TfmJCFNotepad.actCopyExecute(Sender: TObject);
begin
  if pcPages.ActivePage = tsOutput then
    mOutput.CopyToClipboard
  else
    mInput.CopyToClipboard;
end;

procedure TfmJCFNotepad.actPasteExecute(Sender: TObject);
begin
  if (pcPages.ActivePage = tsInput) and Clipboard.HasFormat(CF_TEXT) then
  begin
    mInput.PasteFromClipboard;
    CheckInputState;
  end;
end;

procedure TfmJCFNotepad.mruFilesClick(Sender: TObject;
  const RecentName, Caption: string; UserData: integer);
begin
  DoFileOpen(RecentName);
end;

procedure TfmJCFNotepad.mnuEditCopyOutputClick(Sender: TObject);
begin
  Clipboard.AsText := mOutput.Text;
end;

procedure TfmJCFNotepad.ActCutExecute(Sender: TObject);
begin
  if (pcPages.ActivePage = tsInput) then
  begin
    mInput.CutToClipboard;
    CheckInputState;
  end;
end;

procedure TfmJCFNotepad.mnuEditSelectAllClick(Sender: TObject);
begin
  if (pcPages.ActivePage = tsInput) then
  begin
    mInput.SetFocus;
    mInput.SelectAll;
    CheckCutPasteState;
  end
  else
  begin
    mOutput.SetFocus;
    mOutput.SelectAll;
  end;
end;

procedure TfmJCFNotepad.mnuEditCopyMessagesClick(Sender: TObject);
begin
  Clipboard.AsText := mMessages.Text;
end;

procedure TfmJCFNotepad.mnuFileSaveInClick(Sender: TObject);
begin
  if fsLastInputFileName <> '' then
  begin
    SaveInputToFile(fsLastInputFileName);
  end;

end;


procedure TfmJCFNotepad.mnuFileSaveInAsClick(Sender: TObject);
begin
  SaveDialog1.InitialDir := GetRegSettings.OutputDir;
  SaveDialog1.Title  := 'Save input file';
  SaveDialog1.Filter := SOURCE_FILE_FILTERS;

  if SaveDialog1.Execute then
  begin
    GetRegSettings.OutputDir := ExtractFilePath(SaveDialog1.FileName);

    SaveInputToFile(SaveDialog1.FileName);
  end;
end;

procedure TfmJCFNotepad.mnuHelpAboutClick(Sender: TObject);
var
  lfAbout: TfrmAboutBox;
begin
  lfAbout := TfrmAboutBox.Create(self);
  try
    lfAbout.ShowModal;
  finally
    lfAbout.Release;
  end;
end;

procedure TfmJCFNotepad.mnuShowRegSettingClick(Sender: TObject);
var
  lfSettings: TfmRegistrySettings;
begin
  lfSettings := TfmRegistrySettings.Create(self);
  try
    lfSettings.Execute;
  finally
    lfSettings.Release;
  end;
end;

procedure TfmJCFNotepad.mnuFormatSettingsClick(Sender: TObject);
var
  lfAllSettings: TFormAllSettings;
begin
  lfAllSettings := TFormAllSettings.Create(self);
  try
    lfAllSettings.Execute;
  finally
    lfAllSettings.Release;
  end;
end;

procedure TfmJCFNotepad.mInputMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  CheckCutPasteState;
end;

procedure TfmJCFNotepad.Contents1Click(Sender: TObject);
begin
  try
    Application.HelpContext(HELP_MAIN);
  except
    ShellExecute(Handle, 'open', PChar(Application.HelpFile), nil, nil, SW_SHOWNORMAL);
  end;
end;

procedure TfmJCFNotepad.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_F1 then
    try
      Application.HelpContext(HELP_MAIN);
    except
      ShellExecute(Handle, 'open', PChar(Application.HelpFile), nil, nil, SW_SHOWNORMAL);
    end;
end;

procedure TfmJCFNotepad.ShowFilePos;
const
  POS_NUM_LEN = 4;
var
  liX, liY: integer;
  lsPos: string;
  lsX, lsY: string;
begin
  if pcPages.ActivePage = tsInput then
  begin
    liX := mInput.CaretPos.X;
    liY := mInput.CaretPos.Y;
  end
  else
  begin
    liX := mOutput.CaretPos.X;
    liY := mOutput.CaretPos.Y;
  end;

  { index in delphi is 1-based not 0-based,
    ie starts at line 1 char 1}
  Inc(liX);
  Inc(liY);

  if liX > 0 then
    lsX := StrPadLeft(IntToStr(liX), POS_NUM_LEN, ' ')
  else
    lsX := '?';

  if liY > 0 then
    lsY := StrPadLeft(IntToStr(liY), POS_NUM_LEN, ' ')
  else
    lsY := '?';

  lsPos := lsY + ':' + lsX;

  sb1.Panels[0].Text := lsPos;
end;

procedure TfmJCFNotepad.SendShowFilePos;
begin
  { send a note to yourself to redisplay this
    as soon as the current operation is finished }
  PostMessage(Handle, WM_SHOWFILEPOS, 0, 0);
end;

{ triggered by SendShowFilePos }
procedure TfmJCFNotepad.OnReceiveShowFilePos(var msg: TMessage);
begin
  ShowFilePos;
end;

procedure TfmJCFNotepad.mOutputKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  SendShowFilePos;
end;

procedure TfmJCFNotepad.mInputKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  SendShowFilePos;
end;

procedure TfmJCFNotepad.mInputEnter(Sender: TObject);
begin
  SendShowFilePos;
end;

procedure TfmJCFNotepad.mOutputEnter(Sender: TObject);
begin
  SendShowFilePos;
end;

procedure TfmJCFNotepad.mInputClick(Sender: TObject);
begin
  SendShowFilePos;
end;

procedure TfmJCFNotepad.mOutputClick(Sender: TObject);
begin
  SendShowFilePos;
end;

procedure TfmJCFNotepad.mInputKeyPress(Sender: TObject; var Key: char);
begin
  SendShowFilePos;
end;

procedure TfmJCFNotepad.OnConvertStatusMessage(const psUnit, psMessage: string;
  const peMessageType: TStatusMessageType;
  const piY, piX: integer);
var
  lsWholeMessage: string;
begin
  lsWholeMessage := psMessage;
  if (piY >= 0) and (piX >= 0) then
    lsWholeMessage := lsWholeMessage + ' at line ' + IntToStr(piY) +
      ' col ' + IntToStr(piX);

  mMessages.Lines.Add(lsWholeMessage);

  { make it visible }
  if pcPages.ActivePage <> tsOutput then
    pcPages.ActivePage := tsOutput;
end;

end.
