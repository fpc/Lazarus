unit JcfIdeMain;

{ AFS 7 Jan 2K
  JEDI Code Format IDE plugin main class

  global object that implements the callbacks from the menu items }


{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is JcfIdeMain, released May 2003.
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
  { freepascal }SysUtils, Classes,
  { lazarus design time }
  LazIDEIntf, SrcEditorIntf, IDEMsgIntf, ProjectIntf,
  { local}
  EditorConverter, FileConverter, ConvertTypes;

type
  TJcfIdeMain = class(TObject)
  private
    fcEditorConverter: TEditorConverter;
    fcFileConverter: TFileConverter;

    procedure MakeEditorConverter;

    procedure LogIDEMessage(const psFile, psMessage: string;
      const peMessageType: TStatusMessageType;
      const piY, piX: integer);
    procedure FormatFile(const psFileName: string);

    procedure ClearToolMessages;
    procedure ConvertEditor(const pciEditor: TSourceEditorInterface);


  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoFormatCurrentIDEWindow(Sender: TObject);
    procedure DoFormatProject(Sender: TObject);
    procedure DoFormatOpen(Sender: TObject);
    procedure DoRegistrySettings(Sender: TObject);
    procedure DoFormatSettings(Sender: TObject);
    procedure DoAbout(Sender: TObject);
  end;


implementation

uses
  { lazarus }
  Menus, Dialogs, Controls,
  { jcf }
  JcfStringUtils,
  { local }
  fAbout, frFiles {, JcfRegistrySettings, fRegistrySettings};


function FileIsAllowedType(const psFileName: string): boolean;
const
  ALLOWED_FILE_TYPES: array[1..5] of string = ('.pas', '.pp', '.dpr', '.lpr', '.dpk');
begin
  Result := StrIsOneOf(StrRight(psFileName, 4), ALLOWED_FILE_TYPES);
end;


function GetCurrentProject: TLazProject;
begin
  Result := LazarusIDE.ActiveProject;
end;

constructor TJcfIdeMain.Create;
begin
  inherited;
  { both of these are created on demand }
  fcEditorConverter := nil;
  fcFileConverter   := nil;
end;

destructor TJcfIdeMain.Destroy;
begin
  FreeAndNil(fcEditorConverter);
  FreeAndNil(fcFileConverter);
  inherited;
end;

procedure TJcfIdeMain.DoFormatCurrentIDEWindow(Sender: TObject);
begin
  if (SourceEditorManagerIntf= nil) or (SourceEditorManagerIntf.ActiveEditor = nil) then
  begin
    LogIdeMessage('', 'No current window', mtInputError, -1, -1);
    exit;
  end;

  ConvertEditor(SourceEditorManagerIntf.ActiveEditor);
end;

procedure TJcfIdeMain.ConvertEditor(const pciEditor: TSourceEditorInterface);
begin
  MakeEditorConverter;

  ClearToolMessages;
  fcEditorConverter.Clear;
  fcEditorConverter.BeforeConvert;
  fcEditorConverter.Convert(pciEditor);
  fcEditorConverter.AfterConvert;
end;

procedure TJcfIdeMain.DoFormatProject(Sender: TObject);
var
  lazProject: TLazProject;
  lazFile: TLazProjectFile;
  liLoop: integer;
  lsMsg: string;
begin
  lazProject := GetCurrentProject;
  if lazProject = nil then
    exit;

  lsMsg := 'JEDI Code Format of ' + lazProject.MainFile.FileName + NativeLineBreak +
    'Are you sure that you want to format all ' + IntToStr(lazProject.FileCount) +
    ' files in the project.';

  if MessageDlg(lsMsg, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    exit;

  ClearToolMessages;

  { loop through all modules in the project }
  for liLoop := 0 to lazProject.FileCount - 1 do
  begin
    lazFile := lazProject.Files[liLoop];
    FormatFile(lazFile.FileName);
  end;
end;

procedure TJcfIdeMain.DoFormatOpen(Sender: TObject);
var
  lciEditor: TSourceEditorInterface;
  liLoop: integer;
begin
  MakeEditorConverter;

  if (SourceEditorManagerIntf = nil) then
    Exit;

  ClearToolMessages;
  fcEditorConverter.BeforeConvert;

  for liLoop := 0 to SourceEditorManagerIntf.UniqueSourceEditorCount - 1 do
  begin
    lciEditor := SourceEditorManagerIntf.UniqueSourceEditors[liLoop];

    // check that it's open, and a .pas or .dpr
    if (lciEditor <> nil) and (FileIsAllowedType(lciEditor.FileName)) then
    begin
      fcEditorConverter.Convert(lciEditor);
    end;
  end;

  fcEditorConverter.AfterConvert;
end;


procedure TJcfIdeMain.FormatFile(const psFileName: string);
begin
  if not FileExists(psFileName) then
    exit;

  // check that it's a .pas or .dpr
  if not FileIsAllowedType(psFileName) then
    exit;

  if fcFileConverter = nil then
  begin
    fcFileConverter := TFileConverter.Create;
    fcFileConverter.OnStatusMessage := LogIDEMessage;
  end;

  fcFileConverter.ProcessFile(psFileName);
end;

procedure TJcfIdeMain.DoFormatSettings(Sender: TObject);
begin
  // open with the first frame
  LazarusIDE.DoOpenIDEOptions(TfFiles);
end;

procedure TJcfIdeMain.DoAbout(Sender: TObject);
var
  lcAbout: TfrmAboutBox;
begin
  lcAbout := TfrmAboutBox.Create(nil);
  try
    lcAbout.ShowModal;
  finally
    lcAbout.Free;
  end;
end;

procedure TJcfIdeMain.DoRegistrySettings(Sender: TObject);
{var
  lcAbout: TfmRegistrySettings;
}
begin
  ShowMessage('unimplemented');
{ TODO: convert JCF registry settings (it contains some TJvXXX components atm)
  if not GetRegSettings.HasRead then
    GetRegSettings.ReadAll;

  lcAbout := TfmRegistrySettings.Create(nil);
  try
    lcAbout.Execute;
  finally
    lcAbout.Free;
  end;
}
end;

procedure TJcfIdeMain.LogIDEMessage(const psFile, psMessage: string;
  const peMessageType: TStatusMessageType;
  const piY, piX: integer);
var
  lazMessages: TIDEMessagesWindowInterface;
begin
  { no empty lines in this log }
  if psMessage = '' then
    exit;

  lazMessages := IDEMessagesWindow;
  if lazMessages = nil then
    exit;

  if (piY >= 0) and (piX >= 0) then
    lazMessages.AddMsg('JCF: ' + psMessage, psFile, 0)
    //lazMessages.AddToolMessage(psFile, psMessage, 'JCF', piY, piX)
  else
    lazMessages.AddMsg('JCF: ' + psFile + ' ' + psMessage, '', 0);
    //lazMessages.AddTitleMessage('JCF: ' + psFile + ' ' + psMessage);
end;

procedure TJcfIdeMain.MakeEditorConverter;
begin
  if fcEditorConverter = nil then
  begin
    fcEditorConverter := TEditorConverter.Create;
    fcEditorConverter.OnStatusMessage := LogIDEMessage;
  end;

  Assert(fcEditorConverter <> nil);
end;

procedure TJcfIdeMain.ClearToolMessages;
var
  lazMessages: TIDEMessagesWindowInterface;
begin
  lazMessages := IDEMessagesWindow;
  if lazMessages = nil then
    exit;

  lazMessages.Clear;
end;

end.
