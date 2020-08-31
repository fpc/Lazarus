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
  SysUtils, Classes,
  // BuildIntf
  ProjectIntf, IDEExternToolIntf,
  // IdeIntf
  LazIDEIntf, SrcEditorIntf, IDEMsgIntf,
  // LCL
  Menus, Dialogs, Controls,
  // local
  EditorConverter, FileConverter, Converter, ConvertTypes,
  JcfUIConsts, JcfStringUtils, JcfSettings, fAbout, frFiles;

type

  { TJcfIdeMain }

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
    function CanFormat(const AMsg: String): Boolean;

  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoFormatSelection(Sender: TObject);
    procedure DoFormatCurrentIDEWindow(Sender: TObject);
    procedure DoFormatProject(Sender: TObject);
    procedure DoFormatOpen(Sender: TObject);
    procedure DoRegistrySettings(Sender: TObject);
    procedure DoFormatSettings(Sender: TObject);
    procedure DoAbout(Sender: TObject);
  end;


implementation

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

function TJcfIdeMain.CanFormat(const AMsg: String): Boolean;
begin
  Result := True;
  if FormattingSettings.ConfirmFormat then
    if MessageDlg(AMsg, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Result := False
end;

procedure TJcfIdeMain.DoFormatCurrentIDEWindow(Sender: TObject);
var
  lsMsg: string;
begin
  if (SourceEditorManagerIntf= nil) or (SourceEditorManagerIntf.ActiveEditor = nil) then
    LogIdeMessage('', 'No current window', mtInputError, -1, -1)
  else begin
    lsMsg := Format(lisJEDICodeFormatOfStartFormatting, [SourceEditorManagerIntf
      .ActiveEditor.FileName
            + NativeLineBreak]);
    if CanFormat(lsMsg) then
      ConvertEditor(SourceEditorManagerIntf.ActiveEditor)
  end;
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
  lsMsg := Format(lisJEDICodeFormatOfAreYouSureThatYouWantToFormatAllFi, [
    lazProject.MainFile.FileName + NativeLineBreak, IntToStr(lazProject.
    FileCount)]);
  if CanFormat(lsMsg) then
  begin
    ClearToolMessages;
    { loop through all modules in the project }
    for liLoop := 0 to lazProject.FileCount - 1 do
    begin
      lazFile := lazProject.Files[liLoop];
      FormatFile(lazFile.FileName);
    end;
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

  if not CanFormat(lisJEDICodeFormatAllOpenWindow) then
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

//offset in bytes of first char of the lines. 1 based.
procedure FindLineOffsets(const aStr: string; aLineStart, aLineEnd: integer;
                      out aLineStartOffset: integer; out aLineEndOffset:integer);
var
  lineCount:integer;
  len:integer;
  pC:PChar;
  offset:integer;
begin
  len:=length(aStr);
  pC:=@aStr[1];
  lineCount:=1;
  offset:=1;
  aLineStartOffset:=0;
  aLineEndOffset:=0;
  if len<1 then
    exit;
  if aLineStart=1 then
    aLineStartOffset:=offset;
  if (aLineEnd=1) then
    aLineEndOffset:=offset;
  while (offset<=len) and (lineCount<=aLineEnd) do
  begin
    while (offset<=len) and (pC^<>#10) do
    begin
      inc(offset);
      inc(pC);
    end;
    if (pC^=#10) and (offset<len) then
    begin
      inc(pC);
      inc(offset);
      inc(lineCount);
      if lineCount=aLineStart then
        aLineStartOffset:=offset;
      if lineCount=aLineEnd then
      begin
        aLineEndOffset:=offset;
        exit;
      end;
    end
    else
      exit;
  end;
end;

procedure TJcfIdeMain.DoFormatSelection(Sender: TObject);
var
  srcEditor: TSourceEditorInterface;

  procedure GetSelectedBlockFullLines(out p1: TPoint; out p2: TPoint);
  begin
    p1 := srcEditor.BlockBegin;
    p2 := srcEditor.BlockEnd;
    if (p1.y > p2.y) then
    begin
      p1 := srcEditor.BlockEnd;
      p2 := srcEditor.BlockBegin;
    end;
    if p2.x<=1 then
    begin
      if p2.y>1 then
        p2.y:=p2.y-1;
    end;
    p2.x:=Length(srcEditor.Lines[p2.y-1])+1;   //last char
    p1.x:=1;
  end;

var
  sourceCode: string;
  BlockBegin, BlockEnd: TPoint;
  fcConverter: TConverter;
  lineStartOffset,lineEndOffset: integer;
  wi: integer;
  outputstr: string;
begin
  if (SourceEditorManagerIntf = nil) or (SourceEditorManagerIntf.ActiveEditor = nil) then
  begin
    LogIdeMessage('', 'No current window', mtInputError, -1, -1);
    exit;
  end;
  srcEditor := SourceEditorManagerIntf.ActiveEditor;
  if (srcEditor.SelectionAvailable=false) or srcEditor.ReadOnly then
    Exit;
  sourceCode := srcEditor.GetText(False);   //get ALL editor text.
  GetSelectedBlockFullLines(BlockBegin,BlockEnd);
  fcConverter := TConverter.Create;
  try
    fcConverter.OnStatusMessage := LogIDEMessage;
    fcConverter.InputCode := sourceCode;
    fcConverter.GuiMessages := true;
    FindLineOffsets(sourceCode,BlockBegin.Y,BlockEnd.Y,lineStartOffset,lineEndOffset);
    fcConverter.ConvertPart(lineStartOffset,lineEndOffset,true);
    wI := length(fcConverter.OutputCode);
    // converter adds 2 LineEndings.
    outputstr := Copy(fcConverter.OutputCode, 1, wI-Length(LineEnding+LineEnding));
    if not fcConverter.ConvertError then
      srcEditor.ReplaceText(BlockBegin, BlockEnd, outputstr);
  finally
    fcConverter.Free;
  end;
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
  Urgency: TMessageLineUrgency;
begin
  { no empty lines in this log }
  if psMessage = '' then
    exit;

  lazMessages := IDEMessagesWindow;
  if lazMessages = nil then
    exit;

  case peMessageType of
    mtException,mtInputError,mtParseError: Urgency:=mluError;
    mtCodeWarning: Urgency:=mluWarning;
    mtFinalSummary: Urgency:=mluImportant;
    mtProgress: Urgency:=mluProgress;
    else Urgency:=mluNone; // Suppress compiler warning.
  end;
  lazMessages.AddCustomMessage(Urgency, psMessage, psFile, piY, piX, 'JCF')
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
