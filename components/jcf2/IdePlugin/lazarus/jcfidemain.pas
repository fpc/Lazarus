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

{$mode delphi}

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
  JcfUIConsts, JcfStringUtils, JcfSettings, fAbout, frFiles,
  JcfRegistrySettings, fRegistrySettings, SettingsTypes;

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
    procedure OnIncludeFile(Sender:TObject;AIncludeFileName:string;var AFileContentOrErrorMessage:string;var AFileReaded:boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoFormatSelection(Sender: TObject);
    procedure DoFormatIncludeFile(Sender: TObject);
    procedure DoFormatCurrentIDEWindow(Sender: TObject);
    procedure DoFormatProject(Sender: TObject);
    procedure DoFormatOpen(Sender: TObject);
    procedure DoRemoveCommentsInCurrentIDEWindow(Sender: TObject);
    procedure DoRegistrySettings(Sender: TObject);
    procedure DoFormatSettings(Sender: TObject);
    procedure DoAbout(Sender: TObject);

    class procedure ShowIdeMessages;
  end;


implementation

uses
  diffmerge, lazfileutils, JcfMiscFunctions;

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
    if SourceEditorManagerIntf.ActiveEditor.SelectionAvailable then
      DoFormatSelection(Sender)
    else
    begin
      lsMsg := Format(lisJEDICodeFormatOfStartFormatting,
                [SourceEditorManagerIntf.ActiveEditor.FileName + NativeLineBreak]);
      if CanFormat(lsMsg) then
      begin
        if FilenameExtIs(SourceEditorManagerIntf.ActiveEditor.FileName,'inc') then
          DoFormatIncludeFile(Sender)
        else
          ConvertEditor(SourceEditorManagerIntf.ActiveEditor);
      end;
    end;
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
  lsMsg := Format(lisJEDICodeFormatOfAreYouSureThatYouWantToFormatAllFi,
    [lazProject.MainFile.FileName + NativeLineBreak, IntToStr(lazProject.FileCount)]);
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

procedure TJcfIdeMain.DoRemoveCommentsInCurrentIDEWindow(Sender: TObject);
var
  lcCaps : TCapitalisationType;
  lbEnabled,lbRemoveComments,lbRemoveWhiteSpace,lbRemoveIndent,lbRebreakLines:boolean;
begin
  try
    lbEnabled := FormattingSettings.Obfuscate.Enabled;
    lcCaps := FormattingSettings.Obfuscate.Caps;
    lbRemoveComments := FormattingSettings.Obfuscate.RemoveComments;
    lbRemoveWhiteSpace := FormattingSettings.Obfuscate.RemoveWhiteSpace;
    lbRemoveIndent := FormattingSettings.Obfuscate.RemoveIndent;
    lbRebreakLines := FormattingSettings.Obfuscate.RebreakLines;

    FormattingSettings.Obfuscate.Enabled := True;
    FormattingSettings.Obfuscate.Caps := ctLeaveAlone;
    FormattingSettings.Obfuscate.RemoveComments := True;
    FormattingSettings.Obfuscate.RemoveWhiteSpace := False;
    FormattingSettings.Obfuscate.RemoveIndent := False;
    FormattingSettings.Obfuscate.RebreakLines := False;

    DoFormatCurrentIDEWindow(Sender);
  finally
    //restore settings.
    FormattingSettings.Obfuscate.Enabled := lbEnabled;
    FormattingSettings.Obfuscate.Caps := lcCaps;
    FormattingSettings.Obfuscate.RemoveComments := lbRemoveComments;
    FormattingSettings.Obfuscate.RemoveWhiteSpace := lbRemoveWhiteSpace;
    FormattingSettings.Obfuscate.RemoveIndent := lbRemoveIndent;
    FormattingSettings.Obfuscate.RebreakLines := lbRebreakLines;
  end;
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
    fcFileConverter.OnIncludeFile := OnIncludeFile;
  end;

  fcFileConverter.ProcessFile(psFileName);
end;

procedure TJcfIdeMain.DoFormatSettings(Sender: TObject);
begin
  // open with the first frame
  LazarusIDE.DoOpenIDEOptions(TfFiles);
end;

procedure TJcfIdeMain.DoFormatSelection(Sender: TObject);
var
  srcEditor: TSourceEditorInterface;

  procedure GetSelectedBlockFullLines(out p1: TPoint; out p2: TPoint);
  begin
    p1 := srcEditor.BlockBegin;
    p2 := srcEditor.BlockEnd;
    if p1.y > p2.y then
    begin
      p1 := srcEditor.BlockEnd;
      p2 := srcEditor.BlockBegin;
    end;
    if (p2.x <= 1) and (p2.y > 1) then
      p2.y := p2.y-1;
  end;

var
  sourceCode: string;
  BlockBegin, BlockEnd: TPoint;
  fcConverter: TConverter;
  lineStartOffset,lineEndOffset: integer;
  wi,EndY: integer;
  outputstr: string;
begin
  if (SourceEditorManagerIntf = nil) or (SourceEditorManagerIntf.ActiveEditor = nil) then
  begin
    LogIdeMessage('', 'No current window', mtInputError, -1, -1);
    exit;
  end;
  srcEditor := SourceEditorManagerIntf.ActiveEditor;
  if not srcEditor.SelectionAvailable or srcEditor.ReadOnly then
    Exit;
  sourceCode := srcEditor.GetText(False);   //get ALL editor text.
  GetSelectedBlockFullLines(BlockBegin,BlockEnd);
  fcConverter := TConverter.Create;
  try
    ClearToolMessages;
    fcConverter.OnStatusMessage := LogIDEMessage;
    fcConverter.InputCode := sourceCode;
    fcConverter.GuiMessages := false; //true;
    fcConverter.FileName := SourceEditorManagerIntf.ActiveEditor.FileName;
    fcConverter.OnIncludeFile := OnIncludeFile;
    FindLineOffsets(sourceCode,BlockBegin.Y,BlockEnd.Y,lineStartOffset,lineEndOffset);
    fcConverter.ConvertPart(lineStartOffset, lineEndOffset, True);
    if not fcConverter.ConvertError then
    begin
      wI := length(fcConverter.OutputCode);
      while (wI > 1) and (fcConverter.OutputCode[wI] in [#10, #13, ' ']) do
        Dec(wI);
      outputstr := Copy(fcConverter.OutputCode, 1, wI);
      DiffMergeEditor(srcEditor,outputstr,BlockBegin.Y,BlockEnd.Y);
    end
    else
    begin    //try formating wrapping selected code in fake unit.
      BlockBegin := srcEditor.BlockBegin;
      BlockBegin.X := 1;     // full lines.
      BlockEnd := srcEditor.BlockEnd;
      EndY:=BlockEnd.Y;
      if BlockEnd.X > 1 then
        BlockEnd.Y := BlockEnd.Y + 1;
      if BlockEnd.X > 1 then
        BlockEnd.Y := BlockEnd.Y + 1
      else
      begin
        if EndY>1 then
          EndY:=EndY-1;
      end;
      BlockEnd.X := 1;
      //Since it is very difficult to predict what JCF will do with the empty lines
      //at the beginning and end of the selection, we do not allow them.

      // skip start empty lines.
      while BlockBegin.Y < EndY do
      begin
        if Trim(srcEditor.Lines[BlockBegin.Y-1])<>'' then
          break;
        Inc(BlockBegin.Y);
      end;
      // skip end empty lines.
      while EndY > BlockBegin.Y do
      begin
        if Trim(srcEditor.Lines[EndY-1])<>'' then
          break;
        Dec(BlockEnd.Y);
        Dec(EndY);
      end;
      srcEditor.SelectText(BlockBegin, BlockEnd); //extend selection to full lines.
      fcConverter.InputCode := srcEditor.GetText(True);  // only selected text.
      fcConverter.GuiMessages := true;
      fcConverter.FileName := SourceEditorManagerIntf.ActiveEditor.FileName;
      fcConverter.OnIncludeFile := OnIncludeFile;
      fcConverter.ConvertUsingFakeUnit(BlockBegin.Y);
      if not fcConverter.ConvertError then
      begin
        outputstr:=StrTrimLastEndOfLine(fcConverter.OutputCode);
        DiffMergeEditor(srcEditor,outputstr,BlockBegin.Y,EndY);
      end;
    end;
  finally
    if fcConverter.ConvertError then
      ShowIdeMessages;
    fcConverter.Free;
  end;
end;

procedure TJcfIdeMain.DoFormatIncludeFile(Sender: TObject);
var
  srcEditor: TSourceEditorInterface;
  sourceCode: string;
  BlockBegin, BlockEnd: TPoint;
  fcConverter: TConverter;
  outputstr: string;
begin
  if (SourceEditorManagerIntf = nil) or (SourceEditorManagerIntf.ActiveEditor = nil) then
  begin
    LogIdeMessage('', 'No current window', mtInputError, -1, -1);
    exit;
  end;
  srcEditor := SourceEditorManagerIntf.ActiveEditor;
  if srcEditor.ReadOnly then
    Exit;
  sourceCode := srcEditor.GetText(False);   //get ALL editor text.
  BlockBegin.X := 1;
  BlockBegin.Y := 1;
  //Find position of the last character in editor.
  BlockEnd := Point(1, srcEditor.Lines.Count);
  if BlockEnd.y > 0 then
    Inc(BlockEnd.x, Length(srcEditor.Lines[BlockEnd.y - 1]))
  else
    BlockEnd.y := 1;
  fcConverter := TConverter.Create;
  try
    fcConverter.OnStatusMessage := LogIDEMessage;
    fcConverter.InputCode := sourceCode;
    //try formating wrapping the code in fake unit.
    ClearToolMessages;
    fcConverter.GuiMessages := True;
    fcConverter.FileName := SourceEditorManagerIntf.ActiveEditor.FileName;
    fcConverter.OnIncludeFile := OnIncludeFile;
    fcConverter.ConvertUsingFakeUnit;
    if not fcConverter.ConvertError then
    begin
      outputstr := StrTrimLastEndOfLine(fcConverter.OutputCode);
      DiffMergeEditor(srcEditor, outputstr, BlockBegin.Y, BlockEnd.Y);
    end;
  finally
    if fcConverter.ConvertError then
      ShowIdeMessages;
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

class procedure TJcfIdeMain.ShowIdeMessages;
begin
  LazarusIDE.DoShowMessagesView({PutOnTop} True);
end;

procedure TJcfIdeMain.DoRegistrySettings(Sender: TObject);
var
  lcAbout: TfmRegistrySettings;
begin
  if not GetRegSettings.HasRead then
    GetRegSettings.ReadAll;

  lcAbout := TfmRegistrySettings.Create(nil);
  try
    lcAbout.Execute;
  finally
    lcAbout.Free;
  end;
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
    fcEditorConverter.OnIncludeFile := OnIncludeFile;
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

procedure TJcfIdeMain.OnIncludeFile(Sender: TObject; AIncludeFileName: string; var AFileContentOrErrorMessage: string; var AFileReaded: boolean);
var
  lsFile: string;
  lsDir: string;
  lbFileFound: boolean;

  function IsOnIdeEditor(AFileName: string): boolean;
  var
    lSEI: TSourceEditorInterface;
  begin
    Result := False;
    lSEI := SourceEditorManagerIntf.SourceEditorIntfWithFilename(AFilename);
    if lSEI <> nil then
    begin
      AFileContentOrErrorMessage := lSEI.GetText(False);
      AFileReaded := True;
      Result := True;
    end;
  end;

begin
  lbFileFound := False;

  if ExtractFilePath(AIncludeFileName) = '' then
  begin
    // seach in the same path as formated unit.
    lsFile := ExtractFilePath(TConverter(Sender).FileName) + AIncludeFileName;
    if IsOnIdeEditor(lsFile) then
      Exit;
    lbFileFound := FileExists(lsFile);
  end
  else
  begin
    if FilenameIsAbsolute(AIncludeFileName) then
    begin
      lsFile := AIncludeFileName;
      if IsOnIdeEditor(lsFile) then
        Exit;
      lbFileFound := FileExists(lsFile);
    end;
  end;
  // search in project dir and project include paths.
  if not lbFileFound then
  begin
    lsDir := LazProject1.Directory;
    lsFile := LazarusIDE.FindSourceFile(AIncludeFileName, lsDir, [fsfSearchForProject, fsfUseIncludePaths]);
    lbFileFound := lsFile <> '';
    if lsFile <> '' then
    begin
      if IsOnIdeEditor(lsFile) then
        Exit;
    end;
  end;
  if lbFileFound then
  begin
    AFileContentOrErrorMessage := ReadFileToUTF8String(lsFile);
    AFileReaded := True;
  end
  else
  begin
    AFileReaded := False;
    AFileContentOrErrorMessage := ' Include file not found: ' + AIncludeFileName;
  end;
end;

end.
