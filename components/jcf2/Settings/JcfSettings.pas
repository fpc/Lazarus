{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is Settings.pas, released April 2000.
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

unit JcfSettings;

{ this is the settings on how to parse. As of 2.0 this is always from a file
  The file name is stored in registry
  This allows centralised settings on a shared dir }

{$I JcfGlobal.inc}

interface

uses
  { local }
  SetObfuscate, SetClarify,
  SetIndent, SetSpaces, SetReturns,
  SetComments, SetCaps, SetWordList,
  SetAlign, SetReplace, SetUses, SetPreProcessor,
  SettingsStream, SetTransform,  SetAsm,
  JcfVersionConsts, IDEOptionsIntf, IDEOptEditorIntf;

type

  { TFormattingSettings }

  TFormattingSettings = class(TAbstractIDEEnvironmentOptions)
  private
    fcObfuscate: TSetObfuscate;
    fcClarify: TSetClarify;
    fcSpaces: TSetSpaces;
    fcIndent: TSetIndent;
    fcReturns: TSetReturns;
    fcComments: TSetComments;
 
    fcCaps: TSetCaps;
    fcSpecificWordCaps: TSetWordList;
    fcIdentifierCaps: TSetWordList;
    fcNotIdentifierCaps: TSetWordList;
    fcUnitNameCaps: TSetWordList;

    fcSetAsm: TSetAsm;

    fcPreProcessor: TSetPreProcessor;
    fcAlign: TSetAlign;
    fcUses: TSetUses;

    fcReplace: TSetReplace;

    fcTransform: TSetTransform;

    fbWriteOnExit: boolean;
    fbHasRead: boolean;
    fbDirty: boolean;

    fsDescription: string;
    fdtWriteDateTime: TDateTime;
    fsWriteVersion: string;
    fsConfirmFormat: Boolean;

    procedure FromStream(const pcStream: TSettingsInput);
  public
    constructor Create(const pbReadRegFile: boolean);
    destructor Destroy; override;
    class function GetGroupCaption: String; override;
    class function GetInstance: TAbstractIDEOptions; override;
    procedure DoAfterWrite({%H-}Restore: boolean); override;

    procedure Read;
    procedure ReadFromFile(const psFileName: string; const pbMustExist: boolean);
    procedure ReadDefaults;
    procedure Write;

    procedure MakeConsistent;

    procedure ToStream(const pcStream: TSettingsOutput);

    property Description: string Read fsDescription Write fsDescription;
    property WriteDateTime: TDateTime Read fdtWriteDateTime Write fdtWriteDateTime;
    property WriteVersion: string Read fsWriteVersion Write fsWriteVersion;

    property Obfuscate: TSetObfuscate Read fcObfuscate;
    property Clarify: TSetClarify Read fcClarify;
    property Indent: TSetIndent Read fcIndent;
    property Spaces: TSetSpaces Read fcSpaces;
    property Returns: TSetReturns Read fcReturns;
    property Comments: TSetComments Read fcComments;

    property Caps: TSetCaps Read fcCaps;
    property SpecificWordCaps: TSetWordList Read fcSpecificWordCaps;
    property IdentifierCaps: TSetWordList Read fcIdentifierCaps;
    property NotIdentifierCaps: TSetWordList Read fcNotIdentifierCaps;
    property UnitNameCaps: TSetWordList Read fcUnitNameCaps;
    property SetAsm: TSetAsm Read fcSetAsm;

    property PreProcessor: TSetPreProcessor Read fcPreProcessor;

    property Align: TSetAlign Read fcAlign;
    property Replace: TSetReplace Read fcReplace;
    property UsesClause: TSetUses Read fcUses;

    property Transform: TSetTransform read fcTransform;
 
    property WriteOnExit: boolean Read fbWriteOnExit Write fbWriteOnExit;
    property Dirty: boolean Read fbDirty Write fbDirty;
    property HasRead: boolean read fbHasRead write fbHasRead;
    property ConfirmFormat: boolean read fsConfirmFormat write fsConfirmFormat;
  end;

function FormattingSettings: TFormattingSettings;

// create from a settings file
function FormatSettingsFromFile(const psFileName: string): TFormattingSettings;

var
  JCFOptionsGroup: Integer;
const
  JCFOptionFormatFile = 1;
  JCFOptionObfuscate = 2;
  JCFOptionClarify = 3;
  JCFOptionSpaces = 4;
  JCFOptionIndentation = 5;
  JCFOptionBlankLines = 6;
  JCFOptionAlign = 7;
  JCFOptionLongLines = 8;
  JCFOptionReturns = 9;
  JCFOptionCaseBlocks = 10;
  JCFOptionBlocks = 11;
  JCFOptionCompilerDirectives = 12;
  JCFOptionComments = 13;
  JCFOptionWarnings = 14;
  JCFOptionObjectPascal = 15;
  JCFOptionAnyWord = 16;
  JCFOptionIdentifiers = 17;
  JCFOptionNotIdentifiers = 18;
  JCFOptionUnitName = 19;
  JCFOptionFindAndReplace = 20;
  JCFOptionUses = 21;
  JCFOptionBasic = 22;
  JCFOptionTransform = 23;
  JCFOptionAsm = 24;
  JCFOptionPreProcessor = 25;

const
  GUI_PAD = 3;

implementation

uses
  { delphi }
  {$IFNDEF FPC}Windows,{$ELSE}LazFileUtils, LazUTF8,{$ENDIF} SysUtils, Dialogs,
  { local }
  JcfStringUtils,
  JcfSetBase,
  JcfRegistrySettings,
  jcfuiconsts;


constructor TFormattingSettings.Create(const pbReadRegFile: boolean);
begin
  inherited Create();

  fcObfuscate := TSetObfuscate.Create;
  fcClarify   := TSetClarify.Create;
  fcIndent    := TSetIndent.Create;
  fcSpaces    := TSetSpaces.Create;
  fcReturns   := TSetReturns.Create;

  fcComments := TSetComments.Create;

  fcCaps := TSetCaps.Create;
  fcSpecificWordCaps := TSetWordList.Create('SpecificWordCaps');
  fcIdentifierCaps := TSetWordList.Create('Identifiers');
  fcNotIdentifierCaps := TSetWordList.Create('NotIdent');
  fcUnitNameCaps := TSetWordList.Create('UnitNameCaps');

  fcSetAsm := TSetAsm.Create();

  fcPreProcessor := TSetPreProcessor.Create;

  fcAlign   := TSetAlign.Create;
  fcReplace := TSetReplace.Create;
  fcUses    := TSetUses.Create;
  fcTransform := TSetTransform.Create;

  if pbReadRegFile then
  begin
    Read;
  end;

  fbWriteOnExit := True;
  fbDirty := False;
end;

destructor TFormattingSettings.Destroy;
begin
  if WriteOnExit then
    Write;

  FreeAndNil(fcObfuscate);
  FreeAndNil(fcClarify);
  FreeAndNil(fcIndent);
  FreeAndNil(fcSpaces);
  FreeAndNil(fcReturns);
  FreeAndNil(fcComments);

  FreeAndNil(fcCaps);
  FreeAndNil(fcSpecificWordCaps);
  FreeAndNil(fcIdentifierCaps);
  FreeAndNil(fcNotIdentifierCaps);
  FreeAndNil(fcUnitNameCaps);
  FreeAndNil(fcSetAsm);

  FreeAndNil(fcPreProcessor);

  FreeAndNil(fcReplace);
  FreeAndNil(fcAlign);
  FreeAndNil(fcUses);
  FreeAndNil(fcTransform);
  
  inherited;
end;

class function TFormattingSettings.GetGroupCaption: String;
begin
  Result := lisJCFFormatSettings;
end;

class function TFormattingSettings.GetInstance: TAbstractIDEOptions;
begin
  Result := FormattingSettings;
end;

procedure TFormattingSettings.DoAfterWrite(Restore: boolean);
begin
  { settings are now in need of saving }
  Dirty := True;
  { check consistency of settings }
  MakeConsistent;
  { save to file }
  Write;
end;

const
  CODEFORMAT_SETTINGS_SECTION = 'JediCodeFormatSettings';

  REG_VERSION     = 'WriteVersion';
  REG_WRITE_DATETIME = 'WriteDateTime';
  REG_DESCRIPTION = 'Description';
  REG_CONFIRM_FORMAT = 'ConfirmFormat';

procedure TFormattingSettings.Read;
var
  lcReg: TJCFRegistrySettings;
begin
  // use the Settings File if it exists
  lcReg := GetRegSettings;
  ReadFromFile(lcReg.FormatConfigFileName, lcReg.FormatConfigNameSpecified);
end;

procedure TFormattingSettings.ReadFromFile(const psFileName: string; const pbMustExist: boolean);
var
  lsText: string;
  lcFile: TSettingsInputString;
begin
  if {$ifdef FPC}FileExistsUTF8(psFileName){$else}FileExists(psFileName){$endif} then
  begin
    // debug ShowMessage('Reading settings from file ' + lsSettingsFileName);

    // now we know the file exists - try get settings from it
    {$ifdef FPC}
    lsText := string(FileToString(UTF8ToSys(psFileName)));
    {$else}
    lsText := string(FileToString(psFileName));
    {$endif}
    lcFile := TSettingsInputString.Create(lsText);
    try
      FromStream(lcFile);
    finally
      lcFile.Free;
    end;
  end
  else
  begin
    if pbMustExist then
    begin
      MessageDlg(Format(lisTheSettingsFileDoesNotExist, [psFileName, NativeLineBreak]),
        mtError, [mbOK], 0);
      end;
  end;
end;


procedure TFormattingSettings.ReadDefaults;
var
  lcSetDummy: TSettingsInputDummy;
begin
  lcSetDummy := TSettingsInputDummy.Create;
  try
    FromStream(lcSetDummy);
  finally
    lcSetDummy.Free;
  end;
end;

procedure TFormattingSettings.Write;
var
  lcReg: TJCFRegistrySettings;
  lcFile: TSettingsStreamOutput;
begin
   if not Dirty then
    exit;

  { user may have specified no-write }
  lcReg := GetRegSettings;
  if lcReg.FormatFileWriteOption = eNeverWrite then
    exit;

  if lcReg.FormatConfigFileName = '' then
    exit;

  {$ifdef FPC}
  if FileExistsUTF8(lcReg.FormatConfigFileName) and FileIsReadOnlyUTF8(lcReg.FormatConfigFileName) then
  {$else}
  if FileExists(lcReg.FormatConfigFileName) and FileIsReadOnly(lcReg.FormatConfigFileName) then
  {$endif}
  begin
    { fail quietly? }
    if lcReg.FormatFileWriteOption = eAlwaysWrite then
      MessageDlg(Format(lisErrorWritingSettingsFileReadOnly, [lcReg.FormatConfigFileName]), mtError, [mbOK], 0);
    exit;
  end;

  try
    // use the Settings file name
    {$ifdef FPC}
    lcFile := TSettingsStreamOutput.Create(UTF8ToSys(GetRegSettings.FormatConfigFileName));
    {$else}
    lcFile := TSettingsStreamOutput.Create(GetRegSettings.FormatConfigFileName);
    {$endif}
    try
      ToStream(lcFile);

      // not dirty any more
      fbDirty := False;
    finally
      lcFile.Free;
    end;
  except
    on e: Exception do
    begin
      if lcReg.FormatFileWriteOption = eAlwaysWrite then
      begin
        MessageDlg(Format(lisErrorWritingSettingsException, [GetRegSettings.FormatConfigFileName, NativeLineBreak, E.Message]),
          mtError, [mbOK], 0);
      end;
    end;
  end;
end;


procedure TFormattingSettings.ToStream(const pcStream: TSettingsOutput);

  procedure WriteToStream(const pcSet: TSetBase);
  begin
    Assert(pcSet <> nil);
    pcStream.OpenSection(pcSet.Section);
    pcSet.WriteToStream(pcStream);
    pcStream.CloseSection(pcSet.Section);
  end;

begin
  Assert(pcStream <> nil);
  pcStream.WriteXMLHeader;

  pcStream.OpenSection(CODEFORMAT_SETTINGS_SECTION);

  pcStream.Write(REG_VERSION, PROGRAM_VERSION);
  pcStream.Write(REG_WRITE_DATETIME, Now);
  pcStream.Write(REG_DESCRIPTION, Description);
  pcStream.Write(REG_CONFIRM_FORMAT, fsConfirmFormat);

  WriteToStream(fcObfuscate);
  WriteToStream(fcClarify);
  WriteToStream(fcIndent);
  WriteToStream(fcSpaces);
  WriteToStream(fcReturns);
  WriteToStream(fcComments);

  WriteToStream(fcCaps);
  WriteToStream(fcSpecificWordCaps);
  WriteToStream(fcIdentifierCaps);
  WriteToStream(fcNotIdentifierCaps);
  WriteToStream(fcUnitNameCaps);
  WriteToStream(fcSetAsm);

  WriteToStream(fcPreProcessor);
  WriteToStream(fcAlign);
  WriteToStream(fcReplace);
  WriteToStream(fcUses);
  WriteToStream(fcTransform);

  pcStream.CloseSection(CODEFORMAT_SETTINGS_SECTION);
end;

procedure TFormattingSettings.FromStream(const pcStream: TSettingsInput);
var
  lcAllSettings: TSettingsInput;

  procedure ReadFromStream(const pcSet: TSetBase);
  var
    lcSection: TSettingsInput;
  begin
    Assert(pcSet <> nil);

    lcSection := lcAllSettings.ExtractSection(pcSet.Section);
    if lcSection <> nil then
    begin
      pcSet.ReadFromStream(lcSection);
      lcSection.Free;
    end
    else
    begin
      lcSection :=  TSettingsInputDummy.Create;
      try
        pcSet.ReadFromStream(lcSection);
      finally
        lcSection.Free;
      end;
      //ShowMessage('Skipping section ' + pcSet.Section + ' as it was not found');
    end;
  end;

begin

  { basic test - we are only interested in the
    <JediCodeFormaTFormatSettings> ... </JediCodeFormaTFormatSettings> part of the file
    If this start & end is not present, then is is the wrong file }
  lcAllSettings := pcStream.ExtractSection(CODEFORMAT_SETTINGS_SECTION);
  if lcAllSettings = nil then
  begin
    ShowMessage(lisNoSettingsFound);
    exit;
  end;

  try
    fsWriteVersion   := pcStream.Read(REG_VERSION, '');
    fsDescription    := pcStream.Read(REG_DESCRIPTION, '');
    fsConfirmFormat  := pcStream.Read(REG_CONFIRM_FORMAT, True);
    fdtWriteDateTime := pcStream.Read(REG_WRITE_DATETIME, 0.0);

    ReadFromStream(fcObfuscate);
    ReadFromStream(fcClarify);
    ReadFromStream(fcIndent);
    ReadFromStream(fcSpaces);
    ReadFromStream(fcReturns);
    ReadFromStream(fcComments);
    ReadFromStream(fcCaps);
    ReadFromStream(fcSpecificWordCaps);
    ReadFromStream(fcIdentifierCaps);
    ReadFromStream(fcNotIdentifierCaps);
    ReadFromStream(fcUnitNameCaps);
    ReadFromStream(fcSetAsm);

    ReadFromStream(fcPreProcessor);

    ReadFromStream(fcAlign);
    ReadFromStream(fcReplace);
    ReadFromStream(fcUses);
    ReadFromStream(fcTransform);

    fbHasRead := True;
  finally
    lcAllSettings.Free;
  end;
end;


var
  // a module var
  mcFormattingSettings: TFormattingSettings = nil;

function FormattingSettings: TFormattingSettings;
begin
  if mcFormattingSettings = nil then
    mcFormattingSettings := TFormattingSettings.Create(true);

  Result := mcFormattingSettings;
end;

function FormatSettingsFromFile(const psFileName: string): TFormattingSettings;
begin
  if mcFormattingSettings = nil then
    mcFormattingSettings := TFormattingSettings.Create(false);

  mcFormattingSettings.ReadFromFile(psFileName, true);
  Result := mcFormattingSettings;
end;


procedure TFormattingSettings.MakeConsistent;
begin
  { one consistency check so far
    - if linebreaking is off, then "remove returns in expressions" must also be off }

  if Returns.RebreakLines = rbOff then
    Returns.RemoveExpressionReturns := False;
end;

initialization
  JCFOptionsGroup := GetFreeIDEOptionsGroupIndex(GroupEditor);
  RegisterIDEOptionsGroup(JCFOptionsGroup, TFormattingSettings);
finalization
  FreeAndNil(mcFormattingSettings);
end.
