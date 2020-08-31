{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SetAnyWordCaps.pas, released April 2000.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2000 Anthony Steele.
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

unit SetAnyWordCaps;

{ settings to do capitalisation
  AFS 29 Dec 1999
}

{$I JcfGlobal.inc}

interface

uses
  Classes, SysUtils,
  Dialogs,
  { local }
  JcfSetBase, SettingsStream;

type

  TSetAnyWordCaps = class(TSetBase)
  private
    fbEnabled: boolean;
    fcWords: TStringList;

  protected
  public
    constructor Create;
    destructor Destroy; override;

    function FixWord(const psWord: string): string;
    function HasWord(const psWord: string): boolean;

    procedure WriteToStream(const pcOut: TSettingsOutput); override;
    procedure ReadFromStream(const pcStream: TSettingsInput); override;

    property Enabled: boolean Read fbEnabled Write fbEnabled;
    property Words: TStringList Read fcWords;

  end;

implementation

const
  REG_ENABLED = 'Enabled';
  REG_WORDS   = 'Words';

  { The reserved-word capitalisation makes true & false lower case by default
  I like them to have capitals (unlike nil)
  this is mixed bag of words
  }

const
  DefaultWordsWithCaps: array [0..119] of string =
    (// constants
    'True', 'False',
    'Unassigned',
    // built-in types with more than one letter capitalised
    'WideString', 'TObject',
    // well-known types defined in units that ship with Delphi
    'PChar', 'TNotifyEvent',
    // well-known Com types
    'IUnknown', 'IDispatch',
    // well-known object types
    'TComponent', 'TAutoObject', 'Exception',
    'TForm', 'TDataModule', 'TFrame',
    'TList', 'TStringList', 'TStrings', 'TObjectList',
    'TPersistent', 'TButton', 'TPageControl', 'TTabSheet',
    // well-known functions & procs
    'StrToInt', 'IntToStr', 'FieldByName',
    'AnsiCompareText', 'AnsiCompareStr', 'ShowMessage',
    'AnsiUpperCase', 'ParamStr',
    'Register', // known to cause errors if this capitalisation is not used
    'Free', 'Create', 'Destroy', 'Initialize',
    'Delete', 'Show', 'SetFocus', 'Assign',
    'Release', 'Read', 'Write',
    'Open', 'Close',
    'ProcessMessages', 'Execute',
    'Lines', 'Sender',
    'FreeAndNil', 'Clear', 'Perform',
    { well-known property names }
    'ClassName', 'Name', 'ReadOnly',
    'AsString', 'AsInteger', 'AsBoolean',
    'AsDateTime', 'AsFloat', 'AsVariant',
    'MinValue', 'MaxValue',
    'Text', 'Caption', 'Data',
    'RecordCount',
    'First', 'Next', 'EOF',
    'GetFirstChild', 'EndDrag', 'BeginDrag',
    'Width', 'Height',
    'Visible', 'Enabled', 'Checked', 'Value',
    'ItemIndex', 'Result',
    'Ord', 'Components', 'Count',
    'Source', 'ActivePage',
    // modal results etc.
    'mrNone', 'mrOk',
    'idOk', 'mrCancel', 'idCancel',
    'mrAbort', 'idAbort', 'mrRetry', 'idRetry',
    'mrIgnore', 'idIgnore', 'mrYes', 'idYes',
    'mrNo', 'idNo', 'mrAll', 'mrNoToAll', 'mrYesToAll',
    'mtWarning', 'mtError', 'mtInformation', 'mtConfirmation', 'mtCustom',
    'mbYes', 'mbNo', 'mbOK', 'mbCancel', 'mbAbort', 'mbRetry', 'mbIgnore',
    'mbAll', 'mnNoToAll', 'mbYesToAll', 'mbHelp'
    );

  { TSetAnyWordCaps }

constructor TSetAnyWordCaps.Create;
begin
  inherited;
  SetSection('SpecificWordCaps');

  fcWords := TStringList.Create;
  fcWords.Duplicates := dupIgnore;
end;

destructor TSetAnyWordCaps.Destroy;
begin
  FreeAndNil(fcWords);
  inherited;
end;

procedure TSetAnyWordCaps.ReadFromStream(const pcStream: TSettingsInput);
var
  liLoop:    integer;
  lsNewWord: string;
begin
  Assert(pcStream <> nil);

  fbEnabled := pcStream.Read(REG_ENABLED, True);

  if not pcStream.Read(REG_WORDS, fcWords) then
  begin
    for liLoop := Low(DefaultWordsWithCaps) to High(DefaultWordsWithCaps) do
    begin
      lsNewWord := DefaultWordsWithCaps[liLoop];
      if fcWords.IndexOf(lsNewWord) >= 0 then
        ShowMessage('TSetAnyWordCaps.Read: duplicate default word: ' + lsNewWord)
      else
        fcWords.Add(lsNewWord);
    end;
  end;

  fcWords.Sort;
end;

procedure TSetAnyWordCaps.WriteToStream(const pcOut: TSettingsOutput);
begin
  Assert(pcOut <> nil);

  pcOut.Write(REG_ENABLED, fbEnabled);
  pcOut.Write(REG_WORDS, fcWords);
end;

function TSetAnyWordCaps.FixWord(const psWord: string): string;
var
  liLoop: integer;
begin
  Result := psWord;

  for liLoop := 0 to fcWords.Count - 1 do
  begin
    if AnsiCompareText(psWord, fcWords.Strings[liLoop]) = 0 then
      Result := fcWords.Strings[liLoop]; // this sets it to the right case
  end;
end;

function TSetAnyWordCaps.HasWord(const psWord: string): boolean;
begin
  if psWord = '' then
  begin
    Result := False;
    exit;
  end;

  Result := (fcWords.IndexOf(psWord) > -1);
end;

end.
