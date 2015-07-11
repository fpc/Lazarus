{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SetWordList.pas, released June 2003.
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

unit SetWordList;

{
  This is a base class that abstrects the SetAnyWordCaps class
  Stores a generic word list
}

{$I JcfGlobal.inc}

interface

uses
    { delphi }Classes,
    { local }JcfSetBase, SettingsStream;

type

  TSetWordList = class(TSetBase)
  private
    fbEnabled: boolean;
    fcWords: TStringList;

  protected
    procedure AddDefaultWords; virtual;

  public
    constructor Create(const psSectionName: string);
    destructor Destroy; override;

    procedure Clear;
    procedure Add(const psWord: string);

    function HasWord(const psWord: string): boolean;
    function IndexOfWord(const psWord: string): integer;
    function CapitaliseWord(const psWord: string): string;

    procedure WriteToStream(const pcOut: TSettingsOutput); override;
    procedure ReadFromStream(const pcStream: TSettingsInput); override;

    property Enabled: boolean Read fbEnabled Write fbEnabled;
    property Words: TStringList Read fcWords;

  end;

implementation

uses
  { delphi }
  {$IFNDEF FPC}Windows,{$ENDIF} SysUtils;

const
  REG_ENABLED = 'Enabled';
  REG_WORDS   = 'Words';

{ TSetWordList }
constructor TSetWordList.Create(const psSectionName: string);
begin
  inherited Create;

  Assert(psSectionName <> '');

  SetSection(psSectionName);

  fcWords := TStringList.Create;
  fcWords.Sorted := True;
  fcWords.Duplicates := dupIgnore;
end;

destructor TSetWordList.Destroy;
begin
  FreeAndNil(fcWords);
  inherited;
end;

procedure TSetWordList.ReadFromStream(const pcStream: TSettingsInput);
begin
  Assert(pcStream <> nil);
  fcWords.Sorted := False;

  fbEnabled := pcStream.Read(REG_ENABLED, True);

  if not pcStream.Read(REG_WORDS, fcWords) then
    AddDefaultWords;

  fcWords.Sort;
  fcWords.Sorted := True;
end;

procedure TSetWordList.WriteToStream(const pcOut: TSettingsOutput);
begin
  Assert(pcOut <> nil);

  pcOut.Write(REG_ENABLED, fbEnabled);
  pcOut.Write(REG_WORDS, fcWords);
end;

function TSetWordList.HasWord(const psWord: string): boolean;
begin
  if psWord = '' then
    Result := False
  else
    Result := (IndexOfWord(psWord) > -1);
end;

function TSetWordList.IndexOfWord(const psWord: string): integer;
begin
  if psWord = '' then
    Result := -1
  else
    Result := fcWords.IndexOf(psWord);
end;

function TSetWordList.CapitaliseWord(const psWord: string): string;
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

procedure TSetWordList.AddDefaultWords;
begin
  // do nothing, here for overide
end;


procedure TSetWordList.Add(const psWord: string);
begin
  fcWords.Add(psWord);
end;

procedure TSetWordList.Clear;
begin
  fcWords.Clear;
end;

end.
