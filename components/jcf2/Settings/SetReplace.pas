{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SetReplace.pas, released April 2000.
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

unit SetReplace;

{ settings for finding and replacing text
  AFS 17 Jan 2K
}

{$I JcfGlobal.inc}

interface

uses
  { delphi }
  Classes,
  { local }
  JcfSetBase, SettingsStream;

type

  TSetReplace = class(TSetBase)
  private
    fbEnabled: boolean;
    fcWords: TStringList;

    fcLeftWords, fcRightWords: TStringList;

    procedure SplitWord(const ps: string; out psOut1, psOut2: string);

  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure WriteToStream(const pcOut: TSettingsOutput); override;
    procedure ReadFromStream(const pcStream: TSettingsInput); override;

    function Replace(const psWord: string): string;
    function HasWord(const psWord: string): boolean;
    procedure SplitWords;

    property Enabled: boolean Read fbEnabled Write fbEnabled;
    property Words: TStringList Read fcWords;

  end;

implementation

uses
  { delphi }
  SysUtils,
  { locals }
  JcfStringUtils;

const
  REG_ENABLED = 'Enabled';
  REG_WORDS   = 'Words';

  { TSetReplace }

constructor TSetReplace.Create;
begin
  inherited;
  SetSection('Replace');
  fcWords := TStringList.Create;

  fcLeftWords  := TStringList.Create;
  fcRightWords := TStringList.Create;
end;

destructor TSetReplace.Destroy;
begin
  FreeAndNil(fcWords);
  FreeAndNil(fcLeftWords);
  FreeAndNil(fcRightWords);
  inherited;
end;


procedure TSetReplace.ReadFromStream(const pcStream: TSettingsInput);
begin
  Assert(pcStream <> nil);

  fbEnabled := pcStream.Read(REG_ENABLED, False);
  pcStream.Read(REG_WORDS, fcWords);

  SplitWords;
end;

procedure TSetReplace.WriteToStream(const pcOut: TSettingsOutput);
begin
  Assert(pcOut <> nil);

  pcOut.Write(REG_ENABLED, fbEnabled);
  pcOut.Write(REG_WORDS, fcWords);
end;

procedure TSetReplace.SplitWord(const ps: string; out psOut1, psOut2: string);
var
  liPos: integer;
begin
  psOut1 := '';
  psOut2 := '';

  liPos := Pos(';', ps);
  if liPos > 0 then
  begin
    psOut1 := Trim(StrLeft(ps, liPos - 1));
    psOut2 := Trim(StrRestOf(ps, liPos + 1));
  end;
end;

{ split the input into left & right words - the find & replace parts }
procedure TSetReplace.SplitWords;
var
  liLoop:   integer;
  ls1, ls2: string;
begin
  fcLeftWords.Clear;
  fcRightWords.Clear;

  for liLoop := 0 to fcWords.Count - 1 do
  begin
    SplitWord(fcWords.Strings[liLoop], ls1, ls2);
    if (ls1 <> '') and (ls2 <> '') then
    begin
      fcLeftWords.Add(ls1);
      fcRightWords.Add(ls2);
    end;
  end;
end;

function TSetReplace.Replace(const psWord: string): string;
var
  liIndex: integer;
begin
  Result := psWord;

  if fcLeftWords.Count = 0 then
    exit;

  { replace left with right - it is case-insensitive }
  liIndex := fcLeftWords.IndexOf(psWord);
  if liIndex >= 0 then
    Result := fcRightWords[liIndex];
end;

function TSetReplace.HasWord(const psWord: string): boolean;
begin
  Result := (fcLeftWords.IndexOf(psWord) > -1);
end;

end.
