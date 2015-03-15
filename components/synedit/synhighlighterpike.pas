{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterJava.pas, released 2000-04-10.
The Original Code is based on the DcjSynJava.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Trier.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: synhighlighterjava.pas 46388 2014-09-30 23:57:55Z martin $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Pike highlighter for SynEdit)
@author(Felipe Monteiro de Carvalho)
@created(March 2015)
@lastmod(2015-03-14)
The SynHighlighterPike unit provides SynEdit with a Pike source (.pike) highlighter.
}
unit synhighlighterpike;

{$I SynEdit.inc}

interface

uses
  SysUtils, Classes,
  LCLIntf, LCLType, Graphics,
  SynEditHighlighter, synhighlighterjava;

type
  TSynPikeSyn = class(TSynJavaSyn)
  private
    function Func17: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func34: TtkTokenKind;
    function Func39: TtkTokenKind;
    function Func40: TtkTokenKind;
    function Func42: TtkTokenKind;
    function Func45: TtkTokenKind;
    function Func46: TtkTokenKind;
    function Func47: TtkTokenKind;
    function Func48: TtkTokenKind;
    function Func54: TtkTokenKind;
    function Func55: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func59: TtkTokenKind;
    function Func60: TtkTokenKind;
    function Func61: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func63: TtkTokenKind;
    function Func66: TtkTokenKind;
    function Func68: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func79: TtkTokenKind;
    function Func83: TtkTokenKind;
    function Func86: TtkTokenKind;
    function Func88: TtkTokenKind;
    function Func89: TtkTokenKind;
    function Func90: TtkTokenKind;
    function Func92: TtkTokenKind;
    function Func93: TtkTokenKind;
    function Func95: TtkTokenKind;
    function Func97: TtkTokenKind;
    function Func98: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func109: TtkTokenKind;
    function Func110: TtkTokenKind;
    function Func114: TtkTokenKind;
    function Func115: TtkTokenKind;
    function Func119: TtkTokenKind;
    function Func127: TtkTokenKind;
    function Func136: TtkTokenKind;
    function Func172: TtkTokenKind;
  protected
    procedure InitIdent; override;
    function GetSampleSource: string; override;
  public
    class function Pike_GetSampleSource: string;
    class function GetLanguageName: string; override;
  end;

implementation

uses
  SynEditStrConst;

procedure TSynPikeSyn.InitIdent;
var
  I: Integer;
begin
  for I := 0 to 172 do
    Case I of
      17: fIdentFuncTable[I] := @Func17;
      21: fIdentFuncTable[I] := @Func21;
      32: fIdentFuncTable[I] := @Func32;
      34: fIdentFuncTable[I] := @Func34;
      39: fIdentFuncTable[I] := @Func39;
      40: fIdentFuncTable[I] := @Func40;
      42: fIdentFuncTable[I] := @Func42;
      45: fIdentFuncTable[I] := @Func45;
      46: fIdentFuncTable[I] := @Func46;
      47: fIdentFuncTable[I] := @Func47;
      48: fIdentFuncTable[I] := @Func48;
      54: fIdentFuncTable[I] := @Func54;
      55: fIdentFuncTable[I] := @Func55;
      57: fIdentFuncTable[I] := @Func57;
      59: fIdentFuncTable[I] := @Func59;
      60: fIdentFuncTable[I] := @Func60;
      61: fIdentFuncTable[I] := @Func61;
      62: fIdentFuncTable[I] := @Func62;
      63: fIdentFuncTable[I] := @Func63;
      66: fIdentFuncTable[I] := @Func66;
      68: fIdentFuncTable[I] := @Func68;
      69: fIdentFuncTable[I] := @Func69;
      76: fIdentFuncTable[I] := @Func76;
      78: fIdentFuncTable[I] := @Func78;
      79: fIdentFuncTable[I] := @Func79;
      83: fIdentFuncTable[I] := @Func83;
      86: fIdentFuncTable[I] := @Func86;
      88: fIdentFuncTable[I] := @Func88;
      89: fIdentFuncTable[I] := @Func89;
      90: fIdentFuncTable[I] := @Func90;
      92: fIdentFuncTable[I] := @Func92;
      93: fIdentFuncTable[I] := @Func93;
      95: fIdentFuncTable[I] := @Func95;
      97: fIdentFuncTable[I] := @Func97;
      98: fIdentFuncTable[I] := @Func98;
      102: fIdentFuncTable[I] := @Func102;
      109: fIdentFuncTable[I] := @Func109;
      110: fIdentFuncTable[I] := @Func110;
      114: fIdentFuncTable[I] := @Func114;
      115: fIdentFuncTable[I] := @Func115;
      119: fIdentFuncTable[I] := @Func119;
      127: fIdentFuncTable[I] := @Func127;
      136: fIdentFuncTable[I] := @Func136;
      172: fIdentFuncTable[I] := @Func172;
    else fIdentFuncTable[I] := @AltFunc;
    end;
end;

function TSynPikeSyn.Func17: TtkTokenKind;
begin
  if KeyComp('if') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func21: TtkTokenKind;
begin
  if KeyComp('do') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func32: TtkTokenKind;
begin
  if KeyComp('case') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func34: TtkTokenKind;
begin
  if KeyComp('char') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func39: TtkTokenKind;
begin
  if KeyComp('lambda') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func40: TtkTokenKind;
begin
  if KeyComp('catch') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func42: TtkTokenKind;
begin
  if KeyComp('for') then Result := tkKey else
    if KeyComp('break') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func45: TtkTokenKind;
begin
  if KeyComp('else') or KeyComp('new') then
    Result := tkKey
  else Result := tkIdentifier;
end;

function TSynPikeSyn.Func46: TtkTokenKind;
begin
  if KeyComp('int') or KeyComp('gauge') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func47: TtkTokenKind;
begin
  if KeyComp('final') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func48: TtkTokenKind;
begin
  if KeyComp('false') or KeyComp('local') or KeyComp('bool') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func54: TtkTokenKind;
begin
  if KeyComp('void') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func55: TtkTokenKind;
begin
  if KeyComp('global') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func57: TtkTokenKind;
begin
  if KeyComp('enum') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func59: TtkTokenKind;
begin
  if KeyComp('class') or KeyComp('float') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func60: TtkTokenKind;
begin
  if KeyComp('this') or KeyComp('mixed') or KeyComp('prefed') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func61: TtkTokenKind;
begin
  if KeyComp('goto') or KeyComp('object') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func62: TtkTokenKind;
begin
  if KeyComp('while') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func63: TtkTokenKind;
begin
  if KeyComp('null') or KeyComp('foreach') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func66: TtkTokenKind;
begin
  if KeyComp('try') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func68: TtkTokenKind;
begin
  if KeyComp('true') or KeyComp('array') or KeyComp('sscanf') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func69: TtkTokenKind;
begin
  if KeyComp('public') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func76: TtkTokenKind;
begin
  if KeyComp('default') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func78: TtkTokenKind;
begin
  if KeyComp('static') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func79: TtkTokenKind;
begin
  if KeyComp('nomask') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func83: TtkTokenKind;
begin
  if KeyComp('mapping') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func86: TtkTokenKind;
begin
  if KeyComp('finally') or KeyComp('sizeof') or KeyComp('inline') then
    Result := tkKey
  else Result := tkIdentifier;
end;

function TSynPikeSyn.Func88: TtkTokenKind;
begin
  if KeyComp('switch') or KeyComp('typedef') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func89: TtkTokenKind;
begin
  if KeyComp('throw') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func90: TtkTokenKind;
begin
  if KeyComp('inherit') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func92: TtkTokenKind;
begin
  if KeyComp('variant') or KeyComp('extern') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func93: TtkTokenKind;
begin
  if KeyComp('string') or KeyComp('typeof') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func95: TtkTokenKind;
begin
  if KeyComp('program') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func97: TtkTokenKind;
begin
  if KeyComp('import') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func98: TtkTokenKind;
begin
  if KeyComp('private') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func102: TtkTokenKind;
begin
  if KeyComp('return') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func109: TtkTokenKind;
begin
  if KeyComp('continue') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func110: TtkTokenKind;
begin
  if KeyComp('function') or KeyComp('optional') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func114: TtkTokenKind;
begin
  if KeyComp('constant') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func115: TtkTokenKind;
begin
  if KeyComp('protected') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func119: TtkTokenKind;
begin
  if KeyComp('strictfp') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func127: TtkTokenKind;
begin
  if KeyComp('multiset') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func136: TtkTokenKind;
begin
  if KeyComp('implements') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.Func172: TtkTokenKind;
begin
  if KeyComp('synchronized') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPikeSyn.GetSampleSource: string;
begin
  Result := Pike_GetSampleSource();
end;

class function TSynPikeSyn.Pike_GetSampleSource: string;
begin
  Result := '/* Pike syntax highlighting */'#13#10 +
            'int main()'#13#10 +
            '{'#13#10 +
            '    array(string) words = ({ "first", "second" });'#13#10 +
            '    foreach(words, string cur_word)'#13#10 +
            '        write("%O\n", cur_word);'#13#10 +
            '    return 0;'#13#10 +
            '}'#13#10 +
            '/* Text Block */'#13#10 + #13#10;
end;

class function TSynPikeSyn.GetLanguageName: string;
begin
  Result := SYNS_LangPike;
end;

initialization
  RegisterPlaceableHighlighter(TSynPikeSyn);

end.

