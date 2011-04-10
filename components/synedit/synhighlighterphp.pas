{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterPHP.pas, released 2000-04-21.
The Original Code is based on the wmPHPSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Willo van der Merwe.
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

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a PHP syntax highlighter for SynEdit)
@author(Willo van der Merwe <willo@wack.co.za>, converted to SynEdit by Bruno Mikkelsen <btm@scientist.com>)
@created(1999, converted to SynEdit 2000-04-21)
@lastmod(2000-06-23)
The SynHighlighterPHP unit provides SynEdit with a PHP syntax highlighter.
Thanks to Martin Waldenburg.
}
{$IFNDEF QSYNHIGHLIGHTERPHP}
unit SynHighlighterPHP;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF SYN_CLX}
  Qt, QControls, QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
  {$ELSE}
  {$IFDEF SYN_LAZARUS}
  LCLIntf, LCLType,
  {$ELSE}
  Windows, Messages, Registry,
  {$ENDIF}
  Controls, Graphics,
  SynEditTypes, SynEditHighlighter;
  {$ENDIF}


type
  TtkTokenKind = (tkComment, tkIdentifier, tkInvalidSymbol, tkKey, tkNull,
    tkNumber, tkSpace, tkString, tkSymbol, tkUnknown, tkVariable);

  TRangeState = (rsUnKnown, rsString39, rsString34, rsComment, rsVarExpansion);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

type
  TSynPHPSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fLine: PChar;
    {$IFDEF SYN_LAZARUS}
    fLineLen: integer;
    {$ENDIF}
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..206] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fInvalidSymbolAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: String): Boolean;
    function Func15: TtkTokenKind;
    function Func18: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func28: TtkTokenKind;
    function Func31: TtkTokenKind;
    function Func33: TtkTokenKind;
    function Func36: TtkTokenKind;
    function Func37: TtkTokenKind;
    function Func38: TtkTokenKind;
    function Func39: TtkTokenKind;
    function Func40: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func42: TtkTokenKind;
    function Func43: TtkTokenKind;
    function Func49: TtkTokenKind;
    function Func54: TtkTokenKind;
    function Func55: TtkTokenKind;
    function Func56: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func58: TtkTokenKind;
    function Func59: TtkTokenKind;
    function Func60: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func63: TtkTokenKind;
    function Func64: TtkTokenKind;
    function Func68: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func71: TtkTokenKind;
    function Func72: TtkTokenKind;
    function Func77: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func79: TtkTokenKind;
    function Func80: TtkTokenKind;
    function Func82: TtkTokenKind;
    function Func87: TtkTokenKind;
    function Func91: TtkTokenKind;
    function Func93: TtkTokenKind;
    function Func96: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func105: TtkTokenKind;
    function Func151: TtkTokenKind;
    function Func156: TtkTokenKind;
    function Func164: TtkTokenKind;
    function Func177: TtkTokenKind;
    function Func206: TtkTokenKind;
    procedure AndSymbolProc;
    procedure AtSymbolProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure MultiplyProc;
    procedure NotSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure PoundProc;
    procedure QuestionProc;
    procedure RemainderSymbolProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StringProc;
    procedure VarExpansionProc;
    procedure TildeProc;
    procedure VariableProc;
    procedure XOrSymbolProc;
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure AnsiCProc;
    procedure String39Proc;
    procedure String34Proc;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
  public
    {$IFNDEF SYN_CPPB_1} class {$ENDIF}                                         //mh 2000-07-14
    function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine({$IFDEF FPC}const {$ENDIF}NewValue: String; LineNumber: Integer); override;
    function GetToken: String; override;
    {$IFDEF SYN_LAZARUS}
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    {$ENDIF}

    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property InvalidSymbolAttri: TSynHighlighterAttributes read fInvalidSymbolAttri
      write fInvalidSymbolAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property VariableAttri: TSynHighlighterAttributes read fVariableAttri
      write fVariableAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable: array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  I, J: Char;
begin
  for I := #0 to #255 do
  begin
    Case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else Identifiers[I] := False;
    end;
//    J := UpperCase(I)[1];
    J := UpCase(I);
    Case I in ['_', 'A'..'Z', 'a'..'z'] of
      True: mHashTable[I] := Ord(J) - 64
    else mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynPHPSyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do begin
    pF^ := {$IFDEF FPC}@{$ENDIF}AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[15] := {$IFDEF FPC}@{$ENDIF}Func15;
  fIdentFuncTable[18] := {$IFDEF FPC}@{$ENDIF}Func18;
  fIdentFuncTable[19] := {$IFDEF FPC}@{$ENDIF}Func19;
  fIdentFuncTable[28] := {$IFDEF FPC}@{$ENDIF}Func28;
  fIdentFuncTable[31] := {$IFDEF FPC}@{$ENDIF}Func31;
  fIdentFuncTable[33] := {$IFDEF FPC}@{$ENDIF}Func33;
  fIdentFuncTable[36] := {$IFDEF FPC}@{$ENDIF}Func36;
  fIdentFuncTable[37] := {$IFDEF FPC}@{$ENDIF}Func37;
  fIdentFuncTable[38] := {$IFDEF FPC}@{$ENDIF}Func38;
  fIdentFuncTable[39] := {$IFDEF FPC}@{$ENDIF}Func39;
  fIdentFuncTable[40] := {$IFDEF FPC}@{$ENDIF}Func40;
  fIdentFuncTable[41] := {$IFDEF FPC}@{$ENDIF}Func41;
  fIdentFuncTable[42] := {$IFDEF FPC}@{$ENDIF}Func42;
  fIdentFuncTable[43] := {$IFDEF FPC}@{$ENDIF}Func43;
  fIdentFuncTable[49] := {$IFDEF FPC}@{$ENDIF}Func49;
  fIdentFuncTable[54] := {$IFDEF FPC}@{$ENDIF}Func54;
  fIdentFuncTable[55] := {$IFDEF FPC}@{$ENDIF}Func55;
  fIdentFuncTable[56] := {$IFDEF FPC}@{$ENDIF}Func56;
  fIdentFuncTable[57] := {$IFDEF FPC}@{$ENDIF}Func57;
  fIdentFuncTable[58] := {$IFDEF FPC}@{$ENDIF}Func58;
  fIdentFuncTable[59] := {$IFDEF FPC}@{$ENDIF}Func59;
  fIdentFuncTable[60] := {$IFDEF FPC}@{$ENDIF}Func60;
  fIdentFuncTable[62] := {$IFDEF FPC}@{$ENDIF}Func62;
  fIdentFuncTable[63] := {$IFDEF FPC}@{$ENDIF}Func63;
  fIdentFuncTable[64] := {$IFDEF FPC}@{$ENDIF}Func64;
  fIdentFuncTable[68] := {$IFDEF FPC}@{$ENDIF}Func68;
  fIdentFuncTable[69] := {$IFDEF FPC}@{$ENDIF}Func69;
  fIdentFuncTable[71] := {$IFDEF FPC}@{$ENDIF}Func71;
  fIdentFuncTable[72] := {$IFDEF FPC}@{$ENDIF}Func72;
  fIdentFuncTable[77] := {$IFDEF FPC}@{$ENDIF}Func77;
  fIdentFuncTable[78] := {$IFDEF FPC}@{$ENDIF}Func78;
  fIdentFuncTable[79] := {$IFDEF FPC}@{$ENDIF}Func79;
  fIdentFuncTable[80] := {$IFDEF FPC}@{$ENDIF}Func80;
  fIdentFuncTable[82] := {$IFDEF FPC}@{$ENDIF}Func82;
  fIdentFuncTable[87] := {$IFDEF FPC}@{$ENDIF}Func87;
  fIdentFuncTable[91] := {$IFDEF FPC}@{$ENDIF}Func91;
  fIdentFuncTable[93] := {$IFDEF FPC}@{$ENDIF}Func93;
  fIdentFuncTable[96] := {$IFDEF FPC}@{$ENDIF}Func96;
  fIdentFuncTable[101] := {$IFDEF FPC}@{$ENDIF}Func101;
  fIdentFuncTable[102] := {$IFDEF FPC}@{$ENDIF}Func102;
  fIdentFuncTable[105] := {$IFDEF FPC}@{$ENDIF}Func105;
  fIdentFuncTable[151] := {$IFDEF FPC}@{$ENDIF}Func151;
  fIdentFuncTable[156] := {$IFDEF FPC}@{$ENDIF}Func156;
  fIdentFuncTable[164] := {$IFDEF FPC}@{$ENDIF}Func164;
  fIdentFuncTable[177] := {$IFDEF FPC}@{$ENDIF}Func177;
  fIdentFuncTable[206] := {$IFDEF FPC}@{$ENDIF}Func206;
end;

function TSynPHPSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

function TSynPHPSyn.KeyComp(const aKey: String): Boolean;
var
  I: Integer;
  Temp: PChar;
begin
  Temp := fToIdent;
  if Length(aKey) = fStringLen then
  begin
    Result := True;
    for i := 1 to fStringLen do
    begin
      if mHashTable[Temp^] <> mHashTable[aKey[i]] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end else Result := False;
end;

function TSynPHPSyn.Func15: TtkTokenKind;
begin
  if KeyComp('if') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func18: TtkTokenKind;
begin
  if KeyComp('die') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func19: TtkTokenKind;
begin
  if KeyComp('and') then Result := tkKey else
    if KeyComp('do') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func28: TtkTokenKind;
begin
  if KeyComp('case') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func31: TtkTokenKind;
begin
  if KeyComp('echo') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func33: TtkTokenKind;
begin
  if KeyComp('or') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func36: TtkTokenKind;
begin
  if KeyComp('real') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func37: TtkTokenKind;
begin
  if KeyComp('break') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func38: TtkTokenKind;
begin
  if KeyComp('endif') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func39: TtkTokenKind;
begin
  if KeyComp('for') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func40: TtkTokenKind;
begin
  if KeyComp('eval') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func41: TtkTokenKind;
begin
  if KeyComp('var') then Result := tkKey else
    if KeyComp('else') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func42: TtkTokenKind;
begin
  if KeyComp('new') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func43: TtkTokenKind;
begin
  if KeyComp('false') then Result := tkKey else
    if KeyComp('int') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func49: TtkTokenKind;
begin
  if KeyComp('global') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func54: TtkTokenKind;
begin
  if KeyComp('float') then Result := tkKey else
    if KeyComp('class') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func55: TtkTokenKind;
begin
  if KeyComp('object') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func56: TtkTokenKind;
begin
  if KeyComp('elseif') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func57: TtkTokenKind;
begin
  if KeyComp('while') then Result := tkKey else
    if KeyComp('xor') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func58: TtkTokenKind;
begin
  if KeyComp('exit') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func59: TtkTokenKind;
begin
  if KeyComp('double') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func60: TtkTokenKind;
begin
  if KeyComp('list') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func62: TtkTokenKind;
begin
  if KeyComp('endfor') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func63: TtkTokenKind;
begin
  if KeyComp('array') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func64: TtkTokenKind;
begin
  if KeyComp('true') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func68: TtkTokenKind;
begin
  if KeyComp('include') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func69: TtkTokenKind;
begin
  if KeyComp('default') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func71: TtkTokenKind;
begin
  if KeyComp('const') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func72: TtkTokenKind;
begin
  if KeyComp('isset') then Result := tkKey else
    if KeyComp('static') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func77: TtkTokenKind;
begin
  if KeyComp('print') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func78: TtkTokenKind;
begin
  if KeyComp('integer') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func79: TtkTokenKind;
begin
  if KeyComp('empty') then Result := tkKey else
    if KeyComp('unset') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func80: TtkTokenKind;
begin
  if KeyComp('endwhile') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func82: TtkTokenKind;
begin
  if KeyComp('switch') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func87: TtkTokenKind;
begin
  if KeyComp('string') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func91: TtkTokenKind;
begin
  if KeyComp('extends') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func93: TtkTokenKind;
begin
  if KeyComp('require') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func96: TtkTokenKind;
begin
  if KeyComp('return') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func101: TtkTokenKind;
begin
  if KeyComp('continue') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func102: TtkTokenKind;
begin
  if KeyComp('function') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func105: TtkTokenKind;
begin
  if KeyComp('cfunction') then Result := tkKey else
    if KeyComp('endswitch') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func151: TtkTokenKind;
begin
  if KeyComp('highlight_file') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func156: TtkTokenKind;
begin
  if KeyComp('__FILE__') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func164: TtkTokenKind;
begin
  if KeyComp('old_function') then Result := tkKey else
    if KeyComp('__LINE__') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func177: TtkTokenKind;
begin
  if KeyComp('show_source') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.Func206: TtkTokenKind;
begin
  if KeyComp('highlight_string') then Result := tkKey else Result := tkIdentifier;
end;

function TSynPHPSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynPHPSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 207 then
    Result := fIdentFuncTable[HashKey]{$IFDEF FPC}(){$ENDIF}
  else
    Result := tkIdentifier;
end;

procedure TSynPHPSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '&': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}AndSymbolProc;
      #39: fProcTable[I] := {$IFDEF FPC}@{$ENDIF}String39Proc; // single quote
      '@': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}AtSymbolProc;
      '}': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}BraceCloseProc;
      '{': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}BraceOpenProc;
      #13: fProcTable[I] := {$IFDEF FPC}@{$ENDIF}CRProc;
      ':': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}ColonProc;
      ',': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}CommaProc;
      '=': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}EqualProc;
      '>': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}GreaterProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}IdentProc;
      #10: fProcTable[I] := {$IFDEF FPC}@{$ENDIF}LFProc;
      '<': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}LowerProc;
      '-': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}MinusProc;
      '*': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}MultiplyProc;
      '!': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}NotSymbolProc;
      #0: fProcTable[I] := {$IFDEF FPC}@{$ENDIF}NullProc;
      '0'..'9': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}NumberProc;
      '|': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}OrSymbolProc;
      '+': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}PlusProc;
      '.': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}PointProc;
      '#': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}PoundProc;
      '?': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}QuestionProc;
      '%': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}RemainderSymbolProc;
      ')': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}RoundCloseProc;
      '(': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}RoundOpenProc;
      ';': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}SemiColonProc;
      '/': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}SlashProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := {$IFDEF FPC}@{$ENDIF}SpaceProc;
      ']': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}SquareCloseProc;
      '[': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}SquareOpenProc;
      #34: fProcTable[I] := {$IFDEF FPC}@{$ENDIF}String34Proc; // double quote
      '~': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}TildeProc;
      '$': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}VariableProc;
      '^': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}XOrSymbolProc;
      else fProcTable[I] := {$IFDEF FPC}@{$ENDIF}UnknownProc;
    end;
end;

constructor TSynPHPSyn.Create(AOwner: TComponent);
begin
{begin}                                                                         //mh 2000-01-14
  inherited Create(AOwner);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_XML_AttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_XML_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fInvalidSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrInvalidSymbol, SYNS_XML_AttrInvalidSymbol);
  AddAttribute(fInvalidSymbolAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_XML_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_XML_AttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_XML_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_XML_AttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_XML_AttrSymbol);
  AddAttribute(fSymbolAttri);
  fVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_XML_AttrVariable);
  AddAttribute(fVariableAttri);
{end}                                                                           //mh 2000-01-14
  SetAttributesOnChange({$IFDEF FPC}@{$ENDIF}DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterPHP;
  fRange := rsUnknown;
end;

procedure TSynPHPSyn.SetLine({$IFDEF FPC}const {$ENDIF}NewValue: String; LineNumber: Integer);
begin
  inherited;
  fLine := PChar(NewValue);
  {$IFDEF SYN_LAZARUS}
  fLineLen := length(NewValue);
  {$ENDIF}
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynPHPSyn.AndSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {and assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '&':                               {conditional and}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {and}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.AtSymbolProc;
begin
  fTokenID := tkInvalidSymbol;
  inc(Run);
end;

procedure TSynPHPSyn.BraceCloseProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynPHPSyn.BraceOpenProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynPHPSyn.CRProc;
begin
  fTokenID := tkSpace;
  Case FLine[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;

procedure TSynPHPSyn.ColonProc;
begin
  inc(Run);                            {colon - conditional}
  fTokenID := tkSymbol;
end;

procedure TSynPHPSyn.CommaProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynPHPSyn.EqualProc;
begin
  case FLine[Run + 1] of
    '=':                               {logical equal}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '>':                               {Hash operator}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {assign}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.GreaterProc;
begin
  Case FLine[Run + 1] of
    '=':                               {greater than or equal to}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '>':
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {greater than}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynPHPSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynPHPSyn.LowerProc;
begin
  case FLine[Run + 1] of
    '=':                               {less than or equal to}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '<':
      begin
        if FLine[Run + 2] = '=' then   {shift left assign}
        begin
          inc(Run, 3)
        end
        else                           {shift left}
        begin
          inc(Run, 2);
        end;
        fTokenID := tkSymbol;
      end;
  else                                 {less than}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.MinusProc;
begin
  case FLine[Run + 1] of
    '=':                               {subtract assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '-':                               {decrement}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '>':                               {Class operator}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {subtract}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.MultiplyProc;
begin
  case FLine[Run + 1] of
    '=':                               {multiply assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {multiply}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.NotSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {not equal}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {logical complement}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.NullProc;
begin
  fTokenID := tkNull;
  {$IFDEF SYN_LAZARUS}
  if Run<fLineLen then
    inc(Run);
  {$ENDIF}
end;

procedure TSynPHPSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in
      ['0'..'9', '.', '-', 'l', 'L', 'x', 'X', 'A'..'F', 'a'..'f'] do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
end;

procedure TSynPHPSyn.OrSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {inclusive or assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '|':                               {conditional or}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {inclusive or}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.PlusProc;
begin
  case FLine[Run + 1] of
    '=':                               {add assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '+':                               {increment}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {add}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.PointProc;
begin
  inc(Run);                            {point}
  fTokenID := tkSymbol;
end;

procedure TSynPHPSyn.PoundProc;
begin
  repeat
    inc(Run);
  until FLine[Run] in [#0, #10, #13];
  fTokenID := tkComment;
end;

procedure TSynPHPSyn.QuestionProc;
begin
  fTokenID := tkSymbol;                {question mark - conditional}
  inc(Run);
end;

procedure TSynPHPSyn.RemainderSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {remainder assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {remainder}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.RoundCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynPHPSyn.RoundOpenProc;
begin
  inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynPHPSyn.SemiColonProc;
begin
  inc(Run);                            {semicolon}
  fTokenID := tkSymbol;
end;

procedure TSynPHPSyn.SlashProc;
begin
  case FLine[Run + 1] of
    '/':                               {c++ style comments}
      begin
        inc(Run, 2);
        fTokenID := tkComment;
        while FLine[Run] <> #0 do
        begin
          case FLine[Run] of
            #10, #13: break;
          end;
          inc(Run);
        end;
      end;
    '*':
      begin
        fRange := rsComment;
        inc(Run);
        fTokenID := tkComment;       {c style comment}

        inc(Run);
        while fLine[Run] <> #0 do
          case fLine[Run] of
            '*':
              if fLine[Run + 1] = '/' then
              begin
                fRange := rsUnKnown;
                inc(Run, 2);
                break;
              end else inc(Run);
            #10: break;
            #13: break;
          else inc(Run);
          end;
      end;
    '=':                               {division assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {division}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynPHPSyn.SquareCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynPHPSyn.SquareOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynPHPSyn.StringProc;

  function IsEscaped: boolean;
  var
    iFirstSlashPos: integer;
  begin
    iFirstSlashPos := Run -1;
    while (iFirstSlashPos > 0) and (FLine[iFirstSlashPos] = '\') do
      Dec( iFirstSlashPos );
    Result := (Run - iFirstSlashPos +1) mod 2 <> 0;
  end;

var
  iCloseChar: char;
begin
  if (FLine[Run] in [#0, #10, #13]) and (fTokenPos = Run) then
  begin
    fProcTable[ FLine[Run] ]{$IFDEF FPC}(){$ENDIF};
    Exit;
  end;
  fTokenID := tkString;
  if fRange = rsString39 then
    iCloseChar := #39
  else
    iCloseChar := #34;
  while not( FLine[Run] in [#0, #10, #13] ) do
  begin
    if (FLine[Run] = iCloseChar) and (not IsEscaped) then
      break;
    if (FLine[Run] = '$') and (iCloseChar = '"') and
      ( (FLine[Run +1] = '{') or Identifiers[ FLine[Run +1] ] ) then
    begin
      if (Run > 1) and (FLine[Run -1] = '{') then { complex syntax }
        Dec( Run );
      if not IsEscaped then
      begin
        { break the token to process the variable }
        fRange := rsVarExpansion;
        Exit;
      end
      else if FLine[Run] = '{' then
        Inc( Run ); { restore Run if we previously deincremented it }
    end;
    Inc(Run);
  end;
  if (FLine[Run] = iCloseChar) then
    fRange := rsUnKnown;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynPHPSyn.VarExpansionProc;
type
  TExpansionSyntax = (esNormal, esComplex, esBrace);
var
  iSyntax: TExpansionSyntax;
  iOpenBraces: integer;
  iOpenBrackets: integer;
  iTempRun: integer;
begin
  fRange := rsString34; { var expansion only occurs in double quoted strings }
  FTokenID := tkVariable;
  if FLine[Run] = '{' then
  begin
    iSyntax := esComplex;
    Inc( Run, 2 ); // skips '{$' }
  end
  else begin
    Inc( Run );
    if FLine[Run] = '{' then
    begin
      iSyntax := esBrace;
      Inc( Run );
    end
    else
      iSyntax := esNormal;
  end;
  if iSyntax in [esBrace, esComplex] then
  begin
    iOpenBraces := 1;
    while FLine[Run] <> #0 do
    begin
      if FLine[Run] = '}' then
      begin
        Dec( iOpenBraces );
        if iOpenBraces = 0 then
        begin
          Inc( Run );
          break;
        end;
      end;
      if FLine[Run] = '{' then
        Inc( iOpenBraces );
      Inc( Run );
    end;
  end
  else begin
    while Identifiers[ FLine[Run] ] do
      Inc( Run );
    iOpenBrackets := 0;
    iTempRun := Run;
    { process arrays and objects }
    while FLine[iTempRun] <> #0 do
    begin
      if FLine[iTempRun] = '[' then
      begin
        Inc( iTempRun );
        if FLine[iTempRun] = #39 then
        begin
          Inc( iTempRun );
          while (FLine[iTempRun] <> #39) and (FLine[iTempRun] <> #0) do
            Inc( iTempRun );
          if (FLine[iTempRun] = #39) and (fLine[iTempRun +1] = ']') then
          begin
            Inc( iTempRun, 2 );
            Run := iTempRun;
            continue;
          end
          else
            break;
        end
        else
          Inc( iOpenBrackets );
      end
      else if (FLine[iTempRun] = '-') and (FLine[iTempRun +1] = '>') then
        Inc( iTempRun, 2 )
      else
        break;

      if not Identifiers[ FLine[iTempRun] ] then
        break
      else
        repeat
          Inc( iTempRun );
        until not Identifiers[ FLine[iTempRun] ];

      while FLine[iTempRun] = ']' do
      begin
        if iOpenBrackets = 0 then
          break;
        Dec( iOpenBrackets );
        Inc( iTempRun );
      end;
      if iOpenBrackets = 0 then
        Run := iTempRun;
    end;
  end;
end;

procedure TSynPHPSyn.TildeProc;
begin
  inc(Run);                            {bitwise complement}
  fTokenId := tkSymbol;
end;

procedure TSynPHPSyn.VariableProc;
begin
  fTokenID := tkVariable;
  inc(Run);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynPHPSyn.XOrSymbolProc;
begin
  Case FLine[Run + 1] of
    '=':                               {xor assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {xor}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.UnknownProc;
begin
  inc(Run);
  {$IFDEF SYN_LAZARUS}
  while (fLine[Run] in [#128..#191]) OR // continued utf8 subcode
   ((fLine[Run]<>#0) and (fProcTable[fLine[Run]] = @UnknownProc)) do inc(Run);
  {$ENDIF}
  fTokenID := tkUnknown;
end;

procedure TSynPHPSyn.AnsiCProc;
begin
  fTokenID := tkComment;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        exit;
      end;
    #10:
      begin
        LFProc;
        exit;
      end;
    #13:
      begin
        CRProc;
        exit;
      end;
  end;

  while FLine[Run] <> #0 do
    case FLine[Run] of
      '*':
        if fLine[Run + 1] = '/' then
        begin
          inc(Run, 2);
          fRange := rsUnKnown;
          break;
        end
        else inc(Run);
      #10: break;
      #13: break;
    else inc(Run);
    end;
end;

procedure TSynPHPSyn.String39Proc;
begin
  fRange := rsString39;
  Inc( Run );
  StringProc;
end;

procedure TSynPHPSyn.String34Proc;
begin
  fRange := rsString34;
  Inc( Run );
  StringProc;
end;

procedure TSynPHPSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsComment: AnsiCProc;
    rsString39, rsString34: StringProc;
    rsVarExpansion: VarExpansionProc;
    else begin
      fRange := rsUnknown;
      fProcTable[fLine[Run]]{$IFDEF FPC}(){$ENDIF};
    end;
  end;
end;

function TSynPHPSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynPHPSyn.GetEol: Boolean;
begin
  Result := (fTokenID = tkNull) and (Run>=fLineLen);
end;

function TSynPHPSyn.GetRange: Pointer;
begin
  Result := Pointer(PtrInt(fRange));
end;

function TSynPHPSyn.GetToken: String;
var
  Len: LongInt;
begin
  Result := '';
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynPHPSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynPHPSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkInvalidSymbol: Result := fInvalidSymbolAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkVariable: Result := fVariableAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynPHPSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynPHPSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynPHPSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynPHPSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(PtrUInt(Value));
end;

function TSynPHPSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

{$IFDEF SYN_LAZARUS}
procedure TSynPHPSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength:=Run-fTokenPos;
  TokenStart:=FLine + fTokenPos;
end;
{$ENDIF}



{$IFNDEF SYN_CPPB_1} class {$ENDIF}                                             //mh 2000-07-14
function TSynPHPSyn.GetLanguageName: string;
begin
  Result := SYNS_LangPHP;
end;

function TSynPHPSyn.GetSampleSource: string;
begin
  Result := '// Syntax highlighting'#13#10+
            'function printNumber()'#13#10+
            '{'#13#10+
            '  $number = 1234;'#13#10+
            '  print "The number is $number";'#13#10+
            '  for ($i = 0; $i <= $number; $i++)'#13#10+
            '  {'#13#10+
            '    $x++;'#13#10+
            '    $x--;'#13#10+
            '    $x += 1.0;'#13#10+
            '  }'#13#10+
            '  $number += @; // illegal character'#13#10+
            '}';

end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}                                                            //mh 2000-07-14
  RegisterPlaceableHighlighter(TSynPHPSyn);
{$ENDIF}
end.

