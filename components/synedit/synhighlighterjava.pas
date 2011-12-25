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

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Java highlighter for SynEdit)
@author(Michael Trier)
@created(December 1998, converted to SynEdit 2000-04-10 by Michael Hieke)
@lastmod(2000-06-23)
The SynHighlighterJava unit provides SynEdit with a Java source (.java) highlighter.
}
unit SynHighlighterJava;

{$I SynEdit.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF SYN_LAZARUS}
  LCLIntf, LCLType, Graphics,
  {$ELSE}
  {$IFDEF SYN_CLX}
  Qt, QControls, QGraphics,
  {$ELSE}
  Windows, Messages, Controls, Graphics, Registry,
  {$ENDIF}
  {$ENDIF}
  SynEditTypes, SynEditHighlighter;

type
  TtkTokenKind = (tkComment, tkDocument, tkIdentifier, tkInvalid, tkKey,
    tkNull, tkNumber, tkSpace, tkString, tkSymbol, tkUnknown);

  TxtkTokenKind = (
    xtkAdd, xtkAddAssign, xtkAnd, xtkAndAssign, xtkAssign, xtkBitComplement,
    xtkBraceClose, xtkBraceOpen, xtkColon, xtkCondAnd, xtkCondOr, xtkDecrement,
    xtkDivide, xtkDivideAssign, xtkGreaterThan, xtkGreaterThanEqual, xtkIncOr,
    xtkIncOrAssign, xtkIncrement, xtkLessThan, xtkLessThanEqual,
    xtkLogComplement, xtkLogEqual, xtkMultiply, xtkMultiplyAssign, xtkNotEqual,
    xtkPoint, xtkQuestion, xtkRemainder, xtkRemainderAssign, xtkRoundClose,
    xtkRoundOpen, xtkSemiColon, xtkShiftLeft, xtkShiftLeftAssign, xtkShiftRight,
    xtkShiftRightAssign, xtkSquareClose, xtkSquareOpen, xtkSubtract,
    xtkSubtractAssign, xtkUnsignShiftRight, xtkUnsignShiftRightAssign, xtkXor,
    xtkXorAssign, xtkComma);

  TRangeState = (rsANil, rsComment, rsDocument, rsUnknown);

  TProcTableProc = procedure of Object;
  TIdentFuncTableFunc = function: TtkTokenKind of Object;

  TSynJavaSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fLine: PChar;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    FRoundCount: Integer;
    FSquareCount: Integer;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    FExtTokenID: TxtkTokenKind;
    fEol: Boolean;
    fIdentFuncTable: array[0..172] of TIdentFuncTableFunc;
    fLineNumber: Integer;
    fCommentAttri: TSynHighlighterAttributes;
    fDocumentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fInvalidAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: String): Boolean;
    function Func17: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func34: TtkTokenKind;
    function Func40: TtkTokenKind;
    function Func42: TtkTokenKind;
    function Func45: TtkTokenKind;
    function Func46: TtkTokenKind;
    function Func47: TtkTokenKind;
    function Func48: TtkTokenKind;
    function Func51: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func54: TtkTokenKind;
    function Func56: TtkTokenKind;
    function Func59: TtkTokenKind;
    function Func60: TtkTokenKind;
    function Func61: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func63: TtkTokenKind;
    function Func65: TtkTokenKind;
    function Func66: TtkTokenKind;
    function Func68: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func71: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func77: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func84: TtkTokenKind;
    function Func85: TtkTokenKind;
    function Func86: TtkTokenKind;
    function Func88: TtkTokenKind;
    function Func89: TtkTokenKind;
    function Func90: TtkTokenKind;
    function Func92: TtkTokenKind;
    function Func97: TtkTokenKind;
    function Func98: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func104: TtkTokenKind;
    function Func109: TtkTokenKind;
    function Func115: TtkTokenKind;
    function Func116: TtkTokenKind;
    function Func119: TtkTokenKind;
    function Func129: TtkTokenKind;
    function Func136: TtkTokenKind;
    function Func172: TtkTokenKind;
    procedure CommentProc;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
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
    procedure TildeProc;
    procedure XOrSymbolProc;
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
    function GetExtTokenID: TxtkTokenKind;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;    
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine({$IFDEF FPC}const {$ENDIF}NewValue: String;
      LineNumber:Integer); override;
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
    property ExtTokenID: TxtkTokenKind read GetExtTokenID;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property DocumentAttri: TSynHighlighterAttributes read fDocumentAttri
      write fDocumentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property InvalidAttri: TSynHighlighterAttributes read fInvalidAttri
      write fInvalidAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
  end;

implementation

uses
  SynEditStrConst;

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable: array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  I: Char;
begin
  for I := #0 to #255 do
  begin
    // Java allows special characters in identifier names
    Identifiers[I] := (I in ['_', '$', '0'..'9', 'a'..'z', 'A'..'Z']) or (I in TSynSpecialChars);
    if (I in ['_', '$', 'a'..'z', 'A'..'Z']) or (I in TSynSpecialChars) then
    begin
      if (I > #64) and (I < #91) then
        mHashTable[I] := Ord(I) - 64
      else
        if (I > #96) then
          mHashTable[I] := Ord(I) - 95;
    end
    else
      mHashTable[I] := 0;
  end;
end;

procedure TSynJavaSyn.InitIdent;
var
  I: Integer;
begin
  for I := 0 to 172 do
    Case I of
      17: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func17;
      21: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func21;
      32: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func32;
      34: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func34;
      40: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func40;
      42: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func42;
      45: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func45;
      46: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func46;
      47: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func47;
      48: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func48;
      51: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func51;
      52: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func52;
      54: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func54;
      56: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func56;
      59: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func59;
      60: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func60;
      61: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func61;
      62: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func62;
      63: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func63;
      65: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func65;
      66: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func66;
      68: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func68;
      69: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func69;
      71: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func71;
      76: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func76;
      77: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func77;
      78: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func78;
      84: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func84;
      85: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func85;
      86: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func86;
      88: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func88;
      89: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func89;
      90: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func90;
      92: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func92;
      97: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func97;
      98: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func98;
      102: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func102;
      104: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func104;
      109: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func109;
      115: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func115;
      116: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func116;
      119: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func119;
      129: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func129;
      136: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func136;
      172: fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}Func172;
    else fIdentFuncTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}AltFunc;
    end;
end;

function TSynJavaSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while (ToHash^ in ['_', '$', '0'..'9', 'a'..'z', 'A'..'Z']) or
        (ToHash^ in TSynSpecialChars) do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end; { KeyHash }

function TSynJavaSyn.KeyComp(const aKey: String): Boolean;
var
  I: Integer;
  Temp: PChar;
begin
  Temp := FToIdent;
  if Length(aKey) = fStringLen then
  begin
    Result := True;
    for i := 1 to fStringLen do
    begin
      if Temp^ <> aKey[i] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end else Result := False;
end; { KeyComp }

function TSynJavaSyn.Func17: TtkTokenKind;
begin
  if KeyComp('if') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func21: TtkTokenKind;
begin
  if KeyComp('do') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func32: TtkTokenKind;
begin
  if KeyComp('case') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func34: TtkTokenKind;
begin
  if KeyComp('char') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func40: TtkTokenKind;
begin
  if KeyComp('catch') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func42: TtkTokenKind;
begin
  if KeyComp('for') then Result := tkKey else
    if KeyComp('break') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func45: TtkTokenKind;
begin
  if KeyComp('else') then Result := tkKey else
    if KeyComp('new') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func46: TtkTokenKind;
begin
  if KeyComp('int') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func47: TtkTokenKind;
begin
  if KeyComp('final') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func48: TtkTokenKind;
begin
  if KeyComp('false') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func51: TtkTokenKind;
begin
  if KeyComp('package') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func52: TtkTokenKind;
begin
  if KeyComp('long') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func54: TtkTokenKind;
begin
  if KeyComp('void') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func56: TtkTokenKind;
begin
  if KeyComp('byte') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func59: TtkTokenKind;
begin
  if KeyComp('class') then Result := tkKey else
    if KeyComp('float') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func60: TtkTokenKind;
begin
  if KeyComp('this') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func61: TtkTokenKind;
begin
  if KeyComp('goto') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func62: TtkTokenKind;
begin
  if KeyComp('while') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func63: TtkTokenKind;
begin
  if KeyComp('null') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func65: TtkTokenKind;
begin
  if KeyComp('double') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func66: TtkTokenKind;
begin
  if KeyComp('try') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func68: TtkTokenKind;
begin
  if KeyComp('true') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func69: TtkTokenKind;
begin
  if KeyComp('public') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func71: TtkTokenKind;
begin
  if KeyComp('boolean') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func76: TtkTokenKind;
begin
  if KeyComp('default') then Result := tkKey else
    if KeyComp('const') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func77: TtkTokenKind;
begin
  if KeyComp('native') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func78: TtkTokenKind;
begin
  if KeyComp('static') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func84: TtkTokenKind;
begin
  if KeyComp('super') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func85: TtkTokenKind;
begin
  if KeyComp('short') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func86: TtkTokenKind;
begin
  if KeyComp('finally') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func88: TtkTokenKind;
begin
  if KeyComp('switch') then Result := tkKey else
    if KeyComp('assert') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func89: TtkTokenKind;
begin
  if KeyComp('throw') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func90: TtkTokenKind;
begin
  if KeyComp('interface') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func92: TtkTokenKind;
begin
  if KeyComp('abstract') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func97: TtkTokenKind;
begin
  if KeyComp('import') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func98: TtkTokenKind;
begin
  if KeyComp('extends') then Result := tkKey else
    if KeyComp('private') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func102: TtkTokenKind;
begin
  if KeyComp('return') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func104: TtkTokenKind;
begin
  if KeyComp('volatile') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func109: TtkTokenKind;
begin
  if KeyComp('continue') then Result := tkKey else
    if KeyComp('throws') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func115: TtkTokenKind;
begin
  if KeyComp('protected') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func116: TtkTokenKind;
begin
  if KeyComp('instanceof') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func119: TtkTokenKind;
begin
  if KeyComp('strictfp') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func129: TtkTokenKind;
begin
  if KeyComp('transient') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func136: TtkTokenKind;
begin
  if KeyComp('implements') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.Func172: TtkTokenKind;
begin
  if KeyComp('synchronized') then Result := tkKey else Result := tkIdentifier;
end;

function TSynJavaSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynJavaSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 173 then
    Result := fIdentFuncTable[HashKey]()
  else
    Result := tkIdentifier;
end;

procedure TSynJavaSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '&': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}AndSymbolProc;
      #39: fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}AsciiCharProc;
      '@': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}AtSymbolProc;
      '}': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}BraceCloseProc;
      '{': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}BraceOpenProc;
      #13: fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}CRProc;
      ':': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}ColonProc;
      ',': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}CommaProc;
      '=': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}EqualProc;
      '>': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}GreaterProc;
      'A'..'Z', 'a'..'z', '_', '$':
           fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}IdentProc;
      #10: fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}LFProc;
      '<': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}LowerProc;
      '-': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}MinusProc;
      '*': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}MultiplyProc;
      '!': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}NotSymbolProc;
      #0: fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}NullProc;
      '0'..'9': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}NumberProc;
      '|': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}OrSymbolProc;
      '+': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}PlusProc;
      '.': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}PointProc;
      '#': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}PoundProc;
      '?': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}QuestionProc;
      '%': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}RemainderSymbolProc;
      ')': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}RoundCloseProc;
      '(': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}RoundOpenProc;
      ';': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}SemiColonProc;
      '/': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}SlashProc;
      #1..#9, #11, #12, #14..#32:
           fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}SpaceProc;
      ']': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}SquareCloseProc;
      '[': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}SquareOpenProc;
      #34: fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}StringProc;
      '~': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}TildeProc;
      '^': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}XOrSymbolProc;
    else
      if (I in TSynSpecialChars) then
        fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}IdentProc
      else
        fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}UnknownProc;
    end;
end;

constructor TSynJavaSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_XML_AttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fDocumentAttri := TSynHighlighterAttributes.Create(SYNS_AttrDocumentation, SYNS_XML_AttrDocumentation);
  fDocumentAttri.Style := [fsItalic];
  AddAttribute(fDocumentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_XML_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fInvalidAttri := TSynHighlighterAttributes.Create(SYNS_AttrInvalidSymbol, SYNS_XML_AttrInvalidSymbol);
  AddAttribute(fInvalidAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_XML_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_XML_AttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_XML_AttrSpace);
  fSpaceAttri.Foreground := clWindow;
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_XML_AttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_XML_AttrSymbol);
  AddAttribute(fSymbolAttri);
  fRange := rsUnknown;
  SetAttributesOnChange({$IFDEF SYN_LAZARUS}@{$ENDIF}DefHighlightChange);

  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterJava;
end; { Create }

procedure TSynJavaSyn.SetLine({$IFDEF FPC}const {$ENDIF}NewValue: String;
  LineNumber:Integer);
begin
  inherited;
  fLine := PChar(NewValue);
  Run := 0;
  fEol := False;
  fLineNumber := LineNumber;
  Next;
end; { SetLine }

procedure TSynJavaSyn.CommentProc;
begin
  if fRange = rsComment then
    fTokenID := tkComment
  else
    fTokenID := tkDocument;
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
          fRange := rsUnknown;
          break;
        end
        else inc(Run);
      #10: break;
      #13: break;
    else inc(Run);
    end;
end;

procedure TSynJavaSyn.AndSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {and assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
        FExtTokenID := xtkAndAssign;
      end;
    '&':                               {conditional and}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
        FExtTokenID := xtkCondAnd;
      end;
  else                                 {and}
    begin
      inc(Run);
      fTokenID := tkSymbol;
      FExtTokenID := xtkAnd;
    end;
  end;
end;

procedure TSynJavaSyn.AsciiCharProc;
begin
  fTokenID := tkString;
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
      #92: Inc(Run); // backslash, if we have an escaped single character, skip to the next
    end;
    if FLine[Run] <> #0 then inc(Run); //Add check here to prevent overrun from backslash being last char
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynJavaSyn.AtSymbolProc;
begin
  fTokenID := tkInvalid;
  inc(Run);
end;

procedure TSynJavaSyn.BraceCloseProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
  FExtTokenID := xtkBraceClose;
end;

procedure TSynJavaSyn.BraceOpenProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
  FExtTokenID := xtkBraceOpen;
end;

procedure TSynJavaSyn.CRProc;
begin
  fTokenID := tkSpace;
  Case FLine[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;

procedure TSynJavaSyn.ColonProc;
begin
  inc(Run);                            {colon - conditional}
  fTokenID := tkSymbol;
  FExtTokenID := xtkColon;
end;

procedure TSynJavaSyn.CommaProc;
begin
  inc(Run);
  fTokenID := tkSymbol; //tkInvalid;                                            //DDH Addition from Eden Kirin
  fExtTokenID := xtkComma;                                                      //GBN 13/12/2001
end;

procedure TSynJavaSyn.EqualProc;
begin
  case FLine[Run + 1] of
    '=':                               {logical equal}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
        FExtTokenID := xtkLogEqual;
      end;
  else                                 {assign}
    begin
      inc(Run);
      fTokenID := tkSymbol;
      FExtTokenID := xtkAssign;
    end;
  end;
end;

procedure TSynJavaSyn.GreaterProc;
begin
  Case FLine[Run + 1] of
    '=':                               {greater than or equal to}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
        FExtTokenID := xtkGreaterThanEqual;
      end;
    '>':
      begin
        Case FLine[Run + 2] of
          '=':                         {shift right assign}
            begin
            inc(Run, 3);
            FExtTokenID := xtkShiftRightAssign;
            end;
          '>':
            if FLine[Run + 3] = '=' then
            begin
              inc(Run, 4);             {unsigned shift right assign}
              FExtTokenID := xtkUnsignShiftRightAssign;
            end
            else
            begin
              inc(Run, 3);             {unsigned shift right}
              FExtTokenID := xtkUnsignShiftRight;
            end;
        else                           {shift right}
          begin
            inc(Run, 2);
            FExtTokenID := xtkShiftRight;
          end;
        end;
        fTokenID := tkSymbol;
      end;
  else                                 {greater than}
    begin
      inc(Run);
      fTokenID := tkSymbol;
      FExtTokenID := xtkGreaterThan;
    end;
  end;
end;

procedure TSynJavaSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynJavaSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynJavaSyn.LowerProc;
begin
  case FLine[Run + 1] of
    '=':                               {less than or equal to}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
        FExtTokenID := xtkLessThanEqual;
      end;
    '<':
      begin
        if FLine[Run + 2] = '=' then   {shift left assign}
        begin
          inc(Run, 3);
          FExtTokenID := xtkShiftLeftAssign;
        end
        else                           {shift left}
        begin
          inc(Run, 2);
          FExtTokenID := xtkShiftLeft;
        end;
        fTokenID := tkSymbol;
      end;
  else                                 {less than}
    begin
      inc(Run);
      fTokenID := tkSymbol;
      FExtTokenID := xtkLessThan;
    end;
  end;
end;

procedure TSynJavaSyn.MinusProc;
begin
  case FLine[Run + 1] of
    '=':                               {subtract assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
        FExtTokenID := xtkSubtractAssign;
      end;
    '-':                               {decrement}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
        FExtTokenID := xtkDecrement;
      end;
  else                                 {subtract}
    begin
      inc(Run);
      fTokenID := tkSymbol;
      FExtTokenID := xtkSubtract;
    end;
  end;
end;

procedure TSynJavaSyn.MultiplyProc;
begin
  case FLine[Run + 1] of
    '=':                               {multiply assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
        FExtTokenID := xtkMultiplyAssign;
      end;
  else                                 {multiply}
    begin
      inc(Run);
      fTokenID := tkSymbol;
      FExtTokenID := xtkMultiply;
    end;
  end;
end;

procedure TSynJavaSyn.NotSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {not equal}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
        FExtTokenID := xtkNotEqual;
      end;
  else                                 {logical complement}
    begin
      inc(Run);
      fTokenID := tkSymbol;
      FExtTokenID := xtkLogComplement;
    end;
  end;
end;

procedure TSynJavaSyn.NullProc;
begin
  fTokenID := tkNull;
  fEol := True;
end;

procedure TSynJavaSyn.NumberProc;
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

procedure TSynJavaSyn.OrSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {inclusive or assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
        FExtTokenID := xtkIncOrAssign;
      end;
    '|':                               {conditional or}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
        FExtTokenID := xtkCondOr;
      end;
  else                                 {inclusive or}
    begin
      inc(Run);
      fTokenID := tkSymbol;
      FExtTokenID := xtkIncOr;
    end;
  end;
end;

procedure TSynJavaSyn.PlusProc;
begin
  case FLine[Run + 1] of
    '=':                               {add assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
        FExtTokenID := xtkAddAssign;
      end;
    '+':                               {increment}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
        FExtTokenID := xtkIncrement;
      end;
  else                                 {add}
    begin
      inc(Run);
      fTokenID := tkSymbol;
      FExtTokenID := xtkAdd;
    end;
  end;
end;

procedure TSynJavaSyn.PointProc;
begin
  inc(Run);                            {point}
  if FLine[Run] in ['0'..'9'] then
  begin
    NumberProc;
    Exit;
  end;
  fTokenID := tkSymbol;
  FExtTokenID := xtkPoint;
end;

procedure TSynJavaSyn.PoundProc;
begin
  inc(Run);
  fTokenID := tkInvalid;
end;

procedure TSynJavaSyn.QuestionProc;
begin
  fTokenID := tkSymbol;                {question mark - conditional}
  FExtTokenID := xtkQuestion;
  inc(Run);
end;

procedure TSynJavaSyn.RemainderSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {remainder assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
        FExtTokenID := xtkRemainderAssign;
      end;
  else                                 {remainder}
    begin
      inc(Run);
      fTokenID := tkSymbol;
      FExtTokenID := xtkRemainder;
    end;
  end;
end;

procedure TSynJavaSyn.RoundCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkRoundClose;
  dec(FRoundCount);
end;

procedure TSynJavaSyn.RoundOpenProc;
begin
  inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundOpen;
  inc(FRoundCount);
end;

procedure TSynJavaSyn.SemiColonProc;
begin
  inc(Run);                            {semicolon}
  fTokenID := tkSymbol;
  FExtTokenID := xtkSemiColon;
end;

procedure TSynJavaSyn.SlashProc;
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
        if fLine[Run+2] = '*' then     {documentation comment}
        begin
          fRange := rsDocument;
          fTokenID := tkDocument;
          inc(Run);
        end
        else                           {c style comment}
        begin
          fRange := rsComment;
          fTokenID := tkComment;
        end;

        inc(Run,2);
        while fLine[Run] <> #0 do
          case fLine[Run] of
            '*':
              if fLine[Run + 1] = '/' then
              begin
                inc(Run, 2);
                fRange := rsUnknown;
                break;
              end else inc(Run);
            #10: break;
            #13: break;
          else
            inc(Run);
          end;
      end;
    '=':                               {division assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
        FExtTokenID := xtkDivideAssign;
      end;
  else                                 {division}
    begin
      inc(Run);
      fTokenID := tkSymbol;
      FExtTokenID := xtkDivide;
    end;
  end;
end;

procedure TSynJavaSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynJavaSyn.SquareCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSquareClose;
  dec(FSquareCount);
end;

procedure TSynJavaSyn.SquareOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSquareOpen;
  inc(FSquareCount);
end;

procedure TSynJavaSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
      #92: Inc(Run);  // Backslash, if we have an escaped charcter it can be skipped
    end;
    if FLine[Run] <> #0 then inc(Run); //Add check here to prevent overrun from backslash being last char
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynJavaSyn.TildeProc;
begin
  inc(Run);                            {bitwise complement}
  fTokenId := tkSymbol;
  FExtTokenID := xtkBitComplement;
end;

procedure TSynJavaSyn.XOrSymbolProc;
begin
  Case FLine[Run + 1] of
    '=':                               {xor assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
        FExtTokenID := xtkXorAssign;
      end;
  else                                 {xor}
    begin
      inc(Run);
      fTokenID := tkSymbol;
      FExtTokenID := xtkXor;
    end;
  end;
end;

procedure TSynJavaSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run,2)
  else
{$ENDIF}
  inc(Run);
  {$IFDEF SYN_LAZARUS}
  while (fLine[Run] in [#128..#191]) OR // continued utf8 subcode
   ((fLine[Run]<>#0) and (fProcTable[fLine[Run]] = @UnknownProc)) do inc(Run);
  {$ENDIF}
  fTokenID := tkUnknown;
end;

procedure TSynJavaSyn.Next;
begin
  fTokenPos := Run;
  Case fRange of
    rsComment: CommentProc;
    rsDocument: CommentProc;
  else
    begin
      fRange := rsUnknown;
      fProcTable[fLine[Run]];
    end;
  end;
end;

function TSynJavaSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;       
    else Result := nil;
  end;
end;

function TSynJavaSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynJavaSyn.GetRange: Pointer;
begin
  Result := Pointer(PtrInt(fRange));
end;

procedure TSynJavaSyn.ReSetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynJavaSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(PtrUInt(Value));
end;

function TSynJavaSyn.GetToken: String;
var
  Len: LongInt;
begin
  Result := '';
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

{$IFDEF SYN_LAZARUS}
procedure TSynJavaSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength:=Run-fTokenPos;
  TokenStart:=FLine + fTokenPos;
end;
{$ENDIF}

function TSynJavaSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynJavaSyn.GetExtTokenID: TxtkTokenKind;
begin
  Result := FExtTokenID;
end;

function TSynJavaSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkDocument: Result := fDocumentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkInvalid: Result := fInvalidAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fInvalidAttri;
    else Result := nil;
  end;
end;

function TSynJavaSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynJavaSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynJavaSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', '$', '0'..'9', 'a'..'z', 'A'..'Z'] + TSynSpecialChars;
end;

class function TSynJavaSyn.GetLanguageName: string;
begin
  Result := SYNS_LangJava;
end;

function TSynJavaSyn.GetSampleSource: string;
begin
  Result := '/* Java syntax highlighting */'#13#10 +
            'import java.util.*;'#13#10 +
            #13#10 +
            '/** Example class */'#13#10 +
            'public class Sample {'#13#10 +
            '  public static void main(String[] args) {'#13#10 +
            '    int i = 0;'#13#10 +
            '    for(i = 0; i < 10; i++)'#13#10 +
            '      System.out.println("Hello world");'#13#10 +
            '  }'#13#10 +
            '}';
end;

initialization
  MakeIdentTable;
  RegisterPlaceableHighlighter(TSynJavaSyn);

end.

