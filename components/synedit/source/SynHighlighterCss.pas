{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterCSS.pas, released 2000-04-14.
The Original Code is based on the mwCSSSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Tony De Buys.
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
@abstract(Provides a CSS syntax highlighter for SynEdit)
@author(Tony de Buys)
@created(1999)
@lastmod(2000-06-23)
The SynHighlighterCss unit provides SynEdit with a Cascading Style Sheets syntax highlighter.
Thanks to Martin Waldenburg.
}
unit SynHighlighterCss;

{$I SynEdit.inc}

interface

uses
  SysUtils, Windows, Messages, Classes, Controls, Graphics, Registry,
  SynEditTypes, SynEditHighlighter;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkUnknown);

  TRangeState = (rsUnknown, rsCStyle);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

type
  TSynCssSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..138] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: String): Boolean;
    function Func15: TtkTokenKind;
    function Func18: TtkTokenKind;
    function Func36: TtkTokenKind;
    function Func40: TtkTokenKind;
    function Func43: TtkTokenKind;
    function Func49: TtkTokenKind;
    function Func51: TtkTokenKind;
    function Func55: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func59: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func63: TtkTokenKind;
    function Func64: TtkTokenKind;
    function Func66: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func72: TtkTokenKind;
    function Func85: TtkTokenKind;
    function Func90: TtkTokenKind;
    function Func96: TtkTokenKind;
    function Func116: TtkTokenKind;
    function Func117: TtkTokenKind;
    function Func138: TtkTokenKind;
    procedure AsciiCharProc;
    procedure CRProc;
    procedure CStyleCommentProc;
    procedure IdentProc;
    procedure IntegerProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure RoundOpenProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
  protected
    function GetIdentChars: TSynIdentChars; override;
  public
    {$IFNDEF SYN_CPPB_1} class {$ENDIF}                                         //mh 2000-07-14
    function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEOL: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(NewValue: String; LineNumber: Integer); override;
    function GetToken: string; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ReSetRange; override;
    property IdentChars;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
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
  I, J: Char;
begin
  for I := #0 to #255 do
  begin
    Case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
      else Identifiers[I] := False;
    end;
    J := UpCase(I);
    Case I in ['_', 'A'..'Z', 'a'..'z'] of
      True: mHashTable[I] := Ord(J) - 64
      else mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynCssSyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do begin
    pF^ := AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[15] := Func15;
  fIdentFuncTable[18] := Func18;
  fIdentFuncTable[36] := Func36;
  fIdentFuncTable[40] := Func40;
  fIdentFuncTable[43] := Func43;
  fIdentFuncTable[49] := Func49;
  fIdentFuncTable[51] := Func51;
  fIdentFuncTable[55] := Func55;
  fIdentFuncTable[57] := Func57;
  fIdentFuncTable[59] := Func59;
  fIdentFuncTable[62] := Func62;
  fIdentFuncTable[63] := Func63;
  fIdentFuncTable[64] := Func64;
  fIdentFuncTable[66] := Func66;
  fIdentFuncTable[69] := Func69;
  fIdentFuncTable[72] := Func72;
  fIdentFuncTable[85] := Func85;
  fIdentFuncTable[90] := Func90;
  fIdentFuncTable[96] := Func96;
  fIdentFuncTable[116] := Func116;
  fIdentFuncTable[117] := Func117;
  fIdentFuncTable[138] := Func138;
end;

function TSynCssSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

function TSynCssSyn.KeyComp(const aKey: String): Boolean;
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

function TSynCssSyn.Func15: TtkTokenKind;
begin
  if KeyComp('face') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCssSyn.Func18: TtkTokenKind;
begin
  if KeyComp('em') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCssSyn.Func36: TtkTokenKind;
begin
  if KeyComp('pt') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCssSyn.Func40: TtkTokenKind;
begin
  if KeyComp('px') then Result := tkKey else
    if KeyComp('line') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCssSyn.Func43: TtkTokenKind;
begin
  if KeyComp('left') then Result := tkKey else
    if KeyComp('align') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCssSyn.Func49: TtkTokenKind;
begin
  if KeyComp('shape') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCssSyn.Func51: TtkTokenKind;
begin
  if KeyComp('top') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCssSyn.Func55: TtkTokenKind;
begin
  if KeyComp('padding') then Result := tkKey else
    if KeyComp('font') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCssSyn.Func57: TtkTokenKind;
begin
  if KeyComp('height') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCssSyn.Func59: TtkTokenKind;
begin
  if KeyComp('size') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCssSyn.Func62: TtkTokenKind;
begin
  if KeyComp('right') then Result := tkKey else
    if KeyComp('margin') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCssSyn.Func63: TtkTokenKind;
begin
  if KeyComp('color') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCssSyn.Func64: TtkTokenKind;
begin
  if KeyComp('width') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCssSyn.Func66: TtkTokenKind;
begin
  if KeyComp('family') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCssSyn.Func69: TtkTokenKind;
begin
  if KeyComp('text') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCssSyn.Func72: TtkTokenKind;
begin
  if KeyComp('weight') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCssSyn.Func85: TtkTokenKind;
begin
  if KeyComp('bottom') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCssSyn.Func90: TtkTokenKind;
begin
  if KeyComp('vertical') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCssSyn.Func96: TtkTokenKind;
begin
  if KeyComp('background') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCssSyn.Func116: TtkTokenKind;
begin
  if KeyComp('overflow') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCssSyn.Func117: TtkTokenKind;
begin
  if KeyComp('position') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCssSyn.Func138: TtkTokenKind;
begin
  if KeyComp('horizontal') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCssSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynCssSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 139 then Result := fIdentFuncTable[HashKey] else Result := tkIdentifier;
end;

procedure TSynCssSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '{', '}': fProcTable[I] := AsciiCharProc;
      #13: fProcTable[I] := CRProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := IdentProc;
      '#', '$': fProcTable[I] := IntegerProc;
      #10: fProcTable[I] := LFProc;
      #0: fProcTable[I] := NullProc;
      '0'..'9': fProcTable[I] := NumberProc;
      ')', '(': fProcTable[I] := RoundOpenProc;
      '/': fProcTable[I] := SlashProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := SpaceProc;
      #39: fProcTable[I] := StringProc;
      else fProcTable[I] := UnknownProc;
    end;
end;

constructor TSynCssSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterCSS;
  fRange := rsUnknown;
end;

procedure TSynCssSyn.SetLine(NewValue: String; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynCssSyn.AsciiCharProc;
begin
  fTokenID := tkString;
  inc(Run);
  while FLine[Run] in ['0'..'9'] do inc(Run);
end;

procedure TSynCssSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then inc(Run);
end;

procedure TSynCssSyn.CStyleCommentProc;
begin
  if fLine[Run] = #0 then
    fTokenID := tkNull
  else begin
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then begin
        fRange := rsUnKnown;
        inc(Run, 2);
        break;
      end;
      inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end;
end;

procedure TSynCssSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynCssSyn.IntegerProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', 'A'..'F', 'a'..'f'] do inc(Run);
end;

procedure TSynCssSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynCssSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynCssSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'e', 'E'] do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
end;

procedure TSynCssSyn.RoundOpenProc;
begin
  inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynCssSyn.SlashProc;
begin
  inc(Run);
  if fLine[Run] = '*' then begin
    fTokenID := tkComment;
    fRange := rsCStyle;
    inc(Run);
    if not (fLine[Run] in [#0, #10, #13]) then
      CStyleCommentProc;
  end else
    fTokenID := tkSymbol;
end;

procedure TSynCssSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynCssSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #39) and (FLine[Run + 2] = #39) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynCssSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynCssSyn.Next;
begin
  fTokenPos := Run;
  if fRange = rsCStyle then
    CStyleCommentProc
  else
    fProcTable[fLine[Run]];
end;

function TSynCssSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    else Result := nil;
  end;
end;

function TSynCssSyn.GetEOL: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynCssSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynCssSyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynCssSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynCssSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynCssSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynCssSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynCssSyn.ReSetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynCssSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynCssSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}                                             //mh 2000-07-14
function TSynCssSyn.GetLanguageName: string;
begin
  Result := SYNS_LangCSS;
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}                                                            //mh 2000-07-14
  RegisterPlaceableHighlighter(TSynCssSyn);
{$ENDIF}
end.
