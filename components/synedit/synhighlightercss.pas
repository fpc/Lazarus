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
{$IFNDEF QSYNHIGHLIGHTERCSS}
unit SynHighlighterCss;
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
    fIdentFuncTable: array[0..255] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: String): Boolean;
    function Func16: TtkTokenKind;
    function Func18: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func24: TtkTokenKind;    
    function Func26: TtkTokenKind;
    function Func29: TtkTokenKind;
    function Func30: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func34: TtkTokenKind;            
    function Func36: TtkTokenKind;
    function Func39: TtkTokenKind;
    function Func40: TtkTokenKind;
    function Func43: TtkTokenKind;
    function Func45: TtkTokenKind;
    function Func51: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func54: TtkTokenKind;
    function Func55: TtkTokenKind;
    function Func56: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func58: TtkTokenKind;    
    function Func59: TtkTokenKind;
    function Func60: TtkTokenKind;
    function Func61: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func63: TtkTokenKind;
    function Func64: TtkTokenKind;
    function Func65: TtkTokenKind;
    function Func67: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func70: TtkTokenKind;
    function Func72: TtkTokenKind;    
    function Func74: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func79: TtkTokenKind;
    function Func80: TtkTokenKind;
    function Func81: TtkTokenKind;
    function Func82: TtkTokenKind;
    function Func83: TtkTokenKind;
    function Func85: TtkTokenKind;
    function Func86: TtkTokenKind;
    function Func87: TtkTokenKind;
    function Func88: TtkTokenKind;
    function Func90: TtkTokenKind;
    function Func91: TtkTokenKind;
    function Func93: TtkTokenKind;
    function Func94: TtkTokenKind;
    function Func95: TtkTokenKind;
    function Func96: TtkTokenKind;
    function Func97: TtkTokenKind;
    function Func98: TtkTokenKind;
    function Func99: TtkTokenKind;    
    function Func100: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func103: TtkTokenKind;
    function Func105: TtkTokenKind;
    function Func106: TtkTokenKind;
    function Func107: TtkTokenKind;
    function Func108: TtkTokenKind;
    function Func110: TtkTokenKind;
    function Func111: TtkTokenKind;
    function Func112: TtkTokenKind;
    function Func113: TtkTokenKind;    
    function Func114: TtkTokenKind;
    function Func115: TtkTokenKind;
    function Func116: TtkTokenKind;
    function Func117: TtkTokenKind;
    function Func118: TtkTokenKind;
    function Func120: TtkTokenKind;
    function Func121: TtkTokenKind;
    function Func122: TtkTokenKind;
    function Func124: TtkTokenKind;
    function Func126: TtkTokenKind;
    function Func128: TtkTokenKind;
    function Func129: TtkTokenKind;
    function Func130: TtkTokenKind;
    function Func131: TtkTokenKind;
    function Func134: TtkTokenKind;
    function Func136: TtkTokenKind;
    function Func137: TtkTokenKind;
    function Func138: TtkTokenKind;
    function Func139: TtkTokenKind;
    function Func140: TtkTokenKind;
    function Func141: TtkTokenKind;
    function Func144: TtkTokenKind;
    function Func148: TtkTokenKind;
    function Func149: TtkTokenKind;
    function Func150: TtkTokenKind;
    function Func151: TtkTokenKind;
    function Func152: TtkTokenKind;    
    function Func154: TtkTokenKind;
    function Func156: TtkTokenKind;
    function Func158: TtkTokenKind;
    function Func160: TtkTokenKind;
    function Func164: TtkTokenKind;
    function Func166: TtkTokenKind;
    function Func167: TtkTokenKind;
    function Func169: TtkTokenKind;
    function Func172: TtkTokenKind;
    function Func173: TtkTokenKind;
    function Func174: TtkTokenKind;
    function Func178: TtkTokenKind;
    function Func182: TtkTokenKind;
    function Func187: TtkTokenKind;
    function Func190: TtkTokenKind;
    function Func194: TtkTokenKind;
    function Func195: TtkTokenKind;
    function Func199: TtkTokenKind;
    function Func200: TtkTokenKind;
    function Func210: TtkTokenKind;
    function Func213: TtkTokenKind;
    function Func220: TtkTokenKind;
    function Func250: TtkTokenKind;
    procedure AsciiCharProc;
    procedure CRProc;
    procedure CStyleCommentProc;
    procedure DashProc;
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
    function GetSampleSource: String; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine({$IFDEF FPC}const {$ENDIF}NewValue: String; LineNumber: Integer); override;
    function GetToken: string; override;
    {$IFDEF SYN_LAZARUS}
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    {$ENDIF}
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ReSetRange; override;
    property IdentChars;

    function KeyHash2(ToHash: PChar): Integer;
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
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable : array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  I, J: Char;
begin
  for I := #0 to #255 do begin
    case I of
      'a'..'z', 'A'..'Z', '-', '_', '0'..'9': Identifiers[I] := True;
    else
      Identifiers[I] := False;
    end;
    J := UpCase(I);
    if I in ['a'..'z', 'A'..'Z', '-', '_'] then
      mHashTable[I] := Ord(J) - 64
    else
      mHashTable[I] := 0;
  end;
end;

procedure TSynCssSyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@FIdentFuncTable);
  for I := Low(FIdentFuncTable) to High(FIdentFuncTable) do begin
    pF^ :=  {$IFDEF FPC}@{$ENDIF}AltFunc;
    Inc(pF);
  end;
  FIdentFuncTable[16] := {$IFDEF FPC}@{$ENDIF}Func16;
  FIdentFuncTable[18] := {$IFDEF FPC}@{$ENDIF}Func18;
  FIdentFuncTable[19] := {$IFDEF FPC}@{$ENDIF}Func19;
  FIdentFuncTable[23] := {$IFDEF FPC}@{$ENDIF}Func23;
  FIdentFuncTable[24] := {$IFDEF FPC}@{$ENDIF}Func24;
  FIdentFuncTable[26] := {$IFDEF FPC}@{$ENDIF}Func26;
  FIdentFuncTable[29] := {$IFDEF FPC}@{$ENDIF}Func29;
  FIdentFuncTable[30] := {$IFDEF FPC}@{$ENDIF}Func30;
  FIdentFuncTable[32] := {$IFDEF FPC}@{$ENDIF}Func32;
  FIdentFuncTable[34] := {$IFDEF FPC}@{$ENDIF}Func34;
  FIdentFuncTable[36] := {$IFDEF FPC}@{$ENDIF}Func36;
  FIdentFuncTable[39] := {$IFDEF FPC}@{$ENDIF}Func39;
  FIdentFuncTable[40] := {$IFDEF FPC}@{$ENDIF}Func40;
  FIdentFuncTable[43] := {$IFDEF FPC}@{$ENDIF}Func43;
  FIdentFuncTable[45] := {$IFDEF FPC}@{$ENDIF}Func45;
  FIdentFuncTable[51] := {$IFDEF FPC}@{$ENDIF}Func51;
  FIdentFuncTable[52] := {$IFDEF FPC}@{$ENDIF}Func52;
  FIdentFuncTable[54] := {$IFDEF FPC}@{$ENDIF}Func54;
  FIdentFuncTable[55] := {$IFDEF FPC}@{$ENDIF}Func55;
  FIdentFuncTable[56] := {$IFDEF FPC}@{$ENDIF}Func56;
  FIdentFuncTable[57] := {$IFDEF FPC}@{$ENDIF}Func57;
  FIdentFuncTable[58] := {$IFDEF FPC}@{$ENDIF}Func58;
  FIdentFuncTable[59] := {$IFDEF FPC}@{$ENDIF}Func59;
  FIdentFuncTable[60] := {$IFDEF FPC}@{$ENDIF}Func60;
  FIdentFuncTable[61] := {$IFDEF FPC}@{$ENDIF}Func61;
  FIdentFuncTable[62] := {$IFDEF FPC}@{$ENDIF}Func62;
  FIdentFuncTable[63] := {$IFDEF FPC}@{$ENDIF}Func63;
  FIdentFuncTable[64] := {$IFDEF FPC}@{$ENDIF}Func64;
  FIdentFuncTable[65] := {$IFDEF FPC}@{$ENDIF}Func65;
  FIdentFuncTable[67] := {$IFDEF FPC}@{$ENDIF}Func67;
  FIdentFuncTable[69] := {$IFDEF FPC}@{$ENDIF}Func69;
  FIdentFuncTable[70] := {$IFDEF FPC}@{$ENDIF}Func70;
  FIdentFuncTable[72] := {$IFDEF FPC}@{$ENDIF}Func72;
  FIdentFuncTable[74] := {$IFDEF FPC}@{$ENDIF}Func74;
  FIdentFuncTable[76] := {$IFDEF FPC}@{$ENDIF}Func76;
  FIdentFuncTable[78] := {$IFDEF FPC}@{$ENDIF}Func78;
  FIdentFuncTable[79] := {$IFDEF FPC}@{$ENDIF}Func79;
  FIdentFuncTable[80] := {$IFDEF FPC}@{$ENDIF}Func80;
  FIdentFuncTable[81] := {$IFDEF FPC}@{$ENDIF}Func81;
  FIdentFuncTable[82] := {$IFDEF FPC}@{$ENDIF}Func82;
  FIdentFuncTable[83] := {$IFDEF FPC}@{$ENDIF}Func83;
  FIdentFuncTable[85] := {$IFDEF FPC}@{$ENDIF}Func85;
  FIdentFuncTable[86] := {$IFDEF FPC}@{$ENDIF}Func86;
  FIdentFuncTable[87] := {$IFDEF FPC}@{$ENDIF}Func87;
  FIdentFuncTable[88] := {$IFDEF FPC}@{$ENDIF}Func88;
  FIdentFuncTable[90] := {$IFDEF FPC}@{$ENDIF}Func90;
  FIdentFuncTable[91] := {$IFDEF FPC}@{$ENDIF}Func91;
  FIdentFuncTable[93] := {$IFDEF FPC}@{$ENDIF}Func93;
  FIdentFuncTable[94] := {$IFDEF FPC}@{$ENDIF}Func94;
  FIdentFuncTable[95] := {$IFDEF FPC}@{$ENDIF}Func95;
  FIdentFuncTable[96] := {$IFDEF FPC}@{$ENDIF}Func96;
  FIdentFuncTable[97] := {$IFDEF FPC}@{$ENDIF}Func97;
  FIdentFuncTable[98] := {$IFDEF FPC}@{$ENDIF}Func98;
  FIdentFuncTable[99] := {$IFDEF FPC}@{$ENDIF}Func99;
  FIdentFuncTable[100] := {$IFDEF FPC}@{$ENDIF}Func100;
  FIdentFuncTable[101] := {$IFDEF FPC}@{$ENDIF}Func101;
  FIdentFuncTable[102] := {$IFDEF FPC}@{$ENDIF}Func102;
  FIdentFuncTable[103] := {$IFDEF FPC}@{$ENDIF}Func103;
  FIdentFuncTable[105] := {$IFDEF FPC}@{$ENDIF}Func105;
  FIdentFuncTable[106] := {$IFDEF FPC}@{$ENDIF}Func106;
  FIdentFuncTable[107] := {$IFDEF FPC}@{$ENDIF}Func107;
  FIdentFuncTable[108] := {$IFDEF FPC}@{$ENDIF}Func108;
  FIdentFuncTable[110] := {$IFDEF FPC}@{$ENDIF}Func110;
  FIdentFuncTable[111] := {$IFDEF FPC}@{$ENDIF}Func111;
  FIdentFuncTable[112] := {$IFDEF FPC}@{$ENDIF}Func112;
  FIdentFuncTable[113] := {$IFDEF FPC}@{$ENDIF}Func113;
  FIdentFuncTable[114] := {$IFDEF FPC}@{$ENDIF}Func114;
  FIdentFuncTable[115] := {$IFDEF FPC}@{$ENDIF}Func115;
  FIdentFuncTable[116] := {$IFDEF FPC}@{$ENDIF}Func116;
  FIdentFuncTable[117] := {$IFDEF FPC}@{$ENDIF}Func117;
  FIdentFuncTable[118] := {$IFDEF FPC}@{$ENDIF}Func118;
  FIdentFuncTable[120] := {$IFDEF FPC}@{$ENDIF}Func120;
  FIdentFuncTable[121] := {$IFDEF FPC}@{$ENDIF}Func121;
  FIdentFuncTable[122] := {$IFDEF FPC}@{$ENDIF}Func122;
  FIdentFuncTable[124] := {$IFDEF FPC}@{$ENDIF}Func124;
  FIdentFuncTable[126] := {$IFDEF FPC}@{$ENDIF}Func126;
  FIdentFuncTable[128] := {$IFDEF FPC}@{$ENDIF}Func128;
  FIdentFuncTable[129] := {$IFDEF FPC}@{$ENDIF}Func129;
  FIdentFuncTable[130] := {$IFDEF FPC}@{$ENDIF}Func130;
  FIdentFuncTable[131] := {$IFDEF FPC}@{$ENDIF}Func131;
  FIdentFuncTable[134] := {$IFDEF FPC}@{$ENDIF}Func134;
  FIdentFuncTable[136] := {$IFDEF FPC}@{$ENDIF}Func136;
  FIdentFuncTable[137] := {$IFDEF FPC}@{$ENDIF}Func137;
  FIdentFuncTable[138] := {$IFDEF FPC}@{$ENDIF}Func138;
  FIdentFuncTable[139] := {$IFDEF FPC}@{$ENDIF}Func139;
  FIdentFuncTable[140] := {$IFDEF FPC}@{$ENDIF}Func140;
  FIdentFuncTable[141] := {$IFDEF FPC}@{$ENDIF}Func141;
  FIdentFuncTable[144] := {$IFDEF FPC}@{$ENDIF}Func144;
  FIdentFuncTable[148] := {$IFDEF FPC}@{$ENDIF}Func148;
  FIdentFuncTable[149] := {$IFDEF FPC}@{$ENDIF}Func149;
  FIdentFuncTable[150] := {$IFDEF FPC}@{$ENDIF}Func150;
  FIdentFuncTable[151] := {$IFDEF FPC}@{$ENDIF}Func151;
  FIdentFuncTable[152] := {$IFDEF FPC}@{$ENDIF}Func152;
  FIdentFuncTable[154] := {$IFDEF FPC}@{$ENDIF}Func154;
  FIdentFuncTable[156] := {$IFDEF FPC}@{$ENDIF}Func156;
  FIdentFuncTable[158] := {$IFDEF FPC}@{$ENDIF}Func158;
  FIdentFuncTable[160] := {$IFDEF FPC}@{$ENDIF}Func160;
  FIdentFuncTable[164] := {$IFDEF FPC}@{$ENDIF}Func164;
  FIdentFuncTable[166] := {$IFDEF FPC}@{$ENDIF}Func166;
  FIdentFuncTable[167] := {$IFDEF FPC}@{$ENDIF}Func167;
  FIdentFuncTable[169] := {$IFDEF FPC}@{$ENDIF}Func169;
  FIdentFuncTable[172] := {$IFDEF FPC}@{$ENDIF}Func172;
  FIdentFuncTable[173] := {$IFDEF FPC}@{$ENDIF}Func173;
  FIdentFuncTable[174] := {$IFDEF FPC}@{$ENDIF}Func174;
  FIdentFuncTable[178] := {$IFDEF FPC}@{$ENDIF}Func178;
  FIdentFuncTable[182] := {$IFDEF FPC}@{$ENDIF}Func182;
  FIdentFuncTable[187] := {$IFDEF FPC}@{$ENDIF}Func187;
  FIdentFuncTable[190] := {$IFDEF FPC}@{$ENDIF}Func190;
  FIdentFuncTable[194] := {$IFDEF FPC}@{$ENDIF}Func194;
  FIdentFuncTable[195] := {$IFDEF FPC}@{$ENDIF}Func195;
  FIdentFuncTable[199] := {$IFDEF FPC}@{$ENDIF}Func199;
  FIdentFuncTable[200] := {$IFDEF FPC}@{$ENDIF}Func200;
  FIdentFuncTable[210] := {$IFDEF FPC}@{$ENDIF}Func210;
  FIdentFuncTable[213] := {$IFDEF FPC}@{$ENDIF}Func213;
  FIdentFuncTable[220] := {$IFDEF FPC}@{$ENDIF}Func220;
  FIdentFuncTable[250] := {$IFDEF FPC}@{$ENDIF}Func250;
end;

function TSynCssSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['a'..'z', 'A'..'Z', '0'..'9', '-', '_'] do begin
    Inc(Result, mHashTable[ToHash^]);
    Inc(ToHash);
  end;
  FStringLen := ToHash - FToIdent;
end;

function TSynCssSyn.KeyComp(const aKey: string): Boolean;
var
  iI  : Integer;
  Temp: PChar;
begin
  Temp := FToIdent;
  if Length(aKey) = FStringLen then begin
    Result := True;
    for iI := 1 to fStringLen do begin
      if mHashTable[Temp^] <> mHashTable[aKey[iI]] then begin
        Result := False;
        Break;
      end;
      Inc(Temp);
    end;
  end else
    Result := False;
end;


function TSynCssSyn.Func16: TtkTokenKind;
begin
  if KeyComp('cm') or KeyComp('deg') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func18: TtkTokenKind;
begin
  if KeyComp('em') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func19: TtkTokenKind;
begin
  if KeyComp('pc') or KeyComp('s') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func23: TtkTokenKind;
begin
  if KeyComp('in') or KeyComp('rad') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func24: TtkTokenKind;
begin
  if KeyComp('b-box') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func26: TtkTokenKind;
begin
  if KeyComp('mm') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func29: TtkTokenKind;
begin
  if KeyComp('page') or KeyComp('cue') or KeyComp('ex') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func30: TtkTokenKind;
begin
  if KeyComp('grad') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func32: TtkTokenKind;
begin
  if KeyComp('ms') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func34: TtkTokenKind;
begin
  if KeyComp('Hz') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func36: TtkTokenKind;
begin
  if KeyComp('pt') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func39: TtkTokenKind;
begin
  if KeyComp('clear') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func40: TtkTokenKind;
begin
  if KeyComp('px') or KeyComp('clip') or KeyComp('src') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func43: TtkTokenKind;
begin
  if KeyComp('left') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func45: TtkTokenKind;
begin
  if KeyComp('ime-mode') or KeyComp('kHz') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func51: TtkTokenKind;
begin
  if KeyComp('top') or KeyComp('panose-1') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func52: TtkTokenKind;
begin
  if KeyComp('speak') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func54: TtkTokenKind;
begin
  if KeyComp('float') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func55: TtkTokenKind;
begin
  if KeyComp('font') or KeyComp('padding') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func56: TtkTokenKind;
begin
  if KeyComp('pitch') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func57: TtkTokenKind;
begin
  if KeyComp('height') or KeyComp('run-in') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func58: TtkTokenKind;
begin
  if KeyComp('line-break') or KeyComp('cap-height') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func59: TtkTokenKind;
begin
  if KeyComp('size') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func60: TtkTokenKind;
begin
  if KeyComp('cue-after') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func61: TtkTokenKind;
begin
  if KeyComp('cue-before') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func62: TtkTokenKind;
begin
  if KeyComp('margin') or KeyComp('pause') or KeyComp('right') or
    KeyComp('marks') or KeyComp('border') or KeyComp('x-height') or
    KeyComp('ascent') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func63: TtkTokenKind;
begin
  if KeyComp('color') or KeyComp('z-index') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func64: TtkTokenKind;
begin
  if KeyComp('width') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func65: TtkTokenKind;
begin
  if KeyComp('stemh') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func67: TtkTokenKind;
begin
  if KeyComp('slope') or KeyComp('baseline') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func69: TtkTokenKind;
begin
  if KeyComp('zoom') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func70: TtkTokenKind;
begin
  if KeyComp('filter') or KeyComp('descent') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func72: TtkTokenKind;
begin
  if KeyComp('top-line') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func74: TtkTokenKind;
begin
  if KeyComp('speak-header') or KeyComp('min-height') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func76: TtkTokenKind;
begin
  if KeyComp('max-height') or KeyComp('unicode-bidi') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func78: TtkTokenKind;
begin
  if KeyComp('page-break-after') or KeyComp('word-break') or
    KeyComp('line-height') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func79: TtkTokenKind;
begin
  if KeyComp('padding-left') or KeyComp('page-break-before') or
    KeyComp('stemv') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func80: TtkTokenKind;
begin
  if KeyComp('behavior') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func81: TtkTokenKind;
begin
  if KeyComp('speech-rate') or KeyComp('min-width') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func82: TtkTokenKind;
begin
  if KeyComp('pitch-range') or KeyComp('mathline') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func83: TtkTokenKind;
begin
  if KeyComp('max-width') or KeyComp('widths') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func85: TtkTokenKind;
begin
  if KeyComp('bottom') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func86: TtkTokenKind;
begin
  if KeyComp('margin-left') or KeyComp('border-left') or KeyComp('display') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func87: TtkTokenKind;
begin
  if KeyComp('padding-top') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func88: TtkTokenKind;
begin
  if KeyComp('page-break-inside') or KeyComp('volume') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func90: TtkTokenKind;
begin
  if KeyComp('white-space') or KeyComp('ruby-align') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func91: TtkTokenKind;
begin
  if KeyComp('orphans') or KeyComp('content') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func93: TtkTokenKind;
begin
  if KeyComp('text-align') or KeyComp('pause-after') or KeyComp('widows') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func94: TtkTokenKind;
begin
  if KeyComp('margin-top') or KeyComp('border-top') or KeyComp('pause-before') or
    KeyComp('cursor') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func95: TtkTokenKind;
begin
  if KeyComp('font-size') or KeyComp('richness') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func96: TtkTokenKind;
begin
  if KeyComp('background') or KeyComp('caption-side') or KeyComp('counter') or
    KeyComp('outline') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func97: TtkTokenKind;
begin
  if KeyComp('quotes') or KeyComp('direction') or KeyComp('unicode-range') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func98: TtkTokenKind;
begin
  if KeyComp('padding-right') or KeyComp('azimuth') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func99: TtkTokenKind;
begin
  if KeyComp('word-wrap') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func100: TtkTokenKind;
begin
  if KeyComp('stress') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func101: TtkTokenKind;
begin
  if KeyComp('voice-family') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func102: TtkTokenKind;
begin
  if KeyComp('font-family') or KeyComp('units-per-em') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func103: TtkTokenKind;
begin
  if KeyComp('elevation') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func105: TtkTokenKind;
begin
  if KeyComp('margin-right') or KeyComp('border-right') or
    KeyComp('centerline') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func106: TtkTokenKind;
begin
  if KeyComp('border-color') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func107: TtkTokenKind;
begin
  if KeyComp('border-width') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func108: TtkTokenKind;
begin
  if KeyComp('font-weight') or KeyComp('play-during') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func110: TtkTokenKind;
begin
  if KeyComp('word-spacing') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func111: TtkTokenKind;
begin
  if KeyComp('empty-cells') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func112: TtkTokenKind;
begin
  if KeyComp('background-image') or KeyComp('border-spacing') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func113: TtkTokenKind;
begin
  if KeyComp('layout-grid') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func114: TtkTokenKind;
begin
  if KeyComp('vertical-align') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func115: TtkTokenKind;
begin
  if KeyComp('table-layout') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func116: TtkTokenKind;
begin
  if KeyComp('text-indent') or KeyComp('overflow') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func117: TtkTokenKind;
begin
  if KeyComp('font-style') or KeyComp('position') or KeyComp('speak-numeral') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func118: TtkTokenKind;
begin
  if KeyComp('marker-offset') or KeyComp('writing-mode') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func120: TtkTokenKind;
begin
  if KeyComp('text-shadow') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func121: TtkTokenKind;
begin
  if KeyComp('font-variant') or KeyComp('padding-bottom') or KeyComp('overflow-x') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func122: TtkTokenKind;
begin
  if KeyComp('list-style') or KeyComp('overflow-y') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func124: TtkTokenKind;
begin
  if KeyComp('border-style') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func126: TtkTokenKind;
begin
  if KeyComp('border-collapse') or KeyComp('definition-src') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func128: TtkTokenKind;
begin
  if KeyComp('margin-bottom') or KeyComp('border-bottom') or
    KeyComp('text-kashida-space') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func129: TtkTokenKind;
begin
  if KeyComp('font-stretch') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func130: TtkTokenKind;
begin
  if KeyComp('border-left-color') or KeyComp('letter-spacing') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func131: TtkTokenKind;
begin
  if KeyComp('border-left-width') or KeyComp('layout-grid-mode') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func134: TtkTokenKind;
begin
  if KeyComp('layout-grid-line') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func136: TtkTokenKind;
begin
  if KeyComp('visibility') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func137: TtkTokenKind;
begin
  if KeyComp('ruby-overhang') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func138: TtkTokenKind;
begin
  if KeyComp('border-top-color') or KeyComp('list-style-image') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func139: TtkTokenKind;
begin
  if KeyComp('border-top-width') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func140: TtkTokenKind;
begin
  if KeyComp('background-color') or KeyComp('outline-color') or
    KeyComp('scrollbar-face-color') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func141: TtkTokenKind;
begin
  if KeyComp('outline-width') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func144: TtkTokenKind;
begin
  if KeyComp('counter-reset') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func148: TtkTokenKind;
begin
  if KeyComp('border-left-style') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func149: TtkTokenKind;
begin
  if KeyComp('border-right-color') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func150: TtkTokenKind;
begin
  if KeyComp('border-right-width') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func151: TtkTokenKind;
begin
  if KeyComp('font-size-adjust') or KeyComp('text-autospace') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func152: TtkTokenKind;
begin
  if KeyComp('scrollbar-base-color') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func154: TtkTokenKind;
begin
  if KeyComp('text-decoration') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func156: TtkTokenKind;
begin
  if KeyComp('border-top-style') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func158: TtkTokenKind;
begin
  if KeyComp('outline-style') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func160: TtkTokenKind;
begin
  if KeyComp('layout-grid-type') or KeyComp('text-justify') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func164: TtkTokenKind;
begin
  if KeyComp('ruby-position') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func166: TtkTokenKind;
begin
  if KeyComp('scrollbar-3d-light-color') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func167: TtkTokenKind;
begin
  if KeyComp('border-right-style') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func169: TtkTokenKind;
begin
  if KeyComp('list-style-type') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func172: TtkTokenKind;
begin
  if KeyComp('border-bottom-color') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func173: TtkTokenKind;
begin
  if KeyComp('border-bottom-width') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func174: TtkTokenKind;
begin
  if KeyComp('text-transform') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func178: TtkTokenKind;
begin
  if KeyComp('counter-increment') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func182: TtkTokenKind;
begin
  if KeyComp('background-attachment') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func187: TtkTokenKind;
begin
  if KeyComp('speak-punctuation') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func190: TtkTokenKind;
begin
  if KeyComp('border-bottom-style') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func194: TtkTokenKind;
begin
  if KeyComp('background-position') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func195: TtkTokenKind;
begin
  if KeyComp('scrollbar-shadow-color') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func199: TtkTokenKind;
begin
  if KeyComp('background-position-x') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func200: TtkTokenKind;
begin
  if KeyComp('background-position-y') or KeyComp('scrollbar-arrow-color') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func210: TtkTokenKind;
begin
  if KeyComp('scrollbar-dark-shadow-color') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func213: TtkTokenKind;
begin
  if KeyComp('scrollbar-highlight-color') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func220: TtkTokenKind;
begin
  if KeyComp('list-style-position') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func250: TtkTokenKind;
begin
  if KeyComp('text-underline-position') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynCssSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  FToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if (HashKey >= 16) and (HashKey <= 250) then
    Result := fIdentFuncTable[HashKey]{$IFDEF FPC}(){$ENDIF}
  else
    Result := tkIdentifier;
end;

procedure TSynCssSyn.MakeMethodTables;
var
  chI: Char;
begin
  for chI := #0 to #255 do
    case chI of
      '{', '}'                    : FProcTable[chI] := {$IFDEF FPC}@{$ENDIF}AsciiCharProc;
      #13                         : FProcTable[chI] := {$IFDEF FPC}@{$ENDIF}CRProc;
      '-'                         : FProcTable[chI] := {$IFDEF FPC}@{$ENDIF}DashProc;
      'A'..'Z', 'a'..'z', '_'     : FProcTable[chI] := {$IFDEF FPC}@{$ENDIF}IdentProc;
      '#', '$'                    : FProcTable[chI] := {$IFDEF FPC}@{$ENDIF}IntegerProc;
      #10                         : FProcTable[chI] := {$IFDEF FPC}@{$ENDIF}LFProc;
      #0                          : FProcTable[chI] := {$IFDEF FPC}@{$ENDIF}NullProc;
      '0'..'9'                    : FProcTable[chI] := {$IFDEF FPC}@{$ENDIF}NumberProc;
      ')', '('                    : FProcTable[chI] := {$IFDEF FPC}@{$ENDIF}RoundOpenProc;
      '/'                         : FProcTable[chI] := {$IFDEF FPC}@{$ENDIF}SlashProc;
      #1..#9, #11, #12, #14..#32  : FProcTable[chI] := {$IFDEF FPC}@{$ENDIF}SpaceProc;
      #39                         : FProcTable[chI] := {$IFDEF FPC}@{$ENDIF}StringProc;
    else
      FProcTable[chI] := {$IFDEF FPC}@{$ENDIF}UnknownProc;
    end;
end;

constructor TSynCssSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_XML_AttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_XML_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey, SYNS_XML_AttrKey);
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
  SetAttributesOnChange({$IFDEF FPC}@{$ENDIF}DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterCSS;
  fRange := rsUnknown;
end;

procedure TSynCssSyn.SetLine({$IFDEF FPC}const {$ENDIF}NewValue: String; LineNumber: Integer);
begin
  inherited;
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynCssSyn.AsciiCharProc;
begin
  FTokenID := tkString;
  Inc(Run);
  while FLine[Run] in ['0'..'9'] do
    Inc(Run);
end;

procedure TSynCssSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynCssSyn.CStyleCommentProc;
begin
  if FLine[Run] = #0 then
    FTokenID := tkNull
  else begin
    FTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then begin
        FRange := rsUnKnown;
        Inc(Run, 2);
        Break;
      end;
      Inc(Run);
    until FLine[Run] in [#0, #10, #13];
  end;
end;

procedure TSynCssSyn.DashProc;
begin
  if (FLine[Run - 1] = ' ') then begin
    if (FLine[Run + 1] in ['0'..'9', 'a'..'z', 'A'..'Z']) then
      FTokenID := tkNumber
    else if (FLine[Run + 1] = '.') and (FLine[Run + 2] in ['0'..'9', 'a'..'z', 'A'..'Z']) then begin
      FTokenID := tkNumber;
      Inc(Run);
    end;
  end else
    FTokenID := tkIdentifier;
  Inc(Run);
end;

procedure TSynCssSyn.IdentProc;
begin
  FTokenID := IdentKind((FLine + Run));
  Inc(Run, FStringLen);
  while Identifiers[FLine[Run]] do
    Inc(Run);
end;

procedure TSynCssSyn.IntegerProc;
begin
  Inc(Run);
  FTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', 'A'..'F', 'a'..'f'] do
    Inc(Run);
end;

procedure TSynCssSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynCssSyn.NullProc;
begin
  FTokenID := tkNull;
end;

procedure TSynCssSyn.NumberProc;
begin
  Inc(Run);
  FTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'e', 'E'] do begin
    if ((FLine[Run] = '.') and (FLine[Run + 1] = '.')) or
       ((FLine[Run] = 'e') and ((FLine[Run + 1] = 'x') or (FLine[Run + 1] = 'm'))) then
      Break;
    Inc(Run);
  end;
end;

procedure TSynCssSyn.RoundOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynCssSyn.SlashProc;
begin
  Inc(Run);
  if fLine[Run] = '*' then begin
    FTokenID := tkComment;
    FRange := rsCStyle;
    Inc(Run);
    if not (FLine[Run] in [#0, #10, #13]) then
      CStyleCommentProc;
  end else
    FTokenID := tkSymbol;
end;

procedure TSynCssSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do
    Inc(Run);
end;

procedure TSynCssSyn.StringProc;
begin
  FTokenID := tkString;
  if (FLine[Run + 1] = #39) and (FLine[Run + 2] = #39) then
    Inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: Break;
    end;
    Inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then
    Inc(Run);
end;

procedure TSynCssSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run,2)
  else
{$ENDIF}
  Inc(Run);
  {$IFDEF SYN_LAZARUS}
  while (fLine[Run] in [#128..#191]) OR // continued utf8 subcode
   ((fLine[Run]<>#0) and (fProcTable[fLine[Run]] = @UnknownProc)) do inc(Run);
  {$ENDIF}
  FTokenID := tkUnknown;
end;

procedure TSynCssSyn.Next;
begin
  FTokenPos := Run;
  if FRange = rsCStyle then
    CStyleCommentProc
  else
    FProcTable[FLine[Run]]{$IFDEF FPC}(){$ENDIF};
end;

function TSynCssSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT   : Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD   : Result := FKeyAttri;
    SYN_ATTR_STRING    : Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL    : Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynCssSyn.GetEol: Boolean;
begin
  Result := (FTokenID = tkNull);
end;

function TSynCssSyn.GetRange: Pointer;
begin
  Result := Pointer(PtrInt(fRange));
end;

function TSynCssSyn.GetToken: string;
var
  Len: LongInt;
begin
  Result := '';
  Len := Run - FTokenPos;
  SetString(Result, (FLine + FTokenPos), Len);
end;


{$IFDEF SYN_LAZARUS}
procedure TSynCssSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);

begin
  TokenLength:=Run-fTokenPos;
  TokenStart:=FLine + fTokenPos;
end;
{$ENDIF}

function TSynCssSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenId;
end;

function TSynCssSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment   : Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey       : Result := FKeyAttri;
    tkNumber    : Result := FNumberAttri;
    tkSpace     : Result := FSpaceAttri;
    tkString    : Result := FStringAttri;
    tkSymbol    : Result := FSymbolAttri;
    tkUnknown   : Result := FIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynCssSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynCssSyn.GetTokenPos: Integer;
begin
  Result := FTokenPos;
end;

procedure TSynCssSyn.ReSetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynCssSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(PtrUInt(Value));
end;

function TSynCssSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars + ['-'];
end;

class function TSynCssSyn.GetLanguageName: string;
begin
  Result := SYNS_LangCSS;
end;

function TSynCssSyn.GetSampleSource: String;
begin
  Result := '/* Syntax highlighting */'#13#10 +
    'body'#13#10 +
    '{'#13#10 +
    '  font-family: "Arial";'#13#10 +
    '  font-size: 12pt;'#13#10 +
    '}';
end;

function TSynCSSSyn.KeyHash2(ToHash: PChar): Integer;
begin
  Result := KeyHash(ToHash);
end;


initialization
  MakeIdentTable;
  RegisterPlaceableHighlighter(TSynCssSyn);

end.
