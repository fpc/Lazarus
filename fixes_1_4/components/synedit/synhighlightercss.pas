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

  SysUtils, Classes,
  LCLIntf, LCLType,
  Controls, Graphics,
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
    fIdentFuncTable: array[0..275] of TIdentFuncTableFunc;
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
    function Func41: TtkTokenKind;
    function Func43: TtkTokenKind;
    function Func45: TtkTokenKind;
    function Func51: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func53: TtkTokenKind;
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
    function Func71: TtkTokenKind;
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
    function Func89: TtkTokenKind;
    function Func90: TtkTokenKind;
    function Func91: TtkTokenKind;
    function Func92: TtkTokenKind;
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
    function Func109: TtkTokenKind;
    function Func110: TtkTokenKind;
    function Func111: TtkTokenKind;
    function Func112: TtkTokenKind;
    function Func113: TtkTokenKind;    
    function Func114: TtkTokenKind;
    function Func115: TtkTokenKind;
    function Func116: TtkTokenKind;
    function Func117: TtkTokenKind;
    function Func118: TtkTokenKind;
    function Func119: TtkTokenKind;
    function Func120: TtkTokenKind;
    function Func121: TtkTokenKind;
    function Func122: TtkTokenKind;
    function Func123: TtkTokenKind;
    function Func124: TtkTokenKind;
    function Func126: TtkTokenKind;
    function Func127: TtkTokenKind;
    function Func128: TtkTokenKind;
    function Func129: TtkTokenKind;
    function Func130: TtkTokenKind;
    function Func131: TtkTokenKind;
    function Func132: TtkTokenKind;
    function Func134: TtkTokenKind;
    function Func136: TtkTokenKind;
    function Func137: TtkTokenKind;
    function Func138: TtkTokenKind;
    function Func139: TtkTokenKind;
    function Func140: TtkTokenKind;
    function Func141: TtkTokenKind;
    function Func142: TtkTokenKind;
    function Func144: TtkTokenKind;
    function Func146: TtkTokenKind;
    function Func148: TtkTokenKind;
    function Func149: TtkTokenKind;
    function Func150: TtkTokenKind;
    function Func151: TtkTokenKind;
    function Func152: TtkTokenKind;
    function Func153: TtkTokenKind;
    function Func154: TtkTokenKind;
    function Func156: TtkTokenKind;
    function Func158: TtkTokenKind;
    function Func159: TtkTokenKind;
    function Func160: TtkTokenKind;
    function Func164: TtkTokenKind;
    function Func166: TtkTokenKind;
    function Func167: TtkTokenKind;
    function Func169: TtkTokenKind;
    function Func171: TtkTokenKind;
    function Func172: TtkTokenKind;
    function Func173: TtkTokenKind;
    function Func174: TtkTokenKind;
    function Func177: TtkTokenKind;
    function Func178: TtkTokenKind;
    function Func179: TtkTokenKind;
    function Func182: TtkTokenKind;
    function Func186: TtkTokenKind;
    function Func187: TtkTokenKind;
    function Func190: TtkTokenKind;
    function Func191: TtkTokenKind;
    function Func194: TtkTokenKind;
    function Func195: TtkTokenKind;
    function Func199: TtkTokenKind;
    function Func200: TtkTokenKind;
    function Func205: TtkTokenKind;
    function Func210: TtkTokenKind;
    function Func213: TtkTokenKind;
    function Func220: TtkTokenKind;
    function Func222: TtkTokenKind;
    function Func224: TtkTokenKind;
    function Func232: TtkTokenKind;
    function Func242: TtkTokenKind;
    function Func250: TtkTokenKind;
    function Func253: TtkTokenKind;
    function Func275: TtkTokenKind;
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
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
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
  SynEditStrConst;

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable : array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  I, J: Char;
begin
  for I := #0 to #255 do begin
    case I of
      'a'..'z', 'A'..'Z', '-', '_', '0'..'9','@': Identifiers[I] := True;
    else
      Identifiers[I] := False;
    end;
    J := UpCase(I);
    if I in ['a'..'z', 'A'..'Z', '-', '_','@'] then
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
    pF^ :=  @AltFunc;
    Inc(pF);
  end;
  FIdentFuncTable[16] := @Func16;
  FIdentFuncTable[18] := @Func18;
  FIdentFuncTable[19] := @Func19;
  FIdentFuncTable[23] := @Func23;
  FIdentFuncTable[24] := @Func24;
  FIdentFuncTable[26] := @Func26;
  FIdentFuncTable[29] := @Func29;
  FIdentFuncTable[30] := @Func30;
  FIdentFuncTable[32] := @Func32;
  FIdentFuncTable[34] := @Func34;
  FIdentFuncTable[36] := @Func36;
  FIdentFuncTable[39] := @Func39;
  FIdentFuncTable[40] := @Func40;
  FIdentFuncTable[41] := @Func41;
  FIdentFuncTable[43] := @Func43;
  FIdentFuncTable[45] := @Func45;
  FIdentFuncTable[51] := @Func51;
  FIdentFuncTable[52] := @Func52;
  FIdentFuncTable[53] := @Func53;
  FIdentFuncTable[54] := @Func54;
  FIdentFuncTable[55] := @Func55;
  FIdentFuncTable[56] := @Func56;
  FIdentFuncTable[57] := @Func57;
  FIdentFuncTable[58] := @Func58;
  FIdentFuncTable[59] := @Func59;
  FIdentFuncTable[60] := @Func60;
  FIdentFuncTable[61] := @Func61;
  FIdentFuncTable[62] := @Func62;
  FIdentFuncTable[63] := @Func63;
  FIdentFuncTable[64] := @Func64;
  FIdentFuncTable[65] := @Func65;
  FIdentFuncTable[67] := @Func67;
  FIdentFuncTable[69] := @Func69;
  FIdentFuncTable[70] := @Func70;
  FIdentFuncTable[71] := @Func71;
  FIdentFuncTable[72] := @Func72;
  FIdentFuncTable[74] := @Func74;
  FIdentFuncTable[76] := @Func76;
  FIdentFuncTable[78] := @Func78;
  FIdentFuncTable[79] := @Func79;
  FIdentFuncTable[80] := @Func80;
  FIdentFuncTable[81] := @Func81;
  FIdentFuncTable[82] := @Func82;
  FIdentFuncTable[83] := @Func83;
  FIdentFuncTable[85] := @Func85;
  FIdentFuncTable[86] := @Func86;
  FIdentFuncTable[87] := @Func87;
  FIdentFuncTable[88] := @Func88;
  FIdentFuncTable[89] := @Func89;
  FIdentFuncTable[90] := @Func90;
  FIdentFuncTable[91] := @Func91;
  FIdentFuncTable[92] := @Func92;
  FIdentFuncTable[93] := @Func93;
  FIdentFuncTable[94] := @Func94;
  FIdentFuncTable[95] := @Func95;
  FIdentFuncTable[96] := @Func96;
  FIdentFuncTable[97] := @Func97;
  FIdentFuncTable[98] := @Func98;
  FIdentFuncTable[99] := @Func99;
  FIdentFuncTable[100] := @Func100;
  FIdentFuncTable[101] := @Func101;
  FIdentFuncTable[102] := @Func102;
  FIdentFuncTable[103] := @Func103;
  FIdentFuncTable[105] := @Func105;
  FIdentFuncTable[106] := @Func106;
  FIdentFuncTable[107] := @Func107;
  FIdentFuncTable[108] := @Func108;
  FIdentFuncTable[109] := @Func109;
  FIdentFuncTable[110] := @Func110;
  FIdentFuncTable[111] := @Func111;
  FIdentFuncTable[112] := @Func112;
  FIdentFuncTable[113] := @Func113;
  FIdentFuncTable[114] := @Func114;
  FIdentFuncTable[115] := @Func115;
  FIdentFuncTable[116] := @Func116;
  FIdentFuncTable[117] := @Func117;
  FIdentFuncTable[118] := @Func118;
  FIdentFuncTable[119] := @Func119;
  FIdentFuncTable[120] := @Func120;
  FIdentFuncTable[121] := @Func121;
  FIdentFuncTable[122] := @Func122;
  FIdentFuncTable[123] := @Func123;
  FIdentFuncTable[124] := @Func124;
  FIdentFuncTable[126] := @Func126;
  FIdentFuncTable[127] := @Func127;
  FIdentFuncTable[128] := @Func128;
  FIdentFuncTable[129] := @Func129;
  FIdentFuncTable[130] := @Func130;
  FIdentFuncTable[131] := @Func131;
  FIdentFuncTable[132] := @Func132;
  FIdentFuncTable[134] := @Func134;
  FIdentFuncTable[136] := @Func136;
  FIdentFuncTable[137] := @Func137;
  FIdentFuncTable[138] := @Func138;
  FIdentFuncTable[139] := @Func139;
  FIdentFuncTable[140] := @Func140;
  FIdentFuncTable[141] := @Func141;
  FIdentFuncTable[142] := @Func142;
  FIdentFuncTable[144] := @Func144;
  FIdentFuncTable[146] := @Func146;
  FIdentFuncTable[148] := @Func148;
  FIdentFuncTable[149] := @Func149;
  FIdentFuncTable[150] := @Func150;
  FIdentFuncTable[151] := @Func151;
  FIdentFuncTable[152] := @Func152;
  FIdentFuncTable[153] := @Func153;
  FIdentFuncTable[154] := @Func154;
  FIdentFuncTable[156] := @Func156;
  FIdentFuncTable[158] := @Func158;
  FIdentFuncTable[159] := @Func159;
  FIdentFuncTable[160] := @Func160;
  FIdentFuncTable[164] := @Func164;
  FIdentFuncTable[166] := @Func166;
  FIdentFuncTable[167] := @Func167;
  FIdentFuncTable[169] := @Func169;
  FIdentFuncTable[171] := @Func171;
  FIdentFuncTable[172] := @Func172;
  FIdentFuncTable[173] := @Func173;
  FIdentFuncTable[174] := @Func174;
  FIdentFuncTable[177] := @Func177;
  FIdentFuncTable[178] := @Func178;
  FIdentFuncTable[179] := @Func179;
  FIdentFuncTable[182] := @Func182;
  FIdentFuncTable[186] := @Func186;
  FIdentFuncTable[187] := @Func187;
  FIdentFuncTable[190] := @Func190;
  FIdentFuncTable[191] := @Func191;
  FIdentFuncTable[194] := @Func194;
  FIdentFuncTable[195] := @Func195;
  FIdentFuncTable[199] := @Func199;
  FIdentFuncTable[200] := @Func200;
  FIdentFuncTable[205] := @Func205;
  FIdentFuncTable[210] := @Func210;
  FIdentFuncTable[213] := @Func213;
  FIdentFuncTable[220] := @Func220;
  FIdentFuncTable[222] := @Func222;
  FIdentFuncTable[224] := @Func224;
  FIdentFuncTable[232] := @Func232;
  FIdentFuncTable[242] := @Func242;
  FIdentFuncTable[250] := @Func250;
  FIdentFuncTable[253] := @Func253;
  FIdentFuncTable[275] := @Func275;
end;

function TSynCssSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['a'..'z', 'A'..'Z', '0'..'9', '-', '_','@'] do begin
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
  if KeyComp('ms') or KeyComp('@media') then
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

function TSynCssSyn.Func41: TtkTokenKind;
begin
  if KeyComp('icon') then
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
  if KeyComp('top') or KeyComp('panose-1') or KeyComp('@font-face') then
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

function TSynCssSyn.Func53: TtkTokenKind;
begin
  if KeyComp('box-pack') then
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
  if KeyComp('font') or KeyComp('padding') or KeyComp('nav-up') then
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
  if KeyComp('cue-before') or KeyComp('nav-left') then
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
  if KeyComp('width')then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func65: TtkTokenKind;
begin
  if KeyComp('stemh') or KeyComp('box-align')  then
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
  if KeyComp('zoom') or KeyComp('box-flex') then
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

function TSynCssSyn.Func71: TtkTokenKind;
begin
  if KeyComp('target') then
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
  if KeyComp('speak-header') or KeyComp('min-height') or KeyComp('nav-down')
    or KeyComp('nav-index') then
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
  if KeyComp('page-break-after') or KeyComp('word-break') or KeyComp('border-image') or
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
  if KeyComp('behavior') or KeyComp('appearance') or KeyComp('nav-right') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func81: TtkTokenKind;
begin
  if KeyComp('speech-rate') or KeyComp('min-width')  or KeyComp('box-lines') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func82: TtkTokenKind;
begin
  if KeyComp('pitch-range') or KeyComp('mathline') or KeyComp('resize') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func83: TtkTokenKind;
begin
  if KeyComp('max-width') or KeyComp('widths') or KeyComp('column-gap') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func85: TtkTokenKind;
begin
  if KeyComp('bottom') or KeyComp('target-name') then
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

function TSynCssSyn.Func89: TtkTokenKind;
begin
  if KeyComp('opacity') then
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
  if KeyComp('orphans') or KeyComp('content') or KeyComp('@import') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func92: TtkTokenKind;
begin
  if KeyComp('box-shadow') then
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
    KeyComp('cursor') or KeyComp('grid-rows') or KeyComp('target-new') then
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
    KeyComp('outline') or KeyComp('animation')  then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func97: TtkTokenKind;
begin
  if KeyComp('quotes') or KeyComp('direction') or KeyComp('unicode-range')
    or KeyComp('columns') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func98: TtkTokenKind;
begin
  if KeyComp('padding-right') or KeyComp('azimuth') or KeyComp('column-fill') then
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
  if KeyComp('elevation') or KeyComp('@keyframes') or KeyComp('box-orient') then
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
  if KeyComp('border-color') or KeyComp('box-sizing')  then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func107: TtkTokenKind;
begin
  if KeyComp('border-width') or KeyComp('border-image-slice') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func108: TtkTokenKind;
begin
  if KeyComp('font-weight') or KeyComp('play-during') or KeyComp('text-wrap') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func109: TtkTokenKind;
begin
  if KeyComp('column-span') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func110: TtkTokenKind;
begin
  if KeyComp('word-spacing') or KeyComp('animation-name') then
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
  if KeyComp('background-image') or KeyComp('border-spacing') or KeyComp('rotation') then
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
  if KeyComp('table-layout') or KeyComp('border-radius') or KeyComp('column-rule') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func116: TtkTokenKind;
begin
  if KeyComp('text-indent') or KeyComp('overflow') or KeyComp('grid-columns') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func117: TtkTokenKind;
begin
  if KeyComp('font-style') or KeyComp('position') or KeyComp('speak-numeral')
  Or KeyComp('background-clip') then
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

function TSynCssSyn.Func119: TtkTokenKind;
begin
  if KeyComp('box-direction') then
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

function TSynCssSyn.Func123: TtkTokenKind;
begin
  if KeyComp('border-image-width') or KeyComp('column-width') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func124: TtkTokenKind;
begin
  if KeyComp('border-style') or KeyComp('border-image-repeat') or KeyComp('animation-delay')
     or KeyComp('transform') then Result := tkKey
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

function TSynCssSyn.Func127: TtkTokenKind;
begin
  if KeyComp('box-flex-group') then
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

function TSynCssSyn.Func132: TtkTokenKind;
begin
  if KeyComp('column-count') then
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
  if KeyComp('visibility') or KeyComp('background-size') then
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
  if KeyComp('border-top-color') or KeyComp('list-style-image') or KeyComp('perspective') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func139: TtkTokenKind;
begin
  if KeyComp('border-top-width') or KeyComp('transition') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func140: TtkTokenKind;
begin
  if KeyComp('background-color') or KeyComp('outline-color') or KeyComp('text-emphasis') or
    KeyComp('scrollbar-face-color') or KeyComp('border-image-source') then
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

function TSynCssSyn.Func142: TtkTokenKind;
begin
  if KeyComp('background-repeat') then
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

function TSynCssSyn.Func146: TtkTokenKind;
begin
  if KeyComp('text-outline') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func148: TtkTokenKind;
begin
  if KeyComp('border-left-style') or KeyComp('outline-offset') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func149: TtkTokenKind;
begin
  if KeyComp('border-right-color') or KeyComp('background-origin')
    or KeyComp('backface-visibility') then
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

function TSynCssSyn.Func153: TtkTokenKind;
begin
  if KeyComp('box-ordinal-group') then
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

function TSynCssSyn.Func159: TtkTokenKind;
begin
  if KeyComp('border-image-outset') or KeyComp('column-rule-color') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func160: TtkTokenKind;
begin
  if KeyComp('layout-grid-type') or KeyComp('text-justify') or KeyComp('column-rule-width') then
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
  if KeyComp('scrollbar-3d-light-color') or KeyComp('text-overflow') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func167: TtkTokenKind;
begin
  if KeyComp('border-right-style') or KeyComp('rotation-point') or KeyComp('transition-delay') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func169: TtkTokenKind;
begin
  if KeyComp('list-style-type') or KeyComp('target-position') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func171: TtkTokenKind;
begin
  if KeyComp('border-top-left-radius') then
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
  if KeyComp('text-transform') or KeyComp('animation-direction') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func177: TtkTokenKind;
begin
  if KeyComp('animation-play-state') or KeyComp('column-rule-style') or KeyComp('transform-origin') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func178: TtkTokenKind;
begin
  if KeyComp('counter-increment') or KeyComp('overflow-style') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func179: TtkTokenKind;
begin
  if KeyComp('animation-duration') then
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

function TSynCssSyn.Func186: TtkTokenKind;
begin
  if KeyComp('transform-style') then
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
  if KeyComp('border-bottom-style') or KeyComp('border-top-right-radius') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func191: TtkTokenKind;
begin
  if KeyComp('perspective-origin') then
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
  if KeyComp('scrollbar-shadow-color') or KeyComp('hanging-punctuation')
    or KeyComp('punctuation-trim') then
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

function TSynCssSyn.Func205: TtkTokenKind;
begin
  if KeyComp('border-bottom-left-radius') then
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

function TSynCssSyn.Func222: TtkTokenKind;
begin
  if KeyComp('transition-duration') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func224: TtkTokenKind;
begin
  if KeyComp('border-bottom-right-radius') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func232: TtkTokenKind;
begin
  if KeyComp('animation-timing-function') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func242: TtkTokenKind;
begin
  if KeyComp('animation-iteration-count') then
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

function TSynCssSyn.Func253: TtkTokenKind;
begin
  if KeyComp('transition-property') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynCssSyn.Func275: TtkTokenKind;
begin
  if KeyComp('transition-timing-function') then
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
  if (HashKey >= 16) and (HashKey <= 275) then
    Result := fIdentFuncTable[HashKey]()
  else
    Result := tkIdentifier;
end;

procedure TSynCssSyn.MakeMethodTables;
var
  chI: Char;
begin
  for chI := #0 to #255 do
    case chI of
      '{', '}'                    : FProcTable[chI] := @AsciiCharProc;
      #13                         : FProcTable[chI] := @CRProc;
      '-'                         : FProcTable[chI] := @DashProc;
      'A'..'Z', 'a'..'z', '_','@' : FProcTable[chI] := @IdentProc;
      '#', '$'                    : FProcTable[chI] := @IntegerProc;
      #10                         : FProcTable[chI] := @LFProc;
      #0                          : FProcTable[chI] := @NullProc;
      '0'..'9'                    : FProcTable[chI] := @NumberProc;
      ')', '('                    : FProcTable[chI] := @RoundOpenProc;
      '/'                         : FProcTable[chI] := @SlashProc;
      #1..#9, #11, #12, #14..#32  : FProcTable[chI] := @SpaceProc;
      #39                         : FProcTable[chI] := @StringProc;
    else
      FProcTable[chI] := @UnknownProc;
    end;
end;

constructor TSynCssSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighlighterAttributes.Create(@SYNS_AttrComment, SYNS_XML_AttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(@SYNS_AttrIdentifier, SYNS_XML_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(@SYNS_AttrKey, SYNS_XML_AttrKey);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(@SYNS_AttrNumber, SYNS_XML_AttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(@SYNS_AttrSpace, SYNS_XML_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(@SYNS_AttrString, SYNS_XML_AttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(@SYNS_AttrSymbol, SYNS_XML_AttrSymbol);
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange(@DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterCSS;
  fRange := rsUnknown;
end;

procedure TSynCssSyn.SetLine(const NewValue: String; LineNumber: Integer);
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
  while (fLine[Run] in [#128..#191]) OR // continued utf8 subcode
   ((fLine[Run]<>#0) and (fProcTable[fLine[Run]] = @UnknownProc)) do inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynCssSyn.Next;
begin
  FTokenPos := Run;
  if FRange = rsCStyle then
    CStyleCommentProc
  else
    FProcTable[FLine[Run]]();
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
    SYN_ATTR_NUMBER    : Result := fNumberAttri;
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

procedure TSynCssSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength:=Run-fTokenPos;
  TokenStart:=FLine + fTokenPos;
end;

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
