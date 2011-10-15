{
 /***************************************************************************
                                  lazutf8.pas
 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of LazUtils                                            *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Useful routines for managing UTF-8 strings
}
unit LazUTF8;

{$mode objfpc}{$H+}

{$define LAZUTF8_USE_TABLES}

interface

uses
{$ifdef windows}
  Windows,
{$endif}
  Classes, SysUtils; 

// AnsiToUTF8 and UTF8ToAnsi need a widestring manager under Linux, BSD, MacOSX
// but normally these OS use UTF-8 as system encoding so the widestringmanager
// is not needed.
function NeedRTLAnsi: boolean;// true if system encoding is not UTF-8
procedure SetNeedRTLAnsi(NewValue: boolean);
function UTF8ToSys(const s: string): string;// as UTF8ToAnsi but more independent of widestringmanager
function SysToUTF8(const s: string): string;// as AnsiToUTF8 but more independent of widestringmanager

function UTF8CharacterLength(p: PChar): integer;
function UTF8Length(const s: string): PtrInt;
function UTF8Length(p: PChar; ByteCount: PtrInt): PtrInt;
function UTF8CharacterToUnicode(p: PChar; out CharLen: integer): Cardinal;
function UnicodeToUTF8(u: cardinal; Buf: PChar): integer; inline;
function UnicodeToUTF8SkipErrors(u: cardinal; Buf: PChar): integer;
{function UnicodeToUTF8(u: cardinal): shortstring; inline;
function UTF8ToDoubleByteString(const s: string): string;
function UTF8ToDoubleByte(UTF8Str: PChar; Len: PtrInt; DBStr: PByte): PtrInt;
function UTF8FindNearestCharStart(UTF8Str: PChar; Len: integer;
                                  BytePos: integer): integer;
// find the n-th UTF8 character, ignoring BIDI
function UTF8CharStart(UTF8Str: PChar; Len, CharIndex: PtrInt): PChar;
// find the byte index of the n-th UTF8 character, ignoring BIDI (byte len of substr)
function UTF8CharToByteIndex(UTF8Str: PChar; Len, CharIndex: PtrInt): PtrInt;
procedure UTF8FixBroken(P: PChar);
function UTF8CharacterStrictLength(P: PChar): integer;
function UTF8CStringToUTF8String(SourceStart: PChar; SourceLen: PtrInt) : string;
function UTF8Pos(const SearchForText, SearchInText: string): PtrInt;
function UTF8Copy(const s: string; StartCharIndex, CharCount: PtrInt): string;
procedure UTF8Delete(var s: String; StartCharIndex, CharCount: PtrInt);
procedure UTF8Insert(const source: String; var s: string; StartCharIndex: PtrInt);}

{$ifdef LAZUTF8_USE_TABLES}
function UnicodeLowercase(u: cardinal): cardinal;
function UTF8LowerCaseMattias(const s: utf8string): utf8string;
{$endif}
function UTF8LowerCase(const AInStr: utf8string; ALanguage: utf8string=''): utf8string;
function UTF8LowerCase2(const AInStr: utf8string; ALocale: utf8string=''): utf8string;
function UTF8UpperCase(const AInStr: utf8string; ALanguage: utf8string=''): utf8string;
{function FindInvalidUTF8Character(p: PChar; Count: PtrInt;
//                                  StopOnNonASCII: Boolean = false): PtrInt;
//function ValidUTF8String(const s: String): String;

//procedure AssignUTF8ListToAnsi(UTF8List, AnsiList: TStrings);

//function UTF16CharacterLength(p: PWideChar): integer;
//function UTF16Length(const s: widestring): PtrInt;
//function UTF16Length(p: PWideChar; WordCount: PtrInt): PtrInt;
//function UTF16CharacterToUnicode(p: PWideChar; out CharLen: integer): Cardinal;
//function UnicodeToUTF16(u: cardinal): widestring;}

//compare functions

function UTF8CompareStr(const S1, S2: utf8string): Integer;
function UTF8CompareText(const S1, S2: utf8string): Integer;

type
  TConvertResult = (trNoError, trNullSrc, trNullDest, trDestExhausted,
    trInvalidChar, trUnfinishedChar);

  TConvertOption = (toInvalidCharError, toInvalidCharToSymbol,
    toUnfinishedCharError, toUnfinishedCharToSymbol);
  TConvertOptions = set of TConvertOption;

function ConvertUTF8ToUTF16(Dest: PWideChar; DestWideCharCount: SizeUInt;
  Src: PChar; SrcCharCount: SizeUInt; Options: TConvertOptions;
  out ActualWideCharCount: SizeUInt): TConvertResult;

function ConvertUTF16ToUTF8(Dest: PChar; DestCharCount: SizeUInt;
  Src: PWideChar; SrcWideCharCount: SizeUInt; Options: TConvertOptions;
  out ActualCharCount: SizeUInt): TConvertResult;

function UTF8ToUTF16(const S: AnsiString): UnicodeString;
function UTF16ToUTF8(const S: UnicodeString): AnsiString;

// locale
procedure LazGetLanguageIDs(var Lang, FallbackLang: String);
procedure LazGetShortLanguageID(var Lang: String);

var
  FPUpChars: array[char] of char;

implementation

uses
  gettext
{$IFDEF Darwin}, MacOSAll{$ENDIF}
  ;

var
  FNeedRTLAnsi: boolean = false;
  FNeedRTLAnsiValid: boolean = false;

{$ifdef LAZUTF8_USE_TABLES}
var
  UnicodeLower00C0_00DE: array[$00C0..$00DE] of word;
  UnicodeLower0100_024E: array[$0100..$024E] of word;
  UnicodeLower0386_03AB: array[$0386..$03AB] of word;
  UnicodeLower03D8_042F: array[$03D8..$042F] of word;
  UnicodeLower0460_0512: array[$0460..$0512] of word;
  UnicodeLower1E00_1FFC: array[$1E00..$1FFC] of word;
  UnicodeLower2126_2183: array[$2126..$2183] of word;
  UnicodeLower2C60_2CE2: array[$2C60..$2CE2] of word;

procedure InitUnicodeTables;
var
  i: Integer;
begin
  for i:=Low(UnicodeLower00C0_00DE) to High(UnicodeLower00C0_00DE) do
    UnicodeLower00C0_00DE[i]:=i+32;
  UnicodeLower00C0_00DE[$00D7]:=$00D7;

  for i:=Low(UnicodeLower0100_024E) to High(UnicodeLower0100_024E) do
    UnicodeLower0100_024E[i]:=i;
  UnicodeLower0100_024E[$0100]:=$0101;
  UnicodeLower0100_024E[$0102]:=$0103;
  UnicodeLower0100_024E[$0104]:=$0105;
  UnicodeLower0100_024E[$0106]:=$0107;
  UnicodeLower0100_024E[$0108]:=$0109;
  UnicodeLower0100_024E[$010A]:=$010B;
  UnicodeLower0100_024E[$010C]:=$010D;
  UnicodeLower0100_024E[$010E]:=$010F;
  UnicodeLower0100_024E[$0110]:=$0111;
  UnicodeLower0100_024E[$0112]:=$0113;
  UnicodeLower0100_024E[$0114]:=$0115;
  UnicodeLower0100_024E[$0116]:=$0117;
  UnicodeLower0100_024E[$0118]:=$0119;
  UnicodeLower0100_024E[$011A]:=$011B;
  UnicodeLower0100_024E[$011C]:=$011D;
  UnicodeLower0100_024E[$011E]:=$011F;
  UnicodeLower0100_024E[$0120]:=$0121;
  UnicodeLower0100_024E[$0122]:=$0123;
  UnicodeLower0100_024E[$0124]:=$0125;
  UnicodeLower0100_024E[$0126]:=$0127;
  UnicodeLower0100_024E[$0128]:=$0129;
  UnicodeLower0100_024E[$012A]:=$012B;
  UnicodeLower0100_024E[$012C]:=$012D;
  UnicodeLower0100_024E[$012E]:=$012F;
  UnicodeLower0100_024E[$0130]:=$0069;
  UnicodeLower0100_024E[$0132]:=$0133;
  UnicodeLower0100_024E[$0134]:=$0135;
  UnicodeLower0100_024E[$0136]:=$0137;
  UnicodeLower0100_024E[$0139]:=$013A;
  UnicodeLower0100_024E[$013B]:=$013C;
  UnicodeLower0100_024E[$013D]:=$013E;
  UnicodeLower0100_024E[$013F]:=$0140;
  UnicodeLower0100_024E[$0141]:=$0142;
  UnicodeLower0100_024E[$0143]:=$0144;
  UnicodeLower0100_024E[$0145]:=$0146;
  UnicodeLower0100_024E[$0147]:=$0148;
  UnicodeLower0100_024E[$014A]:=$014B;
  UnicodeLower0100_024E[$014C]:=$014D;
  UnicodeLower0100_024E[$014E]:=$014F;
  UnicodeLower0100_024E[$0150]:=$0151;
  UnicodeLower0100_024E[$0152]:=$0153;
  UnicodeLower0100_024E[$0154]:=$0155;
  UnicodeLower0100_024E[$0156]:=$0157;
  UnicodeLower0100_024E[$0158]:=$0159;
  UnicodeLower0100_024E[$015A]:=$015B;
  UnicodeLower0100_024E[$015C]:=$015D;
  UnicodeLower0100_024E[$015E]:=$015F;
  UnicodeLower0100_024E[$0160]:=$0161;
  UnicodeLower0100_024E[$0162]:=$0163;
  UnicodeLower0100_024E[$0164]:=$0165;
  UnicodeLower0100_024E[$0166]:=$0167;
  UnicodeLower0100_024E[$0168]:=$0169;
  UnicodeLower0100_024E[$016A]:=$016B;
  UnicodeLower0100_024E[$016C]:=$016D;
  UnicodeLower0100_024E[$016E]:=$016F;
  UnicodeLower0100_024E[$0170]:=$0171;
  UnicodeLower0100_024E[$0172]:=$0173;
  UnicodeLower0100_024E[$0174]:=$0175;
  UnicodeLower0100_024E[$0176]:=$0177;
  UnicodeLower0100_024E[$0178]:=$00FF;
  UnicodeLower0100_024E[$0179]:=$017A;
  UnicodeLower0100_024E[$017B]:=$017C;
  UnicodeLower0100_024E[$017D]:=$017E;
  UnicodeLower0100_024E[$0181]:=$0253;
  UnicodeLower0100_024E[$0182]:=$0183;
  UnicodeLower0100_024E[$0184]:=$0185;
  UnicodeLower0100_024E[$0186]:=$0254;
  UnicodeLower0100_024E[$0187]:=$0188;
  UnicodeLower0100_024E[$0189]:=$0256;
  UnicodeLower0100_024E[$018A]:=$0257;
  UnicodeLower0100_024E[$018B]:=$018C;
  UnicodeLower0100_024E[$018E]:=$01DD;
  UnicodeLower0100_024E[$018F]:=$0259;
  UnicodeLower0100_024E[$0190]:=$025B;
  UnicodeLower0100_024E[$0191]:=$0192;
  UnicodeLower0100_024E[$0193]:=$0260;
  UnicodeLower0100_024E[$0194]:=$0263;
  UnicodeLower0100_024E[$0196]:=$0269;
  UnicodeLower0100_024E[$0197]:=$0268;
  UnicodeLower0100_024E[$0198]:=$0199;
  UnicodeLower0100_024E[$019C]:=$026F;
  UnicodeLower0100_024E[$019D]:=$0272;
  UnicodeLower0100_024E[$019F]:=$0275;
  UnicodeLower0100_024E[$01A0]:=$01A1;
  UnicodeLower0100_024E[$01A2]:=$01A3;
  UnicodeLower0100_024E[$01A4]:=$01A5;
  UnicodeLower0100_024E[$01A6]:=$0280;
  UnicodeLower0100_024E[$01A7]:=$01A8;
  UnicodeLower0100_024E[$01A9]:=$0283;
  UnicodeLower0100_024E[$01AC]:=$01AD;
  UnicodeLower0100_024E[$01AE]:=$0288;
  UnicodeLower0100_024E[$01AF]:=$01B0;
  UnicodeLower0100_024E[$01B1]:=$028A;
  UnicodeLower0100_024E[$01B2]:=$028B;
  UnicodeLower0100_024E[$01B3]:=$01B4;
  UnicodeLower0100_024E[$01B5]:=$01B6;
  UnicodeLower0100_024E[$01B7]:=$0292;
  UnicodeLower0100_024E[$01B8]:=$01B9;
  UnicodeLower0100_024E[$01BC]:=$01BD;
  UnicodeLower0100_024E[$01C4]:=$01C6;
  UnicodeLower0100_024E[$01C5]:=$01C6;
  UnicodeLower0100_024E[$01C7]:=$01C9;
  UnicodeLower0100_024E[$01C8]:=$01C9;
  UnicodeLower0100_024E[$01CA]:=$01CC;
  UnicodeLower0100_024E[$01CB]:=$01CC;
  UnicodeLower0100_024E[$01CD]:=$01CE;
  UnicodeLower0100_024E[$01CF]:=$01D0;
  UnicodeLower0100_024E[$01D1]:=$01D2;
  UnicodeLower0100_024E[$01D3]:=$01D4;
  UnicodeLower0100_024E[$01D5]:=$01D6;
  UnicodeLower0100_024E[$01D7]:=$01D8;
  UnicodeLower0100_024E[$01D9]:=$01DA;
  UnicodeLower0100_024E[$01DB]:=$01DC;
  UnicodeLower0100_024E[$01DE]:=$01DF;
  UnicodeLower0100_024E[$01E0]:=$01E1;
  UnicodeLower0100_024E[$01E2]:=$01E3;
  UnicodeLower0100_024E[$01E4]:=$01E5;
  UnicodeLower0100_024E[$01E6]:=$01E7;
  UnicodeLower0100_024E[$01E8]:=$01E9;
  UnicodeLower0100_024E[$01EA]:=$01EB;
  UnicodeLower0100_024E[$01EC]:=$01ED;
  UnicodeLower0100_024E[$01EE]:=$01EF;
  UnicodeLower0100_024E[$01F1]:=$01F3;
  UnicodeLower0100_024E[$01F2]:=$01F3;
  UnicodeLower0100_024E[$01F4]:=$01F5;
  UnicodeLower0100_024E[$01F6]:=$0195;
  UnicodeLower0100_024E[$01F7]:=$01BF;
  UnicodeLower0100_024E[$01F8]:=$01F9;
  UnicodeLower0100_024E[$01FA]:=$01FB;
  UnicodeLower0100_024E[$01FC]:=$01FD;
  UnicodeLower0100_024E[$01FE]:=$01FF;
  UnicodeLower0100_024E[$0200]:=$0201;
  UnicodeLower0100_024E[$0202]:=$0203;
  UnicodeLower0100_024E[$0204]:=$0205;
  UnicodeLower0100_024E[$0206]:=$0207;
  UnicodeLower0100_024E[$0208]:=$0209;
  UnicodeLower0100_024E[$020A]:=$020B;
  UnicodeLower0100_024E[$020C]:=$020D;
  UnicodeLower0100_024E[$020E]:=$020F;
  UnicodeLower0100_024E[$0210]:=$0211;
  UnicodeLower0100_024E[$0212]:=$0213;
  UnicodeLower0100_024E[$0214]:=$0215;
  UnicodeLower0100_024E[$0216]:=$0217;
  UnicodeLower0100_024E[$0218]:=$0219;
  UnicodeLower0100_024E[$021A]:=$021B;
  UnicodeLower0100_024E[$021C]:=$021D;
  UnicodeLower0100_024E[$021E]:=$021F;
  UnicodeLower0100_024E[$0220]:=$019E;
  UnicodeLower0100_024E[$0222]:=$0223;
  UnicodeLower0100_024E[$0224]:=$0225;
  UnicodeLower0100_024E[$0226]:=$0227;
  UnicodeLower0100_024E[$0228]:=$0229;
  UnicodeLower0100_024E[$022A]:=$022B;
  UnicodeLower0100_024E[$022C]:=$022D;
  UnicodeLower0100_024E[$022E]:=$022F;
  UnicodeLower0100_024E[$0230]:=$0231;
  UnicodeLower0100_024E[$0232]:=$0233;
  UnicodeLower0100_024E[$023A]:=$2C65;
  UnicodeLower0100_024E[$023B]:=$023C;
  UnicodeLower0100_024E[$023D]:=$019A;
  UnicodeLower0100_024E[$023E]:=$2C66;
  UnicodeLower0100_024E[$0241]:=$0242;
  UnicodeLower0100_024E[$0243]:=$0180;
  UnicodeLower0100_024E[$0244]:=$0289;
  UnicodeLower0100_024E[$0245]:=$028C;
  UnicodeLower0100_024E[$0246]:=$0247;
  UnicodeLower0100_024E[$0248]:=$0249;
  UnicodeLower0100_024E[$024A]:=$024B;
  UnicodeLower0100_024E[$024C]:=$024D;
  UnicodeLower0100_024E[$024E]:=$024F;

  for i:=Low(UnicodeLower0386_03AB) to High(UnicodeLower0386_03AB) do
    UnicodeLower0386_03AB[i]:=i;
  UnicodeLower0386_03AB[$0386]:=$03AC;
  UnicodeLower0386_03AB[$0388]:=$03AD;
  UnicodeLower0386_03AB[$0389]:=$03AE;
  UnicodeLower0386_03AB[$038A]:=$03AF;
  UnicodeLower0386_03AB[$038C]:=$03CC;
  UnicodeLower0386_03AB[$038E]:=$03CD;
  UnicodeLower0386_03AB[$038F]:=$03CE;
  UnicodeLower0386_03AB[$0391]:=$03B1;
  UnicodeLower0386_03AB[$0392]:=$03B2;
  UnicodeLower0386_03AB[$0393]:=$03B3;
  UnicodeLower0386_03AB[$0394]:=$03B4;
  UnicodeLower0386_03AB[$0395]:=$03B5;
  UnicodeLower0386_03AB[$0396]:=$03B6;
  UnicodeLower0386_03AB[$0397]:=$03B7;
  UnicodeLower0386_03AB[$0398]:=$03B8;
  UnicodeLower0386_03AB[$0399]:=$03B9;
  UnicodeLower0386_03AB[$039A]:=$03BA;
  UnicodeLower0386_03AB[$039B]:=$03BB;
  UnicodeLower0386_03AB[$039C]:=$03BC;
  UnicodeLower0386_03AB[$039D]:=$03BD;
  UnicodeLower0386_03AB[$039E]:=$03BE;
  UnicodeLower0386_03AB[$039F]:=$03BF;
  UnicodeLower0386_03AB[$03A0]:=$03C0;
  UnicodeLower0386_03AB[$03A1]:=$03C1;
  UnicodeLower0386_03AB[$03A3]:=$03C3;
  UnicodeLower0386_03AB[$03A4]:=$03C4;
  UnicodeLower0386_03AB[$03A5]:=$03C5;
  UnicodeLower0386_03AB[$03A6]:=$03C6;
  UnicodeLower0386_03AB[$03A7]:=$03C7;
  UnicodeLower0386_03AB[$03A8]:=$03C8;
  UnicodeLower0386_03AB[$03A9]:=$03C9;
  UnicodeLower0386_03AB[$03AA]:=$03CA;
  UnicodeLower0386_03AB[$03AB]:=$03CB;

  for i:=Low(UnicodeLower03D8_042F) to High(UnicodeLower03D8_042F) do
    UnicodeLower03D8_042F[i]:=i;
  UnicodeLower03D8_042F[$03D8]:=$03D9;
  UnicodeLower03D8_042F[$03DA]:=$03DB;
  UnicodeLower03D8_042F[$03DC]:=$03DD;
  UnicodeLower03D8_042F[$03DE]:=$03DF;
  UnicodeLower03D8_042F[$03E0]:=$03E1;
  UnicodeLower03D8_042F[$03E2]:=$03E3;
  UnicodeLower03D8_042F[$03E4]:=$03E5;
  UnicodeLower03D8_042F[$03E6]:=$03E7;
  UnicodeLower03D8_042F[$03E8]:=$03E9;
  UnicodeLower03D8_042F[$03EA]:=$03EB;
  UnicodeLower03D8_042F[$03EC]:=$03ED;
  UnicodeLower03D8_042F[$03EE]:=$03EF;
  UnicodeLower03D8_042F[$03F4]:=$03B8;
  UnicodeLower03D8_042F[$03F7]:=$03F8;
  UnicodeLower03D8_042F[$03F9]:=$03F2;
  UnicodeLower03D8_042F[$03FA]:=$03FB;
  UnicodeLower03D8_042F[$03FD]:=$037B;
  UnicodeLower03D8_042F[$03FE]:=$037C;
  UnicodeLower03D8_042F[$03FF]:=$037D;
  UnicodeLower03D8_042F[$0400]:=$0450;
  UnicodeLower03D8_042F[$0401]:=$0451;
  UnicodeLower03D8_042F[$0402]:=$0452;
  UnicodeLower03D8_042F[$0403]:=$0453;
  UnicodeLower03D8_042F[$0404]:=$0454;
  UnicodeLower03D8_042F[$0405]:=$0455;
  UnicodeLower03D8_042F[$0406]:=$0456;
  UnicodeLower03D8_042F[$0407]:=$0457;
  UnicodeLower03D8_042F[$0408]:=$0458;
  UnicodeLower03D8_042F[$0409]:=$0459;
  UnicodeLower03D8_042F[$040A]:=$045A;
  UnicodeLower03D8_042F[$040B]:=$045B;
  UnicodeLower03D8_042F[$040C]:=$045C;
  UnicodeLower03D8_042F[$040D]:=$045D;
  UnicodeLower03D8_042F[$040E]:=$045E;
  UnicodeLower03D8_042F[$040F]:=$045F;
  UnicodeLower03D8_042F[$0410]:=$0430;
  UnicodeLower03D8_042F[$0411]:=$0431;
  UnicodeLower03D8_042F[$0412]:=$0432;
  UnicodeLower03D8_042F[$0413]:=$0433;
  UnicodeLower03D8_042F[$0414]:=$0434;
  UnicodeLower03D8_042F[$0415]:=$0435;
  UnicodeLower03D8_042F[$0416]:=$0436;
  UnicodeLower03D8_042F[$0417]:=$0437;
  UnicodeLower03D8_042F[$0418]:=$0438;
  UnicodeLower03D8_042F[$0419]:=$0439;
  UnicodeLower03D8_042F[$041A]:=$043A;
  UnicodeLower03D8_042F[$041B]:=$043B;
  UnicodeLower03D8_042F[$041C]:=$043C;
  UnicodeLower03D8_042F[$041D]:=$043D;
  UnicodeLower03D8_042F[$041E]:=$043E;
  UnicodeLower03D8_042F[$041F]:=$043F;
  UnicodeLower03D8_042F[$0420]:=$0440;
  UnicodeLower03D8_042F[$0421]:=$0441;
  UnicodeLower03D8_042F[$0422]:=$0442;
  UnicodeLower03D8_042F[$0423]:=$0443;
  UnicodeLower03D8_042F[$0424]:=$0444;
  UnicodeLower03D8_042F[$0425]:=$0445;
  UnicodeLower03D8_042F[$0426]:=$0446;
  UnicodeLower03D8_042F[$0427]:=$0447;
  UnicodeLower03D8_042F[$0428]:=$0448;
  UnicodeLower03D8_042F[$0429]:=$0449;
  UnicodeLower03D8_042F[$042A]:=$044A;
  UnicodeLower03D8_042F[$042B]:=$044B;
  UnicodeLower03D8_042F[$042C]:=$044C;
  UnicodeLower03D8_042F[$042D]:=$044D;
  UnicodeLower03D8_042F[$042E]:=$044E;
  UnicodeLower03D8_042F[$042F]:=$044F;

  for i:=Low(UnicodeLower0460_0512) to High(UnicodeLower0460_0512) do
    UnicodeLower0460_0512[i]:=i;
  UnicodeLower0460_0512[$0460]:=$0461;
  UnicodeLower0460_0512[$0462]:=$0463;
  UnicodeLower0460_0512[$0464]:=$0465;
  UnicodeLower0460_0512[$0466]:=$0467;
  UnicodeLower0460_0512[$0468]:=$0469;
  UnicodeLower0460_0512[$046A]:=$046B;
  UnicodeLower0460_0512[$046C]:=$046D;
  UnicodeLower0460_0512[$046E]:=$046F;
  UnicodeLower0460_0512[$0470]:=$0471;
  UnicodeLower0460_0512[$0472]:=$0473;
  UnicodeLower0460_0512[$0474]:=$0475;
  UnicodeLower0460_0512[$0476]:=$0477;
  UnicodeLower0460_0512[$0478]:=$0479;
  UnicodeLower0460_0512[$047A]:=$047B;
  UnicodeLower0460_0512[$047C]:=$047D;
  UnicodeLower0460_0512[$047E]:=$047F;
  UnicodeLower0460_0512[$0480]:=$0481;
  UnicodeLower0460_0512[$048A]:=$048B;
  UnicodeLower0460_0512[$048C]:=$048D;
  UnicodeLower0460_0512[$048E]:=$048F;
  UnicodeLower0460_0512[$0490]:=$0491;
  UnicodeLower0460_0512[$0492]:=$0493;
  UnicodeLower0460_0512[$0494]:=$0495;
  UnicodeLower0460_0512[$0496]:=$0497;
  UnicodeLower0460_0512[$0498]:=$0499;
  UnicodeLower0460_0512[$049A]:=$049B;
  UnicodeLower0460_0512[$049C]:=$049D;
  UnicodeLower0460_0512[$049E]:=$049F;
  UnicodeLower0460_0512[$04A0]:=$04A1;
  UnicodeLower0460_0512[$04A2]:=$04A3;
  UnicodeLower0460_0512[$04A4]:=$04A5;
  UnicodeLower0460_0512[$04A6]:=$04A7;
  UnicodeLower0460_0512[$04A8]:=$04A9;
  UnicodeLower0460_0512[$04AA]:=$04AB;
  UnicodeLower0460_0512[$04AC]:=$04AD;
  UnicodeLower0460_0512[$04AE]:=$04AF;
  UnicodeLower0460_0512[$04B0]:=$04B1;
  UnicodeLower0460_0512[$04B2]:=$04B3;
  UnicodeLower0460_0512[$04B4]:=$04B5;
  UnicodeLower0460_0512[$04B6]:=$04B7;
  UnicodeLower0460_0512[$04B8]:=$04B9;
  UnicodeLower0460_0512[$04BA]:=$04BB;
  UnicodeLower0460_0512[$04BC]:=$04BD;
  UnicodeLower0460_0512[$04BE]:=$04BF;
  UnicodeLower0460_0512[$04C0]:=$04CF;
  UnicodeLower0460_0512[$04C1]:=$04C2;
  UnicodeLower0460_0512[$04C3]:=$04C4;
  UnicodeLower0460_0512[$04C5]:=$04C6;
  UnicodeLower0460_0512[$04C7]:=$04C8;
  UnicodeLower0460_0512[$04C9]:=$04CA;
  UnicodeLower0460_0512[$04CB]:=$04CC;
  UnicodeLower0460_0512[$04CD]:=$04CE;
  UnicodeLower0460_0512[$04D0]:=$04D1;
  UnicodeLower0460_0512[$04D2]:=$04D3;
  UnicodeLower0460_0512[$04D4]:=$04D5;
  UnicodeLower0460_0512[$04D6]:=$04D7;
  UnicodeLower0460_0512[$04D8]:=$04D9;
  UnicodeLower0460_0512[$04DA]:=$04DB;
  UnicodeLower0460_0512[$04DC]:=$04DD;
  UnicodeLower0460_0512[$04DE]:=$04DF;
  UnicodeLower0460_0512[$04E0]:=$04E1;
  UnicodeLower0460_0512[$04E2]:=$04E3;
  UnicodeLower0460_0512[$04E4]:=$04E5;
  UnicodeLower0460_0512[$04E6]:=$04E7;
  UnicodeLower0460_0512[$04E8]:=$04E9;
  UnicodeLower0460_0512[$04EA]:=$04EB;
  UnicodeLower0460_0512[$04EC]:=$04ED;
  UnicodeLower0460_0512[$04EE]:=$04EF;
  UnicodeLower0460_0512[$04F0]:=$04F1;
  UnicodeLower0460_0512[$04F2]:=$04F3;
  UnicodeLower0460_0512[$04F4]:=$04F5;
  UnicodeLower0460_0512[$04F6]:=$04F7;
  UnicodeLower0460_0512[$04F8]:=$04F9;
  UnicodeLower0460_0512[$04FA]:=$04FB;
  UnicodeLower0460_0512[$04FC]:=$04FD;
  UnicodeLower0460_0512[$04FE]:=$04FF;
  UnicodeLower0460_0512[$0500]:=$0501;
  UnicodeLower0460_0512[$0502]:=$0503;
  UnicodeLower0460_0512[$0504]:=$0505;
  UnicodeLower0460_0512[$0506]:=$0507;
  UnicodeLower0460_0512[$0508]:=$0509;
  UnicodeLower0460_0512[$050A]:=$050B;
  UnicodeLower0460_0512[$050C]:=$050D;
  UnicodeLower0460_0512[$050E]:=$050F;
  UnicodeLower0460_0512[$0510]:=$0511;
  UnicodeLower0460_0512[$0512]:=$0513;

  for i:=Low(UnicodeLower1E00_1FFC) to High(UnicodeLower1E00_1FFC) do
    UnicodeLower1E00_1FFC[i]:=i;
  UnicodeLower1E00_1FFC[$1E00]:=$1E01;
  UnicodeLower1E00_1FFC[$1E02]:=$1E03;
  UnicodeLower1E00_1FFC[$1E04]:=$1E05;
  UnicodeLower1E00_1FFC[$1E06]:=$1E07;
  UnicodeLower1E00_1FFC[$1E08]:=$1E09;
  UnicodeLower1E00_1FFC[$1E0A]:=$1E0B;
  UnicodeLower1E00_1FFC[$1E0C]:=$1E0D;
  UnicodeLower1E00_1FFC[$1E0E]:=$1E0F;
  UnicodeLower1E00_1FFC[$1E10]:=$1E11;
  UnicodeLower1E00_1FFC[$1E12]:=$1E13;
  UnicodeLower1E00_1FFC[$1E14]:=$1E15;
  UnicodeLower1E00_1FFC[$1E16]:=$1E17;
  UnicodeLower1E00_1FFC[$1E18]:=$1E19;
  UnicodeLower1E00_1FFC[$1E1A]:=$1E1B;
  UnicodeLower1E00_1FFC[$1E1C]:=$1E1D;
  UnicodeLower1E00_1FFC[$1E1E]:=$1E1F;
  UnicodeLower1E00_1FFC[$1E20]:=$1E21;
  UnicodeLower1E00_1FFC[$1E22]:=$1E23;
  UnicodeLower1E00_1FFC[$1E24]:=$1E25;
  UnicodeLower1E00_1FFC[$1E26]:=$1E27;
  UnicodeLower1E00_1FFC[$1E28]:=$1E29;
  UnicodeLower1E00_1FFC[$1E2A]:=$1E2B;
  UnicodeLower1E00_1FFC[$1E2C]:=$1E2D;
  UnicodeLower1E00_1FFC[$1E2E]:=$1E2F;
  UnicodeLower1E00_1FFC[$1E30]:=$1E31;
  UnicodeLower1E00_1FFC[$1E32]:=$1E33;
  UnicodeLower1E00_1FFC[$1E34]:=$1E35;
  UnicodeLower1E00_1FFC[$1E36]:=$1E37;
  UnicodeLower1E00_1FFC[$1E38]:=$1E39;
  UnicodeLower1E00_1FFC[$1E3A]:=$1E3B;
  UnicodeLower1E00_1FFC[$1E3C]:=$1E3D;
  UnicodeLower1E00_1FFC[$1E3E]:=$1E3F;
  UnicodeLower1E00_1FFC[$1E40]:=$1E41;
  UnicodeLower1E00_1FFC[$1E42]:=$1E43;
  UnicodeLower1E00_1FFC[$1E44]:=$1E45;
  UnicodeLower1E00_1FFC[$1E46]:=$1E47;
  UnicodeLower1E00_1FFC[$1E48]:=$1E49;
  UnicodeLower1E00_1FFC[$1E4A]:=$1E4B;
  UnicodeLower1E00_1FFC[$1E4C]:=$1E4D;
  UnicodeLower1E00_1FFC[$1E4E]:=$1E4F;
  UnicodeLower1E00_1FFC[$1E50]:=$1E51;
  UnicodeLower1E00_1FFC[$1E52]:=$1E53;
  UnicodeLower1E00_1FFC[$1E54]:=$1E55;
  UnicodeLower1E00_1FFC[$1E56]:=$1E57;
  UnicodeLower1E00_1FFC[$1E58]:=$1E59;
  UnicodeLower1E00_1FFC[$1E5A]:=$1E5B;
  UnicodeLower1E00_1FFC[$1E5C]:=$1E5D;
  UnicodeLower1E00_1FFC[$1E5E]:=$1E5F;
  UnicodeLower1E00_1FFC[$1E60]:=$1E61;
  UnicodeLower1E00_1FFC[$1E62]:=$1E63;
  UnicodeLower1E00_1FFC[$1E64]:=$1E65;
  UnicodeLower1E00_1FFC[$1E66]:=$1E67;
  UnicodeLower1E00_1FFC[$1E68]:=$1E69;
  UnicodeLower1E00_1FFC[$1E6A]:=$1E6B;
  UnicodeLower1E00_1FFC[$1E6C]:=$1E6D;
  UnicodeLower1E00_1FFC[$1E6E]:=$1E6F;
  UnicodeLower1E00_1FFC[$1E70]:=$1E71;
  UnicodeLower1E00_1FFC[$1E72]:=$1E73;
  UnicodeLower1E00_1FFC[$1E74]:=$1E75;
  UnicodeLower1E00_1FFC[$1E76]:=$1E77;
  UnicodeLower1E00_1FFC[$1E78]:=$1E79;
  UnicodeLower1E00_1FFC[$1E7A]:=$1E7B;
  UnicodeLower1E00_1FFC[$1E7C]:=$1E7D;
  UnicodeLower1E00_1FFC[$1E7E]:=$1E7F;
  UnicodeLower1E00_1FFC[$1E80]:=$1E81;
  UnicodeLower1E00_1FFC[$1E82]:=$1E83;
  UnicodeLower1E00_1FFC[$1E84]:=$1E85;
  UnicodeLower1E00_1FFC[$1E86]:=$1E87;
  UnicodeLower1E00_1FFC[$1E88]:=$1E89;
  UnicodeLower1E00_1FFC[$1E8A]:=$1E8B;
  UnicodeLower1E00_1FFC[$1E8C]:=$1E8D;
  UnicodeLower1E00_1FFC[$1E8E]:=$1E8F;
  UnicodeLower1E00_1FFC[$1E90]:=$1E91;
  UnicodeLower1E00_1FFC[$1E92]:=$1E93;
  UnicodeLower1E00_1FFC[$1E94]:=$1E95;
  UnicodeLower1E00_1FFC[$1EA0]:=$1EA1;
  UnicodeLower1E00_1FFC[$1EA2]:=$1EA3;
  UnicodeLower1E00_1FFC[$1EA4]:=$1EA5;
  UnicodeLower1E00_1FFC[$1EA6]:=$1EA7;
  UnicodeLower1E00_1FFC[$1EA8]:=$1EA9;
  UnicodeLower1E00_1FFC[$1EAA]:=$1EAB;
  UnicodeLower1E00_1FFC[$1EAC]:=$1EAD;
  UnicodeLower1E00_1FFC[$1EAE]:=$1EAF;
  UnicodeLower1E00_1FFC[$1EB0]:=$1EB1;
  UnicodeLower1E00_1FFC[$1EB2]:=$1EB3;
  UnicodeLower1E00_1FFC[$1EB4]:=$1EB5;
  UnicodeLower1E00_1FFC[$1EB6]:=$1EB7;
  UnicodeLower1E00_1FFC[$1EB8]:=$1EB9;
  UnicodeLower1E00_1FFC[$1EBA]:=$1EBB;
  UnicodeLower1E00_1FFC[$1EBC]:=$1EBD;
  UnicodeLower1E00_1FFC[$1EBE]:=$1EBF;
  UnicodeLower1E00_1FFC[$1EC0]:=$1EC1;
  UnicodeLower1E00_1FFC[$1EC2]:=$1EC3;
  UnicodeLower1E00_1FFC[$1EC4]:=$1EC5;
  UnicodeLower1E00_1FFC[$1EC6]:=$1EC7;
  UnicodeLower1E00_1FFC[$1EC8]:=$1EC9;
  UnicodeLower1E00_1FFC[$1ECA]:=$1ECB;
  UnicodeLower1E00_1FFC[$1ECC]:=$1ECD;
  UnicodeLower1E00_1FFC[$1ECE]:=$1ECF;
  UnicodeLower1E00_1FFC[$1ED0]:=$1ED1;
  UnicodeLower1E00_1FFC[$1ED2]:=$1ED3;
  UnicodeLower1E00_1FFC[$1ED4]:=$1ED5;
  UnicodeLower1E00_1FFC[$1ED6]:=$1ED7;
  UnicodeLower1E00_1FFC[$1ED8]:=$1ED9;
  UnicodeLower1E00_1FFC[$1EDA]:=$1EDB;
  UnicodeLower1E00_1FFC[$1EDC]:=$1EDD;
  UnicodeLower1E00_1FFC[$1EDE]:=$1EDF;
  UnicodeLower1E00_1FFC[$1EE0]:=$1EE1;
  UnicodeLower1E00_1FFC[$1EE2]:=$1EE3;
  UnicodeLower1E00_1FFC[$1EE4]:=$1EE5;
  UnicodeLower1E00_1FFC[$1EE6]:=$1EE7;
  UnicodeLower1E00_1FFC[$1EE8]:=$1EE9;
  UnicodeLower1E00_1FFC[$1EEA]:=$1EEB;
  UnicodeLower1E00_1FFC[$1EEC]:=$1EED;
  UnicodeLower1E00_1FFC[$1EEE]:=$1EEF;
  UnicodeLower1E00_1FFC[$1EF0]:=$1EF1;
  UnicodeLower1E00_1FFC[$1EF2]:=$1EF3;
  UnicodeLower1E00_1FFC[$1EF4]:=$1EF5;
  UnicodeLower1E00_1FFC[$1EF6]:=$1EF7;
  UnicodeLower1E00_1FFC[$1EF8]:=$1EF9;
  UnicodeLower1E00_1FFC[$1F08]:=$1F00;
  UnicodeLower1E00_1FFC[$1F09]:=$1F01;
  UnicodeLower1E00_1FFC[$1F0A]:=$1F02;
  UnicodeLower1E00_1FFC[$1F0B]:=$1F03;
  UnicodeLower1E00_1FFC[$1F0C]:=$1F04;
  UnicodeLower1E00_1FFC[$1F0D]:=$1F05;
  UnicodeLower1E00_1FFC[$1F0E]:=$1F06;
  UnicodeLower1E00_1FFC[$1F0F]:=$1F07;
  UnicodeLower1E00_1FFC[$1F18]:=$1F10;
  UnicodeLower1E00_1FFC[$1F19]:=$1F11;
  UnicodeLower1E00_1FFC[$1F1A]:=$1F12;
  UnicodeLower1E00_1FFC[$1F1B]:=$1F13;
  UnicodeLower1E00_1FFC[$1F1C]:=$1F14;
  UnicodeLower1E00_1FFC[$1F1D]:=$1F15;
  UnicodeLower1E00_1FFC[$1F28]:=$1F20;
  UnicodeLower1E00_1FFC[$1F29]:=$1F21;
  UnicodeLower1E00_1FFC[$1F2A]:=$1F22;
  UnicodeLower1E00_1FFC[$1F2B]:=$1F23;
  UnicodeLower1E00_1FFC[$1F2C]:=$1F24;
  UnicodeLower1E00_1FFC[$1F2D]:=$1F25;
  UnicodeLower1E00_1FFC[$1F2E]:=$1F26;
  UnicodeLower1E00_1FFC[$1F2F]:=$1F27;
  UnicodeLower1E00_1FFC[$1F38]:=$1F30;
  UnicodeLower1E00_1FFC[$1F39]:=$1F31;
  UnicodeLower1E00_1FFC[$1F3A]:=$1F32;
  UnicodeLower1E00_1FFC[$1F3B]:=$1F33;
  UnicodeLower1E00_1FFC[$1F3C]:=$1F34;
  UnicodeLower1E00_1FFC[$1F3D]:=$1F35;
  UnicodeLower1E00_1FFC[$1F3E]:=$1F36;
  UnicodeLower1E00_1FFC[$1F3F]:=$1F37;
  UnicodeLower1E00_1FFC[$1F48]:=$1F40;
  UnicodeLower1E00_1FFC[$1F49]:=$1F41;
  UnicodeLower1E00_1FFC[$1F4A]:=$1F42;
  UnicodeLower1E00_1FFC[$1F4B]:=$1F43;
  UnicodeLower1E00_1FFC[$1F4C]:=$1F44;
  UnicodeLower1E00_1FFC[$1F4D]:=$1F45;
  UnicodeLower1E00_1FFC[$1F59]:=$1F51;
  UnicodeLower1E00_1FFC[$1F5B]:=$1F53;
  UnicodeLower1E00_1FFC[$1F5D]:=$1F55;
  UnicodeLower1E00_1FFC[$1F5F]:=$1F57;
  UnicodeLower1E00_1FFC[$1F68]:=$1F60;
  UnicodeLower1E00_1FFC[$1F69]:=$1F61;
  UnicodeLower1E00_1FFC[$1F6A]:=$1F62;
  UnicodeLower1E00_1FFC[$1F6B]:=$1F63;
  UnicodeLower1E00_1FFC[$1F6C]:=$1F64;
  UnicodeLower1E00_1FFC[$1F6D]:=$1F65;
  UnicodeLower1E00_1FFC[$1F6E]:=$1F66;
  UnicodeLower1E00_1FFC[$1F6F]:=$1F67;
  UnicodeLower1E00_1FFC[$1F88]:=$1F80;
  UnicodeLower1E00_1FFC[$1F89]:=$1F81;
  UnicodeLower1E00_1FFC[$1F8A]:=$1F82;
  UnicodeLower1E00_1FFC[$1F8B]:=$1F83;
  UnicodeLower1E00_1FFC[$1F8C]:=$1F84;
  UnicodeLower1E00_1FFC[$1F8D]:=$1F85;
  UnicodeLower1E00_1FFC[$1F8E]:=$1F86;
  UnicodeLower1E00_1FFC[$1F8F]:=$1F87;
  UnicodeLower1E00_1FFC[$1F98]:=$1F90;
  UnicodeLower1E00_1FFC[$1F99]:=$1F91;
  UnicodeLower1E00_1FFC[$1F9A]:=$1F92;
  UnicodeLower1E00_1FFC[$1F9B]:=$1F93;
  UnicodeLower1E00_1FFC[$1F9C]:=$1F94;
  UnicodeLower1E00_1FFC[$1F9D]:=$1F95;
  UnicodeLower1E00_1FFC[$1F9E]:=$1F96;
  UnicodeLower1E00_1FFC[$1F9F]:=$1F97;
  UnicodeLower1E00_1FFC[$1FA8]:=$1FA0;
  UnicodeLower1E00_1FFC[$1FA9]:=$1FA1;
  UnicodeLower1E00_1FFC[$1FAA]:=$1FA2;
  UnicodeLower1E00_1FFC[$1FAB]:=$1FA3;
  UnicodeLower1E00_1FFC[$1FAC]:=$1FA4;
  UnicodeLower1E00_1FFC[$1FAD]:=$1FA5;
  UnicodeLower1E00_1FFC[$1FAE]:=$1FA6;
  UnicodeLower1E00_1FFC[$1FAF]:=$1FA7;
  UnicodeLower1E00_1FFC[$1FB8]:=$1FB0;
  UnicodeLower1E00_1FFC[$1FB9]:=$1FB1;
  UnicodeLower1E00_1FFC[$1FBA]:=$1F70;
  UnicodeLower1E00_1FFC[$1FBB]:=$1F71;
  UnicodeLower1E00_1FFC[$1FBC]:=$1FB3;
  UnicodeLower1E00_1FFC[$1FC8]:=$1F72;
  UnicodeLower1E00_1FFC[$1FC9]:=$1F73;
  UnicodeLower1E00_1FFC[$1FCA]:=$1F74;
  UnicodeLower1E00_1FFC[$1FCB]:=$1F75;
  UnicodeLower1E00_1FFC[$1FCC]:=$1FC3;
  UnicodeLower1E00_1FFC[$1FD8]:=$1FD0;
  UnicodeLower1E00_1FFC[$1FD9]:=$1FD1;
  UnicodeLower1E00_1FFC[$1FDA]:=$1F76;
  UnicodeLower1E00_1FFC[$1FDB]:=$1F77;
  UnicodeLower1E00_1FFC[$1FE8]:=$1FE0;
  UnicodeLower1E00_1FFC[$1FE9]:=$1FE1;
  UnicodeLower1E00_1FFC[$1FEA]:=$1F7A;
  UnicodeLower1E00_1FFC[$1FEB]:=$1F7B;
  UnicodeLower1E00_1FFC[$1FEC]:=$1FE5;
  UnicodeLower1E00_1FFC[$1FF8]:=$1F78;
  UnicodeLower1E00_1FFC[$1FF9]:=$1F79;
  UnicodeLower1E00_1FFC[$1FFA]:=$1F7C;
  UnicodeLower1E00_1FFC[$1FFB]:=$1F7D;
  UnicodeLower1E00_1FFC[$1FFC]:=$1FF3;

  for i:=Low(UnicodeLower2126_2183) to High(UnicodeLower2126_2183) do
    UnicodeLower2126_2183[i]:=i;
  UnicodeLower2126_2183[$2126]:=$03C9;
  UnicodeLower2126_2183[$212A]:=$006B;
  UnicodeLower2126_2183[$212B]:=$00E5;
  UnicodeLower2126_2183[$2132]:=$214E;
  UnicodeLower2126_2183[$2160]:=$2170;
  UnicodeLower2126_2183[$2161]:=$2171;
  UnicodeLower2126_2183[$2162]:=$2172;
  UnicodeLower2126_2183[$2163]:=$2173;
  UnicodeLower2126_2183[$2164]:=$2174;
  UnicodeLower2126_2183[$2165]:=$2175;
  UnicodeLower2126_2183[$2166]:=$2176;
  UnicodeLower2126_2183[$2167]:=$2177;
  UnicodeLower2126_2183[$2168]:=$2178;
  UnicodeLower2126_2183[$2169]:=$2179;
  UnicodeLower2126_2183[$216A]:=$217A;
  UnicodeLower2126_2183[$216B]:=$217B;
  UnicodeLower2126_2183[$216C]:=$217C;
  UnicodeLower2126_2183[$216D]:=$217D;
  UnicodeLower2126_2183[$216E]:=$217E;
  UnicodeLower2126_2183[$216F]:=$217F;
  UnicodeLower2126_2183[$2183]:=$2184;

  for i:=Low(UnicodeLower2C60_2CE2) to High(UnicodeLower2C60_2CE2) do
    UnicodeLower2C60_2CE2[i]:=i;
  UnicodeLower2C60_2CE2[$2C60]:=$2C61;
  UnicodeLower2C60_2CE2[$2C62]:=$026B;
  UnicodeLower2C60_2CE2[$2C63]:=$1D7D;
  UnicodeLower2C60_2CE2[$2C64]:=$027D;
  UnicodeLower2C60_2CE2[$2C67]:=$2C68;
  UnicodeLower2C60_2CE2[$2C69]:=$2C6A;
  UnicodeLower2C60_2CE2[$2C6B]:=$2C6C;
  UnicodeLower2C60_2CE2[$2C75]:=$2C76;
  UnicodeLower2C60_2CE2[$2C80]:=$2C81;
  UnicodeLower2C60_2CE2[$2C82]:=$2C83;
  UnicodeLower2C60_2CE2[$2C84]:=$2C85;
  UnicodeLower2C60_2CE2[$2C86]:=$2C87;
  UnicodeLower2C60_2CE2[$2C88]:=$2C89;
  UnicodeLower2C60_2CE2[$2C8A]:=$2C8B;
  UnicodeLower2C60_2CE2[$2C8C]:=$2C8D;
  UnicodeLower2C60_2CE2[$2C8E]:=$2C8F;
  UnicodeLower2C60_2CE2[$2C90]:=$2C91;
  UnicodeLower2C60_2CE2[$2C92]:=$2C93;
  UnicodeLower2C60_2CE2[$2C94]:=$2C95;
  UnicodeLower2C60_2CE2[$2C96]:=$2C97;
  UnicodeLower2C60_2CE2[$2C98]:=$2C99;
  UnicodeLower2C60_2CE2[$2C9A]:=$2C9B;
  UnicodeLower2C60_2CE2[$2C9C]:=$2C9D;
  UnicodeLower2C60_2CE2[$2C9E]:=$2C9F;
  UnicodeLower2C60_2CE2[$2CA0]:=$2CA1;
  UnicodeLower2C60_2CE2[$2CA2]:=$2CA3;
  UnicodeLower2C60_2CE2[$2CA4]:=$2CA5;
  UnicodeLower2C60_2CE2[$2CA6]:=$2CA7;
  UnicodeLower2C60_2CE2[$2CA8]:=$2CA9;
  UnicodeLower2C60_2CE2[$2CAA]:=$2CAB;
  UnicodeLower2C60_2CE2[$2CAC]:=$2CAD;
  UnicodeLower2C60_2CE2[$2CAE]:=$2CAF;
  UnicodeLower2C60_2CE2[$2CB0]:=$2CB1;
  UnicodeLower2C60_2CE2[$2CB2]:=$2CB3;
  UnicodeLower2C60_2CE2[$2CB4]:=$2CB5;
  UnicodeLower2C60_2CE2[$2CB6]:=$2CB7;
  UnicodeLower2C60_2CE2[$2CB8]:=$2CB9;
  UnicodeLower2C60_2CE2[$2CBA]:=$2CBB;
  UnicodeLower2C60_2CE2[$2CBC]:=$2CBD;
  UnicodeLower2C60_2CE2[$2CBE]:=$2CBF;
  UnicodeLower2C60_2CE2[$2CC0]:=$2CC1;
  UnicodeLower2C60_2CE2[$2CC2]:=$2CC3;
  UnicodeLower2C60_2CE2[$2CC4]:=$2CC5;
  UnicodeLower2C60_2CE2[$2CC6]:=$2CC7;
  UnicodeLower2C60_2CE2[$2CC8]:=$2CC9;
  UnicodeLower2C60_2CE2[$2CCA]:=$2CCB;
  UnicodeLower2C60_2CE2[$2CCC]:=$2CCD;
  UnicodeLower2C60_2CE2[$2CCE]:=$2CCF;
  UnicodeLower2C60_2CE2[$2CD0]:=$2CD1;
  UnicodeLower2C60_2CE2[$2CD2]:=$2CD3;
  UnicodeLower2C60_2CE2[$2CD4]:=$2CD5;
  UnicodeLower2C60_2CE2[$2CD6]:=$2CD7;
  UnicodeLower2C60_2CE2[$2CD8]:=$2CD9;
  UnicodeLower2C60_2CE2[$2CDA]:=$2CDB;
  UnicodeLower2C60_2CE2[$2CDC]:=$2CDD;
  UnicodeLower2C60_2CE2[$2CDE]:=$2CDF;
  UnicodeLower2C60_2CE2[$2CE0]:=$2CE1;
  UnicodeLower2C60_2CE2[$2CE2]:=$2CE3;
end;
{$endif}

function NeedRTLAnsi: boolean;
{$IFDEF WinCE}
// CP_UTF8 is missing in the windows unit of the Windows CE RTL
const
  CP_UTF8 = 65001;
{$ENDIF}
{$IFNDEF Windows}
var
  Lang: String;
  i: LongInt;
  Encoding: String;
{$ENDIF}
begin
  if FNeedRTLAnsiValid then
    exit(FNeedRTLAnsi);
  {$IFDEF Windows}
  FNeedRTLAnsi:=GetACP<>CP_UTF8;
  {$ELSE}
  FNeedRTLAnsi:=false;
  Lang := SysUtils.GetEnvironmentVariable('LC_ALL');
  if Length(lang) = 0 then
  begin
    Lang := SysUtils.GetEnvironmentVariable('LC_MESSAGES');
    if Length(Lang) = 0 then
    begin
      Lang := SysUtils.GetEnvironmentVariable('LANG');
    end;
  end;
  i:=System.Pos('.',Lang);
  if (i>0) then begin
    Encoding:=copy(Lang,i+1,length(Lang)-i);
    FNeedRTLAnsi:=(SysUtils.CompareText(Encoding,'UTF-8')=0)
              or (SysUtils.CompareText(Encoding,'UTF8')=0);
  end;
  {$ENDIF}
  FNeedRTLAnsiValid:=true;
  Result:=FNeedRTLAnsi;
end;

procedure SetNeedRTLAnsi(NewValue: boolean);
begin
  FNeedRTLAnsi:=NewValue;
  FNeedRTLAnsiValid:=true;
end;

function IsASCII(const s: string): boolean; inline;
var
  i: Integer;
begin
  for i:=1 to length(s) do if ord(s[i])>127 then exit(false);
  Result:=true;
end;

function UTF8ToSys(const s: string): string;
begin
  if NeedRTLAnsi and (not IsASCII(s)) then
    Result:=UTF8ToAnsi(s)
  else
    Result:=s;
end;

function SysToUTF8(const s: string): string;
begin
  if NeedRTLAnsi and (not IsASCII(s)) then
    Result:=AnsiToUTF8(s)
  else
    Result:=s;
end;

function UTF8CharacterLength(p: PChar): integer;
begin
  if p<>nil then begin
    if ord(p^)<%11000000 then begin
      // regular single byte character (#0 is a character, this is pascal ;)
      Result:=1;
    end
    else if ((ord(p^) and %11100000) = %11000000) then begin
      // could be 2 byte character
      if (ord(p[1]) and %11000000) = %10000000 then
        Result:=2
      else
        Result:=1;
    end
    else if ((ord(p^) and %11110000) = %11100000) then begin
      // could be 3 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000) then
        Result:=3
      else
        Result:=1;
    end
    else if ((ord(p^) and %11111000) = %11110000) then begin
      // could be 4 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000)
      and ((ord(p[3]) and %11000000) = %10000000) then
        Result:=4
      else
        Result:=1;
    end
    else
      Result:=1
  end else
    Result:=0;
end;

function UTF8Length(const s: string): PtrInt;
begin
  Result:=UTF8Length(PChar(s),length(s));
end;

function UTF8Length(p: PChar; ByteCount: PtrInt): PtrInt;
var
  CharLen: LongInt;
begin
  Result:=0;
  while (ByteCount>0) do begin
    inc(Result);
    CharLen:=UTF8CharacterLength(p);
    inc(p,CharLen);
    dec(ByteCount,CharLen);
  end;
end;

function UTF8CharacterToUnicode(p: PChar; out CharLen: integer): Cardinal;
begin
  if p<>nil then begin
    if ord(p^)<%11000000 then begin
      // regular single byte character (#0 is a normal char, this is pascal ;)
      Result:=ord(p^);
      CharLen:=1;
    end
    else if ((ord(p^) and %11100000) = %11000000) then begin
      // could be double byte character
      if (ord(p[1]) and %11000000) = %10000000 then begin
        Result:=((ord(p^) and %00011111) shl 6)
                or (ord(p[1]) and %00111111);
        CharLen:=2;
      end else begin
        Result:=ord(p^);
        CharLen:=1;
      end;
    end
    else if ((ord(p^) and %11110000) = %11100000) then begin
      // could be triple byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000) then begin
        Result:=((ord(p^) and %00011111) shl 12)
                or ((ord(p[1]) and %00111111) shl 6)
                or (ord(p[2]) and %00111111);
        CharLen:=3;
      end else begin
        Result:=ord(p^);
        CharLen:=1;
      end;
    end
    else if ((ord(p^) and %11111000) = %11110000) then begin
      // could be 4 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000)
      and ((ord(p[3]) and %11000000) = %10000000) then begin
        Result:=((ord(p^) and %00001111) shl 18)
                or ((ord(p[1]) and %00111111) shl 12)
                or ((ord(p[2]) and %00111111) shl 6)
                or (ord(p[3]) and %00111111);
        CharLen:=4;
      end else begin
        Result:=ord(p^);
        CharLen:=1;
      end;
    end
    else begin
      // invalid character
      Result:=ord(p^);
      CharLen:=1;
    end;
  end else begin
    Result:=0;
    CharLen:=0;
  end;
end;

function UnicodeToUTF8(u: cardinal; Buf: PChar): integer;

  procedure RaiseInvalidUnicode;
  begin
    raise Exception.Create('UnicodeToUTF8: invalid unicode: '+IntToStr(u));
  end;

begin
  Result:=UnicodeToUTF8SkipErrors(u,Buf);
  if Result=0 then
    RaiseInvalidUnicode;
end;

function UnicodeToUTF8SkipErrors(u: cardinal; Buf: PChar): integer;
begin
  case u of
    0..$7f:
      begin
        Result:=1;
        Buf[0]:=char(byte(u));
      end;
    $80..$7ff:
      begin
        Result:=2;
        Buf[0]:=char(byte($c0 or (u shr 6)));
        Buf[1]:=char(byte($80 or (u and $3f)));
      end;
    $800..$ffff:
      begin
        Result:=3;
        Buf[0]:=char(byte($e0 or (u shr 12)));
        Buf[1]:=char(byte((u shr 6) and $3f) or $80);
        Buf[2]:=char(byte(u and $3f) or $80);
      end;
    $10000..$10ffff:
      begin
        Result:=4;
        Buf[0]:=char(byte($f0 or (u shr 18)));
        Buf[1]:=char(byte((u shr 12) and $3f) or $80);
        Buf[2]:=char(byte((u shr 6) and $3f) or $80);
        Buf[3]:=char(byte(u and $3f) or $80);
      end;
  else
    Result:=0;
  end;
end;

{$ifdef LAZUTF8_USE_TABLES}
function UnicodeLowercase(u: cardinal): cardinal;
begin
  if u<$00C0 then begin
    // most common
    if (u>=$0041) and (u<=$0061) then
      Result:=u+32
    else
      Result:=u;
  end else
    case u of
    $00C0..$00DE: Result:=UnicodeLower00C0_00DE[u];
    $0100..$024E: Result:=UnicodeLower0100_024E[u];
    $0386..$03AB: Result:=UnicodeLower0386_03AB[u];
    $03D8..$042F: Result:=UnicodeLower03D8_042F[u];
    $0460..$0512: Result:=UnicodeLower0460_0512[u];
    $0531..$0556: Result:=u+48;
    $10A0..$10C5: Result:=u+7264;
    $1E00..$1FFC: Result:=UnicodeLower1E00_1FFC[u];
    $2126..$2183: Result:=UnicodeLower2126_2183[u];
    $24B6..$24CF: Result:=u+26;
    $2C00..$2C2E: Result:=u+48;
    $2C60..$2CE2: Result:=UnicodeLower2C60_2CE2[u];
    $FF21..$FF3A: Result:=u+32;
    else          Result:=u;
  end;
end;



function UTF8LowercaseDynLength(const s: string): string;
var
  Buf: shortstring;
  SrcPos: PtrInt;
  DstPos: PtrInt;
  CharLen: integer;
  OldCode: LongWord;
  NewCode: LongWord;
begin
  // first compute needed length
  SrcPos:=1;
  DstPos:=1;
  while SrcPos<=length(s) do begin
    case s[SrcPos] of
    #192..#240:
      begin
        OldCode:=UTF8CharacterToUnicode(@s[SrcPos],CharLen);
        NewCode:=UnicodeLowercase(OldCode);
        if NewCode=OldCode then begin
          inc(DstPos,CharLen);
        end else begin
          inc(DstPos,UnicodeToUTF8(NewCode,@Buf[1]));
        end;
        inc(SrcPos,CharLen);
      end;
    else
      inc(SrcPos);
      inc(DstPos);
    end;
  end;
  SetLength(Result,DstPos-1);
  if Result='' then exit;
  // create the new string
  SrcPos:=1;
  DstPos:=1;
  while SrcPos<=length(s) do begin
    case s[SrcPos] of
    #192..#240:
      begin
        OldCode:=UTF8CharacterToUnicode(@s[SrcPos],CharLen);
        NewCode:=UnicodeLowercase(OldCode);
        if NewCode=OldCode then begin
          System.Move(s[SrcPos],Result[DstPos],CharLen);
          inc(DstPos,CharLen);
        end else begin
          inc(DstPos,UnicodeToUTF8(NewCode,@Result[DstPos]));
        end;
        inc(SrcPos,CharLen);
      end;
    else
      Result[DstPos]:=s[SrcPos];
      inc(SrcPos);
      inc(DstPos);
    end;
  end;
end;

function UTF8LowerCaseMattias(const s: utf8string): utf8string;
var
  i: PtrInt;
  CharLen: integer;
  OldCode: LongWord;
  NewCode: LongWord;
  NewCharLen: integer;
  Changed: Boolean;
  p: PChar;
begin
  Result:=s;
  if Result='' then exit;
  Changed:=false;
  p:=PChar(Result);
  repeat
    case p^ of
    #0:
      if p-PChar(Result)=length(Result) then
        exit
      else
        inc(p);
    'A'..'Z': // First ASCII chars
      begin
        if not Changed then begin
          i:=p-PChar(Result)+1;
          UniqueString(Result);
          Changed:=true;
          p:=@Result[i];
        end;
        p^:=chr(ord(p^)+32);
        inc(p);
      end;

    #192..#240: // Now chars with multiple bytes
      begin
        OldCode:=UTF8CharacterToUnicode(p,CharLen);
        NewCode:=UnicodeLowercase(OldCode);
        if NewCode<>OldCode then begin
          if not Changed then begin
            i:=p-PChar(Result)+1;
            UniqueString(Result);
            Changed:=true;
            p:=@Result[i];
          end;
          NewCharLen:=UnicodeToUTF8(NewCode,p);
          if CharLen<>NewCharLen then begin
            // string size changed => use slower function
            Result:=UTF8LowercaseDynLength(s);
            exit;
          end;
        end;
        inc(p,CharLen);
      end;
    else
      inc(p);
    end;
  until false;
end;
{$endif}

function UTF8LowerCase2(const AInStr: utf8string; ALocale: utf8string =''): utf8string;
const
  ResultSizeIncr = 10;
var
  CounterDiff, ExtraResultBytes: PtrInt;
  InStr, InStrEnd, OutStr: PChar;
  // Language identification
  IsTurkish: Boolean;
  c, d: Char;

  procedure IncreaseResult;
  begin
    if -CounterDiff < ExtraResultBytes - 1 then exit;
    OutStr := PChar(OutStr - PChar(Result));
    SetLength(Result,Length(Result)+ResultSizeIncr);// Increase the buffer
    OutStr := PtrInt(OutStr) + PChar(Result);
    inc(ExtraResultBytes, ResultSizeIncr);
  end;

  procedure HandleDualByte; inline;
  begin
    case c of
      #$C3:
        begin
          if d in [#$80..#$9E] then
            OutStr[-1]  := chr(ord(d) + $20);
        end;
      // $C481..$C4A9: if OldChar mod 2 = 0 then NewChar := OldChar + 1;
      // Turkish capital dotted i to small dotted i
      // $C4B0 -> 'i'
      // $C4B1 turkish lowercase undotted ı
      // $C4B2..$C4B6: if OldChar mod 2 = 0 then NewChar := OldChar + 1;
      // $C4B7: ĸ => K ?
      // $C4B8..$C588: if OldChar mod 2 = 1 then NewChar := OldChar + 1;
      #$C4:
        begin
          case d of
            #$81..#$A9, #$B2..#$B6: //0
              begin
                if ord(d) mod 2 = 0 then
                  OutStr[-1]  := chr(ord(d) + 1);
              end;
            #$B8..#$FF: //1
              begin
                if ord(d) mod 2 = 1 then
                  OutStr[-1]  := chr(ord(d) + 1);
              end;
            #$B0:            // Turkish capital dotted i to small dotted i
              begin
                dec(OutStr, 1);
                OutStr[-1] := 'i';
                inc(CounterDiff, 1);
                //break; // InSTr and OutStr are pointing to different indexes
              end;
          end;
        end;
      // $C589 ŉ => ?
      // $C58A..$C5B7: if OldChar mod 2 = 0 then NewChar := OldChar + 1;
      // $C5B8:        NewChar := $C3BF; // Ÿ
      // $C5B9..$C8B3: if OldChar mod 2 = 1 then NewChar := OldChar + 1;
      #$C5:
        begin
          case d of
            #$8A..#$B7: //0
              begin
                if ord(d) mod 2 = 0 then
                  OutStr[-1]  := chr(ord(d) + 1);
              end;
            #$00..#$88, #$B9..#$FF: //1
              begin
                if ord(d) mod 2 = 1 then
                  OutStr[-1]  := chr(ord(d) + 1);
              end;
            #$B8:  // Ÿ
            begin
              OutStr[-2] := #$C3;
              OutStr[-1] := #$BF;
            end;
          end;
        end;
      #$C6..#$C7:
        begin
          if ord(d) mod 2 = 1 then
            OutStr[-1]  := chr(ord(d) + 1);
        end;
      #$C8:
        begin
          if (d in [#$00..#$B3]) and (ord(d) mod 2 = 1) then
            OutStr[-1]  := chr(ord(d) + 1);
        end;
      // $CE91..$CE9F: NewChar := OldChar + $20; // Greek Characters
      // $CEA0..$CEA9: NewChar := OldChar + $E0; // Greek Characters
      #$CE:   // Greek Characters
        begin
          case d of
            #$91..#$9F:
              begin
                OutStr[-1]  := chr(ord(d) + $20);
              end;
            #$A0..#$A9:
              begin
                OutStr[-2]  := chr(ord(c)+1);
                OutStr[-1]  := chr(ord(d) - $10);
              end;
          end;
        end;
      // $D080..$D08F: NewChar := OldChar + $110; // Cyrillic alphabet
      // $D090..$D09F: NewChar := OldChar + $20; // Cyrillic alphabet
      // $D0A0..$D0AF: NewChar := OldChar + $E0; // Cyrillic alphabet
      #$D0:   // Cyrillic alphabet
        begin
          case d of
            #$80..#$8F:
              begin
                OutStr[-2]  := chr(ord(c)+1);
                OutStr[-1]  := chr(ord(d) + $10);
              end;
            #$90..#$9F:
              begin
                OutStr[-1]  := chr(ord(d) + $20);
              end;
            #$A0..#$AF:
              begin
                OutStr[-2]  := chr(ord(c)+1);
                OutStr[-1]  := chr(ord(d) - $20);
              end;
          end;
        end;
        // Archaic and non-slavic cyrillic 460-47F = D1A0-D1BF
        // These require just adding 1 to get the lowercase
        #$D1:
          begin
            case d of
              #$A0..#$BF:
              begin
                if ord(d) mod 2 = 0 then
                  OutStr[-1]  := chr(ord(d) + 1);
              end;
            end;
          end;
        // Archaic and non-slavic cyrillic 480-4BF = D280-D2BF
        // These mostly require just adding 1 to get the lowercase
        #$D2:
          begin
            case d of
              #$80: OutStr[-1]  := chr(ord(d) + 1);
              // #$81 is already lowercase
              // #$82-#$89 ???
              #$8A..#$BF:
              begin
                if ord(d) mod 2 = 0 then
                  OutStr[-1]  := chr(ord(d) + 1);
              end;
            end;
          end;
    end;
  end;

  procedure HandleTripplByte; inline;
  begin
    //case c of
      // Georgian codepoints 10A0-10C5 => 2D00-2D25
      // In UTF-8 this is:
      // E1 82 A0 - E1 82 BF => E2 B4 80 - E2 B4 9F
      // E1 83 80 - E1 83 85 => E2 B4 A0 - E2 B4 A5
      //#$E1:
      //  begin
          c := InStr[-1];
          case d of
            #$82:
              begin
                if (c in [#$A0..#$BF]) then
                begin
                  OutStr[-3] := #$E2;
                  OutStr[-2] := #$B4;
                  OutStr[-1] := chr(ord(c) - $20);
                end
              end;
            #$83:
              begin
                if (c in [#$80..#$85]) then
                begin
                  OutStr[-3] := #$E2;
                  OutStr[-2] := #$B4;
                  OutStr[-1] := chr(ord(c) + $20);
                end;
              end;
          end;
    //    end;
    //end;
  end;

begin
  Result:=AInStr;
  InStr := PChar(AInStr);
  InStrEnd := InStr + length(AInStr); // points behind last char

  // Does a fast initial parsing of the string to maybe avoid doing
  // UniqueString if the resulting string will be identical
  while (InStr < InStrEnd) do
  begin
    c := InStr^;
    case c of
    'A'..'Z',#$C3, #$C4, #$C5..#$C8, #$CE, #$D0..#$D2, #$E1: Break;
    // already lower, or otherwhise not affected
    else
      inc(InStr);
    end;
  end;

  if InStr >= InStrEnd then
    exit;

  // Language identification
  IsTurkish := ALocale = 'tu';

  ExtraResultBytes := 0;
  UniqueString(Result);
  OutStr := PChar(Result) + (InStr - PChar(AInStr));
  CounterDiff := 0;

  while(true) do begin
    // Alternate between 2 loops, depnding on CounterDiff, less IF inside the loops

    while(true) do begin
      (* InStr and OutStr pointing to te same relative position.
         Result at/after OutStr is a copy of AInStr. Using OutStr as Source
         The loop will be exited via break, if InStr and OutStr are increased in different steps
      *)
      c := OutStr^;
      inc(InStr);
      inc(OutStr);
      case c of  // if NOT TABLE
        #0:
          begin
            if InStr >= InStrEnd then begin
              if ExtraResultBytes <> 0 then
                SetLength(Result,OutStr-1 - PChar(Result));
              exit;
            end;
        end;
        'A'..'Z':
          begin
            { First ASCII chars }
            // Special turkish handling
            // capital undotted I to small undotted i
            if IsTurkish and (c = 'I') then
            begin
              IncreaseResult;
              OutStr[-1] := #$C4;
              OutStr^ := #$B1;
              inc(OutStr);
              dec(CounterDiff);
              break; // InSTr and OutStr are pointing to different indexes
            end
            else
            begin
                OutStr[-1] := chr(ord(c)+32);
            end;
          end;
        #$c3..#$E1:
          begin
            case c of
              // Latin Characters 0000–0FFF http://en.wikibooks.org/wiki/Unicode/Character_reference/0000-0FFF
              // $C380..$C39E: NewChar := OldChar + $20;
              // $C39F: ß already lowercase
              #$c3..#$D2:
                begin
                  d := OutStr[0]; // 2nd char in 2 byte utf8
                  inc(InStr);
                  inc(OutStr);
                  HandleDualByte;
                  if CounterDiff <> 0 then break;
                end; //c3..d2
              #$e1:
                begin
                  d := OutStr[0]; // 2nd char in 2 byte utf8
                  inc(InStr, 2);
                  inc(OutStr, 2);
                  HandleTripplByte;
                end;
            end;
          end;
      end; // Case c
    end;

    while (true) do begin
      (* InStr and OutStr pointing to te different relative position.
         All chars from AInStr must be copied to their new pos in Result
         The loop will be exited via break, if InStr and OutStr are syncronized again
      *)
      c := InStr^;
      case c of  // if NOT TABLE
        #0:
        begin
          if InStr >= InStrEnd then
          begin
            SetLength(Result,OutStr - PChar(Result));
            exit;
          end;
          OutStr^:=c;
          inc(InStr);
          inc(OutStr);
        end;
        'A'..'Z':
          begin
            { First ASCII chars }
            // Special turkish handling
            // capital undotted I to small undotted i
            if IsTurkish and (c = 'I') then
            begin
              IncreaseResult;
              OutStr^ := #$C4;
              inc(OutStr);
              OutStr^ := #$B1;
              inc(InStr);
              inc(OutStr);
              dec(CounterDiff);
              if CounterDiff = 0 then break;
            end
            else
            begin
              OutStr^ := chr(ord(c)+32);
              inc(InStr);
              inc(OutStr);
            end;
          end;
         #$c3..#$D2:
            begin
              OutStr^  := c;
              d := InStr[1]; // 2nd char in 2 byte utf8
              OutStr[1]  := d;
              inc(InStr, 2);
              inc(OutStr, 2);
              HandleDualByte;
              if CounterDiff = 0 then break;
            end; // c3..d2
          #$e1:
            begin
              OutStr^  := c;
              d := InStr[1]; // 2nd char in 2 byte utf8
              OutStr[1]  := d;
              OutStr[2]  := InStr[2];
              inc(InStr, 3);
              inc(OutStr, 3);
              HandleTripplByte;
            end;
        else
          begin
            // Copy the character if the string was disaligned by previous changes
            OutStr^:=c;
            inc(InStr);
            inc(OutStr);
          end;
      end; // Case InStr^
    end;
  end;
end;

{
  AInStr - The input string
  ALanguage - The language. Use '' for maximum speed if one desires to ignore the language
              The language should be specified in the format from ISO 639-1,
              which uses 2 characters to represent each language.
              If the language has no code in ISO 639-1, then the 3-chars code
              from ISO 639-2 should be used.
              Example: "tr" - Turkish language locale

  Data from here: ftp://ftp.unicode.org/Public/UNIDATA/UnicodeData.txt

  The columns in the file UnicodeData.txt are explained here:
  http://www.ksu.ru/eng/departments/ktk/test/perl/lib/unicode/UCDFF301.html#Case Mappings
}
function UTF8LowerCase(const AInStr: utf8string; ALanguage: utf8string=''): utf8string;
var
  CounterDiff: PtrInt;
  InStr, InStrEnd, OutStr: PChar;
  // Language identification
  IsTurkish: Boolean;
  c1, c2, c3, new_c1, new_c2, new_c3: Char;
begin
  Result:=AInStr;
  InStr := PChar(AInStr);
  InStrEnd := InStr + length(AInStr); // points behind last char

  // Does a fast initial parsing of the string to maybe avoid doing
  // UniqueString if the resulting string will be identical
  while (InStr < InStrEnd) do
  begin
    c1 := InStr^;
    case c1 of
    'A'..'Z': Break;
    #$C3..#$C9, #$CE, #$D0..#$D5, #$E1..#$E2,#$E5:
    begin
      c2 := InStr[1];
      case c1 of
      #$C3: if c2 in [#$80..#$9E] then Break;
      #$C4:
      begin
        case c2 of
        #$80..#$AF, #$B2..#$B6: if ord(c2) mod 2 = 0 then Break;
        #$B8..#$FF: if ord(c2) mod 2 = 1 then Break;
        #$B0: Break;
        end;
      end;
      #$C5:
      begin
        case c2 of
          #$8A..#$B7: if ord(c2) mod 2 = 0 then Break;
          #$00..#$88, #$B9..#$FF: if ord(c2) mod 2 = 1 then Break;
          #$B8: Break;
        end;
      end;
      // Process E5 to avoid stopping on chinese chars
      #$E5: if (c2 = #$BC) and (c3 in [#$A1..#$BA]) then Break;
      // Others are too complex, better not to pre-inspect them
      else
        Break;
      end;
      // already lower, or otherwhise not affected
    end;
    end;
    inc(InStr);
  end;

  if InStr >= InStrEnd then Exit;

  // Language identification
  IsTurkish := (ALanguage = 'tr') or (ALanguage = 'az'); // Turkish and Azeri have a special handling

  UniqueString(Result);
  OutStr := PChar(Result) + (InStr - PChar(AInStr));
  CounterDiff := 0;

  while InStr < InStrEnd do
  begin
    c1 := InStr^;
    case c1 of
      // codepoints      UTF-8 range           Description                Case change
      // $0041..$005A    $41..$5A              Capital ASCII              X+$20
      'A'..'Z':
      begin
        { First ASCII chars }
        // Special turkish handling
        // capital undotted I to small undotted i
        if IsTurkish and (c1 = 'I') then
        begin
          OutStr := PChar(OutStr - PChar(Result));
          SetLength(Result,Length(Result)+1);// Increase the buffer
          OutStr := PtrInt(OutStr) + PChar(Result);
          OutStr^ := #$C4;
          inc(OutStr);
          OutStr^ := #$B1;
          dec(CounterDiff);
        end
        else
        begin
          OutStr^ := chr(ord(c1)+32);
        end;
        inc(InStr);
        inc(OutStr);
      end;

      // Chars with 2-bytes which might be modified
      #$C3..#$D5:
      begin
        c2 := InStr[1];
        new_c1 := c1;
        new_c2 := c2;
        case c1 of
        // Latin Characters 0000–0FFF http://en.wikibooks.org/wiki/Unicode/Character_reference/0000-0FFF
        // codepoints      UTF-8 range           Description                Case change
        // $00C0..$00D6    C3 80..C3 96          Capital Latin with accents X+$20
        // $D7             C3 97                 Multiplication Sign        N/A
        // $00D8..$00DE    C3 98..C3 9E          Capital Latin with accents X+$20
        // $DF             C3 9F                 German beta ß              already lowercase
        #$C3:
        begin
          case c2 of
          #$80..#$96, #$98..#$9E: new_c2 := chr(ord(c2) + $20)
          end;
        end;
        // $0100..$012F    C4 80..C4 AF        Capital/Small Latin accents  if mod 2 = 0 then X+1
        // $0130..$0131    C4 B0..C4 B1        Turkish
        //  C4 B0 turkish uppercase dotted i -> 'i'
        //  C4 B1 turkish lowercase undotted ı
        // $0132..$0137    C4 B2..C4 B7        Capital/Small Latin accents  if mod 2 = 0 then X+1
        // $0138           C4 B8               ĸ                            N/A
        // $0139..$024F    C4 B9..C5 88        Capital/Small Latin accents  if mod 2 = 1 then X+1
        #$C4:
        begin
          case c2 of
            #$80..#$AF, #$B2..#$B7: if ord(c2) mod 2 = 0 then new_c2 := chr(ord(c2) + 1);
            #$B0: // Turkish
            begin
              OutStr^ := 'i';
              inc(InStr, 2);
              inc(OutStr);
              inc(CounterDiff, 1);
              Continue;
            end;
            #$B9..#$BE: if ord(c2) mod 2 = 1 then new_c2 := chr(ord(c2) + 1);
            #$BF: // This crosses the borders between the first byte of the UTF-8 char
            begin
              new_c1 := #$C5;
              new_c2 := #$80;
            end;
          end;
        end;
        // $C589 ŉ
        // $C58A..$C5B7: if OldChar mod 2 = 0 then NewChar := OldChar + 1;
        // $C5B8:        NewChar := $C3BF; // Ÿ
        // $C5B9..$C8B3: if OldChar mod 2 = 1 then NewChar := OldChar + 1;
        #$C5:
        begin
          case c2 of
            #$8A..#$B7: //0
            begin
              if ord(c2) mod 2 = 0 then
                new_c2 := chr(ord(c2) + 1);
            end;
            #$00..#$88, #$B9..#$BE: //1
            begin
              if ord(c2) mod 2 = 1 then
                new_c2 := chr(ord(c2) + 1);
            end;
            #$B8:  // Ÿ
            begin
              new_c1 := #$C3;
              new_c2 := #$BF;
            end;
          end;
        end;
        {A convoluted part: C6 80..C6 8F

        0180;LATIN SMALL LETTER B WITH STROKE;Ll;0;L;;;;;N;LATIN SMALL LETTER B BAR;;0243;;0243
        0181;LATIN CAPITAL LETTER B WITH HOOK;Lu;0;L;;;;;N;LATIN CAPITAL LETTER B HOOK;;;0253; => C6 81=>C9 93
        0182;LATIN CAPITAL LETTER B WITH TOPBAR;Lu;0;L;;;;;N;LATIN CAPITAL LETTER B TOPBAR;;;0183;
        0183;LATIN SMALL LETTER B WITH TOPBAR;Ll;0;L;;;;;N;LATIN SMALL LETTER B TOPBAR;;0182;;0182
        0184;LATIN CAPITAL LETTER TONE SIX;Lu;0;L;;;;;N;;;;0185;
        0185;LATIN SMALL LETTER TONE SIX;Ll;0;L;;;;;N;;;0184;;0184
        0186;LATIN CAPITAL LETTER OPEN O;Lu;0;L;;;;;N;;;;0254; ==> C9 94
        0187;LATIN CAPITAL LETTER C WITH HOOK;Lu;0;L;;;;;N;LATIN CAPITAL LETTER C HOOK;;;0188;
        0188;LATIN SMALL LETTER C WITH HOOK;Ll;0;L;;;;;N;LATIN SMALL LETTER C HOOK;;0187;;0187
        0189;LATIN CAPITAL LETTER AFRICAN D;Lu;0;L;;;;;N;;;;0256; => C9 96
        018A;LATIN CAPITAL LETTER D WITH HOOK;Lu;0;L;;;;;N;LATIN CAPITAL LETTER D HOOK;;;0257; => C9 97
        018B;LATIN CAPITAL LETTER D WITH TOPBAR;Lu;0;L;;;;;N;LATIN CAPITAL LETTER D TOPBAR;;;018C;
        018C;LATIN SMALL LETTER D WITH TOPBAR;Ll;0;L;;;;;N;LATIN SMALL LETTER D TOPBAR;;018B;;018B
        018D;LATIN SMALL LETTER TURNED DELTA;Ll;0;L;;;;;N;;;;;
        018E;LATIN CAPITAL LETTER REVERSED E;Lu;0;L;;;;;N;LATIN CAPITAL LETTER TURNED E;;;01DD; => C7 9D
        018F;LATIN CAPITAL LETTER SCHWA;Lu;0;L;;;;;N;;;;0259; => C9 99
        }
        #$C6:
        begin
          case c2 of
            #$81:
            begin
              new_c1 := #$C9;
              new_c2 := #$93;
            end;
            #$82..#$85:
            begin
              if ord(c2) mod 2 = 0 then
                new_c2 := chr(ord(c2) + 1);
            end;
            #$87..#$88,#$8B..#$8C:
            begin
              if ord(c2) mod 2 = 1 then
                new_c2 := chr(ord(c2) + 1);
            end;
            #$86:
            begin
              new_c1 := #$C9;
              new_c2 := #$94;
            end;
            #$89:
            begin
              new_c1 := #$C9;
              new_c2 := #$96;
            end;
            #$8A:
            begin
              new_c1 := #$C9;
              new_c2 := #$97;
            end;
            #$8E:
            begin
              new_c1 := #$C7;
              new_c2 := #$9D;
            end;
            #$8F:
            begin
              new_c1 := #$C9;
              new_c2 := #$99;
            end;
          {
          And also C6 90..C6 9F

          0190;LATIN CAPITAL LETTER OPEN E;Lu;0;L;;;;;N;LATIN CAPITAL LETTER EPSILON;;;025B; => C9 9B
          0191;LATIN CAPITAL LETTER F WITH HOOK;Lu;0;L;;;;;N;LATIN CAPITAL LETTER F HOOK;;;0192; => +1
          0192;LATIN SMALL LETTER F WITH HOOK;Ll;0;L;;;;;N;LATIN SMALL LETTER SCRIPT F;;0191;;0191 <=
          0193;LATIN CAPITAL LETTER G WITH HOOK;Lu;0;L;;;;;N;LATIN CAPITAL LETTER G HOOK;;;0260; => C9 A0
          0194;LATIN CAPITAL LETTER GAMMA;Lu;0;L;;;;;N;;;;0263; => C9 A3
          0195;LATIN SMALL LETTER HV;Ll;0;L;;;;;N;LATIN SMALL LETTER H V;;01F6;;01F6 <=
          0196;LATIN CAPITAL LETTER IOTA;Lu;0;L;;;;;N;;;;0269; => C9 A9
          0197;LATIN CAPITAL LETTER I WITH STROKE;Lu;0;L;;;;;N;LATIN CAPITAL LETTER BARRED I;;;0268; => C9 A8
          0198;LATIN CAPITAL LETTER K WITH HOOK;Lu;0;L;;;;;N;LATIN CAPITAL LETTER K HOOK;;;0199; => +1
          0199;LATIN SMALL LETTER K WITH HOOK;Ll;0;L;;;;;N;LATIN SMALL LETTER K HOOK;;0198;;0198 <=
          019A;LATIN SMALL LETTER L WITH BAR;Ll;0;L;;;;;N;LATIN SMALL LETTER BARRED L;;023D;;023D <=
          019B;LATIN SMALL LETTER LAMBDA WITH STROKE;Ll;0;L;;;;;N;LATIN SMALL LETTER BARRED LAMBDA;;;; <=
          019C;LATIN CAPITAL LETTER TURNED M;Lu;0;L;;;;;N;;;;026F; => C9 AF
          019D;LATIN CAPITAL LETTER N WITH LEFT HOOK;Lu;0;L;;;;;N;LATIN CAPITAL LETTER N HOOK;;;0272; => C9 B2
          019E;LATIN SMALL LETTER N WITH LONG RIGHT LEG;Ll;0;L;;;;;N;;;0220;;0220 <=
          019F;LATIN CAPITAL LETTER O WITH MIDDLE TILDE;Lu;0;L;;;;;N;LATIN CAPITAL LETTER BARRED O;;;0275; => C9 B5
          }
          #$90:
          begin
            new_c1 := #$C9;
            new_c2 := #$9B;
          end;
          #$91, #$98: new_c2 := chr(ord(c2)+1);
          #$93:
          begin
            new_c1 := #$C9;
            new_c2 := #$A0;
          end;
          #$94:
          begin
            new_c1 := #$C9;
            new_c2 := #$A3;
          end;
          #$96:
          begin
            new_c1 := #$C9;
            new_c2 := #$A9;
          end;
          #$97:
          begin
            new_c1 := #$C9;
            new_c2 := #$A8;
          end;
          #$9C:
          begin
            new_c1 := #$C9;
            new_c2 := #$AF;
          end;
          #$9D:
          begin
            new_c1 := #$C9;
            new_c2 := #$B2;
          end;
          #$9F:
          begin
            new_c1 := #$C9;
            new_c2 := #$B5;
          end;
          {
          And also C6 A0..C6 AF

          01A0;LATIN CAPITAL LETTER O WITH HORN;Lu;0;L;004F 031B;;;;N;LATIN CAPITAL LETTER O HORN;;;01A1; => +1
          01A1;LATIN SMALL LETTER O WITH HORN;Ll;0;L;006F 031B;;;;N;LATIN SMALL LETTER O HORN;;01A0;;01A0 <=
          01A2;LATIN CAPITAL LETTER OI;Lu;0;L;;;;;N;LATIN CAPITAL LETTER O I;;;01A3; => +1
          01A3;LATIN SMALL LETTER OI;Ll;0;L;;;;;N;LATIN SMALL LETTER O I;;01A2;;01A2 <=
          01A4;LATIN CAPITAL LETTER P WITH HOOK;Lu;0;L;;;;;N;LATIN CAPITAL LETTER P HOOK;;;01A5; => +1
          01A5;LATIN SMALL LETTER P WITH HOOK;Ll;0;L;;;;;N;LATIN SMALL LETTER P HOOK;;01A4;;01A4 <=
          01A6;LATIN LETTER YR;Lu;0;L;;;;;N;LATIN LETTER Y R;;;0280; => CA 80
          01A7;LATIN CAPITAL LETTER TONE TWO;Lu;0;L;;;;;N;;;;01A8; => +1
          01A8;LATIN SMALL LETTER TONE TWO;Ll;0;L;;;;;N;;;01A7;;01A7 <=
          01A9;LATIN CAPITAL LETTER ESH;Lu;0;L;;;;;N;;;;0283; => CA 83
          01AA;LATIN LETTER REVERSED ESH LOOP;Ll;0;L;;;;;N;;;;;
          01AB;LATIN SMALL LETTER T WITH PALATAL HOOK;Ll;0;L;;;;;N;LATIN SMALL LETTER T PALATAL HOOK;;;; <=
          01AC;LATIN CAPITAL LETTER T WITH HOOK;Lu;0;L;;;;;N;LATIN CAPITAL LETTER T HOOK;;;01AD; => +1
          01AD;LATIN SMALL LETTER T WITH HOOK;Ll;0;L;;;;;N;LATIN SMALL LETTER T HOOK;;01AC;;01AC <=
          01AE;LATIN CAPITAL LETTER T WITH RETROFLEX HOOK;Lu;0;L;;;;;N;LATIN CAPITAL LETTER T RETROFLEX HOOK;;;0288; => CA 88
          01AF;LATIN CAPITAL LETTER U WITH HORN;Lu;0;L;0055 031B;;;;N;LATIN CAPITAL LETTER U HORN;;;01B0; => +1
          }
          #$A0..#$A5,#$AC:
          begin
            if ord(c2) mod 2 = 0 then
              new_c2 := chr(ord(c2) + 1);
          end;
          #$A7,#$AF:
          begin
            if ord(c2) mod 2 = 1 then
              new_c2 := chr(ord(c2) + 1);
          end;
          #$A6:
          begin
            new_c1 := #$CA;
            new_c2 := #$80;
          end;
          #$A9:
          begin
            new_c1 := #$CA;
            new_c2 := #$83;
          end;
          #$AE:
          begin
            new_c1 := #$CA;
            new_c2 := #$88;
          end;
          {
          And also C6 B0..C6 BF

          01B0;LATIN SMALL LETTER U WITH HORN;Ll;0;L;0075 031B;;;;N;LATIN SMALL LETTER U HORN;;01AF;;01AF <= -1
          01B1;LATIN CAPITAL LETTER UPSILON;Lu;0;L;;;;;N;;;;028A; => CA 8A
          01B2;LATIN CAPITAL LETTER V WITH HOOK;Lu;0;L;;;;;N;LATIN CAPITAL LETTER SCRIPT V;;;028B; => CA 8B
          01B3;LATIN CAPITAL LETTER Y WITH HOOK;Lu;0;L;;;;;N;LATIN CAPITAL LETTER Y HOOK;;;01B4; => +1
          01B4;LATIN SMALL LETTER Y WITH HOOK;Ll;0;L;;;;;N;LATIN SMALL LETTER Y HOOK;;01B3;;01B3 <=
          01B5;LATIN CAPITAL LETTER Z WITH STROKE;Lu;0;L;;;;;N;LATIN CAPITAL LETTER Z BAR;;;01B6; => +1
          01B6;LATIN SMALL LETTER Z WITH STROKE;Ll;0;L;;;;;N;LATIN SMALL LETTER Z BAR;;01B5;;01B5 <=
          01B7;LATIN CAPITAL LETTER EZH;Lu;0;L;;;;;N;LATIN CAPITAL LETTER YOGH;;;0292; => CA 92
          01B8;LATIN CAPITAL LETTER EZH REVERSED;Lu;0;L;;;;;N;LATIN CAPITAL LETTER REVERSED YOGH;;;01B9; => +1
          01B9;LATIN SMALL LETTER EZH REVERSED;Ll;0;L;;;;;N;LATIN SMALL LETTER REVERSED YOGH;;01B8;;01B8 <=
          01BA;LATIN SMALL LETTER EZH WITH TAIL;Ll;0;L;;;;;N;LATIN SMALL LETTER YOGH WITH TAIL;;;; <=
          01BB;LATIN LETTER TWO WITH STROKE;Lo;0;L;;;;;N;LATIN LETTER TWO BAR;;;; X
          01BC;LATIN CAPITAL LETTER TONE FIVE;Lu;0;L;;;;;N;;;;01BD; => +1
          01BD;LATIN SMALL LETTER TONE FIVE;Ll;0;L;;;;;N;;;01BC;;01BC <=
          01BE;LATIN LETTER INVERTED GLOTTAL STOP WITH STROKE;Ll;0;L;;;;;N;LATIN LETTER INVERTED GLOTTAL STOP BAR;;;; X
          01BF;LATIN LETTER WYNN;Ll;0;L;;;;;N;;;01F7;;01F7  <=
          }
          #$B8,#$BC:
          begin
            if ord(c2) mod 2 = 0 then
              new_c2 := chr(ord(c2) + 1);
          end;
          #$B3..#$B6:
          begin
            if ord(c2) mod 2 = 1 then
              new_c2 := chr(ord(c2) + 1);
          end;
          #$B1:
          begin
            new_c1 := #$CA;
            new_c2 := #$8A;
          end;
          #$B2:
          begin
            new_c1 := #$CA;
            new_c2 := #$8B;
          end;
          #$B7:
          begin
            new_c1 := #$CA;
            new_c2 := #$92;
          end;
          end;
        end;
        #$C7:
        begin
          case c2 of
          #$84..#$8C,#$B1..#$B3:
          begin
            if (ord(c2) and $F) mod 3 = 1 then new_c2 := chr(ord(c2) + 2)
            else if (ord(c2) and $F) mod 3 = 2 then new_c2 := chr(ord(c2) + 1);
          end;
          #$8D..#$9C:
          begin
            if ord(c2) mod 2 = 1 then
              new_c2 := chr(ord(c2) + 1);
          end;
          #$9E..#$AF,#$B4..#$B5,#$B8..#$BF:
          begin
            if ord(c2) mod 2 = 0 then
              new_c2 := chr(ord(c2) + 1);
          end;
          {
          01F6;LATIN CAPITAL LETTER HWAIR;Lu;0;L;;;;;N;;;;0195;
          01F7;LATIN CAPITAL LETTER WYNN;Lu;0;L;;;;;N;;;;01BF;
          }
          #$B6:
          begin
            new_c1 := #$C6;
            new_c2 := #$95;
          end;
          #$B7:
          begin
            new_c1 := #$C6;
            new_c2 := #$BF;
          end;
          end;
        end;
        {
        Codepoints 0200 to 023F
        }
        #$C8:
        begin
          // For this one we can simply start with a default and override for some specifics
          if (c2 in [#$80..#$B3]) and (ord(c2) mod 2 = 0) then new_c2 := chr(ord(c2) + 1);

          case c2 of
          #$A0:
          begin
            new_c1 := #$C6;
            new_c2 := #$9E;
          end;
          #$A1: new_c2 := c2;
          {
          023A;LATIN CAPITAL LETTER A WITH STROKE;Lu;0;L;;;;;N;;;;2C65; => E2 B1 A5
          023B;LATIN CAPITAL LETTER C WITH STROKE;Lu;0;L;;;;;N;;;;023C; => +1
          023C;LATIN SMALL LETTER C WITH STROKE;Ll;0;L;;;;;N;;;023B;;023B <=
          023D;LATIN CAPITAL LETTER L WITH BAR;Lu;0;L;;;;;N;;;;019A; => C6 9A
          023E;LATIN CAPITAL LETTER T WITH DIAGONAL STROKE;Lu;0;L;;;;;N;;;;2C66; => E2 B1 A6
          023F;LATIN SMALL LETTER S WITH SWASH TAIL;Ll;0;L;;;;;N;;;2C7E;;2C7E <=
          0240;LATIN SMALL LETTER Z WITH SWASH TAIL;Ll;0;L;;;;;N;;;2C7F;;2C7F <=
          }
          #$BA,#$BE:
          begin
            OutStr := PChar(OutStr - PChar(Result));
            SetLength(Result,Length(Result)+1);// Increase the buffer
            OutStr := PtrInt(OutStr) + PChar(Result);
            OutStr^ := #$E2;
            inc(OutStr);
            OutStr^ := #$B1;
            inc(OutStr);
            if c2 = #$BA then OutStr^ := #$A5
            else OutStr^ := #$A6;
            dec(CounterDiff);
            inc(OutStr);
            inc(InStr, 2);
            Continue;
          end;
          #$BD:
          begin
            new_c1 := #$C6;
            new_c2 := #$9A;
          end;
          #$BB: new_c2 := chr(ord(c2) + 1);
          end;
        end;
        {
        Codepoints 0240 to 027F

        Here only 0240..024F needs lowercase
        }
        #$C9:
        begin
          case c2 of
          #$81..#$82:
          begin
            if ord(c2) mod 2 = 1 then
              new_c2 := chr(ord(c2) + 1);
          end;
          #$86..#$8F:
          begin
            if ord(c2) mod 2 = 0 then
              new_c2 := chr(ord(c2) + 1);
          end;
          #$83:
          begin
            new_c1 := #$C6;
            new_c2 := #$80;
          end;
          #$84:
          begin
            new_c1 := #$CA;
            new_c2 := #$89;
          end;
          #$85:
          begin
            new_c1 := #$CA;
            new_c2 := #$8C;
          end;
          end;
        end;
        // $CE91..$CE9F: NewChar := OldChar + $20; // Greek Characters
        // $CEA0..$CEA9: NewChar := OldChar + $E0; // Greek Characters
        #$CE:
        begin
          case c2 of
            #$91..#$9F:
            begin
              new_c2 := chr(ord(c2) + $20);
            end;
            #$A0..#$A9:
            begin
              new_c2 := chr(ord(c2) - $10);
            end;
          end;
        end;
        // $D080..$D08F: NewChar := OldChar + $110; // Cyrillic alphabet
        // $D090..$D09F: NewChar := OldChar + $20; // Cyrillic alphabet
        // $D0A0..$D0AF: NewChar := OldChar + $E0; // Cyrillic alphabet
        #$D0:
        begin
          c2 := InStr[1];
          case c2 of
            #$80..#$8F:
            begin
              new_c1 := chr(ord(c1)+1);
              new_c2  := chr(ord(c2) + $10);
            end;
            #$90..#$9F:
            begin
              new_c2 := chr(ord(c2) + $20);
            end;
            #$A0..#$AF:
            begin
              new_c1 := chr(ord(c1)+1);
              new_c2 := chr(ord(c2) - $20);
            end;
          end;
        end;
        // Archaic and non-slavic cyrillic 460-47F = D1A0-D1BF
        // These require just adding 1 to get the lowercase
        #$D1:
        begin
          if (c2 in [#$A0..#$BF]) and (ord(c2) mod 2 = 0) then
            new_c2 := chr(ord(c2) + 1);
        end;
        // Archaic and non-slavic cyrillic 480-4BF = D280-D2BF
        // These mostly require just adding 1 to get the lowercase
        #$D2:
        begin
          case c2 of
            #$80:
            begin
              new_c2 := chr(ord(c2) + 1);
            end;
            // #$81 is already lowercase
            // #$82-#$89 ???
            #$8A..#$BF:
            begin
              if ord(c2) mod 2 = 0 then
                new_c2 := chr(ord(c2) + 1);
            end;
          end;
        end;
        {
        Codepoints  04C0..04FF
        }
        #$D3:
        begin
          case c2 of
            #$80: new_c2 := #$8F;
            #$81..#$8E:
            begin
              if ord(c2) mod 2 = 1 then
                new_c2 := chr(ord(c2) + 1);
            end;
            #$90..#$BF:
            begin
              if ord(c2) mod 2 = 0 then
                new_c2 := chr(ord(c2) + 1);
            end;
          end;
        end;
        {
        Codepoints  0500..053F

        Armenian starts in 0531
        }
        #$D4:
        begin
          if ord(c2) mod 2 = 0 then
            new_c2 := chr(ord(c2) + 1);

          // Armenian
          if c2 in [#$B1..#$BF] then
          begin
            new_c1 := #$D5;
            new_c2 := chr(ord(c2) - $10);
          end;
        end;
        {
        Codepoints  0540..057F

        Armenian
        }
        #$D5:
        begin
          case c2 of
            #$80..#$8F:
            begin
              new_c2 := chr(ord(c2) + $30);
            end;
            #$90..#$96:
            begin
              new_c1 := #$D6;
              new_c2 := chr(ord(c2) - $10);
            end;
          end;
        end;
        end;
        // Common code 2-byte modifiable chars
        if (CounterDiff <> 0) then
        begin
          OutStr^ := new_c1;
          OutStr[1] := new_c2;
        end
        else
        begin
          if (new_c1 <> c1) then OutStr^ := new_c1;
          if (new_c2 <> c2) then OutStr[1] := new_c2;
        end;
        inc(InStr, 2);
        inc(OutStr, 2);
      end;
      {
      Characters with 3 bytes
      }
      #$E1:
      begin
        new_c1 := c1;
        c2 := InStr[1];
        c3 := InStr[2];
        new_c2 := c2;
        new_c3 := c3;
        {
        Georgian codepoints 10A0-10C5 => 2D00-2D25

        In UTF-8 this is:
        E1 82 A0 - E1 82 BF => E2 B4 80 - E2 B4 9F
        E1 83 80 - E1 83 85 => E2 B4 A0 - E2 B4 A5
        }
        case c2 of
        #$82:
        if (c3 in [#$A0..#$BF]) then
        begin
          new_c1 := #$E2;
          new_c2 := #$B4;
          new_c3 := chr(ord(c3) - $20);
        end;
        #$83:
        if (c3 in [#$80..#$85]) then
        begin
          new_c1 := #$E2;
          new_c2 := #$B4;
          new_c3 := chr(ord(c3) + $20);
        end;
        {
        Extra chars between 1E00..1EFF

        Blocks of chars:
          1E00..1E3F    E1 B8 80..E1 B8 BF
          1E40..1E7F    E1 B9 80..E1 B9 BF
          1E80..1EBF    E1 BA 80..E1 BA BF
          1EC0..1EFF    E1 BB 80..E1 BB BF
        }
        #$B8..#$BB:
        begin
          // Start with a default and change for some particular chars
          if ord(c3) mod 2 = 0 then
            new_c3 := chr(ord(c3) + 1);

          { Only 1E96..1E9F are different E1 BA 96..E1 BA 9F

          1E96;LATIN SMALL LETTER H WITH LINE BELOW;Ll;0;L;0068 0331;;;;N;;;;;
          1E97;LATIN SMALL LETTER T WITH DIAERESIS;Ll;0;L;0074 0308;;;;N;;;;;
          1E98;LATIN SMALL LETTER W WITH RING ABOVE;Ll;0;L;0077 030A;;;;N;;;;;
          1E99;LATIN SMALL LETTER Y WITH RING ABOVE;Ll;0;L;0079 030A;;;;N;;;;;
          1E9A;LATIN SMALL LETTER A WITH RIGHT HALF RING;Ll;0;L;<compat> 0061 02BE;;;;N;;;;;
          1E9B;LATIN SMALL LETTER LONG S WITH DOT ABOVE;Ll;0;L;017F 0307;;;;N;;;1E60;;1E60
          1E9C;LATIN SMALL LETTER LONG S WITH DIAGONAL STROKE;Ll;0;L;;;;;N;;;;;
          1E9D;LATIN SMALL LETTER LONG S WITH HIGH STROKE;Ll;0;L;;;;;N;;;;;
          1E9E;LATIN CAPITAL LETTER SHARP S;Lu;0;L;;;;;N;;;;00DF; => C3 9F
          1E9F;LATIN SMALL LETTER DELTA;Ll;0;L;;;;;N;;;;;
          }
          if (c2 = #$BA) and (c3 in [#$96..#$9F]) then new_c3 := c3;
          // LATIN CAPITAL LETTER SHARP S => to german Beta
          if (c2 = #$BA) and (c3 = #$9E) then
          begin
            inc(InStr, 3);
            OutStr^ := #$C3;
            inc(OutStr);
            OutStr^ := #$9F;
            inc(OutStr);
            inc(CounterDiff, 1);
            Continue;
          end;
        end;
        {
        Extra chars between 1F00..1FFF

        Blocks of chars:
          1E00..1E3F    E1 BC 80..E1 BC BF
          1E40..1E7F    E1 BD 80..E1 BD BF
          1E80..1EBF    E1 BE 80..E1 BE BF
          1EC0..1EFF    E1 BF 80..E1 BF BF
        }
        #$BC:
        begin
          // Start with a default and change for some particular chars
          if (ord(c3) mod $10) div 8 = 1 then
            new_c3 := chr(ord(c3) - 8);
        end;
        #$BD:
        begin
          // Start with a default and change for some particular chars
          case c3 of
          #$80..#$8F, #$A0..#$AF: if (ord(c3) mod $10) div 8 = 1 then
                        new_c3 := chr(ord(c3) - 8);
          {
          1F50;GREEK SMALL LETTER UPSILON WITH PSILI;Ll;0;L;03C5 0313;;;;N;;;;;
          1F51;GREEK SMALL LETTER UPSILON WITH DASIA;Ll;0;L;03C5 0314;;;;N;;;1F59;;1F59
          1F52;GREEK SMALL LETTER UPSILON WITH PSILI AND VARIA;Ll;0;L;1F50 0300;;;;N;;;;;
          1F53;GREEK SMALL LETTER UPSILON WITH DASIA AND VARIA;Ll;0;L;1F51 0300;;;;N;;;1F5B;;1F5B
          1F54;GREEK SMALL LETTER UPSILON WITH PSILI AND OXIA;Ll;0;L;1F50 0301;;;;N;;;;;
          1F55;GREEK SMALL LETTER UPSILON WITH DASIA AND OXIA;Ll;0;L;1F51 0301;;;;N;;;1F5D;;1F5D
          1F56;GREEK SMALL LETTER UPSILON WITH PSILI AND PERISPOMENI;Ll;0;L;1F50 0342;;;;N;;;;;
          1F57;GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI;Ll;0;L;1F51 0342;;;;N;;;1F5F;;1F5F
          1F59;GREEK CAPITAL LETTER UPSILON WITH DASIA;Lu;0;L;03A5 0314;;;;N;;;;1F51;
          1F5B;GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA;Lu;0;L;1F59 0300;;;;N;;;;1F53;
          1F5D;GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA;Lu;0;L;1F59 0301;;;;N;;;;1F55;
          1F5F;GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI;Lu;0;L;1F59 0342;;;;N;;;;1F57;
          }
          #$99,#$9B,#$9D,#$9F: new_c3 := chr(ord(c3) - 8);
          end;
        end;
        #$BE:
        begin
          // Start with a default and change for some particular chars
          case c3 of
          #$80..#$B9: if (ord(c3) mod $10) div 8 = 1 then
                        new_c3 := chr(ord(c3) - 8);
          {
          1FB0;GREEK SMALL LETTER ALPHA WITH VRACHY;Ll;0;L;03B1 0306;;;;N;;;1FB8;;1FB8
          1FB1;GREEK SMALL LETTER ALPHA WITH MACRON;Ll;0;L;03B1 0304;;;;N;;;1FB9;;1FB9
          1FB2;GREEK SMALL LETTER ALPHA WITH VARIA AND YPOGEGRAMMENI;Ll;0;L;1F70 0345;;;;N;;;;;
          1FB3;GREEK SMALL LETTER ALPHA WITH YPOGEGRAMMENI;Ll;0;L;03B1 0345;;;;N;;;1FBC;;1FBC
          1FB4;GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI;Ll;0;L;03AC 0345;;;;N;;;;;
          1FB6;GREEK SMALL LETTER ALPHA WITH PERISPOMENI;Ll;0;L;03B1 0342;;;;N;;;;;
          1FB7;GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI;Ll;0;L;1FB6 0345;;;;N;;;;;
          1FB8;GREEK CAPITAL LETTER ALPHA WITH VRACHY;Lu;0;L;0391 0306;;;;N;;;;1FB0;
          1FB9;GREEK CAPITAL LETTER ALPHA WITH MACRON;Lu;0;L;0391 0304;;;;N;;;;1FB1;
          1FBA;GREEK CAPITAL LETTER ALPHA WITH VARIA;Lu;0;L;0391 0300;;;;N;;;;1F70;
          1FBB;GREEK CAPITAL LETTER ALPHA WITH OXIA;Lu;0;L;0386;;;;N;;;;1F71;
          1FBC;GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI;Lt;0;L;0391 0345;;;;N;;;;1FB3;
          1FBD;GREEK KORONIS;Sk;0;ON;<compat> 0020 0313;;;;N;;;;;
          1FBE;GREEK PROSGEGRAMMENI;Ll;0;L;03B9;;;;N;;;0399;;0399
          1FBF;GREEK PSILI;Sk;0;ON;<compat> 0020 0313;;;;N;;;;;
          }
          #$BA:
          begin
            new_c2 := #$BD;
            new_c3 := #$B0;
          end;
          #$BB:
          begin
            new_c2 := #$BD;
            new_c3 := #$B1;
          end;
          #$BC: new_c3 := #$B3;
          end;
        end;
        end;

        if (CounterDiff <> 0) then
        begin
          OutStr^ := new_c1;
          OutStr[1] := new_c2;
          OutStr[2] := new_c3;
        end
        else
        begin
          if c1 <> new_c1 then OutStr^ := new_c1;
          if c2 <> new_c2 then OutStr[1] := new_c2;
          if c3 <> new_c3 then OutStr[2] := new_c3;
        end;

        inc(InStr, 3);
        inc(OutStr, 3);
      end;
      {
      More Characters with 3 bytes, so exotic stuff between:
      $2126..$2183                    E2 84 A6..E2 86 83
      $24B6..$24CF    Result:=u+26;   E2 92 B6..E2 93 8F
      $2C00..$2C2E    Result:=u+48;   E2 B0 80..E2 B0 AE
      $2C60..$2CE2                    E2 B1 A0..E2 B3 A2
      }
      #$E2:
      begin
        new_c1 := c1;
        c2 := InStr[1];
        c3 := InStr[2];
        new_c2 := c2;
        new_c3 := c3;
        // 2126;OHM SIGN;Lu;0;L;03A9;;;;N;OHM;;;03C9; E2 84 A6 => CF 89
        if (c2 = #$84) and (c3 = #$A6) then
        begin
          inc(InStr, 3);
          OutStr^ := #$CF;
          inc(OutStr);
          OutStr^ := #$89;
          inc(OutStr);
          inc(CounterDiff, 1);
          Continue;
        end
        {
        212A;KELVIN SIGN;Lu;0;L;004B;;;;N;DEGREES KELVIN;;;006B; E2 84 AA => 6B
        }
        else if (c2 = #$84) and (c3 = #$AA) then
        begin
          inc(InStr, 3);
          if c3 = #$AA then OutStr^ := #$6B
          else OutStr^ := #$E5;
          inc(OutStr);
          inc(CounterDiff, 2);
          Continue;
        end
        {
        212B;ANGSTROM SIGN;Lu;0;L;00C5;;;;N;ANGSTROM UNIT;;;00E5; E2 84 AB => C3 A5
        }
        else if (c2 = #$84) and (c3 = #$AB) then
        begin
          inc(InStr, 3);
          OutStr^ := #$C3;
          inc(OutStr);
          OutStr^ := #$A5;
          inc(OutStr);
          inc(CounterDiff, 1);
          Continue;
        end
        {
        2160;ROMAN NUMERAL ONE;Nl;0;L;<compat> 0049;;;1;N;;;;2170; E2 85 A0 => E2 85 B0
        2161;ROMAN NUMERAL TWO;Nl;0;L;<compat> 0049 0049;;;2;N;;;;2171;
        2162;ROMAN NUMERAL THREE;Nl;0;L;<compat> 0049 0049 0049;;;3;N;;;;2172;
        2163;ROMAN NUMERAL FOUR;Nl;0;L;<compat> 0049 0056;;;4;N;;;;2173;
        2164;ROMAN NUMERAL FIVE;Nl;0;L;<compat> 0056;;;5;N;;;;2174;
        2165;ROMAN NUMERAL SIX;Nl;0;L;<compat> 0056 0049;;;6;N;;;;2175;
        2166;ROMAN NUMERAL SEVEN;Nl;0;L;<compat> 0056 0049 0049;;;7;N;;;;2176;
        2167;ROMAN NUMERAL EIGHT;Nl;0;L;<compat> 0056 0049 0049 0049;;;8;N;;;;2177;
        2168;ROMAN NUMERAL NINE;Nl;0;L;<compat> 0049 0058;;;9;N;;;;2178;
        2169;ROMAN NUMERAL TEN;Nl;0;L;<compat> 0058;;;10;N;;;;2179;
        216A;ROMAN NUMERAL ELEVEN;Nl;0;L;<compat> 0058 0049;;;11;N;;;;217A;
        216B;ROMAN NUMERAL TWELVE;Nl;0;L;<compat> 0058 0049 0049;;;12;N;;;;217B;
        216C;ROMAN NUMERAL FIFTY;Nl;0;L;<compat> 004C;;;50;N;;;;217C;
        216D;ROMAN NUMERAL ONE HUNDRED;Nl;0;L;<compat> 0043;;;100;N;;;;217D;
        216E;ROMAN NUMERAL FIVE HUNDRED;Nl;0;L;<compat> 0044;;;500;N;;;;217E;
        216F;ROMAN NUMERAL ONE THOUSAND;Nl;0;L;<compat> 004D;;;1000;N;;;;217F;
        }
        else if (c2 = #$85) and (c3 in [#$A0..#$AF]) then new_c3 := chr(ord(c3) + $10)
        {
        2183;ROMAN NUMERAL REVERSED ONE HUNDRED;Lu;0;L;;;;;N;;;;2184; E2 86 83 => E2 86 84
        }
        else if (c2 = #$86) and (c3 = #$83) then new_c3 := chr(ord(c3) + 1)
        {
        $24B6..$24CF    Result:=u+26;   E2 92 B6..E2 93 8F

        Ex: 24B6;CIRCLED LATIN CAPITAL LETTER A;So;0;L;<circle> 0041;;;;N;;;;24D0; E2 92 B6 => E2 93 90
        }
        else if (c2 = #$92) and (c3 in [#$B6..#$BF]) then
        begin
          new_c3 := #$93;
          new_c3 := chr(ord(c3) - $26);
        end
        else if (c2 = #$93) and (c3 in [#$80..#$8F]) then new_c3 := chr(ord(c3) + 26)
        {
        $2C00..$2C2E    Result:=u+48;   E2 B0 80..E2 B0 AE

        2C00;GLAGOLITIC CAPITAL LETTER AZU;Lu;0;L;;;;;N;;;;2C30; E2 B0 80 => E2 B0 B0

        2C10;GLAGOLITIC CAPITAL LETTER NASHI;Lu;0;L;;;;;N;;;;2C40; E2 B0 90 => E2 B1 80
        }
        else if (c2 = #$B0) and (c3 in [#$80..#$8F]) then new_c3 := chr(ord(c3) + $30)
        else if (c2 = #$B0) and (c3 in [#$90..#$AE]) then
        begin
          new_c2 := #$B1;
          new_c3 := chr(ord(c3) - $10);
        end
        {
        $2C60..$2CE2                    E2 B1 A0..E2 B3 A2

        2C60;LATIN CAPITAL LETTER L WITH DOUBLE BAR;Lu;0;L;;;;;N;;;;2C61; E2 B1 A0 => +1
        2C61;LATIN SMALL LETTER L WITH DOUBLE BAR;Ll;0;L;;;;;N;;;2C60;;2C60
        2C62;LATIN CAPITAL LETTER L WITH MIDDLE TILDE;Lu;0;L;;;;;N;;;;026B; => 	C9 AB
        2C63;LATIN CAPITAL LETTER P WITH STROKE;Lu;0;L;;;;;N;;;;1D7D; => E1 B5 BD
        2C64;LATIN CAPITAL LETTER R WITH TAIL;Lu;0;L;;;;;N;;;;027D; => 	C9 BD
        2C65;LATIN SMALL LETTER A WITH STROKE;Ll;0;L;;;;;N;;;023A;;023A
        2C66;LATIN SMALL LETTER T WITH DIAGONAL STROKE;Ll;0;L;;;;;N;;;023E;;023E
        2C67;LATIN CAPITAL LETTER H WITH DESCENDER;Lu;0;L;;;;;N;;;;2C68; => E2 B1 A8
        2C68;LATIN SMALL LETTER H WITH DESCENDER;Ll;0;L;;;;;N;;;2C67;;2C67
        2C69;LATIN CAPITAL LETTER K WITH DESCENDER;Lu;0;L;;;;;N;;;;2C6A; => E2 B1 AA
        2C6A;LATIN SMALL LETTER K WITH DESCENDER;Ll;0;L;;;;;N;;;2C69;;2C69
        2C6B;LATIN CAPITAL LETTER Z WITH DESCENDER;Lu;0;L;;;;;N;;;;2C6C; => E2 B1 AC
        2C6C;LATIN SMALL LETTER Z WITH DESCENDER;Ll;0;L;;;;;N;;;2C6B;;2C6B
        2C6D;LATIN CAPITAL LETTER ALPHA;Lu;0;L;;;;;N;;;;0251; => C9 91
        2C6E;LATIN CAPITAL LETTER M WITH HOOK;Lu;0;L;;;;;N;;;;0271; => C9 B1
        2C6F;LATIN CAPITAL LETTER TURNED A;Lu;0;L;;;;;N;;;;0250; => C9 90

        2C70;LATIN CAPITAL LETTER TURNED ALPHA;Lu;0;L;;;;;N;;;;0252; => C9 92
        }
        else if (c2 = #$B1) then
        begin
          case c3 of
          #$A0: new_c3 := chr(ord(c3)+1);
          #$A2,#$A4,#$AD..#$AF,#$B0:
          begin
            inc(InStr, 3);
            OutStr^ := #$C9;
            inc(OutStr);
            case c3 of
            #$A2: OutStr^ := #$AB;
            #$A4: OutStr^ := #$BD;
            #$AD: OutStr^ := #$90;
            #$AE: OutStr^ := #$B1;
            #$AF: OutStr^ := #$90;
            #$B0: OutStr^ := #$92;
            end;
            inc(OutStr);
            inc(CounterDiff, 1);
            Continue;
          end;
          #$A3:
          begin
            new_c2 := #$B5;
            new_c3 := #$BD;
          end;
          #$A7,#$A9,#$AB: new_c3 := chr(ord(c3)+1);
          {
          2C71;LATIN SMALL LETTER V WITH RIGHT HOOK;Ll;0;L;;;;;N;;;;;
          2C72;LATIN CAPITAL LETTER W WITH HOOK;Lu;0;L;;;;;N;;;;2C73;
          2C73;LATIN SMALL LETTER W WITH HOOK;Ll;0;L;;;;;N;;;2C72;;2C72
          2C74;LATIN SMALL LETTER V WITH CURL;Ll;0;L;;;;;N;;;;;
          2C75;LATIN CAPITAL LETTER HALF H;Lu;0;L;;;;;N;;;;2C76;
          2C76;LATIN SMALL LETTER HALF H;Ll;0;L;;;;;N;;;2C75;;2C75
          2C77;LATIN SMALL LETTER TAILLESS PHI;Ll;0;L;;;;;N;;;;;
          2C78;LATIN SMALL LETTER E WITH NOTCH;Ll;0;L;;;;;N;;;;;
          2C79;LATIN SMALL LETTER TURNED R WITH TAIL;Ll;0;L;;;;;N;;;;;
          2C7A;LATIN SMALL LETTER O WITH LOW RING INSIDE;Ll;0;L;;;;;N;;;;;
          2C7B;LATIN LETTER SMALL CAPITAL TURNED E;Ll;0;L;;;;;N;;;;;
          2C7C;LATIN SUBSCRIPT SMALL LETTER J;Ll;0;L;<sub> 006A;;;;N;;;;;
          2C7D;MODIFIER LETTER CAPITAL V;Lm;0;L;<super> 0056;;;;N;;;;;
          2C7E;LATIN CAPITAL LETTER S WITH SWASH TAIL;Lu;0;L;;;;;N;;;;023F; => C8 BF
          2C7F;LATIN CAPITAL LETTER Z WITH SWASH TAIL;Lu;0;L;;;;;N;;;;0240; => C9 80
          }
          #$B2,#$B5: new_c3 := chr(ord(c3)+1);
          #$BE,#$BF:
          begin
            inc(InStr, 3);
            case c3 of
            #$BE: OutStr^ := #$C8;
            #$BF: OutStr^ := #$C9;
            end;
            OutStr^ := #$C8;
            inc(OutStr);
            case c3 of
            #$BE: OutStr^ := #$BF;
            #$BF: OutStr^ := #$80;
            end;
            inc(OutStr);
            inc(CounterDiff, 1);
            Continue;
          end;
          end;
        end
        {
        2C80;COPTIC CAPITAL LETTER ALFA;Lu;0;L;;;;;N;;;;2C81; E2 B2 80 => E2 B2 81
        ...
        2CBE;COPTIC CAPITAL LETTER OLD COPTIC OOU;Lu;0;L;;;;;N;;;;2CBF; E2 B2 BE => E2 B2 BF
        2CBF;COPTIC SMALL LETTER OLD COPTIC OOU;Ll;0;L;;;;;N;;;2CBE;;2CBE
        ...
        2CC0;COPTIC CAPITAL LETTER SAMPI;Lu;0;L;;;;;N;;;;2CC1; E2 B3 80 => E2 B2 81
        2CC1;COPTIC SMALL LETTER SAMPI;Ll;0;L;;;;;N;;;2CC0;;2CC0
        ...
        2CE2;COPTIC CAPITAL LETTER OLD NUBIAN WAU;Lu;0;L;;;;;N;;;;2CE3; E2 B3 A2 => E2 B3 A3
        2CE3;COPTIC SMALL LETTER OLD NUBIAN WAU;Ll;0;L;;;;;N;;;2CE2;;2CE2 <=
        }
        else if (c2 = #$B2) then
        begin
          if ord(c3) mod 2 = 0 then new_c3 := chr(ord(c3) + 1);
        end
        else if (c2 = #$B3) and (c3 in [#$80..#$A3]) then
        begin
          if ord(c3) mod 2 = 0 then new_c3 := chr(ord(c3) + 1);
        end;

        if (CounterDiff <> 0) then
        begin
          OutStr^ := new_c1;
          OutStr[1] := new_c2;
          OutStr[2] := new_c3;
        end
        else
        begin
          if c1 <> new_c1 then OutStr^ := new_c1;
          if c2 <> new_c2 then OutStr[1] := new_c2;
          if c3 <> new_c3 then OutStr[2] := new_c3;
        end;

        inc(InStr, 3);
        inc(OutStr, 3);
      end;
      {
      FF21;FULLWIDTH LATIN CAPITAL LETTER A;Lu;0;L;<wide> 0041;;;;N;;;;FF41; EF BC A1 => EF BD 81
      ...
      FF3A;FULLWIDTH LATIN CAPITAL LETTER Z;Lu;0;L;<wide> 005A;;;;N;;;;FF5A; EF BC BA => EF BD 9A
      }
      #$EF:
      begin
        c2 := InStr[1];
        c3 := InStr[2];

        if (c2 = #$BC) and (c3 in [#$A1..#$BA]) then
        begin
          OutStr^ := c1;
          OutStr[1] := #$BD;
          OutStr[2] := chr(ord(c3) - $20);
        end;

        if (CounterDiff <> 0) then
        begin
          OutStr^ := c1;
          OutStr[1] := c2;
          OutStr[2] := c3;
        end;

        inc(InStr, 3);
        inc(OutStr, 3);
      end;
    else
      // Copy the character if the string was disaligned by previous changes
      if (CounterDiff <> 0) then OutStr^:= c1;
      inc(InStr);
      inc(OutStr);
    end; // Case InStr^
  end; // while

  // Final correction of the buffer size
  SetLength(Result,OutStr - PChar(Result));
end;

{
  AInStr - The input string
  ALanguage - The language. Use '' for maximum speed if one desires to ignore the language
              The language should be specified in the format from ISO 639-1,
              which uses 2 characters to represent each language.
              If the language has no code in ISO 639-1, then the 3-chars code
              from ISO 639-2 should be used.
              Example: "tr" - Turkish language locale

  Data from here: ftp://ftp.unicode.org/Public/UNIDATA/UnicodeData.txt

  The columns in the file UnicodeData.txt are explained here:
  http://www.ksu.ru/eng/departments/ktk/test/perl/lib/unicode/UCDFF301.html#Case Mappings
}
function UTF8UpperCase(const AInStr: utf8string; ALanguage: utf8string=''): utf8string;
var
  i, InCounter, OutCounter: PtrInt;
  OutStr: PChar;
  CharLen: integer;
  CharProcessed: Boolean;
  NewCharLen: integer;
  NewChar, OldChar: Word;
  // Language identification
  IsTurkish: Boolean;
begin
  // Start with the same string, and progressively modify
  Result:=AInStr;
  UniqueString(Result);
  OutStr := PChar(Result);

  // Language identification
  IsTurkish := (ALanguage = 'tr') or (ALanguage = 'az'); // Turkish and Azeri have a special handling

  InCounter:=1; // for AInStr
  OutCounter := 0; // for Result
  while InCounter<=length(AInStr) do
  begin
    { First ASCII chars }
    if (AInStr[InCounter] <= 'z') and (AInStr[InCounter] >= 'a') then
    begin
      // Special turkish handling
      // small dotted i to capital dotted i
      if IsTurkish and (AInStr[InCounter] = 'i') then
      begin
        SetLength(Result,Length(Result)+1);// Increase the buffer
        OutStr := PChar(Result);
        OutStr[OutCounter]:=#$C4;
        OutStr[OutCounter+1]:=#$B0;
        inc(InCounter);
        inc(OutCounter,2);
      end
      else
      begin
        OutStr[OutCounter]:=chr(ord(AInStr[InCounter])-32);
        inc(InCounter);
        inc(OutCounter);
      end;
    end
    { Now everything else }
    else
    begin
      CharLen := UTF8CharacterLength(@AInStr[InCounter]);
      CharProcessed := False;
      NewCharLen := CharLen;

      if CharLen = 2 then
      begin
        OldChar := (Ord(AInStr[InCounter]) shl 8) or Ord(AInStr[InCounter+1]);
        NewChar := 0;

        // Major processing
        case OldChar of
        // Latin Characters 0000–0FFF http://en.wikibooks.org/wiki/Unicode/Character_reference/0000-0FFF
        $C39F:        NewChar := $5353; // ß => SS
        $C3A0..$C3B6,$C3B8..$C3BE: NewChar := OldChar - $20;
        $C3BF:        NewChar := $C5B8; // ÿ
        $C481..$C4B0: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        // 0130 = C4 B0
        // turkish small undotted i to capital undotted i
        $C4B1:
        begin
          OutStr[OutCounter]:='I';
          NewCharLen := 1;
          CharProcessed := True;
        end;
        $C4B2..$C4B7: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        // $C4B8: ĸ without upper/lower
        $C4B9..$C4BF: if OldChar mod 2 = 0 then NewChar := OldChar - 1;
        $C580: NewChar := $C4BF; // border between bytes
        $C581..$C588: if OldChar mod 2 = 0 then NewChar := OldChar - 1;
        // $C589 ŉ => ?
        $C58A..$C5B7: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        // $C5B8: // Ÿ already uppercase
        $C5B9..$C5BE: if OldChar mod 2 = 0 then NewChar := OldChar - 1;
        $C5BF: // 017F
        begin
          OutStr[OutCounter]:='S';
          NewCharLen := 1;
          CharProcessed := True;
        end;
        // 0180 = C6 80 -> A convoluted part
        $C680: NewChar := $C983;
        $C682..$C685: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        $C688: NewChar := $C687;
        $C68C: NewChar := $C68B;
        // 0190 = C6 90 -> A convoluted part
        $C692: NewChar := $C691;
        $C695: NewChar := $C7B6;
        $C699: NewChar := $C698;
        $C69A: NewChar := $C8BD;
        $C69E: NewChar := $C8A0;
        // 01A0 = C6 A0 -> A convoluted part
        $C6A0..$C6A5: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        $C6A8: NewChar := $C6A7;
        $C6AD: NewChar := $C6AC;
        // 01B0 = C6 B0
        $C6B0: NewChar := $C6AF;
        $C6B3..$C6B6: if OldChar mod 2 = 0 then NewChar := OldChar - 1;
        $C6B9: NewChar := $C6B8;
        $C6BD: NewChar := $C6BC;
        $C6BF: NewChar := $C7B7;
        // 01C0 = C7 80
        $C784..$C786: NewChar := $C784;
        $C787..$C789: NewChar := $C787;
        $C78A..$C78C: NewChar := $C78A;
        $C78E: NewChar := $C78D;
        // 01D0 = C7 90
        $C790: NewChar := $C78F;
        $C791..$C79C: if OldChar mod 2 = 0 then NewChar := OldChar - 1;
        $C79D: NewChar := $C68E;
        $C79F: NewChar := $C79E;
        // 01E0 = C7 A0
        $C7A0..$C7AF: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        // 01F0 = C7 B0
        $C7B2..$C7B3: NewChar := $C7B1;
        $C7B5: NewChar := $C7B4;
        $C7B8..$C7BF: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        // 0200 = C8 80
        // 0210 = C8 90
        $C880..$C89F: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        // 0220 = C8 A0
        // 0230 = C8 B0
        $C8A2..$C8B3: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        $C8BC: NewChar := $C8BB;
        $C8BF:
        begin
          OutStr[OutCounter]  := #$E2;
          OutStr[OutCounter+1]:= #$B1;
          OutStr[OutCounter+2]:= #$BE;
          NewCharLen := 3;
          CharProcessed := True;
        end;
        // 0240 = C9 80
        $C980:
        begin
          OutStr[OutCounter]  := #$E2;
          OutStr[OutCounter+1]:= #$B1;
          OutStr[OutCounter+2]:= #$BF;
          NewCharLen := 3;
          CharProcessed := True;
        end;
        $C982: NewChar := $C981;
        $C986..$C98F: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        // 0250 = C9 90
        $C990:
        begin
          OutStr[OutCounter]  := #$E2;
          OutStr[OutCounter+1]:= #$B1;
          OutStr[OutCounter+2]:= #$AF;
          NewCharLen := 3;
          CharProcessed := True;
        end;
        $C991:
        begin
          OutStr[OutCounter]  := #$E2;
          OutStr[OutCounter+1]:= #$B1;
          OutStr[OutCounter+2]:= #$AD;
          NewCharLen := 3;
          CharProcessed := True;
        end;
        $C992:
        begin
          OutStr[OutCounter]  := #$E2;
          OutStr[OutCounter+1]:= #$B1;
          OutStr[OutCounter+2]:= #$B0;
          NewCharLen := 3;
          CharProcessed := True;
        end;
        $C993: NewChar := $C681;
        $C994: NewChar := $C686;
        $C996: NewChar := $C689;
        $C997: NewChar := $C68A;
        $C999: NewChar := $C68F;
        $C99B: NewChar := $C690;
        // 0260 = C9 A0
        $C9A0: NewChar := $C693;
        $C9A3: NewChar := $C694;
        $C9A5:
        begin
          OutStr[OutCounter]  := #$EA;
          OutStr[OutCounter+1]:= #$9E;
          OutStr[OutCounter+2]:= #$8D;
          NewCharLen := 3;
          CharProcessed := True;
        end;
        $C9A8: NewChar := $C697;
        $C9A9: NewChar := $C696;
        $C9AB:
        begin
          OutStr[OutCounter]  := #$E2;
          OutStr[OutCounter+1]:= #$B1;
          OutStr[OutCounter+2]:= #$A2;
          NewCharLen := 3;
          CharProcessed := True;
        end;
        $C9AF: NewChar := $C69C;
        // 0270 = C9 B0
        $C9B1:
        begin
          OutStr[OutCounter]  := #$E2;
          OutStr[OutCounter+1]:= #$B1;
          OutStr[OutCounter+2]:= #$AE;
          NewCharLen := 3;
          CharProcessed := True;
        end;
        $C9B2: NewChar := $C69D;
        $C9B5: NewChar := $C69F;
        $C9BD:
        begin
          OutStr[OutCounter]  := #$E2;
          OutStr[OutCounter+1]:= #$B1;
          OutStr[OutCounter+2]:= #$A4;
          NewCharLen := 3;
          CharProcessed := True;
        end;
        // 0280 = CA 80
        $CA80: NewChar := $C6A6;
        $CA83: NewChar := $C6A9;
        $CA88: NewChar := $C6AE;
        $CA89: NewChar := $C984;
        $CA8A: NewChar := $C6B1;
        $CA8B: NewChar := $C6B2;
        $CA8C: NewChar := $C985;
        // 0290 = CA 90
        $CA92: NewChar := $C6B7;
        //
        $CEB1..$CEBF: NewChar := OldChar - $20; // Greek Characters
        $CF80..$CF89: NewChar := OldChar - $E0; // Greek Characters
        $D0B0..$D0BF: NewChar := OldChar - $20; // Cyrillic alphabet
        $D180..$D18F: NewChar := OldChar - $E0; // Cyrillic alphabet
        $D190..$D19F: NewChar := OldChar - $110; // Cyrillic alphabet
        end;

        if NewChar <> 0 then
        begin
          OutStr[OutCounter]  := Chr(Hi(NewChar));
          OutStr[OutCounter+1]:= Chr(Lo(NewChar));
          CharProcessed := True;
        end;
      end;

      // Copy the character if the string was disaligned by previous changed
      // and no processing was done in this character
      if (InCounter <> OutCounter+1) and (not CharProcessed) then
      begin
        for i := 0 to CharLen-1 do
          OutStr[OutCounter+i]  :=AInStr[InCounter+i];
      end;

      inc(InCounter, CharLen);
      inc(OutCounter, NewCharLen);
    end;
  end; // while

  // Final correction of the buffer size
  SetLength(Result,OutCounter);
end;

{------------------------------------------------------------------------------
  Name:    UTF8CompareStr
  Params: S1, S2 - UTF8 encoded strings
  Returns: < 0 if S1 < S2, 0 if S1 = S2, > 0 if S2 > S1.
  Compare 2 UTF8 encoded strings, case sensitive.
  Note: Use this function instead of AnsiCompareStr.
  This function guarantees proper collation on all supported platforms.
 ------------------------------------------------------------------------------}
function UTF8CompareStr(const S1, S2: utf8string): Integer;
begin
  Result := SysUtils.CompareStr(S1, S2);
end;

{------------------------------------------------------------------------------
  Name:    UTF8CompareText
  Params: S1, S2 - UTF8 encoded strings
  Returns: < 0 if S1 < S2, 0 if S1 = S2, > 0 if S2 > S1.
  Compare 2 UTF8 encoded strings, case insensitive.
  Note: Use this function instead of AnsiCompareText.
  This function guarantees proper collation on all supported platforms.
 ------------------------------------------------------------------------------}
function UTF8CompareText(const S1, S2: utf8string): Integer;
var
  S1Lower, S2Lower: utf8string;
begin
  S1Lower := UTF8LowerCase(S1);
  S2Lower := UTF8LowerCase(S2);
  Result := UTF8CompareStr(S1Lower, S2Lower);
end;

{------------------------------------------------------------------------------
  Name:    ConvertUTF8ToUTF16
  Params:  Dest                - Pointer to destination string
           DestWideCharCount   - Wide char count allocated in destination string
           Src                 - Pointer to source string
           SrcCharCount        - Char count allocated in source string
           Options             - Conversion options, if none is set, both
             invalid and unfinished source chars are skipped

             toInvalidCharError       - Stop on invalid source char and report
                                      error
             toInvalidCharToSymbol    - Replace invalid source chars with '?'
             toUnfinishedCharError    - Stop on unfinished source char and
                                      report error
             toUnfinishedCharToSymbol - Replace unfinished source char with '?'

           ActualWideCharCount - Actual wide char count converted from source
                               string to destination string
  Returns:
    trNoError        - The string was successfully converted without
                     any error
    trNullSrc        - Pointer to source string is nil
    trNullDest       - Pointer to destination string is nil
    trDestExhausted  - Destination buffer size is not big enough to hold
                     converted string
    trInvalidChar    - Invalid source char has occured
    trUnfinishedChar - Unfinished source char has occured

  Converts the specified UTF-8 encoded string to UTF-16 encoded (system endian)
 ------------------------------------------------------------------------------}
function ConvertUTF8ToUTF16(Dest: PWideChar; DestWideCharCount: SizeUInt;
  Src: PChar; SrcCharCount: SizeUInt; Options: TConvertOptions;
  out ActualWideCharCount: SizeUInt): TConvertResult;
var
  DestI, SrcI: SizeUInt;
  B1, B2, B3, B4: Byte;
  W: Word;
  C: Cardinal;

  function UnfinishedCharError: Boolean;
  begin
    if toUnfinishedCharToSymbol in Options then
    begin
      Dest[DestI] := System.WideChar('?');
      Inc(DestI);
      Result := False;
    end
    else
      if toUnfinishedCharError in Options then
      begin
        ConvertUTF8ToUTF16 := trUnfinishedChar;
        Result := True;
      end
      else Result := False;
  end;

  function InvalidCharError(Count: SizeUInt): Boolean; inline;
  begin
    if not (toInvalidCharError in Options) then
    begin
      if toInvalidCharToSymbol in Options then
      begin
        Dest[DestI] := System.WideChar('?');
        Inc(DestI);
      end;

      Dec(SrcI, Count);

      // skip trailing UTF-8 char bytes
      while (Count > 0) do
      begin
        if (Byte(Src[SrcI]) and %11000000) <> %10000000 then Break;
        Inc(SrcI);
        Dec(Count);
      end;

      Result := False;
    end
    else
      if toInvalidCharError in Options then
      begin
        ConvertUTF8ToUTF16 := trUnfinishedChar;
        Result := True;
      end;
  end;

begin
  ActualWideCharCount := 0;

  if not Assigned(Src) then
  begin
    Result := trNullSrc;
    Exit;
  end;

  if not Assigned(Dest) then
  begin
    Result := trNullDest;
    Exit;
  end;
  SrcI := 0;
  DestI := 0;

  while (DestI < DestWideCharCount) and (SrcI < SrcCharCount) do
  begin
    B1 := Byte(Src[SrcI]);
    Inc(SrcI);

    if B1 < 128 then // single byte UTF-8 char
    begin
      Dest[DestI] := System.WideChar(B1);
      Inc(DestI);
    end
    else
    begin
      if SrcI >= SrcCharCount then
        if UnfinishedCharError then Exit(trInvalidChar)
        else Break;

      B2 := Byte(Src[SrcI]);
      Inc(SrcI);

      if (B1 and %11100000) = %11000000 then // double byte UTF-8 char
      begin
        if (B2 and %11000000) = %10000000 then
        begin
          Dest[DestI] := System.WideChar(((B1 and %00011111) shl 6) or (B2 and %00111111));
          Inc(DestI);
        end
        else // invalid character, assume single byte UTF-8 char
          if InvalidCharError(1) then Exit(trInvalidChar);
      end
      else
      begin
        if SrcI >= SrcCharCount then
          if UnfinishedCharError then Exit(trInvalidChar)
          else Break;

        B3 := Byte(Src[SrcI]);
        Inc(SrcI);

        if (B1 and %11110000) = %11100000 then // triple byte UTF-8 char
        begin
          if ((B2 and %11000000) = %10000000) and ((B3 and %11000000) = %10000000) then
          begin
            W := ((B1 and %00011111) shl 12) or ((B2 and %00111111) shl 6) or (B3 and %00111111);
            if (W < $D800) or (W > $DFFF) then // to single wide char UTF-16 char
            begin
              Dest[DestI] := System.WideChar(W);
              Inc(DestI);
            end
            else // invalid UTF-16 character, assume double byte UTF-8 char
              if InvalidCharError(2) then Exit(trInvalidChar);
          end
          else // invalid character, assume double byte UTF-8 char
            if InvalidCharError(2) then Exit(trInvalidChar);
        end
        else
        begin
          if SrcI >= SrcCharCount then
            if UnfinishedCharError then Exit(trInvalidChar)
            else Break;

          B4 := Byte(Src[SrcI]);
          Inc(SrcI);

          if ((B1 and %11111000) = %11110000) and ((B2 and %11000000) = %10000000)
            and ((B3 and %11000000) = %10000000) and ((B4 and %11000000) = %10000000) then
          begin // 4 byte UTF-8 char
            C := ((B1 and %00011111) shl 18) or ((B2 and %00111111) shl 12)
              or ((B3 and %00111111) shl 6)  or (B4 and %00111111);
            // to double wide char UTF-16 char
            Dest[DestI] := System.WideChar($D800 or ((C - $10000) shr 10));
            Inc(DestI);
            if DestI >= DestWideCharCount then Break;
            Dest[DestI] := System.WideChar($DC00 or ((C - $10000) and %0000001111111111));
            Inc(DestI);
          end
          else // invalid character, assume triple byte UTF-8 char
            if InvalidCharError(3) then Exit(trInvalidChar);
        end;
      end;
    end;
  end;

  if DestI >= DestWideCharCount then
  begin
    DestI := DestWideCharCount - 1;
    Result := trDestExhausted;
  end
  else
    Result := trNoError;

  Dest[DestI] := #0;
  ActualWideCharCount := DestI + 1;
end;

{------------------------------------------------------------------------------
  Name:    ConvertUTF16ToUTF8
  Params:  Dest             - Pointer to destination string
           DestCharCount    - Char count allocated in destination string
           Src              - Pointer to source string
           SrcWideCharCount - Wide char count allocated in source string
           Options          - Conversion options, if none is set, both
             invalid and unfinished source chars are skipped.
             See ConvertUTF8ToUTF16 for details.

           ActualCharCount  - Actual char count converted from source
                            string to destination string
  Returns: See ConvertUTF8ToUTF16

  Converts the specified UTF-16 encoded string (system endian) to UTF-8 encoded
 ------------------------------------------------------------------------------}
function ConvertUTF16ToUTF8(Dest: PChar; DestCharCount: SizeUInt;
  Src: PWideChar; SrcWideCharCount: SizeUInt; Options: TConvertOptions;
  out ActualCharCount: SizeUInt): TConvertResult;
var
  DestI, SrcI: SizeUInt;
  W1, W2: Word;
  C: Cardinal;

  function UnfinishedCharError: Boolean;
  begin
    if toUnfinishedCharToSymbol in Options then
    begin
      Dest[DestI] := Char('?');
      Inc(DestI);
      Result := False;
    end
    else
      if toUnfinishedCharError in Options then
      begin
        ConvertUTF16ToUTF8 := trUnfinishedChar;
        Result := True;
      end
      else Result := False;
  end;

  function InvalidCharError(Count: SizeUInt): Boolean; inline;
  begin
    if not (toInvalidCharError in Options) then
    begin
      if toInvalidCharToSymbol in Options then
      begin
        Dest[DestI] := Char('?');
        Inc(DestI);
      end;

      Dec(SrcI, Count);
      // skip trailing UTF-16 wide char
      if (Word(Src[SrcI]) and $FC00) = $DC00 then Inc(SrcI);

      Result := False;
    end
    else
      if toInvalidCharError in Options then
      begin
        ConvertUTF16ToUTF8 := trUnfinishedChar;
        Result := True;
      end;
  end;

begin
  ActualCharCount := 0;

  if not Assigned(Src) then
  begin
    Result := trNullSrc;
    Exit;
  end;

  if not Assigned(Dest) then
  begin
    Result := trNullDest;
    Exit;
  end;
  SrcI := 0;
  DestI := 0;

  while (DestI < DestCharCount) and (SrcI < SrcWideCharCount) do
  begin
    W1 := Word(Src[SrcI]);
    Inc(SrcI);

    if (W1 < $D800) or (W1 > $DFFF) then // single wide char UTF-16 char
    begin
      if W1 < $0080 then // to single byte UTF-8 char
      begin
        Dest[DestI] := Char(W1);
        Inc(DestI);
      end
      else
        if W1 < $0800 then // to double byte UTF-8 char
        begin
          Dest[DestI] := Char(%11000000 or ((W1 and %11111000000) shr 6));
          Inc(DestI);
          if DestI >= DestCharCount then Break;
          Dest[DestI] := Char(%10000000 or (W1 and %111111));
          Inc(DestI);
        end
        else
        begin // to triple byte UTF-8 char
          Dest[DestI] := Char(%11100000 or ((W1 and %1111000000000000) shr 12));
          Inc(DestI);
          if DestI >= DestCharCount then Break;
          Dest[DestI] := Char(%10000000 or ((W1 and %111111000000) shr 6));
          Inc(DestI);
          if DestI >= DestCharCount then Break;
          Dest[DestI] := Char(%10000000 or (W1 and %111111));
          Inc(DestI);
        end;
    end
    else
    begin
      if SrcI >= SrcWideCharCount then
        if UnfinishedCharError then Exit(trInvalidChar)
        else Break;

      W2 := Word(Src[SrcI]);
      Inc(SrcI);

      if (W1 and $F800) = $D800 then // double wide char UTF-16 char
      begin
        if (W2 and $FC00) = $DC00 then
        begin
          C := (W1 - $D800) shl 10 + (W2 - $DC00) + $10000;

          // to 4 byte UTF-8 char
          Dest[DestI] := Char(%11110000 or (C shr 18));
          Inc(DestI);
          if DestI >= DestCharCount then Break;
          Dest[DestI] := Char(%10000000 or ((C and $3F000) shr 12));
          Inc(DestI);
          if DestI >= DestCharCount then Break;
          Dest[DestI] := Char(%10000000 or ((C and %111111000000) shr 6));
          Inc(DestI);
          if DestI >= DestCharCount then Break;
          Dest[DestI] := Char(%10000000 or (C and %111111));
          Inc(DestI);
        end
        else // invalid character, assume single wide char UTF-16 char
          if InvalidCharError(1) then Exit(trInvalidChar);
      end
      else // invalid character, assume single wide char UTF-16 char
        if InvalidCharError(1) then Exit(trInvalidChar);
    end;
  end;

  if DestI >= DestCharCount then
  begin
    DestI := DestCharCount - 1;
    Result := trDestExhausted;
  end
  else
    Result := trNoError;

  Dest[DestI] := #0;
  ActualCharCount := DestI + 1;
end;

{------------------------------------------------------------------------------
  Name:    UTF8ToUTF16
  Params:  S - Source UTF-8 string
  Returns: UTF-16 encoded string

  Converts the specified UTF-8 encoded string to UTF-16 encoded (system endian)
  Avoid copying the result string since on windows a widestring requires a full
  copy
 ------------------------------------------------------------------------------}
function UTF8ToUTF16(const S: AnsiString): UnicodeString;
var
  L: SizeUInt;
begin
  if S = ''
  then begin
    Result := '';
    Exit;
  end;

  SetLength(Result, Length(S));
  // wide chars of UTF-16 <= bytes of UTF-8 string
  if ConvertUTF8ToUTF16(PWideChar(Result), Length(Result) + 1, PChar(S), Length(S),
    [toInvalidCharToSymbol], L) = trNoError
  then SetLength(Result, L - 1)
  else Result := '';
end;

{------------------------------------------------------------------------------
  Name:    UTF16ToUTF8
  Params:  S - Source UTF-16 string (system endian)
  Returns: UTF-8 encoded string

  Converts the specified UTF-16 encoded string (system endian) to UTF-8 encoded
 ------------------------------------------------------------------------------}
function UTF16ToUTF8(const S: UnicodeString): AnsiString;
var
  L: SizeUInt;
  R: AnsiString;
begin
  Result := '';
  if S = '' then Exit;

  SetLength(R, Length(S) * 3);
  // bytes of UTF-8 <= 3 * wide chars of UTF-16 string
  // e.g. %11100000 10100000 10000000 (UTF-8) is $0800 (UTF-16)
  if ConvertUTF16ToUTF8(PChar(R), Length(R) + 1, PWideChar(S), Length(S),
    [toInvalidCharToSymbol], L) = trNoError then
  begin
    SetLength(R, L - 1);
    Result := R;
  end;
end;

procedure LazGetLanguageIDs(var Lang, FallbackLang: String);

  {$IFDEF DARWIN}
  function GetLanguage: boolean;
  var
    Ref: CFStringRef;
    LangArray: CFMutableArrayRef;
    StrSize: CFIndex;
    StrRange: CFRange;
    Locals: CFArrayRef;
    Bundle: CFBundleRef;
  begin
    Result := false;
    Bundle:=CFBundleGetMainBundle;
    if Bundle=nil then exit;
    Locals:=CFBundleCopyBundleLocalizations(Bundle);
    if Locals=nil then exit;
    LangArray := CFBundleCopyLocalizationsForPreferences(Locals, nil);
    try
      if CFArrayGetCount(LangArray) > 0 then
      begin
        Ref := CFArrayGetValueAtIndex(LangArray, 0);
        StrRange.location := 0;
        StrRange.length := CFStringGetLength(Ref);

        CFStringGetBytes(Ref, StrRange, kCFStringEncodingUTF8,
          Ord('?'), False, nil, 0, StrSize);
        SetLength(Lang, StrSize);

        if StrSize > 0 then
        begin
          CFStringGetBytes(Ref, StrRange, kCFStringEncodingUTF8,
            Ord('?'), False, @Lang[1], StrSize, StrSize);
          Result:=true;
          FallbackLang := Copy(Lang, 1, 2);
        end;
      end;
    finally
      CFRelease(LangArray);
      CFRelease(Locals);
    end;
  end;
  {$ENDIF}
begin
{$IFDEF DARWIN}
  if not GetLanguage then
    GetLanguageIDs(Lang, FallbackLang);
{$ELSE}
  GetLanguageIDs(Lang, FallbackLang);
{$ENDIF}
end;

{
This routine will strip country information from the language ID
making it more simple

Ideally the resulting ID from here should conform to ISO 639-1
or ISO 639-2, if the language has no code in ISO 639-1
}
procedure LazGetShortLanguageID(var Lang: String);
var
  FallbackLang: String;
begin
  LazGetLanguageIDs(Lang, FallbackLang);

  // Simply making sure its length is at most 2 should be enough for most languages
  if Length(Lang) > 2 then Lang := Lang[1] + Lang[2];
end;

procedure InternalInit;
var
  c: Char;
begin
  for c:=Low(char) to High(char) do begin
    FPUpChars[c]:=upcase(c);
  end;
end;

initialization
  InternalInit;
  {$ifdef LAZUTF8_USE_TABLES}
  InitUnicodeTables;
  {$endif}

end.

