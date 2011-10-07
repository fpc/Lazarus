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
function UTF8LowerCase(const AInStr: utf8string): utf8string;
function UTF8LowerCase(const AInStr, ALocale: utf8string): utf8string;
function UTF8UpperCase(const AInStr: utf8string): utf8string;
function UTF8UpperCase(const AInStr, ALocale: utf8string): utf8string;
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

var
  FPUpChars: array[char] of char;

implementation

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
begin
  Result:=s;
  i:=1;
  while i<=length(Result) do begin
    case Result[i] of
    { First ASCII chars }
    'A'..'Z':
      begin
        Result[i]:=chr(ord(Result[i])+32);
        inc(i);
      end;
    { Now chars with multiple bytes }
    #192..#240:
      begin
        OldCode:=UTF8CharacterToUnicode(@Result[i],CharLen);
        NewCode:=UnicodeLowercase(OldCode);
        if NewCode=OldCode then begin
          inc(i,CharLen);
        end else begin
          UniqueString(Result);
          NewCharLen:=UnicodeToUTF8(NewCode,@Result[i]);
          if CharLen=NewCharLen then begin
            inc(i,NewCharLen);
          end else begin
            // string size changed => use slower function
            Result:=UTF8LowercaseDynLength(s);
            exit;
          end;
        end;
      end;
    else
      inc(i);
    end;
  end;
end;
{$endif}

function UTF8LowerCase(const AInStr: utf8string): utf8string;
begin
  Result := UTF8LowerCase(AInStr, '');
end;

{
  AInStr - The input string
  ALocale - The locale. Use '' for maximum speed if one desires to ignore the locale
}
function UTF8LowerCase(const AInStr, ALocale: utf8string): utf8string;
var
  i, InCounter, OutCounter: PtrInt;
  CharLen: integer;
  CharProcessed: Boolean;
  NewCharLen: integer;
  NewChar, OldChar: Word;
  // Language identification
  IsTurkish: Boolean;
begin
  // Start with the same string, and progressively modify
  Result:=AInStr;

  // Language identification
  IsTurkish := ALocale = 'tu';

  InCounter:=1; // for AInStr
  OutCounter := 1; // for Result
  while InCounter<=length(AInStr) do
  begin
    { First ASCII chars }
    if (AInStr[InCounter] <= 'Z') and (AInStr[InCounter] >= 'A') then
    begin
      // Special turkish handling
      // capital undotted I to small undotted i
      if IsTurkish and (AInStr[InCounter] = 'I') then
      begin
        SetLength(Result,Length(Result)+1);// Increase the buffer
        Result[OutCounter]:=#$C4;
        Result[OutCounter+1]:=#$B1;
        inc(InCounter);
        inc(OutCounter,2);
      end
      else
      begin
        Result[OutCounter]:=chr(ord(AInStr[InCounter])+32);
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
        $C380..$C39E: NewChar := OldChar + $20;
        // $C39F: ß already lowercase
        $C481..$C4A9: if OldChar mod 2 = 0 then NewChar := OldChar + 1;
        // Turkish capital dotted i to small dotted i
        $C4B0:
        begin
          Result[OutCounter]:='i';
          NewCharLen := 1;
          CharProcessed := True;
        end;
        // $C4B1 turkish lowercase undotted ı
        $C4B2..$C4B6: if OldChar mod 2 = 0 then NewChar := OldChar + 1;
        //$C4B7: ĸ => K ?
        $C4B8..$C588: if OldChar mod 2 = 1 then NewChar := OldChar + 1;
        //$C589 ŉ => ?
        $C58A..$C5B7: if OldChar mod 2 = 0 then NewChar := OldChar + 1;
        $C5B8:        NewChar := $C3BF; // Ÿ
        $C5B9..$C8B3: if OldChar mod 2 = 1 then NewChar := OldChar + 1;
        //
        $CE91..$CE9F: NewChar := OldChar + $20; // Greek Characters
        $CEA0..$CEA9: NewChar := OldChar + $E0; // Greek Characters
        $D090..$D09F: NewChar := OldChar + $20; // Cyrillic alphabet
        $D0A0..$D0AF: NewChar := OldChar + $E0; // Cyrillic alphabet
        end;

        if NewChar <> 0 then
        begin
          Result[OutCounter]  := Chr(Hi(NewChar));
          Result[OutCounter+1]:= Chr(Lo(NewChar));
          CharProcessed := True;
        end;
      end;

      // Copy the character if the string was disaligned by previous changed
      // and no processing was done in this character
      if (InCounter <> OutCounter) and (not CharProcessed) then
      begin
        for i := 0 to CharLen-1 do
          Result[OutCounter+i]  :=AInStr[InCounter+i];
      end;

      inc(InCounter, CharLen);
      inc(OutCounter, NewCharLen);
    end; // case
  end; // while

  // Final correction of the buffer size
  SetLength(Result,OutCounter-1);
end;

function UTF8UpperCase(const AInStr: utf8string): utf8string;
begin
  Result := UTF8UpperCase(AInStr, '');
end;

{
  AInStr - The input string
  ALocale - The locale. Use '' for maximum speed if one desires to ignore the locale
}
function UTF8UpperCase(const AInStr, ALocale: utf8string): utf8string;
var
  i, InCounter, OutCounter: PtrInt;
  CharLen: integer;
  CharProcessed: Boolean;
  NewCharLen: integer;
  NewChar, OldChar: Word;
  // Language identification
  IsTurkish: Boolean;
begin
  // Start with the same string, and progressively modify
  Result:=AInStr;

  // Language identification
  IsTurkish := ALocale = 'tu';

  InCounter:=1; // for AInStr
  OutCounter := 1; // for Result
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
        Result[OutCounter]:=#$C4;
        Result[OutCounter+1]:=#$B0;
        inc(InCounter);
        inc(OutCounter,2);
      end
      else
      begin
        Result[OutCounter]:=chr(ord(AInStr[InCounter])-32);
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
        $C3A0..$C3BE: NewChar := OldChar - $20;
        $C3BF:        NewChar := $C5B8; // ÿ
        $C481..$C4B0: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        // turkish small undotted i to capital undotted i
        $C4B1:
        begin
          Result[OutCounter]:='I';
          NewCharLen := 1;
          CharProcessed := True;
        end;
        $C4B2..$C4B6: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        //$C4B7: ĸ => K ?
        $C4B8..$C588: if OldChar mod 2 = 0 then NewChar := OldChar - 1;
        //$C589 ŉ => ?
        $C58A..$C5B7: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        //$C5B8: // Ÿ already uppercase
        $C5B9..$C8B3: if OldChar mod 2 = 0 then NewChar := OldChar - 1;
        //
        $CEB1..$CEBF: NewChar := OldChar - $20; // Greek Characters
        $CF80..$CF89: NewChar := OldChar - $E0; // Greek Characters
        $D0B0..$D0BF: NewChar := OldChar - $20; // Cyrillic alphabet
        $D180..$D18F: NewChar := OldChar - $E0; // Cyrillic alphabet
        end;

        if NewChar <> 0 then
        begin
          Result[OutCounter]  := Chr(Hi(NewChar));
          Result[OutCounter+1]:= Chr(Lo(NewChar));
          CharProcessed := True;
        end;
      end
      else if CharLen = 3 then
      begin
        //
      end;

      // Copy the character if the string was disaligned by previous changed
      // and no processing was done in this character
      if (InCounter <> OutCounter) and (not CharProcessed) then
      begin
        for i := 0 to CharLen-1 do
          Result[OutCounter+i]  :=AInStr[InCounter+i];
      end;

      inc(InCounter, CharLen);
      inc(OutCounter, NewCharLen);
    end;
  end; // while

  // Final correction of the buffer size
  SetLength(Result,OutCounter-1);
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

