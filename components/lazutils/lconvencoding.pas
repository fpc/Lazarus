{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Note:
    The functions of this unit are thread-safe.
}
unit LConvEncoding;

{$mode objfpc}{$H+}

{$i lazutils_defines.inc}

interface

{ $Define DisableAsianCodePages}
//{$if FPC_FULLVERSION >= 30000}
  {$IFDEF UTF8_RTL}
    // Windows provides conversion functions.
    // Unix: unit cwstring provides conversion functions which are used by default UTF-8 encoding system.
    {$Define UseSystemCPConv} // use system conversions
  {$ENDIF}
//{$IFEND}
{$ifdef UseLCPConv}{$undef UseSystemCPConv}{$endif}

uses
  SysUtils, Classes, dos, LazUTF8
  {$IFDEF EnableIconvEnc},iconvenc{$ENDIF};

var
  ConvertEncodingFromUtf8RaisesException: boolean = False;

//encoding names
const
  EncodingUTF8 = 'utf8';
  EncodingAnsi = 'ansi';
  EncodingUTF8BOM = 'utf8bom'; // UTF-8 with byte order mark
  EncodingUCS2LE = 'ucs2le'; // UCS 2 byte little endian
  EncodingUCS2BE = 'ucs2be'; // UCS 2 byte big endian

  EncodingCP1250 = 'cp1250';
  EncodingCP1251 = 'cp1251';
  EncodingCP1252 = 'cp1252';
  EncodingCP1253 = 'cp1253';
  EncodingCP1254 = 'cp1254';
  EncodingCP1255 = 'cp1255';
  EncodingCP1256 = 'cp1256';
  EncodingCP1257 = 'cp1257';
  EncodingCP1258 = 'cp1258';

  EncodingCP437 = 'cp437';
  EncodingCP850 = 'cp850';
  EncodingCP852 = 'cp852';
  EncodingCP866 = 'cp866';
  EncodingCP874 = 'cp874';

  EncodingCP932 = 'cp932';
  EncodingCP936 = 'cp936';
  EncodingCP949 = 'cp949';
  EncodingCP950 = 'cp950';

  EncodingCPMac = 'macintosh';
  EncodingCPKOI8 = 'koi8';

  EncodingCPIso1 = 'iso88591';
  EncodingCPIso2 = 'iso88592';
  EncodingCPIso15 = 'iso885915';

//signatures in ansi
const
  UTF8BOM = #$EF#$BB#$BF;
  UTF16BEBOM = #$FE#$FF;
  UTF16LEBOM = #$FF#$FE;
  UTF32BEBOM = #0#0#$FE#$FF;
  UTF32LEBOM = #$FE#$FF#0#0;

function GuessEncoding(const s: string): string;

function ConvertEncodingFromUTF8(const s, ToEncoding: string; out Encoded: boolean
  {$ifdef FPC_HAS_CPSTRING}; SetTargetCodePage: boolean = false{$endif}): string;
function ConvertEncodingToUTF8(const s, FromEncoding: string; out Encoded: boolean): string;
// For UTF8 use the above functions, they save you one parameter
function ConvertEncoding(const s, FromEncoding, ToEncoding: string
  {$ifdef FPC_HAS_CPSTRING}; SetTargetCodePage: boolean = false{$endif}): string;

// This routine should obtain the encoding utilized by ansistring in the RTL
function GetDefaultTextEncoding: string;
// This routine returns the console text encoding, which might be different
// from the normal system encoding in some Windows systems
// see http://mantis.freepascal.org/view.php?id=20552
function GetConsoleTextEncoding: string;
function NormalizeEncoding(const Encoding: string): string;

type
  TConvertEncodingFunction = function(const s: string): string;
  {$ifdef FPC_HAS_CPSTRING}
  TConvertUTF8ToEncodingFunc = function(const s: string; SetTargetCodePage: boolean = false): RawByteString;
  {$else}
  TConvertUTF8ToEncodingFunc = function(const s: string): string;
  {$endif}
  TCharToUTF8Table = array[char] of PChar;
  TUnicodeToCharID = function(Unicode: cardinal): integer;
var
  ConvertAnsiToUTF8: TConvertEncodingFunction = nil;
  ConvertUTF8ToAnsi: TConvertUTF8ToEncodingFunc = nil;

function UTF8BOMToUTF8(const s: string): string; // UTF8 with BOM
function ISO_8859_1ToUTF8(const s: string): string; // central europe
function ISO_8859_15ToUTF8(const s: string): string; // Western European languages
function ISO_8859_2ToUTF8(const s: string): string; // eastern europe
function CP1250ToUTF8(const s: string): string; // central europe
function CP1251ToUTF8(const s: string): string; // cyrillic
function CP1252ToUTF8(const s: string): string; // latin 1
function CP1253ToUTF8(const s: string): string; // greek
function CP1254ToUTF8(const s: string): string; // turkish
function CP1255ToUTF8(const s: string): string; // hebrew
function CP1256ToUTF8(const s: string): string; // arabic
function CP1257ToUTF8(const s: string): string; // baltic
function CP1258ToUTF8(const s: string): string; // vietnam
function CP437ToUTF8(const s: string): string;  // DOS central europe
function CP850ToUTF8(const s: string): string;  // DOS western europe
function CP852ToUTF8(const s: string): string;  // DOS central europe
function CP866ToUTF8(const s: string): string;  // DOS and Windows console's cyrillic
function CP874ToUTF8(const s: string): string;  // thai
function KOI8ToUTF8(const s: string): string;  // russian cyrillic
function MacintoshToUTF8(const s: string): string;  // Macintosh, alias Mac OS Roman
function SingleByteToUTF8(const s: string; const Table: TCharToUTF8Table): string;
function UCS2LEToUTF8(const s: string): string; // UCS2-LE 2byte little endian
function UCS2BEToUTF8(const s: string): string; // UCS2-BE 2byte big endian

function UTF8ToUTF8BOM(const s: string): string; // UTF8 with BOM
{$ifdef FPC_HAS_CPSTRING}
function UTF8ToISO_8859_1(const s: string; SetTargetCodePage: boolean = false): RawByteString; // central europe
function UTF8ToISO_8859_2(const s: string; SetTargetCodePage: boolean = false): RawByteString; // eastern europe
function UTF8ToISO_8859_15(const s: string; SetTargetCodePage: boolean = false): RawByteString; // Western European languages
function UTF8ToCP1250(const s: string; SetTargetCodePage: boolean = false): RawByteString; // central europe
function UTF8ToCP1251(const s: string; SetTargetCodePage: boolean = false): RawByteString; // cyrillic
function UTF8ToCP1252(const s: string; SetTargetCodePage: boolean = false): RawByteString; // latin 1
function UTF8ToCP1253(const s: string; SetTargetCodePage: boolean = false): RawByteString; // greek
function UTF8ToCP1254(const s: string; SetTargetCodePage: boolean = false): RawByteString; // turkish
function UTF8ToCP1255(const s: string; SetTargetCodePage: boolean = false): RawByteString; // hebrew
function UTF8ToCP1256(const s: string; SetTargetCodePage: boolean = false): RawByteString; // arabic
function UTF8ToCP1257(const s: string; SetTargetCodePage: boolean = false): RawByteString; // baltic
function UTF8ToCP1258(const s: string; SetTargetCodePage: boolean = false): RawByteString; // vietnam
function UTF8ToCP437(const s: string; SetTargetCodePage: boolean = false): RawByteString;  // DOS central europe
function UTF8ToCP850(const s: string; SetTargetCodePage: boolean = false): RawByteString;  // DOS western europe
function UTF8ToCP852(const s: string; SetTargetCodePage: boolean = false): RawByteString;  // DOS central europe
function UTF8ToCP866(const s: string; SetTargetCodePage: boolean = false): RawByteString;  // DOS and Windows console's cyrillic
function UTF8ToCP874(const s: string; SetTargetCodePage: boolean = false): RawByteString;  // thai
function UTF8ToKOI8(const s: string; SetTargetCodePage: boolean = false): RawByteString;  // russian cyrillic
function UTF8ToKOI8U(const s: string; SetTargetCodePage: boolean = false): RawByteString;  // ukrainian cyrillic
function UTF8ToKOI8RU(const s: string; SetTargetCodePage: boolean = false): RawByteString;  // belarussian cyrillic
function UTF8ToMacintosh(const s: string; SetTargetCodePage: boolean = false): RawByteString;  // Macintosh, alias Mac OS Roman
{$ELSE}
function UTF8ToISO_8859_1(const s: string): string; // central europe
function UTF8ToISO_8859_15(const s: string): string; // Western European languages
function UTF8ToISO_8859_2(const s: string): string; // eastern europe
function UTF8ToCP1250(const s: string): string; // central europe
function UTF8ToCP1251(const s: string): string; // cyrillic
function UTF8ToCP1252(const s: string): string; // latin 1
function UTF8ToCP1253(const s: string): string; // greek
function UTF8ToCP1254(const s: string): string; // turkish
function UTF8ToCP1255(const s: string): string; // hebrew
function UTF8ToCP1256(const s: string): string; // arabic
function UTF8ToCP1257(const s: string): string; // baltic
function UTF8ToCP1258(const s: string): string; // vietnam
function UTF8ToCP437(const s: string): string;  // DOS central europe
function UTF8ToCP850(const s: string): string;  // DOS western europe
function UTF8ToCP852(const s: string): string;  // DOS central europe
function UTF8ToCP866(const s: string): string;  // DOS and Windows console's cyrillic
function UTF8ToCP874(const s: string): string;  // thai
function UTF8ToKOI8(const s: string): string;  // russian cyrillic
function UTF8ToKOI8U(const s: string): string;  // ukrainian cyrillic
function UTF8ToKOI8RU(const s: string): string;  // belarussian cyrillic
function UTF8ToMacintosh(const s: string): string;  // Macintosh, alias Mac OS Roman
{$ENDIF}
// custom conversion
function UTF8ToSingleByte(const s: string; const UTF8CharConvFunc: TUnicodeToCharID): string;

function UTF8ToUCS2LE(const s: string): string; // UCS2-LE 2byte little endian without BOM
function UTF8ToUCS2BE(const s: string): string; // UCS2-BE 2byte big endian without BOM

{$IFnDEF DisableAsianCodePages}
// Asian encodings
function CP932ToUTF8(const s: string): string;      // Japanese
function CP936ToUTF8(const s: string): string;      // Chinese
function CP949ToUTF8(const s: string): string;      // Korea
function CP950ToUTF8(const s: string): string;      // Chinese Complex

function DBCSToUTF8(const s: string; CodeP: integer): string;

{$ifdef FPC_HAS_CPSTRING}
function UTF8ToCP932(const s: string; SetTargetCodePage: boolean = false): RawByteString; // Japanese
function UTF8ToCP936(const s: string; SetTargetCodePage: boolean = false): RawByteString; // Chinese, essentially the same as GB 2312 and a predecessor to GB 18030
function UTF8ToCP949(const s: string; SetTargetCodePage: boolean = false): RawByteString; // Korea
function UTF8ToCP950(const s: string; SetTargetCodePage: boolean = false): RawByteString; // Chinese Complex
{$ELSE}
function UTF8ToCP932(const s: string): string;      // Japanese
function UTF8ToCP936(const s: string): string;      // Chinese, essentially the same as GB 2312 and a predecessor to GB 18030
function UTF8ToCP949(const s: string): string;      // Korea
function UTF8ToCP950(const s: string): string;      // Chinese Complex
{$ENDIF}
// Common function used by all UTF8ToXXX functions.
function UTF8ToDBCS(const s: string; const UTF8CharConvFunc: TUnicodeToCharID): string;
{$ENDIF}

procedure GetSupportedEncodings(List: TStrings);

implementation

{$IFDEF Windows}
uses Windows;
{$ENDIF}

var
  EncodingValid: boolean = false;
  DefaultTextEncoding: string = EncodingAnsi;

{$IFnDEF DisableAsianCodePages}
{$include asiancodepages.inc}
{$include asiancodepagefunctions.inc}
{$ENDIF}

{$IFDEF Windows}
// AConsole - If false, it is the general system encoding,
//            if true, it is the console encoding
function GetWindowsEncoding(AConsole: Boolean = False): string;
var
  cp : UINT;
{$IFDEF WinCE}
// CP_UTF8 is missing in the windows unit of the Windows CE RTL
const
  CP_UTF8 = 65001;
{$ENDIF}
begin
  if AConsole then cp := GetOEMCP
  else cp := GetACP;

  case cp of
    CP_UTF8: Result := EncodingUTF8;
  else
    Result:='cp'+IntToStr(cp);
  end;
end;
{$ELSE}
{$IFNDEF Darwin}
function GetUnixEncoding: string;
var
  Lang: string;
  i: integer;
begin
  Result:=EncodingAnsi;

  lang := GetEnv('LC_ALL');
  if Length(lang) = 0 then
  begin
    lang := GetEnv('LC_MESSAGES');
    if Length(lang) = 0 then
    begin
      lang := GetEnv('LANG');
    end;
  end;
  i:=pos('.',Lang);
  if (i>0) and (i<=length(Lang)) then
    Result:=copy(Lang,i+1,length(Lang)-i);
end;
{$ENDIF}
{$ENDIF}

function GetDefaultTextEncoding: string;
begin
  if EncodingValid then begin
    Result:=DefaultTextEncoding;
    exit;
  end;

  {$IFDEF Windows}
  Result:=GetWindowsEncoding;
  {$ELSE}
  {$IFDEF Darwin}
  Result:=EncodingUTF8;
  {$ELSE}
  Result:=GetUnixEncoding;
  {$ENDIF}
  {$ENDIF}

  Result:=NormalizeEncoding(Result);

  DefaultTextEncoding:=Result;
  EncodingValid:=true;
end;

function GetConsoleTextEncoding: string;
begin
  {$ifdef Windows}
  Result:=GetWindowsEncoding(True);
  Result:=NormalizeEncoding(Result);
  {$else}
  Result := GetDefaultTextEncoding;
  {$endif}
end;

function NormalizeEncoding(const Encoding: string): string;
var
  i: Integer;
begin
  Result:=LowerCase(Encoding);
  for i:=length(Result) downto 1 do
    if Result[i]='-' then System.Delete(Result,i,1);
end;

const
  ArrayISO_8859_1ToUTF8: TCharToUTF8Table = (
    #0,                 // #0
    #1,                 // #1
    #2,                 // #2
    #3,                 // #3
    #4,                 // #4
    #5,                 // #5
    #6,                 // #6
    #7,                 // #7
    #8,                 // #8
    #9,                 // #9
    #10,                // #10
    #11,                // #11
    #12,                // #12
    #13,                // #13
    #14,                // #14
    #15,                // #15
    #16,                // #16
    #17,                // #17
    #18,                // #18
    #19,                // #19
    #20,                // #20
    #21,                // #21
    #22,                // #22
    #23,                // #23
    #24,                // #24
    #25,                // #25
    #26,                // #26
    #27,                // #27
    #28,                // #28
    #29,                // #29
    #30,                // #30
    #31,                // #31
    ' ',                // ' '
    '!',                // '!'
    '"',                // '"'
    '#',                // '#'
    '$',                // '$'
    '%',                // '%'
    '&',                // '&'
    '''',               // ''''
    '(',                // '('
    ')',                // ')'
    '*',                // '*'
    '+',                // '+'
    ',',                // ','
    '-',                // '-'
    '.',                // '.'
    '/',                // '/'
    '0',                // '0'
    '1',                // '1'
    '2',                // '2'
    '3',                // '3'
    '4',                // '4'
    '5',                // '5'
    '6',                // '6'
    '7',                // '7'
    '8',                // '8'
    '9',                // '9'
    ':',                // ':'
    ';',                // ';'
    '<',                // '<'
    '=',                // '='
    '>',                // '>'
    '?',                // '?'
    '@',                // '@'
    'A',                // 'A'
    'B',                // 'B'
    'C',                // 'C'
    'D',                // 'D'
    'E',                // 'E'
    'F',                // 'F'
    'G',                // 'G'
    'H',                // 'H'
    'I',                // 'I'
    'J',                // 'J'
    'K',                // 'K'
    'L',                // 'L'
    'M',                // 'M'
    'N',                // 'N'
    'O',                // 'O'
    'P',                // 'P'
    'Q',                // 'Q'
    'R',                // 'R'
    'S',                // 'S'
    'T',                // 'T'
    'U',                // 'U'
    'V',                // 'V'
    'W',                // 'W'
    'X',                // 'X'
    'Y',                // 'Y'
    'Z',                // 'Z'
    '[',                // '['
    '\',                // '\'
    ']',                // ']'
    '^',                // '^'
    '_',                // '_'
    '`',                // '`'
    'a',                // 'a'
    'b',                // 'b'
    'c',                // 'c'
    'd',                // 'd'
    'e',                // 'e'
    'f',                // 'f'
    'g',                // 'g'
    'h',                // 'h'
    'i',                // 'i'
    'j',                // 'j'
    'k',                // 'k'
    'l',                // 'l'
    'm',                // 'm'
    'n',                // 'n'
    'o',                // 'o'
    'p',                // 'p'
    'q',                // 'q'
    'r',                // 'r'
    's',                // 's'
    't',                // 't'
    'u',                // 'u'
    'v',                // 'v'
    'w',                // 'w'
    'x',                // 'x'
    'y',                // 'y'
    'z',                // 'z'
    '{',                // '{'
    '|',                // '|'
    '}',                // '}'
    '~',                // '~'
    #127,               // #127
    #194#128,           // #128
    #194#129,           // #129
    #194#130,           // #130
    #194#131,           // #131
    #194#132,           // #132
    #194#133,           // #133
    #194#134,           // #134
    #194#135,           // #135
    #194#136,           // #136
    #194#137,           // #137
    #194#138,           // #138
    #194#139,           // #139
    #194#140,           // #140
    #194#141,           // #141
    #194#142,           // #142
    #194#143,           // #143
    #194#144,           // #144
    #194#145,           // #145
    #194#146,           // #146
    #194#147,           // #147
    #194#148,           // #148
    #194#149,           // #149
    #194#150,           // #150
    #194#151,           // #151
    #194#152,           // #152
    #194#153,           // #153
    #194#154,           // #154
    #194#155,           // #155
    #194#156,           // #156
    #194#157,           // #157
    #194#158,           // #158
    #194#159,           // #159
    #194#160,           // #160
    #194#161,           // #161
    #194#162,           // #162
    #194#163,           // #163
    #194#164,           // #164
    #194#165,           // #165
    #194#166,           // #166
    #194#167,           // #167
    #194#168,           // #168
    #194#169,           // #169
    #194#170,           // #170
    #194#171,           // #171
    #194#172,           // #172
    #194#173,           // #173
    #194#174,           // #174
    #194#175,           // #175
    #194#176,           // #176
    #194#177,           // #177
    #194#178,           // #178
    #194#179,           // #179
    #194#180,           // #180
    #194#181,           // #181
    #194#182,           // #182
    #194#183,           // #183
    #194#184,           // #184
    #194#185,           // #185
    #194#186,           // #186
    #194#187,           // #187
    #194#188,           // #188
    #194#189,           // #189
    #194#190,           // #190
    #194#191,           // #191
    #195#128,           // #192
    #195#129,           // #193
    #195#130,           // #194
    #195#131,           // #195
    #195#132,           // #196
    #195#133,           // #197
    #195#134,           // #198
    #195#135,           // #199
    #195#136,           // #200
    #195#137,           // #201
    #195#138,           // #202
    #195#139,           // #203
    #195#140,           // #204
    #195#141,           // #205
    #195#142,           // #206
    #195#143,           // #207
    #195#144,           // #208
    #195#145,           // #209
    #195#146,           // #210
    #195#147,           // #211
    #195#148,           // #212
    #195#149,           // #213
    #195#150,           // #214
    #195#151,           // #215
    #195#152,           // #216
    #195#153,           // #217
    #195#154,           // #218
    #195#155,           // #219
    #195#156,           // #220
    #195#157,           // #221
    #195#158,           // #222
    #195#159,           // #223
    #195#160,           // #224
    #195#161,           // #225
    #195#162,           // #226
    #195#163,           // #227
    #195#164,           // #228
    #195#165,           // #229
    #195#166,           // #230
    #195#167,           // #231
    #195#168,           // #232
    #195#169,           // #233
    #195#170,           // #234
    #195#171,           // #235
    #195#172,           // #236
    #195#173,           // #237
    #195#174,           // #238
    #195#175,           // #239
    #195#176,           // #240
    #195#177,           // #241
    #195#178,           // #242
    #195#179,           // #243
    #195#180,           // #244
    #195#181,           // #245
    #195#182,           // #246
    #195#183,           // #247
    #195#184,           // #248
    #195#185,           // #249
    #195#186,           // #250
    #195#187,           // #251
    #195#188,           // #252
    #195#189,           // #253
    #195#190,           // #254
    #195#191            // #255
  );

  ArrayISO_8859_15ToUTF8: TCharToUTF8Table = (
    #0,                 // #0
    #1,                 // #1
    #2,                 // #2
    #3,                 // #3
    #4,                 // #4
    #5,                 // #5
    #6,                 // #6
    #7,                 // #7
    #8,                 // #8
    #9,                 // #9
    #10,                // #10
    #11,                // #11
    #12,                // #12
    #13,                // #13
    #14,                // #14
    #15,                // #15
    #16,                // #16
    #17,                // #17
    #18,                // #18
    #19,                // #19
    #20,                // #20
    #21,                // #21
    #22,                // #22
    #23,                // #23
    #24,                // #24
    #25,                // #25
    #26,                // #26
    #27,                // #27
    #28,                // #28
    #29,                // #29
    #30,                // #30
    #31,                // #31
    ' ',                // ' '
    '!',                // '!'
    '"',                // '"'
    '#',                // '#'
    '$',                // '$'
    '%',                // '%'
    '&',                // '&'
    '''',               // ''''
    '(',                // '('
    ')',                // ')'
    '*',                // '*'
    '+',                // '+'
    ',',                // ','
    '-',                // '-'
    '.',                // '.'
    '/',                // '/'
    '0',                // '0'
    '1',                // '1'
    '2',                // '2'
    '3',                // '3'
    '4',                // '4'
    '5',                // '5'
    '6',                // '6'
    '7',                // '7'
    '8',                // '8'
    '9',                // '9'
    ':',                // ':'
    ';',                // ';'
    '<',                // '<'
    '=',                // '='
    '>',                // '>'
    '?',                // '?'
    '@',                // '@'
    'A',                // 'A'
    'B',                // 'B'
    'C',                // 'C'
    'D',                // 'D'
    'E',                // 'E'
    'F',                // 'F'
    'G',                // 'G'
    'H',                // 'H'
    'I',                // 'I'
    'J',                // 'J'
    'K',                // 'K'
    'L',                // 'L'
    'M',                // 'M'
    'N',                // 'N'
    'O',                // 'O'
    'P',                // 'P'
    'Q',                // 'Q'
    'R',                // 'R'
    'S',                // 'S'
    'T',                // 'T'
    'U',                // 'U'
    'V',                // 'V'
    'W',                // 'W'
    'X',                // 'X'
    'Y',                // 'Y'
    'Z',                // 'Z'
    '[',                // '['
    '\',                // '\'
    ']',                // ']'
    '^',                // '^'
    '_',                // '_'
    '`',                // '`'
    'a',                // 'a'
    'b',                // 'b'
    'c',                // 'c'
    'd',                // 'd'
    'e',                // 'e'
    'f',                // 'f'
    'g',                // 'g'
    'h',                // 'h'
    'i',                // 'i'
    'j',                // 'j'
    'k',                // 'k'
    'l',                // 'l'
    'm',                // 'm'
    'n',                // 'n'
    'o',                // 'o'
    'p',                // 'p'
    'q',                // 'q'
    'r',                // 'r'
    's',                // 's'
    't',                // 't'
    'u',                // 'u'
    'v',                // 'v'
    'w',                // 'w'
    'x',                // 'x'
    'y',                // 'y'
    'z',                // 'z'
    '{',                // '{'
    '|',                // '|'
    '}',                // '}'
    '~',                // '~'
    #127,               // #127
    #194#128,           // #128
    #194#129,           // #129
    #194#130,           // #130
    #194#131,           // #131
    #194#132,           // #132
    #194#133,           // #133
    #194#134,           // #134
    #194#135,           // #135
    #194#136,           // #136
    #194#137,           // #137
    #194#138,           // #138
    #194#139,           // #139
    #194#140,           // #140
    #194#141,           // #141
    #194#142,           // #142
    #194#143,           // #143
    #194#144,           // #144
    #194#145,           // #145
    #194#146,           // #146
    #194#147,           // #147
    #194#148,           // #148
    #194#149,           // #149
    #194#150,           // #150
    #194#151,           // #151
    #194#152,           // #152
    #194#153,           // #153
    #194#154,           // #154
    #194#155,           // #155
    #194#156,           // #156
    #194#157,           // #157
    #194#158,           // #158
    #194#159,           // #159
    #194#160,           // #160
    #194#161,           // #161
    #194#162,           // #162
    #194#163,           // #163
    #226#130#172,       // #164
    #194#165,           // #165
    #197#160,           // #166
    #194#167,           // #167
    #197#161,           // #168
    #194#169,           // #169
    #194#170,           // #170
    #194#171,           // #171
    #194#172,           // #172
    #194#173,           // #173
    #194#174,           // #174
    #194#175,           // #175
    #194#176,           // #176
    #194#177,           // #177
    #194#178,           // #178
    #194#179,           // #179
    #197#189,           // #180
    #194#181,           // #181
    #194#182,           // #182
    #194#183,           // #183
    #197#190,           // #184
    #194#185,           // #185
    #194#186,           // #186
    #194#187,           // #187
    #197#146,           // #188
    #197#147,           // #189
    #197#184,           // #190
    #194#191,           // #191
    #195#128,           // #192
    #195#129,           // #193
    #195#130,           // #194
    #195#131,           // #195
    #195#132,           // #196
    #195#133,           // #197
    #195#134,           // #198
    #195#135,           // #199
    #195#136,           // #200
    #195#137,           // #201
    #195#138,           // #202
    #195#139,           // #203
    #195#140,           // #204
    #195#141,           // #205
    #195#142,           // #206
    #195#143,           // #207
    #195#144,           // #208
    #195#145,           // #209
    #195#146,           // #210
    #195#147,           // #211
    #195#148,           // #212
    #195#149,           // #213
    #195#150,           // #214
    #195#151,           // #215
    #195#152,           // #216
    #195#153,           // #217
    #195#154,           // #218
    #195#155,           // #219
    #195#156,           // #220
    #195#157,           // #221
    #195#158,           // #222
    #195#159,           // #223
    #195#160,           // #224
    #195#161,           // #225
    #195#162,           // #226
    #195#163,           // #227
    #195#164,           // #228
    #195#165,           // #229
    #195#166,           // #230
    #195#167,           // #231
    #195#168,           // #232
    #195#169,           // #233
    #195#170,           // #234
    #195#171,           // #235
    #195#172,           // #236
    #195#173,           // #237
    #195#174,           // #238
    #195#175,           // #239
    #195#176,           // #240
    #195#177,           // #241
    #195#178,           // #242
    #195#179,           // #243
    #195#180,           // #244
    #195#181,           // #245
    #195#182,           // #246
    #195#183,           // #247
    #195#184,           // #248
    #195#185,           // #249
    #195#186,           // #250
    #195#187,           // #251
    #195#188,           // #252
    #195#189,           // #253
    #195#190,           // #254
    #195#191            // #255
  );
  ArrayISO_8859_2ToUTF8: TCharToUTF8Table = (
    #0,                 // #0
    #1,                 // #1
    #2,                 // #2
    #3,                 // #3
    #4,                 // #4
    #5,                 // #5
    #6,                 // #6
    #7,                 // #7
    #8,                 // #8
    #9,                 // #9
    #10,                // #10
    #11,                // #11
    #12,                // #12
    #13,                // #13
    #14,                // #14
    #15,                // #15
    #16,                // #16
    #17,                // #17
    #18,                // #18
    #19,                // #19
    #20,                // #20
    #21,                // #21
    #22,                // #22
    #23,                // #23
    #24,                // #24
    #25,                // #25
    #26,                // #26
    #27,                // #27
    #28,                // #28
    #29,                // #29
    #30,                // #30
    #31,                // #31
    ' ',                // ' '
    '!',                // '!'
    '"',                // '"'
    '#',                // '#'
    '$',                // '$'
    '%',                // '%'
    '&',                // '&'
    '''',               // ''''
    '(',                // '('
    ')',                // ')'
    '*',                // '*'
    '+',                // '+'
    ',',                // ','
    '-',                // '-'
    '.',                // '.'
    '/',                // '/'
    '0',                // '0'
    '1',                // '1'
    '2',                // '2'
    '3',                // '3'
    '4',                // '4'
    '5',                // '5'
    '6',                // '6'
    '7',                // '7'
    '8',                // '8'
    '9',                // '9'
    ':',                // ':'
    ';',                // ';'
    '<',                // '<'
    '=',                // '='
    '>',                // '>'
    '?',                // '?'
    '@',                // '@'
    'A',                // 'A'
    'B',                // 'B'
    'C',                // 'C'
    'D',                // 'D'
    'E',                // 'E'
    'F',                // 'F'
    'G',                // 'G'
    'H',                // 'H'
    'I',                // 'I'
    'J',                // 'J'
    'K',                // 'K'
    'L',                // 'L'
    'M',                // 'M'
    'N',                // 'N'
    'O',                // 'O'
    'P',                // 'P'
    'Q',                // 'Q'
    'R',                // 'R'
    'S',                // 'S'
    'T',                // 'T'
    'U',                // 'U'
    'V',                // 'V'
    'W',                // 'W'
    'X',                // 'X'
    'Y',                // 'Y'
    'Z',                // 'Z'
    '[',                // '['
    '\',                // '\'
    ']',                // ']'
    '^',                // '^'
    '_',                // '_'
    '`',                // '`'
    'a',                // 'a'
    'b',                // 'b'
    'c',                // 'c'
    'd',                // 'd'
    'e',                // 'e'
    'f',                // 'f'
    'g',                // 'g'
    'h',                // 'h'
    'i',                // 'i'
    'j',                // 'j'
    'k',                // 'k'
    'l',                // 'l'
    'm',                // 'm'
    'n',                // 'n'
    'o',                // 'o'
    'p',                // 'p'
    'q',                // 'q'
    'r',                // 'r'
    's',                // 's'
    't',                // 't'
    'u',                // 'u'
    'v',                // 'v'
    'w',                // 'w'
    'x',                // 'x'
    'y',                // 'y'
    'z',                // 'z'
    '{',                // '{'
    '|',                // '|'
    '}',                // '}'
    '~',                // '~'
    #127,               // #127
    #194#128,           // #128
    #194#129,           // #129
    #194#130,           // #130
    #194#131,           // #131
    #194#132,           // #132
    #194#133,           // #133
    #194#134,           // #134
    #194#135,           // #135
    #194#136,           // #136
    #194#137,           // #137
    #194#138,           // #138
    #194#139,           // #139
    #194#140,           // #140
    #194#141,           // #141
    #194#142,           // #142
    #194#143,           // #143
    #194#144,           // #144
    #194#145,           // #145
    #194#146,           // #146
    #194#147,           // #147
    #194#148,           // #148
    #194#149,           // #149
    #194#150,           // #150
    #194#151,           // #151
    #194#152,           // #152
    #194#153,           // #153
    #194#154,           // #154
    #194#155,           // #155
    #194#156,           // #156
    #194#157,           // #157
    #194#158,           // #158
    #194#159,           // #159
    #194#160,           // #160
    #196#132,           // #161
    #203#152,           // #162
    #197#129,           // #163
    #194#164,           // #164
    #196#189,           // #165
    #197#154,           // #166
    #194#167,           // #167
    #194#168,           // #168
    #197#160,           // #169
    #197#158,           // #170
    #197#164,           // #171
    #197#185,           // #172
    #194#173,           // #173
    #197#189,           // #174
    #197#187,           // #175
    #194#176,           // #176
    #196#133,           // #177
    #203#155,           // #178
    #197#130,           // #179
    #194#180,           // #180
    #196#190,           // #181
    #197#155,           // #182
    #203#135,           // #183
    #194#184,           // #184
    #197#161,           // #185
    #197#159,           // #186
    #197#165,           // #187
    #197#186,           // #188
    #203#157,           // #189
    #197#190,           // #190
    #197#188,           // #191
    #197#148,           // #192
    #195#129,           // #193
    #195#130,           // #194
    #196#130,           // #195
    #195#132,           // #196
    #196#185,           // #197
    #196#134,           // #198
    #195#135,           // #199
    #196#140,           // #200
    #195#137,           // #201
    #196#152,           // #202
    #195#139,           // #203
    #196#154,           // #204
    #195#141,           // #205
    #195#142,           // #206
    #196#142,           // #207
    #196#144,           // #208
    #197#131,           // #209
    #197#135,           // #210
    #195#147,           // #211
    #195#148,           // #212
    #197#144,           // #213
    #195#150,           // #214
    #195#151,           // #215
    #197#152,           // #216
    #197#174,           // #217
    #195#154,           // #218
    #197#176,           // #219
    #195#156,           // #220
    #195#157,           // #221
    #197#162,           // #222
    #195#159,           // #223
    #197#149,           // #224
    #195#161,           // #225
    #195#162,           // #226
    #196#131,           // #227
    #195#164,           // #228
    #196#186,           // #229
    #196#135,           // #230
    #195#167,           // #231
    #196#141,           // #232
    #195#169,           // #233
    #196#153,           // #234
    #195#171,           // #235
    #196#155,           // #236
    #195#173,           // #237
    #195#174,           // #238
    #196#143,           // #239
    #196#145,           // #240
    #197#132,           // #241
    #197#136,           // #242
    #195#179,           // #243
    #195#180,           // #244
    #197#145,           // #245
    #195#182,           // #246
    #195#183,           // #247
    #197#153,           // #248
    #197#175,           // #249
    #195#186,           // #250
    #197#177,           // #251
    #195#188,           // #252
    #195#189,           // #253
    #197#163,           // #254
    #203#153            // #255
  );


  ArrayCP1250ToUTF8: TCharToUTF8Table = (
    #0,                 // #0
    #1,                 // #1
    #2,                 // #2
    #3,                 // #3
    #4,                 // #4
    #5,                 // #5
    #6,                 // #6
    #7,                 // #7
    #8,                 // #8
    #9,                 // #9
    #10,                // #10
    #11,                // #11
    #12,                // #12
    #13,                // #13
    #14,                // #14
    #15,                // #15
    #16,                // #16
    #17,                // #17
    #18,                // #18
    #19,                // #19
    #20,                // #20
    #21,                // #21
    #22,                // #22
    #23,                // #23
    #24,                // #24
    #25,                // #25
    #26,                // #26
    #27,                // #27
    #28,                // #28
    #29,                // #29
    #30,                // #30
    #31,                // #31
    ' ',                // ' '
    '!',                // '!'
    '"',                // '"'
    '#',                // '#'
    '$',                // '$'
    '%',                // '%'
    '&',                // '&'
    '''',               // ''''
    '(',                // '('
    ')',                // ')'
    '*',                // '*'
    '+',                // '+'
    ',',                // ','
    '-',                // '-'
    '.',                // '.'
    '/',                // '/'
    '0',                // '0'
    '1',                // '1'
    '2',                // '2'
    '3',                // '3'
    '4',                // '4'
    '5',                // '5'
    '6',                // '6'
    '7',                // '7'
    '8',                // '8'
    '9',                // '9'
    ':',                // ':'
    ';',                // ';'
    '<',                // '<'
    '=',                // '='
    '>',                // '>'
    '?',                // '?'
    '@',                // '@'
    'A',                // 'A'
    'B',                // 'B'
    'C',                // 'C'
    'D',                // 'D'
    'E',                // 'E'
    'F',                // 'F'
    'G',                // 'G'
    'H',                // 'H'
    'I',                // 'I'
    'J',                // 'J'
    'K',                // 'K'
    'L',                // 'L'
    'M',                // 'M'
    'N',                // 'N'
    'O',                // 'O'
    'P',                // 'P'
    'Q',                // 'Q'
    'R',                // 'R'
    'S',                // 'S'
    'T',                // 'T'
    'U',                // 'U'
    'V',                // 'V'
    'W',                // 'W'
    'X',                // 'X'
    'Y',                // 'Y'
    'Z',                // 'Z'
    '[',                // '['
    '\',                // '\'
    ']',                // ']'
    '^',                // '^'
    '_',                // '_'
    '`',                // '`'
    'a',                // 'a'
    'b',                // 'b'
    'c',                // 'c'
    'd',                // 'd'
    'e',                // 'e'
    'f',                // 'f'
    'g',                // 'g'
    'h',                // 'h'
    'i',                // 'i'
    'j',                // 'j'
    'k',                // 'k'
    'l',                // 'l'
    'm',                // 'm'
    'n',                // 'n'
    'o',                // 'o'
    'p',                // 'p'
    'q',                // 'q'
    'r',                // 'r'
    's',                // 's'
    't',                // 't'
    'u',                // 'u'
    'v',                // 'v'
    'w',                // 'w'
    'x',                // 'x'
    'y',                // 'y'
    'z',                // 'z'
    '{',                // '{'
    '|',                // '|'
    '}',                // '}'
    '~',                // '~'
    #127,               // #127
    #226#130#172,       // #128
    #194#129,           // #129
    #226#128#154,       // #130
    #194#131,           // #131
    #226#128#158,       // #132
    #226#128#166,       // #133
    #226#128#160,       // #134
    #226#128#161,       // #135
    #194#136,           // #136
    #226#128#176,       // #137
    #197#160,           // #138
    #226#128#185,       // #139
    #197#154,           // #140
    #197#164,           // #141
    #197#189,           // #142
    #197#185,           // #143
    #194#144,           // #144
    #226#128#152,       // #145
    #226#128#153,       // #146
    #226#128#156,       // #147
    #226#128#157,       // #148
    #226#128#162,       // #149
    #226#128#147,       // #150
    #226#128#148,       // #151
    #194#152,           // #152
    #226#132#162,       // #153
    #197#161,           // #154
    #226#128#186,       // #155
    #197#155,           // #156
    #197#165,           // #157
    #197#190,           // #158
    #197#186,           // #159
    #194#160,           // #160
    #203#135,           // #161
    #203#152,           // #162
    #197#129,           // #163
    #194#164,           // #164
    #196#132,           // #165
    #194#166,           // #166
    #194#167,           // #167
    #194#168,           // #168
    #194#169,           // #169
    #197#158,           // #170
    #194#171,           // #171
    #194#172,           // #172
    #194#173,           // #173
    #194#174,           // #174
    #197#187,           // #175
    #194#176,           // #176
    #194#177,           // #177
    #203#155,           // #178
    #197#130,           // #179
    #194#180,           // #180
    #194#181,           // #181
    #194#182,           // #182
    #194#183,           // #183
    #194#184,           // #184
    #196#133,           // #185
    #197#159,           // #186
    #194#187,           // #187
    #196#189,           // #188
    #203#157,           // #189
    #196#190,           // #190
    #197#188,           // #191
    #197#148,           // #192
    #195#129,           // #193
    #195#130,           // #194
    #196#130,           // #195
    #195#132,           // #196
    #196#185,           // #197
    #196#134,           // #198
    #195#135,           // #199
    #196#140,           // #200
    #195#137,           // #201
    #196#152,           // #202
    #195#139,           // #203
    #196#154,           // #204
    #195#141,           // #205
    #195#142,           // #206
    #196#142,           // #207
    #196#144,           // #208
    #197#131,           // #209
    #197#135,           // #210
    #195#147,           // #211
    #195#148,           // #212
    #197#144,           // #213
    #195#150,           // #214
    #195#151,           // #215
    #197#152,           // #216
    #197#174,           // #217
    #195#154,           // #218
    #197#176,           // #219
    #195#156,           // #220
    #195#157,           // #221
    #197#162,           // #222
    #195#159,           // #223
    #197#149,           // #224
    #195#161,           // #225
    #195#162,           // #226
    #196#131,           // #227
    #195#164,           // #228
    #196#186,           // #229
    #196#135,           // #230
    #195#167,           // #231
    #196#141,           // #232
    #195#169,           // #233
    #196#153,           // #234
    #195#171,           // #235
    #196#155,           // #236
    #195#173,           // #237
    #195#174,           // #238
    #196#143,           // #239
    #196#145,           // #240
    #197#132,           // #241
    #197#136,           // #242
    #195#179,           // #243
    #195#180,           // #244
    #197#145,           // #245
    #195#182,           // #246
    #195#183,           // #247
    #197#153,           // #248
    #197#175,           // #249
    #195#186,           // #250
    #197#177,           // #251
    #195#188,           // #252
    #195#189,           // #253
    #197#163,           // #254
    #203#153            // #255
  );

  ArrayCP1251ToUTF8: TCharToUTF8Table = (
    #0,                 // #0
    #1,                 // #1
    #2,                 // #2
    #3,                 // #3
    #4,                 // #4
    #5,                 // #5
    #6,                 // #6
    #7,                 // #7
    #8,                 // #8
    #9,                 // #9
    #10,                // #10
    #11,                // #11
    #12,                // #12
    #13,                // #13
    #14,                // #14
    #15,                // #15
    #16,                // #16
    #17,                // #17
    #18,                // #18
    #19,                // #19
    #20,                // #20
    #21,                // #21
    #22,                // #22
    #23,                // #23
    #24,                // #24
    #25,                // #25
    #26,                // #26
    #27,                // #27
    #28,                // #28
    #29,                // #29
    #30,                // #30
    #31,                // #31
    ' ',                // ' '
    '!',                // '!'
    '"',                // '"'
    '#',                // '#'
    '$',                // '$'
    '%',                // '%'
    '&',                // '&'
    '''',               // ''''
    '(',                // '('
    ')',                // ')'
    '*',                // '*'
    '+',                // '+'
    ',',                // ','
    '-',                // '-'
    '.',                // '.'
    '/',                // '/'
    '0',                // '0'
    '1',                // '1'
    '2',                // '2'
    '3',                // '3'
    '4',                // '4'
    '5',                // '5'
    '6',                // '6'
    '7',                // '7'
    '8',                // '8'
    '9',                // '9'
    ':',                // ':'
    ';',                // ';'
    '<',                // '<'
    '=',                // '='
    '>',                // '>'
    '?',                // '?'
    '@',                // '@'
    'A',                // 'A'
    'B',                // 'B'
    'C',                // 'C'
    'D',                // 'D'
    'E',                // 'E'
    'F',                // 'F'
    'G',                // 'G'
    'H',                // 'H'
    'I',                // 'I'
    'J',                // 'J'
    'K',                // 'K'
    'L',                // 'L'
    'M',                // 'M'
    'N',                // 'N'
    'O',                // 'O'
    'P',                // 'P'
    'Q',                // 'Q'
    'R',                // 'R'
    'S',                // 'S'
    'T',                // 'T'
    'U',                // 'U'
    'V',                // 'V'
    'W',                // 'W'
    'X',                // 'X'
    'Y',                // 'Y'
    'Z',                // 'Z'
    '[',                // '['
    '\',                // '\'
    ']',                // ']'
    '^',                // '^'
    '_',                // '_'
    '`',                // '`'
    'a',                // 'a'
    'b',                // 'b'
    'c',                // 'c'
    'd',                // 'd'
    'e',                // 'e'
    'f',                // 'f'
    'g',                // 'g'
    'h',                // 'h'
    'i',                // 'i'
    'j',                // 'j'
    'k',                // 'k'
    'l',                // 'l'
    'm',                // 'm'
    'n',                // 'n'
    'o',                // 'o'
    'p',                // 'p'
    'q',                // 'q'
    'r',                // 'r'
    's',                // 's'
    't',                // 't'
    'u',                // 'u'
    'v',                // 'v'
    'w',                // 'w'
    'x',                // 'x'
    'y',                // 'y'
    'z',                // 'z'
    '{',                // '{'
    '|',                // '|'
    '}',                // '}'
    '~',                // '~'
    #127,               // #127
    #208#130,           // #128
    #208#131,           // #129
    #226#128#154,       // #130
    #209#147,           // #131
    #226#128#158,       // #132
    #226#128#166,       // #133
    #226#128#160,       // #134
    #226#128#161,       // #135
    #226#130#172,       // #136
    #226#128#176,       // #137
    #208#137,           // #138
    #226#128#185,       // #139
    #208#138,           // #140
    #208#140,           // #141
    #208#139,           // #142
    #208#143,           // #143
    #209#146,           // #144
    #226#128#152,       // #145
    #226#128#153,       // #146
    #226#128#156,       // #147
    #226#128#157,       // #148
    #226#128#162,       // #149
    #226#128#147,       // #150
    #226#128#148,       // #151
    #194#152,           // #152
    #226#132#162,       // #153
    #209#153,           // #154
    #226#128#186,       // #155
    #209#154,           // #156
    #209#156,           // #157
    #209#155,           // #158
    #209#159,           // #159
    #194#160,           // #160
    #208#142,           // #161
    #209#158,           // #162
    #208#136,           // #163
    #194#164,           // #164
    #210#144,           // #165
    #194#166,           // #166
    #194#167,           // #167
    #208#129,           // #168
    #194#169,           // #169
    #208#132,           // #170
    #194#171,           // #171
    #194#172,           // #172
    #194#173,           // #173
    #194#174,           // #174
    #208#135,           // #175
    #194#176,           // #176
    #194#177,           // #177
    #208#134,           // #178
    #209#150,           // #179
    #210#145,           // #180
    #194#181,           // #181
    #194#182,           // #182
    #194#183,           // #183
    #209#145,           // #184
    #226#132#150,       // #185
    #209#148,           // #186
    #194#187,           // #187
    #209#152,           // #188
    #208#133,           // #189
    #209#149,           // #190
    #209#151,           // #191
    #208#144,           // #192
    #208#145,           // #193
    #208#146,           // #194
    #208#147,           // #195
    #208#148,           // #196
    #208#149,           // #197
    #208#150,           // #198
    #208#151,           // #199
    #208#152,           // #200
    #208#153,           // #201
    #208#154,           // #202
    #208#155,           // #203
    #208#156,           // #204
    #208#157,           // #205
    #208#158,           // #206
    #208#159,           // #207
    #208#160,           // #208
    #208#161,           // #209
    #208#162,           // #210
    #208#163,           // #211
    #208#164,           // #212
    #208#165,           // #213
    #208#166,           // #214
    #208#167,           // #215
    #208#168,           // #216
    #208#169,           // #217
    #208#170,           // #218
    #208#171,           // #219
    #208#172,           // #220
    #208#173,           // #221
    #208#174,           // #222
    #208#175,           // #223
    #208#176,           // #224
    #208#177,           // #225
    #208#178,           // #226
    #208#179,           // #227
    #208#180,           // #228
    #208#181,           // #229
    #208#182,           // #230
    #208#183,           // #231
    #208#184,           // #232
    #208#185,           // #233
    #208#186,           // #234
    #208#187,           // #235
    #208#188,           // #236
    #208#189,           // #237
    #208#190,           // #238
    #208#191,           // #239
    #209#128,           // #240
    #209#129,           // #241
    #209#130,           // #242
    #209#131,           // #243
    #209#132,           // #244
    #209#133,           // #245
    #209#134,           // #246
    #209#135,           // #247
    #209#136,           // #248
    #209#137,           // #249
    #209#138,           // #250
    #209#139,           // #251
    #209#140,           // #252
    #209#141,           // #253
    #209#142,           // #254
    #209#143            // #255
  );

  ArrayCP1252ToUTF8: TCharToUTF8Table = (
    #0,                 // #0
    #1,                 // #1
    #2,                 // #2
    #3,                 // #3
    #4,                 // #4
    #5,                 // #5
    #6,                 // #6
    #7,                 // #7
    #8,                 // #8
    #9,                 // #9
    #10,                // #10
    #11,                // #11
    #12,                // #12
    #13,                // #13
    #14,                // #14
    #15,                // #15
    #16,                // #16
    #17,                // #17
    #18,                // #18
    #19,                // #19
    #20,                // #20
    #21,                // #21
    #22,                // #22
    #23,                // #23
    #24,                // #24
    #25,                // #25
    #26,                // #26
    #27,                // #27
    #28,                // #28
    #29,                // #29
    #30,                // #30
    #31,                // #31
    ' ',                // ' '
    '!',                // '!'
    '"',                // '"'
    '#',                // '#'
    '$',                // '$'
    '%',                // '%'
    '&',                // '&'
    '''',               // ''''
    '(',                // '('
    ')',                // ')'
    '*',                // '*'
    '+',                // '+'
    ',',                // ','
    '-',                // '-'
    '.',                // '.'
    '/',                // '/'
    '0',                // '0'
    '1',                // '1'
    '2',                // '2'
    '3',                // '3'
    '4',                // '4'
    '5',                // '5'
    '6',                // '6'
    '7',                // '7'
    '8',                // '8'
    '9',                // '9'
    ':',                // ':'
    ';',                // ';'
    '<',                // '<'
    '=',                // '='
    '>',                // '>'
    '?',                // '?'
    '@',                // '@'
    'A',                // 'A'
    'B',                // 'B'
    'C',                // 'C'
    'D',                // 'D'
    'E',                // 'E'
    'F',                // 'F'
    'G',                // 'G'
    'H',                // 'H'
    'I',                // 'I'
    'J',                // 'J'
    'K',                // 'K'
    'L',                // 'L'
    'M',                // 'M'
    'N',                // 'N'
    'O',                // 'O'
    'P',                // 'P'
    'Q',                // 'Q'
    'R',                // 'R'
    'S',                // 'S'
    'T',                // 'T'
    'U',                // 'U'
    'V',                // 'V'
    'W',                // 'W'
    'X',                // 'X'
    'Y',                // 'Y'
    'Z',                // 'Z'
    '[',                // '['
    '\',                // '\'
    ']',                // ']'
    '^',                // '^'
    '_',                // '_'
    '`',                // '`'
    'a',                // 'a'
    'b',                // 'b'
    'c',                // 'c'
    'd',                // 'd'
    'e',                // 'e'
    'f',                // 'f'
    'g',                // 'g'
    'h',                // 'h'
    'i',                // 'i'
    'j',                // 'j'
    'k',                // 'k'
    'l',                // 'l'
    'm',                // 'm'
    'n',                // 'n'
    'o',                // 'o'
    'p',                // 'p'
    'q',                // 'q'
    'r',                // 'r'
    's',                // 's'
    't',                // 't'
    'u',                // 'u'
    'v',                // 'v'
    'w',                // 'w'
    'x',                // 'x'
    'y',                // 'y'
    'z',                // 'z'
    '{',                // '{'
    '|',                // '|'
    '}',                // '}'
    '~',                // '~'
    #127,               // #127
    #226#130#172,       // #128
    #194#129,           // #129
    #226#128#154,       // #130
    #198#146,           // #131
    #226#128#158,       // #132
    #226#128#166,       // #133
    #226#128#160,       // #134
    #226#128#161,       // #135
    #203#134,           // #136
    #226#128#176,       // #137
    #197#160,           // #138
    #226#128#185,       // #139
    #197#146,           // #140
    #194#141,           // #141
    #197#189,           // #142
    #194#143,           // #143
    #194#144,           // #144
    #226#128#152,       // #145
    #226#128#153,       // #146
    #226#128#156,       // #147
    #226#128#157,       // #148
    #226#128#162,       // #149
    #226#128#147,       // #150
    #226#128#148,       // #151
    #203#156,           // #152
    #226#132#162,       // #153
    #197#161,           // #154
    #226#128#186,       // #155
    #197#147,           // #156
    #194#157,           // #157
    #197#190,           // #158
    #197#184,           // #159
    #194#160,           // #160
    #194#161,           // #161
    #194#162,           // #162
    #194#163,           // #163
    #194#164,           // #164
    #194#165,           // #165
    #194#166,           // #166
    #194#167,           // #167
    #194#168,           // #168
    #194#169,           // #169
    #194#170,           // #170
    #194#171,           // #171
    #194#172,           // #172
    #194#173,           // #173
    #194#174,           // #174
    #194#175,           // #175
    #194#176,           // #176
    #194#177,           // #177
    #194#178,           // #178
    #194#179,           // #179
    #194#180,           // #180
    #194#181,           // #181
    #194#182,           // #182
    #194#183,           // #183
    #194#184,           // #184
    #194#185,           // #185
    #194#186,           // #186
    #194#187,           // #187
    #194#188,           // #188
    #194#189,           // #189
    #194#190,           // #190
    #194#191,           // #191
    #195#128,           // #192
    #195#129,           // #193
    #195#130,           // #194
    #195#131,           // #195
    #195#132,           // #196
    #195#133,           // #197
    #195#134,           // #198
    #195#135,           // #199
    #195#136,           // #200
    #195#137,           // #201
    #195#138,           // #202
    #195#139,           // #203
    #195#140,           // #204
    #195#141,           // #205
    #195#142,           // #206
    #195#143,           // #207
    #195#144,           // #208
    #195#145,           // #209
    #195#146,           // #210
    #195#147,           // #211
    #195#148,           // #212
    #195#149,           // #213
    #195#150,           // #214
    #195#151,           // #215
    #195#152,           // #216
    #195#153,           // #217
    #195#154,           // #218
    #195#155,           // #219
    #195#156,           // #220
    #195#157,           // #221
    #195#158,           // #222
    #195#159,           // #223
    #195#160,           // #224
    #195#161,           // #225
    #195#162,           // #226
    #195#163,           // #227
    #195#164,           // #228
    #195#165,           // #229
    #195#166,           // #230
    #195#167,           // #231
    #195#168,           // #232
    #195#169,           // #233
    #195#170,           // #234
    #195#171,           // #235
    #195#172,           // #236
    #195#173,           // #237
    #195#174,           // #238
    #195#175,           // #239
    #195#176,           // #240
    #195#177,           // #241
    #195#178,           // #242
    #195#179,           // #243
    #195#180,           // #244
    #195#181,           // #245
    #195#182,           // #246
    #195#183,           // #247
    #195#184,           // #248
    #195#185,           // #249
    #195#186,           // #250
    #195#187,           // #251
    #195#188,           // #252
    #195#189,           // #253
    #195#190,           // #254
    #195#191            // #255
  );

  ArrayCP1253ToUTF8: TCharToUTF8Table = (
    #0,                 // #0
    #1,                 // #1
    #2,                 // #2
    #3,                 // #3
    #4,                 // #4
    #5,                 // #5
    #6,                 // #6
    #7,                 // #7
    #8,                 // #8
    #9,                 // #9
    #10,                // #10
    #11,                // #11
    #12,                // #12
    #13,                // #13
    #14,                // #14
    #15,                // #15
    #16,                // #16
    #17,                // #17
    #18,                // #18
    #19,                // #19
    #20,                // #20
    #21,                // #21
    #22,                // #22
    #23,                // #23
    #24,                // #24
    #25,                // #25
    #26,                // #26
    #27,                // #27
    #28,                // #28
    #29,                // #29
    #30,                // #30
    #31,                // #31
    ' ',                // ' '
    '!',                // '!'
    '"',                // '"'
    '#',                // '#'
    '$',                // '$'
    '%',                // '%'
    '&',                // '&'
    '''',               // ''''
    '(',                // '('
    ')',                // ')'
    '*',                // '*'
    '+',                // '+'
    ',',                // ','
    '-',                // '-'
    '.',                // '.'
    '/',                // '/'
    '0',                // '0'
    '1',                // '1'
    '2',                // '2'
    '3',                // '3'
    '4',                // '4'
    '5',                // '5'
    '6',                // '6'
    '7',                // '7'
    '8',                // '8'
    '9',                // '9'
    ':',                // ':'
    ';',                // ';'
    '<',                // '<'
    '=',                // '='
    '>',                // '>'
    '?',                // '?'
    '@',                // '@'
    'A',                // 'A'
    'B',                // 'B'
    'C',                // 'C'
    'D',                // 'D'
    'E',                // 'E'
    'F',                // 'F'
    'G',                // 'G'
    'H',                // 'H'
    'I',                // 'I'
    'J',                // 'J'
    'K',                // 'K'
    'L',                // 'L'
    'M',                // 'M'
    'N',                // 'N'
    'O',                // 'O'
    'P',                // 'P'
    'Q',                // 'Q'
    'R',                // 'R'
    'S',                // 'S'
    'T',                // 'T'
    'U',                // 'U'
    'V',                // 'V'
    'W',                // 'W'
    'X',                // 'X'
    'Y',                // 'Y'
    'Z',                // 'Z'
    '[',                // '['
    '\',                // '\'
    ']',                // ']'
    '^',                // '^'
    '_',                // '_'
    '`',                // '`'
    'a',                // 'a'
    'b',                // 'b'
    'c',                // 'c'
    'd',                // 'd'
    'e',                // 'e'
    'f',                // 'f'
    'g',                // 'g'
    'h',                // 'h'
    'i',                // 'i'
    'j',                // 'j'
    'k',                // 'k'
    'l',                // 'l'
    'm',                // 'm'
    'n',                // 'n'
    'o',                // 'o'
    'p',                // 'p'
    'q',                // 'q'
    'r',                // 'r'
    's',                // 's'
    't',                // 't'
    'u',                // 'u'
    'v',                // 'v'
    'w',                // 'w'
    'x',                // 'x'
    'y',                // 'y'
    'z',                // 'z'
    '{',                // '{'
    '|',                // '|'
    '}',                // '}'
    '~',                // '~'
    #127,               // #127
    #226#130#172,       // #128
    #194#129,           // #129
    #226#128#154,       // #130
    #198#146,           // #131
    #226#128#158,       // #132
    #226#128#166,       // #133
    #226#128#160,       // #134
    #226#128#161,       // #135
    #194#136,           // #136
    #226#128#176,       // #137
    #194#138,           // #138
    #226#128#185,       // #139
    #194#140,           // #140
    #194#141,           // #141
    #194#142,           // #142
    #194#143,           // #143
    #194#144,           // #144
    #226#128#152,       // #145
    #226#128#153,       // #146
    #226#128#156,       // #147
    #226#128#157,       // #148
    #226#128#162,       // #149
    #226#128#147,       // #150
    #226#128#148,       // #151
    #194#152,            // #152
    #226#132#162,       // #153
    #194#154,           // #154
    #226#128#186,       // #155
    #194#156,           // #156
    #194#157,           // #157
    #194#158,           // #158
    #194#159,           // #159
    #194#160,           // #160
    #206#133,           // #161
    #206#134,           // #162
    #194#163,           // #163
    #194#164,           // #164
    #194#165,           // #165
    #194#166,           // #166
    #194#167,           // #167
    #194#168,           // #168
    #194#169,           // #169
    #194#170,           // #170
    #194#171,           // #171
    #194#172,           // #172
    #194#173,           // #173
    #194#174,           // #174
    #226#128#149,       // #175
    #194#176,           // #176
    #194#177,           // #177
    #194#178,           // #178
    #194#179,           // #179
    #206#132,           // #180
    #194#181,           // #181
    #194#182,           // #182
    #194#183,           // #183
    #206#136,           // #184
    #206#137,           // #185
    #206#138,           // #186
    #194#187,           // #187
    #206#140,           // #188
    #194#189,           // #189
    #206#142,           // #190
    #206#143,           // #191
    #206#144,           // #192
    #206#145,           // #193
    #206#146,           // #194
    #206#147,           // #195
    #206#148,           // #196
    #206#149,           // #197
    #206#150,           // #198
    #206#151,           // #199
    #206#152,           // #200
    #206#153,           // #201
    #206#154,           // #202
    #206#155,           // #203
    #206#156,           // #204
    #206#157,           // #205
    #206#158,           // #206
    #206#159,           // #207
    #206#160,           // #208
    #206#161,           // #209
    #206#162,           // #210
    #206#163,           // #211
    #206#164,           // #212
    #206#165,           // #213
    #206#166,           // #214
    #206#167,           // #215
    #206#168,           // #216
    #206#169,           // #217
    #206#170,           // #218
    #206#171,           // #219
    #206#172,           // #220
    #206#173,           // #221
    #206#174,           // #222
    #206#175,           // #223
    #206#176,           // #224
    #206#177,           // #225
    #206#178,           // #226
    #206#179,           // #227
    #206#180,           // #228
    #206#181,           // #229
    #206#182,           // #230
    #206#183,           // #231
    #206#184,           // #232
    #206#185,           // #233
    #206#186,           // #234
    #206#187,           // #235
    #206#188,           // #236
    #206#189,           // #237
    #206#190,           // #238
    #206#191,           // #239
    #207#128,           // #240
    #207#129,           // #241
    #207#130,           // #242
    #207#131,           // #243
    #207#132,           // #244
    #207#133,           // #245
    #207#134,           // #246
    #207#135,           // #247
    #207#136,           // #248
    #207#137,           // #249
    #207#138,           // #250
    #207#139,           // #251
    #207#140,           // #252
    #207#141,           // #253
    #207#142,           // #254
    #207#143            // #255
  );

  ArrayCP1254ToUTF8: TCharToUTF8Table = (
    #0,                 // #0
    #1,                 // #1
    #2,                 // #2
    #3,                 // #3
    #4,                 // #4
    #5,                 // #5
    #6,                 // #6
    #7,                 // #7
    #8,                 // #8
    #9,                 // #9
    #10,                // #10
    #11,                // #11
    #12,                // #12
    #13,                // #13
    #14,                // #14
    #15,                // #15
    #16,                // #16
    #17,                // #17
    #18,                // #18
    #19,                // #19
    #20,                // #20
    #21,                // #21
    #22,                // #22
    #23,                // #23
    #24,                // #24
    #25,                // #25
    #26,                // #26
    #27,                // #27
    #28,                // #28
    #29,                // #29
    #30,                // #30
    #31,                // #31
    ' ',                // ' '
    '!',                // '!'
    '"',                // '"'
    '#',                // '#'
    '$',                // '$'
    '%',                // '%'
    '&',                // '&'
    '''',               // ''''
    '(',                // '('
    ')',                // ')'
    '*',                // '*'
    '+',                // '+'
    ',',                // ','
    '-',                // '-'
    '.',                // '.'
    '/',                // '/'
    '0',                // '0'
    '1',                // '1'
    '2',                // '2'
    '3',                // '3'
    '4',                // '4'
    '5',                // '5'
    '6',                // '6'
    '7',                // '7'
    '8',                // '8'
    '9',                // '9'
    ':',                // ':'
    ';',                // ';'
    '<',                // '<'
    '=',                // '='
    '>',                // '>'
    '?',                // '?'
    '@',                // '@'
    'A',                // 'A'
    'B',                // 'B'
    'C',                // 'C'
    'D',                // 'D'
    'E',                // 'E'
    'F',                // 'F'
    'G',                // 'G'
    'H',                // 'H'
    'I',                // 'I'
    'J',                // 'J'
    'K',                // 'K'
    'L',                // 'L'
    'M',                // 'M'
    'N',                // 'N'
    'O',                // 'O'
    'P',                // 'P'
    'Q',                // 'Q'
    'R',                // 'R'
    'S',                // 'S'
    'T',                // 'T'
    'U',                // 'U'
    'V',                // 'V'
    'W',                // 'W'
    'X',                // 'X'
    'Y',                // 'Y'
    'Z',                // 'Z'
    '[',                // '['
    '\',                // '\'
    ']',                // ']'
    '^',                // '^'
    '_',                // '_'
    '`',                // '`'
    'a',                // 'a'
    'b',                // 'b'
    'c',                // 'c'
    'd',                // 'd'
    'e',                // 'e'
    'f',                // 'f'
    'g',                // 'g'
    'h',                // 'h'
    'i',                // 'i'
    'j',                // 'j'
    'k',                // 'k'
    'l',                // 'l'
    'm',                // 'm'
    'n',                // 'n'
    'o',                // 'o'
    'p',                // 'p'
    'q',                // 'q'
    'r',                // 'r'
    's',                // 's'
    't',                // 't'
    'u',                // 'u'
    'v',                // 'v'
    'w',                // 'w'
    'x',                // 'x'
    'y',                // 'y'
    'z',                // 'z'
    '{',                // '{'
    '|',                // '|'
    '}',                // '}'
    '~',                // '~'
    #127,               // #127
    #226#130#172,       // #128
    #194#129,           // #129
    #226#128#154,       // #130
    #198#146,           // #131
    #226#128#158,       // #132
    #226#128#166,       // #133
    #226#128#160,       // #134
    #226#128#161,       // #135
    #203#134,           // #136
    #226#128#176,       // #137
    #197#160,           // #138
    #226#128#185,       // #139
    #197#146,           // #140
    #194#141,           // #141
    #194#142,           // #142
    #194#143,           // #143
    #194#144,           // #144
    #226#128#152,       // #145
    #226#128#153,       // #146
    #226#128#156,       // #147
    #226#128#157,       // #148
    #226#128#162,       // #149
    #226#128#147,       // #150
    #226#128#148,       // #151
    #203#156,           // #152
    #226#132#162,       // #153
    #197#161,           // #154
    #226#128#186,       // #155
    #197#147,           // #156
    #194#157,           // #157
    #194#158,           // #158
    #197#184,           // #159
    #194#160,           // #160
    #194#161,           // #161
    #194#162,           // #162
    #194#163,           // #163
    #194#164,           // #164
    #194#165,           // #165
    #194#166,           // #166
    #194#167,           // #167
    #194#168,           // #168
    #194#169,           // #169
    #194#170,           // #170
    #194#171,           // #171
    #194#172,           // #172
    #194#173,           // #173
    #194#174,           // #174
    #194#175,           // #175
    #194#176,           // #176
    #194#177,           // #177
    #194#178,           // #178
    #194#179,           // #179
    #194#180,           // #180
    #194#181,           // #181
    #194#182,           // #182
    #194#183,           // #183
    #194#184,           // #184
    #194#185,           // #185
    #194#186,           // #186
    #194#187,           // #187
    #194#188,           // #188
    #194#189,           // #189
    #194#190,           // #190
    #194#191,           // #191
    #195#128,           // #192
    #195#129,           // #193
    #195#130,           // #194
    #195#131,           // #195
    #195#132,           // #196
    #195#133,           // #197
    #195#134,           // #198
    #195#135,           // #199
    #195#136,           // #200
    #195#137,           // #201
    #195#138,           // #202
    #195#139,           // #203
    #195#140,           // #204
    #195#141,           // #205
    #195#142,           // #206
    #195#143,           // #207
    #196#158,           // #208
    #195#145,           // #209
    #195#146,           // #210
    #195#147,           // #211
    #195#148,           // #212
    #195#149,           // #213
    #195#150,           // #214
    #195#151,           // #215
    #195#152,           // #216
    #195#153,           // #217
    #195#154,           // #218
    #195#155,           // #219
    #195#156,           // #220
    #196#176,           // #221
    #197#158,           // #222
    #195#159,           // #223
    #195#160,           // #224
    #195#161,           // #225
    #195#162,           // #226
    #195#163,           // #227
    #195#164,           // #228
    #195#165,           // #229
    #195#166,           // #230
    #195#167,           // #231
    #195#168,           // #232
    #195#169,           // #233
    #195#170,           // #234
    #195#171,           // #235
    #195#172,           // #236
    #195#173,           // #237
    #195#174,           // #238
    #195#175,           // #239
    #196#159,           // #240
    #195#177,           // #241
    #195#178,           // #242
    #195#179,           // #243
    #195#180,           // #244
    #195#181,           // #245
    #195#182,           // #246
    #195#183,           // #247
    #195#184,           // #248
    #195#185,           // #249
    #195#186,           // #250
    #195#187,           // #251
    #195#188,           // #252
    #196#177,           // #253
    #197#159,           // #254
    #195#191            // #255
  );

  ArrayCP1255ToUTF8: TCharToUTF8Table = (
    #0,                 // #0
    #1,                 // #1
    #2,                 // #2
    #3,                 // #3
    #4,                 // #4
    #5,                 // #5
    #6,                 // #6
    #7,                 // #7
    #8,                 // #8
    #9,                 // #9
    #10,                // #10
    #11,                // #11
    #12,                // #12
    #13,                // #13
    #14,                // #14
    #15,                // #15
    #16,                // #16
    #17,                // #17
    #18,                // #18
    #19,                // #19
    #20,                // #20
    #21,                // #21
    #22,                // #22
    #23,                // #23
    #24,                // #24
    #25,                // #25
    #26,                // #26
    #27,                // #27
    #28,                // #28
    #29,                // #29
    #30,                // #30
    #31,                // #31
    ' ',                // ' '
    '!',                // '!'
    '"',                // '"'
    '#',                // '#'
    '$',                // '$'
    '%',                // '%'
    '&',                // '&'
    '''',               // ''''
    '(',                // '('
    ')',                // ')'
    '*',                // '*'
    '+',                // '+'
    ',',                // ','
    '-',                // '-'
    '.',                // '.'
    '/',                // '/'
    '0',                // '0'
    '1',                // '1'
    '2',                // '2'
    '3',                // '3'
    '4',                // '4'
    '5',                // '5'
    '6',                // '6'
    '7',                // '7'
    '8',                // '8'
    '9',                // '9'
    ':',                // ':'
    ';',                // ';'
    '<',                // '<'
    '=',                // '='
    '>',                // '>'
    '?',                // '?'
    '@',                // '@'
    'A',                // 'A'
    'B',                // 'B'
    'C',                // 'C'
    'D',                // 'D'
    'E',                // 'E'
    'F',                // 'F'
    'G',                // 'G'
    'H',                // 'H'
    'I',                // 'I'
    'J',                // 'J'
    'K',                // 'K'
    'L',                // 'L'
    'M',                // 'M'
    'N',                // 'N'
    'O',                // 'O'
    'P',                // 'P'
    'Q',                // 'Q'
    'R',                // 'R'
    'S',                // 'S'
    'T',                // 'T'
    'U',                // 'U'
    'V',                // 'V'
    'W',                // 'W'
    'X',                // 'X'
    'Y',                // 'Y'
    'Z',                // 'Z'
    '[',                // '['
    '\',                // '\'
    ']',                // ']'
    '^',                // '^'
    '_',                // '_'
    '`',                // '`'
    'a',                // 'a'
    'b',                // 'b'
    'c',                // 'c'
    'd',                // 'd'
    'e',                // 'e'
    'f',                // 'f'
    'g',                // 'g'
    'h',                // 'h'
    'i',                // 'i'
    'j',                // 'j'
    'k',                // 'k'
    'l',                // 'l'
    'm',                // 'm'
    'n',                // 'n'
    'o',                // 'o'
    'p',                // 'p'
    'q',                // 'q'
    'r',                // 'r'
    's',                // 's'
    't',                // 't'
    'u',                // 'u'
    'v',                // 'v'
    'w',                // 'w'
    'x',                // 'x'
    'y',                // 'y'
    'z',                // 'z'
    '{',                // '{'
    '|',                // '|'
    '}',                // '}'
    '~',                // '~'
    #127,               // #127
    #226#130#172,       // #128
    #194#129,           // #129
    #226#128#154,       // #130
    #198#146,           // #131
    #226#128#158,       // #132
    #226#128#166,       // #133
    #226#128#160,       // #134
    #226#128#161,       // #135
    #203#134,           // #136
    #226#128#176,       // #137
    #194#138,           // #138
    #226#128#185,       // #139
    #194#140,           // #140
    #194#141,           // #141
    #194#142,           // #142
    #194#143,           // #143
    #194#144,           // #144
    #226#128#152,       // #145
    #226#128#153,       // #146
    #226#128#156,       // #147
    #226#128#157,       // #148
    #226#128#162,       // #149
    #226#128#147,       // #150
    #226#128#148,       // #151
    #203#156,           // #152
    #226#132#162,       // #153
    #194#154,           // #154
    #226#128#186,       // #155
    #194#156,           // #156
    #194#157,           // #157
    #194#158,           // #158
    #194#159,           // #159
    #194#160,           // #160
    #194#161,           // #161
    #194#162,           // #162
    #194#163,           // #163
    #226#130#170,       // #164
    #194#165,           // #165
    #194#166,           // #166
    #194#167,           // #167
    #194#168,           // #168
    #194#169,           // #169
    #195#151,           // #170
    #194#171,           // #171
    #194#172,           // #172
    #194#173,           // #173
    #194#174,           // #174
    #194#175,           // #175
    #194#176,           // #176
    #194#177,           // #177
    #194#178,           // #178
    #194#179,           // #179
    #194#180,           // #180
    #194#181,           // #181
    #194#182,           // #182
    #194#183,           // #183
    #194#184,           // #184
    #194#185,           // #185
    #195#183,           // #186
    #194#187,           // #187
    #194#188,           // #188
    #194#189,           // #189
    #194#190,           // #190
    #194#191,           // #191
    #214#176,           // #192
    #214#177,           // #193
    #214#178,           // #194
    #214#179,           // #195
    #214#180,           // #196
    #214#181,           // #197
    #214#182,           // #198
    #214#183,           // #199
    #214#184,           // #200
    #214#185,           // #201
    #214#186,           // #202
    #214#187,           // #203
    #214#188,           // #204
    #214#189,           // #205
    #214#190,           // #206
    #214#191,           // #207
    #215#128,           // #208
    #215#129,           // #209
    #215#130,           // #210
    #215#131,           // #211
    #215#176,           // #212
    #215#177,           // #213
    #215#178,           // #214
    #215#179,           // #215
    #215#180,           // #216
    #215#181,           // #217
    #215#182,           // #218
    #215#183,           // #219
    #215#184,           // #220
    #215#185,           // #221
    #215#186,           // #222
    #215#187,           // #223
    #215#144,           // #224
    #215#145,           // #225
    #215#146,           // #226
    #215#147,           // #227
    #215#148,           // #228
    #215#149,           // #229
    #215#150,           // #230
    #215#151,           // #231
    #215#152,           // #232
    #215#153,           // #233
    #215#154,           // #234
    #215#155,           // #235
    #215#156,           // #236
    #215#157,           // #237
    #215#158,           // #238
    #215#159,           // #239
    #215#160,           // #240
    #215#161,           // #241
    #215#162,           // #242
    #215#163,           // #243
    #215#164,           // #244
    #215#165,           // #245
    #215#166,           // #246
    #215#167,           // #247
    #215#168,           // #248
    #215#169,           // #249
    #215#170,           // #250
    #215#171,           // #251
    #215#172,           // #252
    #226#128#142,       // #253
    #226#128#143,       // #254
    #215#173            // #255
  );

  ArrayCP1256ToUTF8: TCharToUTF8Table = (
    #0,                 // #0
    #1,                 // #1
    #2,                 // #2
    #3,                 // #3
    #4,                 // #4
    #5,                 // #5
    #6,                 // #6
    #7,                 // #7
    #8,                 // #8
    #9,                 // #9
    #10,                // #10
    #11,                // #11
    #12,                // #12
    #13,                // #13
    #14,                // #14
    #15,                // #15
    #16,                // #16
    #17,                // #17
    #18,                // #18
    #19,                // #19
    #20,                // #20
    #21,                // #21
    #22,                // #22
    #23,                // #23
    #24,                // #24
    #25,                // #25
    #26,                // #26
    #27,                // #27
    #28,                // #28
    #29,                // #29
    #30,                // #30
    #31,                // #31
    ' ',                // ' '
    '!',                // '!'
    '"',                // '"'
    '#',                // '#'
    '$',                // '$'
    '%',                // '%'
    '&',                // '&'
    '''',               // ''''
    '(',                // '('
    ')',                // ')'
    '*',                // '*'
    '+',                // '+'
    ',',                // ','
    '-',                // '-'
    '.',                // '.'
    '/',                // '/'
    '0',                // '0'
    '1',                // '1'
    '2',                // '2'
    '3',                // '3'
    '4',                // '4'
    '5',                // '5'
    '6',                // '6'
    '7',                // '7'
    '8',                // '8'
    '9',                // '9'
    ':',                // ':'
    ';',                // ';'
    '<',                // '<'
    '=',                // '='
    '>',                // '>'
    '?',                // '?'
    '@',                // '@'
    'A',                // 'A'
    'B',                // 'B'
    'C',                // 'C'
    'D',                // 'D'
    'E',                // 'E'
    'F',                // 'F'
    'G',                // 'G'
    'H',                // 'H'
    'I',                // 'I'
    'J',                // 'J'
    'K',                // 'K'
    'L',                // 'L'
    'M',                // 'M'
    'N',                // 'N'
    'O',                // 'O'
    'P',                // 'P'
    'Q',                // 'Q'
    'R',                // 'R'
    'S',                // 'S'
    'T',                // 'T'
    'U',                // 'U'
    'V',                // 'V'
    'W',                // 'W'
    'X',                // 'X'
    'Y',                // 'Y'
    'Z',                // 'Z'
    '[',                // '['
    '\',                // '\'
    ']',                // ']'
    '^',                // '^'
    '_',                // '_'
    '`',                // '`'
    'a',                // 'a'
    'b',                // 'b'
    'c',                // 'c'
    'd',                // 'd'
    'e',                // 'e'
    'f',                // 'f'
    'g',                // 'g'
    'h',                // 'h'
    'i',                // 'i'
    'j',                // 'j'
    'k',                // 'k'
    'l',                // 'l'
    'm',                // 'm'
    'n',                // 'n'
    'o',                // 'o'
    'p',                // 'p'
    'q',                // 'q'
    'r',                // 'r'
    's',                // 's'
    't',                // 't'
    'u',                // 'u'
    'v',                // 'v'
    'w',                // 'w'
    'x',                // 'x'
    'y',                // 'y'
    'z',                // 'z'
    '{',                // '{'
    '|',                // '|'
    '}',                // '}'
    '~',                // '~'
    #127,               // #127
    #226#130#172,       // #128
    #217#190,           // #129
    #226#128#154,       // #130
    #198#146,           // #131
    #226#128#158,       // #132
    #226#128#166,       // #133
    #226#128#160,       // #134
    #226#128#161,       // #135
    #203#134,           // #136
    #226#128#176,       // #137
    #217#185,           // #138
    #226#128#185,       // #139
    #197#146,           // #140
    #218#134,           // #141
    #218#152,           // #142
    #218#136,           // #143
    #218#175,           // #144
    #226#128#152,       // #145
    #226#128#153,       // #146
    #226#128#156,       // #147
    #226#128#157,       // #148
    #226#128#162,       // #149
    #226#128#147,       // #150
    #226#128#148,       // #151
    #218#169,           // #152
    #226#132#162,       // #153
    #218#145,           // #154
    #226#128#186,       // #155
    #197#147,           // #156
    #226#128#140,       // #157
    #226#128#141,       // #158
    #218#186,           // #159
    #194#160,           // #160
    #216#140,           // #161
    #194#162,           // #162
    #194#163,           // #163
    #194#164,           // #164
    #194#165,           // #165
    #194#166,           // #166
    #194#167,           // #167
    #194#168,           // #168
    #194#169,           // #169
    #218#190,           // #170
    #194#171,           // #171
    #194#172,           // #172
    #194#173,           // #173
    #194#174,           // #174
    #194#175,           // #175
    #194#176,           // #176
    #194#177,           // #177
    #194#178,           // #178
    #194#179,           // #179
    #194#180,           // #180
    #194#181,           // #181
    #194#182,           // #182
    #194#183,           // #183
    #194#184,           // #184
    #194#185,           // #185
    #216#155,           // #186
    #194#187,           // #187
    #194#188,           // #188
    #194#189,           // #189
    #194#190,           // #190
    #216#159,           // #191
    #219#129,           // #192
    #216#161,           // #193
    #216#162,           // #194
    #216#163,           // #195
    #216#164,           // #196
    #216#165,           // #197
    #216#166,           // #198
    #216#167,           // #199
    #216#168,           // #200
    #216#169,           // #201
    #216#170,           // #202
    #216#171,           // #203
    #216#172,           // #204
    #216#173,           // #205
    #216#174,           // #206
    #216#175,           // #207
    #216#176,           // #208
    #216#177,           // #209
    #216#178,           // #210
    #216#179,           // #211
    #216#180,           // #212
    #216#181,           // #213
    #216#182,           // #214
    #195#151,           // #215
    #216#183,           // #216
    #216#184,           // #217
    #216#185,           // #218
    #216#186,           // #219
    #217#128,           // #220
    #217#129,           // #221
    #217#130,           // #222
    #217#131,           // #223
    #195#160,           // #224
    #217#132,           // #225
    #195#162,           // #226
    #217#133,           // #227
    #217#134,           // #228
    #217#135,           // #229
    #217#136,           // #230
    #195#167,           // #231
    #195#168,           // #232
    #195#169,           // #233
    #195#170,           // #234
    #195#171,           // #235
    #217#137,           // #236
    #217#138,           // #237
    #195#174,           // #238
    #195#175,           // #239
    #217#139,           // #240
    #217#140,           // #241
    #217#141,           // #242
    #217#142,           // #243
    #195#180,           // #244
    #217#143,           // #245
    #217#144,           // #246
    #195#183,           // #247
    #217#145,           // #248
    #195#185,           // #249
    #217#146,           // #250
    #195#187,           // #251
    #195#188,           // #252
    #226#128#142,       // #253
    #226#128#143,       // #254
    #219#146            // #255
  );

  ArrayCP1257ToUTF8: TCharToUTF8Table = (
    #0,                 // #0
    #1,                 // #1
    #2,                 // #2
    #3,                 // #3
    #4,                 // #4
    #5,                 // #5
    #6,                 // #6
    #7,                 // #7
    #8,                 // #8
    #9,                 // #9
    #10,                // #10
    #11,                // #11
    #12,                // #12
    #13,                // #13
    #14,                // #14
    #15,                // #15
    #16,                // #16
    #17,                // #17
    #18,                // #18
    #19,                // #19
    #20,                // #20
    #21,                // #21
    #22,                // #22
    #23,                // #23
    #24,                // #24
    #25,                // #25
    #26,                // #26
    #27,                // #27
    #28,                // #28
    #29,                // #29
    #30,                // #30
    #31,                // #31
    ' ',                // ' '
    '!',                // '!'
    '"',                // '"'
    '#',                // '#'
    '$',                // '$'
    '%',                // '%'
    '&',                // '&'
    '''',               // ''''
    '(',                // '('
    ')',                // ')'
    '*',                // '*'
    '+',                // '+'
    ',',                // ','
    '-',                // '-'
    '.',                // '.'
    '/',                // '/'
    '0',                // '0'
    '1',                // '1'
    '2',                // '2'
    '3',                // '3'
    '4',                // '4'
    '5',                // '5'
    '6',                // '6'
    '7',                // '7'
    '8',                // '8'
    '9',                // '9'
    ':',                // ':'
    ';',                // ';'
    '<',                // '<'
    '=',                // '='
    '>',                // '>'
    '?',                // '?'
    '@',                // '@'
    'A',                // 'A'
    'B',                // 'B'
    'C',                // 'C'
    'D',                // 'D'
    'E',                // 'E'
    'F',                // 'F'
    'G',                // 'G'
    'H',                // 'H'
    'I',                // 'I'
    'J',                // 'J'
    'K',                // 'K'
    'L',                // 'L'
    'M',                // 'M'
    'N',                // 'N'
    'O',                // 'O'
    'P',                // 'P'
    'Q',                // 'Q'
    'R',                // 'R'
    'S',                // 'S'
    'T',                // 'T'
    'U',                // 'U'
    'V',                // 'V'
    'W',                // 'W'
    'X',                // 'X'
    'Y',                // 'Y'
    'Z',                // 'Z'
    '[',                // '['
    '\',                // '\'
    ']',                // ']'
    '^',                // '^'
    '_',                // '_'
    '`',                // '`'
    'a',                // 'a'
    'b',                // 'b'
    'c',                // 'c'
    'd',                // 'd'
    'e',                // 'e'
    'f',                // 'f'
    'g',                // 'g'
    'h',                // 'h'
    'i',                // 'i'
    'j',                // 'j'
    'k',                // 'k'
    'l',                // 'l'
    'm',                // 'm'
    'n',                // 'n'
    'o',                // 'o'
    'p',                // 'p'
    'q',                // 'q'
    'r',                // 'r'
    's',                // 's'
    't',                // 't'
    'u',                // 'u'
    'v',                // 'v'
    'w',                // 'w'
    'x',                // 'x'
    'y',                // 'y'
    'z',                // 'z'
    '{',                // '{'
    '|',                // '|'
    '}',                // '}'
    '~',                // '~'
    #127,               // #127
    #226#130#172,       // #128
    #194#129,           // #129
    #226#128#154,       // #130
    #194#131,           // #131
    #226#128#158,       // #132
    #226#128#166,       // #133
    #226#128#160,       // #134
    #226#128#161,       // #135
    #194#136,           // #136
    #226#128#176,       // #137
    #194#138,           // #138
    #226#128#185,       // #139
    #194#140,           // #140
    #194#168,           // #141
    #203#135,           // #142
    #194#184,           // #143
    #194#144,           // #144
    #226#128#152,       // #145
    #226#128#153,       // #146
    #226#128#156,       // #147
    #226#128#157,       // #148
    #226#128#162,       // #149
    #226#128#147,       // #150
    #226#128#148,       // #151
    #194#152,           // #152
    #226#132#162,       // #153
    #194#154,           // #154
    #226#128#186,       // #155
    #194#156,           // #156
    #194#175,           // #157
    #203#155,           // #158
    #194#159,           // #159
    #194#160,           // #160
    #194#161,           // #161
    #194#162,           // #162
    #194#163,           // #163
    #194#164,           // #164
    #194#165,           // #165
    #194#166,           // #166
    #194#167,           // #167
    #195#152,           // #168
    #194#169,           // #169
    #197#150,           // #170
    #194#171,           // #171
    #194#172,           // #172
    #194#173,           // #173
    #194#174,           // #174
    #195#134,           // #175
    #194#176,           // #176
    #194#177,           // #177
    #194#178,           // #178
    #194#179,           // #179
    #194#180,           // #180
    #194#181,           // #181
    #194#182,           // #182
    #194#183,           // #183
    #195#184,           // #184
    #194#185,           // #185
    #197#151,           // #186
    #194#187,           // #187
    #194#188,           // #188
    #194#189,           // #189
    #194#190,           // #190
    #195#166,           // #191
    #196#132,           // #192
    #196#174,           // #193
    #196#128,           // #194
    #196#134,           // #195
    #195#132,           // #196
    #195#133,           // #197
    #196#152,           // #198
    #196#146,           // #199
    #196#140,           // #200
    #195#137,           // #201
    #197#185,           // #202
    #196#150,           // #203
    #196#162,           // #204
    #196#182,           // #205
    #196#170,           // #206
    #196#187,           // #207
    #197#160,           // #208
    #197#131,           // #209
    #197#133,           // #210
    #195#147,           // #211
    #197#140,           // #212
    #195#149,           // #213
    #195#150,           // #214
    #195#151,           // #215
    #197#178,           // #216
    #197#129,           // #217
    #197#154,           // #218
    #197#170,           // #219
    #195#156,           // #220
    #197#187,           // #221
    #197#189,           // #222
    #195#159,           // #223
    #196#133,           // #224
    #196#175,           // #225
    #196#129,           // #226
    #196#135,           // #227
    #195#164,           // #228
    #195#165,           // #229
    #196#153,           // #230
    #196#147,           // #231
    #196#141,           // #232
    #195#169,           // #233
    #197#186,           // #234
    #196#151,           // #235
    #196#163,           // #236
    #196#183,           // #237
    #196#171,           // #238
    #196#188,           // #239
    #197#161,           // #240
    #197#132,           // #241
    #197#134,           // #242
    #195#179,           // #243
    #197#141,           // #244
    #195#181,           // #245
    #195#182,           // #246
    #195#183,           // #247
    #197#179,           // #248
    #197#130,           // #249
    #197#155,           // #250
    #197#171,           // #251
    #195#188,           // #252
    #197#188,           // #253
    #197#190,           // #254
    #203#153            // #255
  );

  ArrayCP1258ToUTF8: TCharToUTF8Table = (
  #0,                 // #0
  #1,                 // #1
  #2,                 // #2
  #3,                 // #3
  #4,                 // #4
  #5,                 // #5
  #6,                 // #6
  #7,                 // #7
  #8,                 // #8
  #9,                 // #9
  #10,                // #10
  #11,                // #11
  #12,                // #12
  #13,                // #13
  #14,                // #14
  #15,                // #15
  #16,                // #16
  #17,                // #17
  #18,                // #18
  #19,                // #19
  #20,                // #20
  #21,                // #21
  #22,                // #22
  #23,                // #23
  #24,                // #24
  #25,                // #25
  #26,                // #26
  #27,                // #27
  #28,                // #28
  #29,                // #29
  #30,                // #30
  #31,                // #31
  ' ',                // ' '
  '!',                // '!'
  '"',                // '"'
  '#',                // '#'
  '$',                // '$'
  '%',                // '%'
  '&',                // '&'
  '''',               // ''''
  '(',                // '('
  ')',                // ')'
  '*',                // '*'
  '+',                // '+'
  ',',                // ','
  '-',                // '-'
  '.',                // '.'
  '/',                // '/'
  '0',                // '0'
  '1',                // '1'
  '2',                // '2'
  '3',                // '3'
  '4',                // '4'
  '5',                // '5'
  '6',                // '6'
  '7',                // '7'
  '8',                // '8'
  '9',                // '9'
  ':',                // ':'
  ';',                // ';'
  '<',                // '<'
  '=',                // '='
  '>',                // '>'
  '?',                // '?'
  '@',                // '@'
  'A',                // 'A'
  'B',                // 'B'
  'C',                // 'C'
  'D',                // 'D'
  'E',                // 'E'
  'F',                // 'F'
  'G',                // 'G'
  'H',                // 'H'
  'I',                // 'I'
  'J',                // 'J'
  'K',                // 'K'
  'L',                // 'L'
  'M',                // 'M'
  'N',                // 'N'
  'O',                // 'O'
  'P',                // 'P'
  'Q',                // 'Q'
  'R',                // 'R'
  'S',                // 'S'
  'T',                // 'T'
  'U',                // 'U'
  'V',                // 'V'
  'W',                // 'W'
  'X',                // 'X'
  'Y',                // 'Y'
  'Z',                // 'Z'
  '[',                // '['
  '\',                // '\'
  ']',                // ']'
  '^',                // '^'
  '_',                // '_'
  '`',                // '`'
  'a',                // 'a'
  'b',                // 'b'
  'c',                // 'c'
  'd',                // 'd'
  'e',                // 'e'
  'f',                // 'f'
  'g',                // 'g'
  'h',                // 'h'
  'i',                // 'i'
  'j',                // 'j'
  'k',                // 'k'
  'l',                // 'l'
  'm',                // 'm'
  'n',                // 'n'
  'o',                // 'o'
  'p',                // 'p'
  'q',                // 'q'
  'r',                // 'r'
  's',                // 's'
  't',                // 't'
  'u',                // 'u'
  'v',                // 'v'
  'w',                // 'w'
  'x',                // 'x'
  'y',                // 'y'
  'z',                // 'z'
  '{',                // '{'
  '|',                // '|'
  '}',                // '}'
  '~',                // '~'
  #127,               // #127
  #226#130#172,       // #128
  #194#129,           // #129
  #226#128#154,       // #130
  #198#146,           // #131
  #226#128#158,       // #132
  #226#128#166,       // #133
  #226#128#160,       // #134
  #226#128#161,       // #135
  #203#134,           // #136
  #226#128#176,       // #137
  #194#138,           // #138
  #226#128#185,       // #139
  #197#146,           // #140
  #194#141,           // #141
  #194#142,           // #142
  #194#143,           // #143
  #194#144,           // #144
  #226#128#152,       // #145
  #226#128#153,       // #146
  #226#128#156,       // #147
  #226#128#157,       // #148
  #226#128#162,       // #149
  #226#128#147,       // #150
  #226#128#148,       // #151
  #203#156,           // #152
  #226#132#162,       // #153
  #194#154,           // #154
  #226#128#186,       // #155
  #197#147,           // #156
  #194#157,           // #157
  #194#158,           // #158
  #197#184,           // #159
  #194#160,           // #160
  #194#161,           // #161
  #194#162,           // #162
  #194#163,           // #163
  #194#164,           // #164
  #194#165,           // #165
  #194#166,           // #166
  #194#167,           // #167
  #194#168,           // #168
  #194#169,           // #169
  #194#170,           // #170
  #194#171,           // #171
  #194#172,           // #172
  #194#173,           // #173
  #194#174,           // #174
  #194#175,           // #175
  #194#176,           // #176
  #194#177,           // #177
  #194#178,           // #178
  #194#179,           // #179
  #194#180,           // #180
  #194#181,           // #181
  #194#182,           // #182
  #194#183,           // #183
  #194#184,           // #184
  #194#185,           // #185
  #194#186,           // #186
  #194#187,           // #187
  #194#188,           // #188
  #194#189,           // #189
  #194#190,           // #190
  #194#191,           // #191
  #195#128,           // #192
  #195#129,           // #193
  #195#130,           // #194
  #196#130,           // #195
  #195#132,           // #196
  #195#133,           // #197
  #195#134,           // #198
  #195#135,           // #199
  #195#136,           // #200
  #195#137,           // #201
  #195#138,           // #202
  #195#139,           // #203
  #204#128,           // #204
  #195#141,           // #205
  #195#142,           // #206
  #195#143,           // #207
  #196#144,           // #208
  #195#145,           // #209
  #204#137,           // #210
  #195#147,           // #211
  #195#148,           // #212
  #198#160,           // #213
  #195#150,           // #214
  #195#151,           // #215
  #195#152,           // #216
  #195#153,           // #217
  #195#154,           // #218
  #195#155,           // #219
  #195#156,           // #220
  #198#175,           // #221
  #204#131,           // #222
  #195#159,           // #223
  #195#160,           // #224
  #195#161,           // #225
  #195#162,           // #226
  #196#131,           // #227
  #195#164,           // #228
  #195#165,           // #229
  #195#166,           // #230
  #195#167,           // #231
  #195#168,           // #232
  #195#169,           // #233
  #195#170,           // #234
  #195#171,           // #235
  #204#129,           // #236
  #195#173,           // #237
  #195#174,           // #238
  #195#175,           // #239
  #196#145,           // #240
  #195#177,           // #241
  #204#163,           // #242
  #195#179,           // #243
  #195#180,           // #244
  #198#161,           // #245
  #195#182,           // #246
  #195#183,           // #247
  #195#184,           // #248
  #195#185,           // #249
  #195#186,           // #250
  #195#187,           // #251
  #195#188,           // #252
  #198#176,           // #253
  #226#130#171,       // #254
  #195#191            // #255
  );

  ArrayCP437ToUTF8 : TCharToUTF8Table = (
    #0,                 // #0
    #1,                 // #1
    #2,                 // #2
    #3,                 // #3
    #4,                 // #4
    #5,                 // #5
    #6,                 // #6
    #7,                 // #7
    #8,                 // #8
    #9,                 // #9
    #10,                // #10
    #11,                // #11
    #12,                // #12
    #13,                // #13
    #14,                // #14
    #15,                // #15
    #16,                // #16
    #17,                // #17
    #18,                // #18
    #19,                // #19
    #20,                // #20
    #21,                // #21
    #22,                // #22
    #23,                // #23
    #24,                // #24
    #25,                // #25
    #26,                // #26
    #27,                // #27
    #28,                // #28
    #29,                // #29
    #30,                // #30
    #31,                // #31
    ' ',                // ' '
    '!',                // '!'
    '"',                // '"'
    '#',                // '#'
    '$',                // '$'
    '%',                // '%'
    '&',                // '&'
    '''',               // ''''
    '(',                // '('
    ')',                // ')'
    '*',                // '*'
    '+',                // '+'
    ',',                // ','
    '-',                // '-'
    '.',                // '.'
    '/',                // '/'
    '0',                // '0'
    '1',                // '1'
    '2',                // '2'
    '3',                // '3'
    '4',                // '4'
    '5',                // '5'
    '6',                // '6'
    '7',                // '7'
    '8',                // '8'
    '9',                // '9'
    ':',                // ':'
    ';',                // ';'
    '<',                // '<'
    '=',                // '='
    '>',                // '>'
    '?',                // '?'
    '@',                // '@'
    'A',                // 'A'
    'B',                // 'B'
    'C',                // 'C'
    'D',                // 'D'
    'E',                // 'E'
    'F',                // 'F'
    'G',                // 'G'
    'H',                // 'H'
    'I',                // 'I'
    'J',                // 'J'
    'K',                // 'K'
    'L',                // 'L'
    'M',                // 'M'
    'N',                // 'N'
    'O',                // 'O'
    'P',                // 'P'
    'Q',                // 'Q'
    'R',                // 'R'
    'S',                // 'S'
    'T',                // 'T'
    'U',                // 'U'
    'V',                // 'V'
    'W',                // 'W'
    'X',                // 'X'
    'Y',                // 'Y'
    'Z',                // 'Z'
    '[',                // '['
    '\',                // '\'
    ']',                // ']'
    '^',                // '^'
    '_',                // '_'
    '`',                // '`'
    'a',                // 'a'
    'b',                // 'b'
    'c',                // 'c'
    'd',                // 'd'
    'e',                // 'e'
    'f',                // 'f'
    'g',                // 'g'
    'h',                // 'h'
    'i',                // 'i'
    'j',                // 'j'
    'k',                // 'k'
    'l',                // 'l'
    'm',                // 'm'
    'n',                // 'n'
    'o',                // 'o'
    'p',                // 'p'
    'q',                // 'q'
    'r',                // 'r'
    's',                // 's'
    't',                // 't'
    'u',                // 'u'
    'v',                // 'v'
    'w',                // 'w'
    'x',                // 'x'
    'y',                // 'y'
    'z',                // 'z'
    '{',                // '{'
    '|',                // '|'
    '}',                // '}'
    '~',                // '~'
    #127,               // #127
    #195#135,           // #128
    #195#188,           // #129
    #195#169,           // #130
    #195#162,           // #131
    #195#164,           // #132
    #195#160,           // #133
    #195#165,           // #134
    #195#167,           // #135
    #195#170,           // #136
    #195#171,           // #137
    #195#168,           // #138
    #195#175,           // #139
    #195#174,           // #140
    #195#172,           // #141
    #195#132,           // #142
    #195#133,           // #143
    #195#137,           // #144
    #195#166,           // #145
    #195#134,           // #146
    #195#180,           // #147
    #195#182,           // #148
    #195#178,           // #149
    #195#187,           // #150
    #195#185,           // #151
    #195#191,           // #152
    #195#150,           // #153
    #195#156,           // #154
    #194#162,           // #155
    #194#163,           // #156
    #194#165,           // #157
    #226#130#167,       // #158
    #198#146,           // #159
    #195#161,           // #160
    #195#173,           // #161
    #195#179,           // #162
    #195#186,           // #163
    #195#177,           // #164
    #195#145,           // #165
    #194#170,           // #166
    #194#186,           // #167
    #194#191,           // #168
    #226#140#144,       // #169
    #194#172,           // #170
    #194#189,           // #171
    #194#188,           // #172
    #194#161,           // #173
    #194#171,           // #174
    #194#187,           // #175
    #226#150#145,       // #176
    #226#150#146,       // #177
    #226#150#147,       // #178
    #226#148#130,       // #179
    #226#148#164,       // #180
    #226#149#161,       // #181
    #226#149#162,       // #182
    #226#149#150,       // #183
    #226#149#149,       // #184
    #226#149#163,       // #185
    #226#149#145,       // #186
    #226#149#151,       // #187
    #226#149#157,       // #188
    #226#149#156,       // #189
    #226#149#155,       // #190
    #226#148#144,       // #191
    #226#148#148,       // #192
    #226#148#180,       // #193
    #226#148#172,       // #194
    #226#148#156,       // #195
    #226#148#128,       // #196
    #226#148#188,       // #197
    #226#149#158,       // #198
    #226#149#159,       // #199
    #226#149#154,       // #200
    #226#149#148,       // #201
    #226#149#169,       // #202
    #226#149#166,       // #203
    #226#149#160,       // #204
    #226#149#144,       // #205
    #226#149#172,       // #206
    #226#149#167,       // #207
    #226#149#168,       // #208
    #226#149#164,       // #209
    #226#149#165,       // #210
    #226#149#153,       // #211
    #226#149#152,       // #212
    #226#149#146,       // #213
    #226#149#147,       // #214
    #226#149#171,       // #215
    #226#149#170,       // #216
    #226#148#152,       // #217
    #226#148#140,       // #218
    #226#150#136,       // #219
    #226#150#132,       // #220
    #226#150#140,       // #221
    #226#150#144,       // #222
    #226#150#128,       // #223
    #206#177,           // #224
    #195#159,           // #225
    #206#147,           // #226
    #207#128,           // #227
    #206#163,           // #228
    #207#131,           // #229
    #194#181,           // #230
    #207#132,           // #231
    #206#166,           // #232
    #206#152,           // #233
    #206#169,           // #234
    #206#180,           // #235
    #226#136#158,       // #236
    #207#134,           // #237
    #206#181,           // #238
    #226#136#169,       // #239
    #226#137#161,       // #240
    #194#177,           // #241
    #226#137#165,       // #242
    #226#137#164,       // #243
    #226#140#160,       // #244
    #226#140#161,       // #245
    #195#183,           // #246
    #226#137#136,       // #247
    #194#176,           // #248
    #226#136#153,       // #249
    #194#183,           // #250
    #226#136#154,       // #251
    #226#129#191,       // #252
    #194#178,           // #253
    #226#150#160,       // #254
    #194#160            // #255
  );

  ArrayCP850ToUTF8 : TCharToUTF8Table = (
    #0,                 // #0
    #1,                 // #1
    #2,                 // #2
    #3,                 // #3
    #4,                 // #4
    #5,                 // #5
    #6,                 // #6
    #7,                 // #7
    #8,                 // #8
    #9,                 // #9
    #10,                // #10
    #11,                // #11
    #12,                // #12
    #13,                // #13
    #14,                // #14
    #15,                // #15
    #16,                // #16
    #17,                // #17
    #18,                // #18
    #19,                // #19
    #20,                // #20
    #21,                // #21
    #22,                // #22
    #23,                // #23
    #24,                // #24
    #25,                // #25
    #26,                // #26
    #27,                // #27
    #28,                // #28
    #29,                // #29
    #30,                // #30
    #31,                // #31
    ' ',                // ' '
    '!',                // '!'
    '"',                // '"'
    '#',                // '#'
    '$',                // '$'
    '%',                // '%'
    '&',                // '&'
    '''',               // ''''
    '(',                // '('
    ')',                // ')'
    '*',                // '*'
    '+',                // '+'
    ',',                // ','
    '-',                // '-'
    '.',                // '.'
    '/',                // '/'
    '0',                // '0'
    '1',                // '1'
    '2',                // '2'
    '3',                // '3'
    '4',                // '4'
    '5',                // '5'
    '6',                // '6'
    '7',                // '7'
    '8',                // '8'
    '9',                // '9'
    ':',                // ':'
    ';',                // ';'
    '<',                // '<'
    '=',                // '='
    '>',                // '>'
    '?',                // '?'
    '@',                // '@'
    'A',                // 'A'
    'B',                // 'B'
    'C',                // 'C'
    'D',                // 'D'
    'E',                // 'E'
    'F',                // 'F'
    'G',                // 'G'
    'H',                // 'H'
    'I',                // 'I'
    'J',                // 'J'
    'K',                // 'K'
    'L',                // 'L'
    'M',                // 'M'
    'N',                // 'N'
    'O',                // 'O'
    'P',                // 'P'
    'Q',                // 'Q'
    'R',                // 'R'
    'S',                // 'S'
    'T',                // 'T'
    'U',                // 'U'
    'V',                // 'V'
    'W',                // 'W'
    'X',                // 'X'
    'Y',                // 'Y'
    'Z',                // 'Z'
    '[',                // '['
    '\',                // '\'
    ']',                // ']'
    '^',                // '^'
    '_',                // '_'
    '`',                // '`'
    'a',                // 'a'
    'b',                // 'b'
    'c',                // 'c'
    'd',                // 'd'
    'e',                // 'e'
    'f',                // 'f'
    'g',                // 'g'
    'h',                // 'h'
    'i',                // 'i'
    'j',                // 'j'
    'k',                // 'k'
    'l',                // 'l'
    'm',                // 'm'
    'n',                // 'n'
    'o',                // 'o'
    'p',                // 'p'
    'q',                // 'q'
    'r',                // 'r'
    's',                // 's'
    't',                // 't'
    'u',                // 'u'
    'v',                // 'v'
    'w',                // 'w'
    'x',                // 'x'
    'y',                // 'y'
    'z',                // 'z'
    '{',                // '{'
    '|',                // '|'
    '}',                // '}'
    '~',                // '~'
    #127,               // #127
    #195#135,           // #128
    #195#188,           // #129
    #195#169,           // #130
    #195#162,           // #131
    #195#164,           // #132
    #195#160,           // #133
    #195#165,           // #134
    #195#167,           // #135
    #195#170,           // #136
    #195#171,           // #137
    #195#168,           // #138
    #195#175,           // #139
    #195#174,           // #140
    #195#172,           // #141
    #195#132,           // #142
    #195#133,           // #143
    #195#137,           // #144
    #195#166,           // #145
    #195#134,           // #146
    #195#180,           // #147
    #195#182,           // #148
    #195#178,           // #149
    #195#187,           // #150
    #195#185,           // #151
    #195#191,           // #152
    #195#150,           // #153
    #195#156,           // #154
    #195#184,           // #155
    #194#163,           // #156
    #195#152,           // #157
    #195#151,           // #158
    #198#146,           // #159
    #195#161,           // #160
    #195#173,           // #161
    #195#179,           // #162
    #195#186,           // #163
    #195#177,           // #164
    #195#145,           // #165
    #194#170,           // #166
    #194#186,           // #167
    #194#191,           // #168
    #194#174,           // #169
    #194#172,           // #170
    #194#189,           // #171
    #194#188,           // #172
    #194#161,           // #173
    #194#171,           // #174
    #194#187,           // #175
    #226#150#145,       // #176
    #226#150#146,       // #177
    #226#150#147,       // #178
    #226#148#130,       // #179
    #226#148#164,       // #180
    #195#129,           // #181
    #195#130,           // #182
    #195#128,           // #183
    #194#169,           // #184
    #226#149#163,       // #185
    #226#149#145,       // #186
    #226#149#151,       // #187
    #226#149#157,       // #188
    #194#162,           // #189
    #194#165,           // #190
    #226#148#144,       // #191
    #226#148#148,       // #192
    #226#148#180,       // #193
    #226#148#172,       // #194
    #226#148#156,       // #195
    #226#148#128,       // #196
    #226#148#188,       // #197
    #195#163,           // #198
    #195#131,           // #199
    #226#149#154,       // #200
    #226#149#148,       // #201
    #226#149#169,       // #202
    #226#149#166,       // #203
    #226#149#160,       // #204
    #226#149#144,       // #205
    #226#149#172,       // #206
    #194#164,           // #207
    #195#176,           // #208
    #195#144,           // #209
    #195#138,           // #210
    #195#139,           // #211
    #195#136,           // #212
    #196#177,           // #213
    #195#141,           // #214
    #195#142,           // #215
    #195#143,           // #216
    #226#148#152,       // #217
    #226#148#140,       // #218
    #226#150#136,       // #219
    #226#150#132,       // #220
    #194#166,           // #221
    #195#140,           // #222
    #226#150#128,       // #223
    #195#147,           // #224
    #195#159,           // #225
    #195#148,           // #226
    #195#146,           // #227
    #195#181,           // #228
    #195#149,           // #229
    #194#181,           // #230
    #195#190,           // #231
    #195#158,           // #232
    #195#154,           // #233
    #195#155,           // #234
    #195#153,           // #235
    #195#189,           // #236
    #195#157,           // #237
    #194#175,           // #238
    #194#180,           // #239
    #194#173,           // #240
    #194#177,           // #241
    #226#128#151,       // #242
    #194#190,           // #243
    #194#182,           // #244
    #194#167,           // #245
    #195#183,           // #246
    #194#184,           // #247
    #194#176,           // #248
    #194#168,           // #249
    #194#183,           // #250
    #194#185,           // #251
    #194#179,           // #252
    #194#178,           // #253
    #226#150#160,       // #254
    #194#160            // #255
  );

  // ftp://ftp.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP852.TXT
  ArrayCP852ToUTF8 : TCharToUTF8Table = (
    #0,                 // #0
    #1,                 // #1
    #2,                 // #2
    #3,                 // #3
    #4,                 // #4
    #5,                 // #5
    #6,                 // #6
    #7,                 // #7
    #8,                 // #8
    #9,                 // #9
    #10,                // #10
    #11,                // #11
    #12,                // #12
    #13,                // #13
    #14,                // #14
    #15,                // #15
    #16,                // #16
    #17,                // #17
    #18,                // #18
    #19,                // #19
    #20,                // #20
    #21,                // #21
    #22,                // #22
    #23,                // #23
    #24,                // #24
    #25,                // #25
    #26,                // #26
    #27,                // #27
    #28,                // #28
    #29,                // #29
    #30,                // #30
    #31,                // #31
    ' ',                // ' '
    '!',                // '!'
    '"',                // '"'
    '#',                // '#'
    '$',                // '$'
    '%',                // '%'
    '&',                // '&'
    '''',               // ''''
    '(',                // '('
    ')',                // ')'
    '*',                // '*'
    '+',                // '+'
    ',',                // ','
    '-',                // '-'
    '.',                // '.'
    '/',                // '/'
    '0',                // '0'
    '1',                // '1'
    '2',                // '2'
    '3',                // '3'
    '4',                // '4'
    '5',                // '5'
    '6',                // '6'
    '7',                // '7'
    '8',                // '8'
    '9',                // '9'
    ':',                // ':'
    ';',                // ';'
    '<',                // '<'
    '=',                // '='
    '>',                // '>'
    '?',                // '?'
    '@',                // '@'
    'A',                // 'A'
    'B',                // 'B'
    'C',                // 'C'
    'D',                // 'D'
    'E',                // 'E'
    'F',                // 'F'
    'G',                // 'G'
    'H',                // 'H'
    'I',                // 'I'
    'J',                // 'J'
    'K',                // 'K'
    'L',                // 'L'
    'M',                // 'M'
    'N',                // 'N'
    'O',                // 'O'
    'P',                // 'P'
    'Q',                // 'Q'
    'R',                // 'R'
    'S',                // 'S'
    'T',                // 'T'
    'U',                // 'U'
    'V',                // 'V'
    'W',                // 'W'
    'X',                // 'X'
    'Y',                // 'Y'
    'Z',                // 'Z'
    '[',                // '['
    '\',                // '\'
    ']',                // ']'
    '^',                // '^'
    '_',                // '_'
    '`',                // '`'
    'a',                // 'a'
    'b',                // 'b'
    'c',                // 'c'
    'd',                // 'd'
    'e',                // 'e'
    'f',                // 'f'
    'g',                // 'g'
    'h',                // 'h'
    'i',                // 'i'
    'j',                // 'j'
    'k',                // 'k'
    'l',                // 'l'
    'm',                // 'm'
    'n',                // 'n'
    'o',                // 'o'
    'p',                // 'p'
    'q',                // 'q'
    'r',                // 'r'
    's',                // 's'
    't',                // 't'
    'u',                // 'u'
    'v',                // 'v'
    'w',                // 'w'
    'x',                // 'x'
    'y',                // 'y'
    'z',                // 'z'
    '{',                // '{'
    '|',                // '|'
    '}',                // '}'
    '~',                // '~'
    #127,               // #127
    #195#135,           // #128
    #195#188,           // #129
    #195#169,           // #130
    #195#162,           // #131
    #195#164,           // #132
    #197#175,           // #133
    #196#135,           // #134
    #195#167,           // #135
    #197#130,           // #136
    #195#171,           // #137
    #197#144,           // #138
    #197#145,           // #139
    #195#174,           // #140
    #197#185,           // #141
    #195#132,           // #142
    #196#134,           // #143
    #195#137,           // #144
    #196#185,           // #145
    #196#186,           // #146
    #195#180,           // #147
    #195#182,           // #148
    #196#189,           // #149
    #196#190,           // #150
    #197#154,           // #151
    #197#155,           // #152
    #195#150,           // #153
    #195#156,           // #154
    #197#164,           // #155
    #197#165,           // #156
    #197#129,           // #157
    #195#151,           // #158
    #196#141,           // #159
    #195#161,           // #160
    #195#173,           // #161
    #195#179,           // #162
    #195#186,           // #163
    #196#132,           // #164
    #196#133,           // #165
    #197#189,           // #166
    #197#190,           // #167
    #196#152,           // #168
    #196#153,           // #169
    #194#172,           // #170
    #197#186,           // #171
    #196#140,           // #172
    #197#159,           // #173
    #194#171,           // #174
    #194#187,           // #175
    #226#150#145,       // #176
    #226#150#146,       // #177
    #226#150#147,       // #178
    #226#148#130,       // #179
    #226#148#164,       // #180
    #195#129,           // #181
    #195#130,           // #182
    #196#154,           // #183
    #197#158,           // #184
    #226#149#163,       // #185
    #226#149#145,       // #186
    #226#149#151,       // #187
    #226#149#157,       // #188
    #197#187,           // #189
    #197#188,           // #190
    #226#148#144,       // #191
    #226#148#148,       // #192
    #226#148#180,       // #193
    #226#148#172,       // #194
    #226#148#156,       // #195
    #226#148#128,       // #196
    #226#148#188,       // #197
    #196#130,           // #198
    #196#131,           // #199
    #226#149#154,       // #200
    #226#149#148,       // #201
    #226#149#169,       // #202
    #226#149#166,       // #203
    #226#149#160,       // #204
    #226#149#144,       // #205
    #226#149#172,       // #206
    #194#164,           // #207
    #196#145,           // #208
    #196#144,           // #209
    #196#142,           // #210
    #195#139,           // #211
    #196#143,           // #212
    #197#135,           // #213
    #195#141,           // #214
    #195#142,           // #215
    #196#155,           // #216
    #226#148#152,       // #217
    #226#148#140,       // #218
    #226#150#136,       // #219
    #226#150#132,       // #220
    #197#162,           // #221
    #197#174,           // #222
    #226#150#128,       // #223
    #195#147,           // #224
    #195#159,           // #225
    #195#148,           // #226
    #197#131,           // #227
    #197#132,           // #228
    #197#136,           // #229
    #197#160,           // #230
    #197#161,           // #231
    #197#148,           // #232
    #195#154,           // #233
    #197#149,           // #234
    #197#176,           // #235
    #195#189,           // #236
    #195#157,           // #237
    #197#163,           // #238
    #194#180,           // #239
    #194#173,           // #240
    #203#157,           // #241
    #203#155,           // #242
    #203#135,           // #243
    #203#152,           // #244
    #194#167,           // #245
    #195#183,           // #246
    #194#184,           // #247
    #194#176,           // #248
    #194#168,           // #249
    #203#153,           // #250
    #197#177,           // #251
    #197#152,           // #252
    #197#153,           // #253
    #226#150#160,       // #254
    #194#160            // #255
  );

  ArrayCP866ToUTF8 : TCharToUTF8Table = (
    #0,                 //#0
    #1,                 //#1
    #2,                 //#2
    #3,                 //#3
    #4,                 //#4
    #5,                 //#5
    #6,                 //#6
    #7,                 //#7
    #8,                 //#8
    #9,                 //#9
    #10,                //#10
    #11,                //#11
    #12,                //#12
    #13,                //#13
    #14,                //#14
    #15,                //#15
    #16,                //#16
    #17,                //#17
    #18,                //#18
    #19,                //#19
    #20,                //#20
    #21,                //#21
    #22,                //#22
    #23,                //#23
    #24,                //#24
    #25,                //#25
    #26,                //#26
    #27,                //#27
    #28,                //#28
    #29,                //#29
    #30,                //#30
    #31,                //#31
    #32,                //#32
    #33,                //#33
    #34,                //#34
    #35,                //#35
    #36,                //#36
    #37,                //#37
    #38,                //#38
    #39,                //#39
    #40,                //#40
    #41,                //#41
    #42,                //#42
    #43,                //#43
    #44,                //#44
    #45,                //#45
    #46,                //#46
    #47,                //#47
    #48,                //#48
    #49,                //#49
    #50,                //#50
    #51,                //#51
    #52,                //#52
    #53,                //#53
    #54,                //#54
    #55,                //#55
    #56,                //#56
    #57,                //#57
    #58,                //#58
    #59,                //#59
    #60,                //#60
    #61,                //#61
    #62,                //#62
    #63,                //#63
    #64,                //#64
    #65,                //#65
    #66,                //#66
    #67,                //#67
    #68,                //#68
    #69,                //#69
    #70,                //#70
    #71,                //#71
    #72,                //#72
    #73,                //#73
    #74,                //#74
    #75,                //#75
    #76,                //#76
    #77,                //#77
    #78,                //#78
    #79,                //#79
    #80,                //#80
    #81,                //#81
    #82,                //#82
    #83,                //#83
    #84,                //#84
    #85,                //#85
    #86,                //#86
    #87,                //#87
    #88,                //#88
    #89,                //#89
    #90,                //#90
    #91,                //#91
    #92,                //#92
    #93,                //#93
    #94,                //#94
    #95,                //#95
    #96,                //#96
    #97,                //#97
    #98,                //#98
    #99,                //#99
    #100,               //#100
    #101,               //#101
    #102,               //#102
    #103,               //#103
    #104,               //#104
    #105,               //#105
    #106,               //#106
    #107,               //#107
    #108,               //#108
    #109,               //#109
    #110,               //#110
    #111,               //#111
    #112,               //#112
    #113,               //#113
    #114,               //#114
    #115,               //#115
    #116,               //#116
    #117,               //#117
    #118,               //#118
    #119,               //#119
    #120,               //#120
    #121,               //#121
    #122,               //#122
    #123,               //#123
    #124,               //#124
    #125,               //#125
    #126,               //#126
    #127,               //#127
    #208#144,           //#128
    #208#145,           //#129
    #208#146,           //#130
    #208#147,           //#131
    #208#148,           //#132
    #208#149,           //#133
    #208#150,           //#134
    #208#151,           //#135
    #208#152,           //#136
    #208#153,           //#137
    #208#154,           //#138
    #208#155,           //#139
    #208#156,           //#140
    #208#157,           //#141
    #208#158,           //#142
    #208#159,           //#143
    #208#160,           //#144
    #208#161,           //#145
    #208#162,           //#146
    #208#163,           //#147
    #208#164,           //#148
    #208#165,           //#149
    #208#166,           //#150
    #208#167,           //#151
    #208#168,           //#152
    #208#169,           //#153
    #208#170,           //#154
    #208#171,           //#155
    #208#172,           //#156
    #208#173,           //#157
    #208#174,           //#158
    #208#175,           //#159
    #208#176,           //#160
    #208#177,           //#161
    #208#178,           //#162
    #208#179,           //#163
    #208#180,           //#164
    #208#181,           //#165
    #208#182,           //#166
    #208#183,           //#167
    #208#184,           //#168
    #208#185,           //#169
    #208#186,           //#170
    #208#187,           //#171
    #208#188,           //#172
    #208#189,           //#173
    #208#190,           //#174
    #208#191,           //#175
    #226#150#145,       //#176
    #226#150#146,       //#177
    #226#150#147,       //#178
    #226#148#130,       //#179
    #226#148#164,       //#180
    #226#149#161,       //#181
    #226#149#162,       //#182
    #226#149#150,       //#183
    #226#149#149,       //#184
    #226#149#163,       //#185
    #226#149#145,       //#186
    #226#149#151,       //#187
    #226#149#157,       //#188
    #226#149#156,       //#189
    #226#149#155,       //#190
    #226#148#144,       //#191
    #226#148#148,       //#192
    #226#148#180,       //#193
    #226#148#172,       //#194
    #226#148#156,       //#195
    #226#148#128,       //#196
    #226#148#188,       //#197
    #226#149#158,       //#198
    #226#149#159,       //#199
    #226#149#154,       //#200
    #226#149#148,       //#201
    #226#149#169,       //#202
    #226#149#166,       //#203
    #226#149#160,       //#204
    #226#149#144,       //#205
    #226#149#172,       //#206
    #226#149#167,       //#207
    #226#149#168,       //#208
    #226#149#164,       //#209
    #226#149#165,       //#210
    #226#149#153,       //#211
    #226#149#152,       //#212
    #226#149#146,       //#213
    #226#149#147,       //#214
    #226#149#171,       //#215
    #226#149#170,       //#216
    #226#148#152,       //#217
    #226#148#140,       //#218
    #226#150#136,       //#219
    #226#150#132,       //#220
    #226#150#140,       //#221
    #226#150#144,       //#222
    #226#150#128,       //#223
    #209#128,           //#224
    #209#129,           //#225
    #209#130,           //#226
    #209#131,           //#227
    #209#132,           //#228
    #209#133,           //#229
    #209#134,           //#230
    #209#135,           //#231
    #209#136,           //#232
    #209#137,           //#233
    #209#138,           //#234
    #209#139,           //#235
    #209#140,           //#236
    #209#141,           //#237
    #209#142,           //#238
    #209#143,           //#239
    #208#129,           //#240
    #209#145,           //#241
    #208#132,           //#242
    #209#148,           //#243
    #208#135,           //#244
    #209#151,           //#245
    #208#142,           //#246
    #209#158,           //#247
    #194#176,           //#248
    #226#136#153,       //#249
    #194#183,           //#250
    #226#136#154,       //#251
    #226#132#150,       //#252
    #194#164,           //#253
    #226#150#160,       //#254
    #194#160            //#255
  );

  ArrayCP874ToUTF8: TCharToUTF8Table = (
    #0,                 // #0
    #1,                 // #1
    #2,                 // #2
    #3,                 // #3
    #4,                 // #4
    #5,                 // #5
    #6,                 // #6
    #7,                 // #7
    #8,                 // #8
    #9,                 // #9
    #10,                // #10
    #11,                // #11
    #12,                // #12
    #13,                // #13
    #14,                // #14
    #15,                // #15
    #16,                // #16
    #17,                // #17
    #18,                // #18
    #19,                // #19
    #20,                // #20
    #21,                // #21
    #22,                // #22
    #23,                // #23
    #24,                // #24
    #25,                // #25
    #26,                // #26
    #27,                // #27
    #28,                // #28
    #29,                // #29
    #30,                // #30
    #31,                // #31
    ' ',                // ' '
    '!',                // '!'
    '"',                // '"'
    '#',                // '#'
    '$',                // '$'
    '%',                // '%'
    '&',                // '&'
    '''',               // ''''
    '(',                // '('
    ')',                // ')'
    '*',                // '*'
    '+',                // '+'
    ',',                // ','
    '-',                // '-'
    '.',                // '.'
    '/',                // '/'
    '0',                // '0'
    '1',                // '1'
    '2',                // '2'
    '3',                // '3'
    '4',                // '4'
    '5',                // '5'
    '6',                // '6'
    '7',                // '7'
    '8',                // '8'
    '9',                // '9'
    ':',                // ':'
    ';',                // ';'
    '<',                // '<'
    '=',                // '='
    '>',                // '>'
    '?',                // '?'
    '@',                // '@'
    'A',                // 'A'
    'B',                // 'B'
    'C',                // 'C'
    'D',                // 'D'
    'E',                // 'E'
    'F',                // 'F'
    'G',                // 'G'
    'H',                // 'H'
    'I',                // 'I'
    'J',                // 'J'
    'K',                // 'K'
    'L',                // 'L'
    'M',                // 'M'
    'N',                // 'N'
    'O',                // 'O'
    'P',                // 'P'
    'Q',                // 'Q'
    'R',                // 'R'
    'S',                // 'S'
    'T',                // 'T'
    'U',                // 'U'
    'V',                // 'V'
    'W',                // 'W'
    'X',                // 'X'
    'Y',                // 'Y'
    'Z',                // 'Z'
    '[',                // '['
    '\',                // '\'
    ']',                // ']'
    '^',                // '^'
    '_',                // '_'
    '`',                // '`'
    'a',                // 'a'
    'b',                // 'b'
    'c',                // 'c'
    'd',                // 'd'
    'e',                // 'e'
    'f',                // 'f'
    'g',                // 'g'
    'h',                // 'h'
    'i',                // 'i'
    'j',                // 'j'
    'k',                // 'k'
    'l',                // 'l'
    'm',                // 'm'
    'n',                // 'n'
    'o',                // 'o'
    'p',                // 'p'
    'q',                // 'q'
    'r',                // 'r'
    's',                // 's'
    't',                // 't'
    'u',                // 'u'
    'v',                // 'v'
    'w',                // 'w'
    'x',                // 'x'
    'y',                // 'y'
    'z',                // 'z'
    '{',                // '{'
    '|',                // '|'
    '}',                // '}'
    '~',                // '~'
    #127,               // #127
    #226#130#172,       // #128
    #194#129,           // #129
    #194#130,           // #130
    #194#131,           // #131
    #194#132,           // #132
    #226#128#166,       // #133
    #194#134,           // #134
    #194#135,           // #135
    #194#136,           // #136
    #194#137,           // #137
    #194#138,           // #138
    #194#139,           // #139
    #194#140,           // #140
    #194#141,           // #141
    #194#142,           // #142
    #194#143,           // #143
    #194#144,           // #144
    #226#128#152,       // #145
    #226#128#153,       // #146
    #226#128#156,       // #147
    #226#128#157,       // #148
    #226#128#162,       // #149
    #226#128#147,       // #150
    #226#128#148,       // #151
    #194#152,           // #152
    #194#153,           // #153
    #194#154,           // #154
    #194#155,           // #155
    #194#156,           // #156
    #194#157,           // #157
    #194#158,           // #158
    #194#159,           // #159
    #194#160,           // #160
    #224#184#129,       // #161
    #224#184#130,       // #162
    #224#184#131,       // #163
    #224#184#132,       // #164
    #224#184#133,       // #165
    #224#184#134,       // #166
    #224#184#135,       // #167
    #224#184#136,       // #168
    #224#184#137,       // #169
    #224#184#138,       // #170
    #224#184#139,       // #171
    #224#184#140,       // #172
    #224#184#141,       // #173
    #224#184#142,       // #174
    #224#184#143,       // #175
    #224#184#144,       // #176
    #224#184#145,       // #177
    #224#184#146,       // #178
    #224#184#147,       // #179
    #224#184#148,       // #180
    #224#184#149,       // #181
    #224#184#150,       // #182
    #224#184#151,       // #183
    #224#184#152,       // #184
    #224#184#153,       // #185
    #224#184#154,       // #186
    #224#184#155,       // #187
    #224#184#156,       // #188
    #224#184#157,       // #189
    #224#184#158,       // #190
    #224#184#159,       // #191
    #224#184#160,       // #192
    #224#184#161,       // #193
    #224#184#162,       // #194
    #224#184#163,       // #195
    #224#184#164,       // #196
    #224#184#165,       // #197
    #224#184#166,       // #198
    #224#184#167,       // #199
    #224#184#168,       // #200
    #224#184#169,       // #201
    #224#184#170,       // #202
    #224#184#171,       // #203
    #224#184#172,       // #204
    #224#184#173,       // #205
    #224#184#174,       // #206
    #224#184#175,       // #207
    #224#184#176,       // #208
    #224#184#177,       // #209
    #224#184#178,       // #210
    #224#184#179,       // #211
    #224#184#180,       // #212
    #224#184#181,       // #213
    #224#184#182,       // #214
    #224#184#183,       // #215
    #224#184#184,       // #216
    #224#184#185,       // #217
    #224#184#186,       // #218
    #195#155,           // #219
    #195#156,           // #220
    #195#157,           // #221
    #195#158,           // #222
    #224#184#191,       // #223
    #224#185#128,       // #224
    #224#185#129,       // #225
    #224#185#130,       // #226
    #224#185#131,       // #227
    #224#185#132,       // #228
    #224#185#133,       // #229
    #224#185#134,       // #230
    #224#185#135,       // #231
    #224#185#136,       // #232
    #224#185#137,       // #233
    #224#185#138,       // #234
    #224#185#139,       // #235
    #224#185#140,       // #236
    #224#185#141,       // #237
    #224#185#142,       // #238
    #224#185#143,       // #239
    #224#185#144,       // #240
    #224#185#145,       // #241
    #224#185#146,       // #242
    #224#185#147,       // #243
    #224#185#148,       // #244
    #224#185#149,       // #245
    #224#185#150,       // #246
    #224#185#151,       // #247
    #224#185#152,       // #248
    #224#185#153,       // #249
    #224#185#154,       // #250
    #224#185#155,       // #251
    #195#188,           // #252
    #195#189,           // #253
    #195#190,           // #254
    #195#191            // #255
  );

  ArrayKOI8ToUTF8: TCharToUTF8Table = (
    #0,                 // #0
    #1,                 // #1
    #2,                 // #2
    #3,                 // #3
    #4,                 // #4
    #5,                 // #5
    #6,                 // #6
    #7,                 // #7
    #8,                 // #8
    #9,                 // #9
    #10,                // #10
    #11,                // #11
    #12,                // #12
    #13,                // #13
    #14,                // #14
    #15,                // #15
    #16,                // #16
    #17,                // #17
    #18,                // #18
    #19,                // #19
    #20,                // #20
    #21,                // #21
    #22,                // #22
    #23,                // #23
    #24,                // #24
    #25,                // #25
    #26,                // #26
    #27,                // #27
    #28,                // #28
    #29,                // #29
    #30,                // #30
    #31,                // #31
    ' ',                // ' '
    '!',                // '!'
    '"',                // '"'
    '#',                // '#'
    '$',                // '$'
    '%',                // '%'
    '&',                // '&'
    '''',               // ''''
    '(',                // '('
    ')',                // ')'
    '*',                // '*'
    '+',                // '+'
    ',',                // ','
    '-',                // '-'
    '.',                // '.'
    '/',                // '/'
    '0',                // '0'
    '1',                // '1'
    '2',                // '2'
    '3',                // '3'
    '4',                // '4'
    '5',                // '5'
    '6',                // '6'
    '7',                // '7'
    '8',                // '8'
    '9',                // '9'
    ':',                // ':'
    ';',                // ';'
    '<',                // '<'
    '=',                // '='
    '>',                // '>'
    '?',                // '?'
    '@',                // '@'
    'A',                // 'A'
    'B',                // 'B'
    'C',                // 'C'
    'D',                // 'D'
    'E',                // 'E'
    'F',                // 'F'
    'G',                // 'G'
    'H',                // 'H'
    'I',                // 'I'
    'J',                // 'J'
    'K',                // 'K'
    'L',                // 'L'
    'M',                // 'M'
    'N',                // 'N'
    'O',                // 'O'
    'P',                // 'P'
    'Q',                // 'Q'
    'R',                // 'R'
    'S',                // 'S'
    'T',                // 'T'
    'U',                // 'U'
    'V',                // 'V'
    'W',                // 'W'
    'X',                // 'X'
    'Y',                // 'Y'
    'Z',                // 'Z'
    '[',                // '['
    '\',                // '\'
    ']',                // ']'
    '^',                // '^'
    '_',                // '_'
    '`',                // '`'
    'a',                // 'a'
    'b',                // 'b'
    'c',                // 'c'
    'd',                // 'd'
    'e',                // 'e'
    'f',                // 'f'
    'g',                // 'g'
    'h',                // 'h'
    'i',                // 'i'
    'j',                // 'j'
    'k',                // 'k'
    'l',                // 'l'
    'm',                // 'm'
    'n',                // 'n'
    'o',                // 'o'
    'p',                // 'p'
    'q',                // 'q'
    'r',                // 'r'
    's',                // 's'
    't',                // 't'
    'u',                // 'u'
    'v',                // 'v'
    'w',                // 'w'
    'x',                // 'x'
    'y',                // 'y'
    'z',                // 'z'
    '{',                // '{'
    '|',                // '|'
    '}',                // '}'
    '~',                // '~'
    #127,               // #127
    '',                 // #128
    '',                 // #129
    '',                 // #130
    '',                 // #131
    '',                 // #132
    '',                 // #133
    '',                 // #134
    '',                 // #135
    '',                 // #136
    '',                 // #137
    '',                 // #138
    '',                 // #139
    '',                 // #140
    '',                 // #141
    '',                 // #142
    '',                 // #143
    '',                 // #144
    '',                 // #145
    '',                 // #146
    '',                 // #147
    '',                 // #148
    '',                 // #149
    '',                 // #150
    '',                 // #151
    '',                 // #152
    '',                 // #153
    '',                 // #154
    '',                 // #155
    '',                 // #156
    '',                 // #157
    '',                 // #158
    '',                 // #159
    '',                 // #160
    '',                 // #161
    '',                 // #162
    '',                 // #163
    '',                 // #164
    '',                 // #165
    '',                 // #166
    '',                 // #167
    '',                 // #168
    '',                 // #169
    '',                 // #170
    '',                 // #171
    '',                 // #172
    '',                 // #173
    '',                 // #174
    '',                 // #175
    '',                 // #176
    '',                 // #177
    '',                 // #178
    '',                 // #179
    '',                 // #180
    '',                 // #181
    '',                 // #182
    '',                 // #183
    '',                 // #184
    '',                 // #185
    '',                 // #186
    '',                 // #187
    '',                 // #188
    '',                 // #189
    '',                 // #190
    '',                 // #191
    #209#142,           // #192
    #208#176,           // #193
    #208#177,           // #194
    #209#134,           // #195
    #208#180,           // #196
    #208#181,           // #197
    #209#132,           // #198
    #208#179,           // #199
    #209#133,           // #200
    #208#184,           // #201
    #208#185,           // #202
    #208#186,           // #203
    #208#187,           // #204
    #208#188,           // #205
    #208#189,           // #206
    #208#190,           // #207
    #208#191,           // #208
    #209#143,           // #209
    #209#128,           // #210
    #209#129,           // #211
    #209#130,           // #212
    #209#131,           // #213
    #208#182,           // #214
    #208#178,           // #215
    #209#140,           // #216
    #209#139,           // #217
    #208#183,           // #218
    #209#136,           // #219
    #209#141,           // #220
    #209#137,           // #221
    #209#135,           // #222
    #209#138,           // #223
    #208#174,           // #224
    #208#144,           // #225
    #208#145,           // #226
    #208#166,           // #227
    #208#148,           // #228
    #208#149,           // #229
    #208#164,           // #230
    #208#147,           // #231
    #208#165,           // #232
    #208#152,           // #233
    #208#153,           // #234
    #208#154,           // #235
    #208#155,           // #236
    #208#156,           // #237
    #208#157,           // #238
    #208#158,           // #239
    #208#159,           // #240
    #208#175,           // #241
    #208#160,           // #242
    #208#161,           // #243
    #208#162,           // #244
    #208#163,           // #245
    #208#150,           // #246
    #208#146,           // #247
    #208#172,           // #248
    #208#171,           // #249
    #208#151,           // #250
    #208#168,           // #251
    #208#173,           // #252
    #208#169,           // #253
    #208#167,           // #254
    ''                  // #255
  );

  ArrayMacintoshToUTF8: TCharToUTF8Table = (
    #0,                 // #0
    #1,                 // #1
    #2,                 // #2
    #3,                 // #3
    #4,                 // #4
    #5,                 // #5
    #6,                 // #6
    #7,                 // #7
    #8,                 // #8
    #9,                 // #9
    #10,                // #10
    #11,                // #11
    #12,                // #12
    #13,                // #13
    #14,                // #14
    #15,                // #15
    #16,                // #16
    #17,                // #17
    #18,                // #18
    #19,                // #19
    #20,                // #20
    #21,                // #21
    #22,                // #22
    #23,                // #23
    #24,                // #24
    #25,                // #25
    #26,                // #26
    #27,                // #27
    #28,                // #28
    #29,                // #29
    #30,                // #30
    #31,                // #31
    ' ',                // ' '
    '!',                // '!'
    '"',                // '"'
    '#',                // '#'
    '$',                // '$'
    '%',                // '%'
    '&',                // '&'
    '''',               // ''''
    '(',                // '('
    ')',                // ')'
    '*',                // '*'
    '+',                // '+'
    ',',                // ','
    '-',                // '-'
    '.',                // '.'
    '/',                // '/'
    '0',                // '0'
    '1',                // '1'
    '2',                // '2'
    '3',                // '3'
    '4',                // '4'
    '5',                // '5'
    '6',                // '6'
    '7',                // '7'
    '8',                // '8'
    '9',                // '9'
    ':',                // ':'
    ';',                // ';'
    '<',                // '<'
    '=',                // '='
    '>',                // '>'
    '?',                // '?'
    '@',                // '@'
    'A',                // 'A'
    'B',                // 'B'
    'C',                // 'C'
    'D',                // 'D'
    'E',                // 'E'
    'F',                // 'F'
    'G',                // 'G'
    'H',                // 'H'
    'I',                // 'I'
    'J',                // 'J'
    'K',                // 'K'
    'L',                // 'L'
    'M',                // 'M'
    'N',                // 'N'
    'O',                // 'O'
    'P',                // 'P'
    'Q',                // 'Q'
    'R',                // 'R'
    'S',                // 'S'
    'T',                // 'T'
    'U',                // 'U'
    'V',                // 'V'
    'W',                // 'W'
    'X',                // 'X'
    'Y',                // 'Y'
    'Z',                // 'Z'
    '[',                // '['
    '\',                // '\'
    ']',                // ']'
    '^',                // '^'
    '_',                // '_'
    '`',                // '`'
    'a',                // 'a'
    'b',                // 'b'
    'c',                // 'c'
    'd',                // 'd'
    'e',                // 'e'
    'f',                // 'f'
    'g',                // 'g'
    'h',                // 'h'
    'i',                // 'i'
    'j',                // 'j'
    'k',                // 'k'
    'l',                // 'l'
    'm',                // 'm'
    'n',                // 'n'
    'o',                // 'o'
    'p',                // 'p'
    'q',                // 'q'
    'r',                // 'r'
    's',                // 's'
    't',                // 't'
    'u',                // 'u'
    'v',                // 'v'
    'w',                // 'w'
    'x',                // 'x'
    'y',                // 'y'
    'z',                // 'z'
    '{',                // '{'
    '|',                // '|'
    '}',                // '}'
    '~',                // '~'
    #127,               // #127
    #195#132,           // #128
    #195#133,           // #129
    #195#135,           // #130
    #195#137,           // #131
    #195#145,           // #132
    #195#150,           // #133
    #195#156,           // #134
    #195#161,           // #135
    #195#160,           // #136
    #195#162,           // #137
    #195#164,           // #138
    #195#163,           // #139
    #195#165,           // #140
    #195#167,           // #141
    #195#169,           // #142
    #195#168,           // #143
    #195#170,           // #144
    #195#171,           // #145
    #195#173,           // #146
    #195#172,           // #147
    #195#174,           // #148
    #195#175,           // #149
    #195#177,           // #150
    #195#179,           // #151
    #195#178,           // #152
    #195#180,           // #153
    #195#182,           // #154
    #195#181,           // #155
    #195#186,           // #156
    #195#185,           // #157
    #195#187,           // #158
    #195#188,           // #159
    #226#128#160,       // #160
    #194#176,           // #161
    #194#162,           // #162
    #194#163,           // #163
    #194#167,           // #164
    #226#128#162,       // #165
    #194#182,           // #166
    #195#159,           // #167
    #194#174,           // #168
    #194#169,           // #169
    #226#132#162,       // #170
    #194#180,           // #171
    #194#168,           // #172
    #226#137#160,       // #173
    #195#134,           // #174
    #195#152,           // #175
    #226#136#158,       // #176
    #194#177,           // #177
    #226#137#164,       // #178
    #226#137#165,       // #179
    #194#165,           // #180
    #194#181,           // #181
    #226#136#130,       // #182
    #226#136#145,       // #183
    #226#136#143,       // #184
    #207#128,           // #185
    #226#136#171,       // #186
    #194#170,           // #187
    #194#186,           // #188
    #206#169,           // #189
    #195#166,           // #190
    #195#184,           // #191
    #194#191,           // #192
    #194#161,           // #193
    #194#172,           // #194
    #226#136#154,       // #195
    #198#146,           // #196
    #226#137#136,       // #197
    #206#148,           // #198
    #194#171,           // #199
    #194#187,           // #200
    #226#128#166,       // #201
    #194#160,           // #202
    #195#128,           // #203
    #195#131,           // #204
    #195#149,           // #205
    #197#146,           // #206
    #197#147,           // #207
    #226#128#147,       // #208
    #226#128#148,       // #209
    #226#128#156,       // #210
    #226#128#157,       // #211
    #226#128#152,       // #212
    #226#128#153,       // #213
    #195#183,           // #214
    #226#151#138,       // #215
    #195#191,           // #216
    #197#184,           // #217
    #226#129#132,       // #218
    #226#130#172,       // #219
    #226#128#185,       // #220
    #226#128#186,       // #221
    #239#172#129,       // #222
    #239#172#130,       // #223
    #226#128#161,       // #224
    #194#183,           // #225
    #226#128#154,       // #226
    #226#128#158,       // #227
    #226#128#176,       // #228
    #195#130,           // #229
    #195#138,           // #230
    #195#129,           // #231
    #195#139,           // #232
    #195#136,           // #233
    #195#141,           // #234
    #195#142,           // #235
    #195#143,           // #236
    #195#140,           // #237
    #195#147,           // #238
    #195#148,           // #239
    #238#128#158,       // #240
    #195#146,           // #241
    #195#154,           // #242
    #195#155,           // #243
    #195#153,           // #244
    #196#177,           // #245
    #203#134,           // #246
    #203#156,           // #247
    #194#175,           // #248
    #203#152,           // #249
    #203#153,           // #250
    #203#154,           // #251
    #194#184,           // #252
    #203#157,           // #253
    #203#155,           // #254
    #203#135            // #255
  );

function UTF8BOMToUTF8(const s: string): string;
begin
  Result:=copy(s,4,length(s));
end;

function ISO_8859_1ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayISO_8859_1ToUTF8);
end;

function ISO_8859_15ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayISO_8859_15ToUTF8);
end;

function ISO_8859_2ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayISO_8859_2ToUTF8);
end;

function CP1250ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP1250ToUTF8);
end;

function CP1251ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP1251ToUTF8);
end;

function CP1252ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP1252ToUTF8);
end;

function CP1253ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP1253ToUTF8);
end;

function CP1254ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP1254ToUTF8);
end;

function CP1255ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP1255ToUTF8);
end;

function CP1256ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP1256ToUTF8);
end;

function CP1257ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP1257ToUTF8);
end;

function CP1258ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP1258ToUTF8);
end;

function CP437ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP437ToUTF8);
end;

function CP850ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP850ToUTF8);
end;

function CP852ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP852ToUTF8);
end;

function CP866ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP866ToUTF8);
end;

function CP874ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP874ToUTF8);
end;

function KOI8ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayKOI8ToUTF8);
end;

function MacintoshToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayMacintoshToUTF8);
end;

function SingleByteToUTF8(const s: string; const Table: TCharToUTF8Table): string;
var
  len: Integer;
  i: Integer;
  Src: PChar;
  Dest: PChar;
  p: PChar;
  c: Char;
begin
  if s='' then begin
    Result:='';
    exit;
  end;
  len:=length(s);
  SetLength(Result,len*4);// UTF-8 is at most 4 bytes
  Src:=PChar(s);
  Dest:=PChar(Result);
  for i:=1 to len do begin
    c:=Src^;
    inc(Src);
    if ord(c)<128 then begin
      Dest^:=c;
      inc(Dest);
    end else begin
      p:=Table[c];
      if p<>nil then begin
        while p^<>#0 do begin
          Dest^:=p^;
          inc(p);
          inc(Dest);
        end;
      end;
    end;
  end;
  SetLength(Result,{%H-}PtrUInt(Dest)-PtrUInt(Result));
end;

function UCS2LEToUTF8(const s: string): string;
var
  len: Integer;
  Src: PWord;
  Dest: PChar;
  i: Integer;
  c: Word;
begin
  len:=length(s) div 2;
  if len=0 then
    exit('');
  SetLength(Result,len*3);// UTF-8 is at most 3/2 times the size
  Src:=PWord(Pointer(s));
  Dest:=PChar(Result);
  for i:=1 to len do begin
    c:=LEtoN(Src^);
    inc(Src);
    if ord(c)<128 then begin
      Dest^:=chr(c);
      inc(Dest);
    end else begin
      inc(Dest,UnicodeToUTF8SkipErrors(c,Dest));
    end;
  end;
  len:={%H-}PtrUInt(Dest)-PtrUInt(Result);
  if len>length(Result) then
    raise Exception.Create('');
  SetLength(Result,len);
end;

function UCS2BEToUTF8(const s: string): string;
var
  len: Integer;
  Src: PWord;
  Dest: PChar;
  i: Integer;
  c: Word;
begin
  len:=length(s) div 2;
  if len=0 then
    exit('');
  SetLength(Result,len*3);// UTF-8 is at most three times the size
  Src:=PWord(Pointer(s));
  Dest:=PChar(Result);
  for i:=1 to len do begin
    c:=BEtoN(Src^);
    inc(Src);
    if ord(c)<128 then begin
      Dest^:=chr(c);
      inc(Dest);
    end else begin
      inc(Dest,UnicodeToUTF8SkipErrors(c,Dest));
    end;
  end;
  len:={%H-}PtrUInt(Dest)-PtrUInt(Result);
  if len>length(Result) then
    raise Exception.Create('');
  SetLength(Result,len);
end;

function UTF8ToUTF8BOM(const s: string): string;
begin
  Result:=UTF8BOM+s;
end;

{$IfNdef UseSystemCPConv}
function UnicodeToCP1256(Unicode: cardinal): integer;
begin
  case Unicode of
  0..127: Result:=Unicode;
  160: Result:=160;
  162..169: Result:=Unicode;
  171..185: Result:=Unicode;
  187..190: Result:=Unicode;
  215: Result:=215;
  224: Result:=224;
  226: Result:=226;
  231..235: Result:=Unicode;
  238..239: Result:=Unicode;
  244: Result:=244;
  247: Result:=247;
  249: Result:=249;
  251..252: Result:=Unicode;
  338: Result:=140;
  339: Result:=156;
  402: Result:=131;
  710: Result:=136;
  1548: Result:=161;
  1563: Result:=186;
  1567: Result:=191;
  1569..1590: Result:=Unicode-1376;
  1591..1594: Result:=Unicode-1375;
  1600..1603: Result:=Unicode-1380;
  1604: Result:=225;
  1605..1608: Result:=Unicode-1378;
  1609..1610: Result:=Unicode-1373;
  1611..1614: Result:=Unicode-1371;
  1615..1616: Result:=Unicode-1370;
  1617: Result:=248;
  1618: Result:=250;
  1657: Result:=138;
  1662: Result:=129;
  1670: Result:=141;
  1672: Result:=143;
  1681: Result:=154;
  1688: Result:=142;
  1705: Result:=152;
  1711: Result:=144;
  1722: Result:=159;
  1726: Result:=170;
  1729: Result:=192;
  1746: Result:=255;
  8204..8205: Result:=Unicode-8047;
  8206..8207: Result:=Unicode-7953;
  8211..8212: Result:=Unicode-8061;
  8216..8217: Result:=Unicode-8071;
  8218: Result:=130;
  8220..8221: Result:=Unicode-8073;
  8222: Result:=132;
  8224..8225: Result:=Unicode-8090;
  8226: Result:=149;
  8230: Result:=133;
  8240: Result:=137;
  8249: Result:=139;
  8250: Result:=155;
  8364: Result:=128;
  8482: Result:=153;
  else Result:=-1;
  end;
end;

function UnicodeToCP437(Unicode: cardinal): integer;
begin
  case Unicode of
  0..127: Result:=Unicode;
  160: Result:=255;
  161: Result:=173;
  162..163: Result:=Unicode-7;
  165: Result:=157;
  170: Result:=166;
  171: Result:=174;
  172: Result:=170;
  176: Result:=248;
  177: Result:=241;
  178: Result:=253;
  181: Result:=230;
  183: Result:=250;
  186: Result:=167;
  187: Result:=175;
  188: Result:=172;
  189: Result:=171;
  191: Result:=168;
  196..197: Result:=Unicode-54;
  198: Result:=146;
  199: Result:=128;
  201: Result:=144;
  209: Result:=165;
  214: Result:=153;
  220: Result:=154;
  223: Result:=225;
  224: Result:=133;
  225: Result:=160;
  226: Result:=131;
  228: Result:=132;
  229: Result:=134;
  230: Result:=145;
  231: Result:=135;
  232: Result:=138;
  233: Result:=130;
  234..235: Result:=Unicode-98;
  236: Result:=141;
  237: Result:=161;
  238: Result:=140;
  239: Result:=139;
  241: Result:=164;
  242: Result:=149;
  243: Result:=162;
  244: Result:=147;
  246: Result:=148;
  247: Result:=246;
  249: Result:=151;
  250: Result:=163;
  251: Result:=150;
  252: Result:=129;
  255: Result:=152;
  262: Result := 93;
  263: Result := 125;
  268: Result := 94;
  269: Result := 126;
  272: Result := 92;
  273: Result := 124;
  381: Result := 64;
  382: Result := 96;
  352: Result := 91;
  353: Result := 123;
  402: Result:=159;
  915: Result:=226;
  920: Result:=233;
  931: Result:=228;
  934: Result:=232;
  937: Result:=234;
  945: Result:=224;
  948: Result:=235;
  949: Result:=238;
  960: Result:=227;
  963: Result:=229;
  964: Result:=231;
  966: Result:=237;
  8319: Result:=252;
  8359: Result:=158;
  8729: Result:=249;
  8730: Result:=251;
  8734: Result:=236;
  8745: Result:=239;
  8776: Result:=247;
  8801: Result:=240;
  8804: Result:=243;
  8805: Result:=242;
  8976: Result:=169;
  8992..8993: Result:=Unicode-8748;
  9472: Result:=196;
  9474: Result:=179;
  9484: Result:=218;
  9488: Result:=191;
  9492: Result:=192;
  9496: Result:=217;
  9500: Result:=195;
  9508: Result:=180;
  9516: Result:=194;
  9524: Result:=193;
  9532: Result:=197;
  9552: Result:=205;
  9553: Result:=186;
  9554..9555: Result:=Unicode-9341;
  9556: Result:=201;
  9557: Result:=184;
  9558: Result:=183;
  9559: Result:=187;
  9560: Result:=212;
  9561: Result:=211;
  9562: Result:=200;
  9563: Result:=190;
  9564: Result:=189;
  9565: Result:=188;
  9566..9567: Result:=Unicode-9368;
  9568: Result:=204;
  9569..9570: Result:=Unicode-9388;
  9571: Result:=185;
  9572..9573: Result:=Unicode-9363;
  9574: Result:=203;
  9575..9576: Result:=Unicode-9368;
  9577: Result:=202;
  9578: Result:=216;
  9579: Result:=215;
  9580: Result:=206;
  9600: Result:=223;
  9604: Result:=220;
  9608: Result:=219;
  9612: Result:=221;
  9616: Result:=222;
  9617..9619: Result:=Unicode-9441;
  9632: Result:=254;
  else Result:=-1;
  end;
end;

function UnicodeToCP850(Unicode: cardinal): integer;
begin
  case Unicode of
  0..127: Result:=Unicode;
  160: Result:=255;
  161: Result:=173;
  162: Result:=189;
  163: Result:=156;
  164: Result:=207;
  165: Result:=190;
  166: Result:=221;
  167: Result:=245;
  168: Result:=249;
  169: Result:=184;
  170: Result:=166;
  171: Result:=174;
  172: Result:=170;
  173: Result:=240;
  174: Result:=169;
  175: Result:=238;
  176: Result:=248;
  177: Result:=241;
  178: Result:=253;
  179: Result:=252;
  180: Result:=239;
  181: Result:=230;
  182: Result:=244;
  183: Result:=250;
  184: Result:=247;
  185: Result:=251;
  186: Result:=167;
  187: Result:=175;
  188: Result:=172;
  189: Result:=171;
  190: Result:=243;
  191: Result:=168;
  192: Result:=183;
  193..194: Result:=Unicode-12;
  195: Result:=199;
  196..197: Result:=Unicode-54;
  198: Result:=146;
  199: Result:=128;
  200: Result:=212;
  201: Result:=144;
  202..203: Result:=Unicode+8;
  204: Result:=222;
  205..207: Result:=Unicode+9;
  208: Result:=209;
  209: Result:=165;
  210: Result:=227;
  211: Result:=224;
  212: Result:=226;
  213: Result:=229;
  214: Result:=153;
  215: Result:=158;
  216: Result:=157;
  217: Result:=235;
  218..219: Result:=Unicode+15;
  220: Result:=154;
  221: Result:=237;
  222: Result:=232;
  223: Result:=225;
  224: Result:=133;
  225: Result:=160;
  226: Result:=131;
  227: Result:=198;
  228: Result:=132;
  229: Result:=134;
  230: Result:=145;
  231: Result:=135;
  232: Result:=138;
  233: Result:=130;
  234..235: Result:=Unicode-98;
  236: Result:=141;
  237: Result:=161;
  238: Result:=140;
  239: Result:=139;
  240: Result:=208;
  241: Result:=164;
  242: Result:=149;
  243: Result:=162;
  244: Result:=147;
  245: Result:=228;
  246: Result:=148;
  247: Result:=246;
  248: Result:=155;
  249: Result:=151;
  250: Result:=163;
  251: Result:=150;
  252: Result:=129;
  253: Result:=236;
  254: Result:=231;
  255: Result:=152;
  305: Result:=213;
  402: Result:=159;
  8215: Result:=242;
  9472: Result:=196;
  9474: Result:=179;
  9484: Result:=218;
  9488: Result:=191;
  9492: Result:=192;
  9496: Result:=217;
  9500: Result:=195;
  9508: Result:=180;
  9516: Result:=194;
  9524: Result:=193;
  9532: Result:=197;
  9552: Result:=205;
  9553: Result:=186;
  9556: Result:=201;
  9559: Result:=187;
  9562: Result:=200;
  9565: Result:=188;
  9568: Result:=204;
  9571: Result:=185;
  9574: Result:=203;
  9577: Result:=202;
  9580: Result:=206;
  9600: Result:=223;
  9604: Result:=220;
  9608: Result:=219;
  9617..9619: Result:=Unicode-9441;
  9632: Result:=254;
  else Result:=-1;
  end;
end;

// ftp://ftp.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP852.TXT
function UnicodeToCP852(Unicode: cardinal): integer;
begin
  case Unicode of
  0..127: Result:=Unicode;
  160: Result:=255;
  164: Result:=207;
  167: Result:=245;
  168: Result:=249;
  171: Result:=174;
  172: Result:=170;
  173: Result:=240;
  176: Result:=248;
  180: Result:=239;
  184: Result:=247;
  187: Result:=175;
  193..194: Result:=Unicode-12;
  196: Result:=142;
  199: Result:=128;
  201: Result:=144;
  203: Result:=211;
  205..206: Result:=Unicode+9;
  211: Result:=224;
  212: Result:=226;
  214: Result:=153;
  215: Result:=158;
  218: Result:=233;
  220: Result:=154;
  221: Result:=237;
  223: Result:=225;
  225: Result:=160;
  226: Result:=131;
  228: Result:=132;
  231: Result:=135;
  233: Result:=130;
  235: Result:=137;
  237: Result:=161;
  238: Result:=140;
  243: Result:=162;
  244: Result:=147;
  246: Result:=148;
  247: Result:=246;
  250: Result:=163;
  252: Result:=129;
  253: Result:=236;
  258..259: Result:=Unicode-60;
  260..261: Result:=Unicode-96;
  262: Result:=143;
  263: Result:=134;
  268: Result:=172;
  269: Result:=159;
  270: Result:=210;
  271: Result:=212;
  272: Result:=209;
  273: Result:=208;
  280..281: Result:=Unicode-112;
  282: Result:=183;
  283: Result:=216;
  313..314: Result:=Unicode-168;
  317..318: Result:=Unicode-168;
  321: Result:=157;
  322: Result:=136;
  323..324: Result:=Unicode-96;
  327: Result:=213;
  328: Result:=229;
  336..337: Result:=Unicode-198;
  340: Result:=232;
  341: Result:=234;
  344..345: Result:=Unicode-92;
  346..347: Result:=Unicode-195;
  350: Result:=184;
  351: Result:=173;
  352..353: Result:=Unicode-122;
  354: Result:=221;
  355: Result:=238;
  356..357: Result:=Unicode-201;
  366: Result:=222;
  367: Result:=133;
  368: Result:=235;
  369: Result:=251;
  377: Result:=141;
  378: Result:=171;
  379..380: Result:=Unicode-190;
  381..382: Result:=Unicode-215;
  711: Result:=243;
  728: Result:=244;
  729: Result:=250;
  731: Result:=242;
  733: Result:=241;
  9472: Result:=196;
  9474: Result:=179;
  9484: Result:=218;
  9488: Result:=191;
  9492: Result:=192;
  9496: Result:=217;
  9500: Result:=195;
  9508: Result:=180;
  9516: Result:=194;
  9524: Result:=193;
  9532: Result:=197;
  9552: Result:=205;
  9553: Result:=186;
  9556: Result:=201;
  9559: Result:=187;
  9562: Result:=200;
  9565: Result:=188;
  9568: Result:=204;
  9571: Result:=185;
  9574: Result:=203;
  9577: Result:=202;
  9580: Result:=206;
  9600: Result:=223;
  9604: Result:=220;
  9608: Result:=219;
  9617..9619: Result:=Unicode-9441;
  9632: Result:=254;
  else Result:=-1;
  end;
end;

function UnicodeToCP866(Unicode: cardinal): integer;
begin
  case Unicode of
  0..127: Result:=Unicode;
  1040..1087 : Result := Unicode-912;
  9617..9619 : Result := Unicode-9441;
  9474 : Result := 179;
  9508 : Result := 180;
  9569 : Result := 181;
  9570 : Result := 182;
  9558 : Result := 183;
  9557 : Result := 184;
  9571 : Result := 185;
  9553 : Result := 186;
  9559 : Result := 187;
  9565 : Result := 188;
  9564 : Result := 189;
  9563 : Result := 190;
  9488 : Result := 191;
  9492 : Result := 192;
  9524 : Result := 193;
  9516 : Result := 194;
  9500 : Result := 195;
  9472 : Result := 196;
  9532 : Result := 197;
  9566 : Result := 198;
  9567 : Result := 199;
  9562 : Result := 200;
  9556 : Result := 201;
  9577 : Result := 202;
  9574 : Result := 203;
  9568 : Result := 204;
  9552 : Result := 205;
  9580 : Result := 206;
  9575 : Result := 207;
  9576 : Result := 208;
  9572 : Result := 209;
  9573 : Result := 210;
  9561 : Result := 211;
  9560 : Result := 212;
  9554 : Result := 213;
  9555 : Result := 214;
  9579 : Result := 215;
  9578 : Result := 216;
  9496 : Result := 217;
  9484 : Result := 218;
  9608 : Result := 219;
  9604 : Result := 220;
  9612 : Result := 221;
  9616 : Result := 222;
  9600 : Result := 223;
  1088..1103 : Result := Unicode-864;
  1025 : Result := 240;
  1105 : Result := 241;
  1028 : Result := 242;
  1108 : Result := 243;
  1031 : Result := 244;
  1111 : Result := 245;
  1038 : Result := 246;
  1118 : Result := 247;
  176  : Result := 248;
  8729 : Result := 249;
  183  : Result := 250;
  8730 : Result := 251;
  8470 : Result := 252;
  164  : Result := 253;
  9632 : Result := 254;
  160  : Result := 255;
  else Result:=-1;
  end;
end;

function UnicodeToKOI8(Unicode: cardinal): integer;
begin
  case Unicode of
  0..127: Result:=Unicode;
  1040..1041: Result:=Unicode-815;
  1042: Result:=247;
  1043: Result:=231;
  1044..1045: Result:=Unicode-816;
  1046: Result:=246;
  1047: Result:=250;
  1048..1055: Result:=Unicode-815;
  1056..1059: Result:=Unicode-814;
  1060: Result:=230;
  1061: Result:=232;
  1062: Result:=227;
  1063: Result:=254;
  1064: Result:=251;
  1065: Result:=253;
  1067: Result:=249;
  1068: Result:=248;
  1069: Result:=252;
  1070: Result:=224;
  1071: Result:=241;
  1072..1073: Result:=Unicode-879;
  1074: Result:=215;
  1075: Result:=199;
  1076..1077: Result:=Unicode-880;
  1078: Result:=214;
  1079: Result:=218;
  1080..1087: Result:=Unicode-879;
  1088..1091: Result:=Unicode-878;
  1092: Result:=198;
  1093: Result:=200;
  1094: Result:=195;
  1095: Result:=222;
  1096: Result:=219;
  1097: Result:=221;
  1098: Result:=223;
  1099: Result:=217;
  1100: Result:=216;
  1101: Result:=220;
  1102: Result:=192;
  1103: Result:=209;
  else Result:=-1;
  end;
end;

function UnicodeToKOI8U(Unicode: cardinal): integer;
begin
  case Unicode of
  1025: Result:=179;
  1028: Result:=180;
  1030..1031: Result:=Unicode-848;
  1105: Result:=163;
  1108: Result:=164;
  1110..1111: Result:=Unicode-944;
  1168: Result:=189;
  1169: Result:=173;
  else
    Result:=UnicodeToKOI8(Unicode);
  end;
end;

function UnicodeToKOI8RU(Unicode: cardinal): integer;
begin
  case Unicode of
  1038: Result:=190;
  1118: Result:=174;
  else
    Result:=UnicodeToKOI8U(Unicode);
  end;
end;

function UnicodeToISO_8859_1(Unicode: cardinal): integer;
begin
  case Unicode of
  0..255: Result:=Unicode;
  else Result:=-1;
  end;
end;

function UnicodeToISO_8859_15(Unicode: cardinal): integer;
begin
  case Unicode of
  0..255: Result:=Unicode;
  8364: Result:=164;
  352: Result:=166;
  353: Result:=168;
  381: Result:=180;
  382: Result:=184;
  338: Result:=188;
  339: Result:=189;
  376: Result:=190;
  else Result:=-1;
  end;
end;

function UnicodeToISO_8859_2(Unicode: cardinal): integer;
begin
  case Unicode of
  0..127: Result:=Unicode;
  128..160: Result:=Unicode;
  164: Result:=164;
  167..168: Result:=Unicode;
  173: Result:=173;
  176: Result:=176;
  180: Result:=180;
  184: Result:=184;
  193..194: Result:=Unicode;
  196: Result:=196;
  199: Result:=199;
  201: Result:=201;
  203: Result:=203;
  205..206: Result:=Unicode;
  211..212: Result:=Unicode;
  214..215: Result:=Unicode;
  218: Result:=218;
  220..221: Result:=Unicode;
  223: Result:=223;
  225..226: Result:=Unicode;
  228: Result:=228;
  231: Result:=231;
  233: Result:=233;
  235: Result:=235;
  237..238: Result:=Unicode;
  243..244: Result:=Unicode;
  246..247: Result:=Unicode;
  250: Result:=250;
  252..253: Result:=Unicode;
  258: Result:=195;
  259: Result:=227;
  260: Result:=161;
  261: Result:=177;
  262: Result:=198;
  263: Result:=230;
  268: Result:=200;
  269: Result:=232;
  270: Result:=207;
  271: Result:=239;
  272: Result:=208;
  273: Result:=240;
  280: Result:=202;
  281: Result:=234;
  282: Result:=204;
  283: Result:=236;
  313: Result:=197;
  314: Result:=229;
  317: Result:=165;
  318: Result:=181;
  321: Result:=163;
  322: Result:=179;
  323: Result:=209;
  324: Result:=241;
  327: Result:=210;
  328: Result:=242;
  336: Result:=213;
  337: Result:=245;
  340: Result:=192;
  341: Result:=224;
  344: Result:=216;
  345: Result:=248;
  346: Result:=166;
  347: Result:=182;
  350: Result:=170;
  351: Result:=186;
  352: Result:=169;
  353: Result:=185;
  354: Result:=222;
  355: Result:=254;
  356: Result:=171;
  357: Result:=187;
  366: Result:=217;
  367: Result:=249;
  368: Result:=219;
  369: Result:=251;
  377: Result:=172;
  378: Result:=188;
  379: Result:=175;
  380: Result:=191;
  381: Result:=174;
  382: Result:=190;
  711: Result:=183;
  728: Result:=162;
  729: Result:=255;
  731: Result:=178;
  733: Result:=189;
  else Result:=-1;
  end;
end;

function UnicodeToMacintosh(Unicode: cardinal): integer;
begin
  case Unicode of
  0..127: Result:=Unicode;
  160: Result:=202;
  161: Result:=193;
  162..163: Result:=Unicode;
  165: Result:=180;
  167: Result:=164;
  168: Result:=172;
  169: Result:=169;
  170: Result:=187;
  171: Result:=199;
  172: Result:=194;
  174: Result:=168;
  175: Result:=248;
  176: Result:=161;
  177: Result:=177;
  180: Result:=171;
  181: Result:=181;
  182: Result:=166;
  183: Result:=225;
  184: Result:=252;
  186: Result:=188;
  187: Result:=200;
  191: Result:=192;
  192: Result:=203;
  193: Result:=231;
  194: Result:=229;
  195: Result:=204;
  196..197: Result:=Unicode-68;
  198: Result:=174;
  199: Result:=130;
  200: Result:=233;
  201: Result:=131;
  202: Result:=230;
  203: Result:=232;
  204: Result:=237;
  205..207: Result:=Unicode+29;
  209: Result:=132;
  210: Result:=241;
  211..212: Result:=Unicode+27;
  213: Result:=205;
  214: Result:=133;
  216: Result:=175;
  217: Result:=244;
  218..219: Result:=Unicode+24;
  220: Result:=134;
  223: Result:=167;
  224: Result:=136;
  225: Result:=135;
  226: Result:=137;
  227: Result:=139;
  228: Result:=138;
  229: Result:=140;
  230: Result:=190;
  231: Result:=141;
  232: Result:=143;
  233: Result:=142;
  234..235: Result:=Unicode-90;
  236: Result:=147;
  237: Result:=146;
  238..239: Result:=Unicode-90;
  241: Result:=150;
  242: Result:=152;
  243: Result:=151;
  244: Result:=153;
  245: Result:=155;
  246: Result:=154;
  247: Result:=214;
  248: Result:=191;
  249: Result:=157;
  250: Result:=156;
  251..252: Result:=Unicode-93;
  255: Result:=216;
  305: Result:=245;
  338..339: Result:=Unicode-132;
  376: Result:=217;
  402: Result:=196;
  710: Result:=246;
  711: Result:=255;
  728..730: Result:=Unicode-479;
  731: Result:=254;
  732: Result:=247;
  733: Result:=253;
  916: Result:=198;
  937: Result:=189;
  960: Result:=185;
  8211..8212: Result:=Unicode-8003;
  8216..8217: Result:=Unicode-8004;
  8218: Result:=226;
  8220..8221: Result:=Unicode-8010;
  8222: Result:=227;
  8224: Result:=160;
  8225: Result:=224;
  8226: Result:=165;
  8230: Result:=201;
  8240: Result:=228;
  8249..8250: Result:=Unicode-8029;
  8260: Result:=218;
  8364: Result:=219;
  8482: Result:=170;
  8706: Result:=182;
  8719: Result:=184;
  8721: Result:=183;
  8730: Result:=195;
  8734: Result:=176;
  8747: Result:=186;
  8776: Result:=197;
  8800: Result:=173;
  8804..8805: Result:=Unicode-8626;
  9674: Result:=215;
  57374: Result:=240;
  64257..64258: Result:=Unicode-64035;
  else Result:=-1;
  end;
end;
{$endif}

function UnicodeToCP1250(Unicode: cardinal): integer;
begin
  case Unicode of
  0..127,129,131,136,144,152: Result:=Unicode;
  160: Result:=160;
  164: Result:=164;
  166..169: Result:=Unicode;
  171..174: Result:=Unicode;
  176..177: Result:=Unicode;
  180..184: Result:=Unicode;
  187: Result:=187;
  193..194: Result:=Unicode;
  196: Result:=196;
  199: Result:=199;
  201: Result:=201;
  203: Result:=203;
  205..206: Result:=Unicode;
  211..212: Result:=Unicode;
  214..215: Result:=Unicode;
  218: Result:=218;
  220..221: Result:=Unicode;
  223: Result:=223;
  225..226: Result:=Unicode;
  228: Result:=228;
  231: Result:=231;
  233: Result:=233;
  235: Result:=235;
  237..238: Result:=Unicode;
  243..244: Result:=Unicode;
  246..247: Result:=Unicode;
  250: Result:=250;
  252..253: Result:=Unicode;
  258: Result:=195;
  259: Result:=227;
  260: Result:=165;
  261: Result:=185;
  262: Result:=198;
  263: Result:=230;
  268: Result:=200;
  269: Result:=232;
  270: Result:=207;
  271: Result:=239;
  272: Result:=208;
  273: Result:=240;
  280: Result:=202;
  281: Result:=234;
  282: Result:=204;
  283: Result:=236;
  313: Result:=197;
  314: Result:=229;
  317: Result:=188;
  318: Result:=190;
  321: Result:=163;
  322: Result:=179;
  323: Result:=209;
  324: Result:=241;
  327: Result:=210;
  328: Result:=242;
  336: Result:=213;
  337: Result:=245;
  340: Result:=192;
  341: Result:=224;
  344: Result:=216;
  345: Result:=248;
  346: Result:=140;
  347: Result:=156;
  350: Result:=170;
  351: Result:=186;
  352: Result:=138;
  353: Result:=154;
  354: Result:=222;
  355: Result:=254;
  356: Result:=141;
  357: Result:=157;
  366: Result:=217;
  367: Result:=249;
  368: Result:=219;
  369: Result:=251;
  377: Result:=143;
  378: Result:=159;
  379: Result:=175;
  380: Result:=191;
  381: Result:=142;
  382: Result:=158;
  711: Result:=161;
  728: Result:=162;
  729: Result:=255;
  731: Result:=178;
  733: Result:=189;
  8211..8212: Result:=Unicode-8061;
  8216..8217: Result:=Unicode-8071;
  8218: Result:=130;
  8220..8221: Result:=Unicode-8073;
  8222: Result:=132;
  8224..8225: Result:=Unicode-8090;
  8226: Result:=149;
  8230: Result:=133;
  8240: Result:=137;
  8249: Result:=139;
  8250: Result:=155;
  8364: Result:=128;
  8482: Result:=153;
  else Result:=-1;
  end;
end;

function UnicodeToCP1251(Unicode: cardinal): integer;
begin
  case Unicode of
  0..127,152: Result:=Unicode;
  160: Result:=160;
  164: Result:=164;
  166..167: Result:=Unicode;
  169: Result:=169;
  171..174: Result:=Unicode;
  176..177: Result:=Unicode;
  181..183: Result:=Unicode;
  187: Result:=187;
  1025: Result:=168;
  1026..1027: Result:=Unicode-898;
  1028: Result:=170;
  1029: Result:=189;
  1030: Result:=178;
  1031: Result:=175;
  1032: Result:=163;
  1033: Result:=138;
  1034: Result:=140;
  1035: Result:=142;
  1036: Result:=141;
  1038: Result:=161;
  1039: Result:=143;
  1040..1103: Result:=Unicode-848;
  1105: Result:=184;
  1106: Result:=144;
  1107: Result:=131;
  1108: Result:=186;
  1109: Result:=190;
  1110: Result:=179;
  1111: Result:=191;
  1112: Result:=188;
  1113: Result:=154;
  1114: Result:=156;
  1115: Result:=158;
  1116: Result:=157;
  1118: Result:=162;
  1119: Result:=159;
  1168: Result:=165;
  1169: Result:=180;
  8211..8212: Result:=Unicode-8061;
  8216..8217: Result:=Unicode-8071;
  8218: Result:=130;
  8220..8221: Result:=Unicode-8073;
  8222: Result:=132;
  8224..8225: Result:=Unicode-8090;
  8226: Result:=149;
  8230: Result:=133;
  8240: Result:=137;
  8249: Result:=139;
  8250: Result:=155;
  8364: Result:=136;
  8470: Result:=185;
  8482: Result:=153;
  else Result:=-1;
  end;
end;

function UnicodeToCP1252(Unicode: cardinal): integer;
begin
  case Unicode of
  0..127,129,141,143,144,157: Result:=Unicode;
  160..255: Result:=Unicode;
  338: Result:=140;
  339: Result:=156;
  352: Result:=138;
  353: Result:=154;
  376: Result:=159;
  381: Result:=142;
  382: Result:=158;
  402: Result:=131;
  710: Result:=136;
  732: Result:=152;
  8211..8212: Result:=Unicode-8061;
  8216..8217: Result:=Unicode-8071;
  8218: Result:=130;
  8220..8221: Result:=Unicode-8073;
  8222: Result:=132;
  8224..8225: Result:=Unicode-8090;
  8226: Result:=149;
  8230: Result:=133;
  8240: Result:=137;
  8249: Result:=139;
  8250: Result:=155;
  8364: Result:=128;
  8482: Result:=153;
  else Result:=-1;
  end;
end;

function UnicodeToCP1253(Unicode: cardinal): integer;
begin
  case Unicode of
  0..127,129,136,138,140,141,142,143,144,152,154,156,157,158,159,170: Result:=Unicode;
  160: Result:=160;
  163..169: Result:=Unicode;
  171..174: Result:=Unicode;
  176..179: Result:=Unicode;
  181..183: Result:=Unicode;
  187: Result:=187;
  189: Result:=189;
  402: Result:=131;
  900: Result:=180;
  901..902: Result:=Unicode-740;
  904..906: Result:=Unicode-720;
  908: Result:=188;
  910..975: Result:=Unicode-720;
  8211..8212: Result:=Unicode-8061;
  8213: Result:=175;
  8216..8217: Result:=Unicode-8071;
  8218: Result:=130;
  8220..8221: Result:=Unicode-8073;
  8222: Result:=132;
  8224..8225: Result:=Unicode-8090;
  8226: Result:=149;
  8230: Result:=133;
  8240: Result:=137;
  8249: Result:=139;
  8250: Result:=155;
  8364: Result:=128;
  8482: Result:=153;
  else Result:=-1;
  end;
end;

function UnicodeToCP1254(Unicode: cardinal): integer;
begin
  case Unicode of
  0..127,129,141,142,143,144,157,158: Result:=Unicode;
  160..207: Result:=Unicode;
  209..220: Result:=Unicode;
  223..239: Result:=Unicode;
  241..252: Result:=Unicode;
  255: Result:=255;
  286: Result:=208;
  287: Result:=240;
  304: Result:=221;
  305: Result:=253;
  338: Result:=140;
  339: Result:=156;
  350: Result:=222;
  351: Result:=254;
  352: Result:=138;
  353: Result:=154;
  376: Result:=159;
  402: Result:=131;
  710: Result:=136;
  732: Result:=152;
  8211..8212: Result:=Unicode-8061;
  8216..8217: Result:=Unicode-8071;
  8218: Result:=130;
  8220..8221: Result:=Unicode-8073;
  8222: Result:=132;
  8224..8225: Result:=Unicode-8090;
  8226: Result:=149;
  8230: Result:=133;
  8240: Result:=137;
  8249: Result:=139;
  8250: Result:=155;
  8364: Result:=128;
  8482: Result:=153;
  else Result:=-1;
  end;
end;

function UnicodeToCP1255(Unicode: cardinal): integer;
begin
  case Unicode of
  0..127,129,138,140..144,154: Result:=Unicode;
  156..163: Result:=Unicode;
  165..169: Result:=Unicode;
  171..185: Result:=Unicode;
  187..191: Result:=Unicode;
  215: Result:=170;
  247: Result:=186;
  402: Result:=131;
  710: Result:=136;
  732: Result:=152;
  1456..1475: Result:=Unicode-1264;
  1488..1516: Result:=Unicode-1264;
  1517: Result:=255;
  1520..1535: Result:=Unicode-1308;
  8206..8207: Result:=Unicode-7953;
  8211..8212: Result:=Unicode-8061;
  8216..8217: Result:=Unicode-8071;
  8218: Result:=130;
  8220..8221: Result:=Unicode-8073;
  8222: Result:=132;
  8224..8225: Result:=Unicode-8090;
  8226: Result:=149;
  8230: Result:=133;
  8240: Result:=137;
  8249: Result:=139;
  8250: Result:=155;
  8362: Result:=164;
  8364: Result:=128;
  8482: Result:=153;
  else Result:=-1;
  end;
end;

function UnicodeToCP1257(Unicode: cardinal): integer;
begin
  case Unicode of
  0..127: Result:=Unicode;
  129: Result:=129;
  131: Result:=131;
  136: Result:=136;
  138: Result:=138;
  140: Result:=140;
  144: Result:=144;
  152: Result:=152;
  154: Result:=154;
  156: Result:=156;
  159..167: Result:=Unicode;
  168: Result:=141;
  169: Result:=169;
  171..174: Result:=Unicode;
  175: Result:=157;
  176..183: Result:=Unicode;
  184: Result:=143;
  185: Result:=185;
  187..190: Result:=Unicode;
  196..197: Result:=Unicode;
  198: Result:=175;
  201: Result:=201;
  211: Result:=211;
  213..215: Result:=Unicode;
  216: Result:=168;
  220: Result:=220;
  223: Result:=223;
  228..229: Result:=Unicode;
  230: Result:=191;
  233: Result:=233;
  243: Result:=243;
  245..247: Result:=Unicode;
  248: Result:=184;
  252: Result:=252;
  256: Result:=194;
  257: Result:=226;
  260: Result:=192;
  261: Result:=224;
  262: Result:=195;
  263: Result:=227;
  268: Result:=200;
  269: Result:=232;
  274: Result:=199;
  275: Result:=231;
  278: Result:=203;
  279: Result:=235;
  280: Result:=198;
  281: Result:=230;
  290: Result:=204;
  291: Result:=236;
  298: Result:=206;
  299: Result:=238;
  302: Result:=193;
  303: Result:=225;
  310: Result:=205;
  311: Result:=237;
  315: Result:=207;
  316: Result:=239;
  321: Result:=217;
  322: Result:=249;
  323: Result:=209;
  324: Result:=241;
  325: Result:=210;
  326: Result:=242;
  332: Result:=212;
  333: Result:=244;
  342: Result:=170;
  343: Result:=186;
  346: Result:=218;
  347: Result:=250;
  352: Result:=208;
  353: Result:=240;
  362: Result:=219;
  363: Result:=251;
  370: Result:=216;
  371: Result:=248;
  377: Result:=202;
  378: Result:=234;
  379: Result:=221;
  380: Result:=253;
  381: Result:=222;
  382: Result:=254;
  711: Result:=142;
  729: Result:=255;
  731: Result:=158;
  8211..8212: Result:=Unicode-8061;
  8216..8217: Result:=Unicode-8071;
  8218: Result:=130;
  8220..8221: Result:=Unicode-8073;
  8222: Result:=132;
  8224..8225: Result:=Unicode-8090;
  8226: Result:=149;
  8230: Result:=133;
  8240: Result:=137;
  8249: Result:=139;
  8250: Result:=155;
  8364: Result:=128;
  8482: Result:=153;
  else Result:=-1;
  end;
end;

function UnicodeToCP1258(Unicode: cardinal): integer;
begin
  case Unicode of
  0..127: Result:=Unicode;
  129: Result:=129;
  138: Result:=138;
  141..144: Result:=Unicode;
  154: Result:=154;
  157..158: Result:=Unicode;
  160..194: Result:=Unicode;
  196..203: Result:=Unicode;
  205..207: Result:=Unicode;
  209: Result:=209;
  211..212: Result:=Unicode;
  214..220: Result:=Unicode;
  223..226: Result:=Unicode;
  228..235: Result:=Unicode;
  237..239: Result:=Unicode;
  241: Result:=241;
  243..244: Result:=Unicode;
  246..252: Result:=Unicode;
  255: Result:=255;
  258: Result:=195;
  259: Result:=227;
  272: Result:=208;
  273: Result:=240;
  338: Result:=140;
  339: Result:=156;
  376: Result:=159;
  402: Result:=131;
  416: Result:=213;
  417: Result:=245;
  431: Result:=221;
  432: Result:=253;
  710: Result:=136;
  732: Result:=152;
  768: Result:=204;
  769: Result:=236;
  771: Result:=222;
  777: Result:=210;
  803: Result:=242;
  8211..8212: Result:=Unicode-8061;
  8216..8217: Result:=Unicode-8071;
  8218: Result:=130;
  8220..8221: Result:=Unicode-8073;
  8222: Result:=132;
  8224..8225: Result:=Unicode-8090;
  8226: Result:=149;
  8230: Result:=133;
  8240: Result:=137;
  8249: Result:=139;
  8250: Result:=155;
  8363: Result:=254;
  8364: Result:=128;
  8482: Result:=153;
  else Result:=-1;
  end;
end;

function UnicodeToCP874(Unicode: cardinal): integer;
begin
  case Unicode of
  0..127: Result:=Unicode;
  129..132: Result:=Unicode;
  134..144: Result:=Unicode;
  152..160: Result:=Unicode;
  219..222: Result:=Unicode;
  252..255: Result:=Unicode;
  3585..3642: Result:=Unicode-3424;
  3647..3675: Result:=Unicode-3424;
  8211..8212: Result:=Unicode-8061;
  8216..8217: Result:=Unicode-8071;
  8220..8221: Result:=Unicode-8073;
  8226: Result:=149;
  8230: Result:=133;
  8364: Result:=128;
  else Result:=-1;
  end;
end;

//{$if FPC_FULLVERSION >= 20701}
{$IFDEF FPC_HAS_CPSTRING}
procedure InternalUTF8ToCP(const s: string; TargetCodePage: TSystemCodePage;
  SetTargetCodePage: boolean;
  const UTF8CharConvFunc: TUnicodeToCharID;
  out TheResult: RawByteString); inline;
begin
  if not Assigned(UTF8CharConvFunc) then
  begin
    TheResult:=s;
    SetCodePage(TheResult, TargetCodePage, True);
    if not SetTargetCodePage then
      SetCodePage(TheResult, CP_ACP, False);
  end else begin
    TheResult:=UTF8ToSingleByte(s,UTF8CharConvFunc);
    if SetTargetCodePage then
      SetCodePage(TheResult, TargetCodePage, False);
  end;
end;

function UTF8ToISO_8859_1(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  InternalUTF8ToCP(s,28591,SetTargetCodePage,{$IfDef UseSystemCPConv}nil{$else}@UnicodeToISO_8859_1{$endif},Result);
end;

function UTF8ToISO_8859_2(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  InternalUTF8ToCP(s,28592,SetTargetCodePage,{$IfDef UseSystemCPConv}nil{$else}@UnicodeToISO_8859_2{$endif},Result);
end;

function UTF8ToISO_8859_15(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  InternalUTF8ToCP(s,28605,SetTargetCodePage,{$IfDef UseSystemCPConv}nil{$else}@UnicodeToISO_8859_15{$endif},Result);
end;

function UTF8ToCP1250(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  // system conversion fails for character #129 -> using table
  InternalUTF8ToCP(s,1250,SetTargetCodePage,@UnicodeToCP1250,Result);
end;

function UTF8ToCP1251(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  // system conversion fails for character #152 -> using table
  InternalUTF8ToCP(s,1251,SetTargetCodePage,@UnicodeToCP1251,Result);
end;

function UTF8ToCP1252(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  // system conversion fails for character #128 -> using table
  InternalUTF8ToCP(s,1252,SetTargetCodePage,@UnicodeToCP1252,Result);
end;

function UTF8ToCP1253(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  // system conversion fails for character #129 -> using table
  InternalUTF8ToCP(s,1253,SetTargetCodePage,@UnicodeToCP1253,Result);
end;

function UTF8ToCP1254(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  // system conversion fails for character #129 -> using table
  InternalUTF8ToCP(s,1254,SetTargetCodePage,@UnicodeToCP1254,Result);
end;

function UTF8ToCP1255(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  // system conversion fails for character #129 -> using table
  InternalUTF8ToCP(s,1255,SetTargetCodePage,@UnicodeToCP1255,Result);
end;

function UTF8ToCP1256(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  InternalUTF8ToCP(s,1256,SetTargetCodePage,{$IfDef UseSystemCPConv}nil{$else}@UnicodeToCP1256{$endif},Result);
end;

function UTF8ToCP1257(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  // system conversion fails for character #129 -> using table
  InternalUTF8ToCP(s,1257,SetTargetCodePage,@UnicodeToCP1257,Result);
end;

function UTF8ToCP1258(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  // system conversion fails for character #129 -> using table
  InternalUTF8ToCP(s,1258,SetTargetCodePage,@UnicodeToCP1258,Result);
end;

function UTF8ToCP437(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  InternalUTF8ToCP(s,437,SetTargetCodePage,{$IfDef UseSystemCPConv}nil{$else}@UnicodeToCP437{$endif},Result);
end;

function UTF8ToCP850(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  InternalUTF8ToCP(s,850,SetTargetCodePage,{$IfDef UseSystemCPConv}nil{$else}@UnicodeToCP850{$endif},Result);
end;

function UTF8ToCP852(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  InternalUTF8ToCP(s,852,SetTargetCodePage,{$IfDef UseSystemCPConv}nil{$else}@UnicodeToCP852{$endif},Result);
end;

function UTF8ToCP866(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  InternalUTF8ToCP(s,866,SetTargetCodePage,{$IfDef UseSystemCPConv}nil{$else}@UnicodeToCP866{$endif},Result);
end;

function UTF8ToCP874(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  // system conversion fails for character #129 -> using table
  InternalUTF8ToCP(s,874,SetTargetCodePage,@UnicodeToCP874,Result);
end;

function UTF8ToKOI8(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  InternalUTF8ToCP(s,20866,SetTargetCodePage,{$IfDef UseSystemCPConv}nil{$else}@UnicodeToKOI8{$endif},Result);
end;

function UTF8ToKOI8U(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  InternalUTF8ToCP(s,21866,SetTargetCodePage,{$IfDef UseSystemCPConv}nil{$else}@UnicodeToKOI8U{$endif},Result);
end;

function UTF8ToKOI8RU(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  InternalUTF8ToCP(s,21866,SetTargetCodePage,{$IfDef UseSystemCPConv}nil{$else}@UnicodeToKOI8RU{$endif},Result);
end;

function UTF8ToMacintosh(const s: string; SetTargetCodePage: boolean): RawByteString;
begin
  InternalUTF8ToCP(s,10000,SetTargetCodePage,{$IfDef UseSystemCPConv}nil{$else}@UnicodeToMacintosh{$endif},Result);
end;
{$ELSE}
function UTF8ToISO_8859_1(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToISO_8859_1);
end;

function UTF8ToISO_8859_15(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToISO_8859_15);
end;

function UTF8ToISO_8859_2(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToISO_8859_2);
end;

function UTF8ToCP1250(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP1250);
end;

function UTF8ToCP1251(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP1251);
end;

function UTF8ToCP1252(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP1252);
end;

function UTF8ToCP1253(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP1253);
end;

function UTF8ToCP1254(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP1254);
end;

function UTF8ToCP1255(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP1255);
end;

function UTF8ToCP1256(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP1256);
end;

function UTF8ToCP1257(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP1257);
end;

function UTF8ToCP1258(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP1258);
end;

function UTF8ToCP437(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP437);
end;

function UTF8ToCP850(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP850);
end;

function UTF8ToCP852(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP852);
end;

function UTF8ToCP866(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP866);
end;

function UTF8ToCP874(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP874);
end;

function UTF8ToKOI8(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToKOI8);
end;

function UTF8ToKOI8U(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToKOI8U);
end;

function UTF8ToKOI8RU(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToKOI8RU);
end;

function UTF8ToMacintosh(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToMacintosh);
end;
{$ENDIF}

function UTF8ToSingleByte(const s: string; const UTF8CharConvFunc: TUnicodeToCharID): string;
var
  len, i, CharLen: Integer;
  Src, Dest: PChar;
  c: Char;
  Unicode: LongWord;
begin
  if s='' then begin
    Result:='';
    exit;
  end;
  len:=length(s);
  SetLength(Result,len);
  Src:=PChar(s);
  Dest:=PChar(Result);
  while len>0 do begin
    c:=Src^;
    if c<#128 then begin
      Dest^:=c;
      inc(Dest);
      inc(Src);
      dec(len);
    end else begin
      Unicode:=UTF8CodepointToUnicode(Src,CharLen);
      inc(Src,CharLen);
      dec(len,CharLen);
      i:=UTF8CharConvFunc(Unicode);
      //writeln('UTF8ToSingleByte Unicode=',Unicode,' CharLen=',CharLen,' c="',copy(s,Src-PChar(s)+1-CharLen,CharLen),'" i=',i);
      if i>=0 then begin
        Dest^:=chr(i);
        inc(Dest);
      end
      else
      if ConvertEncodingFromUtf8RaisesException then
        raise EConvertError.Create('Cannot convert UTF8 to single byte');
    end;
  end;
  SetLength(Result,Dest-PChar(Result));
end;

function UTF8ToUCS2LE(const s: string): string;
var
  len: Integer;
  Src: PChar;
  Dest: PWord;
  c: Char;
  Unicode: LongWord;
  CharLen: integer;
begin
  if s='' then begin
    Result:='';
    exit;
  end;
  len:=length(s);
  SetLength(Result,len*2);
  Src:=PChar(s);
  Dest:=PWord(Pointer(Result));
  while len>0 do begin
    c:=Src^;
    if c<#128 then begin
      Dest^:=NtoLE(Word(ord(c)));
      inc(Dest);
      inc(Src);
      dec(len);
    end else begin
      Unicode:=UTF8CodepointToUnicode(Src,CharLen);
      inc(Src,CharLen);
      dec(len,CharLen);
      if Unicode<=$ffff then begin
        Dest^:=NtoLE(Word(Unicode));
        inc(Dest);
      end;
    end;
  end;
  len:={%H-}PtrUInt(Dest)-PtrUInt(Result);
  if len>length(Result) then
    raise Exception.Create('');
  SetLength(Result,len);
end;

function UTF8ToUCS2BE(const s: string): string;
var
  len: Integer;
  Src: PChar;
  Dest: PWord;
  c: Char;
  Unicode: LongWord;
  CharLen: integer;
begin
  if s='' then begin
    Result:='';
    exit;
  end;
  len:=length(s);
  SetLength(Result,len*2);
  Src:=PChar(s);
  Dest:=PWord(Pointer(Result));
  while len>0 do begin
    c:=Src^;
    if c<#128 then begin
      Dest^:=NtoBE(Word(ord(c)));
      inc(Dest);
      inc(Src);
      dec(len);
    end else begin
      Unicode:=UTF8CodepointToUnicode(Src,CharLen);
      inc(Src,CharLen);
      dec(len,CharLen);
      if Unicode<=$ffff then begin
        Dest^:=NtoBE(Word(Unicode));
        inc(Dest);
      end;
    end;
  end;
  len:={%H-}PtrUInt(Dest)-PtrUInt(Result);
  if len>length(Result) then
    raise Exception.Create('');
  SetLength(Result,len);
end;

procedure GetSupportedEncodings(List: TStrings);
begin
  List.Add('UTF-8');
  List.Add('UTF-8BOM');
  List.Add('Ansi');

  List.Add(UpperCase(EncodingCP1250));
  List.Add(UpperCase(EncodingCP1251));
  List.Add(UpperCase(EncodingCP1252));
  List.Add(UpperCase(EncodingCP1253));
  List.Add(UpperCase(EncodingCP1254));
  List.Add(UpperCase(EncodingCP1255));
  List.Add(UpperCase(EncodingCP1256));
  List.Add(UpperCase(EncodingCP1257));
  List.Add(UpperCase(EncodingCP1258));
  List.Add(UpperCase(EncodingCP437));
  List.Add(UpperCase(EncodingCP850));
  List.Add(UpperCase(EncodingCP852));
  List.Add(UpperCase(EncodingCP866));
  List.Add(UpperCase(EncodingCP874));

  {$IFnDEF DisableAsianCodePages}
  List.Add(UpperCase(EncodingCP932));
  List.Add(UpperCase(EncodingCP936));
  List.Add(UpperCase(EncodingCP949));
  List.Add(UpperCase(EncodingCP950));
  {$ENDIF}

  List.Add('ISO-8859-1');
  List.Add('ISO-8859-2');
  List.Add('ISO-8859-15');

  List.Add('KOI-8');
  List.Add('Macintosh');

  // UCS2 are less common, list them last
  List.Add('UCS-2LE');
  List.Add('UCS-2BE');
end;

function GuessEncoding(const s: string): string;

  function CompareI(p1, p2: PChar; Count: integer): boolean;
  var
    i: Integer;
    Chr1: Byte;
    Chr2: Byte;
  begin
    for i:=1 to Count do begin
      Chr1 := byte(p1^);
      Chr2 := byte(p2^);
      if Chr1<>Chr2 then begin
        if Chr1 in [97..122] then
          dec(Chr1,32);
        if Chr2 in [97..122] then
          dec(Chr2,32);
        if Chr1<>Chr2 then exit(false);
      end;
      inc(p1);
      inc(p2);
    end;
    Result:=true;
  end;

  {$IFDEF VerboseIDEEncoding}
  function PosToStr(p: integer): string;
  var
    y: Integer;
    x: Integer;
    i: Integer;
  begin
    y:=1;
    x:=1;
    i:=1;
    while (i<=length(s)) and (i<p) do begin
      if s[i] in [#10,#13] then begin
        inc(i);
        x:=1;
        inc(y);
        if (i<=length(s)) and (s[i] in [#10,#13]) and (s[i]<>s[i-1]) then
          inc(i);
      end else begin
        inc(i);
        inc(x);
      end;
    end;
    Result:='x='+IntToStr(x)+',y='+IntToStr(y);
  end;
  {$ENDIF}

var
  l: Integer;
  p: PChar;
  EndPos: PChar;
  i: LongInt;
begin
  l:=length(s);
  if l=0 then begin
    Result:='';
    exit;
  end;
  p:=PChar(s);

  // try UTF-8 BOM (Byte Order Mark)
  if CompareI(p,UTF8BOM,3) then begin
    Result:=EncodingUTF8BOM;
    exit;
  end;

  // try ucs-2le BOM FF FE (ToDo: nowadays this BOM is UTF16LE)
  if (p^=#$FF) and (p[1]=#$FE) then begin
    Result:=EncodingUCS2LE;
    exit;
  end;

  // try ucs-2be BOM FE FF (ToDo: nowadays this BOM is UTF16BE)
  if (p^=#$FE) and (p[1]=#$FF) then begin
    Result:=EncodingUCS2BE;
    exit;
  end;

  // try {%encoding eee}
  if CompareI(p,'{%encoding ',11) then begin
    inc(p,length('{%encoding '));
    while (p^ in [' ',#9]) do inc(p);
    EndPos:=p;
    while not (EndPos^ in ['}',' ',#9,#0]) do inc(EndPos);
    Result:=NormalizeEncoding(copy(s,p-PChar(s)+1,EndPos-p));
    exit;
  end;

  // try UTF-8 (this includes ASCII)
  p:=PChar(s);
  repeat
    if ord(p^)<128 then begin
      // ASCII
      if (p^=#0) and (p-PChar(s)>=l) then begin
        Result:=EncodingUTF8;
        exit;
      end;
      inc(p);
    end else begin
      i:=UTF8CodepointStrictSize(p);
      //DebugLn(['GuessEncoding ',i,' ',DbgStr(s[p])]);
      if i=0 then begin
        {$IFDEF VerboseIDEEncoding}
        DebugLn(['GuessEncoding non UTF-8 found at ',PosToStr(p-PChar(s)+1),' ',dbgstr(copy(s,p-PChar(s)-10,20))]);
        {$ENDIF}
        break;
      end;
      inc(p,i);
    end;
  until false;

  // use system encoding
  Result:=GetDefaultTextEncoding;

  if NormalizeEncoding(Result)=EncodingUTF8 then begin
    // the system encoding is UTF-8, but the text is not UTF-8
    // use ISO-8859-1 instead. This encoding has a full 1:1 mapping to unicode,
    // so no character is lost during conversion back and forth.
    Result:='ISO-8859-1';
  end;
end;


function ConvertEncodingFromUTF8(const s, ToEncoding: string; out Encoded: boolean
  {$ifdef FPC_HAS_CPSTRING}; SetTargetCodePage: boolean = false{$endif}): string;
var
  ATo: string;

  {$ifdef FPC_HAS_CPSTRING}
  procedure CheckKeepCP; inline;
  begin
    if SetTargetCodePage then
      raise Exception.Create('ConvertEncodingFromUTF8: cannot set AnsiString codepage to "'+ATo+'"');
  end;
  {$endif}

begin
  Result:=s;
  Encoded:=true;
  ATo:=NormalizeEncoding(ToEncoding);

  if ATo=EncodingUTF8BOM then begin Result:=UTF8ToUTF8BOM(s); exit; end;
  if ATo=EncodingCPIso1 then begin Result:=UTF8ToISO_8859_1(s{$ifdef FPC_HAS_CPSTRING},SetTargetCodePage{$endif}); exit; end;
  if ATo=EncodingCPIso15 then begin Result:=UTF8ToISO_8859_15(s{$ifdef FPC_HAS_CPSTRING},SetTargetCodePage{$endif}); exit; end;
  if ATo=EncodingCPIso2 then begin Result:=UTF8ToISO_8859_2(s{$ifdef FPC_HAS_CPSTRING},SetTargetCodePage{$endif}); exit; end;
  if ATo=EncodingCP1250 then begin Result:=UTF8ToCP1250(s{$ifdef FPC_HAS_CPSTRING},SetTargetCodePage{$endif}); exit; end;
  if ATo=EncodingCP1251 then begin Result:=UTF8ToCP1251(s{$ifdef FPC_HAS_CPSTRING},SetTargetCodePage{$endif}); exit; end;
  if ATo=EncodingCP1252 then begin Result:=UTF8ToCP1252(s{$ifdef FPC_HAS_CPSTRING},SetTargetCodePage{$endif}); exit; end;
  if ATo=EncodingCP1253 then begin Result:=UTF8ToCP1253(s{$ifdef FPC_HAS_CPSTRING},SetTargetCodePage{$endif}); exit; end;
  if ATo=EncodingCP1254 then begin Result:=UTF8ToCP1254(s{$ifdef FPC_HAS_CPSTRING},SetTargetCodePage{$endif}); exit; end;
  if ATo=EncodingCP1255 then begin Result:=UTF8ToCP1255(s{$ifdef FPC_HAS_CPSTRING},SetTargetCodePage{$endif}); exit; end;
  if ATo=EncodingCP1256 then begin Result:=UTF8ToCP1256(s{$ifdef FPC_HAS_CPSTRING},SetTargetCodePage{$endif}); exit; end;
  if ATo=EncodingCP1257 then begin Result:=UTF8ToCP1257(s{$ifdef FPC_HAS_CPSTRING},SetTargetCodePage{$endif}); exit; end;
  if ATo=EncodingCP1258 then begin Result:=UTF8ToCP1258(s{$ifdef FPC_HAS_CPSTRING},SetTargetCodePage{$endif}); exit; end;
  if ATo=EncodingCP437 then begin Result:=UTF8ToCP437(s{$ifdef FPC_HAS_CPSTRING},SetTargetCodePage{$endif}); exit; end;
  if ATo=EncodingCP850 then begin Result:=UTF8ToCP850(s{$ifdef FPC_HAS_CPSTRING},SetTargetCodePage{$endif}); exit; end;
  if ATo=EncodingCP852 then begin Result:=UTF8ToCP852(s{$ifdef FPC_HAS_CPSTRING},SetTargetCodePage{$endif}); exit; end;
  if ATo=EncodingCP866 then begin Result:=UTF8ToCP866(s{$ifdef FPC_HAS_CPSTRING},SetTargetCodePage{$endif}); exit; end;
  if ATo=EncodingCP874 then begin Result:=UTF8ToCP874(s{$ifdef FPC_HAS_CPSTRING},SetTargetCodePage{$endif}); exit; end;
  {$IFnDEF DisableAsianCodePages}
  if ATo=EncodingCP936 then begin Result:=UTF8ToCP936(s{$ifdef FPC_HAS_CPSTRING},SetTargetCodePage{$endif}); exit; end;
  if ATo=EncodingCP950 then begin Result:=UTF8ToCP950(s{$ifdef FPC_HAS_CPSTRING},SetTargetCodePage{$endif}); exit; end;
  if ATo=EncodingCP949 then begin Result:=UTF8ToCP949(s{$ifdef FPC_HAS_CPSTRING},SetTargetCodePage{$endif}); exit; end;
  if ATo=EncodingCP932 then begin Result:=UTF8ToCP932(s{$ifdef FPC_HAS_CPSTRING},SetTargetCodePage{$endif}); exit; end;
  {$ENDIF}
  if ATo=EncodingCPKOI8 then begin Result:=UTF8ToKOI8(s{$ifdef FPC_HAS_CPSTRING},SetTargetCodePage{$endif}); exit; end;
  if ATo=EncodingCPMac then begin Result:=UTF8ToMacintosh(s{$ifdef FPC_HAS_CPSTRING},SetTargetCodePage{$endif}); exit; end;
  if ATo=EncodingUCS2LE then begin {$ifdef FPC_HAS_CPSTRING}CheckKeepCP;{$endif} Result:=UTF8ToUCS2LE(s); exit; end;
  if ATo=EncodingUCS2BE then begin {$ifdef FPC_HAS_CPSTRING}CheckKeepCP;{$endif} Result:=UTF8ToUCS2BE(s); exit; end;

  if (ATo=GetDefaultTextEncoding) and Assigned(ConvertUTF8ToAnsi) then begin
    Result:=ConvertUTF8ToAnsi(s);
    exit;
  end;

  Encoded:= false;
end;

function ConvertEncodingToUTF8(const s, FromEncoding: string; out Encoded: boolean): string;
var
  AFrom: string;
begin
  Result:=s;
  Encoded:=true;
  AFrom:=NormalizeEncoding(FromEncoding);

  if AFrom=EncodingUTF8BOM then begin Result:=UTF8BOMToUTF8(s); exit; end;
  if AFrom=EncodingCPIso1 then begin Result:=ISO_8859_1ToUTF8(s); exit; end;
  if AFrom=EncodingCPIso15 then begin Result:=ISO_8859_15ToUTF8(s); exit; end;
  if AFrom=EncodingCPIso2 then begin Result:=ISO_8859_2ToUTF8(s); exit; end;
  if AFrom=EncodingCP1250 then begin Result:=CP1250ToUTF8(s); exit; end;
  if AFrom=EncodingCP1251 then begin Result:=CP1251ToUTF8(s); exit; end;
  if AFrom=EncodingCP1252 then begin Result:=CP1252ToUTF8(s); exit; end;
  if AFrom=EncodingCP1253 then begin Result:=CP1253ToUTF8(s); exit; end;
  if AFrom=EncodingCP1254 then begin Result:=CP1254ToUTF8(s); exit; end;
  if AFrom=EncodingCP1255 then begin Result:=CP1255ToUTF8(s); exit; end;
  if AFrom=EncodingCP1256 then begin Result:=CP1256ToUTF8(s); exit; end;
  if AFrom=EncodingCP1257 then begin Result:=CP1257ToUTF8(s); exit; end;
  if AFrom=EncodingCP1258 then begin Result:=CP1258ToUTF8(s); exit; end;
  if AFrom=EncodingCP437 then begin Result:=CP437ToUTF8(s); exit; end;
  if AFrom=EncodingCP850 then begin Result:=CP850ToUTF8(s); exit; end;
  if AFrom=EncodingCP852 then begin Result:=CP852ToUTF8(s); exit; end;
  if AFrom=EncodingCP866 then begin Result:=CP866ToUTF8(s); exit; end;
  if AFrom=EncodingCP874 then begin Result:=CP874ToUTF8(s); exit; end;
  {$IFnDEF DisableAsianCodePages}
  if AFrom=EncodingCP936 then begin Result:=CP936ToUTF8(s); exit; end;
  if AFrom=EncodingCP950 then begin Result:=CP950ToUTF8(s); exit; end;
  if AFrom=EncodingCP949 then begin Result:=CP949ToUTF8(s); exit; end;
  if AFrom=EncodingCP932 then begin Result:=CP932ToUTF8(s); exit; end;
  {$ENDIF}
  if AFrom=EncodingCPKOI8 then begin Result:=KOI8ToUTF8(s); exit; end;
  if AFrom=EncodingCPMac then begin Result:=MacintoshToUTF8(s); exit; end;
  if AFrom=EncodingUCS2LE then begin Result:=UCS2LEToUTF8(s); exit; end;
  if AFrom=EncodingUCS2BE then begin Result:=UCS2BEToUTF8(s); exit; end;

  if (AFrom=GetDefaultTextEncoding) and Assigned(ConvertAnsiToUTF8) then begin
    Result:=ConvertAnsiToUTF8(s);
    exit;
  end;

  Encoded:= false;
end;

function ConvertEncoding(const s, FromEncoding, ToEncoding: string
  {$ifdef FPC_HAS_CPSTRING}; SetTargetCodePage: boolean{$endif}): string;
var
  AFrom, ATo, SysEnc : String;
  Encoded : Boolean;
  {$ifdef EnableIconvEnc}
  Dummy: String;
  {$endif}
begin
  AFrom:=NormalizeEncoding(FromEncoding);
  ATo:=NormalizeEncoding(ToEncoding);
  SysEnc:=GetDefaultTextEncoding;
  if AFrom=EncodingAnsi then AFrom:=SysEnc
  else if AFrom='' then AFrom:=EncodingUTF8;
  if ATo=EncodingAnsi then ATo:=SysEnc
  else if ATo='' then ATo:=EncodingUTF8;
  if AFrom=ATo then begin
    Result:=s;
    exit;
  end;
  if s='' then begin
    if ATo=EncodingUTF8BOM then
      Result:=UTF8BOM
    else Result := s;
    exit;
  end;
  //DebugLn(['ConvertEncoding ',AFrom,' ',ATo]);

  if AFrom=EncodingUTF8 then begin
    Result:=ConvertEncodingFromUTF8(s, ATo, Encoded{$ifdef FPC_HAS_CPSTRING}, SetTargetCodePage{$endif});
    if Encoded then exit;
  end
  else
  if ATo=EncodingUTF8 then begin
    Result:=ConvertEncodingToUTF8(s, AFrom, Encoded);
    if Encoded then exit;
  end
  else
  begin
    Result:=ConvertEncodingToUTF8(s, AFrom, Encoded);
    if Encoded then
      Result:=ConvertEncodingFromUTF8(Result, ATo, Encoded{$ifdef FPC_HAS_CPSTRING}, SetTargetCodePage{$endif});
    if Encoded then exit;
  end;

  //cannot encode: return orig str
  Result:=s;

  {$ifdef EnableIconvEnc}
  try
    if not IconvLibFound and not InitIconv(Dummy) then
    begin
      {$IFNDEF DisableChecks}
      DebugLn(['Can not init iconv: ',Dummy]);
      {$ENDIF}
      Exit;
    end;
    if Iconvert(s, Result, AFrom, ATo)<>0 then
    begin
      Result:=s;
      Exit;
    end;
  except
  end;
  {$endif}
end;

end.
