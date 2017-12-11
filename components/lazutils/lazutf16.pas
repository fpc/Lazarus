{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  This unit includes string routines which are based on UTF-16 implementations,
  although it might also include routines for other encodings.

  A UTF-16 based implementation for LowerCase, for example, is faster in WideString
  and UnicodeString then the default UTF-8 implementation.

  Currently this unit includes only UTF8LowerCaseViaTables which is based on a
  UTF-16 table, but it might be extended to include various UTF-16 routines.
}
unit LazUTF16;

{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils
{$IFDEF FPC}
  ,lazutf8
{$ENDIF}
  ;

{$IFnDEF FPC}
type
  PtrInt = NativeInt;
{$ENDIF}

function UTF16CharacterLength(p: PWideChar): integer;
function UTF16Length(const s: widestring): PtrInt; overload;
function UTF16Length(p: PWideChar; WordCount: PtrInt): PtrInt; overload;
function UTF16Copy(const s: UnicodeString; StartCharIndex, CharCount: PtrInt): Unicodestring;
function UTF16CharStart(P: PWideChar; Len, CharIndex: PtrInt): PWideChar;
function UTF16Pos(const SearchForText, SearchInText: UnicodeString; StartPos: PtrInt = 1): PtrInt;
function UTF16CharacterToUnicode(p: PWideChar; out CharLen: integer): Cardinal;
function UnicodeToUTF16(u: cardinal): widestring;
function IsUTF16CharValid(AChar, ANextChar: WideChar): Boolean;
function IsUTF16StringValid(AWideStr: widestring): Boolean;
function Utf16StringReplace(const S, OldPattern, NewPattern: WideString; Flags: TReplaceFlags): WideString;

function UnicodeLowercase(u: cardinal): cardinal;
{$IFDEF FPC}
function UTF8LowerCaseViaTables(const s: string): string;
{$ENDIF}

implementation

function UTF16CharacterLength(p: PWideChar): integer;
// returns length of UTF16 character in number of words
// The endianess of the machine will be taken.
begin
  if p<>nil then begin
    if (ord(p[0]) < $D800) or (ord(p[0]) > $DFFF) then
      Result:=1
    else
      Result:=2;
  end else begin
    Result:=0;
  end;
end;

function UTF16Length(const s: widestring): PtrInt;
begin
  Result:=UTF16Length(PWideChar(s),length(s));
end;

function UTF16Length(p: PWideChar; WordCount: PtrInt): PtrInt;
var
  CharLen: LongInt;
begin
  Result:=0;
  while (WordCount>0) do begin
    inc(Result);
    CharLen:=UTF16CharacterLength(p);
    inc(p,CharLen);
    dec(WordCount,CharLen);
  end;
end;

function UTF16Copy(const s: UnicodeString; StartCharIndex, CharCount: PtrInt): Unicodestring;
// returns substring
var
  StartPos: PWideChar;
  EndPos: PWideChar;
  MaxBytes: PtrInt;
begin
  StartPos:=UTF16CharStart(PWideChar(s),length(s),StartCharIndex-1);
  if StartPos=nil then
    Result:=''
  else begin
    MaxBytes:=PtrInt(PWideChar(s)+length(s)-StartPos);
    EndPos:=UTF16CharStart(StartPos,MaxBytes,CharCount);
    if EndPos=nil then
      Result:=copy(s,StartPos-PWideChar(s)+1,MaxBytes)
    else
      Result:=copy(s,StartPos-PWideChar(s)+1,EndPos-StartPos);
  end;
end;

function UTF16CharStart(P: PWideChar; Len, CharIndex: PtrInt): PWideChar;
// Len is the length in words of P.
// CharIndex is the position of the desired UnicodeChar (starting at 0).
var
  CharLen: LongInt;
begin
  Result:=P;
  if Result=nil then Exit;
  while (CharIndex>0) and (Len>0) do
  begin
    CharLen:=UTF16CharacterLength(Result);
    dec(Len,CharLen);
    dec(CharIndex);
    inc(Result,CharLen);
  end;
  if (CharIndex<>0) or (Len<0) then
    Result:=nil;
end;

function IndexOfWideChar(Const Buf: PWideChar; Len: PtrInt; b: WideChar): PtrInt;
begin
  for Result:=0 to len-1 do
    if buf[Result]=b then
      Exit;
  Result:=-1;
end;

// Helper used by UTF16Pos
function UTF16PosP(SearchForText: PWideChar; SearchForTextLen: PtrInt;
  SearchInText: PWideChar; SearchInTextLen: PtrInt): PWideChar;
// returns the position where SearchInText starts in SearchForText
// returns nil if not found
var
  p: PtrInt;
begin
  Result:=nil;
  if (SearchForText=nil) or (SearchForTextLen=0) or (SearchInText=nil) then
    exit;
  while SearchInTextLen>0 do begin
    p:=IndexOfWideChar(SearchInText,SearchInTextLen,SearchForText^);
    if p<0 then exit;
    inc(SearchInText, p);
    dec(SearchInTextLen, p);
    if SearchInTextLen<SearchForTextLen then exit;
    if CompareMem(SearchInText,SearchForText,SearchForTextLen * 2) then
      exit(SearchInText);
    inc(SearchInText);
    dec(SearchInTextLen);
  end;
end;

function UTF16Pos(const SearchForText, SearchInText: UnicodeString; StartPos: PtrInt = 1): PtrInt;
// returns the character index, where the SearchForText starts in SearchInText
// an optional StartPos can be given (in UTF-16 codepoints, not in word)
// returns 0 if not found
var
  i: PtrInt;
  p: PWideChar;
  StartPosP: PWideChar;
begin
  Result:=0;
  if StartPos=1 then
  begin
    i:=System.Pos(SearchForText,SearchInText);
    if i>0 then
      Result:=UTF16Length(PWideChar(SearchInText),i-1)+1;
  end
  else if StartPos>1 then
  begin
    // skip
    StartPosP:=UTF16CharStart(PWideChar(SearchInText),Length(SearchInText),StartPos-1);
    if StartPosP=nil then exit;
    // search
    p:=UTF16PosP(PWideChar(SearchForText),length(SearchForText),
                StartPosP,length(SearchInText)+PWideChar(SearchInText)-StartPosP);
    // get UTF-8 position
    if p=nil then exit;
    Result:=StartPos+UTF16Length(StartPosP,p-StartPosP);
  end;
end;

function UTF16CharacterToUnicode(p: PWideChar; out CharLen: integer): Cardinal;
var
  w1: cardinal;
  w2: Cardinal;
begin
  if p<>nil then begin
    w1:=ord(p[0]);
    if (w1 < $D800) or (w1 > $DFFF) then begin
      // is 1 word character
      Result:=w1;
      CharLen:=1;
    end else begin
      // could be 2 word character
      w2:=ord(p[1]);
      if (w2>=$DC00) then begin
        // is 2 word character
        Result:=(w1-$D800) shl 10 + (w2-$DC00) + $10000;
        CharLen:=2;
      end else begin
        // invalid character
        Result:=w1;
        CharLen:=1;
      end;
    end;
  end else begin
    Result:=0;
    CharLen:=0;
  end;
end;

function UnicodeToUTF16(u: cardinal): widestring;
begin
  // u should be <= $10FFFF to fit into UTF-16

  if u < $10000 then
    // Note: codepoints $D800 - $DFFF are reserved
    Result:=system.widechar(u)
  else
    Result:=system.widechar($D800+((u - $10000) shr 10))+system.widechar($DC00+((u - $10000) and $3ff));
end;

// Specification here: http://unicode.org/faq/utf_bom.html#utf16-7
// Q: Are there any 16-bit values that are invalid?
// A: Unpaired surrogates are invalid in UTFs. These include any value in the
// range D800 to DBFF not followed by a value in the range DC00 to DFFF,
// or any value in the range DC00 to DFFF not preceded by a value in the range D800 to DBFF. [AF]
//
// Use ANextChar = #0 to indicate that there is no next char
function IsUTF16CharValid(AChar, ANextChar: WideChar): Boolean;
begin
  if AChar = #0 then Exit(False);
  Result := ((AChar >= #$D800) and (AChar <= #$DBFF)) and not ((ANextChar >= #$DC00) and (ANextChar <= #$DFFF));
  //Result := (Word(AChar) in [$D800..$DBFF]) and not (Word(ANextChar) in [$DC00..$DFFF]); <= generates range check error
  Result := not Result;
end;

function IsUTF16StringValid(AWideStr: widestring): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to Length(AWideStr)-1 do
  begin
    Result := Result and IsUTF16CharValid(AWideStr[i], AWideStr[i+1]);
    if not Result then Exit;
  end;
end;

//Same as SysUtil.StringReplace but for WideStrings/UnicodeStrings, since it's not available in fpc yet
function Utf16StringReplace(const S, OldPattern, NewPattern: WideString;  Flags: TReplaceFlags): WideString;
var
  Srch, OldP, RemS: WideString; // Srch and OldP can contain WideUpperCase versions of S,OldPattern
  P: Integer;
begin
  Srch:=S;
  OldP:=OldPattern;
  if rfIgnoreCase in Flags then
  begin
    Srch:=WideUpperCase(Srch);
    OldP:=WideUpperCase(OldP);
  end;
  RemS:=S;
  Result:='';
  while (Length(Srch)<>0) do
  begin
    P:=Pos(OldP, Srch);
    if P=0 then
    begin
      Result:=Result+RemS;
      Srch:='';
    end
    else
    begin
      Result:=Result+Copy(RemS,1,P-1)+NewPattern;
      P:=P+Length(OldP);
      RemS:=Copy(RemS,P,Length(RemS)-P+1);
      if not (rfReplaceAll in Flags) then
      begin
        Result:=Result+RemS;
        Srch:='';
      end
      else
        Srch:=Copy(Srch,P,Length(Srch)-P+1);
    end;
  end;
end;


// Lowercase Unicode Tables which match UTF-16 but also UTF-32
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

{$IFDEF FPC}
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
        OldCode:=UTF8CodepointToUnicode(@s[SrcPos],CharLen);
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
        OldCode:=UTF8CodepointToUnicode(@s[SrcPos],CharLen);
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

function UTF8LowerCaseViaTables(const s: string): string;
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
        OldCode:=UTF8CodepointToUnicode(p,CharLen);
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
{$ENDIF}

initialization
  InitUnicodeTables;
end.

