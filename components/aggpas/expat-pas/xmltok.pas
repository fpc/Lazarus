//----------------------------------------------------------------------------
// Copyright (c) 1998, 1999, 2000 Thai Open Source Software Center Ltd
//                                and Clark Cooper
// Copyright (c) 2001, 2002, 2003, 2004, 2005, 2006 Expat maintainers.
//
// Expat - Version 2.0.0 Release Milano 0.83 (PasExpat 2.0.0 RM0.83)
// Pascal Port By: Milan Marusinec alias Milano
//                 milan@marusinec.sk
//                 http://www.pasports.org/pasexpat
// Copyright (c) 2006
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
// [Pascal Port History] -----------------------------------------------------
//
// 10.05.2006-Milano: Unit port establishment
// 17.05.2006-Milano: Interface part
// 06.06.2006-Milano: porting
// 07.06.2006-Milano: -"-
// 09.06.2006-Milano: -"-
// 22.06.2006-Milano: -"-
//
{ xmltok.pas }
unit
 xmltok ;

INTERFACE

uses
 expat_basics ,
 expat_external ;

{$I expat_mode.inc }

{ CONSTANTS DEFINITION }
const
{ The following token may be returned by XmlContentTok }
 XML_TOK_TRAILING_RSQB = -5; { ] or ]] at the end of the scan; might be
                               start of illegal ]]> sequence }

{ The following tokens may be returned by both XmlPrologTok and XmlContentTok. }
 XML_TOK_NONE         = -4;  { The string to be scanned is empty }
 XML_TOK_TRAILING_CR  = -3;  { A CR at the end of the scan;
                                    might be part of CRLF sequence }
 XML_TOK_PARTIAL_CHAR = -2;  { only part of a multibyte sequence }
 XML_TOK_PARTIAL      = -1;  { only part of a token }
 XML_TOK_INVALID      = 0;

{ The following tokens are returned by XmlContentTok; some are also
  returned by XmlAttributeValueTok, XmlEntityTok, XmlCdataSectionTok. }
 XML_TOK_START_TAG_WITH_ATTS     = 1;
 XML_TOK_START_TAG_NO_ATTS       = 2;
 XML_TOK_EMPTY_ELEMENT_WITH_ATTS = 3;  { empty element tag <e/> }
 XML_TOK_EMPTY_ELEMENT_NO_ATTS   = 4;
 XML_TOK_END_TAG                 = 5;
 XML_TOK_DATA_CHARS              = 6;
 XML_TOK_DATA_NEWLINE            = 7;
 XML_TOK_CDATA_SECT_OPEN         = 8;
 XML_TOK_ENTITY_REF              = 9;
 XML_TOK_CHAR_REF                = 10; { numeric character reference }

{ The following tokens may be returned by both XmlPrologTok and XmlContentTok. }
 XML_TOK_PI       = 11;                { processing instruction }
 XML_TOK_XML_DECL = 12;                { XML decl or text decl }
 XML_TOK_COMMENT  = 13;
 XML_TOK_BOM      = 14;                { Byte order mark }

{ The following tokens are returned only by XmlPrologTok }
 XML_TOK_PROLOG_S         = 15;
 XML_TOK_DECL_OPEN        = 16;        { <!foo }
 XML_TOK_DECL_CLOSE       = 17;        { > }
 XML_TOK_NAME             = 18;
 XML_TOK_NMTOKEN          = 19;
 XML_TOK_POUND_NAME       = 20;        { #name }
 XML_TOK_OR               = 21;        { | }
 XML_TOK_PERCENT          = 22;
 XML_TOK_OPEN_PAREN       = 23;
 XML_TOK_CLOSE_PAREN      = 24;
 XML_TOK_OPEN_BRACKET     = 25;
 XML_TOK_CLOSE_BRACKET    = 26;
 XML_TOK_LITERAL          = 27;
 XML_TOK_PARAM_ENTITY_REF = 28;
 XML_TOK_INSTANCE_START   = 29;

{ The following occur only in element type declarations }
 XML_TOK_NAME_QUESTION        = 30;    { name? }
 XML_TOK_NAME_ASTERISK        = 31;    { name* }
 XML_TOK_NAME_PLUS            = 32;    { name+ }
 XML_TOK_COND_SECT_OPEN       = 33;    { <![ }
 XML_TOK_COND_SECT_CLOSE      = 34;    { ]]> }
 XML_TOK_CLOSE_PAREN_QUESTION = 35;    { )? }
 XML_TOK_CLOSE_PAREN_ASTERISK = 36;    { )* }
 XML_TOK_CLOSE_PAREN_PLUS     = 37;    { )+ }
 XML_TOK_COMMA                = 38;

{ The following token is returned only by XmlAttributeValueTok }
 XML_TOK_ATTRIBUTE_VALUE_S = 39;

{ The following token is returned only by XmlCdataSectionTok }
 XML_TOK_CDATA_SECT_CLOSE = 40;

{ With namespace processing this is returned by XmlPrologTok for a
  name with a colon. }
 XML_TOK_PREFIXED_NAME = 41;

{$IFDEF XML_DTD }
 XML_TOK_IGNORE_SECT = 42;

{$ENDIF }

{$IFDEF XML_DTD }
 XML_N_STATES = 4;

{$ELSE }
 XML_N_STATES = 3;

{$ENDIF }

 XML_PROLOG_STATE        = 0;
 XML_CONTENT_STATE       = 1;
 XML_CDATA_SECTION_STATE = 2;

{$IFDEF XML_DTD }
 XML_IGNORE_SECTION_STATE = 3;

{$ENDIF }

 XML_N_LITERAL_TYPES         = 2;
 XML_ATTRIBUTE_VALUE_LITERAL = 0;
 XML_ENTITY_VALUE_LITERAL    = 1;

{ The size of the buffer passed to XmlUtf8Encode must be at least this. }
 XML_UTF8_ENCODE_MAX = 4;

{ The size of the buffer passed to XmlUtf16Encode must be at least this. }
 XML_UTF16_ENCODE_MAX = 2;

{ TYPES DEFINITION }
type
 POSITION_ptr = ^POSITION;
 POSITION = record
  { first line and first column are 0 not 1 }
   lineNumber   ,
   columnNumber : XML_Size;

  end;

 ATTRIBUTE_ptr = ^ATTRIBUTE;
 ATTRIBUTE = record
   name       ,
   valuePtr   ,
   valueEnd   : char_ptr;
   normalized : char;

  end;

 ENCODING_ptr_ptr = ^ENCODING_ptr;
 ENCODING_ptr = ^ENCODING;

 SCANNER = function(p1 : ENCODING_ptr; p2 ,p3 : char_ptr; p4 : char_ptr_ptr ) : int;

 ENCODING = record
   scanners        : array[0..XML_N_STATES - 1 ] of SCANNER;
   literalScanners : array[0..XML_N_LITERAL_TYPES - 1 ] of SCANNER;

   sameName             : function (p1 : ENCODING_ptr; p2 ,p3 : char_ptr ) : int;
   nameMatchesAscii     : function (p1 : ENCODING_ptr; p2 ,p3 ,p4 : char_ptr ) : int;
   nameLength           : function (p1 : ENCODING_ptr; p2 : char_ptr ) : int;
   skipS                : function (p1 : ENCODING_ptr; p2 : char_ptr ) : char_ptr;
   getAtts              : function (enc : ENCODING_ptr; ptr : char_ptr; attsMax : int; atts : ATTRIBUTE_ptr ) : int;
   charRefNumber        : function (enc : ENCODING_ptr; ptr : char_ptr ) : int;
   predefinedEntityName : function (p1 : ENCODING_ptr; p2 ,p3 : char_ptr ) : int;
   updatePosition       : procedure(p1 : ENCODING_ptr; ptr ,end_ : char_ptr; p4 : POSITION_ptr );
   isPublicId           : function (enc : ENCODING_ptr; ptr ,end_ : char_ptr; badPtr : char_ptr_ptr ) : int;
   utf8Convert          : procedure(enc : ENCODING_ptr; fromP : char_ptr_ptr; fromLim : char_ptr; toP : char_ptr_ptr; toLim : char_ptr );
   utf16Convert         : procedure(enc : ENCODING_ptr; fromP : char_ptr_ptr; fromLim : char_ptr; toP : int16u_ptr_ptr; toLim : int16u_ptr );

   minBytesPerChar : int;

   isUtf8  ,
   isUtf16 : char;

  end;

 INIT_ENCODING_ptr = ^INIT_ENCODING;
 INIT_ENCODING = record
   initEnc : ENCODING;
   encPtr  : ENCODING_ptr_ptr;

  end;

{ GLOBAL PROCEDURES }
 function  XmlInitEncoding  (p : INIT_ENCODING_ptr; encPtr : ENCODING_ptr_ptr; name : char_ptr ) : int;
 function  XmlInitEncodingNS(p : INIT_ENCODING_ptr; encPtr : ENCODING_ptr_ptr; name : char_ptr ) : int;

 function  XmlGetInternalEncoding : ENCODING_ptr;
 function  XmlGetInternalEncodingNS : ENCODING_ptr;

 function  XmlTok_      (enc : ENCODING_ptr; state : int; ptr, end_ : char_ptr; nextTokPtr : char_ptr_ptr ) : int;
 function  XmlPrologTok (enc : ENCODING_ptr; ptr, end_ : char_ptr; nextTokPtr : char_ptr_ptr ) : int;
 function  XmlContentTok(enc : ENCODING_ptr; ptr, end_ : char_ptr; nextTokPtr : char_ptr_ptr ) : int;
 function  XmlIsPublicId(enc : ENCODING_ptr; ptr ,end_ : char_ptr; badPtr : char_ptr_ptr ) : int;

 procedure XmlUtf8Convert (enc : ENCODING_ptr; fromP : char_ptr_ptr; fromLim : char_ptr; toP : char_ptr_ptr; toLim : char_ptr );
 procedure XmlUtf16Convert(enc : ENCODING_ptr; fromP : char_ptr_ptr; fromLim : char_ptr; toP : int16u_ptr_ptr; toLim : int16u_ptr );

 function  XmlUtf8Encode (charNumber : int; buf : char_ptr ) : int;
 function  XmlUtf16Encode(charNumber : int; buf : int16u_ptr ) : int;

{ This is used for performing a 2nd-level tokenization on the content
  of a literal that has already been returned by XmlTok. }
 function  XmlLiteralTok          (enc : ENCODING_ptr; literalType : int; ptr ,end_ : char_ptr; nextTokPtr : char_ptr_ptr ) : int;
 function  XmlAttributeValueTok   (enc : ENCODING_ptr; ptr ,end_ : char_ptr; nextTokPtr : char_ptr_ptr ) : int;
 function  XmlEntityValueTok      (enc : ENCODING_ptr; ptr ,end_ : char_ptr; nextTokPtr : char_ptr_ptr ) : int;
 function  XmlSameName            (enc : ENCODING_ptr; ptr1 ,ptr2 : char_ptr ) : int;
 function  XmlNameMatchesAscii    (enc : ENCODING_ptr; ptr1 ,end1 ,ptr2 : char_ptr ) : int;
 function  XmlNameLength          (enc : ENCODING_ptr; ptr : char_ptr ) : int;
 function  XmlGetAttributes       (enc : ENCODING_ptr; ptr : char_ptr; attsMax : int; atts : ATTRIBUTE_ptr ) : int;
 function  XmlCharRefNumber       (enc : ENCODING_ptr; ptr : char_ptr ) : int;
 function  XmlPredefinedEntityName(enc : ENCODING_ptr; ptr ,end_ : char_ptr ) : int;

 function  XmlParseXmlDecl(
            isGeneralTextEntity : int;
            enc : ENCODING_ptr;
            ptr ,end_ : char_ptr;
            badPtr ,versionPtr ,versionEndPtr ,encodingNamePtr : char_ptr_ptr;
            namedEncodingPtr : ENCODING_ptr_ptr;
            standalonePtr : int_ptr ) : int;

 function  XmlParseXmlDeclNS(
            isGeneralTextEntity : int;
            enc : ENCODING_ptr;
            ptr ,end_ : char_ptr;
            badPtr ,versionPtr ,versionEndPtr ,encodingNamePtr : char_ptr_ptr;
            namedEncodingPtr : ENCODING_ptr_ptr;
            standalonePtr : int_ptr ) : int;

IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
type
 encodingFinder_func = function(enc : ENCODING_ptr; ptr ,end_ : char_ptr ) : ENCODING_ptr;

const
{$I ascii.inc }

 KW_version : array[0..7 ] of char = (
  ASCII_vl ,ASCII_el ,ASCII_rl ,ASCII_sl ,ASCII_il ,ASCII_ol ,ASCII_nl ,#0 );

 KW_encoding : array[0..8 ] of char = (
  ASCII_el ,ASCII_nl ,ASCII_cl ,ASCII_ol ,ASCII_dl ,ASCII_il ,ASCII_nl ,ASCII_gl ,#0 );

 KW_standalone : array[0..10 ] of char = (
  ASCII_sl ,ASCII_tl ,ASCII_al ,ASCII_nl ,ASCII_dl ,ASCII_al ,ASCII_ll ,ASCII_ol ,
  ASCII_nl ,ASCII_el ,#0 );

 KW_yes : array[0..3 ] of char = (ASCII_yl ,ASCII_el ,ASCII_sl ,#0 );

 KW_no : array[0..2 ] of char = (ASCII_nl ,ASCII_ol ,#0 );

{ MINBPC }
function MINBPC(enc : ENCODING_ptr ) : int;
begin
{$IFDEF XML_MIN_SIZE }
 result:=enc.minBytesPerChar;

{$ELSE }
 result:=1;

{$ENDIF }

end;

{ utf8_toUtf8 }{unicode}
procedure utf8_toUtf8(enc : ENCODING_ptr; fromP : char_ptr_ptr; fromLim : char_ptr; toP : char_ptr_ptr; toLim : char_ptr );
var
 to_ ,from : char_ptr;

begin
{ Avoid copying partial characters. }
 if ptrcomp(fromLim ) - ptrcomp(fromP^ ) > ptrcomp(toLim ) - ptrcomp(toP^) then
  begin
   fromLim:=char_ptr(ptrcomp(fromP^ ) + (ptrcomp(toLim ) - ptrcomp(toP^ ) ) );

   while ptrcomp(fromLim ) > ptrcomp(fromP^ ) do
    begin
     if int8u(char_ptr(ptrcomp(fromLim ) -1 )^ ) and $c0 <> $80 then
      break;

     dec(ptrcomp(fromLim ) );

    end;

  end;

 to_ :=toP^;
 from:=fromP^;

 while ptrcomp(from ) <> ptrcomp(fromLim ) do
  begin
   to_^:=from^;

   inc(ptrcomp(from ) );
   inc(ptrcomp(to_ ) );

  end;

 fromP^:=from;
 toP^  :=to_;

end;

{ utf8_toUtf16 {..}{unicode}
procedure utf8_toUtf16(enc : ENCODING_ptr; fromP : char_ptr_ptr; fromLim : char_ptr; toP : int16u_ptr_ptr; toLim : int16u_ptr );
begin
end;

{ sb_byteType {..}
function sb_byteType(enc : ENCODING_ptr; p : char_ptr ) : int;
begin
end;

{ isNever }
function isNever(enc : ENCODING_ptr; p : char_ptr ) : int;
begin
 result:=0;

end;

{ sb_byteToAscii }
function sb_byteToAscii(enc : ENCODING_ptr; p : char_ptr ) : int;
begin
 result:=int(p^ );

end;

{ sb_charMatches }
function sb_charMatches(enc : ENCODING_ptr; p : char_ptr; c : int ) : int;
begin
 result:=int(int(p^ ) = c );

end;

{ utf8_isName2 {..}
function utf8_isName2(enc : ENCODING_ptr; p : char_ptr ) : int;
begin
end;

{ utf8_isName3 {..}
function utf8_isName3(enc : ENCODING_ptr; p : char_ptr ) : int;
begin
end;

{ utf8_isNmstrt2 {..}
function utf8_isNmstrt2(enc : ENCODING_ptr; p : char_ptr ) : int;
begin
end;

{ utf8_isNmstrt3 {..}
function utf8_isNmstrt3(enc : ENCODING_ptr; p : char_ptr ) : int;
begin
end;

{ utf8_isInvalid2 {..}
function utf8_isInvalid2(enc : ENCODING_ptr; p : char_ptr ) : int;
begin
end;

{ utf8_isInvalid3 {..}
function utf8_isInvalid3(enc : ENCODING_ptr; p : char_ptr ) : int;
begin
end;

{ utf8_isInvalid4 {..}
function utf8_isInvalid4(enc : ENCODING_ptr; p : char_ptr ) : int;
begin
end;

{ LOCAL VARIABLES & CONSTANTS }
type
 normal_encoding_ptr = ^normal_encoding;
 normal_encoding = record
   enc   : ENCODING;
   type_ : array[0..255 ] of int8u;

  {$IFDEF XML_MIN_SIZE }
   byteType    : function(p1 : ENCODING_ptr; p2 : char_ptr ) : int;
   isNameMin   : function(p1 : ENCODING_ptr; p2 : char_ptr ) : int;
   isNmstrtMin : function(p1 : ENCODING_ptr; p2 : char_ptr ) : int;
   byteToAscii : function(p1 : ENCODING_ptr; p2 : char_ptr ) : int;
   charMatches : function(p1 : ENCODING_ptr; p2 : char_ptr; p3 : int ) : int;

  {$ENDIF }
   isName2    : function(p1 : ENCODING_ptr; p2 : char_ptr ) : int;
   isName3    : function(p1 : ENCODING_ptr; p2 : char_ptr ) : int;
   isName4    : function(p1 : ENCODING_ptr; p2 : char_ptr ) : int;
   isNmstrt2  : function(p1 : ENCODING_ptr; p2 : char_ptr ) : int;
   isNmstrt3  : function(p1 : ENCODING_ptr; p2 : char_ptr ) : int;
   isNmstrt4  : function(p1 : ENCODING_ptr; p2 : char_ptr ) : int;
   isInvalid2 : function(p1 : ENCODING_ptr; p2 : char_ptr ) : int;
   isInvalid3 : function(p1 : ENCODING_ptr; p2 : char_ptr ) : int;
   isInvalid4 : function(p1 : ENCODING_ptr; p2 : char_ptr ) : int;

  end;

const
 BT_NONXML   = 0;
 BT_MALFORM  = 1;
 BT_LT       = 2;
 BT_AMP      = 3;
 BT_RSQB     = 4;
 BT_LEAD2    = 5;
 BT_LEAD3    = 6;
 BT_LEAD4    = 7;
 BT_TRAIL    = 8;
 BT_CR       = 9;
 BT_LF       = 10;
 BT_GT       = 11;
 BT_QUOT     = 12;
 BT_APOS     = 13;
 BT_EQUALS   = 14;
 BT_QUEST    = 15;
 BT_EXCL     = 16;
 BT_SOL      = 17;
 BT_SEMI     = 18;
 BT_NUM      = 19;
 BT_LSQB     = 20;
 BT_S        = 21;
 BT_NMSTRT   = 22;
 BT_COLON    = 23;
 BT_HEX      = 24;
 BT_DIGIT    = 25;
 BT_NAME     = 26;
 BT_MINUS    = 27;
 BT_OTHER    = 28; { known not to be a name or name start character }
 BT_NONASCII = 29; { might be a name or name start character }
 BT_PERCNT   = 30;
 BT_LPAR     = 31;
 BT_RPAR     = 32;
 BT_AST      = 33;
 BT_PLUS     = 34;
 BT_COMMA    = 35;
 BT_VERBAR   = 36;

 BT_COLON_   = BT_NMSTRT;

{ BYTE_TYPE }
function BYTE_TYPE(enc : ENCODING_ptr; p : char_ptr ) : int;
begin
{$IFDEF XML_MIN_SIZE }
 result:=normal_encoding_ptr(enc ).byteType(enc ,p );

{$ELSE }
 result:=normal_encoding_ptr(enc ).type_[int8u(p^ ) ];

{$ENDIF }

end;

{ BYTE_TO_ASCII }
function BYTE_TO_ASCII(enc : ENCODING_ptr; p : char_ptr ) : int;
begin
{$IFDEF XML_MIN_SIZE }
 result:=normal_encoding_ptr(enc ).byteToAscii(enc ,p );

{$ELSE }
 result:=int8u_ptr(p )^;

{$ENDIF }

end;

{ CHAR_MATCHES }
function CHAR_MATCHES(enc : ENCODING_ptr; p : char_ptr; c : int ) : int;
begin
{$IFDEF XML_MIN_SIZE }
 result:=normal_encoding_ptr(enc ).charMatches(enc ,p ,c );

{$ELSE }
 result:=int(int8u_ptr(p )^ = c );

{$ENDIF }

end;

{ IS_NAME_CHAR }
function IS_NAME_CHAR(enc : ENCODING_ptr; p : char_ptr; n : int ) : int;
begin
 case n of
  2 : result:=normal_encoding_ptr(enc ).isName2(enc ,p );
  3 : result:=normal_encoding_ptr(enc ).isName3(enc ,p );
  4 : result:=normal_encoding_ptr(enc ).isName4(enc ,p );

 end;

end;

{ IS_NMSTRT_CHAR }
function IS_NMSTRT_CHAR(enc : ENCODING_ptr; p : char_ptr; n : int ) : int;
begin
 case n of
  2 : result:=normal_encoding_ptr(enc ).isNmstrt2(enc ,p );
  3 : result:=normal_encoding_ptr(enc ).isNmstrt3(enc ,p );
  4 : result:=normal_encoding_ptr(enc ).isNmstrt4(enc ,p );

 end;

end;

{ IS_INVALID_CHAR }
function IS_INVALID_CHAR(enc : ENCODING_ptr; p : char_ptr; n : int ) : int;
begin
 case n of
  2 : result:=normal_encoding_ptr(enc ).isInvalid2(enc ,p );
  3 : result:=normal_encoding_ptr(enc ).isInvalid3(enc ,p );
  4 : result:=normal_encoding_ptr(enc ).isInvalid4(enc ,p );

 end;

end;

{ IS_NAME_CHAR_MINBPC }
function IS_NAME_CHAR_MINBPC(enc : ENCODING_ptr; p : char_ptr ) : int;
begin
{$IFDEF XML_MIN_SIZE }
 result:=normal_encoding_ptr(enc ).isNameMin(enc ,p );

{$ELSE }
 result:=0;

{$ENDIF }

end;

{ IS_NMSTRT_CHAR_MINBPC }
function IS_NMSTRT_CHAR_MINBPC(enc : ENCODING_ptr; p : char_ptr ) : int;
begin
{$IFDEF XML_MIN_SIZE }
 result:=normal_encoding_ptr(enc ).isNmstrtMin(enc ,p );

{$ELSE }
 result:=0;

{$ENDIF }

end;

{ INIT_ENC_INDEX }
function INIT_ENC_INDEX(enc : INIT_ENCODING_ptr ) : int;
begin
 result:=int(enc.initEnc.isUtf16 );

end;

{ SET_INIT_ENC_INDEX }
procedure SET_INIT_ENC_INDEX(enc : INIT_ENCODING_ptr; i : int );
begin
 enc.initEnc.isUtf16:=char(i );

end;

{$I xmltok_impl.inc }

const
{$IFDEF XML_NS }
 utf8_encoding_ns : normal_encoding = (
  enc:(scanners       :(normal_prologTok ,normal_contentTok ,normal_cdataSectionTok {$IFDEF XML_DTD },normal_ignoreSectionTok {$ENDIF } );
       literalScanners:(normal_attributeValueTok ,normal_entityValueTok );

       sameName            :normal_sameName;
       nameMatchesAscii    :normal_nameMatchesAscii;
       nameLength          :normal_nameLength;
       skipS               :normal_skipS;
       getAtts             :normal_getAtts;
       charRefNumber       :normal_charRefNumber;
       predefinedEntityName:normal_predefinedEntityName;
       updatePosition      :normal_updatePosition;
       isPublicId          :normal_isPublicId;
       utf8Convert         :utf8_toUtf8;
       utf16Convert        :utf8_toUtf16;

       minBytesPerChar:1;

       isUtf8 :#1;
       isUtf16:#0 );
  type_:({$I asciitab.inc}
         {$I utf8tab.inc});

 {$IFDEF XML_MIN_SIZE }
  byteType   :sb_byteType;
  isNameMin  :isNever;
  isNmstrtMin:isNever;
  byteToAscii:sb_byteToAscii;
  charMatches:sb_charMatches;

 {$ENDIF }

  isName2   :utf8_isName2;
  isName3   :utf8_isName3;
  isName4   :isNever;
  isNmstrt2 :utf8_isNmstrt2;
  isNmstrt3 :utf8_isNmstrt3;
  isNmstrt4 :isNever;
  isInvalid2:utf8_isInvalid2;
  isInvalid3:utf8_isInvalid3;
  isInvalid4:utf8_isInvalid4 );

{$ENDIF }

 utf8_encoding : normal_encoding = (
  enc:(scanners       :(normal_prologTok ,normal_contentTok ,normal_cdataSectionTok {$IFDEF XML_DTD },normal_ignoreSectionTok {$ENDIF } );
       literalScanners:(normal_attributeValueTok ,normal_entityValueTok );

       sameName            :normal_sameName;
       nameMatchesAscii    :normal_nameMatchesAscii;
       nameLength          :normal_nameLength;
       skipS               :normal_skipS;
       getAtts             :normal_getAtts;
       charRefNumber       :normal_charRefNumber;
       predefinedEntityName:normal_predefinedEntityName;
       updatePosition      :normal_updatePosition;
       isPublicId          :normal_isPublicId;
       utf8Convert         :utf8_toUtf8;
       utf16Convert        :utf8_toUtf16;

       minBytesPerChar:1;

       isUtf8 :#1;
       isUtf16:#0 );
  type_:({$I asciitab_bt_colon_.inc}
         {$I utf8tab.inc});

 {$IFDEF XML_MIN_SIZE }
  byteType   :sb_byteType;
  isNameMin  :isNever;
  isNmstrtMin:isNever;
  byteToAscii:sb_byteToAscii;
  charMatches:sb_charMatches;

 {$ENDIF }

  isName2   :utf8_isName2;
  isName3   :utf8_isName3;
  isName4   :isNever;
  isNmstrt2 :utf8_isNmstrt2;
  isNmstrt3 :utf8_isNmstrt3;
  isNmstrt4 :isNever;
  isInvalid2:utf8_isInvalid2;
  isInvalid3:utf8_isInvalid3;
  isInvalid4:utf8_isInvalid4 );

{$IFDEF XML_NS }
 internal_utf8_encoding_ns : normal_encoding = (
  enc:(scanners       :(normal_prologTok ,normal_contentTok ,normal_cdataSectionTok {$IFDEF XML_DTD },normal_ignoreSectionTok {$ENDIF } );
       literalScanners:(normal_attributeValueTok ,normal_entityValueTok );

       sameName            :normal_sameName;
       nameMatchesAscii    :normal_nameMatchesAscii;
       nameLength          :normal_nameLength;
       skipS               :normal_skipS;
       getAtts             :normal_getAtts;
       charRefNumber       :normal_charRefNumber;
       predefinedEntityName:normal_predefinedEntityName;
       updatePosition      :normal_updatePosition;
       isPublicId          :normal_isPublicId;
       utf8Convert         :utf8_toUtf8;
       utf16Convert        :utf8_toUtf16;

       minBytesPerChar:1;

       isUtf8 :#1;
       isUtf16:#0 );
  type_:({$I iasciitab.inc}
         {$I utf8tab.inc});

 {$IFDEF XML_MIN_SIZE }
  byteType   :sb_byteType;
  isNameMin  :isNever;
  isNmstrtMin:isNever;
  byteToAscii:sb_byteToAscii;
  charMatches:sb_charMatches;

 {$ENDIF }

  isName2   :utf8_isName2;
  isName3   :utf8_isName3;
  isName4   :isNever;
  isNmstrt2 :utf8_isNmstrt2;
  isNmstrt3 :utf8_isNmstrt3;
  isNmstrt4 :isNever;
  isInvalid2:utf8_isInvalid2;
  isInvalid3:utf8_isInvalid3;
  isInvalid4:utf8_isInvalid4 );

{$ENDIF }

 internal_utf8_encoding : normal_encoding = (
  enc:(scanners       :(normal_prologTok ,normal_contentTok ,normal_cdataSectionTok {$IFDEF XML_DTD },normal_ignoreSectionTok {$ENDIF } );
       literalScanners:(normal_attributeValueTok ,normal_entityValueTok );

       sameName            :normal_sameName;
       nameMatchesAscii    :normal_nameMatchesAscii;
       nameLength          :normal_nameLength;
       skipS               :normal_skipS;
       getAtts             :normal_getAtts;
       charRefNumber       :normal_charRefNumber;
       predefinedEntityName:normal_predefinedEntityName;
       updatePosition      :normal_updatePosition;
       isPublicId          :normal_isPublicId;
       utf8Convert         :utf8_toUtf8;
       utf16Convert        :utf8_toUtf16;

       minBytesPerChar:1;

       isUtf8 :#1;
       isUtf16:#0 );
  type_:({$I iasciitab_bt_colon_.inc}
         {$I utf8tab.inc});

 {$IFDEF XML_MIN_SIZE }
  byteType   :sb_byteType;
  isNameMin  :isNever;
  isNmstrtMin:isNever;
  byteToAscii:sb_byteToAscii;
  charMatches:sb_charMatches;

 {$ENDIF }

  isName2   :utf8_isName2;
  isName3   :utf8_isName3;
  isName4   :isNever;
  isNmstrt2 :utf8_isNmstrt2;
  isNmstrt3 :utf8_isNmstrt3;
  isNmstrt4 :isNever;
  isInvalid2:utf8_isInvalid2;
  isInvalid3:utf8_isInvalid3;
  isInvalid4:utf8_isInvalid4 );

{$IFDEF XML_NS }
 latin1_encoding_ns : normal_encoding = ();{..}

{$ENDIF }

 latin1_encoding : normal_encoding = ();{..}

{$IFDEF XML_NS }
 ascii_encoding_ns : normal_encoding = ();{..}

{$ENDIF }

 ascii_encoding : normal_encoding = ();{..}

{$IFDEF XML_NS }
 little2_encoding_ns : normal_encoding = ();{..}

{$ENDIF }

 little2_encoding : normal_encoding = ();{..}

{$IFDEF XML_NS }
 big2_encoding_ns : normal_encoding = ();{..}

{$ENDIF }

 big2_encoding : normal_encoding = ();{..}

{ If this enumeration is changed, getEncodingIndex and encodings
  must also be changed. }
 UNKNOWN_ENC    = -1;
 ISO_8859_1_ENC = 0;
 US_ASCII_ENC   = 1;
 UTF_8_ENC      = 2;
 UTF_16_ENC     = 3;
 UTF_16BE_ENC   = 4;
 UTF_16LE_ENC   = 5;
 NO_ENC         = 6; { must match encodingNames up to here }

 KW_ISO_8859_1 : array[0..10 ] of char = (
  ASCII_I ,ASCII_S ,ASCII_O ,ASCII_MINUS ,ASCII_8 ,ASCII_8 ,ASCII_5 ,ASCII_9 ,
  ASCII_MINUS ,ASCII_1 ,#0 );

 KW_US_ASCII : array[0..8 ] of char = (
  ASCII_U ,ASCII_S ,ASCII_MINUS ,ASCII_A ,ASCII_S ,ASCII_C ,ASCII_I ,ASCII_I ,
  #0 );

 KW_UTF_8 : array[0..5 ] of char =  (
  ASCII_U ,ASCII_T ,ASCII_F ,ASCII_MINUS ,ASCII_8 ,#0 );

 KW_UTF_16 : array[0..6 ] of char = (
  ASCII_U ,ASCII_T ,ASCII_F ,ASCII_MINUS ,ASCII_1 ,ASCII_6 ,#0 );

 KW_UTF_16BE : array[0..8 ] of char = (
  ASCII_U ,ASCII_T ,ASCII_F ,ASCII_MINUS ,ASCII_1 ,ASCII_6 ,ASCII_B ,ASCII_E ,
  #0 );

 KW_UTF_16LE : array[0..8 ] of char = (
  ASCII_U ,ASCII_T ,ASCII_F ,ASCII_MINUS ,ASCII_1 ,ASCII_6 ,ASCII_L ,ASCII_E ,
  #0 );

{ UNIT IMPLEMENTATION }
{ streqci }
function streqci(s1 ,s2 : char_ptr ) : int;
var
 c1 ,c2 : char;

begin
 repeat
  c1:=s1^;
  c2:=s2^;

  inc(ptrcomp(s1 ) );
  inc(ptrcomp(s2 ) );

  if (ASCII_al <= c1 ) and
     (c1 <= ASCII_zl ) then
   inc(byte(c1 ) ,byte(ASCII_A ) - byte(ASCII_al ) );

  if (ASCII_al <= c2 ) and
     (c2 <= ASCII_zl ) then
   inc(byte(c2 ) ,byte(ASCII_A ) - byte(ASCII_al ) );

  if c1 <> c2 then
   begin
    result:=0;

    exit;

   end;

  if (c1 = #0 ) or
     (c2 = #0 ) then
   break;

 until false;

 result:=1;

end;

{ initUpdatePosition {..}
procedure initUpdatePosition(enc : ENCODING_ptr; ptr ,end_ : char_ptr; pos : POSITION_ptr );
begin
end; 

{ getEncodingIndex }
function getEncodingIndex(name : char_ptr ) : int;
const
 encodingNames : array[0..5 ] of char_ptr = (
  @KW_ISO_8859_1 ,
  @KW_US_ASCII ,
  @KW_UTF_8 ,
  @KW_UTF_16 ,
  @KW_UTF_16BE ,
  @KW_UTF_16LE );

var
 i : int; 

begin
 if name = NIL then
  result:=NO_ENC
 else
  begin
   i:=0;

   while i < sizeof(encodingNames ) div sizeof(encodingNames[0 ] ) do
    begin
     if streqci(name ,encodingNames[i ] ) <> 0 then
      begin
       result:=i;

       exit;

      end;

     inc(i );

    end;

   result:=UNKNOWN_ENC;

  end;

end;

{ initScan }
{ This is what detects the encoding.  encodingTable maps from
  encoding indices to encodings; int8u(enc.initEnc.isUtf16 ) is the index of
  the external (protocol) specified encoding; state is
  XML_CONTENT_STATE if we're parsing an external text entity, and
  XML_PROLOG_STATE otherwise. }
function initScan(
          encodingTable : ENCODING_ptr_ptr;
          enc : INIT_ENCODING_ptr;
          state : int;
          ptr ,end_ : char_ptr;
          nextTokPtr : char_ptr_ptr ) : int;
var
 encPtr : ENCODING_ptr_ptr;

 e : int;

label
 _003C ,_esc ;

begin
 if ptr = end_ then
  begin
   result:=XML_TOK_NONE;

   exit;

  end;

 encPtr:=enc.encPtr;

{ only a single byte available for auto-detection }
 if ptrcomp(ptr ) + 1 = ptrcomp(end_ ) then
  begin
  {$IFNDEF XML_DTD } { FIXME }
  { a well-formed document entity must have more than one byte }
   if state <> XML_CONTENT_STATE then
    begin
     result:=XML_TOK_PARTIAL;

     exit;

    end;

  {$ENDIF }
  { so we're parsing an external text entity... }
  { if UTF-16 was externally specified, then we need at least 2 bytes }
   case INIT_ENC_INDEX(enc ) of
    UTF_16_ENC ,UTF_16LE_ENC ,UTF_16BE_ENC :
     begin
      result:=XML_TOK_PARTIAL;

      exit;

     end;

   end;

   case int8u(ptr^ ) of
    $FE ,$FF ,$EF : { possibly first byte of UTF-8 BOM }
     if (INIT_ENC_INDEX(enc ) = ISO_8859_1_ENC ) and
        (state = XML_CONTENT_STATE ) then
     else
      goto _003C;

    $00 ,$3C : { fall through }
    _003C:
     begin
      result:=XML_TOK_PARTIAL;

      exit;

     end;

   end;

  end
 else
  case (ptrcomp(ptr^ ) shl 8 ) or int8u_ptr(ptrcomp(ptr ) + 1 )^ of
   $FEFF :
    if (INIT_ENC_INDEX(enc ) = ISO_8859_1_ENC ) and
       (state = XML_CONTENT_STATE ) then
    else
     begin
      nextTokPtr^:=char_ptr(ptrcomp(ptr ) + 2 );
      encPtr^    :=ENCODING_ptr_ptr(ptrcomp(encodingTable ) + UTF_16BE_ENC * sizeof(ENCODING_ptr ) )^;

      result:=XML_TOK_BOM;

      exit;

     end;

  { 00 3C is handled in the default case }
   $3C00 :
    if ((INIT_ENC_INDEX(enc ) = UTF_16BE_ENC ) or
        (INIT_ENC_INDEX(enc ) = UTF_16_ENC ) ) and
       (state = XML_CONTENT_STATE ) then
    else
     begin
      encPtr^:=ENCODING_ptr_ptr(ptrcomp(encodingTable ) + UTF_16LE_ENC * sizeof(ENCODING_ptr ) )^;
      result :=XmlTok_(encPtr^ ,state ,ptr ,end_ ,nextTokPtr );

      exit;

     end;

   $FFFE :
    if (INIT_ENC_INDEX(enc ) = ISO_8859_1_ENC ) and
       (state = XML_CONTENT_STATE ) then
    else
     begin
      nextTokPtr^:=char_ptr(ptrcomp(ptr ) + 2 );
      encPtr^    :=ENCODING_ptr_ptr(ptrcomp(encodingTable ) + UTF_16LE_ENC * sizeof(ENCODING_ptr ) )^;

      result:=XML_TOK_BOM;

      exit;

     end;

  { Maybe a UTF-8 BOM (EF BB BF) }
  { If there's an explicitly specified (external) encoding
    of ISO-8859-1 or some flavour of UTF-16
    and this is an external text entity,
    don't look for the BOM,
    because it might be a legal data. }
   $EFBB :
    begin
     if state = XML_CONTENT_STATE then
      begin
       e:=INIT_ENC_INDEX(enc );

       if (e = ISO_8859_1_ENC ) or
          (e = UTF_16BE_ENC ) or
          (e = UTF_16LE_ENC ) or
          (e = UTF_16_ENC ) then
        goto _esc;

      end;

     if ptrcomp(ptr ) + 2 = ptrcomp(end_ ) then
      begin
       result:=XML_TOK_PARTIAL;

       exit;

      end;

     if int8u_ptr(ptrcomp(ptr ) + 2 )^ = $BF then
      begin
       nextTokPtr^:=char_ptr(ptrcomp(ptr ) + 3 );
       encPtr^    :=ENCODING_ptr_ptr(ptrcomp(encodingTable ) + UTF_8_ENC * sizeof(ENCODING_ptr ) )^;

       result:=XML_TOK_BOM;

       exit;

      end;

    end;

   else
   { 0 isn't a legal data character. Furthermore a document
     entity can only start with ASCII characters.  So the only
     way this can fail to be big-endian UTF-16 if it it's an
     external parsed general entity that's labelled as
     UTF-16LE. }
    if ptr^ = #0 then
     begin
      if (state = XML_CONTENT_STATE ) and
         (INIT_ENC_INDEX(enc ) = UTF_16LE_ENC ) then
       goto _esc;

      encPtr^:=ENCODING_ptr_ptr(ptrcomp(encodingTable ) + UTF_16BE_ENC * sizeof(ENCODING_ptr ) )^;
      result :=XmlTok_(encPtr^ ,state ,ptr ,end_ ,nextTokPtr );

      exit;

     end
    else
    { We could recover here in the case:
      - parsing an external entity
      - second byte is 0
      - no externally specified encoding
      - no encoding declaration
      by assuming UTF-16LE.  But we don't, because this would mean when
      presented just with a single byte, we couldn't reliably determine
      whether we needed further bytes. }
     if int8u_ptr(ptrcomp(ptr ) + 1 )^ = 0 then
      begin
       if state = XML_CONTENT_STATE then
        goto _esc;

       encPtr^:=ENCODING_ptr_ptr(ptrcomp(encodingTable ) + UTF_16LE_ENC * sizeof(ENCODING_ptr ) )^;
       result :=XmlTok_(encPtr^ ,state ,ptr ,end_ ,nextTokPtr );

      end;

  end;

_esc:
 encPtr^:=ENCODING_ptr_ptr(ptrcomp(encodingTable ) + INIT_ENC_INDEX(enc ) * sizeof(ENCODING_ptr ) )^;
 result :=XmlTok_(encPtr^ ,state ,ptr ,end_ ,nextTokPtr );

end;

{ toAscii }
function toAscii(enc : ENCODING_ptr; ptr ,end_ : char_ptr ) : int;
var
 buf : array[0..0 ] of char;

 p : char_ptr;

begin
 p:=@buf[0 ];

 XmlUtf8Convert(enc ,@ptr ,end_ ,@p ,char_ptr(ptrcomp(p ) + 1 ) );

 if p = @buf[0 ] then
  result:=-1
 else
  result:=int(buf[0 ] );

end;

{ isSpace }
function isSpace(c : int ) : int;
begin
 case c of
  $20 ,$D ,$A ,$9 :
   result:=1;

  else
   result:=0;

 end;

end;

{ parsePseudoAttribute }
{ Return 1 if there's just optional white space or there's an S
  followed by name=val. }
function parsePseudoAttribute(
          enc : ENCODING_ptr;
          ptr ,end_ : char_ptr;
          namePtr ,nameEndPtr ,valPtr ,nextTokPtr : char_ptr_ptr ) : int;
var
 c : int;

 open : char;

begin
 if ptr = end_ then
  begin
   namePtr^:=NIL;
   result  :=1;

   exit;

  end;

 if isSpace(toAscii(enc ,ptr ,end_ ) ) = 0 then
  begin
   nextTokPtr^:=ptr;

   result:=0;

   exit;

  end;

 repeat
  inc(ptrcomp(ptr ) ,enc.minBytesPerChar );

 until isSpace(toAscii(enc ,ptr ,end_ ) ) = 0;

 if ptr = end_ then
  begin
   namePtr^:=NIL;
   result  :=1;

   exit;

  end;

 namePtr^:=ptr;

 repeat
  c:=toAscii(enc ,ptr ,end_ );

  if c = -1 then
   begin
    nextTokPtr^:=ptr;

    result:=0;

    exit;

   end;

  if c = int(ASCII_EQUALS ) then
   begin
    nameEndPtr^:=ptr;
    
    break;

   end;

  if isSpace(c ) <> 0 then
   begin
    nameEndPtr^:=ptr;

    repeat
     inc(ptrcomp(ptr ) ,enc.minBytesPerChar );

     c:=toAscii(enc ,ptr ,end_ );

    until isSpace(c ) = 0;

    if c <> int(ASCII_EQUALS ) then
     begin
      nextTokPtr^:=ptr;

      result:=0;

      exit;

     end;

    break;

   end;

  inc(ptrcomp(ptr ) ,enc.minBytesPerChar );

 until false;

 if ptr = namePtr^ then
  begin
   nextTokPtr^:=ptr;

   result:=0;

   exit;

  end;

 inc(ptrcomp(ptr ) ,enc.minBytesPerChar );

 c:=toAscii(enc ,ptr ,end_ );

 while isSpace(c ) <> 0 do
  begin
   inc(ptrcomp(ptr ) ,enc.minBytesPerChar );

   c:=toAscii(enc ,ptr ,end_ );

  end;

 if (c <> int(ASCII_QUOT ) ) and
    (c <> int(ASCII_APOS ) ) then
  begin
   nextTokPtr^:=ptr;

   result:=0;

   exit;

  end;

 open:=char(c );

 inc(ptrcomp(ptr ) ,enc.minBytesPerChar );

 valPtr^:=ptr;

 repeat
  c:=toAscii(enc ,ptr ,end_ );

  if c = int(open ) then
   break;

  if not((int(ASCII_al ) <= c ) and (c <= int(ASCII_zl ) ) ) and
     not((int(ASCII_A ) <= c ) and (c <= int(ASCII_Z ) ) ) and
     not((int(ASCII_0 ) <= c ) and (c <= int(ASCII_9 ) ) ) and
     (c <> int(ASCII_PERIOD ) ) and
     (c <> int(ASCII_MINUS ) ) and
     (c <> int(ASCII_UNDERSCORE ) ) then
   begin
    nextTokPtr^:=ptr;
    
    result:=0;

    exit;

   end;

  inc(ptrcomp(ptr ) ,enc.minBytesPerChar );

 until false;

 nextTokPtr^:=char_ptr(ptrcomp(ptr ) + enc.minBytesPerChar );

 result:=1;

end;

{ doParseXmlDecl }
function doParseXmlDecl(
          encodingFinder : encodingFinder_func;
          isGeneralTextEntity : int;
          enc : ENCODING_ptr;
          ptr ,end_ : char_ptr;
          badPtr ,versionPtr ,versionEndPtr ,encodingName : char_ptr_ptr;
          encoding : ENCODING_ptr_ptr;
          standalone : int_ptr ) : int;
var
 val ,name ,nameEnd : char_ptr;

 c : int;

begin
 val    :=NIL;
 name   :=NIL;
 nameEnd:=NIL;

 inc(ptrcomp(ptr ) ,5 * enc.minBytesPerChar );
 dec(ptrcomp(end_ ) ,2 * enc.minBytesPerChar );

 if (parsePseudoAttribute(enc ,ptr ,end_ ,@name ,@nameEnd ,@val ,@ptr ) = 0 ) or
    (name = NIL ) then
  begin
   badPtr^:=ptr;
   result :=0;

   exit;

  end;

 if XmlNameMatchesAscii(enc ,name ,nameEnd ,@KW_version[0 ] ) = 0 then
  if isGeneralTextEntity = 0 then
   begin
    badPtr^:=name;
    result :=0;

    exit;

   end
  else
 else
  begin
   if versionPtr <> NIL then
    versionPtr^:=val;

   if versionEndPtr <> NIL then
    versionEndPtr^:=ptr;

   if parsePseudoAttribute(enc ,ptr ,end_ ,@name ,@nameEnd ,@val ,@ptr ) = 0 then
    begin
     badPtr^:=ptr;
     result :=0;

     exit;

    end;

   if name = NIL then
    begin
     if isGeneralTextEntity <> 0 then
      begin
      { a TextDecl must have an EncodingDecl }

       badPtr^:=ptr;
       result :=0;

       exit;

      end;

     result:=1;

     exit;

    end;

  end;

 if XmlNameMatchesAscii(enc ,name ,nameEnd ,@KW_encoding[0 ] ) <> 0 then
  begin
   c:=toAscii(enc ,val ,end_ );

   if not((int(ASCII_al ) <= c ) and (c <= int(ASCII_zl ) ) ) and
      not((int(ASCII_A ) <= c ) and (c <= int(ASCII_Z ) ) ) then
    begin
     badPtr^:=val;
     result :=0;

     exit;

    end;

   if encodingName <> NIL then
    encodingName^:=val;

   if encoding <> NIL then
    encoding^:=encodingFinder(enc ,val ,char_ptr(ptrcomp(ptr ) - enc.minBytesPerChar ) );

   if parsePseudoAttribute(enc ,ptr ,end_ ,@name ,@nameEnd ,@val ,@ptr ) = 0 then
    begin
     badPtr^:=ptr;
     result :=0;

     exit;

    end;

   if name <> NIL then
    begin
     result:=1;

     exit;

    end;

  end;

 if (XmlNameMatchesAscii(enc ,name ,nameEnd ,@KW_standalone[0 ] ) = 0 ) or
    (isGeneralTextEntity <> 0 ) then
  begin
   badPtr^:=name;
   result :=0;

   exit;

  end;

 if XmlNameMatchesAscii(enc ,val ,char_ptr(ptrcomp(ptr ) - enc.minBytesPerChar ) ,@KW_yes[0 ] ) <> 0 then
  if standalone <> NIL then
   standalone^:=1
  else
 else
  if XmlNameMatchesAscii(enc ,val ,char_ptr(ptrcomp(ptr ) - enc.minBytesPerChar ) ,@KW_no[0 ] ) <> 0 then
   if standalone <> NIL then
    standalone^:=0
   else
  else
   begin
    badPtr^:=val;
    result :=0;

    exit;

   end;

 while isSpace(toAscii(enc ,ptr ,end_ ) ) <> 0 do
  inc(ptrcomp(ptr ) ,enc.minBytesPerChar );

 if ptr <> end_ then
  begin
   badPtr^:=ptr;
   result :=0;

   exit;

  end;

 result:=1;

end;

{$I xmltok_ns.inc }

{ XMLTOK_ }
function XmlTok_;
begin
 result:=enc.scanners[state ](enc ,ptr ,end_ ,nextTokPtr );

end;

{ XMLPROLOGTOK }
function XmlPrologTok;
begin
 result:=XmlTok_(enc ,XML_PROLOG_STATE ,ptr ,end_ ,nextTokPtr );

end;

{ XMLCONTENTTOK }
function XmlContentTok;
begin
 result:=XmlTok_(enc ,XML_CONTENT_STATE ,ptr ,end_ ,nextTokPtr );

end;

{ XMLISPUBLICID }
function XmlIsPublicId;
begin
 result:=enc.isPublicId(enc ,ptr ,end_ ,badPtr );

end;

{ XMLUTF8CONVERT }
procedure XmlUtf8Convert;
begin
 enc.utf8Convert(enc ,fromP ,fromLim ,toP ,toLim );

end;

{ XMLUTF16CONVERT }
procedure XmlUtf16Convert;
begin
 enc.utf16Convert(enc ,fromP ,fromLim ,toP ,toLim );

end;

{ XMLUTF8ENCODE {..}{unicode}
function XmlUtf8Encode;
begin
end;

{ XMLUTF16ENCODE {..}{unicode}
function XmlUtf16Encode;
begin
end;

{ XMLLITERALTOK }
function XmlLiteralTok;
begin
 result:=enc.literalScanners[literalType ](enc ,ptr ,end_ ,nextTokPtr );

end;

{ XMLATTRIBUTEVALUETOK }
function XmlAttributeValueTok;
begin
 result:=XmlLiteralTok(enc ,XML_ATTRIBUTE_VALUE_LITERAL ,ptr ,end_ ,nextTokPtr );

end;

{ XMLENTITYVALUETOK }
function XmlEntityValueTok;
begin
 result:=XmlLiteralTok(enc ,XML_ENTITY_VALUE_LITERAL ,ptr ,end_ ,nextTokPtr );

end;

{ XMLSAMENAME }
function XmlSameName;
begin
 result:=enc.sameName(enc ,ptr1 ,ptr2 );

end;

{ XMLNAMEMATCHESASCII }
function XmlNameMatchesAscii;
begin
 result:=enc.nameMatchesAscii(enc ,ptr1 ,end1 ,ptr2 );

end;

{ XMLNAMELENGTH }
function XmlNameLength;
begin
 result:=enc.nameLength(enc ,ptr );

end;

{ XMLGETATTRIBUTES }
function XmlGetAttributes;
begin
 result:=enc.getAtts(enc ,ptr ,attsMax ,atts );

end;

{ XMLCHARREFNUMBER }
function XmlCharRefNumber;
begin
 result:=enc.charRefNumber(enc ,ptr );

end;

{ XMLPREDEFINEDENTITYNAME }
function XmlPredefinedEntityName;
begin
 result:=enc.predefinedEntityName(enc ,ptr ,end_ );

end;

{ XMLPARSEXMLDECLNS {..}
function XmlParseXmlDeclNS;
begin
end;

END.

{unicode}

