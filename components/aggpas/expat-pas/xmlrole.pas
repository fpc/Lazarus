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
// 08.06.2006-Milano: porting
//
{ xmlrole.pas }
unit
 xmlrole ;

INTERFACE

uses
 expat_basics ,
 expat_external ,
 xmltok ;

{$I expat_mode.inc }

{ TYPES DEFINITION }
const
 XML_ROLE_ERROR                    = -1;
 XML_ROLE_NONE                     = 0;
 XML_ROLE_XML_DECL                 = XML_ROLE_NONE + 1;
 XML_ROLE_INSTANCE_START           = XML_ROLE_XML_DECL + 1;
 XML_ROLE_DOCTYPE_NONE             = XML_ROLE_INSTANCE_START + 1;
 XML_ROLE_DOCTYPE_NAME             = XML_ROLE_DOCTYPE_NONE + 1;
 XML_ROLE_DOCTYPE_SYSTEM_ID        = XML_ROLE_DOCTYPE_NAME + 1;
 XML_ROLE_DOCTYPE_PUBLIC_ID        = XML_ROLE_DOCTYPE_SYSTEM_ID + 1;
 XML_ROLE_DOCTYPE_INTERNAL_SUBSET  = XML_ROLE_DOCTYPE_PUBLIC_ID + 1;
 XML_ROLE_DOCTYPE_CLOSE            = XML_ROLE_DOCTYPE_INTERNAL_SUBSET + 1;
 XML_ROLE_GENERAL_ENTITY_NAME      = XML_ROLE_DOCTYPE_CLOSE + 1;
 XML_ROLE_PARAM_ENTITY_NAME        = XML_ROLE_GENERAL_ENTITY_NAME + 1;
 XML_ROLE_ENTITY_NONE              = XML_ROLE_PARAM_ENTITY_NAME + 1;
 XML_ROLE_ENTITY_VALUE             = XML_ROLE_ENTITY_NONE + 1;
 XML_ROLE_ENTITY_SYSTEM_ID         = XML_ROLE_ENTITY_VALUE + 1;
 XML_ROLE_ENTITY_PUBLIC_ID         = XML_ROLE_ENTITY_SYSTEM_ID + 1;
 XML_ROLE_ENTITY_COMPLETE          = XML_ROLE_ENTITY_PUBLIC_ID + 1;
 XML_ROLE_ENTITY_NOTATION_NAME     = XML_ROLE_ENTITY_COMPLETE + 1;
 XML_ROLE_NOTATION_NONE            = XML_ROLE_ENTITY_NOTATION_NAME + 1;
 XML_ROLE_NOTATION_NAME            = XML_ROLE_NOTATION_NONE + 1;
 XML_ROLE_NOTATION_SYSTEM_ID       = XML_ROLE_NOTATION_NAME + 1;
 XML_ROLE_NOTATION_NO_SYSTEM_ID    = XML_ROLE_NOTATION_SYSTEM_ID + 1;
 XML_ROLE_NOTATION_PUBLIC_ID       = XML_ROLE_NOTATION_NO_SYSTEM_ID + 1;
 XML_ROLE_ATTRIBUTE_NAME           = XML_ROLE_NOTATION_PUBLIC_ID + 1;
 XML_ROLE_ATTRIBUTE_TYPE_CDATA     = XML_ROLE_ATTRIBUTE_NAME + 1;
 XML_ROLE_ATTRIBUTE_TYPE_ID        = XML_ROLE_ATTRIBUTE_TYPE_CDATA + 1;
 XML_ROLE_ATTRIBUTE_TYPE_IDREF     = XML_ROLE_ATTRIBUTE_TYPE_ID + 1;
 XML_ROLE_ATTRIBUTE_TYPE_IDREFS    = XML_ROLE_ATTRIBUTE_TYPE_IDREF + 1;
 XML_ROLE_ATTRIBUTE_TYPE_ENTITY    = XML_ROLE_ATTRIBUTE_TYPE_IDREFS + 1;
 XML_ROLE_ATTRIBUTE_TYPE_ENTITIES  = XML_ROLE_ATTRIBUTE_TYPE_ENTITY + 1;
 XML_ROLE_ATTRIBUTE_TYPE_NMTOKEN   = XML_ROLE_ATTRIBUTE_TYPE_ENTITIES + 1;
 XML_ROLE_ATTRIBUTE_TYPE_NMTOKENS  = XML_ROLE_ATTRIBUTE_TYPE_NMTOKEN + 1;
 XML_ROLE_ATTRIBUTE_ENUM_VALUE     = XML_ROLE_ATTRIBUTE_TYPE_NMTOKENS + 1;
 XML_ROLE_ATTRIBUTE_NOTATION_VALUE = XML_ROLE_ATTRIBUTE_ENUM_VALUE + 1;
 XML_ROLE_ATTLIST_NONE             = XML_ROLE_ATTRIBUTE_NOTATION_VALUE + 1;
 XML_ROLE_ATTLIST_ELEMENT_NAME     = XML_ROLE_ATTLIST_NONE + 1;
 XML_ROLE_IMPLIED_ATTRIBUTE_VALUE  = XML_ROLE_ATTLIST_ELEMENT_NAME + 1;
 XML_ROLE_REQUIRED_ATTRIBUTE_VALUE = XML_ROLE_IMPLIED_ATTRIBUTE_VALUE + 1;
 XML_ROLE_DEFAULT_ATTRIBUTE_VALUE  = XML_ROLE_REQUIRED_ATTRIBUTE_VALUE + 1;
 XML_ROLE_FIXED_ATTRIBUTE_VALUE    = XML_ROLE_DEFAULT_ATTRIBUTE_VALUE + 1;
 XML_ROLE_ELEMENT_NONE             = XML_ROLE_FIXED_ATTRIBUTE_VALUE + 1;
 XML_ROLE_ELEMENT_NAME             = XML_ROLE_ELEMENT_NONE + 1;
 XML_ROLE_CONTENT_ANY              = XML_ROLE_ELEMENT_NAME + 1;
 XML_ROLE_CONTENT_EMPTY            = XML_ROLE_CONTENT_ANY + 1;
 XML_ROLE_CONTENT_PCDATA           = XML_ROLE_CONTENT_EMPTY + 1;
 XML_ROLE_GROUP_OPEN               = XML_ROLE_CONTENT_PCDATA + 1;
 XML_ROLE_GROUP_CLOSE              = XML_ROLE_GROUP_OPEN + 1;
 XML_ROLE_GROUP_CLOSE_REP          = XML_ROLE_GROUP_CLOSE + 1;
 XML_ROLE_GROUP_CLOSE_OPT          = XML_ROLE_GROUP_CLOSE_REP + 1;
 XML_ROLE_GROUP_CLOSE_PLUS         = XML_ROLE_GROUP_CLOSE_OPT + 1;
 XML_ROLE_GROUP_CHOICE             = XML_ROLE_GROUP_CLOSE_PLUS + 1;
 XML_ROLE_GROUP_SEQUENCE           = XML_ROLE_GROUP_CHOICE + 1;
 XML_ROLE_CONTENT_ELEMENT          = XML_ROLE_GROUP_SEQUENCE + 1;
 XML_ROLE_CONTENT_ELEMENT_REP      = XML_ROLE_CONTENT_ELEMENT + 1;
 XML_ROLE_CONTENT_ELEMENT_OPT      = XML_ROLE_CONTENT_ELEMENT_REP + 1;
 XML_ROLE_CONTENT_ELEMENT_PLUS     = XML_ROLE_CONTENT_ELEMENT_OPT + 1;
 XML_ROLE_PI                       = XML_ROLE_CONTENT_ELEMENT_PLUS + 1;
 XML_ROLE_COMMENT                  = XML_ROLE_PI + 1;

{$IFDEF XML_DTD }
 XML_ROLE_TEXT_DECL                = XML_ROLE_COMMENT + 1;
 XML_ROLE_IGNORE_SECT              = XML_ROLE_TEXT_DECL + 1;
 XML_ROLE_INNER_PARAM_ENTITY_REF   = XML_ROLE_IGNORE_SECT + 1;
 XML_ROLE_PARAM_ENTITY_REF         = XML_ROLE_INNER_PARAM_ENTITY_REF + 1;

{$ELSE }
 XML_ROLE_PARAM_ENTITY_REF         = XML_ROLE_COMMENT + 1;

{$ENDIF }

type
 PROLOG_STATE_ptr = ^PROLOG_STATE;
 PROLOG_STATE = record
   handler : function(state : PROLOG_STATE_ptr; tok : int; ptr ,end_ : char_ptr; enc : ENCODING_ptr ) : int;

   level     : unsigned;
   role_none : int;

  {$IFDEF XML_DTD }
   includeLevel   : unsigned;
   documentEntity ,
   inEntityValue  : int;

  {$ENDIF }

  end;

{ GLOBAL PROCEDURES }
 procedure XmlPrologStateInit(state : PROLOG_STATE_ptr );
 function  XmlTokenRole      (state : PROLOG_STATE_ptr; tok : int; ptr ,end_ : char_ptr; enc : ENCODING_ptr ) : int;


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
const
{$I ascii.inc }

{ Doesn't check:

  that ,| are not mixed in a model group
  content of literals }

 KW_ANY : array[0..3 ] of char = (
  ASCII_A ,ASCII_N ,ASCII_Y ,#0 );

 KW_ATTLIST : array[0..7 ] of char = (
  ASCII_A ,ASCII_T ,ASCII_T ,ASCII_L ,ASCII_I ,ASCII_S ,ASCII_T ,#0 );

 KW_CDATA : array[0..5 ] of char = (
  ASCII_C ,ASCII_D ,ASCII_A ,ASCII_T ,ASCII_A ,#0 );

 KW_DOCTYPE : array[0..7 ] of char = (
  ASCII_D ,ASCII_O ,ASCII_C ,ASCII_T ,ASCII_Y ,ASCII_P ,ASCII_E ,#0 );

 KW_ELEMENT : array[0..7 ] of char = (
  ASCII_E ,ASCII_L ,ASCII_E ,ASCII_M ,ASCII_E ,ASCII_N ,ASCII_T ,#0 );

 KW_EMPTY : array[0..5 ] of char = (
  ASCII_E ,ASCII_M ,ASCII_P ,ASCII_T ,ASCII_Y ,#0 );

 KW_ENTITIES : array[0..8 ] of char = (
  ASCII_E ,ASCII_N ,ASCII_T ,ASCII_I ,ASCII_T ,ASCII_I ,ASCII_E ,ASCII_S ,
  #0 );

 KW_ENTITY : array[0..6 ] of char = (
  ASCII_E ,ASCII_N ,ASCII_T ,ASCII_I ,ASCII_T ,ASCII_Y ,#0 );

 KW_FIXED : array[0..5 ] of char = (
  ASCII_F ,ASCII_I ,ASCII_X ,ASCII_E ,ASCII_D ,#0 );

 KW_ID : array[0..2 ] of char = (
  ASCII_I ,ASCII_D ,#0 );

 KW_IDREF : array[0..5 ] of char = (
  ASCII_I ,ASCII_D ,ASCII_R ,ASCII_E ,ASCII_F ,#0 );

 KW_IDREFS : array[0..6 ] of char = (
  ASCII_I ,ASCII_D ,ASCII_R ,ASCII_E ,ASCII_F ,ASCII_S ,#0 );

 KW_IGNORE : array[0..6 ] of char = (
  ASCII_I ,ASCII_G ,ASCII_N ,ASCII_O ,ASCII_R ,ASCII_E ,#0 );

 KW_IMPLIED : array[0..7 ] of char = (
  ASCII_I ,ASCII_M ,ASCII_P ,ASCII_L ,ASCII_I ,ASCII_E ,ASCII_D ,#0 );

 KW_INCLUDE : array[0..7 ] of char = (
  ASCII_I ,ASCII_N ,ASCII_C ,ASCII_L ,ASCII_U ,ASCII_D ,ASCII_E ,#0 );

 KW_NDATA : array[0..5 ] of char = (
  ASCII_N ,ASCII_D ,ASCII_A ,ASCII_T ,ASCII_A ,#0 );

 KW_NMTOKEN : array[0..7 ] of char = (
  ASCII_N ,ASCII_M ,ASCII_T ,ASCII_O ,ASCII_K ,ASCII_E ,ASCII_N ,#0 );

 KW_NMTOKENS : array[0..8 ] of char = (
  ASCII_N ,ASCII_M ,ASCII_T ,ASCII_O ,ASCII_K ,ASCII_E ,ASCII_N ,ASCII_S ,
  #0 );

 KW_NOTATION : array[0..8 ] of char = (
  ASCII_N ,ASCII_O ,ASCII_T ,ASCII_A ,ASCII_T ,ASCII_I ,ASCII_O ,ASCII_N ,
  #0 );

 KW_PCDATA : array[0..6 ] of char = (
  ASCII_P ,ASCII_C ,ASCII_D ,ASCII_A ,ASCII_T ,ASCII_A ,#0 );

 KW_PUBLIC : array[0..6 ] of char = (
  ASCII_P ,ASCII_U ,ASCII_B ,ASCII_L ,ASCII_I ,ASCII_C ,#0 );

 KW_REQUIRED : array[0..8 ] of char = (
  ASCII_R ,ASCII_E ,ASCII_Q ,ASCII_U ,ASCII_I ,ASCII_R ,ASCII_E ,ASCII_D ,
  #0 );

 KW_SYSTEM : array[0..6 ] of char = (
  ASCII_S ,ASCII_Y ,ASCII_S ,ASCII_T ,ASCII_E ,ASCII_M ,#0 );


{ UNIT IMPLEMENTATION }
{ MIN_BYTES_PER_CHAR }
function MIN_BYTES_PER_CHAR(enc : ENCODING_ptr ) : int;
begin
 result:=enc.minBytesPerChar;

end;

{ error }
function error(state : PROLOG_STATE_ptr; tok : int; ptr ,end_ : char_ptr; enc : ENCODING_ptr ) : int;
begin
 result:=XML_ROLE_NONE;

end;

{ common {..}
function common(state : PROLOG_STATE_ptr; tok : int ) : int;
begin
end;

{ internalSubset {..}
function internalSubset(state : PROLOG_STATE_ptr; tok : int; ptr ,end_ : char_ptr; enc : ENCODING_ptr ) : int;
begin
end;

{ prolog2 }
function prolog2(state : PROLOG_STATE_ptr; tok : int; ptr ,end_ : char_ptr; enc : ENCODING_ptr ) : int;
begin
 case tok of
  XML_TOK_PROLOG_S :
   begin
    result:=XML_ROLE_NONE;

    exit;

   end;

  XML_TOK_PI :
   begin
    result:=XML_ROLE_PI;

    exit;

   end;

  XML_TOK_COMMENT :
   begin
    result:=XML_ROLE_COMMENT;

    exit;

   end;

  XML_TOK_INSTANCE_START :
   begin
    state.handler:=@error;

    result:=XML_ROLE_INSTANCE_START;

    exit;

   end;

 end;

 result:=common(state ,tok );

end;

{ doctype4 }
function doctype4(state : PROLOG_STATE_ptr; tok : int; ptr ,end_ : char_ptr; enc : ENCODING_ptr ) : int;
begin
 case tok of
  XML_TOK_PROLOG_S :
   begin
    result:=XML_ROLE_DOCTYPE_NONE;

    exit;

   end;

  XML_TOK_OPEN_BRACKET :
   begin
    state.handler:=@internalSubset;

    result:=XML_ROLE_DOCTYPE_INTERNAL_SUBSET;

    exit;

   end;

  XML_TOK_DECL_CLOSE :
   begin
    state.handler:=@prolog2;

    result:=XML_ROLE_DOCTYPE_CLOSE;

    exit;

   end;

 end;

 result:=common(state ,tok );

end;

{ doctype3 }
function doctype3(state : PROLOG_STATE_ptr; tok : int; ptr ,end_ : char_ptr; enc : ENCODING_ptr ) : int;
begin
 case tok of
  XML_TOK_PROLOG_S :
   begin
    result:=XML_ROLE_DOCTYPE_NONE;

    exit;

   end;

  XML_TOK_LITERAL :
   begin
    state.handler:=@doctype4;

    result:=XML_ROLE_DOCTYPE_SYSTEM_ID;

    exit;

   end;

 end;

 result:=common(state ,tok );

end;

{ doctype2 }
function doctype2(state : PROLOG_STATE_ptr; tok : int; ptr ,end_ : char_ptr; enc : ENCODING_ptr ) : int;
begin
 case tok of
  XML_TOK_PROLOG_S :
   begin
    result:=XML_ROLE_DOCTYPE_NONE;

    exit;

   end;

  XML_TOK_LITERAL :
   begin
    state.handler:=@doctype3;

    result:=XML_ROLE_DOCTYPE_PUBLIC_ID;

    exit;

   end;

 end;

 result:=common(state ,tok );

end;

{ doctype1 }
function doctype1(state : PROLOG_STATE_ptr; tok : int; ptr ,end_ : char_ptr; enc : ENCODING_ptr ) : int;
begin
 case tok of
  XML_TOK_PROLOG_S :
   begin
    result:=XML_ROLE_DOCTYPE_NONE;

    exit;

   end;

  XML_TOK_OPEN_BRACKET :
   begin
    state.handler:=@internalSubset;

    result:=XML_ROLE_DOCTYPE_INTERNAL_SUBSET;

    exit;

   end;

  XML_TOK_DECL_CLOSE :
   begin
    state.handler:=@prolog2;

    result:=XML_ROLE_DOCTYPE_CLOSE;

    exit;

   end;

  XML_TOK_NAME :
   begin
    if XmlNameMatchesAscii(enc ,ptr ,end_ ,@KW_SYSTEM[0 ] ) <> 0 then
     begin
      state.handler:=@doctype3;

      result:=XML_ROLE_DOCTYPE_NONE;

      exit;

     end;

    if XmlNameMatchesAscii(enc ,ptr ,end_ ,@KW_PUBLIC[0 ] ) <> 0 then
     begin
      state.handler:=@doctype2;

      result:=XML_ROLE_DOCTYPE_NONE;

      exit;

     end;

   end;

 end;

 result:=common(state ,tok );

end;

{ doctype0 }
function doctype0(state : PROLOG_STATE_ptr; tok : int; ptr ,end_ : char_ptr; enc : ENCODING_ptr ) : int;
begin
 case tok of
  XML_TOK_PROLOG_S :
   begin
    result:=XML_ROLE_DOCTYPE_NONE;

    exit;

   end;

  XML_TOK_NAME ,XML_TOK_PREFIXED_NAME :
   begin
    state.handler:=@doctype1;

    result:=XML_ROLE_DOCTYPE_NAME;

    exit;

   end;

 end;

 result:=common(state ,tok );

end;

{ prolog1 }
function prolog1(state : PROLOG_STATE_ptr; tok : int; ptr ,end_ : char_ptr; enc : ENCODING_ptr ) : int;
begin
 case tok of
  XML_TOK_PROLOG_S :
   begin
    result:=XML_ROLE_NONE;

    exit;

   end;

  XML_TOK_PI :
   begin
    result:=XML_ROLE_PI;

    exit;

   end;

  XML_TOK_COMMENT :
   begin
    result:=XML_ROLE_COMMENT;

    exit;

   end;

  XML_TOK_BOM :
   begin
    result:=XML_ROLE_NONE;

    exit;

   end;

  XML_TOK_DECL_OPEN :
   if XmlNameMatchesAscii(
       enc ,
       char_ptr(ptrcomp(ptr ) + 2 * MIN_BYTES_PER_CHAR(enc ) ) ,
       end_ ,
       @KW_DOCTYPE[0 ] ) = 0 then
    else
     begin
      state.handler:=@doctype0;

      result:=XML_ROLE_DOCTYPE_NONE;

      exit;

     end;

  XML_TOK_INSTANCE_START :
   begin
    state.handler:=@error;

    result:=XML_ROLE_INSTANCE_START;

    exit;

   end;

 end;

 result:=common(state ,tok );

end;

{ prolog0 }
function prolog0(state : PROLOG_STATE_ptr; tok : int; ptr ,end_ : char_ptr; enc : ENCODING_ptr ) : int;
label
 _break ;

begin
 case tok of
  XML_TOK_PROLOG_S :
   begin
    state.handler:=@prolog1;

    result:=XML_ROLE_NONE;

    exit;

   end;

  XML_TOK_XML_DECL :
   begin
    state.handler:=@prolog1;

    result:=XML_ROLE_XML_DECL;

    exit;

   end;

  XML_TOK_PI :
   begin
    state.handler:=@prolog1;
    
    result:=XML_ROLE_PI;

    exit;

   end;

  XML_TOK_COMMENT :
   begin
    state.handler:=@prolog1;

    result:=XML_ROLE_COMMENT;

    exit;

   end;

  XML_TOK_BOM :
   begin
    result:=XML_ROLE_NONE;

    exit;

   end;

  XML_TOK_DECL_OPEN :
   begin
    if XmlNameMatchesAscii(
        enc ,
        char_ptr(ptrcomp(ptr ) + 2 * MIN_BYTES_PER_CHAR(enc ) ) ,
        end_ ,
        @KW_DOCTYPE[0 ] ) = 0 then
      goto _break;

    state.handler:=@doctype0;

    result:=XML_ROLE_DOCTYPE_NONE;

    exit;

   end;

  XML_TOK_INSTANCE_START :
   begin
    state.handler:=@error;

    result:=XML_ROLE_INSTANCE_START;

    exit;

   end;

 end;

_break:
 result:=common(state ,tok );

end;

{ XMLPROLOGSTATEINIT }
procedure XmlPrologStateInit;
begin
 state.handler:=@prolog0;

{$IFDEF XML_DTD }
 state.documentEntity:=1;
 state.includeLevel  :=0;
 state.inEntityValue :=0;

{$ENDIF }

end;

{ XMLTOKENROLE }
function XmlTokenRole;
begin
 result:=state.handler(state ,tok ,ptr ,end_ ,enc );

end;

END.

