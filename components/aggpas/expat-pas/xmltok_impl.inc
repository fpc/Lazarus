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
// 17.05.2006-Milano: Unit port establishment
// 02.06.2006-Milano: porting
// 05.06.2006-Milano: -"-
// 06.06.2006-Milano: -"-
// 12.06.2006-Milano: -"-
// 15.06.2006-Milano: normal_contentTok
// 16.06.2006-Milano: -"-, porting
// 17.06.2006-Milano: -"-
// 21.06.2006-Milano: -"-
// 22.06.2006-Milano: -"-
//
{ xmltok_impl.inc }
{$Q- }
{$R- }
{ normal_scanRef {..}
function normal_scanRef(enc : ENCODING_ptr; ptr ,end_ : char_ptr; nextTokPtr : char_ptr_ptr ) : int;
begin
end;

{ normal_scanAtts }
function normal_scanAtts(enc : ENCODING_ptr; ptr ,end_ : char_ptr; nextTokPtr : char_ptr_ptr ) : int;
var
{$IFDEF XML_NS }
 hadColon : int;

{$ENDIF }

 t ,open ,tok : int;

label
 _bt0 ,_bt1 ,_bte ,sol ,gt ,_bt2 ;

begin
{$IFDEF XML_NS }
 hadColon:=0;

{$ENDIF }

 while ptr <> end_ do
  case BYTE_TYPE(enc ,ptr ) of
  {#define CHECK_NAME_CASES}
   BT_NONASCII :
    if IS_NAME_CHAR_MINBPC(enc ,ptr ) = 0 then
     begin
      nextTokPtr^:=ptr;

      result:=XML_TOK_INVALID;

      exit;

     end
    else
     goto _bt0;

   BT_NMSTRT ,BT_HEX ,BT_DIGIT ,BT_NAME ,BT_MINUS :
   _bt0:
    inc(ptrcomp(ptr ) ,MINBPC(enc ) );

   BT_LEAD2 :
    begin
     if ptrcomp(end_ ) - ptrcomp(ptr ) < 2 then
      begin
       result:=XML_TOK_PARTIAL_CHAR;

       exit;

      end;

     if IS_NAME_CHAR(enc ,ptr ,2 ) = 0 then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

     inc(ptrcomp(ptr ) ,2 );

    end;

   BT_LEAD3 :
    begin
     if ptrcomp(end_ ) - ptrcomp(ptr ) < 3 then
      begin
       result:=XML_TOK_PARTIAL_CHAR;

       exit;

      end;

     if IS_NAME_CHAR(enc ,ptr ,3 ) = 0 then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

     inc(ptrcomp(ptr ) ,3 );

    end;

   BT_LEAD4 :
    begin
     if ptrcomp(end_ ) - ptrcomp(ptr ) < 4 then
      begin
       result:=XML_TOK_PARTIAL_CHAR;

       exit;

      end;

     if IS_NAME_CHAR(enc ,ptr ,4 ) = 0 then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

     inc(ptrcomp(ptr ) ,4 );

    end;

  {CHECK_NAME_CASES #define}

  {$IFDEF XML_NS }
   BT_COLON :
    begin
     if hadColon <> 0 then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

     hadColon:=1;

     inc(ptrcomp(ptr ) ,MINBPC(enc ) );

     if ptr <> end_ then
      begin
       result:=XML_TOK_PARTIAL;

       exit;

      end;

     case BYTE_TYPE(enc ,ptr ) of
     {#define CHECK_NMSTRT_CASES}
      BT_NONASCII :
       if IS_NMSTRT_CHAR_MINBPC(enc ,ptr ) = 0 then
        begin
         nextTokPtr^:=ptr;

         result:=XML_TOK_INVALID;

         exit;

        end
       else
        goto _bt1;

      BT_NMSTRT ,BT_HEX :
      _bt1:
       inc(ptrcomp(ptr ) ,MINBPC(enc ) );

      BT_LEAD2 :
       begin
        if ptrcomp(end_ ) - ptrcomp(ptr ) < 2 then
         begin
          result:=XML_TOK_PARTIAL_CHAR;

          exit;

         end;

        if not IS_NMSTRT_CHAR(enc ,ptr ,2 ) = 0 then
         begin
          nextTokPtr^:=ptr;

          result:=XML_TOK_INVALID;

         end;

        inc(ptrcomp(ptr ) ,2 );

       end;

      BT_LEAD3 :
       begin
        if ptrcomp(end_ ) - ptrcomp(ptr ) < 3 then
         begin
          result:=XML_TOK_PARTIAL_CHAR;

          exit;

         end;

        if not IS_NMSTRT_CHAR(enc ,ptr ,3 ) = 0 then
         begin
          nextTokPtr^:=ptr;

          result:=XML_TOK_INVALID;

         end;

        inc(ptrcomp(ptr ) ,3 );

       end;

      BT_LEAD4 :
       begin
        if ptrcomp(end_ ) - ptrcomp(ptr ) < 4 then
         begin
          result:=XML_TOK_PARTIAL_CHAR;

          exit;

         end;

        if not IS_NMSTRT_CHAR(enc ,ptr ,4 ) = 0 then
         begin
          nextTokPtr^:=ptr;

          result:=XML_TOK_INVALID;

         end;

        inc(ptrcomp(ptr ) ,4 );

       end;

     {CHECK_NMSTRT_CASES #define}

      else
       begin
        nextTokPtr^:=ptr;

        result:=XML_TOK_INVALID;

        exit;

       end;

     end;

    end;

  {$ENDIF }

   BT_S ,BT_CR ,BT_LF :
    begin
     repeat
      inc(ptrcomp(ptr ) ,MINBPC(enc ) );

      if ptr <> end_ then
       result:=XML_TOK_PARTIAL;

      t:=BYTE_TYPE(enc ,ptr );

      if t = BT_EQUALS then
       break;

      case t of
       BT_S ,BT_LF ,BT_CR :
        break;

       else
        begin
         nextTokPtr^:=ptr;

         result:=XML_TOK_INVALID;

         exit;

        end;

      end;

     until false;

    { fall through }
     goto _bte;

    end;

   BT_EQUALS :
   _bte:
    begin
    {$IFDEF XML_NS }
     hadColon:=0;

    {$ENDIF }

     repeat
      inc(ptrcomp(ptr ) ,MINBPC(enc ) );

      if ptr = end_ then
       begin
        result:=XML_TOK_PARTIAL;

        exit;

       end;

      open:=BYTE_TYPE(enc ,ptr );

      if (open = BT_QUOT ) or
         (open = BT_APOS ) then
       break;

      case open of
       BT_S ,BT_LF ,BT_CR :
       else
        begin
         nextTokPtr^:=ptr;

         result:=XML_TOK_INVALID;

         exit;

        end;

      end;

     until false;

     inc(ptrcomp(ptr ) ,MINBPC(enc ) );

    { in attribute value }
     repeat
      if ptr = end_ then
       begin
        result:=XML_TOK_PARTIAL;

        exit;

       end;

      t:=BYTE_TYPE(enc ,ptr );

      if t = open then
       break;

      case t of
      {#define INVALID_CASES}
       BT_LEAD2 :
        begin
         if ptrcomp(end_ ) - ptrcomp(ptr ) < 2 then
          begin
           result:=XML_TOK_PARTIAL_CHAR;

           exit;

          end;

         if IS_INVALID_CHAR(enc ,ptr ,2 ) <> 0 then
          begin
           nextTokPtr^:=ptr;

           result:=XML_TOK_INVALID;

           exit;

          end;

         inc(ptrcomp(ptr ) ,2 );

        end;

       BT_LEAD3 :
        begin
         if ptrcomp(end_ ) - ptrcomp(ptr ) < 3 then
          begin
           result:=XML_TOK_PARTIAL_CHAR;

           exit;

          end;

         if IS_INVALID_CHAR(enc ,ptr ,3 ) <> 0 then
          begin
           nextTokPtr^:=ptr;

           result:=XML_TOK_INVALID;

           exit;

          end;

         inc(ptrcomp(ptr ) ,3 );

        end;

       BT_LEAD4 :
        begin
         if ptrcomp(end_ ) - ptrcomp(ptr ) < 4 then
          begin
           result:=XML_TOK_PARTIAL_CHAR;

           exit;

          end;

         if IS_INVALID_CHAR(enc ,ptr ,4 ) <> 0 then
          begin
           nextTokPtr^:=ptr;

           result:=XML_TOK_INVALID;

           exit;

          end;

         inc(ptrcomp(ptr ) ,4 );

        end;

       BT_NONXML ,BT_MALFORM ,BT_TRAIL :
        begin
         nextTokPtr^:=ptr;

         result:=XML_TOK_INVALID;

         exit;

        end;

      {INVALID_CASES #define}

       BT_AMP :
        begin
         tok:=normal_scanRef(enc ,char_ptr(ptrcomp(ptr ) + MINBPC(enc ) ) ,end_ ,@ptr );

         if tok <= 0 then
          begin
           if tok = XML_TOK_INVALID then
            nextTokPtr^:=ptr;

           result:=tok;

           exit;

          end;

        end;

       BT_LT :
        begin
         nextTokPtr^:=ptr;

         result:=XML_TOK_INVALID;

         exit;

        end;

       else
        inc(ptrcomp(ptr ) ,MINBPC(enc ) );

      end;

     until false;

     inc(ptrcomp(ptr ) ,MINBPC(enc ) );

     if ptr = end_ then
      begin
       result:=XML_TOK_PARTIAL;

       exit;

      end;

     case BYTE_TYPE(enc ,ptr ) of
      BT_SOL :
       goto sol;

      BT_GT :
       goto gt;

      BT_S ,BT_CR ,BT_LF :
      else
       begin
        nextTokPtr^:=ptr;

        result:=XML_TOK_INVALID;

        exit;

       end;

     end;

    { ptr points to closing quote }
     repeat
      inc(ptrcomp(ptr ) ,MINBPC(enc ) );

      if ptr = end_ then
       begin
        result:=XML_TOK_PARTIAL;

        exit;

       end;

      case BYTE_TYPE(enc ,ptr ) of
      {#define CHECK_NMSTRT_CASES}
       BT_NONASCII :
        if IS_NMSTRT_CHAR_MINBPC(enc ,ptr ) = 0 then
         begin
          nextTokPtr^:=ptr;

          result:=XML_TOK_INVALID;

          exit;

         end
        else
         goto _bt2;

       BT_NMSTRT ,BT_HEX :
       _bt2:
        inc(ptrcomp(ptr ) ,MINBPC(enc ) );

       BT_LEAD2 :
        begin
         if ptrcomp(end_ ) - ptrcomp(ptr ) < 2 then
          begin
           result:=XML_TOK_PARTIAL_CHAR;

           exit;

          end;

         if not IS_NMSTRT_CHAR(enc ,ptr ,2 ) = 0 then
          begin
           nextTokPtr^:=ptr;

           result:=XML_TOK_INVALID;

          end;

         inc(ptrcomp(ptr ) ,2 );

        end;

       BT_LEAD3 :
        begin
         if ptrcomp(end_ ) - ptrcomp(ptr ) < 3 then
          begin
           result:=XML_TOK_PARTIAL_CHAR;

           exit;

          end;

         if not IS_NMSTRT_CHAR(enc ,ptr ,3 ) = 0 then
          begin
           nextTokPtr^:=ptr;

           result:=XML_TOK_INVALID;

          end;

         inc(ptrcomp(ptr ) ,3 );

        end;

       BT_LEAD4 :
        begin
         if ptrcomp(end_ ) - ptrcomp(ptr ) < 4 then
          begin
           result:=XML_TOK_PARTIAL_CHAR;

           exit;

          end;

         if not IS_NMSTRT_CHAR(enc ,ptr ,4 ) = 0 then
          begin
           nextTokPtr^:=ptr;

           result:=XML_TOK_INVALID;

          end;

         inc(ptrcomp(ptr ) ,4 );

        end;

      {CHECK_NMSTRT_CASES #define}

       BT_S ,BT_CR ,BT_LF :
        continue;

       BT_GT :
       gt:
        begin
         nextTokPtr^:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

         result:=XML_TOK_START_TAG_WITH_ATTS;

         exit;

        end;

       BT_SOL :
       sol:
        begin
         inc(ptrcomp(ptr ) ,MINBPC(enc ) );

         if ptr = end_ then
          begin
           result:=XML_TOK_PARTIAL;

           exit;

          end;

         if CHAR_MATCHES(enc ,ptr ,int(ASCII_GT ) ) = 0 then
          begin
           nextTokPtr^:=ptr;

           result:=XML_TOK_INVALID;

           exit;

          end;

         nextTokPtr^:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

         result:=XML_TOK_EMPTY_ELEMENT_WITH_ATTS;

         exit;

        end;

       else
        begin
         nextTokPtr^:=ptr;

         result:=XML_TOK_INVALID;

         exit;

        end;

      end;

      break;

     until false;

    end;

   else
    begin
     nextTokPtr^:=ptr;

     result:=XML_TOK_INVALID;

     exit;

    end;

  end;

 result:=XML_TOK_PARTIAL;

end;

{ normal_scanEndTag }
{ ptr points to character following "</" }
function normal_scanEndTag(enc : ENCODING_ptr; ptr ,end_ : char_ptr; nextTokPtr : char_ptr_ptr ) : int;
label
 _bt0 ,_bt1 ;

begin
 if ptr = end_ then
  begin
   result:=XML_TOK_PARTIAL;

   exit;

  end;

 case BYTE_TYPE(enc ,ptr ) of
 {#define CHECK_NMSTRT_CASES}
  BT_NONASCII :
   if IS_NMSTRT_CHAR_MINBPC(enc ,ptr ) = 0 then
    begin
     nextTokPtr^:=ptr;

     result:=XML_TOK_INVALID;

     exit;

    end
   else
    goto _bt0;

  BT_NMSTRT ,BT_HEX :
  _bt0:
   inc(ptrcomp(ptr ) ,MINBPC(enc ) );

  BT_LEAD2 :
   begin
    if ptrcomp(end_ ) - ptrcomp(ptr ) < 2 then
     begin
      result:=XML_TOK_PARTIAL_CHAR;

      exit;

     end;

    if not IS_NMSTRT_CHAR(enc ,ptr ,2 ) = 0 then
     begin
      nextTokPtr^:=ptr;

      result:=XML_TOK_INVALID;

     end;

    inc(ptrcomp(ptr ) ,2 );

   end;

  BT_LEAD3 :
   begin
    if ptrcomp(end_ ) - ptrcomp(ptr ) < 3 then
     begin
      result:=XML_TOK_PARTIAL_CHAR;

      exit;

     end;

    if not IS_NMSTRT_CHAR(enc ,ptr ,3 ) = 0 then
     begin
      nextTokPtr^:=ptr;

      result:=XML_TOK_INVALID;

     end;

    inc(ptrcomp(ptr ) ,3 );

   end;

  BT_LEAD4 :
   begin
    if ptrcomp(end_ ) - ptrcomp(ptr ) < 4 then
     begin
      result:=XML_TOK_PARTIAL_CHAR;

      exit;

     end;

    if not IS_NMSTRT_CHAR(enc ,ptr ,4 ) = 0 then
     begin
      nextTokPtr^:=ptr;

      result:=XML_TOK_INVALID;

     end;

    inc(ptrcomp(ptr ) ,4 );

   end;

 {CHECK_NMSTRT_CASES #define}

  else
   begin
    nextTokPtr^:=ptr;

    result:=XML_TOK_INVALID;

    exit;

   end;

 end;

 while ptr <> end_ do
  case BYTE_TYPE(enc ,ptr ) of
  {#define CHECK_NAME_CASES}
   BT_NONASCII :
    if IS_NAME_CHAR_MINBPC(enc ,ptr ) = 0 then
     begin
      nextTokPtr^:=ptr;

      result:=XML_TOK_INVALID;

      exit;

     end
    else
     goto _bt1;

   BT_NMSTRT ,BT_HEX ,BT_DIGIT ,BT_NAME ,BT_MINUS :
   _bt1:
    inc(ptrcomp(ptr ) ,MINBPC(enc ) );

   BT_LEAD2 :
    begin
     if ptrcomp(end_ ) - ptrcomp(ptr ) < 2 then
      begin
       result:=XML_TOK_PARTIAL_CHAR;

       exit;

      end;

     if IS_NAME_CHAR(enc ,ptr ,2 ) = 0 then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

     inc(ptrcomp(ptr ) ,2 );

    end;

   BT_LEAD3 :
    begin
     if ptrcomp(end_ ) - ptrcomp(ptr ) < 3 then
      begin
       result:=XML_TOK_PARTIAL_CHAR;

       exit;

      end;

     if IS_NAME_CHAR(enc ,ptr ,3 ) = 0 then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

     inc(ptrcomp(ptr ) ,3 );

    end;

   BT_LEAD4 :
    begin
     if ptrcomp(end_ ) - ptrcomp(ptr ) < 4 then
      begin
       result:=XML_TOK_PARTIAL_CHAR;

       exit;

      end;

     if IS_NAME_CHAR(enc ,ptr ,4 ) = 0 then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

     inc(ptrcomp(ptr ) ,4 );

    end;

  {CHECK_NAME_CASES #define}

   BT_S ,BT_CR ,BT_LF :
    begin
     inc(ptrcomp(ptr ) ,MINBPC(enc ) );

     while ptr <> end_ do
      begin
       case BYTE_TYPE(enc ,ptr ) of
        BT_GT :
         begin
          nextTokPtr^:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

          result:=XML_TOK_END_TAG;

          exit;

         end;

        BT_S ,BT_CR ,BT_LF :
        else
         begin
          nextTokPtr^:=ptr;

          result:=XML_TOK_INVALID;

          exit;

         end;

       end;

       inc(ptrcomp(ptr ) ,MINBPC(enc ) );

      end;

     result:=XML_TOK_PARTIAL;

     exit;

    end;

  {$IFDEF XML_NS }
   BT_COLON :
   { no need to check qname syntax here,
     since end-tag must match exactly }
    inc(ptrcomp(ptr ) ,MINBPC(enc ) );

  {$ENDIF }

   BT_GT :
    begin
     nextTokPtr^:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

     result:=XML_TOK_END_TAG;

     exit;

    end;

   else
    begin
     nextTokPtr^:=ptr;

     result:=XML_TOK_INVALID;

     exit;

    end;

  end;

 result:=XML_TOK_PARTIAL;

end;

{ normal_scanComment }
{ ptr points to character following "<!-" }
function normal_scanComment(enc : ENCODING_ptr; ptr ,end_ : char_ptr; nextTokPtr : char_ptr_ptr ) : int;
begin
 if ptr <> end_ then
  begin
   if CHAR_MATCHES(enc ,ptr ,int(ASCII_MINUS ) ) = 0 then
    begin
     nextTokPtr^:=ptr;

     result:=XML_TOK_INVALID;

     exit;

    end;

   inc(ptrcomp(ptr ) ,MINBPC(enc ) );

   while ptr <> end_ do
    case BYTE_TYPE(enc ,ptr ) of
    {#define INVALID_CASES}
     BT_LEAD2 :
      begin
       if ptrcomp(end_ ) - ptrcomp(ptr ) < 2 then
        begin
         result:=XML_TOK_PARTIAL_CHAR;

         exit;

        end;

       if IS_INVALID_CHAR(enc ,ptr ,2 ) <> 0 then
        begin
         nextTokPtr^:=ptr;

         result:=XML_TOK_INVALID;

         exit;

        end;

       inc(ptrcomp(ptr ) ,2 );

      end;

     BT_LEAD3 :
      begin
       if ptrcomp(end_ ) - ptrcomp(ptr ) < 3 then
        begin
         result:=XML_TOK_PARTIAL_CHAR;

         exit;

        end;

       if IS_INVALID_CHAR(enc ,ptr ,3 ) <> 0 then
        begin
         nextTokPtr^:=ptr;

         result:=XML_TOK_INVALID;

         exit;

        end;

       inc(ptrcomp(ptr ) ,3 );

      end;

     BT_LEAD4 :
      begin
       if ptrcomp(end_ ) - ptrcomp(ptr ) < 4 then
        begin
         result:=XML_TOK_PARTIAL_CHAR;

         exit;

        end;

       if IS_INVALID_CHAR(enc ,ptr ,4 ) <> 0 then
        begin
         nextTokPtr^:=ptr;

         result:=XML_TOK_INVALID;

         exit;

        end;

       inc(ptrcomp(ptr ) ,4 );

      end;

     BT_NONXML ,BT_MALFORM ,BT_TRAIL :
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

    {INVALID_CASES #define}

     BT_MINUS :
      begin
       inc(ptrcomp(ptr ) ,MINBPC(enc ) );

       if ptr = end_ then
        begin
         result:=XML_TOK_PARTIAL;

         exit;

        end;

       if CHAR_MATCHES(enc ,ptr ,int(ASCII_MINUS ) ) <> 0 then
        begin
         inc(ptrcomp(ptr ) ,MINBPC(enc ) );

         if ptr = end_ then
          begin
           result:=XML_TOK_PARTIAL;

           exit;

          end;

         if CHAR_MATCHES(enc ,ptr ,int(ASCII_GT ) ) = 0 then
          begin
           nextTokPtr^:=ptr;

           result:=XML_TOK_INVALID;

           exit;

          end;

         nextTokPtr^:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

         result:=XML_TOK_COMMENT;

         exit;

        end;

      end;

     else
      inc(ptrcomp(ptr ) ,MINBPC(enc ) );

    end;

  end;

 result:=XML_TOK_PARTIAL;

end;

{ normal_scanCdataSection {..}
function normal_scanCdataSection(enc : ENCODING_ptr; ptr ,end_ : char_ptr; nextTokPtr : char_ptr_ptr ) : int;
begin
end;

{ normal_checkPiTarget }
function normal_checkPiTarget(enc : ENCODING_ptr; ptr ,end_ : char_ptr; tokPtr : int_ptr ) : int;
var
 upper : int;

begin
 upper  :=0;
 tokPtr^:=XML_TOK_PI;

 if ptrcomp(end_ ) - ptrcomp(ptr ) <> MINBPC(enc ) * 3 then
  begin
   result:=1;

   exit;

  end;

 case BYTE_TO_ASCII(enc ,ptr ) of
  int(ASCII_X ) :
   upper:=1;

  int(ASCII_xl ) :
  else
   begin
    result:=1;

    exit;

   end;

 end;

 inc(ptrcomp(ptr ) ,MINBPC(enc ) );

 case BYTE_TO_ASCII(enc ,ptr ) of
  int(ASCII_M ) :
   upper:=1;

  int(ASCII_ml ) :
  else
   begin
    result:=1;

    exit;

   end;

 end;

 inc(ptrcomp(ptr ) ,MINBPC(enc ) );

 case BYTE_TO_ASCII(enc ,ptr ) of
  int(ASCII_L ) :
   upper:=1;

  int(ASCII_ll ) :
  else
   begin
    result:=1;

    exit;

   end;

 end;

 if upper <> 0 then
  begin
   result:=0;

   exit;

  end;

 tokPtr^:=XML_TOK_XML_DECL;
 result :=1;

end;

{ normal_scanPi }
{ ptr points to character following "<?" }
function normal_scanPi(enc : ENCODING_ptr; ptr ,end_ : char_ptr; nextTokPtr : char_ptr_ptr ) : int;
var
 tok : int;

 target : char_ptr;

label
 _bt0 ,_bt1 ,_else ;

begin
 target:=ptr;

 if ptr = end_ then
  begin
   result:=XML_TOK_PARTIAL;

   exit;

  end;

 case BYTE_TYPE(enc ,ptr ) of
 {#define CHECK_NMSTRT_CASES}
  BT_NONASCII :
   if IS_NMSTRT_CHAR_MINBPC(enc ,ptr ) = 0 then
    begin
     nextTokPtr^:=ptr;

     result:=XML_TOK_INVALID;

     exit;

    end
   else
    goto _bt0;

  BT_NMSTRT ,BT_HEX :
  _bt0:
   inc(ptrcomp(ptr ) ,MINBPC(enc ) );

  BT_LEAD2 :
   begin
    if ptrcomp(end_ ) - ptrcomp(ptr ) < 2 then
     begin
      result:=XML_TOK_PARTIAL_CHAR;

      exit;

     end;

    if not IS_NMSTRT_CHAR(enc ,ptr ,2 ) = 0 then
     begin
      nextTokPtr^:=ptr;

      result:=XML_TOK_INVALID;

     end;

    inc(ptrcomp(ptr ) ,2 );

   end;

  BT_LEAD3 :
   begin
    if ptrcomp(end_ ) - ptrcomp(ptr ) < 3 then
     begin
      result:=XML_TOK_PARTIAL_CHAR;

      exit;

     end;

    if not IS_NMSTRT_CHAR(enc ,ptr ,3 ) = 0 then
     begin
      nextTokPtr^:=ptr;

      result:=XML_TOK_INVALID;

     end;

    inc(ptrcomp(ptr ) ,3 );

   end;

  BT_LEAD4 :
   begin
    if ptrcomp(end_ ) - ptrcomp(ptr ) < 4 then
     begin
      result:=XML_TOK_PARTIAL_CHAR;

      exit;

     end;

    if not IS_NMSTRT_CHAR(enc ,ptr ,4 ) = 0 then
     begin
      nextTokPtr^:=ptr;

      result:=XML_TOK_INVALID;

     end;

    inc(ptrcomp(ptr ) ,4 );

   end;

 {CHECK_NMSTRT_CASES #define}

  else
   begin
    nextTokPtr^:=ptr;

    result:=XML_TOK_INVALID;

    exit;

   end;

 end;

 while ptr <> end_ do
  case BYTE_TYPE(enc ,ptr ) of
  {#define CHECK_NAME_CASES}
   BT_NONASCII :
    if IS_NAME_CHAR_MINBPC(enc ,ptr ) = 0 then
     begin
      nextTokPtr^:=ptr;

      result:=XML_TOK_INVALID;

      exit;

     end
    else
     goto _bt1;

   BT_NMSTRT ,BT_HEX ,BT_DIGIT ,BT_NAME ,BT_MINUS :
   _bt1:
    inc(ptrcomp(ptr ) ,MINBPC(enc ) );

   BT_LEAD2 :
    begin
     if ptrcomp(end_ ) - ptrcomp(ptr ) < 2 then
      begin
       result:=XML_TOK_PARTIAL_CHAR;

       exit;

      end;

     if IS_NAME_CHAR(enc ,ptr ,2 ) = 0 then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

     inc(ptrcomp(ptr ) ,2 );

    end;

   BT_LEAD3 :
    begin
     if ptrcomp(end_ ) - ptrcomp(ptr ) < 3 then
      begin
       result:=XML_TOK_PARTIAL_CHAR;

       exit;

      end;

     if IS_NAME_CHAR(enc ,ptr ,3 ) = 0 then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

     inc(ptrcomp(ptr ) ,3 );

    end;

   BT_LEAD4 :
    begin
     if ptrcomp(end_ ) - ptrcomp(ptr ) < 4 then
      begin
       result:=XML_TOK_PARTIAL_CHAR;

       exit;

      end;

     if IS_NAME_CHAR(enc ,ptr ,4 ) = 0 then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

     inc(ptrcomp(ptr ) ,4 );

    end;

  {CHECK_NAME_CASES #define}

   BT_S ,BT_CR ,BT_LF :
    begin
     if normal_checkPiTarget(enc ,target ,ptr ,@tok ) = 0 then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

     inc(ptrcomp(ptr ) ,MINBPC(enc ) );

     while ptr <> end_ do
      case BYTE_TYPE(enc ,ptr ) of
      {#define INVALID_CASES}
       BT_LEAD2 :
        begin
         if ptrcomp(end_ ) - ptrcomp(ptr ) < 2 then
          begin
           result:=XML_TOK_PARTIAL_CHAR;

           exit;

          end;

         if IS_INVALID_CHAR(enc ,ptr ,2 ) <> 0 then
          begin
           nextTokPtr^:=ptr;

           result:=XML_TOK_INVALID;

           exit;

          end;

         inc(ptrcomp(ptr ) ,2 );

        end;

       BT_LEAD3 :
        begin
         if ptrcomp(end_ ) - ptrcomp(ptr ) < 3 then
          begin
           result:=XML_TOK_PARTIAL_CHAR;

           exit;

          end;

         if IS_INVALID_CHAR(enc ,ptr ,3 ) <> 0 then
          begin
           nextTokPtr^:=ptr;

           result:=XML_TOK_INVALID;

           exit;

          end;

         inc(ptrcomp(ptr ) ,3 );

        end;

       BT_LEAD4 :
        begin
         if ptrcomp(end_ ) - ptrcomp(ptr ) < 4 then
          begin
           result:=XML_TOK_PARTIAL_CHAR;

           exit;

          end;

         if IS_INVALID_CHAR(enc ,ptr ,4 ) <> 0 then
          begin
           nextTokPtr^:=ptr;

           result:=XML_TOK_INVALID;

           exit;

          end;

         inc(ptrcomp(ptr ) ,4 );

        end;

       BT_NONXML ,BT_MALFORM ,BT_TRAIL :
        begin
         nextTokPtr^:=ptr;

         result:=XML_TOK_INVALID;

         exit;

        end;

      {INVALID_CASES #define}

       BT_QUEST :
        begin
         inc(ptrcomp(ptr ) ,MINBPC(enc ) );

         if ptr = end_ then
          begin
           result:=XML_TOK_PARTIAL;

           exit;

          end;

         if CHAR_MATCHES(enc ,ptr ,int(ASCII_GT ) ) <> 0 then
          begin
           nextTokPtr^:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

           result:=tok;

           exit;

          end;

        end;

       else
        inc(ptrcomp(ptr ) ,MINBPC(enc ) );

      end;

     result:=XML_TOK_PARTIAL;

     exit;

    end;

   BT_QUEST :
    begin
     if normal_checkPiTarget(enc ,target ,ptr ,@tok ) = 0 then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

     inc(ptrcomp(ptr ) ,MINBPC(enc ) );

     if ptr = end_ then
      begin
       result:=XML_TOK_PARTIAL;

       exit;

      end;

     if CHAR_MATCHES(enc ,ptr ,int(ASCII_GT ) ) <> 0 then
      begin
       nextTokPtr^:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

       result:=tok;

       exit;

      end;

    { fall through }
     goto _else;

    end;

   else
    begin
    _else:
     nextTokPtr^:=ptr;

     result:=XML_TOK_INVALID;

     exit;

    end;

  end;

 result:=XML_TOK_PARTIAL; 

end;

{ normal_scanLt }
{ ptr points to character following "<" }
function normal_scanLt(enc : ENCODING_ptr; ptr ,end_ : char_ptr; nextTokPtr : char_ptr_ptr ) : int;
{$IFDEF XML_NS }
var
 hadColon : int;

{$ENDIF }

label
 _bt0 ,_bt1 ,_bt2 ,_bt3 ,gt ,sol ;

begin
 if ptr = end_ then
  begin
   result:=XML_TOK_PARTIAL;

   exit;

  end;

 case BYTE_TYPE(enc ,ptr ) of
 {#define CHECK_NMSTRT_CASES}
  BT_NONASCII :
   if IS_NMSTRT_CHAR_MINBPC(enc ,ptr ) = 0 then
    begin
     nextTokPtr^:=ptr;

     result:=XML_TOK_INVALID;

     exit;

    end
   else
    goto _bt0;

  BT_NMSTRT ,BT_HEX :
  _bt0:
   inc(ptrcomp(ptr ) ,MINBPC(enc ) );

  BT_LEAD2 :
   begin
    if ptrcomp(end_ ) - ptrcomp(ptr ) < 2 then
     begin
      result:=XML_TOK_PARTIAL_CHAR;

      exit;

     end;

    if not IS_NMSTRT_CHAR(enc ,ptr ,2 ) = 0 then
     begin
      nextTokPtr^:=ptr;

      result:=XML_TOK_INVALID;

     end;

    inc(ptrcomp(ptr ) ,2 );

   end;

  BT_LEAD3 :
   begin
    if ptrcomp(end_ ) - ptrcomp(ptr ) < 3 then
     begin
      result:=XML_TOK_PARTIAL_CHAR;

      exit;

     end;

    if not IS_NMSTRT_CHAR(enc ,ptr ,3 ) = 0 then
     begin
      nextTokPtr^:=ptr;

      result:=XML_TOK_INVALID;

     end;

    inc(ptrcomp(ptr ) ,3 );

   end;

  BT_LEAD4 :
   begin
    if ptrcomp(end_ ) - ptrcomp(ptr ) < 4 then
     begin
      result:=XML_TOK_PARTIAL_CHAR;

      exit;

     end;

    if not IS_NMSTRT_CHAR(enc ,ptr ,4 ) = 0 then
     begin
      nextTokPtr^:=ptr;

      result:=XML_TOK_INVALID;

     end;

    inc(ptrcomp(ptr ) ,4 );

   end;

 {CHECK_NMSTRT_CASES #define}

  BT_EXCL :
   begin
    inc(ptrcomp(ptr ) ,MINBPC(enc ) );

    if ptr = end_ then
     begin
      result:=XML_TOK_PARTIAL;

      exit;

     end;

    case BYTE_TYPE(enc ,ptr ) of
     BT_MINUS:
      begin
       result:=normal_scanComment(enc ,char_ptr(ptrcomp(ptr ) + MINBPC(enc ) ) ,end_ ,nextTokPtr );

       exit;

      end;

     BT_LSQB:
      begin
       result:=normal_scanCdataSection(enc ,char_ptr(ptrcomp(ptr ) + MINBPC(enc ) ) ,end_ ,nextTokPtr );

       exit;

      end;

    end;

    nextTokPtr^:=ptr;

    result:=XML_TOK_INVALID;

    exit;

   end;

  BT_QUEST :
   begin
    result:=normal_scanPi(enc ,char_ptr(ptrcomp(ptr ) + MINBPC(enc ) ) ,end_ ,nextTokPtr );

    exit;

   end;

  BT_SOL :
   begin
    result:=normal_scanEndTag(enc ,char_ptr(ptrcomp(ptr ) + MINBPC(enc ) ) ,end_ ,nextTokPtr );

    exit;

   end;

  else
   begin
    nextTokPtr^:=ptr;

    result:=XML_TOK_INVALID;

    exit;

   end;

 end;

{$IFDEF XML_NS }
 hadColon:=0;

{$ENDIF }

{ we have a start-tag }
 while ptr <> end_ do
  case BYTE_TYPE(enc ,ptr ) of
  {#define CHECK_NAME_CASES}
   BT_NONASCII :
    if IS_NAME_CHAR_MINBPC(enc ,ptr ) = 0 then
     begin
      nextTokPtr^:=ptr;

      result:=XML_TOK_INVALID;

      exit;

     end
    else
     goto _bt1;

   BT_NMSTRT ,BT_HEX ,BT_DIGIT ,BT_NAME ,BT_MINUS :
   _bt1:
    inc(ptrcomp(ptr ) ,MINBPC(enc ) );

   BT_LEAD2 :
    begin
     if ptrcomp(end_ ) - ptrcomp(ptr ) < 2 then
      begin
       result:=XML_TOK_PARTIAL_CHAR;

       exit;

      end;

     if IS_NAME_CHAR(enc ,ptr ,2 ) = 0 then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

     inc(ptrcomp(ptr ) ,2 );

    end;

   BT_LEAD3 :
    begin
     if ptrcomp(end_ ) - ptrcomp(ptr ) < 3 then
      begin
       result:=XML_TOK_PARTIAL_CHAR;

       exit;

      end;

     if IS_NAME_CHAR(enc ,ptr ,3 ) = 0 then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

     inc(ptrcomp(ptr ) ,3 );

    end;

   BT_LEAD4 :
    begin
     if ptrcomp(end_ ) - ptrcomp(ptr ) < 4 then
      begin
       result:=XML_TOK_PARTIAL_CHAR;

       exit;

      end;

     if IS_NAME_CHAR(enc ,ptr ,4 ) = 0 then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

     inc(ptrcomp(ptr ) ,4 );

    end;

  {CHECK_NAME_CASES #define}

  {$IFDEF XML_NS }
   BT_COLON :
    begin
     if hadColon <> 0 then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

     hadColon:=1;

     inc(ptrcomp(ptr ) ,MINBPC(enc ) );

     if ptr = end_ then
      begin
       result:=XML_TOK_PARTIAL;

       exit;

      end;

     case BYTE_TYPE(enc ,ptr ) of
     {#define CHECK_NMSTRT_CASES}
      BT_NONASCII :
       if IS_NMSTRT_CHAR_MINBPC(enc ,ptr ) = 0 then
        begin
         nextTokPtr^:=ptr;

         result:=XML_TOK_INVALID;

         exit;

        end
       else
        goto _bt2;

      BT_NMSTRT ,BT_HEX :
      _bt2:
       inc(ptrcomp(ptr ) ,MINBPC(enc ) );

      BT_LEAD2 :
       begin
        if ptrcomp(end_ ) - ptrcomp(ptr ) < 2 then
         begin
          result:=XML_TOK_PARTIAL_CHAR;

          exit;

         end;

        if not IS_NMSTRT_CHAR(enc ,ptr ,2 ) = 0 then
         begin
          nextTokPtr^:=ptr;

          result:=XML_TOK_INVALID;

         end;

        inc(ptrcomp(ptr ) ,2 );

       end;

      BT_LEAD3 :
       begin
        if ptrcomp(end_ ) - ptrcomp(ptr ) < 3 then
         begin
          result:=XML_TOK_PARTIAL_CHAR;

          exit;

         end;

        if not IS_NMSTRT_CHAR(enc ,ptr ,3 ) = 0 then
         begin
          nextTokPtr^:=ptr;

          result:=XML_TOK_INVALID;

         end;

        inc(ptrcomp(ptr ) ,3 );

       end;

      BT_LEAD4 :
       begin
        if ptrcomp(end_ ) - ptrcomp(ptr ) < 4 then
         begin
          result:=XML_TOK_PARTIAL_CHAR;

          exit;

         end;

        if not IS_NMSTRT_CHAR(enc ,ptr ,4 ) = 0 then
         begin
          nextTokPtr^:=ptr;

          result:=XML_TOK_INVALID;

         end;

        inc(ptrcomp(ptr ) ,4 );

       end;

     {CHECK_NMSTRT_CASES #define}

      else
       begin
        nextTokPtr^:=ptr;

        result:=XML_TOK_INVALID;

        exit;

       end;

     end;

    end;

  {$ENDIF }
  
   BT_S ,BT_CR ,BT_LF :
    begin
     inc(ptrcomp(ptr ) ,MINBPC(enc ) );

     while ptr <> end_ do
      begin
       case BYTE_TYPE(enc ,ptr ) of
       {#define CHECK_NMSTRT_CASES}
        BT_NONASCII :
         if IS_NMSTRT_CHAR_MINBPC(enc ,ptr ) = 0 then
          begin
           nextTokPtr^:=ptr;

           result:=XML_TOK_INVALID;

           exit;

          end
         else
          goto _bt3;

        BT_NMSTRT ,BT_HEX :
        _bt3:
         inc(ptrcomp(ptr ) ,MINBPC(enc ) );

        BT_LEAD2 :
         begin
          if ptrcomp(end_ ) - ptrcomp(ptr ) < 2 then
           begin
            result:=XML_TOK_PARTIAL_CHAR;

            exit;

           end;

          if not IS_NMSTRT_CHAR(enc ,ptr ,2 ) = 0 then
           begin
            nextTokPtr^:=ptr;

            result:=XML_TOK_INVALID;

           end;

          inc(ptrcomp(ptr ) ,2 );

         end;

        BT_LEAD3 :
         begin
          if ptrcomp(end_ ) - ptrcomp(ptr ) < 3 then
           begin
            result:=XML_TOK_PARTIAL_CHAR;

            exit;

           end;

          if not IS_NMSTRT_CHAR(enc ,ptr ,3 ) = 0 then
           begin
            nextTokPtr^:=ptr;

            result:=XML_TOK_INVALID;

           end;

          inc(ptrcomp(ptr ) ,3 );

         end;

        BT_LEAD4 :
         begin
          if ptrcomp(end_ ) - ptrcomp(ptr ) < 4 then
           begin
            result:=XML_TOK_PARTIAL_CHAR;

            exit;

           end;

          if not IS_NMSTRT_CHAR(enc ,ptr ,4 ) = 0 then
           begin
            nextTokPtr^:=ptr;

            result:=XML_TOK_INVALID;

           end;

          inc(ptrcomp(ptr ) ,4 );

         end;

       {CHECK_NMSTRT_CASES #define}

        BT_GT :
         goto gt;

        BT_SOL :
         goto sol;

        BT_S ,BT_CR ,BT_LF :
         begin
          inc(ptrcomp(ptr ) ,MINBPC(enc ) );

          continue;

         end;

        else
         begin
          nextTokPtr^:=ptr;

          result:=XML_TOK_INVALID;

          exit;

         end;

       end;

       result:=normal_scanAtts(enc ,ptr ,end_ ,nextTokPtr );

       exit;

      end;

     result:=XML_TOK_PARTIAL;

     exit;

    end;

   BT_GT :
   gt:
    begin
     nextTokPtr^:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

     result:=XML_TOK_START_TAG_NO_ATTS;

     exit;

    end;

   BT_SOL :
   sol:
    begin
     inc(ptrcomp(ptr ) ,MINBPC(enc ) );

     if ptr <> end_ then
      begin
       result:=XML_TOK_PARTIAL;

       exit;

      end;

     if CHAR_MATCHES(enc ,ptr ,int(ASCII_GT ) ) = 0 then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

     nextTokPtr^:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

     result:=XML_TOK_EMPTY_ELEMENT_NO_ATTS;

     exit;

    end;

   else
    begin
     nextTokPtr^:=ptr;

     result:=XML_TOK_INVALID;

     exit;

    end;

  end;

 result:=XML_TOK_PARTIAL;

end;

{ normal_scanDecl }
{ ptr points to character following "<!" }
function normal_scanDecl(enc : ENCODING_ptr; ptr ,end_ : char_ptr; nextTokPtr : char_ptr_ptr ) : int;
label
 _fall0 ;

begin
 if ptr = end_ then
  begin
   result:=XML_TOK_PARTIAL;

   exit;

  end;

 case BYTE_TYPE(enc ,ptr ) of
  BT_MINUS :
   begin
    result:=normal_scanComment(enc ,char_ptr(ptrcomp(ptr ) + MINBPC(enc ) ) ,end_ ,nextTokPtr );

    exit;

   end;

  BT_LSQB :
   begin
    nextTokPtr^:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

    result:=XML_TOK_COND_SECT_OPEN;

    exit;

   end;

  BT_NMSTRT ,BT_HEX:
   inc(ptrcomp(ptr ) ,MINBPC(enc ) );

  else
   begin
    nextTokPtr^:=ptr;

    result:=XML_TOK_INVALID;

    exit;

   end;

 end;

 while ptr <> end_ do
  case BYTE_TYPE(enc ,ptr ) of
   BT_PERCNT :
    begin
     if ptrcomp(ptr ) + MINBPC(enc ) = ptrcomp(end_ ) then
      begin
       result:=XML_TOK_PARTIAL;

       exit;

      end;

    { don't allow <!ENTITY% foo "whatever"> }
     case BYTE_TYPE(enc ,char_ptr(ptrcomp(ptr ) + MINBPC(enc ) ) ) of
      BT_S ,BT_CR ,BT_LF ,BT_PERCNT :
       begin
        nextTokPtr^:=ptr;

        result:=XML_TOK_INVALID;

        exit;

       end;

     end;

    { fall through }
     goto _fall0;

    end;

   BT_S ,BT_CR ,BT_LF :
   _fall0:
    begin
     nextTokPtr^:=ptr;

     result:=XML_TOK_DECL_OPEN;

     exit;

    end;

   BT_NMSTRT ,BT_HEX :
    inc(ptrcomp(ptr ) ,MINBPC(enc ) );

   else
    begin
     nextTokPtr^:=ptr;

     result:=XML_TOK_INVALID;

     exit;

    end;

  end;

 result:=XML_TOK_PARTIAL;

end;

{ scanPercent {..}
{ ptr points to character following "%" }
function scanPercent(enc : ENCODING_ptr; ptr ,end_ : char_ptr; nextTokPtr : char_ptr_ptr ) : int;
begin
end;

{ scanPoundName {..}
function scanPoundName(enc : ENCODING_ptr; ptr ,end_ : char_ptr; nextTokPtr : char_ptr_ptr ) : int;
begin
end;

{ normal_scanLit }
function normal_scanLit(
          open : int; enc : ENCODING_ptr;
          ptr ,end_ : char_ptr;
          nextTokPtr : char_ptr_ptr ) : int;
var
 t : int;

label
 _break ;  

begin
 while ptr <> end_ do
  begin
   t:=BYTE_TYPE(enc ,ptr );

   case t of
   {#define INVALID_CASES}
    BT_LEAD2 :
     begin
      if ptrcomp(end_ ) - ptrcomp(ptr ) < 2 then
       begin
        result:=XML_TOK_PARTIAL_CHAR;

        exit;

       end;

      if IS_INVALID_CHAR(enc ,ptr ,2 ) <> 0 then
       begin
        nextTokPtr^:=ptr;

        result:=XML_TOK_INVALID;

        exit;

       end;

      inc(ptrcomp(ptr ) ,2 );

     end;

    BT_LEAD3 :
     begin
      if ptrcomp(end_ ) - ptrcomp(ptr ) < 3 then
       begin
        result:=XML_TOK_PARTIAL_CHAR;

        exit;

       end;

      if IS_INVALID_CHAR(enc ,ptr ,3 ) <> 0 then
       begin
        nextTokPtr^:=ptr;

        result:=XML_TOK_INVALID;

        exit;

       end;

      inc(ptrcomp(ptr ) ,3 );

     end;

    BT_LEAD4 :
     begin
      if ptrcomp(end_ ) - ptrcomp(ptr ) < 4 then
       begin
        result:=XML_TOK_PARTIAL_CHAR;

        exit;

       end;

      if IS_INVALID_CHAR(enc ,ptr ,4 ) <> 0 then
       begin
        nextTokPtr^:=ptr;

        result:=XML_TOK_INVALID;

        exit;

       end;

      inc(ptrcomp(ptr ) ,4 );

     end;

    BT_NONXML ,BT_MALFORM ,BT_TRAIL :
     begin
      nextTokPtr^:=ptr;

      result:=XML_TOK_INVALID;

      exit;

     end;

   {INVALID_CASES #define}

    BT_QUOT ,BT_APOS :
     begin
      inc(ptrcomp(ptr ) ,MINBPC(enc ) );

      if t <> open then
       goto _break;

      if ptr = end_ then
       begin
        result:=-XML_TOK_LITERAL;

        exit;

       end;

      nextTokPtr^:=ptr;

      case BYTE_TYPE(enc ,ptr ) of
       BT_S ,BT_CR ,BT_LF ,BT_GT ,BT_PERCNT ,BT_LSQB :
        begin
         result:=XML_TOK_LITERAL;

         exit;

        end;

       else
        begin
         result:=XML_TOK_INVALID;

         exit;

        end;

      end;

     end;

    else
     inc(ptrcomp(ptr ) ,MINBPC(enc ) );

   end;

  _break:
  end;

 result:=XML_TOK_PARTIAL;

end;

{ normal_prologTok }
function normal_prologTok(enc : ENCODING_ptr; ptr ,end_ : char_ptr; nextTokPtr : char_ptr_ptr ) : int;
var
 tok : int;

 n : size_t;

label
 _bt_s ,_else ,_else2 ,_bt0 ,_bt1 ;

begin
 if ptr = end_ then
  begin
   result:=XML_TOK_NONE;

   exit;

  end;

 if MINBPC(enc ) > 1 then
  begin
   n:=ptrcomp(end_ ) - ptrcomp(ptr );

   if n and (MINBPC(enc ) - 1 ) <> 0 then
    begin
     n:=n and not(MINBPC(enc ) - 1 );

     if n = 0 then
      begin
       result:=XML_TOK_PARTIAL;

       exit;

      end;

     end_:=char_ptr(ptrcomp(ptr ) + n );

    end;

  end;

 case BYTE_TYPE(enc ,ptr ) of
  BT_QUOT :
   begin
    result:=normal_scanLit(BT_QUOT ,enc ,char_ptr(ptrcomp(ptr ) + MINBPC(enc ) ) ,end_ ,nextTokPtr );

    exit;

   end;

  BT_APOS :
   begin
    result:=normal_scanLit(BT_APOS ,enc ,char_ptr(ptrcomp(ptr ) + MINBPC(enc ) ) ,end_ ,nextTokPtr );

    exit;

   end;

  BT_LT :
   begin
    inc(ptrcomp(ptr ) ,MINBPC(enc ) );

    if ptr = end_ then
     begin
      result:=XML_TOK_PARTIAL;

      exit;

     end;

    case BYTE_TYPE(enc ,ptr ) of
     BT_EXCL :
      begin
       result:=normal_scanDecl(enc ,char_ptr(ptrcomp(ptr ) + MINBPC(enc ) ) ,end_ ,nextTokPtr );

       exit;

      end;

     BT_QUEST :
      begin
       result:=normal_scanPi(enc ,char_ptr(ptrcomp(ptr ) + MINBPC(enc ) ) ,end_ ,nextTokPtr );

       exit;

      end;

     BT_NMSTRT ,BT_HEX ,BT_NONASCII ,BT_LEAD2 ,BT_LEAD3 ,BT_LEAD4 :
      begin
       nextTokPtr^:=char_ptr(ptrcomp(ptr ) - MINBPC(enc ) );

       result:=XML_TOK_INSTANCE_START;

       exit;

      end;

    end;

    nextTokPtr^:=ptr;

    result:=XML_TOK_INVALID;

    exit;

   end;

  BT_CR :
   if ptrcomp(ptr ) + MINBPC(enc ) = ptrcomp(end_ ) then
    begin
     nextTokPtr^:=end_;

    { indicate that this might be part of a CR/LF pair }
     result:=-XML_TOK_PROLOG_S;

     exit;

    end
   else
   { fall through }
    goto _bt_s;

  BT_S ,BT_LF :
  _bt_s:
   begin
    repeat
     inc(ptrcomp(ptr ) ,MINBPC(enc ) );

     if ptr = end_ then
      break;

     case BYTE_TYPE(enc ,ptr ) of
      BT_CR :
      { don't split CR/LF pair }
       if ptrcomp(ptr ) + MINBPC(enc ) <> ptrcomp(end_ ) then
       else
       { fall through }
        goto _else;

      BT_S ,BT_LF :
      else
       begin
       _else:
        nextTokPtr^:=ptr;

        result:=XML_TOK_PROLOG_S;

        exit;

       end;

     end;

    until false;

    nextTokPtr^:=ptr;

    result:=XML_TOK_PROLOG_S;

    exit;

   end;

  BT_PERCNT :
   begin
    result:=scanPercent(enc ,char_ptr(ptrcomp(ptr ) + MINBPC(enc ) ) ,end_ ,nextTokPtr );

    exit;

   end;

  BT_COMMA :
   begin
    nextTokPtr^:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

    result:=XML_TOK_COMMA;

    exit;

   end;

  BT_LSQB :
   begin
    nextTokPtr^:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

    result:=XML_TOK_OPEN_BRACKET;

    exit;

   end;

  BT_RSQB :
   begin
    inc(ptrcomp(ptr ) ,MINBPC(enc ) );

    if ptr = end_ then
     begin
      result:=-XML_TOK_CLOSE_BRACKET;

      exit;

     end;

    if CHAR_MATCHES(enc ,ptr ,int(ASCII_RSQB ) ) <> 0 then
     begin
      if ptrcomp(ptr ) + MINBPC(enc ) = ptrcomp(end_ ) then
       begin
        result:=XML_TOK_PARTIAL;

        exit;

       end;

      if CHAR_MATCHES(enc ,char_ptr(ptrcomp(ptr ) + MINBPC(enc ) ) ,int(ASCII_GT ) ) <> 0 then
       begin
        nextTokPtr^:=char_ptr(ptrcomp(ptr ) + 2 * MINBPC(enc ) );

        result:=XML_TOK_COND_SECT_CLOSE;

        exit;

       end;

     end;

    nextTokPtr^:=ptr;

    result:=XML_TOK_CLOSE_BRACKET;

    exit;

   end;

  BT_LPAR :
   begin
    nextTokPtr^:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

    result:=XML_TOK_OPEN_PAREN;

    exit;

   end;

  BT_RPAR :
   begin
    inc(ptrcomp(ptr ) ,MINBPC(enc ) );

    if ptr = end_ then
     begin
      result:=-XML_TOK_CLOSE_PAREN;

      exit;

     end;

    case BYTE_TYPE(enc ,ptr ) of
     BT_AST :
      begin
       nextTokPtr^:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

       result:=XML_TOK_CLOSE_PAREN_ASTERISK;

       exit;

      end;

     BT_QUEST :
      begin
       nextTokPtr^:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

       result:=XML_TOK_CLOSE_PAREN_QUESTION;

       exit;

      end;

     BT_PLUS :
      begin
       nextTokPtr^:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

       result:=XML_TOK_CLOSE_PAREN_PLUS;

       exit;

      end;

     BT_CR ,BT_LF ,BT_S ,BT_GT ,BT_COMMA ,BT_VERBAR ,BT_RPAR :
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_CLOSE_PAREN;

       exit;

      end;

    end;

    nextTokPtr^:=ptr;

    result:=XML_TOK_INVALID;

    exit;

   end;

  BT_VERBAR :
   begin
    nextTokPtr^:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

    result:=XML_TOK_OR;

    exit;

   end;

  BT_GT :
   begin
    nextTokPtr^:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

    result:=XML_TOK_DECL_CLOSE;

    exit;

   end;

  BT_NUM :
   begin
    result:=scanPoundName(enc ,char_ptr(ptrcomp(ptr ) + MINBPC(enc ) ) ,end_ ,nextTokPtr );

    exit;

   end;

  BT_LEAD2 :
   begin
    if ptrcomp(end_ ) - ptrcomp(ptr ) < 2 then
     begin
      result:=XML_TOK_PARTIAL_CHAR;

      exit;

     end;

    if IS_NMSTRT_CHAR(enc ,ptr ,2 ) <> 0 then
     begin
      inc(ptrcomp(ptr ) ,2 );

      tok:=XML_TOK_NAME;

     end
    else
     if IS_NAME_CHAR(enc ,ptr ,2 ) <> 0 then
      begin
       inc(ptrcomp(ptr ) ,2 );

       tok:=XML_TOK_NMTOKEN;

      end
     else
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

   end;

  BT_LEAD3 :
   begin
    if ptrcomp(end_ ) - ptrcomp(ptr ) < 3 then
     begin
      result:=XML_TOK_PARTIAL_CHAR;

      exit;

     end;

    if IS_NMSTRT_CHAR(enc ,ptr ,3 ) <> 0 then
     begin
      inc(ptrcomp(ptr ) ,3 );

      tok:=XML_TOK_NAME;

     end
    else
     if IS_NAME_CHAR(enc ,ptr ,3 ) <> 0 then
      begin
       inc(ptrcomp(ptr ) ,3 );

       tok:=XML_TOK_NMTOKEN;

      end
     else
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

   end;

  BT_LEAD4 :
   begin
    if ptrcomp(end_ ) - ptrcomp(ptr ) < 4 then
     begin
      result:=XML_TOK_PARTIAL_CHAR;

      exit;

     end;

    if IS_NMSTRT_CHAR(enc ,ptr ,4 ) <> 0 then
     begin
      inc(ptrcomp(ptr ) ,4 );

      tok:=XML_TOK_NAME;

     end
    else
     if IS_NAME_CHAR(enc ,ptr ,4 ) <> 0 then
      begin
       inc(ptrcomp(ptr ) ,4 );

       tok:=XML_TOK_NMTOKEN;

      end
     else
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

   end;

  BT_NMSTRT ,BT_HEX :
   begin
    tok:=XML_TOK_NAME;

    inc(ptrcomp(ptr ) ,MINBPC(enc ) );

   end;

  BT_DIGIT ,BT_NAME ,BT_MINUS {$IFDEF XML_NS } ,BT_COLON : {$ELSE }: {$ENDIF }
   begin
    tok:=XML_TOK_NMTOKEN;

    inc(ptrcomp(ptr ) ,MINBPC(enc ) );

   end;

  BT_NONASCII :
   if IS_NMSTRT_CHAR_MINBPC(enc ,ptr ) <> 0 then
    begin
     inc(ptrcomp(ptr ) ,MINBPC(enc ) );

     tok:=XML_TOK_NAME;

    end
   else
    if IS_NAME_CHAR_MINBPC(enc ,ptr ) <> 0 then
     begin
      inc(ptrcomp(ptr ) ,MINBPC(enc ) );

      tok:=XML_TOK_NMTOKEN;

     end
    else
    { fall through }
     goto _else2;

  else
   begin
   _else2:
    nextTokPtr^:=ptr;

    result:=XML_TOK_INVALID;

    exit;

   end;

 end;

 while ptr <> end_ do
  case BYTE_TYPE(enc ,ptr ) of
  {#define CHECK_NAME_CASES}
   BT_NONASCII :
    if IS_NAME_CHAR_MINBPC(enc ,ptr ) = 0 then
     begin
      nextTokPtr^:=ptr;

      result:=XML_TOK_INVALID;

      exit;

     end
    else
     goto _bt0;

   BT_NMSTRT ,BT_HEX ,BT_DIGIT ,BT_NAME ,BT_MINUS :
   _bt0:
    inc(ptrcomp(ptr ) ,MINBPC(enc ) );

   BT_LEAD2 :
    begin
     if ptrcomp(end_ ) - ptrcomp(ptr ) < 2 then
      begin
       result:=XML_TOK_PARTIAL_CHAR;

       exit;

      end;

     if IS_NAME_CHAR(enc ,ptr ,2 ) = 0 then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

     inc(ptrcomp(ptr ) ,2 );

    end;

   BT_LEAD3 :
    begin
     if ptrcomp(end_ ) - ptrcomp(ptr ) < 3 then
      begin
       result:=XML_TOK_PARTIAL_CHAR;

       exit;

      end;

     if IS_NAME_CHAR(enc ,ptr ,3 ) = 0 then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

     inc(ptrcomp(ptr ) ,3 );

    end;

   BT_LEAD4 :
    begin
     if ptrcomp(end_ ) - ptrcomp(ptr ) < 4 then
      begin
       result:=XML_TOK_PARTIAL_CHAR;

       exit;

      end;

     if IS_NAME_CHAR(enc ,ptr ,4 ) = 0 then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

     inc(ptrcomp(ptr ) ,4 );

    end;

  {CHECK_NAME_CASES #define}

   BT_GT ,BT_RPAR ,BT_COMMA ,BT_VERBAR ,BT_LSQB ,BT_PERCNT ,
   BT_S ,BT_CR ,BT_LF :
    begin
     nextTokPtr^:=ptr;

     result:=tok;

     exit;

    end;

  {$IFDEF XML_NS }
   BT_COLON :
    begin
     inc(ptrcomp(ptr ) ,MINBPC(enc ) );

     case tok of
      XML_TOK_NAME :
       begin
        if ptr = end_ then
         begin
          result:=XML_TOK_PARTIAL;

          exit;

         end;

        tok:=XML_TOK_PREFIXED_NAME;

        case BYTE_TYPE(enc ,ptr ) of
        {#define CHECK_NAME_CASES}
         BT_NONASCII :
          if IS_NAME_CHAR_MINBPC(enc ,ptr ) = 0 then
           begin
            nextTokPtr^:=ptr;

            result:=XML_TOK_INVALID;

            exit;

           end
          else
           goto _bt1;

         BT_NMSTRT ,BT_HEX ,BT_DIGIT ,BT_NAME ,BT_MINUS :
         _bt1:
          inc(ptrcomp(ptr ) ,MINBPC(enc ) );

         BT_LEAD2 :
          begin
           if ptrcomp(end_ ) - ptrcomp(ptr ) < 2 then
            begin
             result:=XML_TOK_PARTIAL_CHAR;

             exit;

            end;

           if IS_NAME_CHAR(enc ,ptr ,2 ) = 0 then
            begin
             nextTokPtr^:=ptr;

             result:=XML_TOK_INVALID;

             exit;

            end;

           inc(ptrcomp(ptr ) ,2 );

          end;

         BT_LEAD3 :
          begin
           if ptrcomp(end_ ) - ptrcomp(ptr ) < 3 then
            begin
             result:=XML_TOK_PARTIAL_CHAR;

             exit;

            end;

           if IS_NAME_CHAR(enc ,ptr ,3 ) = 0 then
            begin
             nextTokPtr^:=ptr;

             result:=XML_TOK_INVALID;

             exit;

            end;

           inc(ptrcomp(ptr ) ,3 );

          end;

         BT_LEAD4 :
          begin
           if ptrcomp(end_ ) - ptrcomp(ptr ) < 4 then
            begin
             result:=XML_TOK_PARTIAL_CHAR;

             exit;

            end;

           if IS_NAME_CHAR(enc ,ptr ,4 ) = 0 then
            begin
             nextTokPtr^:=ptr;

             result:=XML_TOK_INVALID;

             exit;

            end;

           inc(ptrcomp(ptr ) ,4 );

          end;

        {CHECK_NAME_CASES #define}

         else
          tok:=XML_TOK_NMTOKEN;

        end;

       end;

      XML_TOK_PREFIXED_NAME :
       tok:=XML_TOK_NMTOKEN;

     end;

    end;

  {$ENDIF }

   BT_PLUS :
    begin
     if tok = XML_TOK_NMTOKEN then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

     nextTokPtr^:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

     result:=XML_TOK_NAME_PLUS;

     exit;

    end;

   BT_AST :
    begin
     if tok = XML_TOK_NMTOKEN then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

     nextTokPtr^:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

     result:=XML_TOK_NAME_ASTERISK;

     exit;

    end;

   BT_QUEST :
    begin
     if tok = XML_TOK_NMTOKEN then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_INVALID;

       exit;

      end;

     nextTokPtr^:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

     result:=XML_TOK_NAME_QUESTION;

     exit;

    end;

   else
    begin
     nextTokPtr^:=ptr;

     result:=XML_TOK_INVALID;

     exit;

    end;

  end;

 result:=-tok;

end;

{ normal_contentTok }
function normal_contentTok(enc : ENCODING_ptr; ptr ,end_ : char_ptr; nextTokPtr : char_ptr_ptr ) : int;
var
 n : size_t;

label
 _break ,_go0 ,_break2 ; 

begin
 if ptr = end_ then
  begin
   result:=XML_TOK_NONE;

   exit;

  end;

 if MINBPC(enc ) > 1 then
  begin
   n:=ptrcomp(end_ ) - ptrcomp(ptr );

   if n and (MINBPC(enc ) - 1 ) <> 0 then
    begin
     n:=n and not(MINBPC(enc ) - 1 );

     if n = 0 then
      begin
       result:=XML_TOK_PARTIAL;

       exit;

      end;

     end_:=char_ptr(ptrcomp(ptr ) + n );

    end;

  end;

 case BYTE_TYPE(enc ,ptr ) of
  BT_LT :
   begin
    result:=normal_scanLt(enc ,char_ptr(ptrcomp(ptr ) + MINBPC(enc ) ) ,end_ ,nextTokPtr );

    exit;

   end;

  BT_AMP :
   begin
    result:=normal_scanRef(enc ,char_ptr(ptrcomp(ptr ) + MINBPC(enc ) ) ,end_ ,nextTokPtr );

    exit;

   end;

  BT_CR :
   begin
    inc(ptrcomp(ptr ) ,MINBPC(enc ) );

    if ptr = end_ then
     begin
      result:=XML_TOK_TRAILING_CR;

      exit;

     end;

    if BYTE_TYPE(enc ,ptr ) = BT_LF then
     inc(ptrcomp(ptr ) ,MINBPC(enc ) );

    nextTokPtr^:=ptr;

    result:=XML_TOK_DATA_NEWLINE;

    exit;

   end;

  BT_LF :
   begin
    nextTokPtr^:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

    result:=XML_TOK_DATA_NEWLINE;

    exit;

   end;

  BT_RSQB :
   begin
    inc(ptrcomp(ptr ) ,MINBPC(enc ) );

    if ptr = end_ then
     begin
      result:=XML_TOK_TRAILING_RSQB;

      exit;

     end;

    if CHAR_MATCHES(enc ,ptr ,int(ASCII_RSQB ) ) = 0 then
     goto _break;

    inc(ptrcomp(ptr ) ,MINBPC(enc ) );

    if ptr = end_ then
     begin
      result:=XML_TOK_TRAILING_RSQB;

      exit;

     end;

    if CHAR_MATCHES(enc ,ptr ,int(ASCII_GT ) ) = 0 then
     begin
      dec(ptrcomp(ptr ) ,MINBPC(enc ) );

      goto _break;

     end;

    nextTokPtr^:=ptr;

    result:=XML_TOK_INVALID;

    exit;

   end;

 {#define INVALID_CASES}
  BT_LEAD2 :
   begin
    if ptrcomp(end_ ) - ptrcomp(ptr ) < 2 then
     begin
      result:=XML_TOK_PARTIAL_CHAR;

      exit;

     end;

    if IS_INVALID_CHAR(enc ,ptr ,2 ) <> 0 then
     begin
      nextTokPtr^:=ptr;

      result:=XML_TOK_INVALID;

      exit;

     end;

    inc(ptrcomp(ptr ) ,2 );

   end;

  BT_LEAD3 :
   begin
    if ptrcomp(end_ ) - ptrcomp(ptr ) < 3 then
     begin
      result:=XML_TOK_PARTIAL_CHAR;

      exit;

     end;

    if IS_INVALID_CHAR(enc ,ptr ,3 ) <> 0 then
     begin
      nextTokPtr^:=ptr;

      result:=XML_TOK_INVALID;

      exit;

     end;

    inc(ptrcomp(ptr ) ,3 );

   end;

  BT_LEAD4 :
   begin
    if ptrcomp(end_ ) - ptrcomp(ptr ) < 4 then
     begin
      result:=XML_TOK_PARTIAL_CHAR;

      exit;

     end;

    if IS_INVALID_CHAR(enc ,ptr ,4 ) <> 0 then
     begin
      nextTokPtr^:=ptr;

      result:=XML_TOK_INVALID;

      exit;

     end;

    inc(ptrcomp(ptr ) ,4 );

   end;

  BT_NONXML ,BT_MALFORM ,BT_TRAIL :
   begin
    nextTokPtr^:=ptr;

    result:=XML_TOK_INVALID;

    exit;

   end;

 {INVALID_CASES #define}

  else
   inc(ptrcomp(ptr ) ,MINBPC(enc ) );

 end;

_break:
 while ptr <> end_ do
  case BYTE_TYPE(enc ,ptr ) of
   BT_LEAD2 :
    begin
     if (ptrcomp(end_ ) - ptrcomp(ptr ) < 2  ) or
        (IS_INVALID_CHAR(enc ,ptr ,2 ) <> 0 ) then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_DATA_CHARS;

       exit;

      end;

     inc(ptrcomp(ptr ) ,2 );

    end;

   BT_LEAD3 :
    begin
     if (ptrcomp(end_ ) - ptrcomp(ptr ) < 3  ) or
        (IS_INVALID_CHAR(enc ,ptr ,3 ) <> 0 ) then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_DATA_CHARS;

       exit;

      end;

     inc(ptrcomp(ptr ) ,3 );

    end;

   BT_LEAD4 :
    begin
     if (ptrcomp(end_ ) - ptrcomp(ptr ) < 4  ) or
        (IS_INVALID_CHAR(enc ,ptr ,4 ) <> 0 ) then
      begin
       nextTokPtr^:=ptr;

       result:=XML_TOK_DATA_CHARS;

       exit;

      end;

     inc(ptrcomp(ptr ) ,4 );

    end;

   BT_RSQB :
    begin
     if ptrcomp(ptr ) + MINBPC(enc ) <> ptrcomp(end_ ) then
      begin
       if CHAR_MATCHES(enc ,char_ptr(ptrcomp(ptr ) + MINBPC(enc ) ) ,int(ASCII_RSQB ) ) = 0 then
        begin
         inc(ptrcomp(ptr ) ,MINBPC(enc ) );

         goto _break2;

        end;

       if ptrcomp(ptr ) + 2 * MINBPC(enc ) <> ptrcomp(end_ ) then
        begin
         if CHAR_MATCHES(enc ,char_ptr(ptrcomp(ptr ) + 2 * MINBPC(enc ) ) ,int(ASCII_GT ) ) = 0 then
          begin
           inc(ptrcomp(ptr ) ,MINBPC(enc ) );

           goto _break2;

          end;

         nextTokPtr^:=char_ptr(ptrcomp(ptr ) + 2 * MINBPC(enc ) );

         result:=XML_TOK_INVALID;

         exit;

        end;

      end;

    { fall through }
     goto _go0;

    end;

   BT_AMP ,BT_LT ,BT_NONXML ,BT_MALFORM ,BT_TRAIL ,BT_CR ,BT_LF :
   _go0:
    begin
     nextTokPtr^:=ptr;

     result:=XML_TOK_DATA_CHARS;

     exit;

    end;

   else
    inc(ptrcomp(ptr ) ,MINBPC(enc ) );

  end;

_break2:
 nextTokPtr^:=ptr;

 result:=XML_TOK_DATA_CHARS;

end;

{ normal_cdataSectionTok {..}
function normal_cdataSectionTok(enc : ENCODING_ptr; ptr ,end_ : char_ptr; nextTokPtr : char_ptr_ptr ) : int;
begin
end;

{ normal_ignoreSectionTok {..}
function normal_ignoreSectionTok(enc : ENCODING_ptr; ptr ,end_ : char_ptr; nextTokPtr : char_ptr_ptr ) : int;
begin
end;

{ normal_attributeValueTok }
function normal_attributeValueTok(enc : ENCODING_ptr; ptr ,end_ : char_ptr; nextTokPtr : char_ptr_ptr ) : int;
var
 start : char_ptr;

begin
 if ptr = end_ then
  begin
   result:=XML_TOK_NONE;

   exit;

  end;

 start:=ptr;

 while ptr <> end_ do
  case BYTE_TYPE(enc ,ptr ) of
   BT_LEAD2 :
    inc(ptrcomp(ptr ) ,2 );

   BT_LEAD3 :
    inc(ptrcomp(ptr ) ,3 );

   BT_LEAD4 :
    inc(ptrcomp(ptr ) ,4 );

   BT_AMP :
    begin
     if ptr = start then
      begin
       result:=normal_scanRef(enc ,char_ptr(ptrcomp(ptr ) + MINBPC(enc ) ) ,end_ ,nextTokPtr );

       exit;

      end;

     nextTokPtr^:=ptr;

     result:=XML_TOK_DATA_CHARS;

     exit;

    end;

   BT_LT :
    begin
    { this is for inside entity references }
     nextTokPtr^:=ptr;

     result:=XML_TOK_INVALID;

     exit;

    end;

   BT_LF :
    begin
     if ptr = start then
      begin
       nextTokPtr^:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

       result:=XML_TOK_DATA_NEWLINE;

       exit;

      end;

     nextTokPtr^:=ptr;

     result:=XML_TOK_DATA_CHARS;

     exit;

    end;

   BT_CR :
    begin
     if ptr = start then
      begin
       inc(ptrcomp(ptr ) ,MINBPC(enc ) );

       if ptr = end_ then
        begin
         result:=XML_TOK_TRAILING_CR;

         exit;

        end;

       if BYTE_TYPE(enc ,ptr ) = BT_LF then
        inc(ptrcomp(ptr ) ,MINBPC(enc ) );

       nextTokPtr^:=ptr;

       result:=XML_TOK_DATA_NEWLINE;

       exit;

      end;

     nextTokPtr^:=ptr;

     result:=XML_TOK_DATA_CHARS;

     exit;

    end;

   BT_S :
    begin
     if ptr = start then
      begin
       nextTokPtr^:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

       result:=XML_TOK_ATTRIBUTE_VALUE_S;

       exit;

      end;

     nextTokPtr^:=ptr;

     result:=XML_TOK_DATA_CHARS;

     exit;

    end;

   else
    inc(ptrcomp(ptr ) ,MINBPC(enc ) );

  end;

 nextTokPtr^:=ptr;

 result:=XML_TOK_DATA_CHARS;

end;

{ normal_entityValueTok {..}
function normal_entityValueTok(enc : ENCODING_ptr; ptr ,end_ : char_ptr; nextTokPtr : char_ptr_ptr ) : int;
begin
end;

{ normal_sameName {..}
function normal_sameName(enc : ENCODING_ptr; ptr1 ,ptr2 : char_ptr ) : int;
begin
end;

{ normal_nameMatchesAscii }
function normal_nameMatchesAscii(enc : ENCODING_ptr; ptr1 ,end1 ,ptr2 : char_ptr ) : int;
begin
 while ptr2^ <> #0 do
  begin
   if ptr1 = end1 then
    begin
     result:=0;

     exit;

    end;

   if CHAR_MATCHES(enc ,ptr1 ,int(ptr2^ ) ) = 0 then
    begin
     result:=0;

     exit;

    end;

   inc(ptrcomp(ptr1 ) ,MINBPC(enc ) );
   inc(ptrcomp(ptr2 ) );

  end;

 result:=int(ptr1 = end1 );

end;

{ normal_nameLength }
function normal_nameLength(enc : ENCODING_ptr; ptr : char_ptr ) : int;
var
 start : char_ptr;

begin
 start:=ptr;

 repeat
  case BYTE_TYPE(enc ,ptr ) of
   BT_LEAD2 :
    inc(ptrcomp(ptr ) ,2 );

   BT_LEAD3 :
    inc(ptrcomp(ptr ) ,3 );

   BT_LEAD4 :
    inc(ptrcomp(ptr ) ,4 );

   BT_NONASCII ,BT_NMSTRT ,{$IFDEF XML_NS }BT_COLON ,{$ENDIF }
   BT_HEX ,BT_DIGIT ,BT_NAME ,BT_MINUS :
    inc(ptrcomp(ptr ) ,MINBPC(enc ) );

   else
    begin
     result:=ptrcomp(ptr ) - ptrcomp(start );

     exit;

    end;

  end;

 until false;

end;

{ normal_skipS {..}
function normal_skipS(enc : ENCODING_ptr; ptr : char_ptr ) : char_ptr;
begin
end;

{ normal_getAtts }
{ This must only be called for a well-formed start-tag or empty
  element tag.  Returns the number of attributes.  Pointers to the
  first attsMax attributes are stored in atts. }
function normal_getAtts(enc : ENCODING_ptr; ptr : char_ptr; attsMax : int; atts : ATTRIBUTE_ptr ) : int;
type
 state_enum = (other ,inName ,inValue );

var
 state : state_enum;

 nAtts ,open : int;

begin
 state:=inName;
 nAtts:=0;
 open :=0; { defined when state = inValue;
             initialization just to shut up compilers }

 inc(ptrcomp(ptr ) ,MINBPC(enc ) );

 repeat
  case BYTE_TYPE(enc ,ptr ) of
   BT_LEAD2 :
    begin
     if state = other then
      begin
       if nAtts < attsMax then
        begin
         ATTRIBUTE_ptr(ptrcomp(atts ) + nAtts * sizeof(ATTRIBUTE ) )^.name      :=ptr;
         ATTRIBUTE_ptr(ptrcomp(atts ) + nAtts * sizeof(ATTRIBUTE ) )^.normalized:=#1;

        end;

       state:=inName;

      end;

     inc(ptrcomp(ptr ) ,2 - MINBPC(enc ) );

    end;

   BT_LEAD3 :
    begin
     if state = other then
      begin
       if nAtts < attsMax then
        begin
         ATTRIBUTE_ptr(ptrcomp(atts ) + nAtts * sizeof(ATTRIBUTE ) )^.name      :=ptr;
         ATTRIBUTE_ptr(ptrcomp(atts ) + nAtts * sizeof(ATTRIBUTE ) )^.normalized:=#1;

        end;

       state:=inName;

      end;

     inc(ptrcomp(ptr ) ,3 - MINBPC(enc ) );

    end;

   BT_LEAD4 :
    begin
     if state = other then
      begin
       if nAtts < attsMax then
        begin
         ATTRIBUTE_ptr(ptrcomp(atts ) + nAtts * sizeof(ATTRIBUTE ) )^.name      :=ptr;
         ATTRIBUTE_ptr(ptrcomp(atts ) + nAtts * sizeof(ATTRIBUTE ) )^.normalized:=#1;

        end;

       state:=inName;

      end;

     inc(ptrcomp(ptr ) ,4 - MINBPC(enc ) );

    end;

   BT_NONASCII ,BT_NMSTRT ,BT_HEX :
    if state = other then
     begin
      if nAtts < attsMax then
       begin
        ATTRIBUTE_ptr(ptrcomp(atts ) + nAtts * sizeof(ATTRIBUTE ) )^.name      :=ptr;
        ATTRIBUTE_ptr(ptrcomp(atts ) + nAtts * sizeof(ATTRIBUTE ) )^.normalized:=#1;

       end;

      state:=inName;

     end;

   BT_QUOT :
    if state <> inValue then
     begin
      if nAtts < attsMax then
       ATTRIBUTE_ptr(ptrcomp(atts ) + nAtts * sizeof(ATTRIBUTE ) )^.valuePtr:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

      state:=inValue;
      open :=BT_QUOT;

     end
    else
     if open = BT_QUOT then
      begin
       state:=other;

       if nAtts < attsMax then
        ATTRIBUTE_ptr(ptrcomp(atts ) + nAtts * sizeof(ATTRIBUTE ) )^.valueEnd:=ptr;

       inc(nAtts );

      end;

   BT_APOS :
    if state <> inValue then
     begin
      if nAtts < attsMax then
       ATTRIBUTE_ptr(ptrcomp(atts ) + nAtts * sizeof(ATTRIBUTE ) )^.valuePtr:=char_ptr(ptrcomp(ptr ) + MINBPC(enc ) );

      state:=inValue;
      open :=BT_APOS;

     end
    else
     if open = BT_APOS then
      begin
       state:=other;

       if nAtts < attsMax then                                       
        ATTRIBUTE_ptr(ptrcomp(atts ) + nAtts * sizeof(ATTRIBUTE ) )^.valueEnd:=ptr;

       inc(nAtts );

      end;

   BT_AMP :
    if nAtts < attsMax then
     ATTRIBUTE_ptr(ptrcomp(atts ) + nAtts * sizeof(ATTRIBUTE ) )^.normalized:=#0;

   BT_S :
    if state = inName then
     state:=other
    else
     if (state = inValue ) and
        (nAtts < attsMax ) and
        (ATTRIBUTE_ptr(ptrcomp(atts ) + nAtts * sizeof(ATTRIBUTE ) )^.normalized <> #0 ) and
        ((ptr = ATTRIBUTE_ptr(ptrcomp(atts ) + nAtts * sizeof(ATTRIBUTE ) )^.valuePtr ) or
         (BYTE_TO_ASCII(enc ,ptr ) <> int(ASCII_SPACE ) ) or
         (BYTE_TO_ASCII(enc ,char_ptr(ptrcomp(ptr ) + MINBPC(enc ) ) ) = int(ASCII_SPACE ) ) or
         (BYTE_TYPE(enc ,char_ptr(ptrcomp(ptr ) + MINBPC(enc ) ) ) = open ) ) then
      ATTRIBUTE_ptr(ptrcomp(atts ) + nAtts * sizeof(ATTRIBUTE ) )^.normalized:=#0;

   BT_CR ,BT_LF :
   { This case ensures that the first attribute name is counted
     Apart from that we could just change state on the quote. }
    if state = inName then
     state:=other
    else
     if (state = inValue ) and
        (nAtts < attsMax ) then
      ATTRIBUTE_ptr(ptrcomp(atts ) + nAtts * sizeof(ATTRIBUTE ) )^.normalized:=#0;

   BT_GT ,BT_SOL :
    if state <> inValue then
     begin
      result:=nAtts;

      exit;

     end;

  end;

  inc(ptrcomp(ptr ) ,MINBPC(enc ) );

 until false;

{ not reached }

end;

{ normal_charRefNumber {..}
function normal_charRefNumber(enc : ENCODING_ptr; ptr : char_ptr ) : int;
begin
end;

{ normal_predefinedEntityName {..}
function normal_predefinedEntityName(enc : ENCODING_ptr; ptr ,end_ : char_ptr ) : int;
begin
end;

{ normal_updatePosition }
procedure normal_updatePosition(enc : ENCODING_ptr; ptr ,end_ : char_ptr; pos : POSITION_ptr );
begin
 while ptr <> end_ do
  begin
   case BYTE_TYPE(enc ,ptr ) of
    BT_LEAD2 :
     inc(ptrcomp(ptr ) ,2 );

    BT_LEAD3 :
     inc(ptrcomp(ptr ) ,3 );

    BT_LEAD4 :
     inc(ptrcomp(ptr ) ,4 );

    BT_LF :
     begin
      pos.columnNumber:=XML_Size(-1 );

      inc(pos.lineNumber );
      inc(ptrcomp(ptr ) ,MINBPC(enc ) );

     end;

    BT_CR :
     begin
      inc(pos.lineNumber );
      inc(ptrcomp(ptr ) ,MINBPC(enc ) );

      if (ptr <> end_ ) and
         (BYTE_TYPE(enc ,ptr ) = BT_LF ) then
       inc(ptrcomp(ptr ) ,MINBPC(enc ) );

      pos.columnNumber:=XML_Size(-1 );

     end;

    else
     inc(ptrcomp(ptr ) ,MINBPC(enc ) );

   end;

   inc(pos.columnNumber );

  end;

end;

{ normal_isPublicId }
function normal_isPublicId(enc : ENCODING_ptr; ptr ,end_ : char_ptr; badPtr : char_ptr_ptr ) : int;
label
 _else ;

begin
 inc(ptrcomp(ptr ) ,MINBPC(enc ) );
 dec(ptrcomp(end_ ) ,MINBPC(enc ) );

 while ptr <> end_ do
  begin
   case BYTE_TYPE(enc ,ptr ) of
    BT_S :
     if CHAR_MATCHES(enc ,ptr ,int(ASCII_TAB ) ) <> 0 then
      begin
       badPtr^:=ptr;
       result :=0;

       exit;

      end;

    BT_NAME ,BT_NMSTRT :
     if BYTE_TO_ASCII(enc ,ptr ) and not $7f = 0 then
     else
      goto _else;

    BT_DIGIT ,BT_HEX ,BT_MINUS ,BT_APOS ,BT_LPAR ,BT_RPAR ,BT_PLUS ,BT_COMMA ,
    BT_SOL ,BT_EQUALS ,BT_QUEST ,BT_CR ,BT_LF ,BT_SEMI ,BT_EXCL ,BT_AST ,BT_PERCNT ,
    BT_NUM {$IFDEF XML_NS } ,BT_COLON : {$ELSE } : {$ENDIF }
    else
    _else:
     case BYTE_TO_ASCII(enc ,ptr ) of
      $24 , { $ }
      $40 : { @ }
      else
       begin
        badPtr^:=ptr;
        result :=0;

        exit;

       end;

     end;

   end;

   inc(ptrcomp(ptr ) ,MINBPC(enc ) );

  end;

 result:=1;

end;

