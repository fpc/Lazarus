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
// 03.05.2006-Milano: Unit port establishment
// 02.06.2006-Milano: porting
// 06.06.2006-Milano: -"-
// 07.06.2006-Milano: doProlog
// 08.06.2006-Milano: doProlog finished, porting
// 09.06.2006-Milano: porting
// 12.06.2006-Milano: -"-
// 14.06.2006-Milano: -"-
// 15.06.2006-Milano: doContent
// 16.06.2006-Milano: porting, storeAtts
// 17.06.2006-Milano: -"-
// 20.06.2006-Milano: epilogProcessor, porting
// 22.06.2006-Milano: appendAttributeValue
//
{ xmlparse.inc }
{$Q- }
{$R- }
function  poolStoreString(pool : STRING_POOL_ptr; enc : ENCODING_ptr; ptr ,end_ : char_ptr ) : XML_Char_ptr; forward;
procedure poolFinish     (pool : STRING_POOL_ptr ); forward;
procedure poolClear      (pool : STRING_POOL_ptr ); forward;
procedure poolDestroy    (pool : STRING_POOL_ptr ); forward;
function  poolAppendChar (pool : STRING_POOL_ptr; c : char ) : int; forward;

function  reportProcessingInstruction(parser : XML_Parser; enc : ENCODING_ptr; start ,end_ : char_ptr ) : int; forward;
function  reportComment              (parser : XML_Parser; enc : ENCODING_ptr; start ,end_ : char_ptr ) : int; forward;

function  getAttributeId(parser : XML_Parser; enc : ENCODING_ptr; start ,end_ : char_ptr ) : ATTRIBUTE_ID_ptr; forward;

function  storeAttributeValue(
           parser : XML_Parser; enc : ENCODING_ptr; isCdata : XML_Bool;
           ptr ,end_ : char_ptr; pool : STRING_POOL_ptr ) : XML_Error; forward;

const
 implicitContext : array[0..40 ] of XML_Char = (
  'x' ,'m' ,'l' ,'=' ,'h' ,'t' ,'t' ,'p' ,':' ,'/' ,'/' ,
  'w' ,'w' ,'w' ,'.' ,'w' ,'3' ,'.' ,'o' ,'r' ,'g' ,'/' ,
  'X' ,'M' ,'L' ,'/' ,'1' ,'9' ,'9' ,'8' ,'/' ,
  'n' ,'a' ,'m' ,'e' ,'s' ,'p' ,'a' ,'c' ,'e' ,#0 );

 INIT_TAG_BUF_SIZE  = 32; { must be a multiple of sizeof(XML_Char) }
 INIT_DATA_BUF_SIZE = 1024;
 INIT_ATTS_SIZE     = 16;
 INIT_ATTS_VERSION  = $FFFFFFFF;
 INIT_BLOCK_SIZE    = 1024;
 INIT_BUFFER_SIZE   = 1024;

 EXPAND_SPARE = 24;

 INIT_SCAFFOLD_ELEMENTS = 32;

 INIT_POWER = 6;

type
 ICHAR_ptr_ptr = ^ICHAR_ptr;
 ICHAR_ptr = ^ICHAR;

{$IFDEF XML_UNICODE }
 ICHAR = int16u;

{$ELSE }
 ICHAR = char;

{$ENDIF }

 HASH_TABLE_ITER_ptr = ^HASH_TABLE_ITER;
 HASH_TABLE_ITER = record
   p    ,
   end_ : NAMED_ptr_ptr;

  end;

const
{$IFDEF XML_UNICODE }
 XML_ENCODE_MAX = XML_UTF16_ENCODE_MAX;

{$ELSE }
 XML_ENCODE_MAX = XML_UTF8_ENCODE_MAX;

{$ENDIF }

{ memcmp }
function  memcmp(p1 ,p2 : int8u_ptr; l : int ) : int;
begin
 while l > 0 do
  begin
   if p1^ <> p2^ then
    begin
     result:=p1^ - p2^;

     exit;

    end;

   dec(l );
   inc(ptrcomp(p1 ) );
   inc(ptrcomp(p2 ) );

  end;

 result:=0;

end;

{ CHAR_HASH }
{ Basic character hash algorithm, taken from Python's string hash:
  h = h * 1000003 ^ character, the constant being a prime number. }
function CHAR_HASH(h : int32u; c : XML_Char ) : int32u;
begin
{$IFDEF XML_UNICODE }
 result:=(h * $F4243 ) xor int16u(c );

{$ELSE }
 result:=(h * $F4243 ) xor int8u(c );

{$ENDIF }

end;

{ MUST_CONVERT }
function MUST_CONVERT(enc : ENCODING_ptr; s : char_ptr ) : int;
begin
{$IFDEF XML_UNICODE }
 result:=
  int(
   not boolean(enc.isUtf16 ) or
   boolean(int32u(s ) and 1 ) );

{$ELSE }
 result:=int(not boolean(enc.isUtf8 ) );

{$ENDIF }

end;

{ For probing (after a collision) we need a step size relative prime
  to the hash table size, which is a power of 2. We use double-hashing,
  since we can calculate a second hash value cheaply by taking those bits
  of the first hash value that were discarded (masked out) when the table
  index was calculated: index:=hash and mask, where mask:=table.size - 1.
  We limit the maximum step size to table.size div 4 (mask shr 2 ) and make
  it odd, since odd numbers are always relative prime to a power of 2. }
{ SECOND_HASH }
function SECOND_HASH(hash ,mask : int32u; power : int8u ) : int8u;
begin
 result:=((hash and not mask ) shr (power - 1 ) ) and (mask shr 2 );

end;

{ PROBE_STEP }
function PROBE_STEP(hash ,mask : int32u; power : int8u ) : int8u;
begin
 result:=SECOND_HASH(hash, mask, power) or 1;

end;

{ XML_T }
function XML_T(x : char ) : XML_Char;
begin
 result:=x;

end;

{ XML_L }
function XML_L(x : char ) : XML_Char;
begin
 result:=x;

end;

{ ROUND_UP }
{ Round up n to be a multiple of sz, where sz is a power of 2. }
function ROUND_UP(n ,sz : int ) : int;
begin
 result:=(n + (sz - 1 ) ) and not(sz - 1 );

end;

{ XmlConvert }
procedure XmlConvert(enc : ENCODING_ptr; fromP ,fromLim ,toP ,toLim : pointer );
begin
{$IFDEF XML_UNICODE }
 XmlUtf16Convert(enc ,fromP ,fromLim ,toP ,toLim );

{$ELSE }
 XmlUtf8Convert(enc ,fromP ,fromLim ,toP ,toLim );

{$ENDIF }

end;

{ XmlEncode }
function XmlEncode(charNumber : int; buf : pointer ) : int;
begin
{$IFDEF XML_UNICODE }
 result:=XmlUtf16Encode(charNumber ,buf );

{$ELSE }
 result:=XmlUtf8Encode(charNumber ,buf );

{$ENDIF }

end;

{ poolInit }
procedure poolInit(pool : STRING_POOL_ptr; ms : XML_Memory_Handling_Suite_ptr );
begin
 pool.blocks    :=NIL;
 pool.freeBlocks:=NIL;
 pool.start     :=NIL;
 pool.ptr       :=NIL;
 pool.end_      :=NIL;
 pool.mem       :=ms;

end;

{ hashTableDestroy }
procedure hashTableDestroy(table : HASH_TABLE_ptr );
var
 i : size_t;

begin
 i:=0;

 while i < table.size do
  begin
   if NAMED_ptr_ptr(ptrcomp(table.v ) + i * sizeof(NAMED_ptr ) )^ <> NIL then
    table.mem.free_fcn(
     pointer(NAMED_ptr_ptr(ptrcomp(table.v ) + i * sizeof(NAMED_ptr ) )^ ) ,
     NAMED_ptr_ptr(ptrcomp(table.v ) + i * sizeof(NAMED_ptr ) )^^.alloc );

   inc(i );

  end;

 table.mem.free_fcn(pointer(table.v ) ,table.a );

end;

{ hashTableInit }
procedure hashTableInit(p : HASH_TABLE_ptr; ms : XML_Memory_Handling_Suite_ptr );
begin
 p.power:=0;
 p.size :=0;
 p.used :=0;
 p.v    :=NIL;
 p.mem  :=ms;

end;

{ hashTableIterInit }
procedure hashTableIterInit(iter : HASH_TABLE_ITER_ptr; table : HASH_TABLE_ptr );
begin
 iter.p   :=table.v;
 iter.end_:=NAMED_ptr_ptr(ptrcomp(iter.p ) + table.size * sizeof(NAMED_ptr ) );

end;

{ hashTableIterNext }
function hashTableIterNext(iter : HASH_TABLE_ITER_ptr ) : NAMED_ptr;
var
 tem : NAMED_ptr;

begin
 while iter.p <> iter.end_ do
  begin
   tem:=iter.p^;

   inc(ptrcomp(iter.p ) ,sizeof(NAMED_ptr ) );

   if tem <> NIL then
    begin
     result:=tem;

     exit;

    end;

  end;

 result:=NIL;

end;

{ dtdCreate }
function dtdCreate(ms : XML_Memory_Handling_Suite_ptr ) : DTD_ptr;
var
 p : DTD_ptr;

begin
 p:=nil;
 ms.malloc_fcn(pointer(p ) ,sizeof(DTD ) );

 if p = NIL then
  begin
   result:=p;

   exit;

  end;

 poolInit(@p.pool ,ms );
 poolInit(@p.entityValuePool ,ms );

 hashTableInit(@p.generalEntities ,ms );
 hashTableInit(@p.elementTypes ,ms );
 hashTableInit(@p.attributeIds ,ms );
 hashTableInit(@p.prefixes ,ms );

{$IFDEF XML_DTD }
 p.paramEntityRead:=XML_FALSE;

 hashTableInit(@p.paramEntities ,ms );

{$ENDIF }

 p.defaultPrefix.name   :=NIL;
 p.defaultPrefix.binding:=NIL;

 p.in_eldecl       :=XML_FALSE;
 p.scaffIndex      :=NIL;
 p.scaffAlloc      :=0;
 p.scaffold        :=NIL;
 p.scaffLevel      :=0;
 p.scaffSize       :=0;
 p.scaffCount      :=0;
 p.contentStringLen:=0;

 p.keepProcessing    :=XML_TRUE;
 p.hasParamEntityRefs:=XML_FALSE;
 p.standalone        :=XML_FALSE;

 result:=p;

end;

{ dtdDestroy }
procedure dtdDestroy(p : DTD_ptr; isDocEntity : XML_Bool; ms : XML_Memory_Handling_Suite_ptr );
var
 iter : HASH_TABLE_ITER;

 e : ELEMENT_TYPE_ptr;

begin
 hashTableIterInit(@iter ,@p.elementTypes );

 repeat
  e:=ELEMENT_TYPE_ptr(hashTableIterNext(@iter ) );

  if e = NIL then
   break;

  if e.allocDefaultAtts <> 0 then
   ms.free_fcn(pointer(e.defaultAtts ) ,e.defaultAttsAlloc );

 until false;

 hashTableDestroy(@p.generalEntities );

{$IFDEF XML_DTD }
 hashTableDestroy(@p.paramEntities );

{$ENDIF }

 hashTableDestroy(@p.elementTypes );
 hashTableDestroy(@p.attributeIds );
 hashTableDestroy(@p.prefixes );

 poolDestroy(@p.pool );
 poolDestroy(@p.entityValuePool );

 if isDocEntity <> 0 then
  begin
   ms.free_fcn(pointer(p.scaffIndex ) ,p.scaffAlloc );
   ms.free_fcn(pointer(p.scaffold ) ,sizeof(CONTENT_SCAFFOLD ) );

  end;

 ms.free_fcn(pointer(p ) ,sizeof(DTD ) );

end;

{ handleUnknownEncoding ..}
function handleUnknownEncoding(parser : XML_Parser; encodingName : XML_Char_ptr ) : XML_Error;
begin
 Result:=XML_ERROR_NONE;
end;

{ initializeEncoding }
function initializeEncoding(parser : XML_Parser ) : XML_Error;
var
 s  : char_ptr;
 ok : int;

begin
{$IFDEF XML_UNICODE ..}


{$ELSE }
 s:=pointer(parser.m_protocolEncodingName );

{$ENDIF }

 if parser.m_ns <> 0 then
  ok:=XmlInitEncodingNS(@parser.m_initEncoding ,@parser.m_encoding ,pointer(s ) )
 else
  ok:=XmlInitEncoding(@parser.m_initEncoding ,@parser.m_encoding ,pointer(s ) );

 if ok <> 0 then
  result:=XML_ERROR_NONE
 else
  result:=handleUnknownEncoding(parser ,parser.m_protocolEncodingName );

end;

{ reportDefault ..}
procedure reportDefault(parser : XML_Parser; enc : ENCODING_ptr; start ,end_ : char_ptr );
begin
end;

{ getContext ..}
function getContext(parser : XML_Parser ) : XML_Char_ptr;
begin
Result:=nil;
end;

{ processXmlDecl }
function processXmlDecl(parser : XML_Parser; isGeneralTextEntity : int; s ,next : char_ptr ) : XML_Error;
var
 encodingName ,version ,versionend : char_ptr;

 storedEncName ,storedversion : XML_Char_ptr;

 newEncoding : ENCODING_ptr;

 standalone ,ok : int;

 result_ : XML_Error;

begin
 encodingName :=NIL;
 storedEncName:=NIL;
 newEncoding  :=NIL;
 version      :=NIL;
 storedversion:=NIL;
 standalone   :=-1;

 if parser.m_ns <> 0 then
  ok:=
   XmlParseXmlDeclNS(
    isGeneralTextEntity ,parser.m_encoding ,pointer(s ) ,pointer(next ) ,
    @parser.m_eventPtr ,@version ,@versionend ,@encodingName ,
    @newEncoding ,@standalone )
 else
  ok:=
   XmlParseXmlDecl(
    isGeneralTextEntity ,parser.m_encoding ,pointer(s ) ,pointer(next ) ,
    @parser.m_eventPtr ,@version ,@versionend ,@encodingName ,
    @newEncoding ,@standalone );

 if ok = 0 then
  if isGeneralTextEntity <> 0 then
   begin
    result:=XML_ERROR_TEXT_DECL;

    exit;

   end
  else
   begin
    result:=XML_ERROR_XML_DECL;

    exit;

   end;

 if (isGeneralTextEntity = 0 ) and
    (standalone = 1 ) then
  begin
   parser.m_dtd.standalone:=XML_TRUE;

  {$IFDEF XML_DTD }
   if parser.m_paramEntityParsing = XML_PARAM_ENTITY_PARSING_UNLESS_STANDALONE then
    parser.m_paramEntityParsing:=XML_PARAM_ENTITY_PARSING_NEVER;

  {$ENDIF }

  end;

 if @parser.m_xmlDeclHandler <> NIL then
  begin
   if encodingName <> NIL then
    begin
     storedEncName:=
      poolStoreString(
       @parser.m_temp2Pool ,
       parser.m_encoding ,
       encodingName,
       char_ptr(ptrcomp(encodingName ) + XmlNameLength(parser.m_encoding ,pointer(encodingName ) ) ) );

     if storedEncName = NIL then
      begin
       result:=XML_ERROR_NO_MEMORY;

       exit;

      end;

     poolFinish(@parser.m_temp2Pool );

    end;

   if version <> NIL then
    begin
     storedversion:=
      poolStoreString(
       @parser.m_temp2Pool ,
       parser.m_encoding ,
       version ,
       char_ptr(ptrcomp(versionend ) - parser.m_encoding.minBytesPerChar ) );

     if storedversion = NIL then
      begin
       result:=XML_ERROR_NO_MEMORY;

       exit;

      end;

    end;

   parser.m_xmlDeclHandler(
    parser.m_handlerArg ,storedversion ,storedEncName ,standalone );

  end
 else
  if @parser.m_defaultHandler <> NIL then
   reportDefault(parser ,parser.m_encoding ,s ,next );

 if parser.m_protocolEncodingName = NIL then
  begin
   if newEncoding <> NIL then
    begin
     if newEncoding.minBytesPerChar <> parser.m_encoding.minBytesPerChar then
      begin
       parser.m_eventPtr:=encodingName;

       result:=XML_ERROR_INCORRECT_ENCODING;

       exit;

      end;

     parser.m_encoding:=newEncoding;

    end
   else
    if encodingName <> NIL then
     begin
      if storedEncName = NIL then
       begin
        storedEncName:=
         poolStoreString(
          @parser.m_temp2Pool ,parser.m_encoding ,encodingName ,
          char_ptr(ptrcomp(encodingName ) + XmlNameLength(parser.m_encoding ,pointer(encodingName ) ) ) );

        if storedEncName = NIL then
         begin
          result:=XML_ERROR_NO_MEMORY;

          exit;

         end;

       end;

      result_:=handleUnknownEncoding(parser ,storedEncName );

      poolClear(@parser.m_temp2Pool );

      if result_ = XML_ERROR_UNKNOWN_ENCODING then
       parser.m_eventPtr:=encodingName;

      result:=result_;

      exit;

     end;

  end;

 if (storedEncName <> NIL ) or
    (storedversion <> NIL ) then
   poolClear(@parser.m_temp2Pool );

 result:=XML_ERROR_NONE;

end;

{ poolClear }
procedure poolClear(pool : STRING_POOL_ptr );
var
 p ,tem : BLOCK_ptr;

begin
 if pool.freeBlocks = NIL then
  pool.freeBlocks:=pool.blocks

 else
  begin
   p:=pool.blocks;

   while p <> NIL do
    begin
     tem            :=p.next;
     p.next         :=pool.freeBlocks;
     pool.freeBlocks:=p;
     p              :=tem;

    end;

  end;

 pool.blocks:=NIL;
 pool.start :=NIL;
 pool.ptr   :=NIL;
 pool.end_  :=NIL;

end;

{ poolDestroy }
procedure poolDestroy(pool : STRING_POOL_ptr );
var
 p ,tem : BLOCK_ptr;

begin
 p:=pool.blocks;

 while p <> NIL do
  begin
   tem:=p.next;

   pool.mem.free_fcn(pointer(p ) ,p.alloc );

   p:=tem;

  end;

 p:=pool.freeBlocks;

 while p <> NIL do
  begin
   tem:=p.next;

   pool.mem.free_fcn(pointer(p ) ,p.alloc );

   p:=tem;

  end;

end;

{ poolGrow }
function poolGrow(pool : STRING_POOL_ptr ) : XML_Bool;
var
 tem : BLOCK_ptr;

 blockSize : int;

begin
 if pool.freeBlocks <> NIL then
  begin
   if pool.start = NIL then
    begin
     pool.blocks     :=pool.freeBlocks;
     pool.freeBlocks :=pool.freeBlocks.next;
     pool.blocks.next:=NIL;

     pool.start:=@pool.blocks.s;
     pool.end_ :=XML_Char_ptr(ptrcomp(pool.start ) + pool.blocks.size * sizeof(XML_Char ) );
     pool.ptr  :=pool.start;

     result:=XML_TRUE;

     exit;

    end;

   if ptrcomp(pool.end_ ) - ptrcomp(pool.start ) < pool.freeBlocks.size then
    begin
     tem:=pool.freeBlocks.next;

     pool.freeBlocks.next:=pool.blocks;
     pool.blocks         :=pool.freeBlocks;
     pool.freeBlocks     :=tem;

     move(
      pool.start^ ,
      pointer(@pool.blocks.s )^ ,
      ptrcomp(pool.end_ ) - ptrcomp(pool.start ) );

     pool.ptr  :=XML_Char_ptr(ptrcomp(@pool.blocks.s ) + ptrcomp(pool.ptr ) - ptrcomp(pool.start ) );
     pool.start:=@pool.blocks.s;
     pool.end_ :=XML_Char_ptr(ptrcomp(pool.start ) + pool.blocks.size * sizeof(XML_Char ) );

     result:=XML_TRUE;

     exit;

    end;

  end;

 if (pool.blocks <> NIL ) and
    (pool.start = @pool.blocks.s ) then
  begin
   blockSize:=(ptrcomp(pool.end_ ) - ptrcomp(pool.start ) ) * 2 div sizeof(XML_Char );

   pool.mem.realloc_fcn(
    pointer(pool.blocks ) ,
    pool.blocks.alloc ,
    (sizeof(BLOCK_ptr ) + sizeof(int ) * 2 ) + blockSize * sizeof(XML_Char ) );

   if pool.blocks = NIL then
    begin
     result:=XML_FALSE;

     exit;

    end
   else
    pool.blocks.alloc:=(sizeof(BLOCK_ptr ) + sizeof(int ) * 2 ) + blockSize * sizeof(XML_Char );

   pool.blocks.size:=blockSize;

   pool.ptr  :=XML_Char_ptr(ptrcomp(@pool.blocks.s ) + (ptrcomp(pool.ptr ) - ptrcomp(pool.start ) ) );
   pool.start:=@pool.blocks.s;
   pool.end_ :=XML_Char_ptr(ptrcomp(pool.start ) + blockSize * sizeof(XML_Char ) );

  end
 else
  begin
   blockSize:=(ptrcomp(pool.end_ ) - ptrcomp(pool.start ) ) div sizeof(XML_Char );

   if blockSize < INIT_BLOCK_SIZE then
    blockSize:=INIT_BLOCK_SIZE
   else
    blockSize:=blockSize * 2;

   pool.mem.malloc_fcn(
    pointer(tem ) ,
    (sizeof(BLOCK_ptr ) + sizeof(int ) * 2 ) + blockSize * sizeof(XML_Char ) );

   if tem = NIL then
    begin
     result:=XML_FALSE;

     exit;

    end;

   tem.size :=blockSize;
   tem.alloc:=(sizeof(BLOCK_ptr ) + sizeof(int ) * 2 ) + blockSize * sizeof(XML_Char );
   tem.next :=pool.blocks;

   pool.blocks:=tem;

   if pool.ptr <> pool.start then
    move(
     pool.start^ ,
     pointer(@tem.s )^ ,
     ptrcomp(pool.ptr ) - ptrcomp(pool.start ) );

   pool.ptr  :=XML_Char_ptr(ptrcomp(@tem.s ) + (ptrcomp(pool.ptr ) - ptrcomp(pool.start ) ) * sizeof(XML_Char ) );
   pool.start:=@tem.s;
   pool.end_ :=XML_Char_ptr(ptrcomp(@tem.s ) + blockSize * sizeof(XML_Char ) );

  end;

 result:=XML_TRUE;

end;

{ poolAppend }
function poolAppend(pool : STRING_POOL_ptr; enc : ENCODING_ptr; ptr ,end_ : char_ptr ) : XML_Char_ptr;
begin
 if (pool.ptr = NIL ) and
    (poolGrow(pool ) = 0 ) then
  begin
   result:=NIL;

   exit;

  end;

 repeat
  XmlConvert(
   enc ,@ptr ,end_ ,
   ICHAR_ptr_ptr(@pool.ptr ) ,
   ICHAR_ptr(pool.end_ ) );

  if ptr = end_ then
   break;

  if poolGrow(pool ) = 0 then
   result:=NIL;

 until false;

 result:=pool.start;

end;

{ poolStoreString }
function poolStoreString(pool : STRING_POOL_ptr; enc : ENCODING_ptr; ptr ,end_ : char_ptr ) : XML_Char_ptr;
begin
 if poolAppend(pool ,enc ,ptr ,end_ ) = NIL then
  begin
   result:=NIL;

   exit;

  end;

 if (pool.ptr = pool.end_ ) and
    (poolGrow(pool ) = 0 ) then
  begin
   result:=NIL;

   exit;

  end;

 pool.ptr^:=XML_Char(0 );

 inc(ptrcomp(pool.ptr ) );

 result:=pool.start;

end;

{ poolCopyString }
function poolCopyString(pool : STRING_POOL_ptr; s : XML_Char_ptr ) : XML_Char_ptr;
label
 _w0 ;

begin
 goto _w0;

 while s^ <> XML_Char(0 ) do
  begin
  _w0:
   if poolAppendChar(pool ,s^ ) = 0 then
    begin
     result:=NIL;

     exit;

    end;

   inc(ptrcomp(s ) ,sizeof(XML_Char ) );

  end;

 s:=pool.start;

 poolFinish(pool );

 result:=s;

end;

{ poolAppendString {..}
function poolAppendString(pool : STRING_POOL_ptr; s : XML_Char_ptr ) : XML_Char_ptr;
begin
 Result:=nil;
end;

{ poolStart }
function poolStart(pool : STRING_POOL_ptr ) : XML_Char_ptr;
begin
 result:=pool.start;

end;

{ poolLength }
function poolLength(pool : STRING_POOL_ptr ) : int;
begin
 result:=ptrcomp(pool.ptr ) - ptrcomp(pool.start );

end;

{ poolChop }
procedure poolChop(pool : STRING_POOL_ptr );
begin
 dec(ptrcomp(pool.ptr ) ,sizeof(XML_Char ) );

end;

{ poolLastChar }
function poolLastChar(pool : STRING_POOL_ptr ) : XML_Char;
begin
 result:=XML_Char_ptr(ptrcomp(pool.ptr ) - 1 * sizeof(XML_Char ) )^;

end;

{ poolDiscard }
procedure poolDiscard(pool : STRING_POOL_ptr );
begin
 pool.ptr:=pool.start;

end;

{ poolFinish }
procedure poolFinish(pool : STRING_POOL_ptr );
begin
 pool.start:=pool.ptr;

end;

{ poolAppendChar }
function poolAppendChar(pool : STRING_POOL_ptr; c : char ) : int;
begin
 if (pool.ptr = pool.end_ ) and
    (poolGrow(pool ) = 0 ) then
  result:=0
 else
  begin
   pool.ptr^:=c;

   inc(ptrcomp(pool.ptr ) );

   result:=1;

  end;

end;

{ keyeq }
function keyeq(s1 ,s2 : KEY ) : XML_Bool;
begin
 while s1^ = s2^ do
  begin
   if s1^ = #0 then
    begin
     result:=XML_TRUE;

     exit;

    end;

   inc(ptrcomp(s1 ) ,sizeof(XML_Char ) );
   inc(ptrcomp(s2 ) ,sizeof(XML_Char ) );

  end;

 result:=XML_FALSE;

end;

{ hash }
function hash(s : KEY ) : int32u;
var
 h : int32u;

begin
 h:=0;

 while s^ <> XML_Char(0 ) do
  begin
   h:=CHAR_HASH(h ,s^ );

   inc(ptrcomp(s ) ,sizeof(XML_Char ) );

  end;

 result:=h;

end;

{ lookup }
function lookup(table : HASH_TABLE_ptr; name : KEY; createSize : size_t ) : NAMED_ptr;
var
 i ,tsize ,newSize ,j : size_t;

 h ,mask ,newMask ,newHash : int32u;

 step ,newPower : int8u;

 newV : NAMED_ptr_ptr;

begin
 newV:=nil;
 if table.size = 0 then
  begin
   if createSize = 0 then
    begin
     result:=NIL;

     exit;

    end;

   table.power:=INIT_POWER;

  { table->size is a power of 2 }
   table.size:=size_t(1 shl INIT_POWER );

   tsize:=table.size * sizeof(NAMED_ptr );

   table.mem.malloc_fcn(pointer(table.v ) ,tsize );

   if table.v = NIL then
    begin
     table.size:=0;

     result:=NIL;

     exit;

    end
   else
    table.a:=tsize;

   fillchar(table.v^ ,tsize ,0 );

   i:=hash(name ) and (table.size - 1 );

  end
 else
  begin
   h   :=hash(name );
   mask:=table.size - 1;
   step:=0;
   i   :=h and mask;

   while NAMED_ptr_ptr(ptrcomp(table.v ) + i * sizeof(NAMED_ptr ) )^ <> NIL do
    begin
     if keyeq(
         name ,
         NAMED_ptr_ptr(ptrcomp(table.v ) + i * sizeof(NAMED_ptr ) )^^.name ) <> 0 then
      begin
       result:=NAMED_ptr_ptr(ptrcomp(table.v ) + i * sizeof(NAMED_ptr ) )^;

       exit;

      end;

     if step = 0 then
      step:=PROBE_STEP(h ,mask ,table.power );

     if i < step then
      inc(i ,table.size - step )
     else
      dec(i ,step );

    end;

   if createSize = 0 then
    begin
     result:=NIL;

     exit;

    end;

  { check for overflow (table is half full) }
   if table.used shr (table.power - 1 ) <> 0 then
    begin
     newPower:=table.power + 1;
     newSize :=size_t(1 shl newPower );
     newMask :=newSize - 1;
     tsize   :=newSize * sizeof(NAMED_ptr );

     table.mem.malloc_fcn(pointer(newV ) ,tsize );

     if newV = NIL then
      begin
       result:=NIL;

       exit;

      end;

     fillchar(newV^ ,tsize ,0 );

     i:=0;

     while i < table.size do
      begin
       if NAMED_ptr_ptr(ptrcomp(table.v ) + i * sizeof(NAMED_ptr ) )^ <> NIL then
        begin
         newHash:=hash(NAMED_ptr_ptr(ptrcomp(table.v ) + i * sizeof(NAMED_ptr ) )^^.name );
         j      :=newHash and newMask;
         step   :=0;

         while NAMED_ptr_ptr(ptrcomp(newV ) + j * sizeof(NAMED_ptr ) )^ <> NIL do
          begin
           if step = 0 then
            step:=PROBE_STEP(newHash ,newMask ,newPower );

           if j < step then
            inc(j ,newSize - step )
           else
            dec(j ,step );

          end;

         NAMED_ptr_ptr(ptrcomp(newV ) + j * sizeof(NAMED_ptr ) )^:=
          NAMED_ptr_ptr(ptrcomp(table.v ) + i * sizeof(NAMED_ptr ) )^;

        end;

       inc(i );

      end;

     table.mem.free_fcn(pointer(table.v ) ,table.a );

     table.v    :=newV;
     table.a    :=tsize;
     table.power:=newPower;
     table.size :=newSize;

     i   :=h and newMask;
     step:=0;

     while NAMED_ptr_ptr(ptrcomp(table.v ) + i * sizeof(NAMED_ptr ) )^ <> NIL do
      begin
       if step = 0 then
        step:=PROBE_STEP(h ,newMask ,newPower );

       if i < step then
        inc(i ,newSize - step )
       else
        dec(i ,step );

      end;

    end;

  end;

 table.mem.malloc_fcn(
  pointer(NAMED_ptr_ptr(ptrcomp(table.v ) + i * sizeof(NAMED_ptr ) )^ ) ,
  createSize );

 if NAMED_ptr_ptr(ptrcomp(table.v ) + i * sizeof(NAMED_ptr ) )^ = NIL then
  begin
   result:=NIL;

   exit;

  end;

 fillchar(NAMED_ptr_ptr(ptrcomp(table.v ) + i * sizeof(NAMED_ptr ) )^^ ,createSize ,0 );

 NAMED_ptr_ptr(ptrcomp(table.v ) + i * sizeof(NAMED_ptr ) )^^.name :=name;
 NAMED_ptr_ptr(ptrcomp(table.v ) + i * sizeof(NAMED_ptr ) )^^.alloc:=createSize;

 inc(table.used );

 result:=NAMED_ptr_ptr(ptrcomp(table.v ) + i * sizeof(NAMED_ptr ) )^;

end;

{ normalizePublicId }
procedure normalizePublicId(publicId : XML_Char_ptr );
var
 p ,s : XML_Char_ptr;

begin
 p:=publicId;
 s:=publicId;

 while s^ <> XML_Char(0 ) do
  begin
   case s^ of
    XML_Char($20 ) ,XML_Char($D ) ,XML_Char($A ) :
     if (p <> publicId ) and
        (XML_Char_ptr(ptrcomp(p ) -1 * sizeof(XML_Char ) )^ <> XML_Char($20 ) ) then
      begin
       p^:=XML_Char($20 );

       inc(ptrcomp(p ) ,sizeof(XML_Char ) );

      end;

    else
     begin
      p^:=s^;

      inc(ptrcomp(p ) ,sizeof(XML_Char ) );

     end;

   end;

   inc(ptrcomp(s ) ,sizeof(XML_Char ) );

  end;

 if (p <> publicId ) and
    (XML_Char_ptr(ptrcomp(p ) -1 * sizeof(XML_Char ) )^ = XML_Char($20 ) ) then
  dec(ptrcomp(p ) ,sizeof(XML_Char ) );

 p^:=XML_T(#0 );

end;

{ setElementTypePrefix {..}
function setElementTypePrefix(parser : XML_Parser; elementType : ELEMENT_TYPE_ptr ) : int;
begin
 Result:=0;
end;

{ addBinding {..}
{ addBinding overwrites the value of prefix.binding without checking.
  Therefore one must keep track of the old value outside of addBinding. }
function addBinding(
          parser : XML_Parser; prefix : PREFIX_ptr; attId : ATTRIBUTE_ID_ptr;
          uri : XML_Char_ptr; bindingsPtr : BINDING_ptr_ptr ) : XML_Error;
begin
 Result:=XML_ERROR_NONE;
end;

{ storeRawNames }
{ Initially tag.rawName always points into the parse buffer;
  for those TAG instances opened while the current parse buffer was
  processed, and not yet closed, we need to store tag.rawName in a more
  permanent location, since the parse buffer is about to be discarded. }
function storeRawNames(parser : XML_Parser ) : XML_Bool;
var
 tag : TAG_ptr;

 bufSize ,nameLen : int;

 rawNameBuf ,temp : char_ptr;

begin
 tag:=parser.m_tagStack;

 while tag <> NIL do
  begin
   nameLen   :=sizeof(XML_Char ) * (tag.name.strLen + 1 );
   rawNameBuf:=char_ptr(ptrcomp(tag.buf ) + nameLen );

  { Stop if already stored. Since tagStack is a stack, we can stop
    at the first entry that has already been copied; everything
    below it in the stack is already been accounted for in a
    previous call to this function. }
   if tag.rawName = rawNameBuf then
    break;

  { For re-use purposes we need to ensure that the
    size of tag.buf is a multiple of sizeof(XML_Char ). }
   bufSize:=nameLen + ROUND_UP(tag.rawNameLength ,sizeof(XML_Char ) );

   if bufSize > ptrcomp(tag.bufEnd ) - ptrcomp(tag.buf ) then
    begin
     if parser.m_mem.realloc_fcn(pointer(tag.buf ) ,tag.alloc ,bufSize ) then
      temp:=tag.buf
     else
      temp:=NIL;

     if temp = NIL then
      begin
       result:=XML_FALSE;

       exit;

      end;

     tag.alloc:=bufSize; 

    { if tag.name.str points to tag.buf (only when namespace
      processing is off) then we have to update it }
     if tag.name.str = XML_Char_ptr(tag.buf ) then
      tag.name.str:=XML_Char_ptr(temp );

    { if tag->name.localPart is set (when namespace processing is on)
      then update it as well, since it will always point into tag->buf }
     if tag.name.localPart <> NIL then
      tag.name.localPart:=
       XML_Char_ptr(
        ptrcomp(temp ) +
        (ptrcomp(tag.name.localPart ) - ptrcomp(tag.buf ) ) );

     tag.buf   :=temp;
     tag.bufEnd:=char_ptr(ptrcomp(temp ) + bufSize );
     rawNameBuf:=char_ptr(ptrcomp(temp ) + nameLen );

    end;

   move(tag.rawName^ ,rawNameBuf^ ,tag.rawNameLength );

   tag.rawName:=rawNameBuf;
   tag        :=tag.parent;

  end;

 result:=XML_TRUE;

end;

{ storeAtts }
{ Precondition: all arguments must be non-NULL;
  Purpose:
  - normalize attributes
  - check attributes for well-formedness
  - generate namespace aware attribute names (URI, prefix)
  - build list of attributes for startElementHandler
  - default attributes
  - process namespace declarations (check and report them)
  - generate namespace aware element name (URI, prefix) }
function storeAtts(
          parser : XML_Parser; enc : ENCODING_ptr;
          attStr : char_ptr; tagNamePtr : TAG_NAME_ptr;
          bindingsPtr : BINDING_ptr_ptr ) : XML_Error;
var
 dtd : DTD_ptr;

 elementType : ELEMENT_TYPE_ptr;

 nDefaultAtts ,attIndex ,prefixLen ,i ,n ,nPrefixes ,oldAttsSize ,j ,nsAttsSize : int;

 version ,uriHash ,mask : int32u;

 step : int8u;

 appAtts : XML_Char_ptr_ptr; { the attribute list for the application }

 uri ,localPart ,name ,s ,s1 ,s2 : XML_Char_ptr;

 c : XML_Char;

 binding ,b : BINDING_ptr;

 attId ,id : ATTRIBUTE_ID_ptr;

 result_ : XML_Error;

 isCdata : XML_Bool;

 da : DEFAULT_ATTRIBUTE_ptr;

 p : TAG_ptr;

label
 _w0 ,_w1 ;

begin
 uri:=nil;
 dtd:=parser.m_dtd; { save one level of indirection }

 attIndex :=0;
 nPrefixes:=0;

{ lookup the element type name }
 elementType:=
  ELEMENT_TYPE_ptr(lookup(
   @dtd.elementTypes ,tagNamePtr.str ,0 ) );

 if elementType = NIL then
  begin
   name:=poolCopyString(@dtd.pool ,tagNamePtr.str );

   if name = NIL then
    begin
     result:=XML_ERROR_NO_MEMORY;

     exit;

    end;

   elementType:=
    ELEMENT_TYPE_ptr(lookup(
     @dtd.elementTypes ,name ,sizeof(ELEMENT_TYPE ) ) );

   if elementType = NIL then
    begin
     result:=XML_ERROR_NO_MEMORY;

     exit;

    end;

   if (parser.m_ns <> 0 ) and
      (setElementTypePrefix(parser ,elementType ) = 0 ) then
    begin
     result:=XML_ERROR_NO_MEMORY;

     exit;

    end;

  end;

 nDefaultAtts:=elementType.nDefaultAtts;

{ get the attributes from the tokenizer }
 n:=XmlGetAttributes(enc ,pointer(attStr ) ,parser.m_attsSize ,parser.m_atts );

 if n + nDefaultAtts > parser.m_attsSize then
  begin
   oldAttsSize      :=parser.m_attsSize;
   parser.m_attsSize:=n + nDefaultAtts + INIT_ATTS_SIZE;

   if not parser.m_mem.realloc_fcn(
           pointer(parser.m_atts ) ,
           parser.m_attsAlloc ,
           parser.m_attsSize * sizeof(ATTRIBUTE ) ) then
    begin
     result:=XML_ERROR_NO_MEMORY;

     exit;

    end
   else
    parser.m_attsAlloc:=parser.m_attsSize * sizeof(ATTRIBUTE );

   if n > oldAttsSize then
    XmlGetAttributes(enc ,pointer(attStr ) ,n ,parser.m_atts );

  end;

 appAtts:=XML_Char_ptr_ptr(parser.m_atts );

 i:=0;

 while i < n do
  begin
  { add the name and value to the attribute list }
   attId:=
    getAttributeId(
     parser ,enc ,
     pointer(ATTRIBUTE_ptr(ptrcomp(parser.m_atts ) + i * sizeof(ATTRIBUTE ) )^.name ) ,
     pointer(
      ptrcomp(ATTRIBUTE_ptr(ptrcomp(parser.m_atts ) + i * sizeof(ATTRIBUTE ) )^.name ) +
      XmlNameLength(enc ,ATTRIBUTE_ptr(ptrcomp(parser.m_atts ) + i * sizeof(ATTRIBUTE ) )^.name ) ) );

   if attId = NIL then
    begin
     result:=XML_ERROR_NO_MEMORY;

     exit;

    end;

  { Detect duplicate attributes by their QNames. This does not work when
    namespace processing is turned on and different prefixes for the same
    namespace are used. For this case we have a check further down. }
   if XML_Char_ptr(ptrcomp(attId.name ) - 1 * sizeof(XML_Char ) )^ <> XML_Char(0 ) then
    begin
     if enc = parser.m_encoding then
      parser.m_eventPtr:=pointer(ATTRIBUTE_ptr(ptrcomp(parser.m_atts ) + i * sizeof(ATTRIBUTE ) )^.name );

     result:=XML_ERROR_DUPLICATE_ATTRIBUTE;

     exit;

    end;

   XML_Char_ptr(ptrcomp(attId.name ) - 1 * sizeof(XML_Char ) )^:=XML_Char(1 );

   XML_Char_ptr_ptr(ptrcomp(appAtts ) + attIndex * sizeof(XML_Char_ptr ) )^:=attId.name;

   inc(attIndex );

   if ATTRIBUTE_ptr(ptrcomp(parser.m_atts ) + i * sizeof(ATTRIBUTE ) )^.normalized = #0 then
    begin
     isCdata:=XML_TRUE;

    { figure out whether declared as other than CDATA }
     if attId.maybeTokenized <> 0 then
      begin
       j:=0;

       while j < nDefaultAtts do
        begin
         if attId = DEFAULT_ATTRIBUTE_ptr(ptrcomp(elementType.defaultAtts ) + j * sizeof(DEFAULT_ATTRIBUTE ) )^.id then
          begin
           isCdata:=DEFAULT_ATTRIBUTE_ptr(ptrcomp(elementType.defaultAtts ) + j * sizeof(DEFAULT_ATTRIBUTE ) )^.isCdata;

           break;

          end;

         inc(j );

        end;

      end;

    { normalize the attribute value }
     result_:=
      storeAttributeValue(
       parser ,enc ,isCdata ,
       pointer(ATTRIBUTE_ptr(ptrcomp(parser.m_atts ) + i * sizeof(ATTRIBUTE ) )^.valuePtr ) ,
       pointer(ATTRIBUTE_ptr(ptrcomp(parser.m_atts ) + i * sizeof(ATTRIBUTE ) )^.valueEnd ) ,
       @parser.m_tempPool );

     if result_ <> XML_Error(0 ) then
      begin
       result:=result_;

       exit;

      end;

     XML_Char_ptr_ptr(ptrcomp(appAtts ) + attIndex * sizeof(XML_Char_ptr ) )^:=poolStart(@parser.m_tempPool );

     poolFinish(@parser.m_tempPool );

    end
   else
    begin
    { the value did not need normalizing }
     XML_Char_ptr_ptr(ptrcomp(appAtts ) + attIndex * sizeof(XML_Char_ptr ) )^:=
      poolStoreString(
       @parser.m_tempPool ,enc ,
       pointer(ATTRIBUTE_ptr(ptrcomp(parser.m_atts ) + i * sizeof(ATTRIBUTE ) )^.valuePtr ) ,
       pointer(ATTRIBUTE_ptr(ptrcomp(parser.m_atts ) + i * sizeof(ATTRIBUTE ) )^.valueEnd ) );

     if XML_Char_ptr_ptr(ptrcomp(appAtts ) + attIndex * sizeof(XML_Char_ptr ) )^ = NIL then
      begin
       result:=XML_ERROR_NO_MEMORY;

       exit;

      end;

     poolFinish(@parser.m_tempPool );

    end;

  { handle prefixed attribute names }
   if attId.prefix <> NIL then
    if attId.xmlns <> 0 then
     begin
     { deal with namespace declarations here }
      result_:=
       addBinding(
        parser ,attId.prefix ,attId ,
        XML_Char_ptr_ptr(ptrcomp(appAtts ) + attIndex * sizeof(XML_Char_ptr ) )^ ,
        bindingsPtr );

      if result_ <> XML_Error(0 ) then
       begin
        result:=result_;

        exit;

       end;

      dec(attIndex );

     end
    else
     begin
     { deal with other prefixed names later }
      inc(attIndex );
      inc(nPrefixes );

      XML_Char_ptr(ptrcomp(attId.name ) - 1 * sizeof(XML_Char ) )^:=XML_Char(2 );

     end
   else
    inc(attIndex );

   inc(i );

  end;

{ set-up for XML_GetSpecifiedAttributeCount and XML_GetIdAttributeIndex }
 parser.m_nSpecifiedAtts:=attIndex;

 if (elementType.idAtt <> NIL ) and
    (XML_Char_ptr(ptrcomp(elementType.idAtt.name ) - 1 * sizeof(XML_Char ) )^ <> XML_Char(0 ) ) then
  begin
   i:=0;

   while i < attIndex do
    begin
     if XML_Char_ptr_ptr(ptrcomp(appAtts ) + i * sizeof(XML_Char_ptr ) )^ = elementType.idAtt.name then
      begin
       parser.m_idAttIndex:=i;

       break;

      end;

     inc(i ,2 );

    end;

  end
 else
  parser.m_idAttIndex:=-1;

{ do attribute defaulting }
 i:=0;

 while i < nDefaultAtts do
  begin
   da:=DEFAULT_ATTRIBUTE_ptr(ptrcomp(elementType.defaultAtts ) + i * sizeof(DEFAULT_ATTRIBUTE ) );

   if (XML_Char_ptr(ptrcomp(da.id.name ) - 1 * sizeof(XML_Char ) )^ = XML_Char(0 ) ) and
      (da.value <> NIL ) then
    if da.id.prefix <> NIL then
     if da.id.xmlns <> 0 then
      begin
       result_:=
        addBinding(
         parser ,da.id.prefix ,da.id ,
         da.value ,bindingsPtr );

       if result_ <> XML_Error(0 ) then
        begin
         result:=result_;

         exit;

        end;

      end
     else
      begin
       XML_Char_ptr(ptrcomp(da.id.name ) - 1 * sizeof(XML_Char ) )^:=XML_Char(2 );

       inc(nPrefixes );

       XML_Char_ptr_ptr(ptrcomp(appAtts ) + attIndex * sizeof(XML_Char_ptr ) )^:=da.id.name;

       inc(attIndex );

       XML_Char_ptr_ptr(ptrcomp(appAtts ) + attIndex * sizeof(XML_Char_ptr ) )^:=da.value;

       inc(attIndex );

      end
    else
     begin
      XML_Char_ptr(ptrcomp(da.id.name ) - 1 * sizeof(XML_Char ) )^:=XML_Char(1 );

      XML_Char_ptr_ptr(ptrcomp(appAtts ) + attIndex * sizeof(XML_Char_ptr ) )^:=da.id.name;

      inc(attIndex );

      XML_Char_ptr_ptr(ptrcomp(appAtts ) + attIndex * sizeof(XML_Char_ptr ) )^:=da.value;

      inc(attIndex );

     end;

   inc(i );

  end;

 XML_Char_ptr_ptr(ptrcomp(appAtts ) + attIndex * sizeof(XML_Char_ptr ) )^:=NIL;

{ expand prefixed attribute names, check for duplicates,
  and clear flags that say whether attributes were specified }
 i:=0;

 if nPrefixes <> 0 then
  begin
  { j = hash table index }
   version   :=parser.m_nsAttsVersion;
   nsAttsSize:=1 shl parser.m_nsAttsPower;

  { size of hash table must be at least 2 * (# of prefixed attributes) }
   if shr_int32(nPrefixes shl 1 ,parser.m_nsAttsPower ) <> 0 then{ true for nsAttsPower = 0 }
    begin
    { hash table size must also be a power of 2 and >= 8 }
     while shr_int32(nPrefixes ,parser.m_nsAttsPower ) <> 0 do
      inc(parser.m_nsAttsPower );

     if parser.m_nsAttsPower < 3 then
      parser.m_nsAttsPower:=3;

     nsAttsSize:=1 shl parser.m_nsAttsPower;

     if not parser.m_mem.realloc_fcn(
         pointer(parser.m_nsAtts ) ,
         parser.m_nsAttsAlloc ,
         nsAttsSize * sizeof(NS_ATT ) ) then
      begin
       result:=XML_ERROR_NO_MEMORY;

       exit;

      end
     else
      parser.m_nsAttsAlloc:=nsAttsSize * sizeof(NS_ATT );

     version:=0;  { force re-initialization of nsAtts hash table }

    end;

  { using a version flag saves us from initializing nsAtts every time }
   if version = 0 then { initialize version flags when version wraps around }
    begin
     version:=INIT_ATTS_VERSION;

     j:=nsAttsSize;

     while j <> 0 do
      begin
       dec(j );

       NS_ATT_ptr(ptrcomp(parser.m_nsAtts ) + j * sizeof(NS_ATT ) )^.version:=version;

      end;

    end;

   dec(version );

   parser.m_nsAttsVersion:=version;

  { expand prefixed names and check for duplicates }
   while i < attIndex do
    begin
     s:=XML_Char_ptr_ptr(ptrcomp(appAtts ) + i * sizeof(XML_Char_ptr ) )^;

     if XML_Char_ptr(ptrcomp(s ) - 1 * sizeof(XML_Char ) )^ = XML_Char(2 ) then { prefixed }
      begin
       uriHash:=0;

       XML_Char_ptr(ptrcomp(s ) - 1 * sizeof(XML_Char ) )^:=XML_Char(0 ); { clear flag }

       id:=ATTRIBUTE_ID_ptr(lookup(@dtd.attributeIds ,s ,0 ) );
       b :=id.prefix.binding;

       if b = NIL then
        begin
         result:=XML_ERROR_UNBOUND_PREFIX;

         exit;

        end;

      { as we expand the name we also calculate its hash value }
       j:=0;

       while j < b.uriLen do
        begin
         c:=XML_Char_ptr(ptrcomp(b.uri ) + j * sizeof(XML_Char ) )^;

         if poolAppendChar(@parser.m_tempPool ,c ) = 0 then
          begin
           result:=XML_ERROR_NO_MEMORY;

           exit;

          end;

         uriHash:=CHAR_HASH(uriHash ,c );

         inc(j );

        end;

       while s^ <> XML_T(':' ) do
        inc(ptrcomp(s ) ,sizeof(XML_Char ) );

       goto _w0;

       while s^ <> XML_Char(0 ) do { copies null terminator }
        begin
        _w0:
         c:=s^;

         if poolAppendChar(@parser.m_tempPool ,s^ ) = 0 then
          begin
           result:=XML_ERROR_NO_MEMORY;

           exit;

          end;

         uriHash:=CHAR_HASH(uriHash ,c );

         inc(ptrcomp(s ) ,sizeof(XML_Char ) );

        end;

      { Check hash table for duplicate of expanded name (uriName).
        Derived from code in lookup(HASH_TABLE *table, ...). }
       step:=0;
       mask:=nsAttsSize - 1;
       j   :=uriHash and mask;  { index into hash table }

       while NS_ATT_ptr(ptrcomp(parser.m_nsAtts ) + j * sizeof(NS_ATT ) )^.version = version do
        begin
        { for speed we compare stored hash values first }
         if uriHash = NS_ATT_ptr(ptrcomp(parser.m_nsAtts ) + j * sizeof(NS_ATT ) )^.hash then
          begin
           s1:=poolStart(@parser.m_tempPool );
           s2:=NS_ATT_ptr(ptrcomp(parser.m_nsAtts ) + j * sizeof(NS_ATT ) )^.uriName;

          { s1 is null terminated, but not s2 }
           while (s1^ = s2^ ) and
                 (s1^ <> XML_Char(0 ) ) do
            begin
             inc(ptrcomp(s1 ) ,sizeof(XML_Char ) );
             inc(ptrcomp(s2 ) ,sizeof(XML_Char ) );

            end;

           if s1^ = XML_Char(0 ) then
            begin
             result:=XML_ERROR_DUPLICATE_ATTRIBUTE;

             exit;

            end;

          end;

         if step = 0 then
          step:=PROBE_STEP(uriHash ,mask ,parser.m_nsAttsPower );

         if j < step then
          inc(j  ,nsAttsSize - step )
         else
          dec(j ,step );

        end;

       if parser.m_ns_triplets <> 0 then { append namespace separator and prefix }
        begin
         XML_Char_ptr(ptrcomp(parser.m_tempPool.ptr ) - 1 * sizeof(XML_Char ) )^:=parser.m_namespaceSeparator;

         s:=b.prefix.name;

         goto _w1;

         while s^ <> XML_Char(0 ) do
          begin
          _w1:
           if poolAppendChar(@parser.m_tempPool ,s^ ) = 0 then
            begin
             result:=XML_ERROR_NO_MEMORY;

             exit;

            end;

           inc(ptrcomp(s ) ,sizeof(XML_Char ) );

          end;

        end;

      { store expanded name in attribute list }
       s:=poolStart(@parser.m_tempPool );

       poolFinish(@parser.m_tempPool );

       XML_Char_ptr_ptr(ptrcomp(appAtts ) + i * sizeof(XML_Char_ptr ) )^:=s;

      { fill empty slot with new version, uriName and hash value }
       NS_ATT_ptr(ptrcomp(parser.m_nsAtts ) + j * sizeof(NS_ATT ) )^.version:=version;
       NS_ATT_ptr(ptrcomp(parser.m_nsAtts ) + j * sizeof(NS_ATT ) )^.hash   :=uriHash;
       NS_ATT_ptr(ptrcomp(parser.m_nsAtts ) + j * sizeof(NS_ATT ) )^.uriName:=s;

       dec(nPrefixes );

       if nPrefixes = 0 then
        begin
         inc(i ,2 );

         break;

        end;

      end
     else { not prefixed }
      XML_Char_ptr(ptrcomp(s ) - 1 * sizeof(XML_Char ) )^:=XML_Char(0 );  { clear flag }

     inc(i ,2 );

    end;

  end;

{ clear flags for the remaining attributes }
 while i < attIndex do
  begin
   XML_Char_ptr(
    ptrcomp(
     XML_Char_ptr_ptr(ptrcomp(appAtts ) + i * sizeof(XML_Char_ptr ) )^ )
     - 1 * sizeof(XML_Char ) )^:=XML_Char(0 );

   inc(i ,2 );

  end;

 binding:=bindingsPtr^;

 while binding <> NIL do
  begin
   XML_Char_ptr(ptrcomp(binding.attId.name ) - 1 * sizeof(XML_Char ) )^:=XML_Char(0 );

   binding:=binding.nextTagBinding;

  end;

 if parser.m_ns = 0 then
  begin
   result:=XML_ERROR_NONE;

   exit;

  end;

{ expand the element type name }
 if elementType.prefix <> NIL then
  begin
   binding:=elementType.prefix.binding;

   if binding = NIL then
    begin
     result:=XML_ERROR_UNBOUND_PREFIX;

     exit;

    end;

   localPart:=tagNamePtr.str;

   while localPart^ <> XML_T(':' ) do
    inc(ptrcomp(localPart ) ,sizeof(XML_Char ) );

  end
 else
  if dtd.defaultPrefix.binding <> NIL then
   begin
    binding  :=dtd.defaultPrefix.binding;
    localPart:=tagNamePtr.str;

   end
  else
   begin
    result:=XML_ERROR_NONE;

    exit;

   end;

 prefixLen:=0;

 if (parser.m_ns_triplets <> 0 ) and
    (binding.prefix.name <> NIL ) then
  begin
   while XML_Char_ptr(ptrcomp(binding.prefix.name ) + prefixLen * sizeof(XML_Char ) )^ <> XML_Char(0 ) do
    inc(prefixLen );

   inc(prefixLen ); { prefixLen includes null terminator }

  end;

 tagNamePtr.localPart:=localPart;
 tagNamePtr.uriLen   :=binding.uriLen;
 tagNamePtr.prefix   :=binding.prefix.name;
 tagNamePtr.prefixLen:=prefixLen;

 i:=0;

 while XML_Char_ptr(ptrcomp(localPart ) + i * sizeof(XML_Char ) )^ <> XML_Char(0 ) do
  inc(i );

 inc(i ); { i includes null terminator }

 n:=i + binding.uriLen + prefixLen;

 if n > binding.uriAlloc then
  begin
   parser.m_mem.malloc_fcn(pointer(uri ) ,(n + EXPAND_SPARE ) * sizeof(XML_Char ) );

   if uri = NIL then
    begin
     result:=XML_ERROR_NO_MEMORY;

     exit;

    end;

   j:=binding.uriAlloc;

   binding.uriAlloc:=n + EXPAND_SPARE;

   move(
    binding.uri^ ,
    uri^ ,
    binding.uriLen * sizeof(XML_Char ) );

   p:=parser.m_tagStack;

   while p <> NIL do
    begin
     if p.name.str = binding.uri then
      p.name.str:=uri;

     p:=p.parent;

    end;

   parser.m_mem.free_fcn(pointer(binding.uri ) ,j * sizeof(XML_Char ) );

   binding.uri:=uri;

  end;

{ if namespaceSeparator != '\0' then uri includes it already }
 uri:=XML_Char_ptr(ptrcomp(binding.uri ) + binding.uriLen * sizeof(XML_Char ) );

 move(
  localPart^ ,
  uri^ ,
  i * sizeof(XML_Char ) );

{ we always have a namespace separator between localPart and prefix }
 if prefixLen <> 0 then
  begin
   inc(ptrcomp(uri ) ,(i - 1 ) * sizeof(XML_Char ) );

   uri^:=parser.m_namespaceSeparator; { replace null terminator }

   move(
    binding.prefix.name^ ,
    XML_Char_ptr(ptrcomp(uri ) + 1 * sizeof(XML_Char ) )^ ,
    prefixLen * sizeof(XML_Char ) );

  end;

 tagNamePtr.str:=binding.uri;

 result:=XML_ERROR_NONE;

end;

{ processInternalEntity {..}
function processInternalEntity(parser : XML_Parser; entity : ENTITY_ptr; betweenDecl : XML_Bool ) : XML_Error;
begin
 Result:=XML_ERROR_NONE;
end;

{ epilogProcessor }
function epilogProcessor(parser : XML_Parser; s ,end_ : char_ptr; nextPtr : char_ptr_ptr ) : XML_Error;
var
 next : char_ptr;

 tok : int;

begin
 parser.m_processor:=@epilogProcessor;
 parser.m_eventPtr :=s;

 repeat
  next:=NIL;
  tok :=XmlPrologTok(parser.m_encoding ,pointer(s ) ,pointer(end_ ) ,@next );

  parser.m_eventEndPtr:=next;

  case tok of
   -XML_TOK_PROLOG_S :
    begin
     if @parser.m_defaultHandler <> NIL then
      begin
       reportDefault(parser ,parser.m_encoding ,s ,next );

       if parser.m_parsingStatus.parsing = XML_FINISHED then
        begin
         result:=XML_ERROR_ABORTED;

         exit;

        end;

      end;

     nextPtr^:=next;
     result  :=XML_ERROR_NONE;

     exit;

    end;

   XML_TOK_NONE :
    begin
     nextPtr^:=s;
     result  :=XML_ERROR_NONE;

     exit;

    end;

   XML_TOK_PROLOG_S :
    if @parser.m_defaultHandler <> NIL then
     reportDefault(parser ,parser.m_encoding ,s ,next );

   XML_TOK_PI :
    if reportProcessingInstruction(parser ,parser.m_encoding ,s ,next ) = 0 then
     begin
      result:=XML_ERROR_NO_MEMORY;

      exit;

     end;

   XML_TOK_COMMENT :
    if reportComment(parser ,parser.m_encoding ,s ,next ) = 0 then
     begin
      result:=XML_ERROR_NO_MEMORY;

      exit;

     end;

   XML_TOK_INVALID :
    begin
     parser.m_eventPtr:=next;

     result:=XML_ERROR_INVALID_TOKEN;

     exit;

    end;

   XML_TOK_PARTIAL :
    begin
     if parser.m_parsingStatus.finalBuffer = 0 then
      begin
       nextPtr^:=s;
       result  :=XML_ERROR_NONE;

       exit;

      end;

     result:=XML_ERROR_UNCLOSED_TOKEN;

     exit;

    end;

   XML_TOK_PARTIAL_CHAR :
    begin
     if parser.m_parsingStatus.finalBuffer = 0 then
      begin
       nextPtr^:=s;
       result  :=XML_ERROR_NONE;

       exit;

      end;

     result:=XML_ERROR_PARTIAL_CHAR;

     exit;

    end;

   else
    begin
     result:=XML_ERROR_JUNK_AFTER_DOC_ELEMENT;

     exit;

    end;

  end;

  parser.m_eventPtr:=next;

  s:=next;

  case parser.m_parsingStatus.parsing of
   XML_SUSPENDED :
    begin
     nextPtr^:=next;
     result  :=XML_ERROR_NONE;

     exit;

    end;

   XML_FINISHED :
    begin
     result:=XML_ERROR_ABORTED;

     exit;

    end;

  end;

 until false;

end;

{ doCdataSection {..}
{ startPtr gets set to non-null if the section is closed, and to null if
  the section is not yet closed. }
function doCdataSection(
          parser : XML_Parser;
          enc : ENCODING_ptr;
          startPtr : char_ptr_ptr;
          end_ : char_ptr;
          nextPtr : char_ptr_ptr;
          haveMore : XML_Bool ) : XML_Error;
begin
 Result:=XML_ERROR_NONE;
end;

{ cdataSectionProcessor {..}
{ The idea here is to avoid using stack for each CDATA section when
  the whole file is parsed with one call. }
function cdataSectionProcessor(parser : XML_Parser; start ,end_ : char_ptr; endPtr : char_ptr_ptr ) : XML_Error;
begin
 Result:=XML_ERROR_NONE;
end;

{ doContent }
function doContent(
          parser : XML_Parser;
          startTagLevel : int;
          enc : ENCODING_ptr;
          s ,end_ : char_ptr;
          nextPtr : char_ptr_ptr;
          haveMore : XML_Bool ) : XML_Error;
var
 dtd : DTD_ptr;

 eventPP ,eventEndPP : char_ptr_ptr;

 next ,rawNameEnd ,fromPtr ,temp ,rawName : char_ptr;

 tok ,bufSize ,convLen ,len ,n : int;

 c ,ch : XML_Char;

 name ,context ,toPtr ,localPart ,prefix ,uri : XML_Char_ptr;

 entity : ENTITY_ptr;

 result_ : XML_Error;

 tag : TAG_ptr;

 bindings ,b : BINDING_ptr;

 noElmHandlers : XML_Bool;

 name_ : TAG_NAME;

 buf : array[0..XML_ENCODE_MAX - 1 ] of XML_Char;

 dataPtr : ICHAR_ptr;

label
 _break ;

begin
{ save one level of indirection }
 temp:=nil;
 dtd:=parser.m_dtd;

 if enc = parser.m_encoding then
  begin
   eventPP   :=@parser.m_eventPtr;
   eventEndPP:=@parser.m_eventEndPtr;

  end
 else
  begin
   eventPP   :=@parser.m_openInternalEntities.internalEventPtr;
   eventEndPP:=@parser.m_openInternalEntities.internalEventEndPtr;

  end;

 eventPP^:=s;

 repeat
  next:=s; { XmlContentTok doesn't always set the last arg }

  tok:=XmlContentTok(enc ,pointer(s ) ,pointer(end_ ) ,@next );

  eventEndPP^:=next;

  case tok of
   XML_TOK_TRAILING_CR :
    begin
     if haveMore <> 0 then
      begin
       nextPtr^:=s;
       result  :=XML_ERROR_NONE;

       exit;

      end;

     eventEndPP^:=end_;

     if @parser.m_characterDataHandler <> NIL then
      begin
       c:=XML_Char($A );

       parser.m_characterDataHandler(parser.m_handlerArg ,@c ,1 );

      end
     else
      if @parser.m_defaultHandler <> NIL then
       reportDefault(parser ,enc ,s ,end_ );

    { We are at the end of the final buffer, should we check for
      XML_SUSPENDED, XML_FINISHED? }
     if startTagLevel = 0 then
      begin
       result:=XML_ERROR_NO_ELEMENTS;

       exit;

      end;

     if parser.m_tagLevel <> startTagLevel then
      begin
       result:=XML_ERROR_ASYNC_ENTITY;

       exit;

      end;

     nextPtr^:=end_;
     result  :=XML_ERROR_NONE;

     exit;

    end;

   XML_TOK_NONE :
    begin
     if haveMore <> 0 then
      begin
       nextPtr^:=s;
       result  :=XML_ERROR_NONE;

       exit;

      end;

     if startTagLevel > 0 then
      begin
       if parser.m_tagLevel <> startTagLevel then
        begin
         result:=XML_ERROR_ASYNC_ENTITY;

         exit;

        end;

       nextPtr^:=s;
       result  :=XML_ERROR_NONE;

       exit;

      end;

     result:=XML_ERROR_NO_ELEMENTS;

     exit;

    end;

   XML_TOK_INVALID :
    begin
     eventPP^:=next;
     result  :=XML_ERROR_INVALID_TOKEN;

     exit;

    end;

   XML_TOK_PARTIAL :
    begin
     if haveMore <> 0 then
      begin
       nextPtr^:=s;
       result  :=XML_ERROR_NONE;

       exit;

      end;

     result:=XML_ERROR_UNCLOSED_TOKEN;

     exit;

    end;

   XML_TOK_PARTIAL_CHAR :
    begin
     if haveMore <> 0 then
      begin
       nextPtr^:=s;
       result  :=XML_ERROR_NONE;

       exit;

      end;

     result:=XML_ERROR_PARTIAL_CHAR;

     exit;

    end;

   XML_TOK_ENTITY_REF :
    begin
     ch:=
      XML_Char(XmlPredefinedEntityName(
       enc ,
       pointer(ptrcomp(s ) + enc.minBytesPerChar ) ,
       pointer(ptrcomp(next ) - enc.minBytesPerChar ) ) );

     if ch <> XML_Char(0 ) then
      begin
       if @parser.m_characterDataHandler <> NIL then
        parser.m_characterDataHandler(parser.m_handlerArg ,@ch ,1 )
       else
        if @parser.m_defaultHandler <> NIL then
         reportDefault(parser ,enc ,s ,next );

       goto _break;

      end;

     name:=
      poolStoreString(
       @dtd.pool ,enc ,
       char_ptr(ptrcomp(s ) + enc.minBytesPerChar ) ,
       char_ptr(ptrcomp(next ) - enc.minBytesPerChar ) );

     if name = NIL then
      begin
       result:=XML_ERROR_NO_MEMORY;

       exit;

      end;

     entity:=ENTITY_ptr(lookup(@dtd.generalEntities ,name ,0 ) );

     poolDiscard(@dtd.pool );

    { First, determine if a check for an existing declaration is needed;
      if yes, check that the entity exists, and that it is internal,
      otherwise call the skipped entity or default handler. }
     if (dtd.hasParamEntityRefs = 0 ) or
        (dtd.standalone <> 0 ) then
      if entity = NIL then
       begin
        result:=XML_ERROR_UNDEFINED_ENTITY;

        exit;

       end
      else
       if entity.is_internal = 0 then
        begin
         result:=XML_ERROR_ENTITY_DECLARED_IN_PE;

         exit;

        end
       else
     else
      if entity = NIL then
       begin
        if @parser.m_skippedEntityHandler <> NIL then
         parser.m_skippedEntityHandler(parser.m_handlerArg ,name ,0 )
        else
         if @parser.m_defaultHandler <> NIL then
          reportDefault(parser ,enc ,s ,next );

        goto _break;

       end;

     if entity.open <> 0 then
      begin
       result:=XML_ERROR_RECURSIVE_ENTITY_REF;

       exit;

      end;

     if entity.notation <> NIL then
      begin
       result:=XML_ERROR_BINARY_ENTITY_REF;

       exit;

      end;

     if entity.textPtr <> NIL then
      begin
       if parser.m_defaultExpandInternalEntities <> 0 then
        begin
         if @parser.m_skippedEntityHandler <> NIL then
          parser.m_skippedEntityHandler(parser.m_handlerArg ,entity.name ,0 )
         else
          if @parser.m_defaultHandler <> NIL then
           reportDefault(parser ,enc ,s ,next );

         goto _break;

        end;

       result_:=processInternalEntity(parser ,entity ,XML_FALSE );

       if result_ <> XML_ERROR_NONE then
        begin
         result:=result_;

         exit;

        end;

      end
     else
      if @parser.m_externalEntityRefHandler <> NIL then
       begin
        entity.open:=XML_TRUE;
        context    :=getContext(parser );
        entity.open:=XML_FALSE;

        if context = NIL then
         begin
          result:=XML_ERROR_NO_MEMORY;

          exit;

         end;

        if parser.m_externalEntityRefHandler(
            parser.m_externalEntityRefHandlerArg ,
            context ,
            entity.base ,
            entity.systemId ,
            entity.publicId ) = 0 then
         begin
          result:=XML_ERROR_EXTERNAL_ENTITY_HANDLING;

          exit;

         end;

        poolDiscard(@parser.m_tempPool );

       end
      else
       if @parser.m_defaultHandler <> NIL then
        reportDefault(parser ,enc ,s ,next );

    end;

   XML_TOK_START_TAG_NO_ATTS ,XML_TOK_START_TAG_WITH_ATTS :
    begin
     if parser.m_freeTagList <> NIL then
      begin
       tag:=parser.m_freeTagList;

       parser.m_freeTagList:=parser.m_freeTagList.parent;

      end
     else
      begin
       parser.m_mem.malloc_fcn(pointer(tag ) ,sizeof(expat.TAG ) );

       if tag = NIL then
        begin
         result:=XML_ERROR_NO_MEMORY;

         exit;

        end;
       FillByte(tag^,SizeOf(expat.tag),0); // MG

       parser.m_mem.malloc_fcn(pointer(tag.buf ) ,INIT_TAG_BUF_SIZE );

       if tag.buf = NIL then
        begin
         parser.m_mem.free_fcn(pointer(tag ) ,sizeof(expat.TAG ) );

         result:=XML_ERROR_NO_MEMORY;

         exit;

        end
       else
        tag.alloc:=INIT_TAG_BUF_SIZE;

       tag.bufEnd:=char_ptr(ptrcomp(tag.buf ) + INIT_TAG_BUF_SIZE );

      end;

     tag.bindings      :=NIL;
     tag.parent        :=parser.m_tagStack;
     parser.m_tagStack :=tag;
     tag.name.localPart:=NIL;
     tag.name.prefix   :=NIL;
     tag.rawName       :=char_ptr(ptrcomp(s ) + enc.minBytesPerChar );
     tag.rawNameLength :=XmlNameLength(enc ,pointer(tag.rawName ) );

     inc(parser.m_tagLevel );

     rawNameEnd:=char_ptr(ptrcomp(tag.rawName ) + tag.rawNameLength );
     fromPtr   :=tag.rawName;
     toPtr     :=XML_Char_ptr(tag.buf );

     repeat
      XmlConvert(
       enc ,
       @fromPtr ,rawNameEnd ,
       ICHAR_ptr_ptr(@toPtr ) ,ICHAR_ptr(ptrcomp(tag.bufEnd ) - 1 ) );

      convLen:=(ptrcomp(toPtr ) - ptrcomp(tag.buf ) ) div sizeof(XML_Char );

      if fromPtr = rawNameEnd then
       begin
        tag.name.strLen:=convLen;

        break;

       end;

      bufSize:=(ptrcomp(tag.bufEnd ) - ptrcomp(tag.buf ) ) shl 1;

      parser.m_mem.realloc_fcn(pointer(tag.buf ) ,tag.alloc ,bufSize );

      if temp = NIL then
       begin
        result:=XML_ERROR_NO_MEMORY;

        exit;

       end
      else
       tag.alloc:=bufSize;

      tag.buf   :=temp;
      tag.bufEnd:=char_ptr(ptrcomp(temp ) + bufSize );

      toPtr:=XML_Char_ptr(ptrcomp(temp ) + convLen );

     until false;

     tag.name.str:=XML_Char_ptr(tag.buf );

     toPtr^ :=XML_T(#0 );
     result_:=storeAtts(parser ,enc ,s ,@tag.name ,@tag.bindings );

     if result_ <> XML_Error(0 ) then
      begin
       result:=result_;

       exit;

      end;
     if @parser.m_startElementHandler <> NIL then
      parser.m_startElementHandler(
       parser.m_handlerArg ,tag.name.str ,
       XML_Char_ptr_ptr(parser.m_atts ) )
     else
      if @parser.m_defaultHandler <> NIL then
       reportDefault(parser ,enc ,s ,next );

     poolClear(@parser.m_tempPool );

    end;

   XML_TOK_EMPTY_ELEMENT_NO_ATTS ,XML_TOK_EMPTY_ELEMENT_WITH_ATTS :
    begin
     rawName      :=char_ptr(ptrcomp(s ) + enc.minBytesPerChar );
     bindings     :=NIL;
     noElmHandlers:=XML_TRUE;

     name_.str:=
      poolStoreString(
       @parser.m_tempPool ,enc ,rawName ,
       char_ptr(ptrcomp(rawName ) + XmlNameLength(enc ,pointer(rawName ) ) ) );

     if name_.str = NIL then
      begin
       result:=XML_ERROR_NO_MEMORY;

       exit;

      end;

     poolFinish(@parser.m_tempPool );
     result_:=storeAtts(parser ,enc ,s ,@name_ ,@bindings );

     if result_ <> XML_Error(0 ) then
      begin
       result:=result_;

       exit;

      end;

     poolFinish(@parser.m_tempPool );

     if @parser.m_startElementHandler <> NIL then
      begin
       parser.m_startElementHandler(
        parser.m_handlerArg ,name_.str ,XML_Char_ptr_ptr(parser.m_atts ) );

       noElmHandlers:=XML_FALSE;

      end;

     if @parser.m_endElementHandler <> NIL then
      begin
       if @parser.m_startElementHandler <> NIL then
        eventPP^:=eventEndPP^;

       parser.m_endElementHandler(parser.m_handlerArg ,name_.str );

       noElmHandlers:=XML_FALSE;

      end;

     if (noElmHandlers <> 0 ) and
        (@parser.m_defaultHandler <> NIL ) then
      reportDefault(parser ,enc ,s ,next );

     poolClear(@parser.m_tempPool );

     while bindings <> NIL do
      begin
       b:=bindings;

       if @parser.m_endNamespaceDeclHandler <> NIL then
        parser.m_endNamespaceDeclHandler(parser.m_handlerArg ,b.prefix.name );

       bindings        :=bindings.nextTagBinding;
       b.nextTagBinding:=parser.m_freeBindingList;

       parser.m_freeBindingList:=b;
       b.prefix.binding        :=b.prevPrefixBinding;

      end;

     if parser.m_tagLevel = 0 then
      begin
       result:=epilogProcessor(parser ,next ,end_ ,nextPtr );

       exit;

      end;

    end;

   XML_TOK_END_TAG :
    if parser.m_tagLevel = startTagLevel then
     begin
      result:=XML_ERROR_ASYNC_ENTITY;

      exit;

     end
    else
     begin
      tag                 :=parser.m_tagStack;
      parser.m_tagStack   :=tag.parent;
      tag.parent          :=parser.m_freeTagList;
      parser.m_freeTagList:=tag;

      rawName:=char_ptr(ptrcomp(s ) + enc.minBytesPerChar * 2 );
      len    :=XmlNameLength(enc ,pointer(rawName ) );

      if (len <> tag.rawNameLength ) or
         (memcmp(pointer(tag.rawName ) ,pointer(rawName ) ,len ) <> 0 ) then
       begin
        eventPP^:=rawName;
        result  :=XML_ERROR_TAG_MISMATCH;

        exit;

       end;

      dec(parser.m_tagLevel );

      if @parser.m_endElementHandler <> NIL then
       begin
        localPart:=tag.name.localPart;

        if (parser.m_ns <> 0 ) and
           (localPart <> NIL ) then
         begin
         { localPart and prefix may have been overwritten in
           tag->name.str, since this points to the binding->uri
           buffer which gets re-used; so we have to add them again }
          uri:=XML_Char_ptr(ptrcomp(tag.name.str ) + tag.name.uriLen );

         { don't need to check for space - already done in storeAtts() }
          while localPart^ <> XML_Char(0 ) do
           begin
            uri^:=localPart^;

            inc(ptrcomp(uri ) ,sizeof(XML_Char ) );
            inc(ptrcomp(localPart ) ,sizeof(XML_Char ) );

           end;

          prefix:=XML_Char_ptr(tag.name.prefix );

          if (parser.m_ns_triplets <> 0 ) and
             (prefix <> NIL ) then
           begin
            uri^:=parser.m_namespaceSeparator;

            inc(ptrcomp(uri ) ,sizeof(XML_Char ) );

            while prefix^ <> XML_Char(0 ) do
             begin
              uri^:=prefix^;

              inc(ptrcomp(uri ) ,sizeof(XML_Char ) );
              inc(ptrcomp(prefix ) ,sizeof(XML_Char ) );

             end;

           end;

          uri^:=XML_T(#0 );

         end;

        parser.m_endElementHandler(parser.m_handlerArg ,tag.name.str );

       end
      else
       if @parser.m_defaultHandler <> NIL then
        reportDefault(parser ,enc ,s ,next );

      while tag.bindings <> NIL do
       begin
        b:=tag.bindings;

        if @parser.m_endNamespaceDeclHandler <> NIL then
         parser.m_endNamespaceDeclHandler(parser.m_handlerArg ,b.prefix.name );

        tag.bindings            :=tag.bindings.nextTagBinding;
        b.nextTagBinding        :=parser.m_freeBindingList;
        parser.m_freeBindingList:=b;
        b.prefix.binding        :=b.prevPrefixBinding;

       end;

      if parser.m_tagLevel = 0 then
       begin
        result:=epilogProcessor(parser ,next ,end_ ,nextPtr );

        exit;

       end;

     end;

   XML_TOK_CHAR_REF :
    begin
     n:=XmlCharRefNumber(enc ,pointer(s ) );

     if n < 0 then
      begin
       result:=XML_ERROR_BAD_CHAR_REF;

       exit;

      end;

     if @parser.m_characterDataHandler <> NIL then
      parser.m_characterDataHandler(
       parser.m_handlerArg ,@buf[0 ] ,
       XmlEncode(n ,ICHAR_ptr(@buf ) ) )
     else
      if @parser.m_defaultHandler <> NIL then
       reportDefault(parser ,enc ,s ,next );

    end;

   XML_TOK_XML_DECL :
    begin
     result:=XML_ERROR_MISPLACED_XML_PI;

     exit;

    end;

   XML_TOK_DATA_NEWLINE :
    if @parser.m_characterDataHandler <> NIL then
     begin
      c:=XML_Char($A );
      parser.m_characterDataHandler(parser.m_handlerArg ,@c ,1 );
     end
    else
     if @parser.m_defaultHandler <> NIL then
      reportDefault(parser ,enc ,s ,next );

   XML_TOK_CDATA_SECT_OPEN :
    begin
     if @parser.m_startCdataSectionHandler <> NIL then
      parser.m_startCdataSectionHandler(parser.m_handlerArg )
    {$IFDEF 0 }
    { Suppose you doing a transformation on a document that involves
      changing only the character data.  You set up a defaultHandler
      and a characterDataHandler.  The defaultHandler simply copies
      characters through.  The characterDataHandler does the
      transformation and writes the characters out escaping them as
      necessary.  This case will fail to work if we leave out the
      following two lines (because & and < inside CDATA sections will
      be incorrectly escaped).

      However, now we have a start/endCdataSectionHandler, so it seems
      easier to let the user deal with this. }
     else
      if @parser.m_characterDataHandler <> NIL then
       parser.m_characterDataHandler(parser.m_handlerArg ,parser.m_dataBuf ,0 )
    {$ENDIF }
     else
      if @parser.m_defaultHandler <> NIL then
       reportDefault(parser ,enc ,s ,next );

     result_:=doCdataSection(parser ,enc ,@next ,end_ ,nextPtr ,haveMore );

     if result_ <> XML_ERROR_NONE then
      begin
       result:=result_;

       exit;

      end
     else
      if next = NIL then
       begin
        parser.m_processor:=@cdataSectionProcessor;

        result:=result_;

        exit;

       end;

    end;

   XML_TOK_TRAILING_RSQB :
    begin
     if haveMore <> 0 then
      begin
       nextPtr^:=s;
       result  :=XML_ERROR_NONE;

       exit;

      end;

     if @parser.m_characterDataHandler <> NIL then
      if MUST_CONVERT(enc ,s ) <> 0 then
       begin
        dataPtr:=ICHAR_ptr(parser.m_dataBuf );

        XmlConvert(enc ,@s ,end_ ,@dataPtr ,ICHAR_ptr(parser.m_dataBufEnd ) );

        parser.m_characterDataHandler(
         parser.m_handlerArg ,
         parser.m_dataBuf ,
         (ptrcomp(dataPtr ) - ptrcomp(parser.m_dataBuf ) ) div sizeof(ICHAR ) );

       end
      else
       parser.m_characterDataHandler(
        parser.m_handlerArg ,
        XML_Char_ptr(s ) ,
        (ptrcomp(end_ ) - ptrcomp(s ) ) div sizeof(XML_Char ) )
     else
      if @parser.m_defaultHandler <> NIL then
       reportDefault(parser ,enc ,s ,end_ );

    { We are at the end of the final buffer, should we check for
      XML_SUSPENDED, XML_FINISHED? }
     if startTagLevel = 0 then
      begin
       eventPP^:=end_;
       result  :=XML_ERROR_NO_ELEMENTS;

       exit;

      end;

     if parser.m_tagLevel <> startTagLevel then
      begin
       eventPP^:=end_;
       result  :=XML_ERROR_ASYNC_ENTITY;

       exit;

      end;

     nextPtr^:=end_;
     result  :=XML_ERROR_NONE;

     exit;

    end;

   XML_TOK_DATA_CHARS :
    if @parser.m_characterDataHandler <> NIL then
     if MUST_CONVERT(enc ,s ) <> 0 then
      repeat
       dataPtr:=ICHAR_ptr(parser.m_dataBuf );

       XmlConvert(enc ,@s ,next ,@dataPtr ,ICHAR_ptr(parser.m_dataBufEnd ) );

       eventEndPP^:=s;

       parser.m_characterDataHandler(
        parser.m_handlerArg ,
        parser.m_dataBuf ,
        (ptrcomp(dataPtr ) - ptrcomp(parser.m_dataBuf ) ) div sizeof(ICHAR ) );

       if s = next then
        break;

       eventPP^:=s;

      until false
     else
      parser.m_characterDataHandler(
       parser.m_handlerArg ,
       XML_Char_ptr(s ) ,
       (ptrcomp(next ) - ptrcomp(s ) ) div sizeof(XML_Char ) )

    else
     if @parser.m_defaultHandler <> NIL then
      reportDefault(parser ,enc ,s ,next );

   XML_TOK_PI :
    if reportProcessingInstruction(parser ,enc ,s ,next ) = 0 then
     begin
      result:=XML_ERROR_NO_MEMORY;

      exit;

     end;

   XML_TOK_COMMENT :
    if reportComment(parser ,enc ,s ,next ) = 0 then
     begin
      result:=XML_ERROR_NO_MEMORY;

      exit;

     end;

   else
    if @parser.m_defaultHandler <> NIL then
     reportDefault(parser ,enc ,s ,next );

  end;

 _break:
  eventPP^:=next;
  s       :=next;

  case parser.m_parsingStatus.parsing of
   XML_SUSPENDED:
    begin
     nextPtr^:=next;
     result  :=XML_ERROR_NONE;

     exit;

    end;

   XML_FINISHED:
    begin
     result:=XML_ERROR_ABORTED;

     exit;

    end;

  end;

 until false;

{ not reached }

end;

{ contentProcessor }
function contentProcessor(parser : XML_Parser; start ,end_ : char_ptr; endPtr : char_ptr_ptr ) : XML_Error;
var
 result_ : XML_Error;

begin
 result_:=
  doContent(
   parser ,0 ,parser.m_encoding ,start ,end_ ,
   endPtr ,XML_Bool(not parser.m_parsingStatus.finalBuffer ) );

 if result_ = XML_ERROR_NONE then
  if storeRawNames(parser ) = 0 then
   begin
    result:=XML_ERROR_NO_MEMORY;

    exit;

   end;

 result:=result_;

end;

{ getElementType {..}
function getElementType(parser : XML_Parser; enc : ENCODING_ptr; ptr ,end_ : char_ptr ) : ELEMENT_TYPE_ptr;
begin
 Result:=0;
end;

{ getAttributeId }
function getAttributeId(parser : XML_Parser; enc : ENCODING_ptr; start ,end_ : char_ptr ) : ATTRIBUTE_ID_ptr;
var
 dtd : DTD_ptr;

 id : ATTRIBUTE_ID_ptr;

 name : XML_Char_ptr;

 i ,j : int;

begin
{ save one level of indirection }
 dtd:=parser.m_dtd;

 if poolAppendChar(@dtd.pool ,XML_T(#0 ) ) = 0 then
  begin
   result:=NIL;

   exit;

  end;

 name:=poolStoreString(@dtd.pool ,enc ,start ,end_ );

 if name = NIL then
  begin
   result:=NIL;

   exit;

  end;

{ skip quotation mark - its storage will be re-used (like in name[-1]) }
 inc(ptrcomp(name ) ,sizeof(XML_Char ) );

 id:=ATTRIBUTE_ID_ptr(lookup(@dtd.attributeIds ,name ,sizeof(ATTRIBUTE_ID ) ) );

 if id = NIL then
  begin
   result:=NIL;

   exit;

  end;

 if id.name <> name then
  poolDiscard(@dtd.pool )

 else
  begin
   poolFinish(@dtd.pool );

   if parser.m_ns = 0 then
   else
    if (XML_Char_ptr(ptrcomp(name ) + 0 * sizeof(XML_Char ) )^ = XML_T('x' ) ) and
       (XML_Char_ptr(ptrcomp(name ) + 1 * sizeof(XML_Char ) )^ = XML_T('m' ) ) and
       (XML_Char_ptr(ptrcomp(name ) + 2 * sizeof(XML_Char ) )^ = XML_T('l' ) ) and
       (XML_Char_ptr(ptrcomp(name ) + 3 * sizeof(XML_Char ) )^ = XML_T('n' ) ) and
       (XML_Char_ptr(ptrcomp(name ) + 4 * sizeof(XML_Char ) )^ = XML_T('s' ) ) and
       ((XML_Char_ptr(ptrcomp(name ) + 5 * sizeof(XML_Char ) )^ = XML_T(#0 ) ) or
        (XML_Char_ptr(ptrcomp(name ) + 5 * sizeof(XML_Char ) )^ = XML_T(':' ) ) ) then
     begin
      if XML_Char_ptr(ptrcomp(name ) + 5 * sizeof(XML_Char ) )^ = XML_T(#0 ) then
       id.prefix:=@dtd.defaultPrefix
      else
       id.prefix:=PREFIX_ptr(lookup(@dtd.prefixes ,XML_Char_ptr(ptrcomp(name ) + 6 * sizeof(XML_Char ) ) ,sizeof(PREFIX ) ) );

      id.xmlns:=XML_TRUE;

     end
    else
     begin
      i:=0;

      while XML_Char_ptr(ptrcomp(name ) + i * sizeof(XML_Char ) )^ <> XML_Char(0 ) do
       begin
       { attributes without prefix are *not* in the default namespace }
        if XML_Char_ptr(ptrcomp(name ) + i * sizeof(XML_Char ) )^ = XML_T(':' ) then
         begin
          j:=0;

          while j < i do
           begin
            if poolAppendChar(@dtd.pool ,XML_Char_ptr(ptrcomp(name ) + j * sizeof(XML_Char ) )^ ) = 0 then
             begin
              result:=NIL;

              exit;

             end;

            inc(j );

           end;

          if poolAppendChar(@dtd.pool ,XML_T(#0 ) ) = 0 then
           begin
            result:=NIL;

            exit;

           end;

          id.prefix:=
           PREFIX_ptr(
            lookup(@dtd.prefixes ,poolStart(@dtd.pool ) ,sizeof(PREFIX ) ) );

          if id.prefix.name = poolStart(@dtd.pool ) then
           poolFinish(@dtd.pool )
          else
           poolDiscard(@dtd.pool );

          break;

         end;

        inc(i );

       end;

     end;

  end;

 result:=id;

end;

{ defineAttribute {..}
function defineAttribute(
          type_ : ELEMENT_TYPE_ptr; attId : ATTRIBUTE_ID_ptr;
          isCdata ,isId : XML_Bool; value : XML_Char_ptr; parser : XML_Parser ) : int;
begin
 Result:=0;
end;

{ appendAttributeValue }
function appendAttributeValue(
          parser : XML_Parser; enc : ENCODING_ptr; isCdata : XML_Bool;
          ptr ,end_ : char_ptr; pool : STRING_POOL_ptr ) : XML_Error;
var
 dtd : DTD_ptr;

 next : char_ptr;

 tok ,i ,n : int;

 buf : array[0..XML_ENCODE_MAX - 1 ] of XML_Char;

 name ,textEnd : XML_Char_ptr;

 entity : ENTITY_ptr;

 checkEntityDecl : char;

 ch : XML_Char;

 result_ : XML_Error;

label
 _break ,_go0 ; 

begin
{ save one level of indirection }
 dtd:=parser.m_dtd;

 repeat
  tok:=XmlAttributeValueTok(enc ,pointer(ptr ) ,pointer(end_ ) ,@next );

  case tok of
   XML_TOK_NONE :
    begin
     result:=XML_ERROR_NONE;

     exit;

    end;

   XML_TOK_INVALID :
    begin
     if enc = parser.m_encoding then
      parser.m_eventPtr:=next;

     result:=XML_ERROR_INVALID_TOKEN;

    end;

   XML_TOK_PARTIAL :
    begin
     if enc = parser.m_encoding then
      parser.m_eventPtr:=ptr;

     result:=XML_ERROR_INVALID_TOKEN;

    end;

   XML_TOK_CHAR_REF :
    begin
     n:=XmlCharRefNumber(enc ,pointer(ptr ) );

     if n < 0 then
      begin
       if enc = parser.m_encoding then
        parser.m_eventPtr:=ptr;

       result:=XML_ERROR_BAD_CHAR_REF;

      end;

     if (isCdata = 0 ) and
        (n = $20 ) and { space }
        ((poolLength(pool ) = 0 ) or
         (poolLastChar(pool ) = XML_Char($20 ) ) ) then
      goto _break;

     n:=XmlEncode(n ,ICHAR_ptr(buf ) );

     if n = 0 then
      begin
       if enc = parser.m_encoding then
        parser.m_eventPtr:=ptr;

       result:=XML_ERROR_BAD_CHAR_REF;

       exit;

      end;

     i:=0;

     while i < n do
      begin
       if poolAppendChar(pool ,buf[i ] ) = 0 then
        begin
         result:=XML_ERROR_NO_MEMORY;

         exit;

        end;

       inc(i );

      end;

    end;

   XML_TOK_DATA_CHARS :
    if poolAppend(pool ,enc ,ptr ,next ) = NIL then
     begin
      result:=XML_ERROR_NO_MEMORY;

      exit;

     end;

   XML_TOK_TRAILING_CR :
    begin
     next:=char_ptr(ptrcomp(ptr ) + enc.minBytesPerChar );

     goto _go0;

    end;

   XML_TOK_ATTRIBUTE_VALUE_S ,XML_TOK_DATA_NEWLINE :
   _go0:
    begin
     if (isCdata = 0 ) and
        ((poolLength(pool ) = 0 ) or
         (poolLastChar(pool ) = XML_Char($20 ) ) ) then
      goto _break;

     if poolAppendChar(pool ,char($20 ) ) = 0 then
      begin
       result:=XML_ERROR_NO_MEMORY;

       exit;

      end;

    end;

   XML_TOK_ENTITY_REF :
    begin
     ch:=
      XML_Char(
       XmlPredefinedEntityName(
        enc ,
        pointer(char_ptr(ptrcomp(ptr ) + enc.minBytesPerChar ) ) ,
        pointer(char_ptr(ptrcomp(next ) - enc.minBytesPerChar ) ) ) );

     if ch <> XML_Char(0 ) then
      begin
       if poolAppendChar(pool, ch) = 0 then
        begin
         result:=XML_ERROR_NO_MEMORY;

         exit;

        end;

       goto _break;

      end;

     name:=
      poolStoreString(
       @parser.m_temp2Pool ,enc ,
       char_ptr(ptrcomp(ptr ) + enc.minBytesPerChar ) ,
       char_ptr(ptrcomp(next ) - enc.minBytesPerChar ) );

     if name = NIL then
      begin
       result:=XML_ERROR_NO_MEMORY;

       exit;

      end;

     entity:=ENTITY_ptr(lookup(@parser.m_dtd.generalEntities ,name ,0 ) );

     poolDiscard(@parser.m_temp2Pool );

    { First, determine if a check for an existing declaration is needed;
      if yes, check that the entity exists, and that it is internal. }
     if pool = @parser.m_dtd.pool then  { are we called from prolog? }
      begin
       if dtd.standalone <> 0 then
        checkEntityDecl:=char(parser.m_openInternalEntities = NIL )
       else
        checkEntityDecl:=char(dtd.hasParamEntityRefs = 0 );

      {$IFDEF XML_DTD }
       checkEntityDecl:=char((checkEntityDecl <> #0 ) and (parser.m_prologState.documentEntity <> 0 ) )

      {$ENDIF }

      end
     else { if pool = @tempPool: we are called from content }
      checkEntityDecl:=char((dtd.hasParamEntityRefs = 0 ) or (dtd.standalone <> 0 ) );

     if checkEntityDecl <> #0 then
      if entity = NIL then
       begin
        result:=XML_ERROR_UNDEFINED_ENTITY;

        exit;

       end
      else
       if entity.is_internal = 0 then
        begin
         result:=XML_ERROR_ENTITY_DECLARED_IN_PE;

         exit;

        end
       else
     else
      if entity = NIL then
      { Cannot report skipped entity here - see comments on
        skippedEntityHandler.
        if @parser.m_skippedEntityHandler <> NIL then
         parser.m_skippedEntityHandler(parser.m_handlerArg ,name ,0 ); }
      { Cannot call the default handler because this would be
        out of sync with the call to the startElementHandler.
        if (pool = @parser.m_tempPool ) and
           (@parser.m_defaultHandler <> NIL ) then
         reportDefault(parser ,enc ,ptr ,next ); }
       goto _break;

     if entity.open <> 0 then
      begin
       if enc = parser.m_encoding then
        parser.m_eventPtr:=ptr;

       result:=XML_ERROR_RECURSIVE_ENTITY_REF;

       exit;

      end;

     if entity.notation <> NIL then
      begin
       if enc = parser.m_encoding then
        parser.m_eventPtr:=ptr;

       result:=XML_ERROR_BINARY_ENTITY_REF;

       exit;

      end;

     if entity.textPtr = NIL then
      begin
       if enc = parser.m_encoding then
        parser.m_eventPtr:=ptr;

       result:=XML_ERROR_ATTRIBUTE_EXTERNAL_ENTITY_REF;

       exit;

      end
     else
      begin
       textEnd:=XML_Char_ptr(ptrcomp(entity.textPtr ) + entity.textLen * sizeof(XML_Char ) );

       entity.open:=XML_TRUE;

       result_:=
        appendAttributeValue(
         parser ,parser.m_internalEncoding ,isCdata ,
         char_ptr(entity.textPtr ) ,
         char_ptr(textEnd ) ,pool );

       entity.open:=XML_FALSE;

       if result_ <> XML_Error(0 ) then
        begin
         result:=result_;

         exit;

        end;

      end;

    end;

   else
    begin
     if enc = parser.m_encoding then
      parser.m_eventPtr:=ptr;

     result:=XML_ERROR_UNEXPECTED_STATE;

     exit;

    end;

  end;

 _break:
  ptr:=next;

 until false;

{ not reached }

end;

{ storeAttributeValue }
function storeAttributeValue(
          parser : XML_Parser; enc : ENCODING_ptr; isCdata : XML_Bool;
          ptr ,end_ : char_ptr; pool : STRING_POOL_ptr ) : XML_Error;
var
 result_ : XML_Error;

begin
 result_:=appendAttributeValue(parser ,enc ,isCdata ,ptr ,end_ ,pool );

 if result_ <> XML_Error(0 ) then
  begin
   result:=result_;

   exit;

  end;

 if (isCdata = 0 ) and
    (poolLength(pool ) <> 0 ) and
    (poolLastChar(pool ) = XML_Char($20 ) ) then
  poolChop(pool );

 if poolAppendChar(pool ,XML_T(#0 ) ) = 0 then
  begin
   result:=XML_ERROR_NO_MEMORY;

   exit;

  end;

 result:=XML_ERROR_NONE;

end;

{ storeEntityValue {..}
function storeEntityValue(parser : XML_Parser; enc : ENCODING_ptr; start ,end_ : char_ptr ) : XML_Error;
begin
 Result:=XML_ERROR_NONE;
end;

{ doIgnoreSection {..}
{ startPtr gets set to non-null is the section is closed, and to null
  if the section is not yet closed. }
function doIgnoreSection(
          parser : XML_Parser;
          enc : ENCODING_ptr;
          startPtr : char_ptr_ptr;
          end_ : char_ptr;
          nextPtr : char_ptr_ptr;
          haveMore : XML_Bool ) : XML_Error;
begin
 Result:=XML_ERROR_NONE;
end;

{ ignoreSectionProcessor {..}
{ The idea here is to avoid using stack for each IGNORE section when
  the whole file is parsed with one call. }
function ignoreSectionProcessor(parser : XML_Parser; start ,end_ : char_ptr; endPtr : char_ptr_ptr ) : XML_Error;
begin
 Result:=XML_ERROR_NONE;
end;

{ nextScaffoldPart {..}
function nextScaffoldPart(parser : XML_Parser ) : int;
begin
 Result:=0;
end;

{ build_model {..}
function build_model(parser : XML_Parser ) : XML_Content_ptr;
begin
 Result:=nil;
end;

{ reportProcessingInstruction {..}
function reportProcessingInstruction(parser : XML_Parser; enc : ENCODING_ptr; start ,end_ : char_ptr ) : int;
begin
 Result:=0;
end;

{ normalizeLines {..}
procedure normalizeLines(s : XML_Char_ptr );
begin
end;

{ reportComment }
function reportComment(parser : XML_Parser; enc : ENCODING_ptr; start ,end_ : char_ptr ) : int;
var
 data : XML_Char_ptr;

begin
 if @parser.m_commentHandler = NIL then
  begin
   if @parser.m_defaultHandler <> NIL then
    reportDefault(parser ,enc ,start ,end_ );

   result:=1;

   exit;

  end;

 data:=
  poolStoreString(
   @parser.m_tempPool ,enc ,
   char_ptr(ptrcomp(start ) + enc.minBytesPerChar * 4 ) ,
   char_ptr(ptrcomp(end_ ) - enc.minBytesPerChar * 3 ) );

 if data = NIL then
  begin
   result:=0;

   exit;

  end;

 normalizeLines(data );

 parser.m_commentHandler(parser.m_handlerArg ,data );

 poolClear(@parser.m_tempPool );

 result:=1;

end;

{ doProlog }
function doProlog(
          parser : XML_Parser;
          enc : ENCODING_ptr;
          s ,end_ : char_ptr;
          tok : int;
          next : char_ptr;
          nextPtr : char_ptr_ptr;
          haveMore : XML_Bool ) : XML_Error;
const
{$IFDEF XML_DTD }
 externalSubsetName : array[0..1 ] of XML_Char = ('#' ,#0 );

{$ENDIF }

 atypeCDATA     : array[0..5 ] of XML_Char = ('C' ,'D' ,'A' ,'T' ,'A' ,#0 );
 atypeID        : array[0..2 ] of XML_Char = ('I' ,'D' ,#0 );
 atypeIDREF     : array[0..5 ] of XML_Char = ('I' ,'D' ,'R' ,'E' ,'F' ,#0 );
 atypeIDREFS    : array[0..6 ] of XML_Char = ('I' ,'D' ,'R' ,'E' ,'F' ,'S' ,#0 );
 atypeENTITY    : array[0..6 ] of XML_Char = ('E' ,'N' ,'T' ,'I' ,'T' ,'Y' ,#0 );
 atypeENTITIES  : array[0..8 ] of XML_Char = ('E' ,'N' ,'T' ,'I' ,'T' ,'I' ,'E' ,'S',#0 );
 atypeNMTOKEN   : array[0..7 ] of XML_Char = ('N' ,'M' ,'T' ,'O' ,'K' ,'E' ,'N' ,#0 );
 atypeNMTOKENS  : array[0..8 ] of XML_Char = ('N' ,'M' ,'T' ,'O' ,'K' ,'E' ,'N' ,'S',#0 );
 notationPrefix : array[0..8 ] of XML_Char = ('N' ,'O' ,'T' ,'A' ,'T' ,'I' ,'O' ,'N',#0 );
 enumValueSep   : array[0..1 ] of XML_Char = ('|' ,#0 );
 enumValueStart : array[0..1 ] of XML_Char = ('(' ,#0 );

var
 dtd : DTD_ptr;

 eventPP ,eventEndPP : char_ptr_ptr;

 quant : XML_Content_Quant;

 role ,myindex ,nameLen : int;

 handleDefault ,hadParamEntityRefs ,ok ,betweenDecl : XML_Bool;

 result_ : XML_Error;

 tem ,prefix ,attVal ,name ,systemId : XML_Char_ptr;

 entity : ENTITY_ptr;

 //ctemp : char_ptr;
 nxt : char_ptr;

 //itemp : int_ptr;

 content ,model : XML_Content_ptr;

 el : ELEMENT_TYPE_ptr;

label
 _break ,_go0 ,_go1 ,
 alreadyChecked ,checkAttListDeclHandler ,elementContent ,closeGroup ;

begin
{ save one level of indirection }
 dtd:=parser.m_dtd;

 if enc = parser.m_encoding then
  begin
   eventPP   :=@parser.m_eventPtr;
   eventEndPP:=@parser.m_eventEndPtr;

  end
 else
  begin
   eventPP   :=@parser.m_openInternalEntities.internalEventPtr;
   eventEndPP:=@parser.m_openInternalEntities.internalEventEndPtr;

  end;

 repeat
  handleDefault:=XML_TRUE;
  eventPP^     :=s;
  eventEndPP^  :=next;

  if tok <= 0 then
   begin
    if (haveMore <> 0 ) and
       (tok <> XML_TOK_INVALID ) then
     begin
      nextPtr^:=s;
      result  :=XML_ERROR_NONE;

      exit;

     end;

    case tok of
     XML_TOK_INVALID :
      begin
       eventPP^:=next;
       result  :=XML_ERROR_INVALID_TOKEN;

       exit;

      end;

     XML_TOK_PARTIAL :
      begin
       result:=XML_ERROR_UNCLOSED_TOKEN;

       exit;

      end;

     XML_TOK_PARTIAL_CHAR :
      begin
       result:=XML_ERROR_PARTIAL_CHAR;

       exit;

      end;

     XML_TOK_NONE :
      begin
      {$IFDEF XML_DTD }
      { for internal PE NOT referenced between declarations }
       if (enc <> parser.m_encoding ) and
          (parser.m_openInternalEntities.betweenDecl = 0 ) then
        begin
         nextPtr^:=s;
         result  :=XML_ERROR_NONE;

         exit;

        end;

      { WFC: PE Between Declarations - must check that PE contains
        complete markup, not only for external PEs, but also for
        internal PEs if the reference occurs between declarations. }
       if (parser.m_isParamEntity <> 0 ) or
          (enc <> parser.m_encoding ) then
        begin
         if XmlTokenRole(@parser.m_prologState ,XML_TOK_NONE ,pointer(end_ ) ,pointer(end_ ) ,enc ) = XML_ROLE_ERROR then
          begin
           result:=XML_ERROR_INCOMPLETE_PE;

           exit;

          end;

         nextPtr^:=s;
         result  :=XML_ERROR_NONE;

         exit;

        end;

      {$ENDIF }

       result:=XML_ERROR_NO_ELEMENTS;

       exit;

      end;

     else
      begin
       tok :=-tok;
       next:=end_;

      end;

    end;

   end;

  role:=XmlTokenRole(@parser.m_prologState ,tok ,pointer(s ) ,pointer(next ) ,enc );

  case role of
   XML_ROLE_XML_DECL :
    begin
     result_:=processXmlDecl(parser ,0 ,s ,next );

     if result_ <> XML_ERROR_NONE then
      begin
       result:=result_;

       exit;

      end;

     enc:=parser.m_encoding;

     handleDefault:=XML_FALSE;

    end;

   XML_ROLE_DOCTYPE_NAME :
    begin
     if @parser.m_startDoctypeDeclHandler <> NIL then
      begin
       parser.m_doctypeName:=poolStoreString(@parser.m_tempPool ,enc ,s ,next );

       if parser.m_doctypeName = NIL then
        begin
         result:=XML_ERROR_NO_MEMORY;

         exit;

        end;

       poolFinish(@parser.m_tempPool );

       parser.m_doctypePubid:=NIL;
       handleDefault        :=XML_FALSE;

      end;

     parser.m_doctypeSysid:=NIL; { always initialize to NULL }

    end;

   XML_ROLE_DOCTYPE_INTERNAL_SUBSET :
    if @parser.m_startDoctypeDeclHandler <> NIL then
     begin
      parser.m_startDoctypeDeclHandler(
       parser.m_handlerArg ,parser.m_doctypeName ,
       parser.m_doctypeSysid ,parser.m_doctypePubid ,1 );

      parser.m_doctypeName:=NIL;

      poolClear(@parser.m_tempPool );

      handleDefault:=XML_FALSE;

     end;

  {$IFDEF XML_DTD }
   XML_ROLE_TEXT_DECL :
    begin
     result_:=processXmlDecl(parser ,1 ,s ,next );

     if result_ <> XML_ERROR_NONE then
      begin
       result:=result_;

       exit;

      end;

     enc          :=parser.m_encoding;
     handleDefault:=XML_FALSE;

    end;

  {$ENDIF }

   XML_ROLE_DOCTYPE_PUBLIC_ID :
    begin
    {$IFDEF XML_DTD }
     parser.m_useForeignDTD:=XML_FALSE;
     parser.m_declEntity   :=
      ENTITY_ptr(
       lookup(@dtd.paramEntities ,@externalSubsetName[0 ] ,sizeof(expat.ENTITY ) ) );

     if parser.m_declEntity = NIL then
      begin
       result:=XML_ERROR_NO_MEMORY;

       exit;

      end;

    {$ENDIF }
     dtd.hasParamEntityRefs:=XML_TRUE;

     if @parser.m_startDoctypeDeclHandler <> NIL then
      begin
       if XmlIsPublicId(enc ,pointer(s ) ,pointer(next ) ,pointer(eventPP ) ) = 0 then
        begin
         result:=XML_ERROR_PUBLICID;

         exit;

        end;

       parser.m_doctypePubid:=
        poolStoreString(
         @parser.m_tempPool ,enc ,
         char_ptr(ptrcomp(s ) + enc.minBytesPerChar ) ,
         char_ptr(ptrcomp(next ) - enc.minBytesPerChar ) );

       if parser.m_doctypePubid = NIL then
        begin
         result:=XML_ERROR_NO_MEMORY;

         exit;

        end;

       normalizePublicId(XML_Char_ptr(parser.m_doctypePubid ) );
       poolFinish       (@parser.m_tempPool );

       handleDefault:=XML_FALSE;

       goto alreadyChecked;

      end;

    { fall through }
     goto _go0;

    end;

   XML_ROLE_ENTITY_PUBLIC_ID :
   _go0:
    begin
     if XmlIsPublicId(enc ,pointer(s ) ,pointer(next ) ,pointer(eventPP ) ) = 0 then
      begin
       result:=XML_ERROR_PUBLICID;

       exit;

      end;

    alreadyChecked:
     if (dtd.keepProcessing <> 0 ) and
        (parser.m_declEntity <> NIL ) then
      begin
       tem:=
        poolStoreString(
         @dtd.pool ,enc ,
         char_ptr(ptrcomp(s ) + enc.minBytesPerChar ) ,
         char_ptr(ptrcomp(next ) - enc.minBytesPerChar ) );

       if tem = NIL then
        begin
         result:=XML_ERROR_NO_MEMORY;

         exit;

        end;

       normalizePublicId(tem );

       parser.m_declEntity.publicId:=tem;

       poolFinish(@dtd.pool );

       if @parser.m_entityDeclHandler <> NIL then
        handleDefault:=XML_FALSE;

      end;

    end;

   XML_ROLE_DOCTYPE_CLOSE :
    begin
     if parser.m_doctypeName <> NIL then
      begin
       parser.m_startDoctypeDeclHandler(
        parser.m_handlerArg ,parser.m_doctypeName ,
        parser.m_doctypeSysid ,parser.m_doctypePubid ,0 );

       poolClear(@parser.m_tempPool );

       handleDefault:=XML_FALSE;

      end;

    { doctypeSysid will be non-NULL in the case of a previous
      XML_ROLE_DOCTYPE_SYSTEM_ID, even if startDoctypeDeclHandler
      was not set, indicating an external subset }
    {$IFDEF XML_DTD }
     if (parser.m_doctypeSysid <> NIL ) or
        (parser.m_useForeignDTD <> 0 ) then
      begin
       hadParamEntityRefs    :=dtd.hasParamEntityRefs;
       dtd.hasParamEntityRefs:=XML_TRUE;

       if (parser.m_paramEntityParsing <> XML_ParamEntityParsing(0 ) ) and
          (@parser.m_externalEntityRefHandler <> NIL ) then
        begin
         entity:=
          ENTITY_ptr(
           lookup(@dtd.paramEntities ,@externalSubsetName[0 ] ,sizeof(expat.ENTITY ) ) );

         if entity = NIL then
          begin
           result:=XML_ERROR_NO_MEMORY;

           exit;

          end;

         if parser.m_useForeignDTD <> 0 then
          entity.base:=parser.m_curBase;

         dtd.paramEntityRead:=XML_FALSE;

         if parser.m_externalEntityRefHandler(
             parser.m_externalEntityRefHandlerArg ,nil ,
             entity.base ,
             entity.systemId ,
             entity.publicId ) = 0 then
          begin
           result:=XML_ERROR_EXTERNAL_ENTITY_HANDLING;

           exit;

          end;

         if dtd.paramEntityRead <> 0 then
          if (dtd.standalone = 0 ) and
             (@parser.m_notStandaloneHandler <> NIL ) and
             (parser.m_notStandaloneHandler(parser.m_handlerArg ) = 0 ) then
           begin
            result:=XML_ERROR_NOT_STANDALONE;

            exit;

           end
          else
         else
         { if we didn't read the foreign DTD then this means that there
           is no external subset and we must reset dtd.hasParamEntityRefs }
          if parser.m_doctypeSysid = NIL then
           dtd.hasParamEntityRefs:=hadParamEntityRefs;

         { end of DTD - no need to update dtd.keepProcessing }

        end;

       parser.m_useForeignDTD:=XML_FALSE;

      end;

    {$ENDIF }
     if @parser.m_endDoctypeDeclHandler <> NIL then
      begin
       parser.m_endDoctypeDeclHandler(parser.m_handlerArg );

       handleDefault:=XML_FALSE;

      end;

    end;

   XML_ROLE_INSTANCE_START :
    begin
    {$IFDEF XML_DTD }
    { if there is no DOCTYPE declaration then now is the
      last chance to read the foreign DTD }
     if parser.m_useForeignDTD <> 0 then
      begin
       hadParamEntityRefs    :=dtd.hasParamEntityRefs;
       dtd.hasParamEntityRefs:=XML_TRUE;

       if (parser.m_paramEntityParsing <> XML_ParamEntityParsing(0 ) ) and
          (@parser.m_externalEntityRefHandler <> NIL ) then
        begin
         entity:=
          ENTITY_ptr(
           lookup(@dtd.paramEntities ,@externalSubsetName[0 ] ,sizeof(expat.ENTITY ) ) );

         if entity = NIL then
          begin
           result:=XML_ERROR_NO_MEMORY;

           exit;

          end;

         entity.base        :=parser.m_curBase;
         dtd.paramEntityRead:=XML_FALSE;

         if parser.m_externalEntityRefHandler(
             parser.m_externalEntityRefHandlerArg ,nil ,
             entity.base ,
             entity.systemId ,
             entity.publicId ) = 0 then
          begin
           result:=XML_ERROR_EXTERNAL_ENTITY_HANDLING;

           exit;

          end;

         if dtd.paramEntityRead <> 0 then
          if (dtd.standalone = 0 ) and
             (@parser.m_notStandaloneHandler <> NIL ) and
             (parser.m_notStandaloneHandler(parser.m_handlerArg ) = 0 ) then
           begin
            result:=XML_ERROR_NOT_STANDALONE;

            exit;

           end
          else
         else
         { if we didn't read the foreign DTD then this means that there
           is no external subset and we must reset dtd.hasParamEntityRefs }
          dtd.hasParamEntityRefs:=hadParamEntityRefs;

         { end of DTD - no need to update dtd.keepProcessing }

        end;

      end;

    {$ENDIF }
     parser.m_processor:=@contentProcessor;
     result:=contentProcessor(parser ,s ,end_ ,nextPtr );
     exit;

    end;

   XML_ROLE_ATTLIST_ELEMENT_NAME :
    begin
     parser.m_declElementType:=getElementType(parser ,enc ,s ,next );

     if parser.m_declElementType = NIL then
      begin
       result:=XML_ERROR_NO_MEMORY;

       exit;

      end;

     goto checkAttListDeclHandler;

    end;

   XML_ROLE_ATTRIBUTE_NAME :
    begin
     parser.m_declAttributeId:=getAttributeId(parser ,enc ,s ,next );

     if parser.m_declAttributeId = NIL then
      begin
       result:=XML_ERROR_NO_MEMORY;

       exit;

      end;

     parser.m_declAttributeIsCdata:=XML_FALSE;
     parser.m_declAttributeType   :=NIL;
     parser.m_declAttributeIsId   :=XML_FALSE;

     goto checkAttListDeclHandler;

    end;

   XML_ROLE_ATTRIBUTE_TYPE_CDATA :
    begin
     parser.m_declAttributeIsCdata:=XML_TRUE;
     parser.m_declAttributeType   :=@atypeCDATA[0 ];

     goto checkAttListDeclHandler;

    end;

   XML_ROLE_ATTRIBUTE_TYPE_ID :
    begin
     parser.m_declAttributeIsId:=XML_TRUE;
     parser.m_declAttributeType:=@atypeID[0 ];

     goto checkAttListDeclHandler;

    end;

   XML_ROLE_ATTRIBUTE_TYPE_IDREF :
    begin
     parser.m_declAttributeType:=@atypeIDREF[0 ];

     goto checkAttListDeclHandler;

    end;

   XML_ROLE_ATTRIBUTE_TYPE_IDREFS :
    begin
     parser.m_declAttributeType:=@atypeIDREFS[0 ];

     goto checkAttListDeclHandler;

    end;

   XML_ROLE_ATTRIBUTE_TYPE_ENTITY :
    begin
     parser.m_declAttributeType:=@atypeENTITY[0 ];

     goto checkAttListDeclHandler;

    end;

   XML_ROLE_ATTRIBUTE_TYPE_ENTITIES :
    begin
     parser.m_declAttributeType:=@atypeENTITIES[0 ];

     goto checkAttListDeclHandler;

    end;

   XML_ROLE_ATTRIBUTE_TYPE_NMTOKEN :
    begin
     parser.m_declAttributeType:=@atypeNMTOKEN[0 ];

     goto checkAttListDeclHandler;

    end;

   XML_ROLE_ATTRIBUTE_TYPE_NMTOKENS :
    begin
     parser.m_declAttributeType:=@atypeNMTOKENS[0 ];

    checkAttListDeclHandler:
     if (dtd.keepProcessing <> 0 ) and
        (@parser.m_attlistDeclHandler <> NIL ) then
      handleDefault:=XML_FALSE;

    end;

   XML_ROLE_ATTRIBUTE_ENUM_VALUE ,XML_ROLE_ATTRIBUTE_NOTATION_VALUE :
    if (dtd.keepProcessing <> 0 ) and
       (@parser.m_attlistDeclHandler <> NIL ) then
     begin
      if parser.m_declAttributeType <> NIL  then
       prefix:=@enumValueSep[0 ]

      else
       if role = XML_ROLE_ATTRIBUTE_NOTATION_VALUE then
        prefix:=@notationPrefix[0 ]
       else
        prefix:=@enumValueStart[0 ];

      if poolAppendString(@parser.m_tempPool ,prefix ) = NIL then
       begin
        result:=XML_ERROR_NO_MEMORY;

        exit;

       end;

      if poolAppend(@parser.m_tempPool ,enc ,s ,next ) = NIL then
       begin
        result:=XML_ERROR_NO_MEMORY;

        exit;

       end;

      parser.m_declAttributeType:=parser.m_tempPool.start;

      handleDefault:=XML_FALSE;

     end;

   XML_ROLE_IMPLIED_ATTRIBUTE_VALUE ,XML_ROLE_REQUIRED_ATTRIBUTE_VALUE :
    if dtd.keepProcessing <> 0 then
     begin
      if defineAttribute(
          parser.m_declElementType ,parser.m_declAttributeId ,
          parser.m_declAttributeIsCdata ,parser.m_declAttributeIsId ,
          nil ,parser ) = 0 then
       begin
        result:=XML_ERROR_NO_MEMORY;

        exit;

       end;

      if (@parser.m_attlistDeclHandler <> NIL ) and
         (parser.m_declAttributeType <> NIL ) then
       begin
        if (parser.m_declAttributeType^ = XML_T('(' ) ) or
           ((parser.m_declAttributeType^ = XML_T('N' ) ) and
            (XML_Char_ptr(ptrcomp(parser.m_declAttributeType ) + 1 )^ = XML_T('O' ) ) ) then
         begin
         { Enumerated or Notation type }
          if (poolAppendChar(@parser.m_tempPool ,XML_T(')' ) ) = 0 ) or
             (poolAppendChar(@parser.m_tempPool ,XML_T(#0 ) ) = 0 ) then
           begin
            result:=XML_ERROR_NO_MEMORY;

            exit;

           end;

          parser.m_declAttributeType:=parser.m_tempPool.start;

          poolFinish(@parser.m_tempPool );

         end;

        eventEndPP^:=s;

        parser.m_attlistDeclHandler(
         parser.m_handlerArg ,parser.m_declElementType.name ,
         parser.m_declAttributeId.name ,parser.m_declAttributeType ,
         nil ,int(role = XML_ROLE_REQUIRED_ATTRIBUTE_VALUE ) );

        poolClear(@parser.m_tempPool );

        handleDefault:=XML_FALSE;

       end;

     end;

   XML_ROLE_DEFAULT_ATTRIBUTE_VALUE ,XML_ROLE_FIXED_ATTRIBUTE_VALUE :
    if dtd.keepProcessing <> 0 then
     begin
      result_:=
       storeAttributeValue(
        parser ,enc ,parser.m_declAttributeIsCdata ,
        char_ptr(ptrcomp(s ) + enc.minBytesPerChar ) ,
        char_ptr(ptrcomp(next ) - enc.minBytesPerChar ) ,
        @dtd.pool );

      if result_ <> XML_Error(0 ) then
       begin
        result:=result_;

        exit;

       end;

      attVal:=poolStart(@dtd.pool );

      poolFinish(@dtd.pool );

     { ID attributes aren't allowed to have a default }
      if defineAttribute(
          parser.m_declElementType ,parser.m_declAttributeId ,
          parser.m_declAttributeIsCdata ,XML_FALSE ,attVal ,parser ) = 0 then
       begin
        result:=XML_ERROR_NO_MEMORY;

        exit;

       end;

      if (@parser.m_attlistDeclHandler <> NIL ) and
         (parser.m_declAttributeType <> NIL ) then
       begin
        if (parser.m_declAttributeType^ = XML_T('(' ) ) or
           ((parser.m_declAttributeType^ = XML_T('N' ) ) and
            (XML_Char_ptr(ptrcomp(parser.m_declAttributeType ) + 1 )^ = XML_T('O' ) ) ) then
         begin
         { Enumerated or Notation type }
          if (poolAppendChar(@parser.m_tempPool ,XML_T(')' ) ) = 0 ) or
             (poolAppendChar(@parser.m_tempPool ,XML_T(#0 ) ) = 0 ) then
           begin
            result:=XML_ERROR_NO_MEMORY;

            exit;

           end;

          parser.m_declAttributeType:=parser.m_tempPool.start;

          poolFinish(@parser.m_tempPool );

         end;

        eventEndPP^:=s;

        parser.m_attlistDeclHandler(
         parser.m_handlerArg ,parser.m_declElementType.name ,
         parser.m_declAttributeId.name ,parser.m_declAttributeType ,
         attVal ,int(role = XML_ROLE_FIXED_ATTRIBUTE_VALUE ) );

        poolClear(@parser.m_tempPool );

        handleDefault:=XML_FALSE;

       end;

     end;

   XML_ROLE_ENTITY_VALUE :
    if dtd.keepProcessing <> 0 then
     begin
      result_:=
       storeEntityValue(
        parser ,enc ,
        char_ptr(ptrcomp(s ) + enc.minBytesPerChar ) ,
        char_ptr(ptrcomp(next ) - enc.minBytesPerChar ) );

      if parser.m_declEntity <> NIL then
       begin
        parser.m_declEntity.textPtr:=poolStart(@dtd.entityValuePool );
        parser.m_declEntity.textLen:=poolLength(@dtd.entityValuePool );

        poolFinish(@dtd.entityValuePool );

        if @parser.m_entityDeclHandler <> NIL then
         begin
          eventEndPP^:=s;

          parser.m_entityDeclHandler(
           parser.m_handlerArg ,
           parser.m_declEntity.name,
           parser.m_declEntity.is_param,
           parser.m_declEntity.textPtr,
           parser.m_declEntity.textLen,
           parser.m_curBase ,nil ,nil ,nil );

          handleDefault:=XML_FALSE;

         end;

       end
      else
       poolDiscard(@dtd.entityValuePool );

      if result_ <> XML_ERROR_NONE then
       begin
        result:=result_;

        exit;

       end;

     end;

   XML_ROLE_DOCTYPE_SYSTEM_ID :
    begin
    {$IFDEF XML_DTD }
     parser.m_useForeignDTD:=XML_FALSE;

    {$ENDIF }

     dtd.hasParamEntityRefs:=XML_TRUE;

     if @parser.m_startDoctypeDeclHandler <> NIL then
      begin
       parser.m_doctypeSysid:=
        poolStoreString(
         @parser.m_tempPool ,enc ,
         char_ptr(ptrcomp(s ) + enc.minBytesPerChar ) ,
         char_ptr(ptrcomp(next ) - enc.minBytesPerChar ) );

       if parser.m_doctypeSysid = NIL then
        begin
         result:=XML_ERROR_NO_MEMORY;

         exit;

        end;

       poolFinish(@parser.m_tempPool );

       handleDefault:=XML_FALSE;

      end
    {$IFDEF XML_DTD }
     else
     { use externalSubsetName to make doctypeSysid non-NULL
       for the case where no startDoctypeDeclHandler is set }
      parser.m_doctypeSysid:=@externalSubsetName[0 ];

    {$ELSE }; {$ENDIF }

     if (dtd.standalone = 0 ) and
       {$IFDEF XML_DTD }
        (parser.m_paramEntityParsing = XML_ParamEntityParsing(0 ) ) and
       {$ENDIF }
        (@parser.m_notStandaloneHandler <> NIL ) and
        (parser.m_notStandaloneHandler(parser.m_handlerArg ) = 0 ) then
      begin
       result:=XML_ERROR_NOT_STANDALONE;

       exit;

      end;

    {$IFNDEF XML_DTD }
    {$ELSE }
     if parser.m_declEntity = NIL then
      begin
       parser.m_declEntity:=
        ENTITY_ptr(
         lookup(
          @dtd.paramEntities ,@externalSubsetName[0 ] ,sizeof(expat.ENTITY ) ) );

       if parser.m_declEntity = NIL then
        begin
         result:=XML_ERROR_NO_MEMORY;

         exit;

        end;

       parser.m_declEntity.publicId:=NIL;

      end;

    {$ENDIF }
    { fall through }
     goto _go1;

    end;

   XML_ROLE_ENTITY_SYSTEM_ID :
   _go1:
    if (dtd.keepProcessing <> 0 ) and
       (parser.m_declEntity <> NIL ) then
     begin
      parser.m_declEntity.systemId:=
       poolStoreString(
        @dtd.pool ,enc ,
        char_ptr(ptrcomp(s ) + enc.minBytesPerChar ) ,
        char_ptr(ptrcomp(next ) - enc.minBytesPerChar ) );

      if parser.m_declEntity.systemId = NIL then
       begin
        result:=XML_ERROR_NO_MEMORY;

        exit;

       end;

      parser.m_declEntity.base:=parser.m_curBase;

      poolFinish(@dtd.pool );

      if @parser.m_entityDeclHandler <> NIL then
       handleDefault:=XML_FALSE;

     end;

   XML_ROLE_ENTITY_COMPLETE :
    if (dtd.keepProcessing <> 0 ) and
       (parser.m_declEntity <> NIL ) and
       (@parser.m_entityDeclHandler <> NIL ) then
     begin
      eventEndPP^:=s;

      parser.m_entityDeclHandler(
       parser.m_handlerArg ,
       parser.m_declEntity.name ,
       parser.m_declEntity.is_param ,
       nil ,0 ,
       parser.m_declEntity.base ,
       parser.m_declEntity.systemId ,
       parser.m_declEntity.publicId ,
       nil );

      handleDefault:=XML_FALSE;

     end;

   XML_ROLE_ENTITY_NOTATION_NAME :
    if (dtd.keepProcessing <> 0 ) and
       (parser.m_declEntity <> NIL ) then
     begin
      parser.m_declEntity.notation:=poolStoreString(@dtd.pool ,enc ,s ,next );

      if parser.m_declEntity.notation = NIL then
       begin
        result:=XML_ERROR_NO_MEMORY;

        exit;

       end;

      poolFinish(@dtd.pool );

      if @parser.m_unparsedEntityDeclHandler <> NIL then
       begin
        eventEndPP^:=s;

        parser.m_unparsedEntityDeclHandler(
         parser.m_handlerArg ,
         parser.m_declEntity.name ,
         parser.m_declEntity.base ,
         parser.m_declEntity.systemId ,
         parser.m_declEntity.publicId ,
         parser.m_declEntity.notation );

        handleDefault:=XML_FALSE;

       end
      else
       if @parser.m_entityDeclHandler <> NIL then
        begin
         eventEndPP^:=s;

         parser.m_entityDeclHandler(
          parser.m_handlerArg ,
          parser.m_declEntity.name ,
          0 ,nil ,0 ,
          parser.m_declEntity.base,
          parser.m_declEntity.systemId ,
          parser.m_declEntity.publicId ,
          parser.m_declEntity.notation );

         handleDefault:=XML_FALSE;

        end;

     end;

   XML_ROLE_GENERAL_ENTITY_NAME :
    begin
     if XmlPredefinedEntityName(enc ,pointer(s ) ,pointer(next ) ) <> 0 then
      begin
       parser.m_declEntity:=NIL;

       goto _break;
       
      end;

     if dtd.keepProcessing <> 0 then
      begin
       name:=poolStoreString(@dtd.pool ,enc ,s ,next );

       if name = NIL then
        begin
         result:=XML_ERROR_NO_MEMORY;

         exit;

        end;

       parser.m_declEntity:=
        ENTITY_ptr(
         lookup(@dtd.generalEntities ,name ,sizeof(expat.ENTITY ) ) );

       if parser.m_declEntity = NIL then
        begin
         result:=XML_ERROR_NO_MEMORY;

         exit;

        end;

       if parser.m_declEntity.name <> name then
        begin
         poolDiscard(@dtd.pool );

         parser.m_declEntity:=NIL;

        end
       else
        begin
         poolFinish(@dtd.pool );

         parser.m_declEntity.publicId:=NIL;
         parser.m_declEntity.is_param:=XML_FALSE;

        { if we have a parent parser or are reading an internal parameter
          entity, then the entity declaration is not considered "internal" }
         parser.m_declEntity.is_internal:=
          XML_Bool(
           not ((parser.m_parentParser <> NIL ) or
                (parser.m_openInternalEntities <> NIL ) ) );

         if @parser.m_entityDeclHandler <> NIL then
          handleDefault:=XML_FALSE;

        end;

      end
     else
      begin
       poolDiscard(@dtd.pool );

       parser.m_declEntity:=NIL;

      end;

    end;

   XML_ROLE_PARAM_ENTITY_NAME :
   {$IFDEF XML_DTD }
    if dtd.keepProcessing <> 0 then
     begin
      name:=poolStoreString(@dtd.pool ,enc ,s ,next );

      if name <> NIL then
       begin
        result:=XML_ERROR_NO_MEMORY;

        exit;

       end;

      parser.m_declEntity:=
       ENTITY_ptr(
        lookup(@dtd.paramEntities ,name ,sizeof(expat.ENTITY ) ) );

      if parser.m_declEntity = NIL then
       begin
        result:=XML_ERROR_NO_MEMORY;

        exit;

       end;

      if parser.m_declEntity.name <> name then
       begin
        poolDiscard(@dtd.pool );

        parser.m_declEntity:=NIL;

       end
      else
       begin
        poolFinish(@dtd.pool );

        parser.m_declEntity.publicId:=NIL;
        parser.m_declEntity.is_param:=XML_TRUE;

       { if we have a parent parser or are reading an internal parameter
         entity, then the entity declaration is not considered "internal" }
        parser.m_declEntity.is_internal:=
         XML_Bool(
          not((parser.m_parentParser <> NIL ) or
              (parser.m_openInternalEntities <> NIL ) ) );

        if @parser.m_entityDeclHandler <> NIL then
         handleDefault:=XML_FALSE;

       end;

     end
    else
     begin
      poolDiscard(@dtd.pool );

      parser.m_declEntity:=NIL;

     end;

   {$ELSE }
    parser.m_declEntity:=NIL;

   {$ENDIF }

   XML_ROLE_NOTATION_NAME :
    begin
     parser.m_declNotationPublicId:=NIL;
     parser.m_declNotationName    :=NIL;

     if @parser.m_notationDeclHandler <> NIL then
      begin
       parser.m_declNotationName:=poolStoreString(@parser.m_tempPool ,enc ,s ,next );

       if parser.m_declNotationName = NIL then
        begin
         result:=XML_ERROR_NO_MEMORY;

         exit;

        end;

       poolFinish(@parser.m_tempPool );

       handleDefault:=XML_FALSE;

      end;

    end;

   XML_ROLE_NOTATION_PUBLIC_ID :
    begin
     if XmlIsPublicId(enc ,pointer(s ) ,pointer(next ) ,pointer(eventPP ) ) = 0 then
      begin
       result:=XML_ERROR_PUBLICID;

       exit;

      end;

     if parser.m_declNotationName <> NIL then { means notationDeclHandler <> NIL }
      begin
       tem:=
        poolStoreString(
         @parser.m_tempPool ,
         enc ,
         char_ptr(ptrcomp(s ) + enc.minBytesPerChar ) ,
         char_ptr(ptrcomp(next ) - enc.minBytesPerChar ) );

       if tem = NIL then
        begin
         result:=XML_ERROR_NO_MEMORY;

         exit;

        end;

       normalizePublicId(tem );

       parser.m_declNotationPublicId:=tem;

       poolFinish(@parser.m_tempPool );

       handleDefault:=XML_FALSE;

      end;

    end;

   XML_ROLE_NOTATION_SYSTEM_ID :
    begin
     if (parser.m_declNotationName <> NIL ) and
        (@parser.m_notationDeclHandler <> NIL ) then
      begin
       systemId:=
        poolStoreString(
         @parser.m_tempPool ,enc ,
         char_ptr(ptrcomp(s ) + enc.minBytesPerChar ) ,
         char_ptr(ptrcomp(next ) - enc.minBytesPerChar ) );

       if systemId = NIL then
        begin
         result:=XML_ERROR_NO_MEMORY;

         exit;

        end;

       eventEndPP^:=s;

       parser.m_notationDeclHandler(
        parser.m_handlerArg ,
        parser.m_declNotationName ,
        parser.m_curBase ,
        systemId ,
        parser.m_declNotationPublicId );

       handleDefault:=XML_FALSE;

      end;

     poolClear(@parser.m_tempPool );

    end;

   XML_ROLE_NOTATION_NO_SYSTEM_ID :
    begin
     if (parser.m_declNotationPublicId <> NIL ) and
        (@parser.m_notationDeclHandler <> NIL ) then
      begin
       eventEndPP^:=s;

       parser.m_notationDeclHandler(
        parser.m_handlerArg ,
        parser.m_declNotationName ,
        parser.m_curBase ,
        nil ,
        parser.m_declNotationPublicId );

       handleDefault:=XML_FALSE;

      end;

     poolClear(@parser.m_tempPool );

    end;

   XML_ROLE_ERROR :
    case tok of
     XML_TOK_PARAM_ENTITY_REF :
     { PE references in internal subset are
       not allowed within declarations. }
      begin
       result:=XML_ERROR_PARAM_ENTITY_REF;

       exit;

      end;

     XML_TOK_XML_DECL :
      begin
       result:=XML_ERROR_MISPLACED_XML_PI;

       exit;

      end;

     else
      begin
       result:=XML_ERROR_SYNTAX;

       exit;

      end;

    end;

  {$IFDEF XML_DTD }
   XML_ROLE_IGNORE_SECT :
    begin
     if @parser.m_defaultHandler <> NIL then
      reportDefault(parser ,enc ,s ,next );

     handleDefault:=XML_FALSE;

     result_:=doIgnoreSection(parser ,enc ,@next ,end_ ,nextPtr ,haveMore );

     if result_ <> XML_ERROR_NONE then
      begin
       result:=result_;

       exit;

      end
     else
      if next = NIL then
       begin
        parser.m_processor:=@ignoreSectionProcessor;

        result:=result_;

        exit;

       end;

    end;

  {$ENDIF }

   XML_ROLE_GROUP_OPEN :
    begin
     if parser.m_prologState.level >= parser.m_groupSize then
      if parser.m_groupSize <> 0 then
       begin
        parser.m_groupSize:=parser.m_groupSize * 2;

        if parser.m_mem.realloc_fcn(
            pointer(parser.m_groupConnector ) ,
            parser.m_groupAlloc ,
            parser.m_groupSize ) then
         parser.m_groupAlloc:=parser.m_groupSize

        else
         begin
          result:=XML_ERROR_NO_MEMORY;

          exit;

         end;

        if dtd.scaffIndex <> NIL then
         if parser.m_mem.realloc_fcn(
             pointer(dtd.scaffIndex ) ,
             dtd.scaffAlloc ,
             parser.m_groupSize * sizeof(int ) ) then
          dtd.scaffAlloc:=parser.m_groupSize * sizeof(int )

         else
          begin
           result:=XML_ERROR_NO_MEMORY;

           exit;

          end;

       end
      else
       begin
        parser.m_groupSize:=32;

        if parser.m_mem.malloc_fcn(
            pointer(parser.m_groupConnector ) ,
            parser.m_groupSize ) then
         parser.m_groupAlloc:=parser.m_groupSize

        else
         begin
          result:=XML_ERROR_NO_MEMORY;

          exit;

         end;

       end;

     char_ptr(ptrcomp(parser.m_groupConnector ) + parser.m_prologState.level )^:=#0;

     if dtd.in_eldecl <> 0 then
      begin
       myindex:=nextScaffoldPart(parser );

       if myindex < 0 then
        begin
         result:=XML_ERROR_NO_MEMORY;

         exit;

        end;

       int_ptr(ptrcomp(dtd.scaffIndex ) + dtd.scaffLevel * sizeof(int ) )^:=myindex;

       inc(dtd.scaffLevel );

       CONTENT_SCAFFOLD_ptr(ptrcomp(dtd.scaffold ) + myindex * sizeof(CONTENT_SCAFFOLD ) )^.type_:=XML_CTYPE_SEQ;

       if @parser.m_elementDeclHandler <> NIL then
        handleDefault:=XML_FALSE;

      end;

    end;

   XML_ROLE_GROUP_SEQUENCE :
    begin
     if char_ptr(ptrcomp(parser.m_groupConnector ) + parser.m_prologState.level )^ = '|' then
      begin
       result:=XML_ERROR_SYNTAX;

       exit;

      end;

     char_ptr(ptrcomp(parser.m_groupConnector ) + parser.m_prologState.level )^:=',';

     if (dtd.in_eldecl <> 0 ) and
        (@parser.m_elementDeclHandler <> NIL ) then
      handleDefault:=XML_FALSE;

    end;

   XML_ROLE_GROUP_CHOICE :
    begin
     if char_ptr(ptrcomp(parser.m_groupConnector ) + parser.m_prologState.level )^ = ',' then
      begin
       result:=XML_ERROR_SYNTAX;

       exit;

      end;

     if (dtd.in_eldecl <> 0 ) and
        (char_ptr(ptrcomp(parser.m_groupConnector ) + parser.m_prologState.level )^ <> #0 ) and
        (CONTENT_SCAFFOLD_ptr(
          ptrcomp(dtd.scaffold ) +
          int_ptr(ptrcomp(dtd.scaffIndex ) + (dtd.scaffLevel - 1 ) * sizeof(int ) )^ *
          sizeof(CONTENT_SCAFFOLD ) )^.type_ <> XML_CTYPE_MIXED ) then
      begin
       CONTENT_SCAFFOLD_ptr(
        ptrcomp(dtd.scaffold ) +
        int_ptr(ptrcomp(dtd.scaffIndex ) + (dtd.scaffLevel - 1 ) * sizeof(int ) )^ *
        sizeof(CONTENT_SCAFFOLD ) )^.type_:=XML_CTYPE_CHOICE;

       if @parser.m_elementDeclHandler <> NIL then
        handleDefault:=XML_FALSE;

      end;

     char_ptr(ptrcomp(parser.m_groupConnector ) + parser.m_prologState.level )^:='|';

    end;

   XML_ROLE_PARAM_ENTITY_REF {$IFDEF XML_DTD } ,XML_ROLE_INNER_PARAM_ENTITY_REF : {$ELSE }: {$ENDIF }
    begin
    {$IFDEF XML_DTD }
     dtd.hasParamEntityRefs:=XML_TRUE;

     if parser.m_paramEntityParsing = XML_ParamEntityParsing(0 ) then
      dtd.keepProcessing:=dtd.standalone

     else
      begin
       name:=
        poolStoreString(
         @dtd.pool ,enc ,
         char_ptr(ptrcomp(s ) + enc.minBytesPerChar ) ,
         char_ptr(ptrcomp(next ) - enc.minBytesPerChar ) );

       if name = NIL then
        begin
         result:=XML_ERROR_NO_MEMORY;

         exit;

        end;

       entity:=ENTITY_ptr(lookup(@dtd.paramEntities ,name ,0 ) );

       poolDiscard(@dtd.pool );

      { first, determine if a check for an existing declaration is needed;
        if yes, check that the entity exists, and that it is internal,
        otherwise call the skipped entity handler }
       if dtd.standalone <> 0 then
        ok:=XML_Bool(parser.m_openInternalEntities = NIL )
       else
        ok:=XML_Bool(dtd.hasParamEntityRefs = 0 );

       if (parser.m_prologState.documentEntity <> 0 ) and
          (ok <> 0 ) then
        if entity = NIL then
         begin
          result:=XML_ERROR_UNDEFINED_ENTITY;

          exit;

         end
        else
         if entity.is_internal = 0 then
          begin
           result:=XML_ERROR_ENTITY_DECLARED_IN_PE;

           exit;

          end
         else
       else
        if entity = NIL then
         begin
          dtd.keepProcessing:=dtd.standalone;

         { cannot report skipped entities in declarations }
          if (role = XML_ROLE_PARAM_ENTITY_REF ) and
             (@parser.m_skippedEntityHandler <> NIL ) then
           begin
            parser.m_skippedEntityHandler(parser.m_handlerArg ,name ,1 );

            handleDefault:=XML_FALSE;

           end;

          goto _break;

         end;

       if entity.open <> 0 then
        begin
         result:=XML_ERROR_RECURSIVE_ENTITY_REF;

         exit;

        end;

       if entity.textPtr <> NIL then
        begin
         if role = XML_ROLE_PARAM_ENTITY_REF then
          betweenDecl:=XML_TRUE
         else
          betweenDecl:=XML_FALSE;

         result_:=processInternalEntity(parser ,entity ,betweenDecl );

         if result_ <> XML_ERROR_NONE then
          begin
           result:=result_;

           exit;

          end;

         handleDefault:=XML_FALSE;

         goto _break;

        end;

       if @parser.m_externalEntityRefHandler <> NIL then
        begin
         dtd.paramEntityRead:=XML_FALSE;
         entity.open        :=XML_TRUE;

         if parser.m_externalEntityRefHandler(
             parser.m_externalEntityRefHandlerArg ,
             nil ,
             entity.base ,
             entity.systemId ,
             entity.publicId ) = 0 then
          begin
           entity.open:=XML_FALSE;

           result:=XML_ERROR_EXTERNAL_ENTITY_HANDLING;

           exit;

          end;

         entity.open  :=XML_FALSE;
         handleDefault:=XML_FALSE;

         if dtd.paramEntityRead = 0 then
          begin
           dtd.keepProcessing:=dtd.standalone;

           goto _break;

          end;

        end
       else
        begin
         dtd.keepProcessing:=dtd.standalone;

         goto _break;

        end;

      end;

    {$ENDIF }

     if (dtd.standalone = 0 ) and
        (@parser.m_notStandaloneHandler <> NIL ) and
        (parser.m_notStandaloneHandler(parser.m_handlerArg ) = 0 ) then
      begin
       result:=XML_ERROR_NOT_STANDALONE;

       exit;

      end;

    end;

  { Element declaration stuff }
   XML_ROLE_ELEMENT_NAME :
    if @parser.m_elementDeclHandler <> NIL then
     begin
      parser.m_declElementType:=getElementType(parser ,enc ,s ,next );

      if parser.m_declElementType = NIL then
       begin
        result:=XML_ERROR_NO_MEMORY;

        exit;

       end;

      dtd.scaffLevel:=0;
      dtd.scaffCount:=0;
      dtd.in_eldecl :=XML_TRUE;
      handleDefault :=XML_FALSE;

     end;

   XML_ROLE_CONTENT_ANY ,XML_ROLE_CONTENT_EMPTY :
    if dtd.in_eldecl <> 0 then
     begin
      if @parser.m_elementDeclHandler <> NIL then
       begin
        content:=nil;
        parser.m_mem.malloc_fcn(pointer(content ) ,sizeof(XML_Content ) );

        if content = NIL then
         begin
          result:=XML_ERROR_NO_MEMORY;

          exit;

         end;

        content.quant      :=XML_CQUANT_NONE;
        content.name       :=NIL;
        content.numchildren:=0;
        content.children   :=NIL;

        if role = XML_ROLE_CONTENT_ANY then
         content.type_:=XML_CTYPE_ANY
        else
         content.type_:=XML_CTYPE_EMPTY;

        eventEndPP^:=s;

        parser.m_elementDeclHandler(
         parser.m_handlerArg ,parser.m_declElementType.name ,content );

        handleDefault:=XML_FALSE;

       end;

      dtd.in_eldecl:=XML_FALSE;

     end;

   XML_ROLE_CONTENT_PCDATA :
    if dtd.in_eldecl <> 0 then
     begin
      CONTENT_SCAFFOLD_ptr(
       ptrcomp(dtd.scaffold ) +
       int_ptr(ptrcomp(dtd.scaffIndex ) + (dtd.scaffLevel - 1 ) * sizeof(int ) )^ *
       sizeof(CONTENT_SCAFFOLD ) )^.type_:=XML_CTYPE_MIXED;

      if @parser.m_elementDeclHandler <> NIL then
       handleDefault:=XML_FALSE;

     end;

   XML_ROLE_CONTENT_ELEMENT :
    begin
     quant:=XML_CQUANT_NONE;

     goto elementContent;

    end;

   XML_ROLE_CONTENT_ELEMENT_OPT :
    begin
     quant:=XML_CQUANT_OPT;

     goto elementContent;

    end;

   XML_ROLE_CONTENT_ELEMENT_REP :
    begin
     quant:=XML_CQUANT_REP;

     goto elementContent;

    end;

   XML_ROLE_CONTENT_ELEMENT_PLUS :
    begin
     quant:=XML_CQUANT_PLUS;

    elementContent:
     if dtd.in_eldecl <> 0 then
      begin
       if quant = XML_CQUANT_NONE then
        nxt:=next
       else
        nxt:=char_ptr(ptrcomp(next ) - enc.minBytesPerChar );

       myindex:=nextScaffoldPart(parser );

       if myindex < 0 then
        begin
         result:=XML_ERROR_NO_MEMORY;

         exit;

        end;

       CONTENT_SCAFFOLD_ptr(
        ptrcomp(dtd.scaffold ) +
        myindex * sizeof(CONTENT_SCAFFOLD ) )^.type_:=XML_CTYPE_NAME;

       CONTENT_SCAFFOLD_ptr(
        ptrcomp(dtd.scaffold ) +
        myindex * sizeof(CONTENT_SCAFFOLD ) )^.quant:=quant;

       el:=getElementType(parser ,enc ,s ,nxt );

       if el = NIL then
        begin
         result:=XML_ERROR_NO_MEMORY;

         exit;

        end;

       name:=el.name;

       CONTENT_SCAFFOLD_ptr(
        ptrcomp(dtd.scaffold ) +
        myindex * sizeof(CONTENT_SCAFFOLD ) )^.name:=name;

       nameLen:=0;

       while XML_Char_ptr(ptrcomp(name ) + nameLen )^ <> XML_Char(0 ) do
        inc(nameLen );

       inc(dtd.contentStringLen ,nameLen );

       if @parser.m_elementDeclHandler <> NIL then
        handleDefault:=XML_FALSE;

      end;

    end;

   XML_ROLE_GROUP_CLOSE :
    begin
     quant:=XML_CQUANT_NONE;

     goto closeGroup;

    end;

   XML_ROLE_GROUP_CLOSE_OPT :
    begin
     quant:=XML_CQUANT_OPT;

     goto closeGroup;

    end;

   XML_ROLE_GROUP_CLOSE_REP :
    begin
     quant:=XML_CQUANT_REP;

     goto closeGroup;

    end;

   XML_ROLE_GROUP_CLOSE_PLUS :
    begin
     quant:=XML_CQUANT_PLUS;

    closeGroup:
     if dtd.in_eldecl <> 0 then
      begin
       if @parser.m_elementDeclHandler <> NIL then
        handleDefault:=XML_FALSE;

       dec(dtd.scaffLevel );

       CONTENT_SCAFFOLD_ptr(
        ptrcomp(dtd.scaffold ) +
        int_ptr(ptrcomp(dtd.scaffIndex ) + dtd.scaffLevel * sizeof(int ) )^ *
        sizeof(CONTENT_SCAFFOLD) )^.quant:=quant;

       if dtd.scaffLevel = 0 then
        begin
         if handleDefault = 0 then
          begin
           model:=build_model(parser );

           if model = NIL then
            begin
             result:=XML_ERROR_NO_MEMORY;

             exit;

            end;

           eventEndPP^:=s;

           parser.m_elementDeclHandler(
            parser.m_handlerArg ,
            parser.m_declElementType.name ,model );

          end;

         dtd.in_eldecl       :=XML_FALSE;
         dtd.contentStringLen:=0;

        end;

      end;

    end; { End element declaration stuff }

   XML_ROLE_PI :
    begin
     if reportProcessingInstruction(parser ,enc ,s ,next ) = 0 then
      begin
       result:=XML_ERROR_NO_MEMORY;

       exit;

      end;

     handleDefault:=XML_FALSE;

    end;

   XML_ROLE_COMMENT :
    begin
     if reportComment(parser ,enc ,s ,next ) = 0 then
      begin
       result:=XML_ERROR_NO_MEMORY;

       exit;

      end;

     handleDefault:=XML_FALSE;

    end;

   XML_ROLE_NONE :
    case tok of
     XML_TOK_BOM :
      handleDefault:=XML_FALSE;

    end;

   XML_ROLE_DOCTYPE_NONE :
    if @parser.m_startDoctypeDeclHandler <> NIL then
     handleDefault:=XML_FALSE;

   XML_ROLE_ENTITY_NONE :
    if (dtd.keepProcessing <> 0 ) and
       (@parser.m_entityDeclHandler <> NIL ) then
     handleDefault:=XML_FALSE;

   XML_ROLE_NOTATION_NONE :
    if @parser.m_notationDeclHandler <> NIL then
     handleDefault:=XML_FALSE;

   XML_ROLE_ATTLIST_NONE :
    if (dtd.keepProcessing <> 0 ) and
       (@parser.m_attlistDeclHandler <> NIL ) then
     handleDefault:=XML_FALSE;

   XML_ROLE_ELEMENT_NONE :
    if @parser.m_elementDeclHandler <> NIL then
     handleDefault:=XML_FALSE;

  end; { end of big case }

 _break:
  if (handleDefault = XML_TRUE ) and
     (@parser.m_defaultHandler <> NIL ) then
   reportDefault(parser ,enc ,s ,next );

  case parser.m_parsingStatus.parsing of
   XML_SUSPENDED :
    begin
     nextPtr^:=next;
     result  :=XML_ERROR_NONE;

     exit;

    end;

   XML_FINISHED :
    begin
     result:=XML_ERROR_ABORTED;

     exit;

    end;

   else
    begin
     s  :=next;
     tok:=XmlPrologTok(enc ,pointer(s ) ,pointer(end_ ) ,@next );

    end;

  end;

 until false;

{ not reached }

end;

{ prologProcessor }
function prologProcessor(parser : XML_Parser; s ,end_ : char_ptr; nextPtr : char_ptr_ptr ) : XML_Error;
var
 next : char_ptr;
 tok  : int;

begin
 next:=s;
 tok :=XmlPrologTok(parser.m_encoding ,pointer(s ) ,pointer(end_ ) ,@next );
 result:=
  doProlog(
   parser ,parser.m_encoding ,s ,end_ ,tok ,next ,
   nextPtr ,XML_Bool(not parser.m_parsingStatus.finalBuffer ) );
end;

{ prologInitProcessor }
function prologInitProcessor(parser : XML_Parser; s ,end_ : char_ptr; nextPtr : char_ptr_ptr ) : XML_Error;
var
 result_ : XML_Error;

begin
 result_:=initializeEncoding(parser );

 if result_ <> XML_ERROR_NONE then
  begin
   result:=result_;

   exit;

  end;

 parser.m_processor:=@prologProcessor;

 result:=prologProcessor(parser ,s ,end_ ,nextPtr );

end;

{ parserInit }
procedure parserInit(parser : XML_Parser; encodingName : XML_Char_ptr );
begin
 parser.m_processor:=@prologInitProcessor;

 XmlPrologStateInit(@parser.m_prologState );

 if encodingName <> NIL then
  parser.m_protocolEncodingName:=poolCopyString(@parser.m_tempPool ,encodingName )
 else
  parser.m_protocolEncodingName:=NIL;

 parser.m_curBase:=NIL;

 XmlInitEncoding(@parser.m_initEncoding ,@parser.m_encoding ,nil );

 parser.m_userData  :=NIL;
 parser.m_handlerArg:=NIL;

 parser.m_startElementHandler         :=NIL;
 parser.m_endElementHandler           :=NIL;
 parser.m_characterDataHandler        :=NIL;
 parser.m_processingInstructionHandler:=NIL;
 parser.m_commentHandler              :=NIL;
 parser.m_startCdataSectionHandler    :=NIL;
 parser.m_endCdataSectionHandler      :=NIL;
 parser.m_defaultHandler              :=NIL;
 parser.m_startDoctypeDeclHandler     :=NIL;
 parser.m_endDoctypeDeclHandler       :=NIL;
 parser.m_unparsedEntityDeclHandler   :=NIL;
 parser.m_notationDeclHandler         :=NIL;
 parser.m_startNamespaceDeclHandler   :=NIL;
 parser.m_endNamespaceDeclHandler     :=NIL;
 parser.m_notStandaloneHandler        :=NIL;
 parser.m_externalEntityRefHandler    :=NIL;
 parser.m_externalEntityRefHandlerArg :=parser;
 parser.m_skippedEntityHandler        :=NIL;
 parser.m_elementDeclHandler          :=NIL;
 parser.m_attlistDeclHandler          :=NIL;
 parser.m_entityDeclHandler           :=NIL;
 parser.m_xmlDeclHandler              :=NIL;

 parser.m_bufferPtr:=parser.m_buffer;
 parser.m_bufferEnd:=parser.m_buffer;

 parser.m_parseEndByteIndex:=0;
 parser.m_parseEndPtr      :=NIL;

 parser.m_declElementType:=NIL;
 parser.m_declAttributeId:=NIL;
 parser.m_declEntity     :=NIL;

 parser.m_doctypeName :=NIL;
 parser.m_doctypeSysid:=NIL;
 parser.m_doctypePubid:=NIL;

 parser.m_declAttributeType   :=NIL;
 parser.m_declNotationName    :=NIL;
 parser.m_declNotationPublicId:=NIL;
 parser.m_declAttributeIsCdata:=XML_FALSE;
 parser.m_declAttributeIsId   :=XML_FALSE;

 fillchar(parser.m_position ,sizeof(POSITION ) ,0 );

 parser.m_errorCode:=XML_ERROR_NONE;

 parser.m_eventPtr   :=NIL;
 parser.m_eventEndPtr:=NIL;
 parser.m_positionPtr:=NIL;

 parser.m_openInternalEntities         :=NIL;
 parser.m_defaultExpandInternalEntities:=XML_TRUE;

 parser.m_tagLevel         :=0;
 parser.m_tagStack         :=NIL;
 parser.m_inheritedBindings:=NIL;
 parser.m_nSpecifiedAtts   :=0;

 parser.m_unknownEncodingMem    :=NIL;
 parser.m_unknownEncodingRelease:=NIL;
 parser.m_unknownEncodingData   :=NIL;
 parser.m_unknownEncodingAlloc  :=0;

 parser.m_parentParser         :=NIL;
 parser.m_parsingStatus.parsing:=XML_INITIALIZED;

{$IFDEF XML_DTD }
 parser.m_isParamEntity:=XML_FALSE;
 parser.m_useForeignDTD:=XML_FALSE;

 parser.m_paramEntityParsing:=XML_PARAM_ENTITY_PARSING_NEVER;

{$ENDIF }

end;

{ parserCreate }
function parserCreate(
          encodingName : XML_Char_ptr;
          memsuite : XML_Memory_Handling_Suite_ptr;
          nameSep : XML_Char_ptr;
          dtd : DTD_ptr ) : XML_Parser;
var
 parser : XML_Parser;
 mtemp  : XML_Memory_Handling_Suite_ptr;

begin
 parser:=NIL;

 if memsuite <> NIL then
  begin
   memsuite.malloc_fcn(pointer(parser ) ,sizeof(XML_ParserStruct ) );

   if parser <> NIL then
    begin
     mtemp:=@parser.m_mem;

     mtemp.malloc_fcn :=memsuite.malloc_fcn;
     mtemp.realloc_fcn:=memsuite.realloc_fcn;
     mtemp.free_fcn   :=memsuite.free_fcn;

    end;

  end
 else
  begin
   expat_getmem(pointer(parser ) ,sizeof(XML_ParserStruct ) );

   if parser <> NIL then
    begin
     mtemp:=@parser.m_mem;

     mtemp.malloc_fcn :=@expat_getmem;
     mtemp.realloc_fcn:=@expat_realloc;
     mtemp.free_fcn   :=@expat_freemem;

    end;

  end;

 if parser = NIL then
  begin
   result:=NIL;

   exit;

  end;

 parser.m_buffer   :=NIL;
 parser.m_bufferLim:=NIL;
 parser.m_attsSize :=INIT_ATTS_SIZE;

 parser.m_attsAlloc  :=0;
 parser.m_nsAttsAlloc:=0;

 parser.m_mem.malloc_fcn(pointer(parser.m_atts ) ,parser.m_attsSize * sizeof(ATTRIBUTE ) );

 if parser.m_atts = NIL then
  begin
   parser.m_mem.free_fcn(pointer(parser ) ,sizeof(XML_ParserStruct ) );

   result:=NIL;

   exit;

  end
 else
  parser.m_attsAlloc:=parser.m_attsSize * sizeof(ATTRIBUTE );

 parser.m_mem.malloc_fcn(pointer(parser.m_dataBuf ) ,INIT_DATA_BUF_SIZE * sizeof(XML_Char ) );

 if parser.m_dataBuf = NIL then
  begin
   parser.m_mem.free_fcn(pointer(parser.m_atts ) ,parser.m_attsAlloc );
   parser.m_mem.free_fcn(pointer(parser ) ,sizeof(XML_ParserStruct ) );

   result:=NIL;

   exit;

  end;

 parser.m_dataBufEnd:=XML_Char_ptr(ptrcomp(parser.m_dataBuf ) + INIT_DATA_BUF_SIZE );

 if dtd <> NIL then
  parser.m_dtd:=dtd
 else
  begin
   parser.m_dtd:=dtdCreate(@parser.m_mem );

   if parser.m_dtd = NIL then
    begin
     parser.m_mem.free_fcn(pointer(parser.m_dataBuf ) ,INIT_DATA_BUF_SIZE * sizeof(XML_Char ) );
     parser.m_mem.free_fcn(pointer(parser.m_atts ) ,parser.m_attsAlloc );
     parser.m_mem.free_fcn(pointer(parser ) ,sizeof(XML_ParserStruct ) );

     result:=NIL;

     exit;

    end;

  end;

 parser.m_freeBindingList     :=NIL;
 parser.m_freeTagList         :=NIL;
 parser.m_freeInternalEntities:=NIL;

 parser.m_groupSize     :=0;
 parser.m_groupAlloc    :=0;
 parser.m_groupConnector:=NIL;

 parser.m_unknownEncodingHandler    :=NIL;
 parser.m_unknownEncodingHandlerData:=NIL;

 parser.m_namespaceSeparator:='!';

 parser.m_ns         :=XML_FALSE;
 parser.m_ns_triplets:=XML_FALSE;

 parser.m_nsAtts       :=NIL;
 parser.m_nsAttsVersion:=0;
 parser.m_nsAttsPower  :=0;

 poolInit  (@parser.m_tempPool ,@parser.m_mem );
 poolInit  (@parser.m_temp2Pool ,@parser.m_mem );
 parserInit(parser ,encodingName );

 if (encodingName <> NIL ) and
    (parser.m_protocolEncodingName = NIL ) then
  begin
   XML_ParserFree(parser );

   result:=NIL;

   exit;

  end;

 if nameSep <> NIL then
  begin
   parser.m_ns:=XML_TRUE;

   parser.m_internalEncoding  :=XmlGetInternalEncodingNS;
   parser.m_namespaceSeparator:=nameSep^;

  end
 else
  parser.m_internalEncoding:=XmlGetInternalEncoding;

 result:=parser;

end;

{ setContext {..}
function setContext(parser : XML_Parser; context : XML_Char_ptr ) : XML_Bool;
begin
 Result:=0;
end;

{ XML_PARSERCREATE }
function XML_ParserCreate;
begin
 result:=XML_ParserCreate_MM(encoding ,NIL ,NIL );

end;

{ XML_PARSER_CREATE_MM }
function XML_ParserCreate_MM;
var
 parser : XML_Parser;

begin
 parser:=parserCreate(encoding ,memsuite ,namespaceSeparator ,NIL );

 if (parser <> NIL ) and
    (parser.m_ns <> 0 ) then
 { implicit context only set for root parser, since child
   parsers (i.e. external entity parsers) will inherit it }
  if not setContext(parser ,@implicitContext[0 ] ) <> 0 then
   begin
    XML_ParserFree(parser );

    result:=NIL;

    exit;

   end;

 result:=parser;

end;

{ XML_SETUSERDATA }
procedure XML_SetUserData;
begin
 if parser.m_handlerArg = parser.m_userData then
  begin
   parser.m_handlerArg:=userData;
   parser.m_userData  :=userData;

  end
 else
  parser.m_userData:=userData;

end;

{ XML_SETELEMENTHANDLER }
procedure XML_SetElementHandler;
begin
 parser.m_startElementHandler:=start;
 parser.m_endElementHandler  :=end_;

end;

{ XML_SETCHARACTERDATAHANDLER }
procedure XML_SetCharacterDataHandler;
begin
 parser.m_characterDataHandler:=handler;

end;

{ XML_GetBuffer }
function XML_GetBuffer(parser : XML_Parser; len : int ) : pointer;
var
 neededSize ,keep ,offset ,bufferSize : int;

 newBuf : char_ptr;

begin
 case parser.m_parsingStatus.parsing of
  XML_SUSPENDED :
   begin
    parser.m_errorCode:=XML_ERROR_SUSPENDED;

    result:=NIL;

    exit;

   end;

  XML_FINISHED :
   begin
    parser.m_errorCode:=XML_ERROR_FINISHED;

    result:=NIL;

    exit;

   end;

 end;

 if len > ptrcomp(parser.m_bufferLim ) - ptrcomp(parser.m_bufferEnd ) then
  begin
  { FIXME avoid integer overflow }
   neededSize:=len + (ptrcomp(parser.m_bufferEnd ) - ptrcomp(parser.m_bufferPtr ) );

  {$IFDEF XML_CONTEXT_BYTES }
   keep:=ptrcomp(parser.m_bufferPtr ) - ptrcomp(parser.m_buffer );

   if keep > _XML_CONTEXT_BYTES then
    keep:=_XML_CONTEXT_BYTES;

   inc(neededSize ,keep );

  {$ENDIF }

   if neededSize <= ptrcomp(parser.m_bufferLim ) - ptrcomp(parser.m_buffer ) then
    begin
    {$IFDEF XML_CONTEXT_BYTES }
     if keep < ptrcomp(parser.m_bufferPtr ) - ptrcomp(parser.m_buffer ) then
      begin
       offset:=ptrcomp(parser.m_bufferPtr ) - ptrcomp(parser.m_buffer ) - keep;

       move(
        char_ptr(ptrcomp(parser.m_buffer ) + offset )^ ,
        parser.m_buffer^ ,
        ptrcomp(parser.m_bufferEnd ) - ptrcomp(parser.m_bufferPtr ) + keep );

       dec(ptrcomp(parser.m_bufferEnd ) ,offset );
       dec(ptrcomp(parser.m_bufferPtr ) ,offset );

      end;

    {$ELSE }
     move(
      parser.m_bufferPtr^ ,
      parser.m_buffer^ ,
      ptrcomp(parser.m_bufferEnd ) - ptrcomp(parser.m_bufferPtr ) );

     parser.m_bufferEnd:=char_ptr(ptrcomp(parser.m_buffer ) + (ptrcomp(parser.m_bufferEnd ) - ptrcomp(parser.m_bufferPtr ) ) );
     parser.m_bufferPtr:=parser.m_buffer;

    {$ENDIF }

    end
   else
    begin
     bufferSize:=ptrcomp(parser.m_bufferLim ) - ptrcomp(parser.m_bufferPtr );

     if bufferSize = 0 then
      bufferSize:=INIT_BUFFER_SIZE;

     repeat
      bufferSize:=bufferSize * 2;

     until bufferSize >= neededSize;

     newBuf:=nil;
     parser.m_mem.malloc_fcn(pointer(newBuf ) ,bufferSize );

     if newBuf = NIL then
      begin
       parser.m_errorCode:=XML_ERROR_NO_MEMORY;

       result:=NIL;

       exit;

      end;

     parser.m_bufferLim :=char_ptr(ptrcomp(newBuf ) + bufferSize );

    {$IFDEF XML_CONTEXT_BYTES }
     if parser.m_bufferPtr <> NIL then
      begin
       keep:=ptrcomp(parser.m_bufferPtr ) - ptrcomp(parser.m_buffer );

       if keep > _XML_CONTEXT_BYTES then
        keep:=_XML_CONTEXT_BYTES;

       move(
        char_ptr(ptrcomp(parser.m_bufferPtr ) - keep )^ ,
        newBuf^ ,
        ptrcomp(parser.m_bufferEnd ) - ptrcomp(parser.m_bufferPtr ) + keep );

       expat_freemem(pointer(parser.m_buffer ) ,parser.m_bufferAloc );

       parser.m_buffer    :=newBuf;
       parser.m_bufferAloc:=bufferSize;

       parser.m_bufferEnd:=
        char_ptr(
         ptrcomp(parser.m_buffer ) +
         (ptrcomp(parser.m_bufferEnd ) - ptrcomp(parser.m_bufferPtr ) ) +
         keep );

       parser.m_bufferPtr:=char_ptr(ptrcomp(parser.m_buffer ) + keep );

      end
     else
      begin
       parser.m_bufferEnd :=char_ptr(ptrcomp(newBuf ) + (ptrcomp(parser.m_bufferEnd ) - ptrcomp(parser.m_bufferPtr ) ) );
       parser.m_buffer    :=newBuf;
       parser.m_bufferPtr :=newBuf;
       parser.m_bufferAloc:=bufferSize;
       
      end;

    {$ELSE }
     if parser.m_bufferPtr <> NIL then
      begin
       move(
        parser.m_bufferPtr^ ,
        newBuf^ ,
        ptrcomp(parser.m_bufferEnd ) - ptrcomp(parser.m_bufferPtr ) );

       expat_freemem(pointer(parser.m_buffer ) ,parser.m_bufferAloc );

      end;

     parser.m_bufferEnd :=char_ptr(ptrcomp(newBuf ) + (ptrcomp(parser.m_bufferEnd ) - ptrcomp(parser.m_bufferPtr ) ) );
     parser.m_buffer    :=newBuf;
     parser.m_bufferPtr :=newBuf;
     parser.m_bufferAloc:=bufferSize;

    {$ENDIF }

    end;

  end;

 result:=parser.m_bufferEnd;

end;

{ errorProcessor }
function errorProcessor(parser : XML_Parser; s ,end_ : char_ptr; nextPtr : char_ptr_ptr ) : XML_Error;
begin
 result:=parser.m_errorCode;

end;

{ XML_ParseBuffer }
function XML_ParseBuffer(parser : XML_Parser; len ,isFinal : int ) : XML_Status;
var
 start   : char_ptr;
 result_ : XML_Status;

begin
 result_:=XML_STATUS_OK;

 case parser.m_parsingStatus.parsing of
  XML_SUSPENDED :
   begin
    parser.m_errorCode:=XML_ERROR_SUSPENDED;

    result:=XML_STATUS_ERROR;

    exit;

   end;

  XML_FINISHED :
   begin
    parser.m_errorCode:=XML_ERROR_FINISHED;

    result:=XML_STATUS_ERROR;

    exit;

   end;

  else
   parser.m_parsingStatus.parsing:=XML_PARSING_;

 end;

 start               :=parser.m_bufferPtr;
 parser.m_positionPtr:=start;

 inc(ptrcomp(parser.m_bufferEnd ) ,len );

 parser.m_parseEndPtr:=parser.m_bufferEnd;

 inc(ptrcomp(parser.m_parseEndByteIndex ) ,len );

 parser.m_parsingStatus.finalBuffer:=XML_Bool(isFinal );
 parser.m_errorCode:=parser.m_processor(parser ,start ,parser.m_parseEndPtr ,@parser.m_bufferPtr );

 if parser.m_errorCode <> XML_ERROR_NONE then
  begin
   parser.m_eventEndPtr:=parser.m_eventPtr;
   parser.m_processor  :=@errorProcessor;

   result:=XML_STATUS_ERROR;

   exit;

  end
 else
  case parser.m_parsingStatus.parsing of
   XML_SUSPENDED :
    result_:=XML_STATUS_SUSPENDED;

   XML_INITIALIZED ,XML_PARSING_ :
    if isFinal <> 0 then
     begin
      parser.m_parsingStatus.parsing:=XML_FINISHED;

      result:=result_;

      exit;

     end;

   else
   { should not happen }
    NoP;

  end;

 parser.m_encoding.updatePosition(
  parser.m_encoding ,
  pointer(parser.m_positionPtr ) ,
  pointer(parser.m_bufferPtr ) ,@parser.m_position );

 parser.m_positionPtr:=parser.m_bufferPtr;

 result:=result_;

end;

{ XML_PARSE {..}
function XML_Parse;
var
 buff : pointer;

begin
 case parser.m_parsingStatus.parsing of
  XML_SUSPENDED :
   begin
    parser.m_errorCode:=XML_ERROR_SUSPENDED;

    result:=XML_STATUS_ERROR;

    exit;

   end;

  XML_FINISHED :
   begin
    parser.m_errorCode:=XML_ERROR_FINISHED;

    result:=XML_STATUS_ERROR;

    exit;

   end;

  else
   parser.m_parsingStatus.parsing:=XML_PARSING_;

 end;

 if len = 0 then
  begin
   NoP;

  end
{$IFNDEF XML_CONTEXT_BYTES }
 else
  if parser.m_bufferPtr = parser.m_bufferEnd then
   begin
    NoP;

   end

{$ENDIF }
  else
   begin
    buff:=XML_GetBuffer(parser ,len );

    if buff = NIL then
     result:=XML_STATUS_ERROR
     
    else
     begin
      move(s^ ,buff^ ,len );

      result:=XML_ParseBuffer(parser ,len ,isFinal );

     end;
     
   end;

end;

{ XML_GETERRORCODE {..}
function XML_GetErrorCode;
begin
 Result:=XML_ERROR_NO_MEMORY;
end;

{ XML_ERRORSTRING {..}
function XML_ErrorString;
begin
 Result:=nil;
end;

{ XML_GETCURRENTLINENUMBER {..}
function XML_GetCurrentLineNumber;
begin
 Result:=0;
end;

{ destroyBindings }
procedure destroyBindings(bindings : BINDING_ptr; parser : XML_Parser );
var
 b : BINDING_ptr;

begin
 repeat
  b:=bindings;

  if b = NIL then
   break;

  bindings:=b.nextTagBinding;

  parser.m_mem.free_fcn(pointer(b.uri ) ,b.uriAlloc );
  parser.m_mem.free_fcn(pointer(b ) ,sizeof(expat.BINDING ) );

 until false;

end;

{ XML_PARSERFREE }
procedure XML_ParserFree;
var
 tagList ,p : TAG_ptr;

 entityList ,openEntity : OPEN_INTERNAL_ENTITY_ptr;

begin
 if parser = NIL then
  exit;

{ free tagStack and freeTagList }
 tagList:=parser.m_tagStack;

 repeat
  if tagList = NIL then
   begin
    if parser.m_freeTagList = NIL then
     break;

    tagList:=parser.m_freeTagList;

    parser.m_freeTagList:=NIL;

   end;

  p      :=tagList;
  tagList:=tagList.parent;

  parser.m_mem.free_fcn(pointer(p.buf ) ,p.alloc );
  destroyBindings      (p.bindings ,parser );
  parser.m_mem.free_fcn(pointer(p ) ,sizeof(expat.TAG ) );

 until false;

{ free openInternalEntities and freeInternalEntities }
 entityList:=parser.m_openInternalEntities;

 repeat
  if entityList = NIL then
   begin
    if parser.m_freeInternalEntities = NIL then
     break;

    entityList:=parser.m_freeInternalEntities;
    
    parser.m_freeInternalEntities:=NIL;

   end;

  openEntity:=entityList;
  entityList:=entityList.next;

  parser.m_mem.free_fcn(pointer(openEntity ) ,sizeof(OPEN_INTERNAL_ENTITY ) );

 until false;

 destroyBindings(parser.m_freeBindingList ,parser );
 destroyBindings(parser.m_inheritedBindings ,parser );

 poolDestroy(@parser.m_tempPool );
 poolDestroy(@parser.m_temp2Pool );

{$IFDEF XML_DTD }
{ external parameter entity parsers share the DTD structure
  parser->m_dtd with the root parser, so we must not destroy it }
 if (parser.m_isParamEntity = 0 ) and
    (parser.m_dtd <> NIL ) then
{$ELSE }
 if parser.m_dtd <> NIL then{$ENDIF }
  dtdDestroy(parser.m_dtd ,XML_Bool(parser.m_parentParser = NIL ),@parser.m_mem );

 parser.m_mem.free_fcn(pointer(parser.m_atts ) ,parser.m_attsAlloc );
 parser.m_mem.free_fcn(pointer(parser.m_groupConnector ) ,parser.m_groupAlloc );
 parser.m_mem.free_fcn(pointer(parser.m_buffer ) ,parser.m_bufferAloc );
 parser.m_mem.free_fcn(pointer(parser.m_dataBuf ) ,INIT_DATA_BUF_SIZE * sizeof(XML_Char ) );
 parser.m_mem.free_fcn(pointer(parser.m_nsAtts ) ,parser.m_nsAttsAlloc );
 parser.m_mem.free_fcn(pointer(parser.m_unknownEncodingMem ) ,parser.m_unknownEncodingAlloc );

 if @parser.m_unknownEncodingRelease <> NIL then
  parser.m_unknownEncodingRelease(parser.m_unknownEncodingData );

 parser.m_mem.free_fcn(pointer(parser ) ,sizeof(XML_ParserStruct ) );

end;

