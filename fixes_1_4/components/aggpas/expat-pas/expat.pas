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
// 01.05.2006-Milano: Unit port establishment
// 07.06.2006-Milano: porting
//
{ expat.pas }
unit
 expat ;

INTERFACE

uses
 expat_basics ,
 xmltok ,
 xmlrole ;

{$I expat_mode.inc }

{ TYPES DEFINITION }
type
{$I expat_external.inc }

 XML_Parser = ^XML_ParserStruct;

 XML_Bool = int8u;

{ The XML_Status enum gives the possible return values for several API functions. }
 XML_Status = (XML_STATUS_ERROR ,XML_STATUS_OK ,XML_STATUS_SUSPENDED );

 XML_Error = (
  XML_ERROR_NONE ,
  XML_ERROR_NO_MEMORY ,
  XML_ERROR_SYNTAX ,
  XML_ERROR_NO_ELEMENTS ,
  XML_ERROR_INVALID_TOKEN ,
  XML_ERROR_UNCLOSED_TOKEN ,
  XML_ERROR_PARTIAL_CHAR ,
  XML_ERROR_TAG_MISMATCH ,
  XML_ERROR_DUPLICATE_ATTRIBUTE ,
  XML_ERROR_JUNK_AFTER_DOC_ELEMENT ,
  XML_ERROR_PARAM_ENTITY_REF ,
  XML_ERROR_UNDEFINED_ENTITY ,
  XML_ERROR_RECURSIVE_ENTITY_REF ,
  XML_ERROR_ASYNC_ENTITY ,
  XML_ERROR_BAD_CHAR_REF ,
  XML_ERROR_BINARY_ENTITY_REF ,
  XML_ERROR_ATTRIBUTE_EXTERNAL_ENTITY_REF ,
  XML_ERROR_MISPLACED_XML_PI ,
  XML_ERROR_UNKNOWN_ENCODING ,
  XML_ERROR_INCORRECT_ENCODING ,
  XML_ERROR_UNCLOSED_CDATA_SECTION ,
  XML_ERROR_EXTERNAL_ENTITY_HANDLING ,
  XML_ERROR_NOT_STANDALONE ,
  XML_ERROR_UNEXPECTED_STATE ,
  XML_ERROR_ENTITY_DECLARED_IN_PE ,
  XML_ERROR_FEATURE_REQUIRES_XML_DTD ,
  XML_ERROR_CANT_CHANGE_FEATURE_ONCE_PARSING ,
  { Added in 1.95.7. }
  XML_ERROR_UNBOUND_PREFIX ,
  { Added in 1.95.8. }
  XML_ERROR_UNDECLARING_PREFIX ,
  XML_ERROR_INCOMPLETE_PE ,
  XML_ERROR_XML_DECL ,
  XML_ERROR_TEXT_DECL ,
  XML_ERROR_PUBLICID ,
  XML_ERROR_SUSPENDED ,
  XML_ERROR_NOT_SUSPENDED ,
  XML_ERROR_ABORTED ,
  XML_ERROR_FINISHED ,
  XML_ERROR_SUSPEND_PE ,
  { Added in 2.0. }
  XML_ERROR_RESERVED_PREFIX_XML ,
  XML_ERROR_RESERVED_PREFIX_XMLNS ,
  XML_ERROR_RESERVED_NAMESPACE_URI );

 XML_Content_Type = (
  ___SKIP_ZERO____  ,
  XML_CTYPE_EMPTY ,
  XML_CTYPE_ANY ,
  XML_CTYPE_MIXED ,
  XML_CTYPE_NAME ,
  XML_CTYPE_CHOICE ,
  XML_CTYPE_SEQ );

 XML_Content_Quant = (
  XML_CQUANT_NONE ,
  XML_CQUANT_OPT ,
  XML_CQUANT_REP ,
  XML_CQUANT_PLUS );

 XML_ParamEntityParsing = (
  XML_PARAM_ENTITY_PARSING_NEVER ,
  XML_PARAM_ENTITY_PARSING_UNLESS_STANDALONE ,
  XML_PARAM_ENTITY_PARSING_ALWAYS );
    
{ If type == XML_CTYPE_EMPTY or XML_CTYPE_ANY, then quant will be
  XML_CQUANT_NONE, and the other fields will be zero or NULL.
  If type == XML_CTYPE_MIXED, then quant will be NONE or REP and
  numchildren will contain number of elements that may be mixed in
  and children point to an array of XML_Content cells that will be
  all of XML_CTYPE_NAME type with no quantification.

  If type == XML_CTYPE_NAME, then the name points to the name, and
  the numchildren field will be zero and children will be NULL. The
  quant fields indicates any quantifiers placed on the name.

  CHOICE and SEQ will have name NULL, the number of children in
  numchildren and children will point, recursively, to an array
  of XML_Content cells.

  The EMPTY, ANY, and MIXED types will only occur at top level. }
 XML_Content_ptr = ^XML_Content;

 XML_cp = record
   type_ : XML_Content_Type;
   quant : XML_Content_Quant;
   name  : XML_Char_ptr;

   numchildren : unsigned;
   children    : XML_Content_ptr;

  end;

 XML_Content = XML_cp;

{ This is called for an element declaration. See above for
  description of the model argument. It's the caller's responsibility
  to free model when finished with it. }
 XML_ElementDeclHandler = procedure(userData : pointer; name : XML_Char_ptr; model : XML_Content_ptr );

{ The Attlist declaration handler is called for *each* attribute. So
  a single Attlist declaration with multiple attributes declared will
  generate multiple calls to this handler. The "default" parameter
  may be NULL in the case of the "#IMPLIED" or "#REQUIRED"
  keyword. The "isrequired" parameter will be true and the default
  value will be NULL in the case of "#REQUIRED". If "isrequired" is
  true and default is non-NULL, then this is a "#FIXED" default. }
 XML_AttlistDeclHandler = procedure(
                           userData : pointer;
                           elname ,attname ,att_type ,dflt : XML_Char_ptr;
                           isrequired : int );

{ The XML declaration handler is called for *both* XML declarations
  and text declarations. The way to distinguish is that the version
  parameter will be NULL for text declarations. The encoding
  parameter may be NULL for XML declarations. The standalone
  parameter will be -1, 0, or 1 indicating respectively that there
  was no standalone parameter in the declaration, that it was given
  as no, or that it was given as yes. }
 XML_XmlDeclHandler = procedure(userData : pointer; version ,encoding : XML_Char_ptr; standalone : int );

{ This is called for entity declarations. The is_parameter_entity
  argument will be non-zero if the entity is a parameter entity, zero
  otherwise.

  For internal entities (<!ENTITY foo "bar">), value will
  be non-NULL and systemId, publicID, and notationName will be NULL.
  The value string is NOT nul-terminated; the length is provided in
  the value_length argument. Since it is legal to have zero-length
  values, do not use this argument to test for internal entities.

  For external entities, value will be NULL and systemId will be
  non-NULL. The publicId argument will be NULL unless a public
  identifier was provided. The notationName argument will have a
  non-NULL value only for unparsed entity declarations.

  Note that is_parameter_entity can't be changed to XML_Bool, since
  that would break binary compatibility. }
 XML_EntityDeclHandler = procedure(
                          userData : pointer;
                          entityName : XML_Char_ptr;
                          is_parameter_entity : int;
                          value : XML_Char_ptr;
                          value_length : int;
                          base ,systemId ,publicId ,notationName : XML_Char_ptr );

{ atts is array of name/value pairs, terminated by 0;
  names and values are 0 terminated. }
 XML_StartElementHandler = procedure(userData : pointer; name : XML_Char_ptr; atts : XML_Char_ptr_ptr );
 XML_EndElementHandler   = procedure(userData : pointer; name : XML_Char_ptr );

{ s is not 0 terminated. }
 XML_CharacterDataHandler = procedure(userData : pointer; s : XML_Char_ptr; len : int );

{ target and data are 0 terminated }
 XML_ProcessingInstructionHandler = procedure(userData : pointer; target ,data : XML_Char_ptr );

{ data is 0 terminated }
 XML_CommentHandler = procedure(userData : pointer; data : XML_Char_ptr );

 XML_StartCdataSectionHandler = procedure(userData : pointer );
 XML_EndCdataSectionHandler   = procedure(userData : pointer );


{ This is called for any characters in the XML document for which
  there is no applicable handler.  This includes both characters that
  are part of markup which is of a kind that is not reported
  (comments, markup declarations), or characters that are part of a
  construct which could be reported but for which no handler has been
  supplied. The characters are passed exactly as they were in the XML
  document except that they will be encoded in UTF-8 or UTF-16.
  Line boundaries are not normalized. Note that a byte order mark
  character is not passed to the default handler. There are no
  guarantees about how characters are divided between calls to the
  default handler: for example, a comment might be split between
  multiple calls. }
 XML_DefaultHandler = procedure(userData : pointer; s : XML_Char_ptr; len : int );

{ This is called for the start of the DOCTYPE declaration, before
  any DTD or internal subset is parsed. }
 XML_StartDoctypeDeclHandler = procedure(
                                userData : pointer;
                                doctypeName ,sysid ,pubid : XML_Char_ptr;
                                has_internal_subset : int );

{ This is called for the start of the DOCTYPE declaration when the
  closing > is encountered, but after processing any external
  subset. }
 XML_EndDoctypeDeclHandler = procedure(userData : pointer );

{ OBSOLETE -- OBSOLETE -- OBSOLETE
  This handler has been superceded by the EntityDeclHandler above.
  It is provided here for backward compatibility.

  This is called for a declaration of an unparsed (NDATA) entity.
  The base argument is whatever was set by XML_SetBase. The
  entityName, systemId and notationName arguments will never be
  NULL. The other arguments may be. }
 XML_UnparsedEntityDeclHandler = procedure(
                                  userData : pointer;
                                  entityName ,base ,systemId ,publicId ,notationName : XML_Char_ptr );

{ This is called for a declaration of notation.  The base argument is
  whatever was set by XML_SetBase. The notationName will never be
  NULL.  The other arguments can be. }
 XML_NotationDeclHandler = procedure(
                            userData : pointer;
                            notationName ,base ,systemId ,publicId : XML_Char_ptr );

{ When namespace processing is enabled, these are called once for
  each namespace declaration. The call to the start and end element
  handlers occur between the calls to the start and end namespace
  declaration handlers. For an xmlns attribute, prefix will be
  NULL.  For an xmlns="" attribute, uri will be NULL. }
 XML_StartNamespaceDeclHandler = procedure(userData : pointer; prefix ,uri : XML_Char_ptr );
 XML_EndNamespaceDeclHandler   = procedure(userData : pointer; prefix : XML_Char_ptr );

{ This is called if the document is not standalone, that is, it has an
  external subset or a reference to a parameter entity, but does not
  have standalone="yes". If this handler returns XML_STATUS_ERROR,
  then processing will not continue, and the parser will return a
  XML_ERROR_NOT_STANDALONE error.
  If parameter entity parsing is enabled, then in addition to the
  conditions above this handler will only be called if the referenced
  entity was actually read. }
 XML_NotStandaloneHandler = function(userData : pointer ) : int;

{ This is called for a reference to an external parsed general
  entity.  The referenced entity is not automatically parsed.  The
  application can parse it immediately or later using
  XML_ExternalEntityParserCreate.

  The parser argument is the parser parsing the entity containing the
  reference; it can be passed as the parser argument to
  XML_ExternalEntityParserCreate.  The systemId argument is the
  system identifier as specified in the entity declaration; it will
  not be NULL.

  The base argument is the system identifier that should be used as
  the base for resolving systemId if systemId was relative; this is
  set by XML_SetBase; it may be NULL.

  The publicId argument is the public identifier as specified in the
  entity declaration, or NULL if none was specified; the whitespace
  in the public identifier will have been normalized as required by
  the XML spec.

  The context argument specifies the parsing context in the format
  expected by the context argument to XML_ExternalEntityParserCreate;
  context is valid only until the handler returns, so if the
  referenced entity is to be parsed later, it must be copied.
  context is NULL only when the entity is a parameter entity.

  The handler should return XML_STATUS_ERROR if processing should not
  continue because of a fatal error in the handling of the external
  entity.  In this case the calling parser will return an
  XML_ERROR_EXTERNAL_ENTITY_HANDLING error.

  Note that unlike other handlers the first argument is the parser,
  not userData. }
 XML_ExternalEntityRefHandler = function(
                                 parser : XML_Parser;
                                 context ,base ,systemId ,publicId : XML_Char_ptr ) : int;

{ This is called in two situations:
  1) An entity reference is encountered for which no declaration
     has been read *and* this is not an error.
  2) An internal entity reference is read, but not expanded, because
     XML_SetDefaultHandler has been called.
  Note: skipped parameter entities in declarations and skipped general
        entities in attribute values cannot be reported, because
        the event would be out of sync with the reporting of the
        declarations or attribute values }
 XML_SkippedEntityHandler = procedure(userData : pointer; entityName : XML_Char_ptr; is_parameter_entity : int );

(* This structure is filled in by the XML_UnknownEncodingHandler to
  provide information to the parser about encodings that are unknown
  to the parser.

  The map[b] member gives information about byte sequences whose
  first byte is b.

  If map[b] is c where c is >= 0, then b by itself encodes the
  Unicode scalar value c.

  If map[b] is -1, then the byte sequence is malformed.

  If map[b] is -n, where n >= 2, then b is the first byte of an
  n-byte sequence that encodes a single Unicode scalar value.

  The data member will be passed as the first argument to the convert
  function.

  The convert function is used to convert multibyte sequences; s will
  point to a n-byte sequence where map[(unsigned char)*s] == -n.  The
  convert function must return the Unicode scalar value represented
  by this byte sequence or -1 if the byte sequence is malformed.

  The convert function may be NULL if the encoding is a single-byte
  encoding, that is if map[b] >= -1 for all bytes b.

  When the parser is finished with the encoding, then if release is
  not NULL, it will call release passing it the data member; once
  release has been called, the convert function will not be called
  again.

  Expat places certain restrictions on the encodings that are supported
  using this mechanism.

  1. Every ASCII character that can appear in a well-formed XML document,
     other than the characters

     $@\^`{}~

     must be represented by a single byte, and that byte must be the
     same byte that represents that character in ASCII.

  2. No character may require more than 4 bytes to encode.

  3. All characters encoded must have Unicode scalar values <=
     0xFFFF, (i.e., characters that would be encoded by surrogates in
     UTF-16 are  not allowed).  Note that this restriction doesn't
     apply to the built-in support for UTF-8 and UTF-16.

  4. No Unicode character may be encoded by more than one distinct
     sequence of bytes. *)
 XML_Encoding_ptr = ^XML_Encoding;
 XML_Encoding = record
   map  : array[0..255 ] of int;
   data : pointer;

   convert : function (data : pointer; s : char_ptr ) : int;
   release : procedure(data : pointer );

  end;

{ This is called for an encoding that is unknown to the parser.

  The encodingHandlerData argument is that which was passed as the
  second argument to XML_SetUnknownEncodingHandler.

  The name argument gives the name of the encoding as specified in
  the encoding declaration.

  If the callback can provide information about the encoding, it must
  fill in the XML_Encoding structure, and return XML_STATUS_OK.
  Otherwise it must return XML_STATUS_ERROR.

  If info does not describe a suitable encoding, then the parser will
  return an XML_UNKNOWN_ENCODING error. }
 XML_UnknownEncodingHandler = function(
                               encodingHandlerData : pointer;
                               name : XML_Char_ptr;
                               info : XML_Encoding_ptr ) : int;


 XML_Memory_Handling_Suite_ptr = ^XML_Memory_Handling_Suite;
 XML_Memory_Handling_Suite = record
   malloc_fcn  : function(var ptr : pointer; sz : integer ) : boolean;
   realloc_fcn : function(var ptr : pointer; old ,sz : integer ) : boolean;
   free_fcn    : function(var ptr : pointer; sz : integer ) : boolean;

  end;

 KEY = XML_Char_ptr;

 NAMED_ptr_ptr = ^NAMED_ptr;
 NAMED_ptr = ^NAMED;
 NAMED = record
   name  : KEY;
   alloc : int;

  end;

 HASH_TABLE_ptr = ^HASH_TABLE;
 HASH_TABLE = record
   v : NAMED_ptr_ptr;
   a : int;

   power : int8u;
   size  ,
   used  : size_t;
   mem   : XML_Memory_Handling_Suite_ptr;

  end;

 ENTITY_ptr = ^ENTITY;
 ENTITY = record
   name  : XML_Char_ptr;
   alloc : int;

   textPtr   : XML_Char_ptr;
   textLen   ,                { length in XML_Chars }
   processed : int;           { # of processed bytes - when suspended }
   systemId  ,
   base      ,
   publicId  ,
   notation  : XML_Char_ptr;

   open        ,
   is_param    ,
   is_internal : XML_Bool;    { true if declared in internal subset outside PE }

  end;

 OPEN_INTERNAL_ENTITY_ptr = ^OPEN_INTERNAL_ENTITY;
 OPEN_INTERNAL_ENTITY = record
   internalEventPtr    ,
   internalEventEndPtr : char_ptr;

   next   : OPEN_INTERNAL_ENTITY_ptr;
   entity : ENTITY_ptr;

   startTagLevel : int;
   betweenDecl   : XML_Bool; { WFC: PE Between Declarations }

  end;

 CONTENT_SCAFFOLD_ptr = ^CONTENT_SCAFFOLD;
 CONTENT_SCAFFOLD = record
   type_ : XML_Content_Type;
   quant : XML_Content_Quant;
   name  : XML_Char_ptr;

   firstchild ,
   lastchild  ,
   childcnt   ,
   nextsib    : int;

  end;

 PREFIX_ptr = ^PREFIX;

 ATTRIBUTE_ID_ptr = ^ATTRIBUTE_ID;
 ATTRIBUTE_ID = record
   name   : XML_Char_ptr;
   alloc  : int;
   prefix : PREFIX_ptr;

   maybeTokenized ,
   xmlns          : XML_Bool;

  end;

 DEFAULT_ATTRIBUTE_ptr = ^DEFAULT_ATTRIBUTE;
 DEFAULT_ATTRIBUTE = record
   id : ATTRIBUTE_ID_ptr;

   isCdata : XML_Bool;
   value   : XML_Char_ptr;

  end;

 ELEMENT_TYPE_ptr = ^ELEMENT_TYPE;
 ELEMENT_TYPE = record
   name   : XML_Char_ptr;
   alloc  : int;
   prefix : PREFIX_ptr;
   idAtt  : ATTRIBUTE_ID_ptr;

   nDefaultAtts     ,
   allocDefaultAtts ,
   defaultAttsAlloc : int;

   defaultAtts : DEFAULT_ATTRIBUTE_ptr;

  end;

 TAG_NAME_ptr = ^TAG_NAME; 
 TAG_NAME = record
   str       ,
   localPart ,
   prefix    : XML_Char_ptr;
   strLen    ,
   uriLen    ,
   prefixLen : int;

  end; 

{ TAG represents an open element.
  The name of the element is stored in both the document and API
  encodings.  The memory buffer 'buf' is a separately-allocated
  memory area which stores the name.  During the XML_Parse()/
  XMLParseBuffer() when the element is open, the memory for the 'raw'
  version of the name (in the document encoding) is shared with the
  document buffer.  If the element is open across calls to
  XML_Parse()/XML_ParseBuffer(), the buffer is re-allocated to
  contain the 'raw' name as well.

  A parser re-uses these structures, maintaining a list of allocated
  TAG objects in a free list. }
 BINDING_ptr_ptr = ^BINDING_ptr;
 BINDING_ptr = ^BINDING;

 TAG_ptr = ^TAG;
 TAG = record
   parent  : TAG_ptr;      { parent of this element }
   rawName : char_ptr;     { tagName in the original encoding }

   rawNameLength : int;

   name : TAG_NAME;        { tagName in the API encoding }

   buf    ,                { buffer for name components }
   bufEnd : char_ptr;      { end of the buffer }
   alloc  : int;

   bindings : BINDING_ptr;

  end;

 BINDING = record
   prefix : PREFIX_ptr;

   nextTagBinding    ,
   prevPrefixBinding : BINDING_ptr;

   attId : ATTRIBUTE_ID_ptr;
   uri   : XML_Char_ptr;

   uriLen   ,
   uriAlloc : int;

  end;

 PREFIX = record
   name    : XML_Char_ptr;
   alloc   : int;
   binding : BINDING_ptr;

  end;

 NS_ATT_ptr = ^NS_ATT;
 NS_ATT = record
   version ,
   hash    : int32u;
   uriName : XML_Char_ptr;

  end;

 BLOCK_ptr = ^BLOCK;
 BLOCK = record
   next  : BLOCK_ptr;
   size  ,
   alloc : int;

   s : array[0..0 ] of XML_Char;

  end;

 STRING_POOL_ptr = ^STRING_POOL;
 STRING_POOL = record
   blocks     ,
   freeBlocks : BLOCK_ptr;

   end_  ,
   ptr   ,
   start : XML_Char_ptr;

   mem : XML_Memory_Handling_Suite_ptr;

  end;

 DTD_ptr = ^DTD;
 DTD = record
   generalEntities ,
   elementTypes    ,
   attributeIds    ,
   prefixes        : HASH_TABLE;

   pool            ,
   entityValuePool : STRING_POOL;

  { false once a parameter entity reference has been skipped }
   keepProcessing : XML_Bool;

  { true once an internal or external PE reference has been encountered;
    this includes the reference to an external subset }
   hasParamEntityRefs ,
   standalone         : XML_Bool;

  {$IFDEF XML_DTD }
  { indicates if external PE has been read }
   paramEntityRead : XML_Bool;
   paramEntities   : HASH_TABLE;

  {$ENDIF }

   defaultPrefix : PREFIX;

  { === scaffolding for building content model === }
   in_eldecl : XML_Bool;
   scaffold  : CONTENT_SCAFFOLD_ptr;

   contentStringLen ,
   scaffSize        ,
   scaffCount       : unsigned;

   scaffLevel : int;
   scaffIndex : int_ptr;
   scaffAlloc : int;

  end;

 XML_Parsing = (
  XML_INITIALIZED ,
  XML_PARSING_    ,
  XML_FINISHED    ,
  XML_SUSPENDED   );

 XML_ParsingStatus = record
   parsing     : XML_Parsing;
   finalBuffer : XML_Bool;

  end;

 Processor = function(parser : XML_Parser; start ,end_ : char_ptr; endPtr : char_ptr_ptr ) : XML_Error;

 XML_ParserStruct = record
   m_userData   ,
   m_handlerArg : pointer;

   m_buffer : char_ptr;
   m_mem    : XML_Memory_Handling_Suite;

  { first character to be parsed }
   m_bufferPtr : char_ptr;

  { past last character to be parsed }
   m_bufferEnd : char_ptr;

  { allocated end of buffer }
   m_bufferLim : char_ptr;

  { the size of the allocated buffer }
   m_bufferAloc : int;

   m_parseEndByteIndex : XML_Index;

   m_parseEndPtr : char_ptr;
   m_dataBuf     ,
   m_dataBufEnd  : XML_Char_ptr;

  { XML Handlers } 
   m_startElementHandler          : XML_StartElementHandler;
   m_endElementHandler            : XML_EndElementHandler;
   m_characterDataHandler         : XML_CharacterDataHandler;
   m_processingInstructionHandler : XML_ProcessingInstructionHandler;
   m_commentHandler               : XML_CommentHandler;
   m_startCdataSectionHandler     : XML_StartCdataSectionHandler;
   m_endCdataSectionHandler       : XML_EndCdataSectionHandler;
   m_defaultHandler               : XML_DefaultHandler;
   m_startDoctypeDeclHandler      : XML_StartDoctypeDeclHandler;
   m_endDoctypeDeclHandler        : XML_EndDoctypeDeclHandler;
   m_unparsedEntityDeclHandler    : XML_UnparsedEntityDeclHandler;
   m_notationDeclHandler          : XML_NotationDeclHandler;
   m_startNamespaceDeclHandler    : XML_StartNamespaceDeclHandler;
   m_endNamespaceDeclHandler      : XML_EndNamespaceDeclHandler;
   m_notStandaloneHandler         : XML_NotStandaloneHandler;
   m_externalEntityRefHandler     : XML_ExternalEntityRefHandler;
   m_externalEntityRefHandlerArg  : XML_Parser;
   m_skippedEntityHandler         : XML_SkippedEntityHandler;
   m_unknownEncodingHandler       : XML_UnknownEncodingHandler;
   m_elementDeclHandler           : XML_ElementDeclHandler;
   m_attlistDeclHandler           : XML_AttlistDeclHandler;
   m_entityDeclHandler            : XML_EntityDeclHandler;
   m_xmlDeclHandler               : XML_XmlDeclHandler;

   m_encoding             : ENCODING_ptr;
   m_initEncoding         : INIT_ENCODING;
   m_internalEncoding     : ENCODING_ptr;
   m_protocolEncodingName : XML_Char_ptr;

   m_ns          ,
   m_ns_triplets : XML_Bool;

   m_unknownEncodingMem         ,
   m_unknownEncodingData        ,
   m_unknownEncodingHandlerData : pointer;
   m_unknownEncodingAlloc       : int;

   m_unknownEncodingRelease : procedure(void : pointer );

   m_prologState : PROLOG_STATE;
   m_processor   : Processor;
   m_errorCode   : XML_Error;
   m_eventPtr    ,
   m_eventEndPtr ,
   m_positionPtr : char_ptr;

   m_openInternalEntities ,
   m_freeInternalEntities : OPEN_INTERNAL_ENTITY_ptr;

   m_defaultExpandInternalEntities : XML_Bool;

   m_tagLevel   : int;
   m_declEntity : ENTITY_ptr;

   m_doctypeName          ,
   m_doctypeSysid         ,
   m_doctypePubid         ,
   m_declAttributeType    ,
   m_declNotationName     ,
   m_declNotationPublicId : XML_Char_ptr;

   m_declElementType : ELEMENT_TYPE_ptr;
   m_declAttributeId : ATTRIBUTE_ID_ptr;

   m_declAttributeIsCdata ,
   m_declAttributeIsId    : XML_Bool;

   m_dtd : DTD_ptr;

   m_curBase : XML_Char_ptr;

   m_tagStack    ,
   m_freeTagList : TAG_ptr;

   m_inheritedBindings ,
   m_freeBindingList   : BINDING_ptr;

   m_attsSize       ,
   m_attsAlloc      ,
   m_nsAttsAlloc    ,
   m_nSpecifiedAtts ,
   m_idAttIndex     : int;

   m_atts   : ATTRIBUTE_ptr;
   m_nsAtts : NS_ATT_ptr;

   m_nsAttsVersion : unsigned;
   m_nsAttsPower   : int8u;

   m_position  : POSITION;
   m_tempPool  ,
   m_temp2Pool : STRING_POOL;

   m_groupConnector : char_ptr;
   m_groupSize      ,
   m_groupAlloc     : unsigned;

   m_namespaceSeparator : XML_Char;

   m_parentParser  : XML_Parser;
   m_parsingStatus : XML_ParsingStatus;

  {$IFDEF XML_DTD }
   m_isParamEntity ,
   m_useForeignDTD : XML_Bool;

   m_paramEntityParsing : XML_ParamEntityParsing;

  {$ENDIF }

  end;

{ GLOBAL CONSTANTS }
const
 XML_TRUE  = 1;
 XML_FALSE = 0;
 

{ GLOBAL PROCEDURES }
{ Constructs a new parser; encoding is the encoding specified by the
  external protocol or NIL if there is none specified. }
 function  XML_ParserCreate(const encoding : XML_Char_ptr ) : XML_Parser;

{ Constructs a new parser using the memory management suite referred to
  by memsuite. If memsuite is NULL, then use the standard library memory
  suite. If namespaceSeparator is non-NULL it creates a parser with
  namespace processing as described above. The character pointed at
  will serve as the namespace separator.

  All further memory operations used for the created parser will come from
  the given suite. }
 function  XML_ParserCreate_MM(
            encoding : XML_Char_ptr;
            memsuite : XML_Memory_Handling_Suite_ptr;
            namespaceSeparator : XML_Char_ptr ) : XML_Parser;

{ This value is passed as the userData argument to callbacks. }
 procedure XML_SetUserData(parser : XML_Parser; userData : pointer );

 procedure XML_SetElementHandler(
            parser : XML_Parser;
            start  : XML_StartElementHandler;
            end_   : XML_EndElementHandler );

 procedure XML_SetCharacterDataHandler(
            parser  : XML_Parser;
            handler : XML_CharacterDataHandler );

{ Parses some input. Returns XML_STATUS_ERROR if a fatal error is
  detected.  The last call to XML_Parse must have isFinal true; len
  may be zero for this call (or any other).

  Though the return values for these functions has always been
  described as a Boolean value, the implementation, at least for the
  1.95.x series, has always returned exactly one of the XML_Status
  values. }
 function  XML_Parse(parser : XML_Parser; const s : char_ptr; len ,isFinal : int ) : XML_Status;

{ If XML_Parse or XML_ParseBuffer have returned XML_STATUS_ERROR, then
  XML_GetErrorCode returns information about the error. }
 function  XML_GetErrorCode(parser : XML_Parser ) : XML_Error;

{ Returns a string describing the error. }
 function  XML_ErrorString(code : XML_Error ) : XML_LChar_ptr;

{ These functions return information about the current parse
  location.  They may be called from any callback called to report
  some parse event; in this case the location is the location of the
  first of the sequence of characters that generated the event.  When
  called from callbacks generated by declarations in the document
  prologue, the location identified isn't as neatly defined, but will
  be within the relevant markup.  When called outside of the callback
  functions, the position indicated will be just past the last parse
  event (regardless of whether there was an associated callback).

  They may also be called after returning from a call to XML_Parse
  or XML_ParseBuffer.  If the return value is XML_STATUS_ERROR then
  the location is the location of the character at which the error
  was detected; otherwise the location is the location of the last
  parse event, as described above. }
 function  XML_GetCurrentLineNumber(parser : XML_Parser ) : XML_Size;

{ Frees memory used by the parser. }
 procedure XML_ParserFree(parser : XML_Parser );

 
IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{$I xmlparse.inc }

END.


