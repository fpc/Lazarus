{
 **********************************************************************
  This file is part of LazUtils.
  It is copied from FCL unit dom svn revision 15251 and adapted to use
  UTF8 instead of widestrings by Mattias Gaertner.

  See the file COPYING.FPC, included in this distribution,
  for details about the license.
 **********************************************************************

  Implementation of DOM interfaces
  Copyright (c) 1999-2000 by Sebastian Guenther, sg@freepascal.org
  Modified in 2006 by Sergei Gorelkin, sergei_gorelkin@mail.ru

}

{
  This unit provides classes which implement the interfaces defined in the
  DOM (Document Object Model) specification.
  The current state is:
  DOM Levels 1 and 2 -  Completely implemented
  DOM Level 3  -  Partially implemented

  Specification used for this implementation:

  "Document Object Model (DOM) Level 2 Specification Version 1.0
   W3C Recommendation 11 November, 2000"
   http://www.w3.org/TR/2000/REC-DOM-Level-2-Core-20001113
}


unit Laz2_DOM;

{$ifdef fpc}
{$MODE objfpc}{$H+}
{$endif}

interface

uses
  SysUtils, Classes, laz2_xmlutils;

// -------------------------------------------------------
//   DOMException
// -------------------------------------------------------

const

  // DOM Level 1 exception codes:

  INDEX_SIZE_ERR              = 1;  // index or size is negative, or greater than the allowed value
  DOMSTRING_SIZE_ERR          = 2;  // Specified range of text does not fit into a DOMString
  HIERARCHY_REQUEST_ERR       = 3;  // node is inserted somewhere it does not belong
  WRONG_DOCUMENT_ERR          = 4;  // node is used in a different document than the one that created it (that does not support it)
  INVALID_CHARACTER_ERR       = 5;  // invalid or illegal character is specified, such as in a name
  NO_DATA_ALLOWED_ERR         = 6;  // data is specified for a node which does not support data
  NO_MODIFICATION_ALLOWED_ERR = 7;  // an attempt is made to modify an object where modifications are not allowed
  NOT_FOUND_ERR               = 8;  // an attempt is made to reference a node in a context where it does not exist
  NOT_SUPPORTED_ERR           = 9;  // implementation does not support the type of object requested
  INUSE_ATTRIBUTE_ERR         = 10;  // an attempt is made to add an attribute that is already in use elsewhere

  // DOM Level 2 exception codes:

  INVALID_STATE_ERR           = 11;  // an attempt is made to use an object that is not, or is no longer, usable
  SYNTAX_ERR                  = 12;  // invalid or illegal string specified
  INVALID_MODIFICATION_ERR    = 13;  // an attempt is made to modify the type of the underlying object
  NAMESPACE_ERR               = 14;  // an attempt is made to create or change an object in a way which is incorrect with regard to namespaces
  INVALID_ACCESS_ERR          = 15;  // parameter or operation is not supported by the underlying object

// -------------------------------------------------------
//   Node
// -------------------------------------------------------

const
  ELEMENT_NODE = 1;
  ATTRIBUTE_NODE = 2;
  TEXT_NODE = 3;
  CDATA_SECTION_NODE = 4;
  ENTITY_REFERENCE_NODE = 5;
  ENTITY_NODE = 6;
  PROCESSING_INSTRUCTION_NODE = 7;
  COMMENT_NODE = 8;
  DOCUMENT_NODE = 9;
  DOCUMENT_TYPE_NODE = 10;
  DOCUMENT_FRAGMENT_NODE = 11;
  NOTATION_NODE = 12;

type
  TDOMDocument = class;
  TDOMNodeList = class;
  TDOMNamedNodeMap = class;
  TDOMNode = class;
  TDOMAttr = class;
  TDOMElement = class;
  TDOMText = class;
  TDOMComment = class;
  TDOMCDATASection = class;
  TDOMDocumentType = class;
  TDOMEntityReference = class;
  TDOMProcessingInstruction = class;

  TDOMAttrDef = class;
  TNodePool = class;
  PNodePoolArray = ^TNodePoolArray;
  TNodePoolArray = array[0..MaxInt div sizeof(Pointer)-1] of TNodePool;

{$ifndef fpc}
  TFPList = TList;
{$endif}

// -------------------------------------------------------
//   DOMString
// -------------------------------------------------------

  TSetOfChar = set of Char;
  DOMString = AnsiString;
  DOMPChar = PChar;
  DOMChar = Char;
  PDOMString = ^DOMString;

  EDOMError = class(Exception)
  public
    Code: Integer;
    constructor Create(ACode: Integer; const ASituation: String);
  end;

  EDOMIndexSize = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMHierarchyRequest = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMWrongDocument = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMNotFound = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMNotSupported = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMInUseAttribute = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMInvalidState = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMSyntax = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMInvalidModification = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMNamespace = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMInvalidAccess = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

{ NodeType, NodeName and NodeValue had been moved from fields to functions.
  This lowers memory usage and also obsoletes most constructors,
  at a slight performance penalty. However, NodeName and NodeValue are
  accessible via fields using specialized properties of descendant classes,
  e.g. TDOMElement.TagName, TDOMCharacterData.Data etc.}

  TNodeFlagEnum = (
    nfReadonly,
    nfRecycled,
    nfLevel2,
    nfIgnorableWS,
    nfSpecified,
    nfDestroying
  );
  TNodeFlags = set of TNodeFlagEnum;

  { TDOMNodeEnumerator }

  TDOMNodeEnumerator = class
  private
    FNode: TDOMNode;
    FCurrent: TDOMNode;
  public
    constructor Create(Node: TDOMNode);
    function MoveNext: boolean;
    property Current: TDOMNode read FCurrent;
  end;

  { TDOMNodeAllChildEnumerator }

  TDOMNodeAllChildEnumerator = class
  private
    FNode: TDOMNode;
    FCurrent: TDOMNode;
    FEnd: TDOMNode;
  public
    constructor Create(Node: TDOMNode);
    function MoveNext: boolean;
    property Current: TDOMNode read FCurrent;
    function GetEnumerator: TDOMNodeAllChildEnumerator; // including grand children
  end;

  { TDOMNode }

  TDOMNode = class
  protected
    FPool: TObject;
    FFlags: TNodeFlags;
    FParentNode: TDOMNode;
    FPreviousSibling, FNextSibling: TDOMNode;
    FOwnerDocument: TDOMDocument;

    function  GetNodeName: DOMString; virtual; abstract;
    function  GetNodeValue: DOMString; virtual;
    procedure SetNodeValue(const {%H-}AValue: DOMString); virtual;
    function  GetFirstChild: TDOMNode; virtual;
    function  GetLastChild: TDOMNode; virtual;
    function  GetAttributes: TDOMNamedNodeMap; virtual;
    function GetRevision: Integer;
    function GetNodeType: Integer; virtual; abstract;
    function GetTextContent: DOMString; virtual;
    procedure SetTextContent(const AValue: DOMString); virtual;
    function GetLocalName: DOMString; virtual;
    function GetNamespaceURI: DOMString; virtual;
    function GetPrefix: DOMString; virtual;
    procedure SetPrefix(const {%H-}Value: DOMString); virtual;
    function GetOwnerDocument: TDOMDocument; virtual;
    function GetBaseURI: DOMString;
    procedure SetReadOnly(Value: Boolean);
    procedure Changing;
  public
    constructor Create(AOwner: TDOMDocument);
    destructor Destroy; override;
    procedure FreeInstance; override;

    function GetChildNodes: TDOMNodeList;
    function GetChildCount: SizeInt; virtual;

    property NodeName: DOMString read GetNodeName;
    property NodeValue: DOMString read GetNodeValue write SetNodeValue;
    property NodeType: Integer read GetNodeType;
    property ParentNode: TDOMNode read FParentNode;
    property FirstChild: TDOMNode read GetFirstChild;
    property LastChild: TDOMNode read GetLastChild;
    property ChildNodes: TDOMNodeList read GetChildNodes;
    property PreviousSibling: TDOMNode read FPreviousSibling;
    property NextSibling: TDOMNode read FNextSibling;
    property Attributes: TDOMNamedNodeMap read GetAttributes;
    property OwnerDocument: TDOMDocument read GetOwnerDocument;
    function GetEnumerator: TDOMNodeEnumerator; // all children excluding grand children
    function GetEnumeratorAllChildren: TDOMNodeAllChildEnumerator; // all children including grand children
    function GetNextNode: TDOMNode; // first child, then next sibling, then next sibling of parent, ...
    function GetNextNodeSkipChildren: TDOMNode; // first next sibling, then next sibling of parent, ...
    function GetPreviousNode: TDOMNode; // the reverse of GetNextNode
    function GetLastLeaf: TDOMNode; // get last child of last child of ...
    function GetLevel: SizeInt; // root node has 0

    function InsertBefore({%H-}NewChild, {%H-}RefChild: TDOMNode): TDOMNode; virtual;
    function ReplaceChild({%H-}NewChild, {%H-}OldChild: TDOMNode): TDOMNode; virtual;
    function DetachChild({%H-}OldChild: TDOMNode): TDOMNode; virtual;
    function RemoveChild(OldChild: TDOMNode): TDOMNode;
    function AppendChild(NewChild: TDOMNode): TDOMNode;
    function HasChildNodes: Boolean; virtual;
    function CloneNode(deep: Boolean): TDOMNode; overload;

    // DOM level 2
    function IsSupported(const Feature, Version: DOMString): Boolean;
    function HasAttributes: Boolean; virtual;
    procedure Normalize; virtual;

    property NamespaceURI: DOMString read GetNamespaceURI;
    property LocalName: DOMString read GetLocalName;
    property Prefix: DOMString read GetPrefix write SetPrefix;
    // DOM level 3
    property TextContent: DOMString read GetTextContent write SetTextContent;
    function LookupPrefix(const nsURI: DOMString): DOMString;
    function LookupNamespaceURI(const APrefix: DOMString): DOMString;
    function IsDefaultNamespace(const nsURI: DOMString): Boolean;
    property baseURI: DOMString read GetBaseURI;
    // Extensions to DOM interface:
    function CloneNode({%H-}deep: Boolean; {%H-}ACloneOwner: TDOMDocument): TDOMNode; overload; virtual;
    function FindNode(const {%H-}ANodeName: DOMString): TDOMNode; virtual;
    function CompareName(const name: DOMString): Integer; virtual;
    property Flags: TNodeFlags read FFlags;
  end;

  TDOMNodeClass = class of TDOMNode;

  { The following class is an implementation specific extension, it is just an
    extended implementation of TDOMNode, the generic DOM::Node interface
    implementation. (Its main purpose is to save memory in a big node tree) }

  { TDOMNode_WithChildren }

  TDOMNode_WithChildren = class(TDOMNode)
  protected
    FFirstChild, FLastChild: TDOMNode;
    FChildNodes: TDOMNodeList;
    function GetFirstChild: TDOMNode; override;
    function GetLastChild: TDOMNode; override;
    procedure CloneChildren(ACopy: TDOMNode; ACloneOwner: TDOMDocument);
    procedure FreeChildren;
    function GetTextContent: DOMString; override;
    procedure SetTextContent(const AValue: DOMString); override;
  public
    destructor Destroy; override;
    function InsertBefore(NewChild, RefChild: TDOMNode): TDOMNode; override;
    function ReplaceChild(NewChild, OldChild: TDOMNode): TDOMNode; override;
    function DetachChild(OldChild: TDOMNode): TDOMNode; override;
    function HasChildNodes: Boolean; override;
    function GetChildCount: SizeInt; override;
    function FindNode(const ANodeName: DOMString): TDOMNode; override;
    procedure InternalAppend(NewChild: TDOMNode);
  end;


// -------------------------------------------------------
//   NodeList
// -------------------------------------------------------

  TFilterResult = (frFalse, frNorecurseFalse, frTrue, frNorecurseTrue);

  TDOMNodeList = class(TObject)
  protected
    FNode: TDOMNode;
    FRevision: Integer;
    FList: TFPList;
    function GetCount: LongWord;
    function GetItem(index: LongWord): TDOMNode;
    function NodeFilter({%H-}aNode: TDOMNode): TFilterResult; virtual;
    procedure BuildList; virtual; deprecated 'Use NodeFilter instead.';
  public
    constructor Create(ANode: TDOMNode);
    destructor Destroy; override;
    property Item[index: LongWord]: TDOMNode read GetItem; default;
    property Count: LongWord read GetCount;
    property Length: LongWord read GetCount;
  end;

  { an extension to DOM interface, used to build recursive lists of elements }

  TDOMElementList = class(TDOMNodeList)
  protected
    filter: DOMString;
    FNSIndexFilter: Integer;
    localNameFilter: DOMString;
    FMatchNS: Boolean;
    FMatchAnyNS: Boolean;
    UseFilter: Boolean;
    function NodeFilter(aNode: TDOMNode): TFilterResult; override;
  public
    constructor Create(ANode: TDOMNode; const AFilter: DOMString); overload;
    constructor Create(ANode: TDOMNode; const nsURI, localName: DOMString); overload;
  end;


// -------------------------------------------------------
//   NamedNodeMap
// -------------------------------------------------------

  { TDOMNamedNodeMap }

  TDOMNamedNodeMap = class(TObject)
  protected
    FOwner: TDOMNode;
    FNodeType: Integer;
    FSortedList: TFPList; // list of TDOMNode sorted via CompareName
    FPosList: TFPList; // list of TDOMNode not sorted
    function GetPosItem(index: LongWord): TDOMNode;
    function GetSortedItem(index: LongWord): TDOMNode;
    function GetLength: LongWord;
    function FindSorted(const name: DOMString; out Index: LongWord): Boolean;
    function DeleteSorted(index: LongWord): TDOMNode;
    procedure RestoreDefault(const name: DOMString);
    function InternalRemove(const name: DOMString): TDOMNode;
    function ValidateInsert(arg: TDOMNode): Integer;
  public
    constructor Create(AOwner: TDOMNode; ANodeType: Integer);
    destructor Destroy; override;

    function GetNamedItem(const name: DOMString): TDOMNode;
    function SetNamedItem(arg: TDOMNode): TDOMNode;
    function RemoveNamedItem(const name: DOMString): TDOMNode;
    // Introduced in DOM Level 2:
    function getNamedItemNS(const {%H-}namespaceURI, {%H-}localName: DOMString): TDOMNode; virtual;
    function setNamedItemNS(arg: TDOMNode): TDOMNode; virtual;
    function removeNamedItemNS(const {%H-}namespaceURI,{%H-}localName: DOMString): TDOMNode; virtual;

    property Item[index: LongWord]: TDOMNode read GetPosItem; default;
    property SortedItem[index: LongWord]: TDOMNode read GetSortedItem;
    property Length: LongWord read GetLength;
  end;


// -------------------------------------------------------
//   CharacterData
// -------------------------------------------------------

  TDOMCharacterData = class(TDOMNode)
  private
    FNodeValue: DOMString;
  protected
    function  GetLength: LongWord;
    function GetNodeValue: DOMString; override;
    procedure SetNodeValue(const AValue: DOMString); override;
  public
    property Data: DOMString read FNodeValue write SetNodeValue;
    property Length: LongWord read GetLength;
    function SubstringData(offset, count: LongWord): DOMString;
    procedure AppendData(const arg: DOMString);
    procedure InsertData(offset: LongWord; const arg: DOMString);
    procedure DeleteData(offset, count: LongWord);
    procedure ReplaceData(offset, count: LongWord; const arg: DOMString);
  end;


// -------------------------------------------------------
//   DOMImplementation
// -------------------------------------------------------

  TDOMImplementation = class
  public
    function HasFeature(const feature, version: DOMString): Boolean;

    // Introduced in DOM Level 2:

    function CreateDocumentType(const QualifiedName, PublicID,
      SystemID: DOMString): TDOMDocumentType;
    function CreateDocument(const NamespaceURI, QualifiedName: DOMString;
      doctype: TDOMDocumentType): TDOMDocument;
  end;


// -------------------------------------------------------
//   DocumentFragment
// -------------------------------------------------------

  TDOMDocumentFragment = class(TDOMNode_WithChildren)
  protected
    function GetNodeType: Integer; override;
    function GetNodeName: DOMString; override;
  public
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
  end;


// -------------------------------------------------------
//   Document
// -------------------------------------------------------
  // TODO: to be replaced by more suitable container
  TNamespaces = array of DOMString;

  TDOMDocument = class(TDOMNode_WithChildren)
  protected
    FIDList: THashTable;
    FRevision: Integer;
    FXML11: Boolean;
    FImplementation: TDOMImplementation;
    FNamespaces: TNamespaces;
    FNames: THashTable;
    FEmptyNode: TDOMElement;
    FNodeLists: THashTable;
    FMaxPoolSize: Integer;
    FPools: PNodePoolArray;
    FDocumentURI: DOMString;
    function GetDocumentElement: TDOMElement;
    function GetDocType: TDOMDocumentType;
    function GetNodeType: Integer; override;
    function GetNodeName: DOMString; override;
    function GetTextContent: DOMString; override;
    function GetOwnerDocument: TDOMDocument; override;
    procedure SetTextContent(const {%H-}value: DOMString); override;
    procedure RemoveID(Elem: TDOMElement);
    function GetChildNodeList(aNode: TDOMNode): TDOMNodeList;
    function GetElementList(aNode: TDOMNode; const nsURI, aLocalName: DOMString; UseNS: Boolean): TDOMNodeList;
    procedure NodeListDestroyed(aList: TDOMNodeList);
    function Alloc(AClass: TDOMNodeClass): TDOMNode;
  public
    function IndexOfNS(const nsURI: DOMString; AddIfAbsent: Boolean = False): Integer;
    function InsertBefore(NewChild, RefChild: TDOMNode): TDOMNode; override;
    function ReplaceChild(NewChild, OldChild: TDOMNode): TDOMNode; override;
    property DocType: TDOMDocumentType read GetDocType;
    property Impl: TDOMImplementation read FImplementation;
    property DocumentElement: TDOMElement read GetDocumentElement;

    function CreateElement(const tagName: DOMString): TDOMElement; virtual;
    function CreateElementBuf(Buf: DOMPChar; Length: Integer): TDOMElement;
    function CreateDocumentFragment: TDOMDocumentFragment;
    function CreateTextNode(const data: DOMString): TDOMText;
    function CreateTextNodeBuf(Buf: DOMPChar; Length: Integer; IgnWS: Boolean): TDOMText;
    function CreateComment(const data: DOMString): TDOMComment;
    function CreateCommentBuf(Buf: DOMPChar; Length: Integer): TDOMComment;
    function CreateCDATASection(const {%H-}data: DOMString): TDOMCDATASection;
      virtual;
    function CreateProcessingInstruction(const {%H-}target, {%H-}data: DOMString):
      TDOMProcessingInstruction; virtual;
    function CreateAttribute(const name: DOMString): TDOMAttr;
    function CreateAttributeBuf(Buf: DOMPChar; Length: Integer): TDOMAttr;
    function CreateAttributeDef(Buf: DOMPChar; Length: Integer): TDOMAttrDef;
    function CreateEntityReference(const {%H-}name: DOMString): TDOMEntityReference;
      virtual;
    function GetElementsByTagName(const tagname: DOMString): TDOMNodeList;

    // DOM level 2 methods
    function ImportNode(ImportedNode: TDOMNode; Deep: Boolean): TDOMNode;
    function CreateElementNS(const nsURI, QualifiedName: DOMString): TDOMElement;
    function CreateAttributeNS(const nsURI, QualifiedName: DOMString): TDOMAttr;
    function GetElementsByTagNameNS(const nsURI, alocalName: DOMString): TDOMNodeList;
    function GetElementById(const ElementID: DOMString): TDOMElement;
    // DOM level 3:
    property documentURI: DOMString read FDocumentURI write FDocumentURI;
    // Extensions to DOM interface:
    constructor Create;
    destructor Destroy; override;
    function AddID(Attr: TDOMAttr): Boolean;
    property Names: THashTable read FNames;
  end;

  TXMLDocument = class(TDOMDocument)
  private
    FXMLVersion: DOMString;
    procedure SetXMLVersion(const aValue: DOMString);
  public
    // These fields are extensions to the DOM interface:
    Encoding, StylesheetType, StylesheetHRef: DOMString;

    function CreateCDATASection(const data: DOMString): TDOMCDATASection; override;
    function CreateProcessingInstruction(const target, data: DOMString):
      TDOMProcessingInstruction; override;
    function CreateEntityReference(const name: DOMString): TDOMEntityReference; override;
    property XMLVersion: DOMString read FXMLVersion write SetXMLVersion;
  end;

  // This limits number of namespaces per document to 65535,
  // and prefix length to 65535, too.
  // I believe that higher values may only be found in deliberately malformed documents.
  TNamespaceInfo = packed record
    NSIndex: Word;
    PrefixLen: Word;
    QName: PHashItem;
  end;

// -------------------------------------------------------
//   Attr
// -------------------------------------------------------

  TAttrDataType = (
    dtCdata,
    dtId,
    dtIdRef,
    dtIdRefs,
    dtEntity,
    dtEntities,
    dtNmToken,
    dtNmTokens,
    dtNotation
  );

  TDOMNode_NS = class(TDOMNode_WithChildren)
  protected
    FNSI: TNamespaceInfo;
    function GetNodeName: DOMString; override;
    function GetLocalName: DOMString; override;
    function GetNamespaceURI: DOMString; override;
    function GetPrefix: DOMString; override;
    procedure SetPrefix(const Value: DOMString); override;
  public
    { Used by parser }
    procedure SetNSI(const nsUri: DOMString; ColonPos: Integer);
    function CompareName(const AName: DOMString): Integer; override;
    property NSI: TNamespaceInfo read FNSI;
  end;

  TDOMAttr = class(TDOMNode_NS)
  protected
    FOwnerElement: TDOMElement;
    FDataType: TAttrDataType;
    function  GetNodeValue: DOMString; override;
    function GetNodeType: Integer; override;
    function GetSpecified: Boolean;
    function GetIsID: Boolean;
    procedure SetNodeValue(const AValue: DOMString); override;
  public
    destructor Destroy; override;
    function CloneNode({%H-}deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
    property Name: DOMString read GetNodeName;
    property Specified: Boolean read GetSpecified;
    property Value: DOMString read GetNodeValue write SetNodeValue;
    property OwnerElement: TDOMElement read FOwnerElement;
    property IsID: Boolean read GetIsID;
    // extensions
    // TODO: this is to be replaced with DOM 3 TypeInfo
    property DataType: TAttrDataType read FDataType write FDataType;
  end;


// -------------------------------------------------------
//   Element
// -------------------------------------------------------

  TDOMElement = class(TDOMNode_NS)
  protected
    FAttributes: TDOMNamedNodeMap;
    function GetNodeType: Integer; override;
    function GetAttributes: TDOMNamedNodeMap; override;
    procedure AttachDefaultAttrs;
    function InternalLookupPrefix(const nsURI: DOMString; Original: TDOMElement): DOMString;
    procedure RestoreDefaultAttr(AttrDef: TDOMAttr);
  public
    destructor Destroy; override;
    function  CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
    function IsEmpty: Boolean; virtual;
    procedure Normalize; override;
    property  TagName: DOMString read GetNodeName;
    function  GetAttribute(const name: DOMString): DOMString;
    procedure SetAttribute(const name, value: DOMString);
    procedure RemoveAttribute(const name: DOMString);
    function  GetAttributeNode(const name: DOMString): TDOMAttr;
    function SetAttributeNode(NewAttr: TDOMAttr): TDOMAttr;
    function RemoveAttributeNode(OldAttr: TDOMAttr): TDOMAttr;
    function  GetElementsByTagName(const name: DOMString): TDOMNodeList;

    // Introduced in DOM Level 2:
    function GetAttributeNS(const nsURI, aLocalName: DOMString): DOMString;
    procedure SetAttributeNS(const nsURI, qualifiedName, value: DOMString);
    procedure RemoveAttributeNS(const nsURI, aLocalName: DOMString);
    function GetAttributeNodeNS(const nsURI, aLocalName: DOMString): TDOMAttr;
    function SetAttributeNodeNS(newAttr: TDOMAttr): TDOMAttr;
    function GetElementsByTagNameNS(const nsURI, aLocalName: DOMString): TDOMNodeList;
    function hasAttribute(const name: DOMString): Boolean;
    function hasAttributeNS(const nsURI, aLocalName: DOMString): Boolean;
    function HasAttributes: Boolean; override;
    // extension
    property AttribStrings[const Name: DOMString]: DOMString
      read GetAttribute write SetAttribute; default;
  end;


// -------------------------------------------------------
//   Text
// -------------------------------------------------------

  TDOMText = class(TDOMCharacterData)
  protected
    function GetNodeType: Integer; override;
    function GetNodeName: DOMString; override;
    procedure SetNodeValue(const aValue: DOMString); override;
  public
    function  CloneNode({%H-}deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
    function SplitText(offset: LongWord): TDOMText;
    function IsElementContentWhitespace: Boolean;
  end;


// -------------------------------------------------------
//   Comment
// -------------------------------------------------------

  TDOMComment = class(TDOMCharacterData)
  protected
    function GetNodeType: Integer; override;
    function GetNodeName: DOMString; override;
  public
    function CloneNode({%H-}deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
  end;


// -------------------------------------------------------
//   CDATASection
// -------------------------------------------------------

  TDOMCDATASection = class(TDOMText)
  protected
    function GetNodeType: Integer; override;
    function GetNodeName: DOMString; override;
  public
    function CloneNode({%H-}deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
  end;


// -------------------------------------------------------
//   DocumentType
// -------------------------------------------------------

  TDOMDocumentType = class(TDOMNode)
  protected
    FName: DOMString;
    FPublicID: DOMString;
    FSystemID: DOMString;
    FInternalSubset: DOMString;
    FEntities, FNotations: TDOMNamedNodeMap;
    function GetEntities: TDOMNamedNodeMap;
    function GetNotations: TDOMNamedNodeMap;
    function GetNodeType: Integer; override;
    function GetNodeName: DOMString; override;
  public
    destructor Destroy; override;
    property Name: DOMString read FName;
    property Entities: TDOMNamedNodeMap read GetEntities;
    property Notations: TDOMNamedNodeMap read GetNotations;
  // Introduced in DOM Level 2:
    property PublicID: DOMString read FPublicID;
    property SystemID: DOMString read FSystemID;
    property InternalSubset: DOMString read FInternalSubset;
  end;


// -------------------------------------------------------
//   Notation
// -------------------------------------------------------

  TDOMNotation = class(TDOMNode)
  protected
    FName: DOMString;
    FPublicID, FSystemID: DOMString;
    function GetNodeType: Integer; override;
    function GetNodeName: DOMString; override;
  public
    function CloneNode({%H-}deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
    property PublicID: DOMString read FPublicID;
    property SystemID: DOMString read FSystemID;
  end;


// -------------------------------------------------------
//   Entity
// -------------------------------------------------------

  TDOMEntity = class(TDOMNode_WithChildren)
  protected
    FName: DOMString;
    FPublicID, FSystemID, FNotationName: DOMString;
    function GetNodeType: Integer; override;
    function GetNodeName: DOMString; override;
  public
    function CloneNode(deep: Boolean; aCloneOwner: TDOMDocument): TDOMNode; override;
    property PublicID: DOMString read FPublicID;
    property SystemID: DOMString read FSystemID;
    property NotationName: DOMString read FNotationName;
  end;


// -------------------------------------------------------
//   EntityReference
// -------------------------------------------------------

  TDOMEntityReference = class(TDOMNode_WithChildren)
  protected
    FName: DOMString;
    function GetNodeType: Integer; override;
    function GetNodeName: DOMString; override;
  public
    function CloneNode({%H-}deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
  end;


// -------------------------------------------------------
//   ProcessingInstruction
// -------------------------------------------------------

  TDOMProcessingInstruction = class(TDOMNode)
  private
    FTarget: DOMString;
    FNodeValue: DOMString;
  protected
    function GetNodeType: Integer; override;
    function GetNodeName: DOMString; override;
    function GetNodeValue: DOMString; override;
    procedure SetNodeValue(const AValue: DOMString); override;
  public
    function CloneNode({%H-}deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
    property Target: DOMString read FTarget;
    property Data: DOMString read FNodeValue write SetNodeValue;
  end;

// Attribute declaration - Attr descendant which carries rudimentary type info
// must be severely improved while developing Level 3

  TAttrDefault = (
    adImplied,
    adDefault,
    adRequired,
    adFixed
  );

  TDOMAttrDef = class(TDOMAttr)
  protected
    FExternallyDeclared: Boolean;
    FDefault: TAttrDefault;
    FTag: Cardinal;
    FEnumeration: array of DOMString;
  public
    function AddEnumToken(Buf: DOMPChar; Len: Integer): Boolean;
    function HasEnumToken(const aValue: DOMString): Boolean;
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
    property Default: TAttrDefault read FDefault write FDefault;
    property ExternallyDeclared: Boolean read FExternallyDeclared write FExternallyDeclared;
    property Tag: Cardinal read FTag write FTag;
  end;

// TNodePool - custom memory management for TDOMNode's
// One pool manages objects of the same InstanceSize (may be of various classes)

  PExtent = ^TExtent;
  TExtent = record
    Next: PExtent;
    // following: array of TDOMNode instances
  end;

  TNodePool = class(TObject)
  private
    FCurrExtent: PExtent;
    FCurrExtentSize: Integer;
    FElementSize: Integer;
    FCurrBlock: TDOMNode;
    FFirstFree: TDOMNode;
    procedure AddExtent(AElemCount: Integer);
  public
    constructor Create(AElementSize: Integer; AElementCount: Integer = 32);
    destructor Destroy; override;
    function AllocNode(AClass: TDOMNodeClass): TDOMNode;
    procedure FreeNode(ANode: TDOMNode);
  end;


// URIs of predefined namespaces
const
  stduri_xml: DOMString = 'http://www.w3.org/XML/1998/namespace';
  stduri_xmlns: DOMString = 'http://www.w3.org/2000/xmlns/';

function StrToXMLValue(const s: string): string; // removes #0, encodes <>&'"
function XMLValueToStr(const s: string): string; // reverse of StrToXMLValue (except for invalid #0)
function EncodeLesserAndGreaterThan(const s: string): string;

// =======================================================
// =======================================================

implementation

function StrToXMLValue(const s: string): string;

  function Convert(Dst: PChar; out NewLen: PtrUInt): boolean;
  var
    h: PChar;
    l: Integer;
    NewLength: Integer;
    Src: PChar;
    i: Integer;
  begin
    Result:=false;
    NewLength:=0;
    Src:=PChar(s);
    repeat
      case Src^ of
      #0:
        if Src-PChar(s)=length(s) then
          break
        else begin
          h:=''; l:=0;
        end;
      '&': begin h:='&amp;'; l:=5; end;
      '<': begin h:='&lt;'#0; l:=4; end;
      '>': begin h:='&gt;'#0; l:=4; end;
      '"': begin h:='&quot;'#0; l:=6; end;
      '''': begin h:='&apos;'#0; l:=6; end;
      else
        if Dst<>nil then begin
          Dst^:=Src^;
          inc(Dst);
        end else
          inc(NewLength);
        inc(Src);
        continue;
      end;
      Result:=true;
      if l>0 then begin
        if Dst<>nil then begin
          for i:=1 to l do begin
            Dst^:=h^;
            inc(Dst);
            inc(h);
          end;
        end else
          inc(NewLength,l);
      end;
      inc(Src);
    until false;
    NewLen:=NewLength;
  end;

var
  NewLen: PtrUInt;
begin
  Result:=s;
  if Result='' then exit;
  if not Convert(nil,NewLen) then exit;
  SetLength(Result,NewLen);
  if NewLen=0 then exit;
  Convert(PChar(Result),NewLen);
end;

function XMLValueToStr(const s: string): string;
// convert &amp &quot &apos &lt &gt
var
  Src: PChar;
  Dst: PChar;
begin
  if Pos('&',s)<1 then exit(s);
  SetLength(Result,length(s));
  Src:=PChar(s);
  Dst:=PChar(Result);
  repeat
    case Src^ of
    #0:
      if Src-PChar(s)=length(s) then
        break
      else
        inc(Src);
    '&':
      begin
        inc(Src);
        case Src^ of
        'a':
          if (Src[1]='m') and (Src[2]='p') then begin
            inc(Src,3);
            if Src^=';' then inc(Src);
            Dst^:='&';
            inc(Dst);
            continue;
          end else if (Src[1]='p') and (Src[2]='o') and (Src[3]='s') then begin
            inc(Src,4);
            if Src^=';' then inc(Src);
            Dst^:='''';
            inc(Dst);
            continue;
          end;
        'q':
          if (Src[1]='u') and (Src[2]='o') and (Src[3]='t') then begin
            inc(Src,4);
            if Src^=';' then inc(Src);
            Dst^:='"';
            inc(Dst);
            continue;
          end;
        'l':
          if (Src[1]='t') then begin
            inc(Src,2);
            if Src^=';' then inc(Src);
            Dst^:='<';
            inc(Dst);
            continue;
          end;
        'g':
          if (Src[1]='t') then begin
            inc(Src,2);
            if Src^=';' then inc(Src);
            Dst^:='>';
            inc(Dst);
            continue;
          end;
        end;
        Dst^:='&';
        inc(Dst);
      end;
    else
      Dst^:=Src^;
      inc(Src);
      inc(Dst);
    end;
  until false;
  SetLength(Result,Dst-PChar(Result));
end;

function EncodeLesserAndGreaterThan(const s: string): string;

  function Convert(Dst: PChar; out NewLen: PtrUInt): boolean;
  var
    h: PChar;
    l: Integer;
    NewLength: Integer;
    Src: PChar;
    i: Integer;
  begin
    Result:=false;
    NewLength:=0;
    Src:=PChar(s);
    repeat
      case Src^ of
      #0:
        if Src-PChar(s)=length(s) then
          break
        else begin
          h:=''; l:=0;
        end;
      '<': begin h:='&lt;'#0; l:=4; end;
      '>': begin h:='&gt;'#0; l:=4; end;
      else
        if Dst<>nil then begin
          Dst^:=Src^;
          inc(Dst);
        end else
          inc(NewLength);
        inc(Src);
        continue;
      end;
      Result:=true;
      if l>0 then begin
        if Dst<>nil then begin
          for i:=1 to l do begin
            Dst^:=h^;
            inc(Dst);
            inc(h);
          end;
        end else
          inc(NewLength,l);
      end;
      inc(Src);
    until false;
    NewLen:=NewLength;
  end;

var
  NewLen: PtrUInt;
begin
  Result:=s;
  if Result='' then exit;
  if not Convert(nil,NewLen) then exit;
  SetLength(Result,NewLen);
  if NewLen=0 then exit;
  Convert(PChar(Result),NewLen);
end;

{ a namespace-enabled NamedNodeMap }
type
  TAttributeMap = class(TDOMNamedNodeMap)
  private
    function FindNS(nsIndex: Integer; const aLocalName: DOMString;
      out SortedIndex: LongWord): Boolean;
    function InternalRemoveNS(const nsURI, aLocalName: DOMString): TDOMNode;
  public
    function getNamedItemNS(const namespaceURI, localName: DOMString): TDOMNode; override;
    function setNamedItemNS(arg: TDOMNode): TDOMNode; override;
    function removeNamedItemNS(const namespaceURI,localName: DOMString): TDOMNode; override;
  end;

{ TDOMNodeAllChildEnumerator }

constructor TDOMNodeAllChildEnumerator.Create(Node: TDOMNode);
begin
  FNode:=Node;
  FEnd:=Node.GetNextNodeSkipChildren;
end;

function TDOMNodeAllChildEnumerator.MoveNext: boolean;
begin
  if FCurrent=nil then
    FCurrent:=FNode.GetNextNode
  else
    FCurrent:=FCurrent.GetNextNode;
  Result:=FCurrent<>FEnd;
end;

function TDOMNodeAllChildEnumerator.GetEnumerator: TDOMNodeAllChildEnumerator;
begin
  Result:=Self;
end;

{ TDOMNodeEnumerator }

constructor TDOMNodeEnumerator.Create(Node: TDOMNode);
begin
  FNode:=Node;
end;

function TDOMNodeEnumerator.MoveNext: boolean;
begin
  if FCurrent=nil then
    FCurrent:=FNode.FirstChild
  else
    FCurrent:=FCurrent.NextSibling;
  Result:=FCurrent<>nil;
end;

// -------------------------------------------------------
//   DOM Exception
// -------------------------------------------------------

constructor EDOMError.Create(ACode: Integer; const ASituation: String);
begin
  Code := ACode;
  inherited Create(Self.ClassName + ' in ' + ASituation);
end;

constructor EDOMIndexSize.Create(const ASituation: String);    // 1
begin
  inherited Create(INDEX_SIZE_ERR, ASituation);
end;

constructor EDOMHierarchyRequest.Create(const ASituation: String);    // 3
begin
  inherited Create(HIERARCHY_REQUEST_ERR, ASituation);
end;

constructor EDOMWrongDocument.Create(const ASituation: String);    // 4
begin
  inherited Create(WRONG_DOCUMENT_ERR, ASituation);
end;

constructor EDOMNotFound.Create(const ASituation: String);    // 8
begin
  inherited Create(NOT_FOUND_ERR, ASituation);
end;

constructor EDOMNotSupported.Create(const ASituation: String);    // 9
begin
  inherited Create(NOT_SUPPORTED_ERR, ASituation);
end;

constructor EDOMInUseAttribute.Create(const ASituation: String);    // 10
begin
  inherited Create(INUSE_ATTRIBUTE_ERR, ASituation);
end;

constructor EDOMInvalidState.Create(const ASituation: String);    // 11
begin
  inherited Create(INVALID_STATE_ERR, ASituation);
end;

constructor EDOMSyntax.Create(const ASituation: String);    // 12
begin
  inherited Create(SYNTAX_ERR, ASituation);
end;

constructor EDOMInvalidModification.Create(const ASituation: String);    // 13
begin
  inherited Create(INVALID_MODIFICATION_ERR, ASituation);
end;

constructor EDOMNamespace.Create(const ASituation: String);    // 14
begin
  inherited Create(NAMESPACE_ERR, ASituation);
end;

constructor EDOMInvalidAccess.Create(const ASituation: String);    // 15
begin
  inherited Create(INVALID_ACCESS_ERR, ASituation);
end;


// -------------------------------------------------------
//   Node
// -------------------------------------------------------

constructor TDOMNode.Create(AOwner: TDOMDocument);
begin
  FOwnerDocument := AOwner;
  inherited Create;
end;

destructor TDOMNode.Destroy;
begin
  if Assigned(FParentNode) then
    FParentNode.DetachChild(Self);
  inherited Destroy;
end;

procedure TDOMNode.FreeInstance;
begin
  if Assigned(FPool) then
  begin
    CleanupInstance;
    TNodePool(FPool).FreeNode(Self);
  end
  else
    inherited FreeInstance;
end;

function TDOMNode.GetNodeValue: DOMString;
begin
  Result := '';
end;

procedure TDOMNode.SetNodeValue(const AValue: DOMString);
begin
  // do nothing
end;

function TDOMNode.GetChildNodes: TDOMNodeList;
begin
  Result := FOwnerDocument.GetChildNodeList(Self);
end;

function TDOMNode.GetChildCount: SizeInt;
begin
  Result:=0;
end;

function TDOMNode.GetEnumerator: TDOMNodeEnumerator;
begin
  Result:=TDOMNodeEnumerator.Create(Self);
end;

function TDOMNode.GetEnumeratorAllChildren: TDOMNodeAllChildEnumerator;
begin
  Result:=TDOMNodeAllChildEnumerator.Create(Self);
end;

function TDOMNode.GetNextNode: TDOMNode;
begin
  Result:=FirstChild;
  if Result=nil then
    Result:=GetNextNodeSkipChildren;
end;

function TDOMNode.GetNextNodeSkipChildren: TDOMNode;
var
  Node: TDOMNode;
begin
  Result:=Self;
  repeat
    Node:=Result.NextSibling;
    if Node<>nil then exit(Node);
    Result:=Result.ParentNode;
  until Result=nil;
  Result:=nil;
end;

function TDOMNode.GetPreviousNode: TDOMNode;
var
  Node: TDOMNode;
begin
  Result:=PreviousSibling;
  if Result=nil then
    exit(ParentNode);
  Node:=Result.GetLastLeaf;
  if Node<>nil then
    Result:=Node;
end;

function TDOMNode.GetLastLeaf: TDOMNode;
var
  Node: TDOMNode;
begin
  Result:=LastChild;
  if Result=nil then exit;
  repeat
    Node:=Result.LastChild;
    if Node=nil then exit;
    Result:=Node;
  until false;
end;

function TDOMNode.GetLevel: SizeInt;
var
  Node: TDOMNode;
begin
  Result:=0;
  Node:=ParentNode;
  while Node<>nil do begin
    inc(Result);
    Node:=Node.ParentNode;
  end;
end;

function TDOMNode.GetFirstChild: TDOMNode;
begin
  Result := nil;
end;

function TDOMNode.GetLastChild: TDOMNode;
begin
  Result := nil;
end;

function TDOMNode.GetAttributes: TDOMNamedNodeMap;
begin
  Result := nil;
end;

function TDOMNode.InsertBefore(NewChild, RefChild: TDOMNode): TDOMNode;
begin
  Changing;  // merely to comply with core3/nodeinsertbefore14
  raise EDOMHierarchyRequest.Create('Node.InsertBefore');
  Result:=nil;
end;

function TDOMNode.ReplaceChild(NewChild, OldChild: TDOMNode): TDOMNode;
begin
  Changing;  // merely to comply with core3/nodereplacechild21
  raise EDOMHierarchyRequest.Create('Node.ReplaceChild');
  Result:=nil;
end;

function TDOMNode.DetachChild(OldChild: TDOMNode): TDOMNode;
begin
  // OldChild isn't in our child list
  raise EDOMNotFound.Create('Node.RemoveChild');
  Result:=nil;
end;

function TDOMNode.RemoveChild(OldChild: TDOMNode): TDOMNode;
begin
  Result := DetachChild(OldChild);
end;

function TDOMNode.AppendChild(NewChild: TDOMNode): TDOMNode;
begin
  Result := InsertBefore(NewChild, nil);
end;

function TDOMNode.HasChildNodes: Boolean;
begin
  Result := False;
end;

function TDOMNode.CloneNode(deep: Boolean): TDOMNode;
begin
  Result := CloneNode(deep, FOwnerDocument);
end;

function TDOMNode.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
// !! CreateFmt() does not set Code property !!
  raise EDOMNotSupported.Create(Format('Cloning/importing of %s is not supported', [ClassName]));
  Result:=nil;
end;

function TDOMNode.FindNode(const ANodeName: DOMString): TDOMNode;
begin
  // FIX: we have no children, hence cannot find anything
  Result := nil;
end;

function TDOMNode.GetRevision: Integer;
begin
  Result := FOwnerDocument.FRevision;
end;

function TDOMNode.IsSupported(const Feature, Version: DOMString): Boolean;
begin
  Result := FOwnerDocument.Impl.HasFeature(Feature, Version);
end;

function TDOMNode.HasAttributes: Boolean;
begin
  Result := False;
end;

procedure TDOMNode.Normalize;
var
  Child, tmp: TDOMNode;
  Txt: TDOMText;
begin
  Child := FirstChild;
  Txt := nil;

  while Assigned(Child) do
  begin
    if Child.NodeType = TEXT_NODE then
    begin
      tmp := Child.NextSibling;
      if TDOMText(Child).Data <> '' then
      begin
        if Assigned(Txt) then
        begin
          Txt.AppendData(TDOMText(Child).Data);
          // TODO: maybe should be smarter
          Exclude(Txt.FFlags, nfIgnorableWS);
        end
        else
        begin
          Txt := TDOMText(Child);
          Child := Child.NextSibling;
          Continue;
        end;
      end;
      Child.Free;
      Child := tmp;
    end
    else
    begin
      Child.Normalize;  // should be recursive!
      Child := Child.NextSibling;
      Txt := nil;
    end;
  end;
end;

function TDOMNode.GetTextContent: DOMString;
begin
  Result := NodeValue;
end;

procedure TDOMNode.SetTextContent(const AValue: DOMString);
begin
  SetNodeValue(AValue);
end;

function TDOMNode.GetNamespaceURI: DOMString;
begin
  Result := '';
end;

function TDOMNode.GetLocalName: DOMString;
begin
  Result := '';
end;

function TDOMNode.GetPrefix: DOMString;
begin
  Result := '';
end;

procedure TDOMNode.SetPrefix(const Value: DOMString);
begin
  // do nothing, override for Elements and Attributes
end;

function TDOMNode.GetOwnerDocument: TDOMDocument;
begin
  Result := FOwnerDocument;
end;

procedure TDOMNode.SetReadOnly(Value: Boolean);
var
  child: TDOMNode;
  attrs: TDOMNamedNodeMap;
  I: Integer;
begin
  if Value then
    Include(FFlags, nfReadOnly)
  else
    Exclude(FFlags, nfReadOnly);
  child := FirstChild;
  while Assigned(child) do
  begin
    child.SetReadOnly(Value);
    child := child.NextSibling;
  end;
  if HasAttributes then
  begin
    attrs := Attributes;
    for I := 0 to attrs.Length-1 do
      attrs[I].SetReadOnly(Value);
  end;
end;

procedure TDOMNode.Changing;
begin
  if (nfReadOnly in FFlags) and not (nfDestroying in FOwnerDocument.FFlags) then
    raise EDOMError.Create(NO_MODIFICATION_ALLOWED_ERR, 'Node.CheckReadOnly');
end;

function CompareDOMStrings(const s1, s2: DOMPChar; l1, l2: integer): integer;
var i: integer;
begin
  Result:=l1-l2;
  i:=0;
  while (i<l1) and (Result=0) do begin
    Result:=ord(s1[i])-ord(s2[i]);
    inc(i);
  end;
end;

// generic version (slow)
function TDOMNode.CompareName(const name: DOMString): Integer;
var
  SelfName: DOMString;
begin
  SelfName := NodeName;
  Result := CompareDOMStrings(DOMPChar(name), DOMPChar(SelfName), Length(name), Length(SelfName));
end;

// This will return nil for Entity, Notation, DocType and DocFragment's
function GetAncestorElement(n: TDOMNode): TDOMElement;
var
  parent: TDOMNode;
begin
  case n.nodeType of
    DOCUMENT_NODE:
      result := TDOMDocument(n).documentElement;
    ATTRIBUTE_NODE:
      result := TDOMAttr(n).OwnerElement;
  else
    parent := n.ParentNode;
    while Assigned(parent) and (parent.NodeType <> ELEMENT_NODE) do
      parent := parent.ParentNode;
    Result := TDOMElement(parent);
  end;  
end;

// TODO: specs prescribe to return default namespace if APrefix=null,
// but we aren't able to distinguish null from an empty string.
// This breaks level3/nodelookupnamespaceuri08 which passes an empty string.
function TDOMNode.LookupNamespaceURI(const APrefix: DOMString): DOMString;
var
  Attr: TDOMAttr;
  Map: TDOMNamedNodeMap;
  I: Integer;
begin
  Result := '';
  if Self = nil then
    Exit;
  if nodeType = ELEMENT_NODE then
  begin
    if (nfLevel2 in FFlags) and (TDOMElement(Self).Prefix = APrefix) then
    begin
      result := Self.NamespaceURI;
      Exit;
    end;
    if HasAttributes then
    begin
      Map := Attributes;
      for I := 0 to Map.Length-1 do
      begin
        Attr := TDOMAttr(Map[I]);
        // should ignore level 1 atts here
        if ((Attr.Prefix = 'xmlns') and (Attr.localName = APrefix)) or
           ((Attr.localName = 'xmlns') and (APrefix = '')) then
        begin
          result := Attr.NodeValue;
          Exit;
        end;
      end
    end;
  end;  
  result := GetAncestorElement(Self).LookupNamespaceURI(APrefix);
end;

function TDOMNode.LookupPrefix(const nsURI: DOMString): DOMString;
begin
  Result := '';
  if (nsURI = '') or (Self = nil) then
    Exit;
  if nodeType = ELEMENT_NODE then
    result := TDOMElement(Self).InternalLookupPrefix(nsURI, TDOMElement(Self))
  else
    result := GetAncestorElement(Self).LookupPrefix(nsURI);
end;

function TDOMNode.IsDefaultNamespace(const nsURI: DOMString): Boolean;
var
  Attr: TDOMAttr;
  Map: TDOMNamedNodeMap;
  I: Integer;
begin
  Result := False;
  if Self = nil then
    Exit;
  if nodeType = ELEMENT_NODE then
  begin
    if TDOMElement(Self).FNSI.PrefixLen = 0 then
    begin
      result := (nsURI = namespaceURI);
      Exit;
    end  
    else if HasAttributes then
    begin
      Map := Attributes;
      for I := 0 to Map.Length-1 do
      begin
        Attr := TDOMAttr(Map[I]);
        if Attr.LocalName = 'xmlns' then
        begin
          result := (Attr.Value = nsURI);
          Exit;
        end;
      end;
    end;
  end;
  result := GetAncestorElement(Self).IsDefaultNamespace(nsURI);
end;

function TDOMNode.GetBaseURI: DOMString;
begin
  case NodeType of
  // !! Incomplete !!
    DOCUMENT_NODE:
      result := TDOMDocument(Self).FDocumentURI;
    PROCESSING_INSTRUCTION_NODE:
      if Assigned(ParentNode) then
        result := ParentNode.GetBaseURI
      else
        result := OwnerDocument.DocumentURI;
  else
    result := '';
  end;
end;

//------------------------------------------------------------------------------

type
  TNodeTypeEnum = ELEMENT_NODE..NOTATION_NODE;
  TNodeTypeSet = set of TNodeTypeEnum;

const
  stdChildren = [TEXT_NODE, ENTITY_REFERENCE_NODE, PROCESSING_INSTRUCTION_NODE,
                 COMMENT_NODE, CDATA_SECTION_NODE, ELEMENT_NODE];

  ValidChildren: array [TNodeTypeEnum] of TNodeTypeSet = (
   stdChildren, { element }
   [TEXT_NODE, ENTITY_REFERENCE_NODE], { attribute }
   [], { text }
   [], { cdata }
   stdChildren, { ent ref }
   stdChildren, { entity }
   [], { pi }
   [], { comment }
   [ELEMENT_NODE, DOCUMENT_TYPE_NODE, PROCESSING_INSTRUCTION_NODE, COMMENT_NODE], { document }
   [], { doctype }
   stdChildren, { fragment }
   []  { notation }
  );

function TDOMNode_WithChildren.GetFirstChild: TDOMNode;
begin
  Result := FFirstChild;
end;

function TDOMNode_WithChildren.GetLastChild: TDOMNode;
begin
  Result := FLastChild;
end;

destructor TDOMNode_WithChildren.Destroy;
begin
  FreeChildren;
  FChildNodes.Free; // its destructor will zero the field
  inherited Destroy;
end;

function TDOMNode_WithChildren.InsertBefore(NewChild, RefChild: TDOMNode):
  TDOMNode;
var
  Tmp: TDOMNode;
  NewChildType: Integer;
begin
  Result := NewChild;
  NewChildType := NewChild.NodeType;

  Changing;
  if NewChild.FOwnerDocument <> FOwnerDocument then
  begin
    if (NewChildType <> DOCUMENT_TYPE_NODE) or
    (NewChild.FOwnerDocument <> nil) then
      raise EDOMWrongDocument.Create('NodeWC.InsertBefore');
  end;

  if Assigned(RefChild) and (RefChild.ParentNode <> Self) then
    raise EDOMNotFound.Create('NodeWC.InsertBefore');

  // TODO: skip checking Fragments as well? (Fragment itself cannot be in the tree)  
  if not (NewChildType in [TEXT_NODE, CDATA_SECTION_NODE, COMMENT_NODE, PROCESSING_INSTRUCTION_NODE]) and (NewChild.FirstChild <> nil) then
  begin
    Tmp := Self;
    while Assigned(Tmp) do
    begin
      if Tmp = NewChild then
        raise EDOMHierarchyRequest.Create('NodeWC.InsertBefore (cycle in tree)');
      Tmp := Tmp.ParentNode;
    end;
  end;
  if NewChild = RefChild then    // inserting node before itself is a no-op
    Exit;

  Inc(FOwnerDocument.FRevision); // invalidate nodelists

  if NewChildType = DOCUMENT_FRAGMENT_NODE then
  begin
    Tmp := NewChild.FirstChild;
    if Assigned(Tmp) then
    begin
      while Assigned(Tmp) do
      begin
        if not (Tmp.NodeType in ValidChildren[NodeType]) then
          raise EDOMHierarchyRequest.Create('NodeWC.InsertBefore');
        Tmp := Tmp.NextSibling;
      end;
    
      while Assigned(TDOMDocumentFragment(NewChild).FFirstChild) do
        InsertBefore(TDOMDocumentFragment(NewChild).FFirstChild, RefChild);
    end;
    Exit;
  end;

  if not (NewChildType in ValidChildren[NodeType]) then
    raise EDOMHierarchyRequest.Create('NodeWC.InsertBefore');

  if Assigned(NewChild.FParentNode) then
    NewChild.FParentNode.DetachChild(NewChild);

  NewChild.FNextSibling := RefChild;
  if RefChild = nil then  // append to the end
  begin
    if Assigned(FFirstChild) then
    begin
      FLastChild.FNextSibling := NewChild;
      NewChild.FPreviousSibling := FLastChild;
    end else
      FFirstChild := NewChild;
    FLastChild := NewChild;
  end
  else   // insert before RefChild
  begin
    if RefChild = FFirstChild then
      FFirstChild := NewChild
    else
    begin
      RefChild.FPreviousSibling.FNextSibling := NewChild;
      NewChild.FPreviousSibling := RefChild.FPreviousSibling;
    end;
    RefChild.FPreviousSibling := NewChild;
  end;
  NewChild.FParentNode := Self;
end;

function TDOMNode_WithChildren.ReplaceChild(NewChild, OldChild: TDOMNode):
  TDOMNode;
begin
  InsertBefore(NewChild, OldChild);
  if Assigned(OldChild) then
    RemoveChild(OldChild);
  Result := OldChild;
end;

function TDOMNode_WithChildren.DetachChild(OldChild: TDOMNode): TDOMNode;
begin
  Changing;

  if OldChild.ParentNode <> Self then
    raise EDOMNotFound.Create('NodeWC.RemoveChild');

  Inc(FOwnerDocument.FRevision); // invalidate nodelists

  if OldChild = FFirstChild then
    FFirstChild := FFirstChild.FNextSibling
  else
    OldChild.FPreviousSibling.FNextSibling := OldChild.FNextSibling;

  if OldChild = FLastChild then
    FLastChild := FLastChild.FPreviousSibling
  else
    OldChild.FNextSibling.FPreviousSibling := OldChild.FPreviousSibling;

  // Make sure removed child does not contain references to nowhere
  OldChild.FPreviousSibling := nil;
  OldChild.FNextSibling := nil;
  OldChild.FParentNode := nil;
  Result := OldChild;
end;

procedure TDOMNode_WithChildren.InternalAppend(NewChild: TDOMNode);
begin
  if Assigned(FFirstChild) then
  begin
    FLastChild.FNextSibling := NewChild;
    NewChild.FPreviousSibling := FLastChild;
  end else
    FFirstChild := NewChild;
  FLastChild := NewChild;
  NewChild.FParentNode := Self;
end;

function TDOMNode_WithChildren.HasChildNodes: Boolean;
begin
  Result := Assigned(FFirstChild);
end;

function TDOMNode_WithChildren.GetChildCount: SizeInt;
var
  Node: TDOMNode;
begin
  if FFirstChild=nil then exit(0);
  if FChildNodes<>nil then
    Result:=FChildNodes.Count
  else begin
    Result:=0;
    Node:=FFirstChild;
    while Node<>nil do begin
      inc(Result);
      Node:=Node.NextSibling;
    end;
  end;
end;


function TDOMNode_WithChildren.FindNode(const ANodeName: DOMString): TDOMNode;
begin
  Result := FFirstChild;
  while Assigned(Result) do
  begin
    if Result.CompareName(ANodeName)=0 then
      Exit;
    Result := Result.NextSibling;
  end;
end;


procedure TDOMNode_WithChildren.CloneChildren(ACopy: TDOMNode;
  ACloneOwner: TDOMDocument);
var
  node: TDOMNode;
begin
  node := FirstChild;
  while Assigned(node) do
  begin
    TDOMNode_WithChildren(ACopy).InternalAppend(node.CloneNode(True, ACloneOwner));
    node := node.NextSibling;
  end;
end;

procedure TDOMNode_WithChildren.FreeChildren;
var
  child, next: TDOMNode;
begin
  child := FFirstChild;
  while Assigned(child) do
  begin
    next := child.NextSibling;
    child.FParentNode := nil;
    child.Destroy;   // we know it's not nil, so save a call
    child := next;
  end;
  FFirstChild := nil;
  FLastChild := nil;
end;

function TDOMNode_WithChildren.GetTextContent: DOMString;
var
  child: TDOMNode;
begin
  Result := '';
  child := FFirstChild;
  // TODO: probably very slow, optimization needed
  while Assigned(child) do
  begin
    case child.NodeType of
      TEXT_NODE: if not (nfIgnorableWS in child.FFlags) then
        Result := Result + TDOMText(child).Data;
      COMMENT_NODE, PROCESSING_INSTRUCTION_NODE: ; // ignored
    else
      Result := Result + child.TextContent;
    end;
    child := child.NextSibling;
  end;
end;

procedure TDOMNode_WithChildren.SetTextContent(const AValue: DOMString);
begin
  Changing;
  while Assigned(FFirstChild) do
    DetachChild(FFirstChild);
  if AValue <> '' then
    AppendChild(FOwnerDocument.CreateTextNode(AValue));
end;

// -------------------------------------------------------
//   NodeList
// -------------------------------------------------------

constructor TDOMNodeList.Create(ANode: TDOMNode);
begin
  inherited Create;
  FNode := ANode;
  FRevision := ANode.GetRevision-1;   // force BuildList at first access
  FList := TFPList.Create;
end;

destructor TDOMNodeList.Destroy;
begin
  if (FNode is TDOMNode_WithChildren) and
    (TDOMNode_WithChildren(FNode).FChildNodes = Self) then
    TDOMNode_WithChildren(FNode).FChildNodes := nil
  else
    FNode.FOwnerDocument.NodeListDestroyed(Self);
  FList.Free;
  inherited Destroy;
end;

function TDOMNodeList.NodeFilter(aNode: TDOMNode): TFilterResult;
begin
// accept all nodes but don't allow recursion
  Result := frNorecurseTrue;
end;

procedure TDOMNodeList.BuildList;
var
  current, next: TDOMNode;
  res: TFilterResult;
begin
  FList.Clear;
  FRevision := FNode.GetRevision; // refresh

  current := FNode.FirstChild;

  while Assigned(current) do
  begin
    res := NodeFilter(current);
    if res in [frTrue, frNorecurseTrue] then
      FList.Add(current);

    next := nil;
    if res in [frTrue, frFalse] then
      next := current.FirstChild;

    if next = nil then
    begin
      while current <> FNode do
      begin
        next := current.NextSibling;
        if Assigned(next) then
          Break;
        current := current.ParentNode;
      end;
    end;
    current := next;
  end;
end;

function TDOMNodeList.GetCount: LongWord;
begin
  if FRevision <> FNode.GetRevision then
    BuildList;

  Result := FList.Count;
end;

function TDOMNodeList.GetItem(index: LongWord): TDOMNode;
begin
  if FRevision <> FNode.GetRevision then
    BuildList;

  if index < LongWord(FList.Count) then
    Result := TDOMNode(FList.List^[index])
  else
    Result := nil;
end;

{ TDOMElementList }

constructor TDOMElementList.Create(ANode: TDOMNode; const AFilter: DOMString);
begin
  inherited Create(ANode);
  filter := AFilter;
  UseFilter := filter <> '*';
end;

constructor TDOMElementList.Create(ANode: TDOMNode; const nsURI, localName: DOMString);
begin
  inherited Create(ANode);
  localNameFilter := localName;
  FMatchNS := True;
  FMatchAnyNS := (nsURI = '*');
  if not FMatchAnyNS then
    FNSIndexFilter := ANode.FOwnerDocument.IndexOfNS(nsURI);
  UseFilter := (localName <> '*');
end;

function TDOMElementList.NodeFilter(aNode: TDOMNode): TFilterResult;
var
  I, L: Integer;
begin
  Result := frFalse;
  if aNode.NodeType = ELEMENT_NODE then with TDOMElement(aNode) do
  begin
    if FMatchNS then
    begin
      if (FMatchAnyNS or (FNSI.NSIndex = Word(FNSIndexFilter))) then
      begin
        I := FNSI.PrefixLen;
        L := system.Length(FNSI.QName^.Key);
        if (not UseFilter or ((L-I = system.Length(localNameFilter)) and
          CompareMem(@FNSI.QName^.Key[I+1], DOMPChar(localNameFilter), system.Length(localNameFilter)*sizeof(DOMChar)))) then
          Result := frTrue;
      end;
    end
    else if (not UseFilter or (TagName = Filter)) then
      Result := frTrue;
  end;
end;


// -------------------------------------------------------
//   NamedNodeMap
// -------------------------------------------------------

constructor TDOMNamedNodeMap.Create(AOwner: TDOMNode; ANodeType: Integer);
begin
  inherited Create;
  FOwner := AOwner;
  FNodeType := ANodeType;
end;

destructor TDOMNamedNodeMap.Destroy;
var
  I: Integer;
begin
  FSortedList.Free;
  if FPosList<>nil then begin
    for I := FPosList.Count-1 downto 0 do
      TDOMNode(FPosList[I]).Free;
    FPosList.Free;
  end;
  inherited Destroy;
end;

function TDOMNamedNodeMap.GetSortedItem(index: LongWord): TDOMNode;
begin
  Result := TDOMNode(FSortedList.List^[index]);
end;

function TDOMNamedNodeMap.GetPosItem(index: LongWord): TDOMNode;
begin
  Result := TDOMNode(FPosList.List^[index]);
end;

function TDOMNamedNodeMap.GetLength: LongWord;
begin
  if FPosList<>nil then
    Result := FPosList.Count
  else
    Result := 0;
end;

function TDOMNamedNodeMap.FindSorted(const name: DOMString; out Index: LongWord): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  if FPosList<>nil then begin
    H := FSortedList.Count - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := TDOMNode(FSortedList.List^[I]).CompareName(name);
      if C > 0 then L := I + 1 else
      begin
        H := I - 1;
        if C = 0 then
        begin
          Result := True;
          L := I;
        end;
      end;
    end;
  end;
  Index := L;
end;

function TDOMNamedNodeMap.GetNamedItem(const name: DOMString): TDOMNode;
var
  i: Cardinal;
begin
  if FindSorted(name, i) then
    Result := TDOMNode(FSortedList.List^[i])
  else
    Result := nil;
end;

// Note: this *may* raise NOT_SUPPORTED_ERR if the document is e.g. HTML.
// This isn't checked now.
function TDOMNamedNodeMap.GetNamedItemNS(const namespaceURI, localName: DOMString): TDOMNode;
begin
  Result := nil;
end;

function TDOMNamedNodeMap.ValidateInsert(arg: TDOMNode): Integer;
var
  AttrOwner: TDOMNode;
begin
  Result := 0;
  if nfReadOnly in FOwner.FFlags then
    Result := NO_MODIFICATION_ALLOWED_ERR
  else if arg.FOwnerDocument <> FOwner.FOwnerDocument then
    Result := WRONG_DOCUMENT_ERR
  else if arg.NodeType <> FNodeType then
    Result := HIERARCHY_REQUEST_ERR
  else if (FNodeType = ATTRIBUTE_NODE) then
  begin
    AttrOwner := TDOMAttr(arg).ownerElement;
    if Assigned(AttrOwner) and (AttrOwner <> FOwner) then
      Result := INUSE_ATTRIBUTE_ERR;
  end;
end;

function TDOMNamedNodeMap.SetNamedItem(arg: TDOMNode): TDOMNode;
var
  i: Cardinal;
  Exists: Boolean;
  res: Integer;
begin
  res := ValidateInsert(arg);
  if res <> 0 then
    raise EDOMError.Create(res, 'NamedNodeMap.SetNamedItem');

  if FNodeType = ATTRIBUTE_NODE then
  begin
    TDOMAttr(arg).FOwnerElement := TDOMElement(FOwner);
    Exists := FindSorted(TDOMAttr(arg).Name, i); // optimization
  end
  else
    Exists := FindSorted(arg.NodeName, i);

  if Exists then
  begin
    Result := TDOMNode(FSortedList.List^[i]);
    if (Result <> arg) then
    begin
      if (FNodeType = ATTRIBUTE_NODE) then
        TDOMAttr(Result).FOwnerElement := nil;
      FSortedList.List^[i] := arg;
      i:=FPosList.IndexOf(Result);
      FPosList.List^[i] := arg;
    end;
    exit;
  end;
  if FSortedList=nil then FSortedList:=TFPList.Create;
  FSortedList.Insert(i, arg);
  if FPosList=nil then FPosList:=TFPList.Create;
  FPosList.Add(arg);
  Result := nil;
end;

function TDOMNamedNodeMap.SetNamedItemNS(arg: TDOMNode): TDOMNode;
begin
{ Since the map contains only namespaceless nodes (all having empty
  localName and namespaceURI properties), a namespaced arg won't match
  any of them. Therefore, add it using nodeName as key.
  Note: a namespaceless arg is another story, as it will match *any* node
  in the map. This can be considered as a flaw in specs. }
  Result := SetNamedItem(arg);
end;

function TDOMNamedNodeMap.DeleteSorted(index: LongWord): TDOMNode;
begin
  Result := TDOMNode(FSortedList.List^[index]);
  FSortedList.Delete(index);
  FPosList.Remove(Result);
  if FNodeType = ATTRIBUTE_NODE then
    TDOMAttr(Result).FOwnerElement := nil;
end;

procedure TDOMNamedNodeMap.RestoreDefault(const name: DOMString);
var
  eldef: TDOMElement;
  attrdef: TDOMAttr;
begin
  if FNodeType = ATTRIBUTE_NODE then
  begin
    if not Assigned(TDOMElement(FOwner).FNSI.QName) then  // safeguard
      Exit;
    eldef := TDOMElement(TDOMElement(FOwner).FNSI.QName^.Data);
    if Assigned(eldef) then
    begin
      // TODO: can be avoided by linking attributes directly to their defs
      attrdef := eldef.GetAttributeNode(name);
      if Assigned(attrdef) and (TDOMAttrDef(attrdef).FDefault in [adDefault, adFixed]) then
        TDOMElement(FOwner).RestoreDefaultAttr(attrdef);
    end;
  end;
end;

function TDOMNamedNodeMap.InternalRemove(const name: DOMString): TDOMNode;
var
  i: Cardinal;
begin
  Result := nil;
  if FindSorted(name, i) then
  begin
    Result := DeleteSorted(I);
    RestoreDefault(name);
  end;
end;

function TDOMNamedNodeMap.RemoveNamedItem(const name: DOMString): TDOMNode;
begin
  if nfReadOnly in FOwner.FFlags then
    raise EDOMError.Create(NO_MODIFICATION_ALLOWED_ERR, 'NamedNodeMap.RemoveNamedItem');
  Result := InternalRemove(name);
  if Result = nil then
    raise EDOMNotFound.Create('NamedNodeMap.RemoveNamedItem');
end;

function TDOMNamedNodeMap.RemoveNamedItemNS(const namespaceURI, localName: DOMString): TDOMNode;
begin
// see comments to SetNamedItemNS. Related tests are written clever enough
// in the sense they don't expect NO_MODIFICATION_ERR in first place.
  raise EDOMNotFound.Create('NamedNodeMap.RemoveNamedItemNS');
  Result := nil;
end;

{ TAttributeMap }

// Since list is kept sorted by nodeName, we must use linear search here.
// This routine is not called while parsing, so parsing speed is not lowered.
function TAttributeMap.FindNS(nsIndex: Integer; const aLocalName: DOMString;
  out SortedIndex: LongWord): Boolean;
var
  I: Integer;
  P: DOMPChar;
begin
  if FSortedList<>nil then begin
    for I := 0 to FSortedList.Count-1 do
    begin
      with TDOMAttr(FSortedList.List^[I]) do
      begin
        if nsIndex = FNSI.NSIndex then
        begin
          P := DOMPChar(FNSI.QName^.Key);
          if FNSI.PrefixLen > 1 then
            Inc(P, FNSI.PrefixLen);
          if CompareDOMStrings(DOMPChar(aLocalName), P, System.Length(aLocalName), System.Length(FNSI.QName^.Key) - FNSI.PrefixLen) = 0 then
          begin
            SortedIndex := I;
            Result := True;
            Exit;
          end;
        end;
      end;
    end;
  end;
  SortedIndex := High(SortedIndex)-1;
  Result := False;
end;

function TAttributeMap.InternalRemoveNS(const nsURI, aLocalName: DOMString): TDOMNode;
var
  i: Cardinal;
  nsIndex: Integer;
begin
  Result := nil;
  nsIndex := FOwner.FOwnerDocument.IndexOfNS(nsURI);
  if (nsIndex >= 0) and FindNS(nsIndex, aLocalName, i) then
  begin
    Result := DeleteSorted(I);
    RestoreDefault(TDOMAttr(Result).FNSI.QName^.Key);
  end;
end;

function TAttributeMap.getNamedItemNS(const namespaceURI, localName: DOMString): TDOMNode;
var
  nsIndex: Integer;
  i: LongWord;
begin
  nsIndex := FOwner.FOwnerDocument.IndexOfNS(namespaceURI);
  if (nsIndex >= 0) and FindNS(nsIndex, localName, i) then
    Result := TDOMNode(FSortedList.List^[i])
  else
    Result := nil;
end;

function TAttributeMap.setNamedItemNS(arg: TDOMNode): TDOMNode;
var
  i: LongWord;
  res: Integer;
  Exists: Boolean;
begin
  res := ValidateInsert(arg);
  if res <> 0 then
    raise EDOMError.Create(res, 'NamedNodeMap.SetNamedItemNS');

  Result := nil;
  with TDOMAttr(arg) do
  begin
    // calling LocalName is no good... but it is done once
    if FindNS(FNSI.NSIndex, localName, i) then
    begin
      Result := TDOMNode(FSortedList.List^[i]);
      FSortedList.Delete(i);
      FPosList.Remove(Result);
    end;
    // Do a non-namespace search in order to keep the list sorted on nodeName
    Exists := FindSorted(FNSI.QName^.Key, i);
    if Exists and (Result = nil) then  // case when arg has no namespace
    begin
      Result := TDOMNode(FSortedList.List^[i]);
      FSortedList.List^[i] := arg;
      i:=FPosList.IndexOf(Result);
      FPosList.List^[i] := arg;
    end
    else begin
      if FSortedList=nil then FSortedList:=TFPList.Create;
      FSortedList.Insert(i, arg);
      if FPosList=nil then FPosList:=TFPList.Create;
      FPosList.Add(arg);
    end;
  end;
  if Assigned(Result) then
    TDOMAttr(Result).FOwnerElement := nil;
  TDOMAttr(arg).FOwnerElement := TDOMElement(FOwner);
end;

function TAttributeMap.removeNamedItemNS(const namespaceURI,
  localName: DOMString): TDOMNode;
begin
  if nfReadOnly in FOwner.FFlags then
    raise EDOMError.Create(NO_MODIFICATION_ALLOWED_ERR, 'NamedNodeMap.RemoveNamedItemNS');
  Result := InternalRemoveNS(namespaceURI, localName);
  if Result = nil then
     raise EDOMNotFound.Create('NamedNodeMap.RemoveNamedItemNS');
end;

// -------------------------------------------------------
//   CharacterData
// -------------------------------------------------------

function TDOMCharacterData.GetLength: LongWord;
begin
  Result := system.Length(FNodeValue);
end;

function TDOMCharacterData.GetNodeValue: DOMString;
begin
  Result := FNodeValue;
end;

procedure TDOMCharacterData.SetNodeValue(const AValue: DOMString);
begin
  Changing;
  FNodeValue := AValue;
end;

function TDOMCharacterData.SubstringData(offset, count: LongWord): DOMString;
begin
  if offset > Length then
    raise EDOMIndexSize.Create('CharacterData.SubstringData');
  Result := Copy(FNodeValue, offset + 1, count);
end;

procedure TDOMCharacterData.AppendData(const arg: DOMString);
begin
  Changing;
  FNodeValue := FNodeValue + arg;
end;

procedure TDOMCharacterData.InsertData(offset: LongWord; const arg: DOMString);
begin
  Changing;
  if offset > Length then
    raise EDOMIndexSize.Create('CharacterData.InsertData');
  Insert(arg, FNodeValue, offset+1);
end;

procedure TDOMCharacterData.DeleteData(offset, count: LongWord);
begin
  Changing;
  if offset > Length then
    raise EDOMIndexSize.Create('CharacterData.DeleteData');
  Delete(FNodeValue, offset+1, count);
end;

procedure TDOMCharacterData.ReplaceData(offset, count: LongWord; const arg: DOMString);
begin
  DeleteData(offset, count);
  InsertData(offset, arg);
end;


// -------------------------------------------------------
//   DocumentFragmet
// -------------------------------------------------------

function TDOMDocumentFragment.GetNodeType: Integer;
begin
  Result := DOCUMENT_FRAGMENT_NODE;
end;

function TDOMDocumentFragment.GetNodeName: DOMString;
begin
  Result := '#document-fragment';
end;

function TDOMDocumentFragment.CloneNode(deep: Boolean; aCloneOwner: TDOMDocument): TDOMNode;
begin
  Result := aCloneOwner.CreateDocumentFragment;
  if deep then
    CloneChildren(Result, aCloneOwner);
end;

// -------------------------------------------------------
//   DOMImplementation
// -------------------------------------------------------

{ if nsIdx = -1, checks only the name. Otherwise additionally checks if the prefix is
  valid for standard namespace specified by nsIdx. 
  Non-negative return value is Pos(':', QName), negative is DOM error code. }
function CheckQName(const QName: DOMString; nsIdx: Integer; Xml11: Boolean): Integer;
var
  I, L: Integer;
begin
  if not IsXmlName(QName, Xml11) then
  begin
    Result := -INVALID_CHARACTER_ERR;
    Exit;
  end;

  L := Length(QName);
  Result := Pos(DOMChar(':'), QName);
  if Result > 0 then
  begin
    for I := Result+1 to L-1 do  // check for second colon (Use IndexWord?)
      if QName[I] = ':' then
      begin
        Result := -NAMESPACE_ERR;
        Exit;
      end;
    // Name validity has already been checked by IsXmlName() call above.  
    // So just check that colon isn't first or last char, and that it is follwed by NameStartChar.
    if ((Result = 1) or (Result = L) or not IsXmlName(@QName[Result+1], 1, Xml11)) then
    begin
      Result := -NAMESPACE_ERR;
      Exit;
    end;
  end;
  if nsIdx < 0 then Exit;
  // QName contains prefix, but no namespace
  if ((nsIdx = 0) and (Result > 0)) or
  // Bad usage of 'http://www.w3.org/2000/xmlns/'
  ((((L = 5) or (Result = 6)) and (Pos(DOMString('xmlns'), QName) = 1)) <> (nsIdx = 2)) or
  // Bad usage of 'http://www.w3.org/XML/1998/namespace'
  ((Result = 4) and (Pos(DOMString('xml'), QName) = 1) and (nsIdx <> 1)) then
    Result := -NAMESPACE_ERR;
end;

function TDOMImplementation.HasFeature(const feature, version: DOMString):
  Boolean;
var
  s: string;
begin
  s := feature;   // force Ansi, features do not contain non-ASCII chars
  Result := (SameText(s, 'XML') and ((version = '') or (version = '1.0') or (version = '2.0'))) or
            (SameText(s, 'Core') and ((version = '') or (version = '2.0')));

end;

function TDOMImplementation.CreateDocumentType(const QualifiedName, PublicID,
  SystemID: DOMString): TDOMDocumentType;
var
  res: Integer;
begin
  res := CheckQName(QualifiedName, -1, False);
  if res < 0 then
    raise EDOMError.Create(-res, 'Implementation.CreateDocumentType');
  Result := TDOMDocumentType.Create(nil);
  Result.FName := QualifiedName;

  // DOM does not restrict PublicID without SystemID (unlike XML spec)
  Result.FPublicID := PublicID;
  Result.FSystemID := SystemID;
end;

function TDOMImplementation.CreateDocument(const NamespaceURI,
  QualifiedName: DOMString; doctype: TDOMDocumentType): TDOMDocument;
var
  Root: TDOMNode;
begin
  if Assigned(doctype) and Assigned(doctype.OwnerDocument) then
    raise EDOMWrongDocument.Create('Implementation.CreateDocument');
  Result := TXMLDocument.Create;
  Result.FImplementation := Self;
  try
    if Assigned(doctype) then
    begin
      Doctype.FOwnerDocument := Result;
      Result.AppendChild(doctype);
    end;
    Root := Result.CreateElementNS(NamespaceURI, QualifiedName);
    Result.AppendChild(Root);
  except
    Result.Free;
    raise;
  end;
end;


// -------------------------------------------------------
//   Document
// -------------------------------------------------------

constructor TDOMDocument.Create;
begin
  inherited Create(nil);
  FOwnerDocument := Self;
  FMaxPoolSize := (TDOMAttr.InstanceSize + sizeof(Pointer)-1) and not (sizeof(Pointer)-1) + sizeof(Pointer);
  FPools := AllocMem(FMaxPoolSize);
  FNames := THashTable.Create(256, True);
  SetLength(FNamespaces, 3);
  // Namespace #0 should always be an empty string
  FNamespaces[1] := stduri_xml;
  FNamespaces[2] := stduri_xmlns;
  FEmptyNode := TDOMElement.Create(Self);
  FNodeLists := THashTable.Create(32, True);
end;

destructor TDOMDocument.Destroy;
var
  i: Integer;
begin
  Include(FFlags, nfDestroying);
  FreeAndNil(FIDList);   // set to nil before starting destroying children
  FNodeLists.Free;
  FEmptyNode.Free;
  inherited Destroy;
  for i := 0 to (FMaxPoolSize div sizeof(TNodePool))-1 do
    FPools^[i].Free;
  FreeMem(FPools);
  FNames.Free;           // free the nametable after inherited has destroyed the children
                         // (because children reference the nametable)
end;

function TDOMDocument.Alloc(AClass: TDOMNodeClass): TDOMNode;
var
  pp: TNodePool;
  size: Integer;
begin
  size := (AClass.InstanceSize + sizeof(Pointer)-1) and not (sizeof(Pointer)-1);
  if size > FMaxPoolSize then
  begin
    Result := TDOMNode(AClass.NewInstance);
    Exit;
  end;

  pp := FPools^[size div sizeof(TNodePool)];
  if pp = nil then
  begin
    pp := TNodePool.Create(size);
    FPools^[size div sizeof(TNodePool)] := pp;
  end;
  Result := pp.AllocNode(AClass);
end;

function TDOMDocument.AddID(Attr: TDOMAttr): Boolean;
var
  ID: DOMString;
  Exists: Boolean;
  p: PHashItem;
begin
  if FIDList = nil then
    FIDList := THashTable.Create(256, False);

  ID := Attr.Value;
  p := FIDList.FindOrAdd(DOMPChar(ID), Length(ID), Exists);
  Result := not Exists;
  if Result then
    p^.Data := Attr.OwnerElement;
end;

// This shouldn't be called if document has no IDs,
// or when it is being destroyed
// TODO: This could be much faster if removing ID happens
// upon modification of corresponding attribute value.

procedure TDOMDocument.RemoveID(Elem: TDOMElement);
begin
  FIDList.RemoveData(Elem);
end;

function TDOMDocument.GetNodeType: Integer;
begin
  Result := DOCUMENT_NODE;
end;

function TDOMDocument.GetNodeName: DOMString;
begin
  Result := '#document';
end;

function TDOMDocument.GetTextContent: DOMString;
begin
  Result := '';
end;

procedure TDOMDocument.SetTextContent(const value: DOMString);
begin
  // Document ignores setting TextContent
end;

function TDOMDocument.GetOwnerDocument: TDOMDocument;
begin
  Result := nil;
end;

function TDOMDocument.InsertBefore(NewChild, RefChild: TDOMNode): TDOMNode;
var
  nType: Integer;
begin
  nType := NewChild.NodeType;
  if ((nType = ELEMENT_NODE) and Assigned(DocumentElement)) or
     ((nType = DOCUMENT_TYPE_NODE) and Assigned(DocType)) then
       raise EDOMHierarchyRequest.Create('Document.InsertBefore');
  Result := inherited InsertBefore(NewChild, RefChild);
end;

function TDOMDocument.ReplaceChild(NewChild, OldChild: TDOMNode): TDOMNode;
var
  nType: Integer;
begin
  nType := NewChild.NodeType;
  if ((nType = ELEMENT_NODE) and (OldChild = DocumentElement)) or   // root can be replaced by another element
     ((nType = DOCUMENT_TYPE_NODE) and (OldChild = DocType)) then   // and so can be DTD
  begin
    inherited InsertBefore(NewChild, OldChild);
    Result := RemoveChild(OldChild);
  end
  else
    Result := inherited ReplaceChild(NewChild, OldChild);
end;

function TDOMDocument.GetDocumentElement: TDOMElement;
var
  node: TDOMNode;
begin
  node := FFirstChild;
  while Assigned(node) and (node.NodeType <> ELEMENT_NODE) do
    node := node.NextSibling;
  Result := TDOMElement(node);
end;

function TDOMDocument.GetDocType: TDOMDocumentType;
var
  node: TDOMNode;
begin
  node := FFirstChild;
  while Assigned(node) and (node.NodeType <> DOCUMENT_TYPE_NODE) do
    node := node.NextSibling;
  Result := TDOMDocumentType(node);
end;

function TDOMDocument.CreateElement(const tagName: DOMString): TDOMElement;
begin
  if not IsXmlName(tagName, FXML11) then
    raise EDOMError.Create(INVALID_CHARACTER_ERR, 'DOMDocument.CreateElement');
  TDOMNode(Result) := Alloc(TDOMElement);
  Result.Create(Self);
  Result.FNSI.QName := FNames.FindOrAdd(DOMPChar(tagName), Length(tagName));
  Result.AttachDefaultAttrs;
end;

function TDOMDocument.CreateElementBuf(Buf: DOMPChar; Length: Integer): TDOMElement;
begin
  TDOMNode(Result) := Alloc(TDOMElement);
  Result.Create(Self);
  Result.FNSI.QName := FNames.FindOrAdd(Buf, Length);
end;

function TDOMDocument.CreateDocumentFragment: TDOMDocumentFragment;
begin
  TDOMNode(Result) := Alloc(TDOMDocumentFragment);
  Result.Create(Self);
end;

function TDOMDocument.CreateTextNode(const data: DOMString): TDOMText;
begin
  TDOMNode(Result) := Alloc(TDOMText);
  Result.Create(Self);
  Result.FNodeValue := data;
end;

function TDOMDocument.CreateTextNodeBuf(Buf: DOMPChar; Length: Integer; IgnWS: Boolean): TDOMText;
begin
  TDOMNode(Result) := Alloc(TDOMText);
  Result.Create(Self);
  SetString(Result.FNodeValue, Buf, Length);
  if IgnWS then
    Include(Result.FFlags, nfIgnorableWS);
end;


function TDOMDocument.CreateComment(const data: DOMString): TDOMComment;
begin
  TDOMNode(Result) := Alloc(TDOMComment);
  Result.Create(Self);
  Result.FNodeValue := data;
end;

function TDOMDocument.CreateCommentBuf(Buf: DOMPChar; Length: Integer): TDOMComment;
begin
  TDOMNode(Result) := Alloc(TDOMComment);
  Result.Create(Self);
  SetString(Result.FNodeValue, Buf, Length);
end;

function TDOMDocument.CreateCDATASection(const data: DOMString):
  TDOMCDATASection;
begin
  raise EDOMNotSupported.Create('DOMDocument.CreateCDATASection');
  Result:=nil;
end;

function TDOMDocument.CreateProcessingInstruction(const target,
  data: DOMString): TDOMProcessingInstruction;
begin
  raise EDOMNotSupported.Create('DOMDocument.CreateProcessingInstruction');
  Result:=nil;
end;

function TDOMDocument.CreateAttribute(const name: DOMString): TDOMAttr;
begin
  if not IsXmlName(name, FXML11) then
    raise EDOMError.Create(INVALID_CHARACTER_ERR, 'DOMDocument.CreateAttribute');
  TDOMNode(Result) := Alloc(TDOMAttr);
  Result.Create(Self);
  Result.FNSI.QName := FNames.FindOrAdd(DOMPChar(name), Length(name));
  Include(Result.FFlags, nfSpecified);
end;

function TDOMDocument.CreateAttributeBuf(Buf: DOMPChar; Length: Integer): TDOMAttr;
begin
  TDOMNode(Result) := Alloc(TDOMAttr);
  Result.Create(Self);
  Result.FNSI.QName := FNames.FindOrAdd(buf, Length);
  Include(Result.FFlags, nfSpecified);
end;

function TDOMDocument.CreateAttributeDef(Buf: DOMPChar; Length: Integer): TDOMAttrDef;
begin
// not using custom allocation here
  Result := TDOMAttrDef.Create(Self);
  Result.FNSI.QName := FNames.FindOrAdd(Buf, Length);
end;

function TDOMDocument.CreateEntityReference(const name: DOMString):
  TDOMEntityReference;
begin
  raise EDOMNotSupported.Create('DOMDocument.CreateEntityReference');
  Result:=nil;
end;

function TDOMDocument.GetChildNodeList(aNode: TDOMNode): TDOMNodeList;
begin
  if not (aNode is TDOMNode_WithChildren) then
    aNode := FEmptyNode;
  Result := TDOMNode_WithChildren(aNode).FChildNodes;
  if Result = nil then
  begin
    Result := TDOMNodeList.Create(aNode);
    TDOMNode_WithChildren(aNode).FChildNodes := Result;
  end;
end;

function TDOMDocument.GetElementList(aNode: TDOMNode; const nsURI, aLocalName: DOMString;
  UseNS: Boolean): TDOMNodeList;
var
  L: Integer;
  Key, P: DOMPChar;
  Item: PHashItem;
begin
  L := (sizeof(Pointer) div sizeof(DOMChar)) + Length(aLocalName);
  if UseNS then
    Inc(L, Length(nsURI)+1);
  GetMem(Key, L*sizeof(DOMChar));
  try
    // compose the key for hashing
    P := Key;
    PPointer(P)^ := aNode;
    Inc(PPointer(P));
    Move(DOMPChar(aLocalName)^, P^, Length(aLocalName)*sizeof(DOMChar));
    if UseNS then
    begin
      Inc(P, Length(aLocalName));
      P^ := #12; Inc(P);  // separator -- diff ('foo','bar') from 'foobar'
      Move(DOMPChar(nsURI)^, P^, Length(nsURI)*sizeof(DOMChar));
    end;
    // try finding in the hashtable
    Item := FNodeLists.FindOrAdd(Key, L);
    Result := TDOMNodeList(Item^.Data);
    if Result = nil then
    begin
      if UseNS then
        Result := TDOMElementList.Create(aNode, nsURI, aLocalName)
      else
        Result := TDOMElementList.Create(aNode, aLocalName);
      Item^.Data := Result;
    end;
  finally
    FreeMem(Key);
  end;
end;

function TDOMDocument.GetElementsByTagName(const tagname: DOMString): TDOMNodeList;
begin
  Result := GetElementList(Self, '', tagname, False);
end;

function TDOMDocument.GetElementsByTagNameNS(const nsURI, aLocalName: DOMString): TDOMNodeList;
begin
  Result := GetElementList(Self, nsURI, aLocalName, True);
end;

{ This is linear hence slow. However:
  - if user code frees each nodelist ASAP, there are only few items in the hashtable
  - if user code does not free nodelists, this is not called at all.
}
procedure TDOMDocument.NodeListDestroyed(aList: TDOMNodeList);
begin
  if not (nfDestroying in FFlags) then
    FNodeLists.RemoveData(aList);
end;

function TDOMDocument.CreateAttributeNS(const nsURI,
  QualifiedName: DOMString): TDOMAttr;
var
  idx, PrefIdx: Integer;
begin
  idx := IndexOfNS(nsURI, True);
  PrefIdx := CheckQName(QualifiedName, idx, FXml11);
  if PrefIdx < 0 then
    raise EDOMError.Create(-PrefIdx, 'Document.CreateAttributeNS');
  TDOMNode(Result) := Alloc(TDOMAttr);
  Result.Create(Self);
  Result.FNSI.QName := FNames.FindOrAdd(DOMPChar(QualifiedName), Length(QualifiedName));
  Result.FNSI.NSIndex := Word(idx);
  Result.FNSI.PrefixLen := Word(PrefIdx);
  Include(Result.FFlags, nfLevel2);
  Include(Result.FFlags, nfSpecified);
end;

function TDOMDocument.CreateElementNS(const nsURI,
  QualifiedName: DOMString): TDOMElement;
var
  idx, PrefIdx: Integer;
begin
  idx := IndexOfNS(nsURI, True);
  PrefIdx := CheckQName(QualifiedName, idx, FXml11);
  if PrefIdx < 0 then
    raise EDOMError.Create(-PrefIdx, 'Document.CreateElementNS');
  TDOMNode(Result) := Alloc(TDOMElement);
  Result.Create(Self);
  Result.FNSI.QName := FNames.FindOrAdd(DOMPChar(QualifiedName), Length(QualifiedName));
  Result.FNSI.NSIndex := Word(idx);
  Result.FNSI.PrefixLen := Word(PrefIdx);
  Include(Result.FFlags, nfLevel2);
  Result.AttachDefaultAttrs;
end;

function TDOMDocument.GetElementById(const ElementID: DOMString): TDOMElement;
begin
  Result := nil;
  if Assigned(FIDList) then
    Result := TDOMElement(FIDList.Get(DOMPChar(ElementID), Length(ElementID)));
end;

function TDOMDocument.ImportNode(ImportedNode: TDOMNode;
  Deep: Boolean): TDOMNode;
begin
  Result := ImportedNode.CloneNode(Deep, Self);
end;

function TDOMDocument.IndexOfNS(const nsURI: DOMString; AddIfAbsent: Boolean): Integer;
var
  I: Integer;
begin
  // TODO: elaborate implementation
  for I := 0 to Length(FNamespaces)-1 do
    if FNamespaces[I] = nsURI then
    begin
      Result := I;
      Exit;
    end;
  if AddIfAbsent then
  begin
    Result := Length(FNamespaces);
    SetLength(FNamespaces, Result+1);
    FNamespaces[Result] := nsURI;
  end
  else
    Result := -1;
end;


function TXMLDocument.CreateCDATASection(const data: DOMString):
  TDOMCDATASection;
begin
  TDOMNode(Result) := Alloc(TDOMCDATASection);
  Result.Create(Self);
  Result.FNodeValue := data;
end;

function TXMLDocument.CreateProcessingInstruction(const target,
  data: DOMString): TDOMProcessingInstruction;
begin
  if not IsXmlName(target, FXML11) then
    raise EDOMError.Create(INVALID_CHARACTER_ERR, 'XMLDocument.CreateProcessingInstruction');
  TDOMNode(Result) := Alloc(TDOMProcessingInstruction);
  Result.Create(Self);
  Result.FTarget := target;
  Result.FNodeValue := data;
end;

function TXMLDocument.CreateEntityReference(const name: DOMString):
  TDOMEntityReference;
var
  dType: TDOMDocumentType;
  ent: TDOMEntity;
begin
  if not IsXmlName(name, FXML11) then
    raise EDOMError.Create(INVALID_CHARACTER_ERR, 'XMLDocument.CreateEntityReference');
  TDOMNode(Result) := Alloc(TDOMEntityReference);
  Result.Create(Self);
  Result.FName := name;
  dType := DocType;
  if Assigned(dType) then
  begin
    TDOMNode(ent) := dType.Entities.GetNamedItem(name);
    if Assigned(ent) then
      ent.CloneChildren(Result, Self);
  end;
  Result.SetReadOnly(True);
end;

procedure TXMLDocument.SetXMLVersion(const aValue: DOMString);
begin
  FXMLVersion := aValue;
  FXML11 := (aValue = '1.1');
end;

{ TDOMNode_NS }

function TDOMNode_NS.GetNodeName: DOMString;
begin
  // Because FNSI.QName is not set by the TDOMNode itself, but is set by
  // other classes/functions, it is necessary to check if FNSQ.QName is
  // assigned.
  if assigned(FNSI.QName) then
    Result := FNSI.QName^.Key
  else
    Result := '';
end;

function TDOMNode_NS.GetLocalName: DOMString;
begin
  if nfLevel2 in FFlags then
    Result := Copy(FNSI.QName^.Key, FNSI.PrefixLen+1, MaxInt)
  else
    Result := '';
end;

function TDOMNode_NS.GetNamespaceURI: DOMString;
begin
  Result := FOwnerDocument.FNamespaces[FNSI.NSIndex];
end;

function TDOMNode_NS.GetPrefix: DOMString;
begin
  if FNSI.PrefixLen < 2 then
    Result := ''
  else
    Result := Copy(FNSI.QName^.Key, 1, FNSI.PrefixLen-1);
end;

procedure TDOMNode_NS.SetPrefix(const Value: DOMString);
var
  NewName: DOMString;
begin
  Changing;
  if not IsXmlName(Value, FOwnerDocument.FXml11) then
    raise EDOMError.Create(INVALID_CHARACTER_ERR, 'Node.SetPrefix');

  if (Pos(DOMChar(':'), Value) > 0) or not (nfLevel2 in FFlags) or
    ((Value = 'xml') and (FNSI.NSIndex <> 1)) or
    ((ClassType = TDOMAttr) and  // BAD!
    ((Value = 'xmlns') and (FNSI.NSIndex <> 2)) or (FNSI.QName^.Key = 'xmlns')) then
    raise EDOMNamespace.Create('Node.SetPrefix');

  // TODO: rehash properly
  NewName := Value + ':' + Copy(FNSI.QName^.Key, FNSI.PrefixLen+1, MaxInt);
  FNSI.QName := FOwnerDocument.FNames.FindOrAdd(DOMPChar(NewName), Length(NewName));
  FNSI.PrefixLen := Length(Value)+1;
end;

function TDOMNode_NS.CompareName(const AName: DOMString): Integer;
begin
  Result := CompareDOMStrings(DOMPChar(AName), DOMPChar(NodeName), Length(AName), Length(NodeName));
end;

procedure TDOMNode_NS.SetNSI(const nsUri: DOMString; ColonPos: Integer);
begin
  FNSI.NSIndex := FOwnerDocument.IndexOfNS(nsURI, True);
  FNSI.PrefixLen := ColonPos;
  Include(FFlags, nfLevel2);
end;

// -------------------------------------------------------
//   Attr
// -------------------------------------------------------

function TDOMAttr.GetNodeType: Integer;
begin
  Result := ATTRIBUTE_NODE;
end;

destructor TDOMAttr.Destroy;
begin
  if Assigned(FOwnerElement) and not (nfDestroying in FOwnerElement.FFlags) then
  // TODO: This may raise NOT_FOUND_ERR in case something's really wrong
    FOwnerElement.RemoveAttributeNode(Self);
  inherited Destroy;
end;

function TDOMAttr.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  // Cloned attribute is always specified and carries its children
  if nfLevel2 in FFlags then
    Result := ACloneOwner.CreateAttributeNS(namespaceURI, NodeName)
  else
    Result := ACloneOwner.CreateAttribute(NodeName);
  TDOMAttr(Result).FDataType := FDataType;
  CloneChildren(Result, ACloneOwner);
end;

function TDOMAttr.GetNodeValue: DOMString;
begin
  Result := GetTextContent;
  if FDataType <> dtCdata then
    NormalizeSpaces(Result);
end;

procedure TDOMAttr.SetNodeValue(const AValue: DOMString);
begin
  SetTextContent(AValue);
  Include(FFlags, nfSpecified);
end;

function TDOMAttr.GetSpecified: Boolean;
begin
  Result := nfSpecified in FFlags;
end;

function TDOMAttr.GetIsID: Boolean;
begin
  Result := FDataType = dtID;
end;

// -------------------------------------------------------
//   Element
// -------------------------------------------------------

function TDOMElement.GetNodeType: Integer;
begin
  Result := ELEMENT_NODE;
end;

destructor TDOMElement.Destroy;
begin
  Include(FFlags, nfDestroying);
  if Assigned(FOwnerDocument.FIDList) then
    FOwnerDocument.RemoveID(Self);
  FreeAndNil(FAttributes);
  inherited Destroy;
end;

function TDOMElement.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
var
  i: Integer;
  Attr, AttrClone: TDOMAttr;
begin
  if ACloneOwner <> FOwnerDocument then
  begin
    // Importing has to go the hard way...
    if nfLevel2 in FFlags then
      Result := ACloneOwner.CreateElementNS(NamespaceURI, NodeName)
    else
      Result := ACloneOwner.CreateElement(NodeName);
    if Assigned(FAttributes) then
    begin
      for i := 0 to FAttributes.Length - 1 do
      begin
        Attr := TDOMAttr(FAttributes[i]);
        // destroy defaulted attributes (if any), it is safe because caller had not seen them yet
        if Attr.Specified then
          TDOMElement(Result).SetAttributeNode(TDOMAttr(Attr.CloneNode(True, ACloneOwner))).Free;
      end;
    end;
  end
  else   // Cloning may cheat a little bit.
  begin
    Result := FOwnerDocument.Alloc(TDOMElement);
    TDOMElement(Result).Create(FOwnerDocument);
    TDOMElement(Result).FNSI := FNSI;
    if nfLevel2 in FFlags then
      Include(Result.FFlags, nfLevel2);
    if Assigned(FAttributes) then
    begin
      // clone all attributes, but preserve nfSpecified flag
      for i := 0 to FAttributes.Length - 1 do
      begin
        Attr := TDOMAttr(FAttributes[i]);
        AttrClone := TDOMAttr(Attr.CloneNode(True, ACloneOwner));
        if not Attr.Specified then
          Exclude(AttrClone.FFlags, nfSpecified);
        TDOMElement(Result).SetAttributeNode(AttrClone);
      end;
    end;
  end;
  if deep then
    CloneChildren(Result, ACloneOwner);
end;

function TDOMElement.IsEmpty: boolean;
begin
  Result:=(FAttributes=nil) or (FAttributes.Length=0);
end;

procedure TDOMElement.AttachDefaultAttrs;
var
  eldef: TDOMElement;
  attrdef: TDOMAttrDef;
  I: Integer;
begin
  if not Assigned(FNSI.QName) then     // safeguard
    Exit;
  eldef := TDOMElement(FNSI.QName^.Data);
  if Assigned(eldef) and Assigned(eldef.FAttributes) then
  begin
    for I := 0 to eldef.FAttributes.Length-1 do
    begin
      attrdef := TDOMAttrDef(eldef.FAttributes[I]);
      if attrdef.FDefault in [adDefault, adFixed] then
        RestoreDefaultAttr(attrdef);
    end;
  end;
end;

function TDOMElement.InternalLookupPrefix(const nsURI: DOMString; Original: TDOMElement): DOMString;
var
  I: Integer;
  Attr: TDOMAttr;
begin
  result := '';
  if Self = nil then
    Exit;
  if (nfLevel2 in FFlags) and (namespaceURI = nsURI) and (FNSI.PrefixLen > 0) then
  begin
    Result := Prefix;
    if Original.LookupNamespaceURI(result) = nsURI then
      Exit;
  end;
  if Assigned(FAttributes) then
  begin
    for I := 0 to FAttributes.Length-1 do
    begin
      Attr := TDOMAttr(FAttributes[I]);
      if (Attr.Prefix = 'xmlns') and (Attr.Value = nsURI) then
      begin
        result := Attr.LocalName;
        if Original.LookupNamespaceURI(result) = nsURI then
          Exit;
      end;
    end;
  end;
  result := GetAncestorElement(Self).InternalLookupPrefix(nsURI, Original);
end;

procedure TDOMElement.RestoreDefaultAttr(AttrDef: TDOMAttr);
var
  Attr: TDOMAttr;
  ColonPos: Integer;
  AttrName, nsuri: DOMString;
begin
  Attr := TDOMAttr(AttrDef.CloneNode(True));
  AttrName := Attr.Name;
  ColonPos := Pos(DOMChar(':'), AttrName);
  if Pos(DOMString('xmlns'), AttrName) = 1 then
  begin
    if (Length(AttrName) = 5) or (ColonPos = 6) then
      Attr.SetNSI(stduri_xmlns, ColonPos);
  end
  else if ColonPos > 0 then
  begin
    if (ColonPos = 4) and (Pos(DOMString('xml'), AttrName) = 1) then
      Attr.SetNSI(stduri_xml, 4)
    else
    begin
      nsuri := LookupNamespaceURI(Copy(AttrName, 1, ColonPos-1));
      // TODO: what if prefix isn't defined?
      Attr.SetNSI(nsuri, ColonPos);
    end
  end;
  // TODO: this is cheat, should look at config['namespaces'] instead.
  // revisit when it is implemented.
  if nfLevel2 in FFlags then
    Include(Attr.FFlags, nfLevel2);
  // There should be no matching attribute at this point, so non-namespace method is ok
  SetAttributeNode(Attr);
end;

procedure TDOMElement.Normalize;
var
  I: Integer;
begin
  if Assigned(FAttributes) then
    for I := 0 to FAttributes.Length - 1 do
      FAttributes[I].Normalize;
  inherited Normalize;    
end;

function TDOMElement.GetAttributes: TDOMNamedNodeMap;
begin
  if FAttributes=nil then
    FAttributes := TAttributeMap.Create(Self, ATTRIBUTE_NODE);
  Result := FAttributes;
end;

function TDOMElement.GetAttribute(const name: DOMString): DOMString;
var
  Attr: TDOMNode;
begin
  SetLength(Result, 0);
  if Assigned(FAttributes) then
  begin
    Attr := FAttributes.GetNamedItem(name);
    if Assigned(Attr) then
      Result := Attr.NodeValue;
  end;
end;

function TDOMElement.GetAttributeNS(const nsURI, aLocalName: DOMString): DOMString;
var
  Attr: TDOMNode;
begin
  SetLength(Result, 0);
  if Assigned(FAttributes) then
  begin
    Attr := FAttributes.GetNamedItemNS(nsURI, aLocalName);
    if Assigned(Attr) then
      Result := Attr.NodeValue;
  end;
end;

procedure TDOMElement.SetAttribute(const name, value: DOMString);
var
  I: Cardinal;
  Attr: TDOMAttr;
begin
  Changing;
  if Attributes.FindSorted(name, I) then
    Attr := Attributes.SortedItem[I] as TDOMAttr
  else
  begin
    Attr := FOwnerDocument.CreateAttribute(name);
    Attr.FOwnerElement := Self;
    if FAttributes.FSortedList=nil then FAttributes.FSortedList:=TFPList.Create;
    FAttributes.FSortedList.Insert(I, Attr);
    if FAttributes.FPosList=nil then FAttributes.FPosList:=TFPList.Create;
    FAttributes.FPosList.Add(Attr);
  end;
  Attr.NodeValue := value;
end;

procedure TDOMElement.RemoveAttribute(const name: DOMString);
begin
  Changing;
// (note) NamedNodeMap.RemoveNamedItem can raise NOT_FOUND_ERR and we should not.
  if Assigned(FAttributes) then
    FAttributes.InternalRemove(name).Free;
end;

procedure TDOMElement.RemoveAttributeNS(const nsURI,
  aLocalName: DOMString);
begin
  Changing;
  if Assigned(FAttributes) then
    TAttributeMap(FAttributes).InternalRemoveNS(nsURI, aLocalName).Free;
end;

procedure TDOMElement.SetAttributeNS(const nsURI, qualifiedName,
  value: DOMString);
var
  I: Cardinal;
  Attr: TDOMAttr;
  idx, prefIdx: Integer;
begin
  Changing;
  idx := FOwnerDocument.IndexOfNS(nsURI, True);
  prefIdx := CheckQName(qualifiedName, idx, FOwnerDocument.FXml11);
  if prefIdx < 0 then
    raise EDOMError.Create(-prefIdx, 'Element.SetAttributeNS');

  if TAttributeMap(Attributes).FindNS(idx, Copy(qualifiedName, prefIdx+1, MaxInt), I) then
  begin
    Attr := TDOMAttr(FAttributes[I]);
    // need to reinsert because the nodeName may change
    FAttributes.FPosList.Remove(FAttributes.FSortedList.List^[i]);
    FAttributes.FSortedList.Delete(I);
  end
  else
  begin
    TDOMNode(Attr) := FOwnerDocument.Alloc(TDOMAttr);
    Attr.Create(FOwnerDocument);
    Attr.FOwnerElement := Self;
    Attr.FNSI.NSIndex := Word(idx);
    Include(Attr.FFlags, nfLevel2);
  end;
  // keep list sorted by DOM Level 1 name
  FAttributes.FindSorted(qualifiedName, I);
  if FAttributes.FSortedList=nil then FAttributes.FSortedList:=TFPList.Create;
  FAttributes.FSortedList.Insert(I, Attr);
  if FAttributes.FPosList=nil then FAttributes.FPosList:=TFPList.Create;
  FAttributes.FPosList.Add(Attr);
  // TODO: rehash properly, same issue as with Node.SetPrefix()
  Attr.FNSI.QName := FOwnerDocument.FNames.FindOrAdd(DOMPChar(qualifiedName), Length(qualifiedName));
  Attr.FNSI.PrefixLen := Word(prefIdx);
  attr.NodeValue := value;
end;

function TDOMElement.GetAttributeNode(const name: DOMString): TDOMAttr;
begin
  if Assigned(FAttributes) then
    Result := FAttributes.GetNamedItem(name) as TDOMAttr
  else
    Result := nil;
end;

function TDOMElement.GetAttributeNodeNS(const nsURI, aLocalName: DOMString): TDOMAttr;
begin
  if Assigned(FAttributes) then
    Result := FAttributes.GetNamedItemNS(nsURI, aLocalName) as TDOMAttr
  else
    Result := nil;
end;

function TDOMElement.SetAttributeNode(NewAttr: TDOMAttr): TDOMAttr;
begin
  Result := Attributes.SetNamedItem(NewAttr) as TDOMAttr;
end;

function TDOMElement.SetAttributeNodeNS(NewAttr: TDOMAttr): TDOMAttr;
begin
  Result := Attributes.SetNamedItemNS(NewAttr) as TDOMAttr;
end;


function TDOMElement.RemoveAttributeNode(OldAttr: TDOMAttr): TDOMAttr;
begin
  Changing;
  Result:=OldAttr;
  if Assigned(FAttributes) and (FAttributes.FSortedList<>nil)
  and (FAttributes.FSortedList.Remove(OldAttr) > -1)
  then begin
    FAttributes.FPosList.Remove(OldAttr);
    if Assigned(OldAttr.FNSI.QName) then  // safeguard
      FAttributes.RestoreDefault(OldAttr.FNSI.QName^.Key);
    Result.FOwnerElement := nil;
  end
  else
    raise EDOMNotFound.Create('Element.RemoveAttributeNode');
end;

function TDOMElement.GetElementsByTagName(const name: DOMString): TDOMNodeList;
begin
  Result := FOwnerDocument.GetElementList(Self, '', name, False);
end;

function TDOMElement.GetElementsByTagNameNS(const nsURI, aLocalName: DOMString): TDOMNodeList;
begin
  Result := FOwnerDocument.GetElementList(Self, nsURI, aLocalName, True);
end;

function TDOMElement.hasAttribute(const name: DOMString): Boolean;
begin
  Result := Assigned(FAttributes) and
    Assigned(FAttributes.GetNamedItem(name));
end;

function TDOMElement.hasAttributeNS(const nsURI, aLocalName: DOMString): Boolean;
begin
  Result := Assigned(FAttributes) and
    Assigned(FAttributes.getNamedItemNS(nsURI, aLocalName));
end;

function TDOMElement.HasAttributes: Boolean;
begin
  Result := Assigned(FAttributes) and (FAttributes.Length > 0);
end;

// -------------------------------------------------------
//   Text
// -------------------------------------------------------

function TDOMText.GetNodeType: Integer;
begin
  Result := TEXT_NODE;
end;

function TDOMText.GetNodeName: DOMString;
begin
  Result := '#text';
end;

procedure TDOMText.SetNodeValue(const aValue: DOMString);
begin
  inherited SetNodeValue(aValue);
  // TODO: may analyze aValue, but this will slow things down...
  Exclude(FFlags, nfIgnorableWS);
end;

function TDOMText.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := ACloneOwner.CreateTextNode(FNodeValue);
end;

function TDOMText.SplitText(offset: LongWord): TDOMText;
begin
  Changing;
  if offset > Length then
    raise EDOMIndexSize.Create('Text.SplitText');

  Result := TDOMText.Create(FOwnerDocument);
  Result.FNodeValue := Copy(FNodeValue, offset + 1, Length);
  Result.FFlags := FFlags * [nfIgnorableWS];
  FNodeValue := Copy(FNodeValue, 1, offset);
  if Assigned(FParentNode) then
    FParentNode.InsertBefore(Result, FNextSibling);
end;

function TDOMText.IsElementContentWhitespace: Boolean;
begin
  Result := nfIgnorableWS in FFlags;
end;

// -------------------------------------------------------
//   Comment
// -------------------------------------------------------

function TDOMComment.GetNodeType: Integer;
begin
  Result := COMMENT_NODE;
end;

function TDOMComment.GetNodeName: DOMString;
begin
  Result := '#comment';
end;

function TDOMComment.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := ACloneOwner.CreateComment(FNodeValue);
end;


// -------------------------------------------------------
//   CDATASection
// -------------------------------------------------------

function TDOMCDATASection.GetNodeType: Integer;
begin
  Result := CDATA_SECTION_NODE;
end;

function TDOMCDATASection.GetNodeName: DOMString;
begin
  Result := '#cdata-section';
end;

function TDOMCDATASection.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := ACloneOwner.CreateCDATASection(FNodeValue);
end;


// -------------------------------------------------------
//   DocumentType
// -------------------------------------------------------

function TDOMDocumentType.GetNodeType: Integer;
begin
  Result := DOCUMENT_TYPE_NODE;
end;

function TDOMDocumentType.GetNodeName: DOMString;
begin
  Result := FName;
end;

destructor TDOMDocumentType.Destroy;
begin
  FEntities.Free;
  FNotations.Free;
  inherited Destroy;
end;

function TDOMDocumentType.GetEntities: TDOMNamedNodeMap;
begin
  if FEntities = nil then
    FEntities := TDOMNamedNodeMap.Create(Self, ENTITY_NODE);
  Result := FEntities;
end;

function TDOMDocumentType.GetNotations: TDOMNamedNodeMap;
begin
  if FNotations = nil then
    FNotations := TDOMNamedNodeMap.Create(Self, NOTATION_NODE);
  Result := FNotations;
end;

// -------------------------------------------------------
//   Notation
// -------------------------------------------------------

function TDOMNotation.GetNodeType: Integer;
begin
  Result := NOTATION_NODE;
end;

function TDOMNotation.GetNodeName: DOMString;
begin
  Result := FName;
end;

function TDOMNotation.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := ACloneOwner.Alloc(TDOMNotation);
  TDOMNotation(Result).Create(ACloneOwner);
  TDOMNotation(Result).FName := FName;
  TDOMNotation(Result).FPublicID := PublicID;
  TDOMNotation(Result).FSystemID := SystemID;
  // notation cannot have children, ignore Deep
end;


// -------------------------------------------------------
//   Entity
// -------------------------------------------------------

function TDOMEntity.GetNodeType: Integer;
begin
  Result := ENTITY_NODE;
end;

function TDOMEntity.GetNodeName: DOMString;
begin
  Result := FName;
end;

function TDOMEntity.CloneNode(deep: Boolean; aCloneOwner: TDOMDocument): TDOMNode;
begin
  Result := aCloneOwner.Alloc(TDOMEntity);
  TDOMEntity(Result).Create(aCloneOwner);
  TDOMEntity(Result).FName := FName;
  TDOMEntity(Result).FSystemID := FSystemID;
  TDOMEntity(Result).FPublicID := FPublicID;
  TDOMEntity(Result).FNotationName := FNotationName;
  if deep then
    CloneChildren(Result, aCloneOwner);
  Result.SetReadOnly(True);
end;

// -------------------------------------------------------
//   EntityReference
// -------------------------------------------------------

function TDOMEntityReference.GetNodeType: Integer;
begin
  Result := ENTITY_REFERENCE_NODE;
end;

function TDOMEntityReference.GetNodeName: DOMString;
begin
  Result := FName;
end;

function TDOMEntityReference.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := ACloneOwner.CreateEntityReference(FName);
end;

// -------------------------------------------------------
//   ProcessingInstruction
// -------------------------------------------------------

function TDOMProcessingInstruction.CloneNode(deep: Boolean;
  ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := ACloneOwner.CreateProcessingInstruction(Target, Data);
end;

function TDOMProcessingInstruction.GetNodeType: Integer;
begin
  Result := PROCESSING_INSTRUCTION_NODE;
end;

function TDOMProcessingInstruction.GetNodeName: DOMString;
begin
  Result := FTarget;
end;

function TDOMProcessingInstruction.GetNodeValue: DOMString;
begin
  Result := FNodeValue;
end;

procedure TDOMProcessingInstruction.SetNodeValue(const AValue: DOMString);
begin
  Changing;
  FNodeValue := AValue;
end;

{ TDOMAttrDef }

function TDOMAttrDef.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := inherited CloneNode(deep, ACloneOwner);
  Exclude(Result.FFlags, nfSpecified);
end;

function TDOMAttrDef.AddEnumToken(Buf: DOMPChar; Len: Integer): Boolean;
var
  I, L: Integer;
begin
  // TODO: this implementaion is the slowest possible...
  Result := False;
  L := Length(FEnumeration);
  for I := 0 to L-1 do
  begin
    if CompareDomStrings(Buf, DOMPChar(FEnumeration[I]), Len, Length(FEnumeration[I])) = 0 then
      Exit;
  end;
  SetLength(FEnumeration, L+1);
  SetString(FEnumeration[L], Buf, Len);
  Result := True;
end;

function TDOMAttrDef.HasEnumToken(const aValue: DOMString): Boolean;
var
  I: Integer;
begin
  Result := True;
  if Length(FEnumeration) = 0 then
    Exit;
  for I := 0 to Length(FEnumeration)-1 do
  begin
    if FEnumeration[I] = aValue then
      Exit;
  end;
  Result := False;
end;

{ TNodePool }

constructor TNodePool.Create(AElementSize: Integer; AElementCount: Integer);
begin
  FElementSize := AElementSize;
  AddExtent(AElementCount);
end;

destructor TNodePool.Destroy;
var
  ext, next: PExtent;
  ptr, ptr_end: PAnsiChar;
  sz: Integer;
begin
  ext := FCurrExtent;
  ptr := PAnsiChar(FCurrBlock) + FElementSize;
  sz := FCurrExtentSize;
  while Assigned(ext) do
  begin
    // call destructors for everyone still there
    ptr_end := PAnsiChar(ext) + sizeof(TExtent) + (sz - 1) * FElementSize;
    while ptr <= ptr_end do
    begin
      if TDOMNode(ptr).FPool = Self then
        TObject(ptr).Destroy;
      Inc(ptr, FElementSize);
    end;
    // dispose the extent and pass to the next one
    next := ext^.Next;
    FreeMem(ext);
    ext := next;
    sz := sz div 2;
    ptr := PAnsiChar(ext) + sizeof(TExtent);
  end;
  inherited Destroy;
end;

procedure TNodePool.AddExtent(AElemCount: Integer);
var
  ext: PExtent;
begin
  Assert((FCurrExtent = nil) or
    (PAnsiChar(FCurrBlock) < PAnsiChar(FCurrExtent) + sizeof(TExtent)));
  Assert(AElemCount > 0);

  GetMem(ext, sizeof(TExtent) + AElemCount * FElementSize);
  ext^.Next := FCurrExtent;
  // point to the beginning of the last block of extent
  FCurrBlock := TDOMNode(PAnsiChar(ext) + sizeof(TExtent) + (AElemCount - 1) * FElementSize);
  FCurrExtent := ext;
  FCurrExtentSize := AElemCount;
end;

function TNodePool.AllocNode(AClass: TDOMNodeClass): TDOMNode;
begin
  if Assigned(FFirstFree) then
  begin
    Result := FFirstFree;       // remove from free list
    FFirstFree := TDOMNode(Result.FPool);
  end
  else
  begin
    if PAnsiChar(FCurrBlock) < PAnsiChar(FCurrExtent) + sizeof(TExtent) then
      AddExtent(FCurrExtentSize * 2);
    Result := FCurrBlock;
    Dec(PAnsiChar(FCurrBlock), FElementSize);
  end;
  AClass.InitInstance(Result);
  Result.FPool := Self;        // mark as used
end;

procedure TNodePool.FreeNode(ANode: TDOMNode);
begin
  ANode.FPool := FFirstFree;
  FFirstFree := ANode;
end;

end.
