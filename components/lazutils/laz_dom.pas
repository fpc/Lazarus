{
  BEWARE !!!
  This is a TEMPORARY file.
  As soon as it is moved to the fcl, it will be removed.
}

{
    $Id$
    This file is part of the Free Component Library

    Implementation of DOM interfaces
    Copyright (c) 1999-2000 by Sebastian Guenther, sg@freepascal.org

    See the file COPYING.modifiedLGPL.txt, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  This unit provides classes which implement the interfaces defined in the
  DOM (Document Object Model) specification.
  The current state is:
  DOM Level 1  -  Almost completely implemented
  DOM Level 2  -  Partially implemented


  Specification used for this implementation:

  "Document Object Model (DOM) Level 2 Specification Version 1.0
   W3C Candidate Recommendation 07 March, 2000"
  http://www.w3.org/TR/2000/CR-DOM-Level-2-20000307
}


unit Laz_DOM;

{$MODE objfpc}
{$H+}

interface

{off $DEFINE MEM_CHECK}

uses
  {$IFDEF MEM_CHECK}MemCheck,{$ENDIF}
  SysUtils, Classes, Avl_Tree;


type
  TDOMImplementation = class;
  TDOMDocumentFragment = class;
  TDOMDocument = class;
  TDOMNode = class;
  TDOMNodeList = class;
  TDOMNamedNodeMap = class;
  TDOMCharacterData = class;
  TDOMAttr = class;
  TDOMElement = class;
  TDOMText = class;
  TDOMComment = class;
  TDOMCDATASection = class;
  TDOMDocumentType = class;
  TDOMNotation = class;
  TDOMEntity = class;
  TDOMEntityReference = class;
  TDOMProcessingInstruction = class;


// -------------------------------------------------------
//   DOMString
// -------------------------------------------------------
  DOMString = String;
  DOMPChar = PChar;
//  DOMString = WideString;
//  DOMPChar = PWideChar;


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


type

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

  TRefClass = class
  protected
    RefCounter: LongInt;
  public
    constructor Create;
    function AddRef: LongInt; virtual;
    function Release: LongInt; virtual;
  end;

  { TDOMNode }

  TDOMNode = class
  protected
    FNodeName, FNodeValue: DOMString;
    FNodeType: Integer;
    FParentNode: TDOMNode;
    FPreviousSibling, FNextSibling: TDOMNode;
    FOwnerDocument: TDOMDocument;

    function  GetNodeValue: DOMString; virtual;
    procedure SetNodeValue(const AValue: DOMString); virtual;
    function  GetFirstChild: TDOMNode; virtual;
    function  GetLastChild: TDOMNode; virtual;
    function  GetAttributes: TDOMNamedNodeMap; virtual;

  public
    constructor Create(AOwner: TDOMDocument);
    
    // Free NodeList with TDOMNodeList.Release!
    function GetChildNodes: TDOMNodeList; virtual;

    property NodeName: DOMString read FNodeName;
    property NodeValue: DOMString read GetNodeValue write SetNodeValue;
    property NodeType: Integer read FNodeType;
    property ParentNode: TDOMNode read FParentNode;
    property FirstChild: TDOMNode read GetFirstChild;
    property LastChild: TDOMNode read GetLastChild;
    property ChildNodes: TDOMNodeList read GetChildNodes;
    property PreviousSibling: TDOMNode read FPreviousSibling;
    property NextSibling: TDOMNode read FNextSibling;
    property Attributes: TDOMNamedNodeMap read GetAttributes;
    property OwnerDocument: TDOMDocument read FOwnerDocument;

    function InsertBefore(NewChild, RefChild: TDOMNode): TDOMNode; virtual;
    function ReplaceChild(NewChild, OldChild: TDOMNode): TDOMNode; virtual;
    function RemoveChild(OldChild: TDOMNode): TDOMNode; virtual;
    function AppendChild(NewChild: TDOMNode): TDOMNode; virtual;
    function HasChildNodes: Boolean; virtual;
    function CloneNode(deep: Boolean): TDOMNode; overload;
    function IsEmpty: Boolean; virtual;

    // Extensions to DOM interface:
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; virtual;
    function FindNode(const ANodeName: DOMString): TDOMNode; virtual;
  end;


  { The following class is an implementation specific extension, it is just an
    extended implementation of TDOMNode, the generic DOM::Node interface
    implementation. (Its main purpose is to save memory in a big node tree) }

  TDOMNode_WithChildren = class(TDOMNode)
  protected
    FFirstChild, FLastChild: TDOMNode;
    FChildNodeTree: TAVLTree;// tree of TDOMNode sorted for Name (=> there can be doubles)
    function GetFirstChild: TDOMNode; override;
    function GetLastChild: TDOMNode; override;
    procedure CloneChildren(ACopy: TDOMNode; ACloneOwner: TDOMDocument);
    procedure AddToChildNodeTree(NewNode: TDOMNode);
    procedure RemoveFromChildNodeTree(OldNode: TDOMNode);
  public
    destructor Destroy; override;
    function InsertBefore(NewChild, RefChild: TDOMNode): TDOMNode; override;
    function ReplaceChild(NewChild, OldChild: TDOMNode): TDOMNode; override;
    function RemoveChild(OldChild: TDOMNode): TDOMNode; override;
    function AppendChild(NewChild: TDOMNode): TDOMNode; override;
    function HasChildNodes: Boolean; override;
    function FindNode(const ANodeName: DOMString): TDOMNode; override;
  end;


// -------------------------------------------------------
//   NodeList
// -------------------------------------------------------

  TDOMNodeList = class(TRefClass)
  protected
    node: TDOMNode;
    filter: DOMString;
    UseFilter: Boolean;
    function GetCount: LongInt;
    function GetItem(index: LongWord): TDOMNode;
  public
    constructor Create(ANode: TDOMNode; const AFilter: DOMString);
    property Item[index: LongWord]: TDOMNode read GetItem;
    property Count: LongInt read GetCount;
  end;


// -------------------------------------------------------
//   NamedNodeMap
// -------------------------------------------------------

  TDOMNamedNodeMap = class(TFPList)
  protected
    OwnerDocument: TDOMDocument;
    function GetItem(index: LongWord): TDOMNode;
    procedure SetItem(index: LongWord; AItem: TDOMNode);
    function GetLength: LongInt;
  public
    constructor Create(AOwner: TDOMDocument);

    function GetNamedItem(const name: DOMString): TDOMNode;
    function SetNamedItem(arg: TDOMNode): TDOMNode;
    function RemoveNamedItem(const name: DOMString): TDOMNode;
    property Item[index: LongWord]: TDOMNode read GetItem write SetItem; default;
    property Length: LongInt read GetLength;
  end;


// -------------------------------------------------------
//   CharacterData
// -------------------------------------------------------

  TDOMCharacterData = class(TDOMNode)
  protected
    function  GetLength: LongInt;
  public
    property Data: DOMString read FNodeValue;
    property Length: LongInt read GetLength;
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
  public
    constructor Create(AOwner: TDOMDocument);
  end;


// -------------------------------------------------------
//   Document
// -------------------------------------------------------

  TDOMDocument = class(TDOMNode_WithChildren)
  protected
    FDocType: TDOMDocumentType;
    FImplementation: TDOMImplementation;
    function GetDocumentElement: TDOMElement;
  public
    property DocType: TDOMDocumentType read FDocType;
    property Impl: TDOMImplementation read FImplementation;
    property DocumentElement: TDOMElement read GetDocumentElement;

    function CreateElement(const tagName: DOMString): TDOMElement; virtual;
    function CreateDocumentFragment: TDOMDocumentFragment;
    function CreateTextNode(const data: DOMString): TDOMText;
    function CreateComment(const data: DOMString): TDOMComment;
    function CreateCDATASection(const data: DOMString): TDOMCDATASection;
      virtual;
    function CreateProcessingInstruction(const target, data: DOMString):
      TDOMProcessingInstruction; virtual;
    function CreateAttribute(const name: DOMString): TDOMAttr; virtual;
    function CreateEntityReference(const name: DOMString): TDOMEntityReference;
      virtual;
    // Free NodeList with TDOMNodeList.Release!
    function GetElementsByTagName(const tagname: DOMString): TDOMNodeList;

    // Extensions to DOM interface:
    constructor Create;
    function CreateEntity(const data: DOMString): TDOMEntity;
  end;

  TXMLDocument = class(TDOMDocument)
  public
    // These fields are extensions to the DOM interface:
    XMLVersion, Encoding, StylesheetType, StylesheetHRef: DOMString;

    function CreateCDATASection(const data: DOMString): TDOMCDATASection; override;
    function CreateProcessingInstruction(const target, data: DOMString):
      TDOMProcessingInstruction; override;
    function CreateEntityReference(const name: DOMString): TDOMEntityReference; override;
  end;


// -------------------------------------------------------
//   Attr
// -------------------------------------------------------

  TDOMAttr = class(TDOMNode_WithChildren)
  protected
    FSpecified: Boolean;
    AttrOwner: TDOMNamedNodeMap;
    function  GetNodeValue: DOMString; override;
    procedure SetNodeValue(const AValue: DOMString); override;
  public
    constructor Create(AOwner: TDOMDocument);

    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
    property Name: DOMString read FNodeName;
    property Specified: Boolean read FSpecified;
    property Value: DOMString read GetNodeValue write SetNodeValue;
  end;


// -------------------------------------------------------
//   Element
// -------------------------------------------------------

  { TDOMElement }

  TDOMElement = class(TDOMNode_WithChildren)
  private
    FAttributes: TDOMNamedNodeMap;
  protected
    function GetAttributes: TDOMNamedNodeMap; override;
  public
    constructor Create(AOwner: TDOMDocument);
    destructor Destroy; override;
    function  CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
    property  TagName: DOMString read FNodeName;
    function  GetAttribute(const name: DOMString): DOMString;
    procedure SetAttribute(const name, value: DOMString);
    procedure RemoveAttribute(const name: DOMString);
    function  GetAttributeNode(const name: DOMString): TDOMAttr;
    procedure SetAttributeNode(NewAttr: TDOMAttr);
    function  RemoveAttributeNode(OldAttr: TDOMAttr): TDOMAttr;
    // Free NodeList with TDOMNodeList.Release!
    function  GetElementsByTagName(const name: DOMString): TDOMNodeList;
    function  IsEmpty: Boolean; override;
    procedure Normalize;

    property AttribStrings[const Name: DOMString]: DOMString
      read GetAttribute write SetAttribute; default;
  end;


// -------------------------------------------------------
//   Text
// -------------------------------------------------------

  TDOMText = class(TDOMCharacterData)
  public
    constructor Create(AOwner: TDOMDocument);
    function  CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
    function SplitText(offset: LongWord): TDOMText;
  end;


// -------------------------------------------------------
//   Comment
// -------------------------------------------------------

  TDOMComment = class(TDOMCharacterData)
  public
    constructor Create(AOwner: TDOMDocument);
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
  end;


// -------------------------------------------------------
//   CDATASection
// -------------------------------------------------------

  TDOMCDATASection = class(TDOMText)
  public
    constructor Create(AOwner: TDOMDocument);
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
  end;


// -------------------------------------------------------
//   DocumentType
// -------------------------------------------------------

  TDOMDocumentType = class(TDOMNode)
  protected
    FEntities, FNotations: TDOMNamedNodeMap;
  public
    constructor Create(AOwner: TDOMDocument);
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
    property Name: DOMString read FNodeName;
    property Entities: TDOMNamedNodeMap read FEntities;
    property Notations: TDOMNamedNodeMap read FEntities;
  end;


// -------------------------------------------------------
//   Notation
// -------------------------------------------------------

  TDOMNotation = class(TDOMNode)
  protected
    FPublicID, FSystemID: DOMString;
  public
    constructor Create(AOwner: TDOMDocument);
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
    property PublicID: DOMString read FPublicID;
    property SystemID: DOMString read FSystemID;
  end;


// -------------------------------------------------------
//   Entity
// -------------------------------------------------------

  TDOMEntity = class(TDOMNode_WithChildren)
  protected
    FPublicID, FSystemID, FNotationName: DOMString;
  public
    constructor Create(AOwner: TDOMDocument);
    property PublicID: DOMString read FPublicID;
    property SystemID: DOMString read FSystemID;
    property NotationName: DOMString read FNotationName;
  end;


// -------------------------------------------------------
//   EntityReference
// -------------------------------------------------------

  TDOMEntityReference = class(TDOMNode_WithChildren)
  public
    constructor Create(AOwner: TDOMDocument);
  end;


// -------------------------------------------------------
//   ProcessingInstruction
// -------------------------------------------------------

  TDOMProcessingInstruction = class(TDOMNode)
  public
    constructor Create(AOwner: TDOMDocument);
    property Target: DOMString read FNodeName;
    property Data: DOMString read FNodeValue;
  end;




// =======================================================
// =======================================================

implementation


constructor TRefClass.Create;
begin
  inherited Create;
  RefCounter := 1;
end;

function TRefClass.AddRef: LongInt;
begin
  Inc(RefCounter);
  Result := RefCounter;
end;

function TRefClass.Release: LongInt;
begin
  Dec(RefCounter);
  Result := RefCounter;
  if RefCounter <= 0 then Free;
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

function TDOMNode.GetNodeValue: DOMString;
begin
  Result := FNodeValue;
end;

procedure TDOMNode.SetNodeValue(const AValue: DOMString);
begin
  FNodeValue := AValue;
end;

function TDOMNode.GetChildNodes: TDOMNodeList;
begin
  Result := TDOMNodeList.Create(Self, '*');
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
  raise EDOMHierarchyRequest.Create('Node.InsertBefore');
  if (NewChild=nil) and (RefChild=nil) then ;
  Result:=nil;
end;

function TDOMNode.ReplaceChild(NewChild, OldChild: TDOMNode): TDOMNode;
begin
  raise EDOMHierarchyRequest.Create('Node.ReplaceChild');
  if (NewChild=nil) and (OldChild=nil) then ;
  Result:=nil;
end;

function TDOMNode.RemoveChild(OldChild: TDOMNode): TDOMNode;
begin
  raise EDOMHierarchyRequest.Create('Node.RemoveChild');
  if (OldChild=nil) then ;
  Result:=nil;
end;

function TDOMNode.AppendChild(NewChild: TDOMNode): TDOMNode;
begin
  raise EDOMHierarchyRequest.Create('Node.AppendChild');
  if (NewChild=nil) then ;
  Result:=nil;
end;

function TDOMNode.HasChildNodes: Boolean;
begin
  Result := False;
end;

function TDOMNode.CloneNode(deep: Boolean): TDOMNode;
begin
  if deep then ;
  Result:=CloneNode(deep, FOwnerDocument);
end;

function TDOMNode.IsEmpty: Boolean;
begin
  Result:=true;
end;

function TDOMNode.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  raise EDOMNotSupported.Create('CloneNode not implemented for ' + ClassName);
  if (deep) and (ACloneOwner=nil) then ;
  Result:=nil;
end;

function TDOMNode.FindNode(const ANodeName: DOMString): TDOMNode;
var
  child: TDOMNode;
begin
  child := FirstChild;
  while Assigned(child) do
  begin
    if child.NodeName = ANodeName then
    begin
      Result := child;
      exit;
    end;
    child := child.NextSibling;
  end;
  Result := nil;
end;

//------------------------------------------------------------------------------

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

function CompareDOMNodeWithDOMNode(Node1, Node2: Pointer): integer;
begin
  Result:=CompareDOMStrings(DOMPChar(Pointer(TDOMNode(Node1).NodeName)),
                            DOMPChar(Pointer(TDOMNode(Node2).NodeName)),
                            length(TDOMNode(Node1).NodeName),
                            length(TDOMNode(Node2).NodeName)
                            );
end;

function CompareDOMStringWithDOMNode(AKey, ANode: Pointer): integer;
begin
  Result:=CompareDOMStrings(DOMPChar(AKey),
                            DOMPChar(Pointer(TDOMNode(ANode).NodeName)),
                            length(DOMString(AKey)),
                            length(TDOMNode(ANode).NodeName)
                            );
end;


function TDOMNode_WithChildren.GetFirstChild: TDOMNode;
begin
  Result := FFirstChild;
end;

function TDOMNode_WithChildren.GetLastChild: TDOMNode;
begin
  Result := FLastChild;
end;

destructor TDOMNode_WithChildren.Destroy;
var
  child, next: TDOMNode;
begin
  if FChildNodeTree<>nil then begin
    FChildNodeTree.Free;
    FChildNodeTree:=nil;
  end;
  child := FirstChild;
  while Assigned(child) do
  begin
    next := child.NextSibling;
    child.Free;
    child := next;
  end;
  inherited Destroy;
end;

function TDOMNode_WithChildren.InsertBefore(NewChild, RefChild: TDOMNode):
  TDOMNode;
begin
  Result := NewChild;

  if not Assigned(RefChild) then
  begin
    AppendChild(NewChild);
    exit;
  end;

  if NewChild.FOwnerDocument <> FOwnerDocument then
    raise EDOMWrongDocument.Create('NodeWC.InsertBefore');

  if RefChild.ParentNode <> Self then
    raise EDOMHierarchyRequest.Create('NodeWC.InsertBefore');

  if NewChild.NodeType = DOCUMENT_FRAGMENT_NODE then
    raise EDOMNotSupported.Create('NodeWC.InsertBefore for DocumentFragment');

  NewChild.FNextSibling := RefChild;
  if RefChild = FFirstChild then
    FFirstChild := NewChild
  else
  begin
    RefChild.FPreviousSibling.FNextSibling := NewChild;
    NewChild.FPreviousSibling := RefChild.FPreviousSibling;
  end;

  RefChild.FPreviousSibling := NewChild;
  NewChild.FParentNode := Self;
  AddToChildNodeTree(NewChild);
end;

function TDOMNode_WithChildren.ReplaceChild(NewChild, OldChild: TDOMNode):
  TDOMNode;
begin
  InsertBefore(NewChild, OldChild);
  if Assigned(OldChild) then
    RemoveChild(OldChild);
  Result := NewChild;
end;

function TDOMNode_WithChildren.RemoveChild(OldChild: TDOMNode):
  TDOMNode;
begin
  if OldChild.ParentNode <> Self then
    raise EDOMHierarchyRequest.Create('NodeWC.RemoveChild');

  if OldChild = FFirstChild then
    FFirstChild := FFirstChild.NextSibling
  else
    OldChild.FPreviousSibling.FNextSibling := OldChild.FNextSibling;

  if OldChild = FLastChild then
    FLastChild := FLastChild.FPreviousSibling
  else
    OldChild.FNextSibling.FPreviousSibling := OldChild.FPreviousSibling;

  RemoveFromChildNodeTree(OldChild);
  OldChild.Free;
  Result:=nil;
end;

function TDOMNode_WithChildren.AppendChild(NewChild: TDOMNode): TDOMNode;
var
  Parent: TDOMNode;
begin
  //writeln('TDOMNode_WithChildren.AppendChild ',NodeName,' NewChild=',NewChild.NodeName);
  if NewChild.FOwnerDocument <> FOwnerDocument then
    raise EDOMWrongDocument.Create('NodeWC.AppendChild');

  Parent := Self;
  while Assigned(Parent) do
  begin
    if Parent = NewChild then
      raise EDOMHierarchyRequest.Create('NodeWC.AppendChild (cycle in tree)');
    Parent := Parent.ParentNode;
  end;

  if NewChild.FParentNode<>nil then begin
    //writeln('TDOMNode_WithChildren.AppendChild old NewChild.FParentNode=',NewChild.FParentNode.NodeName);
    NewChild.FParentNode.RemoveChild(NewChild);
  end;

  if NewChild.NodeType = DOCUMENT_FRAGMENT_NODE then
    raise EDOMNotSupported.Create('NodeWC.AppendChild for DocumentFragments')
  else begin
    if Assigned(FFirstChild) then
    begin
      FLastChild.FNextSibling := NewChild;
      NewChild.FPreviousSibling := FLastChild;
    end else
      FFirstChild := NewChild;
    FLastChild := NewChild;
    NewChild.FParentNode := Self;
  end;
  AddToChildNodeTree(NewChild);
  Result := NewChild;
end;

function TDOMNode_WithChildren.HasChildNodes: Boolean;
begin
  Result := Assigned(FFirstChild);
end;

function TDOMNode_WithChildren.FindNode(const ANodeName: DOMString): TDOMNode;
var
  AVLNode: TAVLTreeNode;
begin
  Result:=nil;
  if FChildNodeTree<>nil then begin
    // use tree for fast search
    //if FChildNodeTree.ConsistencyCheck<>0 then
    //  raise exception.Create('TDOMNode_WithChildren.FindNode');
    AVLNode:=FChildNodeTree.FindKey(DOMPChar(Pointer(ANodeName)),
                                    @CompareDOMStringWithDOMNode);
    if AVLNode<>nil then
      Result:=TDOMNode(AVLNode.Data);
  end else begin
    // search in list
    Result := FirstChild;
    while Assigned(Result) do begin
      if CompareDOMStringWithDOMNode(DOMPChar(Pointer(ANodeName)),Result)=0
      then exit;
      Result := Result.NextSibling;
    end;
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
    ACopy.AppendChild(node.CloneNode(True, ACloneOwner));
    node := node.NextSibling;
  end;
end;

procedure TDOMNode_WithChildren.AddToChildNodeTree(NewNode: TDOMNode);
var
  ChildCount: Integer;
  ANode: TDOMNode;
  NewNodeAdded: Boolean;
begin
  if (FChildNodeTree=nil) then begin
    // there is no childnodetree yet
    // Most xml trees contains nodes with only a few child nodes. It would be
    // overhead to create a tree for only a few children.
    ChildCount := 0;
    ANode := FirstChild;
    while Assigned(ANode) do begin
      inc(ChildCount);
      ANode := ANode.NextSibling;
    end;
    if ChildCount>5 then begin
      FChildNodeTree:=TAVLTree.Create(@CompareDOMNodeWithDOMNode);
      // add all existing children
      ANode := FirstChild;
      NewNodeAdded:=false;
      while Assigned(ANode) do begin
        if ANode=NewNode then NewNodeAdded:=true;
        FChildNodeTree.Add(ANode);
        ANode := ANode.NextSibling;
      end;
      if not NewNodeAdded then
        FChildNodeTree.Add(NewNode);
    end;
  end else begin
    {if (FChildNodeTree.Find(NewNode)<>nil) then begin
      writeln('TDOMNode_WithChildren.AddToChildNodeTree adding same value ',NewNOde.NodeName);
      CTDumpStack;
    end;}
    FChildNodeTree.Add(NewNode);
  end;
  //if FChildNodeTree.ConsistencyCheck<>0 then
  //  raise exception.Create('TDOMNode_WithChildren.FindNode');
end;

procedure TDOMNode_WithChildren.RemoveFromChildNodeTree(OldNode: TDOMNode);
begin
  if FChildNodeTree<>nil then
    FChildNodeTree.RemovePointer(OldNode);// doubles are allowed, so Remove can not be used
  //if (FChildNodeTree<>nil) and (FChildNodeTree.ConsistencyCheck<>0) then
  //  raise exception.Create('TDOMNode_WithChildren.FindNode');
end;


// -------------------------------------------------------
//   NodeList
// -------------------------------------------------------

constructor TDOMNodeList.Create(ANode: TDOMNode; const AFilter: DOMString);
begin
  inherited Create;
  node := ANode;
  filter := AFilter;
  UseFilter := filter <> '*';
end;

function TDOMNodeList.GetCount: LongInt;
var
  child: TDOMNode;
begin
  Result := 0;
  child := node.FirstChild;
  while Assigned(child) do
  begin
    if (not UseFilter) or (child.NodeName = filter) then
      Inc(Result);
    child := child.NextSibling;
  end;
end;

function TDOMNodeList.GetItem(index: LongWord): TDOMNode;
var
  child: TDOMNode;
begin
  Result := nil;
  child := node.FirstChild;
  while Assigned(child) do
  begin
    if index = 0 then
    begin
      Result := child;
      break;
    end;
    if (not UseFilter) or (child.NodeName = filter) then
      Dec(index);
    child := child.NextSibling;
  end;
end;


// -------------------------------------------------------
//   NamedNodeMap
// -------------------------------------------------------

constructor TDOMNamedNodeMap.Create(AOwner: TDOMDocument);
begin
  inherited Create;
  OwnerDocument := AOwner;
end;

function TDOMNamedNodeMap.GetItem(index: LongWord): TDOMNode;
begin
  Result := TDOMNode(Items[index]);
end;

procedure TDOMNamedNodeMap.SetItem(index: LongWord; AItem: TDOMNode);
begin
  Items[index] := AItem;
end;

function TDOMNamedNodeMap.GetLength: LongInt;
begin
  Result := Count;
end;

function TDOMNamedNodeMap.GetNamedItem(const name: DOMString): TDOMNode;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Item[i];
    if Result.NodeName = name then
      exit;
  end;
  Result := nil;
end;

function TDOMNamedNodeMap.SetNamedItem(arg: TDOMNode): TDOMNode;
var
  i: Integer;
begin
  if arg.FOwnerDocument <> OwnerDocument then
    raise EDOMWrongDocument.Create('NamedNodeMap.SetNamedItem');

  if arg.NodeType = ATTRIBUTE_NODE then
  begin
    if Assigned(TDOMAttr(arg).AttrOwner) then
      raise EDOMInUseAttribute.Create('NamedNodeMap.SetNamedItem');
    TDOMAttr(arg).AttrOwner := Self;
  end;

  for i := 0 to Count - 1 do
    if Item[i].NodeName = arg.NodeName then
    begin
      Result := Item[i];
      Item[i] := arg;
      exit;
    end;
  Add(arg);
  Result := nil;
end;

function TDOMNamedNodeMap.RemoveNamedItem(const name: DOMString): TDOMNode;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Item[i].NodeName = name then
    begin
      Result := Item[i];
      Result.FParentNode := nil;
      exit;
    end;
  raise EDOMNotFound.Create('NamedNodeMap.RemoveNamedItem');
end;


// -------------------------------------------------------
//   CharacterData
// -------------------------------------------------------

function TDOMCharacterData.GetLength: LongInt;
begin
  Result := system.Length(FNodeValue);
end;

function TDOMCharacterData.SubstringData(offset, count: LongWord): DOMString;
begin
  if (offset < 0) or (longint(offset) > Length) or (count < 0) then
    raise EDOMIndexSize.Create('CharacterData.SubstringData');
  Result := Copy(FNodeValue, offset + 1, count);
end;

procedure TDOMCharacterData.AppendData(const arg: DOMString);
begin
  FNodeValue := FNodeValue + arg;
end;

procedure TDOMCharacterData.InsertData(offset: LongWord; const arg: DOMString);
begin
  if (offset < 0) or (longint(offset) > Length) then
    raise EDOMIndexSize.Create('CharacterData.InsertData');

  FNodeValue := Copy(FNodeValue, 1, offset) + arg +
    Copy(FNodeValue, offset + 1, Length);
end;

procedure TDOMCharacterData.DeleteData(offset, count: LongWord);
begin
  if (offset < 0) or (longint(offset) > Length) or (count < 0) then
    raise EDOMIndexSize.Create('CharacterData.DeleteData');

  FNodeValue := Copy(FNodeValue, 1, offset) +
    Copy(FNodeValue, offset + count + 1, Length);
end;

procedure TDOMCharacterData.ReplaceData(offset, count: LongWord; const arg: DOMString);
begin
  DeleteData(offset, count);
  InsertData(offset, arg);
end;


// -------------------------------------------------------
//   DocumentFragmet
// -------------------------------------------------------

constructor TDOMDocumentFragment.Create(AOwner: TDOMDocument);
begin
  FNodeType := DOCUMENT_FRAGMENT_NODE;
  FNodeName := '#document-fragment';
  inherited Create(AOwner);
end;


// -------------------------------------------------------
//   DOMImplementation
// -------------------------------------------------------

function TDOMImplementation.HasFeature(const feature, version: DOMString):
  Boolean;
begin
  Result := False;
  if (feature='') and (version='') then ;
end;

function TDOMImplementation.CreateDocumentType(const QualifiedName, PublicID,
  SystemID: DOMString): TDOMDocumentType;
begin
  // !!!: Implement this method (easy to do)
  raise EDOMNotSupported.Create('DOMImplementation.CreateDocumentType');
  if (QualifiedName='') and (PublicID='') and (SystemID='') then ;
  Result:=nil;
end;

function TDOMImplementation.CreateDocument(const NamespaceURI,
  QualifiedName: DOMString; doctype: TDOMDocumentType): TDOMDocument;
begin
  // !!!: Implement this method (easy to do)
  raise EDOMNotSupported.Create('DOMImplementation.CreateDocument');
  if (NamespaceURI='') and (QualifiedName='') and (doctype=nil) then ;
  Result:=nil;
end;


// -------------------------------------------------------
//   Document
// -------------------------------------------------------

constructor TDOMDocument.Create;
begin
  FNodeType := DOCUMENT_NODE;
  FNodeName := '#document';
  inherited Create(nil);
  FOwnerDocument := Self;
end;

function TDOMDocument.GetDocumentElement: TDOMElement;
var
  node: TDOMNode;
begin
  node := FFirstChild;
  while Assigned(node) do
  begin
    if node.FNodeType = ELEMENT_NODE then
    begin
      Result := TDOMElement(node);
      exit;
    end;
    node := node.NextSibling;
  end;
  Result := nil;
end;

function TDOMDocument.CreateElement(const tagName: DOMString): TDOMElement;
begin
  Result := TDOMElement.Create(Self);
  Result.FNodeName := tagName;
end;

function TDOMDocument.CreateDocumentFragment: TDOMDocumentFragment;
begin
  Result := TDOMDocumentFragment.Create(Self);
end;

function TDOMDocument.CreateTextNode(const data: DOMString): TDOMText;
begin
  Result := TDOMText.Create(Self);
  Result.FNodeValue := data;
end;

function TDOMDocument.CreateComment(const data: DOMString): TDOMComment;
begin
  Result := TDOMComment.Create(Self);
  Result.FNodeValue := data;
end;

function TDOMDocument.CreateCDATASection(const data: DOMString):
  TDOMCDATASection;
begin
  raise EDOMNotSupported.Create('DOMDocument.CreateCDATASection');
  if data='' then ;
  Result:=nil;
end;

function TDOMDocument.CreateProcessingInstruction(const target,
  data: DOMString): TDOMProcessingInstruction;
begin
  raise EDOMNotSupported.Create('DOMDocument.CreateProcessingInstruction');
  if (target='') and (data='') then ;
  Result:=nil;
end;

function TDOMDocument.CreateAttribute(const name: DOMString): TDOMAttr;
begin
  Result := TDOMAttr.Create(Self);
  Result.FNodeName := name;
end;

function TDOMDocument.CreateEntityReference(const name: DOMString):
  TDOMEntityReference;
begin
  raise EDOMNotSupported.Create('DOMDocument.CreateEntityReference');
  if name='' then ;
  Result:=nil;
end;

function TDOMDocument.CreateEntity(const data: DOMString): TDOMEntity;
begin
  Result := TDOMEntity.Create(Self);
  Result.FNodeName := data;
end;

function TDOMDocument.GetElementsByTagName(const tagname: DOMString): TDOMNodeList;
begin
  Result := TDOMNodeList.Create(Self, tagname);
end;


function TXMLDocument.CreateCDATASection(const data: DOMString):
  TDOMCDATASection;
begin
  Result := TDOMCDATASection.Create(Self);
  Result.FNodeValue := data;
end;

function TXMLDocument.CreateProcessingInstruction(const target,
  data: DOMString): TDOMProcessingInstruction;
begin
  Result := TDOMProcessingInstruction.Create(Self);
  Result.FNodeName := target;
  Result.FNodeValue := data;
end;

function TXMLDocument.CreateEntityReference(const name: DOMString):
  TDOMEntityReference;
begin
  Result := TDOMEntityReference.Create(Self);
  Result.FNodeName := name;
end;


// -------------------------------------------------------
//   Attr
// -------------------------------------------------------

constructor TDOMAttr.Create(AOwner: TDOMDocument);
begin
  FNodeType := ATTRIBUTE_NODE;
  inherited Create(AOwner);
end;

function TDOMAttr.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := TDOMAttr.Create(ACloneOwner);
  Result.FNodeName := FNodeName;
  TDOMAttr(Result).FSpecified := FSpecified;
  if deep then
    CloneChildren(Result, ACloneOwner);
end;

function TDOMAttr.GetNodeValue: DOMString;
var
  child: TDOMNode;
begin
  SetLength(Result, 0);
  if Assigned(FFirstChild) then
  begin
    child := FFirstChild;
    while Assigned(child) do
    begin
      if child.NodeType = ENTITY_REFERENCE_NODE then
        Result := Result + '&' + child.NodeName + ';'
      else
        Result := Result + child.NodeValue;
      child := child.NextSibling;
    end;
  end;
end;

procedure TDOMAttr.SetNodeValue(const AValue: DOMString);
var
  tn: TDOMText;
begin
  FSpecified := True;
  tn := TDOMText.Create(FOwnerDocument);
  tn.FNodeValue := AValue;
  if Assigned(FFirstChild) then
    ReplaceChild(tn, FFirstChild)
  else
    AppendChild(tn);
end;


// -------------------------------------------------------
//   Element
// -------------------------------------------------------

constructor TDOMElement.Create(AOwner: TDOMDocument);
begin
  FNodeType := ELEMENT_NODE;
  inherited Create(AOwner);
end;

destructor TDOMElement.Destroy;
var
  i: Integer;
begin
  {As the attributes are _not_ children of the element node, we have to free
   them manually here:}
  if FAttributes<>nil then begin
    for i := 0 to FAttributes.Count - 1 do
      FAttributes[i].Free;
    FAttributes.Free;
    FAttributes:=nil;
  end;
  inherited Destroy;
end;

function TDOMElement.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
var
  i: Integer;
begin
  Result := TDOMElement.Create(ACloneOwner);
  Result.FNodeName := FNodeName;
  if FAttributes<>nil then begin
    TDOMElement(Result).GetAttributes;
    for i := 0 to FAttributes.Count - 1 do
      TDOMElement(Result).FAttributes.Add(FAttributes[i].CloneNode(True, ACloneOwner));
  end;
  if deep then
    CloneChildren(Result, ACloneOwner);
end;

function TDOMElement.GetAttributes: TDOMNamedNodeMap;
begin
  if FAttributes=nil then
    FAttributes := TDOMNamedNodeMap.Create(FOwnerDocument);
  Result := FAttributes;
end;

function TDOMElement.GetAttribute(const name: DOMString): DOMString;
var
  i: Integer;
begin
  if FAttributes<>nil then begin
    for i := 0 to FAttributes.Count - 1 do
      if FAttributes[i].NodeName = name then
      begin
        Result := FAttributes[i].NodeValue;
        exit;
      end;
  end;
  SetLength(Result, 0);
end;

procedure TDOMElement.SetAttribute(const name, value: DOMString);
var
  i: Integer;
  attr: TDOMAttr;
begin
  GetAttributes;
  for i := 0 to FAttributes.Count - 1 do
    if FAttributes[i].NodeName = name then
    begin
      FAttributes[i].NodeValue := value;
      exit;
    end;
  attr := TDOMAttr.Create(FOwnerDocument);
  attr.FNodeName := name;
  attr.NodeValue := value;
  FAttributes.Add(attr);
end;

procedure TDOMElement.RemoveAttribute(const name: DOMString);
var
  i: Integer;
begin
  if FAttributes=nil then exit;
  for i := 0 to FAttributes.Count - 1 do
    if FAttributes[i].NodeName = name then
    begin
      FAttributes[i].Free;
      FAttributes.Delete(i);
      exit;
    end;
end;

function TDOMElement.GetAttributeNode(const name: DOMString): TDOMAttr;
var
  i: Integer;
begin
  if FAttributes<>nil then begin
    for i := 0 to FAttributes.Count - 1 do
      if FAttributes[i].NodeName = name then
      begin
        Result := TDOMAttr(FAttributes[i]);
        exit;
      end;
  end;
  Result := nil;
end;

procedure TDOMElement.SetAttributeNode(NewAttr: TDOMAttr);
var
  i: Integer;
begin
  if FAttributes=nil then exit;
  for i := 0 to FAttributes.Count - 1 do
    if FAttributes[i].NodeName = NewAttr.NodeName then
    begin
      FAttributes[i].Free;
      FAttributes[i] := NewAttr;
      exit;
    end;
end;

function TDOMElement.RemoveAttributeNode(OldAttr: TDOMAttr): TDOMAttr;
var
  i: Integer;
  node: TDOMNode;
begin
  Result:=nil;
  if FAttributes=nil then exit;
  for i := 0 to FAttributes.Count - 1 do
  begin
    node := FAttributes[i];
    if node = OldAttr then
    begin
      FAttributes.Delete(i);
      Result := TDOMAttr(node);
      exit;
    end;
  end;
end;

function TDOMElement.GetElementsByTagName(const name: DOMString): TDOMNodeList;
begin
  Result := TDOMNodeList.Create(Self, name);
end;

function TDOMElement.IsEmpty: Boolean;
begin
  Result:=(FAttributes=nil) or (FAttributes.Count=0)
end;

procedure TDOMElement.Normalize;
begin
  // !!!: Not implemented
end;


// -------------------------------------------------------
//   Text
// -------------------------------------------------------

constructor TDOMText.Create(AOwner: TDOMDocument);
begin
  FNodeType := TEXT_NODE;
  FNodeName := '#text';
  inherited Create(AOwner);
end;

function TDOMText.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := TDOMText.Create(ACloneOwner);
  Result.FNodeValue := FNodeValue;
  if deep and (ACloneOwner=nil) then ;
end;

function TDOMText.SplitText(offset: LongWord): TDOMText;
begin
  if longint(offset) > Length then
    raise EDOMIndexSize.Create('Text.SplitText');

  Result := TDOMText.Create(FOwnerDocument);
  Result.FNodeValue := Copy(FNodeValue, offset + 1, Length);
  FNodeValue := Copy(FNodeValue, 1, offset);
  FParentNode.InsertBefore(Result, FNextSibling);
end;


// -------------------------------------------------------
//   Comment
// -------------------------------------------------------

constructor TDOMComment.Create(AOwner: TDOMDocument);
begin
  FNodeType := COMMENT_NODE;
  FNodeName := '#comment';
  inherited Create(AOwner);
end;

function TDOMComment.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := TDOMComment.Create(ACloneOwner);
  Result.FNodeValue := FNodeValue;
  if deep and (ACloneOwner=nil) then ;
end;


// -------------------------------------------------------
//   CDATASection
// -------------------------------------------------------

constructor TDOMCDATASection.Create(AOwner: TDOMDocument);
begin
  inherited Create(AOwner);
  FNodeType := CDATA_SECTION_NODE;
  FNodeName := '#cdata-section';
end;

function TDOMCDATASection.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := TDOMCDATASection.Create(ACloneOwner);
  Result.FNodeValue := FNodeValue;
  if deep and (ACloneOwner=nil) then ;
end;


// -------------------------------------------------------
//   DocumentType
// -------------------------------------------------------

constructor TDOMDocumentType.Create(AOwner: TDOMDocument);
begin
  FNodeType := DOCUMENT_TYPE_NODE;
  inherited Create(AOwner);
end;

function TDOMDocumentType.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := TDOMDocumentType.Create(ACloneOwner);
  Result.FNodeName := FNodeName;
  if deep and (ACloneOwner=nil) then ;
end;


// -------------------------------------------------------
//   Notation
// -------------------------------------------------------

constructor TDOMNotation.Create(AOwner: TDOMDocument);
begin
  FNodeType := NOTATION_NODE;
  inherited Create(AOwner);
end;

function TDOMNotation.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := TDOMNotation.Create(ACloneOwner);
  Result.FNodeName := FNodeName;
  if deep and (ACloneOwner=nil) then ;
end;


// -------------------------------------------------------
//   Entity
// -------------------------------------------------------

constructor TDOMEntity.Create(AOwner: TDOMDocument);
begin
  FNodeType := ENTITY_NODE;
  inherited Create(AOwner);
end;


// -------------------------------------------------------
//   EntityReference
// -------------------------------------------------------

constructor TDOMEntityReference.Create(AOwner: TDOMDocument);
begin
  FNodeType := ENTITY_REFERENCE_NODE;
  inherited Create(AOwner);
end;


// -------------------------------------------------------
//   ProcessingInstruction
// -------------------------------------------------------

constructor TDOMProcessingInstruction.Create(AOwner: TDOMDocument);
begin
  FNodeType := PROCESSING_INSTRUCTION_NODE;
  inherited Create(AOwner);
end;


end.


