{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    SVG node tree, attribute storage and reference resolution.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg.dom;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, System.Contnrs, FpCss.Tree,
     FpCss.ValueParser, FpCss.Resolver, fpsvg.types;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, contnrs, fpcsstree, fpcssresparser, fpcssresolver,
     fpsvg.types;
{$ENDIF FPC_DOTTEDUNITS}

const
  SVGNamespace = 'http://www.w3.org/2000/svg';
  XLinkNamespace = 'http://www.w3.org/1999/xlink';

type
  ESVGDOM = class(ESVGError);

  TSVGDocument = class;
  TSVGElement = class;
  TSVGElementClass = class of TSVGElement;

  { Base class for everything that can be a child of an element.
    The tree owns its nodes, and the CSS resolver only borrows them while a cascade runs. }
  TSVGNode = class(TObject)
  private
    FParent: TSVGElement;
    FDocument: TSVGDocument;
  public
    // A detached deep copy of this node, owned by the caller.
    function Clone: TSVGNode; virtual; abstract;
    // Points this node and its children at a document. The id index is left unchanged.
    procedure AdoptInto(aDocument: TSVGDocument); virtual;
    // Position among the parent's children, or -1 when there is no
    // parent.
    function NodeIndex: Integer;
    // Depth below the root element. The root itself is at zero.
    function Depth: Integer;
    // The element that contains this node, nil for the root.
    property Parent: TSVGElement read FParent;
    // The document that owns this node.
    property Document: TSVGDocument read FDocument;
  end;

  { Character data between element tags. }
  TSVGTextNode = class(TSVGNode)
  private
    FText: TSVGString;
  public
    constructor Create(const aText: TSVGString);
    function Clone: TSVGNode; override;
    // The character data as it appeared in the source.
    property Text: TSVGString read FText write FText;
  end;

  { An element with attributes and children, and the base class of every
    SVG element. The CSS resolver matches selectors against it. }
  TSVGElement = class(TSVGNode, ICSSNode)
  private
    FCSSOrigin: TSVGElement;
    FTagName: TSVGString;
    FChildren: TFPObjectList;
    FAttributes: TStringList;
    FClasses: TStringList;
    FClassIDs: TCSSNumericalIDArray;
    FClassIDStamp: TCSSNumericalID;
    function Resolver: TCSSResolver;
    function GetAttribute(const aName: TSVGString): TSVGString;
    function GetAttributeName(aIndex: Integer): TSVGString;
    function GetAttributeCount: Integer;
    function GetChild(aIndex: Integer): TSVGNode;
    function GetChildCount: Integer;
    function GetClassCount: Integer;
    function GetClassName(aIndex: Integer): TSVGString;
    function GetID: TSVGString;
    procedure SetAttribute(const aName, aValue: TSVGString);
  public
    constructor Create(const aTagName: TSVGString); virtual;
    destructor Destroy; override;
    // The tag this class registers for, empty for the fallback class.
    class function ElementName: TSVGString; virtual;
    // True when the element may contain rendered children.
    class function IsContainer: Boolean; virtual;

    // Appends a node and takes ownership of it.
    procedure AppendChild(aNode: TSVGNode);
    // Removes a node from the children and frees it.
    procedure RemoveChild(aNode: TSVGNode);
    // Frees aOld and puts aNew in its place, taking ownership of aNew.
    procedure ReplaceChild(aOld, aNew: TSVGNode);
    // Removes an attribute. Does nothing when the element has none of
    // that name.
    procedure RemoveAttribute(const aName: TSVGString);
    // True when the attribute is present, whatever its value.
    function HasAttribute(const aName: TSVGString): Boolean;
    // The attribute value, or aDefault when the attribute is absent.
    function AttributeDef(const aName, aDefault: TSVGString): TSVGString;
    // True when the element has the given class name.
    function HasClass(const aName: TSVGString): Boolean;
    // The first child element with the given tag, or nil.
    function FindChildElement(const aTagName: TSVGString): TSVGElement;
    // The next sibling that is an element, or nil.
    function NextElementSibling: TSVGElement;
    // The previous sibling that is an element, or nil.
    function PreviousElementSibling: TSVGElement;
    // All character data below this element, joined together.
    function TextContent: TSVGString;
    // Writes the subtree as indented text, one node per line.
    procedure DumpTo(aLines: TStrings; aIndent: Integer);
    function Clone: TSVGNode; override;
    procedure AdoptInto(aDocument: TSVGDocument); override;
    // The element this one was copied from when a use was expanded.
    property CSSOrigin: TSVGElement read FCSSOrigin write FCSSOrigin;

    { ICSSNode methods }
    function GetCSSID: TCSSNumericalID;
    function GetCSSTypeID: TCSSNumericalID;
    function GetCSSPseudoElementID: TCSSNumericalID;
    function GetCSSParent: ICSSNode;
    function GetCSSDepth: Integer;
    function GetCSSIndex: Integer;
    function GetCSSNextSibling: ICSSNode;
    function GetCSSPreviousSibling: ICSSNode;
    function GetCSSNextOfType: ICSSNode;
    function GetCSSPreviousOfType: ICSSNode;
    function GetCSSEmpty: Boolean;
    function GetCSSChildCount: Integer;
    function GetCSSChild(const anIndex: Integer): ICSSNode;
    function HasCSSClass(const aClassID: TCSSNumericalID): Boolean;
    function GetCSSClasses: TCSSNumericalIDArray;
    function GetCSSAttributeClass: TCSSString;
    function GetCSSAttributeID: TCSSString;
    function GetCSSCustomAttribute(const AttrID: TCSSNumericalID): TBytes;
    function HasCSSExplicitAttribute(const AttrID: TCSSNumericalID): Boolean;
    function GetCSSExplicitAttribute(const AttrID: TCSSNumericalID): TCSSString;
    function HasCSSPseudoClass(
      const aPseudoClassID: TCSSNumericalID): Boolean; virtual;

    // Tag name without its namespace prefix.
    property TagName: TSVGString read FTagName;
    // The value of the id attribute, empty when it is absent.
    property ID: TSVGString read GetID;
    // Attribute values by name. An absent attribute reads as an empty string.
    property Attributes[const aName: TSVGString]: TSVGString read GetAttribute write SetAttribute;
    // Number of attributes present.
    property AttributeCount: Integer read GetAttributeCount;
    // Attribute name by index, in the order the attributes were set.
    property AttributeNames[aIndex: Integer]: TSVGString read GetAttributeName;
    // Number of child nodes.
    property ChildCount: Integer read GetChildCount;
    // Child node by index.
    property Children[aIndex: Integer]: TSVGNode read GetChild; default;
    // Number of class names in the class attribute.
    property ClassCount: Integer read GetClassCount;
    // Class name by index.
    property ClassNames[aIndex: Integer]: TSVGString read GetClassName;
  end;

  { An element in a namespace other than SVG. It is kept, but never
    drawn. }
  TSVGForeignElement = class(TSVGElement)
  private
    FNamespace: TSVGString;
  public
    class function IsContainer: Boolean; override;
    function Clone: TSVGNode; override;
    // The namespace URI the element was declared in.
    property Namespace: TSVGString read FNamespace write FNamespace;
  end;

  { An element whose tag is not part of SVG 1.1. }
  TSVGUnknownElement = class(TSVGElement)
  public
    class function IsContainer: Boolean; override;
  end;

  { The outermost svg element, or a nested viewport. }
  TSVGSVGElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
    class function IsContainer: Boolean; override;
  end;

  TSVGGroupElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
    class function IsContainer: Boolean; override;
  end;

  TSVGDefsElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
    class function IsContainer: Boolean; override;
  end;

  TSVGSymbolElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
    class function IsContainer: Boolean; override;
  end;

  TSVGUseElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  { A link. It matches :link while it has a target to follow }
  TSVGAElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
    class function IsContainer: Boolean; override;
    function HasCSSPseudoClass(
      const aPseudoClassID: TCSSNumericalID): Boolean; override;
  end;

  TSVGSwitchElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
    class function IsContainer: Boolean; override;
  end;

  TSVGRectElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGCircleElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGEllipseElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGLineElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGPolylineElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGPolygonElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGPathElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGImageElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGTextElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
    class function IsContainer: Boolean; override;
  end;

  TSVGTSpanElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
    class function IsContainer: Boolean; override;
  end;

  TSVGTextPathElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
    class function IsContainer: Boolean; override;
  end;

  TSVGTRefElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
    class function IsContainer: Boolean; override;
  end;

  { A run of characters drawn with glyphs the document chooses itself }
  TSVGAltGlyphElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
    class function IsContainer: Boolean; override;
  end;

  { The glyphs of an altGlyph, in the order they are drawn. }
  TSVGAltGlyphDefElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
    class function IsContainer: Boolean; override;
  end;

  { One candidate list inside an altGlyphDef.
    The first list whose glyphs can all be found is the one that is used. }
  TSVGAltGlyphItemElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
    class function IsContainer: Boolean; override;
  end;

  { One glyph of such a list. }
  TSVGGlyphRefElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGLinearGradientElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGRadialGradientElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGStopElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGPatternElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
    class function IsContainer: Boolean; override;
  end;

  TSVGMarkerElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
    class function IsContainer: Boolean; override;
  end;

  TSVGClipPathElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
    class function IsContainer: Boolean; override;
  end;

  TSVGMaskElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
    class function IsContainer: Boolean; override;
  end;

  { A filter, and the primitives that make up its chain.
    A primitive has its own attributes }
  TSVGFilterElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
    class function IsContainer: Boolean; override;
  end;

  TSVGFeBlendElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGFeColorMatrixElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGFeComponentTransferElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
    class function IsContainer: Boolean; override;
  end;

  TSVGFeCompositeElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGFeConvolveMatrixElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGFeDiffuseLightingElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
    class function IsContainer: Boolean; override;
  end;

  TSVGFeDisplacementMapElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGFeDistantLightElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGFeFloodElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGFeFuncAElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGFeFuncBElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGFeFuncGElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGFeFuncRElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGFeGaussianBlurElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGFeImageElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGFeMergeElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
    class function IsContainer: Boolean; override;
  end;

  TSVGFeMergeNodeElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGFeMorphologyElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGFeOffsetElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGFePointLightElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGFeSpecularLightingElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
    class function IsContainer: Boolean; override;
  end;

  TSVGFeSpotLightElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGFeTileElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGFeTurbulenceElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGStyleElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGTitleElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGDescElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  TSVGMetadataElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  { A named view of the document: a viewBox and a preserveAspectRatio that
    a fragment identifier can select. The element itself draws nothing. }
  TSVGViewElement = class(TSVGElement)
  public
    class function ElementName: TSVGString; override;
  end;

  { A parsed document: the root element and its id index. }
  TSVGDocument = class(TObject)
  private
    FRoot: TSVGElement;
    FIDMap: TStringList;
    FBaseURI: String;
    FCSSResolver: TCSSResolver;
    FLinkHistory: ISVGLinkHistory;
    FSheetHRefs: TStringList;
    procedure SetRoot(aValue: TSVGElement);
    function GetSheetHRef(aIndex: Integer): TSVGString;
    function GetSheetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    // Records an element under its id. Any earlier element with that id is replaced.
    procedure RegisterID(aElement: TSVGElement);
    // Drops an id registration.
    procedure UnregisterID(const aID: TSVGString);
    // The element with the given id, or nil.
    function ElementByID(const aID: TSVGString): TSVGElement;
    // The element that "#id", "url(#id)" or a bare id refers to, or nil.
    function ResolveReference(const aReference: TSVGString): TSVGElement;
    // The whole tree as indented text, one node per line.
    function DumpTree: TSVGString;
    // Records a stylesheet of an xml-stylesheet instruction, in the order
    // the instructions appear.
    procedure AddStyleSheetHRef(const aHRef: TSVGString);
    // Number of stylesheets from xml-stylesheet instructions.
    property StyleSheetCount: Integer read GetSheetCount;
    // The href of one of them, in document order.
    property StyleSheetHRef[aIndex: Integer]: TSVGString read GetSheetHRef;
    // The root element, owned by the document.
    property Root: TSVGElement read FRoot write SetRoot;
    // Where the document was read from. Relative references resolve
    // against it.
    property BaseURI: String read FBaseURI write FBaseURI;
    // The resolver that elements answer selector queries against. Not owned.
    property CSSResolver: TCSSResolver read FCSSResolver write FCSSResolver;
    // Which of the links have been followed. Not owned.
    property LinkHistory: ISVGLinkHistory read FLinkHistory write FLinkHistory;
  end;

{ Supplies the documents that references in other files point to. }
{$INTERFACES CORBA}
type
  ISVGDocumentResolver = interface
    { The document for a reference, resolved against a base location.
      Returns nil when it cannot be read.
      The resolver keeps ownership. }
    function ResolveDocument(const aPath, aBaseURI: String): TSVGDocument;
  end;

  ISVGFontFileResolver = interface
    { The local file that has the specified font, resolved against a base location.
      Empty when there is no such file.}
    function ResolveFontFile(const aURL, aBaseURI: String): String;
  end;

  ISVGStyleSheetResolver = interface
    { The text of the stylesheet an href specifies, resolved against a base
      location. The text is bytes, in whatever encoding the sheet is
      written. Empty when it cannot be read. }
    function ResolveStyleSheet(const aHRef,
      aBaseURI: String): RawByteString;
  end;

// Adds an element class to the tag registry.
procedure RegisterSVGElement(aClass: TSVGElementClass);
// The class registered for a tag, or TSVGUnknownElement.
function SVGElementClass(const aTagName: TSVGString): TSVGElementClass;
// Creates an element of the class registered for the tag.
function CreateSVGElement(const aTagName: TSVGString): TSVGElement;
// Strips "url(...)" and any quotes from a reference, leaving path and hash.
function SVGReferenceText(const aReference: TSVGString): TSVGString;
// Strips "url(...)" and a leading '#' from a reference, leaving a bare id.
function SVGReferenceToID(const aReference: TSVGString): TSVGString;
// The part of a reference before the fragment, empty for a reference
// inside the same document.
function SVGReferencePath(const aReference: TSVGString): TSVGString;
// The fragment of a reference, without the hash character.
function SVGReferenceFragment(const aReference: TSVGString): TSVGString;
{ The base a reference on an element resolves against: the given base,
  with every xml:base from the root down to that element applied in turn,
  each one resolved against the base above it. }
function SVGElementBase(aElement: TSVGElement;
  const aDocumentBase: TSVGString): TSVGString;
// Number of tags in the element registry.
function SVGElementTagCount: Integer;
// A registered tag by index, in alphabetical order.
function SVGElementTag(aIndex: Integer): TSVGString;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses fpsvg.strings;
{$ELSE FPC_DOTTEDUNITS}
uses fpsvg.strings;
{$ENDIF FPC_DOTTEDUNITS}

var
  GElements: TStringList = nil;

function ElementRegistry: TStringList;

begin
  if GElements = nil then
    begin
    GElements := TStringList.Create;
    GElements.Sorted := True;
    GElements.Duplicates := dupError;
    GElements.CaseSensitive := True;
    end;
  Result := GElements;
end;


procedure RegisterSVGElement(aClass: TSVGElementClass);

begin
  if aClass.ElementName = '' then
    raise ESVGDOM.CreateFmt(SErrElementClassHasNoTag, [aClass.ClassName]);
  ElementRegistry.AddObject(aClass.ElementName, TObject(Pointer(aClass)));
end;


function SVGElementClass(const aTagName: TSVGString): TSVGElementClass;

var
  lIndex: Integer;

begin
  lIndex := ElementRegistry.IndexOf(aTagName);
  if lIndex = -1 then
    Result := TSVGUnknownElement
  else
    Result := TSVGElementClass(Pointer(ElementRegistry.Objects[lIndex]));
end;


function CreateSVGElement(const aTagName: TSVGString): TSVGElement;

begin
  Result := SVGElementClass(aTagName).Create(aTagName);
end;


function SVGElementTagCount: Integer;

begin
  Result := ElementRegistry.Count;
end;


function SVGElementTag(aIndex: Integer): TSVGString;

begin
  Result := ElementRegistry[aIndex];
end;


// The directory part of a location, empty when it has none.
function SVGBaseDirectory(const aBase: String): String;

var
  I: Integer;

begin
  Result := '';
  for I := Length(aBase) downto 1 do
    if aBase[I] = '/' then
      Exit(Copy(aBase, 1, I));
end;


function SVGElementBase(aElement: TSVGElement;
  const aDocumentBase: TSVGString): TSVGString;

var
  lChain: array of String;
  lAt: TSVGElement;
  lCount, I: Integer;
  lValue: String;

begin
  Result := aDocumentBase;
  SetLength(lChain, 4);
  lCount := 0;
  lAt := aElement;
  while lAt <> nil do
    begin
    lValue := Trim(lAt.AttributeDef('xml:base', ''));
    if lValue <> '' then
      begin
      if lCount = Length(lChain) then
        SetLength(lChain, lCount * 2);
      lChain[lCount] := lValue;
      Inc(lCount);
      end;
    lAt := lAt.Parent;
    end;
  // The tree was walked upwards, so the bases are applied in reverse
  // order.
  for I := lCount - 1 downto 0 do
    if (lChain[I][1] = '/') or (Pos('://', lChain[I]) > 0) then
      Result := lChain[I]
    else
      Result := SVGBaseDirectory(Result) + lChain[I];
end;


// A reference with the url() wrapper and the quotes removed.
function SVGReferenceText(const aReference: TSVGString): TSVGString;

var
  lOpen: Integer;

begin
  Result := Trim(aReference);
  if (Length(Result) > 4) and SameText(Copy(Result, 1, 4), 'url(') then
    begin
    lOpen := Length(Result);
    if Result[lOpen] = ')' then
      Dec(lOpen);
    Result := Trim(Copy(Result, 5, lOpen - 4));
    end;
  if (Result <> '') and (Result[1] = '''') and (Length(Result) > 1)
     and (Result[Length(Result)] = '''') then
    Result := Copy(Result, 2, Length(Result) - 2)
  else if (Result <> '') and (Result[1] = '"') and (Length(Result) > 1)
     and (Result[Length(Result)] = '"') then
    Result := Copy(Result, 2, Length(Result) - 2);
end;


function SVGReferenceToID(const aReference: TSVGString): TSVGString;

begin
  Result := SVGReferenceText(aReference);
  if (Result <> '') and (Result[1] = '#') then
    Delete(Result, 1, 1);
end;


function SVGReferencePath(const aReference: TSVGString): TSVGString;

var
  lHash: Integer;

begin
  Result := SVGReferenceText(aReference);
  lHash := Pos('#', Result);
  if lHash > 0 then
    Result := Copy(Result, 1, lHash - 1);
end;


function SVGReferenceFragment(const aReference: TSVGString): TSVGString;

var
  lHash: Integer;

begin
  Result := SVGReferenceText(aReference);
  lHash := Pos('#', Result);
  if lHash = 0 then
    Result := ''
  else
    Result := Copy(Result, lHash + 1, Length(Result));
end;


{ TSVGNode }

procedure TSVGNode.AdoptInto(aDocument: TSVGDocument);

begin
  FDocument := aDocument;
end;


function TSVGNode.NodeIndex: Integer;

begin
  if FParent = nil then
    Result := -1
  else
    Result := FParent.FChildren.IndexOf(Self);
end;


function TSVGNode.Depth: Integer;

var
  lNode: TSVGNode;

begin
  Result := 0;
  lNode := Self;
  while lNode.FParent <> nil do
    begin
    Inc(Result);
    lNode := lNode.FParent;
    end;
end;


{ TSVGTextNode }

constructor TSVGTextNode.Create(const aText: TSVGString);

begin
  inherited Create;
  FText := aText;
end;


function TSVGTextNode.Clone: TSVGNode;

begin
  Result := TSVGTextNode.Create(FText);
end;


{ TSVGElement }

constructor TSVGElement.Create(const aTagName: TSVGString);

begin
  inherited Create;
  FTagName := aTagName;
  FChildren := TFPObjectList.Create(True);
  FAttributes := TStringList.Create;
  FAttributes.CaseSensitive := True;
  FClasses := TStringList.Create;
  FClasses.CaseSensitive := True;
end;


destructor TSVGElement.Destroy;

begin
  FreeAndNil(FClasses);
  FreeAndNil(FAttributes);
  FreeAndNil(FChildren);
  inherited Destroy;
end;


class function TSVGElement.ElementName: TSVGString;

begin
  Result := '';
end;


class function TSVGElement.IsContainer: Boolean;

begin
  Result := False;
end;


function TSVGElement.GetAttribute(const aName: TSVGString): TSVGString;

begin
  Result := FAttributes.Values[aName];
end;


procedure TSVGElement.SetAttribute(const aName, aValue: TSVGString);

var
  lIndex: Integer;

begin
  lIndex := FAttributes.IndexOfName(aName);
  if lIndex = -1 then
    FAttributes.Add(aName + '=' + aValue)
  else
    FAttributes[lIndex] := aName + '=' + aValue;
  if aName = 'class' then
    begin
    FClasses.Clear;
    FClasses.Delimiter := ' ';
    FClasses.StrictDelimiter := False;
    FClasses.DelimitedText := aValue;
    end;
  if (aName = 'id') and (FDocument <> nil) then
    FDocument.RegisterID(Self);
end;


function TSVGElement.GetAttributeCount: Integer;

begin
  Result := FAttributes.Count;
end;


function TSVGElement.GetAttributeName(aIndex: Integer): TSVGString;

begin
  Result := FAttributes.Names[aIndex];
end;


function TSVGElement.GetChild(aIndex: Integer): TSVGNode;

begin
  Result := TSVGNode(FChildren[aIndex]);
end;


function TSVGElement.GetChildCount: Integer;

begin
  Result := FChildren.Count;
end;


function TSVGElement.GetClassCount: Integer;

begin
  Result := FClasses.Count;
end;


function TSVGElement.GetClassName(aIndex: Integer): TSVGString;

begin
  Result := FClasses[aIndex];
end;


function TSVGElement.GetID: TSVGString;

begin
  Result := FAttributes.Values['id'];
end;


procedure TSVGElement.AppendChild(aNode: TSVGNode);

begin
  if aNode = nil then
    raise ESVGDOM.Create(SErrAppendNilNode);
  if aNode.FParent <> nil then
    raise ESVGDOM.Create(SErrNodeHasParent);
  aNode.FParent := Self;
  aNode.FDocument := FDocument;
  FChildren.Add(aNode);
  if (aNode is TSVGElement) and (FDocument <> nil)
     and (TSVGElement(aNode).ID <> '') then
    FDocument.RegisterID(TSVGElement(aNode));
end;


procedure TSVGElement.RemoveChild(aNode: TSVGNode);

var
  lIndex: Integer;

begin
  lIndex := FChildren.IndexOf(aNode);
  if lIndex = -1 then
    raise ESVGDOM.Create(SErrNodeNotAChild);
  if (aNode is TSVGElement) and (FDocument <> nil)
     and (TSVGElement(aNode).ID <> '') then
    FDocument.UnregisterID(TSVGElement(aNode).ID);
  FChildren.Delete(lIndex);
end;


procedure TSVGElement.ReplaceChild(aOld, aNew: TSVGNode);

var
  lIndex: Integer;

begin
  lIndex := FChildren.IndexOf(aOld);
  if lIndex = -1 then
    raise ESVGDOM.Create(SErrNodeNotAChild);
  if aNew = nil then
    raise ESVGDOM.Create(SErrReplaceWithNothing);
  if aNew.FParent <> nil then
    raise ESVGDOM.Create(SErrReplacementHasParent);
  if (aOld is TSVGElement) and (FDocument <> nil)
     and (TSVGElement(aOld).ID <> '') then
    FDocument.UnregisterID(TSVGElement(aOld).ID);
  FChildren.Delete(lIndex);
  aNew.FParent := Self;
  aNew.FDocument := FDocument;
  FChildren.Insert(lIndex, aNew);
  if (aNew is TSVGElement) and (FDocument <> nil)
     and (TSVGElement(aNew).ID <> '') then
    FDocument.RegisterID(TSVGElement(aNew));
end;


procedure TSVGElement.RemoveAttribute(const aName: TSVGString);

var
  lIndex: Integer;
  lValue: String;

begin
  lIndex := FAttributes.IndexOfName(aName);
  if lIndex = -1 then
    Exit;
  lValue := FAttributes.ValueFromIndex[lIndex];
  FAttributes.Delete(lIndex);
  if aName = 'class' then
    FClasses.Clear;
  if (aName = 'id') and (FDocument <> nil) then
    FDocument.UnregisterID(lValue);
end;


function TSVGElement.HasAttribute(const aName: TSVGString): Boolean;

begin
  Result := FAttributes.IndexOfName(aName) <> -1;
end;


function TSVGElement.AttributeDef(const aName,
  aDefault: TSVGString): TSVGString;

begin
  if HasAttribute(aName) then
    Result := FAttributes.Values[aName]
  else
    Result := aDefault;
end;


function TSVGElement.HasClass(const aName: TSVGString): Boolean;

begin
  Result := FClasses.IndexOf(aName) <> -1;
end;


function TSVGElement.FindChildElement(
  const aTagName: TSVGString): TSVGElement;

var
  I: Integer;

begin
  for I := 0 to FChildren.Count - 1 do
    if (TSVGNode(FChildren[I]) is TSVGElement)
       and (TSVGElement(FChildren[I]).TagName = aTagName) then
      Exit(TSVGElement(FChildren[I]));
  Result := nil;
end;


function TSVGElement.NextElementSibling: TSVGElement;

var
  I: Integer;

begin
  Result := nil;
  if FParent = nil then
    Exit;
  for I := NodeIndex + 1 to FParent.ChildCount - 1 do
    if FParent[I] is TSVGElement then
      Exit(TSVGElement(FParent[I]));
end;


function TSVGElement.PreviousElementSibling: TSVGElement;

var
  I: Integer;

begin
  Result := nil;
  if FParent = nil then
    Exit;
  for I := NodeIndex - 1 downto 0 do
    if FParent[I] is TSVGElement then
      Exit(TSVGElement(FParent[I]));
end;


function TSVGElement.TextContent: TSVGString;

var
  I: Integer;
  lNode: TSVGNode;

begin
  Result := '';
  for I := 0 to FChildren.Count - 1 do
    begin
    lNode := TSVGNode(FChildren[I]);
    if lNode is TSVGTextNode then
      Result := Result + TSVGTextNode(lNode).Text
    else if lNode is TSVGElement then
      Result := Result + TSVGElement(lNode).TextContent;
    end;
end;


procedure TSVGElement.DumpTo(aLines: TStrings; aIndent: Integer);

var
  I: Integer;
  lLine, lPrefix: String;
  lNode: TSVGNode;

begin
  lPrefix := StringOfChar(' ', aIndent * 2);
  lLine := lPrefix + '<' + FTagName;
  for I := 0 to FAttributes.Count - 1 do
    lLine := lLine + ' ' + FAttributes.Names[I] + '="'
      + FAttributes.ValueFromIndex[I] + '"';
  lLine := lLine + '>';
  aLines.Add(lLine);
  for I := 0 to FChildren.Count - 1 do
    begin
    lNode := TSVGNode(FChildren[I]);
    if lNode is TSVGElement then
      TSVGElement(lNode).DumpTo(aLines, aIndent + 1)
    else if lNode is TSVGTextNode then
      aLines.Add(lPrefix + '  "' + TSVGTextNode(lNode).Text + '"');
    end;
end;


function TSVGElement.Resolver: TCSSResolver;

begin
  if FDocument = nil then
    Result := nil
  else
    Result := FDocument.CSSResolver;
end;


function TSVGElement.GetCSSID: TCSSNumericalID;

begin
  if (Resolver = nil) or (ID = '') then
    Result := CSSIDNone
  else
    Result := Resolver.GetCSSIDIndex(ID);
end;


function TSVGElement.GetCSSTypeID: TCSSNumericalID;

begin
  if Resolver = nil then
    Result := CSSIDNone
  else
    Result := Resolver.CSSRegistry.IndexOfTypeName(FTagName);
end;


function TSVGElement.GetCSSPseudoElementID: TCSSNumericalID;

begin
  Result := CSSIDNone;
end;


function TSVGElement.GetCSSParent: ICSSNode;

begin
  Result := FParent;
end;


function TSVGElement.GetCSSDepth: Integer;

begin
  Result := Depth;
end;


function TSVGElement.GetCSSIndex: Integer;

begin
  Result := NodeIndex;
end;


function TSVGElement.GetCSSNextSibling: ICSSNode;

begin
  Result := NextElementSibling;
end;


function TSVGElement.GetCSSPreviousSibling: ICSSNode;

begin
  Result := PreviousElementSibling;
end;


function TSVGElement.GetCSSNextOfType: ICSSNode;

var
  lSibling: TSVGElement;

begin
  Result := nil;
  lSibling := NextElementSibling;
  while lSibling <> nil do
    begin
    if lSibling.TagName = FTagName then
      Exit(lSibling);
    lSibling := lSibling.NextElementSibling;
    end;
end;


function TSVGElement.GetCSSPreviousOfType: ICSSNode;

var
  lSibling: TSVGElement;

begin
  Result := nil;
  lSibling := PreviousElementSibling;
  while lSibling <> nil do
    begin
    if lSibling.TagName = FTagName then
      Exit(lSibling);
    lSibling := lSibling.PreviousElementSibling;
    end;
end;


function TSVGElement.GetCSSEmpty: Boolean;

begin
  Result := FChildren.Count = 0;
end;


function TSVGElement.GetCSSChildCount: Integer;

var
  I: Integer;

begin
  Result := 0;
  for I := 0 to FChildren.Count - 1 do
    if TSVGNode(FChildren[I]) is TSVGElement then
      Inc(Result);
end;


function TSVGElement.GetCSSChild(const anIndex: Integer): ICSSNode;

var
  I, lSeen: Integer;

begin
  Result := nil;
  lSeen := 0;
  for I := 0 to FChildren.Count - 1 do
    if TSVGNode(FChildren[I]) is TSVGElement then
      begin
      if lSeen = anIndex then
        Exit(TSVGElement(FChildren[I]));
      Inc(lSeen);
      end;
end;


function TSVGElement.GetCSSClasses: TCSSNumericalIDArray;

var
  I: Integer;

begin
  if Resolver = nil then
    Exit(nil);
  if FClassIDStamp <> Resolver.CSSClassIDStamp then
    begin
    SetLength(FClassIDs, FClasses.Count);
    for I := 0 to FClasses.Count - 1 do
      FClassIDs[I] := Resolver.GetCSSClassID(FClasses[I]);
    FClassIDStamp := Resolver.CSSClassIDStamp;
    end;
  Result := FClassIDs;
end;


function TSVGElement.HasCSSClass(const aClassID: TCSSNumericalID): Boolean;

var
  I: Integer;
  lClasses: TCSSNumericalIDArray;

begin
  Result := False;
  if aClassID = CSSIDNone then
    Exit;
  lClasses := GetCSSClasses;
  for I := 0 to Length(lClasses) - 1 do
    if lClasses[I] = aClassID then
      Exit(True);
end;


function TSVGElement.GetCSSAttributeClass: TCSSString;

begin
  Result := Attributes['class'];
end;


function TSVGElement.GetCSSAttributeID: TCSSString;

begin
  Result := ID;
end;


function TSVGElement.GetCSSCustomAttribute(const AttrID: TCSSNumericalID): TBytes;

begin
  Result := nil;
end;


function TSVGElement.HasCSSExplicitAttribute(
  const AttrID: TCSSNumericalID): Boolean;

begin
  Result := (Resolver <> nil)
        and (AttrID > 0) and (AttrID < Resolver.CSSRegistry.AttributeCount)
        and HasAttribute(Resolver.CSSRegistry.Attributes[AttrID].Name);
end;


function TSVGElement.GetCSSExplicitAttribute(
  const AttrID: TCSSNumericalID): TCSSString;

begin
  if HasCSSExplicitAttribute(AttrID) then
    Result := Attributes[Resolver.CSSRegistry.Attributes[AttrID].Name]
  else
    Result := '';
end;


function TSVGElement.HasCSSPseudoClass(
  const aPseudoClassID: TCSSNumericalID): Boolean;

begin
  Result := False;
end;


procedure TSVGElement.AdoptInto(aDocument: TSVGDocument);

var
  I: Integer;

begin
  inherited AdoptInto(aDocument);
  for I := 0 to FChildren.Count - 1 do
    TSVGNode(FChildren[I]).AdoptInto(aDocument);
end;


function TSVGElement.Clone: TSVGNode;

var
  I: Integer;
  lCopy: TSVGElement;

begin
  lCopy := TSVGElementClass(ClassType).Create(FTagName);
  for I := 0 to FAttributes.Count - 1 do
    lCopy.Attributes[FAttributes.Names[I]] := FAttributes.ValueFromIndex[I];
  for I := 0 to FChildren.Count - 1 do
    lCopy.AppendChild(TSVGNode(FChildren[I]).Clone);
  Result := lCopy;
end;


function TSVGForeignElement.Clone: TSVGNode;

begin
  Result := inherited Clone;
  TSVGForeignElement(Result).Namespace := FNamespace;
end;


{ Element classes }

class function TSVGForeignElement.IsContainer: Boolean;

begin
  Result := False;
end;


class function TSVGUnknownElement.IsContainer: Boolean;

begin
  Result := False;
end;


class function TSVGSVGElement.ElementName: TSVGString;

begin
  Result := 'svg';
end;


class function TSVGSVGElement.IsContainer: Boolean;

begin
  Result := True;
end;


class function TSVGGroupElement.ElementName: TSVGString;

begin
  Result := 'g';
end;


class function TSVGGroupElement.IsContainer: Boolean;

begin
  Result := True;
end;


class function TSVGDefsElement.ElementName: TSVGString;

begin
  Result := 'defs';
end;


class function TSVGDefsElement.IsContainer: Boolean;

begin
  Result := True;
end;


class function TSVGSymbolElement.ElementName: TSVGString;

begin
  Result := 'symbol';
end;


class function TSVGSymbolElement.IsContainer: Boolean;

begin
  Result := True;
end;


class function TSVGUseElement.ElementName: TSVGString;

begin
  Result := 'use';
end;


class function TSVGSwitchElement.ElementName: TSVGString;

begin
  Result := 'switch';
end;


class function TSVGSwitchElement.IsContainer: Boolean;

begin
  Result := True;
end;


class function TSVGRectElement.ElementName: TSVGString;

begin
  Result := 'rect';
end;


class function TSVGCircleElement.ElementName: TSVGString;

begin
  Result := 'circle';
end;


class function TSVGEllipseElement.ElementName: TSVGString;

begin
  Result := 'ellipse';
end;


class function TSVGLineElement.ElementName: TSVGString;

begin
  Result := 'line';
end;


class function TSVGPolylineElement.ElementName: TSVGString;

begin
  Result := 'polyline';
end;


class function TSVGPolygonElement.ElementName: TSVGString;

begin
  Result := 'polygon';
end;


class function TSVGPathElement.ElementName: TSVGString;

begin
  Result := 'path';
end;


class function TSVGImageElement.ElementName: TSVGString;

begin
  Result := 'image';
end;


class function TSVGTextElement.ElementName: TSVGString;

begin
  Result := 'text';
end;


class function TSVGTextElement.IsContainer: Boolean;

begin
  Result := True;
end;


class function TSVGAElement.ElementName: TSVGString;

begin
  Result := 'a';
end;


class function TSVGAElement.IsContainer: Boolean;

begin
  Result := True;
end;


function TSVGAElement.HasCSSPseudoClass(
  const aPseudoClassID: TCSSNumericalID): Boolean;

var
  lID: TCSSNumericalID;
  lHRef: String;
  lVisited: Boolean;

begin
  Result := False;
  if (Document = nil) or (Document.CSSResolver = nil)
  or (Document.CSSResolver.CSSRegistry = nil) then
    Exit;
  lHRef := Attributes['href'];
  if lHRef = '' then
    lHRef := Attributes['xlink:href'];
  if lHRef = '' then
    Exit;
  lVisited := (Document.LinkHistory <> nil)
          and Document.LinkHistory.WasVisited(lHRef, Document.BaseURI);
  // The registry keeps the names without their leading colon.
  // A link is either :visited or :link, never both.
  if lVisited then
    lID := Document.CSSResolver.CSSRegistry.IndexOfPseudoClassName('visited')
  else
    lID := Document.CSSResolver.CSSRegistry.IndexOfPseudoClassName('link');
  Result := (lID > 0) and (aPseudoClassID = lID);
end;


class function TSVGTextPathElement.ElementName: TSVGString;

begin
  Result := 'textPath';
end;


class function TSVGTextPathElement.IsContainer: Boolean;

begin
  Result := True;
end;


class function TSVGTRefElement.ElementName: TSVGString;

begin
  Result := 'tref';
end;


class function TSVGTRefElement.IsContainer: Boolean;

begin
  Result := True;
end;


class function TSVGTSpanElement.ElementName: TSVGString;

begin
  Result := 'tspan';
end;


class function TSVGTSpanElement.IsContainer: Boolean;

begin
  Result := True;
end;


class function TSVGAltGlyphElement.ElementName: TSVGString;

begin
  Result := 'altGlyph';
end;


class function TSVGAltGlyphElement.IsContainer: Boolean;

begin
  Result := True;
end;


class function TSVGAltGlyphDefElement.ElementName: TSVGString;

begin
  Result := 'altGlyphDef';
end;


class function TSVGAltGlyphDefElement.IsContainer: Boolean;

begin
  Result := True;
end;


class function TSVGAltGlyphItemElement.ElementName: TSVGString;

begin
  Result := 'altGlyphItem';
end;


class function TSVGAltGlyphItemElement.IsContainer: Boolean;

begin
  Result := True;
end;


class function TSVGGlyphRefElement.ElementName: TSVGString;

begin
  Result := 'glyphRef';
end;


class function TSVGLinearGradientElement.ElementName: TSVGString;

begin
  Result := 'linearGradient';
end;


class function TSVGRadialGradientElement.ElementName: TSVGString;

begin
  Result := 'radialGradient';
end;


class function TSVGStopElement.ElementName: TSVGString;

begin
  Result := 'stop';
end;


class function TSVGPatternElement.ElementName: TSVGString;

begin
  Result := 'pattern';
end;


class function TSVGPatternElement.IsContainer: Boolean;

begin
  Result := True;
end;


class function TSVGMarkerElement.ElementName: TSVGString;

begin
  Result := 'marker';
end;


class function TSVGMarkerElement.IsContainer: Boolean;

begin
  Result := True;
end;


class function TSVGClipPathElement.ElementName: TSVGString;

begin
  Result := 'clipPath';
end;


class function TSVGClipPathElement.IsContainer: Boolean;

begin
  Result := True;
end;


class function TSVGMaskElement.ElementName: TSVGString;

begin
  Result := 'mask';
end;


class function TSVGMaskElement.IsContainer: Boolean;

begin
  Result := True;
end;


class function TSVGFilterElement.ElementName: TSVGString;

begin
  Result := 'filter';
end;


class function TSVGFilterElement.IsContainer: Boolean;

begin
  Result := True;
end;


class function TSVGFeBlendElement.ElementName: TSVGString;

begin
  Result := 'feBlend';
end;


class function TSVGFeColorMatrixElement.ElementName: TSVGString;

begin
  Result := 'feColorMatrix';
end;


class function TSVGFeComponentTransferElement.ElementName: TSVGString;

begin
  Result := 'feComponentTransfer';
end;


class function TSVGFeComponentTransferElement.IsContainer: Boolean;

begin
  Result := True;
end;


class function TSVGFeCompositeElement.ElementName: TSVGString;

begin
  Result := 'feComposite';
end;


class function TSVGFeConvolveMatrixElement.ElementName: TSVGString;

begin
  Result := 'feConvolveMatrix';
end;


class function TSVGFeDiffuseLightingElement.ElementName: TSVGString;

begin
  Result := 'feDiffuseLighting';
end;


class function TSVGFeDiffuseLightingElement.IsContainer: Boolean;

begin
  Result := True;
end;


class function TSVGFeDisplacementMapElement.ElementName: TSVGString;

begin
  Result := 'feDisplacementMap';
end;


class function TSVGFeDistantLightElement.ElementName: TSVGString;

begin
  Result := 'feDistantLight';
end;


class function TSVGFeFloodElement.ElementName: TSVGString;

begin
  Result := 'feFlood';
end;


class function TSVGFeFuncAElement.ElementName: TSVGString;

begin
  Result := 'feFuncA';
end;


class function TSVGFeFuncBElement.ElementName: TSVGString;

begin
  Result := 'feFuncB';
end;


class function TSVGFeFuncGElement.ElementName: TSVGString;

begin
  Result := 'feFuncG';
end;


class function TSVGFeFuncRElement.ElementName: TSVGString;

begin
  Result := 'feFuncR';
end;


class function TSVGFeGaussianBlurElement.ElementName: TSVGString;

begin
  Result := 'feGaussianBlur';
end;


class function TSVGFeImageElement.ElementName: TSVGString;

begin
  Result := 'feImage';
end;


class function TSVGFeMergeElement.ElementName: TSVGString;

begin
  Result := 'feMerge';
end;


class function TSVGFeMergeElement.IsContainer: Boolean;

begin
  Result := True;
end;


class function TSVGFeMergeNodeElement.ElementName: TSVGString;

begin
  Result := 'feMergeNode';
end;


class function TSVGFeMorphologyElement.ElementName: TSVGString;

begin
  Result := 'feMorphology';
end;


class function TSVGFeOffsetElement.ElementName: TSVGString;

begin
  Result := 'feOffset';
end;


class function TSVGFePointLightElement.ElementName: TSVGString;

begin
  Result := 'fePointLight';
end;


class function TSVGFeSpecularLightingElement.ElementName: TSVGString;

begin
  Result := 'feSpecularLighting';
end;


class function TSVGFeSpecularLightingElement.IsContainer: Boolean;

begin
  Result := True;
end;


class function TSVGFeSpotLightElement.ElementName: TSVGString;

begin
  Result := 'feSpotLight';
end;


class function TSVGFeTileElement.ElementName: TSVGString;

begin
  Result := 'feTile';
end;


class function TSVGFeTurbulenceElement.ElementName: TSVGString;

begin
  Result := 'feTurbulence';
end;


class function TSVGStyleElement.ElementName: TSVGString;

begin
  Result := 'style';
end;


class function TSVGTitleElement.ElementName: TSVGString;

begin
  Result := 'title';
end;


class function TSVGDescElement.ElementName: TSVGString;

begin
  Result := 'desc';
end;


class function TSVGMetadataElement.ElementName: TSVGString;

begin
  Result := 'metadata';
end;


class function TSVGViewElement.ElementName: TSVGString;

begin
  Result := 'view';
end;


{ TSVGDocument }

constructor TSVGDocument.Create;

begin
  inherited Create;
  FIDMap := TStringList.Create;
  FIDMap.Sorted := True;
  FIDMap.Duplicates := dupIgnore;
  FIDMap.CaseSensitive := True;
  FSheetHRefs := TStringList.Create;
end;


destructor TSVGDocument.Destroy;

begin
  FreeAndNil(FRoot);
  FreeAndNil(FIDMap);
  FreeAndNil(FSheetHRefs);
  inherited Destroy;
end;


procedure TSVGDocument.AddStyleSheetHRef(const aHRef: TSVGString);

begin
  if Trim(aHRef) <> '' then
    FSheetHRefs.Add(Trim(aHRef));
end;


function TSVGDocument.GetSheetCount: Integer;

begin
  Result := FSheetHRefs.Count;
end;


function TSVGDocument.GetSheetHRef(aIndex: Integer): TSVGString;

begin
  Result := FSheetHRefs[aIndex];
end;


procedure TSVGDocument.SetRoot(aValue: TSVGElement);

begin
  if FRoot = aValue then
    Exit;
  FreeAndNil(FRoot);
  FIDMap.Clear;
  FRoot := aValue;
  if FRoot <> nil then
    begin
    FRoot.FDocument := Self;
    if FRoot.ID <> '' then
      RegisterID(FRoot);
    end;
end;


procedure TSVGDocument.RegisterID(aElement: TSVGElement);

var
  lIndex: Integer;

begin
  if aElement.ID = '' then
    Exit;
  lIndex := FIDMap.IndexOf(aElement.ID);
  if lIndex = -1 then
    FIDMap.AddObject(aElement.ID, aElement)
  else
    FIDMap.Objects[lIndex] := aElement;
end;


procedure TSVGDocument.UnregisterID(const aID: TSVGString);

var
  lIndex: Integer;

begin
  lIndex := FIDMap.IndexOf(aID);
  if lIndex <> -1 then
    FIDMap.Delete(lIndex);
end;


function TSVGDocument.ElementByID(const aID: TSVGString): TSVGElement;

var
  lIndex: Integer;

begin
  lIndex := FIDMap.IndexOf(aID);
  if lIndex = -1 then
    Result := nil
  else
    Result := TSVGElement(FIDMap.Objects[lIndex]);
end;


function TSVGDocument.ResolveReference(
  const aReference: TSVGString): TSVGElement;

var
  lID: String;

begin
  lID := SVGReferenceToID(aReference);
  if lID = '' then
    Result := nil
  else
    Result := ElementByID(lID);
end;


function TSVGDocument.DumpTree: TSVGString;

var
  lLines: TStringList;

begin
  lLines := TStringList.Create;
  try
    if FRoot <> nil then
      FRoot.DumpTo(lLines, 0);
    Result := lLines.Text;
  finally
    lLines.Free;
  end;
end;


procedure RegisterStandardElements;

begin
  RegisterSVGElement(TSVGSVGElement);
  RegisterSVGElement(TSVGGroupElement);
  RegisterSVGElement(TSVGDefsElement);
  RegisterSVGElement(TSVGSymbolElement);
  RegisterSVGElement(TSVGUseElement);
  RegisterSVGElement(TSVGSwitchElement);
  RegisterSVGElement(TSVGAElement);
  RegisterSVGElement(TSVGRectElement);
  RegisterSVGElement(TSVGCircleElement);
  RegisterSVGElement(TSVGEllipseElement);
  RegisterSVGElement(TSVGLineElement);
  RegisterSVGElement(TSVGPolylineElement);
  RegisterSVGElement(TSVGPolygonElement);
  RegisterSVGElement(TSVGPathElement);
  RegisterSVGElement(TSVGImageElement);
  RegisterSVGElement(TSVGTextElement);
  RegisterSVGElement(TSVGTSpanElement);
  RegisterSVGElement(TSVGTextPathElement);
  RegisterSVGElement(TSVGTRefElement);
  RegisterSVGElement(TSVGAltGlyphElement);
  RegisterSVGElement(TSVGAltGlyphDefElement);
  RegisterSVGElement(TSVGAltGlyphItemElement);
  RegisterSVGElement(TSVGGlyphRefElement);
  RegisterSVGElement(TSVGLinearGradientElement);
  RegisterSVGElement(TSVGRadialGradientElement);
  RegisterSVGElement(TSVGStopElement);
  RegisterSVGElement(TSVGPatternElement);
  RegisterSVGElement(TSVGMarkerElement);
  RegisterSVGElement(TSVGClipPathElement);
  RegisterSVGElement(TSVGMaskElement);
  RegisterSVGElement(TSVGFilterElement);
  RegisterSVGElement(TSVGFeBlendElement);
  RegisterSVGElement(TSVGFeColorMatrixElement);
  RegisterSVGElement(TSVGFeComponentTransferElement);
  RegisterSVGElement(TSVGFeCompositeElement);
  RegisterSVGElement(TSVGFeConvolveMatrixElement);
  RegisterSVGElement(TSVGFeDiffuseLightingElement);
  RegisterSVGElement(TSVGFeDisplacementMapElement);
  RegisterSVGElement(TSVGFeDistantLightElement);
  RegisterSVGElement(TSVGFeFloodElement);
  RegisterSVGElement(TSVGFeFuncAElement);
  RegisterSVGElement(TSVGFeFuncBElement);
  RegisterSVGElement(TSVGFeFuncGElement);
  RegisterSVGElement(TSVGFeFuncRElement);
  RegisterSVGElement(TSVGFeGaussianBlurElement);
  RegisterSVGElement(TSVGFeImageElement);
  RegisterSVGElement(TSVGFeMergeElement);
  RegisterSVGElement(TSVGFeMergeNodeElement);
  RegisterSVGElement(TSVGFeMorphologyElement);
  RegisterSVGElement(TSVGFeOffsetElement);
  RegisterSVGElement(TSVGFePointLightElement);
  RegisterSVGElement(TSVGFeSpecularLightingElement);
  RegisterSVGElement(TSVGFeSpotLightElement);
  RegisterSVGElement(TSVGFeTileElement);
  RegisterSVGElement(TSVGFeTurbulenceElement);
  RegisterSVGElement(TSVGStyleElement);
  RegisterSVGElement(TSVGTitleElement);
  RegisterSVGElement(TSVGDescElement);
  RegisterSVGElement(TSVGMetadataElement);
  RegisterSVGElement(TSVGViewElement);
end;


initialization
  RegisterStandardElements;

finalization
  FreeAndNil(GElements);
end.
