{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2023 by Michael Van Canneyt (michael@freepascal.org)

    This file contains CSS utility class

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpCSSResParser;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}
{$Interfaces CORBA}
{$ModeSwitch AdvancedRecords}

{$IF FPC_FULLVERSION>30300}
  {$WARN 6060 off} // Case statement does not handle all possible cases
{$ENDIF}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Math, System.Contnrs, System.StrUtils,
  Fcl.AVLTree, FpCss.Tree, FpCss.Scanner, FpCss.Parser;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, Math, Contnrs, AVL_Tree, fpCSSTree, fpCSSScanner,
  fpCSSParser;
{$ENDIF FPC_DOTTEDUNITS}

const
  CSSIDNone = 0;
  // built-in attribute IDs
  CSSAttributeID_ID = 1; // id of attribute key 'id'
  CSSAttributeID_Class = 2; // id of attribute key 'class'
  CSSAttributeID_All = 3; // id of attribute key 'all'
  CSSAttributeID_LastResolver = CSSAttributeID_All;

  // built-in type IDs
  CSSTypeID_Universal = 1; // id of type '*'
  CSSTypeID_LastResolver = CSSTypeID_Universal;

  // built-in pseudo class IDs
  CSSPseudoID_Root = 1; // :root
  CSSPseudoID_Empty = CSSPseudoID_Root+1; // :empty
  CSSPseudoID_FirstChild = CSSPseudoID_Empty+1; // :first-child
  CSSPseudoID_LastChild = CSSPseudoID_FirstChild+1; // :last-child
  CSSPseudoID_OnlyChild = CSSPseudoID_LastChild+1; // :only-child
  CSSPseudoID_FirstOfType = CSSPseudoID_OnlyChild+1; // :first-of-type
  CSSPseudoID_LastOfType = CSSPseudoID_FirstOfType+1; // :last-of-type
  CSSPseudoID_OnlyOfType = CSSPseudoID_LastOfType+1; // :only-of-type
  CSSPseudoID_LastResolver = CSSPseudoID_OnlyOfType;

  CSSPseudoClassNames: array[0..CSSPseudoID_LastResolver] of TCSSString = (
    '?',
    ':root',
    ':empty',
    ':first-child',
    ':last-child',
    ':only-child',
    ':first-of-type',
    ':last-of-type',
    ':only-of-type'
    );

  // built-in pseudo function IDs
  CSSCallID_Not = 1; // :not()
  CSSCallID_Is = CSSCallID_Not+1; // :is()
  CSSCallID_Where = CSSCallID_Is+1; // :where()
  CSSCallID_Has = CSSCallID_Where+1; // :has()
  CSSCallID_NthChild = CSSCallID_Has+1; // :nth-child(n)
  CSSCallID_NthLastChild = CSSCallID_NthChild+1; // :nth-last-child(n)
  CSSCallID_NthOfType = CSSCallID_NthLastChild+1; // :nth-of-type(n)
  CSSCallID_NthLastOfType = CSSCallID_NthOfType+1; // :nth-last-of-type(n)
  CSSCallID_LastResolver = CSSCallID_NthLastOfType;

  CSSSelectorCallNames: array[0..CSSCallID_LastResolver] of TCSSString = (
    '?',
    ':not()',
    ':is()',
    ':where()',
    ':has()',
    ':nth-child(n)',
    ':nth-last-child(n)',
    ':nth-of-type(n)',
    ':nth-last-of-type(n)'
    );

  // keywords
  CSSKeywordNone = 1;
  CSSKeywordInitial = CSSKeywordNone+1;
  CSSKeywordInherit = CSSKeywordInitial+1;
  CSSKeywordUnset = CSSKeywordInherit+1;
  CSSKeywordRevert = CSSKeywordUnset+1;
  CSSKeywordRevertLayer = CSSKeywordRevert+1;
  CSSKeywordAuto = CSSKeywordRevertLayer+1;
  CSSKeywordAnd = CSSKeywordAuto+1;
  CSSKeywordOr = CSSKeywordAnd+1;
  CSSKeywordNot = CSSKeywordOr+1;
  CSSKeyword_LastResolver = CSSKeywordNot;

  // attribute functions
  CSSAttrFuncVar = 1;
  CSSAttrFuncRGB = 2;
  CSSAttrFuncRGBA = 3;

  CSSMinSafeIntDouble = -$1fffffffffffff; // -9007199254740991 54 bits (52 plus signed bit plus implicit highest bit)
  CSSMaxSafeIntDouble =  $1fffffffffffff; //  9007199254740991

type
  TCSSMsgID = int64; // used for message numbers, e.g. error codes
  TCSSNumericalID = integer; // used for IDs of each type and attribute
  TCSSNumericalIDArray = array of TCSSNumericalID;
  TBytesArray = array of TBytes;

  TCSSNumericalIDKind = (
    nikAttribute,
    nikPseudoClass, // e.g. "hover" of ":hover"
    nikPseudoElement, // e.g. "first-line" of "::first-line"
    nikPseudoFunction, // e.g. "is" of ":is()"
    nikType,
    nikKeyword,
    nikAttrFunction // e.g. "calc" of "calc()"
    );
  TCSSNumericalIDs = set of TCSSNumericalIDKind;

const
  nikAllDescriptors = [nikAttribute,nikPseudoClass,nikType]; // all items having a descriptor

  CSSNumericalIDKindNames: array[TCSSNumericalIDKind] of TCSSString = (
    'Type',
    'PseudoClass',
    'PseudoElement',
    'PseudoFunction',
    'Attribute',
    'Keyword',
    'AttributeFunction'
    );

type
  TCSSAlphaColor = DWord;

  TCSSNamedColor = record
    Name: TCSSString;
    Color: TCSSAlphaColor;
  end;
const
  CSSNamedColors: array[0..148] of TCSSNamedColor = (
    (Name: 'aliceblue'; Color: TCSSAlphaColor($fff0f8ff)),
    (Name: 'antiquewhite'; Color: TCSSAlphaColor($fffaebd7)),
    (Name: 'aqua'; Color: TCSSAlphaColor($ff00ffff)),
    (Name: 'aquamarine'; Color: TCSSAlphaColor($ff7fffd4)),
    (Name: 'azure'; Color: TCSSAlphaColor($fff0ffff)),
    (Name: 'beige'; Color: TCSSAlphaColor($fff5f5dc)),
    (Name: 'bisque'; Color: TCSSAlphaColor($ffffe4c4)),
    (Name: 'black'; Color: TCSSAlphaColor($ff000000)),
    (Name: 'blanchedalmond'; Color: TCSSAlphaColor($ffffebcd)),
    (Name: 'blue'; Color: TCSSAlphaColor($ff0000ff)),
    (Name: 'blueviolet'; Color: TCSSAlphaColor($ff8a2be2)),
    (Name: 'brown'; Color: TCSSAlphaColor($ffa52a2a)),
    (Name: 'burlywood'; Color: TCSSAlphaColor($ffdeb887)),
    (Name: 'cadetblue'; Color: TCSSAlphaColor($ff5f9ea0)),
    (Name: 'chartreuse'; Color: TCSSAlphaColor($ff7fff00)),
    (Name: 'chocolate'; Color: TCSSAlphaColor($ffd2691e)),
    (Name: 'coral'; Color: TCSSAlphaColor($ffff7f50)),
    (Name: 'cornflowerblue'; Color: TCSSAlphaColor($ff6495ed)),
    (Name: 'cornsilk'; Color: TCSSAlphaColor($fffff8dc)),
    (Name: 'crimson'; Color: TCSSAlphaColor($ffdc143c)),
    (Name: 'cyan'; Color: TCSSAlphaColor($ff00ffff)),
    (Name: 'darkblue'; Color: TCSSAlphaColor($ff00008b)),
    (Name: 'darkcyan'; Color: TCSSAlphaColor($ff008b8b)),
    (Name: 'darkgoldenrod'; Color: TCSSAlphaColor($ffb8860b)),
    (Name: 'darkgray'; Color: TCSSAlphaColor($ffa9a9a9)),
    (Name: 'darkgreen'; Color: TCSSAlphaColor($ff006400)),
    (Name: 'darkgrey'; Color: TCSSAlphaColor($ffa9a9a9)),
    (Name: 'darkkhaki'; Color: TCSSAlphaColor($ffbdb76b)),
    (Name: 'darkmagenta'; Color: TCSSAlphaColor($ff8b008b)),
    (Name: 'darkolivegreen'; Color: TCSSAlphaColor($ff556b2f)),
    (Name: 'darkorange'; Color: TCSSAlphaColor($ffff8c00)),
    (Name: 'darkorchid'; Color: TCSSAlphaColor($ff9932cc)),
    (Name: 'darkred'; Color: TCSSAlphaColor($ff8b0000)),
    (Name: 'darksalmon'; Color: TCSSAlphaColor($ffe9967a)),
    (Name: 'darkseagreen'; Color: TCSSAlphaColor($ff8fbc8f)),
    (Name: 'darkslateblue'; Color: TCSSAlphaColor($ff483d8b)),
    (Name: 'darkslategray'; Color: TCSSAlphaColor($ff2f4f4f)),
    (Name: 'darkslategrey'; Color: TCSSAlphaColor($ff2f4f4f)),
    (Name: 'darkturquoise'; Color: TCSSAlphaColor($ff00ced1)),
    (Name: 'darkviolet'; Color: TCSSAlphaColor($ff9400d3)),
    (Name: 'deeppink'; Color: TCSSAlphaColor($ffff1493)),
    (Name: 'deepskyblue'; Color: TCSSAlphaColor($ff00bfff)),
    (Name: 'dimgray'; Color: TCSSAlphaColor($ff696969)),
    (Name: 'dimgrey'; Color: TCSSAlphaColor($ff696969)),
    (Name: 'dodgerblue'; Color: TCSSAlphaColor($ff1e90ff)),
    (Name: 'firebrick'; Color: TCSSAlphaColor($ffb22222)),
    (Name: 'floralwhite'; Color: TCSSAlphaColor($fffffaf0)),
    (Name: 'forestgreen'; Color: TCSSAlphaColor($ff228b22)),
    (Name: 'fuchsia'; Color: TCSSAlphaColor($ffff00ff)),
    (Name: 'gainsboro'; Color: TCSSAlphaColor($ffdcdcdc)),
    (Name: 'ghostwhite'; Color: TCSSAlphaColor($fff8f8ff)),
    (Name: 'gold'; Color: TCSSAlphaColor($ffffd700)),
    (Name: 'goldenrod'; Color: TCSSAlphaColor($ffdaa520)),
    (Name: 'gray'; Color: TCSSAlphaColor($ff808080)),
    (Name: 'green'; Color: TCSSAlphaColor($ff008000)),
    (Name: 'greenyellow'; Color: TCSSAlphaColor($ffadff2f)),
    (Name: 'grey'; Color: TCSSAlphaColor($ff808080)),
    (Name: 'honeydew'; Color: TCSSAlphaColor($fff0fff0)),
    (Name: 'hotpink'; Color: TCSSAlphaColor($ffff69b4)),
    (Name: 'indianred'; Color: TCSSAlphaColor($ffcd5c5c)),
    (Name: 'indigo'; Color: TCSSAlphaColor($ff4b0082)),
    (Name: 'ivory'; Color: TCSSAlphaColor($fffffff0)),
    (Name: 'khaki'; Color: TCSSAlphaColor($fff0e68c)),
    (Name: 'lavender'; Color: TCSSAlphaColor($ffe6e6fa)),
    (Name: 'lavenderblush'; Color: TCSSAlphaColor($fffff0f5)),
    (Name: 'lawngreen'; Color: TCSSAlphaColor($ff7cfc00)),
    (Name: 'lemonchiffon'; Color: TCSSAlphaColor($fffffacd)),
    (Name: 'lightblue'; Color: TCSSAlphaColor($ffadd8e6)),
    (Name: 'lightcoral'; Color: TCSSAlphaColor($fff08080)),
    (Name: 'lightcyan'; Color: TCSSAlphaColor($ffe0ffff)),
    (Name: 'lightgoldenrodyellow'; Color: TCSSAlphaColor($fffafad2)),
    (Name: 'lightgray'; Color: TCSSAlphaColor($ffd3d3d3)),
    (Name: 'lightgreen'; Color: TCSSAlphaColor($ff90ee90)),
    (Name: 'lightgrey'; Color: TCSSAlphaColor($ffd3d3d3)),
    (Name: 'lightpink'; Color: TCSSAlphaColor($ffffb6c1)),
    (Name: 'lightsalmon'; Color: TCSSAlphaColor($ffffa07a)),
    (Name: 'lightseagreen'; Color: TCSSAlphaColor($ff20b2aa)),
    (Name: 'lightskyblue'; Color: TCSSAlphaColor($ff87cefa)),
    (Name: 'lightslategray'; Color: TCSSAlphaColor($ff778899)),
    (Name: 'lightslategrey'; Color: TCSSAlphaColor($ff778899)),
    (Name: 'lightsteelblue'; Color: TCSSAlphaColor($ffb0c4de)),
    (Name: 'lightyellow'; Color: TCSSAlphaColor($ffffffe0)),
    (Name: 'lime'; Color: TCSSAlphaColor($ff00ff00)),
    (Name: 'limegreen'; Color: TCSSAlphaColor($ff32cd32)),
    (Name: 'linen'; Color: TCSSAlphaColor($fffaf0e6)),
    (Name: 'magenta'; Color: TCSSAlphaColor($ffff00ff)),
    (Name: 'maroon'; Color: TCSSAlphaColor($ff800000)),
    (Name: 'mediumaquamarine'; Color: TCSSAlphaColor($ff66cdaa)),
    (Name: 'mediumblue'; Color: TCSSAlphaColor($ff0000cd)),
    (Name: 'mediumorchid'; Color: TCSSAlphaColor($ffba55d3)),
    (Name: 'mediumpurple'; Color: TCSSAlphaColor($ff9370db)),
    (Name: 'mediumseagreen'; Color: TCSSAlphaColor($ff3cb371)),
    (Name: 'mediumslateblue'; Color: TCSSAlphaColor($ff7b68ee)),
    (Name: 'mediumspringgreen'; Color: TCSSAlphaColor($ff00fa9a)),
    (Name: 'mediumturquoise'; Color: TCSSAlphaColor($ff48d1cc)),
    (Name: 'mediumvioletred'; Color: TCSSAlphaColor($ffc71585)),
    (Name: 'midnightblue'; Color: TCSSAlphaColor($ff191970)),
    (Name: 'mintcream'; Color: TCSSAlphaColor($fff5fffa)),
    (Name: 'mistyrose'; Color: TCSSAlphaColor($ffffe4e1)),
    (Name: 'moccasin'; Color: TCSSAlphaColor($ffffe4b5)),
    (Name: 'navajowhite'; Color: TCSSAlphaColor($ffffdead)),
    (Name: 'navy'; Color: TCSSAlphaColor($ff000080)),
    (Name: 'oldlace'; Color: TCSSAlphaColor($fffdf5e6)),
    (Name: 'olive'; Color: TCSSAlphaColor($ff808000)),
    (Name: 'olivedrab'; Color: TCSSAlphaColor($ff6b8e23)),
    (Name: 'orange'; Color: TCSSAlphaColor($ffffa500)),
    (Name: 'orangered'; Color: TCSSAlphaColor($ffff4500)),
    (Name: 'orchid'; Color: TCSSAlphaColor($ffda70d6)),
    (Name: 'palegoldenrod'; Color: TCSSAlphaColor($ffeee8aa)),
    (Name: 'palegreen'; Color: TCSSAlphaColor($ff98fb98)),
    (Name: 'paleturquoise'; Color: TCSSAlphaColor($ffafeeee)),
    (Name: 'palevioletred'; Color: TCSSAlphaColor($ffdb7093)),
    (Name: 'papayawhip'; Color: TCSSAlphaColor($ffffefd5)),
    (Name: 'peachpuff'; Color: TCSSAlphaColor($ffffdab9)),
    (Name: 'peru'; Color: TCSSAlphaColor($ffcd853f)),
    (Name: 'pink'; Color: TCSSAlphaColor($ffffc0cb)),
    (Name: 'plum'; Color: TCSSAlphaColor($ffdda0dd)),
    (Name: 'powderblue'; Color: TCSSAlphaColor($ffb0e0e6)),
    (Name: 'purple'; Color: TCSSAlphaColor($ff800080)),
    (Name: 'rebeccapurple'; Color: TCSSAlphaColor($ff663399)),
    (Name: 'red'; Color: TCSSAlphaColor($ffff0000)),
    (Name: 'rosybrown'; Color: TCSSAlphaColor($ffbc8f8f)),
    (Name: 'royalblue'; Color: TCSSAlphaColor($ff4169e1)),
    (Name: 'saddlebrown'; Color: TCSSAlphaColor($ff8b4513)),
    (Name: 'salmon'; Color: TCSSAlphaColor($fffa8072)),
    (Name: 'sandybrown'; Color: TCSSAlphaColor($fff4a460)),
    (Name: 'seagreen'; Color: TCSSAlphaColor($ff2e8b57)),
    (Name: 'seashell'; Color: TCSSAlphaColor($fffff5ee)),
    (Name: 'sienna'; Color: TCSSAlphaColor($ffa0522d)),
    (Name: 'silver'; Color: TCSSAlphaColor($ffc0c0c0)),
    (Name: 'skyblue'; Color: TCSSAlphaColor($ff87ceeb)),
    (Name: 'slateblue'; Color: TCSSAlphaColor($ff6a5acd)),
    (Name: 'slategray'; Color: TCSSAlphaColor($ff708090)),
    (Name: 'slategrey'; Color: TCSSAlphaColor($ff708090)),
    (Name: 'snow'; Color: TCSSAlphaColor($fffffafa)),
    (Name: 'springgreen'; Color: TCSSAlphaColor($ff00ff7f)),
    (Name: 'steelblue'; Color: TCSSAlphaColor($ff4682b4)),
    (Name: 'tan'; Color: TCSSAlphaColor($ffd2b48c)),
    (Name: 'teal'; Color: TCSSAlphaColor($ff008080)),
    (Name: 'thistle'; Color: TCSSAlphaColor($ffd8bfd8)),
    (Name: 'tomato'; Color: TCSSAlphaColor($ffff6347)),
    (Name: 'transparent'; Color: TCSSAlphaColor(0)),
    (Name: 'turquoise'; Color: TCSSAlphaColor($ff40e0d0)),
    (Name: 'violet'; Color: TCSSAlphaColor($ffee82ee)),
    (Name: 'wheat'; Color: TCSSAlphaColor($fff5deb3)),
    (Name: 'white'; Color: TCSSAlphaColor($ffffffff)),
    (Name: 'whitesmoke'; Color: TCSSAlphaColor($fff5f5f5)),
    (Name: 'yellow'; Color: TCSSAlphaColor($ffffff00)),
    (Name: 'yellowgreen'; Color: TCSSAlphaColor($ff9acd32))
  );

type

  { TCSSRegistryNamedItem }

  TCSSRegistryNamedItem = class
  public
    Name: TCSSString; // case sensitive
    Index: TCSSNumericalID;
  end;

type

  { TCSSAttributeKeyData }

  TCSSAttributeKeyData = class(TCSSElementOwnedData)
  public
    Tokens: TBytes; // tokenized value, see TCSSBaseResolver.Tokenize
    Invalid: boolean; // value is invalid, resolver skips it
    Disabled: boolean; // resolver skips it as if commented out, see TCSSResolver.DisableDeclaration
  end;
  TCSSAttributeKeyDataClass = class of TCSSAttributeKeyData;

  { TCSSRuleParserData - TCSSElement.CustomData of TCSSRuleElement and TCSSAtRuleElement }

  TCSSRuleParserData = class(TCSSElementOwnedData)
  end;
  TCSSRuleDataClass = class of TCSSRuleParserData;

  TCSSBaseResolver = class;
  TCSSResolverParser = class;

  { TCSSAttributeDesc - general properties of a CSS attribute }

  TCSSAttributeDesc = class(TCSSRegistryNamedItem)
  public
    type
      TCheckEvent = function(Resolver: TCSSBaseResolver): boolean of object;

      TSplitShorthandPart = record
        AttrIDs: array of TCSSNumericalID;
        StartPos, EndPos: integer; // token positions into Resolver.CurTokens, see CurTokenStart/CurTokenPos
      end;
      PSplitShorthandPart = ^TSplitShorthandPart;
      TSplitShorthandParts = array of TSplitShorthandPart;

      TSplitShorthandEvent = procedure(Resolver: TCSSBaseResolver;
          var AttrIDs: TCSSNumericalIDArray;
          var Values: TBytesArray
          ) of object;
      TParseShorthandEvent = procedure(Resolver: TCSSBaseResolver;
          out Parts: TSplitShorthandParts) of object;
  public
    Inherits: boolean; // true = the default value is the parent's value
    All: boolean; // true = can be changed by the 'all' attribute
    AllowUnknownIdentifiers: boolean; // true = the tokenizer stores unknown words as rtkIdentifier
      // instead of failing, e.g. font-family names, grid line names
    InitialValue: TCSSString;
    CompProps: array of TCSSAttributeDesc; // if this attribute is a shorthand,
      // these are the component properties (longhands + sub-shorthands like border-width)
      // used by the cascade algorithm to delete all overwritten properties
    OnCheck: TCheckEvent; // called by the parser after reading a declaration and there is no var()
      // return false if invalid, so the resolver skips this declaration
    OnSplitShorthand: TSplitShorthandEvent; // called by resolver after resolving var(),
        // if any value is empty, the InitialValue is used
    OnParseShorthand: TParseShorthandEvent; // called by the inspector
  end;
  TCSSAttributeDescClass = class of TCSSAttributeDesc;
  TCSSAttributeDescArray = array of TCSSAttributeDesc;

  TCSSShorthandTokenSpan = record
    StartPos, EndPos: integer; // token positions into Resolver.CurTokens
  end;
  TCSSShorthandTokenSpanArray = array of TCSSShorthandTokenSpan;

  { TCSSPseudoClassDesc }

  TCSSPseudoClassDesc = class(TCSSRegistryNamedItem)
  public
    BuiltIn: boolean;
  end;
  TCSSPseudoClassDescClass = class of TCSSPseudoClassDesc;
  TCSSPseudoClassDescArray = array of TCSSPseudoClassDesc;

  { TCSSPseudoElementDesc }

  TCSSPseudoElementDesc = class(TCSSRegistryNamedItem)
  public
    Attributes: TCSSAttributeDescArray; // allowed attributes
    IsFunction: boolean;
  end;
  TCSSPseudoElementDescClass = class of TCSSPseudoElementDesc;
  TCSSPseudoElementDescArray = array of TCSSPseudoElementDesc;

  { TCSSTypeDesc }

  TCSSTypeDesc = class(TCSSRegistryNamedItem)
  public
  end;
  TCSSTypeDescClass = class of TCSSTypeDesc;
  TCSSTypeDescArray = array of TCSSTypeDesc;

  { TCSSRegistry }

  TCSSRegistry = class
  private
    FAttrFunctionCount: TCSSNumericalID;
    FAttributeCount: TCSSNumericalID;
    FHashLists: array[TCSSNumericalIDKind] of TFPHashList; // name to TCSSRegistryNamedItem
    FKeywordCount: TCSSNumericalID;
    FPseudoClassCount: TCSSNumericalID;
    FPseudoElementCount: TCSSNumericalID;
    FPseudoFunctionCount: TCSSNumericalID;
    FStamp, FModifiedStamp: integer;
    FTypeCount: TCSSNumericalID;
    function GetModified: boolean;
    procedure SetModified(const AValue: boolean);
  public
    constructor Create;
    procedure Init; virtual; // add basic items
    destructor Destroy; override;
    function FindNamedItem(Kind: TCSSNumericalIDKind; const aName: TCSSString): TCSSRegistryNamedItem; overload;
    function IndexOfNamedItem(Kind: TCSSNumericalIDKind; const aName: TCSSString): TCSSNumericalID; overload;
    procedure ConsistencyCheck; virtual;
    procedure ChangeStamp;
    property Stamp: integer read FStamp; // always >0
    property Modified: boolean read GetModified write SetModified;
  public
    // attributes
    Attributes: TCSSAttributeDescArray; // Note: Attributes[0] is nil to spot bugs easily
    Attribute_ClassOf: TCSSAttributeDescClass;
    NotAllAttributes: TCSSAttributeDescArray;
    function AddAttribute(Attr: TCSSAttributeDesc): TCSSAttributeDesc; overload;
    function AddAttribute(const aName: TCSSString; const aInitialValue: TCSSString = '';
      aInherits: boolean = false; aAll: boolean = true; aClass: TCSSAttributeDescClass = nil): TCSSAttributeDesc; overload;
    function FindAttribute(const aName: TCSSString): TCSSAttributeDesc; overload;
    function IndexOfAttributeName(const aName: TCSSString): TCSSNumericalID; overload;
    procedure AddSplitLonghand(var AttrIDs: TCSSNumericalIDArray; var Values: TBytesArray; AttrID: TCSSNumericalID; const Value: TBytes); overload;
    procedure AddSplitLonghandSides(var AttrIDs: TCSSNumericalIDArray; var Values: TBytesArray;
      TopID, RightID, BottomID, LeftID: TCSSNumericalID; const Found: TBytesArray); overload;
    procedure AddSplitLonghandCorners(var AttrIDs: TCSSNumericalIDArray; var Values: TBytesArray;
      TopLeftID, TopRightID, BottomLeftID, BottomRightID: TCSSNumericalID; const Found: TBytesArray); overload;
    procedure AddSplitShorthandPart(var Parts: TCSSAttributeDesc.TSplitShorthandParts;
      const AttrIDs: array of TCSSNumericalID; StartPos, EndPos: integer);
    procedure AddSplitShorthandPartsSides(var Parts: TCSSAttributeDesc.TSplitShorthandParts;
      TopID, RightID, BottomID, LeftID: TCSSNumericalID; const Spans: TCSSShorthandTokenSpanArray);
    property AttributeCount: TCSSNumericalID read FAttributeCount;
  public
    // pseudo classes, e.g. :hover
    PseudoClasses: TCSSPseudoClassDescArray; // Note: PseudoClasses[0] is nil to spot bugs easily
    PseudoClass_ClassOf: TCSSPseudoClassDescClass;
    function AddPseudoClass(aPseudo: TCSSPseudoClassDesc): TCSSPseudoClassDesc; overload;
    function AddPseudoClass(const aName: TCSSString; aClass: TCSSPseudoClassDescClass = nil): TCSSPseudoClassDesc; overload;
    function FindPseudoClass(const aName: TCSSString): TCSSPseudoClassDesc; overload;
    function IndexOfPseudoClassName(const aName: TCSSString): TCSSNumericalID; overload;
    property PseudoClassCount: TCSSNumericalID read FPseudoClassCount;
  public
    // pseudo element, e.g. ::first-line
    PseudoElements: TCSSPseudoElementDescArray;
    PseudoElement_ClassOf: TCSSPseudoElementDescClass;
    function AddPseudoElement(aPseudo: TCSSPseudoElementDesc): TCSSPseudoElementDesc; overload;
    function AddPseudoElement(const aName: TCSSString; aClass: TCSSPseudoElementDescClass = nil): TCSSPseudoElementDesc; overload;
    function FindPseudoElement(const aName: TCSSString): TCSSPseudoElementDesc; overload;
    function IndexOfPseudoElementName(const aName: TCSSString): TCSSNumericalID; overload;
    property PseudoElementCount: TCSSNumericalID read FPseudoElementCount;
  public
    // pseudo functions lowercase (they are parsed case insensitive), e.g. :not()
    PseudoFunctions: TCSSStringArray; // Note: PseudoFunctions[0] is nil to spot bugs easily
    function AddPseudoFunction(const aName: TCSSString): TCSSNumericalID; overload;
    function IndexOfPseudoFunction(const aName: TCSSString): TCSSNumericalID; overload;
    property PseudoFunctionCount: TCSSNumericalID read FPseudoFunctionCount;
  public
    // types, e.g. div
    Types: TCSSTypeDescArray; // Note: Types[0] is nil to spot bugs easily
    Type_ClassOf: TCSSTypeDescClass;
    function AddType(aType: TCSSTypeDesc): TCSSTypeDesc; overload;
    function AddType(const aName: TCSSString; aClass: TCSSTypeDescClass = nil): TCSSTypeDesc; overload;
    function FindType(const aName: TCSSString): TCSSTypeDesc; overload;
    function IndexOfTypeName(const aName: TCSSString): TCSSNumericalID; overload;
    property TypeCount: TCSSNumericalID read FTypeCount;
  public
    // keywords
    Keywords: TCSSStringArray; // Note: Keywords[0] is nil to spot bugs easily
    KeywordTokens: TBytesArray; // tokenized form of each keyword, see TCSSBaseResolver.Tokenize
    kwFirstColor, kwLastColor: TCSSNumericalID; // named colors, excluding transparent, currentcolor and system colors
    kwTransparent: TCSSNumericalID;
    kwCurrentColor: TCSSNumericalID;
    kwFirstSystemColor, kwLastSystemColor: TCSSNumericalID;
    function AddKeyword(const aName: TCSSString): TCSSNumericalID; overload;
    procedure AddKeywords(const Names: TCSSStringArray; out First, Last: TCSSNumericalID); overload;
    function IndexOfKeyword(const aName: TCSSString): TCSSNumericalID; overload;
    function IndexOfValueKeyword(const aName: TCSSString;
      AllowUnknownIdentifiers: boolean): TCSSNumericalID; virtual; // resolve a word of an attribute value
    procedure AddColorKeywords; virtual;
    function IsColorKeyword(KeywordID: TCSSNumericalID; OnlyConst: boolean): boolean;
    function GetNamedColor(const aName: TCSSString): TCSSAlphaColor; virtual; overload;
    function GetKeywordColor(KeywordID: TCSSNumericalID): TCSSAlphaColor; virtual; overload;
    property KeywordCount: TCSSNumericalID read FKeywordCount;
  public
    // attribute functions, e.g. var()
    AttrFunctions: TCSSStringArray; // Note: AttrFunctions[0] is nil to spot bugs easily
    const afVar = CSSAttrFuncVar;
    const afRgb = CSSAttrFuncRGB;
    const afRgba = CSSAttrFuncRGBA;
    function AddAttrFunction(const aName: TCSSString): TCSSNumericalID; overload;
    function IndexOfAttrFunction(const aName: TCSSString): TCSSNumericalID; overload;
    function IsColorFunction(FuncID: TCSSNumericalID): boolean; virtual;
    property AttrFunctionCount: TCSSNumericalID read FAttrFunctionCount;
  end;

  TCSSValueParserLogEvent = procedure(MsgType: TEventType; const ID: TCSSMsgID;
                               const Msg: TCSSString; PosEl: TCSSElement) of object;

  { TCSSResolvedIdentifierElement }

  TCSSResolvedIdentifierElement = class(TCSSIdentifierElement)
  public
    NumericalID: TCSSNumericalID;
    Kind: TCSSNumericalIDKind;
  end;

  { TCSSResolvedPseudoClassElement }

  TCSSResolvedPseudoClassElement = class(TCSSPseudoClassElement)
  public
    NumericalID: TCSSNumericalID;
    Kind: TCSSNumericalIDKind;
  end;

  { TCSSResolvedClassNameElement }

  TCSSResolvedClassNameElement = class(TCSSClassNameElement)
  public
    NumericalID: TCSSNumericalID;
  end;

  { TCSSResolvedHashIdentifierElement - e.g. "x" of "#x" in a selector }

  TCSSResolvedHashIdentifierElement = class(TCSSHashIdentifierElement)
  public
    NumericalID: TCSSNumericalID;
  end;

  { TCSSNthChildParams }

  TCSSNthChildParams = class
  public
    Modulo: integer;
    Start: integer;
    HasOf: boolean; // for nth-of-type() HasOf=true and OfSelector=nil
    OfSelector: TCSSElement;
  end;
  TCSSNthChildParamsClass = class of TCSSNthChildParams;

  { TCSSResolvedCallElement }

  TCSSResolvedCallElement = class(TCSSCallElement)
  public
    NameNumericalID: TCSSNumericalID;
    Kind: TCSSNumericalIDKind;
    Params: TObject; // e.g. TCSSNthChildParams
    destructor Destroy; override;
  end;

  { TCSSValueData }

  TCSSValueData = class(TCSSElementOwnedData)
  public
    NormValue: TCSSString; // normalized value, stripped of comments and e.g. '0.1' instead of '000.100' or '.1'
  end;

  TCSSResTokenKind = (
    rtkNone,
    rtkWhitespace, // any amount of whitespace characters
    rtkSymbol,  // followed by a byte colon, semicolon, dot . , star *, or div /
    rtkComma, // ,
    rtkLParenthesis, // (
    rtkRParenthesis, // )
    rtkLBracket, // [
    rtkRBracket, // ]
    rtkPlus, // plus operator, e.g. '+calc()' or '+ ', a +Number does not generate a rtkPlus
    rtkMinus, // minus operator, e.g. '-calc()' or '- ', a -Number does not generate a rtkMinus
    rtkFloat, // followed by a TCSSUnit as byte and a double, the unit can be cuNone
    rtkKeyword, // followed by the index of the CSSRegistry keyword as word
    rtkFunction, // followed by the index of the CSSRegistry function as word, this token includes the opening parenthesis
    rtkIdentifier, // followed by a dword as count, followed by count bytes for the identifier including the leading --
    rtkStringApos, // followed by a dword as count, followed by count bytes for the string without the enclosing apostrophs
    rtkStringQuote, // followed by a dword as count, followed by count bytes for the string without the enclosing quotes
    rtkHexColor // followed by a byte as count, followed by count hex characters
    );
  TCSSResTokenKinds = set of TCSSResTokenKind;

const
  CSSResTokenToCSS: array[TCSSResTokenKind] of TCSSToken = (
    ctkUNKNOWN,
    ctkWhitespace,
    ctkUNKNOWN,
    ctkComma,
    ctkLParenthesis,
    ctkRParenthesis,
    ctkLBracket,
    ctkRBracket,
    ctkPlus,
    ctkMinus,
    ctkUNKNOWN, // rtkFloat
    ctkUNKNOWN, // rtkKeyword
    ctkUNKNOWN, // rtkFunction
    ctkUNKNOWN, // rtkIdentifier
    ctkUNKNOWN, // rtkStringApos
    ctkUNKNOWN, // rtkStringQuote
    ctkUNKNOWN  // rtkHexColor
    );
type

  TCSSResValueKind = (
    rvkNone, // end of value
    rvkInvalid,
    rvkSymbol,
    rvkFloat,
    rvkKeyword,
    rvkKeywordUnknown,
    rvkFunction,
    rvkFunctionUnknown,
    rvkString,
    rvkBrackets, // []
    rvkParenthesis, // ()
    rvkHexColor
    );

  { TCSSResCompValue }

  TCSSResCompValue = record
    Kind: TCSSResValueKind;
    StartP, EndP: PCSSChar;
    function AsString: TCSSString;
    function FloatAsString: TCSSString;
    function IsInteger: boolean;
    function IsIntegerValue(v: Integer): boolean;
    case longint of
    1: (Float: Double; FloatUnit: TCSSUnit);
    2: (KeywordID: TCSSNumericalID);
    3: (FunctionID: TCSSNumericalID; BracketOpen: PCSSChar);
    4: (Symbol: TCSSToken);
  end;

  { TCSSCheckAttrParams_Dimension }

  TCSSCheckAttrParams_Dimension = record
  public
    AllowedUnits: TCSSUnits;
    AllowNegative, AllowFrac: boolean;
    AllowedKeywordIDs: TCSSNumericalIDArray;
    function Fits(const ResValue: TCSSResCompValue): boolean; overload;
  end;

  TCSSHasMediaBoolEvent = function(aResolver: TCSSBaseResolver; KW: TCSSNumericalID): boolean of object;
  TCSSIsMediaPlainEvent = function(aResolver: TCSSBaseResolver; KW: TCSSNumericalID;
    const aValue: TCSSResCompValue): boolean of object;
  TCSSMediaCompareEvent = function(aResolver: TCSSBaseResolver; KW: TCSSNumericalID;
    const aValue: TCSSResCompValue;
    out Cmp: integer // 0=equal, 1=KW is bigger, -1 aValue is bigger
    ): boolean of object; // false = comparing apples with oranges


  { TCSSBaseResolver }

  TCSSBaseResolver = class(TComponent)
  private
    FCSSRegistry: TCSSRegistry;
    FHasMediaBoolean: TCSSHasMediaBoolEvent;
    FIsMediaPlain: TCSSIsMediaPlainEvent;
    FMediaCompare: TCSSMediaCompareEvent;
    FCurValue: TCSSString;
    FIdentifier: TCSSString;
    FIdentifierValid: boolean;
    function GetCurValue: TCSSString;
    function GetIdentifier: TCSSString;
  protected
    procedure SetCSSRegistry(const AValue: TCSSRegistry); virtual;
    procedure SetHasMediaBoolean(const AValue: TCSSHasMediaBoolEvent); virtual;
    procedure SetIsMediaPlain(const AValue: TCSSIsMediaPlainEvent); virtual;
    procedure SetMediaCompare(const AValue: TCSSMediaCompareEvent); virtual;
    procedure MediaEnvironmentChanged; virtual;
  public
    CurAttrData: TCSSAttributeKeyData;
    CurDesc: TCSSAttributeDesc;

    CurTokens: TBytes; // tokenized value being read, see Tokenize
    CurTokenPos: integer; // offset in CurTokens of the next token (read by ReadNext)
    CurTokenStart: integer; // offset in CurTokens where the current token started
    // current token component, set by ReadNext:
    TokenKind: TCSSResTokenKind;
    Float: Double;
    FloatUnit: TCSSUnit;
    KeywordID: TCSSNumericalID;
    FunctionID: TCSSNumericalID;
    Symbol: TCSSToken;
    Empty: boolean; // no tokens read so far or only whitespace
    procedure ResetCurComp; // clear the current token component
    function InitParseAttr(Desc: TCSSAttributeDesc; AttrData: TCSSAttributeKeyData): boolean; virtual; // true if parsing can start
    function InitParseAttr(Desc: TCSSAttributeDesc; const Value: TCSSString): boolean; virtual; // true if parsing can start
    function InitParseAttr(Desc: TCSSAttributeDesc; const Tokens: TBytes): boolean; virtual; // true if parsing can start, reuses already tokenized value
    procedure InitParseAttr(const Value: TCSSString); virtual;
    property CurValue: TCSSString read GetCurValue; // source text (for diagnostics)
    // text payload of rtkIdentifier/rtkString*/rtkHexColor and symbols of the
    // current token, decoded from CurTokens on demand, see ReadNext
    property Identifier: TCSSString read GetIdentifier;
    function ReadNext: boolean;
    function PeekNextTokenKind: TCSSResTokenKind; // kind of the next token without consuming, skipping whitespace; rtkNone at end
    function AtEnd: boolean; // true if there is no current token, i.e. the last ReadNext reached the end of CurTokens
    class procedure SkipToEndOfAttribute(var p: PCSSChar);
    class function SkipString(var p: PCSSChar): boolean;
    class function SkipBrackets(var p: PCSSChar; Lvl: integer = 1): boolean;
    // check whole attribute:
    function CheckAttribute_Keyword(const Tokens: TBytes): TCSSNumericalID;
    function CheckAttribute_Keyword(const AllowedKeywordIDs: TCSSNumericalIDArray): boolean;
    function CheckAttribute_Keyword_List(const AllowedKeywordIDs: TCSSNumericalIDArray): boolean;
    function CheckAttribute_Dimension(const Params: TCSSCheckAttrParams_Dimension): boolean;
    function CheckAttribute_Color(const AllowedKeywordIDs: TCSSNumericalIDArray = nil): boolean; // check whole value is exactly one color
    // check current component:
    function IsKeywordIn(aKeywordID: TCSSNumericalID; const KeywordIDs: TCSSNumericalIDArray): boolean; overload;
    function IsKeywordIn(const KeywordIDs: TCSSNumericalIDArray): boolean; overload;
    function IsLengthOrPercentage(AllowNegative: boolean): boolean; overload;
    function IsSymbol(Token: TCSSToken): boolean; overload;
    function IsFloat(const Params: TCSSCheckAttrParams_Dimension): boolean; // true if the current float component fits
    function IsDimension(const Params: TCSSCheckAttrParams_Dimension): boolean; // true if the current float or keyword fits
    function IsColor: boolean; // true if the current component is a color
    function ReadColorFunction(out aColor: TCSSAlphaColor): boolean;
    function ReadColor(out StartPos, EndPos: integer): boolean; overload;
    function ReadColor: boolean; overload;
    function GetCompString: TCSSString; overload;
    function GetCompTokens: TBytes; // the current CurTokenStart til CurTokenPos
    function FloatAsString: TCSSString; // the current component as float+unit
    function IsInteger: boolean; // the current component is a unitless number
    function IsIntegerValue(v: Integer): boolean;
    // low level functions to read attribute tokens
    function IsBaseKeyword(aKeywordID: TCSSNumericalID): boolean;
    function Tokenize(const aValue: string; out aData: TBytes; AllowUnknownIdentifiers: boolean = false;
      TrimEnclosingSpace: boolean = true): boolean; // false if invalid (e.g. only whitespace), see TCSSResTokenKind
    function TokenizeKeyword(KW: TCSSNumericalID): TBytes;
    function TokenizeIdentifier(const anIdentifier: TCSSString): TBytes;
    function TokenizeFloat(const aFloat: double; anUnit: TCSSUnit): TBytes;
    function TokenizeHexColor(const aHexDigits: TCSSString): TBytes;
    function ResolveIdentifierTokens(var Tokens: TBytes): boolean; // convert rtkIdentifier to rtkKeyword, false if invalid
    function Detokenize(const aData: TBytes): TCSSString; // convert a token array back to a css value
    function DetokenizeOne(aData: PByte): TCSSString; // convert one token back to a css value
    // registry
    function GetAttributeID(const aName: TCSSString; AutoCreate: boolean = false): TCSSNumericalID; virtual;
    function GetAttributeDesc(AttrID: TCSSNumericalID): TCSSAttributeDesc; virtual;
    function GetTypeID(const aName: TCSSString): TCSSNumericalID; virtual;
    function GetPseudoClassID(const aName: TCSSString): TCSSNumericalID; virtual;
    function GetCSSClassID(const aCSSClassName: TCSSString): TCSSNumericalID; virtual; // lookup only, CSSIDNone if unknown
    function AddCSSClassID(const aCSSClassName: TCSSString): TCSSNumericalID; virtual; // get or create, used while parsing selectors
    function GetCSSIDIndex(const aCSSID: TCSSString): TCSSNumericalID; virtual; // lookup only, CSSIDNone if unknown
    function AddCSSID(const aCSSID: TCSSString): TCSSNumericalID; virtual; // get or create, used while parsing selectors
    function GetPseudoElementID(const aName: TCSSString): TCSSNumericalID; virtual;
    function GetPseudoElFuncID(const aName: TCSSString): TCSSNumericalID; virtual;
    function GetPseudoFunctionID(const aName: TCSSString): TCSSNumericalID; virtual;

    property CSSRegistry: TCSSRegistry read FCSSRegistry write SetCSSRegistry;
    // @media
    property HasMediaBoolean: TCSSHasMediaBoolEvent read FHasMediaBoolean write SetHasMediaBoolean;
    property IsMediaPlain: TCSSIsMediaPlainEvent read FIsMediaPlain write SetIsMediaPlain;
    property MediaCompare: TCSSMediaCompareEvent read FMediaCompare write SetMediaCompare;
  end;

  { TCSSResolverParser
    - resolves identifiers to IDs
    - warns about constructs unsupported by resolver }

  TCSSResolverParser = class(TCSSParser)
  private
    FOnLog: TCSSValueParserLogEvent;
    FResolver: TCSSBaseResolver;
  protected
    function ResolveAttribute(El: TCSSResolvedIdentifierElement): TCSSNumericalID; virtual;
    function ResolveType(El: TCSSResolvedIdentifierElement): TCSSNumericalID; virtual;
    function ResolvePseudoClass(El: TCSSResolvedPseudoClassElement): TCSSNumericalID; virtual;
    function ResolveClassName(El: TCSSResolvedClassNameElement): TCSSNumericalID; virtual;
    function ResolveHashIdentifier(El: TCSSResolvedHashIdentifierElement): TCSSNumericalID; virtual;
    function ResolvePseudoElement(El: TCSSResolvedIdentifierElement): TCSSNumericalID; virtual;
    function ResolvePseudoElementFunction(El: TCSSResolvedCallElement): TCSSNumericalID; virtual;
    function ResolvePseudoFunction(El: TCSSResolvedCallElement): TCSSNumericalID; virtual;
    function ResolveMediaIdentifier(El: TCSSResolvedIdentifierElement): TCSSNumericalID; virtual;
    procedure CheckMediaSelector(El: TCSSElement); virtual;
    function CreateElement(aClass: TCSSElementClass): TCSSElement; override;
    function ParseCall(aName: TCSSString; IsSelector: boolean): TCSSCallElement; override;
    function ParseDeclaration(aIsAt: Boolean): TCSSDeclarationElement; override;
    function ParsePseudoElement: TCSSElement; override;
    function ParseSelector: TCSSElement; override;
    function ParseAtMediaRulePrelude: TCSSAtRuleElement; override;
    procedure CheckSelector(El: TCSSElement); virtual;
    procedure CheckSelectorArray(anArray: TCSSArrayElement); virtual;
    procedure CheckSelectorArrayBinary(aBinary: TCSSBinaryElement); virtual;
    procedure CheckSelectorUnary(aUnary: TCSSUnaryElement); virtual;
    procedure CheckSelectorBinary(aBinary: TCSSBinaryElement); virtual;
    procedure CheckSelectorList(aList: TCSSListElement); virtual;
    procedure CheckNthChildParams(aCall: TCSSResolvedCallElement); virtual;
    function ComputeValue(El: TCSSElement): TCSSString; virtual;
  public
    CSSNthChildParamsClass: TCSSNthChildParamsClass;
    CSSAttributeKeyDataClass: TCSSAttributeKeyDataClass;
    CSSRuleDataClass: TCSSRuleDataClass;
    constructor Create(AScanner: TCSSScanner); override; overload;
    destructor Destroy; override;
    procedure Log(MsgType: TEventType; const ID: TCSSMsgID; const Msg: TCSSString; PosEl: TCSSElement); virtual;
    class function IsWhiteSpace(const s: TCSSString): boolean; virtual; overload;
    property Resolver: TCSSBaseResolver read FResolver write FResolver;
    property OnLog: TCSSValueParserLogEvent read FOnLog write FOnLog;
  end;

  { TCSSResCompParser - utility class to parse a property directly - non tokenized }

  TCSSResCompParser = class
  private
    FCSSRegistry: TCSSRegistry;
    procedure SetCSSRegistry(const AValue: TCSSRegistry);
  public
    CurValue: TCSSString;
    CurComp: TCSSResCompValue;
    procedure InitParseAttr(const Value: TCSSString); virtual;
    // check whole attribute, skipping invalid values, emit warnings:
    function CheckAttribute_Keyword(const AllowedKeywordIDs: TCSSNumericalIDArray): boolean; virtual;
    function CheckAttribute_Keyword_List(const AllowedKeywordIDs: TCSSNumericalIDArray): boolean; virtual;
    function CheckAttribute_Dimension(const Params: TCSSCheckAttrParams_Dimension): boolean; virtual;
    function CheckAttribute_Color(const AllowedKeywordIDs: TCSSNumericalIDArray): boolean; virtual;
    // parse whole attribute:
    function ReadNext: boolean;
    function IsBaseKeyword(KeywordID: TCSSNumericalID): boolean;
    function IsKeywordIn(aKeywordID: TCSSNumericalID; const KeywordIDs: TCSSNumericalIDArray): boolean; overload;
    function IsKeywordIn(const KeywordIDs: TCSSNumericalIDArray): boolean; overload;
    function IsLengthOrPercentage(AllowNegative: boolean): boolean; overload;
    function IsLengthOrPercentage(const ResValue: TCSSResCompValue; AllowNegative: boolean): boolean; overload;
    function IsSymbol(Token: TCSSToken): boolean; overload;
    function GetCompString: TCSSString; overload;
    function GetCompString(const aValue: string; const ResValue: TCSSResCompValue): TCSSString; overload;
    // low level functions to parse attribute components
    function ReadComp(var aComp: TCSSResCompValue): boolean; // true if parsing attribute can continue
    procedure ReadWordID(var aComp: TCSSResCompValue);
    class function ReadValue(var aComp: TCSSResCompValue): boolean; // true if parsing attribute can continue, not using CSSRegistry
    class function ReadNumber(var aComp: TCSSResCompValue): boolean;
    class function ReadIdentifier(var aComp: TCSSResCompValue): boolean;
    class procedure SkipToEndOfAttribute(var p: PCSSChar);
    class function SkipString(var p: PCSSChar): boolean;
    class function SkipBrackets(var p: PCSSChar; Lvl: integer = 1): boolean;

    property CSSRegistry: TCSSRegistry read FCSSRegistry write SetCSSRegistry;
  end;

function HasCSSValueVarCall(const s: TCSSString): boolean; // true if a case insensitive "var(" is in s

function CSSReadTokenWord(const Tokens: TBytes; Ofs: integer): word; // read a word payload (e.g. keyword/function id) at Ofs
function CSSTokenByteLen(const Tokens: TBytes; Ofs: integer): integer; // byte size of the token starting at Ofs, see TCSSResTokenKind
function CSSHasVarToken(const Tokens: TBytes): boolean; // true if the tokens contain a var() function call
function CSSSameTokens(const A, B: TBytes): boolean; // true if both token arrays are byte-identical
function CSSTokensEmpty(const Tokens: TBytes): boolean; // true if there is no token except whitespace and the end marker

implementation

Const
  Alpha = ['A'..'Z','a'..'z'];
  Num   = ['0'..'9'];
  AlNum = Alpha+Num;
  AlIden = Alpha+['-'];
  AlNumIden = AlNum+['-'];
  Whitespace = [#9,#10,#13,' '];
  //WhitespaceZ = Whitespace+[#0];
  Hex = ['0'..'9','a'..'z','A'..'Z'];
  ValEnd = [#0,#10,#13,#9,' ',';',',','}',')',']']; // used for skipping a component value

function HasCSSValueVarCall(const s: TCSSString): boolean;
var
  i, l: SizeInt;
  Quote: TCSSChar;
begin
  l:=length(s);
  i:=1;
  while i<=l do
    begin
    if s[i] in ['"',''''] then
      begin
      // skip string literal
      Quote:=s[i];
      inc(i);
      while i<=l do
        begin
        if s[i]='\' then
          inc(i,2) // skip escaped character
        else if s[i]=Quote then
          begin
          inc(i);
          break;
          end
        else
          inc(i);
        end;
      end
    else if (i<=l-3)
        and (s[i] in ['v','V'])
        and (s[i+1] in ['a','A'])
        and (s[i+2] in ['r','R'])
        and (s[i+3]='(') then
      exit(true)
    else
      inc(i);
    end;
  Result:=false;
end;

function CSSReadTokenWord(const Tokens: TBytes; Ofs: integer): word;
begin
  Result:=PWord(@Tokens[Ofs])^;
end;

function CSSTokenByteLen(const Tokens: TBytes; Ofs: integer): integer;
var
  Cnt: cardinal;
begin
  if (Ofs<0) or (Ofs>=length(Tokens)) then exit(0);
  case TCSSResTokenKind(Tokens[Ofs]) of
  rtkFloat: Result:=1+1+SizeOf(double); // kind + unit byte + double
  rtkKeyword,rtkFunction: Result:=1+2; // kind + word
  rtkIdentifier,rtkStringApos,rtkStringQuote:
    begin
      // kind + dword length + payload
      Cnt:=PDWord(@Tokens[Ofs+1])^;
      Result:=1+4+integer(Cnt)*SizeOf(TCSSChar);
    end;
  rtkHexColor: Result:=1+1+Tokens[Ofs+1]*SizeOf(TCSSChar); // kind + length byte + hex chars
  rtkSymbol: Result:=1+1; // kind + char byte
  else
    Result:=1; // rtkWhitespace, comma, brackets, plus, minus
  end;
end;

function CSSHasVarToken(const Tokens: TBytes): boolean;
var
  Ofs, Len, Step: integer;
  FuncID: word;
begin
  Len:=length(Tokens);
  Ofs:=0;
  while Ofs<Len do
  begin
    if TCSSResTokenKind(Tokens[Ofs])=rtkFunction then
    begin
      FuncID:=PWord(@Tokens[Ofs+1])^;
      if FuncID=CSSAttrFuncVar then exit(true);
    end;
    Step:=CSSTokenByteLen(Tokens,Ofs);
    if Step<=0 then break;
    inc(Ofs,Step);
  end;
  Result:=false;
end;

function CSSSameTokens(const A, B: TBytes): boolean;
var
  l: integer;
begin
  l:=length(A);
  Result:=(l=length(B)) and ((l=0) or (CompareByte(A[0],B[0],l)=0));
end;

function CSSTokensEmpty(const Tokens: TBytes): boolean;
var
  Ofs, Len: integer;
begin
  Len:=length(Tokens);
  Ofs:=0;
  while Ofs<Len do
  begin
    case TCSSResTokenKind(Tokens[Ofs]) of
    rtkWhitespace: inc(Ofs);
    else exit(false);
    end;
  end;
  Result:=true;
end;

{ TCSSRegistry }

procedure TCSSRegistry.SetModified(const AValue: boolean);
begin
  if AValue then
    ChangeStamp
  else
    FModifiedStamp:=FStamp;
end;

function TCSSRegistry.GetModified: boolean;
begin
  Result:=FStamp=FModifiedStamp;
end;

constructor TCSSRegistry.Create;
var
  Kind: TCSSNumericalIDKind;
begin
  for Kind in TCSSNumericalIDKind do
    FHashLists[Kind]:=TFPHashList.Create;
  if Attribute_ClassOf=nil then
    Attribute_ClassOf:=TCSSAttributeDesc;
  if PseudoClass_ClassOf=nil then
    PseudoClass_ClassOf:=TCSSPseudoClassDesc;
  if PseudoElement_ClassOf=nil then
    PseudoElement_ClassOf:=TCSSPseudoElementDesc;
  if Type_ClassOf=nil then
    Type_ClassOf:=TCSSTypeDesc;

  // init attributes
  SetLength(Attributes,32);
  FAttributeCount:=1; // index 0 is CSSIDNone

  // init pseudo classes
  SetLength(PseudoClasses,32);
  FPseudoClassCount:=1; // index 0 is CSSIDNone

  // init pseudo elements
  SetLength(PseudoElements,32);
  FPseudoElementCount:=1; // index 0 is CSSIDNone

  // init pseudo functions
  SetLength(PseudoFunctions,16);
  FPseudoFunctionCount:=1; // index 0 is CSSIDNone

  // init types
  SetLength(Types,32);
  FTypeCount:=1; // index 0 is CSSIDNone

  // init keywords
  SetLength(Keywords,32);
  FKeywordCount:=1; // index 0 is CSSIDNone

  // init keywords
  SetLength(AttrFunctions,32);
  FAttrFunctionCount:=1; // index 0 is CSSIDNone
end;

procedure TCSSRegistry.Init;

  procedure AddBuiltInPseudoClass(const aName: TCSSString; ExpectedID: TCSSNumericalID);
  var
    PC: TCSSPseudoClassDesc;
  begin
    PC:=AddPseudoClass(aName);
    if PC.Index<>ExpectedID then
      raise ECSSParser.Create('20240623165848');
    PC.BuiltIn:=true;
  end;

begin
  // init attributes
  if AddAttribute('id').Index<>CSSAttributeID_ID then
    raise ECSSParser.Create('20240617191749');
  if AddAttribute('class').Index<>CSSAttributeID_Class then
    raise ECSSParser.Create('20240617191801');
  if AddAttribute('all').Index<>CSSAttributeID_All then
    raise ECSSParser.Create('20240617191816');

  // init pseudo classes
  AddBuiltInPseudoClass('root',CSSPseudoID_Root);
  AddBuiltInPseudoClass('empty',CSSPseudoID_Empty);
  AddBuiltInPseudoClass('first-child',CSSPseudoID_FirstChild);
  AddBuiltInPseudoClass('last-child',CSSPseudoID_LastChild);
  AddBuiltInPseudoClass('only-child',CSSPseudoID_OnlyChild);
  AddBuiltInPseudoClass('first-of-type',CSSPseudoID_FirstOfType);
  AddBuiltInPseudoClass('last-of-type',CSSPseudoID_LastOfType);
  AddBuiltInPseudoClass('only-of-type',CSSPseudoID_OnlyOfType);

  // init pseudo elements
  // none by default

  // init pseudo functions
  if AddPseudoFunction('not')<>CSSCallID_Not then
    raise ECSSParser.Create('20240625183757');
  if AddPseudoFunction('is')<>CSSCallID_Is then
    raise ECSSParser.Create('20240625142038');
  if AddPseudoFunction('where')<>CSSCallID_Where then
    raise ECSSParser.Create('20240625142049');
  if AddPseudoFunction('has')<>CSSCallID_Has then
    raise ECSSParser.Create('20240625142104');
  if AddPseudoFunction('nth-child')<>CSSCallID_NthChild then
    raise ECSSParser.Create('20240625142124');
  if AddPseudoFunction('nth-last-child')<>CSSCallID_NthLastChild then
    raise ECSSParser.Create('20240625142136');
  if AddPseudoFunction('nth-of-type')<>CSSCallID_NthOfType then
    raise ECSSParser.Create('20240625142156');
  if AddPseudoFunction('nth-last-of-type')<>CSSCallID_NthLastOfType then
    raise ECSSParser.Create('20240625142212');

  // init types
  if AddType('*').Index<>CSSTypeID_Universal then
    raise ECSSParser.Create('20240617190914');

  // init keywords
  if AddKeyword('none')<>CSSKeywordNone then
    raise ECSSParser.Create('20240623184021');
  if AddKeyword('initial')<>CSSKeywordInitial then
    raise ECSSParser.Create('20240623184030');
  if AddKeyword('inherit')<>CSSKeywordInherit then
    raise ECSSParser.Create('20240623184042');
  if AddKeyword('unset')<>CSSKeywordUnset then
    raise ECSSParser.Create('20240623184053');
  if AddKeyword('revert')<>CSSKeywordRevert then
    raise ECSSParser.Create('20240623184104');
  if AddKeyword('revert-layer')<>CSSKeywordRevertLayer then
    raise ECSSParser.Create('20240623184114');
  if AddKeyword('auto')<>CSSKeywordAuto then
    raise ECSSParser.Create('20240625182731');
  if AddKeyword('and')<>CSSKeywordAnd then
    raise ECSSParser.Create('20260327000001');
  if AddKeyword('or')<>CSSKeywordOr then
    raise ECSSParser.Create('20260327000002');
  if AddKeyword('not')<>CSSKeywordNot then
    raise ECSSParser.Create('20260327000003');

  kwFirstSystemColor:=0;
  kwLastSystemColor:=-1;

  // init attribute functions
  if AddAttrFunction('var')<>CSSAttrFuncVar then
    raise ECSSParser.Create('20240716124054');
  if AddAttrFunction('rgb')<>CSSAttrFuncRGB then
    raise ECSSParser.Create('20260906120100');
  if AddAttrFunction('rgba')<>CSSAttrFuncRGBA then
    raise ECSSParser.Create('20260906120101');
end;

destructor TCSSRegistry.Destroy;
var
  i: Integer;
  Kind: TCSSNumericalIDKind;
begin
  for Kind in TCSSNumericalIDKind do
    FreeAndNil(FHashLists[Kind]);

  // attributes
  NotAllAttributes:=nil;
  for i:=1 to AttributeCount-1 do
    FreeAndNil(Attributes[i]);
  Attributes:=nil;
  FAttributeCount:=0;

  // pseudo classes
  for i:=1 to PseudoClassCount-1 do
    FreeAndNil(PseudoClasses[i]);
  PseudoClasses:=nil;
  FPseudoClassCount:=0;

  // pseudo elements
  for i:=1 to PseudoElementCount-1 do
    FreeAndNil(PseudoElements[i]);
  PseudoElements:=nil;
  FPseudoElementCount:=0;

  // types
  for i:=1 to TypeCount-1 do
    FreeAndNil(Types[i]);
  Types:=nil;
  FTypeCount:=0;

  // keywords
  FKeywordCount:=0;

  inherited Destroy;
end;

function TCSSRegistry.FindNamedItem(Kind: TCSSNumericalIDKind;
  const aName: TCSSString): TCSSRegistryNamedItem;
begin
  if Kind in nikAllDescriptors then
    Result:=TCSSRegistryNamedItem(FHashLists[Kind].Find(aName))
  else
    raise ECSSParser.Create('20240625141820');
end;

function TCSSRegistry.IndexOfNamedItem(Kind: TCSSNumericalIDKind;
  const aName: TCSSString): TCSSNumericalID;
var
  Item: TCSSRegistryNamedItem;
  p: Pointer;
begin
  if Kind in nikAllDescriptors then
  begin
    Item:=TCSSRegistryNamedItem(FHashLists[Kind].Find(aName));
    if Item<>nil then
      Result:=Item.Index
    else
      Result:=-1;
  end else begin
    p:=FHashLists[Kind].Find(aName);
    if p=nil then
      exit(CSSIDNone)
    else
      Result:={%H-}TCSSNumericalID(p);
  end;
end;

procedure TCSSRegistry.ConsistencyCheck;
var
  ID, ID2: TCSSNumericalID;
  AttrDesc, SubAttrDesc: TCSSAttributeDesc;
  PseudoClassDesc: TCSSPseudoClassDesc;
  TypeDesc: TCSSTypeDesc;
  i: Integer;
  aName: TCSSString;
  PseudoElementDesc: TCSSPseudoElementDesc;
begin
  if AttributeCount>length(Attributes) then
    raise ECSSParser.Create('20240629102438');
  for ID:=1 to AttributeCount-1 do
  begin
    AttrDesc:=Attributes[ID];
    if AttrDesc=nil then
      raise ECSSParser.Create('20240629102530 attr ID='+IntToStr(ID)+' Desc=nil');
    aName:=AttrDesc.Name;
    if aName='' then
      raise ECSSParser.Create('20240629100056 attr ID='+IntToStr(ID)+' missing name');
    if length(aName)>255 then
      raise ECSSParser.Create('20240629100143 attr ID='+IntToStr(ID)+' name too long "'+aName+'"');
    if aName[1]=':' then
      raise ECSSParser.Create('20240701231211 attr ID='+IntToStr(ID)+' invalid name "'+aName+'"');
    if AttrDesc.Index<>ID then
      raise ECSSParser.Create('20240629095849 attr ID='+IntToStr(ID)+' Desc.Index='+IntToStr(AttrDesc.Index)+' "'+aName+'"');
    ID2:=IndexOfAttributeName(aName);
    if ID2<>ID then
      raise ECSSParser.Create('20240629101227 attr ID='+IntToStr(ID)+' "'+aName+'" IndexOf failed: '+IntToStr(ID2));

    if length(AttrDesc.CompProps)>0 then
    begin
      // check shorthand
      for i:=0 to length(AttrDesc.CompProps)-1 do
      begin
        SubAttrDesc:=AttrDesc.CompProps[i];
        if SubAttrDesc=nil then
          raise ECSSParser.Create('20240629102701 attr ID='+IntToStr(ID)+' Shorthand="'+aName+'" CompDesc=nil '+IntToStr(i));
        if (SubAttrDesc.Index<=0) then
          raise ECSSParser.Create('20240629100345 attr ID='+IntToStr(ID)+' Shorthand="'+aName+'" invalid CompAttr '+IntToStr(SubAttrDesc.Index));
        if (SubAttrDesc.Index>=ID) then
          raise ECSSParser.Create('20240629100345 attr ID='+IntToStr(ID)+' Shorthand="'+aName+'" CompAttr after Shorthand: SubID='+IntToStr(SubAttrDesc.Index)+' SubName='+SubAttrDesc.Name);
      end;
    end;
  end;

  if PseudoClassCount>length(PseudoClasses) then
    raise ECSSParser.Create('20240629102438');
  for ID:=1 to PseudoClassCount-1 do
  begin
    PseudoClassDesc:=PseudoClasses[ID];
    if PseudoClassDesc=nil then
      raise ECSSParser.Create('20240629102605 pseudo class ID='+IntToStr(ID)+' Desc=nil');
    aName:=PseudoClassDesc.Name;
    if aName='' then
      raise ECSSParser.Create('20240629100652 pseudo class ID='+IntToStr(ID)+' missing name');
    if length(aName)>255 then
      raise ECSSParser.Create('20240629100657 pseudo class ID='+IntToStr(ID)+' name too long "'+aName+'"');
    if aName[1]=':' then
      raise ECSSParser.Create('20240701231235 pseudo class ID='+IntToStr(ID)+' invalid name "'+aName+'"');
    if PseudoClassDesc.Index<>ID then
      raise ECSSParser.Create('20240629100659 pseudo class ID='+IntToStr(ID)+' Desc.Index='+IntToStr(PseudoClassDesc.Index)+' "'+aName+'"');
    ID2:=IndexOfPseudoClassName(PseudoClassDesc.Name);
    if ID2<>ID then
      raise ECSSParser.Create('20240629101227 pseudo class ID='+IntToStr(ID)+' "'+aName+'" IndexOf failed: '+IntToStr(ID2));
  end;

  if PseudoElementCount>length(PseudoElements) then
    raise ECSSParser.Create('20250220140108');
  for ID:=1 to PseudoElementCount-1 do
  begin
    PseudoElementDesc:=PseudoElements[ID];
    if PseudoElementDesc=nil then
      raise ECSSParser.Create('20250220140126 pseudo element ID='+IntToStr(ID)+' Desc=nil');
    aName:=PseudoElementDesc.Name;
    if aName='' then
      raise ECSSParser.Create('20250220140201 pseudo element ID='+IntToStr(ID)+' missing name');
    if length(aName)>255 then
      raise ECSSParser.Create('20250220140202 pseudo element ID='+IntToStr(ID)+' name too long "'+aName+'"');
    if aName[1]=':' then
      raise ECSSParser.Create('20250220140204 pseudo element ID='+IntToStr(ID)+' invalid name "'+aName+'"');
    if PseudoElementDesc.Index<>ID then
      raise ECSSParser.Create('20250220140205 pseudo element ID='+IntToStr(ID)+' Desc.Index='+IntToStr(PseudoElementDesc.Index)+' "'+aName+'"');
    ID2:=IndexOfPseudoElementName(PseudoElementDesc.Name);
    if ID2<>ID then
      raise ECSSParser.Create('20250220140207 pseudo element ID='+IntToStr(ID)+' "'+aName+'" IndexOf failed: '+IntToStr(ID2));
  end;

  if PseudoFunctionCount>length(PseudoFunctions) then
    raise ECSSParser.Create('20240629103430');
  for ID:=1 to PseudoFunctionCount-1 do
  begin
    aName:=PseudoFunctions[ID];
    if aName='' then
      raise ECSSParser.Create('20240629103431 pseudo function ID='+IntToStr(ID)+' missing name');
    if length(aName)>255 then
      raise ECSSParser.Create('20240629103433 pseudo function ID='+IntToStr(ID)+' name too long "'+aName+'"');
    if aName[1]=':' then
      raise ECSSParser.Create('20240701231235 pseudo function ID='+IntToStr(ID)+' invalid name "'+aName+'"');
    ID2:=IndexOfPseudoFunction(aName);
    if ID2<>ID then
      raise ECSSParser.Create('20240629103434 pseudo function ID='+IntToStr(ID)+' "'+aName+'" IndexOf failed: '+IntToStr(ID2));
  end;

  if TypeCount>length(Types) then
    raise ECSSParser.Create('20240629102438');
  for ID:=1 to TypeCount-1 do
  begin
    TypeDesc:=Types[ID];
    if TypeDesc=nil then
      raise ECSSParser.Create('20240629102620 type ID='+IntToStr(ID)+' Desc=nil');
    aName:=TypeDesc.Name;
    if aName='' then
      raise ECSSParser.Create('20240629100812 type ID='+IntToStr(ID)+' missing name');
    if length(aName)>255 then
      raise ECSSParser.Create('20240629100825 type ID='+IntToStr(ID)+' name too long "'+aName+'"');
    if aName[1]=':' then
      raise ECSSParser.Create('20240701231645 type ID='+IntToStr(ID)+' invalid name "'+aName+'"');
    if TypeDesc.Index<>ID then
      raise ECSSParser.Create('20240629101013 type ID='+IntToStr(ID)+' Desc.Index='+IntToStr(TypeDesc.Index)+' "'+aName+'"');
    ID2:=IndexOfTypeName(aName);
    if ID2<>ID then
      raise ECSSParser.Create('20240629101529 type ID='+IntToStr(ID)+' "'+aName+'" IndexOf failed: '+IntToStr(ID2));
  end;

  if KeywordCount>length(Keywords) then
    raise ECSSParser.Create('20240629103200');
  for ID:=1 to KeywordCount-1 do
  begin
    aName:=Keywords[ID];
    if aName='' then
      raise ECSSParser.Create('20240629103223 keyword ID='+IntToStr(ID)+' missing name');
    if length(aName)>255 then
      raise ECSSParser.Create('20240629103242 keyword ID='+IntToStr(ID)+' name too long "'+aName+'"');
    if aName[1]=':' then
      raise ECSSParser.Create('20240701231656 keyword ID='+IntToStr(ID)+' invalid name "'+aName+'"');
    ID2:=IndexOfKeyword(aName);
    if ID2<>ID then
      raise ECSSParser.Create('20240629103303 keyword ID='+IntToStr(ID)+' "'+aName+'" IndexOf failed: '+IntToStr(ID2));
  end;
end;

procedure TCSSRegistry.ChangeStamp;
begin
  if FStamp<high(FStamp) then
    inc(FStamp)
  else
    FStamp:=1;
end;

function TCSSRegistry.AddAttribute(Attr: TCSSAttributeDesc
  ): TCSSAttributeDesc;
begin
  Result:=Attr;
  if Attr.Name='' then
    raise ECSSParser.Create('missing name');
  if FindAttribute(Attr.Name)<>nil then
    raise ECSSParser.Create('duplicate attribute "'+Attr.Name+'"');

  if AttributeCount=length(Attributes) then
  begin
    if AttributeCount<32 then
      SetLength(Attributes,32)
    else
      SetLength(Attributes,2*AttributeCount);
  end;
  Attributes[AttributeCount]:=Attr;
  Attr.Index:=AttributeCount;
  FHashLists[nikAttribute].Add(Attr.Name,Attr);
  inc(FAttributeCount);
  ChangeStamp;
end;

function TCSSRegistry.AddAttribute(const aName: TCSSString;
  const aInitialValue: TCSSString; aInherits: boolean; aAll: boolean;
  aClass: TCSSAttributeDescClass): TCSSAttributeDesc;
begin
  if aName='' then
    raise ECSSParser.Create('missing name');
  if FindAttribute(aName)<>nil then
    raise ECSSParser.Create('duplicate attribute "'+aName+'"');
  if aClass=nil then
    aClass:=Attribute_ClassOf;

  Result:=aClass.Create;
  Result.Name:=aName;
  Result.InitialValue:=aInitialValue;
  Result.Inherits:=aInherits;
  Result.All:=aAll;
  AddAttribute(Result);

  if not aAll then
    Insert(Result,NotAllAttributes,length(NotAllAttributes));
end;

function TCSSRegistry.FindAttribute(const aName: TCSSString
  ): TCSSAttributeDesc;
begin
  Result:=TCSSAttributeDesc(FHashLists[nikAttribute].Find(aName));
end;

function TCSSRegistry.IndexOfAttributeName(const aName: TCSSString
  ): TCSSNumericalID;
var
  Attr: TCSSAttributeDesc;
begin
  Attr:=TCSSAttributeDesc(FHashLists[nikAttribute].Find(aName));
  if Attr<>nil then
    Result:=Attr.Index
  else
    Result:=-1;
end;

procedure TCSSRegistry.AddSplitLonghand(var AttrIDs: TCSSNumericalIDArray;
  var Values: TBytesArray; AttrID: TCSSNumericalID; const Value: TBytes);
begin
  System.Insert(AttrID,AttrIDs,length(AttrIDs));
  System.Insert(Value,Values,length(Values));
end;

procedure TCSSRegistry.AddSplitLonghandSides(var AttrIDs: TCSSNumericalIDArray;
  var Values: TBytesArray; TopID, RightID, BottomID, LeftID: TCSSNumericalID;
  const Found: TBytesArray);
begin
  if length(Found)=0 then
    exit; // invalid

  Setlength(AttrIDs,4);
  Setlength(Values,4);

  AttrIDs[0]:=TopID;
  AttrIDs[1]:=RightID;
  AttrIDs[2]:=BottomID;
  AttrIDs[3]:=LeftID;

  // sets sides depending on how many values were passed:
  // 1: all four the same
  // 2: top and bottom | left and right
  // 3: top | left and right | bottom
  // 4: top | right | bottom | left
  case length(Found) of
  1:
    begin
      Values[0]:=Found[0];
      Values[1]:=Found[0];
      Values[2]:=Found[0];
      Values[3]:=Found[0];
    end;
  2:
    begin
      Values[0]:=Found[0];
      Values[1]:=Found[1];
      Values[2]:=Found[0];
      Values[3]:=Found[1];
    end;
  3:
    begin
      Values[0]:=Found[0];
      Values[1]:=Found[1];
      Values[2]:=Found[2];
      Values[3]:=Found[1];
    end;
  4:
    begin
      Values[0]:=Found[0];
      Values[1]:=Found[1];
      Values[2]:=Found[2];
      Values[3]:=Found[3];
    end;
  end;
end;

procedure TCSSRegistry.AddSplitLonghandCorners(var AttrIDs: TCSSNumericalIDArray;
  var Values: TBytesArray; TopLeftID, TopRightID, BottomLeftID, BottomRightID: TCSSNumericalID;
  const Found: TBytesArray);
begin
  if length(Found)=0 then
    exit; // invalid

  Setlength(AttrIDs,4);
  Setlength(Values,4);

  AttrIDs[0]:=TopLeftID;
  AttrIDs[1]:=TopRightID;
  AttrIDs[2]:=BottomRightID;
  AttrIDs[3]:=BottomLeftID;

  // sets corners depending on how many values were passed:
  // 1: all four the same
  // 2: top-left-and-bottom-right | top-right-and-bottom-left
  // 3: top-left | top-right-and-bottom-left | bottom-right
  // 4: top-left | top-right | bottom-right | bottom-left

  case length(Found) of
  1:
    begin
      Values[0]:=Found[0];
      Values[1]:=Found[0];
      Values[2]:=Found[0];
      Values[3]:=Found[0];
    end;
  2:
    begin
      Values[0]:=Found[0];
      Values[1]:=Found[1];
      Values[2]:=Found[0];
      Values[3]:=Found[1];
    end;
  3:
    begin
      Values[0]:=Found[0];
      Values[1]:=Found[1];
      Values[2]:=Found[2];
      Values[3]:=Found[1];
    end;
  4:
    begin
      Values[0]:=Found[0];
      Values[1]:=Found[1];
      Values[2]:=Found[2];
      Values[3]:=Found[3];
    end;
  end;
end;

procedure TCSSRegistry.AddSplitShorthandPart(
  var Parts: TCSSAttributeDesc.TSplitShorthandParts;
  const AttrIDs: array of TCSSNumericalID; StartPos, EndPos: integer);
var
  i, n: integer;
begin
  if length(AttrIDs)=0 then exit;
  n:=length(Parts);
  SetLength(Parts,n+1);
  SetLength(Parts[n].AttrIDs,length(AttrIDs));
  for i:=0 to high(AttrIDs) do
    Parts[n].AttrIDs[i]:=AttrIDs[i];
  Parts[n].StartPos:=StartPos;
  Parts[n].EndPos:=EndPos;
end;

procedure TCSSRegistry.AddSplitShorthandPartsSides(
  var Parts: TCSSAttributeDesc.TSplitShorthandParts;
  TopID, RightID, BottomID, LeftID: TCSSNumericalID; const Spans: TCSSShorthandTokenSpanArray);
// Groups the four positional ids by the CSS 1/2/3/4-value rule and emits one Part
// per span, so each token span lists the longhands it sets. The 1/2/3/4-value
// distribution matches AddSplitLonghandSides/AddSplitLonghandCorners.
var
  Ids: array[0..3] of TCSSNumericalID;
  SpanIdx: array[0..3] of integer;
  Group: TCSSNumericalIDArray;
  k, j: integer;
begin
  if length(Spans)=0 then exit;
  Ids[0]:=TopID;
  Ids[1]:=RightID;
  Ids[2]:=BottomID;
  Ids[3]:=LeftID;
  case length(Spans) of
  1:
    begin
      SpanIdx[0]:=0;
      SpanIdx[1]:=0;
      SpanIdx[2]:=0;
      SpanIdx[3]:=0;
    end;
  2:
    begin
      SpanIdx[0]:=0;
      SpanIdx[1]:=1;
      SpanIdx[2]:=0;
      SpanIdx[3]:=1;
    end;
  3:
    begin
      SpanIdx[0]:=0;
      SpanIdx[1]:=1;
      SpanIdx[2]:=2;
      SpanIdx[3]:=1;
    end;
  else
    begin
      SpanIdx[0]:=0;
      SpanIdx[1]:=1;
      SpanIdx[2]:=2;
      SpanIdx[3]:=3;
    end;
  end;
  for k:=0 to high(Spans) do
  begin
    Group:=nil;
    for j:=0 to 3 do
      if SpanIdx[j]=k then
        Insert(Ids[j],Group,length(Group));
    AddSplitShorthandPart(Parts,Group,Spans[k].StartPos,Spans[k].EndPos);
  end;
end;

function TCSSRegistry.AddPseudoClass(aPseudo: TCSSPseudoClassDesc
  ): TCSSPseudoClassDesc;
begin
  Result:=aPseudo;
  if aPseudo.Name='' then
    raise ECSSParser.Create('missing name');
  if FindPseudoClass(aPseudo.Name)<>nil then
    raise ECSSParser.Create('duplicate pseudo class "'+aPseudo.Name+'"');

  if PseudoClassCount=length(PseudoClasses) then
  begin
    if PseudoClassCount<32 then
      SetLength(PseudoClasses,32)
    else
      SetLength(PseudoClasses,2*PseudoClassCount);
  end;
  PseudoClasses[PseudoClassCount]:=aPseudo;
  aPseudo.Index:=PseudoClassCount;
  FHashLists[nikPseudoClass].Add(aPseudo.Name,aPseudo);
  inc(FPseudoClassCount);
  ChangeStamp;
end;

function TCSSRegistry.AddPseudoClass(const aName: TCSSString;
  aClass: TCSSPseudoClassDescClass): TCSSPseudoClassDesc;
begin
  if aName='' then
    raise ECSSParser.Create('missing name');
  if aName<>lowercase(aName) then
    raise ECSSParser.Create('name not lowercase');
  if FindPseudoClass(aName)<>nil then
    raise ECSSParser.Create('duplicate pseudo class "'+aName+'"');
  if aClass=nil then
    aClass:=PseudoClass_ClassOf;

  Result:=aClass.Create;
  Result.Name:=aName;
  AddPseudoClass(Result);
end;

function TCSSRegistry.FindPseudoClass(const aName: TCSSString
  ): TCSSPseudoClassDesc;
begin
  Result:=TCSSPseudoClassDesc(FHashLists[nikPseudoClass].Find(aName));
end;

function TCSSRegistry.IndexOfPseudoClassName(const aName: TCSSString
  ): TCSSNumericalID;
var
  aPseudo: TCSSPseudoClassDesc;
begin
  aPseudo:=TCSSPseudoClassDesc(FHashLists[nikPseudoClass].Find(aName));
  if aPseudo<>nil then
    Result:=aPseudo.Index
  else
    Result:=-1;
end;

function TCSSRegistry.AddPseudoElement(aPseudo: TCSSPseudoElementDesc): TCSSPseudoElementDesc;
begin
  Result:=aPseudo;
  if aPseudo.Name='' then
    raise ECSSParser.Create('missing name');
  if FindPseudoElement(aPseudo.Name)<>nil then
    raise ECSSParser.Create('duplicate pseudo element "'+aPseudo.Name+'"');

  if PseudoElementCount=length(PseudoElements) then
  begin
    if PseudoElementCount<32 then
      SetLength(PseudoElements,32)
    else
      SetLength(PseudoElements,2*PseudoElementCount);
  end;
  PseudoElements[PseudoElementCount]:=aPseudo;
  aPseudo.Index:=PseudoElementCount;
  FHashLists[nikPseudoElement].Add(aPseudo.Name,aPseudo);
  inc(FPseudoElementCount);
  ChangeStamp;
end;

function TCSSRegistry.AddPseudoElement(const aName: TCSSString; aClass: TCSSPseudoElementDescClass
  ): TCSSPseudoElementDesc;
begin
  if aName='' then
    raise ECSSParser.Create('missing name');
  if aName<>lowercase(aName) then
    raise ECSSParser.Create('name not lowercase');
  if FindPseudoElement(aName)<>nil then
    raise ECSSParser.Create('duplicate pseudo element "'+aName+'"');
  if aClass=nil then
    aClass:=PseudoElement_ClassOf;

  Result:=aClass.Create;
  Result.Name:=aName;
  AddPseudoElement(Result);
end;

function TCSSRegistry.FindPseudoElement(const aName: TCSSString): TCSSPseudoElementDesc;
begin
  Result:=TCSSPseudoElementDesc(FHashLists[nikPseudoElement].Find(aName));
end;

function TCSSRegistry.IndexOfPseudoElementName(const aName: TCSSString): TCSSNumericalID;
var
  aPseudo: TCSSPseudoElementDesc;
begin
  aPseudo:=TCSSPseudoElementDesc(FHashLists[nikPseudoElement].Find(aName));
  if aPseudo<>nil then
    Result:=aPseudo.Index
  else
    Result:=-1;
end;

function TCSSRegistry.AddPseudoFunction(const aName: TCSSString
  ): TCSSNumericalID;
begin
  if aName='' then
    raise ECSSParser.Create('missing name');
  if length(aName)>255 then
    raise ECSSParser.Create('pseudo function name too long');
  if aName<>LowerCase(aName) then
    raise ECSSParser.Create('pseudo function name not lowercase');
  Result:=IndexOfKeyword(aName);
  if Result>0 then
    raise ECSSParser.Create('duplicate pseudo function "'+aName+'"');

  if PseudoFunctionCount=length(PseudoFunctions) then
  begin
    if PseudoFunctionCount<32 then
      SetLength(PseudoFunctions,32)
    else
      SetLength(PseudoFunctions,2*PseudoFunctionCount);
  end;
  Result:=PseudoFunctionCount;
  PseudoFunctions[Result]:=aName;
  FHashLists[nikPseudoFunction].Add(aName,{%H-}Pointer(Result));
  inc(FPseudoFunctionCount);
  ChangeStamp;
end;

function TCSSRegistry.IndexOfPseudoFunction(const aName: TCSSString
  ): TCSSNumericalID;
var
  p: Pointer;
begin
  p:=FHashLists[nikPseudoFunction].Find(aName);
  if p=nil then
    exit(CSSIDNone)
  else
    Result:={%H-}TCSSNumericalID(p);
end;

function TCSSRegistry.AddType(aType: TCSSTypeDesc): TCSSTypeDesc;
begin
  Result:=aType;
  if aType.Name='' then
    raise ECSSParser.Create('missing name');
  if FindType(aType.Name)<>nil then
    raise ECSSParser.Create('duplicate type "'+aType.Name+'"');

  if TypeCount=length(Types) then
  begin
    if TypeCount<32 then
      SetLength(Types,32)
    else
      SetLength(Types,2*TypeCount);
  end;
  Types[TypeCount]:=aType;
  aType.Index:=TypeCount;
  FHashLists[nikType].Add(aType.Name,aType);
  inc(FTypeCount);
  ChangeStamp;
end;

function TCSSRegistry.AddType(const aName: TCSSString; aClass: TCSSTypeDescClass
  ): TCSSTypeDesc;
begin
  if aName='' then
    raise ECSSParser.Create('missing name');
  if FindType(aName)<>nil then
    raise ECSSParser.Create('duplicate type "'+aName+'"');
  if aClass=nil then
    aClass:=Type_ClassOf;

  Result:=aClass.Create;
  Result.Name:=aName;
  AddType(Result);
end;

function TCSSRegistry.FindType(const aName: TCSSString): TCSSTypeDesc;
begin
  Result:=TCSSTypeDesc(FHashLists[nikType].Find(aName));
end;

function TCSSRegistry.IndexOfTypeName(const aName: TCSSString): TCSSNumericalID;
var
  aType: TCSSTypeDesc;
begin
  aType:=TCSSTypeDesc(FHashLists[nikType].Find(aName));
  if aType<>nil then
    Result:=aType.Index
  else
    Result:=-1;
end;

function TCSSRegistry.AddKeyword(const aName: TCSSString): TCSSNumericalID;
begin
  if aName='' then
    raise ECSSParser.Create('missing name');
  if length(aName)>255 then
    raise ECSSParser.Create('keyword too long');
  Result:=IndexOfKeyword(aName);
  if Result>0 then
    raise ECSSParser.Create('duplicate keyword "'+aName+'"');

  if KeywordCount=length(Keywords) then
  begin
    if KeywordCount<32 then
      SetLength(Keywords,32)
    else
      SetLength(Keywords,2*KeywordCount);
  end;
  if length(KeywordTokens)<length(Keywords) then
    SetLength(KeywordTokens,length(Keywords));
  Result:=KeywordCount;
  Keywords[Result]:=aName;
  // tokenized form: rtkKeyword + word(id)
  SetLength(KeywordTokens[Result],3);
  KeywordTokens[Result][0]:=ord(rtkKeyword);
  PWord(@KeywordTokens[Result][1])^:=Result;
  FHashLists[nikKeyword].Add(lowercase(aName),{%H-}Pointer(Result));
  inc(FKeywordCount);
  ChangeStamp;
end;

procedure TCSSRegistry.AddKeywords(const Names: TCSSStringArray; out First, Last: TCSSNumericalID);
var
  i, NewCnt: integer;
begin
  if Names=nil then begin
    First:=CSSIDNone;
    Last:=CSSIDNone;
    exit;
  end;
  for i:=0 to length(Names)-1 do
    if IndexOfKeyword(Names[i])>CSSIDNone then
      raise ECSSParser.Create('20240712215853');

  NewCnt:=KeywordCount+length(Names);
  if NewCnt>length(Keywords) then
  begin
    NewCnt:=(NewCnt div 32 +1) *32;
    SetLength(Keywords,NewCnt);
  end;

  First:=KeywordCount;
  for i:=0 to length(Names)-1 do
  begin
    Last:=KeywordCount;
    Keywords[Last]:=Names[i];
    FHashLists[nikKeyword].Add(Names[i],{%H-}Pointer(Last));
    inc(FKeywordCount);
  end;
  ChangeStamp;
end;

function TCSSRegistry.IndexOfKeyword(const aName: TCSSString): TCSSNumericalID;
var
  p: Pointer;
begin
  p:=FHashLists[nikKeyword].Find(lowercase(aName));
  if p=nil then
    exit(CSSIDNone)
  else
    Result:={%H-}TCSSNumericalID(p);
end;

function TCSSRegistry.IndexOfValueKeyword(const aName: TCSSString;
  AllowUnknownIdentifiers: boolean): TCSSNumericalID;
// Resolve a word of an attribute value to a keyword, CSSIDNone if there is none.
// Attributes allowing unknown identifiers use case sensitive names, e.g. font-family,
// so they never get a color keyword: 'Red' and 'red' stay identifiers.
begin
  Result:=IndexOfKeyword(aName);
  if AllowUnknownIdentifiers then
  begin
    if IsColorKeyword(Result,false) then
      Result:=CSSIDNone;
    exit;
  end;
end;

procedure TCSSRegistry.AddColorKeywords;
var
  Names: TCSSStringArray;
  i: Integer;
begin
  SetLength(Names{%H-},length(CSSNamedColors));
  for i:=0 to High(CSSNamedColors) do
    Names[i]:=CSSNamedColors[i].Name;
  AddKeywords(Names,kwFirstColor,kwLastColor);

  kwTransparent:=IndexOfKeyword('transparent');
  kwCurrentColor:=AddKeyword('currentColor');
end;

function TCSSRegistry.IsColorKeyword(KeywordID: TCSSNumericalID; OnlyConst: boolean): boolean;
begin
  if (KeywordID>=kwFirstColor) and (KeywordID<=kwLastColor) then
    exit(true);
  if OnlyConst then
    exit(false);
  Result:=(KeywordID=kwCurrentColor)
       or (KeywordID=kwTransparent)
       or ((KeywordID>=kwFirstSystemColor) and (KeywordID<=kwLastSystemColor));
end;

function TCSSRegistry.GetNamedColor(const aName: TCSSString): TCSSAlphaColor;
begin
  Result:=GetKeywordColor(IndexOfKeyword(aName));
end;

function TCSSRegistry.GetKeywordColor(KeywordID: TCSSNumericalID): TCSSAlphaColor;
begin
  if (KeywordID<kwFirstColor) or (KeywordID>kwLastColor) then
    Result:=$ff000000 // black
  else
    Result:=CSSNamedColors[KeywordID-kwFirstColor].Color;
end;

function TCSSRegistry.AddAttrFunction(const aName: TCSSString): TCSSNumericalID;
begin
  if aName='' then
    raise ECSSParser.Create('missing name');
  if length(aName)>255 then
    raise ECSSParser.Create('function name too long');
  Result:=IndexOfAttrFunction(aName);
  if Result>0 then
    raise ECSSParser.Create('duplicate attribute function "'+aName+'"');

  if AttrFunctionCount=length(AttrFunctions) then
  begin
    if AttrFunctionCount<32 then
      SetLength(AttrFunctions,32)
    else
      SetLength(AttrFunctions,2*AttrFunctionCount);
  end;
  Result:=AttrFunctionCount;
  AttrFunctions[Result]:=aName;
  FHashLists[nikAttrFunction].Add(aName,{%H-}Pointer(Result));
  inc(FAttrFunctionCount);
  ChangeStamp;
end;

function TCSSRegistry.IndexOfAttrFunction(const aName: TCSSString
  ): TCSSNumericalID;
var
  p: Pointer;
begin
  p:=FHashLists[nikAttrFunction].Find(aName);
  if p=nil then
    exit(CSSIDNone)
  else
    Result:={%H-}TCSSNumericalID(p);
end;

function TCSSRegistry.IsColorFunction(FuncID: TCSSNumericalID): boolean;
// true if the function returns a <color>, see TCSSBaseResolver.ReadColorFunction
// todo: hsl, hsla, hwb, lab, lch, oklab, oklch, light-dark, color(), color-mix()
begin
  Result:=(FuncID=CSSAttrFuncRGB) or (FuncID=CSSAttrFuncRGBA);
end;

{ TCSSResolvedCallElement }

destructor TCSSResolvedCallElement.Destroy;
begin
  FreeAndNil(Params);
  inherited Destroy;
end;

{ TCSSResCompValue }

function TCSSResCompValue.AsString: TCSSString;
var
  l: integer;
begin
  if (StartP=nil) or (EndP=nil) or (EndP<=StartP) then
    exit('');
  l:=EndP-StartP;
  SetLength(Result,l);
  Move(StartP^,Result[1],l*SizeOf(TCSSChar));
end;

function TCSSResCompValue.FloatAsString: TCSSString;
begin
  Result:=FloatToCSSStr(Float)+CSSUnitNames[FloatUnit];
end;

function TCSSResCompValue.IsInteger: boolean;
begin
  Result:=(Kind=rvkFloat) and (FloatUnit=cuNone);
end;

function TCSSResCompValue.IsIntegerValue(v: Integer): boolean;
begin
  if (Kind<>rvkFloat) or (FloatUnit<>cuNone) then exit(false);
  Result:=SameValue(Float,v);
end;

{ TCSSCheckAttrParams_Dimension }

function TCSSCheckAttrParams_Dimension.Fits(const ResValue: TCSSResCompValue): boolean;
var
  i: Integer;
begin
  Result:=false;
  case ResValue.Kind of
  rvkFloat:
    if ResValue.FloatUnit in AllowedUnits then
    begin
      if not (ResValue.FloatUnit in AllowedUnits) then exit;
      if (not AllowNegative) and (ResValue.Float<0) then exit;
      if (not AllowFrac) and (Frac(ResValue.Float)>0) then exit;
      exit(true);
    end else if (ResValue.FloatUnit=cuNone) and (ResValue.Float=0) then
      exit(true);
  rvkKeyword:
    for i:=0 to length(AllowedKeywordIDs)-1 do
      if ResValue.KeywordID=AllowedKeywordIDs[i] then
        exit(true);
  end;
end;

{ TCSSBaseResolver }

function TCSSBaseResolver.GetCurValue: TCSSString;
begin
  Result:=FCurValue;
  if Result='' then
  begin
    Result:=Detokenize(CurTokens);
    FCurValue:=Result;
  end;
end;

function TCSSBaseResolver.GetIdentifier: TCSSString;
// decode the text payload of the current token, see ReadNext and DetokenizeOne
var
  Cnt: DWord;
  p: integer;
begin
  if FIdentifierValid then
    exit(FIdentifier);
  FIdentifierValid:=true;
  FIdentifier:='';
  p:=CurTokenStart;
  if p<length(CurTokens) then
    case TokenKind of
    rtkIdentifier,rtkStringApos,rtkStringQuote:
      begin
        Cnt:=PDWord(@CurTokens[p+1])^;
        if Cnt>0 then
        begin
          SetLength(FIdentifier,Cnt);
          Move(CurTokens[p+5],FIdentifier[1],Cnt*SizeOf(TCSSChar));
        end;
      end;
    rtkHexColor:
      begin
        Cnt:=CurTokens[p+1];
        if Cnt>0 then
        begin
          SetLength(FIdentifier,Cnt);
          Move(CurTokens[p+2],FIdentifier[1],Cnt*SizeOf(TCSSChar));
        end;
      end;
    rtkSymbol:
      FIdentifier:=TCSSChar(CurTokens[p+1]);
    rtkComma:
      FIdentifier:=',';
    end;
  Result:=FIdentifier;
end;

procedure TCSSBaseResolver.SetCSSRegistry(const AValue: TCSSRegistry);
begin
  if FCSSRegistry=AValue then Exit;
  FCSSRegistry:=AValue;
end;

procedure TCSSBaseResolver.SetHasMediaBoolean(const AValue: TCSSHasMediaBoolEvent);
begin
  if FHasMediaBoolean=AValue then Exit;
  FHasMediaBoolean:=AValue;
  MediaEnvironmentChanged;
end;

procedure TCSSBaseResolver.SetIsMediaPlain(const AValue: TCSSIsMediaPlainEvent);
begin
  if FIsMediaPlain=AValue then Exit;
  FIsMediaPlain:=AValue;
  MediaEnvironmentChanged;
end;

procedure TCSSBaseResolver.SetMediaCompare(const AValue: TCSSMediaCompareEvent);
begin
  if FMediaCompare=AValue then Exit;
  FMediaCompare:=AValue;
  MediaEnvironmentChanged;
end;

procedure TCSSBaseResolver.MediaEnvironmentChanged;
begin
  // nothing to do here, see TCSSResolver.MediaEnvironmentChanged
end;

procedure TCSSBaseResolver.ResetCurComp;
begin
  CurTokens:=nil;
  CurTokenPos:=0;
  CurTokenStart:=0;
  FCurValue:='';
  TokenKind:=rtkNone;
  Float:=0;
  FloatUnit:=cuNone;
  KeywordID:=CSSIDNone;
  FunctionID:=CSSIDNone;
  Symbol:=ctkUNKNOWN;
  FIdentifier:='';
  FIdentifierValid:=true;
  Empty:=true;
end;

function TCSSBaseResolver.InitParseAttr(Desc: TCSSAttributeDesc; AttrData: TCSSAttributeKeyData
  ): boolean;
begin
  Result:=false;
  CurAttrData:=AttrData;
  CurDesc:=Desc;
  ResetCurComp;

  if AttrData.Invalid then
    exit;

  // the value was already tokenized during parsing, reuse the tokens
  CurTokens:=AttrData.Tokens;
  if not ReadNext then
    exit;

  if (CurAttrData<>nil) and (TokenKind=rtkKeyword)
      and IsBaseKeyword(KeywordID) then
  begin
    // a base keyword like "inherit" must be the only component
    if PeekNextTokenKind<>rtkNone then
    begin
      CurAttrData.Invalid:=true;
      exit;
    end;
  end;

  Result:=true;
end;

function TCSSBaseResolver.InitParseAttr(Desc: TCSSAttributeDesc; const Value: TCSSString): boolean;
begin
  CurAttrData:=nil;
  CurDesc:=Desc;
  ResetCurComp;
  FCurValue:=Value;
  if not Tokenize(Value,CurTokens,Desc.AllowUnknownIdentifiers) then
    exit(false);
  Result:=ReadNext;
end;

function TCSSBaseResolver.InitParseAttr(Desc: TCSSAttributeDesc; const Tokens: TBytes): boolean;
begin
  CurAttrData:=nil;
  CurDesc:=Desc;
  ResetCurComp;
  CurTokens:=Tokens;
  Result:=ReadNext;
end;

procedure TCSSBaseResolver.InitParseAttr(const Value: TCSSString);
begin
  CurAttrData:=nil;
  ResetCurComp;
  FCurValue:=Value;
  if Tokenize(Value,CurTokens) then
    ReadNext;
end;

function TCSSBaseResolver.CheckAttribute_Keyword(const Tokens: TBytes): TCSSNumericalID;
// If the value is a single keyword, return its ID, otherwise CSSIDNone.
var
  p, Len: integer;
begin
  Result:=CSSIDNone;
  if Tokens=nil then exit;
  Len:=length(Tokens);
  p:=0;
  // a leading whitespace token (single byte, no payload) is skipped, see Tokenize
  if (p<Len) and (Tokens[p]=ord(rtkWhitespace)) then inc(p);
  if (p+3<>Len) or (Tokens[p]<>ord(rtkKeyword)) then exit;
  Result:=PWord(@Tokens[p+1])^;
end;

function TCSSBaseResolver.CheckAttribute_Keyword(const AllowedKeywordIDs: TCSSNumericalIDArray
  ): boolean;
var
  i: Integer;
begin
  case TokenKind of
  rtkKeyword:
    for i:=0 to length(AllowedKeywordIDs)-1 do
      if KeywordID=AllowedKeywordIDs[i] then
      begin
        if not ReadNext then
          exit(true);
      end;
  rtkFunction:
    if FunctionID=CSSAttrFuncVar then
      // maybe
      exit(true);
  end;

  Result:=false;
end;

function TCSSBaseResolver.CheckAttribute_Keyword_List(
  const AllowedKeywordIDs: TCSSNumericalIDArray): boolean;
var
  i: Integer;
  Fits: Boolean;
begin
  repeat
    Fits:=false;
    case TokenKind of
    rtkKeyword:
      for i:=0 to length(AllowedKeywordIDs)-1 do
        if KeywordID=AllowedKeywordIDs[i] then
        begin
          Fits:=true;
          break;
        end;
    rtkFunction:
      begin
        // todo: check for allowed functions
        Fits:=true;
      end;
    end;
    if not Fits then
      exit(false);
  until not ReadNext;
  Result:=AtEnd;
end;

function TCSSBaseResolver.CheckAttribute_Dimension(const Params: TCSSCheckAttrParams_Dimension
  ): boolean;
var
  i: Integer;
begin
  case TokenKind of
  rtkFloat:
    if IsFloat(Params) and not ReadNext then
      exit(true);
  rtkKeyword:
    for i:=0 to length(Params.AllowedKeywordIDs)-1 do
      if KeywordID=Params.AllowedKeywordIDs[i] then
        if ReadNext then
          break
        else
          exit(true);
  end;

  Result:=false;
end;

function TCSSBaseResolver.CheckAttribute_Color(const AllowedKeywordIDs: TCSSNumericalIDArray
  ): boolean;
var
  i: Integer;
begin
  case TokenKind of
  rtkKeyword:
    begin
      if CSSRegistry.IsColorKeyword(KeywordID,false) then
      begin
        if not ReadNext then
          exit(true);
      end;
      for i:=0 to length(AllowedKeywordIDs)-1 do
        if KeywordID=AllowedKeywordIDs[i] then
          if ReadNext then
            break
          else
            exit(true);
    end;
  rtkFunction:
    if ReadColor then
      exit(true);
  rtkHexColor:
    exit(true);
  end;

  Result:=false;
end;

function TCSSBaseResolver.ReadNext: boolean;
// Reads the next token from CurTokens into the current component fields,
// skipping whitespace tokens. Returns false at the end of the value.

  function ReadByte: Byte;
  begin
    Result:=CurTokens[CurTokenPos];
    inc(CurTokenPos);
  end;

  function ReadWord: Word;
  begin
    Result:=PWord(@CurTokens[CurTokenPos])^;
    inc(CurTokenPos,2);
  end;

  function ReadDWord: DWord;
  begin
    Result:=PDWord(@CurTokens[CurTokenPos])^;
    inc(CurTokenPos,4);
  end;

  function ReadDouble: Double;
  begin
    Result:=PDouble(@CurTokens[CurTokenPos])^;
    inc(CurTokenPos,8);
  end;

var
  Len: integer;
  Cnt: DWord;
begin
  // reset the current component
  Float:=0;
  FloatUnit:=cuNone;
  KeywordID:=CSSIDNone;
  FunctionID:=CSSIDNone;
  Symbol:=ctkUNKNOWN;
  FIdentifierValid:=false; // Identifier is decoded on demand, see GetIdentifier

  Len:=length(CurTokens);
  // any amount of whitespace is a single token and is skipped here
  while (CurTokenPos<Len) and (TCSSResTokenKind(CurTokens[CurTokenPos])=rtkWhitespace) do
    inc(CurTokenPos);

  CurTokenStart:=CurTokenPos;
  if CurTokenPos>=Len then
  begin
    // no more tokens: CurTokenStart=length(CurTokens), see AtEnd
    TokenKind:=rtkNone;
    exit(false);
  end;

  TokenKind:=TCSSResTokenKind(ReadByte);
  if TokenKind<>rtkWhitespace then
    Empty:=false;
  case TokenKind of
  rtkFloat:
    begin
      FloatUnit:=TCSSUnit(ReadByte);
      Float:=ReadDouble;
    end;
  rtkKeyword:
    KeywordID:=ReadWord;
  rtkFunction:
    FunctionID:=ReadWord;
  rtkIdentifier,rtkStringApos,rtkStringQuote:
    begin
      // skip the text, see GetIdentifier
      Cnt:=ReadDWord;
      inc(CurTokenPos,Cnt);
    end;
  rtkHexColor:
    begin
      Cnt:=ReadByte;
      inc(CurTokenPos,Cnt);
    end;
  rtkSymbol:
    case TCSSChar(ReadByte) of
    ':': Symbol:=ctkCOLON;
    ';': Symbol:=ctkSEMICOLON;
    '.': Symbol:=ctkDOT;
    '*': Symbol:=ctkSTAR;
    '/': Symbol:=ctkDIV;
    else Symbol:=ctkUNKNOWN;
    end;
  else
    Symbol:=CSSResTokenToCSS[TokenKind];
  end;
  Result:=true;
end;

function TCSSBaseResolver.PeekNextTokenKind: TCSSResTokenKind;
var
  i, Len: integer;
begin
  Len:=length(CurTokens);
  i:=CurTokenPos;
  while (i<Len) and (TCSSResTokenKind(CurTokens[i])=rtkWhitespace) do
    inc(i);
  if i<Len then
    Result:=TCSSResTokenKind(CurTokens[i])
  else
    Result:=rtkNone;
end;

function TCSSBaseResolver.AtEnd: boolean;
// The current token started at or beyond the end of CurTokens, i.e. the last
// ReadNext found no token. See CurTokenStart in ReadNext.
begin
  Result:=CurTokenStart>=length(CurTokens);
end;

function TCSSBaseResolver.IsFloat(const Params: TCSSCheckAttrParams_Dimension): boolean;
begin
  Result:=false;
  if TokenKind<>rtkFloat then exit;
  if FloatUnit in Params.AllowedUnits then
  begin
    if (not Params.AllowNegative) and (Float<0) then exit;
    if (not Params.AllowFrac) and (Frac(Float)>0) then exit;
    exit(true);
  end else if (FloatUnit=cuNone) and (Float=0) then
    exit(true);
end;

function TCSSBaseResolver.IsDimension(const Params: TCSSCheckAttrParams_Dimension): boolean;
var
  i: Integer;
begin
  Result:=false;
  case TokenKind of
  rtkFloat:
    if FloatUnit in Params.AllowedUnits then
    begin
      if (not Params.AllowNegative) and (Float<0) then exit;
      if (not Params.AllowFrac) and (Frac(Float)>0) then exit;
      exit(true);
    end else if (FloatUnit=cuNone) and (Float=0) then
      exit(true);
  rtkKeyword:
    for i:=0 to length(Params.AllowedKeywordIDs)-1 do
      if Params.AllowedKeywordIDs[i]=KeywordID then
        exit(true);
  end;
end;

function TCSSBaseResolver.IsColor: boolean;
// Note: this only tests the current token, it does not check the arguments of a
// color function and does not advance. Use ReadColor to consume a whole <color>.
begin
  Result:=false;
  case TokenKind of
  rtkKeyword:
    if CSSRegistry.IsColorKeyword(KeywordID,false) then
      exit(true);
  rtkFunction:
    if CSSRegistry.IsColorFunction(FunctionID) then
      exit(true);
  rtkHexColor:
    exit(true);
  end;
end;

function TCSSBaseResolver.ReadColorFunction(out aColor: TCSSAlphaColor): boolean;
// Reads a color function, e.g. rgb() or rgba().
// On entry the current token is the function, which already consumed the '(';
// on success the current token is the closing parenthesis, so a ReadNext works.
//   legacy, comma separated: rgb(R,G,B) and rgb(R,G,B,A)
//   modern, space separated: rgb(R G B) and rgb(R G B / A)
// css-color-4: rgba() is an alias of rgb(), both allow an alpha.
// Out of range values are clamped.
// Note: the legacy syntax requires all three channels to be either numbers or
// percentages. Mixing them is accepted here, browsers are lenient as well and the
// value is clamped either way.
// todo: the 'none' component of css-color-4, e.g. rgb(255 none 0)

  function ReadComponent(out v: byte): boolean;
  // a channel as number 0..255 or percentage 0%..100%
  var
    d: double;
  begin
    Result:=false;
    if TokenKind<>rtkFloat then exit;
    case FloatUnit of
    cuNone: d:=Float;
    cuPercent: d:=Float*255/100;
    else
      exit;
    end;
    if d<=0 then
      v:=0
    else if d>=255 then
      v:=255
    else
      v:=round(d);
    Result:=ReadNext;
  end;

  function ReadAlpha(out v: byte): boolean;
  // alpha as number 0..1 or percentage 0%..100%
  var
    d: double;
  begin
    Result:=false;
    if TokenKind<>rtkFloat then exit;
    case FloatUnit of
    cuNone: d:=Float*255;
    cuPercent: d:=Float*255/100;
    else
      exit;
    end;
    if d<=0 then
      v:=0
    else if d>=255 then
      v:=255
    else
      v:=round(d);
    Result:=ReadNext;
  end;

var
  r, g, b, a: byte;
  Legacy: Boolean;
begin
  Result:=false;
  aColor:=0;
  if TokenKind<>rtkFunction then exit;
  if not CSSRegistry.IsColorFunction(FunctionID) then exit;

  // the function token includes the '(', so ReadNext steps to the first argument
  if not ReadNext then exit;
  if not ReadComponent(r) then exit;

  Legacy:=TokenKind=rtkComma;
  if Legacy and not ReadNext then exit;
  if not ReadComponent(g) then exit;
  if Legacy then
  begin
    if TokenKind<>rtkComma then exit;
    if not ReadNext then exit;
  end;
  if not ReadComponent(b) then exit;

  a:=255;
  if Legacy then
  begin
    if TokenKind=rtkComma then
    begin
      if not ReadNext then exit;
      if not ReadAlpha(a) then exit;
    end;
  end else if IsSymbol(ctkDIV) then
  begin
    if not ReadNext then exit;
    if not ReadAlpha(a) then exit;
  end;

  if TokenKind<>rtkRParenthesis then exit;

  aColor:=(TCSSAlphaColor(a) shl 24) or (TCSSAlphaColor(r) shl 16)
         or (TCSSAlphaColor(g) shl 8) or TCSSAlphaColor(b);
  Result:=true;
end;

function TCSSBaseResolver.ReadColor(out StartPos, EndPos: integer): boolean;
// True if the current token started a <color>.
// After cal current token on last, so that a ReadNext reads the component behind the color,
// e.g. on the ) of rgb().
// EndPos is behind the last token of the color.
var
  aColor: TCSSAlphaColor;
begin
  Result:=false;
  StartPos:=CurTokenStart;
  EndPos:=CurTokenPos;
  case TokenKind of
  rtkKeyword:
    Result:=CSSRegistry.IsColorKeyword(KeywordID,false);
  rtkFunction:
    if CSSRegistry.IsColorFunction(FunctionID) then
    begin
      Result:=ReadColorFunction(aColor);
      EndPos:=CurTokenPos;
    end;
  rtkHexColor:
    Result:=true;
  end;
end;

function TCSSBaseResolver.ReadColor: boolean;
var
  StartPos, EndPos: integer;
begin
  Result:=ReadColor(StartPos{%H-},EndPos{%H-});
end;

function TCSSBaseResolver.IsBaseKeyword(aKeywordID: TCSSNumericalID): boolean;
begin
  Result:=(aKeywordID>=CSSKeywordInitial) and (aKeywordID<=CSSKeywordRevertLayer);
end;

function TCSSBaseResolver.IsKeywordIn(aKeywordID: TCSSNumericalID;
  const KeywordIDs: TCSSNumericalIDArray): boolean;
var
  i: Integer;
begin
  for i:=0 to length(KeywordIDs)-1 do
    if KeywordIDs[i]=aKeywordID then
      exit(true);
  Result:=false;
end;

function TCSSBaseResolver.IsKeywordIn(const KeywordIDs: TCSSNumericalIDArray): boolean;
var
  aKeywordID: TCSSNumericalID;
  i: Integer;
begin
  Result:=false;
  if TokenKind<>rtkKeyword then exit;
  aKeywordID:=KeywordID;
  for i:=0 to length(KeywordIDs)-1 do
    if KeywordIDs[i]=aKeywordID then
      exit(true);
end;

function TCSSBaseResolver.IsLengthOrPercentage(AllowNegative: boolean): boolean;
begin
  Result:=false;
  case TokenKind of
  rtkFloat:
    if FloatUnit in cuAllLengthsAndPercent then
    begin
      if (not AllowNegative) and (Float<0) then exit;
      exit(true);
    end
    else if (FloatUnit=cuNone) and (Float=0) then
      exit(true); // 0 without unit is allowed
  end;
end;

function TCSSBaseResolver.IsSymbol(Token: TCSSToken): boolean;
begin
  Result:=(Symbol=Token)
    and (TokenKind in [rtkSymbol,rtkComma,rtkPlus,rtkMinus,
                  rtkLParenthesis,rtkRParenthesis,rtkLBracket,rtkRBracket]);
end;

function TCSSBaseResolver.GetCompString: TCSSString;
begin
  case TokenKind of
  rtkKeyword: Result:=CSSRegistry.Keywords[KeywordID];
  rtkFunction: Result:=CSSRegistry.AttrFunctions[FunctionID]+'(';
  rtkFloat: Result:=FloatAsString;
  rtkStringApos: Result:=''''+Identifier+'''';
  rtkStringQuote: Result:='"'+Identifier+'"';
  rtkHexColor: Result:='#'+Identifier;
  else
    // rtkIdentifier and symbols carry their text in Identifier
    Result:=Identifier;
  end;
end;

function TCSSBaseResolver.GetCompTokens: TBytes;
// returns the bytes of the current token as a self-contained tokenized value
// (see CurTokenStart/CurTokenPos in ReadNext)
var
  Len: integer;
begin
  Len:=CurTokenPos-CurTokenStart;
  if Len<=0 then exit(nil);
  Result:=Copy(CurTokens,CurTokenStart,Len);
end;

function TCSSBaseResolver.FloatAsString: TCSSString;
begin
  Result:=FloatToCSSStr(Float)+CSSUnitNames[FloatUnit];
end;

function TCSSBaseResolver.IsInteger: boolean;
begin
  Result:=(TokenKind=rtkFloat) and (FloatUnit=cuNone);
end;

function TCSSBaseResolver.IsIntegerValue(v: Integer): boolean;
begin
  if (TokenKind<>rtkFloat) or (FloatUnit<>cuNone) then exit(false);
  Result:=SameValue(Float,v);
end;

function TCSSBaseResolver.Tokenize(const aValue: string; out aData: TBytes; AllowUnknownIdentifiers: boolean;
  TrimEnclosingSpace: boolean): boolean;
// Parses a css property value and creates tokens, see TCSSResTokenKind.
// Returns false if the value is invalid, e.g. if it consists only of whitespace.
// When TrimEnclosingSpace is true, leading and trailing whitespace tokens are omitted.
// Note: this does not use TCSSResCompValue and has its own number and
// identifier readers, so it can be used independently of the value parser.
var
  p, StartP: PCSSChar;
  DataLen, Len: integer;
  BracketStack: array of TCSSChar; // expected closing brackets
  BracketTop: integer;
  PendingWhitespace, SawRealToken: boolean;

  procedure AddMem(Src: Pointer; Count: integer);
  var
    l, Needed: integer;
  begin
    Needed:=DataLen+Count;
    l:=length(aData);
    if Needed>l then
    begin
      if l=0 then l:=16;
      while l<Needed do l:=l*2;
      SetLength(aData,l);
    end;
    Move(Src^,aData[DataLen],Count);
    inc(DataLen,Count);
  end;

  procedure AddByte(b: byte);
  var
    l: integer;
  begin
    l:=length(aData);
    if DataLen>=l then
    begin
      if l=0 then l:=16 else l:=l*2;
      SetLength(aData,l);
    end;
    aData[DataLen]:=b;
    inc(DataLen);
  end;

  procedure AddKind(aKind: TCSSResTokenKind);
  begin
    AddByte(byte(ord(aKind)));
  end;

  procedure AddWord(w: word);
  begin
    AddMem(@w,2);
  end;

  procedure AddDWord(d: cardinal);
  begin
    AddMem(@d,4);
  end;

  procedure PushBracket(CloseBracket: TCSSChar);
  begin
    if BracketTop>=length(BracketStack) then
      SetLength(BracketStack,BracketTop*2+8);
    BracketStack[BracketTop]:=CloseBracket;
    inc(BracketTop);
  end;

  function ReadNumberToken: boolean;
  // p is at the start of a number ('+','-','.' or a digit).
  // On success emits a rtkFloat token and advances p behind the optional unit.
  var
    Negative, HasNumber: Boolean;
    Divisor, d, Value: double;
    Exponent: Integer;
    U: TCSSUnit;
    UnitName: TCSSString;
  begin
    Result:=false;
    Value:=0;
    if p^='-' then
    begin
      Negative:=true;
      inc(p);
    end else begin
      Negative:=false;
      if p^='+' then
        inc(p);
    end;
    HasNumber:=false;
    if p^ in Num then
    begin
      // read significand
      HasNumber:=true;
      repeat
        Value:=Value*10+ord(p^)-ord('0');
        if Value>CSSMaxSafeIntDouble then
          exit; // loosing precision
        inc(p);
      until not (p^ in Num);
    end;
    if p^='.' then
    begin
      // read fraction
      inc(p);
      if not (p^ in Num) then exit;
      Divisor:=1;
      repeat
        Divisor:=Divisor*10;
        Value:=Value*10+ord(p^)-ord('0');
        if (Divisor>CSSMaxSafeIntDouble) or (Value>CSSMaxSafeIntDouble) then
          exit; // loosing precision
        inc(p);
      until not (p^ in Num);
      Value:=Value/Divisor;
    end else if not HasNumber then
      exit;
    if Negative then
      Value:=-Value;

    if (p^ in ['e','E']) and not (p[1] in ['a'..'z']) then
    begin
      // read exponent
      inc(p);
      if p^='-' then
      begin
        Negative:=true;
        inc(p);
      end else begin
        Negative:=false;
        if p^='+' then
          inc(p);
      end;
      if not (p^ in Num) then exit;
      Exponent:=0;
      repeat
        Exponent:=Exponent*10+ord(p^)-ord('0');
        if Exponent>2047 then
          exit; // out of bounds
        inc(p);
      until not (p^ in Num);
      if Exponent>0 then
      begin
        if Negative then
          Exponent:=-Exponent;
        try
          d:=Power(10,Exponent);
          Value:=Value*d;
        except
          exit;
        end;
      end;
    end;

    // parse unit
    U:=cuNone;
    case p^ of
    '%':
      begin
        inc(p);
        U:=cuPercent;
      end;
    'a'..'z','A'..'Z':
      begin
        StartP:=p;
        while p^ in Alpha do inc(p);
        SetString(UnitName,StartP,p-StartP);
        U:=high(TCSSUnit);
        while (U>cuNone) and (CSSUnitNames[U]<>UnitName) do
          U:=pred(U);
        if U=cuNone then
          exit; // unknown unit
      end;
    end;

    AddKind(rtkFloat);
    AddByte(byte(ord(U)));
    AddMem(@Value,SizeOf(double));
    Result:=true;
  end;

  function ReadWordToken: boolean;
  // p is at the start of an identifier word (AlIden).
  // '--xxx'  -> rtkIdentifier
  // 'name('  -> rtkFunction if the function is known, else invalid
  // 'name'   -> rtkKeyword if the keyword is known, else invalid
  // '-name'  -> as 'name', a single leading dash is part of the word,
  //             e.g. the custom identifier -fade
  var
    Name: TCSSString;
    FuncID, KeywordID: TCSSNumericalID;
  begin
    Result:=false;
    StartP:=p;
    repeat
      inc(p);
    until not (p^ in AlNumIden);
    Len:=p-StartP;
    if p^='(' then
    begin
      // function call, the token includes the opening parenthesis
      if (Len>=2) and (StartP[0]='-') and (StartP[1]<>'-') then
      begin
        // a single dash in front of a function is the minus operator,
        // e.g. -calc(), it is not part of the function name
        AddKind(rtkMinus);
        inc(StartP);
        dec(Len);
      end;
      // function names are ASCII case-insensitive, e.g. var() = VAR()
      SetString(Name,StartP,Len);
      FuncID:=CSSRegistry.IndexOfAttrFunction(LowerCase(Name));
      if FuncID<=CSSIDNone then
        exit; // unknown function
      inc(p); // consume '('
      AddKind(rtkFunction);
      AddWord(word(FuncID));
      PushBracket(')');
      exit(true);
    end;
    if (Len>=2) and (StartP[0]='-') and (StartP[1]='-') then
    begin
      // custom identifier, e.g. --my-var
      AddKind(rtkIdentifier);
      AddDWord(cardinal(Len));
      AddMem(StartP,Len*SizeOf(TCSSChar));
      exit(true);
    end;
    SetString(Name,StartP,Len);
    KeywordID:=CSSRegistry.IndexOfValueKeyword(Name,AllowUnknownIdentifiers);
    if KeywordID<=CSSIDNone then
    begin
      if not AllowUnknownIdentifiers then
        exit; // unknown keyword
      // store the unknown word as a custom identifier, e.g. a font-family name
      AddKind(rtkIdentifier);
      AddDWord(cardinal(Len));
      AddMem(StartP,Len*SizeOf(TCSSChar));
      exit(true);
    end;
    AddKind(rtkKeyword);
    AddWord(word(KeywordID));
    Result:=true;
  end;

begin
  Result:=false;
  aData:=nil;
  DataLen:=0;
  BracketStack:=nil;
  BracketTop:=0;
  PendingWhitespace:=false;
  SawRealToken:=false;

  p:=PCSSChar(aValue);
  if p<>nil then
    repeat
      if p^ in Whitespace then
      begin
        // any amount of whitespace becomes a single token
        repeat
          inc(p);
        until not (p^ in Whitespace);
        PendingWhitespace:=true;
      end;
      if p^=#0 then break;
      if PendingWhitespace then
      begin
        PendingWhitespace:=false;
        // skip leading whitespace when trimming
        if SawRealToken or not TrimEnclosingSpace then
          AddKind(rtkWhitespace);
      end;
      SawRealToken:=true;
      case p^ of
      #0: break;
      '0'..'9':
        if not ReadNumberToken then exit;
      '.':
        if p[1] in Num then
        begin
          if not ReadNumberToken then exit;
        end else begin
          AddKind(rtkSymbol);
          AddByte(byte(ord(p^)));
          inc(p);
        end;
      '+':
        case p[1] of
        '0'..'9','.':
          if not ReadNumberToken then exit;
        else
          AddKind(rtkPlus);
          inc(p);
        end;
      '-':
        case p[1] of
        '0'..'9','.':
          if not ReadNumberToken then exit;
        '-','a'..'z','A'..'Z':
          // custom identifier, e.g. --my-var or -fade
          if not ReadWordToken then exit;
        else
          AddKind(rtkMinus);
          inc(p);
        end;
      'a'..'z','A'..'Z':
        if not ReadWordToken then exit;
      ',':
        begin
          AddKind(rtkComma);
          inc(p);
        end;
      ':',';','*','/':
        begin
          AddKind(rtkSymbol);
          AddByte(byte(ord(p^)));
          inc(p);
        end;
      '(':
        begin
          AddKind(rtkLParenthesis);
          PushBracket(')');
          inc(p);
        end;
      ')':
        begin
          if (BracketTop=0) or (BracketStack[BracketTop-1]<>')') then exit;
          dec(BracketTop);
          AddKind(rtkRParenthesis);
          inc(p);
        end;
      '[':
        begin
          AddKind(rtkLBracket);
          PushBracket(']');
          inc(p);
        end;
      ']':
        begin
          if (BracketTop=0) or (BracketStack[BracketTop-1]<>']') then exit;
          dec(BracketTop);
          AddKind(rtkRBracket);
          inc(p);
        end;
      '#':
        begin
          StartP:=p;
          inc(p);
          while p^ in Hex do inc(p);
          Len:=(p-StartP)-1; // number of hex digits
          case Len of
          3,4,6,8:
            begin
              // #rgb, #rgba, #rrggbb, #rrggbbaa
              AddKind(rtkHexColor);
              AddByte(byte(Len));
              AddMem(StartP+1,Len*SizeOf(TCSSChar));
            end;
          else
            exit; // invalid hex color
          end;
        end;
      '''':
        begin
          StartP:=p;
          if not SkipString(p) then exit; // missing end apostroph
          Len:=(p-StartP)-2; // without enclosing apostrophs
          AddKind(rtkStringApos);
          AddDWord(cardinal(Len));
          AddMem(StartP+1,Len*SizeOf(TCSSChar));
        end;
      '"':
        begin
          StartP:=p;
          if not SkipString(p) then exit; // missing end quote
          Len:=(p-StartP)-2; // without enclosing quotes
          AddKind(rtkStringQuote);
          AddDWord(cardinal(Len));
          AddMem(StartP+1,Len*SizeOf(TCSSChar));
        end;
      else
        exit; // unknown symbol
      end;
    until false;

  // a value consisting only of whitespace (or empty) is invalid
  if not SawRealToken then exit;

  // keep a trailing whitespace token unless trimming
  if PendingWhitespace and not TrimEnclosingSpace then
    AddKind(rtkWhitespace);

  // brackets and parenthesis must be balanced
  if BracketTop>0 then exit;

  SetLength(aData,DataLen);
  Result:=true;
end;

function TCSSBaseResolver.TokenizeKeyword(KW: TCSSNumericalID): TBytes;
begin
  Result:=nil;
  SetLength(Result,3);
  Result[0]:=ord(rtkKeyword);
  PWord(@Result[1])^:=KW;
end;

function TCSSBaseResolver.TokenizeIdentifier(const anIdentifier: TCSSString): TBytes;
var
  l: DWord;
begin
  l:=length(anIdentifier);
  Result:=nil;
  if l=0 then exit;
  SetLength(Result,1+4+l*SizeOf(TCSSChar));
  Result[0]:=ord(rtkIdentifier);
  PDWord(@Result[1])^:=l;
  Move(anIdentifier[1],Result[5],l*SizeOf(TCSSChar));
end;

function TCSSBaseResolver.TokenizeFloat(const aFloat: double; anUnit: TCSSUnit): TBytes;
begin
  Result:=nil;
  SetLength(Result,10);
  Result[0]:=ord(rtkFloat);
  Result[1]:=ord(anUnit);
  PDouble(@Result[2])^:=aFloat; // kind + unit + double, see ReadNext
end;

function TCSSBaseResolver.TokenizeHexColor(const aHexDigits: TCSSString): TBytes;
// aHexDigits are the hex characters without the leading '#'.
// Only the four lengths Tokenize creates are valid: #rgb, #rgba, #rrggbb and #rrggbbaa.
// Any other length gives nil, so a caller cannot build a value that Tokenize would refuse.
var
  l: integer;
begin
  Result:=nil;
  l:=length(aHexDigits);
  case l of
  3,4,6,8: ;
  else
    exit;
  end;
  SetLength(Result,2+l*SizeOf(TCSSChar));
  Result[0]:=ord(rtkHexColor);
  Result[1]:=byte(l);
  Move(aHexDigits[1],Result[2],l*SizeOf(TCSSChar)); // kind + count + count hex chars, see ReadNext
end;

function TCSSBaseResolver.ResolveIdentifierTokens(var Tokens: TBytes): boolean;
// An attribute that does not allow unknown identifiers must not contain
// rtkIdentifier tokens, except the custom identifiers --xxx.
// A value containing a var() is tokenized before the target attribute is known,
// so the identifiers are converted to keywords here, e.g. 'Red' to the keyword red.
// Returns false if a word is not a keyword, i.e. the value is invalid.
var
  Len, Ofs, TokLen, NewLen: integer;
  aName: TCSSString;
  aKeywordID: TCSSNumericalID;
  NeedChange: boolean;
  NewTokens: TBytes;

  function ReadName(aOfs: integer): TCSSString;
  // see GetIdentifier
  var
    Cnt: DWord;
  begin
    Result:='';
    Cnt:=PDWord(@Tokens[aOfs+1])^;
    if Cnt=0 then exit;
    SetLength(Result,Cnt);
    Move(Tokens[aOfs+5],Result[1],Cnt*SizeOf(TCSSChar));
  end;

  function IsCustomIdentifier(const anIdentifier: TCSSString): boolean;
  begin
    Result:=(length(anIdentifier)>=2) and (anIdentifier[1]='-') and (anIdentifier[2]='-');
  end;

begin
  Result:=true;
  Len:=length(Tokens);

  // check if there is an identifier to convert
  NeedChange:=false;
  Ofs:=0;
  while Ofs<Len do
  begin
    if TCSSResTokenKind(Tokens[Ofs])=rtkIdentifier then
    begin
      aName:=ReadName(Ofs);
      if not IsCustomIdentifier(aName) then
      begin
        if CSSRegistry.IndexOfValueKeyword(aName,false)<=CSSIDNone then
          exit(false); // this attribute does not allow unknown identifiers
        NeedChange:=true;
      end;
    end;
    TokLen:=CSSTokenByteLen(Tokens,Ofs);
    if TokLen<=0 then
      exit(false);
    inc(Ofs,TokLen);
  end;
  if not NeedChange then
    exit;

  // replace the identifiers with keywords, a keyword token is always shorter
  NewTokens:=nil;
  SetLength(NewTokens,Len);
  NewLen:=0;
  Ofs:=0;
  while Ofs<Len do
  begin
    TokLen:=CSSTokenByteLen(Tokens,Ofs);
    if TCSSResTokenKind(Tokens[Ofs])=rtkIdentifier then
    begin
      aName:=ReadName(Ofs);
      if not IsCustomIdentifier(aName) then
      begin
        aKeywordID:=CSSRegistry.IndexOfValueKeyword(aName,false);
        NewTokens[NewLen]:=ord(rtkKeyword);
        PWord(@NewTokens[NewLen+1])^:=word(aKeywordID);
        inc(NewLen,3);
        inc(Ofs,TokLen);
        continue;
      end;
    end;
    Move(Tokens[Ofs],NewTokens[NewLen],TokLen);
    inc(NewLen,TokLen);
    inc(Ofs,TokLen);
  end;
  SetLength(NewTokens,NewLen);
  Tokens:=NewTokens;
end;

function TCSSBaseResolver.Detokenize(const aData: TBytes): TCSSString;
// Converts a token array created by Tokenize back to a css value.
var
  DataLen, i: integer;

  function ReadByte: byte;
  begin
    Result:=aData[i];
    inc(i);
  end;

  function ReadWord: word;
  begin
    Result:=PWord(@aData[i])^;
    inc(i,2);
  end;

  function ReadDWord: DWord;
  begin
    Result:=PDWord(@aData[i])^;
    inc(i,4);
  end;

  function ReadDouble: double;
  begin
    Result:=PDouble(@aData[i])^;
    inc(i,8);
  end;

  function ReadStr(Count: cardinal): TCSSString;
  begin
    Result:='';
    if Count=0 then exit;
    SetLength(Result,Count);
    Move(aData[i],Result[1],Count*SizeOf(TCSSChar));
    inc(i,Count*SizeOf(TCSSChar));
  end;

var
  aKind: TCSSResTokenKind;
  U: TCSSUnit;
begin
  Result:='';
  DataLen:=length(aData);
  i:=0;
  while i<DataLen do
  begin
    aKind:=TCSSResTokenKind(ReadByte);
    case aKind of
    rtkWhitespace: Result:=Result+' ';
    rtkSymbol: Result:=Result+TCSSChar(ReadByte);
    rtkComma: Result:=Result+',';
    rtkLParenthesis: Result:=Result+'(';
    rtkRParenthesis: Result:=Result+')';
    rtkLBracket: Result:=Result+'[';
    rtkRBracket: Result:=Result+']';
    rtkPlus: Result:=Result+'+';
    rtkMinus: Result:=Result+'-';
    rtkFloat:
      begin
        U:=TCSSUnit(ReadByte);
        Result:=Result+FloatToCSSStr(ReadDouble)+CSSUnitNames[U];
      end;
    rtkKeyword: Result:=Result+CSSRegistry.Keywords[ReadWord];
    rtkFunction: Result:=Result+CSSRegistry.AttrFunctions[ReadWord]+'(';
    rtkIdentifier: Result:=Result+ReadStr(ReadDWord);
    rtkStringApos: Result:=Result+''''+ReadStr(ReadDWord)+'''';
    rtkStringQuote: Result:=Result+'"'+ReadStr(ReadDWord)+'"';
    rtkHexColor: Result:=Result+'#'+ReadStr(ReadByte);
    else
      break;
    end;
  end;
end;

function TCSSBaseResolver.DetokenizeOne(aData: PByte): TCSSString;

  function ReadStr(Cnt, Ofs: DWord): TCSSString;
  begin
    Result:='';
    if Cnt=0 then exit;
    SetLength(Result,Cnt);
    Move(aData[Ofs],Result[1],Cnt*SizeOf(TCSSChar));
  end;

var
  aKind: TCSSResTokenKind;
begin
  aKind:=TCSSResTokenKind(aData^);
  case aKind of
  rtkWhitespace: Result:=' ';
  rtkSymbol: Result:=TCSSChar(aData[1]);
  rtkComma: Result:=',';
  rtkLParenthesis: Result:='(';
  rtkRParenthesis: Result:=')';
  rtkLBracket: Result:='[';
  rtkRBracket: Result:=']';
  rtkPlus: Result:='+';
  rtkMinus: Result:='-';
  rtkFloat: Result:=FloatToCSSStr(PDouble(@aData[2])^)+CSSUnitNames[TCSSUnit(aData[1])];
  rtkKeyword: Result:=CSSRegistry.Keywords[PWord(@aData[1])^];
  rtkFunction: Result:=CSSRegistry.AttrFunctions[PWord(@aData[1])^]+'(';
  rtkIdentifier: Result:=ReadStr(PDWord(@aData[1])^,5);
  rtkStringApos: Result:=''''+ReadStr(PDWord(@aData[1])^,5)+'''';
  rtkStringQuote: Result:='"'+ReadStr(PDWord(@aData[1])^,5)+'"';
  rtkHexColor: Result:='#'+ReadStr(aData[1],2);
  else
    Result:='';
  end;
end;

class procedure TCSSBaseResolver.SkipToEndOfAttribute(var p: PCSSChar);
begin
  repeat
    case p^ of
    #0,'{','}',';': exit;
    '''','"': SkipString(p);
    else inc(p);
    end;
  until false;
end;

class function TCSSBaseResolver.SkipString(var p: PCSSChar): boolean;
var
  Delim, c: TCSSChar;
begin
  Result:=false;
  Delim:=p^;
  repeat
    inc(p);
    c:=p^;
    if c=Delim then
    begin
      inc(p);
      exit(true);
    end else if c=#0 then
      exit;
  until false;
end;

class function TCSSBaseResolver.SkipBrackets(var p: PCSSChar; Lvl: integer): boolean;
const
  CSSMaxBracketLvl = 10;
var
  CloseBracket: TCSSChar;
begin
  Result:=false;
  if Lvl>CSSMaxBracketLvl then
  begin
    SkipToEndOfAttribute(p);
    exit;
  end;

  if p^='[' then
    CloseBracket:=']'
  else
    CloseBracket:=')';
  repeat
    inc(p);
    case p^ of
    #0,'{','}',';': exit;
    '''','"': SkipString(p);
    '(','[': SkipBrackets(p,Lvl+1);
    ')',']':
      if p^=CloseBracket then
      begin
        inc(p);
        exit(true);
      end else begin
        SkipToEndOfAttribute(p);
        exit;
      end;
    end;
  until false;
end;

function TCSSBaseResolver.GetAttributeID(const aName: TCSSString; AutoCreate: boolean
  ): TCSSNumericalID;
begin
  Result:=CSSRegistry.IndexOfAttributeName(aName);
  if AutoCreate then ;
end;

function TCSSBaseResolver.GetAttributeDesc(AttrID: TCSSNumericalID): TCSSAttributeDesc;
begin
  if (AttrID>0) and (AttrID<CSSRegistry.AttributeCount) then
    Result:=CSSRegistry.Attributes[AttrID]
  else
    Result:=nil;
end;

function TCSSBaseResolver.GetTypeID(const aName: TCSSString): TCSSNumericalID;
begin
  Result:=CSSRegistry.IndexOfTypeName(aName);
end;

function TCSSBaseResolver.GetPseudoClassID(const aName: TCSSString): TCSSNumericalID;
begin
  Result:=CSSRegistry.IndexOfPseudoClassName(aName);
end;

function TCSSBaseResolver.GetCSSClassID(const aCSSClassName: TCSSString): TCSSNumericalID;
begin
  if aCSSClassName='' then ;
  Result:=CSSIDNone;
end;

function TCSSBaseResolver.AddCSSClassID(const aCSSClassName: TCSSString): TCSSNumericalID;
begin
  Result:=GetCSSClassID(aCSSClassName);
end;

function TCSSBaseResolver.GetCSSIDIndex(const aCSSID: TCSSString): TCSSNumericalID;
begin
  if aCSSID='' then ;
  Result:=CSSIDNone;
end;

function TCSSBaseResolver.AddCSSID(const aCSSID: TCSSString): TCSSNumericalID;
begin
  Result:=GetCSSIDIndex(aCSSID);
end;

function TCSSBaseResolver.GetPseudoElementID(const aName: TCSSString): TCSSNumericalID;
begin
  Result:=CSSRegistry.IndexOfPseudoElementName(aName);
  if (Result>=0) and CSSRegistry.PseudoElements[Result].IsFunction then
    Result:=-1;
end;

function TCSSBaseResolver.GetPseudoElFuncID(const aName: TCSSString): TCSSNumericalID;
begin
  Result:=CSSRegistry.IndexOfPseudoElementName(aName);
  if (Result>=0) and not CSSRegistry.PseudoElements[Result].IsFunction then
    Result:=-1;
end;

function TCSSBaseResolver.GetPseudoFunctionID(const aName: TCSSString): TCSSNumericalID;
begin
  Result:=CSSRegistry.IndexOfPseudoFunction(aName);
end;

{ TCSSResolverParser }

function TCSSResolverParser.ResolveAttribute(El: TCSSResolvedIdentifierElement): TCSSNumericalID;
var
  aName: TCSSString;
begin
  if El.NumericalID<>CSSIDNone then
    raise ECSSParser.Create('20240701143234');
  aName:=El.Name;
  El.Kind:=nikAttribute;
  Result:=Resolver.GetAttributeID(aName,true);
  if Result<=CSSIDNone then
  begin
    El.NumericalID:=-1;
    Log(etWarning,20240822172823,'unknown attribute "'+aName+'"',El);
  end else
    El.NumericalID:=Result;
end;

function TCSSResolverParser.ResolveType(El: TCSSResolvedIdentifierElement): TCSSNumericalID;
var
  aName: TCSSString;
begin
  if El.NumericalID<>CSSIDNone then
    raise ECSSParser.Create('20240822133813');
  aName:=El.Name;
  El.Kind:=nikType;
  Result:=Resolver.GetTypeID(aName);
  if Result<=CSSIDNone then
  begin
    El.NumericalID:=-1;
    Log(etWarning,20240822133816,'unknown type "'+aName+'"',El);
  end else
    El.NumericalID:=Result;
end;

function TCSSResolverParser.ResolvePseudoClass(
  El: TCSSResolvedPseudoClassElement): TCSSNumericalID;
var
  aName: TCSSString;
begin
  aName:=El.Name;
  // pseudo classes are ASCII case insensitive
  System.Delete(aName,1,1);
  aName:=lowercase(aName);

  if El.NumericalID<>CSSIDNone then
    raise ECSSParser.Create('20240701143234');

  El.Kind:=nikPseudoClass;
  Result:=Resolver.GetPseudoClassID(aName);
  //writeln('TCSSResolverParser.ResolvePseudoClass ',aName,' ID=',Result);
  if Result<=CSSIDNone then
  begin
    El.NumericalID:=-1;
    Log(etWarning,20240822172826,'unknown pseudo class "'+aName+'"',El);
  end else
    El.NumericalID:=Result;
end;

function TCSSResolverParser.ResolveClassName(El: TCSSResolvedClassNameElement
  ): TCSSNumericalID;
begin
  if El.NumericalID<>CSSIDNone then
    raise ECSSParser.Create('20260620120000');
  // class names are case sensitive and registered on first use
  Result:=Resolver.AddCSSClassID(El.Name);
  El.NumericalID:=Result;
end;

function TCSSResolverParser.ResolveHashIdentifier(
  El: TCSSResolvedHashIdentifierElement): TCSSNumericalID;
begin
  if El.NumericalID<>CSSIDNone then
    raise ECSSParser.Create('20260621120000');
  // ids are case sensitive and registered on first use
  Result:=Resolver.AddCSSID(El.Value);
  El.NumericalID:=Result;
end;

function TCSSResolverParser.ResolvePseudoElement(El: TCSSResolvedIdentifierElement
  ): TCSSNumericalID;
var
  aName: TCSSString;
begin
  // pseudo elements are ASCII case insensitive
  aName:=lowercase(El.Name);

  if El.NumericalID<>CSSIDNone then
    raise ECSSParser.Create('20250224203646');

  El.Kind:=nikPseudoElement;
  Result:=Resolver.GetPseudoElementID(aName);
  //writeln('TCSSResolverParser.ResolvePseudoElement ',aName,' ID=',Result);
  if Result<=CSSIDNone then
  begin
    El.NumericalID:=-1;
    Log(etWarning,20250224203703,'unknown pseudo element "'+aName+'"',El);
  end else
    El.NumericalID:=Result;
end;

function TCSSResolverParser.ResolvePseudoElementFunction(El: TCSSResolvedCallElement
  ): TCSSNumericalID;
var
  aName: TCSSString;
begin
  // pseudo elements are ASCII case insensitive
  aName:=lowercase(El.Name);

  if El.NameNumericalID<>CSSIDNone then
    raise ECSSParser.Create('20250224210628');

  El.Kind:=nikPseudoElement;
  Result:=Resolver.GetPseudoElFuncID(aName);
  //writeln('TCSSResolverParser.ResolvePseudoElement ',aName,' ID=',Result);
  if Result<=CSSIDNone then
  begin
    El.NameNumericalID:=-1;
    Log(etWarning,20250224203703,'unknown pseudo element function "'+aName+'"',El);
  end else
    El.NameNumericalID:=Result;
end;

function TCSSResolverParser.ResolvePseudoFunction(El: TCSSResolvedCallElement
  ): TCSSNumericalID;
var
  aName: TCSSString;
begin
  if El.NameNumericalID<>CSSIDNone then
    raise ECSSParser.Create('20240701143035');
  aName:=El.Name;
  if aName[1]<>':' then
    raise ECSSParser.Create('20240701143650');

  // pseudo functions are ASCII case insensitive
  System.Delete(aName,1,1);
  aName:=lowercase(aName);

  El.Kind:=nikPseudoFunction;
  Result:=Resolver.GetPseudoFunctionID(aName);
  //writeln('TCSSResolverParser.ResolvePseudoFunction ',aName,' ID=',Result);
  if Result<=CSSIDNone then
  begin
    El.NameNumericalID:=-1;
    Log(etWarning,20240822172830,'unknown pseudo function "'+aName+'"',El);
  end else
    El.NameNumericalID:=Result;
end;

function TCSSResolverParser.ResolveMediaIdentifier(El: TCSSResolvedIdentifierElement
  ): TCSSNumericalID;
var
  aName: TCSSString;
begin
  if El.NumericalID<>CSSIDNone then
    raise ECSSParser.Create('20260323130501');
  aName:=El.Name;
  El.Kind:=nikKeyword;
  Result:=Resolver.CSSRegistry.IndexOfKeyword(aName);
  if Result<=CSSIDNone then
  begin
    El.NumericalID:=-1;
    Log(etWarning,20260323130502,'unknown media keyword "'+aName+'"',El);
  end else
    El.NumericalID:=Result;
end;

procedure TCSSResolverParser.CheckMediaSelector(El: TCSSElement);
var
  i: Integer;
  Bin: TCSSBinaryElement;
begin
  if El=nil then exit;
  if El.ClassType=TCSSResolvedIdentifierElement then
    ResolveMediaIdentifier(TCSSResolvedIdentifierElement(El))
  else if El.ClassType=TCSSListElement then
    for i:=0 to TCSSListElement(El).ChildCount-1 do
      CheckMediaSelector(TCSSListElement(El).Children[i])
  else if El.ClassType=TCSSBinaryElement then
  begin
    Bin:=TCSSBinaryElement(El);
    CheckMediaSelector(Bin.Left);
    CheckMediaSelector(Bin.Right);
  end;
end;

function TCSSResolverParser.CreateElement(aClass: TCSSElementClass): TCSSElement;
begin
  Result:=inherited CreateElement(aClass);
  // give every rule and @-rule its data, so the resolver can cache per rule
  if (CSSRuleDataClass<>nil) and aClass.InheritsFrom(TCSSRuleElement) then
    Result.CustomData:=CSSRuleDataClass.Create;
end;

function TCSSResolverParser.ParseAtMediaRulePrelude: TCSSAtRuleElement;
// Resolve the media identifiers here, not in ParseAtMediaRule, because a @media
// nested in a style rule or in another at-rule is parsed via ParseAtNestedRule,
// which calls only the prelude.
var
  i: Integer;
begin
  Result:=inherited ParseAtMediaRulePrelude;
  if Result=nil then exit;

  for i:=0 to Result.SelectorCount-1 do
    CheckMediaSelector(Result.Selectors[i]);
end;

function TCSSResolverParser.ParseCall(aName: TCSSString; IsSelector: boolean
  ): TCSSCallElement;
var
  CallID: TCSSNumericalID;
begin
  Result:=inherited ParseCall(aName, IsSelector);
  if Result=nil then exit;

  if IsSelector then
  begin
    if Result.Name[1]=':' then
    begin
      CallID:=ResolvePseudoFunction(TCSSResolvedCallElement(Result));
      case CallID of
      CSSCallID_NthChild,CSSCallID_NthLastChild,
      CSSCallID_NthOfType,CSSCallID_NthLastOfType: CheckNthChildParams(TCSSResolvedCallElement(Result));
      end;
    end
    else
      Log(etWarning,20240701222744,'invalid selector function',Result);
  end;
end;

function TCSSResolverParser.ParseDeclaration(aIsAt: Boolean
  ): TCSSDeclarationElement;
var
  aKey: TCSSElement;
  AttrId: TCSSNumericalID;
  Desc: TCSSAttributeDesc;
  AttrData: TCSSAttributeKeyData;
  i, ChildCnt: Integer;
  ValueStr: TCSSString;
  AllowUnknown, HasVar: boolean;
begin
  Result:=inherited ParseDeclaration(aIsAt);
  if Result=nil then exit;

  if Result.KeyCount<>1 then
  begin
    if Result.KeyCount<1 then
      Log(etWarning,20231112135955,'missing keys in declaration',Result);
    if Result.KeyCount>1 then
      Log(etWarning,20231112140722,'too many keys in declaration',Result);
    exit;
  end;

  aKey:=Result.Keys[0];
  if aKey is TCSSResolvedIdentifierElement then
  begin
    AttrId:=ResolveAttribute(TCSSResolvedIdentifierElement(aKey));

    if aKey.CustomData<>nil then
      raise ECSSParser.Create('20240626113536');
    AttrData:=CSSAttributeKeyDataClass.Create;
    aKey.CustomData:=AttrData;

    ChildCnt:=Result.ChildCount;
    if ChildCnt=0 then
    begin
      AttrData.Invalid:=true;
      exit;
    end;

    ValueStr:='';
    for i:=0 to ChildCnt-1 do
    begin
      if (i>0) then
        ValueStr+=', ';
      ValueStr+=Result.Children[i].AsString;
    end;

    // tokenize the value once, for every known attribute (including 'all' and
    // custom properties). var() values are tokenized too, but cannot be
    // checked until the var() calls have been substituted.
    // AttrId<=CSSIDNone means an unknown attribute (already logged); skip it.
    if AttrId>CSSIDNone then
    begin
      Desc:=Resolver.GetAttributeDesc(AttrId);
      HasVar:=HasCSSValueVarCall(ValueStr);
      if HasVar or (AttrId>=Resolver.CSSRegistry.AttributeCount) or (Desc=nil) then
        AllowUnknown:=true // custom property --xxx, or not yet checkable
      else
        AllowUnknown:=Desc.AllowUnknownIdentifiers;
      if not Resolver.Tokenize(ValueStr,AttrData.Tokens,AllowUnknown) then
        AttrData.Invalid:=true
      else if (not HasVar) and (AttrId>=CSSAttributeID_All)
          and (AttrId<Resolver.CSSRegistry.AttributeCount) then
      begin
        if Resolver.InitParseAttr(Desc,AttrData) then
        begin
          if Assigned(Desc.OnCheck) and not Desc.OnCheck(Resolver) then
            AttrData.Invalid:=true;
        end;
      end;
      {$IFDEF VerboseCSSResolver}
      if AttrData.Invalid then
        Log(etWarning,20240710162400,'Invalid CSS attribute value: '+Result.AsString,aKey);
      {$ENDIF}
    end;
  end else begin
    Log(etWarning,20220908230855,'Expected property name, but found '+aKey.ClassName,aKey);
  end;
end;

function TCSSResolverParser.ParsePseudoElement: TCSSElement;
begin
  Result:=inherited ParsePseudoElement;
  if Result=nil then exit;

  if Result is TCSSResolvedIdentifierElement then
    ResolvePseudoElement(TCSSResolvedIdentifierElement(Result))
  else if Result is TCSSResolvedCallElement then
    ResolvePseudoElementFunction(TCSSResolvedCallElement(Result))
  else
    Log(etWarning,20250224210802,'Unknown CSS selector pseudo element',Result);
end;

function TCSSResolverParser.ParseSelector: TCSSElement;
begin
  Result:=inherited ParseSelector;
  if Result=nil then exit;

  CheckSelector(Result);
end;

procedure TCSSResolverParser.CheckSelector(El: TCSSElement);
var
  C: TClass;
begin
  C:=El.ClassType;
  if C=TCSSResolvedIdentifierElement then
    // e.g. div {}
    ResolveType(TCSSResolvedIdentifierElement(El))
  else if C=TCSSResolvedHashIdentifierElement then
    // e.g. #id {}
    ResolveHashIdentifier(TCSSResolvedHashIdentifierElement(El))
  else if C=TCSSResolvedClassNameElement then
    // e.g. .classname {}
    ResolveClassName(TCSSResolvedClassNameElement(El))
  else if C=TCSSResolvedPseudoClassElement then
    // e.g. :pseudoclass {}
    ResolvePseudoClass(TCSSResolvedPseudoClassElement(El))
  else if C=TCSSUnaryElement then
    CheckSelectorUnary(TCSSUnaryElement(El))
  else if C=TCSSBinaryElement then
    CheckSelectorBinary(TCSSBinaryElement(El))
  else if C=TCSSArrayElement then
    CheckSelectorArray(TCSSArrayElement(El))
  else if C=TCSSListElement then
    CheckSelectorList(TCSSListElement(El))
  else if C=TCSSResolvedCallElement then
    // checked via ParseCall
  else
    Log(etWarning,20240625131810,'Unknown CSS selector element',El);
end;

procedure TCSSResolverParser.CheckSelectorArray(anArray: TCSSArrayElement);
var
  {$IFDEF VerboseCSSResolver}
  i: integer;
  {$ENDIF}
  El: TCSSElement;
  C: TClass;
  aValue: TCSSString;
begin
  if anArray.Prefix<>nil then
  begin
    Log(etWarning,20240625134737,'Invalid CSS attribute selector prefix',anArray.Prefix);
    exit;
  end;

  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolverParser.CheckSelectorArray Prefix=',GetCSSObj(anArray.Prefix),' ChildCount=',anArray.ChildCount);
  for i:=0 to anArray.ChildCount-1 do
    writeln('TCSSResolverParser.CheckSelectorArray ',i,' ',GetCSSObj(anArray.Children[i]));
  {$ENDIF}
  if anArray.ChildCount<1 then
  begin
    Log(etWarning,20220910154033,'Invalid CSS attribute selector',anArray);
    exit;
  end;

  if anArray.ChildCount>1 then
  begin
    El:=anArray.Children[1];
    C:=El.ClassType;
    if C=TCSSResolvedIdentifierElement then
    begin
      aValue:=TCSSResolvedIdentifierElement(El).Value;
      case aValue of
      'i','s': ;
      else
        Log(etWarning,20240625134931,'Invalid attribute modifier "'+aValue+'"',El);
        exit;
      end;
    end else begin
      Log(etWarning,20240625134940,'Invalid CSS attribute modifier',El);
      exit;
    end;
  end;
  if (anArray.ChildCount>2) then
    Log(etWarning,20240625134951,'Invalid CSS attribute modifier',anArray.Children[2]);

  El:=anArray.Children[0];
  C:=El.ClassType;
  if C=TCSSResolvedIdentifierElement then
  begin
    // [name]  ->  has explicit attribute
    ResolveAttribute(TCSSResolvedIdentifierElement(El));
  end else if C=TCSSBinaryElement then
    CheckSelectorArrayBinary(TCSSBinaryElement(El))
  else begin
    Log(etWarning,20240625135119,'Invalid CSS array selector',El);
  end;
end;

procedure TCSSResolverParser.CheckSelectorArrayBinary(aBinary: TCSSBinaryElement
  );
var
  Left, Right: TCSSElement;
  C: TClass;
begin
  Left:=aBinary.Left;
  if Left.ClassType<>TCSSResolvedIdentifierElement then
  begin
    Log(etWarning,20240625154314,'Invalid CSS array selector, expected attribute',Left);
    exit;
  end;
  ResolveAttribute(TCSSResolvedIdentifierElement(Left));

  Right:=aBinary.Right;
  C:=Right.ClassType;
  if (C=TCSSStringElement) or (C=TCSSIntegerElement) or (C=TCSSFloatElement)
      or (C=TCSSResolvedIdentifierElement) then
    // ok
  else begin
    Log(etWarning,20240625154455,'Invalid CSS array selector, expected string',Right);
    exit;
  end;
  ComputeValue(Right);

  case aBinary.Operation of
  boEquals,
  boSquaredEqual,
  boDollarEqual,
  boPipeEqual,
  boStarEqual,
  boTildeEqual: ;
  else
    Log(etWarning,20240625154617,'Invalid CSS array selector operator',aBinary);
  end;
end;

procedure TCSSResolverParser.CheckSelectorUnary(aUnary: TCSSUnaryElement);
begin
  case aUnary.Operation of
  uoDoubleColon:
    ; // right side was done in ParsePseudoElement
  uoGT,
  uoPlus,
  uoTilde:
    // nested rule combinator with implicit &, e.g. "> .class" -> resolve operand
    CheckSelector(aUnary.Right);
  else
    Log(etWarning,20250225103443,'Invalid CSS unary selector '+UnaryOperators[aUnary.Operation],aUnary);
  end;
end;

procedure TCSSResolverParser.CheckSelectorBinary(aBinary: TCSSBinaryElement);
begin
  case aBinary.Operation of
  boGT,
  boPlus,
  boTilde,
  boWhiteSpace:
    ;
  boDoubleColon:
    begin
    CheckSelector(aBinary.Left);
    // right side was done in ParsePseudoElement
    exit;
    end
  else
    Log(etWarning,20240625153307,'Invalid CSS binary selector '+BinaryOperators[aBinary.Operation],aBinary);
  end;

  CheckSelector(aBinary.Left);
  CheckSelector(aBinary.Right);
end;

procedure TCSSResolverParser.CheckSelectorList(aList: TCSSListElement);
var
  i: Integer;
  El: TCSSElement;
begin
  for i:=0 to aList.ChildCount-1 do
  begin
    El:=aList.Children[i];
    {$IFDEF VerboseCSSResolver}
    writeln('TCSSResolverParser.CheckSelectorList ',i,' ',GetCSSObj(El),' AsString=',El.AsString);
    {$ENDIF}
    CheckSelector(El);
  end;
end;

procedure TCSSResolverParser.CheckNthChildParams(aCall: TCSSResolvedCallElement);

  procedure NthWarn(const ID: TCSSMsgID; const Msg: string; PosEl: TCSSElement);
  begin
    Log(etWarning,ID,CSSSelectorCallNames[aCall.NameNumericalID]+' '+Msg,PosEl);
  end;

var
  i, ArgCount, aModulo, aStart: Integer;
  Arg, OffsetEl: TCSSElement;
  Str: TCSSString;
  UnaryEl, anUnary: TCSSUnaryElement;
  Params: TCSSNthChildParams;
begin
  if aCall.Params<>nil then
    raise ECSSParser.Create('20240625150639');
  ArgCount:=aCall.ArgCount;
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolverParser.CheckSelectorCall_NthChild ArgCount=',aCall.ArgCount);
  for i:=0 to aCall.ArgCount-1 do
    writeln('TCSSResolverParser.CheckSelectorCall_NthChild Arg[',i,'] ',GetCSSObj(aCall.Args[i]),' AsString=',aCall.Args[i].AsString);
  {$ENDIF}

  // An, An+B, An+B of S, odd, even
  i:=0;
  aModulo:=0;
  aStart:=1;
  // check step
  if ArgCount<=i then
  begin
    NthWarn(20220915143843,'missing arguments',aCall);
    exit;
  end;
  Arg:=aCall.Args[i];
  if Arg.ClassType=TCSSIntegerElement then
  begin
    if (i+1<ArgCount)
        and (aCall.Args[i+1].ClassType=TCSSResolvedIdentifierElement)
        and (TCSSResolvedIdentifierElement(aCall.Args[i+1]).Value='n') then
    begin
      // An, An+B: integer followed by 'n'
      aModulo:=TCSSIntegerElement(Arg).Value;
      inc(i); // i now points at 'n'
    end else
    begin
      // plain integer B (a=0), e.g. nth-child(2)
      aStart:=TCSSIntegerElement(Arg).Value;
      aModulo:=0;
    end;
  end
  else if Arg.ClassType=TCSSResolvedIdentifierElement then
  begin
    Str:=TCSSResolvedIdentifierElement(Arg).Value;
    case lowercase(Str) of
    'even':
      begin
      aModulo:=2;
      aStart:=2;
      end;
    'odd':
      begin
      aModulo:=2;
      end;
    'n':
      begin
      aModulo:=1;
      end;
    else
      NthWarn(20220915150332,'expected multiplier',Arg);
      exit;
    end
  end else if Arg.ClassType=TCSSUnaryElement then
  begin
    anUnary:=TCSSUnaryElement(Arg);
    case anUnary.Operation of
    uoMinus: aModulo:=-1;
    uoPlus: aModulo:=1;
    else
      NthWarn(20220917080309,'expected multiplier',Arg);
      exit;
    end;
    if (anUnary.Right.ClassType=TCSSResolvedIdentifierElement)
        and (SameText(TCSSResolvedIdentifierElement(anUnary.Right).Value,'n')) then
    begin
      // ok
    end else begin
      NthWarn(20220917080154,'expected multiplier',Arg);
      exit;
    end;
  end else
  begin
    NthWarn(20220915144056,'expected multiplier',Arg);
    exit;
  end;

  inc(i);
  if ArgCount>i then
  begin
    Arg:=aCall.Args[i];
    if Arg.ClassType=TCSSUnaryElement then
    begin
      UnaryEl:=TCSSUnaryElement(Arg);
      if not (UnaryEl.Operation in [uoMinus,uoPlus]) then
      begin
        NthWarn(20220915151422,'unexpected offset',UnaryEl);
        exit;
      end;
      OffsetEl:=UnaryEl.Right;
      if OffsetEl=nil then
      begin
        NthWarn(20220915151511,'unexpected offset',UnaryEl);
        exit;
      end;
      if OffsetEl.ClassType<>TCSSIntegerElement then
      begin
        NthWarn(20220915151718,'unexpected offset',OffsetEl);
        exit;
      end;
      aStart:=TCSSIntegerElement(OffsetEl).Value;
      if UnaryEl.Operation=uoMinus then
        aStart:=-aStart;
    end
    else if (Arg.ClassType=TCSSResolvedIdentifierElement)
        and SameText(TCSSResolvedIdentifierElement(Arg).Value,'of') then
    begin
      // no offset, directly the 'of <selector>' part; let the of-handler below process it
      dec(i);
    end else
    begin
      NthWarn(20220915150851,'expected offset',Arg);
      exit;
    end;
  end;

  Params:=CSSNthChildParamsClass.Create;
  aCall.Params:=Params;
  Params.Modulo:=aModulo;
  Params.Start:=aStart;

  inc(i);
  if (i<ArgCount) then
  begin
    Arg:=aCall.Args[i];
    if (Arg.ClassType=TCSSResolvedIdentifierElement)
        and (SameText(TCSSResolvedIdentifierElement(Arg).Value,'of')) then
    begin
      // An+B of Selector
      inc(i);
      if i=ArgCount then
      begin
        NthWarn(20240711154813,'expected selector',Arg);
        exit;
      end;
      Arg:=aCall.Args[i];
      Params.HasOf:=true;
      Params.OfSelector:=Arg;
    end;
  end;

  if (aCall.NameNumericalID in [CSSCallID_NthOfType,CSSCallID_NthLastOfType]) then
    Params.HasOf:=true;
end;

function TCSSResolverParser.ComputeValue(El: TCSSElement): TCSSString;
var
  ElData: TObject;
  C: TClass;
  StrEl: TCSSStringElement;
  IntEl: TCSSIntegerElement;
  FloatEl: TCSSFloatElement;
begin
  C:=El.ClassType;
  if C=TCSSResolvedIdentifierElement then
    Result:=TCSSResolvedIdentifierElement(El).Value
  else if (C=TCSSStringElement)
      or (C=TCSSIntegerElement)
      or (C=TCSSFloatElement) then
  begin
    ElData:=El.CustomData;
    if ElData is TCSSValueData then
      exit(TCSSValueData(ElData).NormValue);
    if C=TCSSStringElement then
    begin
      StrEl:=TCSSStringElement(El);
      Result:=StrEl.Value;
    end
    else if C=TCSSIntegerElement then
    begin
      IntEl:=TCSSIntegerElement(El);
      Result:=IntEl.AsString;
    end else if C=TCSSFloatElement then
    begin
      FloatEl:=TCSSFloatElement(El);
      Result:=FloatEl.AsString;
    end;
    if El.CustomData<>nil then
      raise ECSSParser.Create('20260416220050');
    ElData:=TCSSValueData.Create;
    TCSSValueData(ElData).NormValue:=Result;
    El.CustomData:=ElData;
  end else begin
    Log(etWarning,20240625162632,'TCSSResolverParser.ComputeValue not supported',El);
  end;
end;

constructor TCSSResolverParser.Create(AScanner: TCSSScanner);
begin
  inherited Create(AScanner);
  CSSIdentifierElementClass:=TCSSResolvedIdentifierElement;
  CSSPseudoClassElementClass:=TCSSResolvedPseudoClassElement;
  CSSClassNameElementClass:=TCSSResolvedClassNameElement;
  CSSHashIdentifierElementClass:=TCSSResolvedHashIdentifierElement;
  CSSCallElementClass:=TCSSResolvedCallElement;
  CSSNthChildParamsClass:=TCSSNthChildParams;
  CSSAttributeKeyDataClass:=TCSSAttributeKeyData;
  CSSRuleDataClass:=TCSSRuleParserData;
end;

destructor TCSSResolverParser.Destroy;
begin
  inherited Destroy;
end;

procedure TCSSResolverParser.Log(MsgType: TEventType; const ID: TCSSMsgID;
  const Msg: TCSSString; PosEl: TCSSElement);
begin
  if Assigned(OnLog) then
    OnLog(MsgType,ID,Msg,PosEl);
end;

class function TCSSResolverParser.IsWhiteSpace(const s: TCSSString): boolean;
var
  i: Integer;
begin
  for i:=1 to length(s) do
    if not (s[i] in [' ',#10,#13]) then
      exit(false);
  Result:=true;
end;

{ TCSSResCompParser }

procedure TCSSResCompParser.SetCSSRegistry(const AValue: TCSSRegistry);
begin
  if FCSSRegistry=AValue then Exit;
  FCSSRegistry:=AValue;
end;

procedure TCSSResCompParser.InitParseAttr(const Value: TCSSString);
begin
  CurValue:=Value;
  CurComp:=Default(TCSSResCompValue);
  CurComp.EndP:=PCSSChar(CurValue);
  ReadNext;
end;

function TCSSResCompParser.CheckAttribute_Keyword(const AllowedKeywordIDs: TCSSNumericalIDArray
  ): boolean;
var
  i: Integer;
begin
  if CurComp.Kind<>rvkKeyword then exit(false);
  for i:=0 to length(AllowedKeywordIDs)-1 do
    if CurComp.KeywordID=AllowedKeywordIDs[i] then
    begin
      if not ReadNext and (CurComp.Kind=rvkNone) then
        exit(true);
    end;
  Result:=false;
end;

function TCSSResCompParser.CheckAttribute_Keyword_List(const AllowedKeywordIDs: TCSSNumericalIDArray
  ): boolean;
var
  i: Integer;
  Fits: Boolean;
begin
  repeat
    Fits:=false;
    case CurComp.Kind of
    rvkKeyword:
      for i:=0 to length(AllowedKeywordIDs)-1 do
        if CurComp.KeywordID=AllowedKeywordIDs[i] then
        begin
          Fits:=true;
          break;
        end;
    end;
    if not Fits then
      exit(false);
  until not ReadNext;
  Result:=CurComp.Kind=rvkNone;
end;

function TCSSResCompParser.CheckAttribute_Dimension(const Params: TCSSCheckAttrParams_Dimension
  ): boolean;
var
  i: Integer;
begin
  case CurComp.Kind of
  rvkFloat:
    if Params.Fits(CurComp) then
      exit(true);
  rvkKeyword:
    for i:=0 to length(Params.AllowedKeywordIDs)-1 do
      if CurComp.KeywordID=Params.AllowedKeywordIDs[i] then
        exit(true);
  end;
  Result:=false;
end;

function TCSSResCompParser.CheckAttribute_Color(const AllowedKeywordIDs: TCSSNumericalIDArray
  ): boolean;
var
  i: Integer;
begin
  case CurComp.Kind of
  rvkKeyword:
    begin
      if CSSRegistry.IsColorKeyword(CurComp.KeywordID,false) then
        exit(true);
      for i:=0 to length(AllowedKeywordIDs)-1 do
        if CurComp.KeywordID=AllowedKeywordIDs[i] then
          exit(true);
    end;
  rvkFunction:
    // the whole rgb(...) is one component, its arguments are not checked here
    if CSSRegistry.IsColorFunction(CurComp.FunctionID) then
      exit(true);
  rvkHexColor:
    exit(true);
  end;
  Result:=false;
end;

function TCSSResCompParser.ReadNext: boolean;
begin
  Result:=ReadComp(CurComp);
end;

function TCSSResCompParser.IsBaseKeyword(KeywordID: TCSSNumericalID): boolean;
begin
  Result:=(KeywordID>=CSSKeywordInitial) and (KeywordID<=CSSKeywordRevertLayer);
end;

function TCSSResCompParser.IsKeywordIn(aKeywordID: TCSSNumericalID;
  const KeywordIDs: TCSSNumericalIDArray): boolean;
var
  i: Integer;
begin
  for i:=0 to length(KeywordIDs)-1 do
    if KeywordIDs[i]=aKeywordID then
      exit(true);
  Result:=false;
end;

function TCSSResCompParser.IsKeywordIn(const KeywordIDs: TCSSNumericalIDArray): boolean;
var
  aKeywordID: TCSSNumericalID;
  i: Integer;
begin
  Result:=false;
  if CurComp.Kind<>rvkKeyword then exit;
  aKeywordID:=CurComp.KeywordID;
  for i:=0 to length(KeywordIDs)-1 do
    if KeywordIDs[i]=aKeywordID then
      exit(true);
end;

function TCSSResCompParser.IsLengthOrPercentage(AllowNegative: boolean): boolean;
begin
  Result:=false;
  case CurComp.Kind of
  rvkFloat:
    if CurComp.FloatUnit in cuAllLengthsAndPercent then
    begin
      if (not AllowNegative) and (CurComp.Float<0) then exit;
      exit(true);
    end
    else if (CurComp.FloatUnit=cuNone) and (CurComp.Float=0) then
      exit(true); // 0 without unit is allowed
  end;
end;

function TCSSResCompParser.IsLengthOrPercentage(const ResValue: TCSSResCompValue;
  AllowNegative: boolean): boolean;
begin
  Result:=false;
  case ResValue.Kind of
  rvkFloat:
    if ResValue.FloatUnit in cuAllLengthsAndPercent then
    begin
      if (not AllowNegative) and (ResValue.Float<0) then exit;
      exit(true);
    end
    else if (ResValue.FloatUnit=cuNone) and (ResValue.Float=0) then
      exit(true);
  end;
end;

function TCSSResCompParser.IsSymbol(Token: TCSSToken): boolean;
begin
  Result:=(CurComp.Kind=rvkSymbol) and (CurComp.Symbol=Token);
end;

function TCSSResCompParser.GetCompString: TCSSString;
var
  StartP: PCSSChar;
begin
  if CurComp.Kind=rvkKeyword then
    exit(CSSRegistry.Keywords[CurComp.KeywordID]);
  StartP:=CurComp.StartP;
  if (StartP=PCSSChar(CurValue)) and (CurComp.EndP-StartP = length(CurValue)) then
    Result:=CurValue
  else
    SetString(Result,StartP,CurComp.EndP-StartP);
end;

function TCSSResCompParser.GetCompString(const aValue: string; const ResValue: TCSSResCompValue
  ): TCSSString;
var
  Start: PCSSChar;
begin
  if ResValue.Kind=rvkKeyword then
    exit(CSSRegistry.Keywords[ResValue.KeywordID]);
  Start:=ResValue.StartP;
  if (Start=PCSSChar(aValue)) and (ResValue.EndP-Start = length(aValue)) then
    Result:=aValue
  else
    SetString(Result,Start,ResValue.EndP-Start);
end;

function TCSSResCompParser.ReadComp(var aComp: TCSSResCompValue): boolean;
begin
  Result:=ReadValue(aComp);
  ReadWordID(aComp);
end;

procedure TCSSResCompParser.ReadWordID(var aComp: TCSSResCompValue);
var
  Identifier: TCSSString;
  p: PCSSChar;
begin
  case aComp.Kind of
  rvkFunctionUnknown:
    begin
      p:=aComp.StartP;
      if not (p^ in AlIden) then exit;
      repeat
        inc(p);
      until not (p^ in AlNumIden);
      SetString(Identifier,aComp.StartP,p-aComp.StartP);
      aComp.FunctionID:=CSSRegistry.IndexOfAttrFunction(Identifier);
      if aComp.FunctionID>CSSIDNone then
        aComp.Kind:=rvkFunction;
    end;
  rvkKeywordUnknown:
    begin
      SetString(Identifier,aComp.StartP,aComp.EndP-aComp.StartP);
      aComp.KeywordID:=CSSRegistry.IndexOfKeyword(Identifier);
      if aComp.KeywordID>CSSIDNone then
        aComp.Kind:=rvkKeyword;
    end;
  end;
end;

class function TCSSResCompParser.ReadValue(var aComp: TCSSResCompValue): boolean;
var
  c: TCSSChar;
  p: PCSSChar;
  l: SizeInt;

  procedure SetSymbol(s: TCSSToken);
  begin
    aComp.Kind:=rvkSymbol;
    aComp.Symbol:=s;
    aComp.EndP:=p+1;
  end;

begin
  Result:=true;
  aComp.Kind:=rvkNone;

  p:=aComp.EndP;
  if p=nil then exit(false);

  // skip whitespace
  while (p^ in Whitespace) do inc(p);
  aComp.StartP:=p;
  aComp.EndP:=p;

  c:=p^;
  case c of
  #0: exit(false);
  '0'..'9':
    if ReadNumber(aComp) then exit;
  ',':
    begin
      SetSymbol(ctkCOMMA);
      exit;
    end;
  ')':
    begin
      SetSymbol(ctkRPARENTHESIS);
      exit;
    end;
  '+':
    case p[1] of
    '0'..'9','.':
      if ReadNumber(aComp) then exit;
    #0,#9,#10,#13,' ':
      begin
        SetSymbol(ctkPLUS);
        exit;
      end;
    end;
  '-':
    case p[1] of
    '0'..'9','.':
      if ReadNumber(aComp) then exit;
    'a'..'z','A'..'Z','-':
      if ReadIdentifier(aComp) then exit;
    #0,#9,#10,#13,' ':
      begin
        SetSymbol(ctkMINUS);
        exit;
      end;
    end;
  '.':
    case p[1] of
    '0'..'9':
      if ReadNumber(aComp) then exit;
    else
      SetSymbol(ctkDOT);
      exit;
    end;
  '*':
    begin
      if p[1]='=' then
      begin
        inc(p);
        SetSymbol(ctkSTAREQUAL);
      end else
        SetSymbol(ctkSTAR);
      exit;
    end;
  '/':
    begin
      SetSymbol(ctkDIV);
      exit;
    end;
  ':':
    begin
      SetSymbol(ctkCOLON);
      exit;
    end;
  ';':
    begin
      SetSymbol(ctkSEMICOLON);
      exit;
    end;
  'a'..'z','A'..'Z':
    if ReadIdentifier(aComp) then exit;
  '#':
    begin
      inc(p);
      while p^ in Hex do inc(p);
      l:=p-aComp.StartP;
      case l of
      4,5,7,9:
        begin
          // #rgb, #rgba, #rrggbb, #rrggbbaa
          aComp.Kind:=rvkHexColor;
          aComp.EndP:=p;
          exit;
        end;
      end;
    end;
  '''','"':
    if SkipString(p) then
    begin
      aComp.Kind:=rvkString;
      aComp.EndP:=p;
      exit;
    end;
  '[':
    if SkipBrackets(p) then
    begin
      aComp.Kind:=rvkBrackets;
      aComp.EndP:=p;
      exit;
    end;
  '(':
    if SkipBrackets(p) then
    begin
      aComp.Kind:=rvkParenthesis;
      aComp.EndP:=p;
      exit;
    end;
  end;

  // skip unknown aComp
  aComp.Kind:=rvkInvalid;
  repeat
    if p^ in ValEnd then break;
    case p^ of
    '(','[': SkipBrackets(p);
    '''','"': SkipString(p);
    else inc(p);
    end;
  until false;
  aComp.EndP:=p;
  Result:=false;
end;

class function TCSSResCompParser.ReadNumber(var aComp: TCSSResCompValue): boolean;
var
  Negative, HasNumber: Boolean;
  Divisor: double;
  Exponent: Integer;
  d: Float;
  U: TCSSUnit;
  StartP, p: PCSSChar;
begin
  Result:=false;
  aComp.Kind:=rvkInvalid;
  p:=aComp.StartP;

  // number: 1, 0.2, .3, 4.01, 0.0, +0.0, -0.0, .50, 2e3, -6.7E-2
  if p^='-' then
  begin
    Negative:=true;
    inc(p);
  end else begin
    Negative:=false;
    if p^='+' then
      inc(p);
  end;
  HasNumber:=false;
  aComp.Float:=0;
  if p^ in Num then
  begin
    // read significand
    HasNumber:=true;
    repeat
      aComp.Float:=aComp.Float*10+ord(p^)-ord('0');
      if aComp.Float>CSSMaxSafeIntDouble then
        exit; // loosing precision
      inc(p);
    until not (p^ in Num);
  end;
  if p^='.' then
  begin
    // read fraction
    inc(p);
    if not (p^ in Num) then exit;
    Divisor:=1;
    repeat
      Divisor:=Divisor*10;
      aComp.Float:=aComp.Float*10+ord(p^)-ord('0');
      if (Divisor>CSSMaxSafeIntDouble)
          or (aComp.Float>CSSMaxSafeIntDouble) then
        exit; // loosing precision
      inc(p);
    until not (p^ in Num);
    aComp.Float:=aComp.Float/Divisor;
  end else if not HasNumber then
    exit;
  if Negative then
    aComp.Float:=-aComp.Float;

  if (p^ in ['e','E']) and not (p[1] in ['a'..'z']) then
  begin
    inc(p);
    if p^='-' then
    begin
      Negative:=true;
      inc(p);
    end else begin
      Negative:=false;
      if p^='+' then
        inc(p);
    end;
    if not (p^ in Num) then exit;
    Exponent:=0;
    repeat
      inc(p);
      Exponent:=Exponent*10+ord(p^)-ord('0');
      if Exponent>2047 then
        exit; // out of bounds
    until not (p^ in Num);
    if Exponent>0 then
    begin
      if Negative then
        Exponent:=-Exponent;
      try
        d:=Power(10,Exponent);
        aComp.Float:=aComp.Float*d;
      except
        exit;
      end;
    end;
  end;
  aComp.Kind:=rvkFloat;

  // parse unit
  U:=cuNone;
  case p^ of
  '%':
    begin
      inc(p);
      U:=cuPercent;
    end;
  'a'..'z','A'..'Z':
    begin
      StartP:=p;
      while p^ in Alpha do inc(p);
      // match the whole unit name, not just a prefix, otherwise the short names
      // shadow the long ones starting with them, e.g. "s" would eat "svw"
      U:=high(TCSSUnit);
      while (U>cuNone)
          and ((length(CSSUnitNames[U])<>p-StartP)
            or not CompareMem(StartP,PCSSChar(CSSUnitNames[U]),(p-StartP)*SizeOf(TCSSChar))) do
        U:=pred(U);
      if U=cuNone then
        exit; // unknown unit
    end;
  end;
  aComp.FloatUnit:=U;
  aComp.EndP:=p;

  Result:=true;
end;

class function TCSSResCompParser.ReadIdentifier(var aComp: TCSSResCompValue): boolean;
var
  IsFunc: Boolean;
  p: PCSSChar;
begin
  Result:=false;
  aComp.Kind:=rvkInvalid;
  p:=aComp.EndP;
  if not (p^ in AlIden) then exit;
  repeat
    inc(p);
  until not (p^ in AlNumIden);
  IsFunc:=p^='(';
  if IsFunc then
  begin
    // function call
    aComp.Kind:=rvkFunctionUnknown;
    aComp.BracketOpen:=p;
    if not SkipBrackets(p) then
    begin
      aComp.EndP:=p;
      exit;
    end;
  end else
    aComp.Kind:=rvkKeywordUnknown;
  aComp.EndP:=p;

  Result:=true;
end;

class procedure TCSSResCompParser.SkipToEndOfAttribute(var p: PCSSChar);
begin
  repeat
    case p^ of
    #0,'{','}',';': exit;
    '''','"': SkipString(p);
    else inc(p);
    end;
  until false;
end;

class function TCSSResCompParser.SkipString(var p: PCSSChar): boolean;
var
  Delim, c: TCSSChar;
begin
  Result:=false;
  Delim:=p^;
  repeat
    inc(p);
    c:=p^;
    if c=Delim then
    begin
      inc(p);
      exit(true);
    end else if c=#0 then
      exit;
  until false;
end;

class function TCSSResCompParser.SkipBrackets(var p: PCSSChar; Lvl: integer): boolean;
const
  CSSMaxBracketLvl = 10;
var
  CloseBracket: TCSSChar;
begin
  Result:=false;
  if Lvl>CSSMaxBracketLvl then
  begin
    SkipToEndOfAttribute(p);
    exit;
  end;

  if p^='[' then
    CloseBracket:=']'
  else
    CloseBracket:=')';
  repeat
    inc(p);
    case p^ of
    #0,'{','}',';': exit;
    '''','"': SkipString(p);
    '(','[': SkipBrackets(p,Lvl+1);
    ')',']':
      if p^=CloseBracket then
      begin
        inc(p);
        exit(true);
      end else begin
        SkipToEndOfAttribute(p);
        exit;
      end;
    end;
  until false;
end;

end.

