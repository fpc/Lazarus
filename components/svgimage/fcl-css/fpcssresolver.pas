{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2022 by Michael Van Canneyt (michael@freepascal.org)

    This file contains CSS utility class

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

Works:

Selector 	Example 	Example description
.class  	.intro  	Selects all elements with class="intro"
.class1.class2 	.name1.name2 	Selects all elements with both name1 and name2 set within its class attribute
.class1 .class2 	.name1 .name2 	Selects all elements with name2 that is a descendant of an element with name1
#id 	#firstname 	Selects the element with name="firstname"
* 	* 	Selects all elements
type 	p 	Selects all <p> elements
type.class 	p.intro 	Selects all <p> elements with class="intro"
type,type 	div, p 	Selects all <div> elements and all <p> elements
type type 	div p 	Selects all <p> elements inside <div> elements
type>type 	div > p 	Selects all <p> elements where the parent is a <div> element
type+type 	div + p 	Selects the first <p> element that is placed immediately after a <div> element
element1~element2 	p ~ ul 	Selects every <ul> element that is preceded by a <p> element
[attribute] 	[target] 	Selects all elements with a target attribute
[attribute=value] 	[target=_blank] 	Selects all elements with target="_blank"
[attribute~=value] 	[title~=flower] 	Selects all elements with a title attribute containing the *word* "flower"
[attribute|=value] 	[lang|=en] 	Selects all elements with a lang attribute value equal to "en" or starting with "en-" (hyphen)
[attribute^=value] 	a[href^="https"] 	Selects every <a> element whose href attribute value begins with "https"
[attribute$=value] 	a[href$=".pdf"] 	Selects every <a> element whose href attribute value ends with ".pdf"
[attribute*=value] 	a[href*="w3schools"] 	Selects every <a> element whose href attribute value contains the substring "w3schools"
:root 	:root 	Selects the document's root element
:empty 	p:empty 	Selects every <p> element that has no children (including text nodes)
:first-child 	p:first-child 	Selects every <p> element that is the first child of its parent
:first-of-type 	p:first-of-type 	Selects every <p> element that is the first <p> element of its parent
:last-child 	p:last-child 	Selects every <p> element that is the last child of its parent
:last-of-type 	p:last-of-type 	Selects every <p> element that is the last <p> element of its parent
:not(selector) 	:not(p) 	Selects every element that is not a <p> element
:nth-child(n) 	p:nth-child(2) 	Selects every <p> element that is the second child of its parent. n can be a number, a keyword (odd or even), or a formula (like an + b).
:nth-last-child(n) 	p:nth-last-child(2) 	Selects every <p> element that is the second child of its parent, counting from the last child
:nth-last-of-type(n) 	p:nth-last-of-type(2) 	Selects every <p> element that is the second <p> element of its parent, counting from the last child
:nth-of-type(n) 	p:nth-of-type(2) 	Selects every <p> element that is the second <p> element of its parent
:only-of-type 	p:only-of-type 	Selects every <p> element that is the only <p> element of its parent
:only-child 	p:only-child 	Selects every <p> element that is the only child of its parent
:is()
:where()

Specificity:
important: 10000
inline: 1000
id: 100 #menu
class+attribute selectors: 10 .button, :hover, [href]
element/type: 1 p, :before
*: 0

ToDo:
- 'all' attribute: resets all properties, except direction, unicode-bidi and custom css properties
- :has()
- namespaces
- layers
- --varname, var(), inherits
- counter-reset
- counter-increment
- @rules:-----------------------------------------------------------------------
  - @media
  - @font-face
  - @property
- Functions and Vars:-----------------------------------------------------------
  - attr() 	Returns the value of an attribute of the selected element
            attr(title)
            attr(src url)
            attr(data-width px, inherit);
  - calc() 	Allows you to perform calculations to determine CSS property values  calc(100% - 100px)
  - max() min() minmax(minvalue,maxvalue)   min(50%, 50px)
             keyword values max-content, min-content, or auto
  - clamp(minvalue,preferred,maxvalue) = max(MIN, min(VAL, MAX))
  - var()   usable in property values, query custom css properties, inherits
            var(--name), var(--name, --default-name), var(--name, var(--foo, #FF0000))
- Pseudo-elements - not case sensitive:-----------------------------------------
  - ::first-letter 	p::first-letter 	Selects the first letter of every <p> element
  - ::first-line 	p::first-line 	Selects the first line of every <p> element
  - ::selection 	::selection 	Selects the portion of an element that is selected by a user
- Altering:---------------------------------------------------------------------
  - ::after 	p::after 	Insert something after the content of each <p> element
  - ::before 	p::before 	Insert something before the content of each <p> element
- grid-structural-selectors:----------------------------------------------------
  - columns combinator ||     col.selected || td
  - :nth-col()
  - :nth-last-col()

}

{$IFNDEF FPC_DOTTEDUNITS}
unit fpCSSResolver;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}
{$Interfaces CORBA}
{$ModeSwitch AdvancedRecords}

{$IF FPC_FULLVERSION>30300}
{$WARN 6060 off} // Case statement does not handle all possible cases
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Types, System.Math, System.Contnrs, System.StrUtils,
  Fcl.AVLTree, FpCss.Tree, FpCss.Scanner, FpCss.Parser, FpCss.ValueParser;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, Types, Math, Contnrs, AVL_Tree, StrUtils, fpCSSTree, fpCSSScanner,
  fpCSSParser, fpCSSResParser;
{$ENDIF FPC_DOTTEDUNITS}

const
  CSSSpecificityInvalid = -2;
  CSSSpecificityNoMatch = -1;
  CSSSpecificityUniversal = 0;
  CSSSpecificityType = 1;
  CSSSpecificityPseudoElement = CSSSpecificityType; // a pseudo element e.g. ::first-line counts as a type selector
  CSSSpecificityClass = 10; // includes attribute selectors e.g. [href]
  CSSSpecificityIdentifier = 100;
  CSSSpecificityUserAgent = 1000;
  CSSSpecificityUser = 2000;
  CSSSpecificityAuthor = 3000;
  CSSSpecificityInline = 5000;
  CSSSpecificityElement = 10000;
  CSSSpecificityImportant = 100000;

type
  TCSSSpecificity = integer; // see CSSSpecificityInvalid..CSSSpecificityImportant

  TCSSOrigin = (
    cssoUserAgent, // e.g. browser
    cssoUser,      // e.g. user settings
    cssoAuthor,    // e.g. stylesheet(s) of a site
    cssoInline     // e.g. inspector
    );
const
  CSSOriginToSpecifity: array[TCSSOrigin] of TCSSNumericalID = (
    CSSSpecificityUserAgent,
    CSSSpecificityUser,
    CSSSpecificityAuthor,
    CSSSpecificityInline
    );

type

  { ECSSResolver }

  ECSSResolver = class(ECSSException)
  end;

  TCSSAttributeMatchKind = (
    camkEqual,
    camkContains,
    camkContainsWord,
    camkBegins,
    camkEnds
    );
  TCSSAttributeMatchKinds = set of TCSSAttributeMatchKind;

  { ICSSNode }

  ICSSNode = interface
    function GetCSSID: TCSSNumericalID; // resolver id index of this node's 'id' (see GetCSSIDIndex), CSSIDNone if none/unknown
    function GetCSSTypeID: TCSSNumericalID;
    function GetCSSPseudoElementID: TCSSNumericalID;
    // parent
    function GetCSSParent: ICSSNode;
    function GetCSSDepth: integer;
    function GetCSSIndex: integer; // node index in parent's children
    // siblings
    function GetCSSNextSibling: ICSSNode;
    function GetCSSPreviousSibling: ICSSNode;
    function GetCSSNextOfType: ICSSNode;
    function GetCSSPreviousOfType: ICSSNode;
    // children
    function GetCSSEmpty: boolean;
    function GetCSSChildCount: integer;
    function GetCSSChild(const anIndex: integer): ICSSNode;
    // attributes
    function HasCSSClass(const aClassID: TCSSNumericalID): boolean;
    function GetCSSClasses: TCSSNumericalIDArray; // all class ids of this node
    function GetCSSAttributeClass: TCSSString; // get the 'class' attribute
    function GetCSSAttributeID: TCSSString; // get the 'id' attribute
    function GetCSSCustomAttribute(const AttrID: TCSSNumericalID): TBytes; // tokenized computed value
    function HasCSSExplicitAttribute(const AttrID: TCSSNumericalID): boolean; // e.g. if the HTML has the attribute
    function GetCSSExplicitAttribute(const AttrID: TCSSNumericalID): TCSSString;
    function HasCSSPseudoClass(const aPseudoClassID: TCSSNumericalID): boolean;
  end;

type

  { TCSSResCustomAttributeDesc }

  TCSSResCustomAttributeDesc = class(TCSSAttributeDesc)
  public
  end;
  TCSSResCustomAttributeDescArray = array of TCSSResCustomAttributeDesc;

  { TCSSResolvedAttribute - used for shared rule lists, merged by the cascade algorithm, not yet computed  }

  TCSSResolvedAttribute = record
    AttrID: TCSSNumericalID;
    Specificity: TCSSSpecificity;
    DeclEl: TCSSDeclarationElement;
  end;
  TCSSResolvedAttributeArray = array of TCSSResolvedAttribute;
  PCSSResolvedAttribute = ^TCSSResolvedAttribute;

  TCSSSharedRule = record
    Rule: TCSSRuleElement;
    Specificity: TCSSSpecificity;
  end;
  PCSSSharedRule = ^TCSSSharedRule;
  TCSSSharedRuleArray = array of TCSSSharedRule;

  // A "sibling selector" is one whose match result for the subject (rightmost) compound
  // depends on the node's siblings, its position among siblings, or its descendants
  // (combinators '+'/'~', :nth-*(), :first/last/only-child/of-type, :empty, :has()).
  // Same-parent siblings share their ancestor chain, so combinators/pseudos that appear
  // only in an ancestor part are NOT sibling selectors. Used by the style-sharing
  // optimization (see TFresnelElement.ComputeCSSValues).
  TCSSSiblingSelector = record
    Selector: TCSSElement;
    Rule: TCSSRuleElement; // owning rule, passed to SelectorMatches for the nested context
    SrcSpecificity: TCSSSpecificity; // origin specificity of the rule's layer
  end;
  TCSSSiblingSelectorArray = array of TCSSSiblingSelector;

  { TCSSSiblingMatchList - the subset of the resolver's sibling selectors that match a
    given node, in FSiblingSelectors order (so two nodes' lists are directly comparable). }
  TCSSSiblingMatchList = record
    Matched: array of TCSSElement;
  end;

  { TCSSSharedRuleList - elements with same CSS rules share the base attributes }

  TCSSSharedRuleList = class
  public
    AllDecl: TCSSDeclarationElement;
    AllSpecificity: TCSSSpecificity;
    Rules: TCSSSharedRuleArray; // sorted ascending for Specificity, secondary for source position
    Values: TCSSResolvedAttributeArray; // not sorted, merged, not computed
    destructor Destroy; override;
    procedure Clear;
    function Clone: TCSSSharedRuleList;
    function IndexOfAttr(AttrId: TCSSNumericalID; ForInsert: boolean = false): integer;
  end;

  { TCSSAttributeValue }

  TCSSAttributeValue = class
  public
    AttrID: TCSSNumericalID; // the resolver has substituted all shorthands
    Invalid: boolean;
    Tokens: TBytes; // tokenized value, the resolver has substituted all var() calls
  end;
  TCSSAttributeValueArray = array of TCSSAttributeValue;

  { TCSSAttributeValues }

  TCSSAttributeValues = class
  public
    AllValue: TCSSNumericalID;
    Values: TCSSAttributeValueArray; // the resolver sorts them ascending for AttrID, shorthands are already replaced with longhands
    procedure SortValues; virtual; // ascending AttrID
    function IndexOf(AttrID: TCSSNumericalID): integer;
    procedure SetComputedValue(AttrID: TCSSNumericalID; const aTokens: TBytes);
    destructor Destroy; override;
  end;

  { TCSSKeyframe - one offset of a @keyframes }

  TCSSKeyframe = record
    Offset: double; // Ascending and unique within a TCSSKeyframeArray.
    // The cascaded declarations of every keyframe block with Offset: merged,
    // var() substituted and shorthands expanded.
    // Never nil, but can be empty. Owned by the caller, see FreeCSSKeyframes.
    Values: TCSSAttributeValues;
  end;
  TCSSKeyframeArray = array of TCSSKeyframe;

  TCSSRuleData = class(TCSSRuleParserData)
  public
    HasDisabledDecls: boolean; // at least one direct child declaration is disabled,
      // maintained by TCSSResolver.DisableDeclaration/EnableDeclaration
    // @media at-rules only, maintained by TCSSResolver.AtMediaMatches:
    MediaResult: boolean; // cached result of the media condition
    MediaStamp: integer; // only valid if equal to TCSSResolver.MediaStamp, 0 = never computed
    // the stylesheet this rule belongs to, maintained by TCSSResolver.BuildRuleBuckets:
    Origin: TCSSOrigin; // origin of the stylesheet, see CSSOriginToSpecifity
    SourceIndex: integer; // index in TCSSResolver.StyleSheets, -1 = unknown
    StyleRuleParent: TCSSRuleElement; // innermost enclosing style rule, nil if top level
    SourceStamp: integer; // Origin, SourceIndex and StyleRuleParent are only valid if
      // this equals TCSSResolver.SourceStamp, 0 = never updated
  end;

  TCSSResolverNthChildParamsCacheItem = record
    TypeID: TCSSNumericalID;
    ChildIDs: TIntegerDynArray;
    Cnt: integer; // = length(ChildIDs), used during creation
  end;
  PCSSNthChildParamsCacheItem = ^TCSSResolverNthChildParamsCacheItem;
  TCSSResolverNthChildParamsCacheItems = array of TCSSResolverNthChildParamsCacheItem;

  TCSSResolverNthChildParams = class;

  TCSSResolverNthChildParamsCache = class
  public
    Owner: TCSSResolverNthChildParams;
    Parent: ICSSNode;
    OfSelector: TCSSElement;
    StackDepth: integer;
    Items: TCSSResolverNthChildParamsCacheItems;
  end;
  TCSSResolverNthChildParamsCaches = array of TCSSResolverNthChildParamsCache;

  { TCSSResolverNthChildParams }

  TCSSResolverNthChildParams = class(TCSSNthChildParams)
  public
    StackCache: TCSSResolverNthChildParamsCaches;
    destructor Destroy; override;
  end;

  TCSSResolverOption = (
    croErrorOnUnknownName
    );
  TCSSResolverOptions = set of TCSSResolverOption;

  { TCSSResolverLogEntry }

  TCSSResolverLogEntry = class
  public
    MsgType: TEventType;
    ID: TCSSMsgID;
    Msg: TCSSString;
    PosEl: TCSSElement;
  end;
  TCSSResolverLogEntryClass = class of TCSSResolverLogEntry;
  TCSSResolverLogEntryArray = array of TCSSResolverLogEntry;

  TCSSResolverLogEvent = procedure(Sender: TObject; Entry: TCSSResolverLogEntry) of object;

  TCSSResStringComparison = (
    crscDefault,
    crscCaseInsensitive,
    crscCaseSensitive
    );
  TCSSResStringComparisons = set of TCSSResStringComparison;

  { TCSSResolver }

  TCSSResolver = class(TCSSBaseResolver)
  public
    type
      TStyleSheet = class
        Source: TCSSString;
        Name: TCSSString; // case sensitive
        Origin: TCSSOrigin;
        Element: TCSSElement;
        Parsed: boolean;
        Stamp: integer; // bumped whenever the stylesheet (re)parses, see GetStyleSheetStamp
      end;
      TStyleSheets = array of TStyleSheet;

      // A stable reference to a declaration that survives a reparse (which
      // replaces the TCSSDeclarationElement instances). See GetDeclarationPath /
      // FindDeclaration.
      // Selectors and RuleIndex describe the chain of (possibly nested) rules from
      // the stylesheet top level (index 0) down to the rule holding the declaration.
      TCSSDeclarationPath = record
        Valid: boolean;
        Origin: TCSSOrigin;
        SheetName: TCSSString;
        Selectors: array of TCSSString; // selectors of each rule in the nesting chain
        PropertyName: TCSSString;       // the declaration's (first) property name
        RuleIndex: array of integer;    // index of each rule within its parent rule list
        PropIndex: integer;             // index of the declaration within the innermost rule
      end;

      TLayerElement = record
        Src: TStyleSheet;
        Element: TCSSElement;
      end;
      TLayerElements = array of TLayerElement;

      TLayer = record
        Name: TCSSString;
        Origin: TCSSOrigin;
        Elements: TLayerElements;
        ElementCount: integer;
      end;
      TLayerArray = array of TLayer;
  private
    FLayers: TLayerArray; // sorted for Origin, named layers before anonymous layers
    FOnLog: TCSSResolverLogEvent;
    FOptions: TCSSResolverOptions;
    FStringComparison: TCSSResStringComparison;
    FStyleSheets: TStyleSheets;
    FStyleSheetCount: integer;
    FStyleSheetStamp: integer; // monotonic counter handed out to TStyleSheet.Stamp on change
    FMediaStamp: integer; // monotonic counter of the @media environment, see InvalidateMedia
    FSourceStamp: integer; // monotonic counter bumped by BuildRuleBuckets, see TCSSRuleData.SourceStamp
    function GetCustomAttributes(Index: TCSSNumericalID): TCSSAttributeDesc;
    function GetLogCount: integer;
    function GetLogEntries(Index: integer): TCSSResolverLogEntry;
    function GetStyleSheets(Index: integer): TStyleSheet;
    procedure SetOptions(const AValue: TCSSResolverOptions);
  protected
    type

      { TMergedAttribute }

      TMergedAttribute = record
        Stamp: Integer; // only valid if equal to FMergedAttributesStamp
        Specificity: TCSSSpecificity;
        DeclEl: TCSSDeclarationElement; // can be nil if set by a shorthand
        Tokens: TBytes; // tokenized value, see TCSSBaseResolver.Tokenize
        Complete: boolean;
        Prev, Next: TCSSNumericalID; // valid if >0, see below FMergedAttributeFirst
      end;
      PMergedAttribute = ^TMergedAttribute;
      TMergedAttributeArray = array of TMergedAttribute;

      // selector rules bucketed by the rightmost identifier of a selector
      TCSSRuleBucketKind = (
        rbkOther, // universal, attribute-only, pseudo-only, complex, nested
        rbkID,    // rightmost is #id
        rbkClass, // rightmost is .class
        rbkType   // rightmost is a tag/type
        );

      TCSSRuleBucketItem = record
        Rule: TCSSRuleElement; // can be a TCSSAtRuleElement (in the Other bucket)
        DocIndex: integer; // document order, used as stable secondary sort key for the cascade
        SourceSpecificity: TCSSSpecificity; // origin specificity of the rule's layer
      end;
      PCSSRuleBucketItem = ^TCSSRuleBucketItem;
      TCSSRuleBucketItemArray = array of TCSSRuleBucketItem;

      { TCSSRuleBucket - a list of rules sharing the same rightmost identifier }

      TCSSRuleBucket = class
      public
        Items: TCSSRuleBucketItemArray;
        Count: integer;
        procedure Add(aRule: TCSSRuleElement; aDocIndex: integer; aSrcSpecificity: TCSSSpecificity);
      end;
      TCSSRuleBucketArray = array of TCSSRuleBucket;

      { TCSSDisabledDecl - a disabled declaration, stored by declaration path so it
        survives a stylesheet reparse (which replaces the declaration elements) }

      TCSSDisabledDecl = class
      public
        Path: TCSSDeclarationPath;
        Decl: TCSSDeclarationElement; // the currently disabled element (updated on reparse)
      end;

  protected
    FCustomAttributes: TCSSResCustomAttributeDescArray;
    FCustomAttributeCount: TCSSNumericalID;
    FCustomAttributeNameToDesc: TFPHashList;
    FElRules: TCSSSharedRuleArray;
    FElRuleCount: integer;
    FNode: ICSSNode;
    FLogEntries: TFPObjectList; // list of TCSSResolverLogEntry
    FSharedRuleLists: TAVLTree; // tree of TCSSSharedRuleList sorted for rules
    FDisabledDecls: TFPHashObjectList; // key = DisabledDeclKey(Path) -> TCSSDisabledDecl (owned)
    FMergedAttributes: TMergedAttributeArray;
    FMergedAttributesStamp: integer;
    FMergedAttributeFirst, FMergedAttributeLast: TCSSNumericalID; // first, last index in FMergedAttributes of linked list of attributes with current stamp
    FMergedAllDecl: TCSSDeclarationElement;
    FMergedAllSpecificity: TCSSSpecificity;
    FSourceSpecificity: TCSSSpecificity; // Origin specificity during Compute run
    FMergeVarsFromNode: boolean; // SubstituteVarCalls resolves a var() against FNode instead of its parent
    FCSSRegistryStamp: TCSSNumericalID;
    FCSSClassNameToID: TFPHashList; // class name -> TCSSNumericalID (>=1)
    FCSSClassNames: TCSSStringArray; // index = ID-1, reverse lookup
    FCSSClassNameCount: TCSSNumericalID;
    FCSSClassIDStamp: TCSSNumericalID; // changed whenever the css class or id name to numerical id mapping changed
    FCSSIDNameToIndex: TFPHashList; // id name -> TCSSNumericalID (>=1)
    FCSSIDNames: TCSSStringArray; // index = ID-1, reverse lookup
    FCSSIDCount: TCSSNumericalID;
    // rule buckets, based on the rightmost identifier of each selector;
    // (re)built lazily by Compute when FRuleBucketsValid is false
    FBucketOther: TCSSRuleBucket;
    FBucketType: TCSSRuleBucketArray; // index = type id
    FBucketClass: TCSSRuleBucketArray; // index = class id
    FBucketID: TCSSRuleBucketArray; // index = id numerical id
    // all outermost @starting-style at-rules, in document order; they are skipped
    // by the normal cascade and only evaluated by ComputeStartingStyle
    FBucketStartingStyle: TCSSRuleBucket;
    // all @keyframes at-rules, animation name -> TCSSAtRuleElement, added in document
    // order; duplicate names are allowed, see AddKeyframes and FindKeyframesRule
    FKeyframes: TFPHashList;
    FRuleCandidates: TCSSRuleBucketItemArray; // working buffer of FindMatchingRules
    FRuleCandidateCount: integer;
    FBucketDocIndex: integer; // running document order while building buckets
    FRuleBucketsValid: boolean; // false when FLayers changed and the buckets need a rebuild
    // all selectors whose subject match depends on siblings/position/descendants;
    // (re)built lazily with the rule buckets, used by MatchSiblingSelectors
    FSiblingSelectors: TCSSSiblingSelectorArray;
    FSiblingSelectorCount: integer;
    procedure ChangeCSSClassIDStamp;

    // parse stylesheets
    function DoCSSWarn(Sender: TObject; Msg: TCSSString; aRow, aCol: integer): boolean; virtual;
    procedure ParseSource(Index: integer); virtual;
    function ParseCSSSource(const Src: TCSSString; Inline: boolean): TCSSElement; virtual;
    procedure ClearElements; virtual;
    // Drop every layer entry that belongs to aSheet (used before reparsing it,
    // so a replaced sheet leaves no dangling element pointer in the layers).
    procedure RemoveStyleSheetFromLayers(aSheet: TStyleSheet); virtual;
    // Move the layer entry of the sheet at Index (which ParseSource appended to
    // the layer end) to the slot matching the FStyleSheets order, so the cascade
    // document order follows the array order after a mid-list InsertStyleSheet.
    procedure OrderStyleSheetInLayer(Index: integer); virtual;
    procedure ClearCustomAttributes; virtual;

    // resolving rules
    procedure ComputeElement(El: TCSSElement); virtual;
    procedure ComputeRule(aRule: TCSSRuleElement); virtual;
    // ParentSpecificity: specificity of the enclosing style rule for the current node,
    // CSSSpecificityNoMatch if there is none or it does not match
    procedure ComputeAtRule(aRule: TCSSAtRuleElement;
      ParentSpecificity: TCSSSpecificity = CSSSpecificityNoMatch); virtual;
    // Result: true if one of the media queries of aRule matches. The result is cached
    // in TCSSRuleData.MediaResult and only recomputed after InvalidateMedia.
    function AtMediaMatches(aRule: TCSSAtRuleElement): boolean; virtual;
    // @starting-style, see ComputeStartingStyle
    function StartingStyleContextMatches(aRule: TCSSAtRuleElement;
      out Specificity: TCSSSpecificity): boolean; virtual;
    procedure ComputeStartingStyleRule(aRule: TCSSAtRuleElement; SrcSpecificity: TCSSSpecificity); virtual;
    function ComputeNestedRuleSelectorSpecifity(aSelector: TCSSElement): TCSSSpecificity;
    function GetRuleOfSelector(aSelector: TCSSElement): TCSSRuleElement; virtual;
    function GetRuleParentOfSelector(aSelector: TCSSElement; SkipAtRules: boolean): TCSSRuleElement; virtual;
    // a media query either matches or not, it has no specificity of its own
    function MediaSelectorIdentifierMatches(aIdentifier: TCSSResolvedIdentifierElement): boolean; virtual;
    function MediaSelectorBinaryMatches(aBinary: TCSSBinaryElement): boolean; virtual;
    function MediaSelectorMatches(aSelector: TCSSElement): boolean; virtual;
    function MediaSelectorListMatches(aList: TCSSListElement): boolean; virtual;
    function SelectorMatches(aSelector: TCSSElement; const TestNode: ICSSNode; OnlySpecificity: boolean; aRule: TCSSRuleElement = nil): TCSSSpecificity; virtual;
    function SelectorIdentifierMatches(aIdentifier: TCSSResolvedIdentifierElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function SelectorAndWhitespaceMatches(aRightSelector: TCSSElement; const TestNode: ICSSNode): TCSSSpecificity; virtual;
    function SelectorAndGTMatches(aRightSelector: TCSSElement; const TestNode: ICSSNode): TCSSSpecificity; virtual;
    function SelectorAndPlusMatches(aRightSelector: TCSSElement; const TestNode: ICSSNode): TCSSSpecificity; virtual;
    function SelectorAndTildeMatches(aRightSelector: TCSSElement; const TestNode: ICSSNode): TCSSSpecificity; virtual;
    function SelectorAndCompoundMatches(aList: TCSSListElement; const TestNode: ICSSNode): TCSSSpecificity; virtual;
    function SelectorAndRightAndMatches(aBinary: TCSSBinaryElement; const TestNode: ICSSNode): TCSSSpecificity; virtual;
    function SelectorHashIdentifierMatches(aIdentifier: TCSSHashIdentifierElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function SelectorClassNameMatches(aClassName: TCSSResolvedClassNameElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function SelectorPseudoClassMatches(aPseudoClass: TCSSResolvedPseudoClassElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function SelectorListMatches(aList: TCSSListElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function SelectorUnaryMatches(aUnary: TCSSUnaryElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function SelectorBinaryMatches(aBinary: TCSSBinaryElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function SelectorPseudoElementMatches(aLeft, aRight: TCSSElement; const TestNode: ICSSNode): TCSSSpecificity; virtual;
    function SelectorArrayMatches(anArray: TCSSArrayElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function SelectorArrayBinaryMatches(aBinary: TCSSBinaryElement; const TestNode: ICSSNode): TCSSSpecificity; virtual;
    function SelectorCallMatches(aCall: TCSSResolvedCallElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function Call_Not(aCall: TCSSResolvedCallElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function Call_Is(aCall: TCSSResolvedCallElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function Call_Where(aCall: TCSSResolvedCallElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function Call_NthChild(PseudoFuncID: TCSSNumericalID; aCall: TCSSResolvedCallElement; const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity; virtual;
    function CollectSiblingsOf(PseudoFuncID: TCSSNumericalID; TestNode: ICSSNode;
      Params: TCSSResolverNthChildParams): TIntegerDynArray; virtual;
    function GetSiblingOfIndex(SiblingIDs: TIntegerDynArray; Index: integer): integer; virtual;
    function ComputeValue(El: TCSSElement): TCSSString; virtual;
    function SameValueText(const A, B: TCSSString): boolean; virtual;
    function SameValueText(A: PCSSChar; ALen: integer; B: PCSSChar; BLen: integer): boolean; virtual;
    function PosSubString(const SearchStr, Str: TCSSString): integer; virtual;
    function PosWord(const SearchWord, Words: TCSSString): integer; virtual;
    function GetSiblingCount(aNode: ICSSNode): integer; virtual;

    // resolving identifiers
    function ResolveIdentifier(El: TCSSResolvedIdentifierElement; aKind: TCSSNumericalIDKind): TCSSNumericalID; virtual;

    // rule buckets
    procedure MediaEnvironmentChanged; override; // one of the @media events changed
    procedure ClearRuleBuckets; virtual;
    procedure BumpSourceStamp; // invalidate everything cached about the current rules
    procedure UpdateRuleBuckets; virtual; // rebuild the buckets if FLayers changed
    procedure BuildRuleBuckets; virtual; // bucket all selector rules; called from EnsureRuleBuckets
    procedure BucketRule(aRule: TCSSRuleElement; SrcSpecificity: TCSSSpecificity); virtual;
    procedure UpdateSourceOfElement(El: TCSSElement; anOrigin: TCSSOrigin;
      aSheetIndex: integer; aStyleRuleParent: TCSSRuleElement = nil); virtual;
    procedure CollectStartingStyleRules(aRule: TCSSRuleElement; SrcSpecificity: TCSSSpecificity); virtual;
    // @keyframes, see FindKeyframesRule
    procedure CollectKeyframes(aRule: TCSSRuleElement); virtual;
    procedure AddKeyframes(aRule: TCSSAtRuleElement); virtual;
    function KeyframesContextMatches(aRule: TCSSAtRuleElement; const aNode: ICSSNode): boolean; virtual;
    // sibling selectors (style sharing)
    procedure CollectSiblingSelectors(aRule: TCSSRuleElement; SrcSpecificity: TCSSSpecificity); virtual;
    procedure AddSiblingSelector(aSelector: TCSSElement; aRule: TCSSRuleElement; SrcSpecificity: TCSSSpecificity);
    function SelectorHasSiblingDependency(aSelector: TCSSElement): boolean; virtual; // subject-side
    function SelectorComponentSiblingDep(aComponent: TCSSElement): boolean; virtual;
    procedure GetSelectorBucketKey(aSelector: TCSSElement; out aKind: TCSSRuleBucketKind;
      out NumID: TCSSNumericalID); virtual;
    function GetTypeBucket(aTypeID: TCSSNumericalID): TCSSRuleBucket;
    function GetClassBucket(aClassID: TCSSNumericalID): TCSSRuleBucket;
    function GetIDBucket(aID: TCSSNumericalID): TCSSRuleBucket;
    procedure AddBucketToRuleCandidates(Bucket: TCSSRuleBucket);
    procedure SortRuleCandidates;
    // shared rules
    procedure ClearSharedRuleLists; virtual;
    procedure FindMatchingRules; virtual; // create FElRules for current FNode
    procedure AddRule(aRule: TCSSRuleElement; Specificity: TCSSSpecificity); // add rule to current array (FElRules)
    function FindSharedRuleList(const Rules: TCSSSharedRuleArray): TCSSSharedRuleList; virtual;
    function CreateSharedRuleList: TCSSSharedRuleList; virtual; // using FElRules, sets FMergedAttributes

    // merge properties
    procedure ClearMerge; virtual;
    procedure InitMerge; virtual; // call this after CSSRegistry has been altered
    procedure SetMergedAttribute(AttrID, aSpecificity: TCSSNumericalID; DeclEl: TCSSDeclarationElement);
    procedure RemoveMergedAttribute(AttrID: TCSSNumericalID);
    procedure MergeAttribute(El: TCSSElement; aSpecificity: TCSSSpecificity); virtual;
    procedure SaveSharedMergedAttributes(SharedMerged: TCSSSharedRuleList); virtual;
    procedure LoadSharedMergedAttributes(SharedMerged: TCSSSharedRuleList); virtual;
    function DeclKeyData(Decl: TCSSDeclarationElement): TCSSAttributeKeyData; virtual;
    function RuleData(Rule: TCSSRuleElement): TCSSRuleData; virtual;
    function DisabledDeclKey(const Path: TCSSDeclarationPath): TCSSString; virtual;
    procedure RestoreDisabledDeclarations(Sheet: TStyleSheet); virtual;
    procedure UpdateRuleHasDisabledDecls(Decl: TCSSDeclarationElement); virtual; overload;
    procedure UpdateRuleHasDisabledDecls(Rule: TCSSRuleElement); virtual; overload;
    procedure WriteMergedAttributes(const Title: TCSSString); virtual;

    // var() and shorthands
    procedure LoadMergedValues; virtual; // load Value strings from css elements and remove longhand placeholders
    procedure SubstituteVarCalls; virtual; // replace all var()
    procedure ApplyShorthands; virtual; // replace all shorthands with their longhands
    function CreateValueList: TCSSAttributeValues; virtual; // from FMergedAttributes
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Init; virtual; // call after adding stylesheets and before computing all nodes
    function GetElPath(El: TCSSElement): TCSSString; virtual;
    function GetElPos(El: TCSSElement): TCSSString; virtual;
    function ParseInlineStyle(const Src: TCSSString): TCSSRuleElement; virtual; // must be freed by caller
    procedure Compute(Node: ICSSNode;
      ElementStyle: TCSSRuleElement; // inline/element style of Node
      out Rules: TCSSSharedRuleList; // owned by resolver
      out Values: TCSSAttributeValues
      ); virtual;
    function HasStartingStyleRules: boolean; virtual; // true if there any @starting-style rules
    // The @keyframes rule named aName applying to aNode, nil if there is none.
    function FindKeyframesRule(const aNode: ICSSNode; const aName: TCSSString): TCSSAtRuleElement; virtual;
    function HasKeyframesRules: boolean; virtual; // true if there any @keyframes rules
    // The values of every offset of the @keyframes rule aRule, ascending for Offset.
    // Result=false no valid keyframe block.
    // Keyframes is owned by the caller, see FreeCSSKeyframes.
    // AttrIDs is the ascending set of AttrIDs of all Keyframes[].Values.
    // Filtering out what cannot be animated - the animation-* properties in particular - is up to the caller.
    function ComputeKeyframes(const aNode: ICSSNode; aRule: TCSSAtRuleElement;
      out Keyframes: TCSSKeyframeArray;
      out AttrIDs: TCSSNumericalIDArray
      ): boolean; virtual;
    // The @starting-style declarations applying to aNode
    function ComputeStartingStyle(const aNode: ICSSNode;
      out Rules: TCSSSharedRuleList; // owned by resolver
      out Values: TCSSAttributeValues
      ): boolean; virtual;
    // Match all sibling/positional selectors against Node
    function MatchSiblingSelectors(const Node: ICSSNode): TCSSSiblingMatchList; virtual;
    // The origin specificity of the stylesheet containing aRule, see CSSOriginToSpecifity.
    // 0 if aRule belongs to none of the stylesheets, e.g. an element style.
    function GetRuleSourceSpecificity(aRule: TCSSRuleElement): TCSSSpecificity; virtual;
    // The specificity of aRule for aNode: the best of its selectors plus the origin of aRule's stylesheet.
    function GetRuleSpecificity(aRule: TCSSRuleElement; const aNode: ICSSNode): TCSSSpecificity; virtual;
    function GetRuleStyleSheet(aRule: TCSSRuleElement): TStyleSheet; virtual;
    // attributes
    property CustomAttributes[Index: TCSSNumericalID]: TCSSAttributeDesc read GetCustomAttributes;
    property CustomAttributeCount: TCSSNumericalID read FCustomAttributeCount;
    function GetAttributeID(const aName: TCSSString; AutoCreate: boolean = false): TCSSNumericalID; override;
    function GetAttributeDesc(AttrId: TCSSNumericalID): TCSSAttributeDesc; override;
    // True when the value is not valid for the attribute AttrID. A value that
    // cannot be checked - a custom property, an attribute below/at 'all', or a value that
    // still contains a var() call - is treated as valid. Used by the style inspector to
    // flag an invalid value live while it is being edited.
    function IsAttrValueInvalid(AttrID: TCSSNumericalID; const Tokens: TBytes): boolean; overload;
    function IsAttrValueInvalid(AttrID: TCSSNumericalID; const aValue: TCSSString): boolean; overload;
    function GetDeclarationTokens(Decl: TCSSDeclarationElement): TBytes; virtual;
    function GetDeclarationValue(Decl: TCSSDeclarationElement): TCSSString; virtual;
    // css class names from selectors, numbered from 1
    function GetCSSClassID(const aCSSClassName: TCSSString): TCSSNumericalID; override;
    function AddCSSClassID(const aCSSClassName: TCSSString): TCSSNumericalID; override;
    function GetCSSClassName(aID: TCSSNumericalID): TCSSString; virtual;
    property CSSClassNameCount: TCSSNumericalID read FCSSClassNameCount;
    property CSSClassIDStamp: TCSSNumericalID read FCSSClassIDStamp; // always >0, changed whenever the css class or id name to numerical id mapping changed
    // css ids from selectors, numbered from 1
    function GetCSSIDIndex(const aCSSID: TCSSString): TCSSNumericalID; override;
    function AddCSSID(const aCSSID: TCSSString): TCSSNumericalID; override;
    function GetCSSIDName(aID: TCSSNumericalID): TCSSString; virtual;
    property CSSIDCount: TCSSNumericalID read FCSSIDCount;
  public
    property Options: TCSSResolverOptions read FOptions write SetOptions;
    property StringComparison: TCSSResStringComparison read FStringComparison;
  public
    // stylesheets
    procedure ClearStyleSheets; virtual;
    function AddStyleSheet(anOrigin: TCSSOrigin; const aName: TCSSString; const aSource: TCSSString): TStyleSheet; virtual;
    function InsertStyleSheet(Index: integer; anOrigin: TCSSOrigin; const aName: TCSSString; const aSource: TCSSString): TStyleSheet; virtual;
    procedure ReplaceStyleSheet(Index: integer; const NewSource: TCSSString); virtual;
    procedure DeleteStyleSheet(Index: integer); virtual;
    // Call after a value used by HasMediaBoolean/IsMediaPlain/MediaCompare changed
    procedure InvalidateMedia; virtual;
    // Same as InvalidateMedia, kept for compatibility.
    procedure InvalidateRuleBuckets; virtual;
    function IndexOfStyleSheet(aSheet: TStyleSheet): integer;
    function IndexOfStyleSheetWithElement(El: TCSSElement): integer;
    function IndexOfStyleSheetWithName(anOrigin: TCSSOrigin; const aName: TCSSString): integer;
    function FindStyleSheetWithElement(El: TCSSElement): TStyleSheet;
    function FindStyleSheetWithName(const aName: TCSSString; anOrigin: TCSSOrigin = cssoAuthor): TStyleSheet;
    function GetStyleSheetStamp(anOrigin: TCSSOrigin; const aName: TCSSString): integer; // -1 if no such sheet
    function GetDeclarationPath(DeclEl: TCSSDeclarationElement; out Path: TCSSDeclarationPath): boolean;
    function FindDeclaration(const Path: TCSSDeclarationPath): TCSSDeclarationElement;
    // disable/enable a single declaration; The disabled state is restored after the stylesheet is reparsed.
    // inline declarations need TransferDisabledDeclarations
    procedure DisableDeclaration(Decl: TCSSDeclarationElement); virtual;
    procedure EnableDeclaration(Decl: TCSSDeclarationElement); virtual;
    function IsDeclarationDisabled(Decl: TCSSDeclarationElement): boolean; virtual;
    function RuleHasDisabledDeclaration(Rule: TCSSRuleElement): boolean; virtual;
    procedure TransferDisabledDeclarations(OldRule, NewRule: TCSSRuleElement); virtual;
    function GetDisabledDeclarations: TFPList; virtual; // TCSSDeclarationElement list, caller frees the list
    function GetDisabledDeclarationPaths: TStrings; virtual; // path -> Objects[i]=TCSSDeclarationElement, caller frees
    property StyleSheetCount: integer read FStyleSheetCount;
    property StyleSheets[Index: integer]: TStyleSheet read GetStyleSheets;
    // Bumped whenever the set of stylesheets or any sheet's source actually
    // changed (Add/Insert/Delete, or Replace with a differing source). Snapshot
    // and compare to detect a real change without re-parsing.
    property StyleSheetsStamp: integer read FStyleSheetStamp;
    // Always >0, bumped by InvalidateMedia. A TCSSRuleData.MediaResult is valid
    // as long as its MediaStamp equals this.
    property MediaStamp: integer read FMediaStamp;
    // Always >0, bumped by BuildRuleBuckets and by every change freeing rule elements.
    // The TCSSRuleData.Origin, SourceIndex and StyleRuleParent of a rule are valid as
    // long as its SourceStamp equals this, and so is a rule pointer stored by a user.
    property SourceStamp: integer read FSourceStamp;
    // Number of rules the last FindMatchingRules had to check, i.e. the content of the
    // buckets selected by that node. For diagnostics and benchmarks.
    property RuleCandidateCount: integer read FRuleCandidateCount;
    property Layers: TLayerArray read FLayers;
  public
    // logging
    procedure Log(MsgType: TEventType; const ID: TCSSMsgID; const Msg: TCSSString; PosEl: TCSSElement); virtual;
    procedure LogWarning(IsError: boolean; const ID: TCSSMsgID; const Msg: TCSSString; PosEl: TCSSElement); virtual;
    property LogCount: integer read GetLogCount;
    property LogEntries[Index: integer]: TCSSResolverLogEntry read GetLogEntries;
    property OnLog: TCSSResolverLogEvent read FOnLog write FOnLog;
  end;

function SameCSSSiblingMatches(const A, B: TCSSSiblingMatchList): boolean;
function ComparePointer(Data1, Data2: Pointer): integer;
function CompareCSSSharedRuleArrays(const Rules1, Rules2: TCSSSharedRuleArray): integer;
function CompareCSSSharedRuleLists(A, B: Pointer): integer;
function CompareRulesArrayWithCSSSharedRuleList(RuleArray, SharedRuleList: Pointer): integer;

function CSSIsPlainRule(El: TCSSElement): boolean; overload; // true if is a rule, but not a @-rule
function CSSIsPlainRule(C: TClass): boolean; overload; // C must not be nil
function CSSRuleSelectorsStr(Rule: TCSSRuleElement): TCSSString;
function CSSDeclPropertyName(DeclEl: TCSSDeclarationElement): TCSSString;
function CSSGetTopLevelRules(Root: TCSSElement): TCSSRuleElementArray;
function CSSGetNestedRules(Rule: TCSSRuleElement): TCSSRuleElementArray;

// @keyframes
function CSSKeyframesName(aRule: TCSSAtRuleElement): TCSSString;
// The offsets of one keyframe block ('from', 'to', '50%', '0%, 100%') as fractions
// 0..1, in source order.
// Result=false if any selector is invalid. css-animations-1 3 drops the whole block.
function CSSKeyframeOffsets(aKeyframe: TCSSRuleElement; out Offsets: TDoubleDynArray): boolean;
// Free the TCSSAttributeValues of every entry and clear the array.
procedure FreeCSSKeyframes(var Keyframes: TCSSKeyframeArray);

implementation

function SameCSSSiblingMatches(const A, B: TCSSSiblingMatchList): boolean;
var
  i, n: Integer;
begin
  n:=length(A.Matched);
  if n<>length(B.Matched) then exit(false);
  for i:=0 to n-1 do
    if A.Matched[i]<>B.Matched[i] then exit(false);
  Result:=true;
end;

function ComparePointer(Data1, Data2: Pointer): integer;
begin
  if Data1>Data2 then Result:=-1
  else if Data1<Data2 then Result:=1
  else Result:=0;
end;

function CompareCSSSharedRuleArrays(const Rules1, Rules2: TCSSSharedRuleArray): integer;
var
  Len1, Len2, i: Integer;
  R1, R2: PCSSSharedRule;
begin
  Len1:=length(Rules1);
  Len2:=length(Rules2);
  if Len1>Len2 then exit(1)
  else if Len1<Len2 then exit(-1);
  if Len1=0 then exit(0);
  R1:=@Rules1[0];
  R2:=@Rules2[0];
  for i:=0 to Len1-1 do
  begin
    if R1^.Specificity>R2^.Specificity then exit(1)
    else if R1^.Specificity<R2^.Specificity then exit(-1);
    Result:=ComparePointer(R1^.Rule,R2^.Rule);
    if Result<>0 then exit;
    inc(R1);
    inc(R2);
  end;
  Result:=0;
end;

function CompareCSSSharedRuleLists(A, B: Pointer): integer;
var
  List1: TCSSSharedRuleList absolute A;
  List2: TCSSSharedRuleList absolute B;
begin
  Result:=CompareCSSSharedRuleArrays(List1.Rules,List2.Rules);
end;

function CompareRulesArrayWithCSSSharedRuleList(RuleArray,
  SharedRuleList: Pointer): integer;
var
  Arr: TCSSSharedRuleArray absolute RuleArray;
  List: TCSSSharedRuleList absolute SharedRuleList;
begin
  Result:=CompareCSSSharedRuleArrays(Arr,List.Rules);
end;

// the rule's selectors joined as one string, matching the inspector's display
function CSSRuleSelectorsStr(Rule: TCSSRuleElement): TCSSString;
var
  i: Integer;
begin
  Result:='';
  for i:=0 to Rule.SelectorCount-1 do
  begin
    if i>0 then Result:=Result+', ';
    Result:=Result+Rule.Selectors[i].AsFormattedString;
  end;
end;

// the declaration's (first) property name
function CSSDeclPropertyName(DeclEl: TCSSDeclarationElement): TCSSString;
begin
  if DeclEl.KeyCount>0 then
    Result:=DeclEl.Keys[0].AsFormattedString
  else
    Result:='';
end;

function CSSIsPlainRule(El: TCSSElement): boolean; overload;
begin
  Result:=(El<>nil) and CSSIsPlainRule(El.ClassType);
end;

function CSSIsPlainRule(C: TClass): boolean; overload;
begin
  while C<>nil do
  begin
    if C=TCSSAtRuleElement then exit(false);
    if C=TCSSRuleElement then exit(true);
    C:=C.ClassParent;
  end;
  Result:=false;
end;

// scan El's subtree for immediate child rules, descending through non-rule
// containers (compounds) but never into a rule (its nested rules are a deeper level)
procedure CSSScanChildRules(El: TCSSElement; var Rules: TCSSRuleElementArray; var Cnt: integer);
var
  i: Integer;
  C: TClass;
begin
  if El=nil then exit;
  if El is TCSSDeclarationElement then exit; // a declaration's children are values, not rules
  C:=El.ClassType;
  if C.InheritsFrom(TCSSRuleElement) then
  begin
    if Cnt>=length(Rules) then
      SetLength(Rules,Cnt*2+8);
    Rules[Cnt]:=TCSSRuleElement(El);
    inc(Cnt);
    exit; // do not descend into the rule
  end;
  if El is TCSSChildrenElement then
    for i:=0 to TCSSChildrenElement(El).ChildCount-1 do
      CSSScanChildRules(TCSSChildrenElement(El).Children[i],Rules,Cnt);
end;

// the top-level rules of a stylesheet (Root is a compound of rules, or a single rule)
function CSSGetTopLevelRules(Root: TCSSElement): TCSSRuleElementArray;
var
  Cnt: Integer;
  C: TClass;
begin
  Result:=nil;
  if Root=nil then exit;
  C:=Root.ClassType;
  if C.InheritsFrom(TCSSRuleElement) then
  begin
    SetLength(Result,1);
    Result[0]:=TCSSRuleElement(Root);
    exit;
  end;
  Cnt:=0;
  if Root is TCSSChildrenElement then
    CSSScanChildRules(Root,Result,Cnt);
  SetLength(Result,Cnt);
end;

// the immediate nested rules of a rule
function CSSGetNestedRules(Rule: TCSSRuleElement): TCSSRuleElementArray;
var
  i: Integer;
begin
  Result:=[];
  SetLength(Result,Rule.NestedRuleCount);
  for i:=0 to Rule.NestedRuleCount-1 do
    Result[i]:=Rule.NestedRules[i];
end;

function CSSKeyframesName(aRule: TCSSAtRuleElement): TCSSString;
var
  El: TCSSElement;
begin
  Result:='';
  if aRule.SelectorCount=0 then
    exit; // a @keyframes without a name, e.g. '@keyframes {'
  El:=aRule.Selectors[0];
  // the name is an identifier or a string, both have a Value
  if not (El is TCSSBaseStringElement) then
    exit;
  Result:=TCSSBaseStringElement(El).Value;
end;

function CSSKeyframeOffsets(aKeyframe: TCSSRuleElement; out Offsets: TDoubleDynArray): boolean;
// e.g. "from, 25%, 50% { }"
var
  i, Cnt: Integer;
  El: TCSSElement;
  C: TClass;
  d: double;
begin
  Result:=false;
  Offsets:=nil;
  Cnt:=aKeyframe.SelectorCount;
  if Cnt=0 then
    exit; // a keyframe without a selector, e.g. '@keyframes fade{ {opacity:0} }'
  SetLength(Offsets,Cnt);
  for i:=0 to Cnt-1 do
  begin
    El:=aKeyframe.Selectors[i];
    C:=El.ClassType;
    if C.InheritsFrom(TCSSIdentifierElement) then
    begin
      // css keywords are ascii case insensitive
      case lowercase(TCSSIdentifierElement(El).Name) of
      'from': d:=0;
      'to': d:=1;
      else
        begin
          Offsets:=nil;
          exit;
        end;
      end;
    end
    else if C=TCSSIntegerElement then
    begin
      if TCSSIntegerElement(El).Units<>cuPercent then
      begin
        Offsets:=nil;
        exit;
      end;
      d:=double(TCSSIntegerElement(El).Value)/100;
    end
    else if C=TCSSFloatElement then
    begin
      if TCSSFloatElement(El).Units<>cuPercent then
      begin
        Offsets:=nil;
        exit;
      end;
      d:=TCSSFloatElement(El).Value/100;
    end
    else begin
      Offsets:=nil;
      exit;
    end;
    // css-animations-1 3: a selector outside 0%..100% is invalid.
    // Note: the parser only checks the unit, not the range.
    if (d<0) or (d>1) then
    begin
      Offsets:=nil;
      exit;
    end;
    Offsets[i]:=d;
  end;
  Result:=true;
end;

procedure FreeCSSKeyframes(var Keyframes: TCSSKeyframeArray);
var
  i: Integer;
begin
  for i:=0 to length(Keyframes)-1 do
    FreeAndNil(Keyframes[i].Values);
  Keyframes:=nil;
end;

// locate a rule in a candidate list: by index if its selectors still match (fast
// path, no rule added/deleted), else scan by selectors, else fall back to the
// stored index (a selector changed)
function CSSLocateRule(const Candidates: TCSSRuleElementArray; Idx: integer;
  const Sel: TCSSString): TCSSRuleElement;
var
  i: Integer;
begin
  Result:=nil;
  if (Idx>=0) and (Idx<length(Candidates))
      and (CSSRuleSelectorsStr(Candidates[Idx])=Sel) then
    exit(Candidates[Idx]);
  for i:=0 to length(Candidates)-1 do
    if CSSRuleSelectorsStr(Candidates[i])=Sel then
      exit(Candidates[i]);
  if (Idx>=0) and (Idx<length(Candidates)) then
    Result:=Candidates[Idx];
end;

{ TCSSResolverNthChildParams }

destructor TCSSResolverNthChildParams.Destroy;
var
  i: Integer;
begin
  for i:=0 to high(StackCache) do
    StackCache[i].Free;
  inherited Destroy;
end;

{ TCSSResolver.TCSSRuleBucket }

procedure TCSSResolver.TCSSRuleBucket.Add(aRule: TCSSRuleElement;
  aDocIndex: integer; aSrcSpecificity: TCSSSpecificity);
var
  l: SizeInt;
begin
  // a rule with multiple selectors landing in the same bucket is added only once
  if (Count>0) and (Items[Count-1].Rule=aRule) then exit;
  l:=length(Items);
  if Count=l then
  begin
    if l<4 then l:=4 else l:=l*2;
    SetLength(Items,l);
  end;
  Items[Count].Rule:=aRule;
  Items[Count].DocIndex:=aDocIndex;
  Items[Count].SourceSpecificity:=aSrcSpecificity;
  inc(Count);
end;

{ TCSSSharedRuleList }

destructor TCSSSharedRuleList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCSSSharedRuleList.Clear;
begin
  Rules:=nil;
end;

function TCSSSharedRuleList.Clone: TCSSSharedRuleList;
var
  l: SizeInt;
begin
  Result:=TCSSSharedRuleList.Create;
  Result.AllDecl:=AllDecl;
  Result.AllSpecificity:=AllSpecificity;

  l:=length(Rules);
  if l>0 then
  begin
    SetLength(Result.Rules,l);
    System.Move(Rules[0],Result.Rules[0],SizeOf(TCSSSharedRule)*l);
  end;

  l:=length(Values);
  if l>0 then
  begin
    SetLength(Result.Values,l);
    System.Move(Values[0],Result.Values[0],SizeOf(TCSSResolvedAttribute)*l);
  end;
end;

function TCSSSharedRuleList.IndexOfAttr(AttrId: TCSSNumericalID;
  ForInsert: boolean): integer;
var
  Cnt, l, r: Integer;
  CurAttrID: TCSSNumericalID;
begin
  Cnt:=length(Values);
  l:=0;
  r:=Cnt-1;
  while r>=l do
  begin
    Result:=(l+r) shr 1;
    CurAttrID:=Values[Result].AttrID;
    if CurAttrID>AttrId then
      l:=Result+1
    else if CurAttrID<AttrId then
      r:=Result-1
    else
      exit;
  end;
  if ForInsert then
    Result:=l
  else
    Result:=-1;
end;

{ TCSSAttributeValues }

procedure TCSSAttributeValues.SortValues;

  procedure QuickSort(L, R : integer);
  var
    I, J, PivotIdx : integer;
    AttrP: TCSSNumericalID;
    V: TCSSAttributeValue;
  begin
    repeat
      I := L;
      J := R;
      PivotIdx := L + ((R - L) shr 1); { same as ((L + R) div 2), but without the possibility of overflow }
      AttrP := Values[PivotIdx].AttrID;
      repeat
        while (I < PivotIdx) and (AttrP > Values[i].AttrID) do
          Inc(I);
        while (J > PivotIdx) and (AttrP < Values[J].AttrID) do
          Dec(J);
        if I < J then
        begin
          V := Values[I];
          Values[I] := Values[J];
          Values[J] := V;
          if PivotIdx = I then
          begin
            PivotIdx := J;
            Inc(I);
          end
          else if PivotIdx = J then
          begin
            PivotIdx := I;
            Dec(J);
          end
          else
          begin
            Inc(I);
            Dec(J);
          end;
        end;
      until I >= J;
      // sort the smaller range recursively
      // sort the bigger range via the loop
      // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
      if (PivotIdx - L) < (R - PivotIdx) then
      begin
        if (L + 1) < PivotIdx then
          QuickSort(L, PivotIdx - 1);
        L := PivotIdx + 1;
      end
      else
      begin
        if (PivotIdx + 1) < R then
          QuickSort(PivotIdx + 1, R);
        if (L + 1) < PivotIdx then
          R := PivotIdx - 1
        else
          exit;
      end;
    until L >= R;
  end;

var
  l: SizeInt;
  i, j: Integer;
  aValue: TCSSAttributeValue;
begin
  l:=length(Values);
  if l<6 then
  begin
    for i:=0 to l-2 do
      for j:=i+1 to l-1 do
        if Values[i].AttrID>Values[j].AttrID then
        begin
          aValue:=Values[i];
          Values[i]:=Values[j];
          Values[j]:=aValue;
        end;
  end else begin
    //for i:=0 to l-1 do
    //  writeln('TCSSAttributeValues.SortValues ',i,' ',Values[i]<>nil);
    QuickSort(0,l-1);
    for i:=0 to l-2 do
      if Values[i].AttrID>=Values[i+1].AttrID then
        raise ECSSResolver.Create('20240816160749');
  end;
end;

function TCSSAttributeValues.IndexOf(AttrID: TCSSNumericalID): integer;
var
  l, r, m: Integer;
  Diff: TCSSNumericalID;
begin
  l:=0;
  r:=length(Values)-1;
  while l<=r do
  begin
    m:=(l+r) shr 1;
    Diff:=Values[m].AttrID-AttrID;
    if Diff>0 then
      r:=m-1
    else if Diff<0 then
      l:=m+1
    else
      exit(m);
  end;
  Result:=-1;
end;

procedure TCSSAttributeValues.SetComputedValue(AttrID: TCSSNumericalID; const aTokens: TBytes);

  procedure AddNew;
  var
    Item: TCSSAttributeValue;
    i, l: integer;
  begin
    l:=length(Values);
    i:=l;
    while (i>0) and (Values[i-1].AttrID>AttrID) do dec(i);
    Item:=TCSSAttributeValue.Create;
    Item.AttrID:=AttrID;
    Item.Invalid:=false;
    Item.Tokens:=aTokens;
    System.Insert(Item,Values,i);
  end;

var
  i: Integer;
begin
  if AttrID<=CSSAttributeID_All then
    raise ECSSResolver.Create('20240729084610');
  if Values=nil then
  begin
    AddNew;
  end else begin
    i:=IndexOf(AttrID);
    if i>=0 then
    begin
      Values[i].Invalid:=false;
      Values[i].Tokens:=aTokens;
    end else begin
      AddNew;
    end;
  end;
end;

destructor TCSSAttributeValues.Destroy;
var
  i: Integer;
begin
  for i:=0 to length(Values)-1 do
    Values[i].Free;
  Values:=nil;
  inherited Destroy;
end;

{ TCSSResolver }

function TCSSResolver.GetLogCount: integer;
begin
  Result:=FLogEntries.Count;
end;

function TCSSResolver.GetCustomAttributes(Index: TCSSNumericalID): TCSSAttributeDesc;
begin
  Result:=FCustomAttributes[Index];
end;

function TCSSResolver.DoCSSWarn(Sender: TObject; Msg: TCSSString; aRow, aCol: integer): boolean;
// An invalid CSS syntax was found. According to the CSS syntax spec the invalid
// part is skipped and parsing continues. Log it as a warning instead of raising
// an exception, which would abort the whole stylesheet.
begin
  Result:=true;
  Log(etWarning,20260803114500,Msg+' at line '+IntToStr(aRow)
    +', column '+IntToStr(aCol),nil);
  if Sender=nil then ;
end;

function TCSSResolver.GetLogEntries(Index: integer): TCSSResolverLogEntry;
begin
  Result:=TCSSResolverLogEntry(FLogEntries[Index]);
end;

function TCSSResolver.GetStyleSheets(Index: integer): TStyleSheet;
begin
  Result:=FStyleSheets[Index];
end;

procedure TCSSResolver.SetOptions(const AValue: TCSSResolverOptions);
begin
  if FOptions=AValue then Exit;
  FOptions:=AValue;
end;

procedure TCSSResolver.ParseSource(Index: integer);

  procedure AddOrigin(LayerIndex: integer; Origin: TCSSOrigin);
  // inserts a anonymous layer
  var
    aLayer: TLayer;
  begin
    aLayer:=Default(TLayer);
    aLayer.Origin:=Origin;
    System.Insert(aLayer,FLayers,LayerIndex);
  end;

var
  Src: TCSSString;
  El: TCSSElement;
  LayerIndex: Integer;
  Cnt: SizeInt;
  aStyleSheet: TStyleSheet;
begin
  aStyleSheet:=FStyleSheets[Index];
  if aStyleSheet.Parsed then exit;
  aStyleSheet.Parsed:=true;
  if aStyleSheet.Element<>nil then
    raise ECSSResolver.Create('20240624200924');

  // parse
  Src:=aStyleSheet.Source;
  if Src='' then
    exit;
  //writeln('TCSSResolver.ParseSource [',Src,'] ',StringCodePage(Src));
  El:=ParseCSSSource(Src,false);
  if El=nil then exit;
  aStyleSheet.Element:=El;
  FRuleBucketsValid:=false; // FLayers about to change -> buckets need a rebuild

  // find last layer with this Origin or lower
  LayerIndex:=length(FLayers);
  while (LayerIndex>0) and (FLayers[LayerIndex-1].Origin>aStyleSheet.Origin) do
    dec(LayerIndex);
  if (LayerIndex=length(FLayers)) or (FLayers[LayerIndex].Origin<>aStyleSheet.Origin) then
    AddOrigin(LayerIndex,aStyleSheet.Origin);

  with FLayers[LayerIndex] do
  begin
    Cnt:=length(Elements);
    if Cnt=ElementCount then
    begin
      if Cnt<8 then
        Cnt:=8
      else
        Cnt:=Cnt*2;
      SetLength(Elements,Cnt);
    end;
    Elements[ElementCount].Src:=aStyleSheet;
    Elements[ElementCount].Element:=El;
    inc(ElementCount);
  end;
end;

function TCSSResolver.ParseCSSSource(const Src: TCSSString; Inline: boolean
  ): TCSSElement;
var
  ms: TMemoryStream;
  aParser: TCSSResolverParser;
  Bytes: RawByteString;
begin
  Result:=nil;
  if Src='' then
    exit;
  if CSSRegistry=nil then
    raise ECSSResolver.Create('20240630203634');

  if (FCSSRegistryStamp>0) then
  begin
    if (FCSSRegistryStamp<>CSSRegistry.Stamp) then
      raise ECSSResolver.Create('20240822143309 Clear was not called after changing CSSRegistry');
  end else
    FCSSRegistryStamp:=CSSRegistry.Stamp;

  aParser:=nil;
  ms:=TMemoryStream.Create;
  try
    // The scanner reads the stream as UTF-8, not as the code units of Src.
    Bytes:=CSSBytesOfString(Src);
    ms.Write(Bytes[1],length(Bytes));
    ms.Position:=0;
    aParser:=TCSSResolverParser.Create(ms); // stream is freed by the parser
    aParser.Resolver:=Self;
    aParser.OnLog:=@Log;
    aParser.Scanner.OnWarn:=@DoCSSWarn;
    aParser.CSSNthChildParamsClass:=TCSSResolverNthChildParams;
    aParser.CSSRuleDataClass:=TCSSRuleData;
    if Inline then
      Result:=aParser.ParseInline
    else
      Result:=aParser.Parse;
  finally
    aParser.Free;
    ms.Free;
  end;
end;

procedure TCSSResolver.ClearElements;
var
  i: Integer;
begin
  FLogEntries.Clear;

  ClearMerge;
  ClearSharedRuleLists;
  ClearRuleBuckets;
  ClearCustomAttributes;

  // clear layers
  for i:=0 to length(FLayers)-1 do
  begin
    FLayers[i].ElementCount:=0;
    FLayers[i].Elements:=nil;
    FLayers[i].Name:='';
  end;
  FLayers:=nil;

  // the rules are freed here, see the BumpSourceStamp in ReplaceStyleSheet
  BumpSourceStamp;
  for i:=0 to FStyleSheetCount-1 do
    FreeAndNil(FStyleSheets[i].Element);

  // class name ids are rebuilt while reparsing the selectors
  FCSSClassNameToID.Clear;
  FCSSClassNames:=nil;
  FCSSClassNameCount:=0;
  ChangeCSSClassIDStamp;

  // id name indices are rebuilt while reparsing the selectors
  FCSSIDNameToIndex.Clear;
  FCSSIDNames:=nil;
  FCSSIDCount:=0;

  // not referencing CSSRegistry anymore
  FCSSRegistryStamp:=0;
end;

procedure TCSSResolver.ClearCustomAttributes;
var
  i: Integer;
begin
  for i:=0 to FCustomAttributeCount-1 do
    FreeAndNil(FCustomAttributes[i]);
  FCustomAttributeCount:=0;
  FCustomAttributeNameToDesc.Clear;
end;

procedure TCSSResolver.AddRule(aRule: TCSSRuleElement; Specificity: TCSSSpecificity
  );
var
  l: SizeInt;
  i: Integer;
begin
  if aRule=nil then
    raise ECSSResolver.Create('20231110202417');
  l:=length(FElRules);
  if FElRuleCount=l then
  begin
    if l<8 then
      l:=8
    else
      l:=l*2;
    Setlength(FElRules,l);
  end;
  i:=FElRuleCount;
  FElRules[i].Rule:=aRule;
  FElRules[i].Specificity:=Specificity;
  inc(FElRuleCount);
end;

procedure TCSSResolver.ComputeElement(El: TCSSElement);
var
  C: TClass;
  Compound: TCSSCompoundElement;
  i: Integer;
begin
  if El=nil then exit;
  C:=El.ClassType;
  {$IFDEF VerboseCSSResolver}
  //writeln('TCSSResolver.ComputeElement ',GetCSSPath(El));
  {$ENDIF}
  if C=TCSSCompoundElement then
  begin
    Compound:=TCSSCompoundElement(El);
    //writeln('TCSSResolver.ComputeElement Compound.ChildCount=',Compound.ChildCount);
    for i:=0 to Compound.ChildCount-1 do
      ComputeElement(Compound.Children[i]);
  end else if CSSIsPlainRule(C) then
    ComputeRule(TCSSRuleElement(El))
  else if C.InheritsFrom(TCSSAtRuleElement) then
    ComputeAtRule(TCSSAtRuleElement(El))
  else
    Log(etWarning,20220908150252,'TCSSResolver.ComputeElement: Unknown CSS element',El);
end;

function TCSSResolver.GetRuleSourceSpecificity(aRule: TCSSRuleElement
  ): TCSSSpecificity;
var
  RData: TCSSRuleData;
begin
  Result:=0;
  if aRule=nil then exit;
  // BuildRuleBuckets stored the origin in the data of every rule of every
  // stylesheet, so no search is needed
  UpdateRuleBuckets;
  RData:=RuleData(aRule);
  if RData=nil then exit; // a rule of another parser, so of no stylesheet of Self
  // an old stamp means aRule is in none of the stylesheets, e.g. an element style
  if RData.SourceStamp=FSourceStamp then
    Result:=CSSOriginToSpecifity[RData.Origin];
end;

function TCSSResolver.GetRuleStyleSheet(aRule: TCSSRuleElement): TStyleSheet;
var
  RData: TCSSRuleData;
begin
  Result:=nil;
  if aRule=nil then exit;
  // BuildRuleBuckets stored the stylesheet index in the data of every rule
  UpdateRuleBuckets;
  RData:=RuleData(aRule);
  if RData=nil then exit; // a rule of another parser, so of no stylesheet of Self
  // an old stamp means aRule is in none of the stylesheets, e.g. an element style
  if (RData.SourceStamp=FSourceStamp)
      and (RData.SourceIndex>=0) and (RData.SourceIndex<FStyleSheetCount) then
    Result:=FStyleSheets[RData.SourceIndex];
end;

function TCSSResolver.GetRuleSpecificity(aRule: TCSSRuleElement;
  const aNode: ICSSNode): TCSSSpecificity;
var
  i: Integer;
  Specificity: TCSSSpecificity;
  SavedNode: ICSSNode;
begin
  Result:=CSSSpecificityNoMatch;
  if (aRule=nil) or (aNode=nil) then exit;
  // SelectorMatches uses FNode for the nested/combinator context (see ComputeRule)
  SavedNode:=FNode;
  try
    FNode:=aNode;
    for i:=0 to aRule.SelectorCount-1 do
    begin
      Specificity:=SelectorMatches(aRule.Selectors[i],aNode,false,aRule);
      if Specificity>Result then
        Result:=Specificity;
    end;
  finally
    FNode:=SavedNode;
  end;
  if Result>=0 then
    inc(Result,GetRuleSourceSpecificity(aRule));
end;

procedure TCSSResolver.ComputeRule(aRule: TCSSRuleElement);
var
  i: Integer;
  BestSpecificity, Specificity: TCSSSpecificity;
  aSelector: TCSSElement;
  NestedRule: TCSSRuleElement;
  C: TClass;
begin
  BestSpecificity:=CSSSpecificityNoMatch;

  for i:=0 to aRule.SelectorCount-1 do
  begin
    aSelector:=aRule.Selectors[i];
    Specificity:=SelectorMatches(aSelector,FNode,false,aRule);
    //writeln('TCSSResolver.ComputeRule ',i,' ',Fnode.GetCSSID,' ',aSelector.ClassName,' ',Specificity);
    if Specificity>BestSpecificity then
      BestSpecificity:=Specificity;
  end;

  if BestSpecificity>=0 then
  begin
    inc(BestSpecificity,FSourceSpecificity);
    // match -> add rule to ruleset
    AddRule(aRule,BestSpecificity);
  end;

  for i:=0 to aRule.NestedRuleCount-1 do
  begin
    NestedRule:=aRule.NestedRules[i];
    C:=NestedRule.ClassType;
    if C.InheritsFrom(TCSSAtRuleElement) then
      // Note: even when aRule does not match, the rules nested in the @-rule can
      // match, e.g. the '.red' of 'div{ @media screen{ .red{} } }' means 'div .red'.
      ComputeAtRule(TCSSAtRuleElement(NestedRule),BestSpecificity)
    else
      ComputeRule(NestedRule);
  end;
end;

procedure TCSSResolver.ComputeAtRule(aRule: TCSSAtRuleElement;
  ParentSpecificity: TCSSSpecificity);
var
  i: Integer;
  C: TClass;
  NestedRule: TCSSRuleElement;
begin
  if CSSIsKeyframesAtKeyword(aRule.AtKeyWord) then
    // @keyframes never contributes to the cascade, see FindKeyframesRule
    exit;

  case aRule.AtKeyWord of
  '@media':
    if not AtMediaMatches(aRule) then
      exit; // the media query does not match -> nothing inside applies
  '@starting-style':
    // only used by ComputeStartingStyle, never by the normal cascade
    exit;
  else
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20260322092255,'Unknown CSS rule @'+aRule.AtKeyWord,aRule);
    {$ENDIF}
    exit;
  end;

  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.ComputeAtRule ',FNode.GetCSSID,' ',ParentSpecificity);
  {$ENDIF}

  // The declarations written directly in the @-rule belong to the enclosing style
  // rule, so they need that rule to match and they take its specificity.
  if (aRule.ChildCount>0) and (ParentSpecificity>=0) then
    AddRule(aRule,ParentSpecificity);

  // The nested rules have their own selectors, which can address an ancestor or a
  // sibling via a combinator, so they must be checked for every node - no matter
  // whether the enclosing style rule matches.
  for i:=0 to aRule.NestedRuleCount-1 do
  begin
    NestedRule:=aRule.NestedRules[i];
    C:=NestedRule.ClassType;
    if C.InheritsFrom(TCSSAtRuleElement) then
      ComputeAtRule(TCSSAtRuleElement(NestedRule),ParentSpecificity)
    else
      ComputeRule(NestedRule);
  end;
end;

function TCSSResolver.AtMediaMatches(aRule: TCSSAtRuleElement): boolean;
// true if one of the media queries of aRule matches.
// The media environment (see the HasMediaBoolean/IsMediaPlain/MediaCompare events)
// changes rarely, while this is called for every node, so the result is cached in
// the rule's TCSSRuleData and only recomputed after InvalidateMedia.
var
  RData: TCSSRuleData;
  i: Integer;
begin
  RData:=RuleData(aRule);
  if (RData<>nil) and (RData.MediaStamp=FMediaStamp) then
    exit(RData.MediaResult);

  Result:=false;
  for i:=0 to aRule.SelectorCount-1 do
    if MediaSelectorMatches(aRule.Selectors[i]) then
    begin
      Result:=true;
      break;
    end;

  // RData=nil: the rule was created by a parser without CSSRuleDataClass -> no cache
  if RData<>nil then
  begin
    RData.MediaResult:=Result;
    RData.MediaStamp:=FMediaStamp;
  end;
end;

function TCSSResolver.StartingStyleContextMatches(aRule: TCSSAtRuleElement; out
  Specificity: TCSSSpecificity): boolean;
// Check the ancestors of a @starting-style rule for the current FNode.
// Result=false: an enclosing @media does not match, or an unsupported @-rule (e.g.
//   @supports) encloses aRule -> nothing inside applies. The normal cascade drops
//   those as well, see ComputeAtRule.
// Specificity: the specificity of the innermost enclosing style rule for FNode, or
//   CSSSpecificityNoMatch when there is none or it does not match FNode. Only the
//   declarations directly in the @starting-style need it; its nested rules are
//   matched by their own selectors, which can address an ancestor or a sibling.
//   SelectorMatches resolves the whole outer nesting chain of the enclosing rule,
//   so the outer style rules need no extra check.
var
  El: TCSSElement;
  C: TClass;
  HasStyleRule: boolean;
begin
  Result:=false;
  Specificity:=CSSSpecificityNoMatch;
  HasStyleRule:=false;
  El:=aRule.Parent;
  while El<>nil do
  begin
    C:=El.ClassType;
    if C.InheritsFrom(TCSSAtRuleElement) then
    begin
      case TCSSAtRuleElement(El).AtKeyWord of
      '@media':
        if not AtMediaMatches(TCSSAtRuleElement(El)) then exit;
      '@starting-style': ; // nested @starting-style: no extra condition
      else
        exit; // unknown @-rule, e.g. @supports -> dropped, like in ComputeAtRule
      end;
    end
    else if CSSIsPlainRule(C) and not HasStyleRule then
    begin
      Specificity:=GetRuleSpecificity(TCSSRuleElement(El),FNode);
      HasStyleRule:=true;
      // keep walking: an outer @media still has to match
    end;
    El:=El.Parent;
  end;
  Result:=true;
end;

procedure TCSSResolver.ComputeStartingStyleRule(aRule: TCSSAtRuleElement;
  SrcSpecificity: TCSSSpecificity);
// Add the rules of one @starting-style at-rule matching FNode to FElRules.
var
  Specificity: TCSSSpecificity;
  i: Integer;
  NestedRule: TCSSRuleElement;
begin
  FSourceSpecificity:=SrcSpecificity;
  if not StartingStyleContextMatches(aRule,Specificity) then exit;

  // Declarations directly in the @starting-style, e.g. div{ @starting-style{ width:1px } }.
  // They belong to the enclosing style rule, so they need it to match.
  if (aRule.ChildCount>0) and (Specificity>=0) then
    AddRule(aRule,Specificity);

  // Nested rules, e.g. @starting-style{ div{ width:1px } }. They have their own
  // selectors, which can address an ancestor or a sibling via a combinator, e.g.
  // the '.red' of 'div{ @starting-style{ .red{} } }' means 'div .red' - so they are
  // checked no matter whether the enclosing style rule matches.
  for i:=0 to aRule.NestedRuleCount-1 do
  begin
    NestedRule:=aRule.NestedRules[i];
    if NestedRule.ClassType=TCSSAtRuleElement then
      // e.g. @starting-style{ @media screen{ div{...} } }
      // Note: a nested @starting-style is ignored, same as in a browser.
      ComputeAtRule(TCSSAtRuleElement(NestedRule),Specificity)
    else
      ComputeRule(NestedRule);
  end;
end;

function TCSSResolver.ComputeNestedRuleSelectorSpecifity(aSelector: TCSSElement): TCSSSpecificity;
var
  aParentRule: TCSSRuleElement;
  ParentSpecificity, i: Integer;
  Spec: TCSSSpecificity;
begin
  Result:=SelectorMatches(aSelector,nil,true,nil);
  if Result<0 then exit;
  aParentRule:=GetRuleParentOfSelector(aSelector,true);
  if aParentRule=nil then
    exit(CSSSpecificityInvalid);

  // parent specificity = max of parent selectors (like :is())
  ParentSpecificity:=CSSSpecificityNoMatch;
  for i:=0 to aParentRule.SelectorCount-1 do
  begin
    Spec:=SelectorMatches(aParentRule.Selectors[i],nil,true,aParentRule);
    if Spec>ParentSpecificity then
      ParentSpecificity:=Spec;
  end;
  inc(Result,ParentSpecificity);
end;

function TCSSResolver.GetRuleOfSelector(aSelector: TCSSElement): TCSSRuleElement;
begin
  Result:=nil;
  if aSelector=nil then exit;
  repeat
    aSelector:=aSelector.Parent;
    if aSelector=nil then exit;
    if aSelector is TCSSRuleElement then
      exit(TCSSRuleElement(aSelector));
  until false;
end;

function TCSSResolver.GetRuleParentOfSelector(aSelector: TCSSElement; SkipAtRules: boolean
  ): TCSSRuleElement;
var
  aRule: TCSSRuleElement;
  aParent: TCSSElement;
  C: TClass;
begin
  Result:=nil;
  aRule:=GetRuleOfSelector(aSelector);
  if aRule=nil then exit;
  // skip @-rules
  aParent:=aRule.Parent;
  while (aParent<>nil) do
  begin
    C:=aParent.ClassType;
    if CSSIsPlainRule(C) then
      exit(TCSSRuleElement(aParent));
    if not SkipAtRules and C.InheritsFrom(TCSSAtRuleElement) then
      exit(TCSSRuleElement(aParent));
    aParent:=aParent.Parent;
  end;
end;

function TCSSResolver.MediaSelectorIdentifierMatches(aIdentifier: TCSSResolvedIdentifierElement
  ): boolean;
var
  KW: TCSSNumericalID;
begin
  KW:=aIdentifier.NumericalID;
  {$IFDEF VerboseCSSResolver}
  if KW>0 then
    writeln('TCSSResolver.MediaSelectorIdentifierMatches ',aIdentifier.Value,' KW=',CSSRegistry.Keywords[KW])
  else
    writeln('TCSSResolver.MediaSelectorIdentifierMatches ',aIdentifier.Value,' unknown');
  {$ENDIF}
  Result:=Assigned(HasMediaBoolean) and HasMediaBoolean(Self,KW);
end;

function TCSSResolver.MediaSelectorBinaryMatches(aBinary: TCSSBinaryElement): boolean;

  function GetCompValue(El: TCSSElement; out aValue: TCSSResCompValue): boolean;
  var
    FloatEl: TCSSFloatElement;
    IntEl: TCSSIntegerElement;
    Ratio: TCSSBinaryElement;
    Num, Den: Double;
  begin
    Result:=true;
    aValue:=Default(TCSSResCompValue);
    if El is TCSSResolvedIdentifierElement then
    begin
      aValue.Kind:=rvkKeyword;
      aValue.KeywordID:=TCSSResolvedIdentifierElement(El).NumericalID;
    end else if El is TCSSFloatElement then
    begin
      FloatEl:=TCSSFloatElement(El);
      aValue.Kind:=rvkFloat;
      aValue.Float:=FloatEl.Value;
      aValue.FloatUnit:=FloatEl.Units;
    end else if El is TCSSIntegerElement then
    begin
      IntEl:=TCSSIntegerElement(El);
      aValue.Kind:=rvkFloat;
      aValue.Float:=IntEl.Value;
      aValue.FloatUnit:=IntEl.Units;
    end else if (El is TCSSBinaryElement)
        and (TCSSBinaryElement(El).Operation=boDIV) then
    begin
      // ratio value N/M, e.g. 3/2
      Ratio:=TCSSBinaryElement(El);
      if Ratio.Left is TCSSIntegerElement then
        Num:=TCSSIntegerElement(Ratio.Left).Value
      else if Ratio.Left is TCSSFloatElement then
        Num:=TCSSFloatElement(Ratio.Left).Value
      else
        exit(false);
      if Ratio.Right is TCSSIntegerElement then
        Den:=TCSSIntegerElement(Ratio.Right).Value
      else if Ratio.Right is TCSSFloatElement then
        Den:=TCSSFloatElement(Ratio.Right).Value
      else
        exit(false);
      if SameValue(Den,0) then
        exit(false);
      aValue.Kind:=rvkFloat;
      aValue.Float:=Num/Den;
      aValue.FloatUnit:=cuNone;
    end else
      Result:=false;
  end;

  // RangeCmpMatches: compare KW against aValue using Op
  // Cmp: 0=equal, 1=KW bigger, -1=value bigger
  // ValueOnLeft=true: operation is written as "value Op name", so flip Cmp
  function RangeCmpMatches(KW: TCSSNumericalID; const aValue: TCSSResCompValue;
    Op: TCSSBinaryOperation; ValueOnLeft: boolean): boolean;
  var
    Cmp: integer;
  begin
    Result:=false;
    if not Assigned(MediaCompare) or not MediaCompare(Self,KW,aValue,Cmp) then exit;
    if ValueOnLeft then Cmp:=-Cmp;
    case Op of
    boEquals: Result:=Cmp=0;
    boGT:     Result:=Cmp>0;
    boGE:     Result:=Cmp>=0;
    boLT:     Result:=Cmp<0;
    boLE:     Result:=Cmp<=0;
    end;
  end;

var
  KW: TCSSNumericalID;
  aValue, aValue2: TCSSResCompValue;
  LeftBin: TCSSBinaryElement;
begin
  Result:=false;
  if aBinary.Left is TCSSBinaryElement then
  begin
    // interval: value1 op1 name op2 value2, e.g. (100px <= width < 1000px)
    // left binary: value1 op1 name (value on left)
    // outer operation: name op2 value2 (name on left)
    LeftBin:=TCSSBinaryElement(aBinary.Left);
    if not (LeftBin.Right is TCSSResolvedIdentifierElement) then exit;
    KW:=TCSSResolvedIdentifierElement(LeftBin.Right).NumericalID;
    if KW<=0 then exit;
    if not GetCompValue(LeftBin.Left,aValue) then exit;   // value1
    if not GetCompValue(aBinary.Right,aValue2) then exit; // value2
    // check both bounds; inner is value-on-left, outer is name-on-left
    if not RangeCmpMatches(KW,aValue,LeftBin.Operation,true) then exit;
    Result:=RangeCmpMatches(KW,aValue2,aBinary.Operation,false);
  end
  else if aBinary.Left is TCSSResolvedIdentifierElement then
  begin
    // name op value: (width > 400px)
    KW:=TCSSResolvedIdentifierElement(aBinary.Left).NumericalID;
    if KW<=0 then exit;
    if not GetCompValue(aBinary.Right,aValue) then exit;
    case aBinary.Operation of
    boColon:
      // plain name:value, e.g. (orientation: portrait)
      Result:=Assigned(IsMediaPlain) and IsMediaPlain(Self,KW,aValue);
    boEquals,boLT,boLE,boGT,boGE:
      Result:=RangeCmpMatches(KW,aValue,aBinary.Operation,false);
    end;
  end
  else if aBinary.Right is TCSSResolvedIdentifierElement then
  begin
    // value op name: (400px < width)
    KW:=TCSSResolvedIdentifierElement(aBinary.Right).NumericalID;
    if KW<=0 then exit;
    if not GetCompValue(aBinary.Left,aValue) then exit;
    case aBinary.Operation of
    boEquals,boLT,boLE,boGT,boGE:
      Result:=RangeCmpMatches(KW,aValue,aBinary.Operation,true);
    end;
  end;
end;

function TCSSResolver.MediaSelectorMatches(aSelector: TCSSElement): boolean;
var
  C: TClass;
begin
  // Note: if this is a nested rule: the parent rule was already checked if it matches

  Result:=false;
  C:=aSelector.ClassType;
  if C=TCSSResolvedIdentifierElement then
    Result:=MediaSelectorIdentifierMatches(TCSSResolvedIdentifierElement(aSelector))
  else if C=TCSSListElement then
    Result:=MediaSelectorListMatches(TCSSListElement(aSelector))
  else if C=TCSSBinaryElement then
    Result:=MediaSelectorBinaryMatches(TCSSBinaryElement(aSelector))
  else begin
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20260322092226,'Unknown CSS media selector element '+aSelector.ClassName,aSelector);
    {$ENDIF}
  end;
end;

function TCSSResolver.MediaSelectorListMatches(aList: TCSSListElement): boolean;
var
  i: Integer;
  El: TCSSElement;
  KW: TCSSNumericalID;
  IsOr: boolean;
begin
  Result:=false;
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.MediaSelectorListMatches ChildCount=',aList.ChildCount);
  {$ENDIF}

  // 'not' list: [not, condition] -> match if condition does NOT match
  if (aList.ChildCount=2) and (aList.Children[0] is TCSSResolvedIdentifierElement) and
      (TCSSResolvedIdentifierElement(aList.Children[0]).NumericalID=CSSKeywordNot) then
    exit(not MediaSelectorMatches(aList.Children[1]));

  // detect connector: 'and' or 'or' (check first connector found)
  IsOr:=false;
  for i:=0 to aList.ChildCount-1 do
  begin
    El:=aList.Children[i];
    if El is TCSSResolvedIdentifierElement then
    begin
      KW:=TCSSResolvedIdentifierElement(El).NumericalID;
      if KW=CSSKeywordOr then
      begin
        IsOr:=true;
        break;
      end else if KW=CSSKeywordAnd then
        break;
    end;
  end;

  if IsOr then
  begin
    // OR: match if any condition matches
    for i:=0 to aList.ChildCount-1 do
    begin
      El:=aList.Children[i];
      if (El is TCSSResolvedIdentifierElement) and
          (TCSSResolvedIdentifierElement(El).NumericalID=CSSKeywordOr) then
        continue;
      if MediaSelectorMatches(El) then
        exit(true);
    end;
  end else
  begin
    // AND: all conditions must match; skip 'and' connectors
    Result:=true;
    for i:=0 to aList.ChildCount-1 do
    begin
      El:=aList.Children[i];
      {$IFDEF VerboseCSSResolver}
      writeln('TCSSResolver.MediaSelectorListMatches ',i,' ',GetCSSObj(El),' AsString=',El.AsString);
      {$ENDIF}
      if (El is TCSSResolvedIdentifierElement) and
          (TCSSResolvedIdentifierElement(El).NumericalID=CSSKeywordAnd) then
        continue;
      if not MediaSelectorMatches(El) then
        exit(false);
    end;
  end;
end;

function TCSSResolver.FindSharedRuleList(const Rules: TCSSSharedRuleArray
  ): TCSSSharedRuleList;
var
  Node: TAVLTreeNode;
begin
  Node:=FSharedRuleLists.FindKey(Pointer(Rules),@CompareRulesArrayWithCSSSharedRuleList);
  if Node<>nil then
    Result:=TCSSSharedRuleList(Node.Data)
  else
    Result:=nil;
end;

function TCSSResolver.CreateSharedRuleList: TCSSSharedRuleList;
var
  i, j: Integer;
  RuleArr: TCSSSharedRule;
  Rule: TCSSRuleElement;
  Specificity: TCSSSpecificity;
begin
  SetLength(FElRules,FElRuleCount); // needed by FindSharedRuleList

  // Sort ascending for Specificity, keeping equal-specificity rules in their
  // original (document) order so the cascade tie-break is "last declaration wins".
  // FElRules arrives in document order (ComputeRule is called in DocIndex order),
  // so a STABLE sort is required: a plain selection/bubble sort that swaps on '>'
  // is not stable and would move the first of an equal-specificity pair behind the
  // second whenever a lower-specificity rule follows them, reversing source order.
  // Insertion sort with a strict '>' comparison is stable and cheap for the small
  // per-element rule counts.
  for i:=1 to FElRuleCount-1 do
  begin
    Rule:=FElRules[i].Rule;
    Specificity:=FElRules[i].Specificity;
    j:=i-1;
    while (j>=0) and (FElRules[j].Specificity>Specificity) do
    begin
      FElRules[j+1]:=FElRules[j];
      dec(j);
    end;
    FElRules[j+1].Rule:=Rule;
    FElRules[j+1].Specificity:=Specificity;
  end;

  Result:=FindSharedRuleList(FElRules);
  if Result<>nil then
  begin
    // already exists
    LoadSharedMergedAttributes(Result);
  end else begin
    // add new shared rule list
    Result:=TCSSSharedRuleList.Create;
    Result.Rules:=copy(FElRules,0,FElRuleCount);
    FSharedRuleLists.Add(Result);

    // merge shared properties
    ClearMerge;
    for i:=0 to length(Result.Rules)-1 do
    begin
      RuleArr:=Result.Rules[i];
      Rule:=RuleArr.Rule;
      Specificity:=RuleArr.Specificity;
      for j:=0 to Rule.ChildCount-1 do
        MergeAttribute(Rule.Children[j],Specificity);
    end;
    SaveSharedMergedAttributes(Result);
  end;
end;

procedure TCSSResolver.ClearMerge;
var
  i: Integer;
begin
  if FMergedAttributesStamp=high(FMergedAttributesStamp) then
  begin
    FMergedAttributesStamp:=1;
    for i:=0 to length(FMergedAttributes)-1 do
      FMergedAttributes[i].Stamp:=0;
  end else
    inc(FMergedAttributesStamp);
  FMergedAllDecl:=nil;
  FMergedAllSpecificity:=CSSSpecificityInvalid;
  FMergedAttributeFirst:=0;
  FMergedAttributeLast:=0;
end;

procedure TCSSResolver.InitMerge;
var
  OldLen, NewLen: TCSSNumericalID;
begin
  if FCustomAttributeCount>0 then
  begin
    if FCustomAttributes[0].Index<>CSSRegistry.AttributeCount then
      raise ECSSResolver.Create('20240822142652');
  end;

  OldLen:=length(FMergedAttributes);
  NewLen:=CSSRegistry.AttributeCount+FCustomAttributeCount;
  if NewLen>OldLen then
  begin
    SetLength(FMergedAttributes,NewLen);
  end;
end;

procedure TCSSResolver.SetMergedAttribute(AttrID, aSpecificity: TCSSNumericalID;
  DeclEl: TCSSDeclarationElement);
var
  AttrP: PMergedAttribute;
begin
  if AttrID<=0 then
    raise ECSSResolver.Create('20240701120038');
  if AttrID>=length(FMergedAttributes) then
    raise ECSSResolver.Create('20240823095544');

  AttrP:=@FMergedAttributes[AttrID];
  AttrP^.Specificity:=aSpecificity;
  AttrP^.DeclEl:=DeclEl;
  if AttrP^.Stamp<>FMergedAttributesStamp then
  begin
    if FMergedAttributeFirst>0 then
    begin
      // append to double linked list
      FMergedAttributes[FMergedAttributeLast].Next:=AttrID;
      AttrP^.Prev:=FMergedAttributeLast;
      FMergedAttributeLast:=AttrID;
    end else begin
      // start double linked list
      FMergedAttributeFirst:=AttrID;
      FMergedAttributeLast:=AttrID;
      AttrP^.Prev:=0;
    end;
    AttrP^.Next:=0;
    AttrP^.Stamp:=FMergedAttributesStamp;
  end;
end;

procedure TCSSResolver.RemoveMergedAttribute(AttrID: TCSSNumericalID);
var
  AttrP: PMergedAttribute;
begin
  AttrP:=@FMergedAttributes[AttrID];
  if AttrP^.Stamp<>FMergedAttributesStamp then exit;
  AttrP^.Stamp:=0;
  if FMergedAttributeFirst=AttrID then
    FMergedAttributeFirst:=AttrP^.Next;
  if FMergedAttributeLast=AttrID then
    FMergedAttributeLast:=AttrP^.Prev;
  if AttrP^.Next>0 then
    FMergedAttributes[AttrP^.Next].Prev:=AttrP^.Prev;
  if AttrP^.Prev>0 then
    FMergedAttributes[AttrP^.Prev].Next:=AttrP^.Next;
  AttrP^.Next:=0;
  AttrP^.Prev:=0;
end;

function TCSSResolver.SelectorMatches(aSelector: TCSSElement; const TestNode: ICSSNode;
  OnlySpecificity: boolean; aRule: TCSSRuleElement): TCSSSpecificity;
var
  C: TClass;
  aBinary: TCSSBinaryElement;
  aUnary: TCSSUnaryElement;
  aList: TCSSListElement;
begin
  Result:=CSSSpecificityInvalid;

  if (aRule<>nil) and (GetRuleParentOfSelector(aSelector,true)<>nil) then
  begin
    // nested rule

    if OnlySpecificity then
    begin
      Result:=ComputeNestedRuleSelectorSpecifity(aRule);
      exit;
    end;

    if (aSelector is TCSSBinaryElement) then
    begin
      aBinary:=TCSSBinaryElement(aSelector);
      if (aBinary.Left is TCSSIdentifierElement)
          and (TCSSIdentifierElement(aBinary.Left).Value='&') then
      begin
        case aBinary.Operation of
        boWhiteSpace:
          begin
            // nested rule with "& <descendant>" -> descendant combinator with & as parent selector:
            // right side must match TestNode AND an ancestor must match the parent rule.
            Result:=SelectorAndWhitespaceMatches(aBinary.Right,TestNode);
            exit;
          end;
        boGT:
          begin
            // nested rule with "& > <child>" -> child combinator with & as parent selector:
            // right side must match TestNode AND the direct parent must match the parent rule.
            Result:=SelectorAndGTMatches(aBinary.Right,TestNode);
            exit;
          end;
        boPlus:
          begin
            // nested rule with "& + <sibling>" -> adjacent sibling combinator with & as parent selector:
            // right side must match TestNode AND the previous sibling must match the parent rule.
            Result:=SelectorAndPlusMatches(aBinary.Right,TestNode);
            exit;
          end;
        boTilde:
          begin
            // nested rule with "& ~ <sibling>" -> general sibling combinator with & as parent selector:
            // right side must match TestNode AND a preceding sibling must match the parent rule.
            Result:=SelectorAndTildeMatches(aBinary.Right,TestNode);
            exit;
          end;
        end;
      end else if (aBinary.Operation=boWhiteSpace)
          and (aBinary.Right is TCSSIdentifierElement)
          and (TCSSIdentifierElement(aBinary.Right).Value='&') then
      begin
        // nested rule with "<selector> &" -> & is the subject, must be descendant of <selector>:
        // TestNode must match the parent rule AND have an ancestor matching the left selector.
        Result:=SelectorAndRightAndMatches(aBinary,TestNode);
        exit;
      end;
    end else if (aSelector is TCSSUnaryElement) then
    begin
      aUnary:=TCSSUnaryElement(aSelector);
      case aUnary.Operation of
      uoGT:
        begin
          // nested rule with "> <selector>" -> child combinator with implicit &:
          // right side must match TestNode AND the direct parent must match the parent rule.
          Result:=SelectorAndGTMatches(aUnary.Right,TestNode);
          exit;
        end;
      uoPlus:
        begin
          // nested rule with "+ <selector>" -> adjacent sibling combinator with implicit &:
          // right side must match TestNode AND the previous sibling must match the parent rule.
          Result:=SelectorAndPlusMatches(aUnary.Right,TestNode);
          exit;
        end;
      uoTilde:
        begin
          // nested rule with "~ <selector>" -> general sibling combinator with implicit &:
          // right side must match TestNode AND a preceding sibling must match the parent rule.
          Result:=SelectorAndTildeMatches(aUnary.Right,TestNode);
          exit;
        end;
      end;
    end else if (aSelector is TCSSListElement) then
    begin
      aList:=TCSSListElement(aSelector);
      if (aList.ChildCount>0)
          and (aList.Children[0] is TCSSIdentifierElement)
          and (TCSSIdentifierElement(aList.Children[0]).Value='&') then
      begin
        // nested rule with "&<selector>" -> compound: TestNode must match
        // both the parent rule's selectors and the remaining selectors.
        Result:=SelectorAndCompoundMatches(aList,TestNode);
        exit;
      end;
    end;
    // nested rule without & -> descendant combinator:
    // own selector must match TestNode AND an ancestor must match the parent rule.
    // Parent rule specificity is like :is() = max of its selectors' specificities.
    // match own selector without nested context
    Result:=SelectorAndWhitespaceMatches(aSelector,TestNode);
    exit;
  end;

  //writeln('TCSSResolver.SelectorMatches ',aSelector.ClassName,' ',TestNode.GetCSSTypeName);
  C:=aSelector.ClassType;
  if C=TCSSResolvedIdentifierElement then
    Result:=SelectorIdentifierMatches(TCSSResolvedIdentifierElement(aSelector),TestNode,OnlySpecificity)
  else if C=TCSSResolvedHashIdentifierElement then
    Result:=SelectorHashIdentifierMatches(TCSSResolvedHashIdentifierElement(aSelector),TestNode,OnlySpecificity)
  else if C=TCSSResolvedClassNameElement then
    Result:=SelectorClassNameMatches(TCSSResolvedClassNameElement(aSelector),TestNode,OnlySpecificity)
  else if C=TCSSResolvedPseudoClassElement then
    Result:=SelectorPseudoClassMatches(TCSSResolvedPseudoClassElement(aSelector),TestNode,OnlySpecificity)
  else if C=TCSSUnaryElement then
    Result:=SelectorUnaryMatches(TCSSUnaryElement(aSelector),TestNode,OnlySpecificity)
  else if C=TCSSBinaryElement then
    Result:=SelectorBinaryMatches(TCSSBinaryElement(aSelector),TestNode,OnlySpecificity)
  else if C=TCSSArrayElement then
    Result:=SelectorArrayMatches(TCSSArrayElement(aSelector),TestNode,OnlySpecificity)
  else if C=TCSSListElement then
    Result:=SelectorListMatches(TCSSListElement(aSelector),TestNode,OnlySpecificity)
  else if C=TCSSResolvedCallElement then
    Result:=SelectorCallMatches(TCSSResolvedCallElement(aSelector),TestNode,OnlySpecificity)
  else begin
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20240625131810,'Unknown CSS selector element '+aSelector.ClassName,aSelector);
    {$ENDIF}
  end;
end;

function TCSSResolver.SelectorIdentifierMatches(
  aIdentifier: TCSSResolvedIdentifierElement; const TestNode: ICSSNode;
  OnlySpecificity: boolean): TCSSSpecificity;
var
  TypeID: TCSSNumericalID;
begin
  Result:=CSSSpecificityNoMatch;
  TypeID:=aIdentifier.NumericalID;
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.SelectorIdentifierMatches ',aIdentifier.Value,' TypeId=',TypeID,' Node=',TestNode.GetCSSTypeID);
  {$ENDIF}
  if TypeID=CSSTypeID_Universal then
    // universal selector
    Result:=CSSSpecificityUniversal
  else if OnlySpecificity then
    Result:=CSSSpecificityType
  else if TypeID=CSSIDNone then
  begin
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20240625153922,'Unknown type ',aIdentifier);
    {$ENDIF}
    Result:=CSSSpecificityInvalid;
  end else if TypeID=TestNode.GetCSSTypeID then
    Result:=CSSSpecificityType;
end;

function TCSSResolver.SelectorAndWhitespaceMatches(aRightSelector: TCSSElement;
  const TestNode: ICSSNode): TCSSSpecificity;
var
  aParentRule: TCSSRuleElement;
  ParentSpecificity: TCSSSpecificity;
  i: Integer;
  aParent: ICSSNode;
begin
  Result:=SelectorMatches(aRightSelector,TestNode,false,nil);
  if Result<0 then exit;
  aParentRule:=GetRuleParentOfSelector(aRightSelector,true);
  if aParentRule=nil then
    exit(CSSSpecificityInvalid);

  // find ancestor element matching any of the css parent rule's selectors
  aParent:=TestNode.GetCSSParent;
  while aParent<>nil do
  begin
    for i:=0 to aParentRule.SelectorCount-1 do
    begin
      ParentSpecificity:=SelectorMatches(aParentRule.Selectors[i],aParent,false,aParentRule);
      if ParentSpecificity=CSSSpecificityInvalid then
        exit(CSSSpecificityInvalid);
      if ParentSpecificity>=0 then
      begin
        inc(Result,ParentSpecificity);
        exit;
      end;
    end;
    aParent:=aParent.GetCSSParent;
  end;
  Result:=CSSSpecificityNoMatch;
end;

function TCSSResolver.SelectorAndGTMatches(aRightSelector: TCSSElement; const TestNode: ICSSNode
  ): TCSSSpecificity;
var
  aParentRule: TCSSRuleElement;
  ParentSpecificity: TCSSSpecificity;
  i: Integer;
  aParent: ICSSNode;
begin
  Result:=SelectorMatches(aRightSelector,TestNode,false,nil);
  if Result<0 then exit;
  aParentRule:=GetRuleParentOfSelector(aRightSelector,true);
  if aParentRule=nil then
    exit(CSSSpecificityInvalid);

  // child combinator: only the direct parent must match the parent rule's selectors
  aParent:=TestNode.GetCSSParent;
  if aParent=nil then
    exit(CSSSpecificityNoMatch);
  for i:=0 to aParentRule.SelectorCount-1 do
  begin
    ParentSpecificity:=SelectorMatches(aParentRule.Selectors[i],aParent,false,aParentRule);
    if ParentSpecificity=CSSSpecificityInvalid then
      exit(CSSSpecificityInvalid);
    if ParentSpecificity>=0 then
    begin
      inc(Result,ParentSpecificity);
      exit;
    end;
  end;
  Result:=CSSSpecificityNoMatch;
end;

function TCSSResolver.SelectorAndPlusMatches(aRightSelector: TCSSElement; const TestNode: ICSSNode
  ): TCSSSpecificity;
var
  aParentRule: TCSSRuleElement;
  ParentSpecificity: TCSSSpecificity;
  i: Integer;
  aSibling: ICSSNode;
begin
  Result:=SelectorMatches(aRightSelector,TestNode,false,nil);
  if Result<0 then exit;
  aParentRule:=GetRuleParentOfSelector(aRightSelector,true);
  if aParentRule=nil then
    exit(CSSSpecificityInvalid);

  // adjacent sibling combinator: the immediately preceding sibling must match the parent rule's selectors
  aSibling:=TestNode.GetCSSPreviousSibling;
  if aSibling=nil then
    exit(CSSSpecificityNoMatch);
  for i:=0 to aParentRule.SelectorCount-1 do
  begin
    ParentSpecificity:=SelectorMatches(aParentRule.Selectors[i],aSibling,false,aParentRule);
    if ParentSpecificity=CSSSpecificityInvalid then
      exit(CSSSpecificityInvalid);
    if ParentSpecificity>=0 then
    begin
      inc(Result,ParentSpecificity);
      exit;
    end;
  end;
  Result:=CSSSpecificityNoMatch;
end;

function TCSSResolver.SelectorAndTildeMatches(aRightSelector: TCSSElement; const TestNode: ICSSNode
  ): TCSSSpecificity;
var
  aParentRule: TCSSRuleElement;
  ParentSpecificity: TCSSSpecificity;
  i: Integer;
  aSibling: ICSSNode;
begin
  Result:=SelectorMatches(aRightSelector,TestNode,false,nil);
  if Result<0 then exit;
  aParentRule:=GetRuleParentOfSelector(aRightSelector,true);
  if aParentRule=nil then
    exit(CSSSpecificityInvalid);

  // general sibling combinator: any preceding sibling must match the parent rule's selectors
  aSibling:=TestNode.GetCSSPreviousSibling;
  while aSibling<>nil do
  begin
    for i:=0 to aParentRule.SelectorCount-1 do
    begin
      ParentSpecificity:=SelectorMatches(aParentRule.Selectors[i],aSibling,false,aParentRule);
      if ParentSpecificity=CSSSpecificityInvalid then
        exit(CSSSpecificityInvalid);
      if ParentSpecificity>=0 then
      begin
        inc(Result,ParentSpecificity);
        exit;
      end;
    end;
    aSibling:=aSibling.GetCSSPreviousSibling;
  end;
  Result:=CSSSpecificityNoMatch;
end;

function TCSSResolver.SelectorAndRightAndMatches(aBinary: TCSSBinaryElement;
  const TestNode: ICSSNode): TCSSSpecificity;
// Called for nested "<selector> &" where & is on the right (subject).
// TestNode must match the parent rule AND be a descendant of an element
// matching the left selector.
var
  aParentRule: TCSSRuleElement;
  ParentSpecificity: TCSSSpecificity;
  i: Integer;
  aParent: ICSSNode;
begin
  // TestNode must match the parent rule's selectors (& = parent rule)
  aParentRule:=GetRuleParentOfSelector(aBinary,true);
  if aParentRule=nil then
    exit(CSSSpecificityInvalid);
  Result:=CSSSpecificityNoMatch;
  for i:=0 to aParentRule.SelectorCount-1 do
  begin
    ParentSpecificity:=SelectorMatches(aParentRule.Selectors[i],TestNode,false,aParentRule);
    if ParentSpecificity=CSSSpecificityInvalid then
      exit(CSSSpecificityInvalid);
    if ParentSpecificity>=0 then
    begin
      Result:=ParentSpecificity;
      break;
    end;
  end;
  if Result<0 then exit;

  // AND an ancestor of TestNode must match the left selector
  aParent:=TestNode.GetCSSParent;
  while aParent<>nil do
  begin
    ParentSpecificity:=SelectorMatches(aBinary.Left,aParent,false,nil);
    if ParentSpecificity=CSSSpecificityInvalid then
      exit(CSSSpecificityInvalid);
    if ParentSpecificity>=0 then
    begin
      inc(Result,ParentSpecificity);
      exit;
    end;
    aParent:=aParent.GetCSSParent;
  end;
  Result:=CSSSpecificityNoMatch;
end;

function TCSSResolver.SelectorAndCompoundMatches(aList: TCSSListElement;
  const TestNode: ICSSNode): TCSSSpecificity;
// Called for nested "&<selector>" compound: TestNode must match both the
// parent rule's selectors and all non-& parts of the list.
var
  aParentRule: TCSSRuleElement;
  ParentSpecificity, Spec: TCSSSpecificity;
  i: Integer;
  El: TCSSElement;
begin
  Result:=0;
  // match all non-& parts against TestNode
  for i:=0 to aList.ChildCount-1 do
  begin
    El:=aList.Children[i];
    if (El is TCSSIdentifierElement) and (TCSSIdentifierElement(El).Value='&') then
      continue;
    Spec:=SelectorMatches(El,TestNode,false,nil);
    if Spec<0 then exit(Spec);
    inc(Result,Spec);
  end;

  // compound: TestNode itself must also match the parent rule's selectors
  aParentRule:=GetRuleParentOfSelector(aList,true);
  if aParentRule=nil then
    exit(CSSSpecificityInvalid);
  for i:=0 to aParentRule.SelectorCount-1 do
  begin
    ParentSpecificity:=SelectorMatches(aParentRule.Selectors[i],TestNode,false,aParentRule);
    if ParentSpecificity=CSSSpecificityInvalid then
      exit(CSSSpecificityInvalid);
    if ParentSpecificity>=0 then
    begin
      inc(Result,ParentSpecificity);
      exit;
    end;
  end;
  Result:=CSSSpecificityNoMatch;
end;

function TCSSResolver.SelectorHashIdentifierMatches(
  aIdentifier: TCSSHashIdentifierElement; const TestNode: ICSSNode;
  OnlySpecificity: boolean): TCSSSpecificity;
var
  aID: TCSSNumericalID;
begin
  if OnlySpecificity then
    exit(CSSSpecificityIdentifier);
  Result:=CSSSpecificityNoMatch;
  aID:=TCSSResolvedHashIdentifierElement(aIdentifier).NumericalID;
  if (aID>=1) and (TestNode.GetCSSID=aID) then
    Result:=CSSSpecificityIdentifier;
end;

function TCSSResolver.SelectorClassNameMatches(
  aClassName: TCSSResolvedClassNameElement; const TestNode: ICSSNode;
  OnlySpecificity: boolean): TCSSSpecificity;
begin
  if OnlySpecificity then
    exit(CSSSpecificityClass);
  if TestNode.HasCSSClass(aClassName.NumericalID) then
    Result:=CSSSpecificityClass
  else
    Result:=CSSSpecificityNoMatch;
  //writeln('TCSSResolver.SelectorClassNameMatches ',aClassName.Name,' ',Result);
end;

function TCSSResolver.SelectorPseudoClassMatches(aPseudoClass: TCSSResolvedPseudoClassElement;
  const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity;
var
  PseudoID: TCSSNumericalID;
begin
  if OnlySpecificity then
    exit(CSSSpecificityClass);
  Result:=CSSSpecificityNoMatch;
  PseudoID:=aPseudoClass.NumericalID;
  case PseudoID of
  CSSIDNone:
    begin
      // already warned by parser
      {$IFDEF VerboseCSSResolver}
      Log(etWarning,20240625153950,'Unknown pseudo class',aPseudoClass);
      {$ENDIF}
    end;
  CSSPseudoID_Root:
    if TestNode.GetCSSParent=nil then
      Result:=CSSSpecificityClass;
  CSSPseudoID_Empty:
    if TestNode.GetCSSEmpty then
      Result:=CSSSpecificityClass;
  CSSPseudoID_FirstChild:
    if TestNode.GetCSSPreviousSibling=nil then
      Result:=CSSSpecificityClass;
  CSSPseudoID_LastChild:
    if TestNode.GetCSSNextSibling=nil then
      Result:=CSSSpecificityClass;
  CSSPseudoID_OnlyChild:
    if (TestNode.GetCSSNextSibling=nil)
        and (TestNode.GetCSSPreviousSibling=nil) then
      Result:=CSSSpecificityClass;
  CSSPseudoID_FirstOfType:
    if TestNode.GetCSSPreviousOfType=nil then
      Result:=CSSSpecificityClass;
  CSSPseudoID_LastOfType:
    if TestNode.GetCSSNextOfType=nil then
      Result:=CSSSpecificityClass;
  CSSPseudoID_OnlyOfType:
    if (TestNode.GetCSSNextOfType=nil)
        and (TestNode.GetCSSPreviousOfType=nil) then
      Result:=CSSSpecificityClass;
  else
    if TestNode.HasCSSPseudoClass(PseudoID) then
      Result:=CSSSpecificityClass;
  end;
end;

function TCSSResolver.SelectorListMatches(aList: TCSSListElement;
  const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity;
var
  i: Integer;
  El: TCSSElement;
  C: TClass;
  Specificity: TCSSSpecificity;
  aNode: ICSSNode;
begin
  Result:=0;
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.SelectorListMatches ChildCount=',aList.ChildCount);
  {$ENDIF}
  aNode:=TestNode;
  for i:=0 to aList.ChildCount-1 do
  begin
    El:=aList.Children[i];
    {$IFDEF VerboseCSSResolver}
    writeln('TCSSResolver.SelectorListMatches ',i,' ',GetCSSObj(El),' AsString=',El.AsString);
    {$ENDIF}
    C:=El.ClassType;
    if (C=TCSSResolvedIdentifierElement) and (i>0) then
    begin
      if OnlySpecificity then
        exit(0);
      // already warned by parser
      {$IFDEF VerboseCSSResolver}
      Log(etWarning,20240625154031,'Type selector must be first',aList);
      {$ENDIF}
      exit(CSSSpecificityInvalid);
    end else
      Specificity:=SelectorMatches(El,aNode,OnlySpecificity);
    if Specificity<0 then
      exit(Specificity);
    inc(Result,Specificity);
  end;
end;

function TCSSResolver.SelectorUnaryMatches(aUnary: TCSSUnaryElement; const TestNode: ICSSNode;
  OnlySpecificity: boolean): TCSSSpecificity;
begin
  Result:=CSSSpecificityInvalid;
  case aUnary.Operation of
  uoDoubleColon:
    begin
      // ::PseudoElement
      if OnlySpecificity then
        // same as *::PseudoElement, the universal selector adds nothing
        Result:=CSSSpecificityPseudoElement
      else
        Result:=SelectorPseudoElementMatches(nil,aUnary.Right,TestNode);
    end;
  else
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20250225103026,'Invalid CSS unary selector '+UnaryOperators[aUnary.Operation],aUnary);
    {$ENDIF}
  end;
end;

function TCSSResolver.SelectorBinaryMatches(aBinary: TCSSBinaryElement;
  const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity;
var
  aParent, Sibling: ICSSNode;
  aSpecificity: TCSSSpecificity;
begin
  if OnlySpecificity then
  begin
    Result:=SelectorMatches(aBinary.Left,TestNode,true);
    if aBinary.Operation=boDoubleColon then
      // the right side is a pseudo element (function), whose numerical ID belongs
      // to the pseudo element namespace and must not be resolved as a type/call
      inc(Result,CSSSpecificityPseudoElement)
    else
      inc(Result,SelectorMatches(aBinary.Right,TestNode,true));
    exit;
  end;

  Result:=CSSSpecificityInvalid;
  case aBinary.Operation of
  boGT:
    begin
      // child combinator >
      Result:=SelectorMatches(aBinary.Right,TestNode,false);
      if Result<0 then exit;
      aParent:=TestNode.GetCSSParent;
      if aParent=nil then
        exit(CSSSpecificityNoMatch);
      aSpecificity:=SelectorMatches(aBinary.Left,aParent,false);
      if aSpecificity<0 then
        exit(aSpecificity);
      inc(Result,aSpecificity);
    end;
  boPlus:
    begin
      // adjacent sibling combinator +
      Result:=SelectorMatches(aBinary.Right,TestNode,false);
      if Result<0 then exit;
      Sibling:=TestNode.GetCSSPreviousSibling;
      if Sibling=nil then
        exit(CSSSpecificityNoMatch);
      aSpecificity:=SelectorMatches(aBinary.Left,Sibling,false);
      if aSpecificity<0 then
        exit(aSpecificity);
      inc(Result,aSpecificity);
    end;
  boTilde:
    begin
      // general sibling combinator ~
      Result:=SelectorMatches(aBinary.Right,TestNode,false);
      if Result<0 then exit;
      Sibling:=TestNode.GetCSSPreviousSibling;
      while Sibling<>nil do
      begin
        aSpecificity:=SelectorMatches(aBinary.Left,Sibling,false);
        if aSpecificity=CSSSpecificityInvalid then
          exit(aSpecificity)
        else if aSpecificity>=0 then
        begin
          inc(Result,aSpecificity);
          exit;
        end;
        Sibling:=Sibling.GetCSSPreviousSibling;
      end;
      Result:=CSSSpecificityNoMatch;
    end;
  boWhiteSpace:
    begin
      // descendant combinator
      Result:=SelectorMatches(aBinary.Right,TestNode,false);
      if Result<0 then exit;
      aParent:=TestNode;
      repeat
        aParent:=aParent.GetCSSParent;
        if aParent=nil then
          exit(CSSSpecificityNoMatch);
        aSpecificity:=SelectorMatches(aBinary.Left,aParent,false);
        if aSpecificity>=0 then
        begin
          inc(Result,aSpecificity);
          exit;
        end
        else if aSpecificity=CSSSpecificityInvalid then
          exit(CSSSpecificityInvalid);
      until false;
    end;
  boDoubleColon:
    Result:=SelectorPseudoElementMatches(aBinary.Left,aBinary.Right,TestNode);
  else
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20240625154050,'Invalid CSS binary selector '+BinaryOperators[aBinary.Operation],aBinary);
    {$ENDIF}
  end;
end;

function TCSSResolver.SelectorPseudoElementMatches(aLeft, aRight: TCSSElement;
  const TestNode: ICSSNode): TCSSSpecificity;
// pseudo element (function)
var
  ID: TCSSNumericalID;
  aParent: ICSSNode;
  aSpecificity: TCSSSpecificity;
begin
  Result:=CSSSpecificityInvalid;
  if aRight is TCSSResolvedIdentifierElement then
  begin
    // pseudo element
    ID:=TCSSResolvedIdentifierElement(aRight).NumericalID;
    if ID<=0 then
    begin
      // already warned by parser
      {$IFDEF VerboseCSSResolver}
      Log(etWarning,20250224211914,'Invalid CSS pseudo element',aRight);
      {$ENDIF}
      exit;
    end;
    if ID<>TestNode.GetCSSPseudoElementID then
      exit(CSSSpecificityNoMatch);
    Result:=CSSSpecificityPseudoElement;
  end else if aRight is TCSSResolvedCallElement then begin
    // pseudo element function
    ID:=TCSSResolvedCallElement(aRight).NameNumericalID;
    if ID<0 then
    begin
      // already warned by parser
      {$IFDEF VerboseCSSResolver}
      Log(etWarning,20250224212143,'Invalid CSS pseudo element function',aRight);
      {$ENDIF}
      exit;
    end;
    if ID<>TestNode.GetCSSPseudoElementID then
      exit(CSSSpecificityNoMatch);
    // todo: check parameters
    Result:=CSSSpecificityPseudoElement;
  end else begin
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20250224212301,'Invalid CSS pseudo element',aRight);
    {$ENDIF}
  end;

  if aLeft=nil then
    exit; // unary ::Name

  // test left side
  aParent:=TestNode.GetCSSParent;
  if aParent=nil then
    exit(CSSSpecificityNoMatch);
  aSpecificity:=SelectorMatches(aLeft,aParent,false);
  if aSpecificity<0 then
    exit(aSpecificity);
  inc(Result,aSpecificity);
end;

function TCSSResolver.SelectorArrayMatches(anArray: TCSSArrayElement;
  const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity;
var
  {$IFDEF VerboseCSSResolver}
  i: integer;
  {$ENDIF}
  El: TCSSElement;
  C: TClass;
  AttrID: TCSSNumericalID;
  OldStringComparison: TCSSResStringComparison;
  aValue: TCSSString;
begin
  if OnlySpecificity then
    exit(CSSSpecificityClass);

  Result:=CSSSpecificityInvalid;
  if anArray.Prefix<>nil then
  begin
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20220910154004,'Invalid CSS attribute selector prefix',anArray.Prefix);
    {$ENDIF}
    exit;
  end;
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.SelectorArrayMatches Prefix=',GetCSSObj(anArray.Prefix),' ChildCount=',anArray.ChildCount);
  for i:=0 to anArray.ChildCount-1 do
    writeln('TCSSResolver.SelectorArrayMatches ',i,' ',GetCSSObj(anArray.Children[i]));
  {$ENDIF}
  if anArray.ChildCount<1 then
  begin
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20220910154033,'Invalid CSS attribute selector',anArray);
    {$ENDIF}
    exit;
  end;
  OldStringComparison:=StringComparison;
  try
    if anArray.ChildCount>1 then
    begin
      El:=anArray.Children[1];
      C:=El.ClassType;
      if C=TCSSResolvedIdentifierElement then
      begin
        aValue:=TCSSResolvedIdentifierElement(El).Value;
        case aValue of
        'i': FStringComparison:=crscCaseInsensitive;
        's': FStringComparison:=crscCaseSensitive;
        else
          // already warned by parser
          {$IFDEF VerboseCSSResolver}
          LogWarning(croErrorOnUnknownName in Options,20220914174409,'Invalid attribute modifier "'+aValue+'"',El);
          {$ENDIF}
          exit;
        end;
      end else begin
        // already warned by parser
        {$IFDEF VerboseCSSResolver}
        Log(etWarning,20220914173643,'Invalid CSS attribute modifier',El);
        {$ENDIF}
        exit;
      end;
    end;
    if (anArray.ChildCount>2) then
    begin
      // already warned by parser
      {$IFDEF VerboseCSSResolver}
      Log(etWarning,20220914174550,'Invalid CSS attribute modifier',anArray.Children[2]);
      {$ENDIF}
    end;

    El:=anArray.Children[0];
    C:=El.ClassType;
    if C=TCSSResolvedIdentifierElement then
    begin
      // [name]  ->  has explicit attribute
      AttrID:=TCSSResolvedIdentifierElement(El).NumericalID;
      case AttrID of
      CSSIDNone:
        Result:=CSSSpecificityNoMatch;
      CSSAttributeID_ID,
      CSSAttributeID_Class:
        // id and class are always defined
        Result:=CSSSpecificityClass;
      CSSAttributeID_All:
        // special CSS attributes without a value
        Result:=CSSSpecificityNoMatch;
      else
        if TestNode.HasCSSExplicitAttribute(AttrID) then
          Result:=CSSSpecificityClass
        else
          Result:=CSSSpecificityNoMatch;
      end;
    end else if C=TCSSBinaryElement then
      Result:=SelectorArrayBinaryMatches(TCSSBinaryElement(El),TestNode)
    else begin
      // already warned by parser
      {$IFDEF VerboseCSSResolver}
      LogWarning(croErrorOnUnknownName in Options,20220910153725,'Invalid CSS array selector',El);
      {$ENDIF}
    end;
  finally
    FStringComparison:=OldStringComparison;
  end;
end;

function TCSSResolver.SelectorArrayBinaryMatches(aBinary: TCSSBinaryElement;
  const TestNode: ICSSNode): TCSSSpecificity;
var
  Left, Right: TCSSElement;
  AttrID: TCSSNumericalID;
  LeftValue, RightValue: TCSSString;
  C: TClass;
begin
  Result:=CSSSpecificityNoMatch;
  Left:=aBinary.Left;
  if Left.ClassType<>TCSSResolvedIdentifierElement then
  begin
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20220910164353,'Invalid CSS array selector, expected attribute',Left);
    {$ENDIF}
    exit;
  end;
  AttrID:=TCSSResolvedIdentifierElement(Left).NumericalID;
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.SelectorArrayBinaryMatches AttrID=',AttrID,' Value=',TCSSResolvedIdentifierElement(Left).Value);
  {$ENDIF}
  case AttrID of
  CSSIDNone: exit(CSSSpecificityNoMatch);
  CSSAttributeID_ID:
    LeftValue:=TestNode.GetCSSAttributeID;
  CSSAttributeID_Class:
    LeftValue:=TestNode.GetCSSAttributeClass;
  CSSAttributeID_All:
    exit(CSSSpecificityNoMatch);
  else
    LeftValue:=TestNode.GetCSSExplicitAttribute(AttrID);
  end;

  Right:=aBinary.Right;
  C:=Right.ClassType;
  if (C=TCSSStringElement) or (C=TCSSIntegerElement) or (C=TCSSFloatElement)
      or (C=TCSSResolvedIdentifierElement) then
    // ok
  else begin
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20220910164921,'Invalid CSS array selector, expected string',Right);
    {$ENDIF}
    exit;
  end;
  RightValue:=ComputeValue(Right);

  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.SelectorArrayBinaryMatches Left="',LeftValue,'" Right="',RightValue,'" Op=',aBinary.Operation);
  {$ENDIF}
  case aBinary.Operation of
  boEquals:
    if SameValueText(LeftValue,RightValue) then
      Result:=CSSSpecificityClass;
  boSquaredEqual:
    // begins with
    if (RightValue<>'') and SameValueText(LeftStr(LeftValue,length(RightValue)),RightValue) then
      Result:=CSSSpecificityClass;
  boDollarEqual:
    // ends with
    if (RightValue<>'') and SameValueText(RightStr(LeftValue,length(RightValue)),RightValue) then
      Result:=CSSSpecificityClass;
  boPipeEqual:
    // equal to or starts with name-hyphen
    if (RightValue<>'')
        and (SameValueText(LeftValue,RightValue)
          or SameValueText(LeftStr(LeftValue,length(RightValue)+1),RightValue+'-')) then
      Result:=CSSSpecificityClass;
  boStarEqual:
    // contains substring
    if (RightValue<>'') and (Pos(RightValue,LeftValue)>0) then
      Result:=CSSSpecificityClass;
  boTildeEqual:
    // contains word
    if PosWord(RightValue,LeftValue)>0 then
      Result:=CSSSpecificityClass;
  else
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20220910164356,'Invalid CSS array selector operator',aBinary);
    {$ENDIF}
    Result:=CSSSpecificityInvalid;
  end;
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.SelectorArrayBinaryMatches Result=',Result);
  {$ENDIF}
end;

function TCSSResolver.SelectorCallMatches(aCall: TCSSResolvedCallElement;
  const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity;
var
  CallID: TCSSNumericalID;
begin
  Result:=CSSSpecificityNoMatch;
  CallID:=aCall.NameNumericalID;
  //writeln('TCSSResolver.SelectorCallMatches ',CallID,' ',aCall.AsString);
  case CallID of
  CSSCallID_Not:
    Result:=Call_Not(aCall,TestNode,OnlySpecificity);
  CSSCallID_Is:
    Result:=Call_Is(aCall,TestNode,OnlySpecificity);
  CSSCallID_Where:
    Result:=Call_Where(aCall,TestNode,OnlySpecificity);
  CSSCallID_NthChild,
  CSSCallID_NthLastChild,
  CSSCallID_NthOfType,
  CSSCallID_NthLastOfType:
    Result:=Call_NthChild(CallID,aCall,TestNode,OnlySpecificity);
  else
    if OnlySpecificity then
      Result:=0
    else
      Result:=CSSSpecificityInvalid;
  end;
end;

function TCSSResolver.Call_Not(aCall: TCSSResolvedCallElement;
  const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity;
// :not(arg1, arg2, ...)
// :not(args) has the same Specificity as :not(:is(args))
var
  i: Integer;
  Specificity: TCSSSpecificity;
  HasMatch: Boolean;
begin
  Result:=0;
  HasMatch:=false;
  for i:=0 to aCall.ArgCount-1 do
  begin
    Specificity:=SelectorMatches(aCall.Args[i],TestNode,OnlySpecificity);
    //writeln('TCSSResolver.Call_Not ',i,' ',TestNode.GetCSSTypeName,' Spec=',Specificity);
    if Specificity>=0 then
      HasMatch:=true
    else begin
      // the Specificity of ":not" is the highest, independent of matching (forgiving)
      if not OnlySpecificity then
        Specificity:=SelectorMatches(aCall.Args[i],TestNode,true);
    end;
    if Specificity>Result then
      Result:=Specificity;
  end;
  if OnlySpecificity then
    // return best
  else if HasMatch then
    Result:=CSSSpecificityNoMatch;
end;

function TCSSResolver.Call_Is(aCall: TCSSResolvedCallElement; const TestNode: ICSSNode;
  OnlySpecificity: boolean): TCSSSpecificity;
var
  i: Integer;
  Specificity: TCSSSpecificity;
  ok: Boolean;
begin
  Result:=0;
  //writeln('TCSSResolver.Call_Is START ',TestNode.GetCSSID,' ArgCount=',aCall.ArgCount);
  ok:=false;
  for i:=0 to aCall.ArgCount-1 do
  begin
    Specificity:=SelectorMatches(aCall.Args[i],TestNode,OnlySpecificity);
    //writeln('TCSSResolver.Call_Is i=',i,' ',TestNode.GetCSSID,' ',aCall.Args[i].AsString,' Spec=',Specificity);
    if Specificity>=0 then
      ok:=true
    else begin
      // the Specificity of :is is the highest, independent of matching (forgiving)
      if not OnlySpecificity then
        Specificity:=SelectorMatches(aCall.Args[i],TestNode,true);
    end;
    if Specificity>Result then
      Result:=Specificity;
  end;
  if (not ok) and (not OnlySpecificity) then
    Result:=CSSSpecificityNoMatch;
end;

function TCSSResolver.Call_Where(aCall: TCSSResolvedCallElement;
  const TestNode: ICSSNode; OnlySpecificity: boolean): TCSSSpecificity;
var
  i: Integer;
begin
  Result:=0;
  if OnlySpecificity then
    exit;
  for i:=0 to aCall.ArgCount-1 do
  begin
    if SelectorMatches(aCall.Args[i],TestNode,false)>=0 then
      // Note: :where is forgiving, so invalid arguments are ignored
      exit;
  end;
  Result:=CSSSpecificityNoMatch;
end;

function TCSSResolver.Call_NthChild(PseudoFuncID: TCSSNumericalID;
  aCall: TCSSResolvedCallElement; const TestNode: ICSSNode; OnlySpecificity: boolean
  ): TCSSSpecificity;
var
  i: Integer;
  Params: TCSSResolverNthChildParams;
  ChildIDs: TIntegerDynArray;
begin
  Params:=aCall.Params as TCSSResolverNthChildParams;
  if Params=nil then
    exit(CSSSpecificityInvalid);

  if OnlySpecificity then
    Result:=CSSSpecificityClass
  else
    Result:=CSSSpecificityInvalid;

  if OnlySpecificity then
  begin
    if Params.OfSelector<>nil then
      inc(Result,SelectorMatches(Params.OfSelector,TestNode,true));
    exit;
  end;

  Result:=CSSSpecificityNoMatch;
  i:=TestNode.GetCSSIndex;
  if Params.HasOf then
  begin
    ChildIDs:=CollectSiblingsOf(PseudoFuncID,TestNode,Params);
    i:=GetSiblingOfIndex(ChildIDs,i);
  end else
    ChildIDs:=nil;
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.Call_NthChild CallID=',PseudoFuncID,' Node=',TestNode.GetCSSID,' ',Params.Modulo,' * N + ',Params.Start,' Index=',TestNode.GetCSSIndex,' i=',i,' HasOf=',Params.HasOf,' OfSelector=',GetCSSObj(Params.OfSelector));
  {$ENDIF}
  if i<0 then
    exit;
  if PseudoFuncID in [CSSCallID_NthLastChild,CSSCallID_NthLastOfType] then
  begin
    if Params.HasOf then
      i:=length(ChildIDs)-i
    else
      i:=GetSiblingCount(TestNode)-i;
  end else
  begin
    i:=i+1;
  end;
  dec(i,Params.Start);
  if Params.Modulo=0 then
  begin
    // plain integer B (a=0): match exactly the B-th sibling, e.g. nth-child(2)
    if i=0 then
      Result:=CSSSpecificityClass;
  end
  else if i mod Params.Modulo = 0 then
  begin
    i:=i div Params.Modulo;
    if i>=0 then
      Result:=CSSSpecificityClass;
  end;
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.Call_NthChild Node=',TestNode.GetCSSID,' ',Params.Modulo,' * N + ',Params.Start,' Index=',TestNode.GetCSSIndex+1,' i=',i,' Result=',Result);
  {$ENDIF}
end;

function TCSSResolver.CollectSiblingsOf(PseudoFuncID: TCSSNumericalID;
  TestNode: ICSSNode; Params: TCSSResolverNthChildParams): TIntegerDynArray;
var
  i, Depth, ChildCount, j: Integer;
  aTypeID: TCSSNumericalID;
  aParent, aNode: ICSSNode;
  aSelector: TCSSElement;
  StackDepth: SizeInt;
  Cache: TCSSResolverNthChildParamsCache;
  Item: PCSSNthChildParamsCacheItem;
  NeedTypeID: Boolean;
begin
  Result:=nil;
  aParent:=TestNode.GetCSSParent;
  {$IFDEF VerboseCSSResolver}
  //writeln('TCSSResolver.CollectSiblingsOf HasParent=',aParent<>nil);
  {$ENDIF}
  if aParent=nil then exit;
  ChildCount:=aParent.GetCSSChildCount;
  if ChildCount=0 then exit;

  Depth:=aParent.GetCSSDepth;
  StackDepth:=length(Params.StackCache);
  if StackDepth<=Depth then
  begin
    SetLength(Params.StackCache,Depth+1);
    for i:=StackDepth to Depth do
      Params.StackCache[i]:=nil;
  end;
  Cache:=Params.StackCache[Depth];
  if Cache=nil then
  begin
    Cache:=TCSSResolverNthChildParamsCache.Create;
    Params.StackCache[Depth]:=Cache;
    Cache.Owner:=Params;
    Cache.StackDepth:=Depth;
  end;

  NeedTypeID:=PseudoFuncID in [CSSCallID_NthOfType,CSSCallID_NthLastOfType];
  aSelector:=Params.OfSelector;

  if (Cache.Parent<>aParent) or (Cache.OfSelector<>aSelector) then
  begin
    // build cache
    Cache.Parent:=aParent;
    Cache.OfSelector:=aSelector;
    SetLength(Cache.Items,0);
    {$IFDEF VerboseCSSResolver}
    writeln('TCSSResolver.CollectSiblingsOf Depth=',Depth,' Candidates=',ChildCount);
    {$ENDIF}
    for i:=0 to ChildCount-1 do
    begin
      aNode:=aParent.GetCSSChild(i);
      if (aSelector<>nil) and (SelectorMatches(aSelector,aNode,false)<0) then
        continue;

      // put
      if NeedTypeID then
        aTypeID:=aNode.GetCSSTypeID
      else
        aTypeID:=0;
      j:=length(Cache.Items)-1;
      while (j>=0) and (Cache.Items[j].TypeID<>aTypeID) do dec(j);
      if j<0 then
      begin
        j:=length(Cache.Items);
        SetLength(Cache.Items,j+1);
        Item:=@Cache.Items[j];
        Item^.TypeID:=aTypeID;
        Item^.Cnt:=0;
        SetLength(Item^.ChildIDs,ChildCount);
      end else
        Item:=@Cache.Items[j];
      Item^.ChildIDs[Item^.Cnt]:=i;
      {$IFDEF VerboseCSSResolver}
      writeln('TCSSResolver.CollectSiblingsOf Sel=',GetCSSObj(aSelector),' CSSTypeID=',aNode.GetCSSTypeID,' ',Item^.Cnt,'=>',i);
      {$ENDIF}
      inc(Item^.Cnt);
    end;

    for i:=0 to high(Cache.Items) do
      with Cache.Items[i] do
        SetLength(ChildIDs,Cnt);
  end;

  // use cache
  if NeedTypeID then
  begin
    aTypeID:=TestNode.GetCSSTypeID;
    for i:=0 to high(Cache.Items) do
      if Cache.Items[i].TypeID=aTypeID then
        exit(Cache.Items[i].ChildIDs);
  end else if length(Cache.Items)>0 then
    Result:=Cache.Items[0].ChildIDs;
end;

function TCSSResolver.GetSiblingOfIndex(SiblingIDs: TIntegerDynArray;
  Index: integer): integer;
// searches the position of Index in a sorted array
var
  l, r, m: Integer;
begin
  l:=0;
  r:=length(SiblingIDs)-1;
  while l<=r do
  begin
    m:=(l+r) div 2;
    Result:=SiblingIDs[m];
    if Index<Result then
      r:=m-1
    else if Index>Result then
      l:=m+1
    else
      exit(m);
  end;
  Result:=-1;
end;

function TCSSResolver.ComputeValue(El: TCSSElement): TCSSString;
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
      {$IFDEF VerboseCSSResolver}
      writeln('TCSSResolver.ComputeValue String=[',Result,']');
      {$ENDIF}
    end
    else if C=TCSSIntegerElement then
    begin
      IntEl:=TCSSIntegerElement(El);
      Result:=IntEl.AsString;
      {$IFDEF VerboseCSSResolver}
      writeln('TCSSResolver.ComputeValue Integer=[',Result,']');
      {$ENDIF}
    end else if C=TCSSFloatElement then
    begin
      FloatEl:=TCSSFloatElement(El);
      Result:=FloatEl.AsString;
      {$IFDEF VerboseCSSResolver}
      writeln('TCSSResolver.ComputeValue Float=[',Result,']');
      {$ENDIF}
    end;
    if El.CustomData<>nil then
      raise ECSSResolver.Create('20260416220031');
    ElData:=TCSSValueData.Create;
    TCSSValueData(ElData).NormValue:=Result;
    El.CustomData:=ElData;
  end else begin
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    LogWarning(croErrorOnUnknownName in Options,20220910235106,'TCSSResolver.ComputeValue not supported',El);
    {$ENDIF}
  end;
end;

function TCSSResolver.SameValueText(const A, B: TCSSString): boolean;
begin
  if StringComparison=crscCaseInsensitive then
    Result:=SameText(A,B)
  else
    Result:=A=B;
end;

function TCSSResolver.SameValueText(A: PCSSChar; ALen: integer; B: PCSSChar; BLen: integer
  ): boolean;
var
  AC, BC: TCSSChar;
  i: Integer;
begin
  if ALen<>BLen then exit(false);
  if ALen=0 then exit(true);
  if StringComparison=crscCaseInsensitive then
  begin
    for i:=0 to ALen-1 do
    begin
      AC:=A^;
      BC:=B^;
      if (AC<>BC) then
      begin
        if (AC in ['a'..'z']) then AC:=TCSSChar(ord(AC)-32);
        if (BC in ['a'..'z']) then BC:=TCSSChar(ord(BC)-32);
        if AC<>BC then
          exit(false);
      end;
      inc(A);
      inc(B);
    end;
    Result:=true;
  end else
    Result:=CompareMem(A,B,ALen*SizeOf(TCSSChar));
end;

function TCSSResolver.PosSubString(const SearchStr, Str: TCSSString): integer;
var
  SearchLen: SizeInt;
  i: Integer;
  SearchP, StrP: PCSSChar;
  AC, BC: TCSSChar;
begin
  Result:=0;
  if SearchStr='' then exit;
  if Str='' then exit;
  if StringComparison=crscCaseInsensitive then
  begin
    SearchP:=PCSSChar(SearchStr);
    StrP:=PCSSChar(Str);
    SearchLen:=length(SearchStr);
    AC:=SearchP^;
    if AC in ['a'..'z'] then AC:=TCSSChar(ord(AC)-32);
    for i:=0 to length(Str)-SearchLen do
    begin
      BC:=StrP^;
      if BC in ['a'..'z'] then BC:=TCSSChar(ord(BC)-32);
      if (AC=BC) and SameValueText(SearchP,SearchLen,StrP,SearchLen) then
        exit(i+1);
      inc(StrP);
    end;
  end else begin
    Result:=Pos(SearchStr,Str);
  end;
end;

function TCSSResolver.PosWord(const SearchWord, Words: TCSSString): integer;
// attribute selector ~=
const
  Whitespace = [#9,#10,#12,#13,' '];
var
  WordsLen, SearchLen: SizeInt;
  p, WordStart: Integer;
begin
  Result:=0;
  if SearchWord='' then exit;
  if Words='' then exit;
  WordsLen:=length(Words);
  SearchLen:=length(SearchWord);
  //writeln('TCSSResolver.PosWord "',SearchWord,'" Words="',words,'"');
  p:=1;
  repeat
    repeat
      if p>WordsLen then
        exit(0);
      if not (Words[p] in Whitespace) then
        break;
      inc(p);
    until false;
    WordStart:=p;
    while (p<=WordsLen) and not (Words[p] in Whitespace) do
      inc(p);
    //writeln('TCSSResolver.PosWord start=',WordStart,' p=',p);
    if SameValueText(@SearchWord[1],SearchLen,@Words[WordStart],p-WordStart) then
      exit(WordStart);
  until p>WordsLen;
end;

function TCSSResolver.GetSiblingCount(aNode: ICSSNode): integer;
var
  aParent, CurNode: ICSSNode;
begin
  if aNode=nil then
    exit(0);
  aParent:=aNode.GetCSSParent;
  if aParent<>nil then
    exit(aParent.GetCSSChildCount);
  Result:=0;
  CurNode:=aNode;
  while CurNode<>nil do
  begin
    inc(Result);
    CurNode:=CurNode.GetCSSPreviousSibling;
  end;
  CurNode:=aNode.GetCSSNextSibling;
  while CurNode<>nil do
  begin
    inc(Result);
    CurNode:=CurNode.GetCSSNextSibling;
  end;
end;

procedure TCSSResolver.MergeAttribute(El: TCSSElement;
  aSpecificity: TCSSSpecificity);
var
  C: TClass;
  Decl: TCSSDeclarationElement;
  aKey: TCSSElement;
  AnAttrID, NextAttrID, SubAttrID: TCSSNumericalID;
  AttrDesc: TCSSAttributeDesc;
  KeyData: TCSSAttributeKeyData;
begin
  C:=El.ClassType;
  if C<>TCSSDeclarationElement then
  begin
    // already warned by parser, e.g. nested rule
    {$IFDEF VerboseCSSResolver}
    //Log(etWarning,20220908232359,'Unknown property',El);
    {$ENDIF}
    exit;
  end;
  Decl:=TCSSDeclarationElement(El);

  if Decl.KeyCount<>1 then
  begin
    if Decl.KeyCount<1 then
    begin
      // already warned by parser
      {$IFDEF VerboseCSSResolver}
      Log(etWarning,20231112135955,'missing keys in declaration',Decl);
      {$ENDIF}
    end;
    if Decl.KeyCount>1 then
    begin
      // already warned by parser
      {$IFDEF VerboseCSSResolver}
      Log(etWarning,20231112140722,'too many keys in declaration',Decl);
      {$ENDIF}
    end;
    exit;
  end;

  if Decl.ChildCount=0 then
    exit;
  if Decl.IsImportant then
    aSpecificity:=CSSSpecificityImportant;

  aKey:=Decl.Keys[0];
  C:=aKey.ClassType;
  if C<>TCSSResolvedIdentifierElement then
  begin
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20220908232359,'Unknown CSS key',aKey);
    {$ENDIF}
    exit;
  end;
  AnAttrID:=TCSSResolvedIdentifierElement(aKey).NumericalID;
  if AnAttrID<=CSSIDNone then
  begin
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    Log(etWarning,20220909000932,'Unknown CSS property "'+TCSSResolvedIdentifierElement(aKey).Name+'"',aKey);
    {$ENDIF}
    exit;
  end;

  KeyData:=TCSSAttributeKeyData(aKey.CustomData);
  if KeyData.Invalid then
  begin
    // already warned by parser
    {$IFDEF VerboseCSSResolver}
    //Log(etWarning,20240710162139,'Invalid CSS property "'+El.AsString+'"',aKey);
    {$ENDIF}
    exit;
  end;
  if KeyData.Disabled then
    exit; // declaration disabled by DisableDeclaration, treat as commented out

  if AnAttrID=CSSAttributeID_All then
  begin
    // 'all' sets almost all attributes to a value
    if FMergedAllSpecificity>aSpecificity then
      exit;
    FMergedAllSpecificity:=aSpecificity;
    FMergedAllDecl:=Decl;

    SubAttrID:=FMergedAttributeFirst;
    while SubAttrID>=1 do
    begin
      NextAttrID:=FMergedAttributes[SubAttrID].Next;
      AttrDesc:=GetAttributeDesc(SubAttrID);
      if AttrDesc.All then
        RemoveMergedAttribute(SubAttrID);
      SubAttrID:=NextAttrID;
    end;
  end
  else begin
    // set property
    AttrDesc:=GetAttributeDesc(AnAttrID);

    if (FMergedAllSpecificity>aSpecificity) and AttrDesc.All then
      exit; // a former 'all' has higher Specificity

    with FMergedAttributes[AnAttrID] do
    begin
      if (Stamp=FMergedAttributesStamp) and (Specificity>aSpecificity) then
        exit; // a former attribute has higher Specificity
    end;
    {$IFDEF VerboseCSSResolver}
    writeln('TCSSResolver.MergeAttribute Node=',FNode.GetCSSID,' AttrID=',AnAttrID,' ',AttrDesc.Name,' Spec=',aSpecificity,' Decl="',Decl.AsString,'"');
    {$ENDIF}
    SetMergedAttribute(AnAttrID,aSpecificity,Decl);

    if (AttrDesc<>nil) and (length(AttrDesc.CompProps)>0) then
    begin
      // shorthand -> set longhands
      // Note: order matters when same Specificity, so longhands must be done during the cascade
      for NextAttrID:=0 to length(AttrDesc.CompProps)-1 do
      begin
        SubAttrID:=AttrDesc.CompProps[NextAttrID].Index;
        with FMergedAttributes[SubAttrID] do
        begin
          if (Stamp=FMergedAttributesStamp) and (Specificity>aSpecificity) then
            continue; // a former attribute has higher Specificity
          SetMergedAttribute(SubAttrID,aSpecificity,nil);
          {$IFDEF VerboseCSSResolver}
          writeln('TCSSResolver.MergeAttribute Longhand Node=',FNode.GetCSSID,' Shorthand=',AttrDesc.Name,' Spec=',aSpecificity,' Decl="',Decl.AsString,'" Longhand=',GetAttributeDesc(SubAttrID).Name);
          {$ENDIF}
        end;
      end;
    end;

    //WriteMergedAttributes('TCSSResolver.MergeAttribute');
  end;
end;

procedure TCSSResolver.SaveSharedMergedAttributes(SharedMerged: TCSSSharedRuleList);
var
  Cnt: Integer;
  AttrID: TCSSNumericalID;
  AttrP: PMergedAttribute;
begin
  SharedMerged.AllDecl:=FMergedAllDecl;
  SharedMerged.AllSpecificity:=FMergedAllSpecificity;

  // count attributes (skip longhands set by shorthands DeclEl=nil)
  Cnt:=0;
  AttrID:=FMergedAttributeFirst;
  while AttrID>0 do
  begin
    AttrP:=@FMergedAttributes[AttrID];
    if AttrP^.DeclEl<>nil then
      inc(Cnt);
    AttrID:=AttrP^.Next;
  end;
  SetLength(SharedMerged.Values,Cnt);

  // save attributes
  Cnt:=0;
  AttrID:=FMergedAttributeFirst;
  while AttrID>0 do
  begin
    AttrP:=@FMergedAttributes[AttrID];
    if AttrP^.DeclEl<>nil then
    begin
      SharedMerged.Values[Cnt].AttrID:=AttrID;
      SharedMerged.Values[Cnt].DeclEl:=AttrP^.DeclEl;
      SharedMerged.Values[Cnt].Specificity:=AttrP^.Specificity;
      inc(Cnt);
    end;
    AttrID:=AttrP^.Next;
  end;
end;

procedure TCSSResolver.LoadSharedMergedAttributes(
  SharedMerged: TCSSSharedRuleList);
var
  i: Integer;
begin
  ClearMerge;
  FMergedAllDecl:=SharedMerged.AllDecl;
  FMergedAllSpecificity:=SharedMerged.AllSpecificity;
  for i:=0 to length(SharedMerged.Values)-1 do
  begin
    with SharedMerged.Values[i] do
      SetMergedAttribute(AttrID,Specificity,DeclEl);
  end;
end;

procedure TCSSResolver.WriteMergedAttributes(const Title: TCSSString);
var
  AttrID, NextAttrID: TCSSNumericalID;
  AttrP: PMergedAttribute;
  Cnt: Integer;
  AttrDesc: TCSSAttributeDesc;
begin
  writeln('TCSSResolver.WriteMergedAttributes START ',Title);
  Cnt:=0;
  AttrID:=FMergedAttributeFirst;
  while AttrID>0 do
  begin
    NextAttrID:=FMergedAttributes[AttrID].Next;
    AttrP:=@FMergedAttributes[AttrID];
    AttrDesc:=GetAttributeDesc(AttrID);
    writeln('  ',Cnt,' AttrID=',AttrID,' ',AttrDesc.Name,' Spec=',AttrP^.Specificity,' Value="',Detokenize(AttrP^.Tokens),'" Complete=',AttrP^.Complete,' Decl=',AttrP^.DeclEl<>nil);
    inc(Cnt);
    AttrID:=NextAttrID;
  end;
  writeln('TCSSResolver.WriteMergedAttributes END Count=',Cnt);
end;

procedure TCSSResolver.LoadMergedValues;
var
  AttrID, NextAttrID: TCSSNumericalID;
  AttrP: PMergedAttribute;
  Key: TCSSElement;
  KeyData: TCSSAttributeKeyData;
begin
  // load tokenized values from css elements
  // and remove longhand placeholders set by shorthands
  AttrID:=FMergedAttributeFirst;
  while AttrID>0 do
  begin
    NextAttrID:=FMergedAttributes[AttrID].Next;
    AttrP:=@FMergedAttributes[AttrID];
    if AttrP^.DeclEl=nil then
      // remove longhand placeholder set by shorthand
      RemoveMergedAttribute(AttrID)
    else begin
      Key:=AttrP^.DeclEl.Keys[0];
      KeyData:=Key.CustomData as TCSSAttributeKeyData;
      AttrP^.Tokens:=KeyData.Tokens;
      if KeyData.Invalid or (length(AttrP^.Tokens)=0) then
        RemoveMergedAttribute(AttrID)
      else
        AttrP^.Complete:=not CSSHasVarToken(AttrP^.Tokens);
    end;
    AttrID:=NextAttrID;
  end;
end;

procedure TCSSResolver.SubstituteVarCalls;
// called after CSS attribute values have been merged by cascade rules
// before replacing shorthands. Operates on the tokenized values.
const
  ReplaceMax = 10;
var
  AttrID, NextAttrID: TCSSNumericalID;
  AttrP: PMergedAttribute;
  AttrDesc: TCSSAttributeDesc;
  ReplaceCnt: integer;

  function SubstituteVars(var Tokens: TBytes): boolean;
  // replace all var() calls in Tokens (leftmost first, repeated).
  // Returns false on error (loop limit or syntax error).
  var
    Ofs, Len, TokLen, VarStart, AfterClose, NameTokOfs: integer;
    DefStart, DefEnd, Depth: integer;
    k: TCSSResTokenKind;
    VarName: TCSSString;
    Desc: TCSSResCustomAttributeDesc;
    aVarNode: ICSSNode;
    Repl: TBytes;
    HasRepl: boolean;
  begin
    repeat
      // find the leftmost var() function token
      Len:=length(Tokens);
      Ofs:=0;
      VarStart:=-1;
      while Ofs<Len do
      begin
        k:=TCSSResTokenKind(Tokens[Ofs]);
        if (k=rtkFunction) and (CSSReadTokenWord(Tokens,Ofs+1)=CSSAttrFuncVar) then
        begin
          VarStart:=Ofs;
          break;
        end;
        TokLen:=CSSTokenByteLen(Tokens,Ofs);
        if TokLen<=0 then exit(false);
        inc(Ofs,TokLen);
      end;
      if VarStart<0 then exit(true); // no more var() calls

      inc(ReplaceCnt);
      if ReplaceCnt=ReplaceMax then exit(false); // probably a loop

      // step inside the parenthesis, skip whitespace
      Ofs:=VarStart+CSSTokenByteLen(Tokens,VarStart);
      while (Ofs<Len) and (TCSSResTokenKind(Tokens[Ofs])=rtkWhitespace) do
        inc(Ofs);
      // the first argument must be a custom property name --xxx (rtkIdentifier)
      if (Ofs>=Len) or (TCSSResTokenKind(Tokens[Ofs])<>rtkIdentifier) then exit(false);
      NameTokOfs:=Ofs;
      VarName:=DetokenizeOne(@Tokens[NameTokOfs]);
      if (length(VarName)<2) or (VarName[1]<>'-') or (VarName[2]<>'-') then exit(false);
      Ofs:=Ofs+CSSTokenByteLen(Tokens,NameTokOfs);
      while (Ofs<Len) and (TCSSResTokenKind(Tokens[Ofs])=rtkWhitespace) do
        inc(Ofs);

      // optional default value after a comma
      DefStart:=-1;
      DefEnd:=-1;
      if (Ofs<Len) and (TCSSResTokenKind(Tokens[Ofs])=rtkComma) then
      begin
        Ofs:=Ofs+CSSTokenByteLen(Tokens,Ofs); // past the comma
        while (Ofs<Len) and (TCSSResTokenKind(Tokens[Ofs])=rtkWhitespace) do
          inc(Ofs);
        DefStart:=Ofs;
      end;

      // find the matching closing parenthesis
      Depth:=1;
      while Ofs<Len do
      begin
        k:=TCSSResTokenKind(Tokens[Ofs]);
        if (k=rtkLParenthesis) or (k=rtkFunction) then
          inc(Depth)
        else if k=rtkRParenthesis then
        begin
          dec(Depth);
          if Depth=0 then break;
        end;
        inc(Ofs,CSSTokenByteLen(Tokens,Ofs));
      end;
      if Depth<>0 then exit(false);
      if DefStart>=0 then
        DefEnd:=Ofs; // default value tokens are [DefStart, DefEnd)
      AfterClose:=Ofs+1; // one past the ')'

      // resolve the value of the custom property
      Repl:=nil;
      HasRepl:=false;
      {$IF SIZEOF(CHAR)=2}
      Desc:=TCSSResCustomAttributeDesc(FCustomAttributeNameToDesc.Find(UTF8Encode(VarName)));
      {$ELSE}
      Desc:=TCSSResCustomAttributeDesc(FCustomAttributeNameToDesc.Find(VarName));
      {$ENDIF}
      if Desc<>nil then
      begin
        if FMergedAttributes[Desc.Index].Stamp=FMergedAttributesStamp then
        begin
          Repl:=FMergedAttributes[Desc.Index].Tokens;
          HasRepl:=not CSSTokensEmpty(Repl);
        end;
        if not HasRepl then
        begin
          if FMergeVarsFromNode then
            aVarNode:=FNode
          else
            aVarNode:=FNode.GetCSSParent;
          if aVarNode<>nil then
          begin
            Repl:=aVarNode.GetCSSCustomAttribute(Desc.Index);
            HasRepl:=not CSSTokensEmpty(Repl);
          end;
        end;
      end;
      if (not HasRepl) and (DefStart>=0) then
        // use the default value
        Repl:=Copy(Tokens,DefStart,DefEnd-DefStart);

      // splice: Tokens[0..VarStart) + Repl + Tokens[AfterClose..end]
      Tokens:=Concat(Copy(Tokens,0,VarStart),Repl,Copy(Tokens,AfterClose,Len-AfterClose));
    until false;
  end;

begin
  AttrID:=FMergedAttributeFirst;
  while AttrID>0 do
  begin
    NextAttrID:=FMergedAttributes[AttrID].Next;
    AttrP:=@FMergedAttributes[AttrID];
    if not AttrP^.Complete then
    begin
      if CSSHasVarToken(AttrP^.Tokens) then
      begin
        ReplaceCnt:=0;
        if not SubstituteVars(AttrP^.Tokens) then
          AttrP^.Tokens:=nil
        else begin
          // the var() values were tokenized without knowing this attribute
          AttrDesc:=GetAttributeDesc(AttrID);
          if (AttrDesc<>nil) and not AttrDesc.AllowUnknownIdentifiers then
            if not ResolveIdentifierTokens(AttrP^.Tokens) then
              AttrP^.Tokens:=nil;
        end;
      end;
      if CSSTokensEmpty(AttrP^.Tokens) then
        RemoveMergedAttribute(AttrID);
    end;
    AttrID:=NextAttrID;
  end;
end;

procedure TCSSResolver.ApplyShorthands;
// called after all var() have been substituted
var
  AttrID, NextAttrID, SubAttrID: TCSSNumericalID;
  AttrP, SubAttrP: PMergedAttribute;
  AttrDesc, SubAttrDesc: TCSSAttributeDesc;
  LHAttrIDs: TCSSNumericalIDArray;
  LHValues: TBytesArray;
  i: Integer;
begin
  AttrID:=FMergedAttributeFirst;
  while AttrID>0 do
  begin
    NextAttrID:=FMergedAttributes[AttrID].Next;
    AttrP:=@FMergedAttributes[AttrID];
    AttrDesc:=GetAttributeDesc(AttrID);
    //writeln('TCSSResolver.ApplyShorthands ',AttrID,' ',AttrDesc.Name,' ShortHand=',AttrDesc.OnSplitShorthand<>nil);
    if Assigned(AttrDesc.OnSplitShorthand) then
    begin
      RemoveMergedAttribute(AttrID);
      if not CSSTokensEmpty(AttrP^.Tokens) then
      begin
        // replace shorthand with longhands, keep already set longhands
        LHAttrIDs:=[];
        LHValues:=[];
        InitParseAttr(AttrDesc,AttrP^.Tokens);
        if not AtEnd then
        begin
          AttrDesc.OnSplitShorthand(Self,LHAttrIDs,LHValues);
          for i:=0 to length(LHAttrIDs)-1 do
          begin
            SubAttrID:=LHAttrIDs[i];
            SubAttrDesc:=GetAttributeDesc(SubAttrID);
            if SubAttrDesc=nil then
              raise ECSSResolver.Create('20240709194135');
            if SubAttrDesc.OnSplitShorthand<>nil then
              raise ECSSResolver.Create('20240709194634');
            SubAttrP:=@FMergedAttributes[SubAttrID];
            if (SubAttrP^.Stamp=FMergedAttributesStamp) and (SubAttrP^.Specificity>=AttrP^.Specificity) then
            begin
              // longhand already exists -> keep
            end else begin
              SetMergedAttribute(SubAttrID,AttrP^.Specificity,nil);
              // the split handler returns tokenized values; fall back to the initial value when empty
              if not CSSTokensEmpty(LHValues[i]) then
                SubAttrP^.Tokens:=LHValues[i]
              else
                Tokenize(SubAttrDesc.InitialValue,SubAttrP^.Tokens,SubAttrDesc.AllowUnknownIdentifiers);
              SubAttrP^.Complete:=false;
              // Note: if NextAttrID=0 then this was the last shorthand
            end;
          end;
        end;
      end;
    end;
    AttrID:=NextAttrID;
  end;
end;

function TCSSResolver.CreateValueList: TCSSAttributeValues;
var
  Cnt: Integer;
  AttrID, AllKeywordID: TCSSNumericalID;
  AttrP: PMergedAttribute;
  AttrValue: TCSSAttributeValue;
begin
  Result:=TCSSAttributeValues.Create;

  // all
  if FMergedAllDecl<>nil then
  begin
    // set Result.AllValue
    // The parser already tokenized and checked the value, so read the keyword
    // directly from the tokens instead of parsing the value again.
    AllKeywordID:=CheckAttribute_Keyword(GetDeclarationTokens(FMergedAllDecl));
    if IsBaseKeyword(AllKeywordID) then
      Result.AllValue:=AllKeywordID;
  end;

  // count and allocate attributes
  Cnt:=0;
  AttrID:=FMergedAttributeFirst;
  while AttrID>0 do
  begin
    //writeln('TCSSResolver.CreateValueList Cnt=',Cnt,' AttrID=',AttrID);
    inc(Cnt);
    AttrID:=FMergedAttributes[AttrID].Next;
  end;
  SetLength(Result.Values,Cnt);

  // copy
  Cnt:=0;
  AttrID:=FMergedAttributeFirst;
  while AttrID>0 do
  begin
    AttrP:=@FMergedAttributes[AttrID];
    AttrValue:=TCSSAttributeValue.Create;
    Result.Values[Cnt]:=AttrValue;
    AttrValue.AttrID:=AttrID;
    AttrValue.Tokens:=AttrP^.Tokens;
    //writeln('TCSSResolver.CreateValueList ',Cnt,' ',AttrID);
    AttrID:=AttrP^.Next;
    inc(Cnt);
  end;

  // sort
  Result.SortValues;
end;

function TCSSResolver.ResolveIdentifier(El: TCSSResolvedIdentifierElement;
  aKind: TCSSNumericalIDKind): TCSSNumericalID;
var
  aName: TCSSString;
begin
  Result:=El.NumericalID;
  if Result=CSSIDNone then
  begin
    // not yet resolved
    aName:=El.Name;
    if aKind in [nikPseudoClass,nikPseudoElement] then
    begin
      // pseudo attributes and elements are ASCII case insensitive
      System.Delete(aName,1,1);
      aName:=lowercase(aName);
    end;

    Result:=CSSRegistry.IndexOfNamedItem(aKind,aName);
    if Result=CSSIDNone then
    begin
      El.NumericalID:=-1;
      Log(etWarning,20240625160211,'unknown '+CSSNumericalIDKindNames[aKind]+' "'+aName+'"',El);
    end else begin
      El.NumericalID:=Result;
      El.Kind:=aKind;
    end;
  end else if Result=-1 then
    Result:=CSSIDNone // name not found
  else if El.Kind<>aKind then
    raise ECSSResolver.Create('20240701105839');
end;

procedure TCSSResolver.LogWarning(IsError: boolean; const ID: TCSSMsgID;
  const Msg: TCSSString; PosEl: TCSSElement);
var
  MsgType: TEventType;
begin
  if IsError then
    MsgType:=etError
  else
    MsgType:=etWarning;
  Log(MsgType,ID,Msg,PosEl);
end;

procedure TCSSResolver.Log(MsgType: TEventType; const ID: TCSSMsgID;
  const Msg: TCSSString; PosEl: TCSSElement);
var
  Entry: TCSSResolverLogEntry;
begin
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolver.Log ',MsgType,' ID=',ID,' ',GetElPos(PosEl),': "',Msg,'"');
  {$ENDIF}
  if Assigned(OnLog) then
  begin
    Entry:=TCSSResolverLogEntry.Create;
    Entry.MsgType:=MsgType;
    Entry.ID:=ID;
    Entry.Msg:=Msg;
    Entry.PosEl:=PosEl;
    FLogEntries.Add(Entry);
    OnLog(Self,Entry);
  end;
  if MsgType=etError then
  begin
    raise ECSSResolver.Create('['+IntToStr(ID)+'] '+Msg+' at '+GetElPos(PosEl));
  end;
end;

function TCSSResolver.GetElPos(El: TCSSElement): TCSSString;
begin
  if El=nil then
    Result:='no element'
  else begin
    Result:=El.SourceFileName+'('+IntToStr(El.SourceCol)+','+IntToStr(El.SourceRow)+')';
    {$IFDEF VerboseCSSResolver}
    Result:='['+GetElPath(El)+']'+Result;
    {$ENDIF}
  end;
end;

function TCSSResolver.ParseInlineStyle(const Src: TCSSString): TCSSRuleElement;
begin
  Result:=ParseCSSSource(Src,true) as TCSSRuleElement;
end;

function TCSSResolver.GetElPath(El: TCSSElement): TCSSString;
begin
  Result:=GetCSSPath(El);
end;

constructor TCSSResolver.Create(AOwner: TComponent);
begin
  inherited;
  FLogEntries:=TFPObjectList.Create(true);
  FSharedRuleLists:=TAVLTree.Create(@CompareCSSSharedRuleLists);
  FDisabledDecls:=TFPHashObjectList.Create(true);
  FCustomAttributeNameToDesc:=TFPHashList.Create;
  FCSSClassNameToID:=TFPHashList.Create;
  FCSSIDNameToIndex:=TFPHashList.Create;
  FKeyframes:=TFPHashList.Create;
  FCSSClassIDStamp:=1;
  FMediaStamp:=1;
  FSourceStamp:=1;
end;

procedure TCSSResolver.ChangeCSSClassIDStamp;
begin
  if FCSSClassIDStamp<high(FCSSClassIDStamp) then
    inc(FCSSClassIDStamp)
  else
    FCSSClassIDStamp:=1;
end;

destructor TCSSResolver.Destroy;
begin
  Clear;
  ClearRuleBuckets;
  FreeAndNil(FKeyframes);
  FreeAndNil(FCSSIDNameToIndex);
  FreeAndNil(FCSSClassNameToID);
  FreeAndNil(FCustomAttributeNameToDesc);
  FreeAndNil(FSharedRuleLists);
  FreeAndNil(FDisabledDecls);
  FreeAndNil(FLogEntries);
  inherited Destroy;
end;

procedure TCSSResolver.Clear;
begin
  ClearStyleSheets;
end;

procedure TCSSResolver.Init;
var
  i: Integer;
begin
  if CSSRegistry.Modified then
  begin
    CSSRegistry.ConsistencyCheck;
    CSSRegistry.Modified:=false;
  end;

  // todo: if CSSRegistry has changed, reparse all stylesheets

  // Note: the rule buckets are built lazily in Compute (see UpdateRuleBuckets),
  // because the stylesheets are added to FLayers after Init.
  // The @media results are computed lazily per rule as well, see AtMediaMatches.

  FMergedAttributesStamp:=1;
  for i:=0 to length(FMergedAttributes)-1 do
    FMergedAttributes[i].Stamp:=0;
end;

procedure TCSSResolver.ClearSharedRuleLists;
begin
  FSharedRuleLists.FreeAndClear;
end;

procedure TCSSResolver.Compute(Node: ICSSNode; ElementStyle: TCSSRuleElement;
  out Rules: TCSSSharedRuleList; out Values: TCSSAttributeValues);
var
  i: Integer;
begin
  Rules:=nil;
  FNode:=Node;
  try
    UpdateRuleBuckets;
    InitMerge;

    FindMatchingRules;

    // create a shared rule list and merge attributes
    Rules:=CreateSharedRuleList;

    // apply inline attributes
    if ElementStyle<>nil then
    begin
      for i:=0 to ElementStyle.ChildCount-1 do
        MergeAttribute(ElementStyle.Children[i],CSSSpecificityElement);
    end;

    LoadMergedValues;
    SubstituteVarCalls; // replace var() calls
    ApplyShorthands;

    // create sorted map AttrId to Value
    Values:=CreateValueList;
  finally
    FNode:=nil;
  end;
end;

function TCSSResolver.HasKeyframesRules: boolean;
begin
  UpdateRuleBuckets;
  Result:=(FKeyframes<>nil) and (FKeyframes.Count>0);
end;

function TCSSResolver.HasStartingStyleRules: boolean;
begin
  UpdateRuleBuckets;
  Result:=(FBucketStartingStyle<>nil) and (FBucketStartingStyle.Count>0);
end;

function TCSSResolver.ComputeStartingStyle(const aNode: ICSSNode; out
  Rules: TCSSSharedRuleList; out Values: TCSSAttributeValues): boolean;
var
  i: Integer;
  OldSrcSpecificity: TCSSSpecificity;
  Item: PCSSRuleBucketItem;
begin
  Result:=false;
  Rules:=nil;
  Values:=nil;
  if not HasStartingStyleRules then exit; // calls UpdateRuleBuckets

  FNode:=aNode;
  OldSrcSpecificity:=FSourceSpecificity;
  try
    InitMerge;

    // Only the @starting-style rules are cascaded. They are collected in document
    // order, so the usual "last declaration wins" tie-break applies.
    FElRuleCount:=0;
    for i:=0 to FBucketStartingStyle.Count-1 do
    begin
      Item:=@FBucketStartingStyle.Items[i];
      ComputeStartingStyleRule(TCSSAtRuleElement(Item^.Rule),Item^.SourceSpecificity);
    end;

    if FElRuleCount=0 then
      exit; // no @starting-style applies to this aNode

    // create a shared rule list and merge attributes
    Rules:=CreateSharedRuleList;

    LoadMergedValues;
    SubstituteVarCalls; // replace var() calls
    ApplyShorthands;

    // create sorted map AttrId to Value
    Values:=CreateValueList;
    Result:=true;
  finally
    FSourceSpecificity:=OldSrcSpecificity;
    FNode:=nil;
  end;
end;

function TCSSResolver.ComputeKeyframes(const aNode: ICSSNode;
  aRule: TCSSAtRuleElement; out Keyframes: TCSSKeyframeArray; out
  AttrIDs: TCSSNumericalIDArray): boolean;
// Cascade the keyframe blocks of aRule, one merge pass per distinct offset.
// Blocks are read in document order and every declaration gets the same specificity,
// so a later declaration wins per property, see MergeAttribute.
var
  BlockOffsets: array of TDoubleDynArray; // per nested rule, nil = block dropped
  UniqueOffsets: TDoubleDynArray;
  OffsetCnt, AttrCnt: integer;

  procedure AddUniqueOffset(d: double);
  // insert d into the ascending UniqueOffsets, skipping a duplicate.
  var
    i: Integer;
  begin
    i:=0;
    while i<OffsetCnt do
    begin
      if SameValue(UniqueOffsets[i],d) then
        exit;
      if UniqueOffsets[i]>d then
        break;
      inc(i);
    end;
    System.Insert(d,UniqueOffsets,i);
    inc(OffsetCnt);
  end;

  function BlockHasOffset(j: integer; d: double): boolean;
  var
    i: Integer;
  begin
    for i:=0 to length(BlockOffsets[j])-1 do
      if SameValue(BlockOffsets[j][i],d) then
        exit(true);
    Result:=false;
  end;

  procedure MergeAttrIDsOf(Values: TCSSAttributeValues);
  // merge AttrIDs of Values into the ascending AttrIDs, dropping duplicates
  var
    i, j, Cnt: Integer;
    AttrID: TCSSNumericalID;
  begin
    Cnt:=length(Values.Values);
    if Cnt=0 then
      exit;
    i:=0;
    j:=0;
    while i<Cnt do
    begin
      AttrID:=Values.Values[i].AttrID;
      while (j<AttrCnt) and (AttrIDs[j]<AttrID) do
        inc(j);
      if (j=AttrCnt) or (AttrIDs[j]>AttrID) then
      begin
        System.Insert(AttrID,AttrIDs,j);
        inc(AttrCnt);
      end;
      inc(i);
    end;
  end;

var
  i, j, k: Integer;
  Block: TCSSRuleElement;
  El: TCSSElement;
  Ofs: TDoubleDynArray;
  OldMergeVars: boolean;
  d: double;
begin
  Result:=false;
  Keyframes:=nil;
  AttrIDs:=nil;
  if aRule=nil then
    exit;
  UpdateRuleBuckets;

  // collect the valid blocks and their offsets, no merging yet
  OffsetCnt:=0;
  UniqueOffsets:=nil;
  BlockOffsets:=nil;
  SetLength(BlockOffsets,aRule.NestedRuleCount);
  for j:=0 to aRule.NestedRuleCount-1 do
  begin
    Block:=aRule.NestedRules[j];
    if not CSSIsPlainRule(Block.ClassType) then
      continue; // e.g. a nested @-rule, not a keyframe
    if not CSSKeyframeOffsets(Block,Ofs) then
    begin
      // css-animations-1 3: an invalid selector drops the whole block.
      {$IFDEF VerboseCSSResolver}
      Log(etWarning,20260904151220,'invalid keyframe selector',Block);
      {$ENDIF}
      continue;
    end;
    BlockOffsets[j]:=Ofs;
    for i:=0 to length(Ofs)-1 do
      AddUniqueOffset(Ofs[i]);
  end;
  if OffsetCnt=0 then
    exit; // no valid keyframe
  SetLength(UniqueOffsets,OffsetCnt);

  // one merge pass per offset
  FNode:=aNode;
  OldMergeVars:=FMergeVarsFromNode;
  FMergeVarsFromNode:=true;
  try
    SetLength(Keyframes,OffsetCnt);
    for i:=0 to OffsetCnt-1 do
    begin
      d:=UniqueOffsets[i];
      Keyframes[i].Offset:=d;
      ClearMerge; // a fresh stamp hides the previous offset's attributes
      for j:=0 to aRule.NestedRuleCount-1 do
      begin
        if BlockOffsets[j]=nil then
          continue;
        if not BlockHasOffset(j,d) then
          continue;
        Block:=aRule.NestedRules[j];
        for k:=0 to Block.ChildCount-1 do
        begin
          El:=Block.Children[k];
          if El.ClassType<>TCSSDeclarationElement then
            continue;
          if TCSSDeclarationElement(El).IsImportant then
          begin
            // css-animations-1 3: an !important declaration in a keyframe is ignored
            {$IFDEF VerboseCSSResolver}
            Log(etWarning,20260904151221,'!important is ignored in a keyframe',El);
            {$ENDIF}
            continue;
          end;
          MergeAttribute(El,CSSSpecificityUniversal);
        end;
      end;
      LoadMergedValues;
      SubstituteVarCalls;
      ApplyShorthands;
      Keyframes[i].Values:=CreateValueList;
    end;
  finally
    FMergeVarsFromNode:=OldMergeVars;
    FNode:=nil;
  end;

  // the union of all attribute ids, ascending
  AttrCnt:=0;
  for i:=0 to OffsetCnt-1 do
    MergeAttrIDsOf(Keyframes[i].Values);
  SetLength(AttrIDs,AttrCnt);

  Result:=true;
end;

function TCSSResolver.GetAttributeID(const aName: TCSSString; AutoCreate: boolean): TCSSNumericalID;
var
  Desc: TCSSResCustomAttributeDesc;
  Cnt: TCSSNumericalID;
begin
  Result:=CSSRegistry.IndexOfAttributeName(aName);
  if Result<0 then
  begin
    Desc:=TCSSResCustomAttributeDesc(FCustomAttributeNameToDesc.Find(aName));
    if Desc<>nil then
      exit(Desc.Index);

    if AutoCreate
        and (length(aName)>2) and (aName[1]='-') and (aName[2]='-')
        and (length(aName)<256) then
    begin
      // create custom attribute
      Cnt:=FCustomAttributeCount;
      if Cnt=length(FCustomAttributes) then
      begin
        if Cnt<32 then
          Cnt:=32
        else
          Cnt:=Cnt*2;
        SetLength(FCustomAttributes,Cnt);
      end;

      Desc:=TCSSResCustomAttributeDesc.Create;
      Desc.Name:=aName;
      Desc.Index:=CSSRegistry.AttributeCount+FCustomAttributeCount;
      Desc.Inherits:=true;
      // the target attribute is unknown, so keep the names case sensitive
      Desc.AllowUnknownIdentifiers:=true;
      FCustomAttributes[FCustomAttributeCount]:=Desc;
      FCustomAttributeNameToDesc.Add(aName,Desc);

      inc(FCustomAttributeCount);

      Result:=Desc.Index;
      Cnt:=GetAttributeID(aName);
      if Cnt<>Result then
        raise ECSSResolver.Create('20240822173412');

      if GetAttributeDesc(Result)<>Desc then
        raise ECSSResolver.Create('20240822174053');
    end;
  end;
end;

procedure TCSSResolver.MediaEnvironmentChanged;
begin
  InvalidateMedia;
end;

procedure TCSSResolver.ClearRuleBuckets;
var
  i: Integer;
begin
  FreeAndNil(FBucketOther);
  for i:=0 to length(FBucketType)-1 do
    FBucketType[i].Free;
  FBucketType:=nil;
  for i:=0 to length(FBucketClass)-1 do
    FBucketClass[i].Free;
  FBucketClass:=nil;
  for i:=0 to length(FBucketID)-1 do
    FBucketID[i].Free;
  FBucketID:=nil;
  FreeAndNil(FBucketStartingStyle);
  FKeyframes.Clear;
  FRuleCandidateCount:=0;
  FBucketDocIndex:=0;
  FSiblingSelectorCount:=0;
  FSiblingSelectors:=nil;
  FRuleBucketsValid:=false;
end;

procedure TCSSResolver.BumpSourceStamp;
begin
  if FSourceStamp<high(FSourceStamp) then
    inc(FSourceStamp)
  else
    FSourceStamp:=1;
end;

procedure TCSSResolver.UpdateRuleBuckets;
begin
  if FRuleBucketsValid then exit;
  BuildRuleBuckets;
end;

procedure TCSSResolver.InvalidateMedia;
begin
  if FMediaStamp<high(FMediaStamp) then
    inc(FMediaStamp)
  else
    FMediaStamp:=1;
  { BuildRuleBuckets skips the rules of a non-matching @media and hoists the rules of a
    matching one, so the buckets depend on the media environment -> drop them as well.
    They are rebuilt lazily by the next UpdateRuleBuckets. }
  ClearRuleBuckets;
end;

procedure TCSSResolver.InvalidateRuleBuckets;
begin
  // kept for compatibility: since the buckets are media dependent this is InvalidateMedia
  InvalidateMedia;
end;

function TCSSResolver.GetTypeBucket(aTypeID: TCSSNumericalID): TCSSRuleBucket;
begin
  if aTypeID>=length(FBucketType) then
    SetLength(FBucketType,aTypeID+1);
  Result:=FBucketType[aTypeID];
  if Result=nil then
  begin
    Result:=TCSSRuleBucket.Create;
    FBucketType[aTypeID]:=Result;
  end;
end;

function TCSSResolver.GetClassBucket(aClassID: TCSSNumericalID): TCSSRuleBucket;
begin
  if aClassID>=length(FBucketClass) then
    SetLength(FBucketClass,aClassID+1);
  Result:=FBucketClass[aClassID];
  if Result=nil then
  begin
    Result:=TCSSRuleBucket.Create;
    FBucketClass[aClassID]:=Result;
  end;
end;

function TCSSResolver.GetIDBucket(aID: TCSSNumericalID): TCSSRuleBucket;
begin
  if aID>=length(FBucketID) then
    SetLength(FBucketID,aID+1);
  Result:=FBucketID[aID];
  if Result=nil then
  begin
    Result:=TCSSRuleBucket.Create;
    FBucketID[aID]:=Result;
  end;
end;

procedure TCSSResolver.GetSelectorBucketKey(aSelector: TCSSElement;
  out aKind: TCSSRuleBucketKind; out NumID: TCSSNumericalID);

  procedure Classify(El: TCSSElement; out aKind: TCSSRuleBucketKind;
    out aNumID: TCSSNumericalID);
  var
    C: TClass;
    TypeID: TCSSNumericalID;
  begin
    aKind:=rbkOther;
    aNumID:=CSSIDNone;
    if El=nil then exit;
    C:=El.ClassType;
    if C=TCSSResolvedHashIdentifierElement then
    begin
      aNumID:=TCSSResolvedHashIdentifierElement(El).NumericalID;
      if aNumID>=1 then
        aKind:=rbkID;
    end
    else if C=TCSSResolvedClassNameElement then
    begin
      aNumID:=TCSSResolvedClassNameElement(El).NumericalID;
      if aNumID>=1 then
        aKind:=rbkClass;
    end
    else if C=TCSSResolvedIdentifierElement then
    begin
      TypeID:=TCSSResolvedIdentifierElement(El).NumericalID;
      // universal '*' and unknown types are not distinctive -> Other
      if (TypeID>=1) and (TypeID<>CSSTypeID_Universal) then
      begin
        aNumID:=TypeID;
        aKind:=rbkType;
      end;
    end;
    // pseudo classes, attribute selectors, calls (:is/:not), '&' etc. -> Other
  end;

var
  aBinary: TCSSBinaryElement;
  aUnary: TCSSUnaryElement;
  aList: TCSSListElement;
  i: Integer;
  ChildKind: TCSSRuleBucketKind;
  ChildNum: TCSSNumericalID;
begin
  aKind:=rbkOther;
  NumID:=CSSIDNone;

  // descend combinators to the rightmost selector part
  while aSelector<>nil do
  begin
    if aSelector.ClassType=TCSSBinaryElement then
    begin
      aBinary:=TCSSBinaryElement(aSelector);
      case aBinary.Operation of
      boWhiteSpace,boGT,boPlus,boTilde:
        begin
          aSelector:=aBinary.Right;
          continue;
        end;
      end;
      break;
    end
    else if aSelector.ClassType=TCSSUnaryElement then
    begin
      aUnary:=TCSSUnaryElement(aSelector);
      case aUnary.Operation of
      uoGT,uoPlus,uoTilde:
        begin
          aSelector:=aUnary.Right;
          continue;
        end;
      end;
      break;
    end;
    break;
  end;
  if aSelector=nil then exit;

  if aSelector.ClassType=TCSSListElement then
  begin
    // compound selector e.g. div.red#x -> pick the most distinctive part: id > class > type
    aList:=TCSSListElement(aSelector);
    for i:=0 to aList.ChildCount-1 do
    begin
      Classify(aList.Children[i],ChildKind,ChildNum);
      if ChildKind>aKind then
      begin
        aKind:=ChildKind;
        NumID:=ChildNum;
        if aKind=rbkID then break; // id is the most distinctive, stop early
      end;
    end;
  end
  else
    Classify(aSelector,aKind,NumID);
end;

procedure TCSSResolver.BucketRule(aRule: TCSSRuleElement; SrcSpecificity: TCSSSpecificity);
var
  i: Integer;
  aKind: TCSSRuleBucketKind;
  NumID: TCSSNumericalID;
  DocIndex: Integer;
begin
  DocIndex:=FBucketDocIndex;
  inc(FBucketDocIndex);

  if aRule.ClassType=TCSSAtRuleElement then
  begin
    if TCSSAtRuleElement(aRule).AtKeyWord='@media' then
    begin
      if not AtMediaMatches(TCSSAtRuleElement(aRule)) then
        exit; // does not match -> nothing in the subtree can apply to any node
      // It matches, so for the cascade it is transparent: bucket its nested rules at
      // this position. The declarations written directly in a top level @media are
      // never applied (ComputeAtRule only adds them for ParentSpecificity>=0, and a
      // bucket candidate is computed with the default CSSSpecificityNoMatch), so the
      // at-rule itself needs no bucket item.
      // Note: a @media nested in a style rule is not bucketed at all. It is reached via
      // ComputeRule of that style rule, which does the media check per node.
      for i:=0 to aRule.NestedRuleCount-1 do
        BucketRule(aRule.NestedRules[i],SrcSpecificity);
      exit;
    end;
    // @keyframes, @starting-style and unknown @-rules keep the original recursion
    FBucketOther.Add(aRule,DocIndex,SrcSpecificity);
    exit;
  end;

  // a rule with nested rules keeps the original recursion (nested ancestor matching)
  if aRule.NestedRuleCount>0 then
  begin
    FBucketOther.Add(aRule,DocIndex,SrcSpecificity);
    exit;
  end;

  if aRule.SelectorCount=0 then
  begin
    FBucketOther.Add(aRule,DocIndex,SrcSpecificity);
    exit;
  end;

  for i:=0 to aRule.SelectorCount-1 do
  begin
    GetSelectorBucketKey(aRule.Selectors[i],aKind,NumID);
    case aKind of
    rbkID:    GetIDBucket(NumID).Add(aRule,DocIndex,SrcSpecificity);
    rbkClass: GetClassBucket(NumID).Add(aRule,DocIndex,SrcSpecificity);
    rbkType:  GetTypeBucket(NumID).Add(aRule,DocIndex,SrcSpecificity);
    else
      FBucketOther.Add(aRule,DocIndex,SrcSpecificity);
    end;
  end;
end;

procedure TCSSResolver.CollectStartingStyleRules(aRule: TCSSRuleElement;
  SrcSpecificity: TCSSSpecificity);
// Add every outermost @starting-style at-rule of aRule's subtree to
// FBucketStartingStyle, in document order. These rules are skipped by the normal
// cascade (see ComputeAtRule) and only evaluated by ComputeStartingStyle.
// The recursion stops at a @starting-style, because ComputeStartingStyleRule
// descends into its nested rules itself.
var
  i: Integer;
begin
  if aRule=nil then exit;
  if aRule.ClassType=TCSSAtRuleElement then
  begin
    if TCSSAtRuleElement(aRule).AtKeyWord='@starting-style' then
    begin
      FBucketStartingStyle.Add(aRule,0,SrcSpecificity);
      exit;
    end;
    if (TCSSAtRuleElement(aRule).AtKeyWord='@media')
        and not AtMediaMatches(TCSSAtRuleElement(aRule)) then
      exit; // the media query does not match -> nothing in the subtree applies
  end;
  for i:=0 to aRule.NestedRuleCount-1 do
    CollectStartingStyleRules(aRule.NestedRules[i],SrcSpecificity);
end;

procedure TCSSResolver.CollectKeyframes(aRule: TCSSRuleElement);
// Add every @keyframes at-rule of aRule's subtree to FKeyframes, in document order.
// Requires the TCSSRuleData of the subtree to be up to date, see UpdateSourceOfElement.
var
  i: Integer;
begin
  if aRule=nil then exit;
  if aRule.ClassType=TCSSAtRuleElement then
  begin
    if CSSIsKeyframesAtKeyword(TCSSAtRuleElement(aRule).AtKeyWord) then
    begin
      AddKeyframes(TCSSAtRuleElement(aRule));
      // its nested rules are the keyframes ('from', '50%', ...), not @keyframes rules
      exit;
    end;
    if (TCSSAtRuleElement(aRule).AtKeyWord='@media')
        and not AtMediaMatches(TCSSAtRuleElement(aRule)) then
      exit; // the media query does not match -> nothing in the subtree applies
  end;
  for i:=0 to aRule.NestedRuleCount-1 do
    CollectKeyframes(aRule.NestedRules[i]);
end;

procedure TCSSResolver.AddKeyframes(aRule: TCSSAtRuleElement);
// Add one @keyframes at-rule to FKeyframes, keyed by its animation name.
// A top level @keyframes supersedes an earlier top level one with the same name.
// A @keyframes nested in a style rule is kept even when a same-named one exists,
// because it applies to other nodes, see FindKeyframesRule.
var
  aName: TCSSString;
  i: SizeInt;
  OldRule: TCSSAtRuleElement;
begin
  aName:=CSSKeyframesName(aRule);
  if aName='' then
    exit; // no name, e.g. '@keyframes {'

  // the stylesheets are parsed by ParseCSSSource, so every rule has a TCSSRuleData
  if RuleData(aRule).StyleRuleParent=nil then
  begin
    i:=FKeyframes.FindIndexOf(aName);
    while i>=0 do
    begin
      OldRule:=TCSSAtRuleElement(FKeyframes[i]);
      if (FKeyframes.NameOfIndex(i)=aName)
          and (RuleData(OldRule).StyleRuleParent=nil) then
      begin
        // Delete instead of replacing the item, so the new rule is added at the end
        // and the collision chain keeps running backwards in document order.
        FKeyframes.Delete(i);
        break;
      end;
      i:=FKeyframes.GetNextCollision(i);
    end;
  end;

  FKeyframes.Add(aName,aRule);
end;

function TCSSResolver.KeyframesContextMatches(aRule: TCSSAtRuleElement;
  const aNode: ICSSNode): boolean;
// Check the ancestors of a @keyframes rule for aNode, the counterpart of
// StartingStyleContextMatches.
// Result=false: an enclosing style rule does not match aNode, an enclosing @media
//   does not match, or an unsupported @-rule (e.g. @supports) encloses aRule.
var
  El: TCSSElement;
  C: TClass;
  aStyleRuleParent: TCSSRuleElement;
begin
  Result:=false;

  // aRule comes from FKeyframes, i.e. from a stylesheet -> it has a TCSSRuleData
  aStyleRuleParent:=RuleData(aRule).StyleRuleParent;
  if aStyleRuleParent<>nil then
  begin
    // e.g. 'div{ @keyframes fade{} }' applies only to the nodes 'div' matches.
    // Only the innermost style rule needs a check: SelectorMatches resolves the
    // whole outer nesting chain, see StartingStyleContextMatches.
    if GetRuleSpecificity(aStyleRuleParent,aNode)<0 then exit;
  end;

  El:=aRule.Parent;
  while El<>nil do
  begin
    C:=El.ClassType;
    if C.InheritsFrom(TCSSAtRuleElement) then
    begin
      if TCSSAtRuleElement(El).AtKeyWord='@media' then
      begin
        if not AtMediaMatches(TCSSAtRuleElement(El)) then exit;
      end
      else
        exit; // unknown @-rule, e.g. @supports -> dropped, like in ComputeAtRule
    end;
    El:=El.Parent;
  end;
  Result:=true;
end;

function TCSSResolver.FindKeyframesRule(const aNode: ICSSNode;
  const aName: TCSSString): TCSSAtRuleElement;
var
  i: SizeInt;
  aRule: TCSSAtRuleElement;
begin
  Result:=nil;
  if aName='' then exit;
  UpdateRuleBuckets;

  // The collision chain runs from the last added item to the first, i.e. backwards in
  // document order, and it can contain other names -> check the name of every item.
  i:=FKeyframes.FindIndexOf(aName);
  while i>=0 do
  begin
    if FKeyframes.NameOfIndex(i)=aName then
    begin
      aRule:=TCSSAtRuleElement(FKeyframes[i]);
      if KeyframesContextMatches(aRule,aNode) then
        exit(aRule);
    end;
    i:=FKeyframes.GetNextCollision(i);
  end;
end;

procedure TCSSResolver.CollectSiblingSelectors(aRule: TCSSRuleElement;
  SrcSpecificity: TCSSSpecificity);
// Add every selector of aRule (and its nested rules) whose subject match depends on
// siblings/position/descendants to FSiblingSelectors. @-rule "selectors" are media
// queries, not node selectors, so only their nested rules are descended.
var
  i: Integer;
begin
  if aRule=nil then exit;
  if aRule.ClassType=TCSSAtRuleElement then
  begin
    if (TCSSAtRuleElement(aRule).AtKeyWord='@media')
        and not AtMediaMatches(TCSSAtRuleElement(aRule)) then
      exit; // the media query does not match -> nothing in the subtree applies
  end
  else
    for i:=0 to aRule.SelectorCount-1 do
      if SelectorHasSiblingDependency(aRule.Selectors[i]) then
        AddSiblingSelector(aRule.Selectors[i],aRule,SrcSpecificity);
  for i:=0 to aRule.NestedRuleCount-1 do
    CollectSiblingSelectors(aRule.NestedRules[i],SrcSpecificity);
end;

procedure TCSSResolver.AddSiblingSelector(aSelector: TCSSElement;
  aRule: TCSSRuleElement; SrcSpecificity: TCSSSpecificity);
var
  l: Integer;
begin
  l:=length(FSiblingSelectors);
  if FSiblingSelectorCount=l then
  begin
    if l<8 then l:=8 else l:=l*2;
    SetLength(FSiblingSelectors,l);
  end;
  FSiblingSelectors[FSiblingSelectorCount].Selector:=aSelector;
  FSiblingSelectors[FSiblingSelectorCount].Rule:=aRule;
  FSiblingSelectors[FSiblingSelectorCount].SrcSpecificity:=SrcSpecificity;
  inc(FSiblingSelectorCount);
end;

function TCSSResolver.SelectorHasSiblingDependency(aSelector: TCSSElement): boolean;
// Walks the subject (rightmost) side of aSelector. Combinators/pseudos in an ancestor
// part are deliberately not followed - same-parent siblings share their ancestors.
var
  aBinary: TCSSBinaryElement;
  aUnary: TCSSUnaryElement;
  aList: TCSSListElement;
  i: Integer;
begin
  Result:=false;
  if aSelector=nil then exit;
  if aSelector.ClassType=TCSSBinaryElement then
  begin
    aBinary:=TCSSBinaryElement(aSelector);
    case aBinary.Operation of
    boPlus,boTilde: Result:=true; // subject preceded by a (matching) sibling
    else
      // descendant/child combinator (or attribute binary): only the subject matters
      Result:=SelectorHasSiblingDependency(aBinary.Right);
    end;
  end
  else if aSelector.ClassType=TCSSUnaryElement then
  begin
    aUnary:=TCSSUnaryElement(aSelector);
    case aUnary.Operation of
    uoPlus,uoTilde: Result:=true;
    uoGT: Result:=SelectorHasSiblingDependency(aUnary.Right);
    end;
  end
  else if aSelector.ClassType=TCSSListElement then
  begin
    aList:=TCSSListElement(aSelector);
    for i:=0 to aList.ChildCount-1 do
      if SelectorComponentSiblingDep(aList.Children[i]) then exit(true);
  end
  else
    Result:=SelectorComponentSiblingDep(aSelector);
end;

function TCSSResolver.SelectorComponentSiblingDep(aComponent: TCSSElement): boolean;
// A single component of the subject compound (pseudo-class, call, type, class, ...).
var
  aCall: TCSSResolvedCallElement;
  i: Integer;
begin
  Result:=false;
  if aComponent=nil then exit;
  if aComponent.ClassType=TCSSResolvedPseudoClassElement then
  begin
    case TCSSResolvedPseudoClassElement(aComponent).NumericalID of
    CSSPseudoID_Empty,
    CSSPseudoID_FirstChild,CSSPseudoID_LastChild,CSSPseudoID_OnlyChild,
    CSSPseudoID_FirstOfType,CSSPseudoID_LastOfType,CSSPseudoID_OnlyOfType:
      Result:=true;
    end;
  end
  else if aComponent.ClassType=TCSSResolvedCallElement then
  begin
    aCall:=TCSSResolvedCallElement(aComponent);
    case aCall.NameNumericalID of
    CSSCallID_NthChild,CSSCallID_NthLastChild,
    CSSCallID_NthOfType,CSSCallID_NthLastOfType,
    CSSCallID_Has:
      Result:=true;
    CSSCallID_Not,CSSCallID_Is,CSSCallID_Where:
      // forgiving wrappers apply to the same subject node -> recurse into the args
      for i:=0 to aCall.ArgCount-1 do
        if SelectorHasSiblingDependency(aCall.Args[i]) then exit(true);
    end;
  end;
end;

function TCSSResolver.MatchSiblingSelectors(const Node: ICSSNode): TCSSSiblingMatchList;
var
  i, Cnt: Integer;
  SavedNode: ICSSNode;
  Sel: TCSSElement;
begin
  Result.Matched:=nil;
  if FSiblingSelectorCount=0 then exit;

  SetLength(Result.Matched,FSiblingSelectorCount);
  Cnt:=0;
  SavedNode:=FNode;
  FNode:=Node;
  try
    for i:=0 to FSiblingSelectorCount-1 do
    begin
      Sel:=FSiblingSelectors[i].Selector;
      if SelectorMatches(Sel,Node,false,FSiblingSelectors[i].Rule)>=0 then
      begin
        Result.Matched[Cnt]:=Sel;
        inc(Cnt);
      end;
    end;
  finally
    FNode:=SavedNode;
  end;
  SetLength(Result.Matched,Cnt);
end;

procedure TCSSResolver.BuildRuleBuckets;

  procedure CollectEl(El: TCSSElement; SrcSpecificity: TCSSSpecificity);
  var
    C: TClass;
    i: Integer;
  begin
    if El=nil then exit;
    C:=El.ClassType;
    if C=TCSSCompoundElement then
    begin
      for i:=0 to TCSSCompoundElement(El).ChildCount-1 do
        CollectEl(TCSSCompoundElement(El).Children[i],SrcSpecificity);
    end
    else if C.InheritsFrom(TCSSRuleElement) then
    begin
      BucketRule(TCSSRuleElement(El),SrcSpecificity);
      CollectSiblingSelectors(TCSSRuleElement(El),SrcSpecificity);
      CollectStartingStyleRules(TCSSRuleElement(El),SrcSpecificity);
      CollectKeyframes(TCSSRuleElement(El));
    end;
    // unknown top-level elements are ignored here (warned by ComputeElement)
  end;

var
  aLayerIndex, i: Integer;
  SrcSpecificity: TCSSSpecificity;
begin
  ClearRuleBuckets;
  FBucketOther:=TCSSRuleBucket.Create;
  FBucketStartingStyle:=TCSSRuleBucket.Create;

  // pre-size the type/class/id bucket arrays to their final length, so the
  // GetTypeBucket/GetClassBucket/GetIDBucket calls in BucketRule never grow
  // (and reallocate) them one id at a time. The bucket objects themselves are
  // still created lazily; index 0 is the unused CSSIDNone slot.
  if CSSRegistry.TypeCount>0 then
    SetLength(FBucketType,CSSRegistry.TypeCount);
  if FCSSClassNameCount>0 then
    SetLength(FBucketClass,FCSSClassNameCount+1);
  if FCSSIDCount>0 then
    SetLength(FBucketID,FCSSIDCount+1);

  BumpSourceStamp;

  // walk in the same order FindMatchingRules used, assigning document order indexes
  for aLayerIndex:=0 to length(FLayers)-1 do
    with FLayers[aLayerIndex] do
    begin
      SrcSpecificity:=CSSOriginToSpecifity[Origin];
      for i:=0 to ElementCount-1 do
      begin
        UpdateSourceOfElement(Elements[i].Element,Origin,IndexOfStyleSheet(Elements[i].Src));
        CollectEl(Elements[i].Element,SrcSpecificity);
      end;
    end;
  FRuleBucketsValid:=true;
end;

procedure TCSSResolver.UpdateSourceOfElement(El: TCSSElement;
  anOrigin: TCSSOrigin; aSheetIndex: integer; aStyleRuleParent: TCSSRuleElement);
var
  C: TClass;
  i: Integer;
  aRule: TCSSRuleElement;
  RData: TCSSRuleData;
begin
  if El=nil then exit;
  C:=El.ClassType;
  if C=TCSSCompoundElement then
  begin
    for i:=0 to TCSSCompoundElement(El).ChildCount-1 do
      UpdateSourceOfElement(TCSSCompoundElement(El).Children[i],anOrigin,aSheetIndex,
                            aStyleRuleParent);
    exit;
  end;
  if not C.InheritsFrom(TCSSRuleElement) then exit;

  aRule:=TCSSRuleElement(El);
  // the stylesheets are parsed by ParseCSSSource, so every rule has a TCSSRuleData
  RData:=RuleData(aRule);
  RData.Origin:=anOrigin;
  RData.SourceIndex:=aSheetIndex;
  RData.StyleRuleParent:=aStyleRuleParent;
  RData.SourceStamp:=FSourceStamp;

  // an @-rule is not a style rule, so it does not become the parent of its nested rules
  if CSSIsPlainRule(C) then
    aStyleRuleParent:=aRule;
  for i:=0 to aRule.NestedRuleCount-1 do
    UpdateSourceOfElement(aRule.NestedRules[i],anOrigin,aSheetIndex,aStyleRuleParent);
end;

procedure TCSSResolver.AddBucketToRuleCandidates(Bucket: TCSSRuleBucket);
var
  l: SizeInt;
begin
  if (Bucket=nil) or (Bucket.Count=0) then exit;
  l:=length(FRuleCandidates);
  if FRuleCandidateCount+Bucket.Count>l then
  begin
    if l<8 then l:=8;
    while l<FRuleCandidateCount+Bucket.Count do
      l:=l*2;
    SetLength(FRuleCandidates,l);
  end;
  Move(Bucket.Items[0],FRuleCandidates[FRuleCandidateCount],Bucket.Count*SizeOf(TCSSRuleBucketItem));
  inc(FRuleCandidateCount,Bucket.Count);
end;

procedure TCSSResolver.SortRuleCandidates;

  procedure QuickSort(L, R: Integer);
  var
    i, j, Pivot: Integer;
    Tmp: TCSSRuleBucketItem;
  begin
    i:=L;
    j:=R;
    Pivot:=FRuleCandidates[(L+R) div 2].DocIndex;
    repeat
      while FRuleCandidates[i].DocIndex<Pivot do inc(i);
      while FRuleCandidates[j].DocIndex>Pivot do dec(j);
      if i<=j then
      begin
        Tmp:=FRuleCandidates[i];
        FRuleCandidates[i]:=FRuleCandidates[j];
        FRuleCandidates[j]:=Tmp;
        inc(i);
        dec(j);
      end;
    until i>j;
    if L<j then QuickSort(L,j);
    if i<R then QuickSort(i,R);
  end;

begin
  if FRuleCandidateCount>1 then
    QuickSort(0,FRuleCandidateCount-1);
end;

procedure TCSSResolver.FindMatchingRules;
var
  i, c: Integer;
  Classes: TCSSNumericalIDArray;
  TypeID, IDIndex: TCSSNumericalID;
  LastRule: TCSSRuleElement;
  Item: PCSSRuleBucketItem;
begin
  FElRuleCount:=0;
  FRuleCandidateCount:=0;

  // only the buckets that can match this node are inspected:
  // the Other bucket, the node's type bucket, its id bucket and its class buckets
  AddBucketToRuleCandidates(FBucketOther);

  TypeID:=FNode.GetCSSTypeID;
  if (TypeID>=0) and (TypeID<length(FBucketType)) then
    AddBucketToRuleCandidates(FBucketType[TypeID]);

  IDIndex:=FNode.GetCSSID;
  if (IDIndex>=1) and (IDIndex<length(FBucketID)) then
    AddBucketToRuleCandidates(FBucketID[IDIndex]);

  Classes:=FNode.GetCSSClasses;
  for i:=0 to length(Classes)-1 do
  begin
    c:=Classes[i];
    if (c>=1) and (c<length(FBucketClass)) then
      AddBucketToRuleCandidates(FBucketClass[c]);
  end;

  // restore document order so the cascade tie-break is identical to a linear scan
  SortRuleCandidates;

  // evaluate each candidate rule once (a multi-selector rule can appear in
  // several buckets; duplicates share a DocIndex and are now adjacent)
  LastRule:=nil;
  for i:=0 to FRuleCandidateCount-1 do
  begin
    Item:=@FRuleCandidates[i];
    if Item^.Rule=LastRule then continue;
    LastRule:=Item^.Rule;
    FSourceSpecificity:=Item^.SourceSpecificity;
    if Item^.Rule.ClassType=TCSSAtRuleElement then
      ComputeAtRule(TCSSAtRuleElement(Item^.Rule))
    else
      ComputeRule(Item^.Rule);
  end;
end;

function TCSSResolver.GetAttributeDesc(AttrId: TCSSNumericalID
  ): TCSSAttributeDesc;
begin
  Result:=nil;
  if AttrID<CSSRegistry.AttributeCount then
    Result:=CSSRegistry.Attributes[AttrId]
  else begin
    dec(AttrID,CSSRegistry.AttributeCount);
    if AttrID<FCustomAttributeCount then
      Result:=FCustomAttributes[AttrId];
  end;
end;

function TCSSResolver.IsAttrValueInvalid(AttrID: TCSSNumericalID;
  const Tokens: TBytes): boolean;
// Run an already tokenized value through the attribute parser and its OnCheck, exactly
// as the parser does per declaration. A temporary key data lets InitParseAttr flag a
// misused base keyword (e.g. "inherit red") as well.
var
  Desc: TCSSAttributeDesc;
  AttrData: TCSSAttributeKeyData;
begin
  Result:=false;
  // only built-in attributes from 'all' upwards are checkable; the ones below 'all' and
  // the custom properties (>=AttributeCount) accept anything
  if (AttrID<CSSAttributeID_All) or (AttrID>=CSSRegistry.AttributeCount) then
    exit;
  Desc:=GetAttributeDesc(AttrID);
  if Desc=nil then exit;
  AttrData:=TCSSAttributeKeyData.Create;
  try
    AttrData.Tokens:=Tokens;
    if InitParseAttr(Desc,AttrData) then
      if Assigned(Desc.OnCheck) and not Desc.OnCheck(Self) then
        AttrData.Invalid:=true;
    Result:=AttrData.Invalid; // InitParseAttr may have flagged it too
  finally
    AttrData.Free;
  end;
end;

function TCSSResolver.IsAttrValueInvalid(AttrID: TCSSNumericalID;
  const aValue: TCSSString): boolean;
// Tokenize aValue as the attribute expects, then check the tokens (see above).
var
  Desc: TCSSAttributeDesc;
  Tokens: TBytes;
begin
  Result:=false;
  if HasCSSValueVarCall(aValue) then
    exit; // a var() cannot be checked until it is substituted
  if (AttrID<CSSAttributeID_All) or (AttrID>=CSSRegistry.AttributeCount) then
    exit;
  Desc:=GetAttributeDesc(AttrID);
  if Desc=nil then exit;
  Tokens:=nil;
  if not Tokenize(aValue,Tokens,Desc.AllowUnknownIdentifiers) then
    exit(true); // the value does not even tokenize
  Result:=IsAttrValueInvalid(AttrID,Tokens);
end;

function TCSSResolver.GetCSSClassID(const aCSSClassName: TCSSString
  ): TCSSNumericalID;
var
  p: Pointer;
begin
  // class names are case sensitive
  p:=FCSSClassNameToID.Find(aCSSClassName);
  if p=nil then
    Result:=CSSIDNone
  else
    Result:={%H-}TCSSNumericalID(p);
end;

function TCSSResolver.AddCSSClassID(const aCSSClassName: TCSSString
  ): TCSSNumericalID;
var
  Cnt: SizeInt;
begin
  Result:=GetCSSClassID(aCSSClassName);
  if Result<>CSSIDNone then exit;
  inc(FCSSClassNameCount);
  Result:=FCSSClassNameCount;
  FCSSClassNameToID.Add(aCSSClassName,{%H-}Pointer(Result));
  Cnt:=length(FCSSClassNames);
  if Cnt<FCSSClassNameCount then
  begin
    if Cnt<8 then
      Cnt:=8
    else
      Cnt:=Cnt*2;
    SetLength(FCSSClassNames,Cnt);
  end;
  FCSSClassNames[Result-1]:=aCSSClassName;
  ChangeCSSClassIDStamp;
end;

function TCSSResolver.GetCSSClassName(aID: TCSSNumericalID): TCSSString;
begin
  if (aID>=1) and (aID<=FCSSClassNameCount) then
    Result:=FCSSClassNames[aID-1]
  else
    Result:='';
end;

function TCSSResolver.GetCSSIDIndex(const aCSSID: TCSSString): TCSSNumericalID;
var
  p: Pointer;
begin
  // ids are case sensitive
  p:=FCSSIDNameToIndex.Find(aCSSID);
  if p=nil then
    Result:=CSSIDNone
  else
    Result:={%H-}TCSSNumericalID(p);
end;

function TCSSResolver.AddCSSID(const aCSSID: TCSSString): TCSSNumericalID;
var
  Cnt: SizeInt;
begin
  Result:=GetCSSIDIndex(aCSSID);
  if Result<>CSSIDNone then exit;
  inc(FCSSIDCount);
  Result:=FCSSIDCount;
  FCSSIDNameToIndex.Add(aCSSID,{%H-}Pointer(Result));
  Cnt:=length(FCSSIDNames);
  if Cnt<FCSSIDCount then
  begin
    if Cnt<8 then
      Cnt:=8
    else
      Cnt:=Cnt*2;
    SetLength(FCSSIDNames,Cnt);
  end;
  FCSSIDNames[Result-1]:=aCSSID;
  ChangeCSSClassIDStamp;
end;

function TCSSResolver.GetCSSIDName(aID: TCSSNumericalID): TCSSString;
begin
  if (aID>=1) and (aID<=FCSSIDCount) then
    Result:=FCSSIDNames[aID-1]
  else
    Result:='';
end;

function TCSSResolver.GetDeclarationTokens(Decl: TCSSDeclarationElement): TBytes;
var
  KeyData: TCSSAttributeKeyData;
begin
  Result:=nil;
  if Decl=nil then exit;
  if Decl.KeyCount=0 then exit;
  KeyData:=TCSSAttributeKeyData(Decl.Keys[0].CustomData);
  if KeyData=nil then exit;
  Result:=KeyData.Tokens;
end;

function TCSSResolver.GetDeclarationValue(Decl: TCSSDeclarationElement): TCSSString;
begin
  Result:=Detokenize(GetDeclarationTokens(Decl));
end;

procedure TCSSResolver.ClearStyleSheets;
var
  i: Integer;
begin
  ClearElements;

  FDisabledDecls.Clear;

  // clear stylesheets. ClearElements above freed the elements and bumped the stamp,
  // this bumps again for the TStyleSheet objects freed here.
  BumpSourceStamp;
  for i:=0 to FStyleSheetCount-1 do
  begin
    FreeAndNil(FStyleSheets[i].Element);
    FreeAndNil(FStyleSheets[i]);
  end;
  FStyleSheetCount:=0;
end;

function TCSSResolver.AddStyleSheet(anOrigin: TCSSOrigin; const aName: TCSSString;
  const aSource: TCSSString): TStyleSheet;
begin
  Result:=InsertStyleSheet(FStyleSheetCount,anOrigin,aName,aSource);
end;

function TCSSResolver.InsertStyleSheet(Index: integer; anOrigin: TCSSOrigin;
  const aName: TCSSString; const aSource: TCSSString): TStyleSheet;
var
  Cnt, i: SizeInt;
begin
  if aName>'' then
  begin
    i:=IndexOfStyleSheetWithName(anOrigin,aName);
    if i>=0 then
    begin
      ReplaceStyleSheet(i,aSource);
      exit(FStyleSheets[i]);
    end;
  end;

  if (Index<0) or (Index>FStyleSheetCount) then
    raise ECSSResolver.Create('20260706153000');

  Cnt:=length(FStyleSheets);
  if Cnt=FStyleSheetCount then
  begin
    if Cnt<32 then
      Cnt:=32
    else
      Cnt:=Cnt*2;
    SetLength(FStyleSheets,Cnt);
  end;

  // repurpose the pooled object in the tail slot, then open a gap at Index
  Result:=FStyleSheets[FStyleSheetCount];
  for i:=FStyleSheetCount downto Index+1 do
    FStyleSheets[i]:=FStyleSheets[i-1];
  if Result=nil then
    Result:=TStyleSheet.Create;
  FStyleSheets[Index]:=Result;
  inc(FStyleSheetCount);

  with Result do begin
    Name:=aName;
    Origin:=anOrigin;
    Source:=aSource;
    Parsed:=false;
    if Element<>nil then
      FreeAndNil(Element);
    inc(FStyleSheetStamp);
    Stamp:=FStyleSheetStamp;
  end;

  ParseSource(Index);

  // ParseSource appended the sheet's element to the end of its layer; when this
  // is a mid-list insert, move it to the slot matching the array order so the
  // cascade (document order) respects the requested position.
  if Index<FStyleSheetCount-1 then
  begin
    // the following stylesheets moved up, so the indexes cached in the rule data
    // are stale, see TCSSRuleData.SourceIndex. Note that ParseSource and
    // OrderStyleSheetInLayer do not invalidate when the sheet has no element,
    // e.g. an empty source.
    FRuleBucketsValid:=false;
    OrderStyleSheetInLayer(Index);
  end;
end;

procedure TCSSResolver.ReplaceStyleSheet(Index: integer; const NewSource: TCSSString);
var
  Sheet: TStyleSheet;
begin
  Sheet:=StyleSheets[Index];
  if NewSource=Sheet.Source then exit;
  ClearMerge;
  ClearSharedRuleLists;
  { ParseSource appended this sheet's element to the layers; drop that entry
    before freeing the element, or the layers keep a dangling pointer that
    FindMatchingRules would later walk (a use-after-free). }
  RemoveStyleSheetFromLayers(Sheet);
  { The rules of this sheet are about to be freed, so every pointer to one of them
    and every TCSSRuleData cached from them is stale from here on. The buckets are
    rebuilt lazily, i.e. their stamp bump comes too late for a user reading a rule
    pointer between this change and the next cascade. }
  BumpSourceStamp;
  FreeAndNil(Sheet.Element);
  Sheet.Parsed:=false;
  Sheet.Source:=NewSource;
  inc(FStyleSheetStamp);
  Sheet.Stamp:=FStyleSheetStamp;

  ParseSource(Index);

  // the reparse replaced all declaration elements with fresh ones (Disabled=false);
  // re-apply the remembered disabled state
  RestoreDisabledDeclarations(Sheet);
end;

procedure TCSSResolver.DeleteStyleSheet(Index: integer);
var
  Sheet: TStyleSheet;
  i: Integer;
  Item: TCSSDisabledDecl;
begin
  if (Index<0) or (Index>=FStyleSheetCount) then
    raise ECSSResolver.Create('20260706120000');
  Sheet:=FStyleSheets[Index];
  ClearMerge;
  ClearSharedRuleLists;
  { The sheet's element (and all its declaration elements) is about to be freed.
    Drop it from the layers first, or FindMatchingRules would walk a dangling
    pointer (a use-after-free). }
  RemoveStyleSheetFromLayers(Sheet);

  // forget disabled declarations of this sheet; their Decl elements are freed below
  for i:=FDisabledDecls.Count-1 downto 0 do
  begin
    Item:=TCSSDisabledDecl(FDisabledDecls[i]);
    if (Item.Path.Origin=Sheet.Origin) and (Item.Path.SheetName=Sheet.Name) then
      FDisabledDecls.Delete(i); // owns the object, frees it
  end;

  // the rules are freed here, see the BumpSourceStamp in ReplaceStyleSheet
  BumpSourceStamp;
  FreeAndNil(Sheet.Element);
  FreeAndNil(FStyleSheets[Index]);

  // close the gap; the vacated tail slot stays nil for AddStyleSheet to reuse
  for i:=Index to FStyleSheetCount-2 do
    FStyleSheets[i]:=FStyleSheets[i+1];
  FStyleSheets[FStyleSheetCount-1]:=nil;
  dec(FStyleSheetCount);

  inc(FStyleSheetStamp);
end;


procedure TCSSResolver.RemoveStyleSheetFromLayers(aSheet: TStyleSheet);
var
  l, i, d: Integer;
begin
  FRuleBucketsValid:=false; // FLayers changing -> buckets need a rebuild
  for l:=0 to length(FLayers)-1 do
    with FLayers[l] do
    begin
      d:=0;
      for i:=0 to ElementCount-1 do
        if Elements[i].Src<>aSheet then
        begin
          if d<>i then
            Elements[d]:=Elements[i];
          inc(d);
        end;
      ElementCount:=d;
    end;
end;

procedure TCSSResolver.OrderStyleSheetInLayer(Index: integer);
// ParseSource gives each stylesheet its own anonymous layer, appended at the end
// of its origin's contiguous run, so the cascade document order is the layer
// order. After a mid-list InsertStyleSheet, move that fresh layer to the slot
// matching the FStyleSheets order, so ancestors keep coming before descendants.
var
  aSheet: TStyleSheet;
  El: TCSSElement;
  l, srcLayer, runStart, destLayer, otherIdx: Integer;
  Tmp: TLayer;
begin
  aSheet:=FStyleSheets[Index];
  El:=aSheet.Element;
  if El=nil then exit; // empty stylesheet contributes no layer

  // locate the layer holding this sheet's element
  srcLayer:=-1;
  for l:=0 to length(FLayers)-1 do
    if (FLayers[l].ElementCount>0) and (FLayers[l].Elements[0].Src=aSheet) then
    begin
      srcLayer:=l;
      break;
    end;
  if srcLayer<0 then exit;

  // first layer of this origin (origins are sorted, so the run is contiguous)
  runStart:=0;
  while (runStart<length(FLayers)) and (FLayers[runStart].Origin<>aSheet.Origin) do
    inc(runStart);

  // destination = runStart + number of same-origin layers whose sheet precedes Index
  destLayer:=runStart;
  for l:=0 to length(FLayers)-1 do
  begin
    if (l=srcLayer) or (FLayers[l].Origin<>aSheet.Origin) or (FLayers[l].ElementCount=0) then
      continue;
    otherIdx:=IndexOfStyleSheet(FLayers[l].Elements[0].Src);
    if (otherIdx>=0) and (otherIdx<Index) then
      inc(destLayer);
  end;

  if destLayer=srcLayer then exit;

  Tmp:=FLayers[srcLayer];
  if destLayer<srcLayer then
  begin
    for l:=srcLayer downto destLayer+1 do
      FLayers[l]:=FLayers[l-1];
  end else begin
    for l:=srcLayer to destLayer-1 do
      FLayers[l]:=FLayers[l+1];
  end;
  FLayers[destLayer]:=Tmp;
  FRuleBucketsValid:=false; // document order changed -> rebuild buckets
end;

function TCSSResolver.IndexOfStyleSheet(aSheet: TStyleSheet): integer;
begin
  for Result:=0 to FStyleSheetCount-1 do
    if FStyleSheets[Result]=aSheet then
      exit;
  Result:=-1;
end;

function TCSSResolver.IndexOfStyleSheetWithElement(El: TCSSElement): integer;
var
  aParent: TCSSElement;
  i: Integer;
begin
  Result:=-1;
  if El=nil then exit;
  repeat
    aParent:=El.Parent;
    if aParent=nil then break;
    El:=aParent
  until false;
  for i:=0 to FStyleSheetCount-1 do
    if FStyleSheets[i].Element=El then
      exit(i);
end;

function TCSSResolver.IndexOfStyleSheetWithName(anOrigin: TCSSOrigin; const aName: TCSSString
  ): integer;
var
  Sheet: TStyleSheet;
begin
  for Result:=0 to FStyleSheetCount-1 do
  begin
    Sheet:=FStyleSheets[Result];
    if (Sheet.Origin=anOrigin) and (aName=Sheet.Name) then
      exit;
  end;
  Result:=-1;
end;

function TCSSResolver.FindStyleSheetWithElement(El: TCSSElement): TStyleSheet;
var
  i: Integer;
begin
  i:=IndexOfStyleSheetWithElement(El);
  if i>=0 then
    Result:=FStyleSheets[i]
  else
    Result:=nil;
end;

function TCSSResolver.FindStyleSheetWithName(const aName: TCSSString; anOrigin: TCSSOrigin
  ): TStyleSheet;
var
  i: Integer;
begin
  i:=IndexOfStyleSheetWithName(anOrigin,aName);
  if i>=0 then
    Result:=FStyleSheets[i]
  else
    Result:=nil;
end;

function TCSSResolver.GetStyleSheetStamp(anOrigin: TCSSOrigin; const aName: TCSSString): integer;
var
  i: Integer;
begin
  i:=IndexOfStyleSheetWithName(anOrigin,aName);
  if i>=0 then
    Result:=FStyleSheets[i].Stamp
  else
    Result:=-1;
end;

function TCSSResolver.GetDeclarationPath(DeclEl: TCSSDeclarationElement;
  out Path: TCSSDeclarationPath): boolean;
var
  ShIdx, i, Level: Integer;
  Sheet: TStyleSheet;
  Root, Node: TCSSElement;
  Innermost: TCSSRuleElement;
  Chain: TCSSRuleElementArray; // rule nesting chain, outermost (0) .. innermost
  ChainCnt: Integer;
  Candidates: TCSSRuleElementArray;
begin
  Result:=false;
  Path:=Default(TCSSDeclarationPath);
  if DeclEl=nil then exit;
  ShIdx:=IndexOfStyleSheetWithElement(DeclEl);
  if ShIdx<0 then exit; // e.g. an inline style declaration, not in a stylesheet
  Sheet:=FStyleSheets[ShIdx];
  Root:=Sheet.Element;

  // collect the chain of enclosing rules, innermost first while walking up
  Chain:=nil;
  ChainCnt:=0;
  Node:=DeclEl.Parent;
  while Node<>nil do
  begin
    if Node is TCSSRuleElement then
    begin
      SetLength(Chain,ChainCnt+1);
      // shift down to insert this (more outer) rule at the front
      for i:=ChainCnt downto 1 do
        Chain[i]:=Chain[i-1];
      Chain[0]:=TCSSRuleElement(Node);
      inc(ChainCnt);
    end;
    if Node=Root then break;
    Node:=Node.Parent;
  end;
  if ChainCnt=0 then exit; // declaration not inside a rule
  Innermost:=Chain[ChainCnt-1];

  Path.Origin:=Sheet.Origin;
  Path.SheetName:=Sheet.Name;
  Path.PropertyName:=CSSDeclPropertyName(DeclEl);
  SetLength(Path.Selectors,ChainCnt);
  SetLength(Path.RuleIndex,ChainCnt);
  for Level:=0 to ChainCnt-1 do
  begin
    if Level=0 then
      Candidates:=CSSGetTopLevelRules(Root)
    else
      Candidates:=CSSGetNestedRules(Chain[Level-1]);
    Path.Selectors[Level]:=CSSRuleSelectorsStr(Chain[Level]);
    Path.RuleIndex[Level]:=-1;
    for i:=0 to length(Candidates)-1 do
      if Candidates[i]=Chain[Level] then
      begin
        Path.RuleIndex[Level]:=i;
        break;
      end;
  end;

  Path.PropIndex:=-1;
  for i:=0 to Innermost.ChildCount-1 do
    if Innermost.Children[i]=DeclEl then
    begin
      Path.PropIndex:=i;
      break;
    end;

  Path.Valid:=true;
  Result:=true;
end;

function TCSSResolver.FindDeclaration(const Path: TCSSDeclarationPath): TCSSDeclarationElement;
var
  ShIdx, i, Level: Integer;
  Sheet: TStyleSheet;
  Candidates: TCSSRuleElementArray;
  Rule: TCSSRuleElement;
  Decl: TCSSDeclarationElement;

  function ChildDecl(aRule: TCSSRuleElement; Idx: integer): TCSSDeclarationElement;
  begin
    Result:=nil;
    if (Idx>=0) and (Idx<aRule.ChildCount) and (aRule.Children[Idx] is TCSSDeclarationElement) then
      Result:=TCSSDeclarationElement(aRule.Children[Idx]);
  end;

begin
  Result:=nil;
  if (not Path.Valid) or (length(Path.RuleIndex)=0) then exit;
  ShIdx:=IndexOfStyleSheetWithName(Path.Origin,Path.SheetName);
  if ShIdx<0 then exit;
  Sheet:=FStyleSheets[ShIdx];

  // walk the rule nesting chain, locating each level by index/selector
  Rule:=nil;
  for Level:=0 to length(Path.RuleIndex)-1 do
  begin
    if Level=0 then
      Candidates:=CSSGetTopLevelRules(Sheet.Element)
    else
      Candidates:=CSSGetNestedRules(Rule);
    Rule:=CSSLocateRule(Candidates,Path.RuleIndex[Level],Path.Selectors[Level]);
    if Rule=nil then exit;
  end;

  // locate the declaration: by index, else by name, else index fallback
  Decl:=ChildDecl(Rule,Path.PropIndex);
  if (Decl<>nil) and (CSSDeclPropertyName(Decl)=Path.PropertyName) then
    exit(Decl);
  for i:=0 to Rule.ChildCount-1 do
    if (Rule.Children[i] is TCSSDeclarationElement)
        and (CSSDeclPropertyName(TCSSDeclarationElement(Rule.Children[i]))=Path.PropertyName) then
      exit(TCSSDeclarationElement(Rule.Children[i]));
  Result:=ChildDecl(Rule,Path.PropIndex);
end;

function TCSSResolver.DeclKeyData(Decl: TCSSDeclarationElement): TCSSAttributeKeyData;
var
  Key: TCSSElement;
begin
  Result:=nil;
  if Decl=nil then exit;
  if Decl.KeyCount=0 then exit;
  Key:=Decl.Keys[0];
  if Key=nil then exit;
  if Key.CustomData is TCSSAttributeKeyData then
    Result:=TCSSAttributeKeyData(Key.CustomData);
end;

function TCSSResolver.RuleData(Rule: TCSSRuleElement): TCSSRuleData;
// nil if the rule was created by a parser without CSSRuleDataClass
begin
  Result:=nil;
  if Rule=nil then exit;
  if Rule.CustomData is TCSSRuleData then
    Result:=TCSSRuleData(Rule.CustomData);
end;

function TCSSResolver.DisabledDeclKey(const Path: TCSSDeclarationPath): TCSSString;
var
  i: Integer;
begin
  // build a unique, reparse-stable key from the declaration path;
  // #10 is a separator that cannot occur inside a selector token
  Result:=IntToStr(ord(Path.Origin))+#10+Path.SheetName+#10;
  Result:=Result+IntToStr(length(Path.Selectors))+#10;
  for i:=0 to length(Path.Selectors)-1 do
    Result:=Result+Path.Selectors[i]+#10;
  for i:=0 to length(Path.RuleIndex)-1 do
    Result:=Result+IntToStr(Path.RuleIndex[i])+#10;
  Result:=Result+Path.PropertyName+#10+IntToStr(Path.PropIndex);
end;

procedure TCSSResolver.DisableDeclaration(Decl: TCSSDeclarationElement);
var
  KeyData: TCSSAttributeKeyData;
  Path: TCSSDeclarationPath;
  Key: TCSSString;
  Item: TCSSDisabledDecl;
  RData: TCSSRuleData;
begin
  KeyData:=DeclKeyData(Decl);
  if KeyData=nil then exit;
  if KeyData.Disabled then exit;
  KeyData.Disabled:=true;
  if Decl.Parent is TCSSRuleElement then
  begin
    RData:=RuleData(TCSSRuleElement(Decl.Parent));
    if RData<>nil then
      RData.HasDisabledDecls:=true;
  end;

  // remember by path so the disabled state survives a reparse
  if GetDeclarationPath(Decl,Path) then
  begin
    Key:=DisabledDeclKey(Path);
    Item:=TCSSDisabledDecl(FDisabledDecls.Find(Key));
    if Item=nil then
    begin
      Item:=TCSSDisabledDecl.Create;
      Item.Path:=Path;
      FDisabledDecls.Add(Key,Item);
    end;
    Item.Decl:=Decl;
  end;

  // the merged/shared caches were built with this declaration active; drop them
  // so the next Compute rebuilds through MergeAttribute (where disabled is skipped)
  ClearMerge;
  ClearSharedRuleLists;
end;

procedure TCSSResolver.EnableDeclaration(Decl: TCSSDeclarationElement);
var
  KeyData: TCSSAttributeKeyData;
  Path: TCSSDeclarationPath;
  Index: Integer;
begin
  KeyData:=DeclKeyData(Decl);
  if KeyData=nil then exit;
  if not KeyData.Disabled then exit;
  KeyData.Disabled:=false;

  if GetDeclarationPath(Decl,Path) then
  begin
    Index:=FDisabledDecls.FindIndexOf(DisabledDeclKey(Path));
    if Index>=0 then
      FDisabledDecls.Delete(Index); // owns the object, frees it
  end;

  UpdateRuleHasDisabledDecls(Decl);

  ClearMerge;
  ClearSharedRuleLists;
end;

procedure TCSSResolver.RestoreDisabledDeclarations(Sheet: TStyleSheet);
var
  i: Integer;
  Item: TCSSDisabledDecl;
  Decl: TCSSDeclarationElement;
  KeyData: TCSSAttributeKeyData;
  RData: TCSSRuleData;
begin
  if Sheet=nil then exit;
  for i:=0 to FDisabledDecls.Count-1 do
  begin
    Item:=TCSSDisabledDecl(FDisabledDecls[i]);
    if (Item.Path.Origin<>Sheet.Origin) or (Item.Path.SheetName<>Sheet.Name) then
      continue;
    Decl:=FindDeclaration(Item.Path);
    Item.Decl:=Decl; // the reparse replaced the element; keep the reference current
    if Decl=nil then continue;
    KeyData:=DeclKeyData(Decl);
    if KeyData<>nil then
    begin
      KeyData.Disabled:=true;
      if Decl.Parent is TCSSRuleElement then
      begin
        RData:=RuleData(TCSSRuleElement(Decl.Parent));
        if RData<>nil then
          RData.HasDisabledDecls:=true;
      end;
    end;
  end;
end;

procedure TCSSResolver.UpdateRuleHasDisabledDecls(Decl: TCSSDeclarationElement);
begin
  if Decl=nil then exit;
  if Decl.Parent is TCSSRuleElement then
    UpdateRuleHasDisabledDecls(TCSSRuleElement(Decl.Parent));
end;

procedure TCSSResolver.UpdateRuleHasDisabledDecls(Rule: TCSSRuleElement);
var
  i: Integer;
  Child: TCSSElement;
  RData: TCSSRuleData;
begin
  RData:=RuleData(Rule);
  if RData=nil then exit;
  for i:=0 to Rule.ChildCount-1 do
  begin
    Child:=Rule.Children[i];
    if (Child is TCSSDeclarationElement)
        and IsDeclarationDisabled(TCSSDeclarationElement(Child)) then
    begin
      RData.HasDisabledDecls:=true;
      exit;
    end;
  end;
  RData.HasDisabledDecls:=false;
end;

function TCSSResolver.IsDeclarationDisabled(Decl: TCSSDeclarationElement): boolean;
var
  KeyData: TCSSAttributeKeyData;
begin
  KeyData:=DeclKeyData(Decl);
  Result:=(KeyData<>nil) and KeyData.Disabled;
end;

function TCSSResolver.RuleHasDisabledDeclaration(Rule: TCSSRuleElement): boolean;
var
  i: Integer;
  RData: TCSSRuleData;
begin
  Result:=false;
  if Rule=nil then exit;
  RData:=RuleData(Rule);
  if RData<>nil then
    exit(RData.HasDisabledDecls);
  // no cached flag, e.g. parsed by a plain TCSSParser -> scan the children
  for i:=0 to Rule.ChildCount-1 do
    if (Rule.Children[i] is TCSSDeclarationElement)
        and IsDeclarationDisabled(TCSSDeclarationElement(Rule.Children[i])) then
      exit(true);
end;

procedure TCSSResolver.TransferDisabledDeclarations(OldRule, NewRule: TCSSRuleElement);
var
  i, j: Integer;
  OldDecl, NewDecl: TCSSDeclarationElement;
  KeyData: TCSSAttributeKeyData;
  Taken: array of boolean;
  PropName: TCSSString;
  HasDisabled: Boolean;
  RData: TCSSRuleData;
begin
  if (OldRule=nil) or (NewRule=nil) then exit;
  Taken:=nil;
  SetLength(Taken,NewRule.ChildCount);
  HasDisabled:=false;
  for i:=0 to OldRule.ChildCount-1 do
  begin
    if not (OldRule.Children[i] is TCSSDeclarationElement) then continue;
    OldDecl:=TCSSDeclarationElement(OldRule.Children[i]);
    if not IsDeclarationDisabled(OldDecl) then continue;
    PropName:=CSSDeclPropertyName(OldDecl);
    // the first new declaration of that property not yet claimed by an earlier old one,
    // so duplicate properties keep their order
    for j:=0 to NewRule.ChildCount-1 do
    begin
      if Taken[j] then continue;
      if not (NewRule.Children[j] is TCSSDeclarationElement) then continue;
      NewDecl:=TCSSDeclarationElement(NewRule.Children[j]);
      if CSSDeclPropertyName(NewDecl)<>PropName then continue;
      Taken[j]:=true;
      KeyData:=DeclKeyData(NewDecl);
      if KeyData<>nil then
      begin
        KeyData.Disabled:=true;
        HasDisabled:=true;
      end;
      break;
    end;
  end;
  RData:=RuleData(NewRule);
  if RData<>nil then
    RData.HasDisabledDecls:=HasDisabled;
end;

function TCSSResolver.GetDisabledDeclarations: TFPList;
var
  i: Integer;
  Item: TCSSDisabledDecl;
begin
  Result:=TFPList.Create;
  for i:=0 to FDisabledDecls.Count-1 do
  begin
    Item:=TCSSDisabledDecl(FDisabledDecls[i]);
    if Item.Decl<>nil then
      Result.Add(Item.Decl);
  end;
end;

function TCSSResolver.GetDisabledDeclarationPaths: TStrings;
var
  i: Integer;
  Item: TCSSDisabledDecl;
begin
  Result:=TStringList.Create;
  for i:=0 to FDisabledDecls.Count-1 do
  begin
    Item:=TCSSDisabledDecl(FDisabledDecls[i]);
    Result.AddObject(DisabledDeclKey(Item.Path),Item.Decl);
  end;
end;

end.

