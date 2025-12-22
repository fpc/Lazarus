{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterXML.pas, released 2000-11-20.
The Initial Author of this file is Jeff Rafter.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

History:
-------------------------------------------------------------------------------
2000-11-30 Removed mHashTable and MakeIdentTable per Michael Hieke

Known Issues:
- Nothing is really constrained (properly) to valid name chars
- Entity Refs are not constrained to valid name chars
- Support for "Combining Chars and Extender Chars" in names are lacking
- The internal DTD is not parsed (and not handled correctly)
-------------------------------------------------------------------------------}

{
@abstract(Provides an XML highlighter for SynEdit)
@author(Jeff Rafter-- Phil 4:13, based on SynHighlighterHTML by Hideo Koiso)
@created(2000-11-17)
@lastmod(2001-03-12)
The SynHighlighterXML unit provides SynEdit with an XML highlighter.
}

unit SynHighlighterXML;

interface

{$I SynEdit.inc}

uses
  Classes, SysUtils, Graphics, SynEditTypes, SynEditHighlighter, SynEditHighlighterFoldBase,
  SynEditHighlighterXMLBase, SynEditStrConst, SynEditMiscClasses, SynEditMiscProcs,
  LazEditTextAttributes;

type
  TtkTokenKind = (tkAttribute, tkAttrValue, tkCDATA,
    tkComment, tkCommentSym, tkElement, tkEntityRef, tkEqual, tkNull, tkProcessingInstruction,
    tkSpace, tkSymbol, tkText,
    //
    tknsAttribute, tknsAttrValue,
    //These are unused at the moment
    tkDocType
    {tkDocTypeAttrValue, tkDocTypeAttribute,
     tkDocTypeElement, tkDocTypeEqual
    }
  );

  TtkTokenDecorator = (tdNone,
    tdNameSpacePrefix, tdNameSpaceColon, tdNameSpaceDefinition,
    tdNameSpaceNoneStart, tdNameSpaceNoneEnd // no decoratar / used for merge-bounds
    );

  TRangeState = (rsAttribute, rsAttributeColon, rsAttributePostColon, rsAttrValue, rsCDATA,
    rsComment, rsElement, rsElementColon, rsElementPostColon, rsCloseElement, rsOpenElement, rsEqual,
    rsText,
    //These are unused at the moment
    rsDocType, rsDocTypeSquareBraces                                           //ek 2001-11-11
    {rsDocTypeAttrValue, rsDocTypeAttribute,
     rsDocTypeElement, rsDocTypeEqual
    }
  );
  TRangeFlag = (rfProcessingInstruction,  // At/Inside <? .. ?>
                rfSingleQuote,            // Only used when frange = rsAttrValue
                rfNameSpace,              // in xmlns:foo-".."
                rfEntityRef               // In Entity &gt;  OVERRIDES whatever is in fRange
               );
  TRangeFlags = set of TRangeFlag;

  TRequiredState = (reaNameSpaceSubToken);
  TRequiredStates = set of TRequiredState;

  TXmlCodeFoldBlockType = (
    cfbtXmlNode,     // <foo>...</node>
    cfbtXmlComment,  // <!-- -->
    cfbtXmlCData,    // <![CDATA[ ]]>
    cfbtXmlDocType,  // <!DOCTYPE
    cfbtXmlProcess,   // <?
    // internal types / not configurable
    cfbtXmlNone
  );

type

  TProcTableProc = procedure of object;

  { TSynXMLSyn }

  TSynXMLSyn = class(TSynCustomXmlHighlighter)
  private type
    TSynPasAttribute = (
    attribElement,
    attribAttribute,
    attribNameSpaceAttribute,
    attribAttributeValue,
    attribNameSpaceAttributeValue,
    attribNamespaceDefinition,
    attribNamespacePrefix,
    attribNamespaceColon,
    attribText,
    attribCDATA,
    attribEntityRef,
    attribProcessingInstruction,
    attribProcessingInstructionSymbol,
    attribComment,
    attribCommentSymbol,
    attribDocType,
    attribSpace,
    attribSymbol
    );

    {$PUSH}{$PackEnum 1}{$PackSet 1}
    TRangeStore = bitpacked record
      case integer of
      1:( fRange: TRangeState;
          fRangeFlags: packed set of TRangeFlag;
        );
      2: (p: Pointer);
    end;
    {$POP}
    {$if sizeof(TRangeStore) <> sizeof(Pointer)}
      {$error range storage needs fixing}
    {$endif}
  private
    fCommentSymbolAttri: TSynHighlighterAttributes;
    FQuotesUseAttribValueAttri: Boolean;
    fRange: TRangeState;
    fRangeFlags: TRangeFlags;
    fRequiredStates: TRequiredStates;
    Run: Longint;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fTokenDecorator: TtkTokenDecorator;
    fLineLen: Integer;
    fElementAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fTextAttri: TSynHighlighterAttributes;
    fEntityRefAttri: TSynHighlighterAttributes;
    fProcessingInstructionAttri: TSynHighlighterAttributesModifier;
    fNamespaceColonAttri: TSynHighlighterAttributesModifier;
    fNamespaceDefinitionAttri: TSynHighlighterAttributesModifier;
    fNamespacePrefixAttri: TSynHighlighterAttributesModifier;
    fProcessingInstructionSymbolAttri: TSynHighlighterAttributes;
    fCDATAAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fDocTypeAttri: TSynHighlighterAttributes;
    fAttributeAttri: TSynHighlighterAttributes;
    fNameSpaceAttributeAttri: TSynHighlighterAttributes;
    fAttributeValueAttri: TSynHighlighterAttributes;
    fNameSpaceAttributeValueAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fProcTable: array[#0..#255] of TProcTableProc;
    FWantBracesParsed: Boolean;
    procedure NullProc;
    procedure CarriageReturnProc;
    procedure LineFeedProc;
    procedure SetAttribute(AnIndex: TSynPasAttribute; AValue: TSynHighlighterAttributes);
    procedure SetAttribute(AnIndex: TSynPasAttribute; AValue: TSynHighlighterAttributesModifier);
    procedure SetAttributeEx(AnIndex: TSynPasAttribute; AValue: TLazEditTextAttribute);
    procedure SpaceProc;
    procedure QuestionMarkProc;
    procedure LessThanProc;
    procedure GreaterThanProc;
    procedure CommentProc;
    procedure DocTypeProc;
    procedure CDATAProc;
    procedure TextProc;
    procedure ElementProc;
    procedure ElementColonProc;
    procedure ElementPostColonProc;
    procedure AttributeProc;
    procedure AttributeColonProc;
    procedure AttributePostColonProc;
    procedure AttributeValueProc;
    procedure EqualProc;
    procedure IdentProc;
    procedure MakeMethodTables;
    function NextTokenIs(T: String): Boolean;
    procedure EntityRefProc;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource : String; override;
  protected
    procedure DoDefHighlightChanged; override;
    // folding
    procedure CreateRootCodeFoldBlock; override;

    function StartXmlCodeFoldBlock(ABlockType: TXmlCodeFoldBlockType): TSynCustomCodeFoldBlock;
    function StartXmlNodeCodeFoldBlock(ABlockType: TXmlCodeFoldBlockType;
                                   OpenPos: Integer; AName: String): TSynCustomCodeFoldBlock;
    procedure EndXmlNodeCodeFoldBlock(ClosePos: Integer = -1; AName: String = '');
    function TopXmlCodeFoldBlockType(DownIndex: Integer = 0): TXmlCodeFoldBlockType;

    function GetFoldConfigInstance(Index: Integer): TSynCustomFoldConfig; override;
    function GetFoldConfigCount: Integer; override;
    function GetFoldConfigInternalCount: Integer; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    procedure InitForScaningLine; override;
    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TLazEditTextAttribute; override;
    function GetTokenAttributeEx: TLazCustomEditTextAttribute; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ReSetRange; override;
    property IdentChars;
  published
    property ElementAttri: TSynHighlighterAttributes index attribElement read fElementAttri write SetAttribute;
    property AttributeAttri: TSynHighlighterAttributes index attribAttribute  read fAttributeAttri write SetAttribute;
    property NamespaceAttributeAttri: TSynHighlighterAttributes index attribNameSpaceAttribute read fNameSpaceAttributeAttri write SetAttribute;
    property AttributeValueAttri: TSynHighlighterAttributes index attribAttributeValue  read fAttributeValueAttri write SetAttribute;
    property NamespaceAttributeValueAttri: TSynHighlighterAttributes index attribNameSpaceAttributeValue read fNameSpaceAttributeValueAttri write SetAttribute;
    property NamespaceDefinitionAttri: TSynHighlighterAttributesModifier index attribNamespaceDefinition read fNamespaceDefinitionAttri write SetAttribute;
    property NamespacePrefixAttri: TSynHighlighterAttributesModifier index attribNamespacePrefix read fNamespacePrefixAttri write SetAttribute;
    property NamespaceColonAttri: TSynHighlighterAttributesModifier index attribNamespaceColon read fNamespaceColonAttri write SetAttribute;
    property TextAttri: TSynHighlighterAttributes index attribText read fTextAttri write SetAttribute;
    property CDATAAttri: TSynHighlighterAttributes index attribCDATA read fCDATAAttri write SetAttribute;
    property EntityRefAttri: TSynHighlighterAttributes index attribEntityRef read fEntityRefAttri write SetAttribute;
    property ProcessingInstructionAttri: TSynHighlighterAttributesModifier index attribProcessingInstruction read fProcessingInstructionAttri write SetAttribute;
    property ProcessingInstructionSymbolAttri: TSynHighlighterAttributes index attribProcessingInstructionSymbol read fProcessingInstructionSymbolAttri write SetAttribute;
    property CommentAttri: TSynHighlighterAttributes index attribComment read fCommentAttri write SetAttribute;
    property CommentSymbolAttri: TSynHighlighterAttributes index attribCommentSymbol read fCommentSymbolAttri write SetAttribute;
    property DocTypeAttri: TSynHighlighterAttributes index attribDocType read fDocTypeAttri write SetAttribute;
    property SpaceAttri: TSynHighlighterAttributes index attribSpace read fSpaceAttri write SetAttribute;
    property SymbolAttri: TSynHighlighterAttributes index attribSymbol read fSymbolAttri write SetAttribute;
    property QuotesUseAttribValueAttri: Boolean read FQuotesUseAttribValueAttri write FQuotesUseAttribValueAttri default False;
    property WantBracesParsed : Boolean read FWantBracesParsed
      write FWantBracesParsed default True;
  end;

implementation

const
  NameChars : set of char = ['0'..'9', 'a'..'z', 'A'..'Z', '_', '.', ':', '-'];
  NameCharsNoColon : set of char = ['0'..'9', 'a'..'z', 'A'..'Z', '_', '.', '-'];

constructor TSynXMLSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fElementAttri:= TSynHighlighterAttributes.Create(@SYNS_AttrElementName, SYNS_XML_AttrElementName);
  fTextAttri:= TSynHighlighterAttributes.Create(@SYNS_AttrText, SYNS_XML_AttrText);
  fSpaceAttri:= TSynHighlighterAttributes.Create(@SYNS_AttrWhitespace, SYNS_XML_AttrWhitespace);
  fEntityRefAttri:= TSynHighlighterAttributes.Create(@SYNS_AttrEntityReference, SYNS_XML_AttrEntityReference);
  fProcessingInstructionAttri:= TSynHighlighterAttributesModifier.Create(@SYNS_AttrProcessingInstr, SYNS_XML_AttrProcessingInstr);
  fProcessingInstructionSymbolAttri:= TSynHighlighterAttributes.Create(@SYNS_AttrProcessingInstrSym, SYNS_XML_AttrProcessingInstrSym);
  fCDATAAttri:= TSynHighlighterAttributes.Create(@SYNS_AttrCDATASection, SYNS_XML_AttrCDATASection);
  fCommentAttri:= TSynHighlighterAttributes.Create(@SYNS_AttrComment, SYNS_XML_AttrComment);
  fCommentSymbolAttri:= TSynHighlighterAttributes.Create(@SYNS_AttrCommentSym, SYNS_XML_AttrCommentSym);
  fDocTypeAttri:= TSynHighlighterAttributes.Create(@SYNS_AttrDOCTYPESection, SYNS_XML_AttrDOCTYPESection);
  fAttributeAttri:= TSynHighlighterAttributes.Create(@SYNS_AttrAttributeName, SYNS_XML_AttrAttributeName);
  fNameSpaceAttributeAttri:= TSynHighlighterAttributes.Create(@SYNS_AttrNamespaceAttrName, SYNS_XML_AttrNamespaceAttrName);
  fAttributeValueAttri:= TSynHighlighterAttributes.Create(@SYNS_AttrAttributeValue, SYNS_XML_AttrAttributeValue);
  fNameSpaceAttributeValueAttri:= TSynHighlighterAttributes.Create(@SYNS_AttrNamespaceAttrValue, SYNS_XML_AttrNamespaceAttrValue);
  fSymbolAttri:= TSynHighlighterAttributes.Create(@SYNS_AttrSymbol, SYNS_XML_AttrSymbol);

  fNamespaceColonAttri := TSynHighlighterAttributesModifier.Create(@SYNS_AttrNamespaceIdentDef, SYNS_XML_AttrNamespaceIdentDef);
  fNamespaceDefinitionAttri := TSynHighlighterAttributesModifier.Create(@SYNS_AttrNamespaceIdentPrefix, SYNS_XML_AttrNamespaceIdentPrefix);
  fNamespacePrefixAttri := TSynHighlighterAttributesModifier.Create(@SYNS_AttrNamespaceColon, SYNS_XML_AttrNamespaceColon);

  fElementAttri.Foreground:= clMaroon;
  fElementAttri.Style:= [fsBold];

  fDocTypeAttri.Foreground:= clblue;
  fDocTypeAttri.Style:= [fsItalic];

  fCDATAAttri.Foreground:= clOlive;
  fCDATAAttri.Style:= [fsItalic];

  fEntityRefAttri.Foreground:= clblue;
  fEntityRefAttri.Style:= [fsbold];

  fProcessingInstructionAttri.Foreground:= clblue;
  fProcessingInstructionAttri.Background:= clDefault;
  fProcessingInstructionAttri.FrameColor:= clDefault;
  fProcessingInstructionAttri.Style:= [];
  fProcessingInstructionAttri.StyleMask:= [low(TFontStyle)..high(TFontStyle)];
  fProcessingInstructionAttri.SetAllPriorities(100);

  fProcessingInstructionSymbolAttri.Foreground:= clblue;
  fProcessingInstructionSymbolAttri.SetAllPriorities(150);

  fTextAttri.Foreground:= clBlack;
  fTextAttri.Style:= [fsBold];

  fAttributeAttri.Foreground:= clMaroon;
  fAttributeAttri.Style:= [];

  fNameSpaceAttributeAttri.Foreground:= clRed;
  fNameSpaceAttributeAttri.Style:= [];

  fAttributeValueAttri.Foreground:= clNavy;
  fAttributeValueAttri.Style:= [fsBold];

  fNameSpaceAttributeValueAttri.Foreground:= clRed;
  fNameSpaceAttributeValueAttri.Style:= [fsBold];

  fCommentAttri.Background:= clSilver;
  fCommentAttri.Foreground:= clGray;
  fCommentAttri.Style:= [fsbold, fsItalic];
  fCommentSymbolAttri.Clear;

  fSymbolAttri.Foreground:= clblue;
  fSymbolAttri.Style:= [];

  AddAttribute(fSymbolAttri);
  AddAttribute(fProcessingInstructionAttri);
  AddAttribute(fProcessingInstructionSymbolAttri);
  AddAttribute(fDocTypeAttri);
  AddAttribute(fCommentAttri);
  AddAttribute(fCommentSymbolAttri);
  AddAttribute(fElementAttri);
  AddAttribute(fAttributeAttri);
  AddAttribute(fNameSpaceAttributeAttri);
  AddAttribute(fAttributeValueAttri);
  AddAttribute(fNameSpaceAttributeValueAttri);
  AddAttribute(fNamespaceColonAttri);
  AddAttribute(fNamespaceDefinitionAttri);
  AddAttribute(fNamespacePrefixAttri);
  AddAttribute(fEntityRefAttri);
  AddAttribute(fCDATAAttri);
  AddAttribute(fSpaceAttri);
  AddAttribute(fTextAttri);

  SetAttributesOnChange(@DefHighlightChange);
  DoDefHighlightChanged;

  MakeMethodTables;
  fRange := rsText;
  fDefaultFilter := SYNS_FilterXML;
end;

procedure TSynXMLSyn.MakeMethodTables;
var
  i: Char;
begin
  for i:= #0 To #255 do begin
    case i of
    #0:
      begin
        fProcTable[i] := @NullProc;
      end;
    #10:
      begin
        fProcTable[i] := @LineFeedProc;
      end;
    #13:
      begin
        fProcTable[i] := @CarriageReturnProc;
      end;
    #1..#9, #11, #12, #14..#32:
      begin
        fProcTable[i] := @SpaceProc;
      end;
    '?':
        fProcTable[i] := @QuestionMarkProc;
    '<':
      begin
        fProcTable[i] := @LessThanProc;
      end;
    '>':
      begin
        fProcTable[i] := @GreaterThanProc;
      end;
    else
      fProcTable[i] := @IdentProc;
    end;
  end;
end;

procedure TSynXMLSyn.InitForScaningLine;
begin
  inherited;
  fLineLen := Length(CurrentLineText);
  Run := 0;
  Next;
end;

procedure TSynXMLSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynXMLSyn.CarriageReturnProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if LinePtr[Run] = #10 then Inc(Run);
end;

procedure TSynXMLSyn.LineFeedProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynXMLSyn.SetAttribute(AnIndex: TSynPasAttribute; AValue: TSynHighlighterAttributes);
begin
  case AnIndex of
    attribElement:                     FElementAttri.Assign(AValue);
    attribAttribute:                   FAttributeAttri.Assign(AValue);
    attribNameSpaceAttribute:          FNameSpaceAttributeAttri.Assign(AValue);
    attribAttributeValue:              FAttributeValueAttri.Assign(AValue);
    attribNameSpaceAttributeValue:     FNameSpaceAttributeValueAttri.Assign(AValue);
    attribNamespaceDefinition:         FNamespaceDefinitionAttri.Assign(AValue);
    attribNamespacePrefix:             FNamespacePrefixAttri.Assign(AValue);
    attribNamespaceColon:              FNamespaceColonAttri.Assign(AValue);
    attribText:                        FTextAttri.Assign(AValue);
    attribCDATA:                       FCDATAAttri.Assign(AValue);
    attribEntityRef:                   FEntityRefAttri.Assign(AValue);
    attribProcessingInstruction:       FProcessingInstructionAttri.Assign(AValue);
    attribProcessingInstructionSymbol: FProcessingInstructionSymbolAttri.Assign(AValue);
    attribComment:                     FCommentAttri.Assign(AValue);
    attribCommentSymbol:               FCommentSymbolAttri.Assign(AValue);
    attribDocType:                     FDocTypeAttri.Assign(AValue);
    attribSpace:                       FSpaceAttri.Assign(AValue);
    attribSymbol:                      FSymbolAttri.Assign(AValue);
  end;
end;

procedure TSynXMLSyn.SetAttribute(AnIndex: TSynPasAttribute;
  AValue: TSynHighlighterAttributesModifier);
begin
  SetAttributeEx(AnIndex, AValue);
end;

procedure TSynXMLSyn.SetAttributeEx(AnIndex: TSynPasAttribute; AValue: TLazEditTextAttribute);
begin
  SetAttributeEx(AnIndex, AValue);
end;

procedure TSynXMLSyn.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while LinePtr[Run] <= #32 do begin
    if LinePtr[Run] in [#0, #9, #10, #13] then break;
    Inc(Run);
  end;
end;

procedure TSynXMLSyn.QuestionMarkProc;
begin
  if (rfProcessingInstruction in fRangeFlags) and (LinePtr[Run+1] = '>') then begin
    fTokenID := tkProcessingInstruction;
    fRange := rsText;
    fRangeFlags := fRangeFlags - [rfSingleQuote, rfNameSpace, rfProcessingInstruction];
    Inc(Run, 2);
    if TopXmlCodeFoldBlockType = cfbtXmlProcess then
      EndXmlCodeFoldBlock;
  end
  else
    IdentProc;
end;

procedure TSynXMLSyn.LessThanProc;
begin
  fRangeFlags := fRangeFlags - [rfSingleQuote, rfNameSpace];
  Inc(Run);
  if (LinePtr[Run] = '/') then begin
    Inc(Run);
    fTokenID := tkSymbol;
    fRange := rsCloseElement;
    exit;
  end;

  if (LinePtr[Run] = '!') then
  begin
    if NextTokenIs('--') then begin
      fTokenID := tkCommentSym;
      fRange := rsComment;
      StartXmlCodeFoldBlock(cfbtXmlComment);
      Inc(Run, 3);
    end else if NextTokenIs('DOCTYPE') then begin
      fTokenID := tkDocType;
      fRange := rsDocType;
      StartXmlCodeFoldBlock(cfbtXmlDocType);
      Inc(Run, 7);
    end else if NextTokenIs('[CDATA[') then begin
      fTokenID := tkCDATA;
      fRange := rsCDATA;
      StartXmlCodeFoldBlock(cfbtXmlCData);
      Inc(Run, 7);
    end else begin
      fTokenID := tkSymbol;
      fRange := rsElement;
      Inc(Run);
    end;
  end else if LinePtr[Run]= '?' then begin
    fTokenID := tkProcessingInstruction;
    fRange := rsElement;
    Include(fRangeFlags, rfProcessingInstruction);
    StartXmlCodeFoldBlock(cfbtXmlProcess);
    Inc(Run);
  end else begin
    fTokenID := tkSymbol;
    fRange := rsOpenElement;
  end;
end;

procedure TSynXMLSyn.GreaterThanProc;
begin
  fRangeFlags := fRangeFlags - [rfSingleQuote, rfNameSpace];
  if (Run > 0) and (LinePtr[Run - 1] = '/') then
    if TopXmlCodeFoldBlockType = cfbtXmlNode then
      EndXmlNodeCodeFoldBlock;

  fTokenId := tkSymbol;
  fRange:= rsText;
  Inc(Run);
end;

procedure TSynXMLSyn.CommentProc;
begin
  if (LinePtr[Run] = '-') and (LinePtr[Run + 1] = '-') and
     (LinePtr[Run + 2] = '>')
  then begin
    fTokenID := tkCommentSym;
    fRange:= rsText;
    Inc(Run, 3);
    if TopXmlCodeFoldBlockType = cfbtXmlComment then
      EndXmlCodeFoldBlock;
    Exit;
  end;

  fTokenID := tkComment;

  if (LinePtr[Run] In [#0, #10, #13]) then begin
    fProcTable[LinePtr[Run]]();
    Exit;
  end;

  while not (LinePtr[Run] in [#0, #10, #13]) do begin
    if (LinePtr[Run] = '-') and (LinePtr[Run + 1] = '-') and (LinePtr[Run + 2] = '>')
    then begin
      fRange := rsComment;
      break;
    end;
    Inc(Run);
  end;
end;

procedure TSynXMLSyn.DocTypeProc;                                              //ek 2001-11-11
begin
  fTokenID := tkDocType;

  if (LinePtr[Run] In [#0, #10, #13]) then begin
    fProcTable[LinePtr[Run]]();
    Exit;
  end;

  case fRange of
    rsDocType:
      begin
        while not (LinePtr[Run] in [#0, #10, #13]) do
        begin
          case LinePtr[Run] of
            '[': begin
                   while True do
                   begin
                     inc(Run);
                     case LinePtr[Run] of
                       ']':
                         begin
                           Inc(Run);
                           Exit;
                         end;
                       #0, #10, #13:
                         begin
                           fRange:=rsDocTypeSquareBraces;
                           Exit;
                         end;
                     end;
                   end;
                 end;
            '>': begin
                   fRange := rsAttribute;
                   if TopXmlCodeFoldBlockType = cfbtXmlDocType then
                     EndXmlCodeFoldBlock;
                   Inc(Run);
                   Break;
                 end;
          end;
          inc(Run);
        end;
    end;
    rsDocTypeSquareBraces:
      begin
        while not (LinePtr[Run] in [#0, #10, #13]) do
        begin
          if (LinePtr[Run]=']') then
          begin
            fRange := rsDocType;
            Inc(Run);
            Exit;
          end;
          inc(Run);
        end;
      end;
  end;
end;

procedure TSynXMLSyn.CDATAProc;
begin
  fTokenID := tkCDATA;
  if (LinePtr[Run] In [#0, #10, #13]) then
  begin
    fProcTable[LinePtr[Run]]();
    Exit;
  end;

  while not (LinePtr[Run] in [#0, #10, #13]) do
  begin
    if (Run >= 2) and (LinePtr[Run] = '>') and (LinePtr[Run - 1] = ']') and
       (LinePtr[Run - 2] = ']')
    then begin
      fRange := rsText;
      Inc(Run);
      if TopXmlCodeFoldBlockType = cfbtXmlCData then
        EndXmlCodeFoldBlock;
      break;
    end;
    Inc(Run);
  end;
end;

procedure TSynXMLSyn.ElementProc;
var
  NameStart, r: LongInt;
  IsPreColon: Boolean;
begin
  if LinePtr[Run] = '/' then
    Inc(Run);
  NameStart := Run;
  IsPreColon := False;
  if not (IsScanning or IsInNextToEOL) and (reaNameSpaceSubToken in fRequiredStates) then begin
    fTokenDecorator := tdNameSpaceNoneStart;
    while (LinePtr[Run] in NameCharsNoColon) do Inc(Run);
    IsPreColon := LinePtr[Run] = ':';
    r := Run;
    while (LinePtr[r] in NameChars) do Inc(r);
  end
  else begin
    while (LinePtr[Run] in NameChars) do Inc(Run);
    r := Run;
  end;

  if fRange = rsOpenElement then
    StartXmlNodeCodeFoldBlock(cfbtXmlNode, NameStart, Copy(LinePtr, NameStart + 1, r - NameStart));

  if fRange = rsCloseElement then
    EndXmlNodeCodeFoldBlock(NameStart, Copy(LinePtr, NameStart + 1, r - NameStart));   // TODO: defer until ">" reached

  fTokenID := tkElement;

  if IsPreColon then begin
    fRange := rsElementColon;
    fTokenDecorator := tdNameSpacePrefix;
  end
  else
    fRange := rsAttribute;
end;

procedure TSynXMLSyn.ElementColonProc;
begin
  Inc(Run); // just one colon
  fTokenID := tkElement;
  fRange := rsElementPostColon;
  fTokenDecorator := tdNameSpaceColon;
end;

procedure TSynXMLSyn.ElementPostColonProc;
begin
  while (LinePtr[Run] in NameChars) do Inc(Run);
  fTokenID := tkElement;
  fRange := rsAttribute;
  fTokenDecorator := tdNameSpaceNoneEnd;
end;

procedure TSynXMLSyn.AttributeProc;
begin
  //Check if we are starting on a closing quote
  if (LinePtr[Run] in [#34, #39]) then
  begin
    fRangeFlags := fRangeFlags - [rfSingleQuote, rfNameSpace];
    if FQuotesUseAttribValueAttri then begin
      if rfNameSpace in fRangeFlags then
        fTokenID := tknsAttrValue
      else
        fTokenID := tkAttrValue;
    end
    else
      fTokenID := tkSymbol;
    fRange := rsAttribute;
    Inc(Run);
    Exit;
  end;

  //Check if this is an xmlns: attribute
  while (LinePtr[Run] in NameCharsNoColon) do Inc(Run);
  if (Run - fTokenPos = 5) and
     (StrLComp(pchar('xmlns'), LinePtr + fTokenPos, 5) = 0) and
     ( (fLineLen - fTokenPos = 5) or
       ((LinePtr + fTokenPos + 5)^ in [':', '=', #10, #13, #9, #32, #0])
     )
  then begin
    Include(fRangeFlags, rfNameSpace);
    fTokenID := tknsAttribute;
  end else begin
    fTokenID := tkAttribute;
  end;
  fRange := rsEqual;


  if not (IsScanning or IsInNextToEOL) and (reaNameSpaceSubToken in fRequiredStates) and
     (LinePtr[Run] = ':')
  then begin
    fTokenDecorator := tdNameSpaceNoneStart;
    fRange := rsAttributeColon;
    if not (rfNameSpace in fRangeFlags) then
      fTokenDecorator := tdNameSpacePrefix;
    exit;
  end;

  //Read the rest of the name
  while (LinePtr[Run] in NameChars) do Inc(Run);
end;

procedure TSynXMLSyn.AttributeColonProc;
begin
  Inc(Run); // just one colon
  if (rfNameSpace in fRangeFlags) then
    fTokenID := tknsAttribute
  else
    fTokenID := tkAttribute;
  fRange := rsAttributePostColon;
  fTokenDecorator := tdNameSpaceColon;
end;

procedure TSynXMLSyn.AttributePostColonProc;
begin
  while (LinePtr[Run] in NameChars) do Inc(Run);
  if (rfNameSpace in fRangeFlags) then begin
    fTokenID := tknsAttribute;
    fTokenDecorator := tdNameSpaceDefinition;
  end
  else begin
    fTokenID := tkAttribute;
    fTokenDecorator := tdNameSpaceNoneEnd;
  end;
  fRange := rsEqual;
end;

procedure TSynXMLSyn.EqualProc;
begin
  fTokenID := tkEqual;

  while not (LinePtr[Run] in [#0, #10, #13]) do
  begin
    if (LinePtr[Run] = '/') then
    begin
      fTokenID := tkSymbol;
      fRange := rsElement;
      Inc(Run);
      Exit;
    end else if (LinePtr[Run] in [#34, #39]) then
    begin
      if FQuotesUseAttribValueAttri and (Run > fTokenPos) then
        exit;
      if (LinePtr[Run] = #39) then
        Include(fRangeFlags, rfSingleQuote);
      fRange := rsAttrValue;
      Inc(Run);
      if FQuotesUseAttribValueAttri then
        AttributeValueProc;
      Exit;
    end;
    Inc(Run);
  end;
end;

procedure TSynXMLSyn.AttributeValueProc;
begin
  if rfNameSpace in fRangeFlags then
    fTokenID := tknsAttrValue
  else
    fTokenID := tkAttrValue;

  if rfSingleQuote in fRangeFlags then
    while not (LinePtr[Run] in [#0, #10, #13, '&', #39]) do Inc(Run)
  else
    while not (LinePtr[Run] in [#0, #10, #13, '&', #34]) do Inc(Run);

  if LinePtr[Run] = '&' then
  begin
    Include(fRangeFlags, rfEntityRef);
    Exit;
  end;

  if rfSingleQuote in fRangeFlags then begin
    if LinePtr[Run] <> #39 then
      Exit;
  end
  else begin
    if LinePtr[Run] <> #34 then
      Exit;
  end;

  if FQuotesUseAttribValueAttri then
    inc(Run);

  fRange := rsAttribute;
  Exclude(fRangeFlags, rfSingleQuote);
  Exclude(fRangeFlags, rfNameSpace);
end;

procedure TSynXMLSyn.TextProc;
const StopSet = [#0..#31, '<', '&'];
begin
  if LinePtr[Run] in (StopSet - ['&']) then begin
    fProcTable[LinePtr[Run]]();
    exit;
  end;

  fTokenID := tkText;
  while not (LinePtr[Run] in StopSet) do Inc(Run);

  if (LinePtr[Run] = '&') then begin
    Include(fRangeFlags, rfEntityRef);
    Exit;
  end;
end;

procedure TSynXMLSyn.EntityRefProc;
begin
  fTokenID := tkEntityRef;
  while not (LinePtr[Run] in [#0..#32, ';', '''', '"', '>']) do
    Inc(Run);
  if (LinePtr[Run] = ';') then
    Inc(Run);

  Exclude(fRangeFlags, rfEntityRef);
end;

function TSynXMLSyn.GetFoldConfigInstance(Index: Integer): TSynCustomFoldConfig;
begin
  Result := inherited GetFoldConfigInstance(Index);
  Result.Enabled := True;
  if TXmlCodeFoldBlockType(Index) in [cfbtXmlNode] then begin
    Result.SupportedModes := Result.SupportedModes + [fmMarkup];
    Result.Modes := Result.Modes + [fmMarkup];
  end;
end;

procedure TSynXMLSyn.IdentProc;
begin
  if rfEntityRef in fRangeFlags then begin
    EntityRefProc;
    exit;
  end;

  case fRange of
  rsElement,
  rsOpenElement, rsCloseElement: ElementProc();
  rsElementColon:                ElementColonProc();
  rsElementPostColon:            ElementPostColonProc();
  rsAttribute:                   AttributeProc();
  rsAttributeColon:              AttributeColonProc();
  rsAttributePostColon:          AttributePostColonProc();
  rsEqual:                       EqualProc();
  rsAttrValue:                   AttributeValueProc();
  else ;
  end;
end;

procedure TSynXMLSyn.Next;
begin
  fTokenPos := Run;
  fTokenDecorator := tdNone;
  while fTokenPos = Run do begin
    if rfEntityRef in fRangeFlags then begin
      EntityRefProc;
    end
    else
    case fRange of
    rsText:
      begin
        TextProc();
      end;
    rsComment:
      begin
        CommentProc();
      end;
    rsDocType, rsDocTypeSquareBraces:                                            //ek 2001-11-11
      begin
        DocTypeProc();
      end;
    rsCDATA:
      begin
        CDATAProc();
      end;
    else
      fProcTable[LinePtr[Run]]();
    end;
    if fTokenId = tkNull then // EOL
      break;
  end;
end;

function TSynXMLSyn.NextTokenIs(T : String) : Boolean;
var I, Len : Integer;
begin
  Result:= True;
  Len:= Length(T);
  for I:= 1 to Len do
    if (LinePtr[Run + I] <> T[I]) then
    begin
      Result:= False;
      Break;
    end;
end;

function TSynXMLSyn.GetDefaultAttribute(
  Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fAttributeAttri;
    SYN_ATTR_KEYWORD: Result := fElementAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynXMLSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TSynXMLSyn.GetToken: string;
var
  len: Longint;
begin
  Result := '';
  Len := (Run - fTokenPos);
  SetString(Result, (LinePtr + fTokenPos), len);
end;

procedure TSynXMLSyn.GetTokenEx(out TokenStart: PChar;
  out TokenLength: integer);
begin
  TokenLength:=Run-fTokenPos;
  TokenStart:=LinePtr + fTokenPos;
end;

function TSynXMLSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynXMLSyn.GetTokenAttribute: TLazEditTextAttribute;
var
  x1, x2: Integer;
begin
case fTokenID of
    tkElement: Result:= fElementAttri;
    tkAttribute: Result:= fAttributeAttri;
    tknsAttribute: Result:= fNameSpaceAttributeAttri;
    tkEqual: Result:= fSymbolAttri;
    tkAttrValue: Result:= fAttributeValueAttri;
    tknsAttrValue: Result:= fNameSpaceAttributeValueAttri;
    tkText: Result:= fTextAttri;
    tkCDATA: Result:= fCDATAAttri;
    tkEntityRef: Result:= fEntityRefAttri;
    tkProcessingInstruction:
      if fProcessingInstructionSymbolAttri.IsEnabled then
        Result:= fProcessingInstructionSymbolAttri
      else
        Result:= fSymbolAttri;
    tkComment: Result:= fCommentAttri;
    tkCommentSym:
      if fCommentSymbolAttri.IsEnabled then
        Result:= fCommentSymbolAttri
      else
        Result:= fSymbolAttri;
    tkDocType: Result:= fDocTypeAttri;
    tkSymbol: Result:= fSymbolAttri;
    tkSpace: Result:= fSpaceAttri;
  else
    Result := nil;
  end;
  if Result <> nil then begin
    x1 := ToPos(fTokenPos);
    x2 := ToPos(Run);
    case fTokenDecorator of
      tdNameSpacePrefix,
      tdNameSpaceNoneStart:  begin          x2 := fLineLen + 1; end;
      tdNameSpaceColon:      begin x1 := 1; x2 := fLineLen + 1; end;
      tdNameSpaceDefinition,
      tdNameSpaceNoneEnd:    begin x1 := 1;                     end;
    end;
    Result.SetFrameBoundsLog(x1, x2);
  end;
end;

function TSynXMLSyn.GetTokenAttributeEx: TLazCustomEditTextAttribute;
var
  x1, x2: Integer;
begin
  Result := GetTokenAttribute;
  x1 := ToPos(fTokenPos);
  x2 := ToPos(Run);

  if (rfProcessingInstruction in fRangeFlags) or
     (fTokenID = tkProcessingInstruction)
  then begin
    if (not(rfProcessingInstruction in fRangeFlags)) or (fTokenID <> tkProcessingInstruction) then
      x1 := 1;
    if (rfProcessingInstruction in fRangeFlags) then
      x2 := fLineLen + 1;
    MergeModifierToTokenAttribute(Result, fProcessingInstructionAttri, x1, x2);
    x1 := ToPos(fTokenPos);
    x2 := ToPos(Run);
  end;

  case fTokenDecorator of
    tdNameSpacePrefix: begin
      MergeModifierToTokenAttribute(Result, fNamespacePrefixAttri, x1, x2);
    end;
    tdNameSpaceColon: begin
      MergeModifierToTokenAttribute(Result, fNamespaceColonAttri, x1, x2);
    end;
    tdNameSpaceDefinition: begin
      MergeModifierToTokenAttribute(Result, fNamespaceDefinitionAttri, x1, x2);
    end;
  end;
end;

function TSynXMLSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynXMLSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynXMLSyn.GetRange: Pointer;
var
  t: TRangeStore;
begin
  t.fRange      := fRange;
  t.fRangeFlags := fRangeFlags;
  CodeFoldRange.RangeType := t.p;
  Result := inherited;
end;

procedure TSynXMLSyn.SetRange(Value: Pointer);
var
  t: TRangeStore;
begin
  inherited;
  t := TRangeStore(CodeFoldRange.RangeType);
  fRange      := t.fRange;
  fRangeFlags := t.fRangeFlags;
end;

procedure TSynXMLSyn.ReSetRange;
begin
  inherited;
  fRange:= rsText;
  fRangeFlags := [];
end;

function TSynXMLSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['0'..'9', 'a'..'z', 'A'..'Z', '_', '.', '-'] + TSynSpecialChars;
end;

class function TSynXMLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangXML;
end;

function TSynXMLSyn.GetSampleSource: String;
begin
  Result:= '<?xml version="1.0"?>'#13#10+
           '<!DOCTYPE root ['#13#10+
           '  ]>'#13#10+
           '<!-- Comment -->'#13#10+
           '<root version="&test;">'#13#10+
           '  <![CDATA[ **CDATA section** ]]>'#13#10+
           '</root>';
end;

procedure TSynXMLSyn.DoDefHighlightChanged;
begin
  inherited DoDefHighlightChanged;
  fRequiredStates := [];
  if fNamespaceColonAttri.IsEnabled or
     fNamespaceDefinitionAttri.IsEnabled or
     fNamespacePrefixAttri.IsEnabled
  then
    Include(fRequiredStates, reaNameSpaceSubToken);
end;

procedure TSynXMLSyn.CreateRootCodeFoldBlock;
begin
  inherited CreateRootCodeFoldBlock;
  RootCodeFoldBlock.InitRootBlockType(Pointer(PtrInt(cfbtXmlNone)));
end;

function TSynXMLSyn.StartXmlCodeFoldBlock(ABlockType: TXmlCodeFoldBlockType): TSynCustomCodeFoldBlock;
begin
  Result := inherited StartXmlCodeFoldBlock(ord(ABlockType));
end;

function TSynXMLSyn.StartXmlNodeCodeFoldBlock(ABlockType: TXmlCodeFoldBlockType;
  OpenPos: Integer; AName: String): TSynCustomCodeFoldBlock;
begin
  if not FFoldConfig[ord(cfbtXmlNode)].Enabled then exit(nil);
  Result := inherited StartXmlNodeCodeFoldBlock(ord(ABlockType), OpenPos, AName);
end;

procedure TSynXMLSyn.EndXmlNodeCodeFoldBlock(ClosePos: Integer; AName: String);
begin
  if not FFoldConfig[ord(cfbtXmlNode)].Enabled then exit;
  inherited EndXmlNodeCodeFoldBlock(ClosePos, AName);
end;

function TSynXMLSyn.TopXmlCodeFoldBlockType(DownIndex: Integer): TXmlCodeFoldBlockType;
begin
  Result := TXmlCodeFoldBlockType(PtrUInt(TopCodeFoldBlockType(DownIndex)));
end;

function TSynXMLSyn.GetFoldConfigCount: Integer;
begin
  // excluded cfbtXmlNone
  Result := ord(high(TXmlCodeFoldBlockType)) - ord(low(TXmlCodeFoldBlockType));
end;

function TSynXMLSyn.GetFoldConfigInternalCount: Integer;
begin
  // excluded cfbtXmlNone;
  Result := ord(high(TXmlCodeFoldBlockType)) - ord(low(TXmlCodeFoldBlockType)) + 1;
end;

initialization
  RegisterPlaceableHighlighter(TSynXMLSyn);

end.

