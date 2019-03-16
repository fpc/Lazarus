{
 **********************************************************************
  This file is part of LazUtils.
  It is based on the FCL unit xmlwrite svn revision 15251.

  See the file COPYING.FPC, included in this distribution,
  for details about the license.
 **********************************************************************

  XML writing routines
  Copyright (c) 1999-2000 by Sebastian Guenther, sg@freepascal.org
  Modified in 2006 by Sergei Gorelkin, sergei_gorelkin@mail.ru
  Converted to use UTF8 instead of widestrings by Mattias Gaertner.

}


unit laz2_XMLWrite;

{$ifdef fpc}{$MODE objfpc}{$endif}
{$H+}

{$DEFINE UseUTF8}
{off $DEFINE UseWideString}

interface

uses Classes, LazUTF8, laz2_DOM, SysUtils, laz2_xmlutils, lazutf8classes;

type
  TXMLWriterFlag = (
    xwfSpecialCharsInAttributeValue, // write #13 as #13 instead of as &xD;
    xwfPreserveWhiteSpace
    );
  TXMLWriterFlags = set of TXMLWriterFlag;

procedure WriteXMLFile(doc: TXMLDocument; const AFileName: String; Flags: TXMLWriterFlags = []); overload;
procedure WriteXMLFile(doc: TXMLDocument; var AFile: Text; Flags: TXMLWriterFlags = []); overload;
procedure WriteXMLFile(doc: TXMLDocument; AStream: TStream; Flags: TXMLWriterFlags = []); overload;

procedure WriteXML(Element: TDOMNode; const AFileName: String; Flags: TXMLWriterFlags = []); overload;
procedure WriteXML(Element: TDOMNode; var AFile: Text; Flags: TXMLWriterFlags = []); overload;
procedure WriteXML(Element: TDOMNode; AStream: TStream; Flags: TXMLWriterFlags = []); overload;


// ===================================================================

implementation

type
  TXMLWriter = class;
  TSpecialCharCallback = procedure(Sender: TXMLWriter; const s: DOMString;
    var idx: Integer);

  PAttrFixup = ^TAttrFixup;
  TAttrFixup = record
    Attr: TDOMNode;
    Prefix: PHashItem;
  end;

  { TXMLWriter }

  TXMLWriter = class(TObject)
  private
    FInsideTextNode: Boolean;
    FCanonical: Boolean;
    FIndent: DOMString;
    FIndentCount: Integer;
    FBuffer: PChar;
    FBufPos: PChar;
    FCapacity: Integer;
    FLineBreak: DOMString;
    FNSHelper: TNSSupport;
    FAttrFixups: TFPList;
    FScratch: TFPList;
    FNSDefs: TFPList;
    FWriteFlags: TXMLWriterFlags;
    procedure wrtChars(Src: DOMPChar; Length: Integer);
    procedure IncIndent;
    procedure DecIndent; {$IFDEF HAS_INLINE} inline; {$ENDIF}
    procedure wrtStr(const ws: DOMString); {$IFDEF HAS_INLINE} inline; {$ENDIF}
    procedure wrtChr(c: DOMChar); {$IFDEF HAS_INLINE} inline; {$ENDIF}
    procedure wrtIndent; {$IFDEF HAS_INLINE} inline; {$ENDIF}
    procedure wrtQuotedLiteral(const ws: DOMString);
    procedure ConvWrite(const s: DOMString; const SpecialChars: TSetOfChar;
      const SpecialCharCallback: TSpecialCharCallback);
    procedure WriteNSDef(B: TBinding);
    procedure NamespaceFixup(Element: TDOMElement);
  protected
    procedure Write(const Buffer; Count: Longint); virtual; abstract;
    procedure WriteNode(Node: TDOMNode);
    procedure VisitDocument(Node: TDOMNode);
    procedure VisitDocument_Canonical(Node: TDOMNode);
    procedure VisitElement(Node: TDOMNode);
    procedure VisitText(Node: TDOMNode);
    procedure VisitCDATA(Node: TDOMNode);
    procedure VisitComment(Node: TDOMNode);
    procedure VisitFragment(Node: TDOMNode);
    procedure VisitAttribute(Node: TDOMNode);
    procedure VisitEntityRef(Node: TDOMNode);
    procedure VisitDocumentType(Node: TDOMNode);
    procedure VisitPI(Node: TDOMNode);
  public
    constructor Create;
    destructor Destroy; override;
    property WriteFlags: TXMLWriterFlags read FWriteFlags write FWriteFlags;
  end;

  TTextXMLWriter = Class(TXMLWriter)
  Private
    F : ^Text;
  Protected
    Procedure Write(Const Buffer; Count : Longint);override;
  Public
    constructor Create(var AFile: Text);
  end;

  TStreamXMLWriter = Class(TXMLWriter)
  Private
    F : TStream;
  Protected
    Procedure Write(Const Buffer; Count : Longint);override;
  Public
    constructor Create(AStream: TStream);
  end;

{ ---------------------------------------------------------------------
    TTextXMLWriter
  ---------------------------------------------------------------------}


constructor TTextXMLWriter.Create(var AFile: Text);
begin
  inherited Create;
  f := @AFile;
end;

procedure TTextXMLWriter.Write(const Buffer; Count: Longint);
var
  s: string;
begin
  if Count>0 then
  begin
    SetString(s, PChar(@Buffer), Count);
    system.Write(f^, s);
  end;
end;

{ ---------------------------------------------------------------------
    TStreamXMLWriter
  ---------------------------------------------------------------------}

constructor TStreamXMLWriter.Create(AStream: TStream);
begin
  inherited Create;
  F := AStream;
end;


procedure TStreamXMLWriter.Write(const Buffer; Count: Longint);
begin
  if Count > 0 then
    F.Write(Buffer, Count);
end;


{ ---------------------------------------------------------------------
    TXMLWriter
  ---------------------------------------------------------------------}

const
  AttrSpecialChars : array[boolean] of TSetOfChar = (
    ['<', '"', '''', '&', #0..#31], // false: default
    ['<', '"', '''', '&']  // true: write special characters
    );
  TextSpecialChars = ['<', '>', '&', #0..#31];
  CDSectSpecialChars = [']'];
  LineEndingChars = [#13, #10];
  QuotStr = '&quot;';
  AposStr = '&apos;';
  AmpStr = '&amp;';
  ltStr = '&lt;';
  gtStr = '&gt;';
  HexChr: PChar = '0123456789ABCDEF';

constructor TXMLWriter.Create;
var
  I: Integer;
begin
  inherited Create;
  // some overhead - always be able to write at least one extra UCS4
  FBuffer := AllocMem(512+32);
  FBufPos := FBuffer;
  FCapacity := 512;
  // Later on, this may be put under user control
  // for now, take OS setting
  if FCanonical then
    FLineBreak := #10
  else
    FLineBreak := sLineBreak;
  // Initialize Indent string
  // TODO: this must be done in setter of FLineBreak
  SetLength(FIndent, 100);
  FIndent[1] := FLineBreak[1];
  if Length(FLineBreak) > 1 then
    FIndent[2] := FLineBreak[2]
  else
    FIndent[2] := ' ';
  for I := 3 to 100 do FIndent[I] := ' ';
  FIndentCount := 0;
  FNSHelper := TNSSupport.Create;
  FScratch := TFPList.Create;
  FNSDefs := TFPList.Create;
  FAttrFixups := TFPList.Create;
end;

destructor TXMLWriter.Destroy;
var
  I: Integer;
begin
  for I := FAttrFixups.Count-1 downto 0 do
    Dispose(PAttrFixup(FAttrFixups.List^[I]));
  FAttrFixups.Free;
  FNSDefs.Free;
  FScratch.Free;
  FNSHelper.Free;
  if FBufPos > FBuffer then
    write(FBuffer^, FBufPos-FBuffer);

  FreeMem(FBuffer);
  inherited Destroy;
end;

procedure TXMLWriter.wrtChars(Src: DOMPChar; Length: Integer);
var
  pb: PChar;
  wc: Cardinal;
  SrcEnd: DOMPChar;
begin
  pb := FBufPos;
  SrcEnd := Src + Length;
  while Src < SrcEnd do
  begin
    if pb >= @FBuffer[FCapacity] then
    begin
      write(FBuffer^, FCapacity);
      Dec(pb, FCapacity);
      if pb > FBuffer then
        Move(FBuffer[FCapacity], FBuffer^, pb - FBuffer);
    end;

    wc := Cardinal(Src^);  Inc(Src);
    {$IFDEF UseUTF8}
      pb^ := char(wc);
      Inc(pb);
    {$ENDIF}
    {$IFDEF UseWideString}
      case wc of
        0..$7F:  begin
          pb^ := char(wc); Inc(pb);
        end;

        $80..$7FF: begin
          pb^ := Char($C0 or (wc shr 6));
          pb[1] := Char($80 or (wc and $3F));
          Inc(pb,2);
        end;

        $D800..$DBFF: begin
          if (Src < SrcEnd) and (Src^ >= #$DC00) and (Src^ <= #$DFFF) then
          begin
            wc := ((LongInt(wc) - $D7C0) shl 10) + LongInt(word(Src^) xor $DC00);
            Inc(Src);

            pb^ := Char($F0 or (wc shr 18));
            pb[1] := Char($80 or ((wc shr 12) and $3F));
            pb[2] := Char($80 or ((wc shr 6) and $3F));
            pb[3] := Char($80 or (wc and $3F));
            Inc(pb,4);
          end
          else
            raise EConvertError.Create('High surrogate without low one');
        end;
        $DC00..$DFFF:
          raise EConvertError.Create('Low surrogate without high one');
        else   // $800 >= wc > $FFFF, excluding surrogates
        begin
          pb^ := Char($E0 or (wc shr 12));
          pb[1] := Char($80 or ((wc shr 6) and $3F));
          pb[2] := Char($80 or (wc and $3F));
          Inc(pb,3);
        end;
      end;
    {$ENDIF UseWideString}
  end;
  FBufPos := pb;
end;

procedure TXMLWriter.wrtStr(const ws: DOMString); { inline }
begin
  wrtChars(DOMPChar(ws), Length(ws));
end;

{ No checks here - buffer always has 32 extra bytes }
procedure TXMLWriter.wrtChr(c: DOMChar); { inline }
begin
  FBufPos^ := char(ord(c));
  Inc(FBufPos);
end;

procedure TXMLWriter.wrtIndent; { inline }
begin
  wrtChars(DOMPChar(FIndent), FIndentCount*2+Length(FLineBreak));
end;

procedure TXMLWriter.IncIndent;
var
  I, NewLen, OldLen: Integer;
begin
  Inc(FIndentCount);
  if Length(FIndent) < 2 * FIndentCount then
  begin
    OldLen := Length(FIndent);
    NewLen := 4 * FIndentCount;
    SetLength(FIndent, NewLen);
    for I := OldLen to NewLen do
      FIndent[I] := ' ';
  end;
end;

procedure TXMLWriter.DecIndent; { inline }
begin
  if FIndentCount>0 then dec(FIndentCount);
end;

procedure TXMLWriter.ConvWrite(const s: DOMString; const SpecialChars: TSetOfChar;
  const SpecialCharCallback: TSpecialCharCallback);
var
  StartPos, EndPos: Integer;
begin
  StartPos := 1;
  EndPos := 1;
  while EndPos <= Length(s) do
  begin
    if (s[EndPos] < #128) and (Char(ord(s[EndPos])) in SpecialChars) then
    begin
      wrtChars(@s[StartPos], EndPos - StartPos);
      SpecialCharCallback(Self, s, EndPos);
      StartPos := EndPos + 1;
    end;
    Inc(EndPos);
  end;
  if StartPos <= length(s) then
    wrtChars(@s[StartPos], EndPos - StartPos);
end;

procedure AttrSpecialCharCallback(Sender: TXMLWriter; const s: DOMString;
  var idx: Integer);
begin
  case s[idx] of
    '"': Sender.wrtStr(QuotStr);
    '''': Sender.wrtStr(AposStr);
    '&': Sender.wrtStr(AmpStr);
    '<': Sender.wrtStr(ltStr);
    // Escape whitespace using CharRefs to be consistent with W3 spec § 3.3.3
    #0..#15: Sender.wrtStr('&#x'+HexChr[ord(s[idx])]+';');
    #16..#31: Sender.wrtStr('&#x1'+HexChr[ord(s[idx])-16]+';');
  else
    Sender.wrtChr(s[idx]);
  end;
end;

procedure TextnodeNormalCallback(Sender: TXMLWriter; const s: DOMString;
  var idx: Integer);
begin
  case s[idx] of
    '<': Sender.wrtStr(ltStr);
    '>': Sender.wrtStr(gtStr); // Required only in ']]>' literal, otherwise optional
    '&': Sender.wrtStr(AmpStr);
    #13:
      begin
        // We normalize #13#10 and #13 to FLineBreak, going somewhat
        // beyond the specs here, see issue #13879.
        Sender.wrtStr(Sender.FLineBreak);
        if (idx < Length(s)) and (s[idx+1] = #10) then
          Inc(idx);
      end;
    #10: Sender.wrtStr(Sender.FLineBreak);
    #0..#9,#11..#12,#14..#15: Sender.wrtStr('&#x'+HexChr[ord(s[idx])]+';');
    #16..#31: Sender.wrtStr('&#x1'+HexChr[ord(s[idx])-16]+';');
  else
    Sender.wrtChr(s[idx]);
  end;
end;

procedure TextnodeCanonicalCallback(Sender: TXMLWriter; const s: DOMString;
  var idx: Integer);
begin
  case s[idx] of
    '<': Sender.wrtStr(ltStr);
    '>': Sender.wrtStr(gtStr);
    '&': Sender.wrtStr(AmpStr);
    #0..#15: Sender.wrtStr('&#x'+HexChr[ord(s[idx])]+';');
    #16..#31: Sender.wrtStr('&#x1'+HexChr[ord(s[idx])-16]+';');
  else
    Sender.wrtChr(s[idx]);
  end;
end;

procedure CDSectSpecialCharCallback(Sender: TXMLWriter; const s: DOMString;
  var idx: Integer);
begin
  if (idx <= Length(s)-2) and (s[idx+1] = ']') and (s[idx+2] = '>') then
  begin
    Sender.wrtStr(']]]]><![CDATA[>');
    Inc(idx, 2);
    // TODO: emit warning 'cdata-section-splitted'
  end
  else
    Sender.wrtChr(s[idx]);
end;

const
  TextnodeCallbacks: array[boolean] of TSpecialCharCallback = (
    @TextnodeNormalCallback,
    @TextnodeCanonicalCallback
  );

procedure TXMLWriter.wrtQuotedLiteral(const ws: DOMString);
var
  Quote: DOMChar;
begin
  // TODO: need to check if the string also contains single quote
  // both quotes present is a error
  if Pos('"', ws) > 0 then
    Quote := ''''
  else
    Quote := '"';
  wrtChr(Quote);
  ConvWrite(ws, LineEndingChars, @TextnodeNormalCallback);
  wrtChr(Quote);
end;

procedure TXMLWriter.WriteNode(node: TDOMNode);
begin
  case node.NodeType of
    ELEMENT_NODE:                VisitElement(node);
    ATTRIBUTE_NODE:              VisitAttribute(node);
    TEXT_NODE:                   VisitText(node);
    CDATA_SECTION_NODE:          VisitCDATA(node);
    ENTITY_REFERENCE_NODE:       VisitEntityRef(node);
    PROCESSING_INSTRUCTION_NODE: VisitPI(node);
    COMMENT_NODE:                VisitComment(node);
    DOCUMENT_NODE:
      if FCanonical then
        VisitDocument_Canonical(node)
      else
        VisitDocument(node);
    DOCUMENT_TYPE_NODE:          VisitDocumentType(node);
    ENTITY_NODE,
    DOCUMENT_FRAGMENT_NODE:      VisitFragment(node);
  end;
end;

procedure TXMLWriter.WriteNSDef(B: TBinding);
begin
  wrtChars(' xmlns', 6);
  if B.Prefix^.Key <> '' then
  begin
    wrtChr(':');
    wrtStr(B.Prefix^.Key);
  end;
  wrtChars('="', 2);
  ConvWrite(B.uri, AttrSpecialChars[xwfSpecialCharsInAttributeValue in FWriteFlags],
            @AttrSpecialCharCallback);
  wrtChr('"');
end;

function Compare(const s1, s2: DOMString): integer;
var
  maxi, temp: integer;
begin
  Result := 0;
  if pointer(S1) = pointer(S2) then
    exit;
  maxi := Length(S1);
  temp := Length(S2);
  if maxi > temp then
    maxi := temp;
  Result := CompareWord(S1[1], S2[1], maxi);
  if Result = 0 then
    Result := Length(S1)-Length(S2);
end;

function SortNSDefs(Item1, Item2: Pointer): Integer;
begin
  Result := Compare(TBinding(Item1).Prefix^.Key, TBinding(Item2).Prefix^.Key);
end;

function SortAtts(Item1, Item2: Pointer): Integer;
var
  p1: PAttrFixup absolute Item1;
  p2: PAttrFixup absolute Item2;
  s1, s2: DOMString;
begin
  Result := Compare(p1^.Attr.namespaceURI, p2^.Attr.namespaceURI);
  if Result = 0 then
  begin
    // TODO: Must fix the parser so it doesn't produce Level 1 attributes
    if nfLevel2 in p1^.Attr.Flags then
      s1 := p1^.Attr.localName
    else
      s1 := p1^.Attr.nodeName;
    if nfLevel2 in p2^.Attr.Flags then
      s2 := p2^.Attr.localName
    else
      s2 := p2^.Attr.nodeName;
    Result := Compare(s1, s2);
  end;
end;

procedure TXMLWriter.NamespaceFixup(Element: TDOMElement);
var
  B: TBinding;
  i, j: Integer;
  node: TDOMNode;
  s: DOMString;
  action: TAttributeAction;
  p: PAttrFixup;
begin
  FScratch.Count := 0;
  FNSDefs.Count := 0;
  if Element.hasAttributes then
  begin
    j := 0;
    for i := 0 to Element.Attributes.Length-1 do
    begin
      node := Element.Attributes[i];
      if TDOMNode_NS(node).NSI.NSIndex = 2 then
      begin
        if TDOMNode_NS(node).NSI.PrefixLen = 0 then
          s := ''
        else
          s := node.localName;
        FNSHelper.DefineBinding(s, node.nodeValue, B);
        if Assigned(B) then  // drop redundant namespace declarations
          FNSDefs.Add(B);
      end
      else if FCanonical or TDOMAttr(node).Specified then
      begin
        // obtain a TAttrFixup record (allocate if needed)
        if j >= FAttrFixups.Count then
        begin
          New(p);
          FAttrFixups.Add(p);
        end
        else
          p := PAttrFixup(FAttrFixups.List^[j]);
        // add it to the working list
        p^.Attr := node;
        p^.Prefix := nil;
        FScratch.Add(p);
        Inc(j);
      end;
    end;
  end;

  FNSHelper.DefineBinding(Element.Prefix, Element.namespaceURI, B);
  if Assigned(B) then
    FNSDefs.Add(B);

  for i := 0 to FScratch.Count-1 do
  begin
    node := PAttrFixup(FScratch.List^[i])^.Attr;
    action := FNSHelper.CheckAttribute(node.Prefix, node.namespaceURI, B);
    if action = aaBoth then
      FNSDefs.Add(B);

    if action in [aaPrefix, aaBoth] then
      PAttrFixup(FScratch.List^[i])^.Prefix := B.Prefix;
  end;

  if FCanonical then
  begin
    FNSDefs.Sort(@SortNSDefs);
    FScratch.Sort(@SortAtts);
  end;

  // now, at last, dump all this stuff.
  for i := 0 to FNSDefs.Count-1 do
    WriteNSDef(TBinding(FNSDefs.List^[I]));

  for i := 0 to FScratch.Count-1 do
  begin
    wrtChr(' ');
    with PAttrFixup(FScratch.List^[I])^ do
    begin
      if Assigned(Prefix) then
      begin
        wrtStr(Prefix^.Key);
        wrtChr(':');
        wrtStr(Attr.localName);
      end
      else
        wrtStr(Attr.nodeName);

      wrtChars('="', 2);
      // TODO: not correct w.r.t. entities
      ConvWrite(attr.nodeValue,
        AttrSpecialChars[xwfSpecialCharsInAttributeValue in FWriteFlags],
        @AttrSpecialCharCallback);
      wrtChr('"');
    end;
  end;
end;

procedure TXMLWriter.VisitElement(node: TDOMNode);
var
  i: Integer;
  child: TDOMNode;
  SavedInsideTextNode: Boolean;
begin
  //writeln(Space(FIndentCount*2),'TXMLWriter.VisitElement ',TDOMElement(node).TagName,' FInsideTextNode=',FInsideTextNode);
  if not FInsideTextNode then
    wrtIndent;
  FNSHelper.StartElement;
  wrtChr('<');
  wrtStr(TDOMElement(node).TagName);

  if nfLevel2 in node.Flags then
    NamespaceFixup(TDOMElement(node))
  else if node.HasAttributes then
    for i := 0 to node.Attributes.Length - 1 do
    begin
      child := node.Attributes.Item[i];
      if FCanonical or TDOMAttr(child).Specified then
        VisitAttribute(child);
    end;
  Child := node.FirstChild;
  if Child = nil then
    wrtChars('/>', 2)
  else
  begin
    SavedInsideTextNode := FInsideTextNode;
    wrtChr('>');
    FInsideTextNode := FCanonical or (Child.NodeType in [TEXT_NODE, CDATA_SECTION_NODE])
      or (xwfPreserveWhiteSpace in WriteFlags);
    //writeln(Space(FIndentCount*2),'TXMLWriter.VisitElement START FirstChild=',Child.ClassName,':',Child.LocalName,' FInsideTextNode=',FInsideTextNode);
    IncIndent;
    repeat
      //writeln(Space(FIndentCount*2),'TXMLWriter.VisitElement CHILD=',Child.ClassName,':',Child.LocalName,' FInsideTextNode=',FInsideTextNode);
      WriteNode(Child);
      FInsideTextNode := FCanonical or (Child.NodeType in [TEXT_NODE, CDATA_SECTION_NODE])
        or (xwfPreserveWhiteSpace in WriteFlags);
      Child := Child.NextSibling;
    until Child = nil;
    DecIndent;
    if not (node.LastChild.NodeType in [TEXT_NODE, CDATA_SECTION_NODE]) then
      wrtIndent;
    FInsideTextNode := SavedInsideTextNode;
    //writeln(Space(FIndentCount*2),'TXMLWriter.VisitElement END Node=',Node.ClassName,':',Node.LocalName,' FInsideTextNode=',FInsideTextNode);
    wrtChars('</', 2);
    wrtStr(TDOMElement(Node).TagName);
    wrtChr('>');
  end;
  FNSHelper.EndElement;
end;

procedure TXMLWriter.VisitText(node: TDOMNode);
begin
  ConvWrite(TDOMCharacterData(node).Data, TextSpecialChars, TextnodeCallbacks[FCanonical]);
end;

procedure TXMLWriter.VisitCDATA(node: TDOMNode);
begin
  if not FInsideTextNode then
    wrtIndent;
  if FCanonical then
    ConvWrite(TDOMCharacterData(node).Data, TextSpecialChars, @TextnodeCanonicalCallback)
  else
  begin
    wrtChars('<![CDATA[', 9);
    ConvWrite(TDOMCharacterData(node).Data, CDSectSpecialChars, @CDSectSpecialCharCallback);
    wrtChars(']]>', 3);
  end;
end;

procedure TXMLWriter.VisitEntityRef(node: TDOMNode);
begin
  wrtChr('&');
  wrtStr(node.NodeName);
  wrtChr(';');
end;

procedure TXMLWriter.VisitPI(node: TDOMNode);
begin
  if not FInsideTextNode then wrtIndent;
  wrtStr('<?');
  wrtStr(TDOMProcessingInstruction(node).Target);
  if TDOMProcessingInstruction(node).Data <> '' then
  begin
    wrtChr(' ');
    // TODO: How does this comply with c14n??
    ConvWrite(TDOMProcessingInstruction(node).Data, LineEndingChars, @TextnodeNormalCallback);
  end;
  wrtStr('?>');
end;

procedure TXMLWriter.VisitComment(node: TDOMNode);
begin
  if not FInsideTextNode then wrtIndent;
  wrtChars('<!--', 4);
  // TODO: How does this comply with c14n??
  ConvWrite(TDOMCharacterData(node).Data, LineEndingChars, @TextnodeNormalCallback);
  wrtChars('-->', 3);
end;

procedure TXMLWriter.VisitDocument(node: TDOMNode);
var
  child: TDOMNode;
begin
  wrtStr('<?xml version="');
  // Definitely should not escape anything here
  if Length(TXMLDocument(node).XMLVersion) > 0 then
    wrtStr(TXMLDocument(node).XMLVersion)
  else
    wrtStr('1.0');
  wrtChr('"');
  
// DISABLED - we are only able write in UTF-8 which does not require labeling
// writing incorrect encoding will render xml unreadable...
(*
  if Length(TXMLDocument(node).Encoding) > 0 then
  begin
    wrtStr(' encoding="');
    wrtStr(TXMLDocument(node).Encoding);
    wrtChr('"');
  end;
*)
  wrtStr(' encoding="UTF-8"');
  wrtStr('?>');

  // TODO: now handled as a regular PI, remove this?
  if Length(TXMLDocument(node).StylesheetType) > 0 then
  begin
    wrtStr(FLineBreak);
    wrtStr('<?xml-stylesheet type="');
    wrtStr(TXMLDocument(node).StylesheetType);
    wrtStr('" href="');
    wrtStr(TXMLDocument(node).StylesheetHRef);
    wrtStr('"?>');
  end;

  child := node.FirstChild;
  while Assigned(Child) do
  begin
    WriteNode(Child);
    Child := Child.NextSibling;
  end;
  wrtStr(FLineBreak);
end;

procedure TXMLWriter.VisitDocument_Canonical(Node: TDOMNode);
var
  child, root: TDOMNode;
begin
  root := TDOMDocument(Node).DocumentElement;
  child := node.FirstChild;
  while Assigned(child) and (child <> root) do
  begin
    if child.nodeType in [COMMENT_NODE, PROCESSING_INSTRUCTION_NODE] then
    begin
      WriteNode(child);
      wrtChr(#10);
    end;
    child := child.nextSibling;
  end;
  if root = nil then
    Exit;
  VisitElement(TDOMElement(root));
  child := root.nextSibling;
  while Assigned(child) do
  begin
    if child.nodeType in [COMMENT_NODE, PROCESSING_INSTRUCTION_NODE] then
    begin
      wrtChr(#10);
      WriteNode(child);
    end;
    child := child.nextSibling;
  end;
end;

procedure TXMLWriter.VisitAttribute(Node: TDOMNode);
var
  Child: TDOMNode;
begin
  wrtChr(' ');
  wrtStr(TDOMAttr(Node).Name);
  wrtChars('="', 2);
  Child := Node.FirstChild;
  while Assigned(Child) do
  begin
    //writeln('TXMLWriter.VisitAttribute ',Child.NodeType);
    case Child.NodeType of
      ENTITY_REFERENCE_NODE:
        VisitEntityRef(Child);
      TEXT_NODE:
        ConvWrite(TDOMCharacterData(Child).Data,
          AttrSpecialChars[xwfSpecialCharsInAttributeValue in FWriteFlags],
          @AttrSpecialCharCallback);
    end;
    Child := Child.NextSibling;
  end;
  wrtChr('"');
end;

procedure TXMLWriter.VisitDocumentType(Node: TDOMNode);
begin
  wrtStr(FLineBreak);
  wrtStr('<!DOCTYPE ');
  wrtStr(Node.NodeName);
  wrtChr(' ');
  with TDOMDocumentType(Node) do
  begin
    if PublicID <> '' then
    begin
      wrtStr('PUBLIC ');
      wrtQuotedLiteral(PublicID);
      wrtChr(' ');
      wrtQuotedLiteral(SystemID);
    end
    else if SystemID <> '' then
    begin
      wrtStr('SYSTEM ');
      wrtQuotedLiteral(SystemID);
    end;
    if InternalSubset <> '' then
    begin
      wrtChr('[');
      ConvWrite(InternalSubset, LineEndingChars, @TextnodeNormalCallback);
      wrtChr(']');
    end;
  end;
  wrtChr('>');
end;

procedure TXMLWriter.VisitFragment(Node: TDOMNode);
var
  Child: TDOMNode;
begin
  // TODO: TextDecl is probably needed
  // Fragment itself should not be written, only its children should...
  Child := Node.FirstChild;
  while Assigned(Child) do
  begin
    WriteNode(Child);
    Child := Child.NextSibling;
  end;
end;


// -------------------------------------------------------------------
//   Interface implementation
// -------------------------------------------------------------------

procedure WriteXMLFile(doc: TXMLDocument; const AFileName: String;
  Flags: TXMLWriterFlags = []);
var
  fs: TFileStreamUTF8;
begin
  fs := TFileStreamUTF8.Create(AFileName, fmCreate);
  try
    WriteXMLFile(doc, fs, Flags);
  finally
    fs.Free;
  end;
end;

procedure WriteXMLFile(doc: TXMLDocument; var AFile: Text;
  Flags: TXMLWriterFlags = []);
begin
  with TTextXMLWriter.Create(AFile) do
  try
    WriteFlags:=Flags;
    WriteNode(doc);
  finally
    Free;
  end;
end;

procedure WriteXMLFile(doc: TXMLDocument; AStream: TStream;
  Flags: TXMLWriterFlags = []);
begin
  with TStreamXMLWriter.Create(AStream) do
  try
    WriteFlags:=Flags;
    WriteNode(doc);
  finally
    Free;
  end;
end;

procedure WriteXML(Element: TDOMNode; const AFileName: String; Flags: TXMLWriterFlags = []);
begin
  WriteXMLFile(TXMLDocument(Element), AFileName, Flags);
end;

procedure WriteXML(Element: TDOMNode; var AFile: Text; Flags: TXMLWriterFlags = []);
begin
  WriteXMLFile(TXMLDocument(Element), AFile, Flags);
end;

procedure WriteXML(Element: TDOMNode; AStream: TStream; Flags: TXMLWriterFlags = []);
begin
  WriteXMLFile(TXMLDocument(Element), AStream, Flags);
end;



end.
