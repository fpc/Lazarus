{
  BEWARE !!!
  This is a TEMPORARY file.
  As soon as it is moved to the fcl, it will be removed.
}

{
    $Id$
    This file is part of the Free Component Library

    XML writing routines
    Copyright (c) 1999-2000 by Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


unit Laz_XMLWrite;

{$MODE objfpc}
{$H+}

interface

uses Classes, Laz_DOM;

procedure WriteXMLFile(doc: TXMLDocument; const AFileName: String);
procedure WriteXMLFile(doc: TXMLDocument; var AFile: Text);
procedure WriteXMLFile(doc: TXMLDocument; var AStream: TStream);

procedure WriteXML(Element: TDOMElement; const AFileName: String);
procedure WriteXML(Element: TDOMElement; var AFile: Text);
procedure WriteXML(Element: TDOMElement; var AStream: TStream);


// ===================================================================

implementation

uses SysUtils;

// -------------------------------------------------------------------
//   Writers for the different node types
// -------------------------------------------------------------------

procedure WriteElement(node: TDOMNode); forward;
procedure WriteAttribute(node: TDOMNode); forward;
procedure WriteText(node: TDOMNode); forward;
procedure WriteCDATA(node: TDOMNode); forward;
procedure WriteEntityRef(node: TDOMNode); forward;
procedure WriteEntity(node: TDOMNode); forward;
procedure WritePI(node: TDOMNode); forward;
procedure WriteComment(node: TDOMNode); forward;
procedure WriteDocument(node: TDOMNode); forward;
procedure WriteDocumentType(node: TDOMNode); forward;
procedure WriteDocumentFragment(node: TDOMNode); forward;
procedure WriteNotation(node: TDOMNode); forward;


type
  TWriteNodeProc = procedure(node: TDOMNode);

const
  WriteProcs: array[ELEMENT_NODE..NOTATION_NODE] of TWriteNodeProc =
    (@WriteElement, @WriteAttribute, @WriteText, @WriteCDATA, @WriteEntityRef,
     @WriteEntity, @WritePI, @WriteComment, @WriteDocument, @WriteDocumentType,
     @WriteDocumentFragment, @WriteNotation);

procedure WriteNode(node: TDOMNode);
begin
  WriteProcs[node.NodeType](node);
end;


// -------------------------------------------------------------------
//   Text file and TStream support
// -------------------------------------------------------------------

type
  TOutputProc = procedure(const Buffer; Count: Longint);

var
  f: ^Text;
  stream: TStream;
  wrt, wrtln: TOutputProc;
  InsideTextNode: Boolean;

procedure Text_Write(const Buffer; Count: Longint);
var s: string;
begin
  if Count>0 then begin
    SetLength(s,Count);
    System.Move(Buffer,s[1],Count);
    Write(f^, s);
  end;
end;

procedure Text_WriteLn(const Buffer; Count: Longint);
var s: string;
begin
  if Count>0 then begin
    SetLength(s,Count);
    System.Move(Buffer,s[1],Count);
    WriteLn(f^, s);
  end;
end;

procedure Stream_Write(const Buffer; Count: Longint);
begin
  if Count > 0 then
    stream.Write(Buffer, Count);
end;

procedure Stream_WriteLn(const Buffer; Count: Longint);
begin
  if Count > 0 then
    stream.Write(Buffer, Count);
  stream.WriteByte(10);
end;

procedure wrtStr(const s: string);
begin
  if s<>'' then
    wrt(s[1],length(s));
end;

procedure wrtStrLn(const s: string);
begin
  if s<>'' then
    wrtln(s[1],length(s));
end;

procedure wrtChr(c: char);
begin
  wrt(c,1);
end;


// -------------------------------------------------------------------
//   Indent handling
// -------------------------------------------------------------------

var
  Indent: String;


procedure IncIndent;
begin
  Indent := Indent + '  ';
end;

procedure DecIndent;
begin
  if Length(Indent) >= 2 then
    SetLength(Indent, Length(Indent) - 2);
end;


// -------------------------------------------------------------------
//   String conversion
// -------------------------------------------------------------------

type
  TCharacters = set of Char;
  TSpecialCharCallback = procedure(c: Char);

const
  AttrSpecialChars = ['"', '&'];
  TextSpecialChars = ['<', '>', '&'];


procedure ConvWrite(const s: String; const SpecialChars: TCharacters;
  const SpecialCharCallback: TSpecialCharCallback);
var
  StartPos, EndPos: Integer;
begin
  StartPos := 1;
  EndPos := 1;
  while EndPos <= Length(s) do
  begin
    if s[EndPos] in SpecialChars then
    begin
      wrt(s[StartPos],EndPos - StartPos);
      SpecialCharCallback(s[EndPos]);
      StartPos := EndPos + 1;
    end;
    Inc(EndPos);
  end;
  if EndPos > StartPos then
    wrt(s[StartPos], EndPos - StartPos);
end;

procedure AttrSpecialCharCallback(c: Char);
const
  QuotStr = '&quot;';
  AmpStr = '&amp;';
begin
  if c = '"' then
    wrtStr(QuotStr)
  else if c = '&' then
    wrtStr(AmpStr)
  else
    wrt(c,1);
end;

procedure TextnodeSpecialCharCallback(c: Char);
const
  ltStr = '&lt;';
  gtStr = '&gt;';
  AmpStr = '&amp;';
begin
  if c = '<' then
    wrtStr(ltStr)
  else if c = '>' then
    wrtStr(gtStr)
  else if c = '&' then
    wrtStr(AmpStr)
  else
    wrt(c,1);
end;


// -------------------------------------------------------------------
//   Node writers implementations
// -------------------------------------------------------------------

procedure WriteElement(node: TDOMNode);
var
  i: Integer;
  attr, child: TDOMNode;
  SavedInsideTextNode: Boolean;
  s: String;
begin
  if not InsideTextNode then
    wrtStr(Indent);
  wrtChr('<');
  wrtStr(node.NodeName);
  for i := 0 to node.Attributes.Length - 1 do
  begin
    attr := node.Attributes.Item[i];
    wrtChr(' ');
    wrtStr(attr.NodeName);
    wrtChr('=');
    s := attr.NodeValue;
    // !!!: Replace special characters in "s" such as '&', '<', '>'
    wrtChr('"');
    ConvWrite(s, AttrSpecialChars, @AttrSpecialCharCallback);
    wrtChr('"');
  end;
  Child := node.FirstChild;
  if Child = nil then begin
    if InsideTextNode then begin
      wrtChr('/'); wrtChr('>');
    end else begin
      wrtChr('/'); wrtln('>',1);
    end;
  end else
  begin
    SavedInsideTextNode := InsideTextNode;
    if InsideTextNode or Child.InheritsFrom(TDOMText) then
      wrtChr('>')
    else
      wrtln('>',1);
    IncIndent;
    repeat
      if Child.InheritsFrom(TDOMText) then
        InsideTextNode := True;
      WriteNode(Child);
      Child := Child.NextSibling;
    until child = nil;
    DecIndent;
    if not InsideTextNode then
      wrtStr(Indent);
    InsideTextNode := SavedInsideTextNode;
    wrtChr('<');
    wrtChr('/');
    wrtStr(node.NodeName);
    if InsideTextNode then
      wrtChr('>')
    else
      wrtln('>',1);
  end;
end;

procedure WriteAttribute(node: TDOMNode);
begin
  WriteLn('WriteAttribute');
  if node=nil then ;
end;

procedure WriteText(node: TDOMNode);
begin
  ConvWrite(node.NodeValue, TextSpecialChars, @TextnodeSpecialCharCallback);
  if node=nil then ;
end;

procedure WriteCDATA(node: TDOMNode);
begin
  if InsideTextNode then
    wrtStr('<![CDATA[' + node.NodeValue + ']]>')
  else
    wrtStrln(Indent + '<![CDATA[' + node.NodeValue + ']]>')
end;

procedure WriteEntityRef(node: TDOMNode);
begin
  wrtChr('&');
  wrtStr(node.NodeName);
  wrtChr(';');
end;

procedure WriteEntity(node: TDOMNode);
begin
  WriteLn('WriteEntity');
  if node=nil then ;
end;

procedure WritePI(node: TDOMNode);
begin
  wrtChr('<'); wrtChr('!');
  wrtStr(TDOMProcessingInstruction(node).Target);
  wrtChr(' ');
  wrtStr(TDOMProcessingInstruction(node).Data);
  wrtChr('>');
  if not InsideTextNode then
    wrtStrln('');
end;

procedure WriteComment(node: TDOMNode);
begin
  wrtStr('<!--');
  wrtStr(node.NodeValue);
  wrtStr('-->');
  if not InsideTextNode then
    wrtStrln('');
end;

procedure WriteDocument(node: TDOMNode);
begin
  WriteLn('WriteDocument');
  if node=nil then ;
end;

procedure WriteDocumentType(node: TDOMNode);
begin
  WriteLn('WriteDocumentType');
  if node=nil then ;
end;

procedure WriteDocumentFragment(node: TDOMNode);
begin
  WriteLn('WriteDocumentFragment');
  if node=nil then ;
end;

procedure WriteNotation(node: TDOMNode);
begin
  WriteLn('WriteNotation');
  if node=nil then ;
end;


procedure InitWriter;
begin
  InsideTextNode := False;
end;

procedure RootWriter(doc: TXMLDocument);
var
  Child: TDOMNode;
begin
  InitWriter;
  wrtStr('<?xml version="');
  if Length(doc.XMLVersion) > 0 then
    wrtStr(doc.XMLVersion)
  else
    wrtStr('1.0');
  wrtChr('"');
  if Length(doc.Encoding) > 0 then
    wrtStr(' encoding="' + doc.Encoding + '"');
  wrtStrln('?>');

  if Length(doc.StylesheetType) > 0 then
    // !!!: Can't handle with HRefs which contain special chars (" and so on)
    wrtStrln(Format('<?xml-stylesheet type="%s" href="%s"?>',
      [doc.StylesheetType, doc.StylesheetHRef]));

  indent := '';

  child := doc.FirstChild;
  while Assigned(Child) do
  begin
    WriteNode(Child);
    Child := Child.NextSibling;
  end;
end;


procedure WriteXMLMemStream(doc: TXMLDocument);
// internally used by the WriteXMLFile procedures
begin
  Stream:=TMemoryStream.Create;
  WriteXMLFile(doc,Stream);
  Stream.Position:=0;
end;

// -------------------------------------------------------------------
//   Interface implementation
// -------------------------------------------------------------------

procedure WriteXMLFile(doc: TXMLDocument; const AFileName: String);
var fs: TFileStream;
begin
  // write first to memory buffer and then as one whole block to file
  WriteXMLMemStream(doc);
  try
    fs := TFileStream.Create(AFileName, fmCreate);
    fs.CopyFrom(Stream,Stream.Size);
    fs.Free;
  finally
    Stream.Free;
  end;
end;

procedure WriteXMLFile(doc: TXMLDocument; var AFile: Text);
begin
  f := @AFile;
  wrt := @Text_Write;
  wrtln := @Text_WriteLn;
  RootWriter(doc);
end;

procedure WriteXMLFile(doc: TXMLDocument; var AStream: TStream);
begin
  Stream := AStream;
  wrt := @Stream_Write;
  wrtln := @Stream_WriteLn;
  RootWriter(doc);
end;


procedure WriteXML(Element: TDOMElement; const AFileName: String);
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  wrt := @Stream_Write;
  wrtln := @Stream_WriteLn;
  InitWriter;
  WriteNode(Element);
  Stream.Free;
end;

procedure WriteXML(Element: TDOMElement; var AFile: Text);
begin
  f := @AFile;
  wrt := @Text_Write;
  wrtln := @Text_WriteLn;
  InitWriter;
  WriteNode(Element);
end;

procedure WriteXML(Element: TDOMElement; var AStream: TStream);
begin
  stream := AStream;
  wrt := @Stream_Write;
  wrtln := @Stream_WriteLn;
  InitWriter;
  WriteNode(Element);
end;


end.


{
  $Log$
  Revision 1.4  2002/09/20 09:27:47  lazarus
  MG: accelerated xml

  Revision 1.3  2002/08/04 07:44:44  lazarus
  MG: fixed xml reading writing of special chars

  Revision 1.2  2002/07/30 14:36:28  lazarus
  MG: accelerated xmlread and xmlwrite

  Revision 1.1  2002/07/30 06:24:06  lazarus
  MG: added a faster version of TXMLConfig

  Revision 1.6  2001/06/07 14:38:44  jonas
    * fixed wrong procvar syntax (patches from Peter)

  Revision 1.5  2000/10/03 20:16:31  sg
  * Now writes Processing Instructions and a stylesheet link, if set

  Revision 1.4  2000/07/29 14:52:25  sg
  * Modified the copyright notice to remove ambiguities

  Revision 1.3  2000/07/25 09:20:08  sg
  * Fixed some small bugs
    - some methods where 'virtual' instead of 'override' in dom.pp
    - corrections regaring wether NodeName or NodeValue is used, for
      some node types (Entity, EntityReference)

  Revision 1.2  2000/07/13 11:33:08  michael
  + removed logs
 
}
