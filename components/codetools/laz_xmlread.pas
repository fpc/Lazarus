{
  BEWARE !!!
  This is a TEMPORARY file.
  As soon as it is moved to the fcl, it will be removed.
}

{
    $Id$
    This file is part of the Free Component Library

    XML reading routines.
    Copyright (c) 1999-2000 by Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit Laz_XMLRead;

interface

uses SysUtils, Classes, Laz_DOM;

type

  EXMLReadError = class(Exception);


procedure ReadXMLFile(var ADoc: TXMLDocument; const AFilename: String);
procedure ReadXMLFile(var ADoc: TXMLDocument; var f: File);
procedure ReadXMLFile(var ADoc: TXMLDocument; var f: TStream);
procedure ReadXMLFile(var ADoc: TXMLDocument; var f: TStream;
  const AFilename: String);

procedure ReadDTDFile(var ADoc: TXMLDocument; const AFilename: String);
procedure ReadDTDFile(var ADoc: TXMLDocument; var f: File);
procedure ReadDTDFile(var ADoc: TXMLDocument; var f: TStream);
procedure ReadDTDFile(var ADoc: TXMLDocument; var f: TStream;
  const AFilename: String);


// =======================================================

implementation

const

  Letter = ['A'..'Z', 'a'..'z'];
  Digit = ['0'..'9'];
  PubidChars: set of Char = [' ', #13, #10, 'a'..'z', 'A'..'Z', '0'..'9',
    '-', '''', '(', ')', '+', ',', '.', '/', ':', '=', '?', ';', '!', '*',
    '#', '@', '$', '_', '%'];
  WhitespaceChars: set of Char = [#9, #10, #13, ' '];

  NmToken: set of Char = Letter + Digit + ['.', '-', '_', ':'];

type

  TXMLReaderDocument = class(TXMLDocument)
  public
    procedure SetDocType(ADocType: TDOMDocumentType);
  end;

  TXMLReaderDocumentType = class(TDOMDocumentType)
  public
    constructor Create(ADocument: TXMLReaderDocument);
    property Name: DOMString read FNodeName write FNodeName;
  end;


  TSetOfChar = set of Char;

  TXMLReader = class
  protected
    buf, BufStart: PChar;
    Filename: String;

    procedure RaiseExc(descr: String);
    function  SkipWhitespace: Boolean;
    procedure ExpectWhitespace;
    procedure ExpectString(s: String);
    function  CheckFor(s: PChar): Boolean;
    procedure SkipString(ValidChars: TSetOfChar);
    function  GetString(ValidChars: TSetOfChar): String;
    function  GetString(BufPos: PChar; Len: integer): String;

    function  GetName(var s: String): Boolean;
    function  ExpectName: String;                                       // [5]
    procedure ExpectAttValue(attr: TDOMAttr);                           // [10]
    function  ExpectPubidLiteral: String;                               // [12]
    function  ParseComment(AOwner: TDOMNode): Boolean;                  // [15]
    function  ParsePI: Boolean;                                         // [16]
    procedure ExpectProlog;                                             // [22]
    function  ParseEq: Boolean;                                         // [25]
    procedure ExpectEq;
    procedure ParseMisc(AOwner: TDOMNode);                              // [27]
    function  ParseMarkupDecl: Boolean;                                 // [29]
    function  ParseElement(AOwner: TDOMNode): Boolean;                  // [39]
    procedure ExpectElement(AOwner: TDOMNode);
    function  ParseReference(AOwner: TDOMNode): Boolean;                // [67]
    procedure ExpectReference(AOwner: TDOMNode);
    function  ParsePEReference: Boolean;                                // [69]
    function  ParseExternalID: Boolean;                                 // [75]
    procedure ExpectExternalID;
    function  ParseEncodingDecl: String;                                // [80]

    procedure ResolveEntities(RootNode: TDOMNode);
  public
    doc: TXMLReaderDocument;
    procedure ProcessXML(ABuf: PChar; AFilename: String);  // [1]
    procedure ProcessDTD(ABuf: PChar; AFilename: String);  // ([29])
  end;



procedure TXMLReaderDocument.SetDocType(ADocType: TDOMDocumentType);
begin
  FDocType := ADocType;
end;


constructor TXMLReaderDocumentType.Create(ADocument: TXMLReaderDocument);
begin
  inherited Create(ADocument);
end;



procedure TXMLReader.RaiseExc(descr: String);
var
  apos: PChar;
  x, y: Integer;
begin
  // find out the line in which the error occured
  apos := BufStart;
  x := 1;
  y := 1;
  while apos < buf do begin
    if apos[0] = #10 then begin
      Inc(y);
      x := 1;
    end else
      Inc(x);
    Inc(apos);
  end;

  raise EXMLReadError.Create('In ' + Filename + ' (line ' + IntToStr(y) + ' pos ' +
    IntToStr(x) + '): ' + descr);
end;

function TXMLReader.SkipWhitespace: Boolean;
begin
  Result := False;
  while buf[0] in WhitespaceChars do
  begin
    Inc(buf);
    Result := True;
  end;
end;

procedure TXMLReader.ExpectWhitespace;
begin
  if not SkipWhitespace then
    RaiseExc('Expected whitespace');
end;

procedure TXMLReader.ExpectString(s: String);
var
  i: Integer;
  s2: PChar;
  s3: String;
begin
  for i := 1 to Length(s) do
    if buf[i - 1] <> s[i] then begin
      GetMem(s2, Length(s) + 1);
      StrLCopy(s2, buf, Length(s));
      s3 := StrPas(s2);
      FreeMem(s2, Length(s) + 1);
      RaiseExc('Expected "' + s + '", found "' + s3 + '"');
    end;
  Inc(buf, Length(s));
end;

function TXMLReader.CheckFor(s: PChar): Boolean;
begin
  if buf[0] = #0 then begin
    Result := False;
    exit;
  end;
  if StrLComp(buf, s, StrLen(s)) = 0 then begin
    Inc(buf, StrLen(s));
    Result := True;
  end else
    Result := False;
end;

procedure TXMLReader.SkipString(ValidChars: TSetOfChar);
begin
  while buf[0] in ValidChars do begin
    Inc(buf);
  end;
end;

function TXMLReader.GetString(ValidChars: TSetOfChar): String;
var
  OldBuf: PChar;
  i, len: integer;
begin
  OldBuf:=Buf;
  while buf[0] in ValidChars do begin
    Inc(buf);
  end;
  len:=buf-OldBuf;
  SetLength(Result, Len);
  for i:=1 to len do begin
    Result[i]:=OldBuf[0];
    inc(OldBuf);
  end;
end;

function TXMLReader.GetString(BufPos: PChar; Len: integer): string;
var i: integer;
begin
  SetLength(Result,Len);
  for i:=1 to Len do begin
    Result[i]:=BufPos[0];
    inc(BufPos);
  end;
end;

procedure TXMLReader.ProcessXML(ABuf: PChar; AFilename: String);    // [1]
//var
//  LastNodeBeforeDoc: TDOMNode;
begin
  buf := ABuf;
  BufStart := ABuf;
  Filename := AFilename;

  doc := TXMLReaderDocument.Create;
  ExpectProlog;
  //LastNodeBeforeDoc := doc.LastChild;
  ExpectElement(doc);
  ParseMisc(doc);

  if buf[0] <> #0 then
    RaiseExc('Text after end of document element found');

  {
  if buf[0] <> #0 then begin
    WriteLn('=== Unparsed: ===');
    //WriteLn(buf);
    WriteLn(StrLen(buf), ' chars');
  end;
  }
end;


function TXMLReader.GetName(var s: String): Boolean;    // [5]
var OldBuf: PChar;
begin
  SetLength(s, 0);
  if not (buf[0] in (Letter + ['_', ':'])) then begin
    Result := False;
    exit;
  end;

  OldBuf := buf;
  Inc(buf);
  SkipString(Letter + ['0'..'9', '.', '-', '_', ':']);
  s := GetString(OldBuf,buf-OldBuf);
  Result := True;
end;

function TXMLReader.ExpectName: String;    // [5]
var OldBuf: PChar;
begin
  if not (buf[0] in (Letter + ['_', ':'])) then
    RaiseExc('Expected letter, "_" or ":" for name, found "' + buf[0] + '"');

  OldBuf := buf;
  Inc(buf);
  SkipString(Letter + ['0'..'9', '.', '-', '_', ':']);
  Result:=GetString(OldBuf,buf-OldBuf);
end;

procedure TXMLReader.ExpectAttValue(attr: TDOMAttr);    // [10]
var
  s: String;
  OldBuf: PChar;

  procedure FlushStringBuffer;
  begin
    if OldBuf<>buf then begin
      s := s + GetString(OldBuf,buf-OldBuf);
      OldBuf := buf;
    end;
    if Length(s) > 0 then
    begin
      attr.AppendChild(doc.CreateTextNode(s));
      SetLength(s, 0);
    end;
  end;

var
  StrDel: array[0..1] of Char;	// String delimiter
begin
  if (buf[0] <> '''') and (buf[0] <> '"') then
    RaiseExc('Expected quotation marks');
  StrDel[0] := buf[0];
  StrDel[1] := #0;
  Inc(buf);
  SetLength(s, 0);
  OldBuf := buf;
  while not CheckFor(StrDel) do
    if buf[0] = '&' then
    begin
      FlushStringBuffer;
      ParseReference(attr);
      OldBuf := buf;
    end else
    begin
      Inc(buf);
    end;
  dec(buf);
  FlushStringBuffer;
  inc(buf);
  ResolveEntities(Attr);
end;

function TXMLReader.ExpectPubidLiteral: String;
begin
  SetLength(Result, 0);
  if CheckFor('''') then begin
    SkipString(PubidChars - ['''']);
    ExpectString('''');
  end else if CheckFor('"') then begin
    SkipString(PubidChars - ['"']);
    ExpectString('"');
  end else
    RaiseExc('Expected quotation marks');
end;

function TXMLReader.ParseComment(AOwner: TDOMNode): Boolean;    // [15]
var
  comment: String;
  OldBuf: PChar;
begin
  if CheckFor('<!--') then begin
    SetLength(comment, 0);
    OldBuf := buf;
    while (buf[0] <> #0) and (buf[1] <> #0) and
      ((buf[0] <> '-') or (buf[1] <> '-')) do begin
      Inc(buf);
    end;
    comment:=GetString(OldBuf,buf-OldBuf);
    AOwner.AppendChild(doc.CreateComment(comment));
    ExpectString('-->');
    Result := True;
  end else
    Result := False;
end;

function TXMLReader.ParsePI: Boolean;    // [16]
var
  checkbuf: array[0..3] of char;
begin
  if CheckFor('<?') then begin
    StrLCopy(checkbuf, buf, 3);
    if UpCase(StrPas(checkbuf)) = 'XML' then
      RaiseExc('"<?xml" processing instruction not allowed here');
    ExpectName;
    if SkipWhitespace then
      while (buf[0] <> #0) and (buf[1] <> #0) and not
        ((buf[0] = '?') and (buf[1] = '>')) do Inc(buf);
    ExpectString('?>');
    Result := True;
  end else
    Result := False;
end;

procedure TXMLReader.ExpectProlog;    // [22]

  procedure ParseVersionNum;
  begin
    doc.XMLVersion :=
      GetString(['a'..'z', 'A'..'Z', '0'..'9', '_', '.', ':', '-']);
  end;

  procedure ParseDoctypeDecls;
  begin
    repeat
      SkipWhitespace;
    until not (ParseMarkupDecl or ParsePEReference);
    ExpectString(']');
  end;


var
  DocType: TXMLReaderDocumentType;

begin
  if CheckFor('<?xml') then
  begin
    // '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'

    // VersionInfo: S 'version' Eq (' VersionNum ' | " VersionNum ")
    SkipWhitespace;
    ExpectString('version');
    ParseEq;
    if buf[0] = '''' then
    begin
      Inc(buf);
      ParseVersionNum;
      ExpectString('''');
    end else if buf[0] = '"' then
    begin
      Inc(buf);
      ParseVersionNum;
      ExpectString('"');
    end else
      RaiseExc('Expected single or double quotation mark');

    // EncodingDecl?
    ParseEncodingDecl;

    // SDDecl?
    SkipWhitespace;
    if CheckFor('standalone') then
    begin
      ExpectEq;
      if buf[0] = '''' then
      begin
        Inc(buf);
        if not (CheckFor('yes''') or CheckFor('no''')) then
          RaiseExc('Expected ''yes'' or ''no''');
      end else if buf[0] = '''' then
      begin
        Inc(buf);
        if not (CheckFor('yes"') or CheckFor('no"')) then
          RaiseExc('Expected "yes" or "no"');
      end;
      SkipWhitespace;
    end;

    ExpectString('?>');
  end;

  // Check for "Misc*"
  ParseMisc(doc);

  // Check for "(doctypedecl Misc*)?"    [28]
  if CheckFor('<!DOCTYPE') then
  begin
    DocType := TXMLReaderDocumentType.Create(doc);
    doc.SetDocType(DocType);
    SkipWhitespace;
    DocType.Name := ExpectName;
    SkipWhitespace;
    if CheckFor('[') then
    begin
      ParseDoctypeDecls;
      SkipWhitespace;
      ExpectString('>');
    end else if not CheckFor('>') then
    begin
      ParseExternalID;
      SkipWhitespace;
      if CheckFor('[') then
      begin
        ParseDoctypeDecls;
        SkipWhitespace;
      end;
      ExpectString('>');
    end;
    ParseMisc(doc);
  end;
end;

function TXMLReader.ParseEq: Boolean;    // [25]
var
  savedbuf: PChar;
begin
  savedbuf := buf;
  SkipWhitespace;
  if buf[0] = '=' then begin
    Inc(buf);
    SkipWhitespace;
    Result := True;
  end else begin
    buf := savedbuf;
    Result := False;
  end;
end;

procedure TXMLReader.ExpectEq;
begin
  if not ParseEq then
    RaiseExc('Expected "="');
end;


// Parse "Misc*":
//   Misc ::= Comment | PI | S

procedure TXMLReader.ParseMisc(AOwner: TDOMNode);    // [27]
begin
  repeat
    SkipWhitespace;
  until not (ParseComment(AOwner) or ParsePI);
end;

function TXMLReader.ParseMarkupDecl: Boolean;    // [29]

  function ParseElementDecl: Boolean;    // [45]

    procedure ExpectChoiceOrSeq;    // [49], [50]

      procedure ExpectCP;    // [48]
      begin
        if CheckFor('(') then
          ExpectChoiceOrSeq
        else
          ExpectName;
        if CheckFor('?') then
        else if CheckFor('*') then
        else if CheckFor('+') then;
      end;

    var
      delimiter: Char;
    begin
      SkipWhitespace;
      ExpectCP;
      SkipWhitespace;
      delimiter := #0;
      while not CheckFor(')') do begin
        if delimiter = #0 then begin
          if (buf[0] = '|') or (buf[0] = ',') then
            delimiter := buf[0]
          else
            RaiseExc('Expected "|" or ","');
          Inc(buf);
        end else
          ExpectString(delimiter);
        SkipWhitespace;
        ExpectCP;
      end;
    end;

  begin
    if CheckFor('<!ELEMENT') then begin
      ExpectWhitespace;
      ExpectName;
      ExpectWhitespace;

      // Get contentspec [46]

      if CheckFor('EMPTY') then
      else if CheckFor('ANY') then
      else if CheckFor('(') then begin
        SkipWhitespace;
        if CheckFor('#PCDATA') then begin
          // Parse Mixed section [51]
          SkipWhitespace;
          if not CheckFor(')') then
            repeat
              ExpectString('|');
              SkipWhitespace;
              ExpectName;
            until CheckFor(')*');
        end else begin
          // Parse Children section [47]

          ExpectChoiceOrSeq;

          if CheckFor('?') then
          else if CheckFor('*') then
          else if CheckFor('+') then;
        end;
      end else
        RaiseExc('Invalid content specification');

      SkipWhitespace;
      ExpectString('>');
      Result := True;
    end else
      Result := False;
  end;

  function ParseAttlistDecl: Boolean;    // [52]
  var
    attr: TDOMAttr;
  begin
    if CheckFor('<!ATTLIST') then begin
      ExpectWhitespace;
      ExpectName;
      SkipWhitespace;
      while not CheckFor('>') do begin
        ExpectName;
        ExpectWhitespace;

        // Get AttType [54], [55], [56]
        if CheckFor('CDATA') then
        else if CheckFor('ID') then
        else if CheckFor('IDREF') then
        else if CheckFor('IDREFS') then
        else if CheckFor('ENTITTY') then
        else if CheckFor('ENTITIES') then
        else if CheckFor('NMTOKEN') then
        else if CheckFor('NMTOKENS') then
        else if CheckFor('NOTATION') then begin   // [57], [58]
          ExpectWhitespace;
          ExpectString('(');
          SkipWhitespace;
          ExpectName;
          SkipWhitespace;
          while not CheckFor(')') do begin
            ExpectString('|');
            SkipWhitespace;
            ExpectName;
            SkipWhitespace;
          end;
        end else if CheckFor('(') then begin    // [59]
          SkipWhitespace;
          SkipString(Nmtoken);
          SkipWhitespace;
          while not CheckFor(')') do begin
            ExpectString('|');
            SkipWhitespace;
            SkipString(Nmtoken);
            SkipWhitespace;
          end;
        end else
          RaiseExc('Invalid tokenized type');

        ExpectWhitespace;

        // Get DefaultDecl [60]
        if CheckFor('#REQUIRED') then
        else if CheckFor('#IMPLIED') then
        else begin
          if CheckFor('#FIXED') then
            SkipWhitespace;
          attr := doc.CreateAttribute('');
          ExpectAttValue(attr);
        end;

        SkipWhitespace;
      end;
      Result := True;
    end else
      Result := False;
  end;

  function ParseEntityDecl: Boolean;    // [70]
  var
    NewEntity: TDOMEntity;

    function ParseEntityValue: Boolean;    // [9]
    var
      strdel: array[0..1] of Char;
    begin
      if (buf[0] <> '''') and (buf[0] <> '"') then begin
        Result := False;
        exit;
      end;
      strdel[0] := buf[0];
      strdel[1] := #0;
      Inc(buf);
      while not CheckFor(strdel) do
        if ParsePEReference then
        else if ParseReference(NewEntity) then
        else begin
          Inc(buf);             // Normal haracter
        end;
      Result := True;
    end;

  begin
    if CheckFor('<!ENTITY') then begin
      ExpectWhitespace;
      if CheckFor('%') then begin    // [72]
        ExpectWhitespace;
        NewEntity := doc.CreateEntity(ExpectName);
        ExpectWhitespace;
        // Get PEDef [74]
        if ParseEntityValue then
        else if ParseExternalID then
        else
          RaiseExc('Expected entity value or external ID');
      end else begin    // [71]
        NewEntity := doc.CreateEntity(ExpectName);
        ExpectWhitespace;
        // Get EntityDef [73]
        if ParseEntityValue then
        else begin
          ExpectExternalID;
          // Get NDataDecl [76]
          ExpectWhitespace;
          ExpectString('NDATA');
          ExpectWhitespace;
          ExpectName;
        end;
      end;
      SkipWhitespace;
      ExpectString('>');
      Result := True;
    end else
      Result := False;
  end;

  function ParseNotationDecl: Boolean;    // [82]
  begin
    if CheckFor('<!NOTATION') then begin
      ExpectWhitespace;
      ExpectName;
      ExpectWhitespace;
      if ParseExternalID then
      else if CheckFor('PUBLIC') then begin    // [83]
        ExpectWhitespace;
        ExpectPubidLiteral;
      end else
        RaiseExc('Expected external or public ID');
      SkipWhitespace;
      ExpectString('>');
      Result := True;
    end else
      Result := False;
  end;

begin
  Result := False;
  while ParseElementDecl or ParseAttlistDecl or ParseEntityDecl or
    ParseNotationDecl or ParsePI or ParseComment(doc) or SkipWhitespace do
    Result := True;
end;

procedure TXMLReader.ProcessDTD(ABuf: PChar; AFilename: String);
begin
  buf := ABuf;
  BufStart := ABuf;
  Filename := AFilename;

  doc := TXMLReaderDocument.Create;
  ParseMarkupDecl;

  {
  if buf[0] <> #0 then begin
    WriteLn('=== Unparsed: ===');
    //WriteLn(buf);
    WriteLn(StrLen(buf), ' chars');
  end;
  }
end;

function TXMLReader.ParseElement(AOwner: TDOMNode): Boolean;    // [39] [40] [44]
var
  NewElem: TDOMElement;

  function ParseCharData: Boolean;    // [14]
  var
    s: String;
    i: Integer;
    OldBuf: PChar;
  begin
    SetLength(s, 0);
    OldBuf := buf;
    while not (buf[0] in [#0, '<', '&']) do
    begin
      Inc(buf);
    end;
    s:=GetString(OldBuf,buf-OldBuf);
    if Length(s) > 0 then
    begin
      // Check if s has non-whitespace content
      i := Length(s);
      while (i > 0) and (s[i] in WhitespaceChars) do
        Dec(i);
      if i > 0 then
        NewElem.AppendChild(doc.CreateTextNode(s));
      Result := True;
    end else
      Result := False;
  end;

  function ParseCDSect: Boolean;    // [18]
  var
    cdata: String;
    OldBuf: PChar;
  begin
    if CheckFor('<![CDATA[') then
    begin
      SetLength(cdata, 0);
      OldBuf := buf;
      while not CheckFor(']]>') do
      begin
        Inc(buf);
      end;
      cdata := GetString(OldBuf,buf-OldBuf);
      NewElem.AppendChild(doc.CreateCDATASection(cdata));
      Result := True;
    end else
      Result := False;
  end;



var
  IsEmpty: Boolean;
  name: String;
  oldpos: PChar;

  attr: TDOMAttr;
begin
  oldpos := buf;
  if CheckFor('<') then
  begin
    if not GetName(name) then
    begin
      buf := oldpos;
      Result := False;
      exit;
    end;

    NewElem := doc.CreateElement(name);
    AOwner.AppendChild(NewElem);

    SkipWhitespace;
    IsEmpty := False;
    while True do
    begin
      if CheckFor('/>') then
      begin
        IsEmpty := True;
        break;
      end;
      if CheckFor('>') then
        break;

      // Get Attribute [41]
      attr := doc.CreateAttribute(ExpectName);
      NewElem.Attributes.SetNamedItem(attr);
      ExpectEq;
      ExpectAttValue(attr);

      SkipWhitespace;
    end;

    if not IsEmpty then
    begin
      // Get content
      SkipWhitespace;
      while ParseCharData or ParseCDSect or ParsePI or
        ParseComment(NewElem) or ParseElement(NewElem) or
        ParseReference(NewElem) do;

      // Get ETag [42]
      ExpectString('</');
      if ExpectName <> name then
        RaiseExc('Unmatching element end tag (expected "</' + name + '>")');
      SkipWhitespace;
      ExpectString('>');
    end;

    ResolveEntities(NewElem);



    Result := True;
  end else
    Result := False;
end;

procedure TXMLReader.ExpectElement(AOwner: TDOMNode);
begin
  if not ParseElement(AOwner) then
    RaiseExc('Expected element');
end;

function TXMLReader.ParsePEReference: Boolean;    // [69]
begin
  if CheckFor('%') then begin
    ExpectName;
    ExpectString(';');
    Result := True;
  end else
    Result := False;
end;

function TXMLReader.ParseReference(AOwner: TDOMNode): Boolean;    // [67] [68]
begin
  if not CheckFor('&') then begin
    Result := False;
    exit;
  end;
  if CheckFor('#') then begin    // Test for CharRef [66]
    if CheckFor('x') then begin
      // !!!: there must be at least one digit
      while buf[0] in ['0'..'9', 'a'..'f', 'A'..'F'] do Inc(buf);
    end else
      // !!!: there must be at least one digit
      while buf[0] in ['0'..'9'] do Inc(buf);
  end else
    AOwner.AppendChild(doc.CreateEntityReference(ExpectName));
  ExpectString(';');
  Result := True;
end;

procedure TXMLReader.ExpectReference(AOwner: TDOMNode);
begin
  if not ParseReference(AOwner) then
    RaiseExc('Expected reference ("&Name;" or "%Name;")');
end;


function TXMLReader.ParseExternalID: Boolean;    // [75]

  function GetSystemLiteral: String;
  var
    OldBuf: PChar;
  begin
    SetLength(Result, 0);
    if buf[0] = '''' then begin
      Inc(buf);
      OldBuf := buf;
      while (buf[0] <> '''') and (buf[0] <> #0) do begin
        Inc(buf);
      end;
      Result := GetString(OldBuf,buf-OldBuf);
      ExpectString('''');
    end else if buf[0] = '"' then begin
      Inc(buf);
      OldBuf := buf;
      while (buf[0] <> '"') and (buf[0] <> #0) do begin
        Inc(buf);
      end;
      Result := GetString(OldBuf,buf-OldBuf);
      ExpectString('"');
    end;
  end;

begin
  if CheckFor('SYSTEM') then begin
    ExpectWhitespace;
    GetSystemLiteral;
    Result := True;
  end else if CheckFor('PUBLIC') then begin
    ExpectWhitespace;
    ExpectPubidLiteral;
    ExpectWhitespace;
    GetSystemLiteral;
    Result := True;
  end else
    Result := False;
end;

procedure TXMLReader.ExpectExternalID;
begin
  if not ParseExternalID then
    RaiseExc('Expected external ID');
end;

function TXMLReader.ParseEncodingDecl: String;    // [80]

  function ParseEncName: String;
  var OldBuf: PChar;
  begin
    if not (buf[0] in ['A'..'Z', 'a'..'z']) then
      RaiseExc('Expected character (A-Z, a-z)');
    OldBuf := buf;
    Inc(buf);
    SkipString(['A'..'Z', 'a'..'z', '0'..'9', '.', '_', '-']);
    Result := GetString(OldBuf,buf-OldBuf);
  end;

begin
  SetLength(Result, 0);
  SkipWhitespace;
  if CheckFor('encoding') then begin
    ExpectEq;
    if buf[0] = '''' then begin
      Inc(buf);
      Result := ParseEncName;
      ExpectString('''');
    end else if buf[0] = '"' then begin
      Inc(buf);
      Result := ParseEncName;
      ExpectString('"');
    end;
  end;
end;


{ Currently, this method will only resolve the entities which are
  predefined in XML: }

procedure TXMLReader.ResolveEntities(RootNode: TDOMNode);

  procedure ReplaceEntityRef(EntityNode: TDOMNode; const Replacement: String);
  var
    PrevSibling, NextSibling: TDOMNode;
  begin
    PrevSibling := EntityNode.PreviousSibling;
    NextSibling := EntityNode.NextSibling;
    if Assigned(PrevSibling) and (PrevSibling.NodeType = TEXT_NODE) then
    begin
      TDOMCharacterData(PrevSibling).AppendData(Replacement);
      RootNode.RemoveChild(EntityNode);
      if Assigned(NextSibling) and (NextSibling.NodeType = TEXT_NODE) then
      begin
        TDOMCharacterData(PrevSibling).AppendData(
        TDOMCharacterData(NextSibling).Data);
        RootNode.RemoveChild(NextSibling);
      end
    end else
      if Assigned(NextSibling) and (NextSibling.NodeType = TEXT_NODE) then
      begin
        TDOMCharacterData(NextSibling).InsertData(0, Replacement);
        RootNode.RemoveChild(EntityNode);
      end else
        RootNode.ReplaceChild(Doc.CreateTextNode(Replacement), EntityNode);
  end;

var
  Node, NextSibling: TDOMNode;
begin
  Node := RootNode.FirstChild;
  while Assigned(Node) do
  begin
    NextSibling := Node.NextSibling;
    if Node.NodeType = ENTITY_REFERENCE_NODE then
      if Node.NodeName = 'amp' then
        ReplaceEntityRef(Node, '&')
      else if Node.NodeName = 'apos' then
        ReplaceEntityRef(Node, '''')
      else if Node.NodeName = 'gt' then
        ReplaceEntityRef(Node, '>')
      else if Node.NodeName = 'lt' then
        ReplaceEntityRef(Node, '<')
      else if Node.NodeName = 'quot' then
        ReplaceEntityRef(Node, '"');
    Node := NextSibling;
  end;
end;



procedure ReadXMLFile(var ADoc: TXMLDocument; var f: File);
var
  reader: TXMLReader;
  buf: PChar;
  BufSize: LongInt;
begin
  ADoc := nil;
  BufSize := FileSize(f) + 1;
  if BufSize <= 1 then exit;

  GetMem(buf, BufSize);
  BlockRead(f, buf^, BufSize - 1);
  buf[BufSize - 1] := #0;
  reader := TXMLReader.Create;
  reader.ProcessXML(buf, Filerec(f).name);
  FreeMem(buf, BufSize);
  ADoc := reader.doc;
  reader.Free;
end;

procedure ReadXMLFile(var ADoc: TXMLDocument; var f: TStream;
  const AFilename: String);
var
  reader: TXMLReader;
  buf: PChar;
begin
  ADoc := nil;
  if f.Size = 0 then exit;

  GetMem(buf, f.Size + 1);
  f.Read(buf^, f.Size);
  buf[f.Size] := #0;
  reader := TXMLReader.Create;
  reader.ProcessXML(buf, AFilename);
  FreeMem(buf, f.Size + 1);
  ADoc := reader.doc;
  reader.Free;
end;

procedure ReadXMLFile(var ADoc: TXMLDocument; var f: TStream);
begin
  ReadXMLFile(ADoc, f, '<Stream>');
end;

procedure ReadXMLFile(var ADoc: TXMLDocument; const AFilename: String);
var
  stream: TFileStream;
begin
  ADoc := nil;
  stream := TFileStream.Create(AFilename, fmOpenRead);
  try
    ReadXMLFile(ADoc, stream, AFilename);
  finally
    stream.Free;
  end;
end;


procedure ReadDTDFile(var ADoc: TXMLDocument; var f: File);
var
  reader: TXMLReader;
  buf: PChar;
  BufSize: LongInt;
begin
  ADoc := nil;
  BufSize := FileSize(f) + 1;
  if BufSize <= 1 then exit;

  GetMem(buf, BufSize + 1);
  BlockRead(f, buf^, BufSize - 1);
  buf[BufSize - 1] := #0;
  reader := TXMLReader.Create;
  reader.ProcessDTD(buf, Filerec(f).name);
  FreeMem(buf, BufSize);
  ADoc := reader.doc;
  reader.Free;
end;

procedure ReadDTDFile(var ADoc: TXMLDocument; var f: TStream;
  const AFilename: String);
var
  reader: TXMLReader;
  buf: PChar;
begin
  ADoc := nil;
  if f.Size = 0 then exit;

  GetMem(buf, f.Size + 1);
  f.Read(buf^, f.Size);
  buf[f.Size] := #0;
  reader := TXMLReader.Create;
  reader.ProcessDTD(buf, AFilename);
  FreeMem(buf, f.Size + 1);
  ADoc := reader.doc;
  reader.Free;
end;

procedure ReadDTDFile(var ADoc: TXMLDocument; var f: TStream);
begin
  ReadDTDFile(ADoc, f, '<Stream>');
end;

procedure ReadDTDFile(var ADoc: TXMLDocument; const AFilename: String);
var
  stream: TFileStream;
begin
  ADoc := nil;
  stream := TFileStream.Create(AFilename, fmOpenRead);
  try
    ReadDTDFile(ADoc, stream, AFilename);
  finally
    stream.Free;
  end;
end;


end.


{
  $Log$
  Revision 1.3  2002/08/04 07:44:44  lazarus
  MG: fixed xml reading writing of special chars

  Revision 1.2  2002/07/30 14:36:28  lazarus
  MG: accelerated xmlread and xmlwrite

  Revision 1.1  2002/07/30 06:24:06  lazarus
  MG: added a faster version of TXMLConfig

  Revision 1.5  2000/10/14 09:41:45  sg
  * Fixed typo in previous fix. (forgot closing bracket. Oops.)

  Revision 1.4  2000/10/14 09:40:44  sg
  * Extended the "Unmatching element end tag" exception, now the expected
    tag name is included in the message string.

  Revision 1.3  2000/07/29 14:52:25  sg
  * Modified the copyright notice to remove ambiguities

  Revision 1.2  2000/07/13 11:33:07  michael
  + removed logs
 
}
