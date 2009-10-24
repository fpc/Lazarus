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

unit Laz_XMLRead;

{$MODE objfpc}
{$H+}
{$inline on}

interface

{off $DEFINE MEM_CHECK}

uses
  {$IFDEF MEM_CHECK}MemCheck,{$ENDIF}
  SysUtils, Classes, types, Laz_DOM, FileProcs;

type

  EXMLReadError = class(Exception)
  public
    Position: PtrInt;
    LineCol: TPoint;
    Descr: string;
  end;


procedure ReadXMLFile(out ADoc: TXMLDocument; const AFilename: String); overload;
procedure ReadXMLFile(out ADoc: TXMLDocument; var f: File); overload;
procedure ReadXMLFile(out ADoc: TXMLDocument; f: TStream); overload;
procedure ReadXMLFile(out ADoc: TXMLDocument; f: TStream; const AFilename: String); overload;

procedure ReadXMLFragment(AParentNode: TDOMNode; const AFilename: String); overload;
procedure ReadXMLFragment(AParentNode: TDOMNode; var f: File); overload;
procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream); overload;
procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream; const AFilename: String); overload;

procedure ReadDTDFile(var ADoc: TXMLDocument; const AFilename: String);  overload;
procedure ReadDTDFile(var ADoc: TXMLDocument; var f: File); overload;
procedure ReadDTDFile(var ADoc: TXMLDocument; var f: TStream); overload;
procedure ReadDTDFile(var ADoc: TXMLDocument; var f: TStream; const AFilename: String); overload;


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

function ComparePChar(p1, p2: PChar): boolean;
begin
  if p1<>p2 then begin
    if (p1<>nil) and (p2<>nil) then begin
      while true do begin
        if (p1^=p2^) then begin
          if p1^<>#0 then begin
            inc(p1);
            inc(p2);
          end else begin
            Result:=true;
            exit;
          end;
        end else begin
          Result:=false;
          exit;
        end;
      end;
      Result:=true;
    end else begin
      Result:=false;
    end;
  end else begin
    Result:=true;
  end;
end;

function CompareLPChar(p1, p2: PChar; Max: integer): boolean;
begin
  if p1<>p2 then begin
    if (p1<>nil) and (p2<>nil) then begin
      while Max>0 do begin
        if (p1^=p2^) then begin
          if (p1^<>#0) then begin
            inc(p1);
            inc(p2);
            dec(Max);
          end else begin
            Result:=true;
            exit;
          end;
        end else begin
          Result:=false;
          exit;
        end;
      end;
      Result:=true;
    end else begin
      Result:=false;
    end;
  end else begin
    Result:=true;
  end;
end;

function CompareIPChar(p1, p2: PChar): boolean;
begin
  if p1<>p2 then begin
    if (p1<>nil) and (p2<>nil) then begin
      while true do begin
        if (p1^=p2^) or (upcase(p1^)=upcase(p2^)) then begin
          if p1^<>#0 then begin
            inc(p1);
            inc(p2);
          end else begin
            Result:=true;
            exit;
          end;
        end else begin
          Result:=false;
          exit;
        end;
      end;
      Result:=true;
    end else begin
      Result:=false;
    end;
  end else begin
    Result:=true;
  end;
end;

function CompareLIPChar(p1, p2: PChar; Max: integer): boolean;
begin
  if p1<>p2 then begin
    if (p1<>nil) and (p2<>nil) then begin
      while Max>0 do begin
        if (p1^=p2^) or (upcase(p1^)=upcase(p2^)) then begin
          if (p1^<>#0) then begin
            inc(p1);
            inc(p2);
            dec(Max);
          end else begin
            Result:=true;
            exit;
          end;
        end else begin
          Result:=false;
          exit;
        end;
      end;
      Result:=true;
    end else begin
      Result:=false;
    end;
  end else begin
    Result:=true;
  end;
end;


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

  { TXMLReader }

  TXMLReader = class
  protected
    buf, BufStart: PChar;
    Filename: String;
    function BufPosToLineCol(p: PChar): TPoint;
    function BufPosToStr(p: PChar): string;
    procedure RaiseExc(const descr: String);
    procedure RaiseCharNotFound(c : char);
    function  SkipWhitespace: Boolean;
    procedure ExpectWhitespace; inline;
    procedure ExpectChar(c: char); inline;
    procedure ExpectString(const s: String);
    function  CheckFor(s: PChar): Boolean;
    function  CheckForChar(c: Char): Boolean;
    procedure SkipString(const ValidChars: TSetOfChar);
    function  GetString(const ValidChars: TSetOfChar): String;
    function  GetString(BufPos: PChar; Len: integer): String;

    function  CheckName: Boolean;
    function  GetName(var s: String): Boolean;
    function  ExpectName: String;                                       // [5]
    procedure SkipName;
    procedure ExpectAttValue(attr: TDOMAttr);                           // [10]
    function  ExpectPubidLiteral: String;                               // [12]
    procedure SkipPubidLiteral;
    function  ParseComment(AOwner: TDOMNode): Boolean;                  // [15]
    function  ParsePI: Boolean;                                         // [16]
    procedure ExpectProlog;                                             // [22]
    function  ParseEq: Boolean;                                         // [25]
    procedure ExpectEq;
    procedure ParseMisc(AOwner: TDOMNode);                              // [27]
    function  ParseMarkupDecl: Boolean;                                 // [29]
    function  ParseCharData(AOwner: TDOMNode): Boolean;                 // [14]
    function  ParseCDSect(AOwner: TDOMNode): Boolean;                   // [18]
    function  ParseElement(AOwner: TDOMNode): Boolean;                  // [39]
    procedure ExpectElement(AOwner: TDOMNode);
    function  ParseReference(AOwner: TDOMNode): Boolean;                // [67]
    procedure ExpectReference(AOwner: TDOMNode);
    function  ParsePEReference: Boolean;                                // [69]
    function  ParseExternalID: Boolean;                                 // [75]
    procedure ExpectExternalID;
    function  ParseEncodingDecl: String;                                // [80]
    procedure SkipEncodingDecl;

    procedure ResolveEntities(RootNode: TDOMNode);
  public
    doc: TDOMDocument;
    procedure ProcessXML(ABuf: PChar; const AFilename: String);  // [1]
    procedure ProcessFragment(AOwner: TDOMNode; ABuf: PChar; const AFilename: String);
    procedure ProcessDTD(ABuf: PChar; const AFilename: String);  // ([29])
  end;

{ TXMLReaderDocument }

procedure TXMLReaderDocument.SetDocType(ADocType: TDOMDocumentType);
begin
  FDocType := ADocType;
end;


constructor TXMLReaderDocumentType.Create(ADocument: TXMLReaderDocument);
begin
  inherited Create(ADocument);
end;

function TXMLReader.BufPosToLineCol(p: PChar): TPoint;
var
  apos: PChar;
  x: Integer;
  y: Integer;
begin
  // find out the line in which the error occured
  apos := BufStart;
  x := 1;
  y := 1;
  while apos < p do begin
    if apos^ in [#10,#13] then begin
      Inc(y);
      x := 1;
      if (apos[1] in [#10,#13]) and (apos[0]<>apos[1]) then
        inc(apos);
    end else
      Inc(x);
    Inc(apos);
  end;
  Result.X:=X;
  Result.Y:=Y;
end;

function TXMLReader.BufPosToStr(p: PChar): string;
var
  LineCol: TPoint;
begin
  // find out the line in which the error occured
  LineCol:=BufPosToLineCol(BufStart);
  Result:=IntToStr(LineCol.y)+','+IntToStr(LineCol.x);
end;

procedure TXMLReader.RaiseExc(const descr: String);
var
  Err: EXMLReadError;
  LineCol: TPoint;
begin
  LineCol:=BufPosToLineCol(buf);
  Err:=EXMLReadError.Create(
    Filename+'('+IntToStr(LineCol.y)+','+IntToStr(LineCol.x)+') Error: ' + descr);
  Err.Position:=buf-BufStart;
  Err.LineCol:=LineCol;
  Err.Descr:=descr;
  raise Err;
end;

procedure TXMLReader.RaiseCharNotFound(c: char);
begin
  RaiseExc('Expected "' + c + '", found "' + buf^ + '"');
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

procedure TXMLReader.ExpectChar(c: char);
begin
  if buf^ <> c then
    RaiseCharNotFound(c);
  Inc(buf);
end;

procedure TXMLReader.ExpectString(const s: String);

  procedure RaiseStringNotFound;
  var
    s2: PChar;
    s3: String;
  begin
    GetMem(s2, Length(s) + 1);
    StrLCopy(s2, buf, Length(s));
    s3 := StrPas(s2);
    FreeMem(s2);
    RaiseExc('Expected "' + s + '", found "' + s3 + '"');
  end;

var
  i: Integer;
begin
  for i := 1 to Length(s) do
    if buf[i - 1] <> s[i] then begin
      RaiseStringNotFound;
    end;
  Inc(buf, Length(s));
end;

function TXMLReader.CheckFor(s: PChar): Boolean;
begin
  if buf[0] <> #0 then begin
    if (buf[0]=s[0]) and (CompareLPChar(buf, s, StrLen(s))) then begin
      Inc(buf, StrLen(s));
      Result := True;
    end else
      Result := False;
  end else begin
    Result := False;
  end;
end;

function TXMLReader.CheckForChar(c: Char): Boolean;
begin
  if (buf[0]=c) and (c<>#0) then begin
    inc(buf);
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

procedure TXMLReader.SkipString(const ValidChars: TSetOfChar);
begin
  while buf[0] in ValidChars do begin
    Inc(buf);
  end;
end;

function TXMLReader.GetString(const ValidChars: TSetOfChar): String;
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

procedure TXMLReader.ProcessXML(ABuf: PChar; const AFilename: String);    // [1]
begin
  buf := ABuf;
  BufStart := ABuf;
  Filename := AFilename;

  doc := TXMLReaderDocument.Create;
  ExpectProlog;
  {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('TXMLReader.ProcessXML A');{$ENDIF}
  ExpectElement(doc);
  {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('TXMLReader.ProcessXML B');{$ENDIF}
  ParseMisc(doc);

  // skip end of file characters
  while buf^=#26 do inc(buf);
  // check if whole document was read
  if buf[0] <> #0 then
    RaiseExc('Text after end of document element found');
end;

procedure TXMLReader.ProcessFragment(AOwner: TDOMNode; ABuf: PChar; const AFilename: String);
begin
  buf := ABuf;
  BufStart := ABuf;
  Filename := AFilename;

  // do not call SkipWhitespace. They are needed by ParseCharData.
  while ParseCharData(AOwner) or ParseCDSect(AOwner) or ParsePI or
    ParseComment(AOwner) or ParseElement(AOwner) or
    ParseReference(AOwner)
  do ;
end;

function TXMLReader.CheckName: Boolean;
var OldBuf: PChar;
begin
  if not (buf[0] in (Letter + ['_', ':'])) then begin
    Result := False;
    exit;
  end;

  OldBuf := buf;
  Inc(buf);
  SkipString(Letter + ['0'..'9', '.', '-', '_', ':']);
  buf := OldBuf;
  Result := True;
end;

function TXMLReader.GetName(var s: String): Boolean;    // [5]
var OldBuf: PChar;
begin
  if not (buf[0] in (Letter + ['_', ':'])) then begin
    SetLength(s, 0);
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

  procedure RaiseNameNotFound;
  begin
    RaiseExc('Expected letter, "_" or ":" for name, found "' + buf[0] + '"');
  end;

var OldBuf: PChar;
begin
  if not (buf[0] in (Letter + ['_', ':'])) then
    RaiseNameNotFound;

  OldBuf := buf;
  Inc(buf);
  SkipString(Letter + ['0'..'9', '.', '-', '_', ':']);
  Result:=GetString(OldBuf,buf-OldBuf);
end;

procedure TXMLReader.SkipName;

  procedure RaiseSkipNameNotFound;
  begin
    RaiseExc('Expected letter, "_" or ":" for name, found "' + buf[0] + '"');
  end;

begin
  if not (buf[0] in (Letter + ['_', ':'])) then
    RaiseSkipNameNotFound;

  Inc(buf);
  SkipString(Letter + ['0'..'9', '.', '-', '_', ':']);
end;

procedure TXMLReader.ExpectAttValue(attr: TDOMAttr);    // [10]
var
  OldBuf: PChar;

  procedure FlushStringBuffer;
  var
    s: String;
  begin
    if OldBuf<>buf then begin
      s := GetString(OldBuf,buf-OldBuf);
      OldBuf := buf;
      attr.AppendChild(doc.CreateTextNode(s));
      SetLength(s, 0);
    end;
  end;

var
  StrDel: char;
begin
  if (buf[0] <> '''') and (buf[0] <> '"') then
    RaiseExc('Expected quotation marks');
  StrDel:=buf[0];
  Inc(buf);
  OldBuf := buf;
  while (buf[0]<>StrDel) and (buf[0]<>#0) do begin
    if buf[0] <> '&' then begin
      Inc(buf);
    end else
    begin
      if OldBuf<>buf then FlushStringBuffer;
      ParseReference(attr);
      OldBuf := buf;
    end;
  end;
  if OldBuf<>buf then FlushStringBuffer;
  inc(buf);
  ResolveEntities(Attr);
end;

function TXMLReader.ExpectPubidLiteral: String;
begin
  SetLength(Result, 0);
  if CheckForChar('''') then begin
    SkipString(PubidChars - ['''']);
    ExpectChar('''');
  end else if CheckForChar('"') then begin
    SkipString(PubidChars - ['"']);
    ExpectChar('"');
  end else
    RaiseExc('Expected quotation marks');
end;

procedure TXMLReader.SkipPubidLiteral;
begin
  if CheckForChar('''') then begin
    SkipString(PubidChars - ['''']);
    ExpectChar('''');
  end else if CheckForChar('"') then begin
    SkipString(PubidChars - ['"']);
    ExpectChar('"');
  end else
    RaiseExc('Expected quotation marks');
end;

function TXMLReader.ParseComment(AOwner: TDOMNode): Boolean;    // [15]
var
  comment: String;
  OldBuf: PChar;
begin
  if CheckFor('<!--') then begin
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
begin
  if CheckFor('<?') then begin
    if CompareLIPChar(buf,'XML ',4) then
      RaiseExc('"<?xml" processing instruction not allowed here');
    SkipName;
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
    if doc.InheritsFrom(TXMLDocument) then
      TXMLDocument(doc).XMLVersion :=
      GetString(['a'..'z', 'A'..'Z', '0'..'9', '_', '.', ':', '-']);
  end;

  procedure ParseDoctypeDecls;
  begin
    repeat
      SkipWhitespace;
    until not (ParseMarkupDecl or ParsePEReference);
    ExpectChar(']');
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
      ExpectChar('''');
    end else if buf[0] = '"' then
    begin
      Inc(buf);
      ParseVersionNum;
      ExpectChar('"');
    end else
      RaiseExc('Expected single or double quotation mark');

    // EncodingDecl?
    SkipEncodingDecl;

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
    DocType := TXMLReaderDocumentType.Create(doc as TXMLReaderDocument);
    if doc.InheritsFrom(TXMLReaderDocument) then
      TXMLReaderDocument(doc).SetDocType(DocType);
    SkipWhitespace;
    DocType.Name := ExpectName;
    SkipWhitespace;
    if CheckForChar('[') then
    begin
      ParseDoctypeDecls;
      SkipWhitespace;
      ExpectChar('>');
    end else if not CheckForChar('>') then
    begin
      ParseExternalID;
      SkipWhitespace;
      if CheckForChar('[') then
      begin
        ParseDoctypeDecls;
        SkipWhitespace;
      end;
      ExpectChar('>');
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
        if CheckForChar('(') then
          ExpectChoiceOrSeq
        else
          SkipName;
        if CheckForChar('?') then
        else if CheckForChar('*') then
        else if CheckForChar('+') then;
      end;

    var
      delimiter: Char;
    begin
      SkipWhitespace;
      ExpectCP;
      SkipWhitespace;
      delimiter := #0;
      while not CheckForChar(')') do begin
        if delimiter = #0 then begin
          if (buf[0] = '|') or (buf[0] = ',') then
            delimiter := buf[0]
          else
            RaiseExc('Expected "|" or ","');
          Inc(buf);
        end else
          ExpectChar(delimiter);
        SkipWhitespace;
        ExpectCP;
      end;
    end;

  begin
    if CheckFor('<!ELEMENT') then begin
      ExpectWhitespace;
      SkipName;
      ExpectWhitespace;

      // Get contentspec [46]

      if CheckFor('EMPTY') then
      else if CheckFor('ANY') then
      else if CheckForChar('(') then begin
        SkipWhitespace;
        if CheckFor('#PCDATA') then begin
          // Parse Mixed section [51]
          SkipWhitespace;
          if not CheckForChar(')') then
            repeat
              ExpectChar('|');
              SkipWhitespace;
              SkipName;
            until CheckFor(')*');
        end else begin
          // Parse Children section [47]

          ExpectChoiceOrSeq;

          if CheckForChar('?') then
          else if CheckForChar('*') then
          else if CheckForChar('+') then;
        end;
      end else
        RaiseExc('Invalid content specification');

      SkipWhitespace;
      ExpectChar('>');
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
      SkipName;
      SkipWhitespace;
      while not CheckForChar('>') do begin
        SkipName;
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
          ExpectChar('(');
          SkipWhitespace;
          SkipName;
          SkipWhitespace;
          while not CheckForChar(')') do begin
            ExpectChar('|');
            SkipWhitespace;
            SkipName;
            SkipWhitespace;
          end;
        end else if CheckForChar('(') then begin    // [59]
          SkipWhitespace;
          SkipString(Nmtoken);
          SkipWhitespace;
          while not CheckForChar(')') do begin
            ExpectChar('|');
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
      strdel: Char;
    begin
      if (buf[0] <> '''') and (buf[0] <> '"') then begin
        Result := False;
        exit;
      end;
      strdel := buf[0];
      Inc(buf);
      while not CheckForChar(strdel) do
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
      if CheckForChar('%') then begin    // [72]
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
          SkipName;
        end;
      end;
      SkipWhitespace;
      ExpectChar('>');
      Result := True;
    end else
      Result := False;
  end;

  function ParseNotationDecl: Boolean;    // [82]
  begin
    if CheckFor('<!NOTATION') then begin
      ExpectWhitespace;
      SkipName;
      ExpectWhitespace;
      if ParseExternalID then
      else if CheckFor('PUBLIC') then begin    // [83]
        ExpectWhitespace;
        SkipPubidLiteral;
      end else
        RaiseExc('Expected external or public ID');
      SkipWhitespace;
      ExpectChar('>');
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

procedure TXMLReader.ProcessDTD(ABuf: PChar; const AFilename: String);
begin
  buf := ABuf;
  BufStart := ABuf;
  Filename := AFilename;

  doc := TXMLReaderDocument.Create;
  ParseMarkupDecl;

  {
  if buf[0] <> #0 then begin
    DebugLn('=== Unparsed: ===');
    //DebugLn(buf);
    DebugLn(StrLen(buf), ' chars');
  end;
  }
end;

function TXMLReader.ParseCharData(AOwner: TDOMNode): Boolean;    // [14]
var
  p: PChar;
  DataLen: integer;
  OldBuf: PChar;
begin
  OldBuf := buf;
  while not (buf[0] in [#0, '<', '&']) do
    begin
      Inc(buf);
    end;
  DataLen:=buf-OldBuf;
  if DataLen > 0 then
    begin
      // Check if chardata has non-whitespace content
      p:=OldBuf;
      while (p<buf) and (p[0] in WhitespaceChars) do
        inc(p);
      if p<buf then
        AOwner.AppendChild(doc.CreateTextNode(GetString(OldBuf,DataLen)));
      Result := True;
    end
  else
    Result := False;
end;

function TXMLReader.ParseCDSect(AOwner: TDOMNode): Boolean;    // [18]
var
  OldBuf: PChar;
begin
  if CheckFor('<![CDATA[') then
    begin
      OldBuf := buf;
      while not CheckFor(']]>') do
      begin
        Inc(buf);
      end;
      AOwner.AppendChild(doc.CreateCDATASection(GetString(OldBuf,buf-OldBuf-3))); { Copy CDATA, discarding terminator }
      Result := True;
    end
  else
    Result := False;
end;

function TXMLReader.ParseElement(AOwner: TDOMNode): Boolean;    // [39] [40] [44]
var
  NewElem: TDOMElement;

  procedure CreateNameElement;
  var
    IsEmpty: Boolean;
    attr: TDOMAttr;
    name: string;
    FoundName: String;
    StartPos: PChar;
  begin
    {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('  CreateNameElement A');{$ENDIF}
    StartPos:=buf;
    GetName(name);
    NewElem := doc.CreateElement(name);
    AOwner.AppendChild(NewElem);

    SkipWhitespace;
    IsEmpty := False;
    while True do
    begin
      {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('  CreateNameElement E');{$ENDIF}
      if CheckFor('/>') then
      begin
        IsEmpty := True;
        break;
      end;
      if CheckForChar('>') then
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
      while ParseCharData(NewElem) or ParseCDSect(NewElem) or ParsePI or
        ParseComment(NewElem) or ParseElement(NewElem) or
        ParseReference(NewElem) do;

      // Get ETag [42]
      ExpectString('</');
      FoundName:=ExpectName;
      if FoundName <> name then
        RaiseExc('Unmatching element end tag (expected "</' + name + '>", found "</'+FoundName+'>", start tag at '+BufPosToStr(StartPos)+')');
      SkipWhitespace;
      ExpectChar('>');
    end;

    {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('  CreateNameElement END');{$ENDIF}
    ResolveEntities(NewElem);
  end;

var
  OldBuf: PChar;
begin
  OldBuf := Buf;
  if CheckForChar('<') then
  begin
    {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('TXMLReader.ParseElement A');{$ENDIF}
    if not CheckName then
    begin
      Buf := OldBuf;
      Result := False;
    end else begin
      CreateNameElement;
      Result := True;
    end;
  end else
    Result := False;
  {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('TXMLReader.ParseElement END');{$ENDIF}
end;

procedure TXMLReader.ExpectElement(AOwner: TDOMNode);
begin
  if not ParseElement(AOwner) then
    RaiseExc('Expected element');
end;

function TXMLReader.ParsePEReference: Boolean;    // [69]
begin
  if CheckForChar('%') then begin
    SkipName;
    ExpectChar(';');
    Result := True;
  end else
    Result := False;
end;

function TXMLReader.ParseReference(AOwner: TDOMNode): Boolean;    // [67] [68]
begin
  if not CheckForChar('&') then begin
    Result := False;
    exit;
  end;
  if CheckForChar('#') then begin    // Test for CharRef [66]
    if CheckForChar('x') then begin
      // !!!: there must be at least one digit
      while buf[0] in ['0'..'9', 'a'..'f', 'A'..'F'] do Inc(buf);
    end else
      // !!!: there must be at least one digit
      while buf[0] in ['0'..'9'] do Inc(buf);
  end else
    AOwner.AppendChild(doc.CreateEntityReference(ExpectName));
  ExpectChar(';');
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
    if buf[0] = '''' then begin
      Inc(buf);
      OldBuf := buf;
      while (buf[0] <> '''') and (buf[0] <> #0) do begin
        Inc(buf);
      end;
      Result := GetString(OldBuf,buf-OldBuf);
      ExpectChar('''');
    end else if buf[0] = '"' then begin
      Inc(buf);
      OldBuf := buf;
      while (buf[0] <> '"') and (buf[0] <> #0) do begin
        Inc(buf);
      end;
      Result := GetString(OldBuf,buf-OldBuf);
      ExpectChar('"');
    end else
      Result:='';
  end;

  procedure SkipSystemLiteral;
  begin
    if buf[0] = '''' then begin
      Inc(buf);
      while (buf[0] <> '''') and (buf[0] <> #0) do begin
        Inc(buf);
      end;
      ExpectChar('''');
    end else if buf[0] = '"' then begin
      Inc(buf);
      while (buf[0] <> '"') and (buf[0] <> #0) do begin
        Inc(buf);
      end;
      ExpectChar('"');
    end;
  end;

begin
  if CheckFor('SYSTEM') then begin
    ExpectWhitespace;
    SkipSystemLiteral;
    Result := True;
  end else if CheckFor('PUBLIC') then begin
    ExpectWhitespace;
    SkipPubidLiteral;
    ExpectWhitespace;
    SkipSystemLiteral;
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
      ExpectChar('''');
    end else if buf[0] = '"' then begin
      Inc(buf);
      Result := ParseEncName;
      ExpectChar('"');
    end;
  end;
end;

procedure TXMLReader.SkipEncodingDecl;

  procedure ParseEncName;
  begin
    if not (buf[0] in ['A'..'Z', 'a'..'z']) then
      RaiseExc('Expected character (A-Z, a-z)');
    Inc(buf);
    SkipString(['A'..'Z', 'a'..'z', '0'..'9', '.', '_', '-']);
  end;

begin
  SkipWhitespace;
  if CheckFor('encoding') then begin
    ExpectEq;
    if buf[0] = '''' then begin
      Inc(buf);
      ParseEncName;
      ExpectChar('''');
    end else if buf[0] = '"' then begin
      Inc(buf);
      ParseEncName;
      ExpectChar('"');
    end;
  end;
end;


{ Currently, this method will only resolve the entities which are
  predefined in XML: }

procedure TXMLReader.ResolveEntities(RootNode: TDOMNode);
var
  Node, NextNode: TDOMNode;

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
        NextNode := NextSibling.NextSibling;
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

begin
  Node := RootNode.FirstChild;
  while Assigned(Node) do
  begin
    NextNode := Node.NextSibling;
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
    Node := NextNode;
  end;
end;



procedure ReadXMLFile(out ADoc: TXMLDocument; var f: File);
var
  reader: TXMLReader;
  buf: PChar;
  BufSize: LongInt;
begin
  ADoc := nil;
  BufSize := FileSize(f) + 1;
  if BufSize <= 1 then
    exit;

  GetMem(buf, BufSize);
  try
    BlockRead(f, buf^, BufSize - 1);
    buf[BufSize - 1] := #0;
    Reader := TXMLReader.Create;
    try
      Reader.ProcessXML(buf, TFileRec(f).name);
    finally
      ADoc := TXMLDocument(Reader.doc);
      Reader.Free;
    end;
  finally
    FreeMem(buf);
  end;
end;

procedure ReadXMLFile(out ADoc: TXMLDocument; f: TStream; const AFilename: String);
var
  reader: TXMLReader;
  buf: PChar;
begin
  ADoc := nil;
  if f.Size = 0 then exit;

  GetMem(buf, f.Size + 1);
  try
    f.Read(buf^, f.Size);
    buf[f.Size] := #0;
    Reader := TXMLReader.Create;
    try
      Reader.ProcessXML(buf, AFilename);
    finally
      ADoc := TXMLDocument(Reader.doc);
      Reader.Free;
    end;
  finally
    FreeMem(buf);
  end;
end;

procedure ReadXMLFile(out ADoc: TXMLDocument; f: TStream);
begin
  ReadXMLFile(ADoc, f, '<Stream>');
end;

procedure ReadXMLFile(out ADoc: TXMLDocument; const AFilename: String);
var
  FileStream: TFileStream;
  MemStream: TMemoryStream;
begin
  ADoc := nil;
  FileStream := TFileStream.Create(UTF8ToSys(AFilename), fmOpenRead or fmShareDenyWrite);
  if FileStream = nil then exit;
  MemStream := TMemoryStream.Create;
  try
    MemStream.LoadFromStream(FileStream);
    ReadXMLFile(ADoc, MemStream, AFilename);
  finally
    FileStream.Free;
    MemStream.Free;
  end;
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; var f: File);
var
  Reader: TXMLReader;
  buf: PChar;
  BufSize: LongInt;
begin
  BufSize := FileSize(f) + 1;
  if BufSize <= 1 then
    exit;

  GetMem(buf, BufSize);
  try
    BlockRead(f, buf^, BufSize - 1);
    buf[BufSize - 1] := #0;
    Reader := TXMLReader.Create;
    try
      Reader.Doc := AParentNode.OwnerDocument;
      Reader.ProcessFragment(AParentNode, buf, TFileRec(f).name);
    finally
      Reader.Free;
    end;
  finally
    FreeMem(buf);
  end;
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream; const AFilename: String);
var
  Reader: TXMLReader;
  buf: PChar;
begin
  if f.Size = 0 then
    exit;

  GetMem(buf, f.Size + 1);
  try
    f.Read(buf^, f.Size);
    buf[f.Size] := #0;
    Reader := TXMLReader.Create;
    Reader.Doc := AParentNode.OwnerDocument;
    try
      Reader.ProcessFragment(AParentNode, buf, AFilename);
    finally
      Reader.Free;
    end;
  finally
    FreeMem(buf);
  end;
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream);
begin
  ReadXMLFragment(AParentNode, f, '<Stream>');
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; const AFilename: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(UTF8ToSys(AFilename), fmOpenRead or fmShareDenyWrite);
  try
    ReadXMLFragment(AParentNode, Stream, AFilename);
  finally
    Stream.Free;
  end;
end;


procedure ReadDTDFile(var ADoc: TXMLDocument; var f: File);
var
  Reader: TXMLReader;
  buf: PChar;
  BufSize: LongInt;
begin
  ADoc := nil;
  BufSize := FileSize(f) + 1;
  if BufSize <= 1 then
    exit;

  GetMem(buf, BufSize);
  try
    BlockRead(f, buf^, BufSize - 1);
    buf[BufSize - 1] := #0;
    Reader := TXMLReader.Create;
    try
      Reader.ProcessDTD(buf, TFileRec(f).name);
      ADoc := TXMLDocument(Reader.doc);
    finally
      Reader.Free;
    end;
  finally
    FreeMem(buf);
  end;
end;

procedure ReadDTDFile(var ADoc: TXMLDocument; var f: TStream; const AFilename: String);
var
  Reader: TXMLReader;
  buf: PChar;
begin
  ADoc := nil;
  if f.Size = 0 then
    exit;

  GetMem(buf, f.Size + 1);
  try
    f.Read(buf^, f.Size);
    buf[f.Size] := #0;
    Reader := TXMLReader.Create;
    try
      Reader.ProcessDTD(buf, AFilename);
      ADoc := TXMLDocument(Reader.doc);
    finally
      Reader.Free;
    end;
  finally
    FreeMem(buf);
  end;
end;

procedure ReadDTDFile(var ADoc: TXMLDocument; var f: TStream);
begin
  ReadDTDFile(ADoc, f, '<Stream>');
end;

procedure ReadDTDFile(var ADoc: TXMLDocument; const AFilename: String);
var
  Stream: TStream;
begin
  ADoc := nil;
  Stream := TFileStream.Create(UTF8ToSys(AFilename), fmOpenRead or fmShareDenyWrite);
  try
    ReadDTDFile(ADoc, Stream, AFilename);
  finally
    Stream.Free;
  end;
end;


end.
