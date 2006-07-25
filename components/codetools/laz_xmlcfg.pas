{
  BEWARE !!!
  This is a TEMPORARY file.
  As soon as it is moved to the fcl, it will be removed.
}

{
    $Id$
    This file is part of the Free Component Library

    Implementation of TXMLConfig class
    Copyright (c) 1999 - 2001 by Sebastian Guenther, sg@freepascal.org

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  TXMLConfig enables applications to use XML files for storing their
  configuration data
}

{$MODE objfpc}
{$H+}

unit Laz_XMLCfg;

interface

{off $DEFINE MEM_CHECK}

uses
  {$IFDEF MEM_CHECK}MemCheck,{$ENDIF}
  Classes, Laz_DOM, Laz_XMLRead, Laz_XMLWrite;

type

  {"APath" is the path and name of a value: A XML configuration file is
   hierachical. "/" is the path delimiter, the part after the last "/"
   is the name of the value. The path components will be mapped to XML
   elements, the name will be an element attribute.}

  TXMLConfig = class(TComponent)
  private
    FFilename: String;
    procedure SetFilename(const AFilename: String);
  protected
    doc: TXMLDocument;
    FModified: Boolean;
    fDoNotLoad: boolean;
    procedure Loaded; override;
    function FindNode(const APath: String; PathHasValue: boolean): TDomNode;
    function ExtendedToStr(const e: extended): string;
    function StrToExtended(const s: string; const ADefault: extended): extended;
  public
    constructor Create(const AFilename: String); overload;
    constructor CreateClean(const AFilename: String);
    destructor Destroy; override;
    procedure Clear;
    procedure Flush;    // Writes the XML file
    function  GetValue(const APath, ADefault: String): String;
    function  GetValue(const APath: String; ADefault: Integer): Integer;
    function  GetValue(const APath: String; ADefault: Boolean): Boolean;
    function  GetExtendedValue(const APath: String;
                               const ADefault: extended): extended;
    procedure SetValue(const APath, AValue: String);
    procedure SetDeleteValue(const APath, AValue, DefValue: String);
    procedure SetValue(const APath: String; AValue: Integer);
    procedure SetDeleteValue(const APath: String; AValue, DefValue: Integer);
    procedure SetValue(const APath: String; AValue: Boolean);
    procedure SetDeleteValue(const APath: String; AValue, DefValue: Boolean);
    procedure SetExtendedValue(const APath: String; const AValue: extended);
    procedure SetDeleteExtendedValue(const APath: String;
                                     const AValue, DefValue: extended);
    procedure DeletePath(const APath: string);
    procedure DeleteValue(const APath: string);
    property Modified: Boolean read FModified write FModified;
  published
    property Filename: String read FFilename write SetFilename;
    property Document: TXMLDocument read doc;
  end;


// ===================================================================

implementation

uses SysUtils;


constructor TXMLConfig.Create(const AFilename: String);
begin
  inherited Create(nil);
  SetFilename(AFilename);
end;

constructor TXMLConfig.CreateClean(const AFilename: String);
begin
  inherited Create(nil);
  fDoNotLoad:=true;
  SetFilename(AFilename);
end;

destructor TXMLConfig.Destroy;
begin
  if Assigned(doc) then
  begin
    Flush;
    doc.Free;
  end;
  inherited Destroy;
end;

procedure TXMLConfig.Clear;
var
  cfg: TDOMElement;
begin
  // free old document
  doc.Free;
  // create new document
  doc := TXMLDocument.Create;
  cfg :=TDOMElement(doc.FindNode('CONFIG'));
  if not Assigned(cfg) then begin
    cfg := doc.CreateElement('CONFIG');
    doc.AppendChild(cfg);
  end;
end;

procedure TXMLConfig.Flush;
begin
  if Modified then
  begin
    WriteXMLFile(doc, Filename);
    FModified := False;
  end;
end;

function TXMLConfig.GetValue(const APath, ADefault: String): String;
var
  Node, Child, Attr: TDOMNode;
  NodeName: String;
  PathLen: integer;
  StartPos, EndPos: integer;
begin
  //CheckHeapWrtMemCnt('TXMLConfig.GetValue A '+APath);
  Result:=ADefault;
  PathLen:=length(APath);
  Node := doc.DocumentElement;
  StartPos:=1;
  while True do begin
    EndPos:=StartPos;
    while (EndPos<=PathLen) and (APath[EndPos]<>'/') do inc(EndPos);
    if EndPos>PathLen then break;
    if EndPos>StartPos then begin
      NodeName:='';
      SetLength(NodeName,EndPos-StartPos);
      //UniqueString(NodeName);
      Move(APath[StartPos],NodeName[1],EndPos-StartPos);
      Child := Node.FindNode(NodeName);
      //writeln('TXMLConfig.GetValue C NodeName="',NodeName,'" ',
      //  PCardinal(Cardinal(NodeName)-8)^,' ',PCardinal(Cardinal(NodeName)-4)^);
      //CheckHeapWrtMemCnt('TXMLConfig.GetValue B2');
      if not Assigned(Child) then exit;
      Node := Child;
    end;
    StartPos:=EndPos+1;
    //CheckHeapWrtMemCnt('TXMLConfig.GetValue D');
  end;
  if StartPos>PathLen then exit;
  //CheckHeapWrtMemCnt('TXMLConfig.GetValue E');
  NodeName:='';
  SetLength(NodeName,PathLen-StartPos+1);
  //CheckHeapWrtMemCnt('TXMLConfig.GetValue F '+IntToStr(length(NodeName))+' '+IntToStr(StartPos)+' '+IntToStr(length(APath))+' '+APath[StartPos]);
  //UniqueString(NodeName);
  Move(APath[StartPos],NodeName[1],length(NodeName));
  //CheckHeapWrtMemCnt('TXMLConfig.GetValue G');
  //writeln('TXMLConfig.GetValue G2 NodeName="',NodeName,'"');
  Attr := Node.Attributes.GetNamedItem(NodeName);
  if Assigned(Attr) then
    Result := Attr.NodeValue;
  //CheckHeapWrtMemCnt('TXMLConfig.GetValue H');
  //writeln('TXMLConfig.GetValue END Result="',Result,'"');
end;

function TXMLConfig.GetValue(const APath: String; ADefault: Integer): Integer;
begin
  Result := StrToIntDef(GetValue(APath, IntToStr(ADefault)),ADefault);
end;

function TXMLConfig.GetValue(const APath: String; ADefault: Boolean): Boolean;
var
  s: String;
begin
  if ADefault then
    s := 'True'
  else
    s := 'False';

  s := GetValue(APath, s);

  if AnsiCompareText(s,'TRUE')=0 then
    Result := True
  else if AnsiCompareText(s,'FALSE')=0 then
    Result := False
  else
    Result := ADefault;
end;

function TXMLConfig.GetExtendedValue(const APath: String;
  const ADefault: extended): extended;
begin
  Result:=StrToExtended(GetValue(APath,ExtendedToStr(ADefault)),ADefault);
end;

procedure TXMLConfig.SetValue(const APath, AValue: String);
var
  Node, Child: TDOMNode;
  NodeName: String;
  PathLen: integer;
  StartPos, EndPos: integer;
begin
  Node := Doc.DocumentElement;
  PathLen:=length(APath);
  StartPos:=1;
  while True do begin
    EndPos:=StartPos;
    while (EndPos<=PathLen) and (APath[EndPos]<>'/') do inc(EndPos);
    if EndPos>PathLen then break;
    SetLength(NodeName,EndPos-StartPos);
    Move(APath[StartPos],NodeName[1],EndPos-StartPos);
    StartPos:=EndPos+1;
    Child := Node.FindNode(NodeName);
    if not Assigned(Child) then
    begin
      Child := Doc.CreateElement(NodeName);
      Node.AppendChild(Child);
    end;
    Node := Child;
  end;

  if StartPos>PathLen then exit;
  SetLength(NodeName,PathLen-StartPos+1);
  Move(APath[StartPos],NodeName[1],length(NodeName));
  if (not Assigned(TDOMElement(Node).GetAttributeNode(NodeName))) or
    (TDOMElement(Node)[NodeName] <> AValue) then
  begin
    TDOMElement(Node)[NodeName] := AValue;
    FModified := True;
  end;
end;

procedure TXMLConfig.SetDeleteValue(const APath, AValue, DefValue: String);
begin
  if AValue=DefValue then
    DeleteValue(APath)
  else
    SetValue(APath,AValue);
end;

procedure TXMLConfig.SetValue(const APath: String; AValue: Integer);
begin
  SetValue(APath, IntToStr(AValue));
end;

procedure TXMLConfig.SetDeleteValue(const APath: String; AValue,
  DefValue: Integer);
begin
  if AValue=DefValue then
    DeleteValue(APath)
  else
    SetValue(APath,AValue);
end;

procedure TXMLConfig.SetValue(const APath: String; AValue: Boolean);
begin
  if AValue then
    SetValue(APath, 'True')
  else
    SetValue(APath, 'False');
end;

procedure TXMLConfig.SetDeleteValue(const APath: String; AValue,
  DefValue: Boolean);
begin
  if AValue=DefValue then
    DeleteValue(APath)
  else
    SetValue(APath,AValue);
end;

procedure TXMLConfig.SetExtendedValue(const APath: String;
  const AValue: extended);
begin
  SetValue(APath,ExtendedToStr(AValue));
end;

procedure TXMLConfig.SetDeleteExtendedValue(const APath: String; const AValue,
  DefValue: extended);
begin
  if AValue=DefValue then
    DeleteValue(APath)
  else
    SetExtendedValue(APath,AValue);
end;

procedure TXMLConfig.DeletePath(const APath: string);
var
  Node: TDomNode;
begin
  Node:=FindNode(APath,false);
  if (Node=nil) or (Node.ParentNode=nil) then exit;
  Node.ParentNode.RemoveChild(Node);
  FModified := True;
end;

procedure TXMLConfig.DeleteValue(const APath: string);
var
  Node: TDomNode;
  StartPos: integer;
  NodeName: string;
  ParentNode: TDOMNode;
begin
  Node:=FindNode(APath,true);
  if (Node=nil) then exit;
  StartPos:=length(APath);
  while (StartPos>0) and (APath[StartPos]<>'/') do dec(StartPos);
  NodeName:=copy(APath,StartPos+1,length(APath)-StartPos);
  if Assigned(TDOMElement(Node).GetAttributeNode(NodeName)) then begin
    TDOMElement(Node).RemoveAttribute(NodeName);
    FModified := True;
  end;
  while (Node.FirstChild=nil) and (Node.ParentNode<>nil)
  and (Node.ParentNode.ParentNode<>nil) do begin
    if (Node is TDOMElement) and (not TDOMElement(Node).IsEmpty) then break;
    ParentNode:=Node.ParentNode;
    //writeln('TXMLConfig.DeleteValue APath="',APath,'" NodeName=',Node.NodeName,' ',Node.ClassName);
    ParentNode.RemoveChild(Node);
    Node:=ParentNode;
    FModified := True;
  end;
end;

procedure TXMLConfig.Loaded;
begin
  inherited Loaded;
  if Length(Filename) > 0 then
    SetFilename(Filename);              // Load the XML config file
end;

function TXMLConfig.FindNode(const APath: String;
  PathHasValue: boolean): TDomNode;
var
  NodePath: String;
  StartPos, EndPos: integer;
  PathLen: integer;
begin
  Result := doc.DocumentElement;
  PathLen:=length(APath);
  StartPos:=1;
  while (Result<>nil) do begin
    EndPos:=StartPos;
    while (EndPos<=PathLen) and (APath[EndPos]<>'/') do inc(EndPos);
    if (EndPos>PathLen) and PathHasValue then exit;
    if EndPos=StartPos then break;
    SetLength(NodePath,EndPos-StartPos);
    Move(APath[StartPos],NodePath[1],length(NodePath));
    Result := Result.FindNode(NodePath);
    StartPos:=EndPos+1;
    if StartPos>PathLen then exit;
  end;
  Result:=nil;
end;

function TXMLConfig.ExtendedToStr(const e: extended): string;
var
  OldDecimalSeparator: Char;
  OldThousandSeparator: Char;
begin
  OldDecimalSeparator:=DecimalSeparator;
  OldThousandSeparator:=ThousandSeparator;
  DecimalSeparator:='.';
  ThousandSeparator:=',';
  Result:=FloatToStr(e);
  DecimalSeparator:=OldDecimalSeparator;
  ThousandSeparator:=OldThousandSeparator;
end;

function TXMLConfig.StrToExtended(const s: string; const ADefault: extended
  ): extended;
var
  OldDecimalSeparator: Char;
  OldThousandSeparator: Char;
begin
  OldDecimalSeparator:=DecimalSeparator;
  OldThousandSeparator:=ThousandSeparator;
  DecimalSeparator:='.';
  ThousandSeparator:=',';
  Result:=StrToFloatDef(s,ADefault);
  DecimalSeparator:=OldDecimalSeparator;
  ThousandSeparator:=OldThousandSeparator;
end;

procedure TXMLConfig.SetFilename(const AFilename: String);
var
  cfg: TDOMElement;
begin
  {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('TXMLConfig.SetFilename A '+AFilename);{$ENDIF}
  if FFilename = AFilename then exit;
  FFilename := AFilename;

  if csLoading in ComponentState then
    exit;

  if Assigned(doc) then
  begin
    Flush;
    doc.Free;
  end;

  doc:=nil;
  if FileExists(AFilename) and (not fDoNotLoad) then
    ReadXMLFile(doc,AFilename);

  if not Assigned(doc) then
    doc := TXMLDocument.Create;

  cfg :=TDOMElement(doc.FindNode('CONFIG'));
  if not Assigned(cfg) then begin
    cfg := doc.CreateElement('CONFIG');
    doc.AppendChild(cfg);
  end;
  {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('TXMLConfig.SetFilename END');{$ENDIF}
end;


end.
