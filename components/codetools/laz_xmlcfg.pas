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

    See the file COPYING.FPC, included in this distribution,
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
    procedure Loaded; override;
    function FindNode(const APath: String; PathHasValue: boolean): TDomNode;
  public
    constructor Create(const AFilename: String); overload;
    destructor Destroy; override;
    procedure Clear;
    procedure Flush;    // Writes the XML file
    function  GetValue(const APath, ADefault: String): String;
    function  GetValue(const APath: String; ADefault: Integer): Integer;
    function  GetValue(const APath: String; ADefault: Boolean): Boolean;
    procedure SetValue(const APath, AValue: String);
    procedure SetDeleteValue(const APath, AValue, DefValue: String);
    procedure SetValue(const APath: String; AValue: Integer);
    procedure SetDeleteValue(const APath: String; AValue, DefValue: Integer);
    procedure SetValue(const APath: String; AValue: Boolean);
    procedure SetDeleteValue(const APath: String; AValue, DefValue: Boolean);
    procedure DeletePath(const APath: string);
    procedure DeleteValue(const APath: string);
    property Modified: Boolean read FModified;
  published
    property Filename: String read FFilename write SetFilename;
  end;


// ===================================================================

implementation

uses SysUtils;


constructor TXMLConfig.Create(const AFilename: String);
begin
  //writeln('TXMLConfig.Create ',AFilename);
  inherited Create(nil);
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
  Result:=ADefault;
  PathLen:=length(APath);
  Node := doc.DocumentElement;
  StartPos:=1;
  while True do begin
    EndPos:=StartPos;
    while (EndPos<=PathLen) and (APath[EndPos]<>'/') do inc(EndPos);
    if EndPos>PathLen then break;
    SetLength(NodeName,EndPos-StartPos);
    Move(APath[StartPos],NodeName[1],EndPos-StartPos);
    StartPos:=EndPos+1;
    Child := Node.FindNode(NodeName);
    if not Assigned(Child) then exit;
    Node := Child;
  end;
  if StartPos>PathLen then exit;
  SetLength(NodeName,PathLen-StartPos+1);
  Move(APath[StartPos],NodeName[1],length(NodeName));
  Attr := Node.Attributes.GetNamedItem(NodeName);
  if Assigned(Attr) then
    Result := Attr.NodeValue;
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
begin
  Node:=FindNode(APath,true);
  if (Node=nil) then exit;
  StartPos:=length(APath);
  while (StartPos>0) and (APath[StartPos]<>'/') do dec(StartPos);
  NodeName:=copy(APath,StartPos+1,length(APath)-StartPos);
  if (not Assigned(TDOMElement(Node).GetAttributeNode(NodeName))) then exit;
  TDOMElement(Node).RemoveAttribute(NodeName);
  FModified := True;
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
  if FileExists(AFilename) then
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
{
  $Log$
  Revision 1.8  2002/12/28 11:29:47  mattias
  xmlcfg deletion, focus fixes

  Revision 1.7  2002/10/22 08:48:04  lazarus
  MG: fixed segfault on loading xmlfile

  Revision 1.6  2002/10/09 12:40:25  lazarus
  MG: reduced exceptions on file not found

  Revision 1.5  2002/10/01 09:09:07  lazarus
  MG: added clear and deletepath

  Revision 1.4  2002/09/20 09:27:47  lazarus
  MG: accelerated xml

  Revision 1.3  2002/09/13 16:58:27  lazarus
  MG: removed the 1x1 bitmap from TBitBtn

  Revision 1.2  2002/07/30 14:36:28  lazarus
  MG: accelerated xmlread and xmlwrite

  Revision 1.1  2002/07/30 06:24:06  lazarus
  MG: added a faster version of TXMLConfig

  Revision 1.4  2001/04/10 23:22:05  peter
    * merged fixes

  Revision 1.3  2000/07/29 14:52:24  sg
  * Modified the copyright notice to remove ambiguities

  Revision 1.2  2000/07/13 11:33:07  michael
  + removed logs

}
