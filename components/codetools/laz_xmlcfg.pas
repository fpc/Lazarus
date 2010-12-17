{
    $Id$
    This file was part of the Free Component Library and was adapted to use UTF8
    strings instead of widestrings.

    Implementation of TXMLConfig class
    Copyright (c) 1999 - 2001 by Sebastian Guenther, sg@freepascal.org

    See the file COPYING.modifiedLGPL.txt, included in this distribution,
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

{$I codetools.inc}

interface

{off $DEFINE MEM_CHECK}


uses
  {$IFDEF MEM_CHECK}MemCheck,{$ENDIF}
  Classes, sysutils,
  {$IFDEF NewXMLCfg}
  Laz2_DOM, Laz2_XMLRead, Laz2_XMLWrite,
  {$ELSE}
  Laz_DOM, Laz_XMLRead, Laz_XMLWrite,
  {$ENDIF}
  FileProcs;

type

  {"APath" is the path and name of a value: A XML configuration file is
   hierachical. "/" is the path delimiter, the part after the last "/"
   is the name of the value. The path components will be mapped to XML
   elements, the name will be an element attribute.}

  { TXMLConfig }

  TXMLConfig = class(TComponent)
  private
    FFilename: String;
    {$IFDEF NewXMLCfg}
    FReadFlags: TXMLReaderFlags;
    {$ENDIF}
    procedure SetFilename(const AFilename: String);
  protected
    doc: TXMLDocument;
    FModified: Boolean;
    fDoNotLoadFromFile: boolean;
    fAutoLoadFromSource: string;
    fPathCache: string;
    fPathNodeCache: array of TDomNode; // starting with doc.DocumentElement, then first child node of first sub path
    procedure Loaded; override;
    function ExtendedToStr(const e: extended): string;
    function StrToExtended(const s: string; const ADefault: extended): extended;
    procedure ReadXMLFile(out ADoc: TXMLDocument; const AFilename: String); virtual;
    procedure WriteXMLFile(ADoc: TXMLDocument; const AFileName: String); virtual;
    procedure FreeDoc; virtual;
    procedure SetPathNodeCache(Index: integer; Node: TDomNode);
    function GetPathNodeCache(Index: integer): TDomNode;
    procedure InvalidateCacheTilEnd(StartIndex: integer);
    function InternalFindNode(const APath: String; PathLen: integer;
                              CreateNodes: boolean = false): TDomNode;
    procedure InternalCleanNode(Node: TDomNode);
  public
    constructor Create(const AFilename: String); overload; // create and load
    constructor CreateClean(const AFilename: String); // create new
    constructor CreateWithSource(const AFilename, Source: String); // create new and load from Source
    destructor Destroy; override;
    procedure Clear;
    procedure Flush;    // Writes the XML file
    procedure ReadFromStream(s: TStream);
    procedure WriteToStream(s: TStream);

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
    function FindNode(const APath: String; PathHasValue: boolean): TDomNode;
    function HasPath(const APath: string; PathHasValue: boolean): boolean; // checks if the path has values, set PathHasValue=true to skip the last part
    function HasChildPaths(const APath: string): boolean;
    property Modified: Boolean read FModified write FModified;
    procedure InvalidatePathCache;
  published
    property Filename: String read FFilename write SetFilename;
    property Document: TXMLDocument read doc;
    {$IFDEF NewXMLCfg}
    property ReadFlags: TXMLReaderFlags read FReadFlags write FReadFlags;
    {$ENDIF}
  end;


// ===================================================================

implementation

constructor TXMLConfig.Create(const AFilename: String);
begin
  //DebugLn(['TXMLConfig.Create ',AFilename]);
  {$IFDEF NewXMLCfg}
  FReadFlags:=[xrfAllowLowerThanInAttributeValue,xrfAllowSpecialCharsInAttributeValue];
  {$ENDIF}
  inherited Create(nil);
  SetFilename(AFilename);
end;

constructor TXMLConfig.CreateClean(const AFilename: String);
begin
  //DebugLn(['TXMLConfig.CreateClean ',AFilename]);
  fDoNotLoadFromFile:=true;
  Create(AFilename);
  FModified:=FileExistsCached(AFilename);
end;

constructor TXMLConfig.CreateWithSource(const AFilename, Source: String);
begin
  fAutoLoadFromSource:=Source;
  try
    CreateClean(AFilename);
  finally
    fAutoLoadFromSource:='';
  end;
end;

destructor TXMLConfig.Destroy;
begin
  if Assigned(doc) then
  begin
    Flush;
    FreeDoc;
  end;
  inherited Destroy;
end;

procedure TXMLConfig.Clear;
var
  cfg: TDOMElement;
begin
  // free old document
  FreeDoc;
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
  if Modified and (Filename<>'') then
  begin
    //DebugLn(['TXMLConfig.Flush ',Filename]);
    WriteXMLFile(doc, Filename);
    FModified := False;
  end;
end;

procedure TXMLConfig.ReadFromStream(s: TStream);
begin
  FreeDoc;
  {$IFDEF NewXMLCfg}
  Laz2_XMLRead.ReadXMLFile(Doc,s,ReadFlags);
  {$ELSE}
  Laz_XMLRead.ReadXMLFile(Doc,s);
  {$ENDIF}
  if Doc=nil then
    Clear;
end;

procedure TXMLConfig.WriteToStream(s: TStream);
begin
  {$IFDEF NewXMLCfg}
  Laz2_XMLWrite.WriteXMLFile(Doc,s);
  {$ELSE}
  Laz_XMLWrite.WriteXMLFile(Doc,s);
  {$ENDIF}
end;

function TXMLConfig.GetValue(const APath, ADefault: String): String;
var
  Node, Attr: TDOMNode;
  NodeName: String;
  StartPos: integer;
begin
  //CheckHeapWrtMemCnt('TXMLConfig.GetValue A '+APath);
  Result:=ADefault;

  StartPos:=length(APath)+1;
  while (StartPos>1) and (APath[StartPos-1]<>'/') do dec(StartPos);
  if StartPos>length(APath) then exit;
  Node:=InternalFindNode(APath,StartPos-1);
  if Node=nil then
    exit;
  //CheckHeapWrtMemCnt('TXMLConfig.GetValue E');
  NodeName:=copy(APath,StartPos,length(APath));
  //CheckHeapWrtMemCnt('TXMLConfig.GetValue G');
  Attr := Node.Attributes.GetNamedItem(NodeName);
  if Assigned(Attr) then
    Result := Attr.NodeValue;
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

  if CompareText(s,'TRUE')=0 then
    Result := True
  else if CompareText(s,'FALSE')=0 then
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
  Node: TDOMNode;
  NodeName: String;
  StartPos: integer;
begin
  StartPos:=length(APath)+1;
  while (StartPos>1) and (APath[StartPos-1]<>'/') do dec(StartPos);
  if StartPos>length(APath) then exit;
  Node:=InternalFindNode(APath,StartPos-1,true);
  if Node=nil then
    exit;
  NodeName:=copy(APath,StartPos,length(APath));
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
  Node: TDOMNode;
  ParentNode: TDOMNode;
begin
  Node:=InternalFindNode(APath,length(APath));
  if (Node=nil) or (Node.ParentNode=nil) then exit;
  ParentNode:=Node.ParentNode;
  ParentNode.RemoveChild(Node);
  FModified:=true;
  InvalidatePathCache;
  InternalCleanNode(ParentNode);
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
  if Assigned(TDOMElement(Node).GetAttributeNode(NodeName)) then begin
    TDOMElement(Node).RemoveAttribute(NodeName);
    FModified := True;
  end;
  InternalCleanNode(Node);
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
  PathLen: Integer;
begin
  PathLen:=length(APath);
  if PathHasValue then begin
    while (PathLen>0) and (APath[PathLen]<>'/') do dec(PathLen);
    while (PathLen>0) and (APath[PathLen]='/') do dec(PathLen);
  end;
  Result:=InternalFindNode(APath,PathLen);
end;

function TXMLConfig.HasPath(const APath: string; PathHasValue: boolean
  ): boolean;
begin
  Result:=FindNode(APath,PathHasValue)<>nil;
end;

function TXMLConfig.HasChildPaths(const APath: string): boolean;
var
  Node: TDOMNode;
begin
  Node:=FindNode(APath,false);
  Result:=(Node<>nil) and Node.HasChildNodes;
end;

procedure TXMLConfig.InvalidatePathCache;
begin
  fPathCache:='';
  InvalidateCacheTilEnd(0);
end;

function TXMLConfig.ExtendedToStr(const e: extended): string;
var
  OldDecimalSeparator: Char;
  OldThousandSeparator: Char;
begin
  OldDecimalSeparator:=DefaultFormatSettings.DecimalSeparator;
  OldThousandSeparator:=DefaultFormatSettings.ThousandSeparator;
  DefaultFormatSettings.DecimalSeparator:='.';
  DefaultFormatSettings.ThousandSeparator:=',';
  Result:=FloatToStr(e);
  DefaultFormatSettings.DecimalSeparator:=OldDecimalSeparator;
  DefaultFormatSettings.ThousandSeparator:=OldThousandSeparator;
end;

function TXMLConfig.StrToExtended(const s: string; const ADefault: extended): extended;
var
  OldDecimalSeparator: Char;
  OldThousandSeparator: Char;
begin
  OldDecimalSeparator:=DefaultFormatSettings.DecimalSeparator;
  OldThousandSeparator:=DefaultFormatSettings.ThousandSeparator;
  DefaultFormatSettings.DecimalSeparator:='.';
  DefaultFormatSettings.ThousandSeparator:=',';
  Result:=StrToFloatDef(s,ADefault);
  DefaultFormatSettings.DecimalSeparator:=OldDecimalSeparator;
  DefaultFormatSettings.ThousandSeparator:=OldThousandSeparator;
end;

procedure TXMLConfig.ReadXMLFile(out ADoc: TXMLDocument; const AFilename: String
  );
begin
  InvalidatePathCache;
  {$IFDEF NewXMLCfg}
  Laz2_XMLRead.ReadXMLFile(ADoc,AFilename,ReadFlags);
  {$ELSE}
  Laz_XMLRead.ReadXMLFile(ADoc,AFilename);
  {$ENDIF}
end;

procedure TXMLConfig.WriteXMLFile(ADoc: TXMLDocument; const AFileName: String);
begin
  {$IFDEF NewXMLCfg}
  Laz2_XMLWrite.WriteXMLFile(ADoc,AFileName);
  {$ELSE}
  Laz_XMLWrite.WriteXMLFile(ADoc,AFileName);
  {$ENDIF}
  InvalidateFileStateCache(AFileName);
end;

procedure TXMLConfig.FreeDoc;
begin
  InvalidatePathCache;
  FreeAndNil(doc);
end;

procedure TXMLConfig.SetPathNodeCache(Index: integer; Node: TDomNode);
var
  OldLength: Integer;
  i: LongInt;
  NewSize: Integer;
begin
  OldLength:=length(fPathNodeCache);
  if OldLength<=Index then begin
    NewSize:=OldLength*2+4;
    if NewSize<Index then NewSize:=Index;
    SetLength(fPathNodeCache,NewSize);
    for i:=OldLength to length(fPathNodeCache)-1 do
      fPathNodeCache[i]:=nil;
  end;
  fPathNodeCache[Index]:=Node;
end;

function TXMLConfig.GetPathNodeCache(Index: integer): TDomNode;
begin
  if Index<length(fPathNodeCache) then
    Result:=fPathNodeCache[Index]
  else
    Result:=nil;
end;

procedure TXMLConfig.InvalidateCacheTilEnd(StartIndex: integer);
var
  i: LongInt;
begin
  for i:=StartIndex to length(fPathNodeCache)-1 do begin
    if fPathNodeCache[i]=nil then break;
    fPathNodeCache[i]:=nil;
  end;
end;

function TXMLConfig.InternalFindNode(const APath: String; PathLen: integer;
  CreateNodes: boolean): TDomNode;
var
  NodePath: String;
  StartPos, EndPos: integer;
  PathIndex: Integer;
  Parent: TDOMNode;
  NameLen: Integer;
begin
  //debugln(['TXMLConfig.InternalFindNode APath="',copy(APath,1,PathLen),'" CreateNodes=',CreateNodes]);
  PathIndex:=0;
  Result:=GetPathNodeCache(PathIndex);
  if Result=nil then begin
    Result := TDOMElement(doc.FindNode('CONFIG'));
    SetPathNodeCache(PathIndex,Result);
  end;
  if PathLen=0 then exit;
  StartPos:=1;
  while (Result<>nil) do begin
    EndPos:=StartPos;
    while (EndPos<=PathLen) and (APath[EndPos]<>'/') do inc(EndPos);
    NameLen:=EndPos-StartPos;
    if NameLen=0 then break;
    inc(PathIndex);
    Parent:=Result;
    Result:=GetPathNodeCache(PathIndex);
    if (Result<>nil) and (length(Result.NodeName)=NameLen)
    and CompareMem(PChar(Result.NodeName),@APath[StartPos],NameLen) then begin
      // cache valid
    end else begin
      // different path => search
      InvalidateCacheTilEnd(PathIndex);
      NodePath:=copy(APath,StartPos,NameLen);
      Result:=Parent.FindNode(NodePath);
      if Result=nil then begin
        if not CreateNodes then exit;
        // create missing node
        Result := Doc.CreateElement(NodePath);
        Parent.AppendChild(Result);
        if EndPos>PathLen then exit;
      end;
      SetPathNodeCache(PathIndex,Result);
    end;
    StartPos:=EndPos+1;
    if StartPos>PathLen then exit;
  end;
  Result:=nil;
end;

procedure TXMLConfig.InternalCleanNode(Node: TDomNode);
var
  ParentNode: TDOMNode;
begin
  if (Node=nil) then exit;
  while (Node.FirstChild=nil) and (Node.ParentNode<>nil)
  and (Node.ParentNode.ParentNode<>nil) do begin
    if (Node is TDOMElement) and (not TDOMElement(Node).IsEmpty) then break;
    ParentNode:=Node.ParentNode;
    ParentNode.RemoveChild(Node);
    InvalidatePathCache;
    Node:=ParentNode;
    FModified := True;
  end;
end;

procedure TXMLConfig.SetFilename(const AFilename: String);
var
  cfg: TDOMElement;
  ms: TMemoryStream;
begin
  {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('TXMLConfig.SetFilename A '+AFilename);{$ENDIF}
  if FFilename = AFilename then exit;
  FFilename := AFilename;
  InvalidatePathCache;

  if csLoading in ComponentState then
    exit;

  if Assigned(doc) then
  begin
    Flush;
    FreeDoc;
  end;

  doc:=nil;
  //debugln(['TXMLConfig.SetFilename ',not fDoNotLoadFromFile,' ',FileExistsCached(Filename)]);
  if (not fDoNotLoadFromFile) and FileExistsCached(Filename) then
    ReadXMLFile(doc,Filename)
  else if fAutoLoadFromSource<>'' then begin
    ms:=TMemoryStream.Create;
    try
      ms.Write(fAutoLoadFromSource[1],length(fAutoLoadFromSource));
      ms.Position:=0;
      {$IFDEF NewXMLCfg}
      Laz2_XMLRead.ReadXMLFile(doc,ms,ReadFlags);
      {$ELSE}
      Laz_XMLRead.ReadXMLFile(doc,ms);
      {$ENDIF}
    finally
      ms.Free;
    end;
  end;

  if not Assigned(doc) then
    doc := TXMLDocument.Create;

  cfg :=TDOMElement(doc.FindNode('CONFIG'));
  //debugln(['TXMLConfig.SetFilename ',DbgSName(cfg)]);
  if not Assigned(cfg) then begin
    cfg := doc.CreateElement('CONFIG');
    doc.AppendChild(cfg);
  end;
  {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('TXMLConfig.SetFilename END');{$ENDIF}
end;


end.
