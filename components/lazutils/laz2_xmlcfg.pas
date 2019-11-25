{
 **********************************************************************
  This file is part of LazUtils.
  It is copied from Free Component Library and adapted to use
  UTF8 strings instead of widestrings.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 **********************************************************************

  Implementation of TXMLConfig class
  Copyright (c) 1999 - 2001 by Sebastian Guenther, sg@freepascal.org

  TXMLConfig enables applications to use XML files for storing their
  configuration data
}

{$MODE objfpc}
{$modeswitch advancedrecords}
{$H+}

unit Laz2_XMLCfg;

interface

{off $DEFINE MEM_CHECK}

uses
  {$IFDEF MEM_CHECK}MemCheck,{$ENDIF}
  Classes, sysutils, LazFileCache,
  Laz2_DOM, Laz2_XMLRead, Laz2_XMLWrite, LazUtilities,
  typinfo;

type

  {"APath" is the path and name of a value: A XML configuration file is
   hierachical. "/" is the path delimiter, the part after the last "/"
   is the name of the value. The path components will be mapped to XML
   elements, the name will be an element attribute.}

  { TXMLConfig }

  TXMLConfig = class(TComponent)
  private
    FFilename: String;
    FReadFlags: TXMLReaderFlags;
    FWriteFlags: TXMLWriterFlags;
    FPointSettings: TFormatSettings;
    procedure CreateConfigNode;
    procedure InitFormatSettings;
    procedure SetFilename(const AFilename: String);
  protected
    type
      TDomNodeArray = array of TDomNode;
      TNodeCache = record
        Node: TDomNode;
        NodeSearchName: string;
        ChildrenValid: boolean;
        Children: TDomNodeArray; // child nodes with NodeName<>'' and sorted

        NodeListName: string;
        NodeList: TDomNodeArray; // child nodes that are accessed with "name[?]" XPath

      public
        class procedure GrowArray(var aArray: TDomNodeArray; aCount: Integer); static;
        procedure RefreshChildren;
        procedure RefreshChildrenIfNeeded;
        procedure RefreshNodeList(const ANodeName: string);
        procedure RefreshNodeListIfNeeded(const ANodeName: string);
        function AddNodeToList: TDOMNode;
      end;
  protected
    doc: TXMLDocument;
    FModified: Boolean;
    fDoNotLoadFromFile: boolean;
    fAutoLoadFromSource: string;
    fPathCache: string;
    fPathNodeCache: array of TNodeCache; // starting with doc.DocumentElement, then first child node of first sub path
    procedure Loaded; override;
    function ExtendedToStr(const e: extended): string;
    function StrToExtended(const s: string; const ADefault: extended): extended;
    procedure ReadXMLFile(out ADoc: TXMLDocument; const AFilename: String); virtual;
    procedure WriteXMLFile(ADoc: TXMLDocument; const AFileName: String); virtual;
    procedure FreeDoc; virtual;
    procedure SetPathNodeCache(Index: integer; aNode: TDomNode; aNodeSearchName: string = '');
    function GetCachedPathNode(Index: integer): TDomNode; inline;
    function GetCachedPathNode(Index: integer; out aNodeSearchName: string): TDomNode; inline;
    procedure InvalidateCacheTilEnd(StartIndex: integer);
    function InternalFindNode(const APath: String; PathLen: integer;
                              CreateNodes: boolean = false): TDomNode;
    procedure InternalCleanNode(Node: TDomNode);
    function FindChildNode(PathIndex: integer; const aName: string;
      CreateNodes: boolean = false): TDomNode;
  public
    constructor Create(AOwner: TComponent); override; overload;
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
    procedure GetValue(const APath: String; out ARect: TRect;
                       const ADefault: TRect);
    procedure SetDeleteValue(const APath: String; const AValue, DefValue: TRect);
    procedure SetExtendedValue(const APath: String; const AValue: extended);
    procedure SetDeleteExtendedValue(const APath: String;
                                     const AValue, DefValue: extended);
    procedure DeletePath(const APath: string);
    procedure DeleteValue(const APath: string);
    function FindNode(const APath: String; PathHasValue: boolean): TDomNode;
    // checks if the path has values, set PathHasValue=true to skip the last part
    function HasPath(const APath: string; PathHasValue: boolean): boolean;
    function HasChildPaths(const APath: string): boolean;
    function GetChildCount(const APath: string): Integer;
    function IsLegacyList(const APath: string): Boolean;
    function GetListItemCount(const APath, AItemName: string; const aLegacyList: Boolean): Integer;
    class function GetListItemXPath(const AName: string; const AIndex: Integer; const aLegacyList: Boolean;
      const aLegacyList1Based: Boolean = False): string;
    procedure SetListItemCount(const APath: string; const ACount: Integer; const ALegacyList: Boolean);
    property Modified: Boolean read FModified write FModified;
    procedure InvalidatePathCache;
  published
    property Filename: String read FFilename write SetFilename;
    property Document: TXMLDocument read doc;
    property ReadFlags: TXMLReaderFlags read FReadFlags write FReadFlags;
    property WriteFlags: TXMLWriterFlags read FWriteFlags write FWriteFlags;
  end;

  { TRttiXMLConfig }

  TRttiXMLConfig = class(TXMLConfig)
  protected
    procedure WriteProperty(Path: String; Instance: TPersistent;
                            PropInfo: Pointer; DefInstance: TPersistent = nil;
                            OnlyProperty: String= '');
    procedure ReadProperty(Path: String; Instance: TPersistent;
                            PropInfo: Pointer; DefInstance: TPersistent = nil;
                            OnlyProperty: String= '');
  public
    procedure WriteObject(Path: String; Obj: TPersistent;
                          DefObject: TPersistent= nil; OnlyProperty: String= '');
    procedure ReadObject(Path: String; Obj: TPersistent;
                          DefObject: TPersistent= nil; OnlyProperty: String= '');
  end;


// ===================================================================

function CompareDomNodeNames(DOMNode1, DOMNode2: Pointer): integer;

implementation

function CompareDomNodeNames(DOMNode1, DOMNode2: Pointer): integer;
var
  Node1: TDOMNode absolute DomNode1;
  Node2: TDOMNode absolute DomNode2;
begin
  Result:=CompareStr(Node1.NodeName,Node2.NodeName);
end;

{ TXMLConfig.TNodeCache }

function TXMLConfig.TNodeCache.AddNodeToList: TDOMNode;
begin
  Result:=Node.OwnerDocument.CreateElement(NodeListName);
  Node.AppendChild(Result);
  SetLength(NodeList, Length(NodeList)+1);
  NodeList[High(NodeList)]:=Result;
end;

class procedure TXMLConfig.TNodeCache.GrowArray(var aArray: TDomNodeArray;
  aCount: Integer);
var
  cCount: Integer;
begin
  cCount:=length(aArray);
  if aCount>cCount then begin
    if cCount<8 then
      cCount:=8
    else
      cCount:=cCount*2;
    if aCount>cCount then
      cCount := aCount;
    SetLength(aArray,cCount);
  end;
end;

procedure TXMLConfig.TNodeCache.RefreshChildren;
var
  aCount, m: Integer;
  aChild: TDOMNode;
begin
  // collect all children and sort
  aCount:=0;
  aChild:=Node.FirstChild;
  while aChild<>nil do begin
    if aChild.NodeName<>'' then begin
      GrowArray(Children, aCount+1);
      Children[aCount]:=aChild;
      inc(aCount);
    end;
    aChild:=aChild.NextSibling;
  end;
  SetLength(Children,aCount);
  if aCount>1 then
    MergeSortWithLen(@Children[0],aCount,@CompareDomNodeNames); // sort ascending [0]<[1]
  for m:=0 to aCount-2 do
    if Children[m].NodeName=Children[m+1].NodeName then begin
      // duplicate found: nodes with same name
      // -> use only the first
      Children[m+1]:=Children[m];
    end;
  ChildrenValid:=true;
end;

procedure TXMLConfig.TNodeCache.RefreshChildrenIfNeeded;
begin
  if not ChildrenValid then
    RefreshChildren;
end;

procedure TXMLConfig.TNodeCache.RefreshNodeList(const ANodeName: string);
var
  aCount: Integer;
  aChild: TDOMNode;
begin
  aCount:=0;
  aChild:=Node.FirstChild;
  while aChild<>nil do
  begin
    if aChild.NodeName=ANodeName then
    begin
      GrowArray(NodeList, aCount+1);
      NodeList[aCount]:=aChild;
      inc(aCount);
    end;
    aChild:=aChild.NextSibling;
  end;
  SetLength(NodeList,aCount);
  NodeListName := ANodeName;
end;

procedure TXMLConfig.TNodeCache.RefreshNodeListIfNeeded(const ANodeName: string
  );
begin
  if NodeListName<>ANodeName then
    RefreshNodeList(ANodeName);
end;

// inline
function TXMLConfig.GetCachedPathNode(Index: integer; out
  aNodeSearchName: string): TDomNode;
begin
  if Index<length(fPathNodeCache) then
  begin
    Result:=fPathNodeCache[Index].Node;
    aNodeSearchName:=fPathNodeCache[Index].NodeSearchName;
  end else
  begin
    Result:=nil;
    aNodeSearchName:='';
  end;
end;

function TXMLConfig.GetChildCount(const APath: string): Integer;
var
  Node: TDOMNode;
begin
  Node:=FindNode(APath,false);
  if Node=nil then
    Result := 0
  else
    Result := Node.GetChildCount;
end;

constructor TXMLConfig.Create(const AFilename: String);
begin
  //DebugLn(['TXMLConfig.Create ',AFilename]);
  Create(nil);
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
    WriteXMLFile(Doc,Filename);
    FModified := False;
  end;
end;

procedure TXMLConfig.ReadFromStream(s: TStream);
begin
  FreeDoc;
  Laz2_XMLRead.ReadXMLFile(Doc,s,ReadFlags);
  if Doc=nil then
    Clear;
end;

procedure TXMLConfig.WriteToStream(s: TStream);
begin
  if Doc=nil then
    CreateConfigNode;
  Laz2_XMLWrite.WriteXMLFile(Doc,s,WriteFlags);
end;

function TXMLConfig.GetValue(const APath, ADefault: String): String;
var
  Node, Attr: TDOMNode;
  NodeName: String;
  StartPos: integer;
begin
  //CheckHeapWrtMemCnt('TXMLConfig.GetValue A '+APath);
  Result:=ADefault;

  // skip root
  StartPos:=length(APath)+1;
  while (StartPos>1) and (APath[StartPos-1]<>'/') do dec(StartPos);
  if StartPos>length(APath) then exit;
  // find sub node
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
  Result := StrToIntDef(GetValue(APath, ''),ADefault);
end;

procedure TXMLConfig.GetValue(const APath: String; out ARect: TRect;
  const ADefault: TRect);
begin
  ARect.Left:=GetValue(APath+'Left',ADefault.Left);
  ARect.Top:=GetValue(APath+'Top',ADefault.Top);
  ARect.Right:=GetValue(APath+'Right',ADefault.Right);
  ARect.Bottom:=GetValue(APath+'Bottom',ADefault.Bottom);
end;

function TXMLConfig.GetValue(const APath: String; ADefault: Boolean): Boolean;
var
  s: String;
begin
  s := GetValue(APath, '');

  if SameText(s, 'True') then
    Result := True
  else if SameText(s, 'False') then
    Result := False
  else
    Result := ADefault;
end;

function TXMLConfig.GetExtendedValue(const APath: String;
  const ADefault: extended): extended;
begin
  Result:=StrToExtended(GetValue(APath,''),ADefault);
end;

function TXMLConfig.GetListItemCount(const APath, AItemName: string;
  const aLegacyList: Boolean): Integer;
var
  Node: TDOMNode;
  NodeLevel: SizeInt;
begin
  if aLegacyList then
    Result := GetValue(APath+'Count',0)
  else
  begin
    Node:=InternalFindNode(APath,Length(APath));
    if Node<>nil then
    begin
      NodeLevel := Node.GetLevel-1;
      fPathNodeCache[NodeLevel].RefreshNodeListIfNeeded(AItemName);
      Result := Length(fPathNodeCache[NodeLevel].NodeList);
    end else
      Result := 0;
  end;
end;

class function TXMLConfig.GetListItemXPath(const AName: string;
  const AIndex: Integer; const aLegacyList: Boolean;
  const aLegacyList1Based: Boolean): string;
begin
  if ALegacyList then
  begin
    if aLegacyList1Based then
      Result := AName+IntToStr(AIndex+1)
    else
      Result := AName+IntToStr(AIndex);
  end else
    Result := AName+'['+IntToStr(AIndex+1)+']';
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
  if Doc=nil then
    CreateConfigNode;
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

procedure TXMLConfig.SetDeleteValue(const APath: String; const AValue,
  DefValue: TRect);
begin
  SetDeleteValue(APath+'Left',AValue.Left,DefValue.Left);
  SetDeleteValue(APath+'Top',AValue.Top,DefValue.Top);
  SetDeleteValue(APath+'Right',AValue.Right,DefValue.Right);
  SetDeleteValue(APath+'Bottom',AValue.Bottom,DefValue.Bottom);
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

function TXMLConfig.FindNode(const APath: String; PathHasValue: boolean): TDomNode;
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

function TXMLConfig.HasPath(const APath: string; PathHasValue: boolean): boolean;
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

function TXMLConfig.IsLegacyList(const APath: string): Boolean;
begin
  Result := GetValue(APath+'Count',-1)>=0;
end;

function TXMLConfig.ExtendedToStr(const e: extended): string;
begin
  Result := FloatToStr(e, FPointSettings);
end;

function TXMLConfig.StrToExtended(const s: string; const ADefault: extended): extended;
begin
  Result := StrToFloatDef(s, ADefault, FPointSettings);
end;

procedure TXMLConfig.ReadXMLFile(out ADoc: TXMLDocument; const AFilename: String);
begin
  InvalidatePathCache;
  Laz2_XMLRead.ReadXMLFile(ADoc,AFilename,ReadFlags);
end;

procedure TXMLConfig.WriteXMLFile(ADoc: TXMLDocument; const AFileName: String);
begin
  Laz2_XMLWrite.WriteXMLFile(ADoc,AFileName,WriteFlags);
  InvalidateFileStateCache(AFileName);
end;

procedure TXMLConfig.FreeDoc;
begin
  InvalidatePathCache;
  FreeAndNil(doc);
end;

function TXMLConfig.GetCachedPathNode(Index: integer): TDomNode;
var
  x: string;
begin
  Result := GetCachedPathNode(Index, x);
end;

procedure TXMLConfig.SetPathNodeCache(Index: integer; aNode: TDomNode;
  aNodeSearchName: string);
var
  OldLength, NewLength: Integer;
begin
  OldLength:=length(fPathNodeCache);
  if OldLength<=Index then begin
    if OldLength<8 then
      NewLength:=8
    else
      NewLength:=OldLength*2;
    if NewLength<Index then NewLength:=Index;
    SetLength(fPathNodeCache,NewLength);
    FillByte(fPathNodeCache[OldLength],SizeOf(TNodeCache)*(NewLength-OldLength),0);
  end else if fPathNodeCache[Index].Node=aNode then
    exit
  else
    InvalidateCacheTilEnd(Index+1);
  if aNodeSearchName='' then
    aNodeSearchName:=aNode.NodeName;
  with fPathNodeCache[Index] do begin
    Node:=aNode;
    NodeSearchName:=aNodeSearchName;
    ChildrenValid:=false;
    NodeListName:='';
  end;
end;

procedure TXMLConfig.InvalidateCacheTilEnd(StartIndex: integer);
var
  i: LongInt;
begin
  for i:=StartIndex to length(fPathNodeCache)-1 do begin
    with fPathNodeCache[i] do begin
      if Node=nil then break;
      Node:=nil;
      ChildrenValid:=false;
      NodeListName:='';
    end;
  end;
end;

function TXMLConfig.InternalFindNode(const APath: String; PathLen: integer;
  CreateNodes: boolean): TDomNode;
var
  NodePath, NdName: String;
  StartPos, EndPos: integer;
  PathIndex: Integer;
  NameLen: Integer;
begin
  //debugln(['TXMLConfig.InternalFindNode APath="',copy(APath,1,PathLen),'" CreateNodes=',CreateNodes]);
  PathIndex:=0;
  Result:=GetCachedPathNode(PathIndex);
  if (Result=nil) and (doc<>nil) then begin
    Result:=TDOMElement(doc.FindNode('CONFIG'));
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
    Result:=GetCachedPathNode(PathIndex,NdName);
    if (Result=nil) or (length(NdName)<>NameLen)
    or not CompareMem(PChar(NdName),@APath[StartPos],NameLen) then begin
      // different path => search
      NodePath:=copy(APath,StartPos,NameLen);
      Result:=FindChildNode(PathIndex-1,NodePath,CreateNodes);
      if Result=nil then
        Exit;
      SetPathNodeCache(PathIndex,Result,NodePath);
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

function TXMLConfig.FindChildNode(PathIndex: integer; const aName: string;
  CreateNodes: boolean): TDomNode;
var
  l, r, m: Integer;
  cmp, BrPos: Integer;
  NodeName: string;
begin
  BrPos := Pos('[', aName);
  if (Length(aName)>=BrPos+2) and (aName[Length(aName)]=']')
  and TryStrToInt(Trim(Copy(aName, BrPos+1, Length(aName)-BrPos-1)), m) then
  begin
    // support XPath in format "name[?]"
    NodeName := Trim(Copy(aName, 1, BrPos-1));
    fPathNodeCache[PathIndex].RefreshNodeListIfNeeded(NodeName);
    if m<=0 then
      raise Exception.CreateFmt('Invalid node index in XPath descriptor "%s".', [aName])
    else if (m<=Length(fPathNodeCache[PathIndex].NodeList)) then
      Result:=fPathNodeCache[PathIndex].NodeList[m-1]
    else if CreateNodes then
    begin
      for l := Length(fPathNodeCache[PathIndex].NodeList)+1 to m do
        Result := fPathNodeCache[PathIndex].AddNodeToList;
      InvalidateCacheTilEnd(PathIndex+1);
    end;
  end else
  begin
    fPathNodeCache[PathIndex].RefreshChildrenIfNeeded;

    // binary search
    l:=0;
    r:=length(fPathNodeCache[PathIndex].Children)-1;
    while l<=r do begin
      m:=(l+r) shr 1;
      cmp:=CompareStr(aName,fPathNodeCache[PathIndex].Children[m].NodeName);
      if cmp<0 then
        r:=m-1
      else if cmp>0 then
        l:=m+1
      else
        exit(fPathNodeCache[PathIndex].Children[m]);
    end;
    if CreateNodes then
    begin
      // create missing node
      Result:=Doc.CreateElement(aName);
      fPathNodeCache[PathIndex].Node.AppendChild(Result);
      fPathNodeCache[PathIndex].ChildrenValid:=false;
      InvalidateCacheTilEnd(PathIndex+1);
    end else
      Result:=nil;
  end;
end;

constructor TXMLConfig.Create(AOwner: TComponent);
begin
  // for compatibility with old TXMLConfig, which wrote #13 as #13, not as &xD;
  FReadFlags:=[xrfAllowLowerThanInAttributeValue,xrfAllowSpecialCharsInAttributeValue];
  FWriteFlags:=[xwfSpecialCharsInAttributeValue];
  inherited Create(AOwner);
  InitFormatSettings;
end;

procedure TXMLConfig.SetFilename(const AFilename: String);
var
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
  //debugln(['TXMLConfig.SetFilename Load=',not fDoNotLoadFromFile,' FileExists=',FileExistsCached(Filename),' File=',Filename]);
  if (not fDoNotLoadFromFile) and FileExistsCached(Filename) then
    Laz2_XMLRead.ReadXMLFile(doc,Filename,ReadFlags)
  else if fAutoLoadFromSource<>'' then begin
    ms:=TMemoryStream.Create;
    try
      ms.Write(fAutoLoadFromSource[1],length(fAutoLoadFromSource));
      ms.Position:=0;
      Laz2_XMLRead.ReadXMLFile(doc,ms,ReadFlags);
    finally
      ms.Free;
    end;
  end;

  CreateConfigNode;
  {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('TXMLConfig.SetFilename END');{$ENDIF}
end;

procedure TXMLConfig.SetListItemCount(const APath: string;
  const ACount: Integer; const ALegacyList: Boolean);
begin
  if ALegacyList then
    SetDeleteValue(APath+'Count',ACount,0)
end;

procedure TXMLConfig.CreateConfigNode;
var
  cfg: TDOMElement;
begin
  if not Assigned(doc) then
    doc := TXMLDocument.Create;

  cfg :=TDOMElement(doc.FindNode('CONFIG'));
  if not Assigned(cfg) then begin
    cfg := doc.CreateElement('CONFIG');
    doc.AppendChild(cfg);
  end;
end;

procedure TXMLConfig.InitFormatSettings;
begin
  FPointSettings := DefaultFormatSettings;
  FPointSettings.DecimalSeparator := '.';
  FPointSettings.ThousandSeparator := ',';
end;

{ TRttiXMLConfig }

procedure TRttiXMLConfig.WriteObject(Path: String; Obj: TPersistent;
  DefObject: TPersistent; OnlyProperty: String = '');
var
  PropCount,i : integer;
  PropList  : PPropList;
begin
  PropCount:=GetPropList(Obj,PropList);
  if PropCount>0 then begin
    try
      for i := 0 to PropCount-1 do
        WriteProperty(Path, Obj, PropList^[i], DefObject, OnlyProperty);
    finally
      Freemem(PropList);
    end;
  end;
end;

// based on FPC TWriter
procedure TRttiXMLConfig.WriteProperty(Path: String; Instance: TPersistent;
  PropInfo: Pointer; DefInstance: TPersistent; OnlyProperty: String= '');
type
  tset = set of 0..31;
var
  i: Integer;
  PropType: PTypeInfo;
  Value, DefValue: LongInt;
  Ident: String;
  IntToIdentFn: TIntToIdent;
  SetType: Pointer;
  FloatValue, DefFloatValue: Extended;
  //WStrValue, WDefStrValue: WideString;
  StrValue, DefStrValue: String;
  //Int64Value, DefInt64Value: Int64;
  BoolValue, DefBoolValue: boolean;
  obj: TObject;

begin
  // do not stream properties without getter and setter
  if not (Assigned(PPropInfo(PropInfo)^.GetProc) and
          Assigned(PPropInfo(PropInfo)^.SetProc)) then
    exit;

  PropType := PPropInfo(PropInfo)^.PropType;
  Path := Path + PPropInfo(PropInfo)^.Name;
  if (OnlyProperty <> '') and (OnlyProperty <> PPropInfo(PropInfo)^.Name) then
    exit;

  case PropType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkSet, tkWChar:
      begin
        Value := GetOrdProp(Instance, PropInfo);
        if (DefInstance <> nil) then
          DefValue := GetOrdProp(DefInstance, PropInfo);
        if ((DefInstance <> nil)  and (Value = DefValue)) or
           ((DefInstance =  nil)  and (not IsStoredProp(Instance, PropInfo)))
        then
          DeleteValue(Path)
        else begin
          case PropType^.Kind of
            tkInteger:
              begin                      // Check if this integer has a string identifier
                IntToIdentFn := FindIntToIdent(PPropInfo(PropInfo)^.PropType);
                if Assigned(IntToIdentFn) and IntToIdentFn(Value, Ident{%H-}) then
                  SetValue(Path, Ident) // Integer can be written a human-readable identifier
                else
                  SetValue(Path, Value); // Integer has to be written just as number
              end;
            tkChar:
              SetValue(Path, Chr(Value));
            tkWChar:
              SetValue(Path, Value);
            tkSet:
              begin
                SetType := GetTypeData(PropType)^.CompType;
                Ident := '';
                for i := 0 to 31 do
                  if (i in tset(Value)) then begin
                    if Ident <> '' then Ident := Ident + ',';
                    Ident := Ident + GetEnumName(PTypeInfo(SetType), i);
                  end;
                SetValue(Path, Ident);
              end;
            tkEnumeration:
              SetValue(Path, GetEnumName(PropType, Value));
          end;
        end;
      end;
    tkFloat:
      begin
        FloatValue := GetFloatProp(Instance, PropInfo);
        if (DefInstance <> nil) then
         DefFloatValue := GetFloatProp(DefInstance, PropInfo);
        if ((DefInstance <> nil)  and (DefFloatValue = FloatValue)) or
           ((DefInstance =  nil)  and (not IsStoredProp(Instance, PropInfo)))
        then
          DeleteValue(Path)
        else
          SetValue(Path, FloatToStr(FloatValue));
      end;
    tkSString, tkLString, tkAString:
      begin
        StrValue := GetStrProp(Instance, PropInfo);
        if (DefInstance <> nil) then
           DefStrValue := GetStrProp(DefInstance, PropInfo);
        if ((DefInstance <> nil)  and (DefStrValue = StrValue)) or
           ((DefInstance =  nil)  and (not IsStoredProp(Instance, PropInfo)))
        then
          DeleteValue(Path)
        else
          SetValue(Path, StrValue);
      end;
(*    tkWString:
      begin
        WStrValue := GetWideStrProp(Instance, PropInfo);
        if (DefInstance <> nil) then
           WDefStrValue := GetWideStrProp(DefInstance, PropInfo);
        if ((DefInstance <> nil)  and (WDefStrValue = WStrValue)) or
           ((DefInstance =  nil)  and (not IsStoredProp(Instance, PropInfo)))
        then
          DeleteValue(Path)
        else
          SetValue(Path, WStrValue);
      end;*)
(*    tkInt64, tkQWord:
      begin
        Int64Value := GetInt64Prop(Instance, PropInfo);
        if (DefInstance <> nil) then
          DefInt64Value := GetInt64Prop(DefInstance, PropInfo)
        if ((DefInstance <> nil) and (Int64Value = DefInt64Value)) or
           ((DefInstance =  nil)  and (not IsStoredProp(Instance, PropInfo)))
        then
          DeleteValue(Path, Path)
        else
          SetValue(StrValue);
      end;*)
    tkBool:
      begin
        BoolValue := GetOrdProp(Instance, PropInfo)<>0;
        if (DefInstance <> nil) then
          DefBoolValue := GetOrdProp(DefInstance, PropInfo)<>0;
        if ((DefInstance <> nil) and (BoolValue = DefBoolValue)) or
           ((DefInstance =  nil)  and (not IsStoredProp(Instance, PropInfo)))
        then
          DeleteValue(Path)
        else
          SetValue(Path, BoolValue);
      end;
    tkClass:
      begin
        obj := GetObjectProp(Instance, PropInfo);
        if (obj is TPersistent) and IsStoredProp(Instance, PropInfo) then
          WriteObject(Path+'/', TPersistent(obj))
        else
          DeleteValue(Path);
      end;
  end;
end;

procedure TRttiXMLConfig.ReadProperty(Path: String; Instance: TPersistent; PropInfo: Pointer;
  DefInstance: TPersistent; OnlyProperty: String);
type
  tset = set of 0..31;
var
  i, j: Integer;
  PropType: PTypeInfo;
  Value, DefValue: LongInt;
  Ident, s: String;
  IdentToIntFn: TIdentToInt;
  SetType: Pointer;
  FloatValue, DefFloatValue: Extended;
  //WStrValue, WDefStrValue: WideString;
  StrValue, DefStrValue: String;
  //Int64Value, DefInt64Value: Int64;
  BoolValue, DefBoolValue: boolean;
  obj: TObject;

begin
  // do not stream properties without getter and setter
  if not (Assigned(PPropInfo(PropInfo)^.GetProc) and
          Assigned(PPropInfo(PropInfo)^.SetProc)) then
    exit;

  PropType := PPropInfo(PropInfo)^.PropType;
  Path := Path + PPropInfo(PropInfo)^.Name;
  if (OnlyProperty <> '') and (OnlyProperty <> PPropInfo(PropInfo)^.Name) then
    exit;
  if DefInstance = nil then
    DefInstance := Instance;

  case PropType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkSet, tkWChar:
      begin
        DefValue := GetOrdProp(DefInstance, PropInfo);
        case PropType^.Kind of
          tkInteger:
            begin                      // Check if this integer has a string identifier
              Ident := GetValue(Path, IntToStr(DefValue));
              IdentToIntFn := FindIdentToInt(PPropInfo(PropInfo)^.PropType);
              if TryStrToInt(Ident, Value) then
                SetOrdProp(Instance, PropInfo, Value)
              else if Assigned(IdentToIntFn) and IdentToIntFn(Ident, Value) then
                SetOrdProp(Instance, PropInfo, Value)
              else
                SetOrdProp(Instance, PropInfo, DefValue)
            end;
          tkChar:
            begin
              Ident := GetValue(Path, chr(DefValue));
              if Length(Ident) > 0 then
                SetOrdProp(Instance, PropInfo, ord(Ident[1]))
              else
                SetOrdProp(Instance, PropInfo, DefValue);
            end;
          tkWChar:
            SetOrdProp(Instance, PropInfo, GetValue(Path, DefValue));
          tkSet:
            begin
              SetType := GetTypeData(PropType)^.CompType;
              Ident := GetValue(Path, '-');
              If Ident = '-' then
                Value := DefValue
              else begin
                Value := 0;
                while length(Ident) > 0 do begin
                  i := Pos(',', Ident);
                  if i < 1 then
                    i := length(Ident) + 1;
                  s := copy(Ident, 1, i-1);
                  Ident := copy(Ident, i+1, length(Ident));
                  j := GetEnumValue(PTypeInfo(SetType), s);
                  if j <> -1 then
                    include(tset(Value), j)
                  else Begin
                    Value := DefValue;
                    break;
                  end;
                end;
              end;
              SetOrdProp(Instance, PropInfo, Value);
            end;
          tkEnumeration:
            begin
              Ident := GetValue(Path, '-');
              If Ident = '-' then
                Value := DefValue
              else
                Value := GetEnumValue(PropType, Ident);
              if Value <> -1 then
                SetOrdProp(Instance, PropInfo, Value)
              else
                SetOrdProp(Instance, PropInfo, DefValue);
            end;
        end;
      end;
    tkFloat:
      begin
        DefFloatValue := GetFloatProp(DefInstance, PropInfo);
        Ident := GetValue(Path, FloatToStr(DefFloatValue));
        if TryStrToFloat(Ident, FloatValue) then
          SetFloatProp(Instance, PropInfo, FloatValue)
        else
          SetFloatProp(Instance, PropInfo, DefFloatValue)
      end;
    tkSString, tkLString, tkAString:
      begin
        DefStrValue := GetStrProp(DefInstance, PropInfo);
        StrValue := GetValue(Path, DefStrValue);
        SetStrProp(Instance, PropInfo, StrValue)
      end;
(*    tkWString:
      begin
      end;*)
(*    tkInt64, tkQWord:
      begin
      end;*)
    tkBool:
      begin
        DefBoolValue := GetOrdProp(DefInstance, PropInfo) <> 0;
        BoolValue := GetValue(Path, DefBoolValue);
        SetOrdProp(Instance, PropInfo, ord(BoolValue));
      end;
    tkClass:
      begin
        obj := GetObjectProp(Instance, PropInfo);
        if (obj is TPersistent) and HasPath(Path, False) then
          ReadObject(Path+'/', TPersistent(obj));
      end;
  end;
end;

procedure TRttiXMLConfig.ReadObject(Path: String; Obj: TPersistent; DefObject: TPersistent;
  OnlyProperty: String);
var
  PropCount,i : integer;
  PropList  : PPropList;
begin
  PropCount:=GetPropList(Obj,PropList);
  if PropCount>0 then begin
    try
      for i := 0 to PropCount-1 do
        ReadProperty(Path, Obj, PropList^[i], DefObject, OnlyProperty);
    finally
      Freemem(PropList);
    end;
  end;
end;

end.
