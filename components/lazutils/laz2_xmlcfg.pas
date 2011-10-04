{
    This file was part of the Free Component Library and was adapted to use UTF8
    strings instead of widestrings.

    Implementation of TXMLConfig class
    Copyright (c) 1999 - 2001 by Sebastian Guenther, sg@freepascal.org

    See the file COPYING.modifiedLGPL.txt, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

  TXMLConfig enables applications to use XML files for storing their
  configuration data
}

{$MODE objfpc}
{$H+}

unit Laz2_XMLCfg;

interface

{off $DEFINE MEM_CHECK}

uses
  {$IFDEF MEM_CHECK}MemCheck,{$ENDIF}
  Classes, sysutils, LazFileCache,
  {$IFNDEF OldXMLCfg}
  Laz2_DOM, Laz2_XMLRead, Laz2_XMLWrite,
  {$ELSE}
  Laz_DOM, Laz_XMLRead, Laz_XMLWrite,
  {$ENDIF}
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
    {$IFNDEF OldXMLCfg}
    FReadFlags: TXMLReaderFlags;
    FWriteFlags: TXMLWriterFlags;
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
    {$IFNDEF OldXMLCfg}
    property ReadFlags: TXMLReaderFlags read FReadFlags write FReadFlags;
    property WriteFlags: TXMLWriterFlags read FWriteFlags write FWriteFlags;
    {$ENDIF}
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

implementation

constructor TXMLConfig.Create(const AFilename: String);
begin
  //DebugLn(['TXMLConfig.Create ',AFilename]);
  {$IFNDEF OldXMLCfg}
  // for compatibility with old TXMLConfig, which wrote #13 as #13, not as &xD;
  FReadFlags:=[xrfAllowLowerThanInAttributeValue,xrfAllowSpecialCharsInAttributeValue];
  FWriteFlags:=[xwfSpecialCharsInAttributeValue];
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
    {$IFNDEF OldXMLCfg}
    Laz2_XMLWrite.WriteXMLFile(Doc,Filename,WriteFlags);
    {$ELSE}
    Laz_XMLWrite.WriteXMLFile(Doc,Filename);
    {$ENDIF}
    InvalidateFileStateCache;
    FModified := False;
  end;
end;

procedure TXMLConfig.ReadFromStream(s: TStream);
begin
  FreeDoc;
  {$IFNDEF OldXMLCfg}
  Laz2_XMLRead.ReadXMLFile(Doc,s,ReadFlags);
  {$ELSE}
  Laz_XMLRead.ReadXMLFile(Doc,s);
  {$ENDIF}
  if Doc=nil then
    Clear;
end;

procedure TXMLConfig.WriteToStream(s: TStream);
begin
  {$IFNDEF OldXMLCfg}
  Laz2_XMLWrite.WriteXMLFile(Doc,s,WriteFlags);
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

procedure TXMLConfig.ReadXMLFile(out ADoc: TXMLDocument; const AFilename: String);
begin
  InvalidatePathCache;
  {$IFNDEF OldXMLCfg}
  Laz2_XMLRead.ReadXMLFile(ADoc,AFilename,ReadFlags);
  {$ELSE}
  Laz_XMLRead.ReadXMLFile(ADoc,AFilename);
  {$ENDIF}
end;

procedure TXMLConfig.WriteXMLFile(ADoc: TXMLDocument; const AFileName: String);
begin
  {$IFNDEF OldXMLCfg}
  Laz2_XMLWrite.WriteXMLFile(ADoc,AFileName,WriteFlags);
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
  //debugln(['TXMLConfig.SetFilename Load=',not fDoNotLoadFromFile,' FileExists=',FileExistsCached(Filename),' File=',Filename]);
  if (not fDoNotLoadFromFile) and FileExistsCached(Filename) then
    {$IFNDEF OldXMLCfg}
    Laz2_XMLRead.ReadXMLFile(doc,Filename,ReadFlags)
    {$ELSE}
    Laz_XMLRead.ReadXMLFile(doc,Filename)
    {$ENDIF}
  else if fAutoLoadFromSource<>'' then begin
    ms:=TMemoryStream.Create;
    try
      ms.Write(fAutoLoadFromSource[1],length(fAutoLoadFromSource));
      ms.Position:=0;
      {$IFNDEF OldXMLCfg}
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
  //debugln(['TXMLConfig.SetFilename cfg=',DbgSName(cfg),' doc=',DbgSName(doc)]);
  if not Assigned(cfg) then begin
    cfg := doc.CreateElement('CONFIG');
    doc.AppendChild(cfg);
  end;
  {$IFDEF MEM_CHECK}CheckHeapWrtMemCnt('TXMLConfig.SetFilename END');{$ENDIF}
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
        if (DefInstance <> nil)  and (Value = DefValue) then
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
        if (DefInstance <> nil)  and (DefFloatValue = FloatValue) then
          DeleteValue(Path)
        else
          SetValue(Path, FloatToStr(FloatValue));
      end;
    tkSString, tkLString, tkAString:
      begin
        StrValue := GetStrProp(Instance, PropInfo);
        if (DefInstance <> nil) then
           DefStrValue := GetStrProp(DefInstance, PropInfo);
        if (DefInstance <> nil)  and (DefStrValue = StrValue) then
          DeleteValue(Path)
        else
          SetValue(Path, StrValue);
      end;
(*    tkWString:
      begin
        WStrValue := GetWideStrProp(Instance, PropInfo);
        if (DefInstance <> nil) then
           WDefStrValue := GetWideStrProp(DefInstance, PropInfo);
        if (DefInstance <> nil)  and (WDefStrValue = WStrValue) then
          DeleteValue(Path)
        else
          SetValue(Path, WStrValue);
      end;*)
(*    tkInt64, tkQWord:
      begin
        Int64Value := GetInt64Prop(Instance, PropInfo);
        if (DefInstance <> nil) then
          DefInt64Value := GetInt64Prop(DefInstance, PropInfo)
        if (DefInstance <> nil) and (Int64Value = DefInt64Value) then
          DeleteValue(Path, Path)
        else
          SetValue(StrValue);
      end;*)
    tkBool:
      begin
        BoolValue := GetOrdProp(Instance, PropInfo)<>0;
        if (DefInstance <> nil) then
          DefBoolValue := GetOrdProp(DefInstance, PropInfo)<>0;
        if (DefInstance <> nil) and (BoolValue = DefBoolValue) then
          DeleteValue(Path)
        else
          SetValue(Path, BoolValue);
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
