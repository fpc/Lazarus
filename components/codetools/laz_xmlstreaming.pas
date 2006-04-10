{
    This file is part of the Free Component Library

    XML serialisation driver
    Copyright (c) 2000 by Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


unit Laz_XMLStreaming;

{$MODE objfpc}
{$H+}

interface

uses SysUtils, Classes, TypInfo, FileProcs, Laz_DOM, Laz_XMLWrite;

type

  TXMLObjectWriterStackElType = (elUnknown, elPropertyList, elChildrenList);

  TXMLObjectWriterStackEl = class
  public
    Element, Parent: TDOMElement;
    ElType: TXMLObjectWriterStackElType;
    CurName: String;
  end;

  { TXMLObjectWriter }

  TXMLObjectWriter = class(TAbstractObjectWriter)
  private
    FDoc: TDOMDocument;
    FRootEl: TDOMElement;
    FStack: TList;
    StackEl: TXMLObjectWriterStackEl;
    procedure StackPush;
    procedure StackPop;
    function GetPropertyElement(const TypeName: String): TDOMElement;
  public
    constructor Create(ADoc: TDOMDocument; const APath: string; Append: Boolean);

    { Begin/End markers. Those ones who don't have an end indicator, use
      "EndList", after the occurrence named in the comment. Note that this
      only counts for "EndList" calls on the same level; each BeginXXX call
      increases the current level. }
    procedure BeginCollection; override;{ Ends with the next "EndList" }
    procedure BeginComponent(Component: TComponent; Flags: TFilerFlags;
      ChildPos: Integer); override;{ Ends after the second "EndList" }
    procedure BeginList; override;
    procedure EndList; override;
    procedure BeginProperty(const PropName: String); override;
    procedure EndProperty; override;

    procedure WriteBinary(const Buffer; Count: Longint); override;
    procedure WriteBoolean(Value: Boolean); override;
    // procedure WriteChar(Value: Char);
    procedure WriteFloat(const Value: Extended); override;
    procedure WriteSingle(const Value: Single); override;
    procedure WriteCurrency(const Value: Currency); override;
    procedure WriteDate(const Value: TDateTime); override;
    procedure WriteIdent(const Ident: string); override;
    procedure WriteInteger(Value: Int64); override;
    procedure WriteMethodName(const Name: String); override;
    procedure WriteSet(Value: LongInt; SetType: Pointer); override;
    procedure WriteString(const Value: String); override;
    procedure WriteWideString(const Value: WideString); override;
  public
    property Doc: TDOMDocument read FDoc;
  end;
  TXMLObjectWriterClass = class of TXMLObjectWriter;


  { TXMLObjectReader }

  TXMLObjectReader = class(TAbstractObjectReader)
  private
    FDoc: TDOMDocument;
    FRootEl: TDOMElement;
  public
    constructor Create(ADoc: TDOMDocument; const APath: string);
    destructor Destroy; override;

    { All ReadXXX methods are called _after_ the value type has been read! }
    function NextValue: TValueType; override;
    function ReadValue: TValueType; override;
    procedure BeginRootComponent; override;
    procedure BeginComponent(var Flags: TFilerFlags; var AChildPos: Integer;
      var CompClassName, CompName: String); override;
    function BeginProperty: String; override;

    procedure ReadBinary(const DestData: TMemoryStream); override;
    function ReadFloat: Extended; override;
    function ReadSingle: Single; override;
    function ReadCurrency: Currency; override;
    function ReadDate: TDateTime; override;
    function ReadIdent(ValueType: TValueType): String; override;
    function ReadInt8: ShortInt; override;
    function ReadInt16: SmallInt; override;
    function ReadInt32: LongInt; override;
    function ReadInt64: Int64; override;
    function ReadSet(EnumType: Pointer): Integer; override;
    function ReadStr: String; override;
    function ReadString(StringType: TValueType): String; override;
    function ReadWideString: WideString; override;
    procedure SkipComponent(SkipComponentInfos: Boolean); override;
    procedure SkipValue; override;
  public
    property Doc: TDOMDocument read FDoc;
  end;
  TXMLObjectReaderClass = class of TXMLObjectReader;

procedure WriteComponentToXMLStream(AComponent: TComponent; AStream: TStream);


implementation

procedure WriteComponentToXMLStream(AComponent: TComponent; AStream: TStream);
var
  Driver: TXMLObjectWriter;
  Writer: TWriter;
  XMLDocument: TXMLDocument;
begin
  XMLDocument:=nil;
  Driver:=nil;
  Writer:=nil;
  try
    XMLDocument:=TXMLDocument.Create;
    Driver:=TXMLObjectWriter.Create(XMLDocument,'fcl-persistent',true);
    Writer:=TWriter.Create(Driver);
    Writer.WriteDescendent(AComponent,nil);
    WriteXMLFile(XMLDocument,AStream);
  finally
    Writer.Free;
    Driver.Free;
    XMLDocument.Free;
  end;
end;

procedure TXMLObjectWriter.StackPush;
var
  Parent: TDOMElement;
begin
  if Assigned(FStack) then
  begin
    Parent := StackEl.Element;
    FStack.Add(StackEl);
    StackEl := TXMLObjectWriterStackEl.Create;
    StackEl.Parent := Parent;
  end else
  begin
    FStack := TList.Create;
    StackEl := TXMLObjectWriterStackEl.Create;
    StackEl.Parent := FRootEl;
  end;
  //DebugLn('TXMLObjectWriter.StackPush ',dbgs(FStack.Count));
end;

procedure TXMLObjectWriter.StackPop;
begin
  //DebugLn('TXMLObjectWriter.StackPop ',dbgs(FStack.Count));
  StackEl.Free;
  if FStack.Count > 0 then
  begin
    StackEl := TXMLObjectWriterStackEl(FStack[FStack.Count - 1]);
    FStack.Delete(FStack.Count - 1);
  end else
  begin
    FStack.Free;
    FStack := nil;
    StackEl := nil;
  end;
end;

function TXMLObjectWriter.GetPropertyElement(const TypeName: String
  ): TDOMElement;
begin
  if not Assigned(StackEl.Element) then
  begin
    StackEl.Element := FDoc.CreateElement(TypeName);
    StackEl.Parent.AppendChild(StackEl.Element);
    StackEl.Element['name'] := StackEl.CurName;
    Result := StackEl.Element;
  end else
    Result := nil;
end;

constructor TXMLObjectWriter.Create(ADoc: TDOMDocument;
  const APath: string; Append: Boolean);
var
  Node: TDOMNode;
  PathLen: Integer;
  StartPos: Integer;
  EndPos: LongInt;
  NodeName: string;
  Child: TDOMNode;
  ParentNode: TDOMNode;
begin
  inherited Create;
  FDoc := ADoc;

  Node := Doc.DocumentElement;
  PathLen:=length(APath);
  StartPos:=1;
  while True do begin
    EndPos:=StartPos;
    while (EndPos<=PathLen) and (APath[EndPos]<>'/') do inc(EndPos);
    if EndPos>StartPos then begin
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
    end else if EndPos>PathLen then begin
      break;
    end else begin
      StartPos:=EndPos+1;
    end;
  end;
  if Node is TDOMElement then
    FRootEl:=TDOMElement(Node)
  else
    FRootEl:=nil;

  NodeName:='fcl-persistent';
  ParentNode:=nil;
  if (not Append) and (FRootEl<>nil) then begin
    NodeName:=FRootEl.NodeName;
    ParentNode:=FRootEl.ParentNode;
    if ParentNode<>nil then
      ParentNode.RemoveChild(FRootEl)
    else
      FRootEl.Free;
    FRootEl:=nil;
  end;
  if FRootEl=nil then
    FRootEl := FDoc.CreateElement(NodeName);
  if ParentNode=nil then
    FDoc.AppendChild(FRootEl)
  else
    ParentNode.AppendChild(FRootEl);
end;

procedure TXMLObjectWriter.BeginCollection;
begin
  GetPropertyElement('collectionproperty');
  StackPush;
  StackEl.Element := FDoc.CreateElement('collection');
  StackEl.Parent.AppendChild(StackEl.Element);
end;

procedure TXMLObjectWriter.BeginComponent(Component: TComponent; Flags: TFilerFlags;
  ChildPos: Integer);
// TWriter expects to push two elements on the stack, which are popped by
// two EndList calls.
begin
  StackPush;
  StackEl.Element := FDoc.CreateElement('component');
  StackEl.Parent.AppendChild(StackEl.Element);

  if Length(Component.Name) > 0 then
    StackEl.Element['name'] := Component.Name;
  StackEl.Element['class'] := Component.ClassName;

  StackPush;
  StackEl.Element := FDoc.CreateElement('properties');
  StackEl.Parent.AppendChild(StackEl.Element);
  StackEl.ElType := elPropertyList;
end;

procedure TXMLObjectWriter.BeginList;
begin
  StackPush;
  StackEl.Element := FDoc.CreateElement('list');
  StackEl.Parent.AppendChild(StackEl.Element);
end;

procedure TXMLObjectWriter.EndList;
begin
  if StackEl.ElType = elPropertyList then
  begin
    // end the property list and start the children list
    if not StackEl.Element.HasChildNodes then
      StackEl.Parent.RemoveChild(StackEl.Element);
    StackPop;

    StackPush;
    StackEl.Element := FDoc.CreateElement('children');
    StackEl.Parent.AppendChild(StackEl.Element);
    StackEl.ElType := elChildrenList;
  end else if StackEl.ElType = elChildrenList then
  begin
    // end the children list and the component
    if not StackEl.Element.HasChildNodes then
      StackEl.Parent.RemoveChild(StackEl.Element);
    StackPop; // end children
    StackPop; // end component
  end else
    StackPop;
end;

procedure TXMLObjectWriter.BeginProperty(const PropName: String);
begin
  //DebugLn('TXMLObjectWriter.BeginProperty "',PropName,'"');
  StackPush;
  StackEl.CurName := PropName;
end;

procedure TXMLObjectWriter.EndProperty;
begin
  StackPop;
end;

procedure TXMLObjectWriter.WriteBinary(const Buffer; Count: Longint);
var
  s: string;
begin
  SetLength(s,Count);
  if s<>'' then
    System.Move(Buffer,s[1],length(s));
  GetPropertyElement('binary')['value'] := s;
end;

procedure TXMLObjectWriter.WriteBoolean(Value: Boolean);
begin
  if Value then
    GetPropertyElement('boolean')['value'] := 'true'
  else
    GetPropertyElement('boolean')['value'] := 'false';
end;

procedure TXMLObjectWriter.WriteFloat(const Value: Extended);
begin
  GetPropertyElement('extended')['value'] := FloatToStr(Value);
end;

procedure TXMLObjectWriter.WriteSingle(const Value: Single);
begin
  GetPropertyElement('single')['value'] := FloatToStr(Value);
end;

procedure TXMLObjectWriter.WriteCurrency(const Value: Currency);
begin
  GetPropertyElement('Currency')['value'] := FloatToStr(Value);
end;

procedure TXMLObjectWriter.WriteDate(const Value: TDateTime);
begin
  GetPropertyElement('date')['value'] := FloatToStr(Value);
end;

procedure TXMLObjectWriter.WriteIdent(const Ident: string);
begin
  GetPropertyElement('ident')['value'] := Ident;
end;

procedure TXMLObjectWriter.WriteInteger(Value: Int64);
begin
  GetPropertyElement('integer')['value'] := IntToStr(Value);
end;

procedure TXMLObjectWriter.WriteMethodName(const Name: String);
begin
  GetPropertyElement('method-name')['value'] := Name;
end;

procedure TXMLObjectWriter.WriteSet(Value: LongInt; SetType: Pointer);
var
  i: Integer;
  Mask: LongInt;
  s: String;
begin
  Mask := 1;
  s:='';
  for i := 0 to 31 do begin
    if (Value and Mask) <> 0 then begin
      if s<>'' then s:=s+',';
      s:=s+GetEnumName(PTypeInfo(SetType), i);
    end;
    Mask := Mask shl 1;
  end;
  GetPropertyElement('set')['value'] := s;
end;

procedure TXMLObjectWriter.WriteString(const Value: String);
begin
  GetPropertyElement('string')['value'] := Value;
end;

procedure TXMLObjectWriter.WriteWideString(const Value: WideString);
// save widestrings as utf8
begin
  GetPropertyElement('widestring')['value'] := System.UTF8Encode(Value);
end;


{ TXMLObjectReader }

constructor TXMLObjectReader.Create(ADoc: TDOMDocument; const APath: string);
var
  Node: TDOMNode;
  PathLen: Integer;
  StartPos: Integer;
  EndPos: LongInt;
  NodeName: string;
  Child: TDOMNode;

  procedure RaiseMissingNode;
  begin
    raise Exception.Create('XML node not found '+APath);
  end;
  
  procedure RaiseNotDOMElement;
  begin
    raise Exception.Create('invalid XML node '+APath);
  end;

begin
  inherited Create;
  FDoc := ADoc;

  Node := Doc.DocumentElement;
  PathLen:=length(APath);
  StartPos:=1;
  while True do begin
    EndPos:=StartPos;
    while (EndPos<=PathLen) and (APath[EndPos]<>'/') do inc(EndPos);
    if EndPos>StartPos then begin
      SetLength(NodeName,EndPos-StartPos);
      Move(APath[StartPos],NodeName[1],EndPos-StartPos);
      StartPos:=EndPos+1;
      Child := Node.FindNode(NodeName);
      if not Assigned(Child) then
        RaiseMissingNode;
      Node := Child;
    end else if EndPos>PathLen then begin
      break;
    end else begin
      StartPos:=EndPos+1;
    end;
  end;
  if not (Node is TDOMElement) then
    RaiseNotDOMElement;
  FRootEl:=TDOMElement(Node)
end;

destructor TXMLObjectReader.Destroy;
begin
  inherited Destroy;
end;

function TXMLObjectReader.NextValue: TValueType;
begin
  writeln('TXMLObjectReader.NextValue ');
  Result:=vaNull;
end;

function TXMLObjectReader.ReadValue: TValueType;
begin
  writeln('TXMLObjectReader.ReadValue ');
  Result:=vaNull;
end;

procedure TXMLObjectReader.BeginRootComponent;
begin
  writeln('TXMLObjectReader.BeginRootComponent ');
end;

procedure TXMLObjectReader.BeginComponent(var Flags: TFilerFlags;
  var AChildPos: Integer; var CompClassName, CompName: String);
begin
  writeln('TXMLObjectReader.BeginComponent ');
end;

function TXMLObjectReader.BeginProperty: String;
begin
  writeln('TXMLObjectReader.BeginProperty ');
  Result:='';
end;

procedure TXMLObjectReader.ReadBinary(const DestData: TMemoryStream);
begin
  writeln('TXMLObjectReader.ReadBinary ');
end;

function TXMLObjectReader.ReadFloat: Extended;
begin
  writeln('TXMLObjectReader.ReadFloat ');
  Result:=0;
end;

function TXMLObjectReader.ReadSingle: Single;
begin
  writeln('TXMLObjectReader.ReadSingle ');
  Result:=0;
end;

function TXMLObjectReader.ReadCurrency: Currency;
begin
  writeln('TXMLObjectReader.ReadCurrency ');
  Result:=0;
end;

function TXMLObjectReader.ReadDate: TDateTime;
begin
  writeln('TXMLObjectReader.ReadDate ');
  Result:=0;
end;

function TXMLObjectReader.ReadIdent(ValueType: TValueType): String;
begin
  writeln('TXMLObjectReader.ReadIdent ');
  Result:='';
end;

function TXMLObjectReader.ReadInt8: ShortInt;
begin
  writeln('TXMLObjectReader.ReadInt8 ');
  Result:=0;
end;

function TXMLObjectReader.ReadInt16: SmallInt;
begin
  writeln('TXMLObjectReader.ReadInt16 ');
  Result:=0;
end;

function TXMLObjectReader.ReadInt32: LongInt;
begin
  writeln('TXMLObjectReader.ReadInt32 ');
  Result:=0;
end;

function TXMLObjectReader.ReadInt64: Int64;
begin
  writeln('TXMLObjectReader.ReadInt64 ');
  Result:=0;
end;

function TXMLObjectReader.ReadSet(EnumType: Pointer): Integer;
begin
  writeln('TXMLObjectReader.ReadSet ');
  Result:=0;
end;

function TXMLObjectReader.ReadStr: String;
begin
  writeln('TXMLObjectReader.ReadStr ');
  Result:='';
end;

function TXMLObjectReader.ReadString(StringType: TValueType): String;
begin
  writeln('TXMLObjectReader.ReadString ');
  Result:='';
end;

function TXMLObjectReader.ReadWideString: WideString;
begin
  writeln('TXMLObjectReader.ReadWideString ');
  Result:='';
end;

procedure TXMLObjectReader.SkipComponent(SkipComponentInfos: Boolean);
begin
  writeln('TXMLObjectReader.SkipComponent ');
end;

procedure TXMLObjectReader.SkipValue;
begin
  writeln('TXMLObjectReader.SkipValue ');
end;

end.
