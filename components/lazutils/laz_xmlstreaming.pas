{
 **********************************************************************
  This file is part of LazUtils.
  It is copied from Free Component Library.

  See the file COPYING.FPC, included in this distribution,
  for details about the license.
 **********************************************************************

  XML serialisation driver
  Copyright (c) 2000 by Sebastian Guenther, sg@freepascal.org

  Extended by Mattias Gaertner:
    Reading/Writing children, all kinds of properties,
    custom properties (via DefineProperties).
}
unit Laz_XMLStreaming;

{$MODE objfpc}
{$H+}

{$DEFINE HasReadWriteBuf}
{$DEFINE USE_NEW_READER_WRITER}

interface

uses
  SysUtils, Classes, TypInfo, Laz2_DOM, Laz2_XMLWrite;

type
  TXMLObjectWriterStackElType = (elUnknown, elPropertyList, elChildrenList);

  TXMLObjectWriterStackEl = class
  public
    Element, Parent: TDOMElement;
    ElemType: TXMLObjectWriterStackElType;
    PropertyName: String;
  end;

  { TXMLObjectWriter }

  TXMLObjectWriter = class(TAbstractObjectWriter)
  private
    FDoc: TDOMDocument;
    FRootEl: TDOMElement;
    FStack: TFPList;
    StackEl: TXMLObjectWriterStackEl;
    procedure StackPush(const Element: string = '';
                        ElementType: TXMLObjectWriterStackElType = elUnknown);
    procedure StackPop;
  protected
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
    {$IF FPC_FULLVERSION >= 30000}
    procedure WriteSignature; override;
    {$ENDIF}
    {$IFDEF USE_NEW_READER_WRITER}
    procedure WriteUInt64(Value: QWord); override;
    procedure WriteUnicodeString(const Value: UnicodeString); override;
    procedure WriteVariant(const VarValue: Variant); override;
    {$ENDIF}
    {$IFDEF HasReadWriteBuf}
    procedure Write(const {%H-}Buffer; Count: Longint); override;
    {$ENDIF}
  public
    property Doc: TDOMDocument read FDoc;
  end;
  TXMLObjectWriterClass = class of TXMLObjectWriter;


  { TXMLObjectReader }

  TXMLObjectReader = class(TAbstractObjectReader)
  private
    FDoc: TDOMDocument;
    FElement: TDOMElement;
    FElementPosition: integer;
    FRootEl: TDOMElement;
    function ReadNextValue(Stay: Boolean): TValueType;
  public
    constructor Create(ADoc: TDOMDocument; const APath: string);
    destructor Destroy; override;
    
    function GetRootClassName(out IsInherited: Boolean): string;

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
    function ReadSet(SetType: Pointer): Integer; override;
    function ReadStr: String; override;
    function ReadString(StringType: TValueType): String; override;
    function ReadWideString: WideString; override;
    {$IF FPC_FULLVERSION >= 30000}
    procedure ReadSignature; override;
    {$ENDIF}
    {$IFDEF USE_NEW_READER_WRITER}
    function ReadUnicodeString: UnicodeString; override;
    {$ENDIF}
    procedure SkipComponent(SkipComponentInfos: Boolean); override;
    procedure SkipValue; override;
    {$IFDEF HasReadWriteBuf}
    procedure Read(var {%H-}Buf; Count: LongInt); override;
    {$ENDIF}
  public
    property Doc: TDOMDocument read FDoc;
    property Element: TDOMElement read FElement;// current element node
    property ElementPosition: integer read FElementPosition;
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

procedure TXMLObjectWriter.StackPush(const Element: string;
  ElementType: TXMLObjectWriterStackElType);
var
  Parent: TDOMElement;
  i: Integer;
begin
  if Assigned(FStack) then
  begin
    // append to stack
    Parent := StackEl.Element;
    if Parent=nil then begin
      i:=FStack.Count-1;
      while (i>=0) do begin
        if (TXMLObjectWriterStackEl(FStack[i]).Element=nil) then
          dec(i)
        else begin
          Parent:=TXMLObjectWriterStackEl(FStack[i]).Element;
          break;
        end;
      end;
      if Parent=nil then
        Parent:=FRootEl;
    end;
    FStack.Add(StackEl);
    StackEl := TXMLObjectWriterStackEl.Create;
    StackEl.Parent := Parent;
  end else
  begin
    // start stack
    FStack := TFPList.Create;
    StackEl := TXMLObjectWriterStackEl.Create;
    StackEl.Parent := FRootEl;
  end;

  if Element<>'' then begin
    // create element
    StackEl.Element := FDoc.CreateElement(Element);
    StackEl.Parent.AppendChild(StackEl.Element);
    StackEl.ElemType:=ElementType;
  end;
  //DebugLn('TXMLObjectWriter.StackPush Element="',Element,'" FStack.Count=',dbgs(FStack.Count),' ',DbgSName(StackEl.Parent));
end;

procedure TXMLObjectWriter.StackPop;
begin
  //DebugLn('TXMLObjectWriter.StackPop ',dbgs(FStack.Count));
  if FStack=nil then
    raise Exception.Create('TXMLObjectWriter.StackPop stack empty');
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
var
  CustomElement: TDOMElement;
begin
  if StackEl.PropertyName<>'' then begin
    // normal property
    if not Assigned(StackEl.Element) then begin
      StackEl.Element := FDoc.CreateElement(TypeName);
      StackEl.Parent.AppendChild(StackEl.Element);
      StackEl.Element['name'] := StackEl.PropertyName;
      Result := StackEl.Element;
    end else begin
      raise Exception.Create('TXMLObjectWriter.GetPropertyElement property already saved');
    end;
  end else begin
    // custom defined property (via DefineProperties)
    CustomElement := FDoc.CreateElement(TypeName);
    StackEl.Element.AppendChild(CustomElement);
    Result := CustomElement;
  end;
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
  StackPush('collection');
end;

procedure TXMLObjectWriter.BeginComponent(Component: TComponent; Flags: TFilerFlags;
  ChildPos: Integer);
// TWriter expects to push two elements on the stack, which are popped by
// two EndList calls.
begin
  StackPush('component');

  if Length(Component.Name) > 0 then
    StackEl.Element['name'] := Component.Name;
  StackEl.Element['class'] := Component.ClassName;

  if ChildPos>=0 then begin
    // ToDo
  end;
  if Flags<>[] then begin
    // ToDo
  end;

  StackPush('properties',elPropertyList);
end;

procedure TXMLObjectWriter.BeginList;
begin
  StackPush('list');
end;

procedure TXMLObjectWriter.EndList;
begin
  if StackEl.ElemType = elPropertyList then
  begin
    // end the property list and start the children list
    if not StackEl.Element.HasChildNodes then
      StackEl.Parent.RemoveChild(StackEl.Element);
    StackPop;

    StackPush('children',elChildrenList);
  end else if StackEl.ElemType = elChildrenList then
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
  StackEl.PropertyName := PropName;
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
  GetPropertyElement('method')['value'] := Name;
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

{$IF FPC_FULLVERSION >= 30000}
procedure TXMLObjectWriter.WriteSignature;
begin
end;
{$ENDIF}

{$IFDEF USE_NEW_READER_WRITER}
procedure TXMLObjectWriter.WriteUInt64(Value: QWord);
begin
  GetPropertyElement('uint64')['value'] := IntToStr(Value);
end;

procedure TXMLObjectWriter.WriteUnicodeString(const Value: UnicodeString);
// save unicodestrings as utf8
begin
  GetPropertyElement('unicodestring')['value'] := System.UTF8Encode(Value);
end;

procedure TXMLObjectWriter.WriteVariant(const VarValue: Variant);
begin
  case tvardata(VarValue).vtype of
    varEmpty:
      begin
        GetPropertyElement('uint64')['value'] := 'nil';
      end;
    varNull:
      begin
        GetPropertyElement('uint64')['value'] := 'null';
      end;
    { all integer sizes must be split for big endian systems }
    varShortInt,varSmallInt,varInteger,varInt64:
      begin
        WriteInteger(VarValue);
      end;
    varQWord:
      begin
        WriteUInt64(VarValue);
      end;
    varBoolean:
      begin
        WriteBoolean(VarValue);
      end;
    varCurrency:
      begin
        WriteCurrency(VarValue);
      end;
    varSingle:
      begin
        WriteSingle(VarValue);
      end;
    varDouble:
      begin
        WriteFloat(VarValue);
      end;
    varDate:
      begin
        WriteDate(VarValue);
      end;
    varOleStr,varString:
      begin
        WriteWideString(VarValue);
      end;
    else
      raise EWriteError.CreateFmt('Unsupported property variant type %d', [Ord(tvardata(VarValue).vtype)]);
  end;
end;

{$ENDIF}

{$IFDEF HasReadWriteBuf}
procedure TXMLObjectWriter.Write(const Buffer; Count: Longint);
begin
  if Count<=0 then exit;
  // there can be arbitrary lots of Write calls
  raise Exception.Create('TODO: TXMLObjectWriter.Write');
end;
{$ENDIF}

{ TXMLObjectReader }

function TXMLObjectReader.ReadNextValue(Stay: Boolean): TValueType;

  procedure RaiseUnknownNode(Node: TDOMNode);
  begin
    raise EReadError.Create('TXMLObjectReader: unknown node "'+Node.NodeName+'"');
  end;

  procedure RaiseUnknownParentNode(Node: TDOMNode);
  begin
    raise EReadError.Create('TXMLObjectReader: unknown parent node "'+Node.NodeName+'" Element="'+FElement.NodeName+'"');
  end;
  
  procedure RaiseInvalidElementPosition;
  begin
    raise EReadError.Create('TXMLObjectReader: invalid ElementPosition='+IntToStr(FElementPosition)+' Node='+FElement.NodeName);
  end;
  
  procedure RaiseNodeNotFound(const NodeName: string);
  begin
    raise EReadError.Create('TXMLObjectReader: expected "'+NodeName+'", but found "'+FElement.NodeName+'"');
  end;
  
  procedure CheckNode(const NodeName: string);
  begin
    if FElement.NodeName<>NodeName then
      RaiseNodeNotFound(NodeName);
  end;
  
  procedure GoToNextComponent;
  begin
    FElement:=FElement.ParentNode as TDOMElement;
    CheckNode('component');
    FElementPosition:=0;
    if FElement.NextSibling is TDOMElement then begin
      // go to next component
      //writeln('TXMLObjectReader.ReadNextValue properties: next component');
      FElement:=TDOMElement(FElement.NextSibling);
      CheckNode('component');
    end else begin
      // end of children list
      if FElement.ParentNode.NodeName='children' then begin
        //writeln('TXMLObjectReader.ReadNextValue end of children list');
        FElement:=FElement.ParentNode as TDOMElement;
        FElementPosition:=1;
      end else begin
        //writeln('TXMLObjectReader.ReadNextValue END reading');
        FElement:=nil;
      end;
    end;
  end;

var
  CurValue: String;
  CurInt64: Int64;
begin
  //writeln('TXMLObjectReader.ReadNextValue Stay=',Stay,' Element=',FElement.NodeName,' Pos=',FElementPosition);
  Result:=vaNull;

  if FElement=nil then begin
    //writeln('TXMLObjectReader.ReadNextValue FElement=nil');
  end else if FElement.NodeName='component' then begin
    //writeln('TXMLObjectReader.ReadNextValue is start of component');
    Result:=vaString;
    if not Stay then begin
      // here a BeginComponent shoud be called, not ReadValue
      RaiseUnknownNode(FElement);
    end;
  end
  else if FElement.NodeName='properties' then begin
    // FElement is at end of property list or non existing children list
    // 0: end of property list
    // 1: end of non existing children list
    //writeln('TXMLObjectReader.ReadNextValue FElement is at end of property list');
    if not Stay then begin
      if FElement.NextSibling is TDOMElement then begin
        // leave properties and go to first child component
        //writeln('TXMLObjectReader.ReadNextValue properties: children');
        FElement:=TDOMElement(FElement.NextSibling);
        FElementPosition:=0;
        CheckNode('children');
        if not (FElement.FirstChild is TDOMElement) then
          RaiseUnknownNode(FElement);
        FElement:=TDOMElement(FElement.FirstChild);
      end else begin
        // there is no children list behind the properties -> simulate it
        if FElementPosition=0 then begin
          inc(FElementPosition);
        end else begin
          // children has been simulated -> now go to next component
          GoToNextComponent;
        end;
      end;
    end;
  end
  else if FElement.NodeName='children' then begin
    // end of children list
    //writeln('TXMLObjectReader.ReadNextValue End of children list');
    if not Stay then begin
      GoToNextComponent;
    end;
  end
  else if FElement.NodeName='list' then begin
    // FElement is a list element
    // It has 2 positions:
    // 0: vaList
    // 1: end of list
    case FElementPosition of

    0:begin
        //writeln('TXMLObjectReader.ReadNextValue list: vaList');
        Result:=vaList;
        if (FElement.FirstChild is TDOMElement) then begin
          // the list has children
          if not Stay then begin
            FElement:=TDOMElement(FElement.FirstChild);
            FElementPosition:=0;
          end;
        end else begin
          // empty list
          if not Stay then
            inc(FElementPosition);
        end;
      end;
      
    1:begin
        // end of list
        if not Stay then begin
          if (FElement.NextSibling is TDOMElement) then begin
            //writeln('TXMLObjectReader.ReadNextValue list: end of children, next list');
            FElement:=TDOMElement(FElement.NextSibling);
            FElementPosition:=0;
          end else begin
            //writeln('TXMLObjectReader.ReadNextValue list: end of children, end of collection');
            FElement:=FElement.ParentNode as TDOMElement;
            FElementPosition:=0;
          end;
        end;
      end;
        
    end;
  end
  else if FElement.NodeName='collection' then begin
    // FElement is at end of collection
    //writeln('TXMLObjectReader.ReadNextValue FElement is at end of collection');
  end
  else if (FElement.ParentNode.NodeName='properties')
  or (FElement.ParentNode.NodeName='list') then begin
    // FElement is a property
    // It has 3 positions:
    // 0: name
    // 1: value type
    // 2: value
    case FElementPosition of
    
    0:// the property name
      begin
        Result:=vaString;
        if not Stay then
          inc(FElementPosition);
      end;

    1:// value type
      begin
        if FElement.NodeName='integer' then begin
          CurValue:=FElement['value'];
          CurInt64:=StrToInt64(CurValue);
          if (CurInt64 >= -128) and (CurInt64 <= 127) then begin
            Result:=vaInt8
          end else if (CurInt64 >= -32768) and (CurInt64 <= 32767) then begin
            Result:=vaInt16;
          end else if (CurInt64 >= Low(Integer)) and (CurInt64 <= High(integer))
          then begin
            Result:=vaInt32;
          end else
            Result:=vaInt64;
        end else if FElement.NodeName='string' then begin
          CurValue:=FElement['value'];
          if length(CurValue)<=255 then
            Result:=vaString
          else
            Result:=vaLString;
        end else if FElement.NodeName='ident' then
          Result:=vaIdent
        else if FElement.NodeName='boolean' then begin
          if FElement['value']='true' then
            Result:=vaTrue
          else
            Result:=vaFalse;
        end else if FElement.NodeName='method' then
          Result:=vaIdent
        else if FElement.NodeName='set' then
          Result:=vaSet
        else if FElement.NodeName='extended' then
          Result:=vaExtended
        else if FElement.NodeName='widestring' then
          Result:=vaWString
        {$IFDEF USE_NEW_READER_WRITER}
        else if FElement.NodeName = 'uint64' then
          Result:=vaQWord
        else if FElement.NodeName='unicodestring' then
          Result:=vaUString
        else if FElement.NodeName='variant' then
        begin
          if FElement['value'] = 'nil' then
            Result := vaNil
          else
            Result := vaNull;
        end
        {$ENDIF}
        else if FElement.NodeName='collectionproperty' then
          Result:=vaCollection
        else if FElement.NodeName='binary' then
          Result:=vaBinary
        else
          RaiseUnknownNode(FElement);
        if not Stay then begin
          inc(FElementPosition);
          case Result of
          vaTrue, vaFalse: ReadValue;
          vaCollection:
            begin
              // go to node 'collection'
              FElement:=FElement.FirstChild as TDOMElement;
              CheckNode('collection');
              FElementPosition:=0;
              // go to node 'list'
              if Assigned(FElement.FirstChild) and (FElement.FirstChild.NodeName='list') then
                FElement:=FElement.FirstChild as TDOMElement;
            end;
          end;
        end;
      end;

    2:// value
      begin
        if FElement.NextSibling<>nil then begin
          Result:=vaString;
          if not Stay then begin
            FElement:=FElement.NextSibling as TDOMElement;
            FElementPosition:=0;
          end;
        end else begin
          // end of property list
          Result:=vaNull;
          if not Stay then begin
            FElement:=FElement.ParentNode as TDOMElement;
            FElementPosition:=0;
            if FElement.NodeName='list' then
              FElementPosition:=1;
          end;
        end;
      end;
    else
      RaiseInvalidElementPosition;
    end;
  end else begin
    RaiseUnknownParentNode(FElement.ParentNode);
  end;
  //writeln('TXMLObjectReader.ReadNextValue Result=',GetEnumName(TypeInfo(TValueType),ord(Result)));
end;

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
  FRootEl:=TDOMElement(Node);
  FElement:=FRootEl;
  FElementPosition:=0;
end;

destructor TXMLObjectReader.Destroy;
begin
  inherited Destroy;
end;

function TXMLObjectReader.GetRootClassName(out IsInherited: Boolean): string;
var
  ComponentNode: TDOMNode;
  CompElement: TDOMElement;
begin
  IsInherited:=false;
  
  ComponentNode:=FRootEl.FindNode('component');
  if ComponentNode=nil then
    raise Exception.Create('component node not found');
  if not (ComponentNode is TDOMElement) then
    raise Exception.Create('component node is not a dom element');
  CompElement:=TDOMElement(ComponentNode);

  Result:=CompElement['class'];
  //DebugLn('TXMLObjectReader.GetRootClassName RootClassName="',Result,'"');
  
  // TODO: IsInherited
end;

function TXMLObjectReader.NextValue: TValueType;
begin
  Result:=ReadNextValue(true);
end;

function TXMLObjectReader.ReadValue: TValueType;
begin
  Result:=ReadNextValue(false);
end;

procedure TXMLObjectReader.BeginRootComponent;

  procedure RaiseComponentNodeNotFound;
  begin
    raise Exception.Create('component node not found');
  end;
  
var
  Node: TDOMNode;
begin
  //writeln('TXMLObjectReader.BeginRootComponent ');
  if FElement=nil then
    Node:=nil
  else
    Node:=FElement.FindNode('component');
  if Node=nil then
    RaiseComponentNodeNotFound;
end;

procedure TXMLObjectReader.BeginComponent(var Flags: TFilerFlags;
  var AChildPos: Integer; var CompClassName, CompName: String);
var
  ComponentNode: TDOMNode;
  PropertiesNode: TDOMNode;
begin
  //writeln('TXMLObjectReader.BeginComponent START');
  if AChildPos>0 then begin
    // ToDo
  end;
  if Flags<>[] then begin
    // ToDo
  end;

  if FElement=nil then
    ComponentNode:=nil
  else if FElement.NodeName='component' then
    ComponentNode:=FElement
  else
    ComponentNode:=FElement.FindNode('component');
  if ComponentNode=nil then
    raise Exception.Create('component node not found');
  if not (ComponentNode is TDOMElement) then
    raise Exception.Create('component node is not a dom element');
  FElement:=TDOMElement(ComponentNode);
  
  CompName:=FElement['name'];
  CompClassName:=FElement['class'];
  //DebugLn('TXMLObjectReader.BeginComponent CompName="',CompName,'" CompClassName="',CompClassName,'"');
  
  PropertiesNode:=FElement.FindNode('properties');
  if (PropertiesNode<>nil) then begin
    if not (PropertiesNode is TDOMElement) then
      raise Exception.Create('properties node is not a dom element');

    // if there are properties, then set FElement to the first property
    FElement:=TDOMElement(PropertiesNode);
    if FElement.FirstChild is TDOMElement then
      FElement:=TDOMElement(FElement.FirstChild);
  end else
    FElement:=nil;
  FElementPosition:=0;
end;

function TXMLObjectReader.BeginProperty: String;
begin
  Result:=FElement['name'];
  //writeln('TXMLObjectReader.BeginProperty Result="',Result,'"');
  inc(FElementPosition);
end;

procedure TXMLObjectReader.ReadBinary(const DestData: TMemoryStream);
var
  Value: String;
begin
  Value:=FElement['value'];
  if Value<>'' then
    DestData.Write(Value[1],length(Value));
  ReadValue;
  //writeln('TXMLObjectReader.ReadBinary ');
end;

function TXMLObjectReader.ReadFloat: Extended;
var
  Value: String;
begin
  Result:=0;
  Value:=FElement['value'];
  Result:=StrToFloat(Value);
  ReadValue;
  //writeln('TXMLObjectReader.ReadFloat ',Result);
end;

function TXMLObjectReader.ReadSingle: Single;
var
  Value: String;
  FloatError: integer;
  Back: single;
begin
  Result:=0;
  Value:=FElement['value'];
  Val(Value, Back, FloatError);
  if FloatError=0 then ;
  Result:=Back;
  ReadValue;
  //writeln('TXMLObjectReader.ReadSingle ',Result);
end;

function TXMLObjectReader.ReadCurrency: Currency;
var
  Value: String;
begin
  Result:=0;
  Value:=FElement['value'];
  Result:=StrToFloat(Value);
  ReadValue;
  //writeln('TXMLObjectReader.ReadCurrency ',Result);
end;

function TXMLObjectReader.ReadDate: TDateTime;
var
  Value: String;
begin
  Result:=0;
  Value:=FElement['value'];
  Result:=StrToFloat(Value);
  ReadValue;
  //writeln('TXMLObjectReader.ReadDate ',Result);
end;

function TXMLObjectReader.ReadIdent(ValueType: TValueType): String;
begin
  Result:=FElement['value'];
  ReadValue;
  // ToDo: check type
  if ValueType=vaNull then ;
  //writeln('TXMLObjectReader.ReadIdent ',Result);
end;

function TXMLObjectReader.ReadInt8: ShortInt;
var
  Value: String;
  FloatError: integer;
  Back: ShortInt;
begin
  Result:=0;
  Value:=FElement['value'];
  Val(Value, Back, FloatError);
  if FloatError=0 then ;
  Result:=Back;
  ReadValue;
  //writeln('TXMLObjectReader.ReadInt8 ',Result);
end;

function TXMLObjectReader.ReadInt16: SmallInt;
var
  Value: String;
  FloatError: integer;
  Back: SmallInt;
begin
  Result:=0;
  Value:=FElement['value'];
  Val(Value, Back, FloatError);
  if FloatError=0 then ;
  Result:=Back;
  ReadValue;
  //writeln('TXMLObjectReader.ReadInt16 ',Result);
end;

function TXMLObjectReader.ReadInt32: LongInt;
var
  Value: String;
  FloatError: integer;
  Back: Longint;
begin
  Result:=0;
  Value:=FElement['value'];
  Val(Value, Back, FloatError);
  if FloatError=0 then ;
  Result:=Back;
  ReadValue;
  //writeln('TXMLObjectReader.ReadInt32 ',Result);
end;

function TXMLObjectReader.ReadInt64: Int64;
var
  Value: String;
  FloatError: integer;
  Back: Int64;
begin
  Result:=0;
  Value:=FElement['value'];
  Val(Value, Back, FloatError);
  if FloatError=0 then ;
  Result:=Back;
  ReadValue;
  //writeln('TXMLObjectReader.ReadInt64 ',Result);
end;

function TXMLObjectReader.ReadSet(SetType: Pointer): Integer;
var
  s: String;
  StartPos: Integer;
  EndPos: LongInt;
  Name: String;
  i: LongInt;
begin
  Result:=0;
  s:=FElement['value'];
  StartPos:=1;
  while (StartPos<=length(s)) do begin
    EndPos:=StartPos;
    while (EndPos<=length(s)) and (s[EndPos]<>',') do inc(EndPos);
    if EndPos>StartPos then begin
      Name:=copy(s,StartPos,EndPos-StartPos);
      i:=GetEnumValue(PTypeInfo(SetType),Name);
      Result:=Result or (1 shl i);
    end;
    StartPos:=EndPos+1;
  end;
  ReadValue;
  //writeln('TXMLObjectReader.ReadSet ',HexStr(Cardinal(Result),8));
end;

function TXMLObjectReader.ReadStr: String;
begin
  Result:=FElement['value'];
  ReadValue;
  //writeln('TXMLObjectReader.ReadStr "',Result,'"');
end;

function TXMLObjectReader.ReadString(StringType: TValueType): String;
begin
  Result:=FElement['value'];
  if (StringType=vaString) and (length(Result)>255) then
    raise Exception.Create('TXMLObjectReader.ReadString invalid StringType');
  ReadValue;
  //writeln('TXMLObjectReader.ReadString "',Result,'"');
end;

function TXMLObjectReader.ReadWideString: WideString;
var
  ValueAsUTF8: String;
begin
  ValueAsUTF8:=FElement['value'];
  Result:=System.UTF8Decode(ValueAsUTF8);
  ReadValue;
  //writeln('TXMLObjectReader.ReadWideString "',ValueAsUTF8,'"');
end;

{$IF FPC_FULLVERSION >= 30000}
procedure TXMLObjectReader.ReadSignature;
begin
end;
{$ENDIF}

{$IFDEF USE_NEW_READER_WRITER}
function TXMLObjectReader.ReadUnicodeString: UnicodeString;
var
  ValueAsUTF8: String;
begin
  ValueAsUTF8:=FElement['value'];
  Result:=System.UTF8Decode(ValueAsUTF8);
  ReadValue;
end;
{$ENDIF}

procedure TXMLObjectReader.SkipComponent(SkipComponentInfos: Boolean);
var
  NextNode: TDOMNode;
begin
  NextNode:=FElement.NextSibling;
  if (NextNode=nil) or (NextNode is TDOMElement) then
    FElement:=TDOMElement(NextNode);
  // ToDo: SkipComponentInfos
  if SkipComponentInfos then ;
  //writeln('TXMLObjectReader.SkipComponent ');
end;

procedure TXMLObjectReader.SkipValue;
begin
  ReadValue;
  //writeln('TXMLObjectReader.SkipValue ');
end;

{$IFDEF HasReadWriteBuf}
procedure TXMLObjectReader.Read(var Buf; Count: LongInt);
begin
  if Count<=0 then exit;
  raise Exception.Create('TODO: TXMLObjectReader.Read');
  //writeln('TXMLObjectReader.Read ');
end;
{$ENDIF}

end.
