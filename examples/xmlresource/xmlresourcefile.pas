unit xmlresourcefile;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, SysUtils,
  LCLMemManager, forms,
  dom, XMLRead,XMLWrite,
  ProjectIntf,
  UnitResources;

type

  { TXMLUnitResourcefileFormat }

  TXMLUnitResourcefileFormat = class(TUnitResourcefileFormat)
  private
    class procedure QuickReadXML(s: TStream; out AComponentName, AClassName, ALCLVersion: string);
  public
    class function FindResourceDirective(Source: TObject): boolean; override;
    class function ResourceDirectiveFilename: string; override;
    class function GetUnitResourceFilename(AUnitFilename: string;
      {%H-}Loading: boolean): string; override;
    class procedure TextStreamToBinStream(ATxtStream, ABinStream: TExtMemoryStream); override;
    class procedure BinStreamToTextStream(ABinStream, ATextStream: TExtMemoryStream); override;
    class function GetClassNameFromStream(s: TStream; out IsInherited: Boolean): shortstring; override;
    class function CreateReader(s: TStream; var {%H-}DestroyDriver: boolean): TReader; override;
    class function CreateWriter(s: TStream; var DestroyDriver: boolean): TWriter; override;
    class function QuickCheckResourceBuffer(PascalBuffer, LFMBuffer: TObject;
      out LFMType, LFMComponentName, LFMClassName: string; out
      LCLVersion: string; out MissingClasses: TStrings): TModalResult; override;
  end;

  { TXMLReader }

  TXMLReader = class(TReader)
  protected
    function CreateDriver(Stream: TStream; BufSize: Integer): TAbstractObjectReader; override;
  end;

  { TXMLObjectReader }

  TXMLObjectReader = class(TAbstractObjectReader)
  private
    FXMLDoc: TXMLDocument;
    FStream: TStream;
    FObjNode: TDOMNode;
    FCurNode: TDOMNode;
    FCurValue: string;
    FReadingChilds: Boolean;
  public
    constructor create(AStream: TStream); virtual;
    destructor Destroy; override;
    function NextValue: TValueType; override;
    function ReadValue: TValueType; override;
    procedure BeginRootComponent; override;
    procedure BeginComponent(var Flags: TFilerFlags; var AChildPos: Integer;
      var CompClassName, CompName: String); override;
    function BeginProperty: String; override;

    //Please don't use read, better use ReadBinary whenever possible
    procedure Read(var Buf; Count: LongInt); override;
    { All ReadXXX methods are called _after_ the value type has been read! }
    procedure ReadBinary(const DestData: TMemoryStream); override;
    function ReadCurrency: Currency; override;
    function ReadIdent(ValueType: TValueType): String; override;
    function ReadInt8: ShortInt; override;
    function ReadInt16: SmallInt; override;
    function ReadInt32: LongInt; override;
    function ReadInt64: Int64; override;
    function ReadSet(EnumType: Pointer): Integer; override;
    function ReadStr: String; override;
    function ReadString(StringType: TValueType): String; override;
    function ReadWideString: WideString;override;
    function ReadUnicodeString: UnicodeString;override;
    procedure SkipComponent(SkipComponentInfos: Boolean); override;
    procedure SkipValue; override;
  end;

  { TXMLWriter }

  TXMLWriter = class(TWriter)
  protected
    function CreateDriver(Stream: TStream; BufSize: Integer): TAbstractObjectWriter; override;
  end;

  { TXMLObjectWriter }

  TXMLObjectWriter = class(TAbstractObjectWriter)
  private
    FXMLCreated: boolean;
    FXMLDoc: TXMLDocument;
    FListLevel: integer;
    FObjNode: TDOMNode;
    FCurNode: TDOMElement;
    FStream: TStream;
    FIsStreamingProps: boolean;
  private
    procedure CreateXML;
  public
    constructor Create(Stream: TStream; BufSize: Integer);
    destructor Destroy; override;

    procedure BeginCollection; override;
    procedure BeginComponent(Component: TComponent; Flags: TFilerFlags;
      ChildPos: Integer); override;
    procedure BeginList; override;
    procedure EndList; override;
    procedure BeginProperty(const PropName: String); override;
    procedure EndProperty; override;

    //Please don't use write, better use WriteBinary whenever possible
    procedure Write(const Buffer; Count: Longint); override;
    procedure WriteBinary(const Buffer; Count: LongInt); override;
    procedure WriteBoolean(Value: Boolean); override;

    procedure WriteCurrency(const Value: Currency); override;
    procedure WriteIdent(const Ident: string); override;
    procedure WriteInteger(Value: Int64); override;
    procedure WriteUInt64(Value: QWord); override;
    procedure WriteMethodName(const Name: String); override;
    procedure WriteSet(Value: LongInt; SetType: Pointer); override;
    procedure WriteString(const Value: String); override;
    procedure WriteWideString(const Value: WideString); override;
    procedure WriteUnicodeString(const Value: UnicodeString); override;
    procedure WriteVariant(const VarValue: Variant);override;

    procedure WriteFloat(const Value: Extended);  override;
    procedure WriteSingle(const Value: Single); override;
    procedure WriteDate(const Value: TDateTime); override;


  end;

  { TFileDescPascalUnitWithXMLResource }

  TFileDescPascalUnitWithXMLResource = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetImplementationSource(const Filename, SourceName, ResourceName: string): string; override;
  end;


procedure register;

implementation

uses
  FileUtil,
  RtlConsts,
  CodeCache;

procedure register;
begin
  RegisterUnitResourcefileFormat(TXMLUnitResourcefileFormat);
  RegisterProjectFileDescriptor(TFileDescPascalUnitWithXMLResource.Create,
                                FileDescGroupName);
end;

{ TFileDescPascalUnitWithXMLResource }

constructor TFileDescPascalUnitWithXMLResource.Create;
begin
  inherited Create;
  ResourceClass:=TForm;
end;

function TFileDescPascalUnitWithXMLResource.GetLocalizedName: string;
begin
  Result:='Form with XML resource file';
end;

function TFileDescPascalUnitWithXMLResource.GetLocalizedDescription: string;
begin
  Result:='Create a new unit with a LCL form with XML resource file.';
end;

function TFileDescPascalUnitWithXMLResource.GetImplementationSource(
  const Filename, SourceName, ResourceName: string): string;
var
  ResourceFilename: String;
  LE: String;
begin
  LE:=LineEnding;
  case GetResourceType of
    rtLRS:
      begin
        ResourceFilename:=TrimFilename(ExtractFilenameOnly(Filename)+DefaultResFileExt);
        Result:='initialization'+LE+'  {$I '+ResourceFilename+'}'+LE+LE;
      end;
    rtRes: Result := '{$R *.xml}'+LE+LE;
  end;
end;

{ TXMLObjectWriter }

procedure TXMLObjectWriter.CreateXML;
begin
  FXMLDoc := TXMLDocument.Create;
  FXMLCreated:=true;
end;

constructor TXMLObjectWriter.Create(Stream: TStream; BufSize: Integer);
begin
  inherited Create;
  FStream:=Stream;
end;

destructor TXMLObjectWriter.Destroy;
begin
  FXMLDoc.Free;
  inherited Destroy;
end;

procedure TXMLObjectWriter.BeginCollection;
begin

end;

procedure TXMLObjectWriter.BeginComponent(Component: TComponent;
  Flags: TFilerFlags; ChildPos: Integer);
var
  ANewNode : TDOMElement;
begin
  if not FXmlCreated then
    begin
    CreateXML;
    end;
  inc(FListLevel,2);
  ANewNode := FXMLDoc.CreateElement('object');

  ANewNode.AttribStrings['type'] := Component.ClassName;
  ANewNode.AttribStrings['name'] := Component.Name;
  if not assigned(FObjNode) then
    FXMLDoc.AppendChild(ANewNode)
  else
    FObjNode.AppendChild(ANewNode);
  FObjNode := ANewNode;
  FIsStreamingProps:=True;
end;

procedure TXMLObjectWriter.BeginList;
begin
  inc(FListLevel);
end;

procedure TXMLObjectWriter.EndList;
begin
  dec(FListLevel);
  if FIsStreamingProps then
    begin
    FIsStreamingProps:=false;
    end
  else
    FObjNode := FObjNode.ParentNode;

  if FListLevel=0 then
    WriteXMLFile(FXMLDoc,FStream);
end;

procedure TXMLObjectWriter.BeginProperty(const PropName: String);
begin
  FCurNode := FXMLDoc.CreateElement('property');
  FObjNode.AppendChild(FCurNode);
  FCurNode.AttribStrings['name'] := PropName;
end;

procedure TXMLObjectWriter.EndProperty;
begin
  // Do nothing
end;

procedure TXMLObjectWriter.Write(const Buffer; Count: Longint);
begin

end;

procedure TXMLObjectWriter.WriteBinary(const Buffer; Count: LongInt);
begin

end;

procedure TXMLObjectWriter.WriteBoolean(Value: Boolean);
begin
  if value then
    begin
    FCurNode.AttribStrings['type'] := 'vatrue';
    FCurNode.TextContent:='True';
    end
  else
    begin
    FCurNode.AttribStrings['type'] := 'vafalse';
    FCurNode.TextContent:='False';
    end
end;

procedure TXMLObjectWriter.WriteCurrency(const Value: Currency);
begin

end;

procedure TXMLObjectWriter.WriteIdent(const Ident: string);
begin
  FCurNode.AttribStrings['type'] := 'ident';
  FCurNode.TextContent:=Ident;
end;

procedure TXMLObjectWriter.WriteInteger(Value: Int64);
begin
  FCurNode.AttribStrings['type'] := 'int64';
  FCurNode.TextContent:=inttostr(value);
end;

procedure TXMLObjectWriter.WriteUInt64(Value: QWord);
begin
  FCurNode.AttribStrings['type'] := 'int64';
  FCurNode.TextContent:=inttostr(value);
end;

procedure TXMLObjectWriter.WriteMethodName(const Name: String);
begin
  FCurNode.AttribStrings['type'] := 'ident';
  FCurNode.TextContent:=Name;
end;

procedure TXMLObjectWriter.WriteSet(Value: LongInt; SetType: Pointer);
begin

end;

procedure TXMLObjectWriter.WriteString(const Value: String);
begin
  FCurNode.AttribStrings['type'] := 'string';
  FCurNode.TextContent:=value;
end;

procedure TXMLObjectWriter.WriteWideString(const Value: WideString);
begin

end;

procedure TXMLObjectWriter.WriteUnicodeString(const Value: UnicodeString);
begin

end;

procedure TXMLObjectWriter.WriteVariant(const VarValue: Variant);
begin

end;

procedure TXMLObjectWriter.WriteFloat(const Value: Extended);
begin
  //
end;

procedure TXMLObjectWriter.WriteSingle(const Value: Single);
begin
  //
end;

procedure TXMLObjectWriter.WriteDate(const Value: TDateTime);
begin
  //
end;

{ TXMLWriter }

function TXMLWriter.CreateDriver(Stream: TStream; BufSize: Integer
  ): TAbstractObjectWriter;
begin
  Result:=TXMLObjectWriter.Create(Stream,BufSize);
end;

{ TXMLObjectReader }

constructor TXMLObjectReader.create(AStream: TStream);
begin
  inherited create;

  If (AStream=Nil) then
    Raise EReadError.Create(SEmptyStreamIllegalReader);

  FStream := AStream;
end;

destructor TXMLObjectReader.Destroy;
begin
  FXMLDoc.Free;
  inherited Destroy;
end;

function TXMLObjectReader.NextValue: TValueType;
var
  StoreNode,
  StoreObjNode: TDOMNode;
  StoreReadingChilds: boolean;
begin
  StoreNode := FCurNode;
  StoreObjNode := FObjNode;
  StoreReadingChilds := FReadingChilds;
  result := ReadValue;
  FCurNode:=StoreNode;
  FObjNode:=StoreObjNode;
  FReadingChilds:=StoreReadingChilds;
end;

function TXMLObjectReader.ReadValue: TValueType;
begin
  result := vaNull;
  if not assigned(FCurNode) then
    begin
    if not FReadingChilds then
      begin
      FCurNode := FObjNode.FirstChild;
      while assigned(FCurNode) and (FCurNode.NodeName<>'object') do
        FCurNode := FCurNode.NextSibling;
      FReadingChilds:=true;
      end
    else
      begin
      if assigned(FObjNode.NextSibling) then
        FCurNode := FObjNode.NextSibling
      else if assigned(FObjNode.ParentNode) then
        FObjNode := FObjNode.ParentNode;

      while assigned(FCurNode) and (FCurNode.NodeName<>'object') do
        FCurNode := FCurNode.NextSibling;
      end;
    Exit;
    end;

  if not FReadingChilds and (FCurNode.NodeName='property') then
    begin
    FCurValue := FCurNode.TextContent;
    if FCurNode.Attributes.GetNamedItem('type').NodeValue='int16' then
      result := vaInt16
    else if FCurNode.Attributes.GetNamedItem('type').NodeValue='int64' then
      result := vaInt32
    else if FCurNode.Attributes.GetNamedItem('type').NodeValue='string' then
      result := vaString
    else if FCurNode.Attributes.GetNamedItem('type').NodeValue='vatrue' then
      result := vaTrue
    else if FCurNode.Attributes.GetNamedItem('type').NodeValue='vafalse' then
      result := vaFalse
    else if FCurNode.Attributes.GetNamedItem('type').NodeValue='ident' then
      result := vaIdent
    else
      raise EReadError.CreateFmt('Unknown property type %s',[FCurNode.Attributes.GetNamedItem('type').NodeValue]);
    end;

  if FReadingChilds and (FCurNode.NodeName='object') then
    result := vaIdent;

  FCurNode := FCurNode.NextSibling;
  while assigned(FCurNode) do
    begin
    if FReadingChilds and (FCurNode.NodeName='object') then
      break;
    if not FReadingChilds and (FCurNode.NodeName='property') then
      break;
    FCurNode := FCurNode.NextSibling;
    end;
end;

procedure TXMLObjectReader.BeginRootComponent;
begin
  FXMLDoc.Free;

  ReadXMLFile(FXMLDoc, FStream);
  FCurNode := FXMLDoc.FindNode('object');
  if not assigned(FCurNode) then
    raise EReadError.Create('Invalid XML-stream format: No object node found');
end;

procedure TXMLObjectReader.BeginComponent(var Flags: TFilerFlags;
  var AChildPos: Integer; var CompClassName, CompName: String);
begin
  flags := [];
  FReadingChilds:=false;

  assert(FObjNode.NodeName='object');
  FObjNode:=FCurNode;
  CompName:=FObjNode.Attributes.GetNamedItem('name').NodeValue;
  CompClassName:=FObjNode.Attributes.GetNamedItem('type').NodeValue;
  FCurNode := FObjNode.FirstChild;
  while assigned(FCurNode) and (FCurNode.NodeName<>'property') do
    FCurNode := FCurNode.NextSibling;
end;

function TXMLObjectReader.BeginProperty: String;
begin
  if FCurNode.NodeName<>'property' then
    raise exception.create('property-element expected but found '+FCurNode.NodeName);
  result := FCurNode.Attributes.GetNamedItem('name').NodeValue;
end;

procedure TXMLObjectReader.Read(var Buf; Count: LongInt);
begin

end;

procedure TXMLObjectReader.ReadBinary(const DestData: TMemoryStream);
begin

end;

function TXMLObjectReader.ReadCurrency: Currency;
begin

end;

function TXMLObjectReader.ReadIdent(ValueType: TValueType): String;
begin
  result := FCurValue;
end;

function TXMLObjectReader.ReadInt8: ShortInt;
begin
  result := strtoint(FCurValue);
end;

function TXMLObjectReader.ReadInt16: SmallInt;
begin
  result := strtoint(FCurValue);
end;

function TXMLObjectReader.ReadInt32: LongInt;
begin
  result := strtoint(FCurValue);
end;

function TXMLObjectReader.ReadInt64: Int64;
begin
  result := StrToInt64(FCurValue);
end;

function TXMLObjectReader.ReadSet(EnumType: Pointer): Integer;
begin

end;

function TXMLObjectReader.ReadStr: String;
begin
  result := FCurValue;
end;

function TXMLObjectReader.ReadString(StringType: TValueType): String;
begin
  result := FCurValue;
end;

function TXMLObjectReader.ReadWideString: WideString;
begin
  result := FCurValue;
end;

function TXMLObjectReader.ReadUnicodeString: UnicodeString;
begin
  result := FCurValue;
end;

procedure TXMLObjectReader.SkipComponent(SkipComponentInfos: Boolean);
begin

end;

procedure TXMLObjectReader.SkipValue;
begin

end;

{ TXMLReader }

function TXMLReader.CreateDriver(Stream: TStream; BufSize: Integer): TAbstractObjectReader;
begin
  Result := TXMLObjectReader.Create(Stream);
end;

{ TXMLUnitResourcefileFormat }

class procedure TXMLUnitResourcefileFormat.QuickReadXML(s: TStream; out
  AComponentName, AClassName, ALCLVersion: string);
var
  AXMLDocument: TXMLDocument;
  ms: TStringStream;
  ObjNode: TDOMNode;
begin
  ReadXMLFile(AXMLDocument, s);
  try
    ObjNode := AXMLDocument.FindNode('lazarusinfo');
    if assigned(ObjNode) then
      begin
      ObjNode := ObjNode.FindNode('lclversion');
      if assigned(ObjNode) then
        ALCLVersion:=ObjNode.TextContent;
      end;

    ObjNode := AXMLDocument.FindNode('object');
    if not assigned(ObjNode) then
      raise EReadError.Create('Invalid XML-stream format: No object node found');
    AComponentName:=ObjNode.Attributes.GetNamedItem('name').NodeValue;
    AClassName:=ObjNode.Attributes.GetNamedItem('type').NodeValue;

  finally
    AXMLDocument.Free;
  end;
end;

class function TXMLUnitResourcefileFormat.FindResourceDirective(Source: TObject): boolean;
var
  cb: TCodeBuffer;
  nx,ny,nt: integer;
begin
//  result := CodeToolBoss.FindResourceDirective(Source as TCodeBuffer,1,1,cb,nx,ny,nt, ResourceDirectiveFilename,false);
end;

class function TXMLUnitResourcefileFormat.ResourceDirectiveFilename: string;
begin
  result := '*.xml';
end;

class function TXMLUnitResourcefileFormat.GetUnitResourceFilename(
  AUnitFilename: string; Loading: boolean): string;
begin
  result := ChangeFileExt(AUnitFilename,'.xml');
end;

class procedure TXMLUnitResourcefileFormat.TextStreamToBinStream(ATxtStream,
  ABinStream: TExtMemoryStream);
begin
  ABinStream.LoadFromStream(ATxtStream);
end;

class procedure TXMLUnitResourcefileFormat.BinStreamToTextStream(ABinStream,
  ATextStream: TExtMemoryStream);
begin
  ATextStream.LoadFromStream(ABinStream);
end;

class function TXMLUnitResourcefileFormat.GetClassNameFromStream(s: TStream;
  out IsInherited: Boolean): shortstring;
var
  AComponentName,
  AClassType,
  ALCLVersion: string;
begin
  IsInherited:=false;
  QuickReadXML(s, AComponentName, AClassType, ALCLVersion);
  s.Seek(0,soFromBeginning);
  result := AClassType;
end;

class function TXMLUnitResourcefileFormat.CreateReader(s: TStream;
  var DestroyDriver: boolean): TReader;
begin
  result := TXMLReader.Create(s,4096);
end;

class function TXMLUnitResourcefileFormat.CreateWriter(s: TStream;
  var DestroyDriver: boolean): TWriter;
var
  ADriver: TXMLObjectWriter;
begin
  ADriver:=TXMLObjectWriter.Create(s,4096);
  result := TWriter.Create(ADriver);
  DestroyDriver:=false;
end;

class function TXMLUnitResourcefileFormat.QuickCheckResourceBuffer(
  PascalBuffer, LFMBuffer: TObject; out LFMType, LFMComponentName,
  LFMClassName: string; out LCLVersion: string; out MissingClasses: TStrings
  ): TModalResult;
var
  ms: TStringStream;
begin
  Result:=mrOk;
  ms := TStringStream.Create((LFMBuffer as TCodeBuffer).Source);
  try
    QuickReadXML(ms, LFMComponentName, LFMClassName, LCLVersion);
  finally
    ms.Free;
  end;

  LFMType:='unknown';
  MissingClasses := nil;
end;

end.
