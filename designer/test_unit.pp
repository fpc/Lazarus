{

}
{$H+}
unit test_unit;

{$mode objfpc}

interface

uses
  Classes, StdCtrls, Forms, Buttons, Menus, ComCtrls, SysUtils, ExtCtrls,
  ObjectInspector, PropEdits, Graphics, TypInfo;

type
  TMyEnum = (MyEnum1,MyEnum2,MyEnum3);
  TMySet = set of TMyEnum;

  TMySubComponent = class(TComponent)
  public
    FMyInteger:integer;
  published
    property MyInteger:integer read FMyInteger write FMyInteger;
  end;

  TMyComponent = class(TComponent)
  public
    FMyInteger:integer;
    FMyCardinal:Cardinal;
    FMyInt64:int64;
    FMyEnum:TMyEnum;
    FMySet:TMySet;
    FMyFloat:extended;
    FMyAnsiString:AnsiString;
    FMyShortString:ShortString;
    FMyBool:boolean;
    FMySubComponent:TMySubComponent;
    FMyGraphicsObject:TGraphicsObject;
    FMyBrush:TBrush;
    FMyPen:TPen;
    FMyFont:TFont;
    FMyEvent:TNotifyEvent;
    procedure DoSomething(Sender:TObject);
    procedure SetMyAnsiString(const NewValue:AnsiString);
    procedure SetMyShortString(const NewValue:ShortString);
    constructor Create(AOwner:TComponent);  override;
    destructor Destroy;  override;
  published
    MySubComponent2:TMySubComponent;
    property MyInteger:integer read FMyInteger write FMyInteger;
    property MyCardinal:cardinal read FMyCardinal write FMyCardinal;
//    property MyInt64:int64 read FMyInt64 write FMyInt64;
    property MyEnum:TMyEnum read FMyEnum write FMyEnum;
    property MySet:TMySet read FMySet write FMySet;
    property MyFloat:Extended read FMyFloat write FMyFloat;
    property MyAnsiString:AnsiString read FMyAnsiString write SetMyAnsiString;
    property MyShortString:ShortString read FMyShortString write SetMyShortString;
    property MyBool:Boolean read FMyBool write FMyBool;
    property MySubComponent:TMySubComponent read FMySubComponent write FMySubComponent;
//    property MyGraphicsObject:TGraphicsObject read FMyGraphicsObject write FMyGraphicsObject;
//    property MyBrush:TBrush read FMyBrush write FMyBrush;
//    property MyPen:TPen read FMyPen write FMyPen;
//    property MyFont:TFont read FMyFont write FMyFont;
//    property MyEvent:TNotifyEvent read FMyEvent write FMyEvent;
  end;

	TForm1 = class(TFORM)
	public
	  Label1 : TLabel;
	  Label2 : TLabel;
	  Label3 : TLabel;
	  EditToComboButton: TButton;
	  AddItemButton: TButton;
	  ComboToEditButton: TButton;
	  SwitchEnabledButton: TButton;
	  DumpButton: TButton;
	  IndexButton: TButton;
    OIResizeButton: TButton;
    OIRefreshButton: TButton;
	  Edit1 : TEdit;
    ComboBox1 : TComboBox;
    ComboBox2 : TComboBox;
    Memo1 : TMemo;
    WriteLFMButton:TButton;
    constructor Create(AOwner: TComponent); override;
    procedure LoadMainMenu;
    procedure LoadFromLFM;
  published
	  procedure FormKill(Sender : TObject);
	  procedure FormShow(Sender : TObject);
	  procedure EditToComboButtonCLick(Sender : TObject);
	  procedure AddItemButtonCLick(Sender : TObject);
	  procedure ComboToEditButtonCLick(Sender : TObject);
	  procedure SwitchEnabledButtonCLick(Sender : TObject);
	  procedure DumpButtonCLick(Sender : TObject);
	  procedure IndexButtonCLick(Sender : TObject);
	  procedure OIResizeButtonCLick(Sender : TObject);
	  procedure OIRefreshButtonCLick(Sender : TObject);
	  procedure ComboOnChange (Sender:TObject);
	  procedure ComboOnClick (Sender:TObject);
    procedure WriteLFMButtonClick(Sender:TObject);
  private
    procedure ReaderFindMethod(Reader: TReader; const FindMethodName: Ansistring;
      var Address: Pointer; var Error: Boolean);
    procedure ReaderSetName(Reader: TReader; Component: TComponent;
      var NewName: Ansistring);
    procedure ReaderReferenceName(Reader: TReader; var RefName: Ansistring);
    procedure ReaderAncestorNotFound(Reader: TReader; const ComponentName: Ansistring;
      ComponentClass: TPersistentClass; var Component: TComponent);
    procedure ReaderError(Reader: TReader; const Message: Ansistring;
      var Handled: Boolean);
    procedure ReaderFindComponentClass(Reader: TReader; const FindClassName: Ansistring;
      var ComponentClass: TComponentClass);
    procedure ReaderCreateComponent(Reader: TReader;
      ComponentClass: TComponentClass; var Component: TComponent);
  public
    // some test variables
    FMyInteger:integer;
    FMyCardinal:Cardinal;
    FMyEnum:TMyEnum;
    FMySet:TMySet;
    FMyAnsiString:AnsiString;
    FMyShortString:ShortString;
    FMyBool:boolean;
    FMyBrush:TBrush;
    FMyPen:TPen;
    FMyFont:TFont;
    FMyComponent:TMyComponent;
    FMyEvent:TNotifyEvent;
    procedure SetMyAnsiString(const NewValue:AnsiString);
    procedure SetMyShortString(const NewValue:ShortString);
  published
    property MyInteger:integer read FMyInteger write FMyInteger;
    property MyCardinal:cardinal read FMyCardinal write FMyCardinal;
    property MyEnum:TMyEnum read FMyEnum write FMyEnum;
    property MySet:TMySet read FMySet write FMySet;
    property MyAnsiString:AnsiString read FMyAnsiString write SetMyAnsiString;
    property MyShortString:ShortString read FMyShortString write SetMyShortString;
    property MyBool:Boolean read FMyBool write FMyBool;
    //property MyBrush:TBrush read FMyBrush write FMyBrush;
    property MyPen:TPen read FMyPen write FMyPen;
    //property MyFont:TFont read FMyFont write FMyFont;
    //property MyComponent:TMyComponent read FMyComponent write FMyComponent;
    property MyEvent:TNotifyEvent read FMyEvent write FMyEvent;
	end;

  TMatBinaryObjectReader = class(TAbstractObjectReader)
  private
    FStream: TStream;
    FBuffer: Pointer;
    FBufSize: Integer;
    FBufPos: Integer;
    FBufEnd: Integer;
    procedure Read(var Buf; Count: LongInt);
    procedure SkipProperty;
    procedure SkipSetBody;
  public
    constructor Create(Stream: TStream; BufSize: Integer);
    destructor Destroy; override;

    function NextValue: TValueType; override;
    function ReadValue: TValueType; override;
    procedure BeginRootComponent; override;
    procedure BeginComponent(var Flags: TFilerFlags; var AChildPos: Integer;
      var CompClassName, CompName: AnsiString); override;
    function BeginProperty: AnsiString; override;

    procedure ReadBinary(const DestData: TMemoryStream); override;
    function ReadFloat: Extended; override;
    function ReadSingle: Single; override;
    {!!!: function ReadCurrency: Currency; override;}
    function ReadDate: TDateTime; override;
    function ReadIdent(ValueType: TValueType): AnsiString; override;
    function ReadInt8: ShortInt; override;
    function ReadInt16: SmallInt; override;
    function ReadInt32: LongInt; override;
    function ReadInt64: Int64; override;
    function ReadSet(EnumType: Pointer): Integer; override;
    function ReadStr: AnsiString; override;
    function ReadString(StringType: TValueType): AnsiString; override;
    procedure SkipComponent(SkipComponentInfos: Boolean); override;
    procedure SkipValue; override;
  end;

  TMatBinaryObjectWriter = class(TAbstractObjectWriter)
  private
    FStream: TStream;
    FBuffer: Pointer;
    FBufSize: Integer;
    FBufPos: Integer;
    FSignatureWritten: Boolean;
    procedure FlushBuffer;
    procedure Write(const Buffer; Count: Longint);
    procedure WriteValue(Value: TValueType);
    procedure WriteStr(const Value: AnsiString);
  public
    constructor Create(Stream: TStream; BufSize: Integer);
    destructor Destroy; override;

    procedure BeginCollection; override;
    procedure BeginComponent(Component: TComponent; Flags: TFilerFlags;
      ChildPos: Integer); override;
    procedure BeginList; override;
    procedure EndList; override;
    procedure BeginProperty(const PropName: AnsiString); override;
    procedure EndProperty; override;

    procedure WriteBinary(const Buffer; Count: LongInt); override;
    procedure WriteBoolean(Value: Boolean); override;
    procedure WriteFloat(const Value: Extended); override;
    procedure WriteSingle(const Value: Single); override;
    {!!!: procedure WriteCurrency(const Value: Currency);  override;}
    procedure WriteDate(const Value: TDateTime); override;
    procedure WriteIdent(const Ident: Ansistring); override;
    procedure WriteInteger(Value: Int64); override;
    procedure WriteMethodName(const Name: AnsiString); override;
    procedure WriteSet(Value: LongInt; SetType: Pointer); override;
    procedure WriteString(const Value: AnsiString); override;
  end;


var
  Form1 : TForm1;
  OI: TObjectInspector;

implementation

//==============================================================================



{ TMyComponent }

constructor TMyComponent.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  Name:='MyComponent';
  FMyInteger:=-1234;
  FMyCardinal:=5678;
  FMySet:=[MyEnum1];
  FMyEnum:=MyEnum2;
  FMyFloat:=3.2;
  FMyBool:=true;
  FMyAnsiString:='Ansi';
  FMyShortString:='Short';
  FMySubComponent:=TMySubComponent.Create(Self);
  with FMySubComponent do begin
    MyInteger:=789;
  end;
  FMyGraphicsObject:=nil;
  FMyFont:=TFont.Create;
  FMyBrush:=TBrush.Create;
  FMyPen:=TPen.Create;
  FMyEvent:=@DoSomething;

  MySubComponent2:=TMySubComponent.Create(Self);
  with MySubComponent2 do begin
    MyInteger:=1928;
  end;
end;

destructor TMyComponent.Destroy;
begin
  FMyPen.Free;
  FMyBrush.Free;
  FMyFont.Free;
  FMySubComponent.Free;
  inherited Destroy;
end;

procedure TMyComponent.SetMyAnsiString(const NewValue:AnsiString);
begin
  FMyAnsiString:=NewValue;
end;

procedure TMyComponent.SetMyShortString(const NewValue:ShortString);
begin
  FMyShortString:=NewValue;
end;

procedure TMyComponent.DoSomething(Sender:TObject);
begin
  //
end;


//==============================================================================


procedure ObjectBinaryToText(Input, Output: TStream);

  procedure OutStr(s: String);
  begin
    writeln('OutStr '''+s+'''');
    if Length(s) > 0 then
      Output.Write(s[1], Length(s));
  end;

  procedure OutLn(s: String);
  begin
    OutStr(s + #10);
  end;

  procedure OutString(s: String);
  var
    res, NewStr: String;
    i: Integer;
    InString, NewInString: Boolean;
  begin
    writeln('OutString '''+s+'''');
    res := '';
    InString := False;
    for i := 1 to Length(s) do begin
      NewInString := InString;
      case s[i] of
        #0..#31: begin
            if InString then
              NewInString := False;
            NewStr := '#' + IntToStr(Ord(s[i]));
          end;
        '''':
            if InString then NewStr := ''''''
            else NewStr := '''''''';
        else begin
          if not InString then
            NewInString := True;
          NewStr := s[i];
        end;
      end;
      if NewInString <> InString then begin
        NewStr := '''' + NewStr;
        InString := NewInString;
      end;
      res := res + NewStr;
    end;
    if InString then res := res + '''';
    OutStr(res);
  end;

  function ReadInt(ValueType: TValueType): LongInt;
  begin
    writeln('ReadInt(ValueType)');
    case ValueType of
      vaInt8: Result := ShortInt(Input.ReadByte);
      vaInt16: Result := SmallInt(Input.ReadWord);
      vaInt32: Result := LongInt(Input.ReadDWord);
    end;
  end;

  function ReadInt: LongInt;
  begin
    writeln('ReadInt');
    Result := ReadInt(TValueType(Input.ReadByte));
  end;

  function ReadSStr: String;
  var
    len: Byte;
  begin
    writeln('ReadStr');
    len := Input.ReadByte;
    SetLength(Result, len);
    Input.Read(Result[1], len);
  end;

  procedure ReadPropList(indent: String);

    procedure ProcessValue(ValueType: TValueType; Indent: String);

      procedure Stop(s: String);
      begin
        WriteLn(s);
        Halt;
      end;

      procedure ProcessBinary;
      var
        ToDo, DoNow, i: LongInt;
        lbuf: array[0..31] of Byte;
        s: String;
      begin
        ToDo := Input.ReadDWord;
        OutLn('{');
        while ToDo > 0 do begin
          DoNow := ToDo;
          if DoNow > 32 then DoNow := 32;
          Dec(ToDo, DoNow);
          s := Indent + '  ';
          Input.Read(lbuf, DoNow);
          for i := 0 to DoNow - 1 do
            s := s + IntToHex(lbuf[i], 2);
          OutLn(s);
        end;
        OutLn(indent + '}');
      end;

    var
      s: String;
      //len: LongInt;
      IsFirst: Boolean;
      ext: Extended;

    begin
    writeln('ProcessValue Indent='''+Indent+'''');
//      OutStr('(' + IntToStr(Ord(Valuetype)) + ') ');
      case ValueType of
        vaList: begin
            OutStr('(');
            IsFirst := True;
            while True do begin
              ValueType := TValueType(Input.ReadByte);
              if ValueType = vaNull then break;
              if IsFirst then begin
                OutLn('');
                IsFirst := False;
              end;
              OutStr(Indent + '  ');
              ProcessValue(ValueType, Indent + '  ');
            end;
            OutLn(Indent + ')');
          end;
        vaInt8: OutLn(IntToStr(ShortInt(Input.ReadByte)));
        vaInt16: OutLn( IntToStr(SmallInt(Input.ReadWord)));
        vaInt32: OutLn(IntToStr(LongInt(Input.ReadDWord)));
        vaExtended: begin
            Input.Read(ext, SizeOf(ext));
            OutLn(FloatToStr(ext));
          end;
        vaString: begin
            OutString(ReadSStr);
            OutLn('');
          end;
        vaIdent: OutLn(ReadSStr);
        vaFalse: OutLn('False');
        vaTrue: OutLn('True');
        vaBinary: ProcessBinary;
        vaSet: begin
            OutStr('[');
            IsFirst := True;
            while True do begin
              s := ReadSStr;
              if Length(s) = 0 then break;
              if not IsFirst then OutStr(', ');
              IsFirst := False;
              OutStr(s);
            end;
            OutLn(']');
          end;
        vaLString: Stop('!!LString!!');
        vaNil: OutLn('nil'); // Stop('nil');
        vaCollection: begin
            OutStr('<');
            while Input.ReadByte <> 0 do begin
              OutLn(Indent);
              Input.Seek(-1, soFromCurrent);
              OutStr(indent + '  item');
              ValueType := TValueType(Input.ReadByte);
              if ValueType <> vaList then
                OutStr('[' + IntToStr(ReadInt(ValueType)) + ']');
              OutLn('');
              ReadPropList(indent + '    ');
              OutStr(indent + '  end');
            end;
            OutLn('>');
          end;
        {vaSingle: begin OutLn('!!Single!!'); exit end;
        vaCurrency: begin OutLn('!!Currency!!'); exit end;
        vaDate: begin OutLn('!!Date!!'); exit end;
        vaWString: begin OutLn('!!WString!!'); exit end;}
        else
          Stop(IntToStr(Ord(ValueType)));
      end;
    end;

  begin
    writeln('ReadPropList');
    while Input.ReadByte <> 0 do begin
      Input.Seek(-1, soFromCurrent);
      OutStr(indent + ReadSStr + ' = ');
      ProcessValue(TValueType(Input.ReadByte), Indent);
    end;
  end;

  procedure ReadObject(indent: String);
  var
    b: Byte;
    ObjClassName, ObjName: String;
    ChildPos: LongInt;
  begin
    writeln('ReadObject');
    // Check for FilerFlags
    b := Input.ReadByte;
    if (b and $f0) = $f0 then begin
      if (b and 2) <> 0 then ChildPos := ReadInt;
    end else begin
      b := 0;
      Input.Seek(-1, soFromCurrent);
    end;

    ObjClassName := ReadSStr;
    ObjName := ReadSStr;

    OutStr(Indent);
    if (b and 1) <> 0 then OutStr('inherited')
    else OutStr('object');
    OutStr(' ');
    if ObjName <> '' then
      OutStr(ObjName + ': ');
    OutStr(ObjClassName);
    if (b and 2) <> 0 then OutStr('[' + IntToStr(ChildPos) + ']');
    OutLn('');

    ReadPropList(indent + '  ');

    while Input.ReadByte <> 0 do begin
      Input.Seek(-1, soFromCurrent);
      ReadObject(indent + '  ');
    end;
    OutLn(indent + 'end');
  end;

type
  PLongWord = ^LongWord;
const
  signature: PChar = 'TPF0';
begin
  if Input.ReadDWord <> PLongWord(Pointer(signature))^ then
    raise EReadError.Create('Illegal stream image' {###SInvalidImage});
  ReadObject('');
end;



//==============================================================================



procedure ObjectTextToBinary(Input, Output: TStream);
var
  parser: TParser;

  procedure WriteString(s: String);
  begin
    writeln('OTTB: WriteStr '''+s+'''');
    Output.WriteByte(Length(s));
    Output.Write(s[1], Length(s));
  end;

  procedure WriteInteger(value: LongInt);
  begin
    writeln('OTTB: WriteInteger '+IntToStr(Value));
    if (value >= -128) and (value <= 127) then begin
      Output.WriteByte(Ord(vaInt8));
      Output.WriteByte(Byte(value));
    end else if (value >= -32768) and (value <= 32767) then begin
      Output.WriteByte(Ord(vaInt16));
      Output.WriteWord(Word(value));
    end else begin
      Output.WriteByte(ord(vaInt32));
      Output.WriteDWord(LongWord(value));
    end;
  end;

  procedure ProcessProperty; forward;

  procedure ProcessValue;
  var
    flt: Extended;
    s: String;
    stream: TMemoryStream;
  begin
    writeln('OTTB: ProcessValue');
    case parser.Token of
      toInteger:
        begin
          WriteInteger(parser.TokenInt);
          parser.NextToken;
        end;
      toFloat:
        begin
          Output.WriteByte(Ord(vaExtended));
          flt := Parser.TokenFloat;
          Output.Write(flt, SizeOf(flt));
          parser.NextToken;
        end;
      toString:
        begin
          s := parser.TokenString;
          while parser.NextToken = '+' do
          begin
            parser.NextToken;   // Get next string fragment
            parser.CheckToken(toString);
            s := s + parser.TokenString;
          end;
          Output.WriteByte(Ord(vaString));
          WriteString(s);
        end;
      toSymbol:
        begin
          if CompareText(parser.TokenString, 'True') = 0 then
            Output.WriteByte(Ord(vaTrue))
          else if CompareText(parser.TokenString, 'False') = 0 then
            Output.WriteByte(Ord(vaFalse))
          else if CompareText(parser.TokenString, 'nil') = 0 then
            Output.WriteByte(Ord(vaNil))
          else
          begin
            Output.WriteByte(Ord(vaIdent));
            WriteString(parser.TokenString);
          end;
          Parser.NextToken;
        end;
      // Set
      '[':
        begin
          parser.NextToken;
          Output.WriteByte(Ord(vaSet));
          if parser.Token <> ']' then
            while True do
            begin
              parser.CheckToken(toSymbol);
              WriteString(parser.TokenString);
              parser.NextToken;
              if parser.Token = ']' then
                break;
              parser.CheckToken(',');
              parser.NextToken;
            end;
          Output.WriteByte(0);
          parser.NextToken;
        end;
      // List
      '(':
        begin
          parser.NextToken;
          Output.WriteByte(Ord(vaList));
          while parser.Token <> ')' do
            ProcessValue;
          Output.WriteByte(0);
          parser.NextToken;
        end;
      // Collection
      '<':
        begin
          parser.NextToken;
          Output.WriteByte(Ord(vaCollection));
          while parser.Token <> '>' do
          begin
            parser.CheckTokenSymbol('item');
            parser.NextToken;
            // ConvertOrder
            Output.WriteByte(Ord(vaList));
            while not parser.TokenSymbolIs('end') do
              ProcessProperty;
            parser.NextToken;   // Skip 'end'
            Output.WriteByte(0);
          end;
          Output.WriteByte(0);
          parser.NextToken;
        end;
      // Binary data
      '{':
        begin
          Output.WriteByte(Ord(vaBinary));
          stream := TMemoryStream.Create;
          try
            parser.HexToBinary(stream);
            Output.WriteDWord(stream.Size);
            Output.Write(Stream.Memory^, stream.Size);
          finally
            stream.Free;
          end;
          parser.NextToken;
        end;
      else
        begin
          writeln('Error: Invalid property');
          halt;
          //parser.Error(SInvalidProperty);
        end;
    end;
  end;

  procedure ProcessProperty;
  var
    name: String;
  begin
    writeln('OTTB: ProcessProperty');
    // Get name of property
    parser.CheckToken(toSymbol);
    name := parser.TokenString;
    while True do begin
      parser.NextToken;
      if parser.Token <> '.' then break;
      parser.NextToken;
      parser.CheckToken(toSymbol);
      name := name + '.' + parser.TokenString;
    end;
    WriteString(name);
    parser.CheckToken('=');
    parser.NextToken;
    ProcessValue;
  end;

  procedure ProcessObject;
  var
    IsInherited: Boolean;
    ObjectName, ObjectType: String;
  begin
    writeln('OTTB: ProcessObject');
    if parser.TokenSymbolIs('OBJECT') then
      IsInherited := False
    else begin
      parser.CheckTokenSymbol('INHERITED');
      IsInherited := True;
    end;
    parser.NextToken;
    parser.CheckToken(toSymbol);
    ObjectName := '';
    ObjectType := parser.TokenString;
    parser.NextToken;
    if parser.Token = ':' then begin
      parser.NextToken;
      parser.CheckToken(toSymbol);
      ObjectName := ObjectType;
      ObjectType := parser.TokenString;
      parser.NextToken;
    end;
    WriteString(ObjectType);
    WriteString(ObjectName);

    // Convert property list
    while not (parser.TokenSymbolIs('END') or
      parser.TokenSymbolIs('OBJECT') or
      parser.TokenSymbolIs('INHERITED')) do
      ProcessProperty;
    Output.WriteByte(0);        // Terminate property list

    // Convert child objects
    while not parser.TokenSymbolIs('END') do ProcessObject;
    parser.NextToken;           // Skip end token
    Output.WriteByte(0);        // Terminate property list
  end;

const
  signature: PChar = 'TPF0';
begin
  parser := TParser.Create(Input);
  try
    Output.Write(signature[0], 4);
    ProcessObject;
  finally
    parser.Free;
  end;
end;



//==============================================================================


{ TMatBinaryObjectReader }

constructor TMatBinaryObjectReader.Create(Stream: TStream; BufSize: Integer);
begin
  writeln('MBOR: Create');
  inherited Create;
  FStream := Stream;
  FBufSize := BufSize;
  GetMem(FBuffer, BufSize);
end;

destructor TMatBinaryObjectReader.Destroy;
begin
  writeln('MBOR: Destroy');
  { Seek back the amount of bytes that we didn't process unitl now: }
  FStream.Seek(Integer(FBufPos) - Integer(FBufEnd), soFromCurrent);

  if Assigned(FBuffer) then
    FreeMem(FBuffer, FBufSize);

  inherited Destroy;
end;

function TMatBinaryObjectReader.ReadValue: TValueType;
begin
  writeln('MBOR: ReadValue');
  Result := vaNull; { Necessary in FPC as TValueType is larger than 1 byte! }
  Read(Result, 1);
end;

function TMatBinaryObjectReader.NextValue: TValueType;
begin
  writeln('MBOR: NextValue');
  Result := ReadValue;
  { We only 'peek' at the next value, so seek back to unget the read value: }
  Dec(FBufPos);
end;

procedure TMatBinaryObjectReader.BeginRootComponent;
var
  Signature: LongInt;
begin
  writeln('MBOR: BeginRootComponent');
  { Read filer signature }
  Read(Signature, 4);
  if Signature <> LongInt(FilerSignature) then
    raise EReadError.Create('SInvalidImage');
    //raise EReadError.Create(SInvalidImage);
end;

procedure TMatBinaryObjectReader.BeginComponent(var Flags: TFilerFlags;
  var AChildPos: Integer; var CompClassName, CompName: AnsiString);
var
  Prefix: Byte;
  ValueType: TValueType;
begin
  writeln('MBOR: BeginComponent');
  { Every component can start with a special prefix: }
  Flags := [];
  if (Byte(NextValue) and $f0) = $f0 then
  begin
    Prefix := Byte(ReadValue);
    Flags := TFilerFlags(Prefix and $0f);
    if ffChildPos in Flags then
    begin
      ValueType := NextValue;
      case ValueType of
        vaInt8:
	  AChildPos := ReadInt8;
	vaInt16:
	  AChildPos := ReadInt16;
        vaInt32:
	  AChildPos := ReadInt32;
	else
	  //raise EReadError.Create(SInvalidPropertyValue);
	  raise EReadError.Create('SInvalidPropertyValue');
      end;
    end;
  end;

  CompClassName := ReadStr;
  CompName := ReadStr;
  writeln('MBOR: BeginComponent! '''+CompClassName+''','''+CompName+'''');
end;

function TMatBinaryObjectReader.BeginProperty: AnsiString;
begin
  writeln('MBOR: BeginProperty');
  Result := ReadStr;
  writeln('MBOR: BeginProperty! '''+Result+'''');
end;

procedure TMatBinaryObjectReader.ReadBinary(const DestData: TMemoryStream);
var
  BinSize: LongInt;
begin
  writeln('MBOR: ReadBinary');
  Read(BinSize, 4);
  DestData.Size := BinSize;
  Read(DestData.Memory^, BinSize);
end;

function TMatBinaryObjectReader.ReadFloat: Extended;
begin
  writeln('MBOR: ReadFloat');
  Read(Result, SizeOf(Extended));
  writeln('MBOR: ReadFloat! '+FloatToStr(Result));
end;

function TMatBinaryObjectReader.ReadSingle: Single;
begin
  writeln('MBOR: ReadSingle');
  Read(Result, SizeOf(Single))
end;

{!!!: function TMatBinaryObjectReader.ReadCurrency: Currency;
begin
  writeln('MBOR: ReadCurrency');
  Read(Result, SizeOf(Currency))
end;}

function TMatBinaryObjectReader.ReadDate: TDateTime;
begin
  writeln('MBOR: ReadDate');
  Read(Result, SizeOf(TDateTime))
end;

function TMatBinaryObjectReader.ReadIdent(ValueType: TValueType): AnsiString;
var
  i: Byte;
begin
  writeln('MBOR: ReadIdent');
  case ValueType of
    vaIdent:
      begin
        Read(i, 1);
	SetLength(Result, i);
        Read(Pointer(@Result[1])^, i);
      end;
    vaNil:
      Result := 'nil';
    vaFalse:
      Result := 'False';
    vaTrue:
      Result := 'True';
    vaNull:
      Result := 'Null';
  end;
  writeln('MBOR: ReadIdent! '''+Result+'''');
end;

function TMatBinaryObjectReader.ReadInt8: ShortInt;
begin
  Read(Result, 1);
  writeln('MBOR: ReadInt8 '+IntToStr(Result));
end;

function TMatBinaryObjectReader.ReadInt16: SmallInt;
begin
  Read(Result, 2);
  writeln('MBOR: ReadInt16'+IntToStr(Result));
end;

function TMatBinaryObjectReader.ReadInt32: LongInt;
begin
  Read(Result, 4);
  writeln('MBOR: ReadInt32 '+IntToStr(Result));
end;

function TMatBinaryObjectReader.ReadInt64: Int64;
begin
  Read(Result, 8);
  writeln('MBOR: ReadInt64 '+IntToStr(Result));
end;

function TMatBinaryObjectReader.ReadSet(EnumType: Pointer): Integer;
var
  Name: String;
  Value: Integer;
begin
  writeln('MBOR: ReadSet');
  try
    while True do
    begin
      Name := ReadStr;
      if Length(Name) = 0 then
        break;
      Value := GetEnumValue(PTypeInfo(EnumType), Name);
      if Value = -1 then
        //raise EReadError.Create(SInvalidPropertyValue);
        raise EReadError.Create('SInvalidPropertyValue');
      Result := Result or Value;
    end;
  except
    SkipSetBody;
    raise;
  end;
  writeln('MBOR: ReadSet! '+IntToStr(Result));
end;

function TMatBinaryObjectReader.ReadStr: AnsiString;
var
  i: Byte;
begin
  writeln('MBOR: ReadStr');
  Read(i, 1);
  SetLength(Result, i);
  Read(Pointer(@Result[1])^, i);
  writeln('MBOR: ReadStr! '''+Result+'''');
end;

function TMatBinaryObjectReader.ReadString(StringType: TValueType): AnsiString;
var
  i: Integer;
begin
  writeln('MBOR: ReadString');
  case StringType of
    vaString:
      begin
        i := 0;
        Read(i, 1);
      end;
    vaLString:
      Read(i, 4);
  end;
  SetLength(Result, i);
  if i > 0 then
    Read(Pointer(@Result[1])^, i);
  writeln('MBOR: ReadString! '''+Result+'''');
end;

{!!!: function TMatBinaryObjectReader.ReadWideString: WideString;
var
  i: Integer;
begin
  writeln('MBOR: ReadWideString');
  FDriver.Read(i, 4);
  SetLength(Result, i);
  if i > 0 then
    Read(PWideChar(Result), i * 2);
end;}

procedure TMatBinaryObjectReader.SkipComponent(SkipComponentInfos: Boolean);
var
  Flags: TFilerFlags;
  Dummy: Integer;
  CompClassName, CompName: AnsiString;
begin
  writeln('MBOR: SkipComponent Infos=',SkipComponentInfos);
  if SkipComponentInfos then
    { Skip prefix, component class name and component object name }
    BeginComponent(Flags, Dummy, CompClassName, CompName);

  { Skip properties }
  while NextValue <> vaNull do
    SkipProperty;
  ReadValue;

  { Skip children }
  while NextValue <> vaNull do
    SkipComponent(True);
  ReadValue;
end;

procedure TMatBinaryObjectReader.SkipValue;

  procedure SkipBytes(Count: LongInt);
  var
    Dummy: array[0..1023] of Byte;
    SkipNow: Integer;
  begin
    while Count > 0 do
    begin
      if Count > 1024 then
        SkipNow := 1024
      else
        SkipNow := Count;
      Read(Dummy, SkipNow);
      Dec(Count, SkipNow);
    end;
  end;

var
  Count: LongInt;
begin
  writeln('MBOR: SkipValue');
  case ReadValue of
    vaNull, vaFalse, vaTrue, vaNil: ;
    vaList:
      begin
        while NextValue <> vaNull do
          SkipValue;
        ReadValue;
      end;
    vaInt8:
      SkipBytes(1);
    vaInt16:
      SkipBytes(2);
    vaInt32:
      SkipBytes(4);
    vaExtended:
      SkipBytes(SizeOf(Extended));
    vaString, vaIdent:
      ReadStr;
    vaBinary, vaLString, vaWString:
      begin
        Read(Count, 4);
        SkipBytes(Count);
      end;
    vaSet:
      SkipSetBody;
    vaCollection:
      begin
        while NextValue <> vaNull do
        begin
	  { Skip the order value if present }
          if NextValue in [vaInt8, vaInt16, vaInt32] then
	    SkipValue;
          SkipBytes(1);
          while NextValue <> vaNull do
	          SkipProperty;
	        ReadValue;
        end;
	      ReadValue;
      end;
    vaSingle:
      SkipBytes(Sizeof(Single));
    {!!!: vaCurrency:
      SkipBytes(SizeOf(Currency));}
    vaDate:
      SkipBytes(Sizeof(TDateTime));
    vaInt64:
      SkipBytes(8);
  end;
end;

{ private methods }

procedure TMatBinaryObjectReader.Read(var Buf; Count: LongInt);
var
  CopyNow: LongInt;
  Dest: Pointer;
begin
  writeln('MBOR: Read Count='+IntToStr(Count));
  Dest := @Buf;
  while Count > 0 do
  begin
    if FBufPos >= FBufEnd then
    begin
      FBufEnd := FStream.Read(FBuffer^, FBufSize);
      if FBufEnd = 0 then
        //raise EReadError.Create(SReadError);
        raise EReadError.Create('SReadError');
      FBufPos := 0;
    end;
    CopyNow := FBufEnd - FBufPos;
    if CopyNow > Count then
      CopyNow := Count;
    Move(PChar(FBuffer)[FBufPos], Dest^, CopyNow);
    Inc(FBufPos, CopyNow);
    Inc(Dest, CopyNow);
    Dec(Count, CopyNow);
  end;
end;

procedure TMatBinaryObjectReader.SkipProperty;
begin
  writeln('MBOR: SkipProperty');
  { Skip property name, then the property value }
  ReadStr;
  SkipValue;
end;

procedure TMatBinaryObjectReader.SkipSetBody;
begin
  writeln('MBOR: SkipSetBody');
  while Length(ReadStr) > 0 do;
end;



{ TMatBinaryObjectWriter }

constructor TMatBinaryObjectWriter.Create(Stream: TStream; BufSize: Integer);
begin
writeln('MBOW: Create');
  inherited Create;
  FStream := Stream;
  FBufSize := BufSize;
  GetMem(FBuffer, BufSize);
end;

destructor TMatBinaryObjectWriter.Destroy;
begin
writeln('MBOW: Destroy');
  // Flush all data which hasn't been written yet
  FlushBuffer;

  if Assigned(FBuffer) then
    FreeMem(FBuffer, FBufSize);

  inherited Destroy;
end;

procedure TMatBinaryObjectWriter.BeginCollection;
begin
writeln('MBOW: BeginCollection');
  WriteValue(vaCollection);
end;

procedure TMatBinaryObjectWriter.BeginComponent(Component: TComponent;
  Flags: TFilerFlags; ChildPos: Integer);
var
  Prefix: Byte;
begin
writeln('MBOW: BeginComponent');
  if not FSignatureWritten then
  begin
    Write(FilerSignature, SizeOf(FilerSignature));
    FSignatureWritten := True;
  end;

  { Only write the flags if they are needed! }
  if Flags <> [] then
  begin
    Prefix := Integer(Flags) or $f0;
    Write(Prefix, 1);
    if ffChildPos in Flags then
      WriteInteger(ChildPos);
  end;

  WriteStr(Component.ClassName);
  WriteStr(Component.Name);
writeln('MBOW: BeginComponent end');
end;

procedure TMatBinaryObjectWriter.BeginList;
begin
writeln('MBOW: BeginList');
  WriteValue(vaList);
end;

procedure TMatBinaryObjectWriter.EndList;
begin
writeln('MBOW: EndList');
  WriteValue(vaNull);
end;

procedure TMatBinaryObjectWriter.BeginProperty(const PropName: AnsiString);
begin
writeln('MBOW: BeginProperty '+PropName);
  WriteStr(PropName);
end;

procedure TMatBinaryObjectWriter.EndProperty;
begin
writeln('MBOW: EndProperty');
end;

procedure TMatBinaryObjectWriter.WriteBinary(const Buffer; Count: LongInt);
begin
writeln('MBOW: WriteBinary');
  WriteValue(vaBinary);
  Write(Count, 4);
  Write(Buffer, Count);
end;

procedure TMatBinaryObjectWriter.WriteBoolean(Value: Boolean);
begin
writeln('MBOW: WriteBoolean');
  if Value then
    WriteValue(vaTrue)
  else
    WriteValue(vaFalse);
end;

procedure TMatBinaryObjectWriter.WriteFloat(const Value: Extended);
begin
writeln('MBOW: WriteFloat');
  WriteValue(vaExtended);
  Write(Value, SizeOf(Value));
end;

procedure TMatBinaryObjectWriter.WriteSingle(const Value: Single);
begin
writeln('MBOW: WriteSingle');
  WriteValue(vaSingle);
  Write(Value, SizeOf(Value));
end;

{!!!: procedure TMatBinaryObjectWriter.WriteCurrency(const Value: Currency);
begin
  WriteValue(vaCurrency);
  Write(Value, SizeOf(Value));
end;}

procedure TMatBinaryObjectWriter.WriteDate(const Value: TDateTime);
begin
writeln('MBOW: WriteDate');
  WriteValue(vaDate);
  Write(Value, SizeOf(Value));
end;

procedure TMatBinaryObjectWriter.WriteIdent(const Ident: Ansistring);
begin
writeln('MBOW: WriteIdent '+Ident);
  { Check if Ident is a special identifier before trying to just write
    Ident directly }
  if UpperCase(Ident) = 'NIL' then
    WriteValue(vaNil)
  else if UpperCase(Ident) = 'FALSE' then
    WriteValue(vaFalse)
  else if UpperCase(Ident) = 'TRUE' then
    WriteValue(vaTrue)
  else if UpperCase(Ident) = 'NULL' then
    WriteValue(vaNull) else
  begin
    WriteValue(vaIdent);
    WriteStr(Ident);
  end;
end;

procedure TMatBinaryObjectWriter.WriteInteger(Value: Int64);
begin
writeln('MBOW: WriteInteger '+IntToStr(Value));
  { Use the smallest possible integer type for the given value: }
  if (Value >= -128) and (Value <= 127) then
  begin
    WriteValue(vaInt8);
    Write(Value, 1);
  end else if (Value >= -32768) and (Value <= 32767) then
  begin
    WriteValue(vaInt16);
    Write(Value, 2);
  end else if (Value >= -$80000000) and (Value <= $7fffffff) then
  begin
    WriteValue(vaInt32);
    Write(Value, 4);
  end else
  begin
    WriteValue(vaInt64);
    Write(Value, 8);
  end;
end;

procedure TMatBinaryObjectWriter.WriteMethodName(const Name: AnsiString);
begin
writeln('MBOW: WriteMethodName '+Name);
  if Length(Name) > 0 then
  begin
    WriteValue(vaIdent);
    WriteStr(Name);
  end else
    WriteValue(vaNil);
end;

procedure TMatBinaryObjectWriter.WriteSet(Value: LongInt; SetType: Pointer);
var
  i: Integer;
  Mask: LongInt;
begin
writeln('MBOW: WriteSet');
  WriteValue(vaSet);
  Mask := 1;
  for i := 0 to 31 do
  begin
    if (Value and Mask) <> 0 then
      WriteStr(GetEnumName(PTypeInfo(SetType), i));
    Mask := Mask shl 1;
  end;
  WriteStr('');
end;

procedure TMatBinaryObjectWriter.WriteString(const Value: AnsiString);
var
  i: Integer;
begin
writeln('MBOW: WriteString '''+Value+'''');
  i := Length(Value);
  if i <= 255 then
  begin
    WriteValue(vaString);
    Write(i, 1);
  end else
  begin
    WriteValue(vaLString);
    Write(i, 4);
  end;
  if i > 0 then
    Write(Value[1], i);
end;

{!!!: procedure TMatBinaryObjectWriter.WriteWideString(const Value: WideString);
var
  i: Integer;
begin
  WriteValue(vaWString);
  i := Length(Value);
  Write(i, 4);
  Write(Value[1], i * 2);
end;}

procedure TMatBinaryObjectWriter.FlushBuffer;
begin
writeln('MBOW: FlushBuffer');
  FStream.WriteBuffer(FBuffer^, FBufPos);
  FBufPos := 0;
end;

procedure TMatBinaryObjectWriter.Write(const Buffer; Count: LongInt);
var
  CopyNow: LongInt;
begin
writeln('MBOW: Write (Count='+IntToStr(Count)+')');
  while Count > 0 do begin
    CopyNow := Count;
    if CopyNow > FBufSize - FBufPos then
      CopyNow := FBufSize - FBufPos;
    Move(Buffer, PChar(FBuffer)[FBufPos], CopyNow);
    Dec(Count, CopyNow);
    Inc(FBufPos, CopyNow);
    if FBufPos = FBufSize then
      FlushBuffer;
  end;
end;

procedure TMatBinaryObjectWriter.WriteValue(Value: TValueType);
begin
writeln('MBOW: WriteValue');
  Write(Value, 1);
end;

procedure TMatBinaryObjectWriter.WriteStr(const Value: AnsiString);
var
  i: Integer;
begin
writeln('MBOW: WriteStr '''+Value+'''');
// Mattias: What about strings > 255 ?
//     Delphi does it the same, but how can this work ?
  i := Length(Value);
  if i > 255 then
    i := 255;
  Write(i, 1);
  if i > 0 then
    Write(Value[1], i);
end;


//==============================================================================

{ TForm1 }

procedure TForm1.SetMyAnsiString(const NewValue:AnsiString);
begin
  FMyAnsiString:=NewValue;
end;

procedure TForm1.SetMyShortString(const NewValue:ShortString);
begin
  FMyShortString:=NewValue;
end;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMySet:=[MyEnum1];
  FMyEnum:=MyEnum2;
  FMyFont:=TFont.Create;
  FMyBrush:=TBrush.Create;
  FMyPen:=TPen.Create;
  FMyComponent:=TMyComponent.Create(nil);
  with FMyComponent do begin
    Name:='FMyComponent';
  end;
  if FileExists(ClassName+'.lfm') then
    LoadFromLFM
  else begin
    Name:='Form1';
    Caption:='Test Form';
    FMyEvent:=@WriteLFMButtonClick;
    OnShow:=@FormShow;
    Left:=250;
    Top:=50;
    LoadMainMenu;
    ActiveControl:=Label1;
    Show;
  end;
  if OI=nil then begin
    OI:=TObjectInspector.Create(Application);
    with OI do begin
      Name:='ObjectInspector';
      SetBounds(7,50,220,700);
      RootComponent:=Self;
      Show;
    end;
  end;
end;

procedure TForm1.FormKill(Sender : TObject);
Begin
  FMyComponent.Free;
  FMyBrush.Free;
  FMyPen.Free;
  FMyFont.Free;
  Application.Terminate;
End;

procedure TForm1.WriteLFMButtonClick(Sender:TObject);
// demonstration of LFM and LFC files
// - streams current form to binary format (BinStream)
// - transforms binary format to text format (TmpTxtStream)
// - transforms text format back to binary format (TmpBinStream)
// - transforms binary format back to txtstream and save LFM file (TxtStream)
var BinStream,TmpTxtStream,TmpBinStream:TMemoryStream;
  Driver: TAbstractObjectWriter;
  Writer:TWriter;
  TxtStream:TFileStream;
begin
  BinStream:=TMemoryStream.Create;
  TmpTxtStream:=TMemoryStream.Create;
  TmpBinStream:=TMemoryStream.Create;
  try
    Driver:=TMatBinaryObjectWriter.Create(BinStream,4096);
    try
      Writer:=TWriter.Create(Driver);
      try
        Writer.WriteDescendent(Self,nil);
      finally
        Writer.Free;
      end;
    finally
      Driver.Free;
    end;
    // transform binary to text and save LFM file
    TxtStream:=TFileStream.Create(ClassName+'.lfm',fmCreate);
    try
      BinStream.Position:=0;
      ObjectBinaryToText(BinStream,TxtStream);
    finally
      TxtStream.Free;
    end;
    // demonstrate transformation of text back to binary
    writeln('');
    writeln('TRANFORMATION: binary to text  ----------------');
    BinStream.Position:=0;
    ObjectBinaryToText(BinStream,TmpTxtStream);
    writeln('');
    writeln('TRANFORMATION: text back to binary  ----------------');
    TmpTxtStream.Position:=0;
    ObjectTextToBinary(TmpTxtStream,TmpBinStream);
    writeln('');
    writeln('TRANFORMATION: binary to text file ----------------');
    TxtStream:=TFileStream.Create(ClassName+'.lfm2',fmCreate);
    try
      TmpBinStream.Position:=0;
      ObjectBinaryToText(TmpBinStream,TxtStream);
    finally
      TxtStream.Free;
    end;
  finally
    TmpBinStream.Free;
    TmpTxtStream.Free;
    BinStream.Free;
  end;
writeln('Object written.');
end;

procedure TForm1.LoadFromLFM;
var
  BinStream:TMemoryStream;
  TxtStream:TFileStream;
  Reader:TReader;
begin
  // read LFM file and convert it to binary format
  TxtStream:=TFileStream.Create(ClassName+'.lfm',fmOpenRead);
  try
    BinStream:=TMemoryStream.Create;
    ObjectTextToBinary(TxtStream,BinStream);
  finally
    TxtStream.Free;
  end;
  //
  BinStream.Position:=0;
  Reader:=TReader.Create(BinStream,4096);
  try
    Reader.OnError:=@ReaderError;
    Reader.OnFindMethod:=@ReaderFindMethod;
    Reader.OnSetName:=@ReaderSetName;
    Reader.OnReferenceName:=@ReaderReferenceName;
    Reader.OnAncestorNotFound:=@ReaderAncestorNotFound;
    Reader.OnCreateComponent:=@ReaderCreateComponent;
    Reader.OnFindComponentClass:=@ReaderFindComponentClass;
    writeln('');
    writeln('PARSING LFM ********************************************');
    Reader.ReadRootComponent(Self);
  finally
    Reader.Free;
  end;
  BinStream.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
end;

procedure TForm1.OIResizeButtonClick(Sender : TObject);
begin
  OI.DoInnerResize;
end;

procedure TForm1.OIRefreshButtonClick(Sender : TObject);
begin
  OI.RefreshPropertyValues;
end;

procedure TForm1.EditToComboButtonClick(Sender : TObject);
Begin
  if assigned (ComboBox1) and assigned (edit1) then
    ComboBox1.Text := edit1.text;
End;

procedure TForm1.AddItemButtonClick(Sender : TObject);
Begin
   if assigned (ComboBox1) 
      then Combobox1.Items.Add ('item ' + IntToStr (comboBox1.Items.Count));
   if assigned (ComboBox2) 
      then Combobox2.Items.Add ('item ' + IntToStr (comboBox2.Items.Count));
End;

procedure TForm1.ComboToEditButtonClick(Sender : TObject);
Begin
   if assigned (ComboBox1) and assigned (edit1) 
      then edit1.Text := ComboBox1.Text;
End;

procedure TForm1.SwitchEnabledButtonClick(Sender : TObject);
Begin
   if assigned (ComboBox1) 
      then ComboBox1.Enabled := not ComboBox1.Enabled;
End;

procedure TForm1.DumpButtonClick(Sender : TObject);
var
   i : integer;
Begin
   if assigned (ComboBox1) then
   begin
      i := 0;
      while i < ComboBox1.Items.Count do
      begin
         if assigned (Memo1)
            then Memo1.Lines.Add (ComboBox1.Items[i]);
	       inc (i);
      end;
   end;
End;

procedure TForm1.IndexButtonClick(Sender : TObject);
var
  s : shortstring;
Begin
  if assigned (ComboBox1) then
  begin
    s := Format ('%x', [ComboBox1.ItemIndex]);
    if assigned (Memo1)
       then Memo1.Lines.Add (s);
  end;
End;

procedure TForm1.ComboOnChange (Sender:TObject);
var
  s : shortstring;
begin
  if sender is TEdit
    then s := 'TEdit'
  else if sender is TComboBox
    then s := 'TComboBox'
  else
    s := 'UNKNOWN';
  if assigned (Memo1)
    then Memo1.Lines.Add (s + 'ONChange');
  if ComboBox1.Text='Create LFM' then
    WriteLFMButtonClick(nil);
end;

procedure TForm1.ComboOnClick (Sender:TObject);
begin
  if assigned (Memo1)
    then Memo1.Lines.Add ('ONClick');
end;

procedure TForm1.ReaderFindMethod(Reader: TReader; const FindMethodName: Ansistring;
  var Address: Pointer; var Error: Boolean);
begin
  writeln('ReaderFindMethod '''+FindMethodName+'''');
end;

procedure TForm1.ReaderSetName(Reader: TReader; Component: TComponent;
  var NewName: Ansistring);
begin
  writeln('ReaderSetName OldName='''+Component.Name+''' NewName='''+NewName+'''');
end;

procedure TForm1.ReaderReferenceName(Reader: TReader; var RefName: Ansistring);
begin
  writeln('ReaderReferenceName Name='''+RefName+'''');
end;

procedure TForm1.ReaderAncestorNotFound(Reader: TReader; const ComponentName: Ansistring;
  ComponentClass: TPersistentClass; var Component: TComponent);
begin
  writeln('ReaderAncestorNotFound ComponentName='''+ComponentName
    +''' Component='''+Component.Name+'''');
end;

procedure TForm1.ReaderError(Reader: TReader; const Message: Ansistring;
  var Handled: Boolean);
begin
  writeln('ReaderError '''+Message+'''');
end;

procedure TForm1.ReaderFindComponentClass(Reader: TReader; const FindClassName: Ansistring;
  var ComponentClass: TComponentClass);
begin
  writeln('ReaderFindComponentClass ClassName='''+ClassName+'''');
end;

procedure TForm1.ReaderCreateComponent(Reader: TReader;
  ComponentClass: TComponentClass; var Component: TComponent);
begin
  writeln('ReaderCreateComponent Class='''+ComponentClass.ClassName+'''');
end;



{------------------------------------------------------------------------------}
procedure TForm1.LoadMainMenu;

begin
   OnDestroy := @FormKill;

   { set the height and width }
   Height := 400;
   Width := 700;

{   OIResizeButton:=TButton.Create(Self);
   with OIResizeButton do begin
     Name:='OIResizeButton';
     Parent:=Self;
     SetBounds(200,10,100,30);
     Caption:='Resize OI';
     OnClick:=@OIResizeButtonClick;
     Show;
   end;

   OIRefreshButton:=TButton.Create(Self);
   with OIRefreshButton do begin
     Name:='OIRefreshButton';
     Parent:=Self;
     SetBounds(200,50,100,30);
     Caption:='Refresh OI';
     OnClick:=@OIRefreshButtonClick;
     Show;
   end;

   WriteLFMButton:=TButton.Create(Self);
   with WriteLFMButton do begin
     Name:='WriteLFMButton';
     Parent:=Self;
     SetBounds(200,90,100,30);
     Caption:='Write LFM file';
     OnClick:=@WriteLFMButtonClick;
     Show;
   end;

   // Create 2 buttons inside the groupbox
   EditToComboButton := TButton.Create(Self);
   with EditToComboButton do begin
     Name:='EditToComboButton';
     Parent := Self;
     Left := 50;
     Top := 80;
     Width := 120;
     Height := 30;
     Caption := 'Edit->Combo';
     OnClick := @EditToComboButtonClick;
     Show;
   end;

   AddItemButton := TButton.Create(Self);
   with AddItemButton do begin
     Name:='AddItemButton';
     Parent := Self;
     Left := 50;
     Top := 40;
     Width := 120;
     Height := 30;
     Caption := 'Add item';
     OnClick := @AddItemButtonClick;
     Show;
   end;

   // Create 2 more buttons outside the groupbox
   ComboToEditButton := TButton.Create(Self);
   with ComboToEditButton do begin
     Name:='ComboToEditButton';
     Parent := Self;
     Left := 50;
     Top := 120;
     Width := 120;
     Height := 30;
     Caption := 'Combo->Edit';
     OnClick := @ComboToEditButtonClick;
     Show;
   end;

   SwitchEnabledButton := TButton.Create(Self);
   with SwitchEnabledButton do begin
     Name:='SwitchEnabledButton';
     Parent := Self;
     Left := 50;
     Top := 160;
     Width := 120;
     Height := 30;
     Caption := 'Enabled On/Off';
     OnClick := @SwitchEnabledButtonClick;
     Show;
   end;

   DumpButton := TButton.Create(Self);
   with DumpButton do begin
     Name:='DumpButton';
     Parent := Self;
     Left := 50;
     Top := 200;
     Width := 120;
     Height := 30;
     Caption := 'Dump';
     OnClick := @DumpButtonClick;
     Show;
   end;

   IndexButton := TButton.Create(Self);
   with IndexButton do begin
     Name:='IndexButton';
     Parent := Self;
     Left := 50;
     Top := 240;
     Width := 120;
     Height := 30;
     Caption := 'Index ?';
     OnClick := @IndexButtonClick;
     Show;
   end;

}
   // Create a label for the edit field
   Label1 := TLabel.Create(Self);
   with Label1 do begin
     Name:='Label1';
     Parent := self;
     top	 := 50;
     left	 := 320;
     Height := 20;
     Width  := 130;
     Caption := 'TEdit :';
     Show;
   end;


   Edit1 := TEdit.Create (self);
   with Edit1 do begin
     Name   := 'Edit1';
     Parent := self;
     Left   := 500;
     Top    := 50;
     Width  := 70;
     Height := 20;
     OnChange := @ComboOnChange;
     OnClick  := @ComboOnClick;
     Show;
   end;

   // Create a label for the 1st combobox
   Label2 := TLabel.Create(Self);
   with Label2 do begin
     Name:='Label2';
     Parent := self;
     top	 := 100;
     left	 := 320;
     Height := 20;
     Width  := 130;
     Caption := 'Combo (unsorted)';
     Show;
   end;

   // Create the menu now
   { WARNING: If you do it after creation of the combo, the menu will not 
     appear. Reason is unknown by now!!!!!!}
   {mnuMain := TMainMenu.Create(Self);
   mnuMain.Name:='mnuMain';
   Menu := mnuMain;
   itmFile := TMenuItem.Create(Self);
   itmFile.Name:='itmFile';
   itmFile.Caption := '&File';
   mnuMain.Items.Add(itmFile);
   itmFileQuit := TMenuItem.Create(Self);
   itmFileQuit.Name:='itmFileQuit';
   itmFileQuit.Caption := '&Quit';
   itmFileQuit.OnClick := @mnuQuitClicked;
   itmFile.Add(itmFileQuit);}

   ComboBox1 := TComboBox.Create (self);
   with ComboBox1 do
   begin
     Name:='ComboBox1';
     Parent := self;
     Left := 500;
     Top	:= 100;
     Width := 170;
     Height := 20;
     Style := csDropDown;
     Items.Add ('wohhh!');
     Items.Add ('22222!');
     ItemIndex := 1;
     Items.Add ('33333!');
     Items.Add ('abcde!');
     Items.Add ('Create LFM');
     OnChange := @ComboOnChange;
     OnClick  := @ComboOnClick;
     Show;
   end;


   { Create a label for the 2nd combobox }
   Label3 := TLabel.Create(Self);
   with Label3 do begin
     Name:='Label3';
     Parent := self;
     top	 := 150;
     left	 := 320;
     Height := 20;
     Width  := 130;
     Caption := 'Combo (sorted)';
     Show;
   end;


   ComboBox2 := TComboBox.Create (self);
   with ComboBox2 do begin
     Name:='ComboBox2';
     Parent := self;
     Left := 500;
     Top	:= 150;
     Width := 170;
     Height := 20;
     Items.Add ('wohhh!');
     Items.Add ('22222!');
     ItemIndex := 1;
     Items.Add ('33333!');
     Items.Add ('abcde!');
     Sorted := true;
     Show;
   end;

   Memo1 := TMemo.Create(Self);
   with Memo1 do begin
     Memo1.Name:='Memo1';
     Parent := Self;
     Scrollbars := ssBoth;
     Left := 200;
     Top := 200;
     Width := 335;
     Height := 155;
     Show;
   end;
end;

{------------------------------------------------------------------------------}
{procedure TForm1.mnuQuitClicked(Sender : TObject);
begin
  Application.Terminate;
end; }
{------------------------------------------------------------------------------}


end.
