{
 Test with:
     ./runtests --format=plain --suite=TTestCompReaderWriterPas
     ./runtests --format=plain --suite=TTestCompReaderWriterPas.TestWriteProperties

ToDo:
- base types
- UTF-8 string
- unicodestring
- enum
- set
- variant
- datetime
- TComponent.Left/Right
- custom range TColor
- subcomponents
- cycle in subcomponents
- ancestor
- childpos
- inline component
- collection
- DefineProperty
}
unit TestCompReaderWriterPas;

{$mode objfpc}{$H+}

{$DEFINE VerboseCompWriterPas}

interface

uses
  Classes, SysUtils, typinfo, RtlConsts, LazLoggerBase, LazUTF8, fpcunit,
  testregistry, CodeToolManager, LinkScanner, TestStdCodetools;

const
  CWPDefaultSignature = '// component writer V1.0';
type
  TDummyComp = class(TComponent); // to access TComponent protected members
  TCWPFindAncestorEvent = procedure(Sender: TObject; Component: TComponent;
    const Name: string; var Ancestor, RootAncestor: TComponent) of object;
  TCWPGetMethodName = procedure(Sender: TObject; Instance: TPersistent;
    PropInfo: PPropInfo; out Name: String) of object;

  TCWPOption = (
    cwpoNoSignature,
    cwpoSetParentFirst,  // add "Parent:=" before properties
    cwpoSrcCodepageUTF8
    );
  TCWPOptions = set of TCWPOption;

  { TCompWriterPas }

  TCompWriterPas = class
  private
    FAssignOp: String;
    FCurIndent: integer;
    FIndentStep: integer;
    FLineEnding: string;
    FOnGetMethodName: TCWPGetMethodName;
    FOptions: TCWPOptions;
    FParent: TComponent;
    FPropPath: string;
    FSignature: String;
    FStream: TStream;
    FRoot: TComponent;
    FLookupRoot: TComponent;
    FAncestor: TPersistent;
    FRootAncestor: TComponent;
    FAncestors: TStringList;
    FAncestorPos: Integer;
    //FCurrentPos: Integer;
    FOnFindAncestor: TCWPFindAncestorEvent;
    procedure DetermineAncestor(Component: TComponent);
    procedure DoFindAncestor(Component: TComponent);
    procedure SetRoot(const AValue: TComponent);
    procedure WriteComponentData(Instance: TComponent);
    procedure WriteProperty(Instance: TPersistent; PropInfo: PPropInfo);
    procedure WriteProperties(Instance: TComponent);
    function GetStringLiteral(const s: string): string;
    function GetWStringLiteral(p: PWideChar; Count: integer): string;
    function GetFloatLiteral(const e: Extended): string;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    procedure WriteComponent(Component: TComponent);
    procedure WriteDescendant(ARoot: TComponent; AAncestor: TComponent = nil);
    procedure WriteSignature;
    procedure WriteIndent;
    procedure Write(const s: string);
    procedure WriteLn;
    procedure WriteAssign(const LHS, RHS: string);
    procedure Indent;
    procedure Unindent;
    property Stream: TStream read FStream;
    property Root: TComponent read FRoot write SetRoot;
    property LookupRoot: TComponent read FLookupRoot;
    property Ancestor: TPersistent read FAncestor write FAncestor;
    property RootAncestor: TComponent read FRootAncestor write FRootAncestor;
    property Parent: TComponent read FParent;
    property OnFindAncestor: TCWPFindAncestorEvent read FOnFindAncestor write FOnFindAncestor;
    property OnGetMethodName: TCWPGetMethodName read FOnGetMethodName write FOnGetMethodName;
    property PropertyPath: string read FPropPath;
    property CurIndent: integer read FCurIndent write FCurIndent;
    property IndentStep: integer read FIndentStep write FIndentStep;
    property Options: TCWPOptions read FOptions write FOptions;
  public
    // code snippets
    property LineEnding: string read FLineEnding write FLineEnding;
    property AssignOp: String read FAssignOp write FAssignOp;
    property Signature: String read FSignature write FSignature;
  end;

// Tests =======================================================================
const
  MinSafeIntCurrency = Low(Int64) div 10000;
  MaxSafeIntCurrency = High(Int64) div 10000;
  MinSafeIntSingle = -16777216;
  MaxSafeIntSingle =  16777216;
  MaskUIntSingle = $3fffff;
  MinSafeIntDouble = -$10000000000000;
  MaxSafeIntDouble =   $fffffffffffff;
  MaskUIntDouble = $fffffffffffff;
type
  TEnum = (red, green, blue, white, black);
  TEnumRg = green..white;
  TSetOfEnum = set of TEnum;
  TSetOfEnumRg = set of TEnumRg;

  { TCompBaseTypes }

  TCompBaseTypes = class(TComponent)
  private
    FABoolean: Boolean;
    FAByte: Byte;
    FAByteBool: ByteBool;
    FAChar: Char;
    FACurrency: Currency;
    FADouble: Double;
    FAExtended: Extended;
    FAInt64: Int64;
    FALongBool: LongBool;
    FALongInt: LongInt;
    FALongWord: LongWord;
    FAQWord: QWord;
    FAShortInt: ShortInt;
    FAShortString: ShortString;
    FASingle: Single;
    FASmallInt: SmallInt;
    FAString: String;
    FAUnicodeString: UnicodeString;
    FAWideChar: WideChar;
    FAWideString: WideString;
    FAWord: Word;
    FAWordBool: WordBool;
    FEnum: TEnum;
    FEnumRg: TEnumRg;
    FSetOfEnum: TSetOfEnum;
    FSetOfEnumRg: TSetOfEnumRg;
    function isACurrencyStored: Boolean;
    function isADoubleStored: Boolean;
    function isAExtendedStored: Boolean;
    function isAShortStringStored: Boolean;
    function isASingleStored: Boolean;
    function isAStringStored: Boolean;
    function isAUnicodeStringStored: Boolean;
    function isAWideStringStored: Boolean;
  published
    constructor Create(AOwner: TComponent); override;
    property ABoolean: Boolean read FABoolean write FABoolean default false;
    property AByteBool: ByteBool read FAByteBool write FAByteBool default false;
    property AWordBool: WordBool read FAWordBool write FAWordBool default false;
    property ALongBool: LongBool read FALongBool write FALongBool default false;
    property AByte: Byte read FAByte write FAByte default 0;
    property AShortInt: ShortInt read FAShortInt write FAShortInt default 0;
    property AWord: Word read FAWord write FAWord default 0;
    property ASmallInt: SmallInt read FASmallInt write FASmallInt default 0;
    property ALongWord: LongWord read FALongWord write FALongWord default 0;
    property ALongInt: LongInt read FALongInt write FALongInt default 0;
    property AQWord: QWord read FAQWord write FAQWord default 0;
    property AInt64: Int64 read FAInt64 write FAInt64 default 0;
    property ACurrency: Currency read FACurrency write FACurrency stored isACurrencyStored;
    property ASingle: Single read FASingle write FASingle stored isASingleStored;
    property ADouble: Double read FADouble write FADouble stored isADoubleStored;
    property AExtended: Extended read FAExtended write FAExtended stored isAExtendedStored;
    property AChar: Char read FAChar write FAChar default #0;
    property AWideChar: WideChar read FAWideChar write FAWideChar default #0;
    property AString: String read FAString write FAString stored isAStringStored;
    property AShortString: ShortString read FAShortString write FAShortString stored isAShortStringStored;
    property AWideString: WideString read FAWideString write FAWideString stored isAWideStringStored;
    property AUnicodeString: UnicodeString read FAUnicodeString write FAUnicodeString stored isAUnicodeStringStored;
    property Enum: TEnum read FEnum write FEnum default low(TEnum);
    property EnumRg: TEnumRg read FEnumRg write FEnumRg default low(TEnumRg);
    property SetOfEnum: TSetOfEnum read FSetOfEnum write FSetOfEnum default [];
    property SetOfEnumRg: TSetOfEnumRg read FSetOfEnumRg write FSetOfEnumRg default [];
  end;

  { TCompBaseTypesCustomStored }

  TCompBaseTypesCustomStored = class(TComponent)
  private
    FABoolean: Boolean;
    FAByte: Byte;
    FAByteBool: ByteBool;
    FAChar: Char;
    FACurrency: Currency;
    FADouble: Double;
    FAExtended: Extended;
    FAInt64: Int64;
    FALongBool: LongBool;
    FALongInt: LongInt;
    FALongWord: LongWord;
    FAQWord: QWord;
    FAShortInt: ShortInt;
    FAShortString: ShortString;
    FASingle: Single;
    FASmallInt: SmallInt;
    FAString: String;
    FAUnicodeString: UnicodeString;
    FAWideChar: WideChar;
    FAWideString: WideString;
    FAWord: Word;
    FAWordBool: WordBool;
    FEnum: TEnum;
    FEnumRg: TEnumRg;
    FSetOfEnum: TSetOfEnum;
    FSetOfEnumRg: TSetOfEnumRg;
    function ABooleanIsStored: Boolean;
    function AByteBoolIsStored: Boolean;
    function AByteIsStored: Boolean;
    function ACharIsStored: Boolean;
    function ACurrencyIsStored: Boolean;
    function ADoubleIsStored: Boolean;
    function AExtendedIsStored: Boolean;
    function AInt64IsStored: Boolean;
    function ALongBoolIsStored: Boolean;
    function ALongIntIsStored: Boolean;
    function ALongWordIsStored: Boolean;
    function AQWordIsStored: Boolean;
    function AShortIntIsStored: Boolean;
    function AShortStringIsStored: Boolean;
    function ASingleIsStored: Boolean;
    function ASmallIntIsStored: Boolean;
    function AStringIsStored: Boolean;
    function AUnicodeStringIsStored: Boolean;
    function AWideCharIsStored: Boolean;
    function AWideStringIsStored: Boolean;
    function AWordBoolIsStored: Boolean;
    function AWordIsStored: Boolean;
    function EnumIsStored: Boolean;
    function EnumRgIsStored: Boolean;
    function SetOfEnumIsStored: Boolean;
    function SetOfEnumRgIsStored: Boolean;
  public
    DefABoolean: Boolean;
    DefAByteBool: ByteBool;
    DefAWordBool: WordBool;
    DefALongBool: LongBool;
    DefAByte: Byte;
    DefAShortInt: ShortInt;
    DefAWord: Word;
    DefASmallInt: SmallInt;
    DefALongWord: LongWord;
    DefALongInt: LongInt;
    DefAQWord: QWord;
    DefAInt64: Int64;
    DefACurrency: Currency;
    DefASingle: Single;
    DefADouble: Double;
    DefAExtended: Extended;
    DefAChar: Char;
    DefAWideChar: WideChar;
    DefAString: String;
    DefAShortString: ShortString;
    DefAWideString: WideString;
    DefAUnicodeString: UnicodeString;
    DefEnum: TEnum;
    DefEnumRg: TEnumRg;
    DefSetOfEnum: TSetOfEnum;
    DefSetOfEnumRg: TSetOfEnumRg;
  published
    constructor Create(AOwner: TComponent); override;
    property ABoolean: Boolean read FABoolean write FABoolean stored ABooleanIsStored;
    property AByteBool: ByteBool read FAByteBool write FAByteBool stored AByteBoolIsStored;
    property AWordBool: WordBool read FAWordBool write FAWordBool stored AWordBoolIsStored;
    property ALongBool: LongBool read FALongBool write FALongBool stored ALongBoolIsStored;
    property AByte: Byte read FAByte write FAByte stored AByteIsStored;
    property AShortInt: ShortInt read FAShortInt write FAShortInt stored AShortIntIsStored;
    property AWord: Word read FAWord write FAWord stored AWordIsStored;
    property ASmallInt: SmallInt read FASmallInt write FASmallInt stored ASmallIntIsStored;
    property ALongWord: LongWord read FALongWord write FALongWord stored ALongWordIsStored;
    property ALongInt: LongInt read FALongInt write FALongInt stored ALongIntIsStored;
    property AQWord: QWord read FAQWord write FAQWord stored AQWordIsStored;
    property AInt64: Int64 read FAInt64 write FAInt64 stored AInt64IsStored;
    property ACurrency: Currency read FACurrency write FACurrency stored ACurrencyIsStored;
    property ASingle: Single read FASingle write FASingle stored ASingleIsStored;
    property ADouble: Double read FADouble write FADouble stored ADoubleIsStored;
    property AExtended: Extended read FAExtended write FAExtended stored AExtendedIsStored;
    property AChar: Char read FAChar write FAChar stored ACharIsStored;
    property AWideChar: WideChar read FAWideChar write FAWideChar stored AWideCharIsStored;
    property AString: String read FAString write FAString stored AStringIsStored;
    property AShortString: ShortString read FAShortString write FAShortString stored AShortStringIsStored;
    property AWideString: WideString read FAWideString write FAWideString stored AWideStringIsStored;
    property AUnicodeString: UnicodeString read FAUnicodeString write FAUnicodeString stored AUnicodeStringIsStored;
    property Enum: TEnum read FEnum write FEnum stored EnumIsStored;
    property EnumRg: TEnumRg read FEnumRg write FEnumRg stored EnumRgIsStored;
    property SetOfEnum: TSetOfEnum read FSetOfEnum write FSetOfEnum stored SetOfEnumIsStored;
    property SetOfEnumRg: TSetOfEnumRg read FSetOfEnumRg write FSetOfEnumRg stored SetOfEnumRgIsStored;
  end;

  { TTestCompReaderWriterPas }

  TTestCompReaderWriterPas = class(TCustomTestCTStdCodetools)
  private
    FStream: TMemoryStream;
    FWriter: TCompWriterPas;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function WriteDescendant(Component: TComponent; Ancestor: TComponent = nil): string;
    procedure TestWriteDescendant(Msg: string; Component: TComponent;
      Ancestor: TComponent; const Expected: array of string);
    property Writer: TCompWriterPas read FWriter write FWriter;
  published
    procedure TestBaseTypesSkipDefaultValue;
    procedure TestBaseTypesZeroes;
    procedure TestBaseTypesMinValues;
    procedure TestBaseTypesMaxValues;
    procedure TestStringASCII;
    procedure TestStringUTF8;
    procedure TestWideString_SrcCodePageSystem;
    procedure TestWideString_SrcCodePageUTF8;
  end;

implementation

function IsValidUTF8(p: PChar): integer;
var
  c: Char;
begin
  c:=p^;
  if ord(c)<%10000000 then begin
    // regular single byte ASCII character (#0 is a character, this is Pascal ;)
    Result:=1;
  end else if ord(c)<=%11000001 then begin
    // single byte character, between valid UTF-8 encodings
    // %11000000 and %11000001 map 2 byte to #0..#128, which is invalid and used for XSS attacks
    Result:=0;
  end else if ord(c)<=%11011111 then begin
    // could be 2 byte character (%110xxxxx %10xxxxxx)
    if ((ord(p[1]) and %11000000) = %10000000) then
      Result:=2
    else
      Result:=0; // missing following bytes
  end
  else if ord(c)<=%11101111 then begin
    // could be 3 byte character (%1110xxxx %10xxxxxx %10xxxxxx)
    if ((ord(p[1]) and %11000000) = %10000000)
    and ((ord(p[2]) and %11000000) = %10000000) then begin
      if (ord(c)=%11100000) and (ord(p[1])<=%10011111) then
        Result:=0; // XSS attack: 3 bytes are mapped to the 1 or 2 byte codes
      Result:=3;
    end else
      Result:=0; // missing following bytes
  end
  else if ord(c)<=%11110111 then begin
    // could be 4 byte character (%11110xxx %10xxxxxx %10xxxxxx %10xxxxxx)
    if ((ord(p[1]) and %11000000) = %10000000)
    and ((ord(p[2]) and %11000000) = %10000000)
    and ((ord(p[3]) and %11000000) = %10000000) then begin
      if (ord(c)=%11110000) and (ord(p[1])<=%10001111) then
        Result:=0; // XSS attack: 4 bytes are mapped to the 1-3 byte codes
      Result:=4;
    end else
      Result:=0; // missing following bytes
  end
  else begin
    Result:=0;
  end;
end;

function IsValidUTF16(p: PWideChar): integer;
var
  c: WideChar;
begin
  c:=p^;
  if c<=#$DC7F then
    exit(1)
  else if c<=#$DBFF then begin
    c:=p[1];
    if (c>=#$DC00) and (c<=#$DFFF) then
      exit(2)
    else
      exit(0);
  end else if c<=#$Dfff then begin
    exit(0);
  end else
    exit(1);
end;


Type

  { TPosComponent }

  TPosComponent = class(TObject)
    FPos: Integer;
    FComponent: TComponent;
    constructor Create(APos: Integer; AComponent: TComponent);
  end;

{ TPosComponent }

constructor TPosComponent.Create(APos: Integer; AComponent: TComponent);
begin
  FPos:=APos;
  FComponent:=AComponent;
end;

{ TCompBaseTypesCustomStored }

function TCompBaseTypesCustomStored.ABooleanIsStored: Boolean;
begin
  Result:=FABoolean<>DefABoolean;
end;

function TCompBaseTypesCustomStored.AByteBoolIsStored: Boolean;
begin
  Result:=FAByteBool<>DefAByteBool;
end;

function TCompBaseTypesCustomStored.AByteIsStored: Boolean;
begin
  Result:=FAByte<>DefAByte;
end;

function TCompBaseTypesCustomStored.ACharIsStored: Boolean;
begin
  Result:=FAChar<>DefAChar;
end;

function TCompBaseTypesCustomStored.ACurrencyIsStored: Boolean;
begin
  Result:=FACurrency<>DefACurrency;
end;

function TCompBaseTypesCustomStored.ADoubleIsStored: Boolean;
begin
  Result:=FADouble<>DefADouble;
end;

function TCompBaseTypesCustomStored.AExtendedIsStored: Boolean;
begin
  Result:=FAExtended<>DefAExtended;
end;

function TCompBaseTypesCustomStored.AInt64IsStored: Boolean;
begin
  Result:=FAInt64<>DefAInt64;
end;

function TCompBaseTypesCustomStored.ALongBoolIsStored: Boolean;
begin
  Result:=FALongBool<>DefALongBool;
end;

function TCompBaseTypesCustomStored.ALongIntIsStored: Boolean;
begin
  Result:=FALongInt<>DefALongInt;
end;

function TCompBaseTypesCustomStored.ALongWordIsStored: Boolean;
begin
  Result:=FALongWord<>DefALongWord;
end;

function TCompBaseTypesCustomStored.AQWordIsStored: Boolean;
begin
  Result:=FAWord<>DefAWord;
end;

function TCompBaseTypesCustomStored.AShortIntIsStored: Boolean;
begin
  Result:=FAShortInt<>DefAShortInt;
end;

function TCompBaseTypesCustomStored.AShortStringIsStored: Boolean;
begin
  Result:=FAShortString<>DefAShortString;
end;

function TCompBaseTypesCustomStored.ASingleIsStored: Boolean;
begin
  Result:=FASingle<>DefASingle;
end;

function TCompBaseTypesCustomStored.ASmallIntIsStored: Boolean;
begin
  Result:=FASmallInt<>DefASmallInt;
end;

function TCompBaseTypesCustomStored.AStringIsStored: Boolean;
begin
  Result:=FAString<>DefAString;
end;

function TCompBaseTypesCustomStored.AUnicodeStringIsStored: Boolean;
begin
  Result:=FAUnicodeString<>DefAUnicodeString;
end;

function TCompBaseTypesCustomStored.AWideCharIsStored: Boolean;
begin
  Result:=FAWideChar<>DefAWideChar;
end;

function TCompBaseTypesCustomStored.AWideStringIsStored: Boolean;
begin
  Result:=FAWideString<>DefAWideString;
end;

function TCompBaseTypesCustomStored.AWordBoolIsStored: Boolean;
begin
  Result:=FAWordBool<>DefAWordBool;
end;

function TCompBaseTypesCustomStored.AWordIsStored: Boolean;
begin
  Result:=FAWord<>DefAWord;
end;

function TCompBaseTypesCustomStored.EnumIsStored: Boolean;
begin
  Result:=FEnum<>DefEnum;
end;

function TCompBaseTypesCustomStored.EnumRgIsStored: Boolean;
begin
  Result:=FEnumRg<>DefEnumRg;
end;

function TCompBaseTypesCustomStored.SetOfEnumIsStored: Boolean;
begin
  Result:=FSetOfEnum<>DefSetOfEnum;
end;

function TCompBaseTypesCustomStored.SetOfEnumRgIsStored: Boolean;
begin
  Result:=FSetOfEnumRg<>DefSetOfEnumRg;
end;

constructor TCompBaseTypesCustomStored.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

{ TCompWriterPas }

procedure TCompWriterPas.DetermineAncestor(Component: TComponent);
var
  i : Integer;
begin
  if not Assigned(FAncestors) then
    exit;
  i:=FAncestors.IndexOf(Component.Name);
  if i<0 then
  begin
    FAncestor:=nil;
    FAncestorPos:=-1;
  end
  else
    With TPosComponent(FAncestors.Objects[i]) do
    begin
      FAncestor:=FComponent;
      FAncestorPos:=FPos;
    end;
end;

procedure TCompWriterPas.DoFindAncestor(Component: TComponent);
var
  C: TComponent;
begin
  if Assigned(FOnFindAncestor) then
    if (Ancestor=Nil) or (Ancestor is TComponent) then
    begin
      C:=TComponent(Ancestor);
      FOnFindAncestor(Self,Component,Component.Name,C,FRootAncestor);
      Ancestor:=C;
    end;
end;

procedure TCompWriterPas.SetRoot(const AValue: TComponent);
begin
  FRoot:=AValue;
  FLookupRoot:=FRoot;
end;

procedure TCompWriterPas.WriteComponentData(Instance: TComponent);

  procedure WriteSetParent;
  begin
    if Parent=nil then exit;
    WriteAssign('Parent',Parent.Name);
  end;

begin
  if Instance<>LookupRoot then
  begin
    WriteIndent;
    Write('with ');
    Write(Instance.Name);
    Write(' do begin');
    WriteLn;
    Indent;
  end;
  WriteAssign('Name',''''+Instance.Name+'''');
  if cwpoSetParentFirst in Options then
    WriteSetParent;
  WriteProperties(Instance);
  if not (cwpoSetParentFirst in Options) then
    WriteSetParent;
  if Instance<>LookupRoot then
  begin
    Unindent;
    WriteIndent;
    Write('end;');
    WriteLn;
  end;
end;

procedure TCompWriterPas.WriteProperty(Instance: TPersistent;
  PropInfo: PPropInfo);
type
  TSet = set of 0..31;
var
  PropType, CompType: PTypeInfo;
  ObjValue: TObject;
  HasAncestor, BoolValue, DefBoolValue: Boolean;
  Int32Value, DefValue: longint;
  PropName, Ident, s, StrValue, DefStrValue: String;
  IntToIdentFn: TIntToIdent;
  i, j: Integer;
  Int64Value, DefInt64Value: Int64;
  FloatValue, DefFloatValue: Extended;
  MethodValue, DefMethodValue: TMethod;
  WStrValue, WDefStrValue: WideString;
  UStrValue, UDefStrValue: UnicodeString;
  VarValue, DefVarValue: tvardata;
  aTypeData: PTypeData;
begin
  // do not stream properties without getter
  if not Assigned(PropInfo^.GetProc) then
    exit;

  // properties without setter are only allowed, if they are subcomponents
  PropType := PropInfo^.PropType;
  if not Assigned(PropInfo^.SetProc) then begin
    if PropType^.Kind<>tkClass then
      exit;
    ObjValue := TObject(GetObjectProp(Instance, PropInfo));
    if not ObjValue.InheritsFrom(TComponent) or
       not (csSubComponent in TComponent(ObjValue).ComponentStyle) then
      exit;
  end;

  { Check if the ancestor can be used }
  HasAncestor := Assigned(Ancestor) and ((Instance = Root) or
    (Instance.ClassType = Ancestor.ClassType));
  PropName:=FPropPath + PropInfo^.Name;
  System.writeln('TWriter.WriteProperty PropName="',PropName,'" TypeName=',PropType^.Name,' Kind=',GetEnumName(TypeInfo(TTypeKind),ord(PropType^.Kind)),' HasAncestor=',HasAncestor);

  case PropType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkSet, tkWChar:
      begin
        Int32Value := GetOrdProp(Instance, PropInfo);
        if HasAncestor then
          DefValue := GetOrdProp(Ancestor, PropInfo)
        else
          DefValue := PPropInfo(PropInfo)^.Default;
        //System.writeln(PropInfo^.Name,', HasAncestor=',HasAncestor,', Value=',Int32Value,', Default=',DefValue);
        if (Int32Value <> DefValue) or (DefValue=longint($80000000)) then
        begin
          case PropType^.Kind of
            tkInteger:
              begin
                // Check if this integer has a string identifier
                IntToIdentFn := FindIntToIdent(PropInfo^.PropType);
                Ident:='';
                if Assigned(IntToIdentFn) and IntToIdentFn(Int32Value, Ident) then
                  // Integer with a custom identifier
                  // ToDo: check if this is an actual Pascal constant and remember the unit
                  WriteAssign(PropName,Ident)
                else begin
                  // Integer has to be written just as number
                  aTypeData:=GetTypeData(PropInfo^.PropType);
                  if aTypeData^.MinValue>=0 then
                    WriteAssign(PropName,IntToStr(longword(Int32Value)))
                  else
                    WriteAssign(PropName,IntToStr(Int32Value));
                end;
              end;
            tkChar:
              case Int32Value of
              0..31,127..255:
                WriteAssign(PropName,'#'+IntToStr(Int32Value))
              else
                WriteAssign(PropName,''''+chr(Int32Value)+'''');
              end;
            tkWChar:
              case Int32Value of
              32..126:
                WriteAssign(PropName,''''+Chr(Int32Value)+'''');
              0..31,127..255,$D800..$DFFF:
                WriteAssign(PropName,'#'+IntToStr(Int32Value));
              else
                if cwpoSrcCodepageUTF8 in Options then
                  WriteAssign(PropName,''''+UTF16ToUTF8(WideChar(Int32Value))+'''')
                else
                  WriteAssign(PropName,'#'+IntToStr(Int32Value));
              end;
            tkSet:
              begin
              s:='';
              CompType:=GetTypeData(PropType)^.CompType;
              i:=0;
              while i<32 do
              begin
                if i in TSet(Int32Value) then
                begin
                  if s<>'' then s:=s+',';
                  // ToDo: store needed unit
                  s:=s+GetEnumName(CompType, i);
                  j:=i;
                  while (i<31) and (byte(i+1) in TSet(Int32Value)) do
                    inc(i);
                  if i>j then
                    s:=s+'..'+GetEnumName(CompType, i);
                end;
                inc(i);
              end;
              WriteAssign(PropName,'['+s+']');
              end;
            tkEnumeration:
              // ToDo: store needed unit
              WriteAssign(PropName,GetEnumName(PropType, Int32Value));
          end;
        end;
      end;
    tkFloat:
      begin
        FloatValue := GetFloatProp(Instance, PropInfo);
        if HasAncestor then
          DefFloatValue := GetFloatProp(Ancestor, PropInfo)
        else
          begin
          DefValue :=PropInfo^.Default;
          DefFloatValue:=PSingle(@PropInfo^.Default)^;
          end;
        if (FloatValue<>DefFloatValue) or (DefValue=longint($80000000)) then
          WriteAssign(PropName,GetFloatLiteral(FloatValue));
      end;
    tkMethod:
      begin
        MethodValue := GetMethodProp(Instance, PropInfo);
        if HasAncestor then
          DefMethodValue := GetMethodProp(Ancestor, PropInfo)
        else begin
          DefMethodValue.Data := nil;
          DefMethodValue.Code := nil;
        end;

        if Assigned(OnGetMethodName) then
        begin
          if (MethodValue.Code <> DefMethodValue.Code) or
            (MethodValue.Data <> DefMethodValue.Data) then
          begin
            OnGetMethodName(Self,Instance,PropInfo,Ident);
            OnGetMethodName(Self,Ancestor,PropInfo,s);
            if Ident<>s then
            begin
              if Ident='' then
                WriteAssign(PropName,'nil')
              else
                WriteAssign(PropName,'@'+Ident);
            end;
          end;
        end else begin
          if (MethodValue.Code <> DefMethodValue.Code) then
          begin
            if not Assigned(MethodValue.Code) then
              s:=''
            else
              s:=FLookupRoot.MethodName(MethodValue.Code);
            if s='' then
              WriteAssign(PropName,'nil')
            else
              WriteAssign(PropName,'@'+s);
          end;
        end;
      end;
    tkSString, tkLString, tkAString:
      begin
        StrValue := GetStrProp(Instance, PropInfo);
        if HasAncestor then
          DefStrValue := GetStrProp(Ancestor, PropInfo)
        else
          SetLength(DefStrValue, 0);

        if StrValue <> DefStrValue then
          WriteAssign(PropName,GetStringLiteral(StrValue));
      end;
    tkWString:
      begin
        WStrValue := GetWideStrProp(Instance, PropInfo);
        if HasAncestor then
          WDefStrValue := GetWideStrProp(Ancestor, PropInfo)
        else
          WDefStrValue := '';

        if WStrValue <> WDefStrValue then
          WriteAssign(PropName,GetWStringLiteral(PWideChar(WStrValue),length(WStrValue)));
      end;
    tkUString:
      begin
        UStrValue := GetUnicodeStrProp(Instance, PropInfo);
        if HasAncestor then
          UDefStrValue := GetUnicodeStrProp(Ancestor, PropInfo)
        else
          SetLength(UDefStrValue, 0);

        if UStrValue <> UDefStrValue then
          WriteAssign(PropName,GetWStringLiteral(PWideChar(UStrValue),length(UStrValue)));
      end;
    tkVariant:
      begin
        { Ensure that a Variant manager is installed }
        if not assigned(VarClearProc) then
          raise EWriteError.Create(SErrNoVariantSupport);

        VarValue := tvardata(GetVariantProp(Instance, PropInfo));
        if HasAncestor then
          DefVarValue := tvardata(GetVariantProp(Ancestor, PropInfo))
        else
          FillChar(DefVarValue,sizeof(DefVarValue),0);

        if (CompareByte(VarValue,DefVarValue,sizeof(VarValue)) <> 0) then
          begin
            {$IFDEF VerboseCompWriterPas}
            System.writeln('TCompWriterPas.WriteProperty Property="',PropName,'" Kind=',PropType^.Kind);
            raise EWriteError.Create('proptype not supported: '+GetEnumName(TypeInfo(PropType^.Kind),ord(PropType^.Kind)));
            {$ENDIF}
            { can't use variant() typecast, pulls in variants unit }
            //ToDo WriteVariant(pvariant(@VarValue)^);
          end;
      end;
    tkInt64, tkQWord:
      begin
        Int64Value := GetInt64Prop(Instance, PropInfo);
        if HasAncestor then
          DefInt64Value := GetInt64Prop(Ancestor, PropInfo)
        else
          DefInt64Value := 0;
        if Int64Value <> DefInt64Value then
          if PropType^.Kind=tkInt64 then
            WriteAssign(PropName,IntToStr(Int64Value))
          else
            WriteAssign(PropName,IntToStr(QWord(Int64Value)));
      end;
    tkBool:
      begin
        BoolValue := GetOrdProp(Instance, PropInfo)<>0;
        if HasAncestor then
          DefBoolValue := GetOrdProp(Ancestor, PropInfo)<>0
        else
          DefBoolValue := PropInfo^.Default<>0;
        DefValue:=PropInfo^.Default;
        //System.writeln(PropInfo^.Name,', HasAncestor=',HasAncestor,', BoolValue=',BoolValue,', DefBoolValue=',DefBoolValue,' Default=',DefValue);
        if (BoolValue<>DefBoolValue) or (DefValue=longint($80000000)) then
          WriteAssign(PropName,BoolToStr(BoolValue,'True','False'));
      end;
  else
    {$IFDEF VerboseCompWriterPas}
    System.writeln('TCompWriterPas.WriteProperty Property="',PropName,'" Kind=',PropType^.Kind);
    raise EWriteError.Create('proptype not supported: '+GetEnumName(TypeInfo(PropType^.Kind),ord(PropType^.Kind)));
    {$ENDIF}
  end;
end;

procedure TCompWriterPas.WriteProperties(Instance: TComponent);
var
  PropCount, i: integer;
  PropList: PPropList;
begin
  PropCount:=GetPropList(Instance,PropList);
  if PropCount>0 then
    try
      for i := 0 to PropCount-1 do
        if IsStoredProp(Instance,PropList^[i]) then
          WriteProperty(Instance,PropList^[i]);
    finally
      Freemem(PropList);
    end;
  // ToDo: Instance.DefineProperties(Self);
end;

function TCompWriterPas.GetStringLiteral(const s: string): string;

  function IsSpecialChar(p: PChar): boolean;
  const
    SpecialChars = [#0..#31,#127,#255];
  begin
    Result:=(p^ in SpecialChars) or (IsValidUTF8(p)=0);
  end;

var
  InLit: Boolean;
  p, StartP: PChar;
  c: Char;
begin
  Result:='';
  if s='' then exit;
  InLit:=false;
  p:=PChar(s);
  repeat
    c:=p^;
    if (c=#0) and (p-PChar(s)=length(s)) then
      break
    else if IsSpecialChar(p) then
    begin
      if InLit then begin
        InLit:=false;
        Result:=Result+'''';
      end;
      Result:=Result+'#'+IntToStr(ord(c));
      inc(p);
    end else begin
      if not InLit then begin
        InLit:=true;
        Result:=Result+'''';
      end;
      if c='''' then begin
        Result:=Result+'''''';
        inc(p);
      end else begin
        StartP:=p;
        repeat
          inc(p,IsValidUTF8(p));
          c:=p^;
        until ((c=#0) and (p-PChar(s)=length(s))) or IsSpecialChar(p) or (c='''');
        Result:=Result+copy(s,StartP-PChar(s)+1,p-StartP);
      end;
    end;
  until false;
  if InLit then
    Result:=Result+'''';
end;

function TCompWriterPas.GetWStringLiteral(p: PWideChar; Count: integer): string;

  function IsSpecialChar(w: PWideChar): boolean;
  const
    SpecialChars = [#0..#31,#127];
  begin
    if w^ in SpecialChars then exit(true);
    if cwpoSrcCodepageUTF8 in FOptions then begin
      Result:=IsValidUTF16(w)=0;
    end else begin
      Result:=w^>=#$7f;
    end;
  end;

var
  InLit: Boolean;
  c: WideChar;
  FirstP, StartP: PWideChar;
  AddLen: SizeUInt;
  s: string;
  OldLen: Integer;
begin
  Result:='';
  if Count=0 then exit;
  FirstP:=p;
  InLit:=false;
  s:='';
  repeat
    c:=p^;
    if (c=#0) and (p-FirstP=Count) then
      break
    else if IsSpecialChar(p) then
    begin
      if InLit then begin
        InLit:=false;
        Result:=Result+'''';
      end;
      Result:=Result+'#'+Format('%.4d',[ord(c)]);
      inc(p);
    end else begin
      if not InLit then begin
        InLit:=true;
        Result:=Result+'''';
      end;
      if c='''' then begin
        Result:=Result+'''''';
        inc(p);
      end else begin
        StartP:=p;
        repeat
          inc(p,IsValidUTF16(p));
          c:=p^;
        until ((c=#0) and (p-FirstP=Count)) or IsSpecialChar(p) or (c='''');
        AddLen:=p-StartP;
        if length(s)<AddLen*3 then SetLength(s,AddLen*3);
        if ConvertUTF16ToUTF8(@s[1],length(s),StartP,AddLen,
            [toInvalidCharError,toUnfinishedCharError],AddLen)=trNoError then
          dec(AddLen); // omit #0
        OldLen:=length(Result);
        SetLength(Result,OldLen+AddLen);
        System.Move(s[1],Result[OldLen+1],AddLen);
      end;
    end;
  until false;
  if InLit then
    Result:=Result+'''';
end;

function TCompWriterPas.GetFloatLiteral(const e: Extended): string;
var
  s: String;
  p, i: SizeInt;
begin
  s:='';
  str(e,s);
  // remove unneeded leading 0 of exponent
  p:=Pos('E',s);
  if p<1 then exit;
  i:=p;
  if s[i+1]='+' then inc(i);
  while (i<length(s)) and (s[i+1]='0') do
    inc(i);
  if i>p then
    if i=length(s) then
      Delete(s,p,i-p+1) // delete whole exponent
    else
      Delete(s,p+1,i-p);
  // remove trailing 0 of base
  i:=p;
  while (i>2) and (s[i-1]='0') do
    dec(i);
  if not (s[i-1] in ['0'..'9']) then inc(i);
  if i<p then
    Delete(s,i,p-i);
  // remove leading space
  if s[1]=' ' then
    Delete(s,1,1);
  Result:=s;
end;

constructor TCompWriterPas.Create(AStream: TStream);
begin
  FIndentStep:=2;
  FStream:=AStream;
  FLineEnding:=system.LineEnding;
  FAssignOp:=':=';
  FSignature:=CWPDefaultSignature;
end;

destructor TCompWriterPas.Destroy;
begin
  inherited Destroy;
end;

procedure TCompWriterPas.WriteComponent(Component: TComponent);
var
  OldAncestor : TPersistent;
  OldRoot, OldRootAncestor : TComponent;
begin
  OldRoot:=FRoot;
  OldAncestor:=FAncestor;
  OldRootAncestor:=FRootAncestor;
  Try
    // Component.ComponentState:=Component.FComponentState+[csWriting];
    DetermineAncestor(Component);
    DoFindAncestor(Component); // Mainly for IDE when a parent form had an ancestor renamed...
    WriteComponentData(Component);
  finally
    FAncestor:=OldAncestor;
    FRoot:=OldRoot;
    FRootAncestor:=OldRootAncestor;
  end;
end;

procedure TCompWriterPas.WriteDescendant(ARoot: TComponent; AAncestor: TComponent);
begin
  FRoot := ARoot;
  FAncestor := AAncestor;
  FRootAncestor := AAncestor;
  FLookupRoot := ARoot;
  if not (cwpoNoSignature in Options) then
    WriteSignature;
  WriteComponent(ARoot);
end;

procedure TCompWriterPas.WriteSignature;
begin
  WriteIndent;
  Write(Signature);
  WriteLn;
end;

procedure TCompWriterPas.WriteIndent;
begin
  Write(StringOfChar(' ',CurIndent));
end;

procedure TCompWriterPas.Write(const s: string);
begin
  if s='' then exit;
  FStream.Write(s[1],length(s));
end;

procedure TCompWriterPas.WriteLn;
begin
  Write(LineEnding);
end;

procedure TCompWriterPas.WriteAssign(const LHS, RHS: string);
begin
  WriteIndent;
  Write(LHS);
  Write(AssignOp);
  Write(RHS);
  Write(';');
  WriteLn;
end;

procedure TCompWriterPas.Indent;
begin
  CurIndent:=CurIndent+IndentStep;
end;

procedure TCompWriterPas.Unindent;
begin
  CurIndent:=CurIndent-IndentStep;
end;

{ TCompBaseTypes }

function TCompBaseTypes.isACurrencyStored: Boolean;
begin
  Result:=ACurrency<>0;
end;

function TCompBaseTypes.isADoubleStored: Boolean;
begin
  Result:=ADouble<>0;
end;

function TCompBaseTypes.isAExtendedStored: Boolean;
begin
  Result:=AExtended<>0;
end;

function TCompBaseTypes.isAShortStringStored: Boolean;
begin
  Result:=AShortString<>'';
end;

function TCompBaseTypes.isASingleStored: Boolean;
begin
  Result:=ASingle<>0;
end;

function TCompBaseTypes.isAStringStored: Boolean;
begin
  Result:=AString<>'';
end;

function TCompBaseTypes.isAUnicodeStringStored: Boolean;
begin
  Result:=AUnicodeString<>'';
end;

function TCompBaseTypes.isAWideStringStored: Boolean;
begin
  Result:=AWideString<>'';
end;

constructor TCompBaseTypes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  EnumRg:=low(TEnumRg);
end;

{ TTestCompReaderWriterPas }

procedure TTestCompReaderWriterPas.SetUp;
begin
  inherited SetUp;
  FStream:=TMemoryStream.Create;
  FWriter:=TCompWriterPas.Create(FStream);
end;

procedure TTestCompReaderWriterPas.TearDown;
begin
  FreeAndNil(FWriter);
  FreeAndNil(FStream);
  inherited TearDown;
end;

function TTestCompReaderWriterPas.WriteDescendant(Component: TComponent;
  Ancestor: TComponent): string;
begin
  Writer.WriteDescendant(Component,Ancestor);
  FStream.Position:=0;
  SetLength(Result,FStream.size);
  if Result<>'' then
    FStream.Read(Result[1],length(Result));
  {$IFDEF VerboseCompWriterPas}
  writeln('TTestCompReaderWriterPas.WriteDescendant "',Result,'"');
  {$ENDIF}
end;

procedure TTestCompReaderWriterPas.TestWriteDescendant(Msg: string;
  Component: TComponent; Ancestor: TComponent; const Expected: array of string);
var
  Actual, ExpS, s: String;
begin
  Actual:=WriteDescendant(Component,Ancestor);
  ExpS:=CWPDefaultSignature+LineEnding
    +'Name:='''+Component.Name+''';'+LineEnding;
  for s in Expected do
    ExpS:=ExpS+s+LineEnding;
  CheckDiff(Msg,ExpS,Actual);
end;

procedure TTestCompReaderWriterPas.TestBaseTypesSkipDefaultValue;
var
  AComponent: TCompBaseTypes;
begin
  AComponent:=TCompBaseTypes.Create(nil);
  try
    AComponent.Name:=AComponent.ClassName+'1';
    TestWriteDescendant('TestBaseTypesSkipDefaultValue',AComponent,nil,[
    ]);
  finally
    AComponent.Free;
  end;
end;

procedure TTestCompReaderWriterPas.TestBaseTypesZeroes;
var
  AComponent: TCompBaseTypesCustomStored;
begin
  AComponent:=TCompBaseTypesCustomStored.Create(nil);
  try
    with AComponent do begin
      Name:=AComponent.ClassName+'1';
      AByte:=0;
      DefAByte:=AByte+1;
      AShortInt:=0;
      DefAShortInt:=AShortInt+1;
      AWord:=0;
      DefAWord:=AWord+1;
      ASmallInt:=0;
      DefASmallInt:=ASmallInt+1;
      ALongWord:=0;
      DefALongWord:=ALongWord+1;
      ALongInt:=0;
      DefALongInt:=ALongInt+1;
      AQWord:=0;
      DefAQWord:=AQWord+1;
      AInt64:=0;
      DefAInt64:=AInt64+1;
      ACurrency:=0;
      DefACurrency:=ACurrency+1;
      ASingle:=0;
      DefASingle:=ASingle+1;
      ADouble:=0;
      DefADouble:=ADouble+1;
      AChar:=#0;
      DefAChar:=succ(AChar);
      AWideChar:=#0;
      DefAWideChar:=succ(AWideChar);
      // ToDo: extended
    end;
    TestWriteDescendant('TestBaseTypesZeroes',AComponent,nil,[
    'AByte:=0;',
    'AShortInt:=0;',
    'AWord:=0;',
    'ASmallInt:=0;',
    'ALongWord:=0;',
    'ALongInt:=0;',
    'ACurrency:= 0.0;',
    'ASingle:= 0.0;',
    'ADouble:= 0.0;',
    'AChar:=#0;',
    'AWideChar:=#0;',
    '']);
  finally
    AComponent.Free;
  end;
end;

procedure TTestCompReaderWriterPas.TestBaseTypesMinValues;
var
  AComponent: TCompBaseTypesCustomStored;
begin
  AComponent:=TCompBaseTypesCustomStored.Create(nil);
  try
    with AComponent do begin
      Name:=AComponent.ClassName+'1';
      ABoolean:=low(boolean);
      DefABoolean:=not ABoolean;
      AByteBool:=boolean(low(byte));
      DefAByteBool:=not AByteBool;
      AWordBool:=boolean(low(word));
      DefAWordBool:=not AWordBool;
      ALongBool:=boolean(low(longword));
      DefALongBool:=not ALongBool;
      AByte:=low(byte);
      DefAByte:=AByte+1;
      AShortInt:=low(ShortInt);
      DefAShortInt:=AShortInt+1;
      AWord:=low(word);
      DefAWord:=AWord+1;
      ASmallInt:=low(SmallInt);
      DefASmallInt:=ASmallInt+1;
      ALongWord:=low(LongWord);
      DefALongWord:=ALongWord+1;
      ALongInt:=low(LongInt);
      DefALongInt:=ALongInt+1;
      AQWord:=low(qword);
      DefAQWord:=AQWord+1;
      AInt64:=low(Int64);
      DefAInt64:=AInt64+1;
      ACurrency:=MinSafeIntCurrency;
      DefACurrency:=ACurrency+1;
      ASingle:=MinSafeIntSingle;
      DefASingle:=ASingle+1;
      ADouble:=MinSafeIntDouble;
      DefADouble:=ADouble+1;
      AChar:=low(char);
      DefAChar:=succ(AChar);
      AWideChar:=low(WideChar);
      DefAWideChar:=succ(AWideChar);
      // ToDo: extended
    end;
    TestWriteDescendant('TestBaseTypesMinValues',AComponent,nil,[
    'ABoolean:=False;',
    'AByteBool:=False;',
    'AWordBool:=False;',
    'ALongBool:=False;',
    'AByte:=0;',
    'AShortInt:=-128;',
    'AWord:=0;',
    'ASmallInt:=-32768;',
    'ALongWord:=0;',
    'ALongInt:=-2147483648;',
    'AInt64:=-9223372036854775808;',
    'ACurrency:=-9.22337203685477E14;',
    'ASingle:=-1.6777216E7;',
    'ADouble:=-4.503599627370496E15;',
    'AChar:=#0;',
    'AWideChar:=#0;',
    '']);
  finally
    AComponent.Free;
  end;
end;

procedure TTestCompReaderWriterPas.TestBaseTypesMaxValues;
var
  AComponent: TCompBaseTypesCustomStored;
begin
  AComponent:=TCompBaseTypesCustomStored.Create(nil);
  try
    with AComponent do begin
      Name:=AComponent.ClassName+'1';
      ABoolean:=high(boolean);
      DefABoolean:=not ABoolean;
      AByteBool:=boolean(high(byte));
      DefAByteBool:=not AByteBool;
      AWordBool:=boolean(high(word));
      DefAWordBool:=not AWordBool;
      ALongBool:=boolean(high(longword));
      DefALongBool:=not ALongBool;
      AByte:=high(byte);
      DefAByte:=AByte-1;
      AShortInt:=high(ShortInt);
      DefAShortInt:=AShortInt-1;
      AWord:=high(word);
      DefAWord:=AWord-1;
      ASmallInt:=high(SmallInt);
      DefASmallInt:=ASmallInt-1;
      ALongWord:=high(LongWord);
      DefALongWord:=ALongWord-1;
      ALongInt:=high(LongInt);
      DefALongInt:=ALongInt-1;
      AQWord:=high(qword);
      DefAQWord:=AQWord-1;
      AInt64:=high(Int64);
      DefAInt64:=AInt64-1;
      ACurrency:=MaxSafeIntCurrency;
      DefACurrency:=ACurrency-1;
      ASingle:=MaxSafeIntSingle;
      DefASingle:=ASingle-1;
      ADouble:=MaxSafeIntDouble;
      DefADouble:=ADouble-1;
      AChar:=high(char);
      DefAChar:=pred(AChar);
      AWideChar:=high(WideChar);
      DefAWideChar:=pred(AWideChar);
      // ToDo: extended
    end;
    TestWriteDescendant('TestBaseTypesMaxValues',AComponent,nil,[
    'ABoolean:=True;',
    'AByteBool:=True;',
    'AWordBool:=True;',
    'ALongBool:=True;',
    'AByte:=255;',
    'AShortInt:=127;',
    'AWord:=65535;',
    'ASmallInt:=32767;',
    'ALongWord:=4294967295;',
    'ALongInt:=2147483647;',
    'AQWord:=18446744073709551615;',
    'AInt64:=9223372036854775807;',
    'ACurrency:=9.22337203685477E14;',
    'ASingle:=1.6777216E7;',
    'ADouble:=4.503599627370495E15;',
    'AChar:=#255;',
    'AWideChar:=#65535;',
    '']);
  finally
    AComponent.Free;
  end;
end;

procedure TTestCompReaderWriterPas.TestStringASCII;
var
  AComponent: TCompBaseTypes;
begin
  AComponent:=TCompBaseTypes.Create(nil);
  try
    with AComponent do begin
      Name:=AComponent.ClassName+'1';
      AString:=#9'A'#13#10;
    end;
    TestWriteDescendant('TestStringASCII',AComponent,nil,[
    'AString:=#9''A''#13#10;']);
  finally
    AComponent.Free;
  end;
end;

procedure TTestCompReaderWriterPas.TestStringUTF8;
var
  AComponent: TCompBaseTypes;
begin
  AComponent:=TCompBaseTypes.Create(nil);
  try
    with AComponent do begin
      Name:=AComponent.ClassName+'1';
      AString:='äöü';
      AShortString:='äöü';
    end;
    TestWriteDescendant('TestStringUTF8',AComponent,nil,[
    'AString:=''äöü'';',
    'AShortString:=''äöü'';',
    '']);
  finally
    AComponent.Free;
  end;
end;

procedure TTestCompReaderWriterPas.TestWideString_SrcCodePageSystem;
var
  AComponent: TCompBaseTypes;
begin
  AComponent:=TCompBaseTypes.Create(nil);
  try
    with AComponent do begin
      Name:=AComponent.ClassName+'1';
      AWideString:=UTF8ToUTF16('äAöü');
      AUnicodeString:=UTF8ToUTF16('äöBCü');
    end;
    TestWriteDescendant('TestWideString_SrcCodePageSystem',AComponent,nil,[
    'AWideString:=#0228''A''#0246#0252;',
    'AUnicodeString:=#0228#0246''BC''#0252;',
    '']);
  finally
    AComponent.Free;
  end;
end;

procedure TTestCompReaderWriterPas.TestWideString_SrcCodePageUTF8;
var
  AComponent: TCompBaseTypes;
begin
  Writer.Options:=Writer.Options+[cwpoSrcCodepageUTF8];
  AComponent:=TCompBaseTypes.Create(nil);
  try
    with AComponent do begin
      Name:=AComponent.ClassName+'1';
      AWideString:=UTF8ToUTF16('äöü');
      AUnicodeString:=UTF8ToUTF16('äöü');
    end;
    TestWriteDescendant('TestWideString_SrcCodePageUTF8',AComponent,nil,[
    'AWideString:=''äöü'';',
    'AUnicodeString:=''äöü'';',
    '']);
  finally
    AComponent.Free;
  end;
end;

initialization
  RegisterTest(TTestCompReaderWriterPas);
end.

