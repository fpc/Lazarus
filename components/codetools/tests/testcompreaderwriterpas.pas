{
 Test with:
     ./runtests --format=plain --suite=TTestCompReaderWriterPas
     ./runtests --format=plain --suite=TTestCompReaderWriterPas.TestBaseTypesMaxValues

Working:
- boolean
- integer
- strings, codepage system and UTF8
- float, currency
- enum, custom enum range
- set of enum, set of custom enum range
- variant
- method
- persistent
- root children
- collection

ToDo:
- enum: add unit, avoid nameclash with-do
- set of boolean
- set of char
- custom integer TColor, add unit, avoid nameclash with-do
- method, avoid nameclash with-do
- reference not yet created child component -> delay property setter
- ancestor
- ancestor: childpos
- ancestor: change parent
- ancestor: change name
- inline component
- reference foreign root
- reference foreign component
- TComponent.Left/Right
- DefineProperties
- tkInterface
- optional: use SetParentComponent instead of Parent:=
}
unit TestCompReaderWriterPas;

{$mode objfpc}{$H+}

{$DEFINE VerboseCompWriterPas}

interface

uses
  Classes, SysUtils, typinfo, RtlConsts, LazLoggerBase, LazUTF8, fpcunit,
  testregistry, CodeToolManager, LinkScanner, TestStdCodetools, variants;

const
  CWPDefaultSignature = '// Pascal stream V1.0';
type
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
    FIgnoreChildren: Boolean;
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
    FCurrentPos: Integer;
    FOnFindAncestor: TCWPFindAncestorEvent;
    procedure AddToAncestorList(Component: TComponent);
    procedure DetermineAncestor(Component: TComponent);
    procedure DoFindAncestor(Component: TComponent);
    procedure SetRoot(const AValue: TComponent);
    procedure WriteComponentData(Instance: TComponent);
    procedure WriteChildren(Component: TComponent);
    procedure WriteProperty(Instance: TPersistent; PropInfo: PPropInfo);
    procedure WriteProperties(Instance: TPersistent);
    procedure WriteCollection(PropName: string; Collection: TCollection);
    function GetComponentPath(Component: TComponent): string;
    function GetBoolLiteral(b: boolean): string;
    function GetStringLiteral(const s: string): string;
    function GetWStringLiteral(p: PWideChar; Count: integer): string;
    function GetFloatLiteral(const e: Extended): string;
    function GetCurrencyLiteral(const c: currency): string;
    function ShortenFloat(s: string): string;
    function GetEnumExpr(TypeInfo: PTypeInfo; Value: integer;
      AllowOutOfRange: boolean): string;
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
    property IgnoreChildren: Boolean read FIgnoreChildren write FIgnoreChildren;
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
    procedure OnClick(Sender: TObject);
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
    FEvent: TNotifyEvent;
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
    function EventIsStored: Boolean;
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
    DefEvent: TNotifyEvent;
    constructor Create(AOwner: TComponent); override;
  published
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
    property Event: TNotifyEvent read FEvent write FEvent stored EventIsStored;
  end;

  { TCompVariants }

  TCompVariants = class(TComponent)
  private
    FV1: variant;
    FV10: variant;
    FV11: variant;
    FV12: variant;
    FV13: variant;
    FV14: variant;
    FV15: variant;
    FV16: variant;
    FV17: variant;
    FV18: variant;
    FV19: variant;
    FV2: variant;
    FV20: variant;
    FV3: variant;
    FV4: variant;
    FV5: variant;
    FV6: variant;
    FV7: variant;
    FV8: variant;
    FV9: variant;
  published
    property V1: variant read FV1 write FV1;
    property V2: variant read FV2 write FV2;
    property V3: variant read FV3 write FV3;
    property V4: variant read FV4 write FV4;
    property V5: variant read FV5 write FV5;
    property V6: variant read FV6 write FV6;
    property V7: variant read FV7 write FV7;
    property V8: variant read FV8 write FV8;
    property V9: variant read FV9 write FV9;
    property V10: variant read FV10 write FV10;
    property V11: variant read FV11 write FV11;
    property V12: variant read FV12 write FV12;
    property V13: variant read FV13 write FV13;
    property V14: variant read FV14 write FV14;
    property V15: variant read FV15 write FV15;
    property V16: variant read FV16 write FV16;
    property V17: variant read FV17 write FV17;
    property V18: variant read FV18 write FV18;
    property V19: variant read FV19 write FV19;
    property V20: variant read FV20 write FV20;
  end;

  { TPersistentSimple }

  TPersistentSimple = class(TPersistent)
  private
    FSize: longint;
    FSub: TPersistentSimple;
  published
    property Size: longint read FSize write FSize default 0;
    property Sub: TPersistentSimple read FSub write FSub;
  end;

  { TCompPropPersistent }

  TCompPropPersistent = class(TComponent)
    procedure OnA(Sender: TObject);
    procedure OnB(Sender: TObject);
    procedure OnC(Sender: TObject);
  private
    FAfter: longint;
    FBefore: longint;
    FMiddle: longint;
    FOnClick: TNotifyEvent;
    FSub: TPersistentSimple;
    FSub2: TPersistentSimple;
  published
    property Before: longint read FBefore write FBefore default 0;
    property Sub: TPersistentSimple read FSub write FSub;
    property Middle: longint read FMiddle write FMiddle default 0;
    property Sub2: TPersistentSimple read FSub2 write FSub2;
    property After: longint read FAfter write FAfter default 0;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  { TSimpleControl }

  TSimpleControl = class(TComponent)
    procedure OnA(Sender: TObject);
    procedure OnB(Sender: TObject);
    procedure OnC(Sender: TObject);
  private
    FChildren: TFPList;
    FNext: TSimpleControl;
    FOnClick: TNotifyEvent;
    FParent: TSimpleControl;
    FSub: TPersistentSimple;
    procedure SetParent(const AValue: TSimpleControl);
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SetParentComponent(Value: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetParentComponent: TComponent; override;
    property Parent: TSimpleControl read FParent write SetParent;
  published
    property Next: TSimpleControl read FNext write FNext;
    property Sub: TPersistentSimple read FSub write FSub;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  { TSimpleCollectionItem }

  TSimpleCollectionItem = class(TCollectionItem)
  private
    FBefore: longint;
    FOnClick: TNotifyEvent;
    FSub: TPersistentSimple;
  published
    property Before: longint read FBefore write FBefore default 0;
    property Sub: TPersistentSimple read FSub write FSub;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TSimpleCollection = class(TCollection)
  private
    function GetThings(Index: integer): TSimpleCollectionItem;
  public
    property Things[Index: integer]: TSimpleCollectionItem read GetThings; default;
  end;

  { TSimpleControlWithCollection }

  TSimpleControlWithCollection = class(TSimpleControl)
  private
    FItems: TSimpleCollection;
    procedure SetItems(const AValue: TSimpleCollection);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TSimpleCollection read FItems write SetItems;
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
    procedure TestVariant;
    procedure TestPropPersistent;
    procedure TestAncestor;
    procedure TestChildComponent;
    procedure TestCollection;
  end;

implementation

function CreateRootName(aComponent: TComponent): string;
begin
  Result:=aComponent.ClassName;
  Delete(Result,1,1);
  Result:=Result+'1';
end;

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
  TAccessComp = class(TComponent); // to access TComponent protected members

  { TPosComponent }

  TPosComponent = class(TObject)
    FPos: Integer;
    FComponent: TComponent;
    constructor Create(APos: Integer; AComponent: TComponent);
  end;

{ TSimpleCollection }

function TSimpleCollection.GetThings(Index: integer): TSimpleCollectionItem;
begin
  Result:=TSimpleCollectionItem(Items[Index]);
end;

{ TSimpleControlWithCollection }

procedure TSimpleControlWithCollection.SetItems(const AValue: TSimpleCollection
  );
begin
  if FItems=AValue then Exit;
  FItems.Assign(AValue);
end;

constructor TSimpleControlWithCollection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems:=TSimpleCollection.Create(TSimpleCollectionItem);
end;

destructor TSimpleControlWithCollection.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

{ TSimpleControl }

procedure TSimpleControl.OnA(Sender: TObject);
begin
  if Sender=nil then ;
end;

procedure TSimpleControl.OnB(Sender: TObject);
begin
  if Sender=nil then ;
end;

procedure TSimpleControl.OnC(Sender: TObject);
begin
  if Sender=nil then ;
end;

procedure TSimpleControl.SetParent(const AValue: TSimpleControl);
begin
  if FParent=AValue then Exit;
  if FParent<>nil then
    FParent.FChildren.Remove(Self);
  FParent:=AValue;
  if FParent<>nil then
    FParent.FChildren.Add(Self);
end;

procedure TSimpleControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  if Root=nil then ;
  for i:=0 to FChildren.Count-1 do
    Proc(TComponent(FChildren[i]));
end;

procedure TSimpleControl.SetParentComponent(Value: TComponent);
begin
  Parent:=Value as TSimpleControl;
end;

constructor TSimpleControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChildren:=TFPList.Create;
end;

destructor TSimpleControl.Destroy;
var
  i: Integer;
begin
  for i:=FChildren.Count-1 downto 0 do
    TSimpleControl(FChildren[i]).Parent:=nil;
  FreeAndNil(FChildren);
  inherited Destroy;
end;

function TSimpleControl.GetParentComponent: TComponent;
begin
  Result:=FParent;
end;

{ TCompPropPersistent }

procedure TCompPropPersistent.OnA(Sender: TObject);
begin
  if Sender=nil then ;
end;

procedure TCompPropPersistent.OnB(Sender: TObject);
begin
  if Sender=nil then ;
end;

procedure TCompPropPersistent.OnC(Sender: TObject);
begin
  if Sender=nil then ;
end;

{ TPosComponent }

constructor TPosComponent.Create(APos: Integer; AComponent: TComponent);
begin
  FPos:=APos;
  FComponent:=AComponent;
end;

{ TCompBaseTypesCustomStored }

procedure TCompBaseTypesCustomStored.OnClick(Sender: TObject);
begin
  if Sender=nil then ;
end;

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

function TCompBaseTypesCustomStored.EventIsStored: Boolean;
begin
  Result:=TMethod(FEvent).Code<>TMethod(DefEvent).Code;
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

procedure TCompWriterPas.AddToAncestorList(Component: TComponent);
begin
  FAncestors.AddObject(Component.Name,TPosComponent.Create(FAncestors.Count,Component));
end;

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
var
  HasAncestor: Boolean;

  procedure WriteSetParent;
  var
    DefParent: TComponent;
  begin
    if HasAncestor and (Ancestor is TComponent) then
    begin
      DefParent:=TComponent(Ancestor).GetParentComponent;
      if Assigned(DefParent) then
      begin
        if DefParent=FRootAncestor then
          DefParent:=Root
        else if (DefParent.Owner = FRootAncestor) and
            (Parent.Owner = Root) and
            (CompareText(DefParent.Name,Parent.Name)=0) then
          DefParent:=Parent;
      end;
    end else
      DefParent:=nil;
    if Parent=DefParent then exit;
    WriteAssign('Parent',GetComponentPath(Parent));
  end;

begin
  HasAncestor := Assigned(Ancestor) and ((Instance = Root) or
    (Instance.ClassType = Ancestor.ClassType));
  if Instance=LookupRoot then
    WriteAssign('Name',''''+Instance.Name+'''')
  else begin
    if not HasAncestor then
      WriteAssign(Instance.Name,Instance.ClassName+'.Create(Self)');
    WriteIndent;
    Write('with ');
    Write(Instance.Name);
    Write(' do begin');
    WriteLn;
    Indent;
    if not HasAncestor then
      WriteAssign('Name',''''+Instance.Name+'''');
    if cwpoSetParentFirst in Options then
      WriteSetParent;
  end;
  WriteProperties(Instance);
  if Instance<>LookupRoot then
  begin
    if not (cwpoSetParentFirst in Options) then
      WriteSetParent;
    Unindent;
    WriteIndent;
    Write('end;');
    WriteLn;
  end;
  if not IgnoreChildren then
    WriteChildren(Instance);
end;

procedure TCompWriterPas.WriteChildren(Component: TComponent);
var
  SRoot, SRootA, SParent: TComponent;
  SList: TStringList;
  SPos, i: Integer;
begin
  // Write children list.
  // While writing children, the ancestor environment must be saved
  // This is recursive...
  SRoot:=FRoot;
  SRootA:=FRootAncestor;
  SList:=FAncestors;
  SPos:=FCurrentPos;
  SParent:=Parent;
  try
    FAncestors:=Nil;
    FCurrentPos:=0;
    FAncestorPos:=-1;
    FParent:=Component;
    if csInline in Component.ComponentState then
      FRoot:=Component;
    if (FAncestor is TComponent) then
    begin
      FAncestors:=TStringList.Create;
      if csInline in TComponent(FAncestor).ComponentState then
        FRootAncestor := TComponent(FAncestor);
      TAccessComp(FAncestor).GetChildren(@AddToAncestorList,FRootAncestor);
      FAncestors.Sorted:=True;
    end;
    try
      TAccessComp(Component).GetChildren(@WriteComponent, FRoot);
    finally
      if Assigned(FAncestor) then
        for i:=0 to FAncestors.Count-1 do
          FAncestors.Objects[i].Free;
      FreeAndNil(FAncestors);
    end;
  finally
    FParent:=SParent;
    FAncestors:=SList;
    FRoot:=SRoot;
    FRootAncestor:=SRootA;
    FCurrentPos:=SPos;
    FAncestorPos:=SPos;
  end;
end;

procedure TCompWriterPas.WriteProperty(Instance: TPersistent;
  PropInfo: PPropInfo);
type
  TSet = set of 0..31;
var
  PropType, CompType: PTypeInfo;
  ObjValue, AncestorObj: TObject;
  HasAncestor, BoolValue, DefBoolValue: Boolean;
  Int32Value, DefValue: longint;
  PropName, Ident, s, StrValue, DefStrValue, Name, SavedPropPath: String;
  IntToIdentFn: TIntToIdent;
  i, j: Integer;
  Int64Value, DefInt64Value: Int64;
  FloatValue, DefFloatValue: Extended;
  MethodValue, DefMethodValue: TMethod;
  WStrValue, WDefStrValue: WideString;
  UStrValue, UDefStrValue: UnicodeString;
  VarValue, DefVarValue: tvardata;
  aTypeData: PTypeData;
  Component, C: TComponent;
  SavedAncestor: TPersistent;
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
  debugln(['TWriter.WriteProperty PropName="',PropName,'" TypeName=',PropType^.Name,' Kind=',GetEnumName(TypeInfo(TTypeKind),ord(PropType^.Kind)),' HasAncestor=',HasAncestor]);

  case PropType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkSet, tkWChar:
      begin
        Int32Value := GetOrdProp(Instance, PropInfo);
        if HasAncestor then
          DefValue := GetOrdProp(Ancestor, PropInfo)
        else
          DefValue := PPropInfo(PropInfo)^.Default;
        //debugln([PropInfo^.Name,', HasAncestor=',HasAncestor,', Value=',Int32Value,', Default=',DefValue]);
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
                  s:=s+GetEnumExpr(CompType, i,false);
                  j:=i;
                  while (i<31) and (byte(i+1) in TSet(Int32Value)) do
                    inc(i);
                  if i>j then
                    s:=s+'..'+GetEnumExpr(CompType, i,false);
                end;
                inc(i);
              end;
              WriteAssign(PropName,'['+s+']');
              end;
            tkEnumeration:
              // ToDo: store needed unit
              WriteAssign(PropName,GetEnumExpr(PropType, Int32Value,true));
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

        //debugln(['TCompWriterPas.WriteProperty ',dbgs(MethodValue.Data),' ',dbgs(MethodValue.Code),' ',dbgs(DefMethodValue.Data),' ',dbgs(DefMethodValue.Code)]);
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
                // ToDo: check nameclash of Ident with current with-do block
                WriteAssign(PropName,'@'+Ident);
            end;
          end;
        end else begin
          if (MethodValue.Code <> DefMethodValue.Code) then
          begin
            if not Assigned(MethodValue.Code) then
              Ident:=''
            else
              Ident:=FLookupRoot.MethodName(MethodValue.Code);
            if Ident='' then
              WriteAssign(PropName,'nil')
            else
              // ToDo: check nameclash of Ident with current with-do block
              WriteAssign(PropName,'@'+Ident);
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
        // Ensure that a Variant manager is installed
        if not Assigned(VarClearProc) then
          raise EWriteError.Create(SErrNoVariantSupport);

        VarValue := tvardata(GetVariantProp(Instance, PropInfo));
        if HasAncestor then
          DefVarValue := tvardata(GetVariantProp(Ancestor, PropInfo))
        else
          FillChar(DefVarValue,sizeof(DefVarValue),0);

        if (CompareByte(VarValue,DefVarValue,sizeof(VarValue)) <> 0) then
          begin
            // can't use variant() typecast, pulls in variants unit
            case VarValue.vtype of
            varsmallint : WriteAssign(PropName,'SmallInt('+IntToStr(VarValue.vsmallint)+')');
            varinteger : WriteAssign(PropName,'LongInt('+IntToStr(VarValue.vinteger)+')');
            varsingle : WriteAssign(PropName,'Single('+GetFloatLiteral(VarValue.vsingle)+')');
            vardouble : WriteAssign(PropName,'Double('+GetFloatLiteral(VarValue.vdouble)+')');
            vardate : WriteAssign(PropName,'TDateTime('+GetFloatLiteral(VarValue.vdate)+')');
            varcurrency : WriteAssign(PropName,'Currency('+GetCurrencyLiteral(VarValue.vcurrency)+')');
            //varolestr : (volestr : pwidechar);
            //vardispatch : (vdispatch : pointer);
            //varerror : (verror : hresult);
            varboolean : WriteAssign(PropName,GetBoolLiteral(VarValue.vboolean));
            //varunknown : (vunknown : pointer);
            // vardecimal : ( : );
            varshortint : WriteAssign(PropName,'ShortInt('+IntToStr(VarValue.vshortint)+')');
            varbyte : WriteAssign(PropName,'Byte('+IntToStr(VarValue.vbyte)+')');
            varword : WriteAssign(PropName,'Word('+IntToStr(VarValue.vword)+')');
            varlongword : WriteAssign(PropName,'LongWord('+IntToStr(VarValue.vlongword)+')');
            varint64 : WriteAssign(PropName,'Int64('+IntToStr(VarValue.vint64)+')');
            varqword : WriteAssign(PropName,'QWord('+IntToStr(VarValue.vqword)+')');
            // duplicate: varword64
            varstring : WriteAssign(PropName,GetStringLiteral(AnsiString(VarValue.vstring)));
            //varany :  (vany : pointer);
            //vararray : (varray : pvararray);
            //varbyref : (vpointer : pointer);
            //varrecord : (vrecord : pointer;precinfo : pointer);
            else
              {$IFDEF VerboseCompWriterPas}
              debugln(['TCompWriterPas.WriteProperty Property="',PropName,'" Kind=',PropType^.Kind,' vtype=',VarValue.vtype]);
              raise EWriteError.Create('proptype not supported: '+GetEnumName(TypeInfo(PropType^.Kind),ord(PropType^.Kind))+' vtype='+dbgs(VarValue.vtype));
              {$ENDIF}
            end;
            //ToDo WriteVariant(pvariant(@VarValue)^);
          end;
      end;
    tkClass:
      begin
        ObjValue := TObject(GetObjectProp(Instance, PropInfo));
        if HasAncestor then
        begin
          AncestorObj := TObject(GetObjectProp(Ancestor, PropInfo));
          if (AncestorObj is TComponent) and
             (ObjValue is TComponent) then
          begin
            //debugln(['TWriter.WriteProperty AncestorObj=',TComponent(AncestorObj).Name,' OwnerFit=',TComponent(AncestorObj).Owner = FRootAncestor,' ',TComponent(ObjValue).Name,' OwnerFit=',TComponent(ObjValue).Owner = Root]);
            if (AncestorObj<>ObjValue) and
               (TComponent(AncestorObj).Owner = FRootAncestor) and
               (TComponent(ObjValue).Owner = Root) and
               (CompareText(TComponent(AncestorObj).Name,TComponent(ObjValue).Name)=0) then
            begin
              // different components, but with the same name
              // -> keep property value
              AncestorObj := ObjValue;
            end;
          end;
        end else
          AncestorObj := nil;

        if not Assigned(ObjValue) then
        begin
          if ObjValue <> AncestorObj then
            WriteAssign(PropName,'Nil');
        end
        else if ObjValue.InheritsFrom(TPersistent) then
        begin
          // Subcomponents are streamed the same way as persistents
          if ObjValue.InheritsFrom(TComponent)
            and ((not (csSubComponent in TComponent(ObjValue).ComponentStyle))
                 or ((TComponent(ObjValue).Owner<>Instance) and (TComponent(ObjValue).Owner<>Nil))) then
          begin
            Component := TComponent(ObjValue);
            if (ObjValue <> AncestorObj)
                and not (csTransient in Component.ComponentStyle) then
            begin
              // set property value
              Name:= '';
              C:= Component;
              While (C<>Nil) and (C.Name<>'') do
              begin
                If (Name<>'') Then
                  Name:='.'+Name;
                if C.Owner = LookupRoot then
                begin
                  Name := C.Name+Name;
                  break;
                end
                else if C = LookupRoot then
                begin
                  Name := 'Self' + Name;
                  break;
                end;
                Name:=C.Name + Name;
                C:= C.Owner;
              end;
              if (C=nil) and (Component.Owner=nil) then
                if (Name<>'') then // Component is a foreign root
                  ; // Name:=Name+'.Owner';
              if Length(Name) > 0 then
                WriteAssign(PropName,Name);
            end; //(ObjValue <> AncestorObj)
          end // ObjValue.InheritsFrom(TComponent)
          else
          begin
            // keep property value, set sub properties recursively with full path
            // e.g. Font.Size:=5;
            SavedAncestor := Ancestor;
            SavedPropPath := FPropPath;
            try
              FPropPath := FPropPath + PPropInfo(PropInfo)^.Name + '.';
              if HasAncestor then
                Ancestor := TPersistent(GetObjectProp(Ancestor, PropInfo));
              WriteProperties(TPersistent(ObjValue));
            finally
              Ancestor := SavedAncestor;
              FPropPath := SavedPropPath;
            end;
            if ObjValue.InheritsFrom(TCollection) then
            begin
              if (not HasAncestor) or (not CollectionsEqual(TCollection(ObjValue),
                TCollection(GetObjectProp(Ancestor, PropInfo)),Root,RootAncestor)) then
              begin
                // create collection items
                SavedPropPath := FPropPath;
                try
                  SetLength(FPropPath, 0);
                  WriteCollection(PropName,TCollection(ObjValue));
                finally
                  FPropPath := SavedPropPath;
                end;
              end;
            end // TCollection
          end;
        end; // Inheritsfrom(TPersistent)
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
        //debugln([PropInfo^.Name,', HasAncestor=',HasAncestor,', BoolValue=',BoolValue,', DefBoolValue=',DefBoolValue,' Default=',DefValue]);
        if (BoolValue<>DefBoolValue) or (DefValue=longint($80000000)) then
          WriteAssign(PropName,GetBoolLiteral(BoolValue));
      end;
  else
    {$IFDEF VerboseCompWriterPas}
    debugln(['TCompWriterPas.WriteProperty Property="',PropName,'" Kind=',PropType^.Kind]);
    raise EWriteError.Create('proptype not supported: '+GetEnumName(TypeInfo(PropType^.Kind),ord(PropType^.Kind)));
    {$ENDIF}
  end;
end;

procedure TCompWriterPas.WriteProperties(Instance: TPersistent);
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

procedure TCompWriterPas.WriteCollection(PropName: string;
  Collection: TCollection);
var
  i: Integer;
  Item: TCollectionItem;
begin
  WriteIndent;
  Write(PropName+'.Clear;');
  WriteLn;
  for i:=0 to Collection.Count-1 do
  begin
    Item:=Collection.Items[i];
    WriteIndent;
    Write('with '+Item.ClassName+'('+PropName+'.Add) do begin');
    WriteLn;
    Indent;
    WriteProperties(Item);
    Unindent;
    WriteIndent;
    Write('end;');
    WriteLn;
  end;
end;

function TCompWriterPas.GetComponentPath(Component: TComponent): string;
begin
  if Component=nil then
    Result:='Nil'
  else if Component=LookupRoot then
    Result:='Self'
  else
    Result:=Component.Name;
end;

function TCompWriterPas.GetBoolLiteral(b: boolean): string;
begin
  if b then
    Result:='True'
  else
    Result:='False';
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
begin
  s:='';
  str(e,s);
  Result:=ShortenFloat(s);
end;

function TCompWriterPas.GetCurrencyLiteral(const c: currency): string;
var
  s: String;
begin
  s:='';
  str(c,s);
  Result:=ShortenFloat(s);
end;

function TCompWriterPas.ShortenFloat(s: string): string;
var
  p, i: SizeInt;
begin
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

function TCompWriterPas.GetEnumExpr(TypeInfo: PTypeInfo; Value: integer;
  AllowOutOfRange: boolean): string;
var
  PT: PTypeData;
begin
  PT:=GetTypeData(TypeInfo);
  if (Value>=PT^.MinValue) and (Value<=PT^.MaxValue) then
    Result:=GetEnumName(TypeInfo,Value)
  else if AllowOutOfRange then
    Result:=TypeInfo^.Name+'('+IntToStr(Value)+')'
  else
    raise EStreamError.Create('enum '+IntToStr(Value)+' is out of range of type "'+TypeInfo^.Name+'"');
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
    AComponent.Name:=CreateRootName(AComponent);
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
      Name:=CreateRootName(AComponent);
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
      // ToDo: extended
      AChar:=#0;
      DefAChar:=succ(AChar);
      AWideChar:=#0;
      DefAWideChar:=succ(AWideChar);
      Enum:=TEnum(0);
      DefEnum:=succ(Enum);
      EnumRg:=TEnumRg(0);
      DefEnumRg:=succ(EnumRg);
      SetOfEnum:=[];
      DefSetOfEnum:=[red];
      SetOfEnumRg:=[];
      DefSetOfEnumRg:=[red];
      Event:=nil;
      DefEvent:=@OnClick;
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
    'Enum:=red;',
    'EnumRg:=TEnumRg(0);',
    'SetOfEnum:=[];',
    'SetOfEnumRg:=[];',
    //'Event:=nil;', must not be written
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
      Name:=CreateRootName(AComponent);
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
      // ToDo: extended
      AChar:=low(char);
      DefAChar:=succ(AChar);
      AWideChar:=low(WideChar);
      DefAWideChar:=succ(AWideChar);
      Enum:=low(TEnum);
      DefEnum:=succ(Enum);
      EnumRg:=low(TEnumRg);
      DefEnumRg:=succ(EnumRg);
      SetOfEnum:=[];
      DefSetOfEnum:=[red];
      SetOfEnumRg:=[];
      DefSetOfEnumRg:=[red];
      Event:=@OnClick;
      DefEvent:=nil;
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
    'Enum:=red;',
    'EnumRg:=green;',
    'SetOfEnum:=[];',
    'SetOfEnumRg:=[];',
    'Event:=@OnClick;',
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
      Name:=CreateRootName(AComponent);
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
      // ToDo: extended
      AChar:=high(char);
      DefAChar:=pred(AChar);
      AWideChar:=high(WideChar);
      DefAWideChar:=pred(AWideChar);
      Enum:=high(TEnum);
      DefEnum:=pred(Enum);
      EnumRg:=high(TEnumRg);
      DefEnumRg:=pred(EnumRg);
      SetOfEnum:=[low(SetOfEnum)..high(SetOfEnum)];
      DefSetOfEnum:=[red];
      SetOfEnumRg:=[low(SetOfEnumRg)..high(SetOfEnumRg)];
      DefSetOfEnumRg:=[red];
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
    'Enum:=black;',
    'EnumRg:=white;',
    'SetOfEnum:=[red..black];',
    'SetOfEnumRg:=[green..white];',
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
      Name:=CreateRootName(AComponent);
      AString:=#9'A'#13#10;
      AShortString:=#9'A'#13#10;
    end;
    TestWriteDescendant('TestStringASCII',AComponent,nil,[
    'AString:=#9''A''#13#10;',
    'AShortString:=#9''A''#13#10;']);
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
      Name:=CreateRootName(AComponent);
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
      Name:=CreateRootName(AComponent);
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
      Name:=CreateRootName(AComponent);
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

procedure TTestCompReaderWriterPas.TestVariant;
var
  AComponent: TCompVariants;
begin
  Writer.Options:=Writer.Options+[cwpoSrcCodepageUTF8];
  AComponent:=TCompVariants.Create(nil);
  try
    with AComponent do begin
      Name:=CreateRootName(AComponent);
      V1:=high(byte);
      V2:=low(ShortInt);
      V3:=high(Word);
      V4:=low(SmallInt);
      V5:=high(LongWord);
      V6:=low(LongInt);
      V7:=high(QWord);
      V8:=low(int64);
      V9:=true;
      V10:='äöü';
      V11:=single(-1.25);
      V12:=double(1.5);
      V13:=currency(17.0001);
    end;
    TestWriteDescendant('TestVariant',AComponent,nil,[
    'V1:=Byte(255);',
    'V2:=ShortInt(-128);',
    'V3:=Word(65535);',
    'V4:=SmallInt(-32768);',
    'V5:=LongWord(4294967295);',
    'V6:=LongInt(-2147483648);',
    'V7:=QWord(18446744073709551615);',
    'V8:=Int64(-9223372036854775808);',
    'V9:=True;',
    'V10:=''äöü'';',
    'V11:=Double(-1.25);',
    'V12:=Double(1.5);',
    'V13:=Currency(1.70001E1);',
    '']);
  finally
    AComponent.Free;
  end;
end;

procedure TTestCompReaderWriterPas.TestPropPersistent;
var
  aRoot: TCompPropPersistent;
begin
  aRoot:=TCompPropPersistent.Create(nil);
  try
    with aRoot do begin
      Name:=CreateRootName(aRoot);
      Before:=1;
      Sub:=TPersistentSimple.Create;
      Sub.Size:=11;
      Middle:=2;
      Sub2:=TPersistentSimple.Create;
      Sub2.Size:=21;
      Sub2.Sub:=TPersistentSimple.Create;
      Sub2.Sub.Size:=211;
      After:=3;
    end;
    TestWriteDescendant('TestPropPersistent',aRoot,nil,[
    'Before:=1;',
    'Sub.Size:=11;',
    'Middle:=2;',
    'Sub2.Size:=21;',
    'Sub2.Sub.Size:=211;',
    'After:=3;',
    '']);
  finally
    FreeAndNil(aRoot.FSub2.FSub);
    FreeAndNil(aRoot.FSub2);
    FreeAndNil(aRoot.FSub);
    aRoot.Free;
  end;
end;

procedure TTestCompReaderWriterPas.TestAncestor;

  procedure InitAncestor(C: TSimpleControl);
  var
    Button1: TSimpleControl;
  begin
    C.Tag:=1;
    Button1:=TSimpleControl.Create(C);
    with Button1 do begin
      Name:='Button1';
      Tag:=2;
      OnClick:=@C.OnA;
      Parent:=C;
    end;
  end;

var
  aRoot, Ancestor: TSimpleControl;
begin
  Ancestor:=TSimpleControl.Create(nil);
  aRoot:=TSimpleControl.Create(nil);
  try
    with Ancestor do begin
      Name:='Ancestor';
    end;
    InitAncestor(Ancestor);

    with aRoot do begin
      Name:='Descendant';
    end;
    InitAncestor(aRoot);

    TestWriteDescendant('TestAncestor',aRoot,Ancestor,[
    'with Button1 do begin',
    'end;',
    '']);
  finally
    aRoot.Free;
    Ancestor.Free;
  end;
end;

procedure TTestCompReaderWriterPas.TestChildComponent;
var
  aRoot, Button1: TSimpleControl;
begin
  aRoot:=TSimpleControl.Create(nil);
  try
    with aRoot do begin
      Name:=CreateRootName(aRoot);
      Tag:=1;
    end;
    Button1:=TSimpleControl.Create(aRoot);
    with Button1 do begin
      Name:='Button1';
      Tag:=2;
      Parent:=aRoot;
    end;

    TestWriteDescendant('TestChildComponent',aRoot,nil,[
    'Tag:=1;',
    'Button1:=TSimpleControl.Create(Self);',
    'with Button1 do begin',
    '  Name:=''Button1'';',
    '  Tag:=2;',
    '  Parent:=Self;',
    'end;',
    '']);
  finally
    aRoot.Free;
  end;
end;

procedure TTestCompReaderWriterPas.TestCollection;
var
  aRoot: TSimpleControlWithCollection;
begin
  aRoot:=TSimpleControlWithCollection.Create(nil);
  try
    with aRoot do begin
      Name:=CreateRootName(aRoot);
      Tag:=1;
      with TSimpleCollectionItem(Items.Add) do begin
        OnClick:=@OnA;
        Sub:=TPersistentSimple.Create;
        Sub.Size:=11;
      end;
      with TSimpleCollectionItem(Items.Add) do begin
        Sub:=TPersistentSimple.Create;
        Sub.Size:=12;
      end;
    end;

    TestWriteDescendant('TestCollection',aRoot,nil,[
    'Tag:=1;',
    'Items.Clear;',
    'with TSimpleCollectionItem(Items.Add) do begin',
    '  Sub.Size:=11;',
    '  OnClick:=@OnA;',
    'end;',
    'with TSimpleCollectionItem(Items.Add) do begin',
    '  Sub.Size:=12;',
    'end;',
    '']);
  finally
    FreeAndNil(aRoot.Items[0].FSub);
    FreeAndNil(aRoot.Items[1].FSub);
    aRoot.Free;
  end;
end;

initialization
  RegisterTest(TTestCompReaderWriterPas);
end.

