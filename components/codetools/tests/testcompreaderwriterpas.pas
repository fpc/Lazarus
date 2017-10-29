{
 Test with:
     ./runtests --format=plain --suite=TTestCompReaderWriterPas
     ./runtests --format=plain --suite=TTestCompReaderWriterPas.TestBaseTypesMaxValues

ToDo:
- enum: add unit
- enum: avoid nameclash with-do
- custom integer TColor: add unit
- custom integer: avoid nameclash with-do
- method: avoid nameclash with-do
- insert/update code and helper class into unit/program
  - find call init proc
  - find old init code
  - error if init proc is behind call
  - add call in existing constructor
  - add constructor with call
  - add new init code
  - replace init code
  - add missing units
}
unit TestCompReaderWriterPas;

{$mode objfpc}{$H+}

{off $DEFINE VerboseCompWriterPas}

interface

uses
  Classes, SysUtils, typinfo, LazLoggerBase, LazUTF8, LazLogger, CompWriterPas,
  LazPasReadUtil, fpcunit, testregistry, CodeToolManager, LinkScanner,
  CodeToolsStructs, CodeCache, BasicCodeTools, TestStdCodetools, TestGlobals,
  variants;

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
  TSetOfBool = set of boolean;
  TMyInt = 1..7;
  TSetOfMyInt = set of TMyInt;
  TMyChar = #3..#10;
  TSetOfMyChar = set of TMyChar;

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
    FMyChar: TMyChar;
    FMyInt: TMyInt;
    FSetOfBool: TSetOfBool;
    FSetOfEnum: TSetOfEnum;
    FSetOfEnumRg: TSetOfEnumRg;
    FSetOfMyChar: TSetOfMyChar;
    FSetOfMyInt: TSetOfMyInt;
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
    property SetOfBool: TSetOfBool read FSetOfBool write FSetOfBool default [];
    property MyInt: TMyInt read FMyInt write FMyInt default low(TMyInt);
    property SetOfMyInt: TSetOfMyInt read FSetOfMyInt write FSetOfMyInt default [];
    property MyChar: TMyChar read FMyChar write FMyChar default low(TMyChar);
    property SetOfMyChar: TSetOfMyChar read FSetOfMyChar write FSetOfMyChar default [];
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
    FMyChar: TMyChar;
    FMyInt: TMyInt;
    FSetOfBool: TSetOfBool;
    FSetOfEnum: TSetOfEnum;
    FSetOfEnumRg: TSetOfEnumRg;
    FSetOfMyChar: TSetOfMyChar;
    FSetOfMyInt: TSetOfMyInt;
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
    function MyCharIsStored: Boolean;
    function MyIntIsStored: Boolean;
    function SetOfBoolIsStored: Boolean;
    function SetOfEnumIsStored: Boolean;
    function SetOfEnumRgIsStored: Boolean;
    function SetOfMyCharIsStored: Boolean;
    function SetOfMyIntIsStored: Boolean;
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
    DefSetOfBool: TSetOfBool;
    DefMyInt: TMyInt;
    DefSetOfMyInt: TSetOfMyInt;
    DefMyChar: TMyChar;
    DefSetOfMyChar: TSetOfMyChar;
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
    property SetOfBool: TSetOfBool read FSetOfBool write FSetOfBool stored SetOfBoolIsStored;
    property MyInt: TMyInt read FMyInt write FMyInt stored MyIntIsStored;
    property SetOfMyInt: TSetOfMyInt read FSetOfMyInt write FSetOfMyInt stored SetOfMyIntIsStored;
    property MyChar: TMyChar read FMyChar write FMyChar stored MyCharIsStored;
    property SetOfMyChar: TSetOfMyChar read FSetOfMyChar write FSetOfMyChar stored SetOfMyCharIsStored;
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
    function GetControls(Index: integer): TSimpleControl;
    procedure SetParent(const AValue: TSimpleControl);
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SetParentComponent(Value: TComponent); override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetParentComponent: TComponent; override;
    property Parent: TSimpleControl read FParent write SetParent;
    function ControlCount: integer;
    property Controls[Index: integer]: TSimpleControl read GetControls;
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

  { TSimpleControlWithInterface }

  TSimpleControlWithInterface = class(TSimpleControl, IInterfaceComponentReference)
  private
    FIntf: IInterfaceComponentReference;
  public
    function GetComponent: TComponent;
  published
    property Intf: IInterfaceComponentReference read FIntf write FIntf;
  end;

  { TSimpleControlWithStrings }

  TSimpleControlWithStrings = class(TSimpleControl)
  private
    FLines: TStrings;
  published
    property Lines: TStrings read FLines write FLines;
  end;

  { TTestCompReaderWriterPas }

  TTestCompReaderWriterPas = class(TCustomTestCTStdCodetools)
  private
    FStream: TMemoryStream;
    FWriter: TCompWriterPas;
    FAncestors: TPointerToPointerTree;
    procedure OnDefinePropertiesTStrings(Writer: TCompWriterPas;
      Instance: TPersistent; const Identifier: string; var Handled: boolean);
    procedure OnWriterFindAncestor(Sender: TCompWriterPas; Component: TComponent;
      const Name: string; var Ancestor, RootAncestor: TComponent);
    procedure OnWriterGetParentProperty(Sender: TCompWriterPas; Component: TComponent;
      var PropName: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function WriteDescendant(Component: TComponent; Ancestor: TComponent = nil): string;
    procedure TestWriteDescendant(Msg: string; Component: TComponent;
      Ancestor: TComponent; const Expected: array of string; NeedAccessClass: boolean = false);
    property Writer: TCompWriterPas read FWriter write FWriter;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AddAncestor(Component, Ancestor: TComponent);
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
    procedure TestInterface;
    procedure TestAncestor;
    procedure TestAncestorChildPos;
    procedure TestWithLookupRootName;
    procedure TestChildComponents;
    procedure TestChildComponentsNoWith;
    procedure TestForeignReference;
    procedure TestCollection;
    procedure TestInline; // e.g. a Frame on a Form
    procedure TestAncestorWithInline; // e.g. a Form inherited from a Form with a Frame
    procedure TestInlineDescendant; // e.g. a Form with a Frame, Frame is inherited from another Frame

    procedure TestDesignInfo;
    procedure TestDefineProperties_ListOfStrings;
    procedure Test_TStrings;

    procedure TestFindComponentInit; // ToDo
  end;

implementation

type
  TAccessComp = class(TComponent);

function CreateRootName(aComponent: TComponent): string;
begin
  Result:=aComponent.ClassName;
  Delete(Result,1,1);
  Result:=Result+'1';
end;

{ TSimpleControlWithInterface }

function TSimpleControlWithInterface.GetComponent: TComponent;
begin
  Result:=Self;
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

function TSimpleControl.GetControls(Index: integer): TSimpleControl;
begin
  Result:=TSimpleControl(FChildren[INdex]);
end;

procedure TSimpleControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  if Root=nil then ;
  for i:=0 to ControlCount-1 do
    Proc(Controls[i]);
end;

procedure TSimpleControl.SetParentComponent(Value: TComponent);
begin
  Parent:=Value as TSimpleControl;
end;

procedure TSimpleControl.SetChildOrder(Child: TComponent; Order: Integer);
begin
  FChildren.Move(FChildren.IndexOf(Child),Order);
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

function TSimpleControl.ControlCount: integer;
begin
  Result:=FChildren.Count;
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

function TCompBaseTypesCustomStored.MyCharIsStored: Boolean;
begin
  Result:=MyChar<>DefMyChar;
end;

function TCompBaseTypesCustomStored.MyIntIsStored: Boolean;
begin
  Result:=FMyInt<>DefMyInt;
end;

function TCompBaseTypesCustomStored.SetOfBoolIsStored: Boolean;
begin
  Result:=FSetOfBool<>DefSetOfBool;
end;

function TCompBaseTypesCustomStored.SetOfEnumIsStored: Boolean;
begin
  Result:=FSetOfEnum<>DefSetOfEnum;
end;

function TCompBaseTypesCustomStored.SetOfEnumRgIsStored: Boolean;
begin
  Result:=FSetOfEnumRg<>DefSetOfEnumRg;
end;

function TCompBaseTypesCustomStored.SetOfMyCharIsStored: Boolean;
begin
  Result:=SetOfMyChar<>DefSetOfMyChar;
end;

function TCompBaseTypesCustomStored.SetOfMyIntIsStored: Boolean;
begin
  Result:=FSetOfMyInt<>DefSetOfMyInt;
end;

constructor TCompBaseTypesCustomStored.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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
  MyInt:=low(TMyInt);
  MyChar:=low(TMyChar);
end;

{ TTestCompReaderWriterPas }

procedure TTestCompReaderWriterPas.OnWriterFindAncestor(Sender: TCompWriterPas;
  Component: TComponent; const Name: string; var Ancestor,
  RootAncestor: TComponent);
var
  C: TComponent;
begin
  if Name='' then ;
  C:=TComponent(FAncestors[Component]);
  if C=nil then exit;
  Ancestor:=C;
  if C.Owner=nil then
    RootAncestor:=C;
end;

procedure TTestCompReaderWriterPas.OnDefinePropertiesTStrings(
  Writer: TCompWriterPas; Instance: TPersistent; const Identifier: string;
  var Handled: boolean);
var
  List: TStrings;
  HasData: Boolean;
  i: Integer;
begin
  if not (Instance is TStrings) then exit;
  List:=TStrings(Instance);
  if Assigned(Writer.Ancestor) then
    // Only serialize if string list is different from ancestor
    if Writer.Ancestor.InheritsFrom(TStrings) then
      HasData := not Equals(TStrings(Writer.Ancestor))
    else
      HasData := True
  else
    HasData := List.Count > 0;
  if not HasData then exit;
  Writer.WriteStatement('with '+Identifier+' do begin');
  Writer.Indent;
  Writer.WriteStatement('Clear;');
  for i:=0 to List.Count-1 do
    Writer.WriteStatement('Add('+Writer.GetStringLiteral(List[i])+');');
  Writer.Unindent;
  Writer.WriteStatement('end;');
  Handled:=true;
end;

procedure TTestCompReaderWriterPas.OnWriterGetParentProperty(
  Sender: TCompWriterPas; Component: TComponent; var PropName: string);
begin
  if Component is TSimpleControl then
    PropName:='Parent';
end;

procedure TTestCompReaderWriterPas.SetUp;
begin
  inherited SetUp;
  FStream:=TMemoryStream.Create;
  FWriter:=TCompWriterPas.Create(FStream);
  FWriter.OnFindAncestor:=@OnWriterFindAncestor;
  FWriter.OnGetParentProperty:=@OnWriterGetParentProperty;
end;

procedure TTestCompReaderWriterPas.TearDown;
begin
  FAncestors.Clear;
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
  Component: TComponent; Ancestor: TComponent; const Expected: array of string;
  NeedAccessClass: boolean);
var
  Actual, ExpS, s: String;
begin
  Actual:=WriteDescendant(Component,Ancestor);
  ExpS:=CSPDefaultSignatureBegin+LineEnding;
  ExpS:=ExpS+Writer.GetVersionStatement+LineEnding;
  if cwpoNoSelf in Writer.Options then begin
    ExpS:=ExpS+'with '+Component.Name+' do begin'+LineEnding;
    ExpS:=ExpS+'  Name:='''+Component.Name+''';'+LineEnding;
  end else
    ExpS:=ExpS+'Name:='''+Component.Name+''';'+LineEnding;
  for s in Expected do
    ExpS:=ExpS+s+LineEnding;
  if cwpoNoSelf in Writer.Options then
    ExpS:=ExpS+'end;'+LineEnding;
  ExpS:=ExpS+CSPDefaultSignatureEnd+LineEnding;
  CheckDiff(Msg,ExpS,Actual);
  AssertEquals(Msg+' NeedAccessClass',NeedAccessClass,Writer.NeedAccessClass);
end;

constructor TTestCompReaderWriterPas.Create;
begin
  inherited Create;
  FAncestors:=TPointerToPointerTree.Create;
end;

destructor TTestCompReaderWriterPas.Destroy;
begin
  FreeAndNil(FAncestors);
  inherited Destroy;
end;

procedure TTestCompReaderWriterPas.AddAncestor(Component, Ancestor: TComponent);
begin
  FAncestors[Component]:=Ancestor;
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
      SetOfBool:=[];
      DefSetOfBool:=[true];
      MyInt:=TMyInt(0);
      DefMyInt:=MyInt+1;
      SetOfMyInt:=[];
      DefSetOfMyInt:=[2];
      MyChar:=TMyChar(0);
      DefMyChar:=succ(MyChar);
      SetOfMyChar:=[];
      DefSetOfMyChar:=[#4];
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
    'SetOfBool:=[];',
    'MyInt:=0;',
    'SetOfMyInt:=[];',
    'MyChar:=#0;',
    'SetOfMyChar:=[];',
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
      SetOfBool:=[];
      DefSetOfBool:=[true];
      MyInt:=low(TMyInt);
      DefMyInt:=MyInt+1;
      SetOfMyInt:=[];
      DefSetOfMyInt:=[2];
      MyChar:=low(TMyChar);
      DefMyChar:=succ(MyChar);
      SetOfMyChar:=[];
      DefSetOfMyChar:=[#4];
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
    'SetOfBool:=[];',
    'MyInt:=1;',
    'SetOfMyInt:=[];',
    'MyChar:=#3;',
    'SetOfMyChar:=[];',
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
      SetOfBool:=[low(Boolean)..high(Boolean)];
      DefSetOfBool:=[true];
      MyInt:=high(TMyInt);
      DefMyInt:=pred(MyInt);
      SetOfMyInt:=[low(MyInt)..high(MyInt)];
      DefSetOfMyInt:=[3];
      MyChar:=high(TMyChar);
      DefMyChar:=pred(MyChar);
      SetOfMyChar:=[low(MyChar)..high(MyChar)];
      DefSetOfMyChar:=[#5];
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
    'SetOfBool:=[False..True];',
    'MyInt:=7;',
    'SetOfMyInt:=[1..7];',
    'MyChar:=#10;',
    'SetOfMyChar:=[#3..#10];',
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
    'V13:=Currency(17.0001);',
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

procedure TTestCompReaderWriterPas.TestInterface;
var
  aRoot: TSimpleControl;
  Button1, Label1: TSimpleControlWithInterface;
begin
  aRoot:=TSimpleControl.Create(nil);
  try
    with aRoot do begin
      Name:=CreateRootName(aRoot);
      Button1:=TSimpleControlWithInterface.Create(aRoot);
      with Button1 do begin
        Name:='Button1';
        Parent:=aRoot;
      end;
      Label1:=TSimpleControlWithInterface.Create(aRoot);
      with Label1 do begin
        Name:='Label1';
        Parent:=aRoot;
        Intf:=Button1;
      end;
      Button1.Intf:=Label1;
    end;
    TestWriteDescendant('TestInterface',aRoot,nil,[
    'Button1:=TSimpleControlWithInterface.Create(Self);',
    'Label1:=TSimpleControlWithInterface.Create(Self);',
    'with Button1 do begin',
    '  Name:=''Button1'';',
    '  Intf:=Label1;',
    '  Parent:=Self;',
    'end;',
    'with Label1 do begin',
    '  Name:=''Label1'';',
    '  Intf:=Button1;',
    '  Parent:=Self;',
    'end;',
    '']);
  finally
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

procedure TTestCompReaderWriterPas.TestAncestorChildPos;

  procedure InitAncestor(C: TSimpleControl);
  var
    Button1, Panel2, Button21, Button22: TSimpleControl;
  begin
    C.Tag:=1;
    Button1:=TSimpleControl.Create(C);
    with Button1 do begin
      Name:='Button1';
      Tag:=11;
      Parent:=C;
    end;
    Panel2:=TSimpleControl.Create(C);
    with Panel2 do begin
      Name:='Panel2';
      Tag:=12;
      Parent:=C;
      Button21:=TSimpleControl.Create(C);
      with Button21 do begin
        Name:='Button21';
        Tag:=121;
        Parent:=Panel2;
      end;
      Button22:=TSimpleControl.Create(C);
      with Button22 do begin
        Name:='Button22';
        Tag:=122;
        Parent:=Panel2;
      end;
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

    // switch Button21 and Button22
    aRoot.Controls[1].FChildren.Move(0,1);

    // switch Button1 and Panel2
    aRoot.FChildren.Move(0,1);

    TestWriteDescendant('TestAncestorChildPos',aRoot,Ancestor,[
    'with Panel2 do begin',
    '  with Button22 do begin',
    '  end;',
    '  TPasStreamAccess(TComponent(Panel2)).SetChildOrder(Button22,0);',
    '  with Button21 do begin',
    '  end;',
    '  TPasStreamAccess(TComponent(Panel2)).SetChildOrder(Button21,1);',
    'end;',
    'SetChildOrder(Panel2,0);',
    'with Button1 do begin',
    'end;',
    'SetChildOrder(Button1,1);',
    ''],true);
  finally
    aRoot.Free;
    Ancestor.Free;
  end;
end;

procedure TTestCompReaderWriterPas.TestWithLookupRootName;

  procedure InitAncestor(C: TSimpleControl);
  var
    Button1, Panel2, Button21, Button22: TSimpleControl;
  begin
    C.Tag:=1;
    Button1:=TSimpleControl.Create(C);
    with Button1 do begin
      Name:='Button1';
      Tag:=11;
      Parent:=C;
    end;
    Panel2:=TSimpleControl.Create(C);
    with Panel2 do begin
      Name:='Panel2';
      Tag:=12;
      Parent:=C;
      Button21:=TSimpleControl.Create(C);
      with Button21 do begin
        Name:='Button21';
        Tag:=121;
        Parent:=Panel2;
      end;
      Button22:=TSimpleControl.Create(C);
      with Button22 do begin
        Name:='Button22';
        Tag:=122;
        Parent:=Panel2;
      end;
    end;
  end;

var
  aRoot, Ancestor, Label1: TSimpleControl;
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
    aRoot.Controls[0].Next:=aRoot;
    aRoot.Next:=aRoot.Controls[0];
    Label1:=TSimpleControl.Create(aRoot);
    with Label1 do begin
      Name:='Label1';
      Parent:=aRoot;
    end;

    // switch Button21 and Button22
    aRoot.Controls[1].FChildren.Move(0,1);

    // switch Button1 and Panel2
    aRoot.FChildren.Move(0,1);

    Writer.Options:=Writer.Options+[cwpoNoSelf];
    TestWriteDescendant('TestWithLookupRootName',aRoot,Ancestor,[
    '  Label1:=TSimpleControl.Create(Descendant);',
    '  Next:=Button1;',
    '  with Panel2 do begin',
    '    with Button22 do begin',
    '    end;',
    '    TPasStreamAccess(TComponent(Panel2)).SetChildOrder(Button22,0);',
    '    with Button21 do begin',
    '    end;',
    '    TPasStreamAccess(TComponent(Panel2)).SetChildOrder(Button21,1);',
    '  end;',
    '  TPasStreamAccess(TComponent(Descendant)).SetChildOrder(Panel2,0);',
    '  with Button1 do begin',
    '    Next:=Descendant;',
    '  end;',
    '  TPasStreamAccess(TComponent(Descendant)).SetChildOrder(Button1,1);',
    '  with Label1 do begin',
    '    Name:=''Label1'';',
    '    Parent:=Descendant;',
    '  end;',
    ''],true);
  finally
    aRoot.Free;
    Ancestor.Free;
  end;
end;

procedure TTestCompReaderWriterPas.TestChildComponents;
var
  aRoot, Button1, Panel1: TSimpleControl;
begin
  aRoot:=TSimpleControl.Create(nil);
  try
    with aRoot do begin
      Name:=CreateRootName(aRoot);
      Tag:=1;
    end;
    Panel1:=TSimpleControl.Create(aRoot);
    with Panel1 do begin
      Name:='Panel1';
      Tag:=2;
      Parent:=aRoot;
      Button1:=TSimpleControl.Create(aRoot);
      with Button1 do begin
        Name:='Button1';
        Tag:=3;
        Parent:=Panel1;
      end;
    end;

    TestWriteDescendant('TestChildComponent',aRoot,nil,[
    'Panel1:=TSimpleControl.Create(Self);',
    'Button1:=TSimpleControl.Create(Self);',
    'Tag:=1;',
    'with Panel1 do begin',
    '  Name:=''Panel1'';',
    '  Tag:=2;',
    '  Parent:=Self;',
    '  with Button1 do begin',
    '    Name:=''Button1'';',
    '    Tag:=3;',
    '    Parent:=Panel1;',
    '  end;',
    'end;',
    '']);
  finally
    aRoot.Free;
  end;
end;

procedure TTestCompReaderWriterPas.TestChildComponentsNoWith;
var
  aRoot, Button1, Panel1: TSimpleControl;
begin
  aRoot:=TSimpleControl.Create(nil);
  try
    with aRoot do begin
      Name:='Root';
      Tag:=1;
    end;
    Panel1:=TSimpleControl.Create(aRoot);
    with Panel1 do begin
      Name:='Panel1';
      Tag:=2;
      Parent:=aRoot;
      Button1:=TSimpleControl.Create(aRoot);
      with Button1 do begin
        Name:='Button1';
        Tag:=3;
        Parent:=Panel1;
      end;
    end;

    Writer.Options:=Writer.Options+[cwpoNoWithBlocks];
    TestWriteDescendant('TestChildComponent',aRoot,nil,[
    'Panel1:=TSimpleControl.Create(Self);',
    'Button1:=TSimpleControl.Create(Self);',
    'Tag:=1;',
    '  Panel1.Name:=''Panel1'';',
    '  Panel1.Tag:=2;',
    '  Panel1.Parent:=Self;',
    '    Button1.Name:=''Button1'';',
    '    Button1.Tag:=3;',
    '    Button1.Parent:=Panel1;',
    '']);
  finally
    aRoot.Free;
  end;
end;

procedure TTestCompReaderWriterPas.TestForeignReference;
var
  aRoot, Button1, aRoot2, Button2: TSimpleControl;
begin
  aRoot:=TSimpleControl.Create(nil);
  aRoot2:=TSimpleControl.Create(nil);
  try
    with aRoot do begin
      Name:=CreateRootName(aRoot);
      Tag:=11;
    end;
    Button1:=TSimpleControl.Create(aRoot);
    with Button1 do begin
      Name:='Button1';
      Tag:=12;
      Parent:=aRoot;
    end;

    with aRoot2 do begin
      Name:='OtherRoot';
      Tag:=21;
    end;
    Button2:=TSimpleControl.Create(aRoot2);
    with Button2 do begin
      Name:='Button2';
      Tag:=22;
      Parent:=aRoot2;
    end;

    aRoot.Next:=aRoot2;
    Button1.Next:=Button2;

    TestWriteDescendant('TestForeignReference',aRoot,nil,[
    'Button1:=TSimpleControl.Create(Self);',
    'Tag:=11;',
    'Next:=OtherRoot;',
    'with Button1 do begin',
    '  Name:=''Button1'';',
    '  Tag:=12;',
    '  Next:=OtherRoot.Button2;',
    '  Parent:=Self;',
    'end;',
    '']);
  finally
    aRoot.Free;
    aRoot2.Free;
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

procedure TTestCompReaderWriterPas.TestInline;

  procedure InitFrame(Frame: TSimpleControl);
  var
    FrameButton1: TSimpleControl;
  begin
    with Frame do begin
      Tag:=12;
      FrameButton1:=TSimpleControl.Create(Frame);
      with FrameButton1 do begin
        Name:='FrameButton1';
        Tag:=123;
        Parent:=Frame;
      end;
    end;
  end;

var
  aRoot, Button1, Frame1, AncestorFrame: TSimpleControl;
begin
  // e.g. a form with a frame
  // the form has no ancestor
  // the frame has an ancestor
  aRoot:=TSimpleControl.Create(nil);
  AncestorFrame:=TSimpleControl.Create(nil);
  try
    AncestorFrame.Name:='AncestorFrame';
    InitFrame(AncestorFrame);

    with aRoot do begin
      Name:=CreateRootName(aRoot);
      Tag:=1;
    end;
    Button1:=TSimpleControl.Create(aRoot);
    with Button1 do begin
      Name:='Button1';
      Parent:=aRoot;
    end;
    Frame1:=TSimpleControl.Create(aRoot);
    TAccessComp(TComponent(Frame1)).SetInline(true);
    InitFrame(Frame1);
    with Frame1 do begin
      Name:='Frame1';
      Parent:=aRoot;
    end;

    AddAncestor(Frame1,AncestorFrame);
    TestWriteDescendant('TestInline',aRoot,nil,[
    'Button1:=TSimpleControl.Create(Self);',
    'Frame1:=TSimpleControl.Create(Self);',
    CSPDefaultAccessClass+'(TComponent(Frame1)).SetInline(True);',
    'Tag:=1;',
    'with Button1 do begin',
    '  Name:=''Button1'';',
    '  Parent:=Self;',
    'end;',
    'with Frame1 do begin',
    '  Name:=''Frame1'';',
    '  Parent:=Self;',
    '  with FrameButton1 do begin',
    '  end;',
    'end;',
    ''],true);
  finally
    AncestorFrame.Free;
    aRoot.Free;
  end;
end;

procedure TTestCompReaderWriterPas.TestAncestorWithInline;

  procedure InitFrame(Frame: TSimpleControl);
  var
    FrameButton1, FrameButton2: TSimpleControl;
  begin
    with Frame do begin
      Tag:=1;
      FrameButton1:=TSimpleControl.Create(Frame);
      with FrameButton1 do begin
        Name:='FrameButton1';
        Tag:=11;
        Parent:=Frame;
      end;
      FrameButton2:=TSimpleControl.Create(Frame);
      with FrameButton2 do begin
        Name:='FrameButton2';
        Tag:=12;
        Parent:=Frame;
      end;
    end;
  end;

  procedure InitForm(Form: TSimpleControl; out Frame1: TSimpleControl);
  var
    Button1: TSimpleControl;
  begin
    with Form do begin
      // add a button
      Button1:=TSimpleControl.Create(Form);
      with Button1 do begin
        Name:='Button1';
        Tag:=21;
        Parent:=Form;
      end;
      // add a frame
      Frame1:=TSimpleControl.Create(Form);
      TAccessComp(TComponent(Frame1)).SetInline(true);
      InitFrame(Frame1);
      with Frame1 do begin
        Name:='Frame1';
        Tag:=22;
        Parent:=Form;
      end;
    end;
  end;

var
  Frame1, AncestorFrame, AncestorForm, Form,
    Frame2, Label1: TSimpleControl;
begin
  // e.g. a form inherited from with a frame
  AncestorFrame:=nil;
  AncestorForm:=nil;
  Form:=nil;
  try
    AncestorFrame:=TSimpleControl.Create(nil);
    AncestorFrame.Name:='AncestorFrame';
    InitFrame(AncestorFrame);

    AncestorForm:=TSimpleControl.Create(nil);
    AncestorForm.Name:='AncestorForm';
    InitForm(AncestorForm,Frame1);
    AddAncestor(Frame1,AncestorFrame);

    Form:=TSimpleControl.Create(nil);
    Form.Name:='Form';
    InitForm(Form,Frame2);
    Frame2.Tag:=32;
    Frame2.Controls[0].Tag:=421;
    // change Z order of buttons in frame
    Form.FChildren.Move(0,1);
    // change Z order of frame in Form
    Frame2.FChildren.Move(0,1);
    // add a label
    Label1:=TSimpleControl.Create(Form);
    with Label1 do begin
      Name:='Label1';
      Tag:=33;
      Parent:=Form;
    end;

    TestWriteDescendant('TestAncestorWithInline',Form,AncestorForm,[
    'Label1:=TSimpleControl.Create(Self);',
    'with Frame1 do begin',
    '  Tag:=32;',
    '  with FrameButton2 do begin',
    '  end;',
    '  TPasStreamAccess(TComponent(Frame1)).SetChildOrder(Frame1.FrameButton2,0);',
    '  with FrameButton1 do begin',
    '    Tag:=421;',
    '  end;',
    '  TPasStreamAccess(TComponent(Frame1)).SetChildOrder(Frame1.FrameButton1,1);',
    'end;',
    'SetChildOrder(Frame1,0);',
    'with Button1 do begin',
    'end;',
    'SetChildOrder(Button1,1);',
    'with Label1 do begin',
    '  Name:=''Label1'';',
    '  Tag:=33;',
    '  Parent:=Self;',
    'end;',
    ''],true);
  finally
    Form.Free;
    AncestorForm.Free;
    AncestorFrame.Free;
  end;
end;

procedure TTestCompReaderWriterPas.TestInlineDescendant;

  procedure InitFrame(Frame: TSimpleControl);
  var
    FrameButton1, FrameButton2: TSimpleControl;
  begin
    with Frame do begin
      Tag:=1;
      FrameButton1:=TSimpleControl.Create(Frame);
      with FrameButton1 do begin
        Name:='FrameButton1';
        Tag:=11;
        Parent:=Frame;
      end;
      FrameButton2:=TSimpleControl.Create(Frame);
      with FrameButton2 do begin
        Name:='FrameButton2';
        Tag:=12;
        Parent:=Frame;
      end;
    end;
  end;

  procedure InitForm(Form: TSimpleControl; out Frame1: TSimpleControl);
  var
    Button1: TSimpleControl;
  begin
    with Form do begin
      // add a button
      Button1:=TSimpleControl.Create(Form);
      with Button1 do begin
        Name:='Button1';
        Tag:=21;
        Parent:=Form;
      end;
      // add a frame
      Frame1:=TSimpleControl.Create(Form);
      TAccessComp(TComponent(Frame1)).SetInline(true);
      InitFrame(Frame1);
      with Frame1 do begin
        Name:='Frame1';
        Tag:=22;
        Parent:=Form;
      end;
    end;
  end;

var
  AncestorFrame, DescendantFrame, Form, Frame: TSimpleControl;
begin
  // e.g. a form inherited from with a frame
  AncestorFrame:=nil;
  DescendantFrame:=nil;
  Form:=nil;
  try
    AncestorFrame:=TSimpleControl.Create(nil);
    AncestorFrame.Name:='AncestorFrame';
    InitFrame(AncestorFrame);

    DescendantFrame:=TSimpleControl.Create(nil);
    DescendantFrame.Name:='DescendantFrame';
    InitFrame(DescendantFrame);
    AddAncestor(DescendantFrame,AncestorFrame);

    Form:=TSimpleControl.Create(nil);
    Form.Name:='Form';
    InitForm(Form,Frame);
    AddAncestor(Frame,DescendantFrame);

    TestWriteDescendant('TestInlineDescendant',Form,nil,[
    'Button1:=TSimpleControl.Create(Self);',
    'Frame1:=TSimpleControl.Create(Self);',
    'TPasStreamAccess(TComponent(Frame1)).SetInline(True);',
    'with Button1 do begin',
    '  Name:=''Button1'';',
    '  Tag:=21;',
    '  Parent:=Self;',
    'end;',
    'with Frame1 do begin',
    '  Name:=''Frame1'';',
    '  Tag:=22;',
    '  Parent:=Self;',
    '  with FrameButton1 do begin',
    '  end;',
    '  with FrameButton2 do begin',
    '  end;',
    'end;',
    ''],true);
  finally
    Form.Free;
    DescendantFrame.Free;
    AncestorFrame.Free;
  end;
end;

procedure TTestCompReaderWriterPas.TestDesignInfo;
var
  AComponent: TComponent;
begin
  AComponent:=TComponent.Create(nil);
  try
    with AComponent do begin
      Name:=CreateRootName(AComponent);
      DesignInfo:=12345678;
    end;
    TestWriteDescendant('TestDesignInfo',AComponent,nil,[
    'DesignInfo:=12345678;',
    '']);
  finally
    aComponent.Free;
  end;
end;

procedure TTestCompReaderWriterPas.TestDefineProperties_ListOfStrings;
var
  ARoot: TSimpleControlWithStrings;
  Expected: String;
  Lines2: TStringList;
begin
  ARoot:=TSimpleControlWithStrings.Create(nil);
  Lines2:=nil;
  try
    with ARoot do begin
      Name:=CreateRootName(ARoot);
      Lines:=TStringList.Create;
      Lines.Text:='First'+LineEnding+'Second';
    end;
    Expected:=#7'Strings'#1#6#5'First'#6#6'Second'#0#0;
    TestWriteDescendant('TestDefineProperites_ListOfStrings',ARoot,nil,[
    CSPDefaultExecCustomProc+'(Lines,[#7''Strings''#1#6#5''First''#6#6''Second''#0#0]);',
    '']);

    Lines2:=TStringList.Create;
    ExecCustomCSP(Lines2,[Expected]);
    AssertEquals('read TStrings.Text',ARoot.Lines.Text,Lines2.Text);

    AssertEquals('NeededUnits.Count',1,Writer.NeededUnits.Count);
    AssertEquals('NeededUnits[0]',Writer.ExecCustomProcUnit,Writer.NeededUnits[0]);

  finally
    Lines2.Free;
    FreeAndNil(ARoot.FLines);
    ARoot.Free;
  end;
end;

procedure TTestCompReaderWriterPas.Test_TStrings;
var
  ARoot: TSimpleControlWithStrings;
begin
  ARoot:=TSimpleControlWithStrings.Create(nil);
  try
    with ARoot do begin
      Name:=CreateRootName(ARoot);
      Lines:=TStringList.Create;
      Lines.Text:='First'+LineEnding+'Second';
    end;
    Writer.OnDefineProperties:=@OnDefinePropertiesTStrings;

    TestWriteDescendant('Test_TStrings',ARoot,nil,[
    'with Lines do begin',
    '  Clear;',
    '  Add(''First'');',
    '  Add(''Second'');',
    'end;',
    '']);
  finally
    FreeAndNil(ARoot.FLines);
    ARoot.Free;
  end;
end;

procedure TTestCompReaderWriterPas.TestFindComponentInit;
var
  Code: TCodeBuffer;
  Init, IndentedInit, Src: String;
begin
  exit;

  Code:=CodeToolBoss.CreateFile('form1.pas');
  Init:='Name:=''Form1'';'+LineEnding;
  IndentText(CSPDefaultSignatureBegin+LineEnding
    +Init
    +CSPDefaultSignatureEnd+LineEnding,2,8,IndentedInit);
  Src:=LinesToStr(['unit Unit1;'
    ,'{$mode objfpc}{$H+}'
    ,'interface'
    ,'uses Classes;'
    ,'type'
    ,'  TForm1 = class(TComponent)'
    ,'  public'
    ,'    constructor Create(TheOwner: TComponent); override;'
    ,'  end;'
    ,'implementation'
    ,'type'
    ,'  '+CSPDefaultAccessClass+' = class(TComponent);'
    ,'constructor TForm.Create(TheOwner: TComponent);'
    ,'begin'+LineEnding
    ,'  inherited;'])
    +IndentedInit
    +LinesToStr(['end;'
    ,'end.']);
  Code.Source:=Src;
  if not CodeToolBoss.UpdateComponentInit(Code,'TForm1',CSPDefaultAccessClass,
    CSPDefaultSignatureBegin,CSPDefaultSignatureEnd,Init)
  then begin
    Fail('CodeToolBoss.UpdateComponentInit failed');
  end;
  CheckDiff('TestFindComponentInit',Src,Code.Source);
end;

initialization
  RegisterTest(TTestCompReaderWriterPas);
end.

