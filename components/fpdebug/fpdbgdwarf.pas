{
 ---------------------------------------------------------------------------
 fpdbgdwarf.pas  -  Native Freepascal debugger - Dwarf symbol processing
 ---------------------------------------------------------------------------

 This unit contains helper classes for handling and evaluating of debuggee data
 described by DWARF debug symbols

 ---------------------------------------------------------------------------

 @created(Mon Aug 1st WET 2006)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.nl>)
 @author(Martin Friebe)

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit FpDbgDwarf;

{$mode objfpc}{$H+}
{off $INLINE OFF}

(* Notes:

   * FpDbgDwarfValues and Context
     The Values do not add a reference to the Context. Yet they require the Context.
     It is the users responsibility to keep the context, as long as any value exists.

*)

interface

uses
  Classes, SysUtils, types, math, FpDbgInfo, FpDbgDwarfDataClasses, FpdMemoryTools, FpErrorMessages,
  FpDbgUtil, FpDbgDwarfConst, DbgIntfBaseTypes, LazUTF8, LazLoggerBase, LazClasses;

type
  TFpDwarfInfo = FpDbgDwarfDataClasses.TFpDwarfInfo;

  { TFpDwarfDefaultSymbolClassMap }

  TFpDwarfDefaultSymbolClassMap = class(TFpSymbolDwarfClassMap)
  private
    class var ExistingClassMap: TFpSymbolDwarfClassMap;
  protected
    class function GetExistingClassMap: PFpDwarfSymbolClassMap; override;
  public
    class function ClassCanHandleCompUnit(ACU: TDwarfCompilationUnit): Boolean; override;
  public
    //function CanHandleCompUnit(ACU: TDwarfCompilationUnit): Boolean; override;
    function GetDwarfSymbolClass(ATag: Cardinal): TDbgDwarfSymbolBaseClass; override;
    function CreateContext(AThreadId, AStackFrame: Integer; AnAddress:
      TDbgPtr; ASymbol: TFpSymbol; ADwarf: TFpDwarfInfo): TFpDbgInfoContext; override;
    function CreateProcSymbol(ACompilationUnit: TDwarfCompilationUnit;
      AInfo: PDwarfAddressInfo; AAddress: TDbgPtr): TDbgDwarfSymbolBase; override;
  end;

  { TFpDwarfInfoAddressContext }

  TFpDwarfInfoAddressContext = class(TFpDbgInfoContext)
  private
    FSymbol: TFpSymbol;
    FAddress: TDBGPtr;
    FThreadId, FStackFrame: Integer;
    FDwarf: TFpDwarfInfo;
    FlastResult: TFpValue;
  protected
    function GetSymbolAtAddress: TFpSymbol; override;
    function GetProcedureAtAddress: TFpValue; override;
    function GetAddress: TDbgPtr; override;
    function GetThreadId: Integer; override;
    function GetStackFrame: Integer; override;
    function GetSizeOfAddress: Integer; override;
    function GetMemManager: TFpDbgMemManager; override;

    property Symbol: TFpSymbol read FSymbol;
    property Dwarf: TFpDwarfInfo read FDwarf;
    property Address: TDBGPtr read FAddress write FAddress;
    property ThreadId: Integer read FThreadId write FThreadId;
    property StackFrame: Integer read FStackFrame write FStackFrame;

    procedure ApplyContext(AVal: TFpValue); inline;
    function SymbolToValue(ASym: TFpSymbol): TFpValue; inline;
    procedure AddRefToVal(AVal: TFpValue); inline;
    function GetSelfParameter: TFpValue; virtual;

    function FindExportedSymbolInUnits(const AName: String; PNameUpper, PNameLower: PChar;
      SkipCompUnit: TDwarfCompilationUnit; out ADbgValue: TFpValue): Boolean; inline;
    function FindSymbolInStructure(const AName: String; PNameUpper, PNameLower: PChar;
      InfoEntry: TDwarfInformationEntry; out ADbgValue: TFpValue): Boolean; inline;
    // FindLocalSymbol: for the subroutine itself
    function FindLocalSymbol(const AName: String; PNameUpper, PNameLower: PChar;
      InfoEntry: TDwarfInformationEntry; out ADbgValue: TFpValue): Boolean; virtual;
  public
    constructor Create(AThreadId, AStackFrame: Integer; AnAddress: TDbgPtr; ASymbol: TFpSymbol; ADwarf: TFpDwarfInfo);
    destructor Destroy; override;
    function FindSymbol(const AName: String): TFpValue; override;
  end;

  TFpSymbolDwarf = class;
  TFpSymbolDwarfType = class;
  TFpSymbolDwarfData = class;
  TFpSymbolDwarfDataClass = class of TFpSymbolDwarfData;
  TFpSymbolDwarfTypeClass = class of TFpSymbolDwarfType;

  PFpSymbolDwarfData = ^TFpSymbolDwarfData;

{%region Value objects }

  { TFpValueDwarfBase }

  TFpValueDwarfBase = class(TFpValue)
  private
    FContext: TFpDbgInfoContext;
  public
    property Context: TFpDbgInfoContext read FContext write FContext;
  end;

  { TFpValueDwarfTypeDefinition }

  TFpValueDwarfTypeDefinition = class(TFpValueDwarfBase)
  private
    FSymbol: TFpSymbol; // stType
  protected
    function GetKind: TDbgSymbolKind; override;
    function GetDbgSymbol: TFpSymbol; override;
  public
    constructor Create(ASymbol: TFpSymbol); // Only for stType
    destructor Destroy; override;
    function GetTypeCastedValue(ADataVal: TFpValue): TFpValue; override;
  end;

  { TFpValueDwarf }

  TFpValueDwarf = class(TFpValueDwarfBase)
  private
    FOwner: TFpSymbolDwarfType;        // the creator, usually the type
    FDataSymbol: TFpSymbolDwarfData;
    FTypeCastTargetType: TFpSymbolDwarfType;
    FTypeCastSourceValue: TFpValue;

    FCachedAddress, FCachedDataAddress: TFpDbgMemLocation;
    FStructureValue: TFpValueDwarf;
    FLastMember: TFpValueDwarf;
    procedure SetStructureValue(AValue: TFpValueDwarf);
  protected
    FLastError: TFpError;
    procedure DoReferenceAdded; override;
    procedure DoReferenceReleased; override;
    procedure CircleBackRefActiveChanged(NewActive: Boolean); override;
    procedure SetLastMember(ALastMember: TFpValueDwarf);
    function GetLastError: TFpError; override;
    function AddressSize: Byte; inline;

    // Address of the symbol (not followed any type deref, or location)
    function GetAddress: TFpDbgMemLocation; override;
    function OrdOrAddress: TFpDbgMemLocation;
    // Address of the data (followed type deref, location, ...)
    function OrdOrDataAddr: TFpDbgMemLocation;
    function GetDataAddress: TFpDbgMemLocation; override;
    function GetDwarfDataAddress(out AnAddress: TFpDbgMemLocation; ATargetType: TFpSymbolDwarfType = nil): Boolean;
    function GetStructureDwarfDataAddress(out AnAddress: TFpDbgMemLocation;
                                          ATargetType: TFpSymbolDwarfType = nil): Boolean;

    procedure Reset; virtual; // keeps lastmember and structureninfo
    function GetFieldFlags: TFpValueFieldFlags; override;
    function HasTypeCastInfo: Boolean;
    function IsValidTypeCast: Boolean; virtual;
    function GetKind: TDbgSymbolKind; override;
    function GetMemberCount: Integer; override;
    function GetMemberByName(AIndex: String): TFpValue; override;
    function GetMember(AIndex: Int64): TFpValue; override;
    function GetDbgSymbol: TFpSymbol; override;
    function GetTypeInfo: TFpSymbol; override;
    function GetContextTypeInfo: TFpSymbol; override;

    property TypeCastTargetType: TFpSymbolDwarfType read FTypeCastTargetType;
    property TypeCastSourceValue: TFpValue read FTypeCastSourceValue;
    property Owner: TFpSymbolDwarfType read FOwner;
  public
    constructor Create(AOwner: TFpSymbolDwarfType);
    destructor Destroy; override;
    function MemManager: TFpDbgMemManager; inline;
    procedure SetValueSymbol(AValueSymbol: TFpSymbolDwarfData);
    function  SetTypeCastInfo(AStructure: TFpSymbolDwarfType;
                              ASource: TFpValue): Boolean; // Used for Typecast
    // StructureValue: Any Value returned via GetMember points to its structure
    property StructureValue: TFpValueDwarf read FStructureValue write SetStructureValue;
    property ValueSymbol: TFpSymbolDwarfData read FDataSymbol;
  end;

  TFpValueDwarfUnknown = class(TFpValueDwarf)
  end;

  { TFpValueDwarfSized }

  TFpValueDwarfSized = class(TFpValueDwarf)
  private
    FSize: Integer;
  protected
    function CanUseTypeCastAddress: Boolean;
    function GetFieldFlags: TFpValueFieldFlags; override;
    function GetSize: Integer; override;
  public
    constructor Create(AOwner: TFpSymbolDwarfType; ASize: Integer);
  end;

  { TFpValueDwarfNumeric }

  TFpValueDwarfNumeric = class(TFpValueDwarfSized)
  protected
    FEvaluated: set of (doneUInt, doneInt, doneAddr, doneFloat);
  protected
    procedure Reset; override;
    function GetFieldFlags: TFpValueFieldFlags; override; // svfOrdinal
    function IsValidTypeCast: Boolean; override;
  public
    constructor Create(AOwner: TFpSymbolDwarfType; ASize: Integer);
  end;

  { TFpValueDwarfInteger }

  TFpValueDwarfInteger = class(TFpValueDwarfNumeric)
  private
    FIntValue: Int64;
  protected
    function GetFieldFlags: TFpValueFieldFlags; override;
    function GetAsCardinal: QWord; override;
    function GetAsInteger: Int64; override;
  end;

  { TFpValueDwarfCardinal }

  TFpValueDwarfCardinal = class(TFpValueDwarfNumeric)
  private
    FValue: QWord;
  protected
    function GetAsCardinal: QWord; override;
    function GetFieldFlags: TFpValueFieldFlags; override;
  end;

  { TFpValueDwarfFloat }

  TFpValueDwarfFloat = class(TFpValueDwarfNumeric) // TDbgDwarfSymbolValue
  // TODO: typecasts to int should convert
  private
    FValue: Extended;
  protected
    function GetFieldFlags: TFpValueFieldFlags; override;
    function GetAsFloat: Extended; override;
  end;

  { TFpValueDwarfBoolean }

  TFpValueDwarfBoolean = class(TFpValueDwarfCardinal)
  protected
    function GetFieldFlags: TFpValueFieldFlags; override;
    function GetAsBool: Boolean; override;
  end;

  { TFpValueDwarfChar }

  TFpValueDwarfChar = class(TFpValueDwarfCardinal)
  protected
    // returns single char(byte) / widechar
    function GetFieldFlags: TFpValueFieldFlags; override;
    function GetAsString: AnsiString; override;
    function GetAsWideString: WideString; override;
  end;

  { TFpValueDwarfPointer }

  TFpValueDwarfPointer = class(TFpValueDwarfNumeric)
  private
    FLastAddrMember: TFpValue;
    FPointetToAddr: TFpDbgMemLocation;
    function GetDerefAddress: TFpDbgMemLocation;
  protected
    function GetAsCardinal: QWord; override;
    function GetFieldFlags: TFpValueFieldFlags; override;
    function GetDataAddress: TFpDbgMemLocation; override;
    function GetAsString: AnsiString; override;
    function GetAsWideString: WideString; override;
    function GetMember(AIndex: Int64): TFpValue; override;
  public
    destructor Destroy; override;
  end;

  { TFpValueDwarfEnum }

  TFpValueDwarfEnum = class(TFpValueDwarfNumeric)
  private
    FValue: QWord;
    FMemberIndex: Integer;
    FMemberValueDone: Boolean;
    procedure InitMemberIndex;
  protected
    procedure Reset; override;
    //function IsValidTypeCast: Boolean; override;
    function GetFieldFlags: TFpValueFieldFlags; override;
    function GetAsCardinal: QWord; override;
    function GetAsString: AnsiString; override;
    // Has exactly 0 (if the ordinal value is out of range) or 1 member (the current value's enum)
    function GetMemberCount: Integer; override;
    function GetMember({%H-}AIndex: Int64): TFpValue; override;
  end;

  { TFpValueDwarfEnumMember }

  TFpValueDwarfEnumMember = class(TFpValueDwarf)
  private
    FOwnerVal: TFpSymbolDwarfData;
  protected
    function GetFieldFlags: TFpValueFieldFlags; override;
    function GetAsCardinal: QWord; override;
    function GetAsString: AnsiString; override;
    function IsValidTypeCast: Boolean; override;
  public
    constructor Create(AOwner: TFpSymbolDwarfData);
  end;

  { TFpValueDwarfConstNumber }

  TFpValueDwarfConstNumber = class(TFpValueConstNumber)
  protected
    procedure Update(AValue: QWord; ASigned: Boolean);
  end;

  { TFpValueDwarfSet }

  TFpValueDwarfSet = class(TFpValueDwarfSized)
  private
    FMem: array of Byte;
    FMemberCount: Integer;
    FMemberMap: array of Integer;
    FNumValue: TFpValueDwarfConstNumber;
    FTypedNumValue: TFpValue;
    procedure InitMap;
  protected
    procedure Reset; override;
    function GetFieldFlags: TFpValueFieldFlags; override;
    function GetMemberCount: Integer; override;
    function GetMember(AIndex: Int64): TFpValue; override;
    function GetAsCardinal: QWord; override; // only up to qmord
    function IsValidTypeCast: Boolean; override;
  public
    destructor Destroy; override;
  end;

  { TFpValueDwarfStruct }

  TFpValueDwarfStruct = class(TFpValueDwarf)
  private
    FDataAddressDone: Boolean;
  protected
    procedure Reset; override;
    function GetFieldFlags: TFpValueFieldFlags; override;
    function GetAsCardinal: QWord; override;
    function GetDataSize: Integer; override;
    function GetSize: Integer; override;
  end;

  { TFpValueDwarfStructTypeCast }

  TFpValueDwarfStructTypeCast = class(TFpValueDwarf)
  private
    FMembers: TFpDbgCircularRefCntObjList;
    FDataAddressDone: Boolean;
  protected
    procedure Reset; override;
    function GetFieldFlags: TFpValueFieldFlags; override;
    function GetKind: TDbgSymbolKind; override;
    function GetAsCardinal: QWord; override;
    function GetSize: Integer; override;
    function GetDataSize: Integer; override;
    function IsValidTypeCast: Boolean; override;
  public
    destructor Destroy; override;
    function GetMemberByName(AIndex: String): TFpValue; override;
    function GetMember(AIndex: Int64): TFpValue; override;
    function GetMemberCount: Integer; override;
  end;

  { TFpValueDwarfConstAddress }

  TFpValueDwarfConstAddress = class(TFpValueConstAddress)
  protected
    procedure Update(AnAddress: TFpDbgMemLocation);
  end;

  { TFpValueDwarfArray }

  TFpValueDwarfArray = class(TFpValueDwarf)
  private
    FAddrObj: TFpValueDwarfConstAddress;
  protected
    function GetFieldFlags: TFpValueFieldFlags; override;
    function GetKind: TDbgSymbolKind; override;
    function GetAsCardinal: QWord; override;
    function GetMember(AIndex: Int64): TFpValue; override;
    function GetMemberEx(const AIndex: array of Int64): TFpValue; override;
    function GetMemberCount: Integer; override;
    function GetMemberCountEx(const AIndex: array of Int64): Integer; override;
    function GetIndexType(AIndex: Integer): TFpSymbol; override;
    function GetIndexTypeCount: Integer; override;
    function IsValidTypeCast: Boolean; override;
  public
    destructor Destroy; override;
  end;

  { TFpValueDwarfSubroutine }

  TFpValueDwarfSubroutine = class(TFpValueDwarf)
  protected
    function IsValidTypeCast: Boolean; override;
  end;
{%endregion Value objects }

{%region Symbol objects }

  TInitLocParserData = record
    (* DW_AT_data_member_location: Is always pushed on stack
       DW_AT_data_location: Is avalibale for DW_OP_push_object_address
    *)
    ObjectDataAddress: TFpDbgMemLocation;
    ObjectDataAddrPush: Boolean; // always push ObjectDataAddress on stack: DW_AT_data_member_location
  end;
  PInitLocParserData = ^TInitLocParserData;

  (* TFpDwarfAtEntryDataReadState
     Since Dwarf-3 several DW_AT_* can be const, expression or reference.
  *)
  TFpDwarfAtEntryDataReadState = (rfNotRead, rfNotFound, rfError, rfConst, rfValue, rfExpression);
  PFpDwarfAtEntryDataReadState = ^TFpDwarfAtEntryDataReadState;

  { TFpSymbolDwarf }

  TFpSymbolDwarf = class(TDbgDwarfSymbolBase)
  private
    FNestedTypeInfo: TFpSymbolDwarfType;
    FParentTypeInfo: TFpSymbolDwarf;
    FDwarfReadFlags: set of (didtNameRead, didtTypeRead, didtArtificialRead, didtIsArtifical);
    function GetNestedTypeInfo: TFpSymbolDwarfType;
  protected
    (* There will be a circular reference between parenttype and self
       "self" will only set its reference to parenttype, if self has other references.  *)
    procedure DoReferenceAdded; override;
    procedure DoReferenceReleased; override;
    procedure CircleBackRefActiveChanged(ANewActive: Boolean); override;
    procedure SetParentTypeInfo(AValue: TFpSymbolDwarf); virtual;

    function  DoGetNestedTypeInfo: TFpSymbolDwarfType; virtual;
    function  ReadMemberVisibility(out AMemberVisibility: TDbgSymbolMemberVisibility): Boolean;
    function  IsArtificial: Boolean; // usud by formal param and subprogram
    procedure NameNeeded; override;
    procedure TypeInfoNeeded; override;
    property NestedTypeInfo: TFpSymbolDwarfType read GetNestedTypeInfo;

    // OwnerTypeInfo: reverse of "NestedTypeInfo" (variable that is of this type)
//    property OwnerTypeInfo: TDbgDwarfIdentifier read FOwnerTypeInfo; // write SetOwnerTypeInfo;
    // ParentTypeInfo: funtion for local var / class for member
    property ParentTypeInfo: TFpSymbolDwarf read FParentTypeInfo write SetParentTypeInfo;

    function DataSize: Integer; virtual;
  protected
    function InitLocationParser(const {%H-}ALocationParser: TDwarfLocationExpression;
                                AnInitLocParserData: PInitLocParserData = nil): Boolean; virtual;
    function ComputeDataMemberAddress(const AnInformationEntry: TDwarfInformationEntry;
                              AValueObj: TFpValueDwarf; var AnAddress: TFpDbgMemLocation): Boolean; inline;
    function ConstRefOrExprFromAttrData(const AnAttribData: TDwarfAttribData;
                              AValueObj: TFpValueDwarf; out AValue: TDBGPtr;
                              AReadState: PFpDwarfAtEntryDataReadState = nil;
                              ADataSymbol: PFpSymbolDwarfData = nil): Boolean;
    function  LocationFromAttrData(const AnAttribData: TDwarfAttribData; AValueObj: TFpValueDwarf;
                              var AnAddress: TFpDbgMemLocation; // kept, if tag does not exist
                              AnInitLocParserData: PInitLocParserData = nil;
                              AnAdjustAddress: Boolean = False
                             ): Boolean;
    function  LocationFromTag(ATag: Cardinal; AValueObj: TFpValueDwarf;
                              var AnAddress: TFpDbgMemLocation; // kept, if tag does not exist
                              AnInitLocParserData: PInitLocParserData = nil;
                              ASucessOnMissingTag: Boolean = False
                             ): Boolean; // deprecated
    function  ConstantFromTag(ATag: Cardinal; out AConstData: TByteDynArray;
                              var AnAddress: TFpDbgMemLocation; // kept, if tag does not exist
                              AnInformationEntry: TDwarfInformationEntry = nil;
                              ASucessOnMissingTag: Boolean = False
                             ): Boolean;
    // GetDataAddress: data of a class, or string
    function GetDataAddress(AValueObj: TFpValueDwarf; var AnAddress: TFpDbgMemLocation;
                            ATargetType: TFpSymbolDwarfType = nil): Boolean;
    function GetNextTypeInfoForDataAddress(ATargetType: TFpSymbolDwarfType): TFpSymbolDwarfType; virtual;
    function GetDataAddressNext(AValueObj: TFpValueDwarf; var AnAddress: TFpDbgMemLocation;
      out ADoneWork: Boolean; ATargetType: TFpSymbolDwarfType): Boolean; virtual;
    function HasAddress: Boolean; virtual;

    procedure Init; override;
  public
    class function CreateSubClass(AName: String; AnInformationEntry: TDwarfInformationEntry): TFpSymbolDwarf;
    destructor Destroy; override;
    function StartScope: TDbgPtr; // return 0, if none. 0 includes all anyway
  end;

  { TFpSymbolDwarfData }

  TFpSymbolDwarfData = class(TFpSymbolDwarf) // var, const, member, ...
  protected
    FValueObject: TFpValueDwarf;

    function GetValueAddress({%H-}AValueObj: TFpValueDwarf;{%H-} out AnAddress: TFpDbgMemLocation): Boolean; virtual;
    procedure KindNeeded; override;
    procedure MemberVisibilityNeeded; override;
    function GetNestedSymbol(AIndex: Int64): TFpSymbol; override;
    function GetNestedSymbolByName(AIndex: String): TFpSymbol; override;
    function GetNestedSymbolCount: Integer; override;

    procedure Init; override;
  public
    destructor Destroy; override;
    class function CreateValueSubClass(AName: String; AnInformationEntry: TDwarfInformationEntry): TFpSymbolDwarfData;
  end;

  { TFpSymbolDwarfDataWithLocation }

  TFpSymbolDwarfDataWithLocation = class(TFpSymbolDwarfData)
  private
    procedure FrameBaseNeeded(ASender: TObject); // Sender = TDwarfLocationExpression
  protected
    function GetValueObject: TFpValue; override;
    function InitLocationParser(const ALocationParser: TDwarfLocationExpression;
                                AnInitLocParserData: PInitLocParserData): Boolean; override;
  end;

  { TFpSymbolDwarfType }

  (* Types and allowed tags in dwarf 2

  DW_TAG_enumeration_type, DW_TAG_subroutine_type, DW_TAG_union_type,
  DW_TAG_ptr_to_member_type, DW_TAG_set_type, DW_TAG_subrange_type, DW_TAG_file_type,
  DW_TAG_thrown_type

                          DW_TAG_base_type
  DW_AT_encoding          Y
  DW_AT_bit_offset        Y
  DW_AT_bit_size          Y

                          DW_TAG_base_type
                          |  DW_TAG_typedef
                          |  |   DW_TAG_string_type
                          |  |   |  DW_TAG_array_type
                          |  |   |  |
                          |  |   |  |    DW_TAG_class_type
                          |  |   |  |    |  DW_TAG_structure_type
                          |  |   |  |    |  |
                          |  |   |  |    |  |    DW_TAG_enumeration_type
                          |  |   |  |    |  |    |  DW_TAG_set_type
                          |  |   |  |    |  |    |  |  DW_TAG_enumerator
                          |  |   |  |    |  |    |  |  |  DW_TAG_subrange_type
  DW_AT_name              Y  Y   Y  Y    Y  Y    Y  Y  Y  Y
  DW_AT_sibling           Y  Y   Y  Y    Y  Y    Y  Y  Y  Y
  DECL                       Y   Y  Y    Y  Y    Y  Y  Y  Y
  DW_AT_byte_size         Y      Y  Y    Y  Y    Y  Y     Y
  DW_AT_abstract_origin      Y   Y  Y    Y  Y    Y  Y     Y
  DW_AT_accessibility        Y   Y  Y    Y  Y    Y  Y     Y
  DW_AT_declaration          Y   Y  Y    Y  Y    Y  Y     Y
  DW_AT_start_scope          Y   Y  Y    Y  Y    Y  Y
  DW_AT_visibility           Y   Y  Y    Y  Y    Y  Y     Y
  DW_AT_type                 Y      Y               Y     Y
  DW_AT_segment                  Y                              DW_TAG_string_type
  DW_AT_string_length            Y
  DW_AT_ordering                    Y                           DW_TAG_array_type
  DW_AT_stride_size                 Y
  DW_AT_const_value                                    Y        DW_TAG_enumerator
  DW_AT_count                                             Y     DW_TAG_subrange_type
  DW_AT_lower_bound                                       Y
  DW_AT_upper_bound                                       Y

                           DW_TAG_pointer_type
                           |  DW_TAG_reference_type
                           |  |  DW_TAG_packed_type
                           |  |  |  DW_TAG_const_type
                           |  |  |  |  DW_TAG_volatile_type
  DW_AT_address_class      Y  Y
  DW_AT_sibling            Y  Y  Y  Y Y
  DW_AT_type               Y  Y  Y  Y Y

DECL = DW_AT_decl_column, DW_AT_decl_file, DW_AT_decl_line
  *)

  TFpSymbolDwarfType = class(TFpSymbolDwarf)
  protected
    procedure Init; override;
    procedure MemberVisibilityNeeded; override;
    procedure SizeNeeded; override;
  public
    function GetTypedValueObject({%H-}ATypeCast: Boolean): TFpValueDwarf; virtual; // returns refcount=1 for caller, no cached copy kept
    class function CreateTypeSubClass(AName: String; AnInformationEntry: TDwarfInformationEntry): TFpSymbolDwarfType;
    function TypeCastValue(AValue: TFpValue): TFpValue; override;

    (*TODO: workaround / quickfix // only partly implemented
      When reading several elements of an array (dyn or stat), the typeinfo is always the same instance (type of array entry)
      But once that instance has read data (like bounds / dwarf3 bounds are read from app mem), this is cached.
      So all consecutive entries get the same info...
        array of string
        array of shortstring
        array of {dyn} array
      This works similar to "Init", but should only clear data that is not static / depends on memory reads

      Bounds (and maybe all such data) should be stored on the value object)
    *)
    procedure ResetValueBounds; virtual;
  end;

  { TFpSymbolDwarfTypeBasic }

  TFpSymbolDwarfTypeBasic = class(TFpSymbolDwarfType)
  //function DoGetNestedTypeInfo: TFpSymbolDwarfType; // return nil
  protected
    procedure KindNeeded; override;
    procedure TypeInfoNeeded; override;
    function GetHasBounds: Boolean; override;
  public
    function GetTypedValueObject({%H-}ATypeCast: Boolean): TFpValueDwarf; override;
    function GetValueBounds(AValueObj: TFpValue; out ALowBound,
      AHighBound: Int64): Boolean; override;
    function GetValueLowBound(AValueObj: TFpValue; out ALowBound: Int64): Boolean; override;
    function GetValueHighBound(AValueObj: TFpValue; out AHighBound: Int64): Boolean; override;
  end;

  { TFpSymbolDwarfTypeModifier }

  TFpSymbolDwarfTypeModifier = class(TFpSymbolDwarfType)
  protected
    procedure TypeInfoNeeded; override;
    procedure ForwardToSymbolNeeded; override;
    function GetNextTypeInfoForDataAddress(ATargetType: TFpSymbolDwarfType): TFpSymbolDwarfType; override;
  public
    function GetTypedValueObject(ATypeCast: Boolean): TFpValueDwarf; override;
  end;

  { TFpSymbolDwarfTypeRef }

  TFpSymbolDwarfTypeRef = class(TFpSymbolDwarfTypeModifier)
  protected
    function GetFlags: TDbgSymbolFlags; override;
    function GetDataAddressNext(AValueObj: TFpValueDwarf; var AnAddress: TFpDbgMemLocation;
      out ADoneWork: Boolean; ATargetType: TFpSymbolDwarfType): Boolean; override;
  end;

  { TFpSymbolDwarfTypeDeclaration }

  TFpSymbolDwarfTypeDeclaration = class(TFpSymbolDwarfTypeModifier)
  end;

  { TFpSymbolDwarfTypeSubRange }

  TFpSymbolDwarfTypeSubRange = class(TFpSymbolDwarfTypeModifier)
  // TODO not a modifier, maybe have a forwarder base class
  // GetNextTypeInfoForDataAddress => wrong behaviour, but basetypes should not change addr anyway.
  private
    FLowBoundConst: Int64;
    FLowBoundValue: TFpValue;
    FLowBoundState: TFpDwarfAtEntryDataReadState;
    FHighBoundConst: Int64;
    FHighBoundValue: TFpValue;
    FHighBoundState: TFpDwarfAtEntryDataReadState;
    FCountConst: Int64;
    FCountValue: TFpValue;
    FCountState: TFpDwarfAtEntryDataReadState;
    FLowEnumIdx, FHighEnumIdx: Integer;
    FEnumIdxValid: Boolean;
    procedure InitEnumIdx;
  protected
    function DoGetNestedTypeInfo: TFpSymbolDwarfType;override;
    function GetHasBounds: Boolean; override;

    procedure NameNeeded; override;
    procedure KindNeeded; override;
    procedure SizeNeeded; override;
    function GetNestedSymbol(AIndex: Int64): TFpSymbol; override;
    function GetNestedSymbolCount: Integer; override;
    function GetFlags: TDbgSymbolFlags; override;
    procedure Init; override;
  public
    procedure ResetValueBounds; override;
    destructor Destroy; override;

    function GetValueBounds(AValueObj: TFpValue; out ALowBound, AHighBound: Int64): Boolean; override;
    function GetValueLowBound(AValueObj: TFpValue; out ALowBound: Int64): Boolean; override;
    function GetValueHighBound(AValueObj: TFpValue; out AHighBound: Int64): Boolean; override;
    property LowBoundState: TFpDwarfAtEntryDataReadState read FLowBoundState; deprecated;
    property HighBoundState: TFpDwarfAtEntryDataReadState read FHighBoundState;  deprecated;
    property CountState: TFpDwarfAtEntryDataReadState read FCountState;  deprecated;

  end;

  { TFpSymbolDwarfTypePointer }

  TFpSymbolDwarfTypePointer = class(TFpSymbolDwarfType)
  protected
    procedure KindNeeded; override;
    procedure SizeNeeded; override;
  public
    function GetTypedValueObject(ATypeCast: Boolean): TFpValueDwarf; override;
  end;

  { TFpSymbolDwarfTypeSubroutine }

  TFpSymbolDwarfTypeSubroutine = class(TFpSymbolDwarfType)
  private
    FProcMembers: TRefCntObjList;
    FLastMember: TFpSymbol;
    procedure CreateMembers;
  protected
    //copied from TFpSymbolDwarfDataProc
    function GetNestedSymbol(AIndex: Int64): TFpSymbol; override;
    function GetNestedSymbolByName(AIndex: String): TFpSymbol; override;
    function GetNestedSymbolCount: Integer; override;

    // TODO: deal with DW_TAG_pointer_type
    function GetDataAddressNext(AValueObj: TFpValueDwarf; var AnAddress: TFpDbgMemLocation;
      out ADoneWork: Boolean; ATargetType: TFpSymbolDwarfType): Boolean; override;
    procedure KindNeeded; override;
  public
    destructor Destroy; override;
    function GetTypedValueObject({%H-}ATypeCast: Boolean): TFpValueDwarf; override;
  end;

  { TFpSymbolDwarfDataEnumMember }

  TFpSymbolDwarfDataEnumMember  = class(TFpSymbolDwarfData)
    FOrdinalValue: Int64;
    FOrdinalValueRead, FHasOrdinalValue: Boolean;
    procedure ReadOrdinalValue;
  protected
    procedure KindNeeded; override;
    function GetHasOrdinalValue: Boolean; override;
    function GetOrdinalValue: Int64; override;
    procedure Init; override;
    function GetValueObject: TFpValue; override;
  end;


  { TFpSymbolDwarfTypeEnum }

  TFpSymbolDwarfTypeEnum = class(TFpSymbolDwarfType)
  private
    FMembers: TFpDbgCircularRefCntObjList;
    procedure CreateMembers;
  protected
    procedure KindNeeded; override;
    function GetNestedSymbol(AIndex: Int64): TFpSymbol; override;
    function GetNestedSymbolByName(AIndex: String): TFpSymbol; override;
    function GetNestedSymbolCount: Integer; override;

    function GetHasBounds: Boolean; override;
  public
    destructor Destroy; override;
    function GetTypedValueObject({%H-}ATypeCast: Boolean): TFpValueDwarf; override;
    function GetValueBounds(AValueObj: TFpValue; out ALowBound,
      AHighBound: Int64): Boolean; override;
    function GetValueLowBound(AValueObj: TFpValue; out ALowBound: Int64): Boolean; override;
    function GetValueHighBound(AValueObj: TFpValue; out AHighBound: Int64): Boolean; override;
  end;


  { TFpSymbolDwarfTypeSet }

  TFpSymbolDwarfTypeSet = class(TFpSymbolDwarfType)
  protected
    procedure KindNeeded; override;
    function GetNestedSymbolCount: Integer; override;
    function GetNestedSymbol(AIndex: Int64): TFpSymbol; override;
  public
    function GetTypedValueObject({%H-}ATypeCast: Boolean): TFpValueDwarf; override;
  end;

  (*
    If not specified
         .NestedTypeInfo --> copy of TypeInfo
         .ParentTypeInfo --> nil

    ParentTypeInfo:     has a weak RefCount (only AddRef, if self has other refs)


    AnObject = TFpSymbolDwarfDataVariable
     |-- .TypeInfo       --> TBar = TFpSymbolDwarfTypeStructure  [*1]
     |-- .ParentTypeInfo --> may point to subroutine, if param or local var // TODO

    TBar = TFpSymbolDwarfTypeStructure
     |-- .TypeInfo       --> TBarBase = TFpSymbolDwarfTypeStructure

    TBarBase = TFpSymbolDwarfTypeStructure
     |-- .TypeInfo       --> TOBject = TFpSymbolDwarfTypeStructure

    TObject = TFpSymbolDwarfTypeStructure
     |-- .TypeInfo       --> nil


    FField = TFpSymbolDwarfDataMember (declared in TBarBase)
     |-- .TypeInfo       --> Integer = TFpSymbolDwarfTypeBasic [*1]
     |-- .ParentTypeInfo --> TBarBase

    [*1] May have TFpSymbolDwarfTypeDeclaration or others
  *)

  { TFpSymbolDwarfDataMember }

  TFpSymbolDwarfDataMember = class(TFpSymbolDwarfDataWithLocation)
  protected
    function GetValueAddress(AValueObj: TFpValueDwarf; out AnAddress: TFpDbgMemLocation): Boolean; override;
    function HasAddress: Boolean; override;
  end;

  { TFpSymbolDwarfTypeStructure }

  TFpSymbolDwarfTypeStructure = class(TFpSymbolDwarfType)
  // record or class
  private
    FMembers: TFpDbgCircularRefCntObjList;
    FLastChildByName: TFpSymbolDwarf;
    FInheritanceInfo: TDwarfInformationEntry;
    procedure CreateMembers;
    procedure InitInheritanceInfo; inline;
  protected
    function DoGetNestedTypeInfo: TFpSymbolDwarfType; override;
    procedure KindNeeded; override;

    // GetNestedSymbol, if AIndex > Count then parent
    function GetNestedSymbol(AIndex: Int64): TFpSymbol; override;
    function GetNestedSymbolByName(AIndex: String): TFpSymbol; override;
    function GetNestedSymbolCount: Integer; override;

    function GetDataAddressNext(AValueObj: TFpValueDwarf; var AnAddress: TFpDbgMemLocation;
      out ADoneWork: Boolean; ATargetType: TFpSymbolDwarfType): Boolean; override;
  public
    destructor Destroy; override;
    function GetTypedValueObject(ATypeCast: Boolean): TFpValueDwarf; override;
  end;

  { TFpSymbolDwarfTypeArray }

  TFpSymbolDwarfTypeArray = class(TFpSymbolDwarfType)
  private
    FMembers: TFpDbgCircularRefCntObjList;
    FRowMajor: Boolean;
    FStrideInBits: Int64;
    FDwarfArrayReadFlags: set of (didtStrideRead, didtOrdering);
    procedure CreateMembers;
    procedure ReadStride;
    procedure ReadOrdering;
  protected
    procedure KindNeeded; override;

    function GetFlags: TDbgSymbolFlags; override;
    // GetNestedSymbol: returns the TYPE/range of each index. NOT the data
    function GetNestedSymbol(AIndex: Int64): TFpSymbol; override;
    function GetNestedSymbolByName({%H-}AIndex: String): TFpSymbol; override;
    function GetNestedSymbolCount: Integer; override;
    function GetMemberAddress(AValObject: TFpValueDwarf; const AIndex: Array of Int64): TFpDbgMemLocation;
  public
    destructor Destroy; override;
    function GetTypedValueObject({%H-}ATypeCast: Boolean): TFpValueDwarf; override;
    procedure ResetValueBounds; override;
  end;

  { TFpSymbolDwarfDataProc }

  TFpSymbolDwarfDataProc = class(TFpSymbolDwarfData)
  private
    //FCU: TDwarfCompilationUnit;
    FProcMembers: TRefCntObjList; // Locals
    FLastMember: TFpSymbol;
    FAddress: TDbgPtr;
    FAddressInfo: PDwarfAddressInfo;
    FStateMachine: TDwarfLineInfoStateMachine;
    FFrameBaseParser: TDwarfLocationExpression;
    FSelfParameter: TFpValueDwarf;
    function StateMachineValid: Boolean;
    function  ReadVirtuality(out AFlags: TDbgSymbolFlags): Boolean;
    procedure CreateMembers;
  protected
    function GetNestedSymbol(AIndex: Int64): TFpSymbol; override;
    function GetNestedSymbolByName(AIndex: String): TFpSymbol; override;
    function GetNestedSymbolCount: Integer; override;

    function  GetFrameBase(ASender: TDwarfLocationExpression): TDbgPtr;
    procedure KindNeeded; override;
    procedure SizeNeeded; override;
    function GetFlags: TDbgSymbolFlags; override;
    procedure TypeInfoNeeded; override;

    function GetColumn: Cardinal; override;
    function GetFile: String; override;
//    function GetFlags: TDbgSymbolFlags; override;
    function GetLine: Cardinal; override;
    function GetValueObject: TFpValue; override;
    function GetValueAddress(AValueObj: TFpValueDwarf; out
      AnAddress: TFpDbgMemLocation): Boolean; override;
  public
    constructor Create(ACompilationUnit: TDwarfCompilationUnit; AInfo: PDwarfAddressInfo; AAddress: TDbgPtr); overload;
    destructor Destroy; override;
    // TODO members = locals ?
    function GetSelfParameter(AnAddress: TDbgPtr = 0): TFpValueDwarf;
  end;

  { TFpSymbolDwarfTypeProc }

  TFpSymbolDwarfTypeProc = class(TFpSymbolDwarfType)
  private
    FDataSymbol: TFpSymbolDwarfDataProc;
  protected
    procedure ForwardToSymbolNeeded; override;
    procedure CircleBackRefActiveChanged(ANewActive: Boolean); override;
    procedure NameNeeded; override;
    procedure KindNeeded; override;
    procedure TypeInfoNeeded; override;
  public
    constructor Create(AName: String; AnInformationEntry: TDwarfInformationEntry;
      ADataSymbol: TFpSymbolDwarfDataProc);
  end;

  { TFpSymbolDwarfDataVariable }

  TFpSymbolDwarfDataVariable = class(TFpSymbolDwarfDataWithLocation)
  private
    FConstData: TByteDynArray;
  protected
    function GetValueAddress(AValueObj: TFpValueDwarf; out AnAddress: TFpDbgMemLocation): Boolean; override;
    function HasAddress: Boolean; override;
  public
  end;

  { TFpSymbolDwarfDataParameter }

  TFpSymbolDwarfDataParameter = class(TFpSymbolDwarfDataWithLocation)
  protected
    function GetValueAddress(AValueObj: TFpValueDwarf; out AnAddress: TFpDbgMemLocation): Boolean; override;
    function HasAddress: Boolean; override;
    function GetFlags: TDbgSymbolFlags; override;
  public
  end;

  { TFpSymbolDwarfUnit }

  TFpSymbolDwarfUnit = class(TFpSymbolDwarf)
  private
    FLastChildByName: TFpSymbol;
  protected
    procedure Init; override;
    function GetNestedSymbolByName(AIndex: String): TFpSymbol; override;
  public
    destructor Destroy; override;
  end;
{%endregion Symbol objects }

function dbgs(ASubRangeBoundReadState: TFpDwarfAtEntryDataReadState): String; overload;

implementation

var
  DBG_WARNINGS, FPDBG_DWARF_VERBOSE, FPDBG_DWARF_ERRORS, FPDBG_DWARF_WARNINGS, FPDBG_DWARF_SEARCH, FPDBG_DWARF_DATA_WARNINGS: PLazLoggerLogGroup;

function dbgs(ASubRangeBoundReadState: TFpDwarfAtEntryDataReadState): String;
begin
  WriteStr(Result, ASubRangeBoundReadState);
end;

{ TFpValueDwarfSubroutine }

function TFpValueDwarfSubroutine.IsValidTypeCast: Boolean;
var
  f: TFpValueFieldFlags;
begin
  Result := HasTypeCastInfo;
  If not Result then
    exit;

  f := FTypeCastSourceValue.FieldFlags;
  if (f * [svfAddress, svfSize, svfSizeOfPointer] = [svfAddress]) then
    exit;

  if (svfOrdinal in f)then
    exit;
  if (f * [svfAddress, svfSize] = [svfAddress, svfSize]) and
     (FTypeCastSourceValue.Size = FOwner.CompilationUnit.AddressSize)
  then
    exit;
  if (f * [svfAddress, svfSizeOfPointer] = [svfAddress, svfSizeOfPointer]) then
    exit;

  Result := False;
end;

{ TFpDwarfDefaultSymbolClassMap }

class function TFpDwarfDefaultSymbolClassMap.GetExistingClassMap: PFpDwarfSymbolClassMap;
begin
  Result := @ExistingClassMap;
end;

class function TFpDwarfDefaultSymbolClassMap.ClassCanHandleCompUnit(ACU: TDwarfCompilationUnit): Boolean;
begin
  Result := True;
end;

function TFpDwarfDefaultSymbolClassMap.GetDwarfSymbolClass(ATag: Cardinal): TDbgDwarfSymbolBaseClass;
begin
  case ATag of
    // TODO:
    DW_TAG_constant:
      Result := TFpSymbolDwarfData;
    DW_TAG_string_type,
    DW_TAG_union_type, DW_TAG_ptr_to_member_type,
    DW_TAG_file_type,
    DW_TAG_thrown_type:
      Result := TFpSymbolDwarfType;

    // Type types
    DW_TAG_packed_type,
    DW_TAG_const_type,
    DW_TAG_volatile_type:    Result := TFpSymbolDwarfTypeModifier;
    DW_TAG_reference_type:   Result := TFpSymbolDwarfTypeRef;
    DW_TAG_typedef:          Result := TFpSymbolDwarfTypeDeclaration;
    DW_TAG_pointer_type:     Result := TFpSymbolDwarfTypePointer;

    DW_TAG_base_type:        Result := TFpSymbolDwarfTypeBasic;
    DW_TAG_subrange_type:    Result := TFpSymbolDwarfTypeSubRange;
    DW_TAG_enumeration_type: Result := TFpSymbolDwarfTypeEnum;
    DW_TAG_enumerator:       Result := TFpSymbolDwarfDataEnumMember;
    DW_TAG_set_type:         Result := TFpSymbolDwarfTypeSet;
    DW_TAG_structure_type,
    DW_TAG_interface_type,
    DW_TAG_class_type:       Result := TFpSymbolDwarfTypeStructure;
    DW_TAG_array_type:       Result := TFpSymbolDwarfTypeArray;
    DW_TAG_subroutine_type:  Result := TFpSymbolDwarfTypeSubroutine;
    // Value types
    DW_TAG_variable:         Result := TFpSymbolDwarfDataVariable;
    DW_TAG_formal_parameter: Result := TFpSymbolDwarfDataParameter;
    DW_TAG_member:           Result := TFpSymbolDwarfDataMember;
    DW_TAG_subprogram:       Result := TFpSymbolDwarfDataProc;
    //DW_TAG_inlined_subroutine, DW_TAG_entry_poin
    //
    DW_TAG_compile_unit:     Result := TFpSymbolDwarfUnit;

    else
      Result := TFpSymbolDwarf;
  end;
end;

function TFpDwarfDefaultSymbolClassMap.CreateContext(AThreadId, AStackFrame: Integer;
  AnAddress: TDbgPtr; ASymbol: TFpSymbol; ADwarf: TFpDwarfInfo): TFpDbgInfoContext;
begin
  Result := TFpDwarfInfoAddressContext.Create(AThreadId, AStackFrame, AnAddress, ASymbol, ADwarf);
end;

function TFpDwarfDefaultSymbolClassMap.CreateProcSymbol(ACompilationUnit: TDwarfCompilationUnit;
  AInfo: PDwarfAddressInfo; AAddress: TDbgPtr): TDbgDwarfSymbolBase;
begin
  Result := TFpSymbolDwarfDataProc.Create(ACompilationUnit, AInfo, AAddress);
end;

{ TDbgDwarfInfoAddressContext }

function TFpDwarfInfoAddressContext.GetSymbolAtAddress: TFpSymbol;
begin
  Result := FSymbol;
end;

function TFpDwarfInfoAddressContext.GetProcedureAtAddress: TFpValue;
begin
  Result := inherited GetProcedureAtAddress;
  ApplyContext(Result);
end;

function TFpDwarfInfoAddressContext.GetAddress: TDbgPtr;
begin
  Result := FAddress;
end;

function TFpDwarfInfoAddressContext.GetThreadId: Integer;
begin
  Result := FThreadId;
end;

function TFpDwarfInfoAddressContext.GetStackFrame: Integer;
begin
  Result := FStackFrame;
end;

function TFpDwarfInfoAddressContext.GetSizeOfAddress: Integer;
begin
  assert(FSymbol is TFpSymbolDwarf, 'TDbgDwarfInfoAddressContext.GetSizeOfAddress');
  Result := TFpSymbolDwarf(FSymbol).CompilationUnit.AddressSize;
end;

function TFpDwarfInfoAddressContext.GetMemManager: TFpDbgMemManager;
begin
  Result := FDwarf.MemManager;
end;

procedure TFpDwarfInfoAddressContext.ApplyContext(AVal: TFpValue);
begin
  if (AVal <> nil) and (TFpValueDwarfBase(AVal).FContext = nil) then
    TFpValueDwarfBase(AVal).FContext := Self;
end;

function TFpDwarfInfoAddressContext.SymbolToValue(ASym: TFpSymbol): TFpValue;
begin
  if ASym = nil then begin
    Result := nil;
    exit;
  end;

  if ASym.SymbolType = stValue then begin
    Result := ASym.Value;
    if Result <> nil then
      Result.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FlastResult, 'FindSymbol'){$ENDIF};
  end
  else begin
    Result := TFpValueDwarfTypeDefinition.Create(ASym);
    {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(@FlastResult, 'FindSymbol'){$ENDIF};
  end;
  ASym.ReleaseReference;
end;

procedure TFpDwarfInfoAddressContext.AddRefToVal(AVal: TFpValue);
begin
  AVal.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FlastResult, 'FindSymbol'){$ENDIF};
end;

function TFpDwarfInfoAddressContext.GetSelfParameter: TFpValue;
begin
  Result := TFpSymbolDwarfDataProc(FSymbol).GetSelfParameter(FAddress);
  if (Result <> nil) and (TFpValueDwarfBase(Result).FContext = nil) then
    TFpValueDwarfBase(Result).FContext := Self;
end;

function TFpDwarfInfoAddressContext.FindExportedSymbolInUnits(const AName: String; PNameUpper,
  PNameLower: PChar; SkipCompUnit: TDwarfCompilationUnit; out ADbgValue: TFpValue): Boolean;
var
  i, ExtVal: Integer;
  CU: TDwarfCompilationUnit;
  InfoEntry, FoundInfoEntry: TDwarfInformationEntry;
  s: String;
begin
  Result := False;
  ADbgValue := nil;
  InfoEntry := nil;
  FoundInfoEntry := nil;
  i := FDwarf.CompilationUnitsCount;
  while i > 0 do begin
    dec(i);
    CU := FDwarf.CompilationUnits[i];
    if CU = SkipCompUnit then
      continue;
    //DebugLn(FPDBG_DWARF_SEARCH, ['TDbgDwarf.FindIdentifier search UNIT Name=', CU.FileName]);

    InfoEntry.ReleaseReference;
    InfoEntry := TDwarfInformationEntry.Create(CU, nil);
    InfoEntry.ScopeIndex := CU.FirstScope.Index;

    if not InfoEntry.AbbrevTag = DW_TAG_compile_unit then
      continue;
    // compile_unit can not have startscope

    s := CU.UnitName;
    if (s <> '') and (CompareUtf8BothCase(PNameUpper, PNameLower, @s[1])) then begin
      ReleaseRefAndNil(FoundInfoEntry);
      ADbgValue := SymbolToValue(TFpSymbolDwarf.CreateSubClass(AName, InfoEntry));
      break;
    end;

    CU.ScanAllEntries;
    if InfoEntry.GoNamedChildEx(PNameUpper, PNameLower) then begin
      if InfoEntry.IsAddressInStartScope(FAddress) then begin
        // only variables are marked "external", but types not / so we may need all top level
        FoundInfoEntry.ReleaseReference;
        FoundInfoEntry := InfoEntry.Clone;
        //DebugLn(FPDBG_DWARF_SEARCH, ['TDbgDwarf.FindIdentifier MAYBE FOUND Name=', CU.FileName]);

        // DW_AT_visibility ?
        if InfoEntry.ReadValue(DW_AT_external, ExtVal) then
          if ExtVal <> 0 then
            break;
        // Search for better ADbgValue
      end;
    end;
  end;

  if FoundInfoEntry <> nil then begin;
    ADbgValue := SymbolToValue(TFpSymbolDwarf.CreateSubClass(AName, FoundInfoEntry));
    FoundInfoEntry.ReleaseReference;
  end;

  InfoEntry.ReleaseReference;
  Result := ADbgValue <> nil;
end;

function TFpDwarfInfoAddressContext.FindSymbolInStructure(const AName: String; PNameUpper,
  PNameLower: PChar; InfoEntry: TDwarfInformationEntry; out ADbgValue: TFpValue): Boolean;
var
  InfoEntryInheritance: TDwarfInformationEntry;
  FwdInfoPtr: Pointer;
  FwdCompUint: TDwarfCompilationUnit;
  SelfParam: TFpValue;
begin
  Result := False;
  ADbgValue := nil;
  InfoEntry.AddReference;

  while True do begin
    if not InfoEntry.IsAddressInStartScope(FAddress) then
      break;

    InfoEntryInheritance := InfoEntry.FindChildByTag(DW_TAG_inheritance);

    if InfoEntry.GoNamedChildEx(PNameUpper, PNameLower) then begin
      if InfoEntry.IsAddressInStartScope(FAddress) then begin
        SelfParam := GetSelfParameter;
        if (SelfParam <> nil) then begin
          // TODO: only valid, as long as context is valid, because if context is freed, then self is lost too
          ADbgValue := SelfParam.MemberByName[AName];
          assert(ADbgValue <> nil, 'FindSymbol: SelfParam.MemberByName[AName]');
          if ADbgValue <> nil then
            ADbgValue.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FlastResult, 'FindSymbol'){$ENDIF};
        end
else debugln(['TDbgDwarfInfoAddressContext.FindSymbol XXXXXXXXXXXXX no self']);
        ;
        if ADbgValue = nil then begin // Todo: abort the searh /SetError
          ADbgValue := SymbolToValue(TFpSymbolDwarf.CreateSubClass(AName, InfoEntry));
        end;
        InfoEntry.ReleaseReference;
        InfoEntryInheritance.ReleaseReference;
        Result := True;
        exit;
      end;
    end;


    if not( (InfoEntryInheritance <> nil) and
            (InfoEntryInheritance.ReadReference(DW_AT_type, FwdInfoPtr, FwdCompUint)) )
    then
      break;
    InfoEntry.ReleaseReference;
    InfoEntry := TDwarfInformationEntry.Create(FwdCompUint, FwdInfoPtr);
    InfoEntryInheritance.ReleaseReference;
    DebugLn(FPDBG_DWARF_SEARCH, ['TDbgDwarf.FindIdentifier  PARENT ', dbgs(InfoEntry, FwdCompUint) ]);
  end;

  InfoEntry.ReleaseReference;
  Result := ADbgValue <> nil;
end;

function TFpDwarfInfoAddressContext.FindLocalSymbol(const AName: String; PNameUpper,
  PNameLower: PChar; InfoEntry: TDwarfInformationEntry; out ADbgValue: TFpValue): Boolean;
begin
  Result := False;
  ADbgValue := nil;
  if not InfoEntry.GoNamedChildEx(PNameUpper, PNameLower) then
    exit;
  if InfoEntry.IsAddressInStartScope(FAddress) and not InfoEntry.IsArtificial then begin
    ADbgValue := SymbolToValue(TFpSymbolDwarf.CreateSubClass(AName, InfoEntry));
    if ADbgValue <> nil then
      TFpSymbolDwarf(ADbgValue.DbgSymbol).ParentTypeInfo := TFpSymbolDwarfDataProc(FSymbol);
  end;
  Result := ADbgValue <> nil;
end;

constructor TFpDwarfInfoAddressContext.Create(AThreadId, AStackFrame: Integer;
  AnAddress: TDbgPtr; ASymbol: TFpSymbol; ADwarf: TFpDwarfInfo);
begin
  inherited Create;
  AddReference;
  FAddress := AnAddress;
  FThreadId := AThreadId;
  FStackFrame := AStackFrame;
  FDwarf   := ADwarf;
  FSymbol  := ASymbol;
  FSymbol.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSymbol, 'Context to Symbol'){$ENDIF};
end;

destructor TFpDwarfInfoAddressContext.Destroy;
begin
  FlastResult.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FlastResult, 'FindSymbol'){$ENDIF};
  FSymbol.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSymbol, 'Context to Symbol'){$ENDIF};
  inherited Destroy;
end;

function TFpDwarfInfoAddressContext.FindSymbol(const AName: String): TFpValue;
var
  SubRoutine: TFpSymbolDwarfDataProc; // TDbgSymbol;
  CU: TDwarfCompilationUnit;
  //Scope,
  StartScopeIdx: Integer;
  InfoEntry: TDwarfInformationEntry;
  NameUpper, NameLower: String;
  InfoName: PChar;
  tg: Cardinal;
  PNameUpper, PNameLower: PChar;
begin
  Result := nil;
  if (FSymbol = nil) or not(FSymbol is TFpSymbolDwarfDataProc) or (AName = '') then
    exit;

  SubRoutine := TFpSymbolDwarfDataProc(FSymbol);
  NameUpper := UTF8UpperCase(AName);
  NameLower := UTF8LowerCase(AName);
  PNameUpper := @NameUpper[1];
  PNameLower := @NameLower[1];

  try
    CU := SubRoutine.CompilationUnit;
    InfoEntry := SubRoutine.InformationEntry.Clone;

    while InfoEntry.HasValidScope do begin
      //debugln(FPDBG_DWARF_SEARCH, ['TDbgDwarf.FindIdentifier Searching ', dbgs(InfoEntry.FScope, CU)]);
      StartScopeIdx := InfoEntry.ScopeIndex;

      //if InfoEntry.Abbrev = nil then
      //  exit;

      if not InfoEntry.IsAddressInStartScope(FAddress) // StartScope = first valid address
      then begin
        // CONTINUE: Search parent(s)
        //InfoEntry.ScopeIndex := StartScopeIdx;
        InfoEntry.GoParent;
        Continue;
      end;

      if InfoEntry.ReadName(InfoName) and not InfoEntry.IsArtificial
      then begin
        if (CompareUtf8BothCase(PNameUpper, PNameLower, InfoName)) then begin
          // TODO: this is a pascal sperific search order? Or not?
          // If this is a type with a pointer or ref, need to find the pointer or ref.
          InfoEntry.GoParent;
          if InfoEntry.HasValidScope and
             InfoEntry.GoNamedChildEx(PNameUpper, PNameLower)
          then begin
            if InfoEntry.IsAddressInStartScope(FAddress) and not InfoEntry.IsArtificial then begin
              Result := SymbolToValue(TFpSymbolDwarf.CreateSubClass(AName, InfoEntry));
              exit;
            end;
          end;

          InfoEntry.ScopeIndex := StartScopeIdx;
          Result := SymbolToValue(TFpSymbolDwarf.CreateSubClass(AName, InfoEntry));
          exit;
        end;
      end;


      tg := InfoEntry.AbbrevTag;
      if (tg = DW_TAG_class_type) or (tg = DW_TAG_structure_type) then begin
        if FindSymbolInStructure(AName,PNameUpper, PNameLower, InfoEntry, Result) then
          exit; // TODO: check error
        //InfoEntry.ScopeIndex := StartScopeIdx;
      end

      else
      if (StartScopeIdx = SubRoutine.InformationEntry.ScopeIndex) then begin // searching in subroutine
        if FindLocalSymbol(AName,PNameUpper, PNameLower, InfoEntry, Result) then
          exit;        // TODO: check error
        //InfoEntry.ScopeIndex := StartScopeIdx;
      end
          // TODO: nested subroutine

      else
      if InfoEntry.GoNamedChildEx(PNameUpper, PNameLower) then begin
        if InfoEntry.IsAddressInStartScope(FAddress) and not InfoEntry.IsArtificial then begin
          Result := SymbolToValue(TFpSymbolDwarf.CreateSubClass(AName, InfoEntry));
          exit;
        end;
      end;

      // Search parent(s)
      InfoEntry.ScopeIndex := StartScopeIdx;
      InfoEntry.GoParent;
    end;

    FindExportedSymbolInUnits(AName, PNameUpper, PNameLower, CU, Result);

  finally
    if (Result = nil) or (InfoEntry = nil)
    then DebugLn(FPDBG_DWARF_SEARCH, ['TDbgDwarf.FindIdentifier NOT found  Name=', AName])
    else DebugLn(FPDBG_DWARF_SEARCH, ['TDbgDwarf.FindIdentifier(',AName,') found Scope=', TFpSymbolDwarf(Result.DbgSymbol).InformationEntry.ScopeDebugText, '  ResultSymbol=', DbgSName(Result.DbgSymbol), ' ', Result.DbgSymbol.Name, ' in ', TFpSymbolDwarf(Result.DbgSymbol).CompilationUnit.FileName]);
    ReleaseRefAndNil(InfoEntry);

    FlastResult.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FlastResult, 'FindSymbol'){$ENDIF};
    FlastResult := Result;

    assert((Result = nil) or (Result is TFpValueDwarfBase), 'TDbgDwarfInfoAddressContext.FindSymbol: (Result = nil) or (Result is TFpValueDwarfBase)');
    ApplyContext(Result);
  end;
end;

{ TFpValueDwarfTypeDefinition }

function TFpValueDwarfTypeDefinition.GetKind: TDbgSymbolKind;
begin
  Result := skType;
end;

function TFpValueDwarfTypeDefinition.GetDbgSymbol: TFpSymbol;
begin
  Result := FSymbol;
end;

constructor TFpValueDwarfTypeDefinition.Create(ASymbol: TFpSymbol);
begin
  inherited Create;
  FSymbol := ASymbol;
  FSymbol.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSymbol, 'TFpValueDwarfTypeDefinition'){$ENDIF};
end;

destructor TFpValueDwarfTypeDefinition.Destroy;
begin
  inherited Destroy;
  FSymbol.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSymbol, 'TFpValueDwarfTypeDefinition'){$ENDIF};
end;

function TFpValueDwarfTypeDefinition.GetTypeCastedValue(ADataVal: TFpValue): TFpValue;
begin
  Result := FSymbol.TypeCastValue(ADataVal);
  assert((Result = nil) or (Result is TFpValueDwarf), 'TFpValueDwarfTypeDefinition.GetTypeCastedValue: (Result = nil) or (Result is TFpValueDwarf)');
  if (Result <> nil) and (TFpValueDwarf(Result).FContext = nil) then
    TFpValueDwarf(Result).FContext := FContext;
end;

{ TFpValueDwarf }

function TFpValueDwarf.MemManager: TFpDbgMemManager;
begin
  Result := nil;
  if FContext <> nil then
    Result := FContext.MemManager;

  if Result = nil then begin
    // Either a typecast, or a member gotten from a typecast,...
    assert((FOwner <> nil) and (FOwner.CompilationUnit <> nil) and (FOwner.CompilationUnit.Owner <> nil), 'TDbgDwarfSymbolValue.MemManager');
    Result := FOwner.CompilationUnit.Owner.MemManager;
  end;
end;

function TFpValueDwarf.AddressSize: Byte;
begin
  assert((FOwner <> nil) and (FOwner.CompilationUnit <> nil), 'TDbgDwarfSymbolValue.AddressSize');
  Result := FOwner.CompilationUnit.AddressSize;
end;

procedure TFpValueDwarf.SetStructureValue(AValue: TFpValueDwarf);
begin
  if FStructureValue <> nil then
    Reset;

  if FStructureValue = AValue then
    exit;

  if CircleBackRefsActive and (FStructureValue <> nil) then
    FStructureValue.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FStructureValue, 'TDbgDwarfSymbolValue'){$ENDIF};
  FStructureValue := AValue;
  if CircleBackRefsActive and (FStructureValue <> nil) then
    FStructureValue.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FStructureValue, 'TDbgDwarfSymbolValue'){$ENDIF};
end;

function TFpValueDwarf.GetLastError: TFpError;
begin
  Result := FLastError;
end;

function TFpValueDwarf.OrdOrDataAddr: TFpDbgMemLocation;
begin
  if HasTypeCastInfo and (svfOrdinal in FTypeCastSourceValue.FieldFlags) then
    Result := ConstLoc(FTypeCastSourceValue.AsCardinal)
  else
    GetDwarfDataAddress(Result);
end;

function TFpValueDwarf.GetDataAddress: TFpDbgMemLocation;
begin
  GetDwarfDataAddress(Result);
end;

function TFpValueDwarf.GetDwarfDataAddress(out AnAddress: TFpDbgMemLocation;
  ATargetType: TFpSymbolDwarfType): Boolean;
var
  fields: TFpValueFieldFlags;
  ti: TFpSymbol;
begin
  AnAddress := FCachedDataAddress;
  Result := IsInitializedLoc(AnAddress);
  if Result then
    exit(IsValidLoc(AnAddress));

  FCachedDataAddress := InvalidLoc;

  if FDataSymbol <> nil then begin
    Assert(FDataSymbol is TFpSymbolDwarfData, 'TDbgDwarfSymbolValue.GetDwarfDataAddress FValueSymbol');
    Assert(TypeInfo is TFpSymbolDwarfType, 'TDbgDwarfSymbolValue.GetDwarfDataAddress TypeInfo');
    Assert(not HasTypeCastInfo, 'TDbgDwarfSymbolValue.GetDwarfDataAddress not HasTypeCastInfo');

    ti := FDataSymbol.TypeInfo;
    Result := ti <> nil;
    if not Result then
      exit;
    Assert((ti is TFpSymbolDwarfType) and (ti.SymbolType = stType), 'TDbgDwarfSymbolValue.GetDwarfDataAddress TypeInfo = stType');

    AnAddress := Address;
    Result := IsReadableLoc(AnAddress);

    if Result then begin
      Result := TFpSymbolDwarf(ti).GetDataAddress(Self, AnAddress, ATargetType);
      if not Result then
        FLastError := ti.LastError;
    end;
  end

  else
  begin
    // TODO: cache own address
    // try typecast
    AnAddress := InvalidLoc;
    Result := HasTypeCastInfo;
    if not Result then
      exit;
    fields := FTypeCastSourceValue.FieldFlags;
    if svfOrdinal in fields then
      AnAddress := ConstLoc(FTypeCastSourceValue.AsCardinal)
    else
    if svfAddress in fields then
      AnAddress := FTypeCastSourceValue.Address;

    Result := IsReadableLoc(AnAddress);
    if Result then begin
      Result := FTypeCastTargetType.GetDataAddress(Self, AnAddress, ATargetType);
      if IsError(FTypeCastTargetType.LastError) then
        FLastError := FTypeCastTargetType.LastError;
    end;
  end;

  if not Result then
    AnAddress := InvalidLoc;
  FCachedDataAddress := AnAddress;
end;

function TFpValueDwarf.GetStructureDwarfDataAddress(out AnAddress: TFpDbgMemLocation;
  ATargetType: TFpSymbolDwarfType): Boolean;
begin
  AnAddress := InvalidLoc;
  Result := StructureValue <> nil;
  if Result then
    Result := StructureValue.GetDwarfDataAddress(AnAddress, ATargetType);
end;

procedure TFpValueDwarf.Reset;
begin
  FCachedAddress := UnInitializedLoc;
  FCachedDataAddress := UnInitializedLoc;
  FLastError := NoError;
end;

function TFpValueDwarf.GetFieldFlags: TFpValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  if FDataSymbol <> nil then begin
    if FDataSymbol.HasAddress then Result := Result + [svfAddress];
  end
  else
  if HasTypeCastInfo then begin
    Result := Result + FTypeCastSourceValue.FieldFlags * [svfAddress];
  end;
end;

function TFpValueDwarf.HasTypeCastInfo: Boolean;
begin
  Result := (FTypeCastTargetType <> nil) and (FTypeCastSourceValue <> nil);
end;

function TFpValueDwarf.IsValidTypeCast: Boolean;
begin
  Result := False;
end;

procedure TFpValueDwarf.DoReferenceAdded;
begin
  inherited DoReferenceAdded;
  DoPlainReferenceAdded;
end;

procedure TFpValueDwarf.DoReferenceReleased;
begin
  inherited DoReferenceReleased;
  DoPlainReferenceReleased;
end;

procedure TFpValueDwarf.CircleBackRefActiveChanged(NewActive: Boolean);
begin
  inherited CircleBackRefActiveChanged(NewActive);
  //if NewActive then;
  if CircleBackRefsActive then begin
    if FDataSymbol <> nil then
      FDataSymbol.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FDataSymbol, 'TDbgDwarfSymbolValue'){$ENDIF};
    if FStructureValue <> nil then
      FStructureValue.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FStructureValue, 'TDbgDwarfSymbolValue'){$ENDIF};
  end
  else begin
    if FDataSymbol <> nil then
      FDataSymbol.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FDataSymbol, 'TDbgDwarfSymbolValue'){$ENDIF};
    if FStructureValue <> nil then
      FStructureValue.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FStructureValue, 'TDbgDwarfSymbolValue'){$ENDIF};
  end;
end;

procedure TFpValueDwarf.SetLastMember(ALastMember: TFpValueDwarf);
begin
  if FLastMember <> nil then
    FLastMember.ReleaseCirclularReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FLastMember, 'TDbgDwarfSymbolValue'){$ENDIF};

  FLastMember := ALastMember;

  if (FLastMember <> nil) then begin
    FLastMember.SetStructureValue(Self);
    FLastMember.AddCirclularReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FLastMember, 'TDbgDwarfSymbolValue'){$ENDIF};
    if (FLastMember.FContext = nil) then
      FLastMember.FContext := FContext;
  end;
end;

function TFpValueDwarf.GetKind: TDbgSymbolKind;
begin
  if FDataSymbol <> nil then
    Result := FDataSymbol.Kind
  else
  if HasTypeCastInfo then
    Result := FTypeCastTargetType.Kind
  else
    Result := inherited GetKind;
end;

function TFpValueDwarf.GetAddress: TFpDbgMemLocation;
begin
  if IsInitializedLoc(FCachedAddress) then
    exit(FCachedAddress);

  if FDataSymbol <> nil then
    FDataSymbol.GetValueAddress(Self, Result)
  else
  if HasTypeCastInfo then
    Result := FTypeCastSourceValue.Address
  else
    Result := inherited GetAddress;

  assert(IsInitializedLoc(Result), 'TFpValueDwarf.GetAddress: IsInitializedLoc(Result)');
  FCachedAddress := Result;
end;

function TFpValueDwarf.OrdOrAddress: TFpDbgMemLocation;
begin
  if HasTypeCastInfo and (svfOrdinal in FTypeCastSourceValue.FieldFlags) then
    Result := ConstLoc(FTypeCastSourceValue.AsCardinal)
  else
    Result := Address;
end;

function TFpValueDwarf.GetMemberCount: Integer;
begin
  if FDataSymbol <> nil then
    Result := FDataSymbol.NestedSymbolCount
  else
    Result := inherited GetMemberCount;
end;

function TFpValueDwarf.GetMemberByName(AIndex: String): TFpValue;
var
  m: TFpSymbol;
begin
  Result := nil;
  if FDataSymbol <> nil then begin
    m := FDataSymbol.NestedSymbolByName[AIndex];
    if m <> nil then
      Result := m.Value;
  end;
  SetLastMember(TFpValueDwarf(Result));
end;

function TFpValueDwarf.GetMember(AIndex: Int64): TFpValue;
var
  m: TFpSymbol;
begin
  Result := nil;
  if FDataSymbol <> nil then begin
    m := FDataSymbol.NestedSymbol[AIndex];
    if m <> nil then
      Result := m.Value;
  end;
  SetLastMember(TFpValueDwarf(Result));
end;

function TFpValueDwarf.GetDbgSymbol: TFpSymbol;
begin
  Result := FDataSymbol;
end;

function TFpValueDwarf.GetTypeInfo: TFpSymbol;
begin
  if HasTypeCastInfo then
    Result := FTypeCastTargetType
  else
    Result := inherited GetTypeInfo;
end;

function TFpValueDwarf.GetContextTypeInfo: TFpSymbol;
begin
  if (FDataSymbol <> nil) and (FDataSymbol.ParentTypeInfo <> nil) then
    Result := FDataSymbol.ParentTypeInfo
  else
    Result := nil; // internal error
end;

constructor TFpValueDwarf.Create(AOwner: TFpSymbolDwarfType);
begin
  FOwner := AOwner;
  inherited Create;
end;

destructor TFpValueDwarf.Destroy;
begin
  FTypeCastTargetType.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FTypeCastTargetType, ClassName+'.FTypeCastTargetType'){$ENDIF};
  FTypeCastSourceValue.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FTypeCastSourceValue, ClassName+'.FTypeCastSourceValue'){$ENDIF};
  SetLastMember(nil);
  inherited Destroy;
end;

procedure TFpValueDwarf.SetValueSymbol(AValueSymbol: TFpSymbolDwarfData);
begin
  if FDataSymbol = AValueSymbol then
    exit;

  if CircleBackRefsActive and (FDataSymbol <> nil) then
    FDataSymbol.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FDataSymbol, 'TDbgDwarfSymbolValue'){$ENDIF};
  FDataSymbol := AValueSymbol;
  if CircleBackRefsActive and (FDataSymbol <> nil) then
    FDataSymbol.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FDataSymbol, 'TDbgDwarfSymbolValue'){$ENDIF};
end;

function TFpValueDwarf.SetTypeCastInfo(AStructure: TFpSymbolDwarfType;
  ASource: TFpValue): Boolean;
begin
  Reset;
  AStructure.ResetValueBounds;

  if FTypeCastSourceValue <> ASource then begin
    if FTypeCastSourceValue <> nil then
      FTypeCastSourceValue.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FTypeCastSourceValue, ClassName+'.FTypeCastSourceValue'){$ENDIF};
    FTypeCastSourceValue := ASource;
    if FTypeCastSourceValue <> nil then
      FTypeCastSourceValue.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FTypeCastSourceValue, ClassName+'.FTypeCastSourceValue'){$ENDIF};
  end;

  if FTypeCastTargetType <> AStructure then begin
    if FTypeCastTargetType <> nil then
      FTypeCastTargetType.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FTypeCastTargetType, ClassName+'.FTypeCastTargetType'){$ENDIF};
    FTypeCastTargetType := AStructure;
    if FTypeCastTargetType <> nil then
      FTypeCastTargetType.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FTypeCastTargetType, ClassName+'.FTypeCastTargetType'){$ENDIF};
  end;

  Result := IsValidTypeCast;
end;

{ TFpValueDwarfSized }

function TFpValueDwarfSized.CanUseTypeCastAddress: Boolean;
begin
  Result := True;
  if (FTypeCastSourceValue.FieldFlags * [svfAddress, svfSize, svfSizeOfPointer] = [svfAddress]) then
    exit
  else
  if (FTypeCastSourceValue.FieldFlags * [svfAddress, svfSize] = [svfAddress, svfSize]) and
     (FTypeCastSourceValue.Size = FSize) and (FSize > 0)
  then
    exit;
  if (FTypeCastSourceValue.FieldFlags * [svfAddress, svfSizeOfPointer] = [svfAddress, svfSizeOfPointer]) and
     not ( (FTypeCastTargetType.Kind = skPointer) //or
           //(FSize = AddressSize xxxxxxx)
         )
  then
    exit;
  Result := False;
end;

function TFpValueDwarfSized.GetFieldFlags: TFpValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfSize];
end;

function TFpValueDwarfSized.GetSize: Integer;
begin
  Result := FSize;
end;

constructor TFpValueDwarfSized.Create(AOwner: TFpSymbolDwarfType; ASize: Integer);
begin
  inherited Create(AOwner);
  FSize := ASize;
end;

{ TFpValueDwarfNumeric }

procedure TFpValueDwarfNumeric.Reset;
begin
  inherited Reset;
  FEvaluated := [];
end;

function TFpValueDwarfNumeric.GetFieldFlags: TFpValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfOrdinal];
end;

function TFpValueDwarfNumeric.IsValidTypeCast: Boolean;
begin
  Result := HasTypeCastInfo;
  If not Result then
    exit;
  if (svfOrdinal in FTypeCastSourceValue.FieldFlags) or CanUseTypeCastAddress then
    exit;
  Result := False;
end;

constructor TFpValueDwarfNumeric.Create(AOwner: TFpSymbolDwarfType; ASize: Integer);
begin
  inherited Create(AOwner, ASize);
  FEvaluated := [];
end;

{ TFpValueDwarfInteger }

function TFpValueDwarfInteger.GetFieldFlags: TFpValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfInteger];
end;

function TFpValueDwarfInteger.GetAsCardinal: QWord;
begin
  Result := QWord(GetAsInteger);  // include sign extension
end;

function TFpValueDwarfInteger.GetAsInteger: Int64;
begin
  if doneInt in FEvaluated then begin
    Result := FIntValue;
    exit;
  end;
  Include(FEvaluated, doneInt);

  if (FSize <= 0) or (FSize > SizeOf(Result)) then
    Result := inherited GetAsInteger
  else
  if not MemManager.ReadSignedInt(OrdOrDataAddr, FSize, Result) then begin
    Result := 0; // TODO: error
    FLastError := MemManager.LastError;
  end;

  FIntValue := Result;
end;

{ TDbgDwarfCardinalSymbolValue }

function TFpValueDwarfCardinal.GetAsCardinal: QWord;
begin
  if doneUInt in FEvaluated then begin
    Result := FValue;
    exit;
  end;
  Include(FEvaluated, doneUInt);

  if (FSize <= 0) or (FSize > SizeOf(Result)) then
    Result := inherited GetAsCardinal
  else
  if not MemManager.ReadUnsignedInt(OrdOrDataAddr, FSize, Result) then begin
    Result := 0; // TODO: error
    FLastError := MemManager.LastError;
  end;

  FValue := Result;
end;

function TFpValueDwarfCardinal.GetFieldFlags: TFpValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfCardinal];
end;

{ TFpValueDwarfFloat }

function TFpValueDwarfFloat.GetFieldFlags: TFpValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfFloat] - [svfOrdinal];
end;

function TFpValueDwarfFloat.GetAsFloat: Extended;
begin
  if doneFloat in FEvaluated then begin
    Result := FValue;
    exit;
  end;
  Include(FEvaluated, doneUInt);

  if (FSize <= 0) or (FSize > SizeOf(Result)) then
    Result := inherited GetAsCardinal
  else
  if not MemManager.ReadFloat(OrdOrDataAddr, FSize, Result) then begin
    Result := 0; // TODO: error
    FLastError := MemManager.LastError;
  end;

  FValue := Result;
end;

{ TFpValueDwarfBoolean }

function TFpValueDwarfBoolean.GetFieldFlags: TFpValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfBoolean];
end;

function TFpValueDwarfBoolean.GetAsBool: Boolean;
begin
  Result := QWord(GetAsCardinal) <> 0;
end;

{ TFpValueDwarfChar }

function TFpValueDwarfChar.GetFieldFlags: TFpValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  case FSize of
    1: Result := Result + [svfString];
    2: Result := Result + [svfWideString];
  end;
end;

function TFpValueDwarfChar.GetAsString: AnsiString;
begin
  // Can typecast, because of FSize = 1, GetAsCardinal only read one byte
  if FSize = 2 then
    Result := GetAsWideString  // temporary workaround for WideChar
  else
  if FSize <> 1 then
    Result := inherited GetAsString
  else
    Result := SysToUTF8(char(byte(GetAsCardinal)));
end;

function TFpValueDwarfChar.GetAsWideString: WideString;
begin
  if FSize > 2 then
    Result := inherited GetAsWideString
  else
    Result := WideChar(Word(GetAsCardinal));
end;

{ TFpValueDwarfPointer }

function TFpValueDwarfPointer.GetDerefAddress: TFpDbgMemLocation;
begin
  if doneAddr in FEvaluated then begin
    Result := FPointetToAddr;
    exit;
  end;
  Include(FEvaluated, doneAddr);

  if (FSize <= 0) then
    Result := InvalidLoc
  else
  if not MemManager.ReadAddress(OrdOrDataAddr, Context.SizeOfAddress, Result) then
    FLastError := MemManager.LastError;
  FPointetToAddr := Result;
end;

function TFpValueDwarfPointer.GetAsCardinal: QWord;
var
  a: TFpDbgMemLocation;
begin
  a := GetDerefAddress;
  if IsTargetAddr(a) then
    Result := LocToAddr(a)
  else
    Result := 0;
end;

function TFpValueDwarfPointer.GetFieldFlags: TFpValueFieldFlags;
var
  t: TFpSymbol;
begin
  Result := inherited GetFieldFlags;
  //TODO: svfDataAddress should depend on (hidden) Pointer or Ref in the TypeInfo
  Result := Result + [svfCardinal, svfOrdinal, svfSizeOfPointer, svfDataAddress] - [svfSize]; // data address

  t := TypeInfo;
  if (t <> nil) then t := t.TypeInfo;
  if (t <> nil) and (t.Kind = skChar) and IsReadableMem(GetDerefAddress) then // pchar
    Result := Result + [svfString]; // data address
end;

function TFpValueDwarfPointer.GetDataAddress: TFpDbgMemLocation;
begin
  if (FSize <= 0) then
    Result := InvalidLoc
  else
    Result := inherited;
end;

function TFpValueDwarfPointer.GetAsString: AnsiString;
var
  t: TFpSymbol;
  i: Integer;
begin
  t := TypeInfo;
  if (t <> nil) then t := t.TypeInfo;
  if t.Size = 2 then
    Result := GetAsWideString
  else
  if  (MemManager <> nil) and (t <> nil) and (t.Kind = skChar) and IsReadableMem(GetDerefAddress) then begin // pchar
    SetLength(Result, 2000);
    i := 2000;
    while (i > 0) and (not MemManager.ReadMemory(GetDerefAddress, i, @Result[1])) do
      i := i div 2;
    SetLength(Result,i);
    i := pos(#0, Result);
    if i > 0 then
      SetLength(Result,i-1);
  end
  else
    Result := inherited GetAsString;
end;

function TFpValueDwarfPointer.GetAsWideString: WideString;
var
  t: TFpSymbol;
  i: Integer;
begin
  t := TypeInfo;
  if (t <> nil) then t := t.TypeInfo;
  // skWideChar ???
  if  (MemManager <> nil) and (t <> nil) and (t.Kind = skChar) and IsReadableMem(GetDerefAddress) then begin // pchar
    SetLength(Result, 2000);
    i := 4000; // 2000 * 16 bit
    while (i > 0) and (not MemManager.ReadMemory(GetDerefAddress, i, @Result[1])) do
      i := i div 2;
    SetLength(Result, i div 2);
    i := pos(#0, Result);
    if i > 0 then
      SetLength(Result, i-1);
  end
  else
    Result := inherited GetAsWideString;
end;

function TFpValueDwarfPointer.GetMember(AIndex: Int64): TFpValue;
var
  ti: TFpSymbol;
  addr: TFpDbgMemLocation;
  Tmp: TFpValueDwarfConstAddress;
begin
  //TODO: ?? if no TypeInfo.TypeInfo;, then return TFpValueDwarfConstAddress.Create(addr); (for mem dump)
  Result := nil;
  ReleaseRefAndNil(FLastAddrMember);
  if (TypeInfo = nil) then begin // TODO dedicanted error code
    FLastError := CreateError(fpErrAnyError, ['Can not dereference an untyped pointer']);
    exit;
  end;

  // TODO re-use last member

  ti := TypeInfo.TypeInfo;
  {$PUSH}{$R-}{$Q-} // TODO: check overflow
  if ti <> nil then
    AIndex := AIndex * ti.Size;
  addr := GetDerefAddress;
  if not IsTargetAddr(addr) then begin
    FLastError := CreateError(fpErrAnyError, ['Internal dereference error']);
    exit;
  end;
  addr.Address := addr.Address + AIndex;
  {$POP}

  Tmp := TFpValueDwarfConstAddress.Create(addr);
  if ti <> nil then begin
    Result := ti.TypeCastValue(Tmp);
    Tmp.ReleaseReference;
    SetLastMember(TFpValueDwarf(Result));
    Result.ReleaseReference;
  end
  else begin
    Result := Tmp;
    FLastAddrMember := Result;
  end;
end;

destructor TFpValueDwarfPointer.Destroy;
begin
  FLastAddrMember.ReleaseReference;
  inherited Destroy;
end;

{ TFpValueDwarfEnum }

procedure TFpValueDwarfEnum.InitMemberIndex;
var
  v: QWord;
  i: Integer;
begin
  // TODO: if TypeInfo is a subrange, check against the bounds, then bypass it, and scan all members (avoid subrange scanning members)
  if FMemberValueDone then exit;
  // FTypeCastTargetType (if not nil) must be same as FOwner. It may have wrappers like declaration.
  v := GetAsCardinal;
  i := FOwner.NestedSymbolCount - 1;
  while i >= 0 do begin
    if FOwner.NestedSymbol[i].OrdinalValue = v then break;
    dec(i);
  end;
  FMemberIndex := i;
  FMemberValueDone := True;
end;

procedure TFpValueDwarfEnum.Reset;
begin
  inherited Reset;
  FMemberValueDone := False;
end;

function TFpValueDwarfEnum.GetFieldFlags: TFpValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfOrdinal, svfMembers, svfIdentifier];
end;

function TFpValueDwarfEnum.GetAsCardinal: QWord;
begin
  if doneUInt in FEvaluated then begin
    Result := FValue;
    exit;
  end;
  Include(FEvaluated, doneUInt);

  if (FSize <= 0) or (FSize > SizeOf(Result)) then
    Result := inherited GetAsCardinal
  else
  if not MemManager.ReadEnum(OrdOrDataAddr, FSize, Result) then begin
    FLastError := MemManager.LastError;
    Result := 0; // TODO: error
  end;

  FValue := Result;
end;

function TFpValueDwarfEnum.GetAsString: AnsiString;
begin
  InitMemberIndex;
  if FMemberIndex >= 0 then
    Result := FOwner.NestedSymbol[FMemberIndex].Name
  else
    Result := '';
end;

function TFpValueDwarfEnum.GetMemberCount: Integer;
begin
  InitMemberIndex;
  if FMemberIndex < 0 then
    Result := 0
  else
    Result := 1;
end;

function TFpValueDwarfEnum.GetMember(AIndex: Int64): TFpValue;
begin
  InitMemberIndex;
  if (FMemberIndex >= 0) and (AIndex = 0) then
    Result := FOwner.NestedSymbol[FMemberIndex].Value
  else
    Result := nil;
end;

{ TFpValueDwarfEnumMember }

function TFpValueDwarfEnumMember.GetFieldFlags: TFpValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfOrdinal, svfIdentifier];
end;

function TFpValueDwarfEnumMember.GetAsCardinal: QWord;
begin
  Result := FOwnerVal.OrdinalValue;
end;

function TFpValueDwarfEnumMember.GetAsString: AnsiString;
begin
  Result := FOwnerVal.Name;
end;

function TFpValueDwarfEnumMember.IsValidTypeCast: Boolean;
begin
  assert(False, 'TDbgDwarfEnumMemberSymbolValue.IsValidTypeCast can not be returned for typecast');
  Result := False;
end;

constructor TFpValueDwarfEnumMember.Create(AOwner: TFpSymbolDwarfData);
begin
  FOwnerVal := AOwner;
  inherited Create(nil);
end;

{ TFpValueDwarfConstNumber }

procedure TFpValueDwarfConstNumber.Update(AValue: QWord; ASigned: Boolean);
begin
  Signed := ASigned;
  Value := AValue;
end;

{ TFpValueDwarfSet }

procedure TFpValueDwarfSet.InitMap;
const
  BitCount: array[0..15] of byte = (0, 1, 1, 2,  1, 2, 2, 3,  1, 2, 2, 3,  2, 3, 3, 4);
var
  i, i2, v, MemIdx, Bit, Cnt: Integer;

  t: TFpSymbol;
  hb, lb: Int64;
  DAddr: TFpDbgMemLocation;
begin
  if (length(FMem) > 0) or (FSize <= 0) then
    exit;
  t := TypeInfo;
  if t = nil then exit;
  t := t.TypeInfo;
  if t = nil then exit;

  GetDwarfDataAddress(DAddr);
  if not MemManager.ReadSet(DAddr, FSize, FMem) then begin
    FLastError := MemManager.LastError;
    exit; // TODO: error
  end;

  Cnt := 0;
  for i := 0 to FSize - 1 do
    Cnt := Cnt + (BitCount[FMem[i] and 15])  + (BitCount[(FMem[i] div 16) and 15]);
  FMemberCount := Cnt;

  if (Cnt = 0) then exit;
  SetLength(FMemberMap, Cnt);

  if (t.Kind = skEnum) then begin
    i2 := 0;
    for i := 0 to t.NestedSymbolCount - 1 do
    begin
      v := t.NestedSymbol[i].OrdinalValue;
      MemIdx := v shr 3;
      Bit := 1 shl (v and 7);
      if (FMem[MemIdx] and Bit) <> 0 then begin
        assert(i2 < Cnt, 'TDbgDwarfSetSymbolValue.InitMap too many members');
        if i2 = Cnt then break;
        FMemberMap[i2] := i;
        inc(i2);
      end;
    end;

    if i2 < Cnt then begin
      FMemberCount := i2;
      debugln(FPDBG_DWARF_DATA_WARNINGS, ['TDbgDwarfSetSymbolValue.InitMap  not enough members']);
    end;
  end
  else begin
    i2 := 0;
    MemIdx := 0;
    Bit := 1;
    t.GetValueBounds(nil, lb, hb);
    for i := lb to hb do
    begin
      if (FMem[MemIdx] and Bit) <> 0 then begin
        assert(i2 < Cnt, 'TDbgDwarfSetSymbolValue.InitMap too many members');
        if i2 = Cnt then break;
        FMemberMap[i2] := i - lb; // offset from low-bound
        inc(i2);
      end;
      if Bit = 128 then begin
        Bit := 1;
        inc(MemIdx);
      end
      else
        Bit := Bit shl 1;
    end;

    if i2 < Cnt then begin
      FMemberCount := i2;
      debugln(FPDBG_DWARF_DATA_WARNINGS, ['TDbgDwarfSetSymbolValue.InitMap  not enough members']);
    end;
  end;

end;

procedure TFpValueDwarfSet.Reset;
begin
  inherited Reset;
  SetLength(FMem, 0);
end;

function TFpValueDwarfSet.GetFieldFlags: TFpValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfMembers];
  if FSize <= 8 then
    Result := Result + [svfOrdinal];
end;

function TFpValueDwarfSet.GetMemberCount: Integer;
begin
  InitMap;
  Result := FMemberCount;
end;

function TFpValueDwarfSet.GetMember(AIndex: Int64): TFpValue;
var
  t: TFpSymbol;
  lb: Int64;
begin
  Result := nil;
  InitMap;
  t := TypeInfo;
  if t = nil then exit;
  t := t.TypeInfo;
  if t = nil then exit;
  assert(t is TFpSymbolDwarfType, 'TDbgDwarfSetSymbolValue.GetMember t');

  if t.Kind = skEnum then begin
    Result := t.NestedSymbol[FMemberMap[AIndex]].Value;
  end
  else begin
    if (FNumValue = nil) or (FNumValue.RefCount > 1) then begin // refcount 1 by FTypedNumValue
      t.GetValueLowBound(nil, lb);
      FNumValue := TFpValueDwarfConstNumber.Create(FMemberMap[AIndex] + lb, t.Kind = skInteger);
    end
    else
    begin
      t.GetValueLowBound(nil, lb);
      FNumValue.Update(FMemberMap[AIndex] + lb, t.Kind = skInteger);
      FNumValue.AddReference;
    end;

    if (FTypedNumValue = nil) or (FTypedNumValue.RefCount > 1) then begin
      FTypedNumValue.ReleaseReference;
      FTypedNumValue := t.TypeCastValue(FNumValue)
    end
    else
      TFpValueDwarf(FTypedNumValue).SetTypeCastInfo(TFpSymbolDwarfType(t), FNumValue); // update
    FNumValue.ReleaseReference;
    Assert((FTypedNumValue <> nil) and (TFpValueDwarf(FTypedNumValue).IsValidTypeCast), 'TDbgDwarfSetSymbolValue.GetMember FTypedNumValue');
    Assert((FNumValue <> nil) and (FNumValue.RefCount > 0), 'TDbgDwarfSetSymbolValue.GetMember FNumValue');
    Result := FTypedNumValue;
  end;
end;

function TFpValueDwarfSet.GetAsCardinal: QWord;
begin
  Result := 0;
  if (FSize <= SizeOf(Result)) and (length(FMem) > 0) then
    move(FMem[0], Result, FSize);
end;

function TFpValueDwarfSet.IsValidTypeCast: Boolean;
var
  f: TFpValueFieldFlags;
begin
  Result := HasTypeCastInfo;
  If not Result then
    exit;

  assert(FTypeCastTargetType.Kind = skSet, 'TFpValueDwarfSet.IsValidTypeCast: FTypeCastTargetType.Kind = skSet');

  if (FTypeCastSourceValue.TypeInfo = FTypeCastTargetType)
  then
    exit; // pointer deref

  f := FTypeCastSourceValue.FieldFlags;
  if (f * [svfAddress, svfSize, svfSizeOfPointer] = [svfAddress]) then
    exit;

  if (f * [svfAddress, svfSize] = [svfAddress, svfSize]) and
     (FTypeCastSourceValue.Size = FTypeCastTargetType.Size)
  then
    exit;

  Result := False;
end;

destructor TFpValueDwarfSet.Destroy;
begin
  FTypedNumValue.ReleaseReference;
  inherited Destroy;
end;

{ TFpValueDwarfStruct }

procedure TFpValueDwarfStruct.Reset;
begin
  inherited Reset;
  FDataAddressDone := False;
end;

function TFpValueDwarfStruct.GetFieldFlags: TFpValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfMembers];

  //TODO: svfDataAddress should depend on (hidden) Pointer or Ref in the TypeInfo
  if Kind in [skClass] then begin
    Result := Result + [svfOrdinal, svfDataAddress, svfDataSize]; // svfDataSize
    if (FDataSymbol <> nil) and FDataSymbol.HasAddress then
      Result := Result + [svfSizeOfPointer];
  end
  else begin
    Result := Result + [svfSize];
  end;
end;

function TFpValueDwarfStruct.GetAsCardinal: QWord;
var
  Addr: TFpDbgMemLocation;
begin
  if not GetDwarfDataAddress(Addr, Owner) then
    Result := 0
  else
  Result := QWord(LocToAddrOrNil(Addr));
end;

function TFpValueDwarfStruct.GetDataSize: Integer;
begin
  Assert((FDataSymbol = nil) or (FDataSymbol.TypeInfo is TFpSymbolDwarf));
  if (FDataSymbol <> nil) and (FDataSymbol.TypeInfo <> nil) then
    if FDataSymbol.TypeInfo.Kind = skClass then
      Result := TFpSymbolDwarf(FDataSymbol.TypeInfo).DataSize
    else
      Result := FDataSymbol.TypeInfo.Size
  else
    Result := -1;
end;

function TFpValueDwarfStruct.GetSize: Integer;
begin
  if (Kind <> skClass) and (FDataSymbol <> nil) and (FDataSymbol.TypeInfo <> nil) then
    Result := FDataSymbol.TypeInfo.Size
  else
    Result := -1;
end;

{ TFpValueDwarfStructTypeCast }

procedure TFpValueDwarfStructTypeCast.Reset;
begin
  inherited Reset;
  FDataAddressDone := False;
end;

function TFpValueDwarfStructTypeCast.GetFieldFlags: TFpValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfMembers];
  if kind = skClass then // todo detect hidden pointer
    Result := Result + [svfDataSize]
  else
    Result := Result + [svfSize];

  //TODO: svfDataAddress should depend on (hidden) Pointer or Ref in the TypeInfo
  if Kind in [skClass] then
    Result := Result + [svfOrdinal, svfDataAddress, svfSizeOfPointer]; // svfDataSize
end;

function TFpValueDwarfStructTypeCast.GetKind: TDbgSymbolKind;
begin
  if HasTypeCastInfo then
    Result := FTypeCastTargetType.Kind
  else
    Result := inherited GetKind;
end;

function TFpValueDwarfStructTypeCast.GetAsCardinal: QWord;
var
  Addr: TFpDbgMemLocation;
begin
  if not GetDwarfDataAddress(Addr, Owner) then
    Result := 0
  else
    Result := QWord(LocToAddrOrNil(Addr));
end;

function TFpValueDwarfStructTypeCast.GetSize: Integer;
begin
  if (Kind <> skClass) and (FTypeCastTargetType <> nil) then
    Result := FTypeCastTargetType.Size
  else
    Result := -1;
end;

function TFpValueDwarfStructTypeCast.GetDataSize: Integer;
begin
  Assert((FTypeCastTargetType = nil) or (FTypeCastTargetType is TFpSymbolDwarf));
  if FTypeCastTargetType <> nil then
    if FTypeCastTargetType.Kind = skClass then
      Result := TFpSymbolDwarf(FTypeCastTargetType).DataSize
    else
      Result := FTypeCastTargetType.Size
  else
    Result := -1;
end;

function TFpValueDwarfStructTypeCast.IsValidTypeCast: Boolean;
var
  f: TFpValueFieldFlags;
begin
  Result := HasTypeCastInfo;
  if not Result then
    exit;

  if FTypeCastTargetType.Kind = skClass then begin
    f := FTypeCastSourceValue.FieldFlags;
    Result := (svfOrdinal in f); // ordinal is prefered in GetDataAddress
    if Result then
      exit;
    Result := (svfAddress in f) and
              ( ( not(svfSize in f) ) or // either svfSizeOfPointer or a void type, e.g. pointer(1)^
                ( (svfSize in f) and (FTypeCastSourceValue.Size = AddressSize) )
              );
  end
  else begin
    f := FTypeCastSourceValue.FieldFlags;
    if (f * [{svfOrdinal, }svfAddress] = [svfAddress]) then begin
      if (f * [svfSize, svfSizeOfPointer]) = [svfSize] then
        Result := Result and (FTypeCastTargetType.Size = FTypeCastSourceValue.Size)
      else
      if (f * [svfSize, svfSizeOfPointer]) = [svfSizeOfPointer] then
        Result := Result and (FTypeCastTargetType.Size = AddressSize)
      else
        Result := (f * [svfSize, svfSizeOfPointer]) = []; // source is a void type, e.g. pointer(1)^
    end
    else
      Result := False;
  end;
end;

destructor TFpValueDwarfStructTypeCast.Destroy;
begin
  FreeAndNil(FMembers);
  inherited Destroy;
end;

function TFpValueDwarfStructTypeCast.GetMemberByName(AIndex: String): TFpValue;
var
  tmp: TFpSymbol;
begin
  Result := nil;
  if not HasTypeCastInfo then
    exit;

  tmp := FTypeCastTargetType.NestedSymbolByName[AIndex];
  if (tmp <> nil) then begin
    assert((tmp is TFpSymbolDwarfData), 'TDbgDwarfStructTypeCastSymbolValue.GetMemberByName'+DbgSName(tmp));
    if FMembers = nil then
      FMembers := TFpDbgCircularRefCntObjList.Create;
    FMembers.Add(tmp);

    Result := tmp.Value;
  end;
  SetLastMember(TFpValueDwarf(Result));
end;

function TFpValueDwarfStructTypeCast.GetMember(AIndex: Int64): TFpValue;
var
  tmp: TFpSymbol;
begin
  Result := nil;
  if not HasTypeCastInfo then
    exit;

  // TODO: Why store them all in list? They are hold by the type
  tmp := FTypeCastTargetType.NestedSymbol[AIndex];
  if (tmp <> nil) then begin
    assert((tmp is TFpSymbolDwarfData), 'TDbgDwarfStructTypeCastSymbolValue.GetMemberByName'+DbgSName(tmp));
    if FMembers = nil then
      FMembers := TFpDbgCircularRefCntObjList.Create;
    FMembers.Add(tmp);

    Result := tmp.Value;
  end;
  SetLastMember(TFpValueDwarf(Result));
end;

function TFpValueDwarfStructTypeCast.GetMemberCount: Integer;
begin
  Result := 0;
  if not HasTypeCastInfo then
    exit;

  Result := FTypeCastTargetType.NestedSymbolCount;
end;

{ TFpValueDwarfConstAddress }

procedure TFpValueDwarfConstAddress.Update(AnAddress: TFpDbgMemLocation);
begin
  Address := AnAddress;
end;

{ TFpValueDwarfArray }

function TFpValueDwarfArray.GetFieldFlags: TFpValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfMembers];
  if (TypeInfo <> nil) and (sfDynArray in TypeInfo.Flags) then
    Result := Result + [svfOrdinal, svfDataAddress];
end;

function TFpValueDwarfArray.GetKind: TDbgSymbolKind;
begin
  Result := skArray;
end;

function TFpValueDwarfArray.GetAsCardinal: QWord;
begin
  // TODO cache
  if not MemManager.ReadUnsignedInt(OrdOrAddress, AddressSize, Result) then begin
    FLastError := MemManager.LastError;
    Result := 0;
  end;
end;

function TFpValueDwarfArray.GetMember(AIndex: Int64): TFpValue;
begin
  Result := GetMemberEx([AIndex]);
end;

function TFpValueDwarfArray.GetMemberEx(const AIndex: array of Int64
  ): TFpValue;
var
  Addr: TFpDbgMemLocation;
  i: Integer;
begin
  Result := nil;
  assert((FOwner is TFpSymbolDwarfTypeArray) and (FOwner.Kind = skArray));

  Addr := TFpSymbolDwarfTypeArray(FOwner).GetMemberAddress(Self, AIndex);
  if not IsReadableLoc(Addr) then exit;

  // FAddrObj.RefCount: hold by self
  i := 1;
  // FAddrObj.RefCount: hold by FLastMember (ignore only, if FLastMember is not hold by others)
  if (FLastMember <> nil) and (FLastMember.RefCount = 1) then
    i := 2;
  if (FAddrObj = nil) or (FAddrObj.RefCount > i) then begin
    FAddrObj.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FAddrObj, 'TDbgDwarfArraySymbolValue'){$ENDIF};
    FAddrObj := TFpValueDwarfConstAddress.Create(Addr);
    {$IFDEF WITH_REFCOUNT_DEBUG}FAddrObj.DbgRenameReference(@FAddrObj, 'TDbgDwarfArraySymbolValue');{$ENDIF}
  end
  else begin
    FAddrObj.Update(Addr);
  end;

  if (FLastMember = nil) or (FLastMember.RefCount > 1) then begin
    SetLastMember(TFpValueDwarf(FOwner.TypeInfo.TypeCastValue(FAddrObj)));
    FLastMember.ReleaseReference;
  end
  else begin
    TFpValueDwarf(FLastMember).SetTypeCastInfo(TFpSymbolDwarfType(FOwner.TypeInfo), FAddrObj);
  end;

  Result := FLastMember;
end;

function TFpValueDwarfArray.GetMemberCount: Integer;
var
  t, t2: TFpSymbol;
  LowBound, HighBound: int64;
begin
  Result := 0;
  t := TypeInfo;
  if t.NestedSymbolCount < 1 then // IndexTypeCount;
    exit;
  t2 := t.NestedSymbol[0]; // IndexType[0];
  if t2.GetValueBounds(self, LowBound, HighBound) then begin
    if (HighBound < LowBound) then
      exit(0); // empty array // TODO: error
    // TODO: XXXXX Dynamic max limit
    {$PUSH}{$Q-}
    if QWord(HighBound - LowBound) > 3000 then
      HighBound := LowBound + 3000;
    Result := Integer(HighBound - LowBound + 1);
    {$POP}
  end;
end;

function TFpValueDwarfArray.GetMemberCountEx(const AIndex: array of Int64
  ): Integer;
var
  t: TFpSymbol;
  lb, hb: Int64;
begin
  Result := 0;
  t := TypeInfo;
  if length(AIndex) >= t.NestedSymbolCount then
    exit;
  t := t.NestedSymbol[length(AIndex)];
  if not t.GetValueBounds(nil, lb, hb) then
    exit;
  Result := hb - lb + 1;
end;

function TFpValueDwarfArray.GetIndexType(AIndex: Integer): TFpSymbol;
begin
  Result := TypeInfo.NestedSymbol[AIndex];
end;

function TFpValueDwarfArray.GetIndexTypeCount: Integer;
begin
  Result := TypeInfo.NestedSymbolCount;
end;

function TFpValueDwarfArray.IsValidTypeCast: Boolean;
var
  f: TFpValueFieldFlags;
begin
  Result := HasTypeCastInfo;
  If not Result then
    exit;

  assert(FTypeCastTargetType.Kind = skArray, 'TFpValueDwarfArray.IsValidTypeCast: FTypeCastTargetType.Kind = skArray');
//TODO: shortcut, if FTypeCastTargetType = FTypeCastSourceValue.TypeInfo ?

  f := FTypeCastSourceValue.FieldFlags;
  if (f * [svfAddress, svfSize, svfSizeOfPointer] = [svfAddress]) then
    exit;

  if sfDynArray in FTypeCastTargetType.Flags then begin
    // dyn array
    if (svfOrdinal in f)then
      exit;
    if (f * [svfAddress, svfSize] = [svfAddress, svfSize]) and
       (FTypeCastSourceValue.Size = FOwner.CompilationUnit.AddressSize)
    then
      exit;
    if (f * [svfAddress, svfSizeOfPointer] = [svfAddress, svfSizeOfPointer]) then
      exit;
  end
  else begin
    // stat array
    if (f * [svfAddress, svfSize] = [svfAddress, svfSize]) and
       (FTypeCastSourceValue.Size = FTypeCastTargetType.Size)
    then
      exit;
  end;
  Result := False;
end;

destructor TFpValueDwarfArray.Destroy;
begin
  FAddrObj.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FAddrObj, 'TDbgDwarfArraySymbolValue'){$ENDIF};
  inherited Destroy;
end;

{ TDbgDwarfIdentifier }

function TFpSymbolDwarf.GetNestedTypeInfo: TFpSymbolDwarfType;
begin
// TODO DW_AT_start_scope;
  Result := FNestedTypeInfo;
  if (Result <> nil) or (didtTypeRead in FDwarfReadFlags) then
    exit;

  include(FDwarfReadFlags, didtTypeRead);
  FNestedTypeInfo := DoGetNestedTypeInfo;
  Result := FNestedTypeInfo;
end;

procedure TFpSymbolDwarf.SetParentTypeInfo(AValue: TFpSymbolDwarf);
begin
  if FParentTypeInfo = AValue then exit;

  if (FParentTypeInfo <> nil) and CircleBackRefsActive then
    FParentTypeInfo.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FParentTypeInfo, 'FParentTypeInfo'){$ENDIF};

  FParentTypeInfo := AValue;

  if (FParentTypeInfo <> nil) and CircleBackRefsActive then
    FParentTypeInfo.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FParentTypeInfo, 'FParentTypeInfo'){$ENDIF};
end;

procedure TFpSymbolDwarf.DoReferenceAdded;
begin
  inherited DoReferenceAdded;
  DoPlainReferenceAdded;
end;

procedure TFpSymbolDwarf.DoReferenceReleased;
begin
  inherited DoReferenceReleased;
  DoPlainReferenceReleased;
end;

procedure TFpSymbolDwarf.CircleBackRefActiveChanged(ANewActive: Boolean);
begin
  if (FParentTypeInfo = nil) then
    exit;
  if ANewActive then
    FParentTypeInfo.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FParentTypeInfo, 'FParentTypeInfo'){$ENDIF}
  else
    FParentTypeInfo.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FParentTypeInfo, 'FParentTypeInfo'){$ENDIF};
end;

function TFpSymbolDwarf.DoGetNestedTypeInfo: TFpSymbolDwarfType;
var
  FwdInfoPtr: Pointer;
  FwdCompUint: TDwarfCompilationUnit;
  InfoEntry: TDwarfInformationEntry;
begin // Do not access anything that may need forwardSymbol
  if InformationEntry.ReadReference(DW_AT_type, FwdInfoPtr, FwdCompUint) then begin
    InfoEntry := TDwarfInformationEntry.Create(FwdCompUint, FwdInfoPtr);
    Result := TFpSymbolDwarfType.CreateTypeSubClass('', InfoEntry);
    ReleaseRefAndNil(InfoEntry);
  end
  else
    Result := nil;
end;

function TFpSymbolDwarf.ReadMemberVisibility(out
  AMemberVisibility: TDbgSymbolMemberVisibility): Boolean;
var
  Val: Integer;
begin
  Result := InformationEntry.ReadValue(DW_AT_external, Val);
  if Result and (Val <> 0) then begin
    AMemberVisibility := svPublic;
    exit;
  end;

  Result := InformationEntry.ReadValue(DW_AT_accessibility, Val);
  if not Result then exit;
  case Val of
    DW_ACCESS_private:   AMemberVisibility := svPrivate;
    DW_ACCESS_protected: AMemberVisibility := svProtected;
    DW_ACCESS_public:    AMemberVisibility := svPublic;
    else                 AMemberVisibility := svPrivate;
  end;
end;

function TFpSymbolDwarf.IsArtificial: Boolean;
begin
  if not(didtArtificialRead in FDwarfReadFlags) then begin
    if InformationEntry.IsArtificial then
      Include(FDwarfReadFlags, didtIsArtifical);
    Include(FDwarfReadFlags, didtArtificialRead);
  end;
  Result := didtIsArtifical in FDwarfReadFlags;
end;

procedure TFpSymbolDwarf.NameNeeded;
var
  AName: String;
begin
  if InformationEntry.ReadName(AName) then
    SetName(AName)
  else
    inherited NameNeeded;
end;

procedure TFpSymbolDwarf.TypeInfoNeeded;
begin
  SetTypeInfo(NestedTypeInfo);
end;

function TFpSymbolDwarf.DataSize: Integer;
var
  t: TFpSymbolDwarfType;
begin
  t := NestedTypeInfo;
  if t <> nil then
    Result := t.DataSize
  else
    Result := 0;
end;

function TFpSymbolDwarf.InitLocationParser(const ALocationParser: TDwarfLocationExpression;
  AnInitLocParserData: PInitLocParserData): Boolean;
var
  ObjDataAddr: TFpDbgMemLocation;
begin
  if (AnInitLocParserData <> nil) then begin
    ObjDataAddr := AnInitLocParserData^.ObjectDataAddress;
    if IsValidLoc(ObjDataAddr) then begin
      if ObjDataAddr.MType = mlfConstant then begin
        DebugLn(DBG_WARNINGS, 'Changing mlfConstant to mlfConstantDeref'); // TODO: Should be done by caller
        ObjDataAddr.MType := mlfConstantDeref;
      end;

      debugln(FPDBG_DWARF_VERBOSE, ['TFpSymbolDwarf.InitLocationParser CurrentObjectAddress=', dbgs(ObjDataAddr), ' Push=',AnInitLocParserData^.ObjectDataAddrPush]);
      ALocationParser.CurrentObjectAddress := ObjDataAddr;
      if AnInitLocParserData^.ObjectDataAddrPush then
        ALocationParser.Push(ObjDataAddr);
    end
    else
      ALocationParser.CurrentObjectAddress := InvalidLoc
  end
  else
    ALocationParser.CurrentObjectAddress := InvalidLoc;

  Result := True;
end;

function TFpSymbolDwarf.ComputeDataMemberAddress(
  const AnInformationEntry: TDwarfInformationEntry; AValueObj: TFpValueDwarf;
  var AnAddress: TFpDbgMemLocation): Boolean;
var
  AttrData: TDwarfAttribData;
  Form: Cardinal;
  ConstOffs: Int64;
  InitLocParserData: TInitLocParserData;
begin
  Result := True;
  if AnInformationEntry.GetAttribData(DW_AT_data_member_location, AttrData) then begin
    Form := AnInformationEntry.AttribForm[AttrData.Idx];
    Result := False;

    if Form in [DW_FORM_data1, DW_FORM_data2, DW_FORM_sdata, DW_FORM_udata] then begin
      if AnInformationEntry.ReadValue(AttrData, ConstOffs) then begin
        {$PUSH}{$R-}{$Q-} // TODO: check overflow
        AnAddress.Address := AnAddress.Address + ConstOffs;
        {$POP}
         Result := True;
      end
      else
        SetLastError(CreateError(fpErrAnyError));
    end

    // TODO: loclistptr: DW_FORM_data4, DW_FORM_data8,
    else

    if Form in [DW_FORM_block, DW_FORM_block1, DW_FORM_block2, DW_FORM_block4] then begin
      InitLocParserData.ObjectDataAddress := AnAddress;
      InitLocParserData.ObjectDataAddrPush := True;
      Result := LocationFromAttrData(AttrData, AValueObj, AnAddress, @InitLocParserData);
    end

    else begin
      SetLastError(CreateError(fpErrAnyError));
    end;
  end;
end;

function TFpSymbolDwarf.ConstRefOrExprFromAttrData(
  const AnAttribData: TDwarfAttribData; AValueObj: TFpValueDwarf; out
  AValue: TDBGPtr; AReadState: PFpDwarfAtEntryDataReadState;
  ADataSymbol: PFpSymbolDwarfData): Boolean;
var
  Form: Cardinal;
  FwdInfoPtr: Pointer;
  FwdCompUint: TDwarfCompilationUnit;
  NewInfo: TDwarfInformationEntry;
  RefSymbol: TFpSymbolDwarfData;
  InitLocParserData: TInitLocParserData;
  t: TFpDbgMemLocation;
begin
  Form := InformationEntry.AttribForm[AnAttribData.Idx];
  Result := False;

  if Form in [DW_FORM_data1, DW_FORM_data2, DW_FORM_data4, DW_FORM_data8,
              DW_FORM_sdata, DW_FORM_udata]
  then begin
    if AReadState <> nil then
      AReadState^ := rfConst;

    Result := InformationEntry.ReadValue(AnAttribData, AValue);
    if not Result then
      SetLastError(CreateError(fpErrAnyError));
  end

  else
  if Form in [DW_FORM_ref1, DW_FORM_ref2, DW_FORM_ref4, DW_FORM_ref8,
              DW_FORM_ref_addr, DW_FORM_ref_udata]
  then begin
    if AReadState <> nil then
      AReadState^ := rfValue;

    Result := InformationEntry.ReadReference(AnAttribData, FwdInfoPtr, FwdCompUint);
    if Result then begin
      NewInfo := TDwarfInformationEntry.Create(FwdCompUint, FwdInfoPtr);
      RefSymbol := TFpSymbolDwarfData.CreateValueSubClass('', NewInfo);
      NewInfo.ReleaseReference;
      Result := RefSymbol <> nil;
      if Result then begin
        AValue := RefSymbol.Value.AsInteger;
        Result := not IsError(RefSymbol.LastError);
        // TODO: copy the error
        if ADataSymbol <> nil then
          ADataSymbol^ := RefSymbol
        else
          RefSymbol.ReleaseReference;
      end;
    end;
    if not Result then
      SetLastError(CreateError(fpErrAnyError));
  end

  else
  if Form in [DW_FORM_block, DW_FORM_block1, DW_FORM_block2, DW_FORM_block4]
  then begin
    // TODO: until there always will be an AValueObj
    if AValueObj = nil then begin
      if AReadState <> nil then
        AReadState^ := rfNotRead;
        exit(true); // claim success
    end;

    if AReadState <> nil then
      AReadState^ := rfExpression;

    InitLocParserData.ObjectDataAddress := AValueObj.Address;
    if not IsValidLoc(InitLocParserData.ObjectDataAddress) then
      InitLocParserData.ObjectDataAddress := AValueObj.OrdOrAddress;
    InitLocParserData.ObjectDataAddrPush := False;
    Result := LocationFromAttrData(AnAttribData, AValueObj, t, @InitLocParserData);
    if Result then
      AValue := t.Address
    else
      SetLastError(CreateError(fpErrLocationParser));
  end

  else begin
    SetLastError(CreateError(fpErrAnyError));
  end;

  if (not Result) and (AReadState <> nil) then
    AReadState^ := rfError;
end;

function TFpSymbolDwarf.LocationFromAttrData(
  const AnAttribData: TDwarfAttribData; AValueObj: TFpValueDwarf;
  var AnAddress: TFpDbgMemLocation; AnInitLocParserData: PInitLocParserData;
  AnAdjustAddress: Boolean): Boolean;
var
  Val: TByteDynArray;
  LocationParser: TDwarfLocationExpression;
begin
  //debugln(['TDbgDwarfIdentifier.LocationFromAttrData', ClassName, '  ',Name, '  ', DwarfAttributeToString(ATag)]);

  Result := False;
  AnAddress := InvalidLoc;

  //TODO: avoid copying data
  // DW_AT_data_member_location in members [ block or const]
  // DW_AT_location [block or reference] todo: const
  if not InformationEntry.ReadValue(AnAttribData, Val) then begin
    DebugLn(['LocationFromAttrData: failed to read DW_AT_location']);
    exit;
  end;

  if Length(Val) = 0 then begin
    DebugLn('LocationFromAttrData: Warning DW_AT_location empty');
    //exit;
  end;

  LocationParser := TDwarfLocationExpression.Create(@Val[0], Length(Val), CompilationUnit,
    AValueObj.MemManager, AValueObj.Context);
  InitLocationParser(LocationParser, AnInitLocParserData);
  LocationParser.Evaluate;

  if IsError(LocationParser.LastError) then
    SetLastError(LocationParser.LastError);

  AnAddress := LocationParser.ResultData;
  Result := IsValidLoc(AnAddress);
  if IsTargetAddr(AnAddress) and  AnAdjustAddress then
    AnAddress.Address :=CompilationUnit.MapAddressToNewValue(AnAddress.Address);
  debugln(not Result, ['TDbgDwarfIdentifier.LocationFromAttrDataFAILED']); // TODO

  LocationParser.Free;
end;

function TFpSymbolDwarf.LocationFromTag(ATag: Cardinal;
  AValueObj: TFpValueDwarf; var AnAddress: TFpDbgMemLocation;
  AnInitLocParserData: PInitLocParserData; ASucessOnMissingTag: Boolean
  ): Boolean;
var
  AttrData: TDwarfAttribData;
begin
  //debugln(['TDbgDwarfIdentifier.LocationFromTag', ClassName, '  ',Name, '  ', DwarfAttributeToString(ATag)]);

  Result := False;
  //TODO: avoid copying data
  // DW_AT_data_member_location in members [ block or const]
  // DW_AT_location [block or reference] todo: const
  if not InformationEntry.GetAttribData(ATag, AttrData) then begin
    (* if ASucessOnMissingTag = true AND tag does not exist
       then AnAddress will NOT be modified
       this can be used for DW_AT_data_member_location, if it does not exist members are on input location
       TODO: review - better use temp var in caller
    *)
    Result := ASucessOnMissingTag;
    if not Result then
      AnAddress := InvalidLoc;
    if not Result then
      DebugLn(['LocationFromTag: failed to read DW_AT_..._location / ASucessOnMissingTag=', dbgs(ASucessOnMissingTag)]);
    exit;
  end;

  Result := LocationFromAttrData(AttrData, AValueObj, AnAddress, AnInitLocParserData, ATag = DW_AT_location);
end;

function TFpSymbolDwarf.ConstantFromTag(ATag: Cardinal; out
  AConstData: TByteDynArray; var AnAddress: TFpDbgMemLocation;
  AnInformationEntry: TDwarfInformationEntry; ASucessOnMissingTag: Boolean
  ): Boolean;
var
  v: QWord;
  AttrData: TDwarfAttribData;
begin
  AConstData := nil;
  if InformationEntry.GetAttribData(DW_AT_const_value, AttrData) then
    case InformationEntry.AttribForm[AttrData.Idx] of
      DW_FORM_string, DW_FORM_strp,
      DW_FORM_block, DW_FORM_block1, DW_FORM_block2, DW_FORM_block4: begin
        Result := InformationEntry.ReadValue(AttrData, AConstData, True);
        if Result then
          if Length(AConstData) > 0 then
            AnAddress := SelfLoc(@AConstData[0])
          else
            AnAddress := InvalidLoc; // TODO: ???
      end;
      DW_FORM_data1, DW_FORM_data2, DW_FORM_data4, DW_FORM_data8, DW_FORM_sdata, DW_FORM_udata: begin
        Result := InformationEntry.ReadValue(AttrData, v);
        if Result then
          AnAddress := ConstLoc(v);
      end;
      else
        Result := False; // ASucessOnMissingTag ?
    end
  else
    Result := ASucessOnMissingTag;
end;

function TFpSymbolDwarf.GetDataAddress(AValueObj: TFpValueDwarf;
  var AnAddress: TFpDbgMemLocation; ATargetType: TFpSymbolDwarfType): Boolean;
var
  ti: TFpSymbolDwarfType;
  AttrData: TDwarfAttribData;
  t: TDBGPtr;
  dummy: Boolean;
begin
Assert(self is TFpSymbolDwarfType);
  Result := False;
  if InformationEntry.GetAttribData(DW_AT_allocated, AttrData) then begin
    if not ConstRefOrExprFromAttrData(AttrData, AValueObj, t) then
      exit;
    if t = 0 then begin
      AnAddress := NilLoc;
      exit(True);
    end;
  end;

  if InformationEntry.GetAttribData(DW_AT_associated, AttrData) then begin
    if not ConstRefOrExprFromAttrData(AttrData, AValueObj, t) then
      exit;
    if t = 0 then begin
      AnAddress := NilLoc;
      exit(True);
    end;
  end;

  Result := GetDataAddressNext(AValueObj, AnAddress, dummy, ATargetType);
  if not Result then
    exit;

  ti := GetNextTypeInfoForDataAddress(ATargetType);
  if ti = nil then
    exit;

  Result := ti.GetDataAddress(AValueObj, AnAddress, ATargetType);
end;

function TFpSymbolDwarf.GetNextTypeInfoForDataAddress(
  ATargetType: TFpSymbolDwarfType): TFpSymbolDwarfType;
begin
  if (ATargetType = nil) or (ATargetType = self) then
    Result := nil
  else
    Result := NestedTypeInfo;
end;

function TFpSymbolDwarf.GetDataAddressNext(AValueObj: TFpValueDwarf;
  var AnAddress: TFpDbgMemLocation; out ADoneWork: Boolean;
  ATargetType: TFpSymbolDwarfType): Boolean;
var
  AttrData: TDwarfAttribData;
  InitLocParserData: TInitLocParserData;
begin
  Result := True;
  ADoneWork := False;

  if InformationEntry.GetAttribData(DW_AT_data_location, AttrData) then begin
    ADoneWork := True;
    InitLocParserData.ObjectDataAddress := AnAddress;
    InitLocParserData.ObjectDataAddrPush := False;
    Result := LocationFromAttrData(AttrData, AValueObj, AnAddress, @InitLocParserData);
  end;
end;

function TFpSymbolDwarf.HasAddress: Boolean;
begin
  Result := False;
end;

procedure TFpSymbolDwarf.Init;
begin
  //
end;

class function TFpSymbolDwarf.CreateSubClass(AName: String;
  AnInformationEntry: TDwarfInformationEntry): TFpSymbolDwarf;
var
  c: TDbgDwarfSymbolBaseClass;
begin
  c := AnInformationEntry.CompUnit.DwarfSymbolClassMap.GetDwarfSymbolClass(AnInformationEntry.AbbrevTag);
  Result := TFpSymbolDwarf(c.Create(AName, AnInformationEntry));
end;

destructor TFpSymbolDwarf.Destroy;
begin
  inherited Destroy;
  ReleaseRefAndNil(FNestedTypeInfo);
  Assert(not CircleBackRefsActive, 'CircleBackRefsActive can not be is destructor');
  // FParentTypeInfo := nil
end;

function TFpSymbolDwarf.StartScope: TDbgPtr;
begin
  if not InformationEntry.ReadStartScope(Result) then
    Result := 0;
end;

{ TFpSymbolDwarfData }

function TFpSymbolDwarfData.GetValueAddress(AValueObj: TFpValueDwarf; out
  AnAddress: TFpDbgMemLocation): Boolean;
begin
  Result := False;
end;

procedure TFpSymbolDwarfData.KindNeeded;
var
  t: TFpSymbol;
begin
  t := TypeInfo;
  if t = nil then
    inherited KindNeeded
  else
    SetKind(t.Kind);
end;

procedure TFpSymbolDwarfData.MemberVisibilityNeeded;
var
  Val: TDbgSymbolMemberVisibility;
begin
  if ReadMemberVisibility(Val) then
    SetMemberVisibility(Val)
  else
  if TypeInfo <> nil then
    SetMemberVisibility(TypeInfo.MemberVisibility)
  else
    inherited MemberVisibilityNeeded;
end;

function TFpSymbolDwarfData.GetNestedSymbol(AIndex: Int64): TFpSymbol;
var
  ti: TFpSymbol;
begin
  ti := TypeInfo;
  if ti = nil then begin
    Result := inherited GetNestedSymbol(AIndex);
    exit;
  end;

  // while holding result, until refcount added, do not call any function
  Result := ti.NestedSymbol[AIndex];
  assert((Result = nil) or (Result is TFpSymbolDwarfData), 'TFpSymbolDwarfData.GetMember is Value');
end;

function TFpSymbolDwarfData.GetNestedSymbolByName(AIndex: String): TFpSymbol;
var
  ti: TFpSymbol;
begin
  ti := TypeInfo;
  if ti = nil then begin
    Result := inherited GetNestedSymbolByName(AIndex);
    exit;
  end;

  // while holding result, until refcount added, do not call any function
  Result := ti.NestedSymbolByName[AIndex];
  assert((Result = nil) or (Result is TFpSymbolDwarfData), 'TFpSymbolDwarfData.GetMember is Value');
end;

function TFpSymbolDwarfData.GetNestedSymbolCount: Integer;
var
  ti: TFpSymbol;
begin
  ti := TypeInfo;
  if ti <> nil then
    Result := ti.NestedSymbolCount
  else
    Result := inherited GetNestedSymbolCount;
end;

procedure TFpSymbolDwarfData.Init;
begin
  inherited Init;
  SetSymbolType(stValue);
end;

destructor TFpSymbolDwarfData.Destroy;
begin
  Assert(not CircleBackRefsActive, 'CircleBackRefsActive can not be is ddestructor');

  if FValueObject <> nil then begin
    FValueObject.SetValueSymbol(nil);
    FValueObject.ReleaseCirclularReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValueObject, ClassName+'.FValueObject'){$ENDIF};
    FValueObject := nil;
  end;
  ParentTypeInfo := nil;
  inherited Destroy;
end;

class function TFpSymbolDwarfData.CreateValueSubClass(AName: String;
  AnInformationEntry: TDwarfInformationEntry): TFpSymbolDwarfData;
var
  c: TDbgDwarfSymbolBaseClass;
begin
  c := AnInformationEntry.CompUnit.DwarfSymbolClassMap.GetDwarfSymbolClass(AnInformationEntry.AbbrevTag);

  if c.InheritsFrom(TFpSymbolDwarfData) then
    Result := TFpSymbolDwarfDataClass(c).Create(AName, AnInformationEntry)
  else
    Result := nil;
end;

{ TFpSymbolDwarfDataWithLocation }

function TFpSymbolDwarfDataWithLocation.InitLocationParser(const ALocationParser: TDwarfLocationExpression;
  AnInitLocParserData: PInitLocParserData): Boolean;
begin
  Result := inherited InitLocationParser(ALocationParser, AnInitLocParserData);
  ALocationParser.OnFrameBaseNeeded := @FrameBaseNeeded;
end;

procedure TFpSymbolDwarfDataWithLocation.FrameBaseNeeded(ASender: TObject);
var
  p: TFpSymbolDwarf;
  fb: TDBGPtr;
begin
  debugln(FPDBG_DWARF_SEARCH, ['TFpSymbolDwarfDataVariable.FrameBaseNeeded ']);
  p := ParentTypeInfo;
  // TODO: what if parent is declaration?
  if (p <> nil) and (p is TFpSymbolDwarfDataProc) then begin
    fb := TFpSymbolDwarfDataProc(p).GetFrameBase(ASender as TDwarfLocationExpression);
    (ASender as TDwarfLocationExpression).FrameBase := fb;
    if fb = 0 then begin
      debugln(FPDBG_DWARF_ERRORS, ['DWARF ERROR in TFpSymbolDwarfDataWithLocation.FrameBaseNeeded result is 0']);
    end;
    exit;
  end;

{$warning TODO}
  //else
  //if OwnerTypeInfo <> nil then
  //  OwnerTypeInfo.fr;
  // TODO: check owner
  debugln(FPDBG_DWARF_ERRORS, ['DWARF ERROR in TFpSymbolDwarfDataWithLocation.FrameBaseNeeded no parent type info']);
  (ASender as TDwarfLocationExpression).FrameBase := 0;
end;

function TFpSymbolDwarfDataWithLocation.GetValueObject: TFpValue;
var
  ti: TFpSymbol;
begin
  Result := FValueObject;
  if Result <> nil then exit;

  ti := TypeInfo;
  if (ti = nil) or not (ti.SymbolType = stType) then exit;

  FValueObject := TFpSymbolDwarfType(ti).GetTypedValueObject(False);
  if FValueObject <> nil then begin
    {$IFDEF WITH_REFCOUNT_DEBUG}FValueObject.DbgRenameReference(@FValueObject, ClassName+'.FValueObject');{$ENDIF}
    FValueObject.MakePlainRefToCirclular;
    FValueObject.SetValueSymbol(self);
  end;

  Result := FValueObject;
end;

{ TFpSymbolDwarfType }

procedure TFpSymbolDwarfType.Init;
begin
  inherited Init;
  SetSymbolType(stType);
end;

procedure TFpSymbolDwarfType.MemberVisibilityNeeded;
var
  Val: TDbgSymbolMemberVisibility;
begin
  if ReadMemberVisibility(Val) then
    SetMemberVisibility(Val)
  else
    inherited MemberVisibilityNeeded;
end;

procedure TFpSymbolDwarfType.SizeNeeded;
var
  ByteSize: Integer;
begin
  if InformationEntry.ReadValue(DW_AT_byte_size, ByteSize) then
    SetSize(ByteSize)
  else
    inherited SizeNeeded;
end;

function TFpSymbolDwarfType.GetTypedValueObject(ATypeCast: Boolean): TFpValueDwarf;
begin
  Result := TFpValueDwarfUnknown.Create(Self);
end;

procedure TFpSymbolDwarfType.ResetValueBounds;
var
  ti: TFpSymbolDwarfType;
begin
  ti := NestedTypeInfo;
  if (ti <> nil) then
    ti.ResetValueBounds;
end;

class function TFpSymbolDwarfType.CreateTypeSubClass(AName: String;
  AnInformationEntry: TDwarfInformationEntry): TFpSymbolDwarfType;
var
  c: TDbgDwarfSymbolBaseClass;
begin
  c := AnInformationEntry.CompUnit.DwarfSymbolClassMap.GetDwarfSymbolClass(AnInformationEntry.AbbrevTag);

  if c.InheritsFrom(TFpSymbolDwarfType) then
    Result := TFpSymbolDwarfTypeClass(c).Create(AName, AnInformationEntry)
  else
    Result := nil;
end;

function TFpSymbolDwarfType.TypeCastValue(AValue: TFpValue): TFpValue;
begin
  Result := GetTypedValueObject(True);
  If Result = nil then
    exit;
  assert(Result is TFpValueDwarf);
  if not TFpValueDwarf(Result).SetTypeCastInfo(self, AValue) then
    ReleaseRefAndNil(Result);
end;

{ TDbgDwarfBaseTypeIdentifier }

procedure TFpSymbolDwarfTypeBasic.KindNeeded;
var
  Encoding, ByteSize: Integer;
begin
  if not InformationEntry.ReadValue(DW_AT_encoding, Encoding) then begin
    DebugLn(FPDBG_DWARF_WARNINGS, ['TFpSymbolDwarfTypeBasic.KindNeeded: Failed reading encoding for ', DwarfTagToString(InformationEntry.AbbrevTag)]);
    inherited KindNeeded;
    exit;
  end;

  if InformationEntry.ReadValue(DW_AT_byte_size, ByteSize) then
    SetSize(ByteSize);

  case Encoding of
    DW_ATE_address :      SetKind(skPointer);
    DW_ATE_boolean:       SetKind(skBoolean);
    //DW_ATE_complex_float:
    DW_ATE_float:         SetKind(skFloat);
    DW_ATE_signed:        SetKind(skInteger);
    DW_ATE_signed_char:   SetKind(skChar);
    DW_ATE_unsigned:      SetKind(skCardinal);
    DW_ATE_unsigned_char: SetKind(skChar);
    DW_ATE_numeric_string:SetKind(skChar); // temporary for widestring
    else
      begin
        DebugLn(FPDBG_DWARF_WARNINGS, ['TFpSymbolDwarfTypeBasic.KindNeeded: Unknown encoding ', DwarfBaseTypeEncodingToString(Encoding), ' for ', DwarfTagToString(InformationEntry.AbbrevTag)]);
        inherited KindNeeded;
      end;
  end;
end;

procedure TFpSymbolDwarfTypeBasic.TypeInfoNeeded;
begin
  SetTypeInfo(nil);
end;

function TFpSymbolDwarfTypeBasic.GetTypedValueObject(ATypeCast: Boolean): TFpValueDwarf;
begin
  case Kind of
    skPointer:  Result := TFpValueDwarfPointer.Create(Self, Size);
    skInteger:  Result := TFpValueDwarfInteger.Create(Self, Size);
    skCardinal: Result := TFpValueDwarfCardinal.Create(Self, Size);
    skBoolean:  Result := TFpValueDwarfBoolean.Create(Self, Size);
    skChar:     Result := TFpValueDwarfChar.Create(Self, Size);
    skFloat:    Result := TFpValueDwarfFloat.Create(Self, Size);
  end;
end;

function TFpSymbolDwarfTypeBasic.GetHasBounds: Boolean;
begin
  Result := (kind = skInteger) or (kind = skCardinal);
end;

function TFpSymbolDwarfTypeBasic.GetValueBounds(AValueObj: TFpValue; out
  ALowBound, AHighBound: Int64): Boolean;
begin
  Result := GetValueLowBound(AValueObj, ALowBound); // TODO: ond GetValueHighBound() // but all callers must check result;
  if not GetValueHighBound(AValueObj, AHighBound) then
    Result := False;
end;

function TFpSymbolDwarfTypeBasic.GetValueLowBound(AValueObj: TFpValue; out
  ALowBound: Int64): Boolean;
begin
  Result := True;
  case Kind of
    skInteger:  ALowBound := -(int64( high(int64) shr (64 - Min(Size, 8) * 8)))-1;
    skCardinal: ALowBound := 0;
    else
      Result := False;
  end;
end;

function TFpSymbolDwarfTypeBasic.GetValueHighBound(AValueObj: TFpValue; out
  AHighBound: Int64): Boolean;
begin
  Result := True;
  case Kind of
    skInteger:  AHighBound := int64( high(int64) shr (64 - Min(Size, 8) * 8));
    skCardinal: AHighBound := int64( high(qword) shr (64 - Min(Size, 8) * 8));
    else
      Result := False;
  end;
end;

{ TFpSymbolDwarfTypeModifier }

procedure TFpSymbolDwarfTypeModifier.TypeInfoNeeded;
var
  p: TFpSymbolDwarfType;
begin
  p := NestedTypeInfo;
  if p <> nil then
    SetTypeInfo(p.TypeInfo)
  else
    SetTypeInfo(nil);
end;

procedure TFpSymbolDwarfTypeModifier.ForwardToSymbolNeeded;
begin
  SetForwardToSymbol(NestedTypeInfo)
end;

function TFpSymbolDwarfTypeModifier.GetNextTypeInfoForDataAddress(
  ATargetType: TFpSymbolDwarfType): TFpSymbolDwarfType;
begin
  if (ATargetType = self) then
    Result := nil
  else
    Result := NestedTypeInfo;
end;

function TFpSymbolDwarfTypeModifier.GetTypedValueObject(ATypeCast: Boolean): TFpValueDwarf;
var
  ti: TFpSymbolDwarfType;
begin
  ti := NestedTypeInfo;
  if ti <> nil then
    Result := ti.GetTypedValueObject(ATypeCast)
  else
    Result := inherited;
end;

{ TFpSymbolDwarfTypeRef }

function TFpSymbolDwarfTypeRef.GetFlags: TDbgSymbolFlags;
begin
  Result := (inherited GetFlags) + [sfInternalRef];
end;

function TFpSymbolDwarfTypeRef.GetDataAddressNext(AValueObj: TFpValueDwarf;
  var AnAddress: TFpDbgMemLocation; out ADoneWork: Boolean;
  ATargetType: TFpSymbolDwarfType): Boolean;
begin
  Result := inherited GetDataAddressNext(AValueObj, AnAddress, ADoneWork, ATargetType);
  if (not Result) or ADoneWork then
    exit;

  Result := AValueObj.MemManager <> nil;
  if not Result then
    exit;
  AnAddress := AValueObj.MemManager.ReadAddress(AnAddress, CompilationUnit.AddressSize);
  Result := IsValidLoc(AnAddress);

  if (not Result) and
     IsError(AValueObj.MemManager.LastError)
  then
    SetLastError(AValueObj.MemManager.LastError);
  // Todo: other error
end;

{ TFpSymbolDwarfTypeSubRange }

procedure TFpSymbolDwarfTypeSubRange.InitEnumIdx;
var
  t: TFpSymbolDwarfType;
  i: Integer;
  h, l: Int64;
begin
  if FEnumIdxValid then
    exit;
  FEnumIdxValid := True;

  t := NestedTypeInfo;
  i := t.NestedSymbolCount - 1;
  GetValueBounds(nil, l, h);

  while (i >= 0) and (t.NestedSymbol[i].OrdinalValue > h) do
    dec(i);
  FHighEnumIdx := i;

  while (i >= 0) and (t.NestedSymbol[i].OrdinalValue >= l) do
    dec(i);
  FLowEnumIdx := i + 1;
end;

function TFpSymbolDwarfTypeSubRange.DoGetNestedTypeInfo: TFpSymbolDwarfType;
begin
  Result := inherited DoGetNestedTypeInfo;
  if Result <> nil then
    exit;

  if FLowBoundState = rfValue then
    Result := FLowBoundValue.TypeInfo as TFpSymbolDwarfType
  else
  if FHighBoundState = rfValue then
    Result := FHighBoundValue.TypeInfo as TFpSymbolDwarfType
  else
  if FCountState = rfValue then
    Result := FCountValue.TypeInfo as TFpSymbolDwarfType;
end;

function TFpSymbolDwarfTypeSubRange.GetHasBounds: Boolean;
var
  dummy: Int64;
begin
  if FLowBoundState = rfNotRead then
    GetValueLowBound(nil, dummy);
  Result := (FLowBoundState in [rfConst, rfValue, rfNotRead]);
  if not Result then
    exit;

  if (FHighBoundState = rfNotRead) and (FCountState = rfNotRead) then
    GetValueHighBound(nil, dummy);
  if FHighBoundState = rfNotRead then
    FCountState := rfNotFound; // dummy marker. HighBound depends on ValueObj
  Result := (FHighBoundState in [rfConst, rfValue, rfNotRead]) or
            (FCountState in [rfConst, rfValue, rfNotRead]);
end;

procedure TFpSymbolDwarfTypeSubRange.NameNeeded;
var
  AName: String;
begin
  if InformationEntry.ReadName(AName) then
    SetName(AName)
  else
    SetName('');
end;

procedure TFpSymbolDwarfTypeSubRange.KindNeeded;
var
  t: TFpSymbol;
begin
// TODO: limit to ordinal types
  if not HasBounds then begin // does ReadBounds;
    SetKind(skNone); // incomplete type
  end;

  t := NestedTypeInfo;
  if t = nil then begin
    SetKind(skInteger);
    SetSize(CompilationUnit.AddressSize);
  end
  else
    SetKind(t.Kind);
end;

procedure TFpSymbolDwarfTypeSubRange.SizeNeeded;
var
  t: TFpSymbol;
begin
  t := NestedTypeInfo;
  if t = nil then begin
    SetKind(skInteger);
    SetSize(CompilationUnit.AddressSize);
  end
  else
    SetSize(t.Size);
end;

function TFpSymbolDwarfTypeSubRange.GetNestedSymbol(AIndex: Int64): TFpSymbol;
begin
  if Kind = skEnum then begin
    if not FEnumIdxValid then
      InitEnumIdx;
    Result := NestedTypeInfo.NestedSymbol[AIndex - FLowEnumIdx];
  end
  else
    Result := inherited GetNestedSymbol(AIndex);
end;

function TFpSymbolDwarfTypeSubRange.GetNestedSymbolCount: Integer;
begin
  if Kind = skEnum then begin
    if not FEnumIdxValid then
      InitEnumIdx;
    Result := FHighEnumIdx - FLowEnumIdx + 1;
  end
  else
    Result := inherited GetNestedSymbolCount;
end;

function TFpSymbolDwarfTypeSubRange.GetFlags: TDbgSymbolFlags;
begin
  Result := (inherited GetFlags) + [sfSubRange];
end;

procedure TFpSymbolDwarfTypeSubRange.ResetValueBounds;
begin
  inherited ResetValueBounds;
  FLowBoundState := rfNotRead;
  FHighBoundState := rfNotRead;
  FCountState := rfNotRead;
end;

destructor TFpSymbolDwarfTypeSubRange.Destroy;
begin
  FLowBoundValue.ReleaseReference;
  FHighBoundValue.ReleaseReference;
  FCountValue.ReleaseReference;
  inherited Destroy;
end;

function TFpSymbolDwarfTypeSubRange.GetValueBounds(AValueObj: TFpValue; out
  ALowBound, AHighBound: Int64): Boolean;
begin
  Result := GetValueLowBound(AValueObj, ALowBound); // TODO: ond GetValueHighBound() // but all callers must check result;
  if not GetValueHighBound(AValueObj, AHighBound) then
    Result := False;
end;

function TFpSymbolDwarfTypeSubRange.GetValueLowBound(AValueObj: TFpValue;
  out ALowBound: Int64): Boolean;
var
  AttrData: TDwarfAttribData;
  t: TDBGPtr;
begin
  assert((AValueObj = nil) or (AValueObj is TFpValueDwarf), 'TFpSymbolDwarfTypeSubRange.GetValueLowBound: AValueObj is TFpValueDwarf(');
  if FLowBoundState = rfNotRead then begin
    if InformationEntry.GetAttribData(DW_AT_lower_bound, AttrData) then
      ConstRefOrExprFromAttrData(AttrData, TFpValueDwarf(AValueObj), t, @FLowBoundState, @FLowBoundValue)
    else
      FLowBoundState := rfNotFound;
    FLowBoundConst := int64(t);
  end;

  Result := FLowBoundState in [rfConst, rfValue, rfExpression];
  ALowBound := FLowBoundConst;
end;

function TFpSymbolDwarfTypeSubRange.GetValueHighBound(AValueObj: TFpValue;
  out AHighBound: Int64): Boolean;
var
  AttrData: TDwarfAttribData;
  t: TDBGPtr;
begin
  assert((AValueObj = nil) or (AValueObj is TFpValueDwarf), 'TFpSymbolDwarfTypeSubRange.GetValueHighBound: AValueObj is TFpValueDwarf(');
  if FHighBoundState = rfNotRead then begin
    if InformationEntry.GetAttribData(DW_AT_upper_bound, AttrData) then
      ConstRefOrExprFromAttrData(AttrData, TFpValueDwarf(AValueObj), t, @FHighBoundState, @FHighBoundValue)
    else
      FHighBoundState := rfNotFound;
    FHighBoundConst := int64(t);
  end;

  Result := FHighBoundState in [rfConst, rfValue, rfExpression];
  AHighBound := FHighBoundConst;

  if FHighBoundState = rfNotFound then begin
    Result := GetValueLowBound(AValueObj, AHighBound);
    if Result then begin
      if FCountState = rfNotRead then begin
        if InformationEntry.GetAttribData(DW_AT_upper_bound, AttrData) then
          ConstRefOrExprFromAttrData(AttrData, TFpValueDwarf(AValueObj), t, @FCountState, @FCountValue)
        else
          FCountState := rfNotFound;
        FCountConst := int64(t);
      end;

      Result := FCountState in [rfConst, rfValue, rfExpression];
      {$PUSH}{$R-}{$Q-}
      AHighBound := AHighBound + FCountConst;
      {$POP}
    end;
  end;
end;

procedure TFpSymbolDwarfTypeSubRange.Init;
begin
  FLowBoundState := rfNotRead;
  FHighBoundState := rfNotRead;
  FCountState := rfNotRead;
  inherited Init;
end;

{ TFpSymbolDwarfTypePointer }

procedure TFpSymbolDwarfTypePointer.KindNeeded;
begin
  SetKind(skPointer);
end;

procedure TFpSymbolDwarfTypePointer.SizeNeeded;
begin
  SetSize(CompilationUnit.AddressSize);
end;

function TFpSymbolDwarfTypePointer.GetTypedValueObject(ATypeCast: Boolean): TFpValueDwarf;
begin
  Result := TFpValueDwarfPointer.Create(Self, CompilationUnit.AddressSize);
end;

{ TFpSymbolDwarfTypeSubroutine }

procedure TFpSymbolDwarfTypeSubroutine.CreateMembers;
var
  Info: TDwarfInformationEntry;
  Info2: TDwarfInformationEntry;
begin
  if FProcMembers <> nil then
    exit;
  FProcMembers := TRefCntObjList.Create;
  Info := InformationEntry.Clone;
  Info.GoChild;

  while Info.HasValidScope do begin
    if ((Info.AbbrevTag = DW_TAG_formal_parameter) or (Info.AbbrevTag = DW_TAG_variable)) //and
       //not(Info.IsArtificial)
    then begin
      Info2 := Info.Clone;
      FProcMembers.Add(Info2);
      Info2.ReleaseReference;
    end;
    Info.GoNext;
  end;

  Info.ReleaseReference;
end;

function TFpSymbolDwarfTypeSubroutine.GetNestedSymbol(AIndex: Int64): TFpSymbol;
begin
  CreateMembers;
  FLastMember.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FLastMember, 'TFpSymbolDwarfDataProc.FLastMember'){$ENDIF};
  FLastMember := TFpSymbolDwarf.CreateSubClass('', TDwarfInformationEntry(FProcMembers[AIndex]));
  {$IFDEF WITH_REFCOUNT_DEBUG}FLastMember.DbgRenameReference(@FLastMember, 'TFpSymbolDwarfDataProc.FLastMember');{$ENDIF}
  Result := FLastMember;
end;

function TFpSymbolDwarfTypeSubroutine.GetNestedSymbolByName(AIndex: String
  ): TFpSymbol;
var
  Info: TDwarfInformationEntry;
  s, s2: String;
  i: Integer;
begin
  CreateMembers;
  s2 := LowerCase(AIndex);
  FLastMember.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FLastMember, 'TFpSymbolDwarfDataProc.FLastMember'){$ENDIF};
  FLastMember := nil;;
  for i := 0 to FProcMembers.Count - 1 do begin
    Info := TDwarfInformationEntry(FProcMembers[i]);
    if Info.ReadName(s) and (LowerCase(s) = s2) then begin
      FLastMember := TFpSymbolDwarf.CreateSubClass('', Info);
      {$IFDEF WITH_REFCOUNT_DEBUG}FLastMember.DbgRenameReference(@FLastMember, 'TFpSymbolDwarfDataProc.FLastMember');{$ENDIF}
      break;
    end;
  end;
  Result := FLastMember;
end;

function TFpSymbolDwarfTypeSubroutine.GetNestedSymbolCount: Integer;
begin
  CreateMembers;
  Result := FProcMembers.Count;
end;

function TFpSymbolDwarfTypeSubroutine.GetTypedValueObject(ATypeCast: Boolean
  ): TFpValueDwarf;
begin
  Result := TFpValueDwarfSubroutine.Create(Self);
end;

function TFpSymbolDwarfTypeSubroutine.GetDataAddressNext(
  AValueObj: TFpValueDwarf; var AnAddress: TFpDbgMemLocation; out
  ADoneWork: Boolean; ATargetType: TFpSymbolDwarfType): Boolean;
begin
  Result := inherited GetDataAddressNext(AValueObj, AnAddress, ADoneWork, ATargetType);
  if (not Result) or ADoneWork then
    exit;

  Result := AValueObj.MemManager <> nil;
  if not Result then
    exit;
  AnAddress := AValueObj.MemManager.ReadAddress(AnAddress, CompilationUnit.AddressSize);
  Result := IsValidLoc(AnAddress);

  if not Result then
    if IsError(AValueObj.MemManager.LastError) then
      SetLastError(AValueObj.MemManager.LastError);
  // Todo: other error
end;

procedure TFpSymbolDwarfTypeSubroutine.KindNeeded;
begin
  if TypeInfo <> nil then
    SetKind(skFunctionRef)
  else
    SetKind(skProcedureRef);
end;

destructor TFpSymbolDwarfTypeSubroutine.Destroy;
begin
  FreeAndNil(FProcMembers);
  FLastMember.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FLastMember, 'TFpSymbolDwarfDataProc.FLastMember'){$ENDIF};
  inherited Destroy;
end;

{ TDbgDwarfIdentifierEnumElement }

procedure TFpSymbolDwarfDataEnumMember.ReadOrdinalValue;
begin
  if FOrdinalValueRead then exit;
  FOrdinalValueRead := True;
  FHasOrdinalValue := InformationEntry.ReadValue(DW_AT_const_value, FOrdinalValue);
end;

procedure TFpSymbolDwarfDataEnumMember.KindNeeded;
begin
  SetKind(skEnumValue);
end;

function TFpSymbolDwarfDataEnumMember.GetHasOrdinalValue: Boolean;
begin
  ReadOrdinalValue;
  Result := FHasOrdinalValue;
end;

function TFpSymbolDwarfDataEnumMember.GetOrdinalValue: Int64;
begin
  ReadOrdinalValue;
  Result := FOrdinalValue;
end;

procedure TFpSymbolDwarfDataEnumMember.Init;
begin
  FOrdinalValueRead := False;
  inherited Init;
end;

function TFpSymbolDwarfDataEnumMember.GetValueObject: TFpValue;
begin
  Result := FValueObject;
  if Result <> nil then exit;

  FValueObject := TFpValueDwarfEnumMember.Create(Self);
  {$IFDEF WITH_REFCOUNT_DEBUG}FValueObject.DbgRenameReference(@FValueObject, ClassName+'.FValueObject');{$ENDIF}
  FValueObject.MakePlainRefToCirclular;
  FValueObject.SetValueSymbol(self);

  Result := FValueObject;
end;

{ TFpSymbolDwarfTypeEnum }

procedure TFpSymbolDwarfTypeEnum.CreateMembers;
var
  Info, Info2: TDwarfInformationEntry;
  sym: TFpSymbolDwarf;
begin
  if FMembers <> nil then
    exit;
  FMembers := TFpDbgCircularRefCntObjList.Create;
  Info := InformationEntry.FirstChild;
  if Info = nil then exit;

  while Info.HasValidScope do begin
    if (Info.AbbrevTag = DW_TAG_enumerator) then begin
      Info2 := Info.Clone;
      sym := TFpSymbolDwarf.CreateSubClass('', Info2);
      FMembers.Add(sym);
      sym.ReleaseReference;
      sym.ParentTypeInfo := self;
      Info2.ReleaseReference;
    end;
    Info.GoNext;
  end;

  Info.ReleaseReference;
end;

function TFpSymbolDwarfTypeEnum.GetTypedValueObject(ATypeCast: Boolean): TFpValueDwarf;
begin
  Result := TFpValueDwarfEnum.Create(Self, Size);
end;

procedure TFpSymbolDwarfTypeEnum.KindNeeded;
begin
  SetKind(skEnum);
end;

function TFpSymbolDwarfTypeEnum.GetNestedSymbol(AIndex: Int64): TFpSymbol;
begin
  CreateMembers;
  Result := TFpSymbol(FMembers[AIndex]);
end;

function TFpSymbolDwarfTypeEnum.GetNestedSymbolByName(AIndex: String): TFpSymbol;
var
  i: Integer;
  s, s1, s2: String;
begin
  if AIndex = '' then
  s1 := UTF8UpperCase(AIndex);
  s2 := UTF8LowerCase(AIndex);
  CreateMembers;
  i := FMembers.Count - 1;
  while i >= 0 do begin
    Result := TFpSymbol(FMembers[i]);
    s := Result.Name;
    if (s <> '') and CompareUtf8BothCase(@s1[1], @s2[1], @s[1]) then
      exit;
    dec(i);
  end;
  Result := nil;
end;

function TFpSymbolDwarfTypeEnum.GetNestedSymbolCount: Integer;
begin
  CreateMembers;
  Result := FMembers.Count;
end;

function TFpSymbolDwarfTypeEnum.GetHasBounds: Boolean;
begin
  Result := True;
end;

destructor TFpSymbolDwarfTypeEnum.Destroy;
var
  i: Integer;
begin
  if FMembers <> nil then
    for i := 0 to FMembers.Count - 1 do
      TFpSymbolDwarf(FMembers[i]).ParentTypeInfo := nil;
  FreeAndNil(FMembers);
  inherited Destroy;
end;

function TFpSymbolDwarfTypeEnum.GetValueBounds(AValueObj: TFpValue; out
  ALowBound, AHighBound: Int64): Boolean;
begin
  Result := GetValueLowBound(AValueObj, ALowBound); // TODO: ond GetValueHighBound() // but all callers must check result;
  if not GetValueHighBound(AValueObj, AHighBound) then
    Result := False;
end;

function TFpSymbolDwarfTypeEnum.GetValueLowBound(AValueObj: TFpValue; out
  ALowBound: Int64): Boolean;
var
  c: Integer;
begin
  Result := True;
  c := NestedSymbolCount;
  if c > 0 then
    ALowBound := NestedSymbol[0].OrdinalValue
  else
    ALowBound := 0;
end;

function TFpSymbolDwarfTypeEnum.GetValueHighBound(AValueObj: TFpValue; out
  AHighBound: Int64): Boolean;
var
  c: Integer;
begin
  Result := True;
  c := NestedSymbolCount;
  if c > 0 then
    AHighBound := NestedSymbol[c-1].OrdinalValue
  else
    AHighBound := -1;
end;

{ TFpSymbolDwarfTypeSet }

procedure TFpSymbolDwarfTypeSet.KindNeeded;
begin
  SetKind(skSet);
end;

function TFpSymbolDwarfTypeSet.GetTypedValueObject(ATypeCast: Boolean): TFpValueDwarf;
begin
  Result := TFpValueDwarfSet.Create(Self, Size);
end;

function TFpSymbolDwarfTypeSet.GetNestedSymbolCount: Integer;
begin
  if TypeInfo.Kind = skEnum then
    Result := TypeInfo.NestedSymbolCount
  else
    Result := inherited GetNestedSymbolCount;
end;

function TFpSymbolDwarfTypeSet.GetNestedSymbol(AIndex: Int64): TFpSymbol;
begin
  if TypeInfo.Kind = skEnum then
    Result := TypeInfo.NestedSymbol[AIndex]
  else
    Result := inherited GetNestedSymbol(AIndex);
end;

{ TFpSymbolDwarfDataMember }

function TFpSymbolDwarfDataMember.GetValueAddress(AValueObj: TFpValueDwarf; out
  AnAddress: TFpDbgMemLocation): Boolean;
begin
  if AValueObj = nil then debugln(['TFpSymbolDwarfDataMember.InitLocationParser: NO VAl Obj !!!!!!!!!!!!!!!'])
  else if AValueObj.StructureValue = nil then debugln(['TFpSymbolDwarfDataMember.InitLocationParser: NO STRUCT Obj !!!!!!!!!!!!!!!']);

  if (AValueObj = nil) or (AValueObj.StructureValue = nil) or (ParentTypeInfo = nil)
  then begin
    debugln(FPDBG_DWARF_ERRORS, ['DWARF ERROR in TFpSymbolDwarfDataMember.InitLocationParser Error: ',ErrorCode(LastError),' ValueObject=', DbgSName(FValueObject)]);
    Result := False;
    if not IsError(LastError) then
      SetLastError(CreateError(fpErrLocationParserInit)); // TODO: error message?
    exit;
  end;
  Assert((ParentTypeInfo is TFpSymbolDwarf) and (ParentTypeInfo.SymbolType = stType), '');
  if not AValueObj.GetStructureDwarfDataAddress(AnAddress, TFpSymbolDwarfType(ParentTypeInfo)) then begin
    debugln(FPDBG_DWARF_ERRORS, ['DWARF ERROR in TFpSymbolDwarfDataMember.InitLocationParser Error: ',ErrorCode(LastError),' ValueObject=', DbgSName(FValueObject)]);
    Result := False;
    if not IsError(LastError) then
      SetLastError(CreateError(fpErrLocationParserInit)); // TODO: error message?
    exit;
  end;
  //TODO: AValueObj.StructureValue.LastError

  Result := ComputeDataMemberAddress(InformationEntry, AValueObj, AnAddress);
  if not Result then
    exit;
end;

function TFpSymbolDwarfDataMember.HasAddress: Boolean;
begin
  Result := (InformationEntry.HasAttrib(DW_AT_data_member_location));
end;

{ TFpSymbolDwarfTypeStructure }

function TFpSymbolDwarfTypeStructure.GetNestedSymbolByName(AIndex: String): TFpSymbol;
var
  Ident: TDwarfInformationEntry;
  ti: TFpSymbol;
begin
  // Todo, maybe create all children?
  if FLastChildByName <> nil then begin
    FLastChildByName.ReleaseCirclularReference;
    FLastChildByName := nil;
  end;
  Result := nil;

  Ident := InformationEntry.FindNamedChild(AIndex);
  if Ident <> nil then begin
    FLastChildByName := TFpSymbolDwarf.CreateSubClass('', Ident);
    FLastChildByName.MakePlainRefToCirclular;
    FLastChildByName.ParentTypeInfo := self;
    //assert is member ?
    ReleaseRefAndNil(Ident);
    Result := FLastChildByName;

    exit;
  end;

  ti := TypeInfo; // Parent
  if ti <> nil then
    Result := ti.NestedSymbolByName[AIndex];
end;

function TFpSymbolDwarfTypeStructure.GetNestedSymbolCount: Integer;
var
  ti: TFpSymbol;
begin
  CreateMembers;
  Result := FMembers.Count;

  ti := TypeInfo;
  if ti <> nil then
    Result := Result + ti.NestedSymbolCount;
end;

function TFpSymbolDwarfTypeStructure.GetDataAddressNext(
  AValueObj: TFpValueDwarf; var AnAddress: TFpDbgMemLocation; out
  ADoneWork: Boolean; ATargetType: TFpSymbolDwarfType): Boolean;
begin
  Result := inherited GetDataAddressNext(AValueObj, AnAddress, ADoneWork, ATargetType);

  (* We have the DataAddress for this class => stop here, unless ATargetType
     indicates that we want a parent-class DataAddress
     Adding the InheritanceInfo's DW_AT_data_member_location would normally
     have to be done by the parent class. But then we would need to make it
     available there.
     // TODO: Could not determine from the Dwarf Spec, if the parent class
        should skip its DW_AT_data_location, if it was reached via
        DW_AT_data_member_location
        The spec says "handled the same as for members" => might indicate it should
  *)

  if (ATargetType = nil) or (ATargetType = self) then
    exit;

  Result := IsReadableMem(AnAddress);
  if not Result then
    exit;
  InitInheritanceInfo;

  Result := FInheritanceInfo = nil;
  if Result then
    exit;

  Result := ComputeDataMemberAddress(FInheritanceInfo, AValueObj, AnAddress);
  if not Result then
    exit;
end;

function TFpSymbolDwarfTypeStructure.GetNestedSymbol(AIndex: Int64): TFpSymbol;
var
  ti: TFpSymbol;
  i: Int64;
begin
  CreateMembers;

  i := AIndex;
  ti := TypeInfo;
  if ti <> nil then
    i := i - ti.NestedSymbolCount;

  if i < 0 then
    Result := ti.NestedSymbol[AIndex]
  else
    Result := TFpSymbol(FMembers[i]);
end;

destructor TFpSymbolDwarfTypeStructure.Destroy;
var
  i: Integer;
begin
  ReleaseRefAndNil(FInheritanceInfo);
  if FMembers <> nil then begin
    for i := 0 to FMembers.Count - 1 do
      TFpSymbolDwarf(FMembers[i]).ParentTypeInfo := nil;
    FreeAndNil(FMembers);
  end;
  if FLastChildByName <> nil then begin
    FLastChildByName.ParentTypeInfo := nil;
    FLastChildByName.ReleaseCirclularReference;
    FLastChildByName := nil;
  end;
  inherited Destroy;
end;

procedure TFpSymbolDwarfTypeStructure.CreateMembers;
var
  Info: TDwarfInformationEntry;
  Info2: TDwarfInformationEntry;
  sym: TFpSymbolDwarf;
begin
  if FMembers <> nil then
    exit;
  FMembers := TFpDbgCircularRefCntObjList.Create;
  Info := InformationEntry.Clone;
  Info.GoChild;

  while Info.HasValidScope do begin
    if (Info.AbbrevTag = DW_TAG_member) or (Info.AbbrevTag = DW_TAG_subprogram) then begin
      Info2 := Info.Clone;
      sym := TFpSymbolDwarf.CreateSubClass('', Info2);
      FMembers.Add(sym);
      sym.ReleaseReference;
      sym.ParentTypeInfo := self;
      Info2.ReleaseReference;
    end;
    Info.GoNext;
  end;

  Info.ReleaseReference;
end;

procedure TFpSymbolDwarfTypeStructure.InitInheritanceInfo;
begin
  if FInheritanceInfo = nil then
    FInheritanceInfo := InformationEntry.FindChildByTag(DW_TAG_inheritance);
end;

function TFpSymbolDwarfTypeStructure.DoGetNestedTypeInfo: TFpSymbolDwarfType;
var
  FwdInfoPtr: Pointer;
  FwdCompUint: TDwarfCompilationUnit;
  ParentInfo: TDwarfInformationEntry;
begin
  Result:= nil;
  InitInheritanceInfo;
  if (FInheritanceInfo <> nil) and
     FInheritanceInfo.ReadReference(DW_AT_type, FwdInfoPtr, FwdCompUint)
  then begin
    ParentInfo := TDwarfInformationEntry.Create(FwdCompUint, FwdInfoPtr);
    //DebugLn(FPDBG_DWARF_SEARCH, ['Inherited from ', dbgs(ParentInfo.FInformationEntry, FwdCompUint) ]);
    Result := TFpSymbolDwarfType.CreateTypeSubClass('', ParentInfo);
    ParentInfo.ReleaseReference;
  end;
end;

procedure TFpSymbolDwarfTypeStructure.KindNeeded;
begin
  if (InformationEntry.AbbrevTag = DW_TAG_class_type) then
    SetKind(skClass)
  else
  if (InformationEntry.AbbrevTag = DW_TAG_interface_type) then
    SetKind(skInterface)
  else
    SetKind(skRecord);
end;

function TFpSymbolDwarfTypeStructure.GetTypedValueObject(ATypeCast: Boolean): TFpValueDwarf;
begin
  if ATypeCast then
    Result := TFpValueDwarfStructTypeCast.Create(Self)
  else
    Result := TFpValueDwarfStruct.Create(Self);
end;

{ TFpSymbolDwarfTypeArray }

procedure TFpSymbolDwarfTypeArray.CreateMembers;
var
  Info, Info2: TDwarfInformationEntry;
  t: Cardinal;
  sym: TFpSymbolDwarf;
begin
  if FMembers <> nil then
    exit;
  FMembers := TFpDbgCircularRefCntObjList.Create;

  Info := InformationEntry.FirstChild;
  if Info = nil then exit;

  while Info.HasValidScope do begin
    t := Info.AbbrevTag;
    if (t = DW_TAG_enumeration_type) or (t = DW_TAG_subrange_type) then begin
      Info2 := Info.Clone;
      sym := TFpSymbolDwarf.CreateSubClass('', Info2);
      FMembers.Add(sym);
      sym.ReleaseReference;
      sym.ParentTypeInfo := self;
      Info2.ReleaseReference;
    end;
    Info.GoNext;
  end;

  Info.ReleaseReference;
end;

procedure TFpSymbolDwarfTypeArray.ReadStride;
var
  t: TFpSymbolDwarfType;
begin
  if didtStrideRead in FDwarfArrayReadFlags then
    exit;
  Include(FDwarfArrayReadFlags, didtStrideRead);
  if InformationEntry.ReadValue(DW_AT_bit_stride, FStrideInBits) then
    exit;

  CreateMembers;
  if (FMembers.Count > 0) and // TODO: stride for diff member
     (TDbgDwarfSymbolBase(FMembers[0]).InformationEntry.ReadValue(DW_AT_byte_stride, FStrideInBits))
  then begin
    FStrideInBits := FStrideInBits * 8;
    exit;
  end;

  t := NestedTypeInfo;
  if t = nil then
    FStrideInBits := 0 //  TODO error
  else
    FStrideInBits := t.Size * 8;
end;

procedure TFpSymbolDwarfTypeArray.ReadOrdering;
var
  AVal: Integer;
begin
  if didtOrdering in FDwarfArrayReadFlags then
    exit;
  Include(FDwarfArrayReadFlags, didtOrdering);
  if InformationEntry.ReadValue(DW_AT_ordering, AVal) then
    FRowMajor := AVal = DW_ORD_row_major
  else
    FRowMajor := True; // default (at least in pas)
end;

procedure TFpSymbolDwarfTypeArray.KindNeeded;
begin
  SetKind(skArray); // Todo: static/dynamic?
end;

function TFpSymbolDwarfTypeArray.GetTypedValueObject(ATypeCast: Boolean): TFpValueDwarf;
begin
  Result := TFpValueDwarfArray.Create(Self);
end;

function TFpSymbolDwarfTypeArray.GetFlags: TDbgSymbolFlags;
  function IsDynSubRange(m: TFpSymbolDwarf): Boolean;
  begin
    Result := sfSubRange in m.Flags;
    if not Result then exit;
    while (m <> nil) and not(m is TFpSymbolDwarfTypeSubRange) do
      m := m.NestedTypeInfo;
    Result := m <> nil;
    if not Result then exit; // TODO: should not happen, handle error
    Result := (TFpSymbolDwarfTypeSubRange(m).FHighBoundState = rfValue) // dynamic high bound // TODO:? Could be rfConst for locationExpr
           or (TFpSymbolDwarfTypeSubRange(m).FHighBoundState = rfNotRead); // dynamic high bound (yet to be read)
  end;
var
  m: TFpSymbol;
  lb, hb: Int64;
begin
  Result := inherited GetFlags;
  if (NestedSymbolCount = 1) then begin   // TODO: move to freepascal specific
    m := NestedSymbol[0];
    if (not m.GetValueBounds(nil, lb, hb)) or                // e.g. Subrange with missing upper bound
       (hb < lb) or
       (IsDynSubRange(TFpSymbolDwarf(m)))
    then
      Result := Result + [sfDynArray]
    else
      Result := Result + [sfStatArray];
  end
  else
    Result := Result + [sfStatArray];
end;

function TFpSymbolDwarfTypeArray.GetNestedSymbol(AIndex: Int64): TFpSymbol;
begin
  CreateMembers;
  Result := TFpSymbol(FMembers[AIndex]);
end;

function TFpSymbolDwarfTypeArray.GetNestedSymbolByName(AIndex: String): TFpSymbol;
begin
  Result := nil; // no named members
end;

function TFpSymbolDwarfTypeArray.GetNestedSymbolCount: Integer;
begin
  CreateMembers;
  Result := FMembers.Count;
end;

function TFpSymbolDwarfTypeArray.GetMemberAddress(AValObject: TFpValueDwarf;
  const AIndex: array of Int64): TFpDbgMemLocation;
var
  Idx, Offs, Factor: Int64;
  LowBound, HighBound: int64;
  i: Integer;
  bsize: Integer;
  m: TFpSymbolDwarf;
begin
  assert((AValObject is TFpValueDwarfArray), 'TFpSymbolDwarfTypeArray.GetMemberAddress AValObject');
  ReadOrdering;
  ReadStride; // TODO Stride per member (member = dimension/index)
  Result := InvalidLoc;
  if (FStrideInBits <= 0) or (FStrideInBits mod 8 <> 0) then
    exit;

  CreateMembers;
  if Length(AIndex) > FMembers.Count then
    exit;

  if AValObject is TFpValueDwarfArray then begin
    if not TFpValueDwarfArray(AValObject).GetDwarfDataAddress(Result, Self) then begin
      Result := InvalidLoc;
      Exit;
    end;
  end
  else
    exit; // TODO error
  if IsTargetNil(Result) then begin
    // TODO: return the nil address, for better error message?
    Result := InvalidLoc;
    Exit;
  end;

  Offs := 0;
  Factor := 1;

  {$PUSH}{$R-}{$Q-} // TODO: check range of index
  bsize := FStrideInBits div 8;
  if FRowMajor then begin
    for i := Length(AIndex) - 1 downto 0 do begin
      Idx := AIndex[i];
      m := TFpSymbolDwarf(FMembers[i]);
      if i > 0 then begin
        if not m.GetValueBounds(AValObject, LowBound, HighBound) then begin
          Result := InvalidLoc;
          exit;
        end;
        Idx := Idx - LowBound;
        Offs := Offs + Idx * bsize * Factor;
        Factor := Factor * (HighBound - LowBound + 1);  // TODO range check
      end
      else begin
        if m.GetValueLowBound(AValObject, LowBound) then
          Idx := Idx - LowBound;
        Offs := Offs + Idx * bsize * Factor;
      end;
    end;
  end
  else begin
    for i := 0 to Length(AIndex) - 1 do begin
      Idx := AIndex[i];
      m := TFpSymbolDwarf(FMembers[i]);
      if i > 0 then begin
        if not m.GetValueBounds(AValObject, LowBound, HighBound) then begin
          Result := InvalidLoc;
          exit;
        end;
        Idx := Idx - LowBound;
        Offs := Offs + Idx * bsize * Factor;
        Factor := Factor * (HighBound - LowBound + 1);  // TODO range check
      end
      else begin
        if m.GetValueLowBound(AValObject, LowBound) then
          Idx := Idx - LowBound;
        Offs := Offs + Idx * bsize * Factor;
      end;
    end;
  end;

  assert(IsReadableMem(Result), 'DwarfArray MemberAddress');
  Result.Address := Result.Address + Offs;
  {$POP}
end;

destructor TFpSymbolDwarfTypeArray.Destroy;
var
  i: Integer;
begin
  if FMembers <> nil then begin
    for i := 0 to FMembers.Count - 1 do
      TFpSymbolDwarf(FMembers[i]).ParentTypeInfo := nil;
    FreeAndNil(FMembers);
  end;
  inherited Destroy;
end;

procedure TFpSymbolDwarfTypeArray.ResetValueBounds;
var
  i: Integer;
begin
  debuglnEnter(['TFpSymbolDwarfTypeArray.ResetValueBounds ' , Self.ClassName, dbgs(self)]); try
  inherited ResetValueBounds;
  FDwarfArrayReadFlags := [];
  if FMembers <> nil then
    for i := 0 to FMembers.Count - 1 do
      if TObject(FMembers[i]) is TFpSymbolDwarfType then
        TFpSymbolDwarfType(FMembers[i]).ResetValueBounds;
  finally debuglnExit(['TFpSymbolDwarfTypeArray.ResetValueBounds ' ]); end;
end;

{ TDbgDwarfSymbol }

constructor TFpSymbolDwarfDataProc.Create(ACompilationUnit: TDwarfCompilationUnit;
  AInfo: PDwarfAddressInfo; AAddress: TDbgPtr);
var
  InfoEntry: TDwarfInformationEntry;
begin
  FAddress := AAddress;
  FAddressInfo := AInfo;

  InfoEntry := TDwarfInformationEntry.Create(ACompilationUnit, nil);
  InfoEntry.ScopeIndex := AInfo^.ScopeIndex;

  inherited Create(
    String(FAddressInfo^.Name),
    InfoEntry
  );

  SetAddress(TargetLoc(FAddressInfo^.StartPC));

  InfoEntry.ReleaseReference;
//BuildLineInfo(

//   AFile: String = ''; ALine: Integer = -1; AFlags: TDbgSymbolFlags = []; const AReference: TDbgSymbol = nil);
end;

destructor TFpSymbolDwarfDataProc.Destroy;
begin
  FreeAndNil(FProcMembers);
  FLastMember.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FLastMember, 'TFpSymbolDwarfDataProc.FLastMember'){$ENDIF};
  FreeAndNil(FStateMachine);
  if FSelfParameter <> nil then begin
    //TDbgDwarfIdentifier(FSelfParameter.DbgSymbol).ParentTypeInfo := nil;
    FSelfParameter.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSelfParameter, 'FSelfParameter'){$ENDIF};
  end;
  inherited Destroy;
end;

function TFpSymbolDwarfDataProc.GetColumn: Cardinal;
begin
  if StateMachineValid
  then Result := FStateMachine.Column
  else Result := inherited GetColumn;
end;

function TFpSymbolDwarfDataProc.GetFile: String;
begin
  if StateMachineValid
  then Result := FStateMachine.FileName
  else Result := inherited GetFile;
end;

function TFpSymbolDwarfDataProc.GetLine: Cardinal;
begin
  if StateMachineValid
  then Result := FStateMachine.Line
  else Result := inherited GetLine;
end;

function TFpSymbolDwarfDataProc.GetValueObject: TFpValue;
begin
  Result := FValueObject;
  if Result <> nil then exit;

  FValueObject := TFpValueDwarfSubroutine.Create(nil);
  {$IFDEF WITH_REFCOUNT_DEBUG}FValueObject.DbgRenameReference(@FValueObject, ClassName+'.FValueObject');{$ENDIF}
  FValueObject.MakePlainRefToCirclular;
  FValueObject.SetValueSymbol(self);

  Result := FValueObject;
end;

function TFpSymbolDwarfDataProc.GetValueAddress(AValueObj: TFpValueDwarf; out
  AnAddress: TFpDbgMemLocation): Boolean;
var
  AttrData: TDwarfAttribData;
  Addr: TDBGPtr;
begin
  AnAddress := InvalidLoc;
  if InformationEntry.GetAttribData(DW_AT_low_pc, AttrData) then
    if InformationEntry.ReadAddressValue(AttrData, Addr) then
      AnAddress := TargetLoc(Addr);
  //DW_AT_ranges
  Result := IsValidLoc(AnAddress);
end;

function TFpSymbolDwarfDataProc.StateMachineValid: Boolean;
var
  SM1, SM2: TDwarfLineInfoStateMachine;
begin
  Result := FStateMachine <> nil;
  if Result then Exit;

  if FAddressInfo^.StateMachine = nil
  then begin
    CompilationUnit.BuildLineInfo(FAddressInfo, False);
    if FAddressInfo^.StateMachine = nil then Exit;
  end;

  // we cannot restore a statemachine to its current state
  // so we shouldn't modify FAddressInfo^.StateMachine
  // so use clones to navigate
  SM1 := FAddressInfo^.StateMachine.Clone;
  if FAddress < SM1.Address
  then begin
    // The address we want to find is before the start of this symbol ??
    SM1.Free;
    Exit;
  end;
  SM2 := FAddressInfo^.StateMachine.Clone;

  repeat
    if (FAddress = SM1.Address)
    or not SM2.NextLine
    or (FAddress < SM2.Address)
    then begin
      // found
      FStateMachine := SM1;
      SM2.Free;
      Result := True;
      Exit;
    end;
  until not SM1.NextLine;

  //if all went well we shouldn't come here
  SM1.Free;
  SM2.Free;
end;

function TFpSymbolDwarfDataProc.ReadVirtuality(out AFlags: TDbgSymbolFlags): Boolean;
var
  Val: Integer;
begin
  AFlags := [];
  Result := InformationEntry.ReadValue(DW_AT_virtuality, Val);
  if not Result then exit;
  case Val of
    DW_VIRTUALITY_none:   ;
    DW_VIRTUALITY_virtual:      AFlags := [sfVirtual];
    DW_VIRTUALITY_pure_virtual: AFlags := [sfVirtual];
  end;
end;

procedure TFpSymbolDwarfDataProc.CreateMembers;
var
  Info: TDwarfInformationEntry;
  Info2: TDwarfInformationEntry;
begin
  if FProcMembers <> nil then
    exit;
  FProcMembers := TRefCntObjList.Create;
  Info := InformationEntry.Clone;
  Info.GoChild;

  while Info.HasValidScope do begin
    if ((Info.AbbrevTag = DW_TAG_formal_parameter) or (Info.AbbrevTag = DW_TAG_variable)) //and
       //not(Info.IsArtificial)
    then begin
      Info2 := Info.Clone;
      FProcMembers.Add(Info2);
      Info2.ReleaseReference;
    end;
    Info.GoNext;
  end;

  Info.ReleaseReference;
end;

function TFpSymbolDwarfDataProc.GetNestedSymbol(AIndex: Int64): TFpSymbol;
begin
  CreateMembers;
  FLastMember.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FLastMember, 'TFpSymbolDwarfDataProc.FLastMember'){$ENDIF};
  FLastMember := TFpSymbolDwarf.CreateSubClass('', TDwarfInformationEntry(FProcMembers[AIndex]));
  {$IFDEF WITH_REFCOUNT_DEBUG}FLastMember.DbgRenameReference(@FLastMember, 'TFpSymbolDwarfDataProc.FLastMember');{$ENDIF}
  Result := FLastMember;
end;

function TFpSymbolDwarfDataProc.GetNestedSymbolByName(AIndex: String): TFpSymbol;
var
  Info: TDwarfInformationEntry;
  s, s2: String;
  i: Integer;
begin
  CreateMembers;
  s2 := LowerCase(AIndex);
  FLastMember.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FLastMember, 'TFpSymbolDwarfDataProc.FLastMember'){$ENDIF};
  FLastMember := nil;;
  for i := 0 to FProcMembers.Count - 1 do begin
    Info := TDwarfInformationEntry(FProcMembers[i]);
    if Info.ReadName(s) and (LowerCase(s) = s2) then begin
      FLastMember := TFpSymbolDwarf.CreateSubClass('', Info);
      {$IFDEF WITH_REFCOUNT_DEBUG}FLastMember.DbgRenameReference(@FLastMember, 'TFpSymbolDwarfDataProc.FLastMember');{$ENDIF}
      break;
    end;
  end;
  Result := FLastMember;
end;

function TFpSymbolDwarfDataProc.GetNestedSymbolCount: Integer;
begin
  CreateMembers;
  Result := FProcMembers.Count;
end;

function TFpSymbolDwarfDataProc.GetFrameBase(ASender: TDwarfLocationExpression): TDbgPtr;
var
  Val: TByteDynArray;
  rd: TFpDbgMemLocation;
begin
  Result := 0;
  if FFrameBaseParser = nil then begin
    //TODO: avoid copying data
    if not  InformationEntry.ReadValue(DW_AT_frame_base, Val) then begin
      // error
      debugln(FPDBG_DWARF_ERRORS, ['TFpSymbolDwarfDataProc.GetFrameBase failed to read DW_AT_frame_base']);
      exit;
    end;
    if Length(Val) = 0 then begin
      // error
      debugln(FPDBG_DWARF_ERRORS, ['TFpSymbolDwarfDataProc.GetFrameBase failed to read DW_AT_location']);
      exit;
    end;

    FFrameBaseParser := TDwarfLocationExpression.Create(@Val[0], Length(Val), CompilationUnit,
      ASender.MemManager, ASender.Context);
    FFrameBaseParser.Evaluate;
  end;

  rd := FFrameBaseParser.ResultData;
  if IsValidLoc(rd) then
    Result := rd.Address;

  if IsError(FFrameBaseParser.LastError) then begin
    SetLastError(FFrameBaseParser.LastError);
    debugln(FPDBG_DWARF_ERRORS, ['TFpSymbolDwarfDataProc.GetFrameBase location parser failed ', ErrorHandler.ErrorAsString(LastError)]);
  end
  else
  if Result = 0 then begin
    debugln(FPDBG_DWARF_ERRORS, ['TFpSymbolDwarfDataProc.GetFrameBase location parser failed. result is 0']);
  end;

end;

procedure TFpSymbolDwarfDataProc.KindNeeded;
begin
  SetKind(TypeInfo.Kind);
end;

procedure TFpSymbolDwarfDataProc.SizeNeeded;
begin
  SetSize(FAddressInfo^.EndPC - FAddressInfo^.StartPC);
end;

function TFpSymbolDwarfDataProc.GetFlags: TDbgSymbolFlags;
var
  flg: TDbgSymbolFlags;
begin
  Result := inherited GetFlags;
  if ReadVirtuality(flg) then
    Result := Result + flg;
end;

procedure TFpSymbolDwarfDataProc.TypeInfoNeeded;
var
  t: TFpSymbolDwarfTypeProc;
begin
  t := TFpSymbolDwarfTypeProc.Create('', InformationEntry, Self); // returns with 1 circulor ref
  SetTypeInfo(t); // TODO: avoid adding a reference, already got one....
  t.ReleaseReference;
end;

function TFpSymbolDwarfDataProc.GetSelfParameter(AnAddress: TDbgPtr): TFpValueDwarf;
const
  this1: string = 'THIS';
  this2: string = 'this';
  self1: string = '$SELF';
  self2: string = '$self';
var
  InfoEntry: TDwarfInformationEntry;
  tg: Cardinal;
  found: Boolean;
begin
  // special: search "self"
  // Todo nested procs
  Result := FSelfParameter;
  if Result <> nil then exit;

  InfoEntry := InformationEntry.Clone;
  //StartScopeIdx := InfoEntry.ScopeIndex;
  InfoEntry.GoParent;
  tg := InfoEntry.AbbrevTag;
  if (tg = DW_TAG_class_type) or (tg = DW_TAG_structure_type) then begin
    InfoEntry.ScopeIndex := InformationEntry.ScopeIndex;
    found := InfoEntry.GoNamedChildEx(@this1[1], @this2[1]);
    if not found then begin
      InfoEntry.ScopeIndex := InformationEntry.ScopeIndex;
      found := InfoEntry.GoNamedChildEx(@self1[1], @self2[1]);
    end;
    if found then begin
      if ((AnAddress = 0) or InfoEntry.IsAddressInStartScope(AnAddress)) and
         InfoEntry.IsArtificial
      then begin
        Result := TFpValueDwarf(TFpSymbolDwarfData.CreateValueSubClass('self', InfoEntry).Value);
        FSelfParameter := Result;
        FSelfParameter.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSelfParameter, 'FSelfParameter'){$ENDIF};
        FSelfParameter.DbgSymbol.ReleaseReference;
        //FSelfParameter.DbgSymbol.ParentTypeInfo := Self;
        debugln(FPDBG_DWARF_SEARCH, ['TFpSymbolDwarfDataProc.GetSelfParameter ', InfoEntry.ScopeDebugText, DbgSName(Result)]);
      end;
    end;
  end;
  InfoEntry.ReleaseReference;
end;

{ TFpSymbolDwarfTypeProc }

procedure TFpSymbolDwarfTypeProc.ForwardToSymbolNeeded;
begin
  SetForwardToSymbol(FDataSymbol); // Does *NOT* add reference
end;

procedure TFpSymbolDwarfTypeProc.CircleBackRefActiveChanged(ANewActive: Boolean
  );
begin
  inherited CircleBackRefActiveChanged(ANewActive);
  if (FDataSymbol = nil) then
    exit;

  if ANewActive then
    FDataSymbol.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FDataSymbol, 'FDataSymbol'){$ENDIF}
  else
    FDataSymbol.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FDataSymbol, 'FDataSymbol'){$ENDIF};
end;

procedure TFpSymbolDwarfTypeProc.NameNeeded;
begin
  case Kind of
    skFunction:  SetName('function');
    skProcedure: SetName('procedure');
    else         SetName('');
  end;
end;

procedure TFpSymbolDwarfTypeProc.KindNeeded;
begin
  if TypeInfo <> nil then
    SetKind(skFunction)
  else
    SetKind(skProcedure);
end;

procedure TFpSymbolDwarfTypeProc.TypeInfoNeeded;
begin
  SetTypeInfo(FDataSymbol.NestedTypeInfo);
end;

constructor TFpSymbolDwarfTypeProc.Create(AName: String;
  AnInformationEntry: TDwarfInformationEntry;
  ADataSymbol: TFpSymbolDwarfDataProc);
begin
  inherited Create(AName, AnInformationEntry);
  MakePlainRefToCirclular;          // Done for the Caller // So we can set FDataSymbol without back-ref
  FDataSymbol := ADataSymbol;
end;

{ TFpSymbolDwarfDataVariable }

function TFpSymbolDwarfDataVariable.GetValueAddress(AValueObj: TFpValueDwarf; out
  AnAddress: TFpDbgMemLocation): Boolean;
var
  AttrData: TDwarfAttribData;
begin
  if InformationEntry.GetAttribData(DW_AT_location, AttrData) then
    Result := LocationFromAttrData(AttrData, AValueObj, AnAddress, nil, True)
  else
    Result := ConstantFromTag(DW_AT_const_value, FConstData, AnAddress);
end;

function TFpSymbolDwarfDataVariable.HasAddress: Boolean;
begin
  // TODO: THis is wrong. It might allow for the @ operator on a const...
  Result := InformationEntry.HasAttrib(DW_AT_location) or
            InformationEntry.HasAttrib(DW_AT_const_value);
end;

{ TFpSymbolDwarfDataParameter }

function TFpSymbolDwarfDataParameter.GetValueAddress(AValueObj: TFpValueDwarf; out
  AnAddress: TFpDbgMemLocation): Boolean;
begin
  Result := LocationFromTag(DW_AT_location, AValueObj, AnAddress);
end;

function TFpSymbolDwarfDataParameter.HasAddress: Boolean;
begin
  Result := InformationEntry.HasAttrib(DW_AT_location);
end;

function TFpSymbolDwarfDataParameter.GetFlags: TDbgSymbolFlags;
begin
  Result := (inherited GetFlags) + [sfParameter];
end;

{ TFpSymbolDwarfUnit }

procedure TFpSymbolDwarfUnit.Init;
begin
  inherited Init;
  SetSymbolType(stNone);
  SetKind(skUnit);
end;

function TFpSymbolDwarfUnit.GetNestedSymbolByName(AIndex: String): TFpSymbol;
var
  Ident: TDwarfInformationEntry;
begin
  // Todo, param to only search external.
  ReleaseRefAndNil(FLastChildByName);
  Result := nil;

  Ident := InformationEntry.Clone;
  Ident.GoNamedChildEx(AIndex);
  if Ident <> nil then
    Result := TFpSymbolDwarf.CreateSubClass('', Ident);
  // No need to set ParentTypeInfo
  ReleaseRefAndNil(Ident);
  FLastChildByName := Result;
end;

destructor TFpSymbolDwarfUnit.Destroy;
begin
  ReleaseRefAndNil(FLastChildByName);
  inherited Destroy;
end;

initialization
  DwarfSymbolClassMapList.SetDefaultMap(TFpDwarfDefaultSymbolClassMap);

  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );
  FPDBG_DWARF_VERBOSE       := DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_VERBOSE' {$IFDEF FPDBG_DWARF_VERBOSE} , True {$ENDIF} );
  FPDBG_DWARF_ERRORS        := DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_ERRORS' {$IFDEF FPDBG_DWARF_ERRORS} , True {$ENDIF} );
  FPDBG_DWARF_WARNINGS      := DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_WARNINGS' {$IFDEF FPDBG_DWARF_WARNINGS} , True {$ENDIF} );
  FPDBG_DWARF_SEARCH        := DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_SEARCH' {$IFDEF FPDBG_DWARF_SEARCH} , True {$ENDIF} );
  FPDBG_DWARF_DATA_WARNINGS := DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_DATA_WARNINGS' {$IFDEF FPDBG_DWARF_DATA_WARNINGS} , True {$ENDIF} );

end.

