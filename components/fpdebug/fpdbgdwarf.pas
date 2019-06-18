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

  TFpDwarfDefaultSymbolClassMap = class(TFpDwarfSymbolClassMap)
  public
    class function HandleCompUnit(ACU: TDwarfCompilationUnit): Boolean; override;
    class function GetDwarfSymbolClass(ATag: Cardinal): TDbgDwarfSymbolBaseClass; override;
    class function CreateContext(AThreadId, AStackFrame: Integer; AnAddress:
      TDbgPtr; ASymbol: TFpDbgSymbol; ADwarf: TFpDwarfInfo): TFpDbgInfoContext; override;
    class function CreateProcSymbol(ACompilationUnit: TDwarfCompilationUnit;
      AInfo: PDwarfAddressInfo; AAddress: TDbgPtr): TDbgDwarfSymbolBase; override;
  end;

  { TFpDwarfInfoAddressContext }

  TFpDwarfInfoAddressContext = class(TFpDbgInfoContext)
  private
    FSymbol: TFpDbgSymbol;
    FAddress: TDBGPtr;
    FThreadId, FStackFrame: Integer;
    FDwarf: TFpDwarfInfo;
    FlastResult: TFpDbgValue;
  protected
    function GetSymbolAtAddress: TFpDbgSymbol; override;
    function GetProcedureAtAddress: TFpDbgValue; override;
    function GetAddress: TDbgPtr; override;
    function GetThreadId: Integer; override;
    function GetStackFrame: Integer; override;
    function GetSizeOfAddress: Integer; override;
    function GetMemManager: TFpDbgMemManager; override;

    property Symbol: TFpDbgSymbol read FSymbol;
    property Dwarf: TFpDwarfInfo read FDwarf;
    property Address: TDBGPtr read FAddress write FAddress;
    property ThreadId: Integer read FThreadId write FThreadId;
    property StackFrame: Integer read FStackFrame write FStackFrame;

    procedure ApplyContext(AVal: TFpDbgValue); inline;
    function SymbolToValue(ASym: TFpDbgSymbol): TFpDbgValue; inline;
    procedure AddRefToVal(AVal: TFpDbgValue); inline;
    function GetSelfParameter: TFpDbgValue; virtual;

    function FindExportedSymbolInUnits(const AName: String; PNameUpper, PNameLower: PChar;
      SkipCompUnit: TDwarfCompilationUnit; out ADbgValue: TFpDbgValue): Boolean; inline;
    function FindSymbolInStructure(const AName: String; PNameUpper, PNameLower: PChar;
      InfoEntry: TDwarfInformationEntry; out ADbgValue: TFpDbgValue): Boolean; inline;
    // FindLocalSymbol: for the subroutine itself
    function FindLocalSymbol(const AName: String; PNameUpper, PNameLower: PChar;
      InfoEntry: TDwarfInformationEntry; out ADbgValue: TFpDbgValue): Boolean; virtual;
  public
    constructor Create(AThreadId, AStackFrame: Integer; AnAddress: TDbgPtr; ASymbol: TFpDbgSymbol; ADwarf: TFpDwarfInfo);
    destructor Destroy; override;
    function FindSymbol(const AName: String): TFpDbgValue; override;
  end;

  TFpDwarfSymbol = class;
  TFpDwarfSymbolType = class;
  TFpDwarfSymbolValue = class;
  TFpDwarfSymbolValueClass = class of TFpDwarfSymbolValue;
  TFpDwarfSymbolTypeClass = class of TFpDwarfSymbolType;

{%region Value objects }

  { TFpDwarfValueBase }

  TFpDwarfValueBase = class(TFpDbgValue)
  private
    FContext: TFpDbgInfoContext;
  public
    property Context: TFpDbgInfoContext read FContext write FContext;
  end;

  { TFpDwarfValueTypeDefinition }

  TFpDwarfValueTypeDefinition = class(TFpDwarfValueBase)
  private
    FSymbol: TFpDbgSymbol; // stType
  protected
    function GetKind: TDbgSymbolKind; override;
    function GetDbgSymbol: TFpDbgSymbol; override;
  public
    constructor Create(ASymbol: TFpDbgSymbol); // Only for stType
    destructor Destroy; override;
    function GetTypeCastedValue(ADataVal: TFpDbgValue): TFpDbgValue; override;
  end;

  { TFpDwarfValue }

  TFpDwarfValue = class(TFpDwarfValueBase)
  private
    FOwner: TFpDwarfSymbolType;        // the creator, usually the type
    FValueSymbol: TFpDwarfSymbolValue;
    FTypeCastTargetType: TFpDwarfSymbolType;
    FTypeCastSourceValue: TFpDbgValue;

    FDataAddressCache: array of TFpDbgMemLocation;
    FStructureValue: TFpDwarfValue;
    FLastMember: TFpDwarfValue;
    function GetDataAddressCache(AIndex: Integer): TFpDbgMemLocation;
    procedure SetDataAddressCache(AIndex: Integer; AValue: TFpDbgMemLocation);
    procedure SetStructureValue(AValue: TFpDwarfValue);
  protected
    FLastError: TFpError;
    function MemManager: TFpDbgMemManager; inline;
    procedure DoReferenceAdded; override;
    procedure DoReferenceReleased; override;
    procedure CircleBackRefActiveChanged(NewActive: Boolean); override;
    procedure SetLastMember(ALastMember: TFpDwarfValue);
    function GetLastError: TFpError; override;
    function AddressSize: Byte; inline;

    // Address of the symbol (not followed any type deref, or location)
    function GetAddress: TFpDbgMemLocation; override;
    function OrdOrAddress: TFpDbgMemLocation;
    // Address of the data (followed type deref, location, ...)
    function DataAddr: TFpDbgMemLocation;
    function OrdOrDataAddr: TFpDbgMemLocation;
    function GetDwarfDataAddress(out AnAddress: TFpDbgMemLocation; ATargetType: TFpDwarfSymbolType = nil): Boolean;
    function GetStructureDwarfDataAddress(out AnAddress: TFpDbgMemLocation;
                                          ATargetType: TFpDwarfSymbolType = nil): Boolean;
    function HasDwarfDataAddress: Boolean; // TODO: is this just HasAddress?

    procedure Reset; virtual; // keeps lastmember and structureninfo
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function HasTypeCastInfo: Boolean;
    function IsValidTypeCast: Boolean; virtual;
    function GetKind: TDbgSymbolKind; override;
    function GetMemberCount: Integer; override;
    function GetMemberByName(AIndex: String): TFpDbgValue; override;
    function GetMember(AIndex: Int64): TFpDbgValue; override;
    function GetDbgSymbol: TFpDbgSymbol; override;
    function GetTypeInfo: TFpDbgSymbol; override;
    function GetContextTypeInfo: TFpDbgSymbol; override;

    property TypeCastTargetType: TFpDwarfSymbolType read FTypeCastTargetType;
    property TypeCastSourceValue: TFpDbgValue read FTypeCastSourceValue;
  public
    constructor Create(AOwner: TFpDwarfSymbolType);
    destructor Destroy; override;
    procedure SetValueSymbol(AValueSymbol: TFpDwarfSymbolValue);
    function  SetTypeCastInfo(AStructure: TFpDwarfSymbolType;
                              ASource: TFpDbgValue): Boolean; // Used for Typecast
    // StructureValue: Any Value returned via GetMember points to its structure
    property StructureValue: TFpDwarfValue read FStructureValue write SetStructureValue;
    // DataAddressCache[0]: ValueAddress // DataAddressCache[1..n]: DataAddress
    property DataAddressCache[AIndex: Integer]: TFpDbgMemLocation read GetDataAddressCache write SetDataAddressCache;
  end;

  TFpDwarfValueUnknown = class(TFpDwarfValue)
  end;

  { TFpDwarfValueSized }

  TFpDwarfValueSized = class(TFpDwarfValue)
  private
    FSize: Integer;
  protected
    function CanUseTypeCastAddress: Boolean;
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetSize: Integer; override;
  public
    constructor Create(AOwner: TFpDwarfSymbolType; ASize: Integer);
  end;

  { TFpDwarfValueNumeric }

  TFpDwarfValueNumeric = class(TFpDwarfValueSized)
  protected
    FEvaluated: set of (doneUInt, doneInt, doneAddr, doneFloat);
  protected
    procedure Reset; override;
    function GetFieldFlags: TFpDbgValueFieldFlags; override; // svfOrdinal
    function IsValidTypeCast: Boolean; override;
  public
    constructor Create(AOwner: TFpDwarfSymbolType; ASize: Integer);
  end;

  { TFpDwarfValueInteger }

  TFpDwarfValueInteger = class(TFpDwarfValueNumeric)
  private
    FIntValue: Int64;
  protected
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetAsCardinal: QWord; override;
    function GetAsInteger: Int64; override;
  end;

  { TFpDwarfValueCardinal }

  TFpDwarfValueCardinal = class(TFpDwarfValueNumeric)
  private
    FValue: QWord;
  protected
    function GetAsCardinal: QWord; override;
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
  end;

  { TFpDwarfValueFloat }

  TFpDwarfValueFloat = class(TFpDwarfValueNumeric) // TDbgDwarfSymbolValue
  // TODO: typecasts to int should convert
  private
    FValue: Extended;
  protected
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetAsFloat: Extended; override;
  end;

  { TFpDwarfValueBoolean }

  TFpDwarfValueBoolean = class(TFpDwarfValueCardinal)
  protected
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetAsBool: Boolean; override;
  end;

  { TFpDwarfValueChar }

  TFpDwarfValueChar = class(TFpDwarfValueCardinal)
  protected
    // returns single char(byte) / widechar
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetAsString: AnsiString; override;
    function GetAsWideString: WideString; override;
  end;

  { TFpDwarfValuePointer }

  TFpDwarfValuePointer = class(TFpDwarfValueNumeric)
  private
    FLastAddrMember: TFpDbgValue;
    FPointetToAddr: TFpDbgMemLocation;
  protected
    function GetAsCardinal: QWord; override;
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetDataAddress: TFpDbgMemLocation; override;
    function GetAsString: AnsiString; override;
    function GetAsWideString: WideString; override;
    function GetMember(AIndex: Int64): TFpDbgValue; override;
  public
    destructor Destroy; override;
  end;

  { TFpDwarfValueEnum }

  TFpDwarfValueEnum = class(TFpDwarfValueNumeric)
  private
    FValue: QWord;
    FMemberIndex: Integer;
    FMemberValueDone: Boolean;
    procedure InitMemberIndex;
  protected
    procedure Reset; override;
    //function IsValidTypeCast: Boolean; override;
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetAsCardinal: QWord; override;
    function GetAsString: AnsiString; override;
    // Has exactly 0 (if the ordinal value is out of range) or 1 member (the current value's enum)
    function GetMemberCount: Integer; override;
    function GetMember({%H-}AIndex: Int64): TFpDbgValue; override;
  end;

  { TFpDwarfValueEnumMember }

  TFpDwarfValueEnumMember = class(TFpDwarfValue)
  private
    FOwnerVal: TFpDwarfSymbolValue;
  protected
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetAsCardinal: QWord; override;
    function GetAsString: AnsiString; override;
    function IsValidTypeCast: Boolean; override;
  public
    constructor Create(AOwner: TFpDwarfSymbolValue);
  end;

  { TFpDwarfValueConstNumber }

  TFpDwarfValueConstNumber = class(TFpDbgValueConstNumber)
  protected
    procedure Update(AValue: QWord; ASigned: Boolean);
  end;

  { TFpDwarfValueSet }

  TFpDwarfValueSet = class(TFpDwarfValueSized)
  private
    FMem: array of Byte;
    FMemberCount: Integer;
    FMemberMap: array of Integer;
    FNumValue: TFpDwarfValueConstNumber;
    FTypedNumValue: TFpDbgValue;
    procedure InitMap;
  protected
    procedure Reset; override;
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetMemberCount: Integer; override;
    function GetMember(AIndex: Int64): TFpDbgValue; override;
    function GetAsCardinal: QWord; override; // only up to qmord
    function IsValidTypeCast: Boolean; override;
  public
    destructor Destroy; override;
  end;

  { TFpDwarfValueStruct }

  TFpDwarfValueStruct = class(TFpDwarfValue)
  private
    FDataAddress: TFpDbgMemLocation;
    FDataAddressDone: Boolean;
  protected
    procedure Reset; override;
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetAsCardinal: QWord; override;
    function GetDataAddress: TFpDbgMemLocation; override;
    function GetDataSize: Integer; override;
    function GetSize: Integer; override;
  end;

  { TFpDwarfValueStructTypeCast }

  TFpDwarfValueStructTypeCast = class(TFpDwarfValue)
  private
    FMembers: TFpDbgCircularRefCntObjList;
    FDataAddress: TFpDbgMemLocation;
    FDataAddressDone: Boolean;
  protected
    procedure Reset; override;
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetKind: TDbgSymbolKind; override;
    function GetAsCardinal: QWord; override;
    function GetSize: Integer; override;
    function GetDataSize: Integer; override;
    function GetDataAddress: TFpDbgMemLocation; override;
    function IsValidTypeCast: Boolean; override;
  public
    destructor Destroy; override;
    function GetMemberByName(AIndex: String): TFpDbgValue; override;
    function GetMember(AIndex: Int64): TFpDbgValue; override;
    function GetMemberCount: Integer; override;
  end;

  { TFpDwarfValueConstAddress }

  TFpDwarfValueConstAddress = class(TFpDbgValueConstAddress)
  protected
    procedure Update(AnAddress: TFpDbgMemLocation);
  end;

  { TFpDwarfValueArray }

  TFpDwarfValueArray = class(TFpDwarfValue)
  private
    FAddrObj: TFpDwarfValueConstAddress;
  protected
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetKind: TDbgSymbolKind; override;
    function GetAsCardinal: QWord; override;
    function GetDataAddress: TFpDbgMemLocation; override;
    function GetMember(AIndex: Int64): TFpDbgValue; override;
    function GetMemberEx(const AIndex: array of Int64): TFpDbgValue; override;
    function GetMemberCount: Integer; override;
    function GetMemberCountEx(const AIndex: array of Int64): Integer; override;
    function GetIndexType(AIndex: Integer): TFpDbgSymbol; override;
    function GetIndexTypeCount: Integer; override;
    function IsValidTypeCast: Boolean; override;
  public
    destructor Destroy; override;
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

  { TDbgDwarfIdentifier }

  { TFpDwarfSymbol }

  TFpDwarfSymbol = class(TDbgDwarfSymbolBase)
  private
    FNestedTypeInfo: TFpDwarfSymbolType;
    FParentTypeInfo: TFpDwarfSymbol;
    FDwarfReadFlags: set of (didtNameRead, didtTypeRead, didtArtificialRead, didtIsArtifical);
    function GetNestedTypeInfo: TFpDwarfSymbolType;
  protected
    (* There will be a circular reference between parenttype and self
       "self" will only set its reference to parenttype, if self has other references.  *)
    procedure DoReferenceAdded; override;
    procedure DoReferenceReleased; override;
    procedure CircleBackRefActiveChanged(ANewActive: Boolean); override;
    procedure SetParentTypeInfo(AValue: TFpDwarfSymbol); virtual;

    function  DoGetNestedTypeInfo: TFpDwarfSymbolType; virtual;
    function  ReadMemberVisibility(out AMemberVisibility: TDbgSymbolMemberVisibility): Boolean;
    function  IsArtificial: Boolean; // usud by formal param and subprogram
    procedure NameNeeded; override;
    procedure TypeInfoNeeded; override;
    property NestedTypeInfo: TFpDwarfSymbolType read GetNestedTypeInfo;

    // OwnerTypeInfo: reverse of "NestedTypeInfo" (variable that is of this type)
//    property OwnerTypeInfo: TDbgDwarfIdentifier read FOwnerTypeInfo; // write SetOwnerTypeInfo;
    // ParentTypeInfo: funtion for local var / class for member
    property ParentTypeInfo: TFpDwarfSymbol read FParentTypeInfo write SetParentTypeInfo;

    function DataSize: Integer; virtual;
  protected
    function InitLocationParser(const {%H-}ALocationParser: TDwarfLocationExpression;
                                AnInitLocParserData: PInitLocParserData = nil): Boolean; virtual;
    function  LocationFromTag(ATag: Cardinal; const AnAttribData: TDwarfAttribData; AValueObj: TFpDwarfValue;
                              var AnAddress: TFpDbgMemLocation; // kept, if tag does not exist
                              AnInitLocParserData: PInitLocParserData = nil
                             ): Boolean;
    function  LocationFromTag(ATag: Cardinal; AValueObj: TFpDwarfValue;
                              var AnAddress: TFpDbgMemLocation; // kept, if tag does not exist
                              AnInitLocParserData: PInitLocParserData = nil;
                              AnInformationEntry: TDwarfInformationEntry = nil;
                              ASucessOnMissingTag: Boolean = False
                             ): Boolean; // deprecated
    function  ConstantFromTag(ATag: Cardinal; out AConstData: TByteDynArray;
                              var AnAddress: TFpDbgMemLocation; // kept, if tag does not exist
                              AnInformationEntry: TDwarfInformationEntry = nil;
                              ASucessOnMissingTag: Boolean = False
                             ): Boolean;
    // GetDataAddress: data of a class, or string
    function GetDataAddress(AValueObj: TFpDwarfValue; var AnAddress: TFpDbgMemLocation;
                            ATargetType: TFpDwarfSymbolType; ATargetCacheIndex: Integer): Boolean;
    function GetDataAddressNext(AValueObj: TFpDwarfValue; var AnAddress: TFpDbgMemLocation;
                            ATargetType: TFpDwarfSymbolType; ATargetCacheIndex: Integer): Boolean; virtual;
    function HasAddress: Boolean; virtual;

    procedure Init; override;
  public
    class function CreateSubClass(AName: String; AnInformationEntry: TDwarfInformationEntry): TFpDwarfSymbol;
    destructor Destroy; override;
    function StartScope: TDbgPtr; // return 0, if none. 0 includes all anyway
  end;

  { TFpDwarfSymbolValue }

  TFpDwarfSymbolValue = class(TFpDwarfSymbol) // var, const, member, ...
  protected
    FValueObject: TFpDwarfValue;
    FMembers: TFpDbgCircularRefCntObjList;

    function GetValueAddress({%H-}AValueObj: TFpDwarfValue;{%H-} out AnAddress: TFpDbgMemLocation): Boolean; virtual;
    function GetValueDataAddress(AValueObj: TFpDwarfValue; out AnAddress: TFpDbgMemLocation;
                                 ATargetType: TFpDwarfSymbolType = nil): Boolean;
    procedure KindNeeded; override;
    procedure MemberVisibilityNeeded; override;
    function GetMember(AIndex: Int64): TFpDbgSymbol; override;
    function GetMemberByName(AIndex: String): TFpDbgSymbol; override;
    function GetMemberCount: Integer; override;

    procedure Init; override;
  public
    destructor Destroy; override;
    class function CreateValueSubClass(AName: String; AnInformationEntry: TDwarfInformationEntry): TFpDwarfSymbolValue;
  end;

  { TFpDwarfSymbolValueWithLocation }

  TFpDwarfSymbolValueWithLocation = class(TFpDwarfSymbolValue)
  private
    procedure FrameBaseNeeded(ASender: TObject); // Sender = TDwarfLocationExpression
  protected
    function GetValueObject: TFpDbgValue; override;
    function InitLocationParser(const ALocationParser: TDwarfLocationExpression;
                                AnInitLocParserData: PInitLocParserData): Boolean; override;
  end;

  { TFpDwarfSymbolType }

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

  TFpDwarfSymbolType = class(TFpDwarfSymbol)
  protected
    procedure Init; override;
    procedure MemberVisibilityNeeded; override;
    procedure SizeNeeded; override;
    function GetTypedValueObject({%H-}ATypeCast: Boolean): TFpDwarfValue; virtual; // returns refcount=1 for caller, no cached copy kept
  public
    class function CreateTypeSubClass(AName: String; AnInformationEntry: TDwarfInformationEntry): TFpDwarfSymbolType;
    function TypeCastValue(AValue: TFpDbgValue): TFpDbgValue; override;
    // TODO: flag bounds as cardinal if needed
    function GetValueBounds({%H-}AValueObj: TFpDwarfValue; out ALowBound, AHighBound: Int64): Boolean; virtual;

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

  { TFpDwarfSymbolTypeBasic }

  TFpDwarfSymbolTypeBasic = class(TFpDwarfSymbolType)
  //function DoGetNestedTypeInfo: TFpDwarfSymbolType; // return nil
  protected
    procedure KindNeeded; override;
    procedure TypeInfoNeeded; override;
    function GetTypedValueObject({%H-}ATypeCast: Boolean): TFpDwarfValue; override;
    function GetHasBounds: Boolean; override;
    function GetOrdHighBound: Int64; override;
    function GetOrdLowBound: Int64; override;
  end;

  { TFpDwarfSymbolTypeModifier }

  TFpDwarfSymbolTypeModifier = class(TFpDwarfSymbolType)
  protected
    procedure TypeInfoNeeded; override;
    procedure ForwardToSymbolNeeded; override;
    function GetTypedValueObject(ATypeCast: Boolean): TFpDwarfValue; override;
  end;

  { TFpDwarfSymbolTypeRef }

  TFpDwarfSymbolTypeRef = class(TFpDwarfSymbolTypeModifier)
  protected
    function GetFlags: TDbgSymbolFlags; override;
    function GetDataAddressNext(AValueObj: TFpDwarfValue; var AnAddress: TFpDbgMemLocation;
                            ATargetType: TFpDwarfSymbolType; ATargetCacheIndex: Integer): Boolean; override;
  end;

  { TFpDwarfSymbolTypeDeclaration }

  TFpDwarfSymbolTypeDeclaration = class(TFpDwarfSymbolTypeModifier)
  protected
    // fpc encodes classes as pointer, not ref (so Obj1 = obj2 compares the pointers)
    // typedef > pointer > srtuct
    // while a pointer to class/object: pointer > typedef > ....
    function DoGetNestedTypeInfo: TFpDwarfSymbolType; override;
  end;

  { TFpDwarfSymbolTypeSubRange }
  TFpDwarfSubRangeBoundReadState = (rfNotRead, rfNotFound, rfConst, rfValue);

  TFpDwarfSymbolTypeSubRange = class(TFpDwarfSymbolTypeModifier)
  // TODO not a modifier, maybe have a forwarder base class
  private
    FLowBoundConst: Int64;
    FLowBoundValue: TFpDwarfSymbolValue;
    FLowBoundState: TFpDwarfSubRangeBoundReadState;
    FHighBoundConst: Int64;
    FHighBoundValue: TFpDwarfSymbolValue;
    FHighBoundState: TFpDwarfSubRangeBoundReadState;
    FCountConst: Int64;
    FCountValue: TFpDwarfSymbolValue;
    FCountState: TFpDwarfSubRangeBoundReadState;
    FLowEnumIdx, FHighEnumIdx: Integer;
    FEnumIdxValid: Boolean;
    procedure InitEnumIdx;
    procedure ReadBounds(AValueObj: TFpDwarfValue);
  protected
    function DoGetNestedTypeInfo: TFpDwarfSymbolType;override;
    function GetHasBounds: Boolean; override;
    function GetOrdHighBound: Int64; override;
    function GetOrdLowBound: Int64; override;

    procedure NameNeeded; override;
    procedure KindNeeded; override;
    procedure SizeNeeded; override;
    function GetMember(AIndex: Int64): TFpDbgSymbol; override;
    function GetMemberCount: Integer; override;
    function GetFlags: TDbgSymbolFlags; override;
    procedure Init; override;
  public
    function GetValueBounds(AValueObj: TFpDwarfValue; out ALowBound,
      AHighBound: Int64): Boolean; override;
    procedure ResetValueBounds; override;
  end;

  { TFpDwarfSymbolTypePointer }

  TFpDwarfSymbolTypePointer = class(TFpDwarfSymbolType)
  private
    FIsInternalPointer: Boolean;
    function GetIsInternalPointer: Boolean; inline;
    function IsInternalDynArrayPointer: Boolean; inline;
  protected
    procedure TypeInfoNeeded; override;
    procedure KindNeeded; override;
    procedure SizeNeeded; override;
    procedure ForwardToSymbolNeeded; override;
    function GetDataAddressNext(AValueObj: TFpDwarfValue; var AnAddress: TFpDbgMemLocation;
                            ATargetType: TFpDwarfSymbolType; ATargetCacheIndex: Integer): Boolean; override;
    function GetTypedValueObject(ATypeCast: Boolean): TFpDwarfValue; override;
    function DataSize: Integer; override;
  public
    property IsInternalPointer: Boolean read GetIsInternalPointer write FIsInternalPointer; // Class (also DynArray, but DynArray is handled without this)
  end;

  { TFpDwarfSymbolValueEnumMember }

  TFpDwarfSymbolValueEnumMember  = class(TFpDwarfSymbolValue)
    FOrdinalValue: Int64;
    FOrdinalValueRead, FHasOrdinalValue: Boolean;
    procedure ReadOrdinalValue;
  protected
    procedure KindNeeded; override;
    function GetHasOrdinalValue: Boolean; override;
    function GetOrdinalValue: Int64; override;
    procedure Init; override;
    function GetValueObject: TFpDbgValue; override;
  end;


  { TFpDwarfSymbolTypeEnum }

  TFpDwarfSymbolTypeEnum = class(TFpDwarfSymbolType)
  private
    FMembers: TFpDbgCircularRefCntObjList;
    procedure CreateMembers;
  protected
    function GetTypedValueObject({%H-}ATypeCast: Boolean): TFpDwarfValue; override;
    procedure KindNeeded; override;
    function GetMember(AIndex: Int64): TFpDbgSymbol; override;
    function GetMemberByName(AIndex: String): TFpDbgSymbol; override;
    function GetMemberCount: Integer; override;

    function GetHasBounds: Boolean; override;
    function GetOrdHighBound: Int64; override;
    function GetOrdLowBound: Int64; override;
  public
    destructor Destroy; override;
  end;


  { TFpDwarfSymbolTypeSet }

  TFpDwarfSymbolTypeSet = class(TFpDwarfSymbolType)
  protected
    procedure KindNeeded; override;
    function GetTypedValueObject({%H-}ATypeCast: Boolean): TFpDwarfValue; override;
    function GetMemberCount: Integer; override;
    function GetMember(AIndex: Int64): TFpDbgSymbol; override;
  end;

  (*
    If not specified
         .NestedTypeInfo --> copy of TypeInfo
         .ParentTypeInfo --> nil

    ParentTypeInfo:     has a weak RefCount (only AddRef, if self has other refs)


    AnObject = TFpDwarfSymbolValueVariable
     |-- .TypeInfo       --> TBar = TFpDwarfSymbolTypeStructure  [*1]
     |-- .ParentTypeInfo --> may point to subroutine, if param or local var // TODO

    TBar = TFpDwarfSymbolTypeStructure
     |-- .TypeInfo       --> TBarBase = TFpDwarfSymbolTypeStructure

    TBarBase = TFpDwarfSymbolTypeStructure
     |-- .TypeInfo       --> TOBject = TFpDwarfSymbolTypeStructure

    TObject = TFpDwarfSymbolTypeStructure
     |-- .TypeInfo       --> nil


    FField = TFpDwarfSymbolValueMember (declared in TBarBase)
     |-- .TypeInfo       --> Integer = TFpDwarfSymbolTypeBasic [*1]
     |-- .ParentTypeInfo --> TBarBase

    [*1] May have TFpDwarfSymbolTypeDeclaration or others
  *)

  { TFpDwarfSymbolValueMember }

  TFpDwarfSymbolValueMember = class(TFpDwarfSymbolValueWithLocation)
  protected
    function GetValueAddress(AValueObj: TFpDwarfValue; out AnAddress: TFpDbgMemLocation): Boolean; override;
    function HasAddress: Boolean; override;
  end;

  { TFpDwarfSymbolTypeStructure }

  TFpDwarfSymbolTypeStructure = class(TFpDwarfSymbolType)
  // record or class
  private
    FMembers: TFpDbgCircularRefCntObjList;
    FLastChildByName: TFpDwarfSymbol;
    FInheritanceInfo: TDwarfInformationEntry;
    procedure CreateMembers;
    procedure InitInheritanceInfo; inline;
  protected
    function DoGetNestedTypeInfo: TFpDwarfSymbolType; override;
    procedure KindNeeded; override;
    function GetTypedValueObject(ATypeCast: Boolean): TFpDwarfValue; override;

    // GetMember, if AIndex > Count then parent
    function GetMember(AIndex: Int64): TFpDbgSymbol; override;
    function GetMemberByName(AIndex: String): TFpDbgSymbol; override;
    function GetMemberCount: Integer; override;

    function GetDataAddressNext(AValueObj: TFpDwarfValue; var AnAddress: TFpDbgMemLocation;
                            ATargetType: TFpDwarfSymbolType; ATargetCacheIndex: Integer): Boolean; override;
  public
    destructor Destroy; override;
  end;

  { TFpDwarfSymbolTypeArray }

  TFpDwarfSymbolTypeArray = class(TFpDwarfSymbolType)
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
    function GetTypedValueObject({%H-}ATypeCast: Boolean): TFpDwarfValue; override;

    function GetFlags: TDbgSymbolFlags; override;
    // GetMember: returns the TYPE/range of each index. NOT the data
    function GetMember(AIndex: Int64): TFpDbgSymbol; override;
    function GetMemberByName({%H-}AIndex: String): TFpDbgSymbol; override;
    function GetMemberCount: Integer; override;
    function GetMemberAddress(AValObject: TFpDwarfValue; const AIndex: Array of Int64): TFpDbgMemLocation;
  public
    destructor Destroy; override;
    procedure ResetValueBounds; override;
  end;

  { TFpDwarfSymbolValueProc }

  TFpDwarfSymbolValueProc = class(TFpDwarfSymbolValue)
  private
    //FCU: TDwarfCompilationUnit;
    FProcMembers: TRefCntObjList; // Locals
    FLastMember: TFpDbgSymbol;
    FAddress: TDbgPtr;
    FAddressInfo: PDwarfAddressInfo;
    FStateMachine: TDwarfLineInfoStateMachine;
    FFrameBaseParser: TDwarfLocationExpression;
    FSelfParameter: TFpDwarfValue;
    function StateMachineValid: Boolean;
    function  ReadVirtuality(out AFlags: TDbgSymbolFlags): Boolean;
    procedure CreateMembers;
  protected
    function GetMember(AIndex: Int64): TFpDbgSymbol; override;
    function GetMemberByName(AIndex: String): TFpDbgSymbol; override;
    function GetMemberCount: Integer; override;

    function  GetFrameBase(ASender: TDwarfLocationExpression): TDbgPtr;
    procedure KindNeeded; override;
    procedure SizeNeeded; override;
    function GetFlags: TDbgSymbolFlags; override;

    function GetColumn: Cardinal; override;
    function GetFile: String; override;
//    function GetFlags: TDbgSymbolFlags; override;
    function GetLine: Cardinal; override;
    function GetValueObject: TFpDbgValue; override;
  public
    constructor Create(ACompilationUnit: TDwarfCompilationUnit; AInfo: PDwarfAddressInfo; AAddress: TDbgPtr); overload;
    destructor Destroy; override;
    // TODO members = locals ?
    function GetSelfParameter(AnAddress: TDbgPtr = 0): TFpDwarfValue;
  end;

  { TFpDwarfSymbolValueVariable }

  TFpDwarfSymbolValueVariable = class(TFpDwarfSymbolValueWithLocation)
  private
    FConstData: TByteDynArray;
  protected
    function GetValueAddress(AValueObj: TFpDwarfValue; out AnAddress: TFpDbgMemLocation): Boolean; override;
    function HasAddress: Boolean; override;
  public
  end;

  { TFpDwarfSymbolValueParameter }

  TFpDwarfSymbolValueParameter = class(TFpDwarfSymbolValueWithLocation)
  protected
    function GetValueAddress(AValueObj: TFpDwarfValue; out AnAddress: TFpDbgMemLocation): Boolean; override;
    function HasAddress: Boolean; override;
    function GetFlags: TDbgSymbolFlags; override;
  public
  end;

  { TFpDwarfSymbolUnit }

  TFpDwarfSymbolUnit = class(TFpDwarfSymbol)
  private
    FLastChildByName: TFpDbgSymbol;
  protected
    procedure Init; override;
    function GetMemberByName(AIndex: String): TFpDbgSymbol; override;
  public
    destructor Destroy; override;
  end;
{%endregion Symbol objects }

implementation

var
  FPDBG_DWARF_VERBOSE, FPDBG_DWARF_ERRORS, FPDBG_DWARF_WARNINGS, FPDBG_DWARF_SEARCH, FPDBG_DWARF_DATA_WARNINGS: PLazLoggerLogGroup;

{ TFpDwarfDefaultSymbolClassMap }

class function TFpDwarfDefaultSymbolClassMap.HandleCompUnit(ACU: TDwarfCompilationUnit): Boolean;
begin
  Result := True;
end;

class function TFpDwarfDefaultSymbolClassMap.GetDwarfSymbolClass(ATag: Cardinal): TDbgDwarfSymbolBaseClass;
begin
  case ATag of
    // TODO:
    DW_TAG_constant:
      Result := TFpDwarfSymbolValue;
    DW_TAG_string_type,
    DW_TAG_union_type, DW_TAG_ptr_to_member_type,
    DW_TAG_file_type,
    DW_TAG_thrown_type, DW_TAG_subroutine_type:
      Result := TFpDwarfSymbolType;

    // Type types
    DW_TAG_packed_type,
    DW_TAG_const_type,
    DW_TAG_volatile_type:    Result := TFpDwarfSymbolTypeModifier;
    DW_TAG_reference_type:   Result := TFpDwarfSymbolTypeRef;
    DW_TAG_typedef:          Result := TFpDwarfSymbolTypeDeclaration;
    DW_TAG_pointer_type:     Result := TFpDwarfSymbolTypePointer;

    DW_TAG_base_type:        Result := TFpDwarfSymbolTypeBasic;
    DW_TAG_subrange_type:    Result := TFpDwarfSymbolTypeSubRange;
    DW_TAG_enumeration_type: Result := TFpDwarfSymbolTypeEnum;
    DW_TAG_enumerator:       Result := TFpDwarfSymbolValueEnumMember;
    DW_TAG_set_type:         Result := TFpDwarfSymbolTypeSet;
    DW_TAG_structure_type,
    DW_TAG_class_type:       Result := TFpDwarfSymbolTypeStructure;
    DW_TAG_array_type:       Result := TFpDwarfSymbolTypeArray;
    // Value types
    DW_TAG_variable:         Result := TFpDwarfSymbolValueVariable;
    DW_TAG_formal_parameter: Result := TFpDwarfSymbolValueParameter;
    DW_TAG_member:           Result := TFpDwarfSymbolValueMember;
    DW_TAG_subprogram:       Result := TFpDwarfSymbolValueProc;
    //
    DW_TAG_compile_unit:     Result := TFpDwarfSymbolUnit;

    else
      Result := TFpDwarfSymbol;
  end;
end;

class function TFpDwarfDefaultSymbolClassMap.CreateContext(AThreadId, AStackFrame: Integer;
  AnAddress: TDbgPtr; ASymbol: TFpDbgSymbol; ADwarf: TFpDwarfInfo): TFpDbgInfoContext;
begin
  Result := TFpDwarfInfoAddressContext.Create(AThreadId, AStackFrame, AnAddress, ASymbol, ADwarf);
end;

class function TFpDwarfDefaultSymbolClassMap.CreateProcSymbol(ACompilationUnit: TDwarfCompilationUnit;
  AInfo: PDwarfAddressInfo; AAddress: TDbgPtr): TDbgDwarfSymbolBase;
begin
  Result := TFpDwarfSymbolValueProc.Create(ACompilationUnit, AInfo, AAddress);
end;

{ TDbgDwarfInfoAddressContext }

function TFpDwarfInfoAddressContext.GetSymbolAtAddress: TFpDbgSymbol;
begin
  Result := FSymbol;
end;

function TFpDwarfInfoAddressContext.GetProcedureAtAddress: TFpDbgValue;
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
  assert(FSymbol is TFpDwarfSymbol, 'TDbgDwarfInfoAddressContext.GetSizeOfAddress');
  Result := TFpDwarfSymbol(FSymbol).CompilationUnit.AddressSize;
end;

function TFpDwarfInfoAddressContext.GetMemManager: TFpDbgMemManager;
begin
  Result := FDwarf.MemManager;
end;

procedure TFpDwarfInfoAddressContext.ApplyContext(AVal: TFpDbgValue);
begin
  if (AVal <> nil) and (TFpDwarfValueBase(AVal).FContext = nil) then
    TFpDwarfValueBase(AVal).FContext := Self;
end;

function TFpDwarfInfoAddressContext.SymbolToValue(ASym: TFpDbgSymbol): TFpDbgValue;
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
    Result := TFpDwarfValueTypeDefinition.Create(ASym);
    {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(@FlastResult, 'FindSymbol'){$ENDIF};
  end;
  ASym.ReleaseReference;
end;

procedure TFpDwarfInfoAddressContext.AddRefToVal(AVal: TFpDbgValue);
begin
  AVal.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FlastResult, 'FindSymbol'){$ENDIF};
end;

function TFpDwarfInfoAddressContext.GetSelfParameter: TFpDbgValue;
begin
  Result := TFpDwarfSymbolValueProc(FSymbol).GetSelfParameter(FAddress);
  if (Result <> nil) and (TFpDwarfValueBase(Result).FContext = nil) then
    TFpDwarfValueBase(Result).FContext := Self;
end;

function TFpDwarfInfoAddressContext.FindExportedSymbolInUnits(const AName: String; PNameUpper,
  PNameLower: PChar; SkipCompUnit: TDwarfCompilationUnit; out ADbgValue: TFpDbgValue): Boolean;
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
      ADbgValue := SymbolToValue(TFpDwarfSymbol.CreateSubClass(AName, InfoEntry));
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
    ADbgValue := SymbolToValue(TFpDwarfSymbol.CreateSubClass(AName, FoundInfoEntry));
    FoundInfoEntry.ReleaseReference;
  end;

  InfoEntry.ReleaseReference;
  Result := ADbgValue <> nil;
end;

function TFpDwarfInfoAddressContext.FindSymbolInStructure(const AName: String; PNameUpper,
  PNameLower: PChar; InfoEntry: TDwarfInformationEntry; out ADbgValue: TFpDbgValue): Boolean;
var
  InfoEntryInheritance: TDwarfInformationEntry;
  FwdInfoPtr: Pointer;
  FwdCompUint: TDwarfCompilationUnit;
  SelfParam: TFpDbgValue;
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
          ADbgValue := SymbolToValue(TFpDwarfSymbol.CreateSubClass(AName, InfoEntry));
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
  PNameLower: PChar; InfoEntry: TDwarfInformationEntry; out ADbgValue: TFpDbgValue): Boolean;
begin
  Result := False;
  ADbgValue := nil;
  if not InfoEntry.GoNamedChildEx(PNameUpper, PNameLower) then
    exit;
  if InfoEntry.IsAddressInStartScope(FAddress) and not InfoEntry.IsArtificial then begin
    ADbgValue := SymbolToValue(TFpDwarfSymbol.CreateSubClass(AName, InfoEntry));
    if ADbgValue <> nil then
      TFpDwarfSymbol(ADbgValue.DbgSymbol).ParentTypeInfo := TFpDwarfSymbolValueProc(FSymbol);
  end;
  Result := ADbgValue <> nil;
end;

constructor TFpDwarfInfoAddressContext.Create(AThreadId, AStackFrame: Integer;
  AnAddress: TDbgPtr; ASymbol: TFpDbgSymbol; ADwarf: TFpDwarfInfo);
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

function TFpDwarfInfoAddressContext.FindSymbol(const AName: String): TFpDbgValue;
var
  SubRoutine: TFpDwarfSymbolValueProc; // TDbgSymbol;
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
  if (FSymbol = nil) or not(FSymbol is TFpDwarfSymbolValueProc) or (AName = '') then
    exit;

  SubRoutine := TFpDwarfSymbolValueProc(FSymbol);
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
              Result := SymbolToValue(TFpDwarfSymbol.CreateSubClass(AName, InfoEntry));
              exit;
            end;
          end;

          InfoEntry.ScopeIndex := StartScopeIdx;
          Result := SymbolToValue(TFpDwarfSymbol.CreateSubClass(AName, InfoEntry));
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
          Result := SymbolToValue(TFpDwarfSymbol.CreateSubClass(AName, InfoEntry));
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
    else DebugLn(FPDBG_DWARF_SEARCH, ['TDbgDwarf.FindIdentifier(',AName,') found Scope=', TFpDwarfSymbol(Result.DbgSymbol).InformationEntry.ScopeDebugText, '  ResultSymbol=', DbgSName(Result.DbgSymbol), ' ', Result.DbgSymbol.Name, ' in ', TFpDwarfSymbol(Result.DbgSymbol).CompilationUnit.FileName]);
    ReleaseRefAndNil(InfoEntry);

    FlastResult.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FlastResult, 'FindSymbol'){$ENDIF};
    FlastResult := Result;

    assert((Result = nil) or (Result is TFpDwarfValueBase), 'TDbgDwarfInfoAddressContext.FindSymbol: (Result = nil) or (Result is TFpDwarfValueBase)');
    ApplyContext(Result);
  end;
end;

{ TFpDwarfValueTypeDefinition }

function TFpDwarfValueTypeDefinition.GetKind: TDbgSymbolKind;
begin
  Result := skNone;
end;

function TFpDwarfValueTypeDefinition.GetDbgSymbol: TFpDbgSymbol;
begin
  Result := FSymbol;
end;

constructor TFpDwarfValueTypeDefinition.Create(ASymbol: TFpDbgSymbol);
begin
  inherited Create;
  FSymbol := ASymbol;
  FSymbol.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSymbol, 'TFpDwarfValueTypeDefinition'){$ENDIF};
end;

destructor TFpDwarfValueTypeDefinition.Destroy;
begin
  inherited Destroy;
  FSymbol.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSymbol, 'TFpDwarfValueTypeDefinition'){$ENDIF};
end;

function TFpDwarfValueTypeDefinition.GetTypeCastedValue(ADataVal: TFpDbgValue): TFpDbgValue;
begin
  Result := FSymbol.TypeCastValue(ADataVal);
  assert((Result = nil) or (Result is TFpDwarfValue), 'TFpDwarfValueTypeDefinition.GetTypeCastedValue: (Result = nil) or (Result is TFpDwarfValue)');
  if (Result <> nil) and (TFpDwarfValue(Result).FContext = nil) then
    TFpDwarfValue(Result).FContext := FContext;
end;

{ TFpDwarfValue }

function TFpDwarfValue.MemManager: TFpDbgMemManager;
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

function TFpDwarfValue.GetDataAddressCache(AIndex: Integer): TFpDbgMemLocation;
begin
  if AIndex < Length(FDataAddressCache) then
    Result := FDataAddressCache[AIndex]
  else
    Result := UnInitializedLoc;
end;

function TFpDwarfValue.AddressSize: Byte;
begin
  assert((FOwner <> nil) and (FOwner.CompilationUnit <> nil), 'TDbgDwarfSymbolValue.AddressSize');
  Result := FOwner.CompilationUnit.AddressSize;
end;

procedure TFpDwarfValue.SetDataAddressCache(AIndex: Integer; AValue: TFpDbgMemLocation);
var
  i, j: Integer;
begin
  i := length(FDataAddressCache);
  if AIndex >= i then begin
    SetLength(FDataAddressCache, AIndex + 1 + 8);
    // todo: Fillbyte 0
    for j := i to Length(FDataAddressCache) - 1 do
      FDataAddressCache[j] := UnInitializedLoc;
  end;
  FDataAddressCache[AIndex] := AValue;
end;

procedure TFpDwarfValue.SetStructureValue(AValue: TFpDwarfValue);
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

function TFpDwarfValue.GetLastError: TFpError;
begin
  Result := FLastError;
end;

function TFpDwarfValue.DataAddr: TFpDbgMemLocation;
begin
  // GetDwarfDataAddress(???); What about FTypeCastSourceValue.AsCardinal ?
  if FValueSymbol <> nil then begin
    //FValueSymbol.GetValueAddress(Self, Result);
    FValueSymbol.GetValueDataAddress(Self, Result, FOwner);
    if IsError(FValueSymbol.LastError) then
      FLastError := FValueSymbol.LastError;
  end
  else
  if HasTypeCastInfo then begin
    Result := FTypeCastSourceValue.Address;
    if IsError(FTypeCastSourceValue.LastError) then
      FLastError := FTypeCastSourceValue.LastError;

    if IsReadableLoc(Result) then begin
      if not FTypeCastTargetType.GetDataAddress(Self, Result, FOwner, 1) then
        Result := InvalidLoc;
      if IsError(FTypeCastTargetType.LastError) then
        FLastError := FTypeCastTargetType.LastError;
    end;
  end
  else
    Result := InvalidLoc;
end;

function TFpDwarfValue.OrdOrDataAddr: TFpDbgMemLocation;
begin
  if HasTypeCastInfo and (svfOrdinal in FTypeCastSourceValue.FieldFlags) then
    Result := ConstLoc(FTypeCastSourceValue.AsCardinal)
  else
    Result := DataAddr;
end;

function TFpDwarfValue.GetDwarfDataAddress(out AnAddress: TFpDbgMemLocation;
  ATargetType: TFpDwarfSymbolType): Boolean;
var
  fields: TFpDbgValueFieldFlags;
begin
  if FValueSymbol <> nil then begin
    Assert(FValueSymbol is TFpDwarfSymbolValue, 'TDbgDwarfSymbolValue.GetDwarfDataAddress FValueSymbol');
    Assert(TypeInfo is TFpDwarfSymbolType, 'TDbgDwarfSymbolValue.GetDwarfDataAddress TypeInfo');
    Assert(not HasTypeCastInfo, 'TDbgDwarfSymbolValue.GetDwarfDataAddress not HasTypeCastInfo');
    Result := FValueSymbol.GetValueDataAddress(Self, AnAddress, ATargetType);
    if IsError(FValueSymbol.LastError) then
      FLastError := FValueSymbol.LastError;
  end

  else
  begin
    // TODO: cache own address
    // try typecast
    Result := HasTypeCastInfo;
    if not Result then
      exit;
    fields := FTypeCastSourceValue.FieldFlags;
    AnAddress := InvalidLoc;
    if svfOrdinal in fields then
      AnAddress := ConstLoc(FTypeCastSourceValue.AsCardinal)
    else
    if svfAddress in fields then
      AnAddress := FTypeCastSourceValue.Address;

    Result := IsReadableLoc(AnAddress);
    if not Result then
      exit;

    Result := FTypeCastTargetType.GetDataAddress(Self, AnAddress, ATargetType, 1);
    if IsError(FTypeCastTargetType.LastError) then
      FLastError := FTypeCastTargetType.LastError;
  end;
end;

function TFpDwarfValue.GetStructureDwarfDataAddress(out AnAddress: TFpDbgMemLocation;
  ATargetType: TFpDwarfSymbolType): Boolean;
begin
  AnAddress := InvalidLoc;
  Result := StructureValue <> nil;
  if Result then
    Result := StructureValue.GetDwarfDataAddress(AnAddress, ATargetType);
end;

function TFpDwarfValue.HasDwarfDataAddress: Boolean;
begin
  if FValueSymbol <> nil then begin
    Assert(FValueSymbol is TFpDwarfSymbolValue, 'TDbgDwarfSymbolValue.GetDwarfDataAddress FValueSymbol');
    Assert(TypeInfo is TFpDwarfSymbolType, 'TDbgDwarfSymbolValue.GetDwarfDataAddress TypeInfo');
    Assert(not HasTypeCastInfo, 'TDbgDwarfSymbolValue.GetDwarfDataAddress not HasTypeCastInfo');
    Result := FValueSymbol.HasAddress;
  end
  else
  begin
    // try typecast
    Result := HasTypeCastInfo;
    if not Result then
      exit;
    Result := FTypeCastSourceValue.FieldFlags * [svfAddress, svfOrdinal] <> [];
  end;
end;

procedure TFpDwarfValue.Reset;
begin
  FDataAddressCache := nil;
  FLastError := NoError;
end;

function TFpDwarfValue.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  if FValueSymbol <> nil then begin
    if FValueSymbol.HasAddress then Result := Result + [svfAddress];
  end
  else
  if HasTypeCastInfo then begin
    Result := Result + FTypeCastSourceValue.FieldFlags * [svfAddress];
  end;
end;

function TFpDwarfValue.HasTypeCastInfo: Boolean;
begin
  Result := (FTypeCastTargetType <> nil) and (FTypeCastSourceValue <> nil);
end;

function TFpDwarfValue.IsValidTypeCast: Boolean;
begin
  Result := False;
end;

procedure TFpDwarfValue.DoReferenceAdded;
begin
  inherited DoReferenceAdded;
  DoPlainReferenceAdded;
end;

procedure TFpDwarfValue.DoReferenceReleased;
begin
  inherited DoReferenceReleased;
  DoPlainReferenceReleased;
end;

procedure TFpDwarfValue.CircleBackRefActiveChanged(NewActive: Boolean);
begin
  inherited CircleBackRefActiveChanged(NewActive);
  if NewActive then;
  if CircleBackRefsActive then begin
    if FValueSymbol <> nil then
      FValueSymbol.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValueSymbol, 'TDbgDwarfSymbolValue'){$ENDIF};
    if FStructureValue <> nil then
      FStructureValue.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FStructureValue, 'TDbgDwarfSymbolValue'){$ENDIF};
  end
  else begin
    if FValueSymbol <> nil then
      FValueSymbol.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValueSymbol, 'TDbgDwarfSymbolValue'){$ENDIF};
    if FStructureValue <> nil then
      FStructureValue.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FStructureValue, 'TDbgDwarfSymbolValue'){$ENDIF};
  end;
end;

procedure TFpDwarfValue.SetLastMember(ALastMember: TFpDwarfValue);
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

function TFpDwarfValue.GetKind: TDbgSymbolKind;
begin
  if FValueSymbol <> nil then
    Result := FValueSymbol.Kind
  else
  if HasTypeCastInfo then
    Result := FTypeCastTargetType.Kind
  else
    Result := inherited GetKind;
end;

function TFpDwarfValue.GetAddress: TFpDbgMemLocation;
begin
  if FValueSymbol <> nil then
    FValueSymbol.GetValueAddress(Self, Result)
  else
  if HasTypeCastInfo then
    Result := FTypeCastSourceValue.Address
  else
    Result := inherited GetAddress;
end;

function TFpDwarfValue.OrdOrAddress: TFpDbgMemLocation;
begin
  if HasTypeCastInfo and (svfOrdinal in FTypeCastSourceValue.FieldFlags) then
    Result := ConstLoc(FTypeCastSourceValue.AsCardinal)
  else
    Result := Address;
end;

function TFpDwarfValue.GetMemberCount: Integer;
begin
  if FValueSymbol <> nil then
    Result := FValueSymbol.MemberCount
  else
    Result := inherited GetMemberCount;
end;

function TFpDwarfValue.GetMemberByName(AIndex: String): TFpDbgValue;
var
  m: TFpDbgSymbol;
begin
  Result := nil;
  if FValueSymbol <> nil then begin
    m := FValueSymbol.MemberByName[AIndex];
    if m <> nil then
      Result := m.Value;
  end;
  SetLastMember(TFpDwarfValue(Result));
end;

function TFpDwarfValue.GetMember(AIndex: Int64): TFpDbgValue;
var
  m: TFpDbgSymbol;
begin
  Result := nil;
  if FValueSymbol <> nil then begin
    m := FValueSymbol.Member[AIndex];
    if m <> nil then
      Result := m.Value;
  end;
  SetLastMember(TFpDwarfValue(Result));
end;

function TFpDwarfValue.GetDbgSymbol: TFpDbgSymbol;
begin
  Result := FValueSymbol;
end;

function TFpDwarfValue.GetTypeInfo: TFpDbgSymbol;
begin
  if HasTypeCastInfo then
    Result := FTypeCastTargetType
  else
    Result := inherited GetTypeInfo;
end;

function TFpDwarfValue.GetContextTypeInfo: TFpDbgSymbol;
begin
  if (FValueSymbol <> nil) and (FValueSymbol.ParentTypeInfo <> nil) then
    Result := FValueSymbol.ParentTypeInfo
  else
    Result := nil; // internal error
end;

constructor TFpDwarfValue.Create(AOwner: TFpDwarfSymbolType);
begin
  FOwner := AOwner;
  inherited Create;
end;

destructor TFpDwarfValue.Destroy;
begin
  FTypeCastTargetType.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FTypeCastTargetType, ClassName+'.FTypeCastTargetType'){$ENDIF};
  FTypeCastSourceValue.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FTypeCastSourceValue, ClassName+'.FTypeCastSourceValue'){$ENDIF};
  SetLastMember(nil);
  inherited Destroy;
end;

procedure TFpDwarfValue.SetValueSymbol(AValueSymbol: TFpDwarfSymbolValue);
begin
  if FValueSymbol = AValueSymbol then
    exit;

  if CircleBackRefsActive and (FValueSymbol <> nil) then
    FValueSymbol.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValueSymbol, 'TDbgDwarfSymbolValue'){$ENDIF};
  FValueSymbol := AValueSymbol;
  if CircleBackRefsActive and (FValueSymbol <> nil) then
    FValueSymbol.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValueSymbol, 'TDbgDwarfSymbolValue'){$ENDIF};
end;

function TFpDwarfValue.SetTypeCastInfo(AStructure: TFpDwarfSymbolType;
  ASource: TFpDbgValue): Boolean;
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

{ TFpDwarfValueSized }

function TFpDwarfValueSized.CanUseTypeCastAddress: Boolean;
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

function TFpDwarfValueSized.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfSize];
end;

function TFpDwarfValueSized.GetSize: Integer;
begin
  Result := FSize;
end;

constructor TFpDwarfValueSized.Create(AOwner: TFpDwarfSymbolType; ASize: Integer);
begin
  inherited Create(AOwner);
  FSize := ASize;
end;

{ TFpDwarfValueNumeric }

procedure TFpDwarfValueNumeric.Reset;
begin
  inherited Reset;
  FEvaluated := [];
end;

function TFpDwarfValueNumeric.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfOrdinal];
end;

function TFpDwarfValueNumeric.IsValidTypeCast: Boolean;
begin
  Result := HasTypeCastInfo;
  If not Result then
    exit;
  if (svfOrdinal in FTypeCastSourceValue.FieldFlags) or CanUseTypeCastAddress then
    exit;
  Result := False;
end;

constructor TFpDwarfValueNumeric.Create(AOwner: TFpDwarfSymbolType; ASize: Integer);
begin
  inherited Create(AOwner, ASize);
  FEvaluated := [];
end;

{ TFpDwarfValueInteger }

function TFpDwarfValueInteger.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfInteger];
end;

function TFpDwarfValueInteger.GetAsCardinal: QWord;
begin
  Result := QWord(GetAsInteger);  // include sign extension
end;

function TFpDwarfValueInteger.GetAsInteger: Int64;
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

function TFpDwarfValueCardinal.GetAsCardinal: QWord;
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

function TFpDwarfValueCardinal.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfCardinal];
end;

{ TFpDwarfValueFloat }

function TFpDwarfValueFloat.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfFloat] - [svfOrdinal];
end;

function TFpDwarfValueFloat.GetAsFloat: Extended;
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

{ TFpDwarfValueBoolean }

function TFpDwarfValueBoolean.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfBoolean];
end;

function TFpDwarfValueBoolean.GetAsBool: Boolean;
begin
  Result := QWord(GetAsCardinal) <> 0;
end;

{ TFpDwarfValueChar }

function TFpDwarfValueChar.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  case FSize of
    1: Result := Result + [svfString];
    2: Result := Result + [svfWideString];
  end;
end;

function TFpDwarfValueChar.GetAsString: AnsiString;
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

function TFpDwarfValueChar.GetAsWideString: WideString;
begin
  if FSize > 2 then
    Result := inherited GetAsWideString
  else
    Result := WideChar(Word(GetAsCardinal));
end;

{ TFpDwarfValuePointer }

function TFpDwarfValuePointer.GetAsCardinal: QWord;
var
  a: TFpDbgMemLocation;
begin
  a := GetDataAddress;
  if IsTargetAddr(a) then
    Result := LocToAddr(a)
  else
    Result := 0;
end;

function TFpDwarfValuePointer.GetFieldFlags: TFpDbgValueFieldFlags;
var
  t: TFpDbgSymbol;
begin
  Result := inherited GetFieldFlags;
  //TODO: svfDataAddress should depend on (hidden) Pointer or Ref in the TypeInfo
  Result := Result + [svfCardinal, svfOrdinal, svfSizeOfPointer, svfDataAddress] - [svfSize]; // data address

  t := TypeInfo;
  if (t <> nil) then t := t.TypeInfo;
  if (t <> nil) and (t.Kind = skChar) and IsReadableMem(DataAddress) then // pchar
    Result := Result + [svfString]; // data address
end;

function TFpDwarfValuePointer.GetDataAddress: TFpDbgMemLocation;
begin
  if doneAddr in FEvaluated then begin
    Result := FPointetToAddr;
    exit;
  end;
  Include(FEvaluated, doneAddr);

  if (FSize <= 0) then
    Result := InvalidLoc
  else
  begin
    if not MemManager.ReadAddress(OrdOrDataAddr, FSize, Result) then
      FLastError := MemManager.LastError;
  end;

  FPointetToAddr := Result;
end;

function TFpDwarfValuePointer.GetAsString: AnsiString;
var
  t: TFpDbgSymbol;
  i: Integer;
begin
  t := TypeInfo;
  if (t <> nil) then t := t.TypeInfo;
  if t.Size = 2 then
    Result := GetAsWideString
  else
  if  (MemManager <> nil) and (t <> nil) and (t.Kind = skChar) and IsReadableMem(DataAddress) then begin // pchar
    SetLength(Result, 2000);
    i := 2000;
    while (i > 0) and (not MemManager.ReadMemory(DataAddress, i, @Result[1])) do
      i := i div 2;
    SetLength(Result,i);
    i := pos(#0, Result);
    if i > 0 then
      SetLength(Result,i-1);
  end
  else
    Result := inherited GetAsString;
end;

function TFpDwarfValuePointer.GetAsWideString: WideString;
var
  t: TFpDbgSymbol;
  i: Integer;
begin
  t := TypeInfo;
  if (t <> nil) then t := t.TypeInfo;
  // skWideChar ???
  if  (MemManager <> nil) and (t <> nil) and (t.Kind = skChar) and IsReadableMem(DataAddress) then begin // pchar
    SetLength(Result, 2000);
    i := 4000; // 2000 * 16 bit
    while (i > 0) and (not MemManager.ReadMemory(DataAddress, i, @Result[1])) do
      i := i div 2;
    SetLength(Result, i div 2);
    i := pos(#0, Result);
    if i > 0 then
      SetLength(Result, i-1);
  end
  else
    Result := inherited GetAsWideString;
end;

function TFpDwarfValuePointer.GetMember(AIndex: Int64): TFpDbgValue;
var
  ti: TFpDbgSymbol;
  addr: TFpDbgMemLocation;
  Tmp: TFpDwarfValueConstAddress;
begin
  //TODO: ?? if no TypeInfo.TypeInfo;, then return TFpDwarfValueConstAddress.Create(addr); (for mem dump)
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
  addr := DataAddress;
  if not IsTargetAddr(addr) then begin
    FLastError := CreateError(fpErrAnyError, ['Internal dereference error']);
    exit;
  end;
  addr.Address := addr.Address + AIndex;
  {$POP}

  Tmp := TFpDwarfValueConstAddress.Create(addr);
  if ti <> nil then begin
    Result := ti.TypeCastValue(Tmp);
    Tmp.ReleaseReference;
    SetLastMember(TFpDwarfValue(Result));
    Result.ReleaseReference;
  end
  else begin
    Result := Tmp;
    FLastAddrMember := Result;
  end;
end;

destructor TFpDwarfValuePointer.Destroy;
begin
  FLastAddrMember.ReleaseReference;
  inherited Destroy;
end;

{ TFpDwarfValueEnum }

procedure TFpDwarfValueEnum.InitMemberIndex;
var
  v: QWord;
  i: Integer;
begin
  // TODO: if TypeInfo is a subrange, check against the bounds, then bypass it, and scan all members (avoid subrange scanning members)
  if FMemberValueDone then exit;
  // FTypeCastTargetType (if not nil) must be same as FOwner. It may have wrappers like declaration.
  v := GetAsCardinal;
  i := FOwner.MemberCount - 1;
  while i >= 0 do begin
    if FOwner.Member[i].OrdinalValue = v then break;
    dec(i);
  end;
  FMemberIndex := i;
  FMemberValueDone := True;
end;

procedure TFpDwarfValueEnum.Reset;
begin
  inherited Reset;
  FMemberValueDone := False;
end;

function TFpDwarfValueEnum.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfOrdinal, svfMembers, svfIdentifier];
end;

function TFpDwarfValueEnum.GetAsCardinal: QWord;
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

function TFpDwarfValueEnum.GetAsString: AnsiString;
begin
  InitMemberIndex;
  if FMemberIndex >= 0 then
    Result := FOwner.Member[FMemberIndex].Name
  else
    Result := '';
end;

function TFpDwarfValueEnum.GetMemberCount: Integer;
begin
  InitMemberIndex;
  if FMemberIndex < 0 then
    Result := 0
  else
    Result := 1;
end;

function TFpDwarfValueEnum.GetMember(AIndex: Int64): TFpDbgValue;
begin
  InitMemberIndex;
  if (FMemberIndex >= 0) and (AIndex = 0) then
    Result := FOwner.Member[FMemberIndex].Value
  else
    Result := nil;
end;

{ TFpDwarfValueEnumMember }

function TFpDwarfValueEnumMember.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfOrdinal, svfIdentifier];
end;

function TFpDwarfValueEnumMember.GetAsCardinal: QWord;
begin
  Result := FOwnerVal.OrdinalValue;
end;

function TFpDwarfValueEnumMember.GetAsString: AnsiString;
begin
  Result := FOwnerVal.Name;
end;

function TFpDwarfValueEnumMember.IsValidTypeCast: Boolean;
begin
  assert(False, 'TDbgDwarfEnumMemberSymbolValue.IsValidTypeCast can not be returned for typecast');
  Result := False;
end;

constructor TFpDwarfValueEnumMember.Create(AOwner: TFpDwarfSymbolValue);
begin
  FOwnerVal := AOwner;
  inherited Create(nil);
end;

{ TFpDwarfValueConstNumber }

procedure TFpDwarfValueConstNumber.Update(AValue: QWord; ASigned: Boolean);
begin
  Signed := ASigned;
  Value := AValue;
end;

{ TFpDwarfValueSet }

procedure TFpDwarfValueSet.InitMap;
const
  BitCount: array[0..15] of byte = (0, 1, 1, 2,  1, 2, 2, 3,  1, 2, 2, 3,  2, 3, 3, 4);
var
  i, i2, v, MemIdx, Bit, Cnt: Integer;

  t: TFpDbgSymbol;
begin
  if (length(FMem) > 0) or (FSize <= 0) then
    exit;
  t := TypeInfo;
  if t = nil then exit;
  t := t.TypeInfo;
  if t = nil then exit;

  if not MemManager.ReadSet(DataAddr, FSize, FMem) then begin
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
    for i := 0 to t.MemberCount - 1 do
    begin
      v := t.Member[i].OrdinalValue;
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
    v := t.OrdLowBound;
    for i := v to t.OrdHighBound do
    begin
      if (FMem[MemIdx] and Bit) <> 0 then begin
        assert(i2 < Cnt, 'TDbgDwarfSetSymbolValue.InitMap too many members');
        if i2 = Cnt then break;
        FMemberMap[i2] := i - v; // offset from low-bound
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

procedure TFpDwarfValueSet.Reset;
begin
  inherited Reset;
  SetLength(FMem, 0);
end;

function TFpDwarfValueSet.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfMembers];
  if FSize <= 8 then
    Result := Result + [svfOrdinal];
end;

function TFpDwarfValueSet.GetMemberCount: Integer;
begin
  InitMap;
  Result := FMemberCount;
end;

function TFpDwarfValueSet.GetMember(AIndex: Int64): TFpDbgValue;
var
  t: TFpDbgSymbol;
begin
  Result := nil;
  InitMap;
  t := TypeInfo;
  if t = nil then exit;
  t := t.TypeInfo;
  if t = nil then exit;
  assert(t is TFpDwarfSymbolType, 'TDbgDwarfSetSymbolValue.GetMember t');

  if t.Kind = skEnum then begin
    Result := t.Member[FMemberMap[AIndex]].Value;
  end
  else begin
    if (FNumValue = nil) or (FNumValue.RefCount > 1) then // refcount 1 by FTypedNumValue
      FNumValue := TFpDwarfValueConstNumber.Create(FMemberMap[AIndex] + t.OrdLowBound, t.Kind = skInteger)
    else
    begin
      FNumValue.Update(FMemberMap[AIndex] + t.OrdLowBound, t.Kind = skInteger);
      FNumValue.AddReference;
    end;

    if (FTypedNumValue = nil) or (FTypedNumValue.RefCount > 1) then begin
      FTypedNumValue.ReleaseReference;
      FTypedNumValue := t.TypeCastValue(FNumValue)
    end
    else
      TFpDwarfValue(FTypedNumValue).SetTypeCastInfo(TFpDwarfSymbolType(t), FNumValue); // update
    FNumValue.ReleaseReference;
    Assert((FTypedNumValue <> nil) and (TFpDwarfValue(FTypedNumValue).IsValidTypeCast), 'TDbgDwarfSetSymbolValue.GetMember FTypedNumValue');
    Assert((FNumValue <> nil) and (FNumValue.RefCount > 0), 'TDbgDwarfSetSymbolValue.GetMember FNumValue');
    Result := FTypedNumValue;
  end;
end;

function TFpDwarfValueSet.GetAsCardinal: QWord;
begin
  Result := 0;
  if (FSize <= SizeOf(Result)) and (length(FMem) > 0) then
    move(FMem[0], Result, FSize);
end;

function TFpDwarfValueSet.IsValidTypeCast: Boolean;
var
  f: TFpDbgValueFieldFlags;
begin
  Result := HasTypeCastInfo;
  If not Result then
    exit;

  assert(FTypeCastTargetType.Kind = skSet, 'TFpDwarfValueSet.IsValidTypeCast: FTypeCastTargetType.Kind = skSet');

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

destructor TFpDwarfValueSet.Destroy;
begin
  FTypedNumValue.ReleaseReference;
  inherited Destroy;
end;

{ TFpDwarfValueStruct }

procedure TFpDwarfValueStruct.Reset;
begin
  inherited Reset;
  FDataAddressDone := False;
end;

function TFpDwarfValueStruct.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfMembers];

  //TODO: svfDataAddress should depend on (hidden) Pointer or Ref in the TypeInfo
  if Kind in [skClass] then begin
    Result := Result + [svfOrdinal, svfDataAddress, svfDataSize]; // svfDataSize
    if (FValueSymbol <> nil) and FValueSymbol.HasAddress then
      Result := Result + [svfSizeOfPointer];
  end
  else begin
    Result := Result + [svfSize];
  end;
end;

function TFpDwarfValueStruct.GetAsCardinal: QWord;
begin
  Result := QWord(LocToAddrOrNil(DataAddress));
end;

function TFpDwarfValueStruct.GetDataAddress: TFpDbgMemLocation;
var
  t: TFpDbgMemLocation;
begin
  if FValueSymbol <> nil then begin
    if not FDataAddressDone then begin
      FDataAddress := InvalidLoc;
      FValueSymbol.GetValueAddress(Self, t);
      assert(SizeOf(FDataAddress) >= AddressSize, 'TDbgDwarfStructSymbolValue.GetDataAddress');
      if (MemManager <> nil) then begin
// TODO: wrong for records (const / DW_AT_FORM) // use GetDwarfDataAddress??
          FDataAddress := MemManager.ReadAddress(t, AddressSize);
        if not IsValidLoc(FDataAddress) then
          FLastError := MemManager.LastError;
      end;
      FDataAddressDone := True;
    end;
    Result := FDataAddress;
  end
  else
    Result := inherited GetDataAddress;
end;

function TFpDwarfValueStruct.GetDataSize: Integer;
begin
  Assert((FValueSymbol = nil) or (FValueSymbol.TypeInfo is TFpDwarfSymbol));
  if (FValueSymbol <> nil) and (FValueSymbol.TypeInfo <> nil) then
    if FValueSymbol.TypeInfo.Kind = skClass then
      Result := TFpDwarfSymbol(FValueSymbol.TypeInfo).DataSize
    else
      Result := FValueSymbol.TypeInfo.Size
  else
    Result := -1;
end;

function TFpDwarfValueStruct.GetSize: Integer;
begin
  if (Kind <> skClass) and (FValueSymbol <> nil) and (FValueSymbol.TypeInfo <> nil) then
    Result := FValueSymbol.TypeInfo.Size
  else
    Result := -1;
end;

{ TFpDwarfValueStructTypeCast }

procedure TFpDwarfValueStructTypeCast.Reset;
begin
  inherited Reset;
  FDataAddressDone := False;
end;

function TFpDwarfValueStructTypeCast.GetFieldFlags: TFpDbgValueFieldFlags;
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

function TFpDwarfValueStructTypeCast.GetKind: TDbgSymbolKind;
begin
  if HasTypeCastInfo then
    Result := FTypeCastTargetType.Kind
  else
    Result := inherited GetKind;
end;

function TFpDwarfValueStructTypeCast.GetAsCardinal: QWord;
begin
  Result := QWord(LocToAddrOrNil(DataAddress));
end;

function TFpDwarfValueStructTypeCast.GetSize: Integer;
begin
  if (Kind <> skClass) and (FTypeCastTargetType <> nil) then
    Result := FTypeCastTargetType.Size
  else
    Result := -1;
end;

function TFpDwarfValueStructTypeCast.GetDataSize: Integer;
begin
  Assert((FTypeCastTargetType = nil) or (FTypeCastTargetType is TFpDwarfSymbol));
  if FTypeCastTargetType <> nil then
    if FTypeCastTargetType.Kind = skClass then
      Result := TFpDwarfSymbol(FTypeCastTargetType).DataSize
    else
      Result := FTypeCastTargetType.Size
  else
    Result := -1;
end;

function TFpDwarfValueStructTypeCast.GetDataAddress: TFpDbgMemLocation;
var
  fields: TFpDbgValueFieldFlags;
  t: TFpDbgMemLocation;
begin
  if HasTypeCastInfo then begin
    if not FDataAddressDone then begin
// TODO: wrong for records // use GetDwarfDataAddress
      fields := FTypeCastSourceValue.FieldFlags;
      if svfOrdinal in fields then
        FDataAddress := TargetLoc(TDbgPtr(FTypeCastSourceValue.AsCardinal))
      else
      if svfAddress in fields then begin
        FDataAddress := InvalidLoc;
        t := FTypeCastSourceValue.Address;
        assert(SizeOf(FDataAddress) >= AddressSize, 'TDbgDwarfStructSymbolValue.GetDataAddress');
        if (MemManager <> nil) then begin
          FDataAddress := MemManager.ReadAddress(t, AddressSize);
          if not IsValidLoc(FDataAddress) then
            FLastError := MemManager.LastError;
        end;
      end;
      FDataAddressDone := True;
    end;
    Result := FDataAddress;
  end
  else
    Result := inherited GetDataAddress;
end;

function TFpDwarfValueStructTypeCast.IsValidTypeCast: Boolean;
var
  f: TFpDbgValueFieldFlags;
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

destructor TFpDwarfValueStructTypeCast.Destroy;
begin
  FreeAndNil(FMembers);
  inherited Destroy;
end;

function TFpDwarfValueStructTypeCast.GetMemberByName(AIndex: String): TFpDbgValue;
var
  tmp: TFpDbgSymbol;
begin
  Result := nil;
  if not HasTypeCastInfo then
    exit;

  tmp := FTypeCastTargetType.MemberByName[AIndex];
  if (tmp <> nil) then begin
    assert((tmp is TFpDwarfSymbolValue), 'TDbgDwarfStructTypeCastSymbolValue.GetMemberByName'+DbgSName(tmp));
    if FMembers = nil then
      FMembers := TFpDbgCircularRefCntObjList.Create;
    FMembers.Add(tmp);

    Result := tmp.Value;
  end;
  SetLastMember(TFpDwarfValue(Result));
end;

function TFpDwarfValueStructTypeCast.GetMember(AIndex: Int64): TFpDbgValue;
var
  tmp: TFpDbgSymbol;
begin
  Result := nil;
  if not HasTypeCastInfo then
    exit;

  // TODO: Why store them all in list? They are hold by the type
  tmp := FTypeCastTargetType.Member[AIndex];
  if (tmp <> nil) then begin
    assert((tmp is TFpDwarfSymbolValue), 'TDbgDwarfStructTypeCastSymbolValue.GetMemberByName'+DbgSName(tmp));
    if FMembers = nil then
      FMembers := TFpDbgCircularRefCntObjList.Create;
    FMembers.Add(tmp);

    Result := tmp.Value;
  end;
  SetLastMember(TFpDwarfValue(Result));
end;

function TFpDwarfValueStructTypeCast.GetMemberCount: Integer;
var
  ti: TFpDbgSymbol;
begin
  Result := 0;
  if not HasTypeCastInfo then
    exit;

  Result := FTypeCastTargetType.MemberCount;

  ti := FTypeCastTargetType;
  //TODO: cache result
  if ti.Kind in [skClass, skObject] then
    while ti.TypeInfo <> nil do begin
      ti := ti.TypeInfo;
      Result := Result + ti.MemberCount;
    end;
end;

{ TFpDwarfValueConstAddress }

procedure TFpDwarfValueConstAddress.Update(AnAddress: TFpDbgMemLocation);
begin
  Address := AnAddress;
end;

{ TFpDwarfValueArray }

function TFpDwarfValueArray.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfMembers];
  if (TypeInfo <> nil) and (sfDynArray in TypeInfo.Flags) then
    Result := Result + [svfOrdinal, svfDataAddress];
end;

function TFpDwarfValueArray.GetKind: TDbgSymbolKind;
begin
  Result := skArray;
end;

function TFpDwarfValueArray.GetAsCardinal: QWord;
begin
  // TODO cache
  if not MemManager.ReadUnsignedInt(OrdOrAddress, AddressSize, Result) then begin
    FLastError := MemManager.LastError;
    Result := 0;
  end;
end;

function TFpDwarfValueArray.GetDataAddress: TFpDbgMemLocation;
begin
  Result := OrdOrDataAddr;
end;

function TFpDwarfValueArray.GetMember(AIndex: Int64): TFpDbgValue;
begin
  Result := GetMemberEx([AIndex]);
end;

function TFpDwarfValueArray.GetMemberEx(const AIndex: array of Int64
  ): TFpDbgValue;
var
  Addr: TFpDbgMemLocation;
  i: Integer;
begin
  Result := nil;
  assert((FOwner is TFpDwarfSymbolTypeArray) and (FOwner.Kind = skArray));

  Addr := TFpDwarfSymbolTypeArray(FOwner).GetMemberAddress(Self, AIndex);
  if not IsReadableLoc(Addr) then exit;

  // FAddrObj.RefCount: hold by self
  i := 1;
  // FAddrObj.RefCount: hold by FLastMember (ignore only, if FLastMember is not hold by others)
  if (FLastMember <> nil) and (FLastMember.RefCount = 1) then
    i := 2;
  if (FAddrObj = nil) or (FAddrObj.RefCount > i) then begin
    FAddrObj.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FAddrObj, 'TDbgDwarfArraySymbolValue'){$ENDIF};
    FAddrObj := TFpDwarfValueConstAddress.Create(Addr);
    {$IFDEF WITH_REFCOUNT_DEBUG}FAddrObj.DbgRenameReference(@FAddrObj, 'TDbgDwarfArraySymbolValue');{$ENDIF}
  end
  else begin
    FAddrObj.Update(Addr);
  end;

  if (FLastMember = nil) or (FLastMember.RefCount > 1) then begin
    SetLastMember(TFpDwarfValue(FOwner.TypeInfo.TypeCastValue(FAddrObj)));
    FLastMember.ReleaseReference;
  end
  else begin
    TFpDwarfValue(FLastMember).SetTypeCastInfo(TFpDwarfSymbolType(FOwner.TypeInfo), FAddrObj);
  end;

  Result := FLastMember;
end;

function TFpDwarfValueArray.GetMemberCount: Integer;
var
  t, t2: TFpDbgSymbol;
  Addr: TFpDbgMemLocation;
  LowBound, HighBound: int64;
  i: Int64;
begin
  Result := 0;
  t := TypeInfo;
  if t.MemberCount < 1 then // IndexTypeCount;
    exit;
  t2 := t.Member[0]; // IndexType[0];
  if not ((t2 is TFpDwarfSymbolType) and (TFpDwarfSymbolType(t2).GetValueBounds(self, LowBound, HighBound))) and
     not t2.HasBounds then begin
    if (sfDynArray in t.Flags) and (AsCardinal <> 0) and
       GetDwarfDataAddress(Addr, TFpDwarfSymbolType(FOwner))
    then begin
      if not (IsReadableMem(Addr) and (LocToAddr(Addr) > AddressSize)) then
        exit;
      Addr.Address := Addr.Address - AddressSize;
      if MemManager.ReadSignedInt(Addr, AddressSize, i) then begin
        Result := Integer(i)+1;
        exit;
      end
      else
        FLastError := MemManager.LastError;
    end;
    exit;
  end;
  if t2.HasBounds then begin
    LowBound  := t2.OrdLowBound;
    HighBound := t2.OrdHighBound;
    if HighBound < LowBound then
      exit(0); // empty array // TODO: error
    // TODO: XXXXX Dynamic max limit
    if HighBound - LowBound > 3000 then
      HighBound := LowBound + 3000;
    Result := Integer(HighBound - LowBound + 1);
  end;
end;

function TFpDwarfValueArray.GetMemberCountEx(const AIndex: array of Int64
  ): Integer;
var
  t: TFpDbgSymbol;
begin
  Result := 0;
  t := TypeInfo;
  if length(AIndex) >= t.MemberCount then
    exit;
  t := t.Member[length(AIndex)];
  if not t.HasBounds then
    exit;
  Result := t.OrdHighBound - t.OrdLowBound + 1;
end;

function TFpDwarfValueArray.GetIndexType(AIndex: Integer): TFpDbgSymbol;
begin
  Result := TypeInfo.Member[AIndex];
end;

function TFpDwarfValueArray.GetIndexTypeCount: Integer;
begin
  Result := TypeInfo.MemberCount;
end;

function TFpDwarfValueArray.IsValidTypeCast: Boolean;
var
  f: TFpDbgValueFieldFlags;
begin
  Result := HasTypeCastInfo;
  If not Result then
    exit;

  assert(FTypeCastTargetType.Kind = skArray, 'TFpDwarfValueArray.IsValidTypeCast: FTypeCastTargetType.Kind = skArray');
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

destructor TFpDwarfValueArray.Destroy;
begin
  FAddrObj.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FAddrObj, 'TDbgDwarfArraySymbolValue'){$ENDIF};
  inherited Destroy;
end;

{ TDbgDwarfIdentifier }

function TFpDwarfSymbol.GetNestedTypeInfo: TFpDwarfSymbolType;
begin
// TODO DW_AT_start_scope;
  Result := FNestedTypeInfo;
  if (Result <> nil) or (didtTypeRead in FDwarfReadFlags) then
    exit;

  include(FDwarfReadFlags, didtTypeRead);
  FNestedTypeInfo := DoGetNestedTypeInfo;
  Result := FNestedTypeInfo;
end;

procedure TFpDwarfSymbol.SetParentTypeInfo(AValue: TFpDwarfSymbol);
begin
  if FParentTypeInfo = AValue then exit;

  if (FParentTypeInfo <> nil) and CircleBackRefsActive then
    FParentTypeInfo.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FParentTypeInfo, 'FParentTypeInfo'){$ENDIF};

  FParentTypeInfo := AValue;

  if (FParentTypeInfo <> nil) and CircleBackRefsActive then
    FParentTypeInfo.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FParentTypeInfo, 'FParentTypeInfo'){$ENDIF};
end;

procedure TFpDwarfSymbol.DoReferenceAdded;
begin
  inherited DoReferenceAdded;
  DoPlainReferenceAdded;
end;

procedure TFpDwarfSymbol.DoReferenceReleased;
begin
  inherited DoReferenceReleased;
  DoPlainReferenceReleased;
end;

procedure TFpDwarfSymbol.CircleBackRefActiveChanged(ANewActive: Boolean);
begin
  if (FParentTypeInfo = nil) then
    exit;
  if ANewActive then
    FParentTypeInfo.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FParentTypeInfo, 'FParentTypeInfo'){$ENDIF}
  else
    FParentTypeInfo.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FParentTypeInfo, 'FParentTypeInfo'){$ENDIF};
end;

function TFpDwarfSymbol.DoGetNestedTypeInfo: TFpDwarfSymbolType;
var
  FwdInfoPtr: Pointer;
  FwdCompUint: TDwarfCompilationUnit;
  InfoEntry: TDwarfInformationEntry;
begin // Do not access anything that may need forwardSymbol
  if InformationEntry.ReadReference(DW_AT_type, FwdInfoPtr, FwdCompUint) then begin
    InfoEntry := TDwarfInformationEntry.Create(FwdCompUint, FwdInfoPtr);
    Result := TFpDwarfSymbolType.CreateTypeSubClass('', InfoEntry);
    ReleaseRefAndNil(InfoEntry);
  end
  else
    Result := nil;
end;

function TFpDwarfSymbol.ReadMemberVisibility(out
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

function TFpDwarfSymbol.IsArtificial: Boolean;
begin
  if not(didtArtificialRead in FDwarfReadFlags) then begin
    if InformationEntry.IsArtificial then
      Include(FDwarfReadFlags, didtIsArtifical);
    Include(FDwarfReadFlags, didtArtificialRead);
  end;
  Result := didtIsArtifical in FDwarfReadFlags;
end;

procedure TFpDwarfSymbol.NameNeeded;
var
  AName: String;
begin
  if InformationEntry.ReadName(AName) then
    SetName(AName)
  else
    inherited NameNeeded;
end;

procedure TFpDwarfSymbol.TypeInfoNeeded;
begin
  SetTypeInfo(NestedTypeInfo);
end;

function TFpDwarfSymbol.DataSize: Integer;
var
  t: TFpDwarfSymbolType;
begin
  t := NestedTypeInfo;
  if t <> nil then
    Result := t.DataSize
  else
    Result := 0;
end;

function TFpDwarfSymbol.InitLocationParser(const ALocationParser: TDwarfLocationExpression;
  AnInitLocParserData: PInitLocParserData): Boolean;
begin
  if (AnInitLocParserData <> nil) and IsValidLoc(AnInitLocParserData^.ObjectDataAddress)
  then begin
    if AnInitLocParserData^.ObjectDataAddrPush then begin
      debugln(FPDBG_DWARF_VERBOSE, ['TFpDwarfSymbol.InitLocationParser Push=', dbgs(AnInitLocParserData^.ObjectDataAddress)]);
      ALocationParser.Push(AnInitLocParserData^.ObjectDataAddress, lseValue);
    end
    else begin
      debugln(FPDBG_DWARF_VERBOSE, ['TFpDwarfSymbol.InitLocationParser CurrentObjectAddress=', dbgs(AnInitLocParserData^.ObjectDataAddress)]);
      ALocationParser.CurrentObjectAddress := AnInitLocParserData^.ObjectDataAddress;
    end;
  end;

  Result := True;
end;

function TFpDwarfSymbol.LocationFromTag(ATag: Cardinal;
  const AnAttribData: TDwarfAttribData; AValueObj: TFpDwarfValue;
  var AnAddress: TFpDbgMemLocation; AnInitLocParserData: PInitLocParserData
  ): Boolean;
var
  Val: TByteDynArray;
  LocationParser: TDwarfLocationExpression;
begin
  //debugln(['TDbgDwarfIdentifier.LocationFromTag', ClassName, '  ',Name, '  ', DwarfAttributeToString(ATag)]);

  Result := False;
  AnAddress := InvalidLoc;

  //TODO: avoid copying data
  // DW_AT_data_member_location in members [ block or const]
  // DW_AT_location [block or reference] todo: const
  if not InformationEntry.ReadValue(AnAttribData, Val) then begin
    DebugLn(['LocationFromTag: failed to read DW_AT_location']);
    exit;
  end;

  if Length(Val) = 0 then begin
    DebugLn('LocationFromTag: Warning DW_AT_location empty');
    //exit;
  end;

  LocationParser := TDwarfLocationExpression.Create(@Val[0], Length(Val), CompilationUnit,
    AValueObj.MemManager, AValueObj.Context);
  InitLocationParser(LocationParser, AnInitLocParserData);
  LocationParser.Evaluate;

  if IsError(LocationParser.LastError) then
    SetLastError(LocationParser.LastError);

  if LocationParser.ResultKind in [lseValue] then begin
    AnAddress := TargetLoc(LocationParser.ResultData);// TODO: wrong, if origin was selfmem
    if ATag=DW_AT_location then
      AnAddress.Address :=CompilationUnit.MapAddressToNewValue(AnAddress.Address);
    Result := True;
  end
  else
  if LocationParser.ResultKind in [lseRegister] then begin
    AnAddress := ConstLoc(LocationParser.ResultData);
    Result := True;
  end
  else
    debugln(['TDbgDwarfIdentifier.LocationFromTag  FAILED']); // TODO

  LocationParser.Free;
end;

function TFpDwarfSymbol.LocationFromTag(ATag: Cardinal; AValueObj: TFpDwarfValue;
  var AnAddress: TFpDbgMemLocation; AnInitLocParserData: PInitLocParserData;
  AnInformationEntry: TDwarfInformationEntry; ASucessOnMissingTag: Boolean): Boolean;
var
  AttrData: TDwarfAttribData;
begin
  //debugln(['TDbgDwarfIdentifier.LocationFromTag', ClassName, '  ',Name, '  ', DwarfAttributeToString(ATag)]);

  Result := False;
  if AnInformationEntry = nil then
    AnInformationEntry := InformationEntry;

  //TODO: avoid copying data
  // DW_AT_data_member_location in members [ block or const]
  // DW_AT_location [block or reference] todo: const
  if not AnInformationEntry.GetAttribData(ATag, AttrData) then begin
    (* if ASucessOnMissingTag = true AND tag does not exist
       then AnAddress will NOT be modified
       this can be used for DW_AT_data_member_location, if it does not exist members are on input location
       TODO: review - better use temp var in caller
    *)
    Result := ASucessOnMissingTag;
    if not Result then
      AnAddress := InvalidLoc;
    if not Result then
      DebugLn(['LocationFromTag: failed to read DW_AT_location / ASucessOnMissingTag=', dbgs(ASucessOnMissingTag)]);
    exit;
  end;

  Result := LocationFromTag(ATag, AttrData, AValueObj, AnAddress, AnInitLocParserData);
end;

function TFpDwarfSymbol.ConstantFromTag(ATag: Cardinal; out
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

function TFpDwarfSymbol.GetDataAddress(AValueObj: TFpDwarfValue;
  var AnAddress: TFpDbgMemLocation; ATargetType: TFpDwarfSymbolType;
  ATargetCacheIndex: Integer): Boolean;
var
  ti: TFpDwarfSymbolType;
  InitLocParserData: TInitLocParserData;
begin
  InitLocParserData.ObjectDataAddress := AnAddress;
  InitLocParserData.ObjectDataAddrPush := False;
  Result := LocationFromTag(DW_AT_data_location, AValueObj, AnAddress, @InitLocParserData, nil, True);
  if not Result then
    exit;


  if ATargetType = Self then begin
    Result := True;
    exit;
  end;


  //TODO: Handle AValueObj.DataAddressCache[ATargetCacheIndex];
  Result := GetDataAddressNext(AValueObj, AnAddress, ATargetType, ATargetCacheIndex);
  if not Result then
    exit;

  ti := NestedTypeInfo;
  if ti <> nil then
    Result := ti.GetDataAddress(AValueObj, AnAddress, ATargetType, ATargetCacheIndex+1)
  else
    Result := ATargetType = nil; // end of type chain
end;

function TFpDwarfSymbol.GetDataAddressNext(AValueObj: TFpDwarfValue;
  var AnAddress: TFpDbgMemLocation; ATargetType: TFpDwarfSymbolType;
  ATargetCacheIndex: Integer): Boolean;
begin
  Result := True;
end;

function TFpDwarfSymbol.HasAddress: Boolean;
begin
  Result := False;
end;

procedure TFpDwarfSymbol.Init;
begin
  //
end;

class function TFpDwarfSymbol.CreateSubClass(AName: String;
  AnInformationEntry: TDwarfInformationEntry): TFpDwarfSymbol;
var
  c: TDbgDwarfSymbolBaseClass;
begin
  c := AnInformationEntry.CompUnit.DwarfSymbolClassMap.GetDwarfSymbolClass(AnInformationEntry.AbbrevTag);
  Result := TFpDwarfSymbol(c.Create(AName, AnInformationEntry));
end;

destructor TFpDwarfSymbol.Destroy;
begin
  inherited Destroy;
  ReleaseRefAndNil(FNestedTypeInfo);
  Assert(not CircleBackRefsActive, 'CircleBackRefsActive can not be is destructor');
  // FParentTypeInfo := nil
end;

function TFpDwarfSymbol.StartScope: TDbgPtr;
begin
  if not InformationEntry.ReadStartScope(Result) then
    Result := 0;
end;

{ TFpDwarfSymbolValue }

function TFpDwarfSymbolValue.GetValueAddress(AValueObj: TFpDwarfValue; out
  AnAddress: TFpDbgMemLocation): Boolean;
begin
  Result := False;
end;

function TFpDwarfSymbolValue.GetValueDataAddress(AValueObj: TFpDwarfValue; out
  AnAddress: TFpDbgMemLocation; ATargetType: TFpDwarfSymbolType): Boolean;
begin
  Result := TypeInfo <> nil;
  if not Result then
    exit;

  Assert((TypeInfo is TFpDwarfSymbol) and (TypeInfo.SymbolType = stType), 'TFpDwarfSymbolValue.GetDataAddress');
  Result := GetValueAddress(AValueObj, AnAddress);
  Result := Result and IsReadableLoc(AnAddress);
  if Result then begin
    Result := TFpDwarfSymbolType(TypeInfo).GetDataAddress(AValueObj, AnAddress, ATargetType, 1);
    if not Result then SetLastError(TypeInfo.LastError);
  end;
end;

procedure TFpDwarfSymbolValue.KindNeeded;
var
  t: TFpDbgSymbol;
begin
  t := TypeInfo;
  if t = nil then
    inherited KindNeeded
  else
    SetKind(t.Kind);
end;

procedure TFpDwarfSymbolValue.MemberVisibilityNeeded;
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

function TFpDwarfSymbolValue.GetMember(AIndex: Int64): TFpDbgSymbol;
var
  ti: TFpDbgSymbol;
  k: TDbgSymbolKind;
begin
  ti := TypeInfo;
  if ti = nil then begin
    Result := inherited GetMember(AIndex);
    exit;
  end;

  k := ti.Kind;
  // while holding result, until refcount added, do not call any function
  Result := ti.Member[AIndex];
  assert((Result = nil) or (Result is TFpDwarfSymbolValue), 'TFpDwarfSymbolValue.GetMember is Value');

  if (k in [skClass, skObject, skRecord {, skArray}]) and
     (Result <> nil) and (Result is TFpDwarfSymbolValue)
  then begin
    if FMembers = nil then
      FMembers := TFpDbgCircularRefCntObjList.Create;
    FMembers.Add(Result); //TODO: last member only?
  end;
end;

function TFpDwarfSymbolValue.GetMemberByName(AIndex: String): TFpDbgSymbol;
var
  ti: TFpDbgSymbol;
  k: TDbgSymbolKind;
begin
  ti := TypeInfo;
  if ti = nil then begin
    Result := inherited GetMemberByName(AIndex);
    exit;
  end;

  k := ti.Kind;

  // while holding result, until refcount added, do not call any function
  Result := ti.MemberByName[AIndex];
  assert((Result = nil) or (Result is TFpDwarfSymbolValue), 'TFpDwarfSymbolValue.GetMember is Value');

  if (k in [skClass, skObject, skRecord {, skArray}]) and
     (Result <> nil) and (Result is TFpDwarfSymbolValue)
  then begin
    if FMembers = nil then
      FMembers := TFpDbgCircularRefCntObjList.Create;
    FMembers.Add(Result);
  end;
end;

function TFpDwarfSymbolValue.GetMemberCount: Integer;
var
  ti: TFpDbgSymbol;
begin
  ti := TypeInfo;
  if ti <> nil then begin
    Result := ti.MemberCount;
    //TODO: cache result
    if ti.Kind in [skClass, skObject] then
      while ti.TypeInfo <> nil do begin
        ti := ti.TypeInfo;
        Result := Result + ti.MemberCount;
      end;
  end
  else
    Result := inherited GetMemberCount;
end;

procedure TFpDwarfSymbolValue.Init;
begin
  inherited Init;
  SetSymbolType(stValue);
end;

destructor TFpDwarfSymbolValue.Destroy;
begin
  Assert(not CircleBackRefsActive, 'CircleBackRefsActive can not be is ddestructor');

  FreeAndNil(FMembers);
  if FValueObject <> nil then begin
    FValueObject.SetValueSymbol(nil);
    FValueObject.ReleaseCirclularReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValueObject, ClassName+'.FValueObject'){$ENDIF};
    FValueObject := nil;
  end;
  ParentTypeInfo := nil;
  inherited Destroy;
end;

class function TFpDwarfSymbolValue.CreateValueSubClass(AName: String;
  AnInformationEntry: TDwarfInformationEntry): TFpDwarfSymbolValue;
var
  c: TDbgDwarfSymbolBaseClass;
begin
  c := AnInformationEntry.CompUnit.DwarfSymbolClassMap.GetDwarfSymbolClass(AnInformationEntry.AbbrevTag);

  if c.InheritsFrom(TFpDwarfSymbolValue) then
    Result := TFpDwarfSymbolValueClass(c).Create(AName, AnInformationEntry)
  else
    Result := nil;
end;

{ TFpDwarfSymbolValueWithLocation }

function TFpDwarfSymbolValueWithLocation.InitLocationParser(const ALocationParser: TDwarfLocationExpression;
  AnInitLocParserData: PInitLocParserData): Boolean;
begin
  Result := inherited InitLocationParser(ALocationParser, AnInitLocParserData);
  ALocationParser.OnFrameBaseNeeded := @FrameBaseNeeded;
end;

procedure TFpDwarfSymbolValueWithLocation.FrameBaseNeeded(ASender: TObject);
var
  p: TFpDwarfSymbol;
  fb: TDBGPtr;
begin
  debugln(FPDBG_DWARF_SEARCH, ['TFpDwarfSymbolValueVariable.FrameBaseNeeded ']);
  p := ParentTypeInfo;
  // TODO: what if parent is declaration?
  if (p <> nil) and (p is TFpDwarfSymbolValueProc) then begin
    fb := TFpDwarfSymbolValueProc(p).GetFrameBase(ASender as TDwarfLocationExpression);
    (ASender as TDwarfLocationExpression).FrameBase := fb;
    if fb = 0 then begin
      debugln(FPDBG_DWARF_ERRORS, ['DWARF ERROR in TFpDwarfSymbolValueWithLocation.FrameBaseNeeded result is 0']);
    end;
    exit;
  end;

{$warning TODO}
  //else
  //if OwnerTypeInfo <> nil then
  //  OwnerTypeInfo.fr;
  // TODO: check owner
  debugln(FPDBG_DWARF_ERRORS, ['DWARF ERROR in TFpDwarfSymbolValueWithLocation.FrameBaseNeeded no parent type info']);
  (ASender as TDwarfLocationExpression).FrameBase := 0;
end;

function TFpDwarfSymbolValueWithLocation.GetValueObject: TFpDbgValue;
var
  ti: TFpDbgSymbol;
begin
  Result := FValueObject;
  if Result <> nil then exit;

  ti := TypeInfo;
  if (ti = nil) or not (ti.SymbolType = stType) then exit;

  FValueObject := TFpDwarfSymbolType(ti).GetTypedValueObject(False);
  if FValueObject <> nil then begin
    {$IFDEF WITH_REFCOUNT_DEBUG}FValueObject.DbgRenameReference(@FValueObject, ClassName+'.FValueObject');{$ENDIF}
    FValueObject.MakePlainRefToCirclular;
    FValueObject.SetValueSymbol(self);
  end;

  Result := FValueObject;
end;

{ TFpDwarfSymbolType }

procedure TFpDwarfSymbolType.Init;
begin
  inherited Init;
  SetSymbolType(stType);
end;

procedure TFpDwarfSymbolType.MemberVisibilityNeeded;
var
  Val: TDbgSymbolMemberVisibility;
begin
  if ReadMemberVisibility(Val) then
    SetMemberVisibility(Val)
  else
    inherited MemberVisibilityNeeded;
end;

procedure TFpDwarfSymbolType.SizeNeeded;
var
  ByteSize: Integer;
begin
  if InformationEntry.ReadValue(DW_AT_byte_size, ByteSize) then
    SetSize(ByteSize)
  else
    inherited SizeNeeded;
end;

function TFpDwarfSymbolType.GetTypedValueObject(ATypeCast: Boolean): TFpDwarfValue;
begin
  Result := TFpDwarfValueUnknown.Create(Self);
end;

function TFpDwarfSymbolType.GetValueBounds(AValueObj: TFpDwarfValue; out ALowBound,
  AHighBound: Int64): Boolean;
begin
  Result := HasBounds;
  ALowBound := OrdLowBound;
  AHighBound := OrdHighBound;
end;

procedure TFpDwarfSymbolType.ResetValueBounds;
var
  ti: TFpDwarfSymbolType;
begin
  ti := NestedTypeInfo;
  if (ti <> nil) then
    ti.ResetValueBounds;
end;

class function TFpDwarfSymbolType.CreateTypeSubClass(AName: String;
  AnInformationEntry: TDwarfInformationEntry): TFpDwarfSymbolType;
var
  c: TDbgDwarfSymbolBaseClass;
begin
  c := AnInformationEntry.CompUnit.DwarfSymbolClassMap.GetDwarfSymbolClass(AnInformationEntry.AbbrevTag);

  if c.InheritsFrom(TFpDwarfSymbolType) then
    Result := TFpDwarfSymbolTypeClass(c).Create(AName, AnInformationEntry)
  else
    Result := nil;
end;

function TFpDwarfSymbolType.TypeCastValue(AValue: TFpDbgValue): TFpDbgValue;
begin
  Result := GetTypedValueObject(True);
  If Result = nil then
    exit;
  assert(Result is TFpDwarfValue);
  if not TFpDwarfValue(Result).SetTypeCastInfo(self, AValue) then
    ReleaseRefAndNil(Result);
end;

{ TDbgDwarfBaseTypeIdentifier }

procedure TFpDwarfSymbolTypeBasic.KindNeeded;
var
  Encoding, ByteSize: Integer;
begin
  if not InformationEntry.ReadValue(DW_AT_encoding, Encoding) then begin
    DebugLn(FPDBG_DWARF_WARNINGS, ['TFpDwarfSymbolTypeBasic.KindNeeded: Failed reading encoding for ', DwarfTagToString(InformationEntry.AbbrevTag)]);
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
        DebugLn(FPDBG_DWARF_WARNINGS, ['TFpDwarfSymbolTypeBasic.KindNeeded: Unknown encoding ', DwarfBaseTypeEncodingToString(Encoding), ' for ', DwarfTagToString(InformationEntry.AbbrevTag)]);
        inherited KindNeeded;
      end;
  end;
end;

procedure TFpDwarfSymbolTypeBasic.TypeInfoNeeded;
begin
  SetTypeInfo(nil);
end;

function TFpDwarfSymbolTypeBasic.GetTypedValueObject(ATypeCast: Boolean): TFpDwarfValue;
begin
  case Kind of
    skPointer:  Result := TFpDwarfValuePointer.Create(Self, Size);
    skInteger:  Result := TFpDwarfValueInteger.Create(Self, Size);
    skCardinal: Result := TFpDwarfValueCardinal.Create(Self, Size);
    skBoolean:  Result := TFpDwarfValueBoolean.Create(Self, Size);
    skChar:     Result := TFpDwarfValueChar.Create(Self, Size);
    skFloat:    Result := TFpDwarfValueFloat.Create(Self, Size);
  end;
end;

function TFpDwarfSymbolTypeBasic.GetHasBounds: Boolean;
begin
  Result := (kind = skInteger) or (kind = skCardinal);
end;

function TFpDwarfSymbolTypeBasic.GetOrdHighBound: Int64;
begin
  case Kind of
    skInteger:  Result := int64( high(int64) shr (64 - Min(Size, 8) * 8));
    skCardinal: Result := int64( high(qword) shr (64 - Min(Size, 8) * 8));
    else
      Result := inherited GetOrdHighBound;
  end;
end;

function TFpDwarfSymbolTypeBasic.GetOrdLowBound: Int64;
begin
  case Kind of
    skInteger:  Result := -(int64( high(int64) shr (64 - Min(Size, 8) * 8)))-1;
    skCardinal: Result := 0;
    else
      Result := inherited GetOrdHighBound;
  end;
end;

{ TFpDwarfSymbolTypeModifier }

procedure TFpDwarfSymbolTypeModifier.TypeInfoNeeded;
var
  p: TFpDwarfSymbolType;
begin
  p := NestedTypeInfo;
  if p <> nil then
    SetTypeInfo(p.TypeInfo)
  else
    SetTypeInfo(nil);
end;

procedure TFpDwarfSymbolTypeModifier.ForwardToSymbolNeeded;
begin
  SetForwardToSymbol(NestedTypeInfo)
end;

function TFpDwarfSymbolTypeModifier.GetTypedValueObject(ATypeCast: Boolean): TFpDwarfValue;
var
  ti: TFpDwarfSymbolType;
begin
  ti := NestedTypeInfo;
  if ti <> nil then
    Result := ti.GetTypedValueObject(ATypeCast)
  else
    Result := inherited;
end;

{ TFpDwarfSymbolTypeRef }

function TFpDwarfSymbolTypeRef.GetFlags: TDbgSymbolFlags;
begin
  Result := (inherited GetFlags) + [sfInternalRef];
end;

function TFpDwarfSymbolTypeRef.GetDataAddressNext(AValueObj: TFpDwarfValue;
  var AnAddress: TFpDbgMemLocation; ATargetType: TFpDwarfSymbolType;
  ATargetCacheIndex: Integer): Boolean;
var
  t: TFpDbgMemLocation;
begin
  t := AValueObj.DataAddressCache[ATargetCacheIndex];
  if IsInitializedLoc(t) then begin
    AnAddress := t;
  end
  else begin
    Result := AValueObj.MemManager <> nil;
    if not Result then
      exit;
    AnAddress := AValueObj.MemManager.ReadAddress(AnAddress, CompilationUnit.AddressSize);
    AValueObj.DataAddressCache[ATargetCacheIndex] := AnAddress;
  end;
  Result := IsValidLoc(AnAddress);

  if Result then
    Result := inherited GetDataAddressNext(AValueObj, AnAddress, ATargetType, ATargetCacheIndex)
  else
  if IsError(AValueObj.MemManager.LastError) then
    SetLastError(AValueObj.MemManager.LastError);
  // Todo: other error
end;

{ TFpDwarfSymbolTypeDeclaration }

function TFpDwarfSymbolTypeDeclaration.DoGetNestedTypeInfo: TFpDwarfSymbolType;
var
  ti: TFpDwarfSymbolType;
  ti2: TFpDbgSymbol;
begin
  Result := inherited DoGetNestedTypeInfo;

  // Is internal class pointer?
  // Do not trigged any cached property of the pointer
  if (Result = nil) then
    exit;

  ti := Result;
  if (ti is TFpDwarfSymbolTypeModifier) then begin
    ti := TFpDwarfSymbolType(ti.TypeInfo);
    if (Result = nil) then
      exit;
  end;
  if not (ti is TFpDwarfSymbolTypePointer) then
    exit;

  ti2 := ti.NestedTypeInfo;
  // only if it is NOT a declaration
  if (ti2 <> nil) and (ti2 is TFpDwarfSymbolTypeStructure) then begin
    TFpDwarfSymbolTypePointer(ti).IsInternalPointer := True;
    // TODO: Flag the structure as class (save teme in KindNeeded)
  end;
end;

{ TFpDwarfSymbolTypeSubRange }

procedure TFpDwarfSymbolTypeSubRange.InitEnumIdx;
var
  t: TFpDwarfSymbolType;
  i: Integer;
  h, l: Int64;
begin
  if FEnumIdxValid then
    exit;
  FEnumIdxValid := True;

  t := NestedTypeInfo;
  i := t.MemberCount - 1;
  h := OrdHighBound;
  l := OrdLowBound;

  while (i >= 0) and (t.Member[i].OrdinalValue > h) do
    dec(i);
  FHighEnumIdx := i;

  while (i >= 0) and (t.Member[i].OrdinalValue >= l) do
    dec(i);
  FLowEnumIdx := i + 1;
end;

procedure TFpDwarfSymbolTypeSubRange.ReadBounds(AValueObj: TFpDwarfValue);
var
  FwdInfoPtr: Pointer;
  FwdCompUint: TDwarfCompilationUnit;
  NewInfo: TDwarfInformationEntry;
var
  AnAddress: TFpDbgMemLocation;
  InitLocParserData: TInitLocParserData;
  AttrData: TDwarfAttribData;
begin
  // TODO: assert(AValueObj <> nil, 'TFpDwarfSymbolTypeSubRange.ReadBounds: AValueObj <> nil');
  if FLowBoundState <> rfNotRead then exit;

  if InformationEntry.GetAttribData(DW_AT_lower_bound, AttrData) then begin
    // Todo: LocationFromTag()
    if InformationEntry.ReadReference(AttrData, FwdInfoPtr, FwdCompUint) then begin
      NewInfo := TDwarfInformationEntry.Create(FwdCompUint, FwdInfoPtr);
      FLowBoundValue := TFpDwarfSymbolValue.CreateValueSubClass('', NewInfo);
      NewInfo.ReleaseReference;
      if FLowBoundValue = nil then begin
        FLowBoundState := rfNotFound; // not implemented => so for all purpose tread it as if there is no dwarf description of the low bound
        exit;
      end
      else
        FLowBoundState := rfValue;
    end
    else
    if InformationEntry.ReadValue(AttrData, FLowBoundConst) then begin
      FLowBoundState := rfConst;
    end
    else
    begin
      //FLowBoundConst := 0; // the default
      //FLowBoundState := rfConst;
      FLowBoundState := rfNotFound;
      exit; // incomplete type
    end;
  end
  else
    FLowBoundState := rfNotFound; // no dwarf description of low bound


  if InformationEntry.GetAttribData(DW_AT_upper_bound, AttrData) then begin
    if InformationEntry.ReadReference(AttrData, FwdInfoPtr, FwdCompUint) then begin
      NewInfo := TDwarfInformationEntry.Create(FwdCompUint, FwdInfoPtr);
      FHighBoundValue := TFpDwarfSymbolValue.CreateValueSubClass('', NewInfo);
      NewInfo.ReleaseReference;
      if FHighBoundValue = nil then begin
        FHighBoundState := rfNotFound;  // not implemented => so for all purpose tread it as if there is no dwarf description of the low bound
        exit;
      end
      else
        FHighBoundState := rfValue;
    end
    else
    if InformationEntry.ReadValue(AttrData, FHighBoundConst) then begin
      FHighBoundState := rfConst;
    end
    else
    begin
      if assigned(AValueObj) then
        InitLocParserData.ObjectDataAddress := AValueObj.Address;
      InitLocParserData.ObjectDataAddrPush := False;
      if assigned(AValueObj) and LocationFromTag(DW_AT_upper_bound, AttrData, AValueObj, AnAddress, @InitLocParserData) then begin
        FHighBoundState := rfConst;
        FHighBoundConst := Int64(AnAddress.Address);
      end
      else
        FHighBoundState := rfNotFound;
    end;
  end
  else
  begin
    FHighBoundState := rfNotFound; // NO DW_AT_upper_bound found

    // ONLY if DW_AT_upper_bound does not exist: try DW_AT_count
    if InformationEntry.GetAttribData(DW_AT_upper_bound, AttrData) then begin
      if InformationEntry.ReadReference(AttrData, FwdInfoPtr, FwdCompUint) then begin
        NewInfo := TDwarfInformationEntry.Create(FwdCompUint, FwdInfoPtr);
        FCountValue := TFpDwarfSymbolValue.CreateValueSubClass('', NewInfo);
        NewInfo.ReleaseReference;
        if FCountValue = nil then begin
          FCountState := rfNotFound;
          exit;
        end
        else
          FCountState := rfValue;
      end
      else
      if InformationEntry.ReadValue(AttrData, FCountConst) then begin
        FCountState := rfConst;
      end
      else
        FCountState := rfNotFound;
    end
    else
      FCountState := rfNotFound;
  end;
end;

function TFpDwarfSymbolTypeSubRange.DoGetNestedTypeInfo: TFpDwarfSymbolType;
begin
  Result := inherited DoGetNestedTypeInfo;
  if Result <> nil then
    exit;

  if FLowBoundState = rfValue then
    Result := FLowBoundValue.TypeInfo as TFpDwarfSymbolType
  else
  if FHighBoundState = rfValue then
    Result := FHighBoundValue.TypeInfo as TFpDwarfSymbolType
  else
  if FCountState = rfValue then
    Result := FCountValue.TypeInfo as TFpDwarfSymbolType;
end;

function TFpDwarfSymbolTypeSubRange.GetHasBounds: Boolean;
begin
  ReadBounds(nil);
// TODO: currently limited to const.
// not standard, but upper may be missing?
  Result := (FLowBoundState in [rfConst]) and
            ( (FHighBoundState in [rfConst]) or
              (FCountState in [rfConst]) );

  (*
  Result := (FLowBoundState in [rfValue, rfConst]) and
            ( (FHighBoundState in [rfValue, rfConst]) or
              (FCountState in [rfValue, rfConst]) );
  *)
end;

function TFpDwarfSymbolTypeSubRange.GetOrdHighBound: Int64;
begin
// Todo range check off.
  //if FHighBoundState = rfValue then
  //  Result := FHighBoundValue.VALUE // TODO
  //else
  if FHighBoundState = rfConst then
    Result := FHighBoundConst
  else
  //if FCountState = rfValue then
  //  Result := GetOrdLowBound + FCountValue.VALUE - 1 // TODO
  //else
  if FHighBoundState = rfConst then
    Result := GetOrdLowBound + FCountConst - 1;
end;

function TFpDwarfSymbolTypeSubRange.GetOrdLowBound: Int64;
begin
  //if FLowBoundState = rfValue then
  //  Result := FLowBoundValue.VALUE // TODO
  //else
    Result := FLowBoundConst;
end;

procedure TFpDwarfSymbolTypeSubRange.NameNeeded;
var
  AName: String;
begin
  if InformationEntry.ReadName(AName) then
    SetName(AName)
  else
    SetName('');
end;

procedure TFpDwarfSymbolTypeSubRange.KindNeeded;
var
  t: TFpDbgSymbol;
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

procedure TFpDwarfSymbolTypeSubRange.SizeNeeded;
var
  t: TFpDbgSymbol;
begin
  t := NestedTypeInfo;
  if t = nil then begin
    SetKind(skInteger);
    SetSize(CompilationUnit.AddressSize);
  end
  else
    SetSize(t.Size);
end;

function TFpDwarfSymbolTypeSubRange.GetMember(AIndex: Int64): TFpDbgSymbol;
begin
  if Kind = skEnum then begin
    if not FEnumIdxValid then
      InitEnumIdx;
    Result := NestedTypeInfo.Member[AIndex - FLowEnumIdx];
  end
  else
    Result := inherited GetMember(AIndex);
end;

function TFpDwarfSymbolTypeSubRange.GetMemberCount: Integer;
begin
  if Kind = skEnum then begin
    if not FEnumIdxValid then
      InitEnumIdx;
    Result := FHighEnumIdx - FLowEnumIdx + 1;
  end
  else
    Result := inherited GetMemberCount;
end;

function TFpDwarfSymbolTypeSubRange.GetFlags: TDbgSymbolFlags;
begin
  Result := (inherited GetFlags) + [sfSubRange];
end;

function TFpDwarfSymbolTypeSubRange.GetValueBounds(AValueObj: TFpDwarfValue; out
  ALowBound, AHighBound: Int64): Boolean;
begin
  ReadBounds(AValueObj);
  Result := inherited GetValueBounds(AValueObj, ALowBound, AHighBound);
end;

procedure TFpDwarfSymbolTypeSubRange.ResetValueBounds;
begin
  inherited ResetValueBounds;
  FLowBoundState := rfNotRead;
  FHighBoundState := rfNotRead;
  FCountState := rfNotRead;
end;

procedure TFpDwarfSymbolTypeSubRange.Init;
begin
  FLowBoundState := rfNotRead;
  FHighBoundState := rfNotRead;
  FCountState := rfNotRead;
  inherited Init;
end;

{ TFpDwarfSymbolTypePointer }

function TFpDwarfSymbolTypePointer.IsInternalDynArrayPointer: Boolean;
var
  ti: TFpDbgSymbol;
begin
  Result := False;
  ti := NestedTypeInfo;  // Same as TypeInfo, but does not try to be forwarded
  Result := (ti <> nil) and (ti is TFpDwarfSymbolTypeArray);
  if Result then
    Result := (sfDynArray in ti.Flags);
end;

procedure TFpDwarfSymbolTypePointer.TypeInfoNeeded;
var
  p: TFpDwarfSymbolType;
begin
  p := NestedTypeInfo;
  if IsInternalPointer and (p <> nil) then begin
    SetTypeInfo(p.TypeInfo);
    exit;
  end;
  SetTypeInfo(p);
end;

function TFpDwarfSymbolTypePointer.GetIsInternalPointer: Boolean;
begin
  Result := FIsInternalPointer or IsInternalDynArrayPointer;
end;

procedure TFpDwarfSymbolTypePointer.KindNeeded;
var
  k: TDbgSymbolKind;
begin
  if IsInternalPointer then begin
      k := NestedTypeInfo.Kind;
      if k = skObject then
        SetKind(skClass)
      else
        SetKind(k);
  end
  else
    SetKind(skPointer);
end;

procedure TFpDwarfSymbolTypePointer.SizeNeeded;
begin
  SetSize(CompilationUnit.AddressSize);
end;

procedure TFpDwarfSymbolTypePointer.ForwardToSymbolNeeded;
begin
  if IsInternalPointer then
    SetForwardToSymbol(NestedTypeInfo) // Same as TypeInfo, but does not try to be forwarded
  else
    SetForwardToSymbol(nil); // inherited ForwardToSymbolNeeded;
end;

function TFpDwarfSymbolTypePointer.GetDataAddressNext(AValueObj: TFpDwarfValue;
  var AnAddress: TFpDbgMemLocation; ATargetType: TFpDwarfSymbolType;
  ATargetCacheIndex: Integer): Boolean;
var
  t: TFpDbgMemLocation;
begin
  t := AValueObj.DataAddressCache[ATargetCacheIndex];
  if IsInitializedLoc(t) then begin
    AnAddress := t;
  end
  else begin
    Result := AValueObj.MemManager <> nil;
    if not Result then
      exit;
    AnAddress := AValueObj.MemManager.ReadAddress(AnAddress, CompilationUnit.AddressSize);
    AValueObj.DataAddressCache[ATargetCacheIndex] := AnAddress;
  end;
  Result := IsValidLoc(AnAddress);

  if Result then
    Result := inherited GetDataAddressNext(AValueObj, AnAddress, ATargetType, ATargetCacheIndex)
  else
  if IsError(AValueObj.MemManager.LastError) then
    SetLastError(AValueObj.MemManager.LastError);
  // Todo: other error
end;

function TFpDwarfSymbolTypePointer.GetTypedValueObject(ATypeCast: Boolean): TFpDwarfValue;
begin
  if IsInternalPointer then
    Result := NestedTypeInfo.GetTypedValueObject(ATypeCast)
  else
    Result := TFpDwarfValuePointer.Create(Self, CompilationUnit.AddressSize);
end;

function TFpDwarfSymbolTypePointer.DataSize: Integer;
begin
  if Kind = skClass then
    Result := NestedTypeInfo.Size
  else
    Result := inherited DataSize;
end;

{ TDbgDwarfIdentifierEnumElement }

procedure TFpDwarfSymbolValueEnumMember.ReadOrdinalValue;
begin
  if FOrdinalValueRead then exit;
  FOrdinalValueRead := True;
  FHasOrdinalValue := InformationEntry.ReadValue(DW_AT_const_value, FOrdinalValue);
end;

procedure TFpDwarfSymbolValueEnumMember.KindNeeded;
begin
  SetKind(skEnumValue);
end;

function TFpDwarfSymbolValueEnumMember.GetHasOrdinalValue: Boolean;
begin
  ReadOrdinalValue;
  Result := FHasOrdinalValue;
end;

function TFpDwarfSymbolValueEnumMember.GetOrdinalValue: Int64;
begin
  ReadOrdinalValue;
  Result := FOrdinalValue;
end;

procedure TFpDwarfSymbolValueEnumMember.Init;
begin
  FOrdinalValueRead := False;
  inherited Init;
end;

function TFpDwarfSymbolValueEnumMember.GetValueObject: TFpDbgValue;
begin
  Result := FValueObject;
  if Result <> nil then exit;

  FValueObject := TFpDwarfValueEnumMember.Create(Self);
  {$IFDEF WITH_REFCOUNT_DEBUG}FValueObject.DbgRenameReference(@FValueObject, ClassName+'.FValueObject');{$ENDIF}
  FValueObject.MakePlainRefToCirclular;
  FValueObject.SetValueSymbol(self);

  Result := FValueObject;
end;

{ TFpDwarfSymbolTypeEnum }

procedure TFpDwarfSymbolTypeEnum.CreateMembers;
var
  Info, Info2: TDwarfInformationEntry;
  sym: TFpDwarfSymbol;
begin
  if FMembers <> nil then
    exit;
  FMembers := TFpDbgCircularRefCntObjList.Create;
  Info := InformationEntry.FirstChild;
  if Info = nil then exit;

  while Info.HasValidScope do begin
    if (Info.AbbrevTag = DW_TAG_enumerator) then begin
      Info2 := Info.Clone;
      sym := TFpDwarfSymbol.CreateSubClass('', Info2);
      FMembers.Add(sym);
      sym.ReleaseReference;
      sym.ParentTypeInfo := self;
      Info2.ReleaseReference;
    end;
    Info.GoNext;
  end;

  Info.ReleaseReference;
end;

function TFpDwarfSymbolTypeEnum.GetTypedValueObject(ATypeCast: Boolean): TFpDwarfValue;
begin
  Result := TFpDwarfValueEnum.Create(Self, Size);
end;

procedure TFpDwarfSymbolTypeEnum.KindNeeded;
begin
  SetKind(skEnum);
end;

function TFpDwarfSymbolTypeEnum.GetMember(AIndex: Int64): TFpDbgSymbol;
begin
  CreateMembers;
  Result := TFpDbgSymbol(FMembers[AIndex]);
end;

function TFpDwarfSymbolTypeEnum.GetMemberByName(AIndex: String): TFpDbgSymbol;
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
    Result := TFpDbgSymbol(FMembers[i]);
    s := Result.Name;
    if (s <> '') and CompareUtf8BothCase(@s1[1], @s2[1], @s[1]) then
      exit;
    dec(i);
  end;
  Result := nil;
end;

function TFpDwarfSymbolTypeEnum.GetMemberCount: Integer;
begin
  CreateMembers;
  Result := FMembers.Count;
end;

function TFpDwarfSymbolTypeEnum.GetHasBounds: Boolean;
begin
  Result := True;
end;

function TFpDwarfSymbolTypeEnum.GetOrdHighBound: Int64;
var
  c: Integer;
begin
  c := MemberCount;
  if c > 0 then
    Result := Member[c-1].OrdinalValue
  else
    Result := -1;
end;

function TFpDwarfSymbolTypeEnum.GetOrdLowBound: Int64;
var
  c: Integer;
begin
  c := MemberCount;
  if c > 0 then
    Result := Member[0].OrdinalValue
  else
    Result := 0;
end;

destructor TFpDwarfSymbolTypeEnum.Destroy;
var
  i: Integer;
begin
  if FMembers <> nil then
    for i := 0 to FMembers.Count - 1 do
      TFpDwarfSymbol(FMembers[i]).ParentTypeInfo := nil;
  FreeAndNil(FMembers);
  inherited Destroy;
end;

{ TFpDwarfSymbolTypeSet }

procedure TFpDwarfSymbolTypeSet.KindNeeded;
begin
  SetKind(skSet);
end;

function TFpDwarfSymbolTypeSet.GetTypedValueObject(ATypeCast: Boolean): TFpDwarfValue;
begin
  Result := TFpDwarfValueSet.Create(Self, Size);
end;

function TFpDwarfSymbolTypeSet.GetMemberCount: Integer;
begin
  if TypeInfo.Kind = skEnum then
    Result := TypeInfo.MemberCount
  else
    Result := inherited GetMemberCount;
end;

function TFpDwarfSymbolTypeSet.GetMember(AIndex: Int64): TFpDbgSymbol;
begin
  if TypeInfo.Kind = skEnum then
    Result := TypeInfo.Member[AIndex]
  else
    Result := inherited GetMember(AIndex);
end;

{ TFpDwarfSymbolValueMember }

function TFpDwarfSymbolValueMember.GetValueAddress(AValueObj: TFpDwarfValue; out
  AnAddress: TFpDbgMemLocation): Boolean;
var
  BaseAddr: TFpDbgMemLocation;
  InitLocParserData: TInitLocParserData;
begin
  AnAddress := AValueObj.DataAddressCache[0];
  Result := IsValidLoc(AnAddress);
  if IsInitializedLoc(AnAddress) then
    exit;

  if AValueObj = nil then debugln(['TFpDwarfSymbolValueMember.InitLocationParser: NO VAl Obj !!!!!!!!!!!!!!!'])
  else if AValueObj.StructureValue = nil then debugln(['TFpDwarfSymbolValueMember.InitLocationParser: NO STRUCT Obj !!!!!!!!!!!!!!!']);

  if (AValueObj = nil) or (AValueObj.StructureValue = nil) or (ParentTypeInfo = nil)
  then begin
    debugln(FPDBG_DWARF_ERRORS, ['DWARF ERROR in TFpDwarfSymbolValueMember.InitLocationParser Error: ',ErrorCode(LastError),' ValueObject=', DbgSName(FValueObject)]);
    Result := False;
    if not IsError(LastError) then
      SetLastError(CreateError(fpErrLocationParserInit)); // TODO: error message?
    exit;
  end;
  Assert((ParentTypeInfo is TFpDwarfSymbol) and (ParentTypeInfo.SymbolType = stType), '');
  if not AValueObj.GetStructureDwarfDataAddress(BaseAddr, TFpDwarfSymbolType(ParentTypeInfo)) then begin
    debugln(FPDBG_DWARF_ERRORS, ['DWARF ERROR in TFpDwarfSymbolValueMember.InitLocationParser Error: ',ErrorCode(LastError),' ValueObject=', DbgSName(FValueObject)]);
    Result := False;
    if not IsError(LastError) then
      SetLastError(CreateError(fpErrLocationParserInit)); // TODO: error message?
    exit;
  end;
  //TODO: AValueObj.StructureValue.LastError

  InitLocParserData.ObjectDataAddress := BaseAddr;
  InitLocParserData.ObjectDataAddrPush := True;
  Result := LocationFromTag(DW_AT_data_member_location, AValueObj, AnAddress, @InitLocParserData);

  AValueObj.DataAddressCache[0] := AnAddress;
end;

function TFpDwarfSymbolValueMember.HasAddress: Boolean;
begin
  Result := (InformationEntry.HasAttrib(DW_AT_data_member_location));
end;

{ TFpDwarfSymbolTypeStructure }

function TFpDwarfSymbolTypeStructure.GetMemberByName(AIndex: String): TFpDbgSymbol;
var
  Ident: TDwarfInformationEntry;
  ti: TFpDbgSymbol;
begin
  // Todo, maybe create all children?
  if FLastChildByName <> nil then begin
    FLastChildByName.ReleaseCirclularReference;
    FLastChildByName := nil;
  end;
  Result := nil;

  Ident := InformationEntry.FindNamedChild(AIndex);
  if Ident <> nil then begin
    FLastChildByName := TFpDwarfSymbol.CreateSubClass('', Ident);
    FLastChildByName.MakePlainRefToCirclular;
    FLastChildByName.ParentTypeInfo := self;
    //assert is member ?
    ReleaseRefAndNil(Ident);
    Result := FLastChildByName;

    exit;
  end;

  ti := TypeInfo; // Parent
  if ti <> nil then
    Result := ti.MemberByName[AIndex];
end;

function TFpDwarfSymbolTypeStructure.GetMemberCount: Integer;
begin
  CreateMembers;
  Result := FMembers.Count;
end;

function TFpDwarfSymbolTypeStructure.GetDataAddressNext(AValueObj: TFpDwarfValue;
  var AnAddress: TFpDbgMemLocation; ATargetType: TFpDwarfSymbolType;
  ATargetCacheIndex: Integer): Boolean;
var
  t: TFpDbgMemLocation;
  InitLocParserData: TInitLocParserData;
begin
  t := AValueObj.DataAddressCache[ATargetCacheIndex];
  if IsInitializedLoc(t) then begin
    AnAddress := t;
    Result := IsValidLoc(AnAddress);
  end
  else begin
    InitInheritanceInfo;
    //TODO: may be a constant // offset
    InitLocParserData.ObjectDataAddress := AnAddress;
    InitLocParserData.ObjectDataAddrPush := True;
    Result := LocationFromTag(DW_AT_data_member_location, AValueObj, t, @InitLocParserData, FInheritanceInfo);
    if not Result then
      exit;
    AnAddress := t;
    AValueObj.DataAddressCache[ATargetCacheIndex] := AnAddress;

    if IsError(AValueObj.MemManager.LastError) then
      SetLastError(AValueObj.MemManager.LastError);
  end;

  Result := inherited GetDataAddressNext(AValueObj, AnAddress, ATargetType, ATargetCacheIndex);
end;

function TFpDwarfSymbolTypeStructure.GetMember(AIndex: Int64): TFpDbgSymbol;
var
  ti: TFpDbgSymbol;
begin
  CreateMembers;
  if AIndex >= FMembers.Count then begin
    ti := TypeInfo;
    if ti <> nil then
      Result := ti.Member[AIndex - FMembers.Count];
  end
  else
    Result := TFpDbgSymbol(FMembers[AIndex]);
end;

destructor TFpDwarfSymbolTypeStructure.Destroy;
var
  i: Integer;
begin
  ReleaseRefAndNil(FInheritanceInfo);
  if FMembers <> nil then begin
    for i := 0 to FMembers.Count - 1 do
      TFpDwarfSymbol(FMembers[i]).ParentTypeInfo := nil;
    FreeAndNil(FMembers);
  end;
  if FLastChildByName <> nil then begin
    FLastChildByName.ParentTypeInfo := nil;
    FLastChildByName.ReleaseCirclularReference;
    FLastChildByName := nil;
  end;
  inherited Destroy;
end;

procedure TFpDwarfSymbolTypeStructure.CreateMembers;
var
  Info: TDwarfInformationEntry;
  Info2: TDwarfInformationEntry;
  sym: TFpDwarfSymbol;
begin
  if FMembers <> nil then
    exit;
  FMembers := TFpDbgCircularRefCntObjList.Create;
  Info := InformationEntry.Clone;
  Info.GoChild;

  while Info.HasValidScope do begin
    if (Info.AbbrevTag = DW_TAG_member) or (Info.AbbrevTag = DW_TAG_subprogram) then begin
      Info2 := Info.Clone;
      sym := TFpDwarfSymbol.CreateSubClass('', Info2);
      FMembers.Add(sym);
      sym.ReleaseReference;
      sym.ParentTypeInfo := self;
      Info2.ReleaseReference;
    end;
    Info.GoNext;
  end;

  Info.ReleaseReference;
end;

procedure TFpDwarfSymbolTypeStructure.InitInheritanceInfo;
begin
  if FInheritanceInfo = nil then
    FInheritanceInfo := InformationEntry.FindChildByTag(DW_TAG_inheritance);
end;

function TFpDwarfSymbolTypeStructure.DoGetNestedTypeInfo: TFpDwarfSymbolType;
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
    Result := TFpDwarfSymbolType.CreateTypeSubClass('', ParentInfo);
    ParentInfo.ReleaseReference;
  end;
end;

procedure TFpDwarfSymbolTypeStructure.KindNeeded;
begin
  if (InformationEntry.AbbrevTag = DW_TAG_class_type) then
    SetKind(skClass)
  else
  begin
    if TypeInfo <> nil then // inheritance
      SetKind(skObject) // skClass
    else
    if MemberByName['_vptr$TOBJECT'] <> nil then
      SetKind(skObject) // skClass
    else
    if MemberByName['_vptr$'+Name] <> nil then
      SetKind(skObject)
    else
      SetKind(skRecord);
  end;
end;

function TFpDwarfSymbolTypeStructure.GetTypedValueObject(ATypeCast: Boolean): TFpDwarfValue;
begin
  if ATypeCast then
    Result := TFpDwarfValueStructTypeCast.Create(Self)
  else
    Result := TFpDwarfValueStruct.Create(Self);
end;

{ TFpDwarfSymbolTypeArray }

procedure TFpDwarfSymbolTypeArray.CreateMembers;
var
  Info, Info2: TDwarfInformationEntry;
  t: Cardinal;
  sym: TFpDwarfSymbol;
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
      sym := TFpDwarfSymbol.CreateSubClass('', Info2);
      FMembers.Add(sym);
      sym.ReleaseReference;
      sym.ParentTypeInfo := self;
      Info2.ReleaseReference;
    end;
    Info.GoNext;
  end;

  Info.ReleaseReference;
end;

procedure TFpDwarfSymbolTypeArray.ReadStride;
var
  t: TFpDwarfSymbolType;
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

procedure TFpDwarfSymbolTypeArray.ReadOrdering;
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

procedure TFpDwarfSymbolTypeArray.KindNeeded;
begin
  SetKind(skArray); // Todo: static/dynamic?
end;

function TFpDwarfSymbolTypeArray.GetTypedValueObject(ATypeCast: Boolean): TFpDwarfValue;
begin
  Result := TFpDwarfValueArray.Create(Self);
end;

function TFpDwarfSymbolTypeArray.GetFlags: TDbgSymbolFlags;
  function IsDynSubRange(m: TFpDwarfSymbol): Boolean;
  begin
    Result := sfSubRange in m.Flags;
    if not Result then exit;
    while (m <> nil) and not(m is TFpDwarfSymbolTypeSubRange) do
      m := m.NestedTypeInfo;
    Result := m <> nil;
    if not Result then exit; // TODO: should not happen, handle error
    Result := TFpDwarfSymbolTypeSubRange(m).FHighBoundState = rfValue; // dynamic high bound
  end;
var
  m: TFpDbgSymbol;
begin
  Result := inherited GetFlags;
  if (MemberCount = 1) then begin   // TODO: move to freepascal specific
    m := Member[0];
    if (not m.HasBounds) or                // e.g. Subrange with missing upper bound
       (m.OrdHighBound < m.OrdLowBound) or
       (IsDynSubRange(TFpDwarfSymbol(m)))
    then
      Result := Result + [sfDynArray]
    else
      Result := Result + [sfStatArray];
  end
  else
    Result := Result + [sfStatArray];
end;

function TFpDwarfSymbolTypeArray.GetMember(AIndex: Int64): TFpDbgSymbol;
begin
  CreateMembers;
  Result := TFpDbgSymbol(FMembers[AIndex]);
end;

function TFpDwarfSymbolTypeArray.GetMemberByName(AIndex: String): TFpDbgSymbol;
begin
  Result := nil; // no named members
end;

function TFpDwarfSymbolTypeArray.GetMemberCount: Integer;
begin
  CreateMembers;
  Result := FMembers.Count;
end;

function TFpDwarfSymbolTypeArray.GetMemberAddress(AValObject: TFpDwarfValue;
  const AIndex: array of Int64): TFpDbgMemLocation;
var
  Idx, Offs, Factor: Int64;
  LowBound, HighBound: int64;
  i: Integer;
  bsize: Integer;
  m: TFpDwarfSymbol;
begin
  assert((AValObject is TFpDwarfValueArray), 'TFpDwarfSymbolTypeArray.GetMemberAddress AValObject');
  ReadOrdering;
  ReadStride; // TODO Stride per member (member = dimension/index)
  Result := InvalidLoc;
  if (FStrideInBits <= 0) or (FStrideInBits mod 8 <> 0) then
    exit;

  CreateMembers;
  if Length(AIndex) > FMembers.Count then
    exit;

  if AValObject is TFpDwarfValueArray then begin
    if not TFpDwarfValueArray(AValObject).GetDwarfDataAddress(Result, Self) then begin
      Result := InvalidLoc;
      Exit;
    end;
  end
  else
    exit; // TODO error

  Offs := 0;
  Factor := 1;

  {$PUSH}{$R-}{$Q-} // TODO: check range of index
  bsize := FStrideInBits div 8;
  if FRowMajor then begin
    for i := Length(AIndex) - 1 downto 0 do begin
      Idx := AIndex[i];
      m := TFpDwarfSymbol(FMembers[i]);
      if ((m is TFpDwarfSymbolType) and (TFpDwarfSymbolType(m).GetValueBounds(AValObject, LowBound, HighBound))) or
         m.HasBounds then begin
        Idx := Idx - m.OrdLowBound;
      end;
      Offs := Offs + Idx * bsize * Factor;
      if i > 0 then begin
        if not m.HasBounds then begin
          Result := InvalidLoc;
          exit;
        end;
// TODO range check
        Factor := Factor * (m.OrdHighBound - m.OrdLowBound + 1);
      end;
    end;
  end
  else begin
    for i := 0 to Length(AIndex) - 1 do begin
      Idx := AIndex[i];
      m := TFpDwarfSymbol(FMembers[i]);
      if m.HasBounds then begin
        Idx := Idx - m.OrdLowBound;
      end;
      Offs := Offs + Idx * bsize * Factor;
      if i < Length(AIndex) - 1 then begin
        if not m.HasBounds then begin
          Result := InvalidLoc;
          exit;
        end;
        Factor := Factor * (m.OrdHighBound - m.OrdLowBound + 1);
      end;
    end;
  end;

  assert(IsTargetAddr(Result), 'DwarfArray MemberAddress');
  Result.Address := Result.Address + Offs;
  {$POP}
end;

destructor TFpDwarfSymbolTypeArray.Destroy;
var
  i: Integer;
begin
  if FMembers <> nil then begin
    for i := 0 to FMembers.Count - 1 do
      TFpDwarfSymbol(FMembers[i]).ParentTypeInfo := nil;
    FreeAndNil(FMembers);
  end;
  inherited Destroy;
end;

procedure TFpDwarfSymbolTypeArray.ResetValueBounds;
var
  i: Integer;
begin
  debuglnEnter(['TFpDwarfSymbolTypeArray.ResetValueBounds ' , Self.ClassName, dbgs(self)]); try
  inherited ResetValueBounds;
  FDwarfArrayReadFlags := [];
  if FMembers <> nil then
    for i := 0 to FMembers.Count - 1 do
      if TObject(FMembers[i]) is TFpDwarfSymbolType then
        TFpDwarfSymbolType(FMembers[i]).ResetValueBounds;
  finally debuglnExit(['TFpDwarfSymbolTypeArray.ResetValueBounds ' ]); end;
end;

{ TDbgDwarfSymbol }

constructor TFpDwarfSymbolValueProc.Create(ACompilationUnit: TDwarfCompilationUnit;
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

destructor TFpDwarfSymbolValueProc.Destroy;
begin
  FreeAndNil(FProcMembers);
  FLastMember.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FLastMember, 'TFpDwarfSymbolValueProc.FLastMember'){$ENDIF};
  FreeAndNil(FStateMachine);
  if FSelfParameter <> nil then begin
    //TDbgDwarfIdentifier(FSelfParameter.DbgSymbol).ParentTypeInfo := nil;
    FSelfParameter.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSelfParameter, 'FSelfParameter'){$ENDIF};
  end;
  inherited Destroy;
end;

function TFpDwarfSymbolValueProc.GetColumn: Cardinal;
begin
  if StateMachineValid
  then Result := FStateMachine.Column
  else Result := inherited GetColumn;
end;

function TFpDwarfSymbolValueProc.GetFile: String;
begin
  if StateMachineValid
  then Result := FStateMachine.FileName
  else Result := inherited GetFile;
end;

function TFpDwarfSymbolValueProc.GetLine: Cardinal;
begin
  if StateMachineValid
  then Result := FStateMachine.Line
  else Result := inherited GetLine;
end;

function TFpDwarfSymbolValueProc.GetValueObject: TFpDbgValue;
begin
  Result := FValueObject;
  if Result <> nil then exit;

  FValueObject := TFpDwarfValue.Create(nil);
  {$IFDEF WITH_REFCOUNT_DEBUG}FValueObject.DbgRenameReference(@FValueObject, ClassName+'.FValueObject');{$ENDIF}
  FValueObject.MakePlainRefToCirclular;
  FValueObject.SetValueSymbol(self);

  Result := FValueObject;
end;

function TFpDwarfSymbolValueProc.StateMachineValid: Boolean;
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

function TFpDwarfSymbolValueProc.ReadVirtuality(out AFlags: TDbgSymbolFlags): Boolean;
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

procedure TFpDwarfSymbolValueProc.CreateMembers;
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

function TFpDwarfSymbolValueProc.GetMember(AIndex: Int64): TFpDbgSymbol;
begin
  CreateMembers;
  FLastMember.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FLastMember, 'TFpDwarfSymbolValueProc.FLastMember'){$ENDIF};
  FLastMember := TFpDwarfSymbol.CreateSubClass('', TDwarfInformationEntry(FProcMembers[AIndex]));
  {$IFDEF WITH_REFCOUNT_DEBUG}FLastMember.DbgRenameReference(@FLastMember, 'TFpDwarfSymbolValueProc.FLastMember');{$ENDIF}
  Result := FLastMember;
end;

function TFpDwarfSymbolValueProc.GetMemberByName(AIndex: String): TFpDbgSymbol;
var
  Info: TDwarfInformationEntry;
  s, s2: String;
  i: Integer;
begin
  CreateMembers;
  s2 := LowerCase(AIndex);
  FLastMember.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FLastMember, 'TFpDwarfSymbolValueProc.FLastMember'){$ENDIF};
  FLastMember := nil;;
  for i := 0 to FProcMembers.Count - 1 do begin
    Info := TDwarfInformationEntry(FProcMembers[i]);
    if Info.ReadName(s) and (LowerCase(s) = s2) then begin
      FLastMember := TFpDwarfSymbol.CreateSubClass('', Info);
      {$IFDEF WITH_REFCOUNT_DEBUG}FLastMember.DbgRenameReference(@FLastMember, 'TFpDwarfSymbolValueProc.FLastMember');{$ENDIF}
      break;
    end;
  end;
  Result := FLastMember;
end;

function TFpDwarfSymbolValueProc.GetMemberCount: Integer;
begin
  CreateMembers;
  Result := FProcMembers.Count;
end;

function TFpDwarfSymbolValueProc.GetFrameBase(ASender: TDwarfLocationExpression): TDbgPtr;
var
  Val: TByteDynArray;
begin
  Result := 0;
  if FFrameBaseParser = nil then begin
    //TODO: avoid copying data
    if not  InformationEntry.ReadValue(DW_AT_frame_base, Val) then begin
      // error
      debugln(FPDBG_DWARF_ERRORS, ['TFpDwarfSymbolValueProc.GetFrameBase failed to read DW_AT_frame_base']);
      exit;
    end;
    if Length(Val) = 0 then begin
      // error
      debugln(FPDBG_DWARF_ERRORS, ['TFpDwarfSymbolValueProc.GetFrameBase failed to read DW_AT_location']);
      exit;
    end;

    FFrameBaseParser := TDwarfLocationExpression.Create(@Val[0], Length(Val), CompilationUnit,
      ASender.MemManager, ASender.Context);
    FFrameBaseParser.Evaluate;
  end;

  if FFrameBaseParser.ResultKind in [lseValue] then
    Result := FFrameBaseParser.ResultData;

  if IsError(FFrameBaseParser.LastError) then begin
    SetLastError(FFrameBaseParser.LastError);
    debugln(FPDBG_DWARF_ERRORS, ['TFpDwarfSymbolValueProc.GetFrameBase location parser failed ', ErrorHandler.ErrorAsString(LastError)]);
  end
  else
  if Result = 0 then begin
    debugln(FPDBG_DWARF_ERRORS, ['TFpDwarfSymbolValueProc.GetFrameBase location parser failed. result is 0']);
  end;

end;

procedure TFpDwarfSymbolValueProc.KindNeeded;
begin
  if TypeInfo <> nil then
    SetKind(skFunction)
  else
    SetKind(skProcedure);
end;

procedure TFpDwarfSymbolValueProc.SizeNeeded;
begin
  SetSize(FAddressInfo^.EndPC - FAddressInfo^.StartPC);
end;

function TFpDwarfSymbolValueProc.GetFlags: TDbgSymbolFlags;
var
  flg: TDbgSymbolFlags;
begin
  Result := inherited GetFlags;
  if ReadVirtuality(flg) then
    Result := Result + flg;
end;

function TFpDwarfSymbolValueProc.GetSelfParameter(AnAddress: TDbgPtr): TFpDwarfValue;
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
        Result := TFpDwarfValue(TFpDwarfSymbolValue.CreateValueSubClass('self', InfoEntry).Value);
        FSelfParameter := Result;
        FSelfParameter.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSelfParameter, 'FSelfParameter'){$ENDIF};
        FSelfParameter.DbgSymbol.ReleaseReference;
        //FSelfParameter.DbgSymbol.ParentTypeInfo := Self;
        debugln(FPDBG_DWARF_SEARCH, ['TFpDwarfSymbolValueProc.GetSelfParameter ', InfoEntry.ScopeDebugText, DbgSName(Result)]);
      end;
    end;
  end;
  InfoEntry.ReleaseReference;
end;

{ TFpDwarfSymbolValueVariable }

function TFpDwarfSymbolValueVariable.GetValueAddress(AValueObj: TFpDwarfValue; out
  AnAddress: TFpDbgMemLocation): Boolean;
var
  AttrData: TDwarfAttribData;
begin
  AnAddress := AValueObj.DataAddressCache[0];
  Result := IsValidLoc(AnAddress);
  if IsInitializedLoc(AnAddress) then
    exit;
  if InformationEntry.GetAttribData(DW_AT_location, AttrData) then
    Result := LocationFromTag(DW_AT_location, AttrData, AValueObj, AnAddress)
  else
    Result := ConstantFromTag(DW_AT_const_value, FConstData, AnAddress);
  AValueObj.DataAddressCache[0] := AnAddress;
end;

function TFpDwarfSymbolValueVariable.HasAddress: Boolean;
begin
  // TODO: THis is wrong. It might allow for the @ operator on a const...
  Result := InformationEntry.HasAttrib(DW_AT_location) or
            InformationEntry.HasAttrib(DW_AT_const_value);
end;

{ TFpDwarfSymbolValueParameter }

function TFpDwarfSymbolValueParameter.GetValueAddress(AValueObj: TFpDwarfValue; out
  AnAddress: TFpDbgMemLocation): Boolean;
begin
  AnAddress := AValueObj.DataAddressCache[0];
  Result := IsValidLoc(AnAddress);
  if IsInitializedLoc(AnAddress) then
    exit;
  Result := LocationFromTag(DW_AT_location, AValueObj, AnAddress);
  AValueObj.DataAddressCache[0] := AnAddress;
end;

function TFpDwarfSymbolValueParameter.HasAddress: Boolean;
begin
  Result := InformationEntry.HasAttrib(DW_AT_location);
end;

function TFpDwarfSymbolValueParameter.GetFlags: TDbgSymbolFlags;
begin
  Result := (inherited GetFlags) + [sfParameter];
end;

{ TFpDwarfSymbolUnit }

procedure TFpDwarfSymbolUnit.Init;
begin
  inherited Init;
  SetSymbolType(stNone);
  SetKind(skUnit);
end;

function TFpDwarfSymbolUnit.GetMemberByName(AIndex: String): TFpDbgSymbol;
var
  Ident: TDwarfInformationEntry;
begin
  // Todo, param to only search external.
  ReleaseRefAndNil(FLastChildByName);
  Result := nil;

  Ident := InformationEntry.Clone;
  Ident.GoNamedChildEx(AIndex);
  if Ident <> nil then
    Result := TFpDwarfSymbol.CreateSubClass('', Ident);
  // No need to set ParentTypeInfo
  ReleaseRefAndNil(Ident);
  FLastChildByName := Result;
end;

destructor TFpDwarfSymbolUnit.Destroy;
begin
  ReleaseRefAndNil(FLastChildByName);
  inherited Destroy;
end;

initialization
  DwarfSymbolClassMapList.SetDefaultMap(TFpDwarfDefaultSymbolClassMap);

  FPDBG_DWARF_VERBOSE       := DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_VERBOSE' {$IFDEF FPDBG_DWARF_VERBOSE} , True {$ENDIF} );
  FPDBG_DWARF_ERRORS        := DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_ERRORS' {$IFDEF FPDBG_DWARF_ERRORS} , True {$ENDIF} );
  FPDBG_DWARF_WARNINGS      := DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_WARNINGS' {$IFDEF FPDBG_DWARF_WARNINGS} , True {$ENDIF} );
  FPDBG_DWARF_SEARCH        := DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_SEARCH' {$IFDEF FPDBG_DWARF_SEARCH} , True {$ENDIF} );
  FPDBG_DWARF_DATA_WARNINGS := DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_DATA_WARNINGS' {$IFDEF FPDBG_DWARF_DATA_WARNINGS} , True {$ENDIF} );

end.

