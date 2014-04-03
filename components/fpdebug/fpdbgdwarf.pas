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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit FpDbgDwarf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, math, FpDbgInfo, FpDbgDwarfDataClasses, FpdMemoryTools, FpErrorMessages,
  FpDbgUtil, FpDbgDwarfConst, DbgIntfBaseTypes, LazUTF8, LazLoggerBase, LazClasses;

type
  TDbgDwarf = FpDbgDwarfDataClasses.TDbgDwarf;

  { TFpDwarfDefaultSymbolClassMap }

  TFpDwarfDefaultSymbolClassMap = class(TFpDwarfSymbolClassMap)
  public
    class function HandleCompUnit(ACU: TDwarfCompilationUnit): Boolean; override;
    class function GetDwarfSymbolClass(ATag: Cardinal): TDbgDwarfSymbolBaseClass; override;
    class function CreateContext(AnAddress: TDbgPtr; ASymbol: TFpDbgSymbol;
      ADwarf: TDbgDwarf): TDbgInfoAddressContext; override;
    class function CreateProcSymbol(ACompilationUnit: TDwarfCompilationUnit;
      AInfo: PDwarfAddressInfo; AAddress: TDbgPtr): TDbgDwarfSymbolBase; override;
  end;

  { TDbgDwarfInfoAddressContext }

  TDbgDwarfInfoAddressContext = class(TDbgInfoAddressContext)
  private
    FSymbol: TFpDbgSymbol;
    FAddress: TDBGPtr;
    FDwarf: TDbgDwarf;
  protected
    function GetSymbolAtAddress: TFpDbgSymbol; override;
    function GetAddress: TDbgPtr; override;
    function GetSizeOfAddress: Integer; override;
    function GetMemManager: TFpDbgMemManager; override;

    property Symbol: TFpDbgSymbol read FSymbol;
    property Address: TDBGPtr read FAddress;
    property Dwarf: TDbgDwarf read FDwarf;
    function FindExportedSymbolInUnits(const AName: String; PNameUpper, PNameLower: PChar;
      SkipCompUnit: TDwarfCompilationUnit): TFpDbgSymbol; inline;
    function FindSymbolInStructure(const AName: String; PNameUpper, PNameLower: PChar;
      InfoEntry: TDwarfInformationEntry): TFpDbgSymbol; inline;
    // FindLocalSymbol: for the subroutine itself
    function FindLocalSymbol(const AName: String; PNameUpper, PNameLower: PChar;
      InfoEntry: TDwarfInformationEntry): TFpDbgSymbol; virtual;
  public
    constructor Create(AnAddress: TDbgPtr; ASymbol: TFpDbgSymbol; ADwarf: TDbgDwarf);
    destructor Destroy; override;
    function FindSymbol(const AName: String): TFpDbgSymbol; override;
  end;

  TDbgDwarfIdentifier = class;
  TDbgDwarfTypeIdentifier = class;
  TDbgDwarfValueIdentifier = class;
  TDbgDwarfIdentifierStructure = class;
  //TDbgDwarfIdentifierClass = class of TDbgDwarfIdentifier;
  TDbgDwarfValueIdentifierClass = class of TDbgDwarfValueIdentifier;
  TDbgDwarfTypeIdentifierClass = class of TDbgDwarfTypeIdentifier;

{%region Value objects }
  { TFpDbgDwarfValue }

  TFpDbgDwarfValue = class(TFpDbgValue)
  private
    FOwner: TDbgDwarfTypeIdentifier;        // the creator, usually the type
    FValueSymbol: TDbgDwarfValueIdentifier;
    FTypeCastTargetType: TDbgDwarfTypeIdentifier;
    FTypeCastSourceValue: TFpDbgValue;

    FDataAddressCache: array of TFpDbgMemLocation;
    FStructureValue: TFpDbgDwarfValue;
    FLastMember: TFpDbgDwarfValue;
    FLastError: TFpError;
    function GetDataAddressCache(AIndex: Integer): TFpDbgMemLocation;
    function MemManager: TFpDbgMemManager; inline;
    function AddressSize: Byte; inline;
    procedure SetDataAddressCache(AIndex: Integer; AValue: TFpDbgMemLocation);
    procedure SetStructureValue(AValue: TFpDbgDwarfValue);
  protected
    procedure DoReferenceAdded; override;
    procedure DoReferenceReleased; override;
    procedure CircleBackRefActiveChanged(NewActive: Boolean); override;
    procedure SetLastMember(ALastMember: TFpDbgDwarfValue);
    function GetLastError: TFpError; override;

    // Address of the symbol (not followed any type deref, or location)
    function GetAddress: TFpDbgMemLocation; override;
    function OrdOrAddress: TFpDbgMemLocation;
    // Address of the data (followed type deref, location, ...)
    function DataAddr: TFpDbgMemLocation;
    function OrdOrDataAddr: TFpDbgMemLocation;
    function GetDwarfDataAddress(out AnAddress: TFpDbgMemLocation; ATargetType: TDbgDwarfTypeIdentifier = nil): Boolean;
    function GetStructureDwarfDataAddress(out AnAddress: TFpDbgMemLocation;
                                          ATargetType: TDbgDwarfTypeIdentifier = nil): Boolean;
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
  public
    constructor Create(AOwner: TDbgDwarfTypeIdentifier);
    destructor Destroy; override;
    procedure SetValueSymbol(AValueSymbol: TDbgDwarfValueIdentifier);
    function  SetTypeCastInfo(AStructure: TDbgDwarfTypeIdentifier;
                              ASource: TFpDbgValue): Boolean; // Used for Typecast
    // StructureValue: Any Value returned via GetMember points to its structure
    property StructureValue: TFpDbgDwarfValue read FStructureValue write SetStructureValue;
    // DataAddressCache[0]: ValueAddress // DataAddressCache[1..n]: DataAddress
    property DataAddressCache[AIndex: Integer]: TFpDbgMemLocation read GetDataAddressCache write SetDataAddressCache;
  end;

  { TFpDbgDwarfValueSized }

  TFpDbgDwarfValueSized = class(TFpDbgDwarfValue)
  private
    FSize: Integer;
  protected
    function CanUseTypeCastAddress: Boolean;
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetSize: Integer; override;
  public
    constructor Create(AOwner: TDbgDwarfTypeIdentifier; ASize: Integer);
  end;

  { TFpDbgDwarfValueNumeric }

  TFpDbgDwarfValueNumeric = class(TFpDbgDwarfValueSized)
  protected
    FEvaluated: set of (doneUInt, doneInt, doneAddr, doneFloat);
  protected
    procedure Reset; override;
    function GetFieldFlags: TFpDbgValueFieldFlags; override; // svfOrdinal
    function IsValidTypeCast: Boolean; override;
  public
    constructor Create(AOwner: TDbgDwarfTypeIdentifier; ASize: Integer);
  end;

  { TFpDbgDwarfValueInteger }

  TFpDbgDwarfValueInteger = class(TFpDbgDwarfValueNumeric)
  private
    FIntValue: Int64;
  protected
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetAsCardinal: QWord; override;
    function GetAsInteger: Int64; override;
  end;

  { TDbgDwarfValueCardinal } // xxxxxxxxxx TODO fix name

  TDbgDwarfValueCardinal = class(TFpDbgDwarfValueNumeric)
  private
    FValue: QWord;
  protected
    function GetAsCardinal: QWord; override;
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
  end;

  { TFpDbgDwarfValueFloat }

  TFpDbgDwarfValueFloat = class(TFpDbgDwarfValueNumeric) // TDbgDwarfSymbolValue
  // TODO: typecasts to int should convert
  private
    FValue: Extended;
  protected
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetAsFloat: Extended; override;
  end;

  { TFpDbgDwarfValueBoolean }

  TFpDbgDwarfValueBoolean = class(TDbgDwarfValueCardinal)
  protected
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetAsBool: Boolean; override;
  end;

  { TFpDbgDwarfValueChar }

  TFpDbgDwarfValueChar = class(TDbgDwarfValueCardinal)
  protected
    // returns single char(byte) / widechar
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetAsString: AnsiString; override;
    function GetAsWideString: WideString; override;
  end;

  { TFpDbgDwarfValuePointer }

  TFpDbgDwarfValuePointer = class(TFpDbgDwarfValueNumeric)
  private
    FPointetToAddr: TFpDbgMemLocation;
  protected
    function GetAsCardinal: QWord; override;
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetDataAddress: TFpDbgMemLocation; override;
    function GetAsString: AnsiString; override;
  end;

  { TFpDbgDwarfValueEnum }

  TFpDbgDwarfValueEnum = class(TFpDbgDwarfValueNumeric)
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

  { TFpDbgDwarfValueEnumMember }

  TFpDbgDwarfValueEnumMember = class(TFpDbgDwarfValue)
  private
    FOwnerVal: TDbgDwarfValueIdentifier;
  protected
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetAsCardinal: QWord; override;
    function GetAsString: AnsiString; override;
    function IsValidTypeCast: Boolean; override;
  public
    constructor Create(AOwner: TDbgDwarfValueIdentifier);
  end;

  { TFpDbgDwarfValueConstNumber }

  TFpDbgDwarfValueConstNumber = class(TFpDbgValueConstNumber)
  protected
    procedure Update(AValue: QWord; ASigned: Boolean);
  end;

  { TFpDbgDwarfValueSet }

  TFpDbgDwarfValueSet = class(TFpDbgDwarfValueSized)
  private
    FMem: array of Byte;
    FMemberCount: Integer;
    FMemberMap: array of Integer;
    FNumValue: TFpDbgDwarfValueConstNumber;
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

  { TFpDbgDwarfValueStruct }

  TFpDbgDwarfValueStruct = class(TFpDbgDwarfValue)
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

  { TFpDbgDwarfValueStructTypeCast }

  TFpDbgDwarfValueStructTypeCast = class(TFpDbgDwarfValue)
  private
    FMembers: TFpDbgCircularRefCntObjList;
    FDataAddress: TFpDbgMemLocation;
    FDataAddressDone: Boolean;
  protected
    procedure Reset; override;
    procedure ClearMembers;
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

  { TFpDbgDwarfValueConstAddress }

  TFpDbgDwarfValueConstAddress = class(TFpDbgValueConstAddress)
  protected
    procedure Update(AnAddress: TFpDbgMemLocation);
  end;

  { TFpDbgDwarfValueArray }

  TFpDbgDwarfValueArray = class(TFpDbgDwarfValue)
  private
    FAddrObj: TFpDbgDwarfValueConstAddress;
  protected
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetKind: TDbgSymbolKind; override;
    function GetAsCardinal: QWord; override;
    function GetDataAddress: TFpDbgMemLocation; override;
    function GetMember(AIndex: Int64): TFpDbgValue; override;
    function GetMemberEx(AIndex: array of Int64): TFpDbgValue; override;
    function GetMemberCount: Integer; override;
    function GetMemberCountEx(AIndex: array of Int64): Integer; override;
    function GetIndexType(AIndex: Integer): TFpDbgSymbol; override;
    function GetIndexTypeCount: Integer; override;
    function IsValidTypeCast: Boolean; override;
  public
    destructor Destroy; override;
  end;
{%endregion Value objects }

{%region Symbol objects }
  { TDbgDwarfIdentifier }

  TDbgDwarfIdentifier = class(TDbgDwarfSymbolBase)
  private
    FNestedTypeInfo: TDbgDwarfTypeIdentifier;
    FParentTypeInfo: TDbgDwarfIdentifier;
    FDwarfReadFlags: set of (didtNameRead, didtTypeRead, didtArtificialRead, didtIsArtifical);
    function GetNestedTypeInfo: TDbgDwarfTypeIdentifier;
  protected
    (* There will be a circular reference between parenttype and self
       "self" will only set its reference to parenttype, if self has other references.  *)
    procedure DoReferenceAdded; override;
    procedure DoReferenceReleased; override;
    procedure CircleBackRefActiveChanged(ANewActive: Boolean); override;
    procedure SetParentTypeInfo(AValue: TDbgDwarfIdentifier); virtual;

    function  DoGetNestedTypeInfo: TDbgDwarfTypeIdentifier; virtual;
    function  ReadMemberVisibility(out AMemberVisibility: TDbgSymbolMemberVisibility): Boolean;
    function  IsArtificial: Boolean; // usud by formal param and subprogram
    procedure NameNeeded; override;
    procedure TypeInfoNeeded; override;
    property NestedTypeInfo: TDbgDwarfTypeIdentifier read GetNestedTypeInfo;

    // OwnerTypeInfo: reverse of "NestedTypeInfo" (variable that is of this type)
//    property OwnerTypeInfo: TDbgDwarfIdentifier read FOwnerTypeInfo; // write SetOwnerTypeInfo;
    // ParentTypeInfo: funtion for local var / class for member
    property ParentTypeInfo: TDbgDwarfIdentifier read FParentTypeInfo write SetParentTypeInfo;

    function DataSize: Integer; virtual;
  protected
    function InitLocationParser(const {%H-}ALocationParser: TDwarfLocationExpression;
                                AValueObj: TFpDbgDwarfValue;
                                {%H-}AnObjectDataAddress: TFpDbgMemLocation): Boolean; virtual;
    function  LocationFromTag(ATag: Cardinal; AValueObj: TFpDbgDwarfValue;
                              out AnAddress: TFpDbgMemLocation;
                              AnObjectDataAddress: TFpDbgMemLocation;
                              AnInformationEntry: TDwarfInformationEntry = nil
                             ): Boolean;
    // GetDataAddress: data of a class, or string
    function GetDataAddress(AValueObj: TFpDbgDwarfValue; var AnAddress: TFpDbgMemLocation;
                            ATargetType: TDbgDwarfTypeIdentifier; ATargetCacheIndex: Integer): Boolean; virtual;
    function HasAddress: Boolean; virtual;

    procedure Init; override;
  public
    class function CreateSubClass(AName: String; AnInformationEntry: TDwarfInformationEntry): TDbgDwarfIdentifier;
    destructor Destroy; override;
    function StartScope: TDbgPtr; // return 0, if none. 0 includes all anyway
  end;

  { TDbgDwarfValueIdentifier }

  TDbgDwarfValueIdentifier = class(TDbgDwarfIdentifier) // var, const, member, ...
  private
    // StructureValueInfo, Member and subproc may need containing class
    FStructureValueInfo: TDbgDwarfIdentifier;
    procedure SetStructureValueInfo(AValue: TDbgDwarfIdentifier);
  protected
    FValueObject: TFpDbgDwarfValue;
    FMembers: TFpDbgCircularRefCntObjList;

    procedure CircleBackRefActiveChanged(ANewActive: Boolean); override;
    procedure SetParentTypeInfo(AValue: TDbgDwarfIdentifier); override;
    function GetValueAddress({%H-}AValueObj: TFpDbgDwarfValue;{%H-} out AnAddress: TFpDbgMemLocation): Boolean; virtual;
    function GetValueDataAddress(AValueObj: TFpDbgDwarfValue; out AnAddress: TFpDbgMemLocation;
                                 ATargetType: TDbgDwarfTypeIdentifier = nil): Boolean;
    procedure KindNeeded; override;
    procedure MemberVisibilityNeeded; override;
    function GetMember(AIndex: Int64): TFpDbgSymbol; override;
    function GetMemberByName(AIndex: String): TFpDbgSymbol; override;
    function GetMemberCount: Integer; override;

    procedure Init; override;
  public
    destructor Destroy; override;
    class function CreateValueSubClass(AName: String; AnInformationEntry: TDwarfInformationEntry): TDbgDwarfValueIdentifier;

    property StructureValueInfo: TDbgDwarfIdentifier read FStructureValueInfo write SetStructureValueInfo;
  end;

  { TDbgDwarfValueLocationIdentifier }

  TDbgDwarfValueLocationIdentifier = class(TDbgDwarfValueIdentifier)
  private
    procedure FrameBaseNeeded(ASender: TObject);
  protected
    function GetValueObject: TFpDbgValue; override;
    function InitLocationParser(const ALocationParser: TDwarfLocationExpression;
                                AValueObj: TFpDbgDwarfValue;
                                AnObjectDataAddress: TFpDbgMemLocation): Boolean; override;
  end;

  { TDbgDwarfTypeIdentifier }

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

  TDbgDwarfTypeIdentifier = class(TDbgDwarfIdentifier)
  protected
    procedure Init; override;
    procedure MemberVisibilityNeeded; override;
    procedure SizeNeeded; override;
    function GetTypedValueObject({%H-}ATypeCast: Boolean): TFpDbgDwarfValue; virtual; // returns refcount=1 for caller, no cached copy kept
    // TODO: flag bounds as cardinal if needed
    function GetValueBounds(AValueObj: TFpDbgDwarfValue; out ALowBound, AHighBound: Int64): Boolean; virtual;
  public
    class function CreateTypeSubClass(AName: String; AnInformationEntry: TDwarfInformationEntry): TDbgDwarfTypeIdentifier;
    function TypeCastValue(AValue: TFpDbgValue): TFpDbgValue; override;
  end;

  { TDbgDwarfBaseIdentifierBase }

  TDbgDwarfBaseIdentifierBase = class(TDbgDwarfTypeIdentifier)
  //function DoGetNestedTypeInfo: TDbgDwarfTypeIdentifier; // return nil
  protected
    procedure KindNeeded; override;
    procedure TypeInfoNeeded; override;
    function GetTypedValueObject({%H-}ATypeCast: Boolean): TFpDbgDwarfValue; override;
    function GetHasBounds: Boolean; override;
    function GetOrdHighBound: Int64; override;
    function GetOrdLowBound: Int64; override;
  end;

  { TDbgDwarfTypeIdentifierModifier }

  TDbgDwarfTypeIdentifierModifier = class(TDbgDwarfTypeIdentifier)
  protected
    procedure TypeInfoNeeded; override;
    procedure ForwardToSymbolNeeded; override;
    function GetTypedValueObject(ATypeCast: Boolean): TFpDbgDwarfValue; override;
  end;

  { TDbgDwarfTypeIdentifierRef }

  TDbgDwarfTypeIdentifierRef = class(TDbgDwarfTypeIdentifierModifier)
  protected
    function GetFlags: TDbgSymbolFlags; override;
    function GetDataAddress(AValueObj: TFpDbgDwarfValue; var AnAddress: TFpDbgMemLocation;
                            ATargetType: TDbgDwarfTypeIdentifier; ATargetCacheIndex: Integer): Boolean; override;
  end;

  { TDbgDwarfTypeIdentifierDeclaration }

  TDbgDwarfTypeIdentifierDeclaration = class(TDbgDwarfTypeIdentifierModifier)
  protected
    // fpc encodes classes as pointer, not ref (so Obj1 = obj2 compares the pointers)
    // typedef > pointer > srtuct
    // while a pointer to class/object: pointer > typedef > ....
    function DoGetNestedTypeInfo: TDbgDwarfTypeIdentifier; override;
  end;

  { TDbgDwarfIdentifierSubRange }
  TDbgDwarfSubRangeBoundReadState = (rfNotRead, rfNotFound, rfConst, rfValue);

  TDbgDwarfIdentifierSubRange = class(TDbgDwarfTypeIdentifierModifier)
  // TODO not a modifier, maybe have a forwarder base class
  private
    FLowBoundConst: Int64;
    FLowBoundValue: TDbgDwarfValueIdentifier;
    FLowBoundState: TDbgDwarfSubRangeBoundReadState;
    FHighBoundConst: Int64;
    FHighBoundValue: TDbgDwarfValueIdentifier;
    FHighBoundState: TDbgDwarfSubRangeBoundReadState;
    FCountConst: Int64;
    FCountValue: TDbgDwarfValueIdentifier;
    FCountState: TDbgDwarfSubRangeBoundReadState;
    FLowEnumIdx, FHighEnumIdx: Integer;
    FEnumIdxValid: Boolean;
    procedure InitEnumIdx;
    procedure ReadBounds;
  protected
    function DoGetNestedTypeInfo: TDbgDwarfTypeIdentifier;override;
    function GetHasBounds: Boolean; override;
    function GetOrdHighBound: Int64; override;
    function GetOrdLowBound: Int64; override;

    procedure NameNeeded; override;
    procedure KindNeeded; override;
    procedure SizeNeeded; override;
    function GetMember(AIndex: Int64): TFpDbgSymbol; override;
    function GetMemberCount: Integer; override;
    function GetFlags: TDbgSymbolFlags; override;
    function GetValueBounds(AValueObj: TFpDbgDwarfValue; out ALowBound,
      AHighBound: Int64): Boolean; override;
    procedure Init; override;
  end;

  { TDbgDwarfTypeIdentifierPointer }

  TDbgDwarfTypeIdentifierPointer = class(TDbgDwarfTypeIdentifier)
  private
    FIsInternalPointer: Boolean;
    function GetIsInternalPointer: Boolean; inline;
    function IsInternalDynArrayPointer: Boolean; inline;
  protected
    procedure TypeInfoNeeded; override;
    procedure KindNeeded; override;
    procedure SizeNeeded; override;
    procedure ForwardToSymbolNeeded; override;
    function GetDataAddress(AValueObj: TFpDbgDwarfValue; var AnAddress: TFpDbgMemLocation;
                            ATargetType: TDbgDwarfTypeIdentifier; ATargetCacheIndex: Integer): Boolean; override;
    function GetTypedValueObject(ATypeCast: Boolean): TFpDbgDwarfValue; override;
    function DataSize: Integer; override;
  public
    property IsInternalPointer: Boolean read GetIsInternalPointer write FIsInternalPointer; // Class (also DynArray, but DynArray is handled without this)
  end;

  { TDbgDwarfIdentifierEnumMember }

  TDbgDwarfIdentifierEnumMember  = class(TDbgDwarfValueIdentifier)
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


  { TDbgDwarfIdentifierEnum }

  TDbgDwarfIdentifierEnum = class(TDbgDwarfTypeIdentifier)
  private
    FMembers: TFpDbgCircularRefCntObjList;
    procedure CreateMembers;
  protected
    function GetTypedValueObject({%H-}ATypeCast: Boolean): TFpDbgDwarfValue; override;
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


  { TDbgDwarfIdentifierSet }

  TDbgDwarfIdentifierSet = class(TDbgDwarfTypeIdentifier)
  protected
    procedure KindNeeded; override;
    function GetTypedValueObject({%H-}ATypeCast: Boolean): TFpDbgDwarfValue; override;
    function GetMemberCount: Integer; override;
    function GetMember(AIndex: Int64): TFpDbgSymbol; override;
  end;

  (*
    If not specified
         .NestedTypeInfo --> copy of TypeInfo
         .ParentTypeInfo --> nil

    ParentTypeInfo:     has a weak RefCount (only AddRef, if self has other refs)


    AnObject = TDbgDwarfIdentifierVariable
     |-- .TypeInfo       --> TBar = TDbgDwarfIdentifierStructure  [*1]
     |-- .ParentTypeInfo --> may point to subroutine, if param or local var // TODO

    TBar = TDbgDwarfIdentifierStructure
     |-- .TypeInfo       --> TBarBase = TDbgDwarfIdentifierStructure

    TBarBase = TDbgDwarfIdentifierStructure
     |-- .TypeInfo       --> TOBject = TDbgDwarfIdentifierStructure

    TObject = TDbgDwarfIdentifierStructure
     |-- .TypeInfo       --> nil


    FField = TDbgDwarfIdentifierMember (declared in TBarBase)
     |-- .TypeInfo       --> Integer = TDbgDwarfBaseIdentifierBase [*1]
     |-- .ParentTypeInfo --> TBarBase

    [*1] May have TDbgDwarfTypeIdentifierDeclaration or others
  *)

  { TDbgDwarfIdentifierMember }

  TDbgDwarfIdentifierMember = class(TDbgDwarfValueLocationIdentifier)
  protected
    function InitLocationParser(const ALocationParser: TDwarfLocationExpression;
                                AValueObj: TFpDbgDwarfValue;
                                AnObjectDataAddress: TFpDbgMemLocation): Boolean; override;
    function GetValueAddress(AValueObj: TFpDbgDwarfValue; out AnAddress: TFpDbgMemLocation): Boolean; override;
    function HasAddress: Boolean; override;
  end;

  { TDbgDwarfIdentifierStructure }

  TDbgDwarfIdentifierStructure = class(TDbgDwarfTypeIdentifier)
  // record or class
  private
    FMembers: TFpDbgCircularRefCntObjList;
    FLastChildByName: TDbgDwarfIdentifier;
    FInheritanceInfo: TDwarfInformationEntry;
    procedure CreateMembers;
    procedure InitInheritanceInfo; inline;
  protected
    function DoGetNestedTypeInfo: TDbgDwarfTypeIdentifier; override;
    procedure KindNeeded; override;
    function GetTypedValueObject(ATypeCast: Boolean): TFpDbgDwarfValue; override;

    // GetMember, if AIndex > Count then parent
    function GetMember(AIndex: Int64): TFpDbgSymbol; override;
    function GetMemberByName(AIndex: String): TFpDbgSymbol; override;
    function GetMemberCount: Integer; override;

    function InitLocationParser(const ALocationParser: TDwarfLocationExpression;
                                AValueObj: TFpDbgDwarfValue;
                                AnObjectDataAddress: TFpDbgMemLocation): Boolean; override;
    function GetDataAddress(AValueObj: TFpDbgDwarfValue; var AnAddress: TFpDbgMemLocation;
                            ATargetType: TDbgDwarfTypeIdentifier; ATargetCacheIndex: Integer): Boolean; override;
  public
    destructor Destroy; override;
  end;

  { TDbgDwarfIdentifierArray }

  TDbgDwarfIdentifierArray = class(TDbgDwarfTypeIdentifier)
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
    function GetTypedValueObject({%H-}ATypeCast: Boolean): TFpDbgDwarfValue; override;

    function GetFlags: TDbgSymbolFlags; override;
    // GetMember: returns the TYPE/range of each index. NOT the data
    function GetMember(AIndex: Int64): TFpDbgSymbol; override;
    function GetMemberByName({%H-}AIndex: String): TFpDbgSymbol; override;
    function GetMemberCount: Integer; override;
    function GetMemberAddress(AValObject: TFpDbgDwarfValue; AIndex: Array of Int64): TFpDbgMemLocation;
  public
    destructor Destroy; override;
  end;

  { TDbgDwarfProcSymbol }

  TDbgDwarfProcSymbol = class(TDbgDwarfValueIdentifier)
  private
    //FCU: TDwarfCompilationUnit;
    FAddress: TDbgPtr;
    FAddressInfo: PDwarfAddressInfo;
    FStateMachine: TDwarfLineInfoStateMachine;
    FFrameBaseParser: TDwarfLocationExpression;
    FSelfParameter: TDbgDwarfValueIdentifier;
    function StateMachineValid: Boolean;
    function  ReadVirtuality(out AFlags: TDbgSymbolFlags): Boolean;
  protected
    function  GetFrameBase: TDbgPtr;
    procedure KindNeeded; override;
    procedure SizeNeeded; override;
    function GetFlags: TDbgSymbolFlags; override;
    function GetSelfParameter(AnAddress: TDbgPtr = 0): TDbgDwarfValueIdentifier;

    function GetColumn: Cardinal; override;
    function GetFile: String; override;
//    function GetFlags: TDbgSymbolFlags; override;
    function GetLine: Cardinal; override;
  public
    constructor Create(ACompilationUnit: TDwarfCompilationUnit; AInfo: PDwarfAddressInfo; AAddress: TDbgPtr); overload;
    destructor Destroy; override;
    // TODO members = locals ?
  end;

  { TDbgDwarfIdentifierVariable }

  TDbgDwarfIdentifierVariable = class(TDbgDwarfValueLocationIdentifier)
  protected
    function GetValueAddress(AValueObj: TFpDbgDwarfValue; out AnAddress: TFpDbgMemLocation): Boolean; override;
    function HasAddress: Boolean; override;
  public
  end;

  { TDbgDwarfIdentifierParameter }

  TDbgDwarfIdentifierParameter = class(TDbgDwarfValueLocationIdentifier)
  protected
    function GetValueAddress(AValueObj: TFpDbgDwarfValue; out AnAddress: TFpDbgMemLocation): Boolean; override;
    function HasAddress: Boolean; override;
  public
  end;

  { TDbgDwarfUnit }

  TDbgDwarfUnit = class(TDbgDwarfIdentifier)
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
  FPDBG_DWARF_ERRORS, FPDBG_DWARF_WARNINGS, FPDBG_DWARF_SEARCH, FPDBG_DWARF_DATA_WARNINGS: PLazLoggerLogGroup;

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
      Result := TDbgDwarfValueIdentifier;
    DW_TAG_string_type,
    DW_TAG_union_type, DW_TAG_ptr_to_member_type,
    DW_TAG_file_type,
    DW_TAG_thrown_type, DW_TAG_subroutine_type:
      Result := TDbgDwarfTypeIdentifier;

    // Type types
    DW_TAG_packed_type,
    DW_TAG_const_type,
    DW_TAG_volatile_type:    Result := TDbgDwarfTypeIdentifierModifier;
    DW_TAG_reference_type:   Result := TDbgDwarfTypeIdentifierRef;
    DW_TAG_typedef:          Result := TDbgDwarfTypeIdentifierDeclaration;
    DW_TAG_pointer_type:     Result := TDbgDwarfTypeIdentifierPointer;

    DW_TAG_base_type:        Result := TDbgDwarfBaseIdentifierBase;
    DW_TAG_subrange_type:    Result := TDbgDwarfIdentifierSubRange;
    DW_TAG_enumeration_type: Result := TDbgDwarfIdentifierEnum;
    DW_TAG_enumerator:       Result := TDbgDwarfIdentifierEnumMember;
    DW_TAG_set_type:         Result := TDbgDwarfIdentifierSet;
    DW_TAG_structure_type,
    DW_TAG_class_type:       Result := TDbgDwarfIdentifierStructure;
    DW_TAG_array_type:       Result := TDbgDwarfIdentifierArray;
    // Value types
    DW_TAG_variable:         Result := TDbgDwarfIdentifierVariable;
    DW_TAG_formal_parameter: Result := TDbgDwarfIdentifierParameter;
    DW_TAG_member:           Result := TDbgDwarfIdentifierMember;
    DW_TAG_subprogram:       Result := TDbgDwarfProcSymbol;
    //
    DW_TAG_compile_unit:     Result := TDbgDwarfUnit;

    else
      Result := TDbgDwarfIdentifier;
  end;
end;

class function TFpDwarfDefaultSymbolClassMap.CreateContext(AnAddress: TDbgPtr;
  ASymbol: TFpDbgSymbol; ADwarf: TDbgDwarf): TDbgInfoAddressContext;
begin
  Result := TDbgDwarfInfoAddressContext.Create(AnAddress, ASymbol, ADwarf);
end;

class function TFpDwarfDefaultSymbolClassMap.CreateProcSymbol(ACompilationUnit: TDwarfCompilationUnit;
  AInfo: PDwarfAddressInfo; AAddress: TDbgPtr): TDbgDwarfSymbolBase;
begin
  Result := TDbgDwarfProcSymbol.Create(ACompilationUnit, AInfo, AAddress);
end;

{ TDbgDwarfInfoAddressContext }

function TDbgDwarfInfoAddressContext.GetSymbolAtAddress: TFpDbgSymbol;
begin
  Result := FSymbol;
end;

function TDbgDwarfInfoAddressContext.GetAddress: TDbgPtr;
begin
  Result := FAddress;
end;

function TDbgDwarfInfoAddressContext.GetSizeOfAddress: Integer;
begin
  assert(FSymbol is TDbgDwarfIdentifier, 'TDbgDwarfInfoAddressContext.GetSizeOfAddress');
  Result := TDbgDwarfIdentifier(FSymbol).CompilationUnit.AddressSize;
end;

function TDbgDwarfInfoAddressContext.GetMemManager: TFpDbgMemManager;
begin
  Result := FDwarf.MemManager;
end;

function TDbgDwarfInfoAddressContext.FindExportedSymbolInUnits(const AName: String; PNameUpper,
  PNameLower: PChar; SkipCompUnit: TDwarfCompilationUnit): TFpDbgSymbol;
var
  i, ExtVal: Integer;
  CU: TDwarfCompilationUnit;
  InfoEntry: TDwarfInformationEntry;
  s: String;
begin
  Result := nil;
  InfoEntry := nil;
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
      Result.ReleaseReference;
      Result := TDbgDwarfIdentifier.CreateSubClass(AName, InfoEntry);
      break;
    end;

    CU.ScanAllEntries;
    if InfoEntry.GoNamedChildEx(PNameUpper, PNameLower) then begin
      if InfoEntry.IsAddressInStartScope(FAddress) then begin
        // only variables are marked "external", but types not / so we may need all top level
        Result.ReleaseReference;
        Result := TDbgDwarfIdentifier.CreateSubClass(AName, InfoEntry);
        //DebugLn(FPDBG_DWARF_SEARCH, ['TDbgDwarf.FindIdentifier MAYBE FOUND Name=', CU.FileName]);

        // DW_AT_visibility ?
        if InfoEntry.ReadValue(DW_AT_external, ExtVal) then
          if ExtVal <> 0 then
            break;
          // Search for better result
      end;
    end;
  end;

  InfoEntry.ReleaseReference;
end;

function TDbgDwarfInfoAddressContext.FindSymbolInStructure(const AName: String; PNameUpper,
  PNameLower: PChar; InfoEntry: TDwarfInformationEntry): TFpDbgSymbol;
var
  InfoEntryInheritance: TDwarfInformationEntry;
  FwdInfoPtr: Pointer;
  FwdCompUint: TDwarfCompilationUnit;
  SelfParam: TDbgDwarfValueIdentifier;
begin
  Result := nil;
  InfoEntry.AddReference;

  while True do begin
    if not InfoEntry.IsAddressInStartScope(FAddress) then
      break;

    InfoEntryInheritance := InfoEntry.FindChildByTag(DW_TAG_inheritance);

    if InfoEntry.GoNamedChildEx(PNameUpper, PNameLower) then begin
      if InfoEntry.IsAddressInStartScope(FAddress) then begin
        SelfParam := TDbgDwarfProcSymbol(FSymbol).GetSelfParameter(FAddress);
        if (SelfParam <> nil) then begin
          // TODO: only valid, as long as context is valid, because if comnext is freed, then self is lost too
          Result := SelfParam.MemberByName[AName];
          assert(Result <> nil, 'FindSymbol: SelfParam.MemberByName[AName]');
          if Result <> nil then
            Result.AddReference;
          if Result = nil then debugln(['TDbgDwarfInfoAddressContext.FindSymbol  NOT IN SELF !!!!!!!!!!!!!']);
        end
else debugln(['TDbgDwarfInfoAddressContext.FindSymbol XXXXXXXXXXXXX no self']);
        ;
        if Result = nil then  // Todo: abort the searh /SetError
          Result := TDbgDwarfIdentifier.CreateSubClass(AName, InfoEntry);
        InfoEntry.ReleaseReference;
        InfoEntryInheritance.ReleaseReference;
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
end;

function TDbgDwarfInfoAddressContext.FindLocalSymbol(const AName: String; PNameUpper,
  PNameLower: PChar; InfoEntry: TDwarfInformationEntry): TFpDbgSymbol;
begin
  Result := nil;
  if not InfoEntry.GoNamedChildEx(PNameUpper, PNameLower) then
    exit;
  if InfoEntry.IsAddressInStartScope(FAddress) and not InfoEntry.IsArtificial then begin
    Result := TDbgDwarfIdentifier.CreateSubClass(AName, InfoEntry);
    TDbgDwarfIdentifier(Result).ParentTypeInfo := TDbgDwarfProcSymbol(FSymbol);
  end;
end;

constructor TDbgDwarfInfoAddressContext.Create(AnAddress: TDbgPtr; ASymbol: TFpDbgSymbol;
  ADwarf: TDbgDwarf);
begin
  inherited Create;
  AddReference;
  FAddress := AnAddress;
  FDwarf   := ADwarf;
  FSymbol  := ASymbol;
  FSymbol.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSymbol, 'Context to Symbol'){$ENDIF};
end;

destructor TDbgDwarfInfoAddressContext.Destroy;
begin
  FSymbol.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSymbol, 'Context to Symbol'){$ENDIF};
  inherited Destroy;
end;

function TDbgDwarfInfoAddressContext.FindSymbol(const AName: String): TFpDbgSymbol;
var
  SubRoutine: TDbgDwarfProcSymbol; // TDbgSymbol;
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
  if (FSymbol = nil) or not(FSymbol is TDbgDwarfProcSymbol) or (AName = '') then
    exit;

  SubRoutine := TDbgDwarfProcSymbol(FSymbol);
  NameUpper := UTF8UpperCase(AName);
  NameLower := UTF8LowerCase(AName);
  PNameUpper := @NameUpper[1];
  PNameLower := @NameLower[1];

  try
    CU := SubRoutine.CompilationUnit;
    InfoEntry := SubRoutine.InformationEntry.Clone;

    // special: search "self" // depends on dwarf version
    // Todo nested procs
    if NameLower = 'self' then begin
      Result := SubRoutine.GetSelfParameter(FAddress);
      if Result <> nil then begin
        Result.AddReference;
        exit;
      end;
    end;

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
          Result := TDbgDwarfIdentifier.CreateSubClass(AName, InfoEntry);
          exit;
        end;
      end;

      tg := InfoEntry.AbbrevTag;
      if (tg = DW_TAG_class_type) or (tg = DW_TAG_structure_type) then begin
        Result := FindSymbolInStructure(AName,PNameUpper, PNameLower, InfoEntry);
        // TODO: check error
        if Result <> nil then
          exit;
        //InfoEntry.ScopeIndex := StartScopeIdx;
      end

      else
      if (StartScopeIdx = SubRoutine.InformationEntry.ScopeIndex) then begin // searching in subroutine
        Result := FindLocalSymbol(AName,PNameUpper, PNameLower, InfoEntry);
        // TODO: check error
        if Result <> nil then
          exit;
        //InfoEntry.ScopeIndex := StartScopeIdx;
      end
          // TODO: nested subroutine

      else
      if InfoEntry.GoNamedChildEx(PNameUpper, PNameLower) then begin
        if InfoEntry.IsAddressInStartScope(FAddress) and not InfoEntry.IsArtificial then begin
          Result := TDbgDwarfIdentifier.CreateSubClass(AName, InfoEntry);
          exit;
        end;
      end;

      // Search parent(s)
      InfoEntry.ScopeIndex := StartScopeIdx;
      InfoEntry.GoParent;
    end;

    Result := FindExportedSymbolInUnits(AName, PNameUpper, PNameLower, CU);

  finally
    if (Result = nil) or (InfoEntry = nil)
    then DebugLn(FPDBG_DWARF_SEARCH, ['TDbgDwarf.FindIdentifier NOT found  Name=', AName])
    else DebugLn(FPDBG_DWARF_SEARCH, ['TDbgDwarf.FindIdentifier(',AName,') found Scope=', TDbgDwarfIdentifier(Result).InformationEntry.ScopeDebugText, '  Result=', DbgSName(Result), ' ', Result.Name, ' in ', TDbgDwarfIdentifier(Result).CompilationUnit.FileName]);
    ReleaseRefAndNil(InfoEntry);
  end;
end;

{ TFpDbgDwarfValue }

function TFpDbgDwarfValue.MemManager: TFpDbgMemManager;
begin
  assert((FOwner <> nil) and (FOwner.CompilationUnit <> nil) and (FOwner.CompilationUnit.Owner <> nil), 'TDbgDwarfSymbolValue.MemManager');
  Result := FOwner.CompilationUnit.Owner.MemManager;
end;

function TFpDbgDwarfValue.GetDataAddressCache(AIndex: Integer): TFpDbgMemLocation;
begin
  if AIndex < Length(FDataAddressCache) then
    Result := FDataAddressCache[AIndex]
  else
    Result := UnInitializedLoc;
end;

function TFpDbgDwarfValue.AddressSize: Byte;
begin
  assert((FOwner <> nil) and (FOwner.CompilationUnit <> nil), 'TDbgDwarfSymbolValue.AddressSize');
  Result := FOwner.CompilationUnit.AddressSize;
end;

procedure TFpDbgDwarfValue.SetDataAddressCache(AIndex: Integer; AValue: TFpDbgMemLocation);
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

procedure TFpDbgDwarfValue.SetStructureValue(AValue: TFpDbgDwarfValue);
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

function TFpDbgDwarfValue.GetLastError: TFpError;
begin
  Result := FLastError;
end;

function TFpDbgDwarfValue.DataAddr: TFpDbgMemLocation;
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

function TFpDbgDwarfValue.OrdOrDataAddr: TFpDbgMemLocation;
begin
  if HasTypeCastInfo and (svfOrdinal in FTypeCastSourceValue.FieldFlags) then
    Result := ConstLoc(FTypeCastSourceValue.AsCardinal)
  else
    Result := DataAddr;
end;

function TFpDbgDwarfValue.GetDwarfDataAddress(out AnAddress: TFpDbgMemLocation;
  ATargetType: TDbgDwarfTypeIdentifier): Boolean;
var
  fields: TFpDbgValueFieldFlags;
begin
  if FValueSymbol <> nil then begin
    Assert(FValueSymbol is TDbgDwarfValueIdentifier, 'TDbgDwarfSymbolValue.GetDwarfDataAddress FValueSymbol');
    Assert(TypeInfo is TDbgDwarfTypeIdentifier, 'TDbgDwarfSymbolValue.GetDwarfDataAddress TypeInfo');
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

function TFpDbgDwarfValue.GetStructureDwarfDataAddress(out AnAddress: TFpDbgMemLocation;
  ATargetType: TDbgDwarfTypeIdentifier): Boolean;
begin
  AnAddress := InvalidLoc;
  Result := StructureValue <> nil;
  if Result then
    Result := StructureValue.GetDwarfDataAddress(AnAddress, ATargetType);
end;

function TFpDbgDwarfValue.HasDwarfDataAddress: Boolean;
begin
  if FValueSymbol <> nil then begin
    Assert(FValueSymbol is TDbgDwarfValueIdentifier, 'TDbgDwarfSymbolValue.GetDwarfDataAddress FValueSymbol');
    Assert(TypeInfo is TDbgDwarfTypeIdentifier, 'TDbgDwarfSymbolValue.GetDwarfDataAddress TypeInfo');
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

procedure TFpDbgDwarfValue.Reset;
begin
  FDataAddressCache := nil;
  FLastError := NoError;
end;

function TFpDbgDwarfValue.GetFieldFlags: TFpDbgValueFieldFlags;
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

function TFpDbgDwarfValue.HasTypeCastInfo: Boolean;
begin
  Result := (FTypeCastTargetType <> nil) and (FTypeCastSourceValue <> nil);
end;

function TFpDbgDwarfValue.IsValidTypeCast: Boolean;
begin
  Result := False;
end;

procedure TFpDbgDwarfValue.DoReferenceAdded;
begin
  inherited DoReferenceAdded;
  DoPlainReferenceAdded;
end;

procedure TFpDbgDwarfValue.DoReferenceReleased;
begin
  inherited DoReferenceReleased;
  DoPlainReferenceReleased;
end;

procedure TFpDbgDwarfValue.CircleBackRefActiveChanged(NewActive: Boolean);
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

procedure TFpDbgDwarfValue.SetLastMember(ALastMember: TFpDbgDwarfValue);
begin
  if FLastMember <> nil then
    FLastMember.ReleaseCirclularReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FLastMember, 'TDbgDwarfSymbolValue'){$ENDIF};
  FLastMember := ALastMember;
  if FLastMember <> nil then begin
    FLastMember.SetStructureValue(Self);
    FLastMember.AddCirclularReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FLastMember, 'TDbgDwarfSymbolValue'){$ENDIF};
  end;
end;

function TFpDbgDwarfValue.GetKind: TDbgSymbolKind;
begin
  if FValueSymbol <> nil then
    Result := FValueSymbol.Kind
  else
  if HasTypeCastInfo then
    Result := FTypeCastTargetType.Kind
  else
    Result := inherited GetKind;
end;

function TFpDbgDwarfValue.GetAddress: TFpDbgMemLocation;
begin
  if FValueSymbol <> nil then
    FValueSymbol.GetValueAddress(Self, Result)
  else
  if HasTypeCastInfo then
    Result := FTypeCastSourceValue.Address
  else
    Result := inherited GetAddress;
end;

function TFpDbgDwarfValue.OrdOrAddress: TFpDbgMemLocation;
begin
  if HasTypeCastInfo and (svfOrdinal in FTypeCastSourceValue.FieldFlags) then
    Result := ConstLoc(FTypeCastSourceValue.AsCardinal)
  else
    Result := Address;
end;

function TFpDbgDwarfValue.GetMemberCount: Integer;
begin
  if FValueSymbol <> nil then
    Result := FValueSymbol.MemberCount
  else
    Result := inherited GetMemberCount;
end;

function TFpDbgDwarfValue.GetMemberByName(AIndex: String): TFpDbgValue;
var
  m: TFpDbgSymbol;
begin
  Result := nil;
  if FValueSymbol <> nil then begin
    m := FValueSymbol.MemberByName[AIndex];
    if m <> nil then
      Result := m.Value;
  end;
  SetLastMember(TFpDbgDwarfValue(Result));
end;

function TFpDbgDwarfValue.GetMember(AIndex: Int64): TFpDbgValue;
var
  m: TFpDbgSymbol;
begin
  Result := nil;
  if FValueSymbol <> nil then begin
    m := FValueSymbol.Member[AIndex];
    if m <> nil then
      Result := m.Value;
  end;
  SetLastMember(TFpDbgDwarfValue(Result));
end;

function TFpDbgDwarfValue.GetDbgSymbol: TFpDbgSymbol;
begin
  Result := FValueSymbol;
end;

function TFpDbgDwarfValue.GetTypeInfo: TFpDbgSymbol;
begin
  if HasTypeCastInfo then
    Result := FTypeCastTargetType
  else
    Result := inherited GetTypeInfo;
end;

function TFpDbgDwarfValue.GetContextTypeInfo: TFpDbgSymbol;
begin
  if (FValueSymbol <> nil) and (FValueSymbol.ParentTypeInfo <> nil) then
    Result := FValueSymbol.ParentTypeInfo
  else
    Result := nil; // internal error
end;

constructor TFpDbgDwarfValue.Create(AOwner: TDbgDwarfTypeIdentifier);
begin
  FOwner := AOwner;
  inherited Create;
end;

destructor TFpDbgDwarfValue.Destroy;
begin
  ReleaseRefAndNil(FTypeCastTargetType);
  ReleaseRefAndNil(FTypeCastSourceValue);
  SetLastMember(nil);
  inherited Destroy;
end;

procedure TFpDbgDwarfValue.SetValueSymbol(AValueSymbol: TDbgDwarfValueIdentifier);
begin
  if FValueSymbol = AValueSymbol then
    exit;

  if CircleBackRefsActive and (FValueSymbol <> nil) then
    FValueSymbol.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValueSymbol, 'TDbgDwarfSymbolValue'){$ENDIF};
  FValueSymbol := AValueSymbol;
  if CircleBackRefsActive and (FValueSymbol <> nil) then
    FValueSymbol.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValueSymbol, 'TDbgDwarfSymbolValue'){$ENDIF};
end;

function TFpDbgDwarfValue.SetTypeCastInfo(AStructure: TDbgDwarfTypeIdentifier;
  ASource: TFpDbgValue): Boolean;
begin
  Reset;

  if FTypeCastSourceValue <> ASource then begin
    if FTypeCastSourceValue <> nil then
      FTypeCastSourceValue.ReleaseReference;
    FTypeCastSourceValue := ASource;
    if FTypeCastSourceValue <> nil then
      FTypeCastSourceValue.AddReference;
  end;

  if FTypeCastTargetType <> AStructure then begin
    if FTypeCastTargetType <> nil then
      FTypeCastTargetType.ReleaseReference;
    FTypeCastTargetType := AStructure;
    if FTypeCastTargetType <> nil then
      FTypeCastTargetType.AddReference;
  end;

  Result := IsValidTypeCast;
end;

{ TFpDbgDwarfValueSized }

function TFpDbgDwarfValueSized.CanUseTypeCastAddress: Boolean;
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

function TFpDbgDwarfValueSized.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfSize];
end;

function TFpDbgDwarfValueSized.GetSize: Integer;
begin
  Result := FSize;
end;

constructor TFpDbgDwarfValueSized.Create(AOwner: TDbgDwarfTypeIdentifier; ASize: Integer);
begin
  inherited Create(AOwner);
  FSize := ASize;
end;

{ TFpDbgDwarfValueNumeric }

procedure TFpDbgDwarfValueNumeric.Reset;
begin
  inherited Reset;
  FEvaluated := [];
end;

function TFpDbgDwarfValueNumeric.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfOrdinal];
end;

function TFpDbgDwarfValueNumeric.IsValidTypeCast: Boolean;
begin
  Result := HasTypeCastInfo;
  If not Result then
    exit;
  if (svfOrdinal in FTypeCastSourceValue.FieldFlags) or CanUseTypeCastAddress then
    exit;
  Result := False;
end;

constructor TFpDbgDwarfValueNumeric.Create(AOwner: TDbgDwarfTypeIdentifier; ASize: Integer);
begin
  inherited Create(AOwner, ASize);
  FEvaluated := [];
end;

{ TFpDbgDwarfValueInteger }

function TFpDbgDwarfValueInteger.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfInteger];
end;

function TFpDbgDwarfValueInteger.GetAsCardinal: QWord;
begin
  Result := QWord(GetAsInteger);  // include sign extension
end;

function TFpDbgDwarfValueInteger.GetAsInteger: Int64;
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

function TDbgDwarfValueCardinal.GetAsCardinal: QWord;
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

function TDbgDwarfValueCardinal.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfCardinal];
end;

{ TFpDbgDwarfValueFloat }

function TFpDbgDwarfValueFloat.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfFloat] - [svfOrdinal];
end;

function TFpDbgDwarfValueFloat.GetAsFloat: Extended;
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

{ TFpDbgDwarfValueBoolean }

function TFpDbgDwarfValueBoolean.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfBoolean];
end;

function TFpDbgDwarfValueBoolean.GetAsBool: Boolean;
begin
  Result := QWord(GetAsCardinal) <> 0;
end;

{ TFpDbgDwarfValueChar }

function TFpDbgDwarfValueChar.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  case FSize of
    1: Result := Result + [svfString];
    2: Result := Result + [svfWideString];
  end;
end;

function TFpDbgDwarfValueChar.GetAsString: AnsiString;
begin
  // Can typecast, because of FSize = 1, GetAsCardinal only read one byte
  if FSize <> 1 then
    Result := inherited GetAsString
  else
    Result := SysToUTF8(char(byte(GetAsCardinal)));
end;

function TFpDbgDwarfValueChar.GetAsWideString: WideString;
begin
  if FSize > 2 then
    Result := inherited GetAsString
  else
    Result := WideChar(Word(GetAsCardinal));
end;

{ TFpDbgDwarfValuePointer }

function TFpDbgDwarfValuePointer.GetAsCardinal: QWord;
var
  a: TFpDbgMemLocation;
begin
  a := GetDataAddress;
  if IsTargetAddr(a) then
    Result := LocToAddr(a)
  else
    Result := 0;
end;

function TFpDbgDwarfValuePointer.GetFieldFlags: TFpDbgValueFieldFlags;
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

function TFpDbgDwarfValuePointer.GetDataAddress: TFpDbgMemLocation;
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

function TFpDbgDwarfValuePointer.GetAsString: AnsiString;
var
  t: TFpDbgSymbol;
  i: Integer;
begin
  t := TypeInfo;
  if (t <> nil) then t := t.TypeInfo;
  if  (MemManager <> nil) and (t <> nil) and (t.Kind = skChar) and IsReadableMem(DataAddress) then begin // pchar
    SetLength(Result, 2000);
    i := 2000;
    while (i > 0) and (not MemManager.ReadMemory(DataAddress, 2000, @Result[1])) do
      i := i div 2;
    SetLength(Result,i);
    i := pos(#0, Result);
    if i > 0 then
      SetLength(Result,i-1);
    exit;
  end;

  Result := inherited GetAsString;
end;

{ TFpDbgDwarfValueEnum }

procedure TFpDbgDwarfValueEnum.InitMemberIndex;
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

procedure TFpDbgDwarfValueEnum.Reset;
begin
  inherited Reset;
  FMemberValueDone := False;
end;

function TFpDbgDwarfValueEnum.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfOrdinal, svfMembers, svfIdentifier];
end;

function TFpDbgDwarfValueEnum.GetAsCardinal: QWord;
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

function TFpDbgDwarfValueEnum.GetAsString: AnsiString;
begin
  InitMemberIndex;
  if FMemberIndex >= 0 then
    Result := FOwner.Member[FMemberIndex].Name
  else
    Result := '';
end;

function TFpDbgDwarfValueEnum.GetMemberCount: Integer;
begin
  InitMemberIndex;
  if FMemberIndex < 0 then
    Result := 0
  else
    Result := 1;
end;

function TFpDbgDwarfValueEnum.GetMember(AIndex: Int64): TFpDbgValue;
begin
  InitMemberIndex;
  if (FMemberIndex >= 0) and (AIndex = 0) then
    Result := FOwner.Member[FMemberIndex].Value
  else
    Result := nil;
end;

{ TFpDbgDwarfValueEnumMember }

function TFpDbgDwarfValueEnumMember.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfOrdinal, svfIdentifier];
end;

function TFpDbgDwarfValueEnumMember.GetAsCardinal: QWord;
begin
  Result := FOwnerVal.OrdinalValue;
end;

function TFpDbgDwarfValueEnumMember.GetAsString: AnsiString;
begin
  Result := FOwnerVal.Name;
end;

function TFpDbgDwarfValueEnumMember.IsValidTypeCast: Boolean;
begin
  assert(False, 'TDbgDwarfEnumMemberSymbolValue.IsValidTypeCast can not be returned for typecast');
  Result := False;
end;

constructor TFpDbgDwarfValueEnumMember.Create(AOwner: TDbgDwarfValueIdentifier);
begin
  FOwnerVal := AOwner;
  inherited Create(nil);
end;

{ TFpDbgDwarfValueConstNumber }

procedure TFpDbgDwarfValueConstNumber.Update(AValue: QWord; ASigned: Boolean);
begin
  Signed := ASigned;
  Value := AValue;
end;

{ TFpDbgDwarfValueSet }

procedure TFpDbgDwarfValueSet.InitMap;
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

procedure TFpDbgDwarfValueSet.Reset;
begin
  inherited Reset;
  SetLength(FMem, 0);
end;

function TFpDbgDwarfValueSet.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfMembers];
  if FSize <= 8 then
    Result := Result + [svfOrdinal];
end;

function TFpDbgDwarfValueSet.GetMemberCount: Integer;
begin
  InitMap;
  Result := FMemberCount;
end;

function TFpDbgDwarfValueSet.GetMember(AIndex: Int64): TFpDbgValue;
var
  t: TFpDbgSymbol;
begin
  Result := nil;
  InitMap;
  t := TypeInfo;
  if t = nil then exit;
  t := t.TypeInfo;
  if t = nil then exit;
  assert(t is TDbgDwarfTypeIdentifier, 'TDbgDwarfSetSymbolValue.GetMember t');

  if t.Kind = skEnum then begin
    Result := t.Member[FMemberMap[AIndex]].Value;
  end
  else begin
    if (FNumValue = nil) or (FNumValue.RefCount > 1) then // refcount 1 by FTypedNumValue
      FNumValue := TFpDbgDwarfValueConstNumber.Create(FMemberMap[AIndex] + t.OrdLowBound, t.Kind = skInteger)
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
      TFpDbgDwarfValue(FTypedNumValue).SetTypeCastInfo(TDbgDwarfTypeIdentifier(t), FNumValue); // update
    FNumValue.ReleaseReference;
    Assert((FTypedNumValue <> nil) and (TFpDbgDwarfValue(FTypedNumValue).IsValidTypeCast), 'TDbgDwarfSetSymbolValue.GetMember FTypedNumValue');
    Assert((FNumValue <> nil) and (FNumValue.RefCount > 0), 'TDbgDwarfSetSymbolValue.GetMember FNumValue');
    Result := FTypedNumValue;
  end;
end;

function TFpDbgDwarfValueSet.GetAsCardinal: QWord;
begin
  Result := 0;
  if (FSize <= SizeOf(Result)) and (length(FMem) > 0) then
    move(FMem[0], Result, FSize);
end;

function TFpDbgDwarfValueSet.IsValidTypeCast: Boolean;
var
  f: TFpDbgValueFieldFlags;
begin
  Result := HasTypeCastInfo;
  If not Result then
    exit;

  assert(FTypeCastTargetType.Kind = skSet, 'TFpDbgDwarfValueSet.IsValidTypeCast: FTypeCastTargetType.Kind = skSet');

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

destructor TFpDbgDwarfValueSet.Destroy;
begin
  FTypedNumValue.ReleaseReference;
  inherited Destroy;
end;

{ TFpDbgDwarfValueStruct }

procedure TFpDbgDwarfValueStruct.Reset;
begin
  inherited Reset;
  FDataAddressDone := False;
end;

function TFpDbgDwarfValueStruct.GetFieldFlags: TFpDbgValueFieldFlags;
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

function TFpDbgDwarfValueStruct.GetAsCardinal: QWord;
begin
  Result := QWord(LocToAddrOrNil(DataAddress));
end;

function TFpDbgDwarfValueStruct.GetDataAddress: TFpDbgMemLocation;
var
  t: TFpDbgMemLocation;
begin
  if FValueSymbol <> nil then begin
    if not FDataAddressDone then begin
      FDataAddress := InvalidLoc;
      FValueSymbol.GetValueAddress(Self, t);
      assert(SizeOf(FDataAddress) >= AddressSize, 'TDbgDwarfStructSymbolValue.GetDataAddress');
      if (MemManager <> nil) then begin
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

function TFpDbgDwarfValueStruct.GetDataSize: Integer;
begin
  Assert((FValueSymbol = nil) or (FValueSymbol.TypeInfo is TDbgDwarfIdentifier));
  if (FValueSymbol <> nil) and (FValueSymbol.TypeInfo <> nil) then
    if FValueSymbol.TypeInfo.Kind = skClass then
      Result := TDbgDwarfIdentifier(FValueSymbol.TypeInfo).DataSize
    else
      Result := FValueSymbol.TypeInfo.Size
  else
    Result := -1;
end;

function TFpDbgDwarfValueStruct.GetSize: Integer;
begin
  if (Kind <> skClass) and (FValueSymbol <> nil) and (FValueSymbol.TypeInfo <> nil) then
    Result := FValueSymbol.TypeInfo.Size
  else
    Result := -1;
end;

{ TFpDbgDwarfValueStructTypeCast }

procedure TFpDbgDwarfValueStructTypeCast.Reset;
begin
  inherited Reset;
  FDataAddressDone := False;
  ClearMembers;
end;

procedure TFpDbgDwarfValueStructTypeCast.ClearMembers;
var
  i: Integer;
begin
  if FMembers <> nil then
    for i := 0 to FMembers.Count - 1 do
      TDbgDwarfValueIdentifier(FMembers[i]).StructureValueInfo := nil;
end;

function TFpDbgDwarfValueStructTypeCast.GetFieldFlags: TFpDbgValueFieldFlags;
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

function TFpDbgDwarfValueStructTypeCast.GetKind: TDbgSymbolKind;
begin
  if HasTypeCastInfo then
    Result := FTypeCastTargetType.Kind
  else
    Result := inherited GetKind;
end;

function TFpDbgDwarfValueStructTypeCast.GetAsCardinal: QWord;
begin
  Result := QWord(LocToAddrOrNil(DataAddress));
end;

function TFpDbgDwarfValueStructTypeCast.GetSize: Integer;
begin
  if (Kind <> skClass) and (FTypeCastTargetType <> nil) then
    Result := FTypeCastTargetType.Size
  else
    Result := -1;
end;

function TFpDbgDwarfValueStructTypeCast.GetDataSize: Integer;
begin
  Assert((FTypeCastTargetType = nil) or (FTypeCastTargetType is TDbgDwarfIdentifier));
  if FTypeCastTargetType <> nil then
    if FTypeCastTargetType.Kind = skClass then
      Result := TDbgDwarfIdentifier(FTypeCastTargetType).DataSize
    else
      Result := FTypeCastTargetType.Size
  else
    Result := -1;
end;

function TFpDbgDwarfValueStructTypeCast.GetDataAddress: TFpDbgMemLocation;
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

function TFpDbgDwarfValueStructTypeCast.IsValidTypeCast: Boolean;
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

destructor TFpDbgDwarfValueStructTypeCast.Destroy;
begin
  ClearMembers;
  FreeAndNil(FMembers);
  inherited Destroy;
end;

function TFpDbgDwarfValueStructTypeCast.GetMemberByName(AIndex: String): TFpDbgValue;
var
  tmp: TFpDbgSymbol;
begin
  Result := nil;
  if not HasTypeCastInfo then
    exit;

  tmp := FTypeCastTargetType.MemberByName[AIndex];
  if (tmp <> nil) then begin
    assert((tmp is TDbgDwarfValueIdentifier), 'TDbgDwarfStructTypeCastSymbolValue.GetMemberByName'+DbgSName(tmp));
    if FMembers = nil then
      FMembers := TFpDbgCircularRefCntObjList.Create;
    FMembers.Add(tmp);

    Result := tmp.Value;
  end;
  SetLastMember(TFpDbgDwarfValue(Result));
end;

function TFpDbgDwarfValueStructTypeCast.GetMember(AIndex: Int64): TFpDbgValue;
var
  tmp: TFpDbgSymbol;
begin
  Result := nil;
  if not HasTypeCastInfo then
    exit;

  // TODO: Why store them all in list? They are hold by the type
  tmp := FTypeCastTargetType.Member[AIndex];
  if (tmp <> nil) then begin
    assert((tmp is TDbgDwarfValueIdentifier), 'TDbgDwarfStructTypeCastSymbolValue.GetMemberByName'+DbgSName(tmp));
    if FMembers = nil then
      FMembers := TFpDbgCircularRefCntObjList.Create;
    FMembers.Add(tmp);

    Result := tmp.Value;
  end;
  SetLastMember(TFpDbgDwarfValue(Result));
end;

function TFpDbgDwarfValueStructTypeCast.GetMemberCount: Integer;
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

{ TFpDbgDwarfValueConstAddress }

procedure TFpDbgDwarfValueConstAddress.Update(AnAddress: TFpDbgMemLocation);
begin
  Address := AnAddress;
end;

{ TFpDbgDwarfValueArray }

function TFpDbgDwarfValueArray.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfMembers];
  if (TypeInfo <> nil) and (sfDynArray in TypeInfo.Flags) then
    Result := Result + [svfOrdinal, svfDataAddress];
end;

function TFpDbgDwarfValueArray.GetKind: TDbgSymbolKind;
begin
  Result := skArray;
end;

function TFpDbgDwarfValueArray.GetAsCardinal: QWord;
begin
  // TODO cache
  if not MemManager.ReadUnsignedInt(OrdOrAddress, AddressSize, Result) then begin
    FLastError := MemManager.LastError;
    Result := 0;
  end;
end;

function TFpDbgDwarfValueArray.GetDataAddress: TFpDbgMemLocation;
begin
  Result := OrdOrDataAddr;
end;

function TFpDbgDwarfValueArray.GetMember(AIndex: Int64): TFpDbgValue;
begin
  Result := GetMemberEx([AIndex]);
end;

function TFpDbgDwarfValueArray.GetMemberEx(AIndex: array of Int64): TFpDbgValue;
var
  Addr: TFpDbgMemLocation;
  i: Integer;
begin
  Result := nil;
  assert((FOwner is TDbgDwarfIdentifierArray) and (FOwner.Kind = skArray));
  Addr := TDbgDwarfIdentifierArray(FOwner).GetMemberAddress(Self, AIndex);
  if not IsReadableLoc(Addr) then exit;

  // FAddrObj.RefCount: hold by self
  i := 1;
  // FAddrObj.RefCount: hold by FLastMember (ignore only, if FLastMember is not hold by others)
  if (FLastMember <> nil) and (FLastMember.RefCount = 1) then
    i := 2;
  if (FAddrObj = nil) or (FAddrObj.RefCount > i) then begin
    FAddrObj.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FAddrObj, 'TDbgDwarfArraySymbolValue'){$ENDIF};
    FAddrObj := TFpDbgDwarfValueConstAddress.Create(Addr);
    {$IFDEF WITH_REFCOUNT_DEBUG}FAddrObj.DbgRenameReference(@FAddrObj, 'TDbgDwarfArraySymbolValue');{$ENDIF}
  end
  else begin
    FAddrObj.Update(Addr);
  end;

  if (FLastMember = nil) or (FLastMember.RefCount > 1) then begin
    SetLastMember(TFpDbgDwarfValue(FOwner.TypeInfo.TypeCastValue(FAddrObj)));
    FLastMember.ReleaseReference;
  end
  else begin
    TFpDbgDwarfValue(FLastMember).SetTypeCastInfo(TDbgDwarfTypeIdentifier(FOwner.TypeInfo), FAddrObj);
  end;

  Result := FLastMember;
end;

function TFpDbgDwarfValueArray.GetMemberCount: Integer;
var
  t, t2: TFpDbgSymbol;
  Addr: TFpDbgMemLocation;
  i: Int64;
begin
  Result := 0;
  t := TypeInfo;
  if t.MemberCount < 1 then // IndexTypeCount;
    exit;
  t2 := t.Member[0]; // IndexType[0];
  if not t2.HasBounds then begin
    if (sfDynArray in t.Flags) and (AsCardinal <> 0) and
       GetDwarfDataAddress(Addr, TDbgDwarfTypeIdentifier(FOwner))
    then begin
      if not (IsReadableMem(Addr) and (LocToAddr(Addr) > 4)) then
        exit;
      Addr.Address := Addr.Address - AddressSize;
      if MemManager.ReadSignedInt(Addr, 4, i) then begin
        Result := Integer(i)+1;
        exit;
      end
      else
        FLastError := MemManager.LastError;
    end;
    exit;
  end;
  Result := t2.OrdHighBound - t2.OrdLowBound + 1;
end;

function TFpDbgDwarfValueArray.GetMemberCountEx(AIndex: array of Int64): Integer;
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

function TFpDbgDwarfValueArray.GetIndexType(AIndex: Integer): TFpDbgSymbol;
begin
  Result := TypeInfo.Member[AIndex];
end;

function TFpDbgDwarfValueArray.GetIndexTypeCount: Integer;
begin
  Result := TypeInfo.MemberCount;
end;

function TFpDbgDwarfValueArray.IsValidTypeCast: Boolean;
var
  f: TFpDbgValueFieldFlags;
begin
  Result := HasTypeCastInfo;
  If not Result then
    exit;

  assert(FTypeCastTargetType.Kind = skArray, 'TFpDbgDwarfValueArray.IsValidTypeCast: FTypeCastTargetType.Kind = skArray');
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

destructor TFpDbgDwarfValueArray.Destroy;
begin
  FAddrObj.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FAddrObj, 'TDbgDwarfArraySymbolValue'){$ENDIF};
  inherited Destroy;
end;

{ TDbgDwarfIdentifier }

function TDbgDwarfIdentifier.GetNestedTypeInfo: TDbgDwarfTypeIdentifier;
begin
// TODO DW_AT_start_scope;
  Result := FNestedTypeInfo;
  if (Result <> nil) or (didtTypeRead in FDwarfReadFlags) then
    exit;

  include(FDwarfReadFlags, didtTypeRead);
  FNestedTypeInfo := DoGetNestedTypeInfo;
  Result := FNestedTypeInfo;
end;

procedure TDbgDwarfIdentifier.SetParentTypeInfo(AValue: TDbgDwarfIdentifier);
begin
  if FParentTypeInfo = AValue then exit;

  if (FParentTypeInfo <> nil) and CircleBackRefsActive then
    FParentTypeInfo.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FParentTypeInfo, 'FParentTypeInfo'){$ENDIF};

  FParentTypeInfo := AValue;

  if (FParentTypeInfo <> nil) and CircleBackRefsActive then
    FParentTypeInfo.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FParentTypeInfo, 'FParentTypeInfo'){$ENDIF};
end;

procedure TDbgDwarfIdentifier.DoReferenceAdded;
begin
  inherited DoReferenceAdded;
  DoPlainReferenceAdded;
end;

procedure TDbgDwarfIdentifier.DoReferenceReleased;
begin
  inherited DoReferenceReleased;
  DoPlainReferenceReleased;
end;

procedure TDbgDwarfIdentifier.CircleBackRefActiveChanged(ANewActive: Boolean);
begin
  if (FParentTypeInfo = nil) then
    exit;
  if ANewActive then
    FParentTypeInfo.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FParentTypeInfo, 'FParentTypeInfo'){$ENDIF}
  else
    FParentTypeInfo.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FParentTypeInfo, 'FParentTypeInfo'){$ENDIF};
end;

function TDbgDwarfIdentifier.DoGetNestedTypeInfo: TDbgDwarfTypeIdentifier;
var
  FwdInfoPtr: Pointer;
  FwdCompUint: TDwarfCompilationUnit;
  InfoEntry: TDwarfInformationEntry;
begin // Do not access anything that may need forwardSymbol
  if InformationEntry.ReadReference(DW_AT_type, FwdInfoPtr, FwdCompUint) then begin
    InfoEntry := TDwarfInformationEntry.Create(FwdCompUint, FwdInfoPtr);
    Result := TDbgDwarfTypeIdentifier.CreateTypeSubClass('', InfoEntry);
    ReleaseRefAndNil(InfoEntry);
  end
  else
    Result := nil;
end;

function TDbgDwarfIdentifier.ReadMemberVisibility(out
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

function TDbgDwarfIdentifier.IsArtificial: Boolean;
begin
  if not(didtArtificialRead in FDwarfReadFlags) then begin
    if InformationEntry.IsArtificial then
      Include(FDwarfReadFlags, didtIsArtifical);
    Include(FDwarfReadFlags, didtArtificialRead);
  end;
  Result := didtIsArtifical in FDwarfReadFlags;
end;

procedure TDbgDwarfIdentifier.NameNeeded;
var
  AName: String;
begin
  if InformationEntry.ReadName(AName) then
    SetName(AName)
  else
    inherited NameNeeded;
end;

procedure TDbgDwarfIdentifier.TypeInfoNeeded;
begin
  SetTypeInfo(NestedTypeInfo);
end;

function TDbgDwarfIdentifier.DataSize: Integer;
var
  t: TDbgDwarfTypeIdentifier;
begin
  t := NestedTypeInfo;
  if t <> nil then
    Result := t.DataSize
  else
    Result := 0;
end;

function TDbgDwarfIdentifier.InitLocationParser(const ALocationParser: TDwarfLocationExpression;
  AValueObj: TFpDbgDwarfValue; AnObjectDataAddress: TFpDbgMemLocation): Boolean;
begin
  Result := True;
end;

function TDbgDwarfIdentifier.LocationFromTag(ATag: Cardinal; AValueObj: TFpDbgDwarfValue;
  out AnAddress: TFpDbgMemLocation; AnObjectDataAddress: TFpDbgMemLocation;
  AnInformationEntry: TDwarfInformationEntry): Boolean;
var
  Val: TByteDynArray;
  LocationParser: TDwarfLocationExpression;
begin
  //debugln(['TDbgDwarfIdentifier.LocationFromTag', ClassName, '  ',Name, '  ', DwarfAttributeToString(ATag)]);

  Result := False;
  AnAddress := InvalidLoc;
  if AnInformationEntry = nil then
    AnInformationEntry := InformationEntry;

  //TODO: avoid copying data
  // DW_AT_data_member_location in members [ block or const]
  // DW_AT_location [block or reference] todo: const
  if not AnInformationEntry.ReadValue(ATag, Val) then begin
    DebugLn('LocationFromTag: failed to read DW_AT_location');
    exit;
  end;
  if Length(Val) = 0 then begin
    DebugLn('LocationFromTag: Warning DW_AT_location empty');
    //exit;
  end;

  LocationParser := TDwarfLocationExpression.Create(@Val[0], Length(Val), CompilationUnit);
  InitLocationParser(LocationParser, AValueObj, AnObjectDataAddress);
  LocationParser.Evaluate;

  if IsError(LocationParser.LastError) then
    SetLastError(LocationParser.LastError);

  if LocationParser.ResultKind in [lseValue] then begin
    AnAddress := TargetLoc(LocationParser.ResultData);
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

function TDbgDwarfIdentifier.GetDataAddress(AValueObj: TFpDbgDwarfValue;
  var AnAddress: TFpDbgMemLocation; ATargetType: TDbgDwarfTypeIdentifier;
  ATargetCacheIndex: Integer): Boolean;
var
  ti: TDbgDwarfTypeIdentifier;
begin
  if ATargetType = Self then begin
    Result := True;
  end
  else begin
    ti := NestedTypeInfo;
    if ti <> nil then
      Result := ti.GetDataAddress(AValueObj, AnAddress, ATargetType, ATargetCacheIndex+1)
    else
      Result := ATargetType = nil; // end of type chain
  end;
end;

function TDbgDwarfIdentifier.HasAddress: Boolean;
begin
  Result := False;
end;

procedure TDbgDwarfIdentifier.Init;
begin
  //
end;

class function TDbgDwarfIdentifier.CreateSubClass(AName: String;
  AnInformationEntry: TDwarfInformationEntry): TDbgDwarfIdentifier;
var
  c: TDbgDwarfSymbolBaseClass;
begin
  c := AnInformationEntry.CompUnit.DwarfSymbolClassMap.GetDwarfSymbolClass(AnInformationEntry.AbbrevTag);
  Result := TDbgDwarfIdentifier(c.Create(AName, AnInformationEntry));
end;

destructor TDbgDwarfIdentifier.Destroy;
begin
  inherited Destroy;
  ReleaseRefAndNil(FNestedTypeInfo);
  Assert(not CircleBackRefsActive, 'CircleBackRefsActive can not be is destructor');
  // FParentTypeInfo := nil
end;

function TDbgDwarfIdentifier.StartScope: TDbgPtr;
begin
  if not InformationEntry.ReadStartScope(Result) then
    Result := 0;
end;

{ TDbgDwarfValueIdentifier }

procedure TDbgDwarfValueIdentifier.SetStructureValueInfo(AValue: TDbgDwarfIdentifier);
begin
  if FStructureValueInfo = AValue then Exit;

  if (FStructureValueInfo <> nil) and CircleBackRefsActive then
    FStructureValueInfo.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FStructureValueInfo, 'FStructureValueInfo'){$ENDIF};

  FStructureValueInfo := AValue;

  if (FStructureValueInfo <> nil) and CircleBackRefsActive then
    FStructureValueInfo.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FStructureValueInfo, 'FStructureValueInfo'){$ENDIF};
end;

procedure TDbgDwarfValueIdentifier.CircleBackRefActiveChanged(ANewActive: Boolean);
begin
  inherited;
  if (FStructureValueInfo = nil) then
    exit;
  if ANewActive then
    FStructureValueInfo.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FStructureValueInfo, 'FStructureValueInfo'){$ENDIF}
  else
    FStructureValueInfo.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FStructureValueInfo, 'FStructureValueInfo'){$ENDIF};
end;

procedure TDbgDwarfValueIdentifier.SetParentTypeInfo(AValue: TDbgDwarfIdentifier);
begin
  if AValue <> ParentTypeInfo then
    StructureValueInfo := nil;
  inherited SetParentTypeInfo(AValue);
end;

function TDbgDwarfValueIdentifier.GetValueAddress(AValueObj: TFpDbgDwarfValue; out
  AnAddress: TFpDbgMemLocation): Boolean;
begin
  Result := False;
end;

function TDbgDwarfValueIdentifier.GetValueDataAddress(AValueObj: TFpDbgDwarfValue; out
  AnAddress: TFpDbgMemLocation; ATargetType: TDbgDwarfTypeIdentifier): Boolean;
begin
  Result := TypeInfo <> nil;
  if not Result then
    exit;

  Assert((TypeInfo is TDbgDwarfIdentifier) and (TypeInfo.SymbolType = stType), 'TDbgDwarfValueIdentifier.GetDataAddress');
  Result := GetValueAddress(AValueObj, AnAddress);
  Result := Result and IsReadableLoc(AnAddress);
  if Result then begin
    Result := TDbgDwarfTypeIdentifier(TypeInfo).GetDataAddress(AValueObj, AnAddress, ATargetType, 1);
    if not Result then SetLastError(TypeInfo.LastError);
  end;
end;

procedure TDbgDwarfValueIdentifier.KindNeeded;
var
  t: TFpDbgSymbol;
begin
  t := TypeInfo;
  if t = nil then
    inherited KindNeeded
  else
    SetKind(t.Kind);
end;

procedure TDbgDwarfValueIdentifier.MemberVisibilityNeeded;
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

function TDbgDwarfValueIdentifier.GetMember(AIndex: Int64): TFpDbgSymbol;
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
  assert((Result = nil) or (Result is TDbgDwarfValueIdentifier), 'TDbgDwarfValueIdentifier.GetMember is Value');

  if (k in [skClass, skObject, skRecord {, skArray}]) and
     (Result <> nil) and (Result is TDbgDwarfValueIdentifier)
  then begin
    if FMembers = nil then
      FMembers := TFpDbgCircularRefCntObjList.Create;
    FMembers.Add(Result);
    TDbgDwarfValueIdentifier(Result).StructureValueInfo := Self;
  end;
end;

function TDbgDwarfValueIdentifier.GetMemberByName(AIndex: String): TFpDbgSymbol;
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
  assert((Result = nil) or (Result is TDbgDwarfValueIdentifier), 'TDbgDwarfValueIdentifier.GetMember is Value');

  if (k in [skClass, skObject, skRecord {, skArray}]) and
     (Result <> nil) and (Result is TDbgDwarfValueIdentifier)
  then begin
    if FMembers = nil then
      FMembers := TFpDbgCircularRefCntObjList.Create;
    FMembers.Add(Result);
    TDbgDwarfValueIdentifier(Result).StructureValueInfo := Self;
  end;
end;

function TDbgDwarfValueIdentifier.GetMemberCount: Integer;
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

procedure TDbgDwarfValueIdentifier.Init;
begin
  inherited Init;
  SetSymbolType(stValue);
end;

destructor TDbgDwarfValueIdentifier.Destroy;
var
  i: Integer;
begin
  Assert(not CircleBackRefsActive, 'CircleBackRefsActive can not be is ddestructor');

  if FMembers <> nil then
    for i := 0 to FMembers.Count - 1 do
      TDbgDwarfValueIdentifier(FMembers[i]).StructureValueInfo := nil;
  FreeAndNil(FMembers);
  if FValueObject <> nil then begin
    FValueObject.SetValueSymbol(nil);
    FValueObject.ReleaseCirclularReference;
    FValueObject := nil;
  end;
  ParentTypeInfo := nil;
  inherited Destroy;
end;

class function TDbgDwarfValueIdentifier.CreateValueSubClass(AName: String;
  AnInformationEntry: TDwarfInformationEntry): TDbgDwarfValueIdentifier;
var
  c: TDbgDwarfSymbolBaseClass;
begin
  c := AnInformationEntry.CompUnit.DwarfSymbolClassMap.GetDwarfSymbolClass(AnInformationEntry.AbbrevTag);

  if c.InheritsFrom(TDbgDwarfValueIdentifier) then
    Result := TDbgDwarfValueIdentifierClass(c).Create(AName, AnInformationEntry)
  else
    Result := nil;
end;

{ TDbgDwarfValueLocationIdentifier }

function TDbgDwarfValueLocationIdentifier.InitLocationParser(const ALocationParser: TDwarfLocationExpression;
  AValueObj: TFpDbgDwarfValue; AnObjectDataAddress: TFpDbgMemLocation): Boolean;
begin
  Result := inherited InitLocationParser(ALocationParser, AValueObj, AnObjectDataAddress);
  ALocationParser.OnFrameBaseNeeded := @FrameBaseNeeded;
end;

procedure TDbgDwarfValueLocationIdentifier.FrameBaseNeeded(ASender: TObject);
var
  p: TDbgDwarfIdentifier;
  fb: TDBGPtr;
begin
  debugln(FPDBG_DWARF_SEARCH, ['TDbgDwarfIdentifierVariable.FrameBaseNeeded ']);
  p := ParentTypeInfo;
  // TODO: what if parent is declaration?
  if (p <> nil) and (p is TDbgDwarfProcSymbol) then begin
    fb := TDbgDwarfProcSymbol(p).GetFrameBase;
    (ASender as TDwarfLocationExpression).FrameBase := fb;
    if fb = 0 then begin
      debugln(FPDBG_DWARF_ERRORS, ['DWARF ERROR in TDbgDwarfValueLocationIdentifier.FrameBaseNeeded result is 0']);
    end;
    exit;
  end;

{$warning TODO}
  //else
  //if OwnerTypeInfo <> nil then
  //  OwnerTypeInfo.fr;
  // TODO: check owner
  debugln(FPDBG_DWARF_ERRORS, ['DWARF ERROR in TDbgDwarfValueLocationIdentifier.FrameBaseNeeded no parent type info']);
  (ASender as TDwarfLocationExpression).FrameBase := 0;
end;

function TDbgDwarfValueLocationIdentifier.GetValueObject: TFpDbgValue;
var
  ti: TFpDbgSymbol;
begin
  Result := FValueObject;
  if Result <> nil then exit;

  ti := TypeInfo;
  if (ti = nil) or not (ti.SymbolType = stType) then exit;

  FValueObject := TDbgDwarfTypeIdentifier(ti).GetTypedValueObject(False);
  if FValueObject <> nil then begin
    FValueObject.MakePlainRefToCirclular;
    FValueObject.SetValueSymbol(self);

    // Used as reference to "self"
    if StructureValueInfo <> nil then
      FValueObject.SetStructureValue(TFpDbgDwarfValue(StructureValueInfo.Value)); // TODO: on request only
  end;

  Result := FValueObject;
end;

{ TDbgDwarfTypeIdentifier }

procedure TDbgDwarfTypeIdentifier.Init;
begin
  inherited Init;
  SetSymbolType(stType);
end;

procedure TDbgDwarfTypeIdentifier.MemberVisibilityNeeded;
var
  Val: TDbgSymbolMemberVisibility;
begin
  if ReadMemberVisibility(Val) then
    SetMemberVisibility(Val)
  else
    inherited MemberVisibilityNeeded;
end;

procedure TDbgDwarfTypeIdentifier.SizeNeeded;
var
  ByteSize: Integer;
begin
  if InformationEntry.ReadValue(DW_AT_byte_size, ByteSize) then
    SetSize(ByteSize)
  else
    inherited SizeNeeded;
end;

function TDbgDwarfTypeIdentifier.GetTypedValueObject(ATypeCast: Boolean): TFpDbgDwarfValue;
begin
  Result := nil;
end;

function TDbgDwarfTypeIdentifier.GetValueBounds(AValueObj: TFpDbgDwarfValue; out ALowBound,
  AHighBound: Int64): Boolean;
begin
  Result := HasBounds;
  ALowBound := OrdLowBound;
  AHighBound := OrdLowBound;
end;

class function TDbgDwarfTypeIdentifier.CreateTypeSubClass(AName: String;
  AnInformationEntry: TDwarfInformationEntry): TDbgDwarfTypeIdentifier;
var
  c: TDbgDwarfSymbolBaseClass;
begin
  c := AnInformationEntry.CompUnit.DwarfSymbolClassMap.GetDwarfSymbolClass(AnInformationEntry.AbbrevTag);

  if c.InheritsFrom(TDbgDwarfTypeIdentifier) then
    Result := TDbgDwarfTypeIdentifierClass(c).Create(AName, AnInformationEntry)
  else
    Result := nil;
end;

function TDbgDwarfTypeIdentifier.TypeCastValue(AValue: TFpDbgValue): TFpDbgValue;
begin
  Result := GetTypedValueObject(True);
  If Result = nil then
    exit;
  assert(Result is TFpDbgDwarfValue);
  if not TFpDbgDwarfValue(Result).SetTypeCastInfo(self, AValue) then
    ReleaseRefAndNil(Result);
end;

{ TDbgDwarfBaseTypeIdentifier }

procedure TDbgDwarfBaseIdentifierBase.KindNeeded;
var
  Encoding, ByteSize: Integer;
begin
  if not InformationEntry.ReadValue(DW_AT_encoding, Encoding) then begin
    DebugLn(FPDBG_DWARF_WARNINGS, ['TDbgDwarfBaseIdentifierBase.KindNeeded: Failed reading encoding for ', DwarfTagToString(InformationEntry.AbbrevTag)]);
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
    else
      begin
        DebugLn(FPDBG_DWARF_WARNINGS, ['TDbgDwarfBaseIdentifierBase.KindNeeded: Unknown encoding ', DwarfBaseTypeEncodingToString(Encoding), ' for ', DwarfTagToString(InformationEntry.AbbrevTag)]);
        inherited KindNeeded;
      end;
  end;
end;

procedure TDbgDwarfBaseIdentifierBase.TypeInfoNeeded;
begin
  SetTypeInfo(nil);
end;

function TDbgDwarfBaseIdentifierBase.GetTypedValueObject(ATypeCast: Boolean): TFpDbgDwarfValue;
begin
  case Kind of
    skPointer:  Result := TFpDbgDwarfValuePointer.Create(Self, Size);
    skInteger:  Result := TFpDbgDwarfValueInteger.Create(Self, Size);
    skCardinal: Result := TDbgDwarfValueCardinal.Create(Self, Size);
    skBoolean:  Result := TFpDbgDwarfValueBoolean.Create(Self, Size);
    skChar:     Result := TFpDbgDwarfValueChar.Create(Self, Size);
    skFloat:    Result := TFpDbgDwarfValueFloat.Create(Self, Size);
  end;
end;

function TDbgDwarfBaseIdentifierBase.GetHasBounds: Boolean;
begin
  Result := (kind = skInteger) or (kind = skCardinal);
end;

function TDbgDwarfBaseIdentifierBase.GetOrdHighBound: Int64;
begin
  case Kind of
    skInteger:  Result := int64( high(int64) shr (64 - Min(Size, 8) * 8));
    skCardinal: Result := int64( high(qword) shr (64 - Min(Size, 8) * 8));
    else
      Result := inherited GetOrdHighBound;
  end;
end;

function TDbgDwarfBaseIdentifierBase.GetOrdLowBound: Int64;
begin
  case Kind of
    skInteger:  Result := -(int64( high(int64) shr (64 - Min(Size, 8) * 8)))-1;
    skCardinal: Result := 0;
    else
      Result := inherited GetOrdHighBound;
  end;
end;

{ TDbgDwarfTypeIdentifierModifier }

procedure TDbgDwarfTypeIdentifierModifier.TypeInfoNeeded;
var
  p: TDbgDwarfTypeIdentifier;
begin
  p := NestedTypeInfo;
  if p <> nil then
    SetTypeInfo(p.TypeInfo)
  else
    SetTypeInfo(nil);
end;

procedure TDbgDwarfTypeIdentifierModifier.ForwardToSymbolNeeded;
begin
  SetForwardToSymbol(NestedTypeInfo)
end;

function TDbgDwarfTypeIdentifierModifier.GetTypedValueObject(ATypeCast: Boolean): TFpDbgDwarfValue;
var
  ti: TDbgDwarfTypeIdentifier;
begin
  ti := NestedTypeInfo;
  if ti <> nil then
    Result := ti.GetTypedValueObject(ATypeCast)
  else
    Result := inherited;
end;

{ TDbgDwarfTypeIdentifierRef }

function TDbgDwarfTypeIdentifierRef.GetFlags: TDbgSymbolFlags;
begin
  Result := (inherited GetFlags) + [sfInternalRef];
end;

function TDbgDwarfTypeIdentifierRef.GetDataAddress(AValueObj: TFpDbgDwarfValue;
  var AnAddress: TFpDbgMemLocation; ATargetType: TDbgDwarfTypeIdentifier;
  ATargetCacheIndex: Integer): Boolean;
var
  t: TFpDbgMemLocation;
begin
  if ATargetType = Self then begin
    Result := True;
    exit;
  end;

  t := AValueObj.DataAddressCache[ATargetCacheIndex];
  if IsInitializedLoc(t) then begin
    AnAddress := t;
  end
  else begin
    Result := CompilationUnit.Owner.MemManager <> nil;
    if not Result then
      exit;
    AnAddress := CompilationUnit.Owner.MemManager.ReadAddress(AnAddress, CompilationUnit.AddressSize);
    AValueObj.DataAddressCache[ATargetCacheIndex] := AnAddress;
  end;
  Result := IsValidLoc(AnAddress);

  if Result then
    Result := inherited GetDataAddress(AValueObj, AnAddress, ATargetType, ATargetCacheIndex)
  else
  if IsError(CompilationUnit.Owner.MemManager.LastError) then
    SetLastError(CompilationUnit.Owner.MemManager.LastError);
  // Todo: other error
end;

{ TDbgDwarfTypeIdentifierDeclaration }

function TDbgDwarfTypeIdentifierDeclaration.DoGetNestedTypeInfo: TDbgDwarfTypeIdentifier;
var
  ti: TDbgDwarfTypeIdentifier;
  ti2: TFpDbgSymbol;
begin
  Result := inherited DoGetNestedTypeInfo;

  // Is internal class pointer?
  // Do not trigged any cached property of the pointer
  if (Result = nil) then
    exit;

  ti := Result;
  if (ti is TDbgDwarfTypeIdentifierModifier) then begin
    ti := TDbgDwarfTypeIdentifier(ti.TypeInfo);
    if (Result = nil) then
      exit;
  end;
  if not (ti is TDbgDwarfTypeIdentifierPointer) then
    exit;

  ti2 := ti.NestedTypeInfo;
  // only if it is NOT a declaration
  if (ti2 <> nil) and (ti2 is TDbgDwarfIdentifierStructure) then begin
    TDbgDwarfTypeIdentifierPointer(ti).IsInternalPointer := True;
    // TODO: Flag the structure as class (save teme in KindNeeded)
  end;
end;

{ TDbgDwarfIdentifierSubRange }

procedure TDbgDwarfIdentifierSubRange.InitEnumIdx;
var
  t: TDbgDwarfTypeIdentifier;
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

procedure TDbgDwarfIdentifierSubRange.ReadBounds;
var
  FwdInfoPtr: Pointer;
  FwdCompUint: TDwarfCompilationUnit;
  NewInfo: TDwarfInformationEntry;
begin
  if FLowBoundState <> rfNotRead then exit;

  // Todo: search attrib-IDX only once
  if InformationEntry.ReadReference(DW_AT_lower_bound, FwdInfoPtr, FwdCompUint) then begin
    NewInfo := TDwarfInformationEntry.Create(FwdCompUint, FwdInfoPtr);
    FLowBoundValue := TDbgDwarfValueIdentifier.CreateValueSubClass('', NewInfo);
    NewInfo.ReleaseReference;
    if FLowBoundValue = nil then begin
      FLowBoundState := rfNotFound;
      exit;
    end
    else
      FLowBoundState := rfValue;
  end
  else
  if InformationEntry.ReadValue(DW_AT_lower_bound, FLowBoundConst) then begin
    FLowBoundState := rfConst;
  end
  else
  begin
    //FLowBoundConst := 0; // the default
    //FLowBoundState := rfConst;
    FLowBoundState := rfNotFound;
    exit; // incomplete type
  end;


  if InformationEntry.ReadReference(DW_AT_upper_bound, FwdInfoPtr, FwdCompUint) then begin
    NewInfo := TDwarfInformationEntry.Create(FwdCompUint, FwdInfoPtr);
    FHighBoundValue := TDbgDwarfValueIdentifier.CreateValueSubClass('', NewInfo);
    NewInfo.ReleaseReference;
    if FHighBoundValue = nil then begin
      FHighBoundState := rfNotFound;
      exit;
    end
    else
      FHighBoundState := rfValue;
  end
  else
  if InformationEntry.ReadValue(DW_AT_upper_bound, FHighBoundConst) then begin
    FHighBoundState := rfConst;
  end
  else
  begin
    FHighBoundState := rfNotFound;

    if InformationEntry.ReadReference(DW_AT_count, FwdInfoPtr, FwdCompUint) then begin
      NewInfo := TDwarfInformationEntry.Create(FwdCompUint, FwdInfoPtr);
      FCountValue := TDbgDwarfValueIdentifier.CreateValueSubClass('', NewInfo);
      NewInfo.ReleaseReference;
      if FCountValue = nil then begin
        FCountState := rfNotFound;
        exit;
      end
      else
        FCountState := rfValue;
    end
    else
    if InformationEntry.ReadValue(DW_AT_count, FCountConst) then begin
      FCountState := rfConst;
    end
    else
      FCountState := rfNotFound;
  end;
end;

function TDbgDwarfIdentifierSubRange.DoGetNestedTypeInfo: TDbgDwarfTypeIdentifier;
begin
  Result := inherited DoGetNestedTypeInfo;
  if Result <> nil then
    exit;

  if FLowBoundState = rfValue then
    Result := FLowBoundValue.TypeInfo as TDbgDwarfTypeIdentifier
  else
  if FHighBoundState = rfValue then
    Result := FHighBoundValue.TypeInfo as TDbgDwarfTypeIdentifier
  else
  if FCountState = rfValue then
    Result := FCountValue.TypeInfo as TDbgDwarfTypeIdentifier;
end;

function TDbgDwarfIdentifierSubRange.GetHasBounds: Boolean;
begin
  ReadBounds;
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

function TDbgDwarfIdentifierSubRange.GetOrdHighBound: Int64;
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

function TDbgDwarfIdentifierSubRange.GetOrdLowBound: Int64;
begin
  //if FLowBoundState = rfValue then
  //  Result := FLowBoundValue.VALUE // TODO
  //else
    Result := FLowBoundConst;
end;

procedure TDbgDwarfIdentifierSubRange.NameNeeded;
var
  AName: String;
begin
  if InformationEntry.ReadName(AName) then
    SetName(AName)
  else
    SetName('');
end;

procedure TDbgDwarfIdentifierSubRange.KindNeeded;
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

procedure TDbgDwarfIdentifierSubRange.SizeNeeded;
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

function TDbgDwarfIdentifierSubRange.GetMember(AIndex: Int64): TFpDbgSymbol;
begin
  if Kind = skEnum then begin
    if not FEnumIdxValid then
      InitEnumIdx;
    Result := NestedTypeInfo.Member[AIndex - FLowEnumIdx];
  end
  else
    Result := inherited GetMember(AIndex);
end;

function TDbgDwarfIdentifierSubRange.GetMemberCount: Integer;
begin
  if Kind = skEnum then begin
    if not FEnumIdxValid then
      InitEnumIdx;
    Result := FHighEnumIdx - FLowEnumIdx + 1;
  end
  else
    Result := inherited GetMemberCount;
end;

function TDbgDwarfIdentifierSubRange.GetFlags: TDbgSymbolFlags;
begin
  Result := (inherited GetFlags) + [sfSubRange];
end;

function TDbgDwarfIdentifierSubRange.GetValueBounds(AValueObj: TFpDbgDwarfValue; out
  ALowBound, AHighBound: Int64): Boolean;
begin
  ReadBounds;
  Result := inherited GetValueBounds(AValueObj, ALowBound, AHighBound);
end;

procedure TDbgDwarfIdentifierSubRange.Init;
begin
  FLowBoundState := rfNotRead;
  FHighBoundState := rfNotRead;
  FCountState := rfNotRead;
  inherited Init;
end;

{ TDbgDwarfTypeIdentifierPointer }

function TDbgDwarfTypeIdentifierPointer.IsInternalDynArrayPointer: Boolean;
var
  ti: TFpDbgSymbol;
begin
  Result := False;
  ti := NestedTypeInfo;  // Same as TypeInfo, but does not try to be forwarded
  Result := (ti <> nil) and (ti is TDbgDwarfIdentifierArray);
  if Result then
    Result := (sfDynArray in ti.Flags);
end;

procedure TDbgDwarfTypeIdentifierPointer.TypeInfoNeeded;
var
  p: TDbgDwarfTypeIdentifier;
begin
  p := NestedTypeInfo;
  if IsInternalPointer and (p <> nil) then begin
    SetTypeInfo(p.TypeInfo);
    exit;
  end;
  SetTypeInfo(p);
end;

function TDbgDwarfTypeIdentifierPointer.GetIsInternalPointer: Boolean;
begin
  Result := FIsInternalPointer or IsInternalDynArrayPointer;
end;

procedure TDbgDwarfTypeIdentifierPointer.KindNeeded;
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

procedure TDbgDwarfTypeIdentifierPointer.SizeNeeded;
begin
  SetSize(CompilationUnit.AddressSize);
end;

procedure TDbgDwarfTypeIdentifierPointer.ForwardToSymbolNeeded;
begin
  if IsInternalPointer then
    SetForwardToSymbol(NestedTypeInfo) // Same as TypeInfo, but does not try to be forwarded
  else
    SetForwardToSymbol(nil); // inherited ForwardToSymbolNeeded;
end;

function TDbgDwarfTypeIdentifierPointer.GetDataAddress(AValueObj: TFpDbgDwarfValue;
  var AnAddress: TFpDbgMemLocation; ATargetType: TDbgDwarfTypeIdentifier;
  ATargetCacheIndex: Integer): Boolean;
var
  t: TFpDbgMemLocation;
begin
  if ATargetType = Self then begin
    Result := True;
    exit;
  end;

  t := AValueObj.DataAddressCache[ATargetCacheIndex];
  if IsInitializedLoc(t) then begin
    AnAddress := t;
  end
  else begin
    Result := CompilationUnit.Owner.MemManager <> nil;
    if not Result then
      exit;
    AnAddress := CompilationUnit.Owner.MemManager.ReadAddress(AnAddress, CompilationUnit.AddressSize);
    AValueObj.DataAddressCache[ATargetCacheIndex] := AnAddress;
  end;
  Result := IsValidLoc(AnAddress);

  if Result then
    Result := inherited GetDataAddress(AValueObj, AnAddress, ATargetType, ATargetCacheIndex)
  else
  if IsError(CompilationUnit.Owner.MemManager.LastError) then
    SetLastError(CompilationUnit.Owner.MemManager.LastError);
  // Todo: other error
end;

function TDbgDwarfTypeIdentifierPointer.GetTypedValueObject(ATypeCast: Boolean): TFpDbgDwarfValue;
begin
  if IsInternalPointer then
    Result := NestedTypeInfo.GetTypedValueObject(ATypeCast)
  else
    Result := TFpDbgDwarfValuePointer.Create(Self, CompilationUnit.AddressSize);
end;

function TDbgDwarfTypeIdentifierPointer.DataSize: Integer;
begin
  if Kind = skClass then
    Result := NestedTypeInfo.Size
  else
    Result := inherited DataSize;
end;

{ TDbgDwarfIdentifierEnumElement }

procedure TDbgDwarfIdentifierEnumMember.ReadOrdinalValue;
begin
  if FOrdinalValueRead then exit;
  FOrdinalValueRead := True;
  FHasOrdinalValue := InformationEntry.ReadValue(DW_AT_const_value, FOrdinalValue);
end;

procedure TDbgDwarfIdentifierEnumMember.KindNeeded;
begin
  SetKind(skEnumValue);
end;

function TDbgDwarfIdentifierEnumMember.GetHasOrdinalValue: Boolean;
begin
  ReadOrdinalValue;
  Result := FHasOrdinalValue;
end;

function TDbgDwarfIdentifierEnumMember.GetOrdinalValue: Int64;
begin
  ReadOrdinalValue;
  Result := FOrdinalValue;
end;

procedure TDbgDwarfIdentifierEnumMember.Init;
begin
  FOrdinalValueRead := False;
  inherited Init;
end;

function TDbgDwarfIdentifierEnumMember.GetValueObject: TFpDbgValue;
begin
  Result := FValueObject;
  if Result <> nil then exit;

  FValueObject := TFpDbgDwarfValueEnumMember.Create(Self);
  FValueObject.MakePlainRefToCirclular;
  FValueObject.SetValueSymbol(self);

  Result := FValueObject;
end;

{ TDbgDwarfIdentifierEnum }

procedure TDbgDwarfIdentifierEnum.CreateMembers;
var
  Info, Info2: TDwarfInformationEntry;
  sym: TDbgDwarfIdentifier;
begin
  if FMembers <> nil then
    exit;
  FMembers := TFpDbgCircularRefCntObjList.Create;
  Info := InformationEntry.FirstChild;
  if Info = nil then exit;

  while Info.HasValidScope do begin
    if (Info.AbbrevTag = DW_TAG_enumerator) then begin
      Info2 := Info.Clone;
      sym := TDbgDwarfIdentifier.CreateSubClass('', Info2);
      FMembers.Add(sym);
      sym.ReleaseReference;
      sym.ParentTypeInfo := self;
      Info2.ReleaseReference;
    end;
    Info.GoNext;
  end;

  Info.ReleaseReference;
end;

function TDbgDwarfIdentifierEnum.GetTypedValueObject(ATypeCast: Boolean): TFpDbgDwarfValue;
begin
  Result := TFpDbgDwarfValueEnum.Create(Self, Size);
end;

procedure TDbgDwarfIdentifierEnum.KindNeeded;
begin
  SetKind(skEnum);
end;

function TDbgDwarfIdentifierEnum.GetMember(AIndex: Int64): TFpDbgSymbol;
begin
  CreateMembers;
  Result := TFpDbgSymbol(FMembers[AIndex]);
end;

function TDbgDwarfIdentifierEnum.GetMemberByName(AIndex: String): TFpDbgSymbol;
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

function TDbgDwarfIdentifierEnum.GetMemberCount: Integer;
begin
  CreateMembers;
  Result := FMembers.Count;
end;

function TDbgDwarfIdentifierEnum.GetHasBounds: Boolean;
begin
  Result := True;
end;

function TDbgDwarfIdentifierEnum.GetOrdHighBound: Int64;
var
  c: Integer;
begin
  c := MemberCount;
  if c > 0 then
    Result := Member[c-1].OrdinalValue
  else
    Result := -1;
end;

function TDbgDwarfIdentifierEnum.GetOrdLowBound: Int64;
var
  c: Integer;
begin
  c := MemberCount;
  if c > 0 then
    Result := Member[0].OrdinalValue
  else
    Result := 0;
end;

destructor TDbgDwarfIdentifierEnum.Destroy;
var
  i: Integer;
begin
  if FMembers <> nil then
    for i := 0 to FMembers.Count - 1 do
      TDbgDwarfIdentifier(FMembers[i]).ParentTypeInfo := nil;
  FreeAndNil(FMembers);
  inherited Destroy;
end;

{ TDbgDwarfIdentifierSet }

procedure TDbgDwarfIdentifierSet.KindNeeded;
begin
  SetKind(skSet);
end;

function TDbgDwarfIdentifierSet.GetTypedValueObject(ATypeCast: Boolean): TFpDbgDwarfValue;
begin
  Result := TFpDbgDwarfValueSet.Create(Self, Size);
end;

function TDbgDwarfIdentifierSet.GetMemberCount: Integer;
begin
  if TypeInfo.Kind = skEnum then
    Result := TypeInfo.MemberCount
  else
    Result := inherited GetMemberCount;
end;

function TDbgDwarfIdentifierSet.GetMember(AIndex: Int64): TFpDbgSymbol;
begin
  if TypeInfo.Kind = skEnum then
    Result := TypeInfo.Member[AIndex]
  else
    Result := inherited GetMember(AIndex);
end;

{ TDbgDwarfIdentifierMember }

function TDbgDwarfIdentifierMember.InitLocationParser(const ALocationParser: TDwarfLocationExpression;
  AValueObj: TFpDbgDwarfValue; AnObjectDataAddress: TFpDbgMemLocation): Boolean;
var
  BaseAddr: TFpDbgMemLocation;
begin
  Result := inherited InitLocationParser(ALocationParser, AValueObj, AnObjectDataAddress);
  if not Result then
    exit;

if AValueObj = nil then debugln(['TDbgDwarfIdentifierMember.InitLocationParser: NO VAl Obj !!!!!!!!!!!!!!!'])
else if AValueObj.StructureValue = nil then debugln(['TDbgDwarfIdentifierMember.InitLocationParser: NO STRUCT Obj !!!!!!!!!!!!!!!']);
  if (AValueObj <> nil) and (AValueObj.StructureValue <> nil) and (ParentTypeInfo <> nil) then begin
    Assert((ParentTypeInfo is TDbgDwarfIdentifier) and (ParentTypeInfo.SymbolType = stType), '');
    if AValueObj.GetStructureDwarfDataAddress(BaseAddr, TDbgDwarfTypeIdentifier(ParentTypeInfo)) then begin
      ALocationParser.Push(BaseAddr, lseValue);
      exit
    end;
    //TODO: AValueObj.StructureValue.LastError
  end;

  debugln(FPDBG_DWARF_ERRORS, ['DWARF ERROR in TDbgDwarfIdentifierMember.InitLocationParser Error: ',ErrorCode(LastError),' ValueObject=', DbgSName(FValueObject)]);
  if not IsError(LastError) then
    SetLastError(CreateError(fpErrLocationParserInit));
  Result := False;
end;

function TDbgDwarfIdentifierMember.GetValueAddress(AValueObj: TFpDbgDwarfValue; out
  AnAddress: TFpDbgMemLocation): Boolean;
begin
  AnAddress := AValueObj.DataAddressCache[0];
  Result := IsValidLoc(AnAddress);
  if IsInitializedLoc(AnAddress) then
    exit;
  Result := LocationFromTag(DW_AT_data_member_location, AValueObj, AnAddress, InvalidLoc);
  AValueObj.DataAddressCache[0] := AnAddress;
end;

function TDbgDwarfIdentifierMember.HasAddress: Boolean;
begin
  Result := (InformationEntry.HasAttrib(DW_AT_data_member_location));
end;

{ TDbgDwarfIdentifierStructure }

function TDbgDwarfIdentifierStructure.GetMemberByName(AIndex: String): TFpDbgSymbol;
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
    FLastChildByName := TDbgDwarfIdentifier.CreateSubClass('', Ident);
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

function TDbgDwarfIdentifierStructure.GetMemberCount: Integer;
begin
  CreateMembers;
  Result := FMembers.Count;
end;

function TDbgDwarfIdentifierStructure.InitLocationParser(const ALocationParser: TDwarfLocationExpression;
  AValueObj: TFpDbgDwarfValue; AnObjectDataAddress: TFpDbgMemLocation): Boolean;
begin
  Result := inherited InitLocationParser(ALocationParser, AValueObj, AnObjectDataAddress);
  if not Result then
    exit;

  // CURRENTLY ONLY USED for DW_AT_data_member_location
  if IsReadableLoc(AnObjectDataAddress) then begin
    debugln(FPDBG_DWARF_SEARCH, ['TDbgDwarfIdentifierStructure.InitLocationParser ', dbgs(AnObjectDataAddress)]);
    ALocationParser.Push(AnObjectDataAddress, lseValue);
    exit;
  end;

  //TODO: error
  debugln(FPDBG_DWARF_ERRORS, ['DWARF ERROR in TDbgDwarfIdentifierStructure.InitLocationParser no ObjectDataAddress ', dbgs(AnObjectDataAddress)]);
  if not IsError(LastError) then
    SetLastError(CreateError(fpErrLocationParserInit));
  Result := False;
end;

function TDbgDwarfIdentifierStructure.GetDataAddress(AValueObj: TFpDbgDwarfValue;
  var AnAddress: TFpDbgMemLocation; ATargetType: TDbgDwarfTypeIdentifier;
  ATargetCacheIndex: Integer): Boolean;
var
  t: TFpDbgMemLocation;
begin
  if ATargetType = Self then begin
    Result := True;
    exit;
  end;

  t := AValueObj.DataAddressCache[ATargetCacheIndex];
  if IsInitializedLoc(t) then begin
    AnAddress := t;
    Result := IsValidLoc(AnAddress);
  end
  else begin
    InitInheritanceInfo;
    //TODO: may be a constant // offset
    Result := LocationFromTag(DW_AT_data_member_location, AValueObj, t, AnAddress, FInheritanceInfo);
    if not Result then
      exit;
    AnAddress := t;
    AValueObj.DataAddressCache[ATargetCacheIndex] := AnAddress;
  end;

  Result := inherited GetDataAddress(AValueObj, AnAddress, ATargetType, ATargetCacheIndex);
end;

function TDbgDwarfIdentifierStructure.GetMember(AIndex: Int64): TFpDbgSymbol;
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

destructor TDbgDwarfIdentifierStructure.Destroy;
var
  i: Integer;
begin
  ReleaseRefAndNil(FInheritanceInfo);
  if FMembers <> nil then begin
    for i := 0 to FMembers.Count - 1 do
      TDbgDwarfIdentifier(FMembers[i]).ParentTypeInfo := nil;
    FreeAndNil(FMembers);
  end;
  if FLastChildByName <> nil then begin
    FLastChildByName.ParentTypeInfo := nil;
    FLastChildByName.ReleaseCirclularReference;
    FLastChildByName := nil;
  end;
  inherited Destroy;
end;

procedure TDbgDwarfIdentifierStructure.CreateMembers;
var
  Info: TDwarfInformationEntry;
  Info2: TDwarfInformationEntry;
  sym: TDbgDwarfIdentifier;
begin
  if FMembers <> nil then
    exit;
  FMembers := TFpDbgCircularRefCntObjList.Create;
  Info := InformationEntry.Clone;
  Info.GoChild;

  while Info.HasValidScope do begin
    if (Info.AbbrevTag = DW_TAG_member) or (Info.AbbrevTag = DW_TAG_subprogram) then begin
      Info2 := Info.Clone;
      sym := TDbgDwarfIdentifier.CreateSubClass('', Info2);
      FMembers.Add(sym);
      sym.ReleaseReference;
      sym.ParentTypeInfo := self;
      Info2.ReleaseReference;
    end;
    Info.GoNext;
  end;

  Info.ReleaseReference;
end;

procedure TDbgDwarfIdentifierStructure.InitInheritanceInfo;
begin
  if FInheritanceInfo = nil then
    FInheritanceInfo := InformationEntry.FindChildByTag(DW_TAG_inheritance);
end;

function TDbgDwarfIdentifierStructure.DoGetNestedTypeInfo: TDbgDwarfTypeIdentifier;
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
    Result := TDbgDwarfTypeIdentifier.CreateTypeSubClass('', ParentInfo);
    ParentInfo.ReleaseReference;
  end;
end;

procedure TDbgDwarfIdentifierStructure.KindNeeded;
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

function TDbgDwarfIdentifierStructure.GetTypedValueObject(ATypeCast: Boolean): TFpDbgDwarfValue;
begin
  if ATypeCast then
    Result := TFpDbgDwarfValueStructTypeCast.Create(Self)
  else
    Result := TFpDbgDwarfValueStruct.Create(Self);
end;

{ TDbgDwarfIdentifierArray }

procedure TDbgDwarfIdentifierArray.CreateMembers;
var
  Info, Info2: TDwarfInformationEntry;
  t: Cardinal;
  sym: TDbgDwarfIdentifier;
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
      sym := TDbgDwarfIdentifier.CreateSubClass('', Info2);
      FMembers.Add(sym);
      sym.ReleaseReference;
      sym.ParentTypeInfo := self;
      Info2.ReleaseReference;
    end;
    Info.GoNext;
  end;

  Info.ReleaseReference;
end;

procedure TDbgDwarfIdentifierArray.ReadStride;
var
  t: TDbgDwarfTypeIdentifier;
begin
  if didtStrideRead in FDwarfArrayReadFlags then
    exit;
  Include(FDwarfArrayReadFlags, didtStrideRead);
  if not InformationEntry.ReadValue(DW_AT_bit_stride, FStrideInBits) then begin
    t := NestedTypeInfo;
    if t = nil then
      FStrideInBits := 0
    else
      FStrideInBits := t.Size * 8;
  end;
end;

procedure TDbgDwarfIdentifierArray.ReadOrdering;
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

procedure TDbgDwarfIdentifierArray.KindNeeded;
begin
  SetKind(skArray); // Todo: static/dynamic?
end;

function TDbgDwarfIdentifierArray.GetTypedValueObject(ATypeCast: Boolean): TFpDbgDwarfValue;
begin
  Result := TFpDbgDwarfValueArray.Create(Self);
end;

function TDbgDwarfIdentifierArray.GetFlags: TDbgSymbolFlags;
  function IsDynSubRange(m: TDbgDwarfIdentifier): Boolean;
  begin
    Result := sfSubRange in m.Flags;
    if not Result then exit;
    while (m <> nil) and not(m is TDbgDwarfIdentifierSubRange) do
      m := m.NestedTypeInfo;
    Result := m <> nil;
    if not Result then exit; // TODO: should not happen, handle error
    Result := TDbgDwarfIdentifierSubRange(m).FHighBoundState = rfValue; // dynamic high bound
  end;
var
  m: TFpDbgSymbol;
begin
  Result := inherited GetFlags;
  if (MemberCount = 1) then begin
    m := Member[0];
    if (not m.HasBounds) or                // e.g. Subrange with missing upper bound
       (m.OrdHighBound < m.OrdLowBound) or
       (IsDynSubRange(TDbgDwarfIdentifier(m)))
    then
      Result := Result + [sfDynArray]
    else
      Result := Result + [sfStatArray];
  end
  else
    Result := Result + [sfStatArray];
end;

function TDbgDwarfIdentifierArray.GetMember(AIndex: Int64): TFpDbgSymbol;
begin
  CreateMembers;
  Result := TFpDbgSymbol(FMembers[AIndex]);
end;

function TDbgDwarfIdentifierArray.GetMemberByName(AIndex: String): TFpDbgSymbol;
begin
  Result := nil; // no named members
end;

function TDbgDwarfIdentifierArray.GetMemberCount: Integer;
begin
  CreateMembers;
  Result := FMembers.Count;
end;

function TDbgDwarfIdentifierArray.GetMemberAddress(AValObject: TFpDbgDwarfValue;
  AIndex: array of Int64): TFpDbgMemLocation;
var
  Idx, Offs, Factor: Int64;
  i: Integer;
  bsize: Integer;
  m: TDbgDwarfIdentifier;
begin
  assert((AValObject is TFpDbgDwarfValueArray), 'TDbgDwarfIdentifierArray.GetMemberAddress AValObject');
  ReadOrdering;
  ReadStride;
  Result := InvalidLoc;
  if (FStrideInBits <= 0) or (FStrideInBits mod 8 <> 0) then
    exit;

  CreateMembers;
  if Length(AIndex) > FMembers.Count then
    exit;

  if AValObject is TFpDbgDwarfValueArray then begin
    if not TFpDbgDwarfValueArray(AValObject).GetDwarfDataAddress(Result, Self) then begin
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
      m := TDbgDwarfIdentifier(FMembers[i]);
      if m.HasBounds then begin
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
      m := TDbgDwarfIdentifier(FMembers[i]);
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

destructor TDbgDwarfIdentifierArray.Destroy;
var
  i: Integer;
begin
  if FMembers <> nil then begin
    for i := 0 to FMembers.Count - 1 do
      TDbgDwarfIdentifier(FMembers[i]).ParentTypeInfo := nil;
    FreeAndNil(FMembers);
  end;
  inherited Destroy;
end;

{ TDbgDwarfSymbol }

constructor TDbgDwarfProcSymbol.Create(ACompilationUnit: TDwarfCompilationUnit;
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

destructor TDbgDwarfProcSymbol.Destroy;
begin
  FreeAndNil(FStateMachine);
  if FSelfParameter <> nil then begin
    FSelfParameter.ParentTypeInfo := nil;
    FSelfParameter.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSelfParameter, 'FSelfParameter'){$ENDIF};
  end;
  inherited Destroy;
end;

function TDbgDwarfProcSymbol.GetColumn: Cardinal;
begin
  if StateMachineValid
  then Result := FStateMachine.Column
  else Result := inherited GetColumn;
end;

function TDbgDwarfProcSymbol.GetFile: String;
begin
  if StateMachineValid
  then Result := FStateMachine.FileName
  else Result := inherited GetFile;
end;

function TDbgDwarfProcSymbol.GetLine: Cardinal;
begin
  if StateMachineValid
  then Result := FStateMachine.Line
  else Result := inherited GetLine;
end;

function TDbgDwarfProcSymbol.StateMachineValid: Boolean;
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

function TDbgDwarfProcSymbol.ReadVirtuality(out AFlags: TDbgSymbolFlags): Boolean;
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

function TDbgDwarfProcSymbol.GetFrameBase: TDbgPtr;
var
  Val: TByteDynArray;
begin
  Result := 0;
  if FFrameBaseParser = nil then begin
    //TODO: avoid copying data
    if not  InformationEntry.ReadValue(DW_AT_frame_base, Val) then begin
      // error
      debugln(FPDBG_DWARF_ERRORS, ['TDbgDwarfProcSymbol.GetFrameBase failed to read DW_AT_frame_base']);
      exit;
    end;
    if Length(Val) = 0 then begin
      // error
      debugln(FPDBG_DWARF_ERRORS, ['TDbgDwarfProcSymbol.GetFrameBase failed to read DW_AT_location']);
      exit;
    end;

    FFrameBaseParser := TDwarfLocationExpression.Create(@Val[0], Length(Val), CompilationUnit);
    FFrameBaseParser.Evaluate;

    if FFrameBaseParser.ResultKind in [lseValue] then
      Result := FFrameBaseParser.ResultData;

    if IsError(FFrameBaseParser.LastError) then begin
      SetLastError(FFrameBaseParser.LastError);
      debugln(FPDBG_DWARF_ERRORS, ['TDbgDwarfProcSymbol.GetFrameBase location parser failed ', ErrorHandler.ErrorAsString(LastError)]);
    end
    else
    if Result = 0 then begin
      debugln(FPDBG_DWARF_ERRORS, ['TDbgDwarfProcSymbol.GetFrameBase location parser failed. result is 0']);
    end;

  end;
end;

procedure TDbgDwarfProcSymbol.KindNeeded;
begin
  if TypeInfo <> nil then
    SetKind(skFunction)
  else
    SetKind(skProcedure);
end;

procedure TDbgDwarfProcSymbol.SizeNeeded;
begin
  SetSize(FAddressInfo^.EndPC - FAddressInfo^.StartPC);
end;

function TDbgDwarfProcSymbol.GetFlags: TDbgSymbolFlags;
var
  flg: TDbgSymbolFlags;
begin
  Result := inherited GetFlags;
  if ReadVirtuality(flg) then
    Result := Result + flg;
end;

function TDbgDwarfProcSymbol.GetSelfParameter(AnAddress: TDbgPtr): TDbgDwarfValueIdentifier;
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
        Result := TDbgDwarfValueIdentifier.CreateValueSubClass('self', InfoEntry);
        FSelfParameter := Result;
        {$IFDEF WITH_REFCOUNT_DEBUG}FSelfParameter.DbgRenameReference(@FSelfParameter, 'FSelfParameter');{$ENDIF}
        //FSelfParameter.DbgSymbol.ParentTypeInfo := Self;
        debugln(FPDBG_DWARF_SEARCH, ['TDbgDwarfProcSymbol.GetSelfParameter ', InfoEntry.ScopeDebugText, DbgSName(Result)]);
      end;
    end;
  end;
  InfoEntry.ReleaseReference;
end;

{ TDbgDwarfIdentifierVariable }

function TDbgDwarfIdentifierVariable.GetValueAddress(AValueObj: TFpDbgDwarfValue; out
  AnAddress: TFpDbgMemLocation): Boolean;
begin
  AnAddress := AValueObj.DataAddressCache[0];
  Result := IsValidLoc(AnAddress);
  if IsInitializedLoc(AnAddress) then
    exit;
  Result := LocationFromTag(DW_AT_location, AValueObj, AnAddress, InvalidLoc);
  AValueObj.DataAddressCache[0] := AnAddress;
end;

function TDbgDwarfIdentifierVariable.HasAddress: Boolean;
begin
  Result := InformationEntry.HasAttrib(DW_AT_location);
end;

{ TDbgDwarfIdentifierParameter }

function TDbgDwarfIdentifierParameter.GetValueAddress(AValueObj: TFpDbgDwarfValue; out
  AnAddress: TFpDbgMemLocation): Boolean;
begin
  AnAddress := AValueObj.DataAddressCache[0];
  Result := IsValidLoc(AnAddress);
  if IsInitializedLoc(AnAddress) then
    exit;
  Result := LocationFromTag(DW_AT_location, AValueObj, AnAddress, InvalidLoc);
  AValueObj.DataAddressCache[0] := AnAddress;
end;

function TDbgDwarfIdentifierParameter.HasAddress: Boolean;
begin
  Result := InformationEntry.HasAttrib(DW_AT_location);
end;

{ TDbgDwarfUnit }

procedure TDbgDwarfUnit.Init;
begin
  inherited Init;
  SetSymbolType(stNone);
  SetKind(skUnit);
end;

function TDbgDwarfUnit.GetMemberByName(AIndex: String): TFpDbgSymbol;
var
  Ident: TDwarfInformationEntry;
begin
  // Todo, param to only search external.
  ReleaseRefAndNil(FLastChildByName);
  Result := nil;

  Ident := InformationEntry.Clone;
  Ident.GoNamedChildEx(AIndex);
  if Ident <> nil then
    Result := TDbgDwarfIdentifier.CreateSubClass('', Ident);
  // No need to set ParentTypeInfo
  ReleaseRefAndNil(Ident);
  FLastChildByName := Result;
end;

destructor TDbgDwarfUnit.Destroy;
begin
  ReleaseRefAndNil(FLastChildByName);
  inherited Destroy;
end;

initialization
  DwarfSymbolClassMapList.SetDefaultMap(TFpDwarfDefaultSymbolClassMap);

  FPDBG_DWARF_ERRORS        := DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_ERRORS' {$IFDEF FPDBG_DWARF_ERRORS} , True {$ENDIF} );
  FPDBG_DWARF_WARNINGS      := DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_WARNINGS' {$IFDEF FPDBG_DWARF_WARNINGS} , True {$ENDIF} );
  FPDBG_DWARF_SEARCH        := DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_SEARCH' {$IFDEF FPDBG_DWARF_SEARCH} , True {$ENDIF} );
  FPDBG_DWARF_DATA_WARNINGS := DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_DATA_WARNINGS' {$IFDEF FPDBG_DWARF_DATA_WARNINGS} , True {$ENDIF} );

end.

