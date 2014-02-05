{ $Id$ }
{
 ---------------------------------------------------------------------------
 fpdbgdwarf.pas  -  Native Freepascal debugger - Dwarf symbol reader
 ---------------------------------------------------------------------------

 This unit contains helper classes for loading and resolving of DWARF debug
 symbols

 ---------------------------------------------------------------------------

 @created(Mon Aug 1st WET 2006)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.nl>)

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
//{$INLINE OFF}
{off $DEFINE USE_ABBREV_TMAP}

interface

uses
  Classes, Types, SysUtils, FpDbgUtil, FpDbgInfo, FpDbgDwarfConst, Maps, Math,
  FpDbgLoader, FpImgReaderBase, LazLoggerBase, // LazLoggerDummy,
  LazClasses, LazFileUtils, LazUTF8, contnrs;

type
  // compilation unit header
  {$PACKRECORDS 1}
  PDwarfCUHeader32 = ^TDwarfCUHeader32;
  TDwarfCUHeader32 = record
    Length: LongWord;
    Version: Word;
    AbbrevOffset: LongWord;
    AddressSize: Byte;
  end;

  PDwarfCUHeader64 = ^TDwarfCUHeader64;
  TDwarfCUHeader64 = record
    Signature: LongWord;
    Length: QWord;
    Version: Word;
    AbbrevOffset: QWord;
    AddressSize: Byte;
  end;
  
  // Line number program header
  PDwarfLNPInfoHeader = ^TDwarfLNPInfoHeader;
  TDwarfLNPInfoHeader = record
    MinimumInstructionLength: Byte;
    DefaultIsStmt: Byte;
    LineBase: ShortInt;
    LineRange: Byte;
    OpcodeBase: Byte;
    StandardOpcodeLengths: record end; {array[1..OpcodeBase-1] of Byte}
    {IncludeDirectories: asciiz, asciiz..z}
    {FileNames: asciiz, asciiz..z}
  end;

  PDwarfLNPHeader32 = ^TDwarfLNPHeader32;
  TDwarfLNPHeader32 = record
    UnitLength: LongWord;
    Version: Word;
    HeaderLength: LongWord;
    Info: TDwarfLNPInfoHeader;
  end;

  PDwarfLNPHeader64 = ^TDwarfLNPHeader64;
  TDwarfLNPHeader64 = record
    Signature: LongWord;
    UnitLength: QWord;
    Version: Word;
    HeaderLength: QWord;
    Info: TDwarfLNPInfoHeader;
  end;
  
  {$PACKRECORDS C}
  
const
  DWARF_HEADER64_SIGNATURE = $FFFFFFFF;

type
  TDbgDwarf = class;
  TDwarfCompilationUnit = class;

  { TDwarfAbbrev }
  TDwarfAbbrevFlag = (
    dafHasChildren,
    dafHasName,
    dafHasLowAddr,
    dafHasStartScope,
    dafHasAbstractOrigin
  );
  TDwarfAbbrevFlags = set of TDwarfAbbrevFlag;

  TDwarfAbbrev = record
    tag: Cardinal;
    index: Integer;
    count: SmallInt; // Integer;
    flags: TDwarfAbbrevFlags;
  end;
  PDwarfAbbrev = ^TDwarfAbbrev;

  TDwarfAbbrevEntry = record
    Attribute: Cardinal;
    Form: Cardinal;
  end;
  PDwarfAbbrevEntry = ^TDwarfAbbrevEntry;

  TLeb128TableEntry = record
    LeadLow, LeadHigh: Byte; // bytes >= 128, more to follow
    EndLow,  EndHigh: Byte;  // bytes < 128, pointer to data
    LeadIndex: cardinal;     // first index in LeadTableData
    EndIndex: cardinal;      // first index in EndTableData
  end;
  PLeb128TableEntry = ^TLeb128TableEntry;

  TPointerDynArray = array of Pointer;
  TAttribPointerList = record
    List: TPointerDynArray;
    Abbrev: PDwarfAbbrev;
    EvalCount: Integer;
  end;

  { TLEB128PreFixTree }

  TLEB128PreFixTree = class
  private
    FTableList: Array of TLeb128TableEntry;
    FTableListGaps: Array of record LeadTable, EndTable: Byte; end;
    FTableListNextFreeIndex: Cardinal;

    FLeadTableData: Array of Cardinal; //  Next Table number
    FLeadTableNextFreeIndex: Cardinal;
    FEndTableData:  Array of TDwarfAbbrev; //Pointer;
    FEndTableNextFreeIndex: Cardinal;

    FDataGrowStep, FTableListGrowStep: Cardinal;
  protected
  public
    procedure SetCapacity(ACapacity: integer);
    procedure Finish;
    function AddLeb128FromPointer(APointer: Pointer; const AData: TDwarfAbbrev): Pointer;
    function FindLe128bFromPointer(APointer: Pointer; out AData: PDwarfAbbrev): Pointer; // returnns pointer to first address after LEB128
    function FindLe128bFromPointer(APointer: Pointer; out AData: TDwarfAbbrev): Pointer; inline; // returnns pointer to first address after LEB128
  end;

  { TDwarfAbbrevList }

  TDwarfAbbrevList = class{$IFnDEF USE_ABBREV_TMAP}(TLEB128PreFixTree){$Endif}
  private
    FAbbrDataEnd: Pointer;
    {$IFDEF USE_ABBREV_TMAP}
    FMap: TMap;  // Abbrevs
    {$Endif}
    FDefinitions: array of TDwarfAbbrevEntry;
    function GetEntryPointer(AIndex: Integer): PDwarfAbbrevEntry; inline;
  protected
    procedure LoadAbbrevs(AnAbbrevDataPtr: Pointer);
  public
    constructor Create(AnAbbrData, AnAbbrDataEnd: Pointer; AnAbbrevOffset, AInfoLen: QWord);
    destructor Destroy; override;
    {$IFDEF USE_ABBREV_TMAP}
    function FindLe128bFromPointer(AnAbbrevPtr: Pointer; out AData: TDwarfAbbrev{Pointer}): Pointer; reintroduce;
    {$Endif}
    property EntryPointer[AIndex: Integer]: PDwarfAbbrevEntry read GetEntryPointer;
  end;

  (* Link, can either be
     - "Next Sibling" (for the parent): Link will be greater than current index
     - "Parent": Link will be smaller than current index

     By Default link is "Parent".
     A first child does not need a "Parent" link (Parent is always at CurrentIndex - 1),
      it will therefore store "Parent"."Next Sibling"
     A first Child of a parent with no Next sibling, has Link = Parent

     "Next Sibling" is either CurrentIndex + 1 (no children), or can be found via
      the first childs link.
     A Sibling has the same Parent. (If there is no child, and CurrentIndex+1 has
      a diff parent, then there is no Next)

     TopLevel Scopes have Link=-1
  *)
  TDwarfScopeInfoRec = record
    Link: Integer;
    Entry: Pointer;
  end;
  PDwarfScopeInfoRec = ^TDwarfScopeInfoRec;

  TDwarfScopeArray = Array of TDwarfScopeInfoRec;
  TDwarfScopeList = record
    List: TDwarfScopeArray;
    HighestKnown: Integer;
  end;
  PDwarfScopeList = ^TDwarfScopeList;

  { TDwarfScopeInfo }

  TDwarfScopeInfo = object
  private
    FScopeList: PDwarfScopeList;
    FIndex: Integer;
    function GetChild: TDwarfScopeInfo; inline;
    function GetChildIndex: Integer; inline;
    function GetEntry: Pointer; inline;
    function GetNext: TDwarfScopeInfo; inline;
    function GetNextIndex: Integer; inline;
    function GetParent: TDwarfScopeInfo; inline;
    function GetParentIndex: Integer;
    procedure SetIndex(AIndex: Integer);
    function CreateScopeForEntry(AEntry: Pointer; ALink: Integer): Integer; inline;
  public
    procedure Init(AScopeList: PDwarfScopeList);
    function CreateNextForEntry(AEntry: Pointer): Integer; inline;
    function CreateChildForEntry(AEntry: Pointer): Integer; inline;

    function IsValid: Boolean; inline;
    property Index: Integer read FIndex write SetIndex;
    property Entry: Pointer read GetEntry;

    function HasParent: Boolean; inline;
    function HasNext: Boolean; inline;
    function HasChild: Boolean; inline;

    procedure GoParent; inline;
    procedure GoNext; inline;
    procedure GoChild; inline;

    property Parent: TDwarfScopeInfo read GetParent;
    property ParentIndex: Integer read GetParentIndex;
    property Next: TDwarfScopeInfo read GetNext;
    property NextIndex: Integer read GetNextIndex;
    property Child: TDwarfScopeInfo read GetChild;
    property ChildIndex: Integer read GetChildIndex;
  end;

  { TDwarfLocationStack }

  TDwarfLocationStackEntryKind = (
    lseRegister,      // value copied from register (data is in register)
    lseValue,         // unsigned number, or address at which value can be found
    lsePart,          // partial data in next stack entry
    lseError
  );

  TDwarfLocationStackEntry = record
    Value: TDbgPtr; // Address of data, unless lseRegister (in vhich case this holds the data (copy of register)
    Kind:  TDwarfLocationStackEntryKind;
  end;

  TDwarfLocationStack = object
  private
    FList: array of TDwarfLocationStackEntry;
    FCount: Integer;
    procedure IncCapacity;
  public
    procedure Clear;
    function  Count: Integer;
    function  Pop: TDwarfLocationStackEntry;
    function  Peek: TDwarfLocationStackEntry;
    function  PeekKind: TDwarfLocationStackEntryKind;
    function  Peek(AIndex: Integer): TDwarfLocationStackEntry;
    procedure Push(const AEntry: TDwarfLocationStackEntry);
    procedure Push(AValue: TDbgPtr; AKind: TDwarfLocationStackEntryKind);
    procedure Modify(AIndex: Integer; const AEntry: TDwarfLocationStackEntry);
    procedure Modify(AIndex: Integer; AValue: TDbgPtr; AKind: TDwarfLocationStackEntryKind);
  end;

  { TDwarfLocationExpression }

  TDwarfLocationExpression = class
  private
    FFrameBase: TDbgPtr;
    FOnFrameBaseNeeded: TNotifyEvent;
    FStack: TDwarfLocationStack;
    FCU: TDwarfCompilationUnit;
    FData: PByte;
    FMaxData: PByte;
  public
  //TODO: caller keeps data, and determines livetime of data
    constructor Create(AExpressionData: Pointer; AMaxCount: Integer; ACU: TDwarfCompilationUnit);
    procedure Evaluate;
    function  ResultKind: TDwarfLocationStackEntryKind;
    function  ResultData: TDbgPtr;
    property  FrameBase: TDbgPtr read FFrameBase write FFrameBase;
    property  OnFrameBaseNeeded: TNotifyEvent read FOnFrameBaseNeeded write FOnFrameBaseNeeded;
  end;

  { TDwarfInformationEntry }

  TDwarfInformationEntry = class(TRefCountedObject)
  private
    FCompUnit: TDwarfCompilationUnit;
    FInformationEntry: Pointer; // pointer to the LEB128 Abbrev at the start of an Information entry in debug_info
    FInformationData: Pointer;  // poinetr after the LEB128
    FScope: TDwarfScopeInfo;
    FAbbrev: PDwarfAbbrev;
    FAbbrevData: PDwarfAbbrevEntry;
    FAbstractOrigin: TDwarfInformationEntry;
    FFlags: set of (dieAbbrevValid, dieAbbrevDataValid, dieAbstractOriginValid);

    procedure PrepareAbbrev; inline;
    function  PrepareAbbrevData: Boolean; inline;
    function  PrepareAbstractOrigin: Boolean; inline; // Onli call, if abbrev is valid AND dafHasAbstractOrigin set

    function SearchScope: Boolean;
    function MaybeSearchScope: Boolean; inline;
    procedure ScopeChanged; inline;

    function AttribIdx(AnAttrib: Cardinal; out AInfoPointer: pointer): Integer; inline;
    function HasAttrib(AnAttrib: Cardinal): Boolean; inline;

    function GetAbbrevTag: Cardinal; inline;
    function GetScopeIndex: Integer;
    procedure SetScopeIndex(AValue: Integer);
  protected
    function GoNamedChild(AName: String): Boolean;
    // find in enum too // TODO: control search with a flags param, if needed
    function GoNamedChildEx(ANameUpper, AnameLower: PChar): Boolean;
    function GoNamedChildEx(AName: String): Boolean; inline;
  public
    constructor Create(ACompUnit: TDwarfCompilationUnit; AnInformationEntry: Pointer);
    constructor Create(ACompUnit: TDwarfCompilationUnit; AScope: TDwarfScopeInfo);
    destructor Destroy; override;
    property CompUnit: TDwarfCompilationUnit read FCompUnit;

    function FindNamedChild(AName: String): TDwarfInformationEntry;
    function FindChildByTag(ATag: Cardinal): TDwarfInformationEntry;
    function FirstChild: TDwarfInformationEntry;
    function Clone: TDwarfInformationEntry;

    property AbbrevTag: Cardinal read GetAbbrevTag;

    function ReadValue(AnAttrib: Cardinal; out AValue: Integer): Boolean;
    function ReadValue(AnAttrib: Cardinal; out AValue: Int64): Boolean;
    function ReadValue(AnAttrib: Cardinal; out AValue: Cardinal): Boolean;
    function ReadValue(AnAttrib: Cardinal; out AValue: QWord): Boolean;
    function ReadValue(AnAttrib: Cardinal; out AValue: PChar): Boolean;
    function ReadValue(AnAttrib: Cardinal; out AValue: String): Boolean;
    function ReadValue(AnAttrib: Cardinal; out AValue: TByteDynArray): Boolean;
    function ReadReference(AnAttrib: Cardinal; out AValue: Pointer; out ACompUnit: TDwarfCompilationUnit): Boolean;

    function  ReadName(out AName: String): Boolean; inline;
    function  ReadName(out AName: PChar): Boolean; inline;
    function  ReadStartScope(out AStartScope: TDbgPtr): Boolean; inline;
    function  IsAddressInStartScope(AnAddress: TDbgPtr): Boolean; inline;
    function  IsArtificial: Boolean; inline;
  public
    // Scope
    procedure GoParent; inline;
    procedure GoNext; inline;
    procedure GoChild; inline;
    function HasValidScope: Boolean; inline;
    property ScopeIndex: Integer read GetScopeIndex write SetScopeIndex;
  end;

  { TDwarfLineInfoStateMachine }

  TDwarfLineInfoStateMachine = class(TObject)
  private
    FOwner: TDwarfCompilationUnit;
    FLineInfoPtr: Pointer;
    FMaxPtr: Pointer;
    FEnded: Boolean;

    FAddress: QWord;
    FFileName: String;
    FLine: Cardinal;
    FColumn: Cardinal;
    FIsStmt: Boolean;
    FBasicBlock: Boolean;
    FEndSequence: Boolean;
    FPrologueEnd: Boolean;
    FEpilogueBegin: Boolean;
    FIsa: QWord;
    
    procedure SetFileName(AIndex: Cardinal);
  protected
  public
    constructor Create(AOwner: TDwarfCompilationUnit; ALineInfoPtr, AMaxPtr: Pointer);
    function Clone: TDwarfLineInfoStateMachine;
    function NextLine: Boolean;
    procedure Reset;
  
    property Address: QWord read FAddress;
    property FileName: String read FFileName;
    property Line: Cardinal read FLine;
    property Column: Cardinal read FColumn;
    property IsStmt: Boolean read FIsStmt;
    property BasicBlock: Boolean read FBasicBlock;
    property EndSequence: Boolean read FEndSequence;
    property PrologueEnd: Boolean read FPrologueEnd;
    property EpilogueBegin: Boolean read FEpilogueBegin;
    property Isa: QWord read FIsa;
    
    property Ended: Boolean read FEnded;
  end;

  PDwarfAddressInfo = ^TDwarfAddressInfo;
  TDwarfAddressInfo = record
    ScopeIndex: Integer;
    ScopeList: PDwarfScopeList;
    StartPC: QWord;
    EndPC: QWord;
    StateMachine: TDwarfLineInfoStateMachine; // set if info found
    Name: PChar;
  end;

  { TDWarfLineMap }

  TDWarfLineMap = object
  private
    NextAFterHighestLine: Cardinal;
    AddressList: array of QWord;
    //Count: Integer;
  public
    procedure Init;
    procedure SetAddressForLine(ALine: Cardinal; AnAddress: QWord); inline;
    function  GetAddressForLine(ALine: Cardinal): QWord; inline;
    procedure Compress;
  end;
  PDWarfLineMap = ^TDWarfLineMap;

  { TDwarfCompilationUnit }

  TDwarfCompilationUnitClass = class of TDwarfCompilationUnit;
  TDwarfCompilationUnit = class
  private
    FOwner: TDbgDwarf;
    FVerbose: Boolean;
    FValid: Boolean; // set if the compilationunit has compile unit tag.
  
    // --- Header ---
    FLength: QWord;  // length of info
    FVersion: Word;
    FAbbrevOffset: QWord;
    FAddressSize: Byte;  // the adress size of the target in bytes
    FIsDwarf64: Boolean; // Set if the dwarf info in this unit is 64bit
    // ------
    
    FInfoData: Pointer;
    FFileName: String;
    FUnitName: String;
    FIdentifierCase: Integer;

    FAbbrevList: TDwarfAbbrevList;

    FLineInfo: record
      Header: Pointer;
      DataStart: Pointer;
      DataEnd: Pointer;

      Valid: Boolean;
      Addr64: Boolean;
      MinimumInstructionLength: Byte;
      DefaultIsStmt: Boolean;
      LineBase: ShortInt;
      LineRange: Byte;
      StandardOpcodeLengths: array of Byte; //record end; {array[1..OpcodeBase-1] of Byte}
      Directories: TStringList;
      FileNames: TStringList;
      // the line info is build incrementy when needed
      StateMachine: TDwarfLineInfoStateMachine;
      StateMachines: TFPObjectList; // list of state machines to be freed
    end;
    
    FLineNumberMap: TStringList;

    FAddressMap: TMap;
    FAddressMapBuild: Boolean;
    
    FMinPC: QWord;  // the min and max PC value found in this unit.
    FMaxPC: QWord;  //
    FScope: TDwarfScopeInfo;
    FScopeList: TDwarfScopeList;
    FScannedToEnd: Boolean;

    procedure BuildAddressMap;
    procedure BuildLineInfo(AAddressInfo: PDwarfAddressInfo; ADoAll: Boolean);
    function GetUnitName: String;
    function  ReadAddressAtPointer(var AData: Pointer; AIncPointer: Boolean = False): QWord;
  protected
    procedure ScanAllEntries; inline;
    function LocateEntry(ATag: Cardinal; out AResultScope: TDwarfScopeInfo): Boolean;
    function InitLocateAttributeList(AEntry: Pointer; var AList: TAttribPointerList): Boolean;
    function LocateAttribute(AEntry: Pointer; AAttribute: Cardinal; var AList: TAttribPointerList;
                             out AAttribPtr: Pointer; out AForm: Cardinal): Boolean;
    function LocateAttribute(AEntry: Pointer; AAttribute: Cardinal;
                             out AAttribPtr: Pointer; out AForm: Cardinal): Boolean;

    function ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: Integer): Boolean;
    function ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: Int64): Boolean;
    function ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: Cardinal): Boolean;
    function ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: QWord): Boolean;
    function ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: String): Boolean;
    function ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: PChar): Boolean;
    function ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: TByteDynArray): Boolean;

    property FirstScope: TDwarfScopeInfo read FScope;
    property AbbrevList: TDwarfAbbrevList read FAbbrevList;
  public
    constructor Create(AOwner: TDbgDwarf; ADataOffset: QWord; ALength: QWord; AVersion: Word; AAbbrevOffset: QWord; AAddressSize: Byte; AIsDwarf64: Boolean); virtual;
    destructor Destroy; override;
    function GetDefinition(AAbbrevPtr: Pointer; out ADefinition: TDwarfAbbrev): Boolean; inline;
    function GetLineAddressMap(const AFileName: String): PDWarfLineMap;
    function GetLineAddress(const AFileName: String; ALine: Cardinal): TDbgPtr;
    property FileName: String read FFileName;
    property UnitName: String read GetUnitName;
    property Valid: Boolean read FValid;
  end;
  
  { TDwarfVerboseCompilationUnit }

  TDwarfVerboseCompilationUnit = class(TDwarfCompilationUnit)
  private
  public
    constructor Create(AOwner: TDbgDwarf; ADataOffset: QWord; ALength: QWord; AVersion: Word; AAbbrevOffset: QWord; AAddressSize: Byte; AIsDwarf64: Boolean); override;
  end;


  { TDwarfAbbrevDecoder }

  TDwarfAbbrevDecoder = class(TObject)
  private
    FCU: TDwarfCompilationUnit;
    procedure InternalDecode(AData: Pointer; AMaxData: Pointer; const AIndent: String = '');
  protected
    procedure DecodeLocation(AData: PByte; ASize: QWord; const AIndent: String = '');
    procedure DecodeLocationList({%H-}AReference: QWord; const {%H-}AIndent: String = '');
    function MakeAddressString(AData: Pointer): string;
  public
    constructor Create(ACompilationUnit: TDwarfCompilationUnit);
    procedure Decode;
  end;
  
  { TDwarfStatementDecoder }

  TDwarfStatementDecoder = class(TObject)
  private
    FCU: TDwarfCompilationUnit;
    procedure InternalDecode(AData: Pointer; {%H-}AMaxData: Pointer; const {%H-}AIndent: String = '');
  protected
  public
    constructor Create(ACompilationUnit: TDwarfCompilationUnit);
    procedure Decode;
  end;
  
  { TVerboseDwarfCallframeDecoder }

  TVerboseDwarfCallframeDecoder = class(TObject)
  private
    FLoader: TDbgImageLoader;
    procedure InternalDecode(AData: Pointer; ASize, AStart: QWord);
  protected
  public
    constructor Create(ALoader: TDbgImageLoader);
    procedure Decode;
  end;

  TDbgDwarfIdentifier = class;
  TDbgDwarfTypeIdentifier = class;
  TDbgDwarfValueIdentifier = class;
  TDbgDwarfIdentifierStructure = class;
  TDbgDwarfIdentifierClass = class of TDbgDwarfIdentifier;
  TDbgDwarfValueIdentifierClass = class of TDbgDwarfValueIdentifier;
  TDbgDwarfTypeIdentifierClass = class of TDbgDwarfTypeIdentifier;

  { TDbgDwarfSymbolValue }

  TDbgDwarfSymbolValue = class(TDbgSymbolValue)
  private
    FMemReader: TFpDbgMemReaderBase;
    FOwner: TDbgDwarfValueIdentifier;
    FTypeCastInfo: TDbgDwarfTypeIdentifier;
    FTypeCastSource: TDbgSymbolValue;
  protected
    function GetFieldFlags: TDbgSymbolValueFieldFlags; override;
    function HasTypeCastInfo: Boolean;
    function IsValidTypeCast: Boolean; virtual;
    procedure DoReferenceAdded; override;
    procedure DoReferenceReleased; override;
    function GetKind: TDbgSymbolKind; override;
    function GetAddress: TDbgPtr; override;
    function GetMemberCount: Integer; override;
    function GetMemberByName(AIndex: String): TDbgSymbolValue; override;
    function GetMember(AIndex: Integer): TDbgSymbolValue; override;
    function GetDbgSymbol: TDbgSymbol; override;
    function GetTypeInfo: TDbgSymbol; override;

    property MemReader: TFpDbgMemReaderBase read FMemReader;
  public
    constructor Create(AMemReader: TFpDbgMemReaderBase);
    destructor Destroy; override;
    procedure SetOwner(AOwner: TDbgDwarfValueIdentifier);
    function SetTypeCastInfo(AStructure: TDbgDwarfTypeIdentifier;
                             ASource: TDbgSymbolValue): Boolean; // Used for Typecast
// SourceValue: TDbgSymbolValue
  end;

  { TDbgDwarfNumericSymbolValue }

  TDbgDwarfNumericSymbolValue = class(TDbgDwarfSymbolValue)
  private
    FValue: QWord;
    FIntValue: Int64;
    FSize: Integer;
    FEvaluated: set of (doneUInt, doneInt);
  protected
    function GetFieldFlags: TDbgSymbolValueFieldFlags; override;
    function CanUseTypeCastAddress: Boolean;
    function IsValidTypeCast: Boolean; override;
    function GetAsCardinal: QWord; override;
    function GetAsInteger: Int64; override;
  public
    constructor Create(AMemReader: TFpDbgMemReaderBase; ASize: Integer);
  end;

  { TDbgDwarfIntegerSymbolValue }

  TDbgDwarfIntegerSymbolValue = class(TDbgDwarfNumericSymbolValue)
  protected
    function GetFieldFlags: TDbgSymbolValueFieldFlags; override;
  end;

  { TDbgDwarfCardinalSymbolValue }

  TDbgDwarfCardinalSymbolValue = class(TDbgDwarfNumericSymbolValue)
  protected
    function GetFieldFlags: TDbgSymbolValueFieldFlags; override;
  end;

  { TDbgDwarfFloatSymbolValue }

  TDbgDwarfFloatSymbolValue = class(TDbgDwarfNumericSymbolValue) // TDbgDwarfSymbolValue
  protected
    //function GetFieldFlags: TDbgSymbolValueFieldFlags; override;
  end;

  { TDbgDwarfBooleanSymbolValue }

  TDbgDwarfBooleanSymbolValue = class(TDbgDwarfNumericSymbolValue)
  protected
    function GetFieldFlags: TDbgSymbolValueFieldFlags; override;
    function GetAsBool: Boolean; override;
  end;

  { TDbgDwarfCharSymbolValue }

  TDbgDwarfCharSymbolValue = class(TDbgDwarfNumericSymbolValue)
  protected
    // returns single char(byte) / widechar
    function GetFieldFlags: TDbgSymbolValueFieldFlags; override;
    function GetAsString: AnsiString; override;
    function GetAsWideString: WideString; override;
  end;

  { TDbgDwarfPointerSymbolValue }

  TDbgDwarfPointerSymbolValue = class(TDbgDwarfNumericSymbolValue)
  protected
    function GetFieldFlags: TDbgSymbolValueFieldFlags; override;
    function GetDataAddress: TDbgPtr; override;
  end;


  { TDbgDwarfStructSymbolValue }

  TDbgDwarfStructSymbolValue = class(TDbgDwarfSymbolValue)
  protected
    function GetFieldFlags: TDbgSymbolValueFieldFlags; override;
    function GetAsCardinal: QWord; override;
  end;

  { TDbgDwarfStructTypeCastSymbolValue }

  TDbgDwarfStructTypeCastSymbolValue = class(TDbgDwarfSymbolValue)
  private
    FMembers: TFpDbgCircularRefCntObjList;
  protected
    function GetFieldFlags: TDbgSymbolValueFieldFlags; override;
    function GetKind: TDbgSymbolKind; override;
    function GetAsCardinal: QWord; override;
    function GetDwarfDataAddress(out AnAddress: TDbgPtr; ATargetType: TDbgDwarfTypeIdentifier = nil): Boolean; reintroduce;
    function IsValidTypeCast: Boolean; override;
  public
    destructor Destroy; override;
    function GetMemberByName(AIndex: String): TDbgSymbolValue; override;
    function GetMember(AIndex: Integer): TDbgSymbolValue; override;
    function GetMemberCount: Integer; override;
  end;

  { TDbgDwarfIdentifier }

  TDbgDwarfIdentifier = class(TDbgSymbolForwarder)
  private
    FCU: TDwarfCompilationUnit;
    FInformationEntry: TDwarfInformationEntry;
//    FOwnerTypeInfo: TDbgDwarfIdentifier;
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
    property InformationEntry: TDwarfInformationEntry read FInformationEntry;

    // OwnerTypeInfo: reverse of "NestedTypeInfo" (variable that is of this type)
//    property OwnerTypeInfo: TDbgDwarfIdentifier read FOwnerTypeInfo; // write SetOwnerTypeInfo;
    // ParentTypeInfo: funtion for local var / class for member
    property ParentTypeInfo: TDbgDwarfIdentifier read FParentTypeInfo write SetParentTypeInfo;

  protected
// TODO: InitLocationParser may fail
    procedure InitLocationParser(const {%H-}ALocationParser: TDwarfLocationExpression; {%H-}AnObjectDataAddress: TDbgPtr = nil); virtual;
    function  LocationFromTag(ATag: Cardinal; out AnAddress: TDbgPtr;
                              AnObjectDataAddress: TDbgPtr = 0;
                              AnInformationEntry: TDwarfInformationEntry = nil
                             ): Boolean;
    // GetDataAddress: data of a class, or string
    function GetDataAddress(var AnAddress: TDbgPtr; ATargetType: TDbgDwarfTypeIdentifier = nil): Boolean; virtual;
    function HasAddress: Boolean; virtual;

    procedure Init; virtual;
    class function GetSubClass(ATag: Cardinal): TDbgDwarfIdentifierClass;
  public
    class function CreateSubClass(AName: String; AnInformationEntry: TDwarfInformationEntry): TDbgDwarfIdentifier;
    constructor Create(AName: String; AnInformationEntry: TDwarfInformationEntry);
    constructor Create(AName: String; AnInformationEntry: TDwarfInformationEntry;
                       AKind: TDbgSymbolKind; AAddress: TDbgPtr);
    destructor Destroy; override;
    function StartScope: TDbgPtr; // return 0, if none. 0 includes all anyway
  end;

  { TDbgDwarfValueIdentifier }

  TDbgDwarfValueIdentifier = class(TDbgDwarfIdentifier) // var, const, member, ...
  protected
    FValueObject: TDbgDwarfSymbolValue;
    FMembers: TFpDbgCircularRefCntObjList;

    function GetDataAddress(out AnAddress: TDbgPtr; ATargetType: TDbgDwarfTypeIdentifier = nil): Boolean; reintroduce;
    procedure KindNeeded; override;
    procedure MemberVisibilityNeeded; override;
    function GetMember(AIndex: Integer): TDbgSymbol; override;
    function GetMemberByName(AIndex: String): TDbgSymbol; override;
    function GetMemberCount: Integer; override;

    procedure Init; override;
  public
    destructor Destroy; override;
    class function CreateValueSubClass(AName: String; AnInformationEntry: TDwarfInformationEntry): TDbgDwarfValueIdentifier;
  end;

  { TDbgDwarfValueLocationIdentifier }

  TDbgDwarfValueLocationIdentifier = class(TDbgDwarfValueIdentifier)
  private
    procedure FrameBaseNeeded(ASender: TObject);
  protected
    function GetValueObject: TDbgSymbolValue; override;
    procedure InitLocationParser(const ALocationParser: TDwarfLocationExpression; AnObjectDataAddress: TDbgPtr = nil); override;
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
    function GetTypedValueObject(ATypeCast: Boolean): TDbgDwarfSymbolValue; virtual; // returns refcount=1 for caller, no cached copy kept
  public
    class function CreateTypeSubClass(AName: String; AnInformationEntry: TDwarfInformationEntry): TDbgDwarfTypeIdentifier;
    function TypeCastValue(AValue: TDbgSymbolValue): TDbgSymbolValue; override;
  end;

  { TDbgDwarfBaseIdentifierBase }

  TDbgDwarfBaseIdentifierBase = class(TDbgDwarfTypeIdentifier)
  //function DoGetNestedTypeInfo: TDbgDwarfTypeIdentifier; // return nil
  protected
    procedure KindNeeded; override;
    procedure SizeNeeded; override;
    procedure TypeInfoNeeded; override;
    function GetTypedValueObject(ATypeCast: Boolean): TDbgDwarfSymbolValue; override;
    function GetHasBounds: Boolean; override;
    function GetOrdHighBound: Int64; override;
    function GetOrdLowBound: Int64; override;
  end;

  { TDbgDwarfTypeIdentifierModifier }

  TDbgDwarfTypeIdentifierModifier = class(TDbgDwarfTypeIdentifier)
  protected
    procedure TypeInfoNeeded; override;
    procedure ForwardToSymbolNeeded; override;
  end;

  { TDbgDwarfTypeIdentifierRef }

  TDbgDwarfTypeIdentifierRef = class(TDbgDwarfTypeIdentifierModifier)
  protected
    function GetFlags: TDbgSymbolFlags; override;
    function GetDataAddress(var AnAddress: TDbgPtr; ATargetType: TDbgDwarfTypeIdentifier = nil): Boolean; override;
  end;

  { TDbgDwarfTypeIdentifierDeclaration }

  TDbgDwarfTypeIdentifierDeclaration = class(TDbgDwarfTypeIdentifierModifier)
  protected
    // fpc encodes classes as pointer, not ref (so Obj1 = obj2 compares the pointers)
    // typedef > pointer > srtuct
    // while a pointer to class/object: pointer > typedef > ....
    function DoGetNestedTypeInfo: TDbgDwarfTypeIdentifier; override;
    function GetTypedValueObject(ATypeCast: Boolean): TDbgDwarfSymbolValue; override;
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
    procedure ReadBounds;
  protected
    function DoGetNestedTypeInfo: TDbgDwarfTypeIdentifier;override;
    function GetHasBounds: Boolean; override;
    function GetOrdHighBound: Int64; override;
    function GetOrdLowBound: Int64; override;

    procedure NameNeeded; override;
    procedure KindNeeded; override;
    procedure SizeNeeded; override;
    function GetFlags: TDbgSymbolFlags; override;
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
    procedure ForwardToSymbolNeeded; override;
    function GetDataAddress(var AnAddress: TDbgPtr; ATargetType: TDbgDwarfTypeIdentifier = nil): Boolean; override;
    function GetTypedValueObject(ATypeCast: Boolean): TDbgDwarfSymbolValue; override;
  public
    property IsInternalPointer: Boolean read GetIsInternalPointer write FIsInternalPointer; // Class (also DynArray, but DynArray is handled without this)
  end;

  { TDbgDwarfIdentifierEnumElement }

  TDbgDwarfIdentifierEnumElement  = class(TDbgDwarfValueIdentifier)
    FOrdinalValue: Int64;
    FOrdinalValueRead, FHasOrdinalValue: Boolean;
    procedure ReadOrdinalValue;
  protected
    procedure KindNeeded; override;
    function GetHasOrdinalValue: Boolean; override;
    function GetOrdinalValue: Int64; override;
    procedure Init; override;
  end;


  { TDbgDwarfIdentifierEnum }

  TDbgDwarfIdentifierEnum = class(TDbgDwarfTypeIdentifier)
  private
    FMembers: TFpDbgCircularRefCntObjList;
    procedure CreateMembers;
  protected
    procedure KindNeeded; override;
    function GetMember(AIndex: Integer): TDbgSymbol; override;
    function GetMemberByName(AIndex: String): TDbgSymbol; override;
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
  end;

  (*
    If not specified
         .NestedTypeInfo --> copy of TypeInfo
         .ParentTypeInfo --> nil

    ParentTypeInfo:     has a weak RefCount (only AddRef, if self has other refs)
    StructureValueInfo: weak Ref (Struct has a ref to member, via parent)


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
     |-- .StructureValueInfo --> AnObject

    [*1] May have TDbgDwarfTypeIdentifierDeclaration or others
  *)

  { TDbgDwarfIdentifierMember }

  TDbgDwarfIdentifierMember = class(TDbgDwarfValueLocationIdentifier)
  private
    FStructureValueInfo: TDbgSymbolBase;
    procedure SetStructureValueInfo(AValue: TDbgSymbolBase);
  protected
    procedure InitLocationParser(const ALocationParser: TDwarfLocationExpression; AnObjectDataAddress: TDbgPtr = nil); override;
    procedure AddressNeeded; override;
    function HasAddress: Boolean; override;
    procedure CircleBackRefActiveChanged(ANewActive: Boolean); override;
    procedure SetParentTypeInfo(AValue: TDbgDwarfIdentifier); override;
  public
    destructor Destroy; override;
    property StructureValueInfo: TDbgSymbolBase read FStructureValueInfo write SetStructureValueInfo;
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
    procedure KindNeeded; override;
    procedure TypeInfoNeeded; override;                       // nil or inherited
    function GetTypedValueObject(ATypeCast: Boolean): TDbgDwarfSymbolValue; override;

    function GetMember(AIndex: Integer): TDbgSymbol; override;
    function GetMemberByName(AIndex: String): TDbgSymbol; override;
    function GetMemberCount: Integer; override;

    procedure InitLocationParser(const ALocationParser: TDwarfLocationExpression; AnObjectDataAddress: TDbgPtr = nil); override;
    function GetDataAddress(var AnAddress: TDbgPtr; ATargetType: TDbgDwarfTypeIdentifier = nil): Boolean; override;
  public
    destructor Destroy; override;
  end;

  { TDbgDwarfIdentifierArray }

  TDbgDwarfIdentifierArray = class(TDbgDwarfTypeIdentifier)
  private
    FMembers: TFpDbgCircularRefCntObjList;
    procedure CreateMembers;
  protected
    procedure KindNeeded; override;

    function GetFlags: TDbgSymbolFlags; override;
    function GetMember(AIndex: Integer): TDbgSymbol; override;
    function GetMemberByName({%H-}AIndex: String): TDbgSymbol; override;
    function GetMemberCount: Integer; override;
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
    procedure AddressNeeded; override;
    function HasAddress: Boolean; override;
  public
  end;

  { TDbgDwarfIdentifierParameter }

  TDbgDwarfIdentifierParameter = class(TDbgDwarfValueLocationIdentifier)
  protected
    procedure AddressNeeded; override;
    function HasAddress: Boolean; override;
  public
  end;

  { TDbgDwarfUnit }

  TDbgDwarfUnit = class(TDbgDwarfIdentifier)
  private
    FLastChildByName: TDbgSymbol;
  protected
    procedure Init; override;
    function GetMemberByName(AIndex: String): TDbgSymbol; override;
  public
    destructor Destroy; override;
  end;

type
  TDwarfSection = (dsAbbrev, dsARanges, dsFrame,  dsInfo, dsLine, dsLoc, dsMacinfo, dsPubNames, dsPubTypes, dsRanges, dsStr);

  TDwarfSectionInfo = record
    Section: TDwarfSection;
    VirtualAdress: QWord;
    Size: QWord; // the virtual size
    RawData: Pointer;
  end;
  PDwarfSectionInfo = ^TDwarfSectionInfo;

const
  DWARF_SECTION_NAME: array[TDwarfSection] of String = (
    '.debug_abbrev', '.debug_aranges', '.debug_frame', '.debug_info',
    '.debug_line', '.debug_loc', '.debug_macinfo', '.debug_pubnames',
    '.debug_pubtypes', '.debug_ranges', '.debug_str'
  );
  
  { TDbgDwarf }

type

  TDbgDwarf = class(TDbgInfo)
  private
    FCompilationUnits: TList;
    FImageBase: QWord;
    FMemReader: TFpDbgMemReaderBase;
    FSections: array[TDwarfSection] of TDwarfSectionInfo;
    function GetCompilationUnit(AIndex: Integer): TDwarfCompilationUnit;
  protected
    function GetCompilationUnitClass: TDwarfCompilationUnitClass; virtual;
    function FindCompilationUnitByOffs(AOffs: QWord): TDwarfCompilationUnit;
    function FindProcSymbol(AAddress: TDbgPtr): TDbgSymbol;
  public
    constructor Create(ALoader: TDbgImageLoader); override;
    destructor Destroy; override;
    function FindContext(AAddress: TDbgPtr): TDbgInfoAddressContext; override;
    function FindSymbol(AAddress: TDbgPtr): TDbgSymbol; override;
    //function FindSymbol(const AName: String): TDbgSymbol; override;
    function GetLineAddress(const AFileName: String; ALine: Cardinal): TDbgPtr; override;
    function GetLineAddressMap(const AFileName: String): PDWarfLineMap;
    function LoadCompilationUnits: Integer;
    function PointerFromRVA(ARVA: QWord): Pointer;
    function PointerFromVA(ASection: TDwarfSection; AVA: QWord): Pointer;
    function CompilationUnitsCount: Integer;
    property CompilationUnits[AIndex: Integer]: TDwarfCompilationUnit read GetCompilationUnit;
    property MemReader: TFpDbgMemReaderBase read FMemReader write FMemReader;
  end;

  { TDbgVerboseDwarf }

  TDbgVerboseDwarf = class(TDbgDwarf)
  private
  protected
    function GetCompilationUnitClass: TDwarfCompilationUnitClass; override;
  public
  end;
  
  { TDbgDwarfInfoAddressContext }

  TDbgDwarfInfoAddressContext = class(TDbgInfoAddressContext)
  private
    FSymbol: TDbgSymbol;
    FAddress: TDbgPtr;
    FDwarf: TDbgDwarf;
  protected
    function GetSymbolAtAddress: TDbgSymbol; override;
    function GetAddress: TDbgPtr; override;
  public
    constructor Create(AnAddress: TDbgPtr; ASymbol: TDbgSymbol; ADwarf: TDbgDwarf);
    destructor Destroy; override;
    function FindSymbol(const AName: String): TDbgSymbol; override;
  end;


function DwarfTagToString(AValue: Integer): String;
function DwarfAttributeToString(AValue: Integer): String;
function DwarfAttributeFormToString(AValue: Integer): String;

function ULEB128toOrdinal(var p: PByte): QWord;
function SLEB128toOrdinal(var p: PByte): Int64;

function Dbgs(AInfoData: Pointer; ACompUnit: TDwarfCompilationUnit): String; overload;
function Dbgs(AScope: TDwarfScopeInfo; ACompUnit: TDwarfCompilationUnit): String; overload;
function Dbgs(AInfoEntry: TDwarfInformationEntry; ACompUnit: TDwarfCompilationUnit): String; overload;
function DbgsDump(AScope: TDwarfScopeInfo; ACompUnit: TDwarfCompilationUnit): String; overload;

implementation

var
  FPDBG_DWARF_WARNINGS, FPDBG_DWARF_SEARCH, FPDBG_DWARF_VERBOSE: PLazLoggerLogGroup;

const
  SCOPE_ALLOC_BLOCK_SIZE = 4096; // Increase scopelist in steps of

function Dbgs(AInfoData: Pointer; ACompUnit: TDwarfCompilationUnit): String;
var
  Attrib: Pointer;
  Form: Cardinal;
  Name: String;
  Def: TDwarfAbbrev;
begin
  Result := '';

  if ACompUnit.LocateAttribute(AInfoData, DW_AT_name, Attrib, Form) then
    if (Form = DW_FORM_string) or (Form = DW_FORM_strp) then
      ACompUnit.ReadValue(Attrib, Form, Name);

  if ACompUnit.GetDefinition(AInfoData, Def) then
    Result := Format('Tag=%s Name=%s', [DwarfTagToString(Def.tag), Name])
  else
    Result := Format('Name=%s', [Name]);
end;

function dbgs(AScope: TDwarfScopeInfo; ACompUnit: TDwarfCompilationUnit): String;
begin
  if not AScope.IsValid then
    exit('Invalid-Scope');
  Result := Format('AScope(Idx=%d %s)', [AScope.Index, dbgs(AScope.Entry, ACompUnit)]);
end;

function Dbgs(AInfoEntry: TDwarfInformationEntry; ACompUnit: TDwarfCompilationUnit): String;
begin
  if AInfoEntry.HasValidScope
  then Result := Dbgs(AInfoEntry.FScope, ACompUnit)
  else Result := Dbgs(AInfoEntry.FInformationEntry, ACompUnit);
end;

function DbgsDump(AScope: TDwarfScopeInfo; ACompUnit: TDwarfCompilationUnit): String;
var
  Def: TDwarfAbbrev;
  i: Integer;
begin
  Result := '';
  if not AScope.IsValid then
    exit('Invalid-Scope');

  if ACompUnit.GetDefinition(AScope.Entry, Def) then begin
  Result := LineEnding;
    for i := Def.index to Def.index + Def.count - 1 do begin
      Result := Result +
        DwarfAttributeToString(ACompUnit.FAbbrevList.EntryPointer[i]^.Attribute) + ' ' +
        DwarfAttributeFormToString(ACompUnit.FAbbrevList.EntryPointer[i]^.Form) +
        LineEnding;
    end;
  end;
end;

function ULEB128toOrdinal(var p: PByte): QWord;
var
  n: Byte;
  Stop: Boolean;
begin
  Result := 0;
  n := 0;
  repeat
    Stop := (p^ and $80) = 0;
    Result := Result + (p^ and $7F) shl n;
    Inc(n, 7);
    Inc(p);
  until Stop or (n > 128);
end;

function SLEB128toOrdinal(var p: PByte): Int64;
var
  n: Byte;
  Stop: Boolean;
begin
  Result := 0;
  n := 0;
  repeat
    Stop := (p^ and $80) = 0;
    Result := Result + (p^ and $7F) shl n;
    Inc(n, 7);
    Inc(p);
  until Stop or (n > 128);

  // sign extend when msbit = 1
  if (p[-1] and $40) <> 0
  then Result := Result or (Int64(-1) shl n);
end;

function SkipEntryDataForForm(var AEntryData: Pointer; AForm: Cardinal; AddrSize: Byte): Boolean; inline;
var
  UValue: QWord;
begin
  Result := True;
  case AForm of
    DW_FORM_addr     : Inc(AEntryData, AddrSize);
    DW_FORM_block    : begin
        UValue := ULEB128toOrdinal(AEntryData);
        Inc(AEntryData, UValue);
      end;
    DW_FORM_block1   : Inc(AEntryData, PByte(AEntryData)^ + 1);
    DW_FORM_block2   : Inc(AEntryData, PWord(AEntryData)^ + 2);
    DW_FORM_block4   : Inc(AEntryData, PLongWord(AEntryData)^ + 4);
    DW_FORM_data1    : Inc(AEntryData, 1);
    DW_FORM_data2    : Inc(AEntryData, 2);
    DW_FORM_data4    : Inc(AEntryData, 4);
    DW_FORM_data8    : Inc(AEntryData, 8);
    DW_FORM_sdata    : begin
        while (PByte(AEntryData)^ and $80) <> 0 do Inc(AEntryData);
        Inc(AEntryData);
      end;
    DW_FORM_udata    : begin
        while (PByte(AEntryData)^ and $80) <> 0 do Inc(AEntryData);
        Inc(AEntryData);
      end;
    DW_FORM_flag     : Inc(AEntryData, 1);
    DW_FORM_ref1     : Inc(AEntryData, 1);
    DW_FORM_ref2     : Inc(AEntryData, 2);
    DW_FORM_ref4     : Inc(AEntryData, 4);
    DW_FORM_ref8     : Inc(AEntryData, 8);
    DW_FORM_ref_udata: begin
        while (PByte(AEntryData)^ and $80) <> 0 do Inc(AEntryData);
        Inc(AEntryData);
      end;
    DW_FORM_ref_addr : Inc(AEntryData, AddrSize); // TODO: Dwarf3 depends on FIsDwarf64
    DW_FORM_string   : begin
        while PByte(AEntryData)^ <> 0 do Inc(AEntryData);
        Inc(AEntryData);
      end;
    DW_FORM_strp     : Inc(AEntryData, AddrSize);
    DW_FORM_indirect : begin
        while AForm = DW_FORM_indirect do AForm := ULEB128toOrdinal(AEntryData);
        Result := SkipEntryDataForForm(AEntryData, AForm, AddrSize);
      end;
  else begin
      DebugLn(FPDBG_DWARF_WARNINGS, ['Error: Unknown Form: ', AForm]);
      Result := False;
    end;
  end;

end;

function DwarfTagToString(AValue: Integer): String;
begin
  case AValue of
    DW_TAG_array_type              : Result := 'DW_TAG_array_type';
    DW_TAG_class_type              : Result := 'DW_TAG_class_type';
    DW_TAG_entry_point             : Result := 'DW_TAG_entry_point';
    DW_TAG_enumeration_type        : Result := 'DW_TAG_enumeration_type';
    DW_TAG_formal_parameter        : Result := 'DW_TAG_formal_parameter';
    DW_TAG_imported_declaration    : Result := 'DW_TAG_imported_declaration';
    DW_TAG_label                   : Result := 'DW_TAG_label';
    DW_TAG_lexical_block           : Result := 'DW_TAG_lexical_block';
    DW_TAG_member                  : Result := 'DW_TAG_member';
    DW_TAG_pointer_type            : Result := 'DW_TAG_pointer_type';
    DW_TAG_reference_type          : Result := 'DW_TAG_reference_type';
    DW_TAG_compile_unit            : Result := 'DW_TAG_compile_unit';
    DW_TAG_string_type             : Result := 'DW_TAG_string_type';
    DW_TAG_structure_type          : Result := 'DW_TAG_structure_type';
    DW_TAG_subroutine_type         : Result := 'DW_TAG_subroutine_type';
    DW_TAG_typedef                 : Result := 'DW_TAG_typedef';
    DW_TAG_union_type              : Result := 'DW_TAG_union_type';
    DW_TAG_unspecified_parameters  : Result := 'DW_TAG_unspecified_parameters';
    DW_TAG_variant                 : Result := 'DW_TAG_variant';
    DW_TAG_common_block            : Result := 'DW_TAG_common_block';
    DW_TAG_common_inclusion        : Result := 'DW_TAG_common_inclusion';
    DW_TAG_inheritance             : Result := 'DW_TAG_inheritance';
    DW_TAG_inlined_subroutine      : Result := 'DW_TAG_inlined_subroutine';
    DW_TAG_module                  : Result := 'DW_TAG_module';
    DW_TAG_ptr_to_member_type      : Result := 'DW_TAG_ptr_to_member_type';
    DW_TAG_set_type                : Result := 'DW_TAG_set_type';
    DW_TAG_subrange_type           : Result := 'DW_TAG_subrange_type';
    DW_TAG_with_stmt               : Result := 'DW_TAG_with_stmt';
    DW_TAG_access_declaration      : Result := 'DW_TAG_access_declaration';
    DW_TAG_base_type               : Result := 'DW_TAG_base_type';
    DW_TAG_catch_block             : Result := 'DW_TAG_catch_block';
    DW_TAG_const_type              : Result := 'DW_TAG_const_type';
    DW_TAG_constant                : Result := 'DW_TAG_constant';
    DW_TAG_enumerator              : Result := 'DW_TAG_enumerator';
    DW_TAG_file_type               : Result := 'DW_TAG_file_type';
    DW_TAG_friend                  : Result := 'DW_TAG_friend';
    DW_TAG_namelist                : Result := 'DW_TAG_namelist';
    DW_TAG_namelist_item           : Result := 'DW_TAG_namelist_item';
    DW_TAG_packed_type             : Result := 'DW_TAG_packed_type';
    DW_TAG_subprogram              : Result := 'DW_TAG_subprogram';
    DW_TAG_template_type_parameter : Result := 'DW_TAG_template_type_parameter';
    DW_TAG_template_value_parameter: Result := 'DW_TAG_template_value_parameter';
    DW_TAG_thrown_type             : Result := 'DW_TAG_thrown_type';
    DW_TAG_try_block               : Result := 'DW_TAG_try_block';
    DW_TAG_variant_part            : Result := 'DW_TAG_variant_part';
    DW_TAG_variable                : Result := 'DW_TAG_variable';
    DW_TAG_volatile_type           : Result := 'DW_TAG_volatile_type';
    DW_TAG_dwarf_procedure         : Result := 'DW_TAG_dwarf_procedure';
    DW_TAG_restrict_type           : Result := 'DW_TAG_restrict_type';
    DW_TAG_interface_type          : Result := 'DW_TAG_interface_type';
    DW_TAG_namespace               : Result := 'DW_TAG_namespace';
    DW_TAG_imported_module         : Result := 'DW_TAG_imported_module';
    DW_TAG_unspecified_type        : Result := 'DW_TAG_unspecified_type';
    DW_TAG_partial_unit            : Result := 'DW_TAG_partial_unit';
    DW_TAG_imported_unit           : Result := 'DW_TAG_imported_unit';
    DW_TAG_condition               : Result := 'DW_TAG_condition';
    DW_TAG_shared_type             : Result := 'DW_TAG_shared_type';
    DW_TAG_lo_user                 : Result := 'DW_TAG_lo_user';
    DW_TAG_hi_user                 : Result := 'DW_TAG_hi_user';
  else
    Result := Format('DW_TAG_%d', [AValue]);
  end;
end;

function DwarfChildrenToString(AValue: Integer): String;
begin
  case AValue of
    DW_CHILDREN_no  : Result := 'DW_CHILDREN_no';
    DW_CHILDREN_yes : Result := 'DW_CHILDREN_yes';
  else
    Result := Format('DW_CHILDREN_%d', [AValue]);
  end;
end;


function DwarfAttributeToString(AValue: Integer): String;
begin
  case AValue of
    DW_AT_sibling             : Result := 'DW_AT_sibling';
    DW_AT_location            : Result := 'DW_AT_location';
    DW_AT_name                : Result := 'DW_AT_name';
    DW_AT_ordering            : Result := 'DW_AT_ordering';
    DW_AT_byte_size           : Result := 'DW_AT_byte_size';
    DW_AT_bit_offset          : Result := 'DW_AT_bit_offset';
    DW_AT_bit_size            : Result := 'DW_AT_bit_size';
    DW_AT_stmt_list           : Result := 'DW_AT_stmt_list';
    DW_AT_low_pc              : Result := 'DW_AT_low_pc';
    DW_AT_high_pc             : Result := 'DW_AT_high_pc';
    DW_AT_language            : Result := 'DW_AT_language';
    DW_AT_discr               : Result := 'DW_AT_discr';
    DW_AT_discr_value         : Result := 'DW_AT_discr_value';
    DW_AT_visibility          : Result := 'DW_AT_visibility';
    DW_AT_import              : Result := 'DW_AT_import';
    DW_AT_string_length       : Result := 'DW_AT_string_length';
    DW_AT_common_reference    : Result := 'DW_AT_common_reference';
    DW_AT_comp_dir            : Result := 'DW_AT_comp_dir';
    DW_AT_const_value         : Result := 'DW_AT_const_value';
    DW_AT_containing_type     : Result := 'DW_AT_containing_type';
    DW_AT_default_value       : Result := 'DW_AT_default_value';
    DW_AT_inline              : Result := 'DW_AT_inline';
    DW_AT_is_optional         : Result := 'DW_AT_is_optional';
    DW_AT_lower_bound         : Result := 'DW_AT_lower_bound';
    DW_AT_producer            : Result := 'DW_AT_producer';
    DW_AT_prototyped          : Result := 'DW_AT_prototyped';
    DW_AT_return_addr         : Result := 'DW_AT_return_addr';
    DW_AT_start_scope         : Result := 'DW_AT_start_scope';
    DW_AT_bit_stride          : Result := 'DW_AT_bit_stride';
    DW_AT_upper_bound         : Result := 'DW_AT_upper_bound';
    DW_AT_abstract_origin     : Result := 'DW_AT_abstract_origin';
    DW_AT_accessibility       : Result := 'DW_AT_accessibility';
    DW_AT_address_class       : Result := 'DW_AT_address_class';
    DW_AT_artificial          : Result := 'DW_AT_artificial';
    DW_AT_base_types          : Result := 'DW_AT_base_types';
    DW_AT_calling_convention  : Result := 'DW_AT_calling_convention';
    DW_AT_count               : Result := 'DW_AT_count';
    DW_AT_data_member_location: Result := 'DW_AT_data_member_location';
    DW_AT_decl_column         : Result := 'DW_AT_decl_column';
    DW_AT_decl_file           : Result := 'DW_AT_decl_file';
    DW_AT_decl_line           : Result := 'DW_AT_decl_line';
    DW_AT_declaration         : Result := 'DW_AT_declaration';
    DW_AT_discr_list          : Result := 'DW_AT_discr_list';
    DW_AT_encoding            : Result := 'DW_AT_encoding';
    DW_AT_external            : Result := 'DW_AT_external';
    DW_AT_frame_base          : Result := 'DW_AT_frame_base';
    DW_AT_friend              : Result := 'DW_AT_friend';
    DW_AT_identifier_case     : Result := 'DW_AT_identifier_case';
    DW_AT_macro_info          : Result := 'DW_AT_macro_info';
    DW_AT_namelist_item       : Result := 'DW_AT_namelist_item';
    DW_AT_priority            : Result := 'DW_AT_priority';
    DW_AT_segment             : Result := 'DW_AT_segment';
    DW_AT_specification       : Result := 'DW_AT_specification';
    DW_AT_static_link         : Result := 'DW_AT_static_link';
    DW_AT_type                : Result := 'DW_AT_type';
    DW_AT_use_location        : Result := 'DW_AT_use_location';
    DW_AT_variable_parameter  : Result := 'DW_AT_variable_parameter';
    DW_AT_virtuality          : Result := 'DW_AT_virtuality';
    DW_AT_vtable_elem_location: Result := 'DW_AT_vtable_elem_location';
    DW_AT_allocated           : Result := 'DW_AT_allocated';
    DW_AT_associated          : Result := 'DW_AT_associated';
    DW_AT_data_location       : Result := 'DW_AT_data_location';
    DW_AT_byte_stride         : Result := 'DW_AT_byte_stride';
    DW_AT_entry_pc            : Result := 'DW_AT_entry_pc';
    DW_AT_use_UTF8            : Result := 'DW_AT_use_UTF8';
    DW_AT_extension           : Result := 'DW_AT_extension';
    DW_AT_ranges              : Result := 'DW_AT_ranges';
    DW_AT_trampoline          : Result := 'DW_AT_trampoline';
    DW_AT_call_column         : Result := 'DW_AT_call_column';
    DW_AT_call_file           : Result := 'DW_AT_call_file';
    DW_AT_call_line           : Result := 'DW_AT_call_line';
    DW_AT_description         : Result := 'DW_AT_description';
    DW_AT_binary_scale        : Result := 'DW_AT_binary_scale';
    DW_AT_decimal_scale       : Result := 'DW_AT_decimal_scale';
    DW_AT_small               : Result := 'DW_AT_small';
    DW_AT_decimal_sign        : Result := 'DW_AT_decimal_sign';
    DW_AT_digit_count         : Result := 'DW_AT_digit_count';
    DW_AT_picture_string      : Result := 'DW_AT_picture_string';
    DW_AT_mutable             : Result := 'DW_AT_mutable';
    DW_AT_threads_scaled      : Result := 'DW_AT_threads_scaled';
    DW_AT_explicit            : Result := 'DW_AT_explicit';
    DW_AT_object_pointer      : Result := 'DW_AT_object_pointer';
    DW_AT_endianity           : Result := 'DW_AT_endianity';
    DW_AT_elemental           : Result := 'DW_AT_elemental';
    DW_AT_pure                : Result := 'DW_AT_pure';
    DW_AT_recursive           : Result := 'DW_AT_recursive';
    DW_AT_lo_user             : Result := 'DW_AT_lo_user';
    DW_AT_hi_user             : Result := 'DW_AT_hi_user';
  else
    Result := Format('DW_AT_%d', [AValue]);
  end;
end;

function DwarfAttributeFormToString(AValue: Integer): String;
begin
  case AValue of
    DW_FORM_addr     : Result := 'DW_FORM_addr';
    DW_FORM_block2   : Result := 'DW_FORM_block2';
    DW_FORM_block4   : Result := 'DW_FORM_block4';
    DW_FORM_data2    : Result := 'DW_FORM_data2';
    DW_FORM_data4    : Result := 'DW_FORM_data4';
    DW_FORM_data8    : Result := 'DW_FORM_data8';
    DW_FORM_string   : Result := 'DW_FORM_string';
    DW_FORM_block    : Result := 'DW_FORM_block';
    DW_FORM_block1   : Result := 'DW_FORM_block1';
    DW_FORM_data1    : Result := 'DW_FORM_data1';
    DW_FORM_flag     : Result := 'DW_FORM_flag';
    DW_FORM_sdata    : Result := 'DW_FORM_sdata';
    DW_FORM_strp     : Result := 'DW_FORM_strp';
    DW_FORM_udata    : Result := 'DW_FORM_udata';
    DW_FORM_ref_addr : Result := 'DW_FORM_ref_addr';
    DW_FORM_ref1     : Result := 'DW_FORM_ref1';
    DW_FORM_ref2     : Result := 'DW_FORM_ref2';
    DW_FORM_ref4     : Result := 'DW_FORM_ref4';
    DW_FORM_ref8     : Result := 'DW_FORM_ref8';
    DW_FORM_ref_udata: Result := 'DW_FORM_ref_udata';
    DW_FORM_indirect : Result := 'DW_FORM_indirect';
  else
    Result := Format('DW_FORM_%d', [AValue]);
  end;
end;

function DwarfLanguageToString(AValue: Integer): String;
begin
  case AValue of
    DW_LANG_C89              : Result := 'DW_LANG_C89';
    DW_LANG_C                : Result := 'DW_LANG_C';
    DW_LANG_Ada83            : Result := 'DW_LANG_Ada83 (reserved)';
    DW_LANG_C_plus_plus      : Result := 'DW_LANG_C_plus_plus';
    DW_LANG_Cobol74          : Result := 'DW_LANG_Cobol74 (reserved)';
    DW_LANG_Cobol85          : Result := 'DW_LANG_Cobol85 (reserved)';
    DW_LANG_Fortran77        : Result := 'DW_LANG_Fortran77';
    DW_LANG_Fortran90        : Result := 'DW_LANG_Fortran90';
    DW_LANG_Pascal83         : Result := 'DW_LANG_Pascal83';
    DW_LANG_Modula2          : Result := 'DW_LANG_Modula2';
    DW_LANG_Java             : Result := 'DW_LANG_Java';
    DW_LANG_C99              : Result := 'DW_LANG_C99';
    DW_LANG_Ada95            : Result := 'DW_LANG_Ada95 (reserved)';
    DW_LANG_Fortran95        : Result := 'DW_LANG_Fortran95';
    DW_LANG_PLI              : Result := 'DW_LANG_PLI (reserved)';
    DW_LANG_ObjC             : Result := 'DW_LANG_ObjC';
    DW_LANG_ObjC_plus_plus   : Result := 'DW_LANG_ObjC_plus_plus';
    DW_LANG_UPC              : Result := 'DW_LANG_UPC';
    DW_LANG_D                : Result := 'DW_LANG_D';
    DW_LANG_lo_user..DW_LANG_hi_user: Result := Format('DW_LANG_user_%d', [AValue]);
  else
    Result := Format('DW_LANG_%d', [AValue]);
  end;
end;


function DwarfBaseTypeEncodingToString(AValue: Integer): String;
begin
  case AValue of
    DW_ATE_address           : Result := 'DW_ATE_address';
    DW_ATE_boolean           : Result := 'DW_ATE_boolean';
    DW_ATE_complex_float     : Result := 'DW_ATE_complex_float';
    DW_ATE_float             : Result := 'DW_ATE_float';
    DW_ATE_signed            : Result := 'DW_ATE_signed';
    DW_ATE_signed_char       : Result := 'DW_ATE_signed_char';
    DW_ATE_unsigned          : Result := 'DW_ATE_unsigned';
    DW_ATE_unsigned_char     : Result := 'DW_ATE_unsigned_char';
    DW_ATE_imaginary_float   : Result := 'DW_ATE_imaginary_float';
    DW_ATE_packed_decimal    : Result := 'DW_ATE_packed_decimal';
    DW_ATE_numeric_string    : Result := 'DW_ATE_numeric_string';
    DW_ATE_edited            : Result := 'DW_ATE_edited';
    DW_ATE_signed_fixed      : Result := 'DW_ATE_signed_fixed';
    DW_ATE_unsigned_fixed    : Result := 'DW_ATE_unsigned_fixed';
    DW_ATE_decimal_float     : Result := 'DW_ATE_decimal_float';
    DW_ATE_lo_user..DW_ATE_hi_user : Result := Format('DW_ATE_user_%d', [AValue]);
  else
    Result := Format('DW_ATE_%d', [AValue]);
  end;
end;

function DwarfAccessibilityToString(AValue: Integer): String;
begin
  case AValue of
    DW_ACCESS_public    : Result := 'DW_ACCESS_public';
    DW_ACCESS_protected : Result := 'DW_ACCESS_protected';
    DW_ACCESS_private   : Result := 'DW_ACCESS_private';
  else
    Result := Format('DW_ACCESS_%d', [AValue]);
  end;
end;


function DwarfVisibilityToString(AValue: Integer): String;
begin
  case AValue of
    DW_VIS_local     : Result := 'DW_VIS_local';
    DW_VIS_exported  : Result := 'DW_VIS_exported';
    DW_VIS_qualified : Result := 'DW_VIS_qualified';
  else
    Result := Format('DW_FORM_%d', [AValue]);
  end;
end;


function DwarfVirtualityToString(AValue: Integer): String;
begin
  case AValue of
    DW_VIRTUALITY_none         : Result := 'DW_VIRTUALITY_none';
    DW_VIRTUALITY_virtual      : Result := 'DW_VIRTUALITY_virtual';
    DW_VIRTUALITY_pure_virtual : Result := 'DW_VIRTUALITY_pure_virtual';
  else
    Result := Format('DW_VIRTUALITY_%d', [AValue]);
  end;
end;

  { Identifier case encodings }

function DwarfIdentifierCaseToString(AValue: Integer): String;
begin
  case AValue of
    DW_ID_case_sensitive   : Result := 'DW_ID_case_sensitive';
    DW_ID_up_case          : Result := 'DW_ID_up_case';
    DW_ID_down_case        : Result := 'DW_ID_down_case';
    DW_ID_case_insensitive : Result := 'DW_ID_case_insensitive';
  else
    Result := Format('DW_ID_%d', [AValue]);
  end;
end;

{ TDbgDwarfPointerSymbolValue }

function TDbgDwarfPointerSymbolValue.GetFieldFlags: TDbgSymbolValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfOrdinal, svfSizeOfPointer, svfDataAddress] - [svfSize]; // data address
end;

function TDbgDwarfPointerSymbolValue.GetDataAddress: TDbgPtr;
begin
  Result := GetAsCardinal;
end;

{ TDbgDwarfIntegerSymbolValue }

function TDbgDwarfIntegerSymbolValue.GetFieldFlags: TDbgSymbolValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfInteger];
end;

{ TDbgDwarfCardinalSymbolValue }

function TDbgDwarfCardinalSymbolValue.GetFieldFlags: TDbgSymbolValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfCardinal];
end;

{ TDbgDwarfStructSymbolValue }

function TDbgDwarfStructSymbolValue.GetFieldFlags: TDbgSymbolValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfMembers, svfSizeOfPointer]; // svfDataSize
  if (FOwner <> nil) and FOwner.HasAddress then Result := Result + [svfOrdinal];
end;

function TDbgDwarfStructSymbolValue.GetAsCardinal: QWord;
begin
  if FOwner <> nil then begin
    Result := FOwner.Address;
    FMemReader.ReadMemory(Result, SizeOf(Result), @Result);
  end
  else
    Result := inherited GetAsCardinal;
end;

{ TDbgDwarfStructSymbolValue }

function TDbgDwarfStructTypeCastSymbolValue.GetFieldFlags: TDbgSymbolValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfMembers, svfSizeOfPointer]; // svfDataSize
end;

function TDbgDwarfStructTypeCastSymbolValue.GetKind: TDbgSymbolKind;
begin
  if HasTypeCastInfo then
    Result := FTypeCastInfo.Kind
  else
    Result := inherited GetKind;
end;

function TDbgDwarfStructTypeCastSymbolValue.GetAsCardinal: QWord;
var
  fields: TDbgSymbolValueFieldFlags;
begin
  if HasTypeCastInfo then begin
    fields := FTypeCastSource.FieldFlags;
    if svfOrdinal in fields then
      Result := FTypeCastSource.AsCardinal
    else
    if svfAddress in fields then begin
      Result := FTypeCastSource.Address;
      FMemReader.ReadMemory(Result, SizeOf(Result), @Result);
    end;
   end
  else
    Result := inherited GetAsCardinal;
end;

function TDbgDwarfStructTypeCastSymbolValue.GetDwarfDataAddress(out AnAddress: TDbgPtr;
  ATargetType: TDbgDwarfTypeIdentifier): Boolean;
var
  fields: TDbgSymbolValueFieldFlags;
  t: TDbgDwarfTypeIdentifier;
begin
  Result := HasTypeCastInfo;
  if not Result then
    exit;
  fields := FTypeCastSource.FieldFlags;
  AnAddress := 0;
  if svfOrdinal in fields then begin
    AnAddress := FTypeCastSource.AsCardinal;
// MUST store, and provide address of it // for now, skip the pointer
    t := FTypeCastInfo;
    if t is TDbgDwarfTypeIdentifierDeclaration then t := t.NestedTypeInfo;
    if (t<> nil) and (t is TDbgDwarfTypeIdentifierPointer)  then t := t.NestedTypeInfo;
    if (t<> nil) then begin
      Result := t.GetDataAddress(AnAddress, ATargetType);
      Result := AnAddress <> 0;
      exit;
    end;
    Result := False;
    exit;

  end
  else
  if svfAddress in fields then
    AnAddress := FTypeCastSource.Address;

  Result := AnAddress <> 0;
  if not Result then
    exit;

  DebugLnEnter(['>>> TDbgDwarfStructSymbolValue.GetDataAddress ', IntToHex(AnAddress,8)]);
  Result := FTypeCastInfo.GetDataAddress(AnAddress, ATargetType);
  DebugLnExit(['<<< TDbgDwarfStructSymbolValue.GetDataAddress ']);
end;

function TDbgDwarfStructTypeCastSymbolValue.IsValidTypeCast: Boolean;
begin
  Result := HasTypeCastInfo and
            (FTypeCastSource.FieldFlags * [svfOrdinal, svfAddress] <> []);
end;

destructor TDbgDwarfStructTypeCastSymbolValue.Destroy;
var
  i: Integer;
begin
  if FMembers <> nil then
    for i := 0 to FMembers.Count - 1 do
      TDbgDwarfIdentifierMember(FMembers[i]).StructureValueInfo := nil;
  FreeAndNil(FMembers);
  inherited Destroy;
end;

function TDbgDwarfStructTypeCastSymbolValue.GetMemberByName(AIndex: String): TDbgSymbolValue;
var
  tmp: TDbgSymbol;
begin
  Result := nil;
  if not HasTypeCastInfo then
    exit;

  tmp := FTypeCastInfo.MemberByName[AIndex];
  if (tmp <> nil) then begin
    assert(tmp is TDbgDwarfIdentifierMember, 'TDbgDwarfStructTypeCastSymbolValue.GetMemberByName'+DbgSName(tmp));
    if FMembers = nil then
      FMembers := TFpDbgCircularRefCntObjList.Create;
    FMembers.Add(tmp);

    TDbgDwarfIdentifierMember(tmp).StructureValueInfo := Self;

    Result := tmp.Value;
  end;
end;

function TDbgDwarfStructTypeCastSymbolValue.GetMember(AIndex: Integer): TDbgSymbolValue;
var
  tmp: TDbgSymbol;
begin
  Result := nil;
  if not HasTypeCastInfo then
    exit;

  tmp := FTypeCastInfo.Member[AIndex];
  if (tmp <> nil) then begin
    assert(tmp is TDbgDwarfIdentifierMember);
    if FMembers = nil then
      FMembers := TFpDbgCircularRefCntObjList.Create;
    FMembers.Add(tmp);

    TDbgDwarfIdentifierMember(tmp).StructureValueInfo := Self;

    Result := tmp.Value;
  end;
end;

function TDbgDwarfStructTypeCastSymbolValue.GetMemberCount: Integer;
begin
  Result := 0;
  if not HasTypeCastInfo then
    exit;

  Result := FTypeCastInfo.MemberCount;
end;

{ TDbgDwarfBooleanSymbolValue }

function TDbgDwarfBooleanSymbolValue.GetFieldFlags: TDbgSymbolValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfBoolean];
end;

function TDbgDwarfBooleanSymbolValue.GetAsBool: Boolean;
begin
  Result := QWord(GetAsInteger) <> 0;
end;

{ TDbgDwarfCharSymbolValue }

function TDbgDwarfCharSymbolValue.GetFieldFlags: TDbgSymbolValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  case FSize of
    1: Result := Result + [svfString];
    2: Result := Result + [svfWideString];
  end;
end;

function TDbgDwarfCharSymbolValue.GetAsString: AnsiString;
begin
  if FSize <> 1 then
    Result := inherited GetAsString
  else
    Result := char(byte(GetAsCardinal));
end;

function TDbgDwarfCharSymbolValue.GetAsWideString: WideString;
begin
  if FSize > 2 then
    Result := inherited GetAsString
  else
    Result := WideChar(Word(GetAsCardinal));
end;

{ TDbgDwarfCardinalSymbolValue }

function TDbgDwarfNumericSymbolValue.GetFieldFlags: TDbgSymbolValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  Result := Result + [svfSize, svfOrdinal];
end;

function TDbgDwarfNumericSymbolValue.CanUseTypeCastAddress: Boolean;
begin
  Result := True;
  if (FTypeCastSource.FieldFlags * [svfAddress, svfSize, svfSizeOfPointer] = [svfAddress]) then
    exit
  else
  if (FTypeCastSource.FieldFlags * [svfAddress, svfSize] = [svfAddress, svfSize]) and
     (FTypeCastSource.Size = FSize)
  then
    exit;
  if (FTypeCastSource.FieldFlags * [svfAddress, svfSizeOfPointer] = [svfAddress, svfSizeOfPointer]) and
     not ( (FTypeCastInfo.Kind = skPointer) //or
           //(FSize = AddressSize xxxxxxx)
         )
  then
    exit;
  Result := False;
end;

function TDbgDwarfNumericSymbolValue.IsValidTypeCast: Boolean;
begin
  Result := HasTypeCastInfo;
  If not Result then
    exit;
  if (svfOrdinal in FTypeCastSource.FieldFlags) or CanUseTypeCastAddress then
    exit;
end;

function TDbgDwarfNumericSymbolValue.GetAsCardinal: QWord;
var
  addr: TDbgPtr;
begin
  // TODO: memory representation of values is not dwarf, but platform - move
  if doneUInt in FEvaluated then begin
    Result := FValue;
    exit;
  end;
  Include(FEvaluated, doneUInt);

  if (FSize <= 0) or (FSize > SizeOf(Result)) then begin
    Result := inherited GetAsInteger;
  end

  else
  if HasTypeCastInfo and (svfOrdinal in FTypeCastSource.FieldFlags) then begin
    Result := FTypeCastSource.AsCardinal;
    Result := Result and (QWord(-1) shr ((SizeOf(Result)-FSize) * 8));
  end

  else
  if ( (FOwner <> nil) or
       (HasTypeCastInfo and CanUseTypeCastAddress)
     ) and (FMemReader <> nil)
  then begin
    if FOwner <> nil then
      addr := FOwner.Address
    else
      addr := FTypeCastSource.Address;
    if (addr = 0) then begin
      Result := inherited GetAsInteger;
      FValue := Result;
      exit;
    end;

    // TODO endian
    Result := 0;
    FMemReader.ReadMemory(addr, FSize, @Result);
  end
  else
    Result := inherited GetAsInteger;

  FValue := Result;
end;

function TDbgDwarfNumericSymbolValue.GetAsInteger: Int64;
begin
  if doneInt in FEvaluated then begin
    Result := FIntValue;
    exit;
  end;
  Include(FEvaluated, doneInt);

  Result := Int64(GetAsCardinal);
  // sign extend
  if Result and (int64(1) shl (FSize * 8 - 1)) <> 0 then
    Result := Result or (int64(-1) shl (FSize * 8));

  FIntValue := Result;
end;

constructor TDbgDwarfNumericSymbolValue.Create(AMemReader: TFpDbgMemReaderBase;
  ASize: Integer);
begin
  inherited Create(AMemReader);
  FSize := ASize;
  FEvaluated := [];
end;

{ TDbgDwarfSymbolValue }

function TDbgDwarfSymbolValue.GetFieldFlags: TDbgSymbolValueFieldFlags;
begin
  Result := inherited GetFieldFlags;
  if FOwner <> nil then begin
    if FOwner.HasAddress then Result := Result + [svfAddress];
  end
  else
  if HasTypeCastInfo then begin
    Result := Result + FTypeCastSource.FieldFlags * [svfAddress];
  end;
end;

function TDbgDwarfSymbolValue.HasTypeCastInfo: Boolean;
begin
  Result := (FTypeCastInfo <> nil) and (FTypeCastSource <> nil);
end;

function TDbgDwarfSymbolValue.IsValidTypeCast: Boolean;
begin
  Result := False;
end;

procedure TDbgDwarfSymbolValue.DoReferenceAdded;
begin
  inherited DoReferenceAdded;
  if (FOwner <> nil) and (RefCount = 2) then
    FOwner.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FOwner, 'TDbgDwarfSymbolValue'){$ENDIF};
end;

procedure TDbgDwarfSymbolValue.DoReferenceReleased;
begin
  inherited DoReferenceReleased;
  if (FOwner <> nil) and (RefCount = 1) then
    FOwner.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FOwner, 'TDbgDwarfSymbolValue'){$ENDIF};
end;

function TDbgDwarfSymbolValue.GetKind: TDbgSymbolKind;
begin
  if FOwner <> nil then
    Result := FOwner.Kind
  else
  if HasTypeCastInfo then
    Result := FTypeCastInfo.Kind
  else
    Result := inherited GetKind;
end;

function TDbgDwarfSymbolValue.GetAddress: TDbgPtr;
begin
  if FOwner <> nil then
    Result := FOwner.Address
  else
  if HasTypeCastInfo then
    Result := FTypeCastSource.Address
  else
    Result := inherited GetAddress;
end;

function TDbgDwarfSymbolValue.GetMemberCount: Integer;
begin
  if FOwner <> nil then
    Result := FOwner.MemberCount
  else
    Result := inherited GetMemberCount;
end;

function TDbgDwarfSymbolValue.GetMemberByName(AIndex: String): TDbgSymbolValue;
var
  m: TDbgSymbol;
begin
  Result := nil;
  if FOwner <> nil then begin
    m := FOwner.MemberByName[AIndex];
    if m <> nil then
      Result := m.Value;
  end;
end;

function TDbgDwarfSymbolValue.GetMember(AIndex: Integer): TDbgSymbolValue;
var
  m: TDbgSymbol;
begin
  Result := nil;
  if FOwner <> nil then begin
    m := FOwner.Member[AIndex];
    if m <> nil then
      Result := m.Value;
  end;
end;

function TDbgDwarfSymbolValue.GetDbgSymbol: TDbgSymbol;
begin
  Result := FOwner;
end;

function TDbgDwarfSymbolValue.GetTypeInfo: TDbgSymbol;
begin
  if HasTypeCastInfo then
    Result := FTypeCastInfo
  else
    Result := inherited GetTypeInfo;
end;

constructor TDbgDwarfSymbolValue.Create(AMemReader: TFpDbgMemReaderBase);
begin
  FMemReader := AMemReader;
  inherited Create;
end;

destructor TDbgDwarfSymbolValue.Destroy;
begin
  ReleaseRefAndNil(FTypeCastInfo);
  ReleaseRefAndNil(FTypeCastSource);
  inherited Destroy;
end;

procedure TDbgDwarfSymbolValue.SetOwner(AOwner: TDbgDwarfValueIdentifier);
begin
  if FOwner = AOwner then
    exit;
  if (FOwner <> nil) and (RefCount >= 2) then
    FOwner.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FOwner, 'TDbgDwarfSymbolValue'){$ENDIF};
  FOwner := AOwner;
  if (FOwner <> nil) and (RefCount >= 2) then
    FOwner.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FOwner, 'TDbgDwarfSymbolValue'){$ENDIF};
end;

function TDbgDwarfSymbolValue.SetTypeCastInfo(AStructure: TDbgDwarfTypeIdentifier;
  ASource: TDbgSymbolValue): Boolean;
begin
  if FTypeCastSource <> ASource then begin
    if FTypeCastSource <> nil then
      FTypeCastSource.ReleaseReference;
    FTypeCastSource := ASource;
    if FTypeCastSource <> nil then
      FTypeCastSource.AddReference;
  end;

  if FTypeCastInfo <> AStructure then begin
    if FTypeCastInfo <> nil then
      FTypeCastInfo.ReleaseReference;
    FTypeCastInfo := AStructure;
    if FTypeCastInfo <> nil then
      FTypeCastInfo.AddReference;
  end;

  Result := IsValidTypeCast;
end;

{ TDbgDwarfIdentifierParameter }

procedure TDbgDwarfIdentifierParameter.AddressNeeded;
var
  t: TDbgPtr;
begin
  if LocationFromTag(DW_AT_location, t) then
    SetAddress(t)
  else
    SetAddress(0);
end;

function TDbgDwarfIdentifierParameter.HasAddress: Boolean;
begin
  Result := FInformationEntry.HasAttrib(DW_AT_location);
end;

{ TDbgDwarfIdentifierVariable }

procedure TDbgDwarfIdentifierVariable.AddressNeeded;
var
  t: TDbgPtr;
begin
  if LocationFromTag(DW_AT_location, t) then
    SetAddress(t)
  else
    SetAddress(0);
end;

function TDbgDwarfIdentifierVariable.HasAddress: Boolean;
begin
  Result := FInformationEntry.HasAttrib(DW_AT_location);
end;

{ TDbgDwarfValueLocationIdentifier }

procedure TDbgDwarfValueLocationIdentifier.InitLocationParser(const ALocationParser: TDwarfLocationExpression;
  AnObjectDataAddress: TDbgPtr);
begin
  inherited InitLocationParser(ALocationParser, AnObjectDataAddress);
  ALocationParser.OnFrameBaseNeeded := @FrameBaseNeeded;
end;

procedure TDbgDwarfValueLocationIdentifier.FrameBaseNeeded(ASender: TObject);
var
  p: TDbgDwarfIdentifier;
begin
debugln(['TDbgDwarfIdentifierVariable.FrameBaseNeeded ']);
  p := ParentTypeInfo;
  if (p <> nil) and (p is TDbgDwarfProcSymbol) then
    (ASender as TDwarfLocationExpression).FFrameBase := TDbgDwarfProcSymbol(p).GetFrameBase
;
{$warning TODO}
  //else
  //if OwnerTypeInfo <> nil then
  //  OwnerTypeInfo.fr;
  // TODO: check owner
end;

function TDbgDwarfValueLocationIdentifier.GetValueObject: TDbgSymbolValue;
var
  ti: TDbgSymbol;
begin
  Result := FValueObject;
  if Result <> nil then exit;

  ti := TypeInfo;
  if (ti = nil) or not (ti is TDbgDwarfTypeIdentifier) then exit;

  FValueObject := TDbgDwarfTypeIdentifier(ti).GetTypedValueObject(False);
  if FValueObject <> nil then
    FValueObject.SetOwner(self);

  Result := FValueObject;
end;

{ TLEB128PreFixTree }

procedure TLEB128PreFixTree.SetCapacity(ACapacity: integer);
begin
  FDataGrowStep      := Min(512, Max(64, ACapacity));
  FTableListGrowStep := Min(32,  Max(4,  ACapacity div 128));
//debugln(['TLEB128PreFixTree.SetCapacity ', ACapacity, '  ', FDataGrowStep, ' ', FTableListGrowStep]);

  SetLength(FTableList, 1); //FTableListGrowStep div 2);
  SetLength(FTableListGaps, 1); //FTableListGrowStep div 2);
  SetLength(FEndTableData, FDataGrowStep div 4);

  FTableList[0].LeadLow := 255;
  FTableList[0].LeadHigh := 0;
  FTableList[0].EndLow := 255;
  FTableList[0].EndHigh := 0;

  FTableListGaps[0].LeadTable := 0;
  FTableListGaps[0].EndTable  := 0;

  FLeadTableNextFreeIndex := 0; // first 16 are reserved
  FEndTableNextFreeIndex  := 0;
  FTableListNextFreeIndex := 1;

end;

procedure TLEB128PreFixTree.Finish;
begin
  //debugln(['TLEB128PreFixTree.Finish ',' t:', Length(FTableList) ,' => ', FTableListNextFreeIndex,' p:', Length(FLeadTableData) ,' => ', FLeadTableNextFreeIndex,' e:', Length(FEndTableData) ,' => ', FEndTableNextFreeIndex]);
  dec(FLeadTableNextFreeIndex, FTableListGaps[FTableListNextFreeIndex-1].LeadTable);
  dec(FEndTableNextFreeIndex, FTableListGaps[FTableListNextFreeIndex-1].EndTable);
  SetLength(FTableList,     FTableListNextFreeIndex);
  SetLength(FLeadTableData, FLeadTableNextFreeIndex);
  SetLength(FEndTableData,  FEndTableNextFreeIndex);
  // TODO: clear gaps
  SetLength(FTableListGaps, 0);
end;

function TLEB128PreFixTree.AddLeb128FromPointer(APointer: Pointer;
  const AData: TDwarfAbbrev): Pointer;
var
  TableListLen: Integer;

  procedure AllocLeadTableIndexes(AnAmount: Integer); inline;
  begin
    inc(FLeadTableNextFreeIndex, AnAmount);
    if Length(FLeadTableData) < FLeadTableNextFreeIndex then begin
      SetLength(FLeadTableData, FLeadTableNextFreeIndex + FDataGrowStep);
      //debugln(['IncreaseLeadTableListSize ', DbgS(self), ' ', FLeadTableNextFreeIndex ]);
    end;
  end;

  procedure AllocEndTableIndexes(AnAmount: Integer); inline;
  begin
    inc(FEndTableNextFreeIndex, AnAmount);
    if Length(FEndTableData) < FEndTableNextFreeIndex then begin
      SetLength(FEndTableData, FEndTableNextFreeIndex + FDataGrowStep);
      //debugln(['IncreaseEndTableListSize ', DbgS(self), ' ', FEndTableNextFreeIndex ]);
    end;
  end;

  function NewEntryInTableList: Cardinal; inline;
  begin
    if FTableListNextFreeIndex >= TableListLen then begin
      //debugln(['inc(TableListLen, 512) ', DbgS(self), ' ', TableListLen]);
      inc(TableListLen, FTableListGrowStep);
      SetLength(FTableList, TableListLen);
      SetLength(FTableListGaps, TableListLen);
    end;

    Result := FTableListNextFreeIndex;
    FTableList[Result].LeadLow  := 255;
    FTableList[Result].LeadHigh := 0;
    FTableList[Result].EndLow  := 255;
    FTableList[Result].EndHigh := 0;
    FTableListGaps[Result].LeadTable := 0;
    FTableListGaps[Result].EndTable  := 0;
    inc(FTableListNextFreeIndex);
  end;

  procedure AppendToLeadTable(ATableListIndex: Cardinal; AEntry: PLeb128TableEntry;
    ALeadByte: Byte; ATarget: Cardinal); //inline;
  var
    GapAvail, ANeeded: Integer;
    AtEnd: Boolean;
    i, NewIndex: Cardinal;
  begin
    if AEntry^.LeadLow > AEntry^.LeadHigh then
    begin // empty table // create new
      AEntry^.LeadLow := ALeadByte;
      AEntry^.LeadIndex := FLeadTableNextFreeIndex;
      AllocLeadTableIndexes(16);
      FTableListGaps[ATableListIndex].LeadTable := 15; // 16-1
    end
    else
    begin // append to existing
      GapAvail := FTableListGaps[ATableListIndex].LeadTable;
      assert(AEntry^.LeadIndex + AEntry^.LeadHigh - AEntry^.LeadLow + 1 + GapAvail <= FLeadTableNextFreeIndex);
      AtEnd := AEntry^.LeadIndex + AEntry^.LeadHigh - AEntry^.LeadLow + 1 + GapAvail = FLeadTableNextFreeIndex;
      ANeeded := ALeadByte - AEntry^.LeadHigh;

      if ANeeded <= GapAvail then begin
        dec(FTableListGaps[ATableListIndex].LeadTable, ANeeded);
      end
      else
      if AtEnd then begin
        AllocLeadTableIndexes(ANeeded + 16);
        FTableListGaps[ATableListIndex].LeadTable := 16;
      end
      else
      begin
        // Todo deal with the GAP at the old location
        i := AEntry^.LeadHigh - AEntry^.LeadLow + 1; // Current Size
        NewIndex := FLeadTableNextFreeIndex;
        //DebugLn(['MOVING LEAD', DbgS(self), ' From: ', AEntry^.LeadIndex, ' To: ', NewIndex] );
        AllocLeadTableIndexes(i + ANeeded +16);
        move(FLeadTableData[AEntry^.LeadIndex], FLeadTableData[NewIndex], i * SizeOf(FLeadTableData[0]));
        AEntry^.LeadIndex := NewIndex;
        FTableListGaps[ATableListIndex].LeadTable := 16;
      end;
    end; // append to existing

    AEntry^.LeadHigh := ALeadByte;
    i := AEntry^.LeadIndex + ALeadByte - AEntry^.LeadLow;
    FLeadTableData[i] := ATarget;
  end;

  procedure PrependToLeadTable(ATableListIndex: Cardinal; AEntry: PLeb128TableEntry;
    ALeadByte: Byte; ATarget: Cardinal); //inline;
  var
    GapAvail, ANeeded: Integer;
    AtEnd: Boolean;
    i, NewIndex: Cardinal;
  begin
    Assert(AEntry^.LeadLow <= AEntry^.LeadHigh, 'emty table must be handled by append');
    GapAvail := FTableListGaps[ATableListIndex].LeadTable;
    assert(AEntry^.LeadIndex + AEntry^.LeadHigh - AEntry^.LeadLow + 1 + GapAvail <= FLeadTableNextFreeIndex);
    AtEnd := AEntry^.LeadIndex + AEntry^.LeadHigh - AEntry^.LeadLow + 1 + GapAvail = FLeadTableNextFreeIndex;
    ANeeded := AEntry^.LeadLow - ALeadByte;

    if (ANeeded <= GapAvail) or AtEnd then begin
      if (ANeeded > GapAvail) then begin
        AllocLeadTableIndexes(ANeeded + 16);
        FTableListGaps[ATableListIndex].LeadTable := 16;
      end
      else
        dec(FTableListGaps[ATableListIndex].LeadTable, ANeeded);
      NewIndex := AEntry^.LeadIndex + ANeeded;
      i := AEntry^.LeadHigh - AEntry^.LeadLow + 1; // Current size
      move(FLeadTableData[AEntry^.LeadIndex], FLeadTableData[NewIndex], i * SizeOf(FLeadTableData[0]));
      FillByte(FLeadTableData[AEntry^.LeadIndex+1], Min(i, ANeeded-1) * SizeOf(FLeadTableData[0]), 0);
    end
    else
    begin
      // Todo deal with the GAP at the old location
      i := AEntry^.LeadHigh - AEntry^.LeadLow + 1; // Current Size
      NewIndex := FLeadTableNextFreeIndex;
      //DebugLn(['MOVING LEAD', DbgS(self), ' From: ', AEntry^.LeadIndex, ' To: ', NewIndex] );
      AllocLeadTableIndexes(i + ANeeded + 16);
      move(FLeadTableData[AEntry^.LeadIndex], FLeadTableData[NewIndex+ANeeded], i * SizeOf(FLeadTableData[0]));
      // FillByte only neede, if gap will be reclaimed
      //FillByte(FLeadTableData[AEntry^.LeadIndex], i * SizeOf(FLeadTableData[0]), 0);
      AEntry^.LeadIndex := NewIndex;
      FTableListGaps[ATableListIndex].LeadTable := 16;
    end;


    AEntry^.LeadLow := ALeadByte;
    FLeadTableData[AEntry^.LeadIndex] := ATarget;
  end;

  procedure AppendToEndTable(ATableListIndex: Cardinal; AEntry: PLeb128TableEntry;
    ALeadByte: Byte; const AData: TDwarfAbbrev {Pointer}); //inline;
  var
    GapAvail, ANeeded: Integer;
    AtEnd: Boolean;
    i, NewIndex: Cardinal;
  begin
    if AEntry^.EndLow > AEntry^.EndHigh then
    begin // empty table // create new
      AEntry^.EndLow := ALeadByte;
      AEntry^.EndIndex := FEndTableNextFreeIndex;
      AllocEndTableIndexes(16);
      FTableListGaps[ATableListIndex].EndTable := 15; // 16-1
    end
    else
    begin // append to existing
      GapAvail := FTableListGaps[ATableListIndex].EndTable;
      assert(AEntry^.EndIndex + AEntry^.LeadHigh - AEntry^.LeadLow + 1 + GapAvail <= FEndTableNextFreeIndex);
      AtEnd := AEntry^.EndIndex + AEntry^.EndHigh - AEntry^.EndLow + 1 + GapAvail = FEndTableNextFreeIndex;
      ANeeded := ALeadByte - AEntry^.EndHigh;

      if ANeeded <= GapAvail then begin
        dec(FTableListGaps[ATableListIndex].EndTable, ANeeded);
      end
      else
      if AtEnd then begin
        AllocEndTableIndexes(ANeeded + 16);
        FTableListGaps[ATableListIndex].EndTable := 16;
      end
      else
      begin
        // Todo deal with the GAP at the old location
        i := AEntry^.EndHigh - AEntry^.EndLow + 1; // Current Size
        NewIndex := FEndTableNextFreeIndex;
        //DebugLn(['MOVING END',  DbgS(self), ' From: ', AEntry^.EndIndex, ' To: ', NewIndex ]);
        AllocEndTableIndexes(i + ANeeded + 16);
        move(FEndTableData[AEntry^.EndIndex], FEndTableData[NewIndex], i * SizeOf(FEndTableData[0]));
        AEntry^.EndIndex := NewIndex;
        FTableListGaps[ATableListIndex].EndTable := 16;
      end;
    end; // append to existing

    AEntry^.EndHigh := ALeadByte;
    i := AEntry^.EndIndex + ALeadByte - AEntry^.EndLow;
    FEndTableData[i] := AData;
  end;

  procedure PrependToEndTable(ATableListIndex: Cardinal; AEntry: PLeb128TableEntry;
    AEndByte: Byte; const AData: TDwarfAbbrev); //inline;
  var
    GapAvail, ANeeded: Integer;
    AtEnd: Boolean;
    i, NewIndex: Cardinal;
  begin
    Assert(AEntry^.EndLow <= AEntry^.EndHigh, 'emty table must be handled by append');
    GapAvail := FTableListGaps[ATableListIndex].EndTable;
    assert(AEntry^.EndIndex + AEntry^.EndHigh - AEntry^.EndLow + 1 + GapAvail <= FEndTableNextFreeIndex);
    AtEnd := AEntry^.EndIndex + AEntry^.EndHigh - AEntry^.EndLow + 1 + GapAvail = FEndTableNextFreeIndex;
    ANeeded := AEntry^.EndLow - AEndByte;

    if (ANeeded <= GapAvail) or AtEnd then begin
      if (ANeeded > GapAvail) then begin
        AllocEndTableIndexes(ANeeded + 16);
        FTableListGaps[ATableListIndex].EndTable := 16;
      end
      else
        dec(FTableListGaps[ATableListIndex].EndTable, ANeeded);
      NewIndex := AEntry^.EndIndex + ANeeded;
      i := AEntry^.EndHigh - AEntry^.EndLow + 1; // Current size
      move(FEndTableData[AEntry^.EndIndex], FEndTableData[NewIndex], i * SizeOf(FEndTableData[0]));
      FillByte(FEndTableData[AEntry^.EndIndex+1], Min(i, ANeeded-1) * SizeOf(FEndTableData[0]), 0);
    end
    else
    begin
      // Todo deal with the GAP at the old location
      i := AEntry^.EndHigh - AEntry^.EndLow + 1; // Current Size
      NewIndex := FEndTableNextFreeIndex;
      //DebugLn(['MOVING END', DbgS(self), ' From: ', AEntry^.EndIndex, ' To: ', NewIndex] );
      AllocEndTableIndexes(i + ANeeded + 16);
      move(FEndTableData[AEntry^.EndIndex], FEndTableData[NewIndex+ANeeded], i * SizeOf(FEndTableData[0]));
      // FillByte only neede, if gap will be reclaimed
      //FillByte(FEndTableData[AEntry^.EndIndex], i * SizeOf(FEndTableData[0]), 0);
      AEntry^.EndIndex := NewIndex;
      FTableListGaps[ATableListIndex].EndTable := 16;
    end;


    AEntry^.EndLow := AEndByte;
    FEndTableData[AEntry^.EndIndex] := AData;
  end;

var
  LEB128: PByte;
  b: Byte;
  TableListIndex: Integer;
  e: PLeb128TableEntry;
  i, NewIdx: Cardinal;
begin
  LEB128 := APointer;
  i := 16; // Just an abort condition, for malformed data.
  while (LEB128^ >= 128) do begin
    inc(LEB128);
    dec(i);
    if i = 0 then begin
      DebugLn(FPDBG_DWARF_WARNINGS, ['ENDLESS LEB128']);
      exit;
    end;
  end;
  Result := LEB128 + 1;

  TableListIndex := 0;
  TableListLen := Length(FTableList);

  while (LEB128 > APointer) and ((LEB128^ and $7f) = 0) do
    dec(LEB128);

  // LeadByte
  while LEB128 > APointer do begin
    b := LEB128^ and $7f;

    Assert(TableListIndex < TableListLen);
    e := @FTableList[TableListIndex];
    if (b > e^.LeadHigh) or (e^.LeadHigh < e^.LeadLow) then begin
      NewIdx := NewEntryInTableList;
      e := @FTableList[TableListIndex];
      AppendToLeadTable(TableListIndex, e, b, NewIdx);
      TableListIndex := NewIdx;
    end
    else
    if (b < e^.LeadLow) then begin
      NewIdx := NewEntryInTableList;
      e := @FTableList[TableListIndex];
      PrependToLeadTable(TableListIndex, e, b, NewIdx);
      TableListIndex := NewIdx;
    end
    else
    begin
      // existing entry
      i := e^.LeadIndex + b - e^.LeadLow;
      TableListIndex := FLeadTableData[i];
      if TableListIndex = 0 then begin // not yet assigned (not allowed to point back to 0)
        TableListIndex := NewEntryInTableList;
        FLeadTableData[i] := TableListIndex;
      end;
    end;

    dec(LEB128);
  end;

  // EndByte
  //if AData = nil then AData := LEB128;
  Assert(TableListIndex < TableListLen);
  b := LEB128^ and $7f;
  e := @FTableList[TableListIndex];
  if (b > e^.EndHigh) or (e^.EndHigh < e^.EndLow) then begin
    AppendToEndTable(TableListIndex, e, b, AData);
  end
  else
  if (b < e^.EndLow) then begin
    PrependToEndTable(TableListIndex, e, b, AData);
  end
  else
  begin
    // in existingc range
    i := e^.EndIndex + b - e^.EndLow;
    //assert(FEndTableData[i] = nil, 'Duplicate LEB128');
    FEndTableData[i] := AData;
  end;

end;

function TLEB128PreFixTree.FindLe128bFromPointer(APointer: Pointer; out
  AData: PDwarfAbbrev): Pointer;
var
  LEB128: PByte;
  b: Byte;
  TableListIndex: Integer;
  e: PLeb128TableEntry;
  i: Cardinal;
  TableListLen: Integer;
  LEB128End: PByte;
begin
  AData := nil;
  Result := nil;

  TableListLen := Length(FTableList);
  if TableListLen = 0 then
    exit;

  LEB128 := APointer;
  i := 16; // Just an abort condition, for malformed data.
  while (LEB128^ >= 128) do begin
    inc(LEB128);
    dec(i);
    if i = 0 then begin
      DebugLn(FPDBG_DWARF_WARNINGS, ['ENDLESS LEB128']);
      exit;
    end;
  end;
  LEB128End := LEB128;

  while (LEB128 > APointer) and ((LEB128^ and $7f) = 0) do
    dec(LEB128);

  TableListIndex := 0;
  // LeadByte
  while LEB128 > APointer do begin
    b := LEB128^ and $7f;

    Assert(TableListIndex < TableListLen);
    e := @FTableList[TableListIndex];
    if (b > e^.LeadHigh) or (b < e^.LeadLow) then begin
      //debugln('1 OUT OF RANGE / NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!');
      exit;
    end
    else
    begin
      TableListIndex := FLeadTableData[e^.LeadIndex + b - e^.LeadLow];
      if TableListIndex = 0 then begin // not yet assigned (not allowed to point back to 0)
        //debugln('3 OUT OF RANGE / NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!');
        exit;
      end;
    end;

    dec(LEB128);
  end;

  // EndByte
  Assert(TableListIndex < TableListLen);
  b := LEB128^ and $7f;
  e := @FTableList[TableListIndex];
  if (b > e^.EndHigh) or (b < e^.EndLow) then begin
    //debugln('4 OUT OF RANGE / NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!');
    exit;
  end
  else
  begin
    i := e^.EndIndex + b - e^.EndLow;
    //assert(FEndTableData[i] = nil, 'Duplicate LEB128');
    AData := @FEndTableData[i];
    if AData^.tag > 0 then // tag 0 does not exist
      Result := LEB128End+1;
  end;

end;

function TLEB128PreFixTree.FindLe128bFromPointer(APointer: Pointer; out
  AData: TDwarfAbbrev): Pointer;
var
  p: PDwarfAbbrev;
begin
  Result := FindLe128bFromPointer(APointer, p);
  if p = nil then begin
    AData.index := -1;
    AData.tag := 0;
    AData.count := 0;
    AData.flags := [];
  end
  else
    AData := p^;
end;

{ TDwarfAbbrevList }

function TDwarfAbbrevList.GetEntryPointer(AIndex: Integer): PDwarfAbbrevEntry;
begin
  Result := @FDefinitions[AIndex];
end;

procedure TDwarfAbbrevList.LoadAbbrevs(AnAbbrevDataPtr: Pointer);
  procedure MakeRoom(AMinSize: Integer);
  var
    len: Integer;
  begin
    len := Length(FDefinitions);
    if len > AMinSize then Exit;
    if len > $4000
    then Inc(len, $4000)
    else len := len * 2;
    SetLength(FDefinitions, len);
  end;
var
  p: Pointer;
  Def: TDwarfAbbrev;
  Def2: PDwarfAbbrev;
  abbrev, attrib, form: Cardinal;
  n: Integer;
  CurAbbrevIndex: Integer;
  DbgVerbose: Boolean;
  f: TDwarfAbbrevFlags;
begin
  abbrev := 0;
  CurAbbrevIndex := 0;
  DbgVerbose := (FPDBG_DWARF_VERBOSE <> nil) and (FPDBG_DWARF_VERBOSE^.Enabled);

  while (pbyte(AnAbbrevDataPtr) < FAbbrDataEnd) and (pbyte(AnAbbrevDataPtr)^ <> 0) do
  begin
    p := AnAbbrevDataPtr;
    abbrev := ULEB128toOrdinal(pbyte(AnAbbrevDataPtr));
    Def.tag := ULEB128toOrdinal(pbyte(AnAbbrevDataPtr));

    {$IFDEF USE_ABBREV_TMAP}
    if FMap.HasId(abbrev)
    {$ELSE}
    if FindLe128bFromPointer(p, Def2) <> nil
    {$Endif}
    then begin
      DebugLn(FPDBG_DWARF_WARNINGS, ['Duplicate abbrev=', abbrev, ' found. Ignoring....']);
      while pword(AnAbbrevDataPtr)^ <> 0 do Inc(pword(AnAbbrevDataPtr));
      Inc(pword(AnAbbrevDataPtr));
      Continue;
    end;

    if DbgVerbose
    then begin
      DebugLn(FPDBG_DWARF_VERBOSE, ['  abbrev:  ', abbrev]);
      DebugLn(FPDBG_DWARF_VERBOSE, ['  tag:     ', Def.tag, '=', DwarfTagToString(Def.tag)]);
      DebugLn(FPDBG_DWARF_VERBOSE, ['  children:', pbyte(AnAbbrevDataPtr)^, '=', DwarfChildrenToString(pbyte(AnAbbrevDataPtr)^)]);
    end;
    if pbyte(AnAbbrevDataPtr)^ = DW_CHILDREN_yes then
      f := [dafHasChildren]
    else
      f := [];
    Inc(pbyte(AnAbbrevDataPtr));

    n := 0;
    Def.Index := CurAbbrevIndex;

    while pword(AnAbbrevDataPtr)^ <> 0 do
    begin
      attrib := ULEB128toOrdinal(pbyte(AnAbbrevDataPtr));
      if attrib = DW_AT_name then
        Include(f, dafHasName)
      else
      if attrib = DW_AT_low_pc then
        Include(f, dafHasLowAddr)
      else
      if attrib = DW_AT_start_scope then
        Include(f, dafHasStartScope)
      else
      if attrib = DW_AT_abstract_origin then
        Include(f, dafHasAbstractOrigin);

      form := ULEB128toOrdinal(pbyte(AnAbbrevDataPtr));

      MakeRoom(CurAbbrevIndex + 1);
      FDefinitions[CurAbbrevIndex].Attribute := attrib;
      FDefinitions[CurAbbrevIndex].Form := form;
      Inc(CurAbbrevIndex);

      if DbgVerbose
      then DebugLn(FPDBG_DWARF_VERBOSE, ['   [', n, '] attrib: ', attrib, '=', DwarfAttributeToString(attrib), ', form: ', form, '=', DwarfAttributeFormToString(form)]);
      Inc(n);
    end;
    Def.Count := n;
    Def.flags := f;
    {$IFDEF USE_ABBREV_TMAP}
    FMap.Add(abbrev, Def);
    {$ELSE}
    AddLeb128FromPointer(p, Def);
    {$Endif}

    Inc(pword(AnAbbrevDataPtr));
  end;
end;

constructor TDwarfAbbrevList.Create(AnAbbrData, AnAbbrDataEnd: Pointer; AnAbbrevOffset,
  AInfoLen: QWord);
begin
  inherited Create;
  FAbbrDataEnd := AnAbbrDataEnd;
  {$IFDEF USE_ABBREV_TMAP}
  FMap := TMap.Create(itu4, SizeOf(TDwarfAbbrev));
  {$ELSE}
  SetCapacity(AInfoLen div 16 + 1);
  {$Endif}
  SetLength(FDefinitions, 256);
  //LoadAbbrevs(FOwner.PointerFromVA(dsAbbrev, FAbbrevOffset));
  LoadAbbrevs(AnAbbrData + AnAbbrevOffset);
  {$IFnDEF USE_ABBREV_TMAP}
  Finish;
  {$Endif}
end;

destructor TDwarfAbbrevList.Destroy;
begin
  {$IFDEF USE_ABBREV_TMAP}
  FreeAndNil(FMap);
  {$Endif}
  inherited Destroy;
end;

{$IFDEF USE_ABBREV_TMAP}
function TDwarfAbbrevList.FindLe128bFromPointer(AnAbbrevPtr: Pointer; out
  AData: TDwarfAbbrev): Pointer;
begin
  Result := AnAbbrevPtr;
  if not FMap.GetData(ULEB128toOrdinal(Result), AData) then
    Result := nil;
end;
{$Endif}

{ TDwarfScopeInfo }

procedure TDwarfScopeInfo.Init(AScopeList: PDwarfScopeList);
begin
  FIndex := -1;
  FScopeList := AScopeList;
end;

function TDwarfScopeInfo.IsValid: Boolean;
begin
  Result := FIndex >= 0;
end;

function TDwarfScopeInfo.GetNext: TDwarfScopeInfo;
begin
  Result.Init(FScopeList);
  if IsValid then
    Result.Index := GetNextIndex;
end;

function TDwarfScopeInfo.GetNextIndex: Integer;
var
  l: Integer;
  p: PDwarfScopeInfoRec;
begin
  Result := -1;
  if (not IsValid) or (FScopeList^.HighestKnown = FIndex) then exit;
  // Use pointer, to avoid calculating the index twice
  p := @FScopeList^.List[FIndex + 1];
  Result := p^.Link;
  assert(Result <= FScopeList^.HighestKnown);
  if (Result > FIndex + 1) then       // Index+1 is First Child, with pointer to Next
    exit;

  l := (p-1)^.Link; // GetParent  (or -1 for toplevel)
  assert(l <= FScopeList^.HighestKnown);
  if l > Index then l := Index - 1;   // This is a first child, make l = parent
  if (Result = l) then begin          // Index + 1 has same parent
    Result := Index + 1;
    exit;
  end;

  Result := -1;
end;

function TDwarfScopeInfo.GetEntry: Pointer;
begin
  Result := nil;
  if IsValid then
    Result := FScopeList^.List[FIndex].Entry;
end;

function TDwarfScopeInfo.HasChild: Boolean;
var
  l2: Integer;
begin
  Result := (IsValid) and (FScopeList^.HighestKnown > FIndex);
  if not Result then exit;
  l2 := FScopeList^.List[FIndex + 1].Link;
  assert(l2 <= FScopeList^.HighestKnown);
  Result := (l2 > FIndex + 1) or        // Index+1 is First Child, with pointer to Next
            (l2 = FIndex);              // Index+1 is First Child, with pointer to parent (self)
end;

function TDwarfScopeInfo.GetChild: TDwarfScopeInfo;
begin
  Result.Init(FScopeList);
  if HasChild then begin
    Result.Index := FIndex + 1;
    assert(Result.Parent.Index = FIndex, 'child has self as parent');
  end;
end;

function TDwarfScopeInfo.GetChildIndex: Integer;
begin
  if HasChild then
    Result := FIndex + 1
  else
    Result := -1;
end;

function TDwarfScopeInfo.GetParent: TDwarfScopeInfo;
var
  l: Integer;
begin
  Result.Init(FScopeList);
  if not IsValid then exit;
  l := FScopeList^.List[FIndex].Link; // GetParent  (or -1 for toplevel)
  assert(l <= FScopeList^.HighestKnown);
  if l > Index then
    l := Index - 1;   // This is a first child, make l = parent
  Result.Index := l;
end;

function TDwarfScopeInfo.GetParentIndex: Integer;
begin
  Result := -1;
  if not IsValid then exit;
  Result := FScopeList^.List[FIndex].Link; // GetParent  (or -1 for toplevel)
  assert(Result <= FScopeList^.HighestKnown);
  if Result > Index then
    Result := Index - 1;   // This is a first child, make l = parent
end;

procedure TDwarfScopeInfo.SetIndex(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex <= FScopeList^.HighestKnown) then
    FIndex := AIndex
  else
    FIndex := -1;
end;

function TDwarfScopeInfo.CreateScopeForEntry(AEntry: Pointer; ALink: Integer): Integer;
begin
  inc(FScopeList^.HighestKnown);
  Result := FScopeList^.HighestKnown;
  if Result >= Length(FScopeList^.List) then
    SetLength(FScopeList^.List, Result + SCOPE_ALLOC_BLOCK_SIZE);
  with FScopeList^.List[Result] do begin
    Entry := AEntry;
    Link := ALink;
  end;
end;

function TDwarfScopeInfo.HasParent: Boolean;
var
  l: Integer;
begin
  Result := (IsValid);
  if not Result then exit;
  l := FScopeList^.List[FIndex].Link;
  assert(l <= FScopeList^.HighestKnown);
  Result := (l >= 0);
end;

function TDwarfScopeInfo.HasNext: Boolean;
var
  l, l2: Integer;
begin
  Result := (IsValid) and (FScopeList^.HighestKnown > FIndex);
  if not Result then exit;
  l2 := FScopeList^.List[FIndex + 1].Link;
  assert(l2 <= FScopeList^.HighestKnown);
  Result := (l2 > FIndex + 1);        // Index+1 is First Child, with pointer to Next
  if Result then
    exit;

  l := FScopeList^.List[FIndex].Link; // GetParent  (or -1 for toplevel)
  assert(l <= FScopeList^.HighestKnown);
  if l > Index then
    l := Index - 1;   // This is a first child, make l = parent
  Result := (l2 = l);                 // Index + 1 has same parent
end;

procedure TDwarfScopeInfo.GoParent;
var
  l: Integer;
begin
  if not IsValid then exit;
  l := FScopeList^.List[FIndex].Link; // GetParent  (or -1 for toplevel)
  assert(l <= FScopeList^.HighestKnown);
  if l > Index then
    l := Index - 1;   // This is a first child, make l = parent
  FIndex := l;
end;

procedure TDwarfScopeInfo.GoNext;
begin
  FIndex := GetNextIndex;
end;

procedure TDwarfScopeInfo.GoChild;
begin
  if HasChild then
    FIndex := FIndex + 1
  else
    FIndex := -1;
end;

function TDwarfScopeInfo.CreateNextForEntry(AEntry: Pointer): Integer;
var
  l: Integer;
begin
  assert(IsValid, 'Creating Child for invalid scope');
  assert(NextIndex<0, 'Next already set');
  l := FScopeList^.List[FIndex].Link; // GetParent (or -1 for toplevel)
  assert(l <= FScopeList^.HighestKnown);
  if l > Index then l := Index - 1;   // This is a first child, make l = parent
  Result := CreateScopeForEntry(AEntry, l);
  if Result > FIndex + 1 then  // We have children
    FScopeList^.List[FIndex+1].Link := Result;
end;

function TDwarfScopeInfo.CreateChildForEntry(AEntry: Pointer): Integer;
begin
  assert(IsValid, 'Creating Child for invalid scope');
  assert(FIndex=FScopeList^.HighestKnown, 'Cannot creating Child.Not at end of list');
  Result := CreateScopeForEntry(AEntry, FIndex); // First Child, but no parent.next yet
end;

{ TDwarfLocationStack }

procedure TDwarfLocationStack.IncCapacity;
begin
  SetLength(FList, Max(Length(FList), FCount) + 64);
end;

procedure TDwarfLocationStack.Clear;
begin
  FCount := 0;
end;

function TDwarfLocationStack.Count: Integer;
begin
  Result := FCount;
end;

function TDwarfLocationStack.Pop: TDwarfLocationStackEntry;
begin
  if FCount = 0 then begin
    Result.Kind := lseError;
    exit;
  end;
  dec(FCount);
  Result := FList[FCount];
end;

function TDwarfLocationStack.Peek: TDwarfLocationStackEntry;
begin
  if FCount = 0 then begin
    Result.Kind := lseError;
    exit;
  end;
  Result := FList[FCount-1];
end;

function TDwarfLocationStack.PeekKind: TDwarfLocationStackEntryKind;
begin
  if FCount = 0 then begin
    Result := lseError;
    exit;
  end;
  Result := FList[FCount-1].Kind;
end;

function TDwarfLocationStack.Peek(AIndex: Integer): TDwarfLocationStackEntry;
begin
  if AIndex >= FCount then begin
    Result.Kind := lseError;
    exit;
  end;
  Result := FList[FCount-1-AIndex];
end;

procedure TDwarfLocationStack.Push(const AEntry: TDwarfLocationStackEntry);
begin
  if Length(FList) <= FCount then
    IncCapacity;
  FList[FCount] := AEntry;
  inc(FCount);
end;

procedure TDwarfLocationStack.Push(AValue: TDbgPtr; AKind: TDwarfLocationStackEntryKind);
begin
  if Length(FList) <= FCount then
    IncCapacity;
  FList[FCount].Value := AValue;
  FList[FCount].Kind  := AKind;
  inc(FCount);
end;

procedure TDwarfLocationStack.Modify(AIndex: Integer; const AEntry: TDwarfLocationStackEntry);
begin
  Assert(AIndex < FCount);
  if AIndex >= FCount then
    exit;
  FList[FCount-1-AIndex] := AEntry;
end;

procedure TDwarfLocationStack.Modify(AIndex: Integer; AValue: TDbgPtr;
  AKind: TDwarfLocationStackEntryKind);
begin
  Assert(AIndex < FCount);
  if AIndex >= FCount then
    exit;
  FList[FCount-1-AIndex].Value := AValue;
  FList[FCount-1-AIndex].Kind  := AKind;
end;

{ TDwarfLocationExpression }

constructor TDwarfLocationExpression.Create(AExpressionData: Pointer; AMaxCount: Integer;
  ACU: TDwarfCompilationUnit);
begin
  FStack.Clear;
  FCU := ACU;
  FData := AExpressionData;
  FMaxData := FData + AMaxCount;
end;

procedure TDwarfLocationExpression.Evaluate;

  procedure SetError;
  begin
    FStack.Push(0, lseError); // Mark as failed
  end;

  function AssertAddressOnStack: Boolean;
  begin
    Result := FStack.PeekKind in [lseValue, lseRegister]; // todo: allow register?
    if not Result then
      SetError;
  end;

  function AssertMinCount(ACnt: Integer): Boolean;
  begin
    Result := FStack.Count >= ACnt;
    if not Result then
      SetError;
  end;

var
  MemReader: TFpDbgMemReaderBase;
  AddrSize: Byte;

  function ReadValueFromMemory(AnAddress: TDbgPtr; ASize: Cardinal; out AValue: TDbgPtr): Boolean;
  begin
    //TODO: zero fill / sign extend
    if (ASize > SizeOf(AValue)) or (ASize > AddrSize) then exit(False);
    AValue := 0;
    Result := MemReader.ReadMemory(AnAddress, ASize, @AValue);
    if not Result then
      SetError;
  end;

  function ReadValueFromMemoryEx(AnAddress, AnAddrSpace: TDbgPtr; ASize: Cardinal; out AValue: TDbgPtr): Boolean;
  begin
    //TODO: zero fill / sign extend
    if (ASize > SizeOf(AValue)) or (ASize > AddrSize) then exit(False);
    AValue := 0;
    Result := MemReader.ReadMemoryEx(AnAddress, AnAddrSpace, ASize, @AValue);
    if not Result then
      SetError;
  end;

  function ReadUnsignedFromExpression(var p: Pointer; ASize: Integer): TDbgPtr;
  begin
    case ASize of
      1: Result := PByte(p)^;
      2: Result := PWord(p)^;
      4: Result := PLongWord(p)^;
      8: Result := PQWord(p)^;
      0: Result := ULEB128toOrdinal(p);
    end;
    inc(p, ASize);
  end;

  function ReadSignedFromExpression(var p: Pointer; ASize: Integer): TDbgPtr;
  begin
    case ASize of
      1: Int64(Result) := PShortInt(p)^;
      2: Int64(Result) := PSmallInt(p)^;
      4: Int64(Result) := PLongint(p)^;
      8: Int64(Result) := PInt64(p)^;
      0: Int64(Result) := SLEB128toOrdinal(p);
    end;
    inc(p, ASize);
  end;

var
  p: PByte;
  NewValue: TDbgPtr;
  i: TDbgPtr;
  x : integer;
  Entry, Entry2: TDwarfLocationStackEntry;
begin
  AddrSize := FCU.FAddressSize;
  MemReader := FCU.FOwner.MemReader;
  while FData < FMaxData do begin
    p := FData;
    inc(FData);
DebugLn(['p=',p^, ' , ', FData^, ' Cnt=',FStack.Count, ' top=',FStack.Peek(0).Value]);
    case p^ of
      DW_OP_nop: ;
      DW_OP_addr:  FStack.Push(FCU.ReadAddressAtPointer(FData, True), lseValue);
      DW_OP_deref: begin
          if not AssertAddressOnStack then exit;
          if not ReadValueFromMemory(FStack.Pop.Value, AddrSize, NewValue) then exit;
          FStack.Push(NewValue, lseValue);
        end;
      DW_OP_xderef: begin
          if not AssertAddressOnStack  then exit;
          i := FStack.Pop.Value;
          if not AssertAddressOnStack then exit;
          if not ReadValueFromMemoryEx(i, FStack.Pop.Value, AddrSize, NewValue) then exit;
          FStack.Push(NewValue, lseValue);
        end;
      DW_OP_deref_size: begin
          if not AssertAddressOnStack then exit;
          if not ReadValueFromMemory(FStack.Pop.Value, ReadUnsignedFromExpression(FData, 1), NewValue) then exit;
          FStack.Push(NewValue, lseValue);
        end;
      DW_OP_xderef_size: begin
          if not AssertAddressOnStack  then exit;
          i := FStack.Pop.Value;
          if not AssertAddressOnStack then exit;
          if not ReadValueFromMemoryEx(i, FStack.Pop.Value, ReadUnsignedFromExpression(FData, 1), NewValue) then exit;
          FStack.Push(NewValue, lseValue);
        end;

      DW_OP_const1u: FStack.Push(ReadUnsignedFromExpression(FData, 1), lseValue);
      DW_OP_const2u: FStack.Push(ReadUnsignedFromExpression(FData, 2), lseValue);
      DW_OP_const4u: FStack.Push(ReadUnsignedFromExpression(FData, 4), lseValue);
      DW_OP_const8u: FStack.Push(ReadUnsignedFromExpression(FData, 8), lseValue);
      DW_OP_constu:  FStack.Push(ReadUnsignedFromExpression(FData, 0), lseValue);
      DW_OP_const1s: FStack.Push(ReadSignedFromExpression(FData, 1), lseValue);
      DW_OP_const2s: FStack.Push(ReadSignedFromExpression(FData, 2), lseValue);
      DW_OP_const4s: FStack.Push(ReadSignedFromExpression(FData, 4), lseValue);
      DW_OP_const8s: FStack.Push(ReadSignedFromExpression(FData, 8), lseValue);
      DW_OP_consts:  FStack.Push(ReadSignedFromExpression(FData, 0), lseValue);
      DW_OP_lit0..DW_OP_lit31: FStack.Push(p^-DW_OP_lit0, lseValue);

      DW_OP_reg0..DW_OP_reg31: begin
          if not MemReader.ReadRegister(p^-DW_OP_reg0, NewValue) then begin
            SetError;
            exit;
          end;
          FStack.Push(NewValue, lseRegister);
        end;
      DW_OP_regx: begin
          if not MemReader.ReadRegister(ULEB128toOrdinal(FData), NewValue) then begin
            SetError;
            exit;
          end;
          FStack.Push(NewValue, lseRegister);
        end;

      DW_OP_breg0..DW_OP_breg31: begin
          if not MemReader.ReadRegister(p^-DW_OP_breg0, NewValue) then begin
            SetError;
            exit;
          end;
          {$PUSH}{$R-}{$Q-}
          FStack.Push(NewValue+SLEB128toOrdinal(FData), lseValue);
          {$POP}
        end;
      DW_OP_bregx: begin
          if not MemReader.ReadRegister(ULEB128toOrdinal(FData), NewValue) then begin
            SetError;
            exit;
          end;
          {$PUSH}{$R-}{$Q-}
          FStack.Push(NewValue+SLEB128toOrdinal(FData), lseValue);
          {$POP}
        end;

      DW_OP_fbreg: begin
          if (FFrameBase = 0) and (FOnFrameBaseNeeded <> nil) then FOnFrameBaseNeeded(Self);
          if FFrameBase = 0 then begin
            SetError;
            exit;
          end;
          {$PUSH}{$R-}{$Q-}
          FStack.Push(FFrameBase+SLEB128toOrdinal(FData), lseValue);
          {$POP}
        end;

      DW_OP_dup: begin
          if not AssertMinCount(1) then exit;
          FStack.Push(FStack.Peek);
        end;
      DW_OP_drop: begin
          if not AssertMinCount(1) then exit;
          FStack.Pop;
        end;
      DW_OP_over: begin
          if not AssertMinCount(2) then exit;
          FStack.Push(FStack.Peek(1));
        end;
      DW_OP_pick: begin
          i := ReadUnsignedFromExpression(FData, 1);
          if not AssertMinCount(i) then exit;
          FStack.Push(FStack.Peek(i));
        end;
      DW_OP_swap: begin
          if not AssertMinCount(2) then exit;
          Entry := FStack.Peek(0);
          FStack.Modify(0, FStack.Peek(1));
          FStack.Modify(1, Entry);
        end;
      DW_OP_rot: begin
          if not AssertMinCount(3) then exit;
          Entry := FStack.Peek(0);
          FStack.Modify(0, FStack.Peek(1));
          FStack.Modify(1, FStack.Peek(2));
          FStack.Modify(2, Entry);
        end;

      DW_OP_abs: begin
          if not AssertMinCount(1) then exit;
          Entry := FStack.Peek(0);
          FStack.Modify(0, abs(int64(Entry.Value)), Entry.Kind); // treat as signed
        end;
      DW_OP_neg: begin
          if not AssertMinCount(1) then exit;
          Entry := FStack.Peek(0);
          FStack.Modify(0, TDbgPtr(-(int64(Entry.Value))), Entry.Kind); // treat as signed
        end;
      DW_OP_plus: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          Entry2 := FStack.Peek(0);
          {$PUSH}{$R-}{$Q-}
          //TODO: 32 bit overflow?
          FStack.Modify(0, Entry.Value+Entry2.Value, lseValue); // adding signed values works via overflow
          {$POP}
        end;
      DW_OP_plus_uconst: begin
          if not AssertMinCount(1) then exit;
          Entry := FStack.Peek(0);
          {$PUSH}{$R-}{$Q-}
          FStack.Modify(0, Entry.Value+ULEB128toOrdinal(FData), lseValue);
          {$POP}
        end;
      DW_OP_minus: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          Entry2 := FStack.Peek(0);
          {$PUSH}{$R-}{$Q-}
          FStack.Modify(0, Entry2.Value-Entry.Value, lseValue); // adding signed values works via overflow
          {$POP}
        end;
      DW_OP_mul: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          Entry2 := FStack.Peek(0);
          //{$PUSH}{$R-}{$Q-}
          FStack.Modify(0, TDbgPtr(int64(Entry2.Value)*int64(Entry.Value)), lseValue);
          //{$POP}
        end;
      DW_OP_div: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          Entry2 := FStack.Peek(0);
          //{$PUSH}{$R-}{$Q-}
          FStack.Modify(0, TDbgPtr(int64(Entry2.Value) div int64(Entry.Value)), lseValue);
          //{$POP}
        end;
      DW_OP_mod: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          Entry2 := FStack.Peek(0);
          //{$PUSH}{$R-}{$Q-}
          FStack.Modify(0, TDbgPtr(int64(Entry2.Value) mod int64(Entry.Value)), lseValue);
          //{$POP}
        end;

      DW_OP_and: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          Entry2 := FStack.Peek(0);
          FStack.Modify(0, Entry2.Value and Entry.Value, lseValue);
        end;
      DW_OP_not: begin
          if not AssertMinCount(1) then exit;
          Entry := FStack.Peek(0);
          FStack.Modify(0, not Entry2.Value and Entry.Value, Entry.Kind);
        end;
      DW_OP_or: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          Entry2 := FStack.Peek(0);
          FStack.Modify(0, Entry2.Value or Entry.Value, lseValue);
        end;
      DW_OP_xor: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          Entry2 := FStack.Peek(0);
          FStack.Modify(0, Entry2.Value xor Entry.Value, lseValue);
        end;
      DW_OP_shl: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          Entry2 := FStack.Peek(0);
          FStack.Modify(0, Entry2.Value shl Entry.Value, lseValue);
        end;
      DW_OP_shr: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          Entry2 := FStack.Peek(0);
          FStack.Modify(0, Entry2.Value shr Entry.Value, lseValue);
        end;
      DW_OP_shra: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          Entry2 := FStack.Peek(0);
          if Entry.Value > 0 then
            FStack.Modify(0, Entry2.Value div (1 shl (Entry.Value - 1)), lseValue);
        end;

      DW_OP_skip: begin
          x := ReadSignedFromExpression(FData, 2);
          FData := FData + x;
        end;
      DW_OP_bra: begin
          if not AssertMinCount(1) then exit;
          Entry  := FStack.Pop;
          x := ReadSignedFromExpression(FData, 2);
          if Entry.Value <> 0 then
            FData := FData + x;
        end;

      DW_OP_eq: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          Entry2 := FStack.Peek(0);
          if Entry.Value = Entry2.Value
          then FStack.Modify(0, 1, lseValue)
          else FStack.Modify(0, 0, lseValue);
        end;
      DW_OP_ge: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          Entry2 := FStack.Peek(0);
          if int64(Entry.Value) >= int64(Entry2.Value)
          then FStack.Modify(0, 1, lseValue)
          else FStack.Modify(0, 0, lseValue);
        end;
      DW_OP_gt: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          Entry2 := FStack.Peek(0);
          if int64(Entry.Value) > int64(Entry2.Value)
          then FStack.Modify(0, 1, lseValue)
          else FStack.Modify(0, 0, lseValue);
        end;
      DW_OP_le: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          Entry2 := FStack.Peek(0);
          if int64(Entry.Value) <= int64(Entry2.Value)
          then FStack.Modify(0, 1, lseValue)
          else FStack.Modify(0, 0, lseValue);
        end;
      DW_OP_lt: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          Entry2 := FStack.Peek(0);
          if int64(Entry.Value) < int64(Entry2.Value)
          then FStack.Modify(0, 1, lseValue)
          else FStack.Modify(0, 0, lseValue);
        end;
      DW_OP_ne: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          Entry2 := FStack.Peek(0);
          if Entry.Value <> Entry2.Value
          then FStack.Modify(0, 1, lseValue)
          else FStack.Modify(0, 0, lseValue);
        end;

      DW_OP_piece: begin
          if not AssertMinCount(1) then exit; // no piece avail
          FStack.Push(0, lsePart);
          exit;
        end;

(*
  // --- DWARF3 ---
  DW_OP_push_object_address   = $97;    // 0
  DW_OP_call2                 = $98;    // 1 2-byte offset of DIE
  DW_OP_call4                 = $99;    // 1 4-byte offset of DIE
  DW_OP_call_ref              = $9a;    // 1 4- or 8-byte offset of DIE
  DW_OP_form_tls_address      = $9b;    // 0
  DW_OP_call_frame_cfa        = $9c;    // 0
  DW_OP_bit_piece             = $9d;    // 2
*)
      else
        begin
          debugln(['TDwarfLocationExpression.Evaluate UNKNOWN ', p^]);
          SetError;
          exit;
        end;
    end;
  end;
end;

function TDwarfLocationExpression.ResultKind: TDwarfLocationStackEntryKind;
begin
  if FStack.Count > 0 then
    Result := FStack.PeekKind
  else
    Result := lseError;
end;

function TDwarfLocationExpression.ResultData: TDbgPtr;
begin
  if FStack.Count > 0 then
    Result := FStack.Peek.Value
  else
    Result := 0;
end;

{ TDwarfInformationEntry }

procedure TDwarfInformationEntry.ScopeChanged;
begin
  FInformationEntry := FScope.Entry;
  FFlags := [];
  FInformationData := nil;
  if FAbstractOrigin <> nil then
    ReleaseRefAndNil(FAbstractOrigin);
end;

procedure TDwarfInformationEntry.PrepareAbbrev;
begin
  if dieAbbrevValid in FFlags then
    exit;
  FInformationData := FCompUnit.FAbbrevList.FindLe128bFromPointer(FInformationEntry, FAbbrev);
  Include(FFlags, dieAbbrevValid);
end;

function TDwarfInformationEntry.PrepareAbbrevData: Boolean;
var
  AbbrList: TDwarfAbbrevList;
begin
  Result := FAbbrevData <> nil;
  if dieAbbrevDataValid in FFlags then
    exit;
  AbbrList := FCompUnit.FAbbrevList;

  // PrepareAbbrev;
  if not(dieAbbrevValid in FFlags) then
    FInformationData := AbbrList.FindLe128bFromPointer(FInformationEntry, FAbbrev);
  Result := FInformationData <> nil;

  if Result
  then FAbbrevData := AbbrList.EntryPointer[FAbbrev^.index]
  else FAbbrevData := nil;
  FFlags := FFlags + [dieAbbrevValid, dieAbbrevDataValid];
end;

function TDwarfInformationEntry.PrepareAbstractOrigin: Boolean;
var
  FwdInfoPtr: Pointer;
  FwdCompUint: TDwarfCompilationUnit;
begin
  if (dieAbstractOriginValid in FFlags) then begin
    Result := FAbstractOrigin <> nil;
    exit;
  end;
  Assert(dieAbbrevValid in FFlags);
  Assert(dafHasAbstractOrigin in FAbbrev^.flags);
  Include(FFlags, dieAbstractOriginValid);
  if ReadReference(DW_AT_abstract_origin, FwdInfoPtr, FwdCompUint) then begin
    FAbstractOrigin := TDwarfInformationEntry.Create(FwdCompUint, FwdInfoPtr);
    // TODO, check correct tag
    Result := FAbstractOrigin <> nil;
  end
  else
    Result := False;
end;

function TDwarfInformationEntry.GetAbbrevTag: Cardinal;
begin
  PrepareAbbrev;
  if FAbbrev <> nil
  then Result := FAbbrev^.tag
  else Result := 0;
end;

procedure TDwarfInformationEntry.GoParent;
begin
  if not MaybeSearchScope then
    exit;
  FScope.GoParent;
  ScopeChanged;
end;

procedure TDwarfInformationEntry.GoNext;
begin
  if not MaybeSearchScope then
    exit;
  FScope.GoNext;
  ScopeChanged;
end;

procedure TDwarfInformationEntry.GoChild;
begin
  if not MaybeSearchScope then
    exit;
  FScope.GoChild;
  ScopeChanged;
end;

function TDwarfInformationEntry.HasValidScope: Boolean;
begin
  Result := FScope.IsValid;
end;

function TDwarfInformationEntry.SearchScope: Boolean;
var
  l, h, m: Integer;
  lst: TDwarfScopeArray;
begin
  Result := FInformationEntry <> nil;
  if not Result then exit;
  l := 0;
  h := FCompUnit.FScopeList.HighestKnown;
  lst := FCompUnit.FScopeList.List;
  while h > l do begin
    m := (h + l) div 2;
    if lst[m].Entry >= FInformationEntry
    then h := m
    else l := m + 1;
  end;

  Result := lst[h].Entry = FInformationEntry;
  if Result then
    ScopeIndex := h;
//debugln(['TDwarfInformationEntry.SearchScope ', h]);
end;

function TDwarfInformationEntry.MaybeSearchScope: Boolean;
begin
  Result := FScope.IsValid;
  if Result then exit;
  Result := SearchScope;
end;

function TDwarfInformationEntry.AttribIdx(AnAttrib: Cardinal; out
  AInfoPointer: pointer): Integer;
var
  i: Integer;
  AddrSize: Byte;
begin
  if not PrepareAbbrevData then exit(-1);
  AInfoPointer := FInformationData;
  AddrSize := FCompUnit.FAddressSize;
  for i := 0 to FAbbrev^.count - 1 do begin
    if FAbbrevData[i].Attribute = AnAttrib then
      exit(i);
    SkipEntryDataForForm(AInfoPointer, FAbbrevData[i].Form, AddrSize);
  end;
  Result := -1;
end;

function TDwarfInformationEntry.HasAttrib(AnAttrib: Cardinal): Boolean;
var
  i: Integer;
  AddrSize: Byte;
begin
  Result := False;
  if not PrepareAbbrevData then exit;
  for i := 0 to FAbbrev^.count - 1 do
    if FAbbrevData[i].Attribute = AnAttrib then begin
      Result := True;
      exit;
  end;
end;

function TDwarfInformationEntry.GetScopeIndex: Integer;
begin
  Result := FScope.Index;
end;

procedure TDwarfInformationEntry.SetScopeIndex(AValue: Integer);
begin
  if FScope.Index = AValue then
    exit;
  FScope.Index := AValue;
  ScopeChanged;
end;

function TDwarfInformationEntry.GoNamedChild(AName: String): Boolean;
var
  EntryName: PChar;
  s1, s2: String;
begin
  Result := False;
  if AName = '' then
    exit;
  GoChild;
  if not HasValidScope then
    exit;
  s1 := UTF8UpperCase(AName);
  s2 := UTF8LowerCase(AName);
  while HasValidScope do begin
    PrepareAbbrev;
    if not (dafHasName in FAbbrev^.flags) then begin
      GoNext;
      Continue;
    end;
    if not ReadValue(DW_AT_name, EntryName) then begin
      GoNext;
      Continue;
    end;

      if CompareUtf8BothCase(@s1[1], @s2[1], EntryName) then begin
      // TODO: check DW_AT_start_scope;
      DebugLn([FPDBG_DWARF_SEARCH, 'GoNamedChild found ', dbgs(FScope, FCompUnit), DbgSName(Self)]);
      Result := True;
      exit;
    end;

    GoNext;
  end;
end;

function TDwarfInformationEntry.GoNamedChildEx(ANameUpper, AnameLower: PChar): Boolean;
var
  EntryName: PChar;
  InEnum: Boolean;
  ParentScopIdx: Integer;
begin
  Result := False;
  InEnum := False;
  if ANameUpper = nil then
    exit;
  GoChild;
  if not HasValidScope then
    exit;
  while true do begin
    while HasValidScope do begin
      PrepareAbbrev;
      if not (dafHasName in FAbbrev^.flags) then begin
        GoNext;
        Continue;
      end;
      if not ReadValue(DW_AT_name, EntryName) then begin
        GoNext;
        Continue;
      end;

      if CompareUtf8BothCase(ANameUpper, AnameLower, EntryName) then begin
        // TODO: check DW_AT_start_scope;
        DebugLn([FPDBG_DWARF_SEARCH, 'GoNamedChildEx found ', dbgs(FScope, FCompUnit), DbgSName(Self)]);
        Result := True;
        exit;
      end;

      if FAbbrev^.tag = DW_TAG_enumeration_type then begin
        assert(not InEnum, 'nested enum');
        InEnum := True;
        ParentScopIdx := ScopeIndex;
        GoChild;
        Continue;
      end;

      GoNext;
    end;

    if InEnum then begin
      InEnum := False;
      ScopeIndex := ParentScopIdx;
      GoNext;
      continue;
    end;
    break;
  end;
end;

function TDwarfInformationEntry.GoNamedChildEx(AName: String): Boolean;
var
  s1, s2: String;
begin
  Result := False;
  if AName = '' then
    exit;
  s1 := UTF8UpperCase(AName);
  s2 := UTF8LowerCase(AName);
  Result := GoNamedChildEx(@s1[1], @s2[1]);
end;

constructor TDwarfInformationEntry.Create(ACompUnit: TDwarfCompilationUnit;
  AnInformationEntry: Pointer);
begin
  AddReference;
  FCompUnit := ACompUnit;
  FInformationEntry := AnInformationEntry;
  FScope.Init(@FCompUnit.FScopeList);
end;

constructor TDwarfInformationEntry.Create(ACompUnit: TDwarfCompilationUnit;
  AScope: TDwarfScopeInfo);
begin
  AddReference;
  FCompUnit := ACompUnit;
  FScope := AScope;
  ScopeChanged;
end;

destructor TDwarfInformationEntry.Destroy;
begin
  FAbstractOrigin.ReleaseReference;
  inherited Destroy;
end;

function TDwarfInformationEntry.FindNamedChild(AName: String): TDwarfInformationEntry;
begin
  Result := nil;
  if not MaybeSearchScope then
    exit;

  Result := TDwarfInformationEntry.Create(FCompUnit, FScope);
// TODO: parent
  if Result.GoNamedChild(AName) then
    exit;
  ReleaseRefAndNil(Result);
end;

function TDwarfInformationEntry.FindChildByTag(ATag: Cardinal): TDwarfInformationEntry;
var
  Scope: TDwarfScopeInfo;
  AbbrList: TDwarfAbbrevList;
  Abbr: TDwarfAbbrev;
begin
  Result := nil;
  if not MaybeSearchScope then
    exit;

  Scope := FScope.Child;
  while Scope.IsValid do begin
    AbbrList := FCompUnit.FAbbrevList;
    if AbbrList.FindLe128bFromPointer(Scope.Entry, Abbr) <> nil then begin
      if Abbr.tag = ATag then begin
        Result := TDwarfInformationEntry.Create(FCompUnit, Scope);
        exit;
      end;
    end;
    Scope.GoNext;
  end;
end;

function TDwarfInformationEntry.FirstChild: TDwarfInformationEntry;
var
  Scope: TDwarfScopeInfo;
begin
  Result := nil;
  if not MaybeSearchScope then
    exit;

  Scope := FScope.Child;
  if Scope.IsValid then
    Result := TDwarfInformationEntry.Create(FCompUnit, Scope);
end;

function TDwarfInformationEntry.Clone: TDwarfInformationEntry;
begin
  if FScope.IsValid then
    Result := TDwarfInformationEntry.Create(FCompUnit, FScope)
  else
    Result := TDwarfInformationEntry.Create(FCompUnit, FInformationEntry);
end;

function TDwarfInformationEntry.ReadValue(AnAttrib: Cardinal; out AValue: Integer): Boolean;
var
  AData: pointer;
  i: Integer;
begin
  i := AttribIdx(AnAttrib, AData);
  if i < 0 then begin
    if (dafHasAbstractOrigin in FAbbrev^.flags) and PrepareAbstractOrigin
    then Result := FAbstractOrigin.ReadValue(AnAttrib, AValue)
    else Result := False;
  end
  else
    Result := FCompUnit.ReadValue(AData, FAbbrevData[i].Form, AValue);
end;

function TDwarfInformationEntry.ReadValue(AnAttrib: Cardinal; out AValue: Int64): Boolean;
var
  AData: pointer;
  i: Integer;
begin
  i := AttribIdx(AnAttrib, AData);
  if i < 0 then begin
    if (dafHasAbstractOrigin in FAbbrev^.flags) and PrepareAbstractOrigin
    then Result := FAbstractOrigin.ReadValue(AnAttrib, AValue)
    else Result := False;
  end
  else
    Result := FCompUnit.ReadValue(AData, FAbbrevData[i].Form, AValue);
end;

function TDwarfInformationEntry.ReadValue(AnAttrib: Cardinal; out AValue: Cardinal): Boolean;
var
  AData: pointer;
  i: Integer;
begin
  i := AttribIdx(AnAttrib, AData);
  if i < 0 then begin
    if (dafHasAbstractOrigin in FAbbrev^.flags) and PrepareAbstractOrigin
    then Result := FAbstractOrigin.ReadValue(AnAttrib, AValue)
    else Result := False;
  end
  else
    Result := FCompUnit.ReadValue(AData, FAbbrevData[i].Form, AValue);
end;

function TDwarfInformationEntry.ReadValue(AnAttrib: Cardinal; out AValue: QWord): Boolean;
var
  AData: pointer;
  i: Integer;
begin
  i := AttribIdx(AnAttrib, AData);
  if i < 0 then begin
    if (dafHasAbstractOrigin in FAbbrev^.flags) and PrepareAbstractOrigin
    then Result := FAbstractOrigin.ReadValue(AnAttrib, AValue)
    else Result := False;
  end
  else
    Result := FCompUnit.ReadValue(AData, FAbbrevData[i].Form, AValue);
end;

function TDwarfInformationEntry.ReadValue(AnAttrib: Cardinal; out AValue: PChar): Boolean;
var
  AData: pointer;
  i: Integer;
begin
  i := AttribIdx(AnAttrib, AData);
  if i < 0 then begin
    if (dafHasAbstractOrigin in FAbbrev^.flags) and PrepareAbstractOrigin
    then Result := FAbstractOrigin.ReadValue(AnAttrib, AValue)
    else Result := False;
  end
  else
    Result := FCompUnit.ReadValue(AData, FAbbrevData[i].Form, AValue);
end;

function TDwarfInformationEntry.ReadValue(AnAttrib: Cardinal; out AValue: String): Boolean;
var
  AData: pointer;
  i: Integer;
begin
  i := AttribIdx(AnAttrib, AData);
  if i < 0 then begin
    if (dafHasAbstractOrigin in FAbbrev^.flags) and PrepareAbstractOrigin
    then Result := FAbstractOrigin.ReadValue(AnAttrib, AValue)
    else Result := False;
  end
  else
    Result := FCompUnit.ReadValue(AData, FAbbrevData[i].Form, AValue);
end;

function TDwarfInformationEntry.ReadValue(AnAttrib: Cardinal; out
  AValue: TByteDynArray): Boolean;
var
  AData: pointer;
  i: Integer;
begin
  i := AttribIdx(AnAttrib, AData);
  if i < 0 then begin
    if (dafHasAbstractOrigin in FAbbrev^.flags) and PrepareAbstractOrigin
    then Result := FAbstractOrigin.ReadValue(AnAttrib, AValue)
    else Result := False;
  end
  else
    Result := FCompUnit.ReadValue(AData, FAbbrevData[i].Form, AValue);
end;

function TDwarfInformationEntry.ReadReference(AnAttrib: Cardinal; out AValue: Pointer; out
  ACompUnit: TDwarfCompilationUnit): Boolean;
var
  InfoData: pointer;
  i: Integer;
  Form: Cardinal;
  Offs: QWord;
begin
  // reference to other debug info
  {Note: Dwarf2 defines DW_FORM_ref_addr as relocated address in the exe,
         Dwarf 3 defines it as offset.
         Since we load the debug_info section without applying any relocation (if indeed present at all),
         this field will always be an offset from start of the debug_info section
  }
  Result := False;
  i := AttribIdx(AnAttrib, InfoData);
  if i < 0 then begin
    if (dafHasAbstractOrigin in FAbbrev^.flags) and PrepareAbstractOrigin
    then Result := FAbstractOrigin.ReadReference(AnAttrib, AValue, ACompUnit);
    exit;
  end;
  Form := FAbbrevData[i].Form;
  if (Form = DW_FORM_ref1) or (Form = DW_FORM_ref2) or (Form = DW_FORM_ref4) or
     (Form = DW_FORM_ref8) or (Form = DW_FORM_ref_udata)
  then begin
    Result := FCompUnit.ReadValue(InfoData, Form, Offs);
    if not Result then
      exit;
    ACompUnit := FCompUnit;
    if ACompUnit.FIsDwarf64
    then AValue := ACompUnit.FScope.Entry + Offs - SizeOf(TDwarfCUHeader64)
    else AValue := ACompUnit.FScope.Entry + Offs - SizeOf(TDwarfCUHeader32);
  end
  else
  if (Form = DW_FORM_ref_addr) then begin
    Result := FCompUnit.ReadValue(InfoData, Form, Offs);
    if not Result then
      exit;
    ACompUnit := FCompUnit.FOwner.FindCompilationUnitByOffs(Offs);
    Result := ACompUnit <> nil;
    if not Result then DebugLn(FPDBG_DWARF_WARNINGS, ['Comp unit not found DW_FORM_ref_addr']);
    AValue := FCompUnit.FOwner.FSections[dsInfo].RawData + Offs;
  end
  else begin
    DebugLn(FPDBG_DWARF_WARNINGS, ['FORM for DW_AT_type not expected ', DwarfAttributeFormToString(Form)]);
  end;
end;

function TDwarfInformationEntry.ReadName(out AName: String): Boolean;
var
  i: Integer;
  AData: pointer;
begin
  PrepareAbbrev;
  if dafHasName in FAbbrev^.flags then begin
    //Result := ReadValue(DW_AT_name, AName);
    i := AttribIdx(DW_AT_name, AData);
    assert(i >= 0, 'TDwarfInformationEntry.ReadName');
    Result := FCompUnit.ReadValue(AData, FAbbrevData[i].Form, AName);
  end
  else
  if (dafHasAbstractOrigin in FAbbrev^.flags) and PrepareAbstractOrigin then
    Result := FAbstractOrigin.ReadName(AName)
  else
    Result := False;
end;

function TDwarfInformationEntry.ReadName(out AName: PChar): Boolean;
var
  AData: pointer;
  i: Integer;
begin
  PrepareAbbrev;
  if dafHasName in FAbbrev^.flags then begin
    //Result := ReadValue(DW_AT_name, AName);
    i := AttribIdx(DW_AT_name, AData);
    assert(i >= 0, 'TDwarfInformationEntry.ReadName');
    Result := FCompUnit.ReadValue(AData, FAbbrevData[i].Form, AName);
  end
  else
  if (dafHasAbstractOrigin in FAbbrev^.flags) and PrepareAbstractOrigin then
    Result := FAbstractOrigin.ReadName(AName)
  else
    Result := False;
end;

function TDwarfInformationEntry.ReadStartScope(out AStartScope: TDbgPtr): Boolean;
var
  AData: pointer;
  i: Integer;
begin
  PrepareAbbrev;
  if dafHasStartScope in FAbbrev^.flags then begin
    //Result := ReadValue(DW_AT_start_scope, AStartScope)
    i := AttribIdx(DW_AT_start_scope, AData);
    assert(i >= 0, 'TDwarfInformationEntry.ReadStartScope');
    Result := FCompUnit.ReadValue(AData, FAbbrevData[i].Form, AStartScope);
  end
  else
  if (dafHasAbstractOrigin in FAbbrev^.flags) and PrepareAbstractOrigin then
    Result := FAbstractOrigin.ReadStartScope(AStartScope)
  else
    Result := False;
end;

function TDwarfInformationEntry.IsAddressInStartScope(AnAddress: TDbgPtr): Boolean;
var
  StartScope: TDbgPtr;
begin
  Result := not ReadStartScope(StartScope);
  if Result then exit; // no startscope, always in scope
  Result := AnAddress >= StartScope;
end;

function TDwarfInformationEntry.IsArtificial: Boolean;
var
  Val: Integer;
begin
  Result := ReadValue(DW_AT_artificial, Val);
  if Result then Result := Val <> 0;
end;

{ TDbgDwarfUnit }

procedure TDbgDwarfUnit.Init;
begin
  inherited Init;
  SetSymbolType(stNone);
  SetKind(skUnit);
end;

function TDbgDwarfUnit.GetMemberByName(AIndex: String): TDbgSymbol;
var
  Ident: TDwarfInformationEntry;
begin
  // Todo, param to only search external.
  ReleaseRefAndNil(FLastChildByName);
  Result := nil;

  Ident := FInformationEntry.Clone;
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

{ TDbgDwarfIdentifierSubRange }

procedure TDbgDwarfIdentifierSubRange.ReadBounds;
var
  FwdInfoPtr: Pointer;
  FwdCompUint: TDwarfCompilationUnit;
  NewInfo: TDwarfInformationEntry;
begin
  if FLowBoundState <> rfNotRead then exit;

  // Todo: search attrib-IDX only once
  if FInformationEntry.ReadReference(DW_AT_lower_bound, FwdInfoPtr, FwdCompUint) then begin
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
  if FInformationEntry.ReadValue(DW_AT_lower_bound, FLowBoundConst) then begin
    FLowBoundState := rfConst;
  end
  else
  begin
    //FLowBoundConst := 0; // the default
    //FLowBoundState := rfConst;
    FLowBoundState := rfNotFound;
    exit; // incomplete type
  end;


  if FInformationEntry.ReadReference(DW_AT_upper_bound, FwdInfoPtr, FwdCompUint) then begin
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
  if FInformationEntry.ReadValue(DW_AT_upper_bound, FHighBoundConst) then begin
    FHighBoundState := rfConst;
  end
  else
  begin
    FHighBoundState := rfNotFound;

    if FInformationEntry.ReadReference(DW_AT_count, FwdInfoPtr, FwdCompUint) then begin
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
    if FInformationEntry.ReadValue(DW_AT_count, FCountConst) then begin
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
  if FInformationEntry.ReadName(AName) then
    SetName(AName)
  else
    SetName('');
end;

procedure TDbgDwarfIdentifierSubRange.KindNeeded;
var
  t: TDbgSymbol;
begin
// TODO: limit to ordinal types
  if not HasBounds then begin // does ReadBounds;
    SetKind(skNone); // incomplete type
  end;

  t := NestedTypeInfo;
  if t = nil then begin
    SetKind(skInteger);
    SetSize(FCU.FAddressSize);
  end
  else
    SetKind(t.Kind);
end;

procedure TDbgDwarfIdentifierSubRange.SizeNeeded;
var
  t: TDbgSymbol;
begin
  t := NestedTypeInfo;
  if t = nil then begin
    SetKind(skInteger);
    SetSize(FCU.FAddressSize);
  end
  else
    SetSize(t.Size);
end;

function TDbgDwarfIdentifierSubRange.GetFlags: TDbgSymbolFlags;
begin
  Result := (inherited GetFlags) + [sfSubRange];
end;

procedure TDbgDwarfIdentifierSubRange.Init;
begin
  FLowBoundState := rfNotRead;
  FHighBoundState := rfNotRead;
  FCountState := rfNotRead;
  inherited Init;
end;

{ TDbgDwarfIdentifierEnumElement }

procedure TDbgDwarfIdentifierEnumElement.ReadOrdinalValue;
begin
  if FOrdinalValueRead then exit;
  FOrdinalValueRead := True;
  FHasOrdinalValue := FInformationEntry.ReadValue(DW_AT_const_value, FOrdinalValue);
end;

procedure TDbgDwarfIdentifierEnumElement.KindNeeded;
begin
  SetKind(skEnumValue);
end;

function TDbgDwarfIdentifierEnumElement.GetHasOrdinalValue: Boolean;
begin
  ReadOrdinalValue;
  Result := FHasOrdinalValue;
end;

function TDbgDwarfIdentifierEnumElement.GetOrdinalValue: Int64;
begin
  ReadOrdinalValue;
  Result := FOrdinalValue;
end;

procedure TDbgDwarfIdentifierEnumElement.Init;
begin
  FOrdinalValueRead := False;
  inherited Init;
end;

{ TDbgDwarfIdentifierSet }

procedure TDbgDwarfIdentifierSet.KindNeeded;
begin
  SetKind(skSet);
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
  Info := FInformationEntry.FirstChild;
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

procedure TDbgDwarfIdentifierEnum.KindNeeded;
begin
  SetKind(skEnum);
end;

function TDbgDwarfIdentifierEnum.GetMember(AIndex: Integer): TDbgSymbol;
begin
  CreateMembers;
  Result := TDbgSymbol(FMembers[AIndex]);
end;

function TDbgDwarfIdentifierEnum.GetMemberByName(AIndex: String): TDbgSymbol;
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
    Result := TDbgSymbol(FMembers[i]);
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

{ TDbgDwarfTypeIdentifierRef }

function TDbgDwarfTypeIdentifierRef.GetFlags: TDbgSymbolFlags;
begin
  Result := (inherited GetFlags) + [sfInternalRef];
end;

function TDbgDwarfTypeIdentifierRef.GetDataAddress(var AnAddress: TDbgPtr;
  ATargetType: TDbgDwarfTypeIdentifier): Boolean;
var
  Addr4: DWORD;
  Addr8: QWord;
begin
  if ATargetType = Self then begin
    Result := True;
    exit;
  end;
  Result := FCU.FOwner.MemReader <> nil;
  if not Result then
    exit;
  //TODO: zero fill / sign extend
  case FCU.FAddressSize of
    4: begin
        FCU.FOwner.MemReader.ReadMemory(AnAddress, 4, @Addr4);
        AnAddress := Addr4;
      end;
    8: begin
        FCU.FOwner.MemReader.ReadMemory(AnAddress, 8, @Addr8);
        AnAddress := Addr8;
      end;
    else
      Result := False;
  end;
  if Result then
    Result := inherited GetDataAddress(AnAddress, ATargetType);
end;

{ TDbgDwarfTypeIdentifierPointer }

function TDbgDwarfTypeIdentifierPointer.IsInternalDynArrayPointer: Boolean;
var
  ti: TDbgSymbol;
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
begin
  if IsInternalPointer then begin
    SetForwardToSymbol(NestedTypeInfo);
    inherited KindNeeded;
  end
  else
    SetKind(skPointer);
end;

procedure TDbgDwarfTypeIdentifierPointer.ForwardToSymbolNeeded;
begin
  if IsInternalPointer then
    SetForwardToSymbol(NestedTypeInfo) // Same as TypeInfo, but does not try to be forwarded
  else
    SetForwardToSymbol(nil); // inherited ForwardToSymbolNeeded;
end;

function TDbgDwarfTypeIdentifierPointer.GetDataAddress(var AnAddress: TDbgPtr;
  ATargetType: TDbgDwarfTypeIdentifier): Boolean;
var
  Addr4: DWORD;
  Addr8: QWord;
begin
  if ATargetType = Self then begin
    Result := True;
    exit;
  end;
  Result := FCU.FOwner.MemReader <> nil;
  if not Result then
    exit;
DebugLnEnter(['>>> POINTER  TDbgDwarfTypeIdentifierPointer.GetDataAddress ']);
  //TODO: zero fill / sign extend
  case FCU.FAddressSize of
    4: begin
        FCU.FOwner.MemReader.ReadMemory(AnAddress, 4, @Addr4);
        AnAddress := Addr4;
      end;
    8: begin
        FCU.FOwner.MemReader.ReadMemory(AnAddress, 8, @Addr8);
        AnAddress := Addr8;
      end;
    else
      Result := False;
  end;
  if Result then
    Result := inherited GetDataAddress(AnAddress, ATargetType);
DebugLnExit(['<<< POINTER  TDbgDwarfTypeIdentifierPointer.GetDataAddress ']);
end;

function TDbgDwarfTypeIdentifierPointer.GetTypedValueObject(ATypeCast: Boolean): TDbgDwarfSymbolValue;
begin
  if IsInternalPointer then
    Result := NestedTypeInfo.GetTypedValueObject(ATypeCast)
  else
    Result := TDbgDwarfPointerSymbolValue.Create(FCU.FOwner.FMemReader, FCU.FAddressSize);
end;

{ TDbgDwarfTypeIdentifierDeclaration }

function TDbgDwarfTypeIdentifierDeclaration.DoGetNestedTypeInfo: TDbgDwarfTypeIdentifier;
var
  ti: TDbgDwarfTypeIdentifier;
  ti2: TDbgSymbol;
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

function TDbgDwarfTypeIdentifierDeclaration.GetTypedValueObject(ATypeCast: Boolean): TDbgDwarfSymbolValue;
var
  ti: TDbgDwarfTypeIdentifier;
begin
  ti := NestedTypeInfo;
  if ti <> nil then
    Result := ti.GetTypedValueObject(ATypeCast)
  else
    Result := inherited;
end;

{ TDbgDwarfValueIdentifier }

function TDbgDwarfValueIdentifier.GetDataAddress(out AnAddress: TDbgPtr;
  ATargetType: TDbgDwarfTypeIdentifier): Boolean;
begin
  Result := TypeInfo <> nil;
  if not Result then
    exit;
  Assert(TypeInfo is TDbgDwarfTypeIdentifier, 'TDbgDwarfValueIdentifier.GetDataAddress');
  AnAddress := Address;
DebugLnEnter(['>>> TDbgDwarfValueIdentifier.GetDataAddress ', IntToHex(AnAddress,8)]);
  Result := TDbgDwarfTypeIdentifier(TypeInfo).GetDataAddress(AnAddress, ATargetType);
DebugLnExit(['<<< TDbgDwarfValueIdentifier.GetDataAddress ']);
end;

procedure TDbgDwarfValueIdentifier.KindNeeded;
var
  t: TDbgSymbol;
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

function TDbgDwarfValueIdentifier.GetMember(AIndex: Integer): TDbgSymbol;
var
  ti: TDbgSymbol;
begin
  ti := TypeInfo;
  if ti <> nil then
    Result := ti.Member[AIndex]
  else
    Result := inherited GetMember(AIndex);

  if (Result <> nil) and (Result is TDbgDwarfIdentifierMember) then begin
    if FMembers = nil then
      FMembers := TFpDbgCircularRefCntObjList.Create;
    FMembers.Add(Result);
    TDbgDwarfIdentifierMember(Result).StructureValueInfo := Self;
  end;
end;

function TDbgDwarfValueIdentifier.GetMemberByName(AIndex: String): TDbgSymbol;
var
  ti: TDbgSymbol;
begin
  ti := TypeInfo;
  if ti <> nil then
    Result := ti.MemberByName[AIndex]
  else
    Result := inherited GetMemberByName(AIndex);

  if (Result <> nil) and (Result is TDbgDwarfIdentifierMember) then begin
    if FMembers = nil then
      FMembers := TFpDbgCircularRefCntObjList.Create;
    FMembers.Add(Result);
    TDbgDwarfIdentifierMember(Result).StructureValueInfo := Self;
  end;
end;

function TDbgDwarfValueIdentifier.GetMemberCount: Integer;
var
  ti: TDbgSymbol;
begin
  ti := TypeInfo;
  if ti <> nil then
    Result := ti.MemberCount
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
  if FMembers <> nil then
    for i := 0 to FMembers.Count - 1 do
      TDbgDwarfIdentifierMember(FMembers[i]).StructureValueInfo := nil;
  FreeAndNil(FMembers);
  if FValueObject <> nil then begin
    FValueObject.SetOwner(nil);
    ReleaseRefAndNil(FValueObject);
  end;
  ParentTypeInfo := nil;
  inherited Destroy;
end;

class function TDbgDwarfValueIdentifier.CreateValueSubClass(AName: String;
  AnInformationEntry: TDwarfInformationEntry): TDbgDwarfValueIdentifier;
var
  c: TDbgDwarfIdentifierClass;
begin
  c := GetSubClass(AnInformationEntry.AbbrevTag);

  if c.InheritsFrom(TDbgDwarfValueIdentifier) then
    Result := TDbgDwarfValueIdentifierClass(c).Create(AName, AnInformationEntry)
  else
    Result := nil;
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

  Info := FInformationEntry.FirstChild;
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

procedure TDbgDwarfIdentifierArray.KindNeeded;
begin
  SetKind(skArray); // Todo: static/dynamic?
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
  m: TDbgSymbol;
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

function TDbgDwarfIdentifierArray.GetMember(AIndex: Integer): TDbgSymbol;
begin
  CreateMembers;
  Result := TDbgSymbol(FMembers[AIndex]);
end;

function TDbgDwarfIdentifierArray.GetMemberByName(AIndex: String): TDbgSymbol;
begin
  Result := nil; // no named members
end;

function TDbgDwarfIdentifierArray.GetMemberCount: Integer;
begin
  CreateMembers;
  Result := FMembers.Count;
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

{ TDbgDwarfIdentifierMember }

procedure TDbgDwarfIdentifierMember.SetStructureValueInfo(AValue: TDbgSymbolBase);
begin
  if FStructureValueInfo = AValue then Exit;

  if (FStructureValueInfo <> nil) and CircleBackRefsActive then
    FStructureValueInfo.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FStructureValueInfo, 'FStructureValueInfo'){$ENDIF};

  FStructureValueInfo := AValue;

  if (FStructureValueInfo <> nil) and CircleBackRefsActive then
    FStructureValueInfo.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FStructureValueInfo, 'FStructureValueInfo'){$ENDIF};
end;

procedure TDbgDwarfIdentifierMember.InitLocationParser(const ALocationParser: TDwarfLocationExpression;
  AnObjectDataAddress: TDbgPtr);
var
  BaseAddr: TDbgPtr;
begin
DebugLnEnter(['>>> TDbgDwarfIdentifierMember.InitLocationParser ',Self.Name]);
  inherited InitLocationParser(ALocationParser, AnObjectDataAddress);

  if (StructureValueInfo <> nil) and (ParentTypeInfo <> nil) then begin
DebugLn(['TDbgDwarfIdentifierMember.InitLocationParser AAA']);
    Assert(ParentTypeInfo is TDbgDwarfTypeIdentifier, '');

    if StructureValueInfo is TDbgDwarfValueIdentifier then begin
      if TDbgDwarfValueIdentifier(StructureValueInfo).GetDataAddress(BaseAddr, TDbgDwarfTypeIdentifier(ParentTypeInfo)) then begin
        ALocationParser.FStack.Push(BaseAddr, lseValue);
  DebugLnExit(['<<< TDbgDwarfIdentifierMember.InitLocationParser GOOD ', BaseAddr,'  ',IntToHex(BaseAddr,8)]);
        exit
      end;
    end;
    if StructureValueInfo is TDbgDwarfStructTypeCastSymbolValue then begin
      if TDbgDwarfStructTypeCastSymbolValue(StructureValueInfo).GetDwarfDataAddress(BaseAddr, TDbgDwarfTypeIdentifier(ParentTypeInfo)) then begin
        ALocationParser.FStack.Push(BaseAddr, lseValue);
  DebugLnExit(['<<< TDbgDwarfIdentifierMember.InitLocationParser GOOD ', BaseAddr,'  ',IntToHex(BaseAddr,8)]);
        exit
      end;
    end;

  end;

  //TODO: error
  debugln(['TDbgDwarfIdentifierMember.InitLocationParser FAILED']);
DebugLnExit(['<<< TDbgDwarfIdentifierMember.InitLocationParser ']);
end;

procedure TDbgDwarfIdentifierMember.AddressNeeded;
var
  t: TDbgPtr;
begin
DebugLnEnter(['>>> TDbgDwarfIdentifierMember.AddressNeeded ']);
  if LocationFromTag(DW_AT_data_member_location, t) then
    SetAddress(t)
  else
    SetAddress(0);
DebugLnExit(['<<< ',t]);
end;

function TDbgDwarfIdentifierMember.HasAddress: Boolean;
begin
  Result := (FStructureValueInfo <> nil) and
            (  (  (FStructureValueInfo is TDbgDwarfIdentifier) and
                  (TDbgDwarfIdentifier(FStructureValueInfo).HasAddress) ) or
               (  (FStructureValueInfo is TDbgSymbolValue) and
                  (svfAddress in TDbgSymbolValue(FStructureValueInfo).FieldFlags) )
               ) and
            FInformationEntry.HasAttrib(DW_AT_data_member_location);
end;

procedure TDbgDwarfIdentifierMember.CircleBackRefActiveChanged(ANewActive: Boolean);
begin
  inherited;
  if (FStructureValueInfo = nil) then
    exit;
  if ANewActive then
    FStructureValueInfo.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FStructureValueInfo, 'FStructureValueInfo'){$ENDIF}
  else
    FStructureValueInfo.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FStructureValueInfo, 'FStructureValueInfo'){$ENDIF};
end;

procedure TDbgDwarfIdentifierMember.SetParentTypeInfo(AValue: TDbgDwarfIdentifier);
begin
  if AValue <> ParentTypeInfo then
    SetStructureValueInfo(nil);
  inherited SetParentTypeInfo(AValue);
end;

destructor TDbgDwarfIdentifierMember.Destroy;
begin
  Assert(not CircleBackRefsActive, 'CircleBackRefsActive can not be is ddestructor');
  // FStructureValueInfo := nil;
  inherited Destroy;
end;

{ TDbgDwarfIdentifierStructure }

function TDbgDwarfIdentifierStructure.GetMemberByName(AIndex: String): TDbgSymbol;
var
  Ident: TDwarfInformationEntry;
  ti: TDbgSymbol;
begin
  // Todo, maybe create all children?
  if FLastChildByName <> nil then begin
    FLastChildByName.ParentTypeInfo := nil;
    FLastChildByName.ReleaseCirclularReference;
    FLastChildByName := nil;
  end;
  Result := nil;

  Ident := FInformationEntry.FindNamedChild(AIndex);
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

procedure TDbgDwarfIdentifierStructure.InitLocationParser(const ALocationParser: TDwarfLocationExpression;
  AnObjectDataAddress: TDbgPtr);
begin
  inherited InitLocationParser(ALocationParser, AnObjectDataAddress);

  // CURRENTLY ONLY USED for DW_AT_data_member_location
  if AnObjectDataAddress <> 0 then begin
    debugln(['TDbgDwarfIdentifierMember.InitLocationParser ', AnObjectDataAddress]);
    ALocationParser.FStack.Push(AnObjectDataAddress, lseValue);
    exit
  end;

  //TODO: error
  debugln(['TDbgDwarfIdentifierMember.InitLocationParser FAILED']);
end;

function TDbgDwarfIdentifierStructure.GetDataAddress(var AnAddress: TDbgPtr;
  ATargetType: TDbgDwarfTypeIdentifier): Boolean;
var
  t: TDbgPtr;
begin
  if ATargetType = Self then begin
    Result := True;
    exit;
  end;

DebugLnEnter(['>>> STRUCT TDbgDwarfIdentifierStructure.GetDataAddress ']); try
  InitInheritanceInfo;
DebugLn([DbgSName(FInheritanceInfo)]);

  //TODO: may be a constant // offset
  Result := LocationFromTag(DW_AT_data_member_location, t, AnAddress, FInheritanceInfo);
  if not Result then
    exit;

debugln(['TDbgDwarfIdentifierStructure.GetDataAddress ', IntToHex(AnAddress,8), ' new ',IntToHex(t,8) ]);
  AnAddress := t;
  Result := inherited GetDataAddress(AnAddress, ATargetType);
finally DebugLnExit(['>>> STRUCT TDbgDwarfIdentifierStructure.GetDataAddress ']);end;
end;

function TDbgDwarfIdentifierStructure.GetMember(AIndex: Integer): TDbgSymbol;
begin
  CreateMembers;
  Result := TDbgSymbol(FMembers[AIndex]);
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
  Info := FInformationEntry.Clone;
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
    FInheritanceInfo := FInformationEntry.FindChildByTag(DW_TAG_inheritance);
end;

procedure TDbgDwarfIdentifierStructure.KindNeeded;
begin
  if (FInformationEntry.AbbrevTag = DW_TAG_class_type) then
    SetKind(skClass)
  else
  begin
    if TypeInfo <> nil then
      SetKind(skClass)
    else
    if MemberByName['_vptr$TOBJECT'] <> nil then
      SetKind(skClass)
    else
      SetKind(skRecord);
  end;
end;

procedure TDbgDwarfIdentifierStructure.TypeInfoNeeded;
var
  FwdInfoPtr: Pointer;
  FwdCompUint: TDwarfCompilationUnit;
  ti: TDbgDwarfIdentifier;
  ParentInfo: TDwarfInformationEntry;
begin
  ti:= nil;
  InitInheritanceInfo;
  if (FInheritanceInfo <> nil) and
     FInheritanceInfo.ReadReference(DW_AT_type, FwdInfoPtr, FwdCompUint)
  then begin
    ParentInfo := TDwarfInformationEntry.Create(FwdCompUint, FwdInfoPtr);
    DebugLn(FPDBG_DWARF_SEARCH, ['Inherited from ', dbgs(ParentInfo.FInformationEntry, FwdCompUint) ]);
    ti := TDbgDwarfIdentifier.CreateSubClass('', ParentInfo);
    ParentInfo.ReleaseReference;
  end;
  SetTypeInfo(ti);
  ti.ReleaseReference;
end;

function TDbgDwarfIdentifierStructure.GetTypedValueObject(ATypeCast: Boolean): TDbgDwarfSymbolValue;
begin
  if ATypeCast then
    Result := TDbgDwarfStructTypeCastSymbolValue.Create(FCU.FOwner.FMemReader)
  else
    Result := TDbgDwarfStructSymbolValue.Create(FCU.FOwner.FMemReader);
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

{ TDbgDwarfBaseTypeIdentifier }

procedure TDbgDwarfBaseIdentifierBase.KindNeeded;
var
  Encoding, ByteSize: Integer;
begin
  if not FInformationEntry.ReadValue(DW_AT_encoding, Encoding) then begin
    DebugLn(FPDBG_DWARF_WARNINGS, ['TDbgDwarfBaseIdentifierBase.KindNeeded: Failed reading encoding for ', DwarfTagToString(FInformationEntry.AbbrevTag)]);
    inherited KindNeeded;
    exit;
  end;

  if FInformationEntry.ReadValue(DW_AT_byte_size, ByteSize) then
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
        DebugLn(FPDBG_DWARF_WARNINGS, ['TDbgDwarfBaseIdentifierBase.KindNeeded: Unknown encoding ', DwarfBaseTypeEncodingToString(Encoding), ' for ', DwarfTagToString(FInformationEntry.AbbrevTag)]);
        inherited KindNeeded;
      end;
  end;
end;

procedure TDbgDwarfBaseIdentifierBase.SizeNeeded;
var
  ByteSize: Integer;
begin
  if FInformationEntry.ReadValue(DW_AT_byte_size, ByteSize) then
    SetSize(ByteSize)
  else
    inherited SizeNeeded;
end;

procedure TDbgDwarfBaseIdentifierBase.TypeInfoNeeded;
begin
  SetTypeInfo(nil);
end;

function TDbgDwarfBaseIdentifierBase.GetTypedValueObject(ATypeCast: Boolean): TDbgDwarfSymbolValue;
begin
  case Kind of
    skPointer:  Result := TDbgDwarfPointerSymbolValue.Create(FCU.FOwner.FMemReader, Size);
    skInteger:  Result := TDbgDwarfIntegerSymbolValue.Create(FCU.FOwner.FMemReader, Size);
    skCardinal: Result := TDbgDwarfCardinalSymbolValue.Create(FCU.FOwner.FMemReader, Size);
    skBoolean:  Result := TDbgDwarfBooleanSymbolValue.Create(FCU.FOwner.FMemReader, Size);
    skChar:     Result := TDbgDwarfCharSymbolValue.Create(FCU.FOwner.FMemReader, Size);
    skFloat:    Result := TDbgDwarfFloatSymbolValue.Create(FCU.FOwner.FMemReader, Size);
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

function TDbgDwarfTypeIdentifier.GetTypedValueObject(ATypeCast: Boolean): TDbgDwarfSymbolValue;
begin
  Result := nil;
end;

class function TDbgDwarfTypeIdentifier.CreateTypeSubClass(AName: String;
  AnInformationEntry: TDwarfInformationEntry): TDbgDwarfTypeIdentifier;
var
  c: TDbgDwarfIdentifierClass;
begin
  c := GetSubClass(AnInformationEntry.AbbrevTag);

  if c.InheritsFrom(TDbgDwarfTypeIdentifier) then
    Result := TDbgDwarfTypeIdentifierClass(c).Create(AName, AnInformationEntry)
  else
    Result := nil;
end;

function TDbgDwarfTypeIdentifier.TypeCastValue(AValue: TDbgSymbolValue): TDbgSymbolValue;
begin
  Result := GetTypedValueObject(True);
  If Result = nil then
    exit;
  assert(Result is TDbgDwarfSymbolValue);
  if not TDbgDwarfSymbolValue(Result).SetTypeCastInfo(self, AValue) then
    ReleaseRefAndNil(Result);
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
  if FInformationEntry.ReadReference(DW_AT_type, FwdInfoPtr, FwdCompUint) then begin
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
  Result := FInformationEntry.ReadValue(DW_AT_external, Val);
  if Result and (Val <> 0) then begin
    AMemberVisibility := svPublic;
    exit;
  end;

  Result := FInformationEntry.ReadValue(DW_AT_accessibility, Val);
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
    if FInformationEntry.IsArtificial then
      Include(FDwarfReadFlags, didtIsArtifical);
    Include(FDwarfReadFlags, didtArtificialRead);
  end;
  Result := didtIsArtifical in FDwarfReadFlags;
end;

procedure TDbgDwarfIdentifier.NameNeeded;
var
  AName: String;
begin
  if FInformationEntry.ReadName(AName) then
    SetName(AName)
  else
    inherited NameNeeded;
end;

procedure TDbgDwarfIdentifier.TypeInfoNeeded;
begin
  SetTypeInfo(NestedTypeInfo);
end;

procedure TDbgDwarfIdentifier.InitLocationParser(const ALocationParser: TDwarfLocationExpression;
  AnObjectDataAddress: TDbgPtr);
begin
end;

function TDbgDwarfIdentifier.LocationFromTag(ATag: Cardinal; out AnAddress: TDbgPtr;
  AnObjectDataAddress: TDbgPtr; AnInformationEntry: TDwarfInformationEntry): Boolean;
var
  Val: TByteDynArray;
  LocationParser: TDwarfLocationExpression;
begin
DebugLnEnter('>>>');
  debugln(['TDbgDwarfIdentifier.LocationFromTag', ClassName, '  ',Name, '  ', DwarfAttributeToString(ATag)]);

  Result := False;
  if AnInformationEntry = nil then
    AnInformationEntry := FInformationEntry;

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

  LocationParser := TDwarfLocationExpression.Create(@Val[0], Length(Val), FCU);
  InitLocationParser(LocationParser, AnObjectDataAddress);
  LocationParser.Evaluate;

  if LocationParser.ResultKind in [lseValue] then begin
    AnAddress := LocationParser.ResultData;
    Result := True;
  end;

  LocationParser.Free;
DebugLnExit('<<<', IntToHex(AnAddress,8));
end;

function TDbgDwarfIdentifier.GetDataAddress(var AnAddress: TDbgPtr;
  ATargetType: TDbgDwarfTypeIdentifier): Boolean;
var
  ti: TDbgDwarfTypeIdentifier;
begin
debugln(['TDbgDwarfIdentifier.GetDataAddress ',DbgSName(Self), '  targ ',DbgSName(ATargetType)]);
  if ATargetType = Self then begin
    Result := True;
  end
  else begin
    ti := NestedTypeInfo;
    if ti <> nil then
      Result := NestedTypeInfo.GetDataAddress(AnAddress, ATargetType)
    else
      Result := True; // end of type chain
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

class function TDbgDwarfIdentifier.GetSubClass(ATag: Cardinal): TDbgDwarfIdentifierClass;
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
    DW_TAG_enumerator:       Result := TDbgDwarfIdentifierEnumElement;
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

class function TDbgDwarfIdentifier.CreateSubClass(AName: String;
  AnInformationEntry: TDwarfInformationEntry): TDbgDwarfIdentifier;
begin
  Result := GetSubClass(AnInformationEntry.AbbrevTag).Create(AName, AnInformationEntry);
end;

constructor TDbgDwarfIdentifier.Create(AName: String;
  AnInformationEntry: TDwarfInformationEntry);
begin
  FCU := AnInformationEntry.CompUnit;
  FInformationEntry := AnInformationEntry;
  FInformationEntry.AddReference;

  inherited Create(AName);
  Init;
end;

constructor TDbgDwarfIdentifier.Create(AName: String;
  AnInformationEntry: TDwarfInformationEntry; AKind: TDbgSymbolKind; AAddress: TDbgPtr);
begin
  FCU := AnInformationEntry.CompUnit;
  FInformationEntry := AnInformationEntry;
  FInformationEntry.AddReference;

  inherited Create(AName, AKind, AAddress);
  Init;
end;

destructor TDbgDwarfIdentifier.Destroy;
begin
  inherited Destroy;
  ReleaseRefAndNil(FInformationEntry);
  ReleaseRefAndNil(FNestedTypeInfo);
  Assert(not CircleBackRefsActive, 'CircleBackRefsActive can not be is ddestructor');
  // FParentTypeInfo := nil
end;

function TDbgDwarfIdentifier.StartScope: TDbgPtr;
begin
  if not FInformationEntry.ReadStartScope(Result) then
    Result := 0;
end;

{ TDWarfLineMap }

procedure TDWarfLineMap.Init;
begin
  NextAFterHighestLine := 0;
  //Count := 0;
end;

procedure TDWarfLineMap.SetAddressForLine(ALine: Cardinal; AnAddress: QWord);
var
  i: Integer;
begin
  i := Length(AddressList);
  if i <= ALine then
    SetLength(AddressList, ALine + 2000);

  if AddressList[ALine] = 0 then begin
    AddressList[ALine] := AnAddress;
    //inc(Count);
  end;
  if ALine > NextAFterHighestLine then
    NextAFterHighestLine := ALine+1;
end;

function TDWarfLineMap.GetAddressForLine(ALine: Cardinal): QWord;
begin
  Result := 0;
  if ALine < Length(AddressList) then
    Result := AddressList[ALine];
end;

procedure TDWarfLineMap.Compress;
begin
  SetLength(AddressList, NextAFterHighestLine);
//DebugLn(['#### ',NextAFterHighestLine, ' / ',Count]);
end;

{ TDbgDwarfSymbol }

constructor TDbgDwarfProcSymbol.Create(ACompilationUnit: TDwarfCompilationUnit; AInfo: PDwarfAddressInfo; AAddress: TDbgPtr);
var
  InfoEntry: TDwarfInformationEntry;
begin
  FAddress := AAddress;
  FAddressInfo := AInfo;
  
  FCU := ACompilationUnit;

  InfoEntry := TDwarfInformationEntry.Create(FCU, nil);
  InfoEntry.ScopeIndex := AInfo^.ScopeIndex;

  inherited Create(
    String(FAddressInfo^.Name),
    InfoEntry
  );

  SetAddress(FAddressInfo^.StartPC);

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
    FCU.BuildLineInfo(FAddressInfo, False);
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
  Result := FInformationEntry.ReadValue(DW_AT_virtuality, Val);
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
DebugLnEnter(['>> TDbgDwarfProcSymbol.GetFrameBase ']); try
  if FFrameBaseParser = nil then begin
    //TODO: avoid copying data
    if not  FInformationEntry.ReadValue(DW_AT_frame_base, Val) then begin
      // error
      DebugLn('failed to read DW_AT_frame_base');
      exit;
    end;
    if Length(Val) = 0 then begin
      // error
      DebugLn('failed to read DW_AT_location');
      exit;
    end;

    FFrameBaseParser := TDwarfLocationExpression.Create(@Val[0], Length(Val), FCU);
    FFrameBaseParser.Evaluate;

    if FFrameBaseParser.ResultKind in [lseValue] then
      Result := FFrameBaseParser.ResultData;
  end;
finally DebugLnExit(['<< TDbgDwarfProcSymbol.GetFrameBase ']); end;
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
var
  InfoEntry: TDwarfInformationEntry;
  tg: Cardinal;
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
    if InfoEntry.GoNamedChildEx(@this1[1], @this2[1]) then begin
      if ((AnAddress = 0) or InfoEntry.IsAddressInStartScope(AnAddress)) and
         InfoEntry.IsArtificial
      then begin
        Result := TDbgDwarfValueIdentifier.CreateValueSubClass('self', InfoEntry);
        FSelfParameter := Result;
        {$IFDEF WITH_REFCOUNT_DEBUG}FSelfParameter.DbgRenameReference(@FSelfParameter, 'FSelfParameter');{$ENDIF}
        //FSelfParameter.ParentTypeInfo := Self;
        debugln(['TDbgDwarfProcSymbol.GetSelfParameter ', dbgs(InfoEntry.FScope, FCU), DbgSName(Result)]);
      end;
    end;
  end;
  InfoEntry.ReleaseReference;
end;

{ TDbgDwarf }

constructor TDbgDwarf.Create(ALoader: TDbgImageLoader);
var
  Section: TDwarfSection;
  p: PDbgImageSection;
begin
  inherited Create(ALoader);
  FCompilationUnits := TList.Create;
  FImageBase := ALoader.ImageBase;
  for Section := Low(Section) to High(Section) do
  begin
    p := ALoader.Section[DWARF_SECTION_NAME[Section]];
    if p = nil then Continue;
    FSections[Section].Section := Section;
    FSections[Section].RawData := p^.RawData;
    FSections[Section].Size := p^.Size;
    FSections[Section].VirtualAdress := p^.VirtualAdress;
  end;
end;

destructor TDbgDwarf.Destroy;
var
  n: integer;
begin
  for n := 0 to FCompilationUnits.Count - 1 do
    TObject(FCompilationUnits[n]).Free;
  FreeAndNil(FCompilationUnits);
  inherited Destroy;
end;

function TDbgDwarf.FindContext(AAddress: TDbgPtr): TDbgInfoAddressContext;
var
  Proc: TDbgSymbol;
begin
  Result := nil;
  Proc := FindProcSymbol(AAddress);
  if Proc = nil then
    exit;

  Result := TDbgDwarfInfoAddressContext.Create(AAddress, Proc, Self);
  Proc.ReleaseReference;
end;

function TDbgDwarf.FindSymbol(AAddress: TDbgPtr): TDbgSymbol;
begin
  Result := FindProcSymbol(AAddress);
end;

function TDbgDwarf.GetCompilationUnit(AIndex: Integer): TDwarfCompilationUnit;
begin
  Result := TDwarfCompilationUnit(FCompilationUnits[Aindex]);
end;

function TDbgDwarf.GetCompilationUnitClass: TDwarfCompilationUnitClass;
begin
  Result := TDwarfCompilationUnit;
end;

function TDbgDwarf.FindCompilationUnitByOffs(AOffs: QWord): TDwarfCompilationUnit;
var
  l, h, m: Integer;
  p: Pointer;
begin
  Result := nil;
  p := FSections[dsInfo].RawData + AOffs;
  l := 0;
  h := FCompilationUnits.Count - 1;
  while h > l do begin
    m := (h + l + 1) div 2;
    if TDwarfCompilationUnit(FCompilationUnits[m]).FInfoData <= p
    then l := m
    else h := m - 1;
  end;

  Result := TDwarfCompilationUnit(FCompilationUnits[m]);
  if (p < Result.FInfoData) or (p > Result.FInfoData + Result.FLength) then
    Result := nil;
end;

function TDbgDwarf.FindProcSymbol(AAddress: TDbgPtr): TDbgSymbol;
var
  n: Integer;
  CU: TDwarfCompilationUnit;
  Iter: TMapIterator;
  Info: PDwarfAddressInfo;
  MinMaxSet: boolean;
begin
  Result := nil;
  for n := 0 to FCompilationUnits.Count - 1 do
  begin
    CU := TDwarfCompilationUnit(FCompilationUnits[n]);
    if not CU.Valid then Continue;
    MinMaxSet := CU.FMinPC <> CU.FMaxPC;
    if MinMaxSet and ((AAddress < CU.FMinPC) or (AAddress > CU.FMaxPC))
    then Continue;

    CU.BuildAddressMap;

    Iter := TMapIterator.Create(CU.FAddressMap);
    try
      if Iter.EOM
      then begin
        if MinMaxSet
        then Exit //  minmaxset and no procs defined ???
        else Continue;
      end;

      if not Iter.Locate(AAddress)
      then begin
        if not Iter.BOM
        then Iter.Previous;

        if Iter.BOM
        then begin
          if MinMaxSet
          then Exit //  minmaxset and no proc @ minpc ???
          else Continue;
        end;
      end;

      // iter is at the closest defined adress before AAddress
      Info := Iter.DataPtr;
      if AAddress > Info^.EndPC
      then begin
        if MinMaxSet
        then Exit //  minmaxset and no proc @ maxpc ???
        else Continue;
      end;

      Result := TDbgDwarfProcSymbol.Create(CU, Iter.DataPtr, AAddress);
    finally
      Iter.Free;
    end;
  end;
end;

function TDbgDwarf.GetLineAddress(const AFileName: String; ALine: Cardinal): TDbgPtr;
var
  n: Integer;
  CU: TDwarfCompilationUnit;
begin
  for n := 0 to FCompilationUnits.Count - 1 do
  begin
    CU := TDwarfCompilationUnit(FCompilationUnits[n]);
    Result := CU.GetLineAddress(AFileName, ALine);
    if Result <> 0 then Exit;
  end;
  Result := 0;
end;

function TDbgDwarf.GetLineAddressMap(const AFileName: String): PDWarfLineMap;
var
  n: Integer;
  CU: TDwarfCompilationUnit;
begin
  // TODO: Deal with line info split on 2 compilation units?
  for n := 0 to FCompilationUnits.Count - 1 do
  begin
    CU := TDwarfCompilationUnit(FCompilationUnits[n]);
    Result := CU.GetLineAddressMap(AFileName);
    if Result <> nil then Exit;
  end;
  Result := nil;
end;

function TDbgDwarf.LoadCompilationUnits: Integer;
var
  p, pe: Pointer;
  CU32: PDwarfCUHeader32 absolute p;
  CU64: PDwarfCUHeader64 absolute p;
  CU: TDwarfCompilationUnit;
  CUClass: TDwarfCompilationUnitClass;
  inf: TDwarfSectionInfo;
begin
  CUClass := GetCompilationUnitClass;
  inf := FSections[dsInfo];
  p := FSections[dsInfo].RawData;
  pe := inf.RawData + inf.Size;
  while (p <> nil) and (p < pe) do
  begin
    if CU64^.Signature = DWARF_HEADER64_SIGNATURE
    then begin
      if CU64^.Version < 3 then
        DebugLn(FPDBG_DWARF_WARNINGS, ['Unexpected 64 bit signature found for DWARF version 2']); // or version 1...
      CU := CUClass.Create(
              Self,
              PtrUInt(CU64 + 1) - PtrUInt(FSections[dsInfo].RawData),
              CU64^.Length - SizeOf(CU64^) + SizeOf(CU64^.Signature) + SizeOf(CU64^.Length),
              CU64^.Version,
              CU64^.AbbrevOffset,
              CU64^.AddressSize,
              True);
      p := Pointer(@CU64^.Version) + CU64^.Length;
    end
    else begin
      if CU32^.Length = 0 then Break;
      CU := CUClass.Create(
              Self,
              PtrUInt(CU32 + 1) - PtrUInt(FSections[dsInfo].RawData),
              CU32^.Length - SizeOf(CU32^) + SizeOf(CU32^.Length),
              CU32^.Version,
              CU32^.AbbrevOffset,
              CU32^.AddressSize,
              False);
      p := Pointer(@CU32^.Version) + CU32^.Length;
    end;
    FCompilationUnits.Add(CU);
    if CU.Valid then SetHasInfo;
  end;
  Result := FCompilationUnits.Count;
end;

function TDbgDwarf.PointerFromRVA(ARVA: QWord): Pointer;
begin
  Result := Pointer(PtrUInt(FImageBase + ARVA));
end;

function TDbgDwarf.PointerFromVA(ASection: TDwarfSection; AVA: QWord): Pointer;
begin
  Result := FSections[ASection].RawData + AVA - FImageBase - FSections[ASection].VirtualAdress;
end;

function TDbgDwarf.CompilationUnitsCount: Integer;
begin
  Result := FCompilationUnits.Count;
end;

{ TDbgVerboseDwarf }

function TDbgVerboseDwarf.GetCompilationUnitClass: TDwarfCompilationUnitClass;
begin
  Result:= TDwarfVerboseCompilationUnit;
end;

{ TDwarfLineInfoStateMachine }

function TDwarfLineInfoStateMachine.Clone: TDwarfLineInfoStateMachine;
begin
  Result := TDwarfLineInfoStateMachine.Create(FOwner, FLineInfoPtr, FMaxPtr);
  Result.FAddress := FAddress;
  Result.FFileName := FFileName;
  Result.FLine := FLine;
  Result.FColumn := FColumn;
  Result.FIsStmt := FIsStmt;
  Result.FBasicBlock := FBasicBlock;
  Result.FEndSequence := FEndSequence;
  Result.FPrologueEnd := FPrologueEnd;
  Result.FEpilogueBegin := FEpilogueBegin;
  Result.FIsa := FIsa;
  Result.FEnded := FEnded;
end;

constructor TDwarfLineInfoStateMachine.Create(AOwner: TDwarfCompilationUnit; ALineInfoPtr, AMaxPtr: Pointer);
begin
  inherited Create;
  FOwner := AOwner;
  FLineInfoPtr := ALineInfoPtr;
  FMaxPtr := AMaxPtr;
  Reset;
end;

function TDwarfLineInfoStateMachine.NextLine: Boolean;
var
  p: Pointer;
  Opcode: Byte;
  instrlen: Cardinal;
  diridx: Cardinal;
begin
  Result := False;
  if FEndSequence
  then begin
    Reset;
  end
  else begin
    FBasicBlock := False;
    FPrologueEnd := False;
    FEpilogueBegin := False;
  end;
  
  while pbyte(FLineInfoPtr) < FMaxPtr do
  begin
    Opcode := pbyte(FLineInfoPtr)^;
    Inc(pbyte(FLineInfoPtr));
    if Opcode <= Length(FOwner.FLineInfo.StandardOpcodeLengths)
    then begin
      // Standard opcode
      case Opcode of
        DW_LNS_copy: begin
          Result := True;
          Exit;
        end;
        DW_LNS_advance_pc: begin
          Inc(FAddress, ULEB128toOrdinal(pbyte(FLineInfoPtr)));
        end;
        DW_LNS_advance_line: begin
          Inc(FLine, SLEB128toOrdinal(pbyte(FLineInfoPtr)));
        end;
        DW_LNS_set_file: begin
          SetFileName(ULEB128toOrdinal(pbyte(FLineInfoPtr)));
        end;
        DW_LNS_set_column: begin
          FColumn := ULEB128toOrdinal(pbyte(FLineInfoPtr));
        end;
        DW_LNS_negate_stmt: begin
          FIsStmt := not FIsStmt;
        end;
        DW_LNS_set_basic_block: begin
          FBasicBlock := True;
        end;
        DW_LNS_const_add_pc: begin
          Opcode := 255 - Length(FOwner.FLineInfo.StandardOpcodeLengths);
          if FOwner.FLineInfo.LineRange = 0
          then Inc(FAddress, Opcode * FOwner.FLineInfo.MinimumInstructionLength)
          else Inc(FAddress, (Opcode div FOwner.FLineInfo.LineRange) * FOwner.FLineInfo.MinimumInstructionLength);
        end;
        DW_LNS_fixed_advance_pc: begin
          Inc(FAddress, PWord(FLineInfoPtr)^);
          Inc(pbyte(FLineInfoPtr), 2);
        end;
        DW_LNS_set_prologue_end: begin
          FPrologueEnd := True;
        end;
        DW_LNS_set_epilogue_begin: begin
          FEpilogueBegin := True;
        end;
        DW_LNS_set_isa: begin
          FIsa := ULEB128toOrdinal(pbyte(FLineInfoPtr));
        end;
        // Extended opcode
        DW_LNS_extended_opcode: begin
          instrlen := ULEB128toOrdinal(pbyte(FLineInfoPtr)); // instruction length

          case pbyte(FLineInfoPtr)^ of
            DW_LNE_end_sequence: begin
              FEndSequence := True;
              Result := True;
              Exit;
            end;
            DW_LNE_set_address: begin
              if FOwner.FLineInfo.Addr64
              then FAddress := PQWord(pbyte(FLineInfoPtr)+1)^
              else FAddress := PLongWord(pbyte(FLineInfoPtr)+1)^;
            end;
            DW_LNE_define_file: begin
              // don't move pb, it's done at the end by instruction length
              p := pbyte(FLineInfoPtr);
              FFileName := String(PChar(p));
              Inc(p, Length(FFileName) + 1);

              //diridx
              diridx := ULEB128toOrdinal(p);
              if diridx < FOwner.FLineInfo.Directories.Count
              then FFileName := FOwner.FLineInfo.Directories[diridx] + FFileName
              else FFileName := Format('Unknown dir(%u)', [diridx]) + DirectorySeparator + FFileName;
              //last modified
              //ULEB128toOrdinal(p);
              //length
              //ULEB128toOrdinal(p));
            end;
          else
            // unknown extendend opcode
          end;
          Inc(pbyte(FLineInfoPtr), instrlen);
        end;
      else
        // unknown opcode
        Inc(pbyte(FLineInfoPtr), FOwner.FLineInfo.StandardOpcodeLengths[Opcode])
      end;
      Continue;
    end;

    // Special opcode
    Dec(Opcode, Length(FOwner.FLineInfo.StandardOpcodeLengths)+1);
    if FOwner.FLineInfo.LineRange = 0
    then begin
      Inc(FAddress, Opcode * FOwner.FLineInfo.MinimumInstructionLength);
    end
    else begin
      Inc(FAddress, (Opcode div FOwner.FLineInfo.LineRange) * FOwner.FLineInfo.MinimumInstructionLength);
      Inc(FLine, FOwner.FLineInfo.LineBase + (Opcode mod FOwner.FLineInfo.LineRange));
    end;
    Result := True;
    Exit;
  end;
  Result := False;
  FEnded := True;
end;

procedure TDwarfLineInfoStateMachine.Reset;
begin
  FAddress := 0;
  SetFileName(1);
  FLine := 1;
  FColumn := 0;
  FIsStmt := FOwner.FLineInfo.DefaultIsStmt;
  FBasicBlock := False;
  FEndSequence := False;
  FPrologueEnd := False;
  FEpilogueBegin := False;
  FIsa := 0;
end;

procedure TDwarfLineInfoStateMachine.SetFileName(AIndex: Cardinal);
begin
  if (Aindex > 0) and (AIndex <= FOwner.FLineInfo.FileNames.Count)
  then FFileName := FOwner.FLineInfo.FileNames[AIndex - 1]
  else FFileName := Format('Unknown fileindex(%u)', [AIndex]);
end;

{ TDwarfCompilationUnit }

procedure TDwarfCompilationUnit.BuildLineInfo(AAddressInfo: PDwarfAddressInfo; ADoAll: Boolean);
var
  Iter: TMapIterator;
  Info, NextInfo: PDwarfAddressInfo;
  idx: Integer;
  LineMap: PDWarfLineMap;
  Line: Cardinal;
  CurrentFileName: String;
  addr: QWord;
begin
  if not ADoAll
  then begin
    if AAddressInfo = nil then Exit;
    if AAddressInfo^.StateMachine <> nil then Exit;
  end;
  if FLineInfo.StateMachine = nil then Exit;
  if FLineInfo.StateMachine.Ended then Exit;

  BuildAddressMap;
  Iter := TMapIterator.Create(FAddressMap);
  idx := -1;
  Info := nil;
  NextInfo := nil;

  while FLineInfo.StateMachine.NextLine do
  begin
    Line := FLineInfo.StateMachine.Line;
    if Line < 0 then begin
      DebugLn(FPDBG_DWARF_WARNINGS, ['NEGATIVE LINE ', Line]);
      Continue;
    end;

    if (idx < 0) or (CurrentFileName <> FLineInfo.StateMachine.FileName) then begin
      idx := FLineNumberMap.IndexOf(FLineInfo.StateMachine.FileName);
      if idx = -1
      then begin
        LineMap := New(PDWarfLineMap);
        LineMap^.Init;
        FLineNumberMap.AddObject(FLineInfo.StateMachine.FileName, TObject(LineMap));
      end
      else begin
        LineMap := PDWarfLineMap(FLineNumberMap.Objects[idx]);
      end;
      CurrentFileName := FLineInfo.StateMachine.FileName;
    end;

    addr := FLineInfo.StateMachine.Address;
    LineMap^.SetAddressForLine(Line, addr);

    if (Info = nil) or
       (addr < Info^.StartPC) or
       ( (NextInfo <> nil) and (addr >= NextInfo^.StartPC) )
    then begin
      if Iter.Locate(FLineInfo.StateMachine.Address)
      then begin
        // set lineinfo
        Info := Iter.DataPtr;
        Iter.Next;
        if not Iter.EOM
        then NextInfo := Iter.DataPtr
        else NextInfo := nil;

        if Info^.StateMachine = nil
        then begin
          Info^.StateMachine := FLineInfo.StateMachine.Clone;
          FLineInfo.StateMachines.Add(Info^.StateMachine);
        end;
        if not ADoAll and (Info = AAddressInfo)
        then Break;
      end;
    end;
  end;
    
  Iter.Free;

  for Idx := 0 to FLineNumberMap.Count - 1 do
    PDWarfLineMap(FLineNumberMap.Objects[idx])^.Compress;
end;

function TDwarfCompilationUnit.GetUnitName: String;
begin
  Result := FUnitName;
  if Result <> '' then exit;

  FUnitName := LazFileUtils.ExtractFileNameOnly(FileName);
  Result := FUnitName;
end;

function TDwarfCompilationUnit.GetDefinition(AAbbrevPtr: Pointer; out ADefinition: TDwarfAbbrev): Boolean;
begin
  Result := FAbbrevList.FindLe128bFromPointer(AAbbrevPtr, ADefinition) <> nil;
end;

procedure TDwarfCompilationUnit.ScanAllEntries;
var
  ResultScope: TDwarfScopeInfo;
begin
  if FScannedToEnd then exit;
  FScannedToEnd := True;
  // scan to end
  LocateEntry(0, ResultScope);
end;

procedure TDwarfCompilationUnit.BuildAddressMap;
var
  AttribList: TAttribPointerList;
  Attrib: Pointer;
  Form: Cardinal;
  Info: TDwarfAddressInfo;
  Scope: TDwarfScopeInfo;
  ScopeIdx: Integer;
  Abbrev: TDwarfAbbrev;
begin
  if FAddressMapBuild then Exit;

  ScanAllEntries;

  Scope := FScope;
  ScopeIdx := Scope.Index;

  while Scope.IsValid do
  begin
    if not GetDefinition(Scope.Entry, Abbrev) then begin
      inc(ScopeIdx);
      Scope.Index := ScopeIdx; // Child or Next, or parent.next
      continue;

      //DebugLn(FPDBG_DWARF_WARNINGS, ['No abbrev found']);
      //break;
    end;

    if Abbrev.tag = DW_TAG_subprogram then begin
      AttribList.EvalCount := 0;
      Info.ScopeIndex := Scope.Index;
      Info.ScopeList := Scope.FScopeList;
      // TODO: abstract origin
      if InitLocateAttributeList(Scope.Entry, AttribList) then begin // TODO: error if not
        if (dafHasLowAddr in AttribList.Abbrev^.flags) and
           LocateAttribute(Scope.Entry, DW_AT_low_pc, AttribList, Attrib, Form)
        then begin
          ReadValue(Attrib, Form, Info.StartPC);

          if LocateAttribute(Scope.Entry, DW_AT_high_pc, AttribList, Attrib, Form)
          then ReadValue(Attrib, Form, Info.EndPC)
          else Info.EndPC := Info.StartPC;

          // TODO (dafHasName in Abbrev.flags)
          if (dafHasName in AttribList.Abbrev^.flags) and
             LocateAttribute(Scope.Entry, DW_AT_name, AttribList, Attrib, Form)
          then ReadValue(Attrib, Form, Info.Name)
          else Info.Name := 'undefined';

          Info.StateMachine := nil;
          if Info.StartPC <> 0
          then begin
            if FAddressMap.HasId(Info.StartPC)
            then DebugLn(FPDBG_DWARF_WARNINGS, ['WARNING duplicate start adress: ', IntToHex(Info.StartPC, FAddressSize * 2)])
            else FAddressMap.Add(Info.StartPC, Info);
          end;
        end;
      end;
    end;

    inc(ScopeIdx);
    Scope.Index := ScopeIdx; // Child or Next, or parent.next
  end;

  FAddressMapBuild := True;
end;

constructor TDwarfCompilationUnit.Create(AOwner: TDbgDwarf; ADataOffset: QWord; ALength: QWord; AVersion: Word; AAbbrevOffset: QWord; AAddressSize: Byte; AIsDwarf64: Boolean);
  procedure FillLineInfo(AData: Pointer);
  var
    LNP32: PDwarfLNPHeader32 absolute AData;
    LNP64: PDwarfLNPHeader64 absolute AData;
    Info: PDwarfLNPInfoHeader;

    UnitLength: QWord;
    Version: Word;
    HeaderLength: QWord;
    Name: PChar;
    diridx: Cardinal;
    S: String;
    pb: PByte absolute Name;
  begin
    FLineInfo.Header := AData;
    if LNP64^.Signature = DWARF_HEADER64_SIGNATURE
    then begin
      if FVersion < 3 then
        DebugLn(FPDBG_DWARF_WARNINGS, ['Unexpected 64 bit signature found for DWARF version 2']); // or version 1...
      FLineInfo.Addr64 := True;
      UnitLength := LNP64^.UnitLength;
      FLineInfo.DataEnd := Pointer(@LNP64^.Version) + UnitLength;
      Version := LNP64^.Version;
      HeaderLength := LNP64^.HeaderLength;
      Info := @LNP64^.Info;
    end
    else begin
      if (FVersion < 3) and (FAddressSize = 8) then
        FLineInfo.Addr64 := True
      else
        FLineInfo.Addr64 := False;
      UnitLength := LNP32^.UnitLength;
      FLineInfo.DataEnd := Pointer(@LNP32^.Version) + UnitLength;
      Version := LNP32^.Version;
      HeaderLength := LNP32^.HeaderLength;
      Info := @LNP32^.Info;
    end;
    FLineInfo.DataStart := PByte(Info) + HeaderLength;


    FLineInfo.MinimumInstructionLength := Info^.MinimumInstructionLength;
    FLineInfo.DefaultIsStmt := Info^.DefaultIsStmt <> 0;
    FLineInfo.LineBase := Info^.LineBase;
    FLineInfo.LineRange := Info^.LineRange;

    // opcodelengths
    SetLength(FLineInfo.StandardOpcodeLengths, Info^.OpcodeBase - 1);
    Move(Info^.StandardOpcodeLengths, FLineInfo.StandardOpcodeLengths[0], Info^.OpcodeBase - 1);

    // directories & filenames
    FLineInfo.Directories := TStringList.Create;
    FLineInfo.Directories.Add(''); // current dir
    Name := @Info^.StandardOpcodeLengths;
    Inc(Name, Info^.OpcodeBase-1);
    // directories
    while Name^ <> #0 do
    begin
      S := String(Name);
      Inc(pb, Length(S)+1);
      FLineInfo.Directories.Add(S + DirectorySeparator);
    end;
    Inc(Name);

    // filenames
    FLineInfo.FileNames := TStringList.Create;
    while Name^ <> #0 do
    begin
      S := String(Name);
      Inc(pb, Length(S)+1);
      //diridx
      diridx := ULEB128toOrdinal(pb);
      if diridx < FLineInfo.Directories.Count
      then S := FLineInfo.Directories[diridx] + S
      else S := Format('Unknown dir(%u)', [diridx]) + DirectorySeparator + S;
      FLineInfo.FileNames.Add(S);
      //last modified
      ULEB128toOrdinal(pb);
      //length
      ULEB128toOrdinal(pb);
    end;

    FLineInfo.StateMachine := TDwarfLineInfoStateMachine.Create(Self, FLineInfo.DataStart, FLineInfo.DataEnd);
    FLineInfo.StateMachines := TFPObjectList.Create(True);

    FLineInfo.Valid := True;
  end;

var
  AttribList: TAttribPointerList;
  Attrib: Pointer;
  Form: Cardinal;
  StatementListOffs, Offs: QWord;
  Scope: TDwarfScopeInfo;
begin
  inherited Create;
  FOwner := AOwner;
  FInfoData := FOwner.FSections[dsInfo].RawData + ADataOffset;
  FLength := ALength;
  FVersion := AVersion;
  FAbbrevOffset := AAbbrevOffset;
  // check for address as offset
  if FAbbrevOffset > FOwner.FSections[dsAbbrev].Size
  then begin
    Offs := FAbbrevOffset - FOwner.FImageBase - FOwner.FSections[dsAbbrev].VirtualAdress;
    if (Offs >= 0) and (Offs < FOwner.FSections[dsAbbrev].Size)
    then begin
      DebugLn(FPDBG_DWARF_WARNINGS, ['WARNING: Got Abbrev offset as address, adjusting..']);
      FAbbrevOffset := Offs;
    end;
  end;

  FAddressSize := AAddressSize;
  FIsDwarf64 := AIsDwarf64;

  FAbbrevList := TDwarfAbbrevList.Create(FOwner.FSections[dsAbbrev].RawData,
    FOwner.FSections[dsAbbrev].RawData + FOwner.FSections[dsAbbrev].Size,
    FAbbrevOffset, FLength);

  // use internally 64 bit target pointer
  FAddressMap := TMap.Create(itu8, SizeOf(TDwarfAddressInfo));
  FLineNumberMap := TStringList.Create;
  FLineNumberMap.Sorted := True;
  FLineNumberMap.Duplicates := dupError;

  SetLength(FScopeList.List, Min(SCOPE_ALLOC_BLOCK_SIZE, FLength div 2 + 1));
  FScopeList.List[0].Link := -1;
  FScopeList.List[0].Entry  := FInfoData;
  FScopeList.HighestKnown := 0;
  FScope.Init(@FScopeList);
  FScope.Index := 0;
  // retrieve some info about this unit
  if not LocateEntry(DW_TAG_compile_unit, Scope)
  then begin
    DebugLn(FPDBG_DWARF_WARNINGS, ['WARNING compilation unit has no compile_unit tag']);
    Exit;
  end;
  FValid := True;

  AttribList.EvalCount := 0;
  /// TODO: (dafHasName in Abbrev.flags)
  if LocateAttribute(Scope.Entry, DW_AT_name, AttribList, Attrib, Form)
  then ReadValue(Attrib, Form, FFileName);
  
  if not LocateAttribute(Scope.Entry, DW_AT_identifier_case, AttribList, Attrib, Form)
  and not ReadValue(Attrib, Form, FIdentifierCase)
  then FIdentifierCase := DW_ID_case_sensitive;
  
  if LocateAttribute(Scope.Entry, DW_AT_stmt_list, AttribList, Attrib, Form)
  and ReadValue(Attrib, Form, StatementListOffs)
  then begin
    // check for address as offset
    if StatementListOffs < FOwner.FSections[dsLine].Size
    then begin
      FillLineInfo(FOwner.FSections[dsLine].RawData + StatementListOffs);
    end
    else begin
      Offs := StatementListOffs - FOwner.FImageBase - FOwner.FSections[dsLine].VirtualAdress;
      if (Offs >= 0) and (Offs < FOwner.FSections[dsLine].Size)
      then begin
        DebugLn(FPDBG_DWARF_WARNINGS, ['WARNING: Got Lineinfo offset as address, adjusting..']);
        FillLineInfo(FOwner.FSections[dsLine].RawData + Offs);
      end;
    end;
  end;

  if LocateAttribute(Scope.Entry, DW_AT_low_pc, AttribList, Attrib, Form)
  then ReadValue(Attrib, Form, FMinPC);

  if LocateAttribute(Scope.Entry, DW_AT_high_pc, AttribList, Attrib, Form)
  then ReadValue(Attrib, Form, FMaxPC);
  
  if FMinPC = 0 then FMinPC := FMaxPC;
  if FMaxPC = 0 then FMAxPC := FMinPC;
end;

destructor TDwarfCompilationUnit.Destroy;
  procedure FreeLineNumberMap;
  var
    n: Integer;
  begin
    for n := 0 to FLineNumberMap.Count - 1 do
      Dispose(PDWarfLineMap(FLineNumberMap.Objects[n]));
    FreeAndNil(FLineNumberMap);
  end;

begin
  FreeAndNil(FAbbrevList);
  FreeAndNil(FAddressMap);
  FreeLineNumberMap;
  FreeAndNil(FLineInfo.StateMachines);
  FreeAndNil(FLineInfo.StateMachine);
  FreeAndNil(FLineInfo.Directories);
  FreeAndNil(FLineInfo.FileNames);

  inherited Destroy;
end;

function TDwarfCompilationUnit.GetLineAddressMap(const AFileName: String): PDWarfLineMap;
  var
    Name: String;
  function FindIndex: Integer;
  begin
    // try fullname first
    Result := FLineNumberMap.IndexOf(AFileName);
    if Result <> -1 then Exit;
    
    Name := ExtractFileName(AFileName);
    Result := FLineNumberMap.IndexOf(Name);
    if Result <> -1 then Exit;

    Name := UpperCase(Name);
    for Result := 0 to FLineNumberMap.Count - 1 do
    begin
      if Name = UpperCase(ExtractFileName(FLineNumberMap[Result]))
      then Exit;
    end;
    Result := -1
  end;
var
  idx: Integer;
begin
  Result := nil;
  if not Valid then Exit;

  // make sure all filenames are there
  BuildLineInfo(nil, True);
  idx := FindIndex;
  if idx = -1 then Exit;
  
  Result := PDWarfLineMap(FLineNumberMap.Objects[idx]);
end;

function TDwarfCompilationUnit.GetLineAddress(const AFileName: String; ALine: Cardinal): TDbgPtr;
var
  Map: PDWarfLineMap;
begin
  Result := 0;
  Map := GetLineAddressMap(AFileName);
  if Map = nil then exit;
  Result := Map^.GetAddressForLine(ALine);
end;

function TDwarfCompilationUnit.InitLocateAttributeList(AEntry: Pointer;
  var AList: TAttribPointerList): Boolean;
var
  AbrCnt: Integer;
begin
  if FAbbrevList.FindLe128bFromPointer(AEntry, AList.Abbrev) = nil then begin
  //if not GetDefinition(AEntry, AList.Abbrev)
  //then begin      //???
    DebugLn(FPDBG_DWARF_WARNINGS, ['Error: Abbrev not found: ', ULEB128toOrdinal(AEntry)]);
    AList.EvalCount := -1;
    Result := False;
    Exit;
  end;

  AbrCnt := AList.Abbrev^.count;
  if AbrCnt = 0 then begin
    AList.EvalCount := -1;
    exit;
  end;
  SetLength(AList.List, AbrCnt);
  ULEB128toOrdinal(AEntry);
  AList.List[0] := AEntry;
  AList.EvalCount := 1;

  Result := True;
end;

function TDwarfCompilationUnit.LocateAttribute(AEntry: Pointer; AAttribute: Cardinal;
  var AList: TAttribPointerList; out AAttribPtr: Pointer; out AForm: Cardinal): Boolean;
var
  Abbrev: Cardinal;
  i, EvalCnt, AbrIdx, AbrCnt: Integer;
  ADefs: PDwarfAbbrevEntry;
begin
  Result := False;
  if AList.EvalCount < 0 then
    exit;

  if AList.EvalCount = 0 then begin
    if FAbbrevList.FindLe128bFromPointer(AEntry, AList.Abbrev) = nil then begin
    //if not GetDefinition(AEntry, AList.Abbrev)
    //then begin      //???
      Abbrev := ULEB128toOrdinal(AEntry);
      DebugLn(FPDBG_DWARF_WARNINGS, ['Error: Abbrev not found: ', Abbrev]);
      AList.EvalCount := -1;
      Exit;
    end;

    AbrIdx := AList.Abbrev^.count;
    if AbrIdx = 0 then begin
      AList.EvalCount := -1;
      exit;
    end;
    SetLength(AList.List, AbrIdx);
    ULEB128toOrdinal(AEntry);
    AList.List[0] := AEntry;
    AList.EvalCount := 1;
  end;

  ADefs := FAbbrevList.EntryPointer[0];
  AbrIdx := AList.Abbrev^.Index;
  AbrCnt := AList.Abbrev^.Count - 1;
  EvalCnt := AList.EvalCount - 1;
  i := 0;

  while true do begin
    if ADefs[AbrIdx].Attribute = AAttribute
    then begin
      Result := True;
      AAttribPtr := AList.List[i];
      AForm := ADefs[AbrIdx].Form;
      break;
    end;

    if i = AbrCnt then
      break;

    if (i < EvalCnt) then begin
      inc(i);
      inc(AbrIdx);
      Continue;
    end;

    AEntry := AList.List[i];
    if not SkipEntryDataForForm(AEntry, ADefs[AbrIdx].Form, FAddressSize) then
      break;
    AList.List[i+1] := AEntry;
    inc(i);
    inc(AbrIdx);
  end;

  if i {+ 1} > EvalCnt {+ 1} then
    AList.EvalCount := i + 1
end;

function TDwarfCompilationUnit.LocateAttribute(AEntry: Pointer; AAttribute: Cardinal; out
  AAttribPtr: Pointer; out AForm: Cardinal): Boolean;
var
  Def: TDwarfAbbrev;
  n: Integer;
  ADefs: PDwarfAbbrevEntry;
begin
  AEntry := FAbbrevList.FindLe128bFromPointer(AEntry, Def);
  if AEntry = nil
  then begin
    //???
    //Abbrev := ULEB128toOrdinal(AEntry);
    DebugLn(FPDBG_DWARF_WARNINGS, ['Error: Abbrev not found: '{, Abbrev}]);
    Result := False;
    Exit;
  end;

  ADefs := FAbbrevList.EntryPointer[0];
  for n := Def.Index to Def.Index + Def.Count - 1 do
  begin
    if ADefs[n].Attribute = AAttribute
    then begin
      Result := True;
      AAttribPtr := AEntry;
      AForm := ADefs[n].Form;
      Exit;
    end
    else begin
      if not SkipEntryDataForForm(AEntry, ADefs[n].Form, FAddressSize) then
        break;
    end;
  end;
  Result := False;
end;

//----------------------------------------
// Params
//   ATag: a tag to search for
//   AStartScope: a startpoint in the data
//   ACurrentOnly: if set, process only current entry
//   AResultScope: the located scope info
//----------------------------------------
function TDwarfCompilationUnit.LocateEntry(ATag: Cardinal; out
  AResultScope: TDwarfScopeInfo): Boolean;

  function ParseAttribs(const ADef: PDwarfAbbrev; var p: Pointer): Boolean;
  var
    idx: Integer;
    ADefs: PDwarfAbbrevEntry;
    AddrSize: Byte;
  begin
    ADefs := FAbbrevList.EntryPointer[ADef^.Index];
    AddrSize := FAddressSize;
    for idx := 0 to ADef^.Count - 1 do
    begin
      if not SkipEntryDataForForm(p, ADefs^.Form, AddrSize) then
        exit(False);
      inc(ADefs);
    end;
    Result := True;
  end;

var
  Abbrev: Cardinal;
  Def: PDwarfAbbrev;
  MaxData: Pointer;
  p, EntryDataPtr, NextEntryDataPtr: Pointer;
  Scope: TDwarfScopeInfo;
  ni: Integer;
  AppendAsChild: Boolean;
begin
  Result := False;
  if not FScope.IsValid then Exit;
  MaxData := FInfoData + FLength;
  Scope := FScope;
  Scope.Index := FScopeList.HighestKnown; // last known scope

  // "last rounds" NextEntryDataPtr
  NextEntryDataPtr := Scope.Entry;
  while (NextEntryDataPtr < MaxData) do
  begin
    EntryDataPtr := NextEntryDataPtr;

    NextEntryDataPtr := FAbbrevList.FindLe128bFromPointer(EntryDataPtr, Def);
    if NextEntryDataPtr = nil then begin
      Abbrev := ULEB128toOrdinal(EntryDataPtr);
      DebugLn(FPDBG_DWARF_WARNINGS, ['Error: Abbrev not found: ', Abbrev]);
          // TODO shorten array
      exit;
    end;

    if (ATag <> 0) and (Def^.tag = ATag) then begin
      Result := True;
      AResultScope := Scope;
      Break;
    end;

    if not ParseAttribs(Def, NextEntryDataPtr) then begin
      DebugLn(FPDBG_DWARF_WARNINGS, ['Error: data not parsed:']);
      exit;
    end;
    // NextEntryDataPtr is now at next scope

    if NextEntryDataPtr >= MaxData then
      break;

    p := NextEntryDataPtr;
    Abbrev := ULEB128toOrdinal(p);
    if Abbrev = 0 then begin      // no more sibling
      AppendAsChild := False;     // children already done
      if (dafHasChildren in Def^.flags) then begin  // current has 0 children
        NextEntryDataPtr := p;
        if NextEntryDataPtr >= MaxData then
          break;
        Abbrev := ULEB128toOrdinal(p);
      end;
      while (Abbrev = 0) do begin
        NextEntryDataPtr := p;
        if NextEntryDataPtr >= MaxData then
          break;
        Scope.GoParent;
        if not Scope.IsValid then begin
          DebugLn(FPDBG_DWARF_WARNINGS, ['Error: Abbrev not found: ', Abbrev]);
          // TODO shorten array
          exit;
        end;
        Abbrev := ULEB128toOrdinal(p);
      end;
      if NextEntryDataPtr >= MaxData then
        break;
    end
    else
      AppendAsChild := (dafHasChildren in Def^.flags);

    if AppendAsChild then
      ni := Scope.CreateChildForEntry(NextEntryDataPtr)
    else
      ni := Scope.CreateNextForEntry(NextEntryDataPtr);

    Scope.FIndex := ni; // skip check, index was just created / must exist
  end;

  if (NextEntryDataPtr >= MaxData) then begin
    if (EntryDataPtr > MaxData) then
      debugln(FPDBG_DWARF_WARNINGS, ['LocateEntry went past end of memory: ', EntryDataPtr-MaxData]);
    SetLength(FScopeList.List, FScopeList.HighestKnown + 1);
  end;

end;

function TDwarfCompilationUnit.ReadAddressAtPointer(var AData: Pointer;
  AIncPointer: Boolean): QWord;
begin
  if FAddressSize = 4 // TODO Dwarf3 depends on FIsDwarf64
  then Result := PLongWord(AData)^
  else Result := PQWord(AData)^;
  if AIncPointer then inc(AData, FAddressSize);
end;

function TDwarfCompilationUnit.ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: Cardinal): Boolean;
begin
  Result := True;
  case AForm of
    DW_FORM_addr,
    DW_FORM_ref_addr : begin
      AValue := ReadAddressAtPointer(AAttribute);
    end;
    DW_FORM_flag,
    DW_FORM_ref1,
    DW_FORM_data1    : begin
      AValue := PByte(AAttribute)^;
    end;
    DW_FORM_ref2,
    DW_FORM_data2    : begin
      AValue := PWord(AAttribute)^;
    end;
    DW_FORM_ref4,
    DW_FORM_data4    : begin
      AValue := PLongWord(AAttribute)^;
    end;
    DW_FORM_ref8,
    DW_FORM_data8    : begin
      AValue := PQWord(AAttribute)^;
    end;
    DW_FORM_sdata    : begin
      AValue := SLEB128toOrdinal(AAttribute);
    end;
    DW_FORM_ref_udata,
    DW_FORM_udata    : begin
      AValue := ULEB128toOrdinal(AAttribute);
    end;
  else
    Result := False;
  end;
end;

function TDwarfCompilationUnit.ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: Int64): Boolean;
begin
  Result := True;
  case AForm of
    DW_FORM_addr,
    DW_FORM_ref_addr : begin
      AValue := ReadAddressAtPointer(AAttribute);
    end;
    DW_FORM_flag,
    DW_FORM_ref1,
    DW_FORM_data1    : begin
      AValue := PShortInt(AAttribute)^;
    end;
    DW_FORM_ref2,
    DW_FORM_data2    : begin
      AValue := PSmallInt(AAttribute)^;
    end;
    DW_FORM_ref4,
    DW_FORM_data4    : begin
      AValue := PLongInt(AAttribute)^;
    end;
    DW_FORM_ref8,
    DW_FORM_data8    : begin
      AValue := PInt64(AAttribute)^;
    end;
    DW_FORM_sdata    : begin
      AValue := SLEB128toOrdinal(AAttribute);
    end;
    DW_FORM_ref_udata,
    DW_FORM_udata    : begin
      AValue := Int64(ULEB128toOrdinal(AAttribute));
    end;
  else
    Result := False;
  end;
end;

function TDwarfCompilationUnit.ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: Integer): Boolean;
begin
  Result := True;
  case AForm of
    DW_FORM_addr,
    DW_FORM_ref_addr : begin
      AValue := ReadAddressAtPointer(AAttribute);
    end;
    DW_FORM_flag,
    DW_FORM_ref1,
    DW_FORM_data1    : begin
      AValue := PShortInt(AAttribute)^;
    end;
    DW_FORM_ref2,
    DW_FORM_data2    : begin
      AValue := PSmallInt(AAttribute)^;
    end;
    DW_FORM_ref4,
    DW_FORM_data4    : begin
      AValue := PLongInt(AAttribute)^;
    end;
    DW_FORM_ref8,
    DW_FORM_data8    : begin
      AValue := PInt64(AAttribute)^;
    end;
    DW_FORM_sdata    : begin
      AValue := SLEB128toOrdinal(AAttribute);
    end;
    DW_FORM_ref_udata,
    DW_FORM_udata    : begin
      AValue := ULEB128toOrdinal(AAttribute);
    end;
  else
    Result := False;
  end;
end;

function TDwarfCompilationUnit.ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: PChar): Boolean;
begin
  Result := True;
  case AForm of
    DW_FORM_string: begin
      AValue := PChar(AAttribute);
    end;
    DW_FORM_strp:   begin
      AValue := 'TODO';
    end;
  else
    Result := False;
  end;
end;

function TDwarfCompilationUnit.ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: QWord): Boolean;
begin
  Result := True;
  case AForm of
    DW_FORM_addr,
    DW_FORM_ref_addr : begin
      AValue := ReadAddressAtPointer(AAttribute);
    end;
    DW_FORM_flag,
    DW_FORM_ref1,
    DW_FORM_data1    : begin
      AValue := PByte(AAttribute)^;
    end;
    DW_FORM_ref2,
    DW_FORM_data2    : begin
      AValue := PWord(AAttribute)^;
    end;
    DW_FORM_ref4,
    DW_FORM_data4    : begin
      AValue := PLongWord(AAttribute)^;
    end;
    DW_FORM_ref8,
    DW_FORM_data8    : begin
      AValue := PQWord(AAttribute)^;
    end;
    DW_FORM_sdata    : begin
      AValue := QWord(SLEB128toOrdinal(AAttribute));
    end;
    DW_FORM_ref_udata,
    DW_FORM_udata    : begin
      AValue := ULEB128toOrdinal(AAttribute);
    end;
  else
    Result := False;
  end;
end;

function TDwarfCompilationUnit.ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: String): Boolean;
begin
  Result := True;
  case AForm of
    DW_FORM_string: begin
      AValue := PChar(AAttribute);
    end;
    DW_FORM_strp:   begin
      AValue := 'TODO';
    end;
  else
    Result := False;
  end;
end;

function TDwarfCompilationUnit.ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: TByteDynArray): Boolean;
var
  Size: Cardinal;
begin
  Result := True;
  case AForm of
    DW_FORM_block    : begin
      Size := ULEB128toOrdinal(AAttribute);
    end;
    DW_FORM_block1   : begin
      Size := PByte(AAttribute)^;
      Inc(AAttribute, 1);
    end;
    DW_FORM_block2   : begin
      Size := PWord(AAttribute)^;
      Inc(AAttribute, 2);
    end;
    DW_FORM_block4   : begin
      Size := PLongWord(AAttribute)^;
      Inc(AAttribute, 4);
    end;
  else
    Result := False;
    Size := 0;
  end;
  SetLength(AValue, Size);
  if Size > 0 then
    Move(AAttribute^, AValue[0], Size);
end;

{ TDwarfVerboseCompilationUnit }

constructor TDwarfVerboseCompilationUnit.Create(AOwner: TDbgDwarf; ADataOffset: QWord; ALength: QWord; AVersion: Word; AAbbrevOffset: QWord; AAddressSize: Byte; AIsDwarf64: Boolean);
begin
  FVerbose := True;
  
  DebugLn(FPDBG_DWARF_VERBOSE, ['-- compilation unit --']);
  DebugLn(FPDBG_DWARF_VERBOSE, [' data offset: ', ADataOffset]);
  DebugLn(FPDBG_DWARF_VERBOSE, [' length: ', ALength]);
  DebugLn(FPDBG_DWARF_VERBOSE, [' version: ', AVersion]);
  DebugLn(FPDBG_DWARF_VERBOSE, [' abbrev offset: ', AAbbrevOffset]);
  DebugLn(FPDBG_DWARF_VERBOSE, [' address size: ', AAddressSize]);
  DebugLn(FPDBG_DWARF_VERBOSE, [' 64bit: ', AIsDwarf64]);
  DebugLn(FPDBG_DWARF_VERBOSE, ['----------------------']);
  inherited;
end;

{ TDbgDwarfInfoAddressContext }

function TDbgDwarfInfoAddressContext.GetSymbolAtAddress: TDbgSymbol;
begin
  Result := FSymbol;
end;

function TDbgDwarfInfoAddressContext.GetAddress: TDbgPtr;
begin
  Result := FAddress;
end;

constructor TDbgDwarfInfoAddressContext.Create(AnAddress: TDbgPtr; ASymbol: TDbgSymbol;
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

function TDbgDwarfInfoAddressContext.FindSymbol(const AName: String): TDbgSymbol;
var
  SubRoutine: TDbgDwarfProcSymbol; // TDbgSymbol;
  CU, CU2, FwdCompUint: TDwarfCompilationUnit;
  //Scope,
  StartScopeIdx, ExtVal, i: Integer;
  InfoEntry, InfoEntryTmp, InfoEntryParent: TDwarfInformationEntry;
  s, s1, s2: String;
  InfoName: PChar;
  FwdInfoPtr: Pointer;
  tg: Cardinal;
  p1, p2: PChar;
  SelfParam: TDbgDwarfValueIdentifier;
begin
  Result := nil;
  if (FSymbol = nil) or not(FSymbol is TDbgDwarfProcSymbol) or (AName = '') then
    exit;

  SubRoutine := TDbgDwarfProcSymbol(FSymbol);
  s1 := UTF8UpperCase(AName);
  s2 := UTF8LowerCase(AName);
  p1 := @s1[1];
  p2 := @s2[1];

  try
    CU := SubRoutine.FCU;
    InfoEntry := SubRoutine.InformationEntry.Clone;
    //InfoEntry := TDwarfInformationEntry.Create(CU, nil);
    //InfoEntry.ScopeIndex := SubRoutine.FAddressInfo^.ScopeIndex;

    // special: search "self"
    // Todo nested procs
    if s2 = 'self' then begin
      Result := SubRoutine.GetSelfParameter(FAddress);
      if Result <> nil then begin
        Result.AddReference;
        exit;
      end;
    end;

    while InfoEntry.HasValidScope do begin
      debugln(FPDBG_DWARF_SEARCH, ['TDbgDwarf.FindIdentifier Searching ', dbgs(InfoEntry.FScope, CU)]);
      StartScopeIdx := InfoEntry.ScopeIndex;

      //if InfoEntry.Abbrev = nil then
      //  exit;

      if not InfoEntry.IsAddressInStartScope(FAddress) // StartScope = first valid address
      then begin
        // CONTINUE: Search parent(s)
        InfoEntry.ScopeIndex := StartScopeIdx;
        InfoEntry.GoParent;
        Continue;
      end;

      if InfoEntry.ReadName(InfoName) and not InfoEntry.IsArtificial
      then begin
        if (CompareUtf8BothCase(p1, p2, InfoName)) then begin
          Result := TDbgDwarfIdentifier.CreateSubClass(AName, InfoEntry);
          DebugLn(FPDBG_DWARF_SEARCH, ['TDbgDwarf.FindIdentifier found ', dbgs(InfoEntry.FScope, CU), DbgSName(Result)]);
          exit;
        end;
      end;

      tg := InfoEntry.AbbrevTag;

      if InfoEntry.GoNamedChildEx(p1, p2) then begin
        if InfoEntry.IsAddressInStartScope(FAddress) and not InfoEntry.IsArtificial then begin
          if (tg = DW_TAG_class_type) or (tg = DW_TAG_structure_type) then begin
            // access via self
            SelfParam := SubRoutine.GetSelfParameter(FAddress);
            if (SelfParam <> nil) then begin
              // TODO: only valid, as long as context is valid, because if comnext is freed, then self is lost too
              Result := SelfParam.MemberByName[AName];
              assert(Result <> nil, 'FindSymbol: SelfParam.MemberByName[AName]');
              Result.AddReference;
if Result<> nil then debugln(['TDbgDwarfInfoAddressContext.FindSymbol SELF !!!!!!!!!!!!!'])
else debugln(['TDbgDwarfInfoAddressContext.FindSymbol ????????????????']);
            end;
          end;
          if Result = nil then
            Result := TDbgDwarfIdentifier.CreateSubClass(AName, InfoEntry);

          // TODO: nested
          if (StartScopeIdx = SubRoutine.InformationEntry.ScopeIndex) then // searching in subroutine
            TDbgDwarfIdentifier(Result).ParentTypeInfo := SubRoutine;

          DebugLn(FPDBG_DWARF_SEARCH, ['TDbgDwarf.FindIdentifier found ', dbgs(InfoEntry.FScope, CU), DbgSName(Result)]);
          exit;
        end;
      end;

      if (tg = DW_TAG_class_type) or (tg = DW_TAG_structure_type) then begin
        // TODO: only search most inner class (if classes can be nested.
        // outer classes static data may be available.
        // search parent class
        InfoEntry.ScopeIndex := StartScopeIdx;
        InfoEntryParent := InfoEntry.FindChildByTag(DW_TAG_inheritance);
        while (InfoEntryParent <> nil) and (InfoEntryParent.ReadReference(DW_AT_type, FwdInfoPtr, FwdCompUint)) do begin
          InfoEntryParent.ReleaseReference;
          InfoEntryParent := TDwarfInformationEntry.Create(FwdCompUint, FwdInfoPtr);
          DebugLn(FPDBG_DWARF_SEARCH, ['TDbgDwarf.FindIdentifier  PARENT ', dbgs(InfoEntryParent, FwdCompUint) ]);

          if not InfoEntry.IsAddressInStartScope(FAddress) then
            break;

          InfoEntryTmp := InfoEntryParent.FindChildByTag(DW_TAG_inheritance);
          if InfoEntryParent.GoNamedChildEx(p1, p2) then begin
            if InfoEntry.IsAddressInStartScope(FAddress) then begin
              SelfParam := SubRoutine.GetSelfParameter(FAddress);
              if (SelfParam <> nil) then begin
                // TODO: only valid, as long as context is valid, because if comnext is freed, then self is lost too
                Result := SelfParam.MemberByName[AName];
                assert(Result <> nil, 'FindSymbol: SelfParam.MemberByName[AName]');
                Result.AddReference;
if Result<> nil then debugln(['TDbgDwarfInfoAddressContext.FindSymbol SELF !!!!!!!!!!!!!'])
else debugln(['TDbgDwarfInfoAddressContext.FindSymbol ????????????????']);
              end
else debugln(['TDbgDwarfInfoAddressContext.FindSymbol XXXXXXXXXXXXX no self']);
              ;
              if Result = nil then
                Result := TDbgDwarfIdentifier.CreateSubClass(AName, InfoEntryParent);
              InfoEntryParent.ReleaseReference;
              InfoEntryTmp.ReleaseReference;
              DebugLn(FPDBG_DWARF_SEARCH, ['TDbgDwarf.FindIdentifier found ', dbgs(InfoEntryParent.FScope, CU), DbgSName(Result)]);
              exit;
            end;
          end;
          InfoEntryParent.ReleaseReference;
          InfoEntryParent := InfoEntryTmp;
        end;
        InfoEntryParent.ReleaseReference;
      end;

      // Search parent(s)
      InfoEntry.ScopeIndex := StartScopeIdx;
      InfoEntry.GoParent;
    end;

    // other units
    i := FDwarf.CompilationUnitsCount;
    while i > 0 do begin
      dec(i);
      CU2 := FDwarf.CompilationUnits[i];
      if CU2 = CU then
        continue;

      InfoEntry.ReleaseReference;
      InfoEntry := TDwarfInformationEntry.Create(CU2, nil);
      InfoEntry.ScopeIndex := CU2.FirstScope.Index;

      if not InfoEntry.AbbrevTag = DW_TAG_compile_unit then
        continue;
      // compile_unit can not have startscope

      s := CU2.UnitName;
      if (s <> '') and (CompareUtf8BothCase(p1, p2, @s[1])) then begin
        Result := TDbgDwarfIdentifier.CreateSubClass(AName, InfoEntry);
        DebugLn(FPDBG_DWARF_SEARCH, ['TDbgDwarf.FindIdentifier found unit ', dbgs(InfoEntry.FScope, CU2), DbgSName(Result)]);
        break;
      end;

      CU2.ScanAllEntries;
      if InfoEntry.GoNamedChildEx(p1, p2) then begin
        if InfoEntry.IsAddressInStartScope(FAddress) then begin
          // only variables are marked "external", but types not / so we may need all top level
          Result := TDbgDwarfIdentifier.CreateSubClass(AName, InfoEntry);
          DebugLn(FPDBG_DWARF_SEARCH, ['TDbgDwarf.FindIdentifier found (other unit) ', dbgs(InfoEntry.FScope, CU2), DbgSName(Result)]);
          // DW_AT_visibility ?
          if InfoEntry.ReadValue(DW_AT_external, ExtVal) then
            if ExtVal <> 0 then
              break;
            // Search for better result
        end;
      end;

    end;

  finally
    ReleaseRefAndNil(InfoEntry);
  end;
end;

{ TDwarfAbbrevDecoder }

constructor TDwarfAbbrevDecoder.Create(ACompilationUnit: TDwarfCompilationUnit);
begin
  inherited Create;
  FCU := ACompilationUnit;
end;

procedure TDwarfAbbrevDecoder.Decode;
var
  Iter: TMapIterator;
  Info: TDwarfAddressInfo;
  Scope: TDwarfScopeInfo;
begin
  // force all abbrevs to be loaded
  InternalDecode(FCU.FInfoData, FCU.FInfoData + FCU.FLength);

  DebugLn(FPDBG_DWARF_VERBOSE, ['addresses: ']);
  Iter := TMapIterator.Create(FCU.FAddressMap);
  while not Iter.EOM do
  begin
    Iter.GetData(Info);
    DbgOut(FPDBG_DWARF_VERBOSE, ['  ']);
    Scope.Init(Info.ScopeList);
    Scope.Index := Info.ScopeIndex;
    Scope := Scope.Parent;
    while Scope.IsValid do
    begin
      DbgOut(FPDBG_DWARF_VERBOSE, ['.']);
      Scope := Scope.Parent;
    end;
    DebugLn(FPDBG_DWARF_VERBOSE, [Info.Name, ': $', IntToHex(Info.StartPC, FCU.FAddressSize * 2), '..$', IntToHex(Info.EndPC, FCU.FAddressSize * 2)]);
    Iter.Next;
  end;
  Iter.Free;

end;

procedure TDwarfAbbrevDecoder.DecodeLocation(AData: PByte; ASize: QWord; const AIndent: String);
var
  MaxData: PByte;
  v: Int64;
begin
  MaxData := AData + ASize - 1;
  while AData <= MaxData do
  begin
    DbgOut(FPDBG_DWARF_VERBOSE, [AIndent]);
    case AData^ of
      DW_OP_addr: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_addr ', MakeAddressString(@AData[1])]);
        Inc(AData, 4);
      end;
      DW_OP_deref: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_deref']);
      end;
      DW_OP_const1u: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_const1u ', AData[1]]);
        Inc(AData, 1);
      end;
      DW_OP_const1s: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_const1s ', PShortInt(@AData[1])^]);
        Inc(AData, 1);
      end;
      DW_OP_const2u: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_const2u ', PWord(@AData[1])^]);
        Inc(AData, 2);
      end;
      DW_OP_const2s: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_const2s ', PSmallInt(@AData[1])^]);
        Inc(AData, 2);
      end;
      DW_OP_const4u: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_const4u ', PLongWord(@AData[1])^]);
        Inc(AData, 4);
      end;
      DW_OP_const4s: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_const4s ', PLongInt(@AData[1])^]);
        Inc(AData, 4);
      end;
      DW_OP_const8u: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_const8u ', PQWord(@AData[1])^]);
        Inc(AData, 8);
      end;
      DW_OP_const8s: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_const8s ', PInt64(@AData[1])^]);
        Inc(AData, 8);
      end;
      DW_OP_constu: begin
        Inc(AData);
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_constu ', ULEB128toOrdinal(AData)]);;
        Dec(AData);
      end;
      DW_OP_consts: begin
        Inc(AData);
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_consts ', SLEB128toOrdinal(AData)]);;
        Dec(AData);
      end;
      DW_OP_dup: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_dup']);
      end;
      DW_OP_drop: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_drop']);
      end;
      DW_OP_over: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_over']);
      end;
      DW_OP_pick: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_pick ', AData[1]]);
        Inc(AData, 1);
      end;
      DW_OP_swap: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_swap']);
      end;
      DW_OP_rot: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_rot']);
      end;
      DW_OP_xderef: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_xderef']);
      end;
      DW_OP_abs: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_abs']);
      end;
      DW_OP_and: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_and']);
      end;
      DW_OP_div: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_div']);
      end;
      DW_OP_minus: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_minus']);
      end;
      DW_OP_mod: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_mod']);
      end;
      DW_OP_mul: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_mul']);
      end;
      DW_OP_neg: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_neg']);
      end;
      DW_OP_not: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_not']);
      end;
      DW_OP_or: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_or']);
      end;
      DW_OP_plus: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_plus']);
      end;
      DW_OP_plus_uconst: begin
        Inc(AData);
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_plus_uconst ', ULEB128toOrdinal(AData)]);;
        Dec(AData);
      end;
      DW_OP_shl: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_shl']);
      end;
      DW_OP_shr: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_shr']);
      end;
      DW_OP_shra: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_shra']);
      end;
      DW_OP_xor: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_xor']);
      end;
      DW_OP_skip: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_skip ', PSmallInt(@AData[1])^]);
        Inc(AData, 2);
      end;
      DW_OP_bra: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_bra ', PSmallInt(@AData[1])^]);
        Inc(AData, 2);
      end;
      DW_OP_eq: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_eq']);
      end;
      DW_OP_ge: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_ge']);
      end;
      DW_OP_gt: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_gt']);
      end;
      DW_OP_le: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_le']);
      end;
      DW_OP_lt: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_lt']);
      end;
      DW_OP_ne: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_ne']);
      end;
      DW_OP_lit0..DW_OP_lit31: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_lit', AData^ - DW_OP_lit0]);
      end;
      DW_OP_reg0..DW_OP_reg31: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_reg', AData^ - DW_OP_reg0]);
      end;
      DW_OP_breg0..DW_OP_breg31: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_breg', AData^ - DW_OP_breg0]);
        Inc(AData);
        v := SLEB128toOrdinal(AData);
        Dec(AData);
        if v >= 0
        then DbgOut(FPDBG_DWARF_VERBOSE, ['+']);
        DbgOut(FPDBG_DWARF_VERBOSE, [v]);
      end;
      DW_OP_regx: begin
        Inc(AData);
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_regx ', ULEB128toOrdinal(AData)]);
        Dec(AData);
      end;
      DW_OP_fbreg: begin
        Inc(AData);
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_fbreg ', SLEB128toOrdinal(AData)]);
        Dec(AData);
      end;
      DW_OP_bregx: begin
        Inc(AData);
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_bregx ', ULEB128toOrdinal(AData)]);
        v := SLEB128toOrdinal(AData);
        Dec(AData);
        if v >= 0
        then DbgOut(FPDBG_DWARF_VERBOSE, ['+']);
        DbgOut(FPDBG_DWARF_VERBOSE, [v]);
      end;
      DW_OP_piece: begin
        Inc(AData);
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_piece ', ULEB128toOrdinal(AData)]);
        Dec(AData);
      end;
      DW_OP_deref_size: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_deref_size ', AData[1]]);
        Inc(AData);
      end;
      DW_OP_xderef_size: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_xderef_size', AData[1]]);
        Inc(AData);
      end;
      DW_OP_nop: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_nop']);
      end;
      DW_OP_push_object_address: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_push_object_address']);
      end;
      DW_OP_call2: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_call2 ', PWord(@AData[1])^]);
        Inc(AData, 2);
      end;
      DW_OP_call4: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_call4 ', PLongWord(@AData[1])^]);
        Inc(AData, 4);
      end;
      DW_OP_call_ref: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_call_ref ', MakeAddressString(@AData[1])]);
        Inc(AData, 4);
      end;
      DW_OP_form_tls_address: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_form_tls_address']);
      end;
      DW_OP_call_frame_cfa: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_call_frame_cfa']);
      end;
      DW_OP_bit_piece: begin
        Inc(AData);
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_bit_piece ', ULEB128toOrdinal(AData), ' ', ULEB128toOrdinal(AData)]);
        Dec(AData);
      end;
      DW_OP_lo_user..DW_OP_hi_user: begin
        DbgOut(FPDBG_DWARF_VERBOSE, ['DW_OP_user=', AData^]);
      end;
    else
      DbgOut(FPDBG_DWARF_VERBOSE, ['Unknown DW_OP_', AData^]);
    end;
    Inc(AData);
    DebugLn(FPDBG_DWARF_VERBOSE, ['']);
  end;
end;

procedure TDwarfAbbrevDecoder.DecodeLocationList(AReference: QWord; const AIndent: String);
begin

end;

procedure TDwarfAbbrevDecoder.InternalDecode(AData: Pointer; AMaxData: Pointer; const AIndent: String);
  procedure Dump(var p: PByte; count: QWord);
  var
    n: integer;
  begin
    for n := 1 to Min(80, count) do
    begin
      DbgOut(FPDBG_DWARF_VERBOSE, [IntToHex(p^, 2), ' ']);
      Inc(p);
    end;
    if Count > 80
    then begin
      Inc(p, Count - 80);
      DbgOut(FPDBG_DWARF_VERBOSE, ['...']);
    end;
  end;
  procedure DumpStr(var p: PChar);
  begin
    while p^ <> #0 do
    begin
      case p^ of
        #32..#127: DbgOut(FPDBG_DWARF_VERBOSE, [p^]);
      else
        DbgOut(FPDBG_DWARF_VERBOSE, ['<', IntToHex(Ord(p^), 2), '>']);
      end;
      Inc(p);
    end;
    Inc(p);
  end;

var
  Attribute: Cardinal;
  Abbrev, Form: Cardinal;
  Def: TDwarfAbbrev;
  idx: Integer;
  Value: QWord;
  ValueSize: QWord;
  ValuePtr, p: Pointer;
  Indent: String;
  Level: Integer;
  ADefs: PDwarfAbbrevEntry;
begin
  // Tag - should not exist. Load all scopes
  FCU.ScanAllEntries;

  Indent := AIndent;
  Level := 0;
  ADefs := FCU.FAbbrevList.EntryPointer[0];
  while (AData <= AMaxData) and (Level >= 0) do
  begin
p := AData;
    Abbrev := ULEB128toOrdinal(AData);
    if Abbrev = 0
    then begin
      Dec(Level);
      SetLength(Indent, Length(Indent) - 2);
      if Level >= 0
      then  DebugLn(FPDBG_DWARF_VERBOSE, [Indent, ' \--']);
      Continue;
    end;
    DbgOut(FPDBG_DWARF_VERBOSE, [Indent, 'abbrev: ', Abbrev]);
    if not FCU.GetDefinition(p, Def)
    then begin
      DebugLn(FPDBG_DWARF_WARNINGS, ['Error: Abbrev not found: ', Abbrev]);
      Exit;
    end;
    DbgOut(FPDBG_DWARF_VERBOSE, [', tag: ', Def.tag, '=', DwarfTagToString(Def.tag)]);
    if (dafHasChildren in Def.flags)
    then begin
      DebugLn(FPDBG_DWARF_VERBOSE, ['']);
      DebugLn(FPDBG_DWARF_VERBOSE, [', has children']);
      Inc(Level);
    end
    else DebugLn(FPDBG_DWARF_VERBOSE, ['']);

    for idx := Def.Index to Def.Index + Def.Count - 1 do
    begin
      Form := ADefs[idx].Form;
      Attribute := ADefs[idx].Attribute;
      DbgOut(FPDBG_DWARF_VERBOSE, [Indent, ' attrib: ', Attribute, '=', DwarfAttributeToString(Attribute)]);
      DbgOut(FPDBG_DWARF_VERBOSE, [', form: ', Form, '=', DwarfAttributeFormToString(Form)]);
      
      ValueSize := 0;
      ValuePtr := nil;
      Value := 0;
      repeat
        DbgOut(FPDBG_DWARF_VERBOSE, [', value: ']);
        case Form of
          DW_FORM_addr     : begin
            Value := FCU.ReadAddressAtPointer(AData);
            ValuePtr := Pointer(PtrUInt(Value));
            ValueSize := FCU.FAddressSize;
            DbgOut(FPDBG_DWARF_VERBOSE, ['$'+IntToHex(Value, FCU.FAddressSize * 2)]);
            Inc(AData, FCU.FAddressSize);
          end;
          DW_FORM_block    : begin
            ValueSize := ULEB128toOrdinal(AData);
            ValuePtr := AData;
            DbgOut(FPDBG_DWARF_VERBOSE, ['Size=', ValueSize, ', Data=']);
            Dump(AData, ValueSize);
          end;
          DW_FORM_block1   : begin
            ValueSize := PByte(AData)^;
            Inc(AData, 1);
            ValuePtr := AData;
            DbgOut(FPDBG_DWARF_VERBOSE, ['Size=', ValueSize, ', Data=']);
            Dump(AData, ValueSize);
          end;
          DW_FORM_block2   : begin
            ValueSize := PWord(AData)^;
            Inc(AData, 2);
            ValuePtr := AData;
            DbgOut(FPDBG_DWARF_VERBOSE, ['Size=', ValueSize, ', Data=']);
            Dump(AData, ValueSize);
          end;
          DW_FORM_block4   : begin
            ValueSize := PLongWord(AData)^;
            Inc(AData, 4);
            ValuePtr := AData;
            DbgOut(FPDBG_DWARF_VERBOSE, ['Size=', ValueSize, ', Data=']);
            Dump(AData, ValueSize);
          end;
          DW_FORM_data1    : begin
            Value := PByte(AData)^;
            ValueSize := 1;
            DbgOut(FPDBG_DWARF_VERBOSE, ['$'+IntToHex(Value, 2)]);
            Inc(AData, 1);
          end;
          DW_FORM_data2    : begin
            Value := PWord(AData)^;
            ValueSize := 2;
            DbgOut(FPDBG_DWARF_VERBOSE, ['$'+IntToHex(Value, 4)]);
            Inc(AData, 2);
          end;
          DW_FORM_data4    : begin
            Value := PLongWord(AData)^;
            ValueSize := 4;
            DbgOut(FPDBG_DWARF_VERBOSE, ['$'+IntToHex(Value, 8)]);
            Inc(AData, 4);
          end;
          DW_FORM_data8    : begin
            Value := PQWord(AData)^;
            ValueSize := 8;
            DbgOut(FPDBG_DWARF_VERBOSE, ['$'+IntToHex(Value, 16)]);
            Inc(AData, 8);
          end;
          DW_FORM_sdata    : begin
            p := AData;
            Value := ULEB128toOrdinal(AData);
            ValueSize := PtrUInt(AData) - PtrUInt(p);
            DbgOut(FPDBG_DWARF_VERBOSE, ['$'+IntToHex(Value, ValueSize * 2)]);
          end;
          DW_FORM_udata    : begin
            p := AData;
            Value := ULEB128toOrdinal(AData);
            ValueSize := PtrUInt(AData) - PtrUInt(p);
            DbgOut(FPDBG_DWARF_VERBOSE, ['$'+IntToHex(Value, ValueSize * 2)]);
          end;
          DW_FORM_flag     : begin
            Value := PByte(AData)^;
            ValueSize := 1;
            DbgOut(FPDBG_DWARF_VERBOSE, ['$'+IntToHex(Value, 2)]);
            Inc(AData, 1);
          end;
          DW_FORM_ref1     : begin
            Value := PByte(AData)^;
            ValueSize := 1;
            DbgOut(FPDBG_DWARF_VERBOSE, ['$'+IntToHex(Value, 2)]);
            Inc(AData, 1);
          end;
          DW_FORM_ref2     : begin
            Value := PWord(AData)^;
            ValueSize := 2;
            DbgOut(FPDBG_DWARF_VERBOSE, ['$'+IntToHex(Value, 4)]);
            Inc(AData, 2);
          end;
          DW_FORM_ref4     : begin
            Value := PLongWord(AData)^;
            ValueSize := 4;
            DbgOut(FPDBG_DWARF_VERBOSE, ['$'+IntToHex(Value, 8)]);
            Inc(AData, 4);
          end;
          DW_FORM_ref8     : begin
            Value := PQWord(AData)^;
            ValueSize := 8;
            DbgOut(FPDBG_DWARF_VERBOSE, ['$'+IntToHex(Value, 16)]);
            Inc(AData, 8);
          end;
          DW_FORM_ref_udata: begin
            p := AData;
            Value := ULEB128toOrdinal(AData);
            ValueSize := PtrUInt(AData) - PtrUInt(p);
            DbgOut(FPDBG_DWARF_VERBOSE, ['$'+IntToHex(Value, ValueSize * 2)]);
          end;
          DW_FORM_ref_addr : begin
            Value := FCU.ReadAddressAtPointer(AData);
            ValuePtr := Pointer(PtrUInt(Value));
            ValueSize := FCU.FAddressSize;
            DbgOut(FPDBG_DWARF_VERBOSE, ['$'+IntToHex(Value, FCU.FAddressSize * 2)]);
            Inc(AData, FCU.FAddressSize);
          end;
          DW_FORM_string   : begin
            ValuePtr := AData;
            DumpStr(AData);
            ValueSize := PtrUInt(AData) - PtrUInt(ValuePtr);
          end;
          DW_FORM_strp     : begin
            Value := FCU.ReadAddressAtPointer(AData);
            ValueSize := FCU.FAddressSize;
            DbgOut(FPDBG_DWARF_VERBOSE, ['$'+IntToHex(Value, FCU.FAddressSize * 2)]);
            Inc(AData, FCU.FAddressSize);
          end;
          DW_FORM_indirect : begin
            Form := ULEB128toOrdinal(AData);
            DbgOut(FPDBG_DWARF_VERBOSE, ['indirect form: ', Form, '=', DwarfAttributeFormToString(Form)]);
            Continue;
          end;
        else
          DebugLn(FPDBG_DWARF_WARNINGS, ['Error: Unknown Form: ', Form]);
          Exit;
        end;
        Break;
      until False;

      case Attribute of
        DW_AT_accessibility: begin
          DebugLn(FPDBG_DWARF_VERBOSE, ['=', DwarfAccessibilityToString(Value)]);
        end;
        DW_AT_data_member_location: begin
          DebugLn(FPDBG_DWARF_VERBOSE, ['-->']);
          DecodeLocation(ValuePtr, ValueSize, Indent + '  ');
        end;
        DW_AT_encoding: begin
          DebugLn(FPDBG_DWARF_VERBOSE, ['=', DwarfBaseTypeEncodingToString(Value)]);
        end;
        DW_AT_language: begin
          DebugLn(FPDBG_DWARF_VERBOSE, ['=', DwarfLanguageToString(Value)]);
        end;
        DW_AT_identifier_case: begin
          DebugLn(FPDBG_DWARF_VERBOSE, ['=', DwarfIdentifierCaseToString(Value)]);
        end;
        DW_AT_location: begin
          if ValuePtr = nil
          then begin
            DebugLn(FPDBG_DWARF_VERBOSE, ['-->']);
            DecodeLocationList(Value, AIndent + '  ');
          end
          else begin
            DebugLn(FPDBG_DWARF_VERBOSE, ['-->']);
            DecodeLocation(ValuePtr, ValueSize, Indent + '  ');
          end;
        end;
        DW_AT_type: begin
          DebugLn(FPDBG_DWARF_VERBOSE, ['-->']);
          try
            p := FCU.FOwner.FSections[dsInfo].RawData + Value - FCU.FOwner.FImageBase - FCU.FOwner.FSections[dsInfo].VirtualAdress;
            InternalDecode(p, p, Indent + '  ');
          except
            on E: Exception do DebugLn(FPDBG_DWARF_WARNINGS, [AIndent, '  ', E.Message]);
          end;
        end;
      else
        DebugLn(FPDBG_DWARF_VERBOSE, ['']);
      end;
    end;

    if (dafHasChildren in Def.flags)
    then begin
      DebugLn(FPDBG_DWARF_VERBOSE, [Indent, ' /--']);
      Indent := Indent + ' |';
    end;
  end;
end;


function TDwarfAbbrevDecoder.MakeAddressString(AData: Pointer): string;
begin
  if FCU.FAddressSize = 4
  then Result := '$'+IntToHex(PLongWord(AData)^, 8)
  else Result := '$'+IntToHex(PQWord(AData)^, 16);
end;

{ TDwarfStatementDecoder }

constructor TDwarfStatementDecoder.Create(ACompilationUnit: TDwarfCompilationUnit);
begin
  inherited Create;
  FCU := ACompilationUnit;
end;

procedure TDwarfStatementDecoder.Decode;
begin
  if FCU.FLineInfo.Header = nil
  then begin
    DebugLn(FPDBG_DWARF_WARNINGS, ['No lineinfo']);
    Exit;
  end;
  InternalDecode(FCU.FLineInfo.Header, FCU.FOwner.FSections[dsInfo].RawData + FCU.FOwner.FSections[dsInfo].Size);
end;

procedure TDwarfStatementDecoder.InternalDecode(AData: Pointer; AMaxData: Pointer; const AIndent: String);
var
  Info: PDwarfLNPInfoHeader;

  Address: QWord;
  Line: Int64;
  FileNr: Cardinal;
  Column: Cardinal;
  IsStmt: Boolean;
  BasicBlock: Boolean;
  PrologueEnd: Boolean;
  EpilogueBegin: Boolean;
  Isa: QWord;

  procedure AddRow(ALast: Boolean = False);
  begin
    DbgOut(FPDBG_DWARF_VERBOSE, ['> ']);
    DbgOut(FPDBG_DWARF_VERBOSE, ['Address=$', IntToHex(Address, FCU.FAddressSize * 2)]);
    DbgOut(FPDBG_DWARF_VERBOSE, [', Line=',Line]);
    DbgOut(FPDBG_DWARF_VERBOSE, [', FileNr=',FileNr]);
    DbgOut(FPDBG_DWARF_VERBOSE, [', Column=',Column]);
    DbgOut(FPDBG_DWARF_VERBOSE, [', IsStmt=',IsStmt]);
    DbgOut(FPDBG_DWARF_VERBOSE, [', BasicBlock=',BasicBlock]);
    DbgOut(FPDBG_DWARF_VERBOSE, [', PrologueEnd=',PrologueEnd]);
    DbgOut(FPDBG_DWARF_VERBOSE, [', EpilogueBegin=',EpilogueBegin]);
    DbgOut(FPDBG_DWARF_VERBOSE, [', Isa=',Isa]);
    DebugLn(FPDBG_DWARF_VERBOSE, ['']);
    if ALast
    then DebugLn(FPDBG_DWARF_VERBOSE, ['> ---------']);
  end;
  
  procedure DoAdjust(AOpcode: Byte);
  begin
    Dec(AOpcode, Info^.OpcodeBase);
    if Info^.LineRange = 0
    then begin
      Inc(Address, AOpcode * Info^.MinimumInstructionLength);
    end
    else begin
      Inc(Address, (AOpcode div Info^.LineRange) * Info^.MinimumInstructionLength);
      Inc(Line, Info^.LineBase + (AOpcode mod Info^.LineRange));
    end;
  end;
  
  procedure DoReset;
  begin
    Address := 0;
    Line := 1;
    FileNr := 1;
    Column := 0;
    IsStmt := Info^.DefaultIsStmt <> 0;
    BasicBlock := False;
    PrologueEnd := False;
    EpilogueBegin := False;
    Isa := 0;
  end;


var
  LNP32: PDwarfLNPHeader32 absolute AData;
  LNP64: PDwarfLNPHeader64 absolute AData;
  UnitLength: QWord;
  Version: Word;
  HeaderLength: QWord;
  n: integer;
  ptr: Pointer;
  p: Pointer;
  pb: PByte absolute p;
  pc: PChar absolute p;
  DataEnd: Pointer;
  DataStart: Pointer;
  UValue: QWord;
  SValue: Int64;
begin
  DebugLn(FPDBG_DWARF_VERBOSE, ['FileName: ', FCU.FFileName]);

  if LNP64^.Signature = DWARF_HEADER64_SIGNATURE
  then begin
    UnitLength := LNP64^.UnitLength;
    DataEnd := Pointer(@LNP64^.Version) + UnitLength;
    Version := LNP64^.Version;
    HeaderLength := LNP64^.HeaderLength;
    Info := @LNP64^.Info;
  end
  else begin
    UnitLength := LNP32^.UnitLength;
    DataEnd := Pointer(@LNP32^.Version) + UnitLength;
    Version := LNP32^.Version;
    HeaderLength := LNP32^.HeaderLength;
    Info := @LNP32^.Info;
  end;
  DataStart := PByte(Info) + HeaderLength;

  DebugLn(FPDBG_DWARF_VERBOSE, ['UnitLength: ', UnitLength]);
  DebugLn(FPDBG_DWARF_VERBOSE, ['Version: ', Version]);
  DebugLn(FPDBG_DWARF_VERBOSE, ['HeaderLength: ', HeaderLength]);

  DebugLn(FPDBG_DWARF_VERBOSE, ['MinimumInstructionLength: ', Info^.MinimumInstructionLength]);
  DebugLn(FPDBG_DWARF_VERBOSE, ['DefaultIsStmt: ', Info^.DefaultIsStmt]);
  DebugLn(FPDBG_DWARF_VERBOSE, ['LineBase: ', Info^.LineBase]);
  DebugLn(FPDBG_DWARF_VERBOSE, ['LineRange: ', Info^.LineRange]);
  DebugLn(FPDBG_DWARF_VERBOSE, ['OpcodeBase: ', Info^.OpcodeBase]);
  p := @Info^.StandardOpcodeLengths;
  DebugLn(FPDBG_DWARF_VERBOSE, ['StandardOpcodeLengths:']);
  for n := 1 to Info^.OpcodeBase - 1 do
  begin
    DebugLn(FPDBG_DWARF_VERBOSE, ['  [', n, '] ', pb^]);
    Inc(pb);
  end;

  DebugLn(FPDBG_DWARF_VERBOSE, ['IncludeDirectories:']);
  while pc^ <> #0 do
  begin
    DbgOut(FPDBG_DWARF_VERBOSE, ['  ']);
    repeat
      DbgOut(FPDBG_DWARF_VERBOSE, [pc^]);
      Inc(pc);
    until pc^ = #0;
    DebugLn(FPDBG_DWARF_VERBOSE, ['']);
    Inc(pc);
  end;
  Inc(pc);
  DebugLn(FPDBG_DWARF_VERBOSE, ['FileNames:']);
  while pc^ <> #0 do
  begin
    DbgOut(FPDBG_DWARF_VERBOSE, ['  ']);
    repeat
      DbgOut(FPDBG_DWARF_VERBOSE, [pc^]);
      Inc(pc);
    until pc^ = #0;
    Inc(pc);
    DbgOut(FPDBG_DWARF_VERBOSE, [', diridx=', ULEB128toOrdinal(p)]);
    DbgOut(FPDBG_DWARF_VERBOSE, [', last modified=', ULEB128toOrdinal(p)]);
    DbgOut(FPDBG_DWARF_VERBOSE, [', length=', ULEB128toOrdinal(p)]);
    DebugLn(FPDBG_DWARF_VERBOSE, ['']);
  end;
  
  DebugLn(FPDBG_DWARF_VERBOSE, ['Program:']);

  p := DataStart;
  DoReset;
  
  while p < DataEnd do
  begin
    DbgOut(FPDBG_DWARF_VERBOSE, ['  ']);
    if (pb^ > 0) and (pb^ < Info^.OpcodeBase)
    then begin
      // Standard opcode
      case pb^ of
        DW_LNS_copy: begin
          Inc(p);
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_LNS_copy']);
          AddRow;
          BasicBlock := False;
          PrologueEnd := False;
          EpilogueBegin := False;
        end;
        DW_LNS_advance_pc: begin
          Inc(p);
          UValue := ULEB128toOrdinal(p);
          Inc(Address, UValue);
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_LNS_advance_pc ', UValue]);
        end;
        DW_LNS_advance_line: begin
          Inc(p);
          SValue := SLEB128toOrdinal(p);
          Inc(Line, SValue);
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_LNS_advance_line ', SValue]);
        end;
        DW_LNS_set_file: begin
          Inc(p);
          UValue := ULEB128toOrdinal(p);
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_LNS_set_file ', UVAlue]);
          FileNr := UValue;
        end;
        DW_LNS_set_column: begin
          Inc(p);
          UValue := ULEB128toOrdinal(p);
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_LNS_set_column ', UValue]);
          Column := UValue;
        end;
        DW_LNS_negate_stmt: begin
          Inc(p);
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_LNS_negate_stmt']);
          IsStmt := not IsStmt;
        end;
        DW_LNS_set_basic_block: begin
          Inc(p);
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_LNS_set_basic_block']);
          BasicBlock := True;
        end;
        DW_LNS_const_add_pc: begin
          Inc(p);
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_LNS_const_add_pc']);
          DoAdjust(255);
        end;
        DW_LNS_fixed_advance_pc: begin
          Inc(p);
          Inc(Address, PWord(p)^);
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_LNS_fixed_advance_pc ', PWord(p)^]);
          Inc(p, 2);
        end;
        DW_LNS_set_prologue_end: begin
          Inc(p);
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_LNS_set_prologue_end']);
          PrologueEnd := True;
        end;
        DW_LNS_set_epilogue_begin: begin
          Inc(p);
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_LNS_set_epilogue_begin']);
          EpilogueBegin := True;
        end;
        DW_LNS_set_isa: begin
          Inc(p);
          UValue := ULEB128toOrdinal(p);
          Isa := UValue;
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_LNS_set_isa ', UValue]);
        end;
      else
        DbgOut(FPDBG_DWARF_VERBOSE, ['unknown opcode: ', pb^]);
        Inc(p, PByte(@Info^.StandardOpcodeLengths)[pb^-1]);
      end;
      Continue;
    end;

    if pb^ = 0
    then begin
      // Extended opcode
      Inc(p);
      UValue := ULEB128toOrdinal(p); // instruction length
      
      case pb^ of
        DW_LNE_end_sequence: begin
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_LNE_end_sequence']);
          AddRow(True);
          DoReset;
          //Inc(p, UValue);
          //Break;
        end;
        DW_LNE_set_address: begin
          if LNP64^.Signature = DWARF_HEADER64_SIGNATURE
          then Address := PQWord(pb+1)^
          else Address := PLongWord(pb+1)^;
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_LNE_set_address $', IntToHex(Address, FCU.FAddressSize * 2)]);
        end;
        DW_LNE_define_file: begin
          ptr := p;
          Inc(ptr);
          DbgOut(FPDBG_DWARF_VERBOSE, ['DW_LNE_define_file name=']);
          repeat
            DbgOut(FPDBG_DWARF_VERBOSE, [PChar(ptr)^]);
            Inc(ptr);
          until PChar(ptr)^ = #0;
          Inc(ptr);
          DbgOut(FPDBG_DWARF_VERBOSE, [', diridx=', ULEB128toOrdinal(ptr)]);
          DbgOut(FPDBG_DWARF_VERBOSE, [', last modified=', ULEB128toOrdinal(ptr)]);
          DbgOut(FPDBG_DWARF_VERBOSE, [', length=', ULEB128toOrdinal(ptr)]);
          DebugLn(FPDBG_DWARF_VERBOSE, ['']);
        end;
      else
        DbgOut(FPDBG_DWARF_VERBOSE, ['unknown extended opcode: ', pb^]);
      end;
      Inc(p, UValue);
    end
    else begin
      DebugLn(FPDBG_DWARF_VERBOSE, ['Special opcode: ', pb^]);
      // Special opcode
      DoAdjust(pb^);
      AddRow;
      BasicBlock := False;
      PrologueEnd := False;
      EpilogueBegin := False;
      Inc(p);
    end;
  end;
end;

{ TVerboseDwarfCallframeDecoder }

constructor TVerboseDwarfCallframeDecoder.Create(ALoader: TDbgImageLoader);
begin
  inherited Create;
  FLoader := Aloader;
end;

procedure TVerboseDwarfCallframeDecoder.Decode;
var
  Section: PDbgImageSection;
begin
  Section := FLoader.Section[DWARF_SECTION_NAME[dsFrame]];
  if Section <> nil
  then InternalDecode(Section^.RawData, Section^.Size, Section^.VirtualAdress);
end;

procedure TVerboseDwarfCallframeDecoder.InternalDecode(AData: Pointer; ASize: QWord; AStart: QWord);
var
  Is64bit: boolean;

  procedure DecodeInstructions(p: Pointer; MaxAddr: Pointer);
  var
    pb: PByte absolute p;
    pw: PWord absolute p;
    pc: PCardinal absolute p;
    pq: PQWord absolute p;
    q: QWord;
  begin
    repeat
      DbgOut(FPDBG_DWARF_VERBOSE, [' ']);
      Inc(pb);
      case pb[-1] of
        DW_CFA_nop: begin
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_CFA_nop']);
        end;
        DW_CFA_set_loc: begin
          // address
          DbgOut(FPDBG_DWARF_VERBOSE, ['DW_CFA_set_loc $']);
          if Is64Bit
          then begin
            DebugLn(FPDBG_DWARF_VERBOSE, [IntToHex(pq^, 16)]);
            Inc(pq);
          end
          else begin
            DebugLn(FPDBG_DWARF_VERBOSE, [IntToHex(pc^, 8)]);
            Inc(pc);
          end;
        end;
        DW_CFA_advance_loc1: begin
          // 1-byte delta
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_CFA_advance_loc1 ', pb^, ' * caf']);
          Inc(pb);
        end;
        DW_CFA_advance_loc2: begin
          // 2-byte delta
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_CFA_advance_loc2 ', pw^, ' * caf']);
          Inc(pw);
        end;
        DW_CFA_advance_loc4: begin
          // 4-byte delta
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_CFA_advance_loc4 ', pc^, ' * caf']);
          Inc(pw);
        end;
        DW_CFA_offset_extended: begin
          // ULEB128 register, ULEB128 offset
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_CFA_offset_extended R', ULEB128toOrdinal(p), ' + ', ULEB128toOrdinal(p), ' * daf']);
        end;
        DW_CFA_restore_extended: begin
          // ULEB128 register
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_CFA_restore_extended R', ULEB128toOrdinal(p)]);
        end;
        DW_CFA_undefined: begin
          // ULEB128 register
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_CFA_undefined R', ULEB128toOrdinal(p)]);
        end;
        DW_CFA_same_value: begin
          // ULEB128 register
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_CFA_same_value R', ULEB128toOrdinal(p)]);
        end;
        DW_CFA_register: begin
          // ULEB128 register, ULEB128 register
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_CFA_register R', ULEB128toOrdinal(p), ' R', ULEB128toOrdinal(p)]);
        end;
        DW_CFA_remember_state: begin
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_CFA_remember_state']);
        end;
        DW_CFA_restore_state: begin
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_CFA_restore_state']);
        end;
        DW_CFA_def_cfa: begin
          // ULEB128 register, ULEB128 offset
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_CFA_def_cfa R', ULEB128toOrdinal(p), ' + ', ULEB128toOrdinal(p)]);
        end;
        DW_CFA_def_cfa_register: begin
          // ULEB128 register
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_CFA_def_cfa_register R', ULEB128toOrdinal(p)]);
        end;
        DW_CFA_def_cfa_offset: begin
          // ULEB128 offset
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_CFA_def_cfa_offset ', ULEB128toOrdinal(p)]);
        end;
        // --- DWARF3 ---
        DW_CFA_def_cfa_expression: begin
          // BLOCK
          q := ULEB128toOrdinal(p);
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_CFA_def_cfa_expression, lenght=',q]);
          Inc(p, q);
        end;
        DW_CFA_expression: begin
          // ULEB128 register, BLOCK
          DbgOut(FPDBG_DWARF_VERBOSE, ['DW_CFA_expression R', ULEB128toOrdinal(p), ' lenght=',q]);
          q := ULEB128toOrdinal(p);
          DebugLn(FPDBG_DWARF_VERBOSE, [q]);
          Inc(p, q);
        end;
        DW_CFA_offset_extended_sf: begin
          // ULEB128 register, SLEB128 offset
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_CFA_offset_extended_sf R', ULEB128toOrdinal(p), ' + ', SLEB128toOrdinal(p), ' * daf']);
        end;
        DW_CFA_def_cfa_sf: begin
          // ULEB128 register, SLEB128 offset
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_CFA_def_cfa_sf R', ULEB128toOrdinal(p), ' + ', SLEB128toOrdinal(p), ' * daf']);
        end;
        DW_CFA_def_cfa_offset_sf: begin
          // SLEB128 offset
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_CFA_def_cfa_offset_sf ', SLEB128toOrdinal(p), ' * daf' ]);
        end;
        DW_CFA_val_offset: begin
          // ULEB128         , ULEB128
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_CFA_val_offset R', ULEB128toOrdinal(p), ' + ', ULEB128toOrdinal(p), ' * daf']);
        end;
        DW_CFA_val_offset_sf: begin
          // ULEB128         , SLEB128
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_CFA_val_offset_sf R', ULEB128toOrdinal(p), ' + ', SLEB128toOrdinal(p), ' * daf']);
        end;
        DW_CFA_val_expression: begin
          // ULEB128         , BLOCK
          DbgOut(FPDBG_DWARF_VERBOSE, ['DW_CFA_val_expression R', ULEB128toOrdinal(p), ' lenght=',q]);
          q := ULEB128toOrdinal(p);
          DebugLn(FPDBG_DWARF_VERBOSE, [q]);
          Inc(p, q);
        end;
        // ---  ---
        DW_CFA_lo_user..DW_CFA_hi_user: begin
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_CFA_user=', pb^]);
        end;
        // ---  ---
        DW_CFA_advance_loc..DW_CFA_offset-1: begin
          // delta
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_CFA_advance_loc ', pb[-1] and $3F, ' * caf']);
        end;
        DW_CFA_offset..DW_CFA_restore-1: begin
          // register  ULEB128 offset
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_CFA_offset R', pb[-1] and $3F, ' + ', ULEB128toOrdinal(p),' * caf']);
        end;
        DW_CFA_restore..$FF: begin
         // register
          DebugLn(FPDBG_DWARF_VERBOSE, ['DW_CFA_restore R', pb[-1] and $3F]);
        end;
      else
        DebugLn(FPDBG_DWARF_VERBOSE, ['Undefined $', IntToHex(pb[-1], 2)]);
      end;
    until p >= MaxAddr;
  end;

var
  p, next: Pointer;
  pb: PByte absolute p;
  pw: PWord absolute p;
  pi: PInteger absolute p;
  pc: PCardinal absolute p;
  pq: PQWord absolute p;
  
  len: QWord;
  version: Byte;
  IsCie: Boolean;

  s: String;
begin
  p := AData;
  while p < Adata + ASize do
  begin
    DebugLn(FPDBG_DWARF_VERBOSE, ['[', PtrUInt(p) - PtrUInt(AData), ']']);

    Is64bit := pi^ = -1;
    if Is64bit
    then begin
      Inc(pi);
      len := pq^;
      Inc(pq);
      IsCie := Int64(pq^) = -1;
    end
    else begin
      len := pc^;
      Inc(pc);
      IsCie := pi^ = -1;
    end;
    next := p + len;

    if IsCie
    then DebugLn(FPDBG_DWARF_VERBOSE, ['=== CIE ==='])
    else DebugLn(FPDBG_DWARF_VERBOSE, ['--- FDE ---']);

    DebugLn(FPDBG_DWARF_VERBOSE, ['Length: ', len]);

    if IsCie
    then begin
      Inc(pi);
      version := pb^;
      DebugLn(FPDBG_DWARF_VERBOSE, ['Version: ', version]);
      Inc(pb);
      S := Pchar(p);
      DebugLn(FPDBG_DWARF_VERBOSE, ['Augmentation: ', S]);
      Inc(p, Length(s) + 1);
      DebugLn(FPDBG_DWARF_VERBOSE, ['Code alignment factor (caf): ', ULEB128toOrdinal(p)]);
      DebugLn(FPDBG_DWARF_VERBOSE, ['Data alignment factor (daf): ', SLEB128toOrdinal(p)]);
      DbgOut(FPDBG_DWARF_VERBOSE, ['Return addr: R']);
      if version <= 2
      then begin
        DebugLn(FPDBG_DWARF_VERBOSE, [pb^]);
        Inc(pb);
      end
      else DebugLn(FPDBG_DWARF_VERBOSE, [ULEB128toOrdinal(p)]);
    end
    else begin
      if pc^ > ASize
      then DebugLn(FPDBG_DWARF_VERBOSE, ['CIE: $', IntToHex(pc^, 8), ' (=adress ?) -> offset: ', pc^ - AStart - FLoader.ImageBase])
      else DebugLn(FPDBG_DWARF_VERBOSE, ['CIE: ', pc^]);
      Inc(pc);
      DebugLn(FPDBG_DWARF_VERBOSE, ['InitialLocation: $', IntToHex(pc^, 8)]);
      Inc(pc);
      DebugLn(FPDBG_DWARF_VERBOSE, ['Address range: ', pc^]);
      Inc(pc);
    end;
    DebugLn(FPDBG_DWARF_VERBOSE, ['Instructions:']);
    DecodeInstructions(p, next);

    p := next;
  end;
end;

initialization
  FPDBG_DWARF_WARNINGS := DebugLogger.RegisterLogGroup('FPDBG_DWARF_WARNINGS' {$IFDEF FPDBG_DWARF_WARNINGS} , True {$ENDIF} );
  FPDBG_DWARF_VERBOSE  := DebugLogger.RegisterLogGroup('FPDBG_DWARF_VERBOSE' {$IFDEF FPDBG_DWARF_VERBOSE} , True {$ENDIF} );
  FPDBG_DWARF_SEARCH   := DebugLogger.RegisterLogGroup('FPDBG_DWARF_SEARCH' {$IFDEF FPDBG_DWARF_SEARCH} , True {$ENDIF} );

end.

