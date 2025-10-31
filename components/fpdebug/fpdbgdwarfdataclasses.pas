{
 ---------------------------------------------------------------------------
 fpdbgdwarfdataclasses.pas  -  Native Freepascal debugger - Dwarf symbol reader
 ---------------------------------------------------------------------------

 This unit contains helper classes for loading and resolving of DWARF debug
 symbols

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
unit FpDbgDwarfDataClasses;

{$mode objfpc}{$H+}
{$IFOPT T+}{$ERROR Typedaddress (-Sy) not allowed}{$ENDIF}
{$ModeSwitch advancedrecords}
{$IFDEF INLINE_OFF}{$INLINE OFF}{$ENDIF}
{$IF FPC_Fullversion=30202}{$Optimization NOPEEPHOLE}{$ENDIF}
{off $DEFINE USE_ABBREV_TMAP}

interface

uses
  Classes, Types, SysUtils, Contnrs, Math, fgl,
  // LazUtils
  Maps, LazClasses, LazFileUtils, LazUTF8, LazCollections,
  {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif},
  // FpDebug
  FpDbgUtil, FpDbgInfo, FpDbgDwarfConst, FpDbgCommon, FpDbgDwarfCFI,
  FpDbgLoader, FpImgReaderBase, FpdMemoryTools, FpErrorMessages, DbgIntfBaseTypes;

type
  TDwarfSection = (dsAbbrev, dsARanges, dsFrame,  dsInfo, dsLine, dsLoc, dsMacinfo, dsPubNames, dsPubTypes, dsRanges, dsStr);

const
  DWARF_SECTION_NAME: array[TDwarfSection] of String = (
    '.debug_abbrev', '.debug_aranges', '.debug_frame', '.debug_info',
    '.debug_line', '.debug_loc', '.debug_macinfo', '.debug_pubnames',
    '.debug_pubtypes', '.debug_ranges', '.debug_str'
  );

type
  TFpDwarfInfo = class;
  TDwarfCompilationUnit = class;

{%region Dwarf Header Structures }
  // compilation unit header
  // In version 5 of the Dwarf-specification, the header has been changed.
  {$PACKRECORDS 1}
  PDwarfCUHeader32 = ^TDwarfCUHeader32;
  TDwarfCUHeader32 = record
    Length: LongWord;
    Version: Word;
    AbbrevOffset: LongWord;
    AddressSize: Byte;
  end;

  PDwarfCUHeader32v5 = ^TDwarfCUHeader32v5;
  TDwarfCUHeader32v5 = record
    Length: LongWord;
    Version: Word;
    unit_type: Byte;
    AddressSize: Byte;
    AbbrevOffset: LongWord;
  end;

  PDwarfCUHeader64 = ^TDwarfCUHeader64;
  TDwarfCUHeader64 = record
    Signature: LongWord;
    Length: QWord;
    Version: Word;
    AbbrevOffset: QWord;
    AddressSize: Byte;
  end;

  PDwarfCUHeader64v5 = ^TDwarfCUHeader64v5;
  TDwarfCUHeader64v5 = record
    Signature: LongWord;
    Length: QWord;
    Version: Word;
    unit_type: Byte;
    AddressSize: Byte;
    AbbrevOffset: QWord;
  end;

  // Line number program header
  PDwarfLNPInfoHeader = ^TDwarfLNPInfoHeader;
  TDwarfLNPInfoHeader = record
    MinimumInstructionLength: Byte;
    //MaximumInstructionLength: Byte; // Version 4 and up
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
{%endregion Dwarf Header Structures }

const
  KnownNameHashesBitMask = $7FF; // 256 bytes
type
  TKnownNameHashesArray = bitpacked array [0..KnownNameHashesBitMask] of Boolean;
  PKnownNameHashesArray = ^TKnownNameHashesArray;

{%region Abbreviation Data / Section "debug_abbrev"}
  { TDwarfAbbrev }
  TDwarfAbbrevFlag = (
    dafHasChildren,
    dafHasName,
    dafHasArtifical,
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
  strict private
    FTableList: Array of TLeb128TableEntry;
    FTableListGaps: Array of record LeadTable, EndTable: Byte; end;
    FTableListNextFreeIndex: Cardinal;

    FLeadTableData: Array of Cardinal; //  Next Table number
    FLeadTableNextFreeIndex: Cardinal;
    FEndTableData:  Array of TDwarfAbbrev; //Pointer;
    FEndTableNextFreeIndex: Cardinal;

    FDataGrowStep, FTableListGrowStep: Cardinal;
  strict protected
    function AddLeb128FromPointer(APointer: Pointer; const AData: TDwarfAbbrev): Pointer;
    procedure SetCapacity(ACapacity: integer);
    procedure Finish;
  public
    function FindLe128bFromPointer(APointer: Pointer; out AData: PDwarfAbbrev): Pointer; // returnns pointer to first address after LEB128
    function FindLe128bFromPointer(APointer: Pointer; out AData: TDwarfAbbrev): Pointer; inline; // returnns pointer to first address after LEB128
  end;

  { TDwarfAbbrevList }

  TDwarfAbbrevList = class{$IFnDEF USE_ABBREV_TMAP}(TLEB128PreFixTree){$Endif}
  strict private
    FAbbrDataEnd: Pointer;
    {$IFDEF USE_ABBREV_TMAP}
    FMap: TMap;  // Abbrevs
    {$Endif}
    FDefinitions: array of TDwarfAbbrevEntry;
    FValid: Boolean;
    function GetEntryPointer(AIndex: Integer): PDwarfAbbrevEntry; inline;
    procedure LoadAbbrevs(AnAbbrevDataPtr: Pointer);
  public
    constructor Create(AnAbbrData, AnAbbrDataEnd: Pointer; AnAbbrevOffset, AInfoLen: QWord);
    destructor Destroy; override;
    {$IFDEF USE_ABBREV_TMAP}
    function FindLe128bFromPointer(AnAbbrevPtr: Pointer; out AData: TDwarfAbbrev{Pointer}): Pointer; reintroduce;
    {$Endif}
    property EntryPointer[AIndex: Integer]: PDwarfAbbrevEntry read GetEntryPointer;
    property Valid: Boolean read FValid;
  end;
{%endregion Abbreviation Data / Section "debug_abbrev"}

  TNameSearchInfo = record
    NameUpper, NameLower: String;
    NameHash: Word;
  end;

{%region Information Entry / Section "debug_info"}
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
    NameHash: Word;
  end;
  PDwarfScopeInfoRec = ^TDwarfScopeInfoRec;
  TDwarfScopeArray = Array of TDwarfScopeInfoRec;

  PDwarfScopeList = ^TDwarfScopeList;

  { TDwarfScopeInfo }

  TDwarfScopeInfo = object
  private
    FScopeListPtr: PDwarfScopeList;
    FIndex: Integer;
    function GetChild: TDwarfScopeInfo; inline;
    function GetChildIndex: Integer; inline;
    function GetCurrent: PDwarfScopeInfoRec;
    function GetEntry: Pointer; inline;
    function GetNext: TDwarfScopeInfo; inline;
    function GetNextIndex: Integer; inline;
    function GetParent: TDwarfScopeInfo; inline;
    function GetParentIndex: Integer;
    procedure SetIndex(AIndex: Integer);
  public
    procedure Init(AScopeListPtr: PDwarfScopeList);

    function IsEqual(AnOther: TDwarfScopeInfo): Boolean;

    function IsValid: Boolean; inline;
    property Index: Integer read FIndex write SetIndex;
    property Entry: Pointer read GetEntry;
    property Current: PDwarfScopeInfoRec read GetCurrent;

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
    property ScopeListPtr: PDwarfScopeList read FScopeListPtr;
  end;

  { TDwarfScopeList }

  TDwarfScopeList = record
  strict private
    FList: TDwarfScopeArray;
    FHighestKnown: Integer;
    function CreateScopeForEntry(AEntry: Pointer; ALink: Integer): Integer; inline;
    function CreateNextForEntry(AScope: TDwarfScopeInfo; AEntry: Pointer): Integer; inline;
    function CreateChildForEntry(AScope: TDwarfScopeInfo; AEntry: Pointer): Integer; inline;
  private
    (* BuildList:
       Once the complete list is scanned, all access is readonly.
       TDwarfScopeList can be accessed from multiple threads  (once build)
    *)
    function BuildList(AnAbbrevList: TDwarfAbbrevList; AnInfoData: Pointer;
      ALength: QWord; AnAddressSize: Byte; AnIsDwarf64: Boolean; AVersion: Word;
      AnUntilTagFound: Cardinal = 0): TDwarfScopeInfo;
  public
    property HighestKnown: Integer read FHighestKnown;
    property List: TDwarfScopeArray read FList; // NameHash will be updated by a thread
  end;

  { TDwarfInformationEntry }
  TDwarfInformationEntry = class;

  TDwarfAttribData = record
    Idx: Integer;
    InfoPointer: pointer;
    InformationEntry: TDwarfInformationEntry;
  end;

  TDwarfInformationEntry = class(TRefCountedObject)
  private
    FCompUnit: TDwarfCompilationUnit;
    FInformationEntry: Pointer; // pointer to the LEB128 Abbrev at the start of an Information entry in debug_info
    FInformationData: Pointer;  // poinetr after the LEB128
    FScope: TDwarfScopeInfo;
    FScopeCurrentInfoPtr: PDwarfScopeInfoRec; // only valid after GoNext/Child[Fast]
    FAbbrev: PDwarfAbbrev;
    FAbbrevData: PDwarfAbbrevEntry;
    FAbstractOrigin: TDwarfInformationEntry;
    FFlags: set of (dieAbbrevValid, dieAbbrevDataValid, dieAbstractOriginValid);

    function GetAttribForm(AnIdx: Integer): Cardinal;
    procedure PrepareAbbrev; inline;
    function  PrepareAbbrevData: Boolean; inline;
    function  PrepareAbstractOrigin: Boolean; inline; // Onli call, if abbrev is valid AND dafHasAbstractOrigin set

    function SearchScope: Boolean;
    function MaybeSearchScope: Boolean; inline;
    procedure ScopeChanged; inline;

    function GetAbbrevTag: Cardinal; inline;
    function GetScopeIndex: Integer;
    procedure SetScopeIndex(AValue: Integer);

    function DoReadReference(InfoIdx: Integer; InfoData: pointer;
      out AValue: Pointer; out ACompUnit: TDwarfCompilationUnit): Boolean;
  public
    constructor Create(ACompUnit: TDwarfCompilationUnit; AnInformationEntry: Pointer);
    constructor Create(ACompUnit: TDwarfCompilationUnit; AScope: TDwarfScopeInfo);
    destructor Destroy; override;
    property CompUnit: TDwarfCompilationUnit read FCompUnit;

    function GetAttribData(AnAttrib: Cardinal; out AnAttribData: TDwarfAttribData): Boolean;
    function HasAttrib(AnAttrib: Cardinal): Boolean; inline;
    property AttribForm[AnIdx: Integer]: Cardinal read GetAttribForm;

    procedure ComputeKnownHashes(AKNownHashes: PKnownNameHashesArray);

    function GoNamedChild(const ANameInfo: TNameSearchInfo): Boolean;
    // find in enum too // TODO: control search with a flags param, if needed
    function GoNamedChildEx(const ANameInfo: TNameSearchInfo; ASkipArtificial: Boolean = False; ASkipEnumMembers: Boolean = False): Boolean;
    // GoNamedChildMatchCaseEx will use
    // - UpperName for Hash
    // - LowerName for compare
    // GoNamedChildMatchCaseEx does not search in enums
    function GoNamedChildMatchCaseEx(const ANameInfo: TNameSearchInfo): Boolean;

    function FindNamedChild(const AName: String): TDwarfInformationEntry;
    function FindChildByTag(ATag: Cardinal): TDwarfInformationEntry;
    function FirstChild: TDwarfInformationEntry;
    function Clone: TDwarfInformationEntry;

    property AbbrevTag: Cardinal read GetAbbrevTag;
    property InfoScope: TDwarfScopeInfo read FScope;

    function ReadValue(const AnAttribData: TDwarfAttribData; out AValue: Integer): Boolean; inline;
    function ReadValue(const AnAttribData: TDwarfAttribData; out AValue: Int64): Boolean; inline;
    function ReadValue(const AnAttribData: TDwarfAttribData; out AValue: Cardinal): Boolean; inline;
    function ReadValue(const AnAttribData: TDwarfAttribData; out AValue: QWord): Boolean; inline;
    function ReadValue(const AnAttribData: TDwarfAttribData; out AValue: PChar): Boolean; inline;
    function ReadValue(const AnAttribData: TDwarfAttribData; out AValue: String): Boolean; inline;
    function ReadValue(const AnAttribData: TDwarfAttribData; out AValue: TByteDynArray; AnFormString: Boolean = False): Boolean; inline;
    function ReadAddressValue(const AnAttribData: TDwarfAttribData; out AValue: TDBGPtr): Boolean; inline;
    function ReadReference(const AnAttribData: TDwarfAttribData; out AValue: Pointer; out ACompUnit: TDwarfCompilationUnit): Boolean; inline;

    function ReadValue(AnAttrib: Cardinal; out AValue: Integer): Boolean; inline;
    function ReadValue(AnAttrib: Cardinal; out AValue: Int64): Boolean; inline;
    function ReadValue(AnAttrib: Cardinal; out AValue: Cardinal): Boolean; inline;
    function ReadValue(AnAttrib: Cardinal; out AValue: QWord): Boolean; inline;
    function ReadValue(AnAttrib: Cardinal; out AValue: PChar): Boolean; inline;
    function ReadValue(AnAttrib: Cardinal; out AValue: String): Boolean; inline;
    function ReadValue(AnAttrib: Cardinal; out AValue: TByteDynArray; AnFormString: Boolean = False): Boolean; inline;
    function ReadReference(AnAttrib: Cardinal; out AValue: Pointer; out ACompUnit: TDwarfCompilationUnit): Boolean; inline;

    function  ReadName(out AName: String): Boolean; inline;
    function  ReadName(out AName: PChar): Boolean; inline;
    function  ReadStartScope(out AStartScope: TDbgPtr): Boolean; inline;
    function  IsAddressInStartScope(AnAddress: TDbgPtr): Boolean; inline;
    function  IsArtificial: Boolean; inline;
    function  IsEqual(AnOther: TDwarfInformationEntry): Boolean; inline;
  public
    // Scope
    procedure GoParent; inline;
    procedure GoNext; inline;
    procedure GoChild; inline;
    procedure GoNextFast; inline;  // Only if we know we have a valid scope
    procedure GoChildFast; inline; // Only if we know we have a valid scope
    function HasValidScope: Boolean; inline;
    property ScopeIndex: Integer read GetScopeIndex write SetScopeIndex;

    function ScopeDebugText: String;
  end;
{%endregion Information Entry / Section "debug_info"}

{%region Line Info / Section "debug_line"}
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

  PDWarfLineMap = ^TDWarfLineMap;

  { TDWarfLineMap }

  TDWarfLineMap = object
  private
    // FLineIndexList[ line div 256 ]
    FLineIndexList: Array of record
      LineOffsets: Array of Byte;
      Addresses: Array of TDBGPtr;
    end;
    FNextMap: PDWarfLineMap; // https:/bugs.freepascal.org/view.php?id=37658
  public
    procedure Init;
    procedure SetAddressForLine(ALine: Cardinal; AnAddress: TDBGPtr); inline;
    function  GetAddressesForLine(ALine: Cardinal; var AResultList: TDBGPtrArray;
      NoData: Boolean = False;
      AFindSibling: TGetLineAddrFindSibling = fsNone;
      AFoundLine: PInteger = nil;
      AMaxSiblingDistance: integer = 0;
      ADbgInfo: TFpDwarfInfo = nil
    ): Boolean; inline;
      // NoData: only return True/False, but nothing in AResultList
    procedure Compress;
  end;
{%endregion Line Info / Section "debug_line"}

{%region Base classes for handling Symbols in unit FPDbgDwarf}
  { TDbgDwarfSymbolBase }

  TDbgDwarfSymbolBase = class(TFpSymbolForwarder)
  private
    FCU: TDwarfCompilationUnit;
    FInformationEntry: TDwarfInformationEntry;
  protected
    procedure Init; virtual;
  public
    constructor Create(const AName: String); overload;
    constructor Create(const AName: String; AnInformationEntry: TDwarfInformationEntry); overload;
    constructor Create(const AName: String; AnInformationEntry: TDwarfInformationEntry;
                       AKind: TDbgSymbolKind; const AAddress: TFpDbgMemLocation); overload;
    destructor Destroy; override;

    function CreateSymbolScope(ALocationContext: TFpDbgSimpleLocationContext; ADwarfInfo: TFpDwarfInfo): TFpDbgSymbolScope; virtual; overload;

    function IsEqual(AnOther: TFpSymbol): Boolean; override;

    property CompilationUnit: TDwarfCompilationUnit read FCU;
    property InformationEntry: TDwarfInformationEntry read FInformationEntry;
  end;
  TDbgDwarfSymbolBaseClass = class of TDbgDwarfSymbolBase;

  { TFpSymbolDwarfDataLineInfo }
  // Not strictly a Symbol...

  TFpSymbolDwarfDataLineInfo = class(TDbgDwarfSymbolBase)
  private
    FLine, FNextLine: Cardinal;
    FLineStartAddress, FLineEndAddress: TDBGPtr;
    FFile: String;
    FFound: Boolean;
  protected
    function GetFlags: TDbgSymbolFlags; override;
    //function GetColumn: Cardinal; override;
    function GetFile: String; override;
    function GetLine: Cardinal; override;
    function GetLineStartAddress: TDBGPtr; override;
    function GetLineEndAddress: TDBGPtr; override;
  public
    constructor Create(AnAddress: TDbgPtr; AStateMachine: TDwarfLineInfoStateMachine; ACU: TDwarfCompilationUnit);
  end;

  { TFpSymbolDwarfClassMap
    Provides Symbol and VAlue evaluation classes depending on the compiler
  }

  PFpDwarfSymbolClassMap = ^TFpSymbolDwarfClassMap;

  TFpSymbolDwarfClassMap = class
  private
    NextExistingClassMap: TFpSymbolDwarfClassMap;
  protected
    function CanHandleCompUnit(ACU: TDwarfCompilationUnit; AHelperData: Pointer): Boolean; virtual;
    class function GetExistingClassMap: PFpDwarfSymbolClassMap; virtual; abstract; // Each class must have its own storage
    class function DoGetInstanceForCompUnit(ACU: TDwarfCompilationUnit; AHelperData: Pointer): TFpSymbolDwarfClassMap;
  public
    class function GetInstanceForCompUnit(ACU: TDwarfCompilationUnit): TFpSymbolDwarfClassMap; virtual;
    class procedure FreeAllInstances;
    class function ClassCanHandleCompUnit(ACU: TDwarfCompilationUnit): Boolean; virtual; abstract;
  public
    constructor Create(ACU: TDwarfCompilationUnit; AHelperData: Pointer); virtual;
    function IgnoreCfiStackEnd: boolean; virtual;
    function GetDwarfSymbolClass(ATag: Cardinal): TDbgDwarfSymbolBaseClass; virtual; abstract;
    function CreateScopeForSymbol(ALocationContext: TFpDbgSimpleLocationContext; ASymbol: TFpSymbol;
                                 ADwarf: TFpDwarfInfo): TFpDbgSymbolScope; virtual; abstract;
    function CreateProcSymbol(ACompilationUnit: TDwarfCompilationUnit;
                                    AInfo: PDwarfAddressInfo; AAddress: TDbgPtr; ADbgInfo: TFpDwarfInfo): TDbgDwarfSymbolBase; virtual; abstract;
    function CreateUnitSymbol(ACompilationUnit: TDwarfCompilationUnit;
                                    AInfoEntry: TDwarfInformationEntry; ADbgInfo: TFpDwarfInfo): TDbgDwarfSymbolBase; virtual; abstract;
  end;
  TFpSymbolDwarfClassMapClass = class of TFpSymbolDwarfClassMap;

  { TFpSymbolDwarfClassMapList }

  TFpSymbolDwarfClassMapList = class
  private
    FDefaultMap: TFpSymbolDwarfClassMapClass;
    FMapList: array of TFpSymbolDwarfClassMapClass;
  public
    destructor Destroy; override;
    function FindMapForCompUnit(ACU: TDwarfCompilationUnit): TFpSymbolDwarfClassMap;
    procedure FreeAllInstances;
    procedure AddMap(AMap: TFpSymbolDwarfClassMapClass);
    procedure SetDefaultMap(AMap: TFpSymbolDwarfClassMapClass);
  end;
{%endregion Base classes for handling Symbols in unit FPDbgDwarf}

  TDwarfSectionInfo = record
    Section: TDwarfSection;
    VirtualAddress: QWord;
    Size: QWord; // the virtual size
    RawData: Pointer;
  end;
  PDwarfSectionInfo = ^TDwarfSectionInfo;

  TDwarfDebugFile = record
    Sections: array[TDwarfSection] of TDwarfSectionInfo;
    AddressMapList: TDbgAddressMapList;
  end;
  PDwarfDebugFile = ^TDwarfDebugFile;

  { TFpThreadWorkerComputeNameHashes }

  TFpThreadWorkerComputeNameHashes = class(TFpThreadWorkerItem)
  strict private
    FCU: TDwarfCompilationUnit;
    FScanScopeList: TDwarfScopeList;
    FReadyToRun: Cardinal;
  protected
    procedure DoExecute; override;
  public
    constructor Create(CU: TDwarfCompilationUnit);
    procedure SetScopeList(const AScanScopeList: TDwarfScopeList); // will queue to run on the 2nd call
    procedure MarkReadyToRun; // will queue to run on the 2nd call
  end;

  { TFpThreadWorkerScanAll }

  TFpThreadWorkerScanAll = class(TFpThreadWorkerItem)
  strict private
    FCU: TDwarfCompilationUnit;
    FScanScopeList: TDwarfScopeList;
    FCompNameHashWorker: TFpThreadWorkerComputeNameHashes;
  protected
    procedure DoExecute; override;
  public
    constructor Create(CU: TDwarfCompilationUnit; ACompNameHashWorker: TFpThreadWorkerComputeNameHashes);
    function  FindCompileUnit(var AScopeList: TDwarfScopeList): TDwarfScopeInfo; // To be called ONLY before the thread starts
    procedure UpdateScopeList(var AScopeList: TDwarfScopeList); // To be called ONLY before the thread starts
  end;

  { TDwarfCompilationUnit }

  TDwarfCompilationUnitClass = class of TDwarfCompilationUnit;
  TDwarfCompilationUnit = class
  strict private
    FWaitForScopeScanSection: TWaitableSection;
    FWaitForComputeHashesSection: TWaitableSection;
    FBuildAddressMapSection: TWaitableSection;
  private type
    TWaitRequirement = (wrScan, wrAddrMap);
    TWaitRequirements = set of TWaitRequirement;
  private
    FOwner: TFpDwarfInfo;
    FDebugFile: PDwarfDebugFile;
    FDwarfSymbolClassMap: TFpSymbolDwarfClassMap;
    FValid: Boolean; // set if the compilationunit has compile unit tag.
  
    // --- Header ---
    FLength: QWord;  // length of info
    FVersion: Word;
    FHeaderSize: Integer;
    FAbbrevOffset: QWord;
    FAddressSize: Byte;  // the address size of the target in bytes
    FIsDwarf64: Boolean; // Set if the dwarf info in this unit is 64bit
    // ------
    
    FInfoData: Pointer;
    FFileName: String;
    FCompDir:  String;
    FUnitName: String;
    FIdentifierCase: Integer;
    FLanguageId: Integer;
    FProducer: String;

    FAbbrevList: TDwarfAbbrevList;

    {$IFDEF DwarfTestAccess} public {$ENDIF}
    FLineInfo: record
      Header: Pointer;
      DataStart: Pointer;
      DataEnd: Pointer;

      Valid: Boolean;
      Version: Word;
      Addr64: Boolean;
      AddrSize: Byte;
      MinimumInstructionLength: Byte;
      MaximumInstructionLength: Byte; // Dwarf 4
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
    {$IFDEF DwarfTestAccess} private {$ENDIF}
    FInitialStateMachine: TDwarfLineInfoStateMachine;

    FAddressMap: TMap; // Holds a key for each DW_TAG_subprogram / TFpSymbolDwarfDataProc, stores TDwarfAddressInfo
    FAddressMapBuild: Boolean;
    
    FMinPC: TDBGPtr;  // the min and max PC value found in this unit.
    FMaxPC: TDBGPtr;  //
    FFirstScope: TDwarfScopeInfo;
    FScopeList: TDwarfScopeList;
    FCompUnitScope: TDwarfScopeInfo;
    FKnownNameHashes: TKnownNameHashesArray;

    FScanAllWorker: TFpThreadWorkerScanAll;
    FComputeNameHashesWorker: TFpThreadWorkerComputeNameHashes;

    function GetFirstScope: TDwarfScopeInfo; inline;
    procedure DoWaitForScopeScan;
    procedure DoWaitForComputeHashes;
    procedure BuildAddressMap;
    function GetAddressMap: TMap;
    function GetKnownNameHashes: PKnownNameHashesArray; inline;
    function GetUnitName: String;
    function  ReadTargetAddressFromDwarfSection(var AData: Pointer; AIncPointer: Boolean = False): TFpDbgMemLocation;
    function  ReadDwarfSectionOffsetOrLenFromDwarfSection(var AData: Pointer; AIncPointer: Boolean = False): TFpDbgMemLocation;
  protected
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
    function ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: PChar): Boolean; // Same as: out AValue: String
    function ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: TByteDynArray; AnFormString: Boolean = False): Boolean;
    // Read a value that contains an address. The address is evaluated using MapAddressToNewValue
    function ReadAddressValue(AAttribute: Pointer; AForm: Cardinal; out AValue: QWord): Boolean;
  public
    constructor Create(AOwner: TFpDwarfInfo; ADebugFile: PDwarfDebugFile; ADataOffset: QWord; ALength: QWord; AVersion: Word; AAbbrevOffset: QWord; AAddressSize: Byte; AIsDwarf64: Boolean); virtual;
    destructor Destroy; override;
    procedure WaitForScopeScan; inline; // MUST be called, before accessing the CU
    procedure WaitForComputeHashes; inline;
    function GetDefinition(AAbbrevPtr: Pointer; out ADefinition: TDwarfAbbrev): Boolean; inline;
    procedure BuildLineInfo(AAddressInfo: PDwarfAddressInfo; ADoAll: Boolean);
    // On Darwin it could be that the debug-information is not included into the executable by the linker.
    // This function is to map object-file addresses into the corresponding addresses in the executable.
    function MapAddressToNewValue(AValue: QWord): QWord;
    // Calculates an address taking the relocation after being loaded (dynamically)
    // into memory into account, based on the library's ImageBase.
    // Conceptually it would be better to let the TDbgLibrary take the relocation
    // into account. But it was chosen to do the relocation during the loading of
    // the Dwarf-information due to performance reasons. And because of consistency
    // with the symbol-information. In which the relocation also takes place while
    // the debug-info is loaded.
    function CalculateRelocatedAddress(AValue: QWord): QWord; inline;
    // Get start/end addresses of proc
    function GetProcStartEnd(const AAddress: TDBGPtr; out AStartPC, AEndPC: TDBGPtr): boolean;

    function HasAddress(AAddress: TDbgPtr; AWaitFor: TWaitRequirements = []): Boolean; inline;
    property Valid: Boolean read FValid;
    property FileName: String read FFileName;
    property UnitName: String read GetUnitName;
    property IdentifierCase: Integer read FIdentifierCase;
    property LanguageId: Integer read FLanguageId;
    property Producer: String read FProducer;
    property BaseAddress: TDBGPtr read FMinPC;

    property Version: Word read FVersion;
    //property AbbrevOffset: QWord read FAbbrevOffset;
    property AddressSize: Byte read FAddressSize;  // the address size of the target in bytes
    (* IsDwarf64, From the spec:
     In the 64-bit DWARF format, all values that
 *** "represent lengths of DWARF sections and offsets relative to the beginning of DWARF sections" ***
     are represented using 64-bits.

     A special convention applies to the initial length field of certain DWARF sections, as well as the CIE and FDE structures,
     so that the 32-bit and 64-bit DWARF formats can coexist and be distinguished within a single linked object.
    *)
    property IsDwarf64: Boolean read FIsDwarf64; // Set if the dwarf info in this unit is 64bit
    property Owner: TFpDwarfInfo read FOwner;
    property DebugFile: PDwarfDebugFile read FDebugFile;

    property DwarfSymbolClassMap: TFpSymbolDwarfClassMap read FDwarfSymbolClassMap;
    property FirstScope: TDwarfScopeInfo read GetFirstScope;

    // public for FpDbgDwarfVerbosePrinter
    property HeaderSize: Integer read FHeaderSize;
    property InfoData: Pointer read FInfoData;
    property InfoDataLength: QWord read FLength;  // length of info
    property AddressMap: TMap read GetAddressMap;
    property AbbrevList: TDwarfAbbrevList read FAbbrevList;
    property KnownNameHashes: PKnownNameHashesArray read GetKnownNameHashes; // Only for TOP-LEVEL entries
  end;

  TLineNumberFileMap = specialize TFPGMap<AnsiString, PDWarfLineMap>;

  { TFpDwarfInfo }

  TFpDwarfInfo = class(TDbgInfo)
  strict private
    FCompilationUnits: TList; // any access must be guarded by Item[n].WaitForScopeScan
    FCallFrameInformationList: TList;
    FWorkQueue: TFpGlobalThreadWorkerQueue;
    FFiles: array of TDwarfDebugFile;
  private
    FLineNumberMap: TLineNumberFileMap;
    FLineNumberMapDone: Boolean;
    FImageBase: QWord;
    FRelocationOffset: QWord;
    function GetCompilationUnit(AIndex: Integer): TDwarfCompilationUnit; inline;
  protected
    function GetCompilationUnitClass: TDwarfCompilationUnitClass; virtual;
    function FindCompilationUnitByOffs(AOffs: QWord): TDwarfCompilationUnit;
    function FindDwarfUnitSymbol(AAddress: TDbgPtr): TDbgDwarfSymbolBase; inline;
  public
    constructor Create(ALoaderList: TDbgImageLoaderList; AMemManager: TFpDbgMemManager; AMemModel: TFpDbgMemModel); override;
    destructor Destroy; override;
    function FindSymbolScope(ALocationContext: TFpDbgSimpleLocationContext; AAddress: TDbgPtr = 0): TFpDbgSymbolScope; override;
    function FindDwarfProcSymbol(AAddress: TDbgPtr): TDbgDwarfSymbolBase; inline;
    function FindProcSymbol(AAddress: TDbgPtr): TFpSymbol; override; overload;
    function FindProcStartEndPC(const AAddress: TDbgPtr; out AStartPC, AEndPC: TDBGPtr): boolean; override;
    function FindLineInfo(AAddress: TDbgPtr): TFpSymbol; override;
    function FindCallFrameInfo(AnAddress: TDBGPtr; out CIE: TDwarfCIE; out Row: TDwarfCallFrameInformationRow): Boolean; virtual;

    //function FindSymbol(const AName: String): TDbgSymbol; override; overload;
    function GetLineAddresses(const AFileName: String; ALine: Cardinal; var AResultList: TDBGPtrArray;
      AFindSibling: TGetLineAddrFindSibling = fsNone; AFoundLine: PInteger = nil; AFoundFilename: PBoolean = nil;
      AMaxSiblingDistance: integer = 0): Boolean; override;
    function GetLineAddressMap(const AFileName: String): PDWarfLineMap;
    procedure LoadCallFrameInstructions;
    function LoadCompilationUnits: Integer;
    function CompilationUnitForAddr(AnAddr: TDBGPtr): TDwarfCompilationUnit;
    function CompilationUnitsCount: Integer; inline;
    property CompilationUnits[AIndex: Integer]: TDwarfCompilationUnit read GetCompilationUnit;

    property ImageBase: QWord read FImageBase;
    property RelocationOffset: QWord read FRelocationOffset;
    property WorkQueue: TFpGlobalThreadWorkerQueue read FWorkQueue;
  end;

  TDwarfLocationExpression = class;

  { TDwarfLocationStack }

  TDwarfLocationStack = object
  private
    FList: array of TFpDbgMemLocation; //TDwarfLocationStackEntry;
    FCount: Integer;
    FError: TFpErrorCode;
    procedure IncCapacity;
  public
    procedure Clear;
    function  Count: Integer; inline;
    function  Pop: TFpDbgMemLocation;
    function  PopForDeref: TFpDbgMemLocation;
    procedure Push(const AEntry: TFpDbgMemLocation);
    procedure PushCopy(AFromIndex: Integer);
    procedure PushConst(const AVal: TDBGPtr);
    procedure PushSignedConst(const AVal: Int64); inline;
    procedure PushTargetMem(const AVal: TDBGPtr);
    procedure PushTargetRegister(const ARegNum: Cardinal);
    function  Peek: PFpDbgMemLocation;
    function  PeekForDeref: PFpDbgMemLocation;
    function  PeekKind: TFpDbgMemLocationType; // Can be called on empty stack
    function  Peek(AIndex: Integer): PFpDbgMemLocation;
    procedure Modify(AIndex: Integer; const AEntry: TFpDbgMemLocation);
    procedure Copy(AFromIndex, AIndex: Integer);
  end;

  { TDwarfLocationExpression }

  TDwarfLocationExpression = class
  private
    FContext: TFpDbgLocationContext;
    FCurrentObjectAddress: TFpDbgMemLocation;
    FFrameBase: TDbgPtr;
    FIsDwAtFrameBase: Boolean;
    FLastError: TFpError;
    FStack: TDwarfLocationStack;
    FCU: TDwarfCompilationUnit;
    FData: PByte;
    FMaxData: PByte;
  public
  //TODO: caller keeps data, and determines livetime of data
    constructor Create(AExpressionData: Pointer; AMaxCount: Integer; ACU: TDwarfCompilationUnit;
      AContext: TFpDbgLocationContext);
    procedure SetLastError(ALastError: TFpError);
    procedure Evaluate;
    function ResultData: TFpDbgMemLocation;
    procedure Push(const AValue: TFpDbgMemLocation);
    //property  FrameBase: TDbgPtr read FFrameBase write FFrameBase;
    property LastError: TFpError read FLastError;
    property Context: TFpDbgLocationContext read FContext write FContext;
    // for DW_OP_push_object_address
    property CurrentObjectAddress: TFpDbgMemLocation read FCurrentObjectAddress write FCurrentObjectAddress;
    property IsDwAtFrameBase: Boolean read FIsDwAtFrameBase write FIsDwAtFrameBase;
    end;

function Dbgs(AInfoData: Pointer; ACompUnit: TDwarfCompilationUnit): String; overload;
function Dbgs(AScope: TDwarfScopeInfo; ACompUnit: TDwarfCompilationUnit): String; overload;
function Dbgs(AInfoEntry: TDwarfInformationEntry; ACompUnit: TDwarfCompilationUnit): String; overload;
function DbgsDump(AScope: TDwarfScopeInfo; ACompUnit: TDwarfCompilationUnit): String; overload;

function GetDwarfSymbolClassMapList: TFpSymbolDwarfClassMapList; inline;
function NameInfoForSearch(const AName: String): TNameSearchInfo;

property DwarfSymbolClassMapList: TFpSymbolDwarfClassMapList read GetDwarfSymbolClassMapList;

implementation

var
  FPDBG_DWARF_ERRORS, FPDBG_DWARF_WARNINGS, FPDBG_DWARF_SEARCH, FPDBG_DWARF_VERBOSE,
  // FPDBG_DWARF_DATA_WARNINGS,
  FPDBG_DWARF_VERBOSE_LOAD: PLazLoggerLogGroup;

var
  TheDwarfSymbolClassMapList: TFpSymbolDwarfClassMapList = nil;
  CachedRtlEvent: PRTLEvent = nil;

const
  SCOPE_ALLOC_BLOCK_SIZE = 4096; // Increase scopelist in steps of

function GetDwarfSymbolClassMapList: TFpSymbolDwarfClassMapList;
begin
  if not Assigned(TheDwarfSymbolClassMapList) then
    TheDwarfSymbolClassMapList := TFpSymbolDwarfClassMapList.Create;
  Result := TheDwarfSymbolClassMapList;
end;

function NameInfoForSearch(const AName: String): TNameSearchInfo;
begin
  Result.NameLower := UTF8LowerCaseFast(AName);
  Result.NameUpper := UTF8UpperCaseFast(AName);
  Result.NameHash := objpas.Hash(Result.NameUpper) and $7fff or $8000;
end;

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

function SkipEntryDataForForm(var AEntryData: Pointer; AForm: Cardinal; AddrSize: Byte; IsDwarf64: boolean; Version: word): Boolean; inline;
var
  UValue: QWord;
begin
  Result := True;
  case AForm of
    DW_FORM_addr     : Inc(AEntryData, AddrSize);
    DW_FORM_block,
    DW_FORM_exprloc  : begin
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
    DW_FORM_ref_sig8 : Inc(AEntryData, 8);
    DW_FORM_strp,
    DW_FORM_sec_offset: begin
        if IsDwarf64 then
          Inc(AEntryData, 8)
        else
          Inc(AEntryData, 4);
      end;
    DW_FORM_ref_addr : begin
        // In Dwarf-version 3 and higher, the size of a DW_FORM_ref_addr depends
        // on the Dwarf-format. In prior Dwarf-versions it is equal to the
        // Addres-size.
        if Version>2 then begin
          if IsDwarf64 then
            Inc(AEntryData, 8)
          else
            Inc(AEntryData, 4);
        end else begin
          Inc(AEntryData, AddrSize);
        end;
      end;
    DW_FORM_string   : begin
        while PByte(AEntryData)^ <> 0 do Inc(AEntryData);
        Inc(AEntryData);
      end;
    DW_FORM_indirect : begin
        while AForm = DW_FORM_indirect do AForm := ULEB128toOrdinal(AEntryData);
        Result := SkipEntryDataForForm(AEntryData, AForm, AddrSize, IsDwarf64, Version);
      end;
    DW_FORM_flag_present: ; // No data
  else begin
      DebugLn(FPDBG_DWARF_WARNINGS, ['Error: Unknown Form: ', AForm]);
      Result := False;
    end;
  end;

end;

{ TFpSymbolDwarfClassMap }

class function TFpSymbolDwarfClassMap.GetInstanceForCompUnit(
  ACU: TDwarfCompilationUnit): TFpSymbolDwarfClassMap;
begin
  Result := DoGetInstanceForCompUnit(ACU, nil);
end;

class procedure TFpSymbolDwarfClassMap.FreeAllInstances;
var
  pm, next: TFpSymbolDwarfClassMap;
begin
  pm := GetExistingClassMap^;
  while pm <> nil do begin
    next := pm.NextExistingClassMap;
    pm.Destroy;
    pm := next;
  end;
  GetExistingClassMap^ := nil;
end;

function TFpSymbolDwarfClassMap.IgnoreCfiStackEnd: boolean;
begin
  Result := False;
end;

constructor TFpSymbolDwarfClassMap.Create(ACU: TDwarfCompilationUnit;
  AHelperData: Pointer);
begin
  inherited Create;
end;

function TFpSymbolDwarfClassMap.CanHandleCompUnit(ACU: TDwarfCompilationUnit;
  AHelperData: Pointer): Boolean;
begin
  Result := True;
end;

class function TFpSymbolDwarfClassMap.DoGetInstanceForCompUnit(
  ACU: TDwarfCompilationUnit; AHelperData: Pointer): TFpSymbolDwarfClassMap;
var
  pm: PFpDwarfSymbolClassMap;
begin
  pm := GetExistingClassMap;
  while pm^ <> nil do begin
    if pm^.CanHandleCompUnit(ACU, AHelperData) then
      exit(pm^);
    pm := @pm^.NextExistingClassMap;
  end;
  Result := Self.Create(ACU, AHelperData);
  pm^ := Result;
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
      assert(Int64(AEntry^.EndIndex) + Int64(AEntry^.LeadHigh) - Int64(AEntry^.LeadLow) + 1 + GapAvail <= FEndTableNextFreeIndex);
      AtEnd := Int64(AEntry^.EndIndex) + Int64(AEntry^.EndHigh) - Int64(AEntry^.EndLow) + 1 + GapAvail = FEndTableNextFreeIndex;
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
      exit(nil);
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
  FValid := False;
  abbrev := 0;
  CurAbbrevIndex := 0;
  DbgVerbose := (FPDBG_DWARF_VERBOSE_LOAD <> nil) and (FPDBG_DWARF_VERBOSE_LOAD^.Enabled);

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
      DebugLn(FPDBG_DWARF_VERBOSE_LOAD, ['  abbrev:  ', abbrev]);
      DebugLn(FPDBG_DWARF_VERBOSE_LOAD, ['  tag:     ', Def.tag, '=', DwarfTagToString(Def.tag)]);
      DebugLn(FPDBG_DWARF_VERBOSE_LOAD, ['  children:', pbyte(AnAbbrevDataPtr)^, '=', DwarfChildrenToString(pbyte(AnAbbrevDataPtr)^)]);
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
      if attrib = DW_AT_artificial then
        Include(f, dafHasArtifical)
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
      if form > DW_FORM_MAX then begin
        DebugLn(FPDBG_DWARF_WARNINGS, ['Unknown FW_FORM: ', form, ' found. Aborting']);
        exit;
      end;

      MakeRoom(CurAbbrevIndex + 1);
      FDefinitions[CurAbbrevIndex].Attribute := attrib;
      FDefinitions[CurAbbrevIndex].Form := form;
      Inc(CurAbbrevIndex);

      if DbgVerbose
      then DebugLn(FPDBG_DWARF_VERBOSE_LOAD, ['   [', n, '] attrib: ', attrib, '=', DwarfAttributeToString(attrib), ', form: ', form, '=', DwarfAttributeFormToString(form)]);
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
  FValid := True;
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

procedure TDwarfScopeInfo.Init(AScopeListPtr: PDwarfScopeList);
begin
  FIndex := -1;
  FScopeListPtr := AScopeListPtr;
end;

function TDwarfScopeInfo.IsEqual(AnOther: TDwarfScopeInfo): Boolean;
begin
  Result := IsValid and AnOther.IsValid and (FIndex = AnOther.FIndex);
end;

function TDwarfScopeInfo.IsValid: Boolean;
begin
  Result := FIndex >= 0;
end;

function TDwarfScopeInfo.GetNextIndex: Integer;
var
  l: Integer;
  p: PDwarfScopeInfoRec;
begin
  Result := -1;
  if (not IsValid) or (FScopeListPtr^.HighestKnown = FIndex) then exit;
  // Use pointer, to avoid calculating the index twice
  p := @FScopeListPtr^.List[FIndex + 1];
  Result := p^.Link;
  assert(Result <= FScopeListPtr^.HighestKnown);
  if (Result > FIndex + 1) then       // Index+1 is First Child, with pointer to Next
    exit;

  l := (p-1)^.Link; // GetParent  (or -1 for toplevel)
  assert(l <= FScopeListPtr^.HighestKnown);
  if l > Index then l := Index - 1;   // This is a first child, make l = parent
  if (Result = l) then begin          // Index + 1 has same parent
    Result := Index + 1;
    exit;
  end;

  Result := -1;
end;

function TDwarfScopeInfo.GetNext: TDwarfScopeInfo;
begin
  Result.Init(FScopeListPtr);
  if IsValid then
    Result.Index := GetNextIndex;
end;

function TDwarfScopeInfo.GetEntry: Pointer;
begin
  Result := nil;
  if IsValid then
    Result := FScopeListPtr^.List[FIndex].Entry;
end;

function TDwarfScopeInfo.HasChild: Boolean;
var
  l2: Integer;
begin
  Result := (IsValid) and (FScopeListPtr^.HighestKnown > FIndex);
  if not Result then exit;
  l2 := FScopeListPtr^.List[FIndex + 1].Link;
  assert(l2 <= FScopeListPtr^.HighestKnown);
  Result := (l2 > FIndex + 1) or        // Index+1 is First Child, with pointer to Next
            (l2 = FIndex);              // Index+1 is First Child, with pointer to parent (self)
end;

function TDwarfScopeInfo.GetChild: TDwarfScopeInfo;
begin
  Result.Init(FScopeListPtr);
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

function TDwarfScopeInfo.GetCurrent: PDwarfScopeInfoRec;
begin
  Result := nil;
  if IsValid then
    Result := @FScopeListPtr^.List[FIndex];
end;

function TDwarfScopeInfo.GetParent: TDwarfScopeInfo;
var
  l: Integer;
begin
  Result.Init(FScopeListPtr);
  if not IsValid then exit;
  l := FScopeListPtr^.List[FIndex].Link; // GetParent  (or -1 for toplevel)
  assert(l <= FScopeListPtr^.HighestKnown);
  if l > Index then
    l := Index - 1;   // This is a first child, make l = parent
  Result.Index := l;
end;

function TDwarfScopeInfo.GetParentIndex: Integer;
begin
  Result := -1;
  if not IsValid then exit;
  Result := FScopeListPtr^.List[FIndex].Link; // GetParent  (or -1 for toplevel)
  assert(Result <= FScopeListPtr^.HighestKnown);
  if Result > Index then
    Result := Index - 1;   // This is a first child, make l = parent
end;

procedure TDwarfScopeInfo.SetIndex(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex <= FScopeListPtr^.HighestKnown) then
    FIndex := AIndex
  else
    FIndex := -1;
end;

function TDwarfScopeInfo.HasParent: Boolean;
var
  l: Integer;
begin
  Result := (IsValid);
  if not Result then exit;
  l := FScopeListPtr^.List[FIndex].Link;
  assert(l <= FScopeListPtr^.HighestKnown);
  Result := (l >= 0);
end;

function TDwarfScopeInfo.HasNext: Boolean;
var
  l, l2: Integer;
begin
  Result := (IsValid) and (FScopeListPtr^.HighestKnown > FIndex);
  if not Result then exit;
  l2 := FScopeListPtr^.List[FIndex + 1].Link;
  assert(l2 <= FScopeListPtr^.HighestKnown);
  Result := (l2 > FIndex + 1);        // Index+1 is First Child, with pointer to Next
  if Result then
    exit;

  l := FScopeListPtr^.List[FIndex].Link; // GetParent  (or -1 for toplevel)
  assert(l <= FScopeListPtr^.HighestKnown);
  if l > Index then
    l := Index - 1;   // This is a first child, make l = parent
  Result := (l2 = l);                 // Index + 1 has same parent
end;

procedure TDwarfScopeInfo.GoParent;
var
  l: Integer;
begin
  if not IsValid then exit;
  l := FScopeListPtr^.List[FIndex].Link; // GetParent  (or -1 for toplevel)
  assert(l <= FScopeListPtr^.HighestKnown);
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

{ TDwarfScopeList }

function TDwarfScopeList.CreateScopeForEntry(AEntry: Pointer; ALink: Integer
  ): Integer;
begin
  inc(FHighestKnown);
  Result := HighestKnown;
  if Result >= Length(List) then
    SetLength(FList, Result + SCOPE_ALLOC_BLOCK_SIZE);
  FList[Result].Entry := AEntry;
  FList[Result].Link := ALink;
end;

function TDwarfScopeList.CreateNextForEntry(AScope: TDwarfScopeInfo;
  AEntry: Pointer): Integer;
var
  l: Integer;
begin
  assert(AScope.IsValid, 'Creating Child for invalid scope');
  assert(AScope.NextIndex < 0, 'Next already set');
  l := List[AScope.Index].Link; // GetParent (or -1 for toplevel)
  assert(l <= HighestKnown);
  if l > AScope.Index then l := AScope.Index - 1;   // This is a first child, make l = parent
  Result := CreateScopeForEntry(AEntry, l);
  if Result > AScope.Index + 1 then  // We have children
    FList[AScope.Index+1].Link := Result;
end;

function TDwarfScopeList.CreateChildForEntry(AScope: TDwarfScopeInfo;
  AEntry: Pointer): Integer;
begin
  //assert(AScope.IsValid, 'Creating Child for invalid scope');
  assert(AScope.Index = HighestKnown, 'Cannot creating Child.Not at end of list');
  Result := CreateScopeForEntry(AEntry, AScope.Index); // First Child, but no parent.next yet
end;

function TDwarfScopeList.BuildList(AnAbbrevList: TDwarfAbbrevList;
  AnInfoData: Pointer; ALength: QWord; AnAddressSize: Byte;
  AnIsDwarf64: Boolean; AVersion: Word; AnUntilTagFound: Cardinal
  ): TDwarfScopeInfo;

  function ParseAttribs(const ADef: PDwarfAbbrev; var p: Pointer): Boolean;
  var
    idx: Integer;
    ADefs: PDwarfAbbrevEntry;
    AddrSize: Byte;
  begin
    ADefs := AnAbbrevList.EntryPointer[ADef^.Index];
    AddrSize := AnAddressSize;
    for idx := 0 to ADef^.Count - 1 do
    begin
      if not SkipEntryDataForForm(p, ADefs^.Form, AddrSize, AnIsDwarf64, AVersion) then
        exit(False);
      inc(ADefs);
    end;
    Result := True;
  end;

var
  Abbrev: Cardinal;
  Def: PDwarfAbbrev;
  p, EntryDataPtr, NextEntryDataPtr, AMaxData: Pointer;
  BuildScope: TDwarfScopeInfo;
  ni: Integer;
  AppendAsChild: Boolean;
begin
  if FList = nil then begin
    SetLength(FList, Min(SCOPE_ALLOC_BLOCK_SIZE, ALength div 2 + 1));
    FHighestKnown := 0;
    FList[0].Link := -1;
    FList[0].Entry  := AnInfoData;
  end;
  assert(FList[0].Entry = AnInfoData, 'TDwarfScopeList.BuildList: FList[0].Entry = AnInfoData');

  Result.Index := -1; // invalid
  BuildScope.Init(@Self);
  BuildScope.Index := FHighestKnown; // last known Buildscope

  // "last rounds" NextEntryDataPtr
  AMaxData := AnInfoData + ALength;
  NextEntryDataPtr := BuildScope.Entry;
  while (NextEntryDataPtr < AMaxData) do
  begin
    EntryDataPtr := NextEntryDataPtr;

    NextEntryDataPtr := AnAbbrevList.FindLe128bFromPointer(EntryDataPtr, Def);
    if NextEntryDataPtr = nil then begin
      Abbrev := ULEB128toOrdinal(EntryDataPtr);
      DebugLn(FPDBG_DWARF_WARNINGS, ['Error: Abbrev not found: ', Abbrev]);
          // TODO shorten array
      exit;
    end;

    if (AnUntilTagFound <> 0) and (Def^.tag = AnUntilTagFound) then begin
      Result := BuildScope;
      Break;
    end;

    if not ParseAttribs(Def, NextEntryDataPtr) then begin
      DebugLn(FPDBG_DWARF_WARNINGS, ['Error: data not parsed:']);
      exit;
    end;
    // NextEntryDataPtr is now at next Buildscope

    if NextEntryDataPtr >= AMaxData then
      break;

    p := NextEntryDataPtr;
    Abbrev := ULEB128toOrdinal(p);
    if Abbrev = 0 then begin      // no more sibling
      AppendAsChild := False;     // children already done
      if (dafHasChildren in Def^.flags) then begin  // current has 0 children
        NextEntryDataPtr := p;
        if NextEntryDataPtr >= AMaxData then
          break;
        Abbrev := ULEB128toOrdinal(p);
      end;
      while (Abbrev = 0) do begin
        NextEntryDataPtr := p;
        if NextEntryDataPtr >= AMaxData then
          break;
        BuildScope.GoParent;
        if not BuildScope.IsValid then begin
          DebugLn(FPDBG_DWARF_WARNINGS, ['Error: Abbrev not found: ', Abbrev]);
          // TODO shorten array
          exit;
        end;
        Abbrev := ULEB128toOrdinal(p);
      end;
      if NextEntryDataPtr >= AMaxData then
        break;
    end
    else
      AppendAsChild := (dafHasChildren in Def^.flags);

    if AppendAsChild then
      ni := CreateChildForEntry(BuildScope, NextEntryDataPtr)
    else
      ni := CreateNextForEntry(BuildScope, NextEntryDataPtr);

    BuildScope.FIndex := ni; // skip check, index was just created / must exist
  end;

  if (NextEntryDataPtr >= AMaxData) then begin
    if (EntryDataPtr > AMaxData) then
      debugln(FPDBG_DWARF_WARNINGS, ['BuildList went past end of memory: ', EntryDataPtr-AMaxData]);
    SetLength(FList, FHighestKnown + 1);
  end;
end;

{ TDwarfLocationStack }

procedure TDwarfLocationStack.IncCapacity;
begin
  SetLength(FList, Max(Length(FList), FCount) + 64);
end;

procedure TDwarfLocationStack.Clear;
begin
  FCount := 0;
  FError := fpErrNoError;
end;

function TDwarfLocationStack.Count: Integer;
begin
  Result := FCount;
end;

function TDwarfLocationStack.Pop: TFpDbgMemLocation;
begin
  Assert(0 < FCount);
  dec(FCount);
  Result := FList[FCount];
  if Result.MType = mlfConstantDeref then
    FError := fpErrLocationParser;
end;

function TDwarfLocationStack.PopForDeref: TFpDbgMemLocation;
begin
  Assert(0 < FCount);
  dec(FCount);
  Result := FList[FCount];
end;

procedure TDwarfLocationStack.Push(const AEntry: TFpDbgMemLocation);
begin
  if Length(FList) <= FCount then
    IncCapacity;
  FList[FCount] := AEntry;
  inc(FCount);
end;

procedure TDwarfLocationStack.PushCopy(AFromIndex: Integer);
begin
  Assert(AFromIndex < FCount);
  if Length(FList) <= FCount then
    IncCapacity;
  FList[FCount] := FList[FCount-1-AFromIndex];
  inc(FCount);
end;

procedure TDwarfLocationStack.PushConst(const AVal: TDBGPtr);
begin
  if Length(FList) <= FCount then
    IncCapacity;
  FList[FCount] := Default(TFpDbgMemLocation);
  with FList[FCount] do begin
    Address := AVal;
    MType := mlfConstant;
  end;
  inc(FCount);
end;

procedure TDwarfLocationStack.PushSignedConst(const AVal: Int64);
begin
  PushConst(TDBGPtr(AVal));
end;

procedure TDwarfLocationStack.PushTargetMem(const AVal: TDBGPtr);
begin
  if Length(FList) <= FCount then
    IncCapacity;
  FList[FCount] := Default(TFpDbgMemLocation);
  with FList[FCount] do begin
    Address := AVal;
    MType := mlfTargetMem;
  end;
  inc(FCount);
end;

procedure TDwarfLocationStack.PushTargetRegister(const ARegNum: Cardinal);
begin
  if Length(FList) <= FCount then
    IncCapacity;
  FList[FCount] := Default(TFpDbgMemLocation);
  FList[FCount] := RegisterLoc(ARegNum);
  inc(FCount);
end;

function TDwarfLocationStack.Peek: PFpDbgMemLocation;
begin
  Assert(0 < FCount);
  Result := @FList[FCount-1];
  if Result^.MType = mlfConstantDeref then
    FError := fpErrLocationParser;
end;

function TDwarfLocationStack.PeekForDeref: PFpDbgMemLocation;
begin
  Assert(0 < FCount);
  Result := @FList[FCount-1];
end;

function TDwarfLocationStack.PeekKind: TFpDbgMemLocationType;
begin
  if FCount = 0 then
    Result := mlfInvalid
  else
    Result := FList[FCount-1].MType;
end;

function TDwarfLocationStack.Peek(AIndex: Integer): PFpDbgMemLocation;
begin
  Assert(AIndex < FCount);
  Result := @FList[FCount-1-AIndex];
  if Result^.MType = mlfConstantDeref then
    FError := fpErrLocationParser;
end;

procedure TDwarfLocationStack.Modify(AIndex: Integer;
  const AEntry: TFpDbgMemLocation);
begin
  Assert(AIndex < FCount);
  FList[FCount-1-AIndex] := AEntry;
end;

procedure TDwarfLocationStack.Copy(AFromIndex, AIndex: Integer);
begin
  Assert(AIndex < FCount);
  Assert(AFromIndex < FCount);
  FList[FCount-1-AIndex] := FList[FCount-1-AFromIndex];
end;

{ TDwarfLocationExpression }

constructor TDwarfLocationExpression.Create(AExpressionData: Pointer;
  AMaxCount: Integer; ACU: TDwarfCompilationUnit;
  AContext: TFpDbgLocationContext);
begin
  FStack.Clear;
  FCU := ACU;
  FData := AExpressionData;
  FMaxData := FData + AMaxCount;
  FContext := AContext;
end;

procedure TDwarfLocationExpression.SetLastError(ALastError: TFpError);
begin
  assert(Not IsError(FLastError), 'TDwarfLocationExpression.SetLastError: Not IsError(FLastError)');
  FLastError := ALastError;
end;

procedure TDwarfLocationExpression.Evaluate;
var
  CurInstr, CurData: PByte;
  AddrSize: Byte;

  procedure SetError(AnError: TFpError);
  begin
    FStack.Push(InvalidLoc); // Mark as failed
    FLastError := AnError;
    debugln(FPDBG_DWARF_ERRORS,
            ['DWARF ERROR in TDwarfLocationExpression: Failed at Pos=', CurInstr-FData,
             ' OpCode=', IntToHex(CurInstr^, 2), ' Depth=', FStack.Count,
             ' Data: ', dbgMemRange(FData, FMaxData-FData),
             ' Extra: ', ErrorHandler.ErrorAsString(AnError) ]);
  end;

  procedure SetError(AnInternalErrorCode: TFpErrorCode = fpErrNoError);
  begin
    FStack.Push(InvalidLoc); // Mark as failed
    if IsError(FContext.LastMemError)
    then FLastError := CreateError(fpErrLocationParserMemRead, FContext.LastMemError, [])
    else FLastError := CreateError(fpErrLocationParser, []);
    debugln(FPDBG_DWARF_ERRORS,
            ['DWARF ERROR in TDwarfLocationExpression: Failed at Pos=', CurInstr-FData,
             ' OpCode=', IntToHex(CurInstr^, 2), ' Depth=', FStack.Count,
             ' Data: ', dbgMemRange(FData, FMaxData-FData),
             ' MemReader.LastError: ', ErrorHandler.ErrorAsString(FContext.LastMemError),
             ' Extra: ', ErrorHandler.ErrorAsString(AnInternalErrorCode, []) ]);
  end;

  function AssertAddressOnStack: Boolean; inline;
  begin
    Result := (FStack.PeekKind in [mlfTargetMem, mlfSelfMem, mlfConstantDeref]);
    if not Result then
      SetError(fpErrLocationParserNoAddressOnStack);
  end;

  function AssertAddressOrRegOnStack: Boolean; inline;
  begin
    Result := (FStack.PeekKind in [mlfTargetMem, mlfSelfMem, mlfConstantDeref, mlfTargetRegister]);
    if not Result then
      SetError(fpErrLocationParserNoAddressOnStack);
  end;

  function AssertMinCount(ACnt: Integer): Boolean; inline;
  begin
    Result := FStack.Count >= ACnt;
    if not Result then
      SetError(fpErrLocationParserMinStack);
  end;

  function ReadAddressFromMemory(const AnAddress: TFpDbgMemLocation; ASize: Cardinal; out AValue: TFpDbgMemLocation): Boolean;
  begin
    //TODO: zero fill / sign extend
    if (ASize > SizeOf(AValue)) or (ASize > AddrSize) then exit(False);
    Result := FContext.ReadAddress(AnAddress, SizeVal(ASize), AValue);
    if not Result then
      SetError;
  end;

  function ReadAddressFromMemoryEx(const AnAddress: TFpDbgMemLocation; AnAddrSpace: TDbgPtr; ASize: Cardinal; out AValue: TFpDbgMemLocation): Boolean;
  begin
    //TODO: zero fill / sign extend
    if (ASize > SizeOf(AValue)) or (ASize > AddrSize) then exit(False);
    AValue := FContext.ReadAddressEx(AnAddress, AnAddrSpace, SizeVal(ASize));
    Result := IsValidLoc(AValue);
    if not Result then
      SetError;
  end;

var
  NewLoc, Loc: TFpDbgMemLocation;
  NewValue: TDbgPtr;
  i: TDbgPtr;
  x : integer;
  Entry: TFpDbgMemLocation;
  EntryP: PFpDbgMemLocation;
begin
  (* Returns the address of the value.
     - Except for DW_OP_regN and DW_OP_piece, which return the value itself. (Not sure about DW_OP_constN)
     - Some tags override that, e.g.: DW_AT_upper_bound will allways interpret the result as a value.
  *)

  AddrSize := FCU.FAddressSize;
  FContext.ClearLastMemError;
  FLastError := NoError;
  CurData := FData;
  while CurData < FMaxData do begin
    CurInstr := CurData;
    inc(CurData);
    case CurInstr^ of
      DW_OP_nop: ;
      DW_OP_addr:  begin
          FStack.Push(FCU.ReadTargetAddressFromDwarfSection(CurData, True)); // always mlfTargetMem;
        end;
      DW_OP_deref: begin
          if not AssertAddressOrRegOnStack then exit;
          EntryP := FStack.PeekForDeref;
          if not ReadAddressFromMemory(EntryP^, AddrSize, NewLoc) then exit;
          EntryP^ := NewLoc; // mlfTargetMem;
        end;
      DW_OP_xderef: begin
          if not AssertAddressOnStack  then exit;
          Loc := FStack.Pop;
          if not AssertAddressOnStack then exit;
          EntryP := FStack.Peek;
// TODO check address is valid
          if not ReadAddressFromMemoryEx(Loc, EntryP^.Address, AddrSize, NewLoc) then exit;
          EntryP^ := NewLoc; // mlfTargetMem;
        end;
      DW_OP_deref_size: begin
          if not AssertAddressOrRegOnStack then exit;
          EntryP := FStack.PeekForDeref;
          if not ReadAddressFromMemory(EntryP^, ReadUnsignedFromExpression(CurData, 1), NewLoc) then exit;
          EntryP^ := NewLoc; // mlfTargetMem;
        end;
      DW_OP_xderef_size: begin
          if not AssertAddressOnStack  then exit;
          Loc := FStack.Pop;
          if not AssertAddressOnStack then exit;
          EntryP := FStack.Peek;
// TODO check address is valid
          if not ReadAddressFromMemoryEx(Loc, EntryP^.Address, ReadUnsignedFromExpression(CurData, 1), NewLoc) then exit;
          EntryP^ := NewLoc; // mlfTargetMem;
        end;

      DW_OP_const1u: FStack.PushConst(ReadUnsignedFromExpression(CurData, 1));
      DW_OP_const2u: FStack.PushConst(ReadUnsignedFromExpression(CurData, 2));
      DW_OP_const4u: FStack.PushConst(ReadUnsignedFromExpression(CurData, 4));
      DW_OP_const8u: FStack.PushConst(ReadUnsignedFromExpression(CurData, 8));
      DW_OP_constu:  FStack.PushConst(ReadUnsignedFromExpression(CurData, 0));
      DW_OP_const1s: FStack.PushSignedConst(ReadSignedFromExpression(CurData, 1));
      DW_OP_const2s: FStack.PushSignedConst(ReadSignedFromExpression(CurData, 2));
      DW_OP_const4s: FStack.PushSignedConst(ReadSignedFromExpression(CurData, 4));
      DW_OP_const8s: FStack.PushSignedConst(ReadSignedFromExpression(CurData, 8));
      DW_OP_consts:  FStack.PushSignedConst(ReadSignedFromExpression(CurData, 0));
      DW_OP_lit0..DW_OP_lit31: FStack.PushConst(CurInstr^-DW_OP_lit0);

      (* DW_OP_reg0..31 and DW_OP_regx
         The DWARF spec does *not* specify that those should push the result on
         the stack.
         However it does not matter, since there can be no other instruction
         after it to access the stack.
         From the spec:
           "A register location description must stand alone as the entire
            description of an object or a piece of an object"

         For DW_AT_frame_base the spec (dwarf 5) says:
           "The use of one of the DW_OP_reg<n> operations in this context is
            equivalent to using DW_OP_breg<n>(0) but more compact"
         However, it does not say if this removes the "stand alone" restriction.
      *)
      DW_OP_reg0..DW_OP_reg31: begin
          FStack.PushTargetRegister(CurInstr^-DW_OP_reg0);
          // Dwarf-5
          if FIsDwAtFrameBase then begin
            EntryP := FStack.PeekForDeref;
            if not ReadAddressFromMemory(EntryP^, AddrSize, NewLoc) then exit;
            EntryP^ := NewLoc; // mlfTargetMem;
          end;
        end;
      DW_OP_regx: begin
          FStack.PushTargetRegister(ULEB128toOrdinal(CurData));
          // Dwarf-5
          if FIsDwAtFrameBase then begin
            EntryP := FStack.PeekForDeref;
            if not ReadAddressFromMemory(EntryP^, AddrSize, NewLoc) then exit;
            EntryP^ := NewLoc; // mlfTargetMem;
          end;
        end;

      DW_OP_breg0..DW_OP_breg31: begin
          if not FContext.ReadRegisterasAddress(CurInstr^-DW_OP_breg0, NewValue) then begin
            SetError;
            exit;
          end;
          {$PUSH}{$R-}{$Q-}
          FStack.PushTargetMem(NewValue+SLEB128toOrdinal(CurData));
          {$POP}
        end;
      DW_OP_bregx: begin
          if not FContext.ReadRegisterasAddress(ULEB128toOrdinal(CurData), NewValue) then begin
            SetError;
            exit;
          end;
          {$PUSH}{$R-}{$Q-}
          FStack.PushTargetMem(NewValue+SLEB128toOrdinal(CurData));
          {$POP}
        end;

      DW_OP_fbreg: begin
          if (FFrameBase = 0) then begin
            FFrameBase := FContext.FrameBase;
            if FFrameBase = 0 then
              SetError(FContext.FrameBaseError);
          end;
          if FFrameBase = 0 then
            exit;
          {$PUSH}{$R-}{$Q-}
          FStack.PushTargetMem(FFrameBase+SLEB128toOrdinal(CurData));
          {$POP}
        end;

      DW_OP_dup: begin
          if not AssertMinCount(1) then exit;
          FStack.PushCopy(0);
        end;
      DW_OP_drop: begin
          if not AssertMinCount(1) then exit;
          FStack.Pop;
        end;
      DW_OP_over: begin
          if not AssertMinCount(2) then exit;
          FStack.PushCopy(1);
        end;
      DW_OP_pick: begin
          i := ReadUnsignedFromExpression(CurData, 1);
          if not AssertMinCount(i) then exit;
          FStack.PushCopy(i);
        end;
      DW_OP_swap: begin
          if not AssertMinCount(2) then exit;
          Entry := FStack.Peek^;
          FStack.Copy(1, 0);
          FStack.Modify(1, Entry);
        end;
      DW_OP_rot: begin
          if not AssertMinCount(3) then exit;
          Entry := FStack.Peek^;
          FStack.Copy(1, 0);
          FStack.Copy(2, 1);
          FStack.Modify(2, Entry);
        end;

      DW_OP_abs: begin
          if not AssertMinCount(1) then exit;
          EntryP := FStack.Peek;
          EntryP^.Address := abs(int64(EntryP^.Address));
        end;
      DW_OP_neg: begin
          if not AssertMinCount(1) then exit;
          EntryP := FStack.Peek;
          EntryP^.Address := TDbgPtr(-int64(EntryP^.Address));
        end;
      DW_OP_plus: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          if FStack.PeekKind = mlfConstantDeref then begin
            EntryP := FStack.PeekForDeref;

            if Entry.Address >= SizeOf(TDbgPtr) then // includes negative // SHL does not make sense, as it pretends there would be data
              FStack.FError := fpErrLocationParser
            else
            {$PUSH}{$R-}{$Q-}
            EntryP^.Address := EntryP^.Address shr (Entry.Address * 8);
            {$POP}
          end
          else begin
            EntryP := FStack.Peek;
            {$PUSH}{$R-}{$Q-}
            //TODO: 32 bit overflow?
            EntryP^.Address := Entry.Address+EntryP^.Address;
            {$POP}
            (* TargetMem may be a constant after deref. So if SelfMem is involved, keep it. *)
            if (EntryP^.MType <> mlfSelfMem) and (Entry.MType in [mlfTargetMem, mlfSelfMem]) then
              EntryP^.MType := Entry.MType;
          end;
        end;
      DW_OP_plus_uconst: begin
          if not AssertMinCount(1) then exit;
          if FStack.PeekKind = mlfConstantDeref then begin
            EntryP := FStack.PeekForDeref;
            i :=  ULEB128toOrdinal(CurData);
            if i >= SizeOf(TDbgPtr) then
              EntryP^.Address := 0
            else
            {$PUSH}{$R-}{$Q-}
            EntryP^.Address := EntryP^.Address shr (i * 8);
            {$POP}
          end
          else begin
            EntryP := FStack.Peek;
            {$PUSH}{$R-}{$Q-}
            EntryP^.Address := EntryP^.Address + ULEB128toOrdinal(CurData);
            {$POP}
          end;
        end;
      DW_OP_minus: begin
          if not AssertMinCount(2) then exit;
          // TODO: small negative for mlfConstantDeref
          Entry  := FStack.Pop;
          EntryP := FStack.Peek;
          {$PUSH}{$R-}{$Q-}
          //TODO: 32 bit overflow?
          EntryP^.Address := EntryP^.Address - Entry.Address;
          {$POP}
          (* TargetMem may be a constant after deref. So if SelfMem is involved, keep it. *)
          if (EntryP^.MType <> mlfSelfMem) and (Entry.MType in [mlfTargetMem, mlfSelfMem]) then
            EntryP^.MType := Entry.MType;
        end;
      DW_OP_mul: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          EntryP := FStack.Peek;
          //{$PUSH}{$R-}{$Q-}
          EntryP^.Address := TDbgPtr(int64(EntryP^.Address) * int64(Entry.Address));
          //{$POP}
          if (EntryP^.MType <> mlfSelfMem) and (Entry.MType in [mlfTargetMem, mlfSelfMem]) then
            EntryP^.MType := Entry.MType;
        end;
      DW_OP_div: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          EntryP := FStack.Peek;
          //{$PUSH}{$R-}{$Q-}
          EntryP^.Address := TDbgPtr(int64(EntryP^.Address) div int64(Entry.Address));
          //{$POP}
          if (EntryP^.MType <> mlfSelfMem) and (Entry.MType in [mlfTargetMem, mlfSelfMem]) then
            EntryP^.MType := Entry.MType;
        end;
      DW_OP_mod: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          EntryP := FStack.Peek;
          //{$PUSH}{$R-}{$Q-}
          EntryP^.Address := TDbgPtr(int64(EntryP^.Address) mod int64(Entry.Address));
          //{$POP}
          if (EntryP^.MType <> mlfSelfMem) and (Entry.MType in [mlfTargetMem, mlfSelfMem]) then
            EntryP^.MType := Entry.MType;
        end;

      DW_OP_and: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          EntryP := FStack.Peek;
          EntryP^.Address := EntryP^.Address and Entry.Address;
          if (EntryP^.MType <> mlfSelfMem) and (Entry.MType in [mlfTargetMem, mlfSelfMem]) then
            EntryP^.MType := Entry.MType;
        end;
      DW_OP_not: begin
          if not AssertMinCount(1) then exit;
          EntryP := FStack.Peek;
          EntryP^.Address := not EntryP^.Address;
        end;
      DW_OP_or: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          EntryP := FStack.Peek;
          EntryP^.Address := EntryP^.Address or Entry.Address;
          if (EntryP^.MType <> mlfSelfMem) and (Entry.MType in [mlfTargetMem, mlfSelfMem]) then
            EntryP^.MType := Entry.MType;
        end;
      DW_OP_xor: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          EntryP := FStack.Peek;
          EntryP^.Address := EntryP^.Address xor Entry.Address;
          if (EntryP^.MType <> mlfSelfMem) and (Entry.MType in [mlfTargetMem, mlfSelfMem]) then
            EntryP^.MType := Entry.MType;
        end;
      DW_OP_shl: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          EntryP := FStack.Peek;
          EntryP^.Address := EntryP^.Address shl Entry.Address;
          if (EntryP^.MType <> mlfSelfMem) and (Entry.MType in [mlfTargetMem, mlfSelfMem]) then
            EntryP^.MType := Entry.MType;
        end;
      DW_OP_shr: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          EntryP := FStack.Peek;
          EntryP^.Address := EntryP^.Address shr Entry.Address;
          if (EntryP^.MType <> mlfSelfMem) and (Entry.MType in [mlfTargetMem, mlfSelfMem]) then
            EntryP^.MType := Entry.MType;
        end;
      DW_OP_shra: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          EntryP := FStack.Peek;
          {$PUSH}{$R-}{$Q-}
          EntryP^.Address := TDBGPtr( int64(EntryP^.Address) div int64(1 shl (Entry.Address - 1)) );
          {$POP}
          if (EntryP^.MType <> mlfSelfMem) and (Entry.MType in [mlfTargetMem, mlfSelfMem]) then
            EntryP^.MType := Entry.MType;
        end;

      DW_OP_skip: begin
          x := ReadSignedFromExpression(CurData, 2);
          CurData := CurData + x;
        end;
      DW_OP_bra: begin
          if not AssertMinCount(1) then exit;
          Entry  := FStack.PopForDeref;
          x := ReadSignedFromExpression(CurData, 2);
          // mlfConstantDeref => The virtual address pointing to this constant is not nil
          if (Entry.Address <> 0) or (Entry.MType = mlfConstantDeref) then
            CurData := CurData + x;
        end;

      DW_OP_eq: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          EntryP := FStack.Peek;
          if Entry.Address = EntryP^.Address
          then EntryP^.Address := 1
          else EntryP^.Address := 0;
          EntryP^.MType := mlfConstant;
        end;
      DW_OP_ge: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          EntryP := FStack.Peek;
          if int64(Entry.Address) >= int64(EntryP^.Address)
          then EntryP^.Address := 1
          else EntryP^.Address := 0;
          EntryP^.MType := mlfConstant;
        end;
      DW_OP_gt: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          EntryP := FStack.Peek;
          if int64(Entry.Address) > int64(EntryP^.Address)
          then EntryP^.Address := 1
          else EntryP^.Address := 0;
          EntryP^.MType := mlfConstant;
        end;
      DW_OP_le: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          EntryP := FStack.Peek;
          if int64(Entry.Address) <= int64(EntryP^.Address)
          then EntryP^.Address := 1
          else EntryP^.Address := 0;
          EntryP^.MType := mlfConstant;
        end;
      DW_OP_lt: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          EntryP := FStack.Peek;
          if int64(Entry.Address) < int64(EntryP^.Address)
          then EntryP^.Address := 1
          else EntryP^.Address := 0;
          EntryP^.MType := mlfConstant;
        end;
      DW_OP_ne: begin
          if not AssertMinCount(2) then exit;
          Entry  := FStack.Pop;
          EntryP := FStack.Peek;
          if Entry.Address <> EntryP^.Address
          then EntryP^.Address := 1
          else EntryP^.Address := 0;
          EntryP^.MType := mlfConstant;
        end;

      DW_OP_piece: begin
          if not AssertMinCount(1) then exit; // no piece avail
          x := ReadUnsignedFromExpression(CurData, 0);
          Entry :=  FStack.Pop;
// TODO: assemble data // Not implemented
// If entry is an address (not a register) then it points to the value
          SetError(fpErrLocationParser);
          exit;
        end;

      // dwarf 3
      DW_OP_push_object_address: begin
        if not IsValidLoc(FCurrentObjectAddress) then begin
          SetError;
          exit;
        end;
        Push(FCurrentObjectAddress);
      end;

      DW_OP_call_frame_cfa: begin
          NewValue := Context.CfaFrameBase;
          if NewValue = 0 then begin
            SetError(Context.FCfarameBaseError);
            exit;
          end;
          FStack.PushTargetMem(NewValue);
        end;
(*
  // --- DWARF3 ---
  DW_OP_call2                 = $98;    // 1 2-byte offset of DIE
  DW_OP_call4                 = $99;    // 1 4-byte offset of DIE
  DW_OP_call_ref              = $9a;    // 1 4- or 8-byte offset of DIE
  DW_OP_form_tls_address      = $9b;    // 0
  DW_OP_bit_piece             = $9d;    // 2
*)
      // dwarf 4
      DW_OP_stack_value: begin
          EntryP := FStack.Peek;
          EntryP^.MType := mlfConstantDeref;
      end;

      else
        begin
          debugln(FPDBG_DWARF_ERRORS, ['DWARF ERROR in TDwarfLocationExpression.Evaluate UNKNOWN ', CurInstr^]);
          SetError;
          exit;
        end;
    end;

    if FStack.FError <> fpErrNoError then begin
      SetError(FStack.FError);
      exit;
    end;
  end;

  if (FLastError = nil) and (FStack.FError = fpErrNoError) then begin
    if not AssertMinCount(1) then exit; // no value for result
    //TODO: If a caller expects it, it could accept mlfConstantDeref as result (but it would still need to deref it)
    if FStack.FError <> fpErrNoError then
      SetError(FStack.FError);
  end;
end;

function TDwarfLocationExpression.ResultData: TFpDbgMemLocation;
begin
  if (FLastError <> nil) or (FStack.FError <> fpErrNoError) or (FStack.Count = 0) then
    exit(InvalidLoc);

  if FStack.Count > 0 then
    Result := FStack.Peek^
  else
    Result := InvalidLoc;
end;

procedure TDwarfLocationExpression.Push(const AValue: TFpDbgMemLocation);
begin
  FStack.Push(AValue);
end;

{ TDwarfInformationEntry }

procedure TDwarfInformationEntry.ScopeChanged;
begin
  FScopeCurrentInfoPtr := FScope.Current;
  if FScopeCurrentInfoPtr <> nil then
    FInformationEntry := FScopeCurrentInfoPtr^.Entry
  else
    FInformationEntry := nil;
  FFlags := [];
  FInformationData := nil;
  if FAbstractOrigin <> nil then
    ReleaseRefAndNil(FAbstractOrigin);
end;

procedure TDwarfInformationEntry.PrepareAbbrev;
begin
  if (dieAbbrevValid in FFlags) or (FInformationEntry = nil) then
    exit;
  FInformationData := FCompUnit.FAbbrevList.FindLe128bFromPointer(FInformationEntry, FAbbrev);
  Include(FFlags, dieAbbrevValid);
end;

function TDwarfInformationEntry.GetAttribForm(AnIdx: Integer): Cardinal;
begin
  Result := FAbbrevData[AnIdx].Form;
end;

function TDwarfInformationEntry.PrepareAbbrevData: Boolean;
var
  AbbrList: TDwarfAbbrevList;
begin
  if FInformationEntry = nil then
    exit(False);

  Result := FAbbrevData <> nil;
  if dieAbbrevDataValid in FFlags then
    exit;
  AbbrList := FCompUnit.FAbbrevList;

  // PrepareAbbrev;
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

procedure TDwarfInformationEntry.GoNextFast;
begin
  FScope.GoNext;
  ScopeChanged;
end;

procedure TDwarfInformationEntry.GoChildFast;
begin
  FScope.GoChild;
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

function TDwarfInformationEntry.ScopeDebugText: String;
begin
  Result := dbgs(FScope, FCompUnit);
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

function TDwarfInformationEntry.HasAttrib(AnAttrib: Cardinal): Boolean;
var
  i: Integer;
begin
  Result := False;
  if not PrepareAbbrevData then exit;
  for i := 0 to FAbbrev^.count - 1 do
    if FAbbrevData[i].Attribute = AnAttrib then begin
      Result := True;
      exit;
  end;
end;

procedure TDwarfInformationEntry.ComputeKnownHashes(
  AKNownHashes: PKnownNameHashesArray);
var
  EntryName: PChar;
  NextTopLevel: Integer;
  h: LongWord;
  InEnum: Boolean;
begin
  InEnum := False;
  if not HasValidScope then
    exit;

  h := objpas.Hash(UTF8UpperCaseFast(CompUnit.UnitName)) and $7fff or $8000;
  AKNownHashes^[h and KnownNameHashesBitMask] := True;

  NextTopLevel := 0;
  dec(FScope.FIndex);
  while (FScope.Index < FScope.ScopeListPtr^.HighestKnown) do begin
    inc(FScope.FIndex);
    ScopeChanged;
    PrepareAbbrev;
    if FAbbrev = nil then
      continue;

    if not (dafHasName in FAbbrev^.flags) then begin
      if FScope.Index >= NextTopLevel then
        NextTopLevel := FScope.GetNextIndex;
      Continue;
    end;

    if not ReadValue(DW_AT_name, EntryName) then begin
      if FScope.Index >= NextTopLevel then
        NextTopLevel := FScope.GetNextIndex;
      Continue;
    end;
    h := objpas.Hash(UTF8UpperCaseFast(EntryName)) and $7fff or $8000;
    FScope.Current^.NameHash := h;
    if (FScope.Index >= NextTopLevel) or InEnum then
      AKNownHashes^[h and KnownNameHashesBitMask] := True;

    if FScope.Index >= NextTopLevel then begin
      InEnum := False;
      if FAbbrev^.tag = DW_TAG_enumeration_type then begin
        InEnum := True;
        NextTopLevel := FScope.GetNextIndex;
        Continue;
      end;
    end;

    if FScope.Index >= NextTopLevel then
      NextTopLevel := FScope.GetNextIndex;
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

function TDwarfInformationEntry.GoNamedChild(const ANameInfo: TNameSearchInfo): Boolean;
var
  EntryName: PChar;
begin
  Result := False;
  if ANameInfo.NameUpper = '' then
    exit;
  GoChild;
  if not HasValidScope then
    exit;

  while HasValidScope do begin
    if FScopeCurrentInfoPtr^.NameHash <> ANameInfo.NameHash then begin
      GoNextFast;
      Continue;
    end;

    PrepareAbbrev;
    if (FAbbrev = nil) or not (dafHasName in FAbbrev^.flags) then begin
      GoNextFast;
      Continue;
    end;
    if not ReadValue(DW_AT_name, EntryName) then begin
      GoNextFast;
      Continue;
    end;

    if CompareUtf8BothCase(PChar(ANameInfo.NameUpper), PChar(ANameInfo.nameLower), EntryName) then begin
      // TODO: check DW_AT_start_scope;
      DebugLn(FPDBG_DWARF_SEARCH, ['GoNamedChild found ', dbgs(FScope, FCompUnit), '  Result=', DbgSName(Self), '  FOR ', ANameInfo.NameUpper]);
      Result := True;
      exit;
    end;

    GoNextFast;
  end;
end;

function TDwarfInformationEntry.GoNamedChildEx(
  const ANameInfo: TNameSearchInfo; ASkipArtificial: Boolean;
  ASkipEnumMembers: Boolean): Boolean;
var
  Val: Integer;
  EntryName: PChar;
  InEnum: Boolean;
  ParentScopIdx: Integer;
  sc: PDwarfScopeInfoRec;
begin
  Result := False;
  InEnum := False;
  if ANameInfo.NameUpper = '' then
    exit;
  GoChild;
  if not HasValidScope then
    exit;
  while true do begin
    while HasValidScope do begin
      sc := FScopeCurrentInfoPtr;
      if ASkipEnumMembers then begin
        if (sc^.NameHash <> ANameInfo.NameHash) then begin
          GoNextFast;
          Continue;
        end;
      end
      else begin
        if sc^.NameHash = 0 then begin
          GoNextFast;
          Continue;
        end;
      end;

      PrepareAbbrev;
      if (FAbbrev = nil) or not (dafHasName in FAbbrev^.flags) then begin
        assert(false);
        GoNextFast;
        Continue;
      end;

      if (sc^.NameHash <> ANameInfo.NameHash) and
         ( ASkipEnumMembers or (FAbbrev^.tag <> DW_TAG_enumeration_type) )
      then begin
        GoNextFast;
        Continue;
      end;

      if ASkipArtificial and (dafHasArtifical in FAbbrev^.flags) then begin
        if ReadValue(DW_AT_artificial, Val) and (Val <> 0) then begin
          GoNextFast;
          Continue;
        end;
      end;


      if (sc^.NameHash = ANameInfo.NameHash) then begin
        if not ReadValue(DW_AT_name, EntryName) then begin
          GoNextFast;
          Continue;
        end;

        if CompareUtf8BothCase(PChar(ANameInfo.NameUpper), PChar(ANameInfo.nameLower), EntryName) then begin
          // TODO: check DW_AT_start_scope;
          DebugLn(FPDBG_DWARF_SEARCH, ['GoNamedChildEX found ', dbgs(FScope, FCompUnit), '  Result=', DbgSName(Self), '  FOR ', ANameInfo.nameLower]);
          Result := True;
          exit;
        end;
      end;

      if (not ASkipEnumMembers) and (FAbbrev^.tag = DW_TAG_enumeration_type) then begin
        assert(not InEnum, 'nested enum');
        InEnum := True;
        ParentScopIdx := ScopeIndex;
        GoChildFast;
        Continue;
      end;

      GoNextFast;
    end;

    if InEnum then begin
      InEnum := False;
      ScopeIndex := ParentScopIdx;
      GoNextFast;
      continue;
    end;
    break;
  end;
end;

function TDwarfInformationEntry.GoNamedChildMatchCaseEx(
  const ANameInfo: TNameSearchInfo): Boolean;
var
  EntryName: PChar;
begin
  Result := False;
  if ANameInfo.NameUpper = '' then
    exit;
  GoChild;
  if not HasValidScope then
    exit;

  while HasValidScope do begin
    if FScopeCurrentInfoPtr^.NameHash = 0 then begin
      GoNextFast;
      Continue;
    end;

    PrepareAbbrev;
    if (FAbbrev = nil) or not (dafHasName in FAbbrev^.flags) then begin
      Assert(false);
      GoNextFast;
      Continue;
    end;

    if (FScopeCurrentInfoPtr^.NameHash = ANameInfo.NameHash) then begin
      if not ReadValue(DW_AT_name, EntryName) then begin
        GoNextFast;
        Continue;
      end;

      if CompareMem(PChar(ANameInfo.nameLower), @EntryName, Length(EntryName)) then begin
        // TODO: check DW_AT_start_scope;
        DebugLn(FPDBG_DWARF_SEARCH, ['GoNamedChildEX found ', dbgs(FScope, FCompUnit), '  Result=', DbgSName(Self), '  FOR ', ANameInfo.nameLower]);
        Result := True;
        exit;
      end;
    end;

    GoNextFast;
  end;
end;

constructor TDwarfInformationEntry.Create(ACompUnit: TDwarfCompilationUnit;
  AnInformationEntry: Pointer);
begin
  inherited Create;
  AddReference;
  FCompUnit := ACompUnit;
  FInformationEntry := AnInformationEntry;
  FScope.Init(@FCompUnit.FScopeList);
  if (FInformationEntry <> nil) and not SearchScope then begin
    DebugLn(FPDBG_DWARF_ERRORS or FPDBG_DWARF_ERRORS, 'Error: Invalid pointer to InformationEntry');
    FInformationEntry := nil;
  end;
end;

constructor TDwarfInformationEntry.Create(ACompUnit: TDwarfCompilationUnit;
  AScope: TDwarfScopeInfo);
begin
  inherited Create;
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

function TDwarfInformationEntry.GetAttribData(AnAttrib: Cardinal; out
  AnAttribData: TDwarfAttribData): Boolean;
var
  i: Integer;
  p: PDwarfAbbrevEntry;
  AddrSize: Byte;
  InfoPointer: Pointer;
  IsDwarf64: Boolean;
  Version: Word;
begin
  Result := False;
  if not PrepareAbbrevData then
    exit;

  AddrSize    := FCompUnit.FAddressSize;
  IsDwarf64   := FCompUnit.IsDwarf64;
  Version     := FCompUnit.Version;
  InfoPointer := FInformationData;
  p := FAbbrevData;
  for i := 0 to FAbbrev^.count - 1 do begin
    if p^.Attribute = AnAttrib then begin
      AnAttribData.Idx := i;
      AnAttribData.InfoPointer := InfoPointer;
      AnAttribData.InformationEntry := Self;
      Result := True;
      exit;
    end;
    SkipEntryDataForForm(InfoPointer, p^.Form, AddrSize, IsDwarf64, Version);
    inc(p);
  end;

  if (dafHasAbstractOrigin in FAbbrev^.flags) and PrepareAbstractOrigin then
    Result := FAbstractOrigin.GetAttribData(AnAttrib, AnAttribData);
end;

function TDwarfInformationEntry.FindNamedChild(const AName: String
  ): TDwarfInformationEntry;
begin
  Result := nil;
  if not MaybeSearchScope then
    exit;

  Result := TDwarfInformationEntry.Create(FCompUnit, FScope);
// TODO: parent
  if Result.GoNamedChild(NameInfoForSearch(AName)) then
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

function TDwarfInformationEntry.ReadValue(const AnAttribData: TDwarfAttribData;
  out AValue: Integer): Boolean;
begin
  Result := AnAttribData.InformationEntry.FCompUnit.ReadValue(
    AnAttribData.InfoPointer,
    AnAttribData.InformationEntry.FAbbrevData[AnAttribData.Idx].Form,
    AValue
  );
end;

function TDwarfInformationEntry.ReadValue(const AnAttribData: TDwarfAttribData;
  out AValue: Int64): Boolean;
begin
  Result := AnAttribData.InformationEntry.FCompUnit.ReadValue(
    AnAttribData.InfoPointer,
    AnAttribData.InformationEntry.FAbbrevData[AnAttribData.Idx].Form,
    AValue
  );
end;

function TDwarfInformationEntry.ReadValue(const AnAttribData: TDwarfAttribData;
  out AValue: Cardinal): Boolean;
begin
  Result := AnAttribData.InformationEntry.FCompUnit.ReadValue(
    AnAttribData.InfoPointer,
    AnAttribData.InformationEntry.FAbbrevData[AnAttribData.Idx].Form,
    AValue
  );
end;

function TDwarfInformationEntry.ReadValue(const AnAttribData: TDwarfAttribData;
  out AValue: QWord): Boolean;
begin
  Result := AnAttribData.InformationEntry.FCompUnit.ReadValue(
    AnAttribData.InfoPointer,
    AnAttribData.InformationEntry.FAbbrevData[AnAttribData.Idx].Form,
    AValue
  );
end;

function TDwarfInformationEntry.ReadValue(const AnAttribData: TDwarfAttribData;
  out AValue: PChar): Boolean;
begin
  Result := AnAttribData.InformationEntry.FCompUnit.ReadValue(
    AnAttribData.InfoPointer,
    AnAttribData.InformationEntry.FAbbrevData[AnAttribData.Idx].Form,
    AValue
  );
end;

function TDwarfInformationEntry.ReadValue(const AnAttribData: TDwarfAttribData;
  out AValue: String): Boolean;
begin
  Result := AnAttribData.InformationEntry.FCompUnit.ReadValue(
    AnAttribData.InfoPointer,
    AnAttribData.InformationEntry.FAbbrevData[AnAttribData.Idx].Form,
    AValue
  );
end;

function TDwarfInformationEntry.ReadValue(const AnAttribData: TDwarfAttribData;
  out AValue: TByteDynArray; AnFormString: Boolean): Boolean;
begin
  Result := AnAttribData.InformationEntry.FCompUnit.ReadValue(
    AnAttribData.InfoPointer,
    AnAttribData.InformationEntry.FAbbrevData[AnAttribData.Idx].Form,
    AValue, AnFormString
  );
end;

function TDwarfInformationEntry.ReadAddressValue(
  const AnAttribData: TDwarfAttribData; out AValue: TDBGPtr): Boolean;
begin
  Result := AnAttribData.InformationEntry.FCompUnit.ReadAddressValue(
    AnAttribData.InfoPointer,
    AnAttribData.InformationEntry.FAbbrevData[AnAttribData.Idx].Form,
    AValue
  );
end;

function TDwarfInformationEntry.ReadReference(
  const AnAttribData: TDwarfAttribData; out AValue: Pointer; out
  ACompUnit: TDwarfCompilationUnit): Boolean;
begin
  Result := AnAttribData.InformationEntry.DoReadReference(
    AnAttribData.Idx, AnAttribData.InfoPointer,
    AValue, ACompUnit
  );
end;

function TDwarfInformationEntry.DoReadReference(
  InfoIdx: Integer; InfoData: pointer; out AValue: Pointer; out
  ACompUnit: TDwarfCompilationUnit): Boolean;
var
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
  if (InfoIdx < 0) or (FAbbrevData = nil) then
    exit;

  Form := FAbbrevData[InfoIdx].Form;
  if (Form = DW_FORM_ref1) or (Form = DW_FORM_ref2) or (Form = DW_FORM_ref4) or
     (Form = DW_FORM_ref8) or (Form = DW_FORM_ref_udata)
  then begin
    Result := FCompUnit.ReadValue(InfoData, Form, Offs);
    if not Result then
      exit;
    ACompUnit := FCompUnit;
    {$PUSH}{$R-}
    AValue := ACompUnit.FirstScope.Entry - ACompUnit.HeaderSize + Offs;
    {$POP}
    if (AValue < ACompUnit.FInfoData) or (AValue >= ACompUnit.FInfoData + ACompUnit.FLength) then begin
      DebugLn(FPDBG_DWARF_ERRORS, 'Error: Reference to invalid location. Offset %d is outsize the CU of size %d', [Offs, ACompUnit.FLength]);
      AValue := nil;
      Result := False;
    end;
  end
  else
  if (Form = DW_FORM_ref_addr) then begin
    if FCompUnit.Version=2 then
      Result := FCompUnit.ReadAddressValue(InfoData, Form, Offs)
    else
      Result := FCompUnit.ReadValue(InfoData, Form, Offs);
    if not Result then
      exit;
    AValue := FCompUnit.DebugFile^.Sections[dsInfo].RawData + Offs;
    if (AValue >= FCompUnit.FInfoData) and (AValue < FCompUnit.FInfoData + FCompUnit.FLength) then
      ACompUnit := FCompUnit
    else
      ACompUnit := FCompUnit.FOwner.FindCompilationUnitByOffs(Offs);
    Result := ACompUnit <> nil;
    DebugLn(FPDBG_DWARF_WARNINGS and (not Result), ['Comp unit not found DW_FORM_ref_addr']);
  end
  else begin
    DebugLn(FPDBG_DWARF_VERBOSE, ['FORM for ReadReference not expected ', DwarfAttributeFormToString(Form)]);
  end;
end;

function TDwarfInformationEntry.ReadValue(AnAttrib: Cardinal; out AValue: Integer): Boolean;
var
  AttrData: TDwarfAttribData;
begin
  Result := GetAttribData(AnAttrib, AttrData);
  if Result then
    Result := ReadValue(AttrData, AValue);
end;

function TDwarfInformationEntry.ReadValue(AnAttrib: Cardinal; out AValue: Int64): Boolean;
var
  AttrData: TDwarfAttribData;
begin
  Result := GetAttribData(AnAttrib, AttrData);
  if Result then
    Result := ReadValue(AttrData, AValue);
end;

function TDwarfInformationEntry.ReadValue(AnAttrib: Cardinal; out AValue: Cardinal): Boolean;
var
  AttrData: TDwarfAttribData;
begin
  Result := GetAttribData(AnAttrib, AttrData);
  if Result then
    Result := ReadValue(AttrData, AValue);
end;

function TDwarfInformationEntry.ReadValue(AnAttrib: Cardinal; out AValue: QWord): Boolean;
var
  AttrData: TDwarfAttribData;
begin
  Result := GetAttribData(AnAttrib, AttrData);
  if Result then
    Result := ReadValue(AttrData, AValue);
end;

function TDwarfInformationEntry.ReadValue(AnAttrib: Cardinal; out AValue: PChar): Boolean;
var
  AttrData: TDwarfAttribData;
begin
  Result := GetAttribData(AnAttrib, AttrData);
  if Result then
    Result := ReadValue(AttrData, AValue);
end;

function TDwarfInformationEntry.ReadValue(AnAttrib: Cardinal; out AValue: String): Boolean;
var
  AttrData: TDwarfAttribData;
begin
  Result := GetAttribData(AnAttrib, AttrData);
  if Result then
    Result := ReadValue(AttrData, AValue);
end;

function TDwarfInformationEntry.ReadValue(AnAttrib: Cardinal; out
  AValue: TByteDynArray; AnFormString: Boolean): Boolean;
var
  AttrData: TDwarfAttribData;
begin
  Result := GetAttribData(AnAttrib, AttrData);
  if Result then
    Result := ReadValue(AttrData, AValue, AnFormString);
end;

function TDwarfInformationEntry.ReadReference(AnAttrib: Cardinal; out AValue: Pointer; out
  ACompUnit: TDwarfCompilationUnit): Boolean;
var
  AttrData: TDwarfAttribData;
begin
  Result := GetAttribData(AnAttrib, AttrData);
  if Result then
    Result := AttrData.InformationEntry.DoReadReference(
      AttrData.Idx, AttrData.InfoPointer,
      AValue, ACompUnit
    );
end;

function TDwarfInformationEntry.ReadName(out AName: String): Boolean;
var
  AttrData: TDwarfAttribData;
begin
  PrepareAbbrev;
  if FAbbrev = nil then
    exit(False);
  if dafHasName in FAbbrev^.flags then begin
    Result := GetAttribData(DW_AT_name, AttrData);
    assert(Result and (AttrData.InformationEntry = Self), 'TDwarfInformationEntry.ReadName');
    Result := ReadValue(AttrData, AName);
  end
  else
  if (dafHasAbstractOrigin in FAbbrev^.flags) and PrepareAbstractOrigin then
    Result := FAbstractOrigin.ReadName(AName)
  else
    Result := False;
end;

function TDwarfInformationEntry.ReadName(out AName: PChar): Boolean;
var
  AttrData: TDwarfAttribData;
begin
  PrepareAbbrev;
  if FAbbrev = nil then
    exit(False);
  if dafHasName in FAbbrev^.flags then begin
    Result := GetAttribData(DW_AT_name, AttrData);
    assert(Result and (AttrData.InformationEntry = Self), 'TDwarfInformationEntry.ReadName');
    Result := ReadValue(AttrData, AName);
  end
  else
  if (dafHasAbstractOrigin in FAbbrev^.flags) and PrepareAbstractOrigin then
    Result := FAbstractOrigin.ReadName(AName)
  else
    Result := False;
end;

function TDwarfInformationEntry.ReadStartScope(out AStartScope: TDbgPtr): Boolean;
var
  AttrData: TDwarfAttribData;
begin
  PrepareAbbrev;
  if FAbbrev = nil then
    exit(False);
  if dafHasStartScope in FAbbrev^.flags then begin
    Result := GetAttribData(DW_AT_start_scope, AttrData);
    assert(Result and (AttrData.InformationEntry = Self), 'TDwarfInformationEntry.ReadName');
    Result := ReadValue(AttrData, AStartScope);
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
  Result := dafHasArtifical in FAbbrev^.flags;
  if Result then Result := ReadValue(DW_AT_artificial, Val);
  if Result then Result := Val <> 0;
end;

function TDwarfInformationEntry.IsEqual(AnOther: TDwarfInformationEntry): Boolean;
begin
  Result := (FCompUnit = AnOther.FCompUnit) and (
              ( (FInformationData <> nil) and (FInformationData = AnOther.FInformationData) ) or
              ( FScope.IsEqual(AnOther.FScope) )
            );
end;

{ TDWarfLineMap }

procedure TDWarfLineMap.Init;
begin
  FNextMap := nil;
end;

procedure TDWarfLineMap.SetAddressForLine(ALine: Cardinal; AnAddress: TDBGPtr);
var
  SectLen, SectCnt, i, j, o, o2: Integer;
  idx, offset: TDBGPtr;
  LineOffsets: Array of Byte;
  Addresses: Array of TDBGPtr;
begin
  idx := ALine div 256;
  offset := ALine mod 256;
  i := Length(FLineIndexList);
  if idx >= i then
    SetLength(FLineIndexList, idx+4);

  LineOffsets := FLineIndexList[idx].LineOffsets;
  Addresses := FLineIndexList[idx].Addresses;

  if Addresses = nil then begin
    SectLen := 192;
    SectCnt := 0;
    SetLength(FLineIndexList[idx].Addresses, 193);
    SetLength(FLineIndexList[idx].LineOffsets, 192);
    LineOffsets := FLineIndexList[idx].LineOffsets;
    Addresses := FLineIndexList[idx].Addresses;
  end
  else begin
    SectLen := Length(LineOffsets);
    SectCnt := Integer(Addresses[SectLen]);
    if SectCnt >= SectLen then begin
      SectLen := SectCnt + 64;
      SetLength(FLineIndexList[idx].Addresses, SectLen+1);
      SetLength(FLineIndexList[idx].LineOffsets, SectLen);
      LineOffsets := FLineIndexList[idx].LineOffsets;
      Addresses := FLineIndexList[idx].Addresses;
    end;
  end;


  i := 0;
  o := 0;
  while (i < SectCnt) do begin
    o2 := o + LineOffsets[i];
    if o2 > offset then break;
    o := o2;
    inc(i);
  end;

  j := SectCnt;
  while j > i do begin
    LineOffsets[j] := LineOffsets[j-1];
    Addresses[j]   := Addresses[j-1];
    dec(j);
  end;

  offset := offset - o;
  LineOffsets[i] := offset;
  Addresses[i]   := AnAddress;

  if i < SectCnt then begin
    assert(LineOffsets[i+1] >= offset, 'TDWarfLineMap.SetAddressForLine LineOffsets[i+1] > offset');
    LineOffsets[i+1] := LineOffsets[i+1] - offset;
  end;

  Addresses[SectLen] := SectCnt + 1;
end;

function TDWarfLineMap.GetAddressesForLine(ALine: Cardinal; var AResultList: TDBGPtrArray;
  NoData: Boolean; AFindSibling: TGetLineAddrFindSibling; AFoundLine: PInteger;
  AMaxSiblingDistance: integer; ADbgInfo: TFpDwarfInfo): Boolean;
var
  idx: integer;
  offset, Addr1, Addr2: TDBGPtr;
  LineOffsets: Array of Byte;
  Addresses: Array of TDBGPtr;
  o: Byte;
  i, j, k, l, ln, CurOffs: Integer;
  TmpResList: TDBGPtrArray;
begin
  Result := False;
  try
    idx := ALine div 256;
    offset := ALine mod 256;
    if idx >= Length(FLineIndexList) then begin
      if AFindSibling  = fsBefore then begin
        idx := Length(FLineIndexList)-1;
        offset := 255;
      end
      else
        exit;
    end;

    repeat
      LineOffsets := FLineIndexList[idx].LineOffsets;
      Addresses := FLineIndexList[idx].Addresses;
      if Addresses = nil then
        case AFindSibling of
          fsNone:
              exit;
          fsBefore: begin
              if idx = 0 then
                exit;
              dec(idx);
              offset := 255;
              Continue;
            end;
          fsNext, fsNextFunc, fsNextFuncLazy: begin
              inc(idx);
              if idx >= Length(FLineIndexList) then
                exit;
              offset := 0;
              Continue;
            end;
        end;

      l := Length(LineOffsets);
      i := 0;
      CurOffs := 0;
      while (i < l) do begin
        o := LineOffsets[i];
        CurOffs := CurOffs + o;
        if o > offset then begin
          case AFindSibling of
            fsNone:
                exit;
            fsBefore: begin
                if i > 0 then begin
                  dec(i);
                  CurOffs := CurOffs - o;
                  offset := 0;  // found line before
                end
                else begin
                  // i=0 => will trigger continue for outer loop
                  dec(idx);
                  if idx < 0 then
                    exit;
                  offset := 255; // Must be last entry from block before (if there is a block before)
                end;
              end;
            fsNext, fsNextFunc, fsNextFuncLazy: begin
                offset := 0;  // found line after/next
              end;
          end;
          break;
        end;
        offset := offset - o;
        if offset = 0 then
          break;
        inc(i);
      end;

      if offset = 0 then
        break;
      case AFindSibling of
        fsNone: exit;
        fsBefore: begin
          if i = 0 then
            continue;
          assert(i=l, 'TDWarfLineMap.GetAddressesForLine: i=l');
          dec(i);
          break;
        end;
        else begin
          inc(idx);
          if idx >= Length(FLineIndexList) then
            exit;
          continue;
        end;
      end;
    until False;

    ln := 256 * idx + CurOffs;
    if ln <> ALine then
      case AFindSibling of
        fsBefore:
          if (AMaxSiblingDistance > 0) and (ALine - ln > AMaxSiblingDistance) then exit;
        fsNext, fsNextFunc, fsNextFuncLazy: begin
          if (AMaxSiblingDistance > 0) and (ln - ALine > AMaxSiblingDistance) then exit;
          if (AFindSibling = fsNextFunc) or
             ((AFindSibling = fsNextFuncLazy) and (ln - ALine > 1))
          then begin
            // check same function
            if ADbgInfo = nil then exit;
            if not ADbgInfo.FindProcStartEndPC(Addresses[i], Addr1, Addr2) then exit;
            if GetAddressesForLine(ALine, TmpResList, False, fsBefore) then begin
              if (Length(TmpResList) = 0) or (TmpResList[0] < Addr1) or (TmpResList[0] > Addr2) then
                exit;
            end;
          end;

          if (AFoundLine <> nil) and (AFoundLine^ <> -1) then begin
            if (ln > AFoundLine^) then exit; // already have better match
            if (ln < AFoundLine^) then
              AResultList := nil;  // found better match
          end;
        end;
      end;
    if AFoundLine <> nil then
      AFoundLine^ := 256 * idx + CurOffs;

    if NoData then begin
      Result := True;
      exit;
    end;

    j := i + 1;
    while (j < l) and (LineOffsets[j] = 0) do inc(j);

    k := Length(AResultList);
    SetLength(AResultList, k + (j-i));
    while i < j do begin
      AResultList[k] := Addresses[i];
      inc(i);
      inc(k);
    end;

    Result := True;
  finally
    if (FNextMap <> nil) and (FNextMap <> pointer(1)) and
       FNextMap^.GetAddressesForLine(ALine, AResultList, NoData, AFindSibling, AFoundLine,
          AMaxSiblingDistance, ADbgInfo)
    then
      Result := True;
  end;
end;

procedure TDWarfLineMap.Compress;
var
  i, j: Integer;
begin
  for i := 0 to high(FLineIndexList) do begin
    j := Length(FLineIndexList[i].LineOffsets);
    if j <> 0 then begin
      j := FLineIndexList[i].Addresses[j];
      SetLength(FLineIndexList[i].Addresses, j+1);
      FLineIndexList[i].Addresses[j] := j;
      SetLength(FLineIndexList[i].LineOffsets, j);
    end;
  end;
end;

{ TFpDwarfInfo }

constructor TFpDwarfInfo.Create(ALoaderList: TDbgImageLoaderList;
  AMemManager: TFpDbgMemManager; AMemModel: TFpDbgMemModel);
var
  Section: TDwarfSection;
  p: PDbgImageSection;
  i: Integer;
begin
  FLineNumberMap := TLineNumberFileMap.Create;
  FWorkQueue := FpDbgGlobalWorkerQueue;
  FWorkQueue.AddRef;

  inherited Create(ALoaderList, AMemManager, AMemModel);
  FTargetInfo := ALoaderList.TargetInfo;
  FCompilationUnits := TList.Create;
  FCallFrameInformationList := TObjectList.Create(True);
  FImageBase := ALoaderList.ImageBase;
  FRelocationOffset := ALoaderList.RelocationOffset;

  SetLength(FFiles, ALoaderList.Count);
  for i := 0 to ALoaderList.Count-1 do
  begin
    FFiles[i].AddressMapList:=ALoaderList[i].AddressMapList;
    for Section := Low(Section) to High(Section) do
    begin
      p := ALoaderList[i].Section[DWARF_SECTION_NAME[Section]];
      if p = nil then Continue;
      FFiles[i].Sections[Section].Section := Section;
      FFiles[i].Sections[Section].RawData := p^.RawData;
      FFiles[i].Sections[Section].Size := p^.Size;
      FFiles[i].Sections[Section].VirtualAddress := p^.VirtualAddress;
    end;
    ALoaderList[i].CloseFileLoader;
  end;
end;

destructor TFpDwarfInfo.Destroy;
  procedure FreeLineNumberMap;
  var
    n: Integer;
  begin
    if FLineNumberMap = nil then
      exit;
    for n := 0 to FLineNumberMap.Count - 1 do
      Dispose(FLineNumberMap.Data[n]);
    FreeAndNil(FLineNumberMap);
  end;

var
  n: integer;
begin
  FWorkQueue.DecRef;
  for n := 0 to FCompilationUnits.Count - 1 do
    TObject(FCompilationUnits[n]).Free;
  FreeAndNil(FCompilationUnits);
  FreeAndNil(FCallFrameInformationList);
  FreeLineNumberMap;

  inherited Destroy;
end;

function TFpDwarfInfo.FindSymbolScope(ALocationContext: TFpDbgSimpleLocationContext;
  AAddress: TDbgPtr): TFpDbgSymbolScope;
var
  Proc, UnitSym: TDbgDwarfSymbolBase;
begin
  Result := nil;
  Proc := FindDwarfProcSymbol(AAddress);  // TFpSymbolDwarfDataProc
  if Proc <> nil then begin
    Result := Proc.CreateSymbolScope(ALocationContext, Self);
    Proc.ReleaseReference;
    exit;
  end;

  UnitSym := FindDwarfUnitSymbol(AAddress);
  if UnitSym <> nil then begin
    Result := UnitSym.CreateSymbolScope(ALocationContext, Self);
    UnitSym.ReleaseReference;
    exit;
  end;

  if CompilationUnitsCount > 0 then
    Result := CompilationUnits[0].DwarfSymbolClassMap.CreateScopeForSymbol
      (ALocationContext, nil, Self);
end;

function TFpDwarfInfo.GetCompilationUnit(AIndex: Integer): TDwarfCompilationUnit;
begin
  Result := TDwarfCompilationUnit(FCompilationUnits[Aindex]);
  Result.WaitForScopeScan;
end;

function TFpDwarfInfo.GetCompilationUnitClass: TDwarfCompilationUnitClass;
begin
  Result := TDwarfCompilationUnit;
end;

function TFpDwarfInfo.FindCompilationUnitByOffs(AOffs: QWord): TDwarfCompilationUnit;
var
  l, h, m: Integer;
  p: Pointer;
begin
  Result := nil;
  l := 0;
  h := FCompilationUnits.Count - 1;
  m := h;
  while h > l do begin
    p := TDwarfCompilationUnit(FCompilationUnits[m]).DebugFile^.Sections[dsInfo].RawData + AOffs;
    m := (h + l + 1) div 2;
    if TDwarfCompilationUnit(FCompilationUnits[m]).FInfoData <= p
    then l := m
    else h := m - 1;
  end;

  Result := TDwarfCompilationUnit(FCompilationUnits[h]);
  if (p < Result.FInfoData) or (p > Result.FInfoData + Result.FLength) then
    Result := nil
  else
    Result.WaitForScopeScan;
end;

function TFpDwarfInfo.FindDwarfProcSymbol(AAddress: TDbgPtr
  ): TDbgDwarfSymbolBase;
begin
  Result := TDbgDwarfSymbolBase(FindProcSymbol(AAddress));
end;

function TFpDwarfInfo.FindProcSymbol(AAddress: TDbgPtr): TFpSymbol;
var
  n: Integer;
  CU: TDwarfCompilationUnit;
  Iter: TLockedMapIterator;
  Info: PDwarfAddressInfo;
begin
  Result := nil;
  for n := 0 to FCompilationUnits.Count - 1 do
  begin
    CU := TDwarfCompilationUnit(FCompilationUnits[n]);
    if not CU.HasAddress(AAddress, [wrAddrMap]) then
      Continue;

    Iter := TLockedMapIterator.Create(CU.FAddressMap);
    try
      if not Iter.Locate(AAddress)
      then begin
        if not Iter.BOM
        then Iter.Previous;

        if Iter.BOM
        then Continue;
      end;

      // iter is at the closest defined address before AAddress
      Info := Iter.DataPtr;
      if AAddress > Info^.EndPC
      then Continue;

      // TDbgDwarfProcSymbol
      Result := Cu.DwarfSymbolClassMap.CreateProcSymbol(CU, Iter.DataPtr, AAddress, Self);
      if Result<>nil then
        break;
    finally
      Iter.Free;
    end;
  end;
end;

function TFpDwarfInfo.FindProcStartEndPC(const AAddress: TDbgPtr; out AStartPC,
  AEndPC: TDBGPtr): boolean;
var
  n: Integer;
  CU: TDwarfCompilationUnit;
begin
  Result := False;
  for n := 0 to FCompilationUnits.Count - 1 do
  begin
    CU := TDwarfCompilationUnit(FCompilationUnits[n]);
    if not CU.HasAddress(AAddress, [wrAddrMap]) then
      Continue;

    Result := CU.GetProcStartEnd(AAddress, AStartPC, AEndPC);
    if Result then exit;
  end;
end;

function TFpDwarfInfo.FindLineInfo(AAddress: TDbgPtr): TFpSymbol;
var
  n: Integer;
  CU: TDwarfCompilationUnit;
  Iter: TLockedMapIterator;
  Info: PDwarfAddressInfo;
  SM: TDwarfLineInfoStateMachine;
begin
  Result := nil;
  for n := 0 to FCompilationUnits.Count - 1 do
  begin
    CU := TDwarfCompilationUnit(FCompilationUnits[n]);
    if not CU.HasAddress(AAddress, [wrAddrMap]) then
      Continue;

    Iter := TLockedMapIterator.Create(CU.FAddressMap);
    try
      if not Iter.Locate(AAddress)
      then begin
        if not Iter.BOM
        then Iter.Previous;
      end;

      SM := nil;
      if not Iter.BOM then begin
        // iter is at the closest defined address before AAddress
        Info := Iter.DataPtr;

        if AAddress <= Info^.EndPC then begin
          // TDbgDwarfProcSymbol
          Result := Cu.DwarfSymbolClassMap.CreateProcSymbol(CU, Iter.DataPtr, AAddress, Self);
          if Result<>nil then
            break;
        end;

        CU.BuildLineInfo(Info, False);
        SM := Info^.StateMachine;
      end
      else
      if (CU.FMinPC <> CU.FMaxPC) and (CU.FMinPC<>0) and (CU.FMaxPC <> 0) and
         (AAddress >= CU.FMinPC) and (AAddress < CU.FMaxPC) and
         (CU.FInitialStateMachine <> nil)
      then begin
        Iter.First;
        if not Iter.BOM then begin
          Info := Iter.DataPtr;
          if (AAddress >= Info^.StartPC) then
            continue;
        end;
        if (AAddress >= CU.FInitialStateMachine.Address)
        then
          SM := CU.FInitialStateMachine;
      end;

      if SM <> nil then begin
        SM := SM.Clone;
        Result := TFpSymbolDwarfDataLineInfo.Create(AAddress, SM, CU);
        if not (sfHasLine in Result.Flags) then
          ReleaseRefAndNil(Result)
        else
          Break;
      end;
    finally
      Iter.Free;
    end;
  end;
end;

function TFpDwarfInfo.FindCallFrameInfo(AnAddress: TDBGPtr; out CIE: TDwarfCIE; out Row: TDwarfCallFrameInformationRow): Boolean;
var
  n: Integer;
  CFI: TDwarfCallFrameInformation;
begin
  Result := False;
  for n := 0 to FCallFrameInformationList.Count - 1 do
  begin
    CFI := TDwarfCallFrameInformation(FCallFrameInformationList[n]);

    Result := CFI.GetRow(FTargetInfo, AnAddress, CIE, Row);
    if Result then
      Break;
  end;
end;

function TFpDwarfInfo.FindDwarfUnitSymbol(AAddress: TDbgPtr
  ): TDbgDwarfSymbolBase;
var
  n: Integer;
  CU: TDwarfCompilationUnit;
  InfoEntry: TDwarfInformationEntry;
begin
  Result := nil;
  for n := 0 to FCompilationUnits.Count - 1 do
  begin
    CU := TDwarfCompilationUnit(FCompilationUnits[n]);
    if not CU.HasAddress(AAddress, [wrScan]) then
      continue;

    InfoEntry := TDwarfInformationEntry.Create(CU, CU.FCompUnitScope);
    Result := Cu.DwarfSymbolClassMap.CreateUnitSymbol(CU, InfoEntry, Self);
    InfoEntry.ReleaseReference;
    break;
  end;
end;

function TFpDwarfInfo.GetLineAddresses(const AFileName: String; ALine: Cardinal;
  var AResultList: TDBGPtrArray; AFindSibling: TGetLineAddrFindSibling; AFoundLine: PInteger;
  AFoundFilename: PBoolean; AMaxSiblingDistance: integer): Boolean;
var
  Map: PDWarfLineMap;
begin
  Result := False;
  if AFoundLine <> nil then
    AFoundLine^ := -1;

  Map := GetLineAddressMap(AFileName);
  if Map = nil then
    exit;

  if AFoundFilename <> nil then
    AFoundFilename^ := True;
  Result := Map^.GetAddressesForLine(ALine, AResultList, False, AFindSibling, AFoundLine, AMaxSiblingDistance, Self);
end;

function TFpDwarfInfo.GetLineAddressMap(const AFileName: String): PDWarfLineMap;


  function FindIndex: Integer;
  var
    Name: String;
  begin
    Name := ExtractFileName(AFileName);
    if Name <> AFileName then begin
      Result := FLineNumberMap.IndexOf(Name);
      if Result <> -1 then Exit;
    end;

    for Result := 0 to FLineNumberMap.Count - 1 do
      if AnsiCompareText(Name, ExtractFileName(FLineNumberMap.Keys[Result])) = 0 then
        Exit;
    Result := -1;
  end;

var
  n, idx: Integer;
  CU: TDwarfCompilationUnit;
  Name: String;
begin
  Result := nil;
  if FLineNumberMap = nil then Exit;

  if not FLineNumberMapDone then begin
    // make sure all filenames are there
    for n := 0 to FCompilationUnits.Count - 1 do
    begin
      CU := TDwarfCompilationUnit(FCompilationUnits[n]);
      CU.WaitForScopeScan;
      if CU.Valid then
        CU.BuildLineInfo(nil, True);
    end;
    for n := 0 to FLineNumberMap.Count - 1 do
      FLineNumberMap.Data[n]^.Compress;
    FLineNumberMapDone := True;
  end;

  idx := FLineNumberMap.IndexOf(AFileName);
  if idx >= 0 then begin
    Result := FLineNumberMap.Data[idx];
    if Result^.FNextMap <> nil then
      exit;
    Result^.FNextMap := pointer(1);
    Name := ExtractFileName(AFileName);
    if Name = AFileName then
      exit;
    idx := FLineNumberMap.IndexOf(Name);
    if idx >= 0 then begin
      Result^.FNextMap := FLineNumberMap.Data[idx];
      Result^.FNextMap^.FNextMap := pointer(1);
    end
    else
      Result^.FNextMap := pointer(1);
    exit;
  end;

  idx := FindIndex;
  if idx = -1 then Exit;

  Result := FLineNumberMap.Data[idx];
end;

procedure TFpDwarfInfo.LoadCallFrameInstructions;
var
  i: Integer;
var
  inf: TDwarfSectionInfo;

  function LoadCiE(Version: Byte; Augmentation: PChar; SizeLeft: QWord): TDwarfCIE;
  var
    p: Pointer;
    Instructions: TDwarfCallFrameInformationInstructions;
  begin
    if Version > 4 then
      DebugLn(FPDBG_DWARF_WARNINGS, ['Unsupported DWARF CFI version (' +IntToStr(Version)+ '). Only versions 1-4 are supported.']);

    Result := TDwarfCIE.Create(Version, String(Augmentation));
    p := Augmentation;
    Inc(p, Length(Result.Augmentation)+1);
    if Version > 3 then
      begin
      Result.AddressSize := PByte(p)^;
      Inc(p);
      Result.SegmentSize := PByte(p)^;
      Inc(p);
      end
    else
      begin
      if TargetInfo.machineType = mtAVR8 then
        Result.AddressSize := 2
      else
        case TargetInfo.bitness of
          b32: Result.AddressSize := 4;
          b64: Result.AddressSize := 8;
        end;
      end;
    Result.CodeAlignmentFactor := ULEB128toOrdinal(p);
    Result.DataAlignmentFactor := SLEB128toOrdinal(p);
    if Version < 3 then
      begin
      Result.ReturnAddressRegister := PByte(p)^;
      Inc(p);
      end
    else
      Result.ReturnAddressRegister := ULEB128toOrdinal(p);
    // Calculate how many bytes are left. (DwarfDump calls this the 'bytes of
    // initial instructions')
    Dec(SizeLeft, p-Augmentation);
    SetLength(Instructions, SizeLeft);
    if SizeLeft > 0 then
      begin
      Move(p^, Instructions[0], SizeLeft);
      Result.InitialInstructions := Instructions;
      end;
  end;

  function LoadFDE(CFI: TDwarfCallFrameInformation; CIEPointer: QWord; InitialLocationAddr: pointer; SizeLeft: QWord): TDwarfFDE;
  var
    Instr: TDwarfCallFrameInformationInstructions;
    CIE: TDwarfCIE;
    InitialLocation: TDBGPtr;
    AddressRange, SegmentSelector: QWord;
    p: pointer;
  begin
    p := InitialLocationAddr;
    CIE := CFI.FindCIEForOffset(CIEPointer);
    SegmentSelector := 0;
    if Assigned(CIE) then
      begin
      if CIE.SegmentSize > 0 then
        SegmentSelector := ReadUnsignedFromExpression(p, CIE.SegmentSize);
      InitialLocation := ReadUnsignedFromExpression(p, CIE.AddressSize);
      if InitialLocation > 0 then
        begin
        AddressRange := ReadUnsignedFromExpression(p, CIE.AddressSize);

        Result := TDwarfFDE.Create(CIEPointer, InitialLocation, SegmentSelector, AddressRange);

        SetLength(Instr, InitialLocationAddr + SizeLeft - p);
        if Length(Instr) > 0 then
          Move(p^, Instr[0], InitialLocationAddr + SizeLeft - p);
        Result.Instructions := Instr;
        end
      else
        begin
        DebugLn(FPDBG_DWARF_WARNINGS, ['Read FDE but it''s initial location is 0. Skipped.']);
        Result := nil;
        end;
      end
    else
      begin
      DebugLn(FPDBG_DWARF_WARNINGS, ['Read FDE but could not locate the corresponding CIE ['+HexStr(CIEPointer, 8)+'].']);
      Result := nil;
      end;
  end;

var
  p, pe: Pointer;
  CIE32: PDwarfCIEEntryHeader32 absolute p;
  CIE64: PDwarfCIEEntryHeader64 absolute p;

  FDE32: PDwarfFDEEntryHeader32 absolute p;
  FDE64: PDwarfFDEEntryHeader64 absolute p;

  CIE: TDwarfCIE;
  FDE: TDwarfFDE;
  Length: QWord;
  CFI: TDwarfCallFrameInformation;

begin
  for i := 0 to high(FFiles) do
    begin
    inf := FFiles[i].Sections[dsFrame];
    if inf.Size = 0 then
      continue;

    CFI := TDwarfCallFrameInformation.Create;
    FCallFrameInformationList.Add(CFI);
    p := inf.RawData;
    pe := inf.RawData + inf.Size;
    while (p <> nil) and (p <= pe - SizeOf(TDwarfCIEEntryHeader32.Length)) do
      begin
      // The first fields in the CIE and FDE structures are the same.
      // First check if it is a 64-bit format. Then
      // detect whether it is a CIE or FDE.
      if CIE64^.Signature = DWARF_HEADER64_SIGNATURE then
        begin
        if (p + SizeOf(TDwarfCIEEntryHeader64.Signature) + SizeOf(TDwarfCIEEntryHeader64.Length) >= pe) or
           (CIE64^.Length > pe - (p + SizeOf(TDwarfCIEEntryHeader64.Signature) + SizeOf(TDwarfCIEEntryHeader64.Length))) or
           (CIE64^.Length <= SizeOf(TDwarfCIEEntryHeader64.CIEId))
        then begin
          debugln(FPDBG_DWARF_ERRORS, 'CFI data exceeds section length');
          break; // exceeding the available data
        end;

        if CIE64^.CIEId = QWord($ffffffffffffffff) then
          begin
          // It is a CIE
          {$PUSH} {$T-}  // Would require 2 Pointer() typecasts.
          CIE := LoadCiE(CIE64^.Version, @CIE64^.Augmentation[0],
                         @CIE64^.CIEId+CIE64^.Length-@CIE64^.Augmentation[0]);
          {$POP}
          CFI.AddCIE(p-inf.RawData, CIE);
          end
        else
          begin
          // It is a FDE
          {$PUSH} {$T-}
          FDE := LoadFDE(CFI, FDE64^.CIEPointer, @FDE64^.InitialLocation,
                         @FDE64^.CIEPointer+FDE64^.Length-@FDE64^.InitialLocation);
          {$POP}
          if Assigned(FDE) then
            CFI.AddFDE(FDE);
          end;
        Length := CIE64^.Length;
        p := @CIE64^.CIEId;
        Inc(p, Length);
        end
      else
      if CIE32^.Length = 0 then
        begin
        p := @CIE32^.CIEId;
        end
      else
        begin
        if (p + SizeOf(TDwarfCIEEntryHeader32.Length) >= pe) or
           (CIE32^.Length > pe - (p + SizeOf(TDwarfCIEEntryHeader32.Length))) or
           (CIE32^.Length <= SizeOf(TDwarfCIEEntryHeader32.CIEId)) or
           (CIE32^.Length >= $fffffff0) // resered values / cannot handle
        then begin
          debugln(FPDBG_DWARF_ERRORS, 'CFI data exceeds section length');
          break; // exceeding the available data
        end;

        if CIE32^.CIEId = $ffffffff then
          begin
          // It is a CIE
          {$PUSH} {$T-}
          CIE := LoadCiE(CIE32^.Version, @CIE32^.Augmentation[0],
                         @CIE32^.CIEId+CIE32^.Length-@CIE32^.Augmentation[0]);
          {$POP}
          CFI.AddCIE(p-inf.RawData, CIE);
          end
        else
          begin
          // It is a FDE
          if FDE32^.Length > 0 then
            begin
            FDE := LoadFDE(CFI, FDE32^.CIEPointer, @FDE32^.InitialLocation,
                           @FDE32^.CIEPointer+FDE32^.Length-@FDE32^.InitialLocation);
            if Assigned(FDE) then
              CFI.AddFDE(FDE);
            end
          else
            // This should never happen, but it did and it leads to a range-check
            // error. (Probably a fpc-bug though)
            DebugLn(FPDBG_DWARF_WARNINGS, ['Read FDE with length 0. FDE is skipped.']);
          end;
        Length := CIE32^.Length;
        p := @CIE32^.CIEId;
        Inc(p, Length);
        end;
      end;
    end;
end;

function TFpDwarfInfo.LoadCompilationUnits: Integer;
var
  p, pe: Pointer;
  CU32: PDwarfCUHeader32 absolute p;
  CU64: PDwarfCUHeader64 absolute p;
  CU32v5: PDwarfCUHeader32v5 absolute p;
  CU64v5: PDwarfCUHeader64v5 absolute p;
  CU: TDwarfCompilationUnit;
  CUClass: TDwarfCompilationUnitClass;
  inf: TDwarfSectionInfo;
  i: integer;
  DataOffs, DataLen: QWord;
  AbbrevOffset: QWord;
  AddressSize: Byte;
begin
  CUClass := GetCompilationUnitClass;
  for i := 0 to high(FFiles) do
  begin
    inf := FFiles[i].Sections[dsInfo];
    p := inf.RawData;
    pe := inf.RawData + inf.Size;
    while (p <> nil) and (p < pe) do
    begin
      if CU64^.Signature = DWARF_HEADER64_SIGNATURE
      then begin
        if CU64^.Version < 3 then
          DebugLn(FPDBG_DWARF_WARNINGS, ['Unexpected 64 bit signature found for DWARF version 2']); // or version 1...
        if CU32^.Version<5 then begin
          DataOffs := PtrUInt(CU64 + 1) - PtrUInt(inf.RawData);
          DataLen := CU64^.Length - SizeOf(CU64^) + SizeOf(CU64^.Signature) + SizeOf(CU64^.Length);
          AbbrevOffset := CU32v5^.AbbrevOffset;
          AddressSize := CU32v5^.AddressSize;
        end
        else begin
          DataOffs := PtrUInt(CU64v5 + 1) - PtrUInt(inf.RawData);
          DataLen := CU64v5^.Length - SizeOf(CU64v5^) + SizeOf(CU64v5^.Signature) + SizeOf(CU64v5^.Length);
          AbbrevOffset := CU32v5^.AbbrevOffset;
          AddressSize := CU32v5^.AddressSize;

          if CU64v5^.unit_type <> $01 then begin
            DebugLn(FPDBG_DWARF_WARNINGS, 'Found Dwarf-5 partial compilation unit ot offset %d, which is not supported. Compilation unit is skipped.', [DataOffs]);
            break; // Do not process invalid data
          end;
        end;

        if DataOffs + DataLen > inf.Size then begin
          DebugLn(FPDBG_DWARF_ERRORS, 'Error: Invalid size for compilation unit at offest %d with size %d exceeds section size %d', [DataOffs, DataLen, inf.Size]);
          break; // Do not process invalid data
        end;

        CU := CUClass.Create(
                Self,
                @FFiles[i],
                DataOffs,
                DataLen,
                CU64^.Version,
                AbbrevOffset,
                AddressSize,
                True);
        p := Pointer(@CU64^.Version) + CU64^.Length;
      end
      else begin
        if CU32^.Length = 0 then Break;
        if CU32^.Version<5 then begin
          DataOffs := PtrUInt(CU32 + 1) - PtrUInt(inf.RawData);
          DataLen := CU32^.Length - SizeOf(CU32^) + SizeOf(CU32^.Length);
          AbbrevOffset := CU32^.AbbrevOffset;
          AddressSize := CU32^.AddressSize;
        end
        else begin
          DataOffs := PtrUInt(CU32v5 + 1) - PtrUInt(inf.RawData);
          DataLen := CU32v5^.Length - SizeOf(CU32v5^) + SizeOf(CU32v5^.Length);
          AbbrevOffset := CU32v5^.AbbrevOffset;
          AddressSize := CU32v5^.AddressSize;

          if CU32v5^.unit_type <> $01 then begin
            DebugLn(FPDBG_DWARF_WARNINGS, 'Found Dwarf-5 partial compilation unit ot offset %d, which is not supported. Compilation unit is skipped.', [DataOffs]);
            break; // Do not process invalid data
          end;
        end;

        if DataOffs + DataLen > inf.Size then begin
          DebugLn(FPDBG_DWARF_ERRORS, 'Error: Invalid size for compilation unit at offest %d with size %d exceeds section size %d', [DataOffs, DataLen, inf.Size]);
          break; // Do not process invalid data
        end;

        CU := CUClass.Create(
                Self,
                @FFiles[i],
                DataOffs,
                DataLen,
                CU32^.Version,
                AbbrevOffset,
                AddressSize,
                False);
        p := Pointer(@CU32^.Version) + CU32^.Length;
      end;
      FCompilationUnits.Add(CU);
      if CU.Valid then SetHasInfo;
    end;
  end;
  Result := FCompilationUnits.Count;

  for i := 0 to Result - 1 do
    if TDwarfCompilationUnit(FCompilationUnits[i]).FComputeNameHashesWorker <> nil then
      TDwarfCompilationUnit(FCompilationUnits[i]).FComputeNameHashesWorker.MarkReadyToRun;
end;

function TFpDwarfInfo.CompilationUnitForAddr(AnAddr: TDBGPtr
  ): TDwarfCompilationUnit;
var
  n: Integer;
begin
  for n := 0 to FCompilationUnits.Count - 1 do
  begin
    Result := TDwarfCompilationUnit(FCompilationUnits[n]);
    if  Result.HasAddress(AnAddr, []) then
      exit;
  end;
  Result := nil;
end;

function TFpDwarfInfo.CompilationUnitsCount: Integer;
begin
  Result := FCompilationUnits.Count;
end;

{ TDbgDwarfSymbolBase }

procedure TDbgDwarfSymbolBase.Init;
begin
  //
end;

constructor TDbgDwarfSymbolBase.Create(const AName: String);
begin
  inherited Create(AName);
  Init;
end;

constructor TDbgDwarfSymbolBase.Create(const AName: String;
  AnInformationEntry: TDwarfInformationEntry);
begin
  FCU := AnInformationEntry.CompUnit;
  FInformationEntry := AnInformationEntry;
  FInformationEntry.AddReference;

  inherited Create(AName);
  Init;
end;

constructor TDbgDwarfSymbolBase.Create(const AName: String;
  AnInformationEntry: TDwarfInformationEntry; AKind: TDbgSymbolKind;
  const AAddress: TFpDbgMemLocation);
begin
  FCU := AnInformationEntry.CompUnit;
  FInformationEntry := AnInformationEntry;
  FInformationEntry.AddReference;

  inherited Create(AName, AKind, AAddress);
  Init;
end;

destructor TDbgDwarfSymbolBase.Destroy;
begin
  ReleaseRefAndNil(FInformationEntry);
  inherited Destroy;
end;

function TDbgDwarfSymbolBase.CreateSymbolScope(
  ALocationContext: TFpDbgSimpleLocationContext; ADwarfInfo: TFpDwarfInfo
  ): TFpDbgSymbolScope;
begin
  Result := nil;
end;

function TDbgDwarfSymbolBase.IsEqual(AnOther: TFpSymbol): Boolean;
begin
  Result := (AnOther is TDbgDwarfSymbolBase) and
            FInformationEntry.IsEqual(TDbgDwarfSymbolBase(AnOther).FInformationEntry);
end;

{ TFpSymbolDwarfDataLineInfo }

function TFpSymbolDwarfDataLineInfo.GetFlags: TDbgSymbolFlags;
begin
  Result := [];
  if FFound then
    Result := Result + [sfHasLine, sfHasLineAddrRng];
end;

function TFpSymbolDwarfDataLineInfo.GetFile: String;
begin
  Result := FFile;
end;

function TFpSymbolDwarfDataLineInfo.GetLine: Cardinal;
begin
  Result := FLine;
  if Result = 0 then
    Result := FNextLine;
end;

function TFpSymbolDwarfDataLineInfo.GetLineStartAddress: TDBGPtr;
begin
  Result := FLineStartAddress;
end;

function TFpSymbolDwarfDataLineInfo.GetLineEndAddress: TDBGPtr;
begin
  Result := FLineEndAddress;
end;

constructor TFpSymbolDwarfDataLineInfo.Create(AnAddress: TDbgPtr;
  AStateMachine: TDwarfLineInfoStateMachine; ACU: TDwarfCompilationUnit);
var
  SM2: TDwarfLineInfoStateMachine;
  SM2val: Boolean;
begin
  inherited Create('', skNone, TargetLoc(AnAddress));
  FCU := ACU;
  //Init;


  if AStateMachine = nil then
    exit;

  if AnAddress < AStateMachine.Address
  then begin
    AStateMachine.Free;
    Exit;    // The address we want to find is before the start pos ??
  end;

  SM2 := AStateMachine.Clone;

  repeat
    SM2val := SM2.NextLine;
    if (not AStateMachine.EndSequence) and
       ( (AnAddress = AStateMachine.Address) or
         ( (AnAddress > AStateMachine.Address) and
           SM2val and (AnAddress < SM2.Address)
         )
       )
    then begin
      // found
      FFound := True;
      FFile := AStateMachine.FileName;
      FLine := AStateMachine.Line;
      FNextLine := SM2.Line;
      FLineStartAddress := AStateMachine.Address;
      FLineEndAddress := SM2.Address;

      break;
    end;
  until not AStateMachine.NextLine;

  AStateMachine.Free;
  SM2.Free;
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
          {$PUSH}{$R-}{$Q-}
          Inc(FAddress, ULEB128toOrdinal(pbyte(FLineInfoPtr)));
          {$POP}
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
          {$PUSH}{$R-}{$Q-}
          if FOwner.FLineInfo.LineRange = 0
          then Inc(FAddress, Opcode * FOwner.FLineInfo.MinimumInstructionLength)
          else Inc(FAddress, (Opcode div FOwner.FLineInfo.LineRange) * FOwner.FLineInfo.MinimumInstructionLength);
          {$POP}
          // version 4 also op_index register, if architecture has VLIW
        end;
        DW_LNS_fixed_advance_pc: begin
          {$PUSH}{$R-}{$Q-}
          Inc(FAddress, PWord(FLineInfoPtr)^);
          {$POP}
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
              Inc(pbyte(FLineInfoPtr), instrlen);
              Exit;
            end;
            DW_LNE_set_address: begin
              if FOwner.FLineInfo.AddrSize = 8 then
                FAddress := PQWord(pbyte(FLineInfoPtr)+1)^
              else if FOwner.FLineInfo.AddrSize = 4 then
                FAddress:= PLongWord(pbyte(FLineInfoPtr)+1)^
              else
                FAddress := PWord(pbyte(FLineInfoPtr)+1)^;
              FAddress:=FOwner.MapAddressToNewValue(FAddress);
              FAddress:=FOwner.CalculateRelocatedAddress(FAddress);
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
            // Version-4
             DW_LNE_set_discriminator: begin
               // for now just skif the value
               //p := pbyte(FLineInfoPtr);
               //FDiscriminator := ULEB128toOrdinal(pbyte(p));
             end;
          else
            // unknown extendend opcode
          end;
          Inc(pbyte(FLineInfoPtr), instrlen);
        end;
      else
        // unknown opcode
        if Opcode >= Length(FOwner.FLineInfo.StandardOpcodeLengths) then begin
          debugln(FPDBG_DWARF_ERRORS, ['Error, unknown line machine opcode: ', Opcode]);
          exit(False); // can't handle unknow upcode
        end;
        debugln(FPDBG_DWARF_WARNINGS, ['Skipping unknown line machine opcode: ', Opcode]);
        Inc(pbyte(FLineInfoPtr), FOwner.FLineInfo.StandardOpcodeLengths[Opcode])
      end;
      Continue;
    end;

    // Special opcode
    Dec(Opcode, Length(FOwner.FLineInfo.StandardOpcodeLengths)+1);
    {$PUSH}{$R-}{$Q-}
    if FOwner.FLineInfo.LineRange = 0
    then begin
      Inc(FAddress, Opcode * FOwner.FLineInfo.MinimumInstructionLength);
    end
    else begin
      Inc(FAddress, (Opcode div FOwner.FLineInfo.LineRange) * FOwner.FLineInfo.MinimumInstructionLength);
      Inc(FLine, FOwner.FLineInfo.LineBase + (Opcode mod FOwner.FLineInfo.LineRange));
    end;
    {$POP}
    FBasicBlock    := False;
    FPrologueEnd   := False;
    FEpilogueBegin := False;
    //FDiscriminator := False;

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

{ TFpSymbolDwarfClassMapList }

destructor TFpSymbolDwarfClassMapList.Destroy;
begin
  FreeAllInstances;
  inherited Destroy;
end;

function TFpSymbolDwarfClassMapList.FindMapForCompUnit(ACU: TDwarfCompilationUnit): TFpSymbolDwarfClassMap;
var
  i: Integer;
  ResClass: TFpSymbolDwarfClassMapClass;
begin
  ResClass := FDefaultMap;
  for i := 0 to length(FMapList) - 1 do
    if FMapList[i].ClassCanHandleCompUnit(ACU) then begin
      ResClass := FMapList[i];
      break;
    end;
  Result := ResClass.GetInstanceForCompUnit(ACU);
end;

procedure TFpSymbolDwarfClassMapList.FreeAllInstances;
var
  i: Integer;
begin
  for i := 0 to length(FMapList) - 1 do begin
    if FMapList[i] = FDefaultMap then
      FDefaultMap := nil; // Should not happen, default map should not be added to list
    FMapList[i].FreeAllInstances;
  end;
  if FDefaultMap <> nil then
    FDefaultMap.FreeAllInstances;
end;

procedure TFpSymbolDwarfClassMapList.AddMap(AMap: TFpSymbolDwarfClassMapClass);
var
  l: Integer;
begin
  l := length(FMapList);
  SetLength(FMapList, l + 1);
  FMapList[l] := AMap;
end;

procedure TFpSymbolDwarfClassMapList.SetDefaultMap(AMap: TFpSymbolDwarfClassMapClass);
begin
  FDefaultMap := AMap;
end;

{ TFpThreadWorkerScanAll }

procedure TFpThreadWorkerScanAll.DoExecute;
begin
  FScanScopeList.BuildList(FCU.FAbbrevList, FCU.FInfoData, FCU.FLength,
    FCU.FAddressSize, FCU.IsDwarf64, FCU.Version);

  FCompNameHashWorker.SetScopeList(FScanScopeList);
  FCompNameHashWorker.MarkReadyToRun;
end;

constructor TFpThreadWorkerScanAll.Create(CU: TDwarfCompilationUnit;
  ACompNameHashWorker: TFpThreadWorkerComputeNameHashes);
begin
  FCU := CU;
  FCompNameHashWorker := ACompNameHashWorker;
end;

function TFpThreadWorkerScanAll.FindCompileUnit(var AScopeList: TDwarfScopeList
  ): TDwarfScopeInfo;
begin
  Result := FScanScopeList.BuildList(FCU.FAbbrevList, FCU.FInfoData, FCU.FLength,
    FCU.FAddressSize, FCU.IsDwarf64, FCU.Version, DW_TAG_compile_unit);

  AScopeList := FScanScopeList;
  Result.FScopeListPtr := @AScopeList;
end;

procedure TFpThreadWorkerScanAll.UpdateScopeList(var AScopeList: TDwarfScopeList
  );
begin
  AScopeList := FScanScopeList;
end;

{ TFpThreadWorkerComputeNameHashes }

procedure TFpThreadWorkerComputeNameHashes.DoExecute;
var
  Scope: TDwarfScopeInfo;
  InfoEntry: TDwarfInformationEntry;
begin
  Scope.Init(@FScanScopeList);
  Scope.Index := 0;
  InfoEntry := TDwarfInformationEntry.Create(FCU, Scope);
  InfoEntry.ComputeKnownHashes(@FCU.FKnownNameHashes);
  InfoEntry.ReleaseReference;
end;

constructor TFpThreadWorkerComputeNameHashes.Create(CU: TDwarfCompilationUnit);
begin
  FCU := CU;
end;

procedure TFpThreadWorkerComputeNameHashes.SetScopeList(
  const AScanScopeList: TDwarfScopeList);
begin
  FScanScopeList := AScanScopeList;
end;

procedure TFpThreadWorkerComputeNameHashes.MarkReadyToRun;
var
  c: Cardinal;
begin
  c := InterLockedIncrement(FReadyToRun);
  if c = 2 then
    FCU.FOwner.WorkQueue.PushItem(Self);
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

  LineMap:=nil;
  while FLineInfo.StateMachine.NextLine do
  begin
    Line := FLineInfo.StateMachine.Line;

    if (idx < 0) or (CurrentFileName <> FLineInfo.StateMachine.FileName) then begin
      idx := FOwner.FLineNumberMap.IndexOf(FLineInfo.StateMachine.FileName);
      if idx = -1
      then begin
        LineMap := New(PDWarfLineMap);
        LineMap^.Init;
        FOwner.FLineNumberMap.Add(FLineInfo.StateMachine.FileName, LineMap);
      end
      else begin
        LineMap := FOwner.FLineNumberMap.Data[idx];
      end;
      CurrentFileName := FLineInfo.StateMachine.FileName;
    end;

    addr := FLineInfo.StateMachine.Address;
    if (not FLineInfo.StateMachine.EndSequence) and (FLineInfo.StateMachine.IsStmt)
    and (Line > 0)
    and (LineMap<>nil)
    then
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

  if not ADoAll then
    for Idx := 0 to FOwner.FLineNumberMap.Count - 1 do
      FOwner.FLineNumberMap.Data[idx]^.Compress;
end;

function TDwarfCompilationUnit.GetAddressMap: TMap;
begin
  BuildAddressMap;
  Result := FAddressMap;
end;

function TDwarfCompilationUnit.GetKnownNameHashes: PKnownNameHashesArray;
begin
  WaitForComputeHashes;
  Result := @FKnownNameHashes;
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

procedure TDwarfCompilationUnit.DoWaitForScopeScan;
begin
  if FWaitForScopeScanSection.EnterOrWait(CachedRtlEvent) then begin
    if FScanAllWorker <> nil then begin
      FOwner.WorkQueue.WaitForItem(FScanAllWorker);

      FScanAllWorker.UpdateScopeList(FScopeList);
      FFirstScope.Init(@FScopeList);
      FFirstScope.Index := 0;

      FScanAllWorker.DecRef;
      FScanAllWorker := nil;
    end;
    FWaitForScopeScanSection.Leave;
  end;
  assert(FScanAllWorker=nil, 'TDwarfCompilationUnit.DoWaitForScopeScan: FScanAllWorker=nil');
end;

procedure TDwarfCompilationUnit.DoWaitForComputeHashes;
begin
  if FWaitForComputeHashesSection.EnterOrWait(CachedRtlEvent) then begin
    if FComputeNameHashesWorker <> nil then begin
      FOwner.WorkQueue.WaitForItem(FComputeNameHashesWorker);
      FComputeNameHashesWorker.DecRef;
      FComputeNameHashesWorker := nil;
    end;
    FWaitForComputeHashesSection.Leave;
  end;
  assert(FComputeNameHashesWorker=nil, 'TDwarfCompilationUnit.DoWaitForComputeHashes: FComputeNameHashesWorker=nil');
end;

procedure TDwarfCompilationUnit.WaitForScopeScan;
begin
  if FScanAllWorker <> nil then
    DoWaitForScopeScan;
end;

procedure TDwarfCompilationUnit.WaitForComputeHashes;
begin
  if FComputeNameHashesWorker <> nil then
    DoWaitForComputeHashes;
end;

function TDwarfCompilationUnit.GetFirstScope: TDwarfScopeInfo;
begin
  Assert(FFirstScope.ScopeListPtr <> nil);
  Result := FFirstScope;
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

  if FBuildAddressMapSection.EnterOrWait(CachedRtlEvent) then begin
    if not FAddressMapBuild then begin

      Scope := FirstScope;
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
          Info.ScopeList := Scope.ScopeListPtr;
          // TODO: abstract origin
          if InitLocateAttributeList(Scope.Entry, AttribList) then begin // TODO: error if not
            if (dafHasLowAddr in AttribList.Abbrev^.flags) and
               LocateAttribute(Scope.Entry, DW_AT_low_pc, AttribList, Attrib, Form)
            then begin
              ReadAddressValue(Attrib, Form, Info.StartPC);

              if LocateAttribute(Scope.Entry, DW_AT_high_pc, AttribList, Attrib, Form) then begin
                ReadAddressValue(Attrib, Form, Info.EndPC);
                if (Form = DW_FORM_data1) or (Form = DW_FORM_data2) or (Form = DW_FORM_data4) or
                   (Form = DW_FORM_data8) or (Form = DW_FORM_udata) or (Form = DW_FORM_sdata)
                then
                  Info.EndPC := Info.StartPC + Info.EndPC;
              end
              else
                Info.EndPC := Info.StartPC;

              // TODO (dafHasName in Abbrev.flags)
              if (dafHasName in AttribList.Abbrev^.flags) and
                 LocateAttribute(Scope.Entry, DW_AT_name, AttribList, Attrib, Form)
              then ReadValue(Attrib, Form, Info.Name)
              else Info.Name := 'undefined';

              Info.StateMachine := nil;
              if Info.StartPC <> 0
              then begin
                if FAddressMap.HasId(Info.StartPC)
                then DebugLn(FPDBG_DWARF_WARNINGS, ['WARNING duplicate start address: ', IntToHex(Info.StartPC, FAddressSize * 2)])
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

    FBuildAddressMapSection.Leave;
  end;
  assert(FAddressMapBuild, 'TDwarfCompilationUnit.BuildAddressMap: FAddressMapBuild');
end;

constructor TDwarfCompilationUnit.Create(AOwner: TFpDwarfInfo; ADebugFile: PDwarfDebugFile; ADataOffset: QWord; ALength: QWord; AVersion: Word; AAbbrevOffset: QWord; AAddressSize: Byte; AIsDwarf64: Boolean);
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
    S, S2: String;
    pb: PByte absolute Name;
    oldFpc: Boolean;
    i: SizeInt;
  begin
    FLineInfo.Header := AData;
    if LNP64^.Signature = DWARF_HEADER64_SIGNATURE
    then begin
      if FVersion < 3 then
        DebugLn(FPDBG_DWARF_WARNINGS, ['Unexpected 64 bit signature found for DWARF version 2']); // or version 1...
      UnitLength := LNP64^.UnitLength;
      FLineInfo.DataEnd := Pointer(@LNP64^.Version) + UnitLength;
      Version := LNP64^.Version;
      HeaderLength := LNP64^.HeaderLength;
      Info := @LNP64^.Info;
    end
    else begin
      UnitLength := LNP32^.UnitLength;
      FLineInfo.DataEnd := Pointer(@LNP32^.Version) + UnitLength;
      Version := LNP32^.Version;
      HeaderLength := LNP32^.HeaderLength;
      Info := @LNP32^.Info;
    end;
    if Version=0 then ;
    FLineInfo.Addr64 := FAddressSize = 8;
    FLineInfo.AddrSize := FAddressSize;
    FLineInfo.DataStart := PByte(Info) + HeaderLength;
    FLineInfo.Version := Version;


    FLineInfo.MinimumInstructionLength := Info^.MinimumInstructionLength;
    FLineInfo.MaximumInstructionLength := 1;
    if Version >= 4 then begin
      // Older FreePascal writes an incorrect header
      oldFpc := False;
      s := LowerCase(FProducer);
      i := Pos('free pascal ',  s);
      if i > 0 then begin
        s := copy(s, i+12,5);
        oldFpc := (Length(s) = 5) and (
          (s[1] = '2') or                                   // fpc 2.x
          ( (s[1] = '3') and (s[3] in ['0', '1']) ) or      // fpc 3.0 / 3.1
          ( (s[1] = '3') and (s[3] = '2') and (s[5] in ['0', '1', '2']) ) // fpc 3.2.[012]]
        );
      end;
      if not oldFpc then begin
        inc(PByte(Info)); // All fields move by 1 byte // Dwarf-4 has a new field
        FLineInfo.MaximumInstructionLength := Info^.MinimumInstructionLength;
      end;
    end;
    FLineInfo.DefaultIsStmt := Info^.DefaultIsStmt <> 0;
    FLineInfo.LineBase := Info^.LineBase;
    FLineInfo.LineRange := Info^.LineRange;

    // opcodelengths
    SetLength(FLineInfo.StandardOpcodeLengths, Info^.OpcodeBase - 1);
    Move(Info^.StandardOpcodeLengths, FLineInfo.StandardOpcodeLengths[0], Info^.OpcodeBase - 1);

    // directories & filenames
    FLineInfo.Directories := TStringList.Create;
    FLineInfo.Directories.Add(''); // current dir
    Name := PChar(@Info^.StandardOpcodeLengths);
    Inc(Name, Info^.OpcodeBase-1);

    // directories
    while Name^ <> #0 do
    begin
      S := String(Name);
      Inc(pb, Length(S)+1);
      FLineInfo.Directories.Add(S + DirectorySeparator); // AppendPathDelim();
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
      if diridx < FLineInfo.Directories.Count then begin
        S2 := FLineInfo.Directories[diridx] + S;
        S := CreateAbsolutePath(S2, FCompDir);
        if (diridx = 0) and (not FileExistsUTF8(S)) and (FLineInfo.FileNames.Count > 0) then // https://gitlab.com/freepascal.org/fpc/source/-/issues/37658 https://bugs.freepascal.org/view.php?id=37658
          S := S2;
      end
      else
        S := Format('Unknown dir(%u)', [diridx]) + DirectorySeparator + S;
      FLineInfo.FileNames.Add(S);
      //last modified
      ULEB128toOrdinal(pb);
      //length
      ULEB128toOrdinal(pb);
    end;

    FLineInfo.StateMachine := TDwarfLineInfoStateMachine.Create(Self, FLineInfo.DataStart, FLineInfo.DataEnd);
    FLineInfo.StateMachines := TFPObjectList.Create(True);

    // MaximumInstructionLength is currently not supported
    if FLineInfo.MaximumInstructionLength <> 1 then
      exit;
    FLineInfo.Valid := True;
  end;

var
  AttribList: TAttribPointerList;
  Attrib: Pointer;
  Form: Cardinal;
  StatementListOffs, Offs: QWord;
  Scope: TDwarfScopeInfo;
begin
  //DebugLn(FPDBG_DWARF_VERBOSE, ['-- compilation unit --']);
  //DebugLn(FPDBG_DWARF_VERBOSE, [' data offset: ', ADataOffset]);
  //DebugLn(FPDBG_DWARF_VERBOSE, [' length: ', ALength]);
  //DebugLn(FPDBG_DWARF_VERBOSE, [' version: ', AVersion]);
  //DebugLn(FPDBG_DWARF_VERBOSE, [' abbrev offset: ', AAbbrevOffset]);
  //DebugLn(FPDBG_DWARF_VERBOSE, [' address size: ', AAddressSize]);
  //DebugLn(FPDBG_DWARF_VERBOSE, [' 64bit: ', AIsDwarf64]);
  //DebugLn(FPDBG_DWARF_VERBOSE, ['----------------------']);
  inherited Create;
  FOwner := AOwner;
  FDebugFile := ADebugFile;
  FInfoData := ADebugFile^.Sections[dsInfo].RawData + ADataOffset;
  FLength := ALength;
  FVersion := AVersion;
  FAbbrevOffset := AAbbrevOffset;

  case AIsDwarf64 of
    True:  case AVersion of
      0..4: FHeaderSize := SizeOf(TDwarfCUHeader64);
      else  FHeaderSize := SizeOf(TDwarfCUHeader64v5);
    end;
    False: case AVersion of
      0..4: FHeaderSize := SizeOf(TDwarfCUHeader32);
      else  FHeaderSize := SizeOf(TDwarfCUHeader32v5);
    end;
  end;

  // check for address as offset
  if FAbbrevOffset > ADebugFile^.Sections[dsAbbrev].Size
  then begin
    Offs := FAbbrevOffset - FOwner.FImageBase - ADebugFile^.Sections[dsAbbrev].VirtualAddress;
    if (Offs >= 0) and (Offs < ADebugFile^.Sections[dsAbbrev].Size)
    then begin
      DebugLn(FPDBG_DWARF_WARNINGS, ['WARNING: Got Abbrev offset as address, adjusting..']);
      FAbbrevOffset := Offs;
    end;
  end;

  FAddressSize := AAddressSize;
  FIsDwarf64 := AIsDwarf64;

  FAbbrevList := TDwarfAbbrevList.Create(ADebugFile^.Sections[dsAbbrev].RawData,
    ADebugFile^.Sections[dsAbbrev].RawData + ADebugFile^.Sections[dsAbbrev].Size,
    FAbbrevOffset, FLength);

  // use internally 64 bit target pointer
  FAddressMap := TMap.Create(itu8, SizeOf(TDwarfAddressInfo));

  FFirstScope.Init(nil); // invalid

  if not FAbbrevList.Valid then begin
    FDwarfSymbolClassMap := DwarfSymbolClassMapList.FDefaultMap.GetInstanceForCompUnit(Self);
    exit;
  end;

  FComputeNameHashesWorker := TFpThreadWorkerComputeNameHashes.Create(Self);
  FComputeNameHashesWorker.AddRef;
  FScanAllWorker := TFpThreadWorkerScanAll.Create(Self, FComputeNameHashesWorker);
  FScanAllWorker.AddRef;

  Scope := FScanAllWorker.FindCompileUnit(FScopeList);
  if not Scope.IsValid then begin
    DebugLn(FPDBG_DWARF_WARNINGS, ['WARNING compilation unit has no compile_unit tag']);
    FDwarfSymbolClassMap := DwarfSymbolClassMapList.FDefaultMap.GetInstanceForCompUnit(Self);
    Exit;
  end;
  FValid := True;
  FCompUnitScope := Scope;

  AttribList.EvalCount := 0;
  /// TODO: (dafHasName in Abbrev.flags)
  if LocateAttribute(Scope.Entry, DW_AT_name, AttribList, Attrib, Form)
  then ReadValue(Attrib, Form, FFileName);

  if LocateAttribute(Scope.Entry, DW_AT_comp_dir, AttribList, Attrib, Form)
  then ReadValue(Attrib, Form, FCompDir);

  if LocateAttribute(Scope.Entry, DW_AT_producer, AttribList, Attrib, Form)
  then ReadValue(Attrib, Form, FProducer);

  if LocateAttribute(Scope.Entry, DW_AT_language, AttribList, Attrib, Form)
  then ReadValue(Attrib, Form, FLanguageId);

  FDwarfSymbolClassMap := DwarfSymbolClassMapList.FindMapForCompUnit(Self);
  assert(FDwarfSymbolClassMap <> nil, 'TDwarfCompilationUnit.Create: FDwarfSymbolClassMap <> nil');

  if not LocateAttribute(Scope.Entry, DW_AT_identifier_case, AttribList, Attrib, Form)
  and not ReadValue(Attrib, Form, FIdentifierCase)
  then FIdentifierCase := DW_ID_case_sensitive;

  if LocateAttribute(Scope.Entry, DW_AT_stmt_list, AttribList, Attrib, Form)
  and ReadValue(Attrib, Form, StatementListOffs)
  then begin
    // check for address as offset
    if StatementListOffs < ADebugFile^.Sections[dsLine].Size
    then begin
      FillLineInfo(ADebugFile^.Sections[dsLine].RawData + StatementListOffs);
    end
    else begin
      Offs := StatementListOffs - FOwner.FImageBase - ADebugFile^.Sections[dsLine].VirtualAddress;
      if (Offs >= 0) and (Offs < ADebugFile^.Sections[dsLine].Size)
      then begin
        DebugLn(FPDBG_DWARF_WARNINGS, ['WARNING: Got Lineinfo offset as address, adjusting..']);
        FillLineInfo(ADebugFile^.Sections[dsLine].RawData + Offs);
      end;
    end;
  end;
  if FLineInfo.StateMachine <> nil then
    FInitialStateMachine := FLineInfo.StateMachine.Clone;
    if FInitialStateMachine <> nil then
      FInitialStateMachine.NextLine;

  if LocateAttribute(Scope.Entry, DW_AT_low_pc, AttribList, Attrib, Form)
  then ReadAddressValue(Attrib, Form, FMinPC);

  if LocateAttribute(Scope.Entry, DW_AT_high_pc, AttribList, Attrib, Form) then begin
    ReadAddressValue(Attrib, Form, FMaxPC);
    if (Form = DW_FORM_data1) or (Form = DW_FORM_data2) or (Form = DW_FORM_data4) or
       (Form = DW_FORM_data8) or (Form = DW_FORM_udata) or (Form = DW_FORM_sdata)
    then
      FMaxPC := FMinPC + FMaxPC;
  end;

  if FMinPC = 0 then FMinPC := FMaxPC;
  if FMaxPC = 0 then FMAxPC := FMinPC;

  // FScope and FScopeList  *MUST NOT*  be accessed while the worker is running
  FOwner.WorkQueue.PushItem(FScanAllWorker);
end;

destructor TDwarfCompilationUnit.Destroy;
begin
  FOwner.WorkQueue.RemoveItem(FComputeNameHashesWorker);
  FOwner.WorkQueue.RemoveItem(FScanAllWorker);
  FComputeNameHashesWorker.DecRef;
  FScanAllWorker.DecRef;

  FreeAndNil(FAbbrevList);
  FreeAndNil(FAddressMap);
  FreeAndNil(FInitialStateMachine);
  FreeAndNil(FLineInfo.StateMachines);
  FreeAndNil(FLineInfo.StateMachine);
  FreeAndNil(FLineInfo.Directories);
  FreeAndNil(FLineInfo.FileNames);

  inherited Destroy;
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
    if not SkipEntryDataForForm(AEntry, ADefs[AbrIdx].Form, FAddressSize, IsDwarf64, Version) then
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
      if not SkipEntryDataForForm(AEntry, ADefs[n].Form, FAddressSize, IsDwarf64, Version) then
        break;
    end;
  end;
  Result := False;
end;

function TDwarfCompilationUnit.ReadTargetAddressFromDwarfSection(var AData: Pointer;
  AIncPointer: Boolean): TFpDbgMemLocation;
begin
  // do not need mem reader, address is in dwarf. Should be in correct format
  if (FAddressSize = 8) then
    Result := Owner.MemModel.AddressToTargetLocation(PQWord(AData)^)
  else if (FAddressSize = 4) then
    Result := Owner.MemModel.AddressToTargetLocation(PLongWord(AData)^)
  else if (FAddressSize = 2) then
    Result := Owner.MemModel.AddressToTargetLocation(PWord(AData)^);
  if AIncPointer then inc(AData, FAddressSize);
end;

function TDwarfCompilationUnit.ReadDwarfSectionOffsetOrLenFromDwarfSection(var AData: Pointer;
  AIncPointer: Boolean): TFpDbgMemLocation;
begin
  // do not need mem reader, address is in dwarf. Should be in correct format
  if ((Version>2) and IsDwarf64) or ((version < 3) and (FAddressSize = 8)) then
    Result := Owner.MemModel.AddressToTargetLocation(PQWord(AData)^)
  else
    Result := Owner.MemModel.AddressToTargetLocation(PLongWord(AData)^);
  if AIncPointer then inc(AData, FAddressSize);
end;

function TDwarfCompilationUnit.MapAddressToNewValue(AValue: QWord): QWord;
var
  i: Integer;
  AddrMap: TDbgAddressMap;
begin
  result := avalue;
  if assigned(DebugFile^.AddressMapList) then
    for i := 0 to DebugFile^.AddressMapList.Count-1 do
    begin
      AddrMap:=DebugFile^.AddressMapList[i];
      if AddrMap.OrgAddr=AValue then
      begin
        result:=AddrMap.NewAddr;
        break;
      end
      else if (AddrMap.OrgAddr<AValue) and (AValue<=(AddrMap.OrgAddr+AddrMap.Length)) then
      begin
        result:=AddrMap.NewAddr + (AValue-AddrMap.OrgAddr) ;
        break;
      end;
    end;
end;

function TDwarfCompilationUnit.CalculateRelocatedAddress(AValue: QWord): QWord;
begin
  {$push}
  {$Q-}{$R-}
  Result := AValue + FOwner.RelocationOffset;
  if FAddressSize = 4 then
    Result := DWORD(Result);
  {$pop}
end;

function TDwarfCompilationUnit.GetProcStartEnd(const AAddress: TDBGPtr; out
  AStartPC, AEndPC: TDBGPtr): boolean;
var
  Iter: TLockedMapIterator;
  Info: PDwarfAddressInfo;
begin
  if not FAddressMapBuild then
    BuildAddressMap;

  Result := false;
  Iter := TLockedMapIterator.Create(FAddressMap);
  try
    if not Iter.Locate(AAddress) then
    begin
      if not Iter.BOM then
        Iter.Previous;

      if Iter.BOM then
        Exit;
    end;

    // iter is at the closest defined address before AAddress
    Info := Iter.DataPtr;
    result := (AAddress >= Info^.StartPC) and (AAddress <= Info^.EndPC);
    if Result then
    begin
      AStartPC := Info^.StartPC;
      AEndPC := Info^.EndPC;
    end;

  finally
    Iter.Free;
  end;
end;

function TDwarfCompilationUnit.HasAddress(AAddress: TDbgPtr;
  AWaitFor: TWaitRequirements): Boolean;
begin
  Result := Valid and
            ( (FMinPC = FMaxPC) or
              ((AAddress >= FMinPC) and (AAddress <= FMaxPC))
            );

  if not Result then
    exit;

  if AWaitFor = [] then
    exit;

  WaitForScopeScan;
  Result := Valid;
  if not Result then
    exit;

  if wrAddrMap in AWaitFor then
    BuildAddressMap;
end;

function TDwarfCompilationUnit.ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: Cardinal): Boolean;
begin
  Result := True;
  case AForm of
    DW_FORM_addr:
      AValue := LocToAddrOrNil(ReadTargetAddressFromDwarfSection(AAttribute));
    DW_FORM_ref_addr : begin
      AValue := LocToAddrOrNil(ReadDwarfSectionOffsetOrLenFromDwarfSection(AAttribute));
    end;
    DW_FORM_flag_present: AValue := 1;
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
    DW_FORM_sec_offset: begin
      if IsDwarf64 then
        AValue := PQWord(AAttribute)^
      else
        AValue := PLongWord(AAttribute)^;
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
    DW_FORM_addr:
      AValue := LocToAddrOrNil(ReadTargetAddressFromDwarfSection(AAttribute));
    DW_FORM_ref_addr : begin
      AValue := LocToAddrOrNil(ReadDwarfSectionOffsetOrLenFromDwarfSection(AAttribute));
    end;
    DW_FORM_flag_present: AValue := 1;
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
    DW_FORM_sec_offset: begin
      if IsDwarf64 then
        AValue := PQWord(AAttribute)^
      else
        AValue := PLongWord(AAttribute)^;
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
    DW_FORM_addr:
      AValue := LocToAddrOrNil(ReadTargetAddressFromDwarfSection(AAttribute));
    DW_FORM_ref_addr : begin
      AValue := LocToAddrOrNil(ReadDwarfSectionOffsetOrLenFromDwarfSection(AAttribute));
    end;
    DW_FORM_flag_present: AValue := 1;
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
    DW_FORM_sec_offset: begin
      if IsDwarf64 then
        AValue := PQWord(AAttribute)^
      else
        AValue := PLongWord(AAttribute)^;
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
      if IsDwarf64 then
        AValue := pchar(PtrUInt(FDebugFile^.Sections[dsStr].RawData)+PQWord(AAttribute)^)
      else
        AValue := pchar(PtrUInt(FDebugFile^.Sections[dsStr].RawData)+PDWord(AAttribute)^);
    end;
  else
    Result := False;
  end;
end;

function TDwarfCompilationUnit.ReadValue(AAttribute: Pointer; AForm: Cardinal; out AValue: QWord): Boolean;
begin
  Result := True;
  case AForm of
    DW_FORM_addr:
      AValue := LocToAddrOrNil(ReadTargetAddressFromDwarfSection(AAttribute));
    DW_FORM_ref_addr : begin
      AValue := LocToAddrOrNil(ReadDwarfSectionOffsetOrLenFromDwarfSection(AAttribute));
    end;
    DW_FORM_flag_present: AValue := 1;
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
    DW_FORM_sec_offset: begin
      if IsDwarf64 then
        AValue := PQWord(AAttribute)^
      else
        AValue := PLongWord(AAttribute)^;
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
      if IsDwarf64 then
        AValue := pchar(PtrUInt(FDebugFile^.Sections[dsStr].RawData)+PQWord(AAttribute)^)
      else
        AValue := pchar(PtrUInt(FDebugFile^.Sections[dsStr].RawData)+PDWord(AAttribute)^);
    end;
  else
    Result := False;
  end;
end;

function TDwarfCompilationUnit.ReadValue(AAttribute: Pointer; AForm: Cardinal;
  out AValue: TByteDynArray; AnFormString: Boolean): Boolean;
var
  Size: Cardinal;
  mx, i: Pointer;
begin
  Result := True;
  case AForm of
    DW_FORM_block,
    DW_FORM_exprloc  : begin
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
    DW_FORM_strp, DW_FORM_string: begin
      Result := AnFormString;
      Size := 0;
      if Result then begin
        mx := FDebugFile^.Sections[dsInfo].RawData +FDebugFile^.Sections[dsInfo].Size;
        if AForm = DW_FORM_strp then begin
          AAttribute := FDebugFile^.Sections[dsStr].RawData+PDWord(AAttribute)^;
          mx := FDebugFile^.Sections[dsStr].RawData +FDebugFile^.Sections[dsStr].Size;
        end;
        i := AAttribute;
        while (PByte(i)^ <> 0) and (i < mx) do Inc(i);
        if i = mx then begin
          DebugLn(FPDBG_DWARF_ERRORS, 'String exceeds section');
          Result := False;
        end
        else begin
          Size := i + 1 - AAttribute; // include #0
        end;
      end;
    end;
  else
    Result := False;
    Size := 0;
  end;
  SetLength(AValue, Size);
  if Size > 0 then
    Move(AAttribute^, AValue[0], Size);
end;

function TDwarfCompilationUnit.ReadAddressValue(AAttribute: Pointer; AForm: Cardinal; out AValue: QWord): Boolean;
begin
  result := ReadValue(AAttribute, AForm, AValue);
  if result then
    begin
    AValue := MapAddressToNewValue(AValue);
    AValue := CalculateRelocatedAddress(AValue);
    end;
end;

initialization
  FPDBG_DWARF_ERRORS        := DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_ERRORS' {$IFDEF FPDBG_DWARF_ERRORS} , True {$ENDIF} );
  FPDBG_DWARF_WARNINGS      := DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_WARNINGS' {$IFDEF FPDBG_DWARF_WARNINGS} , True {$ENDIF} );
  FPDBG_DWARF_VERBOSE       := DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_VERBOSE' {$IFDEF FPDBG_DWARF_VERBOSE} , True {$ENDIF} );
  FPDBG_DWARF_VERBOSE_LOAD  := DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_VERBOSE_LOAD' {$IFDEF FPDBG_DWARF_VERBOSE_LOAD} , True {$ENDIF} );
  FPDBG_DWARF_SEARCH        := DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_SEARCH' {$IFDEF FPDBG_DWARF_SEARCH} , True {$ENDIF} );
  // Target data anormalities
  //FPDBG_DWARF_DATA_WARNINGS :=
  DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_DATA_WARNINGS' {$IFDEF FPDBG_DWARF_DATA_WARNINGS} , True {$ENDIF} );

finalization
  FreeAndNil(TheDwarfSymbolClassMapList);
  if CachedRtlEvent <> nil then
    RTLEventDestroy(CachedRtlEvent);
end.

