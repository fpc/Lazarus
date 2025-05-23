{ $Id$ }
{
 ---------------------------------------------------------------------------
 fpdbgclasses.pp  -  Native freepascal debugger
 ---------------------------------------------------------------------------

 This unit contains debugger classes for a native freepascal debugger

 ---------------------------------------------------------------------------

 @created(Mon Apr 10th WET 2006)
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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit FpDbgClasses;
{$mode objfpc}{$H+}
{$ModeSwitch typehelpers }
{$TYPEDADDRESS on}
{$IFDEF INLINE_OFF}{$INLINE OFF}{$ENDIF}
{$IF FPC_Fullversion=30202}{$Optimization NOPEEPHOLE}{$ENDIF}

interface

uses
  Classes, SysUtils, Maps, FpDbgUtil, FpDbgLoader, FpDbgInfo,
  FpdMemoryTools, {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif}, LazClasses, LazFileUtils, DbgIntfBaseTypes,
  fgl, DbgIntfDebuggerBase, fpDbgSymTableContext,
  FpDbgCommon, FpErrorMessages, FpDbgDwarfCFI, LazDebuggerIntf;

type
  TFPDEvent = (
    deNone,
    deExitProcess, deCreateProcess,
    deLoadLibrary, deUnloadLibrary,
    deFinishedStep, deBreakpoint, deHardCodedBreakpoint,
    deException,
    deInternalContinue,
    deDetachFromProcess,
    deFailed);
  TFPDCompareStepInfo = (dcsiNewLine, dcsiSameLine, dcsiNoLineInfo, dcsiZeroLine);

  TGDbgRegisterValueList = specialize TFPGObjectList<TDbgRegisterValue>;

  { TDbgRegisterValueList }

  TDbgRegisterValueList = class(TGDbgRegisterValueList)
  private
    FPreviousRegisterValueList: TDbgRegisterValueList;

    function GetDbgRegister(AName: string): TDbgRegisterValue;
    function GetDbgRegisterAutoCreate(const AName: string): TDbgRegisterValue;
    function GetDbgRegisterCreate(AName: string): TDbgRegisterValue;
    function GetIsModified(AReg: TDbgRegisterValue): boolean;
  public
    procedure Assign(ASource: TDbgRegisterValueList);
    property DbgRegisterAutoCreate[AName: string]: TDbgRegisterValue read GetDbgRegisterAutoCreate;
    property DbgRegisterCreate[AName: string]: TDbgRegisterValue read GetDbgRegisterCreate;
    function FindRegisterByDwarfIndex(AnIdx: cardinal): TDbgRegisterValue;
    function FindRegisterByName(AnName: String): TDbgRegisterValue;
    property IsModified[AReg: TDbgRegisterValue]: boolean read GetIsModified;
  end;

  { TDbgCallstackEntry }
  TDbgThread = class;
  TFPDThreadArray = array of TDbgThread;
  TDbgInstance = class;
  TDbgLibrary = class;
  TOSDbgClasses = class;
  TDbgAsmInstruction = class;

  TDbgCallstackEntry = class
  private
    FAnAddress: TDBGPtr;
    FAutoFillRegisters: boolean;
    FContext: TFpDbgSimpleLocationContext;
    FFrameAdress: TDBGPtr;
    FThread: TDbgThread;
    FIsSymbolResolved: boolean;
    FSymbol: TFpSymbol;
    FRegisterValueList: TDbgRegisterValueList;
    FIndex: integer;
    function GetContext: TFpDbgSimpleLocationContext;
    function GetFunctionName: string;
    function GetProcSymbol: TFpSymbol;
    function GetLine: integer;
    function GetRegisterValueList: TDbgRegisterValueList;
    function GetSourceFile: string;
    function GetSrcClassName: string;
    procedure SetContext(AValue: TFpDbgSimpleLocationContext);
  public
    constructor create(AThread: TDbgThread; AnIndex: integer; AFrameAddress, AnAddress: TDBGPtr);
    destructor Destroy; override;
    property AnAddress: TDBGPtr read FAnAddress;
    property FrameAdress: TDBGPtr read FFrameAdress;
    property SourceFile: string read GetSourceFile;
    property FunctionName: string read GetFunctionName;
    property SrcClassName: string read GetSrcClassName;
    property Line: integer read GetLine;
    property RegisterValueList: TDbgRegisterValueList read GetRegisterValueList;
    property ProcSymbol: TFpSymbol read GetProcSymbol;
    property Index: integer read FIndex;
    property AutoFillRegisters: boolean read FAutoFillRegisters write FAutoFillRegisters;
    property Context: TFpDbgSimpleLocationContext read GetContext write SetContext;
  end;

  { TDbgCallstackEntryList }

  TDbgCallstackEntryList = class(specialize TFPGObjectList<TDbgCallstackEntry>)
  private
    FHasReadAllAvailableFrames: boolean;
  protected
  public
    procedure SetHasReadAllAvailableFrames;
    procedure Clear;
    property HasReadAllAvailableFrames: boolean read FHasReadAllAvailableFrames;
  end;

  TDbgProcess = class;
  TFpWatchPointData = class;

  { TDbgMemReader }

  TDbgMemReader = class(TFpDbgMemReaderBase)
  protected
    function GetDbgProcess: TDbgProcess; virtual; abstract;
    function GetDbgThread(AContext: TFpDbgLocationContext): TDbgThread; virtual;
  public
    function ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; override; overload;
    function ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer; out ABytesRead: Cardinal): Boolean; override; overload;
    function ReadMemoryEx(AnAddress, AnAddressSpace: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; override;
    function WriteMemory(AnAddress: TDbgPtr; ASize: Cardinal; ASource: Pointer): Boolean; override; overload;
    function ReadRegister(ARegNum: Cardinal; out AValue: TDbgPtr; AContext: TFpDbgLocationContext): Boolean; override;
    function RegisterSize(ARegNum: Cardinal): Integer; override;
    function RegisterNumber(ARegName: String; out ARegNum: Cardinal): Boolean; override;
    function GetRegister(const ARegNum: Cardinal; AContext: TFpDbgLocationContext): TDbgRegisterValue; override;

    function WriteRegister(ARegNum: Cardinal; const AValue: TDbgPtr; AContext: TFpDbgLocationContext): Boolean; override;
  end;

  { TDbgStackFrameInfo
    This can be overridden by each OS dependen class. Or it could be gotten from the Disassemble, if it is CPU specific
    This default assumes an Intel like stack, with StackPointer and FrameBase.
    This default assumes the stack grows by decreasing addresses.
  }

  TDbgStackFrameInfo = class
  private
    FThread: TDbgThread;
    FStoredStackFrame, FStoredStackPointer: TDBGPtr;
    FHasSteppedOut: Boolean;
    FProcessAfterRun: Boolean;
    FLeaveState: (lsNone, lsWasAtLeave1, lsWasAtLeave2, lsLeaveDone);
    Procedure DoAfterRun;
  protected
    procedure DoCheckNextInstruction(ANextInstruction: TDbgAsmInstruction; NextIsSingleStep: Boolean); virtual;
    function  CalculateHasSteppedOut: Boolean;  virtual;
  public
    constructor Create(AThread: TDbgThread);
    procedure CheckNextInstruction(ANextInstruction: TDbgAsmInstruction; NextIsSingleStep: Boolean); inline;
    function  HasSteppedOut: Boolean; inline;
    procedure FlagAsSteppedOut; inline;

    // only for FpLldbDebugger
    property StoredStackFrame: TDBGPtr read FStoredStackFrame;
  end;

  TTDbgStackUnwindResult = (suSuccess, suFailed,
    suFailedAtEOS, // this is the End Of Stack
    suGuessed      // Got a frame, but may be wrong
  );

  TDbgStackUnwinder = class
  public
    procedure InitForThread(AThread: TDbgThread); virtual; abstract;
    // FrameBasePointer is optional
    procedure GetTopFrame(out CodePointer, StackPointer, FrameBasePointer: TDBGPtr;
                          out ANewFrame: TDbgCallstackEntry); virtual; abstract;
    procedure InitForFrame(ACurrentFrame: TDbgCallstackEntry;
                           out CodePointer, StackPointer, FrameBasePointer: TDBGPtr); virtual; abstract;
    // AFrameIndex: The frame-index to be read. Starts at 1 (since 0 is top-lever, and handled by GetTopFrame)
    function Unwind(AFrameIndex: integer;
                    var CodePointer, StackPointer, FrameBasePointer: TDBGPtr;
                    ACurrentFrame: TDbgCallstackEntry; // nil for top frame
                    out ANewFrame: TDbgCallstackEntry
                   ): TTDbgStackUnwindResult; virtual; abstract;
  end;

  { TDbgStackUnwinderX86Base }
  // Avoid circular unit refs

  TDbgStackUnwinderX86Base = class(TDbgStackUnwinder)
  private
    FThread: TDbgThread;
    FProcess: TDbgProcess;
    FAddressSize: Integer;
  protected
    FDwarfNumIP, FDwarfNumBP, FDwarfNumSP: integer;
    FNameIP, FNameBP, FNameSP: String;
    property Process: TDbgProcess read FProcess;
    property Thread: TDbgThread read FThread;
    property AddressSize: Integer read FAddressSize;
  public
    constructor Create(AProcess: TDbgProcess);
    procedure InitForThread(AThread: TDbgThread); override;
    procedure InitForFrame(ACurrentFrame: TDbgCallstackEntry; out CodePointer,
      StackPointer, FrameBasePointer: TDBGPtr); override;
    procedure GetTopFrame(out CodePointer, StackPointer, FrameBasePointer: TDBGPtr;
      out ANewFrame: TDbgCallstackEntry); override;
  end;

  { TDbgThread }
  TFpInternalBreakpoint = class;

  TDbgThread = class(TObject)
  private
    FNextIsSingleStep: boolean;
    FNum: Integer;
    FProcess: TDbgProcess;
    FID: Integer;
    FHandle: THandle;
    FStoredBreakpointInfoState: (rbUnknown, rbNone, rbFound{, rbFoundAndDec});
    FStoredBreakpointInfoAddress: TDBGPtr;
    FPausedAtHardcodeBreakPoint: Boolean;
    FSuspendCount: Integer;

    function GetRegisterValueList: TDbgRegisterValueList;
  protected
    FCallStackEntryList: TDbgCallstackEntryList;
    FRegisterValueListValid: boolean;
    FRegisterValueList,
    FPreviousRegisterValueList: TDbgRegisterValueList;
    FStoreStepSrcFilename, FStoreStepFuncName: string;
    FStoreStepStartAddr, FStoreStepEndAddr: TDBGPtr;
    FStoreStepSrcLineNo: integer;
    FStoreStepFuncAddr: TDBGPtr;
    FStackBeforeAlloc: TDBGPtr;

    procedure StoreHasBreakpointInfoForAddress(AnAddr: TDBGPtr); inline;
    procedure ClearHasBreakpointInfoForAddressMismatch(AKeepOnlyForAddr: TDBGPtr); inline;
    procedure ClearHasBreakpointInfoForAddress; inline;
    function  HasBreakpointInfoForAddressMismatch(AnAddr: TDBGPtr): boolean; inline;

    procedure LoadRegisterValues; virtual;
    property Process: TDbgProcess read FProcess;
    function ResetInstructionPointerAfterBreakpoint: boolean; virtual; abstract;
    procedure DoBeforeBreakLocationMapChange; // A new location added / or a location removed => memory will change
    procedure ValidateRemovedBreakPointInfo;
    function GetName: String; virtual;
    function GetStackUnwinder: TDbgStackUnwinder; virtual; abstract;

    (* The "HasBreakpointInfoForAddress" is used if a breakpoint was hit,
       and removed while some DbgThread may still need to know it was there.
       The address of interest is therefore where the breakpoint is.
       Depending on the architecture the IP has to be adjusted.
    *)
    function GetInstructionPointerForHasBreakpointInfoForAddress: TDBGPtr; virtual;
  public
    constructor Create(const AProcess: TDbgProcess; const AID: Integer; const AHandle: THandle); virtual;
    procedure DoBeforeProcessLoop;
    function HasInsertedBreakInstructionAtLocation(const ALocation: TDBGPtr): Boolean; // include removed breakpoints that (may have) already triggered
    (* CheckAndResetInstructionPointerAfterBreakpoint
       This will check if the last instruction was a breakpoint (int3).
       It must ONLY be called, if the signal indicated that it should have been.
       Since the previous IP is not known, this assumes the length of the
         previous asm statement to be the same as the length of int3.
       If a longer command would end in the signature of int3, then this would
         detect the int3 (false positive)
    *)
    procedure CheckAndResetInstructionPointerAfterBreakpoint;
    function CheckForHardcodeBreakPoint(AnAddr: TDBGPtr): boolean;
    procedure BeforeContinue; virtual;
    procedure ApplyWatchPoints(AWatchPointData: TFpWatchPointData); virtual;
    function DetectHardwareWatchpoint: Pointer; virtual;
    // This function changes the value of a register in the debugee.
    procedure SetRegisterValue(AName: string; AValue: QWord); virtual; abstract;

    function GetInstructionPointerRegisterValue: TDbgPtr; virtual; abstract;
    function GetStackBasePointerRegisterValue: TDbgPtr; virtual; abstract;
    function GetStackPointerRegisterValue: TDbgPtr; virtual; abstract;
    procedure SetStackPointerRegisterValue(AValue: TDbgPtr); virtual; abstract;
    procedure SetInstructionPointerRegisterValue(AValue: TDbgPtr); virtual; abstract;
    function GetCurrentStackFrameInfo: TDbgStackFrameInfo;

    function AllocStackMem(ASize: Integer): TDbgPtr; virtual;
    procedure RestoreStackMem;

    procedure PrepareCallStackEntryList(AFrameRequired: Integer = -1); virtual;
    function  FindCallStackEntryByBasePointer(AFrameBasePointer: TDBGPtr; AMaxFrameToSearch: Integer; AStartFrame: integer = 0): Integer; //virtual;
    function  FindCallStackEntryByInstructionPointer(AInstructionPointer: TDBGPtr; AMaxFrameToSearch: Integer; AStartFrame: integer = 0): Integer; //virtual;
    procedure ClearCallStack;
    // Use these functions to 'save' the value of all registers, and to reset
    // them to their original values. (Used to be able to restore the original
    // situation after calling functions inside the debugee)
    procedure StoreRegisters; virtual; abstract;
    procedure RestoreRegisters; virtual; abstract;
    // It could be that an signal led to an exception, and that this
    // signal is stored to be send to the debuggee again upon continuation.
    // Use ClearExceptionSignal to remove/eat this signal.
    procedure ClearExceptionSignal; virtual;

    procedure IncSuspendCount;
    procedure DecSuspendCount;
    property  SuspendCount: Integer read FSuspendCount;

    destructor Destroy; override;
    function CompareStepInfo(AnAddr: TDBGPtr = 0; ASubLine: Boolean = False): TFPDCompareStepInfo;
    function IsAtStartOfLine: boolean;
    function StoreStepInfo(AnAddr: TDBGPtr = 0): boolean;
    property ID: Integer read FID;
    property Num: Integer read FNum;
    property Handle: THandle read FHandle;
    property Name: String read GetName;
    property NextIsSingleStep: boolean read FNextIsSingleStep write FNextIsSingleStep;
    property RegisterValueList: TDbgRegisterValueList read GetRegisterValueList;
    property CallStackEntryList: TDbgCallstackEntryList read FCallStackEntryList;
    property StoreStepFuncName: String read FStoreStepFuncName;
    property StoreStepFuncAddr: TDBGPtr read FStoreStepFuncAddr;
    property PausedAtHardcodeBreakPoint: Boolean read FPausedAtHardcodeBreakPoint;
  end;
  TDbgThreadClass = class of TDbgThread;

  { TThreadMapUnLockedEnumerator }

  TThreadMapUnLockedEnumerator = class(TMapIterator)
  private
    FDoneFirst: Boolean;
    function GetCurrent: TDbgThread;
  public
    function MoveNext: Boolean;
    property Current: TDbgThread read GetCurrent;
  end;

    { TThreadMapEnumerator }

  TThreadMapEnumerator = class(TLockedMapIterator)
  private
    FDoneFirst: Boolean;
    function GetCurrent: TDbgThread;
  public
    function MoveNext: Boolean;
    property Current: TDbgThread read GetCurrent;
  end;

  { TThreadMap }

  TThreadMap = class(TMap)
  private
    FNumCounter: integer;
  public
    function GetEnumerator: TThreadMapEnumerator;
    procedure Add(const AId, AData); reintroduce;
  end;

  // Simple array to pass a list of multiple libraries in a parameter. Does
  // not own or do anything other with the libraries.
  TDbgLibraryArr = array of TDbgLibrary;

  { TLibraryMapEnumerator }

  TLibraryMapEnumerator = class(TMapIterator)
  private
    FDoneFirst: Boolean;
    function GetCurrent: TDbgLibrary;
  public
    function MoveNext: Boolean;
    property Current: TDbgLibrary read GetCurrent;
  end;

  { TLibraryMap }

  TLibraryMap = class(TMap)
  private
    FLibrariesAdded: TDbgLibraryArr;
    FLibrariesRemoved: TDbgLibraryArr;
  public
    function GetEnumerator: TLibraryMapEnumerator;
    procedure Add(const AId, AData);
    function Delete(const AId): Boolean;
    function GetLib(const AHandle: THandle; out ALib: TDbgLibrary): Boolean;
    function GetLib(const AName: String; out ALib: TDbgLibrary; IsFullName: Boolean = True): Boolean;
    procedure ClearAddedAndRemovedLibraries;
    property LibrariesAdded: TDbgLibraryArr read FLibrariesAdded;
  end;

  TFpInternalBreakpointArray = array of TFpInternalBreakpoint;
  TFpBreakPointTargetHandler = class;
  TFpBreakPointTargetHandlerData = record end;
  PFpBreakPointTargetHandlerDataPointer = ^TFpBreakPointTargetHandlerData;

  { TFpBreakPointMap }

  TFpBreakPointMap = class(TMap)
  strict protected type
    { TFpBreakPointMapEntry }
    TFpBreakPointMapEntry = record
      InternalBreakPoint: Pointer;  // TFpInternalBreakpoint or TFpInternalBreakpointArray
      IsBreakList: ByteBool;
      ErrorSetting: ByteBool;
      TargetHandlerData: TFpBreakPointTargetHandlerData; // must be last
    end;
    PFpBreakPointMapEntry = ^TFpBreakPointMapEntry;

  public type
    { TFpBreakPointMapEnumerationData }
    TFpBreakPointMapEnumerationData = record
      Location: TDBGPtr;
      MapDataPtr: PFpBreakPointMapEntry;
      TargetHandlerDataPtr: PFpBreakPointTargetHandlerDataPointer;
    end;

    { TFpBreakPointMapEnumerator }
    TFpBreakPointMapEnumerator = class(TMapIterator)
    private
      FDoneFirst: Boolean;
      function GetCurrent: TFpBreakPointMapEnumerationData;
    public
      function MoveNext: Boolean;
      property Current: TFpBreakPointMapEnumerationData read GetCurrent;
    end;

  strict private
    FProcess: TDbgProcess;
    FTargetHandler: TFpBreakPointTargetHandler;
    FDataSize: integer;
    FTmpDataPtr: PFpBreakPointMapEntry;
  strict protected
    property Process: TDbgProcess read FProcess;
    property TargetHandler: TFpBreakPointTargetHandler read FTargetHandler;
  public
    constructor Create(AProcess: TDbgProcess; ATargetHandler: TFpBreakPointTargetHandler);
    destructor Destroy; override;
    procedure Clear; reintroduce;

    procedure AddLocation   (const ALocation: TDBGPtr; const AnInternalBreak: TFpInternalBreakpoint; AnIgnoreIfExists: Boolean = True; AForceRetrySetting: Boolean = False);
    procedure RemoveLocation(const ALocation: TDBGPtr; const AnInternalBreak: TFpInternalBreakpoint);
    function HasInsertedBreakInstructionAtLocation(const ALocation: TDBGPtr): Boolean;
    function GetInternalBreaksAtLocation(const ALocation: TDBGPtr): TFpInternalBreakpointArray;

    function GetDataPtr(const AId): PFpBreakPointMapEntry; reintroduce; inline;
    function GetTargetDataPtr(const AId): PFpBreakPointTargetHandlerDataPointer;

    function GetEnumerator: TFpBreakPointMapEnumerator;
  end;

  { TFpBreakPointTargetHandler }

  TFpBreakPointTargetHandler = class abstract
  protected
    class var DBG__VERBOSE, DBG__WARNINGS, DBG__BREAKPOINTS: PLazLoggerLogGroup;
  strict private
    FProcess: TDbgProcess;
    FBreakMap: TFpBreakPointMap;
  protected
    property Process: TDbgProcess read FProcess;
    property BreakMap: TFpBreakPointMap read FBreakMap write FBreakMap;
  public
    constructor Create(AProcess: TDbgProcess);
    function GetDataSize: integer; virtual; abstract;

    function InsertBreakInstructionCode(const ALocation: TDBGPtr; const AnInternalBreak: TFpInternalBreakpoint; AnEntry: PFpBreakPointTargetHandlerDataPointer): boolean; virtual; abstract;
    procedure RemoveBreakInstructionCode(const ALocation: TDBGPtr; const AnInternalBreak: TFpInternalBreakpoint; AnEntry: PFpBreakPointTargetHandlerDataPointer); virtual; abstract;

    // When the debugger modifies the debuggee's code, it might be that the
    // original value underneeth the breakpoint has to be changed. This function
    // makes this possible.
    procedure UpdateMapForNewTargetCode(const AAdress: TDbgPtr; const ASize: Cardinal; const AData); virtual; abstract;

    function IsHardcodeBreakPoint(const ALocation: TDBGPtr): Boolean; virtual; abstract;
    // IsHardcodeBreakPointInCode checks even if no TFpInternalBreakpoint is set at the location
    function IsHardcodeBreakPointInCode(const ALocation: TDBGPtr): Boolean; virtual; abstract;

    procedure TempRemoveBreakInstructionCode(const ALocation: TDBGPtr); virtual; abstract;
    procedure RestoreTempBreakInstructionCodes; virtual; abstract;
    procedure MaskBreakpointsInReadData(const AAdress: TDbgPtr; const ASize: Cardinal; var AData); virtual; abstract;
  end;

  { TGenericBreakPointTargetHandler }

  generic TGenericBreakPointTargetHandler<_BRK_STORE, _BREAK> = class(TFpBreakPointTargetHandler)
  strict protected type
    { TInternalBreakLocationEntry }
    TInternalBreakLocationEntry = packed record
      OrigValue: _BRK_STORE;
    end;
    PInternalBreakLocationEntry = ^TInternalBreakLocationEntry;
    P_BRK_STORE = ^_BRK_STORE;

  private
    FTmpRemovedBreaks: array of TDBGPtr;

  strict protected
    function HPtr(Src: PFpBreakPointTargetHandlerDataPointer): PInternalBreakLocationEntry; inline;
    function GetOrigValueAtLocation(const ALocation: TDBGPtr): _BRK_STORE; // returns break instruction, if there is no break at this location
    procedure AdaptOriginalValueAtLocation(const ALocation: TDBGPtr; const NewOrigValue: _BRK_STORE);

    // Default implementation is to write break instruction to memory
    function DoInsertBreakInstructionCode(const ALocation: TDBGPtr; out OrigValue: _BRK_STORE; AMakeTempRemoved: Boolean): Boolean; virtual;
    function DoRemoveBreakInstructionCode(const ALocation: TDBGPtr; const OrigValue: _BRK_STORE): Boolean; virtual;
  public
    function GetDataSize: integer; override;

    function InsertBreakInstructionCode(const ALocation: TDBGPtr; const AnInternalBreak: TFpInternalBreakpoint; AnEntry: PFpBreakPointTargetHandlerDataPointer): boolean; override;
    procedure RemoveBreakInstructionCode(const ALocation: TDBGPtr; const AnInternalBreak: TFpInternalBreakpoint; AnEntry: PFpBreakPointTargetHandlerDataPointer); override;

    // When the debugger modifies the debuggee's code, it might be that the
    // original value underneeth the breakpoint has to be changed. This function
    // makes this possible.
    procedure UpdateMapForNewTargetCode(const AAdress: TDbgPtr; const ASize: Cardinal; const AData); override;

    function IsHardcodeBreakPoint(const ALocation: TDBGPtr): Boolean; override;
    function IsHardcodeBreakPointInCode(const ALocation: TDBGPtr): Boolean; override;

    procedure TempRemoveBreakInstructionCode(const ALocation: TDBGPtr); override;
    procedure RestoreTempBreakInstructionCodes; override;
    procedure MaskBreakpointsInReadData(const AAdress: TDbgPtr; const ASize: Cardinal; var AData); override;
  end;

  { TFpDbgBreakpoint }

  TFpDbgBreakpoint = class;

  TFpDbgBreakpointState = (bksUnknown, bksOk, bksFailed, bksPending);
  TFpDbgBreakpointStateChangeEvent = procedure(Sender: TFpDbgBreakpoint; ANewState: TFpDbgBreakpointState) of object;

  TFpDbgBreakpoint = class(TObject)
  private
    FFreeByDbgProcess: Boolean;
    FEnabled: boolean;
    FOn_Thread_StateChange: TFpDbgBreakpointStateChangeEvent;
  protected
    procedure SetFreeByDbgProcess(AValue: Boolean); virtual;
    procedure SetEnabled(AValue: boolean);
    function GetState: TFpDbgBreakpointState; virtual;
  public
    function Hit(const AThreadID: Integer; ABreakpointAddress: TDBGPtr): Boolean; virtual; abstract;
    function HasLocation(const ALocation: TDBGPtr): Boolean; virtual; abstract;
    // A breakpoint could also be inside/part of a library.

    procedure AddAddress(const ALocation: TDBGPtr); virtual; abstract;
    procedure RemoveAddress(const ALocation: TDBGPtr); virtual; abstract;
    procedure RemoveAllAddresses; virtual; abstract;

    procedure SetBreak; virtual; abstract;
    procedure ResetBreak; virtual; abstract;

    // FreeByDbgProcess: The breakpoint will be freed by TDbgProcess.Destroy
    // If the breakpoint does not have a process, it will be destroyed immediately
    property FreeByDbgProcess: Boolean read FFreeByDbgProcess write SetFreeByDbgProcess;
    property Enabled: boolean read FEnabled write SetEnabled;
    property State: TFpDbgBreakpointState read GetState;
    // Event runs in dbg-thread
    property On_Thread_StateChange: TFpDbgBreakpointStateChangeEvent read FOn_Thread_StateChange write FOn_Thread_StateChange;
  end;

  { TFpInternalBreakBase }

  TFpInternalBreakBase = class(TFpDbgBreakpoint)
  strict private
    FProcess: TDbgProcess;
  private
    procedure SetProcessToNil;
  protected
    procedure SetFreeByDbgProcess(AValue: Boolean); override;
    procedure UpdateForLibraryLoaded(ALib: TDbgLibrary); virtual;
    procedure UpdateForLibrareUnloaded(ALib: TDbgLibrary); virtual;
    property Process: TDbgProcess read FProcess;
  public
    constructor Create(const AProcess: TDbgProcess); virtual;
  end;

  TFpInternalBreakpointList = specialize TFPGObjectList<TFpInternalBreakBase>;

  { TFpInternalBreakpoint }

  TFpInternalBreakpoint = class(TFpInternalBreakBase)
  private
    FLocation: TDBGPtrArray;
    FInternal: Boolean;
    FState: TFpDbgBreakpointState;
    FErrorSettingCount: integer;
    FUpdateStateLock: integer;
    FNeedUpdateState: boolean;
  protected
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure TriggerUpdateState;
    procedure AddErrorSetting(ALocation: TDBGPtr);
    procedure RemoveErrorSetting(ALocation: TDBGPtr);
    function GetState: TFpDbgBreakpointState; override;
    procedure SetState(AState: TFpDbgBreakpointState);
    procedure UpdateState; virtual;
    procedure UpdateForLibrareUnloaded(ALib: TDbgLibrary); override;
    property Location: TDBGPtrArray read FLocation;
  public
    constructor Create(const AProcess: TDbgProcess; const ALocation: TDBGPtrArray; AnEnabled: Boolean); virtual;
    destructor Destroy; override;
    function Hit(const AThreadID: Integer; ABreakpointAddress: TDBGPtr): Boolean; override;
    function HasLocation(const ALocation: TDBGPtr): Boolean; override;

    procedure AddAddress(const ALocation: TDBGPtr); override;
    procedure AddAddress(const ALocations: TDBGPtrArray);
    procedure RemoveAddress(const ALocation: TDBGPtr); override;
    procedure RemoveAllAddresses; override;

    procedure SetBreak; override;
    procedure ResetBreak; override;

  end;

  { TFpInternalBreakpointAtAddress }

  TFpInternalBreakpointAtAddress = class(TFpInternalBreakpoint) // single address ONLY
  private
    FRemovedLoc: TDBGPtrArray;
  protected
    procedure UpdateState; override;
    procedure UpdateForLibraryLoaded(ALib: TDbgLibrary); override;
    procedure UpdateForLibrareUnloaded(ALib: TDbgLibrary); override; // Don't remove from location list
  public
    destructor Destroy; override;
  end;

  { TFpInternalBreakpointAtSymbol }

  TFpInternalBreakpointAtSymbol = class(TFpInternalBreakpoint)
  private
    FFuncName: String;
    FSymInstance: TDbgInstance;
  protected
    procedure UpdateState; override;
    procedure UpdateForLibraryLoaded(ALib: TDbgLibrary); override;
  public
    constructor Create(const AProcess: TDbgProcess; const AFuncName: String; AnEnabled: Boolean; ASymInstance: TDbgInstance = nil; AIgnoreCase: Boolean = False); virtual;
  end;

  { TFpInternalBreakpointAtFileLine }

  TFpInternalBreakpointAtFileLine = class(TFpInternalBreakpoint)
  private
    FFileName: String;
    FLine: Cardinal;
    FSymInstance: TDbgInstance;
    FFoundFileWithoutLine: Boolean;
  protected
    procedure UpdateState; override;
    procedure UpdateForLibraryLoaded(ALib: TDbgLibrary); override;
  public
    constructor Create(const AProcess: TDbgProcess; const AFileName: String; ALine: Cardinal;
      AnEnabled: Boolean; ASymInstance: TDbgInstance = nil); virtual;
  end;

  { TFpInternalWatchpoint }

  TFpInternalWatchpoint  = class(TFpInternalBreakBase)
  private
    FLocation: TDBGPtr;
    FSize: Cardinal;
    FReadWrite: TDBGWatchPointKind;
    FScope: TDBGWatchPointScope;

    FOtherWatchCount: Integer;
    FFirstWatchLocation: TDBGPtr;
    FFirstWatchSize,
    FOtherWatchesSize,
    FLastWatchSize: Integer;
  protected
    property Location: TDBGPtr read FLocation;
    property Size: Cardinal read FSize;
    property ReadWrite: TDBGWatchPointKind read FReadWrite;
    property Scope: TDBGWatchPointScope read FScope;
  public
    constructor Create(const AProcess: TDbgProcess; const ALocation: TDBGPtr; ASize: Cardinal; AReadWrite: TDBGWatchPointKind;
                      AScope: TDBGWatchPointScope); virtual;
    destructor Destroy; override;

    procedure SetBreak; override;
    procedure ResetBreak; override;
  end;

  // Container to hold target specific process info
  TDbgProcessConfig = class(TPersistent)
  end;

  { TDbgConfig }

  TDbgConfig = class
  private
    FBreakpointSearchMaxLines: integer;
    // WindowBounds
    FUseConsoleWinPos: boolean;
    FUseConsoleWinSize: boolean;
    FUseConsoleWinBuffer: boolean;
    FConsoleWinPos: TPoint;
    FConsoleWinSize: TPoint;
    FConsoleWinBuffer: TPoint;
    // Redir
    FStdInRedirFile: String;
    FStdOutRedirFile: String;
    FStdErrRedirFile: String;
    FFileOverwriteStdIn: Boolean;
    FFileOverwriteStdOut: Boolean;
    FFileOverwriteStdErr: Boolean;
  public
    // WindowBounds
    property UseConsoleWinPos: boolean read FUseConsoleWinPos write FUseConsoleWinPos;
    property UseConsoleWinSize: boolean read FUseConsoleWinSize write FUseConsoleWinSize;
    property UseConsoleWinBuffer: boolean read FUseConsoleWinBuffer write FUseConsoleWinBuffer;
    property ConsoleWinPos: TPoint read FConsoleWinPos write FConsoleWinPos;
    property ConsoleWinSize: TPoint read FConsoleWinSize write FConsoleWinSize;
    property ConsoleWinBuffer: TPoint read FConsoleWinBuffer write FConsoleWinBuffer;
    // Redir
    property StdInRedirFile: String read FStdInRedirFile write FStdInRedirFile;
    property StdOutRedirFile: String read FStdOutRedirFile write FStdOutRedirFile;
    property StdErrRedirFile: String read FStdErrRedirFile write FStdErrRedirFile;
    property FileOverwriteStdIn:  Boolean read FFileOverwriteStdIn  write FFileOverwriteStdIn;
    property FileOverwriteStdOut: Boolean read FFileOverwriteStdOut write FFileOverwriteStdOut;
    property FileOverwriteStdErr: Boolean read FFileOverwriteStdErr write FFileOverwriteStdErr;
    // Breakpoints
    property BreakpointSearchMaxLines: integer read FBreakpointSearchMaxLines  write FBreakpointSearchMaxLines;
  end;

  { TDbgInstance }

  TDbgInstance = class(TObject)
  private
    FMemManager: TFpDbgMemManager;
    FMemModel: TFpDbgMemModel;
    FMode: TFPDMode;
    FFileName: String;
    FProcess: TDbgProcess;
    FSymbolTableInfo: TFpSymbolInfo;
    FLoaderList: TDbgImageLoaderList;
    FLastLineAddressesFoundFile: Boolean;
    function GetOSDbgClasses: TOSDbgClasses;
    function GetPointerSize: Integer;

    function  GetLineAddresses(AFileName: String; ALine: Cardinal; var AResultList: TDBGPtrArray;
      AFindSibling: TGetLineAddrFindSibling = fsNone; AMaxSiblingDistance: integer = 0): Boolean;
    function FindProcSymbol(const AName: String; AIgnoreCase: Boolean = False): TFpSymbol; overload;
    function FindProcSymbol(AAdress: TDbgPtr): TFpSymbol; overload;
  protected
    FDbgInfo: TDbgInfo;
    procedure InitializeLoaders; virtual;
    procedure SetFileName(const AValue: String);
    procedure SetMode(AMode: TFPDMode); experimental; // for testcase
    function FindCallFrameInfo(AnAddress: TDBGPtr; out CIE: TDwarfCIE; out Row: TDwarfCallFrameInformationRow): Boolean;
  public
    constructor Create(const AProcess: TDbgProcess); virtual;
    destructor Destroy; override;

    // Returns the addresses at the given source-filename and line-number.
    // Searches the program and all libraries. This can lead to multiple hits,
    // as the application and libraries can share sourcecode but have their own
    // binary code.

    function  FindProcStartEndPC(AAdress: TDbgPtr; out AStartPC, AEndPC: TDBGPtr): boolean;

    // Check if a certain (range of) address(es) belongs to a specific Instance
    // (for example a library)
    function EnclosesAddress(AnAddress: TDBGPtr): Boolean;
    function EnclosesAddressRange(AStartAddress, AnEndAddress: TDBGPtr): Boolean;

    procedure LoadInfo; virtual;

    property Process: TDbgProcess read FProcess;
    property OSDbgClasses: TOSDbgClasses read GetOSDbgClasses;
    property DbgInfo: TDbgInfo read FDbgInfo;
    property SymbolTableInfo: TFpSymbolInfo read FSymbolTableInfo;
    property Mode: TFPDMode read FMode;
    property PointerSize: Integer read GetPointerSize;
    property MemManager: TFpDbgMemManager read FMemManager;
    property MemModel: TFpDbgMemModel read FMemModel;
    property LoaderList: TDbgImageLoaderList read FLoaderList;
  end;

  { TDbgLibrary }

  TDbgLibrary = class(TDbgInstance)
  private
    FModuleHandle: THandle;
    FBreakUpdateDone: Boolean;
  public
    constructor Create(const AProcess: TDbgProcess; const ADefaultName: String; const AModuleHandle: THandle);
    property Name: String read FFileName;
    property ModuleHandle: THandle read FModuleHandle;
  end;

  TStartInstanceFlag = (siRediretOutput, siForceNewConsole);
  TStartInstanceFlags = set of TStartInstanceFlag;

  { TDbgAsmInstruction }

  TDbgAsmInstruction = class(TRefCountedObject)
  public
    // returns byte len of call instruction at AAddress // 0 if not a call intruction
    function IsCallInstruction: boolean; virtual;
    function IsReturnInstruction: boolean; virtual;
    function IsLeaveStackFrame: boolean; virtual;
    //function ModifiesBasePointer: boolean; virtual;
    function ModifiesStackPointer: boolean; virtual;
    function IsJumpInstruction(IncludeConditional: Boolean = True; IncludeUncoditional: Boolean = True): boolean; virtual;
    function InstructionLength: Integer; virtual;
  end;

  TDbgInstInfo = record
    InstrType: (itAny, itJump);
    InstrTargetOffs: Int64; // offset from the START address of instruction
  end;

  { TDbgAsmDecoder }

  TDbgAsmDecoder = class
  protected
    function GetLastErrorWasMemReadErr: Boolean; virtual;
    function GetMaxInstrSize: integer; virtual; abstract;
    function GetMinInstrSize: integer; virtual; abstract;
    function GetCanReverseDisassemble: boolean; virtual;
  public
    constructor Create(AProcess: TDbgProcess); virtual; abstract;

    procedure Disassemble(var AAddress: Pointer; out ACodeBytes: String; out ACode: String; out AnInfo: TDbgInstInfo); virtual; overload;
    procedure Disassemble(var AAddress: Pointer; out ACodeBytes: String; out ACode: String); virtual; abstract; overload;
    procedure ReverseDisassemble(var AAddress: Pointer; out ACodeBytes: String; out ACode: String); virtual;

    function GetInstructionInfo(AnAddress: TDBGPtr): TDbgAsmInstruction; virtual; abstract;
    function GetFunctionFrameInfo(AnAddress: TDBGPtr; out AnIsOutsideFrame: Boolean): Boolean; virtual;
    function IsAfterCallInstruction(AnAddress: TDBGPtr): boolean; virtual;
    function UnwindFrame(var AnAddress, AStackPtr, AFramePtr: TDBGPtr; AQuick: boolean; ARegisterValueList: TDbgRegisterValueList): boolean; virtual;

    property LastErrorWasMemReadErr: Boolean read GetLastErrorWasMemReadErr;
    property MaxInstructionSize: integer read GetMaxInstrSize;  // abstract
    property MinInstructionSize: integer read GetMinInstrSize;  // abstract
    property CanReverseDisassemble: boolean read GetCanReverseDisassemble;
  end;
  TDbgDisassemblerClass = class of TDbgAsmDecoder;

  TDebugOutputEvent = procedure(Sender: TObject; ProcessId, ThreadId: Integer; AMessage: String) of object;

  TFpDbgDataCache = class(specialize TFPGMapObject<Pointer, TObject>);

  { TDbgProcess }

  TDbgProcess = class(TDbgInstance)
  private
    FDisassembler: TDbgAsmDecoder;
    FExceptionClass: string;
    FExceptionMessage: string;
    FExitCode: DWord;
    FGotExitProcess: Boolean;
    FLastLibraryUnloaded: TDbgLibrary;
    FOnDebugOutputEvent: TDebugOutputEvent;
    FOSDbgClasses: TOSDbgClasses;
    FProcessID: Integer;
    FThreadID: Integer;
    FWatchPointData: TFpWatchPointData;
    FProcessConfig: TDbgProcessConfig;
    FConfig: TDbgConfig;
    FGlobalCache: TFpDbgDataCache;
    function DoGetCfiFrameBase(AContext: TFpDbgLocationContext; out AnError: TFpError): TDBGPtr;
    function DoGetFrameBase(AContext: TFpDbgLocationContext; out AnError: TFpError): TDBGPtr;
    function GetDisassembler: TDbgAsmDecoder;
    function GetLastLibrariesLoaded: TDbgLibraryArr;
    function GetLastLibrariesUnloaded: TDbgLibraryArr;
    function GetPauseRequested: boolean;
    procedure SetPauseRequested(AValue: boolean);
    procedure ThreadDestroyed(const AThread: TDbgThread);
  protected
    FBreakpointList, FWatchPointList: TFpInternalBreakpointList;
    FCurrentBreakpoint: TFpInternalBreakpoint;  // set if we are executing the code at the break
                                         // if the singlestep is done, set the break again
    FCurrentBreakpointList: TFpInternalBreakpointArray; // further current breakpoint
    FCurrentWatchpoint: Pointer;         // Indicates the owner
    FReEnableBreakStep: Boolean;         // Set when we are reenabling a breakpoint
                                         // We need a single step, so the IP is after the break to set

    FSymInstances: TList;  // list of dbgInstances with debug info

    FThreadMap: TThreadMap; // map ThreadID -> ThreadObject
    FLibMap: TLibraryMap;    // map LibAddr -> LibObject
    FBreakMap: TFpBreakPointMap;  // map BreakAddr -> BreakObject
    FBreakTargetHandler: TFpBreakPointTargetHandler;
    FPauseRequested: longint;

    FMainThread: TDbgThread;
    function GetHandle: THandle; virtual;
    procedure SetThreadId(AThreadId: Integer);
    procedure SetExitCode(AValue: DWord);
    function GetLastEventProcessIdentifier: THandle; virtual;
    function DoBreak(BreakpointAddress: TDBGPtr; AThreadID: integer): Boolean;
    procedure SetLastLibraryUnloaded(ALib: TDbgLibrary);
    procedure SetLastLibraryUnloadedNil(ALib: TDbgLibrary);
    procedure AddLibrary(ALib: TDbgLibrary; AnID: TDbgPtr);
    function GetRequiresExecutionInDebuggerThread: boolean; virtual;

    procedure BeforeChangingInstructionCode(const ALocation: TDBGPtr; ACount: Integer); virtual;
    procedure AfterChangingInstructionCode(const ALocation: TDBGPtr; ACount: Integer); virtual;
    procedure AfterBreakpointAdded(ABreak: TFpDbgBreakpoint);

    procedure MaskBreakpointsInReadData(const AAdress: TDbgPtr; const ASize: Cardinal; var AData);
    // Should create a TDbgThread-instance for the given ThreadIdentifier.
    function CreateThread(AthreadIdentifier: THandle; out IsMainThread: boolean): TDbgThread; virtual; abstract;
    // Should analyse why the debugger has stopped.
    function AnalyseDebugEvent(AThread: TDbgThread): TFPDEvent; virtual; abstract;

    function CreateWatchPointData: TFpWatchPointData; virtual;
    procedure Init(const AProcessID, AThreadID: Integer);
    function CreateConfig: TDbgConfig;
    function CreateBreakPointTargetHandler: TFpBreakPointTargetHandler; virtual; abstract;
    procedure InitializeLoaders; override;
  public
    class function isSupported(ATargetInfo: TTargetDescriptor): boolean; virtual;
    constructor Create(const AFileName: string; AnOsClasses: TOSDbgClasses;
                      AMemManager: TFpDbgMemManager; AMemModel: TFpDbgMemModel;
                      AProcessConfig: TDbgProcessConfig = nil); virtual;
    destructor Destroy; override;

    function StartInstance(AParams, AnEnvironment: TStrings; AWorkingDirectory, AConsoleTty: string;
                      AFlags: TStartInstanceFlags; out AnError: TFpError): boolean; virtual;
    function AttachToInstance(APid: Integer; out AnError: TFpError): boolean; virtual;

    function  AddInternalBreak(const ALocation: TDBGPtr): TFpInternalBreakpoint; overload;
    function  AddInternalBreak(const ALocation: TDBGPtrArray): TFpInternalBreakpoint; overload;
    (* ASymInstance: nil = anywhere / TDbgProcess or TDbgLibrary to limit *)
    function  AddBreak(const ALocation: TDBGPtr; AnEnabled: Boolean = True): TFpDbgBreakpoint; overload;
    function  AddBreak(const ALocation: TDBGPtrArray; AnEnabled: Boolean = True): TFpDbgBreakpoint; overload;
    function  AddBreak(const AFileName: String; ALine: Cardinal; AnEnabled: Boolean = True; ASymInstance: TDbgInstance = nil): TFpDbgBreakpoint; overload;
    function  AddBreak(const AFuncName: String; AnEnabled: Boolean = True; ASymInstance: TDbgInstance = nil; AIgnoreCase: Boolean = False): TFpDbgBreakpoint; overload;
    function  AddUserBreak(const ALocation: TDBGPtr; AnEnabled: Boolean = True): TFpDbgBreakpoint; overload;
    function  AddWatch(const ALocation: TDBGPtr; ASize: Cardinal; AReadWrite: TDBGWatchPointKind;
                      AScope: TDBGWatchPointScope): TFpInternalWatchpoint;
    property WatchPointData: TFpWatchPointData read FWatchPointData;
    (* FindProcSymbol(Address)
         Search the program and all libraries.
       FindProcSymbol(Name)
         Search ONLY the program.
       FindProcSymbol(Name, ASymInstance)
         Search ASymInstance (process or lib) / if nil, search all
         Names can be ambigious, as dll can have the same names.
    *)
    function  FindProcSymbol(const AName: String): TFpSymbol; overload; // deprecated 'backward compatible / use FindProcSymbol(AName, TheDbgProcess)';
    function  FindProcSymbol(const AName: String; ASymInstance: TDbgInstance): TFpSymbol; overload;
    procedure FindProcSymbol(const AName: String; ASymInstance: TDbgInstance; out ASymList: TFpSymbolArray; AIgnoreCase: Boolean = False);
    function  FindProcSymbol(const AName, ALibraryName: String; IsFullLibName: Boolean = True): TFpSymbol;  overload;deprecated 'XXXXXXXXXXXXXXXXXXXXXXXXXX';
    function  FindProcSymbol(AAdress: TDbgPtr): TFpSymbol;  overload;
    function  FindSymbolScope(AThreadId, AStackFrame: Integer): TFpDbgSymbolScope;
    function  FindProcStartEndPC(const AAdress: TDbgPtr; out AStartPC, AEndPC: TDBGPtr): boolean;
    function FindCallFrameInfo(AnAddress: TDBGPtr; out CIE: TDwarfCIE; out Row: TDwarfCallFrameInformationRow): Boolean; reintroduce;

    function  GetLineAddresses(AFileName: String; ALine: Cardinal; var AResultList: TDBGPtrArray; ASymInstance: TDbgInstance = nil;
      AFindSibling: TGetLineAddrFindSibling = fsNone; AMaxSiblingDistance: integer = 0): Boolean;
    //function  ContextFromProc(AThreadId, AStackFrame: Integer; AProcSym: TFpSymbol): TFpDbgLocationContext; inline; deprecated 'use TFpDbgSimpleLocationContext.Create';
    function  GetLib(const AHandle: THandle; out ALib: TDbgLibrary): Boolean;
    property  LibMap: TLibraryMap read FLibMap;
    property  LastLibrariesLoaded: TDbgLibraryArr read GetLastLibrariesLoaded;
    property  LastLibrariesUnloaded: TDbgLibraryArr read GetLastLibrariesUnloaded;
    procedure UpdateBreakpointsForLibraryLoaded(ALib: TDbgLibrary);
    procedure UpdateBreakpointsForLibraryUnloaded(ALib: TDbgLibrary);
    function  GetThread(const AID: Integer; out AThread: TDbgThread): Boolean;
    procedure RemoveBreak(const ABreakPoint: TFpDbgBreakpoint);
    procedure DoBeforeBreakLocationMapChange;
    function  HasBreak(const ALocation: TDbgPtr): Boolean; // TODO: remove, once an address can have many breakpoints
    procedure RemoveThread(const AID: DWord);
    function FormatAddress(const AAddress): String;
    function  Pause: boolean; virtual;

    function ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean; virtual;
    function ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData; out APartSize: Cardinal): Boolean; virtual;
    function ReadAddress(const AAdress: TDbgPtr; out AData: TDBGPtr): Boolean; virtual;
    function ReadOrdinal(const AAdress: TDbgPtr; out AData): Boolean; virtual;
    function ReadString(const AAdress: TDbgPtr; const AMaxSize: Cardinal; out AData: String): Boolean; virtual;
    function ReadWString(const AAdress: TDbgPtr; const AMaxSize: Cardinal; out AData: WideString): Boolean; virtual;
    // Get the default location for parameters in default calling mode // Note functions may take result as argument
    function CallParamDefaultLocation(AParamIdx: Integer): TFpDbgMemLocation; virtual;

    //function LocationIsBreakInstructionCode(const ALocation: TDBGPtr): Boolean; // excludes TempRemoved
    procedure TempRemoveBreakInstructionCode(const ALocation: TDBGPtr);
    procedure RestoreTempBreakInstructionCodes;
    function HasInsertedBreakInstructionAtLocation(const ALocation: TDBGPtr): Boolean; // returns Int3, if there is no break at this location

    function CanContinueForWatchEval(ACurrentThread: TDbgThread): boolean; virtual;
    function Continue(AProcess: TDbgProcess; AThread: TDbgThread; SingleStep: boolean): boolean; virtual;
    function WaitForDebugEvent(out ProcessIdentifier, ThreadIdentifier: THandle): boolean; virtual; abstract;
    function ResolveDebugEvent(AThread: TDbgThread): TFPDEvent; virtual;

    // Remove (and free if applicable) all breakpoints for this process. When a
    // library is specified as OnlyForLibrary, only breakpoints that belong to this
    // library are cleared.
    procedure RemoveAllBreakPoints;

    function CheckForConsoleOutput(ATimeOutMs: integer): integer; virtual;
    function GetConsoleOutput: string; virtual;
    procedure SendConsoleInput(AString: string); virtual;

    procedure ClearAddedAndRemovedLibraries;
    procedure DoBeforeProcessLoop;
    function AddThread(AThreadIdentifier: THandle): TDbgThread;
    function GetThreadArray: TFPDThreadArray;
    procedure ThreadsBeforeContinue;
    procedure ThreadsClearCallStack;
    procedure LoadInfo; override;

    function WriteData(const AAdress: TDbgPtr; const ASize: Cardinal; const AData): Boolean; virtual;
    // Modify the debugee's code.
    function WriteInstructionCode(const AAdress: TDbgPtr; const ASize: Cardinal; const AData): Boolean; virtual;

    procedure TerminateProcess; virtual; abstract;
    function Detach(AProcess: TDbgProcess; AThread: TDbgThread): boolean; virtual;

    property OSDbgClasses: TOSDbgClasses read FOSDbgClasses;
    property RequiresExecutionInDebuggerThread: boolean read GetRequiresExecutionInDebuggerThread;
    property Handle: THandle read GetHandle;
    property Name: String read FFileName write SetFileName;
    property ProcessID: integer read FProcessID;
    property ThreadID: integer read FThreadID;
    property ExitCode: DWord read FExitCode;
    property CurrentBreakpoint: TFpInternalBreakpoint read FCurrentBreakpoint;
    property CurrentBreakpointList: TFpInternalBreakpointArray read FCurrentBreakpointList; experimental; // need getter
    property CurrentWatchpoint: Pointer read FCurrentWatchpoint;
    property PauseRequested: boolean read GetPauseRequested write SetPauseRequested;
    function GetAndClearPauseRequested: Boolean;

    // Properties valid when last event was an deException
    property ExceptionMessage: string read FExceptionMessage write FExceptionMessage;
    property ExceptionClass: string read FExceptionClass write FExceptionClass;
    property OnDebugOutputEvent: TDebugOutputEvent read FOnDebugOutputEvent write FOnDebugOutputEvent;

    property LastEventProcessIdentifier: THandle read GetLastEventProcessIdentifier;
    property MainThread: TDbgThread read FMainThread;
    property GotExitProcess: Boolean read FGotExitProcess write FGotExitProcess;
    property Disassembler: TDbgAsmDecoder read GetDisassembler;
    property ThreadMap: TThreadMap read FThreadMap;
    property Config: TDbgConfig read FConfig;
    property GlobalCache: TFpDbgDataCache read FGlobalCache write FGlobalCache;
  end;
  TDbgProcessClass = class of TDbgProcess;

  { TFpWatchPointData }

  TFpWatchPointData = class
  private
    FChanged: Boolean;
  public
    function AddOwnedWatchpoint(AnOwner: Pointer; AnAddr: TDBGPtr; ASize: Cardinal; AReadWrite: TDBGWatchPointKind): boolean; virtual;
    function RemoveOwnedWatchpoint(AnOwner: Pointer): boolean; virtual;
    property Changed: Boolean read FChanged write FChanged;
  end;

  { TFpIntelWatchPointData }

  TFpIntelWatchPointData = class(TFpWatchPointData)
  private
    // For Intel: Dr0..Dr3
    FOwners: array [0..3] of Pointer;
    FDr03: array [0..3] of TDBGPtr;
    FDr7: DWord;
    function GetDr03(AnIndex: Integer): TDBGPtr; inline;
    function GetOwner(AnIndex: Integer): Pointer; inline;
  public
    function AddOwnedWatchpoint(AnOwner: Pointer; AnAddr: TDBGPtr; ASize: Cardinal; AReadWrite: TDBGWatchPointKind): boolean; override;
    function RemoveOwnedWatchpoint(AnOwner: Pointer): boolean; override;
    property Dr03[AnIndex: Integer]: TDBGPtr read GetDr03;
    property Dr7: DWord read FDr7;
    property Owner[AnIndex: Integer]: Pointer read GetOwner;
  end;

  { TOSDbgClasses }

  TOSDbgClasses = class
  public
    DbgProcessClass : TDbgProcessClass;
    DbgThreadClass : TDbgThreadClass;
    DbgDisassemblerClass : TDbgDisassemblerClass;
    constructor Create(
      ADbgProcessClass: TDbgProcessClass;
      ADbgThreadClass: TDbgThreadClass;
      ADbgDisassemblerClass: TDbgDisassemblerClass
    );
    function Equals(AnOther: TOSDbgClasses): Boolean;
  end;

const
  FPDEventNames: array[TFPDEvent] of string = (
    'deNone',
    'deExitProcess', 'deCreateProcess',
    'deLoadLibrary', 'deUnloadLibrary',
    'deFinishedStep', 'deBreakpoint', 'deHardCodedBreakpoint',
    'deException',
    'deInternalContinue',
    'deDetachFromProcess',
    'deFailed'
  );


(* TODO:  refactor those methods to work with a Context, and move (partly) to CFI *)
// GetCanonicalFrameAddress: Get FrameBase
function GetCanonicalFrameAddress(RegisterValueList: TDbgRegisterValueList;
  Row: TDwarfCallFrameInformationRow; out FrameBase: TDBGPtr): Boolean;
function TryObtainNextCallFrame( CurrentCallStackEntry: TDbgCallstackEntry;
  CIE: TDwarfCIE; Size, NextIdx: Integer; Thread: TDbgThread;
  Row: TDwarfCallFrameInformationRow; Process: TDbgProcess;
  out NewCallStackEntry: TDbgCallstackEntry): Boolean;

function GetDbgProcessClass(ATargetInfo: TTargetDescriptor): TOSDbgClasses;

procedure RegisterDbgOsClasses(ADbgOsClasses: TOSDbgClasses);

implementation

uses
  FpDbgDwarfDataClasses,
  FpDbgDwarf;

type
  TOSDbgClassesList = class(specialize TFPGObjectList<TOSDbgClasses>)
  public
    function Find(a: TOSDbgClasses): Integer;
  end;
var
  DBG_VERBOSE, DBG_WARNINGS, DBG_BREAKPOINTS, FPDBG_COMMANDS, FPDBG_DWARF_CFI_WARNINGS: PLazLoggerLogGroup;
  RegisteredDbgProcessClasses: TOSDbgClassesList;

function GetDbgProcessClass(ATargetInfo: TTargetDescriptor): TOSDbgClasses;
var
  i   : Integer;
begin
  for i := 0 to RegisteredDbgProcessClasses.Count - 1 do
  begin
    Result := RegisteredDbgProcessClasses[i];
    try
      if Result.DbgProcessClass.isSupported(ATargetInfo) then
        Exit;
    except
      on e: exception do
      begin
        //writeln('exception! WHY? ', e.Message);
      end;
    end;
  end;
  Result := nil;
end;

procedure RegisterDbgOsClasses(ADbgOsClasses: TOSDbgClasses);
begin
  if not Assigned(RegisteredDbgProcessClasses) then
    RegisteredDbgProcessClasses := TOSDbgClassesList.Create;
  if RegisteredDbgProcessClasses.Find(ADbgOsClasses) < 0 then // TODO: by content
    RegisteredDbgProcessClasses.Add(ADbgOsClasses);
end;

{ TFpDbgBreakpoint }

function TFpDbgBreakpoint.GetState: TFpDbgBreakpointState;
begin
  Result := bksUnknown;
end;

procedure TFpDbgBreakpoint.SetFreeByDbgProcess(AValue: Boolean);
begin
  FFreeByDbgProcess := AValue;
end;

procedure TFpDbgBreakpoint.SetEnabled(AValue: boolean);
begin
  if AValue then
    SetBreak
  else
    ResetBreak;
end;

{ TDbgCallstackEntryList }

procedure TDbgCallstackEntryList.SetHasReadAllAvailableFrames;
begin
  FHasReadAllAvailableFrames := True;
end;

procedure TDbgCallstackEntryList.Clear;
begin
  inherited Clear;
  FHasReadAllAvailableFrames := False;
end;

{ TOSDbgClasses }

constructor TOSDbgClasses.Create(ADbgProcessClass: TDbgProcessClass;
  ADbgThreadClass: TDbgThreadClass;
  ADbgDisassemblerClass: TDbgDisassemblerClass);
begin
  DbgProcessClass      := ADbgProcessClass;
  DbgThreadClass       := ADbgThreadClass;
  DbgDisassemblerClass := ADbgDisassemblerClass;
end;

function TOSDbgClasses.Equals(AnOther: TOSDbgClasses): Boolean;
begin
  Result := (DbgThreadClass       = AnOther.DbgThreadClass) and
            (DbgProcessClass      = AnOther.DbgProcessClass) and
            (DbgDisassemblerClass = AnOther.DbgDisassemblerClass);
end;

{ TOSDbgClassesList }

function TOSDbgClassesList.Find(a: TOSDbgClasses): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and not (Items[Result].Equals(a)) do
    dec(Result);
end;

{ TThreadMapUnLockedEnumerator }

function TThreadMapUnLockedEnumerator.GetCurrent: TDbgThread;
begin
  GetData(Result);
end;

function TThreadMapUnLockedEnumerator.MoveNext: Boolean;
begin
  if FDoneFirst then
    Next
  else
    First;
  FDoneFirst := True;
  Result := not EOM;
end;

{ TThreadMapEnumerator }

function TThreadMapEnumerator.GetCurrent: TDbgThread;
begin
  GetData(Result);
end;

function TThreadMapEnumerator.MoveNext: Boolean;
begin
  if FDoneFirst then
    Next
  else
    First;
  FDoneFirst := True;
  Result := not EOM;
end;

{ TThreadMap }

function TThreadMap.GetEnumerator: TThreadMapEnumerator;
begin
  Result := TThreadMapEnumerator.Create(Self);
end;

procedure TThreadMap.Add(const AId, AData);
begin
  inc(FNumCounter);
  TDbgThread(AData).FNum := FNumCounter;
  inherited Add(AId, AData);
end;

{ TLibraryMapEnumerator }

function TLibraryMapEnumerator.GetCurrent: TDbgLibrary;
begin
  GetData(Result);
end;

function TLibraryMapEnumerator.MoveNext: Boolean;
begin
  if FDoneFirst then
    Next
  else
    First;
  FDoneFirst := True;
  Result := not EOM;
end;

{ TLibraryMap }

function TLibraryMap.GetEnumerator: TLibraryMapEnumerator;
begin
  Result := TLibraryMapEnumerator.Create(Self);
end;

procedure TLibraryMap.Add(const AId, AData);
begin
  inherited Add(AId, AData);
  FLibrariesAdded := Concat(FLibrariesAdded, [TDbgLibrary(AData)]);
end;

function TLibraryMap.Delete(const AId): Boolean;
var
  ALib: TDbgLibrary;
begin
  if GetData(AId, ALib) then
    FLibrariesRemoved := Concat(FLibrariesRemoved, [TDbgLibrary(ALib)]);
  Result := inherited Delete(AId);
end;

function TLibraryMap.GetLib(const AHandle: THandle; out ALib: TDbgLibrary
  ): Boolean;
var
  Iterator: TMapIterator;
  Lib: TDbgLibrary;
begin
  Result := False;
  Iterator := TMapIterator.Create(Self);
  while not Iterator.EOM do
  begin
    Iterator.GetData(Lib);
    Result := Lib.ModuleHandle = AHandle;
    if Result
    then begin
      ALib := Lib;
      Break;
    end;
    Iterator.Next;
  end;
  Iterator.Free;
end;

function TLibraryMap.GetLib(const AName: String; out ALib: TDbgLibrary;
  IsFullName: Boolean): Boolean;
var
  Iterator: TMapIterator;
  Lib: TDbgLibrary;
  s: String;
begin
  Result := False;
  Iterator := TMapIterator.Create(Self);
  while not Iterator.EOM do
  begin
    Iterator.GetData(Lib);
    if IsFullName then
      s := Lib.Name
    else
      s := ExtractFileName(Lib.Name);
    Result := CompareText(s, AName) = 0;
    if Result
    then begin
      ALib := Lib;
      Break;
    end;
    Iterator.Next;
  end;
  Iterator.Free;
end;

procedure TLibraryMap.ClearAddedAndRemovedLibraries;
var
  lib: TDbgLibrary;
begin
  for lib in FLibrariesRemoved do
    lib.Free;
  FLibrariesAdded := [];
  FLibrariesRemoved := [];
end;

{ TFpBreakPointMap.TFpBreakPointMapEnumerator }

function TFpBreakPointMap.TFpBreakPointMapEnumerator.GetCurrent: TFpBreakPointMapEnumerationData;
begin
  Result.MapDataPtr := DataPtr;
  Result.TargetHandlerDataPtr := @Result.MapDataPtr^.TargetHandlerData;
  GetID(Result.Location);
end;

function TFpBreakPointMap.TFpBreakPointMapEnumerator.MoveNext: Boolean;
begin
  if FDoneFirst then
    Next
  else
    First;
  FDoneFirst := True;
  Result := not EOM;
end;

{ TFpBreakPointMap }

constructor TFpBreakPointMap.Create(AProcess: TDbgProcess;
  ATargetHandler: TFpBreakPointTargetHandler);
begin
  FProcess := AProcess;
  FTargetHandler := ATargetHandler;
  ATargetHandler.BreakMap := Self;
  FDataSize := SizeOf(TFpBreakPointMapEntry) + ATargetHandler.GetDataSize;
  FTmpDataPtr := AllocMem(FDataSize);
  inherited Create(itu8, FDataSize);
end;

destructor TFpBreakPointMap.Destroy;
begin
  Clear;
  Freemem(FTmpDataPtr);
  inherited Destroy;
end;

procedure TFpBreakPointMap.Clear;
var
  MapEnumData: TFpBreakPointMap.TFpBreakPointMapEnumerationData;
begin
  debugln(DBG_VERBOSE or DBG_BREAKPOINTS, ['TGenericBreakPointTargetHandler.Clear ']);
  for MapEnumData in Self do begin
    if MapEnumData.MapDataPtr^.IsBreakList then
      TFpInternalBreakpointArray(MapEnumData.MapDataPtr^.InternalBreakPoint) := nil;
  end;
  inherited Clear;
end;

procedure TFpBreakPointMap.AddLocation(const ALocation: TDBGPtr;
  const AnInternalBreak: TFpInternalBreakpoint; AnIgnoreIfExists: Boolean;
  AForceRetrySetting: Boolean);
var
  MapEntryPtr: PFpBreakPointMapEntry;
  Len, i: Integer;
  BList: TFpInternalBreakpointArray;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadIdNotMain('TGenericBreakPointTargetHandler.AddLocation');{$ENDIF}
  MapEntryPtr := GetDataPtr(ALocation);

  if MapEntryPtr <> nil then begin
    if MapEntryPtr^.IsBreakList then begin
      Len := Length(TFpInternalBreakpointArray(MapEntryPtr^.InternalBreakPoint));
      if AnIgnoreIfExists then begin
        i := Len - 1;
        while (i >= 0) and (TFpInternalBreakpointArray(MapEntryPtr^.InternalBreakPoint)[i] <> AnInternalBreak) do
          dec(i);
        if i >= 0 then
          exit;
      end;

      SetLength(TFpInternalBreakpointArray(MapEntryPtr^.InternalBreakPoint), Len+1);
      TFpInternalBreakpointArray(MapEntryPtr^.InternalBreakPoint)[Len] := AnInternalBreak;
    end
    else begin
      if AnIgnoreIfExists and (TFpInternalBreakpoint(MapEntryPtr^.InternalBreakPoint) = AnInternalBreak) then
        exit;

      MapEntryPtr^.IsBreakList := True;
      SetLength(BList, 2);
      BList[0] := TFpInternalBreakpoint(MapEntryPtr^.InternalBreakPoint);
      BList[1] := AnInternalBreak;
      MapEntryPtr^.InternalBreakPoint := nil;
      TFpInternalBreakpointArray(MapEntryPtr^.InternalBreakPoint) := BList;
    end;

    if MapEntryPtr^.ErrorSetting then begin
      if AForceRetrySetting then begin
        FProcess.DoBeforeBreakLocationMapChange; // Only if a new breakpoint is set => memory changed
        MapEntryPtr^.ErrorSetting := not TargetHandler.InsertBreakInstructionCode(ALocation, AnInternalBreak, @MapEntryPtr^.TargetHandlerData);
        if MapEntryPtr^.ErrorSetting then begin
          AnInternalBreak.AddErrorSetting(ALocation);
        end
        else
        if MapEntryPtr^.IsBreakList then begin
          debugln(DBG_VERBOSE or DBG_BREAKPOINTS, ['Retrying failed breakpoint updated multiple instances']);
          for i := 0 to Length(TFpInternalBreakpointArray(MapEntryPtr^.InternalBreakPoint)) - 1 do
            if TFpInternalBreakpointArray(MapEntryPtr^.InternalBreakPoint)[i] <> AnInternalBreak then
              TFpInternalBreakpointArray(MapEntryPtr^.InternalBreakPoint)[i].RemoveErrorSetting(ALocation);
        end;
      end
      else begin
        AnInternalBreak.AddErrorSetting(ALocation);
      end;
    end;
    exit;
  end;


  FillByte(FTmpDataPtr^, FDataSize, 0);
  FTmpDataPtr^.IsBreakList := False;
  FTmpDataPtr^.InternalBreakPoint := AnInternalBreak;

  FProcess.DoBeforeBreakLocationMapChange; // Only if a new breakpoint is set => memory changed
  FTmpDataPtr^.ErrorSetting := not TargetHandler.InsertBreakInstructionCode(ALocation, AnInternalBreak, @FTmpDataPtr^.TargetHandlerData);
  if FTmpDataPtr^.ErrorSetting then
    AnInternalBreak.AddErrorSetting(ALocation);

  Add(ALocation, FTmpDataPtr^);
end;

procedure TFpBreakPointMap.RemoveLocation(const ALocation: TDBGPtr;
  const AnInternalBreak: TFpInternalBreakpoint);
var
  MapEntryPtr: PFpBreakPointMapEntry;
  Len, i: Integer;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadIdNotMain('TGenericBreakPointTargetHandler.RemoveLocation');{$ENDIF}
  MapEntryPtr := GetDataPtr(ALocation);
  if MapEntryPtr = nil then begin
    DebugLn(DBG_WARNINGS or DBG_BREAKPOINTS, ['Missing breakpoint for loc ', FormatAddress(ALocation)]);
    exit;
  end;

  if MapEntryPtr^.IsBreakList then begin
    Len := Length(TFpInternalBreakpointArray(MapEntryPtr^.InternalBreakPoint));
    i := Len - 1;
    while (i >= 0) and (TFpInternalBreakpointArray(MapEntryPtr^.InternalBreakPoint)[i] <> AnInternalBreak) do
      dec(i);
    if i < 0 then begin
      DebugLn(DBG_WARNINGS or DBG_BREAKPOINTS, ['Wrong break for loc ', FormatAddress(ALocation)]);
      exit;
    end;
    if i < Len - 1 then
      move(TFpInternalBreakpointArray(MapEntryPtr^.InternalBreakPoint)[i+1],
           TFpInternalBreakpointArray(MapEntryPtr^.InternalBreakPoint)[i],
           (Len - 1 - i) * sizeof(TFpInternalBreakpoint));
    SetLength(TFpInternalBreakpointArray(MapEntryPtr^.InternalBreakPoint), Len-1);
    if MapEntryPtr^.ErrorSetting then
      AnInternalBreak.RemoveErrorSetting(ALocation);

    if Len > 1 then
      exit;
  end
  else begin
    if AnInternalBreak <> TFpInternalBreakpoint(MapEntryPtr^.InternalBreakPoint) then begin
      DebugLn(DBG_WARNINGS or DBG_BREAKPOINTS, ['Wrong break for loc ', FormatAddress(ALocation)]);
      exit;
    end;

    if MapEntryPtr^.ErrorSetting then
      AnInternalBreak.RemoveErrorSetting(ALocation);
  end;

  FProcess.DoBeforeBreakLocationMapChange; // Only if a breakpoint is removed => memory changed
  if not MapEntryPtr^.ErrorSetting then
    TargetHandler.RemoveBreakInstructionCode(ALocation, AnInternalBreak, @MapEntryPtr^.TargetHandlerData);
  Delete(ALocation);
end;

function TFpBreakPointMap.HasInsertedBreakInstructionAtLocation(
  const ALocation: TDBGPtr): Boolean;
begin
  Result := GetDataPtr(ALocation) <> nil;
end;

function TFpBreakPointMap.GetInternalBreaksAtLocation(const ALocation: TDBGPtr): TFpInternalBreakpointArray;
var
  MapEntryPtr: PFpBreakPointMapEntry;
begin
  MapEntryPtr := GetDataPtr(ALocation);
  if MapEntryPtr = nil then begin
    DebugLn(DBG_WARNINGS or DBG_BREAKPOINTS, ['Missing breakpoint for loc ', FormatAddress(ALocation)]);
    Result := nil;
    exit;
  end;

  if MapEntryPtr^.IsBreakList then begin
    Result := TFpInternalBreakpointArray(MapEntryPtr^.InternalBreakPoint)
  end
  else begin
    SetLength(Result, 1);
    Result[0] := TFpInternalBreakpoint(MapEntryPtr^.InternalBreakPoint);
  end;
end;

function TFpBreakPointMap.GetDataPtr(const AId): PFpBreakPointMapEntry;
begin
  Result := inherited GetDataPtr(AId);
end;

function TFpBreakPointMap.GetTargetDataPtr(const AId): PFpBreakPointTargetHandlerDataPointer;
var
  r: PFpBreakPointMapEntry;
begin
  r := GetDataPtr(AId);
  if r = nil then
    Result := nil
  else
    Result := @GetDataPtr(AId)^.TargetHandlerData;
end;

function TFpBreakPointMap.GetEnumerator: TFpBreakPointMapEnumerator;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TGenericBreakPointTargetHandler.GetEnumerator');{$ENDIF}
  Result := TFpBreakPointMapEnumerator.Create(Self);
end;

{ TFpBreakPointTargetHandler }

constructor TFpBreakPointTargetHandler.Create(AProcess: TDbgProcess);
begin
  FProcess := AProcess;
  inherited Create;
end;

{ TGenericBreakPointTargetHandler }

function TGenericBreakPointTargetHandler.GetDataSize: integer;
begin
  Result := SizeOf(TInternalBreakLocationEntry);
end;

procedure TGenericBreakPointTargetHandler.UpdateMapForNewTargetCode(const AAdress: TDbgPtr;
  const ASize: Cardinal; const AData);
var
  i: Integer;
begin
  for i := 0 to ASize -1 do
    if BreakMap.HasInsertedBreakInstructionAtLocation(AAdress+i) then
      AdaptOriginalValueAtLocation(AAdress+i, PByte(@AData+i)^);
end;

function TGenericBreakPointTargetHandler.GetOrigValueAtLocation(const ALocation: TDBGPtr
  ): _BRK_STORE;
var
  LocData: PInternalBreakLocationEntry;
begin
  LocData := HPtr(BreakMap.GetTargetDataPtr(ALocation));
  if LocData = nil then begin
    DebugLn(DBG__WARNINGS or DBG__BREAKPOINTS, ['Missing breakpoint for loc ', FormatAddress(ALocation)]);
    Result := _BREAK._CODE;
    exit;
  end;
  Result := LocData^.OrigValue;
end;

function TGenericBreakPointTargetHandler.IsHardcodeBreakPoint(const ALocation: TDBGPtr
  ): Boolean;
begin
  Result := GetOrigValueAtLocation(ALocation) = _BREAK._CODE;
end;

function TGenericBreakPointTargetHandler.IsHardcodeBreakPointInCode(
  const ALocation: TDBGPtr): Boolean;
var
  OVal: _BRK_STORE;
begin
  Result := False;
  if Process.ReadData(ALocation, SizeOf(_BRK_STORE), OVal) then
    Result := OVal = _BREAK._CODE;
end;

procedure TGenericBreakPointTargetHandler.TempRemoveBreakInstructionCode(
  const ALocation: TDBGPtr);
var
  OVal: _BRK_STORE;
  l, i: Integer;
begin
  DebugLn(DBG__VERBOSE or DBG__BREAKPOINTS, ['>>> TempRemoveBreakInstructionCode']);
  l := length(FTmpRemovedBreaks);
  for i := 0 to l-1 do
    if FTmpRemovedBreaks[i] = ALocation then
      exit;

  OVal := GetOrigValueAtLocation(ALocation);
  if OVal = _BREAK._CODE then
    exit;

  SetLength(FTmpRemovedBreaks, l+1);
  FTmpRemovedBreaks[l] := ALocation;
  DoRemoveBreakInstructionCode(ALocation, OVal); // Do not update FBreakMap
  DebugLn(DBG__VERBOSE or DBG__BREAKPOINTS, ['<<< TempRemoveBreakInstructionCode']);
end;

procedure TGenericBreakPointTargetHandler.RestoreTempBreakInstructionCodes;
var
  OVal: _BRK_STORE;
  t: array of TDBGPtr;
  i: Integer;
begin
  if Length(FTmpRemovedBreaks) = 0 then
    exit;
  DebugLnEnter(DBG__VERBOSE or DBG__BREAKPOINTS, ['>>> RestoreTempBreakInstructionCodes']);
  t := FTmpRemovedBreaks;
  FTmpRemovedBreaks := nil;
  for i := 0 to length(t) - 1 do
    if BreakMap.HasId(t[i]) then // may have been removed
      DoInsertBreakInstructionCode(t[i], OVal, False);
  DebugLnExit(DBG__VERBOSE or DBG__BREAKPOINTS, ['<<< RestoreTempBreakInstructionCodes']);
end;

procedure TGenericBreakPointTargetHandler.MaskBreakpointsInReadData(const AAdress: TDbgPtr;
  const ASize: Cardinal; var AData);
var
  MapEnumData: TFpBreakPointMap.TFpBreakPointMapEnumerationData;
  i, len: TDBGPtr;
  PtrOrig: Pointer;
begin
  for MapEnumData in BreakMap do begin
    // Does break instruction fall completely outside AData
    {$PUSH}{$R-}{$Q-}
    if (MapEnumData.Location + SizeOf(_BRK_STORE) <= AAdress) or
       (MapEnumData.Location >= (AAdress + ASize)) or
       (MapEnumData.MapDataPtr^.ErrorSetting)
    then
      continue;
    {$POP}

    if (MapEnumData.Location >= AAdress) and (MapEnumData.Location + SizeOf(_BRK_STORE) <= (AAdress + ASize)) then begin
      // Breakpoint is completely inside AData
      // MapEnumData.Location >= AAdress
      i := MapEnumData.Location - AAdress;
      P_BRK_STORE(@AData + i)^ := HPtr(MapEnumData.TargetHandlerDataPtr)^.OrigValue;
    end
    else
    if (MapEnumData.Location < AAdress) then begin
      // Breakpoint starts on or partially overlaps with start of AData
      // Breakpoint may overhang past end of AData
      // AAdress > MapEnumData.Location
      i := AAdress - MapEnumData.Location;
      // i < SizeOf(_BRK_STORE) / since MapEnumData.Location + SizeOf(_BRK_STORE) > AAdress
      len := SizeOf(_BRK_STORE) - i;
      // Do not write past end of AData
      if len > ASize then
        len := ASize;

      PtrOrig := @HPtr(MapEnumData.TargetHandlerDataPtr)^.OrigValue;
      move(PByte(PtrOrig+i)^, PByte(@AData)^, len);
    end
    else begin
      // Breakpoint partially overlaps with end of AData
      // MapEnumData.Location > AAdress
      // MapEnumData.Location < AAdress + ASize;
      i := MapEnumData.Location - AAdress;
      len := ASize - i;  // AAdress + ASize - MapEnumData.Location;
      PtrOrig := @HPtr(MapEnumData.TargetHandlerDataPtr)^.OrigValue;
      move(PByte(PtrOrig)^, PByte(@AData+i)^, len);
    end;
  end;
end;

procedure TGenericBreakPointTargetHandler.AdaptOriginalValueAtLocation(const ALocation: TDBGPtr; const NewOrigValue: _BRK_STORE);
var
  LocData: PInternalBreakLocationEntry;
begin
  LocData := HPtr(BreakMap.GetTargetDataPtr(ALocation));
  if Assigned(LocData) then
    LocData^.OrigValue := NewOrigValue;
end;

function TGenericBreakPointTargetHandler.DoInsertBreakInstructionCode(
  const ALocation: TDBGPtr; out OrigValue: _BRK_STORE;
  AMakeTempRemoved: Boolean): Boolean;
begin
  Result := Process.ReadData(ALocation, SizeOf(_BRK_STORE), OrigValue);
  if not Result then begin
    DebugLn(DBG__WARNINGS or DBG__BREAKPOINTS, 'Unable to read pre-breakpoint at '+FormatAddress(ALocation));
    exit;
  end;

  if (OrigValue = _BREAK._CODE) or AMakeTempRemoved then
    exit; // breakpoint on a hardcoded breakpoint

  Process.BeforeChangingInstructionCode(ALocation, SizeOf(_BRK_STORE));

  Result := Process.WriteData(ALocation, SizeOf(_BRK_STORE), _BREAK._CODE);
  DebugLn(DBG__VERBOSE or DBG__BREAKPOINTS, ['Breakpoint set to '+Process.FormatAddress(ALocation), ' Result:',Result, ' OVal:', OrigValue]);
  if not Result then
    DebugLn(DBG__WARNINGS or DBG__BREAKPOINTS, 'Unable to set breakpoint at '+FormatAddress(ALocation));

  if Result then
    Process.AfterChangingInstructionCode(ALocation, SizeOf(_BRK_STORE));
end;

function TGenericBreakPointTargetHandler.DoRemoveBreakInstructionCode(
  const ALocation: TDBGPtr; const OrigValue: _BRK_STORE): Boolean;
begin
  if OrigValue = _BREAK._CODE then
    exit(True); // breakpoint on a hardcoded breakpoint

  Process.BeforeChangingInstructionCode(ALocation, SizeOf(_BRK_STORE));

  Result := Process.WriteData(ALocation, SizeOf(_BRK_STORE), OrigValue);
  DebugLn(DBG__VERBOSE or DBG__BREAKPOINTS, ['Breakpoint removed from '+FormatAddress(ALocation), ' Result:',Result, ' OVal:', OrigValue]);
  DebugLn((not Result) and (not Process.GotExitProcess) and (DBG__WARNINGS or DBG__BREAKPOINTS), 'Unable to reset breakpoint at %s', [FormatAddress(ALocation)]);

  if Result then
    Process.AfterChangingInstructionCode(ALocation, SizeOf(_BRK_STORE));
end;

function TGenericBreakPointTargetHandler.HPtr(Src: PFpBreakPointTargetHandlerDataPointer): PInternalBreakLocationEntry;
begin
  Result := PInternalBreakLocationEntry(Src);
end;

function TGenericBreakPointTargetHandler.InsertBreakInstructionCode(const ALocation: TDBGPtr;
  const AnInternalBreak: TFpInternalBreakpoint; AnEntry: PFpBreakPointTargetHandlerDataPointer
  ): boolean;
var
  LocData: PInternalBreakLocationEntry absolute AnEntry;
  IsTempRemoved: Boolean;
  i: Integer;
begin
  Result := False;
  IsTempRemoved := False;
  for i := 0 to high(FTmpRemovedBreaks) do begin
    IsTempRemoved := ALocation = FTmpRemovedBreaks[i];
    if IsTempRemoved then
      break;
  end;

  Result := DoInsertBreakInstructionCode(ALocation, LocData^.OrigValue, IsTempRemoved);
end;

procedure TGenericBreakPointTargetHandler.RemoveBreakInstructionCode(const ALocation: TDBGPtr;
  const AnInternalBreak: TFpInternalBreakpoint; AnEntry: PFpBreakPointTargetHandlerDataPointer);
var
  LocData: PInternalBreakLocationEntry absolute AnEntry;
begin
  DoRemoveBreakInstructionCode(ALocation, LocData^.OrigValue);
end;

{ TDbgCallstackEntry }

function TDbgCallstackEntry.GetProcSymbol: TFpSymbol;
begin
  if not FIsSymbolResolved then begin
    if (FIndex > 0) and (FAnAddress <> 0) then
      FSymbol := FThread.Process.FindProcSymbol(FAnAddress - 1) // -1 => inside the call instruction
    else
      FSymbol := FThread.Process.FindProcSymbol(FAnAddress);

    if FSymbol is TFpSymbolDwarfDataProc then
      FSymbol := TFpSymbolDwarfDataProc(FSymbol).ResolveInternalFinallySymbol(FThread.Process);


    FIsSymbolResolved := FSymbol <> nil
  end;
  result := FSymbol;
end;

function TDbgCallstackEntry.GetFunctionName: string;
var
  Symbol: TFpSymbol;
begin
  Symbol := GetProcSymbol;
  if assigned(Symbol) then begin
    if Symbol is TFpSymbolTableProc then begin
      if AnAddress > Symbol.Address.Address then
        result := Format('%s+%d', [Symbol.Name, AnAddress - Symbol.Address.Address])
      else
        result := Symbol.Name;
    end
    else
      result := Symbol.Name;
  end
  else
    result := '';
end;

function TDbgCallstackEntry.GetContext: TFpDbgSimpleLocationContext;
begin
  Result := FContext;
end;

function TDbgCallstackEntry.GetLine: integer;
var
  Symbol: TFpSymbol;
begin
  Symbol := GetProcSymbol;
  if assigned(Symbol) then
    result := Symbol.Line
  else
    result := -1;
end;

function TDbgCallstackEntry.GetRegisterValueList: TDbgRegisterValueList;
begin
  if (FAutoFillRegisters) and (FRegisterValueList.Count = 0) then begin
    FRegisterValueList.Assign(FThread.RegisterValueList);
    FAutoFillRegisters := False;
  end;
  Result := FRegisterValueList;
end;

function TDbgCallstackEntry.GetSourceFile: string;
var
  Symbol: TFpSymbol;
begin
  Symbol := GetProcSymbol;
  if assigned(Symbol) then
    result := Symbol.FileName
  else
    result := '';
end;

function TDbgCallstackEntry.GetSrcClassName: string;
var
  Symbol: TFpSymbol;
begin
  result := '';
  Symbol := GetProcSymbol;
  if assigned(Symbol) then begin
    Symbol := Symbol.Parent;
    if assigned(Symbol) then begin
      result := Symbol.Name;
      Symbol.ReleaseReference;
    end;
  end;
end;

procedure TDbgCallstackEntry.SetContext(AValue: TFpDbgSimpleLocationContext);
begin
  if FContext = AValue then
    exit;
  if FContext <> nil then
    FContext.ReleaseReference;
  FContext := AValue;
  if FContext <> nil then
    FContext.AddReference;
end;

constructor TDbgCallstackEntry.create(AThread: TDbgThread; AnIndex: integer; AFrameAddress, AnAddress: TDBGPtr);
begin
  FThread := AThread;
  FFrameAdress:=AFrameAddress;
  FAnAddress:=AnAddress;
  FIndex:=AnIndex;
  FRegisterValueList := TDbgRegisterValueList.Create;
end;

destructor TDbgCallstackEntry.Destroy;
begin
  FreeAndNil(FRegisterValueList);
  ReleaseRefAndNil(FSymbol);
  FContext.ReleaseReference;
  inherited Destroy;
end;

{ TDbgMemReader }

function TDbgMemReader.GetDbgThread(AContext: TFpDbgLocationContext): TDbgThread;
var
  Process: TDbgProcess;
begin
  Process := GetDbgProcess;
  // In fact, AContext should always be assigned, assuming that the main thread
  // should be used is dangerous. But functions like TFpDbgMemManager.ReadSignedInt
  // have a default value of nil for the context. Which is a lot of work to fix.
  if not Assigned(AContext) or not Process.GetThread(AContext.ThreadId, Result) then
    Result := Process.MainThread;
end;

function TDbgMemReader.ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean;
begin
  result := GetDbgProcess.ReadData(AnAddress, ASize, ADest^);
end;

function TDbgMemReader.ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal;
  ADest: Pointer; out ABytesRead: Cardinal): Boolean;
begin
  result := GetDbgProcess.ReadData(AnAddress, ASize, ADest^, ABytesRead);
end;

function TDbgMemReader.ReadMemoryEx(AnAddress, AnAddressSpace: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean;
begin
  Assert(AnAddressSpace>0,'TDbgMemReader.ReadMemoryEx ignores AddressSpace');
  result := GetDbgProcess.ReadData(AnAddress, ASize, ADest^);
end;

function TDbgMemReader.WriteMemory(AnAddress: TDbgPtr; ASize: Cardinal;
  ASource: Pointer): Boolean;
begin
  result := GetDbgProcess.WriteData(AnAddress, ASize, ASource^);
end;

function TDbgMemReader.ReadRegister(ARegNum: Cardinal; out AValue: TDbgPtr; AContext: TFpDbgLocationContext): Boolean;
var
  ARegister: TDbgRegisterValue;
  StackFrame: Integer;
  AFrame: TDbgCallstackEntry;
  CtxThread: TDbgThread;
begin
  // TODO: Thread with ID
  result := false;
  CtxThread := GetDbgThread(AContext);
  if CtxThread = nil then
    exit;

  if AContext <> nil then // TODO: Always true?
    StackFrame := AContext.StackFrame
  else
    StackFrame := 0;
  if StackFrame = 0 then
    begin
    ARegister:=CtxThread.RegisterValueList.FindRegisterByDwarfIndex(ARegNum);
    end
  else
    begin
    CtxThread.PrepareCallStackEntryList(StackFrame+1);
    if CtxThread.CallStackEntryList.Count <= StackFrame then
      exit;
    AFrame := CtxThread.CallStackEntryList[StackFrame];
    if AFrame <> nil then
      ARegister:=AFrame.RegisterValueList.FindRegisterByDwarfIndex(ARegNum)
    else
      ARegister:=nil;
    end;
  if assigned(ARegister) then
    begin
    AValue := ARegister.NumValue;
    result := true;
    end;
end;

function TDbgMemReader.WriteRegister(ARegNum: Cardinal; const AValue: TDbgPtr; AContext: TFpDbgLocationContext): Boolean;
var
  ARegister: TDbgRegisterValue;
  StackFrame: Integer;
  CtxThread: TDbgThread;
begin
  result := false;
  CtxThread := GetDbgThread(AContext);
  if CtxThread = nil then
    exit;

  if AContext <> nil then // TODO: Always true?
    StackFrame := AContext.StackFrame
  else
    StackFrame := 0;
  if StackFrame = 0 then
    begin
    ARegister:=CtxThread.RegisterValueList.FindRegisterByDwarfIndex(ARegNum);
    if assigned(ARegister) then
      begin
      CtxThread.SetRegisterValue(ARegister.Name, AValue);
      CtxThread.LoadRegisterValues;
      result := true;
      end;
    end
end;

function TDbgMemReader.RegisterSize(ARegNum: Cardinal): Integer;
var
  ARegister: TDbgRegisterValue;
begin
  ARegister:=GetDbgProcess.MainThread.RegisterValueList.FindRegisterByDwarfIndex(ARegNum);
  if assigned(ARegister) then
    result := ARegister.Size
  else
    result := sizeof(pointer);
end;

function TDbgMemReader.RegisterNumber(ARegName: String; out ARegNum: Cardinal
  ): Boolean;
var
  ARegister: TDbgRegisterValue;
  CtxThread: TDbgThread;
begin
  Result := False;
  CtxThread := GetDbgThread(nil);
  if CtxThread = nil then
    exit;

  ARegister:=CtxThread.RegisterValueList.FindRegisterByName(ARegName);
  Result := ARegister <> nil;
  if Result then
    ARegNum := ARegister.DwarfIdx;
end;

function TDbgMemReader.GetRegister(const ARegNum: Cardinal; AContext: TFpDbgLocationContext
  ): TDbgRegisterValue;
var
  StackFrame: Integer;
  AFrame: TDbgCallstackEntry;
  CtxThread: TDbgThread;
begin
  // TODO: Thread with ID
  result := nil;
  CtxThread := GetDbgThread(AContext);
  if CtxThread = nil then
    exit;

  if AContext <> nil then // TODO: Always true?
    StackFrame := AContext.StackFrame
  else
    StackFrame := 0;
  if StackFrame = 0 then
    begin
    Result:=CtxThread.RegisterValueList.FindRegisterByDwarfIndex(ARegNum);
    end
  else
    begin
    CtxThread.PrepareCallStackEntryList(StackFrame+1);
    if CtxThread.CallStackEntryList.Count <= StackFrame then
      exit;
    AFrame := CtxThread.CallStackEntryList[StackFrame];
    if AFrame <> nil then
      Result:=AFrame.RegisterValueList.FindRegisterByDwarfIndex(ARegNum)
    else
      Result:=nil;
    end;
end;

{ TDbgRegisterValueList }

function TDbgRegisterValueList.GetDbgRegister(AName: string
  ): TDbgRegisterValue;
var
  i: integer;
begin
  AName := UpperCase(AName);
  for i := 0 to Count -1 do
    if UpperCase(Items[i].Name)=AName then
      begin
      result := items[i];
      exit;
      end;
  result := nil;
end;

function TDbgRegisterValueList.GetDbgRegisterAutoCreate(const AName: string
  ): TDbgRegisterValue;
begin
  result := GetDbgRegister(AName);
  if not Assigned(result) then
    begin
    result := TDbgRegisterValue.Create(AName);
    add(result);
    end;
end;

function TDbgRegisterValueList.GetDbgRegisterCreate(AName: string): TDbgRegisterValue;
begin
  result := TDbgRegisterValue.Create(AName);
  add(result);
end;

function TDbgRegisterValueList.GetIsModified(AReg: TDbgRegisterValue): boolean;
begin
  Result := FPreviousRegisterValueList <> nil;
  if not Result then
    exit;

  Result := not FPreviousRegisterValueList.FindRegisterByDwarfIndex(AReg.DwarfIdx).HasEqualVal(AReg);
end;

procedure TDbgRegisterValueList.Assign(ASource: TDbgRegisterValueList);
var
  i: Integer;
  Dest: TDbgRegisterValue;
begin
  If Count > ASource.Count then
    Count := ASource.Count;
  Capacity := ASource.Count;

  for i := 0 to ASource.Count - 1 do begin
    if i >= Count then begin
      Dest := TDbgRegisterValue.Create('');
      Add(Dest);
    end
    else
      Dest := Items[i];
    Dest.Assign(ASource[i]);
  end;
end;

function TDbgRegisterValueList.FindRegisterByDwarfIndex(AnIdx: cardinal): TDbgRegisterValue;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    if Items[i].DwarfIdx=AnIdx then
    begin
      result := Items[i];
      exit;
    end;
  result := nil;
end;

function TDbgRegisterValueList.FindRegisterByName(AnName: String
  ): TDbgRegisterValue;
begin
  Result := GetDbgRegister(AnName);
end;

{ TDbgAsmInstruction }

function TDbgAsmInstruction.IsCallInstruction: boolean;
begin
  Result := False;
end;

function TDbgAsmInstruction.IsReturnInstruction: boolean;
begin
  Result := False;
end;

function TDbgAsmInstruction.IsLeaveStackFrame: boolean;
begin
  Result := False;
end;

function TDbgAsmInstruction.ModifiesStackPointer: boolean;
begin
  Result := False;
end;

function TDbgAsmInstruction.IsJumpInstruction(IncludeConditional: Boolean;
  IncludeUncoditional: Boolean): boolean;
begin
  Result := False;
end;

function TDbgAsmInstruction.InstructionLength: Integer;
begin
  Result := 0;
end;

{ TDbgAsmDecoder }

function TDbgAsmDecoder.GetLastErrorWasMemReadErr: Boolean;
begin
  Result := False;
end;

function TDbgAsmDecoder.GetCanReverseDisassemble: boolean;
begin
  Result := false;
end;

procedure TDbgAsmDecoder.Disassemble(var AAddress: Pointer; out
  ACodeBytes: String; out ACode: String; out AnInfo: TDbgInstInfo);
begin
  AnInfo := Default(TDbgInstInfo);
  Disassemble(AAddress, ACodeBytes, ACode);
end;

// Naive backwards scanner, decode MaxInstructionSize
// if pointer to next instruction matches, done!
// If not decrease instruction size and try again.
// Many pitfalls with X86 instruction encoding...
// Avr may give 130/65535 = 0.2% errors per instruction reverse decoded
procedure TDbgAsmDecoder.ReverseDisassemble(var AAddress: Pointer; out
  ACodeBytes: String; out ACode: String);
var
  instrLen: integer;
  tmpAddress: PtrUint;
begin
  // Decode max instruction length backwards,
  instrLen := MaxInstructionSize + MinInstructionSize;
  repeat
    dec(instrLen, MinInstructionSize);
    tmpAddress := PtrUInt(AAddress) - instrLen;
    Disassemble(pointer(tmpAddress), ACodeBytes, ACode);
  until (tmpAddress >= PtrUInt(AAddress)) or (instrLen = MinInstructionSize);

  // After disassemble tmpAddress points to the starting address of next instruction
  // Decrement with the instruction length to point to the start of this instruction
  AAddress := AAddress - instrLen;
end;

function TDbgAsmDecoder.GetFunctionFrameInfo(AnAddress: TDBGPtr; out
  AnIsOutsideFrame: Boolean): Boolean;
begin
  Result := False;
end;

function TDbgAsmDecoder.IsAfterCallInstruction(AnAddress: TDBGPtr): boolean;
begin
  Result := True; // if we don't know, then assume yes
end;

function TDbgAsmDecoder.UnwindFrame(var AnAddress, AStackPtr, AFramePtr: TDBGPtr; AQuick: boolean;
  ARegisterValueList: TDbgRegisterValueList): boolean;
begin
  Result := False;
end;

{ TDbgInstance }


function TDbgInstance.FindProcSymbol(const AName: String; AIgnoreCase: Boolean
  ): TFpSymbol;
begin
  if FDbgInfo <> nil then
    Result := FDbgInfo.FindProcSymbol(AName)
  else
    Result := nil;
  if (Result = nil) and (SymbolTableInfo <> nil) then
    Result := SymbolTableInfo.FindProcSymbol(AName, AIgnoreCase);
end;

constructor TDbgInstance.Create(const AProcess: TDbgProcess);
begin
  FProcess := AProcess;
  FMemManager := AProcess.MemManager;
  FMemModel := AProcess.MemModel;
  FLoaderList := TDbgImageLoaderList.Create(True);

  inherited Create;
end;

destructor TDbgInstance.Destroy;
begin
  FreeAndNil(FDbgInfo);
  FreeAndNil(FSymbolTableInfo);
  FreeAndNil(FLoaderList);
  inherited;
end;

function TDbgInstance.GetLineAddresses(AFileName: String; ALine: Cardinal;
  var AResultList: TDBGPtrArray; AFindSibling: TGetLineAddrFindSibling;
  AMaxSiblingDistance: integer): Boolean;
var
  FoundLine: Integer;
begin
  FLastLineAddressesFoundFile := False;
  if Assigned(DbgInfo) and DbgInfo.HasInfo then
    Result := DbgInfo.GetLineAddresses(AFileName, ALine, AResultList, AFindSibling, @FoundLine, @FLastLineAddressesFoundFile, AMaxSiblingDistance)
  else
    Result := False;
end;

function TDbgInstance.FindProcSymbol(AAdress: TDbgPtr): TFpSymbol;
var
  LI: TFpSymbol;
begin
  {$PUSH}{$R-}{$Q-}
  AAdress := AAdress;
  {$POP}
  Result := nil;
  LI := FDbgInfo.FindLineInfo(AAdress);
  if (LI <> nil) and (LI.Kind in [skFunction, skProcedure]) then begin
    Result := LI;
  end
  else begin
    Result := FSymbolTableInfo.FindProcSymbol(AAdress);
    if (Result <> nil) and (Result is TFpSymbolTableProc) then
      TFpSymbolTableProc(Result).SetLineSym(LI);
    LI.ReleaseReference;
  end;
end;

function TDbgInstance.FindProcStartEndPC(AAdress: TDbgPtr; out AStartPC,
  AEndPC: TDBGPtr): boolean;
begin
  {$PUSH}{$R-}{$Q-}
  AAdress := AAdress;
  {$POP}
  Result := FDbgInfo.FindProcStartEndPC(AAdress, AStartPC, AEndPC);
end;

function TDbgInstance.EnclosesAddress(AnAddress: TDBGPtr): Boolean;
begin
  Result := EnclosesAddressRange(AnAddress, AnAddress);
end;

function TDbgInstance.EnclosesAddressRange(AStartAddress, AnEndAddress: TDBGPtr): Boolean;
begin
  Result := FLoaderList.EnclosesAddressRange(AStartAddress, AnEndAddress);
end;

procedure TDbgInstance.LoadInfo;
begin
  InitializeLoaders;
  if FLoaderList.TargetInfo.bitness = b64 then  //Image64Bit then
    FMode:=dm64
  else
    FMode:=dm32;
  FDbgInfo := TFpDwarfInfo.Create(FLoaderList, MemManager, MemModel);
  TFpDwarfInfo(FDbgInfo).LoadCompilationUnits;
  if self is TDbgProcess then
    FSymbolTableInfo := TFpSymbolInfo.Create(FLoaderList, MemManager, MemModel)
  else
    FSymbolTableInfo := TFpSymbolInfo.Create(FLoaderList, MemManager, ExtractFileNameOnly(FFileName), MemModel);
  TFpDwarfInfo(FDbgInfo).LoadCallFrameInstructions;
end;

procedure TDbgInstance.SetFileName(const AValue: String);
begin
  FFileName := AValue;
end;

procedure TDbgInstance.SetMode(AMode: TFPDMode);
begin
  FMode := AMode;
end;

function TDbgInstance.FindCallFrameInfo(AnAddress: TDBGPtr; out CIE: TDwarfCIE; out
  Row: TDwarfCallFrameInformationRow): Boolean;
begin
  if FDbgInfo <> nil then
    Result := (FDbgInfo as TFpDwarfInfo).FindCallFrameInfo(AnAddress, CIE, Row)
  else
    Result := False;
end;

function TDbgInstance.GetPointerSize: Integer;
const
  PTRSZ: array[TFPDMode] of Integer = (4, 8); // (dm32, dm64)
begin
  Result := PTRSZ[FMode];
end;

function TDbgInstance.GetOSDbgClasses: TOSDbgClasses;
begin
  Result := FProcess.OSDbgClasses;
end;

procedure TDbgInstance.InitializeLoaders;
begin
  // Do nothing;
end;

{ TDbgLibrary }

constructor TDbgLibrary.Create(const AProcess: TDbgProcess; const ADefaultName: String; const AModuleHandle: THandle);

begin
  inherited Create(AProcess);
  FModuleHandle:=AModuleHandle;
end;

{ TDbgProcess }

function TDbgProcess.AddBreak(const ALocation: TDBGPtr; AnEnabled: Boolean
  ): TFpDbgBreakpoint;
var
  a: TDBGPtrArray;
begin
  SetLength(a, 1);
  a[0] := ALocation;
  Result := AddBreak(a, AnEnabled);
// TODO: if a = GetInstructionPointerRegisterValue (of any thread?)
end;

function TDbgProcess.AddBreak(const ALocation: TDBGPtrArray; AnEnabled: Boolean
  ): TFpDbgBreakpoint;
begin
  Result := TFpInternalBreakpoint.Create(Self, ALocation, AnEnabled);
  AfterBreakpointAdded(Result);
end;

function TDbgProcess.AddBreak(const AFileName: String; ALine: Cardinal;
  AnEnabled: Boolean; ASymInstance: TDbgInstance): TFpDbgBreakpoint;
begin
  Result := TFpInternalBreakpointAtFileLine.Create(Self, AFileName, ALine, AnEnabled, ASymInstance);
  AfterBreakpointAdded(Result);
end;

function TDbgProcess.AddBreak(const AFuncName: String; AnEnabled: Boolean;
  ASymInstance: TDbgInstance; AIgnoreCase: Boolean): TFpDbgBreakpoint;
begin
  Result := TFpInternalBreakpointAtSymbol.Create(Self, AFuncName, AnEnabled, ASymInstance, AIgnoreCase);
  AfterBreakpointAdded(Result);
end;

function TDbgProcess.AddUserBreak(const ALocation: TDBGPtr; AnEnabled: Boolean): TFpDbgBreakpoint;
var
  a: TDBGPtrArray;
begin
  SetLength(a, 1);
  a[0] := ALocation;
  Result := TFpInternalBreakpointAtAddress.Create(Self, a, AnEnabled);
  AfterBreakpointAdded(Result);
// TODO: if a = GetInstructionPointerRegisterValue (of any thread?)
end;

function TDbgProcess.AddWatch(const ALocation: TDBGPtr; ASize: Cardinal;
  AReadWrite: TDBGWatchPointKind; AScope: TDBGWatchPointScope
  ): TFpInternalWatchpoint;
begin
  Result := TFpInternalWatchpoint.Create(Self, ALocation, ASize, AReadWrite, AScope);
end;

function TDbgProcess.FindProcSymbol(const AName: String; ASymInstance: TDbgInstance
  ): TFpSymbol;
var
  Lib: TDbgLibrary;
begin
  if ASymInstance <> nil then begin
    Result := ASymInstance.FindProcSymbol(AName);
  end
  else begin
    Result := FindProcSymbol(AName);
    if Result <> nil then
      exit;
    for Lib in FLibMap do begin
      Result := Lib.FindProcSymbol(AName);
      if Result <> nil then
        exit;
    end;
  end;
end;

procedure TDbgProcess.FindProcSymbol(const AName: String;
  ASymInstance: TDbgInstance; out ASymList: TFpSymbolArray; AIgnoreCase: Boolean
  );
var
  Lib: TDbgLibrary;
  Sym: TFpSymbol;
begin
  // TODO: find multiple symbols within the same DbgInfo
  ASymList := nil;
  if ASymInstance <> nil then begin
    Sym := ASymInstance.FindProcSymbol(AName, AIgnoreCase);
    if Sym <> nil then begin
      SetLength(ASymList, 1);
      ASymList[0] := Sym;
    end;
  end
  else begin
    Sym := FindProcSymbol(AName, AIgnoreCase);
    if Sym <> nil then begin
      SetLength(ASymList, 1);
      ASymList[0] := Sym;
    end;

    for Lib in FLibMap do begin
      Sym := Lib.FindProcSymbol(AName, AIgnoreCase);
      if Sym <> nil then begin
        SetLength(ASymList, 1);
        ASymList[0] := Sym;
      end;
    end;
  end;
end;

function TDbgProcess.FindProcSymbol(const AName: String): TFpSymbol;
begin
  Result := inherited FindProcSymbol(AName);
end;

function TDbgProcess.FindProcSymbol(const AName, ALibraryName: String;
  IsFullLibName: Boolean): TFpSymbol;
var
  lib: TDbgLibrary;
begin
  Result := nil;
  if not FLibMap.GetLib(ALibraryName, lib, IsFullLibName) then
    exit;
  Result := lib.FindProcSymbol(AName);
end;

constructor TDbgProcess.Create(const AFileName: string;
  AnOsClasses: TOSDbgClasses; AMemManager: TFpDbgMemManager;
  AMemModel: TFpDbgMemModel; AProcessConfig: TDbgProcessConfig);
const
  {.$IFDEF CPU64}
  MAP_ID_SIZE = itu8;
  {.$ELSE}
//  MAP_ID_SIZE = itu4;
  {.$ENDIF}
begin
  FConfig := CreateConfig;
  FMemManager := AMemManager;
  FMemModel := AMemModel;
  FProcessID := 0;
  FThreadID := 0;
  FOSDbgClasses := AnOsClasses;
  FProcessConfig := AProcessConfig;

  FGlobalCache := TFpDbgDataCache.Create;
  FBreakpointList := TFpInternalBreakpointList.Create(False);
  FWatchPointList := TFpInternalBreakpointList.Create(False);
  FThreadMap := TThreadMap.Create(itu4, SizeOf(TDbgThread));
  FLibMap := TLibraryMap.Create(MAP_ID_SIZE, SizeOf(TDbgLibrary));
  FWatchPointData := CreateWatchPointData;

  FBreakTargetHandler := CreateBreakPointTargetHandler;
  FBreakMap := TFpBreakPointMap.Create(Self, FBreakTargetHandler);
  FBreakTargetHandler.BreakMap := FBreakMap;

  FCurrentBreakpoint := nil;
  FCurrentBreakpointList := nil;
  FCurrentWatchpoint := nil;

  FSymInstances := TList.Create;

  SetFileName(AFileName);

  inherited Create(Self);
end;

destructor TDbgProcess.Destroy;

  procedure FreeItemsInMap(AMap: TMap);
  var
    AnObject: TObject;
    Iterator: TMapIterator;
  begin
    iterator := TMapIterator.Create(AMap);
    try
      Iterator.First;
      while not Iterator.EOM do
      begin
        Iterator.GetData(AnObject);
        AnObject.Free;
        iterator.Next;
      end;
    finally
      Iterator.Free;
    end;
  end;

var
  i: Integer;
begin
  FProcessID:=0;
  SetLastLibraryUnloaded(nil);

  for i := 0 to FBreakpointList.Count - 1 do
    FBreakpointList[i].SetProcessToNil;
  for i := 0 to FWatchPointList.Count - 1 do
    FWatchPointList[i].SetProcessToNil;
  FreeAndNil(FBreakpointList);
  FreeAndNil(FWatchPointList);
  //Assert(FBreakMap.Count=0, 'No breakpoints left');
  //FreeItemsInMap(FBreakMap);
  FreeItemsInMap(FThreadMap);
  FreeItemsInMap(FLibMap);
  FLibMap.ClearAddedAndRemovedLibraries;

  FGlobalCache.Free;
  FreeAndNil(FWatchPointData);
  FBreakTargetHandler.BreakMap := nil;
  FreeAndNil(FBreakMap);
  FreeAndNil(FBreakTargetHandler);
  FreeAndNil(FThreadMap);
  FreeAndNil(FLibMap);
  FreeAndNil(FSymInstances);
  FreeAndNil(FDisassembler);
  FreeAndNil(FConfig);
  inherited;
end;

function TDbgProcess.StartInstance(AParams, AnEnvironment: TStrings;
  AWorkingDirectory, AConsoleTty: string; AFlags: TStartInstanceFlags; out
  AnError: TFpError): boolean;
begin
  DebugLn(DBG_VERBOSE, 'Debug support is not available for this platform.');
  result := false;
end;

function TDbgProcess.AttachToInstance(APid: Integer; out AnError: TFpError): boolean;
begin
  DebugLn(DBG_VERBOSE, 'Attach not supported');
  Result := false;
end;

function TDbgProcess.AddInternalBreak(const ALocation: TDBGPtr): TFpInternalBreakpoint;
begin
  Result := TFpInternalBreakpoint(AddBreak(ALocation));
  Result.FInternal := True;
end;

function TDbgProcess.AddInternalBreak(const ALocation: TDBGPtrArray): TFpInternalBreakpoint;
begin
  Result := TFpInternalBreakpoint(AddBreak(ALocation));
  Result.FInternal := True;
end;

function TDbgProcess.FindProcSymbol(AAdress: TDbgPtr): TFpSymbol;
var
  n: Integer;
  Inst: TDbgInstance;
begin
  for n := 0 to FSymInstances.Count - 1 do
  begin
    Inst := TDbgInstance(FSymInstances[n]);
    Result := Inst.FindProcSymbol(AAdress);
    if Result <> nil then Exit;
  end;
  Result := nil;
end;

function TDbgProcess.FindSymbolScope(AThreadId, AStackFrame: Integer): TFpDbgSymbolScope;
var
  Thread: TDbgThread;
  Frame: TDbgCallstackEntry;
  Addr: TDBGPtr;
  Ctx: TFpDbgSimpleLocationContext;
  sym: TFpSymbol;
begin
  Result := nil;
  Ctx := nil;

  if GetThread(AThreadId, Thread) then begin
    Thread.PrepareCallStackEntryList(AStackFrame + 1);

    if AStackFrame < Thread.CallStackEntryList.Count then begin
      Frame := Thread.CallStackEntryList[AStackFrame];

      if Frame <> nil then begin
        Addr := Frame.AnAddress;
        Ctx := Frame.Context;
        if Ctx <> nil then begin
          Ctx.AddReference;
        end
        else begin
          Ctx := TFpDbgSimpleLocationContext.Create(MemManager, Addr, DBGPTRSIZE[Mode], AThreadId, AStackFrame);
          Ctx.SetFrameBaseCallback(@DoGetFrameBase);
          Ctx.SetCfaFrameBaseCallback(@DoGetCfiFrameBase);
          Ctx.SymbolTableInfo := SymbolTableInfo;
          Frame.Context := Ctx;
        end;
        sym := Frame.ProcSymbol;
        if sym <> nil then
          Result := sym.CreateSymbolScope(Ctx);

        if Result = nil then begin
          if (Addr <> 0) or (FDbgInfo.TargetInfo.machineType = mtAVR8) then
            Result := FDbgInfo.FindSymbolScope(Ctx, Addr);
        end;
      end;
    end;

    // SymbolTableInfo.FindSymbolScope()
  end;

  if Result = nil then begin
    if Ctx = nil then
      Ctx := TFpDbgSimpleLocationContext.Create(MemManager, 0, DBGPTRSIZE[Mode], AThreadId, AStackFrame);
    Result := TFpDbgSymbolScope.Create(Ctx);
  end;

  Ctx.ReleaseReference;
end;

function TDbgProcess.FindProcStartEndPC(const AAdress: TDbgPtr; out AStartPC,
  AEndPC: TDBGPtr): boolean;
var
  n: Integer;
  Inst: TDbgInstance;
begin
  for n := 0 to FSymInstances.Count - 1 do
  begin
    Inst := TDbgInstance(FSymInstances[n]);
    Result := Inst.FindProcStartEndPC(AAdress, AStartPC, AEndPC);
    if Result then Exit;
  end;
end;

function TDbgProcess.FindCallFrameInfo(AnAddress: TDBGPtr; out CIE: TDwarfCIE; out
  Row: TDwarfCallFrameInformationRow): Boolean;
var
  Lib: TDbgLibrary;
begin
  Result := inherited FindCallFrameInfo(AnAddress, CIE, Row);
  if Result then
    exit;

  for Lib in FLibMap do begin
    Result := Lib.FindCallFrameInfo(AnAddress, CIE, Row);
    if Result then
      exit;
  end;
end;

function TDbgProcess.GetLineAddresses(AFileName: String; ALine: Cardinal;
  var AResultList: TDBGPtrArray; ASymInstance: TDbgInstance;
  AFindSibling: TGetLineAddrFindSibling; AMaxSiblingDistance: integer): Boolean;
var
  Lib: TDbgLibrary;
begin
  FLastLineAddressesFoundFile := False;
  if ASymInstance <> nil then begin
    if ASymInstance = self then begin
      Result := inherited GetLineAddresses(AFileName, ALine, AResultList, AFindSibling, AMaxSiblingDistance);
    end
    else begin
      Result := ASymInstance.GetLineAddresses(AFileName, ALine, AResultList, AFindSibling, AMaxSiblingDistance);
      if ASymInstance.FLastLineAddressesFoundFile then
        FLastLineAddressesFoundFile := True;
    end;
    exit;
  end;

  Result := inherited GetLineAddresses(AFileName, ALine, AResultList, AFindSibling, AMaxSiblingDistance);
  for Lib in FLibMap do begin
    if Lib.GetLineAddresses(AFileName, ALine, AResultList, AFindSibling, AMaxSiblingDistance) then
      Result := True;
    if Lib.FLastLineAddressesFoundFile then
      FLastLineAddressesFoundFile := True;
  end;
end;

//function TDbgProcess.ContextFromProc(AThreadId, AStackFrame: Integer;
//  AProcSym: TFpSymbol): TFpDbgLocationContext;
//begin
//  Result := TFpDbgSimpleLocationContext.Create(MemManager, LocToAddrOrNil(AProcSym.Address), DBGPTRSIZE[Mode], AThreadId, AStackFrame);
//end;

function TDbgProcess.GetLib(const AHandle: THandle; out ALib: TDbgLibrary): Boolean;
begin
  Result := FLibMap.GetLib(AHandle, ALib);
end;

procedure TDbgProcess.UpdateBreakpointsForLibraryLoaded(ALib: TDbgLibrary);
var
  i: Integer;
begin
  if (ALib.DbgInfo.HasInfo) or (ALib.SymbolTableInfo.HasInfo) then begin
    debugln(DBG_VERBOSE and (ALib.FBreakUpdateDone), ['TDbgProcess.UpdateBreakpointsForLibraryLoaded: Called twice for ', ALib.Name]);
    assert(not ALib.FBreakUpdateDone, 'TDbgProcess.UpdateBreakpointsForLibraryLoaded: not ALib.FBreakUpdateDone');
    if ALib.FBreakUpdateDone then
      exit;
    ALib.FBreakUpdateDone := True;
    debuglnEnter(DBG_BREAKPOINTS,['> TDbgProcess.UpdateBreakpointsForLibraryLoaded ',ALib.Name ]); try
    for i := 0 to FBreakpointList.Count - 1 do
      FBreakpointList[i].UpdateForLibraryLoaded(ALib);
    finally debuglnExit(DBG_BREAKPOINTS,['< TDbgProcess.UpdateBreakpointsForLibraryLoaded ' ]); end;
  end;
end;

procedure TDbgProcess.UpdateBreakpointsForLibraryUnloaded(ALib: TDbgLibrary);
var
  i: LongInt;
  b: TFpInternalBreakBase;
begin
  // The library is unloaded by the OS, so all breakpoints are already gone.
  // This is more to update our administration and free some memory.
  debuglnEnter(DBG_BREAKPOINTS, ['> TDbgProcess.UpdateBreakpointsForLibraryUnloaded ' ]); try if ALib <> nil then debugln(DBG_BREAKPOINTS, [ALib.Name]);
  i := FBreakpointList.Count - 1;
  while i >= 0 do begin
    b := FBreakpointList[i];
    b.UpdateForLibrareUnloaded(ALib);
    dec(i);
  end;
  i := FWatchPointList.Count - 1;
  while i >= 0 do begin
    b := FWatchPointList[i];
    b.UpdateForLibrareUnloaded(ALib);
    dec(i);
  end;
  finally debuglnExit(DBG_BREAKPOINTS,['< TDbgProcess.UpdateBreakpointsForLibraryUnloaded ' ]); end;
end;

function TDbgProcess.GetThread(const AID: Integer; out AThread: TDbgThread): Boolean;
var
  Thread: TDbgThread;
begin
  AThread := nil;
  Result := FThreadMap.GetData(AID, Thread) and (Thread <> nil);
  if Result
  then AThread := Thread;
end;

function TDbgProcess.ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean;
begin
  result := false
end;

function TDbgProcess.ReadData(const AAdress: TDbgPtr; const ASize: Cardinal;
  out AData; out APartSize: Cardinal): Boolean;
var
  SizeRemaining, sz: Cardinal;
  Offs: Integer;
  APartAddr: TDBGPtr;
  Dummy: QWord;
begin
  // subclasses can do better implementation if checking for error reasons, such as part_read
  APartSize := ASize;
  Result := ReadData(AAdress, APartSize, AData);
  if Result then
    exit;

  SizeRemaining := ASize;
  Offs := 0;
  APartAddr := AAdress;
  APartSize := 0;

  // check if the address is readable at all
  Result := ReadData(AAdress, 1, Dummy);
  if not Result then
    exit;

  while SizeRemaining > 0 do begin
    Result := False;
    sz := SizeRemaining;
    while (not Result) and (sz > 1) do begin
      sz := sz div 2;
      Result := ReadData(APartAddr, sz, (@AData + Offs)^);
    end;
    if not Result then
      break;

    APartSize := APartSize + sz;
    Offs := Offs + sz;
    APartAddr := APartAddr + sz;
    SizeRemaining := SizeRemaining - sz;
  end;

  Result := APartSize > 0;
end;

function TDbgProcess.ReadAddress(const AAdress: TDbgPtr; out AData: TDBGPtr): Boolean;
var
  dw: DWord;
  qw: QWord;
begin
  case Mode of
    dm32:
      begin
        result := ReadData(AAdress, sizeof(dw), dw);
        AData:=dw;
      end;
    dm64:
      begin
        result := ReadData(AAdress, sizeof(qw), qw);
        AData:=qw;
      end;
  end;
end;

function TDbgProcess.ReadOrdinal(const AAdress: TDbgPtr; out AData): Boolean;
begin
  Result := ReadData(AAdress, 4, AData);
end;

function TDbgProcess.ReadString(const AAdress: TDbgPtr; const AMaxSize: Cardinal; out AData: String): Boolean;
begin
  Result := false;
end;

function TDbgProcess.ReadWString(const AAdress: TDbgPtr; const AMaxSize: Cardinal; out AData: WideString): Boolean;
begin
  result := false;
end;

function TDbgProcess.CallParamDefaultLocation(AParamIdx: Integer
  ): TFpDbgMemLocation;
begin
  Result := InvalidLoc;
end;

function TDbgProcess.Continue(AProcess: TDbgProcess; AThread: TDbgThread;
  SingleStep: boolean): boolean;
begin
  result := false;
end;

function TDbgProcess.ResolveDebugEvent(AThread: TDbgThread): TFPDEvent;
var
  CurrentAddr: TDBGPtr;
begin
  if AThread <> nil then
    AThread.ValidateRemovedBreakPointInfo;
  result := AnalyseDebugEvent(AThread);

  if (result = deBreakpoint) and (AThread <> nil) then
  begin
    // Determine the address where the execution has stopped
    CurrentAddr:=AThread.GetInstructionPointerRegisterValue;
    FCurrentWatchpoint:=AThread.DetectHardwareWatchpoint;
    if (FCurrentWatchpoint <> nil) and (FWatchPointList.IndexOf(TFpInternalWatchpoint(FCurrentWatchpoint)) < 0) then
      FCurrentWatchpoint := Pointer(-1);
    FCurrentBreakpoint:=nil;
    FCurrentBreakpointList := nil;
    AThread.NextIsSingleStep:=false;

    // Whatever reason there was to change the result to deInternalContinue,
    // if a breakpoint has been hit, always trigger it...
    if DoBreak(CurrentAddr, AThread.ID) then
      result := deBreakpoint;
  end
end;

function TDbgProcess.CheckForConsoleOutput(ATimeOutMs: integer): integer;
begin
  result := -1;
end;

function TDbgProcess.GetConsoleOutput: string;
begin
  result := '';
end;

procedure TDbgProcess.SendConsoleInput(AString: string);
begin
  // Do nothing
end;

procedure TDbgProcess.ClearAddedAndRemovedLibraries;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadIdNotMain('ClearAddedAndRemovedLibraries');{$ENDIF}
  FLibMap.ClearAddedAndRemovedLibraries;
end;

procedure TDbgProcess.DoBeforeProcessLoop;
var
  t: TDbgThread;
begin
  ClearAddedAndRemovedLibraries;
  FGlobalCache.Clear;

  for t in FThreadMap do
    t.DoBeforeProcessLoop;
end;

function TDbgProcess.AddThread(AThreadIdentifier: THandle): TDbgThread;
var
  IsMainThread: boolean;
begin
  result := CreateThread(AthreadIdentifier, IsMainThread);
  if assigned(result) then
  begin
    FThreadMap.Add(AThreadIdentifier, Result);
    if IsMainThread then
    begin
      assert(FMainThread=nil);
      FMainThread := result;
    end;
    Result.ApplyWatchPoints(FWatchPointData);
  end
  else
    DebugLn(DBG_WARNINGS, 'Unknown thread ID %u for process %u', [AThreadIdentifier, ProcessID]);
end;

function TDbgProcess.GetThreadArray: TFPDThreadArray;
var
  Iterator: TMapIterator;
  Thread: TDbgThread;
  I: Integer;
begin
  SetLength(Result, FThreadMap.Count);
  Iterator := TMapIterator.Create(FThreadMap);
  try
    Iterator.First;
    I := 0;
    while not Iterator.EOM do
    begin
      Iterator.GetData(Thread);
      Result[I] := Thread;
      Inc(I);
      iterator.Next;
    end;
  finally
    Iterator.Free;
  end;
end;

procedure TDbgProcess.ThreadsBeforeContinue;
var
  Iterator: TMapIterator;
  Thread: TDbgThread;
begin
  Iterator := TLockedMapIterator.Create(FThreadMap);
  try
    Iterator.First;
    while not Iterator.EOM do
    begin
      Iterator.GetData(Thread);
      if FWatchPointData.Changed then
        Thread.ApplyWatchPoints(FWatchPointData);
      Thread.BeforeContinue;
      iterator.Next;
    end;
  finally
    Iterator.Free;
  end;
  FWatchPointData.Changed := False;
end;

procedure TDbgProcess.ThreadsClearCallStack;
var
  Iterator: TMapIterator;
  Thread: TDbgThread;
begin
  GlobalCache.Clear;
  Iterator := TLockedMapIterator.Create(FThreadMap);
  try
    Iterator.First;
    while not Iterator.EOM do
    begin
      Iterator.GetData(Thread);
      Thread.ClearCallStack;
      iterator.Next;
    end;
  finally
    Iterator.Free;
  end;
end;

procedure TDbgProcess.RemoveBreak(const ABreakPoint: TFpDbgBreakpoint);
var
  i: SizeInt;
begin
  if ABreakPoint=FCurrentBreakpoint then begin
    FCurrentBreakpoint := nil;
    if Length(FCurrentBreakpointList) > 0 then begin
      FCurrentBreakpoint := FCurrentBreakpointList[0];
      SetLength(FCurrentBreakpointList, Length(FCurrentBreakpointList) - 1);
    end;
  end;

  i := Length(FCurrentBreakpointList) - 1;
  while (i >= 0) and (FCurrentBreakpointList[i] <> ABreakPoint) do
    dec(i);
  if i >= 0 then begin
    while i < Length(FCurrentBreakpointList) - 2 do begin
      FCurrentBreakpointList[i] := FCurrentBreakpointList[i+1];
      inc(i);
    end;
    SetLength(FCurrentBreakpointList, Length(FCurrentBreakpointList) - 1);
  end;
end;

procedure TDbgProcess.DoBeforeBreakLocationMapChange;
var
  t: TDbgThread;
begin
  for t in FThreadMap do
    t.DoBeforeBreakLocationMapChange;
end;

function TDbgProcess.HasBreak(const ALocation: TDbgPtr): Boolean;
begin
  if FBreakMap = nil then
    Result := False
  else
    result := FBreakMap.HasId(ALocation);
end;

procedure TDbgProcess.RemoveThread(const AID: DWord);
begin
  if FThreadMap = nil then Exit;
  FThreadMap.Delete(AID);
end;

function TDbgProcess.FormatAddress(const AAddress): String;
begin
  Result := HexValue(AAddress, DBGPTRSIZE[Mode], [hvfIncludeHexchar]);
end;

function TDbgProcess.Pause: boolean;
begin
  result := false;
end;

function TDbgProcess.GetHandle: THandle;
begin
  result := 0;
end;

procedure TDbgProcess.SetThreadId(AThreadId: Integer);
begin
  assert(FThreadID = 0, 'TDbgProcess.SetThreadId: FThreadID = 0');
  FThreadID := AThreadId;
end;

procedure TDbgProcess.SetExitCode(AValue: DWord);
begin
  FExitCode:=AValue;
end;

class function TDbgProcess.isSupported(ATargetInfo: TTargetDescriptor): boolean;
begin
  result := false;
end;

procedure TDbgProcess.ThreadDestroyed(const AThread: TDbgThread);
begin
  if AThread = FMainThread
  then FMainThread := nil;
end;

function TDbgProcess.GetPauseRequested: boolean;
begin
  Result := Boolean(InterLockedExchangeAdd(FPauseRequested, 0));
end;

function TDbgProcess.GetRequiresExecutionInDebuggerThread: boolean;
begin
  Result := False;
end;

function TDbgProcess.GetDisassembler: TDbgAsmDecoder;
begin
  if FDisassembler = nil then
    FDisassembler := OSDbgClasses.DbgDisassemblerClass.Create(Self);
  Result := FDisassembler;
end;

function TDbgProcess.DoGetCfiFrameBase(AContext: TFpDbgLocationContext; out AnError: TFpError
  ): TDBGPtr;
var
  Thrd: TDbgThread;
  CStck: TDbgCallstackEntry;
  CIE: TDwarfCIE;
  ROW: TDwarfCallFrameInformationRow;
begin
  Result := 0;
  AnError := nil;
  if (not GetThread(AContext.ThreadId, Thrd)) or (Thrd = nil) then
    exit;
  if AContext.StackFrame >= Thrd.CallStackEntryList.Count then
    exit;
  CStck := Thrd.CallStackEntryList[AContext.StackFrame];
  if CStck = nil then
    exit;

  if not FindCallFrameInfo(AContext.Address, CIE, ROW) then
    exit;

  if not GetCanonicalFrameAddress(CStck.RegisterValueList ,ROW, Result) then
    Result := 0;
end;

function TDbgProcess.DoGetFrameBase(AContext: TFpDbgLocationContext; out AnError: TFpError
  ): TDBGPtr;
var
  Thrd: TDbgThread;
  CStck: TDbgCallstackEntry;
  p: TFpSymbol;
begin
  Result := 0;
  AnError := nil;
  if (not GetThread(AContext.ThreadId, Thrd)) or (Thrd = nil) then
    exit;
  if AContext.StackFrame >= Thrd.CallStackEntryList.Count then
    exit;
  CStck := Thrd.CallStackEntryList[AContext.StackFrame];
  if CStck = nil then
    exit;

  p := CStck.ProcSymbol;
  if p =nil then
    exit;

  if p is TFpSymbolDwarfDataProc then
    Result := TFpSymbolDwarfDataProc(p).GetFrameBase(AContext, AnError);
end;

function TDbgProcess.GetLastLibrariesLoaded: TDbgLibraryArr;
begin
  Result := FLibMap.FLibrariesAdded;
end;

function TDbgProcess.GetLastLibrariesUnloaded: TDbgLibraryArr;
begin
  Result := FLibMap.FLibrariesRemoved;
end;

function TDbgProcess.GetAndClearPauseRequested: Boolean;
begin
  Result := Boolean(InterLockedExchange(FPauseRequested, ord(False)));
end;

procedure TDbgProcess.SetPauseRequested(AValue: boolean);
begin
  InterLockedExchange(FPauseRequested, ord(AValue));
end;

procedure TDbgProcess.LoadInfo;
begin
  inherited LoadInfo;

  if DbgInfo.HasInfo then
    FSymInstances.Add(Self);
end;

procedure TDbgProcess.InitializeLoaders;
begin
  inherited InitializeLoaders;
end;

function TDbgProcess.GetLastEventProcessIdentifier: THandle;
begin
  result := 0;
end;

function TDbgProcess.DoBreak(BreakpointAddress: TDBGPtr; AThreadID: integer): Boolean;
var
  BList: TFpInternalBreakpointArray;
  i, xtra: Integer;
begin
  Result := False;

  BList := FBreakMap.GetInternalBreaksAtLocation(BreakpointAddress);
  if BList = nil then exit;
  i := 0;
  FCurrentBreakpoint := nil;
  SetLength(FCurrentBreakpointList, Length(BList));
  xtra := 0;
  for i := 0 to Length(BList) - 1 do begin
    if not BList[0].FInternal then begin
      BList[i].Hit(AThreadId, BreakpointAddress);
      if (FCurrentBreakpoint = nil) then begin
        FCurrentBreakpoint := BList[i];
      end
      else begin
        FCurrentBreakpointList[xtra] := BList[i];
        inc(xtra);
        BList[i].Hit(AThreadId, BreakpointAddress);
      end;
    end;
  end;

  SetLength(FCurrentBreakpointList, xtra);
  Result := (FCurrentBreakpoint <> nil);
end;

procedure TDbgProcess.SetLastLibraryUnloaded(ALib: TDbgLibrary);
begin
  if FLastLibraryUnloaded <> nil then
    FLastLibraryUnloaded.Destroy;
  FLastLibraryUnloaded := ALib;
end;

procedure TDbgProcess.SetLastLibraryUnloadedNil(ALib: TDbgLibrary);
begin
  assert(ALib = nil, 'TDbgProcess.SetLastLibraryUnloadedNil: ALib = nil');
  SetLastLibraryUnloaded(nil);
end;

procedure TDbgProcess.AddLibrary(ALib: TDbgLibrary; AnID: TDbgPtr);
begin
  if FLibMap.HasId(AnID) then begin
    debugln(DBG_VERBOSE or DBG_WARNINGS, ['Error: Attempt to add duplicate library ', AnID]);
    exit;
  end;
  FLibMap.Add(AnID, ALib);

  if (ALib.DbgInfo.HasInfo) or (ALib.SymbolTableInfo.HasInfo) then
    FSymInstances.Add(ALib);
end;

procedure TDbgProcess.RemoveAllBreakPoints;
var
  i: LongInt;
  b: TFpInternalBreakBase;
begin
  i := FBreakpointList.Count - 1;
  while i >= 0 do begin
    b := FBreakpointList[i];
    b.ResetBreak;
    b.SetProcessToNil;
    FBreakpointList.Delete(i);
    dec(i);
  end;
  i := FWatchPointList.Count - 1;
  while i >= 0 do begin
    b := FWatchPointList[i];
    b.ResetBreak;
    b.SetProcessToNil;
    FWatchPointList.Delete(i);
    dec(i);
  end;
end;

procedure TDbgProcess.BeforeChangingInstructionCode(const ALocation: TDBGPtr; ACount: Integer);
begin
  //
end;

procedure TDbgProcess.AfterChangingInstructionCode(const ALocation: TDBGPtr; ACount: Integer);
begin
  //
end;

procedure TDbgProcess.AfterBreakpointAdded(ABreak: TFpDbgBreakpoint);
begin
  if (FMainThread <> nil) and not assigned(FCurrentBreakpoint) then begin
    // TODO: what if there is a hardcoded int3?
    if ABreak.HasLocation(FMainThread.GetInstructionPointerRegisterValue) then
      FCurrentBreakpoint := TFpInternalBreakpoint(ABreak);
  end;
end;

//function TDbgProcess.LocationIsBreakInstructionCode(const ALocation: TDBGPtr
//  ): Boolean;
//var
//  OVal: Byte;
//begin
//  Result := FBreakMap.HasId(ALocation);
//  if not Result then
//    exit;
//
//  Result := FProcess.ReadData(ALocation, 1, OVal);
//  if Result then
//    Result := OVal = Int3
//  else
//    DebugLn(DBG_WARNINGS or DBG_BREAKPOINTS'Unable to read pre-breakpoint at '+FormatAddress(ALocation));
//end;

procedure TDbgProcess.TempRemoveBreakInstructionCode(const ALocation: TDBGPtr);
begin
  FBreakTargetHandler.TempRemoveBreakInstructionCode(ALocation);
end;

procedure TDbgProcess.RestoreTempBreakInstructionCodes;
begin
  FBreakTargetHandler.RestoreTempBreakInstructionCodes;
end;

function TDbgProcess.HasInsertedBreakInstructionAtLocation(
  const ALocation: TDBGPtr): Boolean;
begin
  Result := FBreakMap.HasInsertedBreakInstructionAtLocation(ALocation);
end;

function TDbgProcess.CanContinueForWatchEval(ACurrentThread: TDbgThread
  ): boolean;
begin
  Result := True;
end;

procedure TDbgProcess.MaskBreakpointsInReadData(const AAdress: TDbgPtr; const ASize: Cardinal; var AData);
begin
  if FBreakTargetHandler <> nil then
    FBreakTargetHandler.MaskBreakpointsInReadData(AAdress, ASize, AData);
end;

function TDbgProcess.CreateWatchPointData: TFpWatchPointData;
begin
  Result := TFpWatchPointData.Create;
end;

procedure TDbgProcess.Init(const AProcessID, AThreadID: Integer);
begin
  FProcessID := AProcessID;
  FThreadID := AThreadID;
end;

function TDbgProcess.CreateConfig: TDbgConfig;
begin
  Result := TDbgConfig.Create;
end;

function TDbgProcess.WriteData(const AAdress: TDbgPtr; const ASize: Cardinal; const AData): Boolean;
begin
  result := false;
end;

function TDbgProcess.Detach(AProcess: TDbgProcess; AThread: TDbgThread
  ): boolean;
begin
  Result := False;
end;

function TDbgProcess.WriteInstructionCode(const AAdress: TDbgPtr; const ASize: Cardinal; const AData): Boolean;
begin
  FBreakTargetHandler.UpdateMapForNewTargetCode(AAdress, ASize, AData);
  BeforeChangingInstructionCode(AAdress, ASize);
  Result := WriteData(AAdress, ASize, AData);
  AfterChangingInstructionCode(AAdress, ASize);
end;


{ TDbgStackFrameInfo }

procedure TDbgStackFrameInfo.DoAfterRun;
var
  CurStackFrame: TDBGPtr;
begin
  FProcessAfterRun := False;
  case FLeaveState of
    lsWasAtLeave1: begin
        CurStackFrame   := FThread.GetStackBasePointerRegisterValue;
        FStoredStackPointer := FThread.GetStackPointerRegisterValue;
        if CurStackFrame <> FStoredStackFrame then
          FLeaveState := lsLeaveDone // real leave
        else
          FLeaveState := lsWasAtLeave2; // lea rsp,[rbp+$00] / pop ebp // epb in next command
      end;
    lsWasAtLeave2: begin
        // TODO: maybe check, if stackpointer only goes down by sizeof(pointer) "Pop bp"
        FStoredStackFrame   := FThread.GetStackBasePointerRegisterValue;
        FStoredStackPointer := FThread.GetStackPointerRegisterValue;
        FLeaveState := lsLeaveDone;
      end;
  end;
end;

procedure TDbgStackFrameInfo.DoCheckNextInstruction(
  ANextInstruction: TDbgAsmInstruction; NextIsSingleStep: Boolean);
begin
  if FProcessAfterRun then
    DoAfterRun;

  if not NextIsSingleStep then begin
    if FLeaveState = lsWasAtLeave2 then
      FLeaveState := lsLeaveDone;
    exit;
  end;

  if ANextInstruction.IsReturnInstruction then begin
    FHasSteppedOut := True;
    FLeaveState := lsLeaveDone;
  end
  else if FLeaveState = lsNone then begin
    if ANextInstruction.IsLeaveStackFrame then
      FLeaveState := lsWasAtLeave1;
  end;

  FProcessAfterRun := FLeaveState in [lsWasAtLeave1, lsWasAtLeave2];
end;

function TDbgStackFrameInfo.CalculateHasSteppedOut: Boolean;
var
  CurBp, CurSp: TDBGPtr;
begin
  if FProcessAfterRun then
    DoAfterRun;

  Result := False;
  CurBp := FThread.GetStackBasePointerRegisterValue;
  if FStoredStackFrame < CurBp then begin
    CurSp := FThread.GetStackPointerRegisterValue;
    if FStoredStackPointer >= CurSp then // this happens, if current was recorded before the BP frame was set up // a finally handle may then fake an outer frame
      exit;
//    {$PUSH}{$Q-}{$R-}
//    if CurSp = FStoredStackPointer + FThread.Process.PointerSize then
//      exit; // Still in proc, but passed asm "leave" (BP has been popped, but IP not yet)
//    {$POP}
    Result := True;
    debugln(FPDBG_COMMANDS, ['BreakStepBaseCmd.GetIsSteppedOut: Has stepped out Stored-BP=', FStoredStackFrame, ' < BP=', CurBp, ' / SP', CurSp]);
  end;
end;

constructor TDbgStackFrameInfo.Create(AThread: TDbgThread);
begin
  FThread := AThread;
  FStoredStackFrame   := AThread.GetStackBasePointerRegisterValue;
  FStoredStackPointer := AThread.GetStackPointerRegisterValue;
end;

procedure TDbgStackFrameInfo.CheckNextInstruction(
  ANextInstruction: TDbgAsmInstruction; NextIsSingleStep: Boolean);
begin
  if not FHasSteppedOut then
    DoCheckNextInstruction(ANextInstruction, NextIsSingleStep);
end;

function TDbgStackFrameInfo.HasSteppedOut: Boolean;
begin
  Result := FHasSteppedOut;
  if Result then
    exit;
  FHasSteppedOut := CalculateHasSteppedOut;
  Result := FHasSteppedOut;
end;

procedure TDbgStackFrameInfo.FlagAsSteppedOut;
begin
  FHasSteppedOut := True;
end;

{ TDbgStackUnwinderX86Base }

constructor TDbgStackUnwinderX86Base.Create(AProcess: TDbgProcess);
begin
  FProcess := AProcess;
  case AProcess.Mode of
    dm32: begin
      FAddressSize := 4;
      FDwarfNumIP := 8; // Dwarf Reg Num EIP
      FDwarfNumBP := 5; // EBP
      FDwarfNumSP := 4; // ESP
      FNameIP := 'eip';
      FNameBP := 'ebp';
      FNameSP := 'esp';
    end;
    dm64: begin
      FAddressSize := 8;
      FDwarfNumIP := 16; // Dwarf Reg Num RIP
      FDwarfNumBP := 6; // RBP
      FDwarfNumSP := 7; // RSP
      FNameIP := 'rip';
      FNameBP := 'rbp';
      FNameSP := 'rsp';
    end;
  end;
end;

procedure TDbgStackUnwinderX86Base.InitForThread(AThread: TDbgThread);
begin
  FThread := AThread;
end;

procedure TDbgStackUnwinderX86Base.InitForFrame(
  ACurrentFrame: TDbgCallstackEntry; out CodePointer, StackPointer,
  FrameBasePointer: TDBGPtr);
var
  R: TDbgRegisterValue;
begin
    CodePointer      := ACurrentFrame.AnAddress;
    FrameBasePointer := ACurrentFrame.FrameAdress;
    R := ACurrentFrame.RegisterValueList.FindRegisterByDwarfIndex(FDwarfNumBP);
    if R <> nil then
      FrameBasePointer := R.NumValue;
    StackPointer     := 0;
    R := ACurrentFrame.RegisterValueList.FindRegisterByDwarfIndex(FDwarfNumSP);
    if R = nil then exit;
    StackPointer := R.NumValue;
end;

procedure TDbgStackUnwinderX86Base.GetTopFrame(out CodePointer, StackPointer,
  FrameBasePointer: TDBGPtr; out ANewFrame: TDbgCallstackEntry);
begin
  CodePointer      := Thread.GetInstructionPointerRegisterValue;
  StackPointer     := Thread.GetStackPointerRegisterValue;
  FrameBasePointer := Thread.GetStackBasePointerRegisterValue;
  ANewFrame        := TDbgCallstackEntry.create(Thread, 0, FrameBasePointer, CodePointer);
  ANewFrame.AutoFillRegisters := True;
end;

{ TDbgThread }

function TDbgThread.GetRegisterValueList: TDbgRegisterValueList;
begin
  if not FRegisterValueListValid then
    LoadRegisterValues;
  result := FRegisterValueList;
end;

function TDbgThread.CompareStepInfo(AnAddr: TDBGPtr; ASubLine: Boolean
  ): TFPDCompareStepInfo;
var
  Sym: TFpSymbol;
  l: TDBGPtr;
begin
  if FStoreStepSrcLineNo = -1 then begin // stepping from location with no line info
    Result := dcsiNewLine;
    exit;
  end;

  if AnAddr = 0 then
    AnAddr := GetInstructionPointerRegisterValue;

  if (FStoreStepStartAddr <> 0) then begin
    if (AnAddr > FStoreStepStartAddr) and (AnAddr < FStoreStepEndAddr)
    then begin
      result := dcsiSameLine;
      exit;
    end
    else
    if ASubLine then begin
      // this is used for the (unmarked) proloque of finally handlers in 3.1.1
      result := dcsiNewLine; // may have the same line number, but has a new address block
      exit;
    end;
  end;

  sym := FProcess.FindProcSymbol(AnAddr);
  if assigned(sym) then
  begin
    if sym is TFpSymbolDwarfDataProc then
      l := TFpSymbolDwarfDataProc(sym).LineUnfixed
    else
      l := Sym.Line;
    debugln(FPDBG_COMMANDS, ['CompareStepInfo @IP=',AnAddr,' ',sym.FileName, ':',l, ' in ',sym.Name, ' @Func=',sym.Address.Address]);
    if (((FStoreStepSrcFilename=sym.FileName) and (FStoreStepSrcLineNo=l)) {or FStepOut}) then
      result := dcsiSameLine
    else if sym.FileName = '' then
      result := dcsiNoLineInfo
    else if l = 0 then
      result := dcsiZeroLine
    else
      result := dcsiNewLine;
    sym.ReleaseReference;
  end
  else
    result := dcsiNoLineInfo;
end;

function TDbgThread.IsAtStartOfLine: boolean;
var
  AnAddr, b: TDBGPtr;
  Sym: TFpSymbol;
  CU: TDwarfCompilationUnit;
  a: TDBGPtrArray;
begin
  AnAddr := GetInstructionPointerRegisterValue;
  sym := FProcess.FindProcSymbol(AnAddr);
  if (sym is TDbgDwarfSymbolBase) then
  begin
    CU := TDbgDwarfSymbolBase(sym).CompilationUnit;
    Result := False;
    CU.Owner.GetLineAddresses(sym.FileName, sym.Line, a);
    for b in a do begin
      Result := b = AnAddr;
      if Result then break;
    end;
  end
  else
    Result := True;
  sym.ReleaseReference;
end;

function TDbgThread.StoreStepInfo(AnAddr: TDBGPtr): boolean;
var
  Sym: TFpSymbol;
begin
  if AnAddr = 0 then
    AnAddr := GetInstructionPointerRegisterValue;
  sym := FProcess.FindProcSymbol(AnAddr);
  FStoreStepStartAddr := AnAddr;
  FStoreStepEndAddr := AnAddr;
  FStoreStepFuncAddr:=0;
  if assigned(sym) then
  begin
    FStoreStepSrcFilename:=sym.FileName;
    FStoreStepFuncAddr:=sym.Address.Address;
    FStoreStepFuncName:=sym.Name;
    if sfHasLineAddrRng in sym.Flags then begin
      FStoreStepStartAddr := sym.LineStartAddress;
      FStoreStepEndAddr := sym.LineEndAddress;
    end;
    if sym is TFpSymbolDwarfDataProc then begin
      FStoreStepSrcLineNo := TFpSymbolDwarfDataProc(sym).LineUnfixed;
    end
    else
      FStoreStepSrcLineNo:=sym.Line;
    debugln(FPDBG_COMMANDS, ['StoreStepInfo @IP=',AnAddr,' ',sym.FileName, ':',FStoreStepSrcLineNo, ' in ',sym.Name, ' @Func=',sym.Address.Address]);
    sym.ReleaseReference;
  end
  else begin
    debugln(FPDBG_COMMANDS, ['StoreStepInfo @IP=',AnAddr,' - No symbol']);
    FStoreStepSrcLineNo:=-1;
  end;
  Result := (FStoreStepSrcFilename <> '') or (FStoreStepSrcLineNo > 0);
end;

procedure TDbgThread.LoadRegisterValues;
begin
  // Do nothing
end;

procedure TDbgThread.StoreHasBreakpointInfoForAddress(AnAddr: TDBGPtr);
begin
  // Already stored?
  if (FStoredBreakpointInfoState <> rbUnknown) and
     (FStoredBreakpointInfoAddress = AnAddr)
  then
    exit;

  if (AnAddr <> 0) and Process.HasInsertedBreakInstructionAtLocation(AnAddr) then begin
    (* There is a chance, that the code jumped to this Addr, instead of executing the breakpoint.
       But if the next signal for this thread is a breakpoint at this address, then
       it must be handled (even if the breakpoint has been removed since)
    *)
    FStoredBreakpointInfoAddress := AnAddr;
    FStoredBreakpointInfoState := rbFound;
    // Most likely the debugger should see the previous address (unless we got here
    // by jump.
    // Call something like ResetInstructionPointerAfterBreakpointForPendingSignal; virtual;
    ////ResetInstructionPointerAfterBreakpoint;
  end
  else
    FStoredBreakpointInfoState := rbNone;
end;

procedure TDbgThread.ClearHasBreakpointInfoForAddressMismatch(AKeepOnlyForAddr: TDBGPtr);
begin
  if (FStoredBreakpointInfoState <> rbUnknown) and
     (FStoredBreakpointInfoAddress <> AKeepOnlyForAddr)
  then begin
    FStoredBreakpointInfoAddress := 0;
    FStoredBreakpointInfoState := rbUnknown;
  end;
end;

procedure TDbgThread.ClearHasBreakpointInfoForAddress;
begin
  FStoredBreakpointInfoAddress := 0;
  FStoredBreakpointInfoState := rbUnknown;
end;

function TDbgThread.HasBreakpointInfoForAddressMismatch(AnAddr: TDBGPtr
  ): boolean;
begin
  Result := ( (FStoredBreakpointInfoState = rbFound) and
              (FStoredBreakpointInfoAddress = AnAddr) );
end;

procedure TDbgThread.DoBeforeBreakLocationMapChange;
begin
  StoreHasBreakpointInfoForAddress(GetInstructionPointerForHasBreakpointInfoForAddress);
end;

procedure TDbgThread.ValidateRemovedBreakPointInfo;
begin
  if (FStoredBreakpointInfoState <> rbUnknown) then
    ClearHasBreakpointInfoForAddressMismatch(GetInstructionPointerForHasBreakpointInfoForAddress);
end;

function TDbgThread.GetName: String;
begin
  Result := '';
end;

function TDbgThread.GetInstructionPointerForHasBreakpointInfoForAddress: TDBGPtr;
begin
  Result := GetInstructionPointerRegisterValue;
end;

constructor TDbgThread.Create(const AProcess: TDbgProcess; const AID: Integer; const AHandle: THandle);
begin
  FID := AID;
  FHandle := AHandle;
  FProcess := AProcess;
  FRegisterValueList:=TDbgRegisterValueList.Create;
  FPreviousRegisterValueList:=TDbgRegisterValueList.Create;
  inherited Create;
end;

procedure TDbgThread.DoBeforeProcessLoop;
begin
  FPreviousRegisterValueList.Assign(FRegisterValueList);
  if FRegisterValueListValid then
    FRegisterValueList.FPreviousRegisterValueList := FPreviousRegisterValueList
  else
    FRegisterValueList.FPreviousRegisterValueList := nil;
  FRegisterValueListValid:=false;
end;

function TDbgThread.HasInsertedBreakInstructionAtLocation(const ALocation: TDBGPtr): Boolean;
begin
  Result := HasBreakpointInfoForAddressMismatch(ALocation) or
            ( (ALocation <> 0) and Process.HasInsertedBreakInstructionAtLocation(ALocation) );
end;

procedure TDbgThread.CheckAndResetInstructionPointerAfterBreakpoint;
var
  t: TDBGPtr;
begin
  // todo: check that the breakpoint is NOT in the temp removed list
  t := GetInstructionPointerForHasBreakpointInfoForAddress;
  if t = 0 then
    exit;
  if HasInsertedBreakInstructionAtLocation(t)
  then begin
    FStoredBreakpointInfoState := rbFound;
    ResetInstructionPointerAfterBreakpoint;
  end
  else begin
    // TODO: allow to skip this, while detaching
    FPausedAtHardcodeBreakPoint := Process.FBreakTargetHandler.IsHardcodeBreakPointInCode(t);
  end;
end;

function TDbgThread.CheckForHardcodeBreakPoint(AnAddr: TDBGPtr): boolean;
begin
  Result := False;
  if AnAddr = 0 then
    exit;
  FPausedAtHardcodeBreakPoint := Process.FBreakTargetHandler.IsHardcodeBreakPointInCode(AnAddr);
  Result := FPausedAtHardcodeBreakPoint;
end;

procedure TDbgThread.BeforeContinue;
begin
  FPausedAtHardcodeBreakPoint := False;
  ClearHasBreakpointInfoForAddress;
end;

procedure TDbgThread.ApplyWatchPoints(AWatchPointData: TFpWatchPointData);
begin
  //
end;

function TDbgThread.DetectHardwareWatchpoint: Pointer;
begin
  result := nil;
end;

function TDbgThread.GetCurrentStackFrameInfo: TDbgStackFrameInfo;
begin
  Result := TDbgStackFrameInfo.Create(Self);
end;

function TDbgThread.AllocStackMem(ASize: Integer): TDbgPtr;
begin
  Result := GetStackPointerRegisterValue;
  if FStackBeforeAlloc = 0 then
    FStackBeforeAlloc := Result;
  dec(Result, ASize);
  SetStackPointerRegisterValue(Result);
end;

procedure TDbgThread.RestoreStackMem;
begin
  if FStackBeforeAlloc <> 0 then
    SetStackPointerRegisterValue(FStackBeforeAlloc);
  FStackBeforeAlloc := 0;
end;

procedure TDbgThread.PrepareCallStackEntryList(AFrameRequired: Integer);
const
  MAX_FRAMES = 150000; // safety net
var
  Address, FrameBase, StackPtr: TDBGPtr;
  CountNeeded: integer;
  AnEntry: TDbgCallstackEntry;
  NextIdx: LongInt;
  Unwinder: TDbgStackUnwinder;
  Res: TTDbgStackUnwindResult;
begin
  // TODO: use AFrameRequired // check if already partly done
  if FCallStackEntryList = nil then begin
    FCallStackEntryList := TDbgCallstackEntryList.Create;
    FCallStackEntryList.FreeObjects:=true;
  end;
  if AFrameRequired = -2 then
    exit;

  if (AFrameRequired >= 0) and (AFrameRequired < FCallStackEntryList.Count) then
    exit;

  Unwinder := GetStackUnwinder;
  Unwinder.InitForThread(Self);

  if FCallStackEntryList.Count = 0 then begin
    Unwinder.GetTopFrame(Address, StackPtr, FrameBase, AnEntry);
    FCallStackEntryList.Add(AnEntry);
  end
  else begin
    AnEntry := FCallStackEntryList[FCallStackEntryList.Count - 1];
    Unwinder.InitForFrame(AnEntry, Address, StackPtr, FrameBase);
  end;

  NextIdx := FCallStackEntryList.Count;

  if AFrameRequired < 0 then
    AFrameRequired := MaxInt;
  CountNeeded := AFrameRequired - FCallStackEntryList.Count;
  while (CountNeeded > 0) do
  begin
    Res := Unwinder.Unwind(NextIdx, Address, StackPtr, FrameBase, AnEntry, AnEntry);
    if not (Res in [suSuccess, suGuessed]) then
      break;

    FCallStackEntryList.Add(AnEntry);
    dec(CountNeeded);
    inc(NextIdx);
  end;
  if CountNeeded > 0 then // there was an error / not possible to read more frames
    FCallStackEntryList.SetHasReadAllAvailableFrames;
end;

function TDbgThread.FindCallStackEntryByBasePointer(AFrameBasePointer: TDBGPtr;
  AMaxFrameToSearch: Integer; AStartFrame: integer): Integer;
var
  RegFP: Integer;
  AFrame: TDbgCallstackEntry;
  ARegister: TDbgRegisterValue;
  fp, prev_fp: TDBGPtr;
begin
  if Process.Mode = dm64 then
    RegFP := 6
  else
    RegFP := 5;

  Result := AStartFrame;
  prev_fp := low(prev_fp);
  while Result <= AMaxFrameToSearch do begin
    PrepareCallStackEntryList(Result+1);
    if CallStackEntryList.Count <= Result then
      exit(-1);

    AFrame := CallStackEntryList[Result];
    if AFrame = nil then
      exit(-1);
    ARegister := AFrame.RegisterValueList.FindRegisterByDwarfIndex(RegFP);
    if ARegister = nil then
      exit(-1);

    fp := ARegister.NumValue;

    if fp = AFrameBasePointer then
      exit;

    if (fp < prev_fp) or (fp > AFrameBasePointer) then
      exit(-1);

    prev_fp := fp;
    inc(Result);
  end;
end;

function TDbgThread.FindCallStackEntryByInstructionPointer(
  AInstructionPointer: TDBGPtr; AMaxFrameToSearch: Integer; AStartFrame: integer
  ): Integer;
var
  RegIP: Integer;
  AFrame: TDbgCallstackEntry;
  ARegister: TDbgRegisterValue;
  ip: TDBGPtr;
begin
  if Process.Mode = dm64 then
    RegIP := 16
  else
    RegIP := 8;

  Result := AStartFrame;
  while Result <= AMaxFrameToSearch do begin
    PrepareCallStackEntryList(Result+1);
    if CallStackEntryList.Count <= Result then
      exit(-1);

    AFrame := CallStackEntryList[Result];
    if AFrame = nil then
      exit(-1);
    ARegister := AFrame.RegisterValueList.FindRegisterByDwarfIndex(RegIP);
    if ARegister = nil then
      exit(-1);

    ip := ARegister.NumValue;

    if ip = AInstructionPointer then
      exit;

    inc(Result);
  end;
end;

procedure TDbgThread.ClearCallStack;
begin
  if FCallStackEntryList <> nil then
    FCallStackEntryList.Clear;
end;

destructor TDbgThread.Destroy;
begin
  FProcess.ThreadDestroyed(Self);
  FreeAndNil(FRegisterValueList);
  FreeAndNil(FPreviousRegisterValueList);
  ClearCallStack;
  FreeAndNil(FCallStackEntryList);
  inherited;
end;

procedure TDbgThread.ClearExceptionSignal;
begin
  // To be implemented in sub-classes
end;

procedure TDbgThread.IncSuspendCount;
begin
  inc(FSuspendCount);
end;

procedure TDbgThread.DecSuspendCount;
begin
  dec(FSuspendCount);
  DebugLn((DBG_VERBOSE or DBG_WARNINGS) and (FSuspendCount < 0), ['DecSuspendCount went negative: ', FSuspendCount])
end;

{ TFpWatchPointData }

function TFpWatchPointData.AddOwnedWatchpoint(AnOwner: Pointer;
  AnAddr: TDBGPtr; ASize: Cardinal; AReadWrite: TDBGWatchPointKind): boolean;
begin
  Result := False;
end;

function TFpWatchPointData.RemoveOwnedWatchpoint(AnOwner: Pointer): boolean;
begin
  Result := True;
end;

{ TFpIntelWatchPointData }

function TFpIntelWatchPointData.GetDr03(AnIndex: Integer): TDBGPtr;
begin
  Result := FDr03[AnIndex];
end;

function TFpIntelWatchPointData.GetOwner(AnIndex: Integer): Pointer;
begin
  Result := FOwners[AnIndex];
end;

function TFpIntelWatchPointData.AddOwnedWatchpoint(AnOwner: Pointer;
  AnAddr: TDBGPtr; ASize: Cardinal; AReadWrite: TDBGWatchPointKind): boolean;
var
  SizeBits, ModeBits: DWord;
  idx: Integer;
begin
  Result := False;
  case ASize of
    1: SizeBits := $00000 shl 2;
    2: SizeBits := $10000 shl 2;
    4: SizeBits := $30000 shl 2;
    8: SizeBits := $20000 shl 2; // Only certain cpu / must be 8byte aligned
    else exit;
  end;
  case AReadWrite of
    wpkWrite:     ModeBits := $10000;
    wpkRead:      ModeBits := $30000; // caller must check
    wpkReadWrite: ModeBits := $30000;
    wkpExec:      ModeBits := $00000; // Size must be 1 (SizeBits=0)
  end;

  for idx := 0 to 3 do begin
    if (FDr7 and (1 shl (idx * 2))) = 0 then begin
      FDr7 := FDr7 or (1 shl (idx*2))
                   or (ModeBits shl (idx*4)) // read/write
                   or (SizeBits shl (idx*4)); // size
      FDr03[idx] := AnAddr;
      FOwners[idx] := AnOwner;
      Changed := True;
      Result := True;
      break;
    end;
  end;
end;

function TFpIntelWatchPointData.RemoveOwnedWatchpoint(AnOwner: Pointer
  ): boolean;
var
  idx: Integer;
begin
  Result := False;
  for idx := 0 to 3 do begin
    if FOwners[idx] = AnOwner then begin
      FDr7 := FDr7 and not (
                   (DWord(3) shl (idx*2)) or
                   (DWord($F0000) shl (idx*4))
      );
      FDr03[idx] := 0;
      FOwners[idx] := nil;
      Changed := True;
      Result := True;
    end;
  end;
end;

{ TFpInternalBreakBase }

procedure TFpInternalBreakBase.SetProcessToNil;
begin
  FProcess := nil;
  if FFreeByDbgProcess then
    Destroy;
end;

procedure TFpInternalBreakBase.SetFreeByDbgProcess(AValue: Boolean);
begin
  inherited SetFreeByDbgProcess(AValue);
  if AValue and (FProcess = nil) then
    Destroy;
end;

procedure TFpInternalBreakBase.UpdateForLibraryLoaded(ALib: TDbgLibrary);
begin
  //
end;

procedure TFpInternalBreakBase.UpdateForLibrareUnloaded(ALib: TDbgLibrary);
begin
  //
end;

constructor TFpInternalBreakBase.Create(const AProcess: TDbgProcess);
begin
  inherited Create;
  FProcess := AProcess;
end;

{ TDbgBreak }

procedure TFpInternalBreakpoint.BeginUpdate;
begin
  inc(FUpdateStateLock);
end;

procedure TFpInternalBreakpoint.EndUpdate;
begin
  dec(FUpdateStateLock);
  if (FUpdateStateLock = 0) and FNeedUpdateState then
    TriggerUpdateState;
end;

procedure TFpInternalBreakpoint.TriggerUpdateState;
begin
  FNeedUpdateState := FUpdateStateLock > 0;
  if FNeedUpdateState then
    exit;
  UpdateState;
end;

procedure TFpInternalBreakpoint.AddErrorSetting(ALocation: TDBGPtr);
begin
  inc(FErrorSettingCount);
  TriggerUpdateState;
end;

procedure TFpInternalBreakpoint.RemoveErrorSetting(ALocation: TDBGPtr);
begin
  dec(FErrorSettingCount);
  TriggerUpdateState;
end;

function TFpInternalBreakpoint.GetState: TFpDbgBreakpointState;
begin
  Result := FState;
end;

procedure TFpInternalBreakpoint.SetState(AState: TFpDbgBreakpointState);
begin
  if AState = FState then
    exit;
  FState := AState;
  if FOn_Thread_StateChange <> nil then
    FOn_Thread_StateChange(Self, AState);
end;

procedure TFpInternalBreakpoint.UpdateState;
begin
  if (Length(FLocation) > 0) and (FErrorSettingCount = 0) then
    SetState(bksOk)
  else
    SetState(bksFailed);
end;

procedure TFpInternalBreakpoint.UpdateForLibrareUnloaded(ALib: TDbgLibrary);
var
  i, j: Integer;
  a: TDBGPtr;
begin
  BeginUpdate;
  j := 0;
  for i := 0 to Length(FLocation) - 1 do begin
    a := FLocation[i];
    FLocation[j] := a;
    if ALib.EnclosesAddressRange(a, a) then
      Process.FBreakMap.RemoveLocation(a, Self)
    else
      inc(j);
  end;
  if j < Length(FLocation) then begin
    SetLength(FLocation, j);
    TriggerUpdateState;
  end;
  EndUpdate;
end;

constructor TFpInternalBreakpoint.Create(const AProcess: TDbgProcess;
  const ALocation: TDBGPtrArray; AnEnabled: Boolean);
begin
  inherited Create(AProcess);
  Process.FBreakpointList.Add(Self);
  FLocation := ALocation;
  FEnabled := AnEnabled;
  FState := bksUnknown;
  BeginUpdate;
  if AnEnabled then
    SetBreak;
  TriggerUpdateState;
  EndUpdate;
end;

destructor TFpInternalBreakpoint.Destroy;
begin
  On_Thread_StateChange := nil;
  if Process <> nil then
    Process.FBreakpointList.Remove(Self);
  ResetBreak;
  inherited;
end;

function TFpInternalBreakpoint.Hit(const AThreadID: Integer;
  ABreakpointAddress: TDBGPtr): Boolean;
begin
  Result := False;
  assert(Process<>nil, 'TFpInternalBreakpoint.Hit: Process<>nil');
  if //Process.FBreakMap.HasId(ABreakpointAddress) and
     (Process.FBreakTargetHandler.IsHardcodeBreakPoint(ABreakpointAddress))
  then
    exit; // breakpoint on a hardcoded breakpoint
          // no need to jump back and restore instruction

  Result := true;
end;

function TFpInternalBreakpoint.HasLocation(const ALocation: TDBGPtr): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to High(FLocation) do begin
    if FLocation[i] = ALocation then
      exit;
  end;
  Result := False;
end;

procedure TFpInternalBreakpoint.AddAddress(const ALocation: TDBGPtr);
var
  l: Integer;
begin
  l := Length(FLocation);
  SetLength(FLocation, l+1);
  FLocation[l] := ALocation;
  BeginUpdate;
  if Enabled then
    Process.FBreakMap.AddLocation(ALocation, Self, True);
  TriggerUpdateState;
  EndUpdate;
end;

procedure TFpInternalBreakpoint.AddAddress(const ALocations: TDBGPtrArray);
var
  l, i: Integer;
begin
  l := Length(FLocation);
  SetLength(FLocation, l + Length(ALocations));

  BeginUpdate;
  if Enabled then begin
    for i := 0 to Length(ALocations) - 1 do begin
      FLocation[l + i] := ALocations[i];
      Process.FBreakMap.AddLocation(ALocations[i], Self, True);
    end;
  end
  else begin
    for i := 0 to Length(ALocations) - 1 do
      FLocation[l + i] := ALocations[i];
  end;
  TriggerUpdateState;
  EndUpdate;
end;

procedure TFpInternalBreakpoint.RemoveAddress(const ALocation: TDBGPtr);
var
  l, i: Integer;
begin
  l := Length(FLocation) - 1;
  i := l;
  while (i >= 0) and (FLocation[i] <> ALocation) do
    dec(i);
  if i < 0 then
    exit;
  FLocation[i] := FLocation[l];
  SetLength(FLocation, l);
  BeginUpdate;
  Process.FBreakMap.RemoveLocation(ALocation, Self);
  TriggerUpdateState;
  EndUpdate;
end;

procedure TFpInternalBreakpoint.RemoveAllAddresses;
begin
  BeginUpdate;
  ResetBreak;
  SetLength(FLocation, 0);
  FErrorSettingCount := 0;
  TriggerUpdateState;
  EndUpdate;
end;

procedure TFpInternalBreakpoint.ResetBreak;
var
  i: Integer;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TFpInternalBreakpoint.ResetBreak');{$ENDIF}
  if Process = nil then
    exit;

  FEnabled := False;
  BeginUpdate;
  for i := 0 to High(FLocation) do
    Process.FBreakMap.RemoveLocation(FLocation[i], Self);
  TriggerUpdateState;
  EndUpdate;
end;

procedure TFpInternalBreakpoint.SetBreak;
var
  i: Integer;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TFpInternalBreakpoint.SetBreak');{$ENDIF}
  if Process = nil then
    exit;

  FEnabled := True;
  BeginUpdate;
  for i := 0 to High(FLocation) do
    Process.FBreakMap.AddLocation(FLocation[i], Self, True);
  TriggerUpdateState;
  EndUpdate;
end;

{ TFpInternalBreakpointAtAddress }

procedure TFpInternalBreakpointAtAddress.UpdateState;
begin
  if FErrorSettingCount > 0 then begin
    FRemovedLoc := FLocation;
    SetLength(FRemovedLoc, Length(FRemovedLoc));
    RemoveAllAddresses;
    exit;
  end;

  if (Length(FLocation) > 0) and (FErrorSettingCount = 0) then
    SetState(bksOk)
  else
    SetState(bksPending);
end;

procedure TFpInternalBreakpointAtAddress.UpdateForLibraryLoaded(ALib: TDbgLibrary);
var
  a: TDBGPtrArray;
begin
  if (Length(FRemovedLoc) = 0) or
     (not ALib.EnclosesAddress(FRemovedLoc[0]))
  then
    exit;

  a := FRemovedLoc;
  FRemovedLoc := nil;
  BeginUpdate;
  AddAddress(a);
  Enabled := True; // Must have been enabled when FRemovedLoc was assigned
  EndUpdate;
end;

procedure TFpInternalBreakpointAtAddress.UpdateForLibrareUnloaded(ALib: TDbgLibrary);
begin
  if (Length(FLocation) = 0) or
     (not ALib.EnclosesAddress(FLocation[0]))
  then
    exit;

  FRemovedLoc := FLocation;
  SetLength(FRemovedLoc, Length(FRemovedLoc));
  RemoveAllAddresses;
end;

destructor TFpInternalBreakpointAtAddress.Destroy;
begin
  BeginUpdate; // no need to call EndUpdate
  inherited Destroy;
  FRemovedLoc := nil;
end;

{ TFpInternalBreakpointAtSymbol }

procedure TFpInternalBreakpointAtSymbol.UpdateState;
begin
  if FErrorSettingCount > 0 then
    SetState(bksFailed)
  else
  if Length(FLocation) > 0 then
    SetState(bksOk)
  else
    SetState(bksPending);
end;

procedure TFpInternalBreakpointAtSymbol.UpdateForLibraryLoaded(ALib: TDbgLibrary
  );
var
  a: TDBGPtrArray;
  AProcList: TFpSymbolArray;
  i: Integer;
begin
  if FSymInstance <> nil then // Can not be the newly created ...
    exit;

  Process.FindProcSymbol(FFuncName, ALib, AProcList);
  SetLength(a, Length(AProcList));
  for i := 0 to Length(AProcList) - 1 do begin
    a[i] := AProcList[i].Address.Address;
    AProcList[i].ReleaseReference;
  end;

  AddAddress(a);
end;

constructor TFpInternalBreakpointAtSymbol.Create(const AProcess: TDbgProcess;
  const AFuncName: String; AnEnabled: Boolean; ASymInstance: TDbgInstance;
  AIgnoreCase: Boolean);
var
  a: TDBGPtrArray;
  AProcList: TFpSymbolArray;
  i: Integer;
begin
  FFuncName := AFuncName;
  FSymInstance := ASymInstance;

  AProcess.FindProcSymbol(AFuncName, ASymInstance, AProcList, AIgnoreCase);
  SetLength(a, Length(AProcList));
  for i := 0 to Length(AProcList) - 1 do begin
    a[i] := AProcList[i].Address.Address;
    AProcList[i].ReleaseReference;
  end;

  inherited Create(AProcess, a, AnEnabled);
end;

{ TFpInternalBreakpointAtFileLine }

procedure TFpInternalBreakpointAtFileLine.UpdateState;
begin
  if FErrorSettingCount > 0 then
    SetState(bksFailed)
  else
  if Length(FLocation) > 0 then
    SetState(bksOk)
  else
  if FFoundFileWithoutLine then
    SetState(bksFailed)
  else
    SetState(bksPending);
end;

procedure TFpInternalBreakpointAtFileLine.UpdateForLibraryLoaded(
  ALib: TDbgLibrary);
var
  addr: TDBGPtrArray;
  m: Integer;
begin
  if FSymInstance <> nil then // Can not be the newly created ...
    exit;

  addr := nil;
  m := Process.Config.BreakpointSearchMaxLines;
  if m > 0 then
    Process.GetLineAddresses(FFileName, FLine, addr, ALib, fsNextFuncLazy, m)
  else
    Process.GetLineAddresses(FFileName, FLine, addr, ALib);
  if Process.FLastLineAddressesFoundFile and (Length(addr) = 0) then
    FFoundFileWithoutLine := True;
  AddAddress(addr);
end;

constructor TFpInternalBreakpointAtFileLine.Create(const AProcess: TDbgProcess;
  const AFileName: String; ALine: Cardinal; AnEnabled: Boolean; ASymInstance: TDbgInstance);
var
  addr: TDBGPtrArray;
  m: Integer;
begin
  FFileName := AFileName;
  FLine := ALine;
  FSymInstance := ASymInstance;

  addr := nil;
  m := AProcess.Config.BreakpointSearchMaxLines;
  if m > 0 then
    AProcess.GetLineAddresses(AFileName, ALine, addr, ASymInstance, fsNextFuncLazy, m)
  else
    AProcess.GetLineAddresses(AFileName, ALine, addr, ASymInstance);
  FFoundFileWithoutLine := AProcess.FLastLineAddressesFoundFile and (Length(addr) = 0);
  inherited Create(AProcess, addr, AnEnabled);
end;

{ TFpInternalWatchpoint }

constructor TFpInternalWatchpoint.Create(const AProcess: TDbgProcess;
  const ALocation: TDBGPtr; ASize: Cardinal; AReadWrite: TDBGWatchPointKind;
  AScope: TDBGWatchPointScope);
(* FROM INTEL DOCS / About 8 byte watchpoints
For Pentium® 4 and Intel® Xeon® processors with a CPUID signature corresponding to family 15 (model 3, 4, and 6),
break point conditions permit specifying 8-byte length on data read/write with an of encoding 10B in the LENn field.
Encoding 10B is also supported in processors based on Intel Core microarchitecture or
enhanced Intel Core microarchitecture, the respective CPUID signatures corresponding to family 6, model 15,
and family 6, DisplayModel value 23 (see CPUID instruction in Chapter 3,
“Instruction Set Reference, A-L” in the Intel® 64 and IA-32 Architectures Software Developer’s Manual, Volume 2A).
The Encoding 10B is supported in processors based on Intel® Atom™ microarchitecture,
with CPUID signature of family 6, DisplayModel value 1CH. The encoding 10B is undefined for other processors
*)
const
  MAX_WATCH_SIZE = 8;
  SIZE_TO_BOUNDMASK: array[1..8] of TDBGPtr = (
    0,        // Size=1
    1, 0,     // Size=2
    3, 0,0,0, // Size=4
    7         // Size=8
    );
  SIZE_TO_WATCHSIZE: array[0..8] of Integer = (0, 1, 2, 4, 4, 8, 8, 8, 8);
var
  MaxWatchSize: Integer;
  BoundaryOffset, S, HalfSize: Integer;
begin
  inherited Create(AProcess);
  Process.FWatchPointList.Add(Self);
  FLocation := ALocation;
  FSize := ASize;
  FReadWrite := AReadWrite;
  FScope := AScope;

  MaxWatchSize := MAX_WATCH_SIZE;
//   Wach at 13FFC20:4 TO First 13FFC18:8 Other 0 (0) Last 0

  FFirstWatchSize := MaxWatchSize;
  BoundaryOffset := Integer(FLocation and SIZE_TO_BOUNDMASK[FFirstWatchSize]);
  // As long as the full first half of the watch is unused, use the next smaller watch-size
  HalfSize := FFirstWatchSize div 2;
  while (FFirstWatchSize > 1) and
        ( (BoundaryOffset >= HalfSize) or
          (FSize <= HalfSize)
        )
  do begin
    FFirstWatchSize := HalfSize;
    HalfSize := FFirstWatchSize div 2;
    BoundaryOffset := Integer(FLocation and SIZE_TO_BOUNDMASK[FFirstWatchSize]);
  end;
  FFirstWatchLocation := FLocation - BoundaryOffset;

  FOtherWatchesSize := 0;
  FOtherWatchCount := 0;
  FLastWatchSize := 0;

  S := FSize - FFirstWatchSize + BoundaryOffset; // remainder size
  if S > 0 then begin
    FOtherWatchCount := (S - 1) div MaxWatchSize;
    if FOtherWatchCount > 0 then
      FOtherWatchesSize := MaxWatchSize;

    S := S - FOtherWatchCount * FOtherWatchesSize;
    assert(S >= 0, 'TFpInternalWatchpoint.Create: S >= 0');

    FLastWatchSize := SIZE_TO_WATCHSIZE[S];
  end;
  debugln(DBG_VERBOSE, 'Wach at %x:%d TO First %x:%d Other %d (%d) Last %d',
    [FLocation, FSize, FFirstWatchLocation, FFirstWatchSize, FOtherWatchCount, FOtherWatchesSize, FLastWatchSize]);

  SetBreak;
end;

destructor TFpInternalWatchpoint.Destroy;
begin
  if Process <> nil then
    Process.FWatchPointList.Remove(Self);
  ResetBreak;
  inherited Destroy;
end;

procedure TFpInternalWatchpoint.SetBreak;
var
  a: TDBGPtr;
  wd: TFpWatchPointData;
  R: Boolean;
  i: Integer;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TFpInternalWatchpoint.SetBreak');{$ENDIF}
  if Process = nil then
    exit;
  //TODO: read current mem content. So in case of overlap it can be checked

  wd := Process.WatchPointData;

  a := FFirstWatchLocation;
  R := wd.AddOwnedWatchpoint(Self, a, FFirstWatchSize, FReadWrite);
  if not R then begin
    ResetBreak;
    exit;
  end;

  a := a + FFirstWatchSize;
  for i := 0 to FOtherWatchCount - 1 do begin
    R := wd.AddOwnedWatchpoint(Self, a, FOtherWatchesSize, FReadWrite);
    if not R then begin
      ResetBreak;
      exit;
    end;
    a := a + FOtherWatchesSize;
  end;

  if FLastWatchSize > 0 then
    R := wd.AddOwnedWatchpoint(Self, a, FLastWatchSize, FReadWrite);
  if not R then
    ResetBreak;
end;

procedure TFpInternalWatchpoint.ResetBreak;
begin
  {$IFDEF FPDEBUG_THREAD_CHECK}AssertFpDebugThreadId('TFpInternalWatchpoint.ResetBreak');{$ENDIF}
  if Process = nil then
    exit;

  Process.WatchPointData.RemoveOwnedWatchpoint(Self);
end;

function GetCanonicalFrameAddress(
  RegisterValueList: TDbgRegisterValueList; Row: TDwarfCallFrameInformationRow; out
  FrameBase: TDBGPtr): Boolean;
var
  Rule: TDwarfCallFrameInformationRule;
  Reg: TDbgRegisterValue;
begin
  Result := False;
  // Get CFA (framebase)

  Rule := Row.CFARule;
  case Rule.CFARule of
    cfaRegister:
      begin
      Reg := RegisterValueList.FindRegisterByDwarfIndex(Rule.&Register);
      if Assigned(Reg) then
        begin
        FrameBase := Reg.NumValue;
        {$PUSH}{$R-}{$Q-}
        FrameBase := FrameBase + TDBGPtr(Rule.Offset);
        {$POP}
        Result := True;
        end
      else
        begin
        DebugLn(FPDBG_DWARF_CFI_WARNINGS, 'CFI requested a register [' +IntToStr(Rule.&Register)+ '] that is not available.');
        Exit;
        end;
      end;
    cfaExpression:
      begin
      DebugLn(FPDBG_DWARF_CFI_WARNINGS, 'CFI-expressions are not supported. Not possible to obtain the CFA.');
      Exit;
      end;
    else
      begin
      DebugLn(FPDBG_DWARF_CFI_WARNINGS, 'CFI available but no rule to obtain the CFA.');
      Exit;
      end;
  end; // case
end;

function TryObtainNextCallFrame(
  CurrentCallStackEntry: TDbgCallstackEntry;
  CIE: TDwarfCIE;
  Size, NextIdx: Integer;
  Thread: TDbgThread;
  Row: TDwarfCallFrameInformationRow;
  Process: TDbgProcess;
  out NewCallStackEntry: TDbgCallstackEntry): Boolean;

  function ProcessCFIColumn(Row: TDwarfCallFrameInformationRow; Column: Byte; CFA: QWord; AddressSize: Integer; Entry: TDbgCallstackEntry; out Value: TDbgPtr): Boolean;
  var
    Rule: TDwarfCallFrameInformationRule;
    Reg: TDbgRegisterValue;
  begin
    Result := True;
    Value := 0;
    Rule := Row.RegisterArray[Column];
    case Rule.RegisterRule of
      cfiUndefined:
        begin
        Result := False;
        end;
      cfiSameValue:
        begin
        Reg := CurrentCallStackEntry.RegisterValueList.FindRegisterByDwarfIndex(Column);
        if Assigned(Reg) then
          Value := Reg.NumValue
        else
          Result := False;
        end;
      cfiOffset:
        begin
        {$PUSH}{$R-}{$Q-}
        Process.ReadData(CFA+TDBGPtr(Rule.Offset), AddressSize, Value);
        {$POP}
        end;
      cfiValOffset:
        begin
        {$PUSH}{$R-}{$Q-}
        Value := CFA+TDBGPtr(Rule.Offset);
        {$POP}
        end;
      cfiRegister:
        begin
        Reg := CurrentCallStackEntry.RegisterValueList.FindRegisterByDwarfIndex(Rule.&Register);
        if Assigned(Reg) then
          Value := Reg.NumValue
        else
          Result := False;
        end
      else
        begin
        DebugLn(FPDBG_DWARF_CFI_WARNINGS, 'Encountered unsupported CFI registerrule.');
        Result := False;
        end;
    end; // case
  end;

var
  //Rule: TDwarfCallFrameInformationRule;
  Reg: TDbgRegisterValue;
  i: Integer;
  ReturnAddress, Value: TDbgPtr;
  FrameBase: TDBGPtr;
  RegName: String;
begin
  Result := False;
  NewCallStackEntry := nil;
  // Get CFA (framebase)
  if not GetCanonicalFrameAddress(CurrentCallStackEntry.RegisterValueList, Row, FrameBase) then
    exit;

  Result := True;
  // Get return ReturnAddress
  if not ProcessCFIColumn(Row, CIE.ReturnAddressRegister, FrameBase, Size, CurrentCallStackEntry, ReturnAddress) then
    // Yes, we were succesfull, but there is no return ReturnAddress, so keep
    // NewCallStackEntry nil
    begin
    Result := True;
    Exit;
    end;

  if ReturnAddress=0 then
    // Yes, we were succesfull, but there is no frame left, so keep
    // NewCallStackEntry nil
    begin
    Result := True;
    Exit;
    end;

  NewCallStackEntry := TDbgCallstackEntry.create(Thread, NextIdx, FrameBase, ReturnAddress);

  // Fill other registers
  for i := 0 to High(Row.RegisterArray) do
    begin
    if ProcessCFIColumn(Row, i, FrameBase, Size, CurrentCallStackEntry, Value) then
      begin
      Reg := CurrentCallStackEntry.RegisterValueList.FindRegisterByDwarfIndex(i);
      if Assigned(Reg) then
        RegName := Reg.Name
      else
        RegName := IntToStr(i);
      NewCallStackEntry.RegisterValueList.DbgRegisterAutoCreate[RegName].SetValue(Value, IntToStr(Value),Size, i);
      end;
    end;
end;

initialization
  DBG_VERBOSE := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );
  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );
  DBG_BREAKPOINTS := DebugLogger.FindOrRegisterLogGroup('DBG_BREAKPOINTS' {$IFDEF DBG_BREAKPOINTS} , True {$ENDIF} );
  FPDBG_COMMANDS := DebugLogger.FindOrRegisterLogGroup('FPDBG_COMMANDS' {$IFDEF FPDBG_COMMANDS} , True {$ENDIF} );
  FPDBG_DWARF_CFI_WARNINGS  := DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_CFI_WARNINGS' {$IFDEF FPDBG_DWARF_CFI_WARNINGS} , True {$ENDIF} );

  TFpBreakPointTargetHandler.DBG__VERBOSE     := DBG_VERBOSE;
  TFpBreakPointTargetHandler.DBG__WARNINGS    := DBG_WARNINGS;
  TFpBreakPointTargetHandler.DBG__BREAKPOINTS := DBG_BREAKPOINTS;

finalization
  if assigned(RegisteredDbgProcessClasses) then
    FreeAndNil(RegisteredDbgProcessClasses);

end.
