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
{$TYPEDADDRESS on}

interface

uses
  Classes, SysUtils, Maps, FpDbgDwarf, FpDbgUtil, FpDbgLoader,
  FpDbgInfo, FpdMemoryTools, LazLoggerBase, LazClasses, DbgIntfBaseTypes, fgl,
  DbgIntfDebuggerBase,
  FpPascalBuilder,
  fpDbgSymTableContext,
  FpDbgDwarfDataClasses;

type
  TFPDEvent = (deExitProcess, deFinishedStep, deBreakpoint, deException, deCreateProcess, deLoadLibrary, deInternalContinue);
  TFPDMode = (dm32, dm64);
  TFPDCompareStepInfo = (dcsiNewLine, dcsiSameLine, dcsiNoLineInfo);

  { TDbgRegisterValue }

  TDbgRegisterValue = class
  private
    FDwarfIdx: cardinal;
    FName: string;
    FNumValue: TDBGPtr;
    FSize: byte;
    FStrValue: string;
  public
    constructor Create(AName: String);
    procedure SetValue(ANumValue: TDBGPtr; AStrValue: string; ASize: byte; ADwarfIdx: cardinal);
    procedure Setx86EFlagsValue(ANumValue: TDBGPtr);
    property Name: string read FName;
    property NumValue: TDBGPtr read FNumValue;
    property StrValue: string read FStrValue;
    property Size: byte read FSize;
    property DwarfIdx: cardinal read FDwarfIdx;
  end;

  TGDbgRegisterValueList = specialize TFPGObjectList<TDbgRegisterValue>;

  { TDbgRegisterValueList }

  TDbgRegisterValueList = class(TGDbgRegisterValueList)
  private
    function GetDbgRegister(AName: string): TDbgRegisterValue;
    function GetDbgRegisterAutoCreate(AName: string): TDbgRegisterValue;
  public
    property DbgRegisterAutoCreate[AName: string]: TDbgRegisterValue read GetDbgRegisterAutoCreate;
    function FindRegisterByDwarfIndex(AnIdx: cardinal): TDbgRegisterValue;
  end;

  { TDbgCallstackEntry }
  TDbgThread = class;
  TFPDThreadArray = array of TDbgThread;

  TDbgCallstackEntry = class
  private
    FAnAddress: TDBGPtr;
    FFrameAdress: TDBGPtr;
    FThread: TDbgThread;
    FIsSymbolResolved: boolean;
    FSymbol: TFpSymbol;
    FRegisterValueList: TDbgRegisterValueList;
    FIndex: integer;
    function GetFunctionName: string;
    function GetProcSymbol: TFpSymbol;
    function GetLine: integer;
    function GetSourceFile: string;
  public
    constructor create(AThread: TDbgThread; AnIndex: integer; AFrameAddress, AnAddress: TDBGPtr);
    destructor Destroy; override;
    function GetParamsAsString: string;
    property AnAddress: TDBGPtr read FAnAddress;
    property FrameAdress: TDBGPtr read FFrameAdress;
    property SourceFile: string read GetSourceFile;
    property FunctionName: string read GetFunctionName;
    property Line: integer read GetLine;
    property RegisterValueList: TDbgRegisterValueList read FRegisterValueList;
    property ProcSymbol: TFpSymbol read GetProcSymbol;
    property Index: integer read FIndex;
  end;

  TDbgCallstackEntryList = specialize TFPGObjectList<TDbgCallstackEntry>;

  TDbgProcess = class;

  { TDbgMemReader }

  TDbgMemReader = class(TFpDbgMemReaderBase)
  protected
    function GetDbgProcess: TDbgProcess; virtual; abstract;
    function GetDbgThread(AContext: TFpDbgAddressContext): TDbgThread; virtual;
  public
    function ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; override;
    function ReadMemoryEx(AnAddress, AnAddressSpace: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; override;
    function ReadRegister(ARegNum: Cardinal; out AValue: TDbgPtr; AContext: TFpDbgAddressContext): Boolean; override;
    function RegisterSize(ARegNum: Cardinal): Integer; override;
  end;

  { TDbgThread }
  TFpInternalBreakpoint = class;

  TDbgThread = class(TObject)
  private
    FNextIsSingleStep: boolean;
    FProcess: TDbgProcess;
    FID: Integer;
    FHandle: THandle;

    FPausedAtRemovedBreakPointState: (rbUnknown, rbNone, rbFound{, rbFoundAndDec});
    FPausedAtRemovedBreakPointAddress: TDBGPtr;

    function GetRegisterValueList: TDbgRegisterValueList;
  protected
    FCallStackEntryList: TDbgCallstackEntryList;
    FRegisterValueListValid: boolean;
    FRegisterValueList: TDbgRegisterValueList;
    FStoreStepSrcFilename: string;
    FStoreStepSrcLineNo: integer;
    FStoreStepStackFrame: TDBGPtr;
    FStoreStepFuncAddr: TDBGPtr;
    procedure LoadRegisterValues; virtual;
    property Process: TDbgProcess read FProcess;
    function ResetInstructionPointerAfterBreakpoint: boolean; virtual; abstract;
    procedure DoBeforeBreakLocationMapChange; // A new location added / or a location removed => memory will change
    procedure ValidateRemovedBreakPointInfo;
  public
    constructor Create(const AProcess: TDbgProcess; const AID: Integer; const AHandle: THandle); virtual;
    function HasInsertedBreakInstructionAtLocation(const ALocation: TDBGPtr): Boolean; // include removed breakpoints that (may have) already triggered
    procedure CheckAndResetInstructionPointerAfterBreakpoint;
    procedure BeforeContinue; virtual;
    function AddWatchpoint(AnAddr: TDBGPtr): integer; virtual;
    function RemoveWatchpoint(AnId: integer): boolean; virtual;
    function DetectHardwareWatchpoint: integer; virtual;

    function GetInstructionPointerRegisterValue: TDbgPtr; virtual; abstract;
    function GetStackBasePointerRegisterValue: TDbgPtr; virtual; abstract;
    function GetStackPointerRegisterValue: TDbgPtr; virtual; abstract;

    procedure PrepareCallStackEntryList(AFrameRequired: Integer = -1); virtual;
    procedure ClearCallStack;
    destructor Destroy; override;
    function CompareStepInfo: TFPDCompareStepInfo;
    function IsAtStartOfLine: boolean;
    procedure StoreStepInfo;
    property ID: Integer read FID;
    property Handle: THandle read FHandle;
    property NextIsSingleStep: boolean read FNextIsSingleStep write FNextIsSingleStep;
    property RegisterValueList: TDbgRegisterValueList read GetRegisterValueList;
    property CallStackEntryList: TDbgCallstackEntryList read FCallStackEntryList;
  end;
  TDbgThreadClass = class of TDbgThread;

  { TThreadMapEnumerator }

  TThreadMapEnumerator = class(TMapIterator)
  private
    FDoneFirst: Boolean;
    function GetCurrent: TDbgThread;
  public
    function MoveNext: Boolean;
    property Current: TDbgThread read GetCurrent;
  end;

  { TThreadMap }

  TThreadMap = class(TMap)
  public
    function GetEnumerator: TThreadMapEnumerator;
  end;

  TFpInternalBreakpointArray = array of TFpInternalBreakpoint;

  { TBreakLocationEntry }

  TBreakLocationEntry = object
    Location: TDBGPtr;
    Data: Pointer;
    function OrigValue: Byte;
    function ErrorSetting: ByteBool;
  end;

  { TBreakLocationMap }

  TBreakLocationMap = class(TMap)
  private type
    TInternalBreakLocationEntry = record
      OrigValue: Byte;
      IsBreakList, ErrorSetting: ByteBool;
      InternalBreakPoint: Pointer;
    end;
    PInternalBreakLocationEntry = ^TInternalBreakLocationEntry;

    { TBreakLocationMapEnumerator }

    TBreakLocationMapEnumerator = class(TMapIterator)
    private
      FDoneFirst: Boolean;
      function GetCurrent: TBreakLocationEntry;
    public
      function MoveNext: Boolean;
      property Current: TBreakLocationEntry read GetCurrent;
    end;
  private
    FProcess: TDbgProcess;
    class function OrigByteFromPointer(AData: Pointer): Byte;
    class function ErrorSettingFromPointer(AData: Pointer): ByteBool;
  public
    constructor Create(AProcess: TDbgProcess);
    destructor Destroy; override;
    procedure Clear; reintroduce;
    procedure AddLocotion(const ALocation: TDBGPtr; const AInternalBreak: TFpInternalBreakpoint; AnIgnoreIfExists: Boolean = True);
    procedure RemoveLocotion(const ALocation: TDBGPtr; const AInternalBreak: TFpInternalBreakpoint);
    function GetInternalBreaksAtLocation(const ALocation: TDBGPtr): TFpInternalBreakpointArray;
    function GetOrigValueAtLocation(const ALocation: TDBGPtr): Byte; // returns Int3, if there is no break at this location
    function HasInsertedBreakInstructionAtLocation(const ALocation: TDBGPtr): Boolean;
    function GetEnumerator: TBreakLocationMapEnumerator;
  end;

  { TFpInternalBreakpointBase }

  TFpInternalBreakpointBase = class(TObject)
  public
    function Hit(const AThreadID: Integer; ABreakpointAddress: TDBGPtr): Boolean; virtual; abstract;
    procedure SetBreak; virtual; abstract;
    procedure ResetBreak; virtual; abstract;
  end;

  { TFpInternalBreakpoint }

  TFpInternalBreakpoint = class(TFpInternalBreakpointBase)
  private
    FProcess: TDbgProcess;
    FLocation: TDBGPtrArray;
    FInternal: Boolean;
  protected
    property Process: TDbgProcess read FProcess;
    property Location: TDBGPtrArray read FLocation;
  public
    constructor Create(const AProcess: TDbgProcess; const ALocation: TDBGPtrArray); virtual;
    destructor Destroy; override;
    function Hit(const AThreadID: Integer; ABreakpointAddress: TDBGPtr): Boolean; override;
    function HasLocation(const ALocation: TDBGPtr): Boolean;

    procedure SetBreak; override;
    procedure ResetBreak; override;
  end;
  TFpInternalBreakpointClass = class of TFpInternalBreakpoint;

  TFpInternalBreakpointList = specialize TFPGObjectList<TFpInternalBreakpoint>;


  { TDbgInstance }

  TDbgInstance = class(TObject)
  private
    FMode: TFPDMode;
    FFileName: String;
    FProcess: TDbgProcess;
    FSymbolTableInfo: TFpSymbolInfo;
    FLoaderList: TDbgImageLoaderList;

  protected
    FDbgInfo: TDbgInfo;
    procedure InitializeLoaders; virtual;
    procedure SetFileName(const AValue: String);
    property LoaderList: TDbgImageLoaderList read FLoaderList write FLoaderList;
  public
    constructor Create(const AProcess: TDbgProcess); virtual;
    destructor Destroy; override;

    function AddBreak(const AFileName: String; ALine: Cardinal): TFpInternalBreakpoint; overload;
    function AddrOffset: Int64; virtual;  // gives the offset between  the loaded addresses and the compiled addresses
    function FindProcSymbol(AAdress: TDbgPtr): TFpSymbol;
    procedure LoadInfo; virtual;

    property Process: TDbgProcess read FProcess;
    property DbgInfo: TDbgInfo read FDbgInfo;
    property SymbolTableInfo: TFpSymbolInfo read FSymbolTableInfo;
    property Mode: TFPDMode read FMode;
  end;

  { TDbgLibrary }

  TDbgLibrary = class(TDbgInstance)
  private
    FModuleHandle: THandle;
    FBaseAddr: TDBGPtr;
  public
    constructor Create(const AProcess: TDbgProcess; const ADefaultName: String; const AModuleHandle: THandle; const ABaseAddr: TDbgPtr);
    property Name: String read FFileName;
    property ModuleHandle: THandle read FModuleHandle;
    property BaseAddr: TDBGPtr read FBaseAddr;
  end;

  TStartInstanceFlag = (siRediretOutput, siForceNewConsole);
  TStartInstanceFlags = set of TStartInstanceFlag;

  { TDbgProcess }

  TDbgProcess = class(TDbgInstance)
  protected const
    Int3: Byte = $CC;
  private
    FExceptionClass: string;
    FExceptionMessage: string;
    FExitCode: DWord;
    FGotExitProcess: Boolean;
    FProcessID: Integer;
    FThreadID: Integer;

    function GetPauseRequested: boolean;
    procedure SetPauseRequested(AValue: boolean);
    procedure ThreadDestroyed(const AThread: TDbgThread);
  protected
    FBreakpointList: TFpInternalBreakpointList;
    FCurrentBreakpoint: TFpInternalBreakpoint;  // set if we are executing the code at the break
                                         // if the singlestep is done, set the break again
    FCurrentWatchpoint: integer;
    FReEnableBreakStep: Boolean;         // Set when we are reenabling a breakpoint
                                         // We need a single step, so the IP is after the break to set

    FSymInstances: TList;  // list of dbgInstances with debug info

    FThreadMap: TThreadMap; // map ThreadID -> ThreadObject
    FLibMap: TMap;    // map LibAddr -> LibObject
    FBreakMap: TBreakLocationMap;  // map BreakAddr -> BreakObject
    FTmpRemovedBreaks: array of TDBGPtr;
    FPauseRequested: longint;

    FMainThread: TDbgThread;
    function GetHandle: THandle; virtual;
    procedure SetThreadId(AThreadId: Integer);
    procedure SetExitCode(AValue: DWord);
    function GetLastEventProcessIdentifier: THandle; virtual;
    function DoBreak(BreakpointAddress: TDBGPtr; AThreadID: integer): Boolean;

    function InsertBreakInstructionCode(const ALocation: TDBGPtr; out OrigValue: Byte): Boolean; //virtual;
    function RemoveBreakInstructionCode(const ALocation: TDBGPtr; const OrigValue: Byte): Boolean; //virtual;
    procedure RemoveAllBreakPoints;
    procedure BeforeChangingInstructionCode(const ALocation: TDBGPtr); virtual;
    procedure AfterChangingInstructionCode(const ALocation: TDBGPtr); virtual;

    procedure MaskBreakpointsInReadData(const AAdress: TDbgPtr; const ASize: Cardinal; var AData);
    // Should create a TDbgThread-instance for the given ThreadIdentifier.
    function CreateThread(AthreadIdentifier: THandle; out IsMainThread: boolean): TDbgThread; virtual; abstract;
    // Should analyse why the debugger has stopped.
    function AnalyseDebugEvent(AThread: TDbgThread): TFPDEvent; virtual; abstract;
  public
    class function StartInstance(AFileName: string; AParams, AnEnvironment: TStrings; AWorkingDirectory, AConsoleTty: string; AFlags: TStartInstanceFlags): TDbgProcess; virtual;
    class function AttachToInstance(AFileName: string; APid: Integer): TDbgProcess; virtual;
    constructor Create(const AFileName: string; const AProcessID, AThreadID: Integer); virtual;
    destructor Destroy; override;
    function  AddInternalBreak(const ALocation: TDBGPtr): TFpInternalBreakpoint; overload;
    function  AddInternalBreak(const ALocation: TDBGPtrArray): TFpInternalBreakpoint; overload;
    function  AddBreak(const ALocation: TDBGPtr): TFpInternalBreakpoint; overload;
    function  AddBreak(const ALocation: TDBGPtrArray): TFpInternalBreakpoint; overload;
    function  FindProcSymbol(const AName: String): TFpSymbol;
    function  FindProcSymbol(AAdress: TDbgPtr): TFpSymbol;
    function  FindContext(AThreadId, AStackFrame: Integer): TFpDbgInfoContext;
    function  FindContext(AAddress: TDbgPtr): TFpDbgInfoContext; deprecated 'use FindContext(thread,stack)';
    function  GetLib(const AHandle: THandle; out ALib: TDbgLibrary): Boolean;
    function  GetThread(const AID: Integer; out AThread: TDbgThread): Boolean;
    procedure RemoveBreak(const ABreakPoint: TFpInternalBreakpoint);
    procedure DoBeforeBreakLocationMapChange;
    function  HasBreak(const ALocation: TDbgPtr): Boolean; // TODO: remove, once an address can have many breakpoints
    procedure RemoveThread(const AID: DWord);
    function FormatAddress(const AAddress): String;
    function  Pause: boolean; virtual;

    function ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean; virtual;
    function ReadAddress(const AAdress: TDbgPtr; out AData: TDBGPtr): Boolean; virtual;
    function ReadOrdinal(const AAdress: TDbgPtr; out AData): Boolean; virtual;
    function ReadString(const AAdress: TDbgPtr; const AMaxSize: Cardinal; out AData: String): Boolean; virtual;
    function ReadWString(const AAdress: TDbgPtr; const AMaxSize: Cardinal; out AData: WideString): Boolean; virtual;

    //function LocationIsBreakInstructionCode(const ALocation: TDBGPtr): Boolean; // excludes TempRemoved
    procedure TempRemoveBreakInstructionCode(const ALocation: TDBGPtr);
    procedure RestoreTempBreakInstructionCodes;
    function HasInsertedBreakInstructionAtLocation(const ALocation: TDBGPtr): Boolean; // returns Int3, if there is no break at this location

    function Continue(AProcess: TDbgProcess; AThread: TDbgThread; SingleStep: boolean): boolean; virtual;
    function WaitForDebugEvent(out ProcessIdentifier, ThreadIdentifier: THandle): boolean; virtual; abstract;
    function ResolveDebugEvent(AThread: TDbgThread): TFPDEvent; virtual;

    function CheckForConsoleOutput(ATimeOutMs: integer): integer; virtual;
    function GetConsoleOutput: string; virtual;
    procedure SendConsoleInput(AString: string); virtual;

    function AddThread(AThreadIdentifier: THandle): TDbgThread;
    function GetThreadArray: TFPDThreadArray;
    procedure ThreadsBeforeContinue;
    procedure ThreadsClearCallStack;
    procedure LoadInfo; override;

    function WriteData(const AAdress: TDbgPtr; const ASize: Cardinal; const AData): Boolean; virtual;

    procedure TerminateProcess; virtual; abstract;
    function Detach(AProcess: TDbgProcess; AThread: TDbgThread): boolean; virtual;

    property Handle: THandle read GetHandle;
    property Name: String read FFileName write SetFileName;
    property ProcessID: integer read FProcessID;
    property ThreadID: integer read FThreadID;
    property ExitCode: DWord read FExitCode;
    property CurrentBreakpoint: TFpInternalBreakpoint read FCurrentBreakpoint;
    property CurrentWatchpoint: integer read FCurrentWatchpoint;
    property PauseRequested: boolean read GetPauseRequested write SetPauseRequested;
    function GetAndClearPauseRequested: Boolean;

    // Properties valid when last event was an deException
    property ExceptionMessage: string read FExceptionMessage write FExceptionMessage;
    property ExceptionClass: string read FExceptionClass write FExceptionClass;

    property LastEventProcessIdentifier: THandle read GetLastEventProcessIdentifier;
    property MainThread: TDbgThread read FMainThread;
    property GotExitProcess: Boolean read FGotExitProcess write FGotExitProcess;
  end;
  TDbgProcessClass = class of TDbgProcess;

  TOSDbgClasses = class
  public
    DbgThreadClass : TDbgThreadClass;
    DbgBreakpointClass : TFpInternalBreakpointClass;
    DbgProcessClass : TDbgProcessClass;
  end;

var
  {$ifdef cpui386}
  GMode: TFPDMode = dm32;
  {$else}
  GMode: TFPDMode = dm64;
  {$endif}

const
  DBGPTRSIZE: array[TFPDMode] of Integer = (4, 8);
  FPDEventNames: array[TFPDEvent] of string = ('deExitProcess', 'deFinishedStep', 'deBreakpoint', 'deException', 'deCreateProcess', 'deLoadLibrary', 'deInternalContinue');

function OSDbgClasses: TOSDbgClasses;

implementation

{$ifdef windows}
uses
  FpDbgWinClasses;
{$endif}
{$ifdef darwin}
uses
  FpDbgDarwinClasses;
{$endif}
{$ifdef linux}
uses
  FpDbgLinuxClasses;
{$endif}

var
  GOSDbgClasses : TOSDbgClasses;
  DBG_VERBOSE, DBG_WARNINGS, DBG_BREAKPOINTS: PLazLoggerLogGroup;

function OSDbgClasses: TOSDbgClasses;
begin
  if GOSDbgClasses=nil then
    begin
    GOSDbgClasses := TOSDbgClasses.create;
    GOSDbgClasses.DbgThreadClass := TDbgThread;
    GOSDbgClasses.DbgBreakpointClass := TFpInternalBreakpoint;
    GOSDbgClasses.DbgProcessClass := TDbgProcess;
    {$ifdef windows}
    RegisterDbgClasses;
    {$endif windows}
    {$ifdef darwin}
    RegisterDbgClasses;
    {$endif darwin}
    {$ifdef linux}
    RegisterDbgClasses;
    {$endif linux}
    end;
  result := GOSDbgClasses;
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

{ TBreakLocationEntry }

function TBreakLocationEntry.OrigValue: Byte;
begin
  Result := TBreakLocationMap.OrigByteFromPointer(Data);
end;

function TBreakLocationEntry.ErrorSetting: ByteBool;
begin
  Result := TBreakLocationMap.ErrorSettingFromPointer(Data);
end;

{ TBreakLocationMap.TBreakLocationMapEnumerator }

function TBreakLocationMap.TBreakLocationMapEnumerator.GetCurrent: TBreakLocationEntry;
begin
  Result.Data := DataPtr;
  GetID(Result.Location);
end;

function TBreakLocationMap.TBreakLocationMapEnumerator.MoveNext: Boolean;
begin
  if FDoneFirst then
    Next
  else
    First;
  FDoneFirst := True;
  Result := not EOM;
end;

{ TBreakLocationMap }

procedure TBreakLocationMap.Clear;
var
  LocData: TBreakLocationEntry;
begin
  debugln(DBG_VERBOSE or DBG_BREAKPOINTS, ['TBreakLocationMap.Clear ']);
  for LocData in Self do begin
    if PInternalBreakLocationEntry(LocData.Data)^.IsBreakList then
      TFpInternalBreakpointArray(PInternalBreakLocationEntry(LocData.Data)^.InternalBreakPoint) := nil;
  end;
  inherited Clear;
end;

class function TBreakLocationMap.OrigByteFromPointer(AData: Pointer): Byte;
begin
  Result := PInternalBreakLocationEntry(AData)^.OrigValue;
end;

class function TBreakLocationMap.ErrorSettingFromPointer(AData: Pointer
  ): ByteBool;
begin
  Result := PInternalBreakLocationEntry(AData)^.ErrorSetting;
end;

constructor TBreakLocationMap.Create(AProcess: TDbgProcess);
begin
  FProcess := AProcess;
  inherited Create(itu8, SizeOf(TInternalBreakLocationEntry));
end;

destructor TBreakLocationMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TBreakLocationMap.AddLocotion(const ALocation: TDBGPtr;
  const AInternalBreak: TFpInternalBreakpoint; AnIgnoreIfExists: Boolean);
var
  LocData: PInternalBreakLocationEntry;
  Len, i: Integer;
  BList: TFpInternalBreakpointArray;
begin
  LocData := GetDataPtr(ALocation);

  if LocData <> nil then begin
    if LocData^.IsBreakList then begin
      Len := Length(TFpInternalBreakpointArray(LocData^.InternalBreakPoint));
      if AnIgnoreIfExists then begin
        i := Len - 1;
        while (i >= 0) and (TFpInternalBreakpointArray(LocData^.InternalBreakPoint)[i] <> AInternalBreak) do
          dec(i);
        if i >= 0 then
          exit;
      end;

      SetLength(TFpInternalBreakpointArray(LocData^.InternalBreakPoint), Len+1);
      TFpInternalBreakpointArray(LocData^.InternalBreakPoint)[Len] := AInternalBreak;
    end
    else begin
      if AnIgnoreIfExists and (TFpInternalBreakpoint(LocData^.InternalBreakPoint) = AInternalBreak) then
        exit;

      LocData^.IsBreakList := True;
      SetLength(BList, 2);
      BList[0] := TFpInternalBreakpoint(LocData^.InternalBreakPoint);
      BList[1] := AInternalBreak;
      LocData^.InternalBreakPoint := nil;
      TFpInternalBreakpointArray(LocData^.InternalBreakPoint) := BList;
    end;

    exit;
  end;

  FProcess.DoBeforeBreakLocationMapChange; // Only if a new breakpoint is set => memory changed
  new(LocData);
  LocData^.ErrorSetting := not FProcess.InsertBreakInstructionCode(ALocation, LocData^.OrigValue);
  LocData^.IsBreakList := False;
  LocData^.InternalBreakPoint := AInternalBreak;
  Add(ALocation, LocData^);
  Dispose(LocData);
end;

procedure TBreakLocationMap.RemoveLocotion(const ALocation: TDBGPtr;
  const AInternalBreak: TFpInternalBreakpoint);
var
  LocData: PInternalBreakLocationEntry;
  Len, i: Integer;
begin
  LocData := GetDataPtr(ALocation);
  if LocData = nil then begin
    DebugLn(DBG_WARNINGS or DBG_BREAKPOINTS, ['Missing breakpoint for loc ', FormatAddress(ALocation)]);
    exit;
  end;

  if LocData^.IsBreakList then begin
    Len := Length(TFpInternalBreakpointArray(LocData^.InternalBreakPoint));
    i := Len - 1;
    while (i >= 0) and (TFpInternalBreakpointArray(LocData^.InternalBreakPoint)[i] <> AInternalBreak) do
      dec(i);
    if i < 0 then begin
      DebugLn(DBG_WARNINGS or DBG_BREAKPOINTS, ['Wrong break for loc ', FormatAddress(ALocation)]);
      exit;
    end;
    if i < Len - 1 then
      move(TFpInternalBreakpointArray(LocData^.InternalBreakPoint)[i+1],
           TFpInternalBreakpointArray(LocData^.InternalBreakPoint)[i],
           (Len - 1 - i) * sizeof(TFpInternalBreakpoint));
    SetLength(TFpInternalBreakpointArray(LocData^.InternalBreakPoint), Len-1);

    if Len > 1 then
      exit;
  end
  else
  if AInternalBreak <> TFpInternalBreakpoint(LocData^.InternalBreakPoint) then begin
    DebugLn(DBG_WARNINGS or DBG_BREAKPOINTS, ['Wrong break for loc ', FormatAddress(ALocation)]);

    exit;
  end;

  FProcess.DoBeforeBreakLocationMapChange; // Only if a breakpoint is removed => memory changed
  if not LocData^.ErrorSetting then
    FProcess.RemoveBreakInstructionCode(ALocation, LocData^.OrigValue);
  Delete(ALocation);
end;

function TBreakLocationMap.GetInternalBreaksAtLocation(const ALocation: TDBGPtr
  ): TFpInternalBreakpointArray;
var
  LocData: PInternalBreakLocationEntry;
begin
  LocData := GetDataPtr(ALocation);
  if LocData = nil then begin
    DebugLn(DBG_WARNINGS or DBG_BREAKPOINTS, ['Missing breakpoint for loc ', FormatAddress(ALocation)]);
    Result := nil;
    exit;
  end;

  if LocData^.IsBreakList then begin
    Result := TFpInternalBreakpointArray(LocData^.InternalBreakPoint)
  end
  else begin
    SetLength(Result, 1);
    Result[0] := TFpInternalBreakpoint(LocData^.InternalBreakPoint);
  end;
end;

function TBreakLocationMap.GetOrigValueAtLocation(const ALocation: TDBGPtr
  ): Byte;
var
  LocData: PInternalBreakLocationEntry;
begin
  LocData := GetDataPtr(ALocation);
  if LocData = nil then begin
    DebugLn(DBG_WARNINGS or DBG_BREAKPOINTS, ['Missing breakpoint for loc ', FormatAddress(ALocation)]);
    Result := TDbgProcess.Int3;
    exit;
  end;
  Result := LocData^.OrigValue;
end;

function TBreakLocationMap.HasInsertedBreakInstructionAtLocation(
  const ALocation: TDBGPtr): Boolean;
begin
  Result := GetDataPtr(ALocation) <> nil;
end;

function TBreakLocationMap.GetEnumerator: TBreakLocationMapEnumerator;
begin
  Result := TBreakLocationMapEnumerator.Create(Self);
end;

{ TDbgCallstackEntry }

function TDbgCallstackEntry.GetProcSymbol: TFpSymbol;
begin
  if not FIsSymbolResolved then begin
    if FIndex > 0 then
      FSymbol := FThread.Process.FindProcSymbol(FAnAddress - 1) // -1 => inside the call instruction
    else
      FSymbol := FThread.Process.FindProcSymbol(FAnAddress);
    FIsSymbolResolved := FSymbol <> nil
  end;
  result := FSymbol;
end;

function TDbgCallstackEntry.GetFunctionName: string;
var
  Symbol: TFpSymbol;
begin
  Symbol := GetProcSymbol;
  if assigned(Symbol) then
    result := Symbol.Name
  else
    result := '';
end;

function TDbgCallstackEntry.GetParamsAsString: string;
var
  ProcVal: TFpValue;
  InstrPointerValue: TDBGPtr;
  AContext: TFpDbgInfoContext;
  APrettyPrinter: TFpPascalPrettyPrinter;
  m: TFpValue;
  v: String;
  i: Integer;
begin
  result := '';
  if assigned(ProcSymbol) then begin
    ProcVal := ProcSymbol.Value;
    if (ProcVal <> nil) then begin
      InstrPointerValue := AnAddress;
      if InstrPointerValue <> 0 then begin
        AContext := FThread.Process.DbgInfo.FindContext(FThread.ID, Index, InstrPointerValue);
        if AContext <> nil then begin
          AContext.MemManager.DefaultContext := AContext;
          TFpValueDwarf(ProcVal).Context := AContext;
          APrettyPrinter:=TFpPascalPrettyPrinter.Create(DBGPTRSIZE[FThread.Process.Mode]);
          try
            for i := 0 to ProcVal.MemberCount - 1 do begin
              m := ProcVal.Member[i];
              if (m <> nil) and (sfParameter in m.DbgSymbol.Flags) then begin
                APrettyPrinter.PrintValue(v, m, wdfDefault, -1, [ppoStackParam]);
                if result <> '' then result := result + ', ';
                result := result + v;
              end;
              m.ReleaseReference;
            end;
          finally
            APrettyPrinter.Free;
          end;
          TFpValueDwarf(ProcVal).Context := nil;
          AContext.ReleaseReference;
        end;
      end;
      ProcVal.ReleaseReference;
    end;
    if result <> '' then
      result := '(' + result + ')';
  end;
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
  inherited Destroy;
end;

{ TDbgMemReader }

function TDbgMemReader.GetDbgThread(AContext: TFpDbgAddressContext): TDbgThread;
var
  Process: TDbgProcess;
begin
  Process := GetDbgProcess;
  if not Process.GetThread(AContext.ThreadId, Result) then
    Result := Process.MainThread;
end;

function TDbgMemReader.ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean;
begin
  result := GetDbgProcess.ReadData(AnAddress, ASize, ADest^);
end;

function TDbgMemReader.ReadMemoryEx(AnAddress, AnAddressSpace: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean;
begin
  Assert(AnAddressSpace>0,'TDbgMemReader.ReadMemoryEx ignores AddressSpace');
  result := GetDbgProcess.ReadData(AnAddress, ASize, ADest^);
end;

function TDbgMemReader.ReadRegister(ARegNum: Cardinal; out AValue: TDbgPtr; AContext: TFpDbgAddressContext): Boolean;
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

{ TDbgRegisterValueList }

function TDbgRegisterValueList.GetDbgRegister(AName: string): TDbgRegisterValue;
var
  i: integer;
begin
  for i := 0 to Count -1 do
    if Items[i].Name=AName then
      begin
      result := items[i];
      exit;
      end;
  result := nil;
end;

function TDbgRegisterValueList.GetDbgRegisterAutoCreate(AName: string): TDbgRegisterValue;
begin
  result := GetDbgRegister(AName);
  if not Assigned(result) then
    begin
    result := TDbgRegisterValue.Create(AName);
    add(result);
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

{ TDbgRegisterValue }

constructor TDbgRegisterValue.Create(AName: String);
begin
  FName:=AName;
end;

procedure TDbgRegisterValue.SetValue(ANumValue: TDBGPtr; AStrValue: string;
  ASize: byte; ADwarfIdx: Cardinal);
begin
  FStrValue:=AStrValue;
  FNumValue:=ANumValue;
  FSize:=ASize;
  FDwarfIdx:=ADwarfIdx;
end;

procedure TDbgRegisterValue.Setx86EFlagsValue(ANumValue: TDBGPtr);
var
  FlagS: string;
begin
  FlagS := '';
  if ANumValue and (1 shl 0) <> 0 then FlagS := FlagS + 'CF ';
  if ANumValue and (1 shl 2) <> 0 then FlagS := FlagS + 'PF ';
  if ANumValue and (1 shl 4) <> 0 then FlagS := FlagS + 'AF ';
  if ANumValue and (1 shl 6) <> 0 then FlagS := FlagS + 'ZF ';
  if ANumValue and (1 shl 7) <> 0 then FlagS := FlagS + 'SF ';
  if ANumValue and (1 shl 8) <> 0 then FlagS := FlagS + 'TF ';
  if ANumValue and (1 shl 9) <> 0 then FlagS := FlagS + 'IF ';
  if ANumValue and (1 shl 10) <> 0 then FlagS := FlagS + 'DF ';
  if ANumValue and (1 shl 11) <> 0 then FlagS := FlagS + 'OF ';
  if (ANumValue shr 12) and 3 <> 0 then FlagS := FlagS + 'IOPL=' + IntToStr((ANumValue shr 12) and 3);
  if ANumValue and (1 shl 14) <> 0 then FlagS := FlagS + 'NT ';
  if ANumValue and (1 shl 16) <> 0 then FlagS := FlagS + 'RF ';
  if ANumValue and (1 shl 17) <> 0 then FlagS := FlagS + 'VM ';
  if ANumValue and (1 shl 18) <> 0 then FlagS := FlagS + 'AC ';
  if ANumValue and (1 shl 19) <> 0 then FlagS := FlagS + 'VIF ';
  if ANumValue and (1 shl 20) <> 0 then FlagS := FlagS + 'VIP ';
  if ANumValue and (1 shl 21) <> 0 then FlagS := FlagS + 'ID ';

  SetValue(ANumValue, trim(FlagS),4,Cardinal(-1));
end;

{ TDbgInstance }

function TDbgInstance.AddBreak(const AFileName: String; ALine: Cardinal): TFpInternalBreakpoint;
var
  addr: TDBGPtrArray;
  o: Int64;
  i: Integer;
begin
  Result := nil;
  if not FDbgInfo.HasInfo then Exit;
  if FDbgInfo.GetLineAddresses(AFileName, ALine, addr) then begin
    o := AddrOffset;
    for i := 0 to High(addr) do
      addr[i] := addr[i] - o;
    Result := FProcess.AddBreak(addr);
  end;
end;

function TDbgInstance.AddrOffset: Int64;
begin
  Result := FLoaderList.ImageBase;
end;

constructor TDbgInstance.Create(const AProcess: TDbgProcess);
begin
  FProcess := AProcess;
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

function TDbgInstance.FindProcSymbol(AAdress: TDbgPtr): TFpSymbol;
begin
  Result := FDbgInfo.FindProcSymbol(AAdress + AddrOffset);
  if not assigned(Result) then
    result := FSymbolTableInfo.FindProcSymbol(AAdress + AddrOffset);
end;

procedure TDbgInstance.LoadInfo;
begin
  InitializeLoaders;
  if FLoaderList.Image64Bit then
    FMode:=dm64
  else
    FMode:=dm32;
  FDbgInfo := TFpDwarfInfo.Create(FLoaderList);
  TFpDwarfInfo(FDbgInfo).LoadCompilationUnits;
  FSymbolTableInfo := TFpSymbolInfo.Create(FLoaderList);
end;

procedure TDbgInstance.SetFileName(const AValue: String);
begin
  FFileName := AValue;
end;

procedure TDbgInstance.InitializeLoaders;
begin
  // Do nothing;
end;

{ TDbgLibrary }

constructor TDbgLibrary.Create(const AProcess: TDbgProcess; const ADefaultName: String; const AModuleHandle: THandle; const ABaseAddr: TDbgPtr);

begin
  inherited Create(AProcess);
  FModuleHandle:=AModuleHandle;
  FBaseAddr:=ABaseAddr;
end;

{ TDbgProcess }

function TDbgProcess.AddBreak(const ALocation: TDBGPtr): TFpInternalBreakpoint;
var
  a: TDBGPtrArray;
begin
  SetLength(a, 1);
  a[0] := ALocation;
  Result := AddBreak(a);
// TODO: if a = GetInstructionPointerRegisterValue (of any thread?)
end;

function TDbgProcess.AddBreak(const ALocation: TDBGPtrArray
  ): TFpInternalBreakpoint;
var
  a, ip: TDBGPtr;
begin
  Result := OSDbgClasses.DbgBreakpointClass.Create(Self, ALocation);
  // TODO: empty breakpoint (all address failed to set) = nil
  ip := FMainThread.GetInstructionPointerRegisterValue;
  if not assigned(FCurrentBreakpoint) then
    for a in ALocation do
      if ip=a then begin
        FCurrentBreakpoint := Result;
        break;
      end;
end;

constructor TDbgProcess.Create(const AFileName: string; const AProcessID,
  AThreadID: Integer);
const
  {.$IFDEF CPU64}
  MAP_ID_SIZE = itu8;
  {.$ELSE}
//  MAP_ID_SIZE = itu4;
  {.$ENDIF}
begin
  FProcessID := AProcessID;
  FThreadID := AThreadID;

  FBreakpointList := TFpInternalBreakpointList.Create(False);
  FThreadMap := TThreadMap.Create(itu4, SizeOf(TDbgThread));
  FLibMap := TMap.Create(MAP_ID_SIZE, SizeOf(TDbgLibrary));
  FBreakMap := TBreakLocationMap.Create(Self);
  FCurrentBreakpoint := nil;
  FCurrentWatchpoint := -1;

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

  for i := 0 to FBreakpointList.Count - 1 do
    FBreakpointList[i].FProcess := nil;
  FreeAndNil(FBreakpointList);
  //Assert(FBreakMap.Count=0, 'No breakpoints left');
  //FreeItemsInMap(FBreakMap);
  FreeItemsInMap(FThreadMap);
  FreeItemsInMap(FLibMap);

  FreeAndNil(FBreakMap);
  FreeAndNil(FThreadMap);
  FreeAndNil(FLibMap);
  FreeAndNil(FSymInstances);
  inherited;
end;

function TDbgProcess.AddInternalBreak(const ALocation: TDBGPtr): TFpInternalBreakpoint;
begin
  Result := AddBreak(ALocation);
  Result.FInternal := True;
end;

function TDbgProcess.AddInternalBreak(const ALocation: TDBGPtrArray): TFpInternalBreakpoint;
begin
  Result := AddBreak(ALocation);
  Result.FInternal := True;
end;

function TDbgProcess.FindProcSymbol(const AName: String): TFpSymbol;
begin
  Result := FDbgInfo.FindProcSymbol(AName);
  // SymbolTableInfo.FindProcSymbol()
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
  // SymbolTableInfo.FindProcSymbol()
end;

function TDbgProcess.FindContext(AThreadId, AStackFrame: Integer): TFpDbgInfoContext;
var
  Thread: TDbgThread;
  Frame: TDbgCallstackEntry;
  Addr: TDBGPtr;
begin
  Result := nil;
  if not GetThread(AThreadId, Thread) then
    exit;
  if AStackFrame = 0 then
    Addr := Thread.GetInstructionPointerRegisterValue
  else
  begin
    Thread.PrepareCallStackEntryList(AStackFrame + 1);
    Frame := Thread.CallStackEntryList[AStackFrame];
    if Frame = nil then
      exit;
    Addr := Frame.AnAddress;
  end;
  if Addr = 0 then
    exit;
  Result := FDbgInfo.FindContext(AThreadId, AStackFrame, Addr);
  // SymbolTableInfo.FindContext()
end;

function TDbgProcess.FindContext(AAddress: TDbgPtr): TFpDbgInfoContext;
begin
  Result := FDbgInfo.FindContext(AAddress);
  // SymbolTableInfo.FindContext()
end;

function TDbgProcess.GetLib(const AHandle: THandle; out ALib: TDbgLibrary): Boolean;
var
  Iterator: TMapIterator;
  Lib: TDbgLibrary;
begin
  Result := False;
  Iterator := TMapIterator.Create(FLibMap);
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

function TDbgProcess.Continue(AProcess: TDbgProcess; AThread: TDbgThread;
  SingleStep: boolean): boolean;
begin
  result := false;
end;

function TDbgProcess.ResolveDebugEvent(AThread: TDbgThread): TFPDEvent;
var
  CurrentAddr: TDBGPtr;
begin
  AThread.ValidateRemovedBreakPointInfo;
  result := AnalyseDebugEvent(AThread);

  if result = deBreakpoint then
  begin
    // Determine the address where the execution has stopped
    CurrentAddr:=AThread.GetInstructionPointerRegisterValue;
    FCurrentWatchpoint:=AThread.DetectHardwareWatchpoint;
    FCurrentBreakpoint:=nil;
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
  Iterator := TMapIterator.Create(FThreadMap);
  try
    Iterator.First;
    while not Iterator.EOM do
    begin
      Iterator.GetData(Thread);
      Thread.BeforeContinue;
      iterator.Next;
    end;
  finally
    Iterator.Free;
  end;
end;

procedure TDbgProcess.ThreadsClearCallStack;
var
  Iterator: TMapIterator;
  Thread: TDbgThread;
begin
  Iterator := TMapIterator.Create(FThreadMap);
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

procedure TDbgProcess.RemoveBreak(const ABreakPoint: TFpInternalBreakpoint);
begin
  if ABreakPoint=FCurrentBreakpoint then
    FCurrentBreakpoint := nil;
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

class function TDbgProcess.StartInstance(AFileName: string; AParams, AnEnvironment: TStrings;
  AWorkingDirectory, AConsoleTty: string; AFlags: TStartInstanceFlags): TDbgProcess;
begin
  DebugLn(DBG_VERBOSE, 'Debug support is not available for this platform.');
  result := nil;
end;

class function TDbgProcess.AttachToInstance(AFileName: string; APid: Integer
  ): TDbgProcess;
begin
  DebugLn(DBG_VERBOSE, 'Attach not supported');
  Result := nil;
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

function TDbgProcess.GetLastEventProcessIdentifier: THandle;
begin
  result := 0;
end;

function TDbgProcess.DoBreak(BreakpointAddress: TDBGPtr; AThreadID: integer): Boolean;
var
  BList: TFpInternalBreakpointArray;
  i: Integer;
begin
  Result := False;

  BList := FBreakMap.GetInternalBreaksAtLocation(BreakpointAddress);
  if BList = nil then exit;
  i := 0;
  FCurrentBreakpoint := nil;
  while (i < Length(BList)) and (FCurrentBreakpoint = nil) do
    if BList[0].FInternal then
      inc(i)
    else
      FCurrentBreakpoint := BList[i];
  if FCurrentBreakpoint = nil then Exit;

  Result := True;
  if not FCurrentBreakpoint.Hit(AThreadId, BreakpointAddress)
  then FCurrentBreakpoint := nil; // no need for a singlestep if we continue
end;

function TDbgProcess.InsertBreakInstructionCode(const ALocation: TDBGPtr; out
  OrigValue: Byte): Boolean;
var
  i: Integer;
begin
  Result := FProcess.ReadData(ALocation, 1, OrigValue);
  if not Result then begin
    DebugLn(DBG_WARNINGS or DBG_BREAKPOINTS, 'Unable to read pre-breakpoint at '+FormatAddress(ALocation));
    exit;
  end;

  if OrigValue = Int3 then
    exit; // breakpoint on a hardcoded breakpoint

  // TODO: maybe remove, when TempRemoveBreakInstructionCode is called by "OS"Classes.Continue, which means no breakpoint can be set, while TempRemove are active
  for i := 0 to high(FTmpRemovedBreaks) do
    if ALocation = FTmpRemovedBreaks[i] then
      exit;

  BeforeChangingInstructionCode(ALocation);

  Result := FProcess.WriteData(ALocation, 1, Int3);
  DebugLn(DBG_VERBOSE or DBG_BREAKPOINTS, ['Breakpoint Int3 set to '+FormatAddress(ALocation), ' Result:',Result, ' OVal:', OrigValue]);
  if not Result then
    DebugLn(DBG_WARNINGS or DBG_BREAKPOINTS, 'Unable to set breakpoint at '+FormatAddress(ALocation));

  if Result then
    AfterChangingInstructionCode(ALocation);
end;

function TDbgProcess.RemoveBreakInstructionCode(const ALocation: TDBGPtr;
  const OrigValue: Byte): Boolean;
begin
  if OrigValue = Int3 then
    exit(True); // breakpoint on a hardcoded breakpoint

  BeforeChangingInstructionCode(ALocation);

  Result := WriteData(ALocation, 1, OrigValue);
  DebugLn(DBG_VERBOSE or DBG_BREAKPOINTS, ['Breakpoint Int3 removed from '+FormatAddress(ALocation), ' Result:',Result, ' OVal:', OrigValue]);
  DebugLn((not Result) and (not GotExitProcess) and (DBG_WARNINGS or DBG_BREAKPOINTS), 'Unable to reset breakpoint at %s', [FormatAddress(ALocation)]);

  if Result then
    AfterChangingInstructionCode(ALocation);
end;

procedure TDbgProcess.RemoveAllBreakPoints;
var
  b: TFpInternalBreakpoint;
  i: LongInt;
begin
  i := FBreakpointList.Count - 1;
  while i >= 0 do begin
    b := FBreakpointList[i];
    b.ResetBreak;
    b.FProcess := nil;
    FBreakpointList.Delete(i);
    dec(i);
  end;
  assert(FBreakMap.Count = 0, 'TDbgProcess.RemoveAllBreakPoints: FBreakMap.Count = 0');
end;

procedure TDbgProcess.BeforeChangingInstructionCode(const ALocation: TDBGPtr);
begin
  //
end;

procedure TDbgProcess.AfterChangingInstructionCode(const ALocation: TDBGPtr);
begin
  //
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
var
  OVal: Byte;
  l, i: Integer;
begin
  DebugLn(DBG_VERBOSE or DBG_BREAKPOINTS, ['>>> TempRemoveBreakInstructionCode']);
  l := length(FTmpRemovedBreaks);
  for i := 0 to l-1 do
    if FTmpRemovedBreaks[i] = ALocation then
      exit;

  OVal := FBreakMap.GetOrigValueAtLocation(ALocation);
  if OVal = Int3 then
    exit;

  SetLength(FTmpRemovedBreaks, l+1);
  FTmpRemovedBreaks[l] := ALocation;
  RemoveBreakInstructionCode(ALocation, OVal); // Do not update FBreakMap
  DebugLn(DBG_VERBOSE or DBG_BREAKPOINTS, ['<<< TempRemoveBreakInstructionCode']);
end;

procedure TDbgProcess.RestoreTempBreakInstructionCodes;
var
  OVal: Byte;
  t: array of TDBGPtr;
  i: Integer;
begin
  if Length(FTmpRemovedBreaks) = 0 then
    exit;
  DebugLnEnter(DBG_VERBOSE or DBG_BREAKPOINTS, ['>>> RestoreTempBreakInstructionCodes']);
  t := FTmpRemovedBreaks;
  FTmpRemovedBreaks := nil;
  for i := 0 to length(t) - 1 do
    if FBreakMap.HasId(t[i]) then // may have been removed
      InsertBreakInstructionCode(t[i], OVal);
  DebugLnExit(DBG_VERBOSE or DBG_BREAKPOINTS, ['<<< RestoreTempBreakInstructionCodes']);
end;

function TDbgProcess.HasInsertedBreakInstructionAtLocation(
  const ALocation: TDBGPtr): Boolean;
begin
  Result := FBreakMap.HasInsertedBreakInstructionAtLocation(ALocation);
end;

procedure TDbgProcess.MaskBreakpointsInReadData(const AAdress: TDbgPtr; const ASize: Cardinal; var AData);
var
  Brk: TBreakLocationEntry;
begin
  for Brk in FBreakMap do begin
    if (Brk.Location >= AAdress) and (Brk.Location < (AAdress+ASize)) then
      TByteArray(AData)[Brk.Location-AAdress] := Brk.OrigValue;
  end;
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

{ TDbgThread }

function TDbgThread.GetRegisterValueList: TDbgRegisterValueList;
begin
  if not FRegisterValueListValid then
    LoadRegisterValues;
  result := FRegisterValueList;
end;

function TDbgThread.CompareStepInfo: TFPDCompareStepInfo;
var
  AnAddr: TDBGPtr;
  Sym: TFpSymbol;
begin
  AnAddr := GetInstructionPointerRegisterValue;
  sym := FProcess.FindProcSymbol(AnAddr);
  if assigned(sym) then
  begin
    if (((FStoreStepSrcFilename=sym.FileName) and (FStoreStepSrcLineNo=sym.Line)) {or FStepOut}) and
              (FStoreStepFuncAddr=sym.Address.Address) then
      result := dcsiSameLine
    else if sym.Line = 0 then
      result := dcsiNoLineInfo
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
    CU.GetLineAddresses(sym.FileName, sym.Line, a);
    for b in a do begin
      Result := b = AnAddr;
      if Result then break;
    end;
  end
  else
    Result := True;
  sym.ReleaseReference;
end;

procedure TDbgThread.StoreStepInfo;
var
  AnAddr: TDBGPtr;
  Sym: TFpSymbol;
begin
  FStoreStepStackFrame := GetStackBasePointerRegisterValue;
  AnAddr := GetInstructionPointerRegisterValue;
  sym := FProcess.FindProcSymbol(AnAddr);
  if assigned(sym) then
  begin
    FStoreStepSrcFilename:=sym.FileName;
    FStoreStepSrcLineNo:=sym.Line;
    FStoreStepFuncAddr:=sym.Address.Address;
    sym.ReleaseReference;
  end
  else
    FStoreStepSrcLineNo:=-1;
end;

procedure TDbgThread.LoadRegisterValues;
begin
  // Do nothing
end;

procedure TDbgThread.DoBeforeBreakLocationMapChange;
var
  t: TDBGPtr;
begin
  if (FPausedAtRemovedBreakPointState <> rbUnknown) and
     (FPausedAtRemovedBreakPointAddress = GetInstructionPointerRegisterValue) then
    exit;

  t := GetInstructionPointerRegisterValue;
  if (t <> 0) and Process.HasInsertedBreakInstructionAtLocation(t - 1) then begin
  (* There is a chance, that the code jumped to this Addr, instead of executing the breakpoint.
     But if the next signal for this thread is a breakpoint at this address, then
     it must be handled (even if the breakpoint has been removed since)
  *)
    FPausedAtRemovedBreakPointAddress := t;
    FPausedAtRemovedBreakPointState := rbFound;
    // Most likely the debugger should see the previous address (unless we got here
    // by jump.
    // Call something like ResetInstructionPointerAfterBreakpointForPendingSignal; virtual;
    ////ResetInstructionPointerAfterBreakpoint;
  end
  else
    FPausedAtRemovedBreakPointState := rbNone;
end;

procedure TDbgThread.ValidateRemovedBreakPointInfo;
begin
  if (FPausedAtRemovedBreakPointState <> rbUnknown) and
     (FPausedAtRemovedBreakPointAddress <> GetInstructionPointerRegisterValue)
  then
    FPausedAtRemovedBreakPointState := rbUnknown;
end;

constructor TDbgThread.Create(const AProcess: TDbgProcess; const AID: Integer; const AHandle: THandle);
begin
  FID := AID;
  FHandle := AHandle;
  FProcess := AProcess;
  FRegisterValueList:=TDbgRegisterValueList.Create;
  inherited Create;
end;

function TDbgThread.HasInsertedBreakInstructionAtLocation(const ALocation: TDBGPtr): Boolean;
var
  t: TDBGPtr;
begin
  t := GetInstructionPointerRegisterValue;
  Result := ( (FPausedAtRemovedBreakPointState = rbFound) and
              (FPausedAtRemovedBreakPointAddress = t) ) or
            ( (t <> 0) and Process.HasInsertedBreakInstructionAtLocation(t - 1) );
debugln(['####### CHECK ',result, ' for id ', ID, ' stored ', FPausedAtRemovedBreakPointState=rbFound, ' ',FPausedAtRemovedBreakPointAddress=t, ' ',dbghex(t), ' ', dbghex(FPausedAtRemovedBreakPointAddress)]);
end;

procedure TDbgThread.CheckAndResetInstructionPointerAfterBreakpoint;
var
  t: TDBGPtr;
begin
  // todo: check that the breakpoint is NOT in the temp removed list
  t := GetInstructionPointerRegisterValue;
  if (t <> 0) and HasInsertedBreakInstructionAtLocation(t - 1)
  then begin
    FPausedAtRemovedBreakPointState := rbFound;
    ResetInstructionPointerAfterBreakpoint;
  end;
end;

procedure TDbgThread.BeforeContinue;
begin
  // On Windows this is only called, if this was the signalled thread
  FPausedAtRemovedBreakPointState := rbUnknown;
  FPausedAtRemovedBreakPointAddress := 0;
end;

function TDbgThread.AddWatchpoint(AnAddr: TDBGPtr): integer;
begin
  DebugLn(DBG_VERBOSE, 'Hardware watchpoints are not available.');
  result := -1;
end;

function TDbgThread.RemoveWatchpoint(AnId: integer): boolean;
begin
  DebugLn(DBG_VERBOSE, 'Hardware watchpoints are not available: '+self.classname);
  result := false;
end;

function TDbgThread.DetectHardwareWatchpoint: integer;
begin
  result := -1;
end;

procedure TDbgThread.PrepareCallStackEntryList(AFrameRequired: Integer);
const
  MAX_FRAMES = 50000; // safety net
var
  Address, Frame, LastFrame: QWord;
  Size, CountNeeded, IP, BP: integer;
  AnEntry: TDbgCallstackEntry;
  R: TDbgRegisterValue;
  nIP, nBP: String;
  NextIdx: LongInt;
begin
  // TODO: use AFrameRequired // check if already partly done
  if FCallStackEntryList = nil then
    FCallStackEntryList := TDbgCallstackEntryList.Create;
  if (AFrameRequired >= 0) and (AFrameRequired < FCallStackEntryList.Count) then
    exit;

  case FProcess.Mode of
    dm32: begin
      Size := 4;
      IP := 8; // Dwarf Reg Num EIP
      BP := 5; // EBP
      nIP := 'eip';
      nBP := 'ebp';
    end;
    dm64: begin
      Size := 8;
      IP := 16; // Dwarf Reg Num RIP
      BP := 6; // RBP
      nIP := 'rip';
      nBP := 'rbp';
    end;
    else assert(False, 'unknown address size for stack')
  end;

  FCallStackEntryList.FreeObjects:=true;

  if FCallStackEntryList.Count > 0 then begin
    AnEntry := FCallStackEntryList[FCallStackEntryList.Count - 1];
    R := AnEntry.RegisterValueList.FindRegisterByDwarfIndex(IP);
    if R = nil then exit;
    Address := R.NumValue;
    R := AnEntry.RegisterValueList.FindRegisterByDwarfIndex(BP);
    if R = nil then exit;
    Frame := R.NumValue;
  end
  else begin
    Address := GetInstructionPointerRegisterValue;
    Frame := GetStackBasePointerRegisterValue;
    AnEntry := TDbgCallstackEntry.create(Self, 0, Frame, Address);
    // Top level could be without entry in registerlist / same as GetRegisterValueList / but some code tries to find it here ....
    AnEntry.RegisterValueList.DbgRegisterAutoCreate[nIP].SetValue(Address, IntToStr(Address),Size, IP);
    AnEntry.RegisterValueList.DbgRegisterAutoCreate[nBP].SetValue(Frame, IntToStr(Frame),Size, BP);
    FCallStackEntryList.Add(AnEntry);
  end;

  NextIdx := FCallStackEntryList.Count;
  if AFrameRequired < 0 then
    AFrameRequired := MaxInt;
  CountNeeded := AFrameRequired - FCallStackEntryList.Count;
  LastFrame := 0;
  while (CountNeeded > 0) and (Frame <> 0) and (Frame > LastFrame) do
  begin
    LastFrame := Frame;
    if not Process.ReadData(Frame + Size, Size, Address) or (Address = 0) then Break;
    if not Process.ReadData(Frame, Size, Frame) then Break;
    AnEntry := TDbgCallstackEntry.create(Self, NextIdx, Frame, Address);
    AnEntry.RegisterValueList.DbgRegisterAutoCreate[nIP].SetValue(Address, IntToStr(Address),Size, IP);
    AnEntry.RegisterValueList.DbgRegisterAutoCreate[nBP].SetValue(Frame, IntToStr(Frame),Size, BP);
    FCallStackEntryList.Add(AnEntry);
    Dec(CountNeeded);
    inc(NextIdx);
    If (NextIdx > MAX_FRAMES) then
      break;
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
  ClearCallStack;
  FreeAndNil(FCallStackEntryList);
  inherited;
end;

{ TDbgBreak }

constructor TFpInternalBreakpoint.Create(const AProcess: TDbgProcess;
  const ALocation: TDBGPtrArray);
begin
  FProcess := AProcess;
  FProcess.FBreakpointList.Add(Self);
  FLocation := ALocation;
  inherited Create;
  SetBreak;
end;

destructor TFpInternalBreakpoint.Destroy;
begin
  if FProcess <> nil then
    FProcess.FBreakpointList.Remove(Self);
  ResetBreak;
  inherited;
end;

function TFpInternalBreakpoint.Hit(const AThreadID: Integer;
  ABreakpointAddress: TDBGPtr): Boolean;
begin
  Result := False;
  assert(FProcess<>nil, 'TFpInternalBreakpoint.Hit: FProcess<>nil');
  if //FProcess.FBreakMap.HasId(ABreakpointAddress) and
     (FProcess.FBreakMap.GetOrigValueAtLocation(ABreakpointAddress) = TDbgProcess.Int3)
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

procedure TFpInternalBreakpoint.ResetBreak;
var
  i: Integer;
begin
  if FProcess = nil then
    exit;
  for i := 0 to High(FLocation) do
    FProcess.FBreakMap.RemoveLocotion(FLocation[i], Self);
end;

procedure TFpInternalBreakpoint.SetBreak;
var
  i: Integer;
begin
  if FProcess = nil then
    exit;
  for i := 0 to High(FLocation) do
    FProcess.FBreakMap.AddLocotion(FLocation[i], Self, True);
end;

initialization
  GOSDbgClasses := nil;

  DBG_VERBOSE := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );
  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );
  DBG_BREAKPOINTS := DebugLogger.FindOrRegisterLogGroup('DBG_BREAKPOINTS' {$IFDEF DBG_BREAKPOINTS} , True {$ENDIF} );


finalization
  GOSDbgClasses.Free;
end.
