{ $Id$ }
{
 ---------------------------------------------------------------------------
 dbgclasses.pp  -  Native freepascal debugger
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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit DbgClasses;
{$mode objfpc}{$H+}
interface

uses
  Windows, Classes, Maps, DbgUtil, DbgWinExtra, DbgLoader;

type
  TDbgPtr = QWord; // PtrUInt;

  TDbgProcess = class;

  TDbgThread = class(TObject)
  private
    FProcess: TDbgProcess;
    FID: Integer;
    FHandle: THandle;
    FBaseAddr: Pointer;
    FStartAddr: Pointer;
    FSingleStepping: Boolean;
  protected
  public
    constructor Create(const AProcess: TDbgProcess; const AID: Integer; const AHandle: THandle; const ABase, AStart: Pointer);
    destructor Destroy; override;
    function SingleStep: Boolean;
    property ID: Integer read FID;
    property Handle: THandle read FHandle;
    property SingleStepping: boolean read FSingleStepping;
  end;
  
  TDbgSymbolKind = (
    skNone,          // undefined type
    skUser,          // userdefined type, this sym refers to another sym defined elswhere
    skInstance,      // the main exe/dll, containing all other syms
    skUnit,          // contains syms defined in this unit
    //--------------------------------------------------------------------------
    skRecord,        // the address member is the relative location within the
    skObject,        // structure
    skClass,
    skInterface,
    skProcedure,
    skFunction,
    //--------------------------------------------------------------------------
    skArray,
    //--------------------------------------------------------------------------
    skInteger,       // Basic types, these cannot have references or children
    skCardinal,      // only size matters ( char(1) = Char, char(2) = WideChar
    skBoolean,       // cardinal(1) = Byte etc.
    skChar,
    skFloat,
    skString,
    skAnsiString,
    skCurrency,
    skVariant,
    skWideString,
    skEnum,
    skSet,
    //--------------------------------------------------------------------------
    skRegister       // the Address member is the register number
    //--------------------------------------------------------------------------
  );

  TDbgSymbolFlag =(
    sfPointer,       // The sym is a pointer to the reference
    sfConst,         // The sym is a constan and cannot be modified
    sfVar,
    sfOut,
    sfpropGet,
    sfPropSet,
    sfPropStored
  );
  TDbgSymbolFlags = set of TDbgSymbolFlag;

  { TDbgSymbol }

  TDbgSymbol = class(TObject)
  private
    FName: String;
    FKind: TDbgSymbolKind;
    FAddress: TDbgPtr;
  protected
    function GetChild(AIndex: Integer): TDbgSymbol; virtual;
    function GetColumn: Cardinal; virtual;
    function GetCount: Integer; virtual;
    function GetFile: String; virtual;
    function GetFlags: TDbgSymbolFlags; virtual;
    function GetLine: Cardinal; virtual;
    function GetParent: TDbgSymbol; virtual;
    function GetReference: TDbgSymbol; virtual;
    function GetSize: Integer; virtual;
  public
    constructor Create(const AName: String; AKind: TDbgSymbolKind; AAddress: TDbgPtr);
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Name: String read FName;
    property Kind: TDbgSymbolKind read FKind;
    property Address: TDbgPtr read FAddress;
    property Size: Integer read GetSize;
    property FileName: String read GetFile;
    property Line: Cardinal read GetLine;
    property Column: Cardinal read GetColumn;
    property Flags: TDbgSymbolFlags read GetFlags;
    property Reference: TDbgSymbol read GetReference;
    property Parent: TDbgSymbol read GetParent;
    property Children[AIndex: Integer]: TDbgSymbol read GetChild;
  end;


  { TDbgInfo }

  TDbgInfo = class(TObject)
  private
    FHasInfo: Boolean;
  protected
    procedure SetHasInfo;
  public
    constructor Create(ALoader: TDbgImageLoader); virtual;
    function FindSymbol(const AName: String): TDbgSymbol; virtual;
    function FindSymbol(AAddress: TDbgPtr): TDbgSymbol; virtual;
    property HasInfo: Boolean read FHasInfo;
    function GetLineAddress(const AFileName: String; ALine: Cardinal): TDbgPtr; virtual;
  end;


  TDbgBreakpoint = class;
  TDbgBreakpointEvent = procedure(const ASender: TDbgBreakpoint; const AContext: TContext) of object;
  TDbgBreakpoint = class(TObject)
  private
    FProcess: TDbgProcess;
    FLocation: TDbgPtr;
    FOrgValue: Byte;
    procedure SetBreak;
    procedure ResetBreak;
  protected
  public
    constructor Create(const AProcess: TDbgProcess; const ALocation: TDbgPtr);
    destructor Destroy; override;
    function Hit(const AThreadID: Integer): Boolean;
    property Location: TDbgPtr read FLocation;
  end;


  { TDbgInstance }

  TDbgInstance = class(TObject)
  private
    FName: String;
    FProcess: TDbgProcess;
    FModuleHandle: THandle;
    FBaseAddr: TDbgPtr;
    FBreakList: TList;
    FDbgInfo: TDbgInfo;
    FLoader: TDbgImageLoader;

    procedure LoadInfo;
    procedure CheckName;
    procedure SetName(const AValue: String);
  public
    constructor Create(const AProcess: TDbgProcess; const ADefaultName: String; const AModuleHandle: THandle; const ABaseAddr, ANameAddr: TDbgPtr; const AUnicode: Boolean);
    destructor Destroy; override;

    function AddBreak(const AFileName: String; ALine: Cardinal): TDbgBreakpoint;
    function AddrOffset: Int64;  // gives the offset between  the loaded addresses and the compiled addresses
    function FindSymbol(AAdress: TDbgPtr): TDbgSymbol;
    function RemoveBreak(const AFileName: String; ALine: Cardinal): Boolean;

    property Process: TDbgProcess read FProcess;
    property ModuleHandle: THandle read FModuleHandle;
    property BaseAddr: TDbgPtr read FBaseAddr;
  end;

  TDbgLibrary = class(TDbgInstance)
  private
  public
    constructor Create(const AProcess: TDbgProcess; const ADefaultName: String; const AInfo: TLoadDLLDebugInfo);
    property Name: String read FName;
  end;

  { TDbgProcess }

  TDbgProcess = class(TDbgInstance)
  private
    FProcessID: Integer;
    FThreadID: Integer;
    FInfo: TCreateProcessDebugInfo;

    FThreadMap: TMap; // map ThreadID -> ThreadObject
    FLibMap: TMap;    // map LibAddr -> LibObject
    FBreakMap: TMap;  // map BreakAddr -> BreakObject
    
    FSymInstances: TList;  // list of dbgInstances with debug info

    FMainThread: TDbgThread;

    FCurrentBreakpoint: TDbgBreakpoint;  // set if we are executing the code at the break
                                         // if the singlestep is done, set the break again
    FReEnableBreakStep: Boolean;         // Set when we are reenabling a breakpoint
                                         // We need a single step, so the IP is after the break to set

    procedure SetName(const AValue: String);
    procedure ThreadDestroyed(const AThread: TDbgThread);
  protected
  public
    constructor Create(const ADefaultName: String; const AProcessID, AThreadID: Integer; const AInfo: TCreateProcessDebugInfo);
    destructor Destroy; override;
    function  AddBreak(const ALocation: TDbgPtr): TDbgBreakpoint;
    function  AddLib(const AInfo: TLoadDLLDebugInfo): TDbgLibrary;
    procedure AddThread(const AID: Integer; const AInfo: TCreateThreadDebugInfo);
    function  FindSymbol(const AName: String): TDbgSymbol;
    function  FindSymbol(AAdress: TDbgPtr): TDbgSymbol;
    function  GetLib(const AHandle: THandle; out ALib: TDbgLibrary): Boolean;
    function  GetThread(const AID: Integer; out AThread: TDbgThread): Boolean;
    procedure Interrupt;
    procedure ContinueDebugEvent(const AThread: TDbgThread; const ADebugEvent: TDebugEvent);
    function  HandleDebugEvent(const ADebugEvent: TDebugEvent): Boolean;
    function  RemoveBreak(const ALocation: TDbgPtr): Boolean;
    procedure RemoveLib(const AInfo: TUnloadDLLDebugInfo);
    procedure RemoveThread(const AID: DWord);

    function ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean;
    function ReadOrdinal(const AAdress: TDbgPtr; out AData): Boolean;
    function ReadString(const AAdress: TDbgPtr; const AMaxSize: Cardinal; out AData: String): Boolean;
    function ReadWString(const AAdress: TDbgPtr; const AMaxSize: Cardinal; out AData: WideString): Boolean;

    function WriteData(const AAdress: TDbgPtr; const ASize: Cardinal; const AData): Boolean;

    property Handle: THandle read FInfo.hProcess;
    property Name: String read FName write SetName;
  end;



implementation

uses
  SysUtils, DbgDwarf;

procedure LogLastError;
begin
  WriteLN('ERROR: ', GetLastErrorText);
end;

{ TDbgInstance }

function TDbgInstance.AddBreak(const AFileName: String; ALine: Cardinal): TDbgBreakpoint;
var
  addr: TDbgPtr;
begin
  Result := nil;
  if not FDbgInfo.HasInfo then Exit;
  addr := FDbgInfo.GetLineAddress(AFileName, ALine);
  if addr = 0 then Exit;
  Result := FProcess.AddBreak(addr - AddrOffset);
end;

function TDbgInstance.AddrOffset: Int64;
begin
  Result := FLoader.ImageBase - BaseAddr;
end;

procedure TDbgInstance.CheckName;
begin
  if FName = ''
  then FName := Format('@%p', [Pointer(PtrUInt(FBaseAddr))]);
end;

constructor TDbgInstance.Create(const AProcess: TDbgProcess; const ADefaultName: String; const AModuleHandle: THandle; const ABaseAddr, ANameAddr: TDbgPtr; const AUnicode: Boolean);
var
  NamePtr: TDbgPtr;
  S: String;
  W: WideString;
  len: Integer;
begin
  FBaseAddr := ABaseAddr;
  FModuleHandle := AModuleHandle;
  FBreakList := TList.Create;
  FProcess := AProcess;

  inherited Create;

  W := '';
  if AProcess.ReadOrdinal(ANameAddr, NamePtr)
  then begin
    if AUnicode
    then begin
      AProcess.ReadWString(NamePtr, MAX_PATH, W);
    end
    else begin
      if AProcess.ReadString(NamePtr, MAX_PATH, S)
      then W := S;
    end;
  end;

  if W = ''
  then begin
    SetLength(S, MAX_PATH);
    len := GetModuleFileName(FModuleHandle, @S[1], MAX_PATH);
    if len > 0
    then SetLength(S, len - 1)
    else begin
      S := '';
      LogLastError;
    end;
    W := S;
  end;

  if W = ''
  then W := ADefaultName;

  SetName(W);
  
  LoadInfo;
end;

destructor TDbgInstance.Destroy;
var
  n: integer;
begin
  for n := 0 to FBreakList.Count - 1 do
  begin
    Process.RemoveBreak(TDbgBreakpoint(FBreakList[n]).FLocation);
  end;
  FBreakList.Clear;

  FreeAndNil(FBreakList);
  FreeAndNil(FDbgInfo);
  FreeAndNil(FLoader);
  inherited;
end;

function TDbgInstance.FindSymbol(AAdress: TDbgPtr): TDbgSymbol;
begin
  Result := FDbgInfo.FindSymbol(AAdress + AddrOffset);
end;

procedure TDbgInstance.LoadInfo;
begin
  FLoader := TDbgWinPEImageLoader.Create(FModuleHandle);
  FDbgInfo := TDbgDwarf.Create(FLoader);
  TDbgDwarf(FDbgInfo).LoadCompilationUnits;
end;

function TDbgInstance.RemoveBreak(const AFileName: String; ALine: Cardinal): Boolean;
var
  addr: TDbgPtr;
begin
  Result := False;
  if not FDbgInfo.HasInfo then Exit;
  addr := FDbgInfo.GetLineAddress(AFileName, ALine);
  if addr = 0 then Exit;
  Result := FProcess.RemoveBreak(addr - AddrOffset);
end;

procedure TDbgInstance.SetName(const AValue: String);
begin
  FName := AValue;
  CheckName;
end;

{ TDbgLibrary }

constructor TDbgLibrary.Create(const AProcess: TDbgProcess; const ADefaultName: String; const AInfo: TLoadDLLDebugInfo);
begin
  inherited Create(AProcess, ADefaultName, AInfo.hFile, TDbgPtr(AInfo.lpBaseOfDll), TDbgPtr(AInfo.lpImageName), AInfo.fUnicode <> 0);
end;

{ TDbgProcess }

function TDbgProcess.AddBreak(const ALocation: TDbgPtr): TDbgBreakpoint;
begin
  Result := TDbgBreakpoint.Create(Self, ALocation);
  FBreakMap.Add(ALocation, Result);
end;

function TDbgProcess.AddLib(const AInfo: TLoadDLLDebugInfo): TDbgLibrary;
var
  ID: TDbgPtr;
begin
  Result := TDbgLibrary.Create(Self, HexValue(AInfo.lpBaseOfDll, SizeOf(Pointer), [hvfIncludeHexchar]), AInfo);
  ID := TDbgPtr(AInfo.lpBaseOfDll);
  FLibMap.Add(ID, Result);
  if Result.FDbgInfo.HasInfo
  then FSymInstances.Add(Result);
end;

procedure TDbgProcess.AddThread(const AID: Integer; const AInfo: TCreateThreadDebugInfo);
var
  Thread: TDbgThread;
begin
  Thread := TDbgThread.Create(Self, AID, AInfo.hThread, AInfo.lpThreadLocalBase, AInfo.lpStartAddress);
  FThreadMap.Add(AID, Thread);
end;

procedure TDbgProcess.ContinueDebugEvent(const AThread: TDbgThread; const ADebugEvent: TDebugEvent);
begin
  case ADebugEvent.dwDebugEventCode of
    EXCEPTION_DEBUG_EVENT: begin
      case ADebugEvent.Exception.ExceptionRecord.ExceptionCode of
        EXCEPTION_BREAKPOINT: begin
          if AThread = nil then Exit;
          if FCurrentBreakpoint = nil then Exit;
          if AThread.SingleStepping then Exit;
          AThread.SingleStep;
          FReEnableBreakStep := True;
        end;
      end;
    end;
  end;
end;

constructor TDbgProcess.Create(const ADefaultName: String; const AProcessID, AThreadID: Integer; const AInfo: TCreateProcessDebugInfo);
const
  {.$IFDEF CPU64}
  MAP_ID_SIZE = itu8;
  {.$ELSE}
//  MAP_ID_SIZE = itu4;
  {.$ENDIF}
begin
  FProcessID := AProcessID;
  FThreadID := AThreadID;
  FInfo := AInfo;
  
  FThreadMap := TMap.Create(itu4, SizeOf(TDbgThread));
  FLibMap := TMap.Create(MAP_ID_SIZE, SizeOf(TDbgLibrary));
  FBreakMap := TMap.Create(MAP_ID_SIZE, SizeOf(TDbgBreakpoint));
  FCurrentBreakpoint := nil;

  FSymInstances := TList.Create;

  inherited Create(Self, ADefaultName, AInfo.hFile, TDbgPtr(AInfo.lpBaseOfImage), TDbgPtr(AInfo.lpImageName), AInfo.fUnicode <> 0);

  FMainThread := TDbgThread.Create(Self, AThreadID, AInfo.hThread, AInfo.lpThreadLocalBase, AInfo.lpStartAddress);
  FThreadMap.Add(AThreadID, FMainThread);
  
  if FDbgInfo.HasInfo
  then FSymInstances.Add(Self);
end;

destructor TDbgProcess.Destroy;
begin
//  CloseHandle(FInfo.hThread);
  CloseHandle(FInfo.hProcess);
  FreeAndNil(FBreakMap);
  FreeAndNil(FThreadMap);
  FreeAndNil(FLibMap);
  FreeAndNil(FSymInstances);
  inherited;
end;

function TDbgProcess.FindSymbol(const AName: String): TDbgSymbol;
begin
  Result := FDbgInfo.FindSymbol(AName);
end;

function TDbgProcess.FindSymbol(AAdress: TDbgPtr): TDbgSymbol;
var
  n: Integer;
  Inst: TDbgInstance;
begin
  for n := 0 to FSymInstances.Count - 1 do
  begin
    Inst := TDbgInstance(FSymInstances[n]);
    Result := Inst.FindSymbol(AAdress);
    if Result <> nil then Exit;
  end;
  Result := nil;
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
  Result := FThreadMap.GetData(AID, Thread) and (Thread <> nil);
  if Result
  then AThread := Thread
  else Log('Unknown thread ID %u for process %u', [AID, FProcessID]);
end;

{ ------------------------------------------------------------------
  HandleDebugEvent
  
  Result: True if the event was triggered internally
          The callee should continue the process
  ------------------------------------------------------------------ }
function TDbgProcess.HandleDebugEvent(const ADebugEvent: TDebugEvent): Boolean;

  function DoBreak: Boolean;
  var
    ID: TDbgPtr;
  begin
    Result := False;
    ID := TDbgPtr(ADebugEvent.Exception.ExceptionRecord.ExceptionAddress);
    if not FBreakMap.GetData(ID, FCurrentBreakpoint) then Exit;
    if FCurrentBreakpoint = nil then Exit;

    Result := True;
    if not FCurrentBreakpoint.Hit(ADebugEvent.dwThreadId)
    then FCurrentBreakpoint := nil; // no need for a singlestep if we continue
  end;

  function DoSingleStep: Boolean;
  var
    _UC: record
      C: TContext;
      D: array[1..16] of Byte;
    end;
    Context: PContext;
  begin
    Result := False;
    // check if we are interupting
    Context := AlignPtr(@_UC, $10);
    Context^.ContextFlags := CONTEXT_DEBUG_REGISTERS;
    if GetThreadContext(FInfo.hThread, Context^)
    then begin
      if Context^.Dr6 and 1 <> 0
      then begin
        // interrupt !
        // disable break.
        Context^.Dr7 := Context^.Dr7 and not $1;
        Context^.Dr0 := 0;
        if not SetThreadContext(FInfo.hThread, Context^)
        then begin
          // Heeellppp!!
          Log('Thread %u: Unable to reset BR0', [ADebugEvent.dwThreadId]);
        end;
        // check if we are also singlestepping
        // if not, then exit, else proceed to next check
        if Context^.Dr6 and $40 = 0
        then Exit;
      end;
    end
    else begin
      // if we can not get the context, we probable weren't able to set it either
      Log('Thread %u: Unable to get context', [ADebugEvent.dwThreadId]);
    end;

    // check if we are single stepping ourself
    if FCurrentBreakpoint = nil then Exit;

    FCurrentBreakpoint.SetBreak;
    FCurrentBreakpoint := nil;
    Result := FReEnableBreakStep;
    FReEnableBreakStep := False;
  end;
  
begin
  Result := False;
  case ADebugEvent.dwDebugEventCode of
    EXCEPTION_DEBUG_EVENT: begin
      case ADebugEvent.Exception.ExceptionRecord.ExceptionCode of
        EXCEPTION_BREAKPOINT:  {Result :=} DoBreak; // we never set a break ourself, let the callee pause!
        EXCEPTION_SINGLE_STEP: Result := DoSingleStep;
      end;
    end;
    CREATE_THREAD_DEBUG_EVENT: begin
      AddThread(ADebugEvent.dwThreadId, ADebugEvent.CreateThread)
    end;
    EXIT_THREAD_DEBUG_EVENT: begin
      RemoveThread(ADebugEvent.dwThreadId);
    end;
    LOAD_DLL_DEBUG_EVENT: begin
      AddLib(ADebugEvent.LoadDll);
    end;
    UNLOAD_DLL_DEBUG_EVENT: begin
      RemoveLib(ADebugEvent.UnloadDll);
    end;
  end;
end;

procedure TDbgProcess.Interrupt;
var
  _UC: record
    C: TContext;
    D: array[1..16] of Byte;
  end;
  Context: PContext;
  r: DWORD;
begin
  // Interrupting is implemented by suspending the thread and set DB0 to the
  // (to be) executed EIP. When the thread is resumed, it will generate a break
  // Single stepping doesn't work in all cases.
  
  // A context needs to be aligned to 16 bytes. Unfortunately, the compiler has
  // no directive for this, so align it somewhere in our "reserved" memory
  Context := AlignPtr(@_UC, $10);
  r := SuspendThread(FInfo.hThread);
  try
    Context^.ContextFlags := CONTEXT_CONTROL or CONTEXT_DEBUG_REGISTERS;
    if not GetThreadContext(FInfo.hThread, Context^)
    then begin
      Log('Proces %u interrupt: Unable to get context', [FProcessID]);
      Exit;
    end;

    Context^.ContextFlags := CONTEXT_DEBUG_REGISTERS;
    {$ifdef cpui386}
    Context^.Dr0 := Context^.Eip;
    {$else}
    Context^.Dr0 := Context^.Rip;
    {$endif}
    Context^.Dr7 := (Context^.Dr7 and $FFF0FFFF) or $1;

    if not SetThreadContext(FInfo.hThread, Context^)
    then begin
      Log('Proces %u interrupt: Unable to set context', [FProcessID]);
      Exit;
    end;
  finally
    r := ResumeTHread(FInfo.hThread);
  end;
end;

function TDbgProcess.ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean;
var
  BytesRead: Cardinal;
begin
  Result := ReadProcessMemory(Handle, Pointer(PtrUInt(AAdress)), @AData, ASize, BytesRead) and (BytesRead = ASize);

  if not Result then LogLastError;
end;

function TDbgProcess.ReadOrdinal(const AAdress: TDbgPtr; out AData): Boolean;
begin
  Result := ReadData(AAdress, 4, AData);
end;

function TDbgProcess.ReadString(const AAdress: TDbgPtr; const AMaxSize: Cardinal; out AData: String): Boolean;
var
  BytesRead: Cardinal;
  buf: array of Char;
begin
  SetLength(buf, AMaxSize + 1);
  Result := ReadProcessMemory(Handle, Pointer(PtrUInt(AAdress)), @Buf[0], AMaxSize, BytesRead);
  if not Result then Exit;
  if BytesRead < AMaxSize
  then Buf[BytesRead] := #0
  else Buf[AMaxSize] := #0;
  AData := PChar(@Buf[0]);
end;

function TDbgProcess.ReadWString(const AAdress: TDbgPtr; const AMaxSize: Cardinal; out AData: WideString): Boolean;
var
  BytesRead: Cardinal;
  buf: array of WChar;
begin
  SetLength(buf, AMaxSize + 1);
  Result := ReadProcessMemory(Handle, Pointer(PtrUInt(AAdress)), @Buf[0], SizeOf(WChar) * AMaxSize, BytesRead);
  if not Result then Exit;
  BytesRead := BytesRead div SizeOf(WChar);
  if BytesRead < AMaxSize
  then Buf[BytesRead] := #0
  else Buf[AMaxSize] := #0;
  AData := PWChar(@Buf[0]);
end;

function TDbgProcess.RemoveBreak(const ALocation: TDbgPtr): Boolean;
begin
  if FBreakMap = nil
  then Result := False
  else Result := FBreakMap.Delete(ALocation);
end;

procedure TDbgProcess.RemoveLib(const AInfo: TUnloadDLLDebugInfo);
var
  Lib: TDbgLibrary;
  ID: TDbgPtr;
begin
  if FLibMap = nil then Exit;
  ID := TDbgPtr(AInfo.lpBaseOfDll);
  if not FLibMap.GetData(ID, Lib) then Exit;
  if Lib.FDbgInfo.HasInfo
  then FSymInstances.Remove(Lib);
  FLibMap.Delete(ID);
  // TODO: Free lib ???
end;

procedure TDbgProcess.RemoveThread(const AID: DWord);
begin
  if FThreadMap = nil then Exit;
  FThreadMap.Delete(AID);
end;

procedure TDbgProcess.SetName(const AValue: String);
begin
  FName := AValue;
end;

procedure TDbgProcess.ThreadDestroyed(const AThread: TDbgThread);
begin
  if AThread = FMainThread
  then FMainThread := nil;
end;

function TDbgProcess.WriteData(const AAdress: TDbgPtr; const ASize: Cardinal; const AData): Boolean;
var
  BytesWritten: Cardinal;
begin
  Result := WriteProcessMemory(Handle, Pointer(PtrUInt(AAdress)), @AData, ASize, BytesWritten) and (BytesWritten = ASize);

  if not Result then LogLastError;
end;

{ TDbgThread }

constructor TDbgThread.Create(const AProcess: TDbgProcess; const AID: Integer; const AHandle: THandle; const ABase, AStart: Pointer);
begin
  FID := AID;
  FHandle := AHandle;
  FBaseAddr := ABase;
  FStartAddr := AStart;
  FProcess := AProcess;

  inherited Create;
end;

destructor TDbgThread.Destroy;
begin
  FProcess.ThreadDestroyed(Self);
  inherited;
end;

function TDbgThread.SingleStep: Boolean;
var
  _UC: record
    C: TContext;
    D: array[1..16] of Byte;
  end;
  Context: PContext;
begin
  Context := AlignPtr(@_UC, $10);
  Context^.ContextFlags := CONTEXT_CONTROL;
  if not GetThreadContext(FHandle, Context^)
  then begin
    Log('Thread %u: Unable to get context', [FID]);
    Exit;
  end;

  Context^.ContextFlags := CONTEXT_CONTROL;
  Context^.EFlags := Context^.EFlags or FLAG_TRACE_BIT;

  if not SetThreadContext(FHandle, Context^)
  then begin
    Log('Thread %u: Unable to set context', [FID]);
    Exit;
  end;

  FSingleStepping := True;
end;

{ TDbgInfo }

constructor TDbgInfo.Create(ALoader: TDbgImageLoader);
begin
  inherited Create;
end;

function TDbgInfo.FindSymbol(const AName: String): TDbgSymbol;
begin
  Result := nil;
end;

function TDbgInfo.FindSymbol(AAddress: TDbgPtr): TDbgSymbol;
begin
  Result := nil;
end;

function TDbgInfo.GetLineAddress(const AFileName: String; ALine: Cardinal): TDbgPtr;
begin
  Result := 0;
end;

procedure TDbgInfo.SetHasInfo;
begin
  FHasInfo := True;
end;


{ TDbgSymbol }

constructor TDbgSymbol.Create(const AName: String; AKind: TDbgSymbolKind; AAddress: TDbgPtr);
begin
  FName := AName;
  FKind := AKind;
  FAddress := AAddress;

  inherited Create;
end;

destructor TDbgSymbol.Destroy;
begin
  inherited Destroy;
end;

function TDbgSymbol.GetChild(AIndex: Integer): TDbgSymbol;
begin
  result := nil;
end;

function TDbgSymbol.GetColumn: Cardinal;
begin
  Result := 0;
end;

function TDbgSymbol.GetCount: Integer;
begin
  Result := 0;
end;

function TDbgSymbol.GetFile: String;
begin
  Result := '';
end;

function TDbgSymbol.GetFlags: TDbgSymbolFlags;
begin
  Result := [];
end;

function TDbgSymbol.GetLine: Cardinal;
begin
  Result := 0;
end;

function TDbgSymbol.GetParent: TDbgSymbol;
begin
  Result := nil;
end;

function TDbgSymbol.GetReference: TDbgSymbol;
begin
  Result := nil;
end;

function TDbgSymbol.GetSize: Integer;
begin
  Result := 0;
end;

{ TDbgBreak }

constructor TDbgBreakpoint.Create(const AProcess: TDbgProcess; const ALocation: TDbgPtr);
begin
  FProcess := AProcess;
  FLocation := ALocation;
  inherited Create;
  SetBreak;
end;

destructor TDbgBreakpoint.Destroy;
begin
  ResetBreak;
  inherited;
end;

function TDbgBreakpoint.Hit(const AThreadID: Integer): Boolean;
var
  Thread: TDbgThread;
  _UC: record
    C: TContext;
    D: array[1..16] of Byte;
  end;
  Context: PContext;
begin
  Result := False;
  if FOrgValue = $CC then Exit; // breakpoint on a hardcoded breakpoint
                                // no need to jum back and restore instruction
  ResetBreak;

  if not FProcess.GetThread(AThreadId, Thread) then Exit;
  
  Context := AlignPtr(@_UC, $10);

  Context^.ContextFlags := CONTEXT_CONTROL;
  if not GetThreadContext(Thread.Handle, Context^)
  then begin
    Log('Break $s: Unable to get context', [HexValue(FLocation, SizeOf(Pointer), [hvfIncludeHexchar])]);
    Exit;
  end;

  Context^.ContextFlags := CONTEXT_CONTROL;
  {$ifdef cpui386}
  Dec(Context^.Eip);
  {$else}
  Dec(Context^.Rip);
  {$endif}

  if not SetThreadContext(Thread.Handle, Context^)
  then begin
    Log('Break %s: Unable to set context', [HexValue(FLocation, SizeOf(Pointer), [hvfIncludeHexchar])]);
    Exit;
  end;
  Result := True;
end;

procedure TDbgBreakpoint.ResetBreak;
begin
  if FOrgValue = $CC then Exit; // breakpoint on a hardcoded breakpoint

  if not FProcess.WriteData(FLocation, 1, FOrgValue)
  then begin
    Log('Unable to reset breakpoint at $%p', [FLocation]);
    Exit;
  end;
  FlushInstructionCache(FProcess.FInfo.hProcess, Pointer(PtrUInt(FLocation)), 1);
end;

procedure TDbgBreakpoint.SetBreak;
const
  Int3: Byte = $CC;
begin
  if not FProcess.ReadData(FLocation, 1, FOrgValue)
  then begin
    Log('Unable to read breakpoint at $%p', [FLocation]);
    Exit;
  end;

  if FOrgValue = $CC then Exit; // breakpoint on a hardcoded breakpoint

  if not FProcess.WriteData(FLocation, 1, Int3)
  then begin
    Log('Unable to set breakpoint at $%p', [FLocation]);
    Exit;
  end;
  FlushInstructionCache(FProcess.FInfo.hProcess, Pointer(PtrUInt(FLocation)), 1);
end;

end.
