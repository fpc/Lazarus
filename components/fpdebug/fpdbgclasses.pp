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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit FpDbgClasses;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Maps, FpDbgDwarf, FpDbgUtil, FpDbgWinExtra, FpDbgLoader,
  FpDbgInfo, FpdMemoryTools, LazLoggerBase, LazClasses, DbgIntfBaseTypes;

type
  TFPDState = (dsStop, dsRun, dsPause, dsQuit, dsEvent);
  TFPDEvent = (deExitProcess, deBreakpoint, deException, deCreateProcess, deLoadLibrary, deInternalContinue);
  TFPDMode = (dm32, dm64);
  TOnLog = procedure(AString: string) of object;

  TDbgProcess = class;

  TDbgThread = class(TObject)
  private
    FProcess: TDbgProcess;
    FID: Integer;
    FHandle: THandle;
    FSingleStepping: Boolean;
  protected
  public
    constructor Create(const AProcess: TDbgProcess; const AID: Integer; const AHandle: THandle); virtual;
    function ResetInstructionPointerAfterBreakpoint: boolean; virtual; abstract;
    destructor Destroy; override;
    function SingleStep: Boolean; virtual;
    property ID: Integer read FID;
    property Handle: THandle read FHandle;
    property SingleStepping: boolean read FSingleStepping;
  end;
  TDbgThreadClass = class of TDbgThread;

  TDbgBreakpoint = class(TObject)
  private
    FProcess: TDbgProcess;
    FLocation: TDbgPtr;
  protected
    FOrgValue: Byte;
    property Process: TDbgProcess read FProcess;
  public
    constructor Create(const AProcess: TDbgProcess; const ALocation: TDbgPtr); virtual;
    destructor Destroy; override;
    function Hit(const AThreadID: Integer): Boolean; virtual;
    property Location: TDbgPtr read FLocation;

    procedure SetBreak; virtual;
    procedure ResetBreak; virtual;
  end;
  TDbgBreakpointClass = class of TDbgBreakpoint;

  { TDbgInstance }

  TDbgInstance = class(TObject)
  private
    FName: String;
    FProcess: TDbgProcess;
    FBreakList: TList;
    FDbgInfo: TDbgInfo;
    FLoader: TDbgImageLoader;

  protected
    procedure LoadInfo; virtual;
    function InitializeLoader: TDbgImageLoader; virtual;
    procedure SetName(const AValue: String);
  public
    constructor Create(const AProcess: TDbgProcess); virtual;
    destructor Destroy; override;

    function AddBreak(const AFileName: String; ALine: Cardinal): TDbgBreakpoint;
    function AddrOffset: Int64; virtual;  // gives the offset between  the loaded addresses and the compiled addresses
    function FindSymbol(AAdress: TDbgPtr): TFpDbgSymbol;
    function RemoveBreak(const AFileName: String; ALine: Cardinal): Boolean;

    property Process: TDbgProcess read FProcess;
    property DbgInfo: TDbgInfo read FDbgInfo;
  end;

  { TDbgLibrary }

  TDbgLibrary = class(TDbgInstance)
  private
    FModuleHandle: THandle;
    FBaseAddr: TDBGPtr;
  public
    constructor Create(const AProcess: TDbgProcess; const ADefaultName: String; const AModuleHandle: THandle; const ABaseAddr: TDbgPtr);
    property Name: String read FName;
    property ModuleHandle: THandle read FModuleHandle;
    property BaseAddr: TDBGPtr read FBaseAddr;
  end;

  { TDbgProcess }

  TDbgProcess = class(TDbgInstance)
  private
    FExitCode: DWord;
    FOnLog: TOnLog;
    FProcessID: Integer;
    FThreadID: Integer;

    procedure ThreadDestroyed(const AThread: TDbgThread);
  protected
    FCurrentBreakpoint: TDbgBreakpoint;  // set if we are executing the code at the break
                                         // if the singlestep is done, set the break again
    FReEnableBreakStep: Boolean;         // Set when we are reenabling a breakpoint
                                         // We need a single step, so the IP is after the break to set

    FSymInstances: TList;  // list of dbgInstances with debug info

    FThreadMap: TMap; // map ThreadID -> ThreadObject
    FLibMap: TMap;    // map LibAddr -> LibObject
    FBreakMap: TMap;  // map BreakAddr -> BreakObject

    FMainThread: TDbgThread;
    function GetHandle: THandle; virtual;
    procedure SetExitCode(AValue: DWord);
    function GetLastEventProcessIdentifier: THandle; virtual;
    function DoBreak(BreakpointAddress: TDBGPtr; AThreadID: integer): Boolean;
  public
    class function StartInstance(AFileName: string; AParams: string): TDbgProcess; virtual;
    constructor Create(const AName: string; const AProcessID, AThreadID: Integer); virtual;
    destructor Destroy; override;
    function  AddBreak(const ALocation: TDbgPtr): TDbgBreakpoint;
    function  FindSymbol(const AName: String): TFpDbgSymbol;
    function  FindSymbol(AAdress: TDbgPtr): TFpDbgSymbol;
    function  GetLib(const AHandle: THandle; out ALib: TDbgLibrary): Boolean;
    function  GetThread(const AID: Integer; out AThread: TDbgThread): Boolean;
    function  RemoveBreak(const ALocation: TDbgPtr): Boolean;
    procedure RemoveThread(const AID: DWord);
    procedure Log(AString: string);
    procedure Log(AString: string; Options: array of const);

    function ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean; virtual;
    function ReadOrdinal(const AAdress: TDbgPtr; out AData): Boolean; virtual;
    function ReadString(const AAdress: TDbgPtr; const AMaxSize: Cardinal; out AData: String): Boolean; virtual;
    function ReadWString(const AAdress: TDbgPtr; const AMaxSize: Cardinal; out AData: WideString): Boolean; virtual;

    function Continue(AProcess: TDbgProcess; AThread: TDbgThread; AState: TFPDState): boolean; virtual;
    function WaitForDebugEvent(out ProcessIdentifier, ThreadIdentifier: THandle): boolean; virtual; abstract;
    function ResolveDebugEvent(AThread: TDbgThread): TFPDEvent; virtual; abstract;

    function WriteData(const AAdress: TDbgPtr; const ASize: Cardinal; const AData): Boolean; virtual;

    function GetInstructionPointerRegisterValue: TDbgPtr; virtual; abstract;
    function GetStackBasePointerRegisterValue: TDbgPtr; virtual; abstract;

    procedure TerminateProcess; virtual; abstract;

    property Handle: THandle read GetHandle;
    property Name: String read FName write SetName;
    property ProcessID: integer read FProcessID;
    property ThreadID: integer read FThreadID;
    property ExitCode: DWord read FExitCode;
    property LastEventProcessIdentifier: THandle read GetLastEventProcessIdentifier;
    property OnLog: TOnLog read FOnLog write FOnLog;
  end;
  TDbgProcessClass = class of TDbgProcess;

  TOSDbgClasses = class
  public
    DbgThreadClass : TDbgThreadClass;
    DbgBreakpointClass : TDbgBreakpointClass;
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

var
  GOSDbgClasses : TOSDbgClasses;

function OSDbgClasses: TOSDbgClasses;
begin
  if GOSDbgClasses=nil then
    begin
    GOSDbgClasses := TOSDbgClasses.create;
    GOSDbgClasses.DbgThreadClass := TDbgThread;
    GOSDbgClasses.DbgBreakpointClass := TDbgBreakpoint;
    GOSDbgClasses.DbgProcessClass := TDbgProcess;
    {$ifdef windows}
    RegisterDbgClasses;
    {$endif windows}
    {$ifdef darwin}
    RegisterDbgClasses;
    {$endif darwin}
    end;
  result := GOSDbgClasses;
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
  Result := FLoader.ImageBase;
end;

constructor TDbgInstance.Create(const AProcess: TDbgProcess);
begin
  FBreakList := TList.Create;
  FProcess := AProcess;

  inherited Create;
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

function TDbgInstance.FindSymbol(AAdress: TDbgPtr): TFpDbgSymbol;
begin
  Result := FDbgInfo.FindSymbol(AAdress + AddrOffset);
end;

procedure TDbgInstance.LoadInfo;
begin
  FLoader := InitializeLoader;
  assert(false, 'fpc will not compile this');
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
end;

function TDbgInstance.InitializeLoader: TDbgImageLoader;
begin
  result := nil;
end;

{ TDbgLibrary }

constructor TDbgLibrary.Create(const AProcess: TDbgProcess; const ADefaultName: String; const AModuleHandle: THandle; const ABaseAddr: TDbgPtr);

begin
  inherited Create(AProcess);
  FModuleHandle:=AModuleHandle;
  FBaseAddr:=ABaseAddr;
end;

{ TDbgProcess }

function TDbgProcess.AddBreak(const ALocation: TDbgPtr): TDbgBreakpoint;
begin
  Result := OSDbgClasses.DbgBreakpointClass.Create(Self, ALocation);
  FBreakMap.Add(ALocation, Result);
end;

constructor TDbgProcess.Create(const AName: string; const AProcessID, AThreadID: Integer);
const
  {.$IFDEF CPU64}
  MAP_ID_SIZE = itu8;
  {.$ELSE}
//  MAP_ID_SIZE = itu4;
  {.$ENDIF}
begin
  FProcessID := AProcessID;
  FThreadID := AThreadID;

  FThreadMap := TMap.Create(itu4, SizeOf(TDbgThread));
  FLibMap := TMap.Create(MAP_ID_SIZE, SizeOf(TDbgLibrary));
  FBreakMap := TMap.Create(MAP_ID_SIZE, SizeOf(TDbgBreakpoint));
  FCurrentBreakpoint := nil;

  FSymInstances := TList.Create;

  SetName(AName);

  inherited Create(Self);
end;

destructor TDbgProcess.Destroy;
begin
  FreeAndNil(FBreakMap);
  FreeAndNil(FThreadMap);
  FreeAndNil(FLibMap);
  FreeAndNil(FSymInstances);
  inherited;
end;

function TDbgProcess.FindSymbol(const AName: String): TFpDbgSymbol;
begin
  Result := FDbgInfo.FindSymbol(AName);
end;

function TDbgProcess.FindSymbol(AAdress: TDbgPtr): TFpDbgSymbol;
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

function TDbgProcess.ReadData(const AAdress: TDbgPtr; const ASize: Cardinal; out AData): Boolean;
begin
  result := false
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

function TDbgProcess.Continue(AProcess: TDbgProcess; AThread: TDbgThread; AState: TFPDState): boolean;
begin
  result := false;
end;

function TDbgProcess.RemoveBreak(const ALocation: TDbgPtr): Boolean;
begin
  if FBreakMap = nil
  then Result := False
  else Result := FBreakMap.Delete(ALocation);
end;

procedure TDbgProcess.RemoveThread(const AID: DWord);
begin
  if FThreadMap = nil then Exit;
  FThreadMap.Delete(AID);
end;

procedure TDbgProcess.Log(AString: string);
begin
  if assigned(FOnLog) then
    FOnLog(AString);
end;

procedure TDbgProcess.Log(AString: string; Options: array of const);
begin
  Log(Format(AString, Options));
end;

function TDbgProcess.GetHandle: THandle;
begin
  result := 0;
end;

procedure TDbgProcess.SetExitCode(AValue: DWord);
begin
  FExitCode:=AValue;
end;

class function TDbgProcess.StartInstance(AFileName: string; AParams: string): TDbgProcess;
begin
  DebugLn('Debug support for this platform is not available.');
  result := nil;
end;

procedure TDbgProcess.ThreadDestroyed(const AThread: TDbgThread);
begin
  if AThread = FMainThread
  then FMainThread := nil;
end;

function TDbgProcess.GetLastEventProcessIdentifier: THandle;
begin
  result := 0;
end;

function TDbgProcess.DoBreak(BreakpointAddress: TDBGPtr; AThreadID: integer): Boolean;
begin
  Result := False;
  if not FBreakMap.GetData(BreakpointAddress, FCurrentBreakpoint) then Exit;
  if FCurrentBreakpoint = nil then Exit;

  Result := True;
  if not FCurrentBreakpoint.Hit(AThreadId)
  then FCurrentBreakpoint := nil; // no need for a singlestep if we continue
end;

function TDbgProcess.WriteData(const AAdress: TDbgPtr; const ASize: Cardinal; const AData): Boolean;
begin
  result := false;
end;

{ TDbgThread }

constructor TDbgThread.Create(const AProcess: TDbgProcess; const AID: Integer; const AHandle: THandle);
begin
  FID := AID;
  FHandle := AHandle;
  FProcess := AProcess;

  inherited Create;
end;

destructor TDbgThread.Destroy;
begin
  FProcess.ThreadDestroyed(Self);
  inherited;
end;

function TDbgThread.SingleStep: Boolean;
begin
  FSingleStepping := True;
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
begin
  Result := False;
  if FOrgValue = $CC then Exit; // breakpoint on a hardcoded breakpoint
                                // no need to jum back and restore instruction
  ResetBreak;

  if not Process.GetThread(AThreadId, Thread) then Exit;

  Result := Thread.ResetInstructionPointerAfterBreakpoint;
end;

procedure TDbgBreakpoint.ResetBreak;
begin
  if FOrgValue = $CC then Exit; // breakpoint on a hardcoded breakpoint

  if not FProcess.WriteData(FLocation, 1, FOrgValue)
  then begin
    Log('Unable to reset breakpoint at $%p', [FLocation]);
    Exit;
  end;
end;

procedure TDbgBreakpoint.SetBreak;
const
  Int3: Byte = $CC;
begin
  if not FProcess.ReadData(FLocation, 1, FOrgValue)
  then begin
    Log('Unable to read breakpoint at '+FormatAddress(FLocation));
    Exit;
  end;

  if FOrgValue = $CC then Exit; // breakpoint on a hardcoded breakpoint

  if not FProcess.WriteData(FLocation, 1, Int3)
  then begin
    Log('Unable to set breakpoint at '+FormatAddress(FLocation));
    Exit;
  end;
end;

initialization
  GOSDbgClasses := nil;
finalization
  GOSDbgClasses.Free;
end.
