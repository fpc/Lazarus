{ $Id$ }
{                        ----------------------------------------------
                         GDBDebugger.pp  -  Debugger class forGDB
                         ----------------------------------------------

 @created(Wed Feb 23rd WET 2002)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

 This unit contains debugger class for the GDB/MI debugger.


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
unit GDBMIDebugger;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Dialogs, LazConf, DBGUtils, Debugger,
  CmdLineDebugger, GDBTypeInfo, 
{$IFdef MSWindows}
  Windows,
{$ENDIF}
{$IFDEF UNIX}
   Unix,BaseUnix,
{$ENDIF}
  BaseDebugManager;

type
  TGDBMIProgramInfo = record
    State: TDBGState;
    BreakPoint: Integer; // ID of Breakpoint hit
    Signal: Integer;     // Signal no if we hit one
    SignalText: String;  // Signal text if we hit one
  end;

  TGDBMICmdFlags = set of (
    cfNoMiCommand, // the command is not a MI command
    cfIgnoreState, // ignore the result state of the command
    cfIgnoreError, // ignore errors
    cfExternal     // the command is a result from a user action
  );

  TGDBMIResultFlags = set of (
    rfNoMI         // flag is set if the output is not MI fomatted
                   // some MI functions return normal output
                   // some normal functions return MI output
  );
  
  TGDBMIExecResult = record
    State: TDBGState;
    Values: String;
    Flags: TGDBMIResultFlags
  end;

  TGDBMICallback = procedure(const AResult: TGDBMIExecResult; const ATag: Integer) of object;
  TGDBMIPauseWaitState = (pwsNone, pwsInternal, pwsExternal);

  TGDBMITargetFlags = set of (
    tfHasSymbols,      // Debug symbols are present
    tfRTLUsesRegCall   // the RTL is compiled with RegCall calling convention
  );

  TGDBMIDebuggerFlags = set of (
    dfImplicidTypes    // Debugger supports implicit types (^Type)
  );

  TGDBMIRTLCallingConvention = (ccDefault, ccRegCall, ccStdCall);

  TGDBMIDebuggerProperties = class(TDebuggerProperties)
  private
    FOverrideRTLCallingConvention: TGDBMIRTLCallingConvention;
  public
    constructor Create;
  published
    property OverrideRTLCallingConvention: TGDBMIRTLCallingConvention read FOverrideRTLCallingConvention write FOverrideRTLCallingConvention;
  end;

  { TGDBMIDebugger }

  TGDBMIDebugger = class(TCmdLineDebugger)
  private
    FCommandQueue: TStringList;

    FMainAddr: TDbgPtr;
    FBreakAtMain: TDBGBreakPoint;
    FBreakErrorBreakID: Integer;
    FRunErrorBreakID: Integer;
    FExceptionBreakID: Integer;
    FPauseWaitState: TGDBMIPauseWaitState;
    FInExecuteCount: Integer;
    FDebuggerFlags: TGDBMIDebuggerFlags;
    
    // GDB info (move to ?)
    FGDBVersion: String;
    FGDBCPU: String;
    FGDBOS: String;

    // Target info (move to record ?)
    FTargetPID: Integer;
    FTargetFlags: TGDBMITargetFlags;
    FTargetCPU: String;
    FTargetOS: String;
    FTargetRegisters: array[0..2] of String;
    FTargetPtrSize: Byte; // size in bytes
    FTargetIsBE: Boolean;

    // Implementation of external functions
    function  GDBEnvironment(const AVariable: String; const ASet: Boolean): Boolean;
    function  GDBEvaluate(const AExpression: String; var AResult: String): Boolean;
    function  GDBRun: Boolean;
    function  GDBPause(const AInternal: Boolean): Boolean;
    function  GDBStop: Boolean;
    function  GDBStepOver: Boolean;
    function  GDBStepInto: Boolean;
    function  GDBRunTo(const ASource: String; const ALine: Integer): Boolean;
    function  GDBJumpTo(const ASource: String; const ALine: Integer): Boolean;
    // ---
    procedure GDBStopCallback(const AResult: TGDBMIExecResult; const ATag: Integer);
    function  FindBreakpoint(const ABreakpoint: Integer): TDBGBreakPoint;
    function  GetClassName(const AClass: TDBGPtr): String; overload;
    function  GetClassName(const AExpression: String; const AValues: array of const): String; overload;
    function  GetFrame(const AIndex: Integer): String;
    function  GetInstanceClassName(const AInstance: TDBGPtr): String; overload;
    function  GetInstanceClassName(const AExpression: String; const AValues: array of const): String; overload;
    function  GetText(const ALocation: TDBGPtr): String; overload;
    function  GetText(const AExpression: String; const AValues: array of const): String; overload;
    function  GetData(const ALocation: TDbgPtr): TDbgPtr; overload;
    function  GetData(const AExpression: String; const AValues: array of const): TDbgPtr; overload;
    function  GetStrValue(const AExpression: String; const AValues: array of const): String;
    function  GetIntValue(const AExpression: String; const AValues: array of const): Integer;
    function  GetPtrValue(const AExpression: String; const AValues: array of const): TDbgPtr;
    function  GetGDBTypeInfo(const AExpression: String): TGDBType;
    function  ProcessResult(var AResult: TGDBMIExecResult): Boolean;
    function  ProcessRunning(var AStoppedParams: String): Boolean;
    function  ProcessStopped(const AParams: String; const AIgnoreSigIntState: Boolean): Boolean;
    procedure ProcessFrame(const AFrame: String = '');

    // All ExecuteCommand functions are wrappers for the real (full) implementation
    // ExecuteCommandFull is never called directly
    function  ExecuteCommand(const ACommand: String; const AFlags: TGDBMICmdFlags): Boolean; overload;
    function  ExecuteCommand(const ACommand: String; const AFlags: TGDBMICmdFlags; const ACallback: TGDBMICallback; const ATag: Integer): Boolean; overload;
    function  ExecuteCommand(const ACommand: String; const AFlags: TGDBMICmdFlags; var AResult: TGDBMIExecResult): Boolean; overload;
    function  ExecuteCommand(const ACommand: String; const AValues: array of const; const AFlags: TGDBMICmdFlags): Boolean; overload;
    function  ExecuteCommand(const ACommand: String; const AValues: array of const; const AFlags: TGDBMICmdFlags; const ACallback: TGDBMICallback; const ATag: Integer): Boolean; overload;
    function  ExecuteCommand(const ACommand: String; const AValues: array of const; const AFlags: TGDBMICmdFlags; var AResult: TGDBMIExecResult): Boolean; overload;
    function  ExecuteCommandFull(const ACommand: String; const AValues: array of const; const AFlags: TGDBMICmdFlags; const ACallback: TGDBMICallback; const ATag: Integer; var AResult: TGDBMIExecResult): Boolean; overload;
    function  StartDebugging(const AContinueCommand: String): Boolean;
  protected
    function  ChangeFileName: Boolean; override;
    function  CreateBreakPoints: TDBGBreakPoints; override;
    function  CreateLocals: TDBGLocals; override;
    function  CreateCallStack: TDBGCallStack; override;
    function  CreateWatches: TDBGWatches; override;
    function  GetSupportedCommands: TDBGCommands; override;
    function  GetTargetWidth: Byte; override;
    procedure InterruptTarget; virtual;
    {$IFdef MSWindows}
    procedure InterruptTargetCallback(const AResult: TGDBMIExecResult; const ATag: Integer); virtual;
    {$ENDIF}
    function  ParseInitialization: Boolean; virtual;
    function  RequestCommand(const ACommand: TDBGCommand; const AParams: array of const): Boolean; override;
    procedure ClearCommandQueue;
    property  TargetPID: Integer read FTargetPID;
  public
    class function CreateProperties: TDebuggerProperties; override; // Creates debuggerproperties
    class function Caption: String; override;
    class function ExePaths: String; override;
    
    constructor Create(const AExternalDebugger: String); override;
    destructor Destroy; override;

    procedure Init; override;         // Initializes external debugger
    procedure Done; override;         // Kills external debugger
    
    // internal testing
    procedure TestCmd(const ACommand: String); override;
  end;


implementation

type
  TGDBMIBreakPoint = class(TDBGBreakPoint)
  private
    FBreakID: Integer;
    procedure SetBreakPointCallback(const AResult: TGDBMIExecResult; const ATag: Integer);
    procedure SetBreakPoint;
    procedure ReleaseBreakPoint;
    procedure UpdateEnable;
    procedure UpdateExpression;
  protected
    procedure DoEnableChange; override;
    procedure DoExpressionChange; override;
    procedure DoStateChange(const AOldState: TDBGState); override;
    procedure SetLocation(const ASource: String; const ALine: Integer); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Hit(var ACanContinue: Boolean);
  end;

  TGDBMILocals = class(TDBGLocals)
  private
    FLocals: TStringList;
    FLocalsValid: Boolean;
    procedure LocalsNeeded;
    procedure AddLocals(const AParams:String);
  protected
    procedure DoStateChange(const AOldState: TDBGState); override;
    function GetCount: Integer; override;
    function GetName(const AnIndex: Integer): String; override;
    function GetValue(const AnIndex: Integer): String; override;
  public
    constructor Create(const ADebugger: TDebugger);
    destructor Destroy; override;
  end;

  TGDBMIWatch = class(TDBGWatch)
  private
    FEvaluated: Boolean;
    FValue: String;
    procedure EvaluationNeeded;
  protected
    procedure DoEnableChange; override;
    procedure DoExpressionChange; override;
    procedure DoStateChange(const AOldState: TDBGState); override;
    function  GetValue: String; override;
    function  GetValid: TValidState; override;
  public
    constructor Create(ACollection: TCollection); override;
  end;
  
  TGDBMICallStack = class(TDBGCallStack)
  private
  protected
    function CheckCount: Boolean; override;
    function CreateStackEntry(const AIndex: Integer): TCallStackEntry; override;
  public
  end;

  TGDBMIExpression = class(TObject)
  private
    FDebugger: TGDBMIDebugger; 
    FOperator: String;
    FLeft: TGDBMIExpression;
    FRight: TGDBMIExpression;
    procedure CreateSubExpression(const AExpression: String);
  protected
  public
    constructor Create(const ADebugger: TGDBMIDebugger; const AExpression: String);
    destructor Destroy; override;
    function DumpExpression: String;
    function GetExpression(var AResult: String): Boolean;
  end;
  
  { TGDBMIType }

  TGDBMIType = class(TGDBType)
  private
  protected
  public
    constructor CreateFromResult(const AResult: TGDBMIExecResult);
  end;


  PGDBMICmdInfo = ^TGDBMICmdInfo;
  TGDBMICmdInfo = record
    Flags: TGDBMICmdFlags;
    CallBack: TGDBMICallback;
    Tag: Integer;
  end;

{ =========================================================================== }
{ Some win32 stuff }
{ =========================================================================== }
{$IFdef MSWindows}
var
  DebugBreakAddr: Pointer = nil;
  // use our own version. Win9x doesn't support this, so it is a nice check
  _CreateRemoteThread: function(hProcess: THandle; lpThreadAttributes: Pointer; dwStackSize: DWORD; lpStartAddress: TFNThreadStartRoutine; lpParameter: Pointer; dwCreationFlags: DWORD; var lpThreadId: DWORD): THandle; stdcall = nil;

procedure InitWin32;
var
  hMod: THandle;
begin           
  // Check if we already are initialized
  if DebugBreakAddr <> nil then Exit;
   
  // normally you would load a lib, but since kernel32 is 
  // always loaded we can use this (and we don't have to free it
  hMod := GetModuleHandle(kernel32);
  if hMod = 0 then Exit; //????

  DebugBreakAddr := GetProcAddress(hMod, 'DebugBreak');
  Pointer(_CreateRemoteThread) := GetProcAddress(hMod, 'CreateRemoteThread');
end;
{$ENDIF}  

{ =========================================================================== }
{ Helpers }
{ =========================================================================== }

function CreateMIValueList(AResultValues: String): TStringList;
var
  n: Integer;
  InString: Boolean;
  InList: Integer;
  c: Char;
begin
  Result := TStringList.Create;
  if AResultValues = '' then Exit;
  // strip surrounding '[]' and '{}' first
  case AResultValues[1] of
    '[': begin
      if AResultValues[Length(AResultValues)] = ']'
      then begin
        Delete(AResultValues, Length(AResultValues), 1);
        Delete(AResultValues, 1, 1);
      end;
    end;
    '{': begin
      if AResultValues[Length(AResultValues)] = '}'
      then begin
        Delete(AResultValues, Length(AResultValues), 1);
        Delete(AResultValues, 1, 1);
      end;
    end;
  end;

  n := 1;
  InString := False;
  InList := 0;
  c := #0;
  while (n <= Length(AResultValues)) do
  begin
    if c = '\'
    then begin
      // previous char was escape char
      c := #0;
      Inc(n);
      Continue;
    end;
    c := AResultValues[n];
    if c = '\'
    then begin
      Delete(AResultValues, n, 1);
      Continue;
    end;

    if InString
    then begin
      if  c = '"'
      then begin
        InString := False;
        Delete(AResultValues, n, 1);
        Continue;
      end;
    end
    else begin
      if InList > 0
      then begin
        if c in [']', '}']
        then Dec(InList);
      end
      else begin
        if c = ','
        then begin
          Result.Add(Copy(AResultValues, 1, n - 1));
          Delete(AResultValues, 1, n);
          n := 1;
          Continue;
        end
        else if c = '"'
        then begin
          InString := True;
          Delete(AResultValues, n, 1);
          Continue;
        end;
      end;
      if c in ['[', '{']
      then Inc(InList);
    end;
    Inc(n);
  end;
  if AResultValues <> ''
  then Result.Add(AResultValues);
end;

function CreateMIValueList(AResult: TGDBMIExecResult): TStringList;
begin
  // TODO ? add check ?
  Result := CreateMIValueList(AResult.Values);
end;

function CreateValueList(AResultValues: String): TStringList;
var
  n: Integer;
begin
  Result := TStringList.Create;
  if AResultValues = '' then Exit;
  n := Pos(' = ', AResultValues);
  if n > 0
  then begin
    Delete(AResultValues, n, 1);
    Delete(AResultValues, n + 1, 1);
  end;
  Result.Add(AResultValues);
end;

function ConvertToGDBPath(APath: string): string;
// GDB wants forward slashes in its filenames, even on win32.
begin
  Result := APath;
  // no need to process empty filename
  if Result='' then exit;
  
  {$WARNINGS off}
  if DirectorySeparator <> '/' then
    Result := StringReplace(Result, DirectorySeparator, '/', [rfReplaceAll]);
  {$WARNINGS on}
  Result := '"' + Result + '"';
end;              
  
{ =========================================================================== }
{ TGDBMIDebuggerProperties }
{ =========================================================================== }

constructor TGDBMIDebuggerProperties.Create;
begin
  FOverrideRTLCallingConvention := ccDefault;
  inherited;
end;


{ =========================================================================== }
{ TGDBMIDebugger }
{ =========================================================================== }

class function TGDBMIDebugger.Caption: String;
begin
  Result := 'GNU debugger (gdb)';
end;

function TGDBMIDebugger.ChangeFileName: Boolean;
  procedure ClearBreakpoint(var ABreakID: Integer);
  begin
    if ABreakID = -1 then Exit;
    ExecuteCommand('-break-delete %d', [ABreakID], [cfIgnoreError]);
    ABreakID := -1;
  end;
var
  S: String;
  R: TGDBMIExecResult;
  List: TStringList;
begin
  Result := False;
  
  //Cleanup our own breakpoints
  ClearBreakpoint(FExceptionBreakID);
  ClearBreakpoint(FBreakErrorBreakID);
  ClearBreakpoint(FRunErrorBreakID);


  S := ConvertToGDBPath(FileName); 
  if not ExecuteCommand('-file-exec-and-symbols %s', [S], [cfIgnoreError], R) then Exit;
  if  (R.State = dsError)
  and (FileName <> '')
  then begin
    List := CreateMIValueList(R);
    MessageDlg('Debugger', Format('Failed to load file: %s', [List.Values['msg']]), mtError, [mbOK], 0);
    List.Free;
    SetState(dsStop);
    Exit;
  end;
  if not (inherited ChangeFileName) then Exit;
  if State = dsError then Exit;
  if FileName = '' 
  then begin
    Result := True;
    Exit;
  end;
  
  if tfHasSymbols in FTargetFlags
  then begin
    // Force setting language
    // Setting extensions dumps GDB (bug #508)
    if not ExecuteCommand('-gdb-set language pascal', []) then exit;
    if State=dsError then exit;
(*
    ExecuteCommand('-gdb-set extension-language .lpr pascal', False);
    if not FHasSymbols then Exit; // file-exec-and-symbols not allways result in no symbols
    ExecuteCommand('-gdb-set extension-language .lrs pascal', False);
    ExecuteCommand('-gdb-set extension-language .dpr pascal', False);
    ExecuteCommand('-gdb-set extension-language .pas pascal', False);
    ExecuteCommand('-gdb-set extension-language .pp pascal', False);
    ExecuteCommand('-gdb-set extension-language .inc pascal', False);
*)
  end;
  Result:=true;
end;

constructor TGDBMIDebugger.Create(const AExternalDebugger: String);
begin
  FBreakErrorBreakID := -1;
  FRunErrorBreakID := -1;
  FExceptionBreakID := -1;
  FCommandQueue := TStringList.Create;
  FTargetPID := 0;
  FTargetFlags := [];
  FDebuggerFlags := [];

{$IFdef MSWindows}
  InitWin32;
{$ENDIF}  

  inherited;
end;

function TGDBMIDebugger.CreateBreakPoints: TDBGBreakPoints;
begin
  Result := TDBGBreakPoints.Create(Self, TGDBMIBreakPoint);
end;

function TGDBMIDebugger.CreateCallStack: TDBGCallStack; 
begin
  Result := TGDBMICallStack.Create(Self);
end;

function TGDBMIDebugger.CreateLocals: TDBGLocals;
begin
  Result := TGDBMILocals.Create(Self);
end;

class function TGDBMIDebugger.CreateProperties: TDebuggerProperties;
begin
  Result := TGDBMIDebuggerProperties.Create;
end;

function TGDBMIDebugger.CreateWatches: TDBGWatches;
begin
  Result := TDBGWatches.Create(Self, TGDBMIWatch);
end;

destructor TGDBMIDebugger.Destroy;
begin
  inherited;
  ClearCommandQueue;
  FreeAndNil(FCommandQueue);
end;

procedure TGDBMIDebugger.Done;
begin
  if State = dsRun then GDBPause(True);
  ExecuteCommand('-gdb-exit', []);
  inherited Done;
end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String;
  const AFlags: TGDBMICmdFlags): Boolean;
var
  R: TGDBMIExecResult;
begin
  Result := ExecuteCommandFull(ACommand, [], AFlags, nil, 0, R);
end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String;
  const AFlags: TGDBMICmdFlags; const ACallback: TGDBMICallback; const ATag: Integer): Boolean;
var
  R: TGDBMIExecResult;
begin
  Result := ExecuteCommandFull(ACommand, [], AFlags, ACallback, ATag, R);
end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String; const AFlags: TGDBMICmdFlags;
  var AResult: TGDBMIExecResult): Boolean;
begin
  Result := ExecuteCommandFull(ACommand, [], AFlags, nil, 0, AResult);
end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String;
  const AValues: array of const; const AFlags: TGDBMICmdFlags): Boolean;
var
  R: TGDBMIExecResult;
begin
  Result := ExecuteCommandFull(ACommand, AValues, AFlags, nil, 0, R);
end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String;
  const AValues: array of const; const AFlags: TGDBMICmdFlags;
  const ACallback: TGDBMICallback; const ATag: Integer): Boolean;
var
  R: TGDBMIExecResult;
begin
  Result := ExecuteCommandFull(ACommand, AValues, AFlags, ACallback, ATag, R);
end;

function TGDBMIDebugger.ExecuteCommand(const ACommand: String;
  const AValues: array of const; const AFlags: TGDBMICmdFlags;
  var AResult: TGDBMIExecResult): Boolean;
begin
  Result := ExecuteCommandFull(ACommand, AValues, AFlags, nil, 0, AResult);
end;

function TGDBMIDebugger.ExecuteCommandFull(const ACommand: String;
  const AValues: array of const; const AFlags: TGDBMICmdFlags;
  const ACallback: TGDBMICallback; const ATag: Integer;
  var AResult: TGDBMIExecResult): Boolean;
var
  Cmd: String;
  CmdInfo: PGDBMICmdInfo;
  R, FirstCmd: Boolean;
  StoppedParams: String;
  ExecResult: TGDBMIExecResult;
begin
  Result := False; // Assume queued
  AResult.Values := '';
  AResult.State := dsNone;
  AResult.Flags := [];

  New(CmdInfo);
  CmdInfo^.Flags := AFlags;
  CmdInfo^.Callback := ACallBack;
  CmdInfo^.Tag := ATag;
  FCommandQueue.AddObject(Format(ACommand, AValues), TObject(CmdInfo));

  if FCommandQueue.Count > 1
  then begin
    if cfExternal in AFlags
    then DebugLn('[WARNING] Debugger: Execution of external command "', ACommand, '" while queue exists');
    Exit;
  end;
  // If we are here we can process the command directly
  Result := True;
  FirstCmd := True;
  repeat
    Inc(FInExecuteCount);
    try
      ExecResult.Values := '';
      ExecResult.State := dsNone;
      ExecResult.Flags := [];
      
      Cmd := FCommandQueue[0];
      CmdInfo := PGDBMICmdInfo(FCommandQueue.Objects[0]);
      SendCmdLn(Cmd);
      R := ProcessResult(ExecResult);
      if not R
      then begin
        DebugLn('[WARNING] TGDBMIDebugger:  ExecuteCommand "',Cmd,'" failed.');
        SetState(dsError);
        Break;
      end;

      if (ExecResult.State <> dsNone)
      and not (cfIgnoreState in CmdInfo^.Flags)
      and ((ExecResult.State <> dsError) or not (cfIgnoreError in CmdInfo^.Flags))
      then SetState(ExecResult.State);

      StoppedParams := '';
      if ExecResult.State = dsRun
      then R := ProcessRunning(StoppedParams);

      // Delete command first to allow GDB access while processing stopped
      FCommandQueue.Delete(0);
      try

        if StoppedParams <> ''
        then ProcessStopped(StoppedParams, FPauseWaitState = pwsInternal);

        if Assigned(CmdInfo^.Callback)
        then CmdInfo^.Callback(ExecResult, CmdInfo^.Tag);
      finally
        Dispose(CmdInfo);
      end;

      if FirstCmd
      then begin
        FirstCmd := False;
        AResult := ExecResult;
      end;
    finally
      Dec(FInExecuteCount);
    end;
    
    if  FCommandQueue.Count = 0
    then begin
      if  (FInExecuteCount = 0)
      and (FPauseWaitState = pwsInternal)
      and (State = dsRun)
      then begin
        // reset state
        FPauseWaitState := pwsNone;
        // insert continue command
        New(CmdInfo);
        CmdInfo^.Flags := [];
        CmdInfo^.Callback := nil;
        FCommandQueue.AddObject('-exec-continue', TObject(CmdInfo));
      end
      else Break;
    end;
  until not R;
end;

class function TGDBMIDebugger.ExePaths: String;
begin
  Result := '/usr/bin/gdb;/usr/local/bin/gdb;/opt/fpc/gdb';
end;

function TGDBMIDebugger.FindBreakpoint(
  const ABreakpoint: Integer): TDBGBreakPoint;
var
  n: Integer;
begin
  if  ABreakpoint > 0
  then
    for n := 0 to Breakpoints.Count - 1 do
    begin
      Result := Breakpoints[n];
      if TGDBMIBreakPoint(Result).FBreakID = ABreakpoint
      then Exit;
    end;
  Result := nil;
end;

function TGDBMIDebugger.GetClassName(const AClass: TDBGPtr): String;
var
  S: String;
begin
  // format has a problem with %u, so use Str for it
  Str(AClass, S);
  Result := GetClassName(S, []);
end;

function TGDBMIDebugger.GetClassName(const AExpression: String; const AValues: array of const): String;
var
  OK: Boolean;
  S: String;
  R: TGDBMIExecResult;
  ResultList: TStrings;
begin
  Result := '';

  if dfImplicidTypes in FDebuggerFlags
  then begin
    S := Format(AExpression, AValues);
    OK :=  ExecuteCommand(
          '-data-evaluate-expression ^^shortstring(%s+%d)^^',
          [S, FTargetPtrSize * 3], [cfIgnoreError], R);
  end
  else begin
    Str(TDbgPtr(GetData(AExpression + '+12', AValues)), S);
    OK := ExecuteCommand('-data-evaluate-expression pshortstring(%s)^',
          [S], [cfIgnoreError], R);
  end;

  if OK
  then begin
    ResultList := CreateMIValueList(R);
    S := ResultList.Values['value'];
    Result := GetPart('''', '''', S);
    ResultList.Free;
  end;
end;

function TGDBMIDebugger.GetInstanceClassName(const AInstance: TDBGPtr): String;
var
  S: String;
begin
  Str(AInstance, S);
  Result := GetInstanceClassName(S, []);
end;

function TGDBMIDebugger.GetInstanceClassName(const AExpression: String; const AValues: array of const): String;
begin
  if dfImplicidTypes in FDebuggerFlags
  then begin
    Result := GetClassName('^pointer(' + AExpression + ')^', AValues);
  end
  else begin
    Result := GetClassName(GetData(AExpression, AValues));
  end;
end;

function PosSetEx(const ASubStrSet, AString: string; 
  const Offset: integer): integer;
begin
  for Result := Offset to Length(AString) do
    if Pos(AString[Result], ASubStrSet) > 0 then
      exit;
  Result := 0;
end;

function EscapeGDBCommand(const AInput: string): string;
var
  lPiece: string;
  I, lPos, len: integer;
begin
  lPos := 1;
  Result := '';
  repeat
    I := PosSetEx(#9#10#13, AInput, lPos);
    { copy unmatched characters }
    if I > 0 then
      len := I-lPos
    else
      len := Length(AInput)+1-lPos;
    Result := Result + Copy(AInput, lPos, len);
    { replace a matched character or be done }
    if I > 0 then
    begin
      case AInput[I] of
        #9:  lPiece := '\t';
        #10: lPiece := '\n';
        #13: lPiece := '\r';
      else
        lPiece := '';
      end;
      Result := Result + lPiece;
      lPos := I+1;
    end else
      exit;
  until false;
end;

function TGDBMIDebugger.GDBEnvironment(const AVariable: String; const ASet: Boolean): Boolean;
var
  S: String;
begin
  Result := True;

  if State = dsRun
  then GDBPause(True);
  if ASet then 
  begin
    S := EscapeGDBCommand(AVariable);
    ExecuteCommand('-gdb-set env %s', [S], [cfIgnoreState, cfExternal]);
  end else begin
    S := AVariable;
    ExecuteCommand('unset env %s', [GetPart([], ['='], S, False, False)], [cfNoMiCommand, cfIgnoreState, cfExternal]);
  end;
end;

function TGDBMIDebugger.GDBEvaluate(const AExpression: String;
  var AResult: String): Boolean;
  
  function MakePrintable(const AString: String): String;
  var
    n: Integer;
    InString: Boolean;
  begin
    Result := '';
    InString := False;
    for n := 1 to Length(AString) do
    begin
      case AString[n] of
        ' '..#127: begin
          if not InString
          then begin
            InString := True;
            Result := Result + '''';
          end;
          Result := Result + AString[n];
          //if AString[n] = '''' then Result := Result + '''';
        end;
      else
        if InString
        then begin
          InString := False;
          Result := Result + '''';
        end;
        Result := Result + Format('#%d', [Ord(AString[n])]);
      end;
    end;
    if InString
    then Result := Result + '''';
  end;
  
var
  R: TGDBMIExecResult;
  S: String;
  ResultList: TStringList;
  ResultInfo: TGDBType;
  addr: TDbgPtr;
  e: Integer;
//  Expression: TGDBMIExpression;
begin
// TGDBMIExpression was an attempt to make expression evaluation on Objects possible for GDB <= 5.2
// It is not completed and buggy. Since 5.3 expression evaluation is OK, so maybe in future the
// TGDBMIExpression will be completed to support older gdb versions
(*
  Expression := TGDBMIExpression.Create(Self, AExpression);
  if not Expression.GetExpression(S)
  then S := AExpression;
  WriteLN('[GDBEval] AskExpr: ', AExpression, ' EvalExp:', S ,' Dump: ',
          Expression.DumpExpression);
  Expression.Free;
*)
  S := AExpression;

  Result := ExecuteCommand('-data-evaluate-expression %s', [S], [cfIgnoreError, cfExternal], R);

  ResultList := CreateMIValueList(R);
  if R.State = dsError
  then AResult := ResultList.Values['msg']
  else AResult := ResultList.Values['value'];
  ResultList.Free;
  if R.State = dsError
  then Exit;

  // Check for strings
  ResultInfo := GetGDBTypeInfo(S);
  if (ResultInfo = nil) then Exit;
  
  try
    case ResultInfo.Kind of
      skPointer: begin
        Val(AResult, addr, e);
        if e <> 0 then Exit;

        S := Lowercase(ResultInfo.TypeName);
        if (S = 'character')
        or (S = 'ansistring')
        then begin
          if Addr = 0
          then AResult := ''''''
          else AResult := MakePrintable(GetText(Addr));
        end
        else begin
          if Addr = 0
          then AResult := 'nil';
          if S = 'pointer' then Exit;
          if Length(S) = 0 then Exit;
          if S[1] = 't'
          then begin
            S[1] := 'T';
            if Length(S) > 1 then S[2] := UpperCase(S[2])[1];
          end;
          AResult := '^' + S + ' ' + AResult;
        end;
      end;
      skClass: begin
        Val(AResult, addr, e);
        if e <> 0 then Exit;

        S := GetInstanceClassName(Addr);
        if S = '' then S := '???';
        AResult := S + ' ' + AResult;
      end;
    end;
  finally
    ResultInfo.Free;
  end;
end;

function TGDBMIDebugger.GDBJumpTo(const ASource: String;
  const ALine: Integer): Boolean;
begin
  Result := False;
end;

function TGDBMIDebugger.GDBPause(const AInternal: Boolean): Boolean;
begin
  // Check if we already issued a break
  if FPauseWaitState = pwsNone
  then InterruptTarget;

  if AInternal
  then begin
    if FPauseWaitState = pwsNone
    then FPauseWaitState := pwsInternal;
  end
  else FPauseWaitState := pwsExternal;

  Result := True;
end;

function TGDBMIDebugger.GDBRun: Boolean;
begin
  Result := False;
  case State of
    dsStop: begin
      Result := StartDebugging('-exec-continue');
    end;
    dsPause: begin
      Result := ExecuteCommand('-exec-continue', [cfExternal]);
    end;
    dsIdle: begin
      DebugLn('[WARNING] Debugger: Unable to run in idle state');
    end;
  end;
end;

function TGDBMIDebugger.GDBRunTo(const ASource: String;
  const ALine: Integer): Boolean;
begin
  Result := False;
  case State of
    dsStop: begin
      Result := StartDebugging(Format('-exec-until %s:%d', [ASource, ALine]));
    end;
    dsPause: begin
      Result := ExecuteCommand('-exec-until %s:%d', [ASource, ALine], [cfExternal]);
    end;
    dsIdle: begin
      DebugLn('[WARNING] Debugger: Unable to runto in idle state');
    end;
  end;

end;

function TGDBMIDebugger.GDBStepInto: Boolean;
begin
  Result := False;
  case State of
    dsStop: begin
      Result := StartDebugging('');
    end;
    dsPause: begin
      Result := ExecuteCommand('-exec-step', [cfExternal]);
    end;
    dsIdle: begin
      DebugLn('[WARNING] Debugger: Unable to step in idle state');
    end;
  end;
end;

function TGDBMIDebugger.GDBStepOver: Boolean;
begin
  Result := False;
  case State of
    dsStop: begin
      Result := StartDebugging('');
    end;
    dsPause: begin
      Result := ExecuteCommand('-exec-next', [cfExternal]);
    end;
    dsIdle: begin
      DebugLn('[WARNING] Debugger: Unable to step over in idle state');
    end;
  end;
end;

function TGDBMIDebugger.GDBStop: Boolean;
begin
  if State = dsError
  then begin
    // We don't know the state of the debugger, 
    // force a reinit. Let's hope this works.
    DebugProcess.Terminate(0);
    Done;
    Result := True;
    Exit;
  end;

  if State = dsRun
  then GDBPause(True);

  // not supported yet
  // ExecuteCommand('-exec-abort');
  Result := ExecuteCommand('kill', [cfNoMiCommand], @GDBStopCallback, 0);
end;

procedure TGDBMIDebugger.GDBStopCallback(const AResult: TGDBMIExecResult; const ATag: Integer);
var
  R: TGDBMIExecResult;
begin
  // verify stop
  if not ExecuteCommand('info program', [], [cfNoMICommand], R) then Exit;

  if Pos('not being run', R.Values) > 0
  then SetState(dsStop);
end;

function TGDBMIDebugger.GetGDBTypeInfo(const AExpression: String): TGDBType;
var
  R: TGDBMIExecResult;
begin
  if not ExecuteCommand('ptype %s', [AExpression], [cfIgnoreError, cfNoMiCommand], R)
  or (R.State = dsError)
  then begin
    Result := nil;
  end
  else begin
    Result := TGdbMIType.CreateFromResult(R);
  end;
end;

function TGDBMIDebugger.GetData(const ALocation: TDbgPtr): TDbgPtr;
var
  S: String;
begin
  Str(ALocation, S);
  Result := GetData(S, []);
end;

function TGDBMIDebugger.GetData(const AExpression: String;
  const AValues: array of const): TDbgPtr;
var
  R: TGDBMIExecResult;
  e: Integer;
begin
  Result := 0;
  if ExecuteCommand('x/d ' + AExpression, AValues, [cfNoMICommand], R)
  then Val(StripLN(GetPart('\t', '', R.Values)), Result, e);
  if e=0 then ;
end;

function TGDBMIDebugger.GetFrame(const AIndex: Integer): String;
var
  R: TGDBMIExecResult;
  S: String;
  List: TStringList;
begin
  Result := '';
  if ExecuteCommand('-stack-list-frames %d %d', [AIndex, AIndex], [cfIgnoreError], R)
  then begin
    List := CreateMIValueList(R);
    S := List.Values['stack'];
    List.Free;
    List := CreateMIValueList(S);
    Result := List.Values['frame'];
    List.Free;
  end;
end;

function TGDBMIDebugger.GetIntValue(const AExpression: String; const AValues: array of const): Integer;
var
  e: Integer;
begin
  Result := 0;
  Val(GetStrValue(AExpression, AValues), Result, e);
  if e=0 then ;
end;

function TGDBMIDebugger.GetPtrValue(const AExpression: String; const AValues: array of const): TDbgPtr;
var
  e: Integer;
begin
  Result := 0;
  Val(GetStrValue(AExpression, AValues), Result, e);
  if e=0 then ;
end;

function TGDBMIDebugger.GetStrValue(const AExpression: String; const AValues: array of const): String;
var
  R: TGDBMIExecResult;
  ResultList: TStringList;
begin
  if ExecuteCommand('-data-evaluate-expression %s', [Format(AExpression, AValues)], [cfIgnoreError], R)
  then begin
    ResultList := CreateMIValueList(R);
    Result := ResultList.Values['value'];
    ResultList.Free;
  end
  else Result := '';
end;

function TGDBMIDebugger.GetText(const ALocation: TDBGPtr): String;
var
  S: String;
begin
  Str(ALocation, S);
  Result := GetText(S, []);
end;

function TGDBMIDebugger.GetText(const AExpression: String;
  const AValues: array of const): String;
var
  S, Trailor: String;
  R: TGDBMIExecResult;
  n, len, idx: Integer;
  v: Integer;
begin
  if not ExecuteCommand('x/s ' + AExpression, AValues, [cfNoMICommand, cfIgnoreError], R)
  then begin
    Result := '';
    Exit;
  end;

  S := StripLN(R.Values);
  // don't use ' as end terminator, there might be one as part of the text
  // since ' will be the last char, simply strip it.
  S := GetPart(['\t '], [], S);

  // Scan the string
  len := Length(S);
  // Set the resultstring initially to the same size
  SetLength(Result, len);
  n := 0;
  idx := 1;
  Trailor:='';
  while idx <= len do
  begin
    case S[idx] of
      '''': begin
        Inc(idx);
        // scan till end
        while idx <= len do
        begin
          case S[idx] of
            '''' : begin
              Inc(idx);
              if idx > len then Break;
              if S[idx] <> '''' then Break;
            end;
            '\' : begin
              Inc(idx);
              if idx > len then Break;
              case S[idx] of
                't': S[idx] := #9;
                'n': S[idx] := #10;
                'r': S[idx] := #13;
              end;
            end;
          end;
          Inc(n);
          Result[n] := S[idx];
          Inc(idx);
        end;
      end;
      '#': begin
        Inc(idx);
        v := 0;
        // scan till non number (correct input is assumed)
        while (idx <= len) and (S[idx] >= '0') and (S[idx] <= '9') do
        begin
          v := v * 10 + Ord(S[idx]) - Ord('0');
          Inc(idx)
        end;
        Inc(n);
        Result[n] := Chr(v and $FF);
      end;
      ',', ' ': begin
        Inc(idx); //ignore them;
      end;
      '<': begin
        // Debugger has returned something like <repeats 10 times>
        v := StrToIntDef(GetPart(['<repeats '], [' times>'], S), 0);
        // Since we deleted the first part of S, reset idx
        idx := 8; // the char after ' times>'
        len := Length(S);
        if v <= 1 then Continue;
        
        // limit the amount of repeats
        if v > 1000
        then begin
          Trailor := Trailor + Format('###(repeat truncated: %u -> 1000)###', [v]);
          v := 1000;
        end;
        
        // make sure result has some room
        SetLength(Result, Length(Result) + v - 1);
        while v > 1 do begin
          Inc(n);
          Result[n] := Result[n - 1];
          Dec(v);
        end;
      end;
    else
      // Debugger has returned something we don't know of
      // Append the remainder to our parsed result
      Delete(S, 1, idx - 1);
      Trailor := Trailor + '###(gdb unparsed remainder:' + S + ')###';
      Break;
    end;
  end;
  SetLength(Result, n);
  Result := Result + Trailor;
end;

function TGDBMIDebugger.GetSupportedCommands: TDBGCommands;
begin
  Result := [dcRun, dcPause, dcStop, dcStepOver, dcStepInto, dcRunTo, dcJumpto,
             dcBreak, dcWatch, dcLocal, dcEvaluate, dcModify, dcEnvironment]
end;

function TGDBMIDebugger.GetTargetWidth: Byte;
begin
  Result := FTargetPtrSize*8;
end;

procedure TGDBMIDebugger.Init;
  procedure ParseGDBVersion;
  var
    R: TGDBMIExecResult;
    S: String;
  begin
    FGDBVersion := '';
    FGDBOS := '';
    FGDBCPU := '';
    
    if not ExecuteCommand('-gdb-version', [], [cfNoMiCommand], R) // No MI since the output is no MI
    then Exit;
    
    S := GetPart(['configured as \"'], ['\"'], R.Values, False, False);
    if Pos('--target=', S) <> 0 then
      S := GetPart('--target=', '', S);
    FGDBCPU := GetPart('', '-', S);
    GetPart('-', '-', S); // strip vendor
    FGDBOS := GetPart('-', '-', S);

    FGDBVersion := GetPart(['('], [')'], R.Values, False, False);
    if FGDBVersion <> '' then Exit;
    
    FGDBVersion := GetPart(['gdb '], [#10, #13], R.Values, True, False);
    if FGDBVersion <> '' then Exit;
  end;
  
  procedure CheckGDBVersion;
  begin
    if FGDBVersion < '5.3'
    then begin
      DebugLn('[WARNING] Debugger: Running an old (< 5.3) GDB version: ', FGDBVersion);
      DebugLn('                    Not all functionality will be supported.');
    end
    else begin
      DebugLn('[Debugger] Running GDB version: ', FGDBVersion);
      Include(FDebuggerFlags, dfImplicidTypes);
    end;
  end;

begin
  FPauseWaitState := pwsNone;
  FInExecuteCount := 0;
  
  if CreateDebugProcess('-silent -i mi -nx')
  then begin
    if not ParseInitialization
    then begin
      SetState(dsError);
      Exit;                             
    end;
    
    ExecuteCommand('-gdb-set confirm off', []);
    // for win32, turn off a new console otherwise breaking gdb will fail
    // ignore the error on other platforms
    ExecuteCommand('-gdb-set new-console off', [cfIgnoreError]);
    
    ParseGDBVersion;
    CheckGDBVersion;

    inherited Init;
  end
  else begin
    if DebugProcess = nil
    then MessageDlg('Debugger', 'Failed to create debug process for unknown reason', mtError, [mbOK], 0)
    else MessageDlg('Debugger', Format('Failed to create debug process: %s', [ReadLine]), mtError, [mbOK], 0);
    SetState(dsError);
  end;
end;

procedure TGDBMIDebugger.InterruptTarget;
{$IFdef MSWindows}
  function TryNT: Boolean;
  var
    hProcess: THandle;
    hThread: THandle;
    ThreadID: Cardinal;
    E: Integer;
    Emsg: PChar;
  begin                  
    Result := False;
    
    hProcess := OpenProcess(PROCESS_CREATE_THREAD or PROCESS_QUERY_INFORMATION or PROCESS_VM_OPERATION or PROCESS_VM_WRITE or PROCESS_VM_READ, False, TargetPID);
    if hProcess = 0 then Exit;
  
    try
      hThread := _CreateRemoteThread(hProcess, nil, 0, DebugBreakAddr, nil, 0, ThreadID);
      if hThread = 0
      then begin
        E := GetLastError;
        FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ALLOCATE_BUFFER, nil, E, 0, PChar(@Emsg), 0, nil);
        DebugLN('Error creating remote thread: ' + String(EMsg));
        // Yuck !
        // mixing handles and pointers, but it is how MS documented it
        LocalFree(HLOCAL(Emsg));
        Exit;
      end;
      Result := True;
      CloseHandle(hThread);

      // queue an info to find out if we are stopped in our interrupt thread
      ExecuteCommand('info program', [cfNoMICommand], @InterruptTargetCallback, ThreadID);
    finally
      CloseHandle(hProcess);
    end;
  end;
{$ENDIF}
begin
  if TargetPID = 0 then Exit;
{$IFDEF UNIX}
  FpKill(TargetPID, SIGINT);
{$ENDIF}

{$IFdef MSWindows}
  // GenerateConsoleCtrlEvent is nice, but only works if both gdb and
  // our target have a console. On win95 and family this is our only
  // option, on NT4+ we have a choice. Since this is not likely that
  // we have a console, we do it the hard way. On XP there exists 
  // DebugBreakProcess, but it does efectively the same.
  
  if (DebugBreakAddr = nil)
  or not Assigned(_CreateRemoteThread)
  or not TryNT
  then begin
    // We have no other choice than trying this
    GenerateConsoleCtrlEvent(CTRL_BREAK_EVENT, TargetPID);
    Exit;
  end;   
{$ENDIF}
end;

{$IFdef MSWindows}
procedure TGDBMIDebugger.InterruptTargetCallback(const AResult: TGDBMIExecResult; const ATag: Integer);
var
  R: TGDBMIExecResult;
  S: String;
  List: TStringList;
  n: Integer;
  ID1, ID2: Integer;
begin
  // check if we need to get out of the interrupt thread
  S := AResult.Values;
  S := GetPart(['.0x'], ['.'], S, True, False);
  if StrToIntDef('$'+S, 0) <> ATag then Exit;

  // we're stopped in our thread
  if FPauseWaitState = pwsInternal then Exit; // internal, dont care
  
  S := '';
  if not ExecuteCommand('-thread-list-ids', [cfIgnoreError], R) then Exit;
  List := CreateMIValueList(R);
  try
    n := StrToIntDef(List.Values['number-of-threads'], 0);
    if n < 2 then Exit; //nothing to switch
    S := List.Values['thread-ids'];
  finally
    List.Free;
  end;
  List := CreateMIValueList(S);
  ID1 := StrToIntDef(List.Values['thread-id'], 0);
  List.Delete(0);
  ID2 := StrToIntDef(List.Values['thread-id'], 0);
  List.Free;
  if ID1 = ID2 then Exit;
  
  if not ExecuteCommand('-thread-select %d', [ID2], [cfIgnoreError]) then Exit;
end;
{$ENDIF}

function TGDBMIDebugger.ParseInitialization: Boolean;
var
  Line, S: String;
begin
  Result := True;

  // Get initial debugger lines
  S := '';
  Line := StripLN(ReadLine);
  while DebugProcessRunning and (Line <> '(gdb) ') do
  begin
    S := S + Line + LineEnding;
    Line := StripLN(ReadLine);
  end;
  if S <> ''
  then MessageDlg('Debugger', 'Initialization output: ' + LineEnding + S,
    mtInformation, [mbOK], 0);
end;

procedure TGDBMIDebugger.ProcessFrame(const AFrame: String);
var
  S: String;
  e: Integer;
  Frame: TStringList;
  Location: TDBGLocationRec;
begin
  // Do we have a frame ?
  if AFrame = ''
  then S := GetFrame(0)
  else S := AFrame;

  Frame := CreateMIValueList(S);

  Location.Address := 0;
  Val(Frame.Values['addr'], Location.Address, e);
  if e=0 then ;
  Location.FuncName := Frame.Values['func'];
  Location.SrcFile := Frame.Values['file'];
  Location.SrcLine := StrToIntDef(Frame.Values['line'], -1);

  Frame.Free;

  DoCurrent(Location);
end;

function TGDBMIDebugger.ProcessResult(var AResult: TGDBMIExecResult): Boolean;
  
  function DoResultRecord(Line: String): Boolean;
  var
    ResultClass: String;
  begin
    ResultClass := GetPart('^', ',', Line);

    if Line = ''
    then begin
      if AResult.Values <> ''
      then Include(AResult.Flags, rfNoMI);
    end
    else begin
      AResult.Values := Line;
    end;

    Result := True;
    case StringCase(ResultClass, ['done', 'running', 'exit', 'error']) of
      0: begin // done
      end;
      1: begin // running
        AResult.State := dsRun;
      end;
      2: begin // exit
        AResult.State := dsIdle;
      end;
      3: begin // error
        DebugLn('TGDBMIDebugger.ProcessResult Error: ', Line);
        // todo implement with values
        if  (pos('msg=', Line) > 0)
        and (pos('not being run', Line) > 0)
        then AResult.State := dsStop
        else AResult.State := dsError;
      end;
    else
      Result := False;
      DebugLn('[WARNING] Debugger: Unknown result class: ', ResultClass);
    end;
  end;
  
  procedure DoConsoleStream(Line: String);
  var
    len: Integer;
  begin
    // check for symbol info
    if Pos('no debugging symbols', Line) > 0
    then begin
      Exclude(FTargetFlags, tfHasSymbols);
      DebugLn('[WARNING] Debugger: File ''%s'' has no debug symbols', [FileName]);
    end
    else begin
      // Strip surrounding ~" "
      len := Length(Line) - 3;
      if len < 0 then Exit;
      Line := Copy(Line, 3, len);
      // strip trailing \n (unless it is escaped \\n)
      if (len >= 2) and (Line[len - 1] = '\') and (Line[len] = 'n')
      then begin
        if len = 2
        then Line := LineEnding
        else if Line[len - 2] <> '\'
        then begin
          SetLength(Line, len - 2);
          Line := Line + LineEnding;
        end;
      end;
      
      AResult.Values := AResult.Values + Line;
    end;
  end;

  procedure DoTargetStream(const Line: String);
  begin
    DebugLn('[Debugger] Target output: ', Line);
  end;

  procedure DoLogStream(const Line: String);
  begin
    DebugLn('[Debugger] Log output: ', Line);
    if Line = '&"kill\n"'
    then AResult.State := dsStop
    else if LeftStr(Line, 8) = '&"Error '
    then AResult.State := dsError;
  end;

var
  S: String;
begin
  Result := False;
  AResult.Values := '';
  AResult.Flags := [];
  AResult.State := dsNone;
  repeat
    S := StripLN(ReadLine);
    if S = '' then Continue;
    if S = '(gdb) ' then Break;

    case S[1] of
      '^': Result := DoResultRecord(S);
      '~': DoConsoleStream(S);
      '@': DoTargetStream(S);
      '&': DoLogStream(S);
      '*', '+', '=': begin
        DebugLn('[WARNING] Debugger: Unexpected async-record: ', S);
      end;
    else
      DebugLn('[WARNING] Debugger: Unknown record: ', S);
    end;
    {$message warning condition should also check end-of-file reached for process output stream}
  until not DebugProcessRunning;
end;

function TGDBMIDebugger.ProcessRunning(var AStoppedParams: String): Boolean;
  function DoExecAsync(var Line: String): Boolean;
  var
    S: String;
  begin
    Result := False;
    S := GetPart('*', ',', Line);
    case StringCase(S, ['stopped', 'started', 'disappeared']) of
      0: begin // stopped
        AStoppedParams := Line;
      end;
      1, 2:; // Known, but undocumented classes
    else
      // Assume targetoutput, strip char and continue
      DebugLn('[DBGTGT] *');
      Line := S + Line;
      Result := True;
    end;
  end;

  procedure DoStatusAsync(const Line: String);
  begin
    DebugLn('[Debugger] Status output: ', Line);
  end;

  procedure DoNotifyAsync(var Line: String);
  var
    S: String;
  begin
    S := GetPart('=', ',', Line);
    case StringCase(S, ['shlibs-added', 'shlibs-updated']) of
      0: begin
        //TODO: track libs
      end;
      1:; //ignore
    else
      DebugLn('[Debugger] Notify output: ', Line);
    end;
  end;

  procedure DoResultRecord(const Line: String);
  begin
    DebugLn('[WARNING] Debugger: unexpected result-record: ', Line);
  end;

  procedure DoConsoleStream(const Line: String);
  begin
    DebugLn('[Debugger] Console output: ', Line);
  end;

  procedure DoTargetStream(const Line: String);
  begin
    DebugLn('[Debugger] Target output: ', Line);
  end;

  procedure DoLogStream(const Line: String);
  begin
    DebugLn('[Debugger] Log output: ', Line);
  end;

var
  S: String;
  idx: Integer;
begin
  Result := True;
  while DebugProcessRunning do
  begin
    S := StripLN(ReadLine);
    if S = '(gdb) ' then Break;

    while S <> '' do
    begin
      case S[1] of
        '^': DoResultRecord(S);
        '~': DoConsoleStream(S);
        '@': DoTargetStream(S);
        '&': DoLogStream(S);
        '*': if DoExecAsync(S) then Continue;
        '+': DoStatusAsync(S);
        '=': DoNotifyAsync(S);
      else
        // since target output isn't prefixed (yet?)
        // one of our known commands could be part of it.
        idx := Pos('*stopped', S);
        if idx  > 0
        then begin
          DebugLn('[DBGTGT] ', Copy(S, 1, idx - 1));
          Delete(S, 1, idx - 1);
          Continue;
        end
        else begin
          // normal target output
          DebugLn('[DBGTGT] ', S);
        end;
      end;
      Break;
    end;
  end;
end;

function TGDBMIDebugger.ProcessStopped(const AParams: String; const AIgnoreSigIntState: Boolean): Boolean;
  function GetLocation: TDBGLocationRec;
  var
    R: TGDBMIExecResult;
    S: String;
  begin
    Result.SrcLine := -1;
    Result.SrcFile := '';
    Result.FuncName := '';
    if tfRTLUsesRegCall in FTargetFlags
    then Result.Address := GetPtrValue(FTargetRegisters[1], [])
    else Result.Address := GetData('$fp+%d', [FTargetPtrSize * 3]);

    Str(Result.Address, S);
    if ExecuteCommand('info line * pointer(%s)', [S], [cfIgnoreError, cfNoMiCommand], R)
    then begin
      Result.SrcLine := StrToIntDef(GetPart('Line ', ' of', R.Values), -1);
      Result.SrcFile := GetPart('\"', '\"', R.Values);
    end;
  end;
  

  procedure ProcessException;
  var
    ObjAddr, ExceptionName, ExceptionMessage: String;
  begin
    if tfRTLUsesRegCall in FTargetFlags
    then  ObjAddr := FTargetRegisters[0]
    else begin
      if dfImplicidTypes in FDebuggerFlags
      then ObjAddr := Format('^pointer($fp+%d)^', [FTargetPtrSize * 2])
      else Str(GetData('$fp+%d', [FTargetPtrSize * 2]), ObjAddr);
    end;
    
    ExceptionName := GetInstanceClassName(ObjAddr, []);
    if ExceptionName = ''
    then ExceptionName := 'Unknown';

    // check if we should ignore this exception
    if Exceptions.Find(ExceptionName) <> nil
    then begin
      ExecuteCommand('-exec-continue', []);
      Exit;
    end;

    if dfImplicidTypes in FDebuggerFlags
    then begin
      ExceptionMessage := GetText('^Exception(%s)^.FMessage', [ObjAddr]);
      //ExceptionMessage := GetText('^^Exception($fp+8)^^.FMessage', []);
      ExceptionMessage := DeleteEscapeChars(ExceptionMessage, '\');
    end
    else ExceptionMessage := '### Not supported on GDB < 5.3 ###';

    DoException(ExceptionName, ExceptionMessage);
    DoCurrent(GetLocation);
  end;
  
  procedure ProcessBreak;
  var
    ErrorNo: Integer;
  begin
    if tfRTLUsesRegCall in FTargetFlags
    then ErrorNo := GetIntValue(FTargetRegisters[0], [])
    else ErrorNo := Integer(GetData('$fp+%d', [FTargetPtrSize * 2]));
    ErrorNo := ErrorNo and $FFFF;

    DoException(Format('RunError(%d)', [ErrorNo]), '');
    DoCurrent(GetLocation);
  end;
  
  procedure ProcessRunError;
  var
    ErrorNo: Integer;
  begin
    if tfRTLUsesRegCall in FTargetFlags
    then ErrorNo := GetIntValue(FTargetRegisters[0], [])
    else ErrorNo := Integer(GetData('$fp+%d', [FTargetPtrSize * 2]));
    ErrorNo := ErrorNo and $FFFF;

    DoException(Format('RunError(%d)', [ErrorNo]), '');
    ProcessFrame(GetFrame(1));
  end;

  procedure ProcessSignalReceived(const AList: TStringList);
  var
    SigInt: Boolean;
    S: String;
  begin
    // TODO: check to run (un)handled

    S := AList.Values['signal-name'];
    {$IFdef MSWindows}
    SigInt := S = 'SIGTRAP';
    {$ELSE}
    SigInt := S = 'SIGINT';
    {$ENDIF}
    if not AIgnoreSigIntState
    or not SigInt
    then SetState(dsPause);
    
    if not SigInt
    then DoException('External: ' + S, '');
    
    if not AIgnoreSigIntState
    or not SigInt
    then ProcessFrame(AList.Values['frame']);
  end;

var
  List: TStringList;
  Reason: String;
  BreakID: Integer;
  BreakPoint: TGDBMIBreakPoint;
  CanContinue: Boolean;
begin
  Result := True;
  List := CreateMIValueList(AParams);
  try
    Reason := List.Values['reason'];
    if (Reason = 'exited-normally')
    then begin
      SetState(dsStop);
      Exit;
    end;
    
    if Reason = 'exited'
    then begin
      SetExitCode(StrToIntDef(List.Values['exit-code'], 0));
      SetState(dsStop);
      Exit;
    end;
    
    if Reason = 'exited-signalled'
    then begin
      SetState(dsStop);
      DoException('External: ' + List.Values['signal-name'], '');
      // ProcessFrame(List.Values['frame']);
      Exit;
    end;
    
    if Reason = 'signal-received'
    then begin
      ProcessSignalReceived(List);
      Exit;
    end;   
    
    if Reason = 'breakpoint-hit'
    then begin
      BreakID := StrToIntDef(List.Values['bkptno'], -1);
      if BreakID = -1
      then begin
        SetState(dsError);
        // ???
        Exit;
      end;

      if BreakID = FBreakErrorBreakID
      then begin
        SetState(dsPause);
        ProcessBreak;
        Exit;
      end;

      if BreakID = FRunErrorBreakID
      then begin
        SetState(dsPause);
        ProcessRunError;
        Exit;
      end;
      
      if BreakID = FExceptionBreakID
      then begin
        SetState(dsPause);
        ProcessException;
        Exit;
      end;
      
      BreakPoint := TGDBMIBreakPoint(FindBreakpoint(BreakID));
      if BreakPoint <> nil
      then begin
        CanContinue := False;
        BreakPoint.Hit(CanContinue);
        if CanContinue
        then begin
          ExecuteCommand('-exec-continue', []);
        end
        else begin
          SetState(dsPause);
          ProcessFrame(List.Values['frame']);
        end;
      end;
      Exit;
    end;
    
    if Reason = 'function-finished'
    then begin
      SetState(dsPause);
      ProcessFrame(List.Values['frame']);
      Exit;
    end;
    
    if Reason = 'end-stepping-range'
    then begin
      SetState(dsPause);
      ProcessFrame(List.Values['frame']);
      Exit;
    end;
    
    if Reason = 'location-reached'
    then begin
      SetState(dsPause);
      ProcessFrame(List.Values['frame']);
      Exit;
    end;
    
    Result := False;
    DebugLn('[WARNING] Debugger: Unknown stopped reason: ', Reason);
  finally
    List.Free;
  end; 
end;

function TGDBMIDebugger.RequestCommand(const ACommand: TDBGCommand; const AParams: array of const): Boolean;
begin
  case ACommand of
    dcRun:      Result := GDBRun;
    dcPause:    Result := GDBPause(False);
    dcStop:     Result := GDBStop;
    dcStepOver: Result := GDBStepOver;
    dcStepInto: Result := GDBStepInto;
    dcRunTo:    Result := GDBRunTo(String(APArams[0].VAnsiString), APArams[1].VInteger);
    dcJumpto:   Result := GDBJumpTo(String(APArams[0].VAnsiString), APArams[1].VInteger);
    dcEvaluate: Result := GDBEvaluate(String(APArams[0].VAnsiString), String(APArams[1].VPointer^));
    dcEnvironment: Result := GDBEnvironment(String(APArams[0].VAnsiString), AParams[1].VBoolean);
  end;
end;

procedure TGDBMIDebugger.ClearCommandQueue;
var
  CmdInfo: PGDBMICmdInfo;
  i: Integer;
begin
  for i:=0 to FCommandQueue.Count-1 do begin
    CmdInfo:=PGDBMICmdInfo(FCommandQueue.Objects[i]);
    if CmdInfo<>nil then Dispose(CmdInfo);
  end;
  FCommandQueue.Clear;
end;

function TGDBMIDebugger.StartDebugging(const AContinueCommand: String): Boolean;
  function CheckFunction(const AFunction: String): Boolean;
  var
    R: TGDBMIExecResult;
    idx: Integer;
  begin
    ExecuteCommand('info functions %s', [AFunction], [cfIgnoreError, cfNoMICommand], R);
    idx := Pos(AFunction, R.Values);
    if idx <> 0
    then begin
      // Strip first
      Delete(R.Values, 1, idx + Length(AFunction) - 1);
      idx := Pos(AFunction, R.Values);
    end;
    Result := idx <> 0;
  end;

  procedure RetrieveRegcall;
  var
    R: TGDBMIExecResult;
  begin
    // Assume it is
    Include(FTargetFlags, tfRTLUsesRegCall);

    ExecuteCommand('-data-evaluate-expression FPC_THREADVAR_RELOCATE_PROC', [cfIgnoreError], R);
    if R.State <> dsError then Exit; // guessed right
    
    // next attempt, posibly no symbols, try functions
    if CheckFunction('FPC_CPUINIT') then Exit; // function present --> not 1.0

    // this runerror is only defined for < 1.1 ?
    if not CheckFunction('$$_RUNERROR$') then Exit;

    // We are here in 2 cases
    // 1) there are no symbols at all
    //    We dont have to know the calling convention
    // 2) target is compiled with an earlier version than 1.9.2
    //    params are passes by stack
    Exclude(FTargetFlags, tfRTLUsesRegCall);
  end;
  
  function InsertBreakPoint(const AName: String): Integer;
  var
    R: TGDBMIExecResult;
    ResultList, BkptList: TStringList;
  begin
    ExecuteCommand('-break-insert %s', [AName], [cfIgnoreError], R);
    if R.State = dsError then Exit;

    ResultList := CreateMIValueList(R);
    BkptList := CreateMIValueList(ResultList.Values['bkpt']);
    Result := StrToIntDef(BkptList.Values['number'], -1);
    ResultList.Free;
    BkptList.Free;
  end;

  procedure SetTargetInfo(const AFileType: String);
  begin
    // assume some defaults
    FTargetPtrSize := 4;
    FTargetIsBE := False;
  
    case StringCase(AFileType, [
      'efi-app-ia32', 'elf32-i386', 'pei-i386',
      'elf64-x86-64',
      'mach-o-be',
      'mach-o-le',
      'pei-arm-little',
      'pei-arm-big'
    ], True, False) of
      0..2: FTargetCPU := 'x86';
      3: FTargetCPU := 'x86_64';
      4: begin
         //mach-o-be
        FTargetIsBE := True;
        if FGDBCPU <> ''
        then FTargetCPU := FGDBCPU
        else FTargetCPU := 'powerpc'; // guess
      end;
      5: begin
        //mach-o-le
        if FGDBCPU <> ''
        then FTargetCPU := FGDBCPU
        else FTargetCPU := 'x86'; // guess
      end;
      6: begin
        FTargetCPU := 'arm';
      end;
      7: begin
        FTargetIsBE := True;
        FTargetCPU := 'arm';
      end;
    else
      // Unknown filetype, use GDB cpu
      DebugLn('[WARNING] [Debugger.TargetInfo] Unknown FileType: %s, using GDB cpu', [AFileType]);

      FTargetCPU := FGDBCPU;
    end;

    case StringCase(FTargetCPU, [
      'x86', 'i386', 'i486', 'i586', 'i686',
      'ia64', 'x86_64', 'powerpc',
      'sparc', 'arm'
    ], True, False) of
      0..4: begin // x86
        FTargetRegisters[0] := '$eax';
        FTargetRegisters[1] := '$edx';
        FTargetRegisters[2] := '$ecx';
      end;
      5, 6: begin // ia64, x86_64
        FTargetRegisters[0] := '$rdi';
        FTargetRegisters[1] := '$rsi';
        FTargetRegisters[2] := '$rdx';
        FTargetPtrSize := 8;
      end;
      7: begin // powerpc
        FTargetIsBE := True;
        // alltough darwin can start with r2, it seems that all OS start with r3
//        if UpperCase(FTargetOS) = 'DARWIN'
//        then begin
//          FTargetRegisters[0] := '$r2';
//          FTargetRegisters[1] := '$r3';
//          FTargetRegisters[2] := '$r4';
//        end
//        else begin
          FTargetRegisters[0] := '$r3';
          FTargetRegisters[1] := '$r4';
          FTargetRegisters[2] := '$r5';
//        end;
      end;
      8: begin // sparc
        FTargetIsBE := True;
        FTargetRegisters[0] := '$g1';
        FTargetRegisters[1] := '$o0';
        FTargetRegisters[2] := '$o1';
      end;
      9: begin // arm
        FTargetRegisters[0] := '$r0';
        FTargetRegisters[1] := '$r1';
        FTargetRegisters[2] := '$r2';
      end;
    else
      FTargetRegisters[0] := '';
      FTargetRegisters[1] := '';
      FTargetRegisters[2] := '';
      DebugLn('[WARNING] [Debugger] Unknown target CPU: ', FTargetCPU);
    end;

  end;
  
  function SetTempMainBreak: Boolean;
  var
    R: TGDBMIExecResult;
    S: String;
    ResultList, BkptList: TStringList;
  begin
    // Try to retrieve the address of main. Setting a break on main is past initialization
    if ExecuteCommand('info address main', [cfNoMICommand, cfIgnoreError], R)
    and (R.State <> dsError)
    then begin
      S := GetPart(['at address ', ' at '], ['.', ' '], R.Values);
      if S <> ''
      then begin
        FMainAddr := StrToIntDef(S, 0);
        ExecuteCommand('-break-insert -t *%u', [FMainAddr],  [cfIgnoreError], R);
        Result := R.State <> dsError;
        if Result then Exit;
      end;
    end;

    ExecuteCommand('-break-insert -t main',  [cfIgnoreError], R);
    Result := R.State <> dsError;
    if not Result then Exit;

    ResultList := CreateMIValueList(R);
    BkptList := CreateMIValueList(ResultList.Values['bkpt']);
    FMainAddr := StrToIntDef(BkptList.Values['addr'], 0);
    BkptList.Free;
    ResultList.Free;
  end;
  
var
  R: TGDBMIExecResult;
  S, FileType, EntryPoint: String;
  List: TStringList;
  TargetPIDPart: String;
  TempInstalled, CanContinue: Boolean;
begin
  if not (State in [dsStop])
  then begin
    Result := True;
    Exit;
  end;

  DebugLn(['TGDBMIDebugger.StartDebugging WorkingDir="',WorkingDir,'"']);
  if WorkingDir <> ''
  then begin
    // to workaround a possible bug in gdb, first set the workingdir to .
    // otherwise on second run within the same gdb session the workingdir
    // is set to c:\windows
    ExecuteCommand('-environment-cd %s', ['.'], [cfIgnoreError]);
    ExecuteCommand('-environment-cd %s', [ConvertToGDBPath(WorkingDir)], []);
  end;

  FTargetFlags := [tfHasSymbols]; // Set until proven otherwise

  // check if the exe is compiled with FPC >= 1.9.2
  // then the rtl is compiled with regcalls
  RetrieveRegCall;

  // also call execute -exec-arguments if there are no arguments in this run
  // so the possible arguments of a previous run are cleared
  ExecuteCommand('-exec-arguments %s', [Arguments], [cfIgnoreError]);
  
  if tfHasSymbols in FTargetFlags
  then begin
    // Make sure we are talking pascal
    ExecuteCommand('-gdb-set language pascal', []);
    TempInstalled := SetTempMainBreak;
  end
  else begin
    DebugLn('TGDBMIDebugger.StartDebugging Note: Target has no symbols');
    TempInstalled := False;
  end;
  
  // try Insert Break breakpoint
  // we might have rtl symbols
  if FExceptionBreakID = -1
  then FExceptionBreakID := InsertBreakPoint('FPC_RAISEEXCEPTION');
  if FBreakErrorBreakID = -1
  then FBreakErrorBreakID := InsertBreakPoint('FPC_BREAK_ERROR');
  if FRunErrorBreakID = -1
  then FRunErrorBreakID := InsertBreakPoint('FPC_RUNERROR');

  FTargetCPU := '';
  FTargetOS := FGDBOS; // try to detect ??

  // try to retrieve the filetype and program entry point
  FileType := '';
  EntryPoint := '';
  if ExecuteCommand('info file', [cfIgnoreError, cfNoMICommand], R)
  then begin
    if rfNoMI in R.Flags
    then begin
      FileType := GetPart('file type ', '.', R.Values);
      EntryPoint := GetPart(['Entry point: '], [#10, #13], R.Values);
    end
    else begin
      // OS X gdb has mi output here
      List := CreateMIValueList(R);
      S := List.Values['section-info'];
      List.Free;
      List := CreateMIValueList(S);
      FileType := List.Values['filetype'];
      EntryPoint := List.Values['entry-point'];
      List.Free;
    end;
    DebugLn('[Debugger] File type: ', FileType);
    DebugLn('[Debugger] Entry point: ', EntryPoint);
  end;

  SetTargetInfo(FileType);
  
  if not TempInstalled and (EntryPoint <> '')
  then begin
    // We could not set our initial break to get info and allow stepping
    // Try it with the program entry point
    FMainAddr := StrToIntDef(EntryPoint, 0);
    ExecuteCommand('-break-insert -t *%u', [FMainAddr], [cfIgnoreError], R);
    TempInstalled := R.State <> dsError;
  end;
  
  FTargetPID := 0;
  
  // fire the first step
  if TempInstalled
  and ExecuteCommand('-exec-run', [], R)
  then begin
    // some versions of gdb (OSX) output the PID here
    TargetPIDPart := GetPart(['process '],
                             [' local'], R.Values, True);
    FTargetPID := StrToIntDef(TargetPIDPart, 0);
    R.State := dsNone;
  end;

  // try to find PID (if not already found)
  if (FTargetPID = 0)
  and ExecuteCommand('info program', [], [cfIgnoreError, cfNoMICommand], R)
  then begin
    TargetPIDPart := GetPart(['child process ', 'child thread ', 'lwp '],
                             [' ', '.', ')'], R.Values, True);
    FTargetPID := StrToIntDef(TargetPIDPart, 0);
  end;

  if FTargetPID = 0
  then begin
    Result := False;
    SetState(dsError);
    Exit;
  end;

  DebugLn('[Debugger] Target PID: %u', [FTargetPID]);

  if R.State = dsNone
  then begin
    SetState(dsInit);
    if FBreakAtMain <> nil
    then begin
      CanContinue := False;
      TGDBMIBreakPoint(FBreakAtMain).Hit(CanContinue);
    end
    else CanContinue := True;

    if CanContinue and (AContinueCommand <> '')
    then Result := ExecuteCommand(AContinueCommand, [])
    else SetState(dsPause);
  end
  else SetState(R.State);
  
  if State = dsPause
  then ProcessFrame;

  Result := True;
end;

procedure TGDBMIDebugger.TestCmd(const ACommand: String);
begin
  ExecuteCommand(ACommand, [cfIgnoreError]);
end;

{ =========================================================================== }
{ TGDBMIBreakPoint }
{ =========================================================================== }

constructor TGDBMIBreakPoint.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FBreakID := 0;
end;

destructor TGDBMIBreakPoint.Destroy;
begin
  ReleaseBreakPoint;
  inherited Destroy;
end;

procedure TGDBMIBreakPoint.DoEnableChange;
begin
  UpdateEnable;
  inherited;
end;

procedure TGDBMIBreakPoint.DoExpressionChange;
begin
  UpdateExpression;
  inherited;
end;

procedure TGDBMIBreakPoint.DoStateChange(const AOldState: TDBGState);
begin
  inherited DoStateChange(AOldState);
  
  case Debugger.State of
    dsInit: begin
      SetBreakpoint;
    end;
    dsStop: begin
      if AOldState = dsRun
      then ReleaseBreakpoint;
    end;
  end;
end;

procedure TGDBMIBreakPoint.Hit(var ACanContinue: Boolean);
begin
  DoHit(HitCount + 1, ACanContinue);
end;

procedure TGDBMIBreakPoint.SetBreakpoint;
begin
  if Debugger = nil then Exit;

  if FBreakID <> 0
  then ReleaseBreakPoint;

  if Debugger.State = dsRun
  then TGDBMIDebugger(Debugger).GDBPause(True);
  TGDBMIDebugger(Debugger).ExecuteCommand('-break-insert %s:%d',
    [ExtractFileName(Source), Line], [cfIgnoreError], @SetBreakPointCallback, 0);
  
end;

procedure TGDBMIBreakPoint.SetBreakPointCallback(const AResult: TGDBMIExecResult; const ATag: Integer);
var
  ResultList, BkptList: TStringList;
begin
  BeginUpdate;
  try
    ResultList := CreateMIValueList(AResult);
    BkptList := CreateMIValueList(ResultList.Values['bkpt']);
    FBreakID := StrToIntDef(BkptList.Values['number'], 0);
    SetHitCount(StrToIntDef(BkptList.Values['times'], 0));
    if FBreakID <> 0
    then SetValid(vsValid)
    else SetValid(vsInvalid);
    UpdateExpression;
    UpdateEnable;
    
    if (FBreakID <> 0)
    and Enabled
    and (TGDBMIDebugger(Debugger).FBreakAtMain = nil)
    then begin
      // Check if this BP is at the same location as the temp break
      if StrToIntDef(BkptList.Values['addr'], 0) = TGDBMIDebugger(Debugger).FMainAddr
      then TGDBMIDebugger(Debugger).FBreakAtMain := Self;
    end;

    ResultList.Free;
    BkptList.Free;
  finally
    EndUpdate;
  end;
end;

procedure TGDBMIBreakPoint.ReleaseBreakPoint;
begin
  if FBreakID = 0 then Exit;
  if Debugger = nil then Exit;
  
  if Debugger.State = dsRun
  then TGDBMIDebugger(Debugger).GDBPause(True);
  TGDBMIDebugger(Debugger).ExecuteCommand('-break-delete %d', [FBreakID], []);
  FBreakID:=0;
  SetHitCount(0);
end;

procedure TGDBMIBreakPoint.SetLocation(const ASource: String; const ALine: Integer);
begin
  if (Source = ASource) and (Line = ALine) then exit;
  inherited;
  if Debugger = nil then Exit;
  if TGDBMIDebugger(Debugger).State in [dsStop, dsPause, dsRun]
  then SetBreakpoint;
end;

procedure TGDBMIBreakPoint.UpdateEnable;
const                         
  // Use shortstring as fix for fpc 1.9.5 [2004/07/15]
  CMD: array[Boolean] of ShortString = ('disable', 'enable');
begin
  if (FBreakID = 0)
  or (Debugger = nil)
  then Exit;

  if Debugger.State = dsRun
  then TGDBMIDebugger(Debugger).GDBPause(True);
  //writeln('TGDBMIBreakPoint.UpdateEnable Line=',Line,' Enabled=',Enabled,' InitialEnabled=',InitialEnabled);
  TGDBMIDebugger(Debugger).ExecuteCommand('-break-%s %d',
                                          [CMD[Enabled], FBreakID], []);
end;

procedure TGDBMIBreakPoint.UpdateExpression;
begin
end;

{ =========================================================================== }
{ TGDBMILocals }
{ =========================================================================== }

procedure TGDBMILocals.AddLocals(const AParams: String);
var
  n, e: Integer;
  addr: TDbgPtr;
  LocList, List: TStrings;
  S, Name, Value: String;
begin
  LocList := CreateMIValueList(AParams);
  for n := 0 to LocList.Count - 1 do
  begin
    List := CreateMIValueList(LocList[n]);
    Name := List.Values['name'];
    if Name = 'this'
    then Name := 'Self';

    Value := List.Values['value'];
    // try to deref. strings
    S := GetPart(['(pchar) ', '(ansistring) '], [], Value, True, False);
    if S <> ''
    then begin
      addr := 0;
      Val(S, addr, e);
      if e=0 then ;
      if addr = 0
      then Value := ''''''
      else Value := '''' + TGDBMIDebugger(Debugger).GetText(addr) + '''';
    end;

    FLocals.Add(Name + '=' + Value);
    FreeAndNil(List);
  end;
  FreeAndNil(LocList);
end;

constructor TGDBMILocals.Create(const ADebugger: TDebugger);
begin
  FLocals := TStringList.Create;
  FLocals.Sorted := True;
  FLocalsValid := False;
  inherited;
end;

destructor TGDBMILocals.Destroy;
begin
  inherited;
  FreeAndNil(FLocals);
end;

procedure TGDBMILocals.DoStateChange(const AOldState: TDBGState);
begin
  if  (Debugger <> nil)
  and (Debugger.State = dsPause)
  then begin
    DoChange;
  end
  else begin
    FLocalsValid := False;
    FLocals.Clear;
  end;
end;

function TGDBMILocals.GetCount: Integer;
begin
  if  (Debugger <> nil)
  and (Debugger.State = dsPause)
  then begin
    LocalsNeeded;
    Result := FLocals.Count;
  end
  else Result := 0;
end;

function TGDBMILocals.GetName(const AnIndex: Integer): String;
begin
  if  (Debugger <> nil)
  and (Debugger.State = dsPause)
  then begin
    LocalsNeeded;
    Result := FLocals.Names[AnIndex];
  end
  else Result := '';
end;

function TGDBMILocals.GetValue(const AnIndex: Integer): String;
begin
  if  (Debugger <> nil)
  and (Debugger.State = dsPause)
  then begin
    LocalsNeeded;
    Result := FLocals[AnIndex];
    Result := GetPart('=', '', Result);
  end
  else Result := '';
end;

procedure TGDBMILocals.LocalsNeeded;
var
  R: TGDBMIExecResult;
  S: String;
  List: TStrings;
begin
  if Debugger = nil then Exit;
  if FLocalsValid then Exit;

  // args
  TGDBMIDebugger(Debugger).ExecuteCommand('frame', [], R);
  List := CreateMIValueList(R);
  S := List.Values['frame'];
  FreeAndNil(List);
  List := CreateMIValueList(S);
  AddLocals(List.Values['args']);
  FreeAndNil(List);

  // variables
  TGDBMIDebugger(Debugger).ExecuteCommand('-stack-list-locals 1', [], R);
  List := CreateMIValueList(R);
  AddLocals(List.Values['locals']);
  FreeAndNil(List);
  FLocalsValid := True;
end;

{ =========================================================================== }
{ TGDBMIWatch }
{ =========================================================================== }

constructor TGDBMIWatch.Create(ACollection: TCollection);
begin
  FEvaluated := False;
  inherited;
end;

procedure TGDBMIWatch.DoEnableChange;
begin
  inherited;
end;

procedure TGDBMIWatch.DoExpressionChange;
begin
  FEvaluated := False;
  inherited;
end;

procedure TGDBMIWatch.DoStateChange(const AOldState: TDBGState);
begin
  if Debugger = nil then Exit;

  if Debugger.State in [dsPause, dsStop]
  then FEvaluated := False;
  if Debugger.State = dsPause then Changed;
end;

procedure TGDBMIWatch.EvaluationNeeded;
var
  ExprIsValid: Boolean;
begin
  if FEvaluated then Exit;
  if Debugger = nil then Exit;

  if (Debugger.State in [dsPause, dsStop])
  and Enabled
  then begin
    ExprIsValid:=TGDBMIDebugger(Debugger).GDBEvaluate(Expression, FValue);
    if ExprIsValid then
      SetValid(vsValid)
    else
      SetValid(vsInvalid);
  end
  else begin
    SetValid(vsInvalid);
  end;
  FEvaluated := True;
end;

function TGDBMIWatch.GetValue: String;
begin
  if  (Debugger <> nil)
  and (Debugger.State in [dsStop, dsPause])
  and Enabled
  then begin
    EvaluationNeeded;
    Result := FValue;
  end
  else Result := inherited GetValue;
end;

function TGDBMIWatch.GetValid: TValidState;
begin
  EvaluationNeeded;
  Result := inherited GetValid;
end;

{ =========================================================================== }
{ TGDBMICallStack }
{ =========================================================================== }

function TGDBMICallStack.CheckCount: Boolean;
var
  R: TGDBMIExecResult;
  List: TStrings;
  i, cnt: longint;
begin
  Result := inherited CheckCount;
  if not Result then Exit;

  TGDBMIDebugger(Debugger).ExecuteCommand('-stack-info-depth',
                                          [cfIgnoreError], R);
  List := CreateMIValueList(R);
  cnt := StrToIntDef(List.Values['depth'], -1);
  FreeAndNil(List);
  if cnt = -1 then
  begin
    { In case of error some stackframes still can be accessed.
      Trying to find out how many...
      We try maximum 40 frames, because sometimes a corrupt stack and a bug in
      gdb may cooperate, so that -stack-info-depth X returns always X }
    i:=0;
    repeat
      inc(i);
      TGDBMIDebugger(Debugger).ExecuteCommand('-stack-info-depth ' + IntToStr(i),
                                              [cfIgnoreError], R);
      List := CreateMIValueList(R);
      cnt := StrToIntDef(List.Values['depth'], -1);
      FreeAndNil(List);
      if (cnt = -1) then begin
        // no valid stack-info-depth found, so the previous was the last valid one
        cnt:=i - 1;
      end;
    until (cnt<i) or (i=40);
  end;
  SetCount(cnt);
end;

function TGDBMICallStack.CreateStackEntry(const AIndex: Integer): TCallStackEntry;
var                 
  n, e: Integer;
  R: TGDBMIExecResult;
  S: String;
  addr: TDbgPtr;
  Arguments, ArgList, List: TStrings;
begin
  if Debugger = nil then Exit;

  Arguments := TStringList.Create;
  TGDBMIDebugger(Debugger).ExecuteCommand('-stack-list-arguments 1 %d %d',
                                          [AIndex, AIndex], [cfIgnoreError], R);
  // TODO: check what to display on error

  List := CreateMIValueList(R);
  S := List.Values['stack-args'];
  FreeAndNil(List);
  List := CreateMIValueList(S);
  S := List.Values['frame']; // all arguments
  FreeAndNil(List);
  List := CreateMIValueList(S);   
  S := List.Values['args'];
  FreeAndNil(List);
  
  ArgList := CreateMIValueList(S);
  for n := 0 to ArgList.Count - 1 do
  begin
    List := CreateMIValueList(ArgList[n]);
    Arguments.Add(List.Values['name'] + '=' + List.Values['value']);
    FreeAndNil(List);
  end;
  FreeAndNil(ArgList);
  
  TGDBMIDebugger(Debugger).ExecuteCommand('-stack-list-frames %d %d',
                                          [AIndex, AIndex], [], R);
  List := CreateMIValueList(R);
  S := List.Values['stack'];
  FreeAndNil(List);
  List := CreateMIValueList(S);   
  S := List.Values['frame'];
  FreeAndNil(List);
  List := CreateMIValueList(S);
  addr := 0;
  Val(List.Values['addr'], addr, e);
  if e=0 then ;
  Result := TCallStackEntry.Create(
    AIndex, 
    addr,
    Arguments,
    List.Values['func'],
    List.Values['file'],
    StrToIntDef(List.Values['line'], 0)
  );
  
  FreeAndNil(List);
  Arguments.Free;
end;

{ =========================================================================== }
{ TGDBMIExpression }
{ =========================================================================== }

constructor TGDBMIExpression.Create(const ADebugger: TGDBMIDebugger; const AExpression: String);
begin
  inherited Create;
  FDebugger := ADebugger;
  FLeft := nil;
  FRight := nil;
  CreateSubExpression(Trim(AExpression));
end;

procedure TGDBMIExpression.CreateSubExpression(const AExpression: String);
  function CheckOperator(const APos: Integer; const AOperator: String): Boolean;
  var
    S: String;
  begin
    Result := False;
    if APos + Length(AOperator) > Length(AExpression) then Exit;
    if StrLIComp(@AExpression[APos], @AOperator[1], Length(AOperator)) <> 0 then Exit;
    if (APos > 1) and not (AExpression[APos - 1] in [' ', '(']) then Exit;
    if (APos + Length(AOperator) <= Length(AExpression)) and not (AExpression[APos + Length(AOperator)] in [' ', '(']) then Exit;

    S := Copy(AExpression, 1, APos - 1);
    if S <> ''
    then FLeft := TGDBMIExpression.Create(FDebugger, S);
    S := Copy(AExpression, APos + Length(AOperator), MaxInt);
    if S <> ''
    then FRight := TGDBMIExpression.Create(FDebugger, S);
    FOperator := AOperator;
    Result := True;
  end;
type
  TStringState = (ssNone, ssString, ssLeave);
var
  n: Integer;
  S, LastWord: String;
  HookCount: Integer;
  InString: TStringState;
  Sub: TGDBMIExpression;
begin
  HookCount := 0;
  InString := ssNone;
  LastWord := '';
  S:='';
  for n := 1 to Length(AExpression)  do
  begin
    if AExpression[n] = ''''
    then begin
      case InString of
        ssNone:  InString := ssString;
        ssString:InString := ssLeave;
        ssLeave: InString := ssString;
      end;
      S := S + AExpression[n];
      LastWord := '';
      Continue;
    end;
    if InString = ssString
    then begin
      S := S + AExpression[n];
      LastWord := '';
      Continue;
    end;
    InString := ssNone;

    case AExpression[n] of
      '(', '[': begin
        if HookCount = 0
        then begin
          SetLength(S, Length(S) - Length(LastWord));
          if S <> ''
          then FLeft := TGDBMIExpression.Create(FDebugger, S);
          if LastWord = ''
          then begin
            FOperator := AExpression[n];
          end
          else begin
            FOperator := LastWord;
            FRight := TGDBMIExpression.Create(FDebugger, '');
            FRight.FOperator := AExpression[n];
          end;
          LastWord := '';
          S := '';
        end;
        Inc(HookCount);
        if HookCount = 1
        then Continue;
      end;
      ')', ']': begin
        Dec(HookCount);
        if HookCount = 0
        then begin
          if S <> ''
          then begin
            if FRight = nil
            then FRight := TGDBMIExpression.Create(FDebugger, S)
            else FRight.FRight := TGDBMIExpression.Create(FDebugger, S);
          end;
          if n < Length(AExpression)
          then begin
            Sub := TGDBMIExpression.Create(FDebugger, '');
            Sub.FLeft := FLeft;
            Sub.FOperator := FOperator;
            Sub.FRight := FRight;
            FLeft := Sub;
            Sub := TGDBMIExpression.Create(FDebugger, Copy(AExpression, n + 1, MaxInt));
            if Sub.FLeft = nil
            then begin
              FOperator := Sub.FOperator;
              FRight := Sub.FRight;
              Sub.FRight := nil;
              Sub.Free;
            end
            else begin
              FOperator := '';
              FRight := Sub;
            end;
          end;
          Exit;
        end;
      end;
    end;
    if HookCount = 0
    then begin
      case AExpression[n] of
        '-', '+', '*', '/', '^', '@', '=', ',': begin
          if S <> ''
          then FLeft := TGDBMIExpression.Create(FDebugger, S);
          S := Copy(AExpression, n + 1, MaxInt);
          if Trim(S) <> ''
          then FRight := TGDBMIExpression.Create(FDebugger, S);
          FOperator := AExpression[n];
          Exit;
        end;
        'a', 'A': begin
          if CheckOperator(n, 'and') then Exit;
        end;
        'o', 'O': begin
          if CheckOperator(n, 'or') then Exit;
        end;
        'm', 'M': begin
          if CheckOperator(n, 'mod') then Exit;
        end;
        'd', 'D': begin
          if CheckOperator(n, 'div') then Exit;
        end;
        'x', 'X': begin
          if CheckOperator(n, 'xor') then Exit;
        end;
        's', 'S': begin
          if CheckOperator(n, 'shl') then Exit;
          if CheckOperator(n, 'shr') then Exit;
        end;
      end;
    end;

    if AExpression[n] = ' '
    then LastWord := ''
    else LastWord := LastWord + AExpression[n];
    S := S + AExpression[n];
  end;
  if S = AExpression
  then FOperator := S
  else CreateSubExpression(S);
end;

destructor TGDBMIExpression.Destroy;
begin
  FreeAndNil(FRight);
  FreeAndNil(FLeft);
  inherited;
end;

function TGDBMIExpression.DumpExpression: String;
// Mainly used for debugging purposes
begin
  if FLeft = nil
  then Result := ''
  else Result := '«L:' + FLeft.DumpExpression + '»';

  if FOperator = '('
  then Result := Result + '(«R:' + FRight.DumpExpression + '»)'
  else if FOperator = '['
  then Result := Result + '[«R:' + FRight.DumpExpression + '»]'
  else begin
    if (Length(FOperator) > 0)
    and (FOperator[1] = '''')
    then Result := Result + '«O:' + ConvertToCString(FOperator) + '»'
    else Result := Result + '«O:' + FOperator + '»';
    if FRight <> nil
    then Result := Result + '«R:' + FRight.DumpExpression + '»';
  end;
end;

function TGDBMIExpression.GetExpression(var AResult: String): Boolean;
var
  R: TGDBMIExecResult;
  S: String;
  List: TStrings;
  GDBType: TGDBType;
begin  
  Result := False;
  
  if FLeft = nil
  then AResult := ''
  else begin
    if not FLeft.GetExpression(S) then Exit;
    AResult := S;
  end;

  if FOperator = '('
  then begin
    if not FRight.GetExpression(S) then Exit;
    AResult := AResult + '(' + S + ')';
  end
  else if FOperator = '['
  then begin              
    if not FRight.GetExpression(S) then Exit;
    AResult := AResult + '[' + S + ']';
  end
  else begin
    if (Length(FOperator) > 0)
    and (FOperator[1] = '''')
    then AResult := AResult + ConvertToCString(FOperator)
    else begin                                           
      GDBType := FDebugger.GetGDBTypeInfo(FOperator);
      if GDBType = nil
      then begin
        // no type possible, use literal operator
        AResult := AResult + FOperator;
      end;

      if not FDebugger.ExecuteCommand('ptype %s', [FOperator], [cfIgnoreError, cfNoMiCommand], R)
      then Exit;
      
      if R.State = dsError
      then begin
        // no type possible, use literal operator
        AResult := AResult + FOperator;
      end
      else begin
        DebugLn('PType result: ', R.Values);
        List := CreateValueList(R.Values);
        S := List.Values['type'];
        DebugLn('PType type: ', S);
        List.Free;
        if (S <> '') and (S[1] = '^') and (Pos('class', S) <> 0)
        then begin
          AResult := AResult + GetPart('^', ' ', S) + '(' + FOperator + ')';
        end
        else begin
          // no type possible or no class, use literal operator
          AResult := AResult + FOperator;
        end
      end;
    end;
    if FRight <> nil
    then begin
      if not FRight.GetExpression(S) then Exit;
      AResult := AResult + S;
    end;
  end;
  
  Result := True;
end;

{ TGDBMIType }

constructor TGDBMIType.CreateFromResult(const AResult: TGDBMIExecResult);
begin
  // TODO: add check ?
  CreateFromValues(AResult.Values);
end;

initialization
  RegisterDebugger(TGDBMIDebugger);
  
end.
