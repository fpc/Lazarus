unit DebugThreadCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  FPDbgController,
  FpDbgClasses,
  FpDbgUtil,
  FpDbgInfo,
  FpPascalParser,
  FpPascalBuilder,
  FpErrorMessages,
  DbgIntfDebuggerBase,
  DbgIntfBaseTypes,
  strutils,
  debugthread,
  CustApp,
  SysUtils;

type

  { TFpDebugThreadCommandList }

  TFpDebugThreadCommandList = class(TFPList)
  public
    class function instance: TFpDebugThreadCommandList;
    function GetCommandByName(ATextName: string): TFpDebugThreadCommandClass;
  end;

  { TFpDebugThreadQuitDebugServerCommand }

  TFpDebugThreadQuitDebugServerCommand = class(TFpDebugThreadCommand)
  public
    function PreExecute(AController: TDbgController; out DoQueueCommand: boolean): boolean; override;
    function Execute(AController: TDbgController; out DoProcessLoop: boolean): boolean; override;
    class function TextName: string; override;
  end;

  { TFpDebugThreadSetFilenameCommand }

  TFpDebugThreadSetFilenameCommand = class(TFpDebugThreadCommand)
  private
    FFileName: string;
  public
    function Execute(AController: TDbgController; out DoProcessLoop: boolean): boolean; override;
    class function TextName: string; override;
  published
    property Filename: string read FFileName write FFileName;
  end;

  { TFpDebugThreadRunCommand }

  TFpDebugThreadRunCommand = class(TFpDebugThreadCommand)
  public
    function Execute(AController: TDbgController; out DoProcessLoop: boolean): boolean; override;
    class function TextName: string; override;
  end;

  { TFpDebugThreadContinueCommand }

  TFpDebugThreadContinueCommand = class(TFpDebugThreadCommand)
  public
    function Execute(AController: TDbgController; out DoProcessLoop: boolean): boolean; override;
    class function TextName: string; override;
  end;

  { TFpDebugThreadNextCommand }

  TFpDebugThreadNextCommand = class(TFpDebugThreadCommand)
  public
    function Execute(AController: TDbgController; out DoProcessLoop: boolean): boolean; override;
    class function TextName: string; override;
  end;

  { TFpDebugThreadStepCommand }

  TFpDebugThreadStepCommand = class(TFpDebugThreadCommand)
  public
    function Execute(AController: TDbgController; out DoProcessLoop: boolean): boolean; override;
    class function TextName: string; override;
  end;

  { TFpDebugThreadStepOutCommand }

  TFpDebugThreadStepOutCommand = class(TFpDebugThreadCommand)
  public
    function Execute(AController: TDbgController; out DoProcessLoop: boolean): boolean; override;
    class function TextName: string; override;
  end;

  { TFpDebugThreadStepIntoInstrCommand }

  TFpDebugThreadStepIntoInstrCommand = class(TFpDebugThreadCommand)
  public
    function Execute(AController: TDbgController; out DoProcessLoop: boolean): boolean; override;
    class function TextName: string; override;
  end;

  { TFpDebugThreadStepOverInstrCommand }

  TFpDebugThreadStepOverInstrCommand = class(TFpDebugThreadCommand)
  public
    function Execute(AController: TDbgController; out DoProcessLoop: boolean): boolean; override;
    class function TextName: string; override;
  end;

  { TFpDebugThreadStopCommand }

  TFpDebugThreadStopCommand = class(TFpDebugThreadCommand)
  public
    function Execute(AController: TDbgController; out DoProcessLoop: boolean): boolean; override;
    class function TextName: string; override;
  end;

  { TFpDebugThreadAddBreakpointCommand }

  TFpDebugThreadAddBreakpointCommand = class(TFpDebugThreadCommand)
  private
    FFileName: string;
    FLine: integer;
    FBreakPoint: FpDbgClasses.TDbgBreakpoint;
  public
    procedure ComposeSuccessEvent(var AnEvent: TFpDebugEvent); override;
    function Execute(AController: TDbgController; out DoProcessLoop: boolean): boolean; override;
    class function TextName: string; override;
  published
    property Filename: string read FFileName write FFileName;
    property Line: integer read FLine write FLine;
  end;

  { TFpDebugThreadRemoveBreakpointCommand }

  TFpDebugThreadRemoveBreakpointCommand = class(TFpDebugThreadCommand)
  private
    FLocationValue: TDBGPtr;
    function GetLocation: string;
    procedure SetLocation(AValue: string);
  public
    function Execute(AController: TDbgController; out DoProcessLoop: boolean): boolean; override;
    class function TextName: string; override;
  published
    property Location: string read GetLocation write SetLocation;
  end;

  { TFpDebugThreadGetLocationInfoCommand }

  TFpDebugThreadGetLocationInfoCommand = class(TFpDebugThreadCommand)
  private
    FLocationRec: TDBGLocationRec;
    FAddressValue: TDBGPtr;
    function GetAddress: string;
    procedure SetAddress(AValue: string);
  protected
    procedure ComposeSuccessEvent(var AnEvent: TFpDebugEvent); override;
  public
    function Execute(AController: TDbgController; out DoProcessLoop: boolean): boolean; override;
    class function TextName: string; override;
  published
    property Address: string read GetAddress write SetAddress;
  end;

  { TFpDebugThreadEvaluateCommand }

  TFpDebugThreadEvaluateCommand = class(TFpDebugThreadCommand)
  private
    FExpression: string;
    FResText: string;
    FValidity: TDebuggerDataState;
  public
    function Execute(AController: TDbgController; out DoProcessLoop: boolean): boolean; override;
    class function TextName: string; override;
    procedure ComposeSuccessEvent(var AnEvent: TFpDebugEvent); override;
  published
    property Expression: string read FExpression write FExpression;
  end;

  { TFpDebugThreadStackTraceCommand }

  TFpDebugThreadStackTraceCommand = class(TFpDebugThreadCommand)
  private
    FStackEntryArray: TFpDebugEventCallStackEntryArray;
  public
    function Execute(AController: TDbgController; out DoProcessLoop: boolean): boolean; override;
    class function TextName: string; override;
    procedure ComposeSuccessEvent(var AnEvent: TFpDebugEvent); override;
  end;

implementation

{ TFpDebugThreadCommandList }

var
  GFpDebugThreadCommandList: TFpDebugThreadCommandList = nil;

{ TFpDebugThreadStackTraceCommand }

function TFpDebugThreadStackTraceCommand.Execute(AController: TDbgController; out DoProcessLoop: boolean): boolean;
var
  ThreadCallStack: TDbgCallstackEntryList;
  i: integer;
begin
  AController.CurrentProcess.MainThread.PrepareCallStackEntryList;
  ThreadCallStack := AController.CurrentProcess.MainThread.CallStackEntryList;
  SetLength(FStackEntryArray, ThreadCallStack.Count);
  for i := 0 to ThreadCallStack.Count-1 do
    begin
    FStackEntryArray[i].AnAddress:=ThreadCallStack[i].AnAddress;
    FStackEntryArray[i].FrameAdress:=ThreadCallStack[i].FrameAdress;
    FStackEntryArray[i].FunctionName:=ThreadCallStack[i].FunctionName+ThreadCallStack[i].GetParamsAsString;
    FStackEntryArray[i].Line:=ThreadCallStack[i].Line;
    FStackEntryArray[i].SourceFile:=ThreadCallStack[i].SourceFile;
    end;
  // Clear the callstack immediately. Doing this each time the process continous is
  // cumbersome. And the chances that this command is called twice, so that
  // caching the result is usefull, are slim.
  AController.CurrentProcess.MainThread.ClearCallStack;
  DoProcessLoop:=false;
  result := true;
end;


class function TFpDebugThreadStackTraceCommand.TextName: string;
begin
  result := 'stacktrace';
end;

procedure TFpDebugThreadStackTraceCommand.ComposeSuccessEvent(var AnEvent: TFpDebugEvent);
begin
  inherited ComposeSuccessEvent(AnEvent);
  AnEvent.StackEntryArray:=FStackEntryArray;
end;

{ TFpDebugThreadEvaluateCommand }

procedure TFpDebugThreadEvaluateCommand.ComposeSuccessEvent(var AnEvent: TFpDebugEvent);
begin
  inherited ComposeSuccessEvent(AnEvent);
  AnEvent.Message:=FResText;
  AnEvent.Validity:=FValidity;
end;

function TFpDebugThreadEvaluateCommand.Execute(AController: TDbgController; out DoProcessLoop: boolean): boolean;
var
  AContext: TFpDbgInfoContext;
  APasExpr: TFpPascalExpression;
  ADbgInfo: TDbgInfo;
  Res: Boolean;
  APrettyPrinter: TFpPascalPrettyPrinter;
  ATypeInfo: TDBGType;

begin
  Result := False;
  DoProcessLoop:=false;
  ADbgInfo := AController.CurrentProcess.DbgInfo;
  AContext := ADbgInfo.FindContext(AController.CurrentThread.ID, 0, AController.CurrentProcess.GetInstructionPointerRegisterValue);
  if AContext = nil then
    begin
    FValidity:=ddsInvalid;
    exit;
    end;

  Result := True;
  AContext.MemManager.DefaultContext := AContext;
  APasExpr := TFpPascalExpression.Create(FExpression, AContext);
  try
    APasExpr.ResultValue; // trigger full validation
    if not APasExpr.Valid then
      begin
      FResText := ErrorHandler.ErrorAsString(APasExpr.Error);
      FValidity := ddsError;
      end
    else
      begin
      APrettyPrinter := TFpPascalPrettyPrinter.Create(sizeof(pointer));
      try
        APrettyPrinter.AddressSize:=AContext.SizeOfAddress;
        APrettyPrinter.MemManager := AContext.MemManager;
        Res := APrettyPrinter.PrintValue(FResText, ATypeInfo, APasExpr.ResultValue);
        if Res then
          begin
          FValidity:=ddsValid;
          end
        else
          begin
          FResText := 'Error';
          FValidity:=ddsValid;
          end;
      finally
        APrettyPrinter.Free;
      end;
      end;
  finally
    APasExpr.Free;
    AContext.ReleaseReference;
  end;
end;

class function TFpDebugThreadEvaluateCommand.TextName: string;
begin
  result := 'evaluate';
end;

{ TFpDebugThreadQuitDebugServerCommand }

function TFpDebugThreadQuitDebugServerCommand.PreExecute(AController: TDbgController; out DoQueueCommand: boolean): boolean;
begin
  DoQueueCommand:=false;
  CustomApplication.Terminate;
  result := true;
end;

function TFpDebugThreadQuitDebugServerCommand.Execute(AController: TDbgController; out DoProcessLoop: boolean): boolean;
begin
  result := true;
  DoProcessLoop := false;
end;

class function TFpDebugThreadQuitDebugServerCommand.TextName: string;
begin
  result := 'quitdebugserver';
end;

{ TFpDebugThreadRemoveBreakpointCommand }

function TFpDebugThreadRemoveBreakpointCommand.GetLocation: string;
begin
  result := FormatAddress(FLocationValue);
end;

procedure TFpDebugThreadRemoveBreakpointCommand.SetLocation(AValue: string);
begin
  FLocationValue := Hex2Dec(AValue);
end;

function TFpDebugThreadRemoveBreakpointCommand.Execute(AController: TDbgController; out DoProcessLoop: boolean): boolean;
begin
  result := false;
  DoProcessLoop:=false;
  if not assigned(AController.CurrentProcess) then
    begin
    log('Failed to remove breakpoint: No process', dllInfo);
    exit;
    end;
  if (FLocationValue<>0) then
    result := AController.CurrentProcess.RemoveBreak(FLocationValue)
  else
    log('Failed to remove breakpoint: No location given', dllInfo);
end;

class function TFpDebugThreadRemoveBreakpointCommand.TextName: string;
begin
  result := 'removebreakpoint';
end;

{ TFpDebugThreadStopCommand }

function TFpDebugThreadStopCommand.Execute(AController: TDbgController; out
  DoProcessLoop: boolean): boolean;
begin
  AController.Stop;
  DoProcessLoop:=true;
  result := true;
end;

class function TFpDebugThreadStopCommand.TextName: string;
begin
  result := 'stop';
end;

{ TFpDebugThreadStepOutCommand }

function TFpDebugThreadStepOutCommand.Execute(AController: TDbgController; out
  DoProcessLoop: boolean): boolean;
begin
  AController.StepOut;
  DoProcessLoop:=true;
  result := true;
end;

class function TFpDebugThreadStepOutCommand.TextName: string;
begin
  result := 'stepout';
end;

{ TFpDebugThreadStepOverInstrCommand }

function TFpDebugThreadStepOverInstrCommand.Execute(
  AController: TDbgController; out DoProcessLoop: boolean): boolean;
begin
  AController.StepOverInstr;
  DoProcessLoop:=true;
  result := true;
end;

class function TFpDebugThreadStepOverInstrCommand.TextName: string;
begin
  result := 'stepoverinstr';
end;

{ TFpDebugThreadStepIntoInstrCommand }

function TFpDebugThreadStepIntoInstrCommand.Execute(
  AController: TDbgController; out DoProcessLoop: boolean): boolean;
begin
  AController.StepIntoInstr;
  DoProcessLoop:=true;
  result := true;
end;

class function TFpDebugThreadStepIntoInstrCommand.TextName: string;
begin
  result := 'stepintoinstr';
end;

{ TFpDebugThreadStepCommand }

function TFpDebugThreadStepCommand.Execute(AController: TDbgController; out
  DoProcessLoop: boolean): boolean;
begin
  AController.Step;
  DoProcessLoop:=true;
  result := true;
end;

class function TFpDebugThreadStepCommand.TextName: string;
begin
  result := 'step';
end;

{ TFpDebugThreadNextCommand }

function TFpDebugThreadNextCommand.Execute(AController: TDbgController; out
  DoProcessLoop: boolean): boolean;
begin
  AController.Next;
  DoProcessLoop:=true;
  result := true;
end;

class function TFpDebugThreadNextCommand.TextName: string;
begin
  result := 'next';
end;

{ TFpDebugThreadGetLocationInfoCommand }

function TFpDebugThreadGetLocationInfoCommand.GetAddress: string;
begin
  result := FormatAddress(FAddressValue);
end;

procedure TFpDebugThreadGetLocationInfoCommand.SetAddress(AValue: string);
begin
  FAddressValue := Hex2Dec(AValue);
end;

procedure TFpDebugThreadGetLocationInfoCommand.ComposeSuccessEvent(var AnEvent: TFpDebugEvent);
begin
  inherited ComposeSuccessEvent(AnEvent);
  AnEvent.LocationRec:=FLocationRec;
end;

function TFpDebugThreadGetLocationInfoCommand.Execute(AController: TDbgController; out DoProcessLoop: boolean): boolean;
var
  sym, symproc: TFpDbgSymbol;
begin
  DoProcessLoop:=false;
  result := false;
  if Assigned(AController.CurrentProcess) then
    begin
    FLocationRec.FuncName:='';
    FLocationRec.SrcFile:='';
    FLocationRec.SrcFullName:='';
    FLocationRec.SrcLine:=0;

    if FAddressValue=0 then
      FLocationRec.Address := AController.CurrentProcess.GetInstructionPointerRegisterValue
    else
      FLocationRec.Address := FAddressValue;

    sym := AController.CurrentProcess.FindSymbol(FLocationRec.Address);
    if sym = nil then
      Exit;

    FLocationRec.SrcFile := ExtractFileName(sym.FileName);
    FLocationRec.SrcLine := sym.Line;
    FLocationRec.SrcFullName := sym.FileName;

    symproc := sym;
    while not (symproc.kind in [skProcedure, skFunction]) do
      symproc := symproc.Parent;

    if assigned(symproc) then
      FLocationRec.FuncName:=symproc.Name;
    sym.free;
    result := true;
    end;
end;

class function TFpDebugThreadGetLocationInfoCommand.TextName: string;
begin
  result := 'getlocationinfo'
end;

{ TFpDebugThreadAddBreakpointCommand }

procedure TFpDebugThreadAddBreakpointCommand.ComposeSuccessEvent(var AnEvent: TFpDebugEvent);
begin
  inherited ComposeSuccessEvent(AnEvent);
  AnEvent.BreakpointAddr:=FBreakPoint.Location;
end;

function TFpDebugThreadAddBreakpointCommand.Execute(AController: TDbgController; out DoProcessLoop: boolean): boolean;
begin
  result := false;
  DoProcessLoop:=false;
  if not assigned(AController.CurrentProcess) then
    begin
    log('Failed to add breakpoint: No process', dllInfo);
    exit;
    end;
  if (Filename<>'') and (line>-1) then
    begin
    FBreakPoint := AController.CurrentProcess.AddBreak(FileName, Line);
    result := assigned(FBreakPoint);
    end
  else
    log('Failed to add breakpoint: No filename and line-number given', dllInfo);
end;

class function TFpDebugThreadAddBreakpointCommand.TextName: string;
begin
  result := 'addbreakpoint';
end;

class function TFpDebugThreadCommandList.instance: TFpDebugThreadCommandList;
begin
  if not assigned(GFpDebugThreadCommandList) then
    GFpDebugThreadCommandList := TFpDebugThreadCommandList.Create;
  result := GFpDebugThreadCommandList;
end;

function TFpDebugThreadCommandList.GetCommandByName(ATextName: string): TFpDebugThreadCommandClass;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to count -1 do
    begin
    if TFpDebugThreadCommandClass(Items[i]).TextName=ATextName then
      result := TFpDebugThreadCommandClass(Items[i]);
    end;
end;

{ TFpDebugThreadContinueCommand }

function TFpDebugThreadContinueCommand.Execute(AController: TDbgController; out
  DoProcessLoop: boolean): boolean;
begin
  DoProcessLoop:=true;
  result := true;
end;

class function TFpDebugThreadContinueCommand.TextName: string;
begin
  result := 'continue';
end;

{ TFpDebugThreadRunCommand }

function TFpDebugThreadRunCommand.Execute(AController: TDbgController; out
  DoProcessLoop: boolean): boolean;
begin
  DoProcessLoop := AController.Run;
  result := DoProcessLoop;
end;

class function TFpDebugThreadRunCommand.TextName: string;
begin
  result := 'run';
end;

{ TFpDebugThreadSetFilenameCommand }

function TFpDebugThreadSetFilenameCommand.Execute(AController: TDbgController;
  out DoProcessLoop: boolean): boolean;
begin
  AController.ExecutableFilename:=FFileName;
  DoProcessLoop:=false;
  result:=true;
end;

class function TFpDebugThreadSetFilenameCommand.TextName: string;
begin
  result := 'filename'
end;

initialization
  TFpDebugThreadCommandList.instance.Add(TFpDebugThreadQuitDebugServerCommand);
  TFpDebugThreadCommandList.instance.Add(TFpDebugThreadSetFilenameCommand);
  TFpDebugThreadCommandList.instance.Add(TFpDebugThreadRunCommand);
  TFpDebugThreadCommandList.instance.Add(TFpDebugThreadContinueCommand);
  TFpDebugThreadCommandList.instance.Add(TFpDebugThreadStepOverInstrCommand);
  TFpDebugThreadCommandList.instance.Add(TFpDebugThreadStepIntoInstrCommand);
  TFpDebugThreadCommandList.instance.Add(TFpDebugThreadNextCommand);
  TFpDebugThreadCommandList.instance.Add(TFpDebugThreadStepCommand);
  TFpDebugThreadCommandList.instance.Add(TFpDebugThreadStepOutCommand);
  TFpDebugThreadCommandList.instance.Add(TFpDebugThreadStopCommand);
  TFpDebugThreadCommandList.instance.Add(TFpDebugThreadAddBreakpointCommand);
  TFpDebugThreadCommandList.instance.Add(TFpDebugThreadRemoveBreakpointCommand);
  TFpDebugThreadCommandList.instance.Add(TFpDebugThreadGetLocationInfoCommand);
  TFpDebugThreadCommandList.instance.Add(TFpDebugThreadEvaluateCommand);
  TFpDebugThreadCommandList.instance.Add(TFpDebugThreadStackTraceCommand);
finalization
  GFpDebugThreadCommandList.Free;
end.

