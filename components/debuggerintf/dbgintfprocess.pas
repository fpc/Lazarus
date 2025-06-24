unit DbgIntfProcess;

{$mode objfpc}{$H+}

// New FPC 331 TProcess to allow StdIn/StdOut redirection
{$UnDef HAS_NEW_PROCESS}{$UnDef USES_NEW_PROCESS}
{$IF FPC_Fullversion>=30301} {$define HAS_NEW_PROCESS}
{$ELSE}
{$IFDEF WINDOWS} {$define USES_NEW_PROCESS} {$define HAS_NEW_PROCESS} {$ENDIF}
{$IFDEF WINCE}   {$undef USES_NEW_PROCESS}  {$undef HAS_NEW_PROCESS} {$ENDIF}
{$IFDEF LINUX}   {$define USES_NEW_PROCESS} {$define HAS_NEW_PROCESS} {$ENDIF}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  {$IFDEF USES_NEW_PROCESS} process331
  {$ELSE} process
  {$ENDIF}
  ;

const
  // forwards
  poRunSuspended            = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} poRunSuspended;
  poWaitOnExit              = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} poWaitOnExit;
  poUsePipes                = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} poUsePipes;
  poStderrToOutPut          = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} poStderrToOutPut;
  poNoConsole               = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} poNoConsole;
  poNewConsole              = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} poNewConsole;
  poDefaultErrorMode        = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} poDefaultErrorMode;
  poNewProcessGroup         = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} poNewProcessGroup;
  poDebugProcess            = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} poDebugProcess;
  poDebugOnlyThisProcess    = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} poDebugOnlyThisProcess;
  poDetached                = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} poDetached;
  poPassInput               = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} poPassInput;
  poRunIdle                 = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} poRunIdle;
  swoNone                   = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} swoNone;
  swoHIDE                   = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} swoHIDE;

  swoMaximize               = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} swoMaximize;
  swoMinimize               = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} swoMinimize;
  swoRestore                = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} swoRestore;
  swoShow                   = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} swoShow;
  swoShowDefault            = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} swoShowDefault;
  swoShowMaximized          = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} swoShowMaximized;
  swoShowMinimized          = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} swoShowMinimized;
  swoshowMinNOActive        = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} swoshowMinNOActive;
  swoShowNA                 = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} swoShowNA;
  swoShowNoActivate         = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} swoShowNoActivate;
  swoShowNormal             = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} swoShowNormal;

  suoUseShowWindow          = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} suoUseShowWindow;
  suoUseSize                = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} suoUseSize;
  suoUsePosition            = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} suoUsePosition;
  suoUseCountChars          = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} suoUseCountChars;
  suoUseFillAttribute       = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} suoUseFillAttribute;

type

  TDescriptorType = (dtStdIn, dtStdOut, dtStdErr);

  // forwards
  EProcess = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} EProcess;
  TProcessOptions = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} TProcessOptions;
  TStartupOptions = {$IFDEF USES_NEW_PROCESS} process331. {$ELSE} process. {$ENDIF} TStartupOptions;

  { TProcessWithRedirect }

  TProcessWithRedirect = class(TProcess)
  public
    procedure SetRedirection(AType: TDescriptorType; const AFile: String; AnOverwrite: Boolean);
    procedure ApplyWin7Fix;
  end;

  TNotifyProcessEndMethod = procedure(AnError: String) of object;

  { TNotifyProcessEnd }

  TNotifyProcessEnd = class(TThread)
  private
    fEvent: TNotifyProcessEndMethod;
    fProcess: TProcessWithRedirect;
    FErrMsg: String;
  protected
    procedure Execute; override;
    procedure DoEvent;
  public
    constructor Create(pProcess: TProcessWithRedirect; pEvent: TNotifyProcessEndMethod);
  end;

const
  DBG_PROCESS_HAS_REDIRECT = {$IFDEF HAS_NEW_PROCESS} True {$ELSE} False {$ENDIF};

implementation

{ TProcessWithRedirect }

procedure TProcessWithRedirect.SetRedirection(AType: TDescriptorType; const AFile: String;
  AnOverwrite: Boolean);
{$IFDEF HAS_NEW_PROCESS}
const
  FMODE: array [boolean] of TFileWriteMode = (fwmAppend, fwmTruncate);
var
  d: TIODescriptor;
{$ENDIF}
begin
  if AFile = '' then
    exit;
  {$IFDEF HAS_NEW_PROCESS}
  case AType of
    dtStdIn:  d := InputDescriptor;
    dtStdOut: d := OutputDescriptor;
    dtStdErr: d := ErrorDescriptor;
  end;
  d.FileName := AFile;
  d.FileWriteMode := FMODE[AnOverwrite];
  {$ELSE}
  Assert(False, 'SetRedirections not available');
  {$ENDIF}
end;

procedure TProcessWithRedirect.ApplyWin7Fix;
begin
  {$IFDEF WINDOWS} {$IFDEF HAS_NEW_PROCESS}
  if (InputDescriptor.IOType  <>  iotDefault) or
     (OutputDescriptor.IOType <>  iotDefault) or
     (ErrorDescriptor.IOType  <>  iotDefault)
  then begin
    if (InputDescriptor.IOType = iotDefault) then begin
      InputDescriptor.IOType := iotHandle;
      InputDescriptor.CustomHandle := 3;
      InputDescriptor.CustomHandleIsInheritable := True;
      InputDescriptor.AutoCloseCustomHandle := False;
    end;
    if (OutputDescriptor.IOType = iotDefault) then begin
      OutputDescriptor.IOType := iotHandle;
      OutputDescriptor.CustomHandle := 7;
      OutputDescriptor.CustomHandleIsInheritable := True;
      OutputDescriptor.AutoCloseCustomHandle := False;
    end;
    if (ErrorDescriptor.IOType = iotDefault) then begin
      ErrorDescriptor.IOType := iotHandle;
      ErrorDescriptor.CustomHandle := 11;
      ErrorDescriptor.CustomHandleIsInheritable := True;
      ErrorDescriptor.AutoCloseCustomHandle := False;
    end;
  end;
  {$ENDIF} {$ENDIF}
end;

{ TNotifyProcessEnd }

procedure TNotifyProcessEnd.Execute;
begin
  try
    fProcess.Execute;
  except
    on E: Exception do FErrMsg := E.Message;
  end;
  Synchronize(@DoEvent);
  fProcess.Free;
end;

procedure TNotifyProcessEnd.DoEvent;
begin
  fEvent(FErrMsg);
end;

constructor TNotifyProcessEnd.Create(pProcess: TProcessWithRedirect; pEvent: TNotifyProcessEndMethod);
begin
  inherited Create(True);
  fProcess := pProcess;
  fProcess.Options := fProcess.Options + [poWaitOnExit];
  fEvent := pEvent;
  FreeOnTerminate := True;
  Start;
end;

end.

