{              ----------------------------------------------
                GDBMiServerDebugger.pp  -  Debugger class for gdbserver
               ----------------------------------------------

 This unit contains the debugger class for the GDB/MI debugger through SSH.

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
unit GDBMIServerDebugger;

{$mode objfpc}
{$H+}

interface

uses
  Classes, sysutils,
  // DebuggerIntf
  DbgIntfDebuggerBase,
  // LazDebuggerGdbmi
  GDBMIDebugger, GDBMIMiscClasses;
  
type

  { TGDBMIServerDebugger }

  TGDBMIServerDebugger = class(TGDBMIDebuggerBase)
  private
  protected
    function CreateCommandInit: TGDBMIDebuggerCommandInitDebugger; override;
    function CreateCommandStartDebugging(AContinueCommand: TGDBMIDebuggerCommand): TGDBMIDebuggerCommandStartDebugging; override;
    procedure InterruptTarget; override;
  public
    function NeedReset: Boolean; override;
    class function CreateProperties: TDebuggerProperties; override;  // Creates debuggerproperties
    class function Caption: String; override;
    class function RequiresLocalExecutable: Boolean; override;
  end;

  { TGDBMIServerDebuggerProperties }

  TGDBMIServerDebuggerProperties = class(TGDBMIDebuggerPropertiesBase)
  private
    FArchitecture: string;
    FDebugger_Remote_Hostname: string;
    FDebugger_Remote_Port: string;
    FDebugger_Remote_DownloadExe: boolean;
    FRemoteTimeout: integer;
    FSkipSettingLocalExeName: Boolean;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Debugger_Remote_Hostname: String read FDebugger_Remote_Hostname write FDebugger_Remote_Hostname;
    property Debugger_Remote_Port: String read FDebugger_Remote_Port write FDebugger_Remote_Port;
    property Debugger_Remote_DownloadExe: boolean read FDebugger_Remote_DownloadExe write FDebugger_Remote_DownloadExe;
    property RemoteTimeout: integer read FRemoteTimeout write FRemoteTimeout default -1;
    property Architecture: string read FArchitecture write FArchitecture;
    property SkipSettingLocalExeName: Boolean read FSkipSettingLocalExeName write FSkipSettingLocalExeName default False;
  published
    property Debugger_Startup_Options;
    {$IFDEF UNIX}
    property ConsoleTty;
    {$ENDIF}
    property MaxDisplayLengthForString;
    property MaxDisplayLengthForStaticArray;
    property MaxLocalsLengthForStaticArray;
    property TimeoutForEval;
    property WarnOnTimeOut;
    property WarnOnInternalError;
    property EncodeCurrentDirPath;
    property EncodeExeFileName;
    property InternalStartBreak;
    property UseNoneMiRunCommands;
    property DisableLoadSymbolsForLibraries;
    property DisableForcedBreakpoint;
    //property WarnOnSetBreakpointError;
    property CaseSensitivity;
    property GdbValueMemLimit;
    property GdbLocalsValueMemLimit;
    property AssemblerStyle;
    property DisableStartupShell;
    property FixStackFrameForFpcAssert;
    property FixIncorrectStepOver;
    property InternalExceptionBreakPoints;
  end;

procedure Register;

implementation

resourcestring
  GDBMiSNoAsyncMode = 'GDB does not support async mode';

type

  { TGDBMIServerDebuggerCommandInitDebugger }

  TGDBMIServerDebuggerCommandInitDebugger = class(TGDBMIDebuggerCommandInitDebugger)
  protected
    function  DoExecute: Boolean; override;
  end;

  { TGDBMIServerDebuggerCommandStartDebugging }

  TGDBMIServerDebuggerCommandStartDebugging = class(TGDBMIDebuggerCommandStartDebugging)
  protected
    function GdbRunCommand: String; override;
    procedure DetectTargetPid(InAttach: Boolean = False); override;
    function  DoTargetDownload: boolean; override;
    function DoChangeFilename: Boolean; override;
  end;

{ TGDBMIServerDebuggerCommandStartDebugging }

function TGDBMIServerDebuggerCommandStartDebugging.GdbRunCommand: String;
begin
  Result := '-exec-continue';
end;

procedure TGDBMIServerDebuggerCommandStartDebugging.DetectTargetPid(InAttach: Boolean);
begin
  // do nothing // prevent dsError in inherited
end;

function TGDBMIServerDebuggerCommandStartDebugging.DoTargetDownload: boolean;
begin
  Result := True;
  if TGDBMIServerDebuggerProperties(DebuggerProperties).FDebugger_Remote_DownloadExe then
  begin
    // Called after -file-exec-and-symbols, so gdb knows what file to download
    // If call sequence is different, then supply binary file name below as parameter
    Result := ExecuteCommand('-target-download', [], [cfCheckError]);
    Result := Result and (DebuggerState <> dsError);
  end;
end;

function TGDBMIServerDebuggerCommandStartDebugging.DoChangeFilename: Boolean;
begin
  Result := True;
  if not TGDBMIServerDebuggerProperties(DebuggerProperties).SkipSettingLocalExeName then
    Result := inherited DoChangeFilename;
end;

{ TGDBMIServerDebuggerCommandInitDebugger }

function TGDBMIServerDebuggerCommandInitDebugger.DoExecute: Boolean;
var
  R: TGDBMIExecResult;
  t: Integer;
  s: String;
begin
  Result := inherited DoExecute;
  if (not FSuccess) then exit;

  if not TGDBMIDebuggerBase(FTheDebugger).AsyncModeEnabled then begin
    SetDebuggerErrorState(GDBMiSNoAsyncMode);
    FSuccess := False;
    exit;
  end;

  s := TGDBMIServerDebuggerProperties(DebuggerProperties).Architecture;
  if s <> '' then
    ExecuteCommand(Format('set architecture %s', [s]), R);

  t := TGDBMIServerDebuggerProperties(DebuggerProperties).RemoteTimeout;
  if t >= 0 then
    ExecuteCommand(Format('set remotetimeout %d', [t]), R);

  // TODO: Maybe should be done in CommandStart, But Filename, and Environment will be done before Start
  FSuccess := ExecuteCommand(Format('target remote %s:%s',
                             [TGDBMIServerDebuggerProperties(DebuggerProperties).FDebugger_Remote_Hostname,
                              TGDBMIServerDebuggerProperties(DebuggerProperties).Debugger_Remote_Port ]),
                             R);
  FSuccess := FSuccess and (r.State <> dsError);
end;


{ TGDBMIServerDebuggerProperties }

constructor TGDBMIServerDebuggerProperties.Create;
begin
  inherited Create;
  FDebugger_Remote_Hostname:= '';
  FDebugger_Remote_Port:= '2345';
  FDebugger_Remote_DownloadExe := False;
  FRemoteTimeout := -1;
  FArchitecture := '';
  FSkipSettingLocalExeName := False;
  UseAsyncCommandMode := True;
end;

procedure TGDBMIServerDebuggerProperties.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TGDBMIServerDebuggerProperties then begin
    FDebugger_Remote_Hostname := TGDBMIServerDebuggerProperties(Source).FDebugger_Remote_Hostname;
    FDebugger_Remote_Port := TGDBMIServerDebuggerProperties(Source).FDebugger_Remote_Port;
    FDebugger_Remote_DownloadExe := TGDBMIServerDebuggerProperties(Source).FDebugger_Remote_DownloadExe;
    FRemoteTimeout := TGDBMIServerDebuggerProperties(Source).FRemoteTimeout;
    FArchitecture := TGDBMIServerDebuggerProperties(Source).FArchitecture;
    FSkipSettingLocalExeName := TGDBMIServerDebuggerProperties(Source).FSkipSettingLocalExeName;
    UseAsyncCommandMode := True;
  end;
end;


{ TGDBMIServerDebugger }

class function TGDBMIServerDebugger.Caption: String;
begin
  Result := 'GNU remote debugger (gdbserver)';
end;

function TGDBMIServerDebugger.CreateCommandInit: TGDBMIDebuggerCommandInitDebugger;
begin
  Result := TGDBMIServerDebuggerCommandInitDebugger.Create(Self);
end;

function TGDBMIServerDebugger.CreateCommandStartDebugging(
  AContinueCommand: TGDBMIDebuggerCommand): TGDBMIDebuggerCommandStartDebugging;
begin
  Result:= TGDBMIServerDebuggerCommandStartDebugging.Create(Self, AContinueCommand);
end;

procedure TGDBMIServerDebugger.InterruptTarget;
begin
  if not( CurrentCmdIsAsync and (CurrentCommand <> nil) ) then begin
    exit;
  end;

  inherited InterruptTarget;
end;

function TGDBMIServerDebugger.NeedReset: Boolean;
begin
  Result := True;
end;

class function TGDBMIServerDebugger.CreateProperties: TDebuggerProperties;
begin
  Result := TGDBMIServerDebuggerProperties.Create;
end;

class function TGDBMIServerDebugger.RequiresLocalExecutable: Boolean;
begin
  Result := False;
end;


procedure Register;
begin
  RegisterDebugger(TGDBMIServerDebugger);
end;

end.


