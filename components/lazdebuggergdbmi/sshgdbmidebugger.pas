{ $Id$ }
{              ----------------------------------------------
                SSHGDBDebugger.pp  -  Debugger class for GDB
                                      through SSH
               ----------------------------------------------

 @created(Wed Jul 23rd WET 2003)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

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
unit SSHGDBMIDebugger;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Dialogs, Controls, Graphics,
  // LazUtils
  LazStringUtils,
  // IdeIntf
  PropEdits,
  // DebuggerIntf
  DbgIntfDebuggerBase,
  // LazDebuggerGdbmi
  GDBMIDebugger, GdbmiStringConstants;

type

  { TSSHGDBMIDebugger }

  TSSHGDBMIDebugger = class(TGDBMIDebuggerBase)
  private
  protected
    function ParseInitialization: Boolean; override;
    function CreateDebugProcess(const AOptions: String): Boolean; override;
  public
    class function CreateProperties: TDebuggerProperties; override;  // Creates debuggerproperties
    class function Caption: String; override;
    class function ExePaths: String; override;
    (* TODO: workaround for http://bugs.freepascal.org/view.php?id=21834   *)
    class function RequiresLocalExecutable: Boolean; override;
  end;

  { TSSHGDBMIDebuggerProperties }

  TSSHGDBMIDebuggerProperties = class(TGDBMIDebuggerPropertiesBase)
  private
    FAppendGDBtoSSHopt: Boolean;
    FNote: String; //dummy
    FRemoteGDBExe: String;
    FSSHStartupOptions: String;
    FSSH_TimeOut: Integer;
    procedure SetSSH_TimeOut(AValue: Integer);
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Note: String read FNote write FNote;
    property SSH_Startup_Options: String read FSSHStartupOptions write FSSHStartupOptions;
    property SSH_TimeOut: Integer read FSSH_TimeOut write SetSSH_TimeOut default 30;
    property Remote_GDB_Exe: String read FRemoteGDBExe write FRemoteGDBExe;
    property Append_GDB_to_SSH_opt: Boolean read FAppendGDBtoSSHopt write FAppendGDBtoSSHopt;
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
  end;

procedure Register;

implementation

type

  { TSSHGDBMINotePropertyEditor }

  TSSHGDBMINotePropertyEditor = class(TStringPropertyEditor)
  private
  protected
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
    procedure SetValue(const {%H-}NewValue: ansistring); override;
    procedure PropMeasureHeight(const {%H-}NewValue: ansistring;  {%H-}ACanvas:TCanvas;
                                var AHeight:Integer); override;
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
                  {%H-}AState: TPropEditDrawState); override;
  end;

{ TSSHGDBMIDebuggerProperties }

procedure TSSHGDBMIDebuggerProperties.SetSSH_TimeOut(AValue: Integer);
begin
  if FSSH_TimeOut = AValue then Exit;
  If AValue < 0 then AValue := 0;
  FSSH_TimeOut := AValue;
end;

constructor TSSHGDBMIDebuggerProperties.Create;
begin
  inherited Create;
  FRemoteGDBExe := 'gdb';
  FSSHStartupOptions := '';
  SSH_TimeOut := 30;
  FAppendGDBtoSSHopt := False;
  UseAsyncCommandMode := True;
end;

procedure TSSHGDBMIDebuggerProperties.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TSSHGDBMIDebuggerProperties then begin
    FRemoteGDBExe := TSSHGDBMIDebuggerProperties(Source).FRemoteGDBExe;
    FSSHStartupOptions := TSSHGDBMIDebuggerProperties(Source).FSSHStartupOptions;
    FSSH_TimeOut := TSSHGDBMIDebuggerProperties(Source).FSSH_TimeOut;
    FAppendGDBtoSSHopt := TSSHGDBMIDebuggerProperties(Source).FAppendGDBtoSSHopt;
    UseAsyncCommandMode := True;
  end;
end;

{ TSSHGDBMINotePropertyEditor }

function TSSHGDBMINotePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paReadOnly];
end;

function TSSHGDBMINotePropertyEditor.GetValue: ansistring;
begin
  Result := Format(lisNewTheGNUDebuggerThroughSshAllowsToRemoteDebugViaASsh, []);
end;

procedure TSSHGDBMINotePropertyEditor.PropMeasureHeight(const NewValue: ansistring; ACanvas: TCanvas; var AHeight: Integer);
begin
  AHeight := 100;
end;

procedure TSSHGDBMINotePropertyEditor.PropDrawValue(ACanvas: TCanvas;
  const ARect: TRect; AState: TPropEditDrawState);
var
  Style : TTextStyle;
begin
  FillChar(Style{%H-},SizeOf(Style),0);
  With Style do begin
    Alignment := taLeftJustify;
    Layout := tlTop;
    Opaque := False;
    Clipping := True;
    ShowPrefix := True;
    WordBreak := True;
    SingleLine := False;
    ExpandTabs := True;
    SystemFont := False;
  end;
  ACanvas.TextRect(ARect,ARect.Left+3,ARect.Top,GetVisualValue, Style);
end;

procedure TSSHGDBMINotePropertyEditor.SetValue (const NewValue: ansistring);
begin
  // cannot write to note
end;


{ TSSHGDBMIDebugger }

class function TSSHGDBMIDebugger.Caption: String;
begin
  Result := 'GNU debugger through SSH (gdb)';
end;

class function TSSHGDBMIDebugger.CreateProperties: TDebuggerProperties;
begin
  Result := TSSHGDBMIDebuggerProperties.Create;
end;

class function TSSHGDBMIDebugger.ExePaths: String;
begin
  Result := '/usr/bin/ssh user@remote /usr/bin/gdb';
end;

class function TSSHGDBMIDebugger.RequiresLocalExecutable: Boolean;
begin
  Result := False;
end;

function TSSHGDBMIDebugger.ParseInitialization: Boolean;

  function CheckReadLine(out ALine: String): Boolean;
  // does a checked read
  // returns True if we should process it
  // returns False if it is the gdb prompt
  begin
    ALine := ReadLine(True, 250);
    Result := (Pos('(gdb) ', ALine) <> 1) and
      (pos('=thread-group-added', ALine) <> 1);
    if Result and (ALine <> '')
    then ALine := StripLN(ReadLine);
  end;

var
  t, maxT: QWord;

  function IsTimeOut: Boolean;
  var
    t2, t3: QWord;
  begin
    if maxT = 0 then exit(False);
    t2 := GetTickCount64;
    if t2 < t
    then t3 := t2 + (High(t) - t)
    else t3 := t2 - t;
    Result := (t3 div 1000) > maxT;
  end;

var
  Line, ExtraText: String;
  NotGDB, WasTimeOut: Boolean;
begin
  Result := False;
  t := GetTickCount64;
  maxT := TSSHGDBMIDebuggerProperties(GetProperties).SSH_TimeOut;
  
  // strip leading empty lines
  NotGDB := CheckReadLine(Line);
  while (not IsTimeOut) and NotGDB and (Line = '') and
        (State <> dsError) and DebugProcessRunning
  do
    NotGDB := CheckReadLine(Line);;

  // succesfull login ?
  while (not IsTimeOut) and NotGDB and (Pos('try again', Line) > 0) do
    NotGDB := CheckReadLine(Line);

(*
  if Pos('authenticity', Line) > 0
  then begin
    //
    S := Line + LineEnding + ReadLine + ReadLine;
    if MessageDlg('Debugger', S, mtConfirmation, [mbYes, mbNo], 0) <> mrYes
    then begin
      SendCmdLn('no');
      Exit;
    end;
    SendCmdLn('yes');
    repeat
      Line := StripLN(ReadLine);
    until Pos('password:', Line) > 0
  end;
*)

  ExtraText := '';
  while (not IsTimeOut) and NotGDB and (State <> dsError) and DebugProcessRunning
  do begin
    // No prompt yet
    // skip known warnings
    if (Line <> '') and
      (pos('Pseudo-terminal will not be allocated because stdin is not a terminal', Line) <> 1)
    then
      ExtraText := ExtraText + LineEnding + Line;
    NotGDB := CheckReadLine(Line);
  end;

  WasTimeOut := IsTimeOut;

  if  (ExtraText <> '')
  and (MessageDlg(dlgGroupDebugger,
        Format(lisResponseContinue, [ExtraText + LineEnding]),
        mtConfirmation, [mbYes, mbNo], 0) <> mrYes)
  then begin
//    DebugProcess.Terminate(0);
    Exit;
  end;

  if not NotGDB and (not WasTimeOut)
  then Result := inherited ParseInitialization
  else begin
    // We got an unexpected result
    if ExtraText = '' then
      ExtraText := LineEnding + Line;
    if WasTimeOut then
      ExtraText := LineEnding + lisSSHDebuggerTimeout + LineEnding + ExtraText;
    MessageDlg(dlgGroupDebugger,
      Format(lisUnexpectedResultTheDebuggerWillTerminate, [ExtraText +
        LineEnding]),
      mtInformation, [mbOK], 0);
    Exit;
//    DebugProcess.Terminate(0);
  end;
end;

function TSSHGDBMIDebugger.CreateDebugProcess(const AOptions: String): Boolean;
var
  p: TSSHGDBMIDebuggerProperties;
  SshOpt: String;
begin
  p := TSSHGDBMIDebuggerProperties(GetProperties);
  SshOpt := p.FSSHStartupOptions;
  if p.FAppendGDBtoSSHopt then begin
    Result := inherited CreateDebugProcess(SshOpt + ' ' + p.FRemoteGDBExe + ' ' + AOptions);
  end
  else begin
    Result := inherited CreateDebugProcess(SshOpt);
    if Result then
      SendCmdLn(p.FRemoteGDBExe + ' ' + AOptions);
  end;
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(String), TSSHGDBMIDebuggerProperties, 'Note', TSSHGDBMINotePropertyEditor);
  RegisterDebugger(TSSHGDBMIDebugger);
end;

end.


