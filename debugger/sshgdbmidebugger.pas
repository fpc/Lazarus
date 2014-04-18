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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit SSHGDBMIDebugger;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Controls, GDBMIDebugger,
  BaseDebugManager, Debugger, PropEdits, Graphics, LCLProc, LazarusIDEStrConsts;
  
type

  { TSSHGDBMIDebugger }

  TSSHGDBMIDebugger = class(TGDBMIDebugger)
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
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Note: String read FNote write FNote;
    property SSH_Startup_Options: String read FSSHStartupOptions write FSSHStartupOptions;
    property Remote_GDB_Exe: String read FRemoteGDBExe write FRemoteGDBExe;
    property Append_GDB_to_SSH_opt: Boolean read FAppendGDBtoSSHopt write FAppendGDBtoSSHopt;
  published
    property Debugger_Startup_Options;
    {$IFDEF UNIX}
    property ConsoleTty;
    {$ENDIF}
    property MaxDisplayLengthForString;
    property TimeoutForEval;
    property WarnOnTimeOut;
    property WarnOnInternalError;
    property EncodeCurrentDirPath;
    property EncodeExeFileName;
    property InternalStartBreak;
    property DisableLoadSymbolsForLibraries;
  end;

implementation

type

  { TSSHGDBMINotePropertyEditor }

  TSSHGDBMINotePropertyEditor = class(TStringPropertyEditor)
  private
  protected
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
    procedure SetValue(const NewValue: ansistring); override;
    procedure PropMeasureHeight(const NewValue: ansistring;  ACanvas:TCanvas;
                                var AHeight:Integer); override;
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
                  AState: TPropEditDrawState); override;
  end;

{ TSSHGDBMIDebuggerProperties }

constructor TSSHGDBMIDebuggerProperties.Create;
begin
  inherited Create;
  FRemoteGDBExe := 'gdb';
  FSSHStartupOptions := '';
  FAppendGDBtoSSHopt := False;
  UseAsyncCommandMode := True;
end;

procedure TSSHGDBMIDebuggerProperties.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TSSHGDBMIDebuggerProperties then begin
    FRemoteGDBExe := TSSHGDBMIDebuggerProperties(Source).FRemoteGDBExe;
    FSSHStartupOptions := TSSHGDBMIDebuggerProperties(Source).FSSHStartupOptions;
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
  FillChar(Style,SizeOf(Style),0);
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

  function CheckReadLine(var ALine: String): Boolean;
  // does a checked read
  // returns True if we should process it
  // returns False if it is the gdb prompt
  begin
    ALine := ReadLine(True);
    Result := Pos('(gdb)', ALine) = 0;
    if Result
    then ALine := StripLN(ReadLine);
  end;
var
  Line, ExtraText: String;
begin
  Result := False;
  
  // strip leading empty lines
  while CheckReadLine(Line) and (Line = '') do;

  // succesfull login ?
  while Pos('try again', Line) > 0 do CheckReadLine(Line);

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
  while CheckReadLine(Line) do
  begin
    // No prompt yet
    if ExtraText = ''
    then ExtraText := Line
    else ExtraText := ExtraText + ' ' + Line;
  end;
  
  if  (ExtraText <> '')
  and (MessageDlg(dlgGroupDebugger,
        Format(lisResponseContinue, [LineEnding + ExtraText + LineEnding]),
        mtConfirmation, [mbYes, mbNo], 0) <> mrYes)
  then begin
//    DebugProcess.Terminate(0);
    Exit;
  end;

  if Pos('(gdb)', Line) > 0
  then Result := inherited ParseInitialization
  else begin
    // We got an unexpected result
    MessageDlg(dlgGroupDebugger,
      Format(lisUnexpectedResultTheDebuggerWillTerminate, [LineEnding + Line +
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

initialization
  RegisterPropertyEditor(TypeInfo(String), TSSHGDBMIDebuggerProperties, 'Note', TSSHGDBMINotePropertyEditor);
  
  RegisterDebugger(TSSHGDBMIDebugger);

end.


