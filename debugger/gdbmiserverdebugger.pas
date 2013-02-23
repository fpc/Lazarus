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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit GDBMIServerDebugger;

{$mode objfpc}
{$H+}

interface

uses
  Classes, sysutils, GDBMIDebugger, BaseDebugManager, Debugger,
  GDBMIMiscClasses;
  
type

  { TGDBMIServerDebugger }

  TGDBMIServerDebugger = class(TGDBMIDebugger)
  private
  protected
    function CreateCommandInit: TGDBMIDebuggerCommandInitDebugger; override;
    function CreateCommandStartDebugging(AContinueCommand: TGDBMIDebuggerCommand): TGDBMIDebuggerCommandStartDebugging; override;
  public
    class function CreateProperties: TDebuggerProperties; override;  // Creates debuggerproperties
    class function Caption: String; override;
    class function RequiresLocalExecutable: Boolean; override;
  end;

  { TGDBMIServerDebuggerProperties }

  TGDBMIServerDebuggerProperties = class(TGDBMIDebuggerPropertiesBase)
  private
    FDebugger_Remote_Hostname: string;
    FDebugger_Remote_Port: string;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Debugger_Remote_Hostname: String read FDebugger_Remote_Hostname write FDebugger_Remote_Hostname;
    property Debugger_Remote_Port: String read FDebugger_Remote_Port write FDebugger_Remote_Port;
  published
    property Debugger_Startup_Options;
    {$IFDEF UNIX}
    property ConsoleTty;
    {$ENDIF}
    property MaxDisplayLengthForString;
    property TimeoutForEval;
    property WarnOnTimeOut;
    property EncodeCurrentDirPath;
    property EncodeExeFileName;
    property InternalStartBreak;
  end;

implementation

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
  end;

{ TGDBMIServerDebuggerCommandStartDebugging }

function TGDBMIServerDebuggerCommandStartDebugging.GdbRunCommand: String;
begin
  Result := '-exec-continue';
end;

{ TGDBMIServerDebuggerCommandInitDebugger }

function TGDBMIServerDebuggerCommandInitDebugger.DoExecute: Boolean;
var
  R: TGDBMIExecResult;
begin
  Result := True;

  FSuccess := ExecuteCommand(Format('target remote %s:%s',
                             [TGDBMIServerDebuggerProperties(DebuggerProperties).FDebugger_Remote_Hostname,
                              TGDBMIServerDebuggerProperties(DebuggerProperties).Debugger_Remote_Port ]),
                             R);
  FSuccess := FSuccess and (r.State <> dsError);
  if (not FSuccess) then exit;

  Result := inherited DoExecute;
end;


{ TGDBMIServerDebuggerProperties }

constructor TGDBMIServerDebuggerProperties.Create;
begin
  inherited Create;
  FDebugger_Remote_Hostname:= '';
  FDebugger_Remote_Port:= '2345';
end;

procedure TGDBMIServerDebuggerProperties.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  FDebugger_Remote_Hostname := TGDBMIServerDebuggerProperties(Source).FDebugger_Remote_Hostname;
  FDebugger_Remote_Port := TGDBMIServerDebuggerProperties(Source).FDebugger_Remote_Port;
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

class function TGDBMIServerDebugger.CreateProperties: TDebuggerProperties;
begin
  Result := TGDBMIServerDebuggerProperties.Create;
end;

class function TGDBMIServerDebugger.RequiresLocalExecutable: Boolean;
begin
  Result := False;
end;



initialization
  RegisterDebugger(TGDBMIServerDebugger);

end.


