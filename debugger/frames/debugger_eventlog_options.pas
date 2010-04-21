{
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
unit debugger_eventlog_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, StdCtrls, Spin, CheckLst,
  LazarusIDEStrConsts, IDEOptionsIntf, EnvironmentOpts;

type
  TEventLogEvent = (
    eeBreakpoint,
    eeProcess,
    eeThread,
    eeModule,
    eeOutput,
    eeWindow,
    eeDebugger
  );
  { TDebuggerEventLogOptionsFrame }

  TDebuggerEventLogOptionsFrame = class(TAbstractIDEOptionsEditor)
    cbMessages: TCheckListBox;
    chkClearLogOnRun: TCheckBox;
    chkLimitLinecount: TCheckBox;
    gbGeneral: TGroupBox;
    gbMessages: TGroupBox;
    gbColors: TGroupBox;
    seLimitLinecount: TSpinEdit;
  private
    class function GetEventStr(AEvent: TEventLogEvent): String;
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TDebuggerEventLogOptionsFrame }

class function TDebuggerEventLogOptionsFrame.GetEventStr(AEvent: TEventLogEvent): String;
begin
  case AEvent of
    eeBreakpoint: Result := lisDebugOptionsFrmBreakpoint;
    eeProcess: Result := lisDebugOptionsFrmProcess;
    eeThread: Result := lisDebugOptionsFrmThread;
    eeModule: Result := lisDebugOptionsFrmModule;
    eeOutput: Result := lisDebugOptionsFrmOutput;
    eeWindow: Result := lisDebugOptionsFrmWindow;
    eeDebugger: Result := lisDebugOptionsFrmDebugger;
  else
    Result := '???';
  end;
end;

function TDebuggerEventLogOptionsFrame.GetTitle: String;
begin
  Result := lisDebugOptionsFrmEventLog;
end;

procedure TDebuggerEventLogOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  Event: TEventLogEvent;
begin
  // general
  gbGeneral.Caption := lisMenuInsertGeneral;
  chkClearLogOnRun.Caption := lisDebugOptionsFrmClearLogOnRun;
  chkLimitLinecount.Caption := lisDebugOptionsFrmLimitLinecountTo;

  // messages
  gbMessages.Caption := lisMenuViewMessages;
  for Event := Low(TEventLogEvent) to High(TEventLogEvent) do
    cbMessages.Items.Add(GetEventStr(Event));

  // colors
  gbColors.Caption := dlgEnvColors;
  // TODO: colors
end;

procedure TDebuggerEventLogOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);

  procedure SetChecked(AEvent: TEventLogEvent; AChecked: Boolean); inline;
  begin
    cbMessages.Checked[Ord(AEvent)] := AChecked;
  end;

begin
  with AOptions as TEnvironmentOptions do
  begin
    chkClearLogOnRun.Checked := DebuggerEventLogClearOnRun;
    chkLimitLinecount.Checked := DebuggerEventLogCheckLineLimit;
    seLimitLinecount.Value := DebuggerEventLogLineLimit;
    SetChecked(eeBreakpoint, DebuggerEventLogShowBreakpoint);
    SetChecked(eeProcess, DebuggerEventLogShowProcess);
    SetChecked(eeThread, DebuggerEventLogShowThread);
    SetChecked(eeModule, DebuggerEventLogShowModule);
    SetChecked(eeOutput, DebuggerEventLogShowOutput);
    SetChecked(eeWindow, DebuggerEventLogShowWindow);
    SetChecked(eeDebugger, DebuggerEventLogShowDebugger);
  end;
end;

procedure TDebuggerEventLogOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);

  function GetChecked(AEvent: TEventLogEvent): Boolean; inline;
  begin
    Result := cbMessages.Checked[Ord(AEvent)];
  end;

begin
  with AOptions as TEnvironmentOptions do
  begin
    DebuggerEventLogClearOnRun := chkClearLogOnRun.Checked;
    DebuggerEventLogCheckLineLimit := chkLimitLinecount.Checked;
    DebuggerEventLogLineLimit := seLimitLinecount.Value;
    DebuggerEventLogShowBreakpoint := GetChecked(eeBreakpoint);
    DebuggerEventLogShowProcess := GetChecked(eeProcess);
    DebuggerEventLogShowThread := GetChecked(eeThread);
    DebuggerEventLogShowModule := GetChecked(eeModule);
    DebuggerEventLogShowOutput := GetChecked(eeOutput);
    DebuggerEventLogShowWindow := GetChecked(eeWindow);
    DebuggerEventLogShowDebugger := GetChecked(eeDebugger);
  end;
end;

class function TDebuggerEventLogOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupDebugger, TDebuggerEventLogOptionsFrame, DbgOptionsEventLog);
end.

