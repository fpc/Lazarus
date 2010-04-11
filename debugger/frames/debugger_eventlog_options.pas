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
  Classes, SysUtils, FileUtil, Forms, StdCtrls, Spin,
  LazarusIDEStrConsts, IDEOptionsIntf, EnvironmentOpts;

type

  { TDebuggerEventLogOptionsFrame }

  TDebuggerEventLogOptionsFrame = class(TAbstractIDEOptionsEditor)
    chkMessagesWindow: TCheckBox;
    chkClearLogOnRun: TCheckBox;
    chkLimitLinecount: TCheckBox;
    chkMessagesBreakpoint: TCheckBox;
    chkMessagesModule: TCheckBox;
    chkMessagesOutput: TCheckBox;
    chkMessagesProcess: TCheckBox;
    chkMessagesThread: TCheckBox;
    chkMessagesDebugger: TCheckBox;
    gbGeneral: TGroupBox;
    gbMessages: TGroupBox;
    seLimitLinecount: TSpinEdit;
  private
    { private declarations }
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

function TDebuggerEventLogOptionsFrame.GetTitle: String;
begin
  Result := lisDebugOptionsFrmEventLog;
end;

procedure TDebuggerEventLogOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  gbGeneral.Caption := lisMenuInsertGeneral;
  chkClearLogOnRun.Caption := lisDebugOptionsFrmClearLogOnRun;
  chkLimitLinecount.Caption := lisDebugOptionsFrmLimitLinecountTo;
  gbMessages.Caption := lisMenuViewMessages;
  chkMessagesBreakpoint.Caption := lisDebugOptionsFrmBreakpoint;
  chkMessagesProcess.Caption := lisDebugOptionsFrmProcess;
  chkMessagesThread.Caption := lisDebugOptionsFrmThread;
  chkMessagesModule.Caption := lisDebugOptionsFrmModule;
  chkMessagesOutput.Caption := lisDebugOptionsFrmOutput;
  chkMessagesWindow.Caption := lisDebugOptionsFrmWindow;
  chkMessagesDebugger.Caption := lisDebugOptionsFrmDebugger;
end;

procedure TDebuggerEventLogOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEnvironmentOptions do
  begin
    chkClearLogOnRun.Checked := DebuggerEventLogClearOnRun;
    chkLimitLinecount.Checked := DebuggerEventLogCheckLineLimit;
    seLimitLinecount.Value := DebuggerEventLogLineLimit;
    chkMessagesBreakpoint.Checked := DebuggerEventLogShowBreakpoint;
    chkMessagesProcess.Checked := DebuggerEventLogShowProcess;
    chkMessagesThread.Checked := DebuggerEventLogShowThread;
    chkMessagesModule.Checked := DebuggerEventLogShowModule;
    chkMessagesOutput.Checked := DebuggerEventLogShowOutput;
    chkMessagesWindow.Checked := DebuggerEventLogShowWindow;
    chkMessagesDebugger.Checked := DebuggerEventLogShowDebugger;
  end;
end;

procedure TDebuggerEventLogOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEnvironmentOptions do
  begin
    DebuggerEventLogClearOnRun := chkClearLogOnRun.Checked;
    DebuggerEventLogCheckLineLimit := chkLimitLinecount.Checked;
    DebuggerEventLogLineLimit := seLimitLinecount.Value;
    DebuggerEventLogShowBreakpoint := chkMessagesBreakpoint.Checked;
    DebuggerEventLogShowProcess := chkMessagesProcess.Checked;
    DebuggerEventLogShowThread := chkMessagesThread.Checked;
    DebuggerEventLogShowModule := chkMessagesModule.Checked;
    DebuggerEventLogShowOutput := chkMessagesOutput.Checked;
    DebuggerEventLogShowWindow := chkMessagesWindow.Checked;
    DebuggerEventLogShowDebugger := chkMessagesDebugger.Checked;
  end;
end;

class function TDebuggerEventLogOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupDebugger, TDebuggerEventLogOptionsFrame, DbgOptionsEventLog);
end.

