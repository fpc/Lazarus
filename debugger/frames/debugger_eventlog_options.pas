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
  Debugger, LazarusIDEStrConsts, IDEOptionsIntf, EnvironmentOpts;

type
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
    class function GetCategoryStr(ACategory: TDBGEventCategory): String;
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

class function TDebuggerEventLogOptionsFrame.GetCategoryStr(ACategory: TDBGEventCategory): String;
begin
  case ACategory of
    ecBreakpoint: Result := lisDebugOptionsFrmBreakpoint;
    ecProcess: Result := lisDebugOptionsFrmProcess;
    ecThread: Result := lisDebugOptionsFrmThread;
    ecModule: Result := lisDebugOptionsFrmModule;
    ecOutput: Result := lisDebugOptionsFrmOutput;
    ecWindow: Result := lisDebugOptionsFrmWindow;
    ecDebugger: Result := lisDebugOptionsFrmDebugger;
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
  Category: TDBGEventCategory;
begin
  // general
  gbGeneral.Caption := lisMenuInsertGeneral;
  chkClearLogOnRun.Caption := lisDebugOptionsFrmClearLogOnRun;
  chkLimitLinecount.Caption := lisDebugOptionsFrmLimitLinecountTo;

  // messages
  gbMessages.Caption := lisMenuViewMessages;
  for Category := Low(TDBGEventCategory) to High(TDBGEventCategory) do
    cbMessages.Items.Add(GetCategoryStr(Category));

  // colors
  gbColors.Caption := dlgEnvColors;
  // TODO: colors
end;

procedure TDebuggerEventLogOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);

  procedure SetChecked(ACategory: TDBGEventCategory; AChecked: Boolean); inline;
  begin
    cbMessages.Checked[Ord(ACategory)] := AChecked;
  end;

begin
  with AOptions as TEnvironmentOptions do
  begin
    chkClearLogOnRun.Checked := DebuggerEventLogClearOnRun;
    chkLimitLinecount.Checked := DebuggerEventLogCheckLineLimit;
    seLimitLinecount.Value := DebuggerEventLogLineLimit;
    SetChecked(ecBreakpoint, DebuggerEventLogShowBreakpoint);
    SetChecked(ecProcess, DebuggerEventLogShowProcess);
    SetChecked(ecThread, DebuggerEventLogShowThread);
    SetChecked(ecModule, DebuggerEventLogShowModule);
    SetChecked(ecOutput, DebuggerEventLogShowOutput);
    SetChecked(ecWindow, DebuggerEventLogShowWindow);
    SetChecked(ecDebugger, DebuggerEventLogShowDebugger);
  end;
end;

procedure TDebuggerEventLogOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);

  function GetChecked(ACategory: TDBGEventCategory): Boolean; inline;
  begin
    Result := cbMessages.Checked[Ord(ACategory)];
  end;

begin
  with AOptions as TEnvironmentOptions do
  begin
    DebuggerEventLogClearOnRun := chkClearLogOnRun.Checked;
    DebuggerEventLogCheckLineLimit := chkLimitLinecount.Checked;
    DebuggerEventLogLineLimit := seLimitLinecount.Value;
    DebuggerEventLogShowBreakpoint := GetChecked(ecBreakpoint);
    DebuggerEventLogShowProcess := GetChecked(ecProcess);
    DebuggerEventLogShowThread := GetChecked(ecThread);
    DebuggerEventLogShowModule := GetChecked(ecModule);
    DebuggerEventLogShowOutput := GetChecked(ecOutput);
    DebuggerEventLogShowWindow := GetChecked(ecWindow);
    DebuggerEventLogShowDebugger := GetChecked(ecDebugger);
  end;
end;

class function TDebuggerEventLogOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupDebugger, TDebuggerEventLogOptionsFrame, DbgOptionsEventLog);
end.

