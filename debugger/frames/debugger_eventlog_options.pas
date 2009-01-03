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
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls, Spin,
  LazarusIDEStrConsts, IDEOptionsIntf;

type

  { TDebuggerEventLogOptionsFrame }

  TDebuggerEventLogOptionsFrame = class(TAbstractIDEOptionsEditor)
    chkClearLogOnRun: TCheckBox;
    chkLimitLinecount: TCheckBox;
    chkMessagesBreakpoint: TCheckBox;
    chkMessagesInterface: TCheckBox;
    chkMessagesModule: TCheckBox;
    chkMessagesOutput: TCheckBox;
    chkMessagesProcess: TCheckBox;
    chkMessagesThread: TCheckBox;
    chkMessagesWindow: TCheckBox;
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
  chkMessagesInterface.Caption := lisDebugOptionsFrmInterface;
end;

procedure TDebuggerEventLogOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
begin
  // todo
end;

procedure TDebuggerEventLogOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  // todo
end;

class function TDebuggerEventLogOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := nil; // currently dont know
end;

initialization
  {$I debugger_eventlog_options.lrs}
  RegisterIDEOptionsEditor(GroupDebugger, TDebuggerEventLogOptionsFrame, DbgOptionsEventLog);
end.

