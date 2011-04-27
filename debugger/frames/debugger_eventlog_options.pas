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
  Classes, SysUtils, FileUtil, Graphics, Forms, StdCtrls, Spin, CheckLst,
  ComCtrls, LCLType, LCLIntf, ColorBox, Debugger, LazarusIDEStrConsts,
  IDEOptionsIntf, EnvironmentOpts;

type
  TEventLogColor = record
    Name: String;
    Foreground: TColor;
    Background: TColor;
  end;
  PEventLogColor = ^TEventLogColor;

  { TDebuggerEventLogOptionsFrame }

  TDebuggerEventLogOptionsFrame = class(TAbstractIDEOptionsEditor)
    BackGroundColorBox: TColorBox;
    BackGroundLabel: TLabel;
    cbMessages: TCheckListBox;
    chkUseEventLogColors: TCheckBox;
    chkClearLogOnRun: TCheckBox;
    chkLimitLinecount: TCheckBox;
    ColorTree: TTreeView;
    ForegroundColorBox: TColorBox;
    ForeGroundLabel: TLabel;
    gbGeneral: TGroupBox;
    gbMessages: TGroupBox;
    gbColors: TGroupBox;
    seLimitLinecount: TSpinEdit;
    procedure chkUseEventLogColorsChange(Sender: TObject);
    procedure ColorTreeAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure ColorTreeChange(Sender: TObject; Node: TTreeNode);
    procedure ColorTreeClick(Sender: TObject);
    procedure ForegroundColorBoxChange(Sender: TObject);
  private
    class function GetCategoryStr(ACategory: TDBGEventCategory): String;
    procedure UpdateSelectedColor;
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

const
  COLOR_NODE_PREFIX = ' abc  ';

  TestColors: array[0..12] of TEventLogColor = (
    (Name: 'Default Color';        Foreground: clWindowText; Background: clWindow),
    (Name: 'Breakpoint Evaluation';Foreground: clRed;        Background: clWindow),
    (Name: 'Breakpoint Hit';       Foreground: clRed;        Background: clWindow),
    (Name: 'Breakpoint Message';   Foreground: clRed;        Background: clWindow),
    (Name: 'Breakpoint Stack Dump';Foreground: clRed;        Background: clWindow),
    (Name: 'Exception Raised';     Foreground: clTeal;       Background: clWindow),
    (Name: 'Module Load';          Foreground: clBlue;       Background: clWindow),
    (Name: 'Module Unload';        Foreground: clBlue;       Background: clWindow),
    (Name: 'Output Debug String';  Foreground: clNavy;       Background: clWindow),
    (Name: 'Process Exit';         Foreground: clGray;       Background: clWindow),
    (Name: 'Process Start';        Foreground: clGray;       Background: clWindow),
    (Name: 'Thread Exit';          Foreground: clMaroon;     Background: clWindow),
    (Name: 'Thread Start';         Foreground: clMaroon;     Background: clWindow)
  );

{ TDebuggerEventLogOptionsFrame }

procedure TDebuggerEventLogOptionsFrame.ColorTreeAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var
  NodeRect: TRect;
  FullAbcWidth, AbcWidth: Integer;
  TextY, OldHeight: Integer;
  s: String;
begin
  DefaultDraw := (Node.Data = nil) or not (Stage = cdPostPaint);
  if DefaultDraw then
    Exit;

  // Draw node background and name
  if cdsSelected in State then
  begin
    Sender.Canvas.Brush.Color := Sender.SelectionColor;
    Sender.Canvas.Font.Color := InvertColor(Sender.SelectionColor);
  end
  else
  begin
    Sender.Canvas.Brush.Color := Sender.BackgroundColor;
    Sender.Canvas.Font.Color := Sender.Font.Color;
  end;
  NodeRect := Node.DisplayRect(True);
  FullAbcWidth := Sender.Canvas.TextExtent(COLOR_NODE_PREFIX).cx;
  TextY := (NodeRect.Top + NodeRect.Bottom - Sender.Canvas.TextHeight(Node.Text)) div 2;
  Sender.Canvas.FillRect(NodeRect);
  Sender.Canvas.TextOut(NodeRect.Left + FullAbcWidth, TextY, PEventLogColor(Node.Data)^.Name);

  // Draw preview box - Background
  Sender.Canvas.Brush.Color := PEventLogColor(Node.Data)^.Background;
  Sender.Canvas.FillRect(NodeRect.Left + 2, NodeRect.Top + 2, NodeRect.Left+FullAbcWidth - 2, NodeRect.Bottom - 2);

  s := 'abc';
  Sender.Canvas.Font.Color := PEventLogColor(Node.Data)^.Foreground;
  OldHeight := Sender.Canvas.Font.Height;
  Sender.Canvas.Font.Height := -(NodeRect.Bottom - NodeRect.Top - 7);
  TextY := (NodeRect.Top + NodeRect.Bottom - canvas.TextHeight(s)) div 2;
  AbcWidth := Sender.Canvas.TextExtent(s).cx;
  SetBkMode(Sender.Canvas.Handle, TRANSPARENT);
  Sender.Canvas.TextOut(NodeRect.Left+(FullAbcWidth - AbcWidth) div 2, TextY, s);
  SetBkMode(Sender.Canvas.Handle, OPAQUE);
  Sender.Canvas.Font.Height := OldHeight;
end;

procedure TDebuggerEventLogOptionsFrame.ColorTreeChange(Sender: TObject;
  Node: TTreeNode);
begin
  UpdateSelectedColor;
end;

procedure TDebuggerEventLogOptionsFrame.ColorTreeClick(Sender: TObject);
begin
  UpdateSelectedColor;
end;

procedure TDebuggerEventLogOptionsFrame.ForegroundColorBoxChange(Sender: TObject);
begin
  if Assigned(ColorTree.Selected) then
  begin
    if (Sender = ForegroundColorBox) then
      PEventLogColor(ColorTree.Selected.Data)^.Foreground := ForeGroundColorBox.Selected;
    if Sender = BackGroundColorBox then
      PEventLogColor(ColorTree.Selected.Data)^.Background := BackGroundColorBox.Selected;
    ColorTree.Invalidate;
  end;
end;

procedure TDebuggerEventLogOptionsFrame.chkUseEventLogColorsChange(Sender: TObject);
begin
  ColorTree.Enabled := chkUseEventLogColors.Checked;
  ForeGroundLabel.Enabled := chkUseEventLogColors.Checked;
  BackGroundLabel.Enabled := chkUseEventLogColors.Checked;
  ForegroundColorBox.Enabled := chkUseEventLogColors.Checked;
  BackGroundColorBox.Enabled := chkUseEventLogColors.Checked;
end;

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

procedure TDebuggerEventLogOptionsFrame.UpdateSelectedColor;
begin
  ForegroundColorBox.Enabled := Assigned(ColorTree.Selected);
  BackGroundColorBox.Enabled := Assigned(ColorTree.Selected);
  if Assigned(ColorTree.Selected) then
  begin
    ForegroundColorBox.Selected := PEventLogColor(ColorTree.Selected.Data)^.Foreground;
    BackgroundColorBox.Selected := PEventLogColor(ColorTree.Selected.Data)^.Background;
  end;
end;

function TDebuggerEventLogOptionsFrame.GetTitle: String;
begin
  Result := lisDebugOptionsFrmEventLog;
end;

procedure TDebuggerEventLogOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  Category: TDBGEventCategory;
  i: integer;
  Node: TTreeNode;
begin
  // general
  gbGeneral.Caption := lisGeneral;
  chkClearLogOnRun.Caption := lisDebugOptionsFrmClearLogOnRun;
  chkLimitLinecount.Caption := lisDebugOptionsFrmLimitLinecountTo;

  // messages
  gbMessages.Caption := lisMenuViewMessages;
  for Category := Low(TDBGEventCategory) to High(TDBGEventCategory) do
    cbMessages.Items.Add(GetCategoryStr(Category));

  // colors
  gbColors.Caption := dlgEnvColors;
  chkUseEventLogColors.Caption := lisDebugOptionsFrmUseEventLogColors;
  ForeGroundLabel.Caption := dlgForecolor;
  BackGroundLabel.Caption := dlgBackColor;
  // TODO: colors
  // add test colors to check functionality for now
  for i := Low(TestColors) to High(TestColors) do
  begin
    Node := ColorTree.Items.Add(nil, TestColors[i].Name);
    Node.Data := @TestColors[i];
  end;
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

