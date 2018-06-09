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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit debugger_eventlog_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLType, LCLIntf, Forms, StdCtrls, Spin, CheckLst, ComCtrls, ColorBox, Graphics,
  // DebuggerIntf
  DbgIntfDebuggerBase,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf,
  // IDE
  Debugger, LazarusIDEStrConsts, EnvironmentOpts, BaseDebugManager;

type
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
    procedure chkLimitLinecountChange(Sender: TObject);
    procedure chkUseEventLogColorsChange(Sender: TObject);
    procedure ColorTreeAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var {%H-}PaintImages, DefaultDraw: Boolean);
    procedure ColorTreeChange(Sender: TObject; {%H-}Node: TTreeNode);
    procedure ColorTreeClick(Sender: TObject);
    procedure ForegroundColorBoxChange(Sender: TObject);
  private
    FColors: array[TDBGEventType] of TDebuggerEventLogColor;
    class function GetCategoryStr(ACategory: TDBGEventCategory): String;
    procedure UpdateSelectedColor;
  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

const
  COLOR_NODE_PREFIX = ' abc  ';

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
  DefaultDraw := Stage <> cdPostPaint;
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
    Sender.Canvas.Brush.Color := Sender.Color;
    Sender.Canvas.Font.Color := Sender.Font.Color;
  end;
  NodeRect := Node.DisplayRect(True);
  FullAbcWidth := Sender.Canvas.TextExtent(COLOR_NODE_PREFIX).cx;
  Inc(NodeRect.Right, FullAbcWidth);
  TextY := (NodeRect.Top + NodeRect.Bottom - Sender.Canvas.TextHeight(Node.Text)) div 2;
  Sender.Canvas.FillRect(NodeRect);
  Sender.Canvas.TextOut(NodeRect.Left + FullAbcWidth, TextY, Node.Text);

  // Draw preview box - Background
  Sender.Canvas.Brush.Color := FColors[TDBGEventType(Node.Index)].Background;
  Sender.Canvas.FillRect(NodeRect.Left + 2, NodeRect.Top + 2, NodeRect.Left+FullAbcWidth - 2, NodeRect.Bottom - 2);

  s := 'abc';
  Sender.Canvas.Font.Color := FColors[TDBGEventType(Node.Index)].Foreground;
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
      FColors[TDBGEventType(ColorTree.Selected.Index)].Foreground := ForeGroundColorBox.Selected;
    if Sender = BackGroundColorBox then
      FColors[TDBGEventType(ColorTree.Selected.Index)].Background := BackGroundColorBox.Selected;
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

procedure TDebuggerEventLogOptionsFrame.chkLimitLinecountChange(Sender: TObject);
begin
  seLimitLinecount.Enabled := (Sender as TCheckBox).Checked;
end;

class function TDebuggerEventLogOptionsFrame.GetCategoryStr(ACategory: TDBGEventCategory): String;
begin
  case ACategory of
    ecBreakpoint: Result := lisDebugOptionsFrmBreakpoint;
    ecProcess: Result := lisDebugOptionsFrmProcess;
    ecThread: Result := lisDebugOptionsFrmThread;
    ecModule: Result := lisDebugOptionsFrmModule;
    ecOutput: Result := lisDebugOptionsFrmOutput;
    ecWindows: Result := lisDebugOptionsFrmWindows;
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
    ForegroundColorBox.Selected := FColors[TDBGEventType(ColorTree.Selected.Index)].Foreground;
    BackgroundColorBox.Selected := FColors[TDBGEventType(ColorTree.Selected.Index)].Background;
  end;
end;

function TDebuggerEventLogOptionsFrame.GetTitle: String;
begin
  Result := lisDebugOptionsFrmEventLog;
end;

procedure TDebuggerEventLogOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  Category: TDBGEventCategory;
  i: TDBGEventType;
  DBGEventNames: array[TDBGEventType] of string;
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
  gbColors.Caption := dlgColors;
  chkUseEventLogColors.Caption := lisDebugOptionsFrmUseEventLogColors;
  ForeGroundLabel.Caption := dlgForecolor;
  BackGroundLabel.Caption := dlgBackColor;

  DBGEventNames[etDefault             ] := lisDBGENDefaultColor;
  DBGEventNames[etBreakpointEvaluation] := lisDBGENBreakpointEvaluation;
  DBGEventNames[etBreakpointHit       ] := lisDBGENBreakpointHit;
  DBGEventNames[etBreakpointMessage   ] := lisDBGENBreakpointMessage;
  DBGEventNames[etBreakpointStackDump ] := lisDBGENBreakpointStackDump;
  DBGEventNames[etExceptionRaised     ] := lisDBGENExceptionRaised;
  DBGEventNames[etModuleLoad          ] := lisDBGENModuleLoad;
  DBGEventNames[etModuleUnload        ] := lisDBGENModuleUnload;
  DBGEventNames[etOutputDebugString   ] := lisDBGENOutputDebugString;
  DBGEventNames[etProcessExit         ] := lisDBGENProcessExit;
  DBGEventNames[etProcessStart        ] := lisDBGENProcessStart;
  DBGEventNames[etThreadExit          ] := lisDBGENThreadExit;
  DBGEventNames[etThreadStart         ] := lisDBGENThreadStart;
  DBGEventNames[etWindowsMessagePosted] := lisDBGENWindowsMessagePosted;
  DBGEventNames[etWindowsMessageSent  ] := lisDBGENWindowsMessageSent;

  for i := Low(DebuggerDefaultColors) to High(DebuggerDefaultColors) do
    ColorTree.Items.Add(nil, DBGEventNames[i]);

  chkLimitLinecountChange(chkLimitLinecount);
  chkUseEventLogColorsChange(chkUseEventLogColors);
end;

procedure TDebuggerEventLogOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);

  procedure SetChecked(ACategory: TDBGEventCategory; AChecked: Boolean); inline;
  begin
    cbMessages.Checked[Ord(ACategory)] := AChecked;
  end;

var
  EventType: TDBGEventType;
begin
  with EnvironmentOptions do
  begin
    chkClearLogOnRun.Checked := DebuggerEventLogClearOnRun;
    chkLimitLinecount.Checked := DebuggerEventLogCheckLineLimit;
    seLimitLinecount.Value := DebuggerEventLogLineLimit;
    SetChecked(ecBreakpoint, DebuggerEventLogShowBreakpoint);
    SetChecked(ecProcess, DebuggerEventLogShowProcess);
    SetChecked(ecThread, DebuggerEventLogShowThread);
    SetChecked(ecModule, DebuggerEventLogShowModule);
    SetChecked(ecOutput, DebuggerEventLogShowOutput);
    SetChecked(ecWindows, DebuggerEventLogShowWindows);
    SetChecked(ecDebugger, DebuggerEventLogShowDebugger);
    chkUseEventLogColors.Checked := DebuggerEventLogUseColors;
    for EventType := Low(TDBGEventType) to High(TDBGEventType) do
      FColors[EventType] := DebuggerEventLogColors[EventType];
  end;
end;

procedure TDebuggerEventLogOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);

  function GetChecked(ACategory: TDBGEventCategory): Boolean; inline;
  begin
    Result := cbMessages.Checked[Ord(ACategory)];
  end;

var
  EventType: TDBGEventType;
begin
  with EnvironmentOptions do
  begin
    DebuggerEventLogClearOnRun := chkClearLogOnRun.Checked;
    DebuggerEventLogCheckLineLimit := chkLimitLinecount.Checked;
    DebuggerEventLogLineLimit := seLimitLinecount.Value;
    DebuggerEventLogShowBreakpoint := GetChecked(ecBreakpoint);
    DebuggerEventLogShowProcess := GetChecked(ecProcess);
    DebuggerEventLogShowThread := GetChecked(ecThread);
    DebuggerEventLogShowModule := GetChecked(ecModule);
    DebuggerEventLogShowOutput := GetChecked(ecOutput);
    DebuggerEventLogShowWindows := GetChecked(ecWindows);
    DebuggerEventLogShowDebugger := GetChecked(ecDebugger);
    DebuggerEventLogUseColors := chkUseEventLogColors.Checked;
    for EventType := Low(TDBGEventType) to High(TDBGEventType) do
      DebuggerEventLogColors[EventType] := FColors[EventType];
  end;
end;

class function TDebuggerEventLogOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TDebuggerOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupDebugger, TDebuggerEventLogOptionsFrame, DbgOptionsEventLog);
end.

