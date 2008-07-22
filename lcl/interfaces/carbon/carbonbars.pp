{ $Id$
                  -------------------------------------
                  CarbonBars.pp  -  Carbon bars classes
                  -------------------------------------

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CarbonBars;

{$mode objfpc}{$H+}

interface

// debugging defines
{$I carbondebug.inc}

uses
 // rtl+ftl
  Types, Classes, SysUtils, Math, Contnrs,
 // carbon bindings
{$ifdef ver2_2_0}
  FPCMacOSAll,
{$else}
  MacOSAll,
{$endif}
 // widgetset
  WSControls, WSLCLClasses, WSProc,
 // LCL Carbon
  CarbonDef, CarbonPrivate,
 // LCL
  LMessages, LCLMessageGlue, LCLProc, LCLType, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls, Menus;
  
type

  { TCarbonCustomBar }

  TCarbonCustomBar = class(TCarbonControl)
  private
  public
    function GetPosition: Integer; virtual;
    procedure SetPosition(APosition: Integer); virtual;

    procedure SetColor(const AColor: TColor); override;
    procedure SetFont(const AFont: TFont); override;
  end;

  { TCarbonProgressBar }

  TCarbonProgressBar = class(TCarbonCustomBar)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    procedure ApplyChanges; virtual;
  end;

  { TCarbonMovableBar }

  TCarbonMovableBar = class(TCarbonCustomBar)
  protected
    class function GetValidEvents: TCarbonControlEvents; override;
    procedure IndicatorMoved; override;
    procedure ValueChanged; override;
  end;

  { TCarbonTrackBar }

  TCarbonTrackBar = class(TCarbonMovableBar)
  private
    FTicks: Word;
    function GetTicks: Word;
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    procedure ApplyChanges; virtual;
  end;

  { TCarbonScrollBar }

  TCarbonScrollBar = class(TCarbonMovableBar)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    class function GetValidEvents: TCarbonControlEvents; override;
    procedure IndicatorMoved; override;
    procedure DoAction(AControlPart: ControlPartCode); override;
    procedure SetParams; virtual;
  end;

implementation

uses InterfaceBase, CarbonInt, CarbonProc, CarbonDbgConsts, CarbonUtils,
  CarbonWSStdCtrls, CarbonStrings, CarbonCanvas, CarbonGDIObjects;
  
{ TCarbonCustomBar }

{------------------------------------------------------------------------------
  Method:  TCarbonCustomBar.GetPosition
  Returns: The positon of Carbon bar
 ------------------------------------------------------------------------------}
function TCarbonCustomBar.GetPosition: Integer;
begin
  Result := GetValue;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomBar.SetPosition
  Params:  APosition - New position

  Sets the position of Carbon bar
 ------------------------------------------------------------------------------}
procedure TCarbonCustomBar.SetPosition(APosition: Integer);
begin
  SetValue(APosition);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomBar.SetColor
  Params:  AColor - New color

  Sets the color of control (for edit like controls)
 ------------------------------------------------------------------------------}
procedure TCarbonCustomBar.SetColor(const AColor: TColor);
begin
  // not supported
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomBar.SetFont
  Params:  AFont - New font

  Sets the font of control
 ------------------------------------------------------------------------------}
procedure TCarbonCustomBar.SetFont(const AFont: TFont);
begin
  // not supported
end;

{ TCarbonProgressBar }

{------------------------------------------------------------------------------
  Method:  TCarbonProgressBar.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon progress bar
 ------------------------------------------------------------------------------}
procedure TCarbonProgressBar.CreateWidget(const AParams: TCreateParams);
var
  ProgressBar: TCustomProgressBar;
  Control: ControlRef;
begin
  ProgressBar := LCLObject as TCustomProgressBar;

  // create determinate progress bar
  if OSError(
    CreateProgressBarControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      ProgressBar.Position, ProgressBar.Min, ProgressBar.Max, False, Control),
    Self, SCreateWidget, 'CreateProgressBarControl') then RaiseCreateWidgetError(LCLObject);

  Widget := Control;

  inherited;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonProgressBar.ApplyChanges

  Sets the parameters (Min, Max, Position) of Carbon progress bar
 ------------------------------------------------------------------------------}
procedure TCarbonProgressBar.ApplyChanges;
var
  ProgressBar: TCustomProgressBar;
begin
  ProgressBar := LCLObject as TCustomProgressBar;

  SetValue(ProgressBar.Position);
  SetMinimum(ProgressBar.Min);
  SetMaximum(ProgressBar.Max);
end;

{ TCarbonMovableBar }

{------------------------------------------------------------------------------
  Method:  TCarbonMovableBar.GetValidEvents
  Returns: Set of events with installed handlers
 ------------------------------------------------------------------------------}
class function TCarbonMovableBar.GetValidEvents: TCarbonControlEvents;
begin
  Result := [cceValueChanged, cceIndicatorMoved];
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMovableBar.IndicatorMoved

  Indicator moved event handler
 ------------------------------------------------------------------------------}
procedure TCarbonMovableBar.IndicatorMoved;
begin
  ValueChanged;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMovableBar.ValueChanged

  Value changed event handler
 ------------------------------------------------------------------------------}
procedure TCarbonMovableBar.ValueChanged;
begin
  LCLSendChangedMsg(LCLObject);
end;

{ TCarbonTrackBar }

{------------------------------------------------------------------------------
  Method:  TCarbonTrackBar.GetTicks
  Returns: Number of ticks

  Returns the number of ticks for the track bar
 ------------------------------------------------------------------------------}
function TCarbonTrackBar.GetTicks: Word;
var
  TrackBar: TCustomTrackBar;
begin
  Result := 0;
  TrackBar := LCLObject as TCustomTrackBar;
  if TrackBar = nil then Exit;
  if TrackBar.TickStyle = tsNone then Exit;

  if TrackBar.Frequency > 0 then
    Result := Math.Ceil(Abs(TrackBar.Max - TrackBar.Min) / TrackBar.Frequency) + 1
  else
    Result := 2;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTrackBar.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon track bar
 ------------------------------------------------------------------------------}
procedure TCarbonTrackBar.CreateWidget(const AParams: TCreateParams);
var
  TrackBar: TCustomTrackBar;
  Control: ControlRef;
begin
  TrackBar := LCLObject as TCustomTrackBar;

  FTicks := GetTicks;

  if OSError(
    CreateSliderControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      TrackBar.Position, TrackBar.Min, TrackBar.Max,
      kControlSliderPointsDownOrRight, FTicks, True, nil, Control),
    Self, SCreateWidget, 'CreateSliderControl') then RaiseCreateWidgetError(LCLObject);

  Widget := Control;

  inherited;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTrackBar.ApplyChanges

  Sets the parameters (Min, Max, Position, Ticks) of Carbon track bar
 ------------------------------------------------------------------------------}
procedure TCarbonTrackBar.ApplyChanges;
var
  TrackBar: TCustomTrackBar;
begin
  if FTicks <> GetTicks then
    RecreateWnd(LCLObject) // recreate track bar if ticks have changed
  else
  begin
    TrackBar := LCLObject as TCustomTrackBar;

    SetValue(TrackBar.Position);
    SetMinimum(TrackBar.Min);
    SetMaximum(TrackBar.Max);
  end;
end;

{ TCarbonScrollBar }

{------------------------------------------------------------------------------
  Method:  TCarbonScrollBar.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon scroll bar
 ------------------------------------------------------------------------------}
procedure TCarbonScrollBar.CreateWidget(const AParams: TCreateParams);
var
  ScrollBar: TCustomScrollBar;
  Control: ControlRef;
begin
  ScrollBar := LCLObject as TCustomScrollBar;

  if OSError(
    CreateScrollBarControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      ScrollBar.Position, ScrollBar.Min, ScrollBar.Max, ScrollBar.PageSize, True,
      nil, Control),
    Self, SCreateWidget, 'CreateScrollBarControl') then RaiseCreateWidgetError(LCLObject);

  Widget := Control;

  inherited;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonScrollBar.GetValidEvents
  Returns: Set of events with installed handlers
 ------------------------------------------------------------------------------}
class function TCarbonScrollBar.GetValidEvents: TCarbonControlEvents;
begin
  Result := inherited GetValidEvents + [cceDoAction];
end;

{------------------------------------------------------------------------------
  Method:  TCarbonScrollBar.IndicatorMoved

  Indicator moved event handler
 ------------------------------------------------------------------------------}
procedure TCarbonScrollBar.IndicatorMoved;
var
  ScrollMsg: TLMScroll;
begin
  FillChar(ScrollMsg, SizeOf(TLMScroll), 0);

  ScrollMsg.Msg := LM_HSCROLL;
  ScrollMsg.ScrollCode := SB_THUMBTRACK;
  ScrollMsg.Pos := GetControl32BitValue(ControlRef(Widget));
  ScrollMsg.ScrollBar := HWND(Widget);
  
  ValueChanged;
  DeliverMessage(LCLObject, ScrollMsg);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonScrollBar.DoAction
  Params:  AControlPart - Control part to perform the action

  Action event handler
 ------------------------------------------------------------------------------}
procedure TCarbonScrollBar.DoAction(AControlPart: ControlPartCode);
var
  ScrollMsg: TLMScroll;
  ScrollCode: SmallInt;
begin
  ScrollCode := -1; // valid scrollcode is >= 0

  case AControlPart of
    kControlUpButtonPart  : ScrollCode := SB_LINEUP;
    kControlDownButtonPart: ScrollCode := SB_LINEDOWN;
    kControlPageUpPart    : ScrollCode := SB_PAGEUP;
    kControlPageDownPart  : ScrollCode := SB_PAGEDOWN;
  end;

//DebugLn('TCarbonScrollBar.DoAction ' + IntToStr(Integer(AControlPart)) + ' ' +
//  IntToStr(ScrollCode));

  if ScrollCode >= 0 then
  begin
    FillChar(ScrollMsg, SizeOf(TLMScroll), 0);

    ScrollMsg.Msg := LM_HSCROLL;
    ScrollMsg.ScrollCode := ScrollCode;
    ScrollMsg.Pos := GetControl32BitValue(ControlRef(Widget));
    ScrollMsg.ScrollBar := HWND(Widget);

    DeliverMessage(LCLObject, ScrollMsg);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonScrollBar.SetParams

  Sets the parameters (Min, Max, Position, PageSize) of Carbon scroll bar
 ------------------------------------------------------------------------------}
procedure TCarbonScrollBar.SetParams;
var
  ScrollBar: TCustomScrollBar;
begin
  ScrollBar := LCLObject as TCustomScrollBar;

  SetMinimum(ScrollBar.Min);
  SetMaximum(ScrollBar.Max);
  SetValue(ScrollBar.Position);
  SetViewSize(ScrollBar.PageSize);
end;

end.

