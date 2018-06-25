{                 -------------------------------------
                  CarbonBars.pp  -  Carbon bars classes
                  -------------------------------------

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit CarbonBars;

{$mode objfpc}{$H+}
{$IFNDEF cpui386}
{$ERROR only cpu i386 is supported}
{$ENDIF}

interface

// defines
{$I carbondefines.inc}

uses
 // rtl+ftl
  Classes, SysUtils, Math,
 // carbon bindings
  MacOSAll, WSLCLClasses, CarbonPrivate,
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

    procedure SetColor(const {%H-}AColor: TColor); override;
    procedure SetFont(const {%H-}AFont: TFont); override;

    procedure SetIndetermine(AValue: Boolean);
    function GetIndetermine: Boolean;
  end;

  { TCarbonProgressBar }

  TCarbonProgressBar = class(TCarbonCustomBar)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    procedure BoundsChanged; override;
  public
    procedure ApplyChanges; virtual;
  end;

  { TCarbonMovableBar }

  TCarbonMovableBar = class(TCarbonCustomBar)
  public
    procedure IndicatorMoved; override;
    procedure ValueChanged; override;
    class function GetValidEvents: TCarbonControlEvents; override;
  end;

  { TCarbonTrackBar }

  TCarbonTrackBar = class(TCarbonMovableBar)
  private
    FTicks: Word;
    function GetTicks: Word;
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    procedure BoundsChanged; override;
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
    procedure BoundsChanged; override;
    function SetScrollInfo({%H-}SBStyle: Integer; const ScrollInfo: TScrollInfo): Integer; override;
    procedure GetScrollInfo({%H-}SBStyle: Integer; var ScrollInfo: TScrollInfo); override;
  end;

implementation

uses InterfaceBase, CarbonProc, CarbonDbgConsts;
  
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

procedure TCarbonCustomBar.SetIndetermine(AValue: Boolean);
begin
  SetControlData(Content, kControlEntireControl, kControlProgressBarIndeterminateTag, sizeof(AValue), @AValue);
end;

function TCarbonCustomBar.GetIndetermine: Boolean;
begin
  Result := false;
  GetControlData(Content, kControlEntireControl, kControlProgressBarIndeterminateTag, sizeof(Result), @Result, nil);
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
      ProgressBar.Position, ProgressBar.Min, ProgressBar.Max, False, Control{%H-}),
    Self, SCreateWidget, 'CreateProgressBarControl') then RaiseCreateWidgetError(LCLObject);

  Widget := Control;

  inherited;
end;

const
  // values are used from Interface Builder
  StdProgressBarNormalSize = 19;
  StdProgressBarSmallSize = 0; // 19
  StdProgressBarTinySize = 0; // not supported

procedure TCarbonProgressBar.BoundsChanged;
begin
  inherited BoundsChanged;
  SetControlViewStyle(Widget, StdProgressBarTinySize, StdProgressBarSmallSize, StdProgressBarNormalSize, LCLObject.Height > LCLObject.Width);
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
  SetIndetermine(ProgressBar.Style = pbstMarquee);
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
      kControlSliderPointsDownOrRight, FTicks, True, nil, Control{%H-}),
    Self, SCreateWidget, 'CreateSliderControl') then RaiseCreateWidgetError(LCLObject);

  Widget := Control;

  inherited;
end;

const
  // values are used from Interface Builder
  StdTrackBarNormalSize = 16;
  StdTrackBarSmallSize = 12;
  StdTrackBarTinySize = 0; //11

procedure TCarbonTrackBar.BoundsChanged;
begin
  inherited BoundsChanged;
  SetControlViewStyle(Widget, StdTrackBarTinySize, StdTrackBarSmallSize, StdTrackBarNormalSize,
    LCLObject.Width > LCLObject.Height);
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
      nil, Control{%H-}),
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
  FillChar(ScrollMsg{%H-}, SizeOf(TLMScroll), 0);

  ScrollMsg.Msg := LM_HSCROLL;
  ScrollMsg.ScrollCode := SB_THUMBTRACK;
  ScrollMsg.Pos := GetControl32BitValue(ControlRef(Widget));
  ScrollMsg.ScrollBar := {%H-}HWND(Widget);
  
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
    FillChar(ScrollMsg{%H-}, SizeOf(TLMScroll), 0);

    ScrollMsg.Msg := LM_HSCROLL;
    ScrollMsg.ScrollCode := ScrollCode;
    ScrollMsg.Pos := GetControl32BitValue(ControlRef(Widget));
    ScrollMsg.ScrollBar := {%H-}HWND(Widget);

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

const
  // values are used from Interface Builder
  StdScrollBarNormalSize = 15;
  StdScrollBarSmallSize = 0; // 11
  StdScrollBarTinySize = 0; // not supported

procedure TCarbonScrollBar.BoundsChanged;
begin
  inherited BoundsChanged;
  SetControlViewStyle(Widget, StdScrollBarTinySize, StdScrollBarSmallSize,
    StdScrollBarNormalSize, LCLObject.Width > LCLObject.Height);
end;

function TCarbonScrollBar.SetScrollInfo(SBStyle: Integer;
  const ScrollInfo: TScrollInfo): Integer;
begin
  if (SIF_RANGE and ScrollInfo.fMask) > 0 then
  begin
    SetMinimum(ScrollInfo.nMin);
    SetMaximum(ScrollInfo.nMax);
  end;

  if (SIF_POS and ScrollInfo.fMask) > 0 then
  begin
    SetValue(ScrollInfo.nPos);
  end;

  if (SIF_PAGE and ScrollInfo.fMask) > 0 then
  begin
    SetViewSize(ScrollInfo.nPage);
  end;

  Result := GetValue;
end;

procedure TCarbonScrollBar.GetScrollInfo(SBStyle: Integer; var ScrollInfo: TScrollInfo);  
begin
  ScrollInfo.fMask := SIF_RANGE or SIF_POS or SIF_PAGE;
  with ScrollInfo do 
  begin
    nMin := GetControl32BitMinimum(ControlRef(Widget));
    nMax := GetControl32BitMaximum(ControlRef(Widget));
    nPos := GetControl32BitValue(ControlRef(Widget));
    nPage := GetControlViewSize(ControlRef(Widget));  
  end;
end;

end.

