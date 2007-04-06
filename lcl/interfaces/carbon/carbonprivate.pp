{ $Id: $
                  --------------------------------------------
                  carbonprivate.pp  -  Carbon internal classes
                  --------------------------------------------

 This unit contains the private classhierarchy for the Carbon implemetations
 This hierarchy reflects (more or less) the Carbon widget hierarchy

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CarbonPrivate;

{$mode objfpc}{$H+}

interface

// debugging defines
{$I carbondebug.inc}

uses
 // rtl+ftl
  Types, Classes, SysUtils, Math, Contnrs,
 // carbon bindings
  FPCMacOSAll,
 // widgetset
  WSControls, WSLCLClasses, WSProc,
 // LCL Carbon
  CarbonUtils, CarbonDef,
 // LCL
  LMessages, LCLMessageGlue, LCLProc, LCLType, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Menus;
  
type
  TCarbonControlEvent = (cceValueChanged, cceIndicatorMoved,
    cceTextDidChange, cceDoAction, cceDraw, cceHit, cceListItemSelected);
  TCarbonControlEvents = set of TCarbonControlEvent;
  
  { TCarbonControl }
  
  TCarbonControl = class(TCarbonWidget)
  protected
    procedure RegisterEvents; override;
    procedure UnregisterEvents; override;
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
    function GetContent: ControlRef; override;
    function GetFrame: ControlRef; virtual;
  public
    class function GetValidEvents: TCarbonControlEvents; virtual;
    procedure Hit(AControlPart: ControlPartCode); dynamic;
    procedure Draw; virtual;
    procedure ValueChanged; dynamic;
    procedure IndicatorMoved; dynamic;
    procedure TextDidChange; dynamic;
    procedure DoAction(AControlPart: ControlPartCode); dynamic;
    procedure ListItemSelected(AIndex: Integer); dynamic;
  public
    function GetTopParentWindow: WindowRef; override;
    function GetThemeDrawState: ThemeDrawState;
    function GetMousePos: TPoint; override;
    function GetClientRect(var ARect: TRect): Boolean; override;
    function GetPreferredSize: TPoint; override;
    procedure Invalidate(Rect: PRect = nil); override;
    function IsEnabled: Boolean; override;
    function IsVisible: Boolean; override;
    function Enable(AEnable: Boolean): Boolean; override;
    
    function GetBounds(var ARect: TRect): Boolean; override;
    function GetScreenBounds(var ARect: TRect): Boolean; override;
    function SetBounds(const ARect: TRect): Boolean; override;

    procedure SetColor(const AColor: TColor); override;
    procedure SetFont(const AFont: TFont); override;
    procedure ShowHide(AVisible: Boolean); override;
    
    function GetText(var S: String): Boolean; override;
    function SetText(const S: String): Boolean; override;
    
    function Update: Boolean; override;
  public
  { Frame:
     = widget in controls without special frame control
     - frame area control of control
     - determines bounds of control
     - processes only bounds changed event            }
    property Frame: ControlRef read GetFrame;
  end;
  
  { TCarbonWindow }

  TCarbonWindow = class(TCarbonWidget)
  protected
    procedure RegisterEvents; override;
    procedure UnregisterEvents; override;
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
    function GetContent: ControlRef; override;
  public
    function GetMousePos: TPoint; override;
    function GetTopParentWindow: WindowRef; override;
    function GetClientRect(var ARect: TRect): Boolean; override;
    procedure Invalidate(Rect: PRect = nil); override;
    function IsEnabled: Boolean; override;
    function IsVisible: Boolean; override;
    function Enable(AEnable: Boolean): boolean; override;
    
    function GetBounds(var ARect: TRect): Boolean; override;
    function GetScreenBounds(var ARect: TRect): Boolean; override;
    function SetBounds(const ARect: TRect): Boolean; override;

    procedure SetColor(const AColor: TColor); override;
    procedure SetFont(const AFont: TFont); override;
    procedure ShowHide(AVisible: Boolean); override;
    
    function GetText(var S: String): Boolean; override;
    function SetText(const S: String): Boolean; override;
    
    function Update: Boolean; override;
  end;
  
  { TCarbonHintWindow }

  TCarbonHintWindow = class(TCarbonWindow)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  end;

  { TCarbonCustomControl }

  TCarbonCustomControl = class(TCarbonControl)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  end;
  
  { TCarbonGroupBox }

  TCarbonGroupBox = class(TCarbonControl)
  private
    FUserPane: ControlRef;
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
    function GetContent: ControlRef; override;
  end;
  
  { TCarbonStatusBar }

  TCarbonStatusBar = class(TCarbonControl)
  private
    FPanels: TObjectList;
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
  public
    class function GetValidEvents: TCarbonControlEvents; override;
    procedure Draw; override;
  public
    function GetPreferredSize: TPoint; override;
    procedure UpdatePanel(AIndex: Integer = -1);
  end;
  
  { TCarbonListBox }

  TCarbonListBox = class(TCarbonControl) // TODO
  private
    FItemIndex: Integer;
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    function GetItemsCount: Integer;
    function GetItemIndex: Integer;
    procedure SetItemIndex(AIndex: Integer);
  end;

  { TCarbonCustomCheckBox }

  TCarbonCustomCheckBox = class(TCarbonControl)
  public
    class function GetValidEvents: TCarbonControlEvents; override;
    procedure Hit(AControlPart: ControlPartCode); override;
    procedure ValueChanged; override;
  end;
  
  { TCarbonCheckBox }

  TCarbonCheckBox = class(TCarbonCustomCheckBox)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  end;
  
  { TCarbonToggleBox }

  TCarbonToggleBox = class(TCarbonCustomCheckBox)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  end;

  { TCarbonRadioButton }

  TCarbonRadioButton = class(TCarbonCustomCheckBox)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    procedure ValueChanged; override;
  end;
  
  { TCarbonCustomButton }

  TCarbonCustomButton = class(TCarbonControl)
  public
    class function GetValidEvents: TCarbonControlEvents; override;
    procedure Hit(AControlPart: ControlPartCode); override;
  end;
  
  { TCarbonButton }

  TCarbonButton = class(TCarbonCustomButton)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  end;
  
  { TCarbonBitBtn }

  TCarbonBitBtn = class(TCarbonCustomButton)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  end;
  
  { TCarbonStaticText }

  TCarbonStaticText = class(TCarbonControl)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  end;
  
  { TCarbonCustomBar }

  TCarbonCustomBar = class(TCarbonControl)
  public
    procedure SetData(APos: Integer);
    procedure SetData(APos, AMin, AMax: Integer);
    procedure SetData(APos, AMin, AMax, APage: Integer);
    function GetPos: Integer;
  end;
  
  { TCarbonProgressBar }

  TCarbonProgressBar = class(TCarbonCustomBar)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  end;

  { TCarbonTrackBar }

  TCarbonTrackBar = class(TCarbonCustomBar)
  private
    FTicks: LongWord;
  public
    class function GetValidEvents: TCarbonControlEvents; override;
    procedure ValueChanged; override;
    procedure IndicatorMoved; override;
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    function GetTicks: LongWord;
    property Ticks: LongWord read FTicks;
  end;

  { TCarbonScrollBar }

  TCarbonScrollBar = class(TCarbonCustomBar)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    class function GetValidEvents: TCarbonControlEvents; override;
    procedure ValueChanged; override;
    procedure IndicatorMoved; override;
    procedure DoAction(AControlPart: ControlPartCode); override;
  end;
  
procedure RaiseCreateWidgetError(AControl: TWinControl);

implementation

uses InterfaceBase, CarbonProc, CarbonWSStdCtrls, CarbonStrings,
  CarbonGDIObjects;

procedure RaiseCreateWidgetError(AControl: TWinControl);
begin
  raise Exception.CreateFmt('Unable to create Carbon widget for %s: %s!',
    [AControl.Name, AControl.ClassName]);
end;

// Store state of key modifiers so that we can emulate keyup/keydown
// of keys like control, option, command, caps lock, shift
var PrevKeyModifiers: UInt32 = 0;

// Stores mouse up message to be fired on control hit after value is updated
var SavedMouseUpMsg: TLMMouse;

{$I mackeycodes.inc}

{$I carbonprivatecommon.inc}
{$I carbonprivatecontrol.inc}
{$I carbonprivatewindow.inc}

{ TCarbonHintWindow }

procedure TCarbonHintWindow.CreateWidget(const AParams: TCreateParams);
var
  Window: WindowRef;
begin
  if CreateNewWindow(kHelpWindowClass,
    kWindowCompositingAttribute or
    kWindowHideOnSuspendAttribute or kWindowStandardHandlerAttribute,
    ParamsToCarbonRect(AParams), Window) = noErr then
  begin
    Widget := Window;

    SetWindowProperty(Widget, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Self), @Self);
    SetControlProperty(Content, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Self), @Self);
  end
  else RaiseCreateWidgetError(LCLObject);
  
  SetColor(LCLObject.Color);
end;

{ TCarbonCustomControl }

procedure TCarbonCustomControl.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  Attrs: LongWord;
begin
  Attrs := kControlSupportsEmbedding or kControlSupportsFocus or
    kControlWantsActivate or kControlHandlesTracking or
    kControlHasSpecialBackground or kControlGetsFocusOnClick or
    kControlSupportsSetCursor or kControlSupportsContextualMenus or
    kControlSupportsClickActivation;
  
  if CreateUserPaneControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
    Attrs, Control) = noErr then
  begin
    Widget := Control;
    
    inherited;
  end
  else RaiseCreateWidgetError(LCLObject);
end;

{ TCarbonGroupBox }

procedure TCarbonGroupBox.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  CFString: CFStringRef;
  R: TRect;
begin
  CreateCFString(AParams.Caption, CFString);
  try
    if CreateGroupBoxControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      CFString, not (LCLObject.Parent is TCustomGroupBox), Control) = noErr then
    begin
      Widget := Control;
      
      if not GetClientRect(R) then
      begin
        DebugLn('TCarbonGroupBox.CreateWidget Error - no content region!');
        Exit;
      end;
      
      if CreateUserPaneControl(GetTopParentWindow, GetCarbonRect(R),
        kControlSupportsEmbedding or kControlHandlesTracking, FUserPane) <> noErr then
      begin
        DebugLn('TCarbonGroupBox.CreateWidget Error - unable to create content control!');
        Exit;
      end;
      
      if HIViewAddSubview(Control, FUserPane) <> noErr then
      begin
        DebugLn('TCarbonGroupBox.CreateWidget Error - unable to embed conent control!');
        Exit;
      end;
      
      inherited;
    end
    else RaiseCreateWidgetError(LCLObject);
  finally
    FreeCFString(CFString);
  end;
end;

procedure TCarbonGroupBox.DestroyWidget;
begin
  DisposeControl(FUserPane);

  inherited DestroyWidget;
end;

function TCarbonGroupBox.GetContent: ControlRef;
begin
  Result := FUserPane;
end;

{ TCarbonStatusBar }

procedure TCarbonStatusBar.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
begin
  if not OSError(CreatePlacardControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      Control),
    Self, 'CreateWidget', 'CreatePlacardControl') then
  begin
    Widget := Control;

    inherited;
  end
  else RaiseCreateWidgetError(LCLObject);
  
  FPanels := TObjectList.Create(True);
end;

procedure TCarbonStatusBar.DestroyWidget;
begin
  FPanels.Free;

  inherited DestroyWidget;
end;

class function TCarbonStatusBar.GetValidEvents: TCarbonControlEvents;
begin
  Result := [cceDraw];
end;

procedure TCarbonStatusBar.Draw;
var
  StatusBar: TStatusBar;
  R: TRect;
begin
  StatusBar := LCLObject as TStatusBar;
  
  if StatusBar.SimplePanel and (StatusBar.SimpleText <> '') then
  begin
    GetClientRect(R);
    
    WidgetSet.DrawText(HDC(Context), PChar(StatusBar.SimpleText),
      Length(StatusBar.SimpleText), R, 0);
  end;
end;

function TCarbonStatusBar.GetPreferredSize: TPoint;
begin
  Result := inherited GetPreferredSize;
  
  // stretch status bar to whole window width
  if LCLObject.Parent <> nil then
    Result.X := LCLObject.Parent.ClientWidth;
  
  Result.Y := 20;
end;

procedure TCarbonStatusBar.UpdatePanel(AIndex: Integer);
var
  StatusBar: TStatusBar;
  I, X: Integer;
  Panel: TPanel;
begin
  StatusBar := LCLObject as TStatusBar;
  
  if StatusBar.SimplePanel then
  begin
    // hide panels
    for I := 0 to FPanels.Count - 1 do (FPanels[I] as TPanel).Hide;

    Invalidate; // update text
  end
  else
  begin
    X := 0;
    
    for I := 0 to StatusBar.Panels.Count - 1 do
    begin
      if I >= FPanels.Count then // create new panel
      begin
        Panel := TPanel.Create(LCLObject);
        Panel.Visible := False;
        Panel.Anchors := [akLeft, akTop, akBottom];
        Panel.Height := LCLObject.Height;
        Panel.Parent := LCLObject;
        
        FPanels.Add(Panel);
      end
      else Panel := FPanels[I] as TPanel;
      
      if I >= AIndex then // reposition panel
      begin
        Panel.Hide;
        Panel.Left := X;
        
        if (I = AIndex) or (AIndex = -1) then // update panel attrs
        begin
          Panel.Width := StatusBar.Panels[I].Width;
          Panel.Caption := StatusBar.Panels[I].Text;
          Panel.Alignment := StatusBar.Panels[I].Alignment;
          Panel.BevelOuter := TPanelBevel(StatusBar.Panels[I].Bevel);
        end;
      end;
      Panel.Show;
      
      Inc(X, Panel.Width);
    end;
    
    // delete unneeded panels
    for I := FPanels.Count - 1 downto StatusBar.Panels.Count do
      FPanels.Delete(I);
  end;
end;

{ TCarbonListBox }

procedure TCarbonListBox.CreateWidget(const AParams: TCreateParams);
begin
  // TODO

  Widget := nil;
  inherited;

  FItemIndex := -1;
end;

function TCarbonListBox.GetItemsCount: Integer;
begin
  Result := 0;
  // TODO
end;

function TCarbonListBox.GetItemIndex: Integer;
begin
  Result := FItemIndex;
end;

procedure TCarbonListBox.SetItemIndex(AIndex: Integer);
begin
  // TODO
end;

{ TCarbonCustomCheckBox }

class function TCarbonCustomCheckBox.GetValidEvents: TCarbonControlEvents;
begin
  Result := [cceValueChanged, cceHit];
end;

procedure TCarbonCustomCheckBox.Hit(AControlPart: ControlPartCode);
begin
  // do nothing, because value changed will be fired immediately
end;

procedure TCarbonCustomCheckBox.ValueChanged;
begin
  LCLSendChangedMsg(LCLObject);
end;

{ TCarbonCheckBox }

procedure TCarbonCheckBox.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  CFString: CFStringRef;
  Value: UInt32;
begin
  case (LCLObject as TCustomCheckBox).State of
    cbChecked  : Value := kControlCheckBoxCheckedValue;
    cbUnChecked: Value := kControlCheckBoxUncheckedValue;
    cbGrayed   : Value := kControlCheckBoxMixedValue;
  end;

  CreateCFString(AParams.Caption, CFString);
  try
    if CreateCheckBoxControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      CFString, Value, True, Control) = noErr then
    begin
      Widget := Control;

      inherited;
    end
    else RaisecreateWidgetError(LCLObject);
  finally
    FreeCFString(CFString);
  end;
end;

{ TCarbonToggleBox }

procedure TCarbonToggleBox.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  CFString: CFStringRef;
  Value: UInt32;
begin
  case (LCLObject as TToggleBox).State of
    cbChecked  : Value := kControlCheckBoxCheckedValue;
    cbUnChecked: Value := kControlCheckBoxUncheckedValue;
    cbGrayed   : Value := kControlCheckBoxMixedValue;
  end;

  CreateCFString(AParams.Caption, CFString);
  try
    if CreateBevelButtonControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      CFString, kControlBevelButtonNormalBevel,
      kControlBehaviorToggles, nil, 0, 0, 0, Control) = noErr then
    begin
      Widget := Control;
      
      inherited;
    end
    else RaisecreateWidgetError(LCLObject);
  finally
    FreeCFString(CFString);
  end;
  SetControl32BitValue(Control, Value);
end;

{ TCarbonRadioButton }

procedure TCarbonRadioButton.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  CFString: CFStringRef;
  Value: UInt32;
begin
  case (LCLObject as TRadioButton).State of
    cbChecked  : Value := kControlCheckBoxCheckedValue;
    cbUnChecked: Value := kControlCheckBoxUncheckedValue;
    cbGrayed   : Value := kControlCheckBoxMixedValue;
  end;

  CreateCFString(AParams.Caption, CFString);
  try
    if CreateRadioButtonControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      CFString, Value, True, Control) = noErr then
    begin
      Widget := Control;
      
      inherited;
    end
    else RaisecreateWidgetError(LCLObject);
  finally
    FreeCFString(CFString);
  end;
end;

procedure TCarbonRadioButton.ValueChanged;
var
  RadioButton: TRadioButton;
  Sibling: TControl;
  I: Integer;
begin
  if GetControl32BitValue(ControlRef(Widget)) = kControlCheckBoxCheckedValue then
  begin
    DebugLn('TCarbonRadioButton.ValueChanged Uncheck Sibling');

    // uncheck sibling radio buttons
    RadioButton := (LCLObject as TRadioButton);
    if RadioButton.Parent <> nil then
    begin
      for I := 0 to RadioButton.Parent.ControlCount - 1 do
      begin
        Sibling := RadioButton.Parent.Controls[I];
        if (Sibling is TRadioButton) and (Sibling <> RadioButton) then
          (Sibling as TRadioButton).Checked := False;
      end;
    end;
  end;

  inherited;
end;

{ TCarbonButton }

class function TCarbonCustomButton.GetValidEvents: TCarbonControlEvents;
begin
  Result := [cceHit];
end;

procedure TCarbonCustomButton.Hit(AControlPart: ControlPartCode);
begin
  LCLSendClickedMsg(LCLObject);
end;

{ TCarbonButton }

procedure TCarbonButton.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  CFString: CFStringRef;
begin
  // create the button at bounds with title
  CreateCFString(AParams.Caption, CFString);
  try
    if CreatePushButtonControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      CFString, Control) = noErr then
    begin
      Widget := Control;

      inherited;
    end
    else RaisecreateWidgetError(LCLObject);
  finally
    FreeCFString(CFString);
  end;
end;

{ TCarbonBitBtn }

procedure TCarbonBitBtn.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  CFString: CFStringRef;
begin
  CreateCFString(AParams.Caption, CFString);
  try
    if CreateBevelButtonControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      CFString, kControlBevelButtonNormalBevel, kControlBehaviorPushbutton,
      nil, 0, 0, 0, Control) = noErr then
    begin
      Widget := Control;
      
      inherited;
    end
    else RaisecreateWidgetError(LCLObject);
  finally
    FreeCFString(CFString);
  end;
end;


{ TCarbonStaticText }

procedure TCarbonStaticText.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  CFString: CFStringRef;
  MultiLine: Boolean = True;
  FontStyle: ControlFontStyleRec;
begin
  FontStyle.flags := kControlUseJustMask;
  case (LCLObject as TCustomStaticText).Alignment of
  taLeftJustify:  FontStyle.just := teFlushLeft;
  taRightJustify: FontStyle.just := teFlushRight;
  taCenter:       FontStyle.just := teCenter;
  end;

  CreateCFString(AParams.Caption, CFString);
  try
    if CreateStaticTextControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      CFString, @FontStyle, Control) = noErr then
    begin
      Widget := Control;
      
      inherited;
    end
    else RaisecreateWidgetError(LCLObject);
  finally
    FreeCFString(CFString);
  end;
  
  // switch on multi-line attribute
  SetControlData(Control, kControlEntireControl,
    kControlStaticTextIsMultilineTag, SizeOf(Boolean), @MultiLine);
end;

{ TCarbonCustomBar }

procedure TCarbonCustomBar.SetData(APos: Integer);
begin
  SetControl32BitValue(ControlRef(Widget), APos);
end;

procedure TCarbonCustomBar.SetData(APos, AMin, AMax: Integer);
begin
  SetControl32BitMinimum(ControlRef(Widget), AMin);
  SetControl32BitMaximum(ControlRef(Widget), AMax);
  SetControl32BitValue(ControlRef(Widget), APos);
end;

procedure TCarbonCustomBar.SetData(APos, AMin, AMax, APage: Integer);
begin
  SetControl32BitMinimum(ControlRef(Widget), AMin);
  SetControl32BitMaximum(ControlRef(Widget), AMax);
  SetControl32BitValue(ControlRef(Widget), APos);
  SetControlViewSize(ControlRef(Widget), APage);
end;

function TCarbonCustomBar.GetPos: Integer;
begin
  Result := GetControl32BitValue(ControlRef(Widget));
end;

{ TCarbonProgressBar }

procedure TCarbonProgressBar.CreateWidget(const AParams: TCreateParams);
var
  ProgressBar: TCustomProgressBar;
  Control: ControlRef;
begin
  ProgressBar := LCLObject as TCustomProgressBar;

  // create determinate progress bar
  if CreateProgressBarControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
    ProgressBar.Position, ProgressBar.Min, ProgressBar.Max, False,
    Control) = noErr then
  begin
    Widget := Control;
    
    inherited;
  end
  else RaiseCreateWidgetError(LCLObject);
end;

{ TCarbonTrackBar }

{------------------------------------------------------------------------------
  Method:  TCarbonTrackBar.GetTicks
  Returns: Number of ticks

  Returns the number of ticks for the track bar
 ------------------------------------------------------------------------------}
function TCarbonTrackBar.GetTicks: LongWord;
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

class function TCarbonTrackBar.GetValidEvents: TCarbonControlEvents;
begin
  Result := [cceValueChanged, cceIndicatorMoved];
end;

procedure TCarbonTrackBar.ValueChanged;
begin
  LCLSendChangedMsg(LCLObject);
end;

procedure TCarbonTrackBar.IndicatorMoved;
begin
  ValueChanged;
end;

procedure TCarbonTrackBar.CreateWidget(const AParams: TCreateParams);
var
  TrackBar: TCustomTrackBar;
  Control: ControlRef;
begin
  TrackBar := LCLObject as TCustomTrackBar;
  
  FTicks := GetTicks;

  if CreateSliderControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
    TrackBar.Position, TrackBar.Min, TrackBar.Max,
    kControlSliderPointsDownOrRight, FTicks, True, nil, Control) = noErr then
  begin
    Widget := Control;
    
    inherited;
  end
  else RaiseCreateWidgetError(LCLObject);
end;

{ TCarbonScrollBar }

procedure TCarbonScrollBar.CreateWidget(const AParams: TCreateParams);
var
  ScrollBar: TCustomScrollBar;
  Control: ControlRef;
begin
  ScrollBar := LCLObject as TCustomScrollBar;

  if CreateScrollBarControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
    ScrollBar.Position, ScrollBar.Min, ScrollBar.Max, ScrollBar.PageSize, True,
    nil, Control) = noErr then
  begin
    Widget := Control;
    
    inherited;
  end
  else RaiseCreateWidgetError(LCLObject);
end;

class function TCarbonScrollBar.GetValidEvents: TCarbonControlEvents;
begin
  Result := [cceValueChanged, cceIndicatorMoved, cceDoAction];
end;

procedure TCarbonScrollBar.ValueChanged;
var
  ScrollMsg: TLMScroll;
begin
  FillChar(ScrollMsg, SizeOf(TLMScroll), 0);

  ScrollMsg.Msg := LM_HSCROLL;
  ScrollMsg.ScrollCode := SB_THUMBTRACK;
  ScrollMsg.Pos := GetControl32BitValue(ControlRef(Widget));
  ScrollMsg.ScrollBar := HWND(Widget);

  DeliverMessage(LCLObject, ScrollMsg);
end;

procedure TCarbonScrollBar.IndicatorMoved;
begin
  ValueChanged;
end;

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



end.

