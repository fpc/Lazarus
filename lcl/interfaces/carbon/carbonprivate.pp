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
  CarbonDef,
 // LCL
  LMessages, LCLMessageGlue, LCLProc, LCLType, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls, Menus;
  
type
  TCarbonControlEvent = (cceValueChanged, cceIndicatorMoved, cceDoAction,
    cceDraw, cceHit);
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
    function GetForceEmbedInScrollView: Boolean; virtual;
    function EmbedInScrollView(const AParams: TCreateParams): HIViewRef;
    function EmbedInScrollView(AScrollBars: TScrollStyle): HIViewRef;
    procedure ChangeScrollBars(AScrollView: HIViewRef; var AScrollBars: TScrollStyle; ANewValue: TScrollStyle);
  public
    class function GetValidEvents: TCarbonControlEvents; virtual;
    procedure Hit(AControlPart: ControlPartCode); dynamic;
    procedure Draw; virtual;
    procedure ValueChanged; dynamic;
    procedure IndicatorMoved; dynamic;
    procedure DoAction(AControlPart: ControlPartCode); dynamic;
  public
    procedure AddToWidget(AParent: TCarbonWidget); override;
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
    procedure SetChildZPosition(AChild: TCarbonWidget; const AOldPos, ANewPos: Integer; const AChildren: TFPList); override;

    procedure SetFocus; override;
    procedure SetColor(const AColor: TColor); override;
    procedure SetFont(const AFont: TFont); override;
    procedure ShowHide(AVisible: Boolean); override;
    
    function GetText(var S: String): Boolean; override;
    function SetText(const S: String): Boolean; override;
    
    function Update: Boolean; override;
  public
    function GetValue: Integer;
    procedure SetValue(AValue: Integer);
    procedure SetMinimum(AValue: Integer);
    procedure SetMaximum(AValue: Integer);
    procedure SetViewSize(AValue: Integer);
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
  private
    FBorderStyle: TFormBorderStyle;
  protected
    procedure RegisterEvents; override;
    procedure UnregisterEvents; override;
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
    function GetContent: ControlRef; override;
  public
    procedure AddToWidget(AParent: TCarbonWidget); override;
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
    procedure SetChildZPosition(AChild: TCarbonWidget; const AOldPos, ANewPos: Integer; const AChildren: TFPList); override;

    procedure SetFocus; override;
    procedure SetColor(const AColor: TColor); override;
    procedure SetFont(const AFont: TFont); override;
    procedure ShowHide(AVisible: Boolean); override;
    
    function GetText(var S: String): Boolean; override;
    function SetText(const S: String): Boolean; override;
    
    function Update: Boolean; override;
  public
    function Activate: Boolean; virtual;
    
    procedure CloseModal; virtual;
    procedure ShowModal; virtual;
    
    function Show(AShow: Integer): Boolean; virtual;

    procedure SetBorderIcons(ABorderIcons: TBorderIcons); virtual;
    procedure SetFormBorderStyle(AFormBorderStyle: TFormBorderStyle); virtual;
  end;
  
  { TCarbonHintWindow }

  TCarbonHintWindow = class(TCarbonWindow)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  end;

  { TCarbonCustomControl }

  TCarbonCustomControl = class(TCarbonControl)
  private
    FScrollView: HIViewRef;
    FScrollOrigin: TPoint;
    FScrollSize: TPoint;
    FScrollPageSize: TPoint;
  protected
    procedure RegisterEvents; override;
    procedure UnregisterEvents; override;
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
    function GetFrame: ControlRef; override;
  public
    procedure GetInfo(out AImageSize, AViewSize, ALineSize, AOrigin: TPoint); dynamic;
    procedure ScrollTo(const ANewOrigin: TPoint); dynamic;
  public
    procedure SetColor(const AColor: TColor); override;
    procedure SetFont(const AFont: TFont); override;
    function SetScrollInfo(SBStyle: Integer; const ScrollInfo: TScrollInfo): Integer;
    procedure GetScrollInfo(SBStyle: Integer; var ScrollInfo: TScrollInfo);
  end;
  
  { TCarbonScrollingWinControl }

  TCarbonScrollingWinControl = class(TCarbonCustomControl)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
    function GetForceEmbedInScrollView: Boolean; override;
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
    
    function RetrieveState: TCheckBoxState; virtual;
    procedure SetState(AState: TCheckBoxState); virtual;
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
  public
    procedure SetDefault(ADefault: Boolean); virtual;
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
  public
    procedure SetGlyph(const AGlyph: TBitmap); virtual;
    procedure SetLayout(ALayout: TButtonLayout); virtual;
  end;
  
  { TCarbonStaticText }

  TCarbonStaticText = class(TCarbonControl)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    procedure SetAlignment(AAlignment: TAlignment); virtual;
  end;
  
  { TCarbonCustomBar }
  
  TCarbonCustomBar = class(TCarbonControl)
  public
    function GetPosition: Integer; virtual;
    procedure SetPosition(APosition: Integer); virtual;
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
    procedure ValueChanged; override;
    procedure DoAction(AControlPart: ControlPartCode); override;
    procedure SetParams; virtual;
  end;
  
procedure RaiseCreateWidgetError(AControl: TWinControl);

function GetCarbonWidget(AWidget: Pointer): TCarbonWidget;
function GetCarbonWindow(AWidget: WindowRef): TCarbonWindow;
function GetCarbonControl(AWidget: ControlRef): TCarbonControl;

implementation

uses InterfaceBase, CarbonInt, CarbonProc, CarbonDbgConsts, CarbonUtils,
  CarbonWSStdCtrls, CarbonStrings, CarbonCanvas, CarbonGDIObjects;

{------------------------------------------------------------------------------
  Name:    RaiseCreateWidgetError
  Params:  AControl - Which control was being created
  
  Raises exception for widget creation error
 ------------------------------------------------------------------------------}
procedure RaiseCreateWidgetError(AControl: TWinControl);
begin
  raise Exception.CreateFmt('Unable to create Carbon widget for %s: %s!',
    [AControl.Name, AControl.ClassName]);
end;

{------------------------------------------------------------------------------
  Name:    GetCarbonWidget
  Params:  AWidget - Pointer to control or window widget
  Returns: The Carbon widget

  Retrieves widget for specified Carbon control or window
 ------------------------------------------------------------------------------}
function GetCarbonWidget(AWidget: Pointer): TCarbonWidget;
begin
  if AWidget = nil then
  begin
    Result := nil;
    Exit;
  end;

  if IsValidControlHandle(AWidget) then
    Result := GetCarbonControl(ControlRef(AWidget))
  else
    // there is no (cheap) check for windows so assume a window
    // when it is not a control.
    Result := GetCarbonWindow(WindowRef(AWidget));
end;

{------------------------------------------------------------------------------
  Name:    GetCarbonWindow
  Params:  AWidget - Pointer to window widget
  Returns: The Carbon window

  Retrieves the Carbon window for specified window widget
 ------------------------------------------------------------------------------}
function GetCarbonWindow(AWidget: WindowRef): TCarbonWindow;
begin
  if GetWindowProperty(AWidget, LAZARUS_FOURCC, WIDGETINFO_FOURCC,
    SizeOf(TCarbonWidget), nil, @Result) <> noErr then Result := nil;
end;

{------------------------------------------------------------------------------
  Name:    GetCarbonControl
  Params:  AWidget - Pointer to control widget
  Returns: The Carbon control

  Retrieves the Carbon control for specified control widget
 ------------------------------------------------------------------------------}
function GetCarbonControl(AWidget: ControlRef): TCarbonControl;
begin
  if GetControlProperty(AWidget, LAZARUS_FOURCC, WIDGETINFO_FOURCC,
    SizeOf(TCarbonWidget), nil, @Result) <> noErr then Result := nil;
end;

// Store state of key modifiers so that we can emulate keyup/keydown
// of keys like control, option, command, caps lock, shift
var PrevKeyModifiers: UInt32 = 0;

// Stores mouse up message to be fired on control hit after value is updated
var SavedMouseUpMsg: TLMMouse;

{$I carbonprivatecommon.inc}
{$I carbonprivatecontrol.inc}
{$I carbonprivatewindow.inc}

{ TCarbonHintWindow }

{------------------------------------------------------------------------------
  Method:  TCarbonHintWindow.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon hint window
 ------------------------------------------------------------------------------}
procedure TCarbonHintWindow.CreateWidget(const AParams: TCreateParams);
var
  Window: WindowRef;
begin
  if OSError(
    CreateNewWindow(kHelpWindowClass,
      kWindowCompositingAttribute or
      kWindowHideOnSuspendAttribute or kWindowStandardHandlerAttribute,
      ParamsToCarbonRect(AParams), Window),
    Self, SCreateWidget, 'CreateNewWindow') then RaiseCreateWidgetError(LCLObject);
      

  Widget := Window;

  OSError(
    SetWindowProperty(Widget, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Self), @Self),
    Self, SCreateWidget, 'SetWindowProperty');
  OSError(
    SetControlProperty(Content, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Self), @Self),
    Self, SCreateWidget, SSetControlProp);
  
  SetColor(LCLObject.Color);
end;

{ TCarbonCustomControl }

{------------------------------------------------------------------------------
  Name: CarbonScrollable_GetInfo
  Handles scrollable get info
 ------------------------------------------------------------------------------}
function CarbonScrollable_GetInfo(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  ImageSize, ViewSize, LineSize, Origin: TPoint;
  ImageHISize, ViewHISize, LineHISize: HISize;
  HIOrigin: HIPoint;
const
  SName = 'CarbonScrollable_GetInfo';
begin
  {$IFDEF VerboseControlEvent}
    DebugLn('CarbonScrollable_GetInfo ', DbgSName(AWidget.LCLObject));
  {$ENDIF}

  (AWidget as TCarbonCustomControl).GetInfo(ImageSize, ViewSize, LineSize, Origin);
  
  ImageHISize := PointToHISize(ImageSize);
  ViewHISize := PointToHISize(ViewSize);
  LineHISize := PointToHISize(LineSize);
  HIOrigin := PointToHIPoint(Origin);
  
  OSError(SetEventParameter(AEvent, kEventParamImageSize, typeHISize,
    SizeOf(HISize), @ImageHISize), SName, SSetEvent, 'kEventParamImageSize');
  OSError(SetEventParameter(AEvent, kEventParamViewSize, typeHISize,
    SizeOf(HISize), @ViewHISize), SName, SSetEvent, 'kEventParamViewSize');
  OSError(SetEventParameter(AEvent, kEventParamLineSize, typeHISize,
    SizeOf(HISize), @LineHISize), SName, SSetEvent, 'kEventParamLineSize');
  OSError(SetEventParameter(AEvent, kEventParamOrigin, typeHISize,
    SizeOf(HIPoint), @HIOrigin), SName, SSetEvent, 'kEventParamOrigin');
  
  Result := noErr;
end;

{------------------------------------------------------------------------------
  Name: CarbonScrollable_ScrollTo
  Handles scrollable get info
 ------------------------------------------------------------------------------}
function CarbonScrollable_ScrollTo(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  Origin: HIPoint;
begin
  {$IFDEF VerboseControlEvent}
    DebugLn('CarbonScrollable_ScrollTo ', DbgSName(AWidget.LCLObject));
  {$ENDIF}
  
  if OSError(
    GetEventParameter(AEvent, kEventParamOrigin, typeHIPoint, nil,
      SizeOf(HIPoint), nil, @Origin), 'CarbonScrollable_ScrollTo', SGetEvent,
    'kEventParamOrigin') then Exit;
  
  (AWidget as TCarbonCustomControl).ScrollTo(HIPointToPoint(Origin));
  
  Result := noErr;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomControl.RegisterEvents

  Registers event handlers for custom control
 ------------------------------------------------------------------------------}
procedure TCarbonCustomControl.RegisterEvents;
var
  TmpSpec: EventTypeSpec;
begin
  inherited RegisterEvents;
  
  if FScrollView <> Widget then
  begin
    TmpSpec := MakeEventSpec(kEventClassScrollable, kEventScrollableGetInfo);
    InstallControlEventHandler(Widget,
      RegisterEventHandler(@CarbonScrollable_GetInfo),
      1, @TmpSpec, Pointer(Self), nil);

    TmpSpec := MakeEventSpec(kEventClassScrollable, kEventScrollableScrollTo);
    InstallControlEventHandler(Widget,
      RegisterEventHandler(@CarbonScrollable_ScrollTo),
      1, @TmpSpec, Pointer(Self), nil);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomControl.UnregisterEvents

  Unregisters event handlers
 ------------------------------------------------------------------------------}
procedure TCarbonCustomControl.UnregisterEvents;
begin
  if FScrollView <> Widget then
  begin
    UnregisterEventHandler(@CarbonScrollable_GetInfo);
    UnregisterEventHandler(@CarbonScrollable_ScrollTo);
  end;
  
  inherited UnregisterEvents;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomControl.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon custom control
 ------------------------------------------------------------------------------}
procedure TCarbonCustomControl.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  Attrs: LongWord;
begin
  Attrs := kControlSupportsEmbedding or kControlSupportsFocus or
    kControlWantsActivate or kControlHandlesTracking or
    kControlGetsFocusOnClick or kControlSupportsSetCursor or
    kControlSupportsContextualMenus or kControlSupportsClickActivation;
  
  if OSError(
    CreateUserPaneControl(GetTopParentWindow, ParamsToCarbonRect(AParams), Attrs, Control),
    Self, SCreateWidget, 'CreateUserPaneControl') then RaiseCreateWidgetError(LCLObject);

  Widget := Control;
  
  FScrollView := EmbedInScrollView(AParams);
  FScrollSize := Classes.Point(0, 0);
  FScrollPageSize := Classes.Point(0, 0);
  FScrollOrigin := Classes.Point(0, 0);
    
  inherited;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomControl.DestroyWidget

  Clean-up
 ------------------------------------------------------------------------------}
procedure TCarbonCustomControl.DestroyWidget;
begin
  if (FScrollView <> Widget) and (FScrollView <> nil) then DisposeControl(FScrollView);
  
  inherited DestroyWidget;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomControl.GetFrame
  Returns: Frame area control
 ------------------------------------------------------------------------------}
function TCarbonCustomControl.GetFrame: ControlRef;
begin
  Result := FScrollView;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomControl.GetInfo
  Params:  AImageSize - Size of entire scrollable area
           AViewSize  - Size of scrollable page
           ALineSize  - Size of scrollable line
           AOrigin    - Scroll position

  Handles scrollableget info event
 ------------------------------------------------------------------------------}
procedure TCarbonCustomControl.GetInfo(out AImageSize, AViewSize,
  ALineSize, AOrigin: TPoint);
begin
  AOrigin := FScrollOrigin;
  AImageSize := FScrollSize;
  AViewSize := FScrollPageSize;
  ALineSize := Classes.Point(1, 1);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomControlScrollTo
  Params:  ANewOrigin - New scroll position

  Handles scrollable scroll to event
 ------------------------------------------------------------------------------}
procedure TCarbonCustomControl.ScrollTo(const ANewOrigin: TPoint);
var
  ScrollMsg: TLMScroll;
begin
  FScrollOrigin := ANewOrigin;
  
  // send vertical scroll
  FillChar(ScrollMsg, SizeOf(TLMScroll), 0);
  with ScrollMsg do
  begin
    Msg := LM_VSCROLL;
    Pos := ANewOrigin.Y;
    ScrollCode := SB_THUMBPOSITION;
  end;
  DeliverMessage(LCLObject, ScrollMsg);
  
  // send horizontal scroll
  FillChar(ScrollMsg, SizeOf(TLMScroll), 0);
  with ScrollMsg do
  begin
    Msg := LM_HSCROLL;
    Pos := ANewOrigin.X;
    ScrollCode := SB_THUMBPOSITION;
  end;
  DeliverMessage(LCLObject, ScrollMsg);
  
  OSError(
    HiViewSetNeedsDisplay(Widget, True), Self, 'ScrollTo', SViewNeedsDisplay);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomControl.SetColor
  Params:  AColor - New color

  Sets the color of control (for edit like controls)
 ------------------------------------------------------------------------------}
procedure TCarbonCustomControl.SetColor(const AColor: TColor);
begin
  // not supported
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomControl.SetFont
  Params:  AFont - New font

  Sets the font of control
 ------------------------------------------------------------------------------}
procedure TCarbonCustomControl.SetFont(const AFont: TFont);
begin
  // not supported
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomControl.SetScrollInfo
  Params:  SBStyle    - Scrollbar type (SB_VERT, SB_HORZ)
           ScrollInfo - Scrolling info
  Returns: The old scroll bar position

  Sets the scrolling info of the specified scroll bar
 ------------------------------------------------------------------------------}
function TCarbonCustomControl.SetScrollInfo(SBStyle: Integer;
  const ScrollInfo: TScrollInfo): Integer;
var
  Event: EventRef;
const
  SName = 'SetScrollInfo';
begin
  if SBStyle = SB_HORZ then
    Result := FScrollOrigin.X;
  if SBStyle = SB_VERT then
    Result := FScrollOrigin.Y;

  if (SIF_RANGE and ScrollInfo.fMask) > 0 then
  begin
    if SBStyle = SB_HORZ then
      FScrollSize.X := (ScrollInfo.nMax - ScrollInfo.nMin);
    if SBStyle = SB_VERT then
      FScrollSize.Y := (ScrollInfo.nMax - ScrollInfo.nMin);
  end;
  
  if (SIF_POS and ScrollInfo.fMask) > 0 then
  begin
    if SBStyle = SB_HORZ then
      FScrollOrigin.X := ScrollInfo.nPos;
    if SBStyle = SB_VERT then
      FScrollOrigin.Y := ScrollInfo.nPos;
  end;
  
  if (SIF_PAGE and ScrollInfo.fMask) > 0 then
  begin
    if SBStyle = SB_HORZ then
      FScrollPageSize.X := ScrollInfo.nPage;
    if SBStyle = SB_VERT then
      FScrollPageSize.Y := ScrollInfo.nPage;
  end;
  
  if (SBStyle in [SB_HORZ, SB_VERT]) and
    ((ScrollInfo.fMask and (SIF_RANGE or SIF_POS or SIF_PAGE)) > 0) then
  begin
    if OSError(
      CreateEvent(nil, kEventClassScrollable, kEventScrollableInfoChanged, 0,
        kEventAttributeUserEvent, Event),
      Self, SName, 'CreateEvent') then Exit;
    try
      OSError(SendEventToEventTarget(Event, GetControlEventTarget(Widget)),
        Self, SName, 'SendEventToEventTarget');
    finally
      ReleaseEvent(Event);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomControl.GetScrollInfo
  Params:  SBStyle    - Scrollbar type (SB_VERT, SB_HORZ)
           ScrollInfo - Record fo scrolling info
  Returns: If the function suceeds

  Gets the scrolling info of the specified scroll bar
 ------------------------------------------------------------------------------}
procedure TCarbonCustomControl.GetScrollInfo(SBStyle: Integer;
  var ScrollInfo: TScrollInfo);
const
  SName = 'GetScrollInfo';
begin
  if (SIF_RANGE and ScrollInfo.fMask) > 0 then
  begin
    ScrollInfo.nMin := 0;
    
    if SBStyle = SB_HORZ then
      ScrollInfo.nMax := FScrollSize.X;
    if SBStyle = SB_VERT then
      ScrollInfo.nMax := FScrollSize.Y;
  end;

  if (SIF_POS and ScrollInfo.fMask) > 0 then
  begin
    if SBStyle = SB_HORZ then
      ScrollInfo.nPos := FScrollOrigin.X;
    if SBStyle = SB_VERT then
      ScrollInfo.nPos := FScrollOrigin.Y;
  end;

  if (SIF_PAGE and ScrollInfo.fMask) > 0 then
  begin
    if SBStyle = SB_HORZ then
      ScrollInfo.nPage := FScrollPageSize.X;
    if SBStyle = SB_VERT then
      ScrollInfo.nPage := FScrollPageSize.Y;
  end;
end;

{ TCarbonScrollingWinControl }

{------------------------------------------------------------------------------
  Method:  TCarbonScrollingWinControl.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon scrolling window control
 ------------------------------------------------------------------------------}
procedure TCarbonScrollingWinControl.CreateWidget(const AParams: TCreateParams);
var
  Params: TCreateParams;
begin
  Params := AParams;
  // add both scrollbars
  Params.Style := Params.Style or WS_HSCROLL or WS_VSCROLL;
  
  inherited CreateWidget(Params);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonScrollingWinControl.GetForceEmbedInScrollView
  Returns: Whether use scroll view even if no scroll bars are needed
 ------------------------------------------------------------------------------}
function TCarbonScrollingWinControl.GetForceEmbedInScrollView: Boolean;
begin
  Result := True;
end;

{ TCarbonGroupBox }

{------------------------------------------------------------------------------
  Method:  TCarbonGroupBox.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon custom group box
 ------------------------------------------------------------------------------}
procedure TCarbonGroupBox.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  R: TRect;
begin
  if OSError(
    CreateGroupBoxControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      nil, not (LCLObject.Parent is TCustomGroupBox), Control),
    Self, SCreateWidget, 'CreateGroupBoxControl') then RaiseCreateWidgetError(LCLObject);

  Widget := Control;
  
  if not GetClientRect(R) then
  begin
    DebugLn('TCarbonGroupBox.CreateWidget Error - no content region!');
    Exit;
  end;
  
  if OSError(
    CreateUserPaneControl(GetTopParentWindow, GetCarbonRect(R),
      kControlSupportsEmbedding or kControlHandlesTracking, FUserPane),
    Self, SCreateWidget, 'CreateUserPaneControl') then Exit;
  
  if OSError(HIViewAddSubview(Control, FUserPane), Self, SCreateWidget,
    SViewAddView) then Exit;
  
  inherited;

  SetText(AParams.Caption);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonGroupBox.DestroyWidget
  
  Clean-up
 ------------------------------------------------------------------------------}
procedure TCarbonGroupBox.DestroyWidget;
begin
  DisposeControl(FUserPane);

  inherited DestroyWidget;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonGroupBox.GetContent
  Returns: Content area control
 ------------------------------------------------------------------------------}
function TCarbonGroupBox.GetContent: ControlRef;
begin
  Result := FUserPane;
end;

{ TCarbonStatusBar }

{------------------------------------------------------------------------------
  Method:  TCarbonStatusBar.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon status bar
 ------------------------------------------------------------------------------}
procedure TCarbonStatusBar.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
begin
  if OSError(
    CreatePlacardControl(GetTopParentWindow, ParamsToCarbonRect(AParams), Control),
    Self, SCreateWidget, 'CreatePlacardControl') then RaiseCreateWidgetError(LCLObject);

  Widget := Control;

  inherited;
  
  FPanels := TObjectList.Create(True);
  UpdatePanel; // add panels
end;

{------------------------------------------------------------------------------
  Method:  TCarbonStatusBar.DestroyWidget

  Clean-up
 ------------------------------------------------------------------------------}
procedure TCarbonStatusBar.DestroyWidget;
begin
  FPanels.Free;

  inherited DestroyWidget;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonStatusBar.GetValidEvents
  Returns: Set of events with installed handlers
 ------------------------------------------------------------------------------}
class function TCarbonStatusBar.GetValidEvents: TCarbonControlEvents;
begin
  Result := [cceDraw];
end;

{------------------------------------------------------------------------------
  Method:  TCarbonStatusBar.Draw

  Draw event handler
 ------------------------------------------------------------------------------}
procedure TCarbonStatusBar.Draw;
var
  StatusBar: TStatusBar;
  R: TRect;
begin
  StatusBar := LCLObject as TStatusBar;
  
  if StatusBar.SimplePanel and (StatusBar.SimpleText <> '') then
  begin
    GetClientRect(R);
    
    WidgetSet.ExtTextOut(HDC(Context), R.Top, R.Left, 0, nil, PChar(StatusBar.SimpleText),
      Length(StatusBar.SimpleText), nil);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonStatusBar.GetPreferredSize
  Returns: The preffered size of status bar for autosizing or (0, 0)
 ------------------------------------------------------------------------------}
function TCarbonStatusBar.GetPreferredSize: TPoint;
begin
  Result := inherited GetPreferredSize;
  
  // stretch status bar to whole window width
  if LCLObject.Parent <> nil then
    Result.X := LCLObject.Parent.ClientWidth;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonStatusBar.UpdatePanel
  Params:  AIndex - Index of panel to update or -1 to update all

  Updates properties of the specified panel(s) of status bar
 ------------------------------------------------------------------------------}
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
  
  Invalidate;
end;

{ TCarbonListBox }

{------------------------------------------------------------------------------
  Method:  TCarbonListBox.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon list box
 ------------------------------------------------------------------------------}
procedure TCarbonListBox.CreateWidget(const AParams: TCreateParams);
begin
  // TODO

  Widget := nil;
  inherited;

  FItemIndex := -1;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBox.GetItemsCount
  Returns: The count of items in list box
 ------------------------------------------------------------------------------}
function TCarbonListBox.GetItemsCount: Integer;
begin
  Result := 0;
  // TODO
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBox.GetItemIndex
  Returns: The index of selected item in list box
 ------------------------------------------------------------------------------}
function TCarbonListBox.GetItemIndex: Integer;
begin
  Result := FItemIndex;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBox.SetItemIndex
  Params:  AIndex - Index of item to select

  Sets the index of item to select
 ------------------------------------------------------------------------------}
procedure TCarbonListBox.SetItemIndex(AIndex: Integer);
begin
  // TODO
end;

{ TCarbonCustomCheckBox }

{------------------------------------------------------------------------------
  Method:  TCarbonCustomCheckBox.GetValidEvents
  Returns: Set of events with installed handlers
 ------------------------------------------------------------------------------}
class function TCarbonCustomCheckBox.GetValidEvents: TCarbonControlEvents;
begin
  Result := [cceValueChanged, cceHit];
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomCheckBox.Hit
  Params:  AControlPart - Hitted control part

  Hit event handler
 ------------------------------------------------------------------------------}
procedure TCarbonCustomCheckBox.Hit(AControlPart: ControlPartCode);
begin
  // do nothing, because value changed will be fired immediately
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomCheckBox.ValueChanged

  Value changed event handler
 ------------------------------------------------------------------------------}
procedure TCarbonCustomCheckBox.ValueChanged;
begin
  LCLSendChangedMsg(LCLObject);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomCheckBox.RetrieveState
  Returns: State of Carbon custom check box
 ------------------------------------------------------------------------------}
function TCarbonCustomCheckBox.RetrieveState: TCheckBoxState;
begin
  case GetControl32BitValue(ControlRef(Widget)) of
    kControlCheckBoxCheckedValue   : Result := cbChecked;
    kControlCheckBoxUncheckedValue : Result := cbUnchecked;
    kControlCheckBoxMixedValue     : Result := cbGrayed;
  else
    Result := cbUnchecked;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomCheckBox.SetState
  Params:  AState        - New state

  Sets the new state of Carbon custom check box
 ------------------------------------------------------------------------------}
procedure TCarbonCustomCheckBox.SetState(AState: TCheckBoxState);
var
  Value: UInt32;
begin
  case AState of
    cbChecked  : Value := kControlCheckBoxCheckedValue;
    cbUnChecked: Value := kControlCheckBoxUncheckedValue;
    cbGrayed   : Value := kControlCheckBoxMixedValue;
  end;
  
  SetControl32BitValue(ControlRef(Widget), Value);
end;

{ TCarbonCheckBox }

{------------------------------------------------------------------------------
  Method:  TCarbonCheckBox.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon check box
 ------------------------------------------------------------------------------}
procedure TCarbonCheckBox.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  Value: UInt32;
begin
  case (LCLObject as TCustomCheckBox).State of
    cbChecked  : Value := kControlCheckBoxCheckedValue;
    cbUnChecked: Value := kControlCheckBoxUncheckedValue;
    cbGrayed   : Value := kControlCheckBoxMixedValue;
  end;

  if OSError(
    CreateCheckBoxControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      nil, Value, True, Control),
    Self, SCreateWidget, 'CreateCheckBoxControl') then RaiseCreateWidgetError(LCLObject);
    
  Widget := Control;

  inherited;

  SetText(AParams.Caption);
end;

{ TCarbonToggleBox }

{------------------------------------------------------------------------------
  Method:  TCarbonToggleBox.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon toggle box
 ------------------------------------------------------------------------------}
procedure TCarbonToggleBox.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  Value: UInt32;
begin
  case (LCLObject as TToggleBox).State of
    cbChecked  : Value := kControlCheckBoxCheckedValue;
    cbUnChecked: Value := kControlCheckBoxUncheckedValue;
    cbGrayed   : Value := kControlCheckBoxMixedValue;
  end;

  if OSError(
    CreateBevelButtonControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      nil, kControlBevelButtonNormalBevel,
      kControlBehaviorToggles, nil, 0, 0, 0, Control),
    Self, SCreateWidget, SCreateBevelButton) then RaiseCreateWidgetError(LCLObject);

  Widget := Control;
  
  inherited;

  SetText(AParams.Caption);
  SetControl32BitValue(Control, Value);
end;

{ TCarbonRadioButton }

{------------------------------------------------------------------------------
  Method:  TCarbonRadioButton.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon radio button
 ------------------------------------------------------------------------------}
procedure TCarbonRadioButton.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  Value: UInt32;
begin
  case (LCLObject as TRadioButton).State of
    cbChecked  : Value := kControlCheckBoxCheckedValue;
    cbUnChecked: Value := kControlCheckBoxUncheckedValue;
    cbGrayed   : Value := kControlCheckBoxMixedValue;
  end;

  if OSError(
    CreateRadioButtonControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      nil, Value, True, Control),
    Self, SCreateWidget, 'CreateRadioButtonControl') then RaiseCreateWidgetError(LCLObject);

  Widget := Control;
    
  inherited;

  SetText(AParams.Caption);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonRadioButton.ValueChanged

  Value changed event handler
 ------------------------------------------------------------------------------}
procedure TCarbonRadioButton.ValueChanged;
var
  RadioButton: TRadioButton;
  Sibling: TControl;
  I: Integer;
begin
  if GetControl32BitValue(ControlRef(Widget)) = kControlCheckBoxCheckedValue then
  begin
    //DebugLn('TCarbonRadioButton.ValueChanged Uncheck Sibling');

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

{ TCarbonCustomButton }

{------------------------------------------------------------------------------
  Method:  TCarbonCustomButton.GetValidEvents
  Returns: Set of events with installed handlers
 ------------------------------------------------------------------------------}
class function TCarbonCustomButton.GetValidEvents: TCarbonControlEvents;
begin
  Result := [cceHit];
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomButton.Hit
  Params:  AControlPart - Hitted control part

  Hit event handler
 ------------------------------------------------------------------------------}
procedure TCarbonCustomButton.Hit(AControlPart: ControlPartCode);
begin
  LCLSendClickedMsg(LCLObject);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomButton.SetDefault
  Params:  ADefault - Is default

  Sets the default indication
 ------------------------------------------------------------------------------}
procedure TCarbonCustomButton.SetDefault(ADefault: Boolean);
begin
  OSError(
    SetControlData(ControlRef(Widget), kControlEntireControl,
      kControlPushButtonDefaultTag, SizeOf(Boolean), @ADefault),
    Self, 'SetDefault', SSetData);
end;

{ TCarbonButton }

{------------------------------------------------------------------------------
  Method:  TCarbonButton.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon button
 ------------------------------------------------------------------------------}
procedure TCarbonButton.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
begin
  // create the button at bounds
  if OSError(
    CreatePushButtonControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      nil, Control),
    Self, SCreateWidget, 'CreatePushButtonControl') then RaiseCreateWidgetError(LCLObject);

  Widget := Control;

  inherited;

  SetText(AParams.Caption);
end;

{ TCarbonBitBtn }

{------------------------------------------------------------------------------
  Method:  TCarbonBitBtn.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon bitmap button
 ------------------------------------------------------------------------------}
procedure TCarbonBitBtn.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  ButtonKind: ThemeButtonKind;
begin
  if OSError(
    CreateBevelButtonControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      nil, kControlBevelButtonLargeBevel, kControlBehaviorPushbutton,
      nil, 0, 0, 0, Control),
    Self, SCreateWidget, SCreateBevelButton) then RaiseCreateWidgetError(LCLObject);

  Widget := Control;
    
  inherited;

  SetText(AParams.Caption);
  
  // set round border
  ButtonKind := kThemeRoundedBevelButton;
  OSError(SetControlData(ControlRef(Widget), kControlEntireControl,
      kControlBevelButtonKindTag, SizeOf(ThemeButtonKind), @ButtonKind),
    Self, SCreateWidget, SSetData, 'kControlBevelButtonKindTag');
end;

{------------------------------------------------------------------------------
  Method:  TCarbonBitBtn.SetGlyph
  Params:  AGlyph  - New glyph bitmap

  Sets the glyph bitmap
 ------------------------------------------------------------------------------}
procedure TCarbonBitBtn.SetGlyph(const AGlyph: TBitmap);
var
  ContentInfo: ControlButtonContentInfo;
  FreeImage: Boolean;
  BitBtn: TCustomBitBtn;
  R: TRect;
begin
  ContentInfo.contentType := kControlContentCGImageRef;
  
  FreeImage := False;
  ContentInfo.imageRef := nil;
  
  if AGlyph <> nil then
  begin
    if TObject(AGlyph.Handle) is TCarbonBitmap then
    begin
      BitBtn := LCLObject as TCustomBitBtn;
      
      if BitBtn.NumGlyphs <= 1 then
        ContentInfo.imageRef := TCarbonBitmap(AGlyph.Handle).CGImage
      else
      begin
        // TODO: consider button style (down, disabled)
        R := Classes.Rect(0, 0, AGlyph.Width div BitBtn.NumGlyphs, AGlyph.Height);
        ContentInfo.imageRef := TCarbonBitmap(AGlyph.Handle).GetSubImage(R);
        FreeImage := True;
      end;
    end;
  end;
  
  try
    OSError(SetBevelButtonContentInfo(ControlRef(Widget), @ContentInfo),
      Self, 'SetGlyph', 'SetBevelButtonContentInfo');
  finally
    if FreeImage then CGImageRelease(ContentInfo.imageRef);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonBitBtn.SetLayout
  Params:  ALayout  - Bitmap and caption layout

  Sets the bitmap and caption layout
 ------------------------------------------------------------------------------}
procedure TCarbonBitBtn.SetLayout(ALayout: TButtonLayout);
var
  Placement: ControlButtonTextPlacement;
begin
  case ALayout of
    blGlyphLeft  : Placement := kControlBevelButtonPlaceToRightOfGraphic;
    blGlyphRight : Placement := kControlBevelButtonPlaceToLeftOfGraphic;
    blGlyphTop   : Placement := kControlBevelButtonPlaceBelowGraphic;
    blGlyphBottom: Placement := kControlBevelButtonPlaceAboveGraphic;
  end;

  OSError(SetBevelButtonTextPlacement(ControlRef(Widget), Placement),
    Self, 'SetLayout', 'SetBevelButtonTextPlacement');

  Invalidate;
end;


{ TCarbonStaticText }

{------------------------------------------------------------------------------
  Method:  TCarbonStaticText.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon static text
 ------------------------------------------------------------------------------}
procedure TCarbonStaticText.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  MultiLine: Boolean = True;
  FontStyle: ControlFontStyleRec;
begin
  FontStyle.flags := kControlUseJustMask;
  case (LCLObject as TCustomStaticText).Alignment of
  taLeftJustify:  FontStyle.just := teFlushLeft;
  taRightJustify: FontStyle.just := teFlushRight;
  taCenter:       FontStyle.just := teCenter;
  end;

  if OSError(
    CreateStaticTextControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      nil, @FontStyle, Control),
    Self, SCreateWidget, 'CreateStaticTextControl') then RaiseCreateWidgetError(LCLObject);

  Widget := Control;
    
  inherited;

  SetText(AParams.Caption);
  
  // switch on multi-line attribute
  OSError(
    SetControlData(Control, kControlEntireControl,
      kControlStaticTextIsMultilineTag, SizeOf(Boolean), @MultiLine),
    Self, SCreateWidget, SSetData, 'kControlStaticTextIsMultilineTag');
end;

{------------------------------------------------------------------------------
  Method:  TCarbonStaticText.SetAlignment
  Params:  AAlignment      - New caption alignment

  Sets the new caption alignment of Carbon static text
 ------------------------------------------------------------------------------}
procedure TCarbonStaticText.SetAlignment(AAlignment: TAlignment);
var
  FontStyle: ControlFontStyleRec;
const
  SName = 'SetAlignment';
begin
  // get static text font style and change only justification
  OSError(
    GetControlData(ControlRef(Widget), kControlEntireControl,
      kControlStaticTextStyleTag, SizeOf(FontStyle), @FontStyle, nil),
    Self, SName, SGetData);

  FontStyle.flags := FontStyle.flags or kControlUseJustMask;
  case AAlignment of
  taLeftJustify : FontStyle.just := teFlushLeft;
  taRightJustify: FontStyle.just := teFlushRight;
  taCenter      : FontStyle.just := teCenter;
  end;

  OSError(
    SetControlData(ControlRef(Widget), kControlEntireControl,
      kControlStaticTextStyleTag, SizeOf(FontStyle), @FontStyle),
    Self, SName, SSetData);

  Invalidate;
end;


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
  Method:  TCarbonScrollBar.ValueChanged

  Value changed event handler
 ------------------------------------------------------------------------------}
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

