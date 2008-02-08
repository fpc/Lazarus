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
  CarbonDef, CarbonGDIObjects,
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
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
    procedure AddControlPart(const AControl: ControlRef);
    function GetContent: ControlRef; override;
    function GetControlContentRect(var ARect: TRect): Boolean;
    function GetFrame(Index: Integer): ControlRef; virtual;
    function GetFrameBounds(var ARect: TRect): Boolean; virtual;
    function GetForceEmbedInScrollView: Boolean; virtual;
    function UpdateContentBounds: Boolean;
    function EmbedInScrollView(const AParams: TCreateParams): HIViewRef;
    function EmbedInScrollView(AScrollBars: TScrollStyle): HIViewRef;
    procedure ChangeScrollBars(AScrollView: HIViewRef; var AScrollBars: TScrollStyle; ANewValue: TScrollStyle);
  public
    class function GetFrameCount: Integer; virtual;
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

    procedure SetFocus; override;
    procedure SetColor(const AColor: TColor); override;
    procedure SetFont(const AFont: TFont); override;
    procedure SetZOrder(AOrder: HIViewZOrderOp; ARefWidget: TCarbonWidget); override;
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
    property Frames[Index: Integer]: ControlRef read GetFrame;
  end;
  
  { TCarbonWindow }

  TCarbonWindow = class(TCarbonWidget)
  protected
    procedure RegisterEvents; override;
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
    procedure GetScrollInfo(SBStyle: Integer; var ScrollInfo: TScrollInfo); override;
    function SetScrollInfo(SBStyle: Integer; const ScrollInfo: TScrollInfo): Integer; override;
    function SetBounds(const ARect: TRect): Boolean; override;

    procedure SetFocus; override;
    procedure SetColor(const AColor: TColor); override;
    procedure SetFont(const AFont: TFont); override;
    procedure SetZOrder(AOrder: HIViewZOrderOp; ARefWidget: TCarbonWidget); override;
    procedure ShowHide(AVisible: Boolean); override;
    
    function GetText(var S: String): Boolean; override;
    function SetText(const S: String): Boolean; override;
    
    function Update: Boolean; override;
  public
    function Activate: Boolean; virtual;

    procedure CloseModal; virtual;
    procedure ShowModal; virtual;
    
    function SetForeground: Boolean; virtual;
    function Show(AShow: Integer): Boolean; virtual;

    procedure SetBorderIcons(ABorderIcons: TBorderIcons); virtual;
    procedure SetFormBorderStyle(AFormBorderStyle: TFormBorderStyle); virtual;
  end;
  
  { TCarbonHintWindow }

  TCarbonHintWindow = class(TCarbonWindow)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  end;
  
  { TCarbonDesignWindow }

  TCarbonDesignWindow = class(TCarbonWindow)
  private
    FDesignControl: HIViewRef;
    FDesignContext: TCarbonContext;
    procedure BringDesignerToFront;
  protected
    procedure RegisterEvents; override;
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
  public
    procedure ControlAdded; override;
    procedure BoundsChanged; override;

    procedure SetChildZPosition(AChild: TCarbonWidget; const AOldPos, ANewPos: Integer; const AChildren: TFPList); override;
    
    function GetDesignContext: TCarbonContext;
    procedure ReleaseDesignContext;
  end;

  { TCarbonCustomControl }

  TCarbonCustomControl = class(TCarbonControl)
  private
    FScrollView: HIViewRef;
    FScrollOrigin: HIPoint;
    FScrollSize: TPoint;
    FScrollPageSize: TPoint;
    FMulX: Single; // multiply x coords to fit real page size
    FMulY: Single; // multiply y coords to fit real page size
    FTextFractional: Boolean;
  protected
    procedure RegisterEvents; override;
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
    function GetFrame(Index: Integer): ControlRef; override;
    public
    class function GetValidEvents: TCarbonControlEvents; override;
    procedure Draw; override;
    procedure GetInfo(out AImageSize, AViewSize, ALineSize: HISize; out AOrigin: HIPoint); virtual;
    procedure ScrollTo(const ANewOrigin: HIPoint); virtual;
  public
    procedure SetColor(const AColor: TColor); override;
    procedure SetFont(const AFont: TFont); override;
    procedure GetScrollInfo(SBStyle: Integer; var ScrollInfo: TScrollInfo); override;
    function SetScrollInfo(SBStyle: Integer; const ScrollInfo: TScrollInfo): Integer; override;

    property TextFractional: Boolean read FTextFractional write FTextFractional;
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
  public
    function GetClientRect(var ARect: TRect): Boolean; override;
    function SetBounds(const ARect: TRect): Boolean; override;
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
    procedure SetColor(const AColor: TColor); override;
    procedure SetFont(const AFont: TFont); override;
    procedure UpdatePanel(AIndex: Integer = -1);
  end;

  { TCarbonStaticText }

  TCarbonStaticText = class(TCarbonControl)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    procedure SetAlignment(AAlignment: TAlignment); virtual;
  end;
  
procedure RaiseCreateWidgetError(AControl: TWinControl);

function GetCarbonWidget(AWidget: Pointer): TCarbonWidget;
function GetCarbonWindow(AWidget: WindowRef): TCarbonWindow;
function GetCarbonControl(AWidget: ControlRef): TCarbonControl;

implementation

uses InterfaceBase, CarbonInt, CarbonProc, CarbonDbgConsts, CarbonUtils,
  CarbonWSStdCtrls, CarbonCanvas, CarbonCaret;

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

{ TCarbonDesignWindow }

{------------------------------------------------------------------------------
  Name: CarbonDesign_Draw
  Handles draw event
 ------------------------------------------------------------------------------}
function CarbonDesign_Draw(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  ADesignWindow: TCarbonDesignWindow;
  AStruct: PPaintStruct;
begin
  {$IFDEF VerbosePaint}
    Debugln('CarbonDesign_Draw ', DbgSName(AWidget.LCLObject));
  {$ENDIF}

  ADesignWindow := (AWidget as TCarbonDesignWindow);

  ADesignWindow.FDesignContext := TCarbonControlContext.Create(ADesignWindow);
  try
    // set canvas context
    if OSError(
      GetEventParameter(AEvent, kEventParamCGContextRef, typeCGContextRef, nil,
        SizeOf(CGContextRef), nil, @(ADesignWindow.FDesignContext.CGContext)),
      'CarbonDesign_Draw', SGetEvent, 'kEventParamCGContextRef') then Exit;

    // let carbon draw/update
    Result := CallNextEventHandler(ANextHandler, AEvent);

    // draw designer stuff
    New(AStruct);
    FillChar(AStruct^, SizeOf(TPaintStruct), 0);
    AStruct^.hdc := HDC(ADesignWindow.FDesignContext);
    try
      {$IFDEF VerbosePaint}
        DebugLn('CarbonDesign_Draw LM_PAINT to ', DbgSName(AWidget.LCLObject));
      {$ENDIF}
      LCLSendPaintMsg(AWidget.LCLObject, HDC(ADesignWindow.FDesignContext), AStruct);
    finally
      Dispose(AStruct);
    end;
  finally
    FreeAndNil(ADesignWindow.FDesignContext);
  end;
  {$IFDEF VerbosePaint}
    Debugln('CarbonDesign_Draw end ', DbgSName(AWidget.LCLObject));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDesignWindow.BringDesignerToFront
 ------------------------------------------------------------------------------}
procedure TCarbonDesignWindow.BringDesignerToFront;
begin
  OSError(HIViewSetZOrder(FDesignControl, kHIViewZOrderAbove, nil),
    Self, 'BringDesignerToFront', 'HIViewSetZOrder');
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDesignWindow.RegisterEvents

  Registers event handlers for design window
 ------------------------------------------------------------------------------}
procedure TCarbonDesignWindow.RegisterEvents;
var
  TmpSpec: EventTypeSpec;
begin
  inherited;

  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlDraw);
  InstallControlEventHandler(FDesignControl,
    RegisterEventHandler(@CarbonDesign_Draw),
    1, @TmpSpec, Pointer(Self), nil);
    

  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlTrack);
  InstallControlEventHandler(FDesignControl,
    RegisterEventHandler(@CarbonCommon_Track),
    1, @TmpSpec, Pointer(Self), nil);

  {$IFDEF VerboseWindowEvent}
    DebugLn('TCarbonDesignWindow.RegisterEvents ', ClassName, ' ',
      LCLObject.Name, ': ', LCLObject.ClassName);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDesignWindow.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon window for designing
 ------------------------------------------------------------------------------}
procedure TCarbonDesignWindow.CreateWidget(const AParams: TCreateParams);
var
  R: TRect;
begin
  inherited;
  
  // create custom view above all others
  GetClientRect(R);
  OffsetRect(R, -R.Left, -R.Top);
  FDesignControl := CreateCustomHIView(RectToCGRect(R));
  
  OSError(
    HIViewChangeFeatures(FDesignControl, kHIViewFeatureDoesNotUseSpecialParts,
      kHIViewFeatureGetsFocusOnClick),
    SCreateWidget, 'HIViewChangeFeatures');
    
  OSError(
    SetControlProperty(FDesignControl, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Self), @Self),
    Self, SCreateWidget, SSetControlProp);
    
  OSError(HIViewAddSubview(Content, FDesignControl), Self, SCreateWidget, SViewAddView);
  BringDesignerToFront;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDesignWindow.DestroyWidget

  Override to do some clean-up
 ------------------------------------------------------------------------------}
procedure TCarbonDesignWindow.DestroyWidget;
begin
  DisposeControl(FDesignControl);
  
  inherited;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDesignWindow.ControlAdded

  Notifies about control added
 ------------------------------------------------------------------------------}
procedure TCarbonDesignWindow.ControlAdded;
begin
  BringDesignerToFront;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDesignWindow.BoundsChanged

  Handles bounds change
 ------------------------------------------------------------------------------}
procedure TCarbonDesignWindow.BoundsChanged;
var
  R: TRect;
begin
  inherited;

  GetClientRect(R);
  OffsetRect(R, -R.Left, -R.Top);
  OSError(HIViewSetFrame(FDesignControl, RectToCGRect(R)),
    Self, SSetBounds, SViewFrame);

  BringDesignerToFront;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDesignWindow.SetChildZPosition
  Params:  AChild      - Child widget
           AOldPos     - Old z position
           ANewPos     - New z position
           AChildren   - List of all child controls

  Sets the child z position of Carbon widget
 ------------------------------------------------------------------------------}
procedure TCarbonDesignWindow.SetChildZPosition(AChild: TCarbonWidget; const AOldPos,
  ANewPos: Integer; const AChildren: TFPList);
begin
  inherited;
  
  BringDesignerToFront;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDesignWindow.GetDesignContext
  Returns: Context for drawing designer stuff
 ------------------------------------------------------------------------------}
function TCarbonDesignWindow.GetDesignContext: TCarbonContext;
begin
  if FDesignContext <> nil then Result := FDesignContext
  else Result := DefaultContext;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonDesignWindow.ReleaseDesignContext

  Releases the context for drawing designer stuff
 ------------------------------------------------------------------------------}
procedure TCarbonDesignWindow.ReleaseDesignContext;
begin
  // nothing
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
  ImageHISize, ViewHISize, LineHISize: HISize;
  HIOrigin: HIPoint;
const
  SName = 'CarbonScrollable_GetInfo';
begin
  {$IFDEF VerboseControlEvent}
    DebugLn('CarbonScrollable_GetInfo ', DbgSName(AWidget.LCLObject));
  {$ENDIF}

  (AWidget as TCarbonCustomControl).GetInfo(ImageHISize, ViewHISize, LineHISize, HIOrigin);
  
  OSError(SetEventParameter(AEvent, kEventParamImageSize, typeHISize,
    SizeOf(HISize), @ImageHISize), SName, SSetEvent, 'kEventParamImageSize');
  OSError(SetEventParameter(AEvent, kEventParamViewSize, typeHISize,
    SizeOf(HISize), @ViewHISize), SName, SSetEvent, 'kEventParamViewSize');
  OSError(SetEventParameter(AEvent, kEventParamLineSize, typeHISize,
    SizeOf(HISize), @LineHISize), SName, SSetEvent, 'kEventParamLineSize');
  OSError(SetEventParameter(AEvent, kEventParamOrigin, typeHIPoint,
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
  
  (AWidget as TCarbonCustomControl).ScrollTo(Origin);
  
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
  Method:  TCarbonCustomControl.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon custom control
 ------------------------------------------------------------------------------}
procedure TCarbonCustomControl.CreateWidget(const AParams: TCreateParams);
begin
  Widget := CreateCustomHIView(ParamsToHIRect(AParams));
  if Widget = nil then RaiseCreateWidgetError(LCLObject);
  
  FScrollView := EmbedInScrollView(AParams);
  FScrollSize := Classes.Point(0, 0);
  FScrollPageSize := Classes.Point(0, 0);
  FScrollOrigin := GetHIPoint(0, 0);
  FMulX := 1;
  FMulY := 1;

  if LCLObject.ClassNameIs('TSynEdit') then
     FTextFractional := False
  else
    FTextFractional := True;
    
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
  Params:  Frame index
  Returns: Frame area control
 ------------------------------------------------------------------------------}
function TCarbonCustomControl.GetFrame(Index: Integer): ControlRef;
begin
  Result := FScrollView;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomControl.GetValidEvents
  Returns: Set of events with installed handlers
 ------------------------------------------------------------------------------}
class function TCarbonCustomControl.GetValidEvents: TCarbonControlEvents;
begin
  Result := [cceDraw];
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomControl.Draw

  Draw event handler
 ------------------------------------------------------------------------------}
procedure TCarbonCustomControl.Draw;
var
  DC: TCarbonDeviceContext;
begin
  if Context <> nil then
  begin
    DC := (Context as TCarbonDeviceContext);
    
    if DC.CGContext <> nil then
    begin
      if DC.CurrentBrush <> nil then // apply control background color
      begin
        if LCLObject.Color <> clBtnFace then
          DC.CurrentBrush.SetColor(LCLObject.Color, True)
        else
          DC.CurrentBrush.SetColor(LCLObject.Color, False);

        DC.CurrentBrush.Apply(DC, False);
        if LCLObject.Color <> clBtnFace then DC.FillRect(DC.GetClipRect, DC.CurrentBrush);
      end;
    end;
    
    DC.TextFractional := TextFractional;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomControl.GetInfo
  Params:  AImageSize - Size of entire scrollable area
           AViewSize  - Size of scrollable page
           ALineSize  - Size of scrollable line
           AOrigin    - Scroll position

  Handles scrollable get info event
 ------------------------------------------------------------------------------}
procedure TCarbonCustomControl.GetInfo(out AImageSize, AViewSize, ALineSize: HISize;
  out AOrigin: HIPoint);
var
  C: TRect;
begin
  // modify coordinates to fit real page size
  GetClientRect(C);
  
  if FScrollPageSize.X = 0 then FMulX := 1
  else
    FMulX := (C.Right - C.Left) / FScrollPageSize.X;
  if FScrollPageSize.Y = 0 then FMulY := 1
  else
    FMulY := (C.Bottom - C.Top) / FScrollPageSize.Y;
  
  AOrigin := GetHIPoint(FScrollOrigin.X * FMulX, FScrollOrigin.Y * FMulY);
  AImageSize := GetHISize(FScrollSize.X * FMulX, FScrollSize.Y * FMulY);
  AViewSize := GetHISize(C.Right - C.Left, C.Bottom - C.Top);
  ALineSize := GetHISize(FScrollPageSize.X * FMulX / 40, FScrollPageSize.Y * FMulY / 40);
  
  {$IFDEF VerboseScroll}
    DebugLn('TCarbonCustomControl.GetInfo ' + LCLObject.Name + ' Origin: ' +
      DbgS(AOrigin) + ' Image: ' + DbgS(AImageSize) + ' View: ' +
      DbgS(AViewSize) + 'Line: ' + DbgS(ALineSize));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomControlScrollTo
  Params:  ANewOrigin - New scroll position

  Handles scrollable scroll to event
 ------------------------------------------------------------------------------}
procedure TCarbonCustomControl.ScrollTo(const ANewOrigin: HIPoint);
var
  ScrollMsg: TLMScroll;
begin
  {$IFDEF VerboseScroll}
    DebugLn('TCarbonCustomControl.ScrollTo ' + LCLObject.Name + ' Origin: ' +
      DbgS(ANewOrigin));
  {$ENDIF}
  
  if FMulX = 0 then FScrollOrigin.X := 0
  else
    FScrollOrigin.X := ANewOrigin.X / FMulX;
  if FMulY = 0 then FScrollOrigin.Y := 0
  else
    FScrollOrigin.Y := ANewOrigin.Y / FMulY;
  
  // send vertical scroll
  FillChar(ScrollMsg, SizeOf(TLMScroll), 0);
  with ScrollMsg do
  begin
    Msg := LM_VSCROLL;
    Pos := Round(FScrollOrigin.Y);
    ScrollCode := SB_THUMBPOSITION;
  end;
  DeliverMessage(LCLObject, ScrollMsg);
  
  // send horizontal scroll
  FillChar(ScrollMsg, SizeOf(TLMScroll), 0);
  with ScrollMsg do
  begin
    Msg := LM_HSCROLL;
    Pos := Round(FScrollOrigin.X);
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
  {$IFDEF VerboseScroll}
    DebugLn('TCarbonCustomControl.SetScrollInfo ' + LCLObject.Name +
      ' SBStyle: ' + DbgS(SBStyle) + ' ' + DbgS(ScrollInfo));
  {$ENDIF}

  if SBStyle = SB_HORZ then
    Result := Round(FScrollOrigin.X);
  if SBStyle = SB_VERT then
    Result := Round(FScrollOrigin.Y);

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
      OSError(SendEventToEventTarget(Event, GetControlEventTarget(FScrollView)),
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
  {$IFDEF VerboseScroll}
    DebugLn('TCarbonCustomControl.GetScrollInfo ' + LCLObject.Name +
      ' SBStyle: ' + DbgS(SBStyle) + ' ' + DbgS(ScrollInfo));
  {$ENDIF}
  
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
      ScrollInfo.nPos := Round(FScrollOrigin.X);
    if SBStyle = SB_VERT then
      ScrollInfo.nPos := Round(FScrollOrigin.Y);
  end;

  if (SIF_PAGE and ScrollInfo.fMask) > 0 then
  begin
    if SBStyle = SB_HORZ then
      ScrollInfo.nPage := FScrollPageSize.X;
    if SBStyle = SB_VERT then
      ScrollInfo.nPage := FScrollPageSize.Y;
  end;
  
  {$IFDEF VerboseScroll}
    DebugLn('TCarbonCustomControl.GetScrollInfo Result: ' + DbgS(ScrollInfo));
  {$ENDIF}
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
  
  FUserPane := CreateCustomHIView(RectToCGRect(R));
  if FUserPane = nil then RaiseCreateWidgetError(LCLObject);
    
  OSError(HIViewSetVisible(FUserPane, True), Self, SCreateWidget, SViewVisible);
  
  if OSError(HIViewAddSubview(Control, FUserPane), Self, SCreateWidget,
    SViewAddView) then RaiseCreateWidgetError(LCLObject);
  
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

{------------------------------------------------------------------------------
  Method:  TCarbonGroupBox.GetClientRect
  Params:  ARect - Record for client area coordinates
  Returns: If the function succeeds

  Returns the control client rectangle relative to the control origin
 ------------------------------------------------------------------------------}
function TCarbonGroupBox.GetClientRect(var ARect: TRect): Boolean;
begin
  Result := GetControlContentRect(ARect);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonGroupBox.SetBounds
  Params:  ARect - Record for control coordinates
  Returns: If function succeeds

  Sets the control bounding rectangle relative to the client origin of its
  parent
 ------------------------------------------------------------------------------}
function TCarbonGroupBox.SetBounds(const ARect: TRect): Boolean;
begin
  Result := False;
  if not inherited SetBounds(ARect) then Exit;
  Result := UpdateContentBounds;
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
    
    (Context as TCarbonDeviceContext).ExtTextOut(R.Top, R.Left, 0, nil,
      PChar(StatusBar.SimpleText), Length(StatusBar.SimpleText), nil);
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
  Method:  TCarbonStatusBar.SetColor
  Params:  AColor - New color

  Sets the color of control (for edit like controls)
 ------------------------------------------------------------------------------}
procedure TCarbonStatusBar.SetColor(const AColor: TColor);
begin
  // not supported
end;

{------------------------------------------------------------------------------
  Method:  TCarbonStatusBar.SetFont
  Params:  AFont - New font

  Sets the font of control
 ------------------------------------------------------------------------------}
procedure TCarbonStatusBar.SetFont(const AFont: TFont);
begin
  // not supported
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
        
        // fit last panel
        if I < StatusBar.Panels.Count - 1 then
          Panel.Anchors := [akLeft, akTop, akBottom]
        else
        begin
          Panel.Width := StatusBar.Width - X;
          Panel.Anchors := [akLeft, akRight, akTop, akBottom];
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



end.


