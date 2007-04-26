{%MainUnit carbonprivate.pp}
{ $Id$}
{
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

// ==================================================================
// H A N D L E R S 
// ==================================================================

{------------------------------------------------------------------------------
  Name: CarbonControl_Hit
  Handles click and LM_MOUSEUP events
 ------------------------------------------------------------------------------}
function CarbonControl_Hit(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  ControlPart: ControlPartCode;
begin
  {$IFDEF VerboseControlEvent}
    DebugLn('CarbonControl_Hit: ', DbgSName(AWidget.LCLObject));
  {$ENDIF}
  
  Result := CallNextEventHandler(ANextHandler, AEvent);

  if OSError(
    GetEventParameter(AEvent, kEventParamControlPart, typeControlPartCode, nil,
      SizeOf(ControlPartCode), nil, @ControlPart), 'CarbonControl_Hit', SGetEvent,
    SControlPart) then Exit;

  (AWidget as TCarbonControl).Hit(ControlPart);

  // send postponed mouse up event
  DeliverMessage(AWidget.LCLObject, SavedMouseUpMsg);
end;

{------------------------------------------------------------------------------
  Name: CarbonControl_ValueChanged
  Handles value change
 ------------------------------------------------------------------------------}
function CarbonControl_ValueChanged(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  {$IFDEF VerboseControlEvent}
    DebugLn('CarbonControl_ValueChanged ', DbgSName(AWidget.LCLObject));
  {$ENDIF}
  
  Result := CallNextEventHandler(ANextHandler, AEvent);

  (AWidget as TCarbonControl).ValueChanged;
end;

{------------------------------------------------------------------------------
  Name: CarbonControl_IndicatorMoved
  Handles indicator move
 ------------------------------------------------------------------------------}
function CarbonControl_IndicatorMoved(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  {$IFDEF VerboseControlEvent}
    DebugLn('CarbonControl_IndicatorMoved ', DbgSName(AWidget.LCLObject));
  {$ENDIF}
  
  Result := CallNextEventHandler(ANextHandler, AEvent);
  
  (AWidget as TCarbonControl).IndicatorMoved;
end;

{ TCarbonControl }

{------------------------------------------------------------------------------
  Method:  TCarbonControl.GetValidEvents
  Returns: Set of events with installed handlers
 ------------------------------------------------------------------------------}
class function TCarbonControl.GetValidEvents: TCarbonControlEvents;
begin
  Result := [];
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.Hit
  Params:  AControlPart - Hitted control part

  Hit event handler
 ------------------------------------------------------------------------------}
procedure TCarbonControl.Hit(AControlPart: ControlPartCode);
begin
  DebugLn('TCarbonControl.Hit is invalid ', ClassName, ' ',
    LCLObject.Name, ': ', LCLObject.ClassName);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.Draw

  Draw event handler
 ------------------------------------------------------------------------------}
procedure TCarbonControl.Draw;
begin
  DebugLn('TCarbonControl.Draw is invalid ', ClassName, ' ',
    LCLObject.Name, ': ', LCLObject.ClassName);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.ValueChanged

  Value changed event handler
 ------------------------------------------------------------------------------}
procedure TCarbonControl.ValueChanged;
begin
  DebugLn('TCarbonControl.ValueChanged is invalid ', ClassName, ' ',
    LCLObject.Name, ': ', LCLObject.ClassName);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.IndicatorMoved

  Indicator moved event handler
 ------------------------------------------------------------------------------}
procedure TCarbonControl.IndicatorMoved;
begin
  DebugLn('TCarbonControl.IndicatorMoved is invalid ', ClassName, ' ',
    LCLObject.Name, ': ', LCLObject.ClassName);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.DoAction
  Params:  AControlPart - Control part to perform the action

  Action event handler
 ------------------------------------------------------------------------------}
procedure TCarbonControl.DoAction(AControlPart: ControlPartCode);
begin
  DebugLn('TCarbonControl.DoAction is invalid ', ClassName, ' ',
    LCLObject.Name, ': ', LCLObject.ClassName);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.AddToWidget
  Params:  AParent - Parent widget

  Adds control to parent widget
 ------------------------------------------------------------------------------}
procedure TCarbonControl.AddToWidget(AParent: TCarbonWidget);
begin
  // add frame to parent content area
  OSError(HIViewAddSubview(AParent.Content, Frame), Self, 'AddToWidget',
    SViewAddView);
    
  //DebugLn('TCarbonControl.AddToWidget ' + LCLObject.Name + ' ' + DbgS(LCLObject.Parent.ClientRect));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.RegisterEvents

  Registers event handlers for control
 ------------------------------------------------------------------------------}
procedure TCarbonControl.RegisterEvents;
var
  TmpSpec: EventTypeSpec;
  Events: TCarbonControlEvents;
begin
  Events := GetValidEvents;

  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlDispose);
  InstallControlEventHandler(Widget,
    RegisterEventHandler(@CarbonCommon_Dispose),
    1, @TmpSpec, Pointer(Self), nil);

  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlDraw);
  InstallControlEventHandler(Content,
    RegisterEventHandler(@CarbonCommon_Draw),
    1, @TmpSpec, Pointer(Self), nil);

  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlBoundsChanged);
  InstallControlEventHandler(Frame,
    RegisterEventHandler(@CarbonCommon_BoundsChanged),
    1, @TmpSpec, Pointer(Self), nil);
    
  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlContextualMenuClick);
  InstallControlEventHandler(Widget,
    RegisterEventHandler(@CarbonCommon_ContextualMenuClick),
    1, @TmpSpec, Pointer(Self), nil);
    
  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlTrack);
  InstallControlEventHandler(Widget,
    RegisterEventHandler(@CarbonCommon_Track),
    1, @TmpSpec, Pointer(Self), nil);
    
  if Content <> ControlRef(Widget) then
  begin
    TmpSpec := MakeEventSpec(kEventClassControl, kEventControlTrack);
    InstallControlEventHandler(Content,
      RegisterEventHandler(@CarbonCommon_Track),
      1, @TmpSpec, Pointer(Self), nil);
  end;

  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlSetFocusPart);
  InstallControlEventHandler(Content,
    RegisterEventHandler(@CarbonCommon_SetFocusPart),
    1, @TmpSpec, Pointer(Self), nil);

  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlGetNextFocusCandidate);
  InstallControlEventHandler(Content,
    RegisterEventHandler(@CarbonCommon_GetNextFocusCandidate),
    1, @TmpSpec, Pointer(Self), nil);
    
// cursor set
  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlSetCursor);
  InstallControlEventHandler(Widget,
    RegisterEventHandler(@CarbonCommon_SetCursor),
    1, @TmpSpec, Pointer(Self), nil);

  if cceHit in Events then
  begin
    TmpSpec := MakeEventSpec(kEventClassControl, kEventControlHit);
    InstallControlEventHandler(Widget,
      RegisterEventHandler(@CarbonControl_Hit),
      1, @TmpSpec, Pointer(Self), nil);
  end;

  if cceValueChanged in Events then
  begin
    TmpSpec := MakeEventSpec(kEventClassControl, kEventControlValueFieldChanged);
    InstallControlEventHandler(Widget,
      RegisterEventHandler(@CarbonControl_ValueChanged),
      1, @TmpSpec, Pointer(Self), nil);
  end;

  if cceIndicatorMoved in Events then
  begin
    TmpSpec := MakeEventSpec(kEventClassControl, kEventControlIndicatorMoved);
    InstallControlEventHandler(Widget,
      RegisterEventHandler(@CarbonControl_IndicatorMoved),
      1, @TmpSpec, Pointer(Self), nil);
  end;

  {$IFDEF VerboseControlEvent}
    DebugLn('TCarbonControl.RegisterEvents ', ClassName, ' ',
      LCLObject.Name, ': ', LCLObject.ClassName);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.UnregisterEvents

  Unregisters event handlers
 ------------------------------------------------------------------------------}
procedure TCarbonControl.UnregisterEvents;
var
  Events: TCarbonControlEvents;
begin
  Events := GetValidEvents;

  UnregisterEventHandler(@CarbonCommon_Dispose);
  UnregisterEventHandler(@CarbonCommon_Draw);
  UnregisterEventHandler(@CarbonCommon_BoundsChanged);
  UnregisterEventHandler(@CarbonCommon_ContextualMenuClick);
  UnregisterEventHandler(@CarbonCommon_Track);
  if Content <> ControlRef(Widget) then
    UnregisterEventHandler(@CarbonCommon_Track);
    
  UnregisterEventHandler(@CarbonCommon_SetFocusPart);
  UnregisterEventHandler(@CarbonCommon_GetNextFocusCandidate);
  UnregisterEventHandler(@CarbonCommon_SetCursor);
  
  if cceHit in Events then
    UnregisterEventHandler(@CarbonControl_Hit);
  if cceValueChanged in Events then
    UnregisterEventHandler(@CarbonControl_ValueChanged);
  if cceIndicatorMoved in Events then
    UnregisterEventHandler(@CarbonControl_IndicatorMoved);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.CreateWidget
  Params:  AParams - Creation parameters

  Override to provide Carbon control creation
 ------------------------------------------------------------------------------}
procedure TCarbonControl.CreateWidget(const AParams: TCreateParams);
begin
  OSError(
    SetControlProperty(Widget, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Self), @Self),
    Self, SCreateWidget, SSetControlProp, 'Widget');
    
  if Content <> ControlRef(Widget) then
    OSError(
      SetControlProperty(Content, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Self), @Self),
      SCreateWidget, SSetControlProp, 'Content');
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.DestroyWidget

  Override to do some clean-up
 ------------------------------------------------------------------------------}
procedure TCarbonControl.DestroyWidget;
begin
  DisposeControl(ControlRef(Widget));
  Widget := nil;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.GetContent
  Returns: Content area control
 ------------------------------------------------------------------------------}
function TCarbonControl.GetContent: ControlRef;
begin
  Result := ControlRef(Widget);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.GetFrame
  Returns: Frame area control
 ------------------------------------------------------------------------------}
function TCarbonControl.GetFrame: ControlRef;
begin
  Result := ControlRef(Widget);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.GetForceEmbedInScrollView
  Returns: Whether use scroll view even if no scroll bars are needed
 ------------------------------------------------------------------------------}
function TCarbonControl.GetForceEmbedInScrollView: Boolean;
begin
  Result := False;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.EmbedInScrollView
  Params:  AParams     - Creation parameters
  Returns: Scroll view
  
  Should be called right after widget creation only
 ------------------------------------------------------------------------------}
function TCarbonControl.EmbedInScrollView(const AParams: TCreateParams): HIViewRef;
var
 ScrollBars: TScrollStyle;
begin
  case AParams.Style and (WS_VSCROLL or WS_HSCROLL) of
  WS_VSCROLL: ScrollBars := ssAutoVertical;
  WS_HSCROLL: ScrollBars := ssAutoHorizontal;
  WS_VSCROLL or WS_HSCROLL: ScrollBars := ssAutoBoth;
  else
    ScrollBars := ssNone;
  end;
  
  Result := EmbedInScrollView(ScrollBars);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.EmbedInScrollView
  Params:  AScrollBars - Scroll style
  Returns: Scroll view

  Should be called right after widget creation only
 ------------------------------------------------------------------------------}
function TCarbonControl.EmbedInScrollView(AScrollBars: TScrollStyle): HIViewRef;
var
  ScrollOptions: FPCMacOSAll.OptionBits;
const
  SName = 'EmbedInScrollView';
begin
  Result := nil;
  
  if (not GetForceEmbedInScrollView) and (AScrollBars = ssNone) then
  begin
    Result := Widget;
    Exit;
  end;

  case AScrollBars of
    ssNone, ssBoth, ssAutoBoth:
      ScrollOptions := kHIScrollViewOptionsVertScroll or
        kHIScrollViewOptionsHorizScroll;
    ssVertical, ssAutoVertical:
      ScrollOptions := kHIScrollViewOptionsVertScroll;
    ssHorizontal, ssAutoHorizontal:
      ScrollOptions := kHIScrollViewOptionsHorizScroll;
  end;

  if OSError(HIScrollViewCreate(ScrollOptions, Result), Self, SName,
    'HIScrollViewCreate') then Exit;
  
  OSError(HIScrollViewSetScrollBarAutoHide(Result,
      AScrollBars in [ssNone, ssAutoVertical, ssAutoHorizontal, ssAutoBoth]),
    Self, SName, SViewSetScrollBarAutoHide);
    
  OSError(HIViewSetVisible(Widget, True), Self, SName, SViewVisible);
  if OSError(HIViewAddSubview(Result, Widget), Self, SName, SViewAddView) then Exit;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.ChangeScrollBars
  Params:  AScrollView - Scroll view
           AScrollBars - Actual scroll style
           ANewValue   - New scroll style

  Changes scroll style of scroll view
 ------------------------------------------------------------------------------}
procedure TCarbonControl.ChangeScrollBars(AScrollView: HIViewRef;
  var AScrollBars: TScrollStyle; ANewValue: TScrollStyle);
begin
  if ANewValue <> AScrollBars then
  begin
    if (not GetForceEmbedInScrollView) and (ANewValue = ssNone) or
      (AScrollBars = ssNone) then
    begin
      RecreateWnd(LCLObject);
      Exit;
    end;
    
    if ((ANewValue in [ssNone, ssBoth, ssAutoBoth]) and
      (AScrollBars in [ssNone, ssBoth, ssAutoBoth])) or
      ((ANewValue in [ssVertical, ssAutoVertical]) and
      (AScrollBars in [ssVertical, ssAutoVertical])) or
      ((ANewValue in [ssHorizontal, ssAutoHorizontal]) and
      (AScrollBars in [ssHorizontal, ssAutoHorizontal])) then
    begin
      AScrollBars := ANewValue;

      OSError(HIScrollViewSetScrollBarAutoHide(AScrollView,
          AScrollBars in [ssNone, ssAutoVertical, ssAutoHorizontal, ssAutoBoth]),
        Self, 'ChangeScrollBars', SViewSetScrollBarAutoHide);
    end
    else
      RecreateWnd(LCLObject);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.GetValue
  Returns: The value of Carbon control
 ------------------------------------------------------------------------------}
function TCarbonControl.GetValue: Integer;
begin
  Result := GetControl32BitValue(ControlRef(Widget));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.SetValue
  Params:  AValue - New control value

  Sets the Carbon control value
 ------------------------------------------------------------------------------}
procedure TCarbonControl.SetValue(AValue: Integer);
begin
  SetControl32BitValue(ControlRef(Widget), AValue);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.SetMinimum
  Params:  AValue - New control minimum

  Sets the Carbon control minimum
 ------------------------------------------------------------------------------}
procedure TCarbonControl.SetMinimum(AValue: Integer);
begin
  SetControl32BitMinimum(ControlRef(Widget), AValue);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.SetMaximum
  Params:  AValue - New control maximum

  Sets the Carbon control maximum
 ------------------------------------------------------------------------------}
procedure TCarbonControl.SetMaximum(AValue: Integer);
begin
  SetControl32BitMaximum(ControlRef(Widget), AValue);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.SetViewSize
  Params:  AValue - New control view size

  Sets the Carbon control view size
 ------------------------------------------------------------------------------}
procedure TCarbonControl.SetViewSize(AValue: Integer);
begin
  SetControlViewSize(ControlRef(Widget), AValue);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.Invalidate
  Params:  Rect - Pointer to rect (optional)

  Invalidates the specified client rect or entire area of control
 ------------------------------------------------------------------------------}
procedure TCarbonControl.Invalidate(Rect: PRect);
begin
  if Rect = nil then
    OSError(
      HiViewSetNeedsDisplay(Frame, True), Self, SInvalidate, SViewNeedsDisplay)
  else
    OSError(
      HiViewSetNeedsDisplayInRect(Content, RectToCGRect(Rect^), True), Self,
      SInvalidate, SViewNeedsDisplayRect);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.IsEnabled
  Returns: If control is enabled
 ------------------------------------------------------------------------------}
function TCarbonControl.IsEnabled: Boolean;
begin
  Result := IsControlEnabled(Frame);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.IsVisible
  Returns: If control is visible
 ------------------------------------------------------------------------------}
function TCarbonControl.IsVisible: Boolean;
begin
  Result := FPCMacOSAll.IsControlVisible(Frame);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.Enable
  Params:  AEnable - if enable
  Returns: If control is enabled
  
  Changes control enabled
 ------------------------------------------------------------------------------}
function TCarbonControl.Enable(AEnable: Boolean): Boolean;
begin
  Result := not FPCMacOSAll.IsControlEnabled(Frame);

  if AEnable then
    OSError(FPCMacOSAll.EnableControl(Frame), Self, SEnable, SEnableControl)
  else
    OSError(FPCMacOSAll.DisableControl(Frame), Self, SEnable, SDisableControl);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.GetBounds
  Params:  ARect - Record for control coordinates
  Returns: If function succeeds

  Returns the control bounding rectangle relative to the client origin of its
  parent
 ------------------------------------------------------------------------------}
function TCarbonControl.GetBounds(var ARect: TRect): Boolean;
var
  AWndRect: FPCMacOSAll.Rect;
begin
  Result := FPCMacOSAll.GetControlBounds(Frame, AWndRect) <> nil;
  if Result then
    ARect := CarbonRectToRect(AWndRect)
  else
    DebugLn('TCarbonControl.GetBounds failed for ' + DbgSName(LCLObject));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.GetScreenBounds
  Params:  ARect - Record for control coordinates
  Returns: If function succeeds

  Returns the control bounding rectangle relative to the screen
 ------------------------------------------------------------------------------}
function TCarbonControl.GetScreenBounds(var ARect: TRect): Boolean;
var
  BoundsRect: HIRect;
  WindowRect: FPCMacOSAll.Rect;
const
  SName = 'GetScreenBounds';
begin
  Result := False;

  if OSError(HIViewGetBounds(Frame, BoundsRect), Self, SName, 'HIViewGetBounds') then Exit;
  if OSError(HIViewConvertRect(BoundsRect, Frame, nil), Self, SName,
    'HIViewConvertRect') then Exit;
  
  if OSError(GetWindowBounds(GetTopParentWindow, kWindowStructureRgn,
      WindowRect), Self, SName, SGetWindowBounds) then Exit;
    
  ARect := CGRectToRect(BoundsRect);
  OffsetRect(ARect, WindowRect.left, WindowRect.top);

  Result := True;
end;


{------------------------------------------------------------------------------
  Method:  TCarbonControl.SetBounds
  Params:  ARect - Record for control coordinates
  Returns: If function succeeds

  Sets the control bounding rectangle relative to the client origin of its
  parent
 ------------------------------------------------------------------------------}
function TCarbonControl.SetBounds(const ARect: TRect): Boolean;
var
  R: TRect;
begin
  Result := False;
  FPCMacOSAll.SetControlBounds(Frame, GetCarbonRect(ARect));
  
  if Content <> ControlRef(Widget) then
  begin // adjust content area
    if not GetClientRect(R) then
    begin
      DebugLn('TCarbonControl.SetBounds Error - unable to get client area!');
      Exit;
    end;
    FPCMacOSAll.SetControlBounds(Content, GetCarbonRect(R));
  end;
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.SetChildZPosition
  Params:  AChild      - Child widget
           AOldPos     - Old z position
           ANewPos     - New z position
           AChildren   - List of all child controls

  Sets the child z position of Carbon widget
 ------------------------------------------------------------------------------}
procedure TCarbonControl.SetChildZPosition(AChild: TCarbonWidget;
  const AOldPos, ANewPos: Integer; const AChildren: TFPList);
var
  RefView: HIViewRef;
  Order: HIViewZOrderOp;
  I, StopPos: Integer;
  Child: TWinControl;
begin
  RefView := nil;
  
  if ANewPos <= 0 then // send behind all
    Order := kHIViewZOrderBelow
  else
    if ANewPos >= Pred(AChildren.Count) then // bring to front of all
      Order := kHIViewZOrderAbove
    else // custom position
    begin
      // Search for the first child above us with a handle.
      // The child list is reversed form the windows order.
      // If we don't find an allocated handle then exit.

      if AOldPos > ANewPos then
        StopPos := AOldPos // the child is moved to the bottom
      else
        StopPos := Pred(AChildren.Count); // the child is moved to the top

      for I := Succ(ANewPos) to StopPos do
      begin
        Child := TWinControl(AChildren[I]);
        
        if Child.HandleAllocated then
        begin
          RefView := ControlRef(TCarbonWidget(Child.Handle).Widget);
          Order := kHIViewZOrderBelow;
          Break;
        end;
      end;

      if RefView = nil then Exit;
    end;

  OSError(HIViewSetZOrder(ControlRef(AChild.Widget), Order, RefView),
    Self, 'SetChildZPosition', 'HIViewSetZOrder');
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.SetFocus

  Sets the focus to control
 ------------------------------------------------------------------------------}
procedure TCarbonControl.SetFocus;
var
  Window: WindowRef;
  Control: ControlRef;
begin
  Window := GetTopParentWindow;
  
  OSError(
    SetUserFocusWindow(Window), Self, SSetFocus, SSetUserFocusWindow);

  OSError(GetKeyboardFocus(Window, Control), Self, SSetFocus, SGetKeyboardFocus);

  if Control <> ControlRef(Widget) then
    OSError(SetKeyboardFocus(Window, ControlRef(Widget), kControlFocusNextPart),
    Self, SSetFocus, 'SetKeyboardFocus');
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.SetColor
  Params:  AColor - New color

  Sets the color of control (for edit like controls)
 ------------------------------------------------------------------------------}
procedure TCarbonControl.SetColor(const AColor: TColor);
var
  FontStyle: ControlFontStyleRec;
begin
  // get current font style preserve other font settings
  OSError(GetControlData(ControlRef(Widget), kControlEntireControl,
      kControlFontStyleTag, SizeOf(FontStyle), @FontStyle, nil), Self, SSetColor,
    SGetData, SControlFont);

  FontStyle.flags := FontStyle.flags or kControlUseBackColorMask;
  FontStyle.backColor := ColorToRGBColor(AColor);

  OSError(SetControlFontStyle(ControlRef(Widget), FontStyle), Self, SSetColor,
    SSetFontStyle);
    
  // invalidate control
  Invalidate;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.SetFont
  Params:  AFont - New font

  Sets the font of control
 ------------------------------------------------------------------------------}
procedure TCarbonControl.SetFont(const AFont: TFont);
var
  FontStyle: ControlFontStyleRec;
  ID: ATSUFontID;
const
  SName = 'SetFont';
begin
  // get current font style to preserve other font settings
  OSError(GetControlData(ControlRef(Widget), kControlEntireControl,
      kControlFontStyleTag, SizeOf(FontStyle), @FontStyle, nil), Self, SName,
    SGetData, SControlFont);
    
  ID := FindCarbonFontID(AFont.Name);
  

  FontStyle.flags := FontStyle.flags or kControlUseFontMask or kControlUseSizeMask or
    kControlUseFaceMask or kControlUseForeColorMask;
    
  if ID = 0 then         // use default font
    FontStyle.flags := FontStyle.flags and not kControlUseFontMask;
  if AFont.Size = 0 then // use default size
    FontStyle.flags := FontStyle.flags and not kControlUseSizeMask;

  FontStyle.font := ID;
  FontStyle.size := AFont.Size;
  FontStyle.style := FontStyleToQDStyle(AFont.Style);
  FontStyle.foreColor := ColorToRGBColor(AFont.Color);

  
  OSError(SetControlFontStyle(ControlRef(Widget), FontStyle), Self, SName,
    SSetFontStyle);
    
  // invalidate control
  Invalidate;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.ShowHide
  Params:  AVisible - if show

  Shows or hides control
 ------------------------------------------------------------------------------}
procedure TCarbonControl.ShowHide(AVisible: Boolean);
begin
  OSError(HIViewSetVisible(Frame, AVisible), Self, 'ShowHide', SViewVisible);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.GetText
  Params:  S - Text
  Returns: If the function succeeds

  Gets the text or caption of control
 ------------------------------------------------------------------------------}
function TCarbonControl.GetText(var S: String): Boolean;
begin
  Result := False; // control caption is static, edit controls override this
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.SetText
  Params:  S - New text
  Returns: If the function succeeds

  Sets the text or caption of control
 ------------------------------------------------------------------------------}
function TCarbonControl.SetText(const S: String): Boolean;
var
  CFString: CFStringRef;
  T: String;
begin
  Result := False;
  
  T := S;
  DeleteAmpersands(T);
  
  CreateCFString(T, CFString);
  try
    if OSError(HIViewSetText(HIViewRef(Widget), CFString), Self, SSetText,
      'HIViewSetText') then Exit;
      
    Result := True;
  finally
    FreeCFString(CFString);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.Update
  Returns: If the function succeeds

  Updates control
 ------------------------------------------------------------------------------}
function TCarbonControl.Update: Boolean;
begin
  Result := False;
  if OSError(HIViewRender(HIViewRef(Widget)), Self, 'Update', 'HIViewRender') then Exit;
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.GetTopParentWindow
  Returns: Retrieves the parent window reference of the Carbon control
 ------------------------------------------------------------------------------}
function TCarbonControl.GetTopParentWindow: WindowRef;
var
  Window: TControl;
begin
  Window := LCLObject.GetTopParent;

  if Window is TCustomForm then
    Result := TCarbonWindow((Window as TWinControl).Handle).Widget
  else Result := nil;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.GetThemeDrawState
  Returns: The control widget actual theme draw state (active, ...)
 ------------------------------------------------------------------------------}
function TCarbonControl.GetThemeDrawState: ThemeDrawState;
begin
  if IsControlActive(ControlRef(Widget)) then
  begin
    if IsControlHilited(ControlRef(Widget)) then Result := kThemeStatePressed
    else Result := kThemeStateActive;
  end
  else Result := kThemeStateInactive;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.GetMousePos
  Returns: The position of mouse cursor in local coordinates
 ------------------------------------------------------------------------------}
function TCarbonControl.GetMousePos: TPoint;
var
  P: FPCMacOSAll.Point;
  MousePoint: HIPoint;
  R: FPCMacOSAll.Rect;
const
  SName = 'GetMousePos';
begin
  GetGlobalMouse(P);
  
  OSError(GetWindowBounds(GetTopParentWindow, kWindowStructureRgn, R),
    Self, SName, SGetWindowBounds);
  MousePoint.X := P.h - R.left;
  MousePoint.Y := P.v - R.top;
  
  OSError(HIViewConvertPoint(MousePoint, nil, Content), Self, SName, SViewConvert);
  Result.X := Trunc(MousePoint.X);
  Result.Y := Trunc(MousePoint.Y);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.GetClientRect
  Params:  ARect - Record for client area coordinates
  Returns: If the function succeeds

  Returns the control client rectangle relative to the control origin
 ------------------------------------------------------------------------------}
function TCarbonControl.GetClientRect(var ARect: TRect): Boolean;
var
  AClientRect: FPCMacOSAll.Rect;
  ClientRegion: FPCMacOSAll.RgnHandle;
  R: OSStatus;
begin
  Result := False;
  //DebugLn('TCarbonControl.GetClientRect ' + LCLObject.Name);

  ClientRegion := FPCMacOSAll.NewRgn();
  try
    R := GetControlRegion(ControlRef(Widget), kControlContentMetaPart, ClientRegion);
    case R of
      errInvalidPartCode:
        begin
          // controls without content area have clientrect = boundsrect
          Result := FPCMacOSAll.GetControlBounds(ControlRef(Widget), AClientRect) <> nil;
          if Result then
          begin
            ARect := CarbonRectToRect(AClientRect);
            OffsetRect(ARect, -ARect.Left, -ARect.Top);
          end;
        end;
      noErr:
        begin
          Result := GetRegionBounds(ClientRegion, AClientRect) <> nil;
          if Result then ARect := CarbonRectToRect(AClientRect);
          //DebugLn('TCarbonControl.GetClientRect ' + LCLObject.Name + ' ' + DbgS(Result));
        end;
      else
        OSError(R, Self, 'GetClientRect', 'GetControlRegion');
    end;
  finally
    FPCMacOSAll.DisposeRgn(ClientRegion);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.GetPreferredSize
  Returns: The preffered size of control for autosizing or (0, 0)
 ------------------------------------------------------------------------------}
function TCarbonControl.GetPreferredSize: TPoint;
var
  R: FPCMacOSAll.Rect;
  S: SmallInt;
begin
  Result.X := 0;
  Result.Y := 0;
    
  R := GetCarbonRect(0, 0, 0, 0);
  
  if OSError(GetBestControlRect(ControlRef(Widget), R, S), Self,
    'GetPreferredSize', 'GetBestControlRect') then Exit;
  
  Result.X := R.right - R.left;
  Result.Y := R.bottom - R.top;
end;

