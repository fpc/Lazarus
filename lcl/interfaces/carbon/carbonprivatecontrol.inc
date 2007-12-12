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
  
  NotifyApplicationUserInput(SavedMouseUpMsg.Msg);
  CarbonWidgetSet.SetCaptureWidget(0); // capture is released
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

{------------------------------------------------------------------------------
  Name:  CarbonControl_CommandProcess
  Handles copy/paste... commands
 ------------------------------------------------------------------------------}
function CarbonControl_CommandProcess(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  Command: HICommandExtended;
const SName = 'CarbonControl_CommandProcess';
begin
  {$IFDEF VerboseControlEvent}
    DebugLn('CarbonControl_CommandProcess ' + DbgSName(AWidget.LCLObject));
  {$ENDIF}

  if not OSError(
    GetEventParameter(AEvent, kEventParamDirectObject,
      typeHICommand, nil, SizeOf(HICommand), nil, @Command),
    SName, 'GetEventParameter') then
  begin
    {$IFDEF VerboseControlEvent}
      DebugLn('CarbonControl_CommandProcess ID: ' + DbgS(Command.commandID) + ' Target: ' +
        DbgSName(AWidget.LCLObject));
    {$ENDIF}

    case Command.commandID of
      kHICommandUndo:;
      kHICommandRedo:;
      kHICommandCut: SendSimpleMessage(AWidget.LCLObject, LM_CUTTOCLIP);
      kHICommandCopy: SendSimpleMessage(AWidget.LCLObject, LM_COPYTOCLIP);
      kHICommandPaste: SendSimpleMessage(AWidget.LCLObject, LM_PASTEFROMCLIP);
      kHICommandClear: SendSimpleMessage(AWidget.LCLObject, LM_CLEARSEL);
      kHICommandSelectAll:;
    end;
  end;

  Result := CallNextEventHandler(ANextHandler, AEvent);
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
var
  I: Integer;
begin
  // add frame to parent content area
  for I := 0 to GetFrameCount - 1 do
  begin
    OSError(HIViewSetVisible(Frames[I], LCLObject.Visible), Self, 'AddToWidget', SViewVisible);
    OSError(HIViewAddSubview(AParent.Content, Frames[I]), Self, 'AddToWidget',
      SViewAddView);
  end;
  
  AParent.ControlAdded;
    
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
  InstallControlEventHandler(Frames[0],
    RegisterEventHandler(@CarbonCommon_BoundsChanged),
    1, @TmpSpec, Pointer(Self), nil);

  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlSetFocusPart);
  InstallControlEventHandler(Content,
    RegisterEventHandler(@CarbonCommon_SetFocusPart),
    1, @TmpSpec, Pointer(Self), nil);

  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlGetNextFocusCandidate);
  InstallControlEventHandler(Content,
    RegisterEventHandler(@CarbonCommon_GetNextFocusCandidate),
    1, @TmpSpec, Pointer(Self), nil);
    
  TmpSpec := MakeEventSpec(kEventClassCommand, kEventCommandProcess);
  InstallControlEventHandler(Widget,
    RegisterEventHandler(@CarbonControl_CommandProcess),
    1, @TmpSpec, Pointer(Self), nil);
    
// cursor set
  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlSetCursor);
  InstallControlEventHandler(Content,
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
  Method:  TCarbonControl.CreateWidget
  Params:  AParams - Creation parameters

  Override to provide Carbon control creation
 ------------------------------------------------------------------------------}
procedure TCarbonControl.CreateWidget(const AParams: TCreateParams);
begin
  AddControlPart(Widget);
  if Content <> ControlRef(Widget) then AddControlPart(Content);
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
  Method:  TCarbonControl.AddControlPart
  Params:  AControl - Control

  Adds control part of composite control
 ------------------------------------------------------------------------------}
procedure TCarbonControl.AddControlPart(const AControl: ControlRef);
var
  TmpSpec: EventTypeSpec;
begin
  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlTrack);
  InstallControlEventHandler(AControl,
    RegisterEventHandler(@CarbonCommon_Track),
    1, @TmpSpec, Pointer(Self), nil);
    
  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlContextualMenuClick);
  InstallControlEventHandler(AControl,
    RegisterEventHandler(@CarbonCommon_ContextualMenuClick),
    1, @TmpSpec, Pointer(Self), nil);
    
  OSError(
    SetControlProperty(AControl, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Self), @Self),
    Self, 'AddControlPart', SSetControlProp);
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
  Method:  TCarbonControl.GetControlContentRect
  Params:  ARect - Content area rectangle
  Returns: If the function succeeds
 ------------------------------------------------------------------------------}
function TCarbonControl.GetControlContentRect(var ARect: TRect): Boolean;
var
  AClientRect: FPCMacOSAll.Rect;
  ClientRegion: FPCMacOSAll.RgnHandle;
begin
  Result := False;

  ClientRegion := FPCMacOSAll.NewRgn();
  try
    if OSError(GetControlRegion(ControlRef(Widget), kControlContentMetaPart, ClientRegion),
      Self, 'GetControlContentRect', 'GetControlRegion') then Exit;

     Result := GetRegionBounds(ClientRegion, AClientRect) <> nil;
     if Result then ARect := CarbonRectToRect(AClientRect);
  finally
    FPCMacOSAll.DisposeRgn(ClientRegion);
  end;
  
  {$IFDEF VerboseBounds}
    DebugLn('TCarbonControl.GetControlContentRect ' + LCLObject.Name + ' ' + DbgS(ARect) +
      ' ' + DbgS(Result));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.GetFrame
  Params:  Frame index
  Returns: Frame area control
 ------------------------------------------------------------------------------}
function TCarbonControl.GetFrame(Index: Integer): ControlRef;
begin
  Result := ControlRef(Widget);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.GetFrameCount
  Returns: Count of control frames
 ------------------------------------------------------------------------------}
class function TCarbonControl.GetFrameCount: Integer;
begin
  Result := 1;
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
  Method:  TCarbonControl.UpdateContentBounds

  Updates bounds of content area
 ------------------------------------------------------------------------------}
function TCarbonControl.UpdateContentBounds: Boolean;
var
  R: TRect;
begin
  Result := False;
  
  if not GetClientRect(R) then
  begin
    DebugLn('TCarbonControl.UpdateContentBounds Error - unable to get client area!');
    Exit;
  end;
  
  {$IFDEF VerboseBounds}
    DebugLn('TCarbonControl.UpdateContentBounds ' + DbgS(R));
  {$ENDIF}
  if OSError(HIViewSetFrame(Content, RectToCGRect(R)),
    Self, 'UpdateContentBounds', SViewFrame) then Exit;

  Result := True;
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
  Bounds: HIRect;
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
    
  // set scroll view bounds
  OSError(HIViewGetFrame(Widget, Bounds), Self, SName, 'HIViewGetFrame');
  OSError(HIViewSetFrame(Result, Bounds), Self, SName, SViewFrame);

  OSError(HIScrollViewSetScrollBarAutoHide(Result,
      AScrollBars in [ssNone, ssAutoVertical, ssAutoHorizontal, ssAutoBoth]),
    Self, SName, SViewSetScrollBarAutoHide);
    
  OSError(HIViewAddSubview(Result, Widget), Self, SName, SViewAddView);
  OSError(HIViewSetVisible(Widget, True), Self, SName, SViewVisible);
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
var
  I: Integer;
begin
  if Rect = nil then
  begin
    for I := 0 to GetFrameCount - 1 do
      OSError(
        HiViewSetNeedsDisplay(Frames[I], True), Self, SInvalidate, SViewNeedsDisplay);
  end
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
  Result := IsControlEnabled(Frames[0]);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.IsVisible
  Returns: If control is visible
 ------------------------------------------------------------------------------}
function TCarbonControl.IsVisible: Boolean;
begin
  Result := FPCMacOSAll.IsControlVisible(Frames[0]);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.Enable
  Params:  AEnable - if enable
  Returns: If control is enabled
  
  Changes control enabled
 ------------------------------------------------------------------------------}
function TCarbonControl.Enable(AEnable: Boolean): Boolean;
var
  I: Integer;
begin
  Result := not FPCMacOSAll.IsControlEnabled(Frames[0]);

  if AEnable then
  begin
    for I := 0 to GetFrameCount - 1 do
      OSError(FPCMacOSAll.EnableControl(Frames[I]), Self, SEnable, SEnableControl);
  end
  else
    for I := 0 to GetFrameCount - 1 do
      OSError(FPCMacOSAll.DisableControl(Frames[I]), Self, SEnable, SDisableControl);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.GetFrameBounds
  Params:  ARect - TRect
  Returns: If function succeeds

  Returns the control bounding rectangle relative to the client origin of its
  parent
 ------------------------------------------------------------------------------}
function TCarbonControl.GetFrameBounds(var ARect: TRect): Boolean;
var
  BoundsRect: FPCMacOSAll.Rect;
begin
  Result := False;

  if GetControlBounds(Frames[0], BoundsRect) = nil then
  begin
    DebugLn('TCarbonControl.GetFrameBounds failed!');
    Exit;
  end;
  
  ARect := CarbonRectToRect(BoundsRect);
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.GetBounds
  Params:  ARect - Record for control coordinates
  Returns: If function succeeds

  Returns the control bounding rectangle relative to the client origin of its
  parent
 ------------------------------------------------------------------------------}
function TCarbonControl.GetBounds(var ARect: TRect): Boolean;
begin
  Result := GetFrameBounds(ARect);
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

  if not GetFrameBounds(ARect) then Exit;
  BoundsRect := RectToCGRect(ARect);
  BoundsRect.origin.x := 0;
  BoundsRect.origin.y := 0;
  if OSError(HIViewConvertRect(BoundsRect, Frames[0], nil), Self, SName,
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
begin
  Result := False;
  if OSError(HIViewSetFrame(Frames[0], RectToCGRect(ARect)),
    Self, SSetBounds, SViewFrame) then Exit;
  // ensure bounds are send back to LCL once after creation
  if not BoundsReported then BoundsChanged;
  
  Result := True;
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
  Method:  TCarbonControl.SetZOrder
  Params:  AOrder     - Order
           ARefWidget - Reference widget

  Sets the Z order of control
 ------------------------------------------------------------------------------}
procedure TCarbonControl.SetZOrder(AOrder: HIViewZOrderOp;
  ARefWidget: TCarbonWidget);
var
  RefView: HIViewRef;
const
  SName = 'SetZOrder';
begin
  if ARefWidget = nil then
    RefView := nil
  else
    if AOrder = kHIViewZOrderBelow then
      RefView := TCarbonControl(ARefWidget).Frames[0]
    else
      RefView := TCarbonControl(ARefWidget).Frames[GetFrameCount - 1];

  OSError(HIViewSetZOrder(Frames[0], AOrder, RefView),
    Self, SName, 'HIViewSetZOrder');

  if GetFrameCount = 2 then // second frame is allways above first
    OSError(HIViewSetZOrder(Frames[1], kHIViewZOrderAbove, Frames[0]),
      Self, SName, 'HIViewSetZOrder', 'Frames[1]');
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.ShowHide
  Params:  AVisible - if show

  Shows or hides control
 ------------------------------------------------------------------------------}
procedure TCarbonControl.ShowHide(AVisible: Boolean);
var
  I: Integer;
begin
  //DebugLn('TCarbonControl.ShowHide ' + DbgSName(LCLobject),' ', DbgS(AVisible));
  for I := 0 to GetFrameCount - 1 do
    OSError(HIViewSetVisible(Frames[I], AVisible or (csDesigning in LCLobject.ComponentState)),
      Self, 'ShowHide', SViewVisible);
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
  R: HIRect;
begin
  Result := False;

  // controls without content area have clientrect = boundsrect
  if OSError(HIViewGetFrame(Content, R),
    Self, 'GetClientRect', 'HIViewGetFrame') then Exit;

  ARect := CGrectToRect(R);
  OffsetRect(ARect, -ARect.Left, -ARect.Top);
  Result := True;

  {$IFDEF VerboseBounds}
    DebugLn('TCarbonControl.GetClientRect ' + LCLObject.Name + ' ' + DbgS(ARect) +
      ' ' + DbgS(Result));
  {$ENDIF}
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

