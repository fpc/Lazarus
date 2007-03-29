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
  DebugLn('CarbonControl_Hit: ', DbgSName(AWidget.LCLObject));
  Result := CallNextEventHandler(ANextHandler, AEvent);

  if GetEventParameter(AEvent, kEventParamControlPart, typeControlPartCode, nil,
    SizeOf(ControlPartCode), nil, @ControlPart) = noErr then
  begin
    (AWidget as TCarbonControl).Hit(ControlPart);
  end;

  DeliverMessage(AWidget.LCLObject, SavedMouseUpMsg);
end;

function CarbonControl_ValueChanged(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  DebugLn('CarbonControl_ValueChanged ', DbgSName(AWidget.LCLObject));
  Result := CallNextEventHandler(ANextHandler, AEvent);

  (AWidget as TCarbonControl).ValueChanged;
end;

function CarbonControl_IndicatorMoved(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  DebugLn('CarbonControl_IndicatorMoved ', DbgSName(AWidget.LCLObject));
  Result := CallNextEventHandler(ANextHandler, AEvent);
  
  (AWidget as TCarbonControl).IndicatorMoved;
end;

function CarbonControl_TextDidChange(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  DebugLn('CarbonControl_TextDidChange: ', DbgSName(AWidget.LCLObject));
  Result := CallNextEventHandler(ANextHandler, AEvent);

  (AWidget as TCarbonControl).TextDidChange;
end;

function CarbonControl_ListItemSelected(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  Index: CFIndex;
begin
  DebugLn('CarbonControl_ListItemSelected: ', DbgSName(AWidget.LCLObject));
  Result := CallNextEventHandler(ANextHandler, AEvent);
  
  // get selected item index
  if GetEventParameter(AEvent, kEventParamComboBoxListSelectedItemIndex,
    typeCFIndex, nil, SizeOf(CFIndex), nil, @Index) <> noErr then Index := -1;

  (AWidget as TCarbonControl).ListItemSelected(Index);
end;

{ TCarbonControl }

{------------------------------------------------------------------------------
  Method:  TCarbonControl.GetValidEvents
  Returns: Set of events with installed handlers

  Returns the set of events with installed handlers
 ------------------------------------------------------------------------------}
class function TCarbonControl.GetValidEvents: TCarbonControlEvents;
begin
  Result := [];
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.Hit
  Returns: Nothing

  Hit event handler
 ------------------------------------------------------------------------------}
procedure TCarbonControl.Hit(AControlPart: ControlPartCode);
begin
  DebugLn('TCarbonControl.Hit is invalid ', ClassName, ' ',
    DbgSName(LCLObject), ': ', LCLObject.ClassName);
end;

procedure TCarbonControl.Draw;
begin
  DebugLn('TCarbonControl.Draw is invalid ', ClassName, ' ',
    DbgSName(LCLObject), ': ', LCLObject.ClassName);
end;

procedure TCarbonControl.ValueChanged;
begin
  DebugLn('TCarbonControl.ValueChanged is invalid ', ClassName, ' ',
    DbgSName(LCLObject), ': ', LCLObject.ClassName);
end;

procedure TCarbonControl.IndicatorMoved;
begin
  DebugLn('TCarbonControl.IndicatorMoved is invalid ', ClassName, ' ',
    DbgSName(LCLObject), ': ', LCLObject.ClassName);
end;

procedure TCarbonControl.TextDidChange;
begin
  DebugLn('TCarbonControl.TextDidChange is invalid! ', ClassName, ' ',
    DbgSName(LCLObject), ': ', LCLObject.ClassName);
end;

procedure TCarbonControl.DoAction(AControlPart: ControlPartCode);
begin
  DebugLn('TCarbonControl.DoAction is invalid ', ClassName, ' ',
    DbgSName(LCLObject), ': ', LCLObject.ClassName);
end;

procedure TCarbonControl.ListItemSelected(AIndex: Integer);
begin
  DebugLn('TCarbonControl.ListItemSelected is invalid ', ClassName, ' ',
    DbgSName(LCLObject), ': ', LCLObject.ClassName);
end;

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

  if cceTextDidChange in Events then
  begin
    TmpSpec := MakeEventSpec(kEventClassTextField, kEventTextDidChange);
    InstallControlEventHandler(Widget,
      RegisterEventHandler(@CarbonControl_TextDidChange),
      1, @TmpSpec, Pointer(Self), nil);
  end;
  
  if cceListItemSelected in Events then
  begin
    TmpSpec := MakeEventSpec(kEventClassHIComboBox, kEventComboBoxListItemSelected);
    InstallControlEventHandler(Widget,
      RegisterEventHandler(@CarbonControl_ListItemSelected),
      1, @TmpSpec, Pointer(Self), nil);
  end;

  DebugLn('TCarbonControl.RegisterEvents ', ClassName, ' ',
    LCLObject.Name, ': ', LCLObject.ClassName);
end;

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
  if cceTextDidChange in Events then
    UnregisterEventHandler(@CarbonControl_TextDidChange);
  if cceListItemSelected in Events then
    UnregisterEventHandler(@CarbonControl_ListItemSelected);
end;

procedure TCarbonControl.CreateWidget(const AParams: TCreateParams);
begin
  SetControlProperty(Widget, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Self), @Self);
  if Content <> ControlRef(Widget) then
    SetControlProperty(Content, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Self), @Self);
end;

procedure TCarbonControl.DestroyWidget;
begin
  RemoveControlProperty(Widget, LAZARUS_FOURCC, WIDGETINFO_FOURCC);
  if Content <> ControlRef(Widget) then
    RemoveControlProperty(Content, LAZARUS_FOURCC, WIDGETINFO_FOURCC);
    
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
  Method:  TCarbonControl.Invalidate
  Params:  Rect - Pointer to rect (optional)
  Returns: Nothing

  Invalidates the specified client rect or entire area of control
 ------------------------------------------------------------------------------}
procedure TCarbonControl.Invalidate(Rect: PRect);
begin
  if Rect = nil then HiViewSetNeedsDisplay(Frame, True)
  else
    HiViewSetNeedsDisplayInRect(Content, RectToCGRect(Rect^), True);
end;

function TCarbonControl.IsEnabled: Boolean;
begin
  Result := IsControlEnabled(Frame);
end;

function TCarbonControl.IsVisible: Boolean;
begin
  Result := FPCMacOSAll.IsControlVisible(Frame);
end;

function TCarbonControl.Enable(AEnable: Boolean): Boolean;
begin
  Result := not FPCMacOSAll.IsControlEnabled(Frame);

  if AEnable then
    FPCMacOSAll.EnableControl(Frame)
  else
    FPCMacOSAll.DisableControl(Frame);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.GetBounds
  Params:  ARect         - Record for window coordinates
  Returns: If function succeeds

  Returns the control bounding rectangle relative to the client origin of its
  parent
 ------------------------------------------------------------------------------}
function TCarbonControl.GetBounds(var ARect: TRect): Boolean;
var
  AWndRect: FPCMacOSAll.Rect;
begin
  Result := FPCMacOSAll.GetControlBounds(Frame, AWndRect) <> nil;
  if Result then ARect := CarbonRectToRect(AWndRect);
end;

function TCarbonControl.GetScreenBounds(var ARect: TRect): Boolean;
var
  BoundsRect: HIRect;
  WindowRect: FPCMacOSAll.Rect;
begin
  Result := False;

  if HIViewGetBounds(Frame, BoundsRect) <> noErr then Exit;
  if HIViewConvertRect(BoundsRect, Frame, nil) <> noErr then Exit;
  
  if GetWindowBounds(GetTopParentWindow, kWindowStructureRgn,
    WindowRect) <> noErr then Exit;
    
  ARect := CGRectToRect(BoundsRect);
  OffsetRect(ARect, WindowRect.left, WindowRect.top);

  Result := True;
end;

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

procedure TCarbonControl.SetColor(const AColor: TColor);
var
  FontStyle: ControlFontStyleRec;
begin
  // get current font style preserve other font settings
  GetControlData(ControlRef(Widget), kControlEntireControl,
    kControlFontStyleTag, SizeOf(FontStyle), @FontStyle, nil);

  FontStyle.flags := FontStyle.flags or kControlUseBackColorMask;
  FontStyle.backColor := ColorToRGBColor(AColor);

  SetControlFontStyle(ControlRef(Widget), FontStyle);
  // invalidate control
  Invalidate;
end;

procedure TCarbonControl.SetFont(const AFont: TFont);
var
  FontStyle: ControlFontStyleRec;
begin
  // get current font style to preserve other font settings
  GetControlData(ControlRef(Widget), kControlEntireControl,
    kControlFontStyleTag, SizeOf(FontStyle), @FontStyle, nil);

  FontStyle.flags := FontStyle.flags or kControlUseFontMask or kControlUseSizeMask or
    kControlUseFaceMask or kControlUseForeColorMask;

  FontStyle.font := FindCarbonFontID(AFont.Name);
  FontStyle.size := AFont.Size;
  FontStyle.style := FontStyleToQDStyle(AFont.Style);
  FontStyle.foreColor := ColorToRGBColor(AFont.Color);

  SetControlFontStyle(ControlRef(Widget), FontStyle);
  // invalidate control
  Invalidate;
end;

procedure TCarbonControl.ShowHide(AVisible: Boolean);
begin
  HIViewSetVisible(Frame, AVisible);
end;

function TCarbonControl.GetText(var S: String): Boolean;
var
  CFString: CFStringRef;
begin
  Result := False;

  CFString := HIViewCopyText(HIViewRef(Widget));
  if CFString = nil then Exit;
  try
    S := CFStringToStr(CFString);
    Result := True;
  finally
    FreeCFString(CFString);
  end;
end;

function TCarbonControl.SetText(const S: String): Boolean;
var
  CFString: CFStringRef;
begin
  Result := False;
  
  CreateCFString(S, CFString);
  try
    Result := HIViewSetText(HIViewRef(Widget), CFString) = noErr;
  finally
    FreeCFString(CFString);
  end;
end;

function TCarbonControl.Update: Boolean;
begin
  Result := HIViewRender(HIViewRef(Widget)) = noErr;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.GetTopParentWindow
  Returns: Window reference

  Retrieves the parent window reference of the Carbon control
 ------------------------------------------------------------------------------}
function TCarbonControl.GetTopParentWindow: WindowRef;
var
  Window: TControl;
begin
  Window := LCLObject.GetTopParent;

  if Window is TCustomForm then Result := AsWindowRef((Window as TWinControl).Handle)
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
begin
  GetGlobalMouse(P);
  
  GetWindowBounds(GetTopParentWindow, kWindowStructureRgn, R);
  MousePoint.X := P.h - R.left;
  MousePoint.Y := P.v - R.top;
  
  HIViewConvertPoint(MousePoint, nil, Content);
  Result.X := Trunc(MousePoint.X);
  Result.Y := Trunc(MousePoint.Y);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.GetClientRect
  Params:  ARect - Record for client area coordinates
  Returns: If the function succeeds

  Returns the control client rectangle relative to the parent client area origin
 ------------------------------------------------------------------------------}
function TCarbonControl.GetClientRect(var ARect: TRect): Boolean;
var
  AClientRect: FPCMacOSAll.Rect;
  ClientRegion: FPCMacOSAll.RgnHandle;
begin
  Result := False;

  ClientRegion := FPCMacOSAll.NewRgn();
  try
    case GetControlRegion(ControlRef(Widget), kControlContentMetaPart,
      ClientRegion) of
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
    end;
  finally
    FPCMacOSAll.DisposeRgn(ClientRegion);
  end;
end;

function TCarbonControl.GetPreferredSize: TPoint;
var
  R: FPCMacOSAll.Rect;
  S: SmallInt;
begin
  R := GetCarbonRect(0, 0, 0, 0);
  if GetBestControlRect(ControlRef(Widget), R, S) = noErr then
  begin
    Result.X := R.right - R.left;
    Result.Y := R.bottom - R.top;
  end
  else
  begin
    Result.X := 0;
    Result.Y := 0;
  end;
end;

