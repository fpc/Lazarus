{%MainUnit carbonprivate.pp}
{ $Id: $}
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

function CarbonPrivateCommon_Dispose(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AInfo: TCarbonWidgetInfo): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  PrivateClass: TCarbonPrivateHiViewClass;
begin
  Result := CallNextEventHandler(ANextHandler, AEvent);
  PrivateClass := TCarbonPrivateHiViewClass(AInfo.WSClass.WSPrivate);

  LCLSendDestroyMsg(AInfo.LCLObject as TControl);

  PrivateClass.UnregisterEvents;
  AInfo.Free;
end;

function CarbonPrivateCommon_Draw(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AInfo: TCarbonWidgetInfo): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  EraseMsg: TLMEraseBkgnd;
  AStruct: PPaintStruct;
begin
  {$IFDEF VerbosePaint}
  Debugln('CarbonPrivateCommon_Draw ', DbgSName(AInfo.LCLObject));
  {$ENDIF}
  
  {
  // send erase background messsage
  FillChar(PaintMsg,SizeOf(EraseMsg),0);
  PaintMsg.Msg := LM_ERASEBKGND;
  PaintMsg.DC := HDC(DC);
  DeliverMessage(AInfo.LCLObject, EraseMsg);}
  
  // first let carbon draw/update
  Result := CallNextEventHandler(ANextHandler, AEvent);
  
  GetEventParameter(AEvent, kEventParamCGContextRef, typeCGContextRef, nil,
    SizeOf(CGContextRef), nil, @(AInfo.CGContext));
    
  AInfo.Context.Reset;

  // TODO: save DC
  
  New(AStruct);
  FillChar(AStruct^, SizeOf(TPaintStruct), 0);
  AStruct^.hdc := HDC(AInfo.Context);
  try
    {$IFDEF VerbosePaint}
    DebugLn('CarbonPrivateCommon_Draw LM_PAINT to ', DbgSName(AInfo.LCLObject));
    {$ENDIF}
    LCLSendPaintMsg(AInfo.LCLObject as TControl, HDC(AInfo.Context), AStruct);
  finally
    Dispose(AStruct);
    
    // TODO: restore DC
  end;
end;

function CarbonPrivateCommon_BoundsChanged(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AInfo: TCarbonWidgetInfo): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  AWinControl: TWinControl;
  WidgetBounds: TRect;
begin
  debugln('CarbonPrivateCommon_BoundsChanged ', DbgSName(AInfo.LCLObject));
  // first let carbon draw/update
  Result := CallNextEventHandler(ANextHandler, AEvent);
  
  if AInfo.LCLObject is TWinControl then
  begin
    AWinControl := TWinControl(AInfo.LCLObject);
    GetCarbonLocalWindowRect(AWinControl.Handle, WidgetBounds, AInfo);

    // then send a LM_SIZE message
    if (AWinControl.Width <> WidgetBounds.Right - WidgetBounds.Left) or
       (AWinControl.Height <> WidgetBounds.Bottom - WidgetBounds.Top) then
    begin
      LCLSendSizeMsg(AWinControl, WidgetBounds.Right - WidgetBounds.Left,
        WidgetBounds.Bottom - WidgetBounds.Top, Size_SourceIsInterface);
    end;

    // then send a LM_MOVE message
    if (AWinControl.Left <> WidgetBounds.Left) or
       (AWinControl.Top <> WidgetBounds.Top) then
    begin
      LCLSendMoveMsg(AWinControl, WidgetBounds.Left,
        WidgetBounds.Top, Move_SourceIsInterface);
    end;
  end;
end;

function CarbonPrivateCommon_ValueChanged(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AInfo: TCarbonWidgetInfo): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  ScrollMsg: TLMScroll;
  RadioButton: TRadioButton;
  I: Integer;
begin
  DebugLn('CarbonPrivateCommon_ValueChanged ', DbgSName(AInfo.LCLObject));

  Result := CallNextEventHandler(ANextHandler, AEvent);
  
  if AInfo.LCLObject is TCustomCheckBox then
  begin
    (*// toggle custom check box state before on click event is proceed
    case GetControl32BitValue(ControlRef(AInfo.Widget)) of
      kControlCheckBoxCheckedValue  : Value := cbUnchecked;
      kControlCheckBoxUncheckedValue: Value := cbChecked;
      kControlCheckBoxMixedValue    : Value := cbGrayed; // Is this right?
    end;

    SetControl32BitValue(ControlRef(AInfo.Widget), Value);*)

    if AInfo.LCLObject is TRadioButton then
    begin
      if GetControl32BitValue(ControlRef(AInfo.Widget))
        = kControlCheckBoxCheckedValue then
      begin
        DebugLn('CarbonPrivateCommon_ValueChanged Uncheck Sibling');
        // uncheck sibling radio buttons
        RadioButton := (AInfo.LCLObject as TRadioButton);
        if RadioButton.Parent <> nil then
        begin
          for I := 0 to RadioButton.Parent.ControlCount - 1 do
            if (RadioButton.Parent.Controls[I] is TRadioButton) and
              (RadioButton.Parent.Controls[I] <> RadioButton) then
              (RadioButton.Parent.Controls[I] as TRadioButton).Checked := False;
        end;
      end;
    end;
  end;

  if AInfo.LCLObject is TCustomScrollBar then
  begin
    FillChar(ScrollMsg, SizeOf(TLMScroll), 0);

    ScrollMsg.Msg := LM_HSCROLL;
    ScrollMsg.ScrollCode := SB_THUMBTRACK;
    ScrollMsg.Pos := GetControl32BitValue(ControlRef(AInfo.Widget));
    ScrollMsg.ScrollBar := HWnd(AInfo.Widget);

    DeliverMessage(AInfo.LCLObject, ScrollMsg);
  end
  else
    LCLSendChangedMsg(AInfo.LCLObject as TControl);
end;
