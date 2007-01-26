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
  AInfo: PWidgetInfo): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  Msg: TLMessage;
  PrivateClass: TCarbonPrivateHiViewClass;
begin
  Result := CallNextEventHandler(ANextHandler, AEvent);
  PrivateClass := TCarbonPrivateHiViewClass(AInfo^.WSClass.WSPrivate);

  FillChar(Msg, SizeOf(Msg),0);
  Msg.msg := LM_DESTROY;
  DeliverMessage(AInfo^.LCLObject, Msg);

  PrivateClass.UnregisterEvents;
  FreeWidgetInfo(AInfo);
end;

function CarbonPrivateCommon_Draw(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AInfo: PWidgetInfo): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  PaintMsg: TLMPaint;
  AStruct: PPaintStruct;
begin
  debugln('CarbonPrivateCommon_Draw ',DbgSName(AInfo^.LCLObject));
  // first let carbon draw/update
  Result := CallNextEventHandler(ANextHandler, AEvent);

  FillChar(PaintMsg,SizeOf(PaintMsg),0);
  PaintMsg.Msg := LM_PAINT;
  PaintMsg.DC := HDC(TCarbonDeviceContext.Create(AInfo));
  New(PaintMsg.PaintStruct);
  AStruct := PaintMsg.PaintStruct;
  FillChar(AStruct^, SizeOf(TPaintStruct), 0);
  AStruct^.hdc := PaintMsg.DC;
  try
    debugln('CarbonPrivateCommon_Draw LM_PAINT to ', DbgSName(AInfo^.LCLObject));
    DeliverMessage(AInfo^.LCLObject, PaintMsg);
  finally
    Dispose(PaintMsg.PaintStruct);
    TCarbonDeviceContext(PaintMsg.DC).Free;
  end;
end;

function CarbonPrivateCommon_BoundsChanged(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AInfo: PWidgetInfo): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  AWinControl: TWinControl;
  PosMsg : TLMWindowPosChanged;
  SizeMsg: TLMSize;
  MoveMsg: TLMMove;
  WidgetBounds: TRect;
begin
  debugln('CarbonPrivateCommon_BoundsChanged ', DbgSName(AInfo^.LCLObject));
  // first let carbon draw/update
  Result := CallNextEventHandler(ANextHandler, AEvent);
  
  if AInfo^.LCLObject is TWinControl then
  begin
    AWinControl := TWinControl(AInfo^.LCLObject);
    GetCarbonLocalWindowRect(AWinControl.Handle, WidgetBounds, AInfo);

    // first send a LM_WINDOWPOSCHANGED message
    if (AWinControl.Left <> WidgetBounds.Left) or
       (AWinControl.Top <> WidgetBounds.Top) or
       (AWinControl.Left + AWinControl.Width <> WidgetBounds.Right) or
       (AWinControl.Top + AWinControl.Height <> WidgetBounds.Bottom) then
    begin
      FillChar(PosMsg,SizeOf(PosMsg),0);
      PosMsg.Msg := LM_WINDOWPOSCHANGED;
      New(PosMsg.WindowPos);
      try
        with PosMsg.WindowPos^ do begin
          hWndInsertAfter := 0;
          x := WidgetBounds.Left;
          y := WidgetBounds.Top;
          cx := WidgetBounds.Right - WidgetBounds.Left;
          cy := WidgetBounds.Bottom - WidgetBounds.Top;
          flags := 0;
        end;
        DeliverMessage(AInfo^.LCLObject, PosMsg);
      finally
        Dispose(PosMsg.WindowPos);
      end;
    end;

    // then send a LM_SIZE message
    if (AWinControl.Width <> WidgetBounds.Right - WidgetBounds.Left) or
       (AWinControl.Height <> WidgetBounds.Bottom - WidgetBounds.Top) then
    begin
      FillChar(SizeMsg,SizeOf(SizeMsg),0);
      SizeMsg.Msg := LM_SIZE;
      SizeMsg.SizeType := Size_SourceIsInterface;
      SizeMsg.Width := WidgetBounds.Right - WidgetBounds.Left;
      SizeMsg.Height := WidgetBounds.Bottom - WidgetBounds.Top;
      DeliverMessage(AInfo^.LCLObject, SizeMsg);
    end;

    // then send a LM_MOVE message
    if (AWinControl.Left <> WidgetBounds.Left) or
       (AWinControl.Top <> WidgetBounds.Top) then
    begin
      FillChar(MoveMsg,SizeOf(MoveMsg),0);
      MoveMsg.Msg := LM_MOVE;
      MoveMsg.MoveType := Move_SourceIsInterface;
      MoveMsg.XPos := WidgetBounds.Left;
      MoveMsg.YPos := WidgetBounds.Top;
      DeliverMessage(AInfo^.LCLObject, MoveMsg);
    end;
  end;
end;
