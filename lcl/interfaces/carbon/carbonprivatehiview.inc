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
//var
//  MCarbonWSWinControl_Dispose_UPP: EventHandlerUPP = nil;
//  MCarbonWSWinControl_Hit_UPP: EventHandlerUPP = nil;


{ CarbonPrivateHIView_Dispose
  Is called when the control is freed by carbon.
  Frees the widgetinfo.
   }
function CarbonPrivateHIView_Dispose(ANextHandler: EventHandlerCallRef;
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

function CarbonPrivateHIView_Hit(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AInfo: PWidgetInfo): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  Msg: TLMessage;
begin
  Result := CallNextEventHandler(ANextHandler, AEvent);
  FillChar(Msg, SizeOf(Msg),0);
  Msg.msg := LM_CLICKED;
  DeliverMessage(AInfo^.LCLObject, Msg);
end;

function CarbonPrivateHIView_MouseMove(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AInfo: PWidgetInfo): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  DebugLn('CarbonPrivateHIView_MouseMove');
  Result := CallNextEventHandler(ANextHandler, AEvent);
end;

function CarbonPrivateHIView_Update(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AInfo: PWidgetInfo): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  WidgetBounds: TRect;
  AWinControl: TWinControl;
  PosChanged, SizeChanged: Boolean;

  procedure UpdateBounds;
  var
    AWndRect: FPCMacOSAll.Rect;
  begin
    if AInfo^.widgetType=cwtWindowRef then begin
      GetWindowBounds(WindowRef(AInfo^.Widget),kWindowStructureRgn, AWndRect);
    end else begin
      GetControlBounds(ControlRef(AInfo^.Widget), AWndRect);
    end;
    WidgetBounds:=Rect(AWndRect.left,AWndRect.top,AWndRect.right-AWndRect.left,
                       AWndRect.bottom-AWndRect.top);
    PosChanged:=(WidgetBounds.Left<>AWinControl.Left)
               or (WidgetBounds.Top<>AWinControl.Top);
    SizeChanged:=(WidgetBounds.Right-WidgetBounds.Left<>AWinControl.Width)
               or (WidgetBounds.Bottom-WidgetBounds.Top<>AWinControl.Height);
  end;
  
  procedure SendBoundsMessages;
  var
    PosMsg : TLMWindowPosChanged;
    SizeMsg: TLMSize;
    MoveMsg: TLMMove;
  begin
    if AWinControl<>nil then begin
      // first send a LM_WINDOWPOSCHANGED message
      UpdateBounds;
      if PosChanged or SizeChanged then begin
        FillChar(PosMsg,SizeOf(PosMsg),0);
        PosMsg.Msg := LM_WINDOWPOSCHANGED;
        New(PosMsg.WindowPos);
        try
          with PosMsg.WindowPos^ do begin
            hWndInsertAfter := 0;
            x := WidgetBounds.Left;
            y := WidgetBounds.Top;
            cx := WidgetBounds.Right-WidgetBounds.Left;
            cy := WidgetBounds.Bottom-WidgetBounds.Top;
            flags := 0;
          end;
          DeliverMessage(AInfo^.LCLObject, PosMsg);
        finally
          Dispose(PosMsg.WindowPos);
        end;
        UpdateBounds;
      end;

      // then send a LM_SIZE message
      if SizeChanged then begin
        FillChar(SizeMsg,SizeOf(SizeMsg),0);
        SizeMsg.Msg := LM_SIZE;
        SizeMsg.SizeType := Size_SourceIsInterface;
        SizeMsg.Width := WidgetBounds.Right-WidgetBounds.Left;
        SizeMsg.Height := WidgetBounds.Bottom-WidgetBounds.Top;
        DeliverMessage(AInfo^.LCLObject, SizeMsg);
        UpdateBounds;
      end;

      // then send a LM_MOVE message
      if PosChanged then begin
        FillChar(MoveMsg,SizeOf(MoveMsg),0);
        MoveMsg.Msg := LM_MOVE;
        MoveMsg.MoveType := Move_SourceIsInterface;
        MoveMsg.XPos := WidgetBounds.Left;
        MoveMsg.YPos := WidgetBounds.Top;
        DeliverMessage(AInfo^.LCLObject, MoveMsg);
        UpdateBounds;
      end;
    end;
  end;
  
  procedure SendPaintMessage;
  var
    PaintMsg: TLMPaint;
    AStruct: PPaintStruct;
  begin
    FillChar(PaintMsg,SizeOf(PaintMsg),0);
    PaintMsg.Msg:=LM_PAINT;
    PaintMsg.DC:=HDC(CreateDevContextForInfo(AInfo));
    New(PaintMsg.PaintStruct);
    AStruct:=PaintMsg.PaintStruct;
    FillChar(AStruct^,SizeOf(TPaintStruct),0);
    AStruct^.hdc:=PaintMsg.DC;
    try
      //debugln('CarbonPrivateHIView_Update LM_PAINT to ',DbgSName(AInfo^.LCLObject));
      DeliverMessage(AInfo^.LCLObject, PaintMsg);
    finally
      Dispose(PaintMsg.PaintStruct);
      FreeDevContext(TDeviceContext(PaintMsg.DC));
    end;
  end;
  
begin
  debugln('CarbonPrivateHIView_Update ',DbgSName(AInfo^.LCLObject));
  // first let carbon draw/update
  Result := CallNextEventHandler(ANextHandler, AEvent);
  if AInfo^.LCLObject is TWinControl then begin
    AWinControl:=TWinControl(AInfo^.LCLObject);
  end else begin
    AWinControl:=nil;
  end;

  SendPaintMessage;
end;

// ==================================================================
// C L A S S 
// ==================================================================

{ TCarbonWSWinControlPrivate }

class procedure TCarbonPrivateHiView.RegisterEvents(AInfo: PWidgetInfo);
var
  MouseSpec: array [0..6] of EventTypeSpec;
  TmpSpec: EventTypeSpec;
begin
  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlDispose);
  InstallControlEventHandler(AInfo^.Widget,
    RegisterEventHandler(@CarbonPrivateHIView_Dispose),
    1, @TmpSpec, Pointer(AInfo), nil);

  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlHit);
  InstallControlEventHandler(AInfo^.Widget,
    RegisterEventHandler(@CarbonPrivateHIView_Hit),
    1, @TmpSpec, Pointer(AInfo), nil);

  MouseSpec[0] := MakeEventSpec(kEventClassMouse, kEventMouseMoved);
  MouseSpec[1] := MakeEventSpec(kEventClassMouse, kEventMouseDragged);
  
  InstallControlEventHandler(AInfo^.Widget,
    RegisterEventHandler(@CarbonPrivateHIView_MouseMove),
    2, @MouseSpec[0], Pointer(AInfo), nil);

  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlDraw);
  InstallControlEventHandler(AInfo^.Widget,
    RegisterEventHandler(@CarbonPrivateHIView_Update),
    1, @TmpSpec, Pointer(AInfo), nil);

  
//  InstallControlHandler(AInfo, kEventClassControl, kEventControlDispose, RegisterEventHandler(@CarbonWSWinControl_Dispose));
//  InstallControlHandler(AInfo, kEventClassControl, kEventControlHit, @CarbonWSWinControl_Hit, MCarbonWSWinControl_Hit_UPP);
  DebugLN(TWinControl(Ainfo^.LCLObject).Name,':', TWinControl(Ainfo^.LCLObject).ClassName ,' Events set')
end;

class procedure TCarbonPrivateHiView.UnregisterEvents;
begin
  UnregisterEventHandler(@CarbonPrivateHIView_Dispose);
  UnregisterEventHandler(@CarbonPrivateHIView_Hit);
//  UnregisterEventHandler(@CarbonPrivateHIView_MouseProc);
end;
