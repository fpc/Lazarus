{%MainUnit carbonprivate.pp}
{ $Id: $}
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
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

function CarbonPrivateWindow_Close(ANextHandler: EventHandlerCallRef;
                                   AEvent: EventRef;
                                   AInfo: PWidgetInfo): OSStatus; mwpascal;
var
  Msg: TLMessage;
begin
  // Do canclose query, if false then exit

  FillChar(Msg, SizeOf(Msg),0);
  Msg.msg := LM_CLOSEQUERY;

  // Message results : 0 - do nothing, 1 - destroy or hide window
  if DeliverMessage(AInfo^.LCLObject, Msg) = 0
  then begin
    Result := eventNotHandledErr;
    Exit;
  end;

  Result := CallNextEventHandler(ANextHandler, AEvent);
end;

function CarbonPrivateWindow_Closed(ANextHandler: EventHandlerCallRef;
                                    AEvent: EventRef;
                                    AInfo: PWidgetInfo): OSStatus; mwpascal;
var
  Msg: TLMessage;
  PrivateClass: TCarbonPrivateWindowClass;
begin
  Result := CallNextEventHandler(ANextHandler, AEvent);
  PrivateClass := TCarbonPrivateWindowClass(AInfo^.WSClass.WSPrivate);

  FillChar(Msg, SizeOf(Msg),0);
  Msg.msg := LM_DESTROY;
  DeliverMessage(AInfo^.LCLObject, Msg);
  
  PrivateClass.UnregisterEvents;
  FreeWidgetInfo(AInfo);
end;

//Generic function that handles all types of mouse events

function CarbonPrivateWindow_MouseProc(ANextHandler: EventHandlerCallRef;
                                AEvent: EventRef;
                                AInfo: PWidgetInfo): OSStatus; mwpascal;
var
  Msg: PLMessage;
  LMessage: TLMessage;
  LMMouse: TLMMouse;           // used by WM_LBUTTONDBLCLK
  LMMouseMove: TLMMouseMove;   // used by WM_MOUSEMOVE
  LMMouseEvent: TLMMouseEvent; // used by WM_MOUSEWHEEL
  EventKind: UInt32;
  Control: ControlRef;
  CarbonPt: FPCMacOSALL.Point;
  FCLPoint: TPoint;
    //
    // helper functions used commonly
    //
      function GetClickCount: Integer;
      var
        ClickCount: UInt32;
      begin
        GetEventParameter(AEvent, kEventParamClickCount, typeUInt32, nil, SizeOf(ClickCount), nil, @ClickCount);
        Result := ClickCount;
      end;
      function GetMouseButton:Integer;
      var
        MouseButton: EventMouseButton;
      begin
        GetEventParameter(AEvent, kEventParamMouseButton, typeMouseButton, nil, SizeOf(MouseButton), nil, @MouseButton);
        Result := MouseButton;
      end;

      function GetMousePoint: TPoint;
      var
        MousePoint: HIPoint;//QDPoint;
        ARect: FPCMacOSAll.Rect;
      begin
        ARect.Left := 0;
        ARect.Top := 0;
        GetEventParameter(AEvent, kEventParamWindowMouseLocation, typeHIPoint, nil, SizeOf(MousePoint), nil, @MousePoint);
        HIViewConvertPoint(MousePoint,nil, HiViewRef(AInfo^.Widget));
        if AInfo^.LCLObject is TCustomForm then
          GetWindowStructureWidths(WindowRef(AInfo^.Widget), ARect);
        Result.X := Trunc(MousePoint.X-ARect.left);
        Result.Y := Trunc(MousePoint.Y-ARect.top);
        //WriteLn('Mouse to Widget Coords: X=',Result.X,' Y=',Result.Y);
      end;
    //
    // handler functions
    //
      procedure HandleMouseDownEvent;
      var
        MouseButton: Integer;
        ClickCount: Integer;
        MousePoint: TPoint;
      begin
        Msg := @LMMouse;
        FillChar(LMMouse, SizeOf(LMMouse),0);

        ClickCount := GetClickCount;
        MouseButton := GetMouseButton;
        MousePoint := GetMousePoint;
        
        case ClickCount of
          1:  begin
                case MouseButton of
                  1: LMMouse.Msg := LM_LBUTTONDOWN;
                  2: LMMouse.Msg := LM_RBUTTONDOWN;
                  3: LMMouse.Msg := LM_MButtonDown;
                end;
              end;
          2:  begin
                case MouseButton of
                  1: LMMouse.Msg := LM_MBUTTONDBLCLK;
                  2: LMMouse.Msg := LM_RBUTTONDBLCLK;
                  3: LMMouse.Msg := LM_MBUTTONDBLCLK;
                end;
              end;
          3:  begin
                case MouseButton of
                  1: LMMouse.Msg := LM_LBUTTONTRIPLECLK;
                  2: LMMouse.Msg := LM_RBUTTONTRIPLECLK;
                  3: LMMouse.Msg := LM_MBUTTONTRIPLECLK;
                end;
              end;
          4:
              begin
                case MouseButton of
                  1: LMMouse.Msg := LM_LBUTTONQUADCLK;
                  2: LMMouse.Msg := LM_RBUTTONQUADCLK;
                  3: LMMouse.Msg := LM_MBUTTONQUADCLK;
                end;
              end;
        end;

        LMMouse.XPos := MousePoint.X;
        LMMouse.YPos := MousePoint.Y;

        //LMMouse.Keys;
        {$Warning CarbonPrivateWindow_MouseProc LMMouse.Keys TODO}
      end;
      procedure HandleMouseUpEvent;
      var
        MouseButton: Integer;
        MousePoint: TPoint;
      begin
        // this is not called if NextHandler is called on MouseDown
        // perhaps mousetracking can fix this
        Msg := @LMMouse;
        FillChar(LMMouse, SizeOf(LMMouse),0);

        MouseButton := GetMouseButton;
        MousePoint := GetMousePoint;

        case MouseButton of
          1: LMMouse.Msg := LM_LBUTTONUP;
          2: LMMouse.Msg := LM_RBUTTONUP;
          3: LMMouse.Msg := LM_MButtonUP;
        end;

        LMMouse.XPos := MousePoint.X;
        LMMouse.YPos := MousePoint.Y;

        //LMMouse.Keys;

      end;

      procedure HandleMouseMovedEvent;
      var
        MousePoint: TPoint;
      begin
        Msg := @LMMouseMove;
        FillChar(LMMouseMove, SizeOf(LMMouseMove),0);
        
        MousePoint := GetMousePoint;
        
        LMMouseMove.Msg := LM_MOUSEMOVE;
        LMMouseMove.XPos := MousePoint.X;
        LMMouseMove.YPos := MousePoint.Y;
      end;

      procedure HandleMouseDraggedEvent;
      begin
        //TODO
      end;

      procedure HandleMouseWheelEvent;
      begin
        //TODO should be simple
      end;
begin
  Msg := @LMessage;
  FillChar(LMessage, SizeOf(TLMessage), 0);
  LMessage.Msg := LM_NULL;

  //Find out which control the mouse event should occur for
  FCLPoint := GetMousePoint;
  CarbonPt.h := FCLPoint.X;
  CarbonPt.v := FCLPoint.Y;
  Control := FindControlUnderMouse(CarbonPt, WindowRef(AInfo^.Widget),nil);
  
  if Control <> nil then begin
    AInfo := GetWidgetInfo(Pointer(Control));
  end;

  EventKind := GetEventKind(AEvent);
  case EventKind of
    kEventMouseDown       : HandleMouseDownEvent;
    kEventMouseUp         : HandleMouseUpEvent;
    kEventMouseMoved      : HandleMouseMovedEvent;
    kEventMouseDragged    : HandleMouseDraggedEvent;

    //For the enter and exit events tracking must be enabled
    //tracking is enabled by defining a rect that you want to track
    // TODO: Tracking
    kEventMouseEntered    : LMessage.Msg := CM_MOUSEENTER;
    kEventMouseExited     : LMessage.Msg := CM_MOUSELEAVE;

    kEventMouseWheelMoved : HandleMouseWheelEvent;
  end;
  
  if DeliverMessage(AInfo^.LCLObject, Msg^) = 0 then begin
    Result := CallNextEventHandler(ANextHandler, AEvent);
  end
  else begin
    // the LCL does not want the event propagated
    Result := noErr;
  end;
end;

// ==================================================================
// C L A S S 
// ==================================================================

  { TCarbonPrivateWindow }

procedure TCarbonPrivateWindow.RegisterEvents(AInfo: PWidgetInfo);
var
  MouseSpec: array [0..6] of EventTypeSpec;
  TmpSpec: EventTypeSpec;
begin
  TmpSpec := MakeEventSpec(kEventClassWindow, kEventWindowClose);
  InstallWindowEventHandler(AInfo^.Widget, RegisterEventHandler(@CarbonPrivateWindow_Close), 
    1, @TmpSpec, Pointer(AInfo), nil);

  TmpSpec := MakeEventSpec(kEventClassWindow, kEventWindowClosed);
  InstallWindowEventHandler(AInfo^.Widget, RegisterEventHandler(@CarbonPrivateWindow_Closed), 
    1, @TmpSpec, Pointer(AInfo), nil);

  MouseSpec[0].eventClass := kEventClassMouse;
  MouseSpec[0].eventKind := kEventMouseDown;
  MouseSpec[1].eventClass := kEventClassMouse;
  MouseSpec[1].eventKind := kEventMouseUp;
  MouseSpec[2].eventClass := kEventClassMouse;
  MouseSpec[2].eventKind := kEventMouseMoved;
  MouseSpec[3].eventClass := kEventClassMouse;
  MouseSpec[3].eventKind := kEventMouseDragged;
  MouseSpec[4].eventClass := kEventClassMouse;
  MouseSpec[4].eventKind := kEventMouseEntered;
  MouseSpec[5].eventClass := kEventClassMouse;
  MouseSpec[5].eventKind := kEventMouseExited;
  MouseSpec[6].eventClass := kEventClassMouse;
  MouseSpec[6].eventKind := kEventMouseWheelMoved;

  InstallWindowEventHandler(AInfo^.Widget, RegisterEventHandler(@CarbonPrivateWindow_MouseProc),
    7, @MouseSpec[0], Pointer(AInfo), nil);

end;

procedure TCarbonPrivateWindow.UnregisterEvents;
begin                      
  UnregisterEventHandler(@CarbonPrivateWindow_Close);
  UnregisterEventHandler(@CarbonPrivateWindow_Closed); 
  UnregisterEventHandler(@CarbonPrivateWindow_MouseProc);
end;

