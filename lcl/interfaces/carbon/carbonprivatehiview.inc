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
begin
  debugln('CarbonPrivateHIView_Update ');
  // first let carbon draw/update
  Result := CallNextEventHandler(ANextHandler, AEvent);
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
