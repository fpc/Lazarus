{ $Id$ }
{
 *****************************************************************************
 *                              LCLMessageGlue.pas                           *
 *                              ------------------                           *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  A Unit to make the passing of messages to the LCL from the different
  widgetsets easier. Your mileage will vary if you try to use these procs
  from within your program.

}
unit LCLMessageGlue;

{$MODE objfpc}{$H+}

interface

uses
  Classes, Forms, LCLClasses, LCLProc, Controls, Messages, LMessages, LCLType;

function DeliverMessage(const Target: TObject; var AMessage): PtrInt;
function SendSimpleMessage(const Target: TControl; Msg: Cardinal): PtrInt;

function LCLSendActivateMsg(const Target: TControl; Active: Boolean; Minimized: Boolean; ActiveWindow: HWND = 0): PtrInt;
function LCLSendSetFocusMsg(const Target: TControl): PtrInt;
function LCLSendKillFocusMsg(const Target: TControl): PtrInt;
function LCLSendShowWindowMsg(const Target: TControl; Show: Boolean; Status: LPARAM = 0): PtrInt;
function LCLSendSizeMsg(const Target: TControl; Width, Height: Word; SizeType: Longint; FromInterface: Boolean = True): PtrInt;
function LCLSendMoveMsg(const Target: TControl; XPos, YPos: SmallInt; MoveType: PtrInt = Move_Default; FromInterface: Boolean = True): PtrInt;
function LCLSendMouseMoveMsg(const Target: TControl; XPos, YPos: SmallInt; ShiftState: TShiftState = []): PtrInt;
function LCLSendMouseDownMsg(const Target: TControl; XPos, YPos: SmallInt; Button: TMouseButton; ShiftState: TShiftState = []): PtrInt;
function LCLSendMouseUpMsg(const Target: TControl; XPos, YPos: SmallInt; Button: TMouseButton; ShiftState: TShiftState = []): PtrInt;
function LCLSendMouseWheelMsg(const Target: TControl; XPos, YPos, WheelDelta: SmallInt; ShiftState: TShiftState = []): PtrInt;
function LCLSendCaptureChangedMsg(const Target: TControl): PtrInt;
function LCLSendSelectionChangedMsg(const Target: TControl): PtrInt;
function LCLSendDestroyMsg(const Target: TControl): PtrInt;
function LCLSendChangedMsg(const Target: TControl; ItemIndex: WPARAM = 0): PtrInt;
function LCLSendClickedMsg(const Target: TControl): PtrInt;
function LCLSendMouseEnterMsg(const Target: TControl): PtrInt;
function LCLSendMouseLeaveMsg(const Target: TControl): PtrInt;
function LCLSendSetEditableMsg(const Target: TControl): PtrInt;
function LCLSendMoveWordMsg(const Target: TControl): PtrInt;
function LCLSendMovePageMsg(const Target: TControl): PtrInt;
function LCLSendMoveToRowMsg(const Target: TControl): PtrInt;
function LCLSendMoveToColumnMsg(const Target: TControl): PtrInt;
function LCLSendKillCharMsg(const Target: TControl): PtrInt;
function LCLSendKillWordMsg(const Target: TControl): PtrInt;
function LCLSendKillLineMsg(const Target: TControl): PtrInt;
function LCLSendCutToClipboardMsg(const Target: TControl): PtrInt;
function LCLSendCopyToClipboardMsg(const Target: TControl): PtrInt;
function LCLSendPasteFromClipboardMsg(const Target: TControl): PtrInt;
function LCLSendConfigureEventMsg(const Target: TControl): PtrInt;
function LCLSendPaintMsg(const Target: TControl;const  DC: HDC; const PaintStruct: PPaintStruct): PtrInt;
function LCLSendKeyDownEvent(const Target: TControl; var CharCode: Word; KeyData: PtrInt; BeforeEvent, IsSysKey: Boolean): PtrInt;
function LCLSendKeyUpEvent(const Target: TControl; var CharCode: Word; KeyData: PtrInt; BeforeEvent, IsSysKey: Boolean): PtrInt;
function LCLSendTimerMsg(const Target: TControl; TimerID: WParam; TimerProc: LParam): PtrInt;
function LCLSendExitMsg(const Target: TControl): PtrInt;
function LCLSendCloseQueryMsg(const Target: TControl): PtrInt;
function LCLSendDragStartMsg(const Target: TControl): PtrInt;
function LCLSendDeactivateStartMsg(const Target: TControl): PtrInt;
function LCLSendMonthChangedMsg(const Target: TControl): PtrInt;
function LCLSendYearChangedMsg(const Target: TControl): PtrInt;
function LCLSendDayChangedMsg(const Target: TControl): PtrInt;
function LCLSendMouseMultiClickMsg(const Target: TControl; XPos, YPos: SmallInt; Button: TMouseButton; ClickCount: Byte = 2; ShiftState: TShiftState = []): PtrInt;
function LCLSendDrawListItemMsg(const Target: TControl; const DrawListItemStruct: PDrawListItemStruct): PtrInt;
function LCLSendDropDownMsg(const Target: TControl): PtrInt;
function LCLSendCloseUpMsg(const Target: TControl): PtrInt;

implementation

function DeliverMessage(const Target: TObject; var AMessage): PtrInt;
var
  RefCounted: Boolean;
begin
  if Target=nil then DebugLn('[DeliverMessage] Target = nil');
  {$IFDEF VerboseDeliverMessage}
  if  (TLMessage(AMessage).Msg <>LM_MOUSEMOVE)
    and (TLMessage(AMessage).Msg <>LM_PAINT)
    and (TLMessage(AMessage).Msg <>LM_KEYDOWN)
    and (TLMessage(AMessage).Msg <>LM_KEYUP)
    and (TLMessage(AMessage).Msg < CN_KEYDOWN ) then
    DebugLn('DeliverMessage ',DbgS(Target),
    ' ',TComponent(Target).Name,':',TObject(Target).ClassName,
    ' Message=',GetMessageName(TLMessage(AMessage).Msg));
  {$ENDIF}
  RefCounted := False;
  try
    try
      if Target is TLCLComponent then
      begin
        TLCLComponent(Target).IncLCLRefCount;
        RefCounted := True;
      end;
      if Target is TControl then
        TControl(Target).WindowProc(TLMessage(AMessage))
      else
        Target.Dispatch(TLMessage(AMessage));
    except
      Application.HandleException(nil);
    end;
  finally
    if RefCounted then
      TLCLComponent(Target).DecLCLRefCount;
  end;
  Result := TLMessage(AMessage).Result;
end;

{******************************************************************************
 *                                                                            *
 *  SendSimpleMessage                                                         *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the simple message            *
 *  Msg         : Message type constant                                       *
 *                                                                            *
 ******************************************************************************}
function SendSimpleMessage(const Target: TControl; Msg: Cardinal): PtrInt;
var
  Mess : TLMessage;
begin
  FillChar(Mess,SizeOf(Mess),0);
  Mess.Msg := Msg;
  Result := DeliverMessage(Target,  Mess);
end;


{******************************************************************************
 *                                                                            *
 *  LCLSendActivateMsg                                                        *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_ACTIVATE       *
 *  Active      : Set to True.  This is currently ignored                     *
 *  Minimized   : Set to False. This is currently ignored                     *
 *  ActiveWindow: Optional. Pass the handle of the currently or newly         *
 *                active window                                               *
 *                                                                            *
 ******************************************************************************}
function LCLSendActivateMsg(const Target: TControl; Active: Boolean; Minimized: Boolean; ActiveWindow: HWND = 0): PtrInt;
var
  Mess: TLMActivate;
begin
  Result := 0;

  FillChar(Mess,SizeOf(Mess),0);
  Mess.Msg := LM_ACTIVATE;
  Mess.Active:=Active;
  Mess.Minimized:=Minimized;
  Mess.ActiveWindow:=ActiveWindow;
  Mess.Result := 0;
  DeliverMessage(Target, Mess);
  Result := Mess.Result;
end;




{******************************************************************************
 *                                                                            *
 *  LCLSendSetFocusMessage                                                    *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_SETFOCUS       *
 *                                                                            *
 ******************************************************************************}
function LCLSendSetFocusMsg(const Target: TControl): PtrInt;
begin
  Result:=SendSimpleMessage(Target, LM_SETFOCUS);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendKillFocusMessage                                                   *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_KILLFOCUS      *
 *                                                                            *
 ******************************************************************************}
function LCLSendKillFocusMsg(const Target: TControl): PtrInt;
begin
  Result:=SendSimpleMessage(Target, LM_KILLFOCUS);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendShowWindowMsg                                                      *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_SHOWWINDOW     *
 *  Show        : Set to True if Showing the window False otherwise           *
 *  Status      : Usually zero. Set to non-zero if the message was generated  *
 *                by another window showing or hiding.                        *
 *                                                                            *
 ******************************************************************************}
function LCLSendShowWindowMsg(const Target: TControl; Show: Boolean;
  Status: LPARAM = 0): PtrInt;
var
   Mess : TLMShowWindow;
begin
  FillChar(Mess,SizeOf(Mess),0);
  Mess.Msg := LM_SHOWWINDOW;
  Mess.Show := True;

  Result := DeliverMessage(Target, Mess);
end;

{******************************************************************************
 *                                                                            *
 * LCLSendSizeMsg                                                             *
 *                                                                            *
 * Returns       : 0 to accept the message, non-zero to reject the message    *
 *                                                                            *
 * Params                                                                     *
 *                                                                            *
 * Target        : The Control that will recieve the message LM_SIZE          *
 * Width, Height : The new Width and Height for the control                   *
 * SizeType      : SIZENORMAL, SIZEICONIC, SIZEFULLSCREEN, SIZEZOOMSHOW or    *
 *                 SIZEZOOMHIDE. (In LCLType)                                 *
 * FromInterface : True if this message was sent from the widgetset to notify *
 *                 the LCL of a change.  False to make the widgetset change   *
 *                 the size of the control. Default = True                    *
 *                                                                            *
 ******************************************************************************}
function LCLSendSizeMsg(const Target: TControl; Width, Height: Word;
  SizeType: Longint; FromInterface: Boolean = True): PtrInt;
var
  Mess: TLMSize;
begin
  Result := 0;
  FillChar(Mess, SizeOf(Mess), 0);
  Mess.Msg := LM_SIZE;
  Mess.Width := Width;
  Mess.Height := Height;
  //SIZENORMAL, SIZEICONIC, SIZEFULLSCREEN, SIZEZOOMSHOW, SIZEZOOMHIDE.
  Mess.SizeType := SizeType;
  if FromInterface then
    Mess.SizeType := Mess.SizeType or Size_SourceIsInterface;
    
  Result := DeliverMessage(Target, Mess);
end;


{******************************************************************************
 *                                                                            *
 * LCLSendMoveMsg                                                             *
 *                                                                            *
 * Returns       : 0 to accept the message, non-zero to reject the message    *
 *                                                                            *
 * Params                                                                     *
 *                                                                            *
 * Target        : The Control that will recieve the message LM_MOVE          *
 * XPos, YPos    : The new Top and Left for the control                       *
 * MoveType      : Move_Default = update, 1 = force RequestAlign              *
 * FromInterface : True if this message was sent from the widgetset to notify *
 *                 the LCL of a change.  False to make the widgetset change   *
 *                 the position of the control. Default = True                *
 *                                                                            *
 ******************************************************************************}
function LCLSendMoveMsg(const Target: TControl; XPos, YPos: SmallInt;
  MoveType: PtrInt = Move_Default; FromInterface: Boolean = True): PtrInt;
var
  Mess: TLMMove;
begin
  FillChar(Mess, SizeOf(Mess), 0);
  Mess.Msg := LM_MOVE;
  Mess.XPos := XPos;
  Mess.YPos := YPos;
  Mess.MoveType := MoveType;
  if FromInterface then
    Mess.MoveType := Mess.MoveType or Move_SourceIsInterface;
    
  Result := DeliverMessage(Target, Mess);
end;


{******************************************************************************
 *                                                                            *
 * LCLSendMouseMoveMsg                                                        *
 *                                                                            *
 * Returns       : 0 to accept the message, non-zero to reject the message    *
 *                                                                            *
 * Params                                                                     *
 *                                                                            *
 * Target        : The Control that will recieve the message LM_SIZE          *
 * XPos, YPos    : The Mouses X and Y position relative to the control.       *
 * ShiftState    : Modifier keys that are pressed at the time of the event    *
 *                                                                            *
 ******************************************************************************}
function LCLSendMouseMoveMsg(const Target: TControl; XPos, YPos: SmallInt;
  ShiftState: TShiftState = []): PtrInt;
var
  Mess: TLMMouseMove;
begin
  FillChar(Mess, SizeOf(Mess), 0);
  Mess.Msg := LM_MouseMove;
  Mess.XPos := XPos;
  Mess.YPos := YPos;

  Mess.Keys := ShiftStateToKeys(ShiftState);

  Result := DeliverMessage(Target, Mess);
end;


{******************************************************************************
 *                                                                            *
 * LCLSendMouseDownMsg                                                        *
 *                                                                            *
 * Returns       : 0 to accept the message, non-zero to reject the message    *
 *                                                                            *
 * Params                                                                     *
 *                                                                            *
 * Target        : The Control that will recieve the message LM_xBUTTONDOWN   *
 * XPos, YPos    : The Mouses X and Y position relative to the control.       *
 * Button        : TMouseButton (mbLeft, mbMiddle, mbRight)                   *
 * ShiftState    : Modifier keys that are pressed at the time of the event    *
 *                                                                            *
 ******************************************************************************}
function LCLSendMouseDownMsg(const Target: TControl; XPos, YPos: SmallInt;
  Button: TMouseButton; ShiftState: TShiftState = []): PtrInt;
var
  Mess: TLMMouse;
begin
  FillChar(Mess, SizeOf(Mess), 0);
  case Button of
    mbLeft   : Mess.Msg := LM_LBUTTONDOWN;
    mbMiddle : Mess.Msg := LM_MBUTTONDOWN;
    mbRight  : Mess.Msg := LM_RBUTTONDOWN;
    mbExtra1 : Mess.Msg := LM_XBUTTONDOWN;
    mbExtra2 : Mess.Msg := LM_XBUTTONDOWN;
  end;

  Mess.XPos := XPos;
  Mess.YPos := YPos;

  Mess.Keys := ShiftStateToKeys(ShiftState);

  Result := DeliverMessage(Target, Mess);
end;

{******************************************************************************
 *                                                                            *
 * LCLSendMouseUpMsg                                                          *
 *                                                                            *
 * Returns       : 0 to accept the message, non-zero to reject the message    *
 *                                                                            *
 * Params                                                                     *
 *                                                                            *
 * Target        : The Control that will recieve the message LM_xBUTTONUP     *
 * XPos, YPos    : The Mouses X and Y position relative to the control.       *
 * Button        : TMouseButton (mbLeft, mbMiddle, mbRight)                   *
 * ShiftState    : Modifier keys that are pressed at the time of the event    *
 *                                                                            *
 ******************************************************************************}
function LCLSendMouseUpMsg(const Target: TControl; XPos, YPos: SmallInt;
  Button: TMouseButton; ShiftState: TShiftState): PtrInt;
var
  Mess: TLMMouse;
begin
  FillChar(Mess, SizeOf(Mess), 0);
  case Button of
    mbLeft   : Mess.Msg := LM_LBUTTONUP;
    mbMiddle : Mess.Msg := LM_MBUTTONUP;
    mbRight  : Mess.Msg := LM_RBUTTONUP;
    mbExtra1 : Mess.Msg := LM_XBUTTONUP;
    mbExtra2 : Mess.Msg := LM_XBUTTONUP;
  end;

  Mess.XPos := XPos;
  Mess.YPos := YPos;

  Mess.Keys := ShiftStateToKeys(ShiftState);

  Result := DeliverMessage(Target, Mess);
end;

{******************************************************************************
 *                                                                            *
 * LCLSendMouseWheelMsg                                                       *
 *                                                                            *
 * Returns       : 0 to accept the message, non-zero to reject the message    *
 *                                                                            *
 * Params                                                                     *
 *                                                                            *
 * Target        : The Control that will recieve the message LM_xBUTTONUP     *
 * XPos, YPos    : The Mouses X and Y position relative to the control.       *
 * WheelDelta    : -1 for Up, 1 for Down                                      *
 * ShiftState    : Modifier keys that are pressed at the time of the event    *
 *                                                                            *
 ******************************************************************************}
function LCLSendMouseWheelMsg(const Target: TControl; XPos, YPos,
  WheelDelta: SmallInt; ShiftState: TShiftState): PtrInt;
var
  Mess: TLMMouseEvent;
begin
  FillChar(Mess, SizeOf(Mess), 0);

  Mess.Msg := LM_MOUSEWHEEL;
  Mess.X := XPos;
  Mess.Y := YPos;
  Mess.WheelDelta := WheelDelta;
  Mess.State := ShiftState;

  Result := DeliverMessage(Target, Mess);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendCaptureChangedMsg                                                  *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_CAPTURECHANGED *
 *                                                                            *
 ******************************************************************************}
function LCLSendCaptureChangedMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, LM_CAPTURECHANGED);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendSelectionChangedMsg                                                *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_SELCHANGE      *
 *                                                                            *
 ******************************************************************************}
function LCLSendSelectionChangedMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, LM_SELCHANGE);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendSelectionChangedMsg                                                *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_DESTROY        *
 *                                                                            *
 ******************************************************************************}
function LCLSendDestroyMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, LM_DESTROY);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendChangedMsg                                                         *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_CHANGED        *
 *  ItemIndex   : Only used by Listviews and CheckBoxes. The Index of         *
 *                the Item that has changed.                                  *
 *                                                                            *
 ******************************************************************************}
function LCLSendChangedMsg(const Target: TControl; ItemIndex: WPARAM = 0): PtrInt;
var
  Mess: TLMessage;
begin
  FillChar(Mess, SizeOf(Mess), 0);
  Mess.Msg := LM_CHANGED;
  Mess.WParam := ItemIndex;
  Result := DeliverMessage(Target, Mess);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendClickedMsg                                                         *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_CLICKED        *
 *                                                                            *
 ******************************************************************************}
function LCLSendClickedMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, LM_CLICKED);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendMouseEnterMsg                                                      *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message CM_MOUSEENTER     *
 *                                                                            *
 ******************************************************************************}
function LCLSendMouseEnterMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, CM_MOUSEENTER);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendMouseLeaveMsg                                                      *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message CM_MOUSELEAVE     *
 *                                                                            *
 *                                                                            *
 ******************************************************************************}
function LCLSendMouseLeaveMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, CM_MOUSELEAVE);
end;


{******************************************************************************
 *                                                                            *
 *  LCLSendSetEditableMsg                                                     *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_SETEDITABLE    *
 *                                                                            *
 *  Not used by the LCL                                                       *
 *                                                                            *
 ******************************************************************************}
function LCLSendSetEditableMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, LM_SETEDITABLE);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendMoveWordMsg                                                        *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_MOVEWORD       *
 *                                                                            *
 *  Not used by the LCL                                                       *
 *                                                                            *
 ******************************************************************************}
function LCLSendMoveWordMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, LM_MOVEWORD);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendMovePageMsg                                                        *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_MOVEPAGE       *
 *                                                                            *
 *  Not used by the LCL                                                       *
 *                                                                            *
 ******************************************************************************}
function LCLSendMovePageMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, LM_MOVEPAGE);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendMoveToRowMsg                                                       *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_MOVETOROW      *
 *                                                                            *
 *  Not used by the LCL                                                       *
 *                                                                            *
 ******************************************************************************}
function LCLSendMoveToRowMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, LM_MOVETOROW);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendMoveToColumnMsg                                                    *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_MOVETOCOLUMN   *
 *                                                                            *
 *  Not used by the LCL                                                       *
 *                                                                            *
 ******************************************************************************}
function LCLSendMoveToColumnMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, LM_MOVETOCOLUMN);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendKillCharMsg                                                        *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_KILLCHAR       *
 *                                                                            *
 *  Not used by the LCL                                                       *
 *                                                                            *
 ******************************************************************************}
function LCLSendKillCharMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, LM_KILLCHAR);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendKillWordMsg                                                        *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_KILLWORD       *
 *                                                                            *
 *  Not used by the LCL                                                       *
 *                                                                            *
 ******************************************************************************}
function LCLSendKillWordMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, LM_KILLWORD);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendKillLineMsg                                                        *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_KILLLINE       *
 *                                                                            *
 *  Not used by the LCL                                                       *
 *                                                                            *
 ******************************************************************************}
function LCLSendKillLineMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, LM_KILLLINE);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendCutToClipboardMsg                                                  *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_CUT            *
 *                                                                            *
 ******************************************************************************}
function LCLSendCutToClipboardMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, LM_CUT);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendCopyToClipboardMsg                                                 *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_COPY           *
 *                                                                            *
 ******************************************************************************}
function LCLSendCopyToClipboardMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, LM_COPY);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendPasteFromClipboardMsg                                              *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_COPY           *
 *                                                                            *
 ******************************************************************************}
function LCLSendPasteFromClipboardMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, LM_PASTE);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendConfigureEventMsg                                                  *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_CONFIGUREEVENT *
 *                                                                            *
 *  Not used by the LCL                                                       *
 *                                                                            *
 ******************************************************************************}
function LCLSendConfigureEventMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, LM_CONFIGUREEVENT);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendPaintMsg                                                           *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_PAINT          *
 *  DC          : This is the Device Context                                  *
 *  PaintStruct : PaintStruct                                                 *
 *                                                                            *
 ******************************************************************************}
function LCLSendPaintMsg(const Target: TControl;const  DC: HDC; const PaintStruct: PPaintStruct): PtrInt;
var
  Mess: TLMPaint;
begin
  FillChar(Mess, SizeOf(Mess), 0);
  Mess.Msg := LM_PAINT;
  Mess.DC := DC;
  Mess.PaintStruct := PaintStruct;
  
  Result := DeliverMessage(Target, Mess);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendKeyDownEvent                                                       *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message xx_xxKEYDOWN      *
 *  CharCode    : This is the VK Key code. Check if this has changed after    *
 *                sending the message.                                        *
 *  KeyData     : Bitwise or'ed combination of the KF_xxxx constants.         *
 *  BeforeEvent : True if we are sending the message before the widgetset     *
 *                has handled the keystroke. False otherwise                  *
 *  IsSysKey    : True if the Alt Key was also pressed.                       *
 *                                                                            *
 ******************************************************************************}
function LCLSendKeyDownEvent(const Target: TControl; var CharCode: Word;
  KeyData: PtrInt; BeforeEvent, IsSysKey: Boolean): PtrInt;
var
  Mess: TLMKeyDown;
begin
  FillChar(Mess, SizeOf(Mess), 0);
  if BeforeEvent then begin
    if IsSysKey then
      Mess.Msg := CN_SYSKEYDOWN
    else Mess.Msg := CN_KEYDOWN;
  end
  else begin
    if IsSysKey then
      Mess.Msg := LM_SYSKEYDOWN
    else Mess.Msg := LM_KEYDOWN;
  end;
  Mess.CharCode := CharCode;
  Mess.KeyData := KeyData;
  Result := DeliverMessage(Target, Mess);
  CharCode := Mess.CharCode;
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendKeyUpEvent                                                         *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message xx_xxKEYUP        *
 *  CharCode    : This is the VK Key code. Check if this has changed after    *
 *                sending the message.                                        *
 *  KeyData     : Bitwise or'ed combination of the KF_xxxx constants.         *
 *  BeforeEvent : True if we are sending the message before the widgetset     *
 *                has handled the keystroke. False otherwise.                 *
 *  IsSysKey    : True if the Alt Key was also pressed.                       *
 *                                                                            *
 ******************************************************************************}
function LCLSendKeyUpEvent(const Target: TControl; var CharCode: Word;
  KeyData: PtrInt; BeforeEvent, IsSysKey: Boolean): PtrInt;
var
  Mess: TLMKeyUp;
begin
  FillChar(Mess, SizeOf(Mess), 0);
  if BeforeEvent then begin
    if IsSysKey then
      Mess.Msg := CN_SYSKEYUP
    else Mess.Msg := CN_KEYUP;
  end
  else begin
    if IsSysKey then
      Mess.Msg := LM_SYSKEYUP
    else Mess.Msg := LM_KEYUP;
  end;
  Mess.CharCode := CharCode;
  Mess.KeyData := KeyData;
  Result := DeliverMessage(Target, Mess);
  CharCode := Mess.CharCode;
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendTimerMsg                                                           *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to allow the message to   *
 *                continue to process.                                        *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_TIMER          *
 *  TimerID     : ID of the timer.                                            *
 *  TimerProc   : The procedure to call.                                      *
 *                                                                            *
 ******************************************************************************}
function LCLSendTimerMsg(const Target: TControl; TimerID: WParam;
  TimerProc: LParam): PtrInt;
var
  Mess: TLMessage;
begin
  FillChar(Mess, SizeOf(Mess), 0);
  Mess.Msg := LM_TIMER;
  Mess.WParam := TimerID;
  Mess.LParam := TimerProc;
  Result:=DeliverMessage(Target, Mess);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendExitMsg                                                            *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_EXIT           *
 *                                                                            *
 *  Not used by the LCL                                                       *
 *                                                                            *
 ******************************************************************************}
function LCLSendExitMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, LM_EXIT);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendCloseQueryMsg                                                      *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_CLOSEQUERY     *
 *                                                                            *
 ******************************************************************************}
function LCLSendCloseQueryMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, LM_CLOSEQUERY);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendDragStartMsg                                                       *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_DRAGSTART      *
 *                                                                            *
 ******************************************************************************}
function LCLSendDragStartMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, LM_DRAGSTART);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendDeactivateStartMsg                                                 *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_DEACTIVATE     *
 *                                                                            *
 ******************************************************************************}
function LCLSendDeactivateStartMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, LM_DEACTIVATE);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendMonthChangedMsg                                                    *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_MONTHCHANGED   *
 *                                                                            *
 ******************************************************************************}
function LCLSendMonthChangedMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, LM_MONTHCHANGED);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendYearChangedMsg                                                     *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_YEARCHANGED    *
 *                                                                            *
 ******************************************************************************}
function LCLSendYearChangedMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, LM_YEARCHANGED);
end;

{******************************************************************************
 *                                                                            *
 *  LCLSendDayChangedMsg                                                      *
 *                                                                            *
 *  Returns     : 0 to accept the message, non-zero to reject the message     *
 *                                                                            *
 *  Params                                                                    *
 *                                                                            *
 *  Target      : The Control that will recieve the message LM_DAYCHANGED     *
 *                                                                            *
 ******************************************************************************}
function LCLSendDayChangedMsg(const Target: TControl): PtrInt;
begin
  Result := SendSimpleMessage(Target, LM_DAYCHANGED);
end;

{******************************************************************************
 *                                                                            *
 * LCLSendMouseMultiClickMsg                                                  *
 *                                                                            *
 * Returns       : 0 to accept the message, non-zero to reject the message    *
 *                                                                            *
 * Params                                                                     *
 *                                                                            *
 * Target        : The Control that will recieve the message LM_xBUTTONxxxCLK *
 * XPos, YPos    : The Mouses X and Y position relative to the control.       *
 * Button        : TMouseButton (mbLeft, mbMiddle, mbRight)                   *
 * ClickCount    : 2 = LM_xBUTTONDBLCLK, 3 = LM_xBUTTONTRIPLECLK,             *
 *                 4 = LM_xBUTTONQUADCLK                                      *
 * ShiftState    : Modifier keys that are pressed at the time of the event    *
 *                                                                            *
 ******************************************************************************}
function LCLSendMouseMultiClickMsg(const Target: TControl; XPos, YPos: SmallInt;
  Button: TMouseButton; ClickCount: Byte = 2; ShiftState: TShiftState = []): PtrInt;
var
  Mess: TLMMouse;
begin
  FillChar(Mess, SizeOf(Mess), 0);
  Mess.Msg := LM_UNKNOWN;
  case ClickCount of
    2:
    case Button of
      mbLeft   : Mess.Msg := LM_LBUTTONDBLCLK;
      mbMiddle : Mess.Msg := LM_MBUTTONDBLCLK;
      mbRight  : Mess.Msg := LM_RBUTTONDBLCLK;
      mbExtra1 : Mess.Msg := LM_XBUTTONDBLCLK;
      mbExtra2 : Mess.Msg := LM_XBUTTONDBLCLK;
    end;
    3:
    case Button of
      mbLeft   : Mess.Msg := LM_LBUTTONTRIPLECLK;
      mbMiddle : Mess.Msg := LM_MBUTTONTRIPLECLK;
      mbRight  : Mess.Msg := LM_RBUTTONTRIPLECLK;
      mbExtra1 : Mess.Msg := LM_XBUTTONTRIPLECLK;
      mbExtra2 : Mess.Msg := LM_XBUTTONTRIPLECLK;
    end;
    4:
    case Button of
      mbLeft   : Mess.Msg := LM_LBUTTONQUADCLK;
      mbMiddle : Mess.Msg := LM_MBUTTONQUADCLK;
      mbRight  : Mess.Msg := LM_RBUTTONQUADCLK;
      mbExtra1 : Mess.Msg := LM_XBUTTONQUADCLK;
      mbExtra2 : Mess.Msg := LM_XBUTTONQUADCLK;
    end;
  end;
    
  Mess.XPos := XPos;
  Mess.YPos := YPos;

  Mess.Keys := ShiftStateToKeys(ShiftState);

  Result := DeliverMessage(Target, Mess);
end;

{******************************************************************************
 *                                                                            *
 * LCLSendDrawListItemMsg                                                     *
 *                                                                            *
 * Returns       : 0 to accept the message, non-zero to reject the message    *
 *                                                                            *
 * Params                                                                     *
 *                                                                            *
 * Target        : The Control that will recieve the message LM_DRAWLISTITEM  *
 * DrawListItemStruct : Pointer to a TDrawListItemStruct                      *
 *                                                                            *
 ******************************************************************************}
function LCLSendDrawListItemMsg(const Target: TControl; const DrawListItemStruct: PDrawListItemStruct): PtrInt;
var
  Mess: TLMDrawListItem;
begin
  FillChar(Mess, SizeOf(Mess), 0);
  Mess.Msg := LM_DRAWLISTITEM;
  Mess.DrawListItemStruct := DrawListItemStruct;

  Result := DeliverMessage(Target, Mess);
end;

{******************************************************************************
 *                                                                            *
 * LCLSendDropDownMsg                                                         *
 *                                                                            *
 * Returns       : 0 to accept the message, non-zero to reject the message    *
 *                                                                            *
 * Params                                                                     *
 *                                                                            *
 * Target        : The Control that will recieve the message CN_Command       *
 *                                                                            *
 * Used to notify a combo that the combobox is popping down                   *
 *                                                                            *
 ******************************************************************************}
function LCLSendDropDownMsg(const Target: TControl): PtrInt;
var
  Mess : TLMCommand;
begin
  FillChar(Mess, SizeOf(Mess), 0);
  Mess.Msg := CN_Command;
  Mess.NotifyCode := CBN_DROPDOWN;

  Result := DeliverMessage(Target, Mess);
end;

function LCLSendCloseUpMsg(const Target: TControl): PtrInt;
var
  Mess : TLMCommand;
begin
  FillChar(Mess, SizeOf(Mess), 0);
  Mess.Msg := CN_Command;
  Mess.NotifyCode := CBN_CLOSEUP;

  Result := DeliverMessage(Target, Mess);
end;


// Remove these lines as you implement the function

{
  LM_ACTIVATEITEM    // NOT USED
  LM_FOCUS           // NOT USED
  LM_SIZEALLOCATE    // NOT USED
  LM_CHECKRESIZE     // NOT USED
  LM_SHOW            // NOT USED
  LM_MOVERESIZE      // NOT USED
  LM_DRAW            // NOT USED
  LM_MOUSEBTNPRESS   // NOT USED
  LM_MOUSEBTNRELEASE // NOT USED
}


end.
