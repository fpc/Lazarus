{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}


// temp solution to fill msgqueue
function DeliverPostMessage(const Target: Pointer; var Message): Boolean;
begin
//writeln('delivermessage');
  if TObject(Target) is TWinControl 
  then begin
    Result := PostMessage(TWinControl(Target).Handle, TLMessage(Message).Msg, TLMessage(Message).WParam, TLMessage(Message).LParam);
  end 
  else begin
    Result := DeliverMessage(TObject(Target), Message) = 0;
  end;
end;

{*************************************************************}
{            callback routines                                }
{*************************************************************}

{-----------------------------------------------------------------------------
  Function: PropEnumProc
  Params: Window - The window with the property
          Str    - The property name
          Data   - The property value
  Returns: Whether the enumeration should continue

  Enumerates and removes properties for the target window
 -----------------------------------------------------------------------------}
Function PropEnumProc(Window: Hwnd; Str: PChar; Data: Handle): LongBool; StdCall;
Begin
  Assert(False, 'Trace:PropEnumProc - Start');
  Assert(False, Format('Trace:PropEnumProc - Property %S (with value 0x%X) from window 0x%X removed', [String(Str), Data, Window]));
  RemoveProp(Window, Str);
  Result := True;
  Assert(False, 'Trace:PropEnumProc - Exit');
End;

{------------------------------------------------------------------------------
 Function: WindowProc
 Params: Window - The window that receives a message
         Msg    - The message received
         WParam - Word parameter
         LParam - Long-integer parameter
  Returns: 0 if Msg is handled; non-zero long-integer result otherwise

  Handles the messages sent to the current window by Windows or other
  applications
 ------------------------------------------------------------------------------}
Function WindowProc(Window: HWnd; Msg: UInt; WParam: WParam; LParam: LParam): LResult;
Type
  PLMInsertText = ^TLMInsertText;
Var
  C: Cardinal;
  CbObj: PObject;
  Cls: PChar;
  DataHandle: Handle;
  DC: HDC;
  I: Integer;
  List: TMsgArray;
  LMessage: TLMessage;
  MsgColl: PList;
  ObjCached: Boolean;
  OwnerObject: TObject;
  PS: PaintStruct;
  R: TRect;
  Rec: PLazObject;
  WinProcess: Boolean;
Begin
  Assert(False, 'Trace:WindowProc - Start');

  Result := 0;
  LMessage.Msg := -1;
  WinProcess := True;

  Assert(False, 'Trace:WindowProc - Getting Object With Callback Procedure');
  OwnerObject := TObject(GetProp(Window, 'Lazarus'));
  Assert(False, 'Trace:WindowProc - Getting Callback Object');
  CbObj := GetProp(Window, 'CbObj');
  MsgColl := GetProp(Window, 'MsgColl');
  ObjCached := False;
  Rec := LazObject;

  Assert(False, 'Trace:WindowProc - Checking Proc');
  Assert(False, Format('Trace:WindowProc - Window Value: $%S; Msg Value: %S; WParam: $%S; LParam: $%S', [IntToHex(Window, 4), WM_To_String(Msg), IntToHex(WParam, 4), IntToHex(LParam, 4)]));

  Case Msg Of
    LM_MONTHCHANGED..LM_DAYCHANGED:
    Begin
      If OwnerObject Is TCalendar Then
        LMessage.Msg := Msg;
    End;
    WM_ACTIVATE:
    Begin
      Case Lo(WParam) Of
        WA_ACTIVE:
        Begin
          LMessage.Msg := LM_ACTIVATE;
        End;
        WA_INACTIVE:
        Begin
          LMessage.Msg := LM_DEACTIVATE;
        End;
      End;
    End;
    WM_CAPTURECHANGED:
    Begin
      LMessage.Msg := LM_CHANGED;
    End;
    CB_DELETESTRING, LB_DELETESTRING:
    Begin
      LMessage.Msg := LM_DELETETEXT;
    End;
    CB_INSERTSTRING, LB_INSERTSTRING:
    Begin
      With PLMInsertText(@LMessage)^ Do
      Begin
        Msg := LM_INSERTTEXT;
        Position := WParam;
        NewText := String(LParam);
        Length := System.Length(NewText);
        UserData := Pointer(GetWindowLong(Window, GWL_USERDATA));
      End;
    End;
    WM_CHANGECBCHAIN:
    Begin
      If OldClipboardViewer <> WParam Then
        SendMessage(OldClipboardViewer, Msg, WParam, LParam)
      Else
        OldClipboardViewer := LParam;
    End;
    WM_CLOSE:
    Begin
      LMessage.Msg := LM_CLOSEQUERY;
    End;
    WM_COMMAND:
    Begin
      OwnerObject := TObject(GetProp(LParam, 'Lazarus'));
      Case Hi(WParam) Of
        0:
        Begin
          LMessage.Msg := LM_CLICKED;
        End;
        BN_KILLFOCUS, EN_KILLFOCUS:
        Begin
          LMessage.Msg := LM_EXIT;
        End;
        EN_CHANGE:
        Begin
          LMessage.Msg := CM_TEXTCHANGED;
        End;
      End;
    End;
    WM_CREATE:
    Begin
      Assert(False, 'Trace:WindowProc - Got WM_CREATE');
      LMessage.Msg := LM_CREATE;
    End;
    WM_COPY:
    Begin
      LMessage.Msg := LM_COPYTOCLIP;
    End;
    WM_CUT:
    Begin
      LMessage.Msg := LM_CUTTOCLIP;
    End;
    WM_DESTROY:
    Begin
      Assert(False, 'Trace:WindowProc - Got WM_DESTROY');
      ChangeClipboardChain(Window, OldClipboardViewer);
      For C := 0 To WndList.Count - 1 Do
        EnumProps(HWND(WndList[C]), @PropEnumProc);
      LMessage.Msg := LM_DESTROY;
      PostQuitMessage(0);
    End;
    WM_DRAWCLIPBOARD:
    Begin
      SendMessage(OldClipboardViewer, Msg, 0, 0);
    End;
    WM_ENABLE:
    Begin
      If WParam <> 0 Then
        LMessage.Msg := LM_SETEDITABLE;
    End;
    WM_HSCROLL:
    Begin
      With TLMScroll(LMessage) Do
      Begin
        Msg := LM_HSCROLL;
        ScrollCode := Lo(WParam);
        Pos := Hi(WParam);
        ScrollBar := LParam;
      End;
    End;
    WM_KEYDOWN:
    Begin
      With TLMKey(LMessage) Do
      Begin
        Msg := LM_SYSKEYUP;
        KeyData := LParam;
        CharCode := WParam;
      End;
    End;
    WM_KEYUP:
    Begin
      With TLMKey(LMessage) Do
      Begin
        Msg := LM_SYSKEYUP;
        KeyData := LParam;
        CharCode := WParam;
      End;
    End;
    WM_KILLFOCUS:
    Begin
      LMessage.Msg := LM_KILLFOCUS;
    End;
    //TODO:LM_KILLCHAR,LM_KILLWORD,LM_KILLLINE
    WM_LBUTTONDBLCLK:
    Begin
      With TLMMouse(LMessage) Do
      Begin
        Msg := LM_LBUTTONDBLCLK;
        XPos := Lo(LParam);
        YPos := Hi(LParam);
        Keys := WParam;
      End;
    End;
    WM_LBUTTONDOWN:
    Begin
      With TLMMouse(LMessage) Do
      Begin
        Msg := LM_LBUTTONDOWN;
        XPos := Lo(LParam);
        YPos := Hi(LParam);
        Keys := WParam;
      End;
    End;
    WM_LBUTTONUP:
    Begin
      With TLMMouse(LMessage) Do
      Begin
        Msg := LM_LBUTTONUP;
        XPos := Lo(LParam);
        YPos := Hi(LParam);
        Keys := WParam;
      End;
    End;
    WM_MBUTTONDBLCLK:
    Begin
      With TLMMouse(LMessage) Do
      Begin
        Msg := LM_MBUTTONDBLCLK;
        XPos := Lo(LParam);
        YPos := Hi(LParam);
        Keys := WParam;
      End;
    End;
    WM_MBUTTONDOWN:
    Begin
      With TLMMouse(LMessage) Do
      Begin
        Msg := LM_MBUTTONDOWN;
        XPos := Lo(LParam);
        YPos := Hi(LParam);
        Keys := WParam;
      End;
    End;
    WM_MBUTTONUP:
    Begin
      With TLMMouse(LMessage) Do
      Begin
        Msg := LM_MBUTTONUP;
        XPos := Lo(LParam);
        YPos := Hi(LParam);
        Keys := WParam;
      End;
    End;
    {WM_MOUSEHOVER:
    Begin
      LMessage.Msg := LM_ENTER;
    End;
    WM_MOUSELEAVE:
    Begin
      LMessage.Msg := LM_LEAVE;
    End;}
    WM_MOUSEMOVE:
    Begin
      With TLMMouseMove(LMessage) Do
      Begin
        Msg := LM_MOUSEMOVE;
        XPos := Lo(LParam);
        YPos := Hi(LParam);
        Keys := WParam;
      End;
    End;
    {WM_MOUSEWHEEL:
    Begin
      With TLMMouseEvent(LMessage) Do
      Begin
        Msg := LM_MOUSEWHEEL;
        WheelDelta := Hi(WParam);
        X := Lo(LParam);
        Y := Hi(LParam);
        State := GetShiftState;
        UserData := GetWindowLong(Window, GWL_USERDATA);
      End;
    End;}
    WM_MOVE:
    Begin
      With TLMMove(LMessage) Do
      Begin
        Msg := LM_MOVE;
        MoveType := WParam;
        XPos := Lo(LParam);
        YPos := Lo(LParam);
      End;
    End;
    //TODO:LM_MOVEPAGE,LM_MOVETOROW,LM_MOVETOCOLUMN
    WM_NCLBUTTONDOWN:
    Begin
      Assert(False, 'Trace:WindowProc - Got WM_NCLBUTTONDOWN');
    End;
    WM_NOTIFY:
    Begin
      With TLMNotify(LMessage) Do
      Begin
        Msg := LM_NOTIFY;
        IDCtrl := WParam;
        NMHdr := PNMHDR(LParam);
      End;
    End;
    WM_PAINT:
    Begin
      With TLMPaint(LMessage) Do
      Begin
        Msg := LM_PAINT;
        DC := GetDC(Window);
        Unused := 0;
        ReleaseDC(Window, DC);
      End;
    End;
    WM_PASTE:
    Begin
      LMessage.Msg := LM_PASTEFROMCLIP;
    End;
    WM_RBUTTONDBLCLK:
    Begin
      With TLMMouse(LMessage) Do
      Begin
        Msg := LM_RBUTTONDBLCLK;
        XPos := Lo(LParam);
        YPos := Hi(LParam);
        Keys := WParam;
      End;
    End;
    WM_RBUTTONDOWN:
    Begin
      With TLMMouse(LMessage) Do
      Begin
        Msg := LM_RBUTTONDOWN;
        XPos := Lo(LParam);
        YPos := Hi(LParam);
        Keys := WParam;
      End;
    End;
    WM_RBUTTONUP:
    Begin
      With TLMMouse(LMessage) Do
      Begin
        Msg := LM_RBUTTONUP;
        XPos := Lo(LParam);
        YPos := Hi(LParam);
        Keys := WParam;
      End;
    End;
    WM_SETFOCUS:
    Begin
      LMessage.Msg := LM_SETFOCUS;
    End;
    WM_SHOWWINDOW:
    Begin
      Assert(False, 'Trace:WindowProc - Got WM_SHOWWINDOW');
      With TLMShowWindow(LMessage) Do
      Begin
        Msg := LM_SHOWWINDOW;
        Show := WParam <> 0;
        Status := LParam;
      End;
    End;
    WM_SIZE:
    Begin
      With TLMSize(LMessage) Do
      Begin
        Msg := LM_SIZE;
        SizeType := WParam;
        Width := Lo(LParam);
        Height := Hi(LParam);
      End;
    End;
    WM_SYSKEYDOWN:
    Begin
      With TLMKey(LMessage) Do
      Begin
        Msg := LM_SYSKEYDOWN;
        KeyData := LParam;
        CharCode := WParam;
      End;
    End;
    WM_SYSKEYUP:
    Begin
      With TLMKey(LMessage) Do
      Begin
        Msg := LM_SYSKEYUP;
        KeyData := LParam;
        CharCode := WParam;
      End;
    End;
    WM_TIMER:
    Begin
      LMessage.Msg := LM_TIMER;
      LMessage.WParam := WParam;
      LMessage.LParam := LParam;
    End;
    WM_VSCROLL:
    Begin
      With TLMScroll(LMessage) Do
      Begin
        Msg := LM_VSCROLL;
        ScrollCode := Lo(WParam);
        Pos := Hi(WParam);
        ScrollBar := LParam;
      End;
    End;
    WM_WINDOWPOSCHANGED:
    Begin
      With TLMWindowPosMsg(LMessage) Do
      Begin
        Msg := LM_WINDOWPOSCHANGED;
        Unused := WParam;
        WindowPos := PWindowPos(LParam);
      End;
    End;
  End;

  {$IFDEF VER1_1} 
    List := TMsgArray(GetProp(Window, 'MsgList'));
    If Pointer(List) <> Nil Then
      For C := 0 To Length(List) Do
        If List[C] = LMessage.Msg Then
        Begin
          DeliverMessage(OwnerObject, LMessage);
          Exit;
        End;
  {$ENDIF}

  If (OwnerObject <> Nil) And (LMessage.Msg <> -1) Then
    DeliverMessage(OwnerObject, LMessage);

  If WinProcess Then
    Result := DefWindowProc(Window, Msg, WParam, LParam);

  Assert(False, 'Trace:WindowProc - Exit');
End;

{function Win32showCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   Mess : TLMShowWindow;
begin
   Result := True;
   EventTrace('show', data);
   Mess.Msg := LM_SHOWWINDOW;
   Mess.Show := True;

   Result := DeliverMessage(Data, Mess) = 0;
 end;

function Win32HideCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
  Mess : TLMShowWindow;
begin
  Result := True;
  EventTrace('hide', data);
  Mess.Msg := LM_SHOWWINDOW;
  Mess.Show := False;
  Result := DeliverMessage(Data, Mess) = 0;
end;

function Win32activateCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   Mess : TLMessage;
begin
   EventTrace('activate', data);
   Mess.Msg := LM_ACTIVATE;
   Result := DeliverMessage(Data, Mess) = 0;
end;

function Win32changedCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   Mess : TLMessage;
begin
   Result := True;
   EventTrace('changed', data);

   Mess.Msg := LM_CHANGED;
   Result := DeliverMessage(Data, Mess) = 0;
end;

Function Win32changed_editbox( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   Mess : TLMessage;
begin
   Result := True;
   EventTrace('changed', data);

   Mess.Msg := CM_TEXTCHANGED;
   Result := DeliverMessage(Data, Mess) = 0;
end;

function Win32draw( Win32Control: PWin32Control; area : TRect; data: wPointer) : Boolean; cdecl;
var
  Mess: TLMPaint;
begin
  Result := True;
  EventTrace('draw', data);
  Mess.Msg := LM_PAINT;
  Mess.DC := GetDC(THandle(Win32Control));
  Mess.Unused := 0;

  Result := DeliverPostMessage(Data, Mess);
//   Result := DeliverMessage(Data, MSG) = 0;
end;

function Win32FrmActivate(Control: PWin32Control; Event : Integer {TgdkEventFocus}; data: wPointer) : Boolean; cdecl;
var
   Mess : TLMActivate;
begin
   EventTrace('activate', data);
   Mess.Msg := LM_ACTIVATE;
   Result := DeliverPostMessage(Data, Mess);
end;

function Win32FrmDeactivate( Control: PWin32Control; Event : Integer {TgdkEventFocus}; data: wPointer) : Boolean; cdecl;
var
   Mess : TLMActivate;
begin
   EventTrace('deactivate', data);
   Mess.Msg := LM_DEACTIVATE;
   Result := DeliverPostMessage(Data, Mess);
end;

function Win32Map(Control: PWin32Control; Data: wPointer): Boolean; cdecl;
var
   Mess : TLMessage;
begin
   Result := True;
   EventTrace('map', data);
end;

function Win32configureevent( Win32Control: PWin32Control; {!}{event : PgdkEventConfigure;} data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;
   DC : HDC;
   Win32Control2 : PWin32Control;
   PenColor : TColor;
begin
   EventTrace('<Configure Event>', data);
   MessI := LM_CONFIGUREEVENT;

        //Get the widget owner because the 'fixed' widget called the signal
        //create a pixmap for drawing behind the scenes

        //assign it to the object data for this widget so it's stored there
        //Clear the canvas area

   MessI := LM_REDRAW;  //Should I be sending the Draw event??????????
   TObject(data).Dispatch(MessI);
   Assert(False, 'Trace:Exiting Configure');
end;

function Win32exposeevent( Win32Control: PWin32Control; {!}{event : PgdkEventExpose;} data: wPointer) : Boolean; cdecl;
var
  
  msg: TLMPaint;
begin
  Result := True;
  EventTrace('expose-event', data);
  
  msg.msg := LM_PAINT; 
  MSG.DC := GetDC(THandle(Win32Control));
  msg.Unused := 0; 
   
  Result := DeliverPostMessage(Data, MSG);

//  Result := DeliverMessage(Data, msg) = 0;
   
end;

function Win32keydown( Win32Control: PWin32Control; {!}{event : pgdkeventkey;} data: wPointer) : Boolean; cdecl;
var
   MessI : TLMKeyEvent;

begin
   EventTrace('key down', data);
   MessI.msg := LM_KEYDOWN;
{!}//   MessI.State := event^.state;
{!}//   MessI.Key := Event^.KeyVal;
{!}//   MessI.Length := Event^.Length;
//   MessI.Str := Event^.String;
   MessI.UserData := data;
   Result := DeliverMessage(Data, MessI) = 0;
   // TObject(data).Dispatch(MessI);
end;

function Win32keyup( Win32Control: PWin32Control; {!}{event : pgdkEventKey;} data: wPointer) : Boolean; cdecl;
var
   MessI : TLMKeyEvent;

begin
   EventTrace('Key Up', data);
   MessI.msg := LM_KEYUP;
{!}//   MessI.State := event^.state;
{!}//   MessI.Key := Event^.KeyVal;
{!}//   MessI.Length := Event^.Length;
//   MessI.Str := Event^.String;
   MessI.UserData := data;

   Result := DeliverMessage(Data, MessI) = 0;
   // TObject(data).Dispatch(MessI);
 end;

function Win32KeyUpDown(Win32Control: PWin32Control; Event : PWin32KeyEvent; Data: Pointer) : Boolean; cdecl;
var
  Msg: TLMKey;
  KeyCode: Word;
  Flags: Integer;
  Toggle, Extended, SysKey: Boolean;
begin
  GetWin32KeyInfo(Event, KeyCode, Msg.CharCode, SysKey, Extended, Toggle);
//  Assert(False, Format('Trace:[GTKKeyUpDown] Type: %3:d, GTK: 0x%0:x(%0:d) LCL: 0x%1:x(%1:d) VK: 0x%2:x(%2:d)', [Event^.keyval, KeyCode, Msg.CharCode, Event^.theType]));

  Flags := 0;

  if Extended then Flags := KF_EXTENDED;
  if SysKey then Flags := Flags or KF_ALTDOWN;


  Msg.KeyData := $00000000; //TODO: OEM char
  
  // TODO: Get Win32 constants set up.
  case Event^.theType of

    WIN32_KEY_RELEASE: // Key up
      begin
        EventTrace('key up', data);
        if SysKey
        then Msg.msg := LM_SYSKEYUP
        else Msg.msg := LM_KEYUP;

        Flags := Flags or KF_UP or KF_REPEAT;

        Msg.KeyData := Msg.KeyData or (Flags shl 16) or $0001 {allways};
        Result := DeliverPostMessage(data, msg);

      end;
    WIN32_KEY_PRESS: // Key press
      begin
        EventTrace('key down', data);
        if SysKey
        then Msg.msg := LM_SYSKEYDOWN
        else Msg.msg := LM_KEYDOWN;

        // todo  repeat
        // Flags := Flags or KF_REPEAT;

        Msg.KeyData := Msg.KeyData or (Flags shl 16) or $0001 {TODO:  repeatcount};
        Result := DeliverPostMessage(data, msg);

        if KeyCode <> $FFFF
        then begin
          EventTrace('char', data);
          if SysKey then Msg.msg := LM_SYSCHAR
          else Msg.msg := LM_CHAR;
          Msg.CharCode := KeyCode;
          Result := DeliverPostMessage(data, msg);
        end;
      end;
  end;
end;

function Win32focusCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
Var
   Mess : TLMessage;
begin
   EventTrace('focus', data);
//   Writeln('Getting Focus...');
   //TODO: fill in old focus
   Mess.msg := LM_SETFOCUS;
   Assert(False, Format('Trace:TODO: [gtkfocusCB] %s  finish', [TObject(Data).ClassName]));
   Result := DeliverMessage(Data,  Mess) = 0;
end;

function Win32KillFocusCB( Control: PWin32Control; event: Pointer {PGdkEventFocus}; data: wPointer) : Boolean; cdecl;
var
   Mess : TLMessage;
begin
   EventTrace('killfocus', data);
//   Writeln('Killing Focus...');
   Mess.msg := LM_KILLFOCUS;
   //TODO: fill in new focus
   Assert(False, Format('Trace:TODO: [gtkkillfocusCB] %s  finish', [TObject(Data).ClassName]));
   Result := DeliverMessage(Data,  Mess) = 0;
end;

function Win32destroyCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   Mess: TLMessage;
   Info: PWinControlInfo;
begin
   Result := True;
   EventTrace('destroy', data);
   Mess.msg := LM_DESTROY;
   Result := DeliverMessage(Data, Mess) = 0;
   
   // NOTE: if the destroy message is posted
   // we should post a info destroy message as well
   Info := GetControlInfo(Win32Control, False);
   if Info <> nil then Dispose(Info);
end;

function Win32deleteCB( Control: PWin32Control; event : Pointer {PGdkEvent}; data : wPointer) : Boolean; cdecl;
var Mess : TLMessage;
begin
  Mess.Msg:= LM_CLOSEQUERY;
  { Message results : True - do nothing, False - destroy or hide window }
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function Win32resizeCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   Mess : TLMessage;

begin
   EventTrace('resize', data);
   Mess.Msg := LM_SIZE;
   Result := DeliverMessage(Data, Mess) = 0;
   // TObject(data).Dispatch(MessI);
end;

// Commenting out until Win32 code for tracking motion is done.
{function GTKMotionNotify(widget:PGTKWidget; event: PGDKEventMotion; data: gPointer):GBoolean; cdecl;
var
  Msg: TLMMouseMove;
  ShiftState: TShiftState;
begin


  ShiftState := GTKEventState2ShiftState(Event^.State);
  with Msg do
  begin
    Msg := LM_MouseMove;
    XPos := Round(Event^.X);
    YPos := Round(Event^.Y);
//    XPos := Trunc(Event^.X);
//    YPos := trunc(Event^.Y);
{    Writeln('MOUSEMOVE Signal');
    Writeln('X = ');
    Writeln('  '+inttostr(XPos));
    Writeln('Y = ');
    Writeln('  '+inttostr(YPos));
    Writeln('X_root = ');
    Writeln('  '+inttostr(round(Event^.X_Root)));
    Writeln('Y_root = ');
    Writeln('  '+inttostr(round(Event^.Y_Root)));
    writeln('widget is ='+inttostr(longint(widget)));
    if (TObject(data) is TCOntrol) then
    writeln('Control is ='+TControl(data).classname);
    Writeln('------------------');
 }
    Keys := 0;
    if ssShift in ShiftState then Keys := Keys or MK_SHIFT;
    if ssCtrl in ShiftState then Keys := Keys or MK_CONTROL;
    if ssLeft in ShiftState then Keys := Keys or MK_LBUTTON;
    if ssRight in ShiftState then Keys := Keys or MK_RBUTTON;
    if ssMiddle in ShiftState then Keys := Keys or MK_MBUTTON;
  end;
  Result := DeliverPostMessage(Data, Msg);

  //if ssLeft in ShiftState then WriteLN(Format('[GTKMotionNotify] widget: 0x%p', [widget]));
  
  if (Pointer(MCaptureHandle) <> widget)
  and (MCaptureHandle <> 0)
  then  WriteLN(Format('[GTKMotionNotify] Capture differs -->  cap:0x%x gtk:0x%p', [MCaptureHandle, gtk_grab_get_current]));
end;}

function Win32MouseBtnPress( Win32Control: PWin32Control; {!}{event : pgdkEventButton;} data: wPointer) : Boolean; cdecl;
var
   MessI : TLMMouseEvent;
begin
   EventTrace('Mouse button Press', data);
   MessI.Msg := LM_LBUTTONDOWN;
   MessI.Button := -1;
   MessI.WheelDelta:= 0;
{!}//   MessI.State := Event^.State;
{!}//   MessI.X := Trunc(Event^.X);
{!}//   MessI.Y := trunc(Event^.Y);
   MessI.UserData := Data;
{!}//   case event^.Button of
{!}//    1 :
{!}//      MessI.Button := 0;
{!}//    2 :
{!}//      MessI.Button := 2;
{!}//    3 :
{!}//      MessI.Button := 1;
{!}//    4 :
{!}//      begin
{!}//        MessI.Msg := LM_MOUSEWHEEL;
{!}//        MessI.WheelDelta:=1;
{!}//      end;
{!}//    5 :
{!}//      begin
{!}//        MessI.Msg := LM_MOUSEWHEEL;
{!}//        MessI.WheelDelta:=-1;
{!}//      end;
{!}//   end;
   TObject(data).Dispatch(MessI);
end;

function Win32MouseBtnRelease( Win32Control: PWin32Control; {!}{event : pgdkEventButton;} data: wPointer) : Boolean; cdecl;
var
   MessI : TLMMouseEvent;

begin
   EventTrace('Mouse button release', data);
   MessI.Msg := LM_LBUTTONUP;
   MessI.Button := -1;
   MessI.WheelDelta:= 0;
{!}//   MessI.State := Event^.State;
{!}//   MessI.X := Trunc(Event^.X);
{!}//   MessI.Y := trunc(Event^.Y);
   MessI.UserData := Data;
{!}//   case event^.Button of
{!}//    1 :
{!}//      MessI.Button := 0;
{!}//    2 :
{!}//      MessI.Button := 2;
{!}//    3 :
{!}//      MessI.Button := 1;
{!}//   end;
   TObject(data).Dispatch(MessI);
end;

function Win32clickedCB( OwnerObject: Pointer; AMessage, wParam, lParam: LongInt) : Boolean; cdecl;
var
   MessI : Integer;

begin
 Assert(False, 'Trace:Callback Function - Win32clickedCB');
 EventTrace('clicked', OwnerObject);
 MessI := LM_CLICKED;
 TObject(OwnerObject).Dispatch(MessI);
end;

function Win32DialogOKclickedCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
Type
  TCustomColors = Array[1..16] Of COLORREF;
var
   theDialog : TCommonDialog;
   Fpointer : Pointer;
   colorArray : array[0..2] of double;
   colorsel : TChooseColor;
   newColor : COLORREF;
   FontName : String;
   OpenFile: OpenFileName;
   CustomColors: TCustomColors;
   RGBIO: DWORD;
   FontSel: TChooseFont;
   LF: TLogFont;
   Col: TGDIRGB;
Const
  Filter: PChar = 'All Files'#0'*.*'#0#0;
begin
   Result := True;
   theDialog := TCommonDialog(data);
   FPointer := Pointer(theDialog.Handle);
   if theDialog is TFileDialog then
   begin
      ZeroMemory(@OpenFile, SizeOf(OpenFile));
      With OpenFile Do
      Begin
        LStructSize := SizeOf(OpenFile);
        HWNDOwner := Win32Control^.Window;
        LPStrFilter := Filter;
        NMaxFile := MAX_PATH;
        Flags := OFN_Explorer Or OFN_AllowMultiSelect Or OFN_CreatePrompt Or OFN_HideReadOnly Or OFN_OverwritePrompt;
      End;
      
      If Not GetOpenFileName(@OpenFile) Then
        Assert(False, 'ERROR: [Win32DialogOKclickedCB] got unopenable file');
      TFileDialog(data).FileName := OpenFile.LPStrFile;
   end
   else if theDialog is TColorDialog then
   begin
      ZeroMemory(@colorsel, SizeOf(colorsel));
      With colorsel Do
      Begin
        LStructSize := SizeOf(colorsel);
        HWNDOwner := Win32Control^.Window;
        RGBResult := RGBIO;
        LPCustColors := LPDWORD(@CustomColors);
        Flags := CC_FULLOPEN Or CC_RGBINIT;
      End;
      If Not ChooseColor(@colorsel) Then
        Assert(False, 'ERROR: [Win32DialogOKclickedCB] got invalid color');
      Col.red := GetRValue(colorsel.RGBResult);
      Col.green := GetGValue(colorsel.RGBResult);
      Col.blue := GetBValue(colorsel.RGBResult);

      TColorDialog(theDialog).Color := TColor(Col);
   end
   else if theDialog is TFontDialog then
   begin
     Assert(False, 'Trace:Prssed OK in FontDialog');
     ZeroMemory(@FontSel, SizeOf(FontSel));
     With FontSel Do
     Begin
       LStructSize := SizeOf(FontSel);
       HWNDOwner := Win32Control^.Window;
       LPLogFont := @LF;
       Flags := CF_InitToLogFontStruct Or CF_ScreenFonts Or CF_Effects Or CF_ForceFontExist;
     End;
       
     // FontName := gtk_font_selection_dialog_get_font_name(pgtkfontselectiondialog(FPointer));
     TFontDialog(theDialog).FontName := FontSel.LPLogFont^.LFFaceName;
     Assert(False, 'Trace:-----'+TFontDialog(theDialog).FontName+'----');
   end;
   {
//   gtk_grab_remove(PgtkWidget(TCommonDialog(data).Handle));
   if theDialog is TFileDialog then
   begin
      TFileDialog(data).FileName := gtk_file_selection_get_filename(PGtkFileSelection(FPointer));
   end
   else if theDialog is TColorDialog then
   begin
      colorSel := GTK_COLOR_SELECTION((GTK_COLOR_SELECTION_DIALOG(FPointer))^.colorsel);
      gtk_color_selection_get_color(colorsel, @colorArray[0]);
      newColor.pixel := 0;
      newColor.red := Trunc(colorArray[0] * $FFFF);
      newColor.green := Trunc(colorArray[1] * $FFFF);
      newColor.blue := Trunc(colorArray[2] * $FFFF);

      TColorDialog(theDialog).Color := TGDKColorToTColor(newcolor);
   end
   else if theDialog is TFontDialog then
   begin
     Assert(False, 'Trace:Prssed OK in FontDialog');
     FontName := gtk_font_selection_dialog_get_font_name(pgtkfontselectiondialog(FPointer));
     TFontDialog(theDialog).FontName := FontName;
     Assert(False, 'Trace:-----'+TFontDialog(theDialog).FontName+'----');
   end;}

   theDialog.UserChoice := mrOK;
end;

function Win32DialogCancelclickedCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   theDialog : TCommonDialog;
begin
   Result := True;
   theDialog := TCommonDialog(data);
   if theDialog is TFileDialog then
   begin
      TFileDialog(data).FileName := '';
   end;
   theDialog.UserChoice := mrCancel;
end;

function Win32DialogDestroyCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   theDialog : TCommonDialog;
begin
   Result := True;
   theDialog := TCommonDialog(data);
//   gtk_grab_remove(PgtkWidget(TCommonDialog(data).Handle));
 {  if theDialog is TFileDialog then
   begin
      TFileDialog(data).FileName := '';
   end;
}
 theDialog.UserChoice := -1;
end;

function Win32pressedCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
  Mess : TLMessage;
begin
  Result := True;
  EventTrace('pressed', data);
  Mess.msg := LM_PRESSED;
  Result := DeliverMessage(Data, Mess) = 0;
end;

function Win32enterCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   Mess : TLMessage;
begin
   Result := True;
   EventTrace('enter', data);
   Mess.msg := LM_ENTER;
   Result := DeliverMessage(Data, Mess) = 0;
end;

function Win32leaveCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
  Mess : TLMessage;
begin
  Result := True;
  EventTrace('leave', data);
  Mess.msg := LM_LEAVE;
  Result := DeliverMessage(Data, Mess) = 0;
end;

function Win32movecursorCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
  Mess : TLMessage;
begin
  Result := True;
  EventTrace('move-cursor', data);
  Mess.msg := LM_MOVECURSOR;
  Result := DeliverMessage(Data, Mess) = 0;
end;

function Win32size_allocateCB( Win32Control: PWin32Control; {!}{size :pGtkAllocation;} data: wPointer) : Boolean; cdecl;
var
MessI : Integer;
msg : TLMResize;
begin
   EventTrace('size-allocate', data);
   MessI := LM_WINDOWPOSCHANGED;
   msg.msg := MessI;
{!}//   Msg.Left := Size^.X;
{!}//   Msg.Top := Size^.Y;
{!}//   Msg.Width := Size^.width;
{!}//   Msg.Height := Size^.height;
   Msg.Userdata := Data;
   TObject(data).Dispatch(msg);
end;

function Win32switchpage(Control: PWin32Control; page: PWin32Control; pagenum : integer; data: wPointer) : Boolean; cdecl;
var
   Mess : TLMNotify;
   T : tagNMHDR;
begin
   Result := True;
   EventTrace('switch-page', data);
   Mess.Msg := LM_NOTIFY;
   //this is the
   T.code := TCN_SELCHANGE;//(0-550)-1;
   T.hwndfrom := longint(Control);
   T.idfrom := pagenum;  //use this to set pageindex to the correct page.
   Mess.NMHdr := @T;
   Result := DeliverMessage(Data, Mess) = 0;
end;

function Win32releasedCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
  Mess : TLMEssage;
begin
  Result := True;
  EventTrace('released', data);
  Mess.msg := LM_RELEASED;
  Result := DeliverMessage(Data, Mess) = 0;
end;

function Win32InsertText( Win32Control: PWin32Control; char : pChar; NewTextLength : Integer; {!}{Position : pgint;} data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;
   Msg : TLMInsertText;
   I : Integer;
begin
   EventTrace('Insert Text', data);
   MessI := LM_INSERTTEXT;

   Msg.Msg := MessI;
   Msg.NewText := '';
   For I := 1 to NewTextLength do
    Msg.NewText := Msg.Newtext+Char[i-1];
//   Msg.NewText := String(Char);
   Msg.Length := NewTextLength;
{!}//   Msg.Position := Position^;
   Msg.Userdata := data;
   Result := DeliverMessage(Data, Msg) = 0;
end;

function Win32DeleteText( Win32Control: PWin32Control; Startpos, EndPos : Integer; data: wPointer) : Boolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Delete Text', data);
  Mess.msg := LM_DELETETEXT;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function Win32SetEditable( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Set Editable', data);
  Mess.msg := LM_SETEDITABLE;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function Win32MoveWord( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   Mess : TLMessage;
begin
  EventTrace('Move Word', data);
  Mess.msg := LM_MOVEWORD;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function Win32MovePAge( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Move Page', data);
  Mess.msg := LM_MOVEPAGE;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function Win32MoveToRow( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Move To Row!!', data);
  Mess.msg := LM_MOVETOROW;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function Win32MoveToColumn( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('MoveToColumn', data);
  Mess.msg := LM_MOVETOCOLUMN;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function Win32KillChar( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Kill Char', data);
  Mess.msg := LM_KILLCHAR;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function Win32KillWord( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Kill Word', data);
  Mess.msg := LM_KILLWORD;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function Win32KillLine( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Kill Line', data);
  Mess.msg := LM_KILLLINE;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function Win32CutToClip( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Cut to clip', data);
  Mess.msg := LM_CUTTOCLIP;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function Win32CopyToClip( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Copy to Clip', data);
  Mess.msg := LM_COPYTOCLIP;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function Win32PasteFromClip( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Paste from clip', data);
  Mess.msg := LM_PASTEFROMCLIP;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function Win32MoveResize( Win32Control: PWin32Control; X, Y, Width, Height : PInteger; data: wPointer) : Boolean; cdecl;
var
MessI : Integer;
msg : TLMResize;
begin
   EventTrace('size-allocate', data);
   MessI := LM_USER + 29 {LM_MOVERESIZE};
   msg.msg := MessI;
{!}//   Msg.Left := X^;
{!}//   Msg.Top := Y^;
{!}//   Msg.Width := Width^;
{!}//   Msg.Height := Height^;
   Msg.Userdata := Data;

   TObject(data).Dispatch(msg);
end;

function Win32valuechanged ( Control: PWin32Control; data : wPointer) : Boolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Value changed', data);
  Mess.msg := LM_CHANGED;
  Result := DeliverMessage(Data, Mess) = 0;
end;

{
  Directly call the Timer function of the TTimer object.
  (As far as I know this can't be dispatched like it's done in the other callbacks!)
}
function Win32TimerCB (data : wPointer) : Integer; cdecl;
var
   P : ^TTimer;
begin
  EventTrace('timer', data);
  P := @data;
  {$IFDEF VER1_1}
    P^.Timer(TTimer(data));
  {$ELSE}
    Assert(False, 'TRACE: Cannot dispatch timer in Win32TimerCB');
  {$ENDIF}
  result := 1;                  { returning 0 would stop the timer, 1 will restart it }
end;

function Win32FocusInNotifyCB (Control: PWin32Control; event : Pointer {PGdkEvent}; data : wpointer) : Boolean; cdecl;
var
  MessI : TLMEnter;
begin
  EventTrace ('FocusInNotify (alias Enter)', data);
  MessI.msg := LM_Enter;
  Result:= DeliverMessage(Data, MessI) = 0;
end;

function Win32FocusOutNotifyCB (Control: PWin32Control; event : Pointer {PGdkEvent}; data : wpointer) : Boolean; cdecl;
var
  MessI : TLMExit;
begin
  EventTrace ('FocusOutNotify (alias Exit)', data);
  MessI.msg := LM_Exit;
  Result:= DeliverMessage(Data, MessI) = 0;
end;

{
  Msg : Cardinal;
  ScrollCode : SmallInt;
  Pos : SmallInt;
  ScrollBar : HWND;
  Result : LongInt;
}

function Win32HScrollCB(Scroll: PHandle; data: wPointer): Boolean; cdecl;
var
  Msg: TLMHScroll;
  OldValue,
  V, U, L,
  StepI, PageI, Page: Integer;
  //Scroll: PGTKHScrollBar;  
  X, Y: Integer {GInt};
  Mask: Handle {TGdkModifierType};
  SI: TAGSCROLLINFO;
  R: Windows.RECT;
begin
  //Assert(False, Format('Trace:[GTKHScrollCB] Value: %d', [Round(Adjustment^.Value)]));

  ZeroMemory(@SI, SizeOf(SI));
  With SI Do
  Begin
    CBSize := SizeOf(SCROLLINFO);
    FMask := SIF_PAGE Or SIF_POS Or SIF_RANGE Or SIF_TRACKPOS;
    GetScrollInfo(Scroll^, SB_CTL, SI);
    OldValue := nPos;
    L := nMin;
    U := nMax;
    StepI := nPos;
    PageI := nTrackPos;
    Page := nPage;
  End;
  
  {OldValue := Integer(gtk_object_get_data(PGTKObject(Adjustment), 'OldValue'));
  gtk_object_set_data(PGTKObject(Adjustment), 'OldValue', Pointer(Round(Adjustment^.Value)));
  Scroll := gtk_object_get_data(PGTKObject(Adjustment), 'ScrollBar');

  // Get rounded values
  with Adjustment^ do
  begin
    V := Round(Value);
    U := Round(Upper);
    L := Round(Lower);
    StepI := Round(Step_Increment);
    PageI := Round(Page_Increment);
    Page := Round(Page_Size);
  end;}
  
  // get keystates
  if Scroll <> nil 
  then //gdk_window_get_pointer(PGTKWidget(Scroll)^.Window, @X, @Y, @Mask);
  Windows.GetClientRect(Scroll^, @R);
  X := R.Right - R.Left;
  Y := R.Top - R.Bottom;

  with Msg do
  begin
    msg := LM_HSCROLL;
    // Get scrollcode
    //if ssLeft in GTKEventState2ShiftState(Mask)
    if StepI - U >= 0
    then ScrollCode := SB_THUMBTRACK
    else if V - OldValue = StepI 
    then ScrollCode := SB_LINERIGHT
    else if OldValue - V = StepI
    then ScrollCode := SB_LINELEFT
    else if V - OldValue = PageI 
    then ScrollCode := SB_PAGERIGHT
    else if OldValue - V = PageI
    then ScrollCode := SB_PAGELEFT
    else if V >= U
    then ScrollCode := SB_ENDSCROLL
    else ScrollCode := SB_THUMBPOSITION;
    Pos := V;
    ScrollBar := HWND(Scroll^);
  end;
  Result := DeliverMessage(Data, Msg) = 0;
end;

function Win32VScrollCB(Scroll: PHandle; data: wPointer): Boolean; cdecl;
var
  Msg: TLMVScroll;
  OldValue,
  V, U, L,
  StepI, PageI, Page: Integer;
  //Scroll: PGTKHScrollBar;  
  X, Y: Integer {GInt};
  Mask: Integer {TGdkModifierType};
  SI: TAGSCROLLINFO;
  R: Windows.RECT;
begin
  //Assert(False, Format('Trace:[GTKVScrollCB] Value: %d', [Round(Adjustment^.Value)]));

  ZeroMemory(@SI, SizeOf(SI));
  With SI Do
  Begin
    CBSize := SizeOf(SCROLLINFO);
    FMask := SIF_PAGE Or SIF_POS Or SIF_RANGE Or SIF_TRACKPOS;
    GetScrollInfo(Scroll^, SB_CTL, SI);
    OldValue := nPos;
    L := nMin;
    U := nMax;
    StepI := nPos;
    PageI := nTrackPos;
    Page := nPage;
  End;
  
  {OldValue := Integer(gtk_object_get_data(PGTKObject(Adjustment), 'OldValue'));
  gtk_object_set_data(PGTKObject(Adjustment), 'OldValue', Pointer(Round(Adjustment^.Value)));
  Scroll := gtk_object_get_data(PGTKObject(Adjustment), 'ScrollBar');

  // Get rounded values
  with Adjustment^ do
  begin
    V := Round(Value);
    U := Round(Upper);
    L := Round(Lower);
    StepI := Round(Step_Increment);
    PageI := Round(Page_Increment);
    Page := Round(Page_Size);
  end;}
  
  // get keystates
  if Scroll <> nil 
  then //gdk_window_get_pointer(PGTKWidget(Scroll)^.Window, @X, @Y, @Mask);
  Windows.GetClientRect(Scroll^, @R);
  X := R.Right - R.Left;
  Y := R.Top - R.Bottom;

  with Msg do
  begin
    msg := LM_VSCROLL;
    // Get scrollcode
    //if ssLeft in GTKEventState2ShiftState(Mask)
    if StepI - U >= 0
    then ScrollCode := SB_THUMBTRACK
    else if V - OldValue = StepI 
    then ScrollCode := SB_LINERIGHT
    else if OldValue - V = StepI
    then ScrollCode := SB_LINELEFT
    else if V - OldValue = PageI 
    then ScrollCode := SB_PAGERIGHT
    else if OldValue - V = PageI
    then ScrollCode := SB_PAGELEFT
    else if V >= U
    then ScrollCode := SB_ENDSCROLL
    else ScrollCode := SB_THUMBPOSITION;
    Pos := V;
    ScrollBar := HWND(Scroll^);
  end;
  Result := DeliverMessage(Data, Msg) = 0;
end;

{------------------------------------------------------------------------------
  Function: GTKKeySnooper
  Params:  Widget: The widget for which this event is fired
           Event:  The keyevent data
           FuncData: the user parameter passed when the snooper was installed
  Returns: True if other snoopers shouldn't handled

  Keeps track of which keys are pressed. The keycode is casted to a pointer and
  if it exists in the KeyStateList, it is pressed.
 ------------------------------------------------------------------------------}
function Win32KeySnooper(Win32Control: PWin32Control; Event: PWin32KeyEvent; FuncData: wPointer): Int; cdecl;
type
  PList = ^TList;
var
  Msg: TLMKey;
  KeyCode, VirtKeyCode: Word;
  ListCode: Integer;
  Toggle, Extended, SysKey: Boolean;
begin
  GetWin32KeyInfo(Event, KeyCode, VirtKeyCode, SysKey, Extended, Toggle);
  with Event^ do
  begin
    if VirtKeyCode = VK_UNKNOWN 
    then ListCode := KEYMAP_VKUNKNOWN and KeyCode
    else ListCode := VirtKeyCode;
    if Extended then ListCode := ListCode or KEYMAP_EXTENDED;
    
    case theType of
      WIN32_KEY_PRESS:
        begin
          if PList(FuncData)^.IndexOf(Pointer(ListCode)) = -1 
          then begin
            PList(FuncData)^.Add(Pointer(ListCode));
            if Toggle then PList(FuncData)^.Add(Pointer(ListCode or KEYMAP_TOGGLE));
          end
          else WriteLn(Format('WARNING: [GTKKeySnooper] Pressed key (0x%x) already pressed (LC=0x%x)', [KeyCode, ListCode]));
        end;
      WIN32_KEY_RELEASE:
        begin
          if PList(FuncData)^.Remove(Pointer(ListCode)) = -1 
          then WriteLn(Format('WARNING: [GTKKeySnooper] Released key (0x%x) not pressed (LC=0x%x)', [KeyCode, ListCode]));
          // just remove the togglekey if present
          if not Toggle then PList(FuncData)^.Remove(Pointer(ListCode or KEYMAP_TOGGLE));
        end;
    else
      WriteLn(Format('ERROR: [GTKKeySnooper] Got unknown event %d', [Integer(theType)]));  
    end;


    Assert(False, Format('trace:' + 
//    WriteLN(Format(
      '[GTKKeySnooper] Type %d, window $%x, send_event %d, time %d, state %d, keyval %d, length %d, string %s', 
      [ Integer(thetype), Integer(window), send_event, time, state, keyval, Event^.length, thestring])
    );
  end;
  Result := 0;
end;}

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

{ $I Win32DragCallback.inc}

{
  $Log$
  Revision 1.5  2002/01/25 13:22:56  lazarus
  Keith: Added initial support for events

  Revision 1.4  2002/01/17 03:17:44  lazarus
  Keith: Fixed TPage creation

  Revision 1.3  2002/01/05 13:16:08  lazarus
  MG: win32 interface update from Keith Bowes

  Revision 1.2  2001/08/02 12:58:35  lazarus
  MG: win32 interface patch from Keith Bowes

  Revision 1.1  2000/07/13 10:28:30  michael
  + Initial import
}
