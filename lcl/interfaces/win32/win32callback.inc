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
{$IFOPT C-}
// Uncomment for local trace
  {$C+}
  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

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
Var
  C: Cardinal;
  List: TMsgArray;
  LMessage: TLMessage;
  R: TRect;
  OwnerObject: TObject;
  WinProcess: Boolean;
Begin
  Assert(False, 'Trace:WindowProc - Start');

  Result := 0;
  LMessage.Msg := -1;
  WinProcess := True;

  Assert(False, 'Trace:WindowProc - Getting Object With Callback Procedure');
  OwnerObject := TObject(GetProp(Window, 'Lazarus'));
  Assert(False, 'Trace:WindowProc - Getting Callback Object');

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
          If ((OwnerObject Is TControl) And (Not (OwnerObject Is TButton))) Then
            CallEvent(OwnerObject, TControl(OwnerObject).OnClick, Nil, etNotify)
          Else If OwnerObject Is TMenuItem Then
            LMessage.Msg := LM_ACTIVATE
          Else
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
        Msg := LM_SYSKEYDOWN;
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
    WM_MOUSEHOVER:
    Begin
      LMessage.Msg := LM_ENTER;
    End;
    WM_MOUSELEAVE:
    Begin
      LMessage.Msg := LM_LEAVE;
    End;
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
    WM_MOUSEWHEEL:
    Begin
      With PLMMouseEvent(@LMessage)^ Do
      Begin
        Msg := LM_MOUSEWHEEL;
        WheelDelta := Hi(WParam);
        X := Lo(LParam);
        Y := Hi(LParam);
        State := GetShiftState;
        UserData := Pointer(GetWindowLong(Window, GWL_USERDATA));
      End;
    End;
    WM_MOVE:
    Begin
      Windows.GetWindowRect(Window, @R);
      With TLMMove(LMessage) Do
      Begin
        Msg := LM_MOVE;
        // MoveType := WParam;   WParam is not defined!
        MoveType := 0;
        XPos := R.Left;
        YPos := R.Top;
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
      Windows.GetClientRect(Window, @R);
      With TLMSize(LMessage) Do
      Begin
        Msg := LM_SIZE;
        SizeType := WParam;
        Width := R.Right - R.Left;
        Height := R.Bottom - R.Top;
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
  {$ELSE VER1_1}
    If (OwnerObject <> Nil) And (LMessage.Msg <> -1) Then
      DeliverMessage(OwnerObject, LMessage);
  {$ENDIF VER1_1}

  Writeln();
  
  If WinProcess Then
    Result := DefWindowProc(Window, Msg, WParam, LParam);

  Assert(False, 'Trace:WindowProc - Exit');
End;

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

{
  $Log$
  Revision 1.16  2002/11/15 23:43:54  mattias
  applied patch from Karl Brandt

  Revision 1.15  2002/08/28 17:28:11  lazarus
  Keith: Win32 fixes.  Much appreciation to Markus Lüdin.

  Revision 1.14  2002/08/13 07:08:25  lazarus
  MG: added gdkpixbuf.pp and changes from Andrew Johnson

  Revision 1.13  2002/06/08 19:18:34  lazarus
  Keith: Fixed some bugs that were brought to my attention; fixed compilation problem.

  Revision 1.12  2002/05/10 07:43:48  lazarus
  MG: updated licenses

  Revision 1.11  2002/04/03 01:52:42  lazarus
  Keith: Removed obsolete code, in preperation of a pending TWin32Object cleanup

  Revision 1.10  2002/02/07 08:35:12  lazarus
  Keith: Fixed persistent label captions and a few less noticable things

  Revision 1.9  2002/02/04 10:54:33  lazarus
  Keith:
    * Fixes for Win32
    * Added new listviewtest.pp example

  Revision 1.8  2002/02/03 06:06:25  lazarus
  Keith: Fixed Win32 compilation problems

  Revision 1.7  2002/01/31 09:32:07  lazarus
  Keith:
    * Open and save dialogs can now coexist in apps (however, only one of each type of common dialog can be used per app :( )
    * Fixed make all
    * Fixed crash in Windows 98/ME

  Revision 1.6  2002/01/25 19:42:56  lazarus
  Keith: Improved events and common dialogs on Win32

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
