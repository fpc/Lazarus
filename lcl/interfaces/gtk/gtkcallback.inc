{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}



// temp solution to fill msgqueue
function DeliverPostMessage(const Target: Pointer; var Message): GBoolean;
begin
//writeln('delivermessage');
  if TObject(Target) is TWinControl 
  then begin
    Result := PostMessage(TWinControl(Target).Handle, TLMessage(Message).Msg, TLMessage(Message).WParam, TLMessage(Message).LParam);
  end 
  else begin
    Result := DeliverMessage(Target, Message) = 0;
  end;
end;


{*************************************************************}
{            callback routines                                }
{*************************************************************}

// GTKRealizeCB is used to set extra signal masks after the widget window is created
// define extra events we're interrested in
function GTKRealizeCB(Widget: PGtkWidget; Data: Pointer): GBoolean; cdecl;
begin
  EventTrace('realize', nil);
  gdk_window_set_events(Widget^.Window, gdk_window_get_events(Widget^.Window) or TGdkEventMask(Data));
  Result := True;
end;

function gtkshowCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
   Mess : TLMShowWindow;
begin
   Result := True;
   EventTrace('show', data);
   Mess.Msg := LM_SHOWWINDOW;
   Mess.Show := True;

   Result := DeliverMessage(Data, Mess) = 0;
end;

function gtkHideCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMShowWindow;
begin
  Result := True;
  EventTrace('hide', data);
  Mess.Msg := LM_SHOWWINDOW;
  Mess.Show := False;
  Result := DeliverMessage(Data, Mess) = 0;
end;

function gtkactivateCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
   Mess : TLMessage;
begin
   EventTrace('activate', data);
   Mess.Msg := LM_ACTIVATE;
   Result := DeliverMessage(Data, Mess) = 0;
end;

function gtkchangedCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
   Mess : TLMessage;
begin
   Result := True;
   EventTrace('changed', data);
   Mess.Msg := CM_TEXTCHANGED; //  Changed on 01/04/2001 from LM_CHANGED to CM_TEXTCHANGED by Shane for TCustomEdit
   Result := DeliverMessage(Data, Mess) = 0;
end;

function gtkdraw(Widget: PGtkWidget; area : PgdkRectangle; data: gPointer) : GBoolean; cdecl;
var
   MSG: TLMPaint;
begin
   Result := True;
   EventTrace('draw', data);
   MSG.Msg := LM_PAINT;
   MSG.DC := GetDC(THandle(Widget));
   MSG.Unused := 0;

   Result := DeliverPostMessage(Data, MSG);
//   Result := DeliverMessage(Data, MSG) = 0;
end;

function GTKDrawDefault(Widget: PGTKWidget; Data: gPointer): GBoolean; cdecl;
var
   MSG : TLMPaint;
begin
   Result := True;
   EventTrace('draw', data);
   MSG.Msg := LM_PAINT;
   MSG.DC := GetDC(THandle(Widget));
   MSG.Unused := 0;

   Result := DeliverPostMessage(Data, MSG);
end;


function gtkfrmactivate( widget: PGtkWidget; Event : TgdkEventFocus; data: gPointer) : GBoolean; cdecl;
var
   Mess : TLMActivate;
begin
   EventTrace('activate', data);
   Mess.Msg := LM_ACTIVATE;
   Result := DeliverPostMessage(Data, Mess);
end;

function gtkfrmdeactivate( widget: PGtkWidget; Event : TgdkEventFocus; data: gPointer) : GBoolean; cdecl;
var
   Mess : TLMActivate;
begin
   EventTrace('deactivate', data);
   Mess.Msg := LM_DEACTIVATE;
   Result := DeliverPostMessage(Data, Mess);
end;

function GTKMap(Widget: PGTKWidget; Data: gPointer): GBoolean; cdecl;
var
   Mess : TLMessage;
begin
   Result := True;
   EventTrace('map', data);
end;

function GTKExposeEvent(Widget: PGtkWidget; Event : PGDKEventExpose; Data: gPointer): GBoolean; cdecl;
var
//  Mess : TLMessage;
//  fWindow : pgdkWindow;
//  widget2: pgtkWidget;
//  PixMap : pgdkPixMap;
  
  msg: TLMPaint;
begin
  Result := True;
  EventTrace('expose-event', data);
  
  msg.msg := LM_PAINT; 
  MSG.DC := GetDC(THandle(Widget));
  msg.Unused := 0; 
   
  Result := DeliverPostMessage(Data, MSG);

//  Result := DeliverMessage(Data, msg) = 0;
   
(*    
   
//----------------- Is this still used ??? ---------------   
   Mess.msg := LM_EXPOSEEVENT;
//   Mess.msg := LM_PAINT;
   Widget2 := gtk_Object_get_data(pgtkObject(widget),'Owner');
   If Widget2 = nil then
      PixMap := gtk_object_get_data(PgtkObject(widget),'Pixmap')
   else
      PixMap := gtk_object_get_data(PgtkObject(widget2),'Pixmap');

   If PixMap = nil then begin
     Assert(False, 'Trace:Could not find PixMap!!!!!!!!!!!!');
     Exit;
   end;

   fWindow := pGtkWidget(widget)^.window;


   gdk_draw_pixmap(fwindow,GetPen(pixmap,TColortoTgdkColor(clGray)),
                   pixmap,
                   Event^.Area.x,Event^.Area.y,
                   Event^.Area.x,Event^.Area.y,
                   Event^.Area.Width, Event^.Area.Height);

{Assert(False, 'Trace:X,Y,Width,Height = '+Inttostr(Event^.Area.x)+','+
                                Inttostr(Event^.Area.y)+','+
                                Inttostr(Event^.Area.Width)+','+
                                Inttostr(Event^.Area.Height));
}

  Result := DeliverMessage(Data, Mess);
//-----------------------------------------

*)
end;


function GTKKeyUpDown(Widget: PGtkWidget; Event : pgdkeventkey; Data: gPointer) : GBoolean; cdecl;
var
  Msg: TLMKey;
  KeyCode: Word;
  Flags: Integer;
  Toggle, Extended, SysKey: Boolean;
begin
  GetGTKKeyInfo(Event, KeyCode, Msg.CharCode, SysKey, Extended, Toggle);
//  Assert(False, Format('Trace:[GTKKeyUpDown] Type: %3:d, GTK: 0x%0:x(%0:d) LCL: 0x%1:x(%1:d) VK: 0x%2:x(%2:d)', [Event^.keyval, KeyCode, Msg.CharCode, Event^.theType]));

  Flags := 0;

  if Extended then Flags := KF_EXTENDED;
  if SysKey then Flags := Flags or KF_ALTDOWN;


  Msg.KeyData := $00000000; //TODO: OEM char
  Msg.KeyData := Msg.KeyData or (Flags shl 16);
  
  case Event^.theType of

    GDK_KEY_RELEASE:
      begin
        EventTrace('key up', data);
        if SysKey
        then Msg.msg := LM_SYSKEYUP
        else Msg.msg := LM_KEYUP;

        Flags := KF_UP or KF_REPEAT;

        Msg.KeyData := Msg.KeyData or (Flags shl 16) or $0001 {allways};
        Result := DeliverPostMessage(data, msg);

      end;
    GDK_KEY_PRESS:
      begin
        EventTrace('key down', data);
        if SysKey
        then Msg.msg := LM_SYSKEYDOWN
        else Msg.msg := LM_KEYDOWN;

        Msg.KeyData := Msg.KeyData or 0{TODO: previous keystate} or $0001 {TODO:  repeatcount};
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

(*  Shane 08/11/2000
    I changed the way order the info is sent.
    I send the key info on the DOWN event instead of the UP event.
    GDK_KEY_PRESS:
      begin
        EventTrace('key down', data);
        if SysKey 
        then Msg.msg := LM_SYSKEYDOWN
        else Msg.msg := LM_KEYDOWN;
        Msg.KeyData := Msg.KeyData or 0{TODO: previous keystate} or $0001 {TODO:  repeatcount};
        Result := DeliverPostMessage(data, msg);
      
      end;
    GDK_KEY_RELEASE:
      begin
        EventTrace('key up', data);
        if SysKey 
        then Msg.msg := LM_SYSKEYUP
        else Msg.msg := LM_KEYUP;

        Flags := KF_UP or KF_REPEAT;
        Msg.KeyData := Msg.KeyData or (Flags shl 16) or $0001 {allways};
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
*)
end;

function GTKFocusCB( widget: PGtkWidget; event:PGdkEventFocus; data: gPointer) : GBoolean; cdecl;
var
   Mess : TLMessage;
begin
   EventTrace('focus', data);
   //TODO: fill in old focus
   Mess.msg := LM_SETFOCUS;
   Assert(False, Format('Trace:TODO: [gtkfocusCB] %s  finish', [TObject(Data).ClassName]));
   Result := DeliverMessage(Data,  Mess) = 0;
end;

function GTKKillFocusCB( widget: PGtkWidget; event:PGdkEventFocus; data: gPointer) : GBoolean; cdecl;
var
   Mess : TLMessage;
begin
   EventTrace('killfocus', data);
   Mess.msg := LM_KILLFOCUS;
   //TODO: fill in new focus
   Assert(False, Format('Trace:TODO: [gtkkillfocusCB] %s  finish', [TObject(Data).ClassName]));
   Result := DeliverMessage(Data,  Mess) = 0;
end;

function gtkdestroyCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
   Mess : TLMessage;
begin
   Result := True;
   EventTrace('destroy', data);
   Mess.msg := LM_DESTROY;
   Result := DeliverMessage(Data, Mess) = 0;
end;

function gtkdeleteCB( widget : PGtkWidget; event : PGdkEvent; data : gPointer) : GBoolean; cdecl;
var Mess : TLMessage;
begin
  Mess.Msg:= LM_CLOSEQUERY;
  { Message results : True - do nothing, False - destroy or hide window }
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkresizeCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
   Mess : TLMessage;
begin
   Result := True;
   EventTrace('resize', data);
//   Mess.msg := LM_RESIZE;
//   TControl(data).WindowProc(TLMessage(Mess));
 Assert(False, 'Trace:TODO: [gtkresizeCB] fix (or remove) to new LM_SIZE');
//TObject(data).Dispatch(Mess);
end;

function GTKMotionNotify(widget:PGTKWidget; event: PGDKEventMotion; data: gPointer):GBoolean; cdecl;
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

end;

function gtkMouseBtnPress( widget: PGtkWidget; event : pgdkEventButton; data: gPointer) : GBoolean; cdecl;
const
  WHEEL_DELTA : array[Boolean] of Integer = (-1, 1);
var
  MessI : TLMMouse;
  MessE : TLMMouseEvent;
  ShiftState: TShiftState;
begin
  EventTrace('Mouse button Press', data);

  Assert(False, Format('Trace:[gtkMouseBtnPress] ', []));
   
  ShiftState := GTKEventState2ShiftState(Event^.State);
   
  if event^.Button in [4,5]
  then begin
    MessE.Msg := LM_MOUSEWHEEL;
    MessE.WheelDelta := WHEEL_DELTA[event^.Button = 4];
    MessE.X := Trunc(Event^.X);
    MessE.Y := trunc(Event^.Y);
    MessE.State := ShiftState;
    MessE.UserData := Data;
    Result := DeliverPostMessage(Data, MessE);
  end
   else
       begin
       case event^.Button of
         1 :     if event^.thetype = gdk_button_press then   MessI.Msg := LM_LBUTTONDOWN
                  else
                     MessI.Msg := LM_LBUTTONDBLCLK;
         2 :     if event^.thetype = gdk_button_press then   MessI.Msg := LM_MBUTTONDOWN
                  else
                     MessI.Msg := LM_MBUTTONDBLCLK;
         3 :     if event^.thetype = gdk_button_press then   MessI.Msg := LM_RBUTTONDOWN
                  else
                     MessI.Msg := LM_RBUTTONDBLCLK;
         else MessI.Msg := LM_NULL;
       end; //case
    MessI.XPos := Trunc(Event^.X);
    MessI.YPos := Trunc(Event^.Y);
    MessI.Keys := 0;
    if ssShift in ShiftState then MessI.Keys := MessI.Keys or MK_SHIFT;
    if ssCtrl in ShiftState then MessI.Keys := MessI.Keys or MK_CONTROL;
    if ssLeft in ShiftState then MessI.Keys := MessI.Keys or MK_LBUTTON;
    if ssRight in ShiftState then MessI.Keys := MessI.Keys or MK_RBUTTON;
    if ssMiddle in ShiftState then MessI.Keys := MessI.Keys or MK_MBUTTON;

    if MessI.Msg <> LM_NULL then Result := DeliverPostMessage(Data, MessI); 
  end;
end;

function gtkMouseBtnRelease( widget: PGtkWidget; event : pgdkEventButton; data: gPointer) : GBoolean; cdecl;
var
  MessI : TLMMouse;
  ShiftState: TShiftState;
begin
  EventTrace('Mouse button release', data);

  Assert(False, Format('Trace:[gtkMouseBtnRelease] ', []));

  ShiftState := gtkeventstate2shiftstate(Event^.State);

  case event^.Button of
    1 : MessI.Msg := LM_LBUTTONUP;
    2 : MessI.Msg := LM_MBUTTONUP;
    3 : MessI.Msg := LM_RBUTTONUP;
    else MessI.Msg := LM_NULL;
  end;
  MessI.XPos := Trunc(Event^.X);
  MessI.YPos := Trunc(Event^.Y);
  MessI.Keys := 0;
  if ssShift in ShiftState then MessI.Keys := MessI.Keys or MK_SHIFT;
  if ssCtrl in ShiftState then MessI.Keys := MessI.Keys or MK_CONTROL;
  if ssLeft in ShiftState then MessI.Keys := MessI.Keys or MK_LBUTTON;
  if ssRight in ShiftState then MessI.Keys := MessI.Keys or MK_RBUTTON;
  if ssMiddle in ShiftState then MessI.Keys := MessI.Keys or MK_MBUTTON;

  if MessI.Msg <> LM_NULL 
  then Result := DeliverPostMessage(Data, MessI)
  else Result := True;
end;

function gtkclickedCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
   Mess : TLMessage;
begin
(*
   Result := True;
   EventTrace('clicked', data);
   Assert(False, Format('Trace:OBSOLETE: [gtkclickedCB] ', []));
   Mess.msg := LM_CLICKED;
   Result:= DeliverMessage(Data, Mess) = 0;
*)
  Result := True;
end;

function gtkDialogOKclickedCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
   theDialog : TCommonDialog;
   Fpointer : Pointer;
   colorArray : array[0..2] of double;
   colorsel : GTK_COLOR_SELECTION;
   newColor : TGdkColor;
   FontName : String;
begin
   Result := True;
   theDialog := TCommonDialog(data);
   FPointer := Pointer(theDialog.Handle);
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
   end;

   theDialog.UserChoice := mrOK;
 end;

function gtkDialogCancelclickedCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
   theDialog : TCommonDialog;
begin
   Result := True;
   theDialog := TCommonDialog(data);
//   gtk_grab_remove(PgtkWidget(TCommonDialog(data).Handle));
   if theDialog is TFileDialog then
   begin
      TFileDialog(data).FileName := '';
   end;
   theDialog.UserChoice := mrCancel;

 end;

function gtkDialogDestroyCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
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

function gtkpressedCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  Result := True;
  EventTrace('pressed', data);
  Mess.msg := LM_PRESSED;
  Result := DeliverMessage(Data, Mess) = 0;
end;

function gtkenterCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
   Mess : TLMessage;
begin
   Result := True;
   EventTrace('enter', data);
   Mess.msg := LM_ENTER;
   Result := DeliverMessage(Data, Mess) = 0;
end;

function gtkleaveCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  Result := True;
  EventTrace('leave', data);
  Mess.msg := LM_LEAVE;
  Result := DeliverMessage(Data, Mess) = 0;
end;

function gtkmovecursorCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  Result := True;
  EventTrace('move-cursor', data);
  Mess.msg := LM_MOVECURSOR;
  Result := DeliverMessage(Data, Mess) = 0;
end;

function gtksize_allocateCB( widget: PGtkWidget; size :pGtkAllocation; data: gPointer) : GBoolean; cdecl;
var
  PosMsg : TLMWindowPosChanged;
  SizeMsg: TLMSize;
  MoveMsg: TLMMove;
begin
  EventTrace('size-allocate', data);

  PosMsg.msg := LM_WINDOWPOSCHANGED; //LM_SIZEALLOCATE;
  PosMsg.Result := -1;
  SizeMsg.Result := -1;
  MoveMsg.Result := -1;
  
  with Size^ do Assert(False, Format('Trace:[gtksize_allocateCB] %s --> X: %d, Y: %d, Width: %d, Height: %d', [TObject(data).ClassName, X, Y, Width, Height]));
  
  New(PosMsg.WindowPos);
  try
    with PosMsg.WindowPos^ do
    begin
      if TObject(data) is TWinControl 
      then hWnd := TWinControl(data).Handle
      else hWnd := 0;
      hWndInsertAfter := 0;
      x := Size^.X;
      y := Size^.Y;
      cx := Size^.Width;
      cy := Size^.Height;
      flags := 0;
    end;
    Assert(False, 'Trace:[gtksize_allocateCB] DeliverMessage LM_WINDOWPOSCHANGED');
    Result := DeliverMessage(Data, PosMsg) = 0;
  finally
    Dispose(PosMsg.WindowPos);
  end;
  
  if (PosMsg.Result <> 0 ) then 
  begin
    // message is not handled. A LM_SIZE and LM_MOVE should be sent
    with SizeMsg do
    begin
      Msg := LM_SIZE;
      SizeType := 0; //?? -->SIZE_RESTORED
      Width := Size^.Width;
      Height := Size^.Height;
    end;
    Assert(False, 'Trace:[gtksize_allocateCB] DeliverMessage LM_SIZE');
    Result := DeliverPostMessage(Data, SizeMsg) or Result;
    
    with MoveMsg do
    begin
      Msg := LM_MOVE;
      XPos := Size^.X;
      YPos := Size^.Y;
    end;
    Assert(False, 'Trace:[gtksize_allocateCB] DeliverMessage LM_MOVE');
    Result := DeliverPostMessage(Data,  MoveMsg) or Result;
  end;
  
end;

function gtkswitchpage(widget: PGtkWidget; page: Pgtkwidget; pagenum : integer; data: gPointer) : GBoolean; cdecl;
var
   Mess : TLMNotify;
   T : tagNMHDR;
begin
   Result := True;
   EventTrace('switch-page', data);
   Mess.Msg := LM_NOTIFY;
   //this is the
   T.code := TCN_SELCHANGE;//(0-550)-1;
   T.hwndfrom := longint(widget);
   T.idfrom := pagenum;  //use this to set pageindex to the correct page.
   Mess.NMHdr := @T;
   Result := DeliverMessage(Data, Mess) = 0;
end;


function gtkconfigureevent( widget: PGtkWidget; event : PgdkEventConfigure; data: gPointer) : GBoolean; cdecl;
var 
  Allocation : PGtkAllocation;
begin

  { This signal is emitted for top level controls only, i.e. only controls
    that are not children. Thus, we register this event only for forms.
    This event is fired when the form is sized, moved or changes Z order.
    TODO : We may yet have to figure a way how not to call the WindowPosChanged
    twice in some cases. It may also be that this event is called whenever
    one of the controls of the form gets resized !!! }

  New(Allocation);
  try
    with Allocation^ do begin
      X:= Event^.X;
      Y:= Event^.Y;
      Width:= Event^.Width;
      Height:= Event^.Height;
    end;  
    Result:= gtksize_allocateCB( Widget, Allocation, Data);
  finally
    Dispose(Allocation);
  end;    


    
//   Result := True;
//   Mess.msg := LM_CONFIGUREEVENT;

        //Get the widget owner because the 'fixed' widget called the signal
        //create a pixmap for drawing behind the scenes
{        PixMap := gtk_object_get_data(PgtkObject(widget),'Pixmap');
        if Assigned(PixMap) then
        gdk_pixmap_unref(pixmap);

        PixMap := gdk_pixmap_new(pgtkwidget(widget)^.window, pgtkwidget(widget)^.allocation.width, pgtkwidget(widget)^.allocation.height,-1);
}
        //assign it to the object data for this widget so it's stored there
        //Clear the canvas area

{            PenColor := TCustomForm(data).Color;
        gdk_draw_rectangle(pixmap,GetPen(pixmap,TColortoTgdkColor(PenColor)),1,0,0,pgtkwidget(widget)^.allocation.width,pgtkwidget(widget)^.allocation.height);
//        Assert(False, 'Trace:Storing Pixmap');
        if widget <> nil then
        gtk_Object_set_Data(PgtkObject(widget),'Pixmap',Pixmap);


//  Mess.msg := LM_PAINT;  //Should I be sending the Draw event??????????
  
  Result:= DeliverMessage(Data, Mess);
}  
  Assert(False, 'Trace:Exiting Configure');

end;

function gtkreleasedCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMEssage;
begin
  Result := True;
  EventTrace('released', data);
  Mess.msg := LM_RELEASED;
  Result := DeliverMessage(Data, Mess) = 0;
end;

function gtkInsertText( widget: PGtkWidget; char : pChar; NewTextLength : Integer; Position : pgint; data: gPointer) : GBoolean; cdecl;
var
   Msg : TLMInsertText;
   I : Integer;
begin
   Result := True;
   EventTrace('Insert Text', data);

   Msg.Msg := LM_INSERTTEXT;
   
   Msg.NewText := '';
   For I := 0 to NewTextLength - 1 do
        Msg.NewText := Msg.Newtext+Char[i];

   //Msg.NewText := String(Char);
   Msg.Length := NewTextLength;
   Msg.Position := Position^;
   Msg.Userdata := data;
   Result:= DeliverMessage(Data, Msg) = 0;
end;

function gtkDeleteText( widget: PGtkWidget; Startpos, EndPos : Integer; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Delete Text', data);
  Mess.msg := LM_DELETETEXT;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkSetEditable( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Set Editable', data);
  Mess.msg := LM_SETEDITABLE;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkMoveWord( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
   Mess : TLMessage;
begin
  EventTrace('Move Word', data);
  Mess.msg := LM_MOVEWORD;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkMovePage( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Move Page', data);
  Mess.msg := LM_MOVEPAGE;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkMoveToRow( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Move To Row!!', data);
  Mess.msg := LM_MOVETOROW;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkMoveToColumn( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('MoveToColumn', data);
  Mess.msg := LM_MOVETOCOLUMN;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkKillChar( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Kill Char', data);
  Mess.msg := LM_KILLCHAR;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkKillWord( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Kill Word', data);
  Mess.msg := LM_KILLWORD;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkKillLine( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Kill Line', data);
  Mess.msg := LM_KILLLINE;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkCutToClip( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Cut to clip', data);
  Mess.msg := LM_CUTTOCLIP;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkCopyToClip( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Copy to Clip', data);
  Mess.msg := LM_COPYTOCLIP;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

function gtkPasteFromClip( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Paste from clip', data);
  Mess.msg := LM_PASTEFROMCLIP;
  Result:= DeliverMessage(Data, Mess) = 0;
end;

(*
function gtkMoveResize(widget: PGtkWidget; X,Y,Width,Height : pgInt; data: gPointer) : GBoolean; cdecl;
var
  msg : TLMResize;
begin
  Result := True;
  EventTrace('move_resize', data);
  msg.msg := LM_MOVERESIZE;
  Msg.Left := X^;
  Msg.Top := Y^;
  Msg.Width := Width^;
  Msg.Height := Height^;
  Msg.Userdata := Data;
  
  if TObject(data) is TControl 
  then TControl(data).WindowProc(TLMessage(msg))
  else TObject(data).Dispatch(msg);
end;
*)

function gtkvaluechanged ( widget: PGtkWidget; data : gPointer) : GBoolean; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace('Value changed', data);
  Mess.msg := LM_CHANGED;
  Result := DeliverMessage(Data, Mess) = 0;
end;

{ Directly call the Timer function of the TTimer object.
  (As far as I know this can't be dispatched like it's done in the other callbacks!) }
  
// MWE: Post a LM_Timer message !!!  

function gtkTimerCB (data : gpointer) : gint; cdecl;
var
   P : ^TTimer;
begin
  EventTrace('timer', data);
  P := @data;
  P^.Timer(TTimer(data));
  result := 1;                  { returning 0 would stop the timer, 1 will restart it }
end;

function gtkFocusInNotifyCB (widget : PGtkWidget; event : PGdkEvent; data : gpointer) : GBoolean; cdecl;
var
  MessI : TLMEnter;
begin
  EventTrace ('FocusInNotify (alias Enter)', data);
  MessI.msg := LM_Enter;
  Result:= DeliverMessage(Data, MessI) = 0;
end;

function gtkFocusOutNotifyCB (widget : PGtkWidget; event : PGdkEvent; data : gpointer) : GBoolean; cdecl;
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

function GTKHScrollCB(Adjustment: PGTKAdjustment; data: GPointer): GBoolean; cdecl;
var
  Msg: TLMHScroll;
  OldValue,
  V, U, L,
  StepI, PageI, Page: Integer;
  Scroll: PGTKHScrollBar;  
  X, Y: GInt;
  Mask: TGdkModifierType;
begin
  Assert(False, Format('Trace:[GTKHScrollCB] Value: %d', [Round(Adjustment^.Value)]));

  OldValue := Integer(gtk_object_get_data(PGTKObject(Adjustment), 'OldValue'));
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
  end;
  
  // get keystates
  if Scroll <> nil 
  then gdk_window_get_pointer(PGTKWidget(Scroll)^.Window, @X, @Y, @Mask);

  with Msg do
  begin
    msg := LM_HSCROLL;
    // Get scrollcode
    if ssLeft in GTKEventState2ShiftState(Mask)
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
    ScrollBar := HWND(Scroll);
  end;
  Result := DeliverMessage(Data, Msg) = 0;
end;

function GTKVScrollCB(Adjustment: PGTKAdjustment; data: GPointer): GBoolean; cdecl;
var
  Msg: TLMVScroll;
  OldValue,
  V, U, L,
  StepI, PageI, Page: Integer;
  Scroll: PGTKHScrollBar;  
  X, Y: GInt;
  Mask: TGdkModifierType;
begin
  Assert(False, Format('Trace:[GTKVScrollCB] Value: %d', [Round(Adjustment^.Value)]));

  OldValue := Integer(gtk_object_get_data(PGTKObject(Adjustment), 'OldValue'));
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
  end;
  
  // get keystates
  if Scroll <> nil 
  then gdk_window_get_pointer(PGTKWidget(Scroll)^.Window, @X, @Y, @Mask);

  with Msg do
  begin
    msg := LM_VSCROLL;
    // Get scrollcode
    if ssLeft in GTKEventState2ShiftState(Mask)
    then ScrollCode := SB_THUMBTRACK
    else if V - OldValue = StepI 
    then ScrollCode := SB_LINEDOWN
    else if OldValue - V = StepI
    then ScrollCode := SB_LINEUP
    else if V - OldValue = PageI 
    then ScrollCode := SB_PAGEDOWN
    else if OldValue - V = PageI
    then ScrollCode := SB_PAGEUP
    else if V >= U
    then ScrollCode := SB_ENDSCROLL
    else ScrollCode := SB_THUMBPOSITION;
    Pos := V;
    ScrollBar := HWND(Scroll);
  end;
  Result := DeliverMessage(Data, Msg) = 0;
end;

procedure EventTrace(message : string; data : pointer);
begin
  if Data = nil 
  then Assert(False, Format('Trace:Event [%s] fired',[message]))
  else Assert(False, Format('Trace:Event [%s] fired for %s',[message, TObject(data).Classname]));
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
function GTKKeySnooper(Widget: PGtkWidget; Event: PGdkEventKey; FuncData: gPointer): gInt; cdecl;
type
  PList = ^TList;
var
  Msg: TLMKey;
  KeyCode, VirtKeyCode: Word;
  ListCode: Integer;
  Toggle, Extended, SysKey: Boolean;
begin
  GetGTKKeyInfo(Event, KeyCode, VirtKeyCode, SysKey, Extended, Toggle);
  with Event^ do
  begin
    if VirtKeyCode = VK_UNKNOWN 
    then ListCode := KEYMAP_VKUNKNOWN and KeyCode
    else ListCode := VirtKeyCode;
    if Extended then ListCode := ListCode or KEYMAP_EXTENDED;
    
    case theType of
      GDK_KEY_PRESS:
        begin
          if PList(FuncData)^.IndexOf(Pointer(ListCode)) = -1 
          then begin
            PList(FuncData)^.Add(Pointer(ListCode));
            if Toggle then PList(FuncData)^.Add(Pointer(ListCode or KEYMAP_TOGGLE));
          end
          else WriteLn(Format('WARNING: [GTKKeySnooper] Pressed key (0x%x) already pressed (LC=0x%x)', [KeyCode, ListCode]));
        end;
      GDK_KEY_RELEASE:
        begin
          if PList(FuncData)^.Remove(Pointer(ListCode)) = -1 
          then WriteLn(Format('WARNING: [GTKKeySnooper] Released key (0x%x) not pressed (LC=0x%x)', [KeyCode, ListCode]));
          // just remove the togglekey if present
          if not Toggle then PList(FuncData)^.Remove(Pointer(ListCode or KEYMAP_TOGGLE));
        end;
    else
      WriteLn(Format('ERROR: [GTKKeySnooper] Got unknown event %d', [theType]));  
    end;


    Assert(False, Format('trace:' + 
//    WriteLN(Format(
      '[GTKKeySnooper] Type %d, window $%x, send_event %d, time %d, state %d, keyval %d, length %d, string %s', 
      [ thetype, Integer(window), send_event, time, state, keyval, length, thestring])
    );
  end;
  Result := 0;
end;

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

{$I gtkDragCallback.inc}



{ =============================================================================

  $Log$
  Revision 1.12  2001/01/10 20:12:29  lazarus
  Added the Nudge feature to the IDE.
  Shane

  Revision 1.11  2001/01/09 18:23:21  lazarus
  Worked on moving controls.  It's just not working with the X and Y coord's I'm getting.
  Shane

  Revision 1.10  2001/01/04 15:09:05  lazarus
  Tested TCustomEdit.Readonly, MaxLength and CharCase.
  Shane

  Revision 1.9  2000/12/19 18:43:13  lazarus
  Removed IDEEDITOR.  This causes the PROJECT class to not function.
  Saving projects no longer works.

  I added TSourceNotebook and TSourceEditor.  They do all the work for saving/closing/opening units.  Somethings work but they are in early development.
  Shane

  Revision 1.8  2000/11/29 21:22:35  lazarus
  New Object Inspector code
  Shane

  Revision 1.7  2000/10/09 22:50:32  lazarus
  MWE:
    * fixed some selection code
    + Added selection sample

  Revision 1.6  2000/09/10 23:08:31  lazarus
  MWE:
    + Added CreateCompatibeleBitamp function
    + Updated TWinControl.WMPaint
    + Added some checks to avoid gtk/gdk errors
    - Removed no fixed warning from GetDC
    - Removed some output

  Revision 1.5  2000/08/28 14:23:49  lazarus
  Added a few files for the start of creating classes for the editor.  [SHANE]

  Revision 1.4  2000/08/11 14:59:09  lazarus
  Adding all the Synedit files.
  Changed the GDK_KEY_PRESS and GDK_KEY_RELEASE stuff to fix the problem in the editor with the shift key being ignored.
  Shane

  Revision 1.3  2000/08/10 10:55:45  lazarus
  Changed TCustomDialog to TCommonDialog
  Shane

  Revision 1.2  2000/07/30 21:48:33  lazarus
  MWE:
    = Moved ObjectToGTKObject to GTKProc unit
    * Fixed array checking in LoadPixmap
    = Moved LM_SETENABLED to API func EnableWindow and EnableMenuItem
    ~ Some cleanup

  Revision 1.1  2000/07/13 10:28:29  michael
  + Initial import

  Revision 1.12  2000/06/29 18:08:56  lazarus
  Shane
    Looking for the editor problem I made a few changes.  I changed everything back to the original though.

  Revision 1.10  2000/06/19 18:21:22  lazarus
  Spinedit was never getting created
  Shane

  Revision 1.9  2000/06/16 13:33:22  lazarus
  Created a new method for adding controls to the toolbar to be dropped onto the form!
  Shane

  Revision 1.8  2000/05/17 22:34:07  lazarus
  MWE:
    * Fixed Sizing & events

  Revision 1.7  2000/05/11 22:04:15  lazarus
  MWE:
    + Added messagequeue
    * Recoded SendMessage and Peekmessage
    + Added postmessage
    + added DeliverPostMessage

  Revision 1.6  2000/05/10 22:52:58  lazarus
  MWE:
    = Moved some global api stuf to gtkobject

  Revision 1.5  2000/05/09 02:05:08  lazarus
  Replaced writelns with Asserts.                        CAW

  Revision 1.4  2000/05/08 15:56:59  lazarus
  MWE:
    + Added support for mwedit92 in Makefiles
    * Fixed bug # and #5 (Fillrect)
    * Fixed labelsize in ApiWizz
    + Added a call to the resize event in WMWindowPosChanged

  Revision 1.3  2000/04/18 14:02:32  lazarus
  Added Double Clicks.  Changed the callback in gtkcallback for the buttonpress event to check the event type.
  Shane

  Revision 1.2  2000/03/31 18:41:03  lazarus
  Implemented MessageBox / Application.MessageBox calls. No icons yet, though...

  Revision 1.1  2000/03/30 22:51:42  lazarus
  MWE:
    Moved from ../../lcl

  Revision 1.58  2000/03/24 14:40:41  lazarus
  A little polishing and bug fixing.

  Revision 1.57  2000/03/23 20:40:03  lazarus
  Added some drag code
  Shane

  Revision 1.56  2000/03/15 00:51:57  lazarus
  MWE:
    + Added LM_Paint on expose
    + Added forced creation of gdkwindow if needed
    ~ Modified DrawFrameControl
    + Added BF_ADJUST support on DrawEdge
    - Commented out LM_IMAGECHANGED in TgtkObject.IntSendMessage3
       (It did not compile)

  Revision 1.55  2000/03/08 23:57:38  lazarus
  MWE:
    Added SetSysColors
    Fixed TEdit text bug (thanks to hans-joachim ott <hjott@compuserve.com>)
    Finished GetKeyState
    Added changes from Peter Dyson <peter@skel.demon.co.uk>
    - a new GetSysColor
    - some improvements on ExTextOut

  Revision 1.54  2000/03/03 22:58:26  lazarus
  MWE:
    Fixed focussing problem.
      LM-FOCUS was bound to the wrong signal
    Added GetKeyState api func.
      Now LCL knows if shift/trl/alt is pressed (might be handy for keyboard
      selections ;-)

  Revision 1.53  2000/02/28 19:16:04  lazarus
  Added code to the FILE CLOSE to check if the file was modified.  HAven't gotten the application.messagebox working yet though.  It won't stay visible.
  Shane

  Revision 1.52  2000/02/24 21:15:30  lazarus
  Added TCustomForm.GetClientRect and RequestAlign to try and get the controls to align correctly when a MENU is present.  Not Complete yet.

  Fixed the bug in TEdit that caused it not to update it's text property.  I will have to
  look at TMemo to see if anything there was affected.

  Added SetRect to WinAPI calls
  Added AdjustWindowRectEx to WINAPI calls.
  Shane

  Revision 1.51  2000/02/22 23:26:12  lazarus
  MWE: Fixed cursor movement in editor
       Started on focus problem

  Revision 1.50  2000/02/22 22:19:49  lazarus
  TCustomDialog is a descendant of TComponent.
  Initial cuts a form's proper Close behaviour.

  Revision 1.49  2000/02/19 18:11:59  lazarus
  More work on moving, resizing, forms' border style etc.

  Revision 1.48  2000/02/18 19:38:52  lazarus
  Implemented TCustomForm.Position
  Better implemented border styles. Still needs some tweaks.
  Changed TComboBox and TListBox to work again, at least partially.
  Minor cleanups.

  Revision 1.47  2000/01/31 20:00:21  lazarus
  Added code for Application.ProcessMessages.  Needs work.
  Added TScreen.Width and TScreen.Height.  Added the code into
  GetSystemMetrics for these two properties.
  Shane

  Revision 1.46  2000/01/22 20:07:46  lazarus
  Some cleanups. It needs much more cleanup than this.
  Worked around a compiler bug (?) in mwCustomEdit.
  Reverted some changes to font generation and increased font size.

  Revision 1.45  2000/01/16 23:23:04  lazarus
  MWE:
    Added/completed scrollbar API funcs

  Revision 1.44  2000/01/14 00:33:31  lazarus
  MWE:
    Added Scrollbar messages

  Revision 1.43  2000/01/13 22:44:05  lazarus
  MWE:
    Created/updated net gtkwidget for TWinControl decendants
      also improved foccusing on such a control

  Revision 1.42  2000/01/10 00:07:12  lazarus
  MWE:
    Added more scrollbar support for TWinControl
    Most signals for TWinContorl are jet connected to the wrong widget
      (now scrolling window, should be fixed)
    Added some cvs entries

  Revision 1.41  2000/01/04 23:12:46  lazarus
  MWE:
    Fixed LM_CHAR message. It is now after the LM_KEYUP message
    Fixed Menus at checkbox example.
    Removed references to TTabbedNtBK (somebody removed the files) and
      chanched it on the compileroptions form

  Revision 1.40  2000/01/04 21:00:34  lazarus
  *** empty log message ***

  Revision 1.39  2000/01/03 00:19:21  lazarus
  MWE:
    Added keyup and buttonup events
    Added LM_MOUSEMOVE callback
    Started with scrollbars in editor

  Revision 1.38  1999/12/31 14:58:00  lazarus
  MWE:
    Set unkown VK_ codesto 0
    Added pfDevice support for bitmaps

  Revision 1.37  1999/12/29 00:04:47  lazarus
  MWE:
    Refined key events. TODO get vk keycodes for non alpha keys

  Revision 1.36  1999/12/28 01:10:53  lazarus
  MWE:
    Added most common virtual keycodes

  Revision 1.35  1999/12/22 01:16:03  lazarus
  MWE:
    Changed/recoded keyevent callbacks
    We Can Edit!
    Commented out toolbar stuff

  Revision 1.34  1999/12/18 18:27:31  lazarus
  MWE:
    Rearranged some events to get a LM_SIZE, LM_MOVE and LM_WINDOWPOSCHANGED
    Initialized the TextMetricstruct to zeros to clear unset values
    Get mwEdit to show more than one line
    Fixed some errors in earlier commits

  Revision 1.33  1999/12/09 01:21:40  lazarus
  MWE:
    Added check for TControl in LM_ACTIVATE

  Revision 1.32  1999/12/08 21:42:36  lazarus
  Moved more messages over to wndproc.
  Shane

  Revision 1.31  1999/12/07 01:19:25  lazarus
  MWE:
    Removed some double events
    Changed location of SetCallBack
    Added call to remove signals
    Restructured somethings
    Started to add default handlers in TWinControl
    Made some parts of TControl and TWinControl more delphi compatible
    ... and lots more ...

  Revision 1.30  1999/11/30 21:30:06  lazarus
  Minor Issues
  Shane

  Revision 1.29  1999/11/23 22:06:27  lazarus
  Minor changes to get it running again with the latest compiler.  There is something wrong with the compiler that is preventing certain things from working.
  Shane

  Revision 1.28  1999/11/05 17:48:17  lazarus
  Added a mwedit1 component to lazarus (MAIN.PP)
  It crashes on create.
  Shane

  Revision 1.27  1999/11/04 21:52:08  lazarus
  wndproc being used a little
  Shane

  Revision 1.26  1999/11/02 16:02:34  lazarus
  Added a bunch of wndproc stuff and a lot of functions that really don't do a thing at this point.
  Shane

  Revision 1.25  1999/10/30 12:30:02  peter
    * fixed some stupid crashes

  Revision 1.24  1999/10/28 17:17:42  lazarus
  Removed references to FCOmponent.
  Shane

  Revision 1.23  1999/10/04 23:31:38  lazarus
  Cleaned up the code to make it more readable.    CAW

  Revision 1.22  1999/09/26 13:30:15  lazarus

     Implemented OnEnter & OnExit events for TTrackbar. These properties
     and handler functions have been added to TWincontrol, two new
     callbacks have been added to gtkcallback.
      stoppok

  Revision 1.21  1999/09/15 03:17:31  lazarus
  Changes to Editor.pp
  If the text was actually displayed, then it would work better.  :-)

  Revision 1.20  1999/09/11 12:16:15  lazarus
  Fixed a bug in key press evaluation. Initial cut at Invalidate problem.

  Revision 1.19  1999/09/03 22:01:02  lazarus
  Added TTrackBar
    stoppok

  Revision 1.18  1999/08/26 23:36:02  peter
    + paintbox
    + generic keydefinitions and gtk conversion
    * gtk state -> shiftstate conversion

  Revision 1.17  1999/08/24 20:17:59  lazarus
  *** empty log message ***

  Revision 1.16  1999/08/20 15:44:38  lazarus
  TImageList changes added from Marc Weustink

  Revision 1.15  1999/08/16 18:45:40  lazarus
  Added a TFont Dialog plus minor additions.

  Shane Aug 16th 1999  14:07 CST

  Revision 1.14  1999/07/31 14:27:02  peter
    * mouse fixes
    * wheel support

  Revision 1.13  1999/07/31 06:39:27  lazarus

       Modified the IntSendMessage3 to include a data variable. It isn't used
       yet but will help in merging the Message2 and Message3 features.

       Adjusted TColor routines to match Delphi color format

       Added a TGdkColorToTColor routine in gtkproc.inc

       Finished the TColorDialog added to comDialog example.        MAH

 }
