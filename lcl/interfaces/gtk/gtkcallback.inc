{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

const
  DblClickTime = 250;//250 miliseconds or less between clicks is a double click

var
//testing
LMouseButtonDown,MMouseButtonDown,RMouseButtonDown : Boolean;  //used to track the mouse buttons
LLastClick, RLastClick, MLastClick : TDateTime;
LastFileSelectRow : gint;




// temp solution to fill msgqueue
function DeliverPostMessage(const Target: Pointer; var Message): GBoolean;
begin
  if TObject(Target) is TWinControl 
  then begin
    Result := PostMessage(TWinControl(Target).Handle, TLMessage(Message).Msg,
      TLMessage(Message).WParam, TLMessage(Message).LParam);
  end 
  else begin
    Result := DeliverMessage(Target, Message) = 0;
  end;
end;

procedure EventTrace(message : string; data : pointer);
begin
  if Data = nil 
  then Assert(False, Format('Trace:Event [%s] fired',[message]))
  else Assert(False, Format('Trace:Event [%s] fired for %s',[message, TObject(data).Classname]));
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

   Mess.Msg := LM_CHANGED;
   Result := DeliverMessage(Data, Mess) = 0;
end;

function gtkchanged_editbox( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
   Mess : TLMessage;
begin
   Result := True;
   EventTrace('changed', data);

   Mess.Msg := CM_TEXTCHANGED;
   Result := DeliverMessage(Data, Mess) = 0;
end;

function gtkdaychanged(Widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  MSG: TLMessage;
begin
  Result := True;
  EventTrace('day changed', data);
  MSG.Msg := LM_DAYCHANGED;
  Result := DeliverPostMessage(Data, MSG);
//   Result := DeliverMessage(Data, MSG) = 0;
end;

function gtkdraw(Widget: PGtkWidget; area: PGDKRectangle; data: gPointer) : GBoolean; cdecl;
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


function gtkfrmactivate( widget: PGtkWidget; Event : TgdkEventFocus;
  data: gPointer) : GBoolean; cdecl;
var
   Mess : TLMActivate;
begin
   EventTrace('activate', data);
   Mess.Msg := LM_ACTIVATE;
   Result := DeliverPostMessage(Data, Mess);
end;

function gtkfrmdeactivate( widget: PGtkWidget; Event : TgdkEventFocus;
  data: gPointer) : GBoolean; cdecl;
var
   Mess : TLMActivate;
begin
   EventTrace('deactivate', data);
   Mess.Msg := LM_DEACTIVATE;
   Result := DeliverPostMessage(Data, Mess);
end;

function GTKMap(Widget: PGTKWidget; Data: gPointer): GBoolean; cdecl;
begin
   Result := True;
   EventTrace('map', data);
end;

function GTKExposeEvent(Widget: PGtkWidget; Event : PGDKEventExpose;
  Data: gPointer): GBoolean; cdecl;
var
//  Mess : TLMessage;
//  fWindow : pgdkWindow;
//  widget2: pgtkWidget;
//  PixMap : pgdkPixMap;
  
  msg: TLMPaint;
begin
  Result := True;
  EventTrace('expose-event', data);
  if (Event^.Count > 0) then exit;
  
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

function GTKKeyUpDown(Widget: PGtkWidget; Event : pgdkeventkey;
  Data: gPointer) : GBoolean; cdecl;
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
  
  case Event^.theType of

    GDK_KEY_RELEASE:
      begin
//writeln('GTKKeyUpDown-GDK_KEY_RELEASE Code=',KeyCode,' Char=',Msg.CharCode,' Sys=',SysKey,' Ext=',Extended,' Toggle=',Toggle);
        EventTrace('key up', data);
        if SysKey
        then Msg.msg := LM_SYSKEYUP
        else Msg.msg := LM_KEYUP;

        Flags := Flags or KF_UP or KF_REPEAT;

        Msg.KeyData := Msg.KeyData or (Flags shl 16) or $0001 {allways};
        Result := DeliverPostMessage(data, msg);

      end;
    GDK_KEY_PRESS:
      begin
//writeln('GTKKeyUpDown-GDK_KEY_PRESS Code=',KeyCode,' Char=',Msg.CharCode,' Sys=',SysKey,' Ext=',Extended,' Toggle=',Toggle);
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


function GTKFocusCB( widget: PGtkWidget; event:PGdkEventFocus; data: gPointer) : GBoolean; cdecl;
var
   Mess : TLMessage;
begin
   EventTrace('focus', data);
//Writeln('Getting Focus... ',TObject(Data).ClassName,' widget=',HexStr(Cardinal(widget),8));
//writeln('  Focus=',HexStr(Cardinal(GetFocus),8));
   //TODO: fill in old focus
   Mess.msg := LM_SETFOCUS;
   Assert(False, Format('Trace:TODO: [gtkfocusCB] %s  finish', [TObject(Data).ClassName]));
   Result := DeliverMessage(Data,  Mess) = 0;
//Writeln('Getting Focus...  END ',TObject(Data).ClassName,' ',Result);
end;

function GTKKillFocusCB(widget: PGtkWidget; event:PGdkEventFocus; data: gPointer) : GBoolean; cdecl;
var
   Mess : TLMessage;
begin
   EventTrace('killfocus', data);
//Writeln('Killing Focus... ',TObject(Data).ClassName,' widget=',HexStr(Cardinal(widget),8));
//writeln('  Focus=',HexStr(Cardinal(GetFocus),8));
   Mess.msg := LM_KILLFOCUS;
   //TODO: fill in new focus
   Assert(False, Format('Trace:TODO: [gtkkillfocusCB] %s  finish', [TObject(Data).ClassName]));
   if GetFocus<>0 then
     Result := DeliverMessage(Data,  Mess) = 0;
//Writeln('Killing Focus... END ',TObject(Data).ClassName,' ',Result);
end;

function gtkdestroyCB(widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
   Mess: TLMessage;
   Info: PWinWidgetInfo;
begin
  Result := True;
  EventTrace('destroy', data);
  Mess.msg := LM_DESTROY;
  Result := DeliverMessage(Data, Mess) = 0;
  if longint(widget)=MCaptureHandle then MCaptureHandle:=0;
   
  // NOTE: if the destroy message is posted
  // we should post an info destroy message as well
  Info := GetWidgetInfo(widget, False);
  if Info <> nil then Dispose(Info);
end;

function gtkdeleteCB( widget : PGtkWidget; event : PGdkEvent; data : gPointer) : GBoolean; cdecl;
var Mess : TLMessage;
begin
  Mess.Msg:= LM_CLOSEQUERY;
  { Message results : True - do nothing, False - destroy or hide window }
  Result:= DeliverMessage(Data, Mess) = 0;
  if longint(widget)=MCaptureHandle then MCaptureHandle:=0;
  
end;

function gtkresizeCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
//var
//   Mess : TLMessage;
begin
   Result := True;
   EventTrace('resize', data);
//   Mess.msg := LM_RESIZE;
//   TControl(data).WindowProc(TLMessage(Mess));
 Assert(False, 'Trace:TODO: [gtkresizeCB] fix (or remove) to new LM_SIZE');
//TObject(data).Dispatch(Mess);
end;


function gtkmonthchanged(Widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  MSG: TLMessage;
begin
  Result := True;
  EventTrace('month changed', data);
  MSG.Msg := LM_MONTHCHANGED;
  Result := DeliverPostMessage(Data, MSG);
//   Result := DeliverMessage(Data, MSG) = 0;
end;

function GTKMotionNotify(widget:PGTKWidget; event: PGDKEventMotion; data: gPointer):GBoolean; cdecl;
var
  Msg: TLMMouseMove;
  ShiftState: TShiftState;
  parWindow : PgdkWindow;  //the Parent's GDKWindow
  ShowDebugging : Boolean;
  
begin
  ShowDebugging := False;
  if ShowDebugging then
  Begin
  writeln('_______________');
  Writeln('Motion Notify');
  Writeln('Control = ',TControl(data).Name);
  Writeln('Handle = ',Longint(TWinControl(data).Handle));
  Writeln('Widget = ',LongInt(widget));
  Writeln('Window = ',Longint(Event^.Window));
  Writeln('Coords = ',trunc(Event^.x),',',trunc(Event^.Y));
  Writeln('Send Event',Event^.send_Event);
  Writeln('Event Type',Event^.thetype);
  Writeln('Coords root = ',trunc(Event^.x_root),',',trunc(Event^.Y_root));
  Writeln('State = ',event^.state);
  Writeln('TGtkWidget^.Window is ',Longint(Widget^.Window));
  parWindow := gtk_widget_get_parent_window(widget);
  Writeln('Parwindow is ',LongInt(parwindow));
  Writeln('_______________');
  end;

  //work around
  //if the gdkwindow is the same as the parent's gdkwindow, then adjust the x,y relative to the cotnrol.
  parWindow := gtk_widget_get_parent_window(widget);
  if (ParWindow = Event^.Window) then
     Begin
         if ShowDebugging then
         Begin
           Writeln('***********************');
           Writeln('Calculating new X and Y');
           Writeln('TWincontrol(data).left and Top = ',TWinControl(data).Left,',',TWinControl(data).Top);
           Writeln('Event^.X and Y = ',Event^.X,',',Event^.Y);
         end;
         Event^.X := Event^.X - TWinControl(data).left;
         Event^.Y := Event^.Y - TWinControl(data).Top;
         if ShowDebugging then
         Begin
           Writeln('CAlculated...');
           Writeln('TWincontrol(data).left and Top = ',TWinControl(data).Left,',',TWinControl(data).Top);
           Writeln('Event^.X and Y = ',Event^.X,',',Event^.Y);
           Writeln('***********************');
         end;
     end;
       
  ShiftState := GTKEventState2ShiftState(Event^.State);
  with Msg do
  begin
    Msg := LM_MouseMove;
    if ShowDebugging then
    Begin
       Writeln('re-calcing XPos and YPos');
       Writeln('Event X and Y :',Event^.X,',',Event^.y);
    end;
    
    XPos := trunc(Event^.X);//Round(Event^.X);
    YPos := trunc(Event^.Y); //Round(Event^.Y);
    if ShowDebugging then
    Begin
      Writeln('Done...');
      Writeln('XPos,mYPos :',XPos,',',YPos);
    end;

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
  then  WriteLN(Format('[GTKMotionNotify] Capture differs -->  cap:0x%x gtk:0x%p', [MCaptureHandle, widget]));
end;

function gtkMouseBtnPress(widget: PGtkWidget; event : pgdkEventButton;
  data: gPointer) : GBoolean; cdecl;
const
  WHEEL_DELTA : array[Boolean] of Integer = (-1, 1);
var
  MessI : TLMMouse;
  MessE : TLMMouseEvent;
  ShiftState: TShiftState;
  ShowDebugging : Boolean;
  parWindow : PgdkWindow;  //the Parent's GDKWindow
begin
//writeln('[gtkMouseBtnPress] ',ToBject(Data).ClassName,' ',Trunc(Event^.X),',',Trunc(Event^.Y));
  EventTrace('Mouse button Press', data);
  Assert(False, Format('Trace:[gtkMouseBtnPress] ', []));
  ShiftState := GTKEventState2ShiftState(Event^.State);
  
  ShowDebugging := False;
  if ShowDebugging then
  Begin
  writeln('_______________');
  Writeln('Button Down');
  Writeln('Control = ',TControl(data).Name);
  Writeln('Handle = ',Longint(TWinControl(data).Handle));
  Writeln('Widget = ',LongInt(widget));
  Writeln('Window = ',Longint(Event^.Window));
  Writeln('Coords = ',trunc(Event^.x),',',trunc(Event^.Y));
  Writeln('Event Type',Event^.thetype);
  Writeln('Coords root = ',trunc(Event^.x_root),',',trunc(Event^.Y_root));
  Writeln('State = ',event^.state);
  Writeln('TGtkWidget^.Window is ',Longint(Widget^.Window));
  parWindow := gtk_widget_get_parent_window(widget);
  Writeln('Parwindow is ',LongInt(parwindow));
  Writeln('_______________');
  end;

  
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
  else begin
    MessI.Keys := 0;
    case event^.Button of
         1 : begin
               if (LMouseButtonDown) and (not (Event^.theType = gdk_2button_press)) then Exit;
               MessI.Keys := MessI.Keys or MK_LBUTTON;
               if (now - LLastClick) <= ((1/86400)*(DblClickTime/1000)) then
                  Event^.theType := gdk_2Button_press;
               LLastClick := Now;

               if event^.thetype = gdk_button_press then
                 MessI.Msg := LM_LBUTTONDOWN
                else
                   begin
                     MessI.Msg := LM_LBUTTONDBLCLK;
                     LLastClick := -1;
                   end;

             LMouseButtonDown := True;

             end;
         2 : begin
               if (MMouseButtonDown) and (not (Event^.theType = gdk_2button_press)) then Exit;

               MessI.Keys := MessI.Keys or MK_MBUTTON;
               
               if (now - MLastClick) <= ((1/86400)*(DblClickTime/1000)) then
                  Event^.theType := gdk_2Button_press;
               MLastClick := Now;

               if event^.thetype = gdk_button_press then
                 MessI.Msg := LM_MBUTTONDOWN
               else
                 Begin
                   MessI.Msg := LM_MBUTTONDBLCLK;
                   MLastClick := -1;
                 end;
               MMouseButtonDown := True;
             end;
         3 : begin
               if (RMouseButtonDown) and (not (Event^.theType = gdk_2button_press)) then Exit;
               MessI.Keys := MessI.Keys or MK_RBUTTON;

               if (now - RLastClick) <= ((1/86400)*(DblClickTime/1000)) then
                  Event^.theType := gdk_2Button_press;
               RLastClick := Now;

               if event^.thetype = gdk_button_press then
                 MessI.Msg := LM_RBUTTONDOWN
               else
                 Begin
                   MessI.Msg := LM_RBUTTONDBLCLK;
                   RLastClick := -1;
                 end;
               RMouseButtonDown := True;
             end;
         else MessI.Msg := LM_NULL;
    end; //case

    MessI.XPos := Trunc(Event^.X);
    MessI.YPos := Trunc(Event^.Y);
    if ssShift in ShiftState then MessI.Keys := MessI.Keys or MK_SHIFT;
    if ssCtrl in ShiftState then MessI.Keys := MessI.Keys or MK_CONTROL;
    if ssLeft in ShiftState then MessI.Keys := MessI.Keys or MK_LBUTTON;
    if ssRight in ShiftState then MessI.Keys := MessI.Keys or MK_RBUTTON;
    if ssMiddle in ShiftState then MessI.Keys := MessI.Keys or MK_MBUTTON;
    if MessI.Msg <> LM_NULL then Result := DeliverPostMessage(Data, MessI);
  end;
  Result := not(Result);
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
    1 : if not(LMouseButtonDown) then
           Exit
           else
             Begin
                MessI.Msg := LM_LBUTTONUP;
                LMouseButtonDown := False;
             end;
    2 : if not(MMouseButtonDown) then
           Exit
           else
             Begin
                MessI.Msg := LM_MBUTTONUP;
                MMouseButtonDown := False;
             end;
    3 : if not(RMouseButtonDown) then
           Exit
           else
             Begin
                MessI.Msg := LM_RBUTTONUP;
                RMouseButtonDown := False;
             end
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

  Result := not(Result);

end;

function gtkclickedCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
   Mess : TLMessage;
begin
//writeln('[gtkclickedCB] ',TObject(Data).ClassName);
  EventTrace('clicked', data);
  Assert(False, Format('Trace:OBSOLETE: [gtkclickedCB] ', []));
  Mess.Msg := LM_CLICKED;
  Result:= DeliverMessage(Data, Mess) = 0;

  Result := True;
end;

function gtkOpenDialogRowSelectCB( widget : PGtkWidget; row : gint; column : gint; event : pgdkEventButton; data : gPointer ) : GBoolean; cdecl;
var
   ShiftState: TShiftState;
   loop : gint;
   startRow : gint;
   endRow : gint;
begin
   // only process the callback if there is event data. If there isn't any
   // event data that means it was called due to a direct function call of the
   // widget and not an actual mouse click on the widget.
   if event <> nil then
   begin
      ShiftState := GTKEventState2ShiftState(Event^.State);
      if ssShift in ShiftState then
      begin
         if LastFileSelectRow <> -1 then
         begin
            startRow := LastFileSelectRow;
	    endRow := row;
            if LastFileSelectRow > row then
	    begin
	       startRow := row;
	       endRow := LastFileSelectRow;
	    end;
            for loop := startRow to endRow do
	    begin
               gtk_clist_select_row(PGtkCList(widget), loop, column);
	    end;
         end;
      end
      else if not (ssCtrl in ShiftState) then
      begin
         gtk_clist_unselect_all(PGtkCList(widget));
         gtk_clist_select_row(PGtkCList(widget), row, column);
      end;
      LastFileSelectRow := row;
   end;
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
   cListRow : PGList;
   rowNum : gint;
   fileInfo : PGChar;
   fileList : PGTKCList;
   DirName  : string;
   FileName : string;
   Files: TStringList;
begin
   Result := True;
   theDialog := TCommonDialog(data);
   FPointer := Pointer(theDialog.Handle);
//   gtk_grab_remove(PgtkWidget(TCommonDialog(data).Handle));
   if theDialog is TFileDialog then
   begin
      if theDialog is TOpenDialog then
      begin
         // check extra options
         if ofAllowMultiSelect in TOpenDialog(theDialog).Options then
	 begin
           FileName:=gtk_file_selection_get_filename(
                                    PGtkFileSelection(FPointer));
           DirName:=ExtractFilePath(FileName);
           TFileDialog(data).FileName := '';
           Files:=TStringList(TFileDialog(theDialog).Files);
           Files.Clear;
           if not FileExists(FileName) then
             Files.Add(FileName);
           fileList := PGtkCList(PGtkFileSelection(FPointer)^.file_list);
           rowNum := 0;
	   cListRow := fileList^.row_list;
	   while cListRow <> nil do
	   begin
	     if PGtkCListRow(cListRow^.data)^.state = GTK_STATE_SELECTED then
	     begin
               if gtk_clist_get_cell_type(fileList, rowNum, 0) = GTK_CELL_TEXT
               then begin
	         gtk_clist_get_text(fileList, rowNum, 0, @fileInfo);
                 Files.Add(DirName+fileInfo);
               end;
	     end;
	     // get next row from list
	     rowNum := rowNum + 1;
	     cListRow := g_list_next(cListRow);
	   end;
           if (Filename<>'') and (Files.IndexOf(Filename)<0) then
             Files.Add(Filename);
         end
	 else
	 begin
            TFileDialog(data).FileName := 
              gtk_file_selection_get_filename(PGtkFileSelection(FPointer));
         end;
      end
      else
      begin
         TFileDialog(data).FileName := gtk_file_selection_get_filename(PGtkFileSelection(FPointer));
      end;
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
   if theDialog is TFileDialog then
   begin
     // TFileDialog(data).FileName := '';
   end;
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

function gtksize_allocateCB( widget: PGtkWidget; size :pGtkAllocation;
  data: gPointer) : GBoolean; cdecl;
var
  PosMsg : TLMWindowPosChanged;
  SizeMsg: TLMSize;
  MoveMsg: TLMMove;
  Dummy: TPoint;
  OldLeft, OldTop, OldWidth, OldHeight: integer;
  TopLeftChanged, WidthHeightChanged: boolean;
begin
  EventTrace('size-allocate', data);


  PosMsg.msg := LM_WINDOWPOSCHANGED; //LM_SIZEALLOCATE;
  PosMsg.Result := -1;
  SizeMsg.Result := -1;
  MoveMsg.Result := -1;
  
  with Size^ do Assert(False, Format('Trace:[gtksize_allocateCB] %s --> X: %d, Y: %d, Width: %d, Height: %d', [TObject(data).ClassName, X, Y, Width, Height]));
  
  if (TObject(Data) is TControl) then begin
    OldLeft:=TControl(Data).Left;
    OldTop:=TControl(Data).Top;
    OldWidth:=TControl(Data).Width;
    OldHeight:=TControl(Data).Height;
  end else begin
    OldLeft:=0;
    OldTop:=0;
    OldWidth:=0;
    OldHeight:=0;
  end;
  
  New(PosMsg.WindowPos);
  try
    with PosMsg.WindowPos^ do
    begin
      if TObject(data) is TWinControl then begin
        hWnd := TWinControl(data).Handle;
        if TObject(data) is TCustomForm then begin
          //writeln('[gtksize_allocateCB]  CUSTOMFORM  ************'
          //       ,TControl(Data).Left,',',TControl(Data).Top,'  ',Size^.X,',',Size^.Y);
          Dummy.X:=TControl(Data).Left;
          Dummy.Y:=TControl(Data).Top;
          if widget^.window<>nil then
            gdk_window_get_root_origin(widget^.window, @Dummy.X, @Dummy.Y);
          Size^.X:=Dummy.X;
          Size^.Y:=Dummy.Y;
        end;
      end else
        hWnd := 0;
      hWndInsertAfter := 0;
      x := Size^.X;
      y := Size^.Y;
      cx := Size^.Width;
      cy := Size^.Height;
      flags := 0;
    end;
    TopLeftChanged:=(OldLeft<>Size^.X) or (OldTop<>Size^.Y);
    WidthHeightChanged:=(OldWidth<>Size^.Width) or (OldHeight<>Size^.Height);
    if TopLeftChanged or WidthHeightChanged then begin
      Assert(False, 'Trace:[gtksize_allocateCB] DeliverMessage LM_WINDOWPOSCHANGED');
//writeln('[gtksize_allocateCB] Sending LM_WINDOWPOSCHANGED message: Target=',TObject(Data).ClassName,' Left=',Size^.X,' Top=',Size^.Y,' Width=',Size^.Width,' Height=',Size^.Height);
      Result := DeliverMessage(Data, PosMsg) = 0;
    end;
  finally
    Dispose(PosMsg.WindowPos);
  end;

  { If the message is not handled. A LM_SIZE and LM_MOVE should be sent
 }
  

  if (PosMsg.Result = 0) then 
  begin
    if WidthHeightChanged then begin
      with SizeMsg do
      begin
        Msg := LM_SIZE;
        SizeType := 6; // force ReAlign
        Width := Size^.Width;
        Height := Size^.Height;
      end;
      Assert(False, 'Trace:[gtksize_allocateCB] DeliverMessage LM_SIZE');
      Result := (DeliverMessage(Data, SizeMsg) = 0) or Result;
    end;
    
    if TopLeftChanged then begin
      with MoveMsg do
      begin
        Msg := LM_MOVE;
        MoveType := 1; // force RequestAlign
        XPos := Size^.X;
        YPos := Size^.Y;
      end;
      Assert(False, 'Trace:[gtksize_allocateCB] DeliverMessage LM_MOVE');
      Result := (DeliverMessage(Data,  MoveMsg) = 0) or Result;
    end;
  end;
  
end;

function gtkswitchpage(widget: PGtkWidget; page: Pgtkwidget; pagenum : integer;
  data: gPointer) : GBoolean; cdecl;
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


function gtkconfigureevent( widget: PGtkWidget; event : PgdkEventConfigure;
  data: gPointer) : GBoolean; cdecl;
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

{------------------------------------------------------------------------------
  Method: gtkTimerCB
  Params:  Data - pointer TGtkITimerInfo structure
  Returns: 1    - 1 tells gtk to restart the timer
           0    - 0 will stop the gtk timer

  Callback for gtk timer. Depending on "data" either user-callback function
  will be called or a timer message will be delivered.

  WARNING: There seems to be a bug in gtk-1.2.x which breaks 
           gtk_timeout_remove so we have to dispose data here & return 0
           (s.a. KillTimer).
 ------------------------------------------------------------------------------}
function gtkTimerCB(Data: gPointer): gint; cdecl;
var
  Mess : TLMessage;
begin
  EventTrace ('TimerCB', data);
  Result := 1;  // assume: timer will continue
  if PGtkITimerinfo(Data)^.TimerFunc <> nil
  then begin    // Call users timer function
    PGtkITimerinfo(Data)^.TimerFunc(PGtkITimerinfo(Data)^.Handle, 
                        LM_TIMER,
                        PGtkITimerinfo(Data)^.IDEvent, 
                        0{WARN: should be: GetTickCount});
  end
  else if (pointer (PGtkITimerinfo(Data)^.Handle) <> nil)
  then begin    // Handle through default message handler
    Mess.msg    := LM_TIMER;
    Mess.WParam := PGtkITimerinfo(Data)^.IDEvent;
    Mess.LParam := LongInt (PGtkITimerinfo(Data)^.TimerFunc);
    DeliverMessage (Pointer (PGtkITimerinfo(Data)^.Handle), Mess);
  end
  else begin
     result := 0;       // stop timer
     FOldTimerData.Remove(Data);
     dispose (PGtkITimerinfo(Data));     // free memory with timer data
  end;
end;

function gtkFocusInNotifyCB (widget : PGtkWidget; event : PGdkEvent; data : gpointer) : GBoolean; cdecl;
var
  MessI : TLMEnter;
begin
//writeln('[gtkFocusInNotifyCB] ',TObject(data).ClassName);
  EventTrace ('FocusInNotify (alias Enter)', data);
  MessI.msg := LM_Enter;
  Result:= DeliverMessage(Data, MessI) = 0;
end;

function gtkFocusOutNotifyCB (widget : PGtkWidget; event : PGdkEvent; data : gpointer) : GBoolean; cdecl;
var
  MessI : TLMExit;
begin
//writeln('[gtkFocusOutNotifyCB] ',TObject(data).ClassName);
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
  V, U, {L,}
  StepI, PageI{, Page}: Integer;
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
    //L := Round(Lower);
    StepI := Round(Step_Increment);
    PageI := Round(Page_Increment);
    //Page := Round(Page_Size);
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
  V, U, {L,}
  StepI, PageI{, Page}: Integer;
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
    //L := Round(Lower);
    StepI := Round(Step_Increment);
    PageI := Round(Page_Increment);
    //Page := Round(Page_Size);
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
  //Msg: TLMKey;
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

function gtkyearchanged(Widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  MSG: TLMessage;
begin
  Result := True;
  EventTrace('year changed', data);
  MSG.Msg := LM_YEARCHANGED;
  Result := DeliverPostMessage(Data, MSG);
end;


{------------------------------------------------------------------------------
  ClipboardSelectionReceivedHandler

  copy the received selection data record and buffer to
  internal record and buffer (ClipboardSelectionData)
 ------------------------------------------------------------------------------}
procedure ClipboardSelectionReceivedHandler(TargetWidget: PGtkWidget;
  SelectionData: PGtkSelectionData; TimeID: cardinal; Data: Pointer); cdecl;
var TempBuf: Pointer;
  c: PClipboardEventData;
  i: integer;
begin
  // at any time there can be several requests
  // find the request with the correct TimID
  i:=ClipboardSelectionData.Count-1;
  while (i>=0) do begin
    c:=PClipboardEventData(ClipboardSelectionData[i]);
    if c^.TimeID=TimeID then break;
    dec(i);
  end;
{$IFDEF DEBUG_CLIPBOARD}
writeln('[ClipboardSelectionReceivedHandler] TimeID=',TimeID,' ',i>=0);
{$ENDIF}
  if i<0 then exit;
  // free old data
  if (c^.Data.Data<>nil) then FreeMem(c^.Data.Data);
  // copy the information
  c^.Data:=SelectionData^;
  // copy the raw data to an internal buffer (the gtk buffer will be destroyed
  // right after this event)
  if (c^.Data.Data<>nil) 
  and (c^.Data.Length>0) then begin
    GetMem(TempBuf,c^.Data.Length);
    Move(c^.Data.Data^,TempBuf^,c^.Data.Length);
    c^.Data.Data:=TempBuf;
{$IFDEF DEBUG_CLIPBOARD}
writeln('[ClipboardSelectionReceivedHandler] ',ord(PChar(c^.Data.Data)[0]));
{$ENDIF}
  end else
    c^.Data.Data:=nil;
end;

{------------------------------------------------------------------------------
  ClipboardSelectionRequestHandler

  This signal is emitted if someone requests the clipboard data
  Since the lcl clipboard caches all requests this will typically be another
  application
 ------------------------------------------------------------------------------}
procedure ClipboardSelectionRequestHandler(TargetWidget: PGtkWidget;
  SelectionData: PGtkSelectionData; Info: cardinal; TimeID: cardinal;
  Data: Pointer); cdecl;
type
  PGdkAtom = ^TGdkAtom;
var ClipboardType: TClipboardType;
  MemStream: TMemoryStream;
  FormatID: cardinal;
  Buffer: Pointer;
  BufLength: integer;
  P: PChar;
  BitCount: integer;
  //s: string;
begin
{$IFDEF DEBUG_CLIPBOARD}
writeln('*** [ClipboardSelectionRequestHandler] START');
{$ENDIF}
  if SelectionData^.Target=0 then exit;
  for ClipboardType:=Low(TClipboardType) to High(TClipboardType) do
    if SelectionData^.Selection=ClipboardTypeAtoms[ClipboardType] then begin
      if Assigned(ClipboardHandler[ClipboardType]) then begin
        // handler found for this of clipboard
        // now create a stream and find a supported format
{$IFDEF DEBUG_CLIPBOARD}
p:=gdk_atom_name(SelectionData^.Target);
writeln('[ClipboardSelectionRequestHandler] ',ClipboardTypeName[ClipboardType],' Format=',p,' ID=',SelectionData^.Target);
g_free(p);
{$ENDIF}
        MemStream:=TMemoryStream.Create;
        try
          // the gtk-interface provides autiomatically some formats, that the lcl
          // does not know. Wrapping them to lcl formats ...
          FormatID:=SelectionData^.Target;
          if ((FormatID=gdk_atom_intern('COMPOUND_TEXT',1))
            and (ClipboardExtraGtkFormats[ClipboardType][gfCOMPOUND_TEXT]))
          or ((FormatID=gdk_atom_intern('STRING',1))
            and (ClipboardExtraGtkFormats[ClipboardType][gfSTRING]))
          or ((FormatID=gdk_atom_intern('TEXT',1))
            and (ClipboardExtraGtkFormats[ClipboardType][gfTEXT]))
          then
            FormatID:=gdk_atom_intern('text/plain',0);
            
{$IFDEF DEBUG_CLIPBOARD}
writeln('[ClipboardSelectionRequestHandler] FormatID=',FormatID,' CompoundText=',gdk_atom_intern('COMPOUND_TEXT',1),' ',ClipboardExtraGtkFormats[ClipboardType][gfCOMPOUND_TEXT]);
{$ENDIF}
          // get the requested data by calling the handler for this clipboard type
          ClipboardHandler[ClipboardType](FormatID,MemStream);
          MemStream.Position:=0;

          // build clipboard data for gtk
          Buffer:=nil;
          BufLength:=0;
          BitCount:=8;
          
          // if the format was wrapped, transform it back
          if (FormatID=gdk_atom_intern('text/plain',1)) then begin
            if (SelectionData^.Target=gdk_atom_intern('COMPOUND_TEXT',1)) then
            begin
              // transform text/plain to COMPOUND_TEXT
              P:=StrAlloc(MemStream.Size+1);
              MemStream.Read(P^,MemStream.Size);
              P[MemStream.Size]:=#0;
              gdk_string_to_compound_text(P,@SelectionData^.theType,
                @SelectionData^.Format,@Buffer,@BufLength);
              StrDispose(P);
              gtk_selection_data_set(SelectionData,SelectionData^.Target,
                SelectionData^.Format,Buffer,BufLength);
              gdk_free_compound_text(Buffer);
              exit;
            end;
          end;
          if Buffer=nil then begin
{$IFDEF DEBUG_CLIPBOARD}
writeln('[ClipboardSelectionRequestHandler] Default MemStream.Size=',MemStream.Size);
{$ENDIF}
            BufLength:=MemStream.Size;
            if MemStream.Size>0 then begin
              GetMem(Buffer,MemStream.Size);
              MemStream.Read(Buffer^,MemStream.Size);
              {SetLength(s,MemStream.Size);
              MemStream.Position:=0;
              MemStream.Read(s[1],MemStream.Size);
              writeln(' >>> "',s,'"');}
            end;
          end;
{$IFDEF DEBUG_CLIPBOARD}
writeln('[ClipboardSelectionRequestHandler] Len=',BufLength);
{$ENDIF}
          gtk_selection_data_set(SelectionData,SelectionData^.Target,BitCount,
            Buffer,BufLength);
          if Buffer<>nil then
            FreeMem(Buffer);
        finally
          MemStream.Free;
        end;
      end;
      break;
    end;
end;

{------------------------------------------------------------------------------
  ClipboardSelectionLostOwnershipHandler

  This signal is emitted if another application gets the clipboard ownership.
 ------------------------------------------------------------------------------}
function ClipboardSelectionLostOwnershipHandler(TargetWidget: PGtkWidget;
  EventSelection: PGdkEventSelection;  Data: Pointer): cardinal; cdecl;
var ClipboardType: TClipboardType;
begin
//writeln('*** [ClipboardSelectionLostOwnershipHandler] ',hexstr(cardinal(targetwidget),8));
  for ClipboardType:=Low(TClipboardType) to High(TClipboardType) do
    if EventSelection^.Selection=ClipboardTypeAtoms[ClipboardType] then begin
{$IFDEF DEBUG_CLIPBOARD}
writeln('*** [ClipboardSelectionLostOwnershipHandler] ',ClipboardTypeName[ClipboardType]);
{$ENDIF}
      if (ClipboardWidget<>nil)
      and (gdk_selection_owner_get(ClipboardTypeAtoms[ClipboardType])
        <> ClipboardWidget^.window)
      and Assigned(ClipboardHandler[ClipboardType]) then begin
        // handler found for this type of clipboard
{$IFDEF DEBUG_CLIPBOARD}
writeln('[ClipboardSelectionLostOwnershipHandler] ',ClipboardTypeName[ClipboardType]);
{$ENDIF}
        ClipboardHandler[ClipboardType](0,nil);
        ClipboardHandler[ClipboardType]:=nil;
      end;
      break;
    end;
  Result:=1;
end;




{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

{$I gtkDragCallback.inc}



{ =============================================================================

  $Log$
  Revision 1.60  2002/03/13 22:48:16  lazarus
  Constraints implementation (first cut) and sizig - moving system rework to
  better match Delphi/Kylix way of doing things (the existing implementation
  worked by acident IMHO :-)

  Revision 1.59  2002/01/01 18:38:36  lazarus
  MG: more wmsize messages :(

  Revision 1.58  2002/01/01 15:50:16  lazarus
  MG: fixed initial component aligning

  Revision 1.57  2001/12/28 15:12:02  lazarus
  MG: LM_SIZE and LM_MOVE messages are now send directly, not queued

  Revision 1.56  2001/12/17 11:09:48  lazarus
  MG: fixed typed but not selected filename in TOpenDialog

  Revision 1.55  2001/12/12 20:45:30  lazarus
  MG: added non existing filename to multiselection in TOpenDialog

  Revision 1.54  2001/12/12 20:19:19  lazarus

    Modified the the GTKFileSelection so that it will handle and use
    CTRL and SHIFT keys in a fashion similar to Windows.

  Revision 1.53  2001/12/12 15:12:31  lazarus
  MG: added file path to files in TOpenDialog

  Revision 1.52  2001/12/12 08:29:21  lazarus

        Add code to allow TOpenDialog to do multiple line selects.   MAH

  Revision 1.51  2001/12/07 20:12:15  lazarus
  Added a watch dialog.
  Shane

  Revision 1.50  2001/12/05 18:23:47  lazarus
  Added events to Calendar
  Shane

  Revision 1.49  2001/11/30 16:41:59  lazarus
  Improved hints with overlapping windows.
  Shane

  Revision 1.48  2001/11/29 18:41:27  lazarus
  Improved the double click.
  Shane

  Revision 1.47  2001/11/21 19:32:32  lazarus
  TComboBox can now be moved in FormEditor
  Shane

  Revision 1.46  2001/11/21 14:55:31  lazarus
  Changes for combobox to receive butondown and up events
  DblClick events now working.
  Shane

  Revision 1.45  2001/11/20 17:20:45  lazarus
  Fixed designer problem with moving controls.
  Can no longer drag controls off the form.
  Shane

  Revision 1.44  2001/11/14 17:46:58  lazarus
  Changes to make toggling between form and unit work.
  Added BringWindowToTop
  Shane

  Revision 1.43  2001/11/12 19:32:23  lazarus
  MG: fixed empty clipboard stream crashing bug

  Revision 1.42  2001/11/12 16:56:08  lazarus
  MG: CLIPBOARD

  Revision 1.41  2001/11/09 19:14:24  lazarus
  HintWindow changes
  Shane

  Revision 1.40  2001/11/01 21:30:35  lazarus
  Changes to Messagebox.
  Added line to CodeTools to prevent duplicate USES entries.

  Revision 1.39  2001/10/31 16:29:22  lazarus
  Fixed the gtk mousemove bug where the control gets the coord's based on it's parent instead of itself.
  Shane

  Revision 1.38  2001/10/16 14:19:13  lazarus
  MG: added nvidia opengl support and a new opengl example from satan

  Revision 1.35  2001/10/09 09:46:58  lazarus
  MG: added codetools, fixed synedit unindent, fixed MCatureHandle

  Revision 1.34  2001/10/03 21:03:02  lazarus
  MG: reduced repaints

  Revision 1.32  2001/06/16 09:14:38  lazarus
  MG: added lazqueue and used it for the messagequeue

  Revision 1.31  2001/06/14 14:57:59  lazarus
  MG: small bugfixes and less notes

  Revision 1.30  2001/04/06 22:25:14  lazarus
  * TTimer uses winapi-interface now instead of sendmessage-interface, stoppok

  Revision 1.29  2001/03/27 11:11:13  lazarus
  MG: fixed mouse msg, added filedialog initialdir

  Revision 1.28  2001/03/26 14:58:31  lazarus
  MG: setwindowpos + bugfixes

  Revision 1.26  2001/03/19 14:44:22  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.23  2001/02/28 13:17:34  lazarus
  Added some debug code for the top,left reporting problem.
  Shane

  Revision 1.22  2001/02/20 16:53:27  lazarus
  Changes for wordcompletion and many other things from Mattias.
  Shane

  Revision 1.21  2001/01/31 21:16:45  lazarus
  Changed to TCOmboBox focusing.
  Shane

  Revision 1.20  2001/01/30 18:15:02  lazarus
  Added code for TStatusBar
  I'm now capturing WMPainT and doing the drawing myself.
  Shane

  Revision 1.19  2001/01/28 03:51:42  lazarus
  Fixed the problem with Changed for ComboBoxs
  Shane

  Revision 1.18  2001/01/24 23:26:40  lazarus
  MWE:
    = moved some types to gtkdef
    + added WinWidgetInfo
    + added some initialization to Application.Create

  Revision 1.17  2001/01/24 03:21:03  lazarus
  Removed gtkDrawDefualt signal function from gtkcallback.inc
  It was no longer used.
  Shane

  Revision 1.16  2001/01/23 23:33:55  lazarus
  MWE:
    - Removed old LM_InvalidateRect
    - did some cleanup in old  code
    + added some comments  on gtkobject data (gtkproc)

  Revision 1.15  2001/01/12 18:10:54  lazarus
  Changes for keyevents in the editor.
  Shane

  Revision 1.14  2001/01/11 20:16:47  lazarus
  Added some TImageList code.
  Added a bookmark resource with 10 resource images.
  Removed some of the IFDEF's in mwCustomEdit around the inherited code.
  Shane

  Revision 1.13  2001/01/10 23:53:30  lazarus
  MWE:
    ~ minor change

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

}
