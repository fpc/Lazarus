(******************************************************************************
                        Misc Support Functs  
 ******************************************************************************
   used by:
     GTKObject
     GTKWinAPI
     GTKCallback
 ******************************************************************************)

{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

{------------------------------------------------------------------------------
  Function: NewGDIRawImage
  Params:  Width, Height: Size of the image
           Depth:         Depth of the image
  Returns: a GDIRawImage

  Creates a RawImage
 ------------------------------------------------------------------------------}
function NewGDIRawImage(const AWidth, AHeight: Integer; const ADepth: Byte): PGDIRawImage;
begin
  Result := AllocMem(SizeOf(TGDIRawImage) + ((AWidth * AHeight) - 1) * SizeOf(TGDIRGB));
//  FillChar(Result^, SizeOf(TGDIRawImage), 0);
  with Result^ do
  begin
    Height := AHeight;
    Width := AWidth;
    Depth := ADepth;
  end;
end;

{------------------------------------------------------------------------------
  Function: CreateGDKColor
  Params:  AColor: A RGB color
  Returns: an Allocated GDKColor

  Allocated a GDKColor from a winapi color
 ------------------------------------------------------------------------------}
function AllocGDKColor(const AColor: LongInt): TGDKColor;
begin
  with Result do
  begin
    Red :=   ((AColor shl 8) and $00FF00) or ((AColor       ) and $0000FF);
    Green := ((AColor      ) and $00FF00) or ((AColor shr 8 ) and $0000FF);
    Blue :=  ((AColor shr 8) and $00FF00) or ((AColor shr 16) and $0000FF);
  end;
  gdk_colormap_alloc_color(gdk_colormap_get_system, @Result, False, True);
end;

{------------------------------------------------------------------------------
  Function: CopyDCData
  Params:  DestinationDC:  a dc to copy data to
           SourceDC: a dc to copy data from
  Returns: True if succesfu

  Creates a copy DC from the given DC
 ------------------------------------------------------------------------------}
function CopyDCData(const DestinationDC, SourceDC: PDeviceContext): Boolean;
var
  GCValues: TGDKGCValues;
begin
  Assert(False, Format('Trace:> [CopyDCData] DestDC:0x%x, SourceDC:0x%x', [Integer(DestinationDC), Integer(SourceDC)]));
  Result := (DestinationDC <> nil) and (SourceDC <> nil);
  if Result
  then begin
    with DestinationDC^ do
    begin
      hWnd := SourceDC^.hWnd;
      Drawable := SourceDC^.Drawable;
      if (SourceDC^.GC = nil) or (Drawable = nil)
      then GC := nil
      else begin
        gdk_gc_get_values(SourceDC^.GC, @GCValues);
        //GC := gdk_gc_new(Drawable);
        GC := gdk_gc_new_with_values(Drawable, @GCValues, 3 { $3FF});
      end;
      PenPos := SourceDC^.PenPos;
      CurrentBitmap := SourceDC^.CurrentBitmap;
      CurrentFont := SourceDC^.CurrentFont;
      CurrentPen := SourceDC^.CurrentPen;
      CurrentBrush := SourceDC^.CurrentBrush;
      CurrentTextColor := SourceDC^.CurrentTextColor;
      CurrentBackColor := SourceDC^.CurrentBackColor;
      SavedContext := nil;
    end;
  end;
  Assert(False, Format('Trace:< [CopyDCData] DestDC:0x%x, SourceDC:0x%x --> %d', [Integer(DestinationDC), Integer(SourceDC), Integer(Result)]));
end;

{------------------------------------------------------------------------------
  Procedure: SelectGDKBrushProps
  Params:  DC: a (LCL)devicecontext
  Returns: Nothing

  Sets the forecolor and fill according to the brush
 ------------------------------------------------------------------------------}
procedure SelectGDKBrushProps(const DC: HDC);
begin
  with PDeviceContext(DC)^, CurrentBrush^ do
  begin
    Assert(False, Format('Trace:  [SelectGDKBrushProps] Fill: %d | Color --> pixel: %d, red: 0x%x, green: 0x%x, blue: 0x%x', [Integer(GDIBrushFill), GDIBrushColor.Pixel, GDIBrushColor.Red, GDIBrushColor.Green, GDIBrushColor.Blue]));
    gdk_gc_set_fill(GC, GDIBrushFill);
    gdk_gc_set_foreground(GC, @GDIBrushColor);
    gdk_gc_set_background(GC, @CurrentBackColor);
    //TODO: Brush pixmap
  end;
end;

{------------------------------------------------------------------------------
  Procedure: SelectGDKPenProps
  Params:  DC: a (LCL)devicecontext
  Returns: Nothing

  Sets the forecolor and fill according to the pen
 ------------------------------------------------------------------------------}
procedure SelectGDKPenProps(const DC: HDC);
begin
  with PDeviceContext(DC)^, CurrentPen^ do
  begin
    gdk_gc_set_foreground(GC, @GDIPenColor);
    gdk_gc_set_background(GC, @CurrentBackColor);
    gdk_gc_set_fill(GC, GDK_SOLID);
    if (GDIPenStyle = PS_SOLID) or (GDIPenStyle = PS_INSIDEFRAME) 
    then begin
      gdk_gc_set_line_attributes(GC, GDIPenWidth, GDK_LINE_SOLID, 0, 0)
    end 
    else begin
      gdk_gc_set_line_attributes(GC, GDIPenWidth, GDK_LINE_ON_OFF_DASH, 0, 0);
      case GDIPenStyle of
        PS_DASH:       gdk_gc_set_dashes(GC, 0, [4,4], 2);
        PS_DOT:        gdk_gc_set_dashes(GC, 0, [2,2], 2);
        PS_DASHDOT:    gdk_gc_set_dashes(GC, 0, [4,2,2,2,0], 4);
        PS_DASHDOTDOT: gdk_gc_set_dashes(GC, 0, [4,2,2,2,2,2], 6);
        PS_NULL:       gdk_gc_set_dashes(GC, 0, [0,4], 2);
      end;  
    end;
  end;
end;

{------------------------------------------------------------------------------
  Procedure: SelectGDKTextProps
  Params:  DC: a (LCL)devicecontext
  Returns: Nothing

  Sets the forecolor and fill according to the Textcolor
 ------------------------------------------------------------------------------}
procedure SelectGDKTextProps(const DC: HDC);
begin
  with PDeviceContext(DC)^ do
  begin
    gdk_gc_set_foreground(GC, @CurrentTextColor);
    gdk_gc_set_background(GC, @CurrentBackColor);
    Assert(False, Format('trace:  [SelectGDKTextProps] Color --> pixel: %d, red: 0x%x, green: 0x%x, blue: 0x%x', [CurrentTextColor.Pixel, CurrentTextColor.Red, CurrentTextColor.Green, CurrentTextColor.Blue]));
  end;
end;

{------------------------------------------------------------------------------
  Procedure: GTKEventState2ShiftState
  Params:    KeyState: The gtk keystate
  Returns:   the TShiftState for the given KeyState

  GTKEventState2ShiftState converts a GTK event state to a LCL/Delphi TShiftState
 ------------------------------------------------------------------------------}
function GTKEventState2ShiftState(KeyState: Word): TShiftState;
begin
  result:=[];
  if (KeyState and GDK_SHIFT_MASK)   <> 0 then Result := Result + [ssShift];
  if (KeyState and GDK_LOCK_MASK)    <> 0 then Result := Result + [ssCaps];
  if (KeyState and GDK_CONTROL_MASK) <> 0 then Result := Result + [ssCtrl];
  if (KeyState and GDK_MOD1_MASK)    <> 0 then Result := Result + [ssAlt];
//if (KeyState and GDK_MOD2_MASK)    <> 0 then Result := Result + [??ssWindows??];
  if (KeyState and GDK_MOD3_MASK)    <> 0 then Result := Result + [ssNum];
  if (KeyState and GDK_MOD4_MASK)    <> 0 then Result := Result + [ssSuper];
  if (KeyState and GDK_MOD5_MASK)    <> 0 then Result := Result + [ssScroll];
  if (KeyState and GDK_BUTTON1_MASK) <> 0 then Result := Result + [ssLeft];
  if (KeyState and GDK_BUTTON2_MASK) <> 0 then Result := Result + [ssMiddle];
  if (KeyState and GDK_BUTTON3_MASK) <> 0 then Result := Result + [ssRight];
//if (KeyState and GDK_BUTTON4_MASK) <> 0 then Result := Result + [??WheelMouse??];
//if (KeyState and GDK_BUTTON5_MASK) <> 0 then Result := Result + [??WheelMouse??];
  if (KeyState and GDK_RELEASE_MASK) <> 0 then Result := Result + [ssAltGr];
end;


{------------------------------------------------------------------------------
  Procedure: GetGTKKeyInfo
  Params:  Event:      Requested info  
           KeyCode:    the ASCII key code of the eventkey
           VirtualKey: the virtual key code of the eventkey
           SysKey:     True if the key is a syskey
           Extended:   True if the key is an extended key
           Toggle:     True if the key is a toggle key and its value is on
  Returns: Nothing
  
  GetGTKKeyInfo returns information about the given key event
 ------------------------------------------------------------------------------}
procedure GetGTKKeyInfo(const Event: PGDKEventKey; var KeyCode,VirtualKey: Word;
  var SysKey, Extended, Toggle: Boolean);
var
  CtrlDown: Boolean;
begin
  VirtualKey := VK_UNKNOWN;
  KeyCode := $FFFF;
  SysKey := (Event^.State and GDK_MOD1_MASK) <> 0;
  CtrlDown := (Event^.State and GDK_CONTROL_MASK) <> 0;
  Extended := False;
  Toggle := False;
  
  case Event^.KeyVal of
    // Normal ASCII chars
    32..255 : 
      begin
        { Assign key code}
        KeyCode := Event^.KeyVal;
        
        //TODO: create VK_ code --> [*] and [8] have a different KeyCode but same VK_ code
        case Chr(KeyCode) of
          'A'..'Z', 
          '0'..'9', 
          ' ':      VirtualKey := KeyCode;
          'a'..'z': VirtualKey := KeyCode - Ord('a') + Ord('A');
          '/': VirtualKey := VK_SLASH;
          ',': VirtualKey := VK_COMMA;
          '.': VirtualKey := VK_POINT;
        end;
        
        { look for control code }
        if CtrlDown and (Chr(KeyCode) in ['@'..'Z'])
        then Dec(KeyCode, Ord('@'));
      end;
      
    GDK_KP_Space:
      begin
        KeyCode := VK_SPACE;
        VirtualKey := VK_SPACE;
      end;
    GDK_KP_Tab:
      begin
        KeyCode := VK_TAB;
        VirtualKey := VK_TAB;
      end;
    GDK_KP_Enter:
      begin
        KeyCode := VK_Return;
        VirtualKey := VK_Return;
      end;
    GDK_Tab:
      begin
        KeyCode := VK_TAB;
        VirtualKey := VK_TAB;
      end;
    GDK_Return:
      begin
        KeyCode := VK_RETURN;
        VirtualKey := VK_RETURN;
      end;
    GDK_Linefeed: 
      begin
        KeyCode := $0A;
        VirtualKey := $0A;
      end;
 
    // Cursor block / keypad
    GDK_Insert:         
      begin
        VirtualKey := VK_INSERT;
        Extended := True;
      end;
    GDK_Home:           
      begin
        VirtualKey := VK_HOME;
        Extended := True;
      end;
    GDK_Left:           
      begin
        VirtualKey := VK_LEFT;
        Extended := True;
      end;
    GDK_Up:             
      begin
        VirtualKey := VK_UP;
        Extended := True;
      end;
    GDK_Right:          
      begin
        VirtualKey := VK_RIGHT;
        Extended := True;
      end;
    GDK_Down:           
      begin
        VirtualKey := VK_DOWN;
        Extended := True;
      end;
    GDK_Page_Up:        
      begin
        VirtualKey := VK_PRIOR;
        Extended := True;
      end;
    GDK_Page_Down:      
      begin
        VirtualKey := VK_NEXT;
        Extended := True;
      end;
    GDK_End:            
      begin
        VirtualKey := VK_END;
        Extended := True;
      end;
    
    GDK_KP_Insert:      VirtualKey := VK_INSERT;
    GDK_KP_Home:        VirtualKey := VK_HOME;
    GDK_KP_Left:        VirtualKey := VK_LEFT;
    GDK_KP_Up:          VirtualKey := VK_UP;
    GDK_KP_Right:       VirtualKey := VK_RIGHT;
    GDK_KP_Down:        VirtualKey := VK_DOWN;
    GDK_KP_Page_Up:     VirtualKey := VK_PRIOR;
    GDK_KP_Page_Down:   VirtualKey := VK_NEXT;
    GDK_KP_End:         VirtualKey := VK_END;

    GDK_Num_Lock:       VirtualKey := VK_NUMLOCK;
    GDK_KP_F1:          VirtualKey := VK_F1;
    GDK_KP_F2:          VirtualKey := VK_F2;
    GDK_KP_F3:          VirtualKey := VK_F3;
    GDK_KP_F4:          VirtualKey := VK_F4;

    GDK_KP_Equal:      
      begin
        VirtualKey := VK_EQUAL;
        if not CtrlDown then KeyCode := Ord('=');
      end;
    GDK_KP_Multiply:      
      begin
        VirtualKey := VK_MULTIPLY;
        if not CtrlDown then KeyCode := Ord('*');
      end;
    GDK_KP_Add:      
      begin
        VirtualKey := VK_ADD;
        if not CtrlDown then KeyCode := Ord('+');
      end;
    GDK_KP_Separator:      
      begin
        VirtualKey := VK_SEPARATOR;
//        if not CtrlDown then KeyCode := Ord('????'); 
      end;
    GDK_KP_Subtract:      
      begin
        VirtualKey := VK_SUBTRACT;
        if not CtrlDown then KeyCode := Ord('-');
      end;
    GDK_KP_Decimal:      
      begin
        VirtualKey := VK_DECIMAL;
        if not CtrlDown then KeyCode := Ord('.');
      end;
    GDK_KP_Divide:      
      begin
        VirtualKey := VK_DIVIDE;
        Extended := True;
        if not CtrlDown then KeyCode := Ord('/');
      end;
    GDK_KP_0..GDK_KP_9: 
      begin
        VirtualKey := VK_NUMPAD0 + (Event^.KeyVal - GDK_KP_0);
        if not CtrlDown then KeyCode := Ord('0') + (Event^.KeyVal - GDK_KP_0);
      end;

    GDK_BackSpace:      VirtualKey := VK_BACK;


    GDK_Clear_Key:      VirtualKey := VK_CLEAR;
    GDK_Pause:          VirtualKey := VK_PAUSE;
    GDK_Scroll_Lock:    VirtualKey := VK_SCROLL;
    GDK_Sys_Req:        VirtualKey := VK_SNAPSHOT;
    GDK_Escape:         VirtualKey := VK_ESCAPE;
    GDK_Delete_Key:     VirtualKey := VK_DELETE;
    
//    GDK_Multi_key = $FF20;
//    GDK_SingleCandidate = $FF3C;
//    GDK_MultipleCandidate = $FF3D;
//    GDK_PreviousCandidate = $FF3E;

    GDK_Kanji:          VirtualKey := VK_KANJI;

//    GDK_Muhenkan = $FF22;
//    GDK_Henkan_Mode = $FF23;
//    GDK_Henkan = $FF23;
//    GDK_Romaji = $FF24;
//    GDK_Hiragana = $FF25;
//    GDK_Katakana = $FF26;
//    GDK_Hiragana_Katakana = $FF27;
//    GDK_Zenkaku = $FF28;
//    GDK_Hankaku = $FF29;
//    GDK_Zenkaku_Hankaku = $FF2A;
//    GDK_Touroku = $FF2B;
//    GDK_Massyo = $FF2C;
//    GDK_Kana_Lock = $FF2D;
//    GDK_Kana_Shift = $FF2E;
//    GDK_Eisu_Shift = $FF2F;
//    GDK_Eisu_toggle = $FF30;
//    GDK_Zen_Koho = $FF3D;
//    GDK_Mae_Koho = $FF3E;

    GDK_Select:         VirtualKey := VK_SELECT;
    GDK_Print:          VirtualKey := VK_PRINT;
    GDK_Execute:        VirtualKey := VK_EXECUTE;

    GDK_Menu:           VirtualKey := VK_MENU;
//    GDK_Find = $FF68;
    GDK_Cancel:         VirtualKey := VK_CANCEL;
    GDK_Help:           VirtualKey := VK_HELP;
    GDK_Break:          VirtualKey := VK_CANCEL;
    GDK_Mode_switch:    VirtualKey := VK_MODECHANGE;
//    GDK_script_switch = $FF7E;

    GDK_Caps_Lock:      VirtualKey := VK_CAPITAL;
//    GDK_Shift_Lock = $FFE6;

    GDK_Shift_L:
      begin
        VirtualKey := VK_SHIFT;
      end;
    GDK_Shift_R:
      begin
        VirtualKey := VK_SHIFT;
        Extended := True;
      end;
    GDK_Control_L:
      begin
        VirtualKey := VK_CONTROL;
      end;
    GDK_Control_R:
      begin
        VirtualKey := VK_CONTROL;
        Extended := True;
      end;
    GDK_Alt_L:
      begin
        SysKey := True;
        VirtualKey:= VK_MENU; 
      end; 
    GDK_Alt_R:
      begin
        SysKey := True;
        VirtualKey:= VK_MENU; 
      end; 
    
    // Function keys
    GDK_F1..GDK_F24:    VirtualKey := VK_F1 + (Event^.KeyVal - GDK_F1);
  end;
  
  if VirtualKey=VK_UNKNOWN then
    // map all other keys to VK_IRREGULAR + 
    VirtualKey := VK_IRREGULAR + KeyCode;

//writeln('GetGTKKeyInfo KeyCode=',KeyCode,' VirtualKey=',VirtualKey
//,' SysKey=',SysKey,' Extended=',Extended,' Toggle=',Toggle);

//  Assert(False, Format('Trace:[GetGTKKeyInfo] Event^.KeyVal %d, Event^.State %d, KeyCode %d, VirtualKey %d, SysKey %d, Extended %d, CtrlDown %d', [Integer(Event^.KeyVal), Integer(Event^.State), Integer(KeyCode), Integer(VirtualKey), Integer(SysKey), Integer(Extended), Integer(CtrlDown)]));
end;

{------------------------------------------------------------------------------
  Procedure: DeliverMessage
  Params:    Message: the message to process
  Returns:   True if handled

  Generic function whih calls the WindowProc if defined, otherwise the 
  dispatcher
 ------------------------------------------------------------------------------}
function DeliverMessage(const Target: Pointer; var Message): Integer;
begin
(*
  // Logging
  if Target = nil 
  then Assert(False, 'Trace:[DeliverMessage] nil')
  else Assert(False, Format('Trace:[DeliverMessage] %s --> %d'
     , [TObject(Target).ClassName, TLMessage(Message).msg]));
*)  
//  Assert(False, 'Trace:Delivering message');
if Target=nil then writeln('[DeliverMessage] nil');
  if TObject(Target) is TControl 
  then begin
//    Assert(False, 'Trace:Calling TControl');
    TControl(Target).WindowProc(TLMessage(Message));
  end else begin
//    Assert(False, 'Trace:Calling TObject');
    TObject(Target).Dispatch(TLMessage(Message));
  end;
  
  Result := TLMessage(Message).Result;
end;

{------------------------------------------------------------------------------
  Function: ObjectToGTKObject
  Params: AObject: A LCL Object
  Returns:  The GTKObject of the given object

  Returns the GTKObject of the given object, nil if no object available
 ------------------------------------------------------------------------------}
function ObjectToGTKObject(const AObject: TObject): gtk_object;
var
  handle : HWND;
begin
  Handle := 0;
  if not assigned(AObject) then
  begin
    assert (false, 'TRACE:  [ObjectToGtkObject] Object not assigned');
  end
  else if (AObject is TWinControl) then
  begin
    if TWinControl(AObject).HandleAllocated then
      handle := TWinControl(AObject).Handle;
  end
  else if (AObject is TMenuItem) then
  begin 
    if TMenuItem(AObject).HandleAllocated then
      handle := TMenuItem(AObject).Handle;
  end
  else if (AObject is TMenu) then
  begin 
    if TMenu(AObject).HandleAllocated then
      handle := TMenu(AObject).Items.Handle;
  end
  else if (AObject is TCommonDialog) then
  begin
    {if TCommonDialog(AObject).HandleAllocated then }
    handle := TCommonDialog(AObject).Handle;
  end
  else begin
    Assert(False, Format('Trace:  [ObjectToGtkObject] Message received with unhandled class-type <%s>', [AObject.ClassName])); 
  end;
  Result := gtk_object (handle);
  if handle = 0 then
    Assert (false, 'Trace:  [ObjectToGtkObject]****** Warning: handle = 0 *******');
end;


(***********************************************************************
  Widget member functions
************************************************************************)

// ----------------------------------------------------------------------
// Creates a WinWidget info structure for the given widget
// Info needed by the API of a HWND (=Widget) 
//
// This structure obsoletes:
//   "core-child", "fixed", "class"
// ----------------------------------------------------------------------
function CreateWidgetInfo(const Widget: Pointer): PWinWidgetInfo;
begin
  if Widget = nil
  then begin
    Result := nil;
  end
  else begin
    New(Result);
    FillChar(Result^, SizeOf(Result^), 0);
    gtk_object_set_data(Widget, 'widgetinfo', Result);
  end;
end;

function GetWidgetInfo(const Widget: Pointer; const Create: Boolean): PWinWidgetInfo;
begin
  if Widget = nil
  then begin
    Result := nil;
  end
  else begin
    Result := gtk_object_get_data(Widget, 'widgetinfo');
    if (Result = nil) and Create 
    then Result := CreateWidgetInfo(Widget);
  end;
end;

// ----------------------------------------------------------------------
// the core_child widget points to the actual widget which implements the 
// functionality we needed. It is mainly used in composed controls like
// a listbox. In that case the core_child is the listbox, where a scrolling
// widget is main.
// ----------------------------------------------------------------------
function GetCoreChildWidget(const Widget: Pointer): Pointer;
begin
  Result:= gtk_object_get_data(Widget, 'core_child');
  if Result = nil then Result := Widget;
end;

procedure SetCoreChildWidget(const ParentWidget, ChildWidget: Pointer);
begin
  if (ParentWidget <> nil) and (ChildWidget <> nil) then
  gtk_object_set_data(ParentWidget, 'core_child', ChildWidget);
end;

// ----------------------------------------------------------------------
// the main widget is the widget passed as handle to the winAPI
// main data is stored in the fixed form to get a reference to its parent
// ----------------------------------------------------------------------
function GetMainWidget(const Widget: Pointer): Pointer;
begin
  Result := gtk_object_get_data(Widget, 'Main');
  if Result = nil then Result := Widget; // the widget is the main widget itself.
end;

procedure SetMainWidget(const ParentWidget, ChildWidget: Pointer);
begin
  if (ParentWidget <> nil) and (ChildWidget <> nil) then
  gtk_object_set_data(ChildWidget, 'Main', ParentWidget);
end;

// ----------------------------------------------------------------------
// the fixed widget is the container for controls. By default a widget
// scales/places a control. whith the use of a fixed we can place them.
// ----------------------------------------------------------------------
function GetFixedWidget(const Widget: Pointer): Pointer;
begin
  Result := gtk_object_get_data(Widget, 'Fixed');
end;

procedure SetFixedWidget(const ParentWidget, FixedWidget: Pointer);
begin
  if (ParentWidget <> nil) and (FixedWidget <> nil) then
  gtk_object_set_data(ParentWidget, 'Fixed', FixedWidget);
end;

// ----------------------------------------------------------------------
// Some need the LCLobject which created this widget.
// 
// MWE: IMO this shouldn't be needed
// ----------------------------------------------------------------------
procedure SetLCLObject(const Widget: Pointer; const AnObject: TObject);
begin
  if (Widget <> nil) then
  gtk_object_set_data(Widget, 'Class', Pointer(AnObject));
end;

function GetLCLObject(const Widget: Pointer): TObject;
begin
  Result := TObject(gtk_object_get_data(Widget, 'Class'));
end;

// ----------------------------------------------------------------------
// The Accelgroup and AccelKey is needed by menus
// ----------------------------------------------------------------------
procedure SetAccelGroup(const Widget: Pointer; const AnAccelGroup: Pointer);
begin
  if (Widget <> nil) then
  gtk_object_set_data(Widget, 'AccelGroup', AnAccelGroup);
end;

function GetAccelGroup(const Widget: Pointer): Pointer;
begin
  Result := gtk_object_get_data(Widget, 'AccelGroup');
end;

procedure SetAccelKey(const Widget: Pointer; const AKey: Integer);
begin
  if (Widget <> nil) then
  gtk_object_set_data(Widget, 'AccelKey', Pointer(AKey));
end;

function GetAccelKey(const Widget: Pointer): Integer;
begin
  Result := Integer(gtk_object_get_data(Widget, 'AccelKey'));
end;

// ----------------------------------------------------------------------
// OBSOLETE ???
// ----------------------------------------------------------------------

Procedure ReportnotObsolete(Texts : String);
Begin
Writeln('*********************************************');
Writeln('*********************************************');
Writeln('*************Non-Obsolete report*************');
Writeln('*********************************************');
Writeln('*************'+Texts+'*is being used yet.****');
Writeln('*******Please remove this function from******');
Writeln('*******the obsolete section in gtkproc.inc***');
Writeln('*********************************************');
Writeln('*********************************************');
Writeln('*********************************************');
Writeln('*********************************************');

end;

function TGDKColorToTColor(value : TGDKColor) : TColor;
begin
ReportNotObsolete('TgdkColortoTColor');
   result := ((value.blue div 257) shl 16) + ((value.green div 257) shl 8) + (value.red div 257);
end;

function TColortoTGDKColor(value : TColor) : TGDKColor;
var
   newColor : TGDKColor;
   num : LongInt;                                          // example assume value := $00FF2510

begin
ReportNotObsolete('TColortoTgdkColor');
   NewColor.pixel := 0;
                                                           // value - $00FF2510
   num := (value shr 8) shl 8;                             //   num - $00FF2500
   newColor.red := (value - num) * 257;                    //   red - $00000010  * 257
      
                                                           // value - $00FF2510
   num := (value shr 16) shl 8;                            //   num - $0000FF00
                                                           //value2 - $0000FF25
   newColor.green :=  ((value shr 8) - num) * 257;         // green = $00000025  * 257

                                                           // value - $00FF2510
   newColor.blue := (value shr 16) * 257;                  //  blue - $000000FF  * 257

   result := newColor;
end;




Function GetPen(f : PgdkDrawable; Value : TgdkColor) : pgdkGC;
Var
gc : PgdkGC;
Begin
ReportNotObsolete('GetPen');

gdk_color_alloc (gdk_colormap_get_system (), @value);
gc := gdk_gc_new (f);
gdk_gc_set_foreground (gc, @value);
result := gc;

Assert(False, 'Trace:OBSOLETE gtkproc.inc GetPen');
end;


{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

 { =============================================================================

  $Log$
  Revision 1.18  2001/06/20 17:00:20  lazarus
  MG: alternative key bugfix, shiftstate bugfix

  Revision 1.17  2001/06/20 13:35:51  lazarus
  MG: added VK_IRREGULAR and key grabbing

  Revision 1.16  2001/06/16 09:14:39  lazarus
  MG: added lazqueue and used it for the messagequeue

  Revision 1.15  2001/06/05 10:32:06  lazarus
  MG: small bugfixes for bitbtn, handles

  Revision 1.14  2001/03/21 23:48:29  lazarus
  MG: fixed window positions

  Revision 1.12  2001/03/19 14:44:22  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.10  2001/01/25 21:38:57  lazarus
  MWE:
    * fixed lil bug I commetted yesterday (listbox crash)

  Revision 1.9  2001/01/24 23:26:40  lazarus
  MWE:
    = moved some types to gtkdef
    + added WinWidgetInfo
    + added some initialization to Application.Create

  Revision 1.8  2001/01/23 23:33:55  lazarus
  MWE:
    - Removed old LM_InvalidateRect
    - did some cleanup in old  code
    + added some comments  on gtkobject data (gtkproc)

  Revision 1.7  2001/01/08 21:59:36  lazarus
  MWE:
    ~ applieed patch from Peter Vreman to reflect compiler fix

  Revision 1.6  2000/12/19 18:43:13  lazarus
  Removed IDEEDITOR.  This causes the PROJECT class to not function.
  Saving projects no longer works.

  I added TSourceNotebook and TSourceEditor.  They do all the work for saving/closing/opening units.  Somethings work but they are in early development.
  Shane

  Revision 1.5  2000/10/09 22:50:32  lazarus
  MWE:
    * fixed some selection code
    + Added selection sample

  Revision 1.4  2000/09/10 23:08:31  lazarus
  MWE:
    + Added CreateCompatibeleBitamp function
    + Updated TWinControl.WMPaint
    + Added some checks to avoid gtk/gdk errors
    - Removed no fixed warning from GetDC
    - Removed some output

  Revision 1.3  2000/08/10 10:55:45  lazarus
  Changed TCustomDialog to TCommonDialog
  Shane

  Revision 1.2  2000/07/30 21:48:34  lazarus
  MWE:
    = Moved ObjectToGTKObject to GTKProc unit
    * Fixed array checking in LoadPixmap
    = Moved LM_SETENABLED to API func EnableWindow and EnableMenuItem
    ~ Some cleanup

  Revision 1.1  2000/07/13 10:28:29  michael
  + Initial import

  Revision 1.8  2000/06/29 18:08:56  lazarus
  Shane
    Looking for the editor problem I made a few changes.  I changed everything back to the original though.

  Revision 1.7  2000/06/19 18:21:22  lazarus
  Spinedit was never getting created
  Shane

  Revision 1.6  2000/06/14 21:51:27  lazarus
  MWE:
    + Added menu accelerators. Not finished

  Revision 1.5  2000/05/11 22:04:16  lazarus
  MWE:
    + Added messagequeue
    * Recoded SendMessage and Peekmessage
    + Added postmessage
    + added DeliverPostMessage

  Revision 1.4  2000/05/10 22:52:58  lazarus
  MWE:
    = Moved some global api stuf to gtkobject

  Revision 1.3  2000/05/10 01:45:12  lazarus
  Replaced writelns with Asserts.
  Put ERROR and WARNING messages back to writelns.            CAW

  Revision 1.2  2000/05/08 15:56:59  lazarus
  MWE:
    + Added support for mwedit92 in Makefiles
    * Fixed bug # and #5 (Fillrect)
    * Fixed labelsize in ApiWizz
    + Added a call to the resize event in WMWindowPosChanged

  Revision 1.1  2000/03/30 22:51:42  lazarus
  MWE:
    Moved from ../../lcl

  Revision 1.11  2000/03/30 21:57:44  lazarus
  MWE:
    + Added some general functions to Get/Set the Main/Fixed/CoreChild
      widget
    + Started with graphic scalig/depth stuff. This is way from finished

  Hans-Joachim Ott <hjott@compuserve.com>:
    + Added some improvements for TMEMO

  Revision 1.10  2000/03/19 23:01:43  lazarus
  MWE:
    = Changed splashscreen loading/colordepth
    = Chenged Save/RestoreDC to platform  dependent, since they are
      relative to a DC

  Revision 1.9  2000/03/16 23:58:46  lazarus
  MWE:
    Added TPixmap for XPM support

  Revision 1.8  2000/03/08 23:57:38  lazarus
  MWE:
    Added SetSysColors
    Fixed TEdit text bug (thanks to hans-joachim ott <hjott@compuserve.com>)
    Finished GetKeyState
    Added changes from Peter Dyson <peter@skel.demon.co.uk>
    - a new GetSysColor
    - some improvements on ExTextOut

  Revision 1.7  2000/03/03 22:58:26  lazarus
  MWE:
    Fixed focussing problem.
      LM-FOCUS was bound to the wrong signal
    Added GetKeyState api func.
      Now LCL knows if shift/trl/alt is pressed (might be handy for keyboard
      selections ;-)

  Revision 1.6  2000/01/22 20:07:47  lazarus
  Some cleanups. It needs much more cleanup than this.
  Worked around a compiler bug (?) in mwCustomEdit.
  Reverted some changes to font generation and increased font size.

  Revision 1.5  1999/09/17 14:58:54  lazarus
  Changes made to editor.pp
  Can now press END and some other similiar keys work.  Typing works,
  but doesn't paint correctly yet.

  Revision 1.4  1999/07/31 06:39:30  lazarus

       Modified the IntSendMessage3 to include a data variable. It isn't used
       yet but will help in merging the Message2 and Message3 features.

       Adjusted TColor routines to match Delphi color format

       Added a TGdkColorToTColor routine in gtkproc.inc

       Finished the TColorDialog added to comDialog example.        MAH

 }
