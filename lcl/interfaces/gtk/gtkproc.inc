{******************************************************************************
                        Misc Support Functs  
 ******************************************************************************
   used by:
     GTKObject
     GTKWinAPI
     GTKCallback
 ******************************************************************************
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
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

{------------------------------------------------------------------------------
  Function: FindChar
  Params:  Width, Height: Size of the image
           Depth:         Depth of the image
  Returns: a GDIRawImage

  Creates a RawImage
 ------------------------------------------------------------------------------}
function FindChar(c: char; p:PChar; Max: integer): integer;
begin
  Result:=0;
  while (Result<Max) do begin
    if p[Result]<>c then
      inc(Result)
    else
      exit;
  end;
  Result:=-1;
end;


{------------------------------------------------------------------------------
  function GtkWidgetIsA(Widget: PGtkWidget; AType: TGtkType): boolean;

  The GTK_IS_XXX macro functions in the fpc gtk1.x bindings are not correct.
  They just test the highest level.
  This function checks just like the real C macros.
 ------------------------------------------------------------------------------}
function GtkWidgetIsA(Widget: PGtkWidget; AType: TGtkType): boolean;
begin
  Result:=(Widget<>nil)
    and (PGtkTypeObject(Widget)^.klass<>nil)
    and gtk_type_is_a(PGtkTypeClass(PGtkTypeObject(Widget)^.klass)^.thetype,
                      AType);
end;

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
  Function: AllocGDKColor
  Params:  AColor: A RGB color (TColor)
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
  Returns: True if succesful

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
      if (SourceDC^.GC = nil) or (Drawable = nil) then
        GC := nil
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
      ClipRegion := SourceDC^.ClipRegion;
      {DeleteObject(ClipRegion);
      ClipRegion := 0;
      If SourceDC^.ClipRegion <> 0 then
        SelectClipRGN(hDC(DestinationDC), SourceDC^.ClipRegion);}
      SavedContext := nil;
    end;
  end;
  Assert(False, Format('Trace:< [CopyDCData] DestDC:0x%x, SourceDC:0x%x --> %d', [Integer(DestinationDC), Integer(SourceDC), Integer(Result)]));
end;

Procedure SelectGDIRegion(const DC: HDC);
begin
  with PDeviceContext(DC)^ do
  begin
    gdk_gc_set_clip_region(gc,  nil);
    gdk_gc_set_clip_rectangle (gc,  nil);
    If ClipRegion <> 0 then
      gdk_gc_set_clip_region(gc,  PGDIObject(ClipRegion)^.GDIRegionObject);
  end;
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
    SelectGDIRegion(DC);
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
    gdk_gc_set_fill(GC, GDK_SOLID);
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
 ------------------------------------------------------------------------------}
function KeyToListCode(KeyCode, VirtKeyCode: Word; Extended: boolean): integer;
begin
  if VirtKeyCode = VK_UNKNOWN then
    Result := KEYMAP_VKUNKNOWN and KeyCode
  else
    Result := VirtKeyCode;
  if Extended then
    Result := Result or KEYMAP_EXTENDED;
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
var ShiftState: TShiftState;
begin
  VirtualKey := VK_UNKNOWN;
  KeyCode := $FFFF;
  SysKey := (Event^.State and GDK_MOD1_MASK) <> 0;
  ShiftState := GTKEventState2ShiftState(Event^.State);
  Extended := False;
  Toggle := False;
  
  {if Event^.Length>0 then begin
    writeln('GetGTKKeyInfo Event^.KeyVal=',Event^.KeyVal,
      ' Event^.Length=',Event^.Length,' ',ord(Event^.theString[0])
    );
  end;}

  case Event^.KeyVal of
    // Normal ASCII chars
    32..255:
      begin
        { Assign key code}
        KeyCode := Event^.KeyVal;
        
        //TODO: create VK_ code --> [*] and [8] have a different KeyCode but same VK_ code
        case Chr(KeyCode) of
          '@': VirtualKey:=VK_AT;
          'A'..'Z', 
          '0'..'9', 
          ' ':      VirtualKey := KeyCode;
          'a'..'z': VirtualKey := KeyCode - Ord('a') + Ord('A');
          '/': VirtualKey := VK_SLASH;
          ',': VirtualKey := VK_COMMA;
          '=': VirtualKey := VK_EQUAL;
          '*': VirtualKey := VK_MULTIPLY;
          '+': VirtualKey := VK_ADD;
          '-': VirtualKey := VK_SUBTRACT;
          '.': VirtualKey := VK_POINT;
        end;
        { look for control code }
        if (ssCtrl in ShiftState) and (Chr(KeyCode) in ['@'..'Z']) then
          Dec(KeyCode, Ord('@'));
      end;
      
    GDK_dead_circumflex:
      begin
        KeyCode := Ord('^');
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
        if not (ssCtrl in ShiftState) then KeyCode := Ord('=');
      end;
    GDK_KP_Multiply:      
      begin
        VirtualKey := VK_MULTIPLY;
        if not (ssCtrl in ShiftState) then KeyCode := Ord('*');
      end;
    GDK_KP_Add:      
      begin
        VirtualKey := VK_ADD;
        if not (ssCtrl in ShiftState) then KeyCode := Ord('+');
      end;
    GDK_KP_Separator:      
      begin
        VirtualKey := VK_SEPARATOR;
//        if not CtrlDown then KeyCode := Ord('????'); 
      end;
    GDK_KP_Subtract:      
      begin
        VirtualKey := VK_SUBTRACT;
        if not (ssCtrl in ShiftState) then KeyCode := Ord('-');
      end;
    GDK_KP_Decimal:      
      begin
        VirtualKey := VK_DECIMAL;
        if not (ssCtrl in ShiftState) then KeyCode := Ord('.');
      end;
    GDK_KP_Divide:      
      begin
        VirtualKey := VK_DIVIDE;
        Extended := True;
        if not (ssCtrl in ShiftState) then KeyCode := Ord('/');
      end;
    GDK_KP_0..GDK_KP_9: 
      begin
        VirtualKey := VK_NUMPAD0 + (Event^.KeyVal - GDK_KP_0);
        if not (ssCtrl in ShiftState) then
          KeyCode := Ord('0') + (Event^.KeyVal - GDK_KP_0);
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
        Extended := True;
      end;
    
    // Function keys
    GDK_F1..GDK_F24:    VirtualKey := VK_F1 + (Event^.KeyVal - GDK_F1);
  end;
  
  if VirtualKey=VK_UNKNOWN then begin
    // map all other keys to VK_IRREGULAR + KeyCode
    if ShortInt(KeyCode)>=0 then
      VirtualKey := MapIrregularVirtualKey(VK_IRREGULAR + KeyCode)
    else
      VirtualKey := VK_IRREGULAR;
  end;
end;


{------------------------------------------------------------------------------
  Procedure: StoreCommonDialogSetup
  Params:    ADialog: TCommonDialog
  Returns:   none

  Stores the size of a TCommonDialog.
 ------------------------------------------------------------------------------}
procedure StoreCommonDialogSetup(ADialog: TCommonDialog);
var DlgWindow: PGtkWidget;
begin
  if (ADialog=nil) or (ADialog.Handle=0) then exit;
  DlgWindow:=PGtkWidget(ADialog.Handle);
  if DlgWindow^.Allocation.Width>0 then
    ADialog.Width:=DlgWindow^.Allocation.Width;
  if DlgWindow^.Allocation.Height>0 then
    ADialog.Height:=DlgWindow^.Allocation.Height;
end;

{------------------------------------------------------------------------------
  Procedure: DestroyCommonDialogAddOns
  Params:    ADialog: TCommonDialog
  Returns:   none

  Free the memory of additional data of a TCommonDialog
 ------------------------------------------------------------------------------}
procedure DestroyCommonDialogAddOns(ADialog: TCommonDialog);
var
  DlgWindow: PGtkWidget;
  HistoryList: TList; // list of TFileSelHistoryListEntry
  FilterList: TList; // list of TFileSelFilterListEntry
  AHistoryEntry: PFileSelHistoryEntry;
  AFilterEntry: PFileSelFilterEntry;
  i: integer;
begin
  if (ADialog=nil) or (ADialog.Handle=0) then exit;
  DlgWindow:=PGtkWidget(ADialog.Handle);
  if ADialog is TOpenDialog then begin
  
    // free history
    HistoryList:=TList(gtk_object_get_data(PGtkObject(DlgWindow),
                                           'LCLHistoryList'));
    if HistoryList<>nil then begin
      for i:=0 to HistoryList.Count-1 do begin
        AHistoryEntry:=PFileSelHistoryEntry(HistoryList[i]);
        StrDispose(AHistoryEntry^.Filename);
        AHistoryEntry^.Filename:=nil;
        Dispose(AHistoryEntry);
      end;
      HistoryList.Free;
      gtk_object_set_data(PGtkObject(DlgWindow),'LCLHistoryList',nil);
    end;

    // free filter
    FilterList:=TList(gtk_object_get_data(PGtkObject(DlgWindow),
                                          'LCLFilterList'));
    if FilterList<>nil then begin
      for i:=0 to FilterList.Count-1 do begin
        AFilterEntry:=PFileSelFilterEntry(FilterList[i]);
        StrDispose(AFilterEntry^.Description);
        AFilterEntry^.Description:=nil;
        StrDispose(AFilterEntry^.Mask);
        AFilterEntry^.Mask:=nil;
        Dispose(AFilterEntry);
      end;
      FilterList.Free;
      gtk_object_set_data(PGtkObject(DlgWindow),'LCLFilterList',nil);
    end;
  end;
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
  if Target=nil then writeln('[DeliverMessage] Target = nil');
  if TObject(Target) is TControl then
  begin
    TControl(Target).WindowProc(TLMessage(Message));
  end
  else
  begin
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
  if (ChildWidget=nil) then
    raise Exception.Create('SetMainWidget ChildWidget=nil');
  if (ParentWidget=ChildWidget) then
    raise Exception.Create('SetMainWidget ChildWidget=ParentWidget');
  if (ParentWidget <> nil) and (ChildWidget <> nil) then
    gtk_object_set_data(ChildWidget, 'Main', ParentWidget);
end;

{ ------------------------------------------------------------------------------
 Get the fixed widget of a widget.
 Every LCL control with a clientarea, has at least a main widget for the control
 and a fixed widget for the client area.
------------------------------------------------------------------------------ }
function GetFixedWidget(const Widget: Pointer): Pointer;
begin
  Result := gtk_object_get_data(Widget, 'Fixed');
end;

{ ------------------------------------------------------------------------------
 Set the fixed widget of a widget.
 Every LCL control with a clientarea, has at least a main widget for the control
 and a fixed widget for the client area.
------------------------------------------------------------------------------ }
procedure SetFixedWidget(const ParentWidget, FixedWidget: Pointer);
begin
//writeln('[gtkproc: SetFixedWidget] Parent=',HexStr(Cardinal(ParentWidget),8),
//' Fixed=',HexStr(Cardinal(FixedWidget),8));
  if (ParentWidget=nil) then
    raise Exception.Create('SetFixedWidget ParentWidget=nil');
  if (ParentWidget=FixedWidget) then
    raise Exception.Create('SetFixedWidget FixedWidget=ParentWidget');
  if (ParentWidget <> nil) and (FixedWidget <> nil) then
    gtk_object_set_data(ParentWidget, 'Fixed', FixedWidget);
end;

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

function GetWidgetInfo(const Widget: Pointer;
  const Create: Boolean): PWinWidgetInfo;
var MainWidget: PGtkObject;
begin
  if Widget = nil then begin
    Result := nil;
  end
  else begin
    MainWidget:=GetMainWidget(Widget);
    if MainWidget=nil then MainWidget:=Widget;
    Result := gtk_object_get_data(MainWidget, 'widgetinfo');
    if (Result = nil) and Create then
      Result := CreateWidgetInfo(MainWidget);
  end;
end;

procedure FreeWinWidgetInfo(Widget: Pointer);
var
  WinWidgetInfo: PWinWidgetInfo;
begin
  if Widget=nil then exit;
  WinWidgetInfo := gtk_object_get_data(Widget, 'widgetinfo');
  if WinWidgetInfo<>nil then begin
    Dispose(WinWidgetInfo);
    gtk_object_set_data(Widget,'widgetinfo',nil);
  end;
end;

{-------------------------------------------------------------------------------
  procedure DestroyWidget(Widget: PGtkWidget);


-------------------------------------------------------------------------------}
procedure DestroyWidget(Widget: PGtkWidget);
begin
  FreeWinWidgetInfo(Widget);
  gtk_widget_destroy(Widget);
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

{------------------------------------------------------------------------------
  UpdateNoteBookClientWidget
  Params: ANoteBook: TObject

  The client widget of a notebook is fixed widget of the current page.
  This procedure updates the 'Fixed' object data.
------------------------------------------------------------------------------}
procedure UpdateNoteBookClientWidget(ANoteBook: TObject);
var Index: integer;
  NoteBookWidget, PanelWidget, ClientWidget: PGtkWidget;
begin
  if not TCustomNotebook(ANoteBook).HandleAllocated then exit;
  NoteBookWidget:=PGtkWidget(TCustomNotebook(ANoteBook).Handle);
  ClientWidget:=nil;
  Index:=gtk_notebook_get_current_page(PGtkNoteBook(NoteBookWidget));
  if Index>=0 then begin
    PanelWidget:=gtk_notebook_get_nth_page(PGtkNoteBook(NoteBookWidget),Index);
    if PanelWidget<>nil then
      ClientWidget:=GetFixedWidget(PanelWidget);
  end;
  SetFixedWidget(NoteBookWidget,ClientWidget);
end;

{-------------------------------------------------------------------------------
 Some need the LCLobject which created this widget.

-------------------------------------------------------------------------------}
procedure SetLCLObject(const Widget: Pointer; const AnObject: TObject);
begin
  if (Widget <> nil) then
    gtk_object_set_data(Widget, 'Class', Pointer(AnObject));
end;

function GetLCLObject(const Widget: Pointer): TObject;
begin
  Result := TObject(gtk_object_get_data(Widget, 'Class'));
end;

{-------------------------------------------------------------------------------
 Some need the HiddenLCLobject which created a parent of this widget.

-------------------------------------------------------------------------------}
procedure SetHiddenLCLObject(const Widget: Pointer; const AnObject: TObject);
begin
  if (Widget <> nil) then
    gtk_object_set_data(Widget, 'LCLHiddenClass', Pointer(AnObject));
end;

function GetHiddenLCLObject(const Widget: Pointer): TObject;
begin
  Result := TObject(gtk_object_get_data(Widget, 'LCLHiddenClass'));
end;

{-------------------------------------------------------------------------------
  GetWidgetScreenPos

  Returns the absolute left top position of a widget on the screen.
-------------------------------------------------------------------------------}
function GetWidgetOrigin(TheWidget: PGtkWidget): TPoint;
var
  TheWindow: PGdkWindow;
begin
  TheWindow:=TheWidget^.Window;
  gdk_window_get_origin(TheWindow,@Result.X,@Result.Y);
  // check if the gdkwindow is the clientwindow of the parent
  if gtk_widget_get_parent_window(TheWidget)=TheWindow then begin
    // the widget is using its parent window
    // -> adjust the coordinates
    inc(Result.X,TheWidget^.Allocation.X);
    inc(Result.Y,TheWidget^.Allocation.Y);
  end;
end;

{-------------------------------------------------------------------------------
  GetWidgetClientScreenPos

  Returns the absolute left top position of a widget's client area
  on the screen.
-------------------------------------------------------------------------------}
function GetWidgetClientOrigin(TheWidget: PGtkWidget): TPoint;
var
  ClientWidget: PGtkWidget;
begin
  ClientWidget:=GetFixedWidget(TheWidget);
  if ClientWidget<>nil then begin
    gdk_window_get_origin(ClientWidget^.Window,@Result.X,@Result.Y);
  end else begin
    Result:=GetWidgetOrigin(TheWidget);
  end;
end;

{-------------------------------------------------------------------------------
  TranslateGdkPointToClientArea

  Translates SourcePos relative to SourceWindow to a coordinate relative to the
  client area of the LCL WinControl.
-------------------------------------------------------------------------------}
function TranslateGdkPointToClientArea(SourceWindow: PGdkWindow;
  SourcePos: TPoint;  DestinationWidget: PGtkWidget): TPoint;
var
  SrcWindowOrigin: TPoint;
  ClientAreaWindowOrigin: TPoint;
  Src2ClientAreaVector: TPoint;
begin
  gdk_window_get_origin(SourceWindow,@SrcWindowOrigin.X,@SrcWindowOrigin.Y);
  ClientAreaWindowOrigin:=GetWidgetClientOrigin(DestinationWidget);
  Src2ClientAreaVector.X:=ClientAreaWindowOrigin.X-SrcWindowOrigin.X;
  Src2ClientAreaVector.Y:=ClientAreaWindowOrigin.Y-SrcWindowOrigin.Y;
  Result.X:=SourcePos.X-Src2ClientAreaVector.X;
  Result.Y:=SourcePos.Y-Src2ClientAreaVector.Y;
end;

{------------------------------------------------------------------------------
  Function: UpdateMouseCaptureControl
  Params: none
  Returns:  none

  Sets MCaptureControl to the current capturing widget.
 ------------------------------------------------------------------------------}
procedure UpdateMouseCaptureControl;
var
  OldMouseCaptureHandle,
  CurMouseCaptureHandle: PGtkWidget;

  procedure NotifyCurCaptureControl;
  var
    CurControl: TObject;
    CaptureChangedMsg: TLMessage;
  begin
    if CurMouseCaptureHandle=nil then exit;
    CurControl:=GetLCLObject(CurMouseCaptureHandle);
    if CurControl=nil then exit;
    with CaptureChangedMsg do begin
      Msg := Msg;
      WParam := LM_CAPTURECHANGED;
      LParam := 0;
      Result := longint(OldMouseCaptureHandle); // ToDo: 64bit faulty
    end;
    DeliverMessage(CurControl,CaptureChangedMsg);
  end;

begin
  OldMouseCaptureHandle:=PGtkWidget(MCaptureHandle);
  CurMouseCaptureHandle:=gtk_grab_get_current;
  if OldMouseCaptureHandle<>CurMouseCaptureHandle then begin
    // mouse capture widget has changed
    MCaptureHandle:=integer(CurMouseCaptureHandle); // ToDo: 64bit faulty
    NotifyCurCaptureControl;
  end;
end;

{------------------------------------------------------------------------------
  procedure: SetCursor
  Params:  AWinControl : TWinControl
  Returns: Nothing

  Sets the cursor for a widget.
 ------------------------------------------------------------------------------}
procedure SetCursor(AWinControl : TWinControl);

  procedure DoSetCursor(AWindow: PGdkWindow; Cursor: pGDKCursor);
  begin
    if Cursor<>nil then
      gdk_window_set_cursor(AWindow, Cursor);
  end;

  procedure SetDesigningCursor(AWindow: PGdkWindow);
  var
    ChildWindows, ListEntry: PGList;
  begin
    DoSetCursor(AWindow, Cursor_StdArrow);
    
    ChildWindows:=gdk_window_get_children(AWindow);
    ListEntry:=ChildWindows;
    while ListEntry<>nil do begin
      SetDesigningCursor(PGdkWindow(ListEntry^.Data));
      ListEntry:=ListEntry^.Next;
    end;
    g_list_free(ChildWindows);
  end;

var
  AWidget, FixWidget: PGtkWidget;
  AWindow: PGdkWindow;
begin
  If not (AWinControl is TWinControl)
  or (not AWinControl.HandleAllocated)
  then exit;
  
  AWidget:=PGtkWidget(AWinControl.Handle);

  if csDesigning in AWinControl.ComponentState then begin
    AWindow:=AWidget^.Window;
    if AWindow=nil then exit;
    SetDesigningCursor(AWindow);
  end else begin
    FixWidget:=GetFixedWidget(AWidget);
    if FixWidget<>nil then
      AWindow:=FixWidget^.Window
    else
      AWindow:=AWidget^.Window;
    if AWindow=nil then exit;

    case AWinControl.Cursor of
    crAppStart : DoSetCursor (AWindow, Cursor_Watch);
    crArrow    : DoSetCursor (AWindow, Cursor_Arrow);
    crCross    : DoSetCursor (AWindow, Cursor_Cross);
    crHandPoint: DoSetCursor (AWindow, Cursor_hand1);
    crIBeam    : DoSetCursor (AWindow, Cursor_XTerm);
    crHourGlass: DoSetCursor (AWindow, Cursor_Watch);
    crDefault  : DoSetCursor (AWindow, Cursor_StdArrow);
    crHSplit   : DoSetCursor (AWindow, Cursor_HSplit);
    crVSplit   : DoSetCursor (AWindow, Cursor_VSplit);
    crSizeNWSE : DoSetCursor (AWindow, Cursor_SizeNWSE);
    crSizeNS   : DoSetCursor (AWindow, Cursor_SizeNS);
    crSizeNESW : DoSetCursor (AWindow, Cursor_SizeNESW);
    crSizeWE   : DoSetCursor (AWindow, Cursor_SizeWE);
    else
      Exit;
    end;
  end;
end;


{-------------------------------------------------------------------------------
  procedure ConnectSignal(const AnObject:gtk_Object; const ASignal: PChar;
    const ACallBackProc: Pointer; const ReqSignalMask: TGdkEventMask;
    Flags: TConnectSignalFlags);
    
  Connects a gtk signal handler.
-------------------------------------------------------------------------------}
type
  TConnectSignalFlag = (
    csfAfter,            // connect after signal
    csfConnectRealize,   // auto connect realize handler
    csfUpdateSignalMask, // extend signal mask for gdkwindow
    csfDesignOnly        // mark signal as design only
    );
  TConnectSignalFlags = set of TConnectSignalFlag;

  TDesignSignalType = (
    dstUnknown,
    dstMousePress,
    dstMouseMotion,
    dstMouseRelease
    );
  TDesignSignalTypes = set of TDesignSignalType;
  
  TDesignSignalMask = longint;
  
const
  DesignSignalAfter: array[TDesignSignalType] of boolean =
    (false,false,false,false);
  DesignSignalNames: array[TDesignSignalType] of PChar = (
    '',
    'button-press-event',
    'motion-notify-event',
    'button-release-event'
    );
  DesignSignalFuncs: array[TDesignSignalType] of Pointer = (
    nil,
    @gtkMouseBtnPress,
    @GTKMotionNotify,
    @gtkMouseBtnRelease
    );
  DesignSignalMasks: array[TDesignSignalType] of TDesignSignalMask =
    (0,1,2,4);

function DesignSignalNameToType(Name: PChar; After: boolean): TDesignSignalType;
begin
  for Result:=Low(TDesignSignalType) to High(TDesignSignalType) do
    if (AnsiStrComp(DesignSignalNames[Result],Name)=0)
    and (DesignSignalAfter[Result]=After) then exit;
  Result:=dstUnknown;
end;

function GetDesignSignalMask(Widget: PGtkWidget): TDesignSignalMask;
begin
  Result:=TDesignSignalMask(gtk_object_get_data(PGtkObject(Widget),
                                                'LCLDesignMask'));
end;

procedure SetDesignSignalMask(Widget: PGtkWidget; NewMask: TDesignSignalMask);
begin
  gtk_object_set_data(PGtkObject(Widget),'LCLDesignMask',Pointer(NewMask));
end;

function GetDesignOnlySignalFlag(Widget: PGtkWidget;
  DesignSignalType: TDesignSignalType): boolean;
begin
  Result:=(GetDesignSignalMask(Widget)
          and DesignSignalMasks[DesignSignalType])<>0;
end;

procedure ConnectSignal(const AnObject:gtk_Object; const ASignal: PChar;
  const ACallBackProc: Pointer; LCLComponent: TComponent;
  const ReqSignalMask: TGdkEventMask; SFlags: TConnectSignalFlags);
var
  RealizeHandler, Handler: PGTKHandler;
  RealizeID, SignalID: guint;
  WinWidgetInfo: PWinWidgetInfo;
  MainWidget: PGtkWidget;
  OldDesignMask, NewDesignMask: TDesignSignalMask;
  DesignSignalType: TDesignSignalType;
begin
  if ACallBackProc = nil then exit;

  // first loop through the handlers to:
  // - check if a handler already exists
  // - Find the realize handler to change data
  Handler := gtk_object_get_data_by_id (AnObject, gtk_handler_quark);
  SignalID := gtk_signal_lookup(ASignal, GTK_OBJECT_TYPE(AnObject));
  if csfConnectRealize in SFlags then
    RealizeID := gtk_signal_lookup('realize', GTK_OBJECT_TYPE(AnObject))
  else
    RealizeID := 0;
  RealizeHandler := nil;
  DesignSignalType:=DesignSignalNameToType(ASignal,csfAfter in SFlags);

  while (Handler <> nil) do begin
    with Handler^ do
    begin
      // check if signal is already connected
      if  (Id > 0)
      and (Signal_ID = SignalID)
      and (Func = TGTKSignalFunc(ACallBackProc))
      and (func_data = Pointer(LCLComponent))
      and (((flags and bmSignalAfter)<>0)=(csfAfter in SFlags))
      then begin
        Assert(False, Format('Trace:WARNING: [TGTKObject.SetCallback] %s signal <%s> set twice', [LCLComponent.ClassName, ASignal]));
        // signal is already connected
        // update the DesignSignalMask
        if (DesignSignalType<>dstUnknown)
        and (not (csfDesignOnly in SFlags)) then begin
          OldDesignMask:=GetDesignSignalMask(PGtkWidget(AnObject));
          NewDesignMask:=
            OldDesignMask and not DesignSignalMasks[DesignSignalType];
          if OldDesignMask<>NewDesignMask then
            SetDesignSignalMask(PGtkWidget(AnObject),NewDesignMask);
        end;
        Exit;
      end;

      // look for realize handler
      if  (csfConnectRealize in SFlags)
      and (Id > 0)
      and (Signal_ID = RealizeID)
      and (Func = TGTKSignalFunc(@GTKRealizeCB))
      and (func_data = Pointer(LCLComponent))
      and ((flags and bmSignalAfter)=0) // test if not after
      then
        RealizeHandler := Handler;

      Handler := Next;
    end;
  end;

  // if we are here, then no handler was defined yet
  // -> register handler
  //if (Msg=LM_LBUTTONUP) then writeln('CONNECT ',ReqSignalMask,' Widget=',HexStr(Cardinal(AnObject),8));
  if csfAfter in SFlags then
    gtk_signal_connect_after(AnObject, ASignal,
                             TGTKSignalFunc(ACallBackProc),LCLComponent)
  else
    gtk_signal_connect      (AnObject, ASignal,
                             TGTKSignalFunc(ACallBackProc),LCLComponent);

  // update signal mask which will be set in the realize handler
  if (csfUpdateSignalMask in SFlags) and (ReqSignalMask <> 0) then begin
    MainWidget:=GetMainWidget(PGtkWidget(AnObject));
    if MainWidget=nil then
      MainWidget:=PGtkWidget(AnObject);
    WinWidgetInfo:=GetWidgetInfo(MainWidget,true);
    WinWidgetInfo^.EventMask:=WinWidgetInfo^.EventMask or ReqSignalMask;
  end;
  // -> register realize handler
  if (csfConnectRealize in SFlags)
  and (RealizeHandler = nil) and (RealizeID<>0) then begin
    //writeln('REALIZE CONNECT Widget=',HexStr(Cardinal(AnObject),8));
    gtk_signal_connect(AnObject, 'realize',
      TGTKSignalFunc(@GTKRealizeCB), LCLComponent);
    gtk_signal_connect_after(AnObject, 'realize',
      TGTKSignalFunc(@GTKRealizeAfterCB), LCLComponent);
  end;

  // update the DesignSignalMask
  if (DesignSignalType<>dstUnknown) then begin
    OldDesignMask:=GetDesignSignalMask(PGtkWidget(AnObject));
    if csfDesignOnly in SFlags then
      NewDesignMask:=OldDesignMask or DesignSignalMasks[DesignSignalType]
    else
      NewDesignMask:=OldDesignMask and not DesignSignalMasks[DesignSignalType];
    if OldDesignMask<>NewDesignMask then
      SetDesignSignalMask(PGtkWidget(AnObject),NewDesignMask);
  end;
end;

procedure ConnectSignal(const AnObject:gtk_Object; const ASignal: PChar;
  const ACallBackProc: Pointer; LCLComponent: TComponent;
  const ReqSignalMask: TGdkEventMask);
begin
  ConnectSignal(AnObject,ASignal,ACallBackProc,LCLComponent,ReqSignalMask,
                [csfConnectRealize,csfUpdateSignalMask]);
end;

procedure ConnectSignalAfter(const AnObject:gtk_Object; const ASignal: PChar;
  const ACallBackProc: Pointer; LCLComponent: TComponent;
  const ReqSignalMask: TGdkEventMask);
begin
  ConnectSignal(AnObject,ASignal,ACallBackProc,LCLComponent,ReqSignalMask,
                [csfConnectRealize,csfUpdateSignalMask,csfAfter]);
end;

procedure ConnectSignal(const AnObject:gtk_Object; const ASignal: PChar;
  const ACallBackProc: Pointer; LCLComponent: TComponent);
begin
  ConnectSignal(AnObject,ASignal,ACallBackProc,LCLComponent,0);
end;

procedure ConnectSignalAfter(const AnObject:gtk_Object; const ASignal: PChar;
  const ACallBackProc: Pointer; LCLComponent: TComponent);
begin
  ConnectSignalAfter(AnObject,ASignal,ACallBackProc,LCLComponent,0);
end;

{------------------------------------------------------------------------------
  procedure: ConnectInternalWidgetsSignals
  Params:  AWidget: PGtkWidget; AWinControl: TWinControl
  Returns: Nothing

  Connects hidden child widgets signals.
  Many gtk widgets create internally child widgets (e.g. scrollbars). In
  Design mode these widgets should not auto react themselves, but instead send
  messages to the lcl. Therefore these widgets are connected to special
  signal handlers.
  This procedure is called by the realize-after handler of all LCL widgets
  and each time the design mode of a LCL control changes.
 ------------------------------------------------------------------------------}
procedure ConnectInternalWidgetsSignals(AWidget: PGtkWidget;
  AWinControl: TWinControl);

  function WidgetIsInternal(TheWidget: PGtkWidget): boolean;
  begin
    Result:=(TheWidget<>nil)
      and (PGtkWidget(AWinControl.Handle)<>TheWidget)
      and (GetMainWidget(TheWidget)=nil);
  end;

  procedure ConnectSignals(TheWidget: PGtkWidget); forward;

  procedure ConnectChilds(TheWidget: PGtkWidget);
  var
    ContainerWidget: PGtkContainer;
    ScrolledWindow: PGtkScrolledWindow;
    BinWidget: PGtkBin;
    ChildEntry: PGSList;
    ChildWidget: PGtkWidget;
  begin
    if GtkWidgetIsA(TheWidget,GTK_CONTAINER_TYPE) then begin
      // this is a container widget -> connect all childs
      ContainerWidget:=PGtkContainer(TheWidget);
      ChildEntry:=ContainerWidget^.resize_widgets;
      while ChildEntry<>nil do begin
        ChildWidget:=PGtkWidget(ChildEntry^.Data);
        ConnectSignals(ChildWidget);
        ChildEntry:=ChildEntry^.Next;
      end;
    end;
    if GtkWidgetIsA(TheWidget,GTK_BIN_TYPE) then begin
      BinWidget:=PGtkBin(TheWidget);
      ConnectSignals(BinWidget^.child);
    end;
    if GtkWidgetIsA(TheWidget,GTK_SCROLLED_WINDOW_TYPE) then begin
      ScrolledWindow:=PGtkScrolledWindow(TheWidget);
      ConnectSignals(ScrolledWindow^.hscrollbar);
      ConnectSignals(ScrolledWindow^.vscrollbar);
    end;
  end;

  procedure ConnectSignals(TheWidget: PGtkWidget);
  var
    LCLObject, HiddenLCLObject: TObject;
    DesignSignalType: TDesignSignalType;
    DesignFlags: TConnectSignalFlags;
  begin
    if TheWidget=nil then exit;
    
    // check if widget belongs to another LCL object
    LCLObject:=GetLCLObject(TheWidget);
    HiddenLCLObject:=GetHiddenLCLObject(TheWidget);
    if (LCLObject<>nil) and (LCLObject<>AWinControl) then exit;
    if (HiddenLCLObject<>nil) and (HiddenLCLObject<>AWinControl) then exit;

    // connect signals needed for design mode:
    for DesignSignalType:=Low(TDesignSignalType) to High(TDesignSignalType) do
    begin
      if DesignSignalType=dstUnknown then continue;
      DesignFlags:=[csfDesignOnly];
      if DesignSignalAfter[DesignSignalType] then
        Include(DesignFlags,csfAfter);
      ConnectSignal(PGtkObject(TheWidget),DesignSignalNames[DesignSignalType],
                    DesignSignalFuncs[DesignSignalType],AWinControl,0,
                    DesignFlags);
    end;

    if WidgetIsInternal(TheWidget) then
      // mark widget as 'hidden' connected
      SetHiddenLCLObject(TheWidget,AWinControl);

    // connect recursively ...
    ConnectChilds(TheWidget);
  end;
  

begin
  if (AWinControl=nil) or (AWidget=nil)
  or (not (csDesigning in AWinControl.ComponentState)) then exit;
  ConnectSignals(AWidget);
end;

{-------------------------------------------------------------------------------
  procedure SetEventMaskForDesigning(MainWidget: PGtkWidget);
  
  Sets the event mask for all gtk internal gdkwindows to prevent gtk automatic
  reaction to mouse and keyboard events.
-------------------------------------------------------------------------------}
procedure SetEventMaskForDesigning(MainWidget: PGtkWidget);
var
  TheWinControl: TWinControl;
  PreparedWindows: TList;

  function WindowIsPrepared(AWindow: PGdkWindow): boolean;
  begin
    Result:=(AWindow<>nil)
           and (PreparedWindows<>nil) and (PreparedWindows.IndexOf(AWindow)>=0);
  end;

  procedure AddWindow(AWindow: PGdkWindow);
  begin
    if AWindow=nil then exit;
    if WindowIsPrepared(AWindow) then exit;
    if PreparedWindows=nil then
      PreparedWindows:=TList.Create;
    PreparedWindows.Add(AWindow);
  end;
  
  procedure AddWidgetWindow(AWidget: PGtkWidget);
  begin
    if (AWidget<>nil) then AddWindow(AWidget^.Window);
  end;
  
  procedure AddStandardWindows;
  var
    i: integer;
    AWinControl: TWinControl;
    ClientWidget, CoreChildWidget: PGtkWidget;
  begin
    AddWidgetWindow(MainWidget);
    ClientWidget:=GetFixedWidget(MainWidget);
    AddWidgetWindow(ClientWidget);
    CoreChildWidget:=GetCoreChildWidget(MainWidget);
    AddWidgetWindow(CoreChildWidget);
    for i:=0 to TheWinControl.ComponentCount-1 do begin
      if TheWinControl.Components[i] is TWinControl then begin
        AWinControl:=TWinControl(TheWinControl.Components[i]);
        if AWinControl.HandleAllocated then begin
          AddWidgetWindow(PGtkWidget(AWinControl.Handle));
        end;
      end;
    end;
  end;

  procedure PrepareWindow(AWindow: PGdkWindow);
  var
    NewEventMask: TGdkEventMask;
  begin
    if AWindow=nil then exit;
    if WindowIsPrepared(AWindow) then exit;
writeln('AAA1 SetEventMaskForDesigning ',TheWinControl.Name,':',TheWinControl.ClassName,
' Window=',HexStr(Cardinal(AWindow),8));
    NewEventMask:=gdk_window_get_events(AWindow)
                  -GDK_BUTTON_PRESS_MASK
                  -GDK_POINTER_MOTION_MASK
                  -GDK_BUTTON_RELEASE_MASK;
    gdk_window_set_events(AWindow,NewEventMask);
    AddWindow(AWindow);
  end;

  procedure PrepareChildWindows(AWindow: PGdkWindow);
  var
    ChildWindows, ListEntry: PGList;
  begin
    if AWindow=nil then exit;
    PrepareWindow(AWindow);

    ChildWindows:=gdk_window_get_children(AWindow);
    ListEntry:=ChildWindows;
    while ListEntry<>nil do begin
      PrepareChildWindows(PGdkWindow(ListEntry^.Data));
      ListEntry:=ListEntry^.Next;
    end;
    g_list_free(ChildWindows);
  end;

var
  AWidget: PGtkWidget;
begin
  TheWinControl:=TWinControl(GetLCLObject(MainWidget));
  AWidget:=GetMainWidget(MainWidget);
  if (AWidget<>nil) and (AWidget<>MainWidget) then
    raise Exception.Create(
      'SetEventMaskForDesigning Widget '+HexStr(Cardinal(MainWidget),8)
        +' is not the MainWidget '+HexStr(Cardinal(AWidget),8));
  if (TheWinControl=nil) then
    raise Exception.Create(
      'SetEventMaskForDesigning MainWidget has no LCL Object');
  {if not (csDesigning in TheWinControl.ComponentState) then
    raise Exception.Create(
      'SetEventMaskForDesigning MainWidget is not in Design mode');}
  PreparedWindows:=nil;
  AddStandardWindows;
  PrepareChildWindows(MainWidget^.Window);
  PreparedWindows.Free;
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

procedure Accelerate(const Widget : Pointer; const Msg : TLMShortCut);
var GDKModifier : integer;
    GDKKey : word;
begin
  if Msg.OldKey <> 0 then
    gtk_widget_remove_accelerators(Widget, 'activate_item', false);
  { Map the shift states }
  GDKModifier:= 0;
  if ssShift in Msg.NewModifier then GDKModifier:= GDK_SHIFT_MASK;
  if ssAlt in Msg.NewModifier then GDKModifier:= GDKModifier + GDK_MOD1_MASK;
  if ssCtrl in Msg.NewModifier then GDKModifier:= GDKModifier + GDK_CONTROL_MASK;
  GDKKey:= VK2GDK(Msg.NewKey);
  { Set the accelerator }
  gtk_widget_add_accelerator(Widget, 'activate_item',
    gtk_accel_group_get_default(), GDKKey, GDKModifier, GTK_ACCEL_VISIBLE);
end;

{------------------------------------------------------------------------------
  procedure GetGdkPixmapFromGraphic(LCLGraphic: TGraphic;
    var IconImg, IconMask: PGdkPixmap; var Width, Height: integer);

  Extracts some information from the Handle of a TGraphic
 ------------------------------------------------------------------------------}
procedure GetGdkPixmapFromGraphic(LCLGraphic: TGraphic;
  var IconImg, IconMask: PGdkPixmap; var Width, Height: integer);
var
  GDIObject: PGdiObject;
begin
  IconImg:=nil;
  IconMask:=nil;
  Width:=0;
  Height:=0;
  if (LCLGraphic=nil) then exit;
  if LCLGraphic is TBitmap then
    GDIObject:=PgdiObject(TBitmap(LCLGraphic).Handle)
  else
    GDIObject:=nil;
  if GDIObject<>nil then begin
    IconImg:=GDIObject^.GDIBitmapObject;
    IconMask:=GDIObject^.GDIBitmapMaskObject;
    if IconImg<>nil then
      gdk_window_get_size (IconImg, @Width, @Height);
  end;
end;

{------------------------------------------------------------------------------
  procedure GetGdkPixmapFromGraphic(LCLGraphic: TGraphic;
    var IconImg, IconMask: PGdkPixmap; var Width, Height: integer);

  Extracts some information from the Handle of the icon of a TMenuItem
 ------------------------------------------------------------------------------}
procedure GetGdkPixmapFromMenuItem(LCLMenuItem: TMenuItem;
  var IconImg, IconMask: PGdkPixmap; var Width, Height: integer);
begin
  IconImg:=nil;
  IconMask:=nil;
  Width:=0;
  Height:=0;
  if LCLMenuItem=nil then exit;
  if LCLMenuItem.Graphic<>nil then
    GetGdkPixmapFromGraphic(LCLMenuItem.Graphic,IconImg,IconMask,Width,Height);
end;

{------------------------------------------------------------------------------
  function MENU_ITEM_CLASS(widget: PGtkWidget): PGtkMenuItemClass;

  Returns the gtk klass of a menuitem widget.
 ------------------------------------------------------------------------------}
function MENU_ITEM_CLASS(widget: PGtkWidget): PGtkMenuItemClass;
begin
  Result:=GTK_MENU_ITEM_CLASS(PGtkObject(widget)^.klass);
end;

{------------------------------------------------------------------------------
  function CHECK_MENU_ITEM_CLASS(widget: PGtkWidget): PGtkCheckMenuItemClass;

  Returns the gtk klass of a checkmenuitem widget.
 ------------------------------------------------------------------------------}
function CHECK_MENU_ITEM_CLASS(widget: PGtkWidget): PGtkCheckMenuItemClass;
begin
  Result:=GTK_CHECK_MENU_ITEM_CLASS(PGtkObject(widget)^.klass);
end;

{------------------------------------------------------------------------------
  function GetRadioMenuItemGroup(LCLMenuItem: TMenuItem): PGSList;

  Returns the radio group list with the GroupIndex of the MenuItem
 ------------------------------------------------------------------------------}
function GetRadioMenuItemGroup(LCLMenuItem: TMenuItem): PGSList;
var
  ParentMenuItem: TMenuItem;
  i: integer;
begin
  Result:=nil;
  if (LCLMenuItem=nil) or (LCLMenuItem.GroupIndex=0) then exit;
  ParentMenuItem:=LCLMenuItem.Parent;
  if ParentMenuItem=nil then exit;
  for i:=0 to ParentMenuItem.Count-1 do begin
    if ParentMenuItem[i].RadioItem
    and (ParentMenuItem[i].GroupIndex=LCLMenuItem.GroupIndex)
    and (ParentMenuItem[i]<>LCLMenuItem)
    and ParentMenuItem[i].HandleAllocated
    and GtkWidgetIsA(Pointer(ParentMenuItem[i].Handle),
                      GTK_RADIO_MENU_ITEM_TYPE) then
    begin
      Result:=gtk_radio_menu_item_group(
                                 GTK_RADIO_MENU_ITEM(ParentMenuItem[i].Handle));
      exit;
    end;
  end;
end;

{------------------------------------------------------------------------------
  function GetRadioMenuItemGroup(MenuItem: PGtkRadioMenuItem): PGSList;

  Returns the radio group list with the GroupIndex of the MenuItem
 ------------------------------------------------------------------------------}
function GetRadioMenuItemGroup(MenuItem: PGtkRadioMenuItem): PGSList;
begin
  if MenuItem=nil then
    Result:=nil
  else
    Result:=GetRadioMenuItemGroup(TMenuItem(GetLCLObject(MenuItem)));
end;

{------------------------------------------------------------------------------
  procedure UpdateRadioGroupChecks(RadioGroup: PGSList);

  Set 'checked' for all menuitems in the group
 ------------------------------------------------------------------------------}
procedure UpdateRadioGroupChecks(RadioGroup: PGSList);
var
  CurListItem: PGSList;
  MenuItem: PGtkMenuItem;
  LCLMenuItem: TMenuItem;
begin
  if RadioGroup=nil then exit;
  CurListItem:=RadioGroup;
  // set active radiomenuitem
  while CurListItem<>nil do begin
    MenuItem:=PGtkMenuItem(CurListItem^.Data);
    if MenuItem<>nil then begin
      LCLMenuItem:=TMenuItem(GetLCLObject(MenuItem));
      if (LCLMenuItem<>nil) and LCLMenuItem.Checked then begin
        gtk_check_menu_item_set_active(PGtkCheckMenuItem(MenuItem),
                                       LCLMenuItem.Checked);
      end;
    end;
    CurListItem:=CurListItem^.Next;
  end;
  CurListItem:=RadioGroup;
  // deactivate the other radiomenuitems
  while CurListItem<>nil do begin
    MenuItem:=PGtkMenuItem(CurListItem^.Data);
    if MenuItem<>nil then begin
      LCLMenuItem:=TMenuItem(GetLCLObject(MenuItem));
      if (LCLMenuItem<>nil) then begin
        gtk_check_menu_item_set_active(PGtkCheckMenuItem(MenuItem),
                                       LCLMenuItem.Checked);
      end;
    end;
    CurListItem:=CurListItem^.Next;
  end;
end;

{------------------------------------------------------------------------------
  procedure DrawMenuItemIcon(MenuItem: PGtkCheckMenuItem;
    area: PGdkRectangle); cdecl;

  Handler for drawing the icon of a menuitem.
 ------------------------------------------------------------------------------}
procedure DrawMenuItemIcon(MenuItem: PGtkCheckMenuItem;
  Area: PGdkRectangle); cdecl;
var
  Widget: PGtkWidget;
  Container: PgtkContainer;
  ALeft, ATop, BorderWidth: gint;
  LCLMenuItem: TMenuItem;
  IconImg, IconMask: PGdkPixmap;
  IconWidth, IconHeight: integer;
begin
  if (MenuItem=nil) then exit;
  if not (GTK_WIDGET_DRAWABLE (PGtkWidget(MenuItem))) then exit;

  // get icon
  LCLMenuItem:=TMenuItem(GetLCLObject(MenuItem));
  GetGdkPixmapFromMenuItem(LCLMenuItem,IconImg,IconMask,IconWidth,IconHeight);
  if IconImg=nil then begin
    // call default draw function
    OldCheckMenuItemDrawProc(MenuItem,Area);
    exit;
  end;

  // calculate left and top
  Widget := PGtkWidget (MenuItem);
  Container := GTK_CONTAINER (MenuItem);
  BorderWidth := Container^.flag0 and bm_TGtkContainer_border_width;
  ALeft := (BorderWidth + PGtkStyle(Widget^.theStyle)^.klass^.xthickness + 2)
           +((PGtkMenuItem(MenuItem)^.toggle_size-IconWidth) div 2);
  ATop := (Widget^.Allocation.Height - IconHeight) div 2;

  // draw icon
  gdk_gc_set_clip_mask(pGtkStyle(Widget^.theStyle)^.Black_gc, IconMask);
  gdk_gc_set_clip_origin(pGtkStyle(Widget^.theStyle)^.Black_gc,ALeft,ATop);
  gdk_draw_pixmap(Widget^.Window,pGtkStyle(Widget^.theStyle)^.Black_gc,
     IconImg,0,0,ALeft,ATop,-1,-1);
  gdk_gc_set_clip_mask(pGtkStyle(Widget^.theStyle)^.Black_gc, nil);
end;

{------------------------------------------------------------------------------
  procedure MenuSizeRequest(widget:PGtkWidget;
    requisition:PGtkRequisition); cdecl;

  SizeAllocate Handler for check menuitem widgets.
 ------------------------------------------------------------------------------}
procedure MenuSizeRequest(widget:PGtkWidget; requisition:PGtkRequisition); cdecl;
var
  CurToggleSize, MaxToggleSize: integer;
  MenuShell: PGtkMenuShell;
  ListItem: PGList;
  MenuItem, CheckMenuItem: PGtkMenuItem;
  LCLMenuItem: TMenuItem;
  IconImg, IconMask: PGdkPixmap;
  Width, Height: integer;
begin
  MaxToggleSize:=0;
  MenuShell:=GTK_MENU_SHELL(widget);
  ListItem:=MenuShell^.Children;
  CheckMenuItem:=nil;
  while ListItem<>nil do begin
    MenuItem:=PGtkMenuItem(ListItem^.Data);
    if GTK_IS_CHECK_MENU_ITEM(PGtkWidget(MenuItem)) then begin
      CheckMenuItem:=MenuItem;
      CurToggleSize:=OldCheckMenuItemToggleSize;
      LCLMenuItem:=TMenuItem(GetLCLObject(MenuItem));
      if LCLMenuItem<>nil then begin
        GetGdkPixmapFromMenuItem(LCLMenuItem,IconImg,IconMask,Width,Height);
        if IconImg<>nil then begin
          if CurToggleSize<Width then
            CurToggleSize:=Width;
        end;
      end;
      if MaxToggleSize<CurToggleSize then
        MaxToggleSize:=CurToggleSize;
    end;
    ListItem:=ListItem^.Next;
  end;
  if CheckMenuItem<>nil then
    MENU_ITEM_CLASS(PGtkWidget(CheckMenuItem))^.toggle_size:=MaxToggleSize;
  OldMenuSizeRequestProc(Widget,requisition);
end;

{------------------------------------------------------------------------------
  procedure SetMenuItemLabelText(LCLMenuItem: TMenuItem;
    MenuItemWidget: PGtkWidget);

  Sets the caption of a menuitem
 ------------------------------------------------------------------------------}
procedure SetMenuItemLabelText(LCLMenuItem: TMenuItem;
  MenuItemWidget: PGtkWidget);
var
  ShortCutPos: integer;
  s: string;
  LabelWidget: PGtkLabel;
begin
  if (MenuItemWidget=nil) or (LCLMenuItem=nil) then exit;
  LabelWidget:=gtk_object_get_data(PGtkObject(MenuItemWidget), 'LCLLabel');
  if LabelWidget=nil then exit;
  //Check for a shortcut key
  s:=LCLMenuItem.Caption;
  ShortCutPos := pos('&', s);
  if ShortCutPos <> 0 then begin
    s[ShortCutPos] := '_';
    SetAccelKey(MenuItemWidget,gtk_label_parse_uline(LabelWidget,PChar(s)));
  end
  else begin
    gtk_label_set_text(LabelWidget,PChar(s));
  end;
end;

{------------------------------------------------------------------------------
  procedure CreateInnerMenuItem(LCLMenuItem: TMenuItem;
    MenuItemWidget: PGtkWidget);

  Creates the inner widgets of a menuitem widget.
 ------------------------------------------------------------------------------}
procedure CreateInnerMenuItem(LCLMenuItem: TMenuItem;
  MenuItemWidget: PGtkWidget);
var
  HBoxWidget: PGtkWidget;
  LabelWidget: PGtkAccelLabel;

  procedure CreateIcon;
  var
    IconImg, IconMask: PGdkPixmap;
    IconWidth, IconHeight: integer;
    MinHeightWidget: PGtkWidget;
  begin
    // the icon will be painted instead of the toggle
    // of a normal gtkcheckmenuitem

    // get the icon
    GetGdkPixmapFromMenuItem(LCLMenuItem,IconImg,IconMask,IconWidth,IconHeight);
    if IconImg<>nil then begin
      // set the toggle width
      GTK_MENU_ITEM(MenuItemWidget)^.toggle_size:=IconWidth;
      GTK_MENU_ITEM(MenuItemWidget)^.flag0:=
        PGtkMenuItem(MenuItemWidget)^.flag0
          or bm_show_toggle_indicator;

      // set our own draw handler
      if OldCheckMenuItemDrawProc=nil then
        OldCheckMenuItemDrawProc:=
          CHECK_MENU_ITEM_CLASS(MenuItemWidget)^.draw_indicator;
      CHECK_MENU_ITEM_CLASS(MenuItemWidget)^.draw_indicator:=@DrawMenuItemIcon;

      // add a dummy widget for the icon height
      MinHeightWidget:=gtk_label_new('');
      gtk_widget_show(MinHeightWidget);
      gtk_widget_set_usize(MinHeightWidget,1,IconHeight);
      gtk_box_pack_start(GTK_BOX(HBoxWidget),MinHeightWidget,false,false,0);
    end else
      MinHeightWidget:=nil;
    gtk_object_set_data(PGtkObject(MenuItemWidget),
                        'LCLMinHeight',MinHeightWidget);
  end;

  procedure CreateLabel;
  begin
    // create a label for the Caption
    LabelWidget:=PGtkAccelLabel(gtk_accel_label_new(''));
    gtk_misc_set_alignment(GTK_MISC (LabelWidget), 0.0, 0.5);
    gtk_object_set_data(PGtkObject(MenuItemWidget), 'LCLLabel', LabelWidget);
    gtk_container_add(GTK_CONTAINER(HBoxWidget),PgtkWidget(LabelWidget));
    SetMenuItemLabelText(LCLMenuItem,MenuItemWidget);
    gtk_accel_label_set_accel_widget(GTK_ACCEL_LABEL(LabelWidget),MenuItemWidget);
    gtk_widget_show(PGtkWidget(LabelWidget));
  end;

begin
  HBoxWidget:=gtk_object_get_data(PGtkObject(MenuItemWidget), 'LCLHBox');
  if HBoxWidget=nil then begin
    // create inner widgets
    if LCLMenuItem.Caption='-' then begin
      // a separator is an empty gtkmenuitem
      exit;
    end;
    HBoxWidget:=gtk_hbox_new(false,0);
    gtk_object_set_data(PGtkObject(MenuItemWidget), 'LCLHBox', HBoxWidget);
    CreateIcon;
    CreateLabel;
    gtk_container_add(GTK_CONTAINER(MenuItemWidget),HBoxWidget);
    gtk_widget_show(HBoxWidget);
  end else begin
    // there are already inner widgets
    if LCLMenuItem.Caption='-' then begin
      // a separator is an empty gtkmenuitem -> delete the inner widgets
      DestroyWidget(HBoxWidget);
      gtk_object_set_data(PGtkObject(MenuItemWidget), 'LCLHBox', nil);
    end else begin
      // just update the content
      SetMenuItemLabelText(LCLMenuItem,MenuItemWidget);
    end;
  end;
end;

{------------------------------------------------------------------------------
  function CreateMenuItem(LCLMenuItem: TMenuItem): Pointer;

  Creates a new menuitem widget.
 ------------------------------------------------------------------------------}
function CreateMenuItem(LCLMenuItem: TMenuItem): Pointer;
var
  MenuItemWidget: PGtkWidget;
begin
  // create the menuitem widget (normal, check or radio)
  if LCLMenuItem.Caption='-' then
    // create separator
    MenuItemWidget:=gtk_menu_item_new
  else if LCLMenuItem.RadioItem and not LCLMenuItem.HasIcon then begin
    MenuItemWidget:=gtk_radio_menu_item_new(nil);
  end else if LCLMenuItem.IsCheckItem or LCLMenuItem.HasIcon then begin
    MenuItemWidget:=gtk_check_menu_item_new;
  end else
    MenuItemWidget:=gtk_menu_item_new;
  if GtkWidgetIsA(MenuItemWidget,GTK_CHECK_MENU_ITEM_TYPE) then begin
    // set 'ShowAlwaysCheckable'
    gtk_check_menu_item_set_show_toggle(PGtkCheckMenuItem(MenuItemWidget),
      LCLMenuItem.ShowAlwaysCheckable);
    // set 'Checked'
    gtk_check_menu_item_set_active(PGtkCheckMenuItem(MenuItemWidget),
      LCLMenuItem.Checked);
    if (OldCheckMenuItemToggleSize=0) then
      OldCheckMenuItemToggleSize:=MENU_ITEM_CLASS(MenuItemWidget)^.toggle_size;
  end;

  // set attributes (enabled and rightjustify)
  gtk_widget_set_sensitive(MenuItemWidget, LCLMenuItem.Enabled);
  if LCLMenuItem.RightJustify then
    gtk_menu_item_right_justify(PGtkMenuItem(MenuItemWidget));

  // create the hbox containing the label and the control
  CreateInnerMenuItem(LCLMenuItem,MenuItemWidget);

  gtk_widget_show(MenuItemWidget);
  Result:=MenuItemWidget;
end;

{------------------------------------------------------------------------------
  TgtkObject SetSizeNotification
  Params: Widget: PGtkWidget  A widget that is the handle of a lcl control.

  When the gtk sends a size signal, it is not send directly to the LCL. All gtk
  size/move messages are collected and only the last one for each widget is sent
  to the LCL.
  This is neccessary, because the gtk sends size messages several times and
  it replays resizes. Since the LCL reacts to every size notification and
  resizes child controls, this results in a perpetuum mobile.
 ------------------------------------------------------------------------------}
procedure SaveSizeNotification(Widget: PGtkWidget);
{$IFDEF VerboseSizeMsg}
var
  LCLControl: TWinControl;
{$ENDIF}
begin
  {$IFDEF VerboseSizeMsg}
  write('SaveSizeNotification Widget=',HexStr(Cardinal(Widget),8));
  LCLControl:=TWinControl(GetLCLObject(Widget));
  if (LCLControl<>nil) then begin
    if LCLControl is TWinControl then
      writeln(' ',LCLControl.Name,':',LCLControl.ClassName)
    else
      writeln(' ERROR: ',LCLControl.ClassName);
  end else begin
    writeln(' ERROR: LCLControl=nil');
  end;
  {$ENDIF}
  if not FWidgetsResized.Contains(Widget) then
    FWidgetsResized.Add(Widget);
end;

{------------------------------------------------------------------------------
  TgtkObject SaveClientSizeNotification
  Params: FixWidget: PGtkWidget  A widget that is the fixed widget
                                 of a lcl control.

  When the gtk sends a size signal, it is not send directly to the LCL. All gtk
  size/move messages are collected and only the last one for each widget is sent
  to the LCL.
  This is neccessary, because the gtk sends size messages several times and
  it replays resizes. Since the LCL reacts to every size notification and
  resizes child controls, this results in a perpetuum mobile.
 ------------------------------------------------------------------------------}
procedure SaveClientSizeNotification(FixWidget: PGtkWidget);
{$IFDEF VerboseSizeMsg}
var
  LCLControl: TWinControl;
  MainWidget: PGtkWidget;
{$ENDIF}
begin
  {$IFDEF VerboseSizeMsg}
  MainWidget:=GetMainWidget(FixWidget);
  write('SaveClientSizeNotification',
    ' FixWidget=',HexStr(Cardinal(FixWidget),8),
    ' MainWIdget=',HexStr(Cardinal(MainWidget),8));
  LCLControl:=TWinControl(GetLCLObject(MainWidget));
  if (LCLControl<>nil) then begin
    if LCLControl is TWinControl then
      writeln(' ',LCLControl.Name,':',LCLControl.ClassName)
    else
      writeln(' ERROR: ',LCLControl.ClassName);
  end else begin
    writeln(' ERROR: LCLControl=nil');
  end;
  {$ENDIF}
  if not FFixWidgetsResized.Contains(FixWidget) then
    FFixWidgetsResized.Add(FixWidget);
end;

{-------------------------------------------------------------------------------
  CreateTopologicalSortedWidgets
  Params: HashArray: TDynHashArray  of PGtkWidget
  
  Creates a topologically sorted TList of PGtkWidget.
-------------------------------------------------------------------------------}
function CreateTopologicalSortedWidgets(HashArray: TDynHashArray): TList;
type
  PTopologicalEntry = ^TTopologicalEntry;
  TTopologicalEntry = record
      Widget: PGtkWidget;
      ParentLevel: integer;
    end;
    
  function GetParentLevel(AControl: TControl): integer;
  // nil has lvl -1
  // a control without parent has lvl 0
  begin
    Result:=-1;
    while AControl<>nil do begin
      inc(Result);
      AControl:=AControl.Parent;
    end;
  end;
  
var
  TopologicalList: PTopologicalEntry;
  HashItem: PDynHashArrayItem;
  i, Lvl, MaxLevel: integer;
  LCLControl: TControl;
  LevelCounts: PInteger;
begin
  //writeln(' KKK0');
  Result:=TList.Create;
  if HashArray.Count=0 then exit;
  
  // put all widgets into an array and calculate their parent levels
  GetMem(TopologicalList,SizeOf(TTopologicalEntry)*HashArray.Count);
  HashItem:=HashArray.FirstHashItem;
  i:=0;
  MaxLevel:=0;
  //writeln(' KKK1 HashArray.Count=',HashArray.Count);
  while HashItem<>nil do begin
    TopologicalList[i].Widget:=HashItem^.Item;
    //writeln(' KKK21 i=',i,' Widget=',HexStr(Cardinal(TopologicalList[i].Widget),8));
    LCLControl:=TControl(GetLCLObject(TopologicalList[i].Widget));
    if (LCLControl=nil) or (not (LCLControl is TControl)) then
      raise Exception.Create('CreateTopologicalSortedWidgets: '
                             +'Widget without LCL control');
    Lvl:=GetParentLevel(LCLControl);
    TopologicalList[i].ParentLevel:=Lvl;
    if MaxLevel<Lvl then
      MaxLevel:=Lvl;
    //writeln(' KKK2 i=',i,' Lvl=',Lvl,' MaxLvl=',MaxLevel,' LCLControl=',LCLControl.Name,':',LCLControl.ClassName);
    inc(i);
    HashItem:=HashItem^.Next;
  end;
  inc(MaxLevel);
  
  // bucket sort the widgets
  
  // count each number of levels (= bucketsizes)
  GetMem(LevelCounts,SizeOf(Integer)*MaxLevel);
  FillChar(LevelCounts^,SizeOf(Integer)*MaxLevel,0);
  for i:=0 to HashArray.Count-1 do
    inc(LevelCounts[TopologicalList[i].ParentLevel]);

  // calculate bucketends
  for i:=1 to MaxLevel-1 do
    inc(LevelCounts[i],LevelCounts[i-1]);

  // bucket sort the widgets in Result
  Result.Count:=HashArray.Count;
  for i:=0 to HashArray.Count-1 do
    Result[i]:=nil;
  for i:=0 to HashArray.Count-1 do begin
    Lvl:=TopologicalList[i].ParentLevel;
    dec(LevelCounts[Lvl]);
    //writeln(' KKK5 i=',i,' Lvl=',Lvl,' LevelCounts[Lvl]=',LevelCounts[Lvl],
    //  ' Widget=',HexStr(Cardinal(TopologicalList[i].Widget),8));
    Result[LevelCounts[Lvl]]:=TopologicalList[i].Widget;
  end;
  
  FreeMem(LevelCounts);
  FreeMem(TopologicalList);
end;



Procedure ReportNotObsolete(const Texts : String);
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
  Result := ((Value.Blue shr 8) shl 16) + ((Value.Green shr 8) shl 8)
           + (Value.Red shr 8);
end;

function TColortoTGDKColor(value : TColor) : TGDKColor;
var
   newColor : TGDKColor;
   num : LongInt;                                          // example assume value := $00FF2510

begin
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

{------------------------------------------------------------------------------
  Function: UpdateSysColorMap
  Params:  none
  Returns: none

  Reads the system colors.
 ------------------------------------------------------------------------------}
procedure UpdateSysColorMap(Widget: PGtkWidget);
{ $DEFINE VerboseUpdateSysColorMap}
{$IFDEF VerboseUpdateSysColorMap}
  function GdkColorAsString(c: TgdkColor): string;
  begin
    Result:='LCL='+HexStr(Cardinal(TGDKColorToTColor(c)),8)
             +' Pixel='+HexStr(Cardinal(c.Pixel),8)
             +' Red='+HexStr(Cardinal(c.Red),8)
             +' Green='+HexStr(Cardinal(c.Green),8)
             +' Blue='+HexStr(Cardinal(c.Blue),8)
             ;
  end;
{$ENDIF}
var
  MainStyle: PGtkStyle;
begin
  if Widget=nil then exit;
  gtk_widget_set_rc_style(Widget);
  MainStyle:=pGtkStyle(Widget^.TheStyle);
  if MainStyle=nil then exit;
  with MainStyle^ do begin
  
    {$IFDEF VerboseUpdateSysColorMap}
    if rc_style<>nil then begin
      with rc_style^ do begin
        writeln('rc_style:');
        writeln(' FG GTK_STATE_NORMAL ',GdkColorAsString(fg[GTK_STATE_NORMAL]));
        writeln(' FG GTK_STATE_ACTIVE ',GdkColorAsString(fg[GTK_STATE_ACTIVE]));
        writeln(' FG GTK_STATE_PRELIGHT ',GdkColorAsString(fg[GTK_STATE_PRELIGHT]));
        writeln(' FG GTK_STATE_SELECTED ',GdkColorAsString(fg[GTK_STATE_SELECTED]));
        writeln(' FG GTK_STATE_INSENSITIVE ',GdkColorAsString(fg[GTK_STATE_INSENSITIVE]));
        writeln('');
        writeln(' BG GTK_STATE_NORMAL ',GdkColorAsString(bg[GTK_STATE_NORMAL]));
        writeln(' BG GTK_STATE_ACTIVE ',GdkColorAsString(bg[GTK_STATE_ACTIVE]));
        writeln(' BG GTK_STATE_PRELIGHT ',GdkColorAsString(bg[GTK_STATE_PRELIGHT]));
        writeln(' BG GTK_STATE_SELECTED ',GdkColorAsString(bg[GTK_STATE_SELECTED]));
        writeln(' BG GTK_STATE_INSENSITIVE ',GdkColorAsString(bg[GTK_STATE_INSENSITIVE]));
        writeln('');
        writeln(' TEXT GTK_STATE_NORMAL ',GdkColorAsString(text[GTK_STATE_NORMAL]));
        writeln(' TEXT GTK_STATE_ACTIVE ',GdkColorAsString(text[GTK_STATE_ACTIVE]));
        writeln(' TEXT GTK_STATE_PRELIGHT ',GdkColorAsString(text[GTK_STATE_PRELIGHT]));
        writeln(' TEXT GTK_STATE_SELECTED ',GdkColorAsString(text[GTK_STATE_SELECTED]));
        writeln(' TEXT GTK_STATE_INSENSITIVE ',GdkColorAsString(text[GTK_STATE_INSENSITIVE]));
        writeln('');
      end;
    end;

    writeln('MainStyle:');
    writeln(' FG GTK_STATE_NORMAL ',GdkColorAsString(fg[GTK_STATE_NORMAL]));
    writeln(' FG GTK_STATE_ACTIVE ',GdkColorAsString(fg[GTK_STATE_ACTIVE]));
    writeln(' FG GTK_STATE_PRELIGHT ',GdkColorAsString(fg[GTK_STATE_PRELIGHT]));
    writeln(' FG GTK_STATE_SELECTED ',GdkColorAsString(fg[GTK_STATE_SELECTED]));
    writeln(' FG GTK_STATE_INSENSITIVE ',GdkColorAsString(fg[GTK_STATE_INSENSITIVE]));
    writeln('');
    writeln(' BG GTK_STATE_NORMAL ',GdkColorAsString(bg[GTK_STATE_NORMAL]));
    writeln(' BG GTK_STATE_ACTIVE ',GdkColorAsString(bg[GTK_STATE_ACTIVE]));
    writeln(' BG GTK_STATE_PRELIGHT ',GdkColorAsString(bg[GTK_STATE_PRELIGHT]));
    writeln(' BG GTK_STATE_SELECTED ',GdkColorAsString(bg[GTK_STATE_SELECTED]));
    writeln(' BG GTK_STATE_INSENSITIVE ',GdkColorAsString(bg[GTK_STATE_INSENSITIVE]));
    writeln('');
    writeln(' TEXT GTK_STATE_NORMAL ',GdkColorAsString(text[GTK_STATE_NORMAL]));
    writeln(' TEXT GTK_STATE_ACTIVE ',GdkColorAsString(text[GTK_STATE_ACTIVE]));
    writeln(' TEXT GTK_STATE_PRELIGHT ',GdkColorAsString(text[GTK_STATE_PRELIGHT]));
    writeln(' TEXT GTK_STATE_SELECTED ',GdkColorAsString(text[GTK_STATE_SELECTED]));
    writeln(' TEXT GTK_STATE_INSENSITIVE ',GdkColorAsString(text[GTK_STATE_INSENSITIVE]));
    writeln('');
    writeln(' LIGHT GTK_STATE_NORMAL ',GdkColorAsString(light[GTK_STATE_NORMAL]));
    writeln(' LIGHT GTK_STATE_ACTIVE ',GdkColorAsString(light[GTK_STATE_ACTIVE]));
    writeln(' LIGHT GTK_STATE_PRELIGHT ',GdkColorAsString(light[GTK_STATE_PRELIGHT]));
    writeln(' LIGHT GTK_STATE_SELECTED ',GdkColorAsString(light[GTK_STATE_SELECTED]));
    writeln(' LIGHT GTK_STATE_INSENSITIVE ',GdkColorAsString(light[GTK_STATE_INSENSITIVE]));
    writeln('');
    writeln(' DARK GTK_STATE_NORMAL ',GdkColorAsString(dark[GTK_STATE_NORMAL]));
    writeln(' DARK GTK_STATE_ACTIVE ',GdkColorAsString(dark[GTK_STATE_ACTIVE]));
    writeln(' DARK GTK_STATE_PRELIGHT ',GdkColorAsString(dark[GTK_STATE_PRELIGHT]));
    writeln(' DARK GTK_STATE_SELECTED ',GdkColorAsString(dark[GTK_STATE_SELECTED]));
    writeln(' DARK GTK_STATE_INSENSITIVE ',GdkColorAsString(dark[GTK_STATE_INSENSITIVE]));
    writeln('');
    writeln(' MID GTK_STATE_NORMAL ',GdkColorAsString(mid[GTK_STATE_NORMAL]));
    writeln(' MID GTK_STATE_ACTIVE ',GdkColorAsString(mid[GTK_STATE_ACTIVE]));
    writeln(' MID GTK_STATE_PRELIGHT ',GdkColorAsString(mid[GTK_STATE_PRELIGHT]));
    writeln(' MID GTK_STATE_SELECTED ',GdkColorAsString(mid[GTK_STATE_SELECTED]));
    writeln(' MID GTK_STATE_INSENSITIVE ',GdkColorAsString(mid[GTK_STATE_INSENSITIVE]));
    writeln('');
    writeln(' BASE GTK_STATE_NORMAL ',GdkColorAsString(base[GTK_STATE_NORMAL]));
    writeln(' BASE GTK_STATE_ACTIVE ',GdkColorAsString(base[GTK_STATE_ACTIVE]));
    writeln(' BASE GTK_STATE_PRELIGHT ',GdkColorAsString(base[GTK_STATE_PRELIGHT]));
    writeln(' BASE GTK_STATE_SELECTED ',GdkColorAsString(base[GTK_STATE_SELECTED]));
    writeln(' BASE GTK_STATE_INSENSITIVE ',GdkColorAsString(base[GTK_STATE_INSENSITIVE]));
    writeln('');
    writeln(' BLACK ',GdkColorAsString(black));
    writeln(' WHITE ',GdkColorAsString(white));
    {$ENDIF}
    
    {$IFDEF NewSysColors}
    SysColorMap[COLOR_SCROLLBAR] := TGDKColorToTColor(bg[GTK_STATE_ACTIVE]);
    SysColorMap[COLOR_BACKGROUND] := TGDKColorToTColor(bg[GTK_STATE_PRELIGHT]);
    SysColorMap[COLOR_ACTIVECAPTION] := TGDKColorToTColor(text[GTK_STATE_ACTIVE]);
    SysColorMap[COLOR_INACTIVECAPTION] := TGDKColorToTColor(text[GTK_STATE_INSENSITIVE]);
    SysColorMap[COLOR_MENU] := TGDKColorToTColor(bg[GTK_STATE_NORMAL]);
    SysColorMap[COLOR_WINDOW] := TGDKColorToTColor(white);
    SysColorMap[COLOR_WINDOWFRAME] := TGDKColorToTColor(black);
    SysColorMap[COLOR_MENUTEXT] := TGDKColorToTColor(text[GTK_STATE_NORMAL]);
    SysColorMap[COLOR_WINDOWTEXT] := TGDKColorToTColor(text[GTK_STATE_NORMAL]);
    SysColorMap[COLOR_CAPTIONTEXT] := TGDKColorToTColor(text[GTK_STATE_SELECTED]);
    SysColorMap[COLOR_ACTIVEBORDER] := TGDKColorToTColor(bg[GTK_STATE_ACTIVE]);
    SysColorMap[COLOR_INACTIVEBORDER] := TGDKColorToTColor(bg[GTK_STATE_NORMAL]);
    SysColorMap[COLOR_APPWORKSPACE] := TGDKColorToTColor(bg[GTK_STATE_NORMAL]);
    SysColorMap[COLOR_HIGHLIGHT] := TGDKColorToTColor(bg[GTK_STATE_SELECTED]);
    SysColorMap[COLOR_HIGHLIGHTTEXT] := TGDKColorToTColor(text[GTK_STATE_SELECTED]);
    SysColorMap[COLOR_BTNFACE] := TGDKColorToTColor(bg[GTK_STATE_NORMAL]);
    SysColorMap[COLOR_BTNSHADOW] := TGDKColorToTColor(fg[GTK_STATE_INSENSITIVE]);
    SysColorMap[COLOR_GRAYTEXT] := TGDKColorToTColor(text[GTK_STATE_INSENSITIVE]);
    SysColorMap[COLOR_BTNTEXT] := TGDKColorToTColor(text[GTK_STATE_NORMAL]);
    SysColorMap[COLOR_INACTIVECAPTIONTEXT] := TGDKColorToTColor(text[GTK_STATE_INSENSITIVE]);
    SysColorMap[COLOR_BTNHIGHLIGHT] := TGDKColorToTColor(fg[GTK_STATE_SELECTED]);
    SysColorMap[COLOR_3DDKSHADOW] := TGDKColorToTColor(black);
    SysColorMap[COLOR_3DLIGHT] := TGDKColorToTColor(fg[GTK_STATE_SELECTED]);
    SysColorMap[COLOR_INFOTEXT] := TGDKColorToTColor(text[GTK_STATE_NORMAL]);
    SysColorMap[COLOR_INFOBK] := TGDKColorToTColor(bg[GTK_STATE_PRELIGHT]);
    SysColorMap[COLOR_HOTLIGHT] := TGDKColorToTColor(fg[GTK_STATE_NORMAL]);
    SysColorMap[COLOR_GRADIENTACTIVECAPTION] := TGDKColorToTColor(bg[GTK_STATE_NORMAL]);
    SysColorMap[COLOR_GRADIENTINACTIVECAPTION] := TGDKColorToTColor(bg[GTK_STATE_NORMAL]);
    {$ENDIF}
  end;
(*
    $C0C0C0,     {COLOR_SCROLLBAR}
    $808000,     {COLOR_BACKGROUND}
    $800000,     {COLOR_ACTIVECAPTION}
    $808080,     {COLOR_INACTIVECAPTION}
    $C0C0C0,     {COLOR_MENU}
    $FFFFFF,     {COLOR_WINDOW}
    $000000,     {COLOR_WINDOWFRAME}
    $000000,     {COLOR_MENUTEXT}
    $000000,     {COLOR_WINDOWTEXT}
    $FFFFFF,     {COLOR_CAPTIONTEXT}
    $C0C0C0,     {COLOR_ACTIVEBORDER}
    $C0C0C0,     {COLOR_INACTIVEBORDER}
    $808080,     {COLOR_APPWORKSPACE}
    $800000,     {COLOR_HIGHLIGHT}
    $FFFFFF,     {COLOR_HIGHLIGHTTEXT}
    $D0D0D0,     {COLOR_BTNFACE}
    $808080,     {COLOR_BTNSHADOW}
    $808080,     {COLOR_GRAYTEXT}
    $000000,     {COLOR_BTNTEXT}
    $C0C0C0,     {COLOR_INACTIVECAPTIONTEXT}
    $F0F0F0,     {COLOR_BTNHIGHLIGHT}
    $000000,     {COLOR_3DDKSHADOW}
    $C0C0C0,     {COLOR_3DLIGHT}
    $000000,     {COLOR_INFOTEXT}
    $E1FFFF,     {COLOR_INFOBK}
    $000000,     {unasigned}
    $000000,     {COLOR_HOTLIGHT}
    $000000,     {COLOR_GRADIENTACTIVECAPTION}
    $000000      {COLOR_GRADIENTINACTIVECAPTION}
  *)

end;

{------------------------------------------------------------------------------
  Function: GetPen
  Params: f : PgdkDrawable; Value : TgdkColor
  Result: pgdkGC;


 ------------------------------------------------------------------------------}
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


{------------------------------------------------------------------------------
  Function: WaitForClipbrdAnswerDummyTimer
  
  this is a helper function for WaitForClipboardAnswer
 ------------------------------------------------------------------------------}
function WaitForClipbrdAnswerDummyTimer(Client: Pointer): gint; cdecl;
begin
  Result:=1; // go on, make sure getting a message at least every second
end;

{------------------------------------------------------------------------------
  Function: WaitForClipboardAnswer
  Params: none
  Returns: true, if clipboard data arrived

  waits til clipboard/selection answer arrived (max 1 second)
  ! While waiting the messagequeue will be processed !
 ------------------------------------------------------------------------------}
function WaitForClipboardAnswer(c: PClipboardEventData): boolean;
var
  StartTime, CurTime: TSystemTime;
  Timer: cardinal;
  
begin
  Result:=true;
{$IFDEF DEBUG_CLIPBOARD}
writeln('[TgtkObject.WaitForClipboardAnswer] A');
{$ENDIF}
  if (c^.Data.Selection<>0) then begin
//writeln('[TgtkObject.WaitForClipboardAnswer] B');
    exit;
  end;
  DateTimeToSystemTime(Time,StartTime);
//writeln('[TgtkObject.WaitForClipboardAnswer] C');
  Application.ProcessMessages;
//writeln('[TgtkObject.WaitForClipboardAnswer] D');
  if (c^.Data.Selection<>0) then begin
//writeln('[TgtkObject.WaitForClipboardAnswer] E  Yeah, Response received');
    exit;
  end;
//writeln('[TgtkObject.WaitForClipboardAnswer] F');
  // start a timer to make sure not waiting forever
  Timer := gtk_timeout_add(500, @WaitForClipbrdAnswerDummyTimer, nil);
  try
    repeat
      // just wait ...
{$IFDEF DEBUG_CLIPBOARD}
writeln('[TgtkObject.WaitForClipboardAnswer] G');
{$ENDIF}
      Application.HandleMessage;
      if (c^.Data.Selection<>0) then begin
{$IFDEF DEBUG_CLIPBOARD}
writeln('[TgtkObject.WaitForClipboardAnswer] E  Yeah, Response received');
{$ENDIF}
        exit;
      end;
      DateTimeToSystemTime(Time,CurTime);
    until (CurTime.Second-StartTime.Second>1);
  finally
{$IFDEF DEBUG_CLIPBOARD}
writeln('[TgtkObject.WaitForClipboardAnswer] H');
{$ENDIF}
    // stop the timer
    gtk_timeout_remove(Timer);
//writeln('[TgtkObject.WaitForClipboardAnswer] END');
  end;
  Result:=false;
end;

{------------------------------------------------------------------------------
  Function: RequestSelectionData
  Params: ClipboardWidget - widget with connected signals 'selection_get'
                            and 'selection_clear_event'
          ClipboardType
          FormatID - the selection target format wanted
  Returns: the TGtkSelectionData record

  requests the format FormatID of clipboard of type ClipboardType and
  waits til clipboard/selection answer arrived (max 1 second)
  ! While waiting the messagequeue will be processed !
 ------------------------------------------------------------------------------}
function RequestSelectionData(ClipboardWidget: PGtkWidget;
  ClipboardType: TClipboardType;  FormatID: cardinal): TGtkSelectionData;
var
  TimeID: cardinal;
  i: integer;
  c: PClipboardEventData;
begin
{$IFDEF DEBUG_CLIPBOARD}
writeln('[RequestSelectionData] FormatID=',FormatID);
{$ENDIF}
  FillChar(Result,SizeOf(TGtkSelectionData),0);
  if (ClipboardWidget=nil) or (FormatID=0) 
  or (ClipboardTypeAtoms[ClipboardType]=0) then exit;
  
  TimeID:=1000;
  repeat
    repeat
      inc(TimeID);
      if TimeID>1100 then exit;
      i:=ClipboardSelectionData.Count-1;
      while (i>=0) 
      and (PClipboardEventData(ClipboardSelectionData[i])^.TimeID<>TimeID) do
        dec(i);
    until (i<0);
    New(c);
    c^.TimeID:=TimeID;
    FillChar(c^.Data,SizeOf(TGtkSelectionData),0);
    ClipboardSelectionData.Add(c);
{$IFDEF DEBUG_CLIPBOARD}
writeln('[RequestSelectionData] TimeID=',TimeID);
{$ENDIF}
    if gtk_selection_convert(ClipboardWidget,
        ClipboardTypeAtoms[ClipboardType], FormatID, TimeID)<>0 then break;
    ClipboardSelectionData.Remove(c);
    Dispose(c);
  until false;
  try
    if not WaitForClipboardAnswer(c) then exit;
    Result:=c^.Data;
  finally
    ClipboardSelectionData.Remove(c);
    Dispose(c);
  end;
end;

{------------------------------------------------------------------------------
  Function: FreeClipboardTargetEntries
  Params: ClipboardType
  Returns: -

  frees the memory of a ClipboardTargetEntries list
 ------------------------------------------------------------------------------}
procedure FreeClipboardTargetEntries(ClipboardType: TClipboardType);
var i: integer;
begin
  if ClipboardTargetEntries[ClipboardType]<>nil then begin
    for i:=0 to ClipboardTargetEntryCnt[ClipboardType]-1 do
      StrDispose(ClipboardTargetEntries[ClipboardType][i].Target);
    FreeMem(ClipboardTargetEntries[ClipboardType]);
  end;
end;



{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

 { =============================================================================

  $Log$
  Revision 1.68  2002/08/21 13:35:25  lazarus
  MG: accelerations for synedit

  Revision 1.67  2002/08/21 11:29:36  lazarus
  MG: fixed mem some leaks in ide and gtk

  Revision 1.66  2002/08/21 10:46:37  lazarus
  MG: fixed unreleased gdiRegions

  Revision 1.65  2002/08/19 20:34:48  lazarus
  MG: improved Clipping, TextOut, Polygon functions

  Revision 1.64  2002/08/19 18:00:03  lazarus
  MG: design signals for gtk internal widgets

  Revision 1.63  2002/08/19 08:53:45  lazarus
  MG: fixed broken commit

  Revision 1.62  2002/08/19 08:50:28  lazarus
  MG: fixed parser for Clx enums and empty param lists

  Revision 1.61  2002/08/17 11:38:04  lazarus
  MG: fixed keygrabbing key translation

  Revision 1.60  2002/08/16 17:47:39  lazarus
  MG: added some IDE menuicons, fixed submenu indicator bug

  Revision 1.59  2002/08/15 15:46:49  lazarus
  MG: added changes from Andrew (Clipping)

  Revision 1.58  2002/08/15 15:11:01  lazarus
  MG: fixed showing menu accelarator shortcuts

  Revision 1.57  2002/08/15 13:37:58  lazarus
  MG: started menuitem icon, checked, radio and groupindex

  Revision 1.56  2002/08/05 07:43:29  lazarus
  MG: fixed BadCursor bug and Circle Reference of FixedWidget of csPanel

  Revision 1.55  2002/08/04 07:09:29  lazarus
  MG: fixed client events

  Revision 1.54  2002/07/23 07:40:52  lazarus
  MG: fixed get widget position for inherited gdkwindows

  Revision 1.53  2002/07/20 13:47:04  lazarus
  MG: fixed eventmask for realized windows

  Revision 1.52  2002/07/09 17:18:23  lazarus
  MG: fixed parser for external vars

  Revision 1.51  2002/06/26 15:11:10  lazarus
  MG: added new tool: Guess misplaced $IFDEF/$ENDIF

  Revision 1.50  2002/06/21 18:27:28  lazarus
  MG: non visual component icons are now centered

  Revision 1.49  2002/06/21 17:54:24  lazarus
  MG: in design mode the mouse cursor is now also set for hidden gdkwindows

  Revision 1.48  2002/06/21 16:59:16  lazarus
  MG: TControl.Cursor is now set, reduced auto reaction of widgets in design mode

  Revision 1.47  2002/06/19 19:46:10  lazarus
  MG: Form Editing: snapping, guidelines, modified on move/resize, creating components in csDesigning, ...

  Revision 1.46  2002/06/14 14:57:07  lazarus
  MG: fixed open file at cursor search path

  Revision 1.45  2002/06/11 13:41:11  lazarus
  MG: fixed mouse coords and fixed mouse clicked thru bug

  Revision 1.44  2002/06/09 14:00:42  lazarus
  MG: fixed persistent caret and implemented Form.BorderStyle=bsNone

  Revision 1.43  2002/06/04 15:17:23  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.42  2002/05/31 06:45:23  lazarus
  MG: deactivated new system colors, till we got a consistent solution

  Revision 1.41  2002/05/30 14:11:13  lazarus
  MG: added filters and history to TOpenDialog

  Revision 1.40  2002/05/29 21:44:39  lazarus
  MG: improved TCommon/File/OpenDialog, fixed TListView scrolling and broder

  Revision 1.39  2002/05/28 19:39:46  lazarus
  MG: added gtk rc file support and started stule dependent syscolors

  Revision 1.38  2002/05/13 14:47:02  lazarus
  MG: fixed client rectangles, TRadioGroup, RecreateWnd

  Revision 1.37  2002/05/12 04:56:21  lazarus
  MG: client rect bugs nearly completed

  Revision 1.36  2002/05/10 06:05:57  lazarus
  MG: changed license to LGPL

  Revision 1.35  2002/05/09 12:41:30  lazarus
  MG: further clientrect bugfixes

  Revision 1.34  2002/05/06 08:50:37  lazarus
  MG: replaced logo, increased version to 0.8.3a and some clientrectbugfix

  Revision 1.33  2002/04/26 12:26:51  lazarus
  MG: improved clean up

  Revision 1.32  2002/03/31 23:20:38  lazarus
  MG: fixed initial size of TPage

  Revision 1.31  2002/03/31 22:01:38  lazarus
  MG: fixed unreleased/unpressed Ctrl/Alt/Shift

  Revision 1.30  2002/03/25 17:59:23  lazarus
  GTK Cleanup
  Shane

  Revision 1.29  2002/02/18 22:46:11  lazarus
  Implented TMenuItem.ShortCut (not much tested).

  Revision 1.28  2001/12/10 11:16:00  lazarus
  MG: added GDK_dead_circumflex key

  Revision 1.26  2001/11/16 20:08:41  lazarus
  Object inspector has hints now.
  Shane

  Revision 1.25  2001/11/12 16:56:08  lazarus
  MG: CLIPBOARD

  Revision 1.24  2001/10/31 16:29:23  lazarus
  Fixed the gtk mousemove bug where the control gets the coord's based on it's parent instead of itself.
  Shane

  Revision 1.23  2001/10/08 12:57:07  lazarus
  MG: fixed GetPixel

  Revision 1.22  2001/10/08 08:05:08  lazarus
  MG: fixed TColorDialog set color

  Revision 1.21  2001/10/07 07:28:34  lazarus
  MG: fixed setpixel and TCustomForm.OnResize event

  Revision 1.20  2001/09/30 08:34:52  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.19  2001/06/20 17:34:37  lazarus
  MG: fixed unknown special key code

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
