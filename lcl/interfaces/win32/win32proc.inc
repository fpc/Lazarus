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
  with Result^ do
  begin
    Height := AHeight;
    Width := AWidth;
    Depth := ADepth;
  end;
end;


{------------------------------------------------------------------------------
  Function: CopyDCData
  Params:  DestinationDC:  a dc to copy data to
           SourceDC: a dc to copy data from
  Returns: True if succesfu

  Creates a copy DC from the given DC
 ------------------------------------------------------------------------------}
function CopyDCData(const DestinationDC, SourceDC: PDeviceContext): Boolean;
begin
  Assert(False, Format('Trace:> [CopyDCData] DestDC:0x%x, SourceDC:0x%x', [Integer(DestinationDC), Integer(SourceDC)]));
  Result := (DestinationDC <> nil) and (SourceDC <> nil);
  if Result
  then begin
    with DestinationDC^ do
    begin
      hWnd := SourceDC^.hWnd;
      Drawable := SourceDC^.Drawable;
      if (SourceDC^.GC = HDC(Nil)) or (Drawable = Nil) then
        GC := HDC(Nil)
      else
      begin
        
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
procedure SelectWin32BrushProps(const DC: HDC);
var
  LB: LogBrush;
begin
  with LB, PDeviceContext(DC)^, CurrentBrush^ do
  begin
    Assert(False, 'TODO: Code SelectWin32BrushProps');
    //Assert(False, Format('Trace:  [SelectGDKBrushProps] Fill: %d | Color --> pixel: %d, red: 0x%x, green: 0x%x, blue: 0x%x', [Integer(GDIBrushFill), GDIBrushColor.Pixel, GDIBrushColor.Red, GDIBrushColor.Green, GDIBrushColor.Blue]));
    LBStyle := GDIBrushFill;
    LBColor := GDIBrushColor;
    SelectObject(GC, CreateBrushIndirect(TagLogBrush(LB)));
    SetBkColor(GC, CurrentBackCOlor);
    //TODO: Brush pixmap
  end;
end;

{------------------------------------------------------------------------------
  Procedure: SelectGDKPenProps
  Params:  DC: a (LCL)devicecontext
  Returns: Nothing

  Sets the forecolor and fill according to the pen
 ------------------------------------------------------------------------------}
procedure SelectWin32PenProps(const DC: HDC);
begin
  Assert(False, 'TODO: Code SelectWin32PenProps');
end;

{------------------------------------------------------------------------------
  Procedure: SelectGDKTextProps
  Params:  DC: a (LCL)devicecontext
  Returns: Nothing

  Sets the forecolor and fill according to the Textcolor
 ------------------------------------------------------------------------------}
procedure SelectWin32TextProps(const DC: HDC);
begin
  Assert(False, 'TODO: Code SelectWin32TextProps');
end;

{------------------------------------------------------------------------------
  Procedure: GTKEventState2ShiftState
  Params:    KeyState: The gtk keystate
  Returns:   the TShiftState for the given KeyState

  GTKEventState2ShiftState converts a GTK event state to a LCL/Delphi TShiftState
 ------------------------------------------------------------------------------}
function GTKEventState2ShiftState(KeyState: Word): TShiftState;
begin
  Assert(False, 'TRACE: Using function GTKEventState2ShiftState which isn''t implemented yet');
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
procedure GetWin32KeyInfo(const Event: PWin32KeyEvent; var KeyCode, VirtualKey: Word; var SysKey, Extended, Toggle: Boolean);
var
  TempKeyCode: Word;
  CtrlDown: Boolean;
begin
  Assert(False, 'TRACE: Using function GetWin32KeyInfo which isn''t implemented yet');
  KeyCode := Word(Ord(Integer(Event)));
  VirtualKey := MapVirtualKey(KeyCode, 1);
  TempKeyCode := KeyCode;
  SysKey := SysKey;
  CtrlDown := GetAsyncKeyState(VK_CONTROL) <> 0;
  Extended := (VirtualKey = VK_LSHIFT) Or (VirtualKey = VK_RSHIFT) Or (VirtualKey = VK_LCONTROL) Or (VirtualKey = VK_RCONTROL) Or (VirtualKey = VK_LMENU) Or (VirtualKey = VK_RMENU);
  Toggle := False;
  
  Assert(False, Format('Trace:[GetGTKKeyInfo] Event^.KeyVal %d, Event^.State %d, KeyCode %d, VirtualKey %d, SysKey %d, Extended %d, CtrlDown %d', [Integer(Event^.KeyVal), Integer(Event^.State), Integer(KeyCode), Integer(VirtualKey), Integer(SysKey), Integer(Extended), Integer(CtrlDown)]));
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
if Target=nil then writeln('[DeliverMessage] nil');
  if TObject(Target) is TControl 
  then begin
    TControl(Target).WindowProc(TLMessage(Message));
  end else begin
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
function ObjectToHWND(const AObject: TObject): HWND;
var
   handle : HWND;
begin
   if not assigned (AObject) then
   begin
      assert (false, 'TRACE:  [ObjectToGtkObject] Object not assigned');
      handle := 0
   end
   else if (AObject is TWinControl) then
   begin
      if TWinControl (AObject).HandleAllocated then handle := TWinControl(AObject).Handle
   end
   else if (AObject is TMenuItem) then
   begin 
      if TMenuItem(AObject).HandleAllocated then handle := TMenuItem(AObject).Handle 
   end
   else if (AObject is TMenu) then
   begin 
      if TMenu(AObject).HandleAllocated then handle := TMenu(AObject).Items.Handle 
   end
   else if (AObject is TCommonDialog) then
   begin
      {if TCommonDialog(AObject).HandleAllocated then } handle := TCommonDialog(AObject).Handle
   end
   else begin
      Assert(False, Format('Trace:  [ObjectToGtkObject] Message received with unhandled class-type <%s>', [AObject.ClassName])); 
      handle := 0;
   end;
   result := handle;
   if handle = 0 then Assert (false, 'Trace:  [ObjectToGtkObject]****** Warning: handle = 0 *******');
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
function CreateControlInfo(const Win32Control: PWin32Control): PWinControlInfo;
begin
  Assert(False, 'TRACE: Using function CreateControlInfo which isn''t implemented yet');
  if Win32Control = nil then
  begin
    Result := nil;
  end
  else
  begin
    New(Result);
    FillChar(Result^, SizeOf(Result^), 0);
    SetProp(Win32Control^.Window, 'Control_Info', Result);
  end;
end;

function GetControlInfo(const Control: PWin32Control; const Create: Boolean): PWinControlInfo;
begin
  Assert(False, 'TRACE: Using function GetControlInfo which isn''t implemented yet');
  if Control = nil
  then begin
    Result := nil;
  end
  else begin
    Result := PWinControlInfo(GetProp(Control^.Window, 'Control_Info'));
    if (Result = nil) and (Create) 
    then Result := CreateControlInfo(Control);
  end;
end;

// ----------------------------------------------------------------------
// the core_child widget points to the actual widget which implements the 
// functionality we needed. It is mainly used in composed controls like
// a listbox. In that case the core_child is the listbox, where a scrolling
// widget is main.
// ----------------------------------------------------------------------
function GetCoreChildControl(const Control: TObject): TObject;
begin
  Assert(False, 'TRACE: Using function GetCoreChildControl which isn''t implemented yet');
  Result := TObject(GetProp((Control As TWinControl).Handle, 'Core_Child'));
  if Result = TObject(nil) then Result := Control;
end;

procedure SetCoreChildControl(const ParentControl, ChildControl: TObject);
begin
  Assert(False, 'TRACE: Using function SetCoreChildControl which isn''t implemented yet');
  if (ParentControl <> TObject(nil)) and (ChildControl <> TObject(nil)) then
  SetProp((ParentControl As TWinControl).Handle, 'Core_Child', @ChildControl);
end;

// ----------------------------------------------------------------------
// the main widget is the widget passed as handle to the winAPI
// main data is stored in the fixed form to get a reference to its parent
// ----------------------------------------------------------------------
function GetMainControl(const Control: HWnd): Handle;
begin
  Assert(False, 'TRACE: Using function GetMainControl which isn''t implemented yet');
  Result := Handle(GetProp(Control, 'Main'));
  if Result = Handle(nil) then Result := Handle(Control);
end;

procedure SetMainControl(const ParentControl, ChildControl: HWnd);
begin
  Assert(False, 'TRACE: Using function SetMainControl which isn''t implemented yet');
  if (ParentControl <> HWND(nil)) and (ChildControl <> HWND(nil)) then
  SetProp(ChildControl, 'Main', Pointer(ParentControl));
end;

// ----------------------------------------------------------------------
// the fixed widget is the container for controls. By default a widget
// scales/places a control. whith the use of a fixed we can place them.
// ----------------------------------------------------------------------
function GetFixedControl(const Control: HWnd): Handle;
begin
  Assert(False, 'TRACE: Using function GetFixedControl which isn''t implemented yet');
  Result := Integer(GetProp(Control, 'Fixed'));
end;

procedure SetFixedControl(const ParentControl, FixedControl: HWnd);
begin
  Assert(False, 'TRACE: Using function SetFixedControl which isn''t implemented yet');
  if (ParentControl <> HWND(nil)) and (FixedControl <> HWND(nil)) then
  SetProp(ParentControl, 'Fixed', Pointer(FixedControl));
end;

// ----------------------------------------------------------------------
// Some need the LCLobject which created this widget.
// 
// MWE: IMO this shouldn't be needed
// ----------------------------------------------------------------------
procedure SetLCLObject(const Control: HWnd; const AnObject: TObject);
begin
  Assert(False, 'TRACE: Using function SetLCLObject which isn''t implemented yet');
  if (Control <> HWnd(Nil)) then
  SetProp(Control, 'Class', @AnObject);
end;

function GetLCLObject(const Control: HWnd): TObject;
begin
  Assert(False, 'TRACE: Using function GetLCLObject which isn''t implemented yet');
  Result := TObject(GetProp(Control, 'Class'));
end;

// ----------------------------------------------------------------------
// The Accelgroup and AccelKey is needed by menus
// ----------------------------------------------------------------------
procedure SetAccelGroup(const Control: HWnd; const AnAccelGroup: Pointer);
begin
  Assert(False, 'TRACE: Using function SetAccelGroup which isn''t implemented yet');
end;

function GetAccelGroup(const Control: Hwnd): HACCEL;
begin
  Assert(False, 'TRACE: Using function GetAccelGroup which isn''t implemented yet');
end;

procedure SetAccelKey(const Control: HWnd; const AKey: Integer);
begin
  Assert(False, 'TRACE: Using function SetAccelKey which isn''t implemented yet');
  if (Control <> Hwnd(Nil)) then
  SetProp(Control, 'AccelKey', Pointer(AKey));
end;

function GetAccelKey(const Control: HWnd): Integer;
begin
  Assert(False, 'TRACE: Using function GetAccelKey which isn''t implemented yet');
  Result := Integer(GetProp(Control, 'AccelKey'));
end;

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

 { =============================================================================

  $Log$
  Revision 1.2  2001/11/01 22:40:14  lazarus
  MG: applied Keith Bowes win32 interface updates

  Revision 1.1  2001/08/02 12:58:35  lazarus
  MG: win32 interface patch from Keith Bowes

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
