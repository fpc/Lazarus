{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSControls.pp                             * 
 *                             -----------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit Gtk2WSControls;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Controls,
////////////////////////////////////////////////////
  Classes,
  Gtk2, Gdk2, Glib2, GtkGlobals, GtkDef,
  GtkWsControls,
  gtkProc, LCLType, LCLProc,
  WSControls, WSLCLClasses, WSProc;
  

type

  { TGtk2WSDragImageList }

  TGtk2WSDragImageList = class(TWSDragImageList)
  private
  protected
  public
  end;

  { TGtk2WSControl }

  TGtk2WSControl = class(TWSControl)
  private
  protected
  public
  end;

  { TGtk2WSWinControl }

  TGtk2WSWinControl = class(TGtkWSWinControl)
  private
  protected
  public
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    
    class procedure SetBiDiMode(const AWinControl: TWinControl; UseRightToLeftAlign, UseRightToLeftReading, UseRightToLeftScrollBar : Boolean); override;
    
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;
  end;

  { TGtk2WSGraphicControl }

  TGtk2WSGraphicControl = class(TWSGraphicControl)
  private
  protected
  public
  end;

  { TGtk2WSCustomControl }

  TGtk2WSCustomControl = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TGtk2WSImageList }

  TGtk2WSImageList = class(TWSImageList)
  private
  protected
  public
  end;

function Gtk2RangeScrollCB(ARange: PGtkRange; AScrollType: TGtkScrollType;
                  AValue: gdouble; AWidgetInfo: PWidgetInfo): gboolean; cdecl;


implementation
uses Gtk2Int, LMessages, Math;

{ TGtk2WSWinControl }

function GtkScrollTypeToScrollCode(ScrollType: TGtkScrollType): LongWord;
begin
  case ScrollType of
      GTK_SCROLL_NONE          : Result := SB_ENDSCROLL;
      GTK_SCROLL_JUMP          : Result := SB_THUMBPOSITION;
      GTK_SCROLL_STEP_BACKWARD : Result := SB_LINELEFT;
      GTK_SCROLL_STEP_FORWARD  : Result := SB_LINERIGHT;
      GTK_SCROLL_PAGE_BACKWARD : Result := SB_PAGELEFT;
      GTK_SCROLL_PAGE_FORWARD  : Result := SB_PAGERIGHT;
      GTK_SCROLL_STEP_UP       : Result := SB_LINEUP;
      GTK_SCROLL_STEP_DOWN     : Result := SB_LINEDOWN;
      GTK_SCROLL_PAGE_UP       : Result := SB_PAGEUP;
      GTK_SCROLL_PAGE_DOWN     : Result := SB_PAGEDOWN;
      GTK_SCROLL_STEP_LEFT     : Result := SB_LINELEFT;
      GTK_SCROLL_STEP_RIGHT    : Result := SB_LINERIGHT;
      GTK_SCROLL_PAGE_LEFT     : Result := SB_PAGELEFT;
      GTK_SCROLL_PAGE_RIGHT    : Result := SB_PAGERIGHT;
      GTK_SCROLL_START         : Result := SB_TOP;
      GTK_SCROLL_END           : Result := SB_BOTTOM;
    end;
end;

function Gtk2RangeScrollCB(ARange: PGtkRange; AScrollType: TGtkScrollType;
                  AValue: gdouble; AWidgetInfo: PWidgetInfo): gboolean; cdecl;
var
  Msg: TLMVScroll;
begin
  Result := CallBackDefaultReturn;
  //Assert(False, Format('Trace:[Gtk2RangeScrollCB] Value: %d', [RoundToInt(AValue)]));
  if G_OBJECT_TYPE(ARange) = gtk_hscrollbar_get_type then
    Msg.Msg := LM_HSCROLL
  else
    Msg.Msg := LM_VSCROLL;

  with Msg do begin
    Pos := Round(AValue);
    if Pos < High(SmallPos) then
      SmallPos := Pos
    else
      SmallPos := High(SmallPos);

    ScrollBar := HWND(PtrUInt(ARange));
    ScrollCode := GtkScrollTypeToScrollCode(AScrollType);
  end;
  Result := DeliverMessage(AWidgetInfo^.LCLObject, Msg) <> 0;
end;

function Gtk2ScrolledWindowScrollCB(AScrollWindow: PGtkScrolledWindow; AEvent: PGdkEventScroll; AWidgetInfo: PWidgetInfo): gboolean; cdecl;
var
  Msg: TLMVScroll;
  AValue: Double;
  Range: PGtkRange;
begin
  case AEvent^.direction of
    GDK_SCROLL_UP,
    GDK_SCROLL_DOWN: Msg.Msg := LM_VSCROLL;
    GDK_SCROLL_LEFT,
    GDK_SCROLL_RIGHT: Msg.Msg := LM_HSCROLL;
  end;

  case Msg.Msg of
    LM_VSCROLL: Range := GTK_RANGE(AScrollWindow^.vscrollbar);
    LM_HSCROLL: Range := GTK_RANGE(AScrollWindow^.hscrollbar);
  end;
  
  AValue :=  power(Range^.adjustment^.page_size, 2 / 3);
  
  if AEvent^.direction = GDK_SCROLL_UP then
    AValue := -AValue;
    
  AValue := gtk_range_get_value(Range) + AValue;
  
  AValue := Max(AValue, Range^.adjustment^.lower);
  AValue := Min(AValue, Range^.adjustment^.upper - Range^.adjustment^.page_size);

  with Msg do begin
    Pos := Round(AValue);
    if Pos < High(SmallPos) then
      SmallPos := Pos
    else
      SmallPos := High(SmallPos);

    ScrollBar := HWND(PtrUInt(Range));
    ScrollCode := SB_THUMBPOSITION;
  end;
  Result := DeliverMessage(AWidgetInfo^.LCLObject, Msg) <> 0;
end;


class function TGtk2WSWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
begin
  Widget := GTK2WidgetSet.CreateAPIWidget(AWinControl);
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}

  Result := THandle(PtrUInt(Widget));
  if Result = 0 then Exit;

  WidgetInfo := GetWidgetInfo(Widget); // Widget info already created in CreateAPIWidget
  WidgetInfo^.Style := AParams.Style;
  WidgetInfo^.ExStyle := AParams.ExStyle;
  WidgetInfo^.WndProc := PtrUInt(AParams.WindowClass.lpfnWndProc);

  // set allocation
  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(Widget, @Allocation);

  Set_RC_Name(AWinControl, Widget);
  
  TGtkWSWinControl.SetCallbacks(GTK_OBJECT(Widget), AWinControl);

  g_signal_connect(GTK_SCROLLED_WINDOW(Widget)^.hscrollbar, 'change-value', TGCallback(@Gtk2RangeScrollCB), WidgetInfo);
  g_signal_connect(GTK_SCROLLED_WINDOW(Widget)^.vscrollbar, 'change-value', TGCallback(@Gtk2RangeScrollCB), WidgetInfo);

  g_signal_connect(Widget, 'scroll-event', TGCallback(@Gtk2ScrolledWindowScrollCB), WidgetInfo);
end;

class procedure TGtk2WSWinControl.SetBiDiMode(const AWinControl : TWinControl;
  UseRightToLeftAlign, UseRightToLeftReading, UseRightToLeftScrollBar : Boolean
  );
const
  WidgetDirection : array[boolean] of longint = (GTK_TEXT_DIR_LTR, GTK_TEXT_DIR_RTL);
begin
  gtk_widget_set_direction(PGtkWidget(AWinControl.Handle), WidgetDirection[UseRightToLeftAlign]);
       
  if UseRightToLeftReading then // By default GTK2 support bidi regardless of the layout
    begin
    end
  else begin
       end;

  if UseRightToLeftScrollBar then  // I don't know how to do it for now (if possible)
   begin
   end
  else begin
       end;
end;

class function TGtk2WSWinControl.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
var
  TextBuf: PGtkTextBuffer;
  StartIter,
  EndIter: TGtkTextIter;
  CS: PChar;
  Handle: HWND;
begin
  Result := true;
  Handle := AWinControl.Handle;
  case AWinControl.fCompStyle of
   //csComboBox:
   //  begin
   //    AText := StrPas(gtk_entry_get_text(PGtkEntry(PGtkCombo(Handle)^.entry)));
   //  end;

   //csEdit, csSpinEdit:
   //    AText:= StrPas(gtk_entry_get_text(PgtkEntry(Handle)));

   csMemo    : begin
                  TextBuf := gtk_text_view_get_buffer(PGtkTextView(GetWidgetInfo(Pointer(Handle), True)^.CoreWidget));
                  gtk_text_buffer_get_start_iter(TextBuf, @StartIter);
                  gtk_text_buffer_get_end_iter(TextBuf, @EndIter);
                  CS := gtk_text_buffer_get_text(TextBuf, @StartIter, @EndIter, False);
                  AText := StrPas(CS);
                  g_free(CS);
               end;
  else
    Result := TGtkWSWinControl{(ClassParent)}.GetText(AWinControl, AText);
  end;
end;

class procedure TGtk2WSWinControl.SetText(const AWinControl: TWinControl;
  const AText: string);
var
  P : Pointer;
  TextBuf: PGtkTextBuffer;
  StartIter: TGtkTextIter;
  pLabel: pchar;
begin
  P := Pointer(AWinControl.Handle);
  
  pLabel := pchar(AText);
  
  case AWinControl.fCompStyle of
    csMemo        : begin
                    TextBuf := gtk_text_view_get_buffer(PGtkTextView(GetWidgetInfo(P, True)^.CoreWidget));
                    gtk_text_buffer_set_text(TextBuf, plabel, -1);
                    gtk_text_buffer_get_start_iter(TextBuf, @StartIter);
                    gtk_text_buffer_place_cursor(TextBuf, @StartIter);
                    //debugln('TGtkWSWinControl.SetText A ',dbgs(gtk_text_get_length(PGtkText(P))),' AText="',AText,'"');
                    //gtk_text_freeze(PGtkText(P));
                    //gtk_text_set_point(PGtkText(P), 0);
                    //gtk_text_forward_delete(PGtkText(P), gtk_text_get_length(PGtkText(P)));
                    //gtk_text_insert(PGtkText(P), nil, nil, nil, pLabel, -1);
                    //gtk_text_thaw(PGtkText(P));
                    //debugln('TGtkWSWinControl.SetText B ',dbgs(gtk_text_get_length(PGtkText(P))));
                  end;
  else
    TGtkWSWinControl{(ClassParent)}.SetText(AWinControl, AText);
  end;
end;

class procedure TGtk2WSWinControl.SetBorderStyle(
  const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
var
  Widget: PGtkWidget;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetBorderStyle')
  then Exit;
  
  Widget := PGtkWidget(AWinControl.Handle);
  if GTK_IS_SCROLLED_WINDOW(Widget) then
    gtk_scrolled_window_set_shadow_type(PGtkScrolledWindow(Widget), BorderStyleShadowMap[ABorderStyle])
  else
    TGtkWSWinControl{(ClassParent)}.SetBorderStyle(AWinControl, ABorderStyle);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TDragImageList, TGtk2WSDragImageList);
//  RegisterWSComponent(TControl, TGtk2WSControl);
  RegisterWSComponent(TWinControl, TGtk2WSWinControl);
//  RegisterWSComponent(TGraphicControl, TGtk2WSGraphicControl);
//  RegisterWSComponent(TCustomControl, TGtk2WSCustomControl);
//  RegisterWSComponent(TImageList, TGtk2WSImageList);
////////////////////////////////////////////////////
end.
