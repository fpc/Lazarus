{ $Id$}
{
 *****************************************************************************
 *                             GtkWSControls.pp                              *
 *                             ----------------                              *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

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
unit GtkWSControls;

{$mode objfpc}{$H+}

{$DEFINE UseGDKErrorTrap}

interface

uses
  {$IFDEF GTK2}
  Gtk2, Glib2, Gdk2,
  {$ELSE}
  Gtk, Glib, Gdk,
  {$ENDIF}
  SysUtils, Classes, Controls, LMessages, InterfaceBase,
  WSControls, WSLCLClasses, WSProc,
  Graphics, ComCtrls, Forms, LCLType,
  GTKWSPrivate,
  {$ifdef gtk1}
  GTK1WSPrivate,
  {$endif}
  GtkDef, GTKExtra;

type

  { TGtkWSDragImageList }

  TGtkWSDragImageList = class(TWSDragImageList)
  private
  protected
  public
  end;

  { TGtkWSControl }

  TGtkWSControl = class(TWSControl)
  private
  protected
  public
  end;


  { TGtkWSWinControl }

  TGtkWSWinControl = class(TWSWinControl)
  private
  protected
  public
    // Internal public
    class procedure SetCallbacks(const AGTKObject: PGTKObject; const AComponent: TComponent);
  public
    class procedure AddControl(const AControl: TControl); override;
    class function  CanFocus(const AWinControl: TWinControl): Boolean; override;
    class procedure ConstraintsChange(const AWinControl: TWinControl); override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure Invalidate(const AWinControl: TWinControl); override;

    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;

    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure SetChildZPosition(const AWinControl, AChild: TWinControl; const AOldPos, ANewPos: Integer; const AChildren: TFPList); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetCursor(const AWinControl: TWinControl; const ACursor: HCursor); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetSize(const AWinControl: TWinControl; const AWidth, AHeight: Integer); override;
    class procedure SetPos(const AWinControl: TWinControl; const ALeft, ATop: Integer); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;

    class procedure ShowHide(const AWinControl: TWinControl); override;
  end;

  { TGtkWSGraphicControl }

  TGtkWSGraphicControl = class(TWSGraphicControl)
  private
  protected
  public
  end;

  { TGtkWSCustomControl }

  TGtkWSCustomControl = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TGtkWSImageList }

  TGtkWSImageList = class(TWSImageList)
  private
  protected
  public
  end;

  { TGtkWSBaseScrollingWinControl }
  {
    TGtkWSBaseScrollingWinControl is a shared gtk only base implementation of
    all scrolling widgets, like TListView, TScrollingWinControl etc.
    It only creates a scrolling widget and handles the LM_HSCROLL and LM_VSCROLL
    messages
  }
  PBaseScrollingWinControlData = ^TBaseScrollingWinControlData;
  TBaseScrollingWinControlData = record
    HValue: Integer;
    HScroll: PGTKWidget;
    VValue: Integer;
    VScroll: PGTKWidget;
  end;

  { TGtkWSBaseScrollingWinControl }

  TGtkWSBaseScrollingWinControl = class(TWSWinControl)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class procedure SetCallbacks(const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  end;

procedure GtkWindowShowModal(GtkWindow: PGtkWindow);
function GetWidgetHAdjustment(AWidget: PGTKWidget): PGTKAdjustment;
function GetWidgetVAdjustment(AWidget: PGTKWidget): PGTKAdjustment;

implementation

uses
  GtkInt, gtkglobals, gtkproc, GTKWinApiWindow,
  StdCtrls, LCLProc, LCLIntf;



{ TGtkWSWinControl }

type
  TWinControlHack = class(TWinControl)
  end;

class procedure TGtkWSWinControl.AddControl(const AControl: TControl);
var
  AParent: TWinControl;
  ParentWidget: PGTKWidget;
  ChildWidget: PGTKWidget;
  pFixed: PGTKWidget;
begin
  {$IFDEF OldToolBar}
  if (AControl.Parent is TToolbar) then
    exit;
  {$ENDIF}

  AParent := TWinControl(AControl).Parent;
  //debugln('LM_AddChild: ',TWinControl(Sender).Name,' ',dbgs(AParent<>nil));
  if not Assigned(AParent) then begin
    Assert(true, Format('Trace: [TGtkWSWinControl.AddControl] %s --> Parent is not assigned', [AControl.ClassName]));
  end else begin
    Assert(False, Format('Trace:  [TGtkWSWinControl.AddControl] %s --> Calling Add Child: %s', [AParent.ClassName, AControl.ClassName]));
    ParentWidget := Pgtkwidget(AParent.Handle);
    pFixed := GetFixedWidget(ParentWidget);
    if pFixed <> ParentWidget then begin
      // parent changed for child
      ChildWidget := PgtkWidget(TWinControl(AControl).Handle);
      FixedPutControl(pFixed, ChildWidget, AParent.Left, AParent.Top);
      RegroupAccelerator(ChildWidget);
    end;
  end;
end;

class function TGtkWSWinControl.CanFocus(const AWinControl: TWinControl): Boolean;
var
  Widget, FocusWidget: PGtkWidget;
begin
  if AWinControl.HandleAllocated then
  begin
    Widget := PGtkWidget(AWinControl.Handle);
    FocusWidget := FindFocusWidget(Widget);
    Result := (FocusWidget <> nil) and GTK_WIDGET_CAN_FOCUS(FocusWidget);
  end else
    Result := False;
end;

class procedure TGtkWSWinControl.ConstraintsChange(const AWinControl: TWinControl);
var
  Widget: PGtkWidget;
  Geometry: TGdkGeometry;
begin
  Widget := PGtkWidget(AWinControl.Handle);
  if (Widget <> nil) and (GtkWidgetIsA(Widget,gtk_window_get_type)) then begin
    with Geometry, AWinControl do begin
      if Constraints.MinWidth > 0 then
        min_width:= Constraints.MinWidth else min_width:= 1;
      if Constraints.MaxWidth > 0 then
        max_width:= Constraints.MaxWidth else max_width:= 32767;
      if Constraints.MinHeight > 0 then
        min_height:= Constraints.MinHeight else min_height:= 1;
      if Constraints.MaxHeight > 0 then
        max_height:= Constraints.MaxHeight else max_height:= 32767;
      base_width:= Width;
      base_height:= Height;
      width_inc:= 1;
      height_inc:= 1;
      min_aspect:= 0;
      max_aspect:= 1;
    end;
    //debugln('TGtkWSWinControl.ConstraintsChange A ',GetWidgetDebugReport(Widget),' max=',dbgs(Geometry.max_width),'x',dbgs(Geometry.max_height));
    gtk_window_set_geometry_hints(PGtkWindow(Widget), nil, @Geometry,
                                  GDK_HINT_MIN_SIZE or GDK_HINT_MAX_SIZE);
  end;
end;

class procedure TGtkWSWinControl.DestroyHandle(const AWinControl: TWinControl);
begin
  //DebugLn('TGtkWSWinControl.DestroyHandle ',DbgSName(AWinControl));
  TGtkWidgetSet(WidgetSet).DestroyLCLComponent(AWinControl);
end;

class function TGtkWSWinControl.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  CS: PChar;
  Handle: HWND;
begin
  Result := False;
  if not WSCheckHandleAllocated(AWinControl, 'GetText')
  then Exit;

  Result := true;
  Handle := AWinControl.Handle;
  case AWinControl.fCompStyle of
   csComboBox:
     begin
       AText := StrPas(gtk_entry_get_text(PGtkEntry(PGtkCombo(Handle)^.entry)));
     end;

   csEdit, csSpinEdit:
       AText:= StrPas(gtk_entry_get_text(PgtkEntry(Handle)));

   csMemo    : begin
                  CS := gtk_editable_get_chars(PGtkOldEditable(
                    GetWidgetInfo(Pointer(Handle), True)^.CoreWidget), 0, -1);
                  AText := StrPas(CS);
                  g_free(CS);
               end;
  else
    Result := false;
  end;
end;

class procedure TGtkWSWinControl.Invalidate(const AWinControl: TWinControl);
begin
  if not WSCheckHandleAllocated(AWinControl, 'Invalidate')
  then Exit;

  Assert(false, 'Trace:Trying to invalidate window... !!!');
  gtk_widget_queue_draw(PGtkWidget(AWinControl.Handle));
end;

class procedure TGtkWSWinControl.ShowHide(const AWinControl: TWinControl);
begin
  // other methods use ShowHide also, can't move code
  TGtkWidgetSet(WidgetSet).ShowHide(AWinControl);
end;

class procedure TGtkWSWinControl.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetBounds')
  then Exit;

  ResizeHandle(AWinControl);
end;

class procedure TGtkWSWinControl.SetBorderStyle(const AWinControl: TWinControl;
  const ABorderStyle: TBorderStyle);
var
  Widget: PGtkWidget;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetBorderStyle')
  then Exit;

  Widget := PGtkWidget(AWinControl.Handle);
  if GtkWidgetIsA(Widget, GTKAPIWidget_GetType) then
    GTKAPIWidget_SetShadowType(PGTKAPIWidget(Widget), BorderStyleShadowMap[ABorderStyle]);
end;

class procedure TGtkWSWinControl.SetCallbacks(const AGTKObject: PGTKObject;
  const AComponent: TComponent);
begin
  GtkWidgetSet.SetCallback(LM_SHOWWINDOW, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_DESTROY, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_FOCUS, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_WINDOWPOSCHANGED, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_PAINT, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_EXPOSEEVENT, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_KEYDOWN, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_KEYUP, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_CHAR, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_MOUSEMOVE, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_LBUTTONDOWN, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_LBUTTONUP, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_RBUTTONDOWN, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_RBUTTONUP, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_MBUTTONDOWN, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_MBUTTONUP, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_MOUSEWHEEL, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_DROPFILES, AGTKObject, AComponent);
end;

class procedure TGtkWSWinControl.SetChildZPosition(
  const AWinControl, AChild: TWinControl;
  const AOldPos, ANewPos: Integer; const AChildren: TFPList);
var
  n: Integer;
  child: TWinControlHack;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetChildZPosition')
  then Exit;

  if ANewPos < AChildren.Count div 2
  then begin
    // move down (and others below us)
    for n := ANewPos downto 0 do
    begin
      child := TWinControlHack(AChildren[n]);
      if child.HandleAllocated
      then TGtkPrivateWidgetClass(child.WidgetSetClass.WSPrivate).
                  SetZPosition(child, wszpBack);
    end;
  end
  else begin
    // move up (and others above us)
    for n := ANewPos to AChildren.Count - 1 do
    begin
      child := TWinControlHack(AChildren[n]);
      if child.HandleAllocated
      then TGtkPrivateWidgetClass(child.WidgetSetClass.WSPrivate).SetZPosition(child, wszpFront);
    end;
  end;
end;

class procedure TGtkWSWinControl.SetCursor(const AWinControl: TWinControl; const ACursor: HCursor);
var
  WidgetInfo: PWidgetInfo;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetCursor')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(AWinControl.Handle));
  if (WidgetInfo^.ControlCursor = ACursor) and
     (WidgetInfo^.DefaultCursor <> HCursor(-1)) then Exit;
  if ACursor <> Screen.Cursors[crDefault] then
    WidgetInfo^.ControlCursor := ACursor
  else
  begin
    if WidgetInfo^.DefaultCursor = HCursor(-1) then
      TGtkPrivateWidgetClass(AWinControl.WidgetSetClass.WSPrivate).SetDefaultCursor(WidgetInfo);
    WidgetInfo^.ControlCursor := WidgetInfo^.DefaultCursor;
  end;
  TGtkPrivateWidgetClass(AWinControl.WidgetSetClass.WSPrivate).UpdateCursor(WidgetInfo);
end;

class procedure TGtkWSWinControl.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  Widget: PGtkWidget;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetFont')
  then Exit;

  Widget:=pGtkWidget(AWinControl.handle);
  if GtkWidgetIsA(Widget,GTKAPIWidget_GetType) then
    exit;

  if AFont.IsDefault then exit;
  //DebugLn('TGtkWSWinControl.SetFont ',DbgSName(AWinControl));
  GtkWidgetSet.SetWidgetFont(Widget,AFont);
  GtkWidgetSet.SetWidgetColor(Widget,AWinControl.Font.Color,clNone,
                              [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,
                               GTK_STATE_PRELIGHT,GTK_STATE_SELECTED
                               {$IFDEF GTK2},GTK_STYLE_TEXT{$ENDIF}]);
end;

class procedure TGtkWSWinControl.SetPos(const AWinControl: TWinControl;
  const ALeft, ATop: Integer);
var
  Widget: PGtkWidget;
  Allocation: TGTKAllocation;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetPos')
  then Exit;
  {$IFDEF VerboseSizeMsg}
  DebugLn(['TGtkWSWinControl.SetPos ',DbgSName(AWinControl),' ',ALeft,',',ATop]);
  {$ENDIF}
  
  Widget := PGtkWidget(AWinControl.Handle);
  Allocation.X := gint16(ALeft);
  Allocation.Y := gint16(ATop);
  Allocation.Width := guint16(Widget^.Allocation.Width);
  Allocation.Height := guint16(Widget^.Allocation.Height);
  gtk_widget_size_allocate(Widget, @Allocation);// Beware: this triggers callbacks
end;

class procedure TGtkWSWinControl.SetSize(const AWinControl: TWinControl;
  const AWidth, AHeight: Integer);
var
  Widget: PGtkWidget;
  Allocation: TGTKAllocation;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetSize')
  then Exit;
  {$IFDEF VerboseSizeMsg}
  DebugLn(['TGtkWSWinControl.SetSize ',DbgSName(AWinControl),' ',AWidth,',',AHeight]);
  {$ENDIF}

  Widget := PGtkWidget(AWinControl.Handle);
  Allocation.X := Widget^.Allocation.X;
  Allocation.Y := Widget^.Allocation.Y;
  Allocation.Width := guint16(AWidth);
  Allocation.Height := guint16(AHeight);
  gtk_widget_size_allocate(Widget, @Allocation);// Beware: this triggers callbacks
end;

class procedure TGtkWSWinControl.SetColor(const AWinControl: TWinControl);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetColor')
  then Exit;

  if  ((csOpaque in AWinControl.ControlStyle)
  and GtkWidgetIsA(pGtkWidget(AWinControl.handle),GTKAPIWidget_GetType)) then
    exit;
  //DebugLn('TGtkWSWinControl.SetColor ',DbgSName(AWinControl));
  GtkWidgetSet.SetWidgetColor(pGtkWidget(AWinControl.handle),
                              AWinControl.font.color, AWinControl.color,
                              [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,
                               GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
//    GtkWidgetSet.setWidgetFont(pGtkWidget(AWinControl.handle),aWinControl.font);
  UpdateWidgetStyleOfControl(AWinControl);
end;

class procedure TGtkWSWinControl.SetText(const AWinControl: TWinControl;
  const AText: string);

  procedure SetNotebookPageTabLabel;
  var
    NoteBookWidget: PGtkWidget; // the notebook
    PageWidget: PGtkWidget;     // the page (content widget)
    TabWidget: PGtkWidget;      // the tab (hbox containing a pixmap, a label
                                //          and a close button)
    TabLabelWidget: PGtkWidget; // the label in the tab
    MenuWidget: PGtkWidget;     // the popup menu (hbox containing a pixmap and
                                // a label)
    MenuLabelWidget: PGtkWidget; // the label in the popup menu item
    NewText: PChar;
  begin
    // dig through the hierachy to get the labels
    NoteBookWidget:=PGtkWidget((AWinControl.Parent).Handle);
    PageWidget:=PGtkWidget(AWinControl.Handle);
    TabWidget:=gtk_notebook_get_tab_label(PGtkNoteBook(NotebookWidget),
                                          PageWidget);
    if TabWidget<>nil then
      TabLabelWidget:=gtk_object_get_data(PGtkObject(TabWidget), 'TabLabel')
    else
      TabLabelWidget:=nil;
    MenuWidget:=gtk_notebook_get_menu_label(PGtkNoteBook(NotebookWidget),
                                            PageWidget);
    if MenuWidget<>nil then
      MenuLabelWidget:=gtk_object_get_data(PGtkObject(MenuWidget), 'TabLabel')
    else
      MenuLabelWidget:=nil;
    // set new text
    NewText:=PChar(AText);
    if TabLabelWidget<>nil then
      gtk_label_set_text(pGtkLabel(TabLabelWidget), NewText);
    if MenuLabelWidget<>nil then
      gtk_label_set_text(pGtkLabel(MenuLabelWidget), NewText);
  end;

var
  P : Pointer;
  aLabel, pLabel: pchar;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetText')
  then Exit;

  //TODO: create classprocedures for this in the corresponding classes

  P := Pointer(AWinControl.Handle);
  Assert(p = nil, 'Trace:WARNING: [TGtkWidgetSet.SetLabel] --> got nil pointer');
  Assert(False, 'Trace:Setting Str1 in SetLabel');
  pLabel := pchar(AText);

  case AWinControl.fCompStyle of
  csBitBtn,
  csButton: DebugLn('[WARNING] Obsolete call to TGTKOBject.SetLabel for ', AWinControl.ClassName);

{$IFDEF OldToolBar}
  csToolButton:
    with PgtkButton(P)^ do
    begin
      //aLabel := StrAlloc(Length(AnsiString(PLabel)) + 1);
      aLabel := Ampersands2Underscore(PLabel);
      Try
        //StrPCopy(aLabel, AnsiString(PLabel));
        //Accel := Ampersands2Underscore(aLabel);
        if gtk_bin_get_child(P) = nil then
        begin
          Assert(False, Format('trace:  [TGtkWidgetSet.SetLabel] %s has no child label', [AWinControl.ClassName]));
           gtk_container_add(P, gtk_label_new(aLabel));
        end
        else begin
          Assert(False, Format('trace:  [TGtkWidgetSet.SetLabel] %s has child label', [AWinControl.ClassName]));
          gtk_label_set_text(pgtkLabel( gtk_bin_get_child(P)), aLabel);
        end;
        //If Accel <> -1 then
        AccelKey:=gtk_label_parse_uline(PGtkLabel( gtk_bin_get_child(P)), aLabel);
        Accelerate(AWinControl,PGtkWidget(P),AccelKey,0,'clicked');
      Finally
        StrDispose(aLabel);
      end;
    end;
{$ENDIF OldToolBar}

  csForm,
  csFileDialog, csOpenFileDialog, csSaveFileDialog, csSelectDirectoryDialog,
  csPreviewFileDialog,
  csColorDialog,
  csFontDialog:
    if GtkWidgetIsA(p,gtk_window_get_type) then
      gtk_window_set_title(pGtkWindow(p),PLabel);

  csCheckBox,
  csToggleBox,
  csRadioButton:
    begin
      aLabel := Ampersands2Underscore(PLabel);
      Try
        gtk_label_set_text(
                    pGtkLabel(gtk_bin_get_child(PGtkBin(@PGTKToggleButton(p)^.Button))),
                    aLabel);
        gtk_label_parse_uline(pGtkLabel(gtk_bin_get_child(PGtkBin(@PGTKToggleButton(p)^.Button))),
          aLabel);
      Finally
        StrDispose(aLabel);
      end;
    end;

  csGroupBox    : if AText <> '' then
                    gtk_frame_set_label(pgtkFrame(P), pLabel)
                  else
                    gtk_frame_set_label(pgtkFrame(P), nil);

  csEdit        : begin
                    LockOnChange(PGtkObject(p),+1);
                    gtk_entry_set_text(pGtkEntry(P), pLabel);
                    LockOnChange(PGtkObject(p),-1);
                  end;
                  
  csSpinEdit    : begin
                    LockOnChange(PGtkObject(p),+1);
                    gtk_entry_set_text(pGtkEntry(P), pLabel);
                    gtk_spin_button_update(PGtkSpinButton(p));
                    LockOnChange(PGtkObject(p),-1);
                  end;

  csMemo        : begin
                    P:= GetWidgetInfo(P, True)^.CoreWidget;
                    //debugln('TGtkWSWinControl.SetText A ',dbgs(gtk_text_get_length(PGtkText(P))),' AText="',AText,'"');
                    gtk_text_freeze(PGtkText(P));
                    gtk_text_set_point(PGtkText(P), 0);
                    gtk_text_forward_delete(PGtkText(P), gtk_text_get_length(PGtkText(P)));
                    gtk_text_insert(PGtkText(P), nil, nil, nil, pLabel, -1);
                    gtk_text_thaw(PGtkText(P));
                    //debugln('TGtkWSWinControl.SetText B ',dbgs(gtk_text_get_length(PGtkText(P))));
                  end;

  csPage:
    SetNotebookPageTabLabel;

  csComboBox    :
    begin
      //DebugLn('SetLabel: ',TComboBox(Sender).Name,':',TComboBox(Sender).ClassName,
      //  ' ',DbgS(TComboBox(Sender).Handle),' "',PLabel,'"');
      SetComboBoxText(PGtkCombo(TComboBox(AWinControl).Handle), PLabel);
    end;

  else
    // DebugLn('WARNING: [TGtkWidgetSet.SetLabel] --> not handled for class ',Sender.ClassName);
  end;
  Assert(False, Format('trace:  [TGtkWidgetSet.SetLabel] %s --> END', [AWinControl.ClassName]));
end;



{ helper/common routines }

procedure GtkWindowShowModal(GtkWindow: PGtkWindow);
begin
  if (GtkWindow=nil) then exit;
  UnsetResizeRequest(PgtkWidget(GtkWindow));

  if ModalWindows=nil then ModalWindows:=TFPList.Create;
  ModalWindows.Add(GtkWindow);
  {$IFNDEF gtk_no_set_modal}
  gtk_window_set_modal(GtkWindow, true);
  {$ENDIF}
  gtk_widget_show(PGtkWidget(GtkWindow));
  {$IFDEF Gtk1}
  GDK_WINDOW_ACTIVATE(PGdkWindowPrivate(PGtkWidget(GtkWindow)^.window));
  {$ENDIF}

  {$IFDEF VerboseTransient}
  DebugLn('TGtkWidgetSet.ShowModal ',Sender.ClassName);
  {$ENDIF}
  TGtkWidgetSet(WidgetSet).UpdateTransientWindows;
end;

function GetWidgetHAdjustment(AWidget: PGTKWidget): PGTKAdjustment;
begin
  if GtkWidgetIsA(AWidget,GTK_TYPE_SCROLLED_WINDOW) then
    Result:=gtk_scrolled_window_get_hadjustment(PGTKScrolledWindow(AWidget))
  {$IFDEF Gtk2}
  else if GtkWidgetIsA(AWidget,GTK_TYPE_TREE_VIEW) then
    Result:=gtk_tree_view_get_hadjustment(PGtkTreeView(AWidget))
  {$ENDIF}
  else
    Result:=nil;
end;

function GetWidgetVAdjustment(AWidget: PGTKWidget): PGTKAdjustment;
begin
  if GtkWidgetIsA(AWidget,GTK_TYPE_SCROLLED_WINDOW) then
    Result:=gtk_scrolled_window_get_vadjustment(PGTKScrolledWindow(AWidget))
  {$IFDEF Gtk2}
  else if GtkWidgetIsA(AWidget,GTK_TYPE_TREE_VIEW) then
    Result:=gtk_tree_view_get_vadjustment(PGtkTreeView(AWidget))
  {$ENDIF}
  else
    Result:=nil;
end;


{ TGtkWSBaseScrollingWinControl }

function GtkWSBaseScrollingWinControl_HValueChanged(AAdjustment: PGTKAdjustment; AInfo: PWidgetInfo): GBoolean; cdecl;
var
  ScrollingData: PBaseScrollingWinControlData;
  Msg: TLMHScroll;
  OldValue, V, U, L, StepI, PageI: Integer;
  X, Y: GInt;
  Mask: TGdkModifierType;
begin
  Result := CallBackDefaultReturn;
  if AInfo^.ChangeLock > 0 then Exit;

  ScrollingData := AInfo^.UserData;

  // round values
  V := Round(AAdjustment^.Value);
  U := Round(AAdjustment^.Upper);
  L := Round(AAdjustment^.Lower);
  StepI := Round(AAdjustment^.Step_Increment);
  PageI := Round(AAdjustment^.Page_Increment);

  OldValue := ScrollingData^.HValue;
  ScrollingData^.HValue := V;

  // get keystates
  Mask := 0;
  if ScrollingData^.HScroll <> nil
  then begin
    {$IFDEF UseGDKErrorTrap}
    BeginGDKErrorTrap;
    {$ENDIF}
    gdk_window_get_pointer(GetControlWindow(ScrollingData^.HScroll), @X, @Y, @Mask);
    {$IFDEF UseGDKErrorTrap}
    EndGDKErrorTrap;
    {$ENDIF}
  end;

  Msg.msg := LM_HSCROLL;
  // get scrollcode
  if ssLeft in GTKEventStateToShiftState(Word(Mask))
  then Msg.ScrollCode := SB_THUMBTRACK
  else if V <= L
  then Msg.ScrollCode := SB_TOP
  else if V >= U
  then Msg.ScrollCode := SB_BOTTOM
  else if V - OldValue = StepI
  then Msg.ScrollCode := SB_LINERIGHT
  else if OldValue - V = StepI
  then Msg.ScrollCode := SB_LINELEFT
  else if V - OldValue = PageI
  then Msg.ScrollCode := SB_PAGERIGHT
  else if OldValue - V = PageI
  then Msg.ScrollCode := SB_PAGELEFT
  else Msg.ScrollCode := SB_THUMBPOSITION;
  Msg.Pos := V;
  if V < High(Msg.SmallPos)
  then Msg.SmallPos := V
  else Msg.SmallPos := High(Msg.SmallPos);
  Msg.ScrollBar := HWND(PtrUInt(ScrollingData^.HScroll));

  Result := (DeliverMessage(AInfo^.LCLObject, Msg) <> 0) xor CallBackDefaultReturn;
end;

function GtkWSBaseScrollingWinControl_VValueChanged(AAdjustment: PGTKAdjustment; AInfo: PWidgetInfo): GBoolean; cdecl;
var
  ScrollingData: PBaseScrollingWinControlData;
  Msg: TLMHScroll;
  OldValue, V, U, L, StepI, PageI: Integer;
  X, Y: GInt;
  Mask: TGdkModifierType;
begin
  Result := CallBackDefaultReturn;
  if AInfo^.ChangeLock > 0 then Exit;

  ScrollingData := AInfo^.UserData;

  // round values
  V := Round(AAdjustment^.Value);
  U := Round(AAdjustment^.Upper);
  L := Round(AAdjustment^.Lower);
  StepI := Round(AAdjustment^.Step_Increment);
  PageI := Round(AAdjustment^.Page_Increment);

  OldValue := ScrollingData^.VValue;
  ScrollingData^.VValue := V;

  // get keystates
  Mask := 0;
  if ScrollingData^.VScroll <> nil
  then begin
    {$IFDEF UseGDKErrorTrap}
    BeginGDKErrorTrap;
    {$ENDIF}
    gdk_window_get_pointer(GetControlWindow(ScrollingData^.VScroll), @X, @Y, @Mask);
    {$IFDEF UseGDKErrorTrap}
    EndGDKErrorTrap;
    {$ENDIF}
  end;

  Msg.msg := LM_VSCROLL;
  // Get scrollcode
  if ssLeft in GTKEventStateToShiftState(Word(Mask))
  then Msg.ScrollCode := SB_THUMBTRACK
  else if V <= L
  then Msg.ScrollCode := SB_TOP
  else if V >= U
  then Msg.ScrollCode := SB_BOTTOM
  else if V - OldValue = StepI
  then Msg.ScrollCode := SB_LINEDOWN
  else if OldValue - V = StepI
  then Msg.ScrollCode := SB_LINEUP
  else if V - OldValue = PageI
  then Msg.ScrollCode := SB_PAGEDOWN
  else if OldValue - V = PageI
  then Msg.ScrollCode := SB_PAGEUP
  else Msg.ScrollCode := SB_THUMBPOSITION;
  Msg.Pos := V;
  if V < High(Msg.SmallPos)
  then Msg.SmallPos := V
  else Msg.SmallPos := High(Msg.SmallPos);
  Msg.ScrollBar := HWND(PtrUInt(ScrollingData^.HScroll));

  Result := (DeliverMessage(AInfo^.LCLObject, Msg) <> 0) xor CallBackDefaultReturn;
end;

class function TGtkWSBaseScrollingWinControl.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  ScrollingData: PBaseScrollingWinControlData;
  Allocation: TGTKAllocation;
begin
  Widget := gtk_scrolled_window_new(nil, nil);
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget,dbgsName(AWinControl));
  {$ENDIF}

  Result := THandle(PtrUInt(Widget));
  if Result = 0 then Exit;

  gtk_widget_show(Widget);

  WidgetInfo := CreateWidgetInfo(Widget, AWinControl, AParams);
  New(ScrollingData);
  ScrollingData^.HValue := 0;
  ScrollingData^.VValue := 0;
  ScrollingData^.HScroll := PGtkScrolledWindow(Widget)^.HScrollbar;
  ScrollingData^.VScroll := PGtkScrolledWindow(Widget)^.VScrollbar;
  WidgetInfo^.UserData := ScrollingData;
  WidgetInfo^.DataOwner := True;

  // set allocation
  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(Widget, @Allocation);

  // SetCallbacks isn't called here, it should be done in the 'derived' class
end;

class procedure TGtkWSBaseScrollingWinControl.SetCallbacks(
  const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AWidget),
                                TComponent(AWidgetInfo^.LCLObject));
  SignalConnect(
    PGtkWidget(GetWidgetHAdjustment(AWidget)),
    'value-changed',
    @GtkWSBaseScrollingWinControl_HValueChanged,
    AWidgetInfo
  );
  SignalConnect(
    PGtkWidget(GetWidgetVAdjustment(AWidget)),
    'value-changed',
    @GtkWSBaseScrollingWinControl_VValueChanged,
    AWidgetInfo
  );
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TDragImageList, TGtkWSDragImageList);
//  RegisterWSComponent(TControl, TGtkWSControl);
  RegisterWSComponent(TWinControl, TGtkWSWinControl, TGtkPrivateWidget);
//  RegisterWSComponent(TGraphicControl, TGtkWSGraphicControl);
//  RegisterWSComponent(TCustomControl, TGtkWSCustomControl);
//  RegisterWSComponent(TImageList, TGtkWSImageList);
////////////////////////////////////////////////////
end.
