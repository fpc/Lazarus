{
 *****************************************************************************
 *                                Gtk3WSForms.pp                                 *
 *                                ----------                                 * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Gtk3WSForms;

{$mode objfpc}{$H+}
{$i gtk3defines.inc}

interface
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as posible circles, the uses
//    clause should contain only those LCL units 
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the 
//    initialization section which actually 
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Classes, SysUtils, Math, Graphics, Controls, Forms, LCLType, LCLProc, LMessages,
////////////////////////////////////////////////////
  WSLCLClasses, WSControls, WSForms, WSProc,
  LazGtk3, LazGdk3, LazGLib2, LazGObject2, gtk3widgets, gtk3int, gtk3objects,
  gtk3wscontrols, gtk3mdiemulator;

type
  { TWSScrollingWinControl }

  TGtk3WSScrollingWinControlClass = class of TWSScrollingWinControl;

  { TGtk3WSScrollingWinControl }

  TGtk3WSScrollingWinControl = class(TWSScrollingWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLHandle; override;
    class procedure ScrollBy(const AWinControl: TWinControl; DeltaX, DeltaY: integer); override;
  end;

  { TWSScrollBox }

  TGtk3WSScrollBox = class(TGtk3WSScrollingWinControl)
  published
  end;

  { TWSCustomFrame }

  TGtk3WSCustomFrame = class(TGtk3WSScrollingWinControl)
  published
  end;

  { TWSFrame }

  TGtk3WSFrame = class(TGtk3WSCustomFrame)
  published
  end;

  { TWSCustomForm }

  { TGtk3WSCustomForm }

  TGtk3WSCustomForm = class(TWSCustomForm)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLHandle; override;
    class function  GetDefaultClientRect(const AWinControl: TWinControl;
      const aLeft, aTop, aWidth, aHeight: integer; var aClientRect: TRect): boolean; override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;

    class procedure ScrollBy(const AWinControl: TWinControl; DeltaX, DeltaY: integer); override;

    class procedure CloseModal(const ACustomForm: TCustomForm); override;
    class procedure SetAllowDropFiles(const AForm: TCustomForm; AValue: Boolean); override;
    class procedure SetAlphaBlend(const ACustomForm: TCustomForm; const AlphaBlend: Boolean;
      const Alpha: Byte); override;
    class procedure SetBorderIcons(const AForm: TCustomForm;
        const ABorderIcons: TBorderIcons); override;
    class procedure SetFormBorderStyle(const AForm: TCustomForm;
                             const AFormBorderStyle: TFormBorderStyle); override;
    class procedure SetFormStyle(const AForm: TCustomform; const AFormStyle, AOldFormStyle: TFormStyle); override;
    class procedure SetIcon(const AForm: TCustomForm; const Small, Big: HICON); override;
    class procedure ShowModal(const ACustomForm: TCustomForm); override;
    class procedure SetRealPopupParent(const ACustomForm: TCustomForm;
      const APopupParent: TCustomForm); override;
    class procedure SetShowInTaskbar(const AForm: TCustomForm; const AValue: TShowInTaskbar); override;
    class procedure SetZPosition(const AWinControl: TWinControl; const APosition: TWSZPosition); override;
    class function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; override;

    {mdi support}
    class function ActiveMDIChild(const AForm: TCustomForm): TCustomForm; override;
    class function Cascade(const AForm: TCustomForm): Boolean; override;
    class function GetClientHandle(const AForm: TCustomForm): HWND; override;
    class function GetMDIChildren(const AForm: TCustomForm; AIndex: Integer): TCustomForm; override;
    class function Next(const AForm: TCustomForm): Boolean; override;
    class function Previous(const AForm: TCustomForm): Boolean; override;
    class function Tile(const AForm: TCustomForm): Boolean; override;
    class function MDIChildCount(const AForm: TCustomForm): Integer; override;
    class function ArrangeIcons(const AForm: TCustomForm): Boolean; override;
  end;
  TGtk3WSCustomFormClass = class of TGtk3WSCustomForm;

  { TWSForm }

  TGtk3WSForm = class(TGtk3WSCustomForm)
  published
  end;

  { TWSHintWindow }

  { TGtk3WSHintWindow }

  TGtk3WSHintWindow = class(TGtk3WSCustomForm)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLHandle; override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
  end;

  { TWSScreen }

  TGtk3WSScreen = class(TWSLCLComponent)
  published
  end;

  { TWSApplicationProperties }

  TGtk3WSApplicationProperties = class(TWSLCLComponent)
  published
  end;

implementation
uses gtk3procs, LazLogger;


{ TGtk3WSScrollingWinControl }

class function TGtk3WSScrollingWinControl.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle;
begin
  Result := TLCLHandle(TGtk3ScrollingWinControl.Create(AWinControl, AParams));
end;

class procedure TGtk3WSScrollingWinControl.ScrollBy(const AWinControl: TWinControl;
  DeltaX, DeltaY: integer);
var
  ACtl: TGtk3ScrollingWinControl;
  Scrolled: PGtkScrolledWindow;
  HAdj, VAdj: PGtkAdjustment;
  NewH, NewV, MaxH, MaxV: Double;
  ASBAlloc: TGtkAllocation;
  ASBar: PGtkWidget;
begin
  if not AWinControl.HandleAllocated then
    exit;
  ACtl := TGtk3ScrollingWinControl(AWinControl.Handle);
  Scrolled := ACtl.GetScrolledWindow;

  if not Gtk3IsScrolledWindow(Scrolled) then
    exit;

  if ACtl.InUpdate then
    exit;

  HAdj := gtk_scrolled_window_get_hadjustment(Scrolled);
  VAdj := gtk_scrolled_window_get_vadjustment(Scrolled);

  ACtl.BeginUpdate;
  try

    if (DeltaX <> 0) and (HAdj <> nil) then
    begin
      MaxH := HAdj^.upper - HAdj^.page_size;
      if MaxH < HAdj^.lower then
        MaxH := HAdj^.lower;
      NewH := HAdj^.value - DeltaX;
      if NewH < HAdj^.lower then
        NewH := HAdj^.lower;
      if NewH > MaxH then
        NewH := MaxH;
      gtk_adjustment_set_value(HAdj, NewH);
    end;

    if (DeltaY <> 0) and (VAdj <> nil) then
    begin
      MaxV := VAdj^.upper - VAdj^.page_size;
      if MaxV < VAdj^.lower then
        MaxV := VAdj^.lower;
      NewV := VAdj^.value - DeltaY;
      if NewV < VAdj^.lower then
        NewV := VAdj^.lower;
      if NewV > MaxV then
        NewV := MaxV;
      gtk_adjustment_set_value(VAdj, NewV);
    end;

  finally
    ACtl.EndUpdate;
  end;


  if (wtWindow in ACtl.WidgetType) and
     Assigned(ACtl.LCLObject) and
     (ACtl.LCLObject is TCustomForm) and
     TCustomForm(ACtl.LCLObject).AutoScroll then
  begin

    if (DeltaX <> 0) and (HAdj <> nil) then
    begin
      ASBar := PGtkWidget(Scrolled^.get_hscrollbar);
      if ASBar^.get_realized and ASBar^.get_mapped then
      begin
        ASBar^.get_allocation(@ASBAlloc);
        ASBar^.size_allocate(@ASBAlloc);
      end;
    end;

    if (DeltaY <> 0) and (VAdj <> nil) then
    begin
      ASBar := PGtkWidget(Scrolled^.get_vscrollbar);
      if ASBar^.get_realized and ASBar^.get_mapped then
      begin
        ASBar^.get_allocation(@ASBAlloc);
        ASBar^.size_allocate(@ASBAlloc);
      end;
    end;

  end;

end;

{ TGtk3WSCustomForm }

class procedure TGtk3WSCustomForm.ScrollBy(const AWinControl: TWinControl;
  DeltaX, DeltaY: integer);
begin
  TGtk3WSScrollingWinControl.ScrollBy(AWinControl, DeltaX, DeltaY);
end;

class function TGtk3WSCustomForm.GetDefaultClientRect(
  const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer;
  var aClientRect: TRect): boolean;
var
  AWindow: TGtk3Window;
  Alloc: TGtkAllocation;
  MenuH: Integer;
begin
  Result := False;
  if AWinControl.HandleAllocated then
  begin
    AWindow := TGtk3Window(AWinControl.Handle);
    AWindow.GetContainerWidget^.get_allocation(@Alloc);
    if (Alloc.width > 1) or (Alloc.height > 1) then
      exit;
  end;
  aClientRect := Rect(0, 0, aWidth, aHeight);
  if (AWinControl is TCustomForm) and (TCustomForm(AWinControl).Menu <> nil) then
  begin
    MenuH := Gtk3WidgetSet.GetSystemMetrics(SM_CYMENU);
    if MenuH > 0 then
    begin
      Dec(aClientRect.Bottom, MenuH);
      if aClientRect.Bottom < 0 then
        aClientRect.Bottom := 0;
    end;
  end;
  Result := True;
end;

class function TGtk3WSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  AWindow: TGtk3Window;
  AGtkWindow: PGtkWindow;
  ARect: TGdkRectangle;
  AWidget: PGtkWidget;
begin
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.CreateHandle');
  {$ENDIF}
  if IsFormDesign(AWinControl) or (csDesigning in AWinControl.ComponentState) then
    AWindow := TGtk3DesignWidget.Create(AWinControl, AParams)
  else
  if (AWinControl is TCustomForm) and (TCustomForm(AWinControl).FormStyle = fsMDIChild) then
    AWindow := TGtk3MDIChildWindow.Create(AWinControl, AParams)
  else
    AWindow := TGtk3Window.Create(AWinControl, AParams);

  //debugln(['TGtk3WSCustomForm.CreateHandle AWindow.Widget=',Get3WidgetClassName(AWindow.Widget)]);

  AWidget:=AWindow.Widget;
  AGtkWindow:=nil;
  if Gtk3IsGtkWindow(AWidget) then
  begin
    AGtkWindow := PGtkWindow(AWidget);
    AWindow.Title := AWinControl.Caption;

    AGtkWindow^.set_resizable(True);
    AGtkWindow^.set_has_resize_grip(False);
  end;

  with ARect do
  begin
    x := AWinControl.Left;
    y := AWinControl.Top;
    width := AWinControl.Width;
    height := AWinControl.Height;
  end;
  AWidget^.set_allocation(@ARect);
  if AGtkWindow<>nil then
    Gtk3WidgetSet.AddWindow(AGtkWindow);

  Result := TLCLHandle(AWindow);

  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.CreateHandle handle ',dbgs(Result));
  {$ENDIF}
end;

class procedure TGtk3WSCustomForm.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetBounds') then
    Exit;
  {$IF DEFINED(GTK3DEBUGCORE) OR DEFINED(GTK3DEBUGSIZE)}
  DebugLn('TGtk3WSCustomForm.SetBounds ',dbgsName(AWinControl),Format(' ALeft %d ATop %d AWidth %d AHeight %d InUpdate %s',[ALeft, ATop, AWidth, AHeight, BoolToStr(TGtk3Widget(AWinControl.Handle).InUpdate, True)]));
  {$ENDIF}
  TGtk3Widget(AWinControl.Handle).SetBounds(ALeft,ATop,AWidth,AHeight);
end;

procedure ReleaseInputGrab;
var
  Display: PGdkDisplay;
  Seat: PGdkSeat;
begin
  Display := gdk_display_get_default();
  if not Assigned(Display) then
    Exit;
  Seat := gdk_display_get_default_seat(Display);
  if not Assigned(Seat) then
    Exit;
  gdk_seat_ungrab(Seat);
end;

function ModalFilter(xevent: PGdkXEvent; event: PGdkEvent; data: gpointer): TGdkFilterReturn;
const
  X11_ButtonPress = 4;
  X11_ButtonRelease = 5;
var
  AXType: Integer;
begin
  AXType := PInteger(xevent)^;
  if (AXType = X11_ButtonPress) or (AXType = X11_ButtonRelease) then
    Result := GDK_FILTER_REMOVE
  else
    Result := GDK_FILTER_CONTINUE;
end;

class procedure TGtk3WSCustomForm.ShowHide(const AWinControl: TWinControl);
const
  SplashPaintTimeoutMs = 120;
var
  AForm, OtherForm: TCustomForm;
  AWindow, ATransient: PGtkWindow;
  i: Integer;
  SavedH: PtrInt;
  ShouldBeVisible: Boolean;
  AGtk3Widget: TGtk3Widget;
  OtherGtk3Window: TGtk3Window;
  LCLCanFocus: boolean;
  ATime: guint32;
  NeedSizeProtect: boolean;
  SplashClock: PGdkFrameClock;
  SplashFrame: gint64;
  SplashDeadline: QWord;

  procedure CheckAndFixGeometry;
  const
    WaitDelay: gulong = 4000;
  var
    x, y: gint;
    IsBorderLess: Boolean;
    TargetOpacity: Double;
    GdkDisplay: PGdkDisplay;
    IsX11: Boolean;
    MenuHFix: gint;
  begin
    GdkDisplay := gdk_window_get_display(AWindow^.window);
    IsX11 := not Gtk3WidgetSet.IsWayland;
    MenuHFix := 0;
    if (AGtk3Widget is TGtk3Window) and
       not Assigned(AForm.Parent) and (AForm.FormStyle <> fsMDIChild) then
      MenuHFix := TGtk3Window(AGtk3Widget).GetMenuBarHeight;
    IsBorderLess := (AForm.BorderStyle = bsNone) or (not AWindow^.get_decorated);

    if not IsX11 then
    begin
      if AWindow^.get_window_type = GTK_WINDOW_POPUP then
        if (AWinControl.Width > 0) and (AWinControl.Height > 0) then
          PGtkWidget(AWindow)^.set_size_request(AWinControl.Width, AWinControl.Height);
      AWindow^.show_all;
      {On Wayland the surface is created at show_all time, so we must apply opacity now.
       gtk_widget_set_opacity called in CreateWidget may not persist for popup
       surfaces because the wl_surface doesn't exist until mapping.}
      if AForm.AlphaBlend then
        PGtkWidget(AWindow)^.set_opacity(AForm.AlphaBlendValue / 255.0);
      gdk_display_flush(GdkDisplay);
      exit;
    end;

    if AWindow^.get_window_type = GTK_WINDOW_POPUP then
    begin
      if (AWinControl.Width > 0) and (AWinControl.Height > 0) then
        PGtkWidget(AWindow)^.set_size_request(AWinControl.Width, AWinControl.Height);
      AWindow^.show_all;
      x := 0;
      y := 0;
      if (AWindow^.transient_for <> nil) and not AWindow^.get_decorated then
        if Assigned(AForm.PopupParent) or (AForm.PopupMode = pmAuto) then
          AWindow^.transient_for^.window^.get_origin(@x, @y);
      with AWinControl do
        AWindow^.window^.move_resize(Left + x, Top + y, Width, Height);
      if AForm.AlphaBlend then
        gdk_window_set_opacity(AWindow^.window, AForm.AlphaBlendValue / 255.0);
      gdk_display_flush(GdkDisplay);
      exit;
    end;

    if IsBorderLess then
    begin
      if AForm.AlphaBlend then
        TargetOpacity := AForm.AlphaBlendValue / 255.0
      else
        TargetOpacity := 1.0;

      gdk_window_set_opacity(AWindow^.window, 0.0);
      if (AWinControl.Width > 0) and (AWinControl.Height > 0) and
         (AWindow^.get_window_type = GTK_WINDOW_POPUP) then
        PGtkWidget(AWindow)^.set_size_request(AWinControl.Width, AWinControl.Height);
      AWindow^.show_all;
    end;

    with AWinControl do
      AWindow^.window^.move_resize(Left, Top, Width, Height + MenuHFix);

    gdk_display_flush(GdkDisplay);

    if IsBorderLess then
    begin
      if LCLCanFocus then
      begin
        g_usleep(WaitDelay);
        g_main_context_iteration(nil, False);
      end;
      gdk_window_set_opacity(AWindow^.window, TargetOpacity);
      gdk_window_invalidate_rect(AWindow^.window, nil, True);
      gdk_display_flush(GdkDisplay);
    end;

    AWindow^.window^.process_updates(True);

    if not IsBorderLess then
      AWindow^.show_all;
  end;


begin
  {$IF DEFINED(GTK3DEBUGCORE) OR DEFINED(GTK3DEBUGSIZE)}
  DebugLn('TGtk3WSCustomForm.ShowHide ', dbgsName(AWinControl),
    ' handleAllocated=', dbgs(AWinControl.HandleAllocated),
    ' shouldBeVisible=', dbgs(AWinControl.HandleObjectShouldBeVisible),
    ' bounds=', dbgs(AWinControl.BoundsRect));
  {$ENDIF}
  if not WSCheckHandleAllocated(AWinControl, 'ShowHide') then
    Exit;
  AForm := TCustomForm(AWinControl);
  {$IFDEF GTK3DEBUGCORE}
  writeln('>==== TGtk3WSCustomForm.ShowHide begin ');
  DebugLn('TGtk3WSCustomForm.ShowHide visible=',dbgs(AWinControl.HandleObjectShouldBeVisible));
  {$ENDIF}
  AGtk3Widget:=TGtk3Widget(AForm.Handle);
  if Gtk3IsGtkWindow(AGtk3Widget.Widget) then
    AWindow := PGtkWindow(AGtk3Widget.Widget)
  else
    AWindow := nil;


  ShouldBeVisible:=AForm.HandleObjectShouldBeVisible;

  NeedSizeProtect :=
    Assigned(AWindow) and (AForm.Parent = nil) and not IsFormDesign(AForm) and
    ((AForm.BorderStyle <> bsNone) or
     (Gtk3WidgetSet.IsMarcoWM and
      (AForm.BorderStyle = bsNone) and
      not (csNoFocus in AForm.ControlStyle)));
  if NeedSizeProtect then
  begin
    if ShouldBeVisible then
    begin
      i := PtrInt(g_object_get_data(PGObject(AGtk3Widget.Widget), 'lcl-form-last-w'));
      if i > 1 then
      begin
        SavedH := PtrInt(g_object_get_data(PGObject(AGtk3Widget.Widget), 'lcl-form-last-h'));
        {$IF DEFINED(GTK3DEBUGCORE) OR DEFINED(GTK3DEBUGSIZE)}
        DebugLn('TGtk3WSCustomForm.ShowHide ', dbgsName(AWinControl),
          ' RESTORE bounds from saved (', IntToStr(i), 'x', IntToStr(SavedH),
          ') - LCL was ', IntToStr(AWinControl.Width), 'x', IntToStr(AWinControl.Height));
        {$ENDIF}
        AWinControl.SetBounds(AWinControl.Left, AWinControl.Top, i, SavedH);
        //lcl-form-last-w/h is cleared by WindowSizeAllocate on the first WSA
        //after show. KDE Plasma Wayland needs it kept armed as kwin-override
        //protect target. We add a 100ms time bound for that.
        //TODO: heavy test with 50ms timeout.
        if Gtk3WidgetSet.IsKDEPlasmaWaylandSession then
          g_object_set_data(PGObject(AGtk3Widget.Widget), 'lcl-kwin-protect-until',
            Pointer(PtrUInt(GetTickCount64 + 100)));
      end;
    end else
    if (AWinControl.Width > 1) and (AWinControl.Height > 1) then
    begin
      g_object_set_data(PGObject(AGtk3Widget.Widget), 'lcl-form-last-w', Pointer(PtrInt(AWinControl.Width)));
      g_object_set_data(PGObject(AGtk3Widget.Widget), 'lcl-form-last-h', Pointer(PtrInt(AWinControl.Height)));
      g_object_set_data(PGObject(AGtk3Widget.Widget), 'lcl-kwin-protect-until', nil);
      {$IF DEFINED(GTK3DEBUGCORE) OR DEFINED(GTK3DEBUGSIZE)}
      DebugLn('TGtk3WSCustomForm.ShowHide ', dbgsName(AWinControl),
        ' SAVE bounds (', IntToStr(AWinControl.Width), 'x',
        IntToStr(AWinControl.Height), ') for next show');
      {$ENDIF}
    end;
  end;

  {$IFDEF GTK3DEBUGCORE}
  //use this if pure SetCapture(0) does not work under wayland.
  ReleaseInputGrab;
  {$ENDIF}
  LCLCanFocus := not (csNoFocus in AForm.ControlStyle);
  if AForm.BorderStyle <> bsNone then
    Gtk3WidgetSet.SetCapture(0);

  if ShouldBeVisible and not IsFormDesign(AForm) and (AForm.Parent = nil)
    and (not (wtMDIChild in AGtk3Widget.WidgetType)) then
  begin
    {note that gtk3 docs says that GDK_WINDOW_TYPE_HINT_UTILITY is for fsStayOnTop,
     and set_keep_above() is for fsSystemStayOnTop, but it does not work, so
     we use set_keep_above for both scenarios.}
    if (AForm.FormStyle in fsAllStayOnTop) then
      AWindow^.set_keep_above(True);

    if (fsModal in AForm.FormState) then
    begin
      if Assigned(Application) then
        Application.CancelHint;
      if Assigned(Screen) then
        for I := Screen.CustomFormCount - 1 downto 0 do
        begin
          OtherForm := Screen.CustomForms[I];
          if OtherForm.Visible and (OtherForm is THintWindow) then
            OtherForm.Hide;
        end;
      if not Gtk3WidgetSet.IsWayland then
        ReleaseInputGrab;
      AWindow^.set_modal(True);
      AWindow^.window^.set_modal_hint(true);
    end;

    AWindow^.realize;

    if Assigned(AForm.Menu) then
      TGtk3Window(AGtk3Widget).SetBounds(AWinControl.Left, AWinControl.Top,
        AWinControl.Width, AWinControl.Height);

    if (AForm.BorderStyle = bsNone) then
    begin
      if AWindow^.transient_for = nil then
      begin
        if Assigned(AForm.PopupParent) then
          ATransient := PGtkWindow(TGtk3Window(AForm.PopupParent.Handle).Widget)
        else
        if AForm.PopupMode = pmAuto then
          ATransient := GetActiveGtkWindow
        else
          ATransient := nil;

        {$IFDEF GTK3DEBUGCORE}
        if Assigned(ATransient) then
        begin
          writeln('TGtk3WSCustomFOrm.ShowHide: ATransient (popupParent form) is ',dbgsName(TGtk3Window(HwndFromGtkWidget(ATransient)).LCLObject));
          writeln(dbgsName(AForm),' bounds ',dbgs(Bounds(AForm.Left, AForm.Top, AForm.Width, AForm.Height)));
        end;
        {$ENDIF}

        AWindow^.set_transient_for(ATransient);
      end;
      if Assigned(AGtk3Widget.Shape) then
      begin
        AWindow^.set_app_paintable(True);
        AWindow^.set_visual(TGdkScreen.get_default^.get_rgba_visual);
        AGtk3Widget.SetWindowShape(AGtk3Widget.Shape, AWindow^.window);
      end;
    end;
  end;
  AGtk3Widget.BeginUpdate;
  AGtk3Widget.Visible := ShouldBeVisible;

  if wtMDIChild in AGtk3Widget.WidgetType then
  begin
    AGtk3Widget.EndUpdate;
    exit;
  end;

  if AGtk3Widget.Visible then
  begin
    if not IsFormDesign(AForm) and (fsModal in AForm.FormState) and (Application.ModalLevel > 0) then
    begin
      // DebugLn('TGtk3WSCustomForm.ShowHide ModalLevel=',dbgs(Application.ModalLevel),' Self=',dbgsName(AForm));
      for i := 0 to Screen.CustomFormZOrderCount - 1 do
      begin
        OtherForm:=Screen.CustomFormsZOrdered[i];
        // DebugLn('CustomFormZOrder[',dbgs(i),'].',dbgsName(OtherForm),' modal=',dbgs(fsModal in OtherForm.FormState));
        if (OtherForm <> AForm) and
          OtherForm.HandleAllocated then
        begin
          // DebugLn('TGtk3WSCustomForm.ShowHide setTransient for ',dbgsName(OtherForm));
          OtherGtk3Window:=TGtk3Window(OtherForm.Handle);
          if Gtk3IsGtkWindow(OtherGtk3Window.Widget) then
          begin
            if Gtk3IsGdkWindow(OtherGtk3Window.Widget^.window) then
              gdk_window_add_filter(OtherGtk3Window.Widget^.window, TGdkFilterFunc(@ModalFilter), AGtk3Widget);
            AWindow^.set_transient_for(PGtkWindow(OtherGtk3Window.Widget));
            break;
          end;
        end;
      end;
    end;

    if Assigned(AWinControl.Parent) then
    begin
      AGtk3Widget.EndUpdate;
      exit;
    end;

    //See issue #41412
    CheckAndFixGeometry;

    AWindow^.window^.set_events(GDK_ALL_EVENTS_MASK);
    if not IsFormDesign(AForm) then
    begin
      //If LM_NCHITTEST=true, do not attack WM
      if not AWindow^.window^.get_pass_through and (AForm.BorderStyle <> bsNone) then
      begin
        ATime := gtk_get_current_event_time;
        if ATime = 0 then
          ATime := Gtk3WidgetSet.LastUserEventTime;
        if ATime <> 0 then
          AWindow^.present_with_time(ATime);
      end;

      if Gtk3WidgetSet.IsWayland and (AWindow^.get_window_type = GTK_WINDOW_POPUP) and AWindow^.get_accept_focus
        and not AWindow^.window^.get_pass_through and LCLCanFocus then
      begin
        //wayland, add grab
        //TODO: gdk_display_device_is_grabbed
        gtk_device_grab_add(PGtkWidget(AWindow),
          gdk_seat_get_keyboard(gdk_display_get_default_seat(gdk_display_get_default)), False);
      end;
    end;
  end else
  begin
    if not IsFormDesign(AForm) and
      ((fsModal in AForm.FormState) or (AForm.BorderStyle = bsNone)) then
    begin
      if AWindow <> nil then
      begin
        //wayland, remove grab
        if Gtk3WidgetSet.IsWayland and (AWindow^.get_window_type = GTK_WINDOW_POPUP) and AWindow^.get_accept_focus
          and not AWindow^.window^.get_pass_through and LCLCanFocus then
            gtk_device_grab_remove(PGtkWidget(AWindow),
              gdk_seat_get_keyboard(gdk_display_get_default_seat(gdk_display_get_default)));
        if AWindow^.transient_for <> nil then
        begin
          if (fsModal in AForm.FormState) and Gtk3IsGdkWindow(AWindow^.transient_for^.window) then
            gdk_window_remove_filter(AWindow^.transient_for^.window, TGdkFilterFunc(@ModalFilter), AGtk3Widget);
          AWindow^.set_transient_for(nil);
        end;
      end;
    end;
  end;
  AGtk3Widget.EndUpdate;

  if (not ShouldBeVisible) and (fsModal in AForm.FormState) and (AWindow <> nil)
  and Gtk3IsGdkWindow(AWindow^.window) then
    gdk_display_flush(gdk_window_get_display(AWindow^.window));

  if ShouldBeVisible and Gtk3WidgetSet.IsWayland and (AWindow <> nil) and
     (AForm.BorderStyle in [bsDialog, bsSingle, bsToolWindow]) then
  begin
    while gtk_events_pending do
      gtk_main_iteration;
    TGtk3WSWinControl.ConstraintsChange(AWinControl);
  end;

  if ShouldBeVisible and (AForm.FormStyle = fsSplash) and (AWindow <> nil)
  and Gtk3IsGdkWindow(AWindow^.window) then
  begin
    AGtk3Widget.Update(nil);
    SplashClock := AWindow^.window^.get_frame_clock;
    if Assigned(SplashClock) then
    begin
      SplashFrame := SplashClock^.get_frame_counter;
      SplashClock^.request_phase([GDK_FRAME_CLOCK_PHASE_PAINT]);
      SplashDeadline := GetTickCount64 + SplashPaintTimeoutMs;
      while (SplashClock^.get_frame_counter = SplashFrame) and (GetTickCount64 < SplashDeadline) do
        gtk_main_iteration_do(True);
    end else
    begin
      while gtk_events_pending do
        gtk_main_iteration_do(False);
    end;
    gdk_display_flush(gdk_window_get_display(AWindow^.window));
  end;

  {$IFDEF GTK3DEBUGCORE}
  writeln('<==== TGtk3WSCustomForm.ShowHide end ');
  {$ENDIF}
end;

class procedure TGtk3WSCustomForm.CloseModal(const ACustomForm: TCustomForm);
begin
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.CloseModal');
  {$ENDIF}
end;

class procedure TGtk3WSCustomForm.SetAllowDropFiles(const AForm: TCustomForm;
  AValue: Boolean);
begin
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.SetAllowDropFiles');
  {$ENDIF}
  if not WSCheckHandleAllocated(AForm, 'SetAllowDropFiles') then
    Exit;

  if AValue then
    gtk_drag_dest_set(TGtk3Widget(AForm.Handle).Widget, GTK_DEST_DEFAULT_ALL,
      @FileDragTarget, 3, [GDK_ACTION_COPY, GDK_ACTION_MOVE])
  else
    gtk_drag_dest_unset(TGtk3Widget(AForm.Handle).Widget);
end;

class procedure TGtk3WSCustomForm.SetBorderIcons(const AForm: TCustomForm;
        const ABorderIcons: TBorderIcons);
begin
  if not WSCheckHandleAllocated(AForm, 'SetBorderIcons') then
    Exit;
  TGtk3Window(AForm.Handle).UpdateWindowFunctions;
end;

class procedure TGtk3WSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
  const AFormBorderStyle: TFormBorderStyle);
var
  ChildWin: TGtk3MDIChildWindow;
begin
  if not WSCheckHandleAllocated(AForm, 'SetFormBorderStyle') then
    Exit;
  if (AForm.FormStyle = fsMDIChild) and (not (csDesigning in AForm.ComponentState))
    and (TObject(AForm.Handle) is TGtk3MDIChildWindow) then
  begin
    ChildWin := TGtk3MDIChildWindow(AForm.Handle);
    if ChildWin.MDIFrame <> nil then
    begin
      ChildWin.MDIFrame.SetBorderStyle(AFormBorderStyle);
      exit;
    end;
  end;
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.SetFormBorderStyle');
  {$ENDIF}
  RecreateWnd(AForm);
end;

class procedure TGtk3WSCustomForm.SetFormStyle(const AForm: TCustomform;
  const AFormStyle, AOldFormStyle: TFormStyle);
begin
  if not WSCheckHandleAllocated(AForm, 'SetFormStyle') then
    Exit;
  {$IF DEFINED(GTK3DEBUGCORE) OR DEFINED(GTK3DEBUGFORMS)}
  DebugLn('TGtk3WSCustomForm.SetFormStyle: NOT IMPLEMENTED !');
  {$ENDIF}
end;
    
class procedure TGtk3WSCustomForm.SetIcon(const AForm: TCustomForm; const Small, Big: HICON);
begin
  if not WSCheckHandleAllocated(AForm, 'SetIcon') then
    Exit;
  if Big = 0 then
    TGtk3Window(AForm.Handle).Icon := Gtk3WidgetSet.AppIcon
  else
    TGtk3Window(AForm.Handle).Icon := TGtk3Image(Big).Handle;
end;

class procedure TGtk3WSCustomForm.SetShowInTaskbar(const AForm: TCustomForm;
  const AValue: TShowInTaskbar);
var
  AWindow: TGtk3Window;
  Enable: boolean;
begin
  if not WSCheckHandleAllocated(AForm, 'SetShowInTaskbar') then
    Exit;
  {$IF DEFINED(GTK3DEBUGCORE) OR DEFINED(GTK3DEBUGFORMS)}
  DebugLn('TGtk3WSCustomForm.SetShowInTaskbar');
  {$ENDIF}
  if (AForm.Parent <> nil) or
     (AForm.ParentWindow <> 0) or
     not (AForm.HandleAllocated) then Exit;
  AWindow := TGtk3Window(AForm.Handle);
  if not Gtk3IsGdkWindow(AWindow.Widget^.window) then
    exit;
  Enable := AValue <> stNever;
  if (not Enable) and AWindow.SkipTaskBarHint then
    AWindow.SkipTaskBarHint := False;
  AWindow.SkipTaskBarHint := not Enable;
end;

class procedure TGtk3WSCustomForm.SetZPosition(const AWinControl: TWinControl; const APosition: TWSZPosition);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetZPosition') then
    Exit;
  {$IF DEFINED(GTK3DEBUGCORE) OR DEFINED(GTK3DEBUGFORMS)}
  DebugLn('TGtk3WSCustomForm.SetZPosition: NOT IMPLEMENTED');
  {$ENDIF}
end;

class function TGtk3WSCustomForm.GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor;
const
  DefColors: array[TDefaultColorType] of TColor = (
 { dctBrush } clForm,
 { dctFont  } clBtnText
  );
begin
  Result := DefColors[ADefaultColorType];
end;

class procedure TGtk3WSCustomForm.ShowModal(const ACustomForm: TCustomForm);
begin
  if not WSCheckHandleAllocated(ACustomForm, 'ShowModal') then
    Exit;
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.ShowModal ... we are using ShowHide.');
  {$ENDIF}
end;

class procedure TGtk3WSCustomForm.SetRealPopupParent(
  const ACustomForm: TCustomForm; const APopupParent: TCustomForm);
begin
  if not WSCheckHandleAllocated(ACustomForm, 'SetRealPopupParent') then
    Exit;
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.SetRealPopupParent AForm=',dbgsName(ACustomForm),' PopupParent=',dbgsName(APopupParent));
  {$ENDIF}
  if Assigned(APopupParent) and APopupParent.HandleAllocated then
    PGtkWindow(TGtk3Window(ACustomForm.Handle).Widget)^.set_transient_for(PGtkWindow(TGtk3Window(APopupParent.Handle).Widget))
  else
    PGtkWindow(TGtk3Window(ACustomForm.Handle).Widget)^.set_transient_for(nil);
end;

class procedure TGtk3WSCustomForm.SetAlphaBlend(const ACustomForm: TCustomForm;
  const AlphaBlend: Boolean; const Alpha: Byte);
begin
  if not WSCheckHandleAllocated(ACustomForm, 'SetAlphaBlend') then
    Exit;
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.SetAlphaBlend');
  {$ENDIF}
  if Gtk3IsGtkWindow(TGtk3Widget(ACustomForm.Handle).Widget) then
    if AlphaBlend then
      TGtk3Widget(ACustomForm.Handle).Widget^.set_opacity(Alpha / 255)
    else
      TGtk3Widget(ACustomForm.Handle).Widget^.set_opacity(1);
end;

{ mdi support }

function MDIWorkspaceForForm(AForm: TCustomForm): TGtk3MDIWorkspace;
var
  i: Integer;
  F: TCustomForm;
  Win: TGtk3Window;
begin
  Result := nil;
  if AForm = nil then
    exit;
  if AForm.FormStyle = fsMDIForm then
  begin
    if AForm.HandleAllocated and (TObject(AForm.Handle) is TGtk3Window) then
      Result := TGtk3Window(AForm.Handle).MDIWorkspace;
    exit;
  end;
  if AForm.FormStyle <> fsMDIChild then
    exit;

  if (Application <> nil) and (Application.MainForm <> nil)
    and (Application.MainForm.FormStyle = fsMDIForm)
    and Application.MainForm.HandleAllocated then
  begin
    Win := TGtk3Window(Application.MainForm.Handle);
    if (Win <> nil) and (Win.MDIWorkspace <> nil) then
      exit(Win.MDIWorkspace);
  end;
  if Screen = nil then
    exit;
  for i := 0 to Screen.CustomFormCount - 1 do
  begin
    F := Screen.CustomForms[i];
    if (F.FormStyle = fsMDIForm) and F.HandleAllocated then
    begin
      Win := TGtk3Window(F.Handle);
      if (Win <> nil) and (Win.MDIWorkspace <> nil) then
        exit(Win.MDIWorkspace);
    end;
  end;
end;

function MDILCLFormFromFrame(AFrame: TGtk3MDIChildFrame): TCustomForm;
var
  Win: TGtk3MDIChildWindow;
begin
  Result := nil;
  if (AFrame = nil) or (AFrame.UserData = nil) then
    exit;
  Win := TGtk3MDIChildWindow(AFrame.UserData);
  if (Win <> nil) and (Win.LCLObject is TCustomForm) then
    Result := TCustomForm(Win.LCLObject);
end;

class function TGtk3WSCustomForm.ActiveMDIChild(const AForm: TCustomForm
  ): TCustomForm;
var
  WS: TGtk3MDIWorkspace;
begin
  Result := nil;
  if not WSCheckHandleAllocated(AForm, 'ActiveMDIChild') then
    exit;
  WS := MDIWorkspaceForForm(AForm);
  if (WS = nil) or (WS.ActiveChild = nil) then
    exit;
  Result := MDILCLFormFromFrame(WS.ActiveChild);
end;

class function TGtk3WSCustomForm.Cascade(const AForm: TCustomForm): Boolean;
var
  WS: TGtk3MDIWorkspace;
  i, X, Y, ChildW, ChildH: Integer;
  F: TGtk3MDIChildFrame;
  Alloc: TGtkAllocation;
begin
  Result := False;
  if not WSCheckHandleAllocated(AForm, 'Cascade') then
    exit;
  if AForm.FormStyle <> fsMDIForm then
    exit;
  WS := MDIWorkspaceForForm(AForm);
  if (WS = nil) or (WS.Children.Count = 0) then
    exit(True);
  PGtkWidget(WS.Widget)^.get_allocation(@Alloc);

  ChildW := Alloc.width  * 7 div 10;
  ChildH := Alloc.height * 7 div 10;
  if ChildW < 200 then
    ChildW := 200;
  if ChildH < 150 then
    ChildH := 150;

  X := 0; Y := 0;
  for i := 0 to WS.Children.Count - 1 do
  begin
    F := TGtk3MDIChildFrame(WS.Children[i]);
    if F.State <> mwsNormal then F.Restore;
    F.SetBounds(X, Y, ChildW, ChildH);
    F.BringToFront;
    Inc(X, F.TitleBarH);
    Inc(Y, F.TitleBarH);
  end;
  Result := True;
end;

class function TGtk3WSCustomForm.GetClientHandle(const AForm: TCustomForm): HWND;
var
  Win: TGtk3Window;
begin
  Result := 0;
  if not WSCheckHandleAllocated(AForm, 'GetClientHandle') then
    exit;
  if (AForm.HandleAllocated) and (TGtk3Widget(AForm.Handle) is TGtk3Window) then
  begin
    Win := TGtk3Window(AForm.Handle);
    if Win.MDIArea <> nil then
      exit(HWND(Win.MDIArea));
  end;
  Result := AForm.Handle;
end;

class function TGtk3WSCustomForm.GetMDIChildren(const AForm: TCustomForm;
  AIndex: Integer): TCustomForm;
var
  WS: TGtk3MDIWorkspace;
begin
  Result := nil;
  if not WSCheckHandleAllocated(AForm, 'GetMDIChildren') then
    exit;
  WS := MDIWorkspaceForForm(AForm);
  if (WS = nil) or (AIndex < 0) or (AIndex >= WS.Children.Count) then
    exit;
  Result := MDILCLFormFromFrame(TGtk3MDIChildFrame(WS.Children[AIndex]));
end;

class function TGtk3WSCustomForm.MDIChildCount(const AForm: TCustomForm): Integer;
var
  WS: TGtk3MDIWorkspace;
begin
  Result := 0;
  if not WSCheckHandleAllocated(AForm, 'MDIChildCount') then
    exit;
  WS := MDIWorkspaceForForm(AForm);
  if WS <> nil then
    Result := WS.Children.Count;
end;

class function TGtk3WSCustomForm.Next(const AForm: TCustomForm): Boolean;
var
  WS: TGtk3MDIWorkspace;
  Idx: Integer;
  F: TGtk3MDIChildFrame;
begin
  Result := False;
  if not WSCheckHandleAllocated(AForm, 'Next') then
    exit;
  if AForm.FormStyle <> fsMDIForm then
    exit;
  WS := MDIWorkspaceForForm(AForm);

  if (WS = nil) or (WS.Children.Count = 0) then
    exit;

  Idx := WS.Children.IndexOf(WS.ActiveChild);
  if Idx < 0 then
    Idx := 0
  else
    Idx := (Idx + 1) mod WS.Children.Count;

  F := TGtk3MDIChildFrame(WS.Children[Idx]);
  F.BringToFront;
  Result := True;
end;

class function TGtk3WSCustomForm.Previous(const AForm: TCustomForm): Boolean;
var
  WS: TGtk3MDIWorkspace;
  Idx, N: Integer;
  F: TGtk3MDIChildFrame;
begin
  Result := False;
  if not WSCheckHandleAllocated(AForm, 'Previous') then
    exit;
  if AForm.FormStyle <> fsMDIForm then
    exit;
  WS := MDIWorkspaceForForm(AForm);
  if (WS = nil) or (WS.Children.Count = 0) then
    exit;
  N := WS.Children.Count;
  Idx := WS.Children.IndexOf(WS.ActiveChild);
  if Idx < 0 then
    Idx := 0
  else
    Idx := (Idx - 1 + N) mod N;
  F := TGtk3MDIChildFrame(WS.Children[Idx]);
  F.BringToFront;
  Result := True;
end;

class function TGtk3WSCustomForm.Tile(const AForm: TCustomForm): Boolean;
var
  WS: TGtk3MDIWorkspace;
  Alloc: TGtkAllocation;
  N, Cols, Rows, ColW, RowH, i, R, C: Integer;
  F: TGtk3MDIChildFrame;
begin
  Result := False;
  if not WSCheckHandleAllocated(AForm, 'Tile') then
    exit;
  if AForm.FormStyle <> fsMDIForm then
    exit;
  WS := MDIWorkspaceForForm(AForm);
  if WS = nil then
    exit;
  N := WS.Children.Count;
  if N = 0 then
    exit(True);

  PGtkWidget(WS.Widget)^.get_allocation(@Alloc);

  if (Alloc.width <= 0) or (Alloc.height <= 0) then
    exit(True);

  Cols := Round(Sqrt(N));
  if Cols < 1 then
    Cols := 1;
  Rows := (N + Cols - 1) div Cols;
  ColW := Alloc.width div Cols;
  RowH := Alloc.height div Rows;

  for i := 0 to N - 1 do
  begin
    F := TGtk3MDIChildFrame(WS.Children[i]);
    if F.State <> mwsNormal then F.Restore;
    R := i div Cols;
    C := i mod Cols;
    F.SetBounds(C * ColW, R * RowH, ColW, RowH);
  end;
  Result := True;
end;

class function TGtk3WSCustomForm.ArrangeIcons(const AForm: TCustomForm): Boolean;
var
  WS: TGtk3MDIWorkspace;
begin
  Result := False;
  if not WSCheckHandleAllocated(AForm, 'ArrangeIcons') then
    exit;
  if AForm.FormStyle <> fsMDIForm then
    exit;
  WS := MDIWorkspaceForForm(AForm);
  if WS <> nil then
  begin
    WS.RelayoutMinimized;
    Result := True;
  end;
end;

{ TGtk3WSHintWindow }

class function TGtk3WSHintWindow.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
begin
  Result := TLCLHandle(TGtk3HintWindow.Create(AWinControl, AParams));
end;

class procedure TGtk3WSHintWindow.ShowHide(const AWinControl: TWinControl);
var
  AWidget: PGtkWidget;
  procedure SetPassThroughRecursive(AGdkWindow: PGdkWindow);
  var
    AChildren: PGList;
    AItem: PGList;
  begin
    if not Gtk3IsGdkWindow(AGdkWindow) then
      exit;
    AGdkWindow^.set_pass_through(True);
    AChildren := AGdkWindow^.get_children;
    AItem := AChildren;
    while AItem <> nil do
    begin
      SetPassThroughRecursive(PGdkWindow(AItem^.data));
      AItem := AItem^.next;
    end;
    g_list_free(AChildren);
  end;
begin
  if AWinControl.HandleObjectShouldBeVisible then
  begin
    AWidget := TGtk3HintWindow(AWinControl.Handle).Widget;
    if GTK3WidgetSet.IsWayland then // ref.to #42033, X11 not need this (it lead to incorrect positioning)
      PGtkWindow(AWidget)^.set_transient_for(GetActiveGtkWindow);

    AWidget^.show_all;

    if Gtk3IsGdkWindow(AWidget^.window) and
      (AWinControl.Perform(LM_NCHITTEST, 0, 0) = HTTRANSPARENT) then
    SetPassThroughRecursive(AWidget^.window);
  end else
    TGtk3HintWindow(AWinControl.Handle).Hide;

end;

end.
