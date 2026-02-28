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
  Classes, Graphics, Controls, Forms, LCLType, LCLProc,
////////////////////////////////////////////////////
  WSLCLClasses, WSControls, WSForms, WSProc,
  LazGtk3, LazGdk3, LazGLib2, gtk3widgets, gtk3int, gtk3objects;

type
  { TWSScrollingWinControl }

  TGtk3WSScrollingWinControlClass = class of TWSScrollingWinControl;

  { TGtk3WSScrollingWinControl }

  TGtk3WSScrollingWinControl = class(TWSScrollingWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLHandle; override;
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
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;

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
uses SysUtils, gtk3procs, LazLogger;


{ TGtk3WSScrollingWinControl }

class function TGtk3WSScrollingWinControl.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle;
begin
  Result := TLCLHandle(TGtk3ScrollingWinControl.Create(AWinControl, AParams));
end;

{ TGtk3WSCustomForm }

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
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.SetBounds ',dbgsName(AWinControl),Format(' ALeft %d ATop %d AWidth %d AHeight %d InUpdate %s',[ALeft, ATop, AWidth, AHeight, BoolToStr(TGtk3Widget(AWinControl.Handle).InUpdate, True)]));
  {$ENDIF}
  TGtk3Widget(AWinControl.Handle).SetBounds(ALeft,ATop,AWidth,AHeight);
end;

{$IFDEF GTK3DEBUGCORE}
procedure ReleaseInputGrab;
var
  Display: PGdkDisplay;
  Seat: PGdkSeat;
begin
  // Get the default display
  Display := gdk_display_get_default();
  if not Assigned(Display) then
  begin
    WriteLn('Error: No default display available.');
    Exit;
  end;

  // Get the default seat
  Seat := gdk_display_get_default_seat(Display);
  if not Assigned(Seat) then
  begin
    WriteLn('Error: No default seat available.');
    Exit;
  end;
  Gtk3WidgetSet.SetCapture(0);
  gdk_seat_ungrab(Seat);
end;
{$ENDIF}

function ModalFilter(xevent: PGdkXEvent; event: PGdkEvent; data: gpointer): TGdkFilterReturn;
begin
  Result := GDK_FILTER_REMOVE;
end;

class procedure TGtk3WSCustomForm.ShowHide(const AWinControl: TWinControl);
var
  AForm, OtherForm: TCustomForm;
  AWindow, ATransient: PGtkWindow;
  i: Integer;
  AGeom: TGdkGeometry;
  AGeomMask: TGdkWindowHints;
  ShouldBeVisible: Boolean;
  AGtk3Widget: TGtk3Widget;
  OtherGtk3Window: TGtk3Window;

  procedure CheckAndFixGeometry;
  const
    WaitDelay: gulong = 4000;
  var
    x, y, w, h: gint;
    IsBorderLess: Boolean;
    TargetOpacity: Double;
    GdkDisplay: PGdkDisplay;
    IsX11: Boolean;
  begin
    GdkDisplay := gdk_window_get_display(AWindow^.window);
    IsX11 := not Gtk3WidgetSet.IsWayland;
    IsBorderLess := (AForm.BorderStyle = bsNone) or (not AWindow^.get_decorated);

    if not IsX11 then
    begin
      AWindow^.show_all;
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
      AWindow^.show_all;
    end;

    AWindow^.window^.get_geometry(@x, @y, @w, @h);
    x := 0;
    y := 0;

    if (AWindow^.transient_for <> nil) and not AWindow^.get_decorated then
      if Assigned(AForm.PopupParent) or (AForm.PopupMode = pmAuto) then
        AWindow^.transient_for^.window^.get_origin(@x, @y);

    with AWinControl do
      AWindow^.window^.move_resize(Left + x, Top + y, Width, Height);

    gdk_display_sync(GdkDisplay);

    if IsBorderLess then
    begin
      g_usleep(WaitDelay);
      g_main_context_iteration(nil, False);
      gdk_window_set_opacity(AWindow^.window, TargetOpacity);
      gdk_window_invalidate_rect(AWindow^.window, nil, True);
    end;

    AWindow^.window^.process_updates(True);

    if not IsBorderLess then
      AWindow^.show_all;
  end;


begin
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.ShowHide handleAllocated=',dbgs(AWinControl.HandleAllocated));
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

  {$IFDEF GTK3DEBUGCORE}
  //use this if pure SetCapture(0) does not work under wayland.
  ReleaseInputGrab;
  {$ENDIF}

  Gtk3WidgetSet.SetCapture(0);

  if ShouldBeVisible and not IsFormDesign(AForm) and (AForm.Parent = nil) then
  begin
    {note that gtk3 docs says that GDK_WINDOW_TYPE_HINT_UTILITY is for fsStayOnTop,
     and set_keep_above() is for fsSystemStayOnTop, but it does not work, so
     we use set_keep_above for both scenarios.}
    if (AForm.FormStyle in fsAllStayOnTop) then
      AWindow^.set_keep_above(True);

    if (fsModal in AForm.FormState) then
    begin
      AWindow^.set_modal(True);
      AWindow^.window^.set_modal_hint(true);
    end;

    AWindow^.realize;

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
      if not AWindow^.window^.get_pass_through then
        AWindow^.present_with_time(Gtk3WidgetSet.LastUserEventTime);

      if Gtk3WidgetSet.IsWayland and (AWindow^.get_window_type = GTK_WINDOW_POPUP) and AWindow^.get_accept_focus
        and not AWindow^.window^.get_pass_through then
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
      //wayland, remove grab
      if Gtk3WidgetSet.IsWayland and (AWindow^.get_window_type = GTK_WINDOW_POPUP) and AWindow^.get_accept_focus
        and not AWindow^.window^.get_pass_through then
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
  AGtk3Widget.EndUpdate;
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
begin
  if not WSCheckHandleAllocated(AForm, 'SetFormBorderStyle') then
    Exit;
  // will be done in interface override
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
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.SetFormStyle');
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
  {$IFDEF GTK3DEBUGCORE}
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
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomForm.SetZPosition');
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

class function TGtk3WSCustomForm.ActiveMDIChild(const AForm: TCustomForm
  ): TCustomForm;
begin
  Result := nil;
end;

class function TGtk3WSCustomForm.Cascade(const AForm: TCustomForm): Boolean;
begin
  Result := False;
end;

class function TGtk3WSCustomForm.GetClientHandle(const AForm: TCustomForm): HWND;
begin
  Result := 0;
end;

class function TGtk3WSCustomForm.GetMDIChildren(const AForm: TCustomForm;
  AIndex: Integer): TCustomForm;
begin
  Result := nil;
end;

class function TGtk3WSCustomForm.MDIChildCount(const AForm: TCustomForm): Integer;
begin
  Result := 0;
end;

class function TGtk3WSCustomForm.Next(const AForm: TCustomForm): Boolean;
begin
  Result := False;
end;

class function TGtk3WSCustomForm.Previous(const AForm: TCustomForm): Boolean;
begin
  Result := False;
end;

class function TGtk3WSCustomForm.Tile(const AForm: TCustomForm): Boolean;
begin
  Result := False;
end;

{ TGtk3WSHintWindow }

class function TGtk3WSHintWindow.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
begin
  Result := TLCLHandle(TGtk3HintWindow.Create(AWinControl, AParams));
end;

end.
