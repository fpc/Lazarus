{
 *****************************************************************************
 *                              Gtk3WSMenus.pp                               *
 *                              --------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Gtk3WSMenus;

{$mode objfpc}{$H+}
{$i gtk3defines.inc}

interface

uses
  Classes, Types,
  LazGObject2, LazGlib2, LazGdk3, LazGtk3, gtk3procs,
  LazLogger,
  WSLCLClasses, WSMenus,
  LCLType, LMessages, Graphics, Menus, Forms, Controls, LCLIntf;

type

  { TGtk3WSMenuItem }

  TGtk3WSMenuItem = class(TWSMenuItem)
  published
    class procedure AttachMenu(const AMenuItem: TMenuItem); override;
    class function CreateHandle(const AMenuItem: TMenuItem): HMENU; override;
    class procedure DestroyHandle(const AMenuItem: TMenuItem); override;
    class procedure SetCaption(const AMenuItem: TMenuItem; const ACaption: string); override;
    class procedure SetShortCut(const AMenuItem: TMenuItem; const ShortCutK1, ShortCutK2: TShortCut); override;
    class procedure SetVisible(const AMenuItem: TMenuItem; const Visible: boolean); override;
    class function SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean; override;
    class function SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean; override;
    class function SetRadioItem(const AMenuItem: TMenuItem; const {%H-}RadioItem: boolean): boolean; override;
    class function SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean; override;
    class procedure UpdateMenuIcon(const AMenuItem: TMenuItem; const HasIcon: Boolean; const {%H-}AIcon: TBitmap); override;
  end;

  { TGtk3WSMenu }

  TGtk3WSMenu = class(TWSMenu)
  published
    class function CreateHandle(const AMenu: TMenu): HMENU; override;
    class procedure SetBiDiMode(const AMenu: TMenu; UseRightToLeftAlign, {%H-}UseRightToLeftReading : Boolean); override;
  end;

  { TGtk3WSMainMenu }

  TGtk3WSMainMenu = class(TWSMainMenu)
  published
  end;

  { TGtk3WSPopupMenu }

  TGtk3WSPopupMenu = class(TWSPopupMenu)
  protected
  published
    class function CreateHandle(const AMenu: TMenu): HMENU; override;
    class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: integer); override;
  end;


implementation
uses gtk3widgets, gtk3int, LCLMessageGlue;

var
  MenuWidget: PGtkWidget = nil;

function Gtk3MenuPopupRepositionIdle(data: gpointer): gboolean; cdecl;
var
  TopLevel: PGtkWidget;
  GdkWin: PGdkWindow;
  Display: PGdkDisplay;
  Monitor: PGdkMonitor;
  WorkArea: TGdkRectangle;
  tlMinW, tlNatW, tlMinH, tlNatH: gint;
begin
  Result := False;

  if (data = nil) or not PGtkWidget(data)^.get_mapped then
    exit;

  TopLevel := PGtkWidget(data)^.get_toplevel;
  if not Gtk3IsGtkWindow(TopLevel) then
    exit;

  GdkWin := TopLevel^.get_window;

  if GdkWin = nil then
    exit;

  TopLevel^.get_preferred_width(@tlMinW, @tlNatW);
  TopLevel^.get_preferred_height(@tlMinH, @tlNatH);

  Display := GdkWin^.get_display;
  if Display <> nil then
  begin
    Monitor := Display^.get_monitor_at_window(GdkWin);
    if Monitor <> nil then
    begin
      //fit to monitor if natural size is bigger than workarea
      Monitor^.get_workarea(@WorkArea);
      if (WorkArea.width > 0) and (tlNatW > WorkArea.width) then
        tlNatW := WorkArea.width;
      if (WorkArea.height > 0) and (tlNatH > WorkArea.height) then
        tlNatH := WorkArea.height;
    end;
  end;

  PGtkWindow(TopLevel)^.resize(tlNatW, tlNatH);
  GdkWin^.resize(tlNatW, tlNatH);

end;

procedure Gtk3MenuPopupSizeFix(widget: PGtkWidget;
  alloc: PGtkAllocation; {%H-}data: gpointer); cdecl;
var
  MinW, NatW, MinH, NatH: gint;
begin
  if g_object_get_data(PGObject(widget), 'lcl-popup-szfixed') <> nil then
    exit;
  if not widget^.get_mapped then
    exit;

  widget^.get_preferred_width(@MinW, @NatW);
  widget^.get_preferred_height(@MinH, @NatH);

  //issue #42237, intercept allocation < natural width for dynamic menus
  if (NatH > 0) and (NatW > 0) and
     ((alloc^.height < NatH) or (alloc^.width < NatW)) then
  begin
    g_object_set_data(PGObject(widget), 'lcl-popup-szfixed', widget);
    g_idle_add(@Gtk3MenuPopupRepositionIdle, widget);
  end;
end;

procedure Gtk3MenuPopupHide(widget: PGtkWidget; {%H-}data: gpointer); cdecl;
begin
  g_object_set_data(PGObject(widget), 'lcl-popup-szfixed', nil);
end;

{ TGtk3WSMenuItem }

class procedure TGtk3WSMenuItem.AttachMenu(const AMenuItem: TMenuItem);
var
  MenuItem: TGtk3MenuItem;
  ParentMenuWidget, ContainerMenu: PGtkWidget;
  NewMenu: TGtk3Menu;
  AForm: TCustomForm;
begin
  if not AMenuItem.HandleAllocated then
  begin
    DebugLn('WARNING: TGtk3WSMenuItem.AttachMenu handle not allocated ',AMenuItem.Caption);
    exit;
  end;

  MenuItem := TGtk3MenuItem(AMenuItem.Handle);

  {$IFDEF GTK3DEBUGMENUS}
  DebugLn('*LCL* AMenuItem.Menu ',dbgsName(AMenuItem.Menu),' Parent ',dbgsName(AMenuItem.Parent),
    ' PtPt ',dbgsName(AMenuItem.Parent.Parent),
    ' PtMenu ',dbgsName(AMenuItem.GetParentMenu));

  DebugLn('Item: IsMenuBar ',dbgs(Gtk3IsMenuBar(MenuItem.Widget)),' IsMenu ',dbgs(Gtk3IsMenu(MenuItem.Widget)),
    ' IsMenuItem ',dbgs(Gtk3IsWidget(MenuItem.Widget)));
  {$ENDIF}

  if not(Assigned(AMenuItem.Parent)) and (AMenuItem.GetParentMenu is TPopupMenu) then
    ParentMenuWidget := TGtk3Menu(AMenuItem.GetParentMenu.Handle).Widget
  else
    ParentMenuWidget := MenuItem.Widget^.get_parent;
  {$IFDEF GTK3DEBUGMENUS}
  DebugLn('Parent: IsMenuBar ',dbgs(Gtk3IsMenuBar(ParentMenuWidget)),' IsMenu ',dbgs(Gtk3IsMenu(ParentMenuWidget)),
  ' IsMenuItem ',dbgs(Gtk3IsWidget(ParentMenuWidget)));
  {$ENDIF}

  if not Gtk3IsWidget(ParentMenuWidget) then
  begin
    ParentMenuWidget := TGtk3Widget(AMenuItem.Parent.Handle).Widget;
  end;

  if (not AMenuItem.Parent.HasParent) and (AMenuItem.GetParentMenu is TMainMenu) then
  begin
    AForm := TCustomForm(AMenuItem.GetParentMenu.Parent);
    PGtkMenuShell(TGtk3Window(AForm.Handle).GetMenuBar)^.insert(PGtkMenuItem(MenuItem.Widget), AMenuItem.MenuVisibleIndex);
  end else
  begin
    if Gtk3IsMenu(ParentMenuWidget) then
      ContainerMenu := ParentMenuWidget
    else
    begin
      {$IFDEF GTK3DEBUGMENUS}
      DebugLn('ParentMenuWidget ',dbgs(Gtk3IsWidget(ParentMenuWidget)));
      {$ENDIF}
      if not Gtk3IsWidget(ParentMenuWidget) then
        ParentMenuWidget := MenuItem.Widget;
      if g_object_get_data(ParentMenuWidget, 'ContainerMenu') <> nil then
        ContainerMenu := PGtkWidget(g_object_get_data(ParentMenuWidget,
                                  'ContainerMenu'))
      else
        ContainerMenu := nil;
    end;

    if ContainerMenu = nil then
    begin
      if (AMenuItem.GetParentMenu is TPopupMenu) and (AMenuItem.Parent.Parent=nil) then
      begin
        ContainerMenu := TGtk3Widget(AMenuItem.GetParentMenu.Handle).Widget;
        g_object_set_data(PGObject(ContainerMenu), 'ContainerMenu',
          ContainerMenu);
      end else
      begin
        {$IFDEF GTK3DEBUGMENUS}
        DebugLn('Creating newMenuItem ...');
        {$ENDIF}
        ContainerMenu := TGtkMenu.new;
        g_object_set_data(ParentMenuWidget, 'ContainerMenu',
                            ContainerMenu);
        PGTKMenuItem(ParentMenuWidget)^.set_submenu(PGtkMenu(ContainerMenu));

        //issue #42237
        g_signal_connect_data(PGObject(ContainerMenu), 'size-allocate',
          TGCallback(@Gtk3MenuPopupSizeFix), nil, nil, [G_CONNECT_AFTER]);

        g_signal_connect_data(PGObject(ContainerMenu), 'hide',
          TGCallback(@Gtk3MenuPopupHide), nil, nil, G_CONNECT_DEFAULT);
      end;
    end;
    PGtkMenu(ContainerMenu)^.insert(MenuItem.Widget, AMenuItem.MenuVisibleIndex);
  end;
end;

class function TGtk3WSMenuItem.CreateHandle(const AMenuItem: TMenuItem): HMENU;
var
  AMenu: TGtk3Menu;
begin
  if ((not AMenuItem.Parent.HasParent) and (AMenuItem.GetParentMenu is TMainMenu)) then
  begin
    {$IFDEF GTK3DEBUGMENUS}
    DebugLn('******** CREATING TGtk3Menu ********** FORM ',dbgsName(AMenuItem.GetParentMenu.Owner));
    {$ENDIF}
    Result := HMENU(TGtk3MenuItem.Create(AMenuItem));
    // PGtkMenu(AMenu.Widget)^.insert(TGtk3MenuItem(Result).Widget, 0);
  end
  else
    Result := HMENU(TGtk3MenuItem.Create(AMenuItem));

  if AMenuItem.Visible then
    TGtk3MenuItem(Result).show;
end;

class procedure TGtk3WSMenuItem.DestroyHandle(const AMenuItem: TMenuItem);
begin
  { TODO: cleanup }
  TGtk3MenuItem(AMenuItem.Handle).Free;
end;

class procedure TGtk3WSMenuItem.SetCaption(const AMenuItem: TMenuItem;
  const ACaption: string);
begin
  if not WSCheckMenuItem(AMenuItem, 'SetCaption') then
    Exit;
  {$IFDEF GTK3DEBUGMENUS}
  DebugLn('TGtk3WSMenuItem.SetCaption ',ACaption);
  {$ENDIF}
  TGtk3MenuItem(AMenuItem.Handle).Caption := ACaption;
end;

class procedure TGtk3WSMenuItem.SetShortCut(const AMenuItem: TMenuItem;
  const ShortCutK1, ShortCutK2: TShortCut);
begin
  if not WSCheckMenuItem(AMenuItem, 'SetShortCut') then
    Exit;
  if AMenuItem.HandleAllocated then
    TGtk3MenuItem(AMenuItem.Handle).SetShortCut(ShortCutK1, ShortCutK2);
end;

class procedure TGtk3WSMenuItem.SetVisible(const AMenuItem: TMenuItem;
  const Visible: boolean);
var
  MenuItemWidget: TGtk3Widget;
begin
  if not WSCheckMenuItem(AMenuItem, 'SetVisible') then
    Exit;
  MenuItemWidget := TGtk3Widget(AMenuItem.Handle);
  if MenuItemWidget.Visible = Visible then
    Exit;
  MenuItemWidget.Visible := Visible;
end;

class function TGtk3WSMenuItem.SetCheck(const AMenuItem: TMenuItem;
  const Checked: boolean): boolean;
var
  Item: TGtk3MenuItem;
begin
  Result := False;
  if not WSCheckMenuItem(AMenuItem, 'SetCheck') then
    Exit;
  if not AMenuItem.HandleAllocated then
  begin
    AMenuItem.RecreateHandle;
    Result := True;
    Exit;
  end;
  Item := TGtk3MenuItem(AMenuItem.Handle);
  if AMenuItem.IsCheckItem and
     not Gtk3WidgetIsA(Item.Widget, gtk_check_menu_item_get_type) then
    Item.ReplaceWidget;
  Item.SetCheck(Checked);
  Result := True;
end;

class function TGtk3WSMenuItem.SetEnable(const AMenuItem: TMenuItem;
  const Enabled: boolean): boolean;
begin
  Result := False;
  if not WSCheckMenuItem(AMenuItem, 'SetEnable') then
    Exit;
  TGtk3Widget(AMenuItem.Handle).Enabled := Enabled and not AMenuItem.IsLine;
  Result := True;
end;

class function TGtk3WSMenuItem.SetRadioItem(const AMenuItem: TMenuItem;
  const RadioItem: boolean): boolean;
var
  Item: TGtk3MenuItem;
  IsAlreadyRadio: Boolean;
begin
  Result := True;
  if not AMenuItem.HandleAllocated then
  begin
    AMenuItem.RecreateHandle;
    Exit;
  end;
  Item := TGtk3MenuItem(AMenuItem.Handle);
  IsAlreadyRadio := Gtk3IsRadioMenuItem(PGObject(Item.Widget));
  if RadioItem = IsAlreadyRadio then
    Exit;
  Item.ReplaceWidget;
end;

class function TGtk3WSMenuItem.SetRightJustify(const AMenuItem: TMenuItem;
  const Justified: boolean): boolean;
var
  MenuItemWidget: PGtkMenuItem;
begin
  Result := False;
  if not WSCheckMenuItem(AMenuItem, 'SetRightJustify') then
    Exit;
  // this property does not exist in Gtk3 anymore (deprecated in 3.2).
  // MenuItemWidget := {%H-}PGtkMenuItem(AMenuItem.Handle);
  // gtk_menu_item_set_right_justified(MenuItemWidget, Justified);
  // gtk_widget_queue_resize(GTK_WIDGET(MenuItemWidget));
  Result := True;
end;

class procedure TGtk3WSMenuItem.UpdateMenuIcon(const AMenuItem: TMenuItem;
  const HasIcon: Boolean; const {%H-}AIcon: TBitmap);
var
  Item: TGtk3MenuItem;
begin
  if not WSCheckMenuItem(AMenuItem, 'UpdateMenuIcon') then
    Exit;
  if not AMenuItem.HandleAllocated then
  begin
    AMenuItem.RecreateHandle;
    Exit;
  end;
  Item := TGtk3MenuItem(AMenuItem.Handle);
  if HasIcon <> Gtk3WidgetIsA(Item.Widget, gtk_image_menu_item_get_type) then
    Item.ReplaceWidget;
end;

{ TGtk3WSMenu }

class function TGtk3WSMenu.CreateHandle(const AMenu: TMenu): HMENU;
var
  AParams: TCreateParams;
begin
  if (AMenu is TMainMenu) and (AMenu.Owner is TCustomForm) then
  begin
    {$IFDEF GTK3DEBUGMENUS}
    Debugln('** TGtk3WSMenu.CreateHandle AMenu ',dbgsName(AMenu),' USING MENUBAR OF FORM !');
    {$ENDIF}
    Result := HMENU(TGtk3MenuBar.Create(AMenu, TGtk3Window(TCustomForm(AMenu.Owner).Handle).GetMenuBar));
  end else
  begin
    {$IFDEF GTK3DEBUGMENUS}
    DebugLn('*#*#*#*#* TGtk3WSMenu.CreateHandle AMenu ',dbgsName(AMenu));
    {$ENDIF}
    Result := HMENU(TGtk3MenuBar.Create(AMenu, nil));
  end;
end;

class procedure TGtk3WSMenu.SetBiDiMode(const AMenu : TMenu;
  UseRightToLeftAlign, UseRightToLeftReading : Boolean);

const
  WidgetDirection : array[boolean] of TGtkTextDirection = (GTK_TEXT_DIR_LTR, GTK_TEXT_DIR_RTL);
  MenuDirection : array[Boolean] of TGtkPackDirection = (
    GTK_PACK_DIRECTION_LTR,
    GTK_PACK_DIRECTION_RTL);

procedure Switch(AMenuItem: TMenuItem; Flip: Boolean);
  var
    i: Integer;
  begin
    if Flip then
    begin
      if AMenuItem.HandleAllocated then
      with TGtk3Widget(AMenuItem.Handle).Widget^ do
      begin
        set_direction(WidgetDirection[UseRightToLeftReading]);
      end;
    end;
    for i := 0 to AMenuItem.Count -1 do
      Switch(AMenuItem[i], True);
  end;
begin
  Switch(AMenu.Items, False);

  if AMenu is TMainMenu then
  with PGtkMenuBar(TGtk3MenuBar(Amenu.Handle).Widget)^ do
  begin
    set_pack_direction(MenuDirection[UseRightToLeftAlign]);
    set_child_pack_direction(MenuDirection[UseRightToLeftAlign]);
  end;
end;

{ TGtk3WSPopupMenu }

procedure GtkWS_Popup(menu: PGtkMenu; X, Y: pgint; {%H-}push_in: pgboolean;
  AData: gPointer); cdecl;
var
  Requisition: TGtkRequisition;
  Alignment: TPopupAlignment;
  ScreenHeight: gint;
begin
  X^ := TGtk3Menu(TPopupMenu(AData).Handle).PopupPoint.X;
  Y^ := TGtk3Menu(TPopupMenu(AData).Handle).PopupPoint.Y;
end;

function gtkWSPopupDelayedClose(Data: Pointer): gboolean; cdecl;
var
  PopupMenu: TMenu absolute Data;
begin
  Result := False;
  if PopupMenu is TPopupMenu then
    TPopupMenu(PopupMenu).Close;
end;

procedure gtkWSPopupMenuDeactivate(widget: PGtkWidget; data: gPointer); cdecl;
begin
  if widget = MenuWidget then
    MenuWidget := nil;
  if data <> nil then
    g_idle_add(@gtkWSPopupDelayedClose, TGtk3Menu(data).MenuObject);
end;

function gtkWSPopupMenuButtonPress(widget: PGtkWidget; event: PGdkEventButton;
  data: gPointer): gboolean; cdecl;
var
  EventWidget, W: PGtkWidget;
begin
  Result := False;
  if event = nil then
    exit;
  if not widget^.get_visible then
    exit;
  EventWidget := gtk_get_event_widget(PGdkEvent(event));
  if EventWidget = nil then
    exit;
  W := EventWidget;
  while W <> nil do
  begin
    if W = widget then
      exit;
    if Gtk3IsMenu(PGObject(W)) or Gtk3IsMenuBar(PGObject(W)) then
      W := PGtkWidget(gtk_menu_shell_get_parent_shell(PGtkMenuShell(W)))
    else
      W := gtk_widget_get_parent(W);
  end;
  gtk_menu_popdown(PGtkMenu(widget));
end;

procedure gtkWSPopupMenuGrabNotify(widget: PGtkWidget; was_grabbed: gboolean;
  data: gPointer); cdecl;
var
  NewGrab: PGtkWidget;
  Shell: PGtkMenuShell;
begin
  if was_grabbed then
    exit;
  if not widget^.get_visible then
    exit;
  NewGrab := gtk_grab_get_current;
  if (NewGrab <> nil) and Gtk3IsMenu(PGObject(NewGrab)) then
  begin
    Shell := PGtkMenuShell(NewGrab);
    while Shell <> nil do
    begin
      if PGtkWidget(Shell) = widget then
        exit;
      Shell := PGtkMenuShell(gtk_menu_shell_get_parent_shell(Shell));
    end;
  end;
  gtk_menu_popdown(PGtkMenu(widget));
end;

class function TGtk3WSPopupMenu.CreateHandle(const AMenu: TMenu): HMENU;
begin
  {$IFDEF GTK3DEBUGMENUS}
  DebugLn('****** TGtk3WSPopupMenu.CreateHandle ******');
  {$ENDIF}
  Result := HMENU(TGtk3Menu.Create(AMenu, nil));
  g_signal_connect_data(TGtk3Menu(Result).Widget,'deactivate',
    TGCallback(@gtkWSPopupMenuDeactivate), TGtk3Menu(Result), nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(PGObject(TGtk3Menu(Result).Widget), 'grab-notify',
    TGCallback(@gtkWSPopupMenuGrabNotify), nil, nil, [G_CONNECT_AFTER]);
  g_signal_connect_data(PGObject(TGtk3Menu(Result).Widget), 'button-press-event',
    TGCallback(@gtkWSPopupMenuButtonPress), nil, nil, []);
end;

class procedure TGtk3WSPopupMenu.Popup(const APopupMenu: TPopupMenu; const X,
  Y: integer);

  procedure SynthesizeRelease(ATarget: TWinControl; AMsgId: Cardinal);
  var
    Msg: TLMMouse;
    Pt: TPoint;
  begin
    Pt := ATarget.ScreenToClient(Point(X, Y));
    FillChar(Msg{%H-}, SizeOf(Msg), 0);
    Msg.Msg := AMsgId;
    Msg.XPos := SmallInt(Pt.X);
    Msg.YPos := SmallInt(Pt.Y);
    LCLMessageGlue.DeliverMessage(ATarget, TLMessage(Msg));
  end;

var
  AProc: Pointer;
  ThisMenu: PGtkWidget;
  ALeftDown, ARightDown, AMiddleDown: Boolean;
  ASource: TComponent;
  ASourceWin: TWinControl;
begin
  TGtk3Menu(APopupMenu.Handle).PopupPoint := Point(X, Y);
  AProc := @GtkWS_Popup;

  ALeftDown := SmallInt(Gtk3WidgetSet.GetKeyState(VK_LBUTTON)) < 0;
  ARightDown := SmallInt(Gtk3WidgetSet.GetKeyState(VK_RBUTTON)) < 0;
  AMiddleDown := SmallInt(Gtk3WidgetSet.GetKeyState(VK_MBUTTON)) < 0;

  ASource := APopupMenu.PopupComponent;
  ASourceWin := nil;
  if ASource is TWinControl then
    ASourceWin := TWinControl(ASource);
  if (ASourceWin = nil) or not ASourceWin.HandleAllocated then
    ASourceWin := FindLCLWindow(Point(X, Y));

  {$IFDEF GTK3DEBUGMENUS}
  DebugLn('TGtk3WSPopupMenu.Popup X=',dbgs(X),' Y=',dbgs(Y));
  {$ENDIF}
  ThisMenu := TGtk3Menu(APopupMenu.Handle).Widget;
  MenuWidget := ThisMenu;
  PGtkMenu(ThisMenu)^.popup(nil, nil,
    TGtkMenuPositionFunc(AProc), APopupMenu, 0, gtk_get_current_event_time);

  while ThisMenu^.get_visible do
  begin
    try
      Application.ProcessMessages;
    except
      if Application.CaptureExceptions then
        Application.HandleException(APopupMenu)
      else
        raise;
    end;
    if Application.Terminated then break;
    if not ThisMenu^.get_visible then break;
    Application.Idle(False);
  end;

  if not (ALeftDown or ARightDown or AMiddleDown) then
    Exit;

  if (ASourceWin <> nil) and ASourceWin.HandleAllocated then
  begin
    if ARightDown then
      SynthesizeRelease(ASourceWin, LM_RBUTTONUP);
    if ALeftDown then
      SynthesizeRelease(ASourceWin, LM_LBUTTONUP);
    if AMiddleDown then
      SynthesizeRelease(ASourceWin, LM_MBUTTONUP);
  end;
end;

end.
