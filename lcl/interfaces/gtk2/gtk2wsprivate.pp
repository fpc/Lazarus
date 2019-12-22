{ $Id$ }
{
                 ------------------------------------------
                 gtk2wsprivate.pp  -  Gtk2 internal classes
                 ------------------------------------------

 @created(Thu Feb 1st WET 2007)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

 This unit contains the private classhierarchy for the gtk implemetations
 This hierarchy reflects (more or less) the gtk widget hierarchy

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit Gtk2WSPrivate;
{$mode objfpc}{$H+}

interface

uses
  // libs
  Gtk2, Glib2, Gdk2,
  Classes, SysUtils,
  // LCL
  LCLType, LMessages, LCLProc, Controls, Forms,
  // widgetset
  WSControls, WSLCLClasses, WSProc,
  // interface
  Gtk2Def, Gtk2Proc, Gtk2WSControls;


type
  { TGtkPrivate } // GTK1WS Legacy!
  { Generic base class, don't know if it is needed }

  TGtkPrivate = class(TWSPrivate)
  private
  protected
  public
  end;

  { TGtkPrivateWidget }
  { Private class for all gtk widgets }

  TGtkPrivateWidget = class(TGtkPrivate)
  private
  protected
  public
    class procedure SetZPosition(const AWinControl: TWinControl; const APosition: TWSZPosition); virtual;
    class procedure UpdateCursor(AInfo: PWidgetInfo); virtual;
  end;
  TGtkPrivateWidgetClass = class of TGtkPrivateWidget;

  { TGtkPrivateEntry }
  { Private class for gtkentries (text fields) }

  TGtkPrivateEntry = class(TGtkPrivateWidget)
  private
  protected
  public
  end;


  { TGtkPrivateContainer }
  { Private class for gtkcontainers }

  TGtkPrivateContainer = class(TGtkPrivateWidget)
  private
  protected
  public
  end;

  { TGtkPrivateBin }
  { Private class for gtkbins }

  TGtkPrivateBin = class(TGtkPrivateContainer)
  private
  protected
  public
  end;


  { TGtkWSScrollingPrivate }
  { we may want to use something  like a compund class }

  TGtkPrivateScrolling = class(TGtkPrivateContainer)
  private
  protected
  public
    class procedure SetZPosition(const AWinControl: TWinControl; const APosition: TWSZPosition); override;
  end;

  TGtkPrivateScrollingWinControl = class(TGtkPrivateScrolling)
  private
  protected
  public
    class procedure SetZPosition(const AWinControl: TWinControl; const APosition: TWSZPosition); override;
  end;
  { ------------------------------------}

  { TGtkPrivateWindow }
  { Private class for gtkwindows }

  TGtkPrivateWindow = class(TGtkPrivateBin)
  private
  protected
  public
  end;

  { TGtkPrivateDialog }
  { Private class for gtkdialogs }

  TGtkPrivateDialog = class(TGtkPrivateWindow)
  private
  protected
  public
  end;

  { TGtkPrivateButton }
  { Private class for gtkbuttons }

  TGtkPrivateButton = class(TGtkPrivateBin)
  private
  protected
  public
  end;

  { TGtkPrivateList }
  { Private class for gtklists }

  TGtkPrivateListClass = class of TGtkPrivateList;
  TGtkPrivateList = class(TGtkPrivateScrolling)
  private
  protected
  public
    class procedure SetCallbacks(const {%H-}AGtkWidget: PGtkWidget; const {%H-}AWidgetInfo: PWidgetInfo); virtual;
  end;

  { TGtkPrivateNotebook }
  { Private class for gtknotebooks }

  TGtkPrivateNotebook = class(TGtkPrivateBin)
  private
  protected
  public
  end;

  { TGtkPrivatePaned }
  { Private class for gtkpaned }
  TGtkPrivatePaned = class(TGtkPrivateContainer)
  private
  protected
  public
    class procedure UpdateCursor(AInfo: PWidgetInfo); override;
  end;


  { TGtk2PrivateWidget }
  { Private class for gtkwidgets }

  TGtk2PrivateWidget = class(TGtkPrivateWidget)
  private
  protected
  public
  end;
  
  
  { TGtk2PrivateContainer }
  { Private class for gtkcontainers }

  TGtk2PrivateContainer = class(TGtkPrivateContainer)
  private
  protected
  public
  end;


  { TGtk2PrivateBin }
  { Private class for gtkbins }

  TGtk2PrivateBin = class(TGtkPrivateBin)
  private
  protected
  public
  end;


  { TGtk2PrivateWindow }
  { Private class for gtkwindows }

  TGtk2PrivateWindow = class(TGtkPrivateWindow)
  private
  protected
  public
  end;


  { TGtk2PrivateDialog }
  { Private class for gtkdialogs }

  TGtk2PrivateDialog = class(TGtkPrivateDialog)
  private
  protected
  public
  end;


  { TGtk2PrivateButton }
  { Private class for gtkbuttons }

  TGtk2PrivateButton = class(TGtkPrivateButton)
  private
  protected
  public
    class procedure UpdateCursor(AInfo: PWidgetInfo); override;
  end;
  
  { TGtk2PrivateList }
  { Private class for gtklists }

  TGtk2PrivateList = class(TGtkPrivateList)
  private
  protected
  public
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); override;
  end;

  { TGtk2PrivateMemo }
  { Private class for gtkmemos }

  TGtk2PrivateMemo = class(TGtkPrivateScrolling)
  private
  protected
  public
    class procedure UpdateCursor(AInfo: PWidgetInfo); override;
  end;

  { TGtk2PrivateNotebook }
  { Private class for gtknotebooks }

  TGtk2PrivateNotebook = class(TGtkPrivateNotebook)
  private
  protected
  public
    class procedure UpdateCursor(AInfo: PWidgetInfo); override;
  end;
  
  { TGtk2PrivatePaned }

  TGtk2PrivatePaned = class(TGtkPrivatePaned)
  private
  protected
  public
  end;


function GetWidgetWithWindow(const AHandle: HWND): PGtkWidget;
procedure SetWindowCursor(AWindow: PGdkWindow; ACursor: HCursor;
  ARecursive: Boolean; ASetDefault: Boolean);
procedure SetCursorForWindowsWithInfo(AWindow: PGdkWindow; AInfo: PWidgetInfo;
  ASetDefault: Boolean);
procedure SetGlobalCursor(Cursor: HCURSOR);

implementation

uses
  Gtk2Extra;

{$I Gtk2PrivateWidget.inc}
{$I Gtk2PrivateList.inc}

{ TGtkPrivateScrolling }
{ temp class to keep things working }

class procedure TGtkPrivateScrolling.SetZPosition(const AWinControl: TWinControl; const APosition: TWSZPosition);
var
  ScrollWidget: PGtkScrolledWindow;
//  WidgetInfo: PWidgetInfo;
  Widget: PGtkWidget;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetZPosition')
  then Exit;

  ScrollWidget := {%H-}Pointer(AWinControl.Handle);
//  WidgetInfo := GetWidgetInfo(ScrollWidget);
  // Some controls have viewports, so we get the first window.
  Widget := GetWidgetWithWindow(AWinControl.Handle);

  case APosition of
    wszpBack:  begin
      //gdk_window_lower(WidgetInfo^.CoreWidget^.Window);
      gdk_window_lower(Widget^.Window);
      if ScrollWidget^.hscrollbar <> nil
      then gdk_window_lower(ScrollWidget^.hscrollbar^.Window);
      if ScrollWidget^.vscrollbar <> nil
      then gdk_window_lower(ScrollWidget^.vscrollbar^.Window);
    end;
    wszpFront: begin
      //gdk_window_raise(WidgetInfo^.CoreWidget^.Window);
      gdk_window_raise(Widget^.Window);
      if ScrollWidget^.hscrollbar <> nil
      then gdk_window_raise(ScrollWidget^.hscrollbar^.Window);
      if ScrollWidget^.vscrollbar <> nil
      then gdk_window_raise(ScrollWidget^.vscrollbar^.Window);
    end;
  end;
end;

{ TGtkPrivateScrollingWinControl }

class procedure TGtkPrivateScrollingWinControl.SetZPosition(
  const AWinControl: TWinControl; const APosition: TWSZPosition);
var
  Widget: PGtkWidget;
  ScrollWidget: PGtkScrolledWindow;
//  WidgetInfo: PWidgetInfo;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetZPosition')
  then Exit;

  //TODO: when all scrolling controls are "derived" from TGtkWSBaseScrollingWinControl
  //      retrieve scrollbars from WidgetInfo^.Userdata. In that case, the following
  //      code can be removed and a call to TGtkWSBaseScrollingWinControl.SetZPosition
  //      can be made. This is not possible now since we have a frame around us

  Widget := {%H-}Pointer(AWinControl.Handle);
  //  WidgetInfo := GetWidgetInfo(Widget);

  // Only do the scrollbars, leave the core to the default (we might have a viewport)
  TGtkPrivateWidget.SetZPosition(AWinControl, APosition);

  if GtkWidgetIsA(Widget, gtk_frame_get_type) then
    ScrollWidget := PGtkScrolledWindow(PGtkFrame(Widget)^.Bin.Child)
  else
  if GtkWidgetIsA(Widget, gtk_scrolled_window_get_type) then
    ScrollWidget := PGtkScrolledWindow(Widget)
  else
    ScrollWidget := nil;

  if ScrollWidget <> nil then
  begin
    case APosition of
      wszpBack:  begin
        // gdk_window_lower(WidgetInfo^.CoreWidget^.Window);
        if ScrollWidget^.hscrollbar <> nil then
        begin
          if GDK_IS_WINDOW(ScrollWidget^.hscrollbar^.Window) then
            gdk_window_lower(ScrollWidget^.hscrollbar^.Window);
        end;

        if ScrollWidget^.vscrollbar <> nil then
        begin
          if GDK_IS_WINDOW(ScrollWidget^.vscrollbar^.Window) then
            gdk_window_lower(ScrollWidget^.vscrollbar^.Window);
        end;
      end;
      wszpFront: begin
        // gdk_window_raise(WidgetInfo^.CoreWidget^.Window);
        if ScrollWidget^.hscrollbar <> nil then
        begin
          if GDK_IS_WINDOW(ScrollWidget^.hscrollbar^.Window) then
            gdk_window_raise(ScrollWidget^.hscrollbar^.Window);
        end;
        if ScrollWidget^.vscrollbar <> nil then
        begin
          if GDK_IS_WINDOW(ScrollWidget^.vscrollbar^.Window) then
            gdk_window_raise(ScrollWidget^.vscrollbar^.Window);
        end;
      end;
    end;
  end;
end;

{------------------------------------------------------------------------------
  procedure: SetWindowCursor
  Params:  AWindow : PGDkWindow, ACursor: PGdkCursor, ASetDefault: Boolean
  Returns: Nothing

  Sets the cursor for a window.
  Tries to avoid messing with the cursors of implicitly created
  child windows (e.g. headers in TListView) with the following logic:
  - If Cursor <> nil, saves the old cursor (if not already done or ASetDefault = true)
    before setting the new one.
  - If Cursor = nil, restores the old cursor (if not already done).

  Unfortunately gdk_window_get_cursor is only available from
  version 2.18, so it needs to be retrieved dynamically.
  If gdk_window_get_cursor is not available, the cursor is set
  according to LCL widget data.
  ------------------------------------------------------------------------------}
procedure SetWindowCursor(AWindow: PGdkWindow; Cursor: PGdkCursor; ASetDefault: Boolean);
var
  OldCursor: PGdkCursor;
  Data: gpointer;
  Info: PWidgetInfo;
begin
  Info := nil;
  gdk_window_get_user_data(AWindow, @Data);
  if (Data <> nil) and GTK_IS_WIDGET(Data) then
  begin
    Info := GetWidgetInfo(PGtkWidget(Data));
  end;
  if not Assigned(gdk_window_get_cursor) and (Info = nil)
  then Exit;
  if ASetDefault then //and ((Cursor <> nil) or ( <> nil)) then
  begin
    // Override any old default cursor
    g_object_steal_data(PGObject(AWindow), 'havesavedcursor'); // OK?
    g_object_steal_data(PGObject(AWindow), 'savedcursor');
    gdk_window_set_cursor(AWindow, Cursor);
    Exit;
  end;
  if Cursor <> nil then
  begin
    if Assigned(gdk_window_get_cursor)
    then OldCursor := gdk_window_get_cursor(AWindow)
    else OldCursor := {%H-}PGdkCursor(Info^.ControlCursor);
    // As OldCursor can be nil, use a separate key to indicate whether it
    // is stored.
    if ASetDefault or (g_object_get_data(PGObject(AWindow), 'havesavedcursor') = nil) then
    begin
      g_object_set_data(PGObject(AWindow), 'havesavedcursor', gpointer(1));
      g_object_set_data(PGObject(AWindow), 'savedcursor', gpointer(OldCursor));
    end;
    gdk_window_set_cursor(AWindow, Cursor);
  end else
  begin
    if g_object_steal_data(PGObject(AWindow), 'havesavedcursor') <> nil then
    begin
      Cursor := g_object_steal_data(PGObject(AWindow), 'savedcursor');
      gdk_window_set_cursor(AWindow, Cursor);
    end;
  end;
end;

{------------------------------------------------------------------------------
  procedure: SetWindowCursor
  Params:  AWindow : PGDkWindow, ACursor: HCursor, ARecursive: Boolean
  Returns: Nothing

  Sets the cursor for a window (or recursively for window with children)
 ------------------------------------------------------------------------------}
procedure SetWindowCursor(AWindow: PGdkWindow; ACursor: HCursor;
  ARecursive: Boolean; ASetDefault: Boolean);
var
  Cursor: PGdkCursor;

  procedure SetCursorRecursive(AWindow: PGdkWindow);
  var
    ChildWindows, ListEntry: PGList;
  begin
    SetWindowCursor(AWindow, Cursor, ASetDefault);

    ChildWindows := gdk_window_get_children(AWindow);

    ListEntry := ChildWindows;
    while ListEntry <> nil do
    begin
      SetCursorRecursive(PGdkWindow(ListEntry^.Data));
      ListEntry := ListEntry^.Next;
    end;
    g_list_free(ChildWindows);
  end;
begin
  Cursor := {%H-}PGdkCursor(ACursor);
  if ARecursive
  then SetCursorRecursive(AWindow)
  else SetWindowCursor(AWindow, Cursor, ASetDefault);
end;

// Helper functions

function GetWidgetWithWindow(const AHandle: HWND): PGtkWidget;
var
  Children: PGList;
begin
  Result := {%H-}PGTKWidget(PtrUInt(AHandle));
  while (Result <> nil) and GTK_WIDGET_NO_WINDOW(Result)
  and GtkWidgetIsA(Result,gtk_container_get_type) do
  begin
    Children := gtk_container_children(PGtkContainer(Result));
    if Children = nil
    then Result := nil
    else Result := Children^.Data;
  end;
end;

procedure SetCursorForWindowsWithInfo(AWindow: PGdkWindow; AInfo: PWidgetInfo;
  ASetDefault: Boolean);
var
  Cursor: PGdkCursor;
  Data: gpointer;
  Info: PWidgetInfo;

  procedure SetCursorRecursive(AWindow: PGdkWindow);
  var
    ChildWindows, ListEntry: PGList;
  begin
    gdk_window_get_user_data(AWindow, @Data);
    if (Data <> nil) and GTK_IS_WIDGET(Data) then
    begin
      Info := GetWidgetInfo(PGtkWidget(Data));
      if Info = AInfo then
        SetWindowCursor(AWindow, Cursor, ASetDefault);
    end;

    ChildWindows := gdk_window_get_children(AWindow);

    ListEntry := ChildWindows;
    while ListEntry <> nil do
    begin
      SetCursorRecursive(PGdkWindow(ListEntry^.Data));
      ListEntry := ListEntry^.Next;
    end;
    g_list_free(ChildWindows);
  end;
begin
  if AInfo = nil then Exit;
  Cursor := {%H-}PGdkCursor(AInfo^.ControlCursor);
  SetCursorRecursive(AWindow);
end;

{------------------------------------------------------------------------------
  procedure: SetGlobalCursor
  Params:  ACursor: HCursor
  Returns: Nothing

  Sets the cursor for all toplevel windows. Also sets the cursor for all child
  windows recursively provided gdk_get_window_cursor is available.
 ------------------------------------------------------------------------------}
procedure SetGlobalCursor(Cursor: HCURSOR);
var
  TopList, List: PGList;
begin
  TopList := gdk_window_get_toplevels;
  List := TopList;
  while List <> nil do
  begin
    if (List^.Data <> nil) then
      SetWindowCursor(PGDKWindow(List^.Data), Cursor,
        Assigned(gdk_window_get_cursor), False);
    list := g_list_next(list);
  end;

  if TopList <> nil then
    g_list_free(TopList);
end;


end.
  
