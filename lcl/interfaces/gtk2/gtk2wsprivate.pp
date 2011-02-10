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
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit Gtk2WSPrivate;
{$mode objfpc}{$H+}

interface

uses
  // libs
  Gtk2, Glib2, Gdk2,
  // LCL
  LCLType, LMessages, LCLProc, Controls, Classes, SysUtils, Forms,
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
    class procedure SetDefaultCursor(AInfo: PWidgetInfo); virtual;
  end;
  TGtkPrivateWidgetClass = class of TGtkPrivateWidget;

  { TGtkPrivateEntry }
  { Private class for gtkentries (text fields) }

  TGtkPrivateEntry = class(TGtkPrivateWidget)
  private
  protected
  public
    class procedure SetDefaultCursor(AInfo: PWidgetInfo); override;
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
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
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


function GetWidgetWithWindow(const AHandle: THandle): PGtkWidget;
procedure SetWindowCursor(AWindow: PGdkWindow; ACursor: HCursor; ARecursive: Boolean);
procedure SetCursorForWindowsWithInfo(AWindow: PGdkWindow; AInfo: PWidgetInfo);

implementation

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

  ScrollWidget := Pointer(AWinControl.Handle);
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

  Widget := Pointer(AWinControl.Handle);
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
  Params:  AWindow : PGDkWindow, ACursor: HCursor, ARecursive: Boolean
  Returns: Nothing

  Sets the cursor for a window (or recursively for window with children)
 ------------------------------------------------------------------------------}
procedure SetWindowCursor(AWindow: PGdkWindow; ACursor: HCursor; ARecursive: Boolean);
var
  Cursor: PGdkCursor;

  procedure SetCursorRecursive(AWindow: PGdkWindow);
  var
    ChildWindows, ListEntry: PGList;
  begin
    gdk_window_set_cursor(AWindow, Cursor);

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
  Cursor := PGdkCursor(ACursor);
  if Cursor = nil then Exit;
  if ARecursive
  then SetCursorRecursive(AWindow)
  else gdk_window_set_cursor(AWindow, Cursor);
end;

// Helper functions

function GetWidgetWithWindow(const AHandle: THandle): PGtkWidget;
var
  Children: PGList;
begin
  Result := PGTKWidget(PtrUInt(AHandle));
  while (Result <> nil) and GTK_WIDGET_NO_WINDOW(Result)
  and GtkWidgetIsA(Result,gtk_container_get_type) do
  begin
    Children := gtk_container_children(PGtkContainer(Result));
    if Children = nil
    then Result := nil
    else Result := Children^.Data;
  end;
end;

procedure SetCursorForWindowsWithInfo(AWindow: PGdkWindow; AInfo: PWidgetInfo);
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
      Info := GetWidgetInfo(PGtkWidget(Data), False);
      if Info = AInfo then
        gdk_window_set_cursor(AWindow, Cursor);
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
  Cursor := PGdkCursor(AInfo^.ControlCursor);
  if Cursor = nil then Exit;
  SetCursorRecursive(AWindow);
end;

end.
  
