{ $Id$}
{
 *****************************************************************************
 *                             GtkWSCListBox.pp                              * 
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
unit GtkWSCListBox;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef gtk1}
  Gtk, gdk, Glib,
  {$endif}
  Classes, Math, Controls, CListBox, LCLType, LMessages,
  InterfaceBase, WSCListBox, WSLCLClasses,
  GtkInt, GtkDef, GtkProc, GtkWSControls;

type

  { TGtkWSCListBox }

  TGtkWSCListBox = class(TWSCListBox)
  private
  protected
  public
  {$IFDEF GTK1}
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  {$ENDIF}
  end;


implementation

{ TGtkWSCListBox }

{$IFDEF GTK1}
class procedure TGtkWSCListBox.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
  TGtkWidgetset(Widgetset).SetCallback(LM_SELCHANGE, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject);
end;

class function TGtkWSCListBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  ScrolledWnd: PGtkScrolledWindow absolute Widget;
  CListBox: TCListBox absolute AWinControl;

  CList: Pointer;
  n: Integer;
begin
  Widget := gtk_scrolled_window_new(nil, nil);
  GTK_WIDGET_UNSET_FLAGS(ScrolledWnd^.hscrollbar, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS(ScrolledWnd^.vscrollbar, GTK_CAN_FOCUS);
  gtk_scrolled_window_set_policy(ScrolledWnd,
                                 GTK_POLICY_AUTOMATIC,
                                 GTK_POLICY_AUTOMATIC);
  gtk_widget_show(Widget);

  CList := gtk_clist_new(CListBox.ListColumns);
  for n := 0 to CListBox.ListColumns - 1 do
    gtk_clist_set_column_width(CList, n, (Max(0, CListBox.Width-10)) div CListBox.ListColumns);

  gtk_scrolled_window_add_with_viewport(ScrolledWnd, CList);
  gtk_container_set_focus_vadjustment(CList,
                gtk_scrolled_window_get_vadjustment(ScrolledWnd));
  gtk_container_set_focus_hadjustment(Clist,
                gtk_scrolled_window_get_hadjustment(ScrolledWnd));
  gtk_widget_show(CList);

  WidgetInfo := CreateWidgetInfo(Widget, AWinControl, AParams);
  SetMainWidget(Widget, CList);
  WidgetInfo^.CoreWidget := CList;
  TGtkWidgetSet(WidgetSet).SetSelectionMode(AWinControl, Widget,
    CListBox.MultiSelect, CListBox.ExtendedSelect);
  SetCallbacks(Widget, WidgetInfo);
end;
{$ENDIF}

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCListBox, TGtkWSCListBox);
////////////////////////////////////////////////////
end.
