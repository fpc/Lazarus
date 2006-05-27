{ $Id$}
{
 *****************************************************************************
 *                             GtkWSCheckLst.pp                              * 
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
unit GtkWSCheckLst;

{$mode objfpc}{$H+}

interface

uses
  CheckLst, WSCheckLst, WSLCLClasses,
  {$IFDEF gtk2}
  glib2, gdk2pixbuf, gdk2, gtk2, Pango,
  {$ELSE}
  glib, gdk, gtk, {$Ifndef NoGdkPixbufLib}gdkpixbuf,{$EndIf} GtkFontCache,
  {$ENDIF}
  GtkInt, Classes, GTKWinApiWindow, gtkglobals, gtkproc;

type

  { TGtkWSCustomCheckListBox }

  TGtkWSCustomCheckListBox = class(TWSCustomCheckListBox)
  private
  protected
  public
    class function  GetChecked(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): boolean; override;
    class procedure SetChecked(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AChecked: boolean); override;
  end;


implementation

class function  TGtkWSCustomCheckListBox.GetChecked(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer): boolean;
var
  Widget      : PGtkWidget; // pointer to gtk-widget (local use when neccessary)
  ChildWidget : PGtkWidget; // generic pointer to a child gtk-widget
                            // (local use when neccessary)
  ListItem    : PGtkListItem;
begin
  Result := false;
  { Get the child in question of that index }
  Widget := GetWidgetInfo(Pointer(ACheckListBox.Handle),True)^.CoreWidget;
  ListItem := g_list_nth_data(PGtkList(Widget)^.children, AIndex);
  if ListItem <> nil then 
  begin
    ChildWidget := PPointer(PGTKBox(PGtkBin(ListItem)^.child)^.Children^.Data)^;
    if (ChildWidget <> nil)
      and gtk_toggle_button_get_active(PGTKToggleButton(ChildWidget))
    then Result := true;
  end;
end;

class procedure TGtkWSCustomCheckListBox.SetChecked(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AChecked: boolean);
var
  Widget, ChildWidget: PGtkWidget;
  ListItem: PGtkListItem;
begin
  Widget := GetWidgetInfo(Pointer(ACheckListBox.Handle), True)^.CoreWidget;
  ListItem := g_list_nth_data(PGtkList(Widget)^.children, AIndex);
  if ListItem <> nil then 
  begin
    ChildWidget := PPointer(PGTKBox(PGtkBin(ListItem)^.child)^.Children^.Data)^;
    if (ChildWidget <> nil)
    then gtk_toggle_button_set_active(PGTKToggleButton(ChildWidget), AChecked);
  end;
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomCheckListBox, TGtkWSCustomCheckListBox);
////////////////////////////////////////////////////
end.