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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
  CheckLst, StdCtrls, WSCheckLst, WSLCLClasses,
  {$IFDEF gtk2}
  glib2, gdk2pixbuf, gdk2, gtk2, Pango,
  {$ELSE}
  glib, gdk, gtk, gdkpixbuf, GtkFontCache,
  {$ENDIF}
  GtkInt, Classes, GTKWinApiWindow, gtkglobals, gtkproc;

type

  { TGtkWSCustomCheckListBox }

  TGtkWSCustomCheckListBox = class(TWSCustomCheckListBox)
  published
    class function  GetState(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): TCheckBoxState; override;
    class procedure SetState(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AState: TCheckBoxState); override;
  end;


implementation

class function TGtkWSCustomCheckListBox.GetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer
  ): TCheckBoxState;
var
  Widget     : PGtkWidget; // pointer to gtk-widget (local use when neccessary)
  ChildWidget: PGtkWidget; // generic pointer to a child gtk-widget
                            // (local use when neccessary)
  ListItem   : PGtkListItem;
begin
  Result := cbUnChecked;
  { Get the child in question of that index }
  Widget := GetWidgetInfo(Pointer(ACheckListBox.Handle),True)^.CoreWidget;
  ListItem := g_list_nth_data(PGtkList(Widget)^.children, AIndex);
  if ListItem <> nil then
  begin
    ChildWidget := PPointer(PGTKBox(PGtkBin(ListItem)^.child)^.Children^.Data)^;
    if (ChildWidget <> nil) then
    begin
      if (gtk_object_get_data(PgtkObject(ChildWidget), 'Grayed') <> nil) then
        Result:=cbGrayed
      else
      if gtk_toggle_button_get_active(PGTKToggleButton(ChildWidget)) then
        Result := cbChecked
      else
        Result := cbUnChecked;
    end;
  end;
end;

class procedure TGtkWSCustomCheckListBox.SetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AState: TCheckBoxState);
var
  Widget, ChildWidget: PGtkWidget;
  ListItem: PGtkListItem;
begin
  Widget := GetWidgetInfo(Pointer(ACheckListBox.Handle), True)^.CoreWidget;
  ListItem := g_list_nth_data(PGtkList(Widget)^.children, AIndex);
  if ListItem <> nil then
  begin
    ChildWidget := PPointer(PGTKBox(PGtkBin(ListItem)^.child)^.Children^.Data)^;
    if (ChildWidget <> nil) then
    begin
      LockOnChange(PGtkObject(ChildWidget), 1);
      if AState = cbGrayed then
        gtk_object_set_data(PGtkObject(ChildWidget), 'Grayed', ChildWidget)
      else
        gtk_object_set_data(PGtkObject(ChildWidget), 'Grayed', nil);
      gtk_toggle_button_set_active(PGtkToggleButton(ChildWidget), AState = cbChecked);
      LockOnChange(PGtkObject(ChildWidget), -1);
    end;
  end;
end;

end.
