{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSStdCtrls.pp                             * 
 *                             -----------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit Gtk2WSStdCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Controls, Graphics,
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  StdCtrls,
////////////////////////////////////////////////////
  glib2, gdk2pixbuf, gdk2, gtk2, Pango,
  WSStdCtrls, WSLCLClasses, GtkWSStdCtrls, Gtk2Int, LCLType, GtkDef, LCLProc,
  GTKWinApiWindow, gtkglobals, gtkproc, InterfaceBase;

type

  { TGtk2WSScrollBar }

  TGtk2WSScrollBar = class(TGtkWSScrollBar)
  private
  protected
  public
  end;

  { TGtk2WSCustomGroupBox }

  TGtk2WSCustomGroupBox = class(TGtkWSCustomGroupBox)
  private
  protected
  public
  end;

  { TGtk2WSGroupBox }

  TGtk2WSGroupBox = class(TGtkWSGroupBox)
  private
  protected
  public
  end;

  { TGtk2WSCustomComboBox }

  TGtk2WSCustomComboBox = class(TGtkWSCustomComboBox)
  private
  protected
  public
  end;

  { TGtk2WSComboBox }

  TGtk2WSComboBox = class(TGtkWSComboBox)
  private
  protected
  public
  end;

  { TGtk2WSCustomListBox }

  TGtk2WSCustomListBox = class(TGtkWSCustomListBox)
  private
  protected
  public
    class function  GetItemIndex(const ACustomListBox: TCustomListBox): integer; override;
    class procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer); override;
    class function  GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean; override;
    class function  GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
  end;

  { TGtk2WSListBox }

  TGtk2WSListBox = class(TGtkWSListBox)
  private
  protected
  public
  end;

  { TGtk2WSCustomEdit }

  TGtk2WSCustomEdit = class(TGtkWSCustomEdit)
  private
  protected
  public
  end;

  { TGtk2WSCustomMemo }

  TGtk2WSCustomMemo = class(TGtkWSCustomMemo)
  private
  protected
  public
  end;

  { TGtk2WSEdit }

  TGtk2WSEdit = class(TGtkWSEdit)
  private
  protected
  public
  end;

  { TGtk2WSMemo }

  TGtk2WSMemo = class(TGtkWSMemo)
  private
  protected
  public
  end;

  { TGtk2WSCustomLabel }

  {
  TGtk2WSCustomLabel = class(TGtkWSCustomLabel)
  private
  protected
  public
  end;
  }
  { TGtk2WSLabel }

  {
  TGtk2WSLabel = class(TGtkWSLabel)
  private
  protected
  public
  end;
  }
  
  { TGtk2WSButtonControl }

  TGtk2WSButtonControl = class(TGtkWSButtonControl)
  private
  protected
  public
  end;

  { TGtk2WSCustomCheckBox }

  TGtk2WSCustomCheckBox = class(TGtkWSCustomCheckBox)
  private
  protected
  public
  end;

  { TGtk2WSCheckBox }

  TGtk2WSCheckBox = class(TGtkWSCheckBox)
  private
  protected
  public
  end;

  { TGtk2WSToggleBox }

  TGtk2WSToggleBox = class(TGtkWSToggleBox)
  private
  protected
  public
  end;

  { TGtk2WSRadioButton }

  TGtk2WSRadioButton = class(TGtkWSRadioButton)
  private
  protected
  public
  end;

  { TGtk2WSCustomStaticText }

  TGtk2WSCustomStaticText = class(TGtkWSCustomStaticText)
  private
  protected
  public
  end;

  { TGtk2WSStaticText }

  TGtk2WSStaticText = class(TGtkWSStaticText)
  private
  protected
  public
  end;


implementation

{ TGtk2WSCustomListBox }

function TGtk2WSCustomListBox.GetItemIndex(const ACustomListBox: TCustomListBox
  ): integer;
var
  Handle: HWND;
  Widget: PGtkWidget;
  TreeView: PGtkTreeView;
  Selection: PGtkTreeSelection;
  Model: PGtkTreeModel;
  ListModel: TGtkListStore;
  Iter: TGtkTreeIter;
  Path: PGtkTreePath;
begin
  Result := -1;
  Handle := ACustomListBox.Handle;
  if Handle<>0 then
  begin
    Widget:=GetWidgetInfo(Pointer(Handle),True)^.CoreWidget;
    if GtkWidgetIsA(Widget,gtk_tree_view_get_type) then begin
      TreeView := PGtkTreeView(Widget);
      Selection := Gtk_tree_view_get_selection(TreeView);
      Model := @ListModel;
      if gtk_tree_selection_get_selected(Selection, @Model, @Iter) then begin
        Path := gtk_tree_model_get_path(Model, @Iter);
        if Path <> nil then begin
          Result := gtk_tree_path_get_indices(Path)^;
          gtk_tree_path_free(Path);
        end;
      end;
    end;
  end;

end;

procedure TGtk2WSCustomListBox.SetItemIndex(
  const ACustomListBox: TCustomListBox; const AIndex: integer);
var
  Handle: HWND;
  Widget: PGtkWidget;
  Selection: PGtkTreeSelection;
begin
  Handle := ACustomListBox.Handle;
  if Handle<>0 then
  begin
    Widget:=GetWidgetInfo(Pointer(Handle),True)^.CoreWidget;
    if GtkWidgetIsA(Widget,gtk_tree_view_get_type) then begin
      Selection := Gtk_tree_view_get_selection(PGtkTreeView(Widget));
      if AIndex >= 0 then
      begin
        gtk_tree_selection_select_path(Selection, PGtkTreePath(@AIndex));
        //gtk_list_select_item(PGtkList(Widget), AIndex)
      end else
        gtk_tree_selection_unselect_all(Selection);
        //gtk_list_unselect_all(PGtkList(Widget));
    end else
      raise Exception.Create('');
  end;
end;

function TGtk2WSCustomListBox.GetSelected(const ACustomListBox: TCustomListBox;
  const AIndex: integer): boolean;
var
  Handle: HWND;
  Widget: PGtkWidget; // pointer to gtk-widget (local use when neccessary)
  Selection: PGtkTreeSelection;
  ListStoreModel: PGtkTreeModel;
  Item  : PGtkTreeIter;
begin
  Result := false;      { assume: nothing found }
  Handle := ACustomListBox.Handle;
  Widget:=GetWidgetInfo(Pointer(Handle),True)^.CoreWidget;
  ListStoreModel := gtk_tree_view_get_model(PGtkTreeView(Widget));
  Selection := gtk_tree_view_get_selection(PGtkTreeView(Widget));

  if gtk_tree_model_iter_nth_child(ListStoreModel, Item, nil, AIndex) then begin
    Result := gtk_tree_selection_iter_is_selected(Selection, Item);
  end;
end;

function TGtk2WSCustomListBox.GetStrings(const ACustomListBox: TCustomListBox
  ): TStrings;
var
  Widget: PGtkWidget;// pointer to gtk-widget
  Handle: HWND;
begin
  Handle := ACustomListBox.Handle;
  case ACustomListBox.fCompStyle of
    {csCListBox:
      begin
        Widget:= GetWidgetInfo(Pointer(Handle), True)^.CoreWidget;

        Result := TGtkCListStringList.Create(PGtkCList(Widget));
        if ACustomListBox is TCustomListBox then
          TGtkCListStringList(Result).Sorted :=
                                          TCustomListBox(ACustomListBox).Sorted;
      end;
    }
    csCheckListBox, csListBox:
      begin
        Widget := GetWidgetInfo(Pointer(Handle), True)^.CoreWidget;
        Result := TGtkListStoreStringList.Create(gtk_tree_view_get_model(PGtkTreeView(Widget)),
          Ord(ACustomListBox.fCompStyle = csCheckListBox) ,ACustomListBox);
        if ACustomListBox is TCustomListBox then
          TGtkListStoreStringList(Result).Sorted := ACustomListBox.Sorted;
      end;
  else
    raise Exception.Create('TGtk2WSCustomListBox.GetStrings');
  end;


end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TScrollBar, TGtk2WSScrollBar);
//  RegisterWSComponent(TCustomGroupBox, TGtk2WSCustomGroupBox);
//  RegisterWSComponent(TGroupBox, TGtk2WSGroupBox);
//  RegisterWSComponent(TCustomComboBox, TGtk2WSCustomComboBox);
//  RegisterWSComponent(TComboBox, TGtk2WSComboBox);
  RegisterWSComponent(TCustomListBox, TGtk2WSCustomListBox);
//  RegisterWSComponent(TListBox, TGtk2WSListBox);
//  RegisterWSComponent(TCustomEdit, TGtk2WSCustomEdit);
//  RegisterWSComponent(TCustomMemo, TGtk2WSCustomMemo);
//  RegisterWSComponent(TEdit, TGtk2WSEdit);
//  RegisterWSComponent(TMemo, TGtk2WSMemo);
//  RegisterWSComponent(TCustomLabel, TGtk2WSCustomLabel);
//  RegisterWSComponent(TLabel, TGtk2WSLabel);
//  RegisterWSComponent(TButtonControl, TGtk2WSButtonControl);
//  RegisterWSComponent(TCustomCheckBox, TGtk2WSCustomCheckBox);
//  RegisterWSComponent(TCheckBox, TGtk2WSCheckBox);
//  RegisterWSComponent(TCheckBox, TGtk2WSCheckBox);
//  RegisterWSComponent(TToggleBox, TGtk2WSToggleBox);
//  RegisterWSComponent(TRadioButton, TGtk2WSRadioButton);
//  RegisterWSComponent(TCustomStaticText, TGtk2WSCustomStaticText);
//  RegisterWSComponent(TStaticText, TGtk2WSStaticText);
////////////////////////////////////////////////////
end.
