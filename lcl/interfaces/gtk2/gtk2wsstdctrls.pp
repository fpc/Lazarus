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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  StdCtrls, LMessages,
////////////////////////////////////////////////////
  glib2,  gdk2, gtk2, Pango,
  WSStdCtrls, WSLCLClasses, GtkWSStdCtrls, Gtk2Int, LCLType, GtkDef, LCLProc,
  Gtk2CellRenderer, GTKWinApiWindow, gtkglobals, gtkproc, InterfaceBase;

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
    class procedure SetFont(const AWinControl: TWinControl; const AFont : tFont); override;
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
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  public
    class function  GetSelCount(const ACustomListBox: TCustomListBox): integer; override;
    class function  GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean; override;
    class function  GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
    class function  GetItemIndex(const ACustomListBox: TCustomListBox): integer; override;
    class function  GetTopIndex(const ACustomListBox: TCustomListBox): integer; override;
    class procedure SelectItem(const ACustomListBox: TCustomListBox; AnIndex: integer; ASelected: boolean); override;
    class procedure SetBorder(const ACustomListBox: TCustomListBox); override;
    class procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer); override;
    class procedure SetSelectionMode(const ACustomListBox: TCustomListBox; const AExtendedSelect,
                                     AMultiSelect: boolean); override;
    class procedure SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean); override;
    class procedure SetTopIndex(const ACustomListBox: TCustomListBox; const NewTopIndex: integer); override;
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
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
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function  GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetSelLength(const ACustomEdit: TCustomEdit): integer; override;
  end;

  { TGtk2WSCustomMemo }

  TGtk2WSCustomMemo = class(TGtkWSCustomMemo)
  private
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function  GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;
    class procedure SetColor(const AWinControl: TWinControl);override;
    class procedure SetFont(const AWinControl: TWinControl;const AFont : TFont);override;
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
    class function  RetrieveState(const ACustomCheckBox: TCustomCheckBox
                                  ): TCheckBoxState; override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox;
                             const NewState: TCheckBoxState); override;
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

{$DEFINE MEMOHEADER}
{$I gtk2memostrings.inc}
{$UNDEF MEMOHEADER}

implementation

uses GtkWSControls;

{$I gtk2memostrings.inc}

{ TGtk2WSCustomListBox }

procedure Gtk2WS_ListBoxChange(Selection: PGtkTreeSelection; WidgetInfo: PWidgetInfo); cdecl;
var
  Mess: TLMessage;
begin
  {$IFDEF EventTrace}
  EventTrace('Gtk2WS_ListBoxChange', WidgetInfo^.LCLObject);
  {$ENDIF}
  FillChar(Mess,SizeOf(Mess),0);
  Mess.msg := LM_SelChange;
  DeliverMessage(WidgetInfo^.LCLObject, Mess);
end;

class function TGtk2WSCustomListBox.GetItemIndex(
  const ACustomListBox: TCustomListBox): integer;
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

class function TGtk2WSCustomListBox.GetTopIndex(
  const ACustomListBox: TCustomListBox): integer;
begin
  Result:=inherited GetTopIndex(ACustomListBox);
end;

class procedure TGtk2WSCustomListBox.SelectItem(
  const ACustomListBox: TCustomListBox; AnIndex: integer; ASelected: boolean);
var
  Handle: HWND;
  Widget: PGtkWidget; // pointer to gtk-widget (local use when neccessary)
  Selection: PGtkTreeSelection;
  ListStoreModel: PGtkTreeModel;
  Iter  : TGtkTreeIter;
begin
  Handle := ACustomListBox.Handle;
  Widget:=GetWidgetInfo(Pointer(Handle),True)^.CoreWidget;
  ListStoreModel := gtk_tree_view_get_model(PGtkTreeView(Widget));
  Selection := gtk_tree_view_get_selection(PGtkTreeView(Widget));

  if gtk_tree_model_iter_nth_child(ListStoreModel, @Iter, nil, AnIndex) then
  begin
    case ASelected of
      True:
        if not gtk_tree_selection_iter_is_selected(Selection, @Iter) then
          gtk_tree_selection_select_iter(Selection, @Iter);
      False:
        if gtk_tree_selection_iter_is_selected(Selection, @Iter) then
          gtk_tree_selection_unselect_iter(Selection, @Iter);
    end;
  end;
end;

class procedure TGtk2WSCustomListBox.SetBorder(
  const ACustomListBox: TCustomListBox);
begin
  // TODO
  debugln('TGtk2WSCustomListBox.SetBorder TODO');
end;

class procedure TGtk2WSCustomListBox.SetItemIndex(
  const ACustomListBox: TCustomListBox; const AIndex: integer);
var
  Handle: HWND;
  Widget: PGtkWidget;
  ListStoreModel: PGtkTreeModel;
  Selection: PGtkTreeSelection;
  Iter: TGtkTreeIter;
begin
  Handle := ACustomListBox.Handle;
  if Handle=0 then exit;
  Widget:=GetWidgetInfo(Pointer(Handle),True)^.CoreWidget;
  if not GtkWidgetIsA(Widget,gtk_tree_view_get_type) then
    raise Exception.Create('');
  Selection := Gtk_tree_view_get_selection(PGtkTreeView(Widget));
  if AIndex >= 0 then begin
    ListStoreModel := gtk_tree_view_get_model(PGtkTreeView(Widget));
    if gtk_tree_model_iter_nth_child(ListStoreModel, @Iter, nil, AIndex) then begin
      gtk_tree_selection_select_iter(Selection, @Iter);
    end;
  end else
    gtk_tree_selection_unselect_all(Selection);
end;

class procedure TGtk2WSCustomListBox.SetSelectionMode(
  const ACustomListBox: TCustomListBox; const AExtendedSelect,
  AMultiSelect: boolean);
var
  Handle: HWND;
  Widget: PGtkWidget; // pointer to gtk-widget (local use when neccessary)
  Selection: PGtkTreeSelection;
begin
  Handle := ACustomListBox.Handle;
  Widget:=GetWidgetInfo(Pointer(Handle),True)^.CoreWidget;
  Selection := gtk_tree_view_get_selection(PGtkTreeView(Widget));

  case AMultiSelect of
    True : gtk_tree_selection_set_mode(Selection, GTK_SELECTION_MULTIPLE);
    False: gtk_tree_selection_set_mode(Selection, GTK_SELECTION_SINGLE);
    //GTK_SELECTION_NONE,
    //GTK_SELECTION_SINGLE,
    //GTK_SELECTION_BROWSE,
    //GTK_SELECTION_MULTIPLE
  end;
end;

class procedure TGtk2WSCustomListBox.SetSorted(const ACustomListBox: TCustomListBox;
  AList: TStrings; ASorted: boolean);
begin
  if AList is TGtkListStoreStringList then
    TGtkListStoreStringList(AList).Sorted := ASorted
  //else if AList is TGtkCListStringList then
  //  TGtkCListStringList(AList).Sorted := ASorted
  else
    raise Exception.Create('');
end;

class procedure TGtk2WSCustomListBox.SetTopIndex(
  const ACustomListBox: TCustomListBox; const NewTopIndex: integer);
var
  Widget: PGtkWidget;
  ListStoreModel: PGtkTreeModel;
  Iter: TGtkTreeIter;
  TreeView: PGtkTreeView;
  APath: Pointer;
begin
  Widget:=GetWidgetInfo(Pointer(ACustomListBox.Handle),True)^.CoreWidget;
  TreeView:=PGtkTreeView(Widget);
  ListStoreModel:=gtk_tree_view_get_model(TreeView);
  
  if not gtk_tree_model_iter_nth_child(ListStoreModel, @Iter, nil, NewTopIndex)
  then exit;

  APath := gtk_tree_model_get_path(ListStoreModel, @Iter);
  gtk_tree_view_scroll_to_cell(TreeView, APath, NULL, true, 0.0, 0.0);
  gtk_tree_path_free(APath);
end;

class function TGtk2WSCustomListBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  TempWidget: PGtkWidget;
  p: PGtkWidget;                 // ptr to the newly created GtkWidget
  liststore : PGtkListStore;
  Selection: PGtkTreeSelection;
  renderer : PGtkCellRenderer;
  column : PGtkTreeViewColumn;
  WidgetInfo: PWidgetInfo;
begin
  Result := TGtkWSBaseScrollingWinControl.CreateHandle(AWinControl,AParams);
  p:= PGtkWidget(Result);
  
  if Result = 0 then exit;
  
  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.hscrollbar, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.vscrollbar, GTK_CAN_FOCUS);
  gtk_scrolled_window_set_policy(PGtkScrolledWindow(p),
                                 GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_scrolled_window_set_shadow_type(PGtkScrolledWindow(p),GTK_SHADOW_IN);
  gtk_widget_show(p);

  liststore := gtk_list_store_new (2, [G_TYPE_STRING, G_TYPE_POINTER, nil]);

  TempWidget:= gtk_tree_view_new_with_model (GTK_TREE_MODEL (liststore));
  g_object_unref (G_OBJECT (liststore));

  renderer := LCLIntfCellRenderer_New();
  column := gtk_tree_view_column_new_with_attributes ('LISTITEMS', renderer,
                                                      ['text', 0, nil]);
  gtk_tree_view_append_column (GTK_TREE_VIEW (TempWidget), column);
  gtk_tree_view_column_set_clickable (GTK_TREE_VIEW_COLUMN (column), TRUE);

  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW (TempWidget), False);

  gtk_container_add(GTK_CONTAINER(p), TempWidget);
  gtk_widget_show(TempWidget);

  SetMainWidget(p, TempWidget);
  GetWidgetInfo(p, True)^.CoreWidget := TempWidget;

  Selection := gtk_tree_view_get_selection(PGtkTreeView(TempWidget));

  case TCustomListBox(AWinControl).MultiSelect of
    True : gtk_tree_selection_set_mode(Selection, GTK_SELECTION_MULTIPLE);
    False: gtk_tree_selection_set_mode(Selection, GTK_SELECTION_SINGLE);
  end;

  WidgetInfo := GetWidgetInfo(p, False);
  SetCallbacks(TempWidget, WidgetInfo);
end;

class procedure TGtk2WSCustomListBox.SetCallbacks(
  const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
var
  Selection: PGtkTreeSelection;
begin
  TGtkWSBaseScrollingWinControl.SetCallbacks(AGtkWidget,AWidgetInfo);

  Selection := gtk_tree_view_get_selection(PGtkTreeView(AWidgetInfo^.CoreWidget));
  SignalConnect(PGtkWidget(Selection), 'changed', @Gtk2WS_ListBoxChange, AWidgetInfo);
end;

class function TGtk2WSCustomListBox.GetSelCount(
  const ACustomListBox: TCustomListBox): integer;
var
  Handle: HWND;
  Widget: PGtkWidget; // pointer to gtk-widget (local use when neccessary)
  Selection: PGtkTreeSelection;
  ListStoreModel: PGtkTreeModel;
  Rows: PGList;
begin
  Result := 0;
  Handle := ACustomListBox.Handle;
  Widget:=GetWidgetInfo(Pointer(Handle),True)^.CoreWidget;
  Selection := gtk_tree_view_get_selection(PGtkTreeView(Widget));

  Rows := gtk_tree_selection_get_selected_rows(Selection, @ListStoreModel);
  Result := g_list_length(Rows);
  g_list_free(Rows);
end;

class function TGtk2WSCustomListBox.GetSelected(
  const ACustomListBox: TCustomListBox; const AIndex: integer): boolean;
var
  Handle: HWND;
  Widget: PGtkWidget; // pointer to gtk-widget (local use when neccessary)
  Selection: PGtkTreeSelection;
  ListStoreModel: PGtkTreeModel;
  Item  : TGtkTreeIter;
begin
  Result := false;      { assume: nothing found }
  Handle := ACustomListBox.Handle;
  Widget:=GetWidgetInfo(Pointer(Handle),True)^.CoreWidget;
  ListStoreModel := gtk_tree_view_get_model(PGtkTreeView(Widget));
  Selection := gtk_tree_view_get_selection(PGtkTreeView(Widget));

  if gtk_tree_model_iter_nth_child(ListStoreModel, @Item, nil, AIndex) then begin
    Result := gtk_tree_selection_iter_is_selected(Selection, @Item);
  end;
end;

class function TGtk2WSCustomListBox.GetStrings(
  const ACustomListBox: TCustomListBox): TStrings;
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
        Result := TGtkListStoreStringList.Create(
                                gtk_tree_view_get_model(PGtkTreeView(Widget)),
                                Ord(ACustomListBox.fCompStyle = csCheckListBox),
                                ACustomListBox);
        TGtkListStoreStringList(Result).Sorted := ACustomListBox.Sorted;
      end;
  else
    raise Exception.Create('TGtk2WSCustomListBox.GetStrings');
  end;
end;

{ TGtk2WSCustomCheckBox }

class function TGtk2WSCustomCheckBox.RetrieveState(
  const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
var
  ToggleButton: PGtkToggleButton;
begin
  ToggleButton:=PGtkToggleButton(ACustomCheckBox.Handle);
  if ACustomCheckBox.AllowGrayed
  and gtk_toggle_button_get_inconsistent(ToggleButton) then
    Result:=cbGrayed
  else if gtk_toggle_button_get_active(ToggleButton) then
    Result := cbChecked
  else
    Result := cbUnChecked;
end;

class procedure TGtk2WSCustomCheckBox.SetState(
  const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
var
  GtkObject: PGtkObject;
  ToggleButton: PGtkToggleButton;
begin
  //debugln('TGtk2WSCustomCheckBox.SetState A ',DbgSName(ACustomCheckBox),' State=',dbgs(ord(ACustomCheckBox.State)));
  GtkObject := PGtkObject(ACustomCheckBox.Handle);
  LockOnChange(GtkObject,1);
  ToggleButton:=PGtkToggleButton(GtkObject);
  gtk_toggle_button_set_active(ToggleButton, NewState=cbChecked);
  gtk_toggle_button_set_inconsistent(ToggleButton, NewState=cbGrayed);
  LockOnChange(GtkObject,-1);
end;

{$I gtk2wscustommemo.inc}

{ TGtk2WSCustomEdit }

class function TGtk2WSCustomEdit.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var

  p: PGtkWidget;                 // ptr to the newly created GtkWidget
  SetupProps : boolean;


begin
   SetupProps := false;
   p :=  gtk_entry_new();
   gtk_editable_set_editable (PGtkEditable(P), not TCustomEdit(AWinControl).ReadOnly);
   gtk_widget_show_all(P);
   Result := TLCLIntfHandle(P);
   if result = 0 then exit;
   gtk2WidgetSet.FinishComponentCreate(AWinControl, P, SetupProps);


end;


class function TGtk2WSCustomEdit.GetSelStart(const ACustomEdit: TCustomEdit
  ): integer;
var
  Entry: PGtkEntry;
begin
  Entry := PGtkEntry(ACustomEdit.Handle);
  Result :=  Min(Entry^.current_pos, Entry^.selection_bound);
end;

class function TGtk2WSCustomEdit.GetSelLength(const ACustomEdit: TCustomEdit
  ): integer;
var
  Entry: PGtkEntry;
begin
  Entry := PGtkEntry(ACustomEdit.Handle);
  Result :=  ABS(Entry^.current_pos - Entry^.selection_bound);
end;



class procedure TGtk2WSCustomComboBox.SetFont(const AWinControl: TWinControl;
  const AFont : TFont);
var
  AWidget: PGTKWidget;
  EntryWidget: PGtkWidget;
begin
  if not AWinControl.HandleAllocated then exit;
  if AFont.IsDefault then exit;

  AWidget:= PGtkWidget(AWinControl.Handle);
  EntryWidget:=PGtkCombo(AWidget)^.entry;

  if EntryWidget<>nil then begin
    Gtk2WidgetSet.SetWidgetColor(EntryWidget, AWinControl.font.color, clNone,
       [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED,GTK_STYLE_TEXT]);
    Gtk2WidgetSet.SetWidgetFont(EntryWidget, AFont);
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
  RegisterWSComponent(TCustomComboBox, TGtk2WSCustomComboBox);
//  RegisterWSComponent(TComboBox, TGtk2WSComboBox);
  RegisterWSComponent(TCustomListBox, TGtk2WSCustomListBox);
//  RegisterWSComponent(TListBox, TGtk2WSListBox);
  RegisterWSComponent(TCustomEdit, TGtk2WSCustomEdit);
  RegisterWSComponent(TCustomMemo, TGtk2WSCustomMemo);
//  RegisterWSComponent(TEdit, TGtk2WSEdit);
//  RegisterWSComponent(TMemo, TGtk2WSMemo);
//  RegisterWSComponent(TCustomLabel, TGtk2WSCustomLabel);
//  RegisterWSComponent(TLabel, TGtk2WSLabel);
//  RegisterWSComponent(TButtonControl, TGtk2WSButtonControl);
//  RegisterWSComponent(TCustomCheckBox, TGtk2WSCustomCheckBox);
  RegisterWSComponent(TCustomCheckBox, TGtk2WSCustomCheckBox);
//  RegisterWSComponent(TToggleBox, TGtk2WSToggleBox);
//  RegisterWSComponent(TRadioButton, TGtk2WSRadioButton);
//  RegisterWSComponent(TCustomStaticText, TGtk2WSCustomStaticText);
//  RegisterWSComponent(TStaticText, TGtk2WSStaticText);
////////////////////////////////////////////////////
end.
