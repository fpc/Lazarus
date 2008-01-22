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
{$PACKRECORDS c}
interface

uses
  Classes, SysUtils, Math, Controls, Graphics,
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  StdCtrls, LMessages, LCLType, LCLProc,
////////////////////////////////////////////////////
  glib2,  gdk2, gtk2, Pango,
  WSControls, WSProc, WSStdCtrls, WSLCLClasses, GtkWSStdCtrls, Gtk2Int, GtkDef,
  Gtk2CellRenderer, GTKWinApiWindow, GtkGlobals, GtkProc, InterfaceBase,
  GtkWSPrivate, Gtk2WSPrivate, GtkExtra;

type

  { !!! Both are used: TGtkComboBoxEntry (with entry) and TGtkComboBox (without entry),
           but not the old TGtkCombo !!! }

  PGtkComboBoxPrivate = ^TGtkComboBoxPrivate;
  TGtkComboBoxPrivate = record
    model: PGtkTreeModel;
    col_column,
    row_column: gint;
    wrap_width: gint;
    active_row: PGtkTreeRowReference;
    tree_view: PGtkWidget;
    column: PGtkTreeViewColumn;
    cell_view: PGtkWidget;
    cell_view_frame: PGtkWidget;
    button: PGtkwidget;
    box: PGtkWidget;
    arrow: PGtkWidget;
    serarator: PGtkWidget;
    popup_widget: PGtkWidget;
    popup_window: PGtkWidget;
    popup_frame: PGtkWidget;
    scrolled_window: PGtkwidget;
  end;
  
  
  
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
    class function GetDefaultClientRect(const AWinControl: TWinControl;
             const aLeft, aTop, aWidth, aHeight: integer; var aClientRect: TRect
             ): boolean; override;
  end;

  { TGtk2WSGroupBox }

  TGtk2WSGroupBox = class(TGtkWSGroupBox)
  private
  protected
  public
  end;

  { TGtk2WSCustomComboBox }
  { !!! Both are used: TGtkComboBoxEntry (with entry) and TGtkComboBox (without entry),
           but not the old TGtkCombo !!! }

  TGtk2WSCustomComboBox = class(TGtkWSCustomComboBox)
  private
  protected
    class procedure ReCreateCombo(const ACustomComboBox: TCustomComboBox; const AWithEntry: Boolean; const AWidgetInfo: PWidgetInfo); virtual;
    class procedure SetRenderer(const ACustomComboBox: TCustomComboBox; AWidget: PGtkWidget; AWidgetInfo: PWidgetInfo); virtual;
    class procedure SetCallbacks(const AWinControl: tWinControl; const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  public
    class function  GetSelStart(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetSelLength(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetItemIndex(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetMaxLength(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;

    class procedure SetArrowKeysTraverseList(const ACustomComboBox: TCustomComboBox;
                                             NewTraverseList: boolean); override;
    class procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); override;
    class procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer); override;
    class procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle); override;
    class procedure SetReadOnly(const ACustomComboBox: TCustomComboBox; NewReadOnly: boolean); override;

    class function  GetItems(const ACustomComboBox: TCustomComboBox): TStrings; override;
    class procedure Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont : tFont); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
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
    class function GetIndexAtY(const ACustomListBox: TCustomListBox; y: integer): integer; override;
    class function GetItemIndex(const ACustomListBox: TCustomListBox): integer; override;
    class function GetItemRect(const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect): boolean; override;
    class function GetSelCount(const ACustomListBox: TCustomListBox): integer; override;
    class function GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean; override;
    class function GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
    class function GetTopIndex(const ACustomListBox: TCustomListBox): integer; override;

    class procedure SelectItem(const ACustomListBox: TCustomListBox; AnIndex: integer; ASelected: boolean); override;
    class procedure SetBorder(const ACustomListBox: TCustomListBox); override;
    class procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer); override;
    class procedure SetSelectionMode(const ACustomListBox: TCustomListBox; const AExtendedSelect,
                                     AMultiSelect: boolean); override;
    class procedure SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean); override;
    class procedure SetTopIndex(const ACustomListBox: TCustomListBox; const NewTopIndex: integer); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont : TFont); override;
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
    class function GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function GetSelLength(const ACustomEdit: TCustomEdit): integer; override;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
  end;

  { TGtk2WSCustomMemo }

  TGtk2WSCustomMemo = class(TGtkWSCustomMemo)
  private
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function  GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetSelLength(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;
    class procedure SetAlignment(const ACustomMemo: TCustomMemo; const AAlignment: TAlignment); override;
    class procedure SetColor(const AWinControl: TWinControl);override;
    class procedure SetFont(const AWinControl: TWinControl;const AFont : TFont);override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetWantTabs(const ACustomMemo: TCustomMemo; const NewWantTabs: boolean); override;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
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

  { TGtk2WSButton }

  TGtk2WSButton = class(TWSButton)
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

function GetComboBoxEntry(Widget: PGtkWidget): PGtkEntry;

implementation

uses GtkWSControls, LCLMessageGlue;

function GetComboBoxEntry(Widget: PGtkWidget): PGtkEntry;
begin
  if GtkWidgetIsA(Widget, GTK_TYPE_COMBO_BOX_ENTRY) then
    Result := PGtkEntry(GTK_BIN(Widget)^.child)
  else
    Result := nil;
end;

{$I gtk2memostrings.inc}

{ TGtk2WSCustomListBox }

procedure StoreFirstSelectedPath(model:PGtkTreeModel; path:PGtkTreePath;
  iter:PGtkTreeIter; data:gpointer); cdecl;
begin
  //DebugLn(['StoreFirstSelectedPath ',PInteger(Data)^,' ',gtk_tree_path_get_indices(Path)^]);
  if PInteger(Data)^ < 0 then
    PInteger(Data)^ := gtk_tree_path_get_indices(Path)^;
end;

class function TGtk2WSCustomListBox.GetItemIndex(
  const ACustomListBox: TCustomListBox): integer;
var
  Widget: PGtkWidget;
  Path: PGtkTreePath;
  Column: PGtkTreeViewColumn;
begin
  Result := -1;
  if not WSCheckHandleAllocated(ACustomListBox, 'GetItemIndex') then
    Exit;
  Widget := GetWidgetInfo(Pointer(ACustomListBox.Handle), True)^.CoreWidget;
  if GtkWidgetIsA(Widget, gtk_tree_view_get_type) then
  begin
    gtk_tree_view_get_cursor(PGtkTreeView(Widget), Path, column);
    if Path <> nil then
      Result := gtk_tree_path_get_indices(Path)^
    else
      Result := -1;
  end;
end;

class function TGtk2WSCustomListBox.GetItemRect(
  const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect
  ): boolean;
begin
  Result := False;
  FillChar(ARect, SizeOf(ARect), 0);
  case ACustomListBox.fCompStyle of
  csListBox, csCheckListBox:
    begin
      // ToDo
    end;
  end;
end;

class function TGtk2WSCustomListBox.GetTopIndex(
  const ACustomListBox: TCustomListBox): integer;
begin
  Result := GetIndexAtY(ACustomListBox, 0);
end;

class procedure TGtk2WSCustomListBox.SelectItem(
  const ACustomListBox: TCustomListBox; AnIndex: integer; ASelected: boolean);
var
  Widget: PGtkWidget; // pointer to gtk-widget (local use when neccessary)
  Selection: PGtkTreeSelection;
  ListStoreModel: PGtkTreeModel;
  Iter  : TGtkTreeIter;
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'SelectItem') then
    Exit;
  Widget:=GetWidgetInfo(Pointer(ACustomListBox.Handle), True)^.CoreWidget;
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
  gtk_scrolled_window_set_shadow_type(PGtkScrolledWindow(ACustomListBox.Handle),
    BorderStyleShadowMap[ACustomListBox.BorderStyle]);
end;

class procedure TGtk2WSCustomListBox.SetItemIndex(
  const ACustomListBox: TCustomListBox; const AIndex: integer);
var
  Widget: PGtkWidget;
  ListStoreModel: PGtkTreeModel;
  Selection: PGtkTreeSelection;
  Path: PGtkTreePath;
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'SetItemIndex') then
    Exit;
  Widget := GetWidgetInfo(Pointer(ACustomListBox.Handle), True)^.CoreWidget;
  if not GtkWidgetIsA(Widget, gtk_tree_view_get_type) then
    raise Exception.Create('');

  ListStoreModel := gtk_tree_view_get_model(PGtkTreeView(Widget));
  Selection := gtk_tree_view_get_selection(PGtkTreeView(Widget));
  
  if gtk_tree_row_reference_valid(PGtkTreeView(Widget)^.priv^.cursor) then
  begin
    gtk_tree_row_reference_free(PGtkTreeView(Widget)^.priv^.cursor);
    PGtkTreeView(Widget)^.priv^.cursor := nil;
  end;

  if (gtk_tree_selection_get_mode(Selection) <> GTK_SELECTION_SINGLE) and (AIndex < 0) then
    Path := gtk_tree_path_new_first
  else
    Path := gtk_tree_path_new_from_indices(AIndex, [-1]);

  PGtkTreeView(Widget)^.priv^.cursor := gtk_tree_row_reference_new_proxy(G_OBJECT(Widget), ListStoreModel, Path);
  gtk_tree_path_free(Path);
end;

class procedure TGtk2WSCustomListBox.SetSelectionMode(
  const ACustomListBox: TCustomListBox; const AExtendedSelect,
  AMultiSelect: boolean);
var
  Widget: PGtkWidget; // pointer to gtk-widget (local use when neccessary)
  Selection: PGtkTreeSelection;
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'SetSelectionMode') then
    Exit;
  Widget := GetWidgetInfo(Pointer(ACustomListBox.Handle),True)^.CoreWidget;
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
  if not WSCheckHandleAllocated(ACustomListBox, 'SetTopIndex') then
    Exit;
  Widget := GetWidgetInfo(Pointer(ACustomListBox.Handle), True)^.CoreWidget;
  TreeView := PGtkTreeView(Widget);
  ListStoreModel := gtk_tree_view_get_model(TreeView);
  
  if not gtk_tree_model_iter_nth_child(ListStoreModel, @Iter, nil, NewTopIndex)
  then exit;

  APath := gtk_tree_model_get_path(ListStoreModel, @Iter);
  gtk_tree_view_scroll_to_cell(TreeView, APath, NULL, true, 0.0, 0.0);
  gtk_tree_path_free(APath);
end;

class procedure TGtk2WSCustomListBox.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  Widget: PGtkWidget;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetFont') then
    Exit;
  Widget := GetWidgetInfo(Pointer(AWinControl.Handle), True)^.CoreWidget;

  Gtk2WidgetSet.SetWidgetColor(Widget, AWinControl.Font.Color, clNone,
       [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED,
        GTK_STYLE_TEXT]);
  Gtk2WidgetSet.SetWidgetFont(Widget, AFont);
end;

class function TGtk2WSCustomListBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  TVWidget: PGtkWidget;
  p: PGtkWidget;                 // ptr to the newly created GtkWidget
  liststore : PGtkListStore;
  Selection: PGtkTreeSelection;
  renderer : PGtkCellRenderer;
  column : PGtkTreeViewColumn;
  WidgetInfo: PWidgetInfo;
begin
  Result := TGtkWSBaseScrollingWinControl.CreateHandle(AWinControl, AParams);
  p := PGtkWidget(Result);
  
  if Result = 0 then exit;
  {$IFDEF DebugLCLComponents}
  // already called by inherited: DebugGtkWidgets.MarkCreated(p,dbgsName(AWinControl));
  {$ENDIF}

  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.hscrollbar, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.vscrollbar, GTK_CAN_FOCUS);
  gtk_scrolled_window_set_policy(PGtkScrolledWindow(p),
                                 GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_scrolled_window_set_shadow_type(PGtkScrolledWindow(p),
    BorderStyleShadowMap[TCustomListBox(AWinControl).BorderStyle]);
  gtk_widget_show(p);

  liststore := gtk_list_store_new (2, [G_TYPE_STRING, G_TYPE_POINTER, nil]);

  TVWidget:= gtk_tree_view_new_with_model (GTK_TREE_MODEL (liststore));
  g_object_unref (G_OBJECT (liststore));

  renderer := LCLIntfCellRenderer_New();
  column := gtk_tree_view_column_new_with_attributes ('LISTITEMS', renderer,
                                                      ['text', 0, nil]);
  gtk_tree_view_append_column (GTK_TREE_VIEW (TVWidget), column);
  gtk_tree_view_column_set_clickable (GTK_TREE_VIEW_COLUMN (column), TRUE);

  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW (TVWidget), False);

  gtk_container_add(GTK_CONTAINER(p), TVWidget);
  gtk_widget_show(TVWidget);

  SetMainWidget(p, TVWidget);
  GetWidgetInfo(p, True)^.CoreWidget := TVWidget;

  Selection := gtk_tree_view_get_selection(PGtkTreeView(TVWidget));

  case TCustomListBox(AWinControl).MultiSelect of
    True : gtk_tree_selection_set_mode(Selection, GTK_SELECTION_MULTIPLE);
    False: gtk_tree_selection_set_mode(Selection, GTK_SELECTION_SINGLE);
  end;

  WidgetInfo := GetWidgetInfo(p, False);
  
  TGtkPrivateListClass(WSPrivate).SetCallbacks(p, WidgetInfo);
end;

class function TGtk2WSCustomListBox.GetIndexAtY(
  const ACustomListBox: TCustomListBox; y: integer): integer;
var
  aTreeView: PGtkTreeView;
  aTreeColumn: PGtkTreeViewColumn;
  aTreePath: PGtkTreePath;
begin
  Result := -1;
  if not WSCheckHandleAllocated(ACustomListBox, 'GetIndexAtY') then
    Exit;
  case ACustomListBox.fCompStyle of
  csListBox, csCheckListBox:
    begin
      aTreeView :=
        GTK_TREE_VIEW(GetWidgetInfo(Pointer(ACustomListBox.Handle), True)^.CoreWidget);

      if gtk_tree_view_get_path_at_pos(aTreeView, 0, Y, aTreePath, aTreeColumn,
        nil, nil)
      then begin
        Result := gtk_tree_path_get_indices(aTreePath)[0];
        gtk_tree_path_free(aTreePath);
        exit;
      end;
    end;
  end;
end;

class function TGtk2WSCustomListBox.GetSelCount(
  const ACustomListBox: TCustomListBox): integer;
var
  Widget: PGtkWidget; // pointer to gtk-widget (local use when neccessary)
  Selection: PGtkTreeSelection;
  ListStoreModel: PGtkTreeModel;
  Rows: PGList;
begin
  Result := 0;
  if not WSCheckHandleAllocated(ACustomListBox, 'GetSelCount') then
    Exit;
  Widget := GetWidgetInfo(Pointer(ACustomListBox.Handle), True)^.CoreWidget;
  Selection := gtk_tree_view_get_selection(PGtkTreeView(Widget));

  Rows := gtk_tree_selection_get_selected_rows(Selection, @ListStoreModel);
  Result := g_list_length(Rows);
  g_list_free(Rows);
end;

class function TGtk2WSCustomListBox.GetSelected(
  const ACustomListBox: TCustomListBox; const AIndex: integer): boolean;
var
  Widget: PGtkWidget; // pointer to gtk-widget (local use when neccessary)
  Selection: PGtkTreeSelection;
  ListStoreModel: PGtkTreeModel;
  Item  : TGtkTreeIter;
begin
  Result := False;      { assume: nothing found }
  if not WSCheckHandleAllocated(ACustomListBox, 'GetSelected') then
    Exit;
  Widget := GetWidgetInfo(Pointer(ACustomListBox.Handle), True)^.CoreWidget;
  ListStoreModel := gtk_tree_view_get_model(PGtkTreeView(Widget));
  Selection := gtk_tree_view_get_selection(PGtkTreeView(Widget));

  if gtk_tree_model_iter_nth_child(ListStoreModel, @Item, nil, AIndex) then
  begin
    Result := gtk_tree_selection_iter_is_selected(Selection, @Item);
  end;
end;

class function TGtk2WSCustomListBox.GetStrings(
  const ACustomListBox: TCustomListBox): TStrings;
var
  Widget: PGtkWidget;// pointer to gtk-widget
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'GetStrings') then
    Exit;
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
        Widget := GetWidgetInfo(Pointer(ACustomListBox.Handle), True)^.CoreWidget;
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

class function TGtk2WSCustomEdit.GetSelStart(const ACustomEdit: TCustomEdit
  ): integer;
var
  Entry: PGtkEntry;
begin
  Result := 0;
  if not WSCheckHandleAllocated(ACustomEdit, 'GetSelStart') then
    Exit;
  Entry := PGtkEntry(ACustomEdit.Handle);
  Result :=  Min(Entry^.current_pos, Entry^.selection_bound);
end;

class function TGtk2WSCustomEdit.GetSelLength(const ACustomEdit: TCustomEdit
  ): integer;
var
  Entry: PGtkEntry;
begin
  Result := 0;
  if not WSCheckHandleAllocated(ACustomEdit, 'GetSelLength') then
    Exit;
  Entry := PGtkEntry(ACustomEdit.Handle);
  Result := ABS(Entry^.current_pos - Entry^.selection_bound);
end;

class procedure TGtk2WSCustomEdit.SetEchoMode(const ACustomEdit: TCustomEdit;
  NewMode: TEchoMode);
var
  Entry: PGtkEntry;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetEchoMode') then
    Exit;
  Entry := PGtkEntry(ACustomEdit.Handle);
  if NewMode in [emNone,emPassword] then begin
    gtk_entry_set_visibility(Entry,false);
    SetPasswordChar(ACustomEdit,ACustomEdit.PasswordChar);
  end else begin
    gtk_entry_set_visibility(Entry,true);
  end;
end;

class procedure TGtk2WSCustomEdit.SetPasswordChar(
  const ACustomEdit: TCustomEdit; NewChar: char);
var
  PWChar: Integer;
  Entry: PGtkEntry;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetPasswordChar') then
    Exit;
  Entry := PGtkEntry(ACustomEdit.Handle);
  if ACustomEdit.EchoMode=emNone then
    PWChar:=0
  else begin
    PWChar:=ord(ACustomEdit.PasswordChar);
    if (PWChar<192) or (PWChar=ord('*')) then
      PWChar:=9679;
  end;
  gtk_entry_set_invisible_char(Entry,PWChar);
end;

class procedure TGtk2WSCustomEdit.SetSelStart(const ACustomEdit: TCustomEdit;
  NewStart: integer);
var
  NewPos: Integer;
  Entry: PGtkEntry;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetSelStart') then
    Exit;
  Entry := PGtkEntry(ACustomEdit.Handle);
  if GetSelStart(ACustomEdit) = NewStart then exit;
  NewPos := Min(NewStart, Entry^.text_max_length);
  gtk_entry_set_position(Entry, NewPos);
end;

class procedure TGtk2WSCustomEdit.SetSelLength(
  const ACustomEdit: TCustomEdit; NewLength: integer);
var
  Entry: PGtkEntry;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetSelLength') then
    Exit;
  Entry := PGtkEntry(ACustomEdit.Handle);
  gtk_entry_select_region(Entry,
    Entry^.current_pos,
    Entry^.current_pos + NewLength);
end;

class procedure TGtk2WSCustomComboBox.ReCreateCombo(
  const ACustomComboBox: TCustomComboBox; const AWithEntry: Boolean;
  const AWidgetInfo: PWidgetInfo);
var
  ComboWidget: PGtkWidget;
  Model: PGtkTreeModel;
  Index: Integer;
  Text: String;
  Box: PGtkWidget;
  ItemList: TGtkListStoreStringList;
  LCLIndex: PLongint;
begin
  Box:=PGtkWidget(ACustomComboBox.Handle);
  ComboWidget := AWidgetInfo^.CoreWidget;
  
  // keep the model (increase ref count)
  Model := gtk_combo_box_get_model(PGtkComboBox(ComboWidget));
  g_object_ref(G_OBJECT(Model));
  // keep items
  ItemList := ACustomComboBox.Items as TGtkListStoreStringList;
  
  LCLIndex := gtk_object_get_data(GTK_OBJECT(ComboWidget), GtkComboLCLItemIndexTag);
  if not Assigned(LCLIndex) then begin
    //debugln('Gtk2WSCustomComboBox ReCreateCombo: LCLIndex unassigned!');
    LCLIndex := New(PLongint);
    LCLIndex^ := -1;
  end;
  
  // this should work but may not in all circumstances
  Index := -1;
  if AWithEntry = False then begin // the current widget HAS an entry
    GetText(ACustomComboBox, Text);
    Index := ACustomComboBox.Items.IndexOf(Text);
  end;
  if Index = -1 then Index := GetItemIndex(ACustomComboBox);

  gtk_object_set_data(PGtkObject(ComboWidget), GtkListItemLCLListTag,nil);
  gtk_object_set_data(PGtkObject(ComboWidget), GtkComboLCLItemIndexTag, nil);
  gtk_widget_destroy(ComboWidget);

  // create the new widget with the old model
  case AWithEntry of
    True : ComboWidget := gtk_combo_box_entry_new_with_model(Model, 0);
    False: ComboWidget := gtk_combo_box_new_with_model(Model);
  end;
  // undone the above increase of the ref count
  gtk_object_set_data(PGtkObject(ComboWidget),GtkListItemLCLListTag,ItemList);
  gtk_object_set_data(PGtkObject(ComboWidget), GtkComboLCLItemIndexTag, LCLIndex);
  g_object_unref (G_OBJECT(Model));

  SetMainWidget(Box, GTK_BIN(ComboWidget)^.child);
  AWidgetInfo^.CoreWidget := ComboWidget;
  gtk_object_set_data(Pointer(ComboWidget), 'widgetinfo', AWidgetInfo);
  
  SetItemIndex(ACustomComboBox, Index);
  
  if AWithEntry then begin
    SetMaxLength(ACustomComboBox, TComboBox(ACustomComboBox).MaxLength);
    SetReadOnly(ACustomComboBox, ACustomComboBox.ReadOnly);
  end;

  SetRenderer(ACustomComboBox, ComboWidget, AWidgetInfo);
  
  gtk_container_add(PGtkContainer(AWidgetInfo^.ClientWidget), ComboWidget);
  gtk_widget_show_all(AWidgetInfo^.ClientWidget);
  
  SetCallbacks(ACustomComboBox, Box, AWidgetInfo);
end;

class procedure TGtk2WSCustomComboBox.SetRenderer(
  const ACustomComboBox: TCustomComboBox; AWidget: PGtkWidget; AWidgetInfo: PWidgetInfo);
var
  renderer : PGtkCellRenderer;
begin
  renderer := LCLIntfCellRenderer_New();
  g_object_set_data(G_OBJECT(renderer), 'widgetinfo', AWidgetInfo);
  gtk_cell_layout_clear(PGtkCellLayout(AWidget));
  gtk_cell_layout_pack_start(PGtkCellLayout(AWidget), renderer, True);
  gtk_cell_layout_set_attributes(PGtkCellLayout(AWidget), renderer, ['text', 0, nil]);
end;

procedure GtkPopupShowCB(AMenu: PGtkMenuShell; WidgetInfo: PWidgetInfo); cdecl;
begin
  // let the LCL change the items on the fly:
  LCLSendDropDownMsg(TControl(WidgetInfo^.LCLObject));
end;

procedure GtkPopupHideCB(AMenu: PGtkMenuShell; WidgetInfo: PWidgetInfo); cdecl;
begin
  LCLSendCloseUpMsg(TControl(WidgetInfo^.LCLObject));
end;

function GtkPopupCloseUp(WidgetInfo: Pointer): gboolean; cdecl;
begin
  LCLSendCloseUpMsg(TControl(PWidgetInfo(WidgetInfo)^.LCLObject));
  Result := False;
end;

procedure GtkNotifyCB(AObject: PGObject; pspec: PGParamSpec; WidgetInfo: PWidgetInfo); cdecl;
var
  AValue: TGValue;
  AMenu: PGtkWidget;
begin
  if pspec^.name = 'popup-shown' then
  begin
    FillChar(AValue, SizeOf(AValue), 0); // fill by zeros
    g_value_init(@AValue, G_TYPE_BOOLEAN); // initialize for boolean
    g_object_get_property(AObject, pspec^.name, @AValue); // get property value
    if AValue.data[0].v_int = 0 then // if 0 = False then it is close up
      g_idle_add(@GtkPopupCloseUp, WidgetInfo)
    else // in other case it is drop down
    begin
      LCLSendDropDownMsg(TControl(WidgetInfo^.LCLObject));
      AMenu := PGtkComboBoxPrivate(PGtkComboBox(WidgetInfo^.CoreWidget)^.priv)^.popup_widget;
      gtk_menu_reposition(PGtkMenu(AMenu));
    end;
  end;
end;

procedure GtkChangedCB(AWidget: PGtkWidget; WidgetInfo: PWidgetInfo); cdecl;
var
  LCLIndex: PLongint;
  Index, GtkIndex: Integer;
begin
  if WidgetInfo^.UserData <> nil then Exit;
  LCLSendChangedMsg(TControl(WidgetInfo^.LCLObject));
  
  Index := -1;
  LCLIndex := gtk_object_get_data(GTK_OBJECT(WidgetInfo^.CoreWidget), GtkComboLCLItemIndexTag);
  if Assigned(LCLIndex) then
    Index := LCLIndex^
  else
    debugln('Gtk2WSCustomComboBox GtkChangedCB: LCLIndex unassigned!');

  GtkIndex := gtk_combo_box_get_active(GTK_COMBO_BOX(WidgetInfo^.CoreWidget));
  if Index <> GtkIndex then begin
    LCLSendSelectionChangedMsg(TControl(WidgetInfo^.LCLObject));
    if Assigned(LCLIndex) then
      LCLIndex^ := GtkIndex;
  end;
end;

{procedure GtkSelectedCB(AWidget: PGtkWidget; WidgetInfo: PWidgetInfo); cdecl;
begin
  if WidgetInfo^.UserData <> nil then Exit;
  LCLSendSelectionChangedMsg(TControl(WidgetInfo^.LCLObject));
end;}

class procedure TGtk2WSCustomComboBox.SetCallbacks(
  const AWinControl: TWinControl; const AWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
var
  AGtkObject: PGtkObject;
  AEntry: PGtkObject;
  AButton: PGtkObject;
  APrivate: PGtkComboBoxPrivate;
  AMenu: PGtkObject;
  BtnPressID: guint;
  HandlerID: guint;
  ComboWidget: PGtkComboBox;
  InputObject: PGtkObject;
begin
  ComboWidget:=PGtkComboBox(AWidgetInfo^.CoreWidget);
  AGtkObject := PGtkObject(AWidget);
  AEntry := PGtkObject(GetComboBoxEntry(PGtkWidget(ComboWidget)));
  APrivate := PGtkComboBoxPrivate(ComboWidget^.priv);
  AButton := PGtkObject(APrivate^.button);
  //DebugLn(['TGtk2WSCustomComboBox.SetCallbacks ',dbgsName(AWinControl),' AButton=',GetWidgetClassName(PGtkWidget(AButton)),' ComboWidget=',GetWidgetClassName(PGtkWidget(ComboWidget))]);

  // we have to remove the handler gtk sets up to get the mouse down messages
  BtnPressID := g_signal_lookup('button_press_event', GTK_TYPE_COMBO_BOX);
  if AButton<>nil then begin
    HandlerID := g_signal_handler_find(AButton, G_SIGNAL_MATCH_ID, BtnPressID, 0, nil, nil, nil);
    if HandlerID > 0 then begin
      g_signal_handler_disconnect(AButton, HandlerID);
    end;
  end;
  
  g_signal_connect(ComboWidget, 'changed', TGCallback(@GtkChangedCB), AWidgetInfo);

  // First the combo (or the entry)
  if gtk_is_combo_box_entry(ComboWidget) then begin
    InputObject:=AEntry;
  end else begin
    InputObject:=AGTKObject;
  end;
  Gtk2WidgetSet.SetCallbackDirect(LM_MOUSEMOVE, InputObject, AWinControl);
  Gtk2WidgetSet.SetCallbackDirect(LM_LBUTTONDOWN, InputObject, AWinControl);
  Gtk2WidgetSet.SetCallbackDirect(LM_LBUTTONUP, InputObject, AWinControl);
  Gtk2WidgetSet.SetCallbackDirect(LM_LBUTTONDBLCLK, InputObject, AWinControl);
  Gtk2WidgetSet.SetCallbackDirect(LM_RBUTTONDBLCLK, InputObject, AWinControl);
  Gtk2WidgetSet.SetCallbackDirect(LM_MBUTTONDBLCLK, InputObject, AWinControl);
  Gtk2WidgetSet.SetCallbackDirect(LM_RBUTTONDOWN, InputObject, AWinControl);
  Gtk2WidgetSet.SetCallbackDirect(LM_RBUTTONUP, InputObject, AWinControl);
  Gtk2WidgetSet.SetCallbackDirect(LM_MBUTTONDOWN, InputObject, AWinControl);
  Gtk2WidgetSet.SetCallbackDirect(LM_MBUTTONUP, InputObject, AWinControl);
  Gtk2WidgetSet.SetCallbackDirect(LM_MOUSEWHEEL, InputObject, AWinControl);
  Gtk2WidgetSet.SetCallbackDirect(LM_PAINT, InputObject, AWinControl);
  Gtk2WidgetSet.SetCallbackDirect(LM_FOCUS, InputObject, AWinControl);

  // And now the same for the Button in the combo
  if AButton<>nil then begin
    Gtk2WidgetSet.SetCallbackDirect(LM_MOUSEENTER, AButton, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_MOUSELEAVE, AButton, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_MOUSEMOVE, AButton, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_LBUTTONDOWN, AButton, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_LBUTTONUP, AButton, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_LBUTTONUP, AButton, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_RBUTTONDOWN, AButton, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_RBUTTONUP, AButton, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_MBUTTONDOWN, AButton, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_MBUTTONUP, AButton, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_MOUSEWHEEL, AButton, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_PAINT, AButton, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_FOCUS, AButton, AWinControl);
  end;
  
  // if we are a GtkComboBoxEntry
  if GtkWidgetIsA(PGtkWidget(AEntry), GTK_TYPE_ENTRY) then
  begin
    // Anything?
  end;

  AMenu := nil;
  if (APrivate^.popup_widget <> nil)
  and (GTK_IS_MENU(APrivate^.popup_widget)) then
    AMenu := GTK_OBJECT(APrivate^.popup_widget)
  else if (APrivate^.popup_window <> nil)
  and (GTK_IS_MENU(APrivate^.popup_window)) then
    AMenu := GTK_OBJECT(APrivate^.popup_window);

  if Assigned(AMenu) and (gtk_major_version = 2) and (gtk_minor_version < 10) then
  begin
    g_signal_connect(AMenu, 'show', G_CALLBACK(@GtkPopupShowCB), AWidgetInfo);
    g_signal_connect_after(AMenu, 'selection-done', G_CALLBACK(@GtkPopupHideCB), AWidgetInfo);
  end;

  if (gtk_major_version >= 2) and (gtk_minor_version >= 10) then
    g_signal_connect(ComboWidget, 'notify', TGCallback(@GtkNotifyCB), AWidgetInfo);

  
  // g_signal_connect(ComboWidget, 'set-focus-child', TGCallback(@GtkPopupShowCB), AWidgetInfo);
  g_object_set_data(G_OBJECT(AWidget), 'Menu', APrivate^.popup_widget);
end;

class function TGtk2WSCustomComboBox.GetSelStart(
  const ACustomComboBox: TCustomComboBox): integer;
var
  WidgetInfo: PWidgetInfo;
  Entry: PGtkEntry;
  AStart, AEnd: gint;
begin
  Result := 0;
  WidgetInfo := GetWidgetInfo(Pointer(ACustomComboBox.Handle));

  // if the combo is an editable ...
  Entry := GetComboBoxEntry(WidgetInfo^.CoreWidget);
  if Entry<>nil then begin
    if gtk_editable_get_selection_bounds(PGtkEditable(Entry), @AStart, @AEnd) = False then
      Result := gtk_editable_get_position(PGtkEditable(Entry))
    else
      Result := Min(AStart, AEnd);
  end;
end;

class function TGtk2WSCustomComboBox.GetSelLength(
  const ACustomComboBox: TCustomComboBox): integer;
var
  WidgetInfo: PWidgetInfo;
  Entry: PGtkEntry;
  AStart, AEnd: gint;
begin
  Result := 0;
  WidgetInfo := GetWidgetInfo(Pointer(ACustomComboBox.Handle));

  // if the combo is an editable ...
  Entry := GetComboBoxEntry(WidgetInfo^.CoreWidget);
  if Entry<>nil then begin
    if gtk_editable_get_selection_bounds(PGtkEditable(Entry), @AStart, @AEnd) = False then
      Exit(gtk_editable_get_position(PGtkEditable(Entry)));
    Result := ABS(AStart - AEnd);
  end;
end;

class function TGtk2WSCustomComboBox.GetItemIndex(
  const ACustomComboBox: TCustomComboBox): integer;
var
  WidgetInfo: PWidgetInfo;
begin
  WidgetInfo := GetWidgetInfo(Pointer(ACustomComboBox.Handle));
  
  Result := gtk_combo_box_get_active(PGtkComboBox(WidgetInfo^.CoreWidget));
end;

class function TGtk2WSCustomComboBox.GetMaxLength(
  const ACustomComboBox: TCustomComboBox): integer;
var
  WidgetInfo: PWidgetInfo;
  Entry: PGtkEntry;
begin
  WidgetInfo := GetWidgetInfo(Pointer(ACustomComboBox.Handle));

  // if the combo is an editable ...
  Entry := GetComboBoxEntry(WidgetInfo^.CoreWidget);
  if Entry<>nil then begin
    Result := gtk_entry_get_max_length(Entry);
  end
  else begin
     Result := integer(PtrUInt(g_object_get_data(PGObject(WidgetInfo^.CoreWidget), 'max-length')));
  end;
end;

class function TGtk2WSCustomComboBox.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
var
  WidgetInfo: PWidgetInfo;
  Entry: PGtkEntry;
  Index: Integer;
begin
  Result := True;
  WidgetInfo := GetWidgetInfo(Pointer(AWinControl.Handle));

  // if the combo is an editable ...
  Entry := GetComboBoxEntry(WidgetInfo^.CoreWidget);
  if Entry<>nil then begin
    AText := gtk_entry_get_text(Entry);
    exit;
  end;
  
  // if we are a fixed un-editable combo then ...
  Index := GetItemIndex(TCustomComboBox(AWinControl));
  if Index > -1 then  AText := TCustomComboBox(AWinControl).Items.Strings[Index]
  else Result := False;
end;

class procedure TGtk2WSCustomComboBox.SetArrowKeysTraverseList(
  const ACustomComboBox: TCustomComboBox; NewTraverseList: boolean);
begin
  // TODO
  // This is not an option that is available for this widget
  // we will have to eat the keystrokes to set this to false
end;

class procedure TGtk2WSCustomComboBox.SetSelStart(
  const ACustomComboBox: TCustomComboBox; NewStart: integer);
var
  WidgetInfo: PWidgetInfo;
  Entry: PGtkEntry;
begin
  WidgetInfo := GetWidgetInfo(Pointer(ACustomComboBox.Handle));
  
  Entry := GetComboBoxEntry(WidgetInfo^.CoreWidget);
  if Entry<>nil then begin
    //gtk_entry_select_region(Entry, NewStart, NewStart);
    gtk_editable_set_position(PGtkEditable(Entry), NewStart);
  end;
end;

class procedure TGtk2WSCustomComboBox.SetSelLength(
  const ACustomComboBox: TCustomComboBox; NewLength: integer);
var
  WidgetInfo: PWidgetInfo;
  Entry: PGtkEntry;
  Start: Integer;
begin
  WidgetInfo := GetWidgetInfo(Pointer(ACustomComboBox.Handle));

  Entry := GetComboBoxEntry(WidgetInfo^.CoreWidget);
  if Entry<>nil then begin
    Start := GetSelStart(ACustomComboBox);
    gtk_editable_select_region(PGtkEditable(Entry), Start, Start + NewLength);
  end;
end;

class procedure TGtk2WSCustomComboBox.SetItemIndex(
  const ACustomComboBox: TCustomComboBox; NewIndex: integer);
var
  P: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  LCLIndex: PLongint;
begin
  WidgetInfo := GetWidgetInfo(Pointer(ACustomComboBox.Handle));
  p := WidgetInfo^.CoreWidget;
  if gtk_combo_box_get_active(PGtkComboBox(p)) = NewIndex then exit;
  // to be delphi compatible OnChange only fires in response to user actions not program actions
  // so we use WidgetInfo^.Userdata as a flag to not signal the OnChange Event
  WidgetInfo^.UserData := Pointer(1);
  gtk_combo_box_set_active(PGtkComboBox(p), NewIndex);

  LCLIndex := gtk_object_get_data(GTK_OBJECT(p), GtkComboLCLItemIndexTag);
  if not Assigned(LCLIndex) then
    LCLIndex := New(PLongint);
  LCLIndex^ := NewIndex;
  gtk_object_set_data(GTK_OBJECT(p), GtkComboLCLItemIndexTag, LCLIndex); // set LCLIndex for OnChange -> OnSelect

  WidgetInfo^.UserData := Pointer(nil);
end;

class procedure TGtk2WSCustomComboBox.SetMaxLength(
  const ACustomComboBox: TCustomComboBox; NewLength: integer);
var
  WidgetInfo: PWidgetInfo;
  Entry: PGtkEntry;
begin
  WidgetInfo := GetWidgetInfo(Pointer(ACustomComboBox.Handle));

  Entry := GetComboBoxEntry(WidgetInfo^.CoreWidget);
  if Entry<>nil then begin
    gtk_entry_set_max_length(Entry, NewLength);
  end;
  // We save this in the CoreWidget for when the Entry Changes styles
  g_object_set_data(PGObject(WidgetInfo^.CoreWidget), 'max-length', Pointer(PtrInt(NewLength)));
end;

class procedure TGtk2WSCustomComboBox.SetStyle(
  const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle);
var
  WidgetInfo: PWidgetInfo;
  p: PGtkWidget;
begin
  if csDesigning in ACustomComboBox.ComponentState then exit;
  WidgetInfo := GetWidgetInfo(Pointer(ACustomComboBox.Handle));
  p := WidgetInfo^.CoreWidget;
  case NewStyle of
    csDropDown,
    csSimple:
      begin
        if gtk_is_combo_box_entry(p) then Exit;
        ReCreateCombo(ACustomComboBox, True, WidgetInfo);
      end;
    csDropDownList,
    csOwnerDrawFixed,
    csOwnerDrawVariable:
      begin
        if not gtk_is_combo_box_entry(p) then Exit;
        ReCreateCombo(ACustomComboBox, False, WidgetInfo);
      end;
  end;
end;

class procedure TGtk2WSCustomComboBox.SetReadOnly(
  const ACustomComboBox: TCustomComboBox; NewReadOnly: boolean);
var
  WidgetInfo: PWidgetInfo;
  Entry: PGtkWidget;
begin
  WidgetInfo := GetWidgetInfo(Pointer(ACustomComboBox.Handle));

  if gtk_is_combo_box_entry(WidgetInfo^.CoreWidget) then begin
    Entry := GTK_BIN(WidgetInfo^.CoreWidget)^.child;
    gtk_entry_set_editable(PGtkEntry(Entry), not NewReadOnly);
  end;
end;

class function TGtk2WSCustomComboBox.GetItems(
  const ACustomComboBox: TCustomComboBox): TStrings;
var
  ComboWidget: PGtkWidget;
  Handle: HWND;
begin
  Handle := ACustomComboBox.Handle;
  ComboWidget := GetWidgetInfo(Pointer(Handle), True)^.CoreWidget;
  Result :=  TGtkListStoreStringList(gtk_object_get_data(PGtkObject(ComboWidget),
                                     GtkListItemLCLListTag));
end;

class procedure TGtk2WSCustomComboBox.Sort(const ACustomComboBox: TCustomComboBox;
  AList: TStrings; IsSorted: boolean);
var
  ComboWidget: PGtkWidget;
  Handle: HWND;
begin
  Handle := ACustomComboBox.Handle;
  ComboWidget := GetWidgetInfo(Pointer(Handle), True)^.CoreWidget;
  TGtkListStoreStringList(gtk_object_get_data(PGtkObject(ComboWidget),
                                     GtkListItemLCLListTag)).Sorted := IsSorted;
end;

class procedure TGtk2WSCustomComboBox.SetColor(const AWinControl: TWinControl);
var
  WidgetInfo: PWidgetInfo;
  Child: PGtkWidget; // can be GtkCellRenderer or GtkEntry
begin
  WidgetInfo := GetWidgetInfo(Pointer(AWinControl.Handle));

  Child := GTK_BIN(WidgetInfo^.CoreWidget)^.child;
  Gtk2WidgetSet.SetWidgetColor(Child, AWinControl.Font.Color, AWinControl.Color,
   [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED,GTK_STYLE_BASE]);
end;

class procedure TGtk2WSCustomComboBox.SetFont(const AWinControl: TWinControl;
  const AFont : TFont);
var
  Entry: PGtkEntry;
  WidgetInfo: PWidgetInfo;
begin
  if not AWinControl.HandleAllocated then exit;
  if AFont.IsDefault then exit;

  WidgetInfo := GetWidgetInfo(Pointer(AWinControl.Handle));
  Entry := GetComboBoxEntry(WidgetInfo^.CoreWidget);
  if Entry<>nil then begin
    Gtk2WidgetSet.SetWidgetColor(PGtkWidget(Entry), AWinControl.font.color, clNone,
       [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED,GTK_STYLE_TEXT]);
    Gtk2WidgetSet.SetWidgetFont(PGtkWidget(Entry), AFont);
  end;
end;

class procedure TGtk2WSCustomComboBox.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  WidgetInfo: PWidgetInfo;
  Entry: PGtkWidget;
  Index: Integer;
begin
  WidgetInfo := GetWidgetInfo(Pointer(AWinControl.Handle));
  // we use user data to not signal onchange
  WidgetInfo^.UserData := Pointer(1);
  if gtk_is_combo_box_entry(WidgetInfo^.CoreWidget) then begin
    Entry := GTK_BIN(WidgetInfo^.CoreWidget)^.child;
    gtk_entry_set_text(PGtkEntry(Entry), PChar(AText));
  end
  else begin
    // if not an entry it is a readonly list so we will try to comply by matching the text to an item
    Index := TCustomComboBox(AWinControl).Items.IndexOf(AText);
    SetItemIndex(TCustomComboBox(AWinControl), Index);
  end;
  WidgetInfo^.UserData := nil;
end;

class function TGtk2WSCustomComboBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Box,      // this makes it easy to switch between GtkComBox and GtkComboBoxEntry
  ComboWidget: PGtkWidget; // ptr to the newly created GtkWidget
  ListStore : PGtkListStore;
  WidgetInfo: PWidgetInfo;
  ACustomComboBox: TCustomComboBox;
  ItemList: TGtkListStoreStringList;
  LCLIndex: PLongint;
begin
  ACustomComboBox:=TCustomComboBox(AWinControl);

  Box := gtk_event_box_new;
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Box,dbgsName(AWinControl));
  {$ENDIF}

  WidgetInfo := CreateWidgetInfo(Box, AWinControl, AParams);

  ListStore := gtk_list_store_new (2, [G_TYPE_STRING, G_TYPE_POINTER, nil]);

  case ACustomComboBox.Style of
    csDropDown, csSimple:
      ComboWidget := gtk_combo_box_entry_new_with_model(GTK_TREE_MODEL (ListStore), 0);
    csDropDownList,
    csOwnerDrawFixed,
    csOwnerDrawVariable :
      ComboWidget := gtk_combo_box_new_with_model(GTK_TREE_MODEL (ListStore));
  end;

  g_object_unref (G_OBJECT (liststore));

  gtk_container_add(PGtkContainer(Box), ComboWidget);
  gtk_widget_show_all(Box);
  
  SetRenderer(ACustomComboBox, ComboWidget, WidgetInfo);

  SetMainWidget(Box, ComboWidget);
  SetMainWidget(Box, GTK_BIN(ComboWidget)^.child);
  if PGtkComboBoxPrivate(PGtkComboBox(ComboWidget)^.priv)^.button <> nil then
    SetMainWidget(Box, PGtkComboBoxPrivate(PGtkComboBox(ComboWidget)^.priv)^.button);

  WidgetInfo^.CoreWidget := ComboWidget;
  WidgetInfo^.ClientWidget := Box;
  
  LCLIndex := New(PLongint);
  LCLIndex^ := -1;
  gtk_object_set_data(GTK_OBJECT(ComboWidget), GtkComboLCLItemIndexTag, LCLIndex);

  //gtk_widget_add_events(Box, GDK_ALL_EVENTS_MASK);

  SetCallbacks(AWinControl, Box, WidgetInfo);

  // Items
  ItemList:= TGtkListStoreStringList.Create(
          gtk_combo_box_get_model(PGtkComboBox(ComboWidget)),0,ACustomComboBox);
  gtk_object_set_data(PGtkObject(ComboWidget),GtkListItemLCLListTag,ItemList);
  ItemList.Assign(ACustomComboBox.Items);
  if ACustomComboBox.Items is TStringList then
    ItemList.Sorted:=TStringList(ACustomComboBox.Items).Sorted;

  Result := TLCLIntfHandle(PtrUInt(Box));
end;

class procedure TGtk2WSCustomComboBox.DestroyHandle(
  const AWinControl: TWinControl);
var
  Handle: HWND;
  ComboWidget: PGtkWidget;
  LCLIndex: PLongint;
begin
  Handle := AWinControl.Handle;
  ComboWidget := GetWidgetInfo(Pointer(Handle), True)^.CoreWidget;
  gtk_object_set_data(PGtkObject(ComboWidget),GtkListItemLCLListTag,nil);
  LCLIndex := gtk_object_get_data(PGtkObject(ComboWidget), GtkComboLCLItemIndexTag);
  if Assigned(LCLIndex) then begin
    gtk_object_set_data(PGtkObject(ComboWidget), GtkComboLCLItemIndexTag, nil);
    Dispose(LCLIndex);
  end else
    debugln('Gtk2WSCustomComboBox DestroyHandle: LCLIndex unassigned!');

  //DebugLn(['TGtk2WSCustomComboBox.DestroyHandle ',dbgsName(AWinControl),' ClassParent=',ClassParent.ClassName]);

  // inherited DestroyHandle doesn't work, because that is determined at
  // compile time, while the WS class hierarchy is created at runtime
  TWSWinControlClass(Classparent).DestroyHandle(AWinControl);
end;

{ TGtk2WSCustomGroupBox }

class function TGtk2WSCustomGroupBox.GetDefaultClientRect(
  const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer;
  var aClientRect: TRect): boolean;
var
  FrameBorders: TRect;
begin
  Result:=false;
  //DebugLn(['TGtk2WSCustomGroupBox.GetDefaultClientRect ',DbgSName(AWinControl),' ',aWidth,'x',aHeight]);
  if AWinControl.HandleAllocated then begin

  end else begin
    FrameBorders:=GetStyleGroupboxFrameBorders;
    aClientRect:=Rect(0,0,
                 Max(0,aWidth-FrameBorders.Left-FrameBorders.Right),
                 Max(0,aHeight-FrameBorders.Top-FrameBorders.Bottom));
    Result:=true;
  end;
  //if Result then DebugLn(['TGtk2WSCustomGroupBox.GetDefaultClientRect END FrameBorders=',dbgs(FrameBorders),' aClientRect=',dbgs(aClientRect)]);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TScrollBar, TGtk2WSScrollBar);
  RegisterWSComponent(TCustomGroupBox, TGtk2WSCustomGroupBox);
//  RegisterWSComponent(TGroupBox, TGtk2WSGroupBox);
  RegisterWSComponent(TCustomComboBox, TGtk2WSCustomComboBox);
//  RegisterWSComponent(TComboBox, TGtk2WSComboBox);
  RegisterWSComponent(TCustomListBox, TGtk2WSCustomListBox, TGtk2PrivateList);
//  RegisterWSComponent(TListBox, TGtk2WSListBox);
  RegisterWSComponent(TCustomEdit, TGtk2WSCustomEdit);
  RegisterWSComponent(TCustomMemo, TGtk2WSCustomMemo);
//  RegisterWSComponent(TEdit, TGtk2WSEdit);
//  RegisterWSComponent(TMemo, TGtk2WSMemo);
//  RegisterWSComponent(TCustomLabel, TGtk2WSCustomLabel);
//  RegisterWSComponent(TLabel, TGtk2WSLabel);
//  RegisterWSComponent(TButtonControl, TGtk2WSButtonControl);
//  RegisterWSComponent(TCustomButton, TGtk2WSButton);
//  RegisterWSComponent(TCustomCheckBox, TGtk2WSCustomCheckBox);
  RegisterWSComponent(TCustomCheckBox, TGtk2WSCustomCheckBox);
//  RegisterWSComponent(TToggleBox, TGtk2WSToggleBox);
//  RegisterWSComponent(TRadioButton, TGtk2WSRadioButton);
//  RegisterWSComponent(TCustomStaticText, TGtk2WSCustomStaticText);
//  RegisterWSComponent(TStaticText, TGtk2WSStaticText);
////////////////////////////////////////////////////
end.
