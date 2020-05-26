{
 *****************************************************************************
 *                             Gtk2WSStdCtrls.pp                             * 
 *                             -----------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Gtk2WSStdCtrls;

{$mode objfpc}{$H+}
{$PACKRECORDS c}

interface

uses
  // RTL
  glib2,  gdk2, gtk2,
  Classes, SysUtils, Math,
  // LazUtils
  LazLoggerBase, LazTracer,
  // LCL
  Controls, Graphics, StdCtrls, LMessages, LCLType, LazUtf8Classes, LazUTF8,
  // Widgetset
  WSControls, WSProc, WSStdCtrls, Gtk2Int, Gtk2Def,
  Gtk2CellRenderer, Gtk2Globals, Gtk2Proc, InterfaceBase,
  Gtk2WSControls, Gtk2Extra;

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

  TGtk2WSScrollBar = class(TWSScrollBar)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetKind(const AScrollBar: TCustomScrollBar; const {%H-}AIsHorizontal: Boolean); override;
    class procedure SetParams(const AScrollBar: TCustomScrollBar); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
  end;

  { TGtk2WSCustomGroupBox }

  TGtk2WSCustomGroupBox = class(TWSCustomGroupBox)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
    class procedure SetLabel(AFrame: PGtkFrame; AText: String);
    class function GetFrameWidget(AEventBox: PGtkEventBox): PGtkFrame;
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class function GetDefaultClientRect(const AWinControl: TWinControl;
             const {%H-}aLeft, {%H-}aTop, aWidth, aHeight: integer; var aClientRect: TRect
             ): boolean; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  { TGtk2WSGroupBox }

  TGtk2WSGroupBox = class(TWSGroupBox)
  published
  end;

  { TGtk2WSCustomComboBox }
  { !!! Both are used: TGtkComboBoxEntry (with entry) and TGtkComboBox (without entry),
           but not the old TGtkCombo !!! }

  TGtk2WSCustomComboBox = class(TWSCustomComboBox)
  protected
    class procedure ReCreateCombo(const ACustomComboBox: TCustomComboBox; const AWithEntry: Boolean; const AWidgetInfo: PWidgetInfo); virtual;
    class procedure SetRenderer(const ACustomComboBox: TCustomComboBox; AWidget: PGtkWidget; AWidgetInfo: PWidgetInfo); virtual;
    class procedure SetCallbacks(const AWinControl: tWinControl; const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
    class procedure SetSensitivity(AWinControl: TWinControl; AWidget: PGtkWidget);
  published
    class procedure GetPreferredSize(const AWinControl: TWinControl;
      var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;

    class function GetDroppedDown(const ACustomComboBox: TCustomComboBox): Boolean; override;
    class function GetSelStart(const ACustomComboBox: TCustomComboBox): integer; override;
    class function GetSelLength(const ACustomComboBox: TCustomComboBox): integer; override;
    class function GetItemIndex(const ACustomComboBox: TCustomComboBox): integer; override;
    class function GetMaxLength(const ACustomComboBox: TCustomComboBox): integer; override;
    class function GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;

    class procedure SetArrowKeysTraverseList(const {%H-}ACustomComboBox: TCustomComboBox;
                                             {%H-}NewTraverseList: boolean); override;
    class procedure SetDroppedDown(const ACustomComboBox: TCustomComboBox; ADroppedDown: Boolean); override;
    class procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); override;
    class procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer); override;
    class procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle); override;

    class function  GetItems(const ACustomComboBox: TCustomComboBox): TStrings; override;
    class procedure Sort(const ACustomComboBox: TCustomComboBox; {%H-}AList: TStrings; IsSorted: boolean); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;

    class procedure ShowHide(const AWinControl: TWinControl); override;
    
    class function  CanFocus(const AWinControl: TWinControl): boolean; override;

    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  end;

  { TGtk2WSComboBox }

  TGtk2WSComboBox = class(TWSComboBox)
  published
  end;

  { TGtk2WSCustomListBox }

  TGtk2WSCustomListBox = class(TWSCustomListBox)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function GetIndexAtXY(const ACustomListBox: TCustomListBox; {%H-}X, Y: integer): integer; override;
    class function GetItemIndex(const ACustomListBox: TCustomListBox): integer; override;
    class function GetItemRect(const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect): boolean; override;
    class function GetScrollWidth(const ACustomListBox: TCustomListBox): Integer; override;
    class function GetSelCount(const ACustomListBox: TCustomListBox): integer; override;
    class function GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean; override;
    class function GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
    class function GetTopIndex(const ACustomListBox: TCustomListBox): integer; override;

    class procedure SelectItem(const ACustomListBox: TCustomListBox; AnIndex: integer; ASelected: boolean); override;
    class procedure SetBorder(const ACustomListBox: TCustomListBox); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer); override;
    class procedure SetScrollWidth(const ACustomListBox: TCustomListBox; const AScrollWidth: Integer); override;
    class procedure SetSelectionMode(const ACustomListBox: TCustomListBox; const {%H-}AExtendedSelect,
                                     AMultiSelect: boolean); override;
    class procedure SetStyle(const ACustomListBox: TCustomListBox); override;
    class procedure SetSorted(const {%H-}ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean); override;
    class procedure SetTopIndex(const ACustomListBox: TCustomListBox; const NewTopIndex: integer); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
  end;

  { TGtk2WSListBox }

  TGtk2WSListBox = class(TWSListBox)
  published
  end;

  { TGtk2WSCustomEdit }

  TGtk2WSCustomEdit = class(TWSCustomEdit)
  published
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function GetCaretPos(const ACustomEdit: TCustomEdit): TPoint; override;
    class function GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function GetSelLength(const ACustomEdit: TCustomEdit): integer; override;

    class procedure SetCaretPos(const ACustomEdit: TCustomEdit; const NewPos: TPoint); override;
    class procedure SetCharCase(const {%H-}ACustomEdit: TCustomEdit; {%H-}NewCase: TEditCharCase); override;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); override;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; {%H-}NewChar: char); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
    class procedure SetSelText(const ACustomEdit: TCustomEdit; const NewSelText: string); override;

    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;
    class procedure SetColor(const AWinControl: TWinControl); override;

    class procedure SetAlignment(const ACustomEdit: TCustomEdit; const AAlignment: TAlignment); override;

    class procedure Cut(const ACustomEdit: TCustomEdit); override;
    class procedure Copy(const ACustomEdit: TCustomEdit); override;
    class procedure Paste(const ACustomEdit: TCustomEdit); override;
    class procedure Undo(const ACustomEdit: TCustomEdit); override;
  end;

  { TGtk2WSCustomMemo }

  TGtk2WSCustomMemo = class(TWSCustomMemo)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;

    class function  GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetSelLength(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;

    class procedure SetAlignment(const ACustomEdit: TCustomEdit; const AAlignment: TAlignment); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetWantTabs(const ACustomMemo: TCustomMemo; const NewWantTabs: boolean); override;
    class procedure SetEchoMode(const {%H-}ACustomEdit: TCustomEdit; {%H-}NewMode: TEchoMode); override;
    class procedure SetPasswordChar(const {%H-}ACustomEdit: TCustomEdit; {%H-}NewChar: char); override;
    class procedure SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean); override;

    class procedure SetCharCase(const {%H-}ACustomEdit: TCustomEdit; {%H-}NewCase: TEditCharCase); override;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    class procedure SetSelText(const ACustomEdit: TCustomEdit; const NewSelText: string); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
    class procedure SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle); override;

    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;

    class function GetCaretPos(const ACustomEdit: TCustomEdit): TPoint; override;
    class procedure SetCaretPos(const ACustomEdit: TCustomEdit; const NewPos: TPoint); override;
  end;

  { TGtk2WSEdit }

  TGtk2WSEdit = class(TWSEdit)
  published
  end;

  { TGtk2WSMemo }

  TGtk2WSMemo = class(TWSMemo)
  published
  end;

  { TGtk2WSCustomLabel }

  {
  TGtk2WSCustomLabel = class(TWSCustomLabel)
  private
  protected
  public
  end;
  }
  { TGtk2WSLabel }

  {
  TGtk2WSLabel = class(TWSLabel)
  private
  protected
  public
  end;
  }
  
  { TGtk2WSButtonControl }

  TGtk2WSButtonControl = class(TWSButtonControl)
  published
  end;

  { TGtk2WSButton }

  TGtk2WSButton = class(TWSButton)
  protected
    class function  GetButtonWidget(AEventBox: PGtkEventBox): PGtkButton;
    class function  GetLabelWidget(AEventBox: PGtkEventBox): PGtkLabel;
  public
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;
    class function  GetText(const {%H-}AWinControl: TWinControl; var {%H-}AText: String): Boolean; override;

    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetDefault(const AButton: TCustomButton; ADefault: Boolean); override;
    class procedure SetShortcut(const AButton: TCustomButton; const {%H-}ShortCutK1, {%H-}ShortCutK2: TShortcut); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
  end;

  { TGtk2WSCustomCheckBox }

  TGtk2WSCustomCheckBox = class(TWSCustomCheckBox)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function  CreateHandle(const AWinControl: TWinControl;
                                 const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;
    class function  RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;
    class procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox; const ShortCutK1, {%H-}ShortCutK2: TShortCut); override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox;
      const NewState: TCheckBoxState); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
  end;

  { TGtk2WSCheckBox }

  TGtk2WSCheckBox = class(TWSCheckBox)
  published
  end;

  { TGtk2WSToggleBox }

  TGtk2WSToggleBox = class(TWSToggleBox)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TGtk2WSRadioButton }

  TGtk2WSRadioButton = class(TWSRadioButton)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TGtk2WSCustomStaticText }

  TGtk2WSCustomStaticText = class(TWSCustomStaticText)
  protected
    class function GetLabelWidget(AFrame: PGtkFrame): PGtkLabel;
    class function GetBoxWidget(AFrame: PGtkFrame): PGtkEventBox;
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetAlignment(const ACustomStaticText: TCustomStaticText;
                                 const NewAlignment: TAlignment); override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;
    class function  GetText(const {%H-}AWinControl: TWinControl; var {%H-}AText: String): Boolean; override;
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetStaticBorderStyle(const ACustomStaticText: TCustomStaticText; const NewBorderStyle: TStaticBorderStyle); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
  end;

  { TGtk2WSStaticText }

  TGtk2WSStaticText = class(TWSStaticText)
  published
  end;

{$DEFINE MEMOHEADER}
{$I gtk2memostrings.inc}
{$UNDEF MEMOHEADER}

function GetComboBoxEntry(Widget: PGtkWidget): PGtkEntry;

implementation

uses
  LCLMessageGlue, Forms;

const
  StaticBorderShadowMap: array[TStaticBorderStyle] of TGtkShadowType =
  (
    GTK_SHADOW_NONE,
    GTK_SHADOW_ETCHED_IN,
    GTK_SHADOW_IN
  );


function GetComboBoxEntry(Widget: PGtkWidget): PGtkEntry;
begin
  if GtkWidgetIsA(Widget, GTK_TYPE_COMBO_BOX_ENTRY) then
    Result := PGtkEntry(GTK_BIN(Widget)^.child)
  else
    Result := nil;
end;

{$I gtk2memostrings.inc}

{ TGtk2WSCustomListBox }

procedure StoreFirstSelectedPath({%H-}model:PGtkTreeModel; path:PGtkTreePath;
  {%H-}iter:PGtkTreeIter; data:gpointer); cdecl;
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
  Selection: PGtkTreeSelection;
begin
  Result := -1;
  if not WSCheckHandleAllocated(ACustomListBox, 'GetItemIndex') then
    Exit;
  Widget := GetOrCreateWidgetInfo({%H-}Pointer(ACustomListBox.Handle))^.CoreWidget;
  if GtkWidgetIsA(Widget, gtk_tree_view_get_type) then
  begin
    Path:=nil;
    Column:=nil;
    gtk_tree_view_get_cursor(PGtkTreeView(Widget), Path, column);

    if Path <> nil then
    begin
      Result := gtk_tree_path_get_indices(Path)^;
      if Result = 0 then
      begin
        Selection := gtk_tree_view_get_selection(PGtkTreeView(Widget));
        if not gtk_tree_selection_path_is_selected(Selection, Path) then
          Result := -1;
      end;
      gtk_tree_path_free(Path);
    end else
      Result := -1;
  end;
end;

class function TGtk2WSCustomListBox.GetItemRect(
  const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect
  ): boolean;
var
  Widget: PGtkWidget;
  Column: PGtkTreeViewColumn;
  Path: PGtkTreePath;
  AGdkRect: TGdkRectangle;
begin
  Result := False;
  FillChar(ARect, SizeOf(ARect), 0);
  if not WSCheckHandleAllocated(ACustomListBox, 'GetItemIndex') then
    Exit;
  Widget := GetOrCreateWidgetInfo({%H-}Pointer(ACustomListBox.Handle))^.CoreWidget;
  if GtkWidgetIsA(Widget, gtk_tree_view_get_type) and (Index >= 0) then
  begin
    Path := gtk_tree_path_new_from_indices(Index, -1);
    Column := gtk_tree_view_get_column(PGtkTreeView(Widget), 0);
    FillChar(AGdkRect{%H-}, SizeOf(AGdkRect), 0);
    gtk_tree_view_get_cell_area(PGtkTreeView(Widget), Path, Column, @AGdkRect);
    ARect := Rect(AGdkRect.x, AGdkRect.y, AGdkRect.x + AGdkRect.width, AGdkRect.y + AGdkRect.height);
    gtk_tree_path_free(Path);
    Result := True;
  end;
end;

class function TGtk2WSCustomListBox.GetScrollWidth(const ACustomListBox: TCustomListBox): Integer;
var
  Adjustment: PGtkAdjustment;
begin
  Adjustment := gtk_scrolled_window_get_hadjustment({%H-}PGtkScrolledWindow(ACustomListBox.Handle));
  Result := Trunc(Adjustment^.upper);
end;

class function TGtk2WSCustomListBox.GetTopIndex(const ACustomListBox: TCustomListBox): integer;
begin
  Result := GetIndexAtXY(ACustomListBox, 0, 1);
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
  Widget := GetOrCreateWidgetInfo({%H-}Pointer(ACustomListBox.Handle))^.CoreWidget;
  ListStoreModel := gtk_tree_view_get_model(PGtkTreeView(Widget));
  Selection := gtk_tree_view_get_selection(PGtkTreeView(Widget));

  if gtk_tree_model_iter_nth_child(ListStoreModel, @Iter, nil, AnIndex) then
  begin
    if gtk_tree_view_get_model(PGtkTreeView(Widget)) = nil then
      Exit; // we are in the midst of a begin update end update pair and the following will fail and cause gtk debug messages
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
  gtk_scrolled_window_set_shadow_type({%H-}PGtkScrolledWindow(ACustomListBox.Handle),
    BorderStyleShadowMap[ACustomListBox.BorderStyle]);
end;

class procedure TGtk2WSCustomListBox.SetColor(const AWinControl: TWinControl);
var
  AWidget: PGTKWidget;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetColor') then
    Exit;
  AWidget := {%H-}PGtkWidget(AWinControl.Handle);
  AWidget := GetOrCreateWidgetInfo(AWidget)^.CoreWidget;
  Gtk2WidgetSet.SetWidgetColor(AWidget,
    AWinControl.Font.Color,
    AWinControl.Color,
    [GTK_STATE_NORMAL, GTK_STATE_ACTIVE, GTK_STATE_PRELIGHT, GTK_STYLE_BASE]);
end;

class procedure TGtk2WSCustomListBox.SetItemIndex(
  const ACustomListBox: TCustomListBox; const AIndex: integer);
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Selection: PGtkTreeSelection;
  Path: PGtkTreePath;
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'SetItemIndex') then
    Exit;

  WidgetInfo := GetOrCreateWidgetInfo({%H-}Pointer(ACustomListBox.Handle));
  Widget := WidgetInfo^.CoreWidget;
  if not GtkWidgetIsA(Widget, gtk_tree_view_get_type) then
    raise Exception.Create('');

  Selection := gtk_tree_view_get_selection(PGtkTreeView(Widget));

  Inc(WidgetInfo^.ChangeLock);
  if (AIndex < 0) then
    Path := nil
  else
    Path := gtk_tree_path_new_from_indices(AIndex, -1);

  // if singleselection mode then selection = itemindex
  if Path <> nil then
  begin
    if PGtkTreeView(Widget)^.priv^.tree <> nil then
      gtk_tree_view_set_cursor(PGtkTreeView(Widget), Path, nil, False);
  end
  else
  begin
    Path := gtk_tree_path_new_from_indices(0, -1);
    if PGtkTreeView(Widget)^.priv^.tree <> nil then
      gtk_tree_view_set_cursor(PGtkTreeView(Widget), Path, nil, False);
    gtk_tree_selection_unselect_all(Selection);
  end;

  if Path <> nil then
    gtk_tree_path_free(Path);

  Dec(WidgetInfo^.ChangeLock);
end;

class procedure TGtk2WSCustomListBox.SetScrollWidth(
  const ACustomListBox: TCustomListBox; const AScrollWidth: Integer);
const
  BoolToPolicy: array[Boolean] of TGtkPolicyType = (GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);
var
  Adjustment: PGtkAdjustment;
  ScrolledWindow: PGtkScrolledWindow;
begin
  ScrolledWindow := {%H-}PGtkScrolledWindow(ACustomListBox.Handle);
  gtk_scrolled_window_set_policy(ScrolledWindow, BoolToPolicy[AScrollWidth > PgtkWidget(ScrolledWindow)^.allocation.width], GTK_POLICY_AUTOMATIC);
  Adjustment := gtk_scrolled_window_get_hadjustment(ScrolledWindow);
  Adjustment^.upper := AScrollWidth;
  gtk_adjustment_changed(Adjustment);
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
  Widget := GetOrCreateWidgetInfo({%H-}Pointer(ACustomListBox.Handle))^.CoreWidget;
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

class procedure TGtk2WSCustomListBox.SetStyle(
  const ACustomListBox: TCustomListBox);
var
  AStyle: PtrInt;
  Widget: PGtkWidget;
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'SetStyle') then
    Exit;
  Widget := GetOrCreateWidgetInfo({%H-}Pointer(ACustomListBox.Handle))^.CoreWidget;
  AStyle := {%H-}PtrInt(g_object_get_data(PGObject(Widget), 'lclcustomlistboxstyle'));
  if (AStyle <> Ord(ACustomListBox.Style)) then
    RecreateWnd(ACustomListBox);
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
  Widget := GetOrCreateWidgetInfo({%H-}Pointer(ACustomListBox.Handle))^.CoreWidget;
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
  Widget := GetWidgetInfo({%H-}Pointer(AWinControl.Handle))^.CoreWidget;

  Gtk2WidgetSet.SetWidgetColor(Widget, AFont.Color, clNone,
       [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED,
        GTK_STYLE_TEXT]);
  Gtk2WidgetSet.SetWidgetFont(Widget, AFont);
end;

class procedure TGtk2WSCustomListBox.ShowHide(const AWinControl: TWinControl);
begin
  // issue #27276
  if AWinControl.HandleAllocated and AWinControl.HandleObjectShouldBeVisible and
    (TCustomListBox(AWinControl).ItemIndex = -1) then
    SetItemIndex(TCustomListBox(AWinControl), TCustomListBox(AWinControl).ItemIndex);
  // issue #28341
  if AWinControl.HandleObjectShouldBeVisible then
      SetFont(AWinControl, AWinControl.Font);
  Gtk2WidgetSet.SetVisible(AWinControl, AWinControl.HandleObjectShouldBeVisible);
  InvalidateLastWFPResult(AWinControl, AWinControl.BoundsRect);
end;

function gtk2ListBoxSelectionChangedAfter({%H-}Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo): gboolean; cdecl;
var
  Mess: TLMessage;
begin
  Result := CallBackDefaultReturn;
  if WidgetInfo^.ChangeLock > 0 then
    Exit;
  {$IFDEF EventTrace}
  EventTrace('gtk2ListSelectionChangedAfter', WidgetInfo^.LCLObject);
  {$ENDIF}
  FillChar(Mess{%H-},SizeOf(Mess),0);
  Mess.msg := LM_SelChange;
  DeliverMessage(WidgetInfo^.LCLObject, Mess);
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
  Result := TGtk2WSBaseScrollingWinControl.CreateHandle(AWinControl, AParams);
  p := {%H-}PGtkWidget(Result);
  
  if Result = 0 then exit;
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(p,dbgsName(AWinControl));
  {$ENDIF}

  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.hscrollbar, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.vscrollbar, GTK_CAN_FOCUS);
  // by default horz scrollbar is invisible. it is set by SetScrollWidth
  gtk_scrolled_window_set_policy(PGtkScrolledWindow(p),
                                 GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
  //Set BorderStyle according to the provided Params
  if (AParams.ExStyle and WS_EX_CLIENTEDGE) > 0 then
    gtk_scrolled_window_set_shadow_type(PGtkScrolledWindow(p), GTK_SHADOW_ETCHED_IN)
  else
    gtk_scrolled_window_set_shadow_type(PGtkScrolledWindow(p), GTK_SHADOW_NONE);

  gtk_widget_show(p);

  liststore := gtk_list_store_new (2, [G_TYPE_STRING, G_TYPE_POINTER, nil]);

  TVWidget:= gtk_tree_view_new_with_model (GTK_TREE_MODEL (liststore));
  g_object_unref (G_OBJECT (liststore));

  renderer := LCLIntfCellRenderer_New();
  column := gtk_tree_view_column_new_with_attributes ('LISTITEMS', renderer,
                                                      ['text', 0, nil]);
  gtk_cell_layout_set_cell_data_func(PGtkCellLayout(column), renderer,
    @LCLIntfCellRenderer_CellDataFunc, nil, nil);
  gtk_tree_view_append_column (GTK_TREE_VIEW (TVWidget), column);
  gtk_tree_view_column_set_clickable (GTK_TREE_VIEW_COLUMN (column), TRUE);

  gtk_tree_view_set_headers_visible(GTK_TREE_VIEW (TVWidget), False);

  gtk_container_add(GTK_CONTAINER(p), TVWidget);
  gtk_widget_show(TVWidget);

  SetMainWidget(p, TVWidget);
  WidgetInfo := GetWidgetInfo(p);
  WidgetInfo^.CoreWidget := TVWidget;

  Selection := gtk_tree_view_get_selection(PGtkTreeView(TVWidget));

  case TCustomListBox(AWinControl).MultiSelect of
    True : gtk_tree_selection_set_mode(Selection, GTK_SELECTION_MULTIPLE);
    False: gtk_tree_selection_set_mode(Selection, GTK_SELECTION_SINGLE);
  end;

  if TListBox(AWinControl).Style = lbOwnerDrawFixed then
  begin
    gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_FIXED);
    gtk_tree_view_set_fixed_height_mode(PGtkTreeView(TVWidget), True);
  end;

  
  g_signal_connect_after(Selection, 'changed',
    G_CALLBACK(@gtk2ListBoxSelectionChangedAfter), WidgetInfo);

  g_object_set_data(PGObject(TVWidget), 'lclcustomlistboxstyle', {%H-}gPointer(Ord(TListBox(AWinControl).Style)));

  // Sets the callbacks
  if not AWinControl.HandleObjectShouldBeVisible and not (csDesigning in AWinControl.ComponentState) then
    gtk_widget_hide(p);
  SetCallbacks(p, WidgetInfo);
end;

class procedure TGtk2WSCustomListBox.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
end;

class function TGtk2WSCustomListBox.GetIndexAtXY(
  const ACustomListBox: TCustomListBox; X, Y: integer): integer;
var
  aTreeView: PGtkTreeView;
  aTreeColumn: PGtkTreeViewColumn;
  aTreePath: PGtkTreePath;
begin
  Result := -1;
  if not WSCheckHandleAllocated(ACustomListBox, 'GetIndexAtXY') then
    Exit;
  case ACustomListBox.fCompStyle of
  csListBox, csCheckListBox:
    begin
      aTreeView:=GTK_TREE_VIEW(GetOrCreateWidgetInfo({%H-}Pointer(ACustomListBox.Handle))^.CoreWidget);
      aTreePath:=nil;
      aTreeColumn:=nil;
      if gtk_tree_view_get_path_at_pos(aTreeView, 0, Y, aTreePath, aTreeColumn, nil, nil)
      then begin
        Result := gtk_tree_path_get_indices(aTreePath)[0];
        gtk_tree_path_free(aTreePath);
        exit;
      end;
    end;
  end;
end;

class function TGtk2WSCustomListBox.GetSelCount(const ACustomListBox: TCustomListBox): integer;
var
  Widget: PGtkWidget; // pointer to gtk-widget (local use when neccessary)
  Selection: PGtkTreeSelection;
  ListStoreModel: PGtkTreeModel;
  Rows: PGList;
begin
  Result := 0;
  if not WSCheckHandleAllocated(ACustomListBox, 'GetSelCount') then
    Exit;
  Widget := GetOrCreateWidgetInfo({%H-}Pointer(ACustomListBox.Handle))^.CoreWidget;
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
  Widget := GetOrCreateWidgetInfo({%H-}Pointer(ACustomListBox.Handle))^.CoreWidget;
  ListStoreModel := gtk_tree_view_get_model(PGtkTreeView(Widget));
  Selection := gtk_tree_view_get_selection(PGtkTreeView(Widget));

  if gtk_tree_view_get_model(PGtkTreeView(Widget)) = nil then
    Exit;
  if gtk_tree_model_iter_nth_child(ListStoreModel, @Item, nil, AIndex) then
    Result := gtk_tree_selection_iter_is_selected(Selection, @Item);
end;

class function TGtk2WSCustomListBox.GetStrings(
  const ACustomListBox: TCustomListBox): TStrings;
var
  Widget: PGtkWidget;// pointer to gtk-widget
begin
  Result:=nil;
  if not WSCheckHandleAllocated(ACustomListBox, 'GetStrings') then
    Exit;
  case ACustomListBox.fCompStyle of
    {csCListBox:
      begin
        Widget:= GetOrCreateWidgetInfo(Pointer(Handle))^.CoreWidget;
        Result := TGtkCListStringList.Create(PGtkCList(Widget));
        if ACustomListBox is TCustomListBox then
          TGtkCListStringList(Result).Sorted :=
                                          TCustomListBox(ACustomListBox).Sorted;
      end;
    }
    csCheckListBox, csListBox:
      begin
        Widget := GetOrCreateWidgetInfo({%H-}Pointer(ACustomListBox.Handle))^.CoreWidget;
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

class procedure TGtk2WSCustomCheckBox.SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
  TGtk2Widgetset(WidgetSet).SetCallback(LM_CHANGED, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject);
end;


class function TGtk2WSCustomCheckBox.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLIntfHandle;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
begin
  { ToDo verify if the check box has correct z-order and disable GTK_WIDGET_NO_WINDOW if not.}
  Widget := gtk_check_button_new_with_label(AParams.Caption);
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}
  Result := THandle({%H-}PtrUInt(Widget));
  WidgetInfo := CreateWidgetInfo({%H-}Pointer(Result), AWinControl, AParams);

  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate({%H-}PGtkWidget(Result), @Allocation);
  if AParams.Style and WS_VISIBLE = 0 then
    gtk_widget_hide({%H-}PGtkWidget(Result))
  else
    gtk_widget_show({%H-}PGtkWidget(Result));

  Set_RC_Name(AWinControl, {%H-}PGtkWidget(Result));
  SetCallbacks({%H-}PGtkWidget(Result), WidgetInfo);
end;

class procedure TGtk2WSCustomCheckBox.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  GetGTKDefaultWidgetSize(AWinControl,PreferredWidth,PreferredHeight,
                          WithThemeSpace);
end;

class function TGtk2WSCustomCheckBox.RetrieveState(
  const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
var
  ToggleButton: PGtkToggleButton;
begin
  ToggleButton:={%H-}PGtkToggleButton(ACustomCheckBox.Handle);
  if gtk_toggle_button_get_inconsistent(ToggleButton) then
    Result := cbGrayed
  else
  if gtk_toggle_button_get_active(ToggleButton) then
    Result := cbChecked
  else
    Result := cbUnchecked;
end;

class procedure TGtk2WSCustomCheckBox.SetShortCut(const ACustomCheckBox: TCustomCheckBox;
  const ShortCutK1, ShortCutK2: TShortCut);
begin
  Accelerate(ACustomCheckBox, {%H-}PGtkWidget(ACustomCheckBox.Handle), ShortcutK1,
    'clicked'
    //'activate_item'
    );
end;

class procedure TGtk2WSCustomCheckBox.SetState(
  const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
var
  GtkObject: PGtkObject;
  ToggleButton: PGtkToggleButton;
begin
  //debugln('TGtk2WSCustomCheckBox.SetState A ',DbgSName(ACustomCheckBox),' State=',dbgs(ord(ACustomCheckBox.State)));
  GtkObject := {%H-}PGtkObject(ACustomCheckBox.Handle);
  LockOnChange(GtkObject,1);
  ToggleButton:=PGtkToggleButton(GtkObject);
  gtk_toggle_button_set_inconsistent(ToggleButton, NewState=cbGrayed);
  gtk_toggle_button_set_active(ToggleButton, NewState=cbChecked);
  LockOnChange(GtkObject,-1);
end;

class procedure TGtk2WSCustomCheckBox.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  Widget: PGTKWidget;
  LblWidget: PGtkWidget;
begin
  if not AWinControl.HandleAllocated then exit;

  Widget := {%H-}PGtkWidget(AWinControl.Handle);
  LblWidget := (pGtkBin(Widget)^.Child);
  if LblWidget <> nil then
  begin
    Gtk2WidgetSet.SetWidgetFont(LblWidget, AFont);
    Gtk2WidgetSet.SetWidgetColor(LblWidget, AFont.Color, clNone,
       [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
  end;
end;

class procedure TGtk2WSCustomCheckBox.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  BoxWidget: PGtkWidget;
  B: Boolean;
  P: PGChar;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetText') then Exit;
  BoxWidget := {%H-}PGtkWidget(AWinControl.Handle);
  if AText = '' then
  begin
    gtk_button_set_label(PGtkButton(BoxWidget), '');
    gtk_widget_hide(PGtkBin(BoxWidget)^.child);
  end else
  begin
    P := gtk_label_get_text(PGtkLabel(PGtkBin(BoxWidget)^.child));
    B := (StrPas(P) <> AText);
    gtk_widget_show(PGtkBin(BoxWidget)^.child);
    gtk_button_set_label(PGtkButton(BoxWidget), PChar(Ampersands2Underscore(EscapeUnderscores(AText))));
    gtk_button_set_use_underline(PGtkButton(BoxWidget), True);
    if B then
    begin
      SetColor(AWinControl);
      SetFont(AWinControl, AWinControl.Font);
    end;
  end;
end;

class procedure TGtk2WSCustomCheckBox.ShowHide(const AWinControl: TWinControl);
begin
  // gtk2 doesn't set font properly
  // so we are doing it one more time before showing. Issues #21172, #23152
  if AWinControl.HandleObjectShouldBeVisible then
  begin
    SetFont(AWinControl, AWinControl.Font);
    AWinControl.InvalidatePreferredSize();
    AWinControl.AdjustSize();
  end;
  TGtk2WSWinControl.ShowHide(AWinControl);
end;

{$I gtk2wscustommemo.inc}

{ TGtk2WSCustomEdit }

class procedure TGtk2WSCustomEdit.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));

  with TGtk2Widgetset(Widgetset) do
  begin
    SetCallback(LM_CHANGED, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject);
    SetCallback(LM_ACTIVATE, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject);
    SetCallback(LM_CUT, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject);
    SetCallback(LM_COPY, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject);
    SetCallback(LM_PASTE, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject);
  end;
end;

class procedure TGtk2WSCustomEdit.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  GetGTKDefaultWidgetSize(AWinControl,PreferredWidth,PreferredHeight,
                          WithThemeSpace);
  //debugln('TGtkWSCustomEdit.GetPreferredSize ',DbgSName(AWinControl),' PreferredWidth=',dbgs(PreferredWidth),' PreferredHeight=',dbgs(PreferredHeight));
end;

class procedure TGtk2WSCustomEdit.SetColor(const AWinControl: TWinControl);
var
  AWidget: PGTKWidget;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetColor') then Exit;
  AWidget := {%H-}PGtkWidget(AWinControl.Handle);
  // don't change selected state
  Gtk2WidgetSet.SetWidgetColor(AWidget, clNone, AWinControl.Color,
    [GTK_STATE_NORMAL, GTK_STYLE_BASE]);
end;


class procedure TGtk2WSCustomEdit.SetText(const AWinControl: TWinControl;
  const AText: string);
var
  Widget: PGtkWidget;
  Mess : TLMessage;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetText') then
    Exit;
  {$IFDEF VerboseTWinControlRealText}
  DebugLn(['TGtkWSCustomEdit.SetText START ',DbgSName(AWinControl),' AText="',AText,'"']);
  {$ENDIF}
  Widget:={%H-}PGtkWidget(AWinControl.Handle);
  // some gtk2 versions fire the change event twice
  // lock the event and send the message afterwards
  // see bug http://bugs.freepascal.org/view.php?id=14615
  LockOnChange(PgtkObject(Widget), +1);
  try
    if GTK_IS_SPIN_BUTTON(Widget) then
      gtk_entry_set_text(@PGtkSpinButton(Widget)^.entry, PChar(AText))
    else
      gtk_entry_set_text(PGtkEntry(Widget), PChar(AText));
  finally
    LockOnChange(PgtkObject(Widget), -1);
  end;
  SetSelStart(TCustomEdit(AWinControl), 0);
  {$IFDEF VerboseTWinControlRealText}
  DebugLn(['TGtkWSCustomEdit.SetText SEND TEXTCHANGED message ',DbgSName(AWinControl),' New="',gtk_entry_get_text(PGtkEntry(AWinControl.Handle)),'"']);
  {$ENDIF}
  FillByte(Mess{%H-},SizeOf(Mess),0);
  Mess.Msg := CM_TEXTCHANGED;
  DeliverMessage(AWinControl, Mess);

  {$IFDEF VerboseTWinControlRealText}
  DebugLn(['TGtkWSCustomEdit.SetText END ',DbgSName(AWinControl),' New="',gtk_entry_get_text(PGtkEntry(AWinControl.Handle)),'"']);
  {$ENDIF}
end;

class procedure TGtk2WSCustomEdit.SetSelText(const ACustomEdit: TCustomEdit;
  const NewSelText: string);
var
  Widget: PGtkWidget;
  Entry: PGtkEntry;
  Text: string;
  SelStart: Integer;
  Mess : TLMessage;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetSelText') then
    Exit;
  Widget:={%H-}PGtkWidget(ACustomEdit.Handle);
  if GTK_IS_SPIN_BUTTON(Widget) then
    Entry := @PGtkSpinButton(Widget)^.entry
  else
    Entry := PGtkEntry(Widget);
  Text := gtk_entry_get_text(Entry);
  SelStart := GetSelStart(ACustomEdit);
  Text := UTF8Copy(Text, 1, SelStart) + NewSelText +
    UTF8Copy(Text, SelStart + GetSelLength(ACustomEdit) + 1, MaxInt);
  SelStart := SelStart + UTF8Length(NewSelText);
  // some gtk2 versions fire the change event twice
  // lock the event and send the message afterwards
  // see bug http://bugs.freepascal.org/view.php?id=14615
  LockOnChange(PgtkObject(Widget), +1);
  try
    gtk_entry_set_text(Entry, PChar(Text));
  finally
    LockOnChange(PgtkObject(Widget), -1);
  end;
  SetSelStart(ACustomEdit, SelStart);
  FillByte(Mess{%H-},SizeOf(Mess),0);
  Mess.Msg := CM_TEXTCHANGED;
  DeliverMessage(ACustomEdit, Mess);
end;

class procedure TGtk2WSCustomEdit.SetCharCase(const ACustomEdit: TCustomEdit;
  NewCase: TEditCharCase);
begin
  // TODO: TGtk2WSCustomEdit.SetCharCase: implement me!
end;

class procedure TGtk2WSCustomEdit.SetMaxLength(const ACustomEdit: TCustomEdit;
  NewLength: integer);
var
  Widget: PGtkWidget;
begin
  Widget:={%H-}PGtkWidget(ACustomEdit.Handle);
  if GtkWidgetIsA(Widget, GTK_TYPE_ENTRY) then
    gtk_entry_set_max_length(GTK_ENTRY(Widget), guint16(NewLength));
end;

function CellEntryKeyDown({%H-}Widget: PGtkWidget; Event : pgdkeventkey;
  {%H-}Data: gPointer) : GBoolean; cdecl;
begin
  Result := (Event^.keyval = GDK_KEY_UP) or (Event^.keyval = GDK_KEY_DOWN);
end;

class function TGtk2WSCustomEdit.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Widget: PGtkWidget; // ptr to the newly created GtkWidget
  WidgetInfo: PWidgetInfo;
  CellEditable: PGtkCellEditable;
begin
  Widget := gtk_entry_new();
  gtk_editable_set_editable(PGtkEditable(Widget), not TCustomEdit(AWinControl).ReadOnly);
  if AParams.Style and WS_VISIBLE = 0 then
    gtk_widget_hide(Widget)
  else
    gtk_widget_show(Widget);
  Result := TLCLIntfHandle({%H-}PtrUInt(Widget));
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}
  if Result = 0 then
    Exit;
  WidgetInfo := CreateWidgetInfo({%H-}Pointer(Result), AWinControl, AParams);
  Set_RC_Name(AWinControl, Widget);
  SetCallbacks(Widget, WidgetInfo);

  if Result <> 0 then
  begin
    // hook into GtkEntry interface, so it won't focus another control
    // by pressing VK_UP or VK_DOWN. issue #11115
    CellEditable := GTK_CELL_EDITABLE(Widget);
    g_signal_connect(CellEditable, 'key_press_event',
      TGTKSignalFunc(@CellEntryKeyDown), AWinControl);

    gtk_entry_set_has_frame({%H-}PGtkEntry(Result),
      TCustomEdit(AWinControl).BorderStyle <> bsNone);
    // don't select it on focus since LCL do this itself
    g_object_set(gtk_widget_get_settings({%H-}PGtkWidget(Result)),
	                'gtk-entry-select-on-focus', [0, nil]);
  end;
end;

class function TGtk2WSCustomEdit.GetCaretPos(const ACustomEdit: TCustomEdit
  ): TPoint;
var
  Entry: PGtkEntry;
  AInfo: PWidgetInfo;
begin
  Result := Point(0,0);
  if not WSCheckHandleAllocated(ACustomEdit, 'GetCaretPos') then
    Exit;
  Entry := {%H-}PGtkEntry(ACustomEdit.Handle);
  if gtk_widget_has_focus(PGtkWidget(Entry)) then
    Result.X := Max(Entry^.current_pos, Entry^.selection_bound)
  else begin
    AInfo := GetWidgetInfo(PGtkWidget(Entry));
    if AInfo <> nil then
      Result.X := AInfo^.CursorPos + AInfo^.SelLength;
  end;
end;

class function TGtk2WSCustomEdit.GetSelStart(const ACustomEdit: TCustomEdit
  ): integer;
var
  Entry: PGtkEntry;
  AInfo: PWidgetInfo;
begin
  Result := 0;
  if not WSCheckHandleAllocated(ACustomEdit, 'GetSelStart') then
    Exit;
  Entry := {%H-}PGtkEntry(ACustomEdit.Handle);
  if gtk_widget_has_focus(PGtkWidget(Entry)) then
    Result := Min(Entry^.current_pos, Entry^.selection_bound)
  else begin
    AInfo := GetWidgetInfo(PGtkWidget(Entry));
    if AInfo <> nil then
      Result := AInfo^.CursorPos;
  end;
end;

class function TGtk2WSCustomEdit.GetSelLength(const ACustomEdit: TCustomEdit
  ): integer;
var
  Entry: PGtkEntry;
  AInfo: PWidgetInfo;
begin
  Result := 0;
  if not WSCheckHandleAllocated(ACustomEdit, 'GetSelLength') then
    Exit;
  Entry := {%H-}PGtkEntry(ACustomEdit.Handle);
  if gtk_widget_has_focus(PGtkWidget(Entry)) then
    Result := ABS(Entry^.current_pos - Entry^.selection_bound)
  else begin
    AInfo := GetWidgetInfo(PGtkWidget(Entry));
    if AInfo <> nil then
      Result := AInfo^.SelLength;
  end;
end;

class procedure TGtk2WSCustomEdit.SetCaretPos(const ACustomEdit: TCustomEdit;
  const NewPos: TPoint);
var
  NewStart: Integer;
  Entry: PGtkEntry;
  WidgetInfo: PWidgetInfo;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetCaretPos') then
    Exit;
  SetSelLength(ACustomEdit, 0);
  Entry := {%H-}PGtkEntry(ACustomEdit.Handle);
  // make gtk2 consistent with others. issue #11802
  // if GetCaretPos(ACustomEdit).X = NewPos.X then exit;

  if Entry^.text_max_length > 0 then
    NewStart := Min(NewPos.X, Entry^.text_max_length)
  else
    NewStart := Min(NewPos.X, Entry^.text_length);
  WidgetInfo := GetWidgetInfo(Entry);
  WidgetInfo^.CursorPos := NewStart;
  gtk_editable_set_position(PGtkEditable(Entry), NewStart);
end;

class procedure TGtk2WSCustomEdit.SetEchoMode(const ACustomEdit: TCustomEdit;
  NewMode: TEchoMode);
var
  Entry: PGtkEntry;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetEchoMode') then
    Exit;
  Entry := {%H-}PGtkEntry(ACustomEdit.Handle);
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
  Entry := {%H-}PGtkEntry(ACustomEdit.Handle);
  if ACustomEdit.EchoMode=emNone then
    PWChar:=0
  else begin
    PWChar:=ord(ACustomEdit.PasswordChar);
    if (PWChar<192) or (PWChar=ord('*')) then
      PWChar:=9679;
  end;
  gtk_entry_set_invisible_char(Entry,PWChar);
end;

class procedure TGtk2WSCustomEdit.SetReadOnly(const ACustomEdit: TCustomEdit;
  NewReadOnly: boolean);
var
  Widget: PGtkWidget;
begin
  Widget := {%H-}PGtkWidget(ACustomEdit.Handle);
  if GTK_IS_EDITABLE(Widget) then
    gtk_editable_set_editable(PGtkEditable(Widget), not NewReadOnly);
end;

class procedure TGtk2WSCustomEdit.SetSelStart(const ACustomEdit: TCustomEdit;
  NewStart: integer);
var
  NewPos: Integer;
  Entry: PGtkEntry;
  WidgetInfo: PWidgetInfo;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetSelStart') then
    Exit;
  SetSelLength(ACustomEdit, 0);
  Entry := {%H-}PGtkEntry(ACustomEdit.Handle);
  // make gtk2 consistent with others. issue #11802
  // if GetSelStart(ACustomEdit) = NewStart then exit;

  if Entry^.text_max_length > 0 then
    NewPos := Min(NewStart, Entry^.text_max_length)
  else
    NewPos := Min(NewStart, Entry^.text_length);
  WidgetInfo := GetWidgetInfo(Entry);
  WidgetInfo^.CursorPos := NewPos;
  gtk_editable_set_position(PGtkEditable(Entry), NewPos);
end;

class procedure TGtk2WSCustomEdit.SetSelLength(
  const ACustomEdit: TCustomEdit; NewLength: integer);
var
  Entry: PGtkEntry;
  SelStart: Integer;
  WidgetInfo: PWidgetInfo;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetSelLength') then
    Exit;
  Entry := {%H-}PGtkEntry(ACustomEdit.Handle);
  SelStart := GetSelStart(ACustomEdit);

  WidgetInfo := GetWidgetInfo(Entry);
  if WidgetInfo^.CursorPos = 0 then
    WidgetInfo^.CursorPos := SelStart;
  WidgetInfo^.SelLength := NewLength;
  gtk_entry_select_region(Entry,
    SelStart + NewLength,
    SelStart );
end;

class procedure TGtk2WSCustomEdit.SetAlignment(const ACustomEdit: TCustomEdit;
  const AAlignment: TAlignment);
var
  Entry: PGtkEntry;
  Alignment: GFloat;
begin
  Entry := {%H-}PGtkEntry(ACustomEdit.Handle);
  case AAlignment of
    taLeftJustify: Alignment := 0;
    taRightJustify: Alignment := 1;
    taCenter: Alignment := 0.5;
  end;
  gtk_entry_set_alignment(Entry, Alignment);
end;

class procedure TGtk2WSCustomEdit.Cut(const ACustomEdit: TCustomEdit);
var
  ATextView: PGtkTextView;
  ABuffer: PGtkTextBuffer;
  AStart, AStop: PGtkTextIter;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'Cut') then
    Exit;

  if ACustomEdit.FCompStyle = csMemo then
  begin
    ATextView := GTK_TEXT_VIEW(GetWidgetInfo({%H-}PGtkWidget(ACustomEdit.Handle))^.CoreWidget);
    ABuffer := gtk_text_view_get_buffer(ATextView);
    if ABuffer <> nil then
    begin
      AStart := nil;
      AStop := nil;
      if gtk_text_buffer_get_selection_bounds(ABuffer, AStart, AStop) then
        gtk_text_buffer_cut_clipboard(ABuffer, gtk_clipboard_get(GDK_SELECTION_CLIPBOARD), not ACustomEdit.ReadOnly);
    end;
  end else
    gtk_editable_cut_clipboard({%H-}PGtkEditable(ACustomEdit.Handle));
end;

class procedure TGtk2WSCustomEdit.Copy(const ACustomEdit: TCustomEdit);
var
  ATextView: PGtkTextView;
  ABuffer: PGtkTextBuffer;
  AStart, AStop: PGtkTextIter;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'Copy') then
    Exit;
  if ACustomEdit.FCompStyle = csMemo then
  begin
    ATextView := GTK_TEXT_VIEW(GetWidgetInfo({%H-}PGtkWidget(ACustomEdit.Handle))^.CoreWidget);
    ABuffer := gtk_text_view_get_buffer(ATextView);
    if ABuffer <> nil then
    begin
      AStart := nil;
      AStop := nil;
      if gtk_text_buffer_get_selection_bounds(ABuffer, AStart, AStop) then
        gtk_text_buffer_copy_clipboard(ABuffer, gtk_clipboard_get(GDK_SELECTION_CLIPBOARD));
    end;
  end else
    gtk_editable_copy_clipboard({%H-}PGtkEditable(ACustomEdit.Handle));
end;

class procedure TGtk2WSCustomEdit.Paste(const ACustomEdit: TCustomEdit);
var
  ATextView: PGtkTextView;
  ABuffer: PGtkTextBuffer;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'Paste') then
    Exit;
  if ACustomEdit.FCompStyle = csMemo then
  begin
    ATextView := GTK_TEXT_VIEW(GetWidgetInfo({%H-}PGtkWidget(ACustomEdit.Handle))^.CoreWidget);
    ABuffer := gtk_text_view_get_buffer(ATextView);
    if ABuffer <> nil then
      gtk_text_buffer_paste_clipboard(ABuffer,
        gtk_clipboard_get(GDK_SELECTION_CLIPBOARD), nil, not ACustomEdit.ReadOnly);
  end else
    gtk_editable_paste_clipboard({%H-}PGtkEditable(ACustomEdit.Handle));
end;

class procedure TGtk2WSCustomEdit.Undo(const ACustomEdit: TCustomEdit);
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'Undo') then
    Exit;
  //TODO: I cannot find anything usefull in gtk2 to do this, seem
  //that we have to make our own implementation.
end;

class procedure TGtk2WSCustomComboBox.ReCreateCombo(
  const ACustomComboBox: TCustomComboBox; const AWithEntry: Boolean;
  const AWidgetInfo: PWidgetInfo);
var
  ComboWidget: PGtkWidget;
  Model: PGtkTreeModel;
  Index: Integer;
  Box: PGtkWidget;
  ItemList: TGtkListStoreStringList;
  LCLIndex: PLongint;
begin
  Box:={%H-}PGtkWidget(ACustomComboBox.Handle);
  ComboWidget := AWidgetInfo^.CoreWidget;
  
  // keep the model (increase ref count)
  Model := gtk_combo_box_get_model(PGtkComboBox(ComboWidget));
  g_object_ref(G_OBJECT(Model));
  // keep items
  ItemList := ACustomComboBox.Items as TGtkListStoreStringList;
  
  LCLIndex := AWidgetInfo^.UserData;
  if not Assigned(LCLIndex) then begin
    //debugln('Gtk2WSCustomComboBox ReCreateCombo: LCLIndex unassigned!');
    LCLIndex := New(PLongint);
    LCLIndex^ := -1;
    AWidgetInfo^.UserData := LCLIndex;
    AWidgetInfo^.DataOwner := True;
  end;
  
  Index := GetItemIndex(ACustomComboBox);

  if PGtkComboBoxPrivate(PGtkComboBox(ComboWidget)^.priv)^.button <> nil then
    FreeWidgetInfo(PGtkComboBoxPrivate(PGtkComboBox(ComboWidget)^.priv)^.button);

  gtk_event_box_set_above_child(PGtkEventBox(Box), false);
  // don't remove Combo from Box, just destroy it and during destroy it will
  // be removed by gtk code. Removing from Box and then destroyng can lead to
  // double destroying since removing decrease reference and it can be the
  // last reference
  gtk_widget_destroy(ComboWidget);

  // create the new widget with the old model
  case AWithEntry of
    True : ComboWidget := gtk_combo_box_entry_new_with_model(Model, 0);
    False: ComboWidget := gtk_combo_box_new_with_model(Model);
  end;
  SetSensitivity(ACustomCombobox, ComboWidget);
  // undone the above increase of the ref count
  g_object_set_data(PGObject(ComboWidget),GtkListItemLCLListTag,ItemList);
  g_object_unref (G_OBJECT(Model));

  SetMainWidget(Box, GTK_BIN(ComboWidget)^.child);
  AWidgetInfo^.CoreWidget := ComboWidget;
  g_object_set_data(Pointer(ComboWidget), 'widgetinfo', AWidgetInfo);
  
  SetItemIndex(ACustomComboBox, Index);
  
  if AWithEntry then begin
    SetMaxLength(ACustomComboBox, TComboBox(ACustomComboBox).MaxLength);
  end;

  SetRenderer(ACustomComboBox, ComboWidget, AWidgetInfo);
  
  gtk_container_add(PGtkContainer(Box), ComboWidget);
  gtk_widget_show_all(Box);
  if ACustomComboBox.HandleObjectShouldBeVisible then
    gtk_widget_show(Box)
  else
    gtk_widget_hide(Box);
  if csDesigning in ACustomComboBox.ComponentState then
    gtk_event_box_set_above_child(PGtkEventBox(Box), true);

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
  if not (ACustomComboBox.Style in [csOwnerDrawFixed, csOwnerDrawVariable, csOwnerDrawEditableFixed, csOwnerDrawEditableVariable]) then
    gtk_cell_layout_set_attributes(PGtkCellLayout(AWidget), renderer, ['text', 0, nil]);
  gtk_cell_layout_set_cell_data_func(PGtkCellLayout(AWidget), renderer,
    @LCLIntfCellRenderer_CellDataFunc, AWidgetInfo, nil);
end;

procedure GtkComboFocus({%H-}AWidget: PGtkWidget; WidgetInfo: PWidgetInfo); cdecl;
begin
  LCLSendSetFocusMsg(TControl(WidgetInfo^.LCLObject));
end;

{used only for gtk2 < 2.10 }
procedure GtkPopupShowCB({%H-}AMenu: PGtkMenuShell; WidgetInfo: PWidgetInfo); cdecl;
begin
  g_object_set_data(PGObject(WidgetInfo^.CoreWidget),
    'popup-shown-compat',GPointer(PtrUInt(1)));
  LCLSendSetFocusMsg(TControl(WidgetInfo^.LCLObject));
  // let the LCL change the items on the fly:
  LCLSendDropDownMsg(TControl(WidgetInfo^.LCLObject));
end;

{used only for gtk2 < 2.10 }
procedure GtkPopupHideCB({%H-}AMenu: PGtkMenuShell; WidgetInfo: PWidgetInfo); cdecl;
begin
  g_object_set_data(PGObject(WidgetInfo^.CoreWidget),
    'popup-shown-compat',GPointer(PtrUInt(0)));
  LCLSendCloseUpMsg(TControl(WidgetInfo^.LCLObject));
end;

function GtkPopupCloseUp(WidgetInfo: Pointer): gboolean; cdecl;
begin
  LCLSendCloseUpMsg(TControl(PWidgetInfo(WidgetInfo)^.LCLObject));
  Result := gtk_False;// stop the timer
end;

procedure GtkNotifyCB(AObject: PGObject; pspec: PGParamSpec; WidgetInfo: PWidgetInfo); cdecl;
var
  AValue: TGValue;
  AMenu: PGtkWidget;
  ComboBox: TCustomComboBox;
begin
  if pspec^.name = 'popup-shown' then
  begin
    LCLSendSetFocusMsg(TControl(WidgetInfo^.LCLObject));
    FillChar(AValue{%H-}, SizeOf(AValue), 0); // fill by zeros
    g_value_init(@AValue, G_TYPE_BOOLEAN); // initialize for boolean
    g_object_get_property(AObject, pspec^.name, @AValue); // get property value
    if AValue.data[0].v_int = 0 then // if 0 = False then it is close up
      gtk_timeout_add(0,@GtkPopupCloseUp, WidgetInfo)
    else // in other case it is drop down
    begin
      ComboBox:=WidgetInfo^.LCLObject as TCustomComboBox;
      ComboBox.IntfGetItems;
      LCLSendDropDownMsg(ComboBox);
      AMenu := PGtkComboBoxPrivate(PGtkComboBox(WidgetInfo^.CoreWidget)^.priv)^.popup_widget;
      if GTK_IS_MENU(AMenu) then
        gtk_menu_reposition(PGtkMenu(AMenu));
    end;
  end;
end;

procedure GtkChangedCB({%H-}AWidget: PGtkWidget; WidgetInfo: PWidgetInfo); cdecl;
var
  LCLIndex: PLongint;
  Index, GtkIndex: Integer;
begin
  if WidgetInfo^.ChangeLock > 0 then Exit;
  LCLSendChangedMsg(TControl(WidgetInfo^.LCLObject));
  
  Index := -1;
  LCLIndex := WidgetInfo^.UserData;
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
  if AButton <> nil then
  begin
    BtnPressID := g_signal_lookup('button_press_event', GTK_TYPE_COMBO_BOX);
    HandlerID := g_signal_handler_find(AButton, G_SIGNAL_MATCH_ID, BtnPressID, 0, nil, nil, nil);
    if HandlerID > 0 then
      g_signal_handler_disconnect(AButton, HandlerID);
  end;
  
  g_signal_connect(ComboWidget, 'changed', TGCallback(@GtkChangedCB), AWidgetInfo);

  // First the combo (or the entry)
  if gtk_is_combo_box_entry(ComboWidget) then
    InputObject := AEntry
  else
    InputObject := AGtkObject;

  if not TCustomComboBox(AWinControl).Style.HasEditBox then
  begin
    // Just a combobox without a edit should handle its own keys. Issue #32458
    Gtk2WidgetSet.SetCallbackDirect(LM_KEYDOWN, InputObject, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_KEYUP, InputObject, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_CHAR, InputObject, AWinControl);
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
  Gtk2WidgetSet.SetCallbackDirect(LM_MOUSEHWHEEL, InputObject, AWinControl);
  Gtk2WidgetSet.SetCallbackDirect(LM_PAINT, InputObject, AWinControl);
  Gtk2WidgetSet.SetCallbackDirect(LM_FOCUS, InputObject, AWinControl);

  // And now the same for the Button in the combo
  if AButton<>nil then begin
    if not TCustomComboBox(AWinControl).Style.HasEditBox then
    begin
      // Just a combobox without a edit should handle its own keys. Issue #32458
      Gtk2WidgetSet.SetCallbackDirect(LM_KEYDOWN, AButton, AWinControl);
      Gtk2WidgetSet.SetCallbackDirect(LM_KEYUP, AButton, AWinControl);
      Gtk2WidgetSet.SetCallbackDirect(LM_CHAR, AButton, AWinControl);
    end;
    if not GtkWidgetIsA(PGtkWidget(AButton),GTK_TYPE_CELL_VIEW) then begin
      Gtk2WidgetSet.SetCallbackDirect(LM_MOUSEENTER, AButton, AWinControl);
      Gtk2WidgetSet.SetCallbackDirect(LM_MOUSELEAVE, AButton, AWinControl);
    end;
    Gtk2WidgetSet.SetCallbackDirect(LM_MOUSEMOVE, AButton, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_LBUTTONDOWN, AButton, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_LBUTTONUP, AButton, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_LBUTTONUP, AButton, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_RBUTTONDOWN, AButton, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_RBUTTONUP, AButton, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_MBUTTONDOWN, AButton, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_MBUTTONUP, AButton, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_MOUSEWHEEL, AButton, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_MOUSEHWHEEL, AButton, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_PAINT, AButton, AWinControl);
    Gtk2WidgetSet.SetCallbackDirect(LM_FOCUS, AButton, AWinControl);
  end;
  
  // if we are a GtkComboBoxEntry
  if not GtkWidgetIsA(PGtkWidget(AEntry), GTK_TYPE_ENTRY) then
    g_signal_connect(Combowidget, 'grab-focus', TGCallback(@GtkComboFocus), AWidgetInfo);

  AMenu := nil;
  if (APrivate^.popup_widget <> nil)
  and (GTK_IS_MENU(APrivate^.popup_widget)) then
    AMenu := GTK_OBJECT(APrivate^.popup_widget)
  else if (APrivate^.popup_window <> nil)
  and (GTK_IS_MENU(APrivate^.popup_window)) then
    AMenu := GTK_OBJECT(APrivate^.popup_window);

  if Assigned(AMenu) and
    (gtk_major_version = 2) and (gtk_minor_version < 10) then
  begin
    g_signal_connect(AMenu, 'show', G_CALLBACK(@GtkPopupShowCB), AWidgetInfo);
    g_signal_connect_after(AMenu, 'selection-done', G_CALLBACK(@GtkPopupHideCB), AWidgetInfo);
  end;

  if (gtk_major_version >= 2) and (gtk_minor_version >= 10) then
    g_signal_connect(ComboWidget, 'notify', TGCallback(@GtkNotifyCB), AWidgetInfo);

  
  // g_signal_connect(ComboWidget, 'set-focus-child', TGCallback(@GtkPopupShowCB), AWidgetInfo);
  g_object_set_data(G_OBJECT(AWidget), 'Menu', APrivate^.popup_widget);
end;

class procedure TGtk2WSCustomComboBox.SetSensitivity(AWinControl: TWinControl; AWidget: PGtkWidget);
var
  Value: TGValue;
begin
  if ((gtk_major_version = 2) and (gtk_minor_version < 14)) or
     (csDesigning in AWinControl.ComponentState) then
    Exit;
  Value.g_type := G_TYPE_BOOLEAN;
  Value.data[0].v_int := longint(gTRUE);

  g_object_set_property(PGObject(AWidget), 'button-sensitivity', @Value);
end;

class procedure TGtk2WSCustomComboBox.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
var
  Ignore: Integer;
begin
  Ignore:=0;
  GetGTKDefaultWidgetSize(AWinControl, Ignore, PreferredHeight, WithThemeSpace);
  PreferredWidth := 0;
end;

class function TGtk2WSCustomComboBox.GetDroppedDown(
  const ACustomComboBox: TCustomComboBox): Boolean;
var
  WidgetInfo: PWidgetInfo;
  Combo: PGtkComboBox;
  AValue: TGValue;
begin
  WidgetInfo := GetWidgetInfo({%H-}Pointer(ACustomComboBox.Handle));
  Combo := PGtkComboBox(WidgetInfo^.CoreWidget);

  FillChar(AValue{%H-}, SizeOf(AValue), 0);
  g_value_init(@AValue, G_TYPE_BOOLEAN);
  if (gtk_major_version = 2) and (gtk_minor_version < 10) then
  begin
    if g_object_get_data(PGObject(Combo),'popup-shown-compat') <> nil then
      AValue.data[0].v_int := 1
    else
      AValue.data[0].v_int := 0;
  end else
    g_object_get_property(PGObject(Combo), 'popup-shown', @AValue);
  Result := AValue.data[0].v_int <> 0;
end;

class function TGtk2WSCustomComboBox.GetSelStart(
  const ACustomComboBox: TCustomComboBox): integer;
var
  WidgetInfo: PWidgetInfo;
  Entry: PGtkEntry;
  AStart, AEnd: gint;
begin
  Result := 0;
  WidgetInfo := GetWidgetInfo({%H-}Pointer(ACustomComboBox.Handle));

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
  WidgetInfo := GetWidgetInfo({%H-}Pointer(ACustomComboBox.Handle));

  // if the combo is an editable ...
  Entry := GetComboBoxEntry(WidgetInfo^.CoreWidget);
  if Entry<>nil then
  begin
    if not gtk_editable_get_selection_bounds(PGtkEditable(Entry), @AStart, @AEnd) then
      Exit(0);
    Result := ABS(AStart - AEnd);
  end;
end;

class function TGtk2WSCustomComboBox.GetItemIndex(
  const ACustomComboBox: TCustomComboBox): integer;
var
  WidgetInfo: PWidgetInfo;
begin
  WidgetInfo := GetWidgetInfo({%H-}Pointer(ACustomComboBox.Handle));
  
  Result := gtk_combo_box_get_active(PGtkComboBox(WidgetInfo^.CoreWidget));
end;

class function TGtk2WSCustomComboBox.GetMaxLength(
  const ACustomComboBox: TCustomComboBox): integer;
var
  WidgetInfo: PWidgetInfo;
  Entry: PGtkEntry;
begin
  WidgetInfo := GetWidgetInfo({%H-}Pointer(ACustomComboBox.Handle));

  // if the combo is an editable ...
  Entry := GetComboBoxEntry(WidgetInfo^.CoreWidget);
  if Entry<>nil then begin
    Result := gtk_entry_get_max_length(Entry);
  end
  else begin
     Result := integer({%H-}PtrUInt(g_object_get_data(PGObject(WidgetInfo^.CoreWidget), 'max-length')));
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
  WidgetInfo := GetWidgetInfo({%H-}Pointer(AWinControl.Handle));

  // if the combo is an editable ...
  Entry := GetComboBoxEntry(WidgetInfo^.CoreWidget);
  if Entry<>nil then begin
    AText := gtk_entry_get_text(Entry);
    exit;
  end;
  
  // if we are a fixed un-editable combo then ...
  Index := GetItemIndex(TCustomComboBox(AWinControl));
  if Index > -1 then  AText := TCustomComboBox(AWinControl).Items.Strings[Index];
end;

class procedure TGtk2WSCustomComboBox.SetArrowKeysTraverseList(
  const ACustomComboBox: TCustomComboBox; NewTraverseList: boolean);
begin
  // TODO: TGtk2WSCustomComboBox.SetArrowKeysTraverseList: not supported
  // This is not an option that is available for this widget
  // we will have to eat the keystrokes to set this to false
end;

class procedure TGtk2WSCustomComboBox.SetDroppedDown(
  const ACustomComboBox: TCustomComboBox; ADroppedDown: Boolean);
var
  WidgetInfo: PWidgetInfo;
  Combo: PGtkComboBox;
begin
  WidgetInfo := GetWidgetInfo({%H-}Pointer(ACustomComboBox.Handle));
  Combo := PGtkComboBox(WidgetInfo^.CoreWidget);

  case ADroppedDown of
    True : gtk_combo_box_popup(Combo);
    False: gtk_combo_box_popdown(Combo);
  end;
end;

class procedure TGtk2WSCustomComboBox.SetSelStart(
  const ACustomComboBox: TCustomComboBox; NewStart: integer);
var
  WidgetInfo: PWidgetInfo;
  Entry: PGtkEntry;
begin
  WidgetInfo := GetWidgetInfo({%H-}Pointer(ACustomComboBox.Handle));
  
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
  WidgetInfo := GetWidgetInfo({%H-}Pointer(ACustomComboBox.Handle));

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
  WidgetInfo := GetWidgetInfo({%H-}Pointer(ACustomComboBox.Handle));
  p := WidgetInfo^.CoreWidget;
  if gtk_combo_box_get_active(PGtkComboBox(p)) = NewIndex then exit;
  // to be delphi compatible OnChange only fires in response to user actions not program actions
  // so we use WidgetInfo^.ChangeLock as a flag to not signal the OnChange Event
  Inc(WidgetInfo^.ChangeLock);
  gtk_combo_box_set_active(PGtkComboBox(p), NewIndex);

  if (NewIndex = -1) and gtk_is_combo_box_entry(p) then
    gtk_entry_set_text(PGtkEntry(GTK_BIN(p)^.child), PChar(''));

  LCLIndex := WidgetInfo^.UserData;
  if not Assigned(LCLIndex) then
  begin
    LCLIndex := New(PLongint);
    WidgetInfo^.UserData := LCLIndex;
    WidgetInfo^.DataOwner := True;
  end;
  LCLIndex^ := NewIndex;

  Dec(WidgetInfo^.ChangeLock);
end;

class procedure TGtk2WSCustomComboBox.SetMaxLength(
  const ACustomComboBox: TCustomComboBox; NewLength: integer);
var
  WidgetInfo: PWidgetInfo;
  Entry: PGtkEntry;
begin
  WidgetInfo := GetWidgetInfo({%H-}Pointer(ACustomComboBox.Handle));

  Entry := GetComboBoxEntry(WidgetInfo^.CoreWidget);
  if Entry<>nil then begin
    gtk_entry_set_max_length(Entry, NewLength);
  end;
  // We save this in the CoreWidget for when the Entry Changes styles
  g_object_set_data(PGObject(WidgetInfo^.CoreWidget), 'max-length', {%H-}Pointer(PtrInt(NewLength)));
end;

class procedure TGtk2WSCustomComboBox.SetStyle(
  const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle);
var
  WidgetInfo: PWidgetInfo;
  p: PGtkWidget;
  NeedEntry: Boolean;
begin
  WidgetInfo := GetWidgetInfo({%H-}Pointer(ACustomComboBox.Handle));
  p := WidgetInfo^.CoreWidget;
  case NewStyle of
    csDropDown,
    csSimple,
    csOwnerDrawEditableFixed,
    csOwnerDrawEditableVariable:
      NeedEntry := True;
    csDropDownList,
    csOwnerDrawFixed,
    csOwnerDrawVariable:
      NeedEntry := False;
  end;
  if gtk_is_combo_box_entry(p) = NeedEntry then Exit;
  ReCreateCombo(ACustomComboBox, NeedEntry, WidgetInfo);
end;

class function TGtk2WSCustomComboBox.GetItems(
  const ACustomComboBox: TCustomComboBox): TStrings;
var
  ComboWidget: PGtkWidget;
  Handle: HWND;
begin
  Handle := ACustomComboBox.Handle;
  ComboWidget := GetOrCreateWidgetInfo({%H-}Pointer(Handle))^.CoreWidget;
  Result :=  TGtkListStoreStringList(g_object_get_data(PGObject(ComboWidget),
                                     GtkListItemLCLListTag));
end;

class procedure TGtk2WSCustomComboBox.Sort(const ACustomComboBox: TCustomComboBox;
  AList: TStrings; IsSorted: boolean);
var
  ComboWidget: PGtkWidget;
  Handle: HWND;
begin
  Handle := ACustomComboBox.Handle;
  ComboWidget := GetOrCreateWidgetInfo({%H-}Pointer(Handle))^.CoreWidget;
  TGtkListStoreStringList(g_object_get_data(PGObject(ComboWidget),
                                     GtkListItemLCLListTag)).Sorted := IsSorted;
end;

class procedure TGtk2WSCustomComboBox.SetColor(const AWinControl: TWinControl);
var
  WidgetInfo: PWidgetInfo;
  Child: PGtkWidget; // can be GtkCellRenderer or GtkEntry
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetColor') then
    Exit;
  WidgetInfo := GetWidgetInfo({%H-}Pointer(AWinControl.Handle));

  Child := GTK_BIN(WidgetInfo^.CoreWidget)^.child;
  Gtk2WidgetSet.SetWidgetColor(Child, AWinControl.Font.Color, AWinControl.Color,
   [GTK_STATE_NORMAL,GTK_STYLE_BASE]);
end;

class procedure TGtk2WSCustomComboBox.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  Entry: PGtkEntry;
  WidgetInfo: PWidgetInfo;
  W: PGtkWidget;
begin
  if not AWinControl.HandleAllocated then exit;

  WidgetInfo := GetWidgetInfo({%H-}Pointer(AWinControl.Handle));
  Entry := GetComboBoxEntry(WidgetInfo^.CoreWidget);

  if Entry <> nil then
  begin
    Gtk2WidgetSet.SetWidgetColor(PGtkWidget(Entry), AFont.Color, clNone,
       [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED,GTK_STYLE_TEXT]);
    Gtk2WidgetSet.SetWidgetFont(PGtkWidget(Entry), AFont);
  end else
  begin
    W := GTK_BIN(WidgetInfo^.CoreWidget)^.child;
    if W <> nil then
    begin
      Gtk2WidgetSet.SetWidgetColor(W, AFont.Color, clNone,
        [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED,GTK_STYLE_TEXT]);
      Gtk2WidgetSet.SetWidgetFont(W, AFont);
    end;
  end;
end;

class procedure TGtk2WSCustomComboBox.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  WidgetInfo: PWidgetInfo;
  Entry: PGtkWidget;
begin
  WidgetInfo := GetWidgetInfo({%H-}Pointer(AWinControl.Handle));
  // we use user ChangeLock to not signal onchange
  Inc(WidgetInfo^.ChangeLock);
  if gtk_is_combo_box_entry(WidgetInfo^.CoreWidget) then
  begin
    Entry := GTK_BIN(WidgetInfo^.CoreWidget)^.child;
    gtk_entry_set_text(PGtkEntry(Entry), PChar(AText));
  end;
  Dec(WidgetInfo^.ChangeLock);
end;

class procedure TGtk2WSCustomComboBox.ShowHide(const AWinControl: TWinControl);
begin
  Gtk2WidgetSet.SetVisible(AWinControl, AWinControl.HandleObjectShouldBeVisible);
  InvalidateLastWFPResult(AWinControl, AWinControl.BoundsRect);
end;

class function TGtk2WSCustomComboBox.CanFocus(const AWinControl: TWinControl
  ): boolean;
var
  WidgetInfo: PWidgetInfo;
  Entry: PGtkWidget;
begin
  if not AWinControl.HandleAllocated then exit(false);
  WidgetInfo := GetWidgetInfo({%H-}Pointer(AWinControl.Handle));
  if gtk_is_combo_box_entry(WidgetInfo^.CoreWidget) then begin
    Entry := GTK_BIN(WidgetInfo^.CoreWidget)^.child;
    Result:=GTK_WIDGET_CAN_FOCUS(Entry);
  end else begin
    Result:=inherited CanFocus(AWinControl);
  end;
  //DebugLn(['TGtk2WSCustomComboBox.CanFocus ',dbgsName(AWinControl),' ',gtk_is_combo_box_entry(WidgetInfo^.CoreWidget),' Result=',Result]);
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
  NeedEntry: Boolean;
begin
  ACustomComboBox := TCustomComboBox(AWinControl);

  Box := gtk_event_box_new;
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Box,dbgsName(AWinControl));
  {$ENDIF}

  WidgetInfo := CreateWidgetInfo(Box, AWinControl, AParams);

  ListStore := gtk_list_store_new (2, [G_TYPE_STRING, G_TYPE_POINTER, nil]);

  case ACustomComboBox.Style of
    csDropDown,
    csSimple,
    csOwnerDrawEditableFixed,
    csOwnerDrawEditableVariable:
      NeedEntry := True;
    csDropDownList,
    csOwnerDrawFixed,
    csOwnerDrawVariable:
      NeedEntry := False;
  end;
  if NeedEntry then
    ComboWidget := gtk_combo_box_entry_new_with_model(GTK_TREE_MODEL (ListStore), 0)
  else
    ComboWidget := gtk_combo_box_new_with_model(GTK_TREE_MODEL (ListStore));

  SetSensitivity(AWinControl, ComboWidget);

  g_object_unref (G_OBJECT (liststore));

  gtk_container_add(PGtkContainer(Box), ComboWidget);
  gtk_widget_show_all(Box);
  if csDesigning in AWinControl.ComponentState then
    gtk_event_box_set_above_child(PGtkEventBox(Box), true);

  SetRenderer(ACustomComboBox, ComboWidget, WidgetInfo);

  SetMainWidget(Box, ComboWidget);
  SetMainWidget(Box, GTK_BIN(ComboWidget)^.child);
  if PGtkComboBoxPrivate(PGtkComboBox(ComboWidget)^.priv)^.button <> nil then
    SetMainWidget(Box, PGtkComboBoxPrivate(PGtkComboBox(ComboWidget)^.priv)^.button);

  LCLIndex := New(PLongint);
  //Should not set the ItemIndex value here?
  LCLIndex^ := -1;
  WidgetInfo^.CoreWidget := ComboWidget;
  WidgetInfo^.ClientWidget := Box;
  WidgetInfo^.UserData := LCLIndex;
  WidgetInfo^.DataOwner := True;

  //gtk_widget_add_events(Box, GDK_ALL_EVENTS_MASK);

  SetCallbacks(AWinControl, Box, WidgetInfo);

  // Items
  ItemList:= TGtkListStoreStringList.Create(
          gtk_combo_box_get_model(PGtkComboBox(ComboWidget)),0,ACustomComboBox);
  g_object_set_data(PGObject(ComboWidget),GtkListItemLCLListTag,ItemList);
  // This is done in InitializeWnd: ItemList.Assign(ACustomComboBox.Items);
  if ACustomComboBox.Items is TStringList then
    ItemList.Sorted:=TStringList(ACustomComboBox.Items).Sorted;

  if AParams.Style and WS_VISIBLE = 0 then
    gtk_widget_hide(Box)
  else
    gtk_widget_show(Box);

  Result := TLCLIntfHandle({%H-}PtrUInt(Box));
end;

class procedure TGtk2WSCustomComboBox.DestroyHandle(
  const AWinControl: TWinControl);
var
  Handle: HWND;
  ComboWidget: PGtkWidget;
begin
  Handle := AWinControl.Handle;
  ComboWidget := GetOrCreateWidgetInfo({%H-}Pointer(Handle))^.CoreWidget;

  if PGtkComboBoxPrivate(PGtkComboBox(ComboWidget)^.priv)^.button <> nil then
    FreeWidgetInfo(PGtkComboBoxPrivate(PGtkComboBox(ComboWidget)^.priv)^.button);
  //DebugLn(['TGtk2WSCustomComboBox.DestroyHandle ',dbgsName(AWinControl),' ClassParent=',ClassParent.ClassName]);

  // inherited DestroyHandle doesn't work, because that is determined at
  // compile time, while the WS class hierarchy is created at runtime
  TWSWinControlClass(Classparent).DestroyHandle(AWinControl);
end;

{ TGtk2WSCustomGroupBox }

class procedure TGtk2WSCustomGroupBox.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
end;

class procedure TGtk2WSCustomGroupBox.SetLabel(AFrame: PGtkFrame; AText: String);
var
  Lbl: PGtkWidget;
begin
  Lbl := gtk_frame_get_label_widget(AFrame);
  if (AText = '') then
  begin
    if Lbl <> nil then
      gtk_widget_destroy(Lbl);
  end
  else
  begin
    if Lbl = nil then
    begin
      Lbl := gtk_label_new(nil);
      gtk_widget_show(Lbl);
      gtk_frame_set_label_widget(AFrame, Lbl);
    end;
    Gtk2Widgetset.SetLabelCaption(PGtkLabel(Lbl), AText);
  end;
end;

class function TGtk2WSCustomGroupBox.GetFrameWidget(AEventBox: PGtkEventBox): PGtkFrame;
var
  GBWidget: PGTKWidget;
begin
  GBWidget := PGTKWidget(AEventBox);
  Result:=PGtkFrame(PGtkBin(GBWidget)^.child);
end;

class function TGtk2WSCustomGroupBox.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLIntfHandle;
var
{$if not defined(GtkFixedWithWindow)}
  EventBox: PGtkWidget;
{$endif}
  FrameBox: PGTKWidget;
  TempWidget: PGTKWidget;       // pointer to gtk-widget (local use when neccessary)
  p : pointer;          // ptr to the newly created GtkWidget
  Allocation: TGTKAllocation;
  WidgetInfo: PWidgetInfo;
begin
  P := gtk_frame_new(nil);
  SetLabel(P, AParams.Caption);
  WidgetInfo := CreateWidgetInfo(P, AWinControl, AParams);
  {$if defined(GtkFixedWithWindow)}
  TempWidget := CreateFixedClientWidget;
  gtk_container_add(GTK_CONTAINER(p), TempWidget);
  WidgetInfo^.ClientWidget := TempWidget;
  WidgetInfo^.CoreWidget := TempWidget;
  g_object_set_data(PGObject(TempWidget), 'widgetinfo', WidgetInfo);
  {$else}
  EventBox := gtk_event_box_new;
  gtk_event_box_set_visible_window(PGtkEventBox(EventBox), False);
  TempWidget := CreateFixedClientWidget(False);
  gtk_container_add(GTK_CONTAINER(EventBox), TempWidget);
  gtk_container_add(GTK_CONTAINER(p), EventBox);
  gtk_widget_show(EventBox);
  WidgetInfo^.ClientWidget := TempWidget;
  WidgetInfo^.CoreWidget := EventBox;
  g_object_set_data(PGObject(TempWidget), 'widgetinfo', WidgetInfo);
  g_object_set_data(PGObject(EventBox), 'widgetinfo', WidgetInfo);
  {$endif}
  FrameBox := gtk_event_box_new;
  gtk_event_box_set_visible_window(PGtkEventBox(FrameBox), True);
  gtk_container_add(GTK_CONTAINER(FrameBox), p);
  g_object_set_data(PGObject(FrameBox), 'widgetinfo', WidgetInfo);
  gtk_widget_show(TempWidget);
  gtk_widget_show(P);
  if AWinControl.HandleObjectShouldBeVisible then
    gtk_widget_show(FrameBox);

  Result := TLCLIntfHandle({%H-}PtrUInt(FrameBox));

  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(FrameBox, @Allocation);

  Set_RC_Name(AWinControl, FrameBox);
  SetCallbacks(FrameBox, WidgetInfo);
end;

class procedure TGtk2WSCustomGroupBox.SetColor(const AWinControl: TWinControl);
var
  GBWidget: PGTKWidget;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetColor') then
    Exit;
  GBWidget:={%H-}PGTKWidget(AWinControl.Handle);

  {$if defined(GtkFixedWithWindow)}
    Gtk2WidgetSet.SetWidgetColor(GetFixedWidget(GBWidget), clNone, AWinControl.Color,
       [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
  {$endif}
    Gtk2WidgetSet.SetWidgetColor(GBWidget, clNone, AWinControl.Color,
       [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
end;

class function TGtk2WSCustomGroupBox.GetDefaultClientRect(
  const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer;
  var aClientRect: TRect): boolean;
var
  FrameBorders: TRect;
  Widget: PGtkWidget;
  FixedWidget: PGtkWidget;
begin
  Result:=false;
  //DebugLn(['TGtk2WSCustomGroupBox.GetDefaultClientRect ',DbgSName(AWinControl),' ',aWidth,'x',aHeight]);
  if AWinControl.HandleAllocated then begin
    Widget:={%H-}PGtkWidget(AWinControl.Handle);
    FixedWidget:=PGtkWidget(GetFixedWidget(Widget));
    //DebugLn(['TGtk2WSCustomGroupBox.GetDefaultClientRect Flags=',WidgetFlagsToString(Widget),' FixedFlags=',WidgetFlagsToString(FixedWidget),' FixedSize=',FixedWidget^.allocation.width,'x',FixedWidget^.allocation.height]);
    if not GTK_WIDGET_RC_STYLE(FixedWidget) then
      Result:=true;
  end else begin
    Result:=true;
  end;
  if Result then begin
    FrameBorders:=GetStyleGroupboxFrameBorders;
    aClientRect:=Rect(0,0,
                 Max(0,aWidth-FrameBorders.Left-FrameBorders.Right),
                 Max(0,aHeight-FrameBorders.Top-FrameBorders.Bottom));
  end;
  //if Result then DebugLn(['TGtk2WSCustomGroupBox.GetDefaultClientRect END FrameBorders=',dbgs(FrameBorders),' aClientRect=',dbgs(aClientRect)]);
end;

class procedure TGtk2WSCustomGroupBox.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  // ToDo: compute the minimum size ignoring LCL child controls
  GetGTKDefaultWidgetSize(AWinControl, PreferredWidth, PreferredHeight,
                          WithThemeSpace);
end;

class procedure TGtk2WSCustomGroupBox.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  Frame: PGtkFrame;
  Lbl: PGtkWidget;
begin
  Frame := GetFrameWidget({%H-}PGTKEventBox(AWinControl.Handle));
  Lbl := gtk_frame_get_label_widget(Frame);

  if Lbl <> nil then
  begin
    Gtk2WidgetSet.SetWidgetColor(Lbl, AFont.Color, clNone,
      [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
    Gtk2WidgetSet.SetWidgetFont(Lbl, AFont);
  end;
  inherited SetFont(AWinControl, AFont);
end;

class procedure TGtk2WSCustomGroupBox.SetText(const AWinControl: TWinControl;
  const AText: string);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetText') then Exit;
  SetLabel(GetFrameWidget({%H-}PGtkEventBox(AWinControl.Handle)), AText);
end;

class procedure TGtk2WSCustomGroupBox.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
var
  GroubBox: TCustomGroupBox absolute AWinControl;
  Frame: PGtkFrame;
  Lbl: PGtkWidget;
  MinWidth: NativeInt;
begin
  Frame := GetFrameWidget({%H-}PGTKEventBox(AWinControl.Handle));
  Lbl := gtk_frame_get_label_widget(Frame);
  if Lbl <> nil then
  begin
    MinWidth := Lbl^.allocation.x * 2;
    if AWidth < MinWidth then
    begin
      SetText(AWinControl, '');
      g_object_set_data(PGObject(Frame), 'lcl-groupbox-min-width', {%H-}gPointer(MinWidth));
    end;
  end
  else if GroubBox.Caption <> '' then
  begin
    {%H-}gPointer(MinWidth) := g_object_get_data(PGObject(Frame), 'lcl-groupbox-min-width');
    if (MinWidth > 0) and (AWidth >= MinWidth) then begin
      SetText(AWinControl, GroubBox.Caption);
      g_object_set_data(PGObject(Frame), 'lcl-groupbox-min-width', nil);
    end;
  end;
  TGtk2WSWinControl.SetBounds(AWinControl, ALeft, ATop, AWidth, AHeight);
end;

function Gtk2WSButton_Clicked(AWidget: PGtkWidget; AInfo: PWidgetInfo): GBoolean; cdecl;
var
  Msg: TLMessage;
begin
  Result := CallBackDefaultReturn;
  if AInfo^.ChangeLock > 0 then Exit;

   // do not send LM_CLICKED. issue #21483
  if g_object_get_data(PGObject(AWidget),'lcl-button-stop-clicked') = AWidget then
  begin
    g_object_set_data(PGObject(AWidget),'lcl-button-stop-clicked', nil);
    exit;
  end;
  Msg.Msg := LM_CLICKED;
  Result := DeliverMessage(AInfo^.LCLObject, Msg) = 0;
end;

function Gtk2WSButtonPressedEvent(widget: PGtkWidget; {%H-}event: pgdkEventButton; {%H-}data: gPointer): GBoolean; cdecl;
begin
  Result := CallBackDefaultReturn;
  // set to nil data if we pressed return before,
  // otherwise LM_CLICKED won't trigger. issue #21483
  g_object_set_data(PGObject(Widget),'lcl-button-stop-clicked', nil);
end;

procedure Gtk2WSButton_SizeAllocate(widget: PGtkWidget; {%H-}allocation: PGtkAllocation; {%H-}user_data: gpointer); cdecl;
var
  xthickness, ythickness: gint;
  inner_border: PGtkBorder;
begin
  //the default GtkButton size_allocate handler takes into account
  //*thickness and inner_border properties to position the child (label)
  //see gtk_button_size_allocate in gtkbutton.c
  //here this is reverted so the child is not padded
  xthickness := widget^.style^.xthickness;
  ythickness := widget^.style^.ythickness;
  with PGtkBin(widget)^.child^.allocation do
  begin
    y := y - ythickness;
    height := height + 2 * ythickness;
    x := x - xthickness;
    width := width + 2 * xthickness;
    inner_border := nil;
    if gtk_minor_version > 8 then
      gtk_widget_style_get (widget, 'inner-border', @inner_border, nil);
    if inner_border <> nil then
    begin
       x := x - inner_border^.left;
       width := width + inner_border^.left + inner_border^.right;
       y := y - inner_border^.top;
       height := height + inner_border^.top + inner_border^.bottom;
       gtk_border_free(inner_border);
    end
    else
    begin
      //if no inner-border is set, GtkButton uses a default border = (1,1,1,1)
      dec(x);
      dec(y);
      inc(width, 2);
      inc(height, 2);
    end;
  end;
end;

{ TGtk2WSButton }

class function TGtk2WSButton.GetButtonWidget(AEventBox: PGtkEventBox): PGtkButton;
begin
  Result := PGtkButton(PGtkBin(AEventBox)^.child);
end;

class function TGtk2WSButton.GetLabelWidget(AEventBox: PGtkEventBox): PGtkLabel;
begin
  Result := PGtkLabel(PGtkBin(GetButtonWidget(AEventBox))^.child);
end;

class procedure TGtk2WSButton.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
  SignalConnect(AWidgetInfo^.CoreWidget, 'clicked', @Gtk2WSButton_Clicked, AWidgetInfo);
  SignalConnect(AWidgetInfo^.CoreWidget, 'button-press-event', @Gtk2WSButtonPressedEvent, AWidgetInfo);
  SignalConnect(AWidgetInfo^.CoreWidget, 'size-allocate', @Gtk2WSButton_SizeAllocate, AWidgetInfo);
end;

{
  Under Gtk 2 we need to put a GtkEventBox under the GtkButton, because a
  GtkButton has no window and that causes the Z-Order to be wrong.
}
class function TGtk2WSButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Button: TCustomButton;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
  EventBox, BtnWidget: PGtkWidget;
begin
  Button := AWinControl as TCustomButton;
  //DebugLn(['TGtk2WSButton.CreateHandle ',dbgsName(Button)]);

  { Creates the container control for the button, the EventBox }
  EventBox := gtk_event_box_new;
  Result := TLCLIntfHandle({%H-}PtrUInt(EventBox));
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(EventBox,'button');
  {$ENDIF}

  { Creates the button and inserts it into the EventBox }
  BtnWidget := gtk_button_new_with_label('button');
  gtk_container_add(PGtkContainer(EventBox), BtnWidget);
  gtk_widget_show_all(EventBox);

  { This commented commands can be used if we have event-related
    problems because of the EventBox }
//  gtk_widget_add_events(EventBox, GDK_ALL_EVENTS_MASK);
//  gtk_event_box_set_above_child(PGtkEventBox(EventBox), True);

  { The WidgetInfo is important for the form designer }
  WidgetInfo := CreateWidgetInfo({%H-}Pointer(Result), Button, AParams);
  WidgetInfo^.CoreWidget := BtnWidget;
  WidgetInfo^.ClientWidget := EventBox;
  //DebugLn(['TGtk2WSButton.CreateHandle ',GetWidgetInfo(EventBox)=WidgetInfo,' ',GetWidgetInfo(EventBox)^.ClientWidget=BtnWidget]);
//  g_object_set_data(PGObject(Result), 'widgetinfo', WidgetInfo);
  SetMainWidget(EventBox, BtnWidget);

  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(EventBox, @Allocation);

  Set_RC_Name(AWinControl, EventBox);
  SetCallbacks(EventBox, WidgetInfo);

  if AParams.Style and WS_VISIBLE = 0 then
    gtk_widget_hide(EventBox)
  else
    gtk_widget_show(EventBox);
end;

class function TGtk2WSButton.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
begin
  // The button text is static, so let the LCL fallback to FCaption
  Result := False;
end;

class procedure TGtk2WSButton.SetDefault(const AButton: TCustomButton; ADefault: Boolean);
begin
  if not WSCheckHandleAllocated(AButton, 'SetDefault')
  then Exit;

  if ADefault
  and (GTK_WIDGET_CAN_DEFAULT({%H-}pgtkwidget(AButton.Handle))) then
    //gtk_widget_grab_default(pgtkwidget(handle))
  else begin
    {DebugLn('LM_BTNDEFAULT_CHANGED ',TCustomButton(Sender).Name,':',Sender.ClassName,' widget can not grab default ',
      ' visible=',GTK_WIDGET_VISIBLE(PGtkWidget(Handle)),
      ' realized=',GTK_WIDGET_REALIZED(PGtkWidget(Handle)),
      ' mapped=',GTK_WIDGET_MAPPED(PGtkWidget(Handle)),
      '');}
    //  gtk_widget_Draw_Default(pgtkwidget(Handle));  //this isn't right but I'm not sure what to call
  end;
end;

class procedure TGtk2WSButton.SetShortcut(const AButton: TCustomButton;
  const ShortCutK1, ShortCutK2: TShortcut);
begin
  if not WSCheckHandleAllocated(AButton, 'SetShortcut')
  then Exit;
  // gtk2: shortcuts are handled by the LCL
end;

class procedure TGtk2WSButton.SetText(const AWinControl: TWinControl; const AText: String);
var
  BtnWidget: PGtkButton;
  LblWidget: PGtkLabel;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetText')
  then Exit;

  BtnWidget := GetButtonWidget({%H-}PGtkEventBox(AWinControl.Handle));
  LblWidget := PGtkLabel(PGtkBin(BtnWidget)^.Child);

  if LblWidget = nil
  then begin
    //DebugLn(Format('trace: [WARNING] Button %s(%s) has no label', [AWinControl.Name, AWinControl.ClassName]));
    LblWidget := PGtkLabel(gtk_label_new(''));
    gtk_container_add(PGtkContainer(BtnWidget), PGtkWidget(LblWidget));
  end;

  Gtk2WidgetSet.SetLabelCaption(LblWidget, AText);
end;

class procedure TGtk2WSButton.SetColor(const AWinControl: TWinControl);
var
  BtnWidget: PGTKWidget;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetColor') then
    Exit;
  BtnWidget := PGTKWidget(GetButtonWidget({%H-}PGtkEventBox(AWinControl.Handle)));
  Gtk2WidgetSet.SetWidgetColor(BtnWidget, clNone, AWinControl.Color,
       [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
end;

class procedure TGtk2WSButton.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  LblWidget: PGtkWidget;
begin
  if not AWinControl.HandleAllocated then exit;

  LblWidget := PGtkWidget(GetLabelWidget({%H-}PGtkEventBox(AWinControl.Handle)));

  if (LblWidget <> nil) then
  begin
    Gtk2WidgetSet.SetWidgetColor(LblWidget, AFont.Color, clNone,
       [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
    Gtk2WidgetSet.SetWidgetFont(LblWidget, AFont);
  end;
end;

class procedure TGtk2WSButton.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  GetGTKDefaultWidgetSize(AWinControl,PreferredWidth,PreferredHeight,
                          WithThemeSpace);
  //debugln('TGtkWSButton.GetPreferredSize ',DbgSName(AWinControl),' PreferredWidth=',dbgs(PreferredWidth),' PreferredHeight=',dbgs(PreferredHeight));
end;

{ TGtk2WSScrollBar }

class procedure TGtk2WSScrollBar.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
  
  g_signal_connect(AGtkWidget, 'change-value', TGCallback(@Gtk2RangeScrollCB), AWidgetInfo);
  g_signal_connect(AGtkWidget, 'button-press-event',
    TGCallback(@Gtk2RangeScrollPressCB), AWidgetInfo);
  g_signal_connect(AGtkWidget, 'button-release-event',
    TGCallback(@Gtk2RangeScrollReleaseCB), AWidgetInfo);
end;

class function TGtk2WSScrollBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Adjustment: PGtkAdjustment = nil;
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  with TScrollBar(AWinControl) do
  begin
    {We use Max + PageSize because the GTK scrollbar is meant to scroll from
     min to max-pagesize which would be different from the behaviour on other
     widgetsets.}
    Adjustment := PGtkAdjustment(gtk_adjustment_new(Position, Min,
      Max, SmallChange, LargeChange, PageSize));

    if (Kind = sbHorizontal) then
      Widget := gtk_hscrollbar_new(Adjustment)
    else
      Widget := gtk_vscrollbar_new(Adjustment);
    gtk_range_set_update_policy(PGtkRange(Widget), GTK_UPDATE_CONTINUOUS);
  end;

  Result := TLCLIntfHandle({%H-}PtrUInt(Widget));
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}
  WidgetInfo := CreateWidgetInfo({%H-}Pointer(Result), AWinControl, AParams);

  Set_RC_Name(AWinControl, Widget);
  SetCallbacks(Widget, WidgetInfo);
end;

class procedure TGtk2WSScrollBar.SetKind(const AScrollBar: TCustomScrollBar;
  const AIsHorizontal: Boolean);
begin
  if not AScrollBar.HandleAllocated then
    exit;
  RecreateWnd(AScrollBar);
end;

class procedure TGtk2WSScrollBar.SetParams(const AScrollBar: TCustomScrollBar);
var
  Range: PGtkRange;
begin
  if not AScrollBar.HandleAllocated then
    exit;
  with AScrollBar do
  begin
    Range := GTK_RANGE({%H-}Pointer(Handle));
    {for gtk >= 2.14 use gtk_adjustment_configure}
    if (gtk_major_version >= 2) and (gtk_minor_version >= 14) then
      gtk_adjustment_configure(Range^.adjustment, Position, Min, Max, SmallChange, LargeChange, PageSize)
    else
    begin
      with Range^.adjustment^ do
      begin
        value := Position;
        lower := Min;
        upper := Max;
        step_increment := SmallChange;
        page_increment := LargeChange;
        page_size := PageSize;
      end;
      gtk_adjustment_changed(Range^.adjustment);
    end;
  end;
end;

class procedure TGtk2WSScrollBar.ShowHide(const AWinControl: TWinControl);
begin
  if not AWinControl.HandleAllocated then
    exit;
  if AWinControl.HandleObjectShouldBeVisible then
    SetParams(TCustomScrollBar(AWinControl));
  Gtk2WidgetSet.SetVisible(AWinControl,
    AWinControl.HandleObjectShouldBeVisible);
end;

{ TGtk2WSRadioButton }

class function TGtk2WSRadioButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Widget, TempWidget: PGtkWidget;
  LabelWidget: PGtkLabel;
  TempInt: Integer;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
begin
  with TRadioButton(AWinControl) do
  begin
    // Look for our parent's control and use the first radio we find for grouping
    TempWidget := nil;
    if (Parent <> nil) then
    begin
      for TempInt := 0 to Parent.ControlCount - 1 do
      begin
        if (Parent.Controls[TempInt] is TRadioButton) and
           TWinControl(Parent.Controls[TempInt]).HandleAllocated then
        begin
          TempWidget := {%H-}PGtkWidget(TWinControl(Parent.Controls[TempInt]).Handle);
          Break;
        end;
      end;
    end;

    if TempWidget <> nil then
      Widget := gtk_radio_button_new_with_label(PGtkRadioButton(TempWidget)^.group,'')
    else
      Widget := gtk_radio_button_new_with_label(nil, '');

    LabelWidget := PGtkLabel(gtk_bin_get_child(PGtkBin(@PGTKToggleButton(Widget)^.Button)));
    Gtk2WidgetSet.SetLabelCaption(LabelWidget, AParams.Caption);
  end;

  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}
  Result := THandle({%H-}PtrUInt(Widget));
  WidgetInfo := CreateWidgetInfo({%H-}Pointer(Result), AWinControl, AParams);

  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(Widget, @Allocation);

  Set_RC_Name(AWinControl, Widget);
  TGtk2WSCustomCheckBox.SetCallbacks(Widget, WidgetInfo);
end;

{ TGtk2WSToggleBox }

class function TGtk2WSToggleBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
begin
  Widget := gtk_toggle_button_new_with_label(AParams.Caption);
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}
  Result := THandle({%H-}PtrUInt(Widget));
  WidgetInfo := CreateWidgetInfo({%H-}Pointer(Result), AWinControl, AParams);

  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(Widget, @Allocation);

  Set_RC_Name(AWinControl, Widget);
  TGtk2WSCustomCheckBox.SetCallbacks(Widget, WidgetInfo);
end;

{ TGtk2WSCustomStaticText }

class function TGtk2WSCustomStaticText.GetLabelWidget(AFrame: PGtkFrame): PGtkLabel;
begin
  Result := PGtkLabel(PGtkBin(GetBoxWidget(AFrame))^.child);
end;

class function TGtk2WSCustomStaticText.GetBoxWidget(AFrame: PGtkFrame): PGtkEventBox;
begin
  Result := PGtkEventBox(PGtkBin(AFrame)^.child);
end;

class function TGtk2WSCustomStaticText.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLIntfHandle;
var
  AStaticText: TCustomStaticText;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
  EventBox, LblWidget: PGtkWidget;
begin
  // TStaticText control is a Text area with frame around. Both Text and Area around
  // text can have their own color

  // To implement that in gtk we need:
  // 1. GtkLabel to handle Text
  // 2. GtkEventBox to draw color area around GtkLabel (since GtkLabel have no window)
  // 3. GtkFrame to draw frame around Text area
  // GtkFrame is our main widget - it is container and it contains GtkEventBox
  // GtkEventBox is also containter and it contains GtkLabel

  AStaticText := AWinControl as TCustomStaticText;
  Result := TLCLIntfHandle({%H-}PtrUInt(gtk_frame_new(nil))); // frame is the main container - to decorate label
  if Result = 0 then Exit;

  gtk_frame_set_shadow_type({%H-}PGtkFrame(Result), StaticBorderShadowMap[AStaticText.BorderStyle]);

  EventBox := gtk_event_box_new;  // our area
  LblWidget := gtk_label_new(PChar(TCustomStaticText(AWinControl).Caption)); // our text widget
  gtk_container_add(PGtkContainer(EventBox), LblWidget);
  SetLabelAlignment(PGtkLabel(LblWidget), AStaticText.Alignment);
  gtk_widget_show(LblWidget);
  gtk_widget_show(EventBox);
  gtk_container_add({%H-}PGtkContainer(Result), EventBox);

  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Pointer(Result), dbgsName(AWinControl));
  {$ENDIF}

  WidgetInfo := CreateWidgetInfo({%H-}Pointer(Result), AStaticText, AParams);
  WidgetInfo^.CoreWidget := EventBox;
  g_object_set_data(PGObject(EventBox), 'widgetinfo', WidgetInfo);

  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate({%H-}PGtkWidget(Result), @Allocation);

  Set_RC_Name(AWinControl, {%H-}PGtkWidget(Result));
  SetCallbacks({%H-}PGtkWidget(Result), WidgetInfo);
end;

class procedure TGtk2WSCustomStaticText.SetAlignment(const ACustomStaticText: TCustomStaticText;
  const NewAlignment: TAlignment);
var
  LblWidget: PGtkLabel;
begin
  if not WSCheckHandleAllocated(ACustomStaticText, 'SetAlignment')
  then Exit;

  LblWidget := GetLabelWidget({%H-}PGtkFrame(ACustomStaticText.Handle));
  SetLabelAlignment(LblWidget, NewAlignment);
end;

class procedure TGtk2WSCustomStaticText.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  GetGTKDefaultWidgetSize(AWinControl, PreferredWidth, PreferredHeight,
                          WithThemeSpace);
  //debugln('TGtkWSCustomStaticText.GetPreferredSize ',DbgSName(AWinControl),' PreferredWidth=',dbgs(PreferredWidth),' PreferredHeight=',dbgs(PreferredHeight));
end;

class function TGtk2WSCustomStaticText.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
begin
  // The text is static, so let the LCL fallback to FCaption
  Result := False;
end;

class procedure TGtk2WSCustomStaticText.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  FrameWidget: PGtkFrame;
  LblWidget: PGtkLabel;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetText')
  then Exit;

  FrameWidget := {%H-}PGtkFrame(AWinControl.Handle);
  LblWidget := GetLabelWidget(FrameWidget);

  if TStaticText(AWinControl).ShowAccelChar and (AText <> '') then
    Gtk2WidgetSet.SetLabelCaption(LblWidget, AText)
  else
  begin
    gtk_label_set_text(LblWidget, PChar(AText));
    gtk_label_set_pattern(LblWidget, nil);
  end;
end;

class procedure TGtk2WSCustomStaticText.SetStaticBorderStyle(
  const ACustomStaticText: TCustomStaticText;
  const NewBorderStyle: TStaticBorderStyle);
begin
  if not WSCheckHandleAllocated(ACustomStaticText, 'SetStaticBorderStyle')
  then Exit;
  gtk_frame_set_shadow_type({%H-}PGtkFrame(ACustomStaticText.Handle), StaticBorderShadowMap[NewBorderStyle]);
end;

class procedure TGtk2WSCustomStaticText.SetCallbacks(
  const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));

  SignalConnect(AGtkWidget, 'grab_focus', @gtkActivateCB, AWidgetInfo);
end;

class procedure TGtk2WSCustomStaticText.SetColor(const AWinControl: TWinControl);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetColor') then Exit;

  Gtk2WidgetSet.SetWidgetColor(PGtkWidget(GetBoxWidget({%H-}PGtkFrame(AWinControl.Handle))),
                              clNone, AWinControl.Color,
                              [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,
                               GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
end;

class procedure TGtk2WSCustomStaticText.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  Widget: PGtkWidget;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetFont')
  then Exit;

  Widget := PGtkWidget(GetLabelWidget({%H-}PGtkFrame(AWinControl.Handle)));

  Gtk2WidgetSet.SetWidgetColor(Widget, AFont.Color, clNone,
       [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
  Gtk2WidgetSet.SetWidgetFont(Widget, AFont);
end;

end.
