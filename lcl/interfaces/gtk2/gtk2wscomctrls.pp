{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSComCtrls.pp                             * 
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
unit Gtk2WSComCtrls;

{$mode objfpc}{$H+}

interface

uses
// libs
  GLib2, Gtk2, Gdk2, Gdk2pixbuf,
  // LCL
  ComCtrls, Classes, FPCAdds, LCLType, LMessages, Controls, Graphics, CommCtrl,
  StdCtrls, LCLProc, ImgList, Math, Sysutils,
  // widgetset
  WSComCtrls, GtkWSComCtrls, WSLCLClasses, WSProc,
  // interface
  GtkDef, GtkProc;
  
  {$IFDEF VER1_0}
  type
  TCustomListView = class(ComCtrls.TCustomListView)
  protected
    property Columns;
    property MultiSelect;
    property SmallImages;
    property ViewStyle;
  end;
  {$ENDIF}
type

  // For simplified manipulation
  // Use GetCommonTreeViewWidgets(PGtkTreeView, var TTVWidgets)
  PTVWidgets = ^TTVWidgets;
  TTVWidgets = record
    WidgetInfo: PWidgetInfo;
    MainView: PGtkWidget; // can be a GtkTreeView or GtkIconView. You have been Warned! :)
    TreeModel: PGtkTreeModel;
    TreeSelection: PGtkTreeSelection;
  end;

type

  { TGtk2WSStatusBar }

  TGtk2WSStatusBar = class(TGtkWSStatusBar)
  private
  protected
  public
  end;

  { TGtk2WSTabSheet }

  TGtk2WSTabSheet = class(TGtkWSTabSheet)
  private
  protected
  public
  end;

  { TGtk2WSPageControl }

  TGtk2WSPageControl = class(TGtkWSPageControl)
  private
  protected
  public
  end;

  { TGtk2WSCustomListView }

  TGtk2WSCustomListView = class(TWSCustomListView)
  private
    class function IsIconView(const ALV: TCustomListView): Boolean; virtual;
    // needed when adding or removing columns to a list store
    class procedure ReCreateListStore(const ALV: TCustomListView;const TVWidgets: PTVWidgets); virtual;
    class procedure ReCreateItems(const ALV: TCustomListView); virtual;
    class procedure SetPropertyInternal(const ALV: TCustomListView; const Widgets: PTVWidgets; const AProp: TListViewProperty; const AIsSet: Boolean);
  protected
    class procedure SetCallbacks(const AScrollWidget: PGtkWidget; var Widgets: TTVWidgets; const AWidgetInfo: PWidgetInfo); virtual;
  public
    // columns
    class procedure ColumnDelete(const ALV: TCustomListView; const AIndex: Integer); override;
    class function  ColumnGetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn): Integer; override;
    class procedure ColumnInsert(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn); override;
    class procedure ColumnMove(const ALV: TCustomListView; const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn); override;
    class procedure ColumnSetAlignment(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAlignment: TAlignment); override;
    class procedure ColumnSetAutoSize(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean); override;
    class procedure ColumnSetCaption(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const ACaption: String); override;
    class procedure ColumnSetImage(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer); override;
    class procedure ColumnSetMaxWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer); override;
    class procedure ColumnSetMinWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer); override;
    class procedure ColumnSetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer); override;
    class procedure ColumnSetVisible(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean); override;

    // items
    class procedure ItemDelete(const ALV: TCustomListView; const AIndex: Integer); override;
    class function  ItemGetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; var AIsSet: Boolean): Boolean; override; // returns True if supported
    class procedure ItemInsert(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); override;
    class procedure ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AImageIndex: Integer); override;
    class procedure ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); override;
    class procedure ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer; const AText: String); override;
    class procedure ItemShow(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean); override;

    // lv
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;

    class procedure BeginUpdate(const ALV: TCustomListView); override;
    class procedure EndUpdate(const ALV: TCustomListView); override;

    class function GetBoundingRect(const ALV: TCustomListView): TRect; override;
    class function GetDropTarget(const ALV: TCustomListView): Integer; override;
    class function GetFocused(const ALV: TCustomListView): Integer; override;
    class function GetHoverTime(const ALV: TCustomListView): Integer; override;
    class function GetSelCount(const ALV: TCustomListView): Integer; override;
    class function GetSelection(const ALV: TCustomListView): Integer; override;
    class function GetTopItem(const ALV: TCustomListView): Integer; override;
    class function GetVisibleRowCount(const ALV: TCustomListView): Integer; override;

    class procedure SetAllocBy(const ALV: TCustomListView; const AValue: Integer); override;
    class procedure SetDefaultItemHeight(const ALV: TCustomListView; const AValue: Integer); override;
    class procedure SetHotTrackStyles(const ALV: TCustomListView; const AValue: TListHotTrackStyles); override;
    class procedure SetHoverTime(const ALV: TCustomListView; const AValue: Integer); override;
//    class procedure SetIconOptions(const ALV: TCustomListView; const AValue: TIconOptions); override;
    class procedure SetImageList(const ALV: TCustomListView; const AList: TListViewImageList; const AValue: TCustomImageList); override;
    class procedure SetProperty(const ALV: TCustomListView; const AProp: TListViewProperty; const AIsSet: Boolean); override;
    class procedure SetProperties(const ALV: TCustomListView; const AProps: TListViewProperties); override;
    class procedure SetScrollBars(const ALV: TCustomListView; const AValue: TScrollStyle); override;
    class procedure SetScrolledLeft(const ALV: TCustomListView; const AValue: Integer); override;
    class procedure SetScrolledTop(const ALV: TCustomListView; const AValue: Integer); override;
    class procedure SetSort(const ALV: TCustomListView; const AType: TSortType; const AColumn: Integer); override;
    class procedure SetViewStyle(const ALV: TCustomListView; const Avalue: TViewStyle); override;
  end;

  { TGtk2WSListView }

  TGtk2WSListView = class(TGtkWSListView)
  private
  protected
  public
  end;

  { TGtk2WSProgressBar }

  TGtk2WSProgressBar = class(TGtkWSProgressBar)
  private
  protected
  public
  end;

  { TGtk2WSCustomUpDown }

  TGtk2WSCustomUpDown = class(TGtkWSCustomUpDown)
  private
  protected
  public
  end;

  { TGtk2WSUpDown }

  TGtk2WSUpDown = class(TGtkWSUpDown)
  private
  protected
  public
  end;

  { TGtk2WSToolButton }

  TGtk2WSToolButton = class(TGtkWSToolButton)
  private
  protected
  public
  end;

  { TGtk2WSToolBar }

  TGtk2WSToolBar = class(TGtkWSToolBar)
  private
  protected
  public
  end;

  { TGtk2WSTrackBar }

  TGtk2WSTrackBar = class(TGtkWSTrackBar)
  private
  protected
  public
  end;

  { TGtk2WSCustomTreeView }

  TGtk2WSCustomTreeView = class(TGtkWSCustomTreeView)
  private
  protected
  public
  end;

  { TGtk2WSTreeView }

  TGtk2WSTreeView = class(TGtkWSTreeView)
  private
  protected
  public
  end;


implementation

uses GtkWSControls;


   // Will be used commonly for ListViews and TreeViews
   procedure GetCommonTreeViewWidgets(ATreeViewHandle: PGtkWidget; var TVWidgets: TTVWidgets);
   var
   WidgetInfo: PWidgetInfo;
   begin
     WidgetInfo := GetWidgetInfo(ATreeViewHandle);
     TVWidgets := PTVWidgets(WidgetInfo^.UserData)^;
     TVWidgets.WidgetInfo := WidgetInfo;
   end;
   
   function AlignToGtkAlign(Align: TAlignment): gfloat;
   begin
      case Align of
        taLeftJustify : AlignToGtkAlign := 0.0;
        taCenter      : AlignToGtkAlign := 0.5;
        taRightJustify: AlignToGtkAlign := 1.0;
      end;
   end;

{ TGtk2WSCustomListView }


function TGtk2WSCustomListView.IsIconView(const ALV: TCustomListView): Boolean;
begin
  Result := False;
  if ALV.ViewStyle in [vsIcon{, vsTile?}] then Result := True;
end;

procedure TGtk2WSCustomListView.ReCreateListStore(const ALV: TCustomListView; const TVWidgets: PTVWidgets);
var
GTypeArray: PGType;
NewListStore: PGtkListStore;
nColumns: Integer;
i: Integer;
begin
  nColumns := (ALV.Columns.Count*2);
  GetMem(GTypeArray, SizeOf(GType)*(nColumns+1));
  for i := 0 to ALV.Columns.Count-1 do begin
    GTypeArray[i*2] := GDK_TYPE_PIXBUF;
    GTypeArray[(i*2)+1] := G_TYPE_STRING;
  end;
  GTypeArray[nColumns] := 0;
  
  NewListStore := gtk_list_store_newv(nColumns, GTypeArray);
  ReAllocMem(GTypeArray, 0);
  
  gtk_tree_view_set_model(PGtkTreeView(TVWidgets^.MainView), PGtkTreeModel(NewListStore));
  TVWidgets^.TreeModel := NewListStore;
end;

procedure TGtk2WSCustomListView.ReCreateItems(const ALV: TCustomListView);
var
Widgets: TTVWidgets;
i,x: Integer;
Item: TListItem;
begin
  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  gtk_list_store_clear(PGtkListStore(Widgets.TreeModel));
  for i := 0 to ALV.Items.Count-1 do begin
    Item := ALV.Items.Item[i];
    ItemInsert(ALV, i , Item);
    ItemSetText(ALV, i, Item, 0, Item.Caption);
    for X := 0 to Min(Item.SubItems.Count-1, ALV.Columns.Count-2) do begin
      ItemSetText(ALV, i, Item, x+1, Item.SubItems.Strings[x]);
    end;
  end;
end;

procedure TGtk2WSCustomListView.SetPropertyInternal(const ALV: TCustomListView;
  const Widgets: PTVWidgets; const AProp: TListViewProperty;
  const AIsSet: Boolean);
begin
  with Widgets^ do begin
    case AProp of
      lvpAutoArrange: begin
        // TODO: implement ??
      end;
      lvpCheckboxes: begin
        // TODO: implement
      end;
      lvpColumnClick: begin
        // allow only column modifications when in report mode
        if ALV.ViewStyle <> vsReport then Exit;
        gtk_tree_view_set_headers_clickable(PGtkTreeView(MainView), AIsSet);
      end;
      lvpFlatScrollBars: begin
        // TODO: implement ??
      end;
      lvpFullDrag: begin
        // TODO: implement ??
      end;
      lvpGridLines: begin
        // TODO: better implementation
        // maybe possible with some cellwidget hacking
        // this create rows with alternating colors
        gtk_tree_view_set_rules_hint(PGtkTreeView(MainView), AIsSet);
      end;
      lvpHideSelection: begin
        // TODO: implement
        // should be possible with some focus in/out events
      end;
      lvpHotTrack: begin
        // TODO: implement
        // should be possible with some mouse tracking
      end;
      lvpMultiSelect: begin
        if AIsSet
        then gtk_tree_selection_set_mode(TreeSelection, GTK_SELECTION_MULTIPLE)
        else gtk_tree_selection_set_mode(TreeSelection, GTK_SELECTION_SINGLE);
      end;
      lvpOwnerDraw: begin
        // TODO: implement
        // use custom images/widgets ?
      end;
      lvpReadOnly: begin
        // TODO: implement inline editor ?
      end;
      lvpRowSelect: begin
        // TODO: implement ???
        // how to do cell select
      end;
      lvpShowColumnHeaders: begin
        // allow only column modifications when in report mode
        if ALV.ViewStyle <> vsReport then Exit;
        gtk_tree_view_set_headers_visible(GTK_TREE_VIEW (MainView), AIsSet);
      end;
      lvpShowWorkAreas: begin
        // TODO: implement ???
      end;
      lvpWrapText: begin
        // TODO: implement ???
      end;
    end;
  end;
end;

procedure TGtk2WSCustomListView.SetCallbacks(const AScrollWidget: PGtkWidget;
  var Widgets: TTVWidgets; const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSBaseScrollingWinControl.SetCallbacks(AScrollWidget, AWidgetInfo);
end;

procedure TGtk2WSCustomListView.ColumnDelete(const ALV: TCustomListView;
  const AIndex: Integer);
var
Widgets: TTVWidgets;
GtkColumn: PGtkTreeViewColumn;
begin

  if Not(ALV.HandleAllocated) then exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  
  ReCreateListStore(ALV, PTVWidgets(Widgets.WidgetInfo^.UserData));
  ReCreateItems(ALV);
  
  with Widgets do begin
    GtkColumn := gtk_tree_view_get_column(PGtkTreeView(MainView), AIndex);
    gtk_tree_view_remove_column(PGtkTreeView(MainView), GtkColumn);
  end;
end;

function TGtk2WSCustomListView.ColumnGetWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn): Integer;
var
Widgets: TTVWidgets;
GtkColumn: PGtkTreeViewColumn;
i: Integer;
begin

  if Not(ALV.HandleAllocated) then exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets do begin
    i := AColumn.Index;
    GtkColumn := gtk_tree_view_get_column(PGtkTreeView(MainView), i);
    Result := gtk_tree_view_column_get_width(GtkColumn);
  end;
end;

procedure TGtk2WSCustomListView.ColumnInsert(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn);
var
Widgets: TTVWidgets;
column: PGtkTreeViewColumn;
pixrenderer,
textrenderer: PGtkCellRenderer;
RealIndex: Integer;
begin
  if Not(ALV.HandleAllocated) then exit;
  
  if AIndex < 0 then RealIndex := 0
  else RealIndex := AIndex *2;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  
  if gtk_tree_model_get_n_columns(Widgets.TreeModel) div 2 < ALV.Columns.Count then begin
  
    ReCreateListStore(ALV, PTVWidgets(Widgets.WidgetInfo^.UserData));
    ReCreateItems(ALV);
    
    GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  end;

  column := gtk_tree_view_column_new();
  // add renderers
  pixrenderer := gtk_cell_renderer_pixbuf_new();
  textrenderer := gtk_cell_renderer_text_new();

  gtk_tree_view_column_pack_start(column, pixrenderer, FALSE);
  gtk_tree_view_column_set_attributes(column, pixrenderer,['pixbuf', RealIndex, nil]);
  gtk_tree_view_column_pack_start(column, textrenderer, True);
  gtk_tree_view_column_set_attributes(column, textrenderer, ['text',RealIndex+ 1, nil]);

  // set title
  gtk_tree_view_column_set_title(column, PChar(AColumn.Caption));
  //set width
  gtk_tree_view_column_set_fixed_width(Column, AColumn.Width);
  // set Visible
  gtk_tree_view_column_set_visible(Column, AColumn.Visible);
  // set MinWidth
  if AColumn.MinWidth > 0 then
    gtk_tree_view_column_set_min_width(Column, AColumn.MinWidth);
  // set MaxWidth
  if AColumn.MaxWidth > 0 then
    gtk_tree_view_column_set_max_width(Column, AColumn.MaxWidth);
  //set clickable
  gtk_tree_view_column_set_clickable (GTK_TREE_VIEW_COLUMN (column), TRUE);
  //set resizable
  gtk_tree_view_column_set_resizable(GTK_TREE_VIEW_COLUMN (column), True);
  // insert column
  gtk_tree_view_insert_column(GTK_TREE_VIEW(Widgets.MainView), Column, AIndex);
end;

procedure TGtk2WSCustomListView.ColumnMove(const ALV: TCustomListView;
  const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn);
var
Widgets: TTVWidgets;
Column: PGtkTreeViewColumn;
PrevColumn: PGtkTreeViewColumn;
begin

  if Not(ALV.HandleAllocated) then exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets do begin
    Column := gtk_tree_view_get_column(PGtkTreeView(MainView), AOldIndex);
    if ANewIndex = 0 then  PrevColumn := nil
    else PrevColumn := gtk_tree_view_get_column(PGtkTreeView(MainView), ANewIndex-1);
    gtk_tree_view_move_column_after(PGtkTreeView(MainView), Column, PrevColumn);
  end;
end;

procedure TGtk2WSCustomListView.ColumnSetAlignment(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn;
  const AAlignment: TAlignment);
var
Widgets: TTVWidgets;
GtkColumn: PGtkTreeViewColumn;
begin

  if Not(ALV.HandleAllocated) then exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets do begin
    GtkColumn := gtk_tree_view_get_column(PGtkTreeView(MainView), AIndex);
    gtk_tree_view_column_set_alignment(GtkColumn, AlignToGtkAlign(AAlignment));

  end;

end;

procedure TGtk2WSCustomListView.ColumnSetAutoSize(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean);
var
Widgets: TTVWidgets;
GtkColumn: PGtkTreeViewColumn;
ColSizing: TGtkTreeViewColumnSizing;
begin

  if Not(ALV.HandleAllocated) then exit;
  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets do begin
    GtkColumn := gtk_tree_view_get_column(PGtkTreeView(MainView), AIndex);
    if AAutoSize then
      ColSizing := GTK_TREE_VIEW_COLUMN_AUTOSIZE
    else
      ColSizing := GTK_TREE_VIEW_COLUMN_FIXED;

    gtk_tree_view_column_set_sizing(GtkColumn, ColSizing);
    gtk_tree_view_column_set_resizable(GTK_TREE_VIEW_COLUMN (GtkColumn), True);
  end;
  
end;

procedure TGtk2WSCustomListView.ColumnSetCaption(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const ACaption: String);
var
Widgets: TTVWidgets;
GtkColumn: PGtkTreeViewColumn;
begin

  if Not(ALV.HandleAllocated) then exit;
  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets do begin
    GtkColumn := gtk_tree_view_get_column(PGtkTreeView(MainView), AIndex);
    gtk_tree_view_column_set_title(GtkColumn, PChar(ACaption));
  end;
end;

procedure TGtk2WSCustomListView.ColumnSetImage(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer
  );
begin
  DebugLn('TODO: Gtk2. TGtk2WSCustomListView.ColumnSetImage');
end;

procedure TGtk2WSCustomListView.ColumnSetMaxWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer);
var
Widgets: TTVWidgets;
GtkColumn: PGtkTreeViewColumn;
begin

  if Not(ALV.HandleAllocated) then exit;
  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets do begin
    GtkColumn := gtk_tree_view_get_column(PGtkTreeView(MainView), AIndex);
    gtk_tree_view_column_set_max_width(GtkColumn, AMaxWidth - Ord(AMaxWidth=0));
  end;
end;

procedure TGtk2WSCustomListView.ColumnSetMinWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer);
var
Widgets: TTVWidgets;
GtkColumn: PGtkTreeViewColumn;
begin

  if Not(ALV.HandleAllocated) then exit;
  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets do begin
    GtkColumn := gtk_tree_view_get_column(PGtkTreeView(MainView), AIndex);
    gtk_tree_view_column_set_min_width(GtkColumn, AMinWidth - Ord(AMinWidth=0));
  end;
end;

procedure TGtk2WSCustomListView.ColumnSetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer);
var
Widgets: TTVWidgets;
GtkColumn: PGtkTreeViewColumn;
begin

  if Not(ALV.HandleAllocated) then exit;
  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets do begin
    GtkColumn := gtk_tree_view_get_column(PGtkTreeView(MainView), AIndex);
    gtk_tree_view_column_set_fixed_width(GtkColumn, AWidth + Ord(AWidth<1));
  end;
end;

procedure TGtk2WSCustomListView.ColumnSetVisible(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean);
var
Widgets: TTVWidgets;
GtkColumn: PGtkTreeViewColumn;
begin

  if Not(ALV.HandleAllocated) then exit;
  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets do begin
    GtkColumn := gtk_tree_view_get_column(PGtkTreeView(MainView), AIndex);
    gtk_tree_view_column_set_visible(GtkColumn, AVisible);
  end;
end;

procedure TGtk2WSCustomListView.ItemDelete(const ALV: TCustomListView;
  const AIndex: Integer);
var
Widgets: TTVWidgets;
Iter: TGtkTreeIter;
begin

  if not(ALV.HandleAllocated) then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  with Widgets do begin
    if gtk_tree_model_iter_nth_child(TreeModel, @Iter, nil, AIndex) then
    begin
      gtk_list_store_remove(TreeModel, @Iter);
    end;
  end;
end;

function TGtk2WSCustomListView.ItemGetState(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  var AIsSet: Boolean): Boolean;
var
Widgets: TTVWidgets;
Iter: TGtkTreeIter;
begin

  if not(ALV.HandleAllocated) then Exit;
  Result := False;
  AIsSet := False;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  with Widgets do begin
    if gtk_tree_model_iter_nth_child(TreeModel, @Iter, nil, AIndex) then
    begin
      case AState of
        lisCut,
        lisDropTarget:
        begin
          //TODO: do something with the rowcolor ?
        end;

        lisFocused:
        begin
          AIsSet := gtk_tree_selection_iter_is_selected(TreeSelection, @Iter);//gtk2 iter has no focus??
          Result := True;
        end;

        lisSelected:
        begin
          AIsSet := gtk_tree_selection_iter_is_selected(TreeSelection, @Iter);
          Result := True;
        end;
      end;
    end;
  end;
end;

procedure TGtk2WSCustomListView.ItemInsert(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem);
var
Widgets: TTVWidgets;
Iter: TGtkTreeIter;
//BitImage: TBitmap;
begin
  if not(ALV.HandleAllocated) then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);


  with Widgets do begin
    if AIndex = -1 then
      gtk_list_store_append(PGtkListStore(TreeModel), @Iter)
    else
      gtk_list_store_insert(PGtkListStore(TreeModel), @Iter, AIndex);

{
    //Icon
    if  (ALV.SmallImages <> nil)
    and (AItem.ImageIndex > -1)
    then
    begin
      BitImage := TBitmap.Create;
      ALV.SmallImages.GetBitmap(AItem.ImageIndex,BitImage);

      gtk_list_store_set(TreeModel, @Iter,
            [0 ,PGdkPixmap(PGDIObject(BitImage.handle)^.GDIBitmapObject), -1]);
    end;
}
  end;
end;

procedure TGtk2WSCustomListView.ItemSetImage(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const ASubIndex,
  AImageIndex: Integer);
var
Widgets: TTVWidgets;
Iter: TGtkTreeIter;
BitImage: TBitmap;
begin

  if not(ALV.HandleAllocated) then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);


  with Widgets do begin
    //Icon
    if gtk_tree_model_iter_nth_child(TreeModel, @Iter, nil, AIndex) then begin
      if  (ALV.SmallImages <> nil)
      and (AItem.ImageIndex > -1)
      then
      begin
        BitImage := TBitmap.Create;
        ALV.SmallImages.GetBitmap(AItem.ImageIndex,BitImage);

        gtk_list_store_set(TreeModel, @Iter,
              [0 ,PGdkPixmap(PGDIObject(BitImage.handle)^.GDIBitmapObject), -1]);
      end;
    end;
  end;
end;

procedure TGtk2WSCustomListView.ItemSetState(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  const AIsSet: Boolean);
var
Widgets: TTVWidgets;
Iter: TGtkTreeIter;
Path: PGtkTreePath;
begin

  if not(ALV.HandleAllocated) then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  with Widgets do begin
    if gtk_tree_model_iter_nth_child(TreeModel, @Iter, nil, AIndex) then
    begin
      case AState of
        lisCut,
        lisDropTarget:
        begin
          //TODO: do something with the rowcolor ?
        end;

        lisFocused:
        begin
          //gtk2 iter has no focus??
          Path := gtk_tree_path_new_from_string(PChar(IntToStr(AIndex)));

          if AIsSet then begin
            gtk_tree_view_set_cursor(PGtkTreeView(MainView), Path, nil, False);
          end
          else begin
            gtk_tree_view_set_cursor(PGtkTreeView(MainView), nil, nil, False);
          end;
          gtk_tree_path_free(Path);
        end;

        lisSelected:
        begin
          if AIsSet then begin
            if not gtk_tree_selection_iter_is_selected(TreeSelection, @Iter) then
              gtk_tree_selection_select_iter(TreeSelection, @Iter);
          end
          else begin
            if gtk_tree_selection_iter_is_selected(TreeSelection, @Iter) then
              gtk_tree_selection_unselect_iter(TreeSelection, @Iter);
          end;
        end;
      end;
    end;
  end;
end;

procedure TGtk2WSCustomListView.ItemSetText(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer;
  const AText: String);
var
Widgets: TTVWidgets;
Iter: TGtkTreeIter;
Str: String;
begin
  if not(ALV.HandleAllocated) then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets do begin
    gtk_tree_model_iter_nth_child(TreeModel, @Iter, nil, AIndex);
    if ASubIndex = 0 then
      Str := AItem.Caption
    else begin
      if AItem.Subitems.Count < ASubIndex then Exit;//
      Str := AItem.Subitems.Strings[ASubIndex-1];
    end;
    gtk_list_store_set(PGtkListStore(TreeModel), @Iter, [(ASubIndex*2)+1, PChar(Str), -1]);
  end;
end;

procedure TGtk2WSCustomListView.ItemShow(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean);
var
  Widgets: TTVWidgets;
  Path: PGtkTreePath;
  StrIndex: String;
begin
  //TODO check for partial visiblity. currently scrolls to the Item to make it fully visible
  StrIndex := IntToStr(AItem.Index);
  
  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  
  with Widgets do begin
    Path := gtk_tree_path_new_from_string(PChar(StrIndex));
    gtk_tree_view_scroll_to_cell(PGtkTreeView(MainView),Path,nil,false,0,0);
    gtk_tree_path_free(Path);
  end;

end;

function TGtk2WSCustomListView.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Widgets: PTVWidgets;
  ListView: TCustomListView;
  WidgetInfo: PWidgetInfo;
  //ScrollingData: PBaseScrollingWinControlData;
  //ListViewData: PCustomListViewData;
  //Allocation: TGTKAllocation;

  ScrollWidget: PGtkScrolledWindow;


  nColumns: Integer;
  GTypeArray: PGType;
  i: Integer;

begin
  ListView := TCustomListView(AWinControl as TCustomListView);

  Result := TGtkWSBaseScrollingWinControl.CreateHandle(AWinControl, AParams);
  if Result = 0 then Exit;
  
  ScrollWidget := PGtkScrolledWindow(Result);
  
  gtk_widget_unset_flags(ScrollWidget^.hscrollbar, GTK_CAN_FOCUS);
  gtk_widget_unset_flags(ScrollWidget^.vscrollbar, GTK_CAN_FOCUS);
  gtk_scrolled_window_set_policy(ScrollWidget, GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  
  gtk_widget_show(PGtkWidget(ScrollWidget));
  
  nColumns := (ListView.Columns.Count*2){+1};
  
  if nColumns = 0 then nColumns := 2; // Add One Column By Default

  Widgets := nil;

  New(Widgets);
  with Widgets^ do begin

    GetMem(GTypeArray, SizeOf(GType)*(nColumns+1));
    for i := 0 to nColumns div 2 do begin
      GTypeArray[i*2] := GDK_TYPE_PIXBUF;
      GTypeArray[(i*2)+1] := G_TYPE_STRING;
    end;
    GTypeArray[nColumns{+1}] := 0; // -1;
    
    TreeModel := gtk_list_store_newv(nColumns, GTypeArray);
    ReAllocMem(GTypeArray, 0);
  
    MainView:=gtk_tree_view_new_with_model(TreeModel);
    g_object_unref (G_OBJECT (TreeModel));

    TreeSelection := PGtkTreeSelection(gtk_tree_view_get_selection(PGtkTreeView(MainView)));

    gtk_container_add(GTK_CONTAINER(ScrollWidget),PGtkWidget(MainView));

    WidgetInfo := GetWidgetInfo(ScrollWidget);
    WidgetInfo^.CoreWidget := PGtkWidget(MainView);
    WidgetInfo^.UserData := Widgets;
    //SetMainWidget(ScrollWidget, MainView);
    //GetWidgetInfo(ScrollWidget, True)^.CoreWidget := PGtkWidget(MainView);

    gtk_widget_show_all(PGtkWidget(MainView));
  end;
  //SetCallbacks(PGtkWidget(ScrollWidget), Widgets^, WidgetInfo);
  
end;

procedure TGtk2WSCustomListView.BeginUpdate(const ALV: TCustomListView);
var
Widgets: TTVWidgets;
begin
  if not ALV.HandleAllocated then exit;
  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  g_object_ref(Widgets.TreeModel);

  gtk_tree_view_set_model(PGtkTreeView(Widgets.MainView), nil);

end;

procedure TGtk2WSCustomListView.EndUpdate(const ALV: TCustomListView);
var
Widgets: TTVWidgets;
begin

  if not ALV.HandleAllocated then exit;
  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  gtk_tree_view_set_model(PGtkTreeView(Widgets.MainView), Widgets.TreeModel);
  g_object_unref(Widgets.TreeModel);

end;

function TGtk2WSCustomListView.GetBoundingRect(const ALV: TCustomListView
  ): TRect;
begin
  DebugLn('TODO: TGtk2WSCustomListView.GetBoundingRect');
  Result:=Rect(0,0,0,0);
end;

function TGtk2WSCustomListView.GetDropTarget(const ALV: TCustomListView
  ): Integer;
var
Widgets: TTVWidgets;
begin
  if not ALV.HandleAllocated then exit;
  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  // TODO: implement
  Result := -1;
end;

function TGtk2WSCustomListView.GetFocused(const ALV: TCustomListView): Integer;
var
Widgets: TTVWidgets;
Path: PGtkTreePath;
Column: PGtkTreeViewColumn;
begin
  if not ALV.HandleAllocated then exit;
  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  with Widgets do begin
    gtk_tree_view_get_cursor(PGtkTreeView(MainView), Path, Column);
    if Path = nil then
      Result := -1
    else
      Result := StrToInt(PChar(Path));
  end;
end;

function TGtk2WSCustomListView.GetHoverTime(const ALV: TCustomListView
  ): Integer;
var
  Widgets: TTVWidgets;
begin
  if not WSCheckHandleAllocated(ALV, 'GetHoverTime')
  then Exit;

  DebugLn('TODO: TGtk2WSCustomListView.GetHoverTime');

  // TODO: implement
  Result := -1; // = default
  
  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets do begin

  end;

end;

function TGtk2WSCustomListView.GetSelCount(const ALV: TCustomListView
  ): Integer;
var
Widgets: TTVWidgets;
AList: PGList;
begin
  Result := 0;
  if not ALV.HandleAllocated then exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets do begin
    AList := gtk_tree_selection_get_selected_rows(TreeSelection, nil);
    Result := g_list_length(AList);
    g_list_free(AList);
  end;
end;

function TGtk2WSCustomListView.GetSelection(const ALV: TCustomListView
  ): Integer;
var
Widgets: TTVWidgets;
Iter: PGtkTreeIter;
Path: PGtkTreePath;
begin
  if not ALV.HandleAllocated then exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  with Widgets do begin
    gtk_tree_selection_get_selected(TreeSelection, nil, @Iter);
    Path := gtk_tree_model_get_path(TreeModel, @Iter);
    Result := StrToInt(PChar(Path));
    gtk_tree_path_free(Path);
  end;
end;

function TGtk2WSCustomListView.GetTopItem(const ALV: TCustomListView): Integer;
begin
//  Result:=inherited GetTopItem(ALV);
Result := -1;
end;

function TGtk2WSCustomListView.GetVisibleRowCount(const ALV: TCustomListView
  ): Integer;
begin
//  Result:=inherited GetVisibleRowCount(ALV);
Result := -1;
end;

procedure TGtk2WSCustomListView.SetAllocBy(const ALV: TCustomListView;
  const AValue: Integer);
begin
//  inherited SetAllocBy(ALV, AValue);
end;

procedure TGtk2WSCustomListView.SetDefaultItemHeight(
  const ALV: TCustomListView; const AValue: Integer);
begin
//  inherited SetDefaultItemHeight(ALV, AValue);
end;

procedure TGtk2WSCustomListView.SetHotTrackStyles(const ALV: TCustomListView;
  const AValue: TListHotTrackStyles);
begin
//  inherited SetHotTrackStyles(ALV, AValue);
end;

procedure TGtk2WSCustomListView.SetHoverTime(const ALV: TCustomListView;
  const AValue: Integer);
begin
//  inherited SetHoverTime(ALV, AValue);
end;

procedure TGtk2WSCustomListView.SetImageList(const ALV: TCustomListView;
  const AList: TListViewImageList; const AValue: TCustomImageList);
begin
//  inherited SetImageList(ALV, AList, AValue);
end;

procedure TGtk2WSCustomListView.SetProperty(const ALV: TCustomListView;
  const AProp: TListViewProperty; const AIsSet: Boolean);
var
  Widgets: TTVWidgets;
begin
  if not WSCheckHandleAllocated(ALV, 'SetProperty')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  SetPropertyInternal(ALV, @Widgets, AProp, AIsSet);
end;

procedure TGtk2WSCustomListView.SetProperties(const ALV: TCustomListView;
  const AProps: TListViewProperties);
var
  Widgets: TTVWidgets;
  Prop: TListViewProperty;
begin
  if not WSCheckHandleAllocated(ALV, 'SetProperties')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  for Prop := Low(Prop) to High(Prop) do
    SetPropertyInternal(ALV, @Widgets, Prop, Prop in AProps);
end;

procedure TGtk2WSCustomListView.SetScrollBars(const ALV: TCustomListView;
  const AValue: TScrollStyle);
var
  ScrollWidget: PGtkScrolledWindow;
  hPolicy, vPolicy: TGtkPolicyType;
begin
  if not WSCheckHandleAllocated(ALV, 'SetScrollBars')
  then Exit;

  ScrollWidget := PGtkScrolledWindow(ALV.Handle);

  case AValue of
    ssHorizontal, ssBoth: hPolicy := GTK_POLICY_ALWAYS;
    ssAutoHorizontal, ssAutoBoth: hPolicy := GTK_POLICY_AUTOMATIC;
  else
    hPolicy := GTK_POLICY_NEVER;
  end;

  case AValue of
    ssVertical, ssBoth: vPolicy := GTK_POLICY_ALWAYS;
    ssAutoVertical, ssAutoBoth: vPolicy := GTK_POLICY_AUTOMATIC;
  else
    vPolicy := GTK_POLICY_NEVER;
  end;

  gtk_scrolled_window_set_policy(ScrollWidget, hPolicy, vPolicy);
end;

procedure TGtk2WSCustomListView.SetScrolledLeft(const ALV: TCustomListView;
  const AValue: Integer);
var
  Widgets: TTVWidgets;
begin
  if not WSCheckHandleAllocated(ALV, 'SetProperties')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  with Widgets do begin
    gtk_tree_view_scroll_to_point(PGtkTreeView(MainView), AValue, -1);
  end;
end;

procedure TGtk2WSCustomListView.SetScrolledTop(const ALV: TCustomListView;
  const AValue: Integer);
var
  Widgets: TTVWidgets;
begin
  if not WSCheckHandleAllocated(ALV, 'SetProperties')
  then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  with Widgets do begin
    gtk_tree_view_scroll_to_point(PGtkTreeView(MainView), -1, AValue);
  end;
end;

procedure TGtk2WSCustomListView.SetSort(const ALV: TCustomListView;
  const AType: TSortType; const AColumn: Integer);
begin
//  inherited SetSort(ALV, AType, AColumn);
end;

procedure TGtk2WSCustomListView.SetViewStyle(const ALV: TCustomListView;
  const Avalue: TViewStyle);
var
Widgets: TTVWidgets;
begin
  if not ALV.HandleAllocated then exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets do begin
    case AValue of
      vsList: gtk_tree_view_set_headers_visible(GTK_TREE_VIEW (MainView), False);
      vsReport: if ALV.ShowColumnHeaders = True then gtk_tree_view_set_headers_visible(GTK_TREE_VIEW (MainView), True);
    end;
  end;
//  inherited SetViewStyle(ALV, Avalue);
// this one is going to be fun because you have to free the GtkTreeView and Create GtkIconView etc depending on the new style
end;

{procedure TGtk2WSCustomListView.UpdateProperties(
  const ACustomListView: TCustomListView);
var
  Widgets: TTVWidgets;
  GtkColumn: PGtkTreeViewColumn;
  Column: TListColumn;
  Iter: TGtkTreeIter;
  Item: TListItem;
  X, Y: Integer;
  Count: Integer;
  BitImage: TBitmap;
begin
  if Not(ACustomListView.HandleAllocated) then exit;

  GetCommonTreeViewWidgets(PGtkWidget(ACustomListView.Handle), Widgets);
  with Widgets do begin
    // set up columns..
  for X := 0 to ACustomListView.Columns.Count-1 do begin;
    GtkColumn := gtk_tree_view_get_column(TreeView, X);
    Column := ACustomListView.Columns.Items[X];
    // set captions
    gtk_tree_view_column_set_title(GtkColumn, PChar(Column.Caption));
    // set column alignment
    gtk_tree_view_column_set_alignment(GtkColumn, AlignToGtkAlign(Column.Alignment));
    // set auto sizing
    case Column.AutoSize of
      // The gtk2 docs say that GTK_TREE_VIEW_COLUMN_AUTOSIZE is inefficient
      // for large views, so perhaps this should be
      // GTK_TREE_VIEW_COLUMN_GROW_ONLY
      True : gtk_tree_view_column_set_sizing(GtkColumn, GTK_TREE_VIEW_COLUMN_AUTOSIZE);
      //True : gtk_tree_view_column_set_sizing(GtkColumn, GTK_TREE_VIEW_COLUMN_GROW_ONLY);
      False: gtk_tree_view_column_set_sizing(GtkColumn, GTK_TREE_VIEW_COLUMN_FIXED);
    end;

    // set width
    gtk_tree_view_column_set_fixed_width(GtkColumn, Column.Width+Ord(Column.Width=0));
    // set Visible
    gtk_tree_view_column_set_visible(GtkColumn, Column.Visible);
    // set MinWidth
    gtk_tree_view_column_set_min_width(GtkColumn,
                                       Column.MinWidth-Ord(Column.MinWidth=0));
    // set MaxWidth
    gtk_tree_view_column_set_max_width(GtkColumn,
                                       Column.MaxWidth-Ord(Column.MaxWidth=0));
  end;

  // ViewStyle
  case ACustomListView.ViewStyle of
    vsReport:
      gtk_tree_view_set_headers_visible(TreeView, True);
    vsList:
      _set_headers_visible(TreeView, False);
  end;

  //sorting
  //TODO

  //multiselect
  case ACustomListView.MultiSelect of
    True : gtk_tree_selection_set_mode(TreeSelection, GTK_SELECTION_MULTIPLE);
    False: gtk_tree_selection_set_mode(TreeSelection, GTK_SELECTION_SINGLE);
    //GTK_SELECTION_NONE,
    //GTK_SELECTION_SINGLE,
    //GTK_SELECTION_BROWSE,
    //GTK_SELECTION_MULTIPLE
  end;

  //do items...

  for X := 0 to ACustomListView.Items.Count-1 do begin
    Item:= ACustomListView.Items.Item[X];
    if X = 0 then
      gtk_tree_model_get_iter_first(TreeModel, @Iter)
    else
      gtk_tree_model_iter_next(TreeModel, @Iter);

    //do image if one is assigned....
    if  (ACustomListView.SmallImages <> nil)
    and (Item.ImageIndex > -1)
    then
    begin
      BitImage := TBitmap.Create;
      ACustomListView.SmallImages.GetBitmap(Item.ImageIndex,BitImage);
      // this doesnt seem to be working :(
      gtk_list_store_set(TreeModel, @Iter,
            [0 ,PGdkPixmap(PGDIObject(BitImage.handle)^.GDIBitmapObject), -1]);
    end;

    //Item.Caption
    gtk_list_store_set(TreeModel, @Iter, [1, PChar(Item.Caption), -1]);
    Count := ACustomListView.Columns.Count;

    //Item.Subitems
    for Y := 2 to Count-1 do begin
      if (Y-2) >= Item.SubItems.Count then Break;
      gtk_list_store_set(TreeModel, @Iter, [Y, PChar(Item.SubItems.Strings[Y-2]), -1]);
    end;
  end;
  end;
end;}

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TCustomStatusBar, TGtk2WSStatusBar);
//  RegisterWSComponent(TCustomTabSheet, TGtk2WSTabSheet);
//  RegisterWSComponent(TCustomPageControl, TGtk2WSPageControl);
  RegisterWSComponent(TCustomListView, TGtk2WSCustomListView);
//  RegisterWSComponent(TCustomListView, TGtk2WSListView);
//  RegisterWSComponent(TCustomProgressBar, TGtk2WSProgressBar);
//  RegisterWSComponent(TCustomUpDown, TGtk2WSCustomUpDown);
//  RegisterWSComponent(TCustomUpDown, TGtk2WSUpDown);
//  RegisterWSComponent(TCustomToolButton, TGtk2WSToolButton);
//  RegisterWSComponent(TCustomToolBar, TGtk2WSToolBar);
//  RegisterWSComponent(TCustomToolButton, TGtk2WSToolButton);
//  RegisterWSComponent(TCustomToolBar, TGtk2WSToolBar);
//  RegisterWSComponent(TCustomTrackBar, TGtk2WSTrackBar);
//  RegisterWSComponent(TCustomTreeView, TGtk2WSCustomTreeView);
//  RegisterWSComponent(TCustomTreeView, TGtk2WSTreeView);
////////////////////////////////////////////////////
end.
