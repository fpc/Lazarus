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
  GLib2, Gtk2, Gdk2,
  // LCL
  ComCtrls, Classes, LCLType, LMessages, Controls, Graphics,
  LCLProc,
  // widgetset
  WSComCtrls, GtkWSComCtrls, WSLCLClasses, WSProc,
  // interface
  GtkDef, GtkProc;
  
type

  // For simplified manipulation
  // Use GetCommonTreeViewWidgets(PGtkTreeView, var TTVWidgets)
  TTVWidgets = record
    TreeView: PGtkTreeView;
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

  TGtk2WSCustomListView = class(TGtkWSCustomListView)
  private
  protected
  public
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

    class procedure ItemDelete(const ALV: TCustomListView; const AIndex: Integer); override;
    class function  ItemGetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; var AIsSet: Boolean): Boolean; override; // returns True if supported
    class procedure ItemInsert(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); override;
    class procedure ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AImageIndex: Integer); override;
    class procedure ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); override;
    class procedure ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer; const AText: String); override;
    class procedure ItemShow(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean); override;

    class procedure UpdateProperties(const ACustomListView: TCustomListView); override;
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


   // Will be used commonly for ListViews and TreeViews
   procedure GetCommonTreeViewWidgets(ATreeViewHandle: PGtkWidget; var TVWidgets: TTVWidgets);
   begin
     with TVWidgets do begin
       TreeView := PGtkTreeView(GetWidgetInfo(Pointer(ATreeViewHandle))^.CoreWidget);
       TreeSelection := gtk_tree_view_get_selection(TreeView);
       TreeModel := gtk_tree_view_get_model(TreeView);
     end;
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

procedure TGtk2WSCustomListView.ColumnDelete(const ALV: TCustomListView;
  const AIndex: Integer);
var
Widgets: TTVWidgets;
GtkColumn: PGtkTreeViewColumn;
begin
  if Not(ALV.HandleAllocated) then exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets do begin
    GtkColumn := gtk_tree_view_get_column(TreeView, AIndex);
    gtk_tree_view_remove_column(TreeView, GtkColumn);
  end;
end;

function TGtk2WSCustomListView.ColumnGetWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn): Integer;
var
Widgets: TTVWidgets;
GtkColumn: PGtkTreeViewColumn;
begin
  if Not(ALV.HandleAllocated) then exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets do begin
    GtkColumn := gtk_tree_view_get_column(TreeView, AColumn.Index);
    Result := gtk_tree_view_column_get_width(GtkColumn);
  end;
end;

procedure TGtk2WSCustomListView.ColumnInsert(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn);
var
Widgets: TTVWidgets;
GtkColumn: PGtkTreeViewColumn;
renderer: PGtkCellRenderer;
begin
  if Not(ALV.HandleAllocated) then exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  
  GtkColumn := gtk_tree_view_column_new;
  gtk_tree_view_column_set_title(GtkColumn, PChar(AColumn.Caption));
  
  if AIndex = 0 then begin
    //Add an Icon renderer and a text renderer to the first column
    //for Icons
    renderer := gtk_cell_renderer_pixbuf_new();
    gtk_tree_view_column_pack_start(GtkColumn, renderer, FALSE);
    gtk_tree_view_column_set_attributes(GtkColumn, renderer,['pixbuf', 0, nil]);

    renderer := gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(GtkColumn, renderer, True);
    gtk_tree_view_column_set_attributes(GtkColumn, renderer, ['text', 1, nil]);

    gtk_tree_view_append_column(GTK_TREE_VIEW(Widgets.Treeview), GtkColumn);
    gtk_tree_view_column_set_clickable (GTK_TREE_VIEW_COLUMN (GtkColumn), TRUE);

  end
  else begin
    renderer := gtk_cell_renderer_text_new();
    gtk_tree_view_column_set_attributes(GtkColumn, renderer, ['text', 0, nil]);
    gtk_tree_view_insert_column(Widgets.TreeView, GtkColumn, AIndex);
  end;
end;

procedure TGtk2WSCustomListView.ColumnMove(const ALV: TCustomListView;
  const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn);
begin
  inherited ColumnMove(ALV, AOldIndex, ANewIndex, AColumn);
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
    GtkColumn := gtk_tree_view_get_column(TreeView, AIndex);
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
    GtkColumn := gtk_tree_view_get_column(TreeView, AIndex);
    if AAutoSize then ColSizing := GTK_TREE_VIEW_COLUMN_AUTOSIZE
    else ColSizing := GTK_TREE_VIEW_COLUMN_FIXED;
    gtk_tree_view_column_set_sizing(GtkColumn, ColSizing);
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
    GtkColumn := gtk_tree_view_get_column(TreeView, AIndex);
    gtk_tree_view_column_set_title(GtkColumn, PChar(ACaption));
  end;
end;

procedure TGtk2WSCustomListView.ColumnSetImage(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer
  );
begin
  inherited ColumnSetImage(ALV, AIndex, AColumn, AImageIndex);
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
    GtkColumn := gtk_tree_view_get_column(TreeView, AIndex);
    GtkColumn^.max_width := AMaxWidth;
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
    GtkColumn := gtk_tree_view_get_column(TreeView, AIndex);
    GtkColumn^.min_width := AMinWidth;
  end;
end;

procedure TGtk2WSCustomListView.ColumnSetWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer);
var
Widgets: TTVWidgets;
GtkColumn: PGtkTreeViewColumn;
begin
  if Not(ALV.HandleAllocated) then exit;
  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);
  with Widgets do begin
    GtkColumn := gtk_tree_view_get_column(TreeView, AIndex);
    GtkColumn^.width := AWidth;
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
    GtkColumn := gtk_tree_view_get_column(TreeView, AIndex);
    gtk_tree_view_column_set_visible(GtkColumn, AVisible);
  end;
end;

procedure TGtk2WSCustomListView.ItemDelete(const ALV: TCustomListView;
  const AIndex: Integer);
var
Widgets: TTVWidgets;
Iter: TGtkTreeIter;
begin
  //if not(ALV.HandleAllocated) then Exit;

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
begin
  Result:=inherited ItemGetState(ALV, AIndex, AItem, AState, AIsSet);
end;

procedure TGtk2WSCustomListView.ItemInsert(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem);
var
Widgets: TTVWidgets;
Iter: TGtkTreeIter;
ColCount: Integer;
X: Integer;
BitImage: TBitmap;
begin
  //if not(ALV.HandleAllocated) then Exit;

  GetCommonTreeViewWidgets(PGtkWidget(ALV.Handle), Widgets);

  with Widgets do begin
    gtk_list_store_insert(PGtkListStore(TreeModel), @Iter, AIndex);
    
    //Icon
    if  (ALV.SmallImages <> nil)
    and (AItem.ImageIndex > -1)
    then
    begin
      BitImage := TBitmap.Create;
      ALV.SmallImages.GetBitmap(AItem.ImageIndex,BitImage);

      gtk_tree_store_set(TreeModel, @Iter,
            [0 ,PGdkPixmap(PGDIObject(BitImage.handle)^.GDIBitmapObject), nil]);
    end;
  
    //Caption
    gtk_list_store_set(PGtkListStore(TreeModel), @Iter, [1, PChar(AItem.Caption), -1]);
    ColCount := ALV.Columns.Count;
    if AItem.SubItems.Count > 0 then begin
    
    //SubItems
    for X := 2 to ColCount-1 do begin
      if (X-2) >= AItem.SubItems.Count then Break;
      gtk_tree_store_set(TreeModel, @Iter, [X, PChar(AItem.SubItems.Strings[X-2]), nil]);
    end;
    
    {
      if ColCount > (1 + AItem.SubItems.Count) then
        ColCount := 1 + AItem.SubItems.Count;
      if ColCount < 0 then ColCount := 1;
      for x := 1 to ColCount do begin
        gtk_list_store_set(TreeModel, @Iter, [X+1, PChar(AItem.SubItems.Strings[X-1]), -1]);
      end;
    }
    end;
  end;
end;

procedure TGtk2WSCustomListView.ItemSetImage(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const ASubIndex,
  AImageIndex: Integer);
begin
  inherited ItemSetImage(ALV, AIndex, AItem, ASubIndex, AImageIndex);
end;

procedure TGtk2WSCustomListView.ItemSetState(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  const AIsSet: Boolean);
begin
  inherited ItemSetState(ALV, AIndex, AItem, AState, AIsSet);
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
    else
      Str := AItem.Subitems.Strings[ASubIndex-1];
    
    gtk_list_store_set(PGtkListStore(TreeModel), @Iter, [ASubIndex+1, PChar(Str), -1]);

  end;
end;

procedure TGtk2WSCustomListView.ItemShow(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean);
begin
  inherited ItemShow(ALV, AIndex, AItem, PartialOK);
end;

procedure TGtk2WSCustomListView.UpdateProperties(
  const ACustomListView: TCustomListView);
var
Widgets: TTVWidgets;
GtkColumn: PGtkTreeViewColumn;
Column: TListColumn;
GtkAlign: gfloat;
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
      True : gtk_tree_view_column_set_sizing(GtkColumn, GTK_TREE_VIEW_COLUMN_AUTOSIZE);
      False: gtk_tree_view_column_set_sizing(GtkColumn, GTK_TREE_VIEW_COLUMN_FIXED);
    end;
    // set width
    GtkColumn^.width := Column.Width;
    // set Visible
    gtk_tree_view_column_set_visible(GtkColumn, Column.Visible);
    // set MinWidth
    gtk_tree_view_column_set_min_width(GtkColumn, Column.MinWidth);
    // set MaxWidth
    gtk_tree_view_column_set_max_width(GtkColumn, Column.MaxWidth);
  end;
  
  // ViewStyle
  case ACustomListView.ViewStyle of
    vsReport:
    begin
      gtk_tree_view_set_headers_visible(TreeView, True);
    end;
    vsList:
    begin
      gtk_tree_view_set_headers_visible(TreeView, False);
    end;
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

      gtk_tree_store_set(TreeModel, @Iter,
            [0 ,PGdkPixmap(PGDIObject(BitImage.handle)^.GDIBitmapObject), nil]);
    end;
    
    //Item.Caption
    gtk_tree_store_set(TreeModel, @Iter, [1, PChar(Item.Caption), nil]);
    Count := ACustomListView.Columns.Count;

    //Item.Subitems
    for Y := 2 to Count-1 do begin
      if (Y-2) >= Item.SubItems.Count then Break;
      gtk_tree_store_set(TreeModel, @Iter, [Y, PChar(Item.SubItems.Strings[Y-2]), nil]);
    end;
  end;
  end;
end;

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
