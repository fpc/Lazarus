{ $Id$}
{
 *****************************************************************************
 *                             GtkWSComCtrls.pp                              *
 *                             ----------------                              *
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
unit GtkWSComCtrls;

{$mode objfpc}{$H+}

interface

uses
  // libs
  {$IFDEF GTK2}
  GLib2, Gtk2, Gdk2,
  {$ELSE}
  GLib, Gtk, Gdk,
  {$ENDIF}
  // LCL
  ComCtrls, Classes, LCLType, LMessages, Controls, Graphics,
  LCLProc,
  // widgetset
  WSComCtrls, WSLCLClasses, WSProc,
  // interface
  GtkDef;

type

  { TGtkWSStatusBar }

  TGtkWSStatusBar = class(TWSStatusBar)
  private
  protected
  public
  end;

  { TGtkWSTabSheet }

  TGtkWSTabSheet = class(TWSTabSheet)
  private
  protected
  public
  end;

  { TGtkWSPageControl }

  TGtkWSPageControl = class(TWSPageControl)
  private
  protected
  public
  end;

  { TGtkWSCustomListView }

  TGtkWSCustomListView = class(TWSCustomListView)
{$IFDEF GTK1}
  private
    class procedure ItemChangeInternal(const ACListWidget: PGtkCList; const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem);
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
{$ENDIF}
  end;

  { TGtkWSListView }

  TGtkWSListView = class(TWSListView)
  private
  protected
  public
  end;

  { TGtkWSProgressBar }

  TGtkWSProgressBar = class(TWSProgressBar)
  private
  protected
  public
  end;

  { TGtkWSCustomUpDown }

  TGtkWSCustomUpDown = class(TWSCustomUpDown)
  private
  protected
  public
  end;

  { TGtkWSUpDown }

  TGtkWSUpDown = class(TWSUpDown)
  private
  protected
  public
  end;

  { TGtkWSToolButton }

  TGtkWSToolButton = class(TWSToolButton)
  private
  protected
  public
  end;

  { TGtkWSToolBar }

  TGtkWSToolBar = class(TWSToolBar)
  private
  protected
  public
  end;

  { TGtkWSTrackBar }

  TGtkWSTrackBar = class(TWSTrackBar)
  private
  protected
  public
  end;

  { TGtkWSCustomTreeView }

  TGtkWSCustomTreeView = class(TWSCustomTreeView)
  private
  protected
  public
  end;

  { TGtkWSTreeView }

  TGtkWSTreeView = class(TWSTreeView)
  private
  protected
  public
  end;


implementation

uses
  SysUtils,
  GtkProc, GtkInt, GtkGlobals,
  GtkWSControls;

const
  DEFAULT_IMAGE_SPACING = 3;


  { TGtkWSCustomListView }

type
  TLVHack = class(TCustomListView)
  {$IFDEF VER1_0}
  protected
    property MultiSelect;
    property SmallImages;
  {$ENDIF}
  end;

{$IFDEF GTK1}
procedure TGtkWSCustomListView.ColumnDelete(const ALV: TCustomListView; const AIndex: Integer); 
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnDelete')
  then Exit;
  
  TLVHack(ALV).RecreateWnd;
end;

function TGtkWSCustomListView.ColumnGetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn): Integer;
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
  CListColumn: PGtkCListColumn;
begin
  Result := -1;

  if not WSCheckHandleAllocated(ALV, 'ColumnGetSize')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  // there is no get width function, so we need some internal hacking
  if AIndex >= CListWidget^.columns then Exit;
  CListColumn := CListWidget^.Column;
  Inc(CListColumn, AIndex);
  Result := CListColumn^.width;
end;

procedure TGtkWSCustomListView.ColumnInsert(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn); 
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnInsert')
  then Exit;

  TLVHack(ALV).RecreateWnd;
end;

procedure TGtkWSCustomListView.ColumnMove(const ALV: TCustomListView; const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn); 
  procedure CopyColumn(const AList: PGtkCList; const AIndex: Integer; const ASrc: PGtkCListColumn);
  begin                                      
    gtk_clist_set_column_title(AList, AIndex, ASrc^.title);
    gtk_clist_set_column_min_width(AList, AIndex, ASrc^.min_width);
    gtk_clist_set_column_max_width(AList, AIndex, ASrc^.max_width);
    gtk_clist_set_column_width(AList, AIndex, ASrc^.width);
    gtk_clist_set_column_justification(AList, AIndex, ASrc^.justification);
    gtk_clist_set_column_visibility(AList, AIndex, (ASrc^.flag0 and bm_TGtkCListColumn_visible) <> 0);
    gtk_clist_set_column_resizeable(AList, AIndex, (ASrc^.flag0 and bm_TGtkCListColumn_resizeable) <> 0);
    gtk_clist_set_column_auto_resize(AList, AIndex, (ASrc^.flag0 and bm_TGtkCListColumn_auto_resize) <> 0);
    if (ASrc^.flag0 and bm_TGtkCListColumn_button_passive) <> 0
    then gtk_clist_column_title_passive(AList, AIndex)
    else gtk_clist_column_title_active(AList, AIndex);
  end;
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
  CListColumn: PGtkCListColumn;
  OldCListColumn: TGtkCListColumn;
  Count: Integer;      
  
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnMove')
  then Exit;

  if AOldIndex = ANewIndex then Exit;
  if AOldIndex < 0 then Exit;
  if ANewIndex < 0 then Exit;
  
  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  
  if AOldIndex >= CListWidget^.columns then Exit;
  if ANewIndex >= CListWidget^.columns then Exit;
  
  Count := AOldIndex - ANewIndex;
  
  // Fetch old column values
  CListColumn := CListWidget^.Column;
  Inc(CListColumn, AOldIndex);
  OldCListColumn := CListColumn^; 
  // Create copy of the title
  OldCListColumn.title := StrNew(OldCListColumn.title);

  while Count <> 0 do
  begin
    // move to next source
    if Count < 0
    then Inc(CListColumn)
    else Dec(CListColumn);

    CopyColumn(CListWidget, ANewIndex + Count, CListColumn);

    if Count < 0
    then Inc(Count)
    else Dec(Count);
  end;
  // finally copy original data to new column
  CopyColumn(CListWidget, ANewIndex, @OldCListColumn);
  // dispose copy of the title
  StrDispose(OldCListColumn.title);
end;

procedure TGtkWSCustomListView.ColumnSetAlignment(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAlignment: TAlignment);
const
  JUSTIFICATION: array[TAlignment] of TGtkJustification = (
    GTK_JUSTIFY_LEFT,
    GTK_JUSTIFY_RIGHT,
    GTK_JUSTIFY_CENTER
  );
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetAlignment')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  gtk_clist_set_column_justification(CListWidget, AIndex, JUSTIFICATION[AAlignment]);
end;

procedure TGtkWSCustomListView.ColumnSetAutoSize(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetAutoSize')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  gtk_clist_set_column_auto_resize(CListWidget, AIndex, AAutoSize);
end;

procedure TGtkWSCustomListView.ColumnSetCaption(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const ACaption: String);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetCaption')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  gtk_clist_set_column_title(CListWidget, AIndex, PChar(ACaption));
end;

procedure TGtkWSCustomListView.ColumnSetImage(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetImage')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  //TODO
  if CListWidget=nil then exit;
end;

procedure TGtkWSCustomListView.ColumnSetMaxWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetMaxWidth')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  gtk_clist_set_column_max_width(CListWidget, AIndex, AMaxWidth);
end;

procedure TGtkWSCustomListView.ColumnSetMinWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetMinWidth')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  gtk_clist_set_column_min_width(CListWidget, AIndex, AMinWidth);
end;

procedure TGtkWSCustomListView.ColumnSetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetWidth')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  gtk_clist_set_column_width(CListWidget, AIndex, AWidth);
end;

procedure TGtkWSCustomListView.ColumnSetVisible(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetVisible')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  gtk_clist_set_column_visibility(CListWidget, AIndex, AVisible);
end;

procedure TGtkWSCustomListView.ItemChangeInternal(const ACListWidget: PGtkCList; const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem);
var
  ImageBitmap, MaskBitmap: TBitmap;
  ImageRect: TRect;
  Pixmap: PGdkPixmap;
  Mask: PGdkBitmap;
  n, Count: integer;
begin
  if  (TLVHack(ALV).SmallImages <> nil)
  and (AItem.ImageIndex >= 0)
  and (AItem.ImageIndex < TLVHack(ALV).SmallImages.Count)
  then begin
    // set image & caption
    TLVHack(ALV).SmallImages.GetInternalImage(AItem.ImageIndex, ImageBitmap, MaskBitmap, ImageRect);
    if (ImageRect.Left <> 0)
    or (ImageRect.Top <> 0)
    then DebugLn('WARNING: TGtkWSCustomListView.ItemChangeInternal does not support combined imagelists');
    Pixmap := PGDIObject(ImageBitmap.Handle)^.GDIPixmapObject;
    Mask := PGdkBitmap(PGDIObject(ImageBitmap.Handle)^.GDIBitmapMaskObject);
    gtk_clist_set_pixtext(ACListWidget, AIndex, 0, PChar(AItem.Caption), 3, Pixmap, Mask);
  end
  else begin
    // set caption alone
    gtk_clist_set_text(ACListWidget, AIndex, 0, PChar(AItem.Caption));
  end;

  // set the other column texts
  Count := AItem.SubItems.Count + 1;
  if Count > ACListWidget^.Columns
  then Count := ACListWidget^.Columns;
  // set the existing subitems
  for n := 1 to Count - 1 do
    gtk_clist_set_text(ACListWidget, AIndex, n, PChar(AItem.SubItems[n - 1]));
  // fill remaining
  for n := Count to ACListWidget^.Columns - 1 do
    gtk_clist_set_text(ACListWidget, AIndex, n, #0);
end;

procedure TGtkWSCustomListView.ItemDelete(const ALV: TCustomListView; const AIndex: Integer);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemDelete')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  gtk_clist_remove(CListWidget, AIndex);
end;

function TGtkWSCustomListView.ItemGetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; var AIsSet: Boolean): Boolean; 
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  Result := False;
  
  if not WSCheckHandleAllocated(ALV, 'ItemGetState')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  if (AIndex < 0) or (AIndex >= CListWidget^.rows) 
  then begin
    DebugLN('[TGtkWSCustomListView.ItemGetState] Invalid row index: %d', [Aindex]);
    Exit;
  end;   
  
  case AState of
    lisCut, 
    lisDropTarget: begin
      //TODO: do something with the rowcolor ?
    end;

    lisFocused: begin
      AIsSet := CListWidget^.focus_row = AIndex;
      Result := True;
    end;

    lisSelected: begin 
      AIsSet := (CListWidget^.selection <> nil)
            and (g_list_find(CListWidget^.selection, Pointer(Aindex)) <> nil);
      Result := True;
    end;
  end;
  
end;
  
procedure TGtkWSCustomListView.ItemInsert(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
  Titles: PPGChar;
  idx, Count: Integer;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemInsert')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  Count := CListWidget^.columns;

  if Count = 0
  then begin
    DebugLn('WARNING: TGtkWSCustomListView.ItemInsert  CListWidget^.columns = 0');
    Exit;
  end;

  GetMem(Titles, SizeOf(PGChar) * Count);
  FillChar(Titles^, SizeOf(PGChar) * Count, 0);
  Titles[0] := #0;

  idx := AIndex;
  if idx = -1
  then idx := gtk_clist_append(CListWidget, Titles)
  else gtk_clist_insert(CListWidget, idx, Titles);
  FreeMem(Titles);

  ItemChangeInternal(CListWidget, ALV, idx, AItem);
end;

procedure TGtkWSCustomListView.ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AImageIndex: Integer);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
  ImageBitmap, MaskBitmap: TBitmap;
  ImageRect: TRect;
  Pixmap: PGdkPixmap;
  Mask: PGdkBitmap;
  Spacing: guint8;
  Text: PChar;
  Dummy1, Dummy2: Pointer;
  CellType: TGtkCellType;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetImage')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  if  (TLVHack(ALV).SmallImages <> nil)
  and (AImageIndex >= 0)
  and (AImageIndex < TLVHack(ALV).SmallImages.Count)
  then begin
    // set image & caption
    TLVHack(ALV).SmallImages.GetInternalImage(AImageIndex, ImageBitmap, MaskBitmap, ImageRect);
    if (ImageRect.Left <> 0)
    or (ImageRect.Top <> 0)
    then DebugLn('WARNING: TGtkWSCustomListView.ItemSetImage does not support combined imagelists');
    Pixmap := PGDIObject(ImageBitmap.Handle)^.GDIPixmapObject;
    Mask := PGdkBitmap(PGDIObject(ImageBitmap.Handle)^.GDIBitmapMaskObject);
  end
  else begin
    Pixmap := nil;
    Mask := nil;
  end;

  CellType := gtk_clist_get_cell_type(CListWidget, AIndex, ASubIndex);
  // Sigh. 
  // gtk returns -1 for an invalid cell (which is not part of the enum)
  // so to handle it, we need a case based on integer
  case Ord(CellType) of
    Ord(GTK_CELL_TEXT),
    Ord(GTK_CELL_PIXTEXT),
    Ord(GTK_CELL_EMPTY),
    Ord(GTK_CELL_PIXMAP): begin
      if pixmap <> nil
      then begin
        case CellType of
          GTK_CELL_TEXT: begin
            // convert the cell
            Text := nil;
            gtk_clist_get_text(CListWidget, AIndex, ASubIndex, @Text);
            gtk_clist_set_pixtext(CListWidget, AIndex, ASubIndex, Text, DEFAULT_IMAGE_SPACING, Pixmap, Mask);
          end;
          GTK_CELL_PIXTEXT: begin
            if gtk_clist_get_pixtext(CListWidget, AIndex, ASubIndex, @Text, @Spacing, @Dummy2, @Dummy1) <> 0
            then gtk_clist_set_pixtext(CListWidget, AIndex, ASubIndex, Text, Spacing, Pixmap, Mask)
            else gtk_clist_set_pixmap(CListWidget, AIndex, ASubIndex, Pixmap, Mask);
          end;
          GTK_CELL_EMPTY,
          GTK_CELL_PIXMAP: begin
            gtk_clist_set_pixtext(CListWidget, AIndex, ASubIndex, '', DEFAULT_IMAGE_SPACING, Pixmap, Mask);
          end;
        end;
      end
      else begin
        case CellType of
          GTK_CELL_EMPTY,
          GTK_CELL_TEXT:; // nothing to do
          GTK_CELL_PIXTEXT: begin
            Text := nil;
            if gtk_clist_get_pixtext(CListWidget, AIndex, ASubIndex, @Text, @Spacing, @Dummy2, @Dummy1) <> 0
            then gtk_clist_set_text(CListWidget, AIndex, ASubIndex, Text)
            else gtk_clist_set_text(CListWidget, AIndex, ASubIndex, '');
          end;
          GTK_CELL_PIXMAP: begin
            gtk_clist_set_text(CListWidget, AIndex, ASubIndex, '');
          end;
        end;
      end;
    end;
    Ord(GTK_CELL_WIDGET): DebugLN('[TGtkWSCustomListView.ItemSetImage] Setting text of widget cell');
    -1: DebugLN('[TGtkWSCustomListView.ItemSetText] Cell (%d,%d) not created', [AIndex, ASubIndex]);
  else
    DebugLN('[TGtkWSCustomListView.ItemSetImage] Unknown celltype %d', [Ord(CellType)]);
  end;
end;

procedure TGtkWSCustomListView.ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); 
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetState')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  if (AIndex < 0) or (AIndex >= CListWidget^.rows) 
  then begin
    DebugLN('[TGtkWSCustomListView.ItemSetState] Invalid row index: %d', [Aindex]);
    Exit;
  end;   
  
  case AState of
    lisCut, 
    lisDropTarget: begin
      //TODO: do something with the rowcolor ?
    end;

    lisFocused: begin
      if AIsSet = (CListWidget^.focus_row = AIndex) then Exit;
      // reset old focus
      if (CListWidget^.focus_row <> -1)
      and (gtk_widget_has_focus(PGtkWidget(CListWidget)))
      then gtk_widget_draw_focus(PGtkWidget(CListWidget));
      
      if AIsSet
      then begin
        CListWidget^.focus_row := AIndex;
        if gtk_widget_has_focus(PGtkWidget(CListWidget))
        then gtk_widget_draw_focus(PGtkWidget(CListWidget));
      end
      else CListWidget^.focus_row := -1; 
    end;

    lisSelected: begin
      if AIsSet
      then begin
        if (CListWidget^.selection_mode = GTK_SELECTION_SINGLE)
        or (CListWidget^.selection_mode = GTK_SELECTION_BROWSE)
        then begin
          // check if the row is are already selected
          // since we are in singleselect, the first item is checked
          if (CListWidget^.selection <> nil)
          and (Integer(CListWidget^.selection^.Data) = AIndex)
          then Exit;
          gtk_clist_unselect_all(CListWidget);
        end;
        gtk_clist_select_row(CListWidget, AIndex, 0);
      end
      else gtk_clist_unselect_row(CListWidget, AIndex, 0);
    end;
  end;
end;

procedure TGtkWSCustomListView.ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer; const AText: String);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
  Pixmap: PGdkPixmap;
  Mask: PGdkBitmap;
  Spacing: guint8;
  Dummy: Pointer;
  CellType: TGtkCellType;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetText')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  CellType := gtk_clist_get_cell_type(CListWidget, AIndex, ASubIndex);
  // Sigh. 
  // gtk returns -1 for an invalid cell (which is not part of the enum)
  // so to handle it, we need a case based on integer
  case Ord(CellType) of
    Ord(GTK_CELL_EMPTY),
    Ord(GTK_CELL_TEXT): begin
      // simply set the text
      gtk_clist_set_text(CListWidget, AIndex, ASubIndex, PChar(AText));
    end;
    Ord(GTK_CELL_PIXTEXT): begin
      if gtk_clist_get_pixtext(CListWidget, AIndex, ASubIndex, @Dummy, @Spacing, @Pixmap, @Mask) <> 0
      then gtk_clist_set_pixtext(CListWidget, AIndex, ASubIndex, PChar(AText), Spacing, Pixmap, Mask)
      else gtk_clist_set_text(CListWidget, AIndex, ASubIndex, PChar(AText));
    end;
    Ord(GTK_CELL_PIXMAP): begin
      if gtk_clist_get_pixmap(CListWidget, AIndex, ASubIndex, @Pixmap, @Mask) <> 0
      then gtk_clist_set_pixtext(CListWidget, AIndex, ASubIndex, PChar(AText), DEFAULT_IMAGE_SPACING, Pixmap, Mask)
      else gtk_clist_set_text(CListWidget, AIndex, ASubIndex, PChar(AText));
    end;
    Ord(GTK_CELL_WIDGET): DebugLN('[TGtkWSCustomListView.ItemSetText] Setting text of widget cell');
    -1: DebugLN('[TGtkWSCustomListView.ItemSetText] Cell (%d,%d) not created', [AIndex, ASubIndex]);
  else
    DebugLN('[TGtkWSCustomListView.ItemSetText] Unknown celltype %d', [Ord(CellType)]);
  end;
end;

procedure TGtkWSCustomListView.ItemShow(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean);
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
  RowTopY: Integer;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemShow')
  then Exit;

  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  
  RowTopY := (CListWidget^.row_height * AIndex) + ((AIndex +1) *
                                     {CELL} 1 {SPACING} + CListWidget^.voffset);
                                     
                                                 // 0=NotVisible
                                                 // 1=PartiallyVisible
                                                 // 2=Fully Visible
                                                 //   |
  if gtk_clist_row_is_visible(CListWidget, AIndex) < (2 - Ord(PartialOK)) then begin
    if (RowTopY + CListWidget^.row_height > CListWidget^.clist_window_height) then begin
      gtk_clist_moveto (CListWidget, AIndex, -1, 1, 0);
        //                              |     |  |  |
        //                        The Row     |  |  |
        //                           The Column  |  |
        //                               Row Align  |
    end //                               Column Align
    else if (RowTopY < 0) then begin
      gtk_clist_moveto (CListWidget, AIndex, -1, 0, 0);
    end;//                                       |
  end;  //                                       |
        //                                       |
        //                      0 = your row will be at the top.
        //                      1 = it will be at the bottom.
end;
{$ENDIF}

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TCustomStatusBar, TGtkWSStatusBar);
//  RegisterWSComponent(TCustomTabSheet, TGtkWSTabSheet);
//  RegisterWSComponent(TCustomPageControl, TGtkWSPageControl);
  RegisterWSComponent(TCustomListView, TGtkWSCustomListView);
//  RegisterWSComponent(TCustomListView, TGtkWSListView);
//  RegisterWSComponent(TCustomProgressBar, TGtkWSProgressBar);
//  RegisterWSComponent(TCustomUpDown, TGtkWSCustomUpDown);
//  RegisterWSComponent(TCustomUpDown, TGtkWSUpDown);
//  RegisterWSComponent(TCustomToolButton, TGtkWSToolButton);
//  RegisterWSComponent(TCustomToolBar, TGtkWSToolBar);
//  RegisterWSComponent(TCustomToolButton, TGtkWSToolButton);
//  RegisterWSComponent(TCustomToolBar, TGtkWSToolBar);
//  RegisterWSComponent(TCustomTrackBar, TGtkWSTrackBar);
//  RegisterWSComponent(TCustomTreeView, TGtkWSCustomTreeView);
//  RegisterWSComponent(TCustomTreeView, TGtkWSTreeView);
////////////////////////////////////////////////////
end.
