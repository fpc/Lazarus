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
  ComCtrls, Classes, FPCAdds, LCLType, LMessages, Controls, Graphics,
  LCLProc,
  // widgetset
  WSComCtrls, WSLCLClasses, WSProc,
  // interface
  GtkDef;

type

  { TGtkWSStatusBar }

  TGtkWSStatusBar = class(TWSStatusBar)
  public
    class procedure PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure Update(const AStatusBar: TStatusBar); override;
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

    class procedure UpdateProperties(const ACustomListView: TCustomListView); override;
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
    class procedure ApplyChanges(const AProgressBar: TCustomProgressBar); override;
    class procedure SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer); override;
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
{$ifdef OldToolbar}
    class function  GetButtonCount(const AToolBar: TToolBar): integer; override;
    class procedure InsertToolButton(const AToolBar: TToolbar; const AControl: TControl); override;
    class procedure DeleteToolButton(const AToolBar: TToolbar; const AControl: TControl); override;
{$endif}    
  end;

  { TGtkWSTrackBar }

  TGtkWSTrackBar = class(TWSTrackBar)
  private
  protected
  public
    class procedure ApplyChanges(const ATrackBar: TCustomTrackBar); override;
    class function  GetPosition(const ATrackBar: TCustomTrackBar): integer; override;
    class procedure SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer); override;
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
            and (g_list_find(CListWidget^.selection, Pointer(PtrInt(Aindex))) <> nil);
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

procedure TGtkWSCustomListView.UpdateProperties(const ACustomListView: TCustomListView);
var
  Widget   : PGtkWidget;
  pRowText : PChar;
  BitImage : TBitMap;
  I, X: integer;
begin
  // set up columns..
  Widget:= GetWidgetInfo(PGtkWidget(ACustomListView.Handle), True)^.CoreWidget;

  gtk_clist_freeze(GTK_CLIST(Widget));

  for I := 0 to TListView(ACustomListView).Columns.Count-1 do
  begin
    gtk_clist_set_column_title(GTK_CLIST(Widget),I,
                           PChar(TListView(ACustomListView).Columns[i].Caption));

    // set column alignment
    gtk_clist_set_column_justification(GTK_CLIST(Widget),I,
             aGTKJUSTIFICATION[TListView(ACustomListView).Columns[i].Alignment]);
    // set auto sizing
    gtk_clist_set_column_auto_resize(GTK_CLIST(Widget),I,
             TListView(ACustomListView).Columns[i].AutoSize);
    // set width
    gtk_clist_set_column_width(GTK_CLIST(Widget),I,
             TListView(ACustomListView).Columns[i].Width);
    // set Visible
    gtk_clist_set_column_visibility(GTK_CLIST(Widget),I,
             TListView(ACustomListView).Columns[i].Visible);
    // set MinWidth
    if TListView(ACustomListView).Columns[i].MinWidth>0 then
      gtk_clist_set_column_min_width(GTK_CLIST(Widget), I,
                                 TListView(ACustomListView).Columns[i].MinWidth);
    // set MaxWidth
    if (TListView(ACustomListView).Columns[i].MaxWidth>=
      TListView(ACustomListView).Columns[i].MinWidth)
    and (TListView(ACustomListView).Columns[i].MaxWidth>0) then
      gtk_clist_set_column_max_width(GTK_CLIST(Widget), I,
                                 TListView(ACustomListView).Columns[i].MaxWidth);
  end;

  //sorting
  if (TListView(ACustomListView).ViewStyle = vsReport)
  then gtk_clist_column_titles_show(GTK_CLIST(Widget))
  else gtk_clist_column_titles_Hide(GTK_CLIST(Widget));

  gtk_clist_set_sort_column(GTK_CLIST(Widget),
    TListView(ACustomListView).SortColumn);

  //multiselect
  gtk_clist_set_selection_mode(GTK_CLIST(Widget),
    aGTkSelectionMode[TListView(ACustomListView).MultiSelect]);

//TODO:This doesn't work right now
//                gtk_clist_set_auto_sort(PgtkCList(handle),TListView(ACustomListView).Sorted);
  //
  //do items...
  //
  for I := 0 to TListView(ACustomListView).Items.Count-1 do
  begin
    pRowText:=PChar(TListItem(TListView(ACustomListView).Items[i]).Caption);
    gtk_clist_set_text(GTK_CLIST(Widget),I,0,pRowText);

    //do image if one is assigned....
    // TODO: Largeimage support
    if  (TListView(ACustomListView).SmallImages <> nil)
    and (TListItem(TListView(ACustomListView).Items[i]).ImageIndex > -1)
    then begin
      DebugLn('Checking images');
      if (TListItem(TListView(ACustomListView).Items[i]).ImageIndex
        < TListView(ACustomListView).SmallImages.Count)
      then begin
        //draw image
        //DebugLn('drawing image');
        //DebugLn('TListItem(TListView(ACustomListView).Items[i]).ImageIndex is ',TListItem(TListView(ACustomListView).Items[i]).ImageIndex);

        BitImage := TBitmap.Create;
        TListView(ACustomListView).SmallImages.GetBitmap(
           TListItem(TListView(ACustomListView).Items[i]).ImageIndex,BitImage);

        gtk_clist_set_pixmap(GTK_CLIST(Widget),I,0,
          pgdkPixmap(PgdiObject(BitImage.handle)^.GDIBitmapObject),
          nil);
        gtk_clist_set_pixtext(GTK_CLIST(Widget),I,0,pRowText,3,
          pgdkPixmap(PgdiObject(BitImage.handle)^.GDIBitmapObject),
          nil);
//                      bitimage.Free;
      end;
    end;

    if (TListView(ACustomListView).ViewStyle = vsReport)
    then begin //columns showing
      for X := 1 to TListView(ACustomListView).Columns.Count-1 do
      begin
        if ( X <= TListItem(TListView(ACustomListView).Items[i]).SubItems.Count)
        then begin
          pRowText:=PChar(TListItem(
            TListView(ACustomListView).Items[i]).SubItems.Strings[X-1]);
          gtk_clist_set_text(GTK_CLIST(Widget),I,X,pRowText);
        end;
      end;  //for loop
    end;
  end;
  gtk_clist_thaw(GTK_CLIST(Widget));
end;

{$ENDIF}

{ TGtkWSProgressBar }

procedure TGtkWSProgressBar.ApplyChanges(const AProgressBar: TCustomProgressBar);
var
  wHandle: HWND;
begin
  wHandle := AProgressBar.Handle;
  with AProgressBar do
  begin
    if Smooth
    then gtk_progress_bar_set_bar_style (GTK_PROGRESS_BAR(Pointer(Pointer(wHandle))),
                                         GTK_PROGRESS_CONTINUOUS)
    else gtk_progress_bar_set_bar_style (GTK_PROGRESS_BAR(Pointer(Pointer(wHandle))),
                                         GTK_PROGRESS_DISCRETE);
    case Orientation of
    pbVertical   : gtk_progress_bar_set_orientation(
                                  GTK_PROGRESS_BAR(Pointer(Pointer(wHandle))),
                                  GTK_PROGRESS_BOTTOM_TO_TOP);
    pbRightToLeft: gtk_progress_bar_set_orientation(
                                  GTK_PROGRESS_BAR(Pointer(Pointer(wHandle))),
                                  GTK_PROGRESS_RIGHT_TO_LEFT);
    pbTopDown    : gtk_progress_bar_set_orientation(
                                  GTK_PROGRESS_BAR(Pointer(Pointer(wHandle))),
                                  GTK_PROGRESS_TOP_TO_BOTTOM);
    else { pbHorizontal is default }
      gtk_progress_bar_set_orientation(GTK_PROGRESS_BAR(Pointer(Pointer(wHandle))),
                                        GTK_PROGRESS_LEFT_TO_RIGHT);
    end;
    if BarShowText then
    begin
       gtk_progress_set_format_string (GTK_PROGRESS(Pointer(Pointer(wHandle))),
                                       '%v from [%l-%u] (=%p%%)');
       gtk_progress_set_show_text (GTK_PROGRESS(Pointer(Pointer(wHandle))), GdkTrue);
    end
    else
       gtk_progress_set_show_text (GTK_PROGRESS(Pointer(Pointer(wHandle))), GDKFalse);
    gtk_progress_configure(GTK_PROGRESS(Pointer(Pointer(wHandle))),Position,Min,Max);
  end;
end;

procedure TGtkWSProgressBar.SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer);
begin
  gtk_progress_set_value(GTK_PROGRESS(Pointer(AProgressBar.Handle)), NewPosition);
end;

{ TGtkWSToolbar }

{$ifdef OldToolbar}

function  TGtkWSToolbar.GetButtonCount(const AToolBar: TToolBar): integer;
begin
  Result := PGtkToolbar(AToolbar.Handle)^.num_Children;
end;

procedure TGtkWSToolbar.InsertToolButton(const AToolBar: TToolbar; const AControl: TControl);
var
  Num         : Integer;               // currently only used for LM_INSERTTOOLBUTTON and LM_ADDITEM
  pStr        : PChar;
  pStr2       : PChar;                 // currently only used for LM_INSERTTOOLBUTTON
  handle: HWND;
begin
  if (AControl is TToolbutton) then
  begin
    pStr := StrAlloc(Length(TToolbutton(AControl).Caption)+1);
    handle := TToolButton(AControl).Handle;
    try
      StrPCopy(pStr,TToolbutton(AControl).Caption);
      pStr2 := StrAlloc(Length(TControl(AControl).Hint)+1);
    finally
      StrPCopy(pStr2,TControl(AControl).Hint);
    end;
  end
  else Begin
     RaiseException('Can not assign this control to the toolbar');
     exit;
  end;

  num := TToolbar(TWinControl(AControl).parent).Buttonlist.IndexOf(TControl(AControl));
  if num < 0 then Num := TToolbar(TWinControl(AControl).parent).Buttonlist.Count+1;
  Assert(False, Format('Trace:NUM = %d in INSERTBUTTON',[num]));

  gtk_toolbar_insert_widget(pGTKToolbar(TWinControl(AControl).parent.Handle),
       pgtkwidget(handle),pstr,pStr2,Num);
  StrDispose(pStr);
  StrDispose(pStr2);
end;

procedure TGtkWSToolbar.DeleteToolButton(const AToolBar: TToolbar; const AControl: TControl);
begin
  with pgtkToolbar(TToolbar(TWinControl(AControl).parent).handle)^ do
    children := g_list_remove(pgList(children), AControl);
  // Next 3 lines: should be same as above, remove when above lines are proofed
  //         pgtkToolbar(TToolbar(TWinControl(AControl).parent).handle)^.children :=
  //            g_list_remove(pgList(pgtkToolbar(TToolbar(TWinControl(AControl).parent).handle)^.children),
  //               AControl);
end;

{$endif}

{ TGtkWSTrackBar }

procedure TGtkWSTrackBar.ApplyChanges(const ATrackBar: TCustomTrackBar);
var
  wHandle: HWND;
  Widget: PGtkWidget;
begin
  with ATrackBar do
  begin
    wHandle := Handle;
    Widget := GTK_WIDGET(gtk_range_get_adjustment (GTK_RANGE(Pointer(wHandle))));
    GTK_ADJUSTMENT(Widget)^.lower := Min;
    GTK_ADJUSTMENT(Widget)^.Upper := Max;
    GTK_ADJUSTMENT(Widget)^.Value := Position;
    GTK_ADJUSTMENT(Widget)^.step_increment := LineSize;
    GTK_ADJUSTMENT(Widget)^.page_increment := PageSize;
    { now do some of the more sophisticated features }
    { Hint: For some unknown reason we have to disable the draw_value first,
      otherwise it's set always to true }
    gtk_scale_set_draw_value (GTK_SCALE (Pointer(wHandle)), false);

    if ShowScale then
    begin
       gtk_scale_set_draw_value (GTK_SCALE (Pointer(wHandle)), ShowScale);
       case ScalePos of
          trLeft  : gtk_scale_set_value_pos (GTK_SCALE (Pointer(wHandle)), GTK_POS_LEFT);
          trRight : gtk_scale_set_value_pos (GTK_SCALE (Pointer(wHandle)), GTK_POS_RIGHT);
          trTop   : gtk_scale_set_value_pos (GTK_SCALE (Pointer(wHandle)), GTK_POS_TOP);
          trBottom: gtk_scale_set_value_pos (GTK_SCALE (Pointer(wHandle)), GTK_POS_BOTTOM);
       end;
    end;
    //Not here (Delphi compatibility):  gtk_signal_emit_by_name (GTK_Object (Widget), 'value_changed');
  end;
end;

function  TGtkWSTrackBar.GetPosition(const ATrackBar: TCustomTrackBar): integer;
begin
  if ATrackBar.HandleAllocated then 
  begin
    Result := RoundToInt(gtk_range_get_adjustment(
      GTK_RANGE(Pointer(ATrackBar.Handle)))^.value);
  end else
    Result := 0;
end;

procedure TGtkWSTrackBar.SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer);
var
  Handle: HWND;
begin
  Handle := ATrackBar.Handle;
  gtk_range_get_adjustment(GTK_RANGE(Pointer(Handle)))^.value := NewPosition;
  g_signal_emit_by_name(PGtkObject(
    gtk_range_get_adjustment(GTK_RANGE(Pointer(Handle)))), 'value_changed');
end;

{ TGtkWSStatusBar }

procedure TGtkWSStatusBar.PanelUpdate(const AStatusBar: TStatusBar;
  PanelIndex: integer);
var
  HBox: PGtkWidget;
  StatusPanelWidget: PGtkWidget;
  BoxChild: PGtkBoxChild;
begin
  //DebugLn('TGtkWidgetSet.StatusBarPanelUpdate ',HexStr(Cardinal(AStatusBar),8),' PanelIndex=',dbgs(PanelIndex));
  if PanelIndex>=0 then begin
    // update one
    HBox:=PGtkWidget(AStatusBar.Handle);
    BoxChild:=PGtkBoxChild(g_list_nth_data(PGtkBox(HBox)^.children,PanelIndex));
    if BoxChild=nil then
      RaiseGDBException('TGtkWidgetSet.StatusBarPanelUpdate Index out of bounds');
    StatusPanelWidget:=BoxChild^.Widget;
    UpdateStatusBarPanel(AStatusBar,PanelIndex,StatusPanelWidget);
  end else begin
    // update all
    UpdateStatusBarPanels(AStatusBar,PGtkWidget(AStatusBar.Handle));
  end;
end;

procedure TGtkWSStatusBar.SetPanelText(const AStatusBar: TStatusBar;
  PanelIndex: integer);
begin
  PanelUpdate(AStatusBar,PanelIndex);
end;

procedure TGtkWSStatusBar.Update(const AStatusBar: TStatusBar);
begin
  //DebugLn('TGtkWidgetSet.StatusBarUpdate ',HexStr(Cardinal(AStatusBar),8));
  UpdateStatusBarPanels(AStatusBar,PGtkWidget(AStatusBar.Handle));
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TStatusBar, TGtkWSStatusBar);
//  RegisterWSComponent(TCustomTabSheet, TGtkWSTabSheet);
//  RegisterWSComponent(TCustomPageControl, TGtkWSPageControl);
  RegisterWSComponent(TCustomListView, TGtkWSCustomListView);
//  RegisterWSComponent(TCustomListView, TGtkWSListView);
  RegisterWSComponent(TCustomProgressBar, TGtkWSProgressBar);
//  RegisterWSComponent(TCustomUpDown, TGtkWSCustomUpDown);
//  RegisterWSComponent(TCustomUpDown, TGtkWSUpDown);
//  RegisterWSComponent(TCustomToolButton, TGtkWSToolButton);
  RegisterWSComponent(TToolBar, TGtkWSToolBar);
  RegisterWSComponent(TCustomTrackBar, TGtkWSTrackBar);
//  RegisterWSComponent(TCustomTreeView, TGtkWSCustomTreeView);
//  RegisterWSComponent(TCustomTreeView, TGtkWSTreeView);
////////////////////////////////////////////////////
end.
