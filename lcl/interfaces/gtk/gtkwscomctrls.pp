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
  GLib2, Gtk2,
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
  private
    class procedure InternalChangeItem(const ACListWidget: PGtkCList; const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); 
  protected
  public
    class procedure ChangeItem(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); override;
    class procedure DeleteItem(const ALV: TCustomListView; const AIndex: Integer); override;
    class procedure InsertItem(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); override;
    class procedure SelectItem(const ALV: TCustomListView; const AItem: TListItem); override;
    class procedure ShowItem(const ALV: TCustomListView; const AItem: TListItem); override;
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


  { TGtkWSCustomListView }

procedure TGtkWSCustomListView.ChangeItem(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); 
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'ChangeItem')
  then Exit;
  
  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  InternalChangeItem(CListWidget, ALV, AIndex, AItem);
end;
  

procedure TGtkWSCustomListView.DeleteItem(const ALV: TCustomListView; const AIndex: Integer); 
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'DeleteItem')
  then Exit;
  
  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  gtk_clist_remove(CListWidget, AIndex);
end;
                 
type 
  TLVHack = class(TCustomListView)
  end;
                 
procedure TGtkWSCustomListView.InternalChangeItem(const ACListWidget: PGtkCList; const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); 
var
  ImageBitmap, MaskBitmap: TBitmap;
  ImageRect: TRect;
  Pixmap: PGdkPixmap;
  Mask: PGdkBitmap;
  n, Count: integer;
//  pStr: PChar;
begin
//  pStr:=PChar(ListItem.Caption);
//  if pStr=nil then pStr:=#0;

  if  (TLVHack(ALV).SmallImages <> nil) 
  and (AItem.ImageIndex >= 0)
  and (AItem.ImageIndex < TLVHack(ALV).SmallImages.Count)
  then begin
    // set image & caption
    TLVHack(ALV).SmallImages.GetInternalImage(AItem.ImageIndex, ImageBitmap, MaskBitmap, ImageRect);
    if (ImageRect.Left <> 0) 
    or (ImageRect.Top <> 0) 
    then DebugLn('WARNING: TGtkWidgetSet.ListViewChangeItem does not support combined imagelists');
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


procedure TGtkWSCustomListView.InsertItem(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); 
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
  Titles: PPGChar;
  idx, Count: Integer;
begin
  if not WSCheckHandleAllocated(ALV, 'InsertItem')
  then Exit;
  
  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  Count := CListWidget^.columns;
  
  if Count = 0 
  then begin
    DebugLn('WARNING: TGtkWSCustomListView.InsertItem  CListWidget^.columns = 0');
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
  
  InternalChangeItem(CListWidget, ALV, idx, AItem);
end;

procedure TGtkWSCustomListView.SelectItem(const ALV: TCustomListView; const AItem: TListItem); 
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
begin
  if not WSCheckHandleAllocated(ALV, 'SelectItem')
  then Exit;
  
  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);

  gtk_clist_unselect_all(CListWidget);
  if AItem <> nil
  then gtk_clist_select_row(CListWidget, AItem.Index, 0);
end;

procedure TGtkWSCustomListView.ShowItem(const ALV: TCustomListView; const AItem: TListItem); 
var
  WidgetInfo: PWidgetInfo;
  CListWidget: PGtkCList;
  idx: Integer;
begin
  if not WSCheckHandleAllocated(ALV, 'ShowItem')
  then Exit;
  
  WidgetInfo := GetWidgetInfo(Pointer(ALV.Handle));
  CListWidget := PGtkCList(WidgetInfo^.CoreWidget);
  
  //0=NotVisible
  //1=PartiallyVisible
  //2=Fully Visible
  idx := AItem.Index;
  if gtk_clist_row_is_visible(CListWidget, idx) < 2 
  then gtk_clist_moveto(CListWidget, idx, 0, 1, 0);
end;


initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TStatusBar, TGtkWSStatusBar);
//  RegisterWSComponent(TTabSheet, TGtkWSTabSheet);
//  RegisterWSComponent(TPageControl, TGtkWSPageControl);
  RegisterWSComponent(TCustomListView, TGtkWSCustomListView);
//  RegisterWSComponent(TListView, TGtkWSListView);
//  RegisterWSComponent(TProgressBar, TGtkWSProgressBar);
//  RegisterWSComponent(TCustomUpDown, TGtkWSCustomUpDown);
//  RegisterWSComponent(TUpDown, TGtkWSUpDown);
//  RegisterWSComponent(TToolButton, TGtkWSToolButton);
//  RegisterWSComponent(TToolBar, TGtkWSToolBar);
//  RegisterWSComponent(TToolButton, TGtkWSToolButton);
//  RegisterWSComponent(TToolBar, TGtkWSToolBar);
//  RegisterWSComponent(TTrackBar, TGtkWSTrackBar);
//  RegisterWSComponent(TCustomTreeView, TGtkWSCustomTreeView);
//  RegisterWSComponent(TTreeView, TGtkWSTreeView);
////////////////////////////////////////////////////
end.
