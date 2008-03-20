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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  ComCtrls, Classes, FPCAdds, LCLType, LMessages, Controls, Graphics,
  StdCtrls, LCLProc, ImgList, Math, Sysutils,
  // widgetset
  WSComCtrls, WSLCLClasses, WSControls, WSProc,
  // GtkWidgetset
  GtkWSComCtrls,
  GtkWSControls,
  // interface
  GtkDef, GtkProc;
  
type
  // For simplified manipulation
  // Use GetCommonTreeViewWidgets(PGtkTreeView, var TTVWidgets)
  PTVWidgets = ^TTVWidgets;
  TTVWidgets = record
    ScrollingData: TBaseScrollingWinControlData;
    MainView: PGtkWidget; // can be a GtkTreeView or GtkIconView. You have been Warned! :)
    TreeModel: PGtkTreeModel;
    TreeSelection: PGtkTreeSelection;
    WidgetInfo: PWidgetInfo;
    //this is created and destroyed as needed
    //it only holds items which are about to be changed the list is emptied in Gtk2_ItemSelectionChanged
    ItemCache: TStringList;
  end;

type

  { TGtk2WSStatusBar }

  TGtk2WSStatusBar = class(TGtkWSStatusBar)
  private
  protected
  public
    class procedure SetSizeGrip(const AStatusBar: TStatusBar; SizeGrip: Boolean); override;
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
    class function IsIconView(const ALV: TCustomListView): Boolean; virtual;
    // needed when adding or removing columns to a list store
    class procedure ReCreateListStore(const ALV: TCustomListView; const TVWidgets: PTVWidgets); virtual;
    class procedure ReCreateItems(const ALV: TCustomListView); virtual;
    class procedure SetPropertyInternal(const ALV: TCustomListView; const Widgets: PTVWidgets; const AProp: TListViewProperty; const AIsSet: Boolean);
    class procedure SetNeedDefaultColumn(const ALV: TCustomListView; const AValue: Boolean);
    class procedure AddRemoveCheckboxRenderer(const ALV: TCustomListView; const Widgets: PTVWidgets; const Add: Boolean);
  protected
    class procedure SetCallbacks(const AScrollWidget: PGtkWidget; const Widgets: PTVWidgets; const AWidgetInfo: PWidgetInfo); virtual;
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
    class function  ItemGetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem): Boolean; override;
    class function  ItemGetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; out AIsSet: Boolean): Boolean; override; // returns True if supported
    class procedure ItemInsert(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); override;
    class procedure ItemSetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AChecked: Boolean); override;
    class procedure ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AImageIndex: Integer); override;
    class procedure ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); override;
    class procedure ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer; const AText: String); override;
    class procedure ItemShow(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean); override;

    // lv
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;

    class procedure BeginUpdate(const ALV: TCustomListView); override;
    class procedure EndUpdate(const ALV: TCustomListView); override;

    class function GetBoundingRect(const ALV: TCustomListView): TRect; override;
    class function GetDropTarget(const ALV: TCustomListView): Integer; override;
    class function GetFocused(const ALV: TCustomListView): Integer; override;
    class function GetHoverTime(const ALV: TCustomListView): Integer; override;
    class function GetItemAt(const ALV: TCustomListView; x,y: integer): Integer; override;
    class function GetSelCount(const ALV: TCustomListView): Integer; override;
    class function GetSelection(const ALV: TCustomListView): Integer; override;
    class function GetTopItem(const ALV: TCustomListView): Integer; override;
    class function GetViewOrigin(const ALV: TCustomListView): TPoint; override;
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
    class procedure SetSort(const ALV: TCustomListView; const AType: TSortType; const AColumn: Integer); override;
    class procedure SetViewOrigin(const ALV: TCustomListView; const AValue: TPoint); override;
    class procedure SetViewStyle(const ALV: TCustomListView; const Avalue: TViewStyle); override;
  end;

  { TGtk2WSListView }

  TGtk2WSListView = class(TGtkWSListView)
  private
  protected
  public
  end;

  { TGtk2WSProgressBar }

  TGtk2WSProgressBar = class(TWSProgressBar)
  private
    class procedure UpdateProgressBarText(const AProgressBar: TCustomProgressBar); virtual;
  protected
  public
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ApplyChanges(const AProgressBar: TCustomProgressBar); override;
    class procedure SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer); override;
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
    class function  GetPosition(const ATrackBar: TCustomTrackBar): integer; override;
    class procedure SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer); override;
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
procedure GetCommonTreeViewWidgets(ATreeViewHandle: PGtkWidget;
  var TVWidgets: PTVWidgets);
var
  WidgetInfo: PWidgetInfo;
begin
  WidgetInfo := GetWidgetInfo(ATreeViewHandle);
  TVWidgets := PTVWidgets(WidgetInfo^.UserData);
end;
 
function AlignToGtkAlign(Align: TAlignment): gfloat;
begin
  case Align of
    taLeftJustify : AlignToGtkAlign := 0.0;
    taCenter      : AlignToGtkAlign := 0.5;
    taRightJustify: AlignToGtkAlign := 1.0;
  end;
end;

{$I gtk2wscustomlistview.inc}

{ TGtk2WSTrackBar }

class function TGtk2WSTrackBar.GetPosition(const ATrackBar: TCustomTrackBar
  ): integer;
var
  Range: PGtkRange;
begin
  Result := 0;
  if not ATRackBar.HandleAllocated then exit;

  Range := PGtkRange(ATrackBar.Handle);
  Result := Trunc(gtk_range_get_value(Range));
end;

class procedure TGtk2WSTrackBar.SetPosition(const ATrackBar: TCustomTrackBar;
  const NewPosition: integer);
var
  Range: PGtkRange;
begin
  Range := PGtkRange(ATrackBar.Handle);
  gtk_range_set_value(Range, Trunc(NewPosition));
end;

{ TGtk2WSProgressBar }

class procedure TGtk2WSProgressBar.UpdateProgressBarText(const AProgressBar: TCustomProgressBar);
var
  wText: String;
begin
  with AProgressBar do
  begin
    if BarShowText then
    begin
       wText := Format('%d from [%d-%d] (%%p%%%%)', [AProgressBar.Position, Min, Max]);
       gtk_progress_set_format_string (GTK_PROGRESS(Pointer(Handle)), PChar(wText));
    end;
    gtk_progress_set_show_text (GTK_PROGRESS(Pointer(Handle)), BarShowText);
  end;
end;

class function TGtk2WSProgressBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  Widget := gtk_progress_bar_new;
  Result := TLCLIntfHandle(PtrUInt(Widget));
  WidgetInfo := CreateWidgetInfo(Pointer(Result), AWinControl, AParams);
  Set_RC_Name(AWinControl, Widget);

  TGtkWSWinControl.SetCallbacks(PGtkObject(Widget), TComponent(WidgetInfo^.LCLObject));
end;

class procedure TGtk2WSProgressBar.ApplyChanges(
  const AProgressBar: TCustomProgressBar);
var
  wHandle: Pointer;
  wOrientation: TGtkProgressBarOrientation;
begin
  wHandle := Pointer(AProgressBar.Handle);
  with AProgressBar do
  begin
    gtk_progress_bar_set_bar_style (GTK_PROGRESS_BAR(wHandle),
                                     TGtkProgressBarStyle(Ord(Smooth = False)));
                                     // 0 = Smooth
                                     // 1 = not Smooth :) (AH)
    case Orientation of
      pbHorizontal : wOrientation := GTK_PROGRESS_LEFT_TO_RIGHT;
      pbVertical   : wOrientation := GTK_PROGRESS_BOTTOM_TO_TOP;
      pbRightToLeft: wOrientation := GTK_PROGRESS_RIGHT_TO_LEFT;
      pbTopDown    : wOrientation := GTK_PROGRESS_TOP_TO_BOTTOM;
    end;
    gtk_progress_bar_set_orientation(GTK_PROGRESS_BAR(wHandle), wOrientation);

    UpdateProgressBarText(AProgressBar);
  end;
end;

class procedure TGtk2WSProgressBar.SetPosition(
  const AProgressBar: TCustomProgressBar; const NewPosition: integer);
begin
  with AProgressBar do
    gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(Pointer(Handle)),
            (AProgressBar.Position+Abs(Min)) / (Max+Abs(Min)));
  if AProgressBar.BarShowText then
      UpdateProgressBarText(AProgressBar);
end;

{ TGtk2WSStatusBar }

class procedure TGtk2WSStatusBar.SetSizeGrip(const AStatusBar: TStatusBar;
  SizeGrip: Boolean);
var
  LastWidget, Widget: PGtkWidget;
begin
  if not WSCheckHandleAllocated(AStatusBar, 'SetSizeGrip') then
    Exit;
  Widget := PGtkWidget(AStatusBar.Handle);
  LastWidget := PGtkBoxChild(g_list_last(PGtkBox(Widget)^.children)^.data)^.widget;
  gtk_statusbar_set_has_resize_grip(PGtkStatusBar(LastWidget), AStatusBar.SizeGrip and AStatusBar.SizeGripEnabled);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TStatusBar, TGtk2WSStatusBar);
//  RegisterWSComponent(TCustomTabSheet, TGtk2WSTabSheet);
//  RegisterWSComponent(TCustomPageControl, TGtk2WSPageControl);
  RegisterWSComponent(TCustomListView, TGtk2WSCustomListView);
//  RegisterWSComponent(TCustomListView, TGtk2WSListView);
  RegisterWSComponent(TCustomProgressBar, TGtk2WSProgressBar);
//  RegisterWSComponent(TCustomUpDown, TGtk2WSCustomUpDown);
//  RegisterWSComponent(TCustomUpDown, TGtk2WSUpDown);
//  RegisterWSComponent(TCustomToolButton, TGtk2WSToolButton);
//  RegisterWSComponent(TCustomToolBar, TGtk2WSToolBar);
//  RegisterWSComponent(TCustomToolButton, TGtk2WSToolButton);
//  RegisterWSComponent(TCustomToolBar, TGtk2WSToolBar);
  RegisterWSComponent(TCustomTrackBar, TGtk2WSTrackBar);
//  RegisterWSComponent(TCustomTreeView, TGtk2WSCustomTreeView);
//  RegisterWSComponent(TCustomTreeView, TGtk2WSTreeView);
////////////////////////////////////////////////////
end.
