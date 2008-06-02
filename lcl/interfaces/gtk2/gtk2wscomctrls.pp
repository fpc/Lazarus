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
  // RTL, FCL, LCL
  ComCtrls, Classes, FPCAdds, LCLType, LMessages, Controls, Graphics,
  StdCtrls, LCLProc, ImgList, Math, Sysutils, InterfaceBase,
  // widgetset
  WSComCtrls, WSLCLClasses, WSControls, WSProc,
  // GtkWidgetset
  GtkWSControls, GtkDef, GtkProc,
  // Gtk2Widgetset
  Gtk2WSControls, Gtk2Int;
  
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

  TGtk2WSStatusBar = class(TWSStatusBar)
  private
  protected
  public
    class procedure SetCallbacks(const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure Update(const AStatusBar: TStatusBar); override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;

    class procedure SetSizeGrip(const AStatusBar: TStatusBar; SizeGrip: Boolean); override;
  end;

  { TGtk2WSTabSheet }

  TGtk2WSTabSheet = class(TWSTabSheet)
  private
  protected
  public
  end;

  { TGtk2WSPageControl }

  TGtk2WSPageControl = class(TWSPageControl)
  private
  protected
  public
  end;

  { TGtk2WSCustomListView }

  TGtk2WSCustomListView = class(TWSCustomListView)
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

  TGtk2WSListView = class(TWSListView)
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

  TGtk2WSCustomUpDown = class(TWSCustomUpDown)
  private
  protected
  public
  end;

  { TGtk2WSUpDown }

  TGtk2WSUpDown = class(TWSUpDown)
  private
  protected
  public
  end;

  { TGtk2WSToolButton }

  TGtk2WSToolButton = class(TWSToolButton)
  private
  protected
  public
  end;

  { TGtk2WSToolBar }

  TGtk2WSToolBar = class(TWSToolBar)
  private
  protected
    class procedure SetCallbacks(const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  public
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TGtk2WSTrackBar }

  TGtk2WSTrackBar = class(TWSTrackBar)
  private
  protected
    class procedure SetCallbacks(const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  public
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ApplyChanges(const ATrackBar: TCustomTrackBar); override;
    class function  GetPosition(const ATrackBar: TCustomTrackBar): integer; override;
    class procedure SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer); override;
  end;

  { TGtk2WSCustomTreeView }

  TGtk2WSCustomTreeView = class(TWSCustomTreeView)
  private
  protected
  public
  end;

  { TGtk2WSTreeView }

  TGtk2WSTreeView = class(TWSTreeView)
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

class procedure TGtk2WSTrackBar.SetCallbacks(const AWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AWidget), TComponent(AWidgetInfo^.LCLObject));
  TGtk2Widgetset(WidgetSet).SetCallback(LM_CHANGED, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
end;

class function TGtk2WSTrackBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Adjustment: PGtkAdjustment;
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  with TCustomTrackBar(AWinControl) do
  begin
    Adjustment := PGtkAdjustment(gtk_adjustment_new (Position, Min, Max,
                                                  linesize, pagesize, 1));
    if (Orientation = trHorizontal) then
      Widget := gtk_hscale_new(Adjustment)
    else
      Widget := gtk_vscale_new(Adjustment);
//     gtk_scale_set_digits(PGtkScale(Widget), 0);
  end;
  Result := TLCLIntfHandle(PtrUInt(Widget));
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}
  WidgetInfo := CreateWidgetInfo(Pointer(Result), AWinControl, AParams);
  Set_RC_Name(AWinControl, Widget);
  SetCallbacks(Widget, WidgetInfo);
end;

class procedure TGtk2WSTrackBar.ApplyChanges(const ATrackBar: TCustomTrackBar);
var
  wHandle: HWND;
  Adjustment: PGtkAdjustment;
begin
  with ATrackBar do
  begin
    wHandle := Handle;
    Adjustment := gtk_range_get_adjustment (GTK_RANGE(Pointer(wHandle)));
    Adjustment^.lower := Min;
    Adjustment^.Upper := Max;
    Adjustment^.Value := Position;
    Adjustment^.step_increment := LineSize;
    Adjustment^.page_increment := PageSize;
    { now do some of the more sophisticated features }
    { Hint: For some unknown reason we have to disable the draw_value first,
      otherwise it's set always to true }
    gtk_scale_set_draw_value (GTK_SCALE (Pointer(wHandle)), false);

    if (TickStyle<>tsNone) then
    begin
       gtk_scale_set_draw_value (GTK_SCALE (Pointer(wHandle)), true);
       case ScalePos of
          trLeft  : gtk_scale_set_value_pos (GTK_SCALE (Pointer(wHandle)), GTK_POS_LEFT);
          trRight : gtk_scale_set_value_pos (GTK_SCALE (Pointer(wHandle)), GTK_POS_RIGHT);
          trTop   : gtk_scale_set_value_pos (GTK_SCALE (Pointer(wHandle)), GTK_POS_TOP);
          trBottom: gtk_scale_set_value_pos (GTK_SCALE (Pointer(wHandle)), GTK_POS_BOTTOM);
       end;
    end;
    //Not here (Delphi compatibility):  gtk_signal_emit_by_name (GTK_Object (Adjustment), 'value_changed');
  end;
end;

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
       wText := Format('%d from [%d-%d] (%%p%%%%)', [Position, Min, Max]);
       gtk_progress_set_format_string(PGtkProgress(Handle), PChar(wText));
    end;
    gtk_progress_set_show_text(PGtkProgress(Handle), BarShowText);
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

class procedure TGtk2WSProgressBar.ApplyChanges(const AProgressBar: TCustomProgressBar);
const
  OrientationMap: array[TProgressBarOrientation] of TGtkProgressBarOrientation =
  (
{ pbHorizontal  } GTK_PROGRESS_LEFT_TO_RIGHT,
{ pbVertical,   } GTK_PROGRESS_BOTTOM_TO_TOP,
{ pbRightToLeft } GTK_PROGRESS_RIGHT_TO_LEFT,
{ pbTopDown     } GTK_PROGRESS_TOP_TO_BOTTOM
  );

  SmoothMap: array[Boolean] of TGtkProgressBarStyle =
  (
{ False } GTK_PROGRESS_DISCRETE,
{ True  } GTK_PROGRESS_CONTINUOUS
  );

var
  Progress: PGtkProgressBar;
begin
  if not WSCheckHandleAllocated(AProgressBar, 'TGtk2WSProgressBar.ApplyChanges') then
    Exit;
  Progress := PGtkProgressBar(AProgressBar.Handle);

  with AProgressBar do
  begin
    gtk_progress_bar_set_bar_style(Progress, SmoothMap[Smooth]);
    gtk_progress_bar_set_orientation(Progress, OrientationMap[Orientation]);
  end;

  // The posision also needs to be updated at ApplyChanges
  SetPosition(AProgressBar, AProgressBar.Position);
end;

class procedure TGtk2WSProgressBar.SetPosition(
  const AProgressBar: TCustomProgressBar; const NewPosition: integer);
begin
  if not WSCheckHandleAllocated(AProgressBar, 'TGtk2WSProgressBar.SetPosition') then
    Exit;
    
  // Gtk2 wishes the position in a floating-point value between
  // 0.0 and 1.0, and we calculate that with:
  // (Pos - Min) / (Max - Min)
  // regardless if any of them is negative the result is correct
  gtk_progress_bar_set_fraction(PGtkProgressBar(AProgressBar.Handle),
    (NewPosition - AProgressBar.Min) /
    (AProgressBar.Max - AProgressBar.Min));

  UpdateProgressBarText(AProgressBar);
end;

{ TGtk2WSStatusBar }

class procedure TGtk2WSStatusBar.SetCallbacks(const AWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AWidget), TComponent(AWidgetInfo^.LCLObject));
end;

class function TGtk2WSStatusBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  Widget := gtk_hbox_new(false,0);
  UpdateStatusBarPanels(AWinControl, Widget);
  Result := TLCLIntfHandle(PtrUInt(Widget));
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}
  WidgetInfo := CreateWidgetInfo(Pointer(Result), AWinControl, AParams);
  Set_RC_Name(AWinControl, Widget);
  SetCallbacks(Widget, WidgetInfo);
end;

class procedure TGtk2WSStatusBar.PanelUpdate(const AStatusBar: TStatusBar;
  PanelIndex: integer);
var
  HBox: PGtkWidget;
  StatusPanelWidget: PGtkWidget;
  BoxChild: PGtkBoxChild;
begin
  //DebugLn('TGtkWidgetSet.StatusBarPanelUpdate ',DbgS(AStatusBar),' PanelIndex=',dbgs(PanelIndex));
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

class procedure TGtk2WSStatusBar.SetPanelText(const AStatusBar: TStatusBar;
  PanelIndex: integer);
begin
  PanelUpdate(AStatusBar,PanelIndex);
end;

class procedure TGtk2WSStatusBar.Update(const AStatusBar: TStatusBar);
begin
  //DebugLn('TGtkWidgetSet.StatusBarUpdate ',DbgS(AStatusBar));
  UpdateStatusBarPanels(AStatusBar,PGtkWidget(AStatusBar.Handle));
end;

class procedure TGtk2WSStatusBar.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
var
  StatusBarWidget: PGtkWidget;
  Requisition: TGtkRequisition;
begin
  StatusBarWidget:=GetStyleWidget(lgsStatusBar);
  // set size to default
  gtk_widget_set_usize(StatusBarWidget,-1,-1);
  // ask default size
  gtk_widget_size_request(StatusBarWidget,@Requisition);
  PreferredHeight:=Requisition.height;
  //debugln('TGtkWSStatusBar.GetPreferredSize END ',dbgs(PreferredHeight));
end;

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

{ TGtk2WSToolBar }

class procedure TGtk2WSToolBar.SetCallbacks(const AWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AWidget), TComponent(AWidgetInfo^.LCLObject));
end;

class function TGtk2WSToolBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Widget, ClientWidget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  // Creates the widget
  Widget:= gtk_hbox_new(false,0);
  ClientWidget := CreateFixedClientWidget;
  gtk_container_add(GTK_CONTAINER(Widget), ClientWidget);

  Result := TLCLIntfHandle(PtrUInt(Widget));
  WidgetInfo := CreateWidgetInfo(Widget, AWinControl, AParams);

  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}

  gtk_widget_show(ClientWidget);
  SetFixedWidget(Widget, ClientWidget);
  SetMainWidget(Widget, ClientWidget);
  gtk_widget_show(Widget);

  Set_RC_Name(AWinControl, Widget);
  SetCallbacks(Widget, WidgetInfo);
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
  RegisterWSComponent(TToolBar, TGtk2WSToolBar);
  RegisterWSComponent(TCustomTrackBar, TGtk2WSTrackBar);
//  RegisterWSComponent(TCustomTreeView, TGtk2WSCustomTreeView);
//  RegisterWSComponent(TCustomTreeView, TGtk2WSTreeView);
////////////////////////////////////////////////////
end.
