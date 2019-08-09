{
 *****************************************************************************
 *                             Gtk2WSComCtrls.pp                             * 
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
unit Gtk2WSComCtrls;

{$mode objfpc}{$H+}
{$I gtk2defines.inc}

interface

uses
  // RTL, FCL, libs
  Math, Sysutils, Classes, GLib2, Gtk2, Gdk2, Gdk2pixbuf,
  // LazUtils
  LazTracer,
  // LCL
  LCLType, LCLIntf, LMessages, Controls, Graphics, ComCtrls, StdCtrls, Forms,
  ImgList, InterfaceBase,
  // widgetset
  WSComCtrls, WSLCLClasses, WSControls, WSProc,
  // GtkWidgetset
  Gtk2Def, Gtk2Globals, Gtk2Proc,
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
    OldTreeSelection: PGList; // needed only by gtk < 2.10 ! issue #19820
    Images: TList;
  end;

type
  { TGtk2WSCustomPage }

  TGtk2WSCustomPage = class(TWSCustomPage)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure UpdateProperties(const ACustomPage: TCustomPage); override;
    class procedure SetBounds(const {%H-}AWinControl: TWinControl; const {%H-}ALeft, {%H-}ATop, {%H-}AWidth, {%H-}AHeight: Integer); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
    class function GetDefaultClientRect(const AWinControl: TWinControl;
             const {%H-}aLeft, {%H-}aTop, {%H-}aWidth, {%H-}aHeight: integer; var aClientRect: TRect
             ): boolean; override;
  end;

  { TGtk2WSCustomTabControl }

  TGtk2WSCustomTabControl = class(TWSCustomTabControl)
  private
    class function CreateTTabControlHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): HWND;
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const AWinControl: TWinControl;
                                const AParams: TCreateParams): HWND; override;
    class function GetDefaultClientRect(const AWinControl: TWinControl;
             const {%H-}aLeft, {%H-}aTop, aWidth, aHeight: integer; var aClientRect: TRect
             ): boolean; override;
    class procedure AddPage(const ATabControl: TCustomTabControl;
      const AChild: TCustomPage; const AIndex: integer); override;
    class procedure MovePage(const ATabControl: TCustomTabControl;
      const AChild: TCustomPage; const NewIndex: integer); override;

    class function GetCapabilities: TCTabControlCapabilities; override;
    class function GetNotebookMinTabHeight(const AWinControl: TWinControl): integer; override;
    class function GetNotebookMinTabWidth(const AWinControl: TWinControl): integer; override;
    class function GetTabIndexAtPos(const ATabControl: TCustomTabControl; const AClientPos: TPoint): integer; override;
    class function GetTabRect(const ATabControl: TCustomTabControl; const AIndex: Integer): TRect; override;
    class procedure SetPageIndex(const ATabControl: TCustomTabControl; const AIndex: integer); override;
    class procedure SetTabPosition(const ATabControl: TCustomTabControl; const ATabPosition: TTabPosition); override;
    class procedure ShowTabs(const ATabControl: TCustomTabControl; AShowTabs: boolean); override;
    class procedure UpdateProperties(const ATabControl: TCustomTabControl); override;
  end;

  { TGtk2WSStatusBar }

  TGtk2WSStatusBar = class(TWSStatusBar)
  protected
    class procedure SetCallbacks(const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure Update(const AStatusBar: TStatusBar); override;
    class procedure GetPreferredSize(const {%H-}AWinControl: TWinControl;
                        var {%H-}PreferredWidth, PreferredHeight: integer;
                        {%H-}WithThemeSpace: Boolean); override;

    class procedure SetSizeGrip(const AStatusBar: TStatusBar; {%H-}SizeGrip: Boolean); override;
  end;

  { TGtk2WSTabSheet }

  TGtk2WSTabSheet = class(TWSTabSheet)
  published
  end;

  { TGtk2WSPageControl }

  TGtk2WSPageControl = class(TWSPageControl)
  published
  end;

  { TGtk2WSCustomListView }

  TGtk2WSCustomListView = class(TWSCustomListView)
  private
    class procedure SetPropertyInternal(const ALV: TCustomListView; const Widgets: PTVWidgets; const AProp: TListViewProperty; const AIsSet: Boolean);
    class procedure SetNeedDefaultColumn(const ALV: TCustomListView; const AValue: Boolean);
    class procedure AddRemoveCheckboxRenderer(const ALV: TCustomListView; const WidgetInfo: PWidgetInfo; const Add: Boolean);
    class function GetViewModel(const AView: PGtkWidget): PGtkTreeModel;
  protected
    class procedure SetListCallbacks(const AScrollWidget: PGtkWidget; const Widgets: PTVWidgets; const AWidgetInfo: PWidgetInfo);
  published
    // columns
    class procedure ColumnDelete(const ALV: TCustomListView; const AIndex: Integer); override;
    class function  ColumnGetWidth(const ALV: TCustomListView; const {%H-}AIndex: Integer; const {%H-}AColumn: TListColumn): Integer; override;
    class procedure ColumnInsert(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn); override;
    class procedure ColumnMove(const ALV: TCustomListView; const AOldIndex, ANewIndex: Integer; const {%H-}AColumn: TListColumn); override;
    class procedure ColumnSetAlignment(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AAlignment: TAlignment); override;
    class procedure ColumnSetAutoSize(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AAutoSize: Boolean); override;
    class procedure ColumnSetCaption(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const ACaption: String); override;
    class procedure ColumnSetImage(const ALV: TCustomListView; const {%H-}AIndex: Integer; const {%H-}AColumn: TListColumn; const {%H-}AImageIndex: Integer); override;
    class procedure ColumnSetMaxWidth(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AMaxWidth: Integer); override;
    class procedure ColumnSetMinWidth(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AMinWidth: integer); override;
    class procedure ColumnSetWidth(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AWidth: Integer); override;
    class procedure ColumnSetVisible(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AVisible: Boolean); override;

    // items
    class procedure ItemDelete(const ALV: TCustomListView; const AIndex: Integer); override;
    class function  ItemDisplayRect(const ALV: TCustomListView; const AIndex, ASubItem: Integer; {%H-}ACode: TDisplayCode): TRect; override;
    class procedure ItemExchange(const ALV: TCustomListView; {%H-}AItem: TListItem; const AIndex1, AIndex2: Integer); override;
    class procedure ItemMove(const ALV: TCustomListView; {%H-}AItem: TListItem; const AFromIndex, AToIndex: Integer); override;
    class function  ItemGetChecked(const {%H-}ALV: TCustomListView; const {%H-}AIndex: Integer; const AItem: TListItem): Boolean; override;
    class function  ItemGetState(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const AState: TListItemState; out AIsSet: Boolean): Boolean; override; // returns True if supported
    class procedure ItemInsert(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem); override;
    class procedure ItemSetChecked(const ALV: TCustomListView; const {%H-}AIndex: Integer; const {%H-}AItem: TListItem; const {%H-}AChecked: Boolean); override;
    class procedure ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const {%H-}ASubIndex, AImageIndex: Integer); override;
    class procedure ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); override;
    class procedure ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const {%H-}ASubIndex: Integer; const {%H-}AText: String); override;
    class procedure ItemShow(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const {%H-}PartialOK: Boolean); override;
    class function  ItemGetPosition(const ALV: TCustomListView; const AIndex: Integer): TPoint; override;
    class procedure ItemUpdate(const ALV: TCustomListView; const {%H-}AIndex: Integer; const {%H-}AItem: TListItem); override;

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

    class procedure SelectAll(const ALV: TCustomListView; const AIsSet: Boolean); override;
    class procedure SetAllocBy(const ALV: TCustomListView; const {%H-}AValue: Integer); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetDefaultItemHeight(const ALV: TCustomListView; const {%H-}AValue: Integer); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetHotTrackStyles(const ALV: TCustomListView; const {%H-}AValue: TListHotTrackStyles); override;
//    class procedure SetHoverTime(const ALV: TCustomListView; const {%H-}AValue: Integer); override;
//    class procedure SetIconOptions(const ALV: TCustomListView; const AValue: TIconOptions); override;
    class procedure SetImageList(const ALV: TCustomListView; const AList: TListViewImageList; const AValue: TCustomImageListResolution); override;
    class procedure SetItemsCount(const ALV: TCustomListView; const {%H-}Avalue: Integer); override;
    class procedure SetProperty(const ALV: TCustomListView; const AProp: TListViewProperty; const AIsSet: Boolean); override;
    class procedure SetProperties(const ALV: TCustomListView; const AProps: TListViewProperties); override;
    class procedure SetScrollBars(const ALV: TCustomListView; const AValue: TScrollStyle); override;
    class procedure SetSort(const ALV: TCustomListView; const {%H-}AType: TSortType; const {%H-}AColumn: Integer;
      const {%H-}ASortDirection: TSortDirection); override;
    class procedure SetViewOrigin(const ALV: TCustomListView; const AValue: TPoint); override;
    class procedure SetViewStyle(const ALV: TCustomListView; const AValue: TViewStyle); override;
  end;

  { TGtk2WSListView }

  TGtk2WSListView = class(TWSListView)
  published
  end;

  { TGtk2WSProgressBar }

  TGtk2WSProgressBar = class(TWSProgressBar)
  private
    class procedure UpdateProgressBarText(const AProgressBar: TCustomProgressBar); virtual;
    class procedure InternalSetStyle(AProgressBar: PGtkProgressBar; AStyle: TProgressBarStyle);
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ApplyChanges(const AProgressBar: TCustomProgressBar); override;
    class procedure SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer); override;
    class procedure SetStyle(const AProgressBar: TCustomProgressBar; const NewStyle: TProgressBarStyle); override;
  end;

  { TGtk2WSCustomUpDown }

  TGtk2WSCustomUpDown = class(TWSCustomUpDown)
  published
  end;

  { TGtk2WSUpDown }

  TGtk2WSUpDown = class(TWSUpDown)
  published
  end;

  { TGtk2WSToolButton }

  TGtk2WSToolButton = class(TWSToolButton)
  published
  end;

  { TGtk2WSToolBar }

  TGtk2WSToolBar = class(TWSToolBar)
  protected
    class procedure SetCallbacks(const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TGtk2WSTrackBar }

  TGtk2WSTrackBar = class(TWSTrackBar)
  protected
    class procedure SetCallbacks(const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ApplyChanges(const ATrackBar: TCustomTrackBar); override;
    class function  GetPosition(const ATrackBar: TCustomTrackBar): integer; override;
    class procedure SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer); override;
    class procedure GetPreferredSize(const {%H-}AWinControl: TWinControl;
                        var {%H-}PreferredWidth, PreferredHeight: integer;
                        {%H-}WithThemeSpace: Boolean); override;
  end;

  { TGtk2WSCustomTreeView }

  TGtk2WSCustomTreeView = class(TWSCustomTreeView)
  published
  end;

  { TGtk2WSTreeView }

  TGtk2WSTreeView = class(TWSTreeView)
  published
  end;


implementation

uses Gtk2CellRenderer, Gtk2Extra{$IFNDEF USEORIGTREEMODEL}, Gtk2ListViewTreeModel{$ENDIF};

{$I gtk2pagecontrol.inc}

// Will be used commonly for ListViews and TreeViews
procedure GetCommonTreeViewWidgets(ATreeViewHandle: PGtkWidget;
  out TVWidgets: PTVWidgets);
var
  WidgetInfo: PWidgetInfo;
begin
  WidgetInfo := GetWidgetInfo(ATreeViewHandle);
  TVWidgets := PTVWidgets(WidgetInfo^.UserData);
end;

{$I gtk2wscustomlistview.inc}

procedure GtkWSTrackBar_Changed({%H-}AWidget: PGtkWidget; AInfo: PWidgetInfo); cdecl;
var
  Msg: TLMessage;
begin
  if AInfo^.ChangeLock > 0 then Exit;
  Msg.Msg := LM_CHANGED;
  DeliverMessage(AInfo^.LCLObject, Msg);
end;

{ TGtk2WSTrackBar }

class procedure TGtk2WSTrackBar.SetCallbacks(const AWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AWidget), TComponent(AWidgetInfo^.LCLObject));
  SignalConnect(AWidget, 'value_changed', @GtkWSTrackBar_Changed, AWidgetInfo);
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
                                                  linesize, pagesize, 0));
    if (Orientation = trHorizontal) then
      Widget := gtk_hscale_new(Adjustment)
    else
      Widget := gtk_vscale_new(Adjustment);

    gtk_range_set_inverted(PGtkRange(Widget), Reversed);
    gtk_scale_set_digits(PGtkScale(Widget), 0);
  end;
  Result := TLCLIntfHandle({%H-}PtrUInt(Widget));
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}
  WidgetInfo := CreateWidgetInfo({%H-}Pointer(Result), AWinControl, AParams);
  Set_RC_Name(AWinControl, Widget);
  SetCallbacks(Widget, WidgetInfo);
end;

class procedure TGtk2WSTrackBar.ApplyChanges(const ATrackBar: TCustomTrackBar);
const
  ValuePositionMap: array[TTrackBarScalePos] of TGtkPositionType =
  (
 { trLeft   } GTK_POS_LEFT,
 { trRight  } GTK_POS_RIGHT,
 { trTop    } GTK_POS_TOP,
 { trBottom } GTK_POS_BOTTOM
  );
var
  wHandle: HWND;
  Adjustment: PGtkAdjustment;
begin
  if not WSCheckHandleAllocated(ATrackBar, 'ApplyChanges') then
    Exit;

  with ATrackBar do
  begin
    wHandle := Handle;
    if gtk_range_get_inverted({%H-}PGtkRange(wHandle)) <> Reversed then
      gtk_range_set_inverted({%H-}PGtkRange(wHandle), Reversed);

    Adjustment := gtk_range_get_adjustment(GTK_RANGE({%H-}Pointer(wHandle)));
    // min >= max causes crash
    Adjustment^.lower := Min;
    if Min < Max then
    begin
      Adjustment^.upper := Max;
      gtk_widget_set_sensitive({%H-}PgtkWidget(wHandle), ATrackBar.Enabled);
    end
    else
    begin
      Adjustment^.upper := Min + 1;
      gtk_widget_set_sensitive({%H-}PgtkWidget(wHandle), False);
    end;
    Adjustment^.step_increment := LineSize;
    Adjustment^.page_increment := PageSize;
    Adjustment^.value := Position;
    { now do some of the more sophisticated features }
    { Hint: For some unknown reason we have to disable the draw_value first,
      otherwise it's set always to true }
    gtk_scale_set_draw_value (GTK_SCALE ({%H-}Pointer(wHandle)), false);

    if (TickStyle <> tsNone) then
    begin
      gtk_scale_set_draw_value (GTK_SCALE ({%H-}Pointer(wHandle)), true);
      gtk_scale_set_value_pos (GTK_SCALE ({%H-}Pointer(wHandle)), ValuePositionMap[ScalePos]);
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
  if not WSCheckHandleAllocated(ATrackBar, 'GetPosition') then
    Exit;

  Range := {%H-}PGtkRange(ATrackBar.Handle);
  Result := Trunc(gtk_range_get_value(Range));
end;

class procedure TGtk2WSTrackBar.SetPosition(const ATrackBar: TCustomTrackBar;
  const NewPosition: integer);
var
  Range: PGtkRange;
  WidgetInfo: PWidgetInfo;
begin
  if not WSCheckHandleAllocated(ATrackBar, 'SetPosition') then
    Exit;
  Range := {%H-}PGtkRange(ATrackBar.Handle);
  WidgetInfo := GetWidgetInfo(Range);
  // lock Range, so that no OnChange event is not fired
  Inc(WidgetInfo^.ChangeLock);
  gtk_range_set_value(Range, NewPosition);
  // unlock Range
  Dec(WidgetInfo^.ChangeLock);
end;


class procedure TGtk2WSTrackBar.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
var
  TrackBarWidget: PGtkWidget;
  Requisition: TGtkRequisition;
begin
  TrackBarWidget := {%H-}PGtkWidget(AWinControl.Handle);
  // if vertical, measure width without ticks
  if TCustomTrackBar(AWinControl).Orientation = trVertical then
    gtk_scale_set_draw_value(PGtkScale(TrackBarWidget), False);
  // set size to default
  gtk_widget_set_size_request(TrackBarWidget, -1, -1);
  // ask default size
  gtk_widget_size_request(TrackBarWidget, @Requisition);
  if TCustomTrackBar(AWinControl).Orientation = trHorizontal then
    PreferredHeight := Requisition.Height
  else
    begin
      // gtk_widget_size_request() always returns size of a HScale,
      // so we use the height for the width
      PreferredWidth := Requisition.Height;
      // restore TickStyle
      gtk_scale_set_draw_value(PGtkScale(TrackBarWidget),
                               TCustomTrackBar(AWinControl).TickStyle <> tsNone);
    end;
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
       gtk_progress_set_format_string({%H-}PGtkProgress(Handle), PChar(wText));
    end;
    gtk_progress_set_show_text({%H-}PGtkProgress(Handle), BarShowText);
  end;
end;

function ProgressPulseTimeout(data: gpointer): gboolean; cdecl;
var
  AProgressBar: PGtkProgressBar absolute data;
begin
  Result := {%H-}PtrUInt(g_object_get_data(data, 'ProgressStyle')) = 1;
  if Result then
    gtk_progress_bar_pulse(AProgressBar);
end;

procedure ProgressDestroy(data: gpointer); cdecl;
begin
  g_source_remove({%H-}PtrUInt(data));
end;

class procedure TGtk2WSProgressBar.InternalSetStyle(
  AProgressBar: PGtkProgressBar; AStyle: TProgressBarStyle);
begin
  g_object_set_data(PGObject(AProgressBar), 'ProgressStyle', {%H-}Pointer(PtrUInt(Ord(AStyle))));
  if AStyle = pbstMarquee then
  begin
    g_object_set_data_full(PGObject(AProgressBar), 'timeout',
      {%H-}Pointer(PtrUInt(g_timeout_add(100, @ProgressPulseTimeout, AProgressBar))), @ProgressDestroy);
    gtk_progress_bar_pulse(AProgressBar);
  end;
end;

class function TGtk2WSProgressBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  Widget := gtk_progress_bar_new;
  Result := TLCLIntfHandle({%H-}PtrUInt(Widget));
  WidgetInfo := CreateWidgetInfo({%H-}Pointer(Result), AWinControl, AParams);
  Set_RC_Name(AWinControl, Widget);

  GTK_WIDGET_SET_FLAGS(Widget, GTK_CAN_FOCUS);
  InternalSetStyle(PGtkProgressBar(Widget), TCustomProgressBar(AWinControl).Style);

  TGtk2WSWinControl.SetCallbacks(PGtkObject(Widget), TComponent(WidgetInfo^.LCLObject));
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
  Progress := {%H-}PGtkProgressBar(AProgressBar.Handle);

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
var
  fraction:gdouble;
begin
  if not WSCheckHandleAllocated(AProgressBar, 'TGtk2WSProgressBar.SetPosition') then
    Exit;
    
  // Gtk2 wishes the position in a floating-point value between
  // 0.0 and 1.0, and we calculate that with:
  // (Pos - Min) / (Max - Min)
  // regardless if any of them is negative the result is correct
  if ((AProgressBar.Max - AProgressBar.Min) <> 0) then
    fraction:=(NewPosition - AProgressBar.Min) / (AProgressBar.Max - AProgressBar.Min)
  else
    fraction:=0;

  gtk_progress_bar_set_fraction({%H-}PGtkProgressBar(AProgressBar.Handle), fraction);

  UpdateProgressBarText(AProgressBar);
end;

class procedure TGtk2WSProgressBar.SetStyle(
  const AProgressBar: TCustomProgressBar; const NewStyle: TProgressBarStyle);
begin
  if not WSCheckHandleAllocated(AProgressBar, 'SetStyle') then
    Exit;
  InternalSetStyle({%H-}PGtkProgressBar(AProgressBar.Handle), NewStyle);
  if NewStyle = pbstNormal then
    SetPosition(AProgressBar, AProgressBar.Position);
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
  EventBox, HBox: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  EventBox := gtk_event_box_new;
  HBox := gtk_hbox_new(False, 0);
  gtk_container_add(PGtkContainer(EventBox), HBox);
  gtk_widget_show(HBox);
  UpdateStatusBarPanels(AWinControl, HBox);
  Result := TLCLIntfHandle({%H-}PtrUInt(EventBox));
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(EventBox, dbgsName(AWinControl));
  {$ENDIF}
  WidgetInfo := CreateWidgetInfo({%H-}Pointer(Result), AWinControl, AParams);
  Set_RC_Name(AWinControl, EventBox);
  SetCallbacks(EventBox, WidgetInfo);
end;

class procedure TGtk2WSStatusBar.PanelUpdate(const AStatusBar: TStatusBar;
  PanelIndex: integer);
var
  HBox: PGtkWidget;
  StatusPanelWidget: PGtkWidget;
  BoxChild: PGtkBoxChild;
begin
  //DebugLn('TGtkWidgetSet.StatusBarPanelUpdate ',DbgS(AStatusBar),' PanelIndex=',dbgs(PanelIndex));
  HBox := {%H-}PGtkBin(AStatusBar.Handle)^.child;
  if PanelIndex >= 0 then
  begin
    // update one
    BoxChild := PGtkBoxChild(g_list_nth_data(PGtkBox(HBox)^.children, PanelIndex));
    if BoxChild = nil then
      RaiseGDBException('TGtkWidgetSet.StatusBarPanelUpdate Index out of bounds');
    StatusPanelWidget := BoxChild^.Widget;
    UpdateStatusBarPanel(AStatusBar, PanelIndex, StatusPanelWidget);
  end else
  begin
    // update all
    UpdateStatusBarPanels(AStatusBar, HBox);
  end;
end;

class procedure TGtk2WSStatusBar.SetPanelText(const AStatusBar: TStatusBar;
  PanelIndex: integer);
begin
  PanelUpdate(AStatusBar, PanelIndex);
end;

class procedure TGtk2WSStatusBar.Update(const AStatusBar: TStatusBar);
begin
  //DebugLn('TGtkWidgetSet.StatusBarUpdate ',DbgS(AStatusBar));
  UpdateStatusBarPanels(AStatusBar, {%H-}PGtkBin(AStatusBar.Handle)^.child);
end;

class procedure TGtk2WSStatusBar.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
var
  StatusBarWidget: PGtkWidget;
  Requisition: TGtkRequisition;
begin
  StatusBarWidget := GetStyleWidget(lgsStatusBar);
  // set size to default
  gtk_widget_set_size_request(StatusBarWidget, -1, -1);
  // ask default size
  gtk_widget_size_request(StatusBarWidget, @Requisition);
  PreferredHeight := Requisition.height;
  //debugln('TGtkWSStatusBar.GetPreferredSize END ',dbgs(PreferredHeight));
end;

class procedure TGtk2WSStatusBar.SetSizeGrip(const AStatusBar: TStatusBar;
  SizeGrip: Boolean);
var
  LastWidget, HBox: PGtkWidget;
begin
  if not WSCheckHandleAllocated(AStatusBar, 'SetSizeGrip') then
    Exit;
  HBox := {%H-}PGtkBin(AStatusBar.Handle)^.child;
  LastWidget := PGtkBoxChild(g_list_last(PGtkBox(HBox)^.children)^.data)^.widget;
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

  Result := TLCLIntfHandle({%H-}PtrUInt(Widget));
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

end.
