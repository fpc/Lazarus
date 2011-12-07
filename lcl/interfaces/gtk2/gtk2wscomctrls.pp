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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
{$I gtk2defines.inc}

interface

uses
  // libs
  GLib2, Gtk2, Gdk2, Gdk2pixbuf,
  // RTL, FCL, LCL
  ComCtrls, Classes, FPCAdds, LCLType, LMessages, Controls, Graphics,
  StdCtrls, Forms, LCLProc, ImgList, Math, Sysutils, InterfaceBase,
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
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
    class function GetDefaultClientRect(const AWinControl: TWinControl;
             const aLeft, aTop, aWidth, aHeight: integer; var aClientRect: TRect
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
             const aLeft, aTop, aWidth, aHeight: integer; var aClientRect: TRect
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
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;

    class procedure SetSizeGrip(const AStatusBar: TStatusBar; SizeGrip: Boolean); override;
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
    class function  ItemDisplayRect(const ALV: TCustomListView; const AIndex, ASubItem: Integer; ACode: TDisplayCode): TRect; override;
    class procedure ItemExchange(const ALV: TCustomListView; AItem: TListItem; const AIndex1, AIndex2: Integer); override;
    class procedure ItemMove(const ALV: TCustomListView; AItem: TListItem; const AFromIndex, AToIndex: Integer); override;
    class function  ItemGetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem): Boolean; override;
    class function  ItemGetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; out AIsSet: Boolean): Boolean; override; // returns True if supported
    class procedure ItemInsert(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); override;
    class procedure ItemSetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AChecked: Boolean); override;
    class procedure ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AImageIndex: Integer); override;
    class procedure ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); override;
    class procedure ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer; const AText: String); override;
    class procedure ItemShow(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean); override;
    class function  ItemGetPosition(const ALV: TCustomListView; const AIndex: Integer): TPoint; override;
    class procedure ItemUpdate(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); override;

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
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetDefaultItemHeight(const ALV: TCustomListView; const AValue: Integer); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetHotTrackStyles(const ALV: TCustomListView; const AValue: TListHotTrackStyles); override;
    class procedure SetHoverTime(const ALV: TCustomListView; const AValue: Integer); override;
//    class procedure SetIconOptions(const ALV: TCustomListView; const AValue: TIconOptions); override;
    class procedure SetImageList(const ALV: TCustomListView; const AList: TListViewImageList; const AValue: TCustomImageList); override;
    class procedure SetItemsCount(const ALV: TCustomListView; const Avalue: Integer); override;
    class procedure SetProperty(const ALV: TCustomListView; const AProp: TListViewProperty; const AIsSet: Boolean); override;
    class procedure SetProperties(const ALV: TCustomListView; const AProps: TListViewProperties); override;
    class procedure SetScrollBars(const ALV: TCustomListView; const AValue: TScrollStyle); override;
    class procedure SetSort(const ALV: TCustomListView; const AType: TSortType; const AColumn: Integer;
      const ASortDirection: TSortDirection); override;
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
    class procedure SetOrientation(const ATrackBar: TCustomTrackBar; const AOrientation: TTrackBarOrientation); override;
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
  var TVWidgets: PTVWidgets);
var
  WidgetInfo: PWidgetInfo;
begin
  WidgetInfo := GetWidgetInfo(ATreeViewHandle);
  TVWidgets := PTVWidgets(WidgetInfo^.UserData);
end;

{$I gtk2wscustomlistview.inc}

procedure GtkWSTrackBar_Changed(AWidget: PGtkWidget; AInfo: PWidgetInfo); cdecl;
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
  Result := TLCLIntfHandle(PtrUInt(Widget));
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}
  WidgetInfo := CreateWidgetInfo(Pointer(Result), AWinControl, AParams);
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
    if gtk_range_get_inverted(PGtkRange(wHandle)) <> Reversed then
      gtk_range_set_inverted(PGtkRange(wHandle), Reversed);

    Adjustment := gtk_range_get_adjustment(GTK_RANGE(Pointer(wHandle)));
    // min >= max causes crash
    Adjustment^.lower := Min;
    if Min < Max then
    begin
      Adjustment^.upper := Max;
      gtk_widget_set_sensitive(PgtkWidget(wHandle), ATrackBar.Enabled);
    end
    else
    begin
      Adjustment^.upper := Min + 1;
      gtk_widget_set_sensitive(PgtkWidget(wHandle), False);
    end;
    Adjustment^.step_increment := LineSize;
    Adjustment^.page_increment := PageSize;
    Adjustment^.value := Position;
    { now do some of the more sophisticated features }
    { Hint: For some unknown reason we have to disable the draw_value first,
      otherwise it's set always to true }
    gtk_scale_set_draw_value (GTK_SCALE (Pointer(wHandle)), false);

    if (TickStyle <> tsNone) then
    begin
      gtk_scale_set_draw_value (GTK_SCALE (Pointer(wHandle)), true);
      gtk_scale_set_value_pos (GTK_SCALE (Pointer(wHandle)), ValuePositionMap[ScalePos]);
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

  Range := PGtkRange(ATrackBar.Handle);
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
  Range := PGtkRange(ATrackBar.Handle);
  WidgetInfo := GetWidgetInfo(Range);
  // lock Range, so that no OnChange event is not fired
  Inc(WidgetInfo^.ChangeLock);
  gtk_range_set_value(Range, NewPosition);
  // unlock Range
  Dec(WidgetInfo^.ChangeLock);
end;

class procedure TGtk2WSTrackBar.SetOrientation(
  const ATrackBar: TCustomTrackBar; const AOrientation: TTrackBarOrientation);
var
  B: Boolean;
begin
  if not WSCheckHandleAllocated(ATrackBar, 'SetOrientation') then
    Exit;
  B := ATrackBar.Visible;
  if B then
    ATrackBar.Hide;
  try
    RecreateWnd(ATrackBar);
  finally
    if B then
      ATrackBar.Show;
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
       gtk_progress_set_format_string(PGtkProgress(Handle), PChar(wText));
    end;
    gtk_progress_set_show_text(PGtkProgress(Handle), BarShowText);
  end;
end;

function ProgressPulseTimeout(data: gpointer): gboolean; cdecl;
var
  AProgressBar: PGtkProgressBar absolute data;
begin
  Result := PtrUInt(g_object_get_data(data, 'ProgressStyle')) = 1;
  if Result then
    gtk_progress_bar_pulse(AProgressBar);
end;

procedure ProgressDestroy(data: gpointer); cdecl;
begin
  g_source_remove(PtrUInt(data));
end;

class procedure TGtk2WSProgressBar.InternalSetStyle(
  AProgressBar: PGtkProgressBar; AStyle: TProgressBarStyle);
begin
  g_object_set_data(PGObject(AProgressBar), 'ProgressStyle', Pointer(PtrUInt(Ord(AStyle))));
  if AStyle = pbstMarquee then
  begin
    g_object_set_data_full(PGObject(AProgressBar), 'timeout',
      Pointer(PtrUInt(g_timeout_add(100, @ProgressPulseTimeout, AProgressBar))), @ProgressDestroy);
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
  Result := TLCLIntfHandle(PtrUInt(Widget));
  WidgetInfo := CreateWidgetInfo(Pointer(Result), AWinControl, AParams);
  Set_RC_Name(AWinControl, Widget);

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

  gtk_progress_bar_set_fraction(PGtkProgressBar(AProgressBar.Handle), fraction);

  UpdateProgressBarText(AProgressBar);
end;

class procedure TGtk2WSProgressBar.SetStyle(
  const AProgressBar: TCustomProgressBar; const NewStyle: TProgressBarStyle);
begin
  if not WSCheckHandleAllocated(AProgressBar, 'SetStyle') then
    Exit;
  InternalSetStyle(PGtkProgressBar(AProgressBar.Handle), NewStyle);
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
  Result := TLCLIntfHandle(PtrUInt(EventBox));
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(EventBox, dbgsName(AWinControl));
  {$ENDIF}
  WidgetInfo := CreateWidgetInfo(Pointer(Result), AWinControl, AParams);
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
  HBox := PGtkBin(AStatusBar.Handle)^.child;
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
  UpdateStatusBarPanels(AStatusBar, PGtkBin(AStatusBar.Handle)^.child);
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
  HBox := PGtkBin(AStatusBar.Handle)^.child;
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

end.
