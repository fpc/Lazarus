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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
  StdCtrls, LCLProc, ImgList, Math,
  // widgetset
  InterfaceBase, WSComCtrls, WSLCLClasses, WSProc, WSControls,
  // interface
  GtkDef, GtkExtra, GtkWSPrivate;

type
  { TGtkWSStatusBar }

  TGtkWSStatusBar = class(TWSStatusBar)
  protected
    class procedure SetCallbacks(const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure Update(const AStatusBar: TStatusBar); override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;
  end;

  { TGtkWSTabSheet }

  TGtkWSTabSheet = class(TWSTabSheet)
  published
  end;

  { TGtkWSPageControl }

  TGtkWSPageControl = class(TWSPageControl)
  published
  end;

  { TGtkWSCustomListView }

  TGtkWSCustomListView = class(TWSCustomListView)
{$IFDEF GTK1}
  private
    class procedure ItemChangeInternal(const ACListWidget: PGtkCList; const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem);
    class procedure SetPropertyInternal(const ACListWidget: PGtkCList; const AInfo: PWidgetInfo; const AProp: TListViewProperty; const AIsSet: Boolean);
  protected
    class procedure SetCallbacks(const AScrollWidget, AListWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
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
    class function  ItemGetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; out AIsSet: Boolean): Boolean; override; // returns True if supported
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
{$ENDIF}
  end;

  { TGtkWSListView }

  TGtkWSListView = class(TWSListView)
  published
  end;

  { TGtkWSProgressBar }
  {$IFDEF GTK1}
  TGtkWSProgressBar = class(TWSProgressBar)
  private
  protected
    class procedure SetCallbacks(const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ApplyChanges(const AProgressBar: TCustomProgressBar); override;
    class procedure SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer); override;
  end;
  {$ENDIF}

  { TGtkWSCustomUpDown }

  TGtkWSCustomUpDown = class(TWSCustomUpDown)
  published
  end;

  { TGtkWSUpDown }

  TGtkWSUpDown = class(TWSUpDown)
  published
  end;

  { TGtkWSToolButton }

  TGtkWSToolButton = class(TWSToolButton)
  published
  end;

  { TGtkWSToolBar }

  TGtkWSToolBar = class(TWSToolBar)
  protected
    class procedure SetCallbacks(const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
{$ifdef OldToolbar}
    class function  GetButtonCount(const AToolBar: TToolBar): integer; override;
    class procedure InsertToolButton(const AToolBar: TToolbar; const AControl: TControl); override;
    class procedure DeleteToolButton(const AToolBar: TToolbar; const AControl: TControl); override;
{$endif}    
  end;

  { TGtkWSTrackBar }

  TGtkWSTrackBar = class(TWSTrackBar)
  protected
    class procedure SetCallbacks(const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ApplyChanges(const ATrackBar: TCustomTrackBar); override;
    class function  GetPosition(const ATrackBar: TCustomTrackBar): integer; override;
    class procedure SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer); override;
  end;

  { TGtkWSCustomTreeView }

  TGtkWSCustomTreeView = class(TWSCustomTreeView)
  published
  end;

  { TGtkWSTreeView }

  TGtkWSTreeView = class(TWSTreeView)
  published
  end;


implementation

uses
  SysUtils,
  GtkProc, GtkInt, GtkGlobals,
  GtkWSControls;

const
  DEFAULT_IMAGE_SPACING = 3;


{$IFDEF GTK1}
{$I gtkwscustomlistview.inc }
{$ENDIF}

{ TGtkWSProgressBar }

{$IFDEF GTK1}
class procedure TGtkWSProgressBar.SetCallbacks(const AWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AWidget), TComponent(AWidgetInfo^.LCLObject));
end;

class function TGtkWSProgressBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Adjustment: PGtkAdjustment;
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  with TCustomProgressBar(AWinControl) do
  begin
     // Create a GtkAdjustment object to hold the range of the progress bar
     Adjustment := PGtkAdjustment(gtk_adjustment_new(Position, Min, Max, 0, 0, 0));
     // Create the GtkProgressBar using the adjustment
     Widget := gtk_progress_bar_new_with_adjustment(Adjustment);
  end;
  Result := TLCLIntfHandle(PtrUInt(Widget));
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}
  WidgetInfo := CreateWidgetInfo(Pointer(Result), AWinControl, AParams);
  Set_RC_Name(AWinControl, Widget);
  SetCallbacks(Widget, WidgetInfo);
end;

class procedure TGtkWSProgressBar.ApplyChanges(const AProgressBar: TCustomProgressBar);
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

class procedure TGtkWSProgressBar.SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer);
begin
  gtk_progress_set_value(GTK_PROGRESS(Pointer(AProgressBar.Handle)), NewPosition);
end;
{$ENDIF GTK1}

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

class procedure TGtkWSTrackBar.SetCallbacks(const AWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AWidget), TComponent(AWidgetInfo^.LCLObject));
  TGtkWidgetset(WidgetSet).SetCallback(LM_CHANGED, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
end;

class function TGtkWSTrackBar.CreateHandle(const AWinControl: TWinControl;
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

class procedure TGtkWSTrackBar.ApplyChanges(const ATrackBar: TCustomTrackBar);
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

class function  TGtkWSTrackBar.GetPosition(const ATrackBar: TCustomTrackBar): integer;
begin
  if ATrackBar.HandleAllocated then 
  begin
    Result := RoundToInt(gtk_range_get_adjustment(
      GTK_RANGE(Pointer(ATrackBar.Handle)))^.value);
  end else
    Result := 0;
end;

class procedure TGtkWSTrackBar.SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer);
var
  Handle: HWND;
begin
  Handle := ATrackBar.Handle;
  gtk_range_get_adjustment(GTK_RANGE(Pointer(Handle)))^.value := NewPosition;
  g_signal_emit_by_name(PGtkObject(
    gtk_range_get_adjustment(GTK_RANGE(Pointer(Handle)))), 'value_changed');
end;

{ TGtkWSStatusBar }

class procedure TGtkWSStatusBar.SetCallbacks(const AWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AWidget), TComponent(AWidgetInfo^.LCLObject));
end;

class function TGtkWSStatusBar.CreateHandle(const AWinControl: TWinControl;
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

class procedure TGtkWSStatusBar.PanelUpdate(const AStatusBar: TStatusBar;
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

class procedure TGtkWSStatusBar.SetPanelText(const AStatusBar: TStatusBar;
  PanelIndex: integer);
begin
  PanelUpdate(AStatusBar,PanelIndex);
end;

class procedure TGtkWSStatusBar.Update(const AStatusBar: TStatusBar);
begin
  //DebugLn('TGtkWidgetSet.StatusBarUpdate ',DbgS(AStatusBar));
  UpdateStatusBarPanels(AStatusBar,PGtkWidget(AStatusBar.Handle));
end;

class procedure TGtkWSStatusBar.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
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

{ TGtkWSToolBar }

class procedure TGtkWSToolBar.SetCallbacks(const AWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AWidget), TComponent(AWidgetInfo^.LCLObject));
end;

class function TGtkWSToolBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Widget, ClientWidget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  {$ifdef gtk1}
  Widget := gtk_toolbar_new();
  gtk_toolbar_set_space_size(PGTKToolbar(Widget), 0);
  gtk_toolbar_set_space_style(PGTKToolbar(Widget), GTK_TOOLBAR_SPACE_EMPTY);
  ClientWidget := gtk_fixed_new();
  gtk_toolbar_insert_widget(PGTKToolbar(Widget), ClientWidget, nil, nil, 0);
  {$else}
  Widget:= gtk_hbox_new(false,0);
  ClientWidget := CreateFixedClientWidget;
  gtk_container_add(GTK_CONTAINER(Widget), ClientWidget);
  {$endif}

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
