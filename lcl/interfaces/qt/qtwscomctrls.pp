{ $Id$}
{
 *****************************************************************************
 *                              QtWSComCtrls.pp                              * 
 *                              ---------------                              * 
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
unit QtWSComCtrls;

{$mode delphi}{$H+}

interface

uses
  // Bindings
  qt4, qtwidgets, qtprivate,
  // LCL
  Classes, ComCtrls, Controls, LCLType, Graphics, LCLProc, LCLIntf,
  // Widgetset
  WSProc, WSComCtrls, WSLCLClasses;

type

  { TQtWSStatusBar }

  TQtWSStatusBar = class(TWSStatusBar)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure Update(const AStatusBar: TStatusBar); override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
     var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
  end;

  { TQtWSTabSheet }

  TQtWSTabSheet = class(TWSTabSheet)
  private
  protected
  public
  end;

  { TQtWSPageControl }

  TQtWSPageControl = class(TWSPageControl)
  private
  protected
  public
  end;

  { TQtWSCustomListView }

  TQtWSCustomListView = class(TWSCustomListView)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
     const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  public
    class procedure ColumnInsert(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn); override;
    class function  ColumnGetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn): Integer; override;
    class procedure ColumnSetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer); override;
    class procedure ColumnSetVisible(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean); override;
    class procedure ColumnSetAutoSize(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean); override;
    
    class procedure ColumnSetMinWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer); override;
    class procedure ColumnMove(const ALV: TCustomListView; const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn); override;
    
    
    {items}
    class procedure ItemInsert(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); override;
    class procedure ItemDelete(const ALV: TCustomListView; const AIndex: Integer); override;
    class function  ItemGetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem): Boolean; override;
    class procedure ItemSetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AChecked: Boolean); override;
    class function  ItemGetPosition(const ALV: TCustomListView; const AIndex: Integer): TPoint; override;

    (*
    // Column
    class procedure ColumnDelete(const ALV: TCustomListView; const AIndex: Integer); virtual;
    class function  ColumnGetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn): Integer; virtual;

    class procedure ColumnSetMaxWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer); override;

    class procedure ColumnSetAlignment(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAlignment: TAlignment); virtual;

    class procedure ColumnSetCaption(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const ACaption: String); virtual;
    class procedure ColumnSetImage(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer); virtual;

    // Item
    class function  ItemDisplayRect(const ALV: TCustomListView; const AIndex, ASubItem: Integer; ACode: TDisplayCode): TRect; virtual;

    class function  ItemGetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; out AIsSet: Boolean): Boolean; virtual; // returns True if supported


    class procedure ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AImageIndex: Integer); virtual;
    class function ItemSetPosition(const ALV: TCustomListView; const AIndex: Integer; const ANewPosition: TPoint): Boolean; virtual;
    class procedure ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); virtual;
    class procedure ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer; const AText: String); virtual;
    class procedure ItemShow(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean); virtual;

    // LV
    class procedure BeginUpdate(const ALV: TCustomListView); virtual;
    class procedure EndUpdate(const ALV: TCustomListView); virtual;

    class function GetBoundingRect(const ALV: TCustomListView): TRect; virtual;
    class function GetDropTarget(const ALV: TCustomListView): Integer; virtual;
    class function GetFocused(const ALV: TCustomListView): Integer; virtual;
    class function GetHoverTime(const ALV: TCustomListView): Integer; virtual;
    class function GetItemAt(const ALV: TCustomListView; x,y: integer): Integer; virtual;
    class function GetSelCount(const ALV: TCustomListView): Integer; virtual;
    class function GetSelection(const ALV: TCustomListView): Integer; virtual;
    class function GetTopItem(const ALV: TCustomListView): Integer; virtual;
    class function GetViewOrigin(const ALV: TCustomListView): TPoint; virtual;
    class function GetVisibleRowCount(const ALV: TCustomListView): Integer; virtual;

    class procedure SetAllocBy(const ALV: TCustomListView; const AValue: Integer); virtual;
    class procedure SetDefaultItemHeight(const ALV: TCustomListView; const AValue: Integer); virtual;
    class procedure SetHotTrackStyles(const ALV: TCustomListView; const AValue: TListHotTrackStyles); virtual;
    class procedure SetHoverTime(const ALV: TCustomListView; const AValue: Integer); virtual;
//    class procedure SetIconOptions(const ALV: TCustomListView; const AValue: TIconOptions); virtual;
    class procedure SetImageList(const ALV: TCustomListView; const AList: TListViewImageList; const AValue: TCustomImageList); virtual;
    class procedure SetProperty(const ALV: TCustomListView; const AProp: TListViewProperty; const AIsSet: Boolean); virtual;
    class procedure SetProperties(const ALV: TCustomListView; const AProps: TListViewProperties); virtual;
    class procedure SetScrollBars(const ALV: TCustomListView; const AValue: TScrollStyle); virtual;
    class procedure SetSort(const ALV: TCustomListView; const AType: TSortType; const AColumn: Integer); virtual;
    class procedure SetViewOrigin(const ALV: TCustomListView; const AValue: TPoint); virtual;
    class procedure SetViewStyle(const ALV: TCustomListView; const Avalue: TViewStyle); virtual;
    *)
  end;

  { TQtWSListView }

  TQtWSListView = class(TWSListView)
  private
  protected
  public
  end;

  { TQtWSProgressBar }

  TQtWSProgressBar = class(TWSProgressBar)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure ApplyChanges(const AProgressBar: TCustomProgressBar); override;
    class procedure SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer); override;
  end;

  { TQtWSCustomUpDown }

  TQtWSCustomUpDown = class(TWSCustomUpDown)
  private
  protected
  public
  end;

  { TQtWSUpDown }

  TQtWSUpDown = class(TWSUpDown)
  private
  protected
  public
  end;

  { TQtWSToolButton }

  TQtWSToolButton = class(TWSToolButton)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
  end;

  { TQtWSToolBar }

  TQtWSToolBar = class(TWSToolBar)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
  end;

  { TQtWSTrackBar }

  TQtWSTrackBar = class(TWSTrackBar)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure ApplyChanges(const ATrackBar: TCustomTrackBar); override;
    class function  GetPosition(const ATrackBar: TCustomTrackBar): integer; override;
    class procedure SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer); override;
  end;

  { TQtWSCustomTreeView }

  TQtWSCustomTreeView = class(TWSCustomTreeView)
  private
  protected
  public
  end;

  { TQtWSTreeView }

  TQtWSTreeView = class(TWSTreeView)
  private
  protected
  public
  end;


implementation


{ TQtWSToolButton }

class function TQtWSToolButton.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
   QtToolButton: TQtToolButton;
   Hook: QToolButton_hookH;
   Method: TMethod;
begin
  QtToolButton := TQtToolButton.Create(AWinControl, AParams);

  Hook := QToolButton_hook_create(QtToolButton.Widget);
  TEventFilterMethod(Method) := QtToolButton.EventFilter;
  QObject_hook_hook_events(Hook, Method);
  
  Result := THandle(QtToolButton);
end;

{------------------------------------------------------------------------------
  Method: TQtWSToolButton.DestroyHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSToolButton.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtToolButton(AWinControl.Handle).Free;
  
  AWinControl.Handle := 0;
end;

{------------------------------------------------------------------------------
  Method: TQtWSToolButton.GetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSToolButton.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  Str: WideString;
begin
  TQtToolButton(AWinControl.Handle).Text(@Str);

  AText := UTF8Encode(Str);

  Result := True;
end;

{------------------------------------------------------------------------------
  Method: TQtWSToolButton.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSToolButton.SetText(const AWinControl: TWinControl; const AText: String);
var
  Str: WideString;
begin
  Str := UTF8Decode(AText);

  TQtToolButton(AWinControl.Handle).SetText(@Str);
end;

{------------------------------------------------------------------------------
  Method: TQtWSToolButton.SetColor
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSToolButton.SetColor(const AWinControl: TWinControl);
var
  QColor: TQColor;
  Color: TColor;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetColor')
  then Exit;

  if AWinControl.Color = CLR_INVALID then exit;

  // Get the color numeric value (system colors are mapped to numeric colors depending on the widget style)
  Color:=ColorToRGB(AWinControl.Color);

  // Fill QColor
  QColor_setRgb(@QColor,Red(Color),Green(Color),Blue(Color));

  // Set color of the widget to QColor
  TQtAbstractButton(AWinControl.Handle).SetColor(@QColor);
end;

{ TQtWSToolBar }

class function TQtWSToolBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
   QtToolBar: TQtToolBar;
   Hook: QToolBar_hookH;
   Method: TMethod;
begin
  QtToolBar := TQtToolBar.Create(AWinControl, AParams);

  Hook := QToolBar_hook_create(QtToolBar.Widget);
  TEventFilterMethod(Method) := QtToolBar.EventFilter;
  QObject_hook_hook_events(Hook, Method);
     
  Result := THandle(QtToolBar);
end;

{------------------------------------------------------------------------------
  Method: TQtWSToolBar.DestroyHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSToolBar.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtToolBar(AWinControl.Handle).Free;
  AWinControl.Handle := 0;
end;


{------------------------------------------------------------------------------
  Method: TQtWSToolBar.SetColor
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSToolBar.SetColor(const AWinControl: TWinControl);
var
  QColor: TQColor;
  Color: TColor;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetColor')
  then Exit;

  if AWinControl.Color = CLR_INVALID then exit;

  // Get the color numeric value (system colors are mapped to numeric colors depending on the widget style)
  Color:=ColorToRGB(AWinControl.Color);

  // Fill QColor
  QColor_setRgb(@QColor,Red(Color),Green(Color),Blue(Color));

  // Set color of the widget to QColor
  TQtToolBar(AWinControl.Handle).SetColor(@QColor);
end;

{ TQtWSTrackBar }

class function TQtWSTrackBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  QtTrackBar: TQtTrackBar;
  Method: TMethod;
  Hook : QSlider_hookH;
begin
  QtTrackBar := TQtTrackBar.Create(AWinControl, AParams);

  Hook := QSlider_hook_create(QtTrackBar.Widget);
  TEventFilterMethod(Method) := QtTrackBar.EventFilter;
  QObject_hook_hook_events(Hook, Method);

  QAbstractSlider_rangeChanged_Event(Method) := QtTrackbar.SlotRangeChanged;
  QAbstractSlider_hook_hook_rangeChanged(QAbstractSlider_hook_create(QtTrackBar.Widget), Method);

  QAbstractSlider_sliderMoved_Event(Method) := QtTrackBar.SlotSliderMoved;
  QAbstractSlider_hook_hook_sliderMoved(QAbstractSlider_hook_create(QtTrackBar.Widget), Method);

  QAbstractSlider_sliderPressed_Event(Method) := QtTrackBar.SlotSliderPressed;
  QAbstractSlider_hook_hook_sliderPressed(QAbstractSlider_hook_create(QtTrackBar.Widget), Method);

  QAbstractSlider_sliderReleased_Event(Method) := QtTrackBar.SlotSliderReleased;
  QAbstractSlider_hook_hook_sliderReleased(QAbstractSlider_hook_create(QtTrackBar.Widget), Method);

  QAbstractSlider_valueChanged_Event(Method) := QtTrackBar.SlotValueChanged;
  QAbstractSlider_hook_hook_valueChanged(QAbstractSlider_hook_create(QtTrackBar.Widget), Method);

  Result := THandle(QtTrackBar);
end;

class procedure TQtWSTrackBar.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtTrackBar(AWinControl.Handle).Free;
  AWinControl.Handle := 0;
end;

class procedure TQtWSTrackBar.ApplyChanges(const ATrackBar: TCustomTrackBar);
var
  QtTrackBar: TQtTrackBar;
begin

  QtTrackBar := TQtTrackBar(ATrackBar.Handle);

  QtTrackBar.setRange(ATrackBar.Min, ATrackBar.Max);

  case ATrackBar.TickMarks of
    tmBoth:QtTrackBar.SetTickPosition(QSliderTicksBothSides);
    tmTopLeft:QtTrackBar.SetTickPosition(QSliderTicksAbove);
    tmBottomRight:QtTrackBar.SetTickPosition(QSliderTicksBelow);
  end;

  if QAbstractSlider_pageStep(QAbstractSliderH(QtTrackBar.Widget)) <> ATrackBar.PageSize then
    QtTrackBar.setPageStep(ATrackBar.PageSize);
  if QSlider_tickInterval(QSliderH(QtTrackBar.Widget)) <> ATrackBar.Frequency then
    QtTrackBar.setTickInterval(ATrackBar.Frequency);
  if QAbstractSlider_value(QAbstractSliderH(QtTrackBar.Widget)) <> ATrackBar.Position then
    QtTrackBar.setSliderPosition(ATrackBar.Position);

  case ATrackBar.Orientation of
    trVertical:
    begin
      QtTrackBar.Hide;
      QtTrackBar.setOrientation(QtVertical);
      QtTrackBar.setInvertedAppereance(False);
      QtTrackBar.setInvertedControls(False);
      QtTrackBar.Show;
    end;
    else {trHorizontal}
    begin
      QtTrackBar.Hide;
      QtTrackBar.setOrientation(QtHorizontal);
      QtTrackBar.setInvertedAppereance(False);
      QtTrackBar.setInvertedControls(False);
      QtTrackBar.Show;
    end;
  end;
end;

class function  TQtWSTrackBar.GetPosition(const ATrackBar: TCustomTrackBar): integer;
var
  QtTrackBar: TQtTrackBar;
begin
  QtTrackBar := TQtTrackBar(ATrackBar.Handle);
  Result := QAbstractSlider_value(QAbstractSliderH(QtTrackBar.Widget));
end;

class procedure TQtWSTrackBar.SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer);
var
  QtTrackBar: TQtTrackBar;
begin
  QtTrackBar := TQtTrackBar(ATrackBar.Handle);
  QtTrackBar.setValue(NewPosition);
end;

{ TQtWSProgressBar }

class function TQtWSProgressBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  QtProgressBar: TQtProgressBar;
{  Method: TMethod;
  Hook : QObject_hookH;}
begin
  QtProgressBar := TQtProgressBar.Create(AWinControl, AParams);

  // Various Events

{  Hook := QObject_hook_create(QtWidget.Widget);

  TEventFilterMethod(Method) := QtWidget.EventFilter;

  QObject_hook_hook_events(Hook, Method);}

  Result := THandle(QtProgressBar);
end;

class procedure TQtWSProgressBar.DestroyHandle(const AWinControl: TWinControl);
begin
//  TQtWidget(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
end;

class procedure TQtWSProgressBar.ApplyChanges(const AProgressBar: TCustomProgressBar);
var
  QtProgressBar: TQtProgressBar;
begin
  QtProgressBar := TQtProgressBar(AProgressBar.Handle);

//  if AProgressBar.Smooth then
//    gtk_progress_bar_set_bar_style (GTK_PROGRESS_BAR(Pointer(Pointer(wHandle))),
//                                         GTK_PROGRESS_CONTINUOUS)
//   else gtk_progress_bar_set_bar_style (GTK_PROGRESS_BAR(Pointer(Pointer(wHandle))),
//                                         GTK_PROGRESS_DISCRETE);

  case AProgressBar.Orientation of
    pbVertical:
    begin
      QtProgressBar.setOrientation(QtVertical);
      QtProgressBar.setInvertedAppearance(False);
    end;
    pbRightToLeft:
    begin
      QtProgressBar.setOrientation(QtHorizontal);
      QtProgressBar.setInvertedAppearance(True);
    end;
    pbTopDown:
    begin
      QtProgressBar.setOrientation(QtVertical);
      QtProgressBar.setInvertedAppearance(True);
    end;
  else { pbHorizontal is default }
    QtProgressBar.setOrientation(QtHorizontal);
    QtProgressBar.setInvertedAppearance(False);
  end;

  if AProgressBar.BarShowText then
  begin
//       gtk_progress_set_format_string (GTK_PROGRESS(Pointer(Pointer(wHandle))),
//                                       '%v from [%l-%u] (=%p%%)');

    QtProgressBar.setTextVisible(True);
  end
  else QtProgressBar.setTextVisible(False);

  // The position, minumum and maximum values
  QtProgressBar.setValue(AProgressBar.Position);

  QtProgressBar.setRange(AProgressBar.Min, AProgressBar.Max);
end;

class procedure TQtWSProgressBar.SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer);
begin
  TQtProgressBar(AProgressBar.Handle).setValue(NewPosition);
end;

{ TQtWSStatusBar }

class function TQtWSStatusBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  QtStatusBar: TQtStatusBar;
  Method: TMethod;
  Hook : QStatusBar_hookH;
begin
  QtStatusBar := TQtStatusBar.Create(AWinControl, AParams);

  // Various Events

  Hook := QStatusBar_hook_create(QtStatusBar.Widget);

  TEventFilterMethod(Method) := QtStatusBar.EventFilter;

  QObject_hook_hook_events(Hook, Method);

  // Return handle

  Result := THandle(QtStatusBar);
end;

class procedure TQtWSStatusBar.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtWidget(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
end;

class procedure TQtWSStatusBar.PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer);
begin

end;

class procedure TQtWSStatusBar.SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer);
var
  Text: Widestring;
begin
  Text := UTF8Decode(AStatusBar.SimpleText);
  TQtStatusBar(AStatusBar.Handle).showMessage(@Text);
end;

class procedure TQtWSStatusBar.Update(const AStatusBar: TStatusBar);
begin

end;

class procedure TQtWSStatusBar.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin

end;

{ TQtWSCustomListView }

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomListView.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  QtTreeWidget: TQtTreeWidget;
  Method: TMethod;
  Hook : QTreeWidget_hookH;
begin
  QtTreeWidget := TQtTreeWidget.Create(AWinControl, AParams);

  // Various Events

  Hook := QTreeWidget_hook_create(QtTreeWidget.Widget);

  TEventFilterMethod(Method) := QtTreeWidget.EventFilter;

  QObject_hook_hook_events(Hook, Method);

  // OnSelectionChange event
  (*
  QListWidget_currentItemChanged_Event(Method) := QtListWidget.SlotSelectionChange;
  QListWidget_hook_hook_currentItemChanged(QListWidget_hook_create(QtListWidget.Widget), Method);

  QListWidget_itemDoubleClicked_Event(Method) := QtListWidget.SignalItemDoubleClicked;
  QListWidget_hook_hook_ItemDoubleClicked(QListWidget_hook_create(QtListWidget.Widget), Method);

  QListWidget_itemClicked_Event(Method) := QtListWidget.SignalItemClicked;
  QListWidget_hook_hook_ItemClicked(QListWidget_hook_create(QtListWidget.Widget), Method);
    *)
  // QListWidget_itemClicked_Event(Method;


  Result := THandle(QtTreeWidget);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.DestroyHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtTreeWidget(AWinControl.Handle).Free;
  AWinControl.Handle := 0;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnInsert
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ColumnInsert(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn);
var
  i: Integer;
  TW: QTreeWidgetH;
  IH: QTreeWidgetItemH;
  HV: QHeaderViewH;
  AIM: QAbstractItemModelH;
begin
  TW := QTreeWidgetH(TQtTreeWidget(ALV.Handle).Widget);
  i := QTreeWidget_columnCount(TW);

  if i <> TListView(ALV).Columns.Count then
  QTreeWidget_setColumnCount(TW, TListView(ALV).Columns.Count);

  HV := QTreeView_header(TW);

  QHeaderView_setClickable(HV, True);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnGetWidth
  Params:  None
  Returns: Integer
 ------------------------------------------------------------------------------}
class function  TQtWSCustomListView.ColumnGetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn): Integer;
var
  TW: QTreeWidgetH;
begin
  TW := QTreeWidgetH(TQtTreeWidget(ALV.Handle).Widget);
  Result := QTreeView_columnWidth(QTreeViewH(TW), AIndex);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnMove
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ColumnMove(const ALV: TCustomListView; const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn);
var
  TW: QTreeWidgetH;
  HV: QHeaderViewH;
begin
  TW := QTreeWidgetH(TQtTreeWidget(ALV.Handle).Widget);
  HV := QTreeView_header(TW);
  QHeaderView_moveSection(HV, AOldIndex, ANewIndex);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnSetAutoSize
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ColumnSetAutoSize(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean);
var
  TW: QTreeWidgetH;
  HV: QHeaderViewH;
begin
  TW := QTreeWidgetH(TQtTreeWidget(ALV.Handle).Widget);
  HV := QTreeView_header(TW);
  if AAutoSize then
  QHeaderView_setResizeMode(HV, AIndex, QHeaderViewResizeToContents)
  else
  QHeaderView_setResizeMode(HV, AIndex, QHeaderViewInteractive);
  
end;
    
{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnSetMinWidth
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ColumnSetMinWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer);
var
  TW: QTreeWidgetH;
  HV: QHeaderViewH;
begin
  TW := QTreeWidgetH(TQtTreeWidget(ALV.Handle).Widget);
  HV := QTreeView_header(TW);
  QHeaderView_setMinimumSectionSize(HV, AMinWidth);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnSetWidth
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ColumnSetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer);
var
  TW: QTreeWidgetH;
begin
  TW := QTreeWidgetH(TQtTreeWidget(ALV.Handle).Widget);
  QTreeView_setColumnWidth(QTreeViewH(TW), AIndex, AWidth);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnSetVisible
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ColumnSetVisible(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean);
var
  TW: QTreeWidgetH;
begin
  TW := QTreeWidgetH(TQtTreeWidget(ALV.Handle).Widget);
  QTreeView_setColumnHidden(QTreeViewH(TW), AIndex, not AVisible);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemDelete
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ItemDelete(const ALV: TCustomListView; const AIndex: Integer);
var
  TW: QTreeWidgetH;
  TWI: QTreeWidgetItemH;
begin
  TW := QTreeWidgetH(TQtTreeWidget(ALV.Handle).Widget);
  QTreeWidget_takeTopLevelItem(TW, AIndex);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemGetChecked
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function  TQtWSCustomListView.ItemGetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem): Boolean;
var
  TW: QTreeWidgetH;
  TWI: QTreeWidgetItemH;
  AState: QtCheckState;
begin
  TW := QTreeWidgetH(TQtTreeWidget(ALV.Handle).Widget);
  TWI := QTreeWidget_topLevelItem(TW, AIndex);
  AState := QTreeWidgetItem_checkState(TWI, 0);
  if AState = QtChecked then
  Result := True
  else
  Result := False;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemGetPosition
  Params:  None
  Returns: TPoint
 ------------------------------------------------------------------------------}
class function  TQtWSCustomListView.ItemGetPosition(const ALV: TCustomListView; const AIndex: Integer): TPoint;
var
  TW: QTreeWidgetH;
  TWI: QTreeWidgetItemH;
  R: TRect;
begin
  TW := QTreeWidgetH(TQtTreeWidget(ALV.Handle).Widget);
  TWI := QTreeWidget_topLevelItem(TW, AIndex);
  QTreeWidget_visualItemRect(TW, @R, TWI);

  Result.X := R.Left;
  Result.Y := R.Top;
  
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemSetChecked
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ItemSetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AChecked: Boolean);
var
  TW: QTreeWidgetH;
  TWI: QTreeWidgetItemH;
  AState: QtCheckState;
begin
  TW := QTreeWidgetH(TQtTreeWidget(ALV.Handle).Widget);
  TWI := QTreeWidget_topLevelItem(TW, AIndex);
  if AChecked then
  QTreeWidgetItem_setCheckState(TWI, 0, QtChecked)
  else
  QTreeWidgetItem_setCheckState(TWI, 0, QtUnChecked);
end;


{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemInsert
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ItemInsert(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem);
var
  TW: QTreeWidgetH;
  TWI: QTreeWidgetItemH;
  Str: WideString;
  i: Integer;
begin
  TW := QTreeWidgetH(TQtTreeWidget(ALV.Handle).Widget);

  TWI := QTreeWidgetItem_create(TW, 0);
  Str := UTF8Decode(AItem.Caption);
  
  if ALV.CheckBoxes then
  begin
    if AItem.Checked then
    QTreeWidgetItem_setCheckState(TWI, 0, QtChecked)
    else
    QTreeWidgetItem_setCheckState(TWI, 0, QtUnchecked);
  end;
  
  QTreeWidgetItem_setText(TWI, 0, @Str);
  for i := 0 to AItem.SubItems.Count - 1 do
  begin
    Str := UTF8Decode(AItem.Subitems.Strings[i]);
    QTreeWidgetItem_setText(TWI, i+1, @Str);
  end;
  
  QTreeWidget_insertTopLevelItem(TW, AIndex, TWI);
end;



initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TStatusBar, TQtWSStatusBar);
//  RegisterWSComponent(TCustomTabSheet, TQtWSTabSheet);
//  RegisterWSComponent(TCustomPageControl, TQtWSPageControl);
  RegisterWSComponent(TCustomListView, TQtWSCustomListView);
//  RegisterWSComponent(TCustomListView, TQtWSListView);
  RegisterWSComponent(TCustomProgressBar, TQtWSProgressBar);
//  RegisterWSComponent(TCustomUpDown, TQtWSCustomUpDown);
//  RegisterWSComponent(TCustomUpDown, TQtWSUpDown);
  RegisterWSComponent(TToolButton, TQtWSToolButton);
  RegisterWSComponent(TToolBar, TQtWSToolBar);
//  RegisterWSComponent(TCustomToolButton, TQtWSToolButton);
//  RegisterWSComponent(TCustomToolBar, TQtWSToolBar);
  RegisterWSComponent(TCustomTrackBar, TQtWSTrackBar);
//  RegisterWSComponent(TCustomTreeView, TQtWSCustomTreeView);
//  RegisterWSComponent(TCustomTreeView, TQtWSTreeView);
////////////////////////////////////////////////////
end.
