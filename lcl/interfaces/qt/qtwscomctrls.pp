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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit QtWSComCtrls;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  // Bindings
  qt4,
  qtwidgets, qtprivate, qtobjects, qtproc, qtwscontrols,
  // LCL
  SysUtils, Classes, Types, ComCtrls, Controls, LCLType, Graphics, StdCtrls,
  LCLProc, LCLIntf, Forms,
  // Widgetset
  WSProc, WSComCtrls, WSLCLClasses;

type

  { TQtWSStatusBar }

  TQtWSStatusBar = class(TWSStatusBar)
  protected
    class procedure ClearPanels(const Widget: TQtStatusBar);
    class procedure RecreatePanels(const AStatusBar: TStatusBar; const Widget: TQtStatusBar);
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure SetSizeGrip(const AStatusBar: TStatusBar; SizeGrip: Boolean); override;
    class procedure Update(const AStatusBar: TStatusBar); override;
  end;

  { TQtWSTabSheet }

  TQtWSTabSheet = class(TWSTabSheet)
  published
  end;

  { TQtWSPageControl }

  TQtWSPageControl = class(TWSPageControl)
  published
  end;

  { TQtWSCustomListView }

  TQtWSCustomListView = class(TWSCustomListView)
  protected
    class function IsIconView(const AList: TCustomListView): boolean;
  published
    class function CreateHandle(const AWinControl: TWinControl;
     const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ColumnDelete(const ALV: TCustomListView; const AIndex: Integer); override;
    class procedure ColumnInsert(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn); override;
    class function  ColumnGetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn): Integer; override;
    class procedure ColumnSetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer); override;
    class procedure ColumnSetVisible(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean); override;
    class procedure ColumnSetAlignment(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAlignment: TAlignment); override;
    class procedure ColumnSetAutoSize(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean); override;
    class procedure ColumnSetCaption(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const ACaption: String); override;
    class procedure ColumnSetImage(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer); override;

    class procedure ColumnSetMinWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer); override;
    class procedure ColumnMove(const ALV: TCustomListView; const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn); override;


    {items}
    class procedure ItemInsert(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); override;
    class procedure ItemDelete(const ALV: TCustomListView; const AIndex: Integer); override;
    class function  ItemGetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem): Boolean; override;
    class procedure ItemSetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AChecked: Boolean); override;
    class function  ItemGetPosition(const ALV: TCustomListView; const AIndex: Integer): TPoint; override;
    class function  ItemGetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; out AIsSet: Boolean): Boolean; override; // returns True if supported
    class procedure ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AImageIndex: Integer); override;
    class procedure ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); override;
    class procedure ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer; const AText: String); override;
    class procedure ItemShow(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean); override;
    class function  ItemDisplayRect(const ALV: TCustomListView; const AIndex, ASubItem: Integer; ACode: TDisplayCode): TRect; override;

    {parent}
    class procedure BeginUpdate(const ALV: TCustomListView); override;
    class procedure EndUpdate(const ALV: TCustomListView); override;

    class function GetFocused(const ALV: TCustomListView): Integer; override;
    class function GetItemAt(const ALV: TCustomListView; x,y: integer): Integer; override;
    class function GetSelCount(const ALV: TCustomListView): Integer; override;
    class function GetSelection(const ALV: TCustomListView): Integer; override;
    class function GetTopItem(const ALV: TCustomListView): Integer; override;
    class procedure SetSort(const ALV: TCustomListView; const AType: TSortType; const AColumn: Integer); override;

    class function GetBoundingRect(const ALV: TCustomListView): TRect; override;
    class function GetViewOrigin(const ALV: TCustomListView): TPoint; override;
    class function GetVisibleRowCount(const ALV: TCustomListView): Integer; override;

    class procedure SetAllocBy(const ALV: TCustomListView; const AValue: Integer); override;
    class procedure SetIconArrangement(const ALV: TCustomListView; const AValue: TIconArrangement); override;
    class procedure SetItemsCount(const ALV: TCustomListView; const Avalue: Integer); override;
    class procedure SetOwnerData(const ALV: TCustomListView; const AValue: Boolean); override;

    class procedure SetProperty(const ALV: TCustomListView; const AProp: TListViewProperty; const AIsSet: Boolean); override;
    class procedure SetProperties(const ALV: TCustomListView; const AProps: TListViewProperties); override;

    class procedure SetScrollBars(const ALV: TCustomListView; const AValue: TScrollStyle); override;
    class procedure SetViewStyle(const ALV: TCustomListView; const Avalue: TViewStyle); override;

    (*
    // Column

    class procedure ColumnSetMaxWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer); override;


    // Item
    class function ItemSetPosition(const ALV: TCustomListView; const AIndex: Integer; const ANewPosition: TPoint): Boolean; virtual;

    // LV

    class function GetDropTarget(const ALV: TCustomListView): Integer; virtual;

    class function GetHoverTime(const ALV: TCustomListView): Integer; virtual;
    class function GetViewOrigin(const ALV: TCustomListView): TPoint; virtual;

    class procedure SetDefaultItemHeight(const ALV: TCustomListView; const AValue: Integer); virtual;
    class procedure SetHotTrackStyles(const ALV: TCustomListView; const AValue: TListHotTrackStyles); virtual;
    class procedure SetHoverTime(const ALV: TCustomListView; const AValue: Integer); virtual;
    class procedure SetImageList(const ALV: TCustomListView; const AList: TListViewImageList; const AValue: TCustomImageList); virtual;

    class procedure SetViewOrigin(const ALV: TCustomListView; const AValue: TPoint); virtual;
    *)
  end;

  { TQtWSListView }

  TQtWSListView = class(TWSListView)
  published
  end;

  { TQtWSProgressBar }

  TQtWSProgressBar = class(TWSProgressBar)
  protected
    class procedure SetRangeStyle(AProgressBar: TQtProgressBar;
      AStyle: TProgressBarStyle; AMin, AMax: Integer; const AIsDesign: Boolean);
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ApplyChanges(const AProgressBar: TCustomProgressBar); override;
    class procedure SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer); override;
    class procedure SetStyle(const AProgressBar: TCustomProgressBar; const NewStyle: TProgressBarStyle); override;
  end;

  { TQtWSCustomUpDown }

  TQtWSCustomUpDown = class(TWSCustomUpDown)
  published
  end;

  { TQtWSUpDown }

  TQtWSUpDown = class(TWSUpDown)
  published
  end;

  { TQtWSToolButton }

  TQtWSToolButton = class(TWSToolButton)
  published
  end;

  { TQtWSToolBar }

  TQtWSToolBar = class(TWSToolBar)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TQtWSTrackBar }

  TQtWSTrackBar = class(TWSTrackBar)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ApplyChanges(const ATrackBar: TCustomTrackBar); override;
    class function  GetPosition(const ATrackBar: TCustomTrackBar): integer; override;
    class procedure SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer); override;
  end;

  { TQtWSCustomTreeView }

  TQtWSCustomTreeView = class(TWSCustomTreeView)
  published
  end;

  { TQtWSTreeView }

  TQtWSTreeView = class(TWSTreeView)
  published
  end;


implementation

const
  TickMarkToQtSliderTickPositionMap: array[TTickMark] of QSliderTickPosition =
  (
{tmBottomRight} QSliderTicksBelow,
{tmTopLeft    } QSliderTicksAbove,
{tmBoth       } QSliderTicksBothSides
  );

  TrackBarOrientationToQtOrientationMap: array[TTrackBarOrientation] of QtOrientation =
  (
{trHorizontal} QtHorizontal,
{trVertical  } QtVertical
  );

  AlignmentToQtAlignmentMap: array[TAlignment] of QtAlignment =
  (
{taLeftJustify } QtAlignLeft,
{taRightJustify} QtAlignRight,
{taCenter      } QtAlignCenter
  );

  IconArngToQListFlow: array[TIconArrangement] of QListViewFlow =
  (
{iaTop} QListViewTopToBottom,
{iaLeft} QListViewLeftToRight
  );

{ TQtWSToolBar }

class function TQtWSToolBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  QtToolBar: TQtCustomControl;
begin
  {$note TToolBar implementation under LCL is wrong. TToolBar isn't
  TCustomControl but TWinControl.
  To avoid theoretical crashes we use TQtCustomControl here, but indeed it
  should be TQtWidget - so no viewport.}
  QtToolBar := TQtCustomControl.Create(AWinControl, AParams);
  QtToolBar.setFrameShape(QFrameNoFrame);
  QtToolBar.viewportNeeded;
  QtToolBar.setFocusPolicy(QtTabFocus);
  QtToolBar.AttachEvents;
  Result := TLCLIntfHandle(QtToolBar);
end;

{ TQtWSTrackBar }

class function TQtWSTrackBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  QtTrackBar: TQtTrackBar;
begin
  QtTrackBar := TQtTrackBar.Create(AWinControl, AParams);
  QtTrackBar.AttachEvents;

  Result := TLCLIntfHandle(QtTrackBar);
end;

class procedure TQtWSTrackBar.ApplyChanges(const ATrackBar: TCustomTrackBar);
var
  QtTrackBar: TQtTrackBar;

  function TrackBarReversed: Boolean;
  begin
    Result :=
      ((ATrackBar.Orientation = trHorizontal) and
      (QtTrackbar.getInvertedAppereance <> ATrackBar.Reversed))
      or
      ((ATrackBar.Orientation = trVertical) and
      (QtTrackbar.getInvertedAppereance <> not ATrackBar.Reversed))

  end;
begin
  QtTrackBar := TQtTrackBar(ATrackBar.Handle);

  QtTrackBar.BeginUpdate;
  try
    QtTrackBar.setRange(ATrackBar.Min, ATrackBar.Max);

    if ATrackBar.TickStyle = tsNone then
      QtTrackBar.SetTickPosition(QSliderNoTicks)
    else
      QtTrackBar.SetTickPosition(TickMarkToQtSliderTickPositionMap[ATrackBar.TickMarks]);

    if QtTrackBar.getPageStep <> ATrackBar.PageSize then
      QtTrackBar.setPageStep(ATrackBar.PageSize);
    if QtTrackBar.getTickInterval <> ATrackBar.Frequency then
      QtTrackBar.setTickInterval(ATrackBar.Frequency);
    if QtTrackBar.getSliderPosition <> ATrackBar.Position then
      QtTrackBar.setSliderPosition(ATrackBar.Position);

    if (QtTrackBar.getOrientation <>
      TrackBarOrientationToQtOrientationMap[ATrackBar.Orientation])
      or TrackBarReversed then
    begin
      QtTrackBar.Hide;
      QtTrackBar.setOrientation(TrackBarOrientationToQtOrientationMap[ATrackBar.Orientation]);
      if ATrackBar.Orientation = trHorizontal then
        QtTrackBar.setInvertedAppereance(ATrackBar.Reversed)
      else
        {make it delphi and msdn compatibile when vertical then 0 = top}
        QtTrackBar.setInvertedAppereance(not ATrackBar.Reversed);
      QtTrackBar.setInvertedControls(False);
      QtTrackBar.Show;
    end;
  finally
    QtTrackBar.EndUpdate;
  end;
end;

class function  TQtWSTrackBar.GetPosition(const ATrackBar: TCustomTrackBar): integer;
var
  QtTrackBar: TQtTrackBar;
begin
  QtTrackBar := TQtTrackBar(ATrackBar.Handle);
  Result := QtTrackBar.getSliderPosition;
end;

class procedure TQtWSTrackBar.SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer);
var
  QtTrackBar: TQtTrackBar;
begin
  QtTrackBar := TQtTrackBar(ATrackBar.Handle);
  QtTrackBar.BeginUpdate;
  try
    QtTrackBar.setSliderPosition(NewPosition);
  finally
    QtTrackBar.EndUpdate;
  end;
end;

{ TQtWSProgressBar }

class procedure TQtWSProgressBar.SetRangeStyle(AProgressBar: TQtProgressBar;
  AStyle: TProgressBarStyle; AMin, AMax: Integer; const AIsDesign: Boolean);
begin
  if AStyle = pbstNormal then
  begin
    if (AMin = 0) and (AMax = 0) then
      AProgressBar.setRange(0, 1)
    else
      AProgressBar.setRange(AMin, AMax)
  end
  else
    AProgressBar.setRange(0, Integer(AIsDesign));
end;

class function TQtWSProgressBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  QtProgressBar: TQtProgressBar;
begin
  QtProgressBar := TQtProgressBar.Create(AWinControl, AParams);
  QtProgressBar.AttachEvents;
  Result := TLCLIntfHandle(QtProgressBar);
end;

class procedure TQtWSProgressBar.ApplyChanges(const AProgressBar: TCustomProgressBar);
var
  QtProgressBar: TQtProgressBar;
begin
  QtProgressBar := TQtProgressBar(AProgressBar.Handle);

  // AProgressBar.Smooth is not supported by qt

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
    begin
      QtProgressBar.setOrientation(QtHorizontal);
      QtProgressBar.setInvertedAppearance(False);
    end;
  end;

  QtProgressBar.setTextVisible(AProgressBar.BarShowText);

  // The position, minumum and maximum values
  SetRangeStyle(QtProgressBar, AProgressBar.Style,
    AProgressBar.Min, AProgressBar.Max,
    csDesigning in AProgressBar.ComponentState);
  QtProgressBar.BeginUpdate;
  QtProgressBar.setValue(AProgressBar.Position);
  QtProgressBar.EndUpdate;
end;

class procedure TQtWSProgressBar.SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer);
begin
  TQtProgressBar(AProgressBar.Handle).BeginUpdate;
  TQtProgressBar(AProgressBar.Handle).setValue(NewPosition);
  TQtProgressBar(AProgressBar.Handle).EndUpdate;
end;

class procedure TQtWSProgressBar.SetStyle(
  const AProgressBar: TCustomProgressBar; const NewStyle: TProgressBarStyle);
var
  QProgressBar: TQtProgressBar;
begin
  if not WSCheckHandleAllocated(AProgressBar, 'SetStyle') then
    Exit;
  QProgressBar := TQtProgressBar(AProgressBar.Handle);
  QProgressBar.reset;
  SetRangeStyle(QProgressBar, NewStyle, AProgressBar.Min, AProgressBar.Max,
    csDesigning in AProgressBar.ComponentState);
  if NewStyle = pbstNormal then
    QProgressBar.setValue(AProgressBar.Position);
end;

{ TQtWSStatusBar }

class procedure TQtWSStatusBar.ClearPanels(const Widget: TQtStatusBar);
var
  i: integer;
begin
  if length(Widget.Panels) > 0 then
  begin
    Widget.setUpdatesEnabled(False);
    for i := High(Widget.Panels) downto 0 do
    begin
      Widget.removeWidget(Widget.Panels[i].Widget);
      Widget.Panels[i].DetachEvents;
      QLabel_destroy(QLabelH(Widget.Panels[i].Widget));
      Widget.Panels[i].Widget := nil;
      Widget.Panels[i].Free;
    end;
    Widget.setUpdatesEnabled(True);
    SetLength(Widget.Panels, 0);
  end;
end;

class procedure TQtWSStatusBar.RecreatePanels(const AStatusBar: TStatusBar;
  const Widget: TQtStatusBar);
var
  Str: WideString;
  i: Integer;
begin
  ClearPanels(Widget);
  if AStatusBar.SimplePanel then
  begin
    Str := GetUtf8String(AStatusBar.SimpleText);
    Widget.showMessage(@Str);
  end else
  if AStatusBar.Panels.Count > 0 then
  begin
    Widget.setUpdatesEnabled(False);
    SetLength(Widget.Panels, AStatusBar.Panels.Count);
    for i := 0 to AStatusBar.Panels.Count - 1 do
    begin
      Str := GetUtf8String(AStatusBar.Panels[i].Text);
      Widget.Panels[i] := TQtStatusBarPanel.CreateFrom(AStatusBar,
        QLabel_create(@Str, Widget.Widget));
      Widget.Panels[i].HasPaint := AStatusBar.Panels[i].Style = psOwnerDraw;
      Widget.Panels[i].ID := AStatusBar.Panels[i].ID;
      QLabel_setText(QLabelH(Widget.Panels[i].Widget), @Str);
      QLabel_setAlignment(QLabelH(Widget.Panels[i].Widget),
        AlignmentToQtAlignmentMap[AStatusBar.Panels[i].Alignment]);
      QWidget_setMinimumWidth(Widget.Panels[i].Widget, AStatusBar.Panels[i].Width);
      Widget.Panels[i].AttachEvents;
      Widget.addWidget(Widget.Panels[i].Widget, ord(i = AStatusBar.Panels.Count - 1));
    end;
    Widget.setUpdatesEnabled(True);
  end;
end;

class function TQtWSStatusBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  AStatusBar: TStatusBar absolute AWinControl;
  QtStatusBar: TQtStatusBar;
begin
  QtStatusBar := TQtStatusBar.Create(AWinControl, AParams);
  QtStatusBar.setSizeGripEnabled(TStatusBar(AWinControl).SizeGrip and
    TStatusBar(AWinControl).SizeGripEnabled);

  RecreatePanels(TStatusBar(AWinControl), QtStatusBar);

  QtStatusBar.AttachEvents;

  // Return handle

  Result := TLCLIntfHandle(QtStatusBar);
end;

class procedure TQtWSStatusBar.DestroyHandle(const AWinControl: TWinControl);
var
  QtStatusBar: TQtStatusBar;
begin
  QtStatusBar := TQtStatusBar(AWinControl.Handle);
  ClearPanels(QtStatusBar);
  QtStatusBar.Release;
end;

class procedure TQtWSStatusBar.PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer);
var
  QtStatusBar: TQtStatusBar;
  Str: Widestring;
begin
  QtStatusBar := TQtStatusBar(AStatusBar.Handle);
  if AStatusBar.SimplePanel then
  begin
    ClearPanels(QtStatusBar);
    Str := GetUtf8String(AStatusBar.SimpleText);
    QtStatusBar.showMessage(@Str);
  end else
  if AStatusBar.Panels.Count > 0 then
  begin
    QStatusBar_clearMessage(QStatusBarH(QtStatusBar.Widget));

    if (PanelIndex >= Low(QtStatusBar.Panels)) and
      (PanelIndex <= High(QtStatusBar.Panels)) then
    begin
      Str := GetUtf8String(AStatusBar.Panels[PanelIndex].Text);
      QLabel_setText(QLabelH(QtStatusBar.Panels[PanelIndex].Widget), @Str);
      QLabel_setAlignment(QLabelH(QtStatusBar.Panels[PanelIndex].Widget),
        AlignmentToQtAlignmentMap[AStatusBar.Panels[PanelIndex].Alignment]);
      QWidget_setMinimumWidth(QtStatusBar.Panels[PanelIndex].Widget,
        AStatusBar.Panels[PanelIndex].Width);
    end;
  end;
end;

class procedure TQtWSStatusBar.SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer);
var
  QtStatusBar: TQtStatusBar;
  Str: Widestring;
begin
  QtStatusBar := TQtStatusBar(AStatusBar.Handle);
  if AStatusBar.SimplePanel then
  begin
    Str := GetUtf8String(AStatusBar.SimpleText);
    QtStatusBar.showMessage(@Str);
  end else
  begin
    if (PanelIndex >= Low(QtStatusBar.Panels)) and
      (PanelIndex <= High(QtStatusBar.Panels)) then
    begin
      Str := GetUtf8String(AStatusBar.Panels[PanelIndex].Text);
      QLabel_setText(QLabelH(QtStatusBar.Panels[PanelIndex].Widget), @Str);
    end;
  end;
end;

class procedure TQtWSStatusBar.SetSizeGrip(const AStatusBar: TStatusBar;
  SizeGrip: Boolean);
var
  QtStatusBar: TQtStatusBar;
begin
  if not WSCheckHandleAllocated(AStatusBar, 'SetSizeGrip') then
    Exit;
  QtStatusBar := TQtStatusBar(AStatusBar.Handle);
  QtStatusBar.setSizeGripEnabled(SizeGrip and AStatusBar.SizeGripEnabled);
end;

class procedure TQtWSStatusBar.Update(const AStatusBar: TStatusBar);
var
  QtStatusBar: TQtStatusBar;
begin
  QtStatusBar := TQtStatusBar(AStatusBar.Handle);
  RecreatePanels(AStatusBar, QtStatusBar);
end;

{ TQtWSCustomListView }

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}

class function TQtWSCustomListView.IsIconView(const AList: TCustomListView): boolean;
begin
  Result := TListView(AList).ViewStyle <> vsReport;
end;

class function TQtWSCustomListView.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtTreeWidget: TQtTreeWidget;
  QtListWidget: TQtListWidget;
begin
  if IsIconView(TCustomListView(AWinControl)) then
  begin
    QtListWidget := TQtListWidget.Create(AWinControl, AParams);
    QtListWidget.ViewStyle := Ord(TListView(AWinControl).ViewStyle);
    if TListView(AWinControl).ViewStyle in [vsIcon, vsSmallIcon] then
    begin
      // emabarcadero docs says
      // vsIcon, vsSmallIcon
      // Each item appears as a full-sized icon with a label below it.
      // The user can drag the items to any location in the list view window.
      QtListWidget.setViewMode(QListViewIconMode);
      QtListWidget.setResizeMode(QListViewAdjust);
      QtListWidget.setMovement(QListViewFree);

      with TCustomListView(AWinControl) do
      begin
        QtListWidget.setWrapping(IconOptions.AutoArrange);
        QtListWidget.setViewFlow(IconArngToQListFlow[IconOptions.Arrangement]);
        QtListWidget.setWordWrap(IconOptions.WrapText);
      end;

    end else
      QtListWidget.setViewMode(QListViewListMode);

    QtListWidget.Checkable := TCustomListView(AWinControl).Checkboxes;
    QtListWidget.AttachEvents;
    Result := TLCLIntfHandle(QtListWidget);
  end else
  begin
    QtTreeWidget := TQtTreeWidget.Create(AWinControl, AParams);
    QtTreeWidget.ViewStyle := Ord(TListView(AWinControl).ViewStyle);
    QtTreeWidget.setStretchLastSection(False);
    QtTreeWidget.setRootIsDecorated(False);
    QtTreeWidget.AttachEvents;
    Result := TLCLIntfHandle(QtTreeWidget);
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnDelete
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ColumnDelete(const ALV: TCustomListView;
  const AIndex: Integer);
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnDelete') then
    Exit;

  // TODO: columns in vsIcon mode
  if IsIconView(ALV) then
    exit;

  // we must recreate handle since there's no column removal support
  // in our bindings (protected methods in qt).
  RecreateWnd(ALV);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnInsert
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ColumnInsert(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn);
var
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  TWIChild: QTreeWidgetItemH;
  Str: WideString;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnInsert') then
    Exit;

  // TODO: columns in vsIcon mode
  if IsIconView(ALV) then
    exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);

  if QtTreeWidget.ColCount <> TListView(ALV).Columns.Count then
   	QtTreeWidget.ColCount := TListView(ALV).Columns.Count;

  if (QtTreeWidget.ColCount <= 1) and TListView(ALV).ShowColumnHeaders then
    QtTreeWidget.setHeaderVisible(True);

  TWI := QtTreeWidget.headerItem;

  if QTreeWidgetItem_childCount(TWI) < (AIndex + 1) then
  begin
    TWIChild := QTreeWidgetItem_create(0);
    QTreeWidgetItem_setFlags(TWIChild, QtItemIsEnabled);
    QTreeWidgetItem_addChild(TWI, TWIChild);
    Str := GetUtf8String(ALV.Column[AIndex].Caption);
    QTreeWidgetItem_setText(TWI, AIndex, @Str);
  end;

  if (csDesigning in ALV.ComponentState) then
    exit;

	QtTreeWidget.Header.Clickable := TListView(ALV).ColumnClick;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnGetWidth
  Params:  None
  Returns: Integer
 ------------------------------------------------------------------------------}
class function  TQtWSCustomListView.ColumnGetWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn): Integer;
var
  QtTreeWidget: TQtTreeWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnGetWidth') then
    Exit;

  // TODO: columns in vsIcon mode
  if IsIconView(ALV) then
    exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  Result := QtTreeWidget.ColWidth[AIndex];
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnMove
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ColumnMove(const ALV: TCustomListView;
  const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn);
var
  QtTreeWidget: TQtTreeWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnMove') then
    Exit;

  if (csDesigning in ALV.ComponentState) then
    exit;

  // TODO: columns in vsIcon mode
  if IsIconView(ALV) then
    exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  QtTreeWidget.Header.moveSection(AOldIndex, ANewIndex);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnSetAlignment
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ColumnSetAlignment(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AAlignment: TAlignment);
var
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  i: Integer;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetAlignment') then
    Exit;

  // TODO: columns in vsIcon mode
  if IsIconView(ALV) then
    exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  TWI := QtTreeWidget.headerItem;
  QTreeWidgetItem_setTextAlignment(TWI, AIndex,
    AlignmentToQtAlignmentMap[AAlignment]);


  if not (csLoading in ALV.ComponentState) then
    for i := 0 to QtTreeWidget.ItemCount - 1 do
    begin
      TWI := QtTreeWidget.topLevelItem(i);
      if TWI <> nil then
        QTreeWidgetItem_setTextAlignment(TWI, AIndex,
          AlignmentToQtAlignmentMap[AAlignment]);
    end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnSetAutoSize
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ColumnSetAutoSize(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean);
var
  QtTreeWidget: TQtTreeWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetAutoSize') then
    Exit;

  if (csDesigning in ALV.ComponentState) then
    exit;

  // TODO: columns in vsIcon mode
  if IsIconView(ALV) then
    exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  if AAutoSize then
    QtTreeWidget.Header.setResizeMode(AIndex, QHeaderViewResizeToContents)
  else
    QtTreeWidget.Header.setResizeMode(AIndex, QHeaderViewInteractive);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnSetCaption
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ColumnSetCaption(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const ACaption: String);
var
  Str: WideString;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetCaption') then
    Exit;

  // TODO: columns in vsIcon mode
  if IsIconView(ALV) then
    exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  TWI := QtTreeWidget.headerItem;
  if TWI <> NiL then
  begin
    Str := GetUtf8String(ACaption);
    QTreeWidgetItem_setText(TWI, AIndex, @Str);
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnSetImage
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ColumnSetImage(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer);
var
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  Bmp: TBitmap;
  ImgList: TImageList;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetImage') then
    Exit;

  // TODO: columns in vsIcon mode
  if IsIconView(ALV) then
    exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  TWI := QtTreeWidget.headerItem;
  if TWI <> NiL then
  begin
    ImgList := TImageList.Create(nil);
    try
      if (TListView(ALV).ViewStyle = vsIcon) and
        Assigned(TListView(ALV).LargeImages) then
        ImgList.Assign(TListView(ALV).LargeImages);

      if (TListView(ALV).ViewStyle in [vsSmallIcon, vsReport, vsList]) and
        Assigned(TListView(ALV).SmallImages) then
        ImgList.Assign(TListView(ALV).SmallImages);

      if (ImgList.Count > 0) and
        ((AImageIndex >= 0) and (AImageIndex < ImgList.Count)) then
      begin
        Bmp := TBitmap.Create;
        try
          ImgList.GetBitmap(AImageIndex, Bmp);
          QTreeWidgetItem_setIcon(TWI, AIndex, TQtImage(Bmp.Handle).AsIcon);
        finally
          Bmp.Free;
        end;
      end;
    finally
      ImgList.Free;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnSetMinWidth
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ColumnSetMinWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer);
var
  QtTreeWidget: TQtTreeWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetMinWidth') then
    Exit;

  // TODO: columns in vsIcon mode
  if IsIconView(ALV) then
    exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  QtTreeWidget.MinColSize[AIndex] := AMinWidth;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnSetWidth
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ColumnSetWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer);
var
  QtTreeWidget: TQtTreeWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetWidth') then
    Exit;

  // TODO: columns in vsIcon mode
  if IsIconView(ALV) then
    exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  QtTreeWidget.ColWidth[AIndex] := AWidth;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnSetVisible
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ColumnSetVisible(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean);
var
  QtTreeWidget: TQtTreeWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetVisible') then
    Exit;

  // TODO: columns in vsIcon mode
  if IsIconView(ALV) then
    exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  QtTreeWidget.ColVisible[AIndex] := AVisible;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemDelete
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ItemDelete(const ALV: TCustomListView;
  const AIndex: Integer);
var
  QtTreeWidget: TQtTreeWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemDelete') then
    Exit;
  if IsIconView(ALV) then
    TQtListWidget(ALV.Handle).removeItem(AIndex)
  else
  begin
    TQtListWidget(ALV.Handle).BeginUpdate;
    try
      QtTreeWidget := TQtTreeWidget(ALV.Handle);
      QtTreeWidget.DeleteItem(AIndex);
    finally
      TQtListWidget(ALV.Handle).EndUpdate;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemGetChecked
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function  TQtWSCustomListView.ItemGetChecked(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem): Boolean;
var
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  LWI: QListWidgetItemH;
  AState: QtCheckState;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemGetChecked') then
    Exit;

  Result := ALV.CheckBoxes;
  if not Result then
    exit;

  AState := QtUnChecked;

  if IsIconView(ALV) then
  begin
    LWI := TQtListWidget(ALV.Handle).getItem(AIndex);
    if LWI <> nil then
      AState := QListWidgetItem_checkState(LWI);
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    TWI := QtTreeWidget.topLevelItem(AIndex);
    if TWI <> nil then
      AState := QTreeWidgetItem_checkState(TWI, 0);
  end;
  Result := AState = QtChecked;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemGetPosition
  Params:  None
  Returns: TPoint
 ------------------------------------------------------------------------------}
class function  TQtWSCustomListView.ItemGetPosition(const ALV: TCustomListView;
  const AIndex: Integer): TPoint;
var
  QtListWidget: TQtListWidget;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  R: TRect;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemGetPosition') then
    Exit;

  R := Rect(0, 0, 0, 0);
  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    R := QtListWidget.getVisualItemRect(QtListWidget.getItem(AIndex));
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    TWI := QtTreeWidget.topLevelItem(AIndex);
    R := QtTreeWidget.visualItemRect(TWI);
  end;
  Result.X := R.Left;
  Result.Y := R.Top;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemGetState
  Params:  None
  Returns: TPoint
 ------------------------------------------------------------------------------}
class function  TQtWSCustomListView.ItemGetState(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem;
  const AState: TListItemState; out AIsSet: Boolean): Boolean;
var
  QtListWidget: TQtListWidget;
  LWI: QListWidgetItemH;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  i: Integer;
  Arr: TPtrIntArray;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemGetState') then
    Exit;

  AIsSet := False;
  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    LWI := QtListWidget.getItem(AIndex);
    if LWI <> nil then
      case AState of
        lisFocused: AIsSet := LWI = QtListWidget.currentItem;
        lisSelected: AIsSet := QtListWidget.getItemSelected(LWI);
      end;
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    TWI := QtTreeWidget.topLevelItem(AIndex);
    if TWI <> nil then
    begin
      case AState of
        lisFocused: AIsSet := TWI = QtTreeWidget.currentItem;
        lisSelected:
        begin
          Arr := QtTreeWidget.selectedItems;
          for i := 0 to High(Arr) do
          begin
            TWI := QTreeWidgetItemH(Arr[i]);
            if AIndex = QtTreeWidget.getRow(TWI) then
            begin
              AIsSet := True;
              break;
            end;
          end;
        end;
      end;
    end;
  end;
  Result := True;

end;

class procedure TQtWSCustomListView.ItemSetImage(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const ASubIndex,
  AImageIndex: Integer);
var
  QtListWidget: TQtListWidget;
  LWI: QListWidgetItemH;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  Bmp: TBitmap;
  ImgList: TImageList;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetImage') then
    Exit;

  if not Assigned(TListView(ALV).LargeImages) and not
    Assigned(TListView(ALV).SmallImages) then
      exit;
  TWI := nil;
  LWI := nil;
  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    LWI := QtListWidget.getItem(AIndex);
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    TWI := QtTreeWidget.topLevelItem(AIndex);
  end;
  if (TWI <> nil) or (LWI <> nil) then
  begin
    ImgList := TImageList.Create(nil);
    try
      if (TListView(ALV).ViewStyle = vsIcon) and
        Assigned(TListView(ALV).LargeImages) then
        ImgList.Assign(TListView(ALV).LargeImages);

      if (TListView(ALV).ViewStyle in [vsSmallIcon, vsReport, vsList]) and
        Assigned(TListView(ALV).SmallImages) then
        ImgList.Assign(TListView(ALV).SmallImages);

      if (ImgList.Count > 0) and
        ((AImageIndex >= 0) and (AImageIndex < ImgList.Count)) then
      begin
        Bmp := TBitmap.Create;
        try
          ImgList.GetBitmap(AImageIndex, Bmp);
          if LWI <> nil then
            QListWidgetItem_setIcon(LWI, TQtImage(Bmp.Handle).AsIcon)
          else
            QTreeWidgetItem_setIcon(TWI, ASubIndex, TQtImage(Bmp.Handle).AsIcon);
        finally
          Bmp.Free;
        end;
      end;
    finally
      ImgList.Free;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemSetChecked
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ItemSetChecked(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const AChecked: Boolean);
var
  QtListWidget: TQtListWidget;
  LWI: QListWidgetItemH;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetChecked') then
    Exit;

  if not ALV.CheckBoxes then
    exit;

  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    LWI := QtListWidget.getItem(AIndex);
    if AChecked then
      QListWidgetItem_setCheckState(LWI, QtChecked)
    else
      QListWidgetItem_setCheckState(LWI, QtUnChecked);
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    TWI := QtTreeWidget.topLevelItem(AIndex);
    if AChecked then
      QTreeWidgetItem_setCheckState(TWI, 0, QtChecked)
    else
      QTreeWidgetItem_setCheckState(TWI, 0, QtUnChecked);
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemSetState
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ItemSetState(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem;
  const AState: TListItemState; const AIsSet: Boolean);
var
  QtListWidget: TQtListWidget;
  LWI: QListWidgetItemH;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetState') then
    Exit;
  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    LWI := QtListWidget.getItem(AIndex);
    case AState of
      lisFocused: QtListWidget.setCurrentItem(LWI);
      lisSelected: QtListWidget.setItemSelected(LWI, AIsSet);
    end;
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    TWI := QtTreeWidget.topLevelItem(AIndex);
    QtTreeWidget.BeginUpdate;
    case AState of
      lisFocused: QtTreeWidget.setCurrentItem(TWI);
      lisSelected:
      begin
        if ALV.RowSelect and AIsSet and not ALV.MultiSelect then
          QtTreeWidget.setCurrentItem(TWI);
        QtTreeWidget.setItemSelected(TWI, AIsSet);
      end;
    end;
    QtTreeWidget.EndUpdate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemInsert
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ItemInsert(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem);
var
  QtListWidget: TQtListWidget;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  Str: WideString;
  i: Integer;
  AAlignment: QtAlignment;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemInsert') then
    Exit;

  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    QtListWidget.Checkable := ALV.Checkboxes;
    QtListWidget.insertItem(AIndex, AItem.Caption);
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    TWI := QTreeWidgetItem_create(Integer(0));
    if AItem.Caption <> '' then
      Str := GetUtf8String(AItem.Caption)
    else
      Str := '';

    if ALV.CheckBoxes then
    begin
      if AItem.Checked then
      	QTreeWidgetItem_setCheckState(TWI, 0, QtChecked)
      else
      	QTreeWidgetItem_setCheckState(TWI, 0, QtUnchecked);
    end;

    AAlignment := QtAlignLeft;
    if TListView(ALV).Columns.Count > 0 then
      AAlignment := AlignmentToQtAlignmentMap[ALV.Column[0].Alignment];

    if Str <> '' then
      QtTreeWidget.setItemText(TWI, 0, Str, AAlignment);

    for i := 0 to AItem.SubItems.Count - 1 do
    begin
      AAlignment := QtAlignLeft;
      if (TListView(ALV).Columns.Count > 0) and (i + 1 < TListView(ALV).Columns.Count) then
        AAlignment := AlignmentToQtAlignmentMap[ALV.Column[i + 1].Alignment];
      if AItem.Subitems.Strings[i] <> '' then
      begin
        Str := GetUtf8String(AItem.Subitems.Strings[i]);
        QtTreeWidget.setItemText(TWI, i + 1, Str, AAlignment);
      end;
    end;
    QtTreeWidget.insertTopLevelItem(AIndex, TWI);
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemSetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ItemSetText(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer;
  const AText: String);
var
  QtListWidget: TQtListWidget;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  Str: WideString;
  AAlignment: QtAlignment;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetText') then
    Exit;

  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    AAlignment := QtAlignLeft;
    if (TListView(ALV).Columns.Count > 0) and (ASubIndex < TListView(ALV).Columns.Count)  then
      AAlignment := AlignmentToQtAlignmentMap[ALV.Column[ASubIndex].Alignment];
    QtListWidget.setItemText(AIndex, AText, AAlignment);
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    Str := GetUtf8String(AText);
    TWI := QtTreeWidget.topLevelItem(AIndex);
    if TWI <> NiL then
    begin
      AAlignment := QtAlignLeft;
      if (TListView(ALV).Columns.Count > 0) and (ASubIndex < TListView(ALV).Columns.Count)  then
        AAlignment := AlignmentToQtAlignmentMap[ALV.Column[ASubIndex].Alignment];
      QtTreeWidget.setItemText(TWI, ASubIndex, Str, AAlignment);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemShow
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ItemShow(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean);
var
  QtListWidget: TQtListWidget;
  LWI: QListWidgetItemH;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemShow') then
    Exit;
  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    LWI := QtListWidget.getItem(AIndex);
    QtListWidget.setItemVisible(LWI, True);
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    TWI := QtTreeWidget.topLevelItem(AIndex);
    QtTreeWidget.setItemVisible(TWI, True);
  end;
end;


{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemDisplayRect
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function  TQtWSCustomListView.ItemDisplayRect(const ALV: TCustomListView;
  const AIndex, ASubItem: Integer; ACode: TDisplayCode): TRect;
var
  QtListWidget: TQtListWidget;
  LWI: QListWidgetItemH;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemDisplayRect') then
    Exit;

  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    LWI := QtListWidget.getItem(AIndex);
    Result := QtListWidget.getVisualItemRect(LWI);
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    TWI := QtTreeWidget.topLevelItem(AIndex);
    if QTreeWidgetItem_childCount(TWI) > 0 then
      Result := QtTreeWidget.visualItemRect(QTreeWidgetItem_child(TWI, ASubItem))
    else
      Result := QtTreeWidget.visualItemRect(TWI);
  end;
end;

class procedure TQtWSCustomListView.BeginUpdate(const ALV: TCustomListView);
var
  QtWidget: TQtWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'BeginUpdate') then
    Exit;
  QtWidget := TQtWidget(ALV.Handle);
  if not QtWidget.InUpdate then
    QtWidget.setUpdatesEnabled(False);
  QtWidget.BeginUpdate;
end;

class procedure TQtWSCustomListView.EndUpdate(const ALV: TCustomListView);
var
  QtWidget: TQtWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'EndUpdate') then
    Exit;
  QtWidget := TQtWidget(ALV.Handle);
  QtWidget.EndUpdate;
  if not QtWidget.InUpdate then
    QtWidget.setUpdatesEnabled(True);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.GetFocused
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomListView.GetFocused(const ALV: TCustomListView): Integer;
var
  QtListWidget: TQtListWidget;
  LWI: QListWidgetItemH;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  i: Integer;
begin
  if not WSCheckHandleAllocated(ALV, 'GetFocused') then
    Exit;

  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    LWI := QtListWidget.currentItem;
    if QtListWidget.getItemSelected(LWI) then
      Result := QtListWidget.getRow(LWI)
    else
      Result := -1;
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    TWI := QtTreeWidget.currentItem;
    i := QtTreeWidget.getRow(TWI);
    if QTreeWidgetItem_isSelected(TWI) then
      Result := i
    else
      Result := -1;
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.GetItemAt
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomListView.GetItemAt(const ALV: TCustomListView; x,y: integer): Integer;
var
  QtListWidget: TQtListWidget;
  LWI: QListWidgetItemH;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
begin
  if not WSCheckHandleAllocated(ALV, 'GetItemAt') then
    Exit;
  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    LWI := QtListWidget.itemAt(x, y);
    Result := QtListWidget.getRow(LWI);
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    TWI := QtTreeWidget.itemAt(x, y);
    Result := QtTreeWidget.getRow(TWI);
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.GetSelCount
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomListView.GetSelCount(const ALV: TCustomListView): Integer;
begin
  if not WSCheckHandleAllocated(ALV, 'GetSelCount') then
    Exit;
  if IsIconView(ALV) then
    Result := TQtListWidget(ALV.Handle).getSelCount
  else
    Result := TQtTreeWidget(ALV.Handle).selCount;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.GetSelection
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomListView.GetSelection(const ALV: TCustomListView): Integer;
var
  QtListWidget: TQtListWidget;
  QtTreeWidget: TQtTreeWidget;
  FPInts: TPtrIntArray;
begin
  if not WSCheckHandleAllocated(ALV, 'GetSelection') then
    Exit;
  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    FPInts := QtListWidget.selectedItems;
  end else
  begin
    {implement selection event so we can return Alv.Selected.Index}
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    FPInts := QtTreeWidget.selectedItems;
  end;
  if Length(FPInts)>0 then
    Result := FPInts[0]
  else
    Result := -1;
end;

class function TQtWSCustomListView.GetTopItem(const ALV: TCustomListView
  ): Integer;
var
  QtItemView: TQtAbstractItemView;
begin
  Result := -1;
  if not WSCheckHandleAllocated(ALV, 'GetTopItem') then
    Exit;
  // according to embarcadero docs this should return
  // only for vsList and vsReport
  if not (TListView(ALV).ViewStyle in [vsList, vsReport]) then
    exit;
  QtItemView := TQtAbstractItemView(ALV.Handle);
  Result := QtItemView.getTopItem;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.SetSort
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.SetSort(const ALV: TCustomListView;
  const AType: TSortType; const AColumn: Integer);
var
  QtTreeWidget: TQtTreeWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'SetSort') then
    Exit;

  if (csDesigning in ALV.ComponentState) then
    exit;

  if IsIconView(ALV) then
    exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);

  if AType = stNone then
    QtTreeWidget.Header.SetSortIndicatorVisible(False)
  else
  begin
    with QtTreeWidget.Header do
    begin
      SetSortIndicatorVisible(True);
      if (AColumn >= 0) and (AColumn < QtTreeWidget.ColCount) then
        SetSortIndicator(AColumn, QtAscendingOrder);
    end;
  end;

end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.GetBoundingRect
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomListView.GetBoundingRect(const ALV: TCustomListView): TRect;
begin
  if not WSCheckHandleAllocated(ALV, 'GetBoundingRect') then
    Exit;
  Result := TQtWidget(ALV.Handle).getFrameGeometry;
end;

class function TQtWSCustomListView.GetViewOrigin(const ALV: TCustomListView
  ): TPoint;
var
  QtItemView: TQtAbstractItemView;
begin
  Result := Point(0, 0);
  if not WSCheckHandleAllocated(ALV, 'GetViewOrigin') then
    Exit;
  QtItemView := TQtAbstractItemView(ALV.Handle);
  Result := QtItemView.getViewOrigin;
end;

class function TQtWSCustomListView.GetVisibleRowCount(const ALV: TCustomListView
  ): Integer;
begin
  Result := 0;
  if not WSCheckHandleAllocated(ALV, 'GetVisibleRowCount') then
    Exit;
  Result := TQtAbstractItemView(ALV.Handle).getVisibleRowCount;
end;

class procedure TQtWSCustomListView.SetAllocBy(const ALV: TCustomListView;
  const AValue: Integer);
var
  QtList: TQtListWidget;
  NewValue: integer;
begin
  if not WSCheckHandleAllocated(ALV, 'SetAllocBy') then
    Exit;
  if TListView(ALV).ViewStyle <> vsReport then
  begin
    NewValue := AValue;
    if NewValue < 0 then
      NewValue := 0;
    QtList := TQtListWidget(ALV.Handle);
    if NewValue > 0 then
    begin
      QtList.setLayoutMode(QListViewBatched);
      QtList.BatchSize := NewValue;
    end else
      QtList.setLayoutMode(QListViewSinglePass);
  end;
end;

class procedure TQtWSCustomListView.SetIconArrangement(
  const ALV: TCustomListView; const AValue: TIconArrangement);
var
  QtList: TQtListWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'SetIconArrangement') then
    Exit;
  if IsIconView(ALV) then
  begin
    // hm...seem that QListView have bug, doesn't want to rearrange items
    // in any case when iaTop and AutoArrange=True (then it looks same as
    // iaLeft without arrange, so we must set GridSize in that case
    // update: bug is fixed in Qt-4.6.2
    {$note set workaround for QListView bug via QtList.GridSize}
    QtList := TQtListWidget(ALV.Handle);
    if QtList.ViewStyle <> Ord(vsList) then
      QtList.setViewFlow(IconArngToQListFlow[AValue]);
  end;
end;

class procedure TQtWSCustomListView.SetItemsCount(const ALV: TCustomListView;
  const Avalue: Integer);
var
  QtListWidget: TQtListWidget;
  QtTreeWidget: TQtTreeWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'SetItemsCount') then
    Exit;
  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    QtListWidget.ItemCount := AValue;
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    QtTreeWidget.ItemCount := AValue;
  end;
end;

class procedure TQtWSCustomListView.SetOwnerData(const ALV: TCustomListView;
  const AValue: Boolean);
var
  QtItemView: TQtAbstractItemView;
begin
  if not WSCheckHandleAllocated(ALV, 'SetOwnerData') then
    Exit;
  QtItemView := TQtAbstractItemView(ALV.Handle);
  QtItemView.OwnerData := AValue;
end;

class procedure TQtWSCustomListView.SetProperty(const ALV: TCustomListView;
  const AProp: TListViewProperty; const AIsSet: Boolean);
const
  BoolToSelectionMode: array[Boolean] of QAbstractItemViewSelectionMode =
  (
    QAbstractItemViewSingleSelection,
    QAbstractItemViewExtendedSelection
  );
  BoolToSelectionBehavior: array[Boolean] of QAbstractItemViewSelectionBehavior =
  (
    QAbstractItemViewSelectItems,
    QAbstractItemViewSelectRows
  );
  BoolToEditTriggers: array[Boolean] of QAbstractItemViewEditTriggers =
  (
    QAbstractItemViewSelectedClicked,
    QAbstractItemViewNoEditTriggers
  );
var
  SavedCheckable: Boolean;
  QtItemView: TQtAbstractItemView;
begin
  if not WSCheckHandleAllocated(ALV, 'SetProperty')
  then Exit;
  QtItemView := TQtAbstractItemView(ALV.Handle);
  case AProp of
    lvpAutoArrange:
      begin
        if IsIconView(ALV) and
          (TQtListWidget(ALV.Handle).ViewStyle <> Ord(vsList)) then
          TQtListWidget(ALV.Handle).setWrapping(AIsSet);
      end;
    lvpCheckboxes:
      begin
        SavedCheckable := QtItemView.Checkable;
        QtItemView.Checkable := AIsSet;
        if SavedCheckable <> AIsSet then
          RecreateWnd(ALV);
      end;
    lvpMultiSelect:
      begin
        if (QtItemView.getSelectionMode <> QAbstractItemViewNoSelection) then
          QtItemView.setSelectionMode(BoolToSelectionMode[AIsSet]);
      end;
    lvpShowColumnHeaders:
      begin
        if not IsIconView(ALV) then
          with TQtTreeWidget(ALV.Handle) do
            setHeaderVisible(AIsSet and (TListView(ALV).ViewStyle = vsReport)
              and (TListView(ALV).Columns.Count > 0) );
      end;
    lvpReadOnly: QtItemView.setEditTriggers(BoolToEditTriggers[AIsSet]);
    lvpRowSelect:
      begin
        if not IsIconView(ALV) then
          TQtTreeWidget(ALV.Handle).setAllColumnsShowFocus(AIsSet);
        QtItemView.setSelectionBehavior(BoolToSelectionBehavior[AIsSet]);
      end;
    lvpWrapText: QtItemView.setWordWrap(AIsSet);
    lvpHideSelection: QtItemView.HideSelection := AIsSet;
  end;
end;

class procedure TQtWSCustomListView.SetProperties(const ALV: TCustomListView;
  const AProps: TListViewProperties);
var
  i: TListViewProperty;
begin
  if not WSCheckHandleAllocated(ALV, 'SetProperties')
  then Exit;
  for i := Low(TListViewProperty) to High(TListViewProperty) do
    SetProperty(ALV, i, i in AProps);
end;

class procedure TQtWSCustomListView.SetScrollBars(const ALV: TCustomListView;
  const AValue: TScrollStyle);
var
  QtItemView: TQtAbstractItemView;
begin
  if not WSCheckHandleAllocated(ALV, 'SetScrollBars') then
    Exit;
  QtItemView := TQtAbstractItemView(ALV.Handle);
  {always reset before applying new TScrollStyle}
  QtItemView.setScrollStyle(ssNone);
  if AValue <> ssNone then
    QtItemView.setScrollStyle(AValue);
end;

class procedure TQtWSCustomListView.SetViewStyle(const ALV: TCustomListView;
  const AValue: TViewStyle);
var
  QtItemView: TQtAbstractItemView;
  QtListWidget: TQtListWidget;
  LWI: QListWidgetItemH;
  QtTreeWidget: TQtTreeWidget;
  ItemViewWidget: QAbstractItemViewH;
  Item: QTreeWidgetItemH;
  Size: TSize;
  x: Integer;
  j: Integer;
begin
  if not WSCheckHandleAllocated(ALV, 'SetViewStyle') then
    Exit;
  QtItemView := TQtAbstractItemView(ALV.Handle);

  if (QtItemView.ViewStyle <> Ord(AValue)) then
  begin
    RecreateWnd(ALV);
    exit;
  end;

  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    ItemViewWidget := QListWidgetH(QtListWidget.Widget);
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    ItemViewWidget := QTreeWidgetH(QtTreeWidget.Widget);
    with QtTreeWidget do
      setHeaderVisible(TListView(ALV).ShowColumnHeaders and (AValue = vsReport)
        and (TListView(ALV).Columns.Count > 0) );
  end;
  case AValue of
    vsIcon:
       begin
        x := QStyle_pixelMetric(QApplication_style(), QStylePM_IconViewIconSize,
          nil, ItemViewWidget);
        Size.cx := x;
        Size.cy := x;
        if Assigned(TListView(ALV).LargeImages) then
        begin
          Size.cy := TListView(ALV).LargeImages.Height;
          Size.cx := TListView(ALV).LargeImages.Width;
        end;
      end;
    vsSmallIcon:
      begin
        x := QStyle_pixelMetric(QApplication_style(), QStylePM_ListViewIconSize,
          nil, ItemViewWidget);
        Size.cx := x;
        Size.cy := x;
        if Assigned(TListView(ALV).SmallImages) then
        begin
          Size.cy := TListView(ALV).SmallImages.Height;
          Size.cx := TListView(ALV).SmallImages.Width;
        end;
      end;
    vsList, vsReport:
      begin
        x := QStyle_pixelMetric(QApplication_style(), QStylePM_ListViewIconSize,
          nil, ItemViewWidget);
        Size.cx := x;
        Size.cy := x;
      end;
  end;

  TQtAbstractItemView(ALV.Handle).IconSize := Size;

  if IsIconView(ALV) then
  begin
    LWI := QtListWidget.getItem(0);
    if LWI <> nil then
    begin
      X := Size.CY;
      QListWidgetItem_sizeHint(LWI, @Size);
      Size.Cy := X;
      QListWidgetItem_setSizeHint(LWI, @Size);
    end;
  end else
  begin
    Item := QtTreeWidget.topLevelItem(0);
    if Item <> nil then
    begin
      X := Size.CY;
      QTreeWidgetItem_sizeHint(Item, @Size, 0);
      Size.Cy := X;
      QTreeWidgetItem_setSizeHint(Item, 0, @Size);

      for j := 0 to QtTreeWidget.ColCount - 1 do
      begin
        Item := QtTreeWidget.itemAt(j, 0);
        QTreeWidgetItem_setSizeHint(Item, j, @Size);
      end;
    end;
    QtTreeWidget.UniformRowHeights := True;
  end;
end;

end.
