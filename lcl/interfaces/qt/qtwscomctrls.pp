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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
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
  SysUtils, Classes, Types, ComCtrls, Controls, LCLType, Graphics, LCLProc, LCLIntf, Forms,
  // Widgetset
  WSProc, WSComCtrls, WSLCLClasses;

type

  { TQtWSStatusBar }

  TQtWSStatusBar = class(TWSStatusBar)
  private
  protected
    class procedure ClearPanels(const Widget: TQtStatusBar);
    class procedure RecreatePanels(const AStatusBar: TStatusBar; const Widget: TQtStatusBar);
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure SetSizeGrip(const AStatusBar: TStatusBar; SizeGrip: Boolean); override;
    class procedure Update(const AStatusBar: TStatusBar); override;
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
  public
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
    class procedure ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); override;
    class procedure ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer; const AText: String); override;
    class procedure ItemShow(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean); override;
    class function  ItemDisplayRect(const ALV: TCustomListView; const AIndex, ASubItem: Integer; ACode: TDisplayCode): TRect; override;
    
    {parent}
    class function GetFocused(const ALV: TCustomListView): Integer; override;
    class function GetItemAt(const ALV: TCustomListView; x,y: integer): Integer; override;
    class function GetSelCount(const ALV: TCustomListView): Integer; override;
    class function GetSelection(const ALV: TCustomListView): Integer; override;
    class procedure SetSort(const ALV: TCustomListView; const AType: TSortType; const AColumn: Integer); override;
    
    class function GetBoundingRect(const ALV: TCustomListView): TRect; override;


    (*
    // Column

    class procedure ColumnSetMaxWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer); override;


    // Item

    class procedure ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AImageIndex: Integer); virtual;
    class function ItemSetPosition(const ALV: TCustomListView; const AIndex: Integer; const ANewPosition: TPoint): Boolean; virtual;

    // LV
    class procedure BeginUpdate(const ALV: TCustomListView); virtual;
    class procedure EndUpdate(const ALV: TCustomListView); virtual;


    class function GetDropTarget(const ALV: TCustomListView): Integer; virtual;

    class function GetHoverTime(const ALV: TCustomListView): Integer; virtual;

    class function GetTopItem(const ALV: TCustomListView): Integer; virtual;
    class function GetViewOrigin(const ALV: TCustomListView): TPoint; virtual;
    class function GetVisibleRowCount(const ALV: TCustomListView): Integer; override;

    class procedure SetAllocBy(const ALV: TCustomListView; const AValue: Integer); virtual;
    class procedure SetDefaultItemHeight(const ALV: TCustomListView; const AValue: Integer); virtual;
    class procedure SetHotTrackStyles(const ALV: TCustomListView; const AValue: TListHotTrackStyles); virtual;
    class procedure SetHoverTime(const ALV: TCustomListView; const AValue: Integer); virtual;
//    class procedure SetIconOptions(const ALV: TCustomListView; const AValue: TIconOptions); virtual;
    class procedure SetImageList(const ALV: TCustomListView; const AList: TListViewImageList; const AValue: TCustomImageList); virtual;
    class procedure SetProperty(const ALV: TCustomListView; const AProp: TListViewProperty; const AIsSet: Boolean); virtual;
    class procedure SetProperties(const ALV: TCustomListView; const AProps: TListViewProperties); virtual;
    class procedure SetScrollBars(const ALV: TCustomListView; const AValue: TScrollStyle); virtual;

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
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
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
{$ifdef WSToolBar}
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetColor(const AWinControl: TWinControl); override;
{$endif}
  end;

  { TQtWSToolBar }

  TQtWSToolBar = class(TWSToolBar)
  private
  protected
  public
{$ifdef WSToolBar}
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetColor(const AWinControl: TWinControl); override;
{$endif}
  end;

  { TQtWSTrackBar }

  TQtWSTrackBar = class(TWSTrackBar)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
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
  

{ TQtWSToolButton }

{$ifdef WSToolBar}
class function TQtWSToolButton.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  QtToolButton: TQtToolButton;
begin
  QtToolButton := TQtToolButton.Create(AWinControl, AParams);
  QtToolButton.AttachEvents;

  Result := TLCLIntfHandle(QtToolButton);
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
  QColor_setRgb(QColorH(@QColor),Red(Color),Green(Color),Blue(Color));

  // Set color of the widget to QColor
  TQtAbstractButton(AWinControl.Handle).SetColor(@QColor);
end;

{ TQtWSToolBar }

class function TQtWSToolBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  QtToolBar: TQtToolBar;
begin
  QtToolBar := TQtToolBar.Create(AWinControl, AParams);
  QtToolBar.AttachEvents;

  Result := TLCLIntfHandle(QtToolBar);
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
  QColor_setRgb(QColorH(@QColor),Red(Color),Green(Color),Blue(Color));

  // Set color of the widget to QColor
  TQtToolBar(AWinControl.Handle).SetColor(@QColor);
end;
{$endif}

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
begin
  QtTrackBar := TQtTrackBar(ATrackBar.Handle);
  QtTrackBar.BeginUpdate;

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

  if QtTrackBar.getOrientation <> TrackBarOrientationToQtOrientationMap[ATrackBar.Orientation] then
  begin
    QtTrackBar.Hide;
    QtTrackBar.setOrientation(TrackBarOrientationToQtOrientationMap[ATrackBar.Orientation]);
    QtTrackBar.setInvertedAppereance(False);
    QtTrackBar.setInvertedControls(False);
    QtTrackBar.Show;
  end;
  QtTrackBar.EndUpdate;
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
  QtTrackBar.setSliderPosition(NewPosition);
end;

{ TQtWSProgressBar }

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

  //  AProgressBar.Smooth is not supported by qt

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
  QtProgressBar.setValue(AProgressBar.Position);

  QtProgressBar.setRange(AProgressBar.Min, AProgressBar.Max);
end;

class procedure TQtWSProgressBar.SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer);
begin
  TQtProgressBar(AProgressBar.Handle).setValue(NewPosition);
end;

{ TQtWSStatusBar }

class procedure TQtWSStatusBar.ClearPanels(const Widget: TQtStatusBar);
var
  i: integer;
begin
  if length(Widget.Panels) > 0 then
  begin
    for i := High(Widget.Panels) downto 0 do
    begin
      QStatusBar_removeWidget(QStatusBarH(Widget.Widget), Widget.Panels[i]);
      QLabel_destroy(Widget.Panels[i]);
    end;
    SetLength(Widget.Panels, 0);
  end;
end;

class procedure TQtWSStatusBar.RecreatePanels(const AStatusBar: TStatusBar;
  const Widget: TQtStatusBar);
var
  Str: WideString;
  i: Integer;
  R: TRect;
begin
  ClearPanels(Widget);
  if AStatusBar.SimplePanel then
  begin
    Str := GetUtf8String(AStatusBar.SimpleText);
    Widget.showMessage(@Str);
  end else
  if AStatusBar.Panels.Count > 0 then
  begin
    SetLength(Widget.Panels, AStatusBar.Panels.Count);
    for i := 0 to AStatusBar.Panels.Count - 1 do
    begin
      Str := GetUtf8String(AStatusBar.Panels[i].Text);
      Widget.Panels[i] := QLabel_create(@Str, Widget.Widget);
      QLabel_setAlignment(Widget.Panels[i],
        AlignmentToQtAlignmentMap[AStatusBar.Panels[i].Alignment]);
      R := Widget.getFrameGeometry;
      QWidget_setGeometry(Widget.Panels[i], 0, 0, AStatusBar.Panels[i].Width, R.Bottom);
      Widget.addWidget(Widget.Panels[i], AStatusBar.Panels[i].Width);
    end;
  end;
end;

class function TQtWSStatusBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  AStatusBar: TStatusBar absolute AWinControl;
  QtStatusBar: TQtStatusBar;
begin
  QtStatusBar := TQtStatusBar.Create(AWinControl, AParams);
  QtStatusBar.setSizeGripEnabled(TStatusBar(AWinControl).SizeGrip and TStatusBar(AWinControl).SizeGripEnabled);

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
  TQtStatusBar(AWinControl.Handle).Release;
end;

class procedure TQtWSStatusBar.PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer);
var
  QtStatusBar: TQtStatusBar;
  Str: Widestring;
  i: Integer;
begin
  QtStatusBar := TQtStatusBar(AStatusBar.Handle);
  if AStatusBar.SimplePanel then
  begin
    if Length(QtStatusBar.Panels) > 0 then
    begin
      for i := High(QtStatusBar.Panels) downto 0 do
      begin
        QStatusBar_removeWidget(QStatusBarH(QtStatusBar.Widget), QtStatusBar.Panels[i]);
        QLabel_destroy(QtStatusBar.Panels[i]);
      end;
      SetLength(QtStatusBar.Panels, 0);
    end;
      
    Str := GetUtf8String(AStatusBar.SimpleText);
    QtStatusBar.showMessage(@Str);
  end else
  if AStatusBar.Panels.Count > 0 then
  begin
    QStatusBar_clearMessage(QStatusBarH(QtStatusBar.Widget));
    
    if (PanelIndex >= Low(QtStatusBar.Panels)) and (PanelIndex <= High(QtStatusBar.Panels)) then
    begin
      Str := GetUtf8String(AStatusBar.Panels[PanelIndex].Text);
      QLabel_setText(QtStatusBar.Panels[PanelIndex], @Str);
      QLabel_setAlignment(QtStatusBar.Panels[PanelIndex],
        AlignmentToQtAlignmentMap[AStatusBar.Panels[PanelIndex].Alignment]);
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
    if (PanelIndex >= Low(QtStatusBar.Panels)) and (PanelIndex <= High(QtStatusBar.Panels)) then
    begin
      Str := GetUtf8String(AStatusBar.Panels[PanelIndex].Text);
      QLabel_setText(QtStatusBar.Panels[PanelIndex], @Str);
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
class function TQtWSCustomListView.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtTreeWidget: TQtTreeWidget;
begin
  QtTreeWidget := TQtTreeWidget.Create(AWinControl, AParams);
  QtTreeWidget.AttachEvents;
  Result := TLCLIntfHandle(QtTreeWidget);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnDelete
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ColumnDelete(const ALV: TCustomListView;
  const AIndex: Integer);
var
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnDelete') then
    Exit;
  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  TWI := QtTreeWidget.headerItem;
  QTreeWidgetItem_takeChild(TWI, AIndex);
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

  QtTreeWidget := TQtTreeWidget(ALV.Handle);

  if QtTreeWidget.ColCount <> TListView(ALV).Columns.Count then
  	QtTreeWidget.ColCount := TListView(ALV).Columns.Count;
  
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
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetAlignment') then
    Exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  TWI := QtTreeWidget.headerItem;
  QTreeWidgetItem_setTextAlignment(TWI, AIndex, AlignmentToQtAlignmentMap[AAlignment]);
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
  if not WSCheckHandleAllocated(ALV, 'ColumnSetAutoSize') then
    Exit;

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
{var
  TW: QTreeWidgetH;
  TWI: QTreeWidgetItemH;}
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetImage') then
    Exit;
{$note review}
{  TW := QTreeWidgetH(TQtTreeWidget(ALV.Handle).Widget);
  TWI := QTreeWidget_headerItem(TW);
  if Assigned(TListView(ALV).SmallImages) then
  begin
    // what to implement here ?!? SmallImages, LargeImages, StateImages ?!?
    // QTreeWidgetItem_setIcon(TWI, AIndex, QIconH ?!? -> wait for TImageList implementation ?!? );
  end;}
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
  QtItem: QTreeWidgetItemH;
  Item: TListItem;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemDelete') then
    Exit;
  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  QtTreeWidget.takeTopLevelItem(AIndex);

  {$note FIXME workaround issue #9746}
  {workaround for ListOutOfBounds in some cases. Described with issue #9746}
  QtItem := QtTreeWidget.currentItem;

  if QtItem <> nil then
  begin
    Item := ALV.Selected;
    if Assigned(Item) then
    begin
      if Item.Index <> QtTreeWidget.indexOfTopLevelItem(QtItem) then
        TListView(ALV).Items[QtTreeWidget.indexOfTopLevelItem(QtItem)].Selected := True;
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
  AState: QtCheckState;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemGetChecked') then
    Exit;

  Result := ALV.CheckBoxes;
  if not Result then
    exit;
    
  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  TWI := QtTreeWidget.topLevelItem(AIndex);
  AState := QTreeWidgetItem_checkState(TWI, 0);
  if AState = QtChecked then
  	Result := True
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemGetPosition
  Params:  None
  Returns: TPoint
 ------------------------------------------------------------------------------}
class function  TQtWSCustomListView.ItemGetPosition(const ALV: TCustomListView;
  const AIndex: Integer): TPoint;
var
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  R: TRect;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemGetPosition') then
    Exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  TWI := QtTreeWidget.topLevelItem(AIndex);
  R := QtTreeWidget.visualItemRect(TWI);
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
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemGetState') then
    Exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  TWI := QtTreeWidget.topLevelItem(AIndex);

  case AState of
    lisFocused: AIsSet := TWI = QtTreeWidget.currentItem;
    lisSelected: AIsSet := QTreeWidgetItem_isSelected(TWI);
    else
    AIsSet := False;
  end;
  
  Result := True;
  
end;
    
{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemSetChecked
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ItemSetChecked(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const AChecked: Boolean);
var
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetChecked') then
    Exit;
    
  if not ALV.CheckBoxes then
    exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  TWI := QtTreeWidget.topLevelItem(AIndex);
  if AChecked then
    QTreeWidgetItem_setCheckState(TWI, 0, QtChecked)
  else
    QTreeWidgetItem_setCheckState(TWI, 0, QtUnChecked);
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
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetState') then
    Exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  TWI := QtTreeWidget.topLevelItem(AIndex);
  case AState of
    lisFocused: QtTreeWidget.setCurrentItem(TWI);
    lisSelected: QtTreeWidget.setItemSelected(TWI, AIsSet);
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
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  Str: WideString;
  i: Integer;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemInsert') then
    Exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  TWI := QTreeWidgetItem_create(QTreeWidgetH(QtTreeWidget.Widget), 0);
  Str := GetUtf8String(AItem.Caption);

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
    Str := GetUtf8String(AItem.Subitems.Strings[i]);
    QTreeWidgetItem_setText(TWI, i+1, @Str);
  end;
  QtTreeWidget.insertTopLevelItem(AIndex, TWI);
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
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  Str: WideString;
begin
  writeln('TQtWSCustomListView.ItemSetText() AIndex=',AIndex,' ASubIndex=',ASubIndex,
   ' Text=',AText,' SubItemsCount=',AItem.SubItems.Count);
  if not WSCheckHandleAllocated(ALV, 'ItemSetText') then
    Exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  Str := GetUtf8String(AText);
  TWI := QtTreeWidget.topLevelItem(AIndex);
  if TWI <> NiL then
    QTreeWidgetItem_setText(TWI, ASubIndex, @Str);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemShow
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomListView.ItemShow(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean);
var
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemShow') then
    Exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  TWI := QtTreeWidget.topLevelItem(AIndex);
  QtTreeWidget.setItemVisible(TWI, True);
end;


{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemDisplayRect
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function  TQtWSCustomListView.ItemDisplayRect(const ALV: TCustomListView;
  const AIndex, ASubItem: Integer; ACode: TDisplayCode): TRect;
var
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemDisplayRect') then
    Exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  TWI := QtTreeWidget.topLevelItem(AIndex);
  if QTreeWidgetItem_childCount(TWI) > 0 then
    Result := QtTreeWidget.visualItemRect(QTreeWidgetItem_child(TWI, ASubItem))
  else
    Result := QtTreeWidget.visualItemRect(TWI);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.GetFocused
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomListView.GetFocused(const ALV: TCustomListView): Integer;
var
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  i: Integer;
begin
  if not WSCheckHandleAllocated(ALV, 'GetFocused') then
    Exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  TWI := QtTreeWidget.currentItem;
  i := QtTreeWidget.indexOfTopLevelItem(TWI);
  if QTreeWidgetItem_isSelected(TWI) then
    Result := i
  else
    Result := -1;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.GetItemAt
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomListView.GetItemAt(const ALV: TCustomListView; x,y: integer): Integer;
var
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
begin
  if not WSCheckHandleAllocated(ALV, 'GetItemAt') then
    Exit;
  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  TWI := QtTreeWidget.itemAt(x, y);
  Result := QtTreeWidget.indexOfTopLevelItem(TWI);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.GetSelCount
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomListView.GetSelCount(const ALV: TCustomListView): Integer;
var
  QtTreeWidget: TQtTreeWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'GetSelCount') then
    Exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  Result := QtTreeWidget.selCount;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.GetSelection
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSCustomListView.GetSelection(const ALV: TCustomListView): Integer;
var
  QtTreeWidget: TQtTreeWidget;
  FPInts: TIntArray;
begin
  if not WSCheckHandleAllocated(ALV, 'GetSelection') then
    Exit;
    
  {implement selection event so we can return Alv.Selected.Index}
  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  FPInts := QtTreeWidget.selectedItems;
  if Length(FPInts)>0 then
    Result := FPInts[0]
  else
    Result := -1;
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

  QtTreeWidget := TQtTreeWidget(ALV.Handle);

  if AType <> stNone then
  begin
    QtTreeWidget.SortEnabled := True;
    {$note for proper implementation of all TSortType
     we have to implement item sorting roles.
    if QtTreeWidget.Header.sortIndicatorOrder = QtDescendingOrder then
      QtTreeWidget.sortItems(AColumn, QtAscendingOrder)
    else
      QtTreeWidget.sortItems(AColumn, QtDescendingOrder);
     }
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
  Result := TQtTreeWidget(ALV.Handle).getFrameGeometry;
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
//  RegisterWSComponent(TToolButton, TQtWSToolButton);
//  RegisterWSComponent(TToolBar, TQtWSToolBar);
{$ifdef WSToolBar}
  RegisterWSComponent(TCustomToolButton, TQtWSToolButton);
  RegisterWSComponent(TCustomToolBar, TQtWSToolBar);
{$endif}
  RegisterWSComponent(TCustomTrackBar, TQtWSTrackBar);
//  RegisterWSComponent(TCustomTreeView, TQtWSCustomTreeView);
//  RegisterWSComponent(TCustomTreeView, TQtWSTreeView);
////////////////////////////////////////////////////
end.
