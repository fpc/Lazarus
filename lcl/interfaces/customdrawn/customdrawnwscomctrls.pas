{
 *****************************************************************************
 *                           CustomDrawnWSComCtrls.pas                       *
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
unit CustomDrawnWSComCtrls;

{$mode objfpc}{$H+}

interface

//{$I qtdefines.inc}

uses
  // LCL
  SysUtils, Classes, Types, ComCtrls, Controls, LCLType, Graphics, StdCtrls,
  LCLProc, LCLIntf, Forms, ImgList,
  customdrawncontrols, customdrawn_common,
  // Widgetset
  WSProc, WSComCtrls, WSLCLClasses,
  // LCL-CustomDrawn
  customdrawnproc, customdrawnwscontrols;

type
  { TCDWSCustomPage }

  TCDWSCustomPage = class(TWSCustomPage)
  published
{    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure UpdateProperties(const ACustomPage: TCustomPage); override;}
  end;

  { TCDWSCustomTabControl }

  TCDWSCustomTabControl = class(TWSCustomTabControl)
  published
{    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;

    class procedure AddPage(const ATabControl: TCustomTabControl;
      const AChild: TCustomPage; const AIndex: integer); override;
    class procedure MovePage(const ATabControl: TCustomTabControl;
      const AChild: TCustomPage; const NewIndex: integer); override;
    class procedure RemovePage(const ATabControl: TCustomTabControl;
      const AIndex: integer); override;

    class function GetCapabilities: TCTabControlCapabilities; override;
    class function GetDesignInteractive(const AWinControl: TWinControl; AClientPos: TPoint): Boolean; override;
    class function GetTabIndexAtPos(const ATabControl: TCustomTabControl; const AClientPos: TPoint): integer; override;
    class function GetTabRect(const ATabControl: TCustomTabControl; const AIndex: Integer): TRect; override;
    class procedure SetPageIndex(const ATabControl: TCustomTabControl; const AIndex: integer); override;
    class procedure SetTabCaption(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const AText: string); override;
    class procedure SetTabPosition(const ATabControl: TCustomTabControl; const ATabPosition: TTabPosition); override;
    class procedure ShowTabs(const ATabControl: TCustomTabControl; AShowTabs: boolean); override;
    class procedure UpdateProperties(const ATabControl: TCustomTabControl); override;    }
  end;

  { TCDWSStatusBar }

  TCDWSStatusBar = class(TWSStatusBar)
  published
{    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure SetSizeGrip(const AStatusBar: TStatusBar; SizeGrip: Boolean); override;
    class procedure Update(const AStatusBar: TStatusBar); override;}
  end;

  { TCDWSTabSheet }

  TCDWSTabSheet = class(TWSTabSheet)
  published
  end;

  { TCDWSPageControl }

  TCDWSPageControl = class(TWSPageControl)
  published
  end;

  { TCDWSCustomListView }

  TCDWSCustomListView = class(TWSCustomListView)
  published
{    class function CreateHandle(const AWinControl: TWinControl;
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
    class procedure ItemExchange(const ALV: TCustomListView; AItem: TListItem; const AIndex1, AIndex2: Integer); override;
    class procedure ItemMove(const ALV: TCustomListView; AItem: TListItem; const AFromIndex, AToIndex: Integer); override;
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
    class procedure SetSort(const ALV: TCustomListView; const AType: TSortType; const AColumn: Integer;
      const ASortDirection: TSortDirection); override;

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
           }
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

  { TCDWSListView }

  TCDWSListView = class(TWSListView)
  published
  end;

  { TCDWSProgressBar }

  TCDWSProgressBar = class(TWSProgressBar)
  public
    class procedure CreateCDControl(const AWinControl: TWinControl; var ACDControlField: TCDControl);
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
    class procedure ApplyChanges(const AProgressBar: TCustomProgressBar); override;
    class procedure SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer); override;
//    class procedure SetStyle(const AProgressBar: TCustomProgressBar; const NewStyle: TProgressBarStyle); override;
  end;

  { TCDWSCustomUpDown }

  TCDWSCustomUpDown = class(TWSCustomUpDown)
  published
  end;

  { TCDWSUpDown }

  TCDWSUpDown = class(TWSUpDown)
  published
  end;

  { TCDWSToolButton }

  TCDWSToolButton = class(TWSToolButton)
  published
  end;

  { TCDWSToolBar }

  TCDWSToolBar = class(TWSToolBar)
  published
//    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCDWSTrackBar }

  TCDWSTrackBar = class(TWSTrackBar)
  published
{    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ApplyChanges(const ATrackBar: TCustomTrackBar); override;
    class function  GetPosition(const ATrackBar: TCustomTrackBar): integer; override;
    class procedure SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer); override;
    class procedure SetOrientation(const ATrackBar: TCustomTrackBar; const AOrientation: TTrackBarOrientation); override;}
  end;

  { TCDWSCustomTreeView }

  TCDWSCustomTreeView = class(TWSCustomTreeView)
  published
  end;

  { TCDWSTreeView }

  TCDWSTreeView = class(TWSTreeView)
  published
  end;


implementation

(*{ TCDWSToolBar }

class function TCDWSToolBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
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

{ TCDWSTrackBar }

class function TCDWSTrackBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  QtTrackBar: TQtTrackBar;
begin
  QtTrackBar := TQtTrackBar.Create(AWinControl, AParams);
  QtTrackBar.AttachEvents;

  Result := TLCLIntfHandle(QtTrackBar);
end;

function TrackBarReversed(const ATrackBar: TCustomTrackBar;
  const AQtTrackBar: TQtTrackBar): Boolean;
begin
  Result :=
    ((ATrackBar.Orientation = trHorizontal) and
    (AQtTrackbar.getInvertedAppereance <> ATrackBar.Reversed))
    or
    ((ATrackBar.Orientation = trVertical) and
    (AQtTrackbar.getInvertedAppereance <> not ATrackBar.Reversed))
end;

class procedure TCDWSTrackBar.ApplyChanges(const ATrackBar: TCustomTrackBar);
var
  QtTrackBar: TQtTrackBar;
begin

  if not WSCheckHandleAllocated(ATrackBar, 'ApplyChanges') then
    Exit;

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
      or TrackBarReversed(ATrackBar, QtTrackBar) then
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

class function  TCDWSTrackBar.GetPosition(const ATrackBar: TCustomTrackBar): integer;
var
  QtTrackBar: TQtTrackBar;
begin
  Result := 0;
  if not WSCheckHandleAllocated(ATrackBar, 'GetPosition') then
    Exit;
  QtTrackBar := TQtTrackBar(ATrackBar.Handle);
  Result := QtTrackBar.getSliderPosition;
end;

class procedure TCDWSTrackBar.SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer);
var
  QtTrackBar: TQtTrackBar;
begin
  if not WSCheckHandleAllocated(ATrackBar, 'SetPosition') then
    Exit;
  QtTrackBar := TQtTrackBar(ATrackBar.Handle);
  QtTrackBar.BeginUpdate;
  try
    QtTrackBar.setSliderPosition(NewPosition);
  finally
    QtTrackBar.EndUpdate;
  end;
end;

class procedure TCDWSTrackBar.SetOrientation(const ATrackBar: TCustomTrackBar;
  const AOrientation: TTrackBarOrientation);
var
  QtTrackBar: TQtTrackBar;
begin
  if not WSCheckHandleAllocated(ATrackBar, 'SetOrientation') then
    Exit;
  QtTrackBar := TQtTrackBar(ATrackBar.Handle);
  QtTrackBar.BeginUpdate;
  try
    if (QtTrackBar.getOrientation <>
      TrackBarOrientationToQtOrientationMap[ATrackBar.Orientation])
      or TrackBarReversed(ATrackBar, QtTrackBar) then
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
end;*)

{ TCDWSProgressBar }

class procedure TCDWSProgressBar.CreateCDControl(const AWinControl: TWinControl; var ACDControlField: TCDControl);
begin
  ACDControlField := TCDProgressBar.Create(AWinControl);
//    TCDIntfButton(lCDWinControl.CDControl).LCLButton := TButton(AWinControl);
  ACDControlField.Parent := AWinControl;
  ACDControlField.Align := alClient;
end;

class function TCDWSProgressBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  lCDWinControl: TCDWinControl;
begin
  Result := TCDWSWinControl.CreateHandle(AWinControl, AParams);
  lCDWinControl := TCDWinControl(Result);
end;

class procedure TCDWSProgressBar.ShowHide(const AWinControl: TWinControl);
var
  lCDWinControl: TCDWinControl;
begin
  lCDWinControl := TCDWinControl(AWinControl.Handle);

  TCDWSWinControl.ShowHide(AWinControl);

  if lCDWinControl.CDControl = nil then
    CreateCDControl(AWinControl, lCDWinControl.CDControl);
end;

class procedure TCDWSProgressBar.ApplyChanges(const AProgressBar: TCustomProgressBar);
var
  lCDWinControl: TCDWinControl;
begin
  lCDWinControl := TCDWinControl(AProgressBar.Handle);

  if lCDWinControl.CDControl = nil then
    CreateCDControl(AProgressBar, lCDWinControl.CDControl);

  TCDProgressBar(lCDWinControl.CDControl).Position := AProgressBar.Position;

{  // AProgressBar.Smooth is not supported by qt

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
  QtProgressBar.EndUpdate;}
end;

class procedure TCDWSProgressBar.SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer);
var
  lCDWinControl: TCDWinControl;
begin
  lCDWinControl := TCDWinControl(AProgressBar.Handle);

  if lCDWinControl.CDControl = nil then
    CreateCDControl(AProgressBar, lCDWinControl.CDControl);

  TCDProgressBar(lCDWinControl.CDControl).Position := NewPosition;
end;

(*class procedure TCDWSProgressBar.SetStyle(
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

{ TCDWSStatusBar }

class procedure TCDWSStatusBar.ClearPanels(const Widget: TQtStatusBar);
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

class procedure TCDWSStatusBar.RecreatePanels(const AStatusBar: TStatusBar;
  const Widget: TQtStatusBar);
var
  Str: WideString;
  i: Integer;
begin
  Str := '';
  //clean up. http://bugs.freepascal.org/view.php?id=18683
  Widget.showMessage(@Str);
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

class function TCDWSStatusBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
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

class procedure TCDWSStatusBar.DestroyHandle(const AWinControl: TWinControl);
var
  QtStatusBar: TQtStatusBar;
begin
  QtStatusBar := TQtStatusBar(AWinControl.Handle);
  ClearPanels(QtStatusBar);
  QtStatusBar.Release;
end;

class procedure TCDWSStatusBar.PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer);
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

class procedure TCDWSStatusBar.SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer);
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

class procedure TCDWSStatusBar.SetSizeGrip(const AStatusBar: TStatusBar;
  SizeGrip: Boolean);
var
  QtStatusBar: TQtStatusBar;
begin
  if not WSCheckHandleAllocated(AStatusBar, 'SetSizeGrip') then
    Exit;
  QtStatusBar := TQtStatusBar(AStatusBar.Handle);
  QtStatusBar.setSizeGripEnabled(SizeGrip and AStatusBar.SizeGripEnabled);
end;

class procedure TCDWSStatusBar.Update(const AStatusBar: TStatusBar);
var
  QtStatusBar: TQtStatusBar;
begin
  QtStatusBar := TQtStatusBar(AStatusBar.Handle);
  RecreatePanels(AStatusBar, QtStatusBar);
end;

{ TCDWSCustomListView }

{------------------------------------------------------------------------------
  Method: TCDWSCustomListView.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}

class function TCDWSCustomListView.IsIconView(const AList: TCustomListView): boolean;
begin
  Result := TListView(AList).ViewStyle <> vsReport;
end;

class function TCDWSCustomListView.CreateHandle(const AWinControl: TWinControl;
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
  Method: TCDWSCustomListView.ColumnDelete
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomListView.ColumnDelete(const ALV: TCustomListView;
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
  Method: TCDWSCustomListView.ColumnInsert
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomListView.ColumnInsert(const ALV: TCustomListView;
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
  Method: TCDWSCustomListView.ColumnGetWidth
  Params:  None
  Returns: Integer
 ------------------------------------------------------------------------------}
class function  TCDWSCustomListView.ColumnGetWidth(const ALV: TCustomListView;
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
  Method: TCDWSCustomListView.ColumnMove
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomListView.ColumnMove(const ALV: TCustomListView;
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
  Method: TCDWSCustomListView.ColumnSetAlignment
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomListView.ColumnSetAlignment(const ALV: TCustomListView;
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
  Method: TCDWSCustomListView.ColumnSetAutoSize
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomListView.ColumnSetAutoSize(const ALV: TCustomListView;
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
  Method: TCDWSCustomListView.ColumnSetCaption
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomListView.ColumnSetCaption(const ALV: TCustomListView;
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
  Method: TCDWSCustomListView.ColumnSetImage
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomListView.ColumnSetImage(const ALV: TCustomListView;
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
  Method: TCDWSCustomListView.ColumnSetMinWidth
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomListView.ColumnSetMinWidth(const ALV: TCustomListView;
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
  Method: TCDWSCustomListView.ColumnSetWidth
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomListView.ColumnSetWidth(const ALV: TCustomListView;
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
  Method: TCDWSCustomListView.ColumnSetVisible
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomListView.ColumnSetVisible(const ALV: TCustomListView;
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
  Method: TCDWSCustomListView.ItemDelete
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomListView.ItemDelete(const ALV: TCustomListView;
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

class procedure TCDWSCustomListView.ItemExchange(const ALV: TCustomListView;
  AItem: TListItem; const AIndex1, AIndex2: Integer);
var
  QtTreeWidget: TQtTreeWidget;
  QtListWidget: TQtListWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemExchange') then
    Exit;
  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    QtListWidget.BeginUpdate;
    QtListWidget.ExchangeItems(AIndex1, AIndex2);
    QtListWidget.EndUpdate;
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    QtTreeWidget.BeginUpdate;
    QtTreeWidget.ExchangeItems(AIndex1, AIndex2);
    QtTreeWidget.EndUpdate;
  end;
end;

class procedure TCDWSCustomListView.ItemMove(const ALV: TCustomListView;
  AItem: TListItem; const AFromIndex, AToIndex: Integer);
var
  QtTreeWidget: TQtTreeWidget;
  QtListWidget: TQtListWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemMove') then
    Exit;
  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    QtListWidget.BeginUpdate;
    QtListWidget.MoveItem(AFromIndex, AToIndex);
    QtListWidget.EndUpdate;
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    QtTreeWidget.BeginUpdate;
    QtTreeWidget.MoveItem(AFromIndex, AToIndex);
    QtTreeWidget.EndUpdate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomListView.ItemGetChecked
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function  TCDWSCustomListView.ItemGetChecked(const ALV: TCustomListView;
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
  Method: TCDWSCustomListView.ItemGetPosition
  Params:  None
  Returns: TPoint
 ------------------------------------------------------------------------------}
class function  TCDWSCustomListView.ItemGetPosition(const ALV: TCustomListView;
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
  Method: TCDWSCustomListView.ItemGetState
  Params:  None
  Returns: TPoint
 ------------------------------------------------------------------------------}
class function  TCDWSCustomListView.ItemGetState(const ALV: TCustomListView;
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

class procedure TCDWSCustomListView.ItemSetImage(const ALV: TCustomListView;
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
  Method: TCDWSCustomListView.ItemSetChecked
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomListView.ItemSetChecked(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const AChecked: Boolean);
var
  QtListWidget: TQtListWidget;
  QtTreeWidget: TQtTreeWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetChecked') then
    Exit;

  if not ALV.CheckBoxes then
    exit;

  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    QtListWidget.ItemChecked[AIndex] := AChecked;
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    QtTreeWidget.ItemChecked[AIndex] := AChecked;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomListView.ItemSetState
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomListView.ItemSetState(const ALV: TCustomListView;
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
    QtListWidget.BeginUpdate;
    case AState of
      lisFocused: QtListWidget.setCurrentItem(LWI);
      lisSelected:
      begin
        if AIsSet and not ALV.MultiSelect then
          QtListWidget.setCurrentItem(LWI);
        QtListWidget.setItemSelected(LWI, AIsSet);
      end;
    end;
    QtListWidget.EndUpdate;
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
  Method: TCDWSCustomListView.ItemInsert
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomListView.ItemInsert(const ALV: TCustomListView;
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

    QtTreeWidget.setItemData(TWI, 0, AItem);

    for i := 0 to AItem.SubItems.Count - 1 do
    begin
      AAlignment := QtAlignLeft;
      if (TListView(ALV).Columns.Count > 0) and (i + 1 < TListView(ALV).Columns.Count) then
        AAlignment := AlignmentToQtAlignmentMap[ALV.Column[i + 1].Alignment];
      if AItem.Subitems.Strings[i] <> '' then
      begin
        Str := GetUtf8String(AItem.Subitems.Strings[i]);
        QtTreeWidget.setItemText(TWI, i + 1, Str, AAlignment);
        QtTreeWidget.setItemData(TWI, i + 1, AItem);
      end;
    end;
    QtTreeWidget.insertTopLevelItem(AIndex, TWI);
  end;
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomListView.ItemSetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomListView.ItemSetText(const ALV: TCustomListView;
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
  Method: TCDWSCustomListView.ItemShow
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomListView.ItemShow(const ALV: TCustomListView;
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
  Method: TCDWSCustomListView.ItemDisplayRect
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function  TCDWSCustomListView.ItemDisplayRect(const ALV: TCustomListView;
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

class procedure TCDWSCustomListView.BeginUpdate(const ALV: TCustomListView);
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

class procedure TCDWSCustomListView.EndUpdate(const ALV: TCustomListView);
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
  Method: TCDWSCustomListView.GetFocused
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TCDWSCustomListView.GetFocused(const ALV: TCustomListView): Integer;
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
  Method: TCDWSCustomListView.GetItemAt
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TCDWSCustomListView.GetItemAt(const ALV: TCustomListView; x,y: integer): Integer;
var
  QtListWidget: TQtListWidget;
  LWI: QListWidgetItemH;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  AOrientation: QtOrientation;
  HeaderOffset: Integer;
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
    HeaderOffset := QtTreeWidget.getHeaderHeight(AOrientation);

    HeaderOffset := y - HeaderOffset;
    if HeaderOffset < 0 then
      HeaderOffset := 0;
    TWI := QtTreeWidget.itemAt(x, HeaderOffset);
    Result := QtTreeWidget.getRow(TWI);
  end;
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomListView.GetSelCount
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TCDWSCustomListView.GetSelCount(const ALV: TCustomListView): Integer;
begin
  if not WSCheckHandleAllocated(ALV, 'GetSelCount') then
    Exit;
  if IsIconView(ALV) then
    Result := TQtListWidget(ALV.Handle).getSelCount
  else
    Result := TQtTreeWidget(ALV.Handle).selCount;
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomListView.GetSelection
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TCDWSCustomListView.GetSelection(const ALV: TCustomListView): Integer;
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

class function TCDWSCustomListView.GetTopItem(const ALV: TCustomListView
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
  Method: TCDWSCustomListView.InternalUpdateItems
  Params:  TCustomListView
  Returns: Nothing
  Sync TCustomListView with QTreeWidget items.
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomListView.InternalUpdateItems(
  const AList: TCustomListView);
var
  QtTreeWidget: TQtTreeWidget;
  i: Integer;
  j: Integer;
  AItem: TListItem;
  WStr: WideString;
  Item: QTreeWidgetItemH;
  AAlignment: QtAlignment;
  ImgList: TImageList;
  Bmp: TBitmap;
begin
  QtTreeWidget := TQtTreeWidget(AList.Handle);
  ImgList := TImageList.Create(nil);

  if (TListView(AList).ViewStyle = vsIcon) and
    Assigned(TListView(AList).LargeImages) then
    ImgList.Assign(TListView(AList).LargeImages);

  if (TListView(AList).ViewStyle in [vsSmallIcon, vsReport, vsList]) and
    Assigned(TListView(AList).SmallImages) then
    ImgList.Assign(TListView(AList).SmallImages);

  BeginUpdate(AList);
  try
    for i := 0 to AList.Items.Count - 1 do
    begin
      AItem := AList.Items[i];
      WStr := GetUTF8String(AItem.Caption);
      Item := QtTreeWidget.topLevelItem(i);
      QtTreeWidget.setItemText(Item, 0, WStr, AlignmentToQtAlignmentMap[AList.Column[0].Alignment]);
      QtTreeWidget.setItemData(Item, 0, AItem);
      if AList.Checkboxes then
      begin
        if AItem.Checked then
          QTreeWidgetItem_setCheckState(Item, 0, QtChecked)
        else
          QTreeWidgetItem_setCheckState(Item, 0, QtUnChecked);
      end;

      if (ImgList.Count > 0) and
        ((AItem.ImageIndex >= 0) and (AItem.ImageIndex < ImgList.Count)) then
      begin
        Bmp := TBitmap.Create;
        try
          ImgList.GetBitmap(AItem.ImageIndex, Bmp);
          QTreeWidgetItem_setIcon(Item, 0, TQtImage(Bmp.Handle).AsIcon);
        finally
          Bmp.Free;
        end;
      end;

      // subitems
      for j := 0 to AItem.SubItems.Count - 1 do
      begin
        AAlignment := QtAlignLeft;
        if (TListView(AList).Columns.Count > 0) and (j + 1 < TListView(AList).Columns.Count) then
          AAlignment := AlignmentToQtAlignmentMap[TListView(AList).Column[j + 1].Alignment];
        WStr := GetUtf8String(AItem.Subitems.Strings[j]);
        QtTreeWidget.setItemText(Item, j + 1, WStr, AAlignment);
        QtTreeWidget.setItemData(Item, j + 1, AItem);
      end;
    end;

  finally
    ImgList.Free;
    EndUpdate(AList);
  end;
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomListView.SetSort
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomListView.SetSort(const ALV: TCustomListView;
  const AType: TSortType; const AColumn: Integer; const ASortDirection: TSortDirection);
var
  QtTreeWidget: TQtTreeWidget;
  {$IFDEF TEST_QT_SORTING}
  StdModel: QStandardItemModelH;
  {$ELSE}
  CanSort: Boolean;
  {$ENDIF}
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
    {$IFDEF TEST_QT_SORTING}
    // QTreeWidget crashes sometimes on changing sort role (possible qt bug).
    // need deeper investigation.
    if QtTreeWidget.ItemCount > 0 then
    begin
      StdModel := QStandardItemModelH(QtTreeWidget.getModel);
      if QStandardItemModel_sortRole(StdModel) <> Ord(QtUserRole) then
        QStandardItemModel_setSortRole(StdModel, Ord(QtUserRole));
    end;
    {$ELSE}
    with QtTreeWidget do
    begin
      CanSort := ItemCount > 0;
      Header.SetSortIndicatorVisible(True);
      if (AColumn >= 0) and (AColumn < ColCount) and
        CanSort then
      begin
        Header.SetSortIndicator(AColumn, QtSortOrder(Ord(ASortDirection)));
        InternalUpdateItems(ALV);
      end;
    end;
    {$ENDIF}
  end;
end;


{------------------------------------------------------------------------------
  Method: TCDWSCustomListView.GetBoundingRect
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TCDWSCustomListView.GetBoundingRect(const ALV: TCustomListView): TRect;
begin
  if not WSCheckHandleAllocated(ALV, 'GetBoundingRect') then
    Exit;
  Result := TQtWidget(ALV.Handle).getFrameGeometry;
end;

class function TCDWSCustomListView.GetViewOrigin(const ALV: TCustomListView
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

class function TCDWSCustomListView.GetVisibleRowCount(const ALV: TCustomListView
  ): Integer;
begin
  Result := 0;
  if not WSCheckHandleAllocated(ALV, 'GetVisibleRowCount') then
    Exit;
  Result := TQtAbstractItemView(ALV.Handle).getVisibleRowCount;
end;

class procedure TCDWSCustomListView.SetAllocBy(const ALV: TCustomListView;
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

class procedure TCDWSCustomListView.SetIconArrangement(
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

class procedure TCDWSCustomListView.SetItemsCount(const ALV: TCustomListView;
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

class procedure TCDWSCustomListView.SetOwnerData(const ALV: TCustomListView;
  const AValue: Boolean);
var
  QtItemView: TQtAbstractItemView;
begin
  if not WSCheckHandleAllocated(ALV, 'SetOwnerData') then
    Exit;
  QtItemView := TQtAbstractItemView(ALV.Handle);
  QtItemView.OwnerData := AValue;
end;

class procedure TCDWSCustomListView.SetProperty(const ALV: TCustomListView;
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

class procedure TCDWSCustomListView.SetProperties(const ALV: TCustomListView;
  const AProps: TListViewProperties);
var
  i: TListViewProperty;
begin
  if not WSCheckHandleAllocated(ALV, 'SetProperties')
  then Exit;
  for i := Low(TListViewProperty) to High(TListViewProperty) do
    SetProperty(ALV, i, i in AProps);
end;

class procedure TCDWSCustomListView.SetScrollBars(const ALV: TCustomListView;
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

class procedure TCDWSCustomListView.SetViewStyle(const ALV: TCustomListView;
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
end;*)

end.
