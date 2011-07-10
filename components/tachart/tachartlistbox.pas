{
 /***************************************************************************
                            TAChartListbox.pas
                            ------------------
                    Component Library Standard Graph Listbox


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

Authors: Werner Pamler, Alexander Klenin

Usage:
- Add the ChartListbox to a form with a TChart
- Connect the chart to the listbox by setting the chart property of the
  TChartlistbox. The ChartListbox will be populated with the series in the
  chart und will automatically track changes of the series.
- Check/uncheck series in the ChartListbox to show/hide them in the chart.
  As usual, checking is done by mouse clicks or by pressing the space bar.
- Play with the properties to modify the standard behavior, e.g. set
  CheckStyle to cbsRadioButton in order to get a radiobutton behavior, i.e. to
  allow only one visible series.
}

unit TAChartListbox;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, SysUtils, LCLType, StdCtrls,
  TAChartUtils, TACustomSeries, TALegend, TAGraph;

type
  TChartListboxIndexEvent = procedure(Sender:TObject; Index:integer) of object;

  TCheckBoxesStyle = (cbsCheckbox, cbsRadiobutton);

  TChartListbox = class(TCustomListbox)
  private
    FChart : TChart;
    FListener : TListener;
    FLegendItems : TChartLegendItems;
    FShowSeriesIcons : boolean;
    FShowCheckboxes : boolean;
    FCheckStyle : TCheckBoxesStyle;
    FLockCount : integer;
    FSeriesIconClicked : integer;
    FOnCheckboxClick : TChartListboxIndexEvent;
    FOnSeriesIconClick : TChartListboxIndexEvent;
    FOnItemClick : TChartListboxIndexEvent;
    FOnPopulate : TNotifyEvent;
    function  GetChecked(AIndex:integer) : boolean;
    function  GetLegendItem(AIndex:integer) : TLegendItem;
    function  GetSeries(AIndex:integer) : TCustomChartSeries;
    function  GetSeriesCount : integer;
    procedure SeriesChanged(ASender:TObject);
    procedure SetChart(AValue:TChart);
    procedure SetChecked(AIndex:integer; AValue:boolean);
    procedure SetCheckStyle(AValue:TCheckBoxesStyle);
    procedure SetShowCheckboxes(AValue:boolean);
    procedure SetShowSeriesIcons(AValue:boolean);

  protected
    procedure CalcRects(const AItemRect:TRect; out CheckboxRect,SeriesIconRect: TRect);
    procedure ClickedCheckbox(AIndex:integer); virtual;
    procedure ClickedItem(AIndex:integer); virtual;
    procedure ClickedSeriesIcon(AIndex:integer); virtual;
    function  CreateLegendItems : TChartLegendItems;
    procedure DblClick; override;
    procedure DrawItem(Index: Integer; ARect: TRect; AState: TOwnerDrawState); override;
    function  FirstCheckedIndex : integer;
    function  IsLocked : boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Lock;
    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState; AX, AY: Integer); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure Populate;
    procedure Unlock;

  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    function  FindSeriesIndex(ASeries:TCustomChartSeries) : integer;
    procedure MeasureItem(Index: Integer; var AHeight: Integer); override;
    procedure RemoveSeries(ASeries:TCustomChartSeries);
    property Checked[AIndex:integer] : boolean read GetChecked write SetChecked;
    property Series[AIndex:integer] : TCustomChartSeries read GetSeries;
    property SeriesCount : integer read GetSeriesCount;

  published
    property Chart : TChart
        read FChart write SetChart;
    property CheckStyle : TCheckBoxesStyle
        read FCheckStyle write SetCheckStyle default cbsCheckbox;
    property ShowCheckboxes : boolean
        read FShowCheckboxes write SetShowCheckboxes default true;
    property ShowSeriesIcons : boolean
        read FShowSeriesIcons write SetShowSeriesIcons default true;
    property OnCheckboxClick : TChartListboxIndexEvent
        read FOnCheckboxClick write FOnCheckboxClick;
    property OnItemClick : TChartListboxIndexEvent
        read FOnItemClick write FOnItemClick;
    property OnSeriesIconClick : TChartListboxIndexEvent
        read FOnSeriesIconClick write FOnSeriesIconClick;
    property OnPopulate : TNotifyEvent
        read FOnPopulate write FOnPopulate;

    property Align;
//    property AllowGrayed;
    property Anchors;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property Color;
    property Columns;
    property Constraints;
    property DragCursor;
    property DragMode;
    property ExtendedSelect;
    property Enabled;
    property Font;
    property IntegralHeight;
//    property Items;
    property ItemHeight;
    property MultiSelect;
    property OnChangeBounds;
    property OnClick;
//    property OnClickCheck;
    property OnContextPopup;
    property OnDblClick;
    property OnDrawItem;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
//    property OnItemClick;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnShowHint;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentBidiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
//    property Style;
    property TabOrder;
    property TabStop;
    property TopIndex;
    property Visible;
  end;

procedure Register;


implementation

uses
  math, LCLIntf, Themes, TAGeometry, TADrawUtils, TADrawerCanvas;

procedure Register;
begin
  RegisterComponents(CHART_COMPONENT_IDE_PAGE, [TChartListbox]);
end;

constructor TChartListbox.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  Style := lbOwnerDrawFixed;
  FListener := TListener.Create(@FChart, @SeriesChanged);
  FShowSeriesIcons := true;
  FShowCheckboxes := true;
end;

destructor TChartListbox.Destroy;
begin
  FreeAndNil(FListener);
  FreeAndNil(FLegendItems);
  inherited Destroy;
end;

procedure TChartListbox.CalcRects(const AItemRect:TRect;
  out CheckboxRect,SeriesIconRect: TRect);
{ based on the rect of a listbox item, calculates the locations of the
  checkbox and of the series icon }
var
  w, x: integer;
begin
  CheckBoxRect := Rect(-1, -1, -1, -1);
  SeriesIconRect := Rect(-1, -1, -1, -1);
  w := GetSystemMetrics(SM_CYMENUCHECK);
  x := 2;
  if FShowCheckboxes then begin
    CheckboxRect := Bounds(AItemRect.Left+1, AItemRect.Top+1, w, w);
    if FShowSeriesIcons then
      inc(x, CheckboxRect.Right);
  end else begin
    if FShowSeriesIcons then
      inc(x, AItemRect.Left);
  end;
  if FShowSeriesIcons then
    SeriesIconRect := Rect(x, AItemRect.Top+2, x+FChart.Legend.SymbolWidth, AItemRect.Bottom-2);
end;

procedure TChartListbox.ClickedCheckbox(AIndex:integer);
begin
  Checked[AIndex] := not Checked[AIndex];
  if Assigned(OnCheckboxClick) then
    OnCheckboxClick(self, AIndex);
end;

procedure TChartListbox.ClickedItem(AIndex:integer);
begin
  if Assigned(OnItemClick) then
    OnItemClick(self, AIndex);
end;

procedure TChartListbox.ClickedSeriesIcon(AIndex:integer);
begin
  if Assigned(OnSeriesIconClick) then
    OnSeriesIconClick(self, AIndex);
end;

function TChartListbox.CreateLegendItems : TChartLegendItems;
{ creates the a TLegendItems list and populates it with information for
  all series contained in the Chart. In case of MultiLegend items, only
  a single legend item is used }
var
  i : integer;
  j : integer = MaxInt;
begin
  result := TChartLegendItems.Create;
  try
    if FChart <> nil then
      for i:=0 to FChart.SeriesCount-1 do begin
        if (FChart.Series[i] is TCustomChartSeries) then begin
          TCustomChartSeries(FChart.Series[i]).GetSingleLegendItem(result);
          result[i].Owner := FChart.Series[i];
        end;
      end;
    for i := Result.Count - 1 downto 0 do
      if Result[i].Order = LEGEND_ITEM_ORDER_AS_ADDED then begin
        Result[i].Order := j;
        j -= 1;
      end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TChartListbox.DblClick;
begin
  inherited DblClick;
  if (FSeriesIconClicked <> -1) then
    ClickedSeriesIcon(FSeriesIconClicked);
end;

procedure TChartListbox.DrawItem(Index: Integer; ARect: TRect;
  AState: TOwnerDrawState);
{ draws the listbox item }
const
  IsChecked : array[TCheckboxesStyle, Boolean] of integer = (
    (DFCS_BUTTONCHECK, DFCS_BUTTONCHECK or DFCS_CHECKED),
    (DFCS_BUTTONRADIO, DFCS_BUTTONRADIO or DFCS_CHECKED)
  );
var
  id : IChartDrawer;
  Rcb, Ricon: TRect;
  te : TThemedElementDetails;
  x : integer;
begin
  Unused(AState);

  if FChart = nil then
    exit;

  CalcRects(ARect, Rcb, Ricon);

  if FShowCheckboxes then begin
    if ThemeServices.ThemesEnabled then begin
      case FCheckStyle of
        cbsCheckbox :
          if Checked[Index] then
            te := ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal)
          else
            te := ThemeServices.GetElementDetails(tbCheckBoxUncheckedNormal);
        cbsRadioButton :
          if Checked[Index] then
            te := ThemeServices.GetElementDetails(tbRadioButtonCheckedNormal)
          else
            te := ThemeServices.GetElementDetails(tbRadioButtonUnCheckedNormal);
      end;
      ThemeServices.DrawElement(Canvas.Handle, te, Rcb);
    end else
      DrawFrameControl(Canvas.Handle, Rcb, DFC_BUTTON, IsChecked[FCheckStyle, Checked[Index]]);
    x := Rcb.Right;
  end else
    x := ARect.Left;

  if FShowSeriesIcons then begin
    id := TCanvasDrawer.Create(Canvas);
    id.Pen := Chart.Legend.SymbolFrame;
    FLegendItems[Index].Draw(id, Ricon);
  end else
    Canvas.TextOut(x+2, ARect.Top, FLegendItems.Items[Index].Text);
end;

function TChartListbox.FindSeriesIndex(ASeries:TCustomChartSeries) : integer;
{ searches the internal legend items list for the specified series }
var
  i : integer;
  ser : TBasicChartSeries;
begin
  for i:=0 to FLegendItems.Count-1 do begin
    ser := GetSeries(i);
    if ser = ASeries then begin
      result := i;
      exit;
    end;
  end;
  result := -1;
end;

function TChartListbox.FirstCheckedIndex : integer;
{ Returns the index of the first listbox series that is active }
var
  i : integer;
begin
  for i:=0 to FLegendItems.Count-1 do
    if Checked[i] then begin
      result := i;
      exit;
    end;
  result := -1;
end;

function TChartListbox.GetChecked(AIndex:integer) : boolean;
{ report the checked status. This is determined by the visibility of the
  series with the given index. }
var
  ser : TBasicChartSeries;
begin
  ser := GetSeries(AIndex);
  result := ser.Active;
end;

function TChartListbox.GetLegendItem(AIndex:integer) : TLegendItem;
begin
  result := FLegendItems[AIndex];
end;

function TChartListbox.GetSeries(AIndex:integer) : TCustomChartSeries;
{ extracts, for the given index, the series from the internal
  legend items list. }
var
  legitem : TLegendItem;
begin
  legitem := GetLegendItem(AIndex);
  if (legitem <> nil) and (legitem.Owner is TCustomChartSeries) then
    result := TCustomChartSeries(legitem.Owner)
  else
    result := nil;
end;

function TChartListbox.GetSeriesCount : integer;
{ determines the number of series displayed in the listbox.
  Note that this may be different from the Chart.SeriesCount if
  RemoveSeries has been called }
begin
  result := FLegendItems.Count;
end;

function TChartListbox.IsLocked : boolean;
begin
  result := FLockCount <> 0;
end;

procedure TChartListBox.KeyDown(var Key: Word; Shift: TShiftState);
{ allows checking/unchecking of items by means of pressing the space bar }
begin
  if (Key = VK_SPACE) and (Shift=[]) and FShowCheckboxes then begin
    ClickedCheckbox(ItemIndex);
    Key := VK_UNKNOWN;
  end else
    inherited KeyDown(Key,Shift);
end;

procedure TChartListBox.Lock;
{ locking mechanism to avoid excessive broadcasting of chart changes.
  See also: IsLocked and UnLock }
begin
  inc(FLockCount);
end;

procedure TChartListBox.MeasureItem(Index: Integer; var AHeight: Integer);
{ inherited from ancestor: measures the height of a listbox item taking into
  account the height of the checkbox }
begin
  Unused(Index);
  AHeight := CalculateStandardItemHeight;
  if FShowCheckboxes then
    AHeight := Max(AHeight, GetSystemMetrics(SM_CYMENUCHECK) + 2);
end;

procedure TChartListbox.MouseDown(AButton: TMouseButton; AShift: TShiftState;
  AX, AY: Integer);
{ standard MouseDown handler: checks if the click occured on the checkbox,
  on the series icon, or on the text.
  The visibility state of the item's series is changed when clicking on the
  checkbox, and an event OnCheckboxClick is generated.
  An event OnSeriesIconClick is generated when double-clicking on the
  series icon; the method stores the series list index here.
  An event OnItemClick is generated when the click occured neither on the
  checkbox nor the series icon.
}
var
  Rcb, Ricon : TRect;
  index : integer;
  P : TPoint;
  done : boolean;
begin
  if (AButton = mbLeft) then begin
    P := Point(AX, AY);
    index := GetIndexAtXY(AX, AY);
    CalcRects(ItemRect(index), Rcb, Ricon);
    FSeriesIconClicked := -1;
    done := false;
    if FShowCheckboxes and IsPointInRect(P, Rcb) then begin
      ClickedCheckbox(index);
      done := true;
    end;
    if FShowSeriesIcons and PtInRect(Ricon, P) then begin
      FSeriesIconClicked := index;
      // remember the clicked index for the double click event
      done := true;
    end;
    if not done then
      ClickedItem(index);
  end;
  inherited MouseDown(AButton, AShift, AX, AY);
end;

procedure TChartListbox.Notification(AComponent: TComponent;
  AOperation: TOperation);
{ avoids AV if Chart is deleted from its container form }
begin
  if (AOperation = opRemove) and (AComponent = FChart) then
    SetChart(nil);
  inherited Notification(AComponent, AOperation);
end;

procedure TChartListbox.Populate;
{ populates the listbox with all series contained in the chart. Use the event
  OnPopulate if you don't omit special series from the listbox (RemoveSeries) }
var
  i : integer;
begin
  try
    Items.BeginUpdate;
    Items.Clear;
    if FChart <> nil then begin
      FreeAndNil(FLegendItems);
      FLegendItems := CreateLegendItems;
      for i:=0 to FLegendItems.Count-1 do
        Items.Add('');
        // the caption is owner-drawn using info from the FLegendItems
        // --> no need to pass the text here

      if Assigned(OnPopulate) then
        OnPopulate(self);
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TChartListbox.RemoveSeries(ASeries:TCustomChartSeries);
{ removes the series from the listbox, but keeps it in the chart }
var
  index : integer;
begin
  index := FindSeriesIndex(ASeries);
  if index <> -1 then begin
    FLegendItems.Delete(index);
    Items.Delete(index);
    Invalidate;
  end;
end;

procedure TChartListbox.SeriesChanged(ASender:TObject);
{ Notification procedure of the listener. Responds to chart broadcasts
  by populating the listbox with the chart's series }
var
  index : integer;
begin
  if not IsLocked then begin
    Populate;
    { in case of radiobutton mode, it is necessary to uncheck the other
      series; there can be only one active series in this mode }
    if (FCheckStyle = cbsRadioButton) and (ASender is TBasicChartSeries) then
    begin
      index := FindSeriesIndex(ASender as TCustomChartSeries);
      if (index <> -1) then
        Checked[index] := (ASender as TCustomChartSeries).Active;
    end;
  end;
end;

procedure TChartListbox.SetChart(AValue:TChart);
{ connects the ListBox to the chart }
begin
  if FChart = AValue then exit;

  if FListener.IsListening then
    FChart.Broadcaster.Unsubscribe(FListener);
  FChart := AValue;
  if FChart <> nil then
    FChart.Broadcaster.Subscribe(FListener);
  SeriesChanged(nil);
end;

procedure TChartListbox.SetChecked(AIndex:integer; AValue:boolean);
{ shows/hides the series with the specified index of its listbox item.
  In case of radiobutton style, all other series are hidden if AValue=true }
var
  ser : TBasicChartSeries;
  i : integer;
begin
  ser := GetSeries(AIndex);
  if ser <> nil then begin
    Lock;
    try
      ser.Active := AValue;
      if AValue and (FCheckStyle = cbsRadioButton) then
        for i:=0 to FLegendItems.Count-1 do begin
          ser := GetSeries(i);
          if (i <> AIndex) and (ser <> nil) then
            ser.Active := false;
        end;
      Invalidate;
    finally
      Unlock;
    end;
  end;
end;

procedure TChartlistbox.SetCheckStyle(AValue:TCheckBoxesStyle);
{ selects "checkbox" or "radiobutton" styles. In radiobutton mode, only
  one series can be visible }
var
  j : integer;
begin
  if AValue <> FCheckStyle then begin
    if AValue = cbsRadioButton then
      j := FirstCheckedIndex
    else
      j := -1;
    FCheckStyle := AValue;
    if (FCheckStyle = cbsRadioButton) and (j <> -1) then
      Checked[j] := true;
    Invalidate;
  end;
end;

procedure TChartListbox.SetShowCheckboxes(AValue:boolean);
begin
  if AValue <> FShowCheckboxes then begin
    FShowCheckboxes := AValue;
    Invalidate;
  end;
end;

procedure TChartListbox.SetShowSeriesIcons(AValue:boolean);
begin
  if AValue <> FShowSeriesIcons then begin
    FShowSeriesIcons := AValue;
    Invalidate;
  end;
end;

procedure TChartListbox.Unlock;
begin
  dec(FLockCount);
end;

end.

