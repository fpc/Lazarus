{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Werner Pamler, Alexander Klenin

  Usage:
  - Add the ChartListbox to a form with a TChart
  - Connect the chart to the listbox by setting the chart property of the
    TChartlistbox. The ChartListbox will be populated with the series in the
    chart and will automatically track changes of the series.
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
  Classes, Controls, StdCtrls,
  TAChartUtils, TACustomSeries, TALegend, TAGraph;

type
  TChartListbox = class;

  TChartListboxIndexEvent = procedure (
    ASender: TObject; AIndex: Integer) of object;

  TChartListboxAddSeriesEvent = procedure (
    ASender: TChartListbox; ASeries: TCustomChartSeries;
    AItems: TChartLegendItems; var ASkip: Boolean
  ) of object;

  TCheckBoxesStyle = (cbsCheckbox, cbsRadiobutton);

  TChartListOption = (cloShowCheckboxes, cloShowIcons, cloRefreshOnSourceChange);
  TChartListOptions = set of TChartListOption;

const
  SHOW_ALL = [cloShowCheckboxes, cloShowIcons];

type
  TChartListbox = class(TCustomListbox)
  private
    FChart: TChart;
    FCheckStyle: TCheckBoxesStyle;
    FLegendItems: TChartLegendItems;
    FListener: TListener;
    FOnAddSeries: TChartListboxAddSeriesEvent;
    FOnCheckboxClick: TChartListboxIndexEvent;
    FOnItemClick: TChartListboxIndexEvent;
    FOnPopulate: TNotifyEvent;
    FOnSeriesIconDblClick: TChartListboxIndexEvent;
    FOptions: TChartListOptions;
    FSeriesIconClicked: Integer;
    function GetChecked(AIndex: Integer): Boolean;
    function GetSeries(AIndex: Integer): TCustomChartSeries;
    function GetSeriesCount: Integer;
    procedure EnsureSingleChecked(AIndex: Integer = -1);
    procedure SetChart(AValue: TChart);
    procedure SetChecked(AIndex: Integer; AValue: Boolean);
    procedure SetCheckStyle(AValue: TCheckBoxesStyle);
    procedure SetOnAddSeries(AValue: TChartListboxAddSeriesEvent);
    procedure SetOnPopulate(AValue: TNotifyEvent);
    procedure SetOptions(AValue: TChartListOptions);

  protected
    procedure DblClick; override;
    procedure DrawItem(
      AIndex: Integer; ARect: TRect; AState: TOwnerDrawState); override;
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
    procedure MouseDown(
      AButton: TMouseButton; AShift: TShiftState; AX, AY: Integer); override;
  protected
    procedure CalcRects(
      const AItemRect: TRect; out ACheckboxRect, ASeriesIconRect: TRect);
    procedure ClickedCheckbox(AIndex: Integer); virtual;
    procedure ClickedItem(AIndex: Integer); virtual;
    procedure ClickedSeriesIcon(AIndex: Integer); virtual;
    function CreateLegendItems: TChartLegendItems;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure Populate;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindSeriesIndex(ASeries: TCustomChartSeries): Integer;
    procedure MeasureItem(AIndex: Integer; var AHeight: Integer); override;
    procedure RemoveSeries(ASeries: TCustomChartSeries);
    procedure SeriesChanged(ASender: TObject);

    property Checked[AIndex: Integer]: Boolean read GetChecked write SetChecked;
    property Series[AIndex: Integer]: TCustomChartSeries read GetSeries;
    property SeriesCount: Integer read GetSeriesCount;

  published
    property Chart: TChart read FChart write SetChart;
    property CheckStyle: TCheckBoxesStyle
      read FCheckStyle write SetCheckStyle default cbsCheckbox;
    property Options: TChartListOptions
      read FOptions write SetOptions default SHOW_ALL;
  published
    property OnAddSeries: TChartListboxAddSeriesEvent
      read FOnAddSeries write SetOnAddSeries;
    property OnCheckboxClick: TChartListboxIndexEvent
      read FOnCheckboxClick write FOnCheckboxClick;
    property OnItemClick: TChartListboxIndexEvent
      read FOnItemClick write FOnItemClick;
    property OnPopulate: TNotifyEvent read FOnPopulate write SetOnPopulate;
    property OnSeriesIconDblClick: TChartListboxIndexEvent
      read FOnSeriesIconDblClick write FOnSeriesIconDblClick;
  published
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
    property Enabled;
    property ExtendedSelect;
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
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
//    property OnItemClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
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
  Graphics, Math, LCLIntf, LCLType, SysUtils, Themes,
  TACustomSource, TADrawerCanvas, TADrawUtils, TAEnumerators, TAGeometry;

procedure Register;
begin
  RegisterComponents(CHART_COMPONENT_IDE_PAGE, [TChartListbox]);
end;

{ TChartListbox }

constructor TChartListbox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := lbOwnerDrawFixed;
  FListener := TListener.Create(@FChart, @SeriesChanged);
  FOptions := SHOW_ALL;
end;

destructor TChartListbox.Destroy;
begin
  FreeAndNil(FListener);
  FreeAndNil(FLegendItems);
  inherited;
end;

procedure TChartListbox.CalcRects(
  const AItemRect: TRect; out ACheckboxRect, ASeriesIconRect: TRect);
{ based on the rect of a listbox item, calculates the locations of the
  checkbox and of the series icon }
var
  w, x: Integer;
begin
  ACheckBoxRect := ZeroRect;
  ASeriesIconRect := ZeroRect;

  w := Canvas.TextHeight('Tg');
  x := 4;
  if cloShowCheckboxes in Options then begin
    with AItemRect do
      ACheckboxRect := Bounds(Left + 4, (Top + Bottom - w) div 2, w, w);
    if cloShowIcons in Options then
      x += ACheckboxRect.Right;
  end
  else begin
    if cloShowIcons in Options then
      x += AItemRect.Left;
  end;
  if cloShowIcons in Options then
    ASeriesIconRect := Rect(
      x, AItemRect.Top + 2, x + FChart.Legend.SymbolWidth, AItemRect.Bottom - 2);
end;

procedure TChartListbox.ClickedCheckbox(AIndex: Integer);
begin
  Checked[AIndex] := not Checked[AIndex];
  if Assigned(OnCheckboxClick) then
    OnCheckboxClick(Self, AIndex);
end;

procedure TChartListbox.ClickedItem(AIndex: Integer);
begin
  if Assigned(OnItemClick) then
    OnItemClick(Self, AIndex);
end;

procedure TChartListbox.ClickedSeriesIcon(AIndex: Integer);
begin
  if Assigned(OnSeriesIconDblClick) then
    OnSeriesIconDblClick(Self, AIndex);
end;

function TChartListbox.CreateLegendItems: TChartLegendItems;
{ creates the a TLegendItems list and populates it with information for
  all series contained in the Chart. In case of MultiLegend items, only
  a single legend item is used }
var
  skip: Boolean;
  s: TCustomChartSeries;
begin
  Result := TChartLegendItems.Create;
  try
    if FChart = nil then exit;
    for s in CustomSeries(Chart) do begin
      if Assigned(OnAddSeries) then begin
        skip := false;
        FOnAddSeries(Self, s, Result, skip);
        if skip then continue;
      end;
      s.GetSingleLegendItem(Result);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TChartListbox.DblClick;
begin
  inherited DblClick;
  if FSeriesIconClicked <> -1 then
    ClickedSeriesIcon(FSeriesIconClicked);
end;

procedure TChartListbox.DrawItem(
  AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
{ draws the listbox item }
const
  THEMED_FLAGS: array [TCheckboxesStyle, Boolean] of TThemedButton = (
    (tbCheckBoxUncheckedNormal, tbCheckBoxCheckedNormal),
    (tbRadioButtonUnCheckedNormal, tbRadioButtonCheckedNormal)
  );
var
  id: IChartDrawer;
  rcb, ricon: TRect;
  te: TThemedElementDetails;
  x: Integer;
  ch: Boolean;
begin
  if Assigned(OnDrawItem) then begin
    OnDrawItem(Self, AIndex, ARect, AState);
    exit;
  end;
  if (FChart = nil) or not InRange(AIndex, 0, Count - 1) then
    exit;

  Canvas.FillRect(ARect);
  CalcRects(ARect, rcb, ricon);

  if cloShowCheckboxes in Options then begin
    ch := Checked[AIndex];
    te := ThemeServices.GetElementDetails(THEMED_FLAGS[FCheckStyle, ch]);
    ThemeServices.DrawElement(Canvas.Handle, te, rcb);
    x := rcb.Right;
  end
  else
    x := ARect.Left;

  Canvas.Brush.Style := bsClear;
  if cloShowIcons in Options then begin
    id := TCanvasDrawer.Create(Canvas);
    id.Pen := Chart.Legend.SymbolFrame;
    FLegendItems[AIndex].Draw(id, ricon);
  end
  else
    Canvas.TextOut(x + 2, ARect.Top, FLegendItems.Items[AIndex].Text);
end;

procedure TChartListbox.EnsureSingleChecked(AIndex: Integer);
var
  i: Integer;
  ser: TCustomChartSeries;
begin
  if (FCheckStyle <> cbsRadioButton) or not (cloShowCheckboxes in Options) then
    exit;
  if FLegendItems = nil then
    exit;

  FListener.OnNotify := nil;
  try
    for i := 0 to FLegendItems.Count - 1 do begin
      ser := GetSeries(i);
      if ser = nil then continue;
      if (AIndex < 0) and ser.Active then
        AIndex := i
      else
        ser.Active := AIndex = i;
    end;
  finally
    FListener.OnNotify := @SeriesChanged;
  end;
end;

function TChartListbox.FindSeriesIndex(ASeries: TCustomChartSeries): Integer;
{ searches the internal legend items list for the specified series }
begin
  for Result := 0 to FLegendItems.Count - 1 do
    if GetSeries(Result) = ASeries then exit;
  Result := -1;
end;

function TChartListbox.GetChecked(AIndex: Integer): Boolean;
{ report the checked status. This is determined by the visibility of the
  series with the given index. }
var
  ser: TBasicChartSeries;
begin
  ser := GetSeries(AIndex);
  Result := (ser <> nil) and ser.Active;
end;

function TChartListbox.GetSeries(AIndex: Integer): TCustomChartSeries;
{ extracts, for the given index, the series from the internal
  legend items list. }
var
  legitem: TLegendItem;
begin
  legitem := FLegendItems[AIndex];
  if (legitem <> nil) and (legitem.Owner is TCustomChartSeries) then
    Result := TCustomChartSeries(legitem.Owner)
  else
    Result := nil;
end;

function TChartListbox.GetSeriesCount : Integer;
{ determines the number of series displayed in the listbox.
  Note that this may be different from the Chart.SeriesCount if
  RemoveSeries has been called }
begin
  Result := FLegendItems.Count;
end;

procedure TChartListbox.KeyDown(var AKey: Word; AShift: TShiftState);
{ allows checking/unchecking of items by means of pressing the space bar }
begin
  if
    (AKey = VK_SPACE) and (AShift = []) and
    (cloShowCheckboxes in Options) and (ItemIndex >= 0)
  then begin
    ClickedCheckbox(ItemIndex);
    AKey := VK_UNKNOWN;
  end
  else
    inherited KeyDown(AKey, AShift);
end;

// Item height is determined as maximum of:
// checkbox height, text height, ItemHeight property value.
procedure TChartListbox.MeasureItem(AIndex: Integer; var AHeight: Integer);
begin
  inherited MeasureItem(AIndex, AHeight);
  AHeight := Max(CalculateStandardItemHeight, AHeight);
  if cloShowCheckboxes in Options then
    AHeight := Max(AHeight, GetSystemMetrics(SM_CYMENUCHECK) + 2);
end;

procedure TChartListbox.MouseDown(
  AButton: TMouseButton; AShift: TShiftState; AX, AY: Integer);
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
  rcb, ricon: TRect;
  index: Integer;
  p: TPoint;
begin
  FSeriesIconClicked := -1;
  try
    if AButton <> mbLeft then exit;
    p := Point(AX, AY);
    index := GetIndexAtXY(AX, AY);
    if index < 0 then exit;
    CalcRects(ItemRect(index), rcb, ricon);
    if (cloShowCheckboxes in Options) and IsPointInRect(p, rcb) then
      ClickedCheckbox(index)
    else if (cloShowIcons in Options) and IsPointInRect(p, ricon) then
      // Remember clicked index for the double click event.
      FSeriesIconClicked := index
    else
      ClickedItem(index);
  finally
    inherited MouseDown(AButton, AShift, AX, AY);
  end;
end;

procedure TChartListbox.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FChart) then
    FChart := nil;
  inherited Notification(AComponent, AOperation);
end;

procedure TChartListbox.Populate;
{ populates the listbox with all series contained in the chart. Use the event
  OnPopulate if you don't omit special series from the listbox (RemoveSeries) }
var
  li: TLegendItem;
  list: TFPList;
  ser: TCustomChartSeries;
  i, idx: Integer;
begin
  Items.BeginUpdate;
  list := TFPList.Create;
  try
    // In case of multiselect, the selected items would get lost here.
    // Store series belonging to selected items in temporary list
    for i:=0 to Items.Count-1 do
      if Selected[i] then begin
        li := TLegendItem(Items.Objects[i]);
        if (li <> nil) and (li.Owner is TCustomChartSeries) then begin
          ser := TCustomChartSeries(li.Owner);
          list.Add(ser);
        end;
      end;

    Items.Clear;
    if (FChart = nil) or (FChart.Series = nil) then exit;
    FreeAndNil(FLegendItems);
    FLegendItems := CreateLegendItems;
    Chart.Legend.SortItemsByOrder(FLegendItems);
    for li in FLegendItems do begin
      // The caption is owner-drawn, but add it anyway for user convenience.
      idx := Items.AddObject(li.Text, li);
      // Restore selected state from temporary list
      if (li.Owner is TCustomChartSeries) then begin
        ser := TCustomChartSeries(li.Owner);
        if list.IndexOf(ser) <> -1 then Selected[idx] := true;
      end;
    end;
    if Assigned(OnPopulate) then
      OnPopulate(Self);
  finally
    list.Free;
    Items.EndUpdate;
  end;
end;

procedure TChartListbox.RemoveSeries(ASeries: TCustomChartSeries);
{ removes the series from the listbox, but keeps it in the chart }
var
  index: Integer;
begin
  index := FindSeriesIndex(ASeries);
  if index = -1 then exit;
  FLegendItems.Delete(index);
  Items.Delete(index);
  Invalidate;
end;

procedure TChartListbox.SeriesChanged(ASender: TObject);
{ Notification procedure of the listener. Responds to chart broadcasts
  by populating the listbox with the chart's series }
begin
  if
    (ASender is TCustomChartSource) and
    not (cloRefreshOnSourceChange in Options)
  then
    exit;
  Populate;
  { in case of radiobutton mode, it is necessary to uncheck the other
    series; there can be only one active series in this mode }
  if
    (ASender is TCustomChartSeries) and TCustomChartSeries(ASender).Active
  then
    EnsureSingleChecked(FindSeriesIndex(TCustomChartSeries(ASender)))
  else
    EnsureSingleChecked;
end;

procedure TChartListbox.SetChart(AValue: TChart);
{ connects the ListBox to the chart }
begin
  if FChart = AValue then exit;

  if FListener.IsListening then
    FChart.Broadcaster.Unsubscribe(FListener);
  FChart := AValue;
  if FChart <> nil then
    FChart.Broadcaster.Subscribe(FListener);
  SeriesChanged(Self);
end;

procedure TChartListbox.SetChecked(AIndex: Integer; AValue: Boolean);
{ shows/hides the series with the specified index of its listbox item.
  In case of radiobutton style, all other series are hidden if AValue=true }
var
  ser: TCustomChartSeries;
begin
  ser := GetSeries(AIndex);
  if (ser = nil) or (ser.Active = AValue) then exit;
  // Do not listen to this change since we know what changed.
  FListener.OnNotify := nil;
  try
    ser.Active := AValue;
  finally
    FListener.OnNotify := @SeriesChanged;
  end;
  if AValue then
    EnsureSingleChecked(FindSeriesIndex(ser));
  Invalidate;
end;

procedure TChartListbox.SetCheckStyle(AValue: TCheckBoxesStyle);
{ selects "checkbox" or "radiobutton" styles. In radiobutton mode, only
  one series can be visible }
begin
  if FCheckStyle = AValue then exit;
  FCheckStyle := AValue;
  EnsureSingleChecked;
  Invalidate;
end;

procedure TChartListbox.SetOnAddSeries(AValue: TChartListboxAddSeriesEvent);
begin
  if TMethod(FOnAddSeries) = TMethod(AValue) then exit;
  FOnAddSeries := AValue;
  Populate;
end;

procedure TChartListbox.SetOnPopulate(AValue: TNotifyEvent);
begin
  if TMethod(FOnPopulate) = TMethod(AValue) then exit;
  FOnPopulate := AValue;
  Populate;
end;

procedure TChartListbox.SetOptions(AValue: TChartListOptions);
begin
  if FOptions = AValue then exit;
  FOptions := AValue;
  EnsureSingleChecked;
  Invalidate;
end;

end.

