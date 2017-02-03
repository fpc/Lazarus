unit TAChartCombos;

interface

uses
  SysUtils, Graphics, Classes, Controls, StdCtrls, TATypes, TAGraph;

const
  DEFAULT_POINTER_STYLE = psCircle;

type

  TSeriesPointerStyleCombobox = class(TCustomCombobox)
  private
    FSelected : TSeriesPointerStyle;
    FSymbolBordercolor : TColor;
    FSymbolFillColor : TColor;
    FShowNames : boolean;
    FAlignment : TAlignment;
    FBitmaps: array[TSeriespointerStyle] of TBitmap;
    procedure SetAlignment(Value:TAlignment);
    procedure SetSelected(AValue: TSeriesPointerStyle);
    procedure SetShowNames(AValue: boolean);
    procedure SetSymbolBorderColor(AValue: TColor);
    procedure SetSymbolFillColor(AValue: TColor);
  protected
    procedure CreateBitmaps(AWidth, AHeight: Integer);
    procedure DrawItem(AIndex: Integer; ARect: TRect; AState: TOwnerDrawState); override;
    procedure DestroyBitmaps;
    procedure GetItems; override;
    function GetSymbol(AIndex: Integer): TSeriesPointerStyle; inline;
    procedure RealSetText(const AValue: TCaption); override;
    procedure SetItemIndex(const AValue: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // new properties
    property Selected : TSeriesPointerStyle read FSelected write SetSelected default DEFAULT_POINTER_STYLE;
    property ShowNames : boolean read FShowNames write SetShowNames default true;
    property SymbolBorderColor : TColor read FSymbolBorderColor write SetSymbolBorderColor default clBlack;
    property SymbolFillColor : TColor read FSymbolFillColor write SetSymbolFillColor default clWhite;
    property Alignment : TAlignment read FAlignment write SetAlignment default taLeftJustify;

    // inherited
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderSpacing;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property AutoDropDown default False;
    property CharCase;
    property Color;
    property DragMode;
    property DragCursor;
    property DropDownCount default 24;
    property Enabled;
    property Font;
    property ItemHeight;
    property ItemWidth;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEndDock;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;

procedure Register;


implementation

uses
  LCLType, Types, TypInfo, Math,
  TAChartStrConsts, TAChartUtils, TADrawUtils, TADrawerCanvas, TACustomSeries,
  TASeries, TALegend;

procedure Register;
begin
  RegisterComponents(CHART_COMPONENT_IDE_PAGE, [TSeriesPointerStyleCombobox]);
end;

function GetSymbolName(ASymbol: TSeriesPointerStyle) : string;
begin
  case ASymbol of
    psRectangle    : Result := rsRectangleSymbol;
    psCircle       : Result := rsCircleSymbol;
    psCross        : Result := rsCrossSymbol;
    psDiagCross    : Result := rsDiagCrossSymbol;
    psStar         : Result := rsStarSymbol;
    psLowBracket   : Result := rsLowBracketSymbol;
    psHighBracket  : Result := rsHighBracketSymbol;
    psLeftTriangle : Result := rsLeftTriangleSymbol;
    psRightTriangle: Result := rsRightTriangleSymbol;
    psDiamond      : Result := rsDiamondSymbol;
    psVertBar      : Result := rsVertBarSymbol;
    psTriangle     : Result := rsTriangleSymbol;
    psLeftBracket  : Result := rsLeftBracketSymbol;
    psRightBracket : Result := rsRightBracketSymbol;
    psHorBar       : Result := rsHorBarSymbol;
    psPoint        : Result := rsPointSymbol;
    psDownTriangle : Result := rsDownTriangleSymbol;
    psHexagon      : Result := rsHexagonSymbol;
    psFullStar     : Result := rsFullStarSymbol;
    else             Result := rsNoSymbol;
  end;
end;


{ TSeriesPointerStyleCombobox }

constructor TSeriesPointerStyleCombobox.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  Style := csOwnerDrawFixed;
  DropdownCount := 24;
  ReadOnly := true;
  FSymbolBorderColor := clBlack;
  FSymbolFillColor := clWhite;
  FShowNames := true;
  FAlignment := taLeftJustify;
  FSelected := DEFAULT_POINTER_STYLE;
  GetItems;
  Caption := GetSymbolName(FSelected);
end;

destructor TSeriesPointerStyleCombobox.Destroy;
begin
  DestroyBitmaps;
  inherited;
end;

procedure TSeriesPointerStyleCombobox.CreateBitmaps(AWidth, AHeight: Integer);
var
  ps: TSeriesPointerStyle;
  chart: TChart;
  id: IChartDrawer;
  series: TLineSeries;
  legItems: TChartLegendItems;
begin
  DestroyBitmaps;

  chart := TChart.Create(nil);
  try
    for ps in TSeriesPointerStyle do begin
      FBitmaps[ps] := TBitmap.Create;
      FBitmaps[ps].Transparent := true;
      FBitmaps[ps].TransparentColor := RgbToColor(254,254,254);
      FBitmaps[ps].SetSize(AWidth, AHeight);
      FBitmaps[ps].Canvas.Brush.Color := FBitmaps[ps].TransparentColor;
      FBitmaps[ps].Canvas.FillRect(0, 0, AWidth, AHeight);

      series := TLineSeries.Create(chart);
      try
        with series do begin
          Pointer.Style := ps;
          Pointer.Brush.Color := FSymbolFillColor;
          Pointer.Pen.Color := FSymbolBorderColor;
          Pointer.HorizSize := Min(AWidth, AHeight);
          Pointer.VertSize := Pointer.HorizSize;
          ShowPoints := true;
          LineType := ltNone;
        end;
        chart.AddSeries(series);
        legitems := TChartLegendItems.Create;
        try
          series.GetSingleLegendItem(legItems);
          id := TCanvasDrawer.Create(FBitmaps[ps].Canvas);
          id.Pen := Chart.Legend.SymbolFrame;
          legItems[0].Draw(id, Rect(0, 0, AWidth-1, AHeight-1));
        finally
          legitems.Free;
        end;
      finally
        series.Free;
      end;
    end;
  finally
    chart.Free;
  end;
end;

procedure TSeriesPointerStyleCombobox.DestroyBitmaps;
var
  ps: TSeriesPointerStyle;
begin
  for ps in TSeriesPointerStyle do
    FreeAndNil(FBitmaps[ps]);
end;

procedure TSeriesPointerStyleCombobox.DrawItem(AIndex:Integer; ARect:TRect;
  AState: TOwnerDrawState);
const
  MARGIN = 2;
var
  symRect: TRect;
  symheight : integer;
  symwidth: Integer;
  txt: string;
  ps: TSeriesPointerStyle;
  ts: TTextStyle;
  alignmnt: TAlignment;
begin
  SymRect := ARect;
  inc(SymRect.Top, MARGIN);
  dec(SymRect.Bottom, MARGIN);
  symheight := SymRect.Bottom - SymRect.Top;
  symwidth := symheight * 6 div 4; // see: TLegendItemLinePointer.Draw in TALagend
  if (BiDiMode <> bdLeftToRight) then
    case FAlignment of
      taLeftJustify : alignmnt := taRightJustify;
      taCenter      : alignmnt := taCenter;
      taRightJustify: alignmnt := taLeftJustify
    end
  else
    alignmnt := FAlignment;
  case alignmnt of
    taLeftJustify  : ;
    taCenter       : SymRect.Left := (ARect.Left + ARect.Right - symwidth) div 2;
    taRightJustify : SymRect.Left := ARect.Right - MARGIN - symwidth;
  end;
  SymRect.Right := SymRect.Left + symwidth;

  with Canvas do begin
    if odSelected in AState then begin
      Brush.Color := clHighlight;
      Font.Color := clHighlightText;
    end else begin
      Brush.Color := Color;
      Font.Color := clWindowText;
    end;
    Brush.Style := bsSolid;
    FillRect(ARect);

    // Create bitmaps of pointer symbols if they are nil, or if height has changed
    if (FBitmaps[psCircle] = nil) or (FBitmaps[psCircle].Height <> symheight)
      then CreateBitmaps(symwidth, symheight);

    Pen.Color := FSymbolBorderColor;
    Pen.Style := psSolid;
    Pen.Width := 1;
    Brush.Color := FSymbolFillColor;
    ps := GetSymbol(AIndex);
    Canvas.Draw(SymRect.Left, SymRect.Top, FBitmaps[ps]);

    if FShowNames and (alignmnt <> taCenter) then begin   // Note: No text output for taCenter!
      txt := Items[AIndex];
      case alignmnt of
        taLeftJustify  : ARect.Left := SymRect.Right + 2 * MARGIN;
        taRightJustify : ARect.Left := SymRect.Left - 2 * MARGIN - Canvas.TextWidth(txt);
      end;
      ts := Canvas.TextStyle;
      ts.Layout := tlCenter;
      ts.Opaque := false;
      ts.EndEllipsis := true;
      TextRect(ARect, ARect.Left, ARect.Top, txt, ts);
    end;
  end;
end;

procedure TSeriesPointerStyleCombobox.GetItems;
const
  // Arrange symbols in "nice" order
  LIST: array[0..19] of TSeriesPointerStyle = (
    psNone, psRectangle, psCircle, psDiamond,
    psTriangle, psDownTriangle, psLeftTriangle, psRightTriangle,
    psHexagon, psFullStar,
    psStar, psCross, psDiagCross,
    psLowBracket, psHighBracket, psLeftBracket, psRightBracket,
    psHorBar, psVertBar, psPoint);
var
  ps: TSeriesPointerStyle;
  s: String;
  i: Integer;
  sel: TSeriesPointerStyle;
  styleItems: TStrings;
begin
  sel := FSelected;
  styleItems := TStringList.Create;
  try
    for i:=0 to High(LIST) do begin
      ps := LIST[i];
      s := GetSymbolName(ps);
      if s <> '' then
        styleItems.AddObject(s, TObject(PtrInt(ps)));
    end;
    inherited Items.Assign(styleitems);
  finally
    styleItems.Free;
    SetSelected(sel);
  end;
end;

function TSeriesPointerStyleCombobox.GetSymbol(AIndex: Integer): TSeriesPointerStyle;
begin
  if AIndex = -1 then
    Result := psNone
  else
    Result := TSeriesPointerStyle(Items.Objects[AIndex]);
end;

{ Is overridden to prevent loss of default selected pointer style when
  combo is added to a form in designer. }
procedure TSeriesPointerStyleCombobox.RealSetText(const AValue: TCaption);
var
  sel: TSeriesPointerStyle;
begin
  sel := FSelected;
  inherited RealSetText(AValue);
  SetSelected(sel);
end;

procedure TSeriesPointerStyleCombobox.SetAlignment(Value:TAlignment);
begin
  if Value <> FAlignment then begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TSeriesPointerStyleCombobox.SetItemIndex(const AValue: Integer);
begin
  FSelected := GetSymbol(AValue);
  if AValue = inherited ItemIndex then exit;
  inherited SetItemIndex(AValue);
end;

procedure TSeriesPointerStyleCombobox.SetSelected(AValue:TSeriesPointerStyle);
var
  i : integer;
begin
  for i := 0 to Items.Count-1 do begin
    if GetSymbol(i) = AValue then begin
      FSelected := AValue;
      ItemIndex := i;
      Invalidate;
      exit;
    end;
  end;
  ItemIndex := -1;
  FSelected := psNone;
end;

procedure TSeriesPointerStyleCombobox.SetShowNames(AValue: boolean);
begin
  if (FShowNames <> AValue) then begin
    FShowNames := AValue;
    Invalidate;
  end;
end;

procedure TSeriesPointerStyleCombobox.SetSymbolBorderColor(AValue: TColor);
begin
  if FSymbolBorderColor <> AValue then begin
    FSymbolBorderColor := AValue;
    DestroyBitmaps;
    Invalidate;
  end;
end;

procedure TSeriesPointerStyleCombobox.SetSymbolFillColor(AValue: TColor);
begin
  if FSymbolFillColor <> AValue then begin
    FSymbolFillColor := AValue;
    DestroyBitmaps;
    Invalidate;
  end;
end;


end.
