unit TAChartCombos;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Graphics, Classes, Controls, StdCtrls, TATypes, TAGraph;

type
  TChartComboMode = (ccmPointerStyle, ccmPenStyle, ccmPenWidth, ccmBrushStyle);

  TChartComboOptions = set of (
    ccoNames,           // Show item names in combo
    ccoPatternBrush,    // Include bsPattern item in brush style mode
    ccoImageBrush,      // Include bsImage item in brush style mode
    ccoPatternPen);     // Include psPattern item in pen style mode

const
  DEFAULT_POINTER_STYLE = psCircle;
  DEFAULT_SYMBOL_WIDTH = 40;
  DEFAULT_DROPDOWN_COUNT = 24;
  DEFAULT_OPTIONS = [ccoNames, ccoPatternBrush, ccoPatternPen];

type
  TChartComboBox = class(TCustomComboBox)
    private
      FAlignment: TAlignment;
      FBitmaps: array[TSeriesPointerStyle] of TBitmap;
      FBrushBitmap: TBitmap;
      FBrushColor: TColor;
      FBrushStyle: TBrushStyle;
      FPenPattern: TPenPattern;
      FPenColor: TColor;
      FPenStyle: TPenStyle;
      FPenWidth: Integer;
      FMaxPenWidth: Integer;
      FPointerStyle: TSeriesPointerStyle;
      FCosmetic: Boolean;
      FLockItemIndex: Integer;
      FMode: TChartComboMode;
      FOptions: TChartComboOptions;
      FSymbolWidth: Integer;
      function GetPenPattern: String;
      function GetSymbolWidth: Integer;
      procedure SetAlignment(const AValue: TAlignment);
      procedure SetBrushBitmap(const AValue: TBitmap);
      procedure SetBrushColor(const AValue: TColor);
      procedure SetCosmetic(const AValue: Boolean);
      procedure SetMaxPenWidth(const AValue: Integer);
      procedure SetMode(const AValue: TChartComboMode);
      procedure SetOptions(const AValue: TChartComboOptions);
      procedure SetPenColor(const AValue: TColor);
      procedure SetPenPattern(const AValue: String); overload;
      procedure SetSelectedBrushStyle(const AValue: TBrushStyle);
      procedure SetSelectedPenStyle(const AValue: TPenStyle);
      procedure SetSelectedPenWidth(const AValue: Integer);
      procedure SetSelectedPointerStyle(const AValue: TSeriesPointerStyle);
      procedure SetSymbolWidth(const AValue: Integer);
      function SymbolWidthStored: Boolean;
    protected
      procedure Change; override;
      procedure CreateBitmaps(AWidth, AHeight: Integer);
      procedure DestroyBitmaps;
      procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
        const AXProportion, AYProportion: Double); override;
      procedure DrawItem(AIndex: Integer; ARect: TRect; AState: TOwnerDrawState); override;
      function GetBrushStyle(const AIndex: Integer): TBrushStyle;
      function GetPenStyle(const AIndex: Integer): TPenStyle;
      function GetPenWidth(const AIndex: Integer): Integer;
      function GetPointerStyle(AIndex: Integer): TSeriesPointerStyle; inline;
      procedure PopulateBrushStyles;
      procedure PopulatePenStyles;
      procedure PopulatePenWidths;
      procedure PopulatePointerStyles;
      procedure RealSetText(const AValue: TCaption); override;
      procedure SetItemIndex(const AValue: Integer); override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure SetPenPattern(APen: TPen); overload;
    published
      property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
      property BrushBitmap: TBitmap read FBrushBitmap write SetBrushBitmap;
      property BrushColor: TColor read FBrushColor write SetBrushColor default clBlack;
      property BrushStyle: TBrushStyle read FBrushStyle write SetSelectedBrushStyle default bsSolid;
      property Cosmetic: Boolean read FCosmetic write SetCosmetic default true;
      property MaxPenWidth: Integer read FMaxPenWidth write SetMaxPenWidth default 5;
      property Mode: TChartComboMode read FMode write SetMode default ccmPenStyle;
      property Options: TChartComboOptions read FOptions write SetOptions default DEFAULT_OPTIONS;
      property PenPattern: string read GetPenPattern write SetPenPattern;
      property PenColor: TColor read FPenColor write SetPenColor default clBlack;
      property PenStyle: TPenStyle read FPenStyle write SetSelectedPenStyle default psSolid;
      property PenWidth: Integer read FPenWidth write SetSelectedPenWidth default 1;
      property PointerStyle: TSeriesPointerStyle read FPointerStyle write SetSelectedPointerStyle default DEFAULT_POINTER_STYLE;
//      property ShowNames: Boolean read FShowNames write SetShowNames default true;
      property SymbolWidth: Integer read GetSymbolWidth write SetSymbolWidth stored SymbolWidthStored;

      property Align;
      property Anchors;
      property AutoDropDown;
      property BidiMode;
      property BorderSpacing;
      property Color;
      property Constraints;
      property Cursor;
      property DragCursor;
      property DragKind;
      property DragMode;
      property DropDownCount default DEFAULT_DROPDOWN_COUNT;
      property Enabled;
      property Font;
      property ItemHeight;
      property ItemIndex;
      property ItemWidth;
      property Left;
      property ParentBidiMode;
      property ParentColor;
      property ParentFont;
      property ParentShowHint;
      property PopupMenu;
      property ShowHint;
      property TabOrder;
      property TabStop;
      property Top;
      property Visible;
      property Width;

      property OnChange;
      property OnChangeBounds;
      property OnClick;
      property OnCloseUp;
      property OnContextPopup;
      property OnDblClick;
      property OnDragDrop;
      property OnDragOver;
      property OnDrawItem;
      property OnEndDrag;
      property OnDropDown;
      property OnEditingDone;
      property OnEnter;
      property OnExit;
      property OnKeyDown;
      property OnKeyPress;
      property OnKeyUp;
      property OnMeasureItem;
      property OnMouseDown;
      property OnMouseEnter;
      property OnMouseLeave;
      property OnMouseMove;
      property OnMouseUp;
      property OnStartDrag;
      property OnSelect;
      property OnUTF8KeyPress;
    end;

procedure Register;


implementation

uses
  LCLType, Types, TypInfo, Math, FPCanvas,
  TAChartStrConsts, TAChartUtils, TADrawUtils, TADrawerCanvas, TACustomSeries,
  TASeries, TALegend;

procedure Register;
begin
  RegisterComponents(CHART_COMPONENT_IDE_PAGE, [
    //TSeriesPointerStyleCombobox,
    TChartComboBox
  ]);
end;

function GetBrushStyleName(AStyle: TBrushStyle): String;
begin
  case AStyle of
    bsSolid      : Result := rsBSSolid;
    bsHorizontal : Result := rsBSHorizontal;
    bsVertical   : Result := rsBSVertical;
    bsFDiagonal  : Result := rsBSFDiagonal;
    bsBDiagonal  : Result := rsBSBDiagonal;
    bsCross      : Result := rsBSCross;
    bsDiagCross  : Result := rsBSDiagCross;
    bsImage      : Result := rsBSImage;
    bsPattern    : Result := rsBSPattern;
    else           Result := rsBSClear;
  end;
end;

function GetSymbolName(ASymbol: TSeriesPointerStyle): string;
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

function GetPenStyleName(AStyle: TPenStyle): String;
begin
  case AStyle of
    psSolid       : Result := rsPSSolid;
    psDash        : Result := rsPSDash;
    psDot         : Result := rsPSDot;
    psDashDot     : Result := rsPSDashDot;
    psDashDotDot  : Result := rsPSDashDotDot;
    psInsideFrame : Result := rsPSInsideFrame;
    psPattern     : Result := rsPSPattern;
    else            Result := rsPSClear;
  end;
end;


{ TChartComboBox }

constructor TChartComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DropdownCount := DEFAULT_DROPDOWN_COUNT;
  ReadOnly := true;  // Needed to see the symbol without dropdown.
  Style := csOwnerDrawFixed;
  SetLength(FPenPattern, 2);
  FPenPattern[0] := 1;
  FPenPattern[1] := 1;
  FPenColor := clBlack;
  FCosmetic := true;
  FMode := ccmPenStyle;
  FBrushBitmap := TBitmap.Create;
  FBrushColor := clBlack;
  FBrushStyle := bsSolid;
  FPenStyle := psSolid;
  FPenWidth := 1;
  FMaxPenWidth := 5;
  FOptions := DEFAULT_OPTIONS;
  FSymbolWidth := -1;
  PopulatePenStyles;
  SetSelectedPenStyle(FPenStyle);
  GetItems;
end;

destructor TChartCombobox.Destroy;
begin
  DestroyBitmaps;
  FBrushBitmap.Free;
  inherited;
end;

procedure TChartCombobox.Change;
begin
  SetItemIndex(ItemIndex);
  inherited;
end;

procedure TChartCombobox.CreateBitmaps(AWidth, AHeight: Integer);
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
          Pointer.Brush.Color := FBrushColor;
          Pointer.Pen.Color := FPenColor;
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

procedure TChartCombobox.DestroyBitmaps;
var
  ps: TSeriesPointerStyle;
begin
  for ps in TSeriesPointerStyle do
    FreeAndNil(FBitmaps[ps]);
end;

procedure TChartComboBox.DoAutoAdjustLayout(
  const AMode: TLayoutAdjustmentPolicy;
  const AXProportion, AYProportion: Double);
begin
  inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);

  if AMode in [lapAutoAdjustWithoutHorizontalScrolling, lapAutoAdjustForDPI] then
  begin
    if SymbolWidthStored then
      FSymbolWidth := Round(FSymbolWidth * AXProportion);
    Invalidate;
  end;
end;

procedure TChartComboBox.DrawItem(AIndex: Integer; ARect: TRect;
  AState: TOwnerDrawState);
const
  DIST = 4;
  MARGIN = 2;
var
  ts: TTextStyle;
  alignmnt: TAlignment;
  x1, x2, y: Integer;
  txt: String;
  bs: TBrushStyle;
  sps: TSeriesPointerStyle;
  symwidth, symheight: Integer;
begin
  if Assigned(OnDrawItem) then
    OnDrawItem(Self, AIndex, ARect, AState)
  else begin
    if odFocused in AState then begin
      Canvas.Brush.Color := clHighlight;
      Canvas.Font.Color := clHighlightText;
    end;
    Canvas.FillRect(ARect);

    if (BiDiMode <> bdLeftToRight) then
      case FAlignment of
        taLeftJustify : alignmnt := taRightJustify;
        taCenter      : alignmnt := taCenter;
        taRightJustify: alignmnt := taLeftJustify
      end
    else
      alignmnt := FAlignment;

    symheight := ARect.Bottom - ARect.Top - 2 * MARGIN;
    symwidth := IfThen(FMode = ccmPointerStyle, symheight * 6 div 4, SymbolWidth);

    case alignmnt of
      taLeftJustify  : x1 := IfThen(DroppedDown, MARGIN, MARGIN * 2);
      taRightJustify : x1 := ARect.Right - MARGIN - symwidth;
      taCenter       : x1 := (ARect.Left + ARect.Right - symwidth) div 2;
    end;
    x2 := x1 + symwidth;

    if (odSelected in AState) or (odFocused in AState) then
      Canvas.Pen.Color := clHighlightText
    else
      Canvas.Pen.Color := FPenColor;
    Canvas.Pen.Cosmetic := FCosmetic;
    case FMode of
      ccmBrushStyle:
        begin
          bs := GetBrushStyle(AIndex);
          if bs <> bsClear then begin
            Canvas.Brush.Color := clWhite;
            Canvas.Brush.Style := bsSolid;
            Canvas.FillRect(x1, ARect.Top + MARGIN, x2, ARect.Bottom - MARGIN);
          end;
          Canvas.Brush.Color := FBrushColor;
          Canvas.Brush.Style := bs;
          if (bs = bsImage) or (bs = bsPattern) then
            Canvas.Brush.Bitmap := FBrushBitmap; // AFTER assigning Brush.Style!
          Canvas.Pen.Color := clBlack;
          Canvas.Pen.Style := psSolid;
          Canvas.Rectangle(x1, ARect.Top + MARGIN, x2, ARect.Bottom - MARGIN)
        end;
      ccmPenStyle:
        begin
          Canvas.Pen.Style := GetPenStyle(AIndex);
          if Canvas.Pen.Style = psPattern then
            Canvas.Pen.SetPattern(FPenPattern);
          Canvas.Pen.Width := 1;
          Canvas.Brush.Style := bsClear;
          y := (ARect.Top + ARect.Bottom) div 2;
          Canvas.Line(x1, y, x2, y);
        end;
      ccmPenWidth:
        begin
          Canvas.Pen.Style := psSolid;
          Canvas.Pen.Width := GetPenWidth(AIndex);
          Canvas.Pen.EndCap := pecFlat;
          Canvas.Brush.Style := bsClear;
          y := (ARect.Top + ARect.Bottom) div 2;
          Canvas.Line(x1, y, x2, y);
        end;
      ccmPointerStyle:
        begin
          if (FBitmaps[psCircle] = nil) or (FBitmaps[psCircle].Height <> symHeight)
            then CreateBitmaps(symwidth, symHeight);
          sps := GetPointerStyle(AIndex);
          Canvas.Draw(x1, ARect.Top + MARGIN, FBitmaps[sps]);
        end;
    end;

    if (ccoNames in FOptions) and (FAlignment <> taCenter) then begin
      ts := Canvas.TextStyle;
      ts.Layout := tlCenter;
      ts.Opaque := false;
      ts.EndEllipsis := true;
      txt := Items[AIndex];
      case alignmnt of
        taLeftJustify  : ARect.Left := x2 + DIST;
        taRightJustify : ARect.Left := x1 - DIST - Canvas.TextWidth(txt);
      end;
      Canvas.TextRect(ARect, ARect.Left, ARect.Top, txt, ts);
    end;
  end;
end;

function TChartCombobox.GetBrushStyle(const AIndex: Integer): TBrushStyle;
begin
  if AIndex < 0 then
    Result := bsSolid
  else
    Result := TBrushStyle(AIndex);
end;

function TChartComboBox.GetPenPattern: String;
var
  i: Integer;
begin
  if Length(FPenPattern) > 0 then begin
    Result := IntToStr(FPenPattern[0]);
    for i := 1 to High(FPenPattern) do
      Result := Result + '|' + IntToStr(FPenPattern[i]);
  end else
    Result := '';
end;

function TChartComboBox.GetPenStyle(const AIndex: Integer): TPenStyle;
begin
  if AIndex < 0 then
    Result := psSolid
  else
//    Result := TPenStyle(AIndex);
    Result := TPenStyle(PtrInt(Items.Objects[AIndex]));
end;

function TChartComboBox.GetPenWidth(const AIndex: Integer): Integer;
begin
  if AIndex < 0 then Result := 1 else Result := AIndex + 1;
end;

function TChartCombobox.GetPointerStyle(AIndex: Integer): TSeriesPointerStyle;
begin
  if AIndex = -1 then
    Result := psNone
  else
    Result := TSeriesPointerStyle(PtrInt(Items.Objects[AIndex]));
end;

function TChartCombobox.GetSymbolWidth: Integer;
begin
  if SymbolWidthStored then
    Result := FSymbolWidth
  else
    Result := Scale96ToFont(DEFAULT_SYMBOL_WIDTH);
end;

procedure TChartCombobox.PopulateBrushStyles;
var
  bs: TBrushStyle;
begin
  inc(FLockItemIndex);
  Items.BeginUpdate;
  try
    Items.Clear;
    for bs in TBrushStyle do begin
      if (bs = bsPattern) and not (ccoPatternBrush in FOptions) then
        Continue;
      if (bs = bsImage) and not (ccoImageBrush in FOptions) then
        Continue;
      Items.Add(GetBrushStylename(bs));
    end;
  finally
    Items.EndUpdate;
    dec(FLockItemIndex);
  end;
end;

procedure TChartComboBox.PopulatePenStyles;
var
  ps: TPenStyle;
begin
  inc(FLockItemIndex);
  Items.BeginUpdate;
  try
    Items.Clear;
    for ps in TPenStyle do begin
      if (ps = psPattern) and not (ccoPatternPen in FOptions) then
        Continue;
      Items.AddObject(GetPenStyleName(ps), TObject(PtrInt(ps)));
    end;
  finally
    Items.EndUpdate;
    dec(FLockItemIndex);
  end;
end;

procedure TChartComboBox.PopulatePenWidths;
var
  w: Integer;
begin
  inc(FLockItemIndex);
  Items.BeginUpdate;
  try
    Items.Clear;
    for w := 1 to FMaxPenWidth do
      Items.Add(IntToStr(w));
  finally
    Items.EndUpdate;
    dec(FLockItemIndex);
  end;
end;

procedure TChartCombobox.PopulatePointerStyles;
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
  sel := FPointerStyle;
  styleItems := TStringList.Create;
  try
    for i:=0 to High(LIST) do begin
      ps := LIST[i];
      s := GetSymbolName(ps);
      if s <> '' then
        styleItems.AddObject(s, TObject(PtrInt(ps)));
    end;
    Items.Assign(styleitems);
  finally
    styleItems.Free;
    SetSelectedPointerStyle(sel);
  end;
end;

{ Is overridden to prevent loss of default selected pointer style when
  combo is added to a form in designer. }
procedure TChartComboBox.RealSetText(const AValue: TCaption);
var
  bs: TBrushStyle;
  ps: TPenStyle;
  sps: TSeriesPointerStyle;
  pw: Integer;
begin
  case FMode of
    ccmBrushStyle   : bs := GetBrushStyle(ItemIndex);
    ccmPenStyle     : ps := GetPenStyle(ItemIndex);
    ccmPenWidth     : pw := GetPenWidth(ItemIndex);
    ccmPointerStyle : sps := GetPointerStyle(ItemIndex);
  end;
  inherited RealSetText(AValue);
  case FMode of
    ccmBrushStyle   : SetSelectedBrushStyle(bs);
    ccmPenStyle     : SetSelectedPenStyle(ps);
    ccmPenWidth     : SetSelectedPenWidth(pw);
    ccmPointerStyle : SetSelectedPointerStyle(sps);
  end;
end;

procedure TChartComboBox.SetAlignment(const AValue: TAlignment);
begin
  if FAlignment = AValue then exit;
  FAlignment := AValue;
  Invalidate;
end;

procedure TChartCombobox.SetBrushBitmap(const AValue: TBitmap);
begin
  if FBrushBitmap = AValue then exit;
  FBrushBitmap.Assign(AValue);
  Invalidate;
end;

procedure TChartCombobox.SetBrushColor(const AValue: TColor);
begin
  if FBrushColor = AValue then exit;
  FBrushColor := AValue;
  DestroyBitmaps;
  Invalidate;
end;

procedure TChartComboBox.SetCosmetic(const AValue: Boolean);
begin
  if FCosmetic = AValue then exit;
  FCosmetic := AValue;
  Invalidate;
end;

procedure TChartComboBox.SetItemIndex(const AValue: Integer);
begin
  if FLockItemIndex = 0 then
    case FMode of
      ccmBrushStyle   : FBrushStyle := GetBrushStyle(AValue);
      ccmPenStyle     : FPenStyle := GetPenStyle(AValue);
      ccmPenWidth     : FPenWidth := AValue + 1;
      ccmPointerStyle : FPointerStyle := GetPointerStyle(AValue);
    end;
  inherited SetItemIndex(AValue);
end;

procedure TChartCombobox.SetMaxPenWidth(const AValue: Integer);
var
  pw: Integer;
begin
  if AValue = FMaxPenWidth then exit;
  FMaxPenWidth := AValue;
  if FMode = ccmPenWidth then begin
    pw := GetPenWidth(ItemIndex);
    PopulatePenWidths;
    SetSelectedPenWidth(pw);
  end;
  Invalidate;
end;

procedure TChartComboBox.SetMode(const AValue: TChartComboMode);
begin
  if AValue = FMode then exit;
  FMode := AValue;
  case FMode of
    ccmBrushStyle:
      begin
        DestroyBitmaps;
        PopulateBrushStyles;
        SetSelectedBrushStyle(FBrushStyle);
      end;
    ccmPenStyle:
      begin
        DestroyBitmaps;
        PopulatePenStyles;
        SetSelectedPenStyle(FPenStyle);
      end;
    ccmPenWidth:
      begin
        DestroyBitmaps;
        PopulatePenWidths;
        SetSelectedPenWidth(FPenWidth);
      end;
    ccmPointerStyle:
      begin
        DestroyBitmaps;  // bitmaps will be created when painting
        PopulatePointerStyles;
        SetSelectedPointerStyle(FPointerStyle);
      end;
  end;
end;

procedure TChartComboBox.SetOptions(const AValue: TChartComboOptions);
begin
  if FOptions = AValue then exit;
  FOptions := AValue;
  case FMode of
    ccmBrushStyle   : PopulateBrushStyles;
    ccmPenStyle     : PopulatePenStyles;
    ccmPenWidth     : PopulatePenWidths;
    ccmPointerStyle : PopulatePointerStyles;
  end;
  Invalidate;
end;

procedure TChartComboBox.SetPenPattern(const AValue: String);
var
  L: TStrings;
  i: Integer;
  patt: Integer;
begin
  if AValue = GetPenPattern then
    exit;

  L := TStringList.Create;
  try
    Split(AValue, L, '|');
    SetLength(FPenPattern, L.Count);
    for i := 0 to L.Count - 1 do
      if TryStrToInt(L[i], patt) then
        FPenPattern[i] := patt
      else
        FPenPattern[i] := 0;
    Invalidate;
  finally
    L.Free;
  end;
end;

procedure TChartComboBox.SetPenColor(const AValue: TColor);
begin
  if AValue = FPenColor then exit;
  FPenColor := AValue;
  DestroyBitmaps;
  Invalidate;
end;

procedure TChartComboBox.SetPenPattern(APen: TPen);
begin
  if Assigned(APen) then APen.SetPattern(FPenPattern);
end;

procedure TChartCombobox.SetSelectedBrushStyle(const AValue: TBrushStyle);
begin
  ItemIndex := EnsureRange(ord(AValue), 0, Items.Count - 1);
end;

procedure TChartComboBox.SetSelectedPenStyle(const AValue: TPenStyle);
var
  i: Integer;
begin
  for i := 0 to Items.Count - 1 do
    if GetPenStyle(i) = AValue then begin
      ItemIndex := i;
      exit;
    end;
  ItemIndex := -1;
end;

procedure TChartComboBox.SetSelectedPenWidth(const AValue: Integer);
begin
  ItemIndex := EnsureRange(AValue - 1, 0, Items.Count - 1);
end;

procedure TChartCombobox.SetSelectedPointerStyle(const AValue: TSeriesPointerStyle);
var
  i: Integer;
begin
  for i:= 0 to Items.Count - 1 do begin
    if GetPointerStyle(i) = AValue then begin
      FPointerStyle := AValue;
      ItemIndex := i;
      Invalidate;
      exit;
    end;
  end;
  ItemIndex := -1;
  FPointerStyle := psNone;
end;

procedure TChartComboBox.SetSymbolWidth(const AValue: Integer);
begin
  if FSymbolWidth = AValue then exit;
  FSymbolWidth := AValue;
  Invalidate;
end;

function TChartCombobox.SymbolWidthStored: Boolean;
begin
  Result := FSymbolWidth >= 0;
end;

end.
