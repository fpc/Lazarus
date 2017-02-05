unit TAChartCombos;

interface

uses
  SysUtils, Graphics, Classes, Controls, StdCtrls, TATypes, TAGraph;

const
  DEFAULT_POINTER_STYLE = psCircle;
  DEFAULT_SYMBOL_WIDTH = 40;
  DEFAULT_DROPDOWN_COUNT = 24;

type
(*
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
    property DropDownCount default DEFAULT_DROPDOWN_COUNT;
    property Enabled;
    property Font;
    property ItemHeight;
    property ItemWidth;
    property Left;
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
                    *)
  TChartComboMode = (ccmPointerStyle, ccmPenStyle, ccmPenWidth, ccmBrushStyle);

  TChartComboBox = class(TCustomComboBox)
    private
      FAlignment: TAlignment;
      FBitmaps: array[TSeriesPointerStyle] of TBitmap;
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
      FShowNames: Boolean;
      FSymbolWidth: Integer;
      function GetPenPattern: String;
      procedure SetAlignment(const AValue: TAlignment);
      procedure SetBrushColor(const AValue: TColor);
      procedure SetCosmetic(const AValue: Boolean);
      procedure SetMaxPenWidth(const AValue: Integer);
      procedure SetMode(const AValue: TChartComboMode);
      procedure SetPenColor(const AValue: TColor);
      procedure SetPenPattern(const AValue: String); overload;
      procedure SetSelectedBrushStyle(const AValue: TBrushStyle);
      procedure SetSelectedPenStyle(const AValue: TPenStyle);
      procedure SetSelectedPenWidth(const AValue: Integer);
      procedure SetSelectedPointerStyle(const AValue: TSeriesPointerStyle);
      procedure SetShowNames(const AValue: Boolean);
      procedure SetSymbolWidth(const AValue: Integer);
    protected
      procedure CreateBitmaps(AWidth, AHeight: Integer);
      procedure DestroyBitmaps;
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
      procedure ApplyToPen(APen: TPen);
      procedure ExtractFromPen(APen: TPen);
      procedure SetPenPattern(APen: TPen); overload;
    published
      property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
      property BrushColor: TColor read FBrushColor write SetBrushColor default clBlack;
      property BrushStyle: TBrushStyle read FBrushStyle write SetSelectedBrushStyle default bsSolid;
      property Cosmetic: Boolean read FCosmetic write SetCosmetic default true;
      property MaxPenWidth: Integer read FMaxPenWidth write SetMaxPenWidth default 5;
      property Mode: TChartComboMode read FMode write SetMode default ccmPenStyle;
      property PenPattern: string read GetPenPattern write SetPenPattern;
      property PenColor: TColor read FPenColor write SetPenColor default clBlack;
      property PenStyle: TPenStyle read FPenStyle write SetSelectedPenStyle default psSolid;
      property PenWidth: Integer read FPenWidth write SetSelectedPenWidth default 1;
      property PointerStyle: TSeriesPointerStyle read FPointerStyle write SetSelectedPointerStyle default DEFAULT_POINTER_STYLE;
      property ShowNames: Boolean read FShowNames write SetShowNames default true;
      property SymbolWidth: Integer read FSymbolWidth write SetSymbolWidth default DEFAULT_SYMBOL_WIDTH;

      property Align;
      property Anchors;
//      property ArrowKeysTraverseList;
//      property AutoComplete;
//      property AutoCompleteText;
      property AutoDropDown;
//      property AutoSelect;
//      property AutoSize;// Note: windows has a fixed height in some styles
      property BidiMode;
      property BorderSpacing;
//      property BorderStyle;
//      property CharCase;
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
     // property Items;
      property ItemWidth;
      property Left;
     // property MaxLength;
      property ParentBidiMode;
      property ParentColor;
      property ParentFont;
      property ParentShowHint;
      property PopupMenu;
//      property ReadOnly;
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
//      property OnGetItems;
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

                            (*
{ TSeriesPointerStyleCombobox }

constructor TSeriesPointerStyleCombobox.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  Style := csOwnerDrawFixed;
  DropdownCount := DEFAULT_DROPDOWN_COUNT;
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

procedure TSeriesPointerStyleCombobox.DrawItem(AIndex: Integer; ARect: TRect;
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
  if inherited Items.Count > 0 then
    exit;

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
    Result := TSeriesPointerStyle(PtrInt(Items.Objects[AIndex]));
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
               *)

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
  FBrushColor := clBlack;
  FBrushStyle := bsSolid;
  FPenStyle := psSolid;
  FPenWidth := 1;
  FMaxPenWidth := 5;
  FShowNames := true;
  FSymbolWidth := DEFAULT_SYMBOL_WIDTH;
  PopulatePenStyles;
  SetSelectedPenStyle(FPenStyle);
end;

destructor TChartCombobox.Destroy;
begin
  DestroyBitmaps;
  inherited;
end;

procedure TChartComboBox.ApplyToPen(APen: TPen);
begin
  if Assigned(APen) then begin
    APen.Style := FPenStyle;
    APen.Color := FPenColor;
    APen.Cosmetic := FCosmetic;
    APen.SetPattern(FPenPattern);
    APen.Width := FPenWidth;
  end;
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
    symwidth := IfThen(FMode = ccmPointerStyle, symheight * 6 div 4, FSymbolWidth);

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
          y := (ARect.Top + ARect.Bottom) div 2; // - FPenWidth) div 2;
          Canvas.Line(x1, y, x2, y);
        end;
      ccmPenWidth:
        begin
          Canvas.Pen.Style := psSolid;
          Canvas.Pen.Width := GetPenWidth(AIndex);
          Canvas.Pen.EndCap := pecFlat;
          Canvas.Brush.Style := bsClear;
          y := (ARect.Top + ARect.Bottom) div 2; // - FPenWidth) div 2;
          Canvas.Line(x1, y, x2, y);
        end;
      ccmPointerStyle:
        begin
          if (FBitmaps[psCircle] = nil) or (FBitmaps[psCircle].Height <> symHeight)
            then CreateBitmaps(symwidth, symHeight);
          {
          Canvas.Pen.Color := FPenColor;
          Canvas.Pen.Style := psSolid;
          Canvas.Pen.Width := 1;
          Canvas.Brush.Color := FBrushColor;
          Canvas.Brush.Style := bsSolid;
          }
          sps := GetPointerStyle(AIndex);
          Canvas.Draw(x1, ARect.Top + MARGIN, FBitmaps[sps]);
        end;
    end;

    if FShowNames and (FAlignment <> taCenter) then begin
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

procedure TChartComboBox.ExtractFromPen(APen: TPen);
begin
  if Assigned(APen) then begin
    FCosmetic := APen.Cosmetic;
    FPenPattern := APen.GetPattern;
    FPenColor := APen.Color;
    FPenStyle := APen.Style;
    FPenWidth := APen.Width;
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
    Result := TPenStyle(AIndex);
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

procedure TChartCombobox.PopulateBrushStyles;
var
  bs: TBrushStyle;
begin
  inc(FLockItemIndex);
  Items.BeginUpdate;
  try
    Items.Clear;
    for bs in TBrushStyle do
      Items.Add(GetBrushStylename(bs));
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
    for ps in TPenStyle do
      Items.Add(GetPenStyleName(ps));
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
begin
  ItemIndex := EnsureRange(ord(AValue), 0, Items.Count - 1);
end;

procedure TChartComboBox.SetSelectedPenWidth(const AValue: Integer);
begin
  ItemIndex := EnsureRange(FPenWidth - 1, 0, Items.Count - 1);
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

procedure TChartComboBox.SetShowNames(const AValue: Boolean);
begin
  if FShowNames = AValue then exit;
  FShowNames := AValue;
  Invalidate;
end;

procedure TChartComboBox.SetSymbolWidth(const AValue: Integer);
begin
  if FSymbolWidth = AValue then exit;
  FSymbolWidth := AValue;
  Invalidate;
end;

end.
