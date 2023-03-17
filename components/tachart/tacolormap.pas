unit TAColorMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  TAChartUtils, TACustomSource, TASources, TACustomSeries;

type
  TColorMapPalette = (cmpHot, cmpCold, cmpRainbow, cmpMonochrome);

  TColorMap = class(TComponent)
  private
    FBuiltinColorSource: TListChartSource;
    FBuiltinPalette: TColormapPalette;
    FColorExtentMin, FColorExtentMax: Double;
    FColorSource: TCustomChartSource;
    FColorSourceListener: TListener;
    FInterpolate: Boolean;
    FNumXCountNeeded: Integer;
    FNumYCountNeeded: Integer;
    FOwnerSeries: TCustomChartSeries;
    FPaletteMax: Double;
    FPaletteMin: Double;
    FOnChanged: TNotifyEvent;
    function GetColorSource: TCustomChartSource;
    procedure SetBuiltinPalette(AValue: TColorMapPalette);
    procedure SetColorSource(AValue: TCustomChartSource);
    procedure SetPaletteMax(AValue: Double);
    procedure SetPaletteMin(AValue: Double);
  protected
    procedure BuildPalette(APalette: TColorMapPalette);
    procedure CheckColorSource(ASource: TCustomChartSource);
    procedure ColorMapChanged(Sender: TObject); virtual;
    procedure ColorSourceChanged(Sender: TObject); virtual;
    function OwnerSeries: TCustomChartSeries; inline;
    procedure UpdateColorExtent;
  public
    constructor Create(AOwner: TCustomChartSeries; ANumXCountNeeded, ANumYCountNeeded: Integer); reintroduce;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    function ColorByValue(AValue: Double): TColor;
    function IsColorSourceStored: boolean;
    function IsPaletteMaxStored: Boolean;
    function IsPaletteMinStored: Boolean;

    // Read-only properties
    property BuiltinColorSource: TListChartSource read FBuiltinColorSource;
    property ColorExtentMax: Double read FColorExtentMax;
    property ColorExtentMin: Double read FColorExtentMin;

    // Read/write properties
    property BuiltinPalette: TColormapPalette
      read FBuiltInPalette write SetBuiltinPalette;
    property ColorSource: TCustomChartSource
      read GetColorSource write SetColorSource;
    property Interpolate: Boolean
      read FInterpolate write FInterpolate;
    property PaletteMax: Double
      read FPaletteMax write SetPaletteMax;
    property PaletteMin: Double
      read FPaletteMin write SetPaletteMin;
    property OnChanged: TNotifyEvent
      read FOnChanged write FOnChanged;
  end;


implementation

uses
  GraphUtil, Math,
  TAChartStrConsts, TAMath;

constructor TColorMap.Create(AOwner: TCustomChartSeries;
  ANumXCountNeeded, ANumYCountNeeded: Integer);
const
  BUILTIN_SOURCE_NAME = 'BuiltinColors';
begin
  inherited Create(AOwner);
  FNumXCountNeeded := ANumXCountNeeded;
  FNumYCountNeeded := ANumYCountNeeded;
  FColorSourceListener := TListener.Create(@FColorSource, @ColorSourceChanged);
  FBuiltinColorSource := TBuiltinListChartSource.Create(self, ANumXCountNeeded, ANumYCountNeeded);
  FBuiltinColorSource.XCount := ANumXCountNeeded;
  FBuiltinColorSource.YCount := ANumYCountNeeded;
  FBuiltinColorSource.Name := BUILTIN_SOURCE_NAME;
  FBuiltinColorSource.Broadcaster.Subscribe(FColorSourceListener);
end;

destructor TColorMap.Destroy;
begin
  FreeAndNil(FBuiltinColorSource);
  FreeAndNil(FColorSourceListener);
  inherited Destroy;
end;

procedure TColorMap.Assign(ASource: TPersistent);
begin
  if ASource is TColorMap then
    with TColorMap(ASource) do begin
      Self.BuiltinPalette := FBuiltinPalette;
      {
      Self.BuiltinPaletteMax := FPaletteMax;
      Self.BuiltinPaletteMin := FPaletteMin;
      }
      Self.ColorSource := FColorSource;
      Self.FInterpolate := FInterpolate;
      Self.FNumXCountNeeded := FNumXCountNeeded;
      Self.FNumYCountNeeded := FNumYCountNeeded;
    end;
  inherited Assign(ASource);
end;

procedure TColorMap.BuildPalette(APalette: TColorMapPalette);
var
  i: Integer;
  h, s, l: Byte;
  cmax, cmin, factor: Double;
  ex: TDoubleRect;
begin
  with FBuiltinColorSource do begin
    BeginUpdate;
    try
      Clear;
      case APalette of
        cmpHot:
          begin
            Add(0, 0, '', clBlack);
            Add(1/3, 0, '', clRed);
            Add(2/3, 0, '', clYellow);
            Add(1, 0, '', clWhite);
          end;
        cmpCold:
          begin
            ColorToHLS(clBlue, h, l, s);
            i := 0;
            while i <= 255 do begin
              Add(i, 0, '', HLSToColor(h, i, s));
              inc(i, 32);
            end;
            Add(255, 0, '', clWhite);
          end;
        cmpRainbow:
          begin
            i := 0;
            while i <= 255 do begin      // i is hue
              Add(i, 0, '', HLSToColor(i, 128, 255));
              inc(i, 32);
            end;
            Add(255, 0, '', HLSToColor(255, 128, 255));
          end;
        cmpMonochrome:
          begin
            i := 0;
            while i <= 255 do begin
              Add(i, 0, '', RgbToColor(i, i, i));
              inc(i, 32);
            end;
            Add(255, 0, '', clWhite);
          end;
      else
        raise EChartError.Create('[TAColorMap.BuildPalette] Palette kind not supported.');
      end;

      if FPaletteMin < FPaletteMax then begin
        cmin := FPaletteMin;
        cmax := FPaletteMax;
       end else
      if FPaletteMax < FPaletteMin then begin
        cmin := FPaletteMax;
        cmax := FPaletteMin;
      end else
        exit;

      ex := Extent;
      if (ex.a.x = ex.b.x) then
        exit;
      factor := (cmax - cmin) / (ex.b.x - ex.a.x);
      for i:=0 to Count-1 do
        Item[i]^.X := (Item[i]^.X - ex.a.x) * factor + cmin;
    finally
      EndUpdate;
    end;
  end;
end;

function TColorMap.ColorByValue(AValue: Double): TColor;
var
  lb, ub: Integer;
  c1, c2: TColor;
  v1, v2: Double;
begin
  if (ColorSource = nil) or (ColorSource.Count = 0) then
    exit(clTAColor);
  ColorSource.FindBounds(AValue, SafeInfinity, lb, ub);
  if Interpolate and InRange(lb, 1, ColorSource.Count - 1) then begin
    with ColorSource[lb - 1]^ do begin
      v1 := X;
      c1 := Color;
    end;
    with ColorSource[lb]^ do begin
      v2 := X;
      c2 := Color;
    end;
    if v2 <= v1 then
      Result := c1
    else
      Result := InterpolateRGB(c1, c2, (AValue - v1) / (v2 - v1));
  end
  else
    Result := ColorSource[EnsureRange(lb, 0, ColorSource.Count - 1)]^.Color;
end;

procedure TColorMap.CheckColorSource(ASource: TCustomChartSource);
begin
  if ASource = nil then
    exit;
  if ASource.XCount < FNumXCountNeeded then
    raise EXCountError.CreateFmt(rsSourceCountError, [ClassName, FNumXCountNeeded, 'x']);
  if ASource.YCount < FNumYCountNeeded then
    raise EYCountError.CreateFmt(rsSourceCountError, [ClassName, FNumYCountNeeded, 'y']);
end;

procedure TColorMap.ColorMapChanged(Sender: TObject);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Sender);
end;

procedure TColorMap.ColorSourceChanged(Sender: TObject);
begin
  if (Sender <> FBuiltinColorSource) and (Sender is TCustomChartSource) then
    try
      CheckColorSource(TCustomChartSource(Sender));
    except
      ColorSource := nil; // revert to built-in source
      raise;
    end;
  UpdateColorExtent;
  ColorMapChanged(Sender);
end;

function TColorMap.GetColorSource: TCustomChartSource;
begin
  if Assigned(FColorSource) then
    Result := FColorSource
  else
    Result := FBuiltinColorSource;
end;

function TColorMap.IsColorSourceStored: boolean;
begin
  Result := FColorSource <> nil;
end;

function TColorMap.IsPaletteMaxStored: Boolean;
begin
  Result := FPaletteMax <> 0;
end;

function TColorMap.IsPaletteMinStored: Boolean;
begin
  Result := FPaletteMin <> 0;
end;

function TColorMap.OwnerSeries: TCustomChartSeries;
begin
  Result := Owner as TCustomChartSeries;
end;

procedure TColorMap.SetBuiltinPalette(AValue: TColorMapPalette);
begin
  FBuiltinPalette := AValue;
  BuildPalette(FBuiltinPalette);
end;

procedure TColorMap.SetColorSource(AValue: TCustomChartSource);
begin
  if AValue = FBuiltinColorSource then
    AValue := nil;
  if FColorSource = AValue then
    exit;
  CheckColorSource(AValue);
  if FColorSourceListener.IsListening then
    ColorSource.Broadcaster.Unsubscribe(FColorSourceListener);
  FColorSource := AValue;
  ColorSource.Broadcaster.Subscribe(FColorSourceListener);
  ColorSourceChanged(Self);
end;

procedure TColorMap.SetPaletteMax(AValue: Double);
begin
  if AValue = FPaletteMax then exit;
  FPaletteMax := AValue;
  BuildPalette(FBuiltinPalette);
end;

procedure TColorMap.SetPaletteMin(AValue: Double);
begin
  if AValue = FPaletteMin then exit;
  FPaletteMin := AValue;
  BuildPalette(FBuiltinPalette);
end;

procedure TColorMap.UpdateColorExtent;
var
  ext: TDoubleRect;
begin
  if (csDestroying in ComponentState) then
    exit;

  ext := ColorSource.Extent;
  FColorExtentMin := ext.a.x;
  FColorExtentMax := ext.b.x;
end;

end.

