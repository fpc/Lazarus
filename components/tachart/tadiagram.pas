{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}
unit TADiagram;

{$H+}

interface

uses
  Types,
  TAChartUtils;

type
  TDiaObject = class(TInterfacedObject)
  public
    procedure Notify(ASender: TDiaObject); virtual; abstract;
    procedure Changed(ASender: TDiaObject); virtual; abstract;
  end;

  TDiaCoordConverter = class
    function AxisToGraph(AP: TDoublePoint): TDoublePoint; virtual;
    function GraphToAxis(AP: TDoublePoint): TDoublePoint; virtual;
    function GraphToImage(AP: TDoublePoint): TPoint; virtual;
    function ImageToGraph(AP: TPoint): TDoublePoint; virtual;
    function ImageToDevice(AP: TPoint): TDoublePoint; virtual;
    function DeviceToImage(AP: TDoublePoint): TPoint; virtual;
  end;

  TDiaContext = class(TDiaObject)
  private
    FConverter: TDiaCoordConverter;
  public
    property Converter: TDiaCoordConverter read FConverter write FConverter;
  end;

  TDiaUnits = (duNone, duAxis, duGraph, duPixels, duCentimeters);
  TDiaBoxSide = (dbsTop, dbsLeft, dbsRight, dbsBottom);
  TDiaCoordinate = array [duAxis .. High(TDiaUnits)] of Double;
  TDiaPoint = object
  public
    X, Y: TDiaCoordinate;
    function AsUnits(AUnits: TDiaUnits): TDoublePoint; inline;
  end;

  TDiaElement = class;

  TDiaDecoratorList = class;

  IDiaDecorator = interface
  end;

  TDiaDecorator = class(TDiaObject, IDiaDecorator)
  private
    FOwner: TDiaObject;
  public
    constructor Create(AOwner: TDiaDecoratorList);
    property Owner: TDiaObject read FOwner;
  end;

  TDiaDecoratorEnumerator = class;

  TDiaDecoratorList = class(TDiaObject)
  private
    FDecorators: array of IDiaDecorator;
    FOwner: TDiaObject;
    procedure Add(ADecorator: IDiaDecorator);
  public
    constructor Create(AOwner: TDiaObject);
    procedure Changed(ASender: TDiaObject); override;
    procedure Notify(ASender: TDiaObject); override;
    property Owner: TDiaObject read FOwner;
    function GetEnumerator: TDiaDecoratorEnumerator;
  end;

  TDiaDecoratorEnumerator = class
    FList: TDiaDecoratorList;
    FPosition: Integer;
  public
    constructor Create(AList: TDiaDecoratorList);
    function GetCurrent: IDiaDecorator;
    function MoveNext: Boolean;
    property Current: IDiaDecorator read GetCurrent;
  end;

  TDiagram = class(TDiaObject)
  strict private
    FBounds: array [TDiaBoxSide] of TDiaCoordinate;
    FContext: TDiaContext;
    FElements: array of TDiaElement;
    function GetBounds(AIndex: TDiaBoxSide): TDiaCoordinate;
    procedure SetBounds(AIndex: TDiaBoxSide; AValue: TDiaCoordinate);
    procedure SetContext(AValue: TDiaContext);
  public
    destructor Destroy; override;
    procedure Draw;
    procedure Add(AElement: TDiaElement);
    procedure SetPixelBounds(const ARect: TRect);
    function FindElementByPoint(
      const APoint: TDoublePoint; AUnits: TDiaUnits): TDiaElement;
    procedure Changed(ASender: TDiaObject); override;
    procedure Notify(ASender: TDiaObject); override;
    property Context: TDiaContext read FContext write SetContext;
    property Bounds[AIndex: TDiaBoxSide]: TDiaCoordinate
      read GetBounds write SetBounds; default;
  end;

  TDiaConnector = class;

  TDiaElement = class(TDiaObject)
  strict private
    FConnectors: array of TDiaConnector;
    FOwner: TDiagram;
  private
    FDecorators: TDiaDecoratorList;
  protected
    procedure SetOwner(AValue: TDiagram); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function DiaPoint(AX, AY: TDiaBoxSide): TDiaPoint; inline;
    procedure Add(AConnector: TDiaConnector);
    procedure Changed(ASender: TDiaObject); override;
    procedure Draw; virtual;
    function IsPointInside(
      const APoint: TDoublePoint; AUnits: TDiaUnits): Boolean; virtual;
    procedure Notify(ASender: TDiaObject); override;
    property Owner: TDiagram read FOwner;
    property Decorators: TDiaDecoratorList read FDecorators;
  end;

  TDiaPosition = object
  strict private
    FPercentage: Boolean;
    FReverse: Boolean;
    FUnits: TDiaUnits;
    FValue: Double;
    procedure SetPercentage(AValue: Boolean);
    procedure SetReverse(AValue: Boolean);
    procedure SetUnits(AValue: TDiaUnits);
    procedure SetValue(AValue: Double);
    procedure Notify;
  private
    FOwner: TDiaElement;
  public
    constructor Init(AOwner: TDiaElement);
    procedure Assign(const ASource: TDiaPosition);
    class function Equals(const A, B: TDiaPosition): Boolean; static;
    function Calc(const A, B: TDiaPoint): TDiaPoint;

    property Owner: TDiaElement read FOwner;
  public
    function Defined: Boolean; inline;
    property Percentage: Boolean read FPercentage write SetPercentage;
    property Reverse: Boolean read FReverse write SetReverse;
    property Units: TDiaUnits read FUnits write SetUnits;
    property Value: Double read FValue write SetValue;
  end;

  TDiaCenterElement = class(TDiaElement)
  strict private
    FActualPos: TDiaPoint;
    FLeft: TDiaPosition;
    FTop: TDiaPosition;
    procedure SetLeft(AValue: TDiaPosition);
    procedure SetTop(AValue: TDiaPosition);
  public
    constructor Create;
    procedure Changed(ASender: TDiaObject); override;
  public
    property ActualPos: TDiaPoint read FActualPos;
    property Left: TDiaPosition read FLeft write SetLeft;
    property Top: TDiaPosition read FTop write SetTop;
  end;

  TDiaBox = class(TDiaElement)
  strict private
    FBottom: TDiaPosition;
    FCaption: String;
    FHeight: TDiaPosition;
    FLeft: TDiaPosition;
    FRight: TDiaPosition;
    FTop: TDiaPosition;
    FWidth: TDiaPosition;
    procedure SetBottom(const AValue: TDiaPosition);
    procedure SetCaption(AValue: String);
    procedure SetHeight(const AValue: TDiaPosition);
    procedure SetLeft(const AValue: TDiaPosition);
    procedure SetRight(const AValue: TDiaPosition);
    procedure SetTop(const AValue: TDiaPosition);
    procedure SetWidth(const AValue: TDiaPosition);
  public
    FBottomLeft: TDiaPoint;
    FBottomRight: TDiaPoint;
    FTopLeft: TDiaPoint;
    FTopRight: TDiaPoint;
  class var
    FInternalDraw: procedure (ASelf: TDiaBox);
  public
    constructor Create;
    procedure Draw; override;
    procedure Changed(ASender: TDiaObject); override;
    function IsPointInside(
      const APoint: TDoublePoint; AUnits: TDiaUnits): Boolean; override;
  public
    property Left: TDiaPosition read FLeft write SetLeft;
    property Top: TDiaPosition read FTop write SetTop;
    property Right: TDiaPosition read FRight write SetRight;
    property Bottom: TDiaPosition read FBottom write SetBottom;
    property Caption: String read FCaption write SetCaption;
    //property Height: TDiaPosition read FHeight write SetHeight;
    //property Width: TDiaPosition read FWidth write SetWidth;
  end;

  TDiaConnector = class(TDiaObject)
  strict private
    FElement: TDiaElement;
    FPosition: TDiaPosition;
    procedure SetPosition(const AValue: TDiaPosition);
  private
    FActualPos: TDiaPoint;
    procedure SetElement(AValue: TDiaElement);
  public
    constructor Create;
    procedure Changed(ASender: TDiaObject); override;
    procedure Notify(ASender: TDiaObject); override;
    property ActualPos: TDiaPoint read FActualPos;
    property Element: TDiaElement read FElement;
    property Position: TDiaPosition read FPosition write SetPosition;
  end;

  TDiaCenterConnector = class(TDiaConnector)
  public
    procedure Changed(ASender: TDiaObject); override;
  end;

  TDiaBoxConnector = class(TDiaConnector)
  private
    FSide: TDiaBoxSide;
    procedure SetSide(AValue: TDiaBoxSide);
  public
    procedure Changed(ASender: TDiaObject); override;
  published
    property Side: TDiaBoxSide read FSide write SetSide;
  end;

  TDiaEndPointShape = (depsNone, depsOpenArrow, depsClosedArrow);

  TDiaEndPoint = class(TDiaElement)
  private
    FConnector: TDiaConnector;
    FLength: TDiaPosition;
    FShape: TDiaEndPointShape;
    FWidth: TDiaPosition;
    procedure SetConnector(AValue: TDiaConnector);
    procedure SetLength(const AValue: TDiaPosition);
    procedure SetShape(AValue: TDiaEndPointShape);
    procedure SetWidth(const AValue: TDiaPosition);
  public
    constructor Create;
    property Connector: TDiaConnector read FConnector write SetConnector;
    property Length: TDiaPosition read FLength write SetLength;
    property Shape: TDiaEndPointShape read FShape write SetShape;
    property Width: TDiaPosition read FWidth write SetWidth;
  end;

  TDiaLinkRouting = (dlrStraight, dlrXThenY, dlrYThenX);

  TDiaLink = class(TDiaElement)
  private
    FFinish: TDiaEndPoint;
    FRouting: TDiaLinkRouting;
    FStart: TDiaEndPoint;
    procedure SetFinish(AValue: TDiaEndPoint);
    procedure SetRouting(AValue: TDiaLinkRouting);
    procedure SetStart(AValue: TDiaEndPoint);
  protected
    procedure SetOwner(AValue: TDiagram); override;
  public
    class var
      FInternalDraw: procedure (ASelf: TDiaLink);
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
  published
    property Start: TDiaEndPoint read FStart write SetStart;
    property Finish: TDiaEndPoint read FFinish write SetFinish;
    property Routing: TDiaLinkRouting read FRouting write SetRouting;
  end;

  function DiaPos(
    AValue: Double; AUnits: TDiaUnits = duNone;
    AReverse: Boolean = false; APercentage: Boolean = false): TDiaPosition; inline;
  operator =(const A, B: TDiaPosition): Boolean; overload; inline;

implementation

uses
  SysUtils,
  TAGeometry;

const
  DEFAULT_DPI = 72;
  CM_PER_INCH = 2.54;

function XY(const AX, AY: TDiaPoint): TDiaPoint;
var
  u: TDiaUnits;
begin
  for u in TDiaUnits do begin
    Result.X[u] := AX.X[u];
    Result.Y[u] := AY.Y[u];
  end;
end;

function DiaPos(
  AValue: Double; AUnits: TDiaUnits;
  AReverse: Boolean; APercentage: Boolean): TDiaPosition;
begin
  Result.Init(nil);
  Result.Value := AValue;
  Result.Units := AUnits;
  Result.Reverse := AReverse;
  Result.Percentage := APercentage;
end;

function PointDist(const A, B: TDoublePoint): Double; overload;
begin
  Result := Sqrt(Sqr(A.X - B.X) + Sqr(A.Y - B.Y));
end;

function WeightedAverage(
  const AP1, AP2: TDoublePoint; ACoeff: Double): TDoublePoint; overload; inline;
begin
  Result.X := WeightedAverage(AP1.X, AP2.X, ACoeff);
  Result.Y := WeightedAverage(AP1.Y, AP2.Y, ACoeff);
end;

operator = (const A, B: TDiaPosition): Boolean;
begin
  Result := TDiaPosition.Equals(A, B);
end;

{ TDiaCenterElement }

procedure TDiaCenterElement.Changed(ASender: TDiaObject);
var
  tl, tr, bl{, br}: TDiaPoint;
begin
  tl := DiaPoint(dbsLeft, dbsTop);
  tr := DiaPoint(dbsRight, dbsTop);
  bl := DiaPoint(dbsLeft, dbsBottom);
  // br := DiaPoint(dbsRight, dbsBottom);  // Commenting to silence the compilation note on unused variable...
  if Left.Defined and Top.Defined then
    FActualPos := XY(Left.Calc(tl, tr), Top.Calc(tl, bl));
  inherited;
end;

constructor TDiaCenterElement.Create;
begin
  FLeft.Init(Self);
  FTop.Init(Self);
end;

procedure TDiaCenterElement.SetLeft(AValue: TDiaPosition);
begin
  if FLeft = AValue then exit;
  FLeft.Assign(AValue);
end;

procedure TDiaCenterElement.SetTop(AValue: TDiaPosition);
begin
  if FTop = AValue then exit;
  FTop.Assign(AValue);
end;

{ TDiaPoint }

function TDiaPoint.AsUnits(AUnits: TDiaUnits): TDoublePoint;
begin
  Result.X := X[AUnits];
  Result.Y := Y[AUnits];
end;

{ TDiaEndPoint }

constructor TDiaEndPoint.Create;
begin
  inherited;
  FLength.Init(Self);
  FWidth.Init(Self);
end;

procedure TDiaEndPoint.SetConnector(AValue: TDiaConnector);
begin
  if FConnector = AValue then exit;
  FConnector := AValue;
  Notify(Self);
end;

procedure TDiaEndPoint.SetLength(const AValue: TDiaPosition);
begin
  if FLength = AValue then exit;
  FLength.Assign(AValue);
end;

procedure TDiaEndPoint.SetShape(AValue: TDiaEndPointShape);
begin
  if FShape = AValue then exit;
  FShape := AValue;
  Notify(Self);
end;

procedure TDiaEndPoint.SetWidth(const AValue: TDiaPosition);
begin
  if FWidth = AValue then exit;
  FWidth.Assign(AValue);
end;

{ TDiaDecoratorEnumerator }

constructor TDiaDecoratorEnumerator.Create(AList: TDiaDecoratorList);
begin
  FList := AList;
  FPosition := -1;
end;

function TDiaDecoratorEnumerator.GetCurrent: IDiaDecorator;
begin
  Result := FList.FDecorators[FPosition];
end;

function TDiaDecoratorEnumerator.MoveNext: Boolean;
begin
  FPosition += 1;
  Result := FPosition < Length(FList.FDecorators);
end;

{ TDiaDecoratorList }

procedure TDiaDecoratorList.Add(ADecorator: IDiaDecorator);
begin
  SetLength(FDecorators, Length(FDecorators) + 1);
  FDecorators[High(FDecorators)] := ADecorator;
  //Notify(ADecorator);
end;

constructor TDiaDecoratorList.Create(AOwner: TDiaObject);
begin
  FOwner := AOwner;
end;

procedure TDiaDecoratorList.Changed(ASender: TDiaObject);
begin
  // Remove compiler warning of abstract method
  Unused(ASender);
end;

function TDiaDecoratorList.GetEnumerator: TDiaDecoratorEnumerator;
begin
  Result := TDiaDecoratorEnumerator.Create(Self);
end;

procedure TDiaDecoratorList.Notify(ASender: TDiaObject);
begin
  // Remove compiler warning of abstract method
  Unused(ASender);
end;

{ TDiaDecorator }

constructor TDiaDecorator.Create(AOwner: TDiaDecoratorList);
begin
  if AOwner <> nil then
    AOwner.Add(Self);
end;

{ TDiaLink }

constructor TDiaLink.Create;
begin
  inherited;
  FStart := TDiaEndPoint.Create;
  FFinish := TDiaEndPoint.Create;
end;

destructor TDiaLink.Destroy;
begin
  FreeAndNil(FStart);
  FreeAndNil(FFinish);
  inherited;
end;

procedure TDiaLink.Draw;
begin
  inherited;
  if FInternalDraw <> nil then
    FInternalDraw(Self);
end;

procedure TDiaLink.SetFinish(AValue: TDiaEndPoint);
begin
  if FFinish = AValue then exit;
  FFinish := AValue;
  Notify(Self);
end;

procedure TDiaLink.SetOwner(AValue: TDiagram);
begin
  inherited SetOwner(AValue);
  FStart.SetOwner(AValue);
  FFinish.SetOwner(AValue);
end;

procedure TDiaLink.SetRouting(AValue: TDiaLinkRouting);
begin
  if FRouting = AValue then exit;
  FRouting := AValue;
  Notify(Self);
end;

procedure TDiaLink.SetStart(AValue: TDiaEndPoint);
begin
  if FStart = AValue then exit;
  FStart := AValue;
  Notify(Self);
end;

{ TDiaCenterConnector }

procedure TDiaCenterConnector.Changed(ASender: TDiaObject);
begin
  inherited;
  if Element is TDiaCenterElement then
    FActualPos := (Element as TDiaCenterElement).ActualPos;
end;

{ TDiaBoxConnector }

procedure TDiaBoxConnector.Changed(ASender: TDiaObject);
var
  b: TDiaBox;
begin
  inherited;
  if not (Element is TDiaBox) then exit;
  b := Element as TDiaBox;
  case Side of
    dbsTop: FActualPos := Position.Calc(b.FTopLeft, b.FTopRight);
    dbsBottom: FActualPos := Position.Calc(b.FBottomLeft, b.FBottomRight);
    dbsLeft: FActualPos := Position.Calc(b.FTopLeft, b.FBottomLeft);
    dbsRight: FActualPos := Position.Calc(b.FTopRight, b.FBottomRight);
  end;
end;

procedure TDiaBoxConnector.SetSide(AValue: TDiaBoxSide);
begin
  if FSide = AValue then Exit;
  FSide := AValue;
  Notify(Self);
end;

{ TDiaConnector }

procedure TDiaConnector.Changed(ASender: TDiaObject);
begin
  Unused(ASender);
end;

constructor TDiaConnector.Create;
begin
  FPosition.Init(Element);
end;

procedure TDiaConnector.Notify(ASender: TDiaObject);
begin
  if Element <> nil then
    Element.Notify(ASender);
end;

procedure TDiaConnector.SetElement(AValue: TDiaElement);
begin
  if FElement = AValue then exit;
  Notify(Self);
  FElement := AValue;
  FPosition.FOwner := FElement;
  Notify(Self);
end;

procedure TDiaConnector.SetPosition(const AValue: TDiaPosition);
begin
  if FPosition = AValue then exit;
  FPosition.Assign(AValue);
end;

{ TDiaCoordConverter }

function TDiaCoordConverter.AxisToGraph(AP: TDoublePoint): TDoublePoint;
begin
  Result := AP;
end;

function TDiaCoordConverter.DeviceToImage(AP: TDoublePoint): TPoint;
begin
  Result := RoundPoint(AP * (DEFAULT_DPI / CM_PER_INCH));
end;

function TDiaCoordConverter.GraphToAxis(AP: TDoublePoint): TDoublePoint;
begin
  Result := AP;
end;

function TDiaCoordConverter.GraphToImage(AP: TDoublePoint): TPoint;
begin
  Result := RoundPoint(AP);
end;

function TDiaCoordConverter.ImageToDevice(AP: TPoint): TDoublePoint;
begin
  Result := DoublePoint(AP) * (CM_PER_INCH / DEFAULT_DPI);
end;

function TDiaCoordConverter.ImageToGraph(AP: TPoint): TDoublePoint;
begin
  Result := DoublePoint(AP);
end;

{ TDiaBox }

procedure TDiaBox.Changed(ASender: TDiaObject);
var
  tl, tr, bl, br: TDiaPoint;
begin
  tl := DiaPoint(dbsLeft, dbsTop);
  tr := DiaPoint(dbsRight, dbsTop);
  bl := DiaPoint(dbsLeft, dbsBottom);
  br := DiaPoint(dbsRight, dbsBottom);
  if Left.Defined and Top.Defined then
    FTopLeft := XY(Left.Calc(tl, tr), Top.Calc(tl, bl));
  if Right.Defined and Top.Defined then
    FTopRight := XY(Right.Calc(tr, tl), Top.Calc(tr, br));
  if Left.Defined and Bottom.Defined then
    FBottomLeft := XY(Left.Calc(tl, tr), Bottom.Calc(bl, tl));
  if Right.Defined and Bottom.Defined then
    FBottomRight := XY(Right.Calc(br, bl), Bottom.Calc(br, tr));
  inherited;
end;

constructor TDiaBox.Create;
begin
  inherited;
  FLeft.Init(Self);
  FRight.Init(Self);
  FTop.Init(Self);
  FBottom.Init(Self);
end;

procedure TDiaBox.Draw;
begin
  if FInternalDraw <> nil then
    FInternalDraw(Self);
end;

function TDiaBox.IsPointInside(
  const APoint: TDoublePoint; AUnits: TDiaUnits): Boolean;
begin
  Result := IsPointInPolygon(RoundPoint(APoint), [
    RoundPoint(FTopLeft.AsUnits(AUnits)),
    RoundPoint(FTopRight.AsUnits(AUnits)),
    RoundPoint(FBottomRight.AsUnits(AUnits)),
    RoundPoint(FBottomLeft.AsUnits(AUnits))
  ]);
end;

procedure TDiaBox.SetBottom(const AValue: TDiaPosition);
begin
  if FBottom = AValue then exit;
  FBottom.Assign(AValue);
end;

procedure TDiaBox.SetCaption(AValue: String);
begin
  if FCaption = AValue then exit;
  FCaption := AValue;
  Notify(Self);
end;

procedure TDiaBox.SetHeight(const AValue: TDiaPosition);
begin
  if FHeight = AValue then exit;
  FHeight.Assign(AValue);
end;

procedure TDiaBox.SetLeft(const AValue: TDiaPosition);
begin
  if FLeft = AValue then exit;
  FLeft.Assign(AValue);
end;

procedure TDiaBox.SetRight(const AValue: TDiaPosition);
begin
  if FRight = AValue then exit;
  FRight.Assign(AValue);
end;

procedure TDiaBox.SetTop(const AValue: TDiaPosition);
begin
  if FTop = AValue then exit;
  FTop.Assign(AValue);
end;

procedure TDiaBox.SetWidth(const AValue: TDiaPosition);
begin
  if FWidth = AValue then exit;
  FWidth.Assign(AValue);
end;

{ TDiagram }

procedure TDiagram.Add(AElement: TDiaElement);
begin
  if AElement.Owner = Self then exit;
  SetLength(FElements, Length(FElements) + 1);
  FElements[High(FElements)] := AElement;
  AElement.SetOwner(Self);
end;

procedure TDiagram.Changed(ASender: TDiaObject);
var
  e: TDiaElement;
begin
  for e in FElements do
    e.Changed(ASender);
end;

destructor TDiagram.Destroy;
var
  e: TDiaElement;
begin
  for e in FElements do
    e.Free;
  inherited;
end;

procedure TDiagram.Draw;
var
  e: TDiaElement;
begin
  for e in FElements do
    e.Draw;
end;

function TDiagram.FindElementByPoint(
  const APoint: TDoublePoint; AUnits: TDiaUnits): TDiaElement;
begin
  for Result in FElements do
    if Result.IsPointInside(APoint, AUnits) then exit;
  Result := nil;
end;

function TDiagram.GetBounds(AIndex: TDiaBoxSide): TDiaCoordinate;
begin
  Result := FBounds[AIndex];
end;

procedure TDiagram.Notify(ASender: TDiaObject);
begin
  Changed(ASender);
end;

procedure TDiagram.SetBounds(AIndex: TDiaBoxSide; AValue: TDiaCoordinate);
begin
  FBounds[AIndex] := AValue;
  Notify(Self);
end;

procedure TDiagram.SetContext(AValue: TDiaContext);
begin
  if FContext = AValue then exit;
  FContext := AValue;
  Notify(AValue);
end;

procedure TDiagram.SetPixelBounds(const ARect: TRect);
begin
  FBounds[dbsTop][duPixels] := ARect.Top;
  FBounds[dbsBottom][duPixels] := ARect.Bottom;
  FBounds[dbsLeft][duPixels] := ARect.Left;
  FBounds[dbsRight][duPixels] := ARect.Right;
  Notify(Self);
end;

{ TDiaElement }

procedure TDiaElement.Add(AConnector: TDiaConnector);
begin
  if AConnector.Element = Self then exit;
  SetLength(FConnectors, Length(FConnectors) + 1);
  FConnectors[High(FConnectors)] := AConnector;
  AConnector.SetElement(Self);
end;

procedure TDiaElement.Changed(ASender: TDiaObject);
var
  c: TDiaConnector;
begin
  for c in FConnectors do
    c.Changed(ASender);
end;

constructor TDiaElement.Create;
begin
  FDecorators := TDiaDecoratorList.Create(Self);
end;

destructor TDiaElement.Destroy;
begin
  FreeAndNil(FDecorators);
  inherited;
end;

function TDiaElement.DiaPoint(AX, AY: TDiaBoxSide): TDiaPoint;
begin
  Result.X := Owner[AX];
  Result.Y := Owner[AY];
end;

procedure TDiaElement.Draw;
begin
  //
end;

function TDiaElement.IsPointInside(
  const APoint: TDoublePoint; AUnits: TDiaUnits): Boolean;
begin
  Unused(APoint, AUnits);
  Result := false;
end;

procedure TDiaElement.Notify(ASender: TDiaObject);
begin
  if Owner <> nil then
    Owner.Notify(ASender);
end;

procedure TDiaElement.SetOwner(AValue: TDiagram);
begin
  if FOwner = AValue then Exit;
  Notify(Self);
  FOwner := AValue;
  Notify(Self);
end;

{ TDiaPosition }

procedure TDiaPosition.Assign(const ASource: TDiaPosition);
begin
  Self.FPercentage := ASource.Percentage;
  Self.FReverse := ASource.Reverse;
  Self.FUnits := ASource.Units;
  Self.FValue := ASource.Value;
  Notify;
end;

function TDiaPosition.Calc(const A, B: TDiaPoint): TDiaPoint;
var
  d: TDiagram;
  conv: TDiaCoordConverter;
  w: Double;
  pa, pb: TDoublePoint;
  ap, gp, dp: TDoublePoint;
  ip: TPoint;
begin
  if Owner = nil then exit;
  d := Owner.Owner;
  conv := d.Context.Converter;
  pa := DoublePoint(A.X[Units], A.Y[Units]);
  pb := DoublePoint(B.X[Units], B.Y[Units]);
  if Percentage then
    w := Value * PERCENT
  else begin
    w := PointDist(pa, pb);
    if w > 0 then
      w := Value / w;
  end;
  if Reverse then
    w := 1.0 - w;
  case Units of
    duNone: ;
    duAxis: begin
      ap := WeightedAverage(pa, pb, w);
      gp := conv.AxisToGraph(ap);
      ip := conv.GraphToImage(gp);
      dp := conv.ImageToDevice(ip);
    end;
    duGraph: begin
      gp := WeightedAverage(pa, pb, w);
      ap := conv.GraphToAxis(gp);
      ip := conv.GraphToImage(gp);
      dp := conv.ImageToDevice(ip);
    end;
    duPixels: begin
      ip := RoundPoint(WeightedAverage(pa, pb, w));
      gp := conv.ImageToGraph(ip);
      ap := conv.GraphToAxis(gp);
      dp := conv.ImageToDevice(ip);
    end;
    duCentimeters: begin
      dp := WeightedAverage(pa, pb, w);
      ip := conv.DeviceToImage(dp);
      gp := conv.ImageToGraph(ip);
      ap := conv.GraphToAxis(gp);
    end;
  end;
  Result.X[duAxis] := ap.X;
  Result.Y[duAxis] := ap.Y;
  Result.X[duGraph] := gp.X;
  Result.Y[duGraph] := gp.Y;
  Result.X[duPixels] := ip.X;
  Result.Y[duPixels] := ip.Y;
  Result.X[duCentimeters] := dp.X;
  Result.Y[duCentimeters] := dp.Y;
end;

function TDiaPosition.Defined: Boolean;
begin
  Result := Units <> duNone;
end;

class function TDiaPosition.Equals(const A, B: TDiaPosition): Boolean;
begin
  Result :=
    (A.Percentage = B.Percentage) and
    (A.Reverse = B.Reverse) and
    (A.Units = B.Units) and
    (A.Value = B.Value);
end;

constructor TDiaPosition.Init(AOwner: TDiaElement);
begin
  FOwner := AOwner;
  FPercentage := false;
  FReverse := false;
  FUnits := duNone;
  FValue := 0.0;
end;

procedure TDiaPosition.Notify;
begin
  if Owner <> nil then
    Owner.Notify(Owner);
end;

procedure TDiaPosition.SetPercentage(AValue: Boolean);
begin
  if FPercentage = AValue then Exit;
  FPercentage := AValue;
  Notify;
end;

procedure TDiaPosition.SetReverse(AValue: Boolean);
begin
  if FReverse = AValue then exit;
  FReverse := AValue;
  Notify;
end;

procedure TDiaPosition.SetUnits(AValue: TDiaUnits);
begin
  if FUnits = AValue then exit;
  FUnits := AValue;
  Notify;
end;

procedure TDiaPosition.SetValue(AValue: Double);
begin
  if FValue = AValue then exit;
  FValue := AValue;
  Notify;
end;

end.

