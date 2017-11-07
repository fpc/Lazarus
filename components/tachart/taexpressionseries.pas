{

 Function series with expression parser for TAChart.

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Author: Werner Pamler

}
unit TAExpressionSeries;

{$H+}

interface

uses
  Classes, Graphics, typ, Types, fpexprpars,
  TAChartUtils, TAFuncSeries;

const
  DEF_FUNC_STEP = 2;
  DEF_SPLINE_DEGREE = 3;
  DEF_SPLINE_STEP = 4;
  DEF_FIT_STEP = 4;
  DEF_FIT_PARAM_COUNT = 3;
  DEF_COLORMAP_STEP = 4;

type
  TFuncCalculateEvent = procedure (const AX: Double; out AY: Double) of object;

  TFuncSeriesStep = 1..MaxInt;

  TExpressionSeries = class(TCustomFuncSeries)
  private
    FParser: TFpExpressionParser;
    FX: TFPExprIdentifierDef;
    function GetExpression: String;
    function GetIdentifiers: TFPExprIdentifierDefs;
    procedure SetExpression(const AValue: string);
    procedure SetIdentifiers(const AValue: TFPExprIdentifierDefs);
  protected
    function DoCalculate(AX: Double): Double; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    function IsEmpty: Boolean; override;
  published
    property Expression: String read GetExpression write SetExpression;
    property Identifiers: TFPExprIdentifierDefs read GetIdentifiers Write SetIdentifiers;

  end;


implementation

uses
  Math, TAGraph;

resourcestring
  rsExpressionSeries = 'Math expression series';
  rsErrInvalidResultType = 'Expression result type must be integer or float. Got %s"';


{ TExpressionSeries }

constructor TExpressionSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParser := TFpExpressionParser.Create(self);
  FParser.BuiltIns := [bcMath];
  FX := FParser.Identifiers.AddFloatVariable('x', 0.0);
  FParser.Expression := 'x';
end;

destructor TExpressionSeries.Destroy;
begin
  FX := nil;
  inherited;
end;

procedure TExpressionSeries.Assign(ASource: TPersistent);
begin
  if ASource is TExpressionSeries then begin
    Expression := TExpressionSeries(ASource).Expression;
    Identifiers := TExpressionSeries(Asource).Identifiers;
  end;
  inherited Assign(ASource);
end;

function TExpressionSeries.DoCalculate(AX: Double): Double;
var
  res: TFPExpressionResult;
begin
  FX.AsFloat := AX;
  res := FParser.Evaluate;
  if res.ResultType=rtFloat then
    Result := res.ResFloat
  else if res.ResultType=rtInteger then
    Result := res.ResInteger
  else
    Result := NaN;  // should not happen
end;

function TExpressionSeries.GetExpression: String;
begin
  Result := FParser.Expression;
end;

function TExpressionSeries.GetIdentifiers: TFPExprIdentifierDefs;
begin
  Result := FParser.Identifiers;
end;

function TExpressionSeries.IsEmpty: Boolean;
begin
  Result := false;
end;

procedure TExpressionSeries.SetExpression(const AValue: String);
begin
  FParser.Expression := AValue;
  if not (FParser.ResultType in [rtInteger,rtFLoat]) then
    raise EExprParser.CreateFmt(rsErrInvalidResultType, [ResultTypeName(FParser.ResultType)]);
  UpdateParentChart;
end;

procedure TExpressionSeries.SetIdentifiers(const AValue: TFPExprIdentifierDefs);
begin
  FParser.Identifiers.Assign(AValue);
  // Reassign...
  FX := FParser.IdentifierByName('x');
end;


initialization
  RegisterSeriesClass(TExpressionSeries, @rsExpressionSeries);

end.

