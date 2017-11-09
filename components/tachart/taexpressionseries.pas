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
  Classes, Graphics, typ, Types, SysUtils, fpexprpars,
  TAChartUtils, TAFuncSeries;

type
  TExpressionSeries = class;

  TChartExprParam = class(TCollectionItem)
  private
    FName: String;
    FValue: Double;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Name: String read FName write FName;
    property Value: Double read FValue write FValue;
  end;

  TChartExprParams = class(TCollection)
  private
    FParser: TFpExpressionParser;
    function GetP(AIndex: Integer): TChartExprParam;
    procedure SetP(AIndex: Integer; AValue: TChartExprParam);
  protected
    procedure UpdateIdentifier(const AName: String; const AValue: Double);
  public
    function AddParam(const AName: String; const AValue: Double): TChartExprParam;
    procedure Update(AItem: TCollectionItem); override;
    property Params[AIndex: Integer]: TChartExprParam read GetP write SetP; default;
  end;

  TDomainParts = array[0..4] of String;
  // parts of a domain expression
  // e.g. '1>= x >0' has the parts '1', '>=', 'x', '>', '0'

  TChartDomainScanner = class
  private
    FSeries: TExpressionSeries;
    FParser: TFpExpressionParser;
    FExpression: String;
    FEpsilon: Double;
    function GetVariable: String;
  protected
    procedure Analyze(AList, ADomain: TIntervalList; const AParts: TDomainParts);
    procedure ConvertToExclusions(AList, ADomain: TIntervalList);
    procedure ExpressionError;
    procedure ParseExpression(AList, ADomain: TIntervalList);
  public
    constructor Create(ASeries: TExpressionSeries);
    procedure ExtractDomainExclusions(AList: TIntervalList);
    property Epsilon: Double read FEpsilon write FEpsilon;
    property Expression: String read FExpression write FExpression;
    property Variable: String read GetVariable;
  end;

  TExpressionSeries = class(TCustomFuncSeries)
  private
    FParser: TFpExpressionParser;
    FDomainScanner: TChartDomainScanner;
    FParams: TChartExprParams;
    FX: TFPExprIdentifierDef;
    FVariable: String;
    function GetDomain: String;
    function GetDomainEpsilon: Double;
    function GetExpression: String;
    procedure SetDomain(const AValue: String);
    procedure SetDomainEpsilon(const AValue: Double);
    procedure SetExpression(const AValue: string);
    procedure SetParams(const AValue: TChartExprParams);
    procedure SetVariable(const AValue: String);
  protected
    function DoCalculate(AX: Double): Double; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    function IsEmpty: Boolean; override;
  published
    property Domain: String read GetDomain write SetDomain;
    property DomainEpsilon: Double read GetDomainEpsilon write SetDomainEpsilon;
    property Expression: String read GetExpression write SetExpression;
    property Params: TChartExprParams read FParams write SetParams;
    property Variable: String read FVariable write SetVariable;
  end;


implementation

uses
  Math, TAGraph, TAChartStrConsts;

  { TChartExprParam }

procedure TChartExprParam.Assign(Source: TPersistent);
begin
  if Source is TChartExprParam then begin
    FName := TChartExprParam(Source).Name;
    FValue := TChartExprParam(Source).Value;
  end else
    inherited Assign(Source);
end;


{ TChartExprParams }

function TChartExprParams.AddParam(const AName: String;
  const AValue: Double): TChartExprParam;
begin
  Result := Add as TChartExprParam;
  Result.FName := AName;
  Result.FValue := AValue;
  UpdateIdentifier(AName, AValue);
  //Changed;
end;

function TChartExprParams.GetP(AIndex: Integer): TChartExprParam;
begin
  Result := TChartExprParam(Items[AIndex]);
end;

procedure TChartExprParams.SetP(AIndex: Integer;
  AValue: TChartExprParam);
begin
  Items[AIndex] := AValue;
  UpdateIdentifier(AValue.Name, AValue.Value);
end;

procedure TChartExprParams.Update(AItem: TCollectionItem);
var
  p: TChartExprParam;
begin
  if Assigned(FParser) then begin
    p := TChartExprParam(AItem);
    if p <> nil then UpdateIdentifier(p.Name, p.Value);
  end;
end;

procedure TChartExprParams.UpdateIdentifier(const AName: String;
  const AValue: Double);
var
  ident: TFpExprIdentifierDef;
  s: String;
begin
  Str(AValue:0, s);
  ident := FParser.Identifiers.FindIdentifier(AName);
  if ident = nil then
    FParser.Identifiers.AddFloatVariable(AName, AValue)
  else
    ident.Value := s;
end;


{ TChartDomainScanner }

constructor TChartDomainScanner.Create(ASeries: TExpressionSeries);
begin
  FSeries := ASeries;
  FParser := ASeries.FParser;
end;

{ Analyzes the parts of the domain expression and extract the intervals on
  which the function is defined (--> ADomain).
  NOTE: Although supported by the scanner the method does not distinguish
  between cases < and <=, as well as between > and >=. }
procedure TChartDomainScanner.Analyze(AList, ADomain: TIntervalList;
  const AParts: TDomainParts);
var
  a, b: Double;
begin
  // two-sided interval, e.g. "0 < x <= 1", or "2 > x >= 1"
  if (AParts[2] = Variable) and (AParts[3] <> '') and (AParts[4] <> '') then
  begin
    FParser.Expression := AParts[0];
    a := ArgToFloat(FParser.Evaluate);
    FParser.Expression := AParts[4];
    b := ArgToFloat(FParser.Evaluate);
    if (AParts[1][1] = '<') and (AParts[3][1] = '<') and (a < b) then
      ADomain.AddRange(a, b)
    else
    if (AParts[1][1] = '>') and (AParts[3][1] = '>') and (a > b) then
      ADomain.AddRange(b, a);
  end else
  // one-sided interval, variable is at left
  if (AParts[0] = Variable) and (AParts[3] = '') and (AParts[4] = '') then
  begin
    FParser.Expression := AParts[2];
    a := ArgToFloat(FParser.Evaluate);
    case AParts[1] of
      '<>'      : AList.AddPoint(a);                    //  x <> a
      '<', '<=' : ADomain.AddRange(-Infinity, a);       //  x < a, x <= a
      '>', '>=' : ADomain.AddRange(a, Infinity);        //  x > a, x >= a
      else  Expressionerror;
    end;
  end else
  // one-sided interval, variable is at right
  if (AParts[2] = Variable) and (AParts[3] = '') and (AParts[4] = '') then
  begin
    FParser.Expression := AParts[0];
    a := ArgToFloat(FParser.Evaluate);
    case AParts[1] of
      '<>'      : AList.AddPoint(a);                    //  a <> x
      '<', '<=' : ADomain.AddRange(a, Infinity);        //  a < x, a <= x
      '>', '>=' : ADomain.AddRange(-Infinity, a);       //  a > x, a >= x
      else   ExpressionError;
    end;
  end else
    ExpressionError;
end;

{ Converts the intervals in ADomain on which the function is defined to
  intervals in AList in which the function is NOT defined (= DomainExclusion) }
procedure TChartDomainScanner.ConvertToExclusions(AList, ADomain: TIntervalList);

  function IsPoint(i: Integer): Boolean;
  begin
    Result := (i >= 0) and (i < ADomain.IntervalCount) and
      (ADomain.Interval[i].FStart = ADomain.Interval[i].FEnd);
  end;

type
  TIntervalPoint = record
    Value: Double;
    Contained: Boolean;
  end;

var
  a, b: Double;
  i, j: Integer;
  intervalWithStart: Boolean;
  intervalWithEnd: Boolean;
  points: array of TIntervalPoint;
  npoints: Integer;
begin
  if ADomain.IntervalCount = 0 then
    exit;

  j := 0;
  SetLength(points, ADomain.IntervalCount*2);

  for i:=0 to ADomain.IntervalCount-1 do begin
    if IsPoint(i) then
      Continue;
    if ADomain.Interval[i].FStart <> -Infinity then begin
      points[j].Value := ADomain.Interval[i].FStart;
      points[j].Contained := IsPoint(i-1);
      inc(j);
    end;
    if ADomain.Interval[i].FEnd <> +Infinity then begin
      points[j].Value := ADomain.Interval[i].FEnd;
      points[j].Contained := IsPoint(i+1);
      inc(j);
    end;
  end;
  SetLength(points, j);

  // Case 1: domain extends to neg infinity
  // -INF <---------|xxxxxxxx|------|xxxx> INF     with - = allowed, x = forbidden
  //                0        1      2
  if ADomain.Interval[0].FStart = -Infinity then
    j := 0
  else
  // Case 2: domain begins at finite value
  // -INF <xxxxxxxxx|--------|xxxxxx|------>INF
  //                0        1      2
  begin
    a := -Infinity;
    b := points[0].Value;
    AList.AddRange(a, b);
    if not points[0].Contained then
      AList.AddPoint(b);
    j := 1;
  end;
  while j < Length(points) do begin
    a := points[j].Value;
    if not points[j].Contained then
      AList.AddPoint(a);
    if j = High(points) then begin
      AList.AddRange(a, Infinity);
    end else
    begin
      b := points[j+1].Value;
      AList.AddRange(a, b);
      if not points[j+1].Contained then
        AList.AddPoint(b);
    end;
    inc(j, 2);
  end;
end;

procedure TChartDomainScanner.ExpressionError;
begin
  raise Exception.Create('Incorrect domain expression in "' + FExpression + '"');
end;

procedure TChartDomainScanner.ExtractDomainExclusions(AList: TIntervalList);
var
  domains: TIntervalList;
  savedExpr: String;
begin
  Assert(AList <> nil);

  AList.Clear;
  if FExpression = '' then
    exit;

  savedExpr := FParser.Expression;
  domains := TIntervalList.Create;
  try
    AList.Epsilon := FEpsilon;
    domains.Epsilon := FEpsilon;
    ParseExpression(AList, domains);
    ConvertToExclusions(AList, domains);
  finally
    domains.Free;
    FParser.Expression := savedExpr;
  end;
end;

function TChartDomainScanner.GetVariable: String;
begin
  Result := FSeries.Variable;
end;

{ Parses the expression string and creates an interval list with the
  domain of the function, i.e. the intervals in which the function is DEFINED. }
procedure TChartDomainScanner.ParseExpression(AList, ADomain: TIntervalList);
var
  i: Integer;
  s: String;
  parts: TDomainParts;
  p: Integer;  // 0=left, 1=operator left , 2=middle, 3=operator right, 4=right
               //   0          <=               x           <              1
begin
  for p := 0 to Length(parts)-1 do
    parts[p] := '';
  p := 0;
  i := 1;
  s := FExpression + ';';  // Simplify parsing...
  while i <= Length(s) do begin
    case s[i] of
      ' ': ;
      ';': begin
             Analyze(AList, ADomain, parts);
             for p := 0 to Length(parts)-1 do parts[p] := '';
               p := 0;
           end;
      '<': if (i < Length(FExpression)) then begin
             inc(p);
             case s[i+1] of
               '=': begin parts[p] := '<='; inc(i); end;
               '>': begin parts[p] := '<>'; inc(i); end;
               else parts[p] := '<';
             end;
             inc(p);
           end else
             ExpressionError;
      '>': if (i < Length(FExpression)) then begin
             inc(p);
             if s[i+1] = '=' then begin
               parts[p] := '>=';
               inc(i);
             end else
               parts[p] := '>';
             inc(p);
           end else
             ExpressionError;
      '=': ExpressionError;
    else
      parts[p] := Parts[p] + s[i];
    end;
    inc(i);
  end;
end;


{ TExpressionSeries }

constructor TExpressionSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVariable := 'x';

  FParser := TFpExpressionParser.Create(self);
  FParser.BuiltIns := [bcMath];
  FX := FParser.Identifiers.AddFloatVariable(FVariable, 0.0);
  FParser.Expression := FVariable;

  FDomainScanner := TChartDomainScanner.Create(self);
  FDomainScanner.Epsilon := DEFAULT_EPSILON;

  FParams := TChartExprParams.Create(TChartExprParam);
  FParams.FParser := FParser;
end;

destructor TExpressionSeries.Destroy;
begin
  FX := nil;
  FDomainScanner.Free;
  inherited;
end;

procedure TExpressionSeries.Assign(ASource: TPersistent);
begin
  if ASource is TExpressionSeries then begin
    Domain := TExpressionSeries(ASource).Domain;
    Expression := TExpressionSeries(ASource).Expression;
    Params := TExpressionSeries(ASource).Params;
    Variable := TExpressionSeries(ASource).Variable;
  end;
  inherited Assign(ASource);
end;

function TExpressionSeries.DoCalculate(AX: Double): Double;
var
  res: TFPExpressionResult;
begin
  Result := 0.0;
  FX.AsFloat := AX;
  try
    res := FParser.Evaluate;
    if res.ResultType=rtFloat then
      Result := res.ResFloat
    else
    if res.ResultType=rtInteger then
      Result := res.ResInteger;
  except
  end;
end;

function TExpressionSeries.GetDomain: String;
begin
  Result := FDomainScanner.Expression;
end;

function TExpressionSeries.GetDomainEpsilon: Double;
begin
  Result := FDomainScanner.Epsilon;
end;

function TExpressionSeries.GetExpression: String;
begin
  Result := FParser.Expression;
end;

function TExpressionSeries.IsEmpty: Boolean;
begin
  Result := FParser.Expression <> '';
end;

procedure TExpressionSeries.SetDomain(const AValue: String);
begin
  FDomainScanner.Expression := AValue;
  FDomainScanner.ExtractDomainExclusions(DomainExclusions);
  UpdateParentChart;
end;

procedure TExpressionSeries.SetDomainEpsilon(const AValue: Double);
begin
  FDomainScanner.Epsilon := AValue;
end;

procedure TExpressionSeries.SetExpression(const AValue: String);
begin
  FParser.Expression := AValue;
  if not (FParser.ResultType in [rtInteger, rtFLoat]) then
    raise EExprParser.CreateFmt(rsErrInvalidResultType, [ResultTypeName(FParser.ResultType)]);
  UpdateParentChart;
end;

procedure TExpressionSeries.SetParams(const AValue: TChartExprParams);
var
  i: Integer;
  p: TChartExprParam;
begin
  FParams.Assign(AValue);
  FParser.Identifiers.Clear;
  FX := FParser.Identifiers.AddFloatVariable(FVariable, 0.0);
  for i:=0 to FParams.Count-1 do begin
    p := FParams[i];
    FParser.Identifiers.AddFloatVariable(p.Name, p.Value);
  end;
end;

procedure TExpressionSeries.SetVariable(const AValue: String);
begin
  if FVariable = AValue then exit;
  FVariable := AValue;
  SetParams(FParams);
  UpdateParentChart;
end;


{ Additional functions for the parser }

procedure ExprDegToRad(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := degtorad(x);
end;

procedure ExprRadToDeg(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := radtodeg(x);
end;

procedure ExprTan(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := tan(x);
end;

procedure ExprCot(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := cot(x);
end;
(*
procedure ExprArcsin(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if IsNumber(x) and InRange(x, -1.0, 1.0) then
    Result.resFloat := arcsin(x)
  else
    Result.resFloat := NaN;
end;

procedure ExprArccos(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if IsNumber(x) and InRange(x, -1.0, 1.0) then
    Result.resFloat := arccos(x)
  else
    Result.resFloat := NaN;
end;

Procedure ExprArccot(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if IsNumber(x) and InRange(x, -1.0, 1.0) then
    Result.resFloat := pi/2 - arctan(x)
  else
    Result.resFloat := NaN;
end;

procedure ExprCosh(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if IsNumber(x) then
    Result.resFloat := cosh(x)
  else
    Result.resFloat := NaN;
end;

procedure ExprCoth(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if IsNumber(x) and (x <> 0.0) then
    Result.resFloat := 1/tanh(x)
  else
    Result.resFloat := NaN;
end;

procedure ExprSinh(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if IsNumber(x) then
    Result.resFloat := sinh(x)
  else
    Result.resFloat := NaN;
end;

procedure ExprTanh(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if IsNumber(x) then
    Result.resFloat := tanh(x)
  else
    Result.resFloat := NaN;
end;

procedure ExprArcosh(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if IsNumber(x) and (x >= 1.0) then
    Result.resFloat := arcosh(x)
  else
    Result.resFloat := NaN;
end;

procedure ExprArsinh(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgtoFloat(Args[0]);
  if IsNumber(x) then
    Result.resFloat := arsinh(x)
  else
    Result.resFloat := NaN;
end;

procedure ExprArtanh(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if IsNumber(x) and (x > -1.0) and (x < 1.0) then
    Result.resFloat := artanh(x)
  else
    Result.resFloat := NaN;
end;

procedure ExprArcoth(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if IsNumber(x) and (x < -1.0) and (x > 1.0) then
    Result.resFloat := artanh(1.0/x)
  else
    Result.resFloat := NaN;
end;                   *)

procedure ExprSinc(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if x = 0 then
    Result.ResFloat := 1.0
  else
    Result.resFloat := sin(x)/x;
end;

procedure ExprPower(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x,y: Double;
begin
  x := ArgToFloat(Args[0]);
  y := ArgToFloat(Args[1]);
  Result.resFloat := Power(x, y);
end;

procedure ExprHypot(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x,y: Double;
begin
  x := ArgToFloat(Args[0]);
  y := ArgToFloat(Args[1]);
  Result.resFloat := Hypot(x,y);
end;
                    (*
procedure ExprLg(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if IsNumber(x) then
    Result.resFloat := log10(x)
  else
    Result.resFloat := NaN;
end;

procedure ExprLog10(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if IsNumber(x) then
    Result.resFloat := log10(x)
  else
    Result.resFloat := NaN;
end;

procedure ExprLog2(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if IsNumber(x) then
    Result.resFloat := log2(x)
  else
    Result.resFloat := NaN;
end;

procedure ExprErf(Var Result: TFPExpressionResult; const Args: TExprParameterArray);
// Error function
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if IsNumber(x) then
    Result.resFloat := speerf(x)
  else
    Result.resFloat := NaN;
end;

procedure ExprErfc(Var Result: TFPExpressionResult; const Args: TExprParameterArray);
// Error function complement
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if IsNumber(x) then
    Result.resFloat := speefc(x)
  else
    Result.resFloat := NaN;
end;

// Incomplete gamma function P
procedure ExprGammaP(var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x, s: Double;
begin
  s := ArgToFloat(Args[0]);
  x := ArgToFloat(Args[1]);
  if IsNumber(x) and IsNumber(s) then
    Result.resFloat := gammap(s, x)
  else
    Result.resFloat := NaN;
end;

// Incomplete gamma function Q
procedure ExprGammaQ(var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x, s: Double;
begin
  s := ArgToFloat(Args[0]);
  x := ArgToFloat(Args[1]);
  if IsNumber(x) and IsNumber(s) then
    Result.resFloat := gammaq(s, x)
  else
    Result.resFloat := NaN;
end;

// Incomplete beta function
procedure ExprBetaI(var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  a, b, x: Double;
begin
  a := ArgToFloat(Args[0]);
  b := ArgToFloat(Args[1]);
  x := ArgToFloat(Args[2]);
  if IsNumber(x) and IsNumber(a) and IsNumber(b) then
    Result.resFloat := betai(a, b, x)
  else
    Result.resFloat := NaN;
end;

procedure ExprChi2Dist(var  Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
  n: Double;
begin
  x := ArgToFloat(Args[0]);
  n := ArgToFloat(Args[1]);
  if IsNumber(x) and IsNumber(n) then
    Result.resFloat := chi2dist(x, round(n))
  else
    Result.resFloat := NaN;
end;

procedure ExprtDist(var  Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
  n: Double;
begin
  x := ArgToFloat(Args[0]);
  n := ArgToFloat(Args[1]);
  if IsNumber(x) and IsNumber(n) then
    Result.resFloat := tdist(x, round(n))
  else
    Result.resFloat := NaN;
end;

procedure ExprFDist(var  Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
  n1, n2: Double;
begin
  x := ArgToFloat(Args[0]);
  n1 := ArgToFloat(Args[1]);
  n2 := ArgToFloat(Args[2]);
  if IsNumber(x) and IsNumber(n1) and IsNumber(n2) then
    Result.resFloat := Fdist(x, round(n1), round(n2))
  else
    Result.resFloat := NaN;
end;

procedure ExprI0(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
// Bessel function of the first kind I0(x)
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if IsNumber(x) then
    Result.resFloat := spebi0(x)
  else
    Result.resFloat := NaN;
end;

procedure ExprI1(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
// Bessel function of the first kind I1(x)
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if IsNumber(x) then
    Result.resFloat := spebi1(x)
  else
    Result.resFloat := NaN;
end;

procedure ExprJ0(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
// Bessel function of the first kind J0(x)
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if IsNumber(x) then
    Result.resFloat := spebj0(x)
  else
    Result.resFloat := NaN;
end;

procedure ExprJ1(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
// Bessel function of the first kind J1(x)
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if IsNumber(x) then
    Result.resFloat := spebj1(x)
  else
    Result.resFloat := NaN;
end;

procedure ExprK0(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
// Bessel function of the second kind K0(x)
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if IsNumber(x) then
    Result.resFloat := spebk0(x)
  else
    Result.resFloat := NaN;
end;

procedure ExprK1(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
// Bessel function of the second kind K1(x)
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if IsNumber(x) then
    Result.resFloat := spebk1(x)
  else
    Result.resFloat := NaN;
end;

procedure ExprY0(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
// Bessel function of the second kind Y0(x)
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if IsNumber(x) then
    Result.resFloat := speby0(x)
  else
    Result.resFloat := NaN;
end;

procedure ExprY1(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
// Bessel function of the second kind Y1(x)
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if IsNumber(x) then
    Result.resFloat := speby1(x)
  else
    Result.resFloat := NaN;
end;

procedure ExprMax(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x1, x2: Double;
begin
  x1 := ArgToFloat(Args[0]);
  x2 := ArgToFloat(Args[1]);
  if IsNumber(x1) and IsNumber(x2) then
    Result.resFloat := Max(x1, x2)
  else
    Result.resFloat := NaN;
end;

procedure ExprMin(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x1, x2: Double;
begin
  x1 := ArgToFloat(Args[0]);
  x2 := ArgToFloat(Args[1]);
  if IsNumber(x1) and IsNumber(x2) then
    Result.resFloat := Min(x1, x2)
  else
    Result.resFloat := NaN;
end;

function FixDecSep(const AExpression: String): String;
var
  i: Integer;
begin
  Result := AExpression;
  for i:=1 to Length(Result) do begin
    if Result[i] = ',' then Result[i] := '.';
  end;
end;*)

procedure ExtendBuiltins;
begin
  with BuiltinIdentifiers do begin
    AddFunction(bcMath, 'degtorad', 'F', 'F', @ExprDegtorad);
    AddFunction(bcMath, 'radtodeg', 'F', 'F', @ExprRadtodeg);

    AddFunction(bcMath, 'tan', 'F', 'F', @ExprTan);
    AddFunction(bcMath, 'cot', 'F', 'F', @ExprCot);
    (*
    AddFunction(bcMath, 'arcsin', 'F', 'F', @ExprArcSin);
    AddFunction(bcMath, 'arccos', 'F', 'F', @ExprArcCos);
    AddFunction(bcMath, 'arccot', 'F', 'F', @ExprArcCot);
    AddFunction(bcMath, 'cosh', 'F', 'F', @ExprCosh);
    AddFunction(bcMath, 'coth', 'F', 'F', @ExprCoth);
    AddFunction(bcMath, 'sinh', 'F', 'F', @ExprSinh);
    AddFunction(bcMath, 'tanh', 'F', 'F', @ExprTanh);
    AddFunction(bcMath, 'arcosh', 'F', 'F', @ExprArcosh);
    AddFunction(bcMath, 'arsinh', 'F', 'F', @ExprArsinh);
    AddFunction(bcMath, 'artanh', 'F', 'F', @ExprArtanh);
    AddFunction(bcMath, 'arcoth', 'F', 'F', @ExprArcoth);
    *)
    AddFunction(bcMath, 'sinc', 'F', 'F', @ExprSinc);

    AddFunction(bcMath, 'power', 'F', 'FF', @ExprPower);
    AddFunction(bcMath, 'hypot', 'F', 'FF', @ExprHypot);

(*
    AddFunction(bcMath, 'lg', 'F', 'F', @ExprLog10);
    AddFunction(bcMath, 'log10', 'F', 'F', @ExprLog10);
    AddFunction(bcMath, 'log2', 'F', 'F', @ExprLog2);

    // Error function
    AddFunction(bcMath, 'erf', 'F', 'F', @ExprErf);
    AddFunction(bcMath, 'erfc', 'F', 'F', @ExprErfc);

    // Incomplete gamma and beta functions
    AddFunction(bcMath, 'gammap', 'F', 'FF', @ExprGammaP);
    AddFunction(bcMath, 'gammaq', 'F', 'FF', @ExprGammaQ);
    AddFunction(bcMath, 'betai', 'F', 'FFF', @ExprBetaI);

    // Probability distributions
    AddFunction(bcMath, 'chi2dist', 'F', 'FI', @ExprChi2Dist);
    AddFunction(bcMath, 'tdist', 'F', 'FI', @Exprtdist);
    AddFunction(bcMath, 'Fdist', 'F', 'FII', @ExprFDist);

    // Bessel functions of the first kind
    AddFunction(bcMath, 'I0', 'F', 'F', @ExprI0);
    AddFunction(bcMath, 'I1', 'F', 'F', @ExprI1);
    AddFunction(bcMath, 'J0', 'F', 'F', @ExprJ0);
    AddFunction(bcMath, 'J1', 'F', 'F', @ExprJ1);

    // Bessel functions of the second kind
    AddFunction(bcMath, 'K0', 'F', 'F', @ExprK0);
    AddFunction(bcMath, 'K1', 'F', 'F', @ExprK1);
    AddFunction(bcMath, 'Y0', 'F', 'F', @ExprY0);
    AddFunction(bcMath, 'Y1', 'F', 'F', @ExprY1);

    // Max/min
    AddFunction(bcMath, 'max', 'F', 'FF', @ExprMax);
    AddFunction(bcMath, 'min', 'F', 'FF', @ExprMin);
    *)
  end;
end;

initialization
  ExtendBuiltins;
  RegisterSeriesClass(TExpressionSeries, @rsExpressionSeries);

end.

