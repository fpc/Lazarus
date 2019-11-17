{

 Axis transformations.

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Authors: Alexander Klenin

}
unit TATransformations;

{$H+}

interface

uses
  Classes, SysUtils,
  TAChartUtils;

type

  TChartAxisTransformations = class;

  { TAxisTransform }

  TAxisTransform = class(TIndexedComponent)
  strict private
    FEnabled: Boolean;
    FTransformations: TChartAxisTransformations;
    procedure SetEnabled(AValue: Boolean);
    procedure SetTransformations(AValue: TChartAxisTransformations);
  protected
    procedure ReadState(Reader: TReader); override;
    procedure SetParentComponent(AParent: TComponent); override;
  protected
    procedure Changed;
    function GetIndex: Integer; override;
    procedure SetIndex(AValue: Integer); override;
  protected
    FDrawData: TDrawDataItem;
    procedure ClearBounds; virtual;
    function GetDrawDataClass: TDrawDataItemClass; virtual;
    procedure SetChart(AChart: TObject);
    procedure UpdateBounds(var AMin, AMax: Double); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Assign(ASource: TPersistent); override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
  public
    function AxisToGraph(AX: Double): Double; virtual;
    function GraphToAxis(AX: Double): Double; virtual;

    property Transformations: TChartAxisTransformations
      read FTransformations write SetTransformations;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default true;
  end;

  TAxisTransformClass = class of TAxisTransform;

  {$IFNDEF fpdoc} // Workaround for issue #18549.
  TAxisTransformEnumerator = specialize TTypedFPListEnumerator<TAxisTransform>;
  {$ENDIF}

  TAxisTransformList = class(TIndexedComponentList)
  public
    function GetEnumerator: TAxisTransformEnumerator;
  end;

  { TChartAxisTransformations }

  TChartAxisTransformations = class(TComponent)
  strict private
    FBroadcaster: TBroadcaster;
    FList: TAxisTransformList;
  protected
    procedure SetName(const AValue: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
  public
    function AxisToGraph(AX: Double): Double;
    procedure ClearBounds;
    function GraphToAxis(AX: Double): Double;
    procedure SetChart(AChart: TObject);
    procedure UpdateBounds(var AMin, AMax: Double);

    property Broadcaster: TBroadcaster read FBroadcaster;
  published
    property List: TAxisTransformList read FList;
  end;

  { TLinearAxisTransform }

  TLinearAxisTransform = class(TAxisTransform)
  strict private
    FOffset: Double;
    FScale: Double;
    function OffsetIsStored: Boolean;
    function ScaleIsStored: Boolean;
    procedure SetOffset(AValue: Double);
    procedure SetScale(AValue: Double);
  public
    constructor Create(AOwner: TComponent); override;
  public
    procedure Assign(ASource: TPersistent); override;

    function AxisToGraph(AX: Double): Double; override;
    function GraphToAxis(AX: Double): Double; override;
  published
    property Offset: Double read FOffset write SetOffset stored OffsetIsStored;
    property Scale: Double read FScale write SetScale stored ScaleIsStored;
  end;

  { TAutoScaleAxisTransform }

  TAutoScaleAxisTransform = class(TAxisTransform)
  strict private
    FMaxValue: Double;
    FMinValue: Double;
    function MaxValueIsStored: Boolean;
    function MinValueIsStored: Boolean;
    procedure SetMaxValue(AValue: Double);
    procedure SetMinValue(AValue: Double);
  protected
    procedure ClearBounds; override;
    function GetDrawDataClass: TDrawDataItemClass; override;
    procedure UpdateBounds(var AMin, AMax: Double); override;
  public
    constructor Create(AOwner: TComponent); override;
  public
    procedure Assign(ASource: TPersistent); override;

    function AxisToGraph(AX: Double): Double; override;
    function GraphToAxis(AX: Double): Double; override;
  published
    property MaxValue: Double
      read FMaxValue write SetMaxValue stored MaxValueIsStored;
    property MinValue: Double
      read FMinValue write SetMinValue stored MinValueIsStored;
  end;

  { TLogarithmAxisTransform }

  TLogarithmAxisTransform = class(TAxisTransform)
  strict private
    FBase: Double;
    procedure SetBase(AValue: Double);
  public
    constructor Create(AOwner: TComponent); override;
  public
    procedure Assign(Source: TPersistent); override;

    function AxisToGraph(AX: Double): Double; override;
    function GraphToAxis(AX: Double): Double; override;
  published
    property Base: Double read FBase write SetBase;
  end;

  TCumulNormDistrAxisTransform = class(TAxisTransform)
  public
    function AxisToGraph(AX: Double): Double; override;
    function GraphToAxis(AX: Double): Double; override;
  end;

  TTransformEvent = procedure (AX: Double; out AT: Double) of object;

  { TUserDefinedAxisTransform }

  TUserDefinedAxisTransform = class(TAxisTransform)
  private
    FOnAxisToGraph: TTransformEvent;
    FOnGraphToAxis: TTransformEvent;
    procedure SetOnAxisToGraph(AValue: TTransformEvent);
    procedure SetOnGraphToAxis(AValue: TTransformEvent);
  public
    procedure Assign(ASource: TPersistent); override;

    function AxisToGraph(AX: Double): Double; override;
    function GraphToAxis(AX: Double): Double; override;
  published
    property OnAxisToGraph: TTransformEvent read FOnAxisToGraph write SetOnAxisToGraph;
    property OnGraphToAxis: TTransformEvent read FOnGraphToAxis write SetOnGraphToAxis;
  end;

  procedure Register;

  procedure RegisterAxisTransformClass(AAxisTransformClass: TAxisTransformClass;
    const ACaption: String); overload;
  procedure RegisterAxisTransformClass(AAxisTransformClass: TAxisTransformClass;
    ACaptionPtr: PStr); overload;

implementation

uses
  ComponentEditors, Forms, Math, PropEdits,
  TAChartStrConsts, TAMath, TASubcomponentsEditor;

type
  { TAxisTransformsComponentEditor }

  TAxisTransformsComponentEditor = class(TSubComponentListEditor)
  protected
    function MakeEditorForm: TForm; override;
  public
    function GetVerb(Index: Integer): string; override;
  end;

  { TAxisTransformsPropertyEditor }

  TAxisTransformsPropertyEditor = class(TComponentListPropertyEditor)
  protected
    function GetChildrenCount: Integer; override;
    function MakeEditorForm: TForm; override;
  end;

  { TAxisTransformsEditorForm }

  TAxisTransformsEditorForm = class(TComponentListEditorForm)
  protected
    procedure AddSubcomponent(AParent, AChild: TComponent); override;
    procedure BuildCaption; override;
    function ChildClass: TComponentClass; override;
    procedure EnumerateSubcomponentClasses; override;
    function GetChildrenList: TFPList; override;
    function MakeSubcomponent(
      AOwner: TComponent; ATag: Integer): TComponent; override;
  end;

  TAutoScaleTransformData = class (TDrawDataItem)
  private
    FMin, FMax, FOffset, FScale: Double;
  end;

var
  AxisTransformsClassRegistry: TClassRegistry;

procedure Register;
var
  i: Integer;
begin
  with AxisTransformsClassRegistry do
    for i := 0 to Count - 1 do
      RegisterNoIcon([TAxisTransformClass(GetClass(i))]);
  RegisterComponents(CHART_COMPONENT_IDE_PAGE, [TChartAxisTransformations]);
  RegisterPropertyEditor(
    TypeInfo(TAxisTransformList), TChartAxisTransformations,
    'List', TAxisTransformsPropertyEditor);
  RegisterComponentEditor(
    TChartAxisTransformations, TAxisTransformsComponentEditor);
end;

procedure RegisterAxisTransformClass(AAxisTransformClass: TAxisTransformClass;
  const ACaption: String);
begin
  RegisterClass(AAxisTransformClass);
  with AxisTransformsClassRegistry do
    if IndexOfClass(AAxisTransformClass) < 0 then
      Add(TClassRegistryItem.Create(AAxisTransformClass, ACaption));
end;

procedure RegisterAxisTransformClass(AAxisTransformClass: TAxisTransformClass;
  ACaptionPtr: PStr);
begin
  RegisterClass(AAxisTransformClass);
  with AxisTransformsClassRegistry do
    if IndexOfClass(AAxisTransformClass) < 0 then
      Add(TClassRegistryItem.CreateRes(AAxisTransformClass, ACaptionPtr));
end;

{ TAxisTransformList }

function TAxisTransformList.GetEnumerator: TAxisTransformEnumerator;
begin
  Result := TAxisTransformEnumerator.Create(Self);
end;

{ TAxisTransformsComponentEditor }

function TAxisTransformsComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := tasAxisTransformsEditorTitle
  else
    Result := '';
end;

function TAxisTransformsComponentEditor.MakeEditorForm: TForm;
begin
  Result := TAxisTransformsEditorForm.Create(Application, GetComponent, Self, nil);
end;

{ TAxisTransformsPropertyEditor }

function TAxisTransformsPropertyEditor.GetChildrenCount: Integer;
begin
  Result := (GetObjectValue as TAxisTransformList).Count;
end;

function TAxisTransformsPropertyEditor.MakeEditorForm: TForm;
begin
  with TAxisTransformsEditorForm do
    Result := Create(Application, GetComponent(0) as TComponent, nil, Self);
end;

{ TAxisTransformsEditorForm }

procedure TAxisTransformsEditorForm.AddSubcomponent(
  AParent, AChild: TComponent);
begin
  (AChild as TAxisTransform).Transformations :=
    AParent as TChartAxisTransformations;
end;

procedure TAxisTransformsEditorForm.BuildCaption;
begin
  Caption := tasAxisTransformsEditorTitle + ' - ' + Parent.Name;
end;

function TAxisTransformsEditorForm.ChildClass: TComponentClass;
begin
  Result := TAxisTransform;
end;

procedure TAxisTransformsEditorForm.EnumerateSubcomponentClasses;
var
  i: Integer;
begin
  for i := 0 to AxisTransformsClassRegistry.Count - 1 do
    AddSubcomponentClass(AxisTransformsClassRegistry.GetCaption(i), i);
end;

function TAxisTransformsEditorForm.GetChildrenList: TFPList;
begin
  Result := (Parent as TChartAxisTransformations).List;
end;

function TAxisTransformsEditorForm.MakeSubcomponent(
  AOwner: TComponent; ATag: Integer): TComponent;
begin
  with AxisTransformsClassRegistry do
    Result := TAxisTransformClass(GetClass(ATag)).Create(AOwner);
end;

{ TAxisTransform }

procedure TAxisTransform.Assign(ASource: TPersistent);
begin
  if ASource is TAxisTransform then
    with TAxisTransform(ASource) do
      Self.FEnabled := Enabled
  else
    inherited Assign(ASource);
end;

function TAxisTransform.AxisToGraph(AX: Double): Double;
begin
  Result := AX;
end;

procedure TAxisTransform.Changed;
begin
  if Transformations <> nil then
    Transformations.Broadcaster.Broadcast(Self);
end;

procedure TAxisTransform.ClearBounds;
begin
  // empty
end;

constructor TAxisTransform.Create(AOwner: TComponent);
begin
  FEnabled := true;
  inherited Create(AOwner);
end;

destructor TAxisTransform.Destroy;
begin
  Transformations := nil;
  DrawData.DeleteByOwner(Self);
  inherited;
end;

function TAxisTransform.GetDrawDataClass: TDrawDataItemClass;
begin
  Result := nil;
end;

function TAxisTransform.GetIndex: Integer;
begin
  if Transformations = nil then
    Result := -1
  else
    Result := Transformations.List.IndexOf(Self);
end;

function TAxisTransform.GetParentComponent: TComponent;
begin
  Result := Transformations;
end;

function TAxisTransform.GraphToAxis(AX: Double): Double;
begin
  Result := AX;
end;

function TAxisTransform.HasParent: Boolean;
begin
  Result := true;
end;

procedure TAxisTransform.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TChartAxisTransformations then
    Transformations := TChartAxisTransformations(Reader.Parent);
end;

procedure TAxisTransform.SetChart(AChart: TObject);
begin
  if GetDrawDataClass = nil then exit;
  FDrawData := DrawData.Find(AChart, Self);
  if FDrawData <> nil then exit;
  FDrawData := GetDrawDataClass.Create(AChart, Self);
  DrawData.Add(FDrawData);
end;

procedure TAxisTransform.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then exit;
  FEnabled := AValue;
  Changed;
end;

procedure TAxisTransform.SetIndex(AValue: Integer);
begin
  with Transformations.List do
    Move(Index, EnsureRange(AValue, 0, Count - 1));
end;

procedure TAxisTransform.SetParentComponent(AParent: TComponent);
begin
  if not (csLoading in ComponentState) then
    Transformations := AParent as TChartAxisTransformations;
end;

procedure TAxisTransform.SetTransformations(AValue: TChartAxisTransformations);
begin
  if FTransformations = AValue then exit;
  if FTransformations  <> nil then
    FTransformations.List.Remove(Self);
  FTransformations := AValue;
  if FTransformations <> nil then
    FTransformations.List.Add(Self);
end;

procedure TAxisTransform.UpdateBounds(var AMin, AMax: Double);
begin
  if not IsInfinite(AMin) then
    AMin := AxisToGraph(AMin);
  if not IsInfinite(AMax) then
    AMax := AxisToGraph(AMax);
end;

{ TChartAxisTransformations }

function TChartAxisTransformations.AxisToGraph(AX: Double): Double;
var
  t: TAxisTransform;
begin
  Result := AX;
  if IsNan(Result) then exit;
  for t in List do
    if t.Enabled then
      Result := t.AxisToGraph(Result);
end;

procedure TChartAxisTransformations.ClearBounds;
var
  t: TAxisTransform;
begin
  for t in List do
    if t.Enabled then
      t.ClearBounds;
end;

constructor TChartAxisTransformations.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBroadcaster := TBroadcaster.Create;
  FList := TAxisTransformList.Create;
end;

destructor TChartAxisTransformations.Destroy;
begin
  while List.Count > 0 do
    TAxisTransform(List[List.Count - 1]).Free;
  FreeAndNil(FList);
  FreeAndNil(FBroadcaster);
  inherited;
end;

procedure TChartAxisTransformations.GetChildren(
  Proc: TGetChildProc; Root: TComponent);
var
  t: TAxisTransform;
begin
  for t in List do
    if t.Owner = Root then
      Proc(t);
end;

function TChartAxisTransformations.GraphToAxis(AX: Double): Double;
var
  i: Integer;
begin
  Result := AX;
  for i := List.Count - 1 downto 0 do
    with TAxisTransform(List[i]) do
      if Enabled then
        Result := GraphToAxis(Result);
end;

procedure TChartAxisTransformations.SetChart(AChart: TObject);
var
  t: TAxisTransform;
begin
  for t in List do
    if t.Enabled then
      t.SetChart(AChart);
end;

procedure TChartAxisTransformations.SetChildOrder(
  Child: TComponent; Order: Integer);
var
  i: Integer;
begin
  i := List.IndexOf(Child);
  if i >= 0 then
    List.Move(i, Order);
end;

procedure TChartAxisTransformations.SetName(const AValue: TComponentName);
var
  oldName: String;
begin
  if Name = AValue then exit;
  oldName := Name;
  inherited SetName(AValue);
  if csDesigning in ComponentState then
    List.ChangeNamePrefix(oldName, AValue);
end;

procedure TChartAxisTransformations.UpdateBounds(var AMin, AMax: Double);
var
  t: TAxisTransform;
begin
  for t in List do
    if t.Enabled then
      t.UpdateBounds(AMin, AMax);
end;

{ TLinearAxisTransform }

procedure TLinearAxisTransform.Assign(ASource: TPersistent);
begin
  if ASource is TLinearAxisTransform then
    with TLinearAxisTransform(ASource) do begin
      Self.FOffset := Offset;
      Self.FScale := Scale;
    end;
  inherited Assign(ASource);
end;

function TLinearAxisTransform.AxisToGraph(AX: Double): Double;
begin
  Result := AX * Scale + Offset;
end;

constructor TLinearAxisTransform.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScale := 1.0;
end;

function TLinearAxisTransform.GraphToAxis(AX: Double): Double;
begin
  Result := (AX - Offset) / Scale;
end;

function TLinearAxisTransform. OffsetIsStored: Boolean;
begin
  Result := not SameValue(Offset, 0.0);
end;

function TLinearAxisTransform.ScaleIsStored: Boolean;
begin
  Result := not SameValue(Scale, 1.0);
end;

procedure TLinearAxisTransform.SetOffset(AValue: Double);
begin
  if SameValue(FOffset, AValue) then exit;
  FOffset := AValue;
  Changed;
end;

procedure TLinearAxisTransform.SetScale(AValue: Double);
begin
  if SameValue(FScale, AValue) then exit;
  FScale := AValue;
  if SameValue(FScale, 0.0) then FScale := 1.0;
  Changed;
end;

{ TLogarithmAxisTransform }

procedure TLogarithmAxisTransform.Assign(Source: TPersistent);
begin
  if Source is TLogarithmAxisTransform then
    with Source as TLogarithmAxisTransform do
      Self.FBase := Base
  else
    inherited Assign(Source);
end;

function TLogarithmAxisTransform.AxisToGraph(AX: Double): Double;
begin
  if AX > 0 then
    Result := LogN(Base, AX)
  else
    Result := NegInfinity;
end;

constructor TLogarithmAxisTransform.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBase := Exp(1);
end;

function TLogarithmAxisTransform.GraphToAxis(AX: Double): Double;
begin
  Result := Power(Base, AX);
end;

procedure TLogarithmAxisTransform.SetBase(AValue: Double);
begin
  if FBase = AValue then exit;
  if (AValue <= 0) or (AValue = 1.0) then
    raise Exception.Create(rsInvalidLogBase);
  FBase := AValue;
  Changed;
end;

{ TAutoScaleAxisTransform }

procedure TAutoScaleAxisTransform.Assign(ASource: TPersistent);
begin
  if ASource is TAutoScaleAxisTransform then
    with TAutoScaleAxisTransform(ASource) do begin
      Self.FMinValue := FMinValue;
      Self.FMaxValue := FMaxValue;
    end;
  inherited Assign(ASource);
end;

function TAutoScaleAxisTransform.AxisToGraph(AX: Double): Double;
begin
  with TAutoScaleTransformData(FDrawData) do
    Result := AX * FScale + FOffset;
end;

procedure TAutoScaleAxisTransform.ClearBounds;
begin
  inherited ClearBounds;

  // Avoid crashing when called too early, e.g. when a TNavPanel is on the form
  // https://forum.lazarus.freepascal.org/index.php/topic,47429.0.html
  if FDrawData = nil then
    exit;

  with TAutoScaleTransformData(FDrawData) do begin
    FMin := SafeInfinity;
    FMax := NegInfinity;
    FOffset := 0.0;
    FScale := 1.0;
  end;
end;

constructor TAutoScaleAxisTransform.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaxValue := 1.0;
end;

function TAutoScaleAxisTransform.GetDrawDataClass: TDrawDataItemClass;
begin
  Result := TAutoScaleTransformData;
end;

function TAutoScaleAxisTransform.GraphToAxis(AX: Double): Double;
begin
  with TAutoScaleTransformData(FDrawData) do
    Result := (AX - FOffset) / FScale;
end;

function TAutoScaleAxisTransform.MaxValueIsStored: Boolean;
begin
  Result := MaxValue <> 1.0;
end;

function TAutoScaleAxisTransform.MinValueIsStored: Boolean;
begin
  Result := MinValue <> 0.0;
end;

procedure TAutoScaleAxisTransform.SetMaxValue(AValue: Double);
begin
  if FMaxValue = AValue then exit;
  FMaxValue := AValue;
  Changed;
end;

procedure TAutoScaleAxisTransform.SetMinValue(AValue: Double);
begin
  if FMinValue = AValue then exit;
  FMinValue := AValue;
  Changed;
end;

procedure TAutoScaleAxisTransform.UpdateBounds(var AMin, AMax: Double);
begin
  // Auto-scale is only defined for finite bounds.
  if IsInfinite(AMin) or IsInfinite(AMax) then exit;
  with TAutoScaleTransformData(FDrawData) do begin
    UpdateMinMax(AMin, FMin, FMax);
    UpdateMinMax(AMax, FMin, FMax);
    if FMax = FMin then
      FScale := 1.0
    else
      FScale := (MaxValue - MinValue) / (FMax - FMin);
    FOffset := MinValue - FMin * FScale;
  end;
  AMin := MinValue;
  AMax := MaxValue;
end;

{ TCumulNormDistrAxisTransform }

function TCumulNormDistrAxisTransform.AxisToGraph(AX: Double): Double;
begin
  Result := InvCumulNormDistr(AX);
end;

function TCumulNormDistrAxisTransform.GraphToAxis(AX: Double): Double;
begin
  Result := CumulNormDistr(AX);
end;

{ TUserDefinedAxisTransform }

procedure TUserDefinedAxisTransform.Assign(ASource: TPersistent);
begin
  if ASource is TUserDefinedAxisTransform then
    with TUserDefinedAxisTransform(ASource) do begin
      Self.FOnAxisToGraph := FOnAxisToGraph;
      Self.FOnGraphToAxis := FOnGraphToAxis;
    end;
  inherited Assign(ASource);
end;

function TUserDefinedAxisTransform.AxisToGraph(AX: Double): Double;
begin
  if Assigned(OnAxisToGraph) then
    OnAxisToGraph(AX, Result)
  else
    Result := AX;
end;

function TUserDefinedAxisTransform.GraphToAxis(AX: Double): Double;
begin
  if Assigned(OnGraphToAxis) then
    OnGraphToAxis(AX, Result)
  else
    Result := AX;
end;

procedure TUserDefinedAxisTransform.SetOnAxisToGraph(AValue: TTransformEvent);
begin
  if TMethod(FOnAxisToGraph) = TMethod(AValue) then exit;
  FOnAxisToGraph := AValue;
  Changed;
end;

procedure TUserDefinedAxisTransform.SetOnGraphToAxis(AValue: TTransformEvent);
begin
  if TMethod(FOnGraphToAxis) = TMethod(AValue) then exit;
  FOnGraphToAxis := AValue;
  Changed;
end;

initialization

  AxisTransformsClassRegistry := TClassRegistry.Create;
  RegisterAxisTransformClass(TAutoScaleAxisTransform, @rsAutoScale);
  RegisterAxisTransformClass(
    TCumulNormDistrAxisTransform, @rsCumulativeNormalDistribution);
  RegisterAxisTransformClass(TLinearAxisTransform, @rsLinear);
  RegisterAxisTransformClass(TLogarithmAxisTransform, @rsLogarithmic);
  RegisterAxisTransformClass(TUserDefinedAxisTransform, @rsUserDefined);

finalization

  FreeAndNil(AxisTransformsClassRegistry);

end.

