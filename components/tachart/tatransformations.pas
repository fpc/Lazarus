{

 Axis transformations.

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
  private
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
    procedure Assign(Source: TPersistent); override;
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

  TAxisTransformList = class(TFPList)
  end;

  { TChartAxisTransformations }

  TChartAxisTransformations = class (TComponent)
  private
    FBroadcaster: TBroadcaster;
    FList: TAxisTransformList;
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
  private
    FOffset: Double;
    FScale: Double;
    function OffsetIsStored: Boolean;
    function ScaleIsStored: Boolean;
    procedure SetOffset(AValue: Double);
    procedure SetScale(AValue: Double);
  public
    constructor Create(AOwner: TComponent); override;
  public
    procedure Assign(Source: TPersistent); override;

    function AxisToGraph(AX: Double): Double; override;
    function GraphToAxis(AX: Double): Double; override;
  published
    property Offset: Double read FOffset write SetOffset stored OffsetIsStored;
    property Scale: Double read FScale write SetScale stored ScaleIsStored;
  end;

  { TAutoScaleAxisTransform }

  TAutoScaleAxisTransform = class(TAxisTransform)
  private
    FMaxValue: Double;
    FMinValue: Double;
    function MaxValueIsStored: boolean;
    function MinValueIsStored: boolean;
    procedure SetMaxValue(const AValue: Double);
    procedure SetMinValue(const AValue: Double);
  protected
    procedure ClearBounds; override;
    function GetDrawDataClass: TDrawDataItemClass; override;
    procedure UpdateBounds(var AMin, AMax: Double); override;
  public
    constructor Create(AOwner: TComponent); override;
  public
    procedure Assign(Source: TPersistent); override;

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
  private
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

  procedure Register;

resourcestring
  tasAxisTransformsEditorTitle = 'Edit axis transformations';

implementation

uses
  ComponentEditors, Forms, Math, PropEdits,
  TASubcomponentsEditor;

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
  AxisTransformsClassRegistry: TStringList;

procedure Register;
var
  i: Integer;
begin
  with AxisTransformsClassRegistry do
    for i := 0 to Count - 1 do
      RegisterNoIcon([TAxisTransformClass(Objects[i])]);
  RegisterComponents(CHART_COMPONENT_IDE_PAGE, [TChartAxisTransformations]);
  RegisterPropertyEditor(
    TypeInfo(TAxisTransformList), TChartAxisTransformations,
    'List', TAxisTransformsPropertyEditor);
  RegisterComponentEditor(
    TChartAxisTransformations, TAxisTransformsComponentEditor);
end;

procedure RegisterAxisTransformClass(
  AAxisTransformClass: TAxisTransformClass; const ACaption: String);
begin
  RegisterClass(AAxisTransformClass);
  AxisTransformsClassRegistry.AddObject(ACaption, TObject(AAxisTransformClass));
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
    AddSubcomponentClass(AxisTransformsClassRegistry[i], i);
end;

function TAxisTransformsEditorForm.GetChildrenList: TFPList;
begin
  Result := (Parent as TChartAxisTransformations).List;
end;

function TAxisTransformsEditorForm.MakeSubcomponent(
  AOwner: TComponent; ATag: Integer): TComponent;
begin
  with AxisTransformsClassRegistry do
    Result := TAxisTransformClass(Objects[ATag]).Create(AOwner);
end;

{ TAxisTransform }

procedure TAxisTransform.Assign(Source: TPersistent);
begin
  if Source is TAxisTransform then
    with TAxisTransform(Source) do
      Self.FEnabled := Enabled
  else
    inherited Assign(Source);
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
    Transformations := Reader.Parent as TChartAxisTransformations;
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
  i: Integer;
begin
  Result := AX;
  for i := 0 to List.Count - 1 do
    with TAxisTransform(List[i]) do
      if Enabled then
        Result := AxisToGraph(Result);
end;

procedure TChartAxisTransformations.ClearBounds;
var
  i: Integer;
begin
  for i := List.Count - 1 downto 0 do
    with TAxisTransform(List[i]) do
      if Enabled then
        ClearBounds;
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
  i: Integer;
begin
  for i := 0 to List.Count - 1 do
    if TAxisTransform(List[i]).Owner = Root then
      Proc(TAxisTransform(List[i]));
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
  i: Integer;
begin
  for i := 0 to List.Count - 1 do
    with TAxisTransform(List[i]) do
      if Enabled then
        TAxisTransform(List[i]).SetChart(AChart);
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

procedure TChartAxisTransformations.UpdateBounds(var AMin, AMax: Double);
var
  i: Integer;
begin
  for i := 0 to List.Count - 1 do
    with TAxisTransform(List[i]) do
      if Enabled then
        UpdateBounds(AMin, AMax);
end;

{ TLinearAxisTransform }

procedure TLinearAxisTransform.Assign(Source: TPersistent);
begin
  if Source is TLinearAxisTransform then
    with Source as TLinearAxisTransform do begin
      Self.FOffset := Offset;
      Self.FScale := Scale;
    end;
  inherited Assign(Source);
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
  Result := Offset <> 0;
end;

function TLinearAxisTransform.ScaleIsStored: Boolean;
begin
  Result := Scale <> 1.0;
end;

procedure TLinearAxisTransform.SetOffset(AValue: Double);
begin
  if FOffset = AValue then exit;
  FOffset := AValue;
  Changed;
end;

procedure TLinearAxisTransform.SetScale(AValue: Double);
begin
  if FScale = AValue then exit;
  FScale := AValue;
  if FScale = 0 then FScale := 1.0;
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
  FBase := AValue;
  Changed;
end;

{ TAutoScaleAxisTransform }

procedure TAutoScaleAxisTransform.Assign(Source: TPersistent);
begin
  if Source is TAutoScaleAxisTransform then
    with TAutoScaleAxisTransform(Source) do begin
      Self.FMinValue := FMinValue;
      Self.FMaxValue := FMaxValue;
    end;
  inherited Assign(Source);
end;

function TAutoScaleAxisTransform.AxisToGraph(AX: Double): Double;
begin
  with TAutoScaleTransformData(FDrawData) do
    Result := AX * FScale + FOffset;
end;

procedure TAutoScaleAxisTransform.ClearBounds;
begin
  inherited ClearBounds;
  with TAutoScaleTransformData(FDrawData) do begin
    FMin := SafeInfinity;
    FMax := NegInfinity;
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

function TAutoScaleAxisTransform.MaxValueIsStored: boolean;
begin
  Result := MaxValue <> 1.0;
end;

function TAutoScaleAxisTransform.MinValueIsStored: boolean;
begin
  Result := MinValue <> 0.0;
end;

procedure TAutoScaleAxisTransform.SetMaxValue(const AValue: Double);
begin
  if FMaxValue = AValue then exit;
  FMaxValue := AValue;
  Changed;
end;

procedure TAutoScaleAxisTransform.SetMinValue(const AValue: Double);
begin
  if FMinValue = AValue then exit;
  FMinValue := AValue;
  Changed;
end;

procedure TAutoScaleAxisTransform.UpdateBounds(var AMin, AMax: Double);
begin
  with TAutoScaleTransformData(FDrawData) do begin
    UpdateMinMax(AMin, FMin, FMax);
    UpdateMinMax(AMax, FMin, FMax);
    if FMax = FMin then
      FScale := 1.0
    else
      FScale := (MaxValue - MinValue) / (FMax - FMin);
    FOffset := MinValue - FMin * FScale;
  end;
  if not IsInfinite(AMin) then
    AMin := MinValue;
  if not IsInfinite(AMax) then
    AMax := MaxValue;
end;

initialization

  AxisTransformsClassRegistry := TStringList.Create;
  RegisterAxisTransformClass(TAutoScaleAxisTransform, 'Auto scale');
  RegisterAxisTransformClass(TLinearAxisTransform, 'Linear');
  RegisterAxisTransformClass(TLogarithmAxisTransform, 'Logarithmic');

finalization

  FreeAndNil(AxisTransformsClassRegistry);

end.

