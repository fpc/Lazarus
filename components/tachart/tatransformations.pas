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
    FOnChanged: TNotifyEvent;
    FTransformations: TChartAxisTransformations;
    procedure SetOnChanged(const AValue: TNotifyEvent);
    procedure SetTransformations(AValue: TChartAxisTransformations);
  protected
    procedure ReadState(Reader: TReader); override;
    procedure SetParentComponent(AParent: TComponent); override;
  protected
    procedure Changed;
    function GetIndex: Integer; override;
    procedure SetIndex(AValue: Integer); override;
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

    property OnChanged: TNotifyEvent read FOnChanged write SetOnChanged;
    property Transformations: TChartAxisTransformations read FTransformations write SetTransformations;
  published
    property Enabled: Boolean read FEnabled write FEnabled default true;
  end;

  TAxisTransformClass = class of TAxisTransform;

  TAxisTransformList = class(TFPList)
  end;

  { TChartAxisTransformations }

  TChartAxisTransformations = class (TComponent)
  private
    FList: TAxisTransformList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
  published
    property List: TAxisTransformList read FList;
  end;

  { TLinearAxisTransform }

  TLinearAxisTransform = class(TAxisTransform)
  private
    FOffset: Double;
    FScale: Double;
    procedure SetOffset(AValue: Double);
    procedure SetScale(AValue: Double);
  public
    constructor Create(AOwner: TComponent); override;
  public
    procedure Assign(Source: TPersistent); override;

    function AxisToGraph(AX: Double): Double; override;
    function GraphToAxis(AX: Double): Double; override;
  published
    property Offset: Double read FOffset write SetOffset;
    property Scale: Double read FScale write SetScale;
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
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

constructor TAxisTransform.Create(AOwner: TComponent);
begin
  FEnabled := true;
  inherited Create(AOwner);
end;

destructor TAxisTransform.Destroy;
begin
  Transformations := nil;
  inherited;
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

procedure TAxisTransform.SetIndex(AValue: Integer);
begin
  with Transformations.List do
    Move(Index, EnsureRange(AValue, 0, Count - 1));
end;

procedure TAxisTransform.SetOnChanged(const AValue: TNotifyEvent);
begin
  if TMethod(FOnChanged) = TMethod(AValue) then exit;
  FOnChanged := AValue;
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

{ TChartAxisTransformations }

constructor TChartAxisTransformations.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := TAxisTransformList.Create;
end;

destructor TChartAxisTransformations.Destroy;
begin
  while List.Count > 0 do
    TAxisTransform(List[List.Count - 1]).Free;
  FList.Free;
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

procedure TChartAxisTransformations.SetChildOrder(
  Child: TComponent; Order: Integer);
var
  i: Integer;
begin
  i := List.IndexOf(Child);
  if i >= 0 then
    List.Move(i, Order);
end;

{ TLinearAxisTransform }

procedure TLinearAxisTransform.Assign(Source: TPersistent);
begin
  if Source is TLinearAxisTransform then
    with Source as TLinearAxisTransform do begin
      Self.FOffset := Offset;
      Self.FScale := Scale;
    end
  else
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

initialization

  AxisTransformsClassRegistry := TStringList.Create;
  RegisterAxisTransformClass(TLinearAxisTransform, 'Linear');

finalization

  AxisTransformsClassRegistry.Free;

end.

