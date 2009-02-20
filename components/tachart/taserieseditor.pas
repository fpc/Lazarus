unit TASeriesEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  PropEdits, ComponentEditors, StdCtrls, Menus,
  TAGraph;

type
  { TSeriesComponentEditor }

  TSeriesComponentEditor = class(TComponentEditor)
  private
    FEditorForm: TForm;
  public
    constructor Create(
      AComponent: TComponent; ADesigner: TComponentEditorDesigner); override;
    destructor Destroy; override;
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  { TSeriesPropertyEditor }

  TSeriesPropertyEditor = class(TPropertyEditor)
  private
    FEditorForm: TForm;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
  end;

  { TSeriesEditorForm }

  TSeriesEditorForm = class(TForm)
    MainMenu1: TMainMenu;
    miAddSeries: TMenuItem;
    miAdd: TMenuItem;
    SeriesListBox: TListBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure miAddSeriesClick(Sender: TObject);
  private
    FChart: TChart;
    FComponentEditor: TSeriesComponentEditor;
    FPropertyEditor: TSeriesPropertyEditor;
    FDesigner: TComponentEditorDesigner;
    procedure RefreshSeriesList;
  public
    constructor Create(
      AOwner: TComponent; AChart: TChart; AComponentEditor: TComponentEditor;
      APropertyEditor: TPropertyEditor); reintroduce;
  end;

  procedure Register;

resourcestring
  sesSeriesEditorTitle = 'Edit series';

implementation

procedure Register;
begin
  RegisterComponentEditor(TChart, TSeriesComponentEditor);
end;

{ TSeriesComponentEditor }

constructor TSeriesComponentEditor.Create(
  AComponent: TComponent; ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent, ADesigner);
end;

destructor TSeriesComponentEditor.Destroy;
begin
  FreeAndNil(FEditorForm);
  inherited Destroy;
end;

procedure TSeriesComponentEditor.ExecuteVerb(Index: Integer);
var
  chart: TChart;
begin
  if Index <> 0 then exit;
  chart := GetComponent as TChart;
  if chart = nil then
    raise Exception.Create('TSeriesComponentEditor.Chart=nil');
  if FEditorForm = nil then
    FEditorForm := TSeriesEditorForm.Create(Application, chart, Self, nil);
  FEditorForm.ShowOnTop;
end;

function TSeriesComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := sesSeriesEditorTitle;
    else Result := '';
  end;
end;

function TSeriesComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TSeriesPropertyEditor }

procedure TSeriesPropertyEditor.Edit;
var
  chart: TChart;
begin
  chart := GetComponent(0) as TChart;
  if chart = nil then
    raise Exception.Create('TSeriesComponentEditor.Chart=nil');
  if FEditorForm = nil then
    FEditorForm := TSeriesEditorForm.Create(Application, chart, nil, Self);
  FEditorForm.ShowOnTop;
end;

function TSeriesPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TSeriesPropertyEditor.GetValue: ansistring;
begin
  with GetObjectValue as TChartSeriesList do
    if Count = 1 then
      Result := '1 item'
    else
      Result := IntToStr(Count) + ' items';
end;

{ TSeriesEditorForm }

constructor TSeriesEditorForm.Create(
  AOwner: TComponent; AChart: TChart; AComponentEditor: TComponentEditor;
  APropertyEditor: TPropertyEditor);
begin
  inherited Create(AOwner);
  FChart := AChart;
  FComponentEditor := AComponentEditor as TSeriesComponentEditor;
  FPropertyEditor := APropertyEditor as TSeriesPropertyEditor;
  if FComponentEditor <> nil then
    FDesigner := FComponentEditor.Designer
  else
    FDesigner := FindRootDesigner(AChart) as TComponentEditorDesigner;
  Caption := sesSeriesEditorTitle + ' - ' + FChart.Name;
  RefreshSeriesList;
end;

procedure TSeriesEditorForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TSeriesEditorForm.FormDestroy(Sender: TObject);
begin
  if FComponentEditor <> nil then
    FComponentEditor.FEditorForm := nil;
  if FPropertyEditor <> nil then
    FPropertyEditor.FEditorForm := nil;
end;

procedure TSeriesEditorForm.miAddSeriesClick(Sender: TObject);
begin
end;

procedure TSeriesEditorForm.RefreshSeriesList;
var
  i: Integer;
begin
  SeriesListBox.Clear;
  for i := 0 to FChart.SeriesCount - 1 do
    SeriesListBox.Items.AddObject(FChart.Series[i].Name, FChart.Series[i]);
end;

initialization
  {$I taserieseditor.lrs}

end.

