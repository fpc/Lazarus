{
 /***************************************************************************
                               TASeriesEditor.pas
                               ----------------
              Component Library Standard Graph Design-time Editors


 ***************************************************************************/

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

Author: Alexander Klenin

}
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
    miDelete: TMenuItem;
    miAdd: TMenuItem;
    SeriesListBox: TListBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
    procedure SeriesListBoxClick(Sender: TObject);
  private
    FChart: TChart;
    FComponentEditor: TSeriesComponentEditor;
    FPropertyEditor: TSeriesPropertyEditor;
    FDesigner: TComponentEditorDesigner;
    procedure BuildCaption;
    procedure InitAddMenu;
    procedure RefreshSeriesList;
    procedure miAddSeriesClick(Sender: TObject);
    function FindSeries(ASeries: TObject; out AIndex: Integer): Boolean;

    procedure SelectionChanged;
    procedure OnComponentRenamed(AComponent: TComponent);
    procedure OnPersistentDeleting(APersistent: TPersistent);
    procedure OnGetSelection(const ASelection: TPersistentSelectionList);
    procedure OnSetSelection(const ASelection: TPersistentSelectionList);
    procedure OnPersistentAdded(APersistent: TPersistent; ASelect: Boolean);
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
  RegisterPropertyEditor(
    TypeInfo(TChartSeriesList), TChart, 'Series', TSeriesPropertyEditor);
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

procedure TSeriesEditorForm.BuildCaption;
begin
  Caption := sesSeriesEditorTitle + ' - ' + FChart.Name;
end;

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
  BuildCaption;
  InitAddMenu;

  RefreshSeriesList;

  GlobalDesignHook.AddHandlerComponentRenamed(@OnComponentRenamed);
  GlobalDesignHook.AddHandlerPersistentDeleting(@OnPersistentDeleting);
  GlobalDesignHook.AddHandlerGetSelection(@OnGetSelection);
  GlobalDesignHook.AddHandlerSetSelection(@OnSetSelection);
  GlobalDesignHook.AddHandlerPersistentAdded(@OnPersistentAdded);

  SelectionChanged;
end;

function TSeriesEditorForm.FindSeries(
  ASeries: TObject; out AIndex: Integer): Boolean;
begin
  if ASeries is TBasicChartSeries then
    AIndex := SeriesListBox.Items.IndexOfObject(ASeries)
  else
    AIndex := -1;
  Result := AIndex >= 0;
end;

procedure TSeriesEditorForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TSeriesEditorForm.FormDestroy(Sender: TObject);
begin
  if FComponentEditor <> nil then begin
    FComponentEditor.FEditorForm := nil;
    if
      (FChart <> nil) and (not (csDestroying in FChart.ComponentState)) and
      (SeriesListBox.SelCount > 0)
    then
      GlobalDesignHook.SelectOnlyThis(FChart);
  end;
  if FPropertyEditor <> nil then
    FPropertyEditor.FEditorForm := nil;
  if Assigned(GlobalDesignHook) then
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
end;

procedure TSeriesEditorForm.InitAddMenu;
var
  i: Integer;
  mi: TMenuItem;
begin
  for i := 0 to SeriesClassRegistry.Count - 1 do begin
    mi := TMenuItem.Create(Self);
    mi.OnClick := @miAddSeriesClick;
    mi.Caption := SeriesClassRegistry[i];
    mi.Tag := i;
    miAdd.Add(mi);
  end;
end;

procedure TSeriesEditorForm.miAddSeriesClick(Sender: TObject);
var
  s: TBasicChartSeries;
  c: TSeriesClass;
  n: String;
begin
  c := TSeriesClass(SeriesClassRegistry.Objects[(Sender as TMenuItem).Tag]);
  n := Copy(c.ClassName, 2, Length(c.ClassName) - 1);
  s := c.Create(FChart.Owner);
  try
    s.Name := FDesigner.CreateUniqueComponentName(FChart.Name + n);
    FChart.AddSeries(s);
    FDesigner.PropertyEditorHook.PersistentAdded(s, true);
    FDesigner.Modified;
    RefreshSeriesList;
  except
    s.Free;
    raise;
  end;
end;

procedure TSeriesEditorForm.miDeleteClick(Sender: TObject);
var
  i: Integer;
  s: TBasicChartSeries;
begin
  if SeriesListBox.SelCount = 0 then exit;
  for i := SeriesListBox.Items.Count - 1 downto 0 do
    if SeriesListBox.Selected[i] then begin
      s := TBasicChartSeries(SeriesListBox.Items.Objects[i]);
      SeriesListBox.Items.Delete(i);
      FDesigner.PropertyEditorHook.PersistentDeleting(s);
      s.Free;
    end;
  FDesigner.Modified;
  SelectionChanged;
end;

procedure TSeriesEditorForm.OnComponentRenamed(AComponent: TComponent);
var
  i: Integer;
begin
  if AComponent = nil then exit;
  if FindSeries(AComponent, i) then
    SeriesListBox.Items[i] := AComponent.Name
  else if AComponent = FChart then
    BuildCaption;
end;

procedure TSeriesEditorForm.OnGetSelection(
  const ASelection: TPersistentSelectionList);
var
  i: Integer;
begin
  if ASelection = nil then exit;
  ASelection.Clear;
  with SeriesListBox do
    for i := 0 to Items.Count - 1 do
      if Selected[i] then
        ASelection.Add(TPersistent(Items.Objects[i]));
end;

procedure TSeriesEditorForm.OnPersistentAdded(
  APersistent: TPersistent; ASelect: Boolean);
var
  i: Integer;
  s: TBasicChartSeries;
begin
  if (APersistent = nil) or not (APersistent is TBasicChartSeries) then exit;
  s := APersistent as TBasicChartSeries;
  if s.ParentChart <> FChart then exit;
  i := SeriesListBox.Items.AddObject(s.Name, s);
  SeriesListBox.Selected[i] := ASelect;
end;

procedure TSeriesEditorForm.OnPersistentDeleting(APersistent: TPersistent);
var
  i: Integer;
begin
  if FindSeries(APersistent, i) then
    SeriesListBox.Items.Delete(i);
end;

procedure TSeriesEditorForm.OnSetSelection(
  const ASelection: TPersistentSelectionList);
var
  i, j: Integer;
begin
  if ASelection = nil then exit;
  SeriesListBox.ClearSelection;
  for i := 0 to ASelection.Count - 1 do
    if FindSeries(ASelection.Items[i], j) then
      SeriesListBox.Selected[j] := true;
end;

procedure TSeriesEditorForm.RefreshSeriesList;
var
  i: Integer;
begin
  SeriesListBox.Clear;
  for i := 0 to FChart.SeriesCount - 1 do
    SeriesListBox.Items.AddObject(FChart.Series[i].Name, FChart.Series[i]);
end;

procedure TSeriesEditorForm.SelectionChanged;
var
  sel: TPersistentSelectionList;
begin
  GlobalDesignHook.RemoveHandlerSetSelection(@OnSetSelection);
  try
    sel := TPersistentSelectionList.Create;
    try
      OnGetSelection(sel);
      FDesigner.PropertyEditorHook.SetSelection(sel) ;
    finally
      sel.Free;
    end;
  finally
    GlobalDesignHook.AddHandlerSetSelection(@OnSetSelection);
  end;
end;

procedure TSeriesEditorForm.SeriesListBoxClick(Sender: TObject);
begin
  SelectionChanged;
end;

initialization
  {$I taserieseditor.lrs}

end.

