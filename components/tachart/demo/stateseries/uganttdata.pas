{ This unit provides a data structure for the Gantt data.
  Basically Gantt data could be added to the StateSeries directly, but the
  same data are needed at several places. Therefore, it is better to keep them
  at a common place. }

unit uGanttData;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type
  TBasicGanttItem = class
  protected
    FTitle: String;
    FStartDate: TDateTime;
    FDuration: Double;
    property Duration: Double read FDuration;
  public
    property Title: String read FTitle;
  end;

  TGanttTask = class(TBasicGanttItem)
  private
    FPercentageComplete: Double;
    function GetEndDate: TDateTime;
  public
    constructor Create(ATitle: String; AStartDate: TDateTime;
      ADuration, APercentageDone: Double);
    property EndDate: TDateTime read GetEndDate;
    property PercentageComplete: double read FPercentageComplete write FPercentageComplete;
    property StartDate: TDateTime read FStartDate;
  end;

  TGanttMilestone = class(TBasicGanttItem)
  private
    FComplete: Boolean;
  public
    constructor Create(ATitle: String; ADate: TDateTime; AComplete: Boolean);
    property Complete: Boolean read FComplete write FComplete;
    property DateDue: TDateTime read FStartDate;
  end;

  TGanttTaskList = class(TObjectList)
  private
    function GetItem(AIndex: Integer): TBasicGanttItem;
    procedure SetItem(AIndex: Integer; AValue: TBasicGanttItem);
  public
    function AddMilestone(ATitle: String; ADate: TDateTime; AComplete: Boolean): Integer;
    function AddTask(ATitle: String; AStartDate: TDateTime; ADuration, APercentageDone: Double): Integer;
    property Items[AIndex: Integer]: TBasicGanttItem read GetItem write SetItem; default;
  end;

implementation

constructor TGanttTask.Create(ATitle: String; AStartDate: TDateTime;
  ADuration, APercentageDone: Double);
begin
  inherited Create;
  FTitle := ATitle;
  FStartDate := AStartDate;
  FDuration := ADuration;
  FPercentageComplete := APercentageDone;
end;

function TGanttTask.GetEndDate: TDateTime;
begin
  Result := FStartDate + FDuration;
end;

{ TGanttMilestone }

constructor TGanttMilestone.Create(ATitle: String; ADate: TDateTime;
  AComplete: Boolean);
begin
  inherited Create;
  FTitle := ATitle;
  FStartDate := ADate;
  FComplete := AComplete;
end;

{ TGanttTaskList }

function TGanttTaskList.AddMilestone(ATitle: String; ADate: TDateTime;
  AComplete: Boolean): Integer;
var
  item: TBasicGanttItem;
begin
  item := TGanttMilestone.Create(ATitle, ADate, AComplete);
  Result := Add(item);
end;

function TGanttTaskList.AddTask(ATitle: String; AStartDate: TDateTime;
  ADuration, APercentageDone: Double): Integer;
var
  item: TBasicGanttItem;
begin
  item := TGanttTask.Create(ATitle, AStartDate, ADuration, APercentageDone);
  Result := Add(item);
end;

function TGanttTaskList.GetItem(AIndex: Integer): TBasicGanttItem;
begin
  Result := TBasicGanttItem(inherited Items[AIndex]);
end;

procedure TGanttTaskList.SetItem(AIndex: Integer; AValue: TBasicGanttItem);
begin
  inherited Items[AIndex] := AValue;
end;

end.

