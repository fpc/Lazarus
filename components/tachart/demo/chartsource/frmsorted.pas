unit frmSorted;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Controls, StdCtrls,
  TAGraph, TAChartUtils, TAMultiSeries, TACustomSource, TASources;

type
  TDataForSorting = record
    Year: Integer;
    CityName: String;
    Visitors: Integer;
    Color: TChartColor;
  end;

  TSortedFrame = class(TFrame)
    btnSort: TButton;
    BubbleChart: TChart;
    BubbleSeries: TBubbleSeries;
    lblSortInfo: TLabel;
    SortedChartSource: TSortedChartSource;
    UserDefinedChartSource: TUserDefinedChartSource;
    procedure btnSortClick(Sender: TObject);
    procedure BubbleChartAfterPaint({%H-}ASender: TChart);
    procedure UserDefinedChartSourceGetChartDataItem({%H-}ASource:
      TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
  private
    FDataForSorting: array of TDataForSorting;
    procedure CreateDataForSorting;
  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

constructor TSortedFrame.Create(AOwner: TComponent);
begin
  inherited;
  CreateDataForSorting;
end;

procedure TSortedFrame.btnSortClick(Sender: TObject);
begin
  SortedChartSource.Sorted := not SortedChartSource.Sorted;
end;

procedure TSortedFrame.BubbleChartAfterPaint(ASender: TChart);
begin
  if SortedChartSource.IsSorted then
    lblSortInfo.Caption := 'Sorted by radius (descending)'
  else
    lblSortInfo.Caption := 'Not sorted';
end;

procedure TSortedFrame.UserDefinedChartSourceGetChartDataItem(ASource:
  TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := FDataForSorting[AIndex].Year;
  AItem.Y := 0;
  AItem.YList[0] := Sqrt(FDataForSorting[AIndex].Visitors/100000 / PI);
  AItem.Text := FDataForSorting[AIndex].CityName;
  AItem.Color := FDataForSorting[AIndex].Color;
end;

procedure TSortedFrame.CreateDataForSorting;

  procedure AddCity(AYear: Integer; const ACityName: string;
    ANumberOfVisitors: Integer; AColor: TChartColor);
  begin
    SetLength(FDataForSorting, Length(FDataForSorting) + 1);
    with FDataForSorting[High(FDataForSorting)] do begin
      Year := AYear;
      CityName := ACityName;
      Visitors := ANumberOfVisitors;
      Color := AColor;
    end;
  end;

begin
  SetLength(FDataForSorting, 0);
  AddCity(1967, 'City A', 155000, $00FFFF);  // yellow
  AddCity(1968, 'City B', 30000, $FF0000);   // blue
  AddCity(1969, 'City C', 450000, $FF00FF);  // fuchsia
  AddCity(1970, 'City D', 50000, $FFFF00);   // aqua
  AddCity(1971, 'City E', 80000, $00FF00);   // lime
  AddCity(1972, 'City F', 615000, $0000FF);  // red
  UserDefinedChartSource.PointsNumber := Length(FDataForSorting);

  { These settings are made in the Object Inspector for the SortedChartSource:
    - Origin = UserDefinedChartSource
    - SortBy = sbY
    - SortIndex = 1            // --> sort by radius
    - SortDir = sdDescending   // --> draw large bubbles first
  }
end;

end.

