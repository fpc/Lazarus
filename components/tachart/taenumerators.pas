{

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
unit TAEnumerators;

{$H+}

interface

uses
  Classes, Types,
  TACustomSeries, TAGraph;

type
  TBasicFilteredChartSeriesEnumeratorFactory = class
  strict protected
    FChart: TChart;
    FFilter: TBooleanDynArray;
  public
    constructor Create(AChart: TChart; AFilter: TBooleanDynArray = nil);
    property Chart: TChart read FChart;
    property Filter: TBooleanDynArray read FFilter;
  end;

  {$IFNDEF fpdoc} // Workaround for issue #18549.
  generic TFilteredChartSeriesEnumerator<_T> = class
  {$ELSE}
  TFilteredChartSeriesEnumerator = class
  {$ENDIF}
  strict private
    FFactory: TBasicFilteredChartSeriesEnumeratorFactory;
    FPosition: Integer;
  public
    constructor Create(AFactory: TBasicFilteredChartSeriesEnumeratorFactory);
    destructor Destroy; override;
    function GetCurrent: _T;
    function MoveNext: Boolean;
    property Current: _T read GetCurrent;
  end;

  {$IFNDEF fpdoc} // Workaround for issue #18549.
  generic TFilteredChartSeriesEnumeratorFactory<_T> = class(
    TBasicFilteredChartSeriesEnumeratorFactory)
  {$ELSE}
  TFilteredChartSeriesEnumeratorFactory = class(
    TBasicFilteredChartSeriesEnumeratorFactory)
  {$ENDIF}
  public
    type
      TEnum = specialize TFilteredChartSeriesEnumerator<_T>;
    function GetEnumerator: TEnum;
  end;

  {$IFNDEF fpdoc} // Workaround for issue #18549.
  TFilteredCustomChartSeriesEnumeratorFactory =
    specialize TFilteredChartSeriesEnumeratorFactory<TCustomChartSeries>;
  {$ENDIF}

  function CustomSeries(
    AChart: TChart; AFilter: TBooleanDynArray = nil
  ): TFilteredCustomChartSeriesEnumeratorFactory;

implementation

uses
  SysUtils;

function CustomSeries(
  AChart: TChart; AFilter: TBooleanDynArray):
  TFilteredCustomChartSeriesEnumeratorFactory;
begin
  Result := TFilteredCustomChartSeriesEnumeratorFactory.Create(AChart, AFilter);
end;

{ TBasicFilteredChartSeriesEnumeratorFactory }

constructor TBasicFilteredChartSeriesEnumeratorFactory.Create(
  AChart: TChart; AFilter: TBooleanDynArray);
begin
  FChart := AChart;
  FFilter := AFilter;
end;

{ TFilteredChartSeriesEnumerator }

constructor TFilteredChartSeriesEnumerator.Create(
  AFactory: TBasicFilteredChartSeriesEnumeratorFactory);
begin
  FFactory := AFactory;
  FPosition := -1;
end;

destructor TFilteredChartSeriesEnumerator.Destroy;
begin
  FreeAndNil(FFactory);
  inherited;
end;

function TFilteredChartSeriesEnumerator.GetCurrent: _T;
begin
  Result := _T(FFactory.Chart.Series.List[FPosition]);
end;

function TFilteredChartSeriesEnumerator.MoveNext: Boolean;
begin
  Result := false;
  repeat
    FPosition += 1;
    if FPosition >= FFactory.Chart.SeriesCount then exit;
    Result :=
      (TBasicChartSeries(FFactory.Chart.Series.List[FPosition]).InheritsFrom(_T)) and
      ((FPosition > High(FFactory.Filter)) or FFactory.Filter[FPosition]);
  until Result;
end;

{ TFilteredChartSeriesEnumeratorFactory }

function TFilteredChartSeriesEnumeratorFactory.GetEnumerator: TEnum;
begin
  Result := TEnum.Create(Self);
end;

end.

