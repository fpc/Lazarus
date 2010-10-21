{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TAChartLazarusPkg; 

interface

uses
    TAChartAxis, TAChartUtils, TACustomSeries, TADbSource, TAGraph, TASeries, 
  TASeriesEditor, TASources, TASubcomponentsEditor, TATools, 
  TATransformations, TATypes, TADrawUtils, TAMultiSeries, TALegend, TAStyles, 
  TAFuncSeries, TALegendPanel, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('TADbSource', @TADbSource.Register); 
  RegisterUnit('TAGraph', @TAGraph.Register); 
  RegisterUnit('TASeriesEditor', @TASeriesEditor.Register); 
  RegisterUnit('TASources', @TASources.Register); 
  RegisterUnit('TATools', @TATools.Register); 
  RegisterUnit('TATransformations', @TATransformations.Register); 
  RegisterUnit('TAStyles', @TAStyles.Register); 
  RegisterUnit('TALegendPanel', @TALegendPanel.Register); 
end; 

initialization
  RegisterPackage('TAChartLazarusPkg', @Register); 
end.
