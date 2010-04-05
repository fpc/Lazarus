{ This file was automatically created by Lazarus. do not edit ! 
  This source is only used to compile and install the package.
 }

unit tachartlazaruspkg; 

interface

uses
    TASeries, TAGraph, TAChartUtils, TASeriesEditor, TATypes, TASources, 
  TADbSource, TACustomSeries, TALegend, TAChartAxis, TATools, 
  TASubcomponentsEditor, TATransformations, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('TAGraph', @TAGraph.Register); 
  RegisterUnit('TASeriesEditor', @TASeriesEditor.Register); 
  RegisterUnit('TASources', @TASources.Register); 
  RegisterUnit('TADbSource', @TADbSource.Register); 
  RegisterUnit('TATools', @TATools.Register); 
  RegisterUnit('TATransformations', @TATransformations.Register); 
end; 

initialization
  RegisterPackage('TAChartLazarusPkg', @Register); 
end.
