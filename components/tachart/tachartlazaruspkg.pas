{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TAChartLazarusPkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  TAGraph, TAChartAxis, TAChartUtils, TACustomSeries, TASources, TADbSource, 
  TASeries, TASeriesEditor, TASubcomponentsEditor, TATools, TATransformations, 
  TATypes, TADrawUtils, TAMultiSeries, TALegend, TAStyles, TAFuncSeries, 
  TALegendPanel, TARadialSeries, TACustomSource, TAGeometry, TANavigation, 
  TADrawerCanvas, TADrawerSVG, TAIntervalSources, TAChartAxisUtils, 
  TAChartListbox, TAEnumerators, TAChartExtentLink, TAToolEditors, TAMath, 
  TAChartImageList, TAChartTeeChart, TADataTools, TAAnimatedSource, 
  TATextElements, TAAxisSource, TASeriesPropEditors, TACustomFuncSeries, 
  TAFitUtils, TAGUIConnector, TADiagram, TADiagramDrawing, TADiagramLayout, 
  TAChartStrConsts, TAChartCombos, TAHtml, TAFonts, TAExpressionSeries, 
  TAOpenGL, TAFitLib, TASourcePropEditors, TADataPointsEditor, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('TAGraph', @TAGraph.Register);
  RegisterUnit('TASources', @TASources.Register);
  RegisterUnit('TADbSource', @TADbSource.Register);
  RegisterUnit('TASeriesEditor', @TASeriesEditor.Register);
  RegisterUnit('TATools', @TATools.Register);
  RegisterUnit('TATransformations', @TATransformations.Register);
  RegisterUnit('TAStyles', @TAStyles.Register);
  RegisterUnit('TALegendPanel', @TALegendPanel.Register);
  RegisterUnit('TANavigation', @TANavigation.Register);
  RegisterUnit('TAIntervalSources', @TAIntervalSources.Register);
  RegisterUnit('TAChartListbox', @TAChartListbox.Register);
  RegisterUnit('TAChartExtentLink', @TAChartExtentLink.Register);
  RegisterUnit('TAToolEditors', @TAToolEditors.Register);
  RegisterUnit('TAChartImageList', @TAChartImageList.Register);
  RegisterUnit('TASeriesPropEditors', @TASeriesPropEditors.Register);
  RegisterUnit('TAChartCombos', @TAChartCombos.Register);
  RegisterUnit('TASourcePropEditors', @TASourcePropEditors.Register);
end;

initialization
  RegisterPackage('TAChartLazarusPkg', @Register);
end.
