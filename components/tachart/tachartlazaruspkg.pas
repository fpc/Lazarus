{ Этот файл был автоматически создан Lazarus. Не редактировать!
  Исходный код используется только для компиляции и установки пакета.
 }

unit tachartlazaruspkg;

interface

uses
    TAChartAxis, TAChartUtils, TACustomSeries, TADbSource, TAGraph, TASeries, 
  TASeriesEditor, TASources, TASubcomponentsEditor, TATools, 
  TATransformations, TATypes, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('TADbSource',@TADbSource.Register);
  RegisterUnit('TAGraph',@TAGraph.Register);
  RegisterUnit('TASeriesEditor',@TASeriesEditor.Register);
  RegisterUnit('TASources',@TASources.Register);
  RegisterUnit('TATools',@TATools.Register);
  RegisterUnit('TATransformations',@TATransformations.Register);
end;

initialization
  RegisterPackage('TAChartLazarusPkg',@Register);
end.
