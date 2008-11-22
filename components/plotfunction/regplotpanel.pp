unit regplotpanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  plotpanel,
{$IFNDEF VER2_2}
  exprplotpanel,
{$ENDIF VER2_2}
  lresources;

Procedure Register;

implementation

Procedure Register;

begin
  Classes.RegisterComponents('Misc',[TPlotFunctionPanel
                    {$IFNDEF VER2_2},TPlotExpressionPanel{$ENDIF VER2_2}]);
end;

initialization
  {$i regplotpanel.lrs}
end.

