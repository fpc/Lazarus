unit regplotpanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  plotpanel,
  exprplotpanel,
  lresources;

Procedure Register;

implementation

Procedure Register;

begin
  Classes.RegisterComponents('Misc',[TPlotFunctionPanel, TPlotExpressionPanel]);
end;

initialization
  {$i regplotpanel.lrs}
end.

