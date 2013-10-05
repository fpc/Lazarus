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

{$R regplotpanel.res}

Procedure Register;

begin
  Classes.RegisterComponents('Misc',[TPlotFunctionPanel, TPlotExpressionPanel]);
end;

end.

