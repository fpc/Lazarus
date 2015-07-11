{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}
unit TAPrint;

{$H+}

interface

uses
  Printers, TADrawerCanvas;

type

  { TPrinterDrawer }

  TPrinterDrawer = class(TCanvasDrawer)
  private
    FPrinter: TPrinter;
    FCoeff: Double;
  public
    constructor Create(APrinter: TPrinter);
    function Scale(ADistance: Integer): Integer; override;
  end;

implementation

uses
  Forms, Math;

{ TPrinterDrawer }

constructor TPrinterDrawer.Create(APrinter: TPrinter);
begin
  FPrinter := APrinter;
  inherited Create(FPrinter.Canvas);
  FCoeff := Max(FPrinter.XDPI, FPrinter.YDPI) / Screen.PixelsPerInch;
end;

function TPrinterDrawer.Scale(ADistance: Integer): Integer;
begin
  Result := Round(ADistance * FCoeff);
end;

end.

