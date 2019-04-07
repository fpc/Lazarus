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

  TPrinterDrawer = class(TScaledCanvasDrawer)
  private
    FPrinter: TPrinter;
  public
    constructor Create(APrinter: TPrinter; AScalePens: Boolean = false);
  end;

implementation

uses
  Forms, Math, TADrawUtils;

{ TPrinterDrawer }

constructor TPrinterDrawer.Create(APrinter: TPrinter;
  AScalePens: Boolean = false);
var
  f: Double;
  si: TScaleItems;
begin
  FPrinter := APrinter;
  f := Max(FPrinter.XDPI, FPrinter.YDPI) / Screen.PixelsPerInch;
  if AScalePens then si := [scalePen] else si := [];
  inherited Create(FPrinter.Canvas, f, si);
end;

end.

