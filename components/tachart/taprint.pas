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
unit TAPrint;

{$mode objfpc}

interface

uses
  Printers, TADrawUtils;

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

