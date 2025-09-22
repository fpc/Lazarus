{
 *****************************************************************************
 *                               QtWSGrids.pp                                * 
 *                               ------------                                * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit QtWSGrids;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  Controls, Types, Graphics, Grids,
  // Widgetset
  WSGrids, WSLCLClasses;

type

  { TQtWSCustomGrid }

  TQtWSCustomGrid = class(TWSCustomGrid)
  published
    class function GetEditorBoundsFromCellRect(ACanvas: TCanvas;
      const ACellRect: TRect; const AColumnLayout: TTextLayout): TRect; override;
  end;


implementation

{ TQtWSCustomGrid }

class function TQtWSCustomGrid.GetEditorBoundsFromCellRect(ACanvas: TCanvas;
  const ACellRect: TRect; const AColumnLayout: TTextLayout): TRect;
var
  EditorTop: LongInt;
  TextHeight: Integer;
begin
  Result:=ACellRect;
  Dec(Result.Right);
  Dec(Result.Bottom);
  TextHeight := ACanvas.TextHeight(' ');
  case AColumnLayout of
    tlTop: EditorTop:=Result.Top+varCellPadding;
    tlCenter: EditorTop:=Result.Top+(Result.Bottom-Result.Top-TextHeight+1) div 2;
    tlBottom: EditorTop:=Result.Bottom-varCellPadding-TextHeight+1;
  end;
  if EditorTop>Result.Top then Result.Top:=EditorTop;
  Result.Bottom:=Result.Top+TextHeight;
end;

end.
