{ $Id$}
{
 *****************************************************************************
 *                              Gtk2WSGrids.pp                               * 
 *                              --------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Gtk2WSGrids;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Types, Grids, Graphics,
////////////////////////////////////////////////////
  WSGrids, WSLCLClasses;

type

  { TGtk2WSCustomGrid }

  TGtk2WSCustomGrid = class(TWSCustomGrid)
  published
    class function GetEditorBoundsFromCellRect(ACanvas: TCanvas;
      const ACellRect: TRect; const AColumnLayout: TTextLayout): TRect; override;
    class procedure Invalidate(sender: TCustomGrid); override;
  end;

implementation

{ TGtk2WSCustomGrid }

class function TGtk2WSCustomGrid.GetEditorBoundsFromCellRect(ACanvas: TCanvas;
  const ACellRect: TRect; const AColumnLayout: TTextLayout): TRect;
var
  EditorTop: LongInt;
  TextHeight: Integer;
begin
  Result:=ACellRect;
  Inc(Result.Left);
  Dec(Result.Right, 2);
  Dec(Result.Bottom);
  TextHeight := ACanvas.TextHeight(' ');
  case AColumnLayout of
    tlTop: EditorTop:=Result.Top+constCellPadding;
    tlCenter: EditorTop:=Result.Top+(Result.Bottom-Result.Top-TextHeight+1) div 2;
    tlBottom: EditorTop:=Result.Bottom-constCellPadding-TextHeight+1;
  end;
  if EditorTop>Result.Top then Result.Top:=EditorTop;
  Result.Bottom:=Result.Top+TextHeight;
end;

class procedure TGtk2WSCustomGrid.Invalidate(sender: TCustomGrid);
begin
  Sender.Invalidate;
end;

end.
