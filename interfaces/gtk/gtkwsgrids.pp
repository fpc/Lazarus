{ $Id$}
{
 *****************************************************************************
 *                               GtkWSGrids.pp                               * 
 *                               -------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit GtkWSGrids;

{$mode objfpc}{$H+}

interface

uses
  Controls, Graphics, Grids, WSGrids, WSLCLClasses;

type

  { TGtkWSStringCellEditor }

  TGtkWSStringCellEditor = class(TWSStringCellEditor)
  published
  end;

  { TGtkWSCustomGrid }

  TGtkWSCustomGrid = class(TWSCustomGrid)
  published
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
  end;

  { TGtkWSDrawGrid }

  TGtkWSDrawGrid = class(TWSDrawGrid)
  published
  end;

  { TGtkWSStringGrid }

  TGtkWSStringGrid = class(TWSStringGrid)
  published
  end;


implementation

{ TGtkWSCustomGrid }

class procedure TGtkWSCustomGrid.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
begin
  //
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TStringCellEditor, TGtkWSStringCellEditor);
  RegisterWSComponent(TCustomGrid, TGtkWSCustomGrid);
//  RegisterWSComponent(TDrawGrid, TGtkWSDrawGrid);
//  RegisterWSComponent(TStringGrid, TGtkWSStringGrid);
////////////////////////////////////////////////////
end.
