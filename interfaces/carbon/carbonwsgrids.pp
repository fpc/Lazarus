{ $Id$}
{
 *****************************************************************************
 *                               CarbonWSGrids.pp                            * 
 *                               ------------                                * 
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
unit CarbonWSGrids;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  Grids,
////////////////////////////////////////////////////
  WSGrids, WSLCLClasses;

type

  { TCarbonWSStringCellEditor }

  TCarbonWSStringCellEditor = class(TWSStringCellEditor)
  published
  end;

  { TCarbonWSCustomGrid }

  TCarbonWSCustomGrid = class(TWSCustomGrid)
  published
  end;

  { TCarbonWSDrawGrid }

  TCarbonWSDrawGrid = class(TWSDrawGrid)
  published
  end;

  { TCarbonWSStringGrid }

  TCarbonWSStringGrid = class(TWSStringGrid)
  published
  end;


implementation

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TStringCellEditor, TCarbonWSStringCellEditor);
//  RegisterWSComponent(TCustomGrid, TCarbonWSCustomGrid);
//  RegisterWSComponent(TDrawGrid, TCarbonWSDrawGrid);
//  RegisterWSComponent(TStringGrid, TCarbonWSStringGrid);
////////////////////////////////////////////////////
end.