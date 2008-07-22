{ $Id$}
{
 *****************************************************************************
 *                               CarbonWSGrids.pp                                * 
 *                               ------------                                * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
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
  private
  protected
  public
  end;

  { TCarbonWSCustomGrid }

  TCarbonWSCustomGrid = class(TWSCustomGrid)
  private
  protected
  public
  end;

  { TCarbonWSDrawGrid }

  TCarbonWSDrawGrid = class(TWSDrawGrid)
  private
  protected
  public
  end;

  { TCarbonWSStringGrid }

  TCarbonWSStringGrid = class(TWSStringGrid)
  private
  protected
  public
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