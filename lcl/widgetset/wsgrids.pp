{ $Id$}
{
 *****************************************************************************
 *                                WSGrids.pp                                 * 
 *                                ----------                                 * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit WSGrids;

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
  WSLCLClasses, WSMaskEdit, WSControls;

type

  { TWSStringCellEditor }

  TWSStringCellEditor = class(TWSCustomMaskEdit)
  private
  protected
  public
  end;

  { TWSCustomGrid }

  TWSCustomGrid = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TWSDrawGrid }

  TWSDrawGrid = class(TWSCustomGrid)
  private
  protected
  public
  end;

  { TWSStringGrid }

  TWSStringGrid = class(TWSDrawGrid)
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
//  RegisterWSComponent(TStringCellEditor, TWSStringCellEditor);
//  RegisterWSComponent(TCustomGrid, TWSCustomGrid);
//  RegisterWSComponent(TDrawGrid, TWSDrawGrid);
//  RegisterWSComponent(TStringGrid, TWSStringGrid);
////////////////////////////////////////////////////
end.
