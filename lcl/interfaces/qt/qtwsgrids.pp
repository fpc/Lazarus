{ $Id$}
{
 *****************************************************************************
 *                               qtwsgrids.pp                                * 
 *                               ------------                                * 
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
unit qtwsgrids;

{$mode objfpc}{H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as litle as posible circles,
// Uncomment only when needed for registration
////////////////////////////////////////////////////
//  grids,
////////////////////////////////////////////////////
  wsgrids, wslclclasses;

type

  { TQtWSStringCellEditor }

  TQtWSStringCellEditor = class(TWSStringCellEditor)
  private
  protected
  public
  end;

  { TQtWSCustomGrid }

  TQtWSCustomGrid = class(TWSCustomGrid)
  private
  protected
  public
  end;

  { TQtWSDrawGrid }

  TQtWSDrawGrid = class(TWSDrawGrid)
  private
  protected
  public
  end;

  { TQtWSStringGrid }

  TQtWSStringGrid = class(TWSStringGrid)
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
//  RegisterWSComponent(TStringCellEditor, TQtWSStringCellEditor);
//  RegisterWSComponent(TCustomGrid, TQtWSCustomGrid);
//  RegisterWSComponent(TDrawGrid, TQtWSDrawGrid);
//  RegisterWSComponent(TStringGrid, TQtWSStringGrid);
////////////////////////////////////////////////////
end.
