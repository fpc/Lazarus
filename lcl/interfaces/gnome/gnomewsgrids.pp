{ $Id$}
{
 *****************************************************************************
 *                              GnomeWSGrids.pp                              * 
 *                              ---------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit GnomeWSGrids;

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

  { TGnomeWSStringCellEditor }

  TGnomeWSStringCellEditor = class(TWSStringCellEditor)
  private
  protected
  public
  end;

  { TGnomeWSCustomGrid }

  TGnomeWSCustomGrid = class(TWSCustomGrid)
  private
  protected
  public
  end;

  { TGnomeWSDrawGrid }

  TGnomeWSDrawGrid = class(TWSDrawGrid)
  private
  protected
  public
  end;

  { TGnomeWSStringGrid }

  TGnomeWSStringGrid = class(TWSStringGrid)
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
//  RegisterWSComponent(TStringCellEditor, TGnomeWSStringCellEditor);
//  RegisterWSComponent(TCustomGrid, TGnomeWSCustomGrid);
//  RegisterWSComponent(TDrawGrid, TGnomeWSDrawGrid);
//  RegisterWSComponent(TStringGrid, TGnomeWSStringGrid);
////////////////////////////////////////////////////
end.