{ $Id$}
{
 *****************************************************************************
 *                              Gtk2WSGrids.pp                               * 
 *                              --------------                               * 
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
//  Grids,
////////////////////////////////////////////////////
  WSGrids, WSLCLClasses;

type

  { TGtk2WSStringCellEditor }

  TGtk2WSStringCellEditor = class(TWSStringCellEditor)
  private
  protected
  public
  end;

  { TGtk2WSCustomGrid }

  TGtk2WSCustomGrid = class(TWSCustomGrid)
  private
  protected
  public
  end;

  { TGtk2WSDrawGrid }

  TGtk2WSDrawGrid = class(TWSDrawGrid)
  private
  protected
  public
  end;

  { TGtk2WSStringGrid }

  TGtk2WSStringGrid = class(TWSStringGrid)
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
//  RegisterWSComponent(TStringCellEditor, TGtk2WSStringCellEditor);
//  RegisterWSComponent(TCustomGrid, TGtk2WSCustomGrid);
//  RegisterWSComponent(TDrawGrid, TGtk2WSDrawGrid);
//  RegisterWSComponent(TStringGrid, TGtk2WSStringGrid);
////////////////////////////////////////////////////
end.