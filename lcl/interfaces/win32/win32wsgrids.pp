{ $Id$}
{
 *****************************************************************************
 *                              Win32WSGrids.pp                              * 
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
unit Win32WSGrids;

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

  { TWin32WSStringCellEditor }

  TWin32WSStringCellEditor = class(TWSStringCellEditor)
  private
  protected
  public
  end;

  { TWin32WSCustomGrid }

  TWin32WSCustomGrid = class(TWSCustomGrid)
  private
  protected
  public
  end;

  { TWin32WSDrawGrid }

  TWin32WSDrawGrid = class(TWSDrawGrid)
  private
  protected
  public
  end;

  { TWin32WSStringGrid }

  TWin32WSStringGrid = class(TWSStringGrid)
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
//  RegisterWSComponent(TStringCellEditor, TWin32WSStringCellEditor);
//  RegisterWSComponent(TCustomGrid, TWin32WSCustomGrid);
//  RegisterWSComponent(TDrawGrid, TWin32WSDrawGrid);
//  RegisterWSComponent(TStringGrid, TWin32WSStringGrid);
////////////////////////////////////////////////////
end.