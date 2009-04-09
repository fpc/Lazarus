{ $Id: FpGuiwsdbgrids.pp 5319 2004-03-17 20:11:29Z marc $}
{
 *****************************************************************************
 *                              FpGuiWSDBGrids.pp                               * 
 *                              --------------                               * 
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
unit FpGuiWSDBGrids;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  DBGrids,
////////////////////////////////////////////////////
  WSDBGrids, WSLCLClasses;

type

  { TFpGuiWSCustomDbGrid }

  TFpGuiWSCustomDbGrid = class(TWSCustomDbGrid)
  private
  protected
  public
  end;

  { TFpGuiWSdbGrid }

  TFpGuiWSdbGrid = class(TWSdbGrid)
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
//  RegisterWSComponent(TCustomDbGrid, TFpGuiWSCustomDbGrid);
//  RegisterWSComponent(TdbGrid, TFpGuiWSdbGrid);
////////////////////////////////////////////////////
end.
