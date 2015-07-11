{ $Id: FpGuiwsgrids.pp 5319 2004-03-17 20:11:29Z marc $}
{
 *****************************************************************************
 *                               FpGuiWSGrids.pp                                * 
 *                               ------------                                * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.LCL, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit FpGuiWSGrids;

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
  { TFpGuiWSCustomGrid }

  TFpGuiWSCustomGrid = class(TWSCustomGrid)
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
//  RegisterWSComponent(TCustomGrid, TFpGuiWSCustomGrid);
////////////////////////////////////////////////////
end.
