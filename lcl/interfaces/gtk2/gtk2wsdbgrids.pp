{ $Id$}
{
 *****************************************************************************
 *                             gtk2wsdbgrids.pp                              * 
 *                             ----------------                              * 
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
unit gtk2wsdbgrids;

{$mode objfpc}{H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as litle as posible circles,
// Uncomment only when needed for registration
////////////////////////////////////////////////////
//  dbgrids,
////////////////////////////////////////////////////
  wsdbgrids, wslclclasses;

type

  { TGtk2WSCustomDbGrid }

  TGtk2WSCustomDbGrid = class(TWSCustomDbGrid)
  private
  protected
  public
  end;

  { TGtk2WSdbGrid }

  TGtk2WSdbGrid = class(TWSdbGrid)
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
//  RegisterWSComponent(TCustomDbGrid, TGtk2WSCustomDbGrid);
//  RegisterWSComponent(TdbGrid, TGtk2WSdbGrid);
////////////////////////////////////////////////////
end.
