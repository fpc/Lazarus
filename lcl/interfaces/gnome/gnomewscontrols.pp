{ $Id$}
{
 *****************************************************************************
 *                            gnomewscontrols.pp                             * 
 *                            ------------------                             * 
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
unit gnomewscontrols;

{$mode objfpc}{H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as litle as posible circles,
// Uncomment only when needed for registration
////////////////////////////////////////////////////
//  controls,
////////////////////////////////////////////////////
  wscontrols, wslclclasses;

type

  { TGnomeWSDragImageList }

  TGnomeWSDragImageList = class(TWSDragImageList)
  private
  protected
  public
  end;

  { TGnomeWSControl }

  TGnomeWSControl = class(TWSControl)
  private
  protected
  public
  end;

  { TGnomeWSWinControl }

  TGnomeWSWinControl = class(TWSWinControl)
  private
  protected
  public
  end;

  { TGnomeWSGraphicControl }

  TGnomeWSGraphicControl = class(TWSGraphicControl)
  private
  protected
  public
  end;

  { TGnomeWSCustomControl }

  TGnomeWSCustomControl = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TGnomeWSImageList }

  TGnomeWSImageList = class(TWSImageList)
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
//  RegisterWSComponent(TDragImageList, TGnomeWSDragImageList);
//  RegisterWSComponent(TControl, TGnomeWSControl);
//  RegisterWSComponent(TWinControl, TGnomeWSWinControl);
//  RegisterWSComponent(TGraphicControl, TGnomeWSGraphicControl);
//  RegisterWSComponent(TCustomControl, TGnomeWSCustomControl);
//  RegisterWSComponent(TImageList, TGnomeWSImageList);
////////////////////////////////////////////////////
end.
