{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSControls.pp                             * 
 *                             -----------------                             * 
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
unit Gtk2WSControls;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  Controls,
////////////////////////////////////////////////////
  WSControls, WSLCLClasses;

type

  { TGtk2WSDragImageList }

  TGtk2WSDragImageList = class(TWSDragImageList)
  private
  protected
  public
  end;

  { TGtk2WSControl }

  TGtk2WSControl = class(TWSControl)
  private
  protected
  public
  end;

  { TGtk2WSWinControl }

  TGtk2WSWinControl = class(TWSWinControl)
  private
  protected
  public
  end;

  { TGtk2WSGraphicControl }

  TGtk2WSGraphicControl = class(TWSGraphicControl)
  private
  protected
  public
  end;

  { TGtk2WSCustomControl }

  TGtk2WSCustomControl = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TGtk2WSImageList }

  TGtk2WSImageList = class(TWSImageList)
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
//  RegisterWSComponent(TDragImageList, TGtk2WSDragImageList);
//  RegisterWSComponent(TControl, TGtk2WSControl);
//  RegisterWSComponent(TWinControl, TGtk2WSWinControl);
//  RegisterWSComponent(TGraphicControl, TGtk2WSGraphicControl);
//  RegisterWSComponent(TCustomControl, TGtk2WSCustomControl);
//  RegisterWSComponent(TImageList, TGtk2WSImageList);
////////////////////////////////////////////////////
end.
