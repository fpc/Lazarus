{ $Id: FpGuiwscontrols.pp 5319 2004-03-17 20:11:29Z marc $}
{
 *****************************************************************************
 *                              FpGuiWSControls.pp                              * 
 *                              ---------------                              * 
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
unit FpGuiWSControls;

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

  { TFpGuiWSDragImageList }

  TFpGuiWSDragImageList = class(TWSDragImageList)
  private
  protected
  public
  end;

  { TFpGuiWSControl }

  TFpGuiWSControl = class(TWSControl)
  private
  protected
  public
  end;

  { TFpGuiWSWinControl }

  TFpGuiWSWinControl = class(TWSWinControl)
  private
  protected
  public
  end;

  { TFpGuiWSGraphicControl }

  TFpGuiWSGraphicControl = class(TWSGraphicControl)
  private
  protected
  public
  end;

  { TFpGuiWSCustomControl }

  TFpGuiWSCustomControl = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TFpGuiWSImageList }

  TFpGuiWSImageList = class(TWSImageList)
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
//  RegisterWSComponent(TDragImageList, TFpGuiWSDragImageList);
//  RegisterWSComponent(TControl, TFpGuiWSControl);
//  RegisterWSComponent(TWinControl, TFpGuiWSWinControl);
//  RegisterWSComponent(TGraphicControl, TFpGuiWSGraphicControl);
//  RegisterWSComponent(TCustomControl, TFpGuiWSCustomControl);
//  RegisterWSComponent(TImageList, TFpGuiWSImageList);
////////////////////////////////////////////////////
end.
