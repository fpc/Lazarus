{ $Id: wscontrols.pp 7963 2005-10-12 22:22:02Z marc $}
{
 *****************************************************************************
 *                               WSControls.pp                               * 
 *                               -------------                               * 
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
unit WinCEWSControls;

{$mode objfpc}{$H+}

interface
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as posible circles, the uses
//    clause should contain only those LCL units 
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the 
//    initialization section which actually 
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
  Classes,
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Controls, Graphics, LCLType,
////////////////////////////////////////////////////
  WSLCLClasses, WSImgList, WSControls,
  { TODO: remove when CreateHandle/Component code moved }
  InterfaceBase;

type
  { TWinCEWSDragImageList }

  TWinCEWSDragImageList = class(TWSDragImageList)
  private
  protected
  public
  end;

  { TWinCEWSControl }

  TWinCEWSControl = class(TWSControl)
  private
  protected
  public
  end;

  { TWinCEWSWinControlPrivate }

  TWinCEWSWinControlPrivate = class(TWSPrivate)
  private
  protected
  public
  end;
  TWinCEWSWinControlPrivateClass = class of TWinCEWSWinControlPrivate;


  { TWinCEWSWinControl }

  TWinCEWSWinControl = class(TWSWinControl)
  private
  protected
  public
  end;

  { TWinCEWSGraphicControl }

  TWinCEWSGraphicControl = class(TWSGraphicControl)
  private
  protected
  public
  end;

  { TWinCEWSCustomControl }

  TWinCEWSCustomControl = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TWinCEWSImageList }

  TWinCEWSImageList = class(TWSImageList)
  private
  protected
  public
  end;


implementation


initialization

////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TDragImageList, TWSDragImageList);
//  RegisterWSComponent(TControl, TWSControl);
//  RegisterWSComponent(TWinControl, TWSWinControl);
//  RegisterWSComponent(TGraphicControl, TWSGraphicControl);
//  RegisterWSComponent(TCustomControl, TWSCustomControl);
//  RegisterWSComponent(TImageList, TWSImageList);
////////////////////////////////////////////////////
end.
