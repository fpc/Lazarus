{ $Id$}
{
 *****************************************************************************
 *                              win32wsforms.pp                              * 
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
unit win32wsforms;

{$mode objfpc}{H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as litle as posible circles,
// Uncomment only when needed for registration
////////////////////////////////////////////////////
//  forms,
////////////////////////////////////////////////////
  wsforms, wslclclasses;

type

  { TWin32WSScrollingWinControl }

  TWin32WSScrollingWinControl = class(TWSScrollingWinControl)
  private
  protected
  public
  end;

  { TWin32WSScrollBox }

  TWin32WSScrollBox = class(TWSScrollBox)
  private
  protected
  public
  end;

  { TWin32WSCustomFrame }

  TWin32WSCustomFrame = class(TWSCustomFrame)
  private
  protected
  public
  end;

  { TWin32WSFrame }

  TWin32WSFrame = class(TWSFrame)
  private
  protected
  public
  end;

  { TWin32WSCustomForm }

  TWin32WSCustomForm = class(TWSCustomForm)
  private
  protected
  public
  end;

  { TWin32WSForm }

  TWin32WSForm = class(TWSForm)
  private
  protected
  public
  end;

  { TWin32WSHintWindow }

  TWin32WSHintWindow = class(TWSHintWindow)
  private
  protected
  public
  end;

  { TWin32WSScreen }

  TWin32WSScreen = class(TWSScreen)
  private
  protected
  public
  end;

  { TWin32WSApplicationProperties }

  TWin32WSApplicationProperties = class(TWSApplicationProperties)
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
//  RegisterWSComponent(TScrollingWinControl, TWin32WSScrollingWinControl);
//  RegisterWSComponent(TScrollBox, TWin32WSScrollBox);
//  RegisterWSComponent(TCustomFrame, TWin32WSCustomFrame);
//  RegisterWSComponent(TFrame, TWin32WSFrame);
//  RegisterWSComponent(TCustomForm, TWin32WSCustomForm);
//  RegisterWSComponent(TForm, TWin32WSForm);
//  RegisterWSComponent(THintWindow, TWin32WSHintWindow);
//  RegisterWSComponent(TScreen, TWin32WSScreen);
//  RegisterWSComponent(TApplicationProperties, TWin32WSApplicationProperties);
////////////////////////////////////////////////////
end.
