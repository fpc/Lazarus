{ $Id$}
{
 *****************************************************************************
 *                                wsforms.pp                                 * 
 *                                ----------                                 * 
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
unit wsforms;

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
  wslclclasses, wscontrols;

type

  { TWSScrollingWinControl }

  TWSScrollingWinControl = class(TWSWinControl)
  private
  protected
  public
  end;

  { TWSScrollBox }

  TWSScrollBox = class(TWSScrollingWinControl)
  private
  protected
  public
  end;

  { TWSCustomFrame }

  TWSCustomFrame = class(TWSScrollingWinControl)
  private
  protected
  public
  end;

  { TWSFrame }

  TWSFrame = class(TWSCustomFrame)
  private
  protected
  public
  end;

  { TWSCustomForm }

  TWSCustomForm = class(TWSScrollingWinControl)
  private
  protected
  public
  end;

  { TWSForm }

  TWSForm = class(TWSCustomForm)
  private
  protected
  public
  end;

  { TWSHintWindow }

  TWSHintWindow = class(TWSCustomForm)
  private
  protected
  public
  end;

  { TWSScreen }

  TWSScreen = class(TWSLCLComponent)
  private
  protected
  public
  end;

  { TWSApplicationProperties }

  TWSApplicationProperties = class(TWSLCLComponent)
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
//  RegisterWSComponent(TScrollingWinControl, TWSScrollingWinControl);
//  RegisterWSComponent(TScrollBox, TWSScrollBox);
//  RegisterWSComponent(TCustomFrame, TWSCustomFrame);
//  RegisterWSComponent(TFrame, TWSFrame);
//  RegisterWSComponent(TCustomForm, TWSCustomForm);
//  RegisterWSComponent(TForm, TWSForm);
//  RegisterWSComponent(THintWindow, TWSHintWindow);
//  RegisterWSComponent(TScreen, TWSScreen);
//  RegisterWSComponent(TApplicationProperties, TWSApplicationProperties);
////////////////////////////////////////////////////
end.
