{ $Id$}
{
 *****************************************************************************
 *                               qtwsforms.pp                                * 
 *                               ------------                                * 
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
unit qtwsforms;

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

  { TQtWSScrollingWinControl }

  TQtWSScrollingWinControl = class(TWSScrollingWinControl)
  private
  protected
  public
  end;

  { TQtWSScrollBox }

  TQtWSScrollBox = class(TWSScrollBox)
  private
  protected
  public
  end;

  { TQtWSCustomFrame }

  TQtWSCustomFrame = class(TWSCustomFrame)
  private
  protected
  public
  end;

  { TQtWSFrame }

  TQtWSFrame = class(TWSFrame)
  private
  protected
  public
  end;

  { TQtWSCustomForm }

  TQtWSCustomForm = class(TWSCustomForm)
  private
  protected
  public
  end;

  { TQtWSForm }

  TQtWSForm = class(TWSForm)
  private
  protected
  public
  end;

  { TQtWSHintWindow }

  TQtWSHintWindow = class(TWSHintWindow)
  private
  protected
  public
  end;

  { TQtWSScreen }

  TQtWSScreen = class(TWSScreen)
  private
  protected
  public
  end;

  { TQtWSApplicationProperties }

  TQtWSApplicationProperties = class(TWSApplicationProperties)
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
//  RegisterWSComponent(TScrollingWinControl, TQtWSScrollingWinControl);
//  RegisterWSComponent(TScrollBox, TQtWSScrollBox);
//  RegisterWSComponent(TCustomFrame, TQtWSCustomFrame);
//  RegisterWSComponent(TFrame, TQtWSFrame);
//  RegisterWSComponent(TCustomForm, TQtWSCustomForm);
//  RegisterWSComponent(TForm, TQtWSForm);
//  RegisterWSComponent(THintWindow, TQtWSHintWindow);
//  RegisterWSComponent(TScreen, TQtWSScreen);
//  RegisterWSComponent(TApplicationProperties, TQtWSApplicationProperties);
////////////////////////////////////////////////////
end.
