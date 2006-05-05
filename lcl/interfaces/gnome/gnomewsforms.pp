{ $Id$}
{
 *****************************************************************************
 *                              GnomeWSForms.pp                              * 
 *                              ---------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit GnomeWSForms;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  Forms,
////////////////////////////////////////////////////
  WSForms, WSLCLClasses;

type

  { TGnomeWSScrollingWinControl }

  TGnomeWSScrollingWinControl = class(TWSScrollingWinControl)
  private
  protected
  public
  end;

  { TGnomeWSScrollBox }

  TGnomeWSScrollBox = class(TWSScrollBox)
  private
  protected
  public
  end;

  { TGnomeWSCustomFrame }

  TGnomeWSCustomFrame = class(TWSCustomFrame)
  private
  protected
  public
  end;

  { TGnomeWSFrame }

  TGnomeWSFrame = class(TWSFrame)
  private
  protected
  public
  end;

  { TGnomeWSCustomForm }

  TGnomeWSCustomForm = class(TWSCustomForm)
  private
  protected
  public
  end;

  { TGnomeWSForm }

  TGnomeWSForm = class(TWSForm)
  private
  protected
  public
  end;

  { TGnomeWSHintWindow }

  TGnomeWSHintWindow = class(TWSHintWindow)
  private
  protected
  public
  end;

  { TGnomeWSScreen }

  TGnomeWSScreen = class(TWSScreen)
  private
  protected
  public
  end;

  { TGnomeWSApplicationProperties }

  TGnomeWSApplicationProperties = class(TWSApplicationProperties)
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
//  RegisterWSComponent(TScrollingWinControl, TGnomeWSScrollingWinControl);
//  RegisterWSComponent(TScrollBox, TGnomeWSScrollBox);
//  RegisterWSComponent(TCustomFrame, TGnomeWSCustomFrame);
//  RegisterWSComponent(TFrame, TGnomeWSFrame);
//  RegisterWSComponent(TCustomForm, TGnomeWSCustomForm);
//  RegisterWSComponent(TForm, TGnomeWSForm);
//  RegisterWSComponent(THintWindow, TGnomeWSHintWindow);
//  RegisterWSComponent(TScreen, TGnomeWSScreen);
//  RegisterWSComponent(TApplicationProperties, TGnomeWSApplicationProperties);
////////////////////////////////////////////////////
end.