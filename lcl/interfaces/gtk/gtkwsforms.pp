{ $Id$}
{
 *****************************************************************************
 *                               GtkWSForms.pp                               * 
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
unit GtkWSForms;

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

  { TGtkWSScrollingWinControl }

  TGtkWSScrollingWinControl = class(TWSScrollingWinControl)
  private
  protected
  public
  end;

  { TGtkWSScrollBox }

  TGtkWSScrollBox = class(TWSScrollBox)
  private
  protected
  public
  end;

  { TGtkWSCustomFrame }

  TGtkWSCustomFrame = class(TWSCustomFrame)
  private
  protected
  public
  end;

  { TGtkWSFrame }

  TGtkWSFrame = class(TWSFrame)
  private
  protected
  public
  end;

  { TGtkWSCustomForm }

  TGtkWSCustomForm = class(TWSCustomForm)
  private
  protected
  public
  end;

  { TGtkWSForm }

  TGtkWSForm = class(TWSForm)
  private
  protected
  public
  end;

  { TGtkWSHintWindow }

  TGtkWSHintWindow = class(TWSHintWindow)
  private
  protected
  public
  end;

  { TGtkWSScreen }

  TGtkWSScreen = class(TWSScreen)
  private
  protected
  public
  end;

  { TGtkWSApplicationProperties }

  TGtkWSApplicationProperties = class(TWSApplicationProperties)
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
//  RegisterWSComponent(TScrollingWinControl, TGtkWSScrollingWinControl);
//  RegisterWSComponent(TScrollBox, TGtkWSScrollBox);
//  RegisterWSComponent(TCustomFrame, TGtkWSCustomFrame);
//  RegisterWSComponent(TFrame, TGtkWSFrame);
//  RegisterWSComponent(TCustomForm, TGtkWSCustomForm);
//  RegisterWSComponent(TForm, TGtkWSForm);
//  RegisterWSComponent(THintWindow, TGtkWSHintWindow);
//  RegisterWSComponent(TScreen, TGtkWSScreen);
//  RegisterWSComponent(TApplicationProperties, TGtkWSApplicationProperties);
////////////////////////////////////////////////////
end.
