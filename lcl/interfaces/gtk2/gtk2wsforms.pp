{ $Id$}
{
 *****************************************************************************
 *                              Gtk2WSForms.pp                               * 
 *                              --------------                               * 
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
unit Gtk2WSForms;

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

  { TGtk2WSScrollingWinControl }

  TGtk2WSScrollingWinControl = class(TWSScrollingWinControl)
  private
  protected
  public
  end;

  { TGtk2WSScrollBox }

  TGtk2WSScrollBox = class(TWSScrollBox)
  private
  protected
  public
  end;

  { TGtk2WSCustomFrame }

  TGtk2WSCustomFrame = class(TWSCustomFrame)
  private
  protected
  public
  end;

  { TGtk2WSFrame }

  TGtk2WSFrame = class(TWSFrame)
  private
  protected
  public
  end;

  { TGtk2WSCustomForm }

  TGtk2WSCustomForm = class(TWSCustomForm)
  private
  protected
  public
  end;

  { TGtk2WSForm }

  TGtk2WSForm = class(TWSForm)
  private
  protected
  public
  end;

  { TGtk2WSHintWindow }

  TGtk2WSHintWindow = class(TWSHintWindow)
  private
  protected
  public
  end;

  { TGtk2WSScreen }

  TGtk2WSScreen = class(TWSScreen)
  private
  protected
  public
  end;

  { TGtk2WSApplicationProperties }

  TGtk2WSApplicationProperties = class(TWSApplicationProperties)
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
//  RegisterWSComponent(TScrollingWinControl, TGtk2WSScrollingWinControl);
//  RegisterWSComponent(TScrollBox, TGtk2WSScrollBox);
//  RegisterWSComponent(TCustomFrame, TGtk2WSCustomFrame);
//  RegisterWSComponent(TFrame, TGtk2WSFrame);
//  RegisterWSComponent(TCustomForm, TGtk2WSCustomForm);
//  RegisterWSComponent(TForm, TGtk2WSForm);
//  RegisterWSComponent(THintWindow, TGtk2WSHintWindow);
//  RegisterWSComponent(TScreen, TGtk2WSScreen);
//  RegisterWSComponent(TApplicationProperties, TGtk2WSApplicationProperties);
////////////////////////////////////////////////////
end.