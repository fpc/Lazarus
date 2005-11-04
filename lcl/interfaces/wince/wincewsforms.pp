{ $Id: wsforms.pp 7361 2005-07-16 00:08:26Z marc $}
{
 *****************************************************************************
 *                                WSForms.pp                                 * 
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
unit WinCEWSForms;

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
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Forms,
////////////////////////////////////////////////////
  WSLCLClasses, WSControls, Controls, LCLType, WSForms;

type
  { TWSScrollingWinControl }

  TWSScrollingWinControlClass = class of TWSScrollingWinControl;
  TWSScrollingWinControl = class(TWSWinControl)
  end;

  { TWSScrollBox }

  TWSScrollBox = class(TWSScrollingWinControl)
  end;

  { TWSCustomFrame }

  TWSCustomFrame = class(TWSScrollingWinControl)
  end;

  { TWSFrame }

  TWSFrame = class(TWSCustomFrame)
  end;

  { TWSCustomForm }

  TWSCustomForm = class(TWSScrollingWinControl)
  end;
  TWSCustomFormClass = class of TWSCustomForm;

  { TWSForm }

  TWSForm = class(TWSCustomForm)
  end;

  { TWSHintWindow }

  TWSHintWindow = class(TWSCustomForm)
  end;

  { TWSScreen }

  TWSScreen = class(TWSLCLComponent)
  end;

  { TWSApplicationProperties }

  TWSApplicationProperties = class(TWSLCLComponent)
  end;


implementation


initialization

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
