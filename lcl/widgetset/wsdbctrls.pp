{ $Id$}
{
 *****************************************************************************
 *                               WSDbCtrls.pp                                * 
 *                               ------------                                * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit WSDbCtrls;

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
//  DbCtrls,
////////////////////////////////////////////////////
  WSLCLClasses, WSMaskEdit, WSStdCtrls, WSExtCtrls,
  WSCalendar, WSButtons;

type
  { TWSDBEdit }

  TWSDBEdit = class(TWSCustomMaskEdit)
  end;

  { TWSDBListBox }

  TWSDBListBox = class(TWSCustomListBox)
  end;

  { TWSDBRadioGroup }

  TWSDBRadioGroup = class(TWSCustomRadioGroup)
  end;

  { TWSDBCheckBox }

  TWSDBCheckBox = class(TWSCustomCheckBox)
  end;

  { TWSDBComboBox }

  TWSDBComboBox = class(TWSCustomComboBox)
  end;

  { TWSDBMemo }

  TWSDBMemo = class(TWSCustomMemo)
  end;

  { TWSDBGroupBox }

  TWSDBGroupBox = class(TWSCustomGroupBox)
  end;

  { TWSDBImage }

  TWSDBImage = class(TWSCustomImage)
  end;

  { TWSDBCalendar }

  TWSDBCalendar = class(TWSCustomCalendar)
  end;

  { TWSDBCustomNavigator }

  TWSDBCustomNavigator = class(TWSCustomPanel)
  end;

  { TWSDBNavButton }

  TWSDBNavButton = class(TWSSpeedButton)
  end;

  { TWSDBNavigator }

  TWSDBNavigator = class(TWSDBCustomNavigator)
  end;


implementation

initialization

////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TDBEdit, TWSDBEdit);
//  RegisterWSComponent(TDBListBox, TWSDBListBox);
//  RegisterWSComponent(TDBRadioGroup, TWSDBRadioGroup);
//  RegisterWSComponent(TDBCheckBox, TWSDBCheckBox);
//  RegisterWSComponent(TDBComboBox, TWSDBComboBox);
//  RegisterWSComponent(TDBMemo, TWSDBMemo);
//  RegisterWSComponent(TDBGroupBox, TWSDBGroupBox);
//  RegisterWSComponent(TDBImage, TWSDBImage);
//  RegisterWSComponent(TDBCalendar, TWSDBCalendar);
//  RegisterWSComponent(TDBCustomNavigator, TWSDBCustomNavigator);
//  RegisterWSComponent(TDBNavButton, TWSDBNavButton);
//  RegisterWSComponent(TDBNavigator, TWSDBNavigator);
////////////////////////////////////////////////////
end.