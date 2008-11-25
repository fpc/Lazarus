{ $Id$}
{
 *****************************************************************************
 *                              CarbonWSDbCtrls.pp                           *
 *                              --------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CarbonWSDbCtrls;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  DbCtrls,
////////////////////////////////////////////////////
  WSDbCtrls, WSLCLClasses;

type

  { TCarbonWSDBEdit }

  TCarbonWSDBEdit = class(TWSDBEdit)
  published
  end;

  { TCarbonWSDBText }

  TCarbonWSDBText = class(TWSDBText)
  published
  end;

  { TCarbonWSDBListBox }

  TCarbonWSDBListBox = class(TWSDBListBox)
  published
  end;

  { TCarbonWSDBRadioGroup }

  TCarbonWSDBRadioGroup = class(TWSDBRadioGroup)
  published
  end;

  { TCarbonWSDBCheckBox }

  TCarbonWSDBCheckBox = class(TWSDBCheckBox)
  published
  end;

  { TCarbonWSDBComboBox }

  TCarbonWSDBComboBox = class(TWSDBComboBox)
  published
  end;

  { TCarbonWSDBMemo }

  TCarbonWSDBMemo = class(TWSDBMemo)
  published
  end;

  { TCarbonWSDBGroupBox }

  TCarbonWSDBGroupBox = class(TWSDBGroupBox)
  published
  end;

  { TCarbonWSDBImage }

  TCarbonWSDBImage = class(TWSDBImage)
  published
  end;

  { TCarbonWSDBCalendar }

  TCarbonWSDBCalendar = class(TWSDBCalendar)
  published
  end;

  { TCarbonWSDBCustomNavigator }

  TCarbonWSDBCustomNavigator = class(TWSDBCustomNavigator)
  published
  end;

  { TCarbonWSDBNavButton }

  TCarbonWSDBNavButton = class(TWSDBNavButton)
  published
  end;

  { TCarbonWSDBNavigator }

  TCarbonWSDBNavigator = class(TWSDBNavigator)
  published
  end;


implementation

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TDBEdit, TCarbonWSDBEdit);
//  RegisterWSComponent(TDBText, TCarbonWSDBText);
//  RegisterWSComponent(TDBListBox, TCarbonWSDBListBox);
//  RegisterWSComponent(TDBRadioGroup, TCarbonWSDBRadioGroup);
//  RegisterWSComponent(TDBCheckBox, TCarbonWSDBCheckBox);
//  RegisterWSComponent(TDBComboBox, TCarbonWSDBComboBox);
//  RegisterWSComponent(TDBMemo, TCarbonWSDBMemo);
//  RegisterWSComponent(TDBGroupBox, TCarbonWSDBGroupBox);
//  RegisterWSComponent(TDBImage, TCarbonWSDBImage);
//  RegisterWSComponent(TDBCalendar, TCarbonWSDBCalendar);
//  RegisterWSComponent(TDBCustomNavigator, TCarbonWSDBCustomNavigator);
//  RegisterWSComponent(TDBNavButton, TCarbonWSDBNavButton);
//  RegisterWSComponent(TDBNavigator, TCarbonWSDBNavigator);
////////////////////////////////////////////////////
end.
