{ $Id$}
{
 *****************************************************************************
 *                             Win32WSDbCtrls.pp                             * 
 *                             -----------------                             * 
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
unit Win32WSDbCtrls;

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

  { TWin32WSDBEdit }

  TWin32WSDBEdit = class(TWSDBEdit)
  published
  end;

  { TWin32WSDBText }

  TWin32WSDBText = class(TWSDBText)
  published
  end;

  { TWin32WSDBListBox }

  TWin32WSDBListBox = class(TWSDBListBox)
  published
  end;

  { TWin32WSDBRadioGroup }

  TWin32WSDBRadioGroup = class(TWSDBRadioGroup)
  published
  end;

  { TWin32WSDBCheckBox }

  TWin32WSDBCheckBox = class(TWSDBCheckBox)
  published
  end;

  { TWin32WSDBComboBox }

  TWin32WSDBComboBox = class(TWSDBComboBox)
  published
  end;

  { TWin32WSDBMemo }

  TWin32WSDBMemo = class(TWSDBMemo)
  published
  end;

  { TWin32WSDBGroupBox }

  TWin32WSDBGroupBox = class(TWSDBGroupBox)
  published
  end;

  { TWin32WSDBImage }

  TWin32WSDBImage = class(TWSDBImage)
  published
  end;

  { TWin32WSDBCalendar }

  TWin32WSDBCalendar = class(TWSDBCalendar)
  published
  end;

  { TWin32WSDBCustomNavigator }

  TWin32WSDBCustomNavigator = class(TWSDBCustomNavigator)
  published
  end;

  { TWin32WSDBNavButton }

  TWin32WSDBNavButton = class(TWSDBNavButton)
  published
  end;

  { TWin32WSDBNavigator }

  TWin32WSDBNavigator = class(TWSDBNavigator)
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
//  RegisterWSComponent(TDBEdit, TWin32WSDBEdit);
//  RegisterWSComponent(TDBText, TWin32WSDBText);
//  RegisterWSComponent(TDBListBox, TWin32WSDBListBox);
//  RegisterWSComponent(TDBRadioGroup, TWin32WSDBRadioGroup);
//  RegisterWSComponent(TDBCheckBox, TWin32WSDBCheckBox);
//  RegisterWSComponent(TDBComboBox, TWin32WSDBComboBox);
//  RegisterWSComponent(TDBMemo, TWin32WSDBMemo);
//  RegisterWSComponent(TDBGroupBox, TWin32WSDBGroupBox);
//  RegisterWSComponent(TDBImage, TWin32WSDBImage);
//  RegisterWSComponent(TDBCalendar, TWin32WSDBCalendar);
//  RegisterWSComponent(TDBCustomNavigator, TWin32WSDBCustomNavigator);
//  RegisterWSComponent(TDBNavButton, TWin32WSDBNavButton);
//  RegisterWSComponent(TDBNavigator, TWin32WSDBNavigator);
////////////////////////////////////////////////////
end.