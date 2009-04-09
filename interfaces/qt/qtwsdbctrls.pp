{ $Id$}
{
 *****************************************************************************
 *                              QtWSDbCtrls.pp                               * 
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
unit QtWSDbCtrls;

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

  { TQtWSDBEdit }

  TQtWSDBEdit = class(TWSDBEdit)
  published
  end;

  { TQtWSDBText }

  TQtWSDBText = class(TWSDBText)
  published
  end;

  { TQtWSDBListBox }

  TQtWSDBListBox = class(TWSDBListBox)
  published
  end;

  { TQtWSDBRadioGroup }

  TQtWSDBRadioGroup = class(TWSDBRadioGroup)
  published
  end;

  { TQtWSDBCheckBox }

  TQtWSDBCheckBox = class(TWSDBCheckBox)
  published
  end;

  { TQtWSDBComboBox }

  TQtWSDBComboBox = class(TWSDBComboBox)
  published
  end;

  { TQtWSDBMemo }

  TQtWSDBMemo = class(TWSDBMemo)
  published
  end;

  { TQtWSDBGroupBox }

  TQtWSDBGroupBox = class(TWSDBGroupBox)
  published
  end;

  { TQtWSDBImage }

  TQtWSDBImage = class(TWSDBImage)
  published
  end;

  { TQtWSDBCalendar }

  TQtWSDBCalendar = class(TWSDBCalendar)
  published
  end;

  { TQtWSDBCustomNavigator }

  TQtWSDBCustomNavigator = class(TWSDBCustomNavigator)
  published
  end;

  { TQtWSDBNavButton }

  TQtWSDBNavButton = class(TWSDBNavButton)
  published
  end;

  { TQtWSDBNavigator }

  TQtWSDBNavigator = class(TWSDBNavigator)
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
//  RegisterWSComponent(TDBEdit, TQtWSDBEdit);
//  RegisterWSComponent(TDBText, TQtWSDBText);
//  RegisterWSComponent(TDBListBox, TQtWSDBListBox);
//  RegisterWSComponent(TDBRadioGroup, TQtWSDBRadioGroup);
//  RegisterWSComponent(TDBCheckBox, TQtWSDBCheckBox);
//  RegisterWSComponent(TDBComboBox, TQtWSDBComboBox);
//  RegisterWSComponent(TDBMemo, TQtWSDBMemo);
//  RegisterWSComponent(TDBGroupBox, TQtWSDBGroupBox);
//  RegisterWSComponent(TDBImage, TQtWSDBImage);
//  RegisterWSComponent(TDBCalendar, TQtWSDBCalendar);
//  RegisterWSComponent(TDBCustomNavigator, TQtWSDBCustomNavigator);
//  RegisterWSComponent(TDBNavButton, TQtWSDBNavButton);
//  RegisterWSComponent(TDBNavigator, TQtWSDBNavigator);
////////////////////////////////////////////////////
end.
