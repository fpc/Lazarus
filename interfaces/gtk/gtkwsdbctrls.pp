{ $Id$}
{
 *****************************************************************************
 *                              GtkWSDbCtrls.pp                              * 
 *                              ---------------                              * 
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
unit GtkWSDbCtrls;

{$mode objfpc}{$H+}

interface

uses
  DbCtrls, WSDbCtrls, WSLCLClasses;

type

  { TGtkWSDBEdit }

  TGtkWSDBEdit = class(TWSDBEdit)
  published
  end;

  { TGtkWSDBText }

  TGtkWSDBText = class(TWSDBText)
  published
  end;

  { TGtkWSDBListBox }

  TGtkWSDBListBox = class(TWSDBListBox)
  published
  end;

  { TGtkWSDBRadioGroup }

  TGtkWSDBRadioGroup = class(TWSDBRadioGroup)
  published
  end;

  { TGtkWSDBCheckBox }

  TGtkWSDBCheckBox = class(TWSDBCheckBox)
  published
  end;

  { TGtkWSDBComboBox }

  TGtkWSDBComboBox = class(TWSDBComboBox)
  published
  end;

  { TGtkWSDBMemo }

  TGtkWSDBMemo = class(TWSDBMemo)
  published
  end;

  { TGtkWSDBGroupBox }

  TGtkWSDBGroupBox = class(TWSDBGroupBox)
  published
  end;

  { TGtkWSDBImage }

  TGtkWSDBImage = class(TWSDBImage)
  published
  end;

  { TGtkWSDBCalendar }

  TGtkWSDBCalendar = class(TWSDBCalendar)
  published
  end;

  { TGtkWSDBCustomNavigator }

  TGtkWSDBCustomNavigator = class(TWSDBCustomNavigator)
  published
  end;

  { TGtkWSDBNavButton }

  TGtkWSDBNavButton = class(TWSDBNavButton)
  published
  end;

  { TGtkWSDBNavigator }

  TGtkWSDBNavigator = class(TWSDBNavigator)
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
//  RegisterWSComponent(TDBEdit, TGtkWSDBEdit);
//  RegisterWSComponent(TDBText, TGtkWSDBText);
//  RegisterWSComponent(TDBListBox, TGtkWSDBListBox);
//  RegisterWSComponent(TDBRadioGroup, TGtkWSDBRadioGroup);
//  RegisterWSComponent(TDBCheckBox, TGtkWSDBCheckBox);
//  RegisterWSComponent(TDBComboBox, TGtkWSDBComboBox);
//  RegisterWSComponent(TDBMemo, TGtkWSDBMemo);
//  RegisterWSComponent(TDBGroupBox, TGtkWSDBGroupBox);
//  RegisterWSComponent(TDBImage, TGtkWSDBImage);
//  RegisterWSComponent(TDBCalendar, TGtkWSDBCalendar);
//  RegisterWSComponent(TDBCustomNavigator, TGtkWSDBCustomNavigator);
//  RegisterWSComponent(TDBNavButton, TGtkWSDBNavButton);
//  RegisterWSComponent(TDBNavigator, TGtkWSDBNavigator);
////////////////////////////////////////////////////
end.