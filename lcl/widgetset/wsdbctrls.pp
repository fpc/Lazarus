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
 *  See the file COPYING.LCL, included in this distribution,                 *
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

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
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
  private
  protected
  public
  end;

  { TWSDBText }

  TWSDBText = class(TWSLabel)
  private
  protected
  public
  end;

  { TWSDBListBox }

  TWSDBListBox = class(TWSCustomListBox)
  private
  protected
  public
  end;

  { TWSDBRadioGroup }

  TWSDBRadioGroup = class(TWSCustomRadioGroup)
  private
  protected
  public
  end;

  { TWSDBCheckBox }

  TWSDBCheckBox = class(TWSCustomCheckBox)
  private
  protected
  public
  end;

  { TWSDBComboBox }

  TWSDBComboBox = class(TWSCustomComboBox)
  private
  protected
  public
  end;

  { TWSDBMemo }

  TWSDBMemo = class(TWSCustomMemo)
  private
  protected
  public
  end;

  { TWSDBGroupBox }

  TWSDBGroupBox = class(TWSCustomGroupBox)
  private
  protected
  public
  end;

  { TWSDBImage }

  TWSDBImage = class(TWSCustomImage)
  private
  protected
  public
  end;

  { TWSDBCalendar }

  TWSDBCalendar = class(TWSCalendar)
  private
  protected
  public
  end;

  { TWSDBCustomNavigator }

  TWSDBCustomNavigator = class(TWSCustomPanel)
  private
  protected
  public
  end;

  { TWSDBNavButton }

  TWSDBNavButton = class(TWSSpeedButton)
  private
  protected
  public
  end;

  { TWSDBNavigator }

  TWSDBNavigator = class(TWSDBCustomNavigator)
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
//  RegisterWSComponent(TDBEdit, TWSDBEdit);
//  RegisterWSComponent(TDBText, TWSDBText);
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
