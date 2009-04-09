{ $Id: FpGuiwsdbctrls.pp 5319 2004-03-17 20:11:29Z marc $}
{
 *****************************************************************************
 *                              FpGuiWSDbCtrls.pp                               * 
 *                              --------------                               * 
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
unit FpGuiWSDbCtrls;

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

  { TFpGuiWSDBEdit }

  TFpGuiWSDBEdit = class(TWSDBEdit)
  private
  protected
  public
  end;

  { TFpGuiWSDBText }

  TFpGuiWSDBText = class(TWSDBText)
  private
  protected
  public
  end;

  { TFpGuiWSDBListBox }

  TFpGuiWSDBListBox = class(TWSDBListBox)
  private
  protected
  public
  end;

  { TFpGuiWSDBRadioGroup }

  TFpGuiWSDBRadioGroup = class(TWSDBRadioGroup)
  private
  protected
  public
  end;

  { TFpGuiWSDBCheckBox }

  TFpGuiWSDBCheckBox = class(TWSDBCheckBox)
  private
  protected
  public
  end;

  { TFpGuiWSDBComboBox }

  TFpGuiWSDBComboBox = class(TWSDBComboBox)
  private
  protected
  public
  end;

  { TFpGuiWSDBMemo }

  TFpGuiWSDBMemo = class(TWSDBMemo)
  private
  protected
  public
  end;

  { TFpGuiWSDBGroupBox }

  TFpGuiWSDBGroupBox = class(TWSDBGroupBox)
  private
  protected
  public
  end;

  { TFpGuiWSDBImage }

  TFpGuiWSDBImage = class(TWSDBImage)
  private
  protected
  public
  end;

  { TFpGuiWSDBCalendar }

  TFpGuiWSDBCalendar = class(TWSDBCalendar)
  private
  protected
  public
  end;

  { TFpGuiWSDBCustomNavigator }

  TFpGuiWSDBCustomNavigator = class(TWSDBCustomNavigator)
  private
  protected
  public
  end;

  { TFpGuiWSDBNavButton }

  TFpGuiWSDBNavButton = class(TWSDBNavButton)
  private
  protected
  public
  end;

  { TFpGuiWSDBNavigator }

  TFpGuiWSDBNavigator = class(TWSDBNavigator)
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
//  RegisterWSComponent(TDBEdit, TFpGuiWSDBEdit);
//  RegisterWSComponent(TDBText, TFpGuiWSDBText);
//  RegisterWSComponent(TDBListBox, TFpGuiWSDBListBox);
//  RegisterWSComponent(TDBRadioGroup, TFpGuiWSDBRadioGroup);
//  RegisterWSComponent(TDBCheckBox, TFpGuiWSDBCheckBox);
//  RegisterWSComponent(TDBComboBox, TFpGuiWSDBComboBox);
//  RegisterWSComponent(TDBMemo, TFpGuiWSDBMemo);
//  RegisterWSComponent(TDBGroupBox, TFpGuiWSDBGroupBox);
//  RegisterWSComponent(TDBImage, TFpGuiWSDBImage);
//  RegisterWSComponent(TDBCalendar, TFpGuiWSDBCalendar);
//  RegisterWSComponent(TDBCustomNavigator, TFpGuiWSDBCustomNavigator);
//  RegisterWSComponent(TDBNavButton, TFpGuiWSDBNavButton);
//  RegisterWSComponent(TDBNavigator, TFpGuiWSDBNavigator);
////////////////////////////////////////////////////
end.
