{ $Id$}
{
 *****************************************************************************
 *                              CarbonWSDbCtrls.pp                               * 
 *                              --------------                               * 
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
  private
  protected
  public
  end;

  { TCarbonWSDBText }

  TCarbonWSDBText = class(TWSDBText)
  private
  protected
  public
  end;

  { TCarbonWSDBListBox }

  TCarbonWSDBListBox = class(TWSDBListBox)
  private
  protected
  public
  end;

  { TCarbonWSDBRadioGroup }

  TCarbonWSDBRadioGroup = class(TWSDBRadioGroup)
  private
  protected
  public
  end;

  { TCarbonWSDBCheckBox }

  TCarbonWSDBCheckBox = class(TWSDBCheckBox)
  private
  protected
  public
  end;

  { TCarbonWSDBComboBox }

  TCarbonWSDBComboBox = class(TWSDBComboBox)
  private
  protected
  public
  end;

  { TCarbonWSDBMemo }

  TCarbonWSDBMemo = class(TWSDBMemo)
  private
  protected
  public
  end;

  { TCarbonWSDBGroupBox }

  TCarbonWSDBGroupBox = class(TWSDBGroupBox)
  private
  protected
  public
  end;

  { TCarbonWSDBImage }

  TCarbonWSDBImage = class(TWSDBImage)
  private
  protected
  public
  end;

  { TCarbonWSDBCalendar }

  TCarbonWSDBCalendar = class(TWSDBCalendar)
  private
  protected
  public
  end;

  { TCarbonWSDBCustomNavigator }

  TCarbonWSDBCustomNavigator = class(TWSDBCustomNavigator)
  private
  protected
  public
  end;

  { TCarbonWSDBNavButton }

  TCarbonWSDBNavButton = class(TWSDBNavButton)
  private
  protected
  public
  end;

  { TCarbonWSDBNavigator }

  TCarbonWSDBNavigator = class(TWSDBNavigator)
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