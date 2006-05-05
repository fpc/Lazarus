{ $Id$}
{
 *****************************************************************************
 *                             GnomeWSDbCtrls.pp                             * 
 *                             -----------------                             * 
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
unit GnomeWSDbCtrls;

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

  { TGnomeWSDBEdit }

  TGnomeWSDBEdit = class(TWSDBEdit)
  private
  protected
  public
  end;

  { TGnomeWSDBText }

  TGnomeWSDBText = class(TWSDBText)
  private
  protected
  public
  end;

  { TGnomeWSDBListBox }

  TGnomeWSDBListBox = class(TWSDBListBox)
  private
  protected
  public
  end;

  { TGnomeWSDBRadioGroup }

  TGnomeWSDBRadioGroup = class(TWSDBRadioGroup)
  private
  protected
  public
  end;

  { TGnomeWSDBCheckBox }

  TGnomeWSDBCheckBox = class(TWSDBCheckBox)
  private
  protected
  public
  end;

  { TGnomeWSDBComboBox }

  TGnomeWSDBComboBox = class(TWSDBComboBox)
  private
  protected
  public
  end;

  { TGnomeWSDBMemo }

  TGnomeWSDBMemo = class(TWSDBMemo)
  private
  protected
  public
  end;

  { TGnomeWSDBGroupBox }

  TGnomeWSDBGroupBox = class(TWSDBGroupBox)
  private
  protected
  public
  end;

  { TGnomeWSDBImage }

  TGnomeWSDBImage = class(TWSDBImage)
  private
  protected
  public
  end;

  { TGnomeWSDBCalendar }

  TGnomeWSDBCalendar = class(TWSDBCalendar)
  private
  protected
  public
  end;

  { TGnomeWSDBCustomNavigator }

  TGnomeWSDBCustomNavigator = class(TWSDBCustomNavigator)
  private
  protected
  public
  end;

  { TGnomeWSDBNavButton }

  TGnomeWSDBNavButton = class(TWSDBNavButton)
  private
  protected
  public
  end;

  { TGnomeWSDBNavigator }

  TGnomeWSDBNavigator = class(TWSDBNavigator)
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
//  RegisterWSComponent(TDBEdit, TGnomeWSDBEdit);
//  RegisterWSComponent(TDBText, TGnomeWSDBText);
//  RegisterWSComponent(TDBListBox, TGnomeWSDBListBox);
//  RegisterWSComponent(TDBRadioGroup, TGnomeWSDBRadioGroup);
//  RegisterWSComponent(TDBCheckBox, TGnomeWSDBCheckBox);
//  RegisterWSComponent(TDBComboBox, TGnomeWSDBComboBox);
//  RegisterWSComponent(TDBMemo, TGnomeWSDBMemo);
//  RegisterWSComponent(TDBGroupBox, TGnomeWSDBGroupBox);
//  RegisterWSComponent(TDBImage, TGnomeWSDBImage);
//  RegisterWSComponent(TDBCalendar, TGnomeWSDBCalendar);
//  RegisterWSComponent(TDBCustomNavigator, TGnomeWSDBCustomNavigator);
//  RegisterWSComponent(TDBNavButton, TGnomeWSDBNavButton);
//  RegisterWSComponent(TDBNavigator, TGnomeWSDBNavigator);
////////////////////////////////////////////////////
end.