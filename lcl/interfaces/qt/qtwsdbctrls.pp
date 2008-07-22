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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
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
  private
  protected
  public
  end;

  { TQtWSDBText }

  TQtWSDBText = class(TWSDBText)
  private
  protected
  public
  end;

  { TQtWSDBListBox }

  TQtWSDBListBox = class(TWSDBListBox)
  private
  protected
  public
  end;

  { TQtWSDBRadioGroup }

  TQtWSDBRadioGroup = class(TWSDBRadioGroup)
  private
  protected
  public
  end;

  { TQtWSDBCheckBox }

  TQtWSDBCheckBox = class(TWSDBCheckBox)
  private
  protected
  public
  end;

  { TQtWSDBComboBox }

  TQtWSDBComboBox = class(TWSDBComboBox)
  private
  protected
  public
  end;

  { TQtWSDBMemo }

  TQtWSDBMemo = class(TWSDBMemo)
  private
  protected
  public
  end;

  { TQtWSDBGroupBox }

  TQtWSDBGroupBox = class(TWSDBGroupBox)
  private
  protected
  public
  end;

  { TQtWSDBImage }

  TQtWSDBImage = class(TWSDBImage)
  private
  protected
  public
  end;

  { TQtWSDBCalendar }

  TQtWSDBCalendar = class(TWSDBCalendar)
  private
  protected
  public
  end;

  { TQtWSDBCustomNavigator }

  TQtWSDBCustomNavigator = class(TWSDBCustomNavigator)
  private
  protected
  public
  end;

  { TQtWSDBNavButton }

  TQtWSDBNavButton = class(TWSDBNavButton)
  private
  protected
  public
  end;

  { TQtWSDBNavigator }

  TQtWSDBNavigator = class(TWSDBNavigator)
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