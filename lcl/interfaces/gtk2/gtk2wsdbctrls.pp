{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSDbCtrls.pp                              * 
 *                             ----------------                              * 
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
unit Gtk2WSDbCtrls;

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

  { TGtk2WSDBEdit }

  TGtk2WSDBEdit = class(TWSDBEdit)
  private
  protected
  public
  end;

  { TGtk2WSDBText }

  TGtk2WSDBText = class(TWSDBText)
  private
  protected
  public
  end;

  { TGtk2WSDBListBox }

  TGtk2WSDBListBox = class(TWSDBListBox)
  private
  protected
  public
  end;

  { TGtk2WSDBRadioGroup }

  TGtk2WSDBRadioGroup = class(TWSDBRadioGroup)
  private
  protected
  public
  end;

  { TGtk2WSDBCheckBox }

  TGtk2WSDBCheckBox = class(TWSDBCheckBox)
  private
  protected
  public
  end;

  { TGtk2WSDBComboBox }

  TGtk2WSDBComboBox = class(TWSDBComboBox)
  private
  protected
  public
  end;

  { TGtk2WSDBMemo }

  TGtk2WSDBMemo = class(TWSDBMemo)
  private
  protected
  public
  end;

  { TGtk2WSDBGroupBox }

  TGtk2WSDBGroupBox = class(TWSDBGroupBox)
  private
  protected
  public
  end;

  { TGtk2WSDBImage }

  TGtk2WSDBImage = class(TWSDBImage)
  private
  protected
  public
  end;

  { TGtk2WSDBCalendar }

  TGtk2WSDBCalendar = class(TWSDBCalendar)
  private
  protected
  public
  end;

  { TGtk2WSDBCustomNavigator }

  TGtk2WSDBCustomNavigator = class(TWSDBCustomNavigator)
  private
  protected
  public
  end;

  { TGtk2WSDBNavButton }

  TGtk2WSDBNavButton = class(TWSDBNavButton)
  private
  protected
  public
  end;

  { TGtk2WSDBNavigator }

  TGtk2WSDBNavigator = class(TWSDBNavigator)
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
//  RegisterWSComponent(TDBEdit, TGtk2WSDBEdit);
//  RegisterWSComponent(TDBText, TGtk2WSDBText);
//  RegisterWSComponent(TDBListBox, TGtk2WSDBListBox);
//  RegisterWSComponent(TDBRadioGroup, TGtk2WSDBRadioGroup);
//  RegisterWSComponent(TDBCheckBox, TGtk2WSDBCheckBox);
//  RegisterWSComponent(TDBComboBox, TGtk2WSDBComboBox);
//  RegisterWSComponent(TDBMemo, TGtk2WSDBMemo);
//  RegisterWSComponent(TDBGroupBox, TGtk2WSDBGroupBox);
//  RegisterWSComponent(TDBImage, TGtk2WSDBImage);
//  RegisterWSComponent(TDBCalendar, TGtk2WSDBCalendar);
//  RegisterWSComponent(TDBCustomNavigator, TGtk2WSDBCustomNavigator);
//  RegisterWSComponent(TDBNavButton, TGtk2WSDBNavButton);
//  RegisterWSComponent(TDBNavigator, TGtk2WSDBNavigator);
////////////////////////////////////////////////////
end.