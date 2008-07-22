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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
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
  private
  protected
  public
  end;

  { TGtkWSDBText }

  TGtkWSDBText = class(TWSDBText)
  private
  protected
  public
  end;

  { TGtkWSDBListBox }

  TGtkWSDBListBox = class(TWSDBListBox)
  private
  protected
  public
  end;

  { TGtkWSDBRadioGroup }

  TGtkWSDBRadioGroup = class(TWSDBRadioGroup)
  private
  protected
  public
  end;

  { TGtkWSDBCheckBox }

  TGtkWSDBCheckBox = class(TWSDBCheckBox)
  private
  protected
  public
  end;

  { TGtkWSDBComboBox }

  TGtkWSDBComboBox = class(TWSDBComboBox)
  private
  protected
  public
  end;

  { TGtkWSDBMemo }

  TGtkWSDBMemo = class(TWSDBMemo)
  private
  protected
  public
  end;

  { TGtkWSDBGroupBox }

  TGtkWSDBGroupBox = class(TWSDBGroupBox)
  private
  protected
  public
  end;

  { TGtkWSDBImage }

  TGtkWSDBImage = class(TWSDBImage)
  private
  protected
  public
  end;

  { TGtkWSDBCalendar }

  TGtkWSDBCalendar = class(TWSDBCalendar)
  private
  protected
  public
  end;

  { TGtkWSDBCustomNavigator }

  TGtkWSDBCustomNavigator = class(TWSDBCustomNavigator)
  private
  protected
  public
  end;

  { TGtkWSDBNavButton }

  TGtkWSDBNavButton = class(TWSDBNavButton)
  private
  protected
  public
  end;

  { TGtkWSDBNavigator }

  TGtkWSDBNavigator = class(TWSDBNavigator)
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