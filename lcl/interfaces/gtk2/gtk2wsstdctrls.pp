{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSStdCtrls.pp                             * 
 *                             -----------------                             * 
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
unit Gtk2WSStdCtrls;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  StdCtrls,
////////////////////////////////////////////////////
  WSStdCtrls, WSLCLClasses;

type

  { TGtk2WSScrollBar }

  TGtk2WSScrollBar = class(TWSScrollBar)
  private
  protected
  public
  end;

  { TGtk2WSCustomGroupBox }

  TGtk2WSCustomGroupBox = class(TWSCustomGroupBox)
  private
  protected
  public
  end;

  { TGtk2WSGroupBox }

  TGtk2WSGroupBox = class(TWSGroupBox)
  private
  protected
  public
  end;

  { TGtk2WSCustomComboBox }

  TGtk2WSCustomComboBox = class(TWSCustomComboBox)
  private
  protected
  public
  end;

  { TGtk2WSComboBox }

  TGtk2WSComboBox = class(TWSComboBox)
  private
  protected
  public
  end;

  { TGtk2WSCustomListBox }

  TGtk2WSCustomListBox = class(TWSCustomListBox)
  private
  protected
  public
  end;

  { TGtk2WSListBox }

  TGtk2WSListBox = class(TWSListBox)
  private
  protected
  public
  end;

  { TGtk2WSCustomEdit }

  TGtk2WSCustomEdit = class(TWSCustomEdit)
  private
  protected
  public
  end;

  { TGtk2WSCustomMemo }

  TGtk2WSCustomMemo = class(TWSCustomMemo)
  private
  protected
  public
  end;

  { TGtk2WSEdit }

  TGtk2WSEdit = class(TWSEdit)
  private
  protected
  public
  end;

  { TGtk2WSMemo }

  TGtk2WSMemo = class(TWSMemo)
  private
  protected
  public
  end;

  { TGtk2WSCustomLabel }

  TGtk2WSCustomLabel = class(TWSCustomLabel)
  private
  protected
  public
  end;

  { TGtk2WSLabel }

  TGtk2WSLabel = class(TWSLabel)
  private
  protected
  public
  end;

  { TGtk2WSButtonControl }

  TGtk2WSButtonControl = class(TWSButtonControl)
  private
  protected
  public
  end;

  { TGtk2WSCustomCheckBox }

  TGtk2WSCustomCheckBox = class(TWSCustomCheckBox)
  private
  protected
  public
  end;

  { TGtk2WSCheckBox }

  TGtk2WSCheckBox = class(TWSCheckBox)
  private
  protected
  public
  end;

  { TGtk2WSCheckBox }

  TGtk2WSCheckBox = class(TWSCheckBox)
  private
  protected
  public
  end;

  { TGtk2WSToggleBox }

  TGtk2WSToggleBox = class(TWSToggleBox)
  private
  protected
  public
  end;

  { TGtk2WSRadioButton }

  TGtk2WSRadioButton = class(TWSRadioButton)
  private
  protected
  public
  end;

  { TGtk2WSCustomStaticText }

  TGtk2WSCustomStaticText = class(TWSCustomStaticText)
  private
  protected
  public
  end;

  { TGtk2WSStaticText }

  TGtk2WSStaticText = class(TWSStaticText)
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
//  RegisterWSComponent(TScrollBar, TGtk2WSScrollBar);
//  RegisterWSComponent(TCustomGroupBox, TGtk2WSCustomGroupBox);
//  RegisterWSComponent(TGroupBox, TGtk2WSGroupBox);
//  RegisterWSComponent(TCustomComboBox, TGtk2WSCustomComboBox);
//  RegisterWSComponent(TComboBox, TGtk2WSComboBox);
//  RegisterWSComponent(TCustomListBox, TGtk2WSCustomListBox);
//  RegisterWSComponent(TListBox, TGtk2WSListBox);
//  RegisterWSComponent(TCustomEdit, TGtk2WSCustomEdit);
//  RegisterWSComponent(TCustomMemo, TGtk2WSCustomMemo);
//  RegisterWSComponent(TEdit, TGtk2WSEdit);
//  RegisterWSComponent(TMemo, TGtk2WSMemo);
//  RegisterWSComponent(TCustomLabel, TGtk2WSCustomLabel);
//  RegisterWSComponent(TLabel, TGtk2WSLabel);
//  RegisterWSComponent(TButtonControl, TGtk2WSButtonControl);
//  RegisterWSComponent(TCustomCheckBox, TGtk2WSCustomCheckBox);
//  RegisterWSComponent(TCheckBox, TGtk2WSCheckBox);
//  RegisterWSComponent(TCheckBox, TGtk2WSCheckBox);
//  RegisterWSComponent(TToggleBox, TGtk2WSToggleBox);
//  RegisterWSComponent(TRadioButton, TGtk2WSRadioButton);
//  RegisterWSComponent(TCustomStaticText, TGtk2WSCustomStaticText);
//  RegisterWSComponent(TStaticText, TGtk2WSStaticText);
////////////////////////////////////////////////////
end.
