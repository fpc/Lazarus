{ $Id$}
{
 *****************************************************************************
 *                            GnomeWSStdCtrls.pp                             * 
 *                            ------------------                             * 
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
unit GnomeWSStdCtrls;

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

  { TGnomeWSScrollBar }

  TGnomeWSScrollBar = class(TWSScrollBar)
  private
  protected
  public
  end;

  { TGnomeWSCustomGroupBox }

  TGnomeWSCustomGroupBox = class(TWSCustomGroupBox)
  private
  protected
  public
  end;

  { TGnomeWSGroupBox }

  TGnomeWSGroupBox = class(TWSGroupBox)
  private
  protected
  public
  end;

  { TGnomeWSCustomComboBox }

  TGnomeWSCustomComboBox = class(TWSCustomComboBox)
  private
  protected
  public
  end;

  { TGnomeWSComboBox }

  TGnomeWSComboBox = class(TWSComboBox)
  private
  protected
  public
  end;

  { TGnomeWSCustomListBox }

  TGnomeWSCustomListBox = class(TWSCustomListBox)
  private
  protected
  public
  end;

  { TGnomeWSListBox }

  TGnomeWSListBox = class(TWSListBox)
  private
  protected
  public
  end;

  { TGnomeWSCustomEdit }

  TGnomeWSCustomEdit = class(TWSCustomEdit)
  private
  protected
  public
  end;

  { TGnomeWSCustomMemo }

  TGnomeWSCustomMemo = class(TWSCustomMemo)
  private
  protected
  public
  end;

  { TGnomeWSEdit }

  TGnomeWSEdit = class(TWSEdit)
  private
  protected
  public
  end;

  { TGnomeWSMemo }

  TGnomeWSMemo = class(TWSMemo)
  private
  protected
  public
  end;

  { TGnomeWSCustomLabel }

  TGnomeWSCustomLabel = class(TWSCustomLabel)
  private
  protected
  public
  end;

  { TGnomeWSLabel }

  TGnomeWSLabel = class(TWSLabel)
  private
  protected
  public
  end;

  { TGnomeWSButtonControl }

  TGnomeWSButtonControl = class(TWSButtonControl)
  private
  protected
  public
  end;

  { TGnomeWSCustomCheckBox }

  TGnomeWSCustomCheckBox = class(TWSCustomCheckBox)
  private
  protected
  public
  end;

  { TGnomeWSCheckBox }

  TGnomeWSCheckBox = class(TWSCheckBox)
  private
  protected
  public
  end;

  { TGnomeWSCheckBox }

  TGnomeWSCheckBox = class(TWSCheckBox)
  private
  protected
  public
  end;

  { TGnomeWSToggleBox }

  TGnomeWSToggleBox = class(TWSToggleBox)
  private
  protected
  public
  end;

  { TGnomeWSRadioButton }

  TGnomeWSRadioButton = class(TWSRadioButton)
  private
  protected
  public
  end;

  { TGnomeWSCustomStaticText }

  TGnomeWSCustomStaticText = class(TWSCustomStaticText)
  private
  protected
  public
  end;

  { TGnomeWSStaticText }

  TGnomeWSStaticText = class(TWSStaticText)
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
//  RegisterWSComponent(TScrollBar, TGnomeWSScrollBar);
//  RegisterWSComponent(TCustomGroupBox, TGnomeWSCustomGroupBox);
//  RegisterWSComponent(TGroupBox, TGnomeWSGroupBox);
//  RegisterWSComponent(TCustomComboBox, TGnomeWSCustomComboBox);
//  RegisterWSComponent(TComboBox, TGnomeWSComboBox);
//  RegisterWSComponent(TCustomListBox, TGnomeWSCustomListBox);
//  RegisterWSComponent(TListBox, TGnomeWSListBox);
//  RegisterWSComponent(TCustomEdit, TGnomeWSCustomEdit);
//  RegisterWSComponent(TCustomMemo, TGnomeWSCustomMemo);
//  RegisterWSComponent(TEdit, TGnomeWSEdit);
//  RegisterWSComponent(TMemo, TGnomeWSMemo);
//  RegisterWSComponent(TCustomLabel, TGnomeWSCustomLabel);
//  RegisterWSComponent(TLabel, TGnomeWSLabel);
//  RegisterWSComponent(TButtonControl, TGnomeWSButtonControl);
//  RegisterWSComponent(TCustomCheckBox, TGnomeWSCustomCheckBox);
//  RegisterWSComponent(TCheckBox, TGnomeWSCheckBox);
//  RegisterWSComponent(TCheckBox, TGnomeWSCheckBox);
//  RegisterWSComponent(TToggleBox, TGnomeWSToggleBox);
//  RegisterWSComponent(TRadioButton, TGnomeWSRadioButton);
//  RegisterWSComponent(TCustomStaticText, TGnomeWSCustomStaticText);
//  RegisterWSComponent(TStaticText, TGnomeWSStaticText);
////////////////////////////////////////////////////
end.