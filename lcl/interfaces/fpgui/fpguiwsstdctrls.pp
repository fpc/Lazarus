{ $Id: FpGuiwsstdctrls.pp 5319 2004-03-17 20:11:29Z marc $}
{
 *****************************************************************************
 *                              FpGuiWSStdCtrls.pp                              * 
 *                              ---------------                              * 
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
unit FpGuiWSStdCtrls;

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

  { TFpGuiWSScrollBar }

  TFpGuiWSScrollBar = class(TWSScrollBar)
  private
  protected
  public
  end;

  { TFpGuiWSCustomGroupBox }

  TFpGuiWSCustomGroupBox = class(TWSCustomGroupBox)
  private
  protected
  public
  end;

  { TFpGuiWSGroupBox }

  TFpGuiWSGroupBox = class(TWSGroupBox)
  private
  protected
  public
  end;

  { TFpGuiWSCustomComboBox }

  TFpGuiWSCustomComboBox = class(TWSCustomComboBox)
  private
  protected
  public
  end;

  { TFpGuiWSComboBox }

  TFpGuiWSComboBox = class(TWSComboBox)
  private
  protected
  public
  end;

  { TFpGuiWSCustomListBox }

  TFpGuiWSCustomListBox = class(TWSCustomListBox)
  private
  protected
  public
  end;

  { TFpGuiWSListBox }

  TFpGuiWSListBox = class(TWSListBox)
  private
  protected
  public
  end;

  { TFpGuiWSCustomEdit }

  TFpGuiWSCustomEdit = class(TWSCustomEdit)
  private
  protected
  public
  end;

  { TFpGuiWSCustomMemo }

  TFpGuiWSCustomMemo = class(TWSCustomMemo)
  private
  protected
  public
  end;

  { TFpGuiWSEdit }

  TFpGuiWSEdit = class(TWSEdit)
  private
  protected
  public
  end;

  { TFpGuiWSMemo }

  TFpGuiWSMemo = class(TWSMemo)
  private
  protected
  public
  end;

  { TFpGuiWSCustomLabel }

  TFpGuiWSCustomLabel = class(TWSCustomLabel)
  private
  protected
  public
  end;

  { TFpGuiWSLabel }

  TFpGuiWSLabel = class(TWSLabel)
  private
  protected
  public
  end;

  { TFpGuiWSButtonControl }

  TFpGuiWSButtonControl = class(TWSButtonControl)
  private
  protected
  public
  end;

  { TFpGuiWSCustomCheckBox }

  TFpGuiWSCustomCheckBox = class(TWSCustomCheckBox)
  private
  protected
  public
  end;

  { TFpGuiWSCheckBox }

  TFpGuiWSCheckBox = class(TWSCheckBox)
  private
  protected
  public
  end;

  { TFpGuiWSCheckBox }

  TFpGuiWSCheckBox = class(TWSCheckBox)
  private
  protected
  public
  end;

  { TFpGuiWSToggleBox }

  TFpGuiWSToggleBox = class(TWSToggleBox)
  private
  protected
  public
  end;

  { TFpGuiWSRadioButton }

  TFpGuiWSRadioButton = class(TWSRadioButton)
  private
  protected
  public
  end;

  { TFpGuiWSCustomStaticText }

  TFpGuiWSCustomStaticText = class(TWSCustomStaticText)
  private
  protected
  public
  end;

  { TFpGuiWSStaticText }

  TFpGuiWSStaticText = class(TWSStaticText)
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
//  RegisterWSComponent(TScrollBar, TFpGuiWSScrollBar);
//  RegisterWSComponent(TCustomGroupBox, TFpGuiWSCustomGroupBox);
//  RegisterWSComponent(TGroupBox, TFpGuiWSGroupBox);
//  RegisterWSComponent(TCustomComboBox, TFpGuiWSCustomComboBox);
//  RegisterWSComponent(TComboBox, TFpGuiWSComboBox);
//  RegisterWSComponent(TCustomListBox, TFpGuiWSCustomListBox);
//  RegisterWSComponent(TListBox, TFpGuiWSListBox);
//  RegisterWSComponent(TCustomEdit, TFpGuiWSCustomEdit);
//  RegisterWSComponent(TCustomMemo, TFpGuiWSCustomMemo);
//  RegisterWSComponent(TEdit, TFpGuiWSEdit);
//  RegisterWSComponent(TMemo, TFpGuiWSMemo);
//  RegisterWSComponent(TCustomLabel, TFpGuiWSCustomLabel);
//  RegisterWSComponent(TLabel, TFpGuiWSLabel);
//  RegisterWSComponent(TButtonControl, TFpGuiWSButtonControl);
//  RegisterWSComponent(TCustomCheckBox, TFpGuiWSCustomCheckBox);
//  RegisterWSComponent(TCheckBox, TFpGuiWSCheckBox);
//  RegisterWSComponent(TCheckBox, TFpGuiWSCheckBox);
//  RegisterWSComponent(TToggleBox, TFpGuiWSToggleBox);
//  RegisterWSComponent(TRadioButton, TFpGuiWSRadioButton);
//  RegisterWSComponent(TCustomStaticText, TFpGuiWSCustomStaticText);
//  RegisterWSComponent(TStaticText, TFpGuiWSStaticText);
////////////////////////////////////////////////////
end.
