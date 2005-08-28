{ $Id: Carbonwsstdctrls.pp 5319 2004-03-17 20:11:29Z marc $}
{
 *****************************************************************************
 *                              CarbonWSStdCtrls.pp                              * 
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
unit CarbonWSStdCtrls;

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

  { TCarbonWSScrollBar }

  TCarbonWSScrollBar = class(TWSScrollBar)
  private
  protected
  public
  end;

  { TCarbonWSCustomGroupBox }

  TCarbonWSCustomGroupBox = class(TWSCustomGroupBox)
  private
  protected
  public
  end;

  { TCarbonWSGroupBox }

  TCarbonWSGroupBox = class(TWSGroupBox)
  private
  protected
  public
  end;

  { TCarbonWSCustomComboBox }

  TCarbonWSCustomComboBox = class(TWSCustomComboBox)
  private
  protected
  public
  end;

  { TCarbonWSComboBox }

  TCarbonWSComboBox = class(TWSComboBox)
  private
  protected
  public
  end;

  { TCarbonWSCustomListBox }

  TCarbonWSCustomListBox = class(TWSCustomListBox)
  private
  protected
  public
  end;

  { TCarbonWSListBox }

  TCarbonWSListBox = class(TWSListBox)
  private
  protected
  public
  end;

  { TCarbonWSCustomEdit }

  TCarbonWSCustomEdit = class(TWSCustomEdit)
  private
  protected
  public
  end;

  { TCarbonWSCustomMemo }

  TCarbonWSCustomMemo = class(TWSCustomMemo)
  private
  protected
  public
  end;

  { TCarbonWSEdit }

  TCarbonWSEdit = class(TWSEdit)
  private
  protected
  public
  end;

  { TCarbonWSMemo }

  TCarbonWSMemo = class(TWSMemo)
  private
  protected
  public
  end;

  { TCarbonWSCustomLabel }

  TCarbonWSCustomLabel = class(TWSCustomLabel)
  private
  protected
  public
  end;

  { TCarbonWSLabel }

  TCarbonWSLabel = class(TWSLabel)
  private
  protected
  public
  end;

  { TCarbonWSButtonControl }

  TCarbonWSButtonControl = class(TWSButtonControl)
  private
  protected
  public
  end;

  { TCarbonWSCustomCheckBox }

  TCarbonWSCustomCheckBox = class(TWSCustomCheckBox)
  private
  protected
  public
  end;

  { TCarbonWSCheckBox }

  TCarbonWSCheckBox = class(TWSCheckBox)
  private
  protected
  public
  end;

  { TCarbonWSCheckBox }

  TCarbonWSCheckBox = class(TWSCheckBox)
  private
  protected
  public
  end;

  { TCarbonWSToggleBox }

  TCarbonWSToggleBox = class(TWSToggleBox)
  private
  protected
  public
  end;

  { TCarbonWSRadioButton }

  TCarbonWSRadioButton = class(TWSRadioButton)
  private
  protected
  public
  end;

  { TCarbonWSCustomStaticText }

  TCarbonWSCustomStaticText = class(TWSCustomStaticText)
  private
  protected
  public
  end;

  { TCarbonWSStaticText }

  TCarbonWSStaticText = class(TWSStaticText)
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
//  RegisterWSComponent(TScrollBar, TCarbonWSScrollBar);
//  RegisterWSComponent(TCustomGroupBox, TCarbonWSCustomGroupBox);
//  RegisterWSComponent(TGroupBox, TCarbonWSGroupBox);
//  RegisterWSComponent(TCustomComboBox, TCarbonWSCustomComboBox);
//  RegisterWSComponent(TComboBox, TCarbonWSComboBox);
//  RegisterWSComponent(TCustomListBox, TCarbonWSCustomListBox);
//  RegisterWSComponent(TListBox, TCarbonWSListBox);
//  RegisterWSComponent(TCustomEdit, TCarbonWSCustomEdit);
//  RegisterWSComponent(TCustomMemo, TCarbonWSCustomMemo);
//  RegisterWSComponent(TEdit, TCarbonWSEdit);
//  RegisterWSComponent(TMemo, TCarbonWSMemo);
//  RegisterWSComponent(TCustomLabel, TCarbonWSCustomLabel);
//  RegisterWSComponent(TLabel, TCarbonWSLabel);
//  RegisterWSComponent(TButtonControl, TCarbonWSButtonControl);
//  RegisterWSComponent(TCustomCheckBox, TCarbonWSCustomCheckBox);
//  RegisterWSComponent(TCheckBox, TCarbonWSCheckBox);
//  RegisterWSComponent(TCheckBox, TCarbonWSCheckBox);
//  RegisterWSComponent(TToggleBox, TCarbonWSToggleBox);
//  RegisterWSComponent(TRadioButton, TCarbonWSRadioButton);
//  RegisterWSComponent(TCustomStaticText, TCarbonWSCustomStaticText);
//  RegisterWSComponent(TStaticText, TCarbonWSStaticText);
////////////////////////////////////////////////////
end.
