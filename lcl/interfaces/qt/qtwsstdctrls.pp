{ $Id$}
{
 *****************************************************************************
 *                              qtwsstdctrls.pp                              * 
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
unit qtwsstdctrls;

{$mode objfpc}{H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as litle as posible circles,
// Uncomment only when needed for registration
////////////////////////////////////////////////////
//  stdctrls,
////////////////////////////////////////////////////
  wsstdctrls, wslclclasses;

type

  { TQtWSScrollBar }

  TQtWSScrollBar = class(TWSScrollBar)
  private
  protected
  public
  end;

  { TQtWSCustomGroupBox }

  TQtWSCustomGroupBox = class(TWSCustomGroupBox)
  private
  protected
  public
  end;

  { TQtWSGroupBox }

  TQtWSGroupBox = class(TWSGroupBox)
  private
  protected
  public
  end;

  { TQtWSCustomComboBox }

  TQtWSCustomComboBox = class(TWSCustomComboBox)
  private
  protected
  public
  end;

  { TQtWSComboBox }

  TQtWSComboBox = class(TWSComboBox)
  private
  protected
  public
  end;

  { TQtWSCustomListBox }

  TQtWSCustomListBox = class(TWSCustomListBox)
  private
  protected
  public
  end;

  { TQtWSListBox }

  TQtWSListBox = class(TWSListBox)
  private
  protected
  public
  end;

  { TQtWSCustomEdit }

  TQtWSCustomEdit = class(TWSCustomEdit)
  private
  protected
  public
  end;

  { TQtWSCustomMemo }

  TQtWSCustomMemo = class(TWSCustomMemo)
  private
  protected
  public
  end;

  { TQtWSEdit }

  TQtWSEdit = class(TWSEdit)
  private
  protected
  public
  end;

  { TQtWSMemo }

  TQtWSMemo = class(TWSMemo)
  private
  protected
  public
  end;

  { TQtWSCustomLabel }

  TQtWSCustomLabel = class(TWSCustomLabel)
  private
  protected
  public
  end;

  { TQtWSLabel }

  TQtWSLabel = class(TWSLabel)
  private
  protected
  public
  end;

  { TQtWSButtonControl }

  TQtWSButtonControl = class(TWSButtonControl)
  private
  protected
  public
  end;

  { TQtWSCustomCheckBox }

  TQtWSCustomCheckBox = class(TWSCustomCheckBox)
  private
  protected
  public
  end;

  { TQtWSCheckBox }

  TQtWSCheckBox = class(TWSCheckBox)
  private
  protected
  public
  end;

  { TQtWSCheckBox }

  TQtWSCheckBox = class(TWSCheckBox)
  private
  protected
  public
  end;

  { TQtWSToggleBox }

  TQtWSToggleBox = class(TWSToggleBox)
  private
  protected
  public
  end;

  { TQtWSRadioButton }

  TQtWSRadioButton = class(TWSRadioButton)
  private
  protected
  public
  end;

  { TQtWSCustomStaticText }

  TQtWSCustomStaticText = class(TWSCustomStaticText)
  private
  protected
  public
  end;

  { TQtWSStaticText }

  TQtWSStaticText = class(TWSStaticText)
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
//  RegisterWSComponent(TScrollBar, TQtWSScrollBar);
//  RegisterWSComponent(TCustomGroupBox, TQtWSCustomGroupBox);
//  RegisterWSComponent(TGroupBox, TQtWSGroupBox);
//  RegisterWSComponent(TCustomComboBox, TQtWSCustomComboBox);
//  RegisterWSComponent(TComboBox, TQtWSComboBox);
//  RegisterWSComponent(TCustomListBox, TQtWSCustomListBox);
//  RegisterWSComponent(TListBox, TQtWSListBox);
//  RegisterWSComponent(TCustomEdit, TQtWSCustomEdit);
//  RegisterWSComponent(TCustomMemo, TQtWSCustomMemo);
//  RegisterWSComponent(TEdit, TQtWSEdit);
//  RegisterWSComponent(TMemo, TQtWSMemo);
//  RegisterWSComponent(TCustomLabel, TQtWSCustomLabel);
//  RegisterWSComponent(TLabel, TQtWSLabel);
//  RegisterWSComponent(TButtonControl, TQtWSButtonControl);
//  RegisterWSComponent(TCustomCheckBox, TQtWSCustomCheckBox);
//  RegisterWSComponent(TCheckBox, TQtWSCheckBox);
//  RegisterWSComponent(TCheckBox, TQtWSCheckBox);
//  RegisterWSComponent(TToggleBox, TQtWSToggleBox);
//  RegisterWSComponent(TRadioButton, TQtWSRadioButton);
//  RegisterWSComponent(TCustomStaticText, TQtWSCustomStaticText);
//  RegisterWSComponent(TStaticText, TQtWSStaticText);
////////////////////////////////////////////////////
end.
