{ $Id$}
{
 *****************************************************************************
 *                             gtkwsstdctrls.pp                              * 
 *                             ----------------                              * 
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
unit gtkwsstdctrls;

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

  { TGtkWSScrollBar }

  TGtkWSScrollBar = class(TWSScrollBar)
  private
  protected
  public
  end;

  { TGtkWSCustomGroupBox }

  TGtkWSCustomGroupBox = class(TWSCustomGroupBox)
  private
  protected
  public
  end;

  { TGtkWSGroupBox }

  TGtkWSGroupBox = class(TWSGroupBox)
  private
  protected
  public
  end;

  { TGtkWSCustomComboBox }

  TGtkWSCustomComboBox = class(TWSCustomComboBox)
  private
  protected
  public
  end;

  { TGtkWSComboBox }

  TGtkWSComboBox = class(TWSComboBox)
  private
  protected
  public
  end;

  { TGtkWSCustomListBox }

  TGtkWSCustomListBox = class(TWSCustomListBox)
  private
  protected
  public
  end;

  { TGtkWSListBox }

  TGtkWSListBox = class(TWSListBox)
  private
  protected
  public
  end;

  { TGtkWSCustomEdit }

  TGtkWSCustomEdit = class(TWSCustomEdit)
  private
  protected
  public
  end;

  { TGtkWSCustomMemo }

  TGtkWSCustomMemo = class(TWSCustomMemo)
  private
  protected
  public
  end;

  { TGtkWSEdit }

  TGtkWSEdit = class(TWSEdit)
  private
  protected
  public
  end;

  { TGtkWSMemo }

  TGtkWSMemo = class(TWSMemo)
  private
  protected
  public
  end;

  { TGtkWSCustomLabel }

  TGtkWSCustomLabel = class(TWSCustomLabel)
  private
  protected
  public
  end;

  { TGtkWSLabel }

  TGtkWSLabel = class(TWSLabel)
  private
  protected
  public
  end;

  { TGtkWSButtonControl }

  TGtkWSButtonControl = class(TWSButtonControl)
  private
  protected
  public
  end;

  { TGtkWSCustomCheckBox }

  TGtkWSCustomCheckBox = class(TWSCustomCheckBox)
  private
  protected
  public
  end;

  { TGtkWSCheckBox }

  TGtkWSCheckBox = class(TWSCheckBox)
  private
  protected
  public
  end;

  { TGtkWSCheckBox }

  TGtkWSCheckBox = class(TWSCheckBox)
  private
  protected
  public
  end;

  { TGtkWSToggleBox }

  TGtkWSToggleBox = class(TWSToggleBox)
  private
  protected
  public
  end;

  { TGtkWSRadioButton }

  TGtkWSRadioButton = class(TWSRadioButton)
  private
  protected
  public
  end;

  { TGtkWSCustomStaticText }

  TGtkWSCustomStaticText = class(TWSCustomStaticText)
  private
  protected
  public
  end;

  { TGtkWSStaticText }

  TGtkWSStaticText = class(TWSStaticText)
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
//  RegisterWSComponent(TScrollBar, TGtkWSScrollBar);
//  RegisterWSComponent(TCustomGroupBox, TGtkWSCustomGroupBox);
//  RegisterWSComponent(TGroupBox, TGtkWSGroupBox);
//  RegisterWSComponent(TCustomComboBox, TGtkWSCustomComboBox);
//  RegisterWSComponent(TComboBox, TGtkWSComboBox);
//  RegisterWSComponent(TCustomListBox, TGtkWSCustomListBox);
//  RegisterWSComponent(TListBox, TGtkWSListBox);
//  RegisterWSComponent(TCustomEdit, TGtkWSCustomEdit);
//  RegisterWSComponent(TCustomMemo, TGtkWSCustomMemo);
//  RegisterWSComponent(TEdit, TGtkWSEdit);
//  RegisterWSComponent(TMemo, TGtkWSMemo);
//  RegisterWSComponent(TCustomLabel, TGtkWSCustomLabel);
//  RegisterWSComponent(TLabel, TGtkWSLabel);
//  RegisterWSComponent(TButtonControl, TGtkWSButtonControl);
//  RegisterWSComponent(TCustomCheckBox, TGtkWSCustomCheckBox);
//  RegisterWSComponent(TCheckBox, TGtkWSCheckBox);
//  RegisterWSComponent(TCheckBox, TGtkWSCheckBox);
//  RegisterWSComponent(TToggleBox, TGtkWSToggleBox);
//  RegisterWSComponent(TRadioButton, TGtkWSRadioButton);
//  RegisterWSComponent(TCustomStaticText, TGtkWSCustomStaticText);
//  RegisterWSComponent(TStaticText, TGtkWSStaticText);
////////////////////////////////////////////////////
end.
