{ $Id$}
{
 *****************************************************************************
 *                               WSStdCtrls.pp                               * 
 *                               -------------                               * 
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
unit WSStdCtrls;

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
  WSLCLClasses, WSControls;

type

  { TWSScrollBar }

  TWSScrollBar = class(TWSWinControl)
  private
  protected
  public
  end;

  { TWSCustomGroupBox }

  TWSCustomGroupBox = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TWSGroupBox }

  TWSGroupBox = class(TWSCustomGroupBox)
  private
  protected
  public
  end;

  { TWSCustomComboBox }

  TWSCustomComboBox = class(TWSWinControl)
  private
  protected
  public
  end;

  { TWSComboBox }

  TWSComboBox = class(TWSCustomComboBox)
  private
  protected
  public
  end;

  { TWSCustomListBox }

  TWSCustomListBox = class(TWSWinControl)
  private
  protected
  public
  end;

  { TWSListBox }

  TWSListBox = class(TWSCustomListBox)
  private
  protected
  public
  end;

  { TWSCustomEdit }

  TWSCustomEdit = class(TWSWinControl)
  private
  protected
  public
  end;

  { TWSCustomMemo }

  TWSCustomMemo = class(TWSCustomEdit)
  private
  protected
  public
  end;

  { TWSEdit }

  TWSEdit = class(TWSCustomEdit)
  private
  protected
  public
  end;

  { TWSMemo }

  TWSMemo = class(TWSCustomMemo)
  private
  protected
  public
  end;

  { TWSCustomLabel }

  TWSCustomLabel = class(TWSWinControl)
  private
  protected
  public
  end;

  { TWSLabel }

  TWSLabel = class(TWSCustomLabel)
  private
  protected
  public
  end;

  { TWSButtonControl }

  TWSButtonControl = class(TWSWinControl)
  private
  protected
  public
  end;

  { TWSCustomCheckBox }

  TWSCustomCheckBox = class(TWSButtonControl)
  private
  protected
  public
  end;

  { TWSCheckBox }

  TWSCheckBox = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TWSCheckBox }

  TWSCheckBox = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TWSToggleBox }

  TWSToggleBox = class(TWSCustomCheckBox)
  private
  protected
  public
  end;

  { TWSRadioButton }

  TWSRadioButton = class(TWSCustomCheckBox)
  private
  protected
  public
  end;

  { TWSCustomStaticText }

  TWSCustomStaticText = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TWSStaticText }

  TWSStaticText = class(TWSCustomStaticText)
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
//  RegisterWSComponent(TScrollBar, TWSScrollBar);
//  RegisterWSComponent(TCustomGroupBox, TWSCustomGroupBox);
//  RegisterWSComponent(TGroupBox, TWSGroupBox);
//  RegisterWSComponent(TCustomComboBox, TWSCustomComboBox);
//  RegisterWSComponent(TComboBox, TWSComboBox);
//  RegisterWSComponent(TCustomListBox, TWSCustomListBox);
//  RegisterWSComponent(TListBox, TWSListBox);
//  RegisterWSComponent(TCustomEdit, TWSCustomEdit);
//  RegisterWSComponent(TCustomMemo, TWSCustomMemo);
//  RegisterWSComponent(TEdit, TWSEdit);
//  RegisterWSComponent(TMemo, TWSMemo);
//  RegisterWSComponent(TCustomLabel, TWSCustomLabel);
//  RegisterWSComponent(TLabel, TWSLabel);
//  RegisterWSComponent(TButtonControl, TWSButtonControl);
//  RegisterWSComponent(TCustomCheckBox, TWSCustomCheckBox);
//  RegisterWSComponent(TCheckBox, TWSCheckBox);
//  RegisterWSComponent(TCheckBox, TWSCheckBox);
//  RegisterWSComponent(TToggleBox, TWSToggleBox);
//  RegisterWSComponent(TRadioButton, TWSRadioButton);
//  RegisterWSComponent(TCustomStaticText, TWSCustomStaticText);
//  RegisterWSComponent(TStaticText, TWSStaticText);
////////////////////////////////////////////////////
end.
