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
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as posible circles, the uses
//    clause should contain only those LCL units 
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the 
//    initialization section which actually 
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
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
  end;

  { TWSCustomGroupBox }

  TWSCustomGroupBox = class(TWSCustomControl)
  end;

  { TWSGroupBox }

  TWSGroupBox = class(TWSCustomGroupBox)
  end;

  { TWSCustomComboBox }

  TWSCustomComboBox = class(TWSWinControl)
  end;

  { TWSComboBox }

  TWSComboBox = class(TWSCustomComboBox)
  end;

  { TWSCustomListBox }

  TWSCustomListBox = class(TWSWinControl)
  end;

  { TWSListBox }

  TWSListBox = class(TWSCustomListBox)
  end;

  { TWSCustomEdit }

  TWSCustomEdit = class(TWSWinControl)
  end;

  { TWSCustomMemo }

  TWSCustomMemo = class(TWSCustomEdit)
  end;

  { TWSEdit }

  TWSEdit = class(TWSCustomEdit)
  end;

  { TWSMemo }

  TWSMemo = class(TWSCustomMemo)
  end;

  { TWSCustomLabel }

  TWSCustomLabel = class(TWSWinControl)
  end;

  { TWSLabel }

  TWSLabel = class(TWSCustomLabel)
  end;

  { TWSButtonControl }

  TWSButtonControl = class(TWSWinControl)
  end;

  { TWSCustomCheckBox }

  TWSCustomCheckBox = class(TWSButtonControl)
  end;

  { TWSCheckBox }

  TWSCheckBox = class(TWSCustomCheckBox)
  end;

  { TWSToggleBox }

  TWSToggleBox = class(TWSCustomCheckBox)
  end;

  { TWSRadioButton }

  TWSRadioButton = class(TWSCustomCheckBox)
  end;

  { TWSCustomStaticText }

  TWSCustomStaticText = class(TWSCustomControl)
  end;

  { TWSStaticText }

  TWSStaticText = class(TWSCustomStaticText)
  end;


implementation

initialization

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
