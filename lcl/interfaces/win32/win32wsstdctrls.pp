{ $Id$}
{
 *****************************************************************************
 *                            Win32WSStdCtrls.pp                             * 
 *                            ------------------                             * 
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
unit Win32WSStdCtrls;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  StdCtrls,
////////////////////////////////////////////////////
  WSStdCtrls, WSLCLClasses;

type

  { TWin32WSScrollBar }

  TWin32WSScrollBar = class(TWSScrollBar)
  private
  protected
  public
  end;

  { TWin32WSCustomGroupBox }

  TWin32WSCustomGroupBox = class(TWSCustomGroupBox)
  private
  protected
  public
  end;

  { TWin32WSGroupBox }

  TWin32WSGroupBox = class(TWSGroupBox)
  private
  protected
  public
  end;

  { TWin32WSCustomComboBox }

  TWin32WSCustomComboBox = class(TWSCustomComboBox)
  private
  protected
  public
  end;

  { TWin32WSComboBox }

  TWin32WSComboBox = class(TWSComboBox)
  private
  protected
  public
  end;

  { TWin32WSCustomListBox }

  TWin32WSCustomListBox = class(TWSCustomListBox)
  private
  protected
  public
    class procedure SetStyle(const ACustomListBox: TCustomListBox); override;
  end;
    
  { TWin32WSListBox }

  TWin32WSListBox = class(TWSListBox)
  private
  protected
  public
  end;

  { TWin32WSCustomEdit }

  TWin32WSCustomEdit = class(TWSCustomEdit)
  private
  protected
  public
  end;

  { TWin32WSCustomMemo }

  TWin32WSCustomMemo = class(TWSCustomMemo)
  private
  protected
  public
  end;

  { TWin32WSEdit }

  TWin32WSEdit = class(TWSEdit)
  private
  protected
  public
  end;

  { TWin32WSMemo }

  TWin32WSMemo = class(TWSMemo)
  private
  protected
  public
  end;

  { TWin32WSCustomLabel }

  TWin32WSCustomLabel = class(TWSCustomLabel)
  private
  protected
  public
  end;

  { TWin32WSLabel }

  TWin32WSLabel = class(TWSLabel)
  private
  protected
  public
  end;

  { TWin32WSButtonControl }

  TWin32WSButtonControl = class(TWSButtonControl)
  private
  protected
  public
  end;

  { TWin32WSCustomCheckBox }

  TWin32WSCustomCheckBox = class(TWSCustomCheckBox)
  private
  protected
  public
  end;

  { TWin32WSCheckBox }

  TWin32WSCheckBox = class(TWSCheckBox)
  private
  protected
  public
  end;

  { TWin32WSToggleBox }

  TWin32WSToggleBox = class(TWSToggleBox)
  private
  protected
  public
  end;

  { TWin32WSRadioButton }

  TWin32WSRadioButton = class(TWSRadioButton)
  private
  protected
  public
  end;

  { TWin32WSCustomStaticText }

  TWin32WSCustomStaticText = class(TWSCustomStaticText)
  private
  protected
  public
  end;

  { TWin32WSStaticText }

  TWin32WSStaticText = class(TWSStaticText)
  private
  protected
  public
  end;


implementation

uses
  Win32Int, InterfaceBase;

{ TWin32WSCustomListBox }

procedure TWin32WSCustomListBox.SetStyle(const ACustomListBox: TCustomListBox);
begin
  // The listbox styles can't be updated, so recreate the listbox
  TWin32WidgetSet(InterfaceObject).RecreateWnd(ACustomListBox);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TScrollBar, TWin32WSScrollBar);
//  RegisterWSComponent(TCustomGroupBox, TWin32WSCustomGroupBox);
//  RegisterWSComponent(TGroupBox, TWin32WSGroupBox);
//  RegisterWSComponent(TCustomComboBox, TWin32WSCustomComboBox);
//  RegisterWSComponent(TComboBox, TWin32WSComboBox);
  RegisterWSComponent(TCustomListBox, TWin32WSCustomListBox);
//  RegisterWSComponent(TListBox, TWin32WSListBox);
//  RegisterWSComponent(TCustomEdit, TWin32WSCustomEdit);
//  RegisterWSComponent(TCustomMemo, TWin32WSCustomMemo);
//  RegisterWSComponent(TEdit, TWin32WSEdit);
//  RegisterWSComponent(TMemo, TWin32WSMemo);
//  RegisterWSComponent(TCustomLabel, TWin32WSCustomLabel);
//  RegisterWSComponent(TLabel, TWin32WSLabel);
//  RegisterWSComponent(TButtonControl, TWin32WSButtonControl);
//  RegisterWSComponent(TCustomCheckBox, TWin32WSCustomCheckBox);
//  RegisterWSComponent(TCheckBox, TWin32WSCheckBox);
//  RegisterWSComponent(TCheckBox, TWin32WSCheckBox);
//  RegisterWSComponent(TToggleBox, TWin32WSToggleBox);
//  RegisterWSComponent(TRadioButton, TWin32WSRadioButton);
//  RegisterWSComponent(TCustomStaticText, TWin32WSCustomStaticText);
//  RegisterWSComponent(TStaticText, TWin32WSStaticText);
////////////////////////////////////////////////////
end.
