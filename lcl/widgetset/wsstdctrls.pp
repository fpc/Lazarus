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
  StdCtrls,
////////////////////////////////////////////////////
  WSLCLClasses, WSControls, Classes;

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
  public
    class function  GetSelStart(const ACustomComboBox: TCustomComboBox): integer; virtual;
    class function  GetSelLength(const ACustomComboBox: TCustomComboBox): integer; virtual;
    class function  GetItemIndex(const ACustomComboBox: TCustomComboBox): integer; virtual;
    class function  GetMaxLength(const ACustomComboBox: TCustomComboBox): integer; virtual;
    
    class procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); virtual;
    class procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); virtual;
    class procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer); virtual;
    class procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); virtual;

    class function  GetItems(const ACustomComboBox: TCustomComboBox): TStrings; virtual;
    class procedure Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean); virtual;
  end;
  TWSCustomComboBoxClass = class of TWSCustomComboBox;

  { TWSComboBox }

  TWSComboBox = class(TWSCustomComboBox)
  end;

  { TWSCustomListBox }

  TWSCustomListBox = class(TWSWinControl)
  public
    class function  GetSelCount(const ACustomListBox: TCustomListBox): integer; virtual;
    class function  GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean; virtual;
    class function  GetStrings(const ACustomListBox: TCustomListBox): TStrings; virtual;
    class function  GetItemIndex(const ACustomListBox: TCustomListBox): integer; virtual;
    class procedure SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean); virtual;
    class procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer); virtual;
    class procedure SetStyle(const ACustomListBox: TCustomListBox); virtual;
    class procedure SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean); virtual;
  end;
  TWSCustomListBoxClass = class of TWSCustomListBox;
  
  { TWSListBox }

  TWSListBox = class(TWSCustomListBox)
  end;

  { TWSCustomEdit }

  TWSCustomEdit = class(TWSWinControl)
  public
    class function  GetSelStart(const ACustomEdit: TCustomEdit): integer; virtual;
    class function  GetSelLength(const ACustomEdit: TCustomEdit): integer; virtual;

    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); virtual;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); virtual;
  end;
  TWSCustomEditClass = class of TWSCustomEdit;

  { TWSCustomMemo }

  TWSCustomMemo = class(TWSCustomEdit)
  public
    class procedure AppendText(const ACustomMemo: TCustomMemo; AText: string); virtual;
  end;
  TWSCustomMemoClass = class of TWSCustomMemo;

  { TWSEdit }

  TWSEdit = class(TWSCustomEdit)
  end;

  { TWSMemo }

  TWSMemo = class(TWSCustomMemo)
  end;

  { TWSCustomLabel }

  TWSCustomLabel = class(TWSWinControl)
  public
  end;
  TWSCustomLabelClass = class of TWSCustomLabel;

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

{ TWSCustomListBox }

function  TWSCustomListBox.GetItemIndex(const ACustomListBox: TCustomListBox): integer;
begin
  Result := 0;
end;

function  TWSCustomListBox.GetSelCount(const ACustomListBox: TCustomListBox): integer;
begin
  Result := 0;
end;

function  TWSCustomListBox.GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean;
begin
  Result := false;
end;

function  TWSCustomListBox.GetStrings(const ACustomListBox: TCustomListBox): TStrings;
begin
  Result := nil;
end;

procedure TWSCustomListBox.SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean);
begin
end;

procedure TWSCustomListBox.SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer);
begin
end;

procedure TWSCustomListBox.SetStyle(const ACustomListBox: TCustomListBox);
begin
end;

procedure TWSCustomListBox.SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean);
begin
end;

{ TWSCustomComboBox }

function  TWSCustomComboBox.GetSelStart(const ACustomComboBox: TCustomComboBox): integer;
begin
  result := -1;
end;

function  TWSCustomComboBox.GetSelLength(const ACustomComboBox: TCustomComboBox): integer;
begin
  result := 0;
end;

function  TWSCustomComboBox.GetItemIndex(const ACustomComboBox: TCustomComboBox): integer;
begin
  result := -1;
end;

function  TWSCustomComboBox.GetMaxLength(const ACustomComboBox: TCustomComboBox): integer;
begin
  result := 0;
end;

procedure TWSCustomComboBox.SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer);
begin
end;

procedure TWSCustomComboBox.SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer);
begin
end;

procedure TWSCustomComboBox.SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer);
begin
end;

procedure TWSCustomComboBox.SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer);
begin
end;

function  TWSCustomComboBox.GetItems(const ACustomComboBox: TCustomComboBox): TStrings;
begin
  result := nil;
end;

procedure TWSCustomComboBox.Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean);
begin
end;

{ TWSCustomEdit }

function  TWSCustomEdit.GetSelStart(const ACustomEdit: TCustomEdit): integer;
begin
  result := -1;
end;

function  TWSCustomEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
begin
  result := 0;
end;

procedure TWSCustomEdit.SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer);
begin
end;

procedure TWSCustomEdit.SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer);
begin
end;

{ TWSCustomMemo }

procedure TWSCustomMemo.AppendText(const ACustomMemo: TCustomMemo; AText: string);
begin
end;

initialization

////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TScrollBar, TWSScrollBar);
//  RegisterWSComponent(TCustomGroupBox, TWSCustomGroupBox);
//  RegisterWSComponent(TGroupBox, TWSGroupBox);
  RegisterWSComponent(TCustomComboBox, TWSCustomComboBox);
//  RegisterWSComponent(TComboBox, TWSComboBox);
  RegisterWSComponent(TCustomListBox, TWSCustomListBox);
//  RegisterWSComponent(TListBox, TWSListBox);
  RegisterWSComponent(TCustomEdit, TWSCustomEdit);
  RegisterWSComponent(TCustomMemo, TWSCustomMemo);
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
