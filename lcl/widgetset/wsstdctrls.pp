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
  StdCtrls, Graphics,
////////////////////////////////////////////////////
  WSLCLClasses, WSControls, Classes;

type
  { TWSScrollBar }

  TWSScrollBarClass = class of TWSScrollBar;
  TWSScrollBar = class(TWSWinControl)
    class procedure SetParams(const AScrollBar: TCustomScrollBar); virtual;
  end;

  { TWSCustomGroupBox }

  TWSCustomGroupBox = class(TWSCustomControl)
  end;

  { TWSGroupBox }

  TWSGroupBox = class(TWSCustomGroupBox)
  end;

  { TWSCustomComboBox }

  TWSCustomComboBox = class(TWSWinControl)
    class function  GetSelStart(const ACustomComboBox: TCustomComboBox): integer; virtual;
    class function  GetSelLength(const ACustomComboBox: TCustomComboBox): integer; virtual;
    class function  GetItemIndex(const ACustomComboBox: TCustomComboBox): integer; virtual;
    class function  GetMaxLength(const ACustomComboBox: TCustomComboBox): integer; virtual;
    
    class procedure SetArrowKeysTraverseList(const ACustomComboBox: TCustomComboBox; 
      NewTraverseList: boolean); virtual;
    class procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); virtual;
    class procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); virtual;
    class procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer); virtual;
    class procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); virtual;
    class procedure SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle); virtual;

    class function  GetItems(const ACustomComboBox: TCustomComboBox): TStrings; virtual;
    class procedure Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean); virtual;
  end;
  TWSCustomComboBoxClass = class of TWSCustomComboBox;

  { TWSComboBox }

  TWSComboBox = class(TWSCustomComboBox)
  end;

  { TWSCustomListBox }

  TWSCustomListBox = class(TWSWinControl)
    class function  GetSelCount(const ACustomListBox: TCustomListBox): integer; virtual;
    class function  GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean; virtual;
    class function  GetStrings(const ACustomListBox: TCustomListBox): TStrings; virtual;
    class function  GetItemIndex(const ACustomListBox: TCustomListBox): integer; virtual;
    class function  GetTopIndex(const ACustomListBox: TCustomListBox): integer; virtual;
    class procedure SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean); virtual;
    class procedure SetBorder(const ACustomListBox: TCustomListBox); virtual;
    class procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer); virtual;
    class procedure SetSelectionMode(const ACustomListBox: TCustomListBox; const AExtendedSelect, 
      AMultiSelect: boolean); virtual;
    class procedure SetStyle(const ACustomListBox: TCustomListBox); virtual;
    class procedure SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean); virtual;
    class procedure SetTopIndex(const ACustomListBox: TCustomListBox; const NewTopIndex: integer); virtual;
  end;
  TWSCustomListBoxClass = class of TWSCustomListBox;
  
  { TWSListBox }

  TWSListBox = class(TWSCustomListBox)
  end;

  { TWSCustomEdit }

  TWSCustomEdit = class(TWSWinControl)
    class function  GetSelStart(const ACustomEdit: TCustomEdit): integer; virtual;
    class function  GetSelLength(const ACustomEdit: TCustomEdit): integer; virtual;

    class procedure SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase); virtual;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); virtual;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); virtual;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); virtual;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); virtual;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); virtual;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); virtual;
  end;
  TWSCustomEditClass = class of TWSCustomEdit;

  { TWSCustomMemo }

  TWSCustomMemo = class(TWSCustomEdit)
    class procedure AppendText(const ACustomMemo: TCustomMemo; const AText: string); virtual;
    class procedure SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle); virtual;
    class procedure SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean); virtual;
  end;
  TWSCustomMemoClass = class of TWSCustomMemo;

  { TWSEdit }

  TWSEdit = class(TWSCustomEdit)
  end;

  { TWSMemo }

  TWSMemo = class(TWSCustomMemo)
  end;

  { TWSCustomStaticText }

  TWSCustomStaticTextClass = class of TWSCustomStaticText;
  TWSCustomStaticText = class(TWSWinControl)
    class procedure SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment); virtual;
    class procedure SetLayout(const ACustomStaticText: TCustomStaticText; const NewLayout: TTextLayout); virtual;
  end;

  { TWSStaticText }

  TWSStaticText = class(TWSCustomStaticText)
  end;

  { TWSButtonControl }

  TWSButtonControl = class(TWSWinControl)
  end;

  { TWSCustomCheckBox }

  TWSCustomCheckBox = class(TWSButtonControl)
    class function  RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; virtual;
    class procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox; 
      const OldShortCut, NewShortCut: TShortCut); virtual;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); virtual;
  end;
  TWSCustomCheckBoxClass = class of TWSCustomCheckBox;

  { TWSCheckBox }

  TWSCheckBox = class(TWSCustomCheckBox)
  end;

  { TWSToggleBox }

  TWSToggleBox = class(TWSCustomCheckBox)
  end;

  { TWSRadioButton }

  TWSRadioButton = class(TWSCustomCheckBox)
  end;


implementation

{ TWSScrollBar }

procedure TWSScrollBar.SetParams(const AScrollBar: TCustomScrollBar);
begin
end;

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

function  TWSCustomListBox.GetTopIndex(const ACustomListBox: TCustomListBox): integer;
begin
  Result := 0;
end;

procedure TWSCustomListBox.SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean);
begin
end;

procedure TWSCustomListBox.SetBorder(const ACustomListBox: TCustomListBox);
begin
end;

procedure TWSCustomListBox.SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer);
begin
end;

procedure TWSCustomListBox.SetSelectionMode(const ACustomListBox: TCustomListBox; 
  const AExtendedSelect, AMultiSelect: boolean);
begin
end;

procedure TWSCustomListBox.SetStyle(const ACustomListBox: TCustomListBox);
begin
end;

procedure TWSCustomListBox.SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean);
begin
end;

procedure TWSCustomListBox.SetTopIndex(const ACustomListBox: TCustomListBox; const NewTopIndex: integer);
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

procedure TWSCustomComboBox.SetArrowKeysTraverseList(const ACustomComboBox: TCustomComboBox; 
  NewTraverseList: boolean);
begin
end;

procedure TWSCustomComboBox.SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer);
begin
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

procedure TWSCustomComboBox.SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle);
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

procedure TWSCustomEdit.SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase);
begin
end;

procedure TWSCustomEdit.SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode);
begin
end;

procedure TWSCustomEdit.SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer);
begin
end;

procedure TWSCustomEdit.SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char);
begin
end;

procedure TWSCustomEdit.SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean);
begin
end;

procedure TWSCustomEdit.SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer);
begin
end;

procedure TWSCustomEdit.SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer);
begin
end;

{ TWSCustomMemo }

procedure TWSCustomMemo.AppendText(const ACustomMemo: TCustomMemo; const AText: string);
begin
end;

procedure TWSCustomMemo.SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle);
begin
end;

procedure TWSCustomMemo.SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean);
begin
end;

{ TWSCustomStaticText }

procedure TWSCustomStaticText.SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment);
begin
end;

procedure TWSCustomStaticText.SetLayout(const ACustomStaticText: TCustomStaticText; const NewLayout: TTextLayout);
begin
end;

{ TWSCustomCheckBox }

function  TWSCustomCheckBox.RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
begin
  Result := cbUnchecked;
end;

procedure TWSCustomCheckBox.SetShortCut(const ACustomCheckBox: TCustomCheckBox; const OldShortCut, NewShortCut: TShortCut);
begin
end;

procedure TWSCustomCheckBox.SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
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
//  RegisterWSComponent(TCustomComboBox, TWSCustomComboBox);
//  RegisterWSComponent(TComboBox, TWSComboBox);
//  RegisterWSComponent(TCustomListBox, TWSCustomListBox);
//  RegisterWSComponent(TListBox, TWSListBox);
//  RegisterWSComponent(TCustomEdit, TWSCustomEdit);
//  RegisterWSComponent(TCustomMemo, TWSCustomMemo);
//  RegisterWSComponent(TEdit, TWSEdit);
//  RegisterWSComponent(TMemo, TWSMemo);
//  RegisterWSComponent(TCustomStaticText, TWSCustomStaticText);
//  RegisterWSComponent(TStaticText, TWSStaticText);
//  RegisterWSComponent(TButtonControl, TWSButtonControl);
//  RegisterWSComponent(TCustomCheckBox, TWSCustomCheckBox);
//  RegisterWSComponent(TCheckBox, TWSCheckBox);
//  RegisterWSComponent(TCheckBox, TWSCheckBox);
//  RegisterWSComponent(TToggleBox, TWSToggleBox);
//  RegisterWSComponent(TRadioButton, TWSRadioButton);
//  RegisterWSComponent(TLabel, TWSLabel);
////////////////////////////////////////////////////
end.
