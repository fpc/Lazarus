{ $Id$}
{
 *****************************************************************************
 *                               WSStdCtrls.pp                               * 
 *                               -------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WSStdCtrls;

{$mode objfpc}{$H+}
{$I lcl_defines.inc}

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
  Classes,
////////////////////////////////////////////////////
// To get as little as possible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Graphics, Controls, StdCtrls,
////////////////////////////////////////////////////
  Clipbrd, LazUTF8, WSLCLClasses, WSControls, WSFactory;

type
  { TWSScrollBar }

  TWSScrollBarClass = class of TWSScrollBar;
  TWSScrollBar = class(TWSWinControl_CallWS)
  private class var
    FWSScrollBar_Impl: TWSScrollBarClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
    class procedure SetParams(const AScrollBar: TCustomScrollBar); virtual;
    class procedure SetKind(const AScrollBar: TCustomScrollBar; const AIsHorizontal: Boolean); virtual;
  end;

  { TWSCustomGroupBox }

  TWSCustomGroupBox = class(TWSCustomControl)
  published
  end;

  { TWSGroupBox }

  TWSGroupBoxClass = class of TWSGroupBox;
  TWSGroupBox = class(TWSCustomGroupBox)
  private class var
    FWSGroupBox_Impl: TWSGroupBoxClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
    class function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; override;
  end;

  { TWSCustomComboBox }

  TWSCustomComboBoxClass = class of TWSCustomComboBox;
  TWSCustomComboBox = class(TWSWinControl_CallWS)
  private class var
    FWSCustomComboBox_Impl: TWSCustomComboBoxClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
    class function GetDroppedDown(const ACustomComboBox: TCustomComboBox): Boolean; virtual;
    class function GetSelStart(const ACustomComboBox: TCustomComboBox): integer; virtual;
    class function GetSelLength(const ACustomComboBox: TCustomComboBox): integer; virtual;
    class function GetItemIndex(const ACustomComboBox: TCustomComboBox): integer; virtual;
    class function GetMaxLength(const ACustomComboBox: TCustomComboBox): integer; virtual;
    
    class procedure SetArrowKeysTraverseList(const ACustomComboBox: TCustomComboBox; 
      NewTraverseList: boolean); virtual;
    class procedure SetDropDownCount(const ACustomComboBox: TCustomComboBox; NewCount: Integer); virtual;
    class procedure SetDroppedDown(const ACustomComboBox: TCustomComboBox; ADroppedDown: Boolean); virtual;
    class procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); virtual;
    class procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); virtual;
    class procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer); virtual;
    class procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); virtual;
    class procedure SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle); virtual;
    class procedure SetReadOnly(const ACustomComboBox: TCustomComboBox; NewReadOnly: boolean); virtual;

    class function  GetItems(const ACustomComboBox: TCustomComboBox): TStrings; virtual;
    class procedure FreeItems(var AItems: TStrings); virtual;
    class procedure Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean); virtual;
    
    class function GetItemHeight(const ACustomComboBox: TCustomComboBox): Integer; virtual;
    class procedure SetItemHeight(const ACustomComboBox: TCustomComboBox; const AItemHeight: Integer); virtual;
  end;

  { TWSComboBox }

  TWSComboBox = class(TWSCustomComboBox)
  published
  end;

  { TWSCustomListBox }

  TWSCustomListBoxClass = class of TWSCustomListBox;
  TWSCustomListBox = class(TWSWinControl_CallWS)
  private class var
    FWSCustomListBox_Impl: TWSCustomListBoxClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
    class procedure DragStart(const ACustomListBox: TCustomListBox); virtual;

    class function GetIndexAtXY(const ACustomListBox: TCustomListBox; X, Y: integer): integer; virtual;
    class function GetItemIndex(const ACustomListBox: TCustomListBox): integer; virtual;
    class function GetItemRect(const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect): boolean; virtual;
    class function GetScrollWidth(const ACustomListBox: TCustomListBox): Integer; virtual;
    class function GetSelCount(const ACustomListBox: TCustomListBox): integer; virtual;
    class function GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean; virtual;
    class function GetStrings(const ACustomListBox: TCustomListBox): TStrings; virtual;
    class procedure FreeStrings(var AStrings: TStrings); virtual;
    class function GetTopIndex(const ACustomListBox: TCustomListBox): integer; virtual;

    class procedure SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean); virtual;

    class procedure SetBorder(const ACustomListBox: TCustomListBox); virtual;
    class procedure SetColumnCount(const ACustomListBox: TCustomListBox; ACount: Integer); virtual;
    class procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer); virtual;
    class procedure SetScrollWidth(const ACustomListBox: TCustomListBox; const AScrollWidth: Integer); virtual;
    class procedure SetSelectionMode(const ACustomListBox: TCustomListBox; const AExtendedSelect, 
      AMultiSelect: boolean); virtual;
    class procedure SetStyle(const ACustomListBox: TCustomListBox); virtual;
    class procedure SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean); virtual;
    class procedure SetTopIndex(const ACustomListBox: TCustomListBox; const NewTopIndex: integer); virtual;
  end;

  { TWSCustomListBox_CallWS }

  TWSCustomListBox_CallWS = class(TWSCustomListBox)
  public
    class procedure DragStart(const ACustomListBox: TCustomListBox); override;

    class function GetIndexAtXY(const ACustomListBox: TCustomListBox; X, Y: integer): integer; override;
    class function GetItemIndex(const ACustomListBox: TCustomListBox): integer; override;
    class function GetItemRect(const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect): boolean; override;
    class function GetScrollWidth(const ACustomListBox: TCustomListBox): Integer; override;
    class function GetSelCount(const ACustomListBox: TCustomListBox): integer; override;
    class function GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean; override;
    class function GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
    class procedure FreeStrings(var AStrings: TStrings); override;
    class function GetTopIndex(const ACustomListBox: TCustomListBox): integer; override;

    class procedure SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean); override;

    class procedure SetBorder(const ACustomListBox: TCustomListBox); override;
    class procedure SetColumnCount(const ACustomListBox: TCustomListBox; ACount: Integer); override;
    class procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer); override;
    class procedure SetScrollWidth(const ACustomListBox: TCustomListBox; const AScrollWidth: Integer); override;
    class procedure SetSelectionMode(const ACustomListBox: TCustomListBox; const AExtendedSelect, AMultiSelect: boolean); override;
    class procedure SetStyle(const ACustomListBox: TCustomListBox); override;
    class procedure SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean); override;
    class procedure SetTopIndex(const ACustomListBox: TCustomListBox; const NewTopIndex: integer); override;
  end;

  { TWSListBox }

  TWSListBox = class(TWSCustomListBox_CallWS)
  published
  end;

  { TWSCustomEdit }

  TWSCustomEditClass = class of TWSCustomEdit;
  TWSCustomEdit = class(TWSWinControl_CallWS)
  private class var
    FWSCustomEdit_Impl: TWSCustomEditClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
    class function GetCanUndo(const ACustomEdit: TCustomEdit): Boolean; virtual;
    class function GetCaretPos(const ACustomEdit: TCustomEdit): TPoint; virtual;
    class function GetSelStart(const ACustomEdit: TCustomEdit): integer; virtual;
    class function GetSelLength(const ACustomEdit: TCustomEdit): integer; virtual;

    class procedure SetAlignment(const ACustomEdit: TCustomEdit; const AAlignment: TAlignment); virtual;
    class procedure SetCaretPos(const ACustomEdit: TCustomEdit; const NewPos: TPoint); virtual;
    class procedure SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase); virtual;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); virtual;
    class procedure SetHideSelection(const ACustomEdit: TCustomEdit; NewHideSelection: Boolean); virtual;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); virtual;
    class procedure SetNumbersOnly(const ACustomEdit: TCustomEdit; NewNumbersOnly: Boolean); virtual;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); virtual;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); virtual;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); virtual;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); virtual;
    class procedure SetSelText(const ACustomEdit: TCustomEdit; const NewSelText: string); virtual;
    class procedure SetTextHint(const ACustomEdit: TCustomEdit; const ATextHint: string); virtual;
    class function CreateEmulatedTextHintFont(const ACustomEdit: TCustomEdit): TFont; virtual;

    class procedure Cut(const ACustomEdit: TCustomEdit); virtual;
    class procedure Copy(const ACustomEdit: TCustomEdit); virtual;
    class procedure Paste(const ACustomEdit: TCustomEdit); virtual;
    class procedure Undo(const ACustomEdit: TCustomEdit); virtual;
  end;

  { TWSCustomEdit_CallWS }

  TWSCustomEdit_CallWS = class(TWSCustomEdit)
  public
    class function GetCanUndo(const ACustomEdit: TCustomEdit): Boolean; override;
    class function GetCaretPos(const ACustomEdit: TCustomEdit): TPoint; override;
    class function GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function GetSelLength(const ACustomEdit: TCustomEdit): integer; override;

    class procedure SetAlignment(const ACustomEdit: TCustomEdit; const AAlignment: TAlignment); override;
    class procedure SetCaretPos(const ACustomEdit: TCustomEdit; const NewPos: TPoint); override;
    class procedure SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase); override;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); override;
    class procedure SetHideSelection(const ACustomEdit: TCustomEdit; NewHideSelection: Boolean); override;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetNumbersOnly(const ACustomEdit: TCustomEdit; NewNumbersOnly: Boolean); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetSelText(const ACustomEdit: TCustomEdit; const NewSelText: string); override;
    class procedure SetTextHint(const ACustomEdit: TCustomEdit; const ATextHint: string); override;
    class function CreateEmulatedTextHintFont(const ACustomEdit: TCustomEdit): TFont; override;

    class procedure Cut(const ACustomEdit: TCustomEdit); override;
    class procedure Copy(const ACustomEdit: TCustomEdit); override;
    class procedure Paste(const ACustomEdit: TCustomEdit); override;
    class procedure Undo(const ACustomEdit: TCustomEdit); override;
    //
    class procedure GetPreferredSize(const AWinControl: TWinControl;
          var PreferredWidth, PreferredHeight: integer;
          WithThemeSpace: Boolean); override;
  end;

  { TWSCustomMemo }

  TWSCustomMemoClass = class of TWSCustomMemo;
  TWSCustomMemo = class(TWSCustomEdit_CallWS)
  private class var
    FWSCustomMemo_Impl: TWSCustomMemoClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
    class procedure AppendText(const ACustomMemo: TCustomMemo; const AText: string); virtual;
    class function  GetStrings(const ACustomMemo: TCustomMemo): TStrings; virtual;
    class procedure FreeStrings(var AStrings: TStrings); virtual;
    class procedure SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle); virtual;
    class procedure SetWantTabs(const ACustomMemo: TCustomMemo; const NewWantTabs: boolean); virtual;
    class procedure SetWantReturns(const ACustomMemo: TCustomMemo; const NewWantReturns: boolean); virtual;
    class procedure SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean); virtual;
    class procedure SetSelText(const ACustomEdit: TCustomEdit; const NewSelText: string); override;
  end;

  { TWSEdit }

  TWSEdit = class(TWSCustomEdit_CallWS)
  published
  end;

  { TWSMemo }

  TWSMemo = class(TWSCustomMemo)
  published
  end;

  { TWSCustomStaticText }

  TWSCustomStaticTextClass = class of TWSCustomStaticText;
  TWSCustomStaticText = class(TWSWinControl_CallWS)
  private class var
    FWSCustomStaticText: TWSCustomStaticTextClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
    class procedure SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment); virtual;
    class procedure SetStaticBorderStyle(const ACustomStaticText: TCustomStaticText; const NewBorderStyle: TStaticBorderStyle); virtual;
    class function GetDefaultColor(const AControl: TControl;
      const ADefaultColorType: TDefaultColorType): TColor; override;
  end;

  { TWSStaticText }

  TWSStaticText = class(TWSCustomStaticText)
  published
  end;

  { TWSButtonControl }

  TWSButtonControlClass = class of TWSButtonControl;
  TWSButtonControl = class(TWSWinControl_CallWS)
  private class var
    FWSButtonControl_Impl: TWSButtonControlClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
    class function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; override;
  end;

  { TWSButtonControl_CallWS }

  TWSButtonControl_CallWS = class(TWSButtonControl)
  public
    class procedure GetPreferredSize(const AWinControl: TWinControl;
          var PreferredWidth, PreferredHeight: integer;
          WithThemeSpace: Boolean); override;
  end;

  { TWSButton }

  TWSButtonClass = class of TWSButton;
  TWSButton = class(TWSButtonControl_CallWS)
  private class var
    FWSButton_Impl: TWSButtonClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
    class procedure SetDefault(const AButton: TCustomButton; ADefault: Boolean); virtual;
    class procedure SetShortCut(const AButton: TCustomButton; const ShortCutK1, ShortCutK2: TShortCut); virtual;
  end;

  { TWSButton_CallWS }

  TWSButton_CallWS = class(TWSButton)
  public
    class procedure SetDefault(const AButton: TCustomButton; ADefault: Boolean); override;
    class procedure SetShortCut(const AButton: TCustomButton; const ShortCutK1, ShortCutK2: TShortCut); override;
  end;

  { TWSCustomCheckBox }

  TWSCustomCheckBoxClass = class of TWSCustomCheckBox;
  TWSCustomCheckBox = class(TWSButtonControl_CallWS)
  private class var
    FWSCustomCheckBox_Impl: TWSCustomCheckBoxClass;
  public
    class function GetImplementation: TWSObjectClass; override;
    class procedure SetImplementation(AImpl: TWSObjectClass); override;
    class function  RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; virtual;
    class procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox; const ShortCutK1, ShortCutK2: TShortCut); virtual;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); virtual;
    class procedure SetAlignment(const ACustomCheckBox: TCustomCheckBox; const NewAlignment: TLeftRight); virtual;
  end;

  { TWSCheckBox }

  TWSCheckBox = class(TWSCustomCheckBox)
  published
  end;

  { TWSToggleBox }

  TWSToggleBox = class(TWSCustomCheckBox)
  published
  end;

  { TWSRadioButton }

  TWSRadioButton = class(TWSCustomCheckBox)
  published
  end;

  { WidgetSetRegistration }

  procedure RegisterCustomScrollBar;
  procedure RegisterCustomGroupBox;
  procedure RegisterCustomComboBox;
  procedure RegisterCustomListBox;
  procedure RegisterCustomEdit;
  procedure RegisterCustomMemo;
  procedure RegisterButtonControl;
  procedure RegisterCustomButton;
  procedure RegisterCustomCheckBox;
  procedure RegisterToggleBox;
  procedure RegisterRadioButton;
  procedure RegisterCustomStaticText;
  procedure RegisterCustomLabel;

implementation

uses
  LResources;

{ TWSGroupBox }

class function TWSGroupBox.GetImplementation: TWSObjectClass;
begin
  Result:= FWSGroupBox_Impl;
end;

class procedure TWSGroupBox.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSGroupBox_Impl := TWSGroupBoxClass(AImpl);
end;

class function TWSGroupBox.GetDefaultColor(const AControl: TControl;
  const ADefaultColorType: TDefaultColorType): TColor;
begin
  Result:=DefBtnColors[ADefaultColorType];
end;

{ TWSScrollBar }

class function TWSScrollBar.GetImplementation: TWSObjectClass;
begin
  Result := FWSScrollBar_Impl;
end;

class procedure TWSScrollBar.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSScrollBar_Impl := TWSScrollBarClass(AImpl);
end;

class procedure TWSScrollBar.SetParams(const AScrollBar: TCustomScrollBar);
begin
end;

class procedure TWSScrollBar.SetKind(const AScrollBar: TCustomScrollBar;
  const AIsHorizontal: Boolean);
begin
  RecreateWnd(AScrollBar);
end;

{ TWSCustomListBox }

class function TWSCustomListBox.GetImplementation: TWSObjectClass;
begin
  Result:= FWSCustomListBox_Impl;
end;

class procedure TWSCustomListBox.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSCustomListBox_Impl := TWSCustomListBoxClass(AImpl);
end;

class procedure TWSCustomListBox.DragStart(const ACustomListBox: TCustomListBox);
begin
end;

class function TWSCustomListBox.GetIndexAtXY(
  const ACustomListBox: TCustomListBox; X, Y: integer): integer;
begin
  Result := -1;
end;

class function  TWSCustomListBox.GetItemIndex(const ACustomListBox: TCustomListBox): integer;
begin
  Result := 0;
end;

class function TWSCustomListBox.GetItemRect(
  const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect
  ): boolean;
begin
  FillChar(ARect,SizeOf(ARect),0);
  Result:=false;
end;

class function TWSCustomListBox.GetScrollWidth(
  const ACustomListBox: TCustomListBox): Integer;
begin
  Result := 0;
end;

class function  TWSCustomListBox.GetSelCount(const ACustomListBox: TCustomListBox): integer;
begin
  Result := 0;
end;

class function  TWSCustomListBox.GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean;
begin
  Result := false;
end;

class function  TWSCustomListBox.GetStrings(const ACustomListBox: TCustomListBox): TStrings;
begin
  Result := nil;
end;

class procedure TWSCustomListBox.FreeStrings(var AStrings: TStrings);
begin
  AStrings.Free;
  AStrings := nil;
end;

class function  TWSCustomListBox.GetTopIndex(const ACustomListBox: TCustomListBox): integer;
begin
  Result := 0;
end;

class procedure TWSCustomListBox.SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean);
begin
end;

class procedure TWSCustomListBox.SetBorder(const ACustomListBox: TCustomListBox);
begin
end;

class procedure TWSCustomListBox.SetColumnCount(const ACustomListBox: TCustomListBox;
  ACount: Integer);
begin
end;

class procedure TWSCustomListBox.SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer);
begin
end;

class procedure TWSCustomListBox.SetScrollWidth(
  const ACustomListBox: TCustomListBox; const AScrollWidth: Integer);
begin

end;

class procedure TWSCustomListBox.SetSelectionMode(const ACustomListBox: TCustomListBox;
  const AExtendedSelect, AMultiSelect: boolean);
begin
end;

class procedure TWSCustomListBox.SetStyle(const ACustomListBox: TCustomListBox);
begin
end;

class procedure TWSCustomListBox.SetSorted(const ACustomListBox: TCustomListBox;
  AList: TStrings; ASorted: boolean);
begin
end;

class procedure TWSCustomListBox.SetTopIndex(const ACustomListBox: TCustomListBox;
  const NewTopIndex: integer);
begin
end;

{ TWSCustomListBox_CallWS }

class procedure TWSCustomListBox_CallWS.DragStart(
  const ACustomListBox: TCustomListBox);
begin
  FWSCustomListBox_Impl.DragStart(ACustomListBox);
end;

class function TWSCustomListBox_CallWS.GetIndexAtXY(
  const ACustomListBox: TCustomListBox; X, Y: integer): integer;
begin
  Result:= FWSCustomListBox_Impl.GetIndexAtXY(ACustomListBox, X, Y);
end;

class function TWSCustomListBox_CallWS.GetItemIndex(
  const ACustomListBox: TCustomListBox): integer;
begin
  Result:= FWSCustomListBox_Impl.GetItemIndex(ACustomListBox);
end;

class function TWSCustomListBox_CallWS.GetItemRect(
  const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect
  ): boolean;
begin
  Result:= FWSCustomListBox_Impl.GetItemRect(ACustomListBox, Index, ARect);
end;

class function TWSCustomListBox_CallWS.GetScrollWidth(
  const ACustomListBox: TCustomListBox): Integer;
begin
  Result:= FWSCustomListBox_Impl.GetScrollWidth(ACustomListBox);
end;

class function TWSCustomListBox_CallWS.GetSelCount(
  const ACustomListBox: TCustomListBox): integer;
begin
  Result:= FWSCustomListBox_Impl.GetSelCount(ACustomListBox);
end;

class function TWSCustomListBox_CallWS.GetSelected(
  const ACustomListBox: TCustomListBox; const AIndex: integer): boolean;
begin
  Result:= FWSCustomListBox_Impl.GetSelected(ACustomListBox, AIndex);
end;

class function TWSCustomListBox_CallWS.GetStrings(
  const ACustomListBox: TCustomListBox): TStrings;
begin
  Result:= FWSCustomListBox_Impl.GetStrings(ACustomListBox);
end;

class procedure TWSCustomListBox_CallWS.FreeStrings(var AStrings: TStrings);
begin
  FWSCustomListBox_Impl.FreeStrings(AStrings);
end;

class function TWSCustomListBox_CallWS.GetTopIndex(
  const ACustomListBox: TCustomListBox): integer;
begin
  Result:= FWSCustomListBox_Impl.GetTopIndex(ACustomListBox);
end;

class procedure TWSCustomListBox_CallWS.SelectItem(
  const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean);
begin
  FWSCustomListBox_Impl.SelectItem(ACustomListBox, AIndex, ASelected);
end;

class procedure TWSCustomListBox_CallWS.SetBorder(
  const ACustomListBox: TCustomListBox);
begin
  FWSCustomListBox_Impl.SetBorder(ACustomListBox);
end;

class procedure TWSCustomListBox_CallWS.SetColumnCount(
  const ACustomListBox: TCustomListBox; ACount: Integer);
begin
  FWSCustomListBox_Impl.SetColumnCount(ACustomListBox, ACount);
end;

class procedure TWSCustomListBox_CallWS.SetItemIndex(
  const ACustomListBox: TCustomListBox; const AIndex: integer);
begin
  FWSCustomListBox_Impl.SetItemIndex(ACustomListBox, AIndex);
end;

class procedure TWSCustomListBox_CallWS.SetScrollWidth(
  const ACustomListBox: TCustomListBox; const AScrollWidth: Integer);
begin
  FWSCustomListBox_Impl.SetScrollWidth(ACustomListBox, AScrollWidth);
end;

class procedure TWSCustomListBox_CallWS.SetSelectionMode(
  const ACustomListBox: TCustomListBox; const AExtendedSelect,
  AMultiSelect: boolean);
begin
  FWSCustomListBox_Impl.SetSelectionMode(ACustomListBox, AExtendedSelect, AMultiSelect);
end;

class procedure TWSCustomListBox_CallWS.SetStyle(
  const ACustomListBox: TCustomListBox);
begin
  FWSCustomListBox_Impl.SetStyle(ACustomListBox);
end;

class procedure TWSCustomListBox_CallWS.SetSorted(
  const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean);
begin
  FWSCustomListBox_Impl.SetSorted(ACustomListBox, AList, ASorted);
end;

class procedure TWSCustomListBox_CallWS.SetTopIndex(
  const ACustomListBox: TCustomListBox; const NewTopIndex: integer);
begin
  FWSCustomListBox_Impl.SetTopIndex(ACustomListBox, NewTopIndex);
end;

{ TWSCustomComboBox }

class function TWSCustomComboBox.GetImplementation: TWSObjectClass;
begin
  Result:= FWSCustomComboBox_Impl;
end;

class procedure TWSCustomComboBox.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSCustomComboBox_Impl := TWSCustomComboBoxClass(AImpl);
end;

class function TWSCustomComboBox.GetDroppedDown(
  const ACustomComboBox: TCustomComboBox): Boolean;
begin
  Result := False;
end;

class function  TWSCustomComboBox.GetSelStart(const ACustomComboBox: TCustomComboBox
  ): integer;
begin
  Result := -1;
end;

class function  TWSCustomComboBox.GetSelLength(const ACustomComboBox: TCustomComboBox
  ): integer;
begin
  Result := 0;
end;

class function  TWSCustomComboBox.GetItemIndex(const ACustomComboBox: TCustomComboBox
  ): integer;
begin
  Result := -1;
end;

class function  TWSCustomComboBox.GetMaxLength(const ACustomComboBox: TCustomComboBox
  ): integer;
begin
  Result := 0;
end;

class procedure TWSCustomComboBox.SetArrowKeysTraverseList(
  const ACustomComboBox: TCustomComboBox; NewTraverseList: boolean);
begin
end;

class procedure TWSCustomComboBox.SetDropDownCount(
  const ACustomComboBox: TCustomComboBox; NewCount: Integer);
begin
end;

class procedure TWSCustomComboBox.SetDroppedDown(
  const ACustomComboBox: TCustomComboBox; ADroppedDown: Boolean);
begin
end;

class procedure TWSCustomComboBox.SetMaxLength(const ACustomComboBox: TCustomComboBox;
  NewLength: integer);
begin
end;

class procedure TWSCustomComboBox.SetSelStart(const ACustomComboBox: TCustomComboBox;
  NewStart: integer);
begin
end;

class procedure TWSCustomComboBox.SetSelLength(const ACustomComboBox: TCustomComboBox;
  NewLength: integer);
begin
end;

class procedure TWSCustomComboBox.SetItemIndex(const ACustomComboBox: TCustomComboBox;
  NewIndex: integer);
begin
end;

class procedure TWSCustomComboBox.SetStyle(const ACustomComboBox: TCustomComboBox;
  NewStyle: TComboBoxStyle);
begin
end;

class procedure TWSCustomComboBox.SetReadOnly(const ACustomComboBox: TCustomComboBox;
  NewReadOnly: boolean);
begin
end;

class function  TWSCustomComboBox.GetItems(const ACustomComboBox: TCustomComboBox
  ): TStrings;
begin
  Result := nil;
end;

class procedure TWSCustomComboBox.FreeItems(var AItems: TStrings);
begin
  AItems.Free;
  AItems := nil;
end;

class procedure TWSCustomComboBox.Sort(const ACustomComboBox: TCustomComboBox;
  AList: TStrings; IsSorted: boolean);
begin
end;

class function TWSCustomComboBox.GetItemHeight(const ACustomComboBox: TCustomComboBox): Integer;
begin
  Result := 0;
end;

class procedure TWSCustomComboBox.SetItemHeight(const ACustomComboBox: TCustomComboBox; const AItemHeight: Integer);
begin
end;

{ TWSCustomEdit }

class function TWSCustomEdit.GetImplementation: TWSObjectClass;
begin
  Result:= FWSCustomEdit_Impl;
end;

class procedure TWSCustomEdit.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSCustomEdit_Impl := TWSCustomEditClass(AImpl);
end;

class function TWSCustomEdit.GetCanUndo(const ACustomEdit: TCustomEdit): Boolean;
begin
  Result := False;
end;

class function TWSCustomEdit.GetCaretPos(const ACustomEdit: TCustomEdit): TPoint;
begin
  Result := Point(0, 0);
end;

class function  TWSCustomEdit.GetSelStart(const ACustomEdit: TCustomEdit): integer;
begin
  result := -1;
end;

class function  TWSCustomEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
begin
  result := 0;
end;

class procedure TWSCustomEdit.SetAlignment(const ACustomEdit: TCustomEdit;
  const AAlignment: TAlignment);
begin
end;

class procedure TWSCustomEdit.SetCaretPos(const ACustomEdit: TCustomEdit; const NewPos: TPoint);
begin
end;

class procedure TWSCustomEdit.SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase);
begin
end;

class procedure TWSCustomEdit.SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode);
begin
end;

class procedure TWSCustomEdit.SetHideSelection(const ACustomEdit: TCustomEdit;
  NewHideSelection: Boolean);
begin
end;

class procedure TWSCustomEdit.SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer);
begin
end;

class procedure TWSCustomEdit.SetNumbersOnly(const ACustomEdit: TCustomEdit;
  NewNumbersOnly: Boolean);
begin
end;

class procedure TWSCustomEdit.SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char);
begin
end;

class procedure TWSCustomEdit.SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean);
begin
end;

class procedure TWSCustomEdit.SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer);
begin
end;

class procedure TWSCustomEdit.SetTextHint(const ACustomEdit: TCustomEdit;
  const ATextHint: string);
begin
end;

class procedure TWSCustomEdit.SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer);
begin
end;

class procedure TWSCustomEdit.SetSelText(const ACustomEdit: TCustomEdit;
  const NewSelText: string);
var
  OldText, NewText: string;
  OldPos: Integer;
begin
  OldPos := ACustomEdit.SelStart;
  OldText := ACustomEdit.Text;
  NewText := UTF8Copy(OldText, 1, OldPos) +
             NewSelText +
             UTF8Copy(OldText, OldPos + ACustomEdit.SelLength + 1, MaxInt);
  ACustomEdit.Text := NewText;
  ACustomEdit.SelStart := OldPos + UTF8Length(NewSelText);
end;

class procedure TWSCustomEdit.Cut(const ACustomEdit: TCustomEdit);
begin
  ACustomEdit.CopyToClipboard;
  ACustomEdit.ClearSelection;
end;

class procedure TWSCustomEdit.Copy(const ACustomEdit: TCustomEdit);
begin
  if (ACustomEdit.EchoMode = emNormal) and (ACustomEdit.SelLength > 0) then
    Clipboard.AsText := ACustomEdit.SelText;
end;

class function TWSCustomEdit.CreateEmulatedTextHintFont(
  const ACustomEdit: TCustomEdit): TFont;
begin
  Result := TFont.Create;
  try
    Result.Assign(ACustomEdit.Font);
    Result.Color := clGrayText;
  except
    Result.Free;
    Result := nil;
    raise;
  end;
end;

class procedure TWSCustomEdit.Paste(const ACustomEdit: TCustomEdit);
begin
  if Clipboard.HasFormat(CF_TEXT) then
    ACustomEdit.SelText := Clipboard.AsText;
end;

class procedure TWSCustomEdit.Undo(const ACustomEdit: TCustomEdit);
begin
  // nothing
end;

{ TWSCustomEdit_CallWS }

class function TWSCustomEdit_CallWS.GetCanUndo(const ACustomEdit: TCustomEdit): Boolean;
begin
  Result := FWSCustomEdit_Impl.GetCanUndo(ACustomEdit);
end;

class function TWSCustomEdit_CallWS.GetCaretPos(const ACustomEdit: TCustomEdit): TPoint;
begin
  Result:= FWSCustomEdit_Impl.GetCaretPos(ACustomEdit);
end;

class function TWSCustomEdit_CallWS.GetSelStart(const ACustomEdit: TCustomEdit): integer;
begin
  Result:= FWSCustomEdit_Impl.GetSelStart(ACustomEdit);
end;

class function TWSCustomEdit_CallWS.GetSelLength(const ACustomEdit: TCustomEdit): integer;
begin
  Result:= FWSCustomEdit_Impl.GetSelLength(ACustomEdit);
end;

class procedure TWSCustomEdit_CallWS.SetAlignment(
  const ACustomEdit: TCustomEdit; const AAlignment: TAlignment);
begin
  FWSCustomEdit_Impl.SetAlignment(ACustomEdit, AAlignment);
end;

class procedure TWSCustomEdit_CallWS.SetCaretPos(
  const ACustomEdit: TCustomEdit; const NewPos: TPoint);
begin
  FWSCustomEdit_Impl.SetCaretPos(ACustomEdit, NewPos);
end;

class procedure TWSCustomEdit_CallWS.SetCharCase(
  const ACustomEdit: TCustomEdit; NewCase: TEditCharCase);
begin
  FWSCustomEdit_Impl.SetCharCase(ACustomEdit, NewCase);
end;

class procedure TWSCustomEdit_CallWS.SetEchoMode(
  const ACustomEdit: TCustomEdit; NewMode: TEchoMode);
begin
  FWSCustomEdit_Impl.SetEchoMode(ACustomEdit, NewMode);
end;

class procedure TWSCustomEdit_CallWS.SetHideSelection(
  const ACustomEdit: TCustomEdit; NewHideSelection: Boolean);
begin
  FWSCustomEdit_Impl.SetHideSelection(ACustomEdit, NewHideSelection);
end;

class procedure TWSCustomEdit_CallWS.SetMaxLength(
  const ACustomEdit: TCustomEdit; NewLength: integer);
begin
  FWSCustomEdit_Impl.SetMaxLength(ACustomEdit, NewLength);
end;

class procedure TWSCustomEdit_CallWS.SetNumbersOnly(
  const ACustomEdit: TCustomEdit; NewNumbersOnly: Boolean);
begin
  FWSCustomEdit_Impl.SetNumbersOnly(ACustomEdit, NewNumbersOnly);
end;

class procedure TWSCustomEdit_CallWS.SetPasswordChar(
  const ACustomEdit: TCustomEdit; NewChar: char);
begin
  FWSCustomEdit_Impl.SetPasswordChar(ACustomEdit, NewChar);
end;

class procedure TWSCustomEdit_CallWS.SetReadOnly(
  const ACustomEdit: TCustomEdit; NewReadOnly: boolean);
begin
  FWSCustomEdit_Impl.SetReadOnly(ACustomEdit, NewReadOnly);
end;

class procedure TWSCustomEdit_CallWS.SetSelStart(
  const ACustomEdit: TCustomEdit; NewStart: integer);
begin
  FWSCustomEdit_Impl.SetSelStart(ACustomEdit, NewStart);
end;

class procedure TWSCustomEdit_CallWS.SetSelLength(
  const ACustomEdit: TCustomEdit; NewLength: integer);
begin
  FWSCustomEdit_Impl.SetSelLength(ACustomEdit, NewLength);
end;

class procedure TWSCustomEdit_CallWS.SetSelText(const ACustomEdit: TCustomEdit;
  const NewSelText: string);
begin
  FWSCustomEdit_Impl.SetSelText(ACustomEdit, NewSelText);
end;

class procedure TWSCustomEdit_CallWS.SetTextHint(
  const ACustomEdit: TCustomEdit; const ATextHint: string);
begin
  FWSCustomEdit_Impl.SetTextHint(ACustomEdit, ATextHint);
end;

class function TWSCustomEdit_CallWS.CreateEmulatedTextHintFont(
  const ACustomEdit: TCustomEdit): TFont;
begin
  Result:= FWSCustomEdit_Impl.CreateEmulatedTextHintFont(ACustomEdit);
end;

class procedure TWSCustomEdit_CallWS.Cut(const ACustomEdit: TCustomEdit);
begin
  FWSCustomEdit_Impl.Cut(ACustomEdit);
end;

class procedure TWSCustomEdit_CallWS.Copy(const ACustomEdit: TCustomEdit);
begin
  FWSCustomEdit_Impl.Copy(ACustomEdit);
end;

class procedure TWSCustomEdit_CallWS.Paste(const ACustomEdit: TCustomEdit);
begin
  FWSCustomEdit_Impl.Paste(ACustomEdit);
end;

class procedure TWSCustomEdit_CallWS.Undo(const ACustomEdit: TCustomEdit);
begin
  FWSCustomEdit_Impl.Undo(ACustomEdit);
end;

class procedure TWSCustomEdit_CallWS.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  FWSCustomEdit_Impl.GetPreferredSize(AWinControl, PreferredWidth, PreferredHeight,
    WithThemeSpace);
end;

{ TWSCustomMemo }

class function TWSCustomMemo.GetImplementation: TWSObjectClass;
begin
  Result:= FWSCustomMemo_Impl;
end;

class procedure TWSCustomMemo.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSCustomMemo_Impl := TWSCustomMemoClass(AImpl);
end;

class procedure TWSCustomMemo.AppendText(const ACustomMemo: TCustomMemo; const AText: string);
begin
end;

class function TWSCustomMemo.GetStrings(const ACustomMemo: TCustomMemo): TStrings;
begin
  Result := ACustomMemo.Lines; //use default if the WS has not defined any
end;

class procedure TWSCustomMemo.FreeStrings(var AStrings: TStrings);
begin
  AStrings.Free;
  AStrings := nil;
end;

class procedure TWSCustomMemo.SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle);
begin
end;

class procedure TWSCustomMemo.SetSelText(const ACustomEdit: TCustomEdit;
  const NewSelText: string);
begin
  TCustomMemo(ACustomEdit).Lines.BeginUpdate;
  try
    TWSCustomEdit.SetSelText(ACustomEdit, NewSelText);
  finally
    TCustomMemo(ACustomEdit).Lines.EndUpdate;
  end;
end;

class procedure TWSCustomMemo.SetWantTabs(const ACustomMemo: TCustomMemo; const NewWantTabs: boolean);
begin
end;

class procedure TWSCustomMemo.SetWantReturns(const ACustomMemo: TCustomMemo; const NewWantReturns: boolean);
begin
end;

class procedure TWSCustomMemo.SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean);
begin
end;

{ TWSCustomStaticText }

class function TWSCustomStaticText.GetImplementation: TWSObjectClass;
begin
  Result:= FWSCustomStaticText;
end;

class procedure TWSCustomStaticText.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSCustomStaticText := TWSCustomStaticTextClass(AImpl);
end;

class procedure TWSCustomStaticText.SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment);
begin
end;

class procedure TWSCustomStaticText.SetStaticBorderStyle(
  const ACustomStaticText: TCustomStaticText;
  const NewBorderStyle: TStaticBorderStyle);
begin
  // nothing
end;

class function TWSCustomStaticText.GetDefaultColor(const AControl: TControl;
  const ADefaultColorType: TDefaultColorType): TColor;
begin
  Result:=DefBtnColors[ADefaultColorType];
end;

{ TWSButton }

class function TWSButton.GetImplementation: TWSObjectClass;
begin
  Result:= FWSButton_Impl;
end;

class procedure TWSButton.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSButton_Impl := TWSButtonClass(AImpl);
end;

class procedure TWSButton.SetDefault(const AButton: TCustomButton; ADefault: Boolean);
begin
end;

class procedure TWSButton.SetShortCut(const AButton: TCustomButton;
  const ShortCutK1, ShortCutK2: TShortCut);
begin;
end;

{ TWSButton_CallWS }

class procedure TWSButton_CallWS.SetDefault(const AButton: TCustomButton;
  ADefault: Boolean);
begin
  FWSButton_Impl.SetDefault(AButton, ADefault);
end;

class procedure TWSButton_CallWS.SetShortCut(const AButton: TCustomButton;
  const ShortCutK1, ShortCutK2: TShortCut);
begin
  FWSButton_Impl.SetShortCut(AButton, ShortCutK1, ShortCutK2);
end;

{ TWSCustomCheckBox }

class function TWSCustomCheckBox.GetImplementation: TWSObjectClass;
begin
  Result:= FWSCustomCheckBox_Impl;
end;

class procedure TWSCustomCheckBox.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSCustomCheckBox_Impl := TWSCustomCheckBoxClass(AImpl);
end;

class function  TWSCustomCheckBox.RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
begin
  Result := cbUnchecked;
end;

class procedure TWSCustomCheckBox.SetShortCut(const ACustomCheckBox: TCustomCheckBox;
  const ShortCutK1, ShortCutK2: TShortCut);
begin
end;

class procedure TWSCustomCheckBox.SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
begin
end;

class procedure TWSCustomCheckBox.SetAlignment(
  const ACustomCheckBox: TCustomCheckBox; const NewAlignment: TLeftRight);
begin
end;

{ WidgetSetRegistration }

procedure RegisterCustomScrollBar;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomScrollBar;
//  if not WSRegisterCustomScrollBar then
//    RegisterWSComponent(TCustomScrollBar, TWSCustomScrollBar);
  Done := True;
end;

procedure RegisterCustomGroupBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomGroupBox;
//  if not WSRegisterCustomGroupBox then
//    RegisterWSComponent(TCustomGroupBox, TWSCustomGroupBox);
  Done := True;
end;

procedure RegisterCustomComboBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomComboBox;
//  if not WSRegisterCustomComboBox then
//    RegisterWSComponent(TCustomComboBox, TWSCustomComboBox);
  Done := True;
end;

procedure RegisterCustomListBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomListBox;
//  if not WSRegisterCustomListBox then
//    RegisterWSComponent(TCustomListBox, TWSCustomListBox);
  Done := True;
end;

procedure RegisterCustomEdit;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomEdit;
//  if not WSRegisterCustomEdit then
//    RegisterWSComponent(TCustomEdit, TWSCustomEdit);
  Done := True;
end;

procedure RegisterCustomMemo;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomMemo;
  RegisterPropertyToSkip(TCustomMemo, 'BevelInner', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TCustomMemo, 'BevelOuter', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TCustomMemo, 'BevelEdges', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TCustomMemo, 'Margins',    'VCL compatibility property', '');
  RegisterPropertyToSkip(TCustomMemo, 'DoubleBuffered', 'VCL compatibility property', '');
//  if not WSRegisterCustomMemo then
//    RegisterWSComponent(TCustomMemo, TWSCustomMemo);
  Done := True;
end;

procedure RegisterButtonControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterButtonControl;
  RegisterPropertyToSkip(TButtonControl, 'UseOnChange',
    'Removed in 0.9.27. It was an old workaround which is not needed anymore.',
    '');
//  if not WSRegisterButtonControl then
//    RegisterWSComponent(TButtonControl, TWSButtonControl);
  Done := True;
end;

procedure RegisterCustomButton;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomButton;
//  if not WSRegisterCustomButton then
//    RegisterWSComponent(TCustomButton, TWSButton);
  Done := True;
end;

procedure RegisterCustomCheckBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomCheckBox;
  RegisterPropertyToSkip(TCustomCheckBox, 'Alignment', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TCustomCheckBox, 'WordWrap', 'VCL compatibility property', '');
//  if not WSRegisterCustomCheckBox then
//    RegisterWSComponent(TCustomCheckBox, TWSCustomCheckBox);
  Done := True;
end;

procedure RegisterToggleBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterToggleBox;
//  if not WSRegisterToggleBox then
//    RegisterWSComponent(TToggleBox, TWSToggleBox);
  Done := True;
end;

procedure RegisterRadioButton;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterRadioButton;
  RegisterPropertyToSkip(TRadioButton, 'State', 'Removed in 0.9.29. It should not be allowed to set the State directly', '');
  RegisterPropertyToSkip(TRadioButton, 'AllowGrayed', 'Removed in 0.9.29. Grayed state is not supported by TRadioButton', '');
//  if not WSRegisterRadioButton then
//    RegisterWSComponent(TRadioButton, TWSRadioButton);
  Done := True;
end;

procedure RegisterCustomStaticText;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomStaticText;
//  if not WSRegisterCustomStaticText then
//    RegisterWSComponent(TCustomStaticText, TWSCustomStaticText);
  Done := True;
end;

procedure RegisterCustomLabel;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomLabel;
//  if not WSRegisterCustomLabel then
//    RegisterWSComponent(TCustomLabel, TWSCustomLabel);
  Done := True;
end;

{ TWSButtonControl }

class function TWSButtonControl.GetImplementation: TWSObjectClass;
begin
  Result:= FWSButtonControl_Impl;
end;

class procedure TWSButtonControl.SetImplementation(AImpl: TWSObjectClass);
begin
  FWSButtonControl_Impl := TWSButtonControlClass(AImpl);
end;

class function TWSButtonControl.GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor;
begin
  Result := DefBtnColors[ADefaultColorType];
end;

{ TWSButtonControl_CallWS }

class procedure TWSButtonControl_CallWS.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  FWSButtonControl_Impl.GetPreferredSize(AWinControl, PreferredWidth, PreferredHeight,
    WithThemeSpace);
end;

end.
