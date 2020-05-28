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

  TWSScrollBar = class(TWSWinControl)
  published
    class procedure SetParams(const AScrollBar: TCustomScrollBar); virtual;
    class procedure SetKind(const AScrollBar: TCustomScrollBar; const AIsHorizontal: Boolean); virtual;
  end;
  TWSScrollBarClass = class of TWSScrollBar;

  { TWSCustomGroupBox }

  TWSCustomGroupBox = class(TWSCustomControl)
  published
  end;

  { TWSGroupBox }

  TWSGroupBox = class(TWSCustomGroupBox)
  published
    class function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; override;
  end;

  { TWSCustomComboBox }

  TWSCustomComboBox = class(TWSWinControl)
  published
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
  TWSCustomComboBoxClass = class of TWSCustomComboBox;

  { TWSComboBox }

  TWSComboBox = class(TWSCustomComboBox)
  published
  end;

  { TWSCustomListBox }

  TWSCustomListBox = class(TWSWinControl)
  published
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
  TWSCustomListBoxClass = class of TWSCustomListBox;
  
  { TWSListBox }

  TWSListBox = class(TWSCustomListBox)
  published
  end;

  { TWSCustomEdit }

  TWSCustomEdit = class(TWSWinControl)
  published
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
  TWSCustomEditClass = class of TWSCustomEdit;

  { TWSCustomMemo }

  TWSCustomMemo = class(TWSCustomEdit)
  published
    class procedure AppendText(const ACustomMemo: TCustomMemo; const AText: string); virtual;
    class function  GetStrings(const ACustomMemo: TCustomMemo): TStrings; virtual;
    class procedure FreeStrings(var AStrings: TStrings); virtual;
    class procedure SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle); virtual;
    class procedure SetWantTabs(const ACustomMemo: TCustomMemo; const NewWantTabs: boolean); virtual;
    class procedure SetWantReturns(const ACustomMemo: TCustomMemo; const NewWantReturns: boolean); virtual;
    class procedure SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean); virtual;
    class procedure SetSelText(const ACustomEdit: TCustomEdit; const NewSelText: string); override;
  end;
  TWSCustomMemoClass = class of TWSCustomMemo;

  { TWSEdit }

  TWSEdit = class(TWSCustomEdit)
  published
  end;

  { TWSMemo }

  TWSMemo = class(TWSCustomMemo)
  published
  end;

  { TWSCustomStaticText }

  TWSCustomStaticTextClass = class of TWSCustomStaticText;
  TWSCustomStaticText = class(TWSWinControl)
  published
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

  TWSButtonControl = class(TWSWinControl)
  published
    class function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; override;
  end;

  { TWSButton }

  TWSButton = class(TWSButtonControl)
  published
    class procedure SetDefault(const AButton: TCustomButton; ADefault: Boolean); virtual;
    class procedure SetShortCut(const AButton: TCustomButton; const ShortCutK1, ShortCutK2: TShortCut); virtual;
  end;
  TWSButtonClass = class of TWSButton;

  { TWSCustomCheckBox }

  TWSCustomCheckBox = class(TWSButtonControl)
  published
    class function  RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; virtual;
    class procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox; const ShortCutK1, ShortCutK2: TShortCut); virtual;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); virtual;
    class procedure SetAlignment(const ACustomCheckBox: TCustomCheckBox; const NewAlignment: TLeftRight); virtual;
  end;
  TWSCustomCheckBoxClass = class of TWSCustomCheckBox;

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

class function TWSGroupBox.GetDefaultColor(const AControl: TControl;
  const ADefaultColorType: TDefaultColorType): TColor;
begin
  Result:=DefBtnColors[ADefaultColorType];
end;

{ TWSScrollBar }

class procedure TWSScrollBar.SetParams(const AScrollBar: TCustomScrollBar);
begin
end;

class procedure TWSScrollBar.SetKind(const AScrollBar: TCustomScrollBar;
  const AIsHorizontal: Boolean);
begin
  RecreateWnd(AScrollBar);
end;

{ TWSCustomListBox }

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

{ TWSCustomComboBox }

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

class function TWSCustomEdit.GetCanUndo(const ACustomEdit: TCustomEdit
  ): Boolean;
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

{ TWSCustomMemo }

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

class procedure TWSButton.SetDefault(const AButton: TCustomButton; ADefault: Boolean);
begin
end;

class procedure TWSButton.SetShortCut(const AButton: TCustomButton;
  const ShortCutK1, ShortCutK2: TShortCut);
begin;
end;

{ TWSCustomCheckBox }

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

class function TWSButtonControl.GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor;
begin
  Result := DefBtnColors[ADefaultColorType];
end;

end.
