{
 *****************************************************************************
 *                               Gtk3WSStdCtrls.pp                           *
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
unit Gtk3WSStdCtrls;

{$mode objfpc}{$H+}
{$I gtk3defines.inc}

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
  Graphics, Controls, StdCtrls, LCLType, LCLProc,
////////////////////////////////////////////////////
  WSLCLClasses, WSControls, WSStdCtrls, WSProc, Classes, WSFactory, Clipbrd,
  gtk3widgets, gtk3procs, LazGdk3;

type
  { TGtk3WSScrollBar }

  TGtk3WSScrollBar = class(TWSScrollBar)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetParams(const AScrollBar: TCustomScrollBar); override;
    class procedure SetKind(const AScrollBar: TCustomScrollBar; const AIsHorizontal: Boolean); override;
  end;
  TGtk3WSScrollBarClass = class of TGtk3WSScrollBar;

  { TGtk3WSCustomGroupBox }

  TGtk3WSCustomGroupBox = class(TWSCustomGroupBox)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;
  end;

  { TGtk3WSGroupBox }

  TGtk3WSGroupBox = class(TGtk3WSCustomGroupBox)
  published
  end;

  { TGtk3WSCustomComboBox }

  TGtk3WSCustomComboBox = class(TWSCustomComboBox)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;

    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;

    class function GetDroppedDown(const ACustomComboBox: TCustomComboBox): Boolean; override;
    class function GetSelStart(const ACustomComboBox: TCustomComboBox): integer; override;
    class function GetSelLength(const ACustomComboBox: TCustomComboBox): integer; override;
    class function GetItemIndex(const ACustomComboBox: TCustomComboBox): integer; override;
    class function GetMaxLength(const ACustomComboBox: TCustomComboBox): integer; override;
    
    class procedure SetArrowKeysTraverseList(const ACustomComboBox: TCustomComboBox; 
      NewTraverseList: boolean); override;
    class procedure SetDropDownCount(const ACustomComboBox: TCustomComboBox; NewCount: Integer); override;
    class procedure SetDroppedDown(const ACustomComboBox: TCustomComboBox; ADroppedDown: Boolean); override;
    class procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); override;
    class procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer); override;
    class procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle); override;

    class function  GetItems(const ACustomComboBox: TCustomComboBox): TStrings; override;
    class procedure Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean); override;
    
    class function GetItemHeight(const ACustomComboBox: TCustomComboBox): Integer; override;
    class procedure SetItemHeight(const ACustomComboBox: TCustomComboBox; const AItemHeight: Integer); override;
  end;
  TGtk3WSCustomComboBoxClass = class of TGtk3WSCustomComboBox;

  { TGtk3WSComboBox }

  TGtk3WSComboBox = class(TGtk3WSCustomComboBox)
  published
  end;

  { TGtk3WSCustomListBox }

  TGtk3WSCustomListBox = class(TWSCustomListBox)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
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
    class procedure SetSelectionMode(const ACustomListBox: TCustomListBox; const AExtendedSelect, 
      AMultiSelect: boolean); override;
    class procedure SetStyle(const ACustomListBox: TCustomListBox); override;
    class procedure SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean); override;
    class procedure SetTopIndex(const ACustomListBox: TCustomListBox; const NewTopIndex: integer); override;
  end;
  TGtk3WSCustomListBoxClass = class of TGtk3WSCustomListBox;
  
  { TWSListBox }

  TGtk3WSListBox = class(TGtk3WSCustomListBox)
  published
  end;

  { TGtk3WSCustomEdit }

  TGtk3WSCustomEdit = class(TWSCustomEdit)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;

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
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;

    class procedure Cut(const ACustomEdit: TCustomEdit); override;
    class procedure Copy(const ACustomEdit: TCustomEdit); override;
    class procedure Paste(const ACustomEdit: TCustomEdit); override;
    class procedure Undo(const ACustomEdit: TCustomEdit); override;
  end;
  TGtk3WSCustomEditClass = class of TGtk3WSCustomEdit;

  { TGtk3WSCustomMemo }

  TGtk3WSCustomMemo = class(TWSCustomMemo)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;

    class function GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;
    class procedure SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle); override;
    class procedure SetWantTabs(const ACustomMemo: TCustomMemo; const NewWantTabs: boolean); override;
    class procedure SetWantReturns(const ACustomMemo: TCustomMemo; const NewWantReturns: boolean); override;
    class procedure SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean); override;

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
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;

  end;
  TGtk3WSCustomMemoClass = class of TGtk3WSCustomMemo;

  { TGtk3WSEdit }

  TGtk3WSEdit = class(TGtk3WSCustomEdit)
  published
  end;

  { TGtk3WSMemo }

  TGtk3WSMemo = class(TGtk3WSCustomMemo)
  published
  end;

  { TGtk3WSCustomStaticText }

  TGtk3WSCustomStaticText = class(TWSCustomStaticText)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;
    class procedure SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment); override;
    class procedure SetStaticBorderStyle(const ACustomStaticText: TCustomStaticText; const NewBorderStyle: TStaticBorderStyle); override;
  end;
  TGtk3WSCustomStaticTextClass = class of TGtk3WSCustomStaticText;

  { TGtk3WSStaticText }

  TGtk3WSStaticText = class(TGtk3WSCustomStaticText)
  published
  end;

  { TGtk3WSButtonControl }

  TGtk3WSButtonControl = class(TWSButtonControl)
  published
    class function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; override;
  end;

  { TGtk3WSButton }

  TGtk3WSButton = class(TWSButton)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;
    class procedure SetDefault(const AButton: TCustomButton; ADefault: Boolean); override;
    class procedure SetShortCut(const AButton: TCustomButton; const ShortCutK1, ShortCutK2: TShortCut); override;
  end;
  TGtk3WSButtonClass = class of TGtk3WSButton;

  { TGtk3WSCustomCheckBox }

  TGtk3WSCustomCheckBox = class(TWSCustomCheckBox)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;
    class function RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;
    class procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox; const ShortCutK1, ShortCutK2: TShortCut); override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;
  end;
  TGtk3WSCustomCheckBoxClass = class of TGtk3WSCustomCheckBox;

  { TGtk3WSCheckBox }

  TGtk3WSCheckBox = class(TGtk3WSCustomCheckBox)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TWSToggleBox }

  { TGtk3WSToggleBox }

  TGtk3WSToggleBox = class(TGtk3WSCustomCheckBox)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TGtk3WSRadioButton }

  TGtk3WSRadioButton = class(TGtk3WSCustomCheckBox)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

implementation

uses
  LResources, gtk3private, LazGObject2;

{ TGtk3WSCustomGroupBox }

class function TGtk3WSCustomGroupBox.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  AGroupBox: TGtk3GroupBox;
begin
  AGroupBox := TGtk3GroupBox.Create(AWinControl, AParams);
  Result := TLCLIntfHandle(AGroupBox);
end;

class procedure TGtk3WSCustomGroupBox.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetPreferredSize') then
    Exit;
  TGtk3GroupBox(AWinControl.Handle).preferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);
end;

{ TGtk3WSRadioButton }

class function TGtk3WSRadioButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  ARadioButton: TGtk3RadioButton;
begin
  ARadioButton := TGtk3RadioButton.Create(AWinControl, AParams);
  Result := TLCLIntfHandle(ARadioButton);
end;

{ TGtk3WSToggleBox }

class function TGtk3WSToggleBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  AToggleBox: TGtk3ToggleButton;
begin
  AToggleBox := TGtk3ToggleButton.Create(AWinControl, AParams);
  Result := TLCLIntfHandle(AToggleBox);
end;

{ TGtk3WSCheckBox }

class function TGtk3WSCheckBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  ACheckBox: TGtk3CheckBox;
begin
  ACheckBox := TGtk3CheckBox.Create(AWinControl, AParams);
  Result := TLCLIntfHandle(ACheckBox);
end;

{ TGtk3WSScrollBar }

class function TGtk3WSScrollBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  AGtkScrollbar: TGtk3ScrollBar;
begin
  AGtkScrollBar := TGtk3ScrollBar.Create(AWinControl, AParams);
  Result:= TLCLIntfHandle(AGtkScrollBar);
end;

class procedure TGtk3WSScrollBar.SetParams(const AScrollBar: TCustomScrollBar);
begin
  if not WSCheckHandleAllocated(AScrollBar, 'SetParams') then
    Exit;
  TGtk3ScrollBar(AScrollBar.Handle).BeginUpdate;
  TGtk3ScrollBar(AScrollBar.Handle).SetParams;
  TGtk3ScrollBar(AScrollBar.Handle).EndUpdate;
end;

class procedure TGtk3WSScrollBar.SetKind(const AScrollBar: TCustomScrollBar;
  const AIsHorizontal: Boolean);
begin
  if not WSCheckHandleAllocated(AScrollBar, 'SetKind') then
    Exit;
  RecreateWnd(AScrollBar);
end;

{ TGtk3WSCustomListBox }

class function TGtk3WSCustomListBox.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  AListBox: TGtk3ListBox;
begin
  AListBox := TGtk3ListBox.Create(AWinControl, AParams);
  AListBox.BorderStyle := TCustomListBox(AWinControl).BorderStyle;
  AListBox.MultiSelect := TCustomListBox(AWinControl).MultiSelect;
  Result := TLCLIntfHandle(AListBox);
end;

class function TGtk3WSCustomListBox.GetIndexAtXY(
  const ACustomListBox: TCustomListBox; X, Y: integer): integer;
begin
  Result := -1;
end;

class function  TGtk3WSCustomListBox.GetItemIndex(const ACustomListBox: TCustomListBox): integer;
begin
  Result := 0;
  if not WSCheckHandleAllocated(ACustomListBox, 'GetItemIndex') then
    Exit;
  Result := TGtk3ListBox(ACustomListBox.Handle).ItemIndex;
end;

class function TGtk3WSCustomListBox.GetItemRect(
  const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect
  ): boolean;
begin
  FillChar(ARect,SizeOf(ARect),0);
  Result:=false;
end;

class function TGtk3WSCustomListBox.GetScrollWidth(
  const ACustomListBox: TCustomListBox): Integer;
begin
  Result := 0;
end;

class function  TGtk3WSCustomListBox.GetSelCount(const ACustomListBox: TCustomListBox): integer;
begin
  Result := 0;
  if not WSCheckHandleAllocated(ACustomListBox, 'GetSelCount') then
    Exit;
  Result := TGtk3ListBox(ACustomListBox.Handle).GetSelCount;
end;

class function  TGtk3WSCustomListBox.GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean;
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'GetSelected') then
    Exit(False);
  //DebugLn('WARNING: TGtk3WSCustomListBox.GetSelected is not implemented.');
  Result := TGtk3ListBox(ACustomListBox.Handle).GetItemSelected(AIndex);
end;

class function TGtk3WSCustomListBox.GetStrings(const ACustomListBox: TCustomListBox): TStrings;
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'GetStrings') then
    Exit;
  //DebugLn('TGtk3WSCustomListBox.GetStrings creating TGtkListStoreStringList ...');
  Result := TGtkListStoreStringList(g_object_get_data(PGObject(TGtk3ListBox(ACustomListBox.Handle).GetContainerWidget),
                                     GtkListItemLCLListTag));
  TGtkListStoreStringList(Result).Sorted := ACustomListBox.Sorted;
end;

class procedure TGtk3WSCustomListBox.FreeStrings(var AStrings: TStrings);
begin
  AStrings.Free;
  AStrings := nil;
end;

class function  TGtk3WSCustomListBox.GetTopIndex(const ACustomListBox: TCustomListBox): integer;
begin
  Result := 0;
end;

class procedure TGtk3WSCustomListBox.SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean);
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'SelectItem') then
    Exit;
  TGtk3ListBox(ACustomListBox.Handle).BeginUpdate;
  TGtk3ListBox(ACustomListBox.Handle).SelectItem(AIndex, ASelected);
  TGtk3ListBox(ACustomListBox.Handle).EndUpdate;
end;

class procedure TGtk3WSCustomListBox.SetBorder(const ACustomListBox: TCustomListBox);
begin

end;

class procedure TGtk3WSCustomListBox.SetColumnCount(const ACustomListBox: TCustomListBox;
  ACount: Integer);
begin

end;

class procedure TGtk3WSCustomListBox.SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer);
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'SetItemIndex') then
    Exit;
  TGtk3ListBox(ACustomListBox.Handle).BeginUpdate;
  TGtk3ListBox(ACustomListBox.Handle).ItemIndex := AIndex;
  TGtk3ListBox(ACustomListBox.Handle).EndUpdate;
end;

class procedure TGtk3WSCustomListBox.SetScrollWidth(
  const ACustomListBox: TCustomListBox; const AScrollWidth: Integer);
begin

end;

class procedure TGtk3WSCustomListBox.SetSelectionMode(const ACustomListBox: TCustomListBox;
  const AExtendedSelect, AMultiSelect: boolean);
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'SetSelectionMode') then
    Exit;
  TGtk3ListBox(ACustomListBox.Handle).MultiSelect := AMultiSelect;
end;

class procedure TGtk3WSCustomListBox.SetStyle(const ACustomListBox: TCustomListBox);
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'SetStyle') then
    Exit;
  if TGtk3ListBox(ACustomListBox.Handle).ListBoxStyle <> ACustomListBox.Style then
    RecreateWnd(ACustomListBox);
end;

class procedure TGtk3WSCustomListBox.SetSorted(const ACustomListBox: TCustomListBox;
  AList: TStrings; ASorted: boolean);
begin
end;

class procedure TGtk3WSCustomListBox.SetTopIndex(const ACustomListBox: TCustomListBox;
  const NewTopIndex: integer);
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'SetTopIndex') then
    Exit;
  TGtk3ListBox(ACustomListBox.Handle).SetTopIndex(NewTopIndex);
end;

{ TGtk3WSCustomComboBox }

class function TGtk3WSCustomComboBox.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  AGtkCombo: TGtk3ComboBox;
begin
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomComboBox.CreateHandle');
  {$ENDIF}
  AGtkCombo := TGtk3ComboBox.Create(AWinControl, AParams);
  Result := TLCLIntfHandle(AGtkCombo);
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSCustomComboBox.CreateHandle Handle=',dbgs(Result));
  {$ENDIF}
end;

class procedure TGtk3WSCustomComboBox.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetPreferredSize') then
    Exit;
  TGtk3ComboBox(AWinControl.Handle).preferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);
end;

class function TGtk3WSCustomComboBox.GetDroppedDown(
  const ACustomComboBox: TCustomComboBox): Boolean;
begin
  Result := False;
  if not WSCheckHandleAllocated(ACustomComboBox, 'GetDroppedDown') then
    Exit;
  Result := TGtk3ComboBox(ACustomComboBox.Handle).DroppedDown;
end;

class function TGtk3WSCustomComboBox.GetSelStart(const ACustomComboBox: TCustomComboBox
  ): integer;
begin
  Result := -1;
end;

class function TGtk3WSCustomComboBox.GetSelLength(const ACustomComboBox: TCustomComboBox
  ): integer;
begin
  Result := 0;
end;

class function TGtk3WSCustomComboBox.GetItemIndex(const ACustomComboBox: TCustomComboBox
  ): integer;
begin
  Result := -1;
  if not WSCheckHandleAllocated(ACustomComboBox, 'GetItemIndex') then
    Exit;
  Result := TGtk3ComboBox(ACustomComboBox.Handle).ItemIndex;
end;

class function TGtk3WSCustomComboBox.GetMaxLength(const ACustomComboBox: TCustomComboBox
  ): integer;
begin
  Result := 0;
end;

class procedure TGtk3WSCustomComboBox.SetArrowKeysTraverseList(
  const ACustomComboBox: TCustomComboBox; NewTraverseList: boolean);
begin
end;

class procedure TGtk3WSCustomComboBox.SetDropDownCount(
  const ACustomComboBox: TCustomComboBox; NewCount: Integer);
begin

end;

class procedure TGtk3WSCustomComboBox.SetDroppedDown(
  const ACustomComboBox: TCustomComboBox; ADroppedDown: Boolean);
begin
  if not WSCheckHandleAllocated(ACustomComboBox, 'SetDroppedDown') then
    Exit;
  TGtk3ComboBox(ACustomComboBox.Handle).DroppedDown := ADroppedDown;
end;

class procedure TGtk3WSCustomComboBox.SetMaxLength(const ACustomComboBox: TCustomComboBox;
  NewLength: integer);
begin

end;

class procedure TGtk3WSCustomComboBox.SetSelStart(const ACustomComboBox: TCustomComboBox;
  NewStart: integer);
begin

end;

class procedure TGtk3WSCustomComboBox.SetSelLength(const ACustomComboBox: TCustomComboBox;
  NewLength: integer);
begin

end;

class procedure TGtk3WSCustomComboBox.SetItemIndex(const ACustomComboBox: TCustomComboBox;
  NewIndex: integer);
begin
  if not WSCheckHandleAllocated(ACustomComboBox, 'SetItemIndex') then
    Exit;
  // DebugLn('TGtk3WSCustomComboBox.SetItemIndex ',dbgs(NewIndex));
  TGtk3ComboBox(ACustomComboBox.Handle).BeginUpdate;
  TGtk3ComboBox(ACustomComboBox.Handle).ItemIndex := NewIndex;
  TGtk3ComboBox(ACustomComboBox.Handle).EndUpdate;
end;

class procedure TGtk3WSCustomComboBox.SetStyle(const ACustomComboBox: TCustomComboBox;
  NewStyle: TComboBoxStyle);
begin

end;

class function TGtk3WSCustomComboBox.GetItems(const ACustomComboBox: TCustomComboBox
  ): TStrings;
begin
  Result := nil;

  if not WSCheckHandleAllocated(ACustomComboBox, 'GetItems') then
  begin
    Exit;
  end;
  // DebugLn('TGtk3WSCustomComboBox.GetItems creating TGtkListStoreStringList ...');
  Result := TGtkListStoreStringList(g_object_get_data(PGObject(TGtk3ComboBox(ACustomComboBox.Handle).GetContainerWidget),
                                     GtkListItemLCLListTag));
end;

class procedure TGtk3WSCustomComboBox.Sort(const ACustomComboBox: TCustomComboBox;
  AList: TStrings; IsSorted: boolean);
begin
end;

class function TGtk3WSCustomComboBox.GetItemHeight(const ACustomComboBox: TCustomComboBox): Integer;
begin
  Result := 0;
end;

class procedure TGtk3WSCustomComboBox.SetItemHeight(const ACustomComboBox: TCustomComboBox; const AItemHeight: Integer);
begin
end;

{ TGtk3WSCustomEdit }

class function TGtk3WSCustomEdit.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  AGtkEntry: TGtk3Entry;
begin
  AGtkEntry := TGtk3Entry.Create(AWinControl, AParams);
  Result := TLCLIntfHandle(AGtkEntry);
end;

class procedure TGtk3WSCustomEdit.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetPreferredSize') then
    Exit;
  TGtk3Entry(AWinControl.Handle).preferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);
end;

class function TGtk3WSCustomEdit.GetCanUndo(const ACustomEdit: TCustomEdit
  ): Boolean;
begin
  Result := False;
end;

class function TGtk3WSCustomEdit.GetCaretPos(const ACustomEdit: TCustomEdit): TPoint;
begin
  Result := Point(0, 0);
  if not WSCheckHandleAllocated(ACustomEdit, 'GetCaretPos') then
    Exit;
  Result := TGtk3Editable(ACustomEdit.Handle).CaretPos;
end;

class function  TGtk3WSCustomEdit.GetSelStart(const ACustomEdit: TCustomEdit): integer;
begin
  result := -1;
  if not WSCheckHandleAllocated(ACustomEdit, 'GetSelStart') then
    Exit;
  Result := TGtk3Editable(ACustomEdit.Handle).getSelStart;
end;

class function  TGtk3WSCustomEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
begin
  result := 0;
  if not WSCheckHandleAllocated(ACustomEdit, 'GetSelLength') then
    Exit;
  Result := TGtk3Editable(ACustomEdit.Handle).getSelLength;
end;

class procedure TGtk3WSCustomEdit.SetAlignment(const ACustomEdit: TCustomEdit;
  const AAlignment: TAlignment);
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetAlignment') then
    Exit;
  TGtk3Editable(ACustomEdit.Handle).BeginUpdate;
  TGtk3Entry(ACustomEdit.Handle).Alignment := AAlignment;
  TGtk3Editable(ACustomEdit.Handle).EndUpdate;
end;

class procedure TGtk3WSCustomEdit.SetCaretPos(const ACustomEdit: TCustomEdit; const NewPos: TPoint);
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'GetCaretPos') then
    Exit;
  TGtk3Editable(ACustomEdit.Handle).BeginUpdate;
  TGtk3Editable(ACustomEdit.Handle).CaretPos := NewPos;
  TGtk3Editable(ACustomEdit.Handle).EndUpdate;
end;

class procedure TGtk3WSCustomEdit.SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase);
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetCharCase') then
    Exit;
end;

class procedure TGtk3WSCustomEdit.SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode);
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetEchoMode') then
    Exit;
  if NewMode in [emNone,emPassword] then
  begin
    TGtk3Entry(ACustomEdit.Handle).SetEchoMode(False);
    SetPasswordChar(ACustomEdit, ACustomEdit.PasswordChar);
  end else
  begin
    TGtk3Entry(ACustomEdit.Handle).SetEchoMode(True);
  end;
end;

class procedure TGtk3WSCustomEdit.SetHideSelection(const ACustomEdit: TCustomEdit;
  NewHideSelection: Boolean);
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetHideSelection') then
    Exit;
end;

class procedure TGtk3WSCustomEdit.SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer);
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetMaxLength') then
    Exit;
  TGtk3Entry(ACustomEdit.Handle).SetMaxLength(NewLength);
end;

class procedure TGtk3WSCustomEdit.SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char);
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetPasswordChar') then
    Exit;
  TGtk3Entry(ACustomEdit.Handle).SetPasswordChar(NewChar);
end;

class procedure TGtk3WSCustomEdit.SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean);
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetReadOnly') then
    Exit;
  TGtk3Editable(ACustomEdit.Handle).ReadOnly := NewReadOnly;
end;

class procedure TGtk3WSCustomEdit.SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer);
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetSelStart') then
    Exit;
  TGtk3Editable(ACustomEdit.Handle).BeginUpdate;
  TGtk3Editable(ACustomEdit.Handle).SetSelStart(NewStart);
  TGtk3Editable(ACustomEdit.Handle).EndUpdate;
end;

class procedure TGtk3WSCustomEdit.SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer);
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetSelLength') then
    Exit;
  TGtk3Editable(ACustomEdit.Handle).BeginUpdate;
  TGtk3Editable(ACustomEdit.Handle).SetSelLength(NewLength);
  TGtk3Editable(ACustomEdit.Handle).EndUpdate;
end;

class procedure TGtk3WSCustomEdit.Cut(const ACustomEdit: TCustomEdit);
begin
  ACustomEdit.CopyToClipboard;
  ACustomEdit.ClearSelection;
end;

class procedure TGtk3WSCustomEdit.Copy(const ACustomEdit: TCustomEdit);
begin
  if (ACustomEdit.EchoMode = emNormal) and (ACustomEdit.SelLength > 0) then
    Clipboard.AsText := ACustomEdit.SelText;
end;

class procedure TGtk3WSCustomEdit.Paste(const ACustomEdit: TCustomEdit);
begin
  if Clipboard.HasFormat(CF_TEXT) then
    ACustomEdit.SelText := Clipboard.AsText;
end;

class procedure TGtk3WSCustomEdit.Undo(const ACustomEdit: TCustomEdit);
begin
  // nothing
end;

{ TGtk3WSCustomMemo }

class function TGtk3WSCustomMemo.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  AGtkMemo: TGtk3Memo;
begin
  AGtkMemo := TGtk3Memo.Create(AWinControl, AParams);
  AGtkMemo.BorderStyle := TCustomMemo(AWinControl).BorderStyle;
  Result := TLCLIntfHandle(AGtkMemo);
end;

class procedure TGtk3WSCustomMemo.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetPreferredSize') then
    Exit;
  TGtk3Memo(AWinControl.Handle).preferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);
end;

class function TGtk3WSCustomMemo.GetStrings(const ACustomMemo: TCustomMemo
  ): TStrings;
begin
  Result := TGtk3MemoStrings.Create(ACustomMemo);
end;

class procedure TGtk3WSCustomMemo.SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle);
var
  AScrollStyle: TPoint;
begin
  if not WSCheckHandleAllocated(ACustomMemo, 'SetScrollBars') then
    Exit;
  AScrollStyle := Gtk3TranslateScrollStyle(ACustomMemo.ScrollBars);
  TGtk3Memo(ACustomMemo.Handle).BeginUpdate;
  TGtk3Memo(ACustomMemo.Handle).HScrollBarPolicy := AScrollStyle.X;
  TGtk3Memo(ACustomMemo.Handle).VScrollBarPolicy := AScrollStyle.Y;
  TGtk3Memo(ACustomMemo.Handle).EndUpdate;
end;

class procedure TGtk3WSCustomMemo.SetWantTabs(const ACustomMemo: TCustomMemo; const NewWantTabs: boolean);
begin
  if not WSCheckHandleAllocated(ACustomMemo, 'SetWantTabs') then
    Exit;
  TGtk3Memo(ACustomMemo.Handle).WantTabs := NewWantTabs;
end;

class procedure TGtk3WSCustomMemo.SetWantReturns(const ACustomMemo: TCustomMemo; const NewWantReturns: boolean);
begin
  // no way
end;

class procedure TGtk3WSCustomMemo.SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean);
begin
  if not WSCheckHandleAllocated(ACustomMemo, 'SetWordWrap') then
    Exit;
  TGtk3Memo(ACustomMemo.Handle).WordWrap := NewWordWrap;
end;

class function TGtk3WSCustomMemo.GetCanUndo(const ACustomEdit: TCustomEdit
  ): Boolean;
begin
  Result := False;
end;

class function TGtk3WSCustomMemo.GetCaretPos(const ACustomEdit: TCustomEdit
  ): TPoint;
begin
  Result := Point(0, 0);
end;

class function TGtk3WSCustomMemo.GetSelStart(const ACustomEdit: TCustomEdit
  ): integer;
begin
  Result := 0;
end;

class function TGtk3WSCustomMemo.GetSelLength(const ACustomEdit: TCustomEdit
  ): integer;
begin
  Result := 0;
end;

class procedure TGtk3WSCustomMemo.SetAlignment(const ACustomEdit: TCustomEdit;
  const AAlignment: TAlignment);
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetAlignment') then
    Exit;
  TGtk3Memo(ACustomEdit.Handle).Alignment := AAlignment;
end;

class procedure TGtk3WSCustomMemo.SetCaretPos(const ACustomEdit: TCustomEdit;
  const NewPos: TPoint);
begin
  // inherited SetCaretPos(ACustomEdit, NewPos);
end;

class procedure TGtk3WSCustomMemo.SetCharCase(const ACustomEdit: TCustomEdit;
  NewCase: TEditCharCase);
begin
  // inherited SetCharCase(ACustomEdit, NewCase);
end;

class procedure TGtk3WSCustomMemo.SetEchoMode(const ACustomEdit: TCustomEdit;
  NewMode: TEchoMode);
begin
  // inherited SetEchoMode(ACustomEdit, NewMode);
end;

class procedure TGtk3WSCustomMemo.SetHideSelection(
  const ACustomEdit: TCustomEdit; NewHideSelection: Boolean);
begin
  // inherited SetHideSelection(ACustomEdit, NewHideSelection);
end;

class procedure TGtk3WSCustomMemo.SetMaxLength(const ACustomEdit: TCustomEdit;
  NewLength: integer);
begin
  // inherited SetMaxLength(ACustomEdit, NewLength);
end;

class procedure TGtk3WSCustomMemo.SetPasswordChar(
  const ACustomEdit: TCustomEdit; NewChar: char);
begin
  // inherited SetPasswordChar(ACustomEdit, NewChar);
end;

class procedure TGtk3WSCustomMemo.SetReadOnly(const ACustomEdit: TCustomEdit;
  NewReadOnly: boolean);
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetReadOnly') then
    Exit;
  TGtk3Memo(ACustomEdit.Handle).ReadOnly := NewReadOnly;
end;

class procedure TGtk3WSCustomMemo.SetSelStart(const ACustomEdit: TCustomEdit;
  NewStart: integer);
begin
  // inherited SetSelStart(ACustomEdit, NewStart);
end;

class procedure TGtk3WSCustomMemo.SetSelLength(const ACustomEdit: TCustomEdit;
  NewLength: integer);
begin
  // inherited SetSelLength(ACustomEdit, NewLength);
end;

{ TGtk3WSCustomStaticText }

class function TGtk3WSCustomStaticText.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  AStaticText: TGtk3StaticText;
begin
  AStaticText := TGtk3StaticText.Create(AWinControl, AParams);
  Result := TLCLIntfHandle(AStaticText);
end;

class procedure TGtk3WSCustomStaticText.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetPreferredSize') then
    Exit;
  TGtk3StaticText(AWinControl.Handle).preferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);
end;

class procedure TGtk3WSCustomStaticText.SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment);
begin
  if not WSCheckHandleAllocated(ACustomStaticText, 'SetAlignment') then
    Exit;
  TGtk3StaticText(ACustomStaticText.Handle).Alignment := NewAlignment;
end;

class procedure TGtk3WSCustomStaticText.SetStaticBorderStyle(
  const ACustomStaticText: TCustomStaticText;
  const NewBorderStyle: TStaticBorderStyle);
begin
  if not WSCheckHandleAllocated(ACustomStaticText, 'SetStaticBorderStyle') then
    Exit;
  TGtk3StaticText(ACustomStaticText.Handle).StaticBorderStyle := NewBorderStyle;
end;

{ TGtk3WSButton }

class function TGtk3WSButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  AButton: TGtk3Button;
begin
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSButton.CreateHandle');
  {$ENDIF}
  AButton := TGtk3Button.Create(AWinControl, AParams);
  Result := TLCLIntfHandle(AButton);
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSButton.CreateHandle Handle=',dbgs(Result));
  {$ENDIF}
end;

class procedure TGtk3WSButton.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetPreferredSize') then
    Exit;
  TGtk3Button(AWinControl.Handle).preferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);
end;

class procedure TGtk3WSButton.SetDefault(const AButton: TCustomButton; ADefault: Boolean);
begin
  if not WSCheckHandleAllocated(AButton, 'SetDefault') then
    Exit;
  TGtk3Button(AButton.Handle).SetDefault(ADefault);
end;

class procedure TGtk3WSButton.SetShortCut(const AButton: TCustomButton;
  const ShortCutK1, ShortCutK2: TShortCut);
begin;
  //TODO:
end;

{ TGtk3WSCustomCheckBox }

class function TGtk3WSCustomCheckBox.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  ACheckBox: TGtk3CheckBox;
begin
  ACheckBox := TGtk3CheckBox.Create(AWinControl, AParams);

  Result := TLCLIntfHandle(ACheckBox);
end;

class procedure TGtk3WSCustomCheckBox.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetPreferredSize') then
    Exit;
  TGtk3CheckBox(AWinControl.Handle).preferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);
end;

class function TGtk3WSCustomCheckBox.RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
begin
  Result := cbUnchecked;
  if not WSCheckHandleAllocated(ACustomCheckBox, 'RetrieveState') then
    Exit;
  Result := TGtk3CheckBox(ACustomCheckBox.Handle).State;
end;

class procedure TGtk3WSCustomCheckBox.SetShortCut(const ACustomCheckBox: TCustomCheckBox;
  const ShortCutK1, ShortCutK2: TShortCut);
begin
  //TODO:
end;

class procedure TGtk3WSCustomCheckBox.SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
begin
  if not WSCheckHandleAllocated(ACustomCheckBox, 'SetState') then
    Exit;
  TGtk3CheckBox(ACustomCheckBox.Handle).State := NewState;
end;

{ TGtk3WSButtonControl }

class function TGtk3WSButtonControl.GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor;
begin
  Result := DefBtnColors[ADefaultColorType];
end;

end.
