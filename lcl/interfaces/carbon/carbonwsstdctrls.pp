{ $Id$}
{
 *****************************************************************************
 *                              CarbonWSStdCtrls.pp                          *
 *                              ---------------                              *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CarbonWSStdCtrls;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  FPCMacOSAll, CarbonDef, CarbonProc, CarbonUtils,
  Classes, Controls, StdCtrls, LCLType, LCLProc, LMessages, LCLMessageGlue,
////////////////////////////////////////////////////
  WSStdCtrls, WSLCLClasses, WSControls, WSProc,
  CarbonWSControls, CarbonPrivate;
type

  { TCarbonWSScrollBar }

  TCarbonWSScrollBar = class(TWSScrollBar)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetParams(const AScrollBar: TCustomScrollBar); override;
  end;

  { TCarbonWSCustomGroupBox }

  TCarbonWSCustomGroupBox = class(TWSCustomGroupBox)
  private
  protected
  public
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCarbonWSGroupBox }

  TCarbonWSGroupBox = class(TWSGroupBox)
  private
  protected
  public
  end;

  { TCarbonWSCustomComboBox }

  TCarbonWSCustomComboBox = class(TWSCustomComboBox)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function  GetSelStart(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetSelLength(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetItemIndex(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetMaxLength(const ACustomComboBox: TCustomComboBox): integer; override;

    class procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); override;
    class procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer); override;
    class procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;

    class function  GetItems(const ACustomComboBox: TCustomComboBox): TStrings; override;
    class procedure Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean); override;
  end;

  { TCarbonWSComboBox }

  TCarbonWSComboBox = class(TWSComboBox)
  private
  protected
  public
  end;

  { TCarbonWSCustomListBox }

  TCarbonWSCustomListBox = class(TWSCustomListBox)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function  GetSelCount(const ACustomListBox: TCustomListBox): integer; override;
    class function  GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean; override;
    class function  GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
    class function  GetItemIndex(const ACustomListBox: TCustomListBox): integer; override;
    class function  GetTopIndex(const ACustomListBox: TCustomListBox): integer; override;
    class procedure SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean); override;
    // class procedure SetBorder(const ACustomListBox: TCustomListBox); override;
    class procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer); override;
    class procedure SetSelectionMode(const ACustomListBox: TCustomListBox; const AExtendedSelect, AMultiSelect: boolean); override;
    class procedure SetStyle(const ACustomListBox: TCustomListBox); override;
    class procedure SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean); override;
    class procedure SetTopIndex(const ACustomListBox: TCustomListBox; const NewTopIndex: integer); override;
  end;

  { TCarbonWSListBox }

  TCarbonWSListBox = class(TWSListBox)
  private
  protected
  public
  end;

  { TCarbonWSCustomEdit }

  TCarbonWSCustomEdit = class(TWSCustomEdit)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;

    class function  GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetSelLength(const ACustomEdit: TCustomEdit): integer; override;

    class procedure SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase); override;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); override;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
  end;
  
  { TCarbonWSCustomMemo }

  TCarbonWSCustomMemo = class(TWSCustomMemo)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function  GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;

    class procedure AppendText(const ACustomMemo: TCustomMemo; const AText: string); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
    class procedure SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    class procedure SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean); override;
  end;

  { TCarbonWSEdit }

  TCarbonWSEdit = class(TWSEdit)
  private
  protected
  public
  end;

  { TCarbonWSMemo }

  TCarbonWSMemo = class(TWSMemo)
  private
  protected
  public
  end;

  { TCarbonWSCustomLabel }

  {TCarbonWSCustomLabel = class(TWSCustomLabel)
  private
  protected
  public
  end;}

  { TCarbonWSLabel }

  {TCarbonWSLabel = class(TWSLabel)
  private
  protected
  public
  end;}

  { TCarbonWSButtonControl }

  TCarbonWSButtonControl = class(TWSButtonControl)
  private
  protected
  public
  end;

  { TCarbonWSCustomCheckBox }

  TCarbonWSCustomCheckBox = class(TWSCustomCheckBox)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function  RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;
  end;

  { TCarbonWSCheckBox }

  TCarbonWSCheckBox = class(TWSCheckBox)
  private
  protected
  public
  end;

  { TCarbonWSToggleBox }

  TCarbonWSToggleBox = class(TWSToggleBox)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCarbonWSRadioButton }

  TCarbonWSRadioButton = class(TWSRadioButton)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCarbonWSCustomStaticText }

  TCarbonWSCustomStaticText = class(TWSCustomStaticText)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment); override;
  end;

  { TCarbonWSStaticText }

  TCarbonWSStaticText = class(TWSStaticText)
  private
  protected
  public
  end;
  

implementation

uses CarbonStrings;

{ TCarbonWSScrollBar }

{------------------------------------------------------------------------------
  Method:  TCarbonWSScrollBar.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new scroll bar in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSScrollBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonScrollBar.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSScrollBar.SetParams
  Params:  AScrollBar - LCL custom scroll bar

  Sets the parameters (Min, Max, PageSize, Position) of scroll bar in Carbon
  interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSScrollBar.SetParams(const AScrollBar: TCustomScrollBar);
begin
  if not CheckHandle(AScrollBar, Self, 'SetParams') then Exit;
  
  TCarbonCustomBar(AScrollBar.Handle).SetData(AScrollBar.Position,
    AScrollBar.Min, AScrollBar.Max, AScrollBar.PageSize);
end;

{ TCarbonWSCustomGroupBox }

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomGroupBox.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new group box in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomGroupBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonGroupBox.Create(AWinControl, AParams));
end;

{ TCarbonWSCustomComboBox }

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomComboBox.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new combo box in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomComboBox.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonComboBox.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomComboBox.GetSelStart
  Params:  ACustomComboBox - LCL custom combo box
  Returns: Position of selection start of combo box in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomComboBox.GetSelStart(
  const ACustomComboBox: TCustomComboBox): integer;
begin
  Result := 0;
  if not CheckHandle(ACustomComboBox, Self, 'GetSelStart') then Exit;

  TCarbonComboBox(ACustomComboBox.Handle).GetSelStart(Result);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomComboBox.GetSelLength
  Params:  ACustomComboBox - LCL custom combo box
  Returns: Length of selection of combo box in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomComboBox.GetSelLength(
  const ACustomComboBox: TCustomComboBox): integer;
begin
  Result := 0;
  if not CheckHandle(ACustomComboBox, Self, 'GetSelLength') then Exit;

  TCarbonComboBox(ACustomComboBox.Handle).GetSelLength(Result);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomComboBox.GetItemIndex
  Params:  ACustomComboBox - LCL custom combo box
  Returns: Selected item index of combo box in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomComboBox.GetItemIndex(
  const ACustomComboBox: TCustomComboBox): integer;
begin
  Result := -1;
  if not CheckHandle(ACustomComboBox, Self, 'GetItemIndex') then Exit;

  Result := TCarbonComboBox(ACustomComboBox.Handle).GetItemIndex;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomComboBox.GetMaxLength
  Params:  ACustomComboBox - LCL custom combo box
  Returns: Maximal length of text of combo box in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomComboBox.GetMaxLength(
  const ACustomComboBox: TCustomComboBox): integer;
begin
  Result := 0;
  if not CheckHandle(ACustomComboBox, Self, 'GetMaxLength') then Exit;

  Result := TCarbonComboBox(ACustomComboBox.Handle).MaxLength;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomComboBox.SetSelStart
  Params:  ACustomComboBox - LCL custom combo box
           NewStart        - New position of selection start
  Returns: Nothing

  Sets the new position of selection start of combo box in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomComboBox.SetSelStart(
  const ACustomComboBox: TCustomComboBox; NewStart: integer);
begin
  if not CheckHandle(ACustomComboBox, Self, 'SetSelStart') then Exit;

  TCarbonComboBox(ACustomComboBox.Handle).SetSelStart(NewStart);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomComboBox.SetSelLength
  Params:  ACustomComboBox - LCL custom combo box
           NewLength       - New length of selection
  Returns: Nothing

  Sets the new length of selection of combo box in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomComboBox.SetSelLength(
  const ACustomComboBox: TCustomComboBox; NewLength: integer);
begin
    if not CheckHandle(ACustomComboBox, Self, 'SetSelLength') then Exit;

  TCarbonComboBox(ACustomComboBox.Handle).SetSelLength(NewLength);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomComboBox.SetItemIndex
  Params:  ACustomComboBox - LCL custom combo box
           NewIndex        - New selected item index
  Returns: Nothing

  Selects the item with index in combo box in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomComboBox.SetItemIndex(
  const ACustomComboBox: TCustomComboBox; NewIndex: integer);
begin
  if not CheckHandle(ACustomComboBox, Self, 'SetItemIndex') then Exit;

  TCarbonComboBox(ACustomComboBox.Handle).SetItemIndex(NewIndex);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomComboBox.SetMaxLength
  Params:  ACustomEdit - LCL custom combo box
           NewLength   - New max length
  Returns: Nothing

  Sets the new maximal length of text of combo box in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomComboBox.SetMaxLength(
  const ACustomComboBox: TCustomComboBox; NewLength: integer);
begin
  if not CheckHandle(ACustomComboBox, Self, 'SetMaxLength') then Exit;

  // text is cropped in callback
  TCarbonComboBox(ACustomComboBox.Handle).MaxLength := NewLength;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomComboBox.GetItems
  Params:  ACustomEdit - LCL custom combo box
  Returns: Items of combo box in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomComboBox.GetItems(
  const ACustomComboBox: TCustomComboBox): TStrings;
begin
  Result := nil;
  if not CheckHandle(ACustomComboBox, Self, 'GetItems') then Exit;

  Result := TCarbonComboBoxStrings.Create(TCarbonComboBox(ACustomComboBox.Handle));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomComboBox.Sort
  Params:  ACustomEdit - LCL custom combo box
           AList       - Strings to sort
           IsSorted    - Sorted state
  Returns: Nothing

  Sorts the items in combo box in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomComboBox.Sort(
  const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean);
begin
  if not CheckHandle(ACustomComboBox, Self, 'Sort') then Exit;
  
  TCarbonComboBoxStrings(AList).Sorted := IsSorted;
end;

{ TCarbonWSCustomListBox }

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomListBox.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new list box in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomListBox.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonListBox.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomListBox.GetSelCount
  Params:  ACustomListBox - LCL custom list box
  Returns: Count of selected items in list box in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomListBox.GetSelCount(
  const ACustomListBox: TCustomListBox): integer;
var
  Item: Cell;
  I: Integer;
  List: ListHandle;
begin
  Result := 0;
  if not CheckHandle(ACustomListBox, Self, 'GetSelCount') then Exit;
  
  List := TCarbonListBox(ACustomListBox.Handle).List;
  Item.h := 0;
  Item.v := 0;
  for I := 0 to TCarbonListBox(ACustomListBox.Handle).GetItemsCount - 1 do
  begin
    if LGetSelect(False, Item, List) then Inc(Result);
    Inc(Item.v);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomListBox.GetSelected
  Params:  ACustomListBox - LCL custom list box
           AIndex         - Item index
  Returns: If the specified item in list box in Carbon interface is selected
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomListBox.GetSelected(
  const ACustomListBox: TCustomListBox; const AIndex: integer): boolean;
var
  Item: Cell;
begin
  Result := False;
  if not CheckHandle(ACustomListBox, Self, 'GetSelected') then Exit;

  Item.h := 0;
  Item.v := AIndex;
  Result := LGetSelect(False, Item, TCarbonListBox(ACustomListBox.Handle).List);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomListBox.GetStrings
  Params:  ACustomListBox - LCL custom list box
  Returns: Items of list box in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomListBox.GetStrings(
  const ACustomListBox: TCustomListBox): TStrings;
begin
  Result := nil;
  if not CheckHandle(ACustomListBox, Self, 'GetStrings') then Exit;
  
  Result := TCarbonListBoxStrings.Create(TCarbonListBox(ACustomListBox.Handle));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomListBox.GetItemIndex
  Params:  ACustomListBox - LCL custom list box
  Returns: Index of selected item of list box in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomListBox.GetItemIndex(
  const ACustomListBox: TCustomListBox): integer;
begin
  Result := -1;
  if not CheckHandle(ACustomListBox, Self, 'GetItemIndex') then Exit;
  
  Result := TCarbonListBox(ACustomListBox.Handle).GetItemIndex;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomListBox.GetTopIndex
  Params:  ACustomListBox - LCL custom list box
  Returns: Index of top visible item of list box in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomListBox.GetTopIndex(
  const ACustomListBox: TCustomListBox): integer;
var
  Bounds: FPCMacOSAll.Rect;
begin
  Result := 0;
  if not CheckHandle(ACustomListBox, Self, 'GetTopIndex') then Exit;

  if GetListViewBounds(TCarbonListBox(ACustomListBox.Handle).List,
    Bounds) <> nil then Result := Bounds.top;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomListBox.SelectItem
  Params:  ACustomListBox - LCL custom list box
           AIndex         - Item index to change selection
           ASelected      - New selection value
  Returns: Nothing

  Changes selection of item with the specified index of list box in Carbon
  interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomListBox.SelectItem(
  const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean);
var
  Item: Cell;
begin
  if not CheckHandle(ACustomListBox, Self, 'SelectItem') then Exit;

  Item.h := 0;
  Item.v := AIndex;
  LSetSelect(ASelected, Item, TCarbonListBox(ACustomListBox.Handle).List);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomListBox.SetItemIndex
  Params:  ACustomListBox - LCL custom list box
           AIndex         - Item index
  Returns: Nothing

  Sets item index of list box in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomListBox.SetItemIndex(
  const ACustomListBox: TCustomListBox; const AIndex: integer);
begin
  if not CheckHandle(ACustomListBox, Self, 'SetItemIndex') then Exit;

  TCarbonListBox(ACustomListBox.Handle).SetItemIndex(AIndex);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomListBox.SetSelectionMode
  Params:  ACustomListBox  - LCL custom list box
           AExtendedSelect - New extended selection value
           AMultiSelect    - New mutliple selection value
  Returns: Nothing

  Changes selection mode of list box in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomListBox.SetSelectionMode(
  const ACustomListBox: TCustomListBox; const AExtendedSelect,
  AMultiSelect: boolean);
var
  Options: OptionBits;
begin
  if not CheckHandle(ACustomListBox, Self, 'SetSelectionMode') then Exit;

  if AMultiSelect then
  begin
    if AExtendedSelect then Options := lUseSense
    else Options := 0;
  end
  else Options := lOnlyOne;
  
  SetListSelectionFlags(TCarbonListBox(ACustomListBox.Handle).List, Options);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomListBox.SetStyle
  Params:  ACustomListBox - LCL custom list box
  Returns: Nothing

  Changes style of list box in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomListBox.SetStyle(
  const ACustomListBox: TCustomListBox);
begin
  // TODO
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomListBox.SetSorted
  Params:  ACustomListBox - LCL custom list box
           AList          - Items to sort
           ASorted        - New sorted value
  Returns: Nothing

  Sorts items of list box in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomListBox.SetSorted(
  const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean);
begin
  if not CheckHandle(ACustomListBox, Self, 'SetSorted') then Exit;
  
  TCarbonListBoxStrings(AList).Sorted := ASorted;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomListBox.SetTopIndex
  Params:  ACustomListBox - LCL custom list box
           NewTopIndex    - New top index
  Returns: Nothing

  Sets top visible item of list box in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomListBox.SetTopIndex(
  const ACustomListBox: TCustomListBox; const NewTopIndex: integer);
var
  Bounds: FPCMacOSAll.Rect;
begin
  if not CheckHandle(ACustomListBox, Self, 'SetTopIndex') then Exit;

  if GetListViewBounds(TCarbonListBox(ACustomListBox.Handle).List,
    Bounds) <> nil then
  begin
    Bounds.bottom := Bounds.bottom + (NewTopIndex - Bounds.top);
    Bounds.top := NewTopIndex;
    SetListViewBounds(TCarbonListBox(ACustomListBox.Handle).List, Bounds);
  end;
end;

{ TCarbonWSCustomEdit }

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomEdit.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new edit in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomEdit.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonEdit.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomEdit.GetSelStart
  Params:  ACustomEdit - LCL custom edit
  Returns: Position of selection start of edit in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomEdit.GetSelStart(const ACustomEdit: TCustomEdit): integer;
begin
  Result := 0;
  if not CheckHandle(ACustomEdit, Self, 'GetSelStart') then Exit;

  TCarbonEdit(ACustomEdit.Handle).GetSelStart(Result);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomEdit.GetSelLength
  Params:  ACustomEdit - LCL custom edit
  Returns: Length of selection of edit in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
begin
  Result := 0;
  if not CheckHandle(ACustomEdit, Self, 'GetSelLength') then Exit;

  TCarbonEdit(ACustomEdit.Handle).GetSelLength(Result);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomEdit.SetCharCase
  Params:  ACustomEdit - LCL custom edit
           NewCase     - New char case
  Returns: Nothing

  Sets the new char case of edit in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomEdit.SetCharCase(const ACustomEdit: TCustomEdit;
  NewCase: TEditCharCase);
begin
 // TODO
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomEdit.SetEchoMode
  Params:  ACustomEdit - LCL custom edit
           NewMode     - New echo mode
  Returns: Nothing

  Sets the new echo mode of edit in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomEdit.SetEchoMode(const ACustomEdit: TCustomEdit;
  NewMode: TEchoMode);
begin
  // TODO
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomEdit.SetMaxLength
  Params:  ACustomEdit - LCL custom edit
           NewLength   - New max length
  Returns: Nothing

  Sets the new max length of edit in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomEdit.SetMaxLength(const ACustomEdit: TCustomEdit;
  NewLength: integer);
begin
  if not CheckHandle(ACustomEdit, Self, 'SetMaxLength') then Exit;

  // text is cropped in callback
  TCarbonEdit(ACustomEdit.Handle).MaxLength := NewLength;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomEdit.SetPasswordChar
  Params:  ACustomEdit - LCL custom edit
           NewChar     - New password char
  Returns: Nothing

  Sets the new password char of edit in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomEdit.SetPasswordChar(const ACustomEdit: TCustomEdit;
  NewChar: char);
begin
  if not CheckHandle(ACustomEdit, Self, 'SetPasswordChar') then Exit;

  if TCarbonEdit(ACustomEdit.Handle).IsPassword <> (NewChar <> #0) then
    RecreateWnd(ACustomEdit);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomEdit.SetReadOnly
  Params:  ACustomEdit - LCL custom edit
           NewReadOnly - Read only behavior
  Returns: Nothing

  Sets the read only behavior of edit in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomEdit.SetReadOnly(const ACustomEdit: TCustomEdit;
  NewReadOnly: boolean);
begin
  if not CheckHandle(ACustomEdit, Self, 'SetReadOnly') then Exit;

  SetControlData(AsControlRef(ACustomEdit.Handle), kControlEntireControl,
    kControlEditTextLockedTag, SizeOf(Boolean), @NewReadOnly);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomEdit.SetSelStart
  Params:  ACustomEdit - LCL custom edit
           NewStart    - New position of selection start
  Returns: Nothing

  Sets the new position of selection start of edit in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomEdit.SetSelStart(const ACustomEdit: TCustomEdit;
  NewStart: integer);
begin
  if not CheckHandle(ACustomEdit, Self, 'SetSelStart') then Exit;

  TCarbonEdit(ACustomEdit.Handle).SetSelStart(NewStart);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomEdit.SetSelLength
  Params:  ACustomEdit - LCL custom edit
           NewLength   - New length of selection
  Returns: Nothing

  Sets the new length of selection of edit in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomEdit.SetSelLength(const ACustomEdit: TCustomEdit;
  NewLength: integer);
begin
  if not CheckHandle(ACustomEdit, Self, 'SetSelLength') then Exit;

  TCarbonEdit(ACustomEdit.Handle).SetSelLength(NewLength);
end;

{ TCarbonWSCustomMemo }

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomMemo.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new memo in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomMemo.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonMemo.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomMemo.GetStrings
  Params:  ACustomMemo - LCL custom memo
  Returns: Strings of memo in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomMemo.GetStrings(const ACustomMemo: TCustomMemo
  ): TStrings;
begin
  Result := nil;
  if not CheckHandle(ACustomMemo, Self, 'GetStrings') then Exit;

  Result := TCarbonMemoStrings.Create(TCarbonMemo(ACustomMemo.Handle));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomMemo.AppendText
  Params:  ACustomMemo - LCL custom memo
           AText       - Text to append
  Returns: Nothing

  Appends the specified text to memo in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomMemo.AppendText(const ACustomMemo: TCustomMemo;
  const AText: string);
var
  S: String;
begin
  if not CheckHandle(ACustomMemo, Self, 'AppendText') then Exit;

  if Length(AText) > 0 then
  begin
    if TCarbonMemo(ACustomMemo.Handle).GetText(S) then
      TCarbonMemo(ACustomMemo.Handle).SetText(S + AText);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomMemo.SetPasswordChar
  Params:  ACustomEdit - LCL custom edit
           NewChar     - New password char
  Returns: Nothing

  Sets the new password char of memo in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomMemo.SetPasswordChar(
  const ACustomEdit: TCustomEdit; NewChar: char);
begin
  if not CheckHandle(ACustomEdit, Self, 'SetPasswordChar') then Exit;

  TXNEchoMode(HITextViewGetTXNObject(AsControlRef(ACustomEdit.Handle)),
    UniChar(NewChar), CreateTextEncoding(kTextEncodingUnicodeDefault,
    kUnicodeNoSubset, kUnicodeUTF8Format), NewChar <> #0);
    
  TCarbonWidget(ACustomEdit.Handle).Invalidate;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomMemo.SetScrollbars
  Params:  ACustomEdit   - LCL custom memo
           NewScrollbars - New scrollbars style
  Returns: Nothing

  Sets the new scrollbars style of memo in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomMemo.SetScrollbars(
  const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle);
begin
  if not CheckHandle(ACustomMemo, Self, 'SetScrollbars') then Exit;
  
  TCarbonMemo(ACustomMemo.Handle).ScrollBars := NewScrollbars;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomMemo.SetReadOnly
  Params:  ACustomEdit - LCL custom edit
           NewReadOnly - Read only behavior
  Returns: Nothing

  Sets the read only behavior of memo in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomMemo.SetReadOnly(const ACustomEdit: TCustomEdit;
  NewReadOnly: boolean);
var
  Tag: TXNControlTag;
  Data: TXNControlData;
begin
  if not CheckHandle(ACustomEdit, Self, 'SetReadOnly') then Exit;

  Tag := kTXNNoUserIOTag;
  if NewReadOnly then
    Data.uValue := UInt32(kTXNReadOnly)
  else
    Data.uValue := UInt32(kTXNReadWrite);

  TXNSetTXNObjectControls(HITextViewGetTXNObject(
    AsControlRef(ACustomEdit.Handle)), False, 1, @Tag, @Data);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomMemo.SetWordWrap
  Params:  ACustomMemo - LCL custom memo
           NewWordWrap - New word wrap
  Returns: Nothing

  Sets the word wrap of memo in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomMemo.SetWordWrap(const ACustomMemo: TCustomMemo;
  const NewWordWrap: boolean);
var
  Tag: TXNControlTag;
  Data: TXNControlData;
begin
  if not CheckHandle(ACustomMemo, Self, 'SetWordWrap') then Exit;

  Tag := kTXNWordWrapStateTag;
  if NewWordWrap then
    Data.uValue := UInt32(kTXNAutoWrap)
  else
    Data.uValue := UInt32(kTXNNoAutoWrap);

  TXNSetTXNObjectControls(HITextViewGetTXNObject(
    AsControlRef(ACustomMemo.Handle)), False, 1, @Tag, @Data);
    
  TCarbonWidget(ACustomMemo.Handle).Invalidate;
end;

{ TCarbonWSCustomCheckBox }

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomCheckBox.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new check box in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomCheckBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonCheckBox.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomCheckBox.RetrieveState
  Params:  ACustomCheckBox - LCL custom check box
  Returns: State of check box

  Retrieves the state of check box in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomCheckBox.RetrieveState(
  const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
begin
  Result := cbUnchecked;
  if not CheckHandle(ACustomCheckBox, Self, 'RetrieveState') then Exit;

  case GetControl32BitValue(AsControlRef(ACustomCheckBox.Handle)) of
    kControlCheckBoxCheckedValue   : Result := cbChecked;
    kControlCheckBoxUncheckedValue : Result := cbUnchecked;
    kControlCheckBoxMixedValue     : Result := cbGrayed;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomCheckBox.SetState
  Params:  ACustomCheckBox - LCL custom check box
           NewState        - New state of check box
  Returns: Nothing

  Sets the new state of check box in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomCheckBox.SetState(
  const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
var
  Value: UInt32;
begin
  if not CheckHandle(ACustomCheckBox, Self, 'SetState') then Exit;

  case NewState of
    cbChecked  : Value := kControlCheckBoxCheckedValue;
    cbUnChecked: Value := kControlCheckBoxUncheckedValue;
    cbGrayed   : Value := kControlCheckBoxMixedValue;
  end;
  SetControl32BitValue(AsControlRef(ACustomCheckBox.Handle), Value);
end;


{ TCarbonWSToggleBox }

{------------------------------------------------------------------------------
  Method:  TCarbonWSToggleBox.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new toggle push button in Carbon interface with the specified
  parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSToggleBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonToggleBox.Create(AWinControl, AParams));
end;

{ TCarbonWSRadioButton }

{------------------------------------------------------------------------------
  Method:  TCarbonWSRadioButton.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new radio button in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSRadioButton.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonRadioButton.Create(AWinControl, AParams));
end;

{ TCarbonWSCustomStaticText }

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomStaticText.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new static text in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomStaticText.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonStaticText.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomStaticText.SetAlignment
  Params:  ACustomStaticText - LCL custom static text
           NewAlignment      - New caption alignment
  Returns: Nothing

  Sets the new cpation alignment of static text in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomStaticText.SetAlignment(
  const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment);
var
  FontStyle: ControlFontStyleRec;
begin
  if not CheckHandle(ACustomStaticText, Self, 'SetAlignment') then Exit;

  // get static text font style and change only justification
  GetControlData(AsControlRef(ACustomStaticText.Handle), kControlEntireControl,
    kControlStaticTextStyleTag, SizeOf(FontStyle), @FontStyle, nil);

  FontStyle.flags := FontStyle.flags or kControlUseJustMask;
  case NewAlignment of
  taLeftJustify : FontStyle.just := teFlushLeft;
  taRightJustify: FontStyle.just := teFlushRight;
  taCenter      : FontStyle.just := teCenter;
  end;

  SetControlData(AsControlRef(ACustomStaticText.Handle), kControlEntireControl,
    kControlStaticTextStyleTag, SizeOf(FontStyle), @FontStyle);
  // invalidate static text
  TCarbonWidget(ACustomStaticText.Handle).Invalidate;
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TScrollBar, TCarbonWSScrollBar);
  RegisterWSComponent(TCustomGroupBox, TCarbonWSCustomGroupBox);
//  RegisterWSComponent(TGroupBox, TCarbonWSGroupBox);
  RegisterWSComponent(TCustomComboBox, TCarbonWSCustomComboBox);
//  RegisterWSComponent(TComboBox, TCarbonWSComboBox);
  RegisterWSComponent(TCustomListBox, TCarbonWSCustomListBox);
//  RegisterWSComponent(TListBox, TCarbonWSListBox);
  RegisterWSComponent(TCustomEdit, TCarbonWSCustomEdit);
  RegisterWSComponent(TCustomMemo, TCarbonWSCustomMemo);
//  RegisterWSComponent(TEdit, TCarbonWSEdit);
//  RegisterWSComponent(TMemo, TCarbonWSMemo);
//  RegisterWSComponent(TCustomLabel, TCarbonWSCustomLabel);
//  RegisterWSComponent(TLabel, TCarbonWSLabel);
//  RegisterWSComponent(TButtonControl, TCarbonWSButtonControl);
  RegisterWSComponent(TCustomCheckBox, TCarbonWSCustomCheckBox);
//  RegisterWSComponent(TCheckBox, TCarbonWSCheckBox);
  RegisterWSComponent(TToggleBox, TCarbonWSToggleBox);
  RegisterWSComponent(TRadioButton, TCarbonWSRadioButton);
  RegisterWSComponent(TCustomStaticText, TCarbonWSCustomStaticText);
//  RegisterWSComponent(TStaticText, TCarbonWSStaticText);
////////////////////////////////////////////////////
end.
