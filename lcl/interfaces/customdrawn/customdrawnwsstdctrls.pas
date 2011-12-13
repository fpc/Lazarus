{
 *****************************************************************************
 *                         CustomDrawnWSStdCtrls.pp                          *
 *                              ---------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CustomDrawnWSStdCtrls;

{$mode objfpc}{$H+}

interface

//{$I qtdefines.inc}

uses
  // RTL
  math,
  // LCL
  Classes, Types, StdCtrls, Controls, Forms, SysUtils, InterfaceBase, LCLType,
  customdrawncontrols, LCLProc,
  // Widgetset
  WSProc, WSStdCtrls, WSLCLClasses, CustomDrawnWsControls, customdrawnproc,
  customdrawnprivate;

type

  { TCDWSScrollBar }

  TCDWSScrollBar = class(TWSScrollBar)
  published
{    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetKind(const AScrollBar: TCustomScrollBar; const AIsHorizontal: Boolean); override;
    class procedure SetParams(const AScrollBar: TCustomScrollBar); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;}
  end;

  { TCDWSCustomGroupBox }

  TCDWSCustomGroupBox = class(TWSCustomGroupBox)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
{    class function GetDefaultClientRect(const AWinControl: TWinControl;
             const aLeft, aTop, aWidth, aHeight: integer; var aClientRect: TRect
             ): boolean; override;}
    class procedure ShowHide(const AWinControl: TWinControl); override;
  end;

  { TCDWSGroupBox }

  TCDWSGroupBox = class(TWSGroupBox)
  published
  end;

  { TCDWSCustomComboBox }

  TCDWSCustomComboBox = class(TWSCustomComboBox)
  published
{    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class function GetDroppedDown(const ACustomComboBox: TCustomComboBox
       ): Boolean; override;
    class function GetItemIndex(const ACustomComboBox: TCustomComboBox): integer; override;
    class function GetItems(const ACustomComboBox: TCustomComboBox): TStrings; override;
    class function GetMaxLength(const ACustomComboBox: TCustomComboBox): integer; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
      var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
    class function GetSelStart(const ACustomComboBox: TCustomComboBox): integer; override;
    class function GetSelLength(const ACustomComboBox: TCustomComboBox): integer; override;
    class procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); override;
    class procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;

    class procedure SetArrowKeysTraverseList(const ACustomComboBox: TCustomComboBox;
      NewTraverseList: boolean); override;
    class procedure SetDropDownCount(const ACustomComboBox: TCustomComboBox; NewCount: Integer); override;
    class procedure SetDroppedDown(const ACustomComboBox: TCustomComboBox;
       ADroppedDown: Boolean); override;
    class procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer); override;
    class procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle); override;

    class procedure Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean); override;}
  end;

  { TCDWSComboBox }

  TCDWSComboBox = class(TWSComboBox)
  published
  end;

  { TCDWSCustomListBox }

  TCDWSCustomListBox = class(TWSCustomListBox)
  published
{    class function  CreateHandle(const AWinControl: TWinControl;
     const AParams: TCreateParams): TLCLIntfHandle; override;
    class function GetIndexAtXY(const ACustomListBox: TCustomListBox; X, Y: integer): integer; override;
    class function GetItemIndex(const ACustomListBox: TCustomListBox): integer; override;
    class function GetItemRect(const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect): boolean; override;
    class function GetScrollWidth(const ACustomListBox: TCustomListBox): Integer; override;
    class function GetSelCount(const ACustomListBox: TCustomListBox): integer; override;
    class function GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean; override;
    class function GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
    class function GetTopIndex(const ACustomListBox: TCustomListBox): integer; override;

    class procedure SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean); override;
    class procedure SetBorder(const ACustomListBox: TCustomListBox); override;
    class procedure SetColumnCount(const ACustomListBox: TCustomListBox; ACount: Integer); override;
    class procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer); override;
    class procedure SetScrollWidth(const ACustomListBox: TCustomListBox; const AScrollWidth: Integer); override;
    class procedure SetSelectionMode(const ACustomListBox: TCustomListBox; const AExtendedSelect, AMultiSelect: boolean); override;
    class procedure SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean); override;
    class procedure SetStyle(const ACustomListBox: TCustomListBox); override;
    class procedure SetTopIndex(const ACustomListBox: TCustomListBox; const NewTopIndex: integer); override;}
  end;

  { TCDWSListBox }

  TCDWSListBox = class(TWSListBox)
  published
  end;

  { TCDWSCustomEdit }

  TCDWSCustomEdit = class(TWSCustomEdit)
  published
{    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure SetAlignment(const ACustomEdit: TCustomEdit; const AAlignment: TAlignment); override;
    class function GetCaretPos(const ACustomEdit: TCustomEdit): TPoint; override;
    class function GetCanUndo(const ACustomEdit: TCustomEdit): Boolean; override;
    class procedure SetCaretPos(const ACustomEdit: TCustomEdit; const NewPos: TPoint); override;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); override;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    class function GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function GetSelLength(const ACustomEdit: TCustomEdit): integer; override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;

    //class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
    class procedure Cut(const ACustomEdit: TCustomEdit); override;
    class procedure Copy(const ACustomEdit: TCustomEdit); override;
    class procedure Paste(const ACustomEdit: TCustomEdit); override;
    class procedure Undo(const ACustomEdit: TCustomEdit); override;}
  end;

  { TCDWSCustomMemo }

  TCDWSCustomMemo = class(TWSCustomMemo)
  published
{    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure AppendText(const ACustomMemo: TCustomMemo; const AText: string); override;
    class function GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;
    class procedure SetAlignment(const ACustomEdit: TCustomEdit; const AAlignment: TAlignment); override;
    class procedure SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle); override;
    class procedure SetWantReturns(const ACustomMemo: TCustomMemo; const NewWantReturns: boolean); override;
    class procedure SetWantTabs(const ACustomMemo: TCustomMemo; const NewWantTabs: boolean); override;
    class procedure SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean); override;}
  end;

  { TCDWSEdit }

  TCDWSEdit = class(TWSEdit)
  published
  end;

  { TCDWSMemo }

  TCDWSMemo = class(TWSMemo)
  published
  end;

  { TCDWSButtonControl }

  TCDWSButtonControl = class(TWSButtonControl)
  published
  end;

  { TCDWSButton }

  TCDWSButton = class(TWSButton)
  public
    class procedure CreateCDControl(const AWinControl: TWinControl; var ACDControlField: TCDControl);
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
//    class procedure SetDefault(const AButton: TCustomButton; ADefault: Boolean); override;
//    class procedure SetShortcut(const AButton: TCustomButton; const ShortCutK1, ShortCutK2: TShortcut); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
  end;

  { TCDWSCustomCheckBox }

  TCDWSCustomCheckBox = class(TWSCustomCheckBox)
  public
    class procedure CreateCDControl(const AWinControl: TWinControl; var ACDControlField: TCDControl);
  published
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ShowHide(const AWinControl: TWinControl); override;

    class procedure GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;

{    class procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox; const ShortCutK1, ShortCutK2: TShortCut); override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;

    class function RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;}
  end;

  { TCDWSCheckBox }

  TCDWSCheckBox = class(TWSCheckBox)
  published
  end;

  { TCDWSToggleBox }

  TCDWSToggleBox = class(TWSToggleBox)
  published
{    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;

    class procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox; const ShortCutK1, ShortCutK2: TShortCut); override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;

    class function  RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;}
  end;

  { TCDWSRadioButton }

  TCDWSRadioButton = class(TWSRadioButton)
  published
{    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;

    class procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox; const ShortCutK1, ShortCutK2: TShortCut); override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;

    class function RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;}
  end;

  { TCDWSCustomStaticText }

  TCDWSCustomStaticText = class(TWSCustomStaticText)
  public
    class procedure CreateCDControl(const AWinControl: TWinControl; var ACDControlField: TCDControl);
  published
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ShowHide(const AWinControl: TWinControl); override;

{    class procedure SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment); override;
    class procedure SetStaticBorderStyle(const ACustomStaticText: TCustomStaticText; const NewBorderStyle: TStaticBorderStyle); override;}
  end;

  { TCDWSStaticText }

  TCDWSStaticText = class(TWSStaticText)
  published
  end;


implementation

{ TCDWSScrollBar }

(*{------------------------------------------------------------------------------
  Method: TCDWSCustomScrollBar.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TCDWSScrollBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  QtScrollBar: TQtScrollBar;
begin
  QtScrollBar := TQtScrollBar.Create(AWinControl, AParams);

  QtScrollBar.AttachEvents;
  
  case TScrollBar(AWinControl).Kind of
    sbHorizontal: QtScrollBar.SetOrientation(QtHorizontal);
    sbVertical: QtScrollBar.SetOrientation(QtVertical);
  end;
  
  Result := TLCLIntfHandle(QtScrollbar);
end;

class procedure TCDWSScrollBar.SetKind(const AScrollBar: TCustomScrollBar;
  const AIsHorizontal: Boolean);
var
  QtScrollBar: TQtScrollBar;
begin
  if not WSCheckHandleAllocated(AScrollBar, 'SetKind') then
    Exit;
  QtScrollBar := TQtScrollBar(AScrollBar.Handle);
  QtScrollBar.BeginUpdate;
  try
    case AScrollBar.Kind of
      sbHorizontal:
      begin
        if QtScrollBar.getOrientation <> QtHorizontal then
          QtScrollBar.SetOrientation(QtHorizontal);
        if QtScrollBar.getInvertedAppereance then
          QtScrollBar.setInvertedAppereance(False);
        if QtScrollbar.getInvertedControls then
          QtScrollBar.setInvertedControls(False);
      end;
      sbVertical:
      begin
        if QtScrollBar.getOrientation <> QtVertical then
          QtScrollBar.SetOrientation(QtVertical);
        if QtScrollBar.getInvertedAppereance then
          QtScrollBar.setInvertedAppereance(False);
        if not QtScrollbar.getInvertedControls then
          QtScrollBar.setInvertedControls(True);
      end;
    end;
  finally
    QtScrollbar.EndUpdate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomScrollBar.SetParams
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSScrollBar.SetParams(const AScrollBar: TCustomScrollBar);
var
  QtScrollBar: TQtScrollBar;
begin
  if not WSCheckHandleAllocated(AScrollBar, 'SetParams') then
    Exit;
  QtScrollBar := TQtScrollBar(AScrollBar.Handle);	

  QtScrollBar.BeginUpdate;
  try
    if (QtScrollBar.getMin <> AScrollBar.Min) or
      (QtScrollBar.getMax <> AScrollbar.Max) then
      QtScrollBar.setRange(AScrollBar.Min, AScrollBar.Max);
    if QtScrollBar.getPageStep <> AScrollBar.PageSize then
    begin
      QtScrollBar.setPageStep(AScrollBar.PageSize);
      QtScrollBar.setSingleStep((AScrollBar.PageSize div 6) + 1);
    end;
    if QtScrollbar.getValue <> AScrollBar.Position then
      QtScrollBar.setValue(AScrollBar.Position);

    case AScrollBar.Kind of
      sbHorizontal:
      begin
        if QtScrollBar.getOrientation <> QtHorizontal then
          QtScrollBar.SetOrientation(QtHorizontal);
        if QtScrollBar.getInvertedAppereance then
          QtScrollBar.setInvertedAppereance(False);
        if QtScrollbar.getInvertedControls then
          QtScrollBar.setInvertedControls(False);
      end;
      sbVertical:
      begin
        if QtScrollBar.getOrientation <> QtVertical then
          QtScrollBar.SetOrientation(QtVertical);
        if QtScrollBar.getInvertedAppereance then
          QtScrollBar.setInvertedAppereance(False);
        if not QtScrollbar.getInvertedControls then
          QtScrollBar.setInvertedControls(True);
      end;
    end;
  finally
    QtScrollbar.EndUpdate;
  end;
end;

class procedure TCDWSScrollBar.ShowHide(const AWinControl: TWinControl);
var
  Widget: TQtWidget;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'ShowHide') then
    Exit;

  Widget := TQtWidget(AWinControl.Handle);

  {reapply params just before visible since slider isn't updated
   properly sometimes.}
  if AWinControl.HandleObjectShouldBeVisible then
    SetParams(TCustomScrollBar(AWinControl));

  Widget.BeginUpdate;
  Widget.setVisible(AWinControl.HandleObjectShouldBeVisible);
  Widget.EndUpdate;
end;

{ TCDWSCustomListBox }

{------------------------------------------------------------------------------
  Method: TCDWSCustomListBox.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TCDWSCustomListBox.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  QtListWidget: TQtListWidget;
  SelMode: QAbstractItemViewSelectionMode;
begin
  QtListWidget := TQtListWidget.Create(AWinControl, AParams);
  
  if TCustomListBox(AWinControl).MultiSelect then
    if TCustomListBox(AWinControl).ExtendedSelect then
      SelMode := QAbstractItemViewExtendedSelection
    else
      SelMode := QAbstractItemViewMultiSelection
  else
    SelMode := QAbstractItemViewSingleSelection;

  QtListWidget.setSelectionMode(SelMode);

  //Set BorderStyle according to the provided Params
  if (AParams.ExStyle and WS_EX_CLIENTEDGE) > 0 then
    QtListWidget.setFrameShape(QFrameStyledPanel)
  else
    QtListWidget.setFrameShape(QFrameNoFrame);

  QtListWidget.AttachEvents;
  
  // create our FList helper
  QtListWidget.FList := TQtListStrings.Create(AWinControl, QtListWidget);

  QtListWidget.OwnerDrawn := TCustomListBox(AWinControl).Style in [lbOwnerDrawFixed, lbOwnerDrawVariable];

  Result := TLCLIntfHandle(QtListWidget);
end;

class function TCDWSCustomListBox.GetIndexAtXY(
  const ACustomListBox: TCustomListBox; X, Y: integer): integer;
var
  APoint: TQtPoint;
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'GetIndexAtXY') then
    Exit(-1);
  APoint := QtPoint(X, Y);
  Result := TQtListWidget(ACustomListBox.Handle).indexAt(@APoint);
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomListBox.GetSelCount
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TCDWSCustomListBox.GetSelCount(const ACustomListBox: TCustomListBox): integer;
var
  QtListWidget: TQtListWidget;
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'GetSelCount') then
    Exit(0);
  QtListWidget := TQtListWidget(ACustomListBox.Handle);
  Result := QtListWidget.getSelCount;
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomListBox.GetSelected
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TCDWSCustomListBox.GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean;
var
  QtListWidget: TQtListWidget;
  ListItem: QListWidgetItemH;
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'GetSelected') then
    Exit(False);
  QtListWidget := TQtListWidget(ACustomListBox.Handle);
  ListItem := QtListWidget.getItem(AIndex);
  Result := QtListWidget.getItemSelected(ListItem);
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomListBox.GetStrings
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TCDWSCustomListBox.GetStrings(const ACustomListBox: TCustomListBox): TStrings;
var
  ListWidget: TQtListWidget;
begin
  Result := nil;
  if not WSCheckHandleAllocated(ACustomListBox, 'GetStrings') then
    Exit;
  ListWidget := TQtListWidget(ACustomListBox.Handle);
  if not Assigned(ListWidget.FList) then
    ListWidget.FList := TQtListStrings.Create(ACustomListBox, ListWidget);

  Result := ListWidget.FList;
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomListBox.GetItemIndex
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TCDWSCustomListBox.GetItemIndex(const ACustomListBox: TCustomListBox): integer;
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'GetItemIndex') then
    Exit(-1);
  Result := TQtListWidget(ACustomListBox.Handle).currentRow;
end;

class function TCDWSCustomListBox.GetItemRect(
  const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect
  ): boolean;
var
  QtListWidget: TQtListWidget;
  Item: QListWidgetItemH;
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'GetItemRect') then
    Exit(False);
  QtListWidget := TQtListWidget(ACustomListBox.Handle);
  Item := QtListWidget.getItem(Index);
  Result := Item <> nil;
  if Result then
    ARect := QtListWidget.getVisualItemRect(Item)
  else
    ARect := Rect(-1,-1,-1,-1);
end;

class function TCDWSCustomListBox.GetScrollWidth(
  const ACustomListBox: TCustomListBox): Integer;
var
  QtListWidget: TQtListWidget;
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'GetScrollWidth') then
    Exit(0);
  QtListWidget := TQtListWidget(ACustomListBox.Handle);
  Result := QtListWidget.horizontalScrollBar.getMax;
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomListBox.GetTopIndex
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TCDWSCustomListBox.GetTopIndex(const ACustomListBox: TCustomListBox): integer;
begin
  Result := 0;
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomListBox.SelectItem
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomListBox.SelectItem(const ACustomListBox: TCustomListBox;
  AIndex: integer; ASelected: boolean);
var
  QtListWidget: TQtListWidget;
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'SelectItem') then
    Exit;
  QtListWidget := TQtListWidget(ACustomListBox.Handle);
  QtListWidget.Selected[AIndex] := ASelected;
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomListBox.SetBorder
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomListBox.SetBorder(const ACustomListBox: TCustomListBox);
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'SetBorder') then
    Exit;
  TCDWSWinControl.SetBorderStyle(ACustomListBox, ACustomListBox.BorderStyle);
end;

class procedure TCDWSCustomListBox.SetColumnCount(const ACustomListBox: TCustomListBox;
  ACount: Integer);
{var
  QtListWidget: TQtListWidget;
  AModel: QAbstractItemModelH;}
begin
  {$note implement TCDWSCustomListBox.SetColumnCount}
{  QtListWidget := TQtListWidget(ACustomListBox.Handle);
  AModel := QtListWidget.getModel;

  if QAbstractItemModel_columnCount(AModel) <> ACount then
    QAbstractItemModel_insertColumns(AModel, 0, ACount);
}
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomListBox.SetItemIndex
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomListBox.SetItemIndex(const ACustomListBox: TCustomListBox;
  const AIndex: integer);
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'SetItemIndex') then
    Exit;
  TQtListWidget(ACustomListBox.Handle).setCurrentRow(AIndex);
end;

class procedure TCDWSCustomListBox.SetScrollWidth(const ACustomListBox: TCustomListBox; const AScrollWidth: Integer);
const
  BoolToPolicy: array[Boolean] of QtScrollBarPolicy = (QtScrollBarAlwaysOff, QtScrollBarAlwaysOn);
var
  QtListWidget: TQtListWidget;
  ClientWidth: Integer;
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'SetScrollWidth') then
    Exit;
  QtListWidget := TQtListWidget(ACustomListBox.Handle);
  QtListWidget.horizontalScrollBar.setMaximum(AScrollWidth);
  with QtListWidget.getClientBounds do
    ClientWidth := Right - Left;
  QtListWidget.ScrollBarPolicy[False] := BoolToPolicy[AScrollWidth > ClientWidth];
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomListBox.SetSelectionMode
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomListBox.SetSelectionMode(
  const ACustomListBox: TCustomListBox; const AExtendedSelect, AMultiSelect: boolean);
var
  QtListWidget: TQtListWidget;
  SelMode: QAbstractItemViewSelectionMode;
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'SetSelectionMode') then
    Exit;
  QtListWidget := TQtListWidget(ACustomListBox.Handle);

  if AMultiSelect then
    if AExtendedSelect then
      SelMode := QAbstractItemViewExtendedSelection
    else
      SelMode := QAbstractItemViewMultiSelection
  else
    SelMode := QAbstractItemViewSingleSelection;

  QtListWidget.setSelectionMode(SelMode);
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomListBox.SetSorted
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomListBox.SetSorted(const ACustomListBox: TCustomListBox;
  AList: TStrings; ASorted: boolean);
begin
  TQtListStrings(AList).Sorted := ASorted;
end;

class procedure TCDWSCustomListBox.SetStyle(const ACustomListBox: TCustomListBox);
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'SetStyle') then
    Exit;
  TQtListWidget(ACustomListBox.Handle).OwnerDrawn :=
    ACustomListBox.Style in [lbOwnerDrawFixed, lbOwnerDrawVariable];
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomListBox.SetTopIndex
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomListBox.SetTopIndex(const ACustomListBox: TCustomListBox;
  const NewTopIndex: integer);
begin
  if not WSCheckHandleAllocated(ACustomListBox, 'SetTopIndex') then
    Exit;
  TQtListWidget(ACustomListBox.Handle).scrollToItem(NewTopIndex,
    QAbstractItemViewPositionAtTop);
end;

{ TCDWSCustomMemo }

{------------------------------------------------------------------------------
  Method: TCDWSCustomMemo.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TCDWSCustomMemo.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  QtTextEdit: TQtTextEdit;
begin
  QtTextEdit := TQtTextEdit.Create(AWinControl, AParams);
  QtTextEdit.ClearText;
  QtTextEdit.setBorder(TCustomMemo(AWinControl).BorderStyle = bsSingle);
  QtTextEdit.setReadOnly(TCustomMemo(AWinControl).ReadOnly);
  QtTextEdit.setLineWrapMode(WordWrapMap[TCustomMemo(AWinControl).WordWrap]);
  // create our FList helper
  QtTextEdit.FList := TQtMemoStrings.Create(TCustomMemo(AWinControl));
  QtTextEdit.setScrollStyle(TCustomMemo(AWinControl).ScrollBars);
  QtTextEdit.setTabChangesFocus(not TCustomMemo(AWinControl).WantTabs);

  QtTextEdit.AttachEvents;

  Result := TLCLIntfHandle(QtTextEdit);
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomMemo.AppendText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomMemo.AppendText(const ACustomMemo: TCustomMemo; const AText: string);
var
  AStr: WideString;
begin
  if not WSCheckHandleAllocated(ACustomMemo, 'AppendText') or (Length(AText) = 0) then
    Exit;
  AStr := GetUtf8String(AText);
  TQtTextEdit(ACustomMemo.Handle).BeginUpdate;
  TQtTextEdit(ACustomMemo.Handle).Append(AStr);
  TQtTextEdit(ACustomMemo.Handle).EndUpdate;
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomMemo.GetStrings
  Params:  None
  Returns: Memo Contents as TStrings
 ------------------------------------------------------------------------------}
class function TCDWSCustomMemo.GetStrings(const ACustomMemo: TCustomMemo): TStrings;
begin
  if not WSCheckHandleAllocated(ACustomMemo, 'GetStrings') then
    Exit;
  if not Assigned(TQtTextEdit(ACustomMemo.Handle).FList) then
    TQtTextEdit(ACustomMemo.Handle).FList := TQtMemoStrings.Create(ACustomMemo);
  
  Result := TQtTextEdit(ACustomMemo.Handle).FList;
end;

class procedure TCDWSCustomMemo.SetAlignment(const ACustomEdit: TCustomEdit;
  const AAlignment: TAlignment);
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetAlignment') then
    Exit;
  TQtTextEdit(ACustomEdit.Handle).setAlignment(AlignmentMap[AAlignment]);
end;

class procedure TCDWSCustomMemo.SetScrollbars(const ACustomMemo: TCustomMemo;
  const NewScrollbars: TScrollStyle);
begin
  if not WSCheckHandleAllocated(ACustomMemo, 'SetScrollBars') then
    Exit;
  TQtTextEdit(ACustomMemo.Handle).setScrollStyle(NewScrollBars);
end;

class procedure TCDWSCustomMemo.SetWantReturns(const ACustomMemo: TCustomMemo;
  const NewWantReturns: boolean);
begin
  if not WSCheckHandleAllocated(ACustomMemo, 'SetWantReturns') then
    Exit;
  with TQtTextEdit(ACustomMemo.Handle) do
  begin
    if NewWantReturns then
      KeysToEat := KeysToEat - [VK_RETURN]
    else
      KeysToEat := KeysToEat + [VK_RETURN];
  end;
end;

class procedure TCDWSCustomMemo.SetWantTabs(const ACustomMemo: TCustomMemo;
  const NewWantTabs: boolean);
begin
  if not WSCheckHandleAllocated(ACustomMemo, 'SetWantTabs') then
    Exit;
  with TQtTextEdit(ACustomMemo.Handle) do
  begin
    setTabChangesFocus(not NewWantTabs);
    if NewWantTabs then
      KeysToEat := KeysToEat - [VK_TAB]
    else
      KeysToEat := KeysToEat + [VK_TAB];
  end;
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomMemo.SetWordWrap
  Params:  NewWordWrap boolean
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomMemo.SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean);
begin
  if not WSCheckHandleAllocated(ACustomMemo, 'SetWordWrap') then
    Exit;
  TQtTextEdit(ACustomMemo.Handle).setLineWrapMode(WordWrapMap[NewWordWrap]);
end;

{ TCDWSCustomEdit }

{------------------------------------------------------------------------------
  Method: TCDWSCustomEdit.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TCDWSCustomEdit.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  QtLineEdit: TQtLineEdit;
begin
  QtLineEdit := TQtLineEdit.Create(AWinControl, AParams);
  QtLineEdit.setBorder(TCustomEdit(AWinControl).BorderStyle = bsSingle);
  QtLineEdit.setAlignment(AlignmentMap[TCustomEdit(AWinControl).Alignment]);
  QtLineEdit.AttachEvents;

  Result := TLCLIntfHandle(QtLineEdit);
end;

class procedure TCDWSCustomEdit.SetAlignment(const ACustomEdit: TCustomEdit;
  const AAlignment: TAlignment);
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetAlignment') then
    Exit;
  TQtLineEdit(ACustomEdit.Handle).setAlignment(AlignmentMap[AAlignment]);
end;

class function TCDWSCustomEdit.GetCaretPos(const ACustomEdit: TCustomEdit
  ): TPoint;
var
  Widget: TQtWidget;
  QtEdit: IQtEdit;
begin
  Result := Point(0,0);
  if not WSCheckHandleAllocated(ACustomEdit, 'GetCaretPos') then
    Exit;
  Widget := TQtWidget(ACustomEdit.Handle);
  if Supports(Widget, IQtEdit, QtEdit) then
    Result.X := QtEdit.getCursorPosition;
end;

class function TCDWSCustomEdit.GetCanUndo(const ACustomEdit: TCustomEdit): Boolean;
var
  Widget: TQtWidget;
  QtEdit: IQtEdit;
begin
  Result := False;
  if not WSCheckHandleAllocated(ACustomEdit, 'GetCanUndo') then
    Exit;
  Widget := TQtWidget(ACustomEdit.Handle);
  if Supports(Widget, IQtEdit, QtEdit) then
    Result := QtEdit.isUndoAvailable;
end;

class procedure TCDWSCustomEdit.SetCaretPos(const ACustomEdit: TCustomEdit;
  const NewPos: TPoint);
var
  Widget: TQtWidget;
  QtEdit: IQtEdit;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetCaretPos') then
    Exit;
  Widget := TQtWidget(ACustomEdit.Handle);
  if Supports(Widget, IQtEdit, QtEdit) then
    QtEdit.setCursorPosition(NewPos.X);
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomEdit.SetEchoMode
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomEdit.SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode);
var
  Widget: TQtWidget;
  QtEdit: IQtEdit;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetEchoMode') then
    Exit;

  Widget := TQtWidget(ACustomEdit.Handle);
  if Supports(Widget, IQtEdit, QtEdit) then
    QtEdit.setEchoMode(QLineEditEchoMode(Ord(NewMode)));
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomEdit.SetMaxLength
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomEdit.SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer);
var
  Widget: TQtWidget;
  QtEdit: IQtEdit;
  MaxLength: Integer;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetMaxLength') then
    Exit;

  Widget := TQtWidget(ACustomEdit.Handle);
  if Supports(Widget, IQtEdit, QtEdit) then
  begin
    // qt doesn't accept -1
    MaxLength := QtEdit.getMaxLength;
    if (NewLength <= 0) or (NewLength > QtMaxEditLength) then
      NewLength := QtMaxEditLength;
    if NewLength <> MaxLength then
      QtEdit.setMaxLength(NewLength);
  end;
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomEdit.SetReadOnly
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomEdit.SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean);
var
  Widget: TQtWidget;
  QtEdit: IQtEdit;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetReadOnly') then
    Exit;

  Widget := TQtWidget(ACustomEdit.Handle);
  if Supports(Widget, IQtEdit, QtEdit) then
    QtEdit.setReadOnly(NewReadOnly);
end;

class function TCDWSCustomEdit.GetSelStart(const ACustomEdit: TCustomEdit): integer;
var
  Widget: TQtWidget;
  QtEdit: IQtEdit;
begin
  Result := 0;
  if not WSCheckHandleAllocated(ACustomEdit, 'GetSelStart') then
    Exit;

  Widget := TQtWidget(ACustomEdit.Handle);
  if Supports(Widget, IQtEdit, QtEdit) then
    Result := QtEdit.getSelectionStart;
end;

class function TCDWSCustomEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
var
  Widget: TQtWidget;
  QtEdit: IQtEdit;
begin
  Result := 0;
  if not WSCheckHandleAllocated(ACustomEdit, 'GetSelLength') then
    Exit;

  Widget := TQtWidget(ACustomEdit.Handle);
  if Supports(Widget, IQtEdit, QtEdit) then
    Result := QtEdit.getSelectionLength;
end;

class procedure TCDWSCustomEdit.SetSelStart(const ACustomEdit: TCustomEdit;
  NewStart: integer);
var
  Widget: TQtWidget;
  QtEdit: IQtEdit;
  ALength: Integer;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetSelStart') then
    Exit;

  Widget := TQtWidget(ACustomEdit.Handle);
  ALength := GetSelLength(ACustomEdit);
  if Supports(Widget, IQtEdit, QtEdit) then
    QtEdit.setSelection(NewStart, ALength);
end;

class procedure TCDWSCustomEdit.SetSelLength(const ACustomEdit: TCustomEdit;
  NewLength: integer);
var
  Widget: TQtWidget;
  QtEdit: IQtEdit;
  AStart: Integer;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetSelLength') then
    Exit;

  Widget := TQtWidget(ACustomEdit.Handle);
  AStart := GetSelStart(ACustomEdit);
  if Supports(Widget, IQtEdit, QtEdit) then
    QtEdit.setSelection(AStart, NewLength);
end;

class procedure TCDWSCustomEdit.Cut(const ACustomEdit: TCustomEdit);
var
  Widget: TQtWidget;
  QtEdit: IQtEdit;
begin
  Widget := TQtWidget(ACustomEdit.Handle);
  if Supports(Widget, IQtEdit, QtEdit) then
    QtEdit.Cut;
end;

class procedure TCDWSCustomEdit.Copy(const ACustomEdit: TCustomEdit);
var
  Widget: TQtWidget;
  QtEdit: IQtEdit;
begin
  Widget := TQtWidget(ACustomEdit.Handle);
  if Supports(Widget, IQtEdit, QtEdit) then
    QtEdit.Copy;
end;

class procedure TCDWSCustomEdit.Paste(const ACustomEdit: TCustomEdit);
var
  Widget: TQtWidget;
  QtEdit: IQtEdit;
begin
  Widget := TQtWidget(ACustomEdit.Handle);
  if Supports(Widget, IQtEdit, QtEdit) then
    QtEdit.Paste;
end;

class procedure TCDWSCustomEdit.Undo(const ACustomEdit: TCustomEdit);
var
  Widget: TQtWidget;
  QtEdit: IQtEdit;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'Undo') then
    Exit;
  Widget := TQtWidget(ACustomEdit.Handle);
  if Supports(Widget, IQtEdit, QtEdit) then
    QtEdit.Undo;
end;*)

{ TCDWSStaticText }

class procedure TCDWSCustomStaticText.CreateCDControl(
  const AWinControl: TWinControl; var ACDControlField: TCDControl);
begin
  ACDControlField := TCDStaticText.Create(AWinControl);
//    TCDIntfButton(lCDWinControl.CDControl).LCLButton := TButton(AWinControl);
  ACDControlField.Parent := AWinControl;
  ACDControlField.Caption := AWinControl.Caption;
  ACDControlField.Align := alClient;
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomStaticText.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TCDWSCustomStaticText.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  lCDWinControl: TCDWinControl;
begin
  Result := TCDWSWinControl.CreateHandle(AWinControl, AParams);
  lCDWinControl := TCDWinControl(Result);
end;

class procedure TCDWSCustomStaticText.ShowHide(const AWinControl: TWinControl);
var
  lCDWinControl: TCDWinControl;
begin
  lCDWinControl := TCDWinControl(AWinControl.Handle);

  TCDWSWinControl.ShowHide(AWinControl);

  if lCDWinControl.CDControl = nil then
    CreateCDControl(AWinControl, lCDWinControl.CDControl);
end;

(*{------------------------------------------------------------------------------
  Method: TCDWSCustomStaticText.SetAlignment
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomStaticText.SetAlignment(
  const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment);
begin
  TQtStaticText(ACustomStaticText.Handle).setAlignment(AlignmentMap[NewAlignment]);
end;

class procedure TCDWSCustomStaticText.SetStaticBorderStyle(
  const ACustomStaticText: TCustomStaticText;
  const NewBorderStyle: TStaticBorderStyle);
begin
  TQtStaticText(ACustomStaticText.Handle).setFrameShape(StaticBorderFrameShapeMap[NewBorderStyle]);
  TQtStaticText(ACustomStaticText.Handle).setFrameShadow(StaticBorderFrameShadowMap[NewBorderStyle]);
end;*)

{ TCDWSButton }

class procedure TCDWSButton.CreateCDControl(const AWinControl: TWinControl; var ACDControlField: TCDControl);
begin
  ACDControlField := TCDIntfButton.Create(AWinControl);
  TCDIntfButton(ACDControlField).LCLControl := TButton(AWinControl);
  ACDControlField.Caption := AWinControl.Caption;
  ACDControlField.Parent := AWinControl;
  ACDControlField.Align := alClient;
end;

{------------------------------------------------------------------------------
  Function: TCDWSButton.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TCDWSButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  lCDWinControl: TCDWinControl;
begin
  Result := TCDWSWinControl.CreateHandle(AWinControl, AParams);
  lCDWinControl := TCDWinControl(Result);
end;

class procedure TCDWSButton.ShowHide(const AWinControl: TWinControl);
var
  lCDWinControl: TCDWinControl;
begin
  lCDWinControl := TCDWinControl(AWinControl.Handle);

  TCDWSWinControl.ShowHide(AWinControl);

  if lCDWinControl.CDControl = nil then
    CreateCDControl(AWinControl, lCDWinControl.CDControl);
end;

(*class procedure TCDWSButton.SetDefault(const AButton: TCustomButton;
  ADefault: Boolean);
var
  QtPushButton: TQtPushButton;
begin
  if not WSCheckHandleAllocated(AButton, 'SetDefault') then Exit;
  QtPushButton := TQtPushButton(AButton.Handle);
  QtPushButton.SetDefault(ADefault);
end;

class procedure TCDWSButton.SetShortcut(const AButton: TCustomButton;
  const ShortCutK1, ShortCutK2: TShortcut);
begin
  if not WSCheckHandleAllocated(AButton, 'SetShortcut') then Exit;
  
  TQtPushButton(AButton.Handle).setShortcut(ShortCutK1, ShortCutK2);
end;*)

{ TCDWSCustomCheckBox }

class procedure TCDWSCustomCheckBox.CreateCDControl(const AWinControl: TWinControl; var ACDControlField: TCDControl);
begin
  ACDControlField := TCDCheckBox.Create(AWinControl);
//    TCDIntfButton(lCDWinControl.CDControl).LCLButton := TButton(AWinControl);
  ACDControlField.Parent := AWinControl;
  ACDControlField.Caption := AWinControl.Caption;
  ACDControlField.Align := alClient;
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomCheckBox.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TCDWSCustomCheckBox.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  lCDWinControl: TCDWinControl;
begin
  Result := TCDWSWinControl.CreateHandle(AWinControl, AParams);
  lCDWinControl := TCDWinControl(Result);
end;

class procedure TCDWSCustomCheckBox.ShowHide(const AWinControl: TWinControl);
var
  lCDWinControl: TCDWinControl;
begin
  lCDWinControl := TCDWinControl(AWinControl.Handle);

  TCDWSWinControl.ShowHide(AWinControl);

  if lCDWinControl.CDControl = nil then
    CreateCDControl(AWinControl, lCDWinControl.CDControl);
end;

class procedure TCDWSCustomCheckBox.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
var
  lCDWinControl: TCDWinControl;
begin
  lCDWinControl := TCDWinControl(AWinControl.Handle);

  if lCDWinControl.CDControl = nil then
    CreateCDControl(AWinControl, lCDWinControl.CDControl);

  lCDWinControl.CDControl.LCLWSCalculatePreferredSize(
    PreferredWidth, PreferredHeight, WithThemeSpace, AWinControl.AutoSize);
  DebugLn(Format('[TCDWSCustomCheckBox.GetPreferredSize] Width=%d Height=%d', [PreferredWidth, PreferredHeight]));
end;

{ TCDWSRadioButton }

(*{------------------------------------------------------------------------------
  Method: TCDWSRadioButton.RetrieveState
  Params:  None
  Returns: The state of the control
 ------------------------------------------------------------------------------}
class function TCDWSRadioButton.RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
begin
  if TQtRadioButton(ACustomCheckBox.Handle).isChecked then
    Result := cbChecked
  else
    Result := cbUnchecked;
end;

{------------------------------------------------------------------------------
  Method: TCDWSRadioButton.SetShortCut
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSRadioButton.SetShortCut(const ACustomCheckBox: TCustomCheckBox;
  const ShortCutK1, ShortCutK2: TShortCut);
begin
  TQtRadioButton(ACustomCheckBox.Handle).setShortcut(ShortCutK1, ShortCutK2);
end;

{------------------------------------------------------------------------------
  Method: TCDWSRadioButton.SetState
  Params:  None
  Returns: Nothing

  Sets the state of the control
 ------------------------------------------------------------------------------}
class procedure TCDWSRadioButton.SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
var
  QtRadioButton: TQtRadioButton;
begin
  //enclose the call between Begin/EndUpdate to avoid send LM_CHANGE message
  QtRadioButton := TQtRadioButton(ACustomCheckBox.Handle);
  QtRadioButton.BeginUpdate;
  QtRadioButton.setChecked(NewState = cbChecked);
  QtRadioButton.EndUpdate;
end;

{------------------------------------------------------------------------------
  Method: TCDWSRadioButton.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TCDWSRadioButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtRadioButton: TQtRadioButton;
begin
  QtRadioButton := TQtRadioButton.Create(AWinControl, AParams);
  QtRadioButton.AttachEvents;

  Result := TLCLIntfHandle(QtRadioButton);
end;*)

{ TCDWSCustomGroupBox }

{------------------------------------------------------------------------------
  Method: TCDWSCustomGroupBox.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TCDWSCustomGroupBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  lCDWinControl: TCDWinControl;
begin
  Result := TCDWSWinControl.CreateHandle(AWinControl, AParams);
  lCDWinControl := TCDWinControl(Result);
end;

(*class function TCDWSCustomGroupBox.GetDefaultClientRect(
  const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer;
  var aClientRect: TRect): boolean;
var
  dx, dy: integer;
begin
  Result:=false;
  if AWinControl.HandleAllocated then
  begin
  end else
  begin
    dx := QStyle_pixelMetric(QApplication_style(), QStylePM_LayoutLeftMargin) +
          QStyle_pixelMetric(QApplication_style(), QStylePM_LayoutRightMargin);
    dy := QStyle_pixelMetric(QApplication_style(), QStylePM_LayoutTopMargin) +
          QStyle_pixelMetric(QApplication_style(), QStylePM_LayoutBottomMargin);

    aClientRect:=Rect(0,0,
                 Max(0, aWidth - dx),
                 Max(0, aHeight - dy));
    Result:=true;
  end;
end;*)

class procedure TCDWSCustomGroupBox.ShowHide(const AWinControl: TWinControl);
var
  lCDWinControl: TCDWinControl;
begin
  lCDWinControl := TCDWinControl(AWinControl.Handle);

  TCDWSWinControl.ShowHide(AWinControl);

// if lCDWinControl.CDControl = nil then
//    CreateCDControl(AWinControl, lCDWinControl.CDControl);
end;

(*{ TCDWSCustomComboBox }

{------------------------------------------------------------------------------
  Method: TCDWSCustomComboBox.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TCDWSCustomComboBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtComboBox: TQtComboBox;
  ItemIndex: Integer;
  Text: String;
begin
  QtComboBox := TQtComboBox.Create(AWinControl, AParams);

  // create our FList helper
  QtComboBox.FList := TQtComboStrings.Create(AWinControl, QtComboBox);
  QtComboBox.setMaxVisibleItems(TCustomComboBox(AWinControl).DropDownCount);

  // load combo data imediatelly and set LCLs itemIndex and Text otherwise
  // qt will set itemindex to 0 if lcl itemindex = -1.
  ItemIndex := TCustomComboBox(AWinControl).ItemIndex;
  Text := TCustomComboBox(AWinControl).Text;
  QtComboBox.FList.Assign(TCustomComboBox(AWinControl).Items);
  QtComboBox.setCurrentIndex(ItemIndex);
  QtComboBox.setText(GetUTF8String(Text));
  QtComboBox.setEditable(AParams.Style and CBS_DROPDOWN <> 0);

  QtComboBox.AttachEvents;
  QtComboBox.OwnerDrawn := (AParams.Style and CBS_OWNERDRAWFIXED <> 0) or
    (AParams.Style and CBS_OWNERDRAWVARIABLE <> 0);

  Result := TLCLIntfHandle(QtComboBox);
end;

class function TCDWSCustomComboBox.GetDroppedDown(
  const ACustomComboBox: TCustomComboBox): Boolean;
var
  QtComboBox: TQtComboBox;
begin
  Result := False;
  if not ACustomComboBox.HandleAllocated then
    exit;
  QtComboBox := TQtComboBox(ACustomComboBox.Handle);
  Result := QtComboBox.getDroppedDown;
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomComboBox.GetItemIndex
  Params:  None
  Returns: The state of the control
 ------------------------------------------------------------------------------}
class function TCDWSCustomComboBox.GetItemIndex(
  const ACustomComboBox: TCustomComboBox): integer;
var
  QtComboBox: TQtComboBox;
  WStr: WideString;
  i: Integer;
begin
  Result := -1;
  if not WSCheckHandleAllocated(ACustomComboBox, 'GetItemIndex') then
    Exit;
  QtComboBox := TQtComboBox(ACustomComboBox.Handle);
  if QtComboBox.getEditable then
  begin
    WStr := QtComboBox.getText;
    i := QComboBox_findText(QComboBoxH(QtComboBox.Widget), @WStr);
    Result := i;
  end else
    Result := TQtComboBox(ACustomComboBox.Handle).currentIndex;
end;

class function TCDWSCustomComboBox.GetMaxLength(
  const ACustomComboBox: TCustomComboBox): integer;
var
  LineEdit: TQtLineEdit;
begin
  LineEdit := TQtComboBox(ACustomComboBox.Handle).LineEdit;
  if LineEdit <> nil then
  begin
    Result := LineEdit.getMaxLength;
    if Result = QtMaxEditLength then
      Result := 0;
  end
  else
    Result := 0;
end;

{------------------------------------------------------------------------------
  Set's the size of a TComboBox when autosized
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomComboBox.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  if AWinControl.HandleAllocated then
    TQtWidget(AWinControl.Handle).PreferredSize(PreferredWidth,
      PreferredHeight, WithThemeSpace);

  // The correct behavior for the LCL is not forcing any specific value for
  // TComboBox.Width, so we set it to zero to signal that here
  PreferredWidth := 0;
end;

class function TCDWSCustomComboBox.GetSelStart(const ACustomComboBox: TCustomComboBox): integer;
var
  Widget: TQtWidget;
  QtEdit: IQtEdit;
begin
  Result := 0;
  if not WSCheckHandleAllocated(ACustomComboBox, 'GetSelStart') then
    Exit;

  Widget := TQtWidget(ACustomComboBox.Handle);
  if Supports(Widget, IQtEdit, QtEdit) then
    Result := QtEdit.getSelectionStart;
end;

class function TCDWSCustomComboBox.GetSelLength(const ACustomComboBox: TCustomComboBox): integer;
var
  Widget: TQtWidget;
  QtEdit: IQtEdit;
begin
  Result := 0;
  if not WSCheckHandleAllocated(ACustomComboBox, 'GetSelLength') then
    Exit;

  Widget := TQtWidget(ACustomComboBox.Handle);
  if Supports(Widget, IQtEdit, QtEdit) then
    Result := QtEdit.getSelectionLength;
end;

class procedure TCDWSCustomComboBox.SetSelStart(const ACustomComboBox: TCustomComboBox;
   NewStart: integer);
var
  Widget: TQtWidget;
  QtEdit: IQtEdit;
  ALength: Integer;
begin
  if not WSCheckHandleAllocated(ACustomComboBox, 'SetSelStart') then
    Exit;

  Widget := TQtWidget(ACustomComboBox.Handle);
  ALength := GetSelLength(ACustomComboBox);
  if Supports(Widget, IQtEdit, QtEdit) then
    QtEdit.setSelection(NewStart, ALength);
end;

class procedure TCDWSCustomComboBox.SetSelLength(
  const ACustomComboBox: TCustomComboBox; NewLength: integer);
var
  Widget: TQtWidget;
  QtEdit: IQtEdit;
  AStart: Integer;
begin
  if not WSCheckHandleAllocated(ACustomComboBox, 'SetSelLength') then
    Exit;

  Widget := TQtWidget(ACustomComboBox.Handle);
  AStart := GetSelStart(ACustomComboBox);
  if Supports(Widget, IQtEdit, QtEdit) then
    QtEdit.setSelection(AStart, NewLength);
end;

class procedure TCDWSCustomComboBox.SetArrowKeysTraverseList(
  const ACustomComboBox: TCustomComboBox; NewTraverseList: boolean);
begin
  {$note implement TCDWSCustomComboBox.SetArrowKeysTraverseList}
end;

class procedure TCDWSCustomComboBox.SetDropDownCount(
  const ACustomComboBox: TCustomComboBox; NewCount: Integer);
begin
  TQtComboBox(ACustomComboBox.Handle).setMaxVisibleItems(NewCount);
end;

class procedure TCDWSCustomComboBox.SetDroppedDown(
  const ACustomComboBox: TCustomComboBox; ADroppedDown: Boolean);
var
  QtComboBox: TQtComboBox;
begin
  QtComboBox := TQtComboBox(ACustomComboBox.Handle);
  QtComboBox.setDroppedDown(ADroppedDown);
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomComboBox.SetItemIndex
  Params:  None
  Returns: The state of the control
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomComboBox.SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer);
begin
  TQtComboBox(ACustomComboBox.Handle).setCurrentIndex(NewIndex);
end;

class procedure TCDWSCustomComboBox.SetMaxLength(
  const ACustomComboBox: TCustomComboBox; NewLength: integer);
var
  Widget: TQtWidget;
  QtEdit: IQtEdit;
  MaxLength: Integer;
begin
  if not WSCheckHandleAllocated(ACustomComboBox, 'SetMaxLength') then
    Exit;

  Widget := TQtWidget(ACustomComboBox.Handle);
  if Supports(Widget, IQtEdit, QtEdit) then
  begin
    // qt doesn't accept -1
    MaxLength := QtEdit.getMaxLength;
    if (NewLength <= 0) or (NewLength > QtMaxEditLength) then
      NewLength := QtMaxEditLength;
    if NewLength <> MaxLength then
      QtEdit.setMaxLength(NewLength);
  end;
end;

class procedure TCDWSCustomComboBox.SetStyle(
  const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle);
begin
  TQtComboBox(ACustomComboBox.Handle).setEditable(NewStyle = csDropDown);
  TQtComboBox(ACustomComboBox.Handle).OwnerDrawn := NewStyle in
                                                   [csOwnerDrawFixed,
                                                    csOwnerDrawVariable];
  // TODO: implement styles: csSimple
  inherited SetStyle(ACustomComboBox, NewStyle);
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomComboBox.GetItems
  Params:  None
  Returns: ComboBox items
 ------------------------------------------------------------------------------}
class function TCDWSCustomComboBox.GetItems(const ACustomComboBox: TCustomComboBox): TStrings;
var
  ComboBox: TQtComboBox;
begin
  ComboBox := TQtComboBox(ACustomComboBox.Handle);
  if not Assigned(ComboBox.FList) then
  begin
    ComboBox.BeginUpdate;
    ComboBox.FList := TQtComboStrings.Create(ACustomComboBox, ComboBox);
    ComboBox.EndUpdate;
  end;
  Result := ComboBox.FList;
end;

class procedure TCDWSCustomComboBox.Sort(
  const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean);
begin
  TQtComboStrings(AList).Sorted := IsSorted;
end;


{ TCDWSToggleBox }

{------------------------------------------------------------------------------
  Method: TCDWSToggleBox.RetrieveState
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TCDWSToggleBox.RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
begin
  if not WSCheckHandleAllocated(ACustomCheckBox, 'RetrieveState') then
    Exit;
  if TQtToggleBox(ACustomCheckBox.Handle).isChecked then
    Result := cbChecked
  else
    Result := cbUnChecked;
end;

{------------------------------------------------------------------------------
  Method: TCDWSToggleBox.SetShortCut
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSToggleBox.SetShortCut(const ACustomCheckBox: TCustomCheckBox;
  const ShortCutK1, ShortCutK2: TShortCut);
begin
  if not WSCheckHandleAllocated(ACustomCheckBox, 'SetShortCut') then
    Exit;
  TQtToggleBox(ACustomCheckBox.Handle).setShortcut(ShortCutK1, ShortCutK2);
end;

{------------------------------------------------------------------------------
  Method: TCDWSToggleBox.SetState
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSToggleBox.SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
begin
  if not WSCheckHandleAllocated(ACustomCheckBox, 'SetState') then
    Exit;
  TQtToggleBox(ACustomCheckBox.Handle).BeginUpdate;
  TQtToggleBox(ACustomCheckBox.Handle).setChecked(NewState = cbChecked);
  TQtToggleBox(ACustomCheckBox.Handle).EndUpdate;
end;

{------------------------------------------------------------------------------
  Method: TCDWSToggleBox.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TCDWSToggleBox.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  QtToggleBox: TQtToggleBox;
begin
  QtToggleBox := TQtToggleBox.Create(AWinControl, AParams);
  QtToggleBox.setCheckable(True);
  QtToggleBox.AttachEvents;
  
  Result := TLCLIntfHandle(QtToggleBox);
end;*)

end.
