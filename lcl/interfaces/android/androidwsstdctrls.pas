{
 *****************************************************************************
 *                            AndroidWSStdCtrls.pas                          *
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
unit androidwsstdctrls;

{$mode objfpc}{$H+}

interface

//{$I androiddefines.inc}

uses
  // Bindings
  // RTL, FCL
  Classes, Types, StdCtrls,
  // LCL
  Controls, Forms, SysUtils, InterfaceBase, LCLType,
  // Widgetset
  WSProc, WSStdCtrls, WSLCLClasses,
  androidprivate, androidstringlists;

type

  { TAndroidWSScrollBar }

  TAndroidWSScrollBar = class(TWSScrollBar)
  published
{    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetKind(const AScrollBar: TCustomScrollBar; const AIsHorizontal: Boolean); override;
    class procedure SetParams(const AScrollBar: TCustomScrollBar); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;}
  end;

  { TAndroidWSCustomGroupBox }

  TAndroidWSCustomGroupBox = class(TWSCustomGroupBox)
  published
{    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class function GetDefaultClientRect(const AWinControl: TWinControl;
             const aLeft, aTop, aWidth, aHeight: integer; var aClientRect: TRect
             ): boolean; override;}
  end;

  { TAndroidWSGroupBox }

  TAndroidWSGroupBox = class(TWSGroupBox)
  published
  end;

  { TAndroidWSCustomComboBox }

  TAndroidWSCustomComboBox = class(TWSCustomComboBox)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
{    class function GetDroppedDown(const ACustomComboBox: TCustomComboBox
       ): Boolean; override;}
    class function GetItemIndex(const ACustomComboBox: TCustomComboBox): integer; override;
    class function GetItems(const ACustomComboBox: TCustomComboBox): TStrings; override;
{    class function GetMaxLength(const ACustomComboBox: TCustomComboBox): integer; override;
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
       ADroppedDown: Boolean); override;}
    class procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer); override;
{    class procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle); override;

    class procedure Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean); override;}
  end;

  { TAndroidWSComboBox }

  TAndroidWSComboBox = class(TWSComboBox)
  published
  end;

  { TAndroidWSCustomListBox }

  TAndroidWSCustomListBox = class(TWSCustomListBox)
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

  { TAndroidWSListBox }

  TAndroidWSListBox = class(TWSListBox)
  published
  end;

  { TAndroidWSCustomEdit }

  TAndroidWSCustomEdit = class(TWSCustomEdit)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
{    class procedure SetAlignment(const ACustomEdit: TCustomEdit; const AAlignment: TAlignment); override;
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

    // From TWSWinControl
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
  end;

  { TAndroidWSCustomMemo }

  TAndroidWSCustomMemo = class(TWSCustomMemo)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
{    class procedure AppendText(const ACustomMemo: TCustomMemo; const AText: string); override;
    class function GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;
    class procedure SetAlignment(const ACustomEdit: TCustomEdit; const AAlignment: TAlignment); override;
    class procedure SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle); override;
    class procedure SetWantReturns(const ACustomMemo: TCustomMemo; const NewWantReturns: boolean); override;
    class procedure SetWantTabs(const ACustomMemo: TCustomMemo; const NewWantTabs: boolean); override;
    class procedure SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean); override;}

    // From TWSWinControl
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
  end;

  { TAndroidWSEdit }

  TAndroidWSEdit = class(TWSEdit)
  published
  end;

  { TAndroidWSMemo }

  TAndroidWSMemo = class(TWSMemo)
  published
  end;

  { TAndroidWSButtonControl }

  TAndroidWSButtonControl = class(TWSButtonControl)
  published
  end;

  { TAndroidWSButton }

  TAndroidWSButton = class(TWSButton)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
{    class procedure SetDefault(const AButton: TCustomButton; ADefault: Boolean); override;
    class procedure SetShortcut(const AButton: TCustomButton; const ShortCutK1, ShortCutK2: TShortcut); override;}
    // From TWSWinControl
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
  end;

  { TAndroidWSCustomCheckBox }

  TAndroidWSCustomCheckBox = class(TWSCustomCheckBox)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;

//    class procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox; const ShortCutK1, ShortCutK2: TShortCut); override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;

    class function RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;
  end;

  { TAndroidWSCheckBox }

  TAndroidWSCheckBox = class(TWSCheckBox)
  published
  end;

  { TAndroidWSToggleBox }

  TAndroidWSToggleBox = class(TWSToggleBox)
  published
{    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;

    class procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox; const ShortCutK1, ShortCutK2: TShortCut); override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;

    class function  RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;}
  end;

  { TAndroidWSRadioButton }

  TAndroidWSRadioButton = class(TWSRadioButton)
  published
{    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;

    class procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox; const ShortCutK1, ShortCutK2: TShortCut); override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;

    class function RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;}
  end;

  { TAndroidWSCustomStaticText }

  TAndroidWSCustomStaticText = class(TWSCustomStaticText)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;

{    class procedure SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment); override;
    class procedure SetStaticBorderStyle(const ACustomStaticText: TCustomStaticText; const NewBorderStyle: TStaticBorderStyle); override;}

    // From TWSWinControl
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
  end;

  { TAndroidWSStaticText }

  TAndroidWSStaticText = class(TWSStaticText)
  published
  end;

implementation

{ TAndroidWSCustomComboBox }

class function TAndroidWSCustomComboBox.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLIntfHandle;
var
  lControl: TCustomComboBox absolute AWinControl;
begin
  Result := TLCLIntfHandle(TAndroidComboBox.Create(lControl, AParams));
end;

class function TAndroidWSCustomComboBox.GetItemIndex(
  const ACustomComboBox: TCustomComboBox): integer;
var
  lHandle: TAndroidComboBox;
begin
  lHandle := TAndroidComboBox(ACustomComboBox.Handle);
  Result := lHandle.GetItemIndex();
end;

class function TAndroidWSCustomComboBox.GetItems(
  const ACustomComboBox: TCustomComboBox): TStrings;
var
  lHandle: TAndroidComboBox;
begin
  lHandle := TAndroidComboBox(ACustomComboBox.Handle);
  if not Assigned(lHandle.FList) then
  begin
    //lHandle.BeginUpdate;
    lHandle.FList := TAndroidComboBoxStrings.Create(ACustomComboBox, lHandle);
    //lHandle.EndUpdate;
  end;
  Result := lHandle.FList;
end;

class procedure TAndroidWSCustomComboBox.SetItemIndex(
  const ACustomComboBox: TCustomComboBox; NewIndex: integer);
var
  lHandle: TAndroidComboBox;
begin
  lHandle := TAndroidComboBox(ACustomComboBox.Handle);
  lHandle.SetItemIndex(NewIndex);
end;

{ TAndroidWSCustomEdit }

class function TAndroidWSCustomEdit.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  lControl: TCustomEdit absolute AWinControl;
begin
  Result := TLCLIntfHandle(TAndroidEdit.Create(lControl, AParams));
end;

class function TAndroidWSCustomEdit.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
var
  lControl: TCustomEdit absolute AWinControl;
  lHandle: TAndroidEdit;
begin
  if lControl = nil then Exit(False);
  lHandle := TAndroidEdit(lControl.Handle);
  if lHandle = nil then Exit(False);

  Result := True;
  AText := lHandle.GetText();
end;

class procedure TAndroidWSCustomEdit.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  lControl: TCustomEdit absolute AWinControl;
  lHandle: TAndroidEdit;
begin
  if lControl = nil then Exit();
  lHandle := TAndroidEdit(lControl.Handle);
  if lHandle = nil then Exit();

  lHandle.SetText(AText);
end;

{ TAndroidWSCustomMemo }

class function TAndroidWSCustomMemo.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  lControl: TCustomMemo absolute AWinControl;
begin
  Result := TLCLIntfHandle(TAndroidEdit.Create(lControl, AParams));
end;

class function TAndroidWSCustomMemo.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
var
  lControl: TCustomEdit absolute AWinControl;
  lHandle: TAndroidEdit;
begin
  if lControl = nil then Exit(False);
  lHandle := TAndroidEdit(lControl.Handle);
  if lHandle = nil then Exit(False);

  Result := True;
  AText := lHandle.GetText();
end;

class procedure TAndroidWSCustomMemo.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  lControl: TCustomEdit absolute AWinControl;
  lHandle: TAndroidEdit;
begin
  if lControl = nil then Exit();
  lHandle := TAndroidEdit(lControl.Handle);
  if lHandle = nil then Exit();

  lHandle.SetText(AText);
end;

{ TAndroidWSButton }

class function TAndroidWSButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  lControl: TCustomButton absolute AWinControl;
begin
  Result := TLCLIntfHandle(TAndroidButton.Create(lControl, AParams));
end;

class function TAndroidWSButton.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
var
  lControl: TCustomEdit absolute AWinControl;
  lHandle: TAndroidStaticText;
begin
  if lControl = nil then Exit(False);
  lHandle := TAndroidStaticText(lControl.Handle);
  if lHandle = nil then Exit(False);

  Result := True;
  AText := lHandle.GetText();
end;

class procedure TAndroidWSButton.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  lControl: TCustomEdit absolute AWinControl;
  lHandle: TAndroidStaticText;
begin
  if lControl = nil then Exit;
  lHandle := TAndroidStaticText(lControl.Handle);
  if lHandle = nil then Exit;

  lHandle.SetText(AText);
end;

{ TAndroidWSCustomCheckBox }

class function TAndroidWSCustomCheckBox.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLIntfHandle;
var
  lControl: TCustomCheckBox absolute AWinControl;
begin
  Result := TLCLIntfHandle(TAndroidCheckBox.Create(lControl, AParams));
end;

class procedure TAndroidWSCustomCheckBox.SetState(
  const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
var
  lHandle: TAndroidCheckBox;
begin
  lHandle := TAndroidCheckBox(ACustomCheckBox.Handle);
  lHandle.SetState(NewState);
end;

class function TAndroidWSCustomCheckBox.RetrieveState(
  const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
var
  lHandle: TAndroidCheckBox;
begin
  lHandle := TAndroidCheckBox(ACustomCheckBox.Handle);
  Result := lHandle.GetState();
end;

{ TAndroidWSCustomStaticText }

class function TAndroidWSCustomStaticText.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLIntfHandle;
var
  lControl: TCustomStaticText absolute AWinControl;
begin
  Result := TLCLIntfHandle(TAndroidStaticText.Create(lControl, AParams));
end;

class function TAndroidWSCustomStaticText.GetText(
  const AWinControl: TWinControl; var AText: String): Boolean;
var
  lControl: TCustomEdit absolute AWinControl;
  lHandle: TAndroidStaticText;
begin
  if lControl = nil then Exit(False);
  lHandle := TAndroidStaticText(lControl.Handle);
  if lHandle = nil then Exit(False);

  Result := True;
  AText := lHandle.GetText();
end;

class procedure TAndroidWSCustomStaticText.SetText(
  const AWinControl: TWinControl; const AText: String);
var
  lControl: TCustomEdit absolute AWinControl;
  lHandle: TAndroidStaticText;
begin
  if lControl = nil then Exit;
  lHandle := TAndroidStaticText(lControl.Handle);
  if lHandle = nil then Exit;

  lHandle.SetText(AText);
end;

end.
