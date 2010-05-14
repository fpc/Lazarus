{ $Id: FpGuiwsstdctrls.pp 5319 2004-03-17 20:11:29Z marc $}
{
 *****************************************************************************
 *                              FpGuiWSStdCtrls.pp                              * 
 *                              ---------------                              * 
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
unit FpGuiWSStdCtrls;

{$mode objfpc}{$H+}

interface

uses
  // Bindings
  fpguiwsprivate,
  // LCL
  Classes, StdCtrls, Controls, LCLType, sysutils,
  // Widgetset
  WSStdCtrls, WSLCLClasses;

type

  { TFpGuiWSScrollBar }

  TFpGuiWSScrollBar = class(TWSScrollBar)
  private
  protected
  public
  end;

  { TFpGuiWSCustomGroupBox }

  TFpGuiWSCustomGroupBox = class(TWSCustomGroupBox)
  private
  protected
  published
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  end;

  { TFpGuiWSGroupBox }

  TFpGuiWSGroupBox = class(TWSCustomGroupBox)
  private
  protected
  public
  end;

  { TFpGuiWSCustomComboBox }

  TFpGuiWSCustomComboBox = class(TWSCustomComboBox)
  private
  protected
  published
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;

{    class function  GetSelStart(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetSelLength(const ACustomComboBox: TCustomComboBox): integer; override;}
    class function  GetItemIndex(const ACustomComboBox: TCustomComboBox): integer; override;
{    class function  GetMaxLength(const ACustomComboBox: TCustomComboBox): integer; override;

    class procedure SetArrowKeysTraverseList(const ACustomComboBox: TCustomComboBox;
      NewTraverseList: boolean); virtual;
    class procedure SetDroppedDown(const ACustomComboBox: TCustomComboBox;
                                     ADroppedDown: Boolean); override;
    class procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); override;
    class procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;}
    class procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer); override;
{    class procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle); override;
    class procedure SetReadOnly(const ACustomComboBox: TCustomComboBox; NewReadOnly: boolean); override;}

    class function  GetItems(const ACustomComboBox: TCustomComboBox): TStrings; override;
    class procedure FreeItems(var AItems: TStrings); override;
  end;

  { TFpGuiWSComboBox }

  TFpGuiWSComboBox = class(TWSComboBox)
  private
  protected
  public
  end;

  { TFpGuiWSCustomListBox }

  TFpGuiWSCustomListBox = class(TWSCustomListBox)
  private
  protected
  published
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class function  GetStrings(const ACustomListBox: TCustomListBox
                            ): TStrings; override;
    class procedure FreeStrings(var AStrings: TStrings); override;
    class procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer); override;
  end;

  { TFpGuiWSListBox }

  TFpGuiWSListBox = class(TWSListBox)
  private
  protected
  public
  end;

  { TFpGuiWSCustomEdit }

  TFpGuiWSCustomEdit = class(TWSCustomEdit)
  private
  protected
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;
  public
  end;

  { TFpGuiWSCustomMemo }

  TFpGuiWSCustomMemo = class(TWSCustomMemo)
  private
  protected
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;

{    class procedure AppendText(const ACustomMemo: TCustomMemo; const AText: string); override;
    class procedure SetAlignment(const ACustomMemo: TCustomMemo; const AAlignment: TAlignment); override;}
    class function GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;
    class procedure FreeStrings(var AStrings: TStrings); override;
  end;

  { TFpGuiWSEdit }

  TFpGuiWSEdit = class(TWSEdit)
  private
  protected
  public
  end;

  { TFpGuiWSMemo }

  TFpGuiWSMemo = class(TWSMemo)
  private
  protected
  public
  end;

  { TFpGuiWSButtonControl }

  TFpGuiWSButtonControl = class(TWSButtonControl)
  private
  protected
  public
  end;

  { TFpGuiWSButton }

  TFpGuiWSButton = class(TWSButton)
  private
  protected
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure Invalidate(const AWinControl: TWinControl); override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
  end;

  { TFpGuiWSCustomCheckBox }

  TFpGuiWSCustomCheckBox = class(TWSCustomCheckBox)
  private
  protected
  published
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;

    class function  RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                             var PreferredWidth, PreferredHeight: integer;
                             WithThemeSpace: Boolean); override;
  end;

  { TFpGuiWSCheckBox }

  TFpGuiWSCheckBox = class(TWSCheckBox)
  private
  protected
  public
  end;

  { TFpGuiWSToggleBox }

  TFpGuiWSToggleBox = class(TWSToggleBox)
  private
  protected
  public
  end;

  { TFpGuiWSRadioButton }

  TFpGuiWSRadioButton = class(TWSRadioButton)
  private
  protected
  published
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;

    class function  RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;

    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                             var PreferredWidth, PreferredHeight: integer;
                             WithThemeSpace: Boolean); override;
  end;

  { TFpGuiWSCustomStaticText }

  TFpGuiWSCustomStaticText = class(TWSCustomStaticText)
  private
  protected
  public
  end;

  { TFpGuiWSStaticText }

  TFpGuiWSStaticText = class(TWSStaticText)
  private
  protected
  public
  end;


implementation

uses
  fpg_combobox,
  fpg_edit,
  fpg_checkbox,
  fpg_radiobutton,
  fpg_listbox,
  fpg_panel;

{ TFpGuiWSCustomComboBox }

{------------------------------------------------------------------------------
  Method: TQtWSCustomComboBox.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TFpGuiWSCustomComboBox.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TFPGUIPrivateComboBox.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomComboBox.DestroyHandle
  Params:  None
  Returns: Nothing

  Releases allocated memory and resources
 ------------------------------------------------------------------------------}
class procedure TFpGuiWSCustomComboBox.DestroyHandle(const AWinControl: TWinControl);
begin
  TFPGUIPrivateComboBox(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomComboBox.GetItemIndex
  Params:  None
  Returns: The state of the control
 ------------------------------------------------------------------------------}
class function TFpGuiWSCustomComboBox.GetItemIndex(
  const ACustomComboBox: TCustomComboBox): integer;
var
  vComboBox: TfpgComboBox;
begin
  vComboBox := TFPGUIPrivateComboBox(ACustomComboBox.Handle).ComboBox;

  Result := vComboBox.FocusItem;
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWSCustomComboBox.SetItemIndex
  Params:  Item index in combo
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TFpGuiWSCustomComboBox.SetItemIndex(
  const ACustomComboBox: TCustomComboBox; NewIndex: integer);
var
  vComboBox: TfpgComboBox;
begin
  vComboBox := TFPGUIPrivateComboBox(ACustomComboBox.Handle).ComboBox;

  vComboBox.FocusItem := NewIndex;
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWSCustomComboBox.GetItems
  Params:  None
  Returns: Returns a TStrings controlling the combo items
 ------------------------------------------------------------------------------}
class function TFpGuiWSCustomComboBox.GetItems(
  const ACustomComboBox: TCustomComboBox): TStrings;
var
  FComboBox: TfpgComboBox;
begin
  FComboBox := TFPGUIPrivateComboBox(ACustomComboBox.Handle).ComboBox;

  Result := FComboBox.Items;
end;

class procedure TFpGuiWSCustomComboBox.FreeItems(var AItems: TStrings);
begin
  //Widgetset atomatically frees the items, so override
  //and do not call inherited.
end;

{ TFpGuiWSCustomEdit }

{------------------------------------------------------------------------------
  Method: TFpGuiWSCustomEdit.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TFpGuiWSCustomEdit.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TFPGUIPrivateEdit.Create(AWinControl, AParams));
end;

{ TFpGuiWSButton }

{------------------------------------------------------------------------------
  Method: TFpGuiWSButton.GetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TFpGuiWSButton.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
begin
  Result := True;
  AText := TFPGUIPrivateButton(AWinControl.Handle).GetText;
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWSButton.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TFpGuiWSButton.SetText(const AWinControl: TWinControl;
  const AText: String);
begin
  TFPGUIPrivateButton(AWinControl.Handle).SetText(AText);
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWSButton.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TFpGuiWSButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TFPGUIPrivateButton.Create(AWinControl, AParams));
end;

class procedure TFpGuiWSButton.DestroyHandle(const AWinControl: TWinControl);
begin
  TFPGUIPrivateButton(AWinControl.Handle).Free;
  AWinControl.Handle := 0;
end;

class procedure TFpGuiWSButton.Invalidate(const AWinControl: TWinControl);
begin
  inherited Invalidate(AWinControl);
  TFPGUIPrivateButton(AWinControl.Handle).Widget.Invalidate;
end;

{ TFpGuiWSCustomCheckBox }

class function TFpGuiWSCustomCheckBox.RetrieveState(
  const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
var
  vCheckBox: TfpgCheckBox;
begin
  vCheckBox := TFPGUIPrivateCheckBox(ACustomCheckBox.Handle).CheckBox;

  if vCheckBox.Checked then
    Result := cbChecked
  else
    Result := cbUnchecked;
end;

class procedure TFpGuiWSCustomCheckBox.SetState(
  const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
var
  vCheckBox: TfpgCheckBox;
begin
  vCheckBox := TFPGUIPrivateCheckBox(ACustomCheckBox.Handle).CheckBox;

  if NewState = cbChecked then
    vCheckBox.Checked := True
  else
    vCheckBox.Checked := False;
end;

class function TFpGuiWSCustomCheckBox.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
var
  vCheckBox: TfpgCheckBox;
begin
  Result := False;
  vCheckBox := TFPGUIPrivateCheckBox(AWinControl.Handle).CheckBox;
  AText := vCheckBox.Text;
  Result := True;
end;

class procedure TFpGuiWSCustomCheckBox.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  vCheckBox: TfpgCheckBox;
begin
  vCheckBox := TFPGUIPrivateCheckBox(AWinControl.Handle).CheckBox;
  vCheckBox.Text := AText;
end;

class procedure TFpGuiWSCustomCheckBox.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  TFPGUIPrivateCheckBox(AWinControl.Handle).GetPreferredSize(PreferredWidth,PreferredHeight,WithThemeSpace);
end;

class function TFpGuiWSCustomCheckBox.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TFPGUIPrivateCheckBox.Create(AWinControl, AParams));
end;

class procedure TFpGuiWSCustomCheckBox.DestroyHandle(
  const AWinControl: TWinControl);
begin
  TFPGUIPrivateCheckBox(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
end;

{ TFpGuiWSRadioButton }

class function TFpGuiWSRadioButton.RetrieveState(
  const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
var
  vRadioButton: TfpgRadioButton;
begin
  vRadioButton := TFPGUIPrivateRadioButton(ACustomCheckBox.Handle).RadioButton;

  if vRadioButton.Checked then
    Result := cbChecked
  else
    Result := cbUnchecked;
end;

class procedure TFpGuiWSRadioButton.SetState(
  const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
var
  vRadioButton: TfpgRadioButton;
begin
  vRadioButton := TFPGUIPrivateRadioButton(ACustomCheckBox.Handle).RadioButton;

  if NewState = cbChecked then
    vRadioButton.Checked := True
  else
    vRadioButton.Checked := False;
end;

class function TFpGuiWSRadioButton.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
var
  vRadioButton: TfpgRadioButton;
begin
  Result := False;
  vRadioButton := TFPGUIPrivateRadioButton(AWinControl.Handle).RadioButton;
  AText := vRadioButton.Text;
  Result := True;
end;

class procedure TFpGuiWSRadioButton.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  vRadioButton: TfpgRadioButton;
begin
  vRadioButton := TFPGUIPrivateRadioButton(AWinControl.Handle).RadioButton;
  vRadioButton.Text := AText;
end;

class procedure TFpGuiWSRadioButton.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  TFPGUIPrivateRadioButton(AWinControl.Handle).GetPreferredSize(PreferredWidth,PreferredHeight,WithThemeSpace);
end;

class function TFpGuiWSRadioButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TFPGUIPrivateRadioButton.Create(AWinControl, AParams));
end;

class procedure TFpGuiWSRadioButton.DestroyHandle(const AWinControl: TWinControl);
begin
  TFPGUIPrivateRadioButton(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
end;

{ TFpGuiWSCustomMemo }

class function TFpGuiWSCustomMemo.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TFPGUIPrivateMemo.Create(AWinControl, AParams));
end;

class function TFpGuiWSCustomMemo.GetStrings(const ACustomMemo: TCustomMemo): TStrings;
var
  PrivateMemo: TFPGUIPrivateMemo;
begin
  PrivateMemo := TFPGUIPrivateMemo(ACustomMemo.Handle);
  Result:=PrivateMemo.GetStrings;
end;

class procedure TFpGuiWSCustomMemo.FreeStrings(var AStrings: TStrings);
begin
  //Do nothing, autofree by fpguimemo
end;

{ TFpGuiWSListBox }

class function TFpGuiWSCustomListBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TFPGUIPrivateListBox.Create(AWinControl, AParams));
end;

class function TFpGuiWSCustomListBox.GetStrings(
  const ACustomListBox: TCustomListBox): TStrings;
var
  FListBox: TfpgListBox;
begin
  FListBox := TFPGUIPrivateListBox(ACustomListBox.Handle).ListBox;
  Result := FListBox.Items;
end;

class procedure TFpGuiWSCustomListBox.FreeStrings(var AStrings: TStrings);
begin
  //Do nothing, autofree by fpguilistbox
end;

class procedure TFpGuiWSCustomListBox.SetItemIndex(
  const ACustomListBox: TCustomListBox; const AIndex: integer);
var
  PrivateListBox: TFPGUIPrivateListBox;
begin
  PrivateListBox:=TFPGUIPrivateListBox(ACustomListBox.Handle);
  PrivateListBox.ItemIndex:=AIndex;
end;

{ TFpGuiWSCustomGroupBox }

class function TFpGuiWSCustomGroupBox.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TFPGUIPrivateGroupBox.Create(AWinControl, AParams));
end;

class procedure TFpGuiWSCustomGroupBox.DestroyHandle(
  const AWinControl: TWinControl);
begin
  TFPGUIPrivateGroupBox(AWinControl.Handle).Free;
  AWinControl.Handle := 0;
end;

end.
