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
  Classes, StdCtrls, Controls, LCLType,
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
  public
  end;

  { TFpGuiWSGroupBox }

  TFpGuiWSGroupBox = class(TWSGroupBox)
  private
  protected
  public
  end;

  { TFpGuiWSCustomComboBox }

  TFpGuiWSCustomComboBox = class(TWSCustomComboBox)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  public
{    class function  GetSelStart(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetSelLength(const ACustomComboBox: TCustomComboBox): integer; override;}
    class function  GetItemIndex(const ACustomComboBox: TCustomComboBox): integer; override;
{    class function  GetMaxLength(const ACustomComboBox: TCustomComboBox): integer; override;

    class procedure SetArrowKeysTraverseList(const ACustomComboBox: TCustomComboBox;
      NewTraverseList: boolean); virtual;
    class procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); override;
    class procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;}
    class procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer); override;
{    class procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle); override;
    class procedure SetReadOnly(const ACustomComboBox: TCustomComboBox; NewReadOnly: boolean); override;}

    class function GetItems(const ACustomComboBox: TCustomComboBox): TStrings; override;
//    class procedure Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean); override;
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
  public
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
  public
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;
  public
{    class function  GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetSelLength(const ACustomEdit: TCustomEdit): integer; override;

    class procedure SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase); override;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); override;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;

    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer); override;}
//    class procedure SetColor(const AWinControl: TWinControl); override;
  end;

  { TFpGuiWSCustomMemo }

  TFpGuiWSCustomMemo = class(TWSCustomMemo)
  private
  protected
  public
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;
  public
{    class procedure AppendText(const ACustomMemo: TCustomMemo; const AText: string); override;
    class procedure SetAlignment(const ACustomMemo: TCustomMemo; const AAlignment: TAlignment); override;}
    class function GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;
{    class procedure SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle); override;
    class procedure SetWantReturns(const ACustomMemo: TCustomMemo; const NewWantReturns: boolean); override;
    class procedure SetWantTabs(const ACustomMemo: TCustomMemo; const NewWantTabs: boolean); override;
    class procedure SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean); override;}
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
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;

    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
  end;

  { TFpGuiWSCustomCheckBox }

  TFpGuiWSCustomCheckBox = class(TWSCustomCheckBox)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;

    class function  RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;
    class procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox;
      const OldShortCut, NewShortCut: TShortCut); override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;

    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;

    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
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
  public
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;

    class function  RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;
    class procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox;
      const OldShortCut, NewShortCut: TShortCut); override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;

    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
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
  gui_combobox,
  gui_edit,
  gui_checkbox,
  gui_radiobutton;

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

  Result := vComboBox.FocusItem-1;    // TfpgComboBox is 1-based
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomComboBox.SetItemIndex
  Params:  None
  Returns: The state of the control
 ------------------------------------------------------------------------------}
class procedure TFpGuiWSCustomComboBox.SetItemIndex(
  const ACustomComboBox: TCustomComboBox; NewIndex: integer);
var
  vComboBox: TfpgComboBox;
begin
  vComboBox := TFPGUIPrivateComboBox(ACustomComboBox.Handle).ComboBox;

  vComboBox.FocusItem := NewIndex+1;    // TfpgComboBox is 1-based
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomComboBox.GetItems
  Params:  None
  Returns: The state of the control
 ------------------------------------------------------------------------------}
class function TFpGuiWSCustomComboBox.GetItems(
  const ACustomComboBox: TCustomComboBox): TStrings;
var
  FComboBox: TfpgComboBox;
begin
  FComboBox := TFPGUIPrivateComboBox(ACustomComboBox.Handle).ComboBox;

  Result := FComboBox.Items;
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

class procedure TFpGuiWSCustomCheckBox.SetShortCut(
  const ACustomCheckBox: TCustomCheckBox; const OldShortCut,
  NewShortCut: TShortCut);
begin

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

class procedure TFpGuiWSRadioButton.SetShortCut(
  const ACustomCheckBox: TCustomCheckBox; const OldShortCut,
  NewShortCut: TShortCut);
begin

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
  Result:=inherited CreateHandle(AWinControl, AParams);
end;

class function TFpGuiWSCustomMemo.GetStrings(const ACustomMemo: TCustomMemo): TStrings;
begin
  Result:=inherited GetStrings(ACustomMemo);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TScrollBar, TFpGuiWSScrollBar);
//  RegisterWSComponent(TCustomGroupBox, TFpGuiWSCustomGroupBox);
//  RegisterWSComponent(TGroupBox, TFpGuiWSGroupBox);
  RegisterWSComponent(TCustomComboBox, TFpGuiWSCustomComboBox);
//  RegisterWSComponent(TComboBox, TFpGuiWSComboBox);
//  RegisterWSComponent(TCustomListBox, TFpGuiWSCustomListBox);
//  RegisterWSComponent(TListBox, TFpGuiWSListBox);
  RegisterWSComponent(TCustomEdit, TFpGuiWSCustomEdit);
//  RegisterWSComponent(TCustomMemo, TFpGuiWSCustomMemo);
//  RegisterWSComponent(TEdit, TFpGuiWSEdit);
//  RegisterWSComponent(TMemo, TFpGuiWSMemo);
//  RegisterWSComponent(TCustomLabel, TFpGuiWSCustomLabel);
//  RegisterWSComponent(TLabel, TFpGuiWSLabel);
//  RegisterWSComponent(TButtonControl, TFpGuiWSButtonControl);
  RegisterWSComponent(TCustomButton, TFpGuiWSButton);
  RegisterWSComponent(TCustomCheckBox, TFpGuiWSCustomCheckBox);
//  RegisterWSComponent(TCheckBox, TFpGuiWSCheckBox);
//  RegisterWSComponent(TToggleBox, TFpGuiWSToggleBox);
  RegisterWSComponent(TRadioButton, TFpGuiWSRadioButton);
//  RegisterWSComponent(TCustomStaticText, TFpGuiWSCustomStaticText);
//  RegisterWSComponent(TStaticText, TFpGuiWSStaticText);
////////////////////////////////////////////////////
end.
