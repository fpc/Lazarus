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
  Classes, Controls, StdCtrls, LCLType, LCLProc,
////////////////////////////////////////////////////
  WSStdCtrls, WSLCLClasses, WSControls, WSProc,
  CarbonWSControls, CarbonPrivate;
type

  { TCarbonWSScrollBar }

  TCarbonWSScrollBar = class(TWSScrollBar)
  private
  protected
  public
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
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;

    class procedure SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase); override;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); override;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
  end;

  { TCarbonWSCustomMemo }

  TCarbonWSCustomMemo = class(TWSCustomMemo)
  private
  protected
  public
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
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class procedure SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment); override;
  end;

  { TCarbonWSStaticText }

  TCarbonWSStaticText = class(TWSStaticText)
  private
  protected
  public
  end;


implementation

{ TCarbonWSCustomGroupBox }

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomGroupBox.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new custom group box in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
function TCarbonWSCustomGroupBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Control: ControlRef;
  CFString: CFStringRef;
  Info: PWidgetInfo;
begin
  Result := 0;

  CreateCarbonString(AParams.Caption, CFString);
  try
    if CreateGroupBoxControl(GetTopParentWindow(AWinControl),
      ParamsToCarbonRect(AParams), CFString,
      not (AWinControl.Parent is TCustomGroupBox), Control) = noErr
    then
      Result := TLCLIntfHandle(Control);
  finally
    FreeCarbonString(CFString);
  end;
  if Result = 0 then Exit;

  Info := CreateCtrlWidgetInfo(Control, AWinControl);
  TCarbonPrivateHandleClass(WSPrivate).RegisterEvents(Info);
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
var
  Edit: TCustomEdit;
  Control: ControlRef;
  CFString: CFStringRef;
  Info: PWidgetInfo;
  IsPassword: PBoolean;
  SingleLine: Boolean = True;
begin
  Result := 0;
  Edit := AWinControl as TCustomEdit;

  CreateCarbonString(AParams.Caption, CFString);
  try
    if CreateEditUniCodeTextControl(GetTopParentWindow(AWinControl),
      ParamsToCarbonRect(AParams), CFString, (Edit.PasswordChar <> #0), nil,
      Control) = noErr
    then
      Result := TLCLIntfHandle(Control);
  finally
    FreeCarbonString(CFString);
  end;
  if Result = 0 then Exit;

  New(IsPassword);
  IsPassword^ := Edit.PasswordChar <> #0;
  
  SetControlData(Control, kControlEntireControl, kControlEditTextSingleLineTag,
                 SizeOf(Boolean), @SingleLine);


  Info := CreateCtrlWidgetInfo(Control, AWinControl);
  Info^.UserData := IsPassword;
  Info^.DataOwner := True;
  TCarbonPrivateHandleClass(WSPrivate).RegisterEvents(Info);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomEdit.GetSelStart
  Params:  ACustomEdit - LCL custom edit
  Returns: Position of selection start

  Retrieves the position of selection start of edit in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomEdit.GetSelStart(const ACustomEdit: TCustomEdit): integer;
var
  Control: ControlRef;
  SelData: ControlEditTextSelectionRec;
  RecSize: FPCMacOSAll.Size;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'GetSelStart') then Exit;

  Result := 0;
  Control := ControlRef(ACustomEdit.Handle);
  if GetControlData(Control, kControlEntireControl, kControlEditTextSelectionTag,
      SizeOf(ControlEditTextSelectionRec), @SelData, @RecSize) <> noErr
  then Exit;

  Result := SelData.SelStart;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomEdit.GetSelLength
  Params:  ACustomEdit - LCL custom edit
  Returns: Length of selection

  Retrieves the length of selection of edit in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
var
  Control: ControlRef;
  SelData: ControlEditTextSelectionRec;
  RecSize: FPCMacOSAll.Size;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'GetSelLength') then Exit;

  Result := 0;
  Control := ControlRef(ACustomEdit.Handle);

  if GetControlData(Control, kControlEntireControl, kControlEditTextSelectionTag,
    SizeOf(ControlEditTextSelectionRec), @SelData, @RecSize) <> noErr then exit;

  Result := SelData.SelEnd - SelData.SelStart;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomEdit.GetText
  Params:  AWinControl - LCL control
           AText       - Text
  Returns: If the function succeeds

  Retrieves the text of edit in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomEdit.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
var
  Control: ControlRef;
  TextType: ResType;
  CFString: CFStringRef;
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetText') then Exit;

  Result := False;
  Control := ControlRef(AWinControl.Handle);
  if PBoolean(GetCtrlWidgetInfo(Pointer(AWincontrol.Handle))^.UserData)^ = True then
    TextType := kControlEditTextPasswordCFStringTag // IsPassword
  else
    TextType := kControlEditTextCFStringTag;
    
  if GetControlData(Control, kControlEntireControl, TextType,
    SizeOf(CFStringRef), @CFString, nil) <> noErr then Exit;
  try
    AText := CarbonStringToString(CFString);
    Result := True;
  finally
    FreeCarbonString(CFString);
  end;
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
 //TODO
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
 //AFAICS this will have to be checked in a callback
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
var
  NeedsPassword: Boolean;
  IsPassword: Boolean;
  Info: PWidgetInfo;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetPasswordChar') then Exit;

  Info := GetCtrlWidgetInfo(Pointer(ACustomEdit.Handle));
  IsPassword := PBoolean(Info^.UserData)^;
  NeedsPassword := (NewChar <> #0);

  if IsPassword = NeedsPassword then Exit;
  PBoolean(Info^.UserData)^ := NeedsPassword;
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
var
  Control: ControlRef;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetReadOnly') then Exit;

  Control := ControlRef(ACustomEdit.Handle);

  SetControlData(Control, kControlEntireControl, kControlEditTextLockedTag,
    SizeOf(Boolean), @NewReadOnly);
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
var
  Control: ControlRef;
  SelData: ControlEditTextSelectionRec;
  RecSize: FPCMacOSAll.Size;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'GetSelStart') then Exit;

  Control := ControlRef(ACustomEdit.Handle);

  if GetControlData(Control, kControlEntireControl, kControlEditTextSelectionTag,
    SizeOf(ControlEditTextSelectionRec), @SelData, @RecSize) <> noErr then Exit;

  SelData.SelStart := NewStart;
  SetControlData(Control, kControlEntireControl, kControlEditTextSelectionTag,
    SizeOf(ControlEditTextSelectionRec), @SelData);
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
var
  Control: ControlRef;
  SelData: ControlEditTextSelectionRec;
  RecSize: FPCMacOSAll.Size;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetSelLength') then Exit;

  Control := ControlRef(ACustomEdit.Handle);
  if GetControlData(Control, kControlEntireControl, kControlEditTextSelectionTag,
    SizeOf(ControlEditTextSelectionRec), @SelData, @RecSize) <> noErr then Exit;

  SelData.SelEnd := SelData.SelStart + NewLength;
  SetControlData(Control, kControlEntireControl, kControlEditTextSelectionTag,
    SizeOf(ControlEditTextSelectionRec), @SelData);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomEdit.SetText
  Params:  AWinControl - LCL control
           AText       - Text
  Returns: Nothing

  Sets the text of edit in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomEdit.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  Control: ControlRef;
  TextType: ResType;
  CFString: CFStringRef;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetText') then Exit;

  Control := ControlRef(AWinControl.Handle);
  if PBoolean(GetCtrlWidgetInfo(Pointer(AWincontrol.Handle))^.UserData)^ = True then
    TextType := kControlEditTextPasswordCFStringTag // IsPassword
  else
    TextType := kControlEditTextCFStringTag;

  CreateCarbonString(AText, CFString);
  try
    SetControlData(Control, kControlEntireControl, TextType, SizeOf(CFStringRef), @CFString);
  finally
    FreeCarbonString(CFString);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomEdit.GetPreferredSize
  Params:  AWinControl     - LCL control
           PreferredWidth  - Preferred width, valid if > 0
           PreferredHeight - Preferred height, valid if > 0
           WithThemeSpace  - Whether to include space for theme
  Returns: Nothing

  Retrieves the preferred size of edit in Carbon interface to support
  autosizing of controls
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomEdit.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  //TODO
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
var
  Control: ControlRef;
  CFString: CFStringRef;
  Info: PWidgetInfo;
begin
  Result := 0;

  CreateCarbonString(AParams.Caption, CFString);
  try
    if CreateCheckBoxControl(GetTopParentWindow(AWinControl),
      ParamsToCarbonRect(AParams), CFString,
      Ord((AWinControl as TCheckBox).Checked), True, Control) = noErr
    then
      Result := TLCLIntfHandle(Control);
  finally
    FreeCarbonString(CFString);
  end;
  if Result = 0 then Exit;

  Info := CreateCtrlWidgetInfo(Control, AWinControl);
  TCarbonPrivateHandleClass(WSPrivate).RegisterEvents(Info);
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
  if not WSCheckHandleAllocated(ACustomCheckBox, 'RetrieveState') then Exit;

  case GetControl32BitValue(ControlRef(ACustomCheckBox.Handle)) of
    kControlCheckBoxCheckedValue   : Result := cbChecked;
    kControlCheckBoxUncheckedValue : Result := cbUnchecked;
    kControlCheckBoxMixedValue     : ; // what the heck does this do?
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomCheckBox.SetState
  Params:  ACustomCheckBox - LCL custom check box
          NewState         - New state of check box
  Returns: Nothing

  Sets the new state of check box in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomCheckBox.SetState(
  const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
var
  Value: UInt32;
begin
  if not WSCheckHandleAllocated(ACustomCheckBox, 'SetState') then Exit;

  Value := kControlCheckBoxMixedValue; // give it a default value
  case NewState of
    cbChecked  :  Value := kControlCheckBoxCheckedValue;
    cbUnChecked: Value := kControlCheckBoxUncheckedValue;
    //cbGrayed   : kControlCheckBoxMixedValue; // what the heck does this do?
  end;
  SetControl32BitValue(ControlRef(ACustomCheckBox.Handle), Value);
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
var
  Control: ControlRef;
  CFString: CFStringRef;
  Info: PWidgetInfo;
begin
  Result := 0;

  CreateCarbonString(AParams.Caption, CFString);
  try
    if CreateRadioButtonControl(GetTopParentWindow(AWinControl),
      ParamsToCarbonRect(AParams), CFString,
      Ord((AWinControl as TRadioButton).Checked), True, Control) = noErr
    then
      Result := TLCLIntfHandle(Control);
  finally
    FreeCarbonString(CFString);
  end;
  if Result = 0 then Exit;

  Info := CreateCtrlWidgetInfo(Control, AWinControl);
  TCarbonPrivateHandleClass(WSPrivate).RegisterEvents(Info);
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
var
  Control: ControlRef;
  CFString: CFStringRef;
  Info: PWidgetInfo;
  MultiLine: Boolean = True;
  FontStyle: ControlFontStyleRec;
begin
  Result := 0;

  FontStyle.flags := kControlUseJustMask;
  case (AWinControl as TCustomStaticText).Alignment of
  taLeftJustify: FontStyle.just := teFlushLeft;
  taRightJustify: FontStyle.just := teFlushRight;
  taCenter: FontStyle.just := teCenter;
  end;

  CreateCarbonString(AParams.Caption, CFString);
  try
    if CreateStaticTextControl(GetTopParentWindow(AWinControl),
      ParamsToCarbonRect(AParams), CFString, @FontStyle, Control) = noErr
    then
      Result := TLCLIntfHandle(Control);
  finally
    FreeCarbonString(CFString);
  end;
  if Result = 0 then Exit;

  // switch on multi-line attribute
  SetControlData(Control, kControlEntireControl, kControlStaticTextIsMultilineTag,
    SizeOf(Boolean), @MultiLine);
    
  Info := CreateCtrlWidgetInfo(Control, AWinControl);
  TCarbonPrivateHandleClass(WSPrivate).RegisterEvents(Info);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomStaticText.GetText
  Params:  AWinControl - LCL control
           AText       - Text
  Returns: If the function succeeds

  Retrieves the text (caption) of static text in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomStaticText.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
var
  CFString: CFStringRef;
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetText') then Exit;

  Result := GetControlData(ControlRef(AWinControl.Handle), kControlEntireControl,
    kControlStaticTextCFStringTag, SizeOf(CFStringRef), @CFString, nil) = NoErr;
  try
    if Result = False then Exit;

    AText := CarbonStringToString(CFString);
  finally
    FreeCarbonString(CFString);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomStaticText.SetText
  Params:  AWinControl - LCL control
           AText       - Text
  Returns: Nothing

  Sets the text (caption) of static text in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomStaticText.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  CFString: CFStringRef;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetText') then Exit;

  CreateCarbonString(AText, CFString);
  try
    SetControlData(ControlRef(AWinControl.Handle), kControlEntireControl,
      kControlStaticTextCFStringTag, SizeOf(CFStringRef), @CFString);
  finally
    FreeCarbonString(CFString);
  end;
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
  if not WSCheckHandleAllocated(ACustomStaticText, 'SetAlignment') then Exit;

  // get static text font style and change only justification
  GetControlData(ControlRef(ACustomStaticText.Handle), kControlEntireControl,
    kControlStaticTextStyleTag, SizeOf(FontStyle), @FontStyle, nil);

  FontStyle.flags := FontStyle.flags or kControlUseJustMask;
  case NewAlignment of
  taLeftJustify : FontStyle.just := teFlushLeft;
  taRightJustify: FontStyle.just := teFlushRight;
  taCenter      : FontStyle.just := teCenter;
  end;

  SetControlData(ControlRef(ACustomStaticText.Handle), kControlEntireControl,
    kControlStaticTextStyleTag, SizeOf(FontStyle), @FontStyle);
  // invalidate static text
  InvalidateCarbonControl(ACustomStaticText.Handle);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TScrollBar, TCarbonWSScrollBar);
  RegisterWSComponent(TCustomGroupBox, TCarbonWSCustomGroupBox);
//  RegisterWSComponent(TGroupBox, TCarbonWSGroupBox);
//  RegisterWSComponent(TCustomComboBox, TCarbonWSCustomComboBox);
//  RegisterWSComponent(TComboBox, TCarbonWSComboBox);
//  RegisterWSComponent(TCustomListBox, TCarbonWSCustomListBox);
//  RegisterWSComponent(TListBox, TCarbonWSListBox);
  RegisterWSComponent(TCustomEdit, TCarbonWSCustomEdit);
//  RegisterWSComponent(TCustomMemo, TCarbonWSCustomMemo);
//  RegisterWSComponent(TEdit, TCarbonWSEdit);
//  RegisterWSComponent(TMemo, TCarbonWSMemo);
//  RegisterWSComponent(TCustomLabel, TCarbonWSCustomLabel);
//  RegisterWSComponent(TLabel, TCarbonWSLabel);
//  RegisterWSComponent(TButtonControl, TCarbonWSButtonControl);
//  RegisterWSComponent(TCustomCheckBox, TCarbonWSCustomCheckBox);
  RegisterWSComponent(TCheckBox, TCarbonWSCustomCheckBox, TCarbonPrivateCheckBox);
//  RegisterWSComponent(TCheckBox, TCarbonWSCheckBox);
//  RegisterWSComponent(TToggleBox, TCarbonWSToggleBox);
  RegisterWSComponent(TRadioButton, TCarbonWSRadioButton, TCarbonPrivateCheckBox);
  RegisterWSComponent(TCustomStaticText, TCarbonWSCustomStaticText);
//  RegisterWSComponent(TStaticText, TCarbonWSStaticText);
////////////////////////////////////////////////////
end.
