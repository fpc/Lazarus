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
  end;
  
  { TCarbonWSCustomMemo }

  TCarbonWSCustomMemo = class(TWSCustomMemo)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function  GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;

    class procedure AppendText(const ACustomMemo: TCustomMemo; const AText: string); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
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
  
  { TCarbonMemoStrings }

  TCarbonMemoStrings = class(TStrings)
  private
    FStringList: TStringList; // internal strings
    FOwner: TWinControl;      // LCL control owning strings
    FExternalChanged: Boolean;// Carbon strings object has changed
    procedure InternalUpdate;
    procedure ExternalUpdate;
  protected
    function GetTextStr: string; override;
    function GetCount: Integer; override;
    function Get(Index: Integer): string; override;
  public
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure SetText(TheText: PChar); override;
    
    procedure ExternalChanged; dynamic;
  public
    property Owner: TWinControl read FOwner;
  end;


implementation

{ TCarbonWSScrollBar }

{------------------------------------------------------------------------------
  Name:  ScrollBarLiveTrack

  Callback procedure for Carbon scroll bar live tracking
 ------------------------------------------------------------------------------}
procedure ScrollBarLiveTrack(Control: ControlRef; partCode: ControlPartCode);
  {$IFDEF darwin}mwpascal;{$ENDIF}
var
  Msg: TLMScroll;
  AInfo: TCarbonWidgetInfo;
begin
  AInfo := GetCtrlWidgetInfo(Pointer(Control));
  if AInfo = nil then Exit;
  
  DebugLn('ScrollBarLiveTrack ' + DbgS(GetControl32BitValue(Control)));
  
  FillChar(Msg, SizeOf(Msg), 0);
  
  Msg.Msg := LM_HSCROLL;
  Msg.ScrollCode := SB_THUMBTRACK;
  Msg.Pos := GetControl32BitValue(Control);
  Msg.ScrollBar := HWnd(Control);

  DeliverMessage(AInfo.LCLObject, Msg);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSScrollBar.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new scroll bar in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSScrollBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  ScrollBar: TCustomScrollBar;
  Control: ControlRef;
  Info: TCarbonWidgetInfo;
begin
  Result := 0;
  
  ScrollBar := AWinControl as TCustomScrollBar;

  if CreateScrollBarControl(GetTopParentWindow(AWinControl),
    ParamsToCarbonRect(AParams), ScrollBar.Position, ScrollBar.Min, ScrollBar.Max,
    ScrollBar.PageSize, False, nil, Control) = noErr
  then
    Result := TLCLIntfHandle(Control);
      
  if Result = 0 then Exit;

  Info := TCarbonWidgetInfo.CreateForControl(Control, AWinControl);
  TCarbonPrivateHandleClass(WSPrivate).RegisterEvents(Info);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSScrollBar.SetParams
  Params:  AScrollBar - LCL custom scroll bar

  Sets the parameters (Min, Max, PageSize, Position) of scroll bar in Carbon
  interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSScrollBar.SetParams(const AScrollBar: TCustomScrollBar);
begin
  if not WSCheckHandleAllocated(AScrollBar, 'SetParams') then Exit;
  
  SetControl32BitMinimum(ControlRef(AScrollBar.Handle), AScrollBar.Min);
  SetControl32BitMaximum(ControlRef(AScrollBar.Handle), AScrollBar.Max);
  SetControl32BitValue(ControlRef(AScrollBar.Handle), AScrollBar.Position);
  SetControlViewSize(ControlRef(AScrollBar.Handle), AScrollBar.PageSize);
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
var
  Control: ControlRef;
  CFString: CFStringRef;
  Info: TCarbonWidgetInfo;
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

  Info := TCarbonWidgetInfo.CreateForControl(Control, AWinControl);
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
  Info: TCarbonWidgetInfo;
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

  // set edit single line
  SetControlData(Control, kControlEntireControl, kControlEditTextSingleLineTag,
    SizeOf(Boolean), @SingleLine);

  Info := TCarbonWidgetInfo.CreateForControl(Control, AWinControl);
  Info.UserData := IsPassword;
  Info.DataOwner := True;
  TCarbonPrivateHandleClass(WSPrivate).RegisterEvents(Info);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomEdit.GetSelStart
  Params:  ACustomEdit - LCL custom edit
  Returns: Position of selection start

  Retrieves the position of selection start of edit in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomEdit.GetSelStart(const ACustomEdit: TCustomEdit): integer;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'GetSelStart') then Exit;

  if not GetEditControlSelStart(ACustomEdit.Handle, Result) then Result := 0;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomEdit.GetSelLength
  Params:  ACustomEdit - LCL custom edit
  Returns: Length of selection

  Retrieves the length of selection of edit in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'GetSelLength') then Exit;

  if not GetEditControlSelLength(ACustomEdit.Handle, Result) then Result := 0;
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
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetText') then Exit;

  Result := GetEditControlText(AWinControl.Handle, AText);
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
 // checked in a callback
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
  Info: TCarbonWidgetInfo;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetPasswordChar') then Exit;

  Info := GetCtrlWidgetInfo(Pointer(ACustomEdit.Handle));
  IsPassword := PBoolean(Info.UserData)^;
  NeedsPassword := (NewChar <> #0);

  if IsPassword = NeedsPassword then Exit;
  PBoolean(Info.UserData)^ := NeedsPassword;
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
  if not WSCheckHandleAllocated(ACustomEdit, 'SetReadOnly') then Exit;

  SetControlData(ControlRef(ACustomEdit.Handle), kControlEntireControl,
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
  if not WSCheckHandleAllocated(ACustomEdit, 'GetSelStart') then Exit;

  SetEditControlSelStart(ACustomEdit.Handle, NewStart);
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
  if not WSCheckHandleAllocated(ACustomEdit, 'SetSelLength') then Exit;

  SetEditControlSelLength(ACustomEdit.Handle, NewLength);
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
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetText') then Exit;

  SetEditControlText(AWinControl.Handle, AText);
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
var
  Control: ControlRef;
  Info: TCarbonWidgetInfo;
  Options: FPCMacOSAll.OptionBits;
  R: HIRect;
begin
  Result := 0;

  Options := kTXNMonostyledTextMask or kOutputTextInUnicodeEncodingMask;

  R := ParamsToHIRect(AParams);
  if HITextViewCreate(@R, 0, Options, Control) = noErr then
    Result := TLCLIntfHandle(Control);
  if Result = 0 then Exit;

  Info := TCarbonWidgetInfo.CreateForControl(Control, AWinControl);
  TCarbonPrivateHandleClass(WSPrivate).RegisterEvents(Info);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomMemo.GetStrings
  Params:  ACustomMemo - LCL custom memo
  Returns: Strings of memo in Carbon interface

  Creates new strings of memo in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomMemo.GetStrings(const ACustomMemo: TCustomMemo
  ): TStrings;
begin
  if not WSCheckHandleAllocated(ACustomMemo, 'GetStrings') then Exit;

  Result := TCarbonMemoStrings.Create(ACustomMemo);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomMemo.GetStrings
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
  if not WSCheckHandleAllocated(ACustomMemo, 'AppendText') then Exit;

  if Length(AText) > 0 then
  begin
    GetText(ACustomMemo, S);
    S := S + AText;
    SetText(ACustomMemo, S);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomMemo.SetColor
  Params:  AWinControl - LCL control
  Returns: Nothing

  Sets the color of memo in Carbon interface according to the LCL control
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomMemo.SetColor(const AWinControl: TWinControl);
var
  AColor: CGColorRef;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetColor') then Exit;
  
  AColor := CreateCGColor(AWinControl.Color);
  try
    HITextViewSetBackgroundColor(HIViewRef(AWinControl.Handle), AColor);
  finally
    CGColorRelease(AColor);
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
  if not WSCheckHandleAllocated(ACustomEdit, 'SetPasswordChar') then Exit;

  TXNEchoMode(HITextViewGetTXNObject(HIViewRef(ACustomEdit.Handle)), UniChar(NewChar),
    CreateTextEncoding(kTextEncodingMacRoman, kTextEncodingDefaultVariant,
    kUnicodeUTF8Format), NewChar <> #0);
    
  InvalidateCarbonControl(ACustomEdit.Handle);
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
  Data: Boolean;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetReadOnly') then Exit;

  Tag := kTXNNoUserIOTag;
  if NewReadOnly then
    Data := kTXNReadOnly
  else
    Data := kTXNReadWrite;

  TXNSetTXNObjectControls(HITextViewGetTXNObject(HIViewRef(ACustomEdit.Handle)),
    False, 1, @Tag, @Data);
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
  Data: Boolean;
begin
  if not WSCheckHandleAllocated(ACustomMemo, 'SetWordWrap') then Exit;

  Tag := kTXNWordWrapStateTag;
  if NewWordWrap then
    Data := kTXNAutoWrap
  else
    Data := kTXNNoAutoWrap;

  TXNSetTXNObjectControls(HITextViewGetTXNObject(HIViewRef(ACustomMemo.Handle)),
    False, 1, @Tag, @Data);
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
  Info: TCarbonWidgetInfo;
  Value: UInt32;
begin
  Result := 0;
  
  case (AWinControl as TCustomCheckBox).State of
    cbChecked  : Value := kControlCheckBoxCheckedValue;
    cbUnChecked: Value := kControlCheckBoxUncheckedValue;
    cbGrayed   : Value := kControlCheckBoxMixedValue;
  end;

  CreateCarbonString(AParams.Caption, CFString);
  try
    if CreateCheckBoxControl(GetTopParentWindow(AWinControl),
      ParamsToCarbonRect(AParams), CFString, Value, True, Control) = noErr
    then
      Result := TLCLIntfHandle(Control);
  finally
    FreeCarbonString(CFString);
  end;
  if Result = 0 then Exit;

  Info := TCarbonWidgetInfo.CreateForControl(Control, AWinControl);
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
    kControlCheckBoxMixedValue     : Result := cbGrayed;
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

  case NewState of
    cbChecked  : Value := kControlCheckBoxCheckedValue;
    cbUnChecked: Value := kControlCheckBoxUncheckedValue;
    cbGrayed   : Value := kControlCheckBoxMixedValue;
  end;
  SetControl32BitValue(ControlRef(ACustomCheckBox.Handle), Value);
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
var
  Control: ControlRef;
  CFString: CFStringRef;
  Info: TCarbonWidgetInfo;
  Value: UInt32;
begin
  Result := 0;

  case (AWinControl as TToggleBox).State of
    cbChecked  : Value := kControlCheckBoxCheckedValue;
    cbUnChecked: Value := kControlCheckBoxUncheckedValue;
    cbGrayed   : Value := kControlCheckBoxMixedValue;
  end;

  CreateCarbonString(AParams.Caption, CFString);
  try
    if CreateBevelButtonControl(GetTopParentWindow(AWinControl),
      ParamsToCarbonRect(AParams), CFString, kControlBevelButtonNormalBevel,
      kControlBehaviorToggles, nil, 0, 0, 0, Control) = noErr
    then
      Result := TLCLIntfHandle(Control);
  finally
    FreeCarbonString(CFString);
  end;
  if Result = 0 then Exit;
  
  SetControl32BitValue(Control, Value);

  Info := TCarbonWidgetInfo.CreateForControl(Control, AWinControl);
  TCarbonPrivateHandleClass(WSPrivate).RegisterEvents(Info);
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
  Info: TCarbonWidgetInfo;
  Value: UInt32;
begin
  Result := 0;

  case (AWinControl as TRadioButton).State of
    cbChecked  : Value := kControlCheckBoxCheckedValue;
    cbUnChecked: Value := kControlCheckBoxUncheckedValue;
    cbGrayed   : Value := kControlCheckBoxMixedValue;
  end;

  CreateCarbonString(AParams.Caption, CFString);
  try
    if CreateRadioButtonControl(GetTopParentWindow(AWinControl),
      ParamsToCarbonRect(AParams), CFString, Value, True, Control) = noErr
    then
      Result := TLCLIntfHandle(Control);
  finally
    FreeCarbonString(CFString);
  end;
  if Result = 0 then Exit;

  Info := TCarbonWidgetInfo.CreateForControl(Control, AWinControl);
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
  Info: TCarbonWidgetInfo;
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
    
  Info := TCarbonWidgetInfo.CreateForControl(Control, AWinControl);
  TCarbonPrivateHandleClass(WSPrivate).RegisterEvents(Info);
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

{ TCarbonMemoStrings }

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.InternalUpdate
  Returns: Nothing

  Updates the internal strings from Carbon interface
 ------------------------------------------------------------------------------}
procedure TCarbonMemoStrings.InternalUpdate;
var
  S: String;
begin
  S := '';
  //DebugLn('TCarbonMemoStrings.InternalUpdate ' + FOwner.Name);
  if GetEditControlText(FOwner.Handle, S) then
    FStringList.Text := S;
  
  FExternalChanged := False;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.ExternalUpdate
  Returns: Nothing

  Updates the strings in Carbon interface from internal
 ------------------------------------------------------------------------------}
procedure TCarbonMemoStrings.ExternalUpdate;
begin
  //DebugLn('TCarbonMemoStrings.ExternalUpdate ' + FOwner.Name + ' Text: ' + FStringList.Text);
  SetEditControlText(FOwner.Handle, FStringList.Text);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.GetTextStr
  Returns: Text of Carbon strings

  Returns the text
 ------------------------------------------------------------------------------}
function TCarbonMemoStrings.GetTextStr: string;
begin
  if FExternalChanged then InternalUpdate;
  Result := FStringList.Text;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.GetCount
  Returns: Number of lines

  Returns the number of lines
 ------------------------------------------------------------------------------}
function TCarbonMemoStrings.GetCount: Integer;
begin
  if FExternalChanged then InternalUpdate;
  Result := FStringList.Count;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.Get
  Params:  Index - Line index
  Returns: Text on line

  Returns the text on line with the specified index
 ------------------------------------------------------------------------------}
function TCarbonMemoStrings.Get(Index: Integer): string;
begin
  if FExternalChanged then InternalUpdate;
  Result := FStringList[Index];
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.Create
  Params:  AOwner     - LCL owner of strings
  Returns: Nothing

  Creates new strings from Carbon memo strings
 ------------------------------------------------------------------------------}
constructor TCarbonMemoStrings.Create(AOwner: TWinControl);
begin
  FOwner := AOwner;
  FStringList := TStringList.Create;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.Destroy
  Returns: Nothing

  Releases strings from Carbon memo strings
 ------------------------------------------------------------------------------}
destructor TCarbonMemoStrings.Destroy;
begin
  FStringList.Free;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.Assign
  Params:  Source - Object to assing
  Returns: Nothing

  Assings strings object
 ------------------------------------------------------------------------------}
procedure TCarbonMemoStrings.Assign(Source: TPersistent);
begin
  if (Source = Self) or (Source = nil) then Exit;
  if Source is TStrings then
  begin
    FStringList.Clear;
    FStringList.Text := TStrings(Source).Text;
    ExternalUpdate;
  end
  else
    inherited Assign(Source);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.Clear
  Returns: Nothing

  Clears strings
 ------------------------------------------------------------------------------}
procedure TCarbonMemoStrings.Clear;
begin
  FStringList.Clear;
  ExternalUpdate;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.Delete
  Params:  Index - Line index
  Returns: Nothing

  Deletes line with the specified index from strings
 ------------------------------------------------------------------------------}
procedure TCarbonMemoStrings.Delete(Index: Integer);
begin
  FStringList.Delete(Index);
  ExternalUpdate;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.Insert
  Params:  Index - Line index
           S     - Text to insert
  Returns: Nothing

  Inserts the text on line with the specified index
 ------------------------------------------------------------------------------}
procedure TCarbonMemoStrings.Insert(Index: Integer; const S: string);
begin
  FStringList.Insert(Index, S);
  ExternalUpdate;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.SetText
  Params:  TheText - Text to set
  Returns: Nothing

  Sets the text of strings
 ------------------------------------------------------------------------------}
procedure TCarbonMemoStrings.SetText(TheText: PChar);
begin
  FStringList.Text := TheText;
  ExternalUpdate;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.ExternalChanged
  Returns: Nothing

  Notifies that strings object in Carbon interface has changed
 ------------------------------------------------------------------------------}
procedure TCarbonMemoStrings.ExternalChanged;
begin
  FExternalChanged := True;
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TScrollBar, TCarbonWSScrollBar, TCarbonPrivateValueControl);
  RegisterWSComponent(TCustomGroupBox, TCarbonWSCustomGroupBox);
//  RegisterWSComponent(TGroupBox, TCarbonWSGroupBox);
//  RegisterWSComponent(TCustomComboBox, TCarbonWSCustomComboBox);
//  RegisterWSComponent(TComboBox, TCarbonWSComboBox);
//  RegisterWSComponent(TCustomListBox, TCarbonWSCustomListBox);
//  RegisterWSComponent(TListBox, TCarbonWSListBox);
  RegisterWSComponent(TCustomEdit, TCarbonWSCustomEdit, TCarbonPrivateEdit);
  RegisterWSComponent(TCustomMemo, TCarbonWSCustomMemo, TCarbonPrivateEdit);
//  RegisterWSComponent(TEdit, TCarbonWSEdit);
//  RegisterWSComponent(TMemo, TCarbonWSMemo);
//  RegisterWSComponent(TCustomLabel, TCarbonWSCustomLabel);
//  RegisterWSComponent(TLabel, TCarbonWSLabel);
//  RegisterWSComponent(TButtonControl, TCarbonWSButtonControl);
  RegisterWSComponent(TCustomCheckBox, TCarbonWSCustomCheckBox, TCarbonPrivateValueControl);
//  RegisterWSComponent(TCheckBox, TCarbonWSCheckBox);
  RegisterWSComponent(TToggleBox, TCarbonWSToggleBox, TCarbonPrivateValueControl);
  RegisterWSComponent(TRadioButton, TCarbonWSRadioButton, TCarbonPrivateValueControl);
  RegisterWSComponent(TCustomStaticText, TCarbonWSCustomStaticText);
//  RegisterWSComponent(TStaticText, TCarbonWSStaticText);
////////////////////////////////////////////////////
end.
