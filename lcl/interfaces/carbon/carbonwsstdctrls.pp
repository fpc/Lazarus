{ $Id$}
{
 *****************************************************************************
 *                              CarbonWSStdCtrls.pp                              *
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
  Controls, StdCtrls, LCLType,
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
    class procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox;
      const OldShortCut, NewShortCut: TShortCut); override;
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
  end;

  { TCarbonWSCustomStaticText }

  TCarbonWSCustomStaticText = class(TWSCustomStaticText)
  private
  protected
  public
  end;

  { TCarbonWSStaticText }

  TCarbonWSStaticText = class(TWSStaticText)
  private
  protected
  public
  end;


implementation

class function  TCarbonWSCustomEdit.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  Edit: TCustomEdit;
  Control: ControlRef;
  CFString: CFStringRef;
  R: Rect;
  Info: PWidgetInfo;
  IsPassword: PBoolean;
  SingleLine: Boolean = True;
begin
  Result := 0;
  Edit := AWinControl as TCustomEdit;

  R.Left := AParams.X;
  R.Top := AParams.Y;
  R.Right := AParams.X + AParams.Width;
  R.Bottom := AParams.Y + AParams.Height;

  CFString := CFStringCreateWithCString(nil, Pointer(AParams.Caption), DEFAULT_CFSTRING_ENCODING);
  if CreateEditUniCodeTextControl(WindowRef(AParams.WndParent), R, CFString, (Edit.PasswordChar <> #0), nil, Control) = noErr
  then Result := TLCLIntfHandle(Control);
  CFRelease(Pointer(CFString));
  if Result = 0 then Exit;

  New(IsPassword);
  IsPassword^ := Edit.PasswordChar <> #0;
  
  SetControlData(Control, kControlEntireControl, kControlEditTextSingleLineTag,
                 SizeOf(Boolean), @SingleLine);


  Info := CreateWidgetInfo(Control, AWinControl, cwtControlRef);
  Info^.UserData := IsPassword;
  Info^.DataOwner := True;
  TCarbonPrivateHandleClass(WSPrivate).RegisterEvents(Info);
end;

class function TCarbonWSCustomEdit.GetSelStart(const ACustomEdit: TCustomEdit): integer;
var
  Control: ControlRef;
  SelData: ControlEditTextSelectionRec;
  RecSize: FPCMacOSAll.Size;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'GetSelStart')
  then Exit;

  Result := 0;
  Control := ControlRef(ACustomEdit.Handle);
  if GetControlData(Control, kControlEntireControl, kControlEditTextSelectionTag,
      SizeOf(ControlEditTextSelectionRec), @SelData, @RecSize) <> noErr
  then Exit;

  Result := SelData.SelStart;
end;

class function TCarbonWSCustomEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
var
  Control: ControlRef;
  SelData: ControlEditTextSelectionRec;
  RecSize: FPCMacOSAll.Size;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'GetSelLength')
  then Exit;

  Result := 0;
  Control := ControlRef(ACustomEdit.Handle);

  if GetControlData(Control, kControlEntireControl, kControlEditTextSelectionTag,
                    SizeOf(ControlEditTextSelectionRec), @SelData, @RecSize) <> noErr then exit;

  Result := SelData.SelEnd - SelData.SelStart;
end;

class function TCarbonWSCustomEdit.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
var
  Control: ControlRef;
  TextType: ResType;
  CFString: CFStringRef;
  RecSize: FPCMacOSAll.Size;
  Str: Pointer;
  StrSize: CFIndex; //Integer;
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetText')
  then Exit;

  Result := False;
  Control := ControlRef(AWinControl.Handle);
  if PBoolean(GetWidgetInfo(Pointer(AWincontrol.Handle))^.UserData)^ = True then// IsPassword
    TextType := kControlEditTextPasswordCFStringTag
  else
    TextType := kControlEditTextCFStringTag;
    
  if GetControlData(Control, kControlEntireControl, TextType,
      SizeOf(CFStringRef), @CFString, @RecSize) <> noErr
  then Exit;

  StrSize := CFStringGetLength(CFString)*SizeOf(WideChar);
  GetMem(Str,(StrSize));
  Result := CFStringGetCString(CFString, Str, StrSize, DEFAULT_CFSTRING_ENCODING);

  CFRelease(Pointer(CFString));
  
  if Result = False then Exit;
  AText := PChar(Str);
end;

class procedure TCarbonWSCustomEdit.SetCharCase(const ACustomEdit: TCustomEdit;
  NewCase: TEditCharCase);
begin
 // TODO
end;

class procedure TCarbonWSCustomEdit.SetEchoMode(const ACustomEdit: TCustomEdit;
  NewMode: TEchoMode);
begin
 //TODO
end;

class procedure TCarbonWSCustomEdit.SetMaxLength(const ACustomEdit: TCustomEdit;
  NewLength: integer);
begin
 //AFAICS this will have to be checked in a callback
end;

class procedure TCarbonWSCustomEdit.SetPasswordChar(const ACustomEdit: TCustomEdit;
  NewChar: char);
var
  NeedsPassword: Boolean;
  IsPassword: Boolean;
  Info: PWidgetInfo;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetReadOnly')
  then Exit;

  Info := GetWidgetInfo(Pointer(ACustomEdit.Handle));
  IsPassword := PBoolean(Info^.UserData)^;
  NeedsPassword := (NewChar <> #0);

  if IsPassword = NeedsPassword then exit;
  PBoolean(Info^.UserData)^ := NeedsPassword;
  RecreateWnd(ACustomEdit);

end;

class procedure TCarbonWSCustomEdit.SetReadOnly(const ACustomEdit: TCustomEdit;
  NewReadOnly: boolean);
var
  Control: ControlRef;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetReadOnly')
  then Exit;

  Control := ControlRef(ACustomEdit.Handle);

  SetControlData(Control, kControlEntireControl, kControlEditTextLockedTag,
                 SizeOf(Boolean), @NewReadOnly);
end;

class procedure TCarbonWSCustomEdit.SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer);
var
  Control: ControlRef;
  SelData: ControlEditTextSelectionRec;
  RecSize: FPCMacOSAll.Size;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'GetSelStart')
  then Exit;

  Control := ControlRef(ACustomEdit.Handle);

  if GetControlData(Control, kControlEntireControl, kControlEditTextSelectionTag,
                    SizeOf(ControlEditTextSelectionRec), @SelData, @RecSize) <> noErr then exit;

  SelData.SelStart := NewStart;
  SetControlData(Control, kControlEntireControl, kControlEditTextSelectionTag,
                 SizeOf(ControlEditTextSelectionRec), @SelData);

end;

class procedure TCarbonWSCustomEdit.SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer);
var
  Control: ControlRef;
  SelData: ControlEditTextSelectionRec;
  RecSize: FPCMacOSAll.Size;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetSelLength')
  then Exit;

  Control := ControlRef(ACustomEdit.Handle);
  if GetControlData(Control, kControlEntireControl, kControlEditTextSelectionTag,
                    SizeOf(ControlEditTextSelectionRec), @SelData, @RecSize) <> noErr then Exit;

  SelData.SelEnd := SelData.SelStart + NewLength;
  SetControlData(Control, kControlEntireControl, kControlEditTextSelectionTag,
                 SizeOf(ControlEditTextSelectionRec), @SelData);

end;

class procedure TCarbonWSCustomEdit.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  Control: ControlRef;
  TextType: ResType;
  CFString: CFStringRef;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetText')
  then Exit;

  Control := ControlRef(AWinControl.Handle);
  if PBoolean(GetWidgetInfo(Pointer(AWincontrol.Handle))^.UserData)^ = True then// IsPassword
    TextType := kControlEditTextPasswordCFStringTag
  else
    TextType := kControlEditTextCFStringTag;

  CFString := CFStringCreateWithCString(nil, Pointer(AText), DEFAULT_CFSTRING_ENCODING);
  SetControlData(Control, kControlEntireControl, TextType, SizeOf(CFStringRef), @CFString);

  CFRelease(Pointer(CFString));
end;

class procedure TCarbonWSCustomEdit.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  //TODO
end;

{ TCarbonWSCustomCheckBox }

class function TCarbonWSCustomCheckBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  CheckBox:TCheckBox;
  Control: ControlRef;
  CFString: CFStringRef;
  R: Rect;
  Info: PWidgetInfo;
begin
  Result := 0;
  CheckBox := AWinControl as TCheckBox;

  R.Left := AParams.X;
  R.Top := AParams.Y;
  R.Right := AParams.X + AParams.Width;
  R.Bottom := AParams.Y + AParams.Height;

  CFString := CFStringCreateWithCString(nil, Pointer(AParams.Caption), DEFAULT_CFSTRING_ENCODING);
  if CreateCheckBoxControl(WindowRef(AParams.WndParent), R, CFString, Ord(CheckBox.Checked), True, Control) = noErr
  then Result := TLCLIntfHandle(Control);
  CFRelease(Pointer(CFString));
  if Result = 0 then Exit;

  Info := CreateWidgetInfo(Control, AWinControl, cwtControlRef);

  TCarbonPrivateHandleClass(WSPrivate).RegisterEvents(Info);
end;

class function TCarbonWSCustomCheckBox.RetrieveState(
  const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
var
  Control: ControlRef;
begin
  if not WSCheckHandleAllocated(ACustomCheckBox, 'RetrieveState')
  then Exit;

  Control := ControlRef(ACustomCheckBox.Handle);

  case GetControl32BitValue(Control) of
    kControlCheckBoxCheckedValue   : Result := cbChecked;
    kControlCheckBoxUncheckedValue : Result := cbUnchecked;
    kControlCheckBoxMixedValue     : ; // what the heck does this do?
  end;
end;

class procedure TCarbonWSCustomCheckBox.SetShortCut(
  const ACustomCheckBox: TCustomCheckBox; const OldShortCut,
  NewShortCut: TShortCut);
begin
  // TODO
end;

class procedure TCarbonWSCustomCheckBox.SetState(
  const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
var
  Control: ControlRef;
  Value: UInt32;
begin
  if not WSCheckHandleAllocated(ACustomCheckBox, 'SetState')
  then Exit;

  Control := ControlRef(ACustomCheckBox.Handle);

  Value := kControlCheckBoxMixedValue; // give it a default value
  
  case NewState of
    cbChecked  :  Value := kControlCheckBoxCheckedValue;
    cbUnChecked: Value := kControlCheckBoxUncheckedValue;
    //cbGrayed   : kControlCheckBoxMixedValue; // what the heck does this do?
  end;
  SetControl32BitValue(Control, Value);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TScrollBar, TCarbonWSScrollBar);
//  RegisterWSComponent(TCustomGroupBox, TCarbonWSCustomGroupBox);
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
//  RegisterWSComponent(TRadioButton, TCarbonWSRadioButton);
//  RegisterWSComponent(TCustomStaticText, TCarbonWSCustomStaticText);
//  RegisterWSComponent(TStaticText, TCarbonWSStaticText);
////////////////////////////////////////////////////
end.
