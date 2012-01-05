{ $Id: carbonwsstdctrls.pp 15309 2008-06-04 22:12:59Z vincents $}
{
 *****************************************************************************
 *                              CocoaWSStdCtrls.pp                           *
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
unit CocoaWSStdCtrls;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  // Libs
  MacOSAll, CocoaAll,
  // LCL
  Controls, StdCtrls, Graphics, LCLType, LMessages, LCLProc, LCLMessageGlue, Classes,
  // Widgetset
  WSStdCtrls, WSLCLClasses, WSControls, WSProc,
  // LCL Cocoa
  CocoaWSCommon, CocoaPrivate, CocoaUtils;

type

  { TCocoaWSScrollBar }

  TCocoaWSScrollBar = class(TWSScrollBar)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetParams(const AScrollBar: TCustomScrollBar); override;
  end;

  { TCocoaWSCustomGroupBox }

  TCocoaWSCustomGroupBox = class(TWSCustomGroupBox)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TLCLComboboxCallback }

  TLCLComboboxCallback = class(TLCLCommonCallback, IComboBoxCallback)
    procedure ComboBoxWillPopUp;
    procedure ComboBoxWillDismiss;
    procedure ComboBoxSelectionDidChange;
    procedure ComboBoxSelectionIsChanging;
  end;

  { TCocoaWSCustomComboBox }

  TCocoaWSCustomComboBox = class(TWSCustomComboBox)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    {class function  GetSelStart(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetSelLength(const ACustomComboBox: TCustomComboBox): integer; override;}
    class function  GetItemIndex(const ACustomComboBox: TCustomComboBox): integer; override;
    {class function  GetMaxLength(const ACustomComboBox: TCustomComboBox): integer; override;

    class procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); override;
    class procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;}
    class procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer); override;
    {class procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle); override;}
    class procedure SetReadOnly(const ACustomComboBox: TCustomComboBox; NewReadOnly: boolean); override;
    class procedure SetDropDownCount(const ACustomComboBox: TCustomComboBox; NewCount: Integer); override;

    class function  GetItems(const ACustomComboBox: TCustomComboBox): TStrings; override;
    {class procedure Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean); override;}

    class function GetItemHeight(const ACustomComboBox: TCustomComboBox): Integer; override;
    class procedure SetItemHeight(const ACustomComboBox: TCustomComboBox; const AItemHeight: Integer); override;
  end;

  { TCocoaWSCustomListBox }

  TCocoaWSCustomListBox = class(TWSCustomListBox)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    {class function GetIndexAtXY(const ACustomListBox: TCustomListBox; X, Y: integer): integer; override;
    class function GetItemIndex(const ACustomListBox: TCustomListBox): integer; override;
    class function GetItemRect(const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect): boolean; override;
    class function GetSelCount(const ACustomListBox: TCustomListBox): integer; override;
    class function GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean; override;}
    class function GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
    {class function GetTopIndex(const ACustomListBox: TCustomListBox): integer; override;

    class procedure SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean); override;
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;
    //class procedure SetBorder(const ACustomListBox: TCustomListBox); override;
    class procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer); override;
    class procedure SetSelectionMode(const ACustomListBox: TCustomListBox; const AExtendedSelect, AMultiSelect: boolean); override;
    class procedure SetStyle(const ACustomListBox: TCustomListBox); override;
    class procedure SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean); override;
    class procedure SetTopIndex(const ACustomListBox: TCustomListBox; const NewTopIndex: integer); override;}
  end;

  { TCocoaWSCustomEdit }

  TCocoaWSCustomEdit = class(TWSCustomEdit)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;

    class function  GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetSelLength(const ACustomEdit: TCustomEdit): integer; override;

    {class procedure SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase); override;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); override;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;}
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    {class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;}
  end;
  
  { TCocoaWSCustomMemo }

  TCocoaWSCustomMemo = class(TWSCustomMemo)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;

    class procedure AppendText(const ACustomMemo: TCustomMemo; const AText: string); override;
    {class procedure SetAlignment(const ACustomMemo: TCustomMemo; const AAlignment: TAlignment); override;
    class procedure SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle); override;
    class procedure SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean); override;}
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;

    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class function GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
  end;

  { TLCLButtonCallback }

  TLCLButtonCallback = class(TLCLCommonCallback, IButtonCallback)
  public
    procedure ButtonClick; virtual;
  end;
  TLCLButtonCallBackClass = class of TLCLButtonCallBack;

  { TCocoaWSButton }

  TCocoaWSButton = class(TWSButton)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetDefault(const AButton: TCustomButton; ADefault: Boolean); override;
  end;

  { TLCLCheckBoxCallback }

  TLCLCheckBoxCallback = class(TLCLButtonCallBack)
  public
    procedure ButtonClick; override;
  end;

  { TCocoaWSCustomCheckBox }

  TCocoaWSCustomCheckBox = class(TWSCustomCheckBox)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;
  end;

  { TCocoaWSToggleBox }

  TCocoaWSToggleBox = class(TWSToggleBox)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCocoaWSRadioButton }

  TCocoaWSRadioButton = class(TWSRadioButton)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCocoaWSCustomStaticText }

  TCocoaWSCustomStaticText = class(TWSCustomStaticText)
  private
  protected
  public
//    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
//    class procedure SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment); override;
  end;

function AllocTextView(ATarget: TWinControl; const AParams: TCreateParams; fieldEditor: Boolean): NSTextView;
function AllocButton(const ATarget: TWinControl; const ACallBackClass: TLCLButtonCallBackClass; const AParams: TCreateParams; btnBezel: NSBezelStyle; btnType: NSButtonType): NSButton;
function AllocTextField(ATarget: TWinControl; const AParams: TCreateParams): TCocoaTextField;
function AllocSecureTextField(ATarget: TWinControl; const AParams: TCreateParams): TCocoaSecureTextField;

implementation

function AllocButton(const ATarget: TWinControl; const ACallBackClass: TLCLButtonCallBackClass; const AParams: TCreateParams; btnBezel: NSBezelStyle; btnType: NSButtonType): NSButton;
var
  cap: NSString;
begin
  Result := TCocoaButton.alloc.lclInitWithCreateParams(AParams);
  if Assigned(Result) then
  begin
    TCocoaButton(Result).callback := ACallBackClass.Create(Result, ATarget);
    cap := NSStringUTF8(AParams.Caption);
    Result.setTitle(cap);
    cap.release;
    if btnBezel <> 0 then
      Result.setBezelStyle(btnBezel);
    Result.setButtonType(btnType);
  end;
end;

function AllocTextView(ATarget: TWinControl; const AParams: TCreateParams; fieldEditor: Boolean): NSTextView;
begin
  Result:=TCocoaTextView.alloc;
  if Assigned(Result) then begin
    TCocoaTextView(Result).callback:=TLCLCommonCallback.Create(Result, ATarget);
    Result.initWithFrame(CreateParamsToNSRect(AParams));
  end;
end;

function AllocTextField(ATarget: TWinControl; const AParams: TCreateParams): TCocoaTextField;
begin
  Result:=TCocoaTextField(TCocoaTextField.alloc.lclInitWithCreateParams(AParams));
  if Assigned(Result) then begin
    TCocoaTextField(Result).callback:=TLCLCommonCallback.Create(Result, ATarget);
    Result.initWithFrame(CreateParamsToNSRect(AParams));
    SetNSControlValue(Result, AParams.Caption);
  end;
end;

function AllocSecureTextField(ATarget: TWinControl; const AParams: TCreateParams): TCocoaSecureTextField;
begin
  Result:=TCocoaSecureTextField(TCocoaSecureTextField.alloc);
  if Assigned(Result) then begin
    TCocoaSecureTextField(Result).callback:=TLCLCommonCallback.Create(Result, ATarget);
    Result.initWithFrame(CreateParamsToNSRect(AParams));
    SetNSText(Result.currentEditor, AParams.Caption);
  end;
end;

{ TLCLButtonCallback }

procedure TLCLButtonCallback.ButtonClick;
begin
  SendSimpleMessage(Target, LM_CLICKED);
end;

{ TLCLCheckBoxCallback }

procedure TLCLCheckBoxCallback.ButtonClick;
begin
  inherited;
  SendSimpleMessage(Target, LM_CHANGED);
  // todo: win32 has something about dbcheckbox handling here. so maybe we need to handle it special too
end;

{ TLCLComboboxCallback }

procedure TLCLComboboxCallback.ComboBoxWillPopUp;
begin
  LCLSendDropDownMsg(Target);
end;

procedure TLCLComboboxCallback.ComboBoxWillDismiss;
begin
  LCLSendCloseUpMsg(Target);
end;

procedure TLCLComboboxCallback.ComboBoxSelectionDidChange;
begin
  // todo: send correct messages here. LM_CHANGED must be sent on editbox change
  SendSimpleMessage(Target, LM_CHANGED);
  SendSimpleMessage(Target, LM_SELCHANGE);
end;

procedure TLCLComboboxCallback.ComboBoxSelectionIsChanging;
begin

end;


{ TCocoaWSButton }

{------------------------------------------------------------------------------
  Method:  TCocoaWSButton.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Cocoa interface

  Creates new button control in Cocoa interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCocoaWSButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  btn: NSButton;
begin
  btn := AllocButton(AWinControl, TLCLButtonCallback, AParams, NSRoundedBezelStyle, NSMomentaryPushInButton);
  Result := TLCLIntfHandle(btn);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSButton.SetDefault
  Params:  AButton  - LCL button control
           ADefault

  Sets button default indication in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSButton.SetDefault(const AButton: TCustomButton; ADefault: Boolean);
var
  cf: NSString;
const
  DefEq: array [Boolean] of String = (#0, #13);
begin
  if not AButton.HandleAllocated then
    Exit;
  cf := NSStringUtf8(DefEq[ADefault]);

  NSButton(AButton.Handle).setKeyEquivalent(cf);
  cf.release;
end;

{ TCocoaWSCustomCheckBox }

{------------------------------------------------------------------------------
  Method:  TCocoaWSCustomCheckBox.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Cocoa interface

  Creates new check box in Cocoa interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCocoaWSCustomCheckBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  btn: NSButton;
begin
  btn := AllocButton(AWinControl, TLCLCheckBoxCallBack, AParams, 0, NSSwitchButton);
  Result := TLCLIntfHandle(btn);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSCustomCheckBox.RetrieveState
  Params:  ACustomCheckBox - LCL custom check box
  Returns: State of check box

  Retrieves the state of check box in Cocoa interface
 ------------------------------------------------------------------------------}
class function TCocoaWSCustomCheckBox.RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
var
  state : NSInteger;
begin
  Result := cbUnchecked;
  if not ACustomCheckBox.HandleAllocated then
    Exit;
  state := NSButton(ACustomCheckBox.Handle).state;
  case state of
    NSOnState: Result := cbChecked;
    NSMixedState: Result := cbGrayed;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSCustomCheckBox.SetState
  Params:  ACustomCheckBox - LCL custom check box
           NewState        - New state of check box

  Sets the new state of check box in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSCustomCheckBox.SetState(
  const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
const
  buttonState: array [TcheckBoxState] of NSInteger = (NSOffState, NSOnState, NSMixedState);
begin
  if ACustomCheckBox.HandleAllocated then
    NSButton(ACustomCheckBox.Handle).setState(buttonState[NewState]);
end;

{ TCocoaWSRadioButton }

class function TCocoaWSRadioButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  btn: NSButton;
begin
  btn := AllocButton(AWinControl, TLCLCheckBoxCallBack, AParams, 0, NSRadioButton);
  Result := TLCLIntfHandle(btn);
end;

{ TCocoaWSCustomEdit }

class function TCocoaWSCustomEdit.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  field : NSTextField;
begin
  if TCustomEdit(AWinControl).PasswordChar=#0
    then field:=NSTextField(AllocTextField(AWinControl, AParams))
    else field:=NSTextField(AllocSecureTextField(AWinControl, AParams));
  Result:=TLCLIntfHandle(field);
end;

class function TCocoaWSCustomEdit.GetSelStart(const ACustomEdit: TCustomEdit): integer;
var
  field : TCocoaTextField;
  txt   :  NSText;
begin
  Result:=0;
  field:=TCocoaTextField(ACustomEdit.Handle);
  if not Assigned(field) then Exit;
  txt:=NSText(field.currentEditor);
  if not Assigned(txt) then Exit;

  Result:=txt.selectedRange.location;
end;

class function TCocoaWSCustomEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
var
  field : TCocoaTextField;
  txt   :  NSText;
begin
  Result:=0;
  field:=TCocoaTextField(ACustomEdit.Handle);
  if not Assigned(field) then Exit;
  txt:=NSText(field.currentEditor);
  if not Assigned(txt) then Exit;

  Result:=txt.selectedRange.length;
end;

class procedure TCocoaWSCustomEdit.SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char);
begin
  if (NewChar<>#0) xor TCocoaTextField(ACustomEdit.Handle).isKindOfClass_(NSSecureTextField) then
    RecreateWnd(ACustomEdit);
end;


class procedure TCocoaWSCustomEdit.SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean);
begin
//  NSTextField(ACustomEdit.Handle).setEditable(not NewReadOnly);
end;


type

  { TCocoaMemoStrings }

  TCocoaMemoStrings = class(TStrings)
  private
    FTextView: NSTextView;
  protected
    function GetTextStr: string; override;
    procedure SetTextStr(const Value: string); override;
    function GetCount: Integer; override;
    function Get(Index: Integer): string; override;
  public
    constructor Create(ATextView: NSTextView);
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  end;

{ TCocoaMemoStrings }

constructor TCocoaMemoStrings.Create(ATextView: NSTextView);
begin
  inherited Create;
  FTextView := ATextView;
end;

function TCocoaMemoStrings.GetTextStr: string;
begin
  Result := NSStringToString(FTextView.string_);
end;

procedure TCocoaMemoStrings.SetTextStr(const Value: string);
var
  ns: NSString;
begin
  ns := NSStringUtf8(Value);
  FTextView.setString(ns);
  ns.release;
end;

procedure GetLineStart(const s: AnsiString; LineIndex: Integer; var Offset, LinesSkipped: Integer);
var
  i : Integer;
begin
  i:=1;
  LinesSkipped:=0;
  while (LinesSkipped<>LineIndex) and (i<=length(s)) do begin
    if s[i] in [#10, #13] then begin
      inc(i);
      inc(LinesSkipped);
      if (i<=length(s)) and (s[i] in [#10,#13]) and (s[i-1]<>s[i]) then
        inc(i);
    end else
      inc(i);
  end;
  Offset:=i;
end;

function GetLinesCount(const s: AnsiString): Integer;
var
  ofs : Integer;
begin
  GetLineStart(s, -1, ofs, Result);
end;

function TCocoaMemoStrings.GetCount:Integer;
begin
  Result:=GetLinesCount(GetTextStr);
  inc(Result);
end;

function TCocoaMemoStrings.Get(Index:Integer):string;
var
  s     : AnsiString;
  ofs   : Integer;
  eofs  : Integer;
  t     : Integer;
begin
  s:=GetTextStr;
  GetLineStart(s, Index, ofs, t);
  eofs:=ofs;
  while (eofs<=length(s)) and not (s[eofs] in [#10,#13]) do
    inc(eofs);
  Result:=Copy(s, ofs, eofs-ofs);
end;

procedure TCocoaMemoStrings.Clear;
begin
  SetTextStr('');
end;

procedure TCocoaMemoStrings.Delete(Index:Integer);
var
  s     : AnsiString;
  ofs   : Integer;
  eofs  : Integer;
  t     : Integer;
begin
  s:=GetTextStr;
  GetLineStart(s, Index, ofs, t);
  eofs:=ofs;
  while (eofs<=length(s)) and not (s[eofs] in [#10,#13]) do
    inc(eofs);
  if eofs<=length(s) then begin
    inc(eofs);
    if (eofs<=length(s)) and (s[eofs] in [#10,#13]) and (s[eofs-1]<>s[eofs]) then
      inc(eofs);
  end;
  System.Delete(s, ofs, eofs-ofs);
  SetTextStr(s);
end;

procedure TCocoaMemoStrings.Insert(Index:Integer;const S:string);
var
  txt   : AnsiString;
  ofs   : Integer;
  t     : Integer;
begin
  txt:=GetTextStr;
  GetLineStart(txt, Index, ofs, t);
  System.Insert(s+LineEnding, txt, ofs);
  SetTextStr(txt)
end;

{ TCocoaWSCustomMemo }

function MemoTextView(AWinControl: TWinControl): TCocoaTextView;
begin
  if not Assigned(AWinControl) or (AWinControl.Handle=0) then
    Result := nil
  else
    Result := TCocoaTextView(NSScrollView(AWinControl.Handle).documentView);
end;

class function TCocoaWSCustomMemo.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams):TLCLIntfHandle;
var
  txt: TCocoaTextView;
  ns: NSString;
  scr: TCocoaScrollView;
begin
  txt := TCocoaTextView(NSView(TCocoaTextView.alloc).lclInitWithCreateParams(AParams));
  txt.callback := TLCLCommonCallback.Create(txt, AWinControl);
  ns := NSStringUtf8(AParams.Caption);
  txt.setString(ns);
  ns.release;
  scr := EmbedInScrollView(txt);
  scr.callback := txt.callback;
  Result := TLCLIntfHandle(scr);
end;

class function TCocoaWSCustomMemo.GetStrings(const ACustomMemo: TCustomMemo): TStrings;
var
  txt: TCocoaTextView;
begin
  txt := MemoTextView(ACustomMemo);
  if Assigned(txt) then
    Result := TCocoaMemoStrings.Create(txt)
  else
    Result := nil
end;

class procedure TCocoaWSCustomMemo.AppendText(const ACustomMemo: TCustomMemo;
  const AText: string);
begin
  //todo:
end;

class procedure TCocoaWSCustomMemo.SetReadOnly(const ACustomEdit:TCustomEdit;
  NewReadOnly:boolean);
var
  txt: TCocoaTextView;
begin
  txt := MemoTextView(ACustomEdit);
  if Assigned(txt) then
    txt.setEditable(not NewReadOnly);
end;

class procedure TCocoaWSCustomMemo.SetText(const AWinControl:TWinControl;const AText:String);
var
  txt: TCocoaTextView;
  ns: NSString;
begin
  txt := MemoTextView(AWinControl);
  if not Assigned(txt) then Exit;
  ns := NSStringUtf8(AText);
  txt.setString(ns);
  ns.release;
end;

class function TCocoaWSCustomMemo.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  txt: TCocoaTextView;
begin
  txt := MemoTextView(AWinControl);
  Result := Assigned(txt);
  if Result then
    AText := NSStringToString(txt.string_);
end;

{ TCocoaWSCustomComboBox }

class function TCocoaWSCustomComboBox.CreateHandle(const AWinControl:TWinControl;
  const AParams:TCreateParams):TLCLIntfHandle;
var
  cmb : TCocoaComboBox;
  ns  : NSRect;
begin
  cmb := NSView(TCocoaComboBox.alloc).lclInitWithCreateParams(AParams);
  if not Assigned(cmb) then begin
    Result:=0;
    Exit;
  end;

  cmb.callback:=TLCLComboboxCallback.Create(cmb, AWinControl);
  cmb.list:=TCocoaComboBoxList.Create(cmb);
  cmb.setUsesDataSource(true);
  cmb.setDataSource(cmb);
  cmb.setDelegate(cmb);
  Result:=TLCLIntfHandle(cmb);
  //todo: 26 pixels is the height of 'normal' combobox. The value is taken from the Interface Builder!
  //      use the correct way to set the size constraints
  AWinControl.Constraints.SetInterfaceConstraints(0,26,0,26);
end;

class function TCocoaWSCustomComboBox.GetItemIndex(const ACustomComboBox:
  TCustomComboBox):integer;
begin
  if Assigned(ACustomComboBox) and (ACustomComboBox.Handle<>0) then
    Result:=TCocoaComboBox(ACustomComboBox.Handle).indexOfSelectedItem;
end;

class procedure TCocoaWSCustomComboBox.SetItemIndex(const ACustomComboBox:
  TCustomComboBox;NewIndex:integer);
begin
  if Assigned(ACustomComboBox) and (ACustomComboBox.Handle<>0) then
    TCocoaComboBox(ACustomComboBox.Handle).selectItemAtIndex(NewIndex);
end;

class procedure TCocoaWSCustomComboBox.SetReadOnly(const ACustomComboBox:
  TCustomComboBox;NewReadOnly:boolean);
begin
  if Assigned(ACustomComboBox) and (ACustomComboBox.Handle<>0) then begin
    TCocoaComboBox(ACustomComboBox.Handle).setEditable(not NewReadOnly);
    TCocoaComboBox(ACustomComboBox.Handle).setSelectable(True);
  end;
end;

class procedure TCocoaWSCustomComboBox.SetDropDownCount(const ACustomComboBox:
  TCustomComboBox;NewCount:Integer);
begin
  if Assigned(ACustomComboBox) and (ACustomComboBox.Handle<>0) then
    TCocoaComboBox(ACustomComboBox.Handle).setNumberOfVisibleItems(NewCount);
end;

class function TCocoaWSCustomComboBox.GetItems(const ACustomComboBox: TCustomComboBox):TStrings;
begin
  if Assigned(ACustomComboBox) and (ACustomComboBox.Handle<>0) then
    Result:=TCocoaComboBox(ACustomComboBox.Handle).list;
end;

class function TCocoaWSCustomComboBox.GetItemHeight(const ACustomComboBox:
  TCustomComboBox):Integer;
begin
  if Assigned(ACustomComboBox) and (ACustomComboBox.Handle<>0) then
    Result:=Round(TCocoaComboBox(ACustomComboBox.Handle).itemHeight);
end;

class procedure TCocoaWSCustomComboBox.SetItemHeight(const ACustomComboBox:
  TCustomComboBox;const AItemHeight:Integer);
begin
  if Assigned(ACustomComboBox) and (ACustomComboBox.Handle<>0) then
    TCocoaComboBox(ACustomComboBox.Handle).setItemHeight(AItemHeight);
end;

{ TCocoaWSToggleBox }

class function TCocoaWSToggleBox.CreateHandle(const AWinControl:TWinControl;
  const AParams:TCreateParams):TLCLIntfHandle;
var
  btn: NSButton;
  cl: NSButtonCell;
begin
  btn := AllocButton(AWinControl, TLCLButtonCallBack, AParams, NSTexturedRoundedBezelStyle, NSToggleButton);
  cl := NSButtonCell(NSButton(btn).cell);
  cl.setShowsStateBy(cl.showsStateBy or NSContentsCellMask);
  Result := TLCLIntfHandle(btn);
end;

{ TCocoaWSScrollBar }

class function TCocoaWSScrollBar.CreateHandle(const AWinControl:TWinControl;
  const AParams:TCreateParams):TLCLIntfHandle;
var
  scr : TCocoaScrollBar;
begin
  scr:=NSView(TCocoaScrollBar.alloc).lclInitWithCreateParams(AParams);
  scr.callback:=TLCLCommonCallback.Create(scr, AWinControl);
  Result:=TLCLIntfHandle(scr);
end;

class procedure TCocoaWSScrollBar.SetParams(const AScrollBar:TCustomScrollBar);
begin
  if not Assigned(AScrollBar) or (AScrollBar.Handle=0) then Exit;
  with AScrollBar do
   if Max>0 then begin
     TCocoaScrollBar(Handle).setFloatValue_knobProportion( Position/Max, PageSize/Max);
     //if TCocoaScrollBar(Handle).setKnobProportion( PageSize/Max );
     //if TCocoaScrollBar(Handle).setDoubleValue( Position/Max );
   end;
end;

{ TCocoaWSCustomGroupBox }

class function TCocoaWSCustomGroupBox.CreateHandle(const AWinControl:TWinControl;
  const AParams:TCreateParams):TLCLIntfHandle;
var
  box : TCocoaGroupBox;
begin
  box := NSView(TCocoaGroupBox.alloc).lclInitWithCreateParams(AParams);
  box.callback:=TLCLCommonCallback.Create(box, AWinControl);
  Result:=TLCLIntfHandle(box);
end;

{ TCocoaWSCustomListBox }

function GetListView(AWinControl: TWinControl): TCocoaListView;
begin
  if not Assigned(AWinControl) or (AWinControl.Handle=0) then
    Result:=nil
  else
    Result:=TCocoaListView(TCocoaScrollView(AWinControl.Handle).documentView);
end;

class function TCocoaWSCustomListBox.CreateHandle(const AWinControl:TWinControl;
  const AParams:TCreateParams):TLCLIntfHandle;
var
  list    : TCocoaListView;
  scroll  : TCocoaScrollView;
begin
  list:=NSView(TCocoaListView.alloc).lclInitWithCreateParams(AParams);
  list.callback:=TLCLCommonCallback.Create(list, AWinControl);
  list.list:=TCocoaStringList.Create(list);
  list.addTableColumn(NSTableColumn.alloc.init);
  list.setHeaderView(nil);
  list.setDataSource(list);

  if not Assigned(list) then begin
    Result:=0;
    Exit;
  end;
  scroll:=EmbedInScrollView(list);
  scroll.callback:=list.callback;
  scroll.setHasVerticalScroller(true);
  scroll.setAutohidesScrollers(true);
  Result:=TLCLIntfHandle(scroll);
end;

class function TCocoaWSCustomListBox.GetStrings(const ACustomListBox: TCustomListBox):TStrings;
var
  view : TCocoaListView;
begin
  view:=GetListView(ACustomListBox);
  if not Assigned(view) then
    Result:=nil
  else
    Result:=view.list;
end;

end.

