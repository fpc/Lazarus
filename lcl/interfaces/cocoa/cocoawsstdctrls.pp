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
  Controls, StdCtrls, Graphics, LCLType, LMessages, LCLProc, Classes,
  // Widgetset
  WSStdCtrls, WSLCLClasses, WSControls, WSProc,
  // LCL Cocoa
  CocoaWSCommon, CocoaPrivate, CocoaUtils;

type

  { TCocoaWSScrollBar }

  TCocoaWSScrollBar = class(TWSScrollBar)
  private
  protected
  public
//    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
//    class procedure SetParams(const AScrollBar: TCustomScrollBar); override;
  end;

  { TCocoaWSCustomGroupBox }

  TCocoaWSCustomGroupBox = class(TWSCustomGroupBox)
  private
  protected
  public
//    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCocoaWSGroupBox }

  TCocoaWSGroupBox = class(TWSGroupBox)
  private
  protected
  public
  end;

  { TCocoaWSCustomComboBox }

  TCocoaWSCustomComboBox = class(TWSCustomComboBox)
  private
  protected
  public
{    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function  GetSelStart(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetSelLength(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetItemIndex(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetMaxLength(const ACustomComboBox: TCustomComboBox): integer; override;

    class procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); override;
    class procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer); override;
    class procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle); override;
    class procedure SetReadOnly(const ACustomComboBox: TCustomComboBox; NewReadOnly: boolean); override;

    class function  GetItems(const ACustomComboBox: TCustomComboBox): TStrings; override;
    class procedure Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean); override;}
  end;

  { TCocoaWSComboBox }

  TCocoaWSComboBox = class(TWSComboBox)
  private
  protected
  public
  end;

  { TCocoaWSCustomListBox }

  TCocoaWSCustomListBox = class(TWSCustomListBox)
  private
  protected
  public
{    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function GetIndexAtXY(const ACustomListBox: TCustomListBox; X, Y: integer): integer; override;
    class function GetItemIndex(const ACustomListBox: TCustomListBox): integer; override;
    class function GetItemRect(const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect): boolean; override;
    class function GetSelCount(const ACustomListBox: TCustomListBox): integer; override;
    class function GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean; override;
    class function GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
    class function GetTopIndex(const ACustomListBox: TCustomListBox): integer; override;

    class procedure SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean); override;
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;
    //class procedure SetBorder(const ACustomListBox: TCustomListBox); override;
    class procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer); override;
    class procedure SetSelectionMode(const ACustomListBox: TCustomListBox; const AExtendedSelect, AMultiSelect: boolean); override;
    class procedure SetStyle(const ACustomListBox: TCustomListBox); override;
    class procedure SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean); override;
    class procedure SetTopIndex(const ACustomListBox: TCustomListBox; const NewTopIndex: integer); override;}
  end;

  { TCocoaWSListBox }

  TCocoaWSListBox = class(TWSListBox)
  private
  protected
  public
  end;

  { TCocoaWSCustomEdit }

  TCocoaWSCustomEdit = class(TWSCustomEdit)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;

    class function  GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetSelLength(const ACustomEdit: TCustomEdit): integer; override;

    {class procedure SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase); override;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); override;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;}
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    {class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;}
  end;
  
  { TCocoaWSCustomMemo }

  TCocoaWSCustomMemo = class(TWSCustomMemo)
  {published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function  GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;

    class procedure AppendText(const ACustomMemo: TCustomMemo; const AText: string); override;
    class procedure SetAlignment(const ACustomMemo: TCustomMemo; const AAlignment: TAlignment); override;
    class procedure SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle); override;
    class procedure SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean); override;}
  end;

  { TCocoaWSEdit }

  TCocoaWSEdit = class(TWSEdit)
  private
  protected
  public
  end;

  { TCocoaWSMemo }

  TCocoaWSMemo = class(TWSMemo)
  private
  protected
  public
  end;

  { TCocoaWSCustomLabel }

  {TCocoaWSCustomLabel = class(TWSCustomLabel)
  private
  protected
  public
  end;}

  { TCocoaWSLabel }

  {TCocoaWSLabel = class(TWSLabel)
  private
  protected
  public
  end;}

  { TCocoaWSButtonControl }

  TCocoaWSButtonControl = class(TWSButtonControl)
  end;

  { TCocoaWSButton }

  TCocoaWSButton = class(TWSButton)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetDefault(const AButton: TCustomButton; ADefault: Boolean); override;
  end;

  { TCocoaWSCustomCheckBox }

  TCocoaWSCustomCheckBox = class(TWSCustomCheckBox)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function  RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;
  end;

  { TCocoaWSCheckBox }

  TCocoaWSCheckBox = class(TWSCheckBox)
  private
  protected
  public
  end;

  { TCocoaWSToggleBox }

  TCocoaWSToggleBox = class(TWSToggleBox)
  private
  protected
  public
//    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
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

  { TCocoaWSStaticText }

  TCocoaWSStaticText = class(TWSStaticText)
  private
  protected
  public
  end;

function AllocTextView(ATarget: TWinControl; const AParams: TCreateParams; fieldEditor: Boolean): NSTextView;

implementation

procedure DefaultViewSettings(view: NSView);
begin
  view.setAutoresizingMask(NSViewMinYMargin or NSViewMaxXMargin);
end;

function AllocButton(ATarget: TWinControl; const AParams: TCreateParams; btnBezel: NSBezelStyle; btnType: NSButtonType): NSButton;
begin
  Result:=TCocoaButton.alloc;
  if Assigned(Result) then begin
    TCocoaButton(Result).callback:=TControlCallback.Create(Result, ATarget);
    Result.initWithFrame(CreateParamsToNSRect(AParams));
    Result.setTitle(NSStringUTF8(AParams.Caption));
    if btnBezel<>0 then Result.setBezelStyle(btnBezel);
    Result.setButtonType(btnType);
    DefaultViewSettings(Result);
  end;
end;

function AllocTextView(ATarget: TWinControl; const AParams: TCreateParams; fieldEditor: Boolean): NSTextView;
begin
  Result:=TCocoaTextView.alloc;
  if Assigned(Result) then begin
    TCocoaTextView(Result).callback:=TControlCallback.Create(Result, ATarget);
    Result.initWithFrame(CreateParamsToNSRect(AParams));
    DefaultViewSettings(Result);
    Result.setFieldEditor(fieldEditor);
    {Result.setTitle(NSStringUTF8(AParams.Caption));}
  end;
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
  btn : NSButton;
begin
  btn:=AllocButton(AWinControl, AParams, NSRoundedBezelStyle, NSMomentaryPushInButton);
  if Assigned(btn) then
    AddViewToNSObject(btn, NSObject(AParams.WndParent), AParams.X, AParams.Y);
  Result:=TLCLIntfHandle(btn);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSButton.SetDefault
  Params:  AButton  - LCL button control
           ADefault

  Sets button default indication in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSButton.SetDefault(const AButton: TCustomButton;
  ADefault: Boolean);
var
  cf :NSString;
const
  DefEq : array [Boolean] of String = (#0, #13);
begin
  if AButton.Handle=0 then Exit;
  cf:=NSStringUtf8(DefEq[ADefault]);

  NSButton(AButton.Handle).setKeyEquivalent(cf);
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
  btn : NSButton;
begin
  btn:=AllocButton(AWinControl, AParams, 0, NSSwitchButton);
  if Assigned(btn) then
      AddViewToNSObject(btn, NSObject(AParams.WndParent), AParams.X, AParams.Y);
  Result:=TLCLIntfHandle(btn);
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
  if ACustomCheckBox.Handle=0 then Exit;
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
  if ACustomCheckBox.Handle=0 then Exit;
  NSButton(ACustomCheckBox.Handle).setState( buttonState[NewState]);
end;

{ TCocoaWSRadioButton }

class function TCocoaWSRadioButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  btn : NSButton;
begin
  btn:=AllocButton(AWinControl, AParams, 0, NSRadioButton);
  if Assigned(btn) then
     AddViewToNSObject(btn, NSObject(AParams.WndParent), AParams.X, AParams.Y);
  Result:=TLCLIntfHandle(btn);
end;

{ TCocoaWSCustomEdit }

class function  TCocoaWSCustomEdit.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  txt     : NSTextView;
begin
  txt:=AllocTextView(AWinControl, AParams, true);
  if Assigned(txt) then
    AddViewToNSObject(txt, NSObject(AParams.WndParent), AParams.X, AParams.Y);

  Result:=TLCLIntfHandle(txt);
end;

class function TCocoaWSCustomEdit.GetSelStart(const ACustomEdit: TCustomEdit): integer;
begin
  Result:=0;
end;

class function TCocoaWSCustomEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
begin
  Result:=0;
end;

class procedure TCocoaWSCustomEdit.SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean);
begin
  if ACustomEdit.Handle=0 then Exit;

end;

end.

