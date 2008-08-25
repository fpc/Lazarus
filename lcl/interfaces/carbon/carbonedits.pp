{ $Id$
                  --------------------------------------------
                  carbonedits.pp  -  Carbon edit-like controls
                  --------------------------------------------

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CarbonEdits;

{$mode objfpc}{$H+}

interface

// debugging defines
{$I carbondebug.inc}

uses
 // rtl+ftl
  Types, Classes, SysUtils, Math, Contnrs,
 // carbon bindings
  MacOSAll,
 // LCL
  LMessages, LCLMessageGlue, LCLProc, LCLType, Graphics, Controls, StdCtrls, ExtCtrls,
  Spin,
 // widgetset
  WSControls, WSLCLClasses, WSProc,
 // LCL Carbon
  CarbonDef, CarbonPrivate, CarbonGDIObjects;
  
type

  { TCarbonControlWithEdit }

  TCarbonControlWithEdit = class(TCarbonControl)
  private
    FMaxLength: Integer;
  protected
    procedure LimitTextLength;
    procedure AdaptCharCase;
    function GetEditPart: ControlPartCode; virtual;
    procedure RegisterEvents; override;
  public
    procedure TextDidChange; dynamic;
  public
    function GetPreferredSize: TPoint; override;

    function GetSelStart(var ASelStart: Integer): Boolean;
    function GetSelLength(var ASelLength: Integer): Boolean;
    function SetSelStart(ASelStart: Integer): Boolean;
    function SetSelLength(ASelLength: Integer): Boolean;

    function GetText(var S: String): Boolean; override;
    function SetText(const S: String): Boolean; override;
  public
    property MaxLength: Integer read FMaxLength write FMaxLength;
    procedure SetReadOnly(AReadOnly: Boolean); virtual;
  end;

  { TCarbonComboBox }

  TCarbonComboBox = class(TCarbonControlWithEdit)
  private
    FItemIndex: Integer;
    FReadOnly: Boolean;
    FPopupMenu: MenuRef;
    FTimer: TTimer;
    FLastDroppedDown: Boolean;
    procedure DropDownTimer(Sender: TObject);
    // there is no drop down nor close up event in Carbon, we must check it with timer
  protected
    procedure RegisterEvents; override;
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
    function GetEditPart: ControlPartCode; override;
    function GetPopupButtonMenu: MenuRef;
  public
    class function GetValidEvents: TCarbonControlEvents; override;
    procedure ListItemSelected(AIndex: Integer); virtual;
    procedure ValueChanged; override;
    procedure FocusSet; override;
    procedure FocusKilled; override;
    procedure TextDidChange; override;
  public
    function GetText(var S: String): Boolean; override;
    function SetBounds(const ARect: TRect): Boolean; override;
    procedure SetReadOnly(AReadOnly: Boolean); override;
    
    function GetItemIndex: Integer;
    function SetItemIndex(AIndex: Integer): Boolean;
    
    procedure Insert(AIndex: Integer; const S: String);
    procedure Remove(AIndex: Integer);
    
    function DropDown(ADropDown: Boolean): Boolean;
    function IsDroppedDown: Boolean;
  end;
  
  { TCarbonCustomEdit }

  TCarbonCustomEdit = class(TCarbonControlWithEdit)
  public
    procedure SetPasswordChar(AChar: Char); virtual; abstract;
    function FilterKeyPress(SysKey: Boolean; const Char: TUTF8Char): Boolean; override;
  end;
  
  { TCarbonSpinEdit }

  TCarbonSpinEdit = class(TCarbonCustomEdit)
  private
    FUpDown: ControlRef;
    FValue: Double;
    FMin: Double;
    FMax: Double;
    FIncrement: Double;
    FDecimalPlaces: Integer;
    function UpDownThemeWidth: Integer;
    function FocusRectThemeOutset: Integer;
    function GetEditBounds(const ARect: HIRect): HIRect;
    function GetUpDownBounds(const ARect: HIRect): HIRect;
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
    function GetFrame(Index: Integer): ControlRef; override;
    function GetFrameBounds(var ARect: TRect): Boolean; override;
  public
    class function GetFrameCount: Integer; override;
    class function GetValidEvents: TCarbonControlEvents; override;
    procedure TextDidChange; override;
    procedure DoAction(AControlPart: ControlPartCode); override;
    function SetBounds(const ARect: TRect): Boolean; override;
    procedure SetPasswordChar(AChar: Char); override;
    function SetText(const S: String): Boolean; override;
  public
    procedure UpdateControl;
    property Value: Double read FValue;
  end;

  { TCarbonEdit }

  TCarbonEdit = class(TCarbonCustomEdit)
  private
    FIsPassword: Boolean;
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    function GetText(var S: String): Boolean; override;
    function SetText(const S: String): Boolean; override;
    procedure SetPasswordChar(AChar: Char); override;
  end;

  { TCarbonMemo }

  TCarbonMemo = class(TCarbonCustomEdit)
  private
    FScrollView: HIViewRef;
    FScrollBars: TScrollStyle;
    procedure SetScrollBars(const AValue: TScrollStyle);
  protected
    function GetFrame(Index: Integer): ControlRef; override;
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
    procedure GetLineOffset(AIndex: Integer; out AStart, AEnd: TXNOffset);
  public
    procedure TextDidChange; override;
    function FilterKeyPress(SysKey: Boolean; const Char: TUTF8Char): Boolean; override;
    
    function SetTXNControl(Tag: TXNControlTag; const Data: TXNControlData): Boolean;
  public
    procedure SetAlignment(AAlignment: TAlignment);
    procedure SetColor(const AColor: TColor); override;
    procedure SetFont(const AFont: TFont); override;
    procedure SetPasswordChar(AChar: Char); override;
    procedure SetReadOnly(AReadOnly: Boolean); override;
    procedure SetWordWrap(AWordWrap: Boolean); virtual;
  public
    function GetLineCount: Integer;
    function GetLine(AIndex: Integer): String;
    procedure DeleteLine(AIndex: Integer);
    procedure InsertLine(AIndex: Integer; const S: String);
  public
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars;
  end;

implementation

uses CarbonProc, CarbonDbgConsts, CarbonUtils, CarbonStrings, CarbonWSStdCtrls;

{ TCarbonControlWithEdit }

{------------------------------------------------------------------------------
  Name: CarbonTextField_DidChange
  Handles text change
 ------------------------------------------------------------------------------}
function CarbonTextField_DidChange(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  {$IFDEF VerboseControlEvent}
    DebugLn('CarbonTextField_DidChange: ', DbgSName(AWidget.LCLObject));
  {$ENDIF}

  Result := CallNextEventHandler(ANextHandler, AEvent);

  (AWidget as TCarbonControlWithEdit).TextDidChange;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControlWithEdit.LimitTextLength

  Limits the text length to maximum length
 ------------------------------------------------------------------------------}
procedure TCarbonControlWithEdit.LimitTextLength;
var
  S: String;
  R: Boolean;
  SelStart: Integer;
begin
  if MaxLength > 0 then
  begin
    if GetText(S) then
      if UTF8Length(S) > MaxLength then
      begin
        R := GetSelStart(SelStart);
        S := UTF8Copy(S, 1, MaxLength);
        if SetText(S) then
          if R then SetSelStart(SelStart);
      end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControlWithEdit.AdaptCharCase

  Change text char case
 ------------------------------------------------------------------------------}
procedure TCarbonControlWithEdit.AdaptCharCase;
begin
  // TODO
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControlWithEdit.GetEditPart
  Returns: Control part code of edit control
 ------------------------------------------------------------------------------}
function TCarbonControlWithEdit.GetEditPart: ControlPartCode;
begin
  Result := kControlEntireControl;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControlWithEdit.RegisterEvents

  Registers event handlers for control with edit
 ------------------------------------------------------------------------------}
procedure TCarbonControlWithEdit.RegisterEvents;
var
  TmpSpec: EventTypeSpec;
begin
  inherited RegisterEvents;
  
  if GetEditPart >= 0 then
  begin
    TmpSpec := MakeEventSpec(kEventClassTextField, kEventTextDidChange);
    InstallControlEventHandler(Widget,
      RegisterEventHandler(@CarbonTextField_DidChange),
      1, @TmpSpec, Pointer(Self), nil);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControlWithEdit.TextDidChange

  Text changed event handler
 ------------------------------------------------------------------------------}
procedure TCarbonControlWithEdit.TextDidChange;
var
  Msg: TLMessage;
begin
  // limit the text according to MaxLength
  LimitTextLength;

  // set char case TODO
  AdaptCharCase;

  FillChar(Msg, SizeOf(Msg), 0);
  Msg.Msg := CM_TEXTCHANGED;
  DeliverMessage(LCLObject, Msg);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControlWithEdit.GetPreferredSize
  Returns: The preffered size of control for autosizing or (0, 0)
 ------------------------------------------------------------------------------}
function TCarbonControlWithEdit.GetPreferredSize: TPoint;
begin
  Result := inherited GetPreferredSize;
  Result.X := 0; // don not autosize width of edit like controls
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControlWithEdit.GetSelStart
  Params:  ASelStart - Selection start
  Returns: If the function suceeds

  Gets the selection start from the edit part of control
 ------------------------------------------------------------------------------}
function TCarbonControlWithEdit.GetSelStart(var ASelStart: Integer): Boolean;
var
  SelData: ControlEditTextSelectionRec;
begin
  Result := False;
  ASelStart := 0;
  if GetEditPart < 0 then Exit;
  
  if OSError(
    GetControlData(ControlRef(Widget), GetEditPart, kControlEditTextSelectionTag,
      SizeOf(ControlEditTextSelectionRec), @SelData, nil),
    Self, 'GetSelStart', SGetData) then Exit;

  ASelStart := SelData.SelStart;
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControlWithEdit.GetSelLength
  Params:  ASelLength - Selection length
  Returns: If the function suceeds

  Gets the selection length from the edit part of control
 ------------------------------------------------------------------------------}
function TCarbonControlWithEdit.GetSelLength(var ASelLength: Integer): Boolean;
var
  SelData: ControlEditTextSelectionRec;
begin
  Result := False;
  ASelLength := 0;
  if GetEditPart < 0 then Exit;
  
  if OSError(
    GetControlData(ControlRef(Widget), GetEditPart, kControlEditTextSelectionTag,
    SizeOf(ControlEditTextSelectionRec), @SelData, nil),
    Self, 'GetSelLength', SGetData) then Exit;

  ASelLength := SelData.SelEnd - SelData.SelStart;
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControlWithEdit.SetSelStart
  Params:  ASelStart - Selection start
  Returns: If the function suceeds

  Sets the selection start of the edit part of control
 ------------------------------------------------------------------------------}
function TCarbonControlWithEdit.SetSelStart(ASelStart: Integer): Boolean;
var
  SelData: ControlEditTextSelectionRec;
const
  SName = 'SetSelStart';
begin
  Result := False;
  if GetEditPart < 0 then Exit;
  
  if OSError(
    GetControlData(ControlRef(Widget), GetEditPart, kControlEditTextSelectionTag,
      SizeOf(ControlEditTextSelectionRec), @SelData, nil),
    Self, SName, SGetData) then Exit;

  if SelData.SelStart = ASelStart then
  begin
    Result := True;
    Exit;
  end;

  SelData.SelEnd := (SelData.SelEnd - SelData.SelStart) + ASelStart;
  SelData.SelStart := ASelStart;
  
  if OSError(
    SetControlData(ControlRef(Widget), GetEditPart, kControlEditTextSelectionTag,
      SizeOf(ControlEditTextSelectionRec), @SelData),
    Self, SName, SSetData) then Exit;
      
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControlWithEdit.SetSelLength
  Params:  ASelLength - Selection length
  Returns: If the function suceeds

  Sets the selection length of the edit part of control
 ------------------------------------------------------------------------------}
function TCarbonControlWithEdit.SetSelLength(ASelLength: Integer): Boolean;
var
  SelData: ControlEditTextSelectionRec;
const
  SName = 'SetSelLength';
begin
  Result := False;
  if GetEditPart < 0 then Exit;

  if OSError(
    GetControlData(ControlRef(Widget), GetEditPart, kControlEditTextSelectionTag,
      SizeOf(ControlEditTextSelectionRec), @SelData, nil),
    Self, SName, SGetData) then Exit;

  if SelData.SelEnd = SelData.SelStart + ASelLength then
  begin
    Result := True;
    Exit;
  end;

  SelData.SelEnd := SelData.SelStart + ASelLength;
  
  if OSError(
    SetControlData(ControlRef(Widget), GetEditPart, kControlEditTextSelectionTag,
      SizeOf(ControlEditTextSelectionRec), @SelData),
    Self, SName, SSetData) then Exit;

  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControlWithEdit.GetText
  Params:  S - Text
  Returns: If the function succeeds

  Gets the text of edit control
 ------------------------------------------------------------------------------}
function TCarbonControlWithEdit.GetText(var S: String): Boolean;
var
  CFString: CFStringRef;
begin
  Result := False;
  S := '';
  if GetEditPart < 0 then Exit;
  
  if OSError(
    GetControlData(ControlRef(Widget), GetEditPart, kControlEditTextCFStringTag,
      SizeOf(CFStringRef), @CFString, nil),
    Self, SGetText, SGetData) then Exit;
  try
    S := CFStringToStr(CFString);
    Result := True;
  finally
    FreeCFString(CFString);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControlWithEdit.SetText
  Params:  S - New text
  Returns: If the function succeeds

  Sets the text of edit control
 ------------------------------------------------------------------------------}
function TCarbonControlWithEdit.SetText(const S: String): Boolean;
var
  CFString: CFStringRef;
begin
  Result := False;
  if GetEditPart < 0 then Exit;
  
  CreateCFString(S, CFString);
  try
    if OSError(
      SetControlData(ControlRef(Widget), GetEditPart, kControlEditTextCFStringTag,
        SizeOf(CFStringRef), @CFString),
      Self, SSetText, SSetData) then Exit;
      
    Result := True;
  finally
    FreeCFString(CFString);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControlWithEdit.SetReadOnly
  Params:  AReadOnly - Read only behavior

  Sets the read only behavior of edit control
 ------------------------------------------------------------------------------}
procedure TCarbonControlWithEdit.SetReadOnly(AReadOnly: Boolean);
begin
  if GetEditPart < 0 then Exit;
  
  OSError(SetControlData(ControlRef(Widget), GetEditPart,
      kControlEditTextLockedTag, SizeOf(Boolean), @AReadOnly),
    Self, 'SetReadOnly', SSetData);
end;

{ TCarbonComboBox }

{------------------------------------------------------------------------------
  Name: CarbonComboBox_ListItemSelected
  Handles combo box list item change
 ------------------------------------------------------------------------------}
function CarbonComboBox_ListItemSelected(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  Index: CFIndex;
begin
  {$IFDEF VerboseControlEvent}
    DebugLn('CarbonComboBox_ListItemSelected: ', DbgSName(AWidget.LCLObject));
  {$ENDIF}

  Result := CallNextEventHandler(ANextHandler, AEvent);

  // get selected item index
  if OSError(
    GetEventParameter(AEvent, kEventParamComboBoxListSelectedItemIndex,
      typeCFIndex, nil, SizeOf(CFIndex), nil, @Index),
    'CarbonComboBox_ListItemSelected', SGetEvent,
    'kEventParamComboBoxListSelectedItemIndex') then Index := -1;
  (AWidget as TCarbonComboBox).ListItemSelected(Index);
end;

{------------------------------------------------------------------------------
  Name: CarbonComboBox_MenuOpening
  Handles combo box menu open event
 ------------------------------------------------------------------------------}
function CarbonComboBox_MenuOpening(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  {$IFDEF VerboseControlEvent}
    DebugLn('CarbonComboBox_MenuOpening: ', DbgSName(AWidget.LCLObject));
  {$ENDIF}

  Result := CallNextEventHandler(ANextHandler, AEvent);

  LCLSendDropDownMsg(AWidget.LCLObject);
end;

{------------------------------------------------------------------------------
  Name: CarbonComboBox_MenuClosed
  Handles combo box menu closed event
 ------------------------------------------------------------------------------}
function CarbonComboBox_MenuClosed(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  {$IFDEF VerboseControlEvent}
    DebugLn('CarbonComboBox_MenuClosed: ', DbgSName(AWidget.LCLObject));
  {$ENDIF}

  Result := CallNextEventHandler(ANextHandler, AEvent);

  LCLSendCloseUpMsg(AWidget.LCLObject);
end;

procedure TCarbonComboBox.DropDownTimer(Sender: TObject);
var
  D: Boolean;
begin
  D := IsDroppedDown;
  if D <> FLastDroppedDown then
  begin
    FLastDroppedDown := D;
    if FLastDroppedDown then
      LCLSendDropDownMsg(LCLObject)
    else
      LCLSendCloseUpMsg(LCLObject);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBox.RegisterEvents

  Registers event handlers for combo box
 ------------------------------------------------------------------------------}
procedure TCarbonComboBox.RegisterEvents;
var
  TmpSpec: EventTypeSpec;
begin
  inherited RegisterEvents;
  
  TmpSpec := MakeEventSpec(kEventClassHIComboBox, kEventComboBoxListItemSelected);
  InstallControlEventHandler(Widget,
    RegisterEventHandler(@CarbonComboBox_ListItemSelected),
    1, @TmpSpec, Pointer(Self), nil);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBox.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon combo box
 ------------------------------------------------------------------------------}
procedure TCarbonComboBox.CreateWidget(const AParams: TCreateParams);
var
  CFString: CFStringRef;
  TmpSpec: EventTypeSpec;
begin
  FReadOnly := (LCLObject as TCustomComboBox).ReadOnly;
  FLastDroppedDown := False;
  
  if FReadOnly then
  begin
    if OSError(
      CreatePopupButtonControl(GetTopParentWindow,
        ParamsToCarbonRect(AParams), nil, -12345, False, 0, popupTitleLeftJust,
        Normal, Widget),
      Self, SCreateWidget, 'CreatePopupButtonControl')then RaiseCreateWidgetError(LCLObject);
      
    OSError(CreateNewMenu(0, kMenuAttrAutoDisable, FPopupMenu),
      Self, SCreateWidget, 'CreateNewMenu');
      
    TmpSpec := MakeEventSpec(kEventClassMenu, kEventMenuOpening);
    InstallMenuEventHandler(FPopupMenu,
      RegisterEventHandler(@CarbonComboBox_MenuOpening),
      1, @TmpSpec, Pointer(Self), nil);
      
    TmpSpec := MakeEventSpec(kEventClassMenu, kEventMenuClosed);
    InstallMenuEventHandler(FPopupMenu,
      RegisterEventHandler(@CarbonComboBox_MenuClosed),
      1, @TmpSpec, Pointer(Self), nil);
        
    OSError(
      SetControlData(ControlRef(Widget), kControlEntireControl,
        kControlPopupButtonOwnedMenuRefTag, SizeOf(MenuRef), @FPopupMenu),
      Self, SCreateWidget, SSetData);

    // count of popup button items is initially zero
    SetMaximum(0);
  end
  else
  begin
    CreateCFString(AParams.Caption, CFString);
    try
      if OSError(HIComboBoxCreate(ParamsToHIRect(AParams), CFString, nil, nil,
          kHIComboBoxAutoSizeListAttribute, Widget),
        Self, SCreateWidget, 'HIComboBoxCreate')then RaiseCreateWidgetError(LCLObject);

    finally
      FreeCFString(CFString);
    end;
    
    FreeAndNil(FTimer);
    FTimer := TTimer.Create(nil);
    FTimer.Interval := 200;
    FTimer.OnTimer := @DropDownTimer;
  end;
  
  FItemIndex := -1;
  FMaxLength := 0;
  
  inherited;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBox.DestroyWidget

  Destroys Carbon combo box
 ------------------------------------------------------------------------------}
procedure TCarbonComboBox.DestroyWidget;
begin
  if FReadOnly then DisposeMenu(FPopupMenu);
  if FTimer.Enabled then DropDownTimer(nil);
  FTimer.Free;

  inherited DestroyWidget;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBox.GetEditPart
  Returns: Control part code of edit control
 ------------------------------------------------------------------------------}
function TCarbonComboBox.GetEditPart: ControlPartCode;
begin
  if FReadOnly then Result := -1
  else Result := kHIComboBoxEditTextPart;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBox.GetPopupButtonMenu
  Returns: Popup Button menu
 ------------------------------------------------------------------------------}
function TCarbonComboBox.GetPopupButtonMenu: MenuRef;
begin
  Result := FPopupMenu;

end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBox.GetValidEvents
  Returns: Set of events with installed handlers
 ------------------------------------------------------------------------------}
class function TCarbonComboBox.GetValidEvents: TCarbonControlEvents;
begin
  Result := [cceValueChanged];
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBox.ListItemSelected
  Params:  AIndex - Index of selected item

  List item selected event handler
 ------------------------------------------------------------------------------}
procedure TCarbonComboBox.ListItemSelected(AIndex: Integer);
begin
  if FItemIndex <> AIndex then
  begin
    FItemIndex := AIndex;
    LCLSendSelectionChangedMsg(LCLObject);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBox.ValueChanged

  Value changed event handler
 ------------------------------------------------------------------------------}
procedure TCarbonComboBox.ValueChanged;
begin
  if FReadOnly then ListItemSelected(GetValue - 1);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBox.FocusSet

  Handles set focus
 ------------------------------------------------------------------------------}
procedure TCarbonComboBox.FocusSet;
begin
  inherited;
  // emulate DropDown event here
  //LCLSendDropDownMsg(LCLObject);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBox.FocusKilled

  Handles kill focus
 ------------------------------------------------------------------------------}
procedure TCarbonComboBox.FocusKilled;
begin
  inherited;
  // emulate CloseUp event here
  //LCLSendCloseUpMsg(LCLObject);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBox.TextDidChange

  Text changed event handler
 ------------------------------------------------------------------------------}
procedure TCarbonComboBox.TextDidChange;
begin
  inherited TextDidChange;
  
  // TComboBox needs LM_CHANGED message type
  SendSimpleMessage(LCLObject, LM_CHANGED);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBox.GetText
  Params:  S - Text
  Returns: If the function succeeds

  Gets the text of combo box
 ------------------------------------------------------------------------------}
function TCarbonComboBox.GetText(var S: String): Boolean;
var
  ComboBox: TCustomComboBox;
begin
  if FReadOnly then
  begin
    ComboBox := LCLObject as TCustomComboBox;
    if (FItemIndex >= 0) and (FItemIndex < ComboBox.Items.Count) then
      S := ComboBox.Items[FItemIndex]
    else
      S := '';
      
    Result := True;
  end
  else
    Result := inherited GetText(S);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBox.SetBounds
  Params:  ARect - Record for control coordinates
  Returns: If function succeeds

  Sets the control bounding rectangle relative to the client origin of its
  parent
 ------------------------------------------------------------------------------}
function TCarbonComboBox.SetBounds(const ARect: TRect): Boolean;
var
  R: TRect;
begin
  R := ARect;
  R.Bottom := R.Top + GetPreferredSize.Y;
  Result := inherited SetBounds(R);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBox.SetReadOnly
  Params:  AReadOnly - Read only behavior

  Sets the read only behavior of combo box
 ------------------------------------------------------------------------------}
procedure TCarbonComboBox.SetReadOnly(AReadOnly: Boolean);
begin
  if AReadOnly <> FReadOnly then RecreateWnd(LCLObject);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBox.GetItemIndex
  Returns: The current item selected index
 ------------------------------------------------------------------------------}
function TCarbonComboBox.GetItemIndex: Integer;
begin
  Result := FItemIndex;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBox.SetItemIndex
  Params:  AIndex - New item index
  
  Changes currently selected item
 ------------------------------------------------------------------------------}
function TCarbonComboBox.SetItemIndex(AIndex: Integer): Boolean;
begin
  Result := False;
  //DebugLn('TCarbonComboBox.SetItemIndex New: ' + DbgS(AIndex) + ' Prev: ' + DbgS(FItemIndex));
  if AIndex <> FItemIndex then
  begin
    if FReadOnly then SetValue(AIndex + 1)
    else
    begin
      if AIndex = -1 then
      begin
        FItemIndex := -1;
        Result := SetText('');
      end
      else
      begin
        FItemIndex := AIndex;
        Result := SetText((LCLObject as TCustomComboBox).Items[AIndex]);
      end;
      end;
  end
  else Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBox.Insert
  Params:  AIndex - Item index
           S      - Item text

  Inserts item with the specified text at index
 ------------------------------------------------------------------------------}
procedure TCarbonComboBox.Insert(AIndex: Integer; const S: String);
var
  CFString: CFStringRef;
begin
  CreateCFString(S, CFString);
  try
    if FReadOnly then
    begin
      OSError(InsertMenuItemTextWithCFString(GetPopupButtonMenu, CFString, AIndex,
          kMenuItemAttrIgnoreMeta, 0),
        Self, 'Inset', 'InsertMenuItemTextWithCFString');
        
      SetMaximum((LCLObject as TCustomComboBox).Items.Count);
    end
    else
    begin
      OSError(HIComboBoxInsertTextItemAtIndex(HIViewRef(Widget), AIndex, CFString),
        Self, 'Insert', 'HIComboBoxInsertTextItemAtIndex');
     end;
  finally
    FreeCFString(CFString);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBox.Remove
  Params:  AIndex - Item index

  Removes item with the specified index
 ------------------------------------------------------------------------------}
procedure TCarbonComboBox.Remove(AIndex: Integer);
begin
  if FReadOnly then
  begin
    DeleteMenuItem(GetPopupButtonMenu, AIndex + 1);
    SetMaximum((LCLObject as TCustomComboBox).Items.Count);
  end
  else
  begin
    OSError(HIComboBoxRemoveItemAtIndex(HIViewRef(Widget), AIndex),
      Self, 'Remove', 'HIComboBoxRemoveItemAtIndex');
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBox.DropDown
  Params:  ADropDown - Drop down
  Returns: If the function succeeds

  Shows or hides drop down list
 ------------------------------------------------------------------------------}
function TCarbonComboBox.DropDown(ADropDown: Boolean): Boolean;
begin
  Result := False;
  if FReadOnly then
  begin
    //P := LCLObject.ClientToScreen(Classes.Point(0, 0));
    //PopUpMenuSelect(FPopupMenu, P.Y, P.X, FItemIndex + 1);
    DebugLn('TCarbonComboBox.DropDown for DropDownList TODO');
    Exit;
  end;
  
  if OSError(HIComboBoxSetListVisible(ControlRef(Widget), ADropDown), Self,
    'DropDown', 'HIComboBoxSetListVisible') then Exit;

  Result := True;
end;

function TCarbonComboBox.IsDroppedDown: Boolean;
begin
  Result := HIComboBoxIsListVisible(ControlRef(Widget));
end;

{ TCarbonCustomEdit }

{------------------------------------------------------------------------------
  Method:  TCarbonCustomEdit.FilterKeyPress

  Filters key presses from being send to Carbon control
 ------------------------------------------------------------------------------}
function TCarbonCustomEdit.FilterKeyPress(SysKey: Boolean; const Char: TUTF8Char): Boolean;
begin
  Result := (Char = #13);
end;

{ TCarbonSpinEdit }

{------------------------------------------------------------------------------
  Method:  TCarbonSpinEdit.UpDownThemeWidth
  Returns: UpDown theme width
 ------------------------------------------------------------------------------}
function TCarbonSpinEdit.UpDownThemeWidth: Integer;
begin
  Result := GetCarbonThemeMetric(kThemeMetricLittleArrowsWidth, 13);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonSpinEdit.FocusRectThemeOutset
  Returns: Focus rectangle theme outset
 ------------------------------------------------------------------------------}
function TCarbonSpinEdit.FocusRectThemeOutset: Integer;
begin
  Result := GetCarbonThemeMetric(kThemeMetricFocusRectOutset, 4);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonSpinEdit.GetEditBounds
  Params:  ARect - Bounding rect
  Returns: Bounding rect for edit
 ------------------------------------------------------------------------------}
function TCarbonSpinEdit.GetEditBounds(const ARect: HIRect): HIRect;
var
  H: Single;
begin
  if LCLObject.AutoSize then
  begin // apply edit preffered height if autosize
    H := GetPreferredSize.y;
    if H = 0 then H := ARect.size.height;
  end
  else
    H := ARect.size.height;
    
  Result.origin.x := ARect.origin.x;
  Result.origin.y := ARect.origin.y + (ARect.size.height - H) / 2;
  Result.size.width := ARect.size.width - (UpDownThemeWidth + 2 * FocusRectThemeOutset);
  Result.size.height := H;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonSpinEdit.GetUpDownBounds
  Params:  ARect - Bounding rect
  Returns: Bounding rect for updown
 ------------------------------------------------------------------------------}
function TCarbonSpinEdit.GetUpDownBounds(const ARect: HIRect): HIRect;
begin
  Result.origin.x := ARect.origin.x + ARect.size.width - (UpDownThemeWidth);
  Result.origin.y := ARect.origin.y;
  Result.size.width := UpDownThemeWidth;
  Result.size.height := ARect.size.height;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonSpinEdit.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon spin edit
 ------------------------------------------------------------------------------}
procedure TCarbonSpinEdit.CreateWidget(const AParams: TCreateParams);
var
  CFString: CFStringRef;
const
  SingleLine: Boolean = True;
begin
  CreateCFString(AParams.Caption, CFString);
  try
    if OSError(
      CreateEditUniCodeTextControl(GetTopParentWindow,
        HIRectToCarbonRect(GetEditBounds(ParamsToHIRect(AParams))),
        CFString, False, nil, Widget),
      Self, SCreateWidget, 'CreateEditUniCodeTextControl') then RaiseCreateWidgetError(LCLObject);
  finally
    FreeCFString(CFString);
  end;
  
  // set edit single line
  OSError(
    SetControlData(Widget, kControlEntireControl, kControlEditTextSingleLineTag,
      SizeOf(Boolean), @SingleLine),
    Self, SCreateWidget, SSetData);
  
  if OSError(
      CreateLittleArrowsControl(GetTopParentWindow,
        HIRectToCarbonRect(GetUpDownBounds(ParamsToHIRect(AParams))),
        1, 0, 2, 1, FUpDown),
      Self, SCreateWidget, 'CreateLittleArrowsControl') then RaiseCreateWidgetError(LCLObject);
    
  AddControlPart(FUpDown);
    
  inherited;
  
  UpdateControl;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonSpinEdit.DestroyWidget

  Destroys Carbon spin edit
 ------------------------------------------------------------------------------}
procedure TCarbonSpinEdit.DestroyWidget;
begin
  DisposeControl(FUpDown);

  inherited;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonSpinEdit.GetFrame
  Params:  Frame index
  Returns: Frame area control
 ------------------------------------------------------------------------------}
function TCarbonSpinEdit.GetFrame(Index: Integer): ControlRef;
begin
  case Index of
  0: Result := ControlRef(Widget);
  1: Result := FUpDown;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonSpinEdit.GetFrameBounds
  Params:  ARect - Rectangle
  Returns: If function succeeds

  Returns the control bounding rectangle relative to the client origin of its
  parent
 ------------------------------------------------------------------------------}
function TCarbonSpinEdit.GetFrameBounds(var ARect: TRect): Boolean;
begin
  Result := False;

  if inherited GetFrameBounds(ARect) then
  begin
    // add updown width
    ARect.Right := ARect.Right + (UpDownThemeWidth + 2 * FocusRectThemeOutset);
    
    Result := True;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonSpinEdit.GetFrameCount
  Returns: Count of control frames
 ------------------------------------------------------------------------------}
class function TCarbonSpinEdit.GetFrameCount: Integer;
begin
  Result := 2;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonSpinEdit.GetValidEvents
  Returns: Set of events with installed handlers
 ------------------------------------------------------------------------------}
class function TCarbonSpinEdit.GetValidEvents: TCarbonControlEvents;
begin
  Result := inherited GetValidEvents + [cceDoAction];
end;

{------------------------------------------------------------------------------
  Method:  TCarbonSpinEdit.TextDidChange

  Text changed event handler
 ------------------------------------------------------------------------------}
procedure TCarbonSpinEdit.TextDidChange;
begin
  FValue := (LCLObject as TCustomFloatSpinEdit).StrToValue(
    (LCLObject as TCustomFloatSpinEdit).Text);

  inherited TextDidChange;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonSpinEdit.DoAction
  Params:  AControlPart - Control part to perform the action

  Action event handler
 ------------------------------------------------------------------------------}
procedure TCarbonSpinEdit.DoAction(AControlPart: ControlPartCode);
begin
  case AControlPart of
    kControlUpButtonPart:   FValue := FValue + FIncrement;
    kControlDownButtonPart: FValue := FValue - FIncrement;
  end;
  
  FValue := (LCLObject as TCustomFloatSpinEdit).GetLimitedValue(FValue);
  
  inherited SetText((LCLObject as TCustomFloatSpinEdit).ValueToStr(FValue));
  inherited TextDidChange;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonSpinEdit.SetBounds
  Params:  ARect - Record for control coordinates
  Returns: If function succeeds

  Sets the control bounding rectangle relative to the client origin of its
  parent
 ------------------------------------------------------------------------------}
function TCarbonSpinEdit.SetBounds(const ARect: TRect): Boolean;
begin
  Result := False;

  if OSError(
    HIViewSetFrame(Widget, GetEditBounds(RectToCGRect(ARect))),
    Self, SSetBounds, SViewFrame) then Exit;
    
  if OSError(HIViewSetFrame(FUpDown, GetUpDownBounds(RectToCGRect(ARect))),
    Self, SSetBounds, SViewFrame) then Exit;
    
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonSpinEdit.SetPasswordChar
  Params:  AChar     - New password char

  Sets the new password char of Carbon edit
 ------------------------------------------------------------------------------}
procedure TCarbonSpinEdit.SetPasswordChar(AChar: Char);
begin
  // not supported
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControlWithEdit.SetText
  Params:  S - New text
  Returns: If the function succeeds

  Sets the text of edit control
 ------------------------------------------------------------------------------}
function TCarbonSpinEdit.SetText(const S: String): Boolean;
begin
  FValue := (LCLObject as TCustomFloatSpinEdit).StrToValue(S);
  
  Result := inherited SetText(S);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonSpinEdit.UpdateControl

  Updates the value, min, max and increment of Carbon spin edit
 ------------------------------------------------------------------------------}
procedure TCarbonSpinEdit.UpdateControl;
var
  SpinEdit: TCustomFloatSpinEdit;
begin
  SpinEdit := (LCLObject as TCustomFloatSpinEdit);
  FValue := SpinEdit.Value;
  FMin := SpinEdit.MinValue;
  FMax := SpinEdit.MaxValue;
  FIncrement := SpinEdit.Increment;
  FDecimalPlaces :=  SpinEdit.DecimalPlaces;
    
  // update edit text
  inherited SetText((LCLObject as TCustomFloatSpinEdit).ValueToStr(FValue));
end;

{ TCarbonEdit }

{------------------------------------------------------------------------------
  Method:  TCarbonEdit.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon edit
 ------------------------------------------------------------------------------}
procedure TCarbonEdit.CreateWidget(const AParams: TCreateParams);
var
  Edit: TCustomEdit;
  Control: ControlRef;
  CFString: CFStringRef;
  SingleLine: Boolean = True;
begin
  Edit := LCLObject as TCustomEdit;

  CreateCFString(AParams.Caption, CFString);
  try
    if OSError(
      CreateEditUniCodeTextControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
        CFString, (Edit.PasswordChar <> #0), nil, Control),
      Self, SCreateWidget, 'CreateEditUniCodeTextControl') then RaiseCreateWidgetError(LCLObject);

    Widget := Control;
    
    inherited;
  finally
    FreeCFString(CFString);
  end;

  // set edit single line
  OSError(
    SetControlData(Control, kControlEntireControl, kControlEditTextSingleLineTag,
      SizeOf(Boolean), @SingleLine),
    Self, SCreateWidget, SSetData);
    
  FIsPassword := Edit.PasswordChar <> #0;
  FMaxLength := Edit.MaxLength;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonEdit.GetText
  Params:  S - Text
  Returns: If the function succeeds

  Gets the text of edit control
 ------------------------------------------------------------------------------}
function TCarbonEdit.GetText(var S: String): Boolean;
var
  CFString: CFStringRef;
begin
  if not FIsPassword then
    Result := inherited GetText(S)
  else
  begin
    Result := False;

    if OSError(
      GetControlData(ControlRef(Widget), GetEditPart,
        kControlEditTextPasswordCFStringTag, SizeOf(CFStringRef), @CFString, nil),
      Self, SGetText, SGetData) then Exit;

    try
      S := CFStringToStr(CFString);
      Result := True;
    finally
      FreeCFString(CFString);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonEdit.SetText
  Params:  S - New text
  Returns: If the function succeeds

  Sets the text of edit control
 ------------------------------------------------------------------------------}
function TCarbonEdit.SetText(const S: String): Boolean;
var
  CFString: CFStringRef;
begin
  if not FIsPassword then
    Result := inherited SetText(S)
  else
  begin
    Result := False;
    
    CreateCFString(S, CFString);
    try
      if OSError(
        SetControlData(ControlRef(Widget), GetEditPart,
          kControlEditTextPasswordCFStringTag, SizeOf(CFStringRef), @CFString),
        Self, SSetText, SSetData) then Exit;
        
      Result := True;
    finally
      FreeCFString(CFString);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonEdit.SetPasswordChar
  Params:  AChar     - New password char

  Sets the new password char of Carbon edit
 ------------------------------------------------------------------------------}
procedure TCarbonEdit.SetPasswordChar(AChar: Char);
begin
  if FIsPassword <> (AChar <> #0) then RecreateWnd(LCLObject);
end;

{ TCarbonMemo }

{------------------------------------------------------------------------------
  Method:  TCarbonMemo.SetScrollBars
  Params:  AValue - New scroll style

  Sets the memo scrollbars
 ------------------------------------------------------------------------------}
procedure TCarbonMemo.SetScrollBars(const AValue: TScrollStyle);
begin
  ChangeScrollBars(FScrollView, FScrollBars, AValue);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemo.GetFrame
  Params:  Frame index
  Returns: Frame area control
 ------------------------------------------------------------------------------}
function TCarbonMemo.GetFrame(Index: Integer): ControlRef;
begin
  Result := FScrollView;
end;


{------------------------------------------------------------------------------
  Method:  TCarbonMemo.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon memo
 ------------------------------------------------------------------------------}
procedure TCarbonMemo.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  Options: OptionBits;
  R: HIRect;
begin
  Options := kTXNMonostyledTextMask or kOutputTextInUnicodeEncodingMask;

  R := ParamsToHIRect(AParams);
  if OSError(HITextViewCreate(@R, 0, Options, Control),
    Self, SCreateWidget, 'HITextViewCreate') then RaiseCreateWidgetError(LCLObject);

  Widget := Control;
  
  // force embed in scroll view because HITextView is not scrolling into
  // caret position when not embedded

  FScrollBars := (LCLObject as TCustomMemo).ScrollBars;
  FScrollView := EmbedInScrollView(FScrollBars);

  inherited;

  FMaxLength := 0;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemo.DestroyWidget

  Destroys Carbon memo
 ------------------------------------------------------------------------------}
procedure TCarbonMemo.DestroyWidget;
begin
  inherited DestroyWidget;
  
  DisposeControl(FScrollView);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemo.GetLineOffset
  Returns: Offset of specified line
 ------------------------------------------------------------------------------}
procedure TCarbonMemo.GetLineOffset(AIndex: Integer; out AStart, AEnd: TXNOffset);
const
  SName = 'GetLineOffset';
var
  O: TXNObject;
  W, H: Fixed;
  P: HIPoint;
  Line, TextStart, TextEnd: TXNOffset;
  LineTop, LineBottom: Single;
begin
  AStart := 0;
  AEnd := 0;
  if AIndex >= GetLineCount then
  begin
    AStart := kTXNEndOffset;
    AEnd := kTXNEndOffset;
    Exit;
  end;
  O := HITextViewGetTXNObject(ControlRef(Widget));
  if TXNDataSize(O) = 0 then Exit;

  if OSError(TXNGetLineMetrics(O, AIndex, W, H), Self, SName, 'TXNGetLineMetrics') then Exit;
  if OSError(TXNOffsetToHIPoint(O, 0, P), Self, SName, 'TXNOffsetToHIPoint') then Exit;
  LineTop := P.y + AIndex * (H / $10000);
  LineBottom := LineTop + H / $10000;

  // find line offset with bisection
  TextStart := 0;
  TextEnd := TXNDataSize(O) div 2;
  repeat
    Line := (TextStart + TextEnd) div 2;
    if OSError(TXNOffsetToHIPoint(O, Line, P), Self, SName, 'TXNOffsetToHIPoint') then Exit;
    if P.y < LineTop then
      TextStart := Line + 1
    else
      TextEnd := Line;
      
    if (P.y >= LineTop) and (P.y < LineBottom) then Break;
  until TextEnd < TextStart;

  LineTop := P.y;

  // find line start offset
  AStart := Line;
  while AStart > 0 do
  begin
    if OSError(TXNOffsetToHIPoint(O, AStart - 1, P), Self, SName, 'TXNOffsetToHIPoint') then Exit;
    if P.y <> LineTop then Break;
    Dec(AStart);
  end;
  
  // find line end offset
  AEnd := Line;
  TextEnd := TXNDataSize(O) div 2;
  while AEnd < TextEnd do
  begin
    if OSError(TXNOffsetToHIPoint(O, AEnd + 1, P), Self, SName, 'TXNOffsetToHIPoint') then Exit;
    if P.y <> LineTop then Break;
    Inc(AEnd);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemo.TextDidChange

  Text changed event handler
 ------------------------------------------------------------------------------}
procedure TCarbonMemo.TextDidChange;
var
  Msg: TLMessage;
begin
  // limit the text according to MaxLength
  LimitTextLength;

  AdaptCharCase;

  FillChar(Msg, SizeOf(Msg), 0);
  Msg.Msg := CM_TEXTCHANGED;
  DeliverMessage(LCLObject, Msg);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemo.SetTXNControl
  Params:  Tag  - Tag
           Data - Tag data

  Sets the control data of TXN object in Carbon interface
 ------------------------------------------------------------------------------}
function TCarbonMemo.SetTXNControl(Tag: TXNControlTag;
  const Data: TXNControlData): Boolean;
begin
  Result := False;
  
  if OSError(TXNSetTXNObjectControls(HITextViewGetTXNObject(ControlRef(Widget)),
         False, 1, @Tag, @Data),
      Self, 'SetTXNControl', SSetTXNControls) then Exit;
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemo.SetAlignment
  Params:  AAlignment  - New alignment

  Sets the alignment of memo in Carbon interface
 ------------------------------------------------------------------------------}
procedure TCarbonMemo.SetAlignment(AAlignment: TAlignment);
var
  Data: TXNControlData;
begin
  case AAlignment of
    taLeftJustify:  Data.uValue := UInt32(kTXNFlushLeft);
    taRightJustify: Data.uValue := UInt32(kTXNFlushRight);
    taCenter:       Data.uValue := UInt32(kTXNCenter);
  end;
  SetTXNControl(kTXNJustificationTag, Data);
  
  Invalidate;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemo.SetColor
  Params:  AColor - New color

  Sets the color of memo
 ------------------------------------------------------------------------------}
procedure TCarbonMemo.SetColor(const AColor: TColor);
var
  CGColor: CGColorRef;
begin
  CGColor := CreateCGColor(AColor);
  try
    OSError(HITextViewSetBackgroundColor(HIViewRef(Widget), CGColor),
      Self, SSetColor, 'HITextViewSetBackgroundColor');
  finally
    CGColorRelease(CGColor);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemo.SetFont
  Params:  AFont - New font

  Sets the font of memo
 ------------------------------------------------------------------------------}
procedure TCarbonMemo.SetFont(const AFont: TFont);
var
  Attrs: Array [0..3] of TXNTypeAttributes;
  FontColor: RGBColor;
begin
 // font name
 Attrs[0].tag := kATSUFontTag;
 Attrs[0].size := SizeOf(ATSUFontID);
 Attrs[0].data.dataValue := FindCarbonFontID(AFont.Name);

 // font color
 FontColor := ColorToRGBColor(AFont.Color);
 Attrs[1].tag := kTXNQDFontColorAttribute;
 Attrs[1].size := kTXNQDFontColorAttributeSize;
 Attrs[1].data.dataPtr := @FontColor;

 // font size
 Attrs[2].tag := kTXNQDFontSizeAttribute;
 Attrs[2].size := kTXNQDFontSizeAttributeSize;
 Attrs[2].data.dataValue := AFont.Size;

 TCarbonFont(AFont.Reference.Handle).SetColor(AFont.Color);

 // font style
 Attrs[3].tag := kTXNATSUIStyle;
 Attrs[3].size := kTXNATSUIStyleSize;
 Attrs[3].data.dataPtr := Pointer(TCarbonFont(AFont.Reference.Handle).Style);

 // apply
 OSError(
   TXNSetTypeAttributes(HITextViewGetTXNObject(ControlRef(Widget)), 4, @Attrs[0],
     kTXNStartOffset, kTXNEndOffset),
   Self, 'SetFont', 'TXNSetTypeAttributes');

  // invalidate control
  Invalidate;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemo.SetPasswordChar
  Params:  AChar     - New password char

  Sets the new password char of Carbon memo
 ------------------------------------------------------------------------------}
procedure TCarbonMemo.SetPasswordChar(AChar: Char);
begin
  OSError(
    TXNEchoMode(HITextViewGetTXNObject(ControlRef(Widget)),
      UniChar(AChar), CreateTextEncoding(kTextEncodingUnicodeDefault,
      kUnicodeNoSubset, kUnicodeUTF8Format), AChar <> #0),
    Self, 'SetPasswordChar', 'TXNEchoMode');

  Invalidate;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemo.SetReadOnly
  Params:  AReadOnly - Read only behavior

  Sets the read only behavior of Carbon memo
 ------------------------------------------------------------------------------}
procedure TCarbonMemo.SetReadOnly(AReadOnly: Boolean);
var
  Data: TXNControlData;
begin
  if AReadOnly then
    Data.uValue := UInt32(kTXNReadOnly)
  else
    Data.uValue := UInt32(kTXNReadWrite);
    
  SetTXNControl(kTXNNoUserIOTag, Data);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemo.SetWordWrap
  Params:  AWordWrap - New word wrap

  Sets the word wrap of Carbon memo
 ------------------------------------------------------------------------------}
procedure TCarbonMemo.SetWordWrap(AWordWrap: Boolean);
var
  Data: TXNControlData;
begin
  if AWordWrap then
    Data.uValue := UInt32(kTXNAutoWrap)
  else
    Data.uValue := UInt32(kTXNNoAutoWrap);

  SetTXNControl(kTXNWordWrapStateTag, Data);

  Invalidate;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemo.GetLineCount
  Returns: Memo line count
 ------------------------------------------------------------------------------}
function TCarbonMemo.GetLineCount: Integer;
var
  C: ItemCount;
  S: Integer;
  O: TXNObject;
begin
  Result := 0;
  O := HITextViewGetTXNObject(ControlRef(Widget));
  if not OSError(TXNGetLineCount(O, C),
      Self, 'GetLineCount', 'TXNGetLineCount') then
  begin
    Result := C;
    S := TXNDataSize(O);
    if S = 0 then Dec(Result);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemo.GetLine
  Returns: Memo line text
 ------------------------------------------------------------------------------}
function TCarbonMemo.GetLine(AIndex: Integer): String;
var
  AStart, AEnd: TXNOffset;
  Data: Handle;
  W: WideString;
begin
  Result := '';

  GetLineOffset(AIndex, AStart, AEnd);
  if OSError(TXNGetData(HITextViewGetTXNObject(ControlRef(Widget)), AStart, AEnd, Data), Self, 'GetLine', 'TXNGetData') then Exit;
  
  W := PWideChar(Data^);

  Result := UTF16ToUTF8(Copy(W, 0, GetHandleSize(Data) div 2));
  // remove CRLF
  if (Result <> '') and (Result[Length(Result)] in [#10, #13]) then
    Delete(Result, Length(Result), 1);
  if (Result <> '') and (Result[Length(Result)] in [#10, #13]) then
    Delete(Result, Length(Result), 1);

  DisposeHandle(Data);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemo.DeleteLine
 ------------------------------------------------------------------------------}
procedure TCarbonMemo.DeleteLine(AIndex: Integer);
var
  AStart, AEnd: TXNOffset;
begin
  GetLineOffset(AIndex, AStart, AEnd);
  if (AIndex > 0) and (AIndex = GetLineCount - 1) then Dec(AStart);
  
  OSError(TXNSetData(HITextViewGetTXNObject(ControlRef(Widget)), kTXNUnicodeTextData, nil, 0, AStart, AEnd),
    Self, 'DeleteLine', 'TXNSetData');
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemo.InsertLine
 ------------------------------------------------------------------------------}
procedure TCarbonMemo.InsertLine(AIndex: Integer; const S: String);
var
  AStart, AEnd: TXNOffset;
  W: WideString;
begin
  if AIndex < 0 then AIndex := 0;
  W := UTF8ToUTF16(S);
  
  if GetLineCount = 0 then
    AStart := 0
  else
  if AIndex < GetLineCount then
  begin
    GetLineOffset(AIndex, AStart, AEnd);
    W := W + #10;
  end
  else
  begin
    GetLineOffset(GetLineCount - 1, AStart, AEnd);
    W := #10 + W;
    AStart := AEnd;
  end;

  OSError(TXNSetData(HITextViewGetTXNObject(ControlRef(Widget)), kTXNUnicodeTextData, @W[1], Length(W) * 2, AStart, AStart),
    Self, 'InsertLine', 'TXNSetData');
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemo.FilterKeyPress

  Filters key presses from being send to Carbon control
 ------------------------------------------------------------------------------}
function TCarbonMemo.FilterKeyPress(SysKey: Boolean; const Char: TUTF8Char): Boolean;
begin
  Result := False;
end;



end.

