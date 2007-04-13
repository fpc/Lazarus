{ $Id$
                  --------------------------------------------
                  carbonedits.pp  -  Carbon edit-like controls
                  --------------------------------------------

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
unit CarbonEdits;

{$mode objfpc}{$H+}

interface

// debugging defines
{$I carbondebug.inc}

uses
 // rtl+ftl
  Types, Classes, SysUtils, Math, Contnrs,
 // carbon bindings
  FPCMacOSAll,
 // LCL
  LMessages, LCLMessageGlue, LCLProc, LCLType, Graphics, Controls, StdCtrls,
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
    class function GetEditPart: ControlPartCode; virtual;
  public
    class function GetValidEvents: TCarbonControlEvents; override;
    procedure TextDidChange; override;
  public
    function GetSelStart(var ASelStart: Integer): Boolean;
    function GetSelLength(var ASelLength: Integer): Boolean;
    function SetSelStart(ASelStart: Integer): Boolean;
    function SetSelLength(ASelLength: Integer): Boolean;

    function GetText(var S: String): Boolean; override;
    function SetText(const S: String): Boolean; override;
  public
    property MaxLength: Integer read FMaxLength write FMaxLength;
  end;

  { TCarbonComboBox }

  TCarbonComboBox = class(TCarbonControlWithEdit)
  private
    FItemIndex: Integer;
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
    class function GetEditPart: ControlPartCode; override;
  public
    class function GetValidEvents: TCarbonControlEvents; override;
    procedure ListItemSelected(AIndex: Integer); override;
  public
    function GetItemIndex: Integer;
    function SetItemIndex(AIndex: Integer): Boolean;
  end;

  { TCarbonEdit }

  TCarbonEdit = class(TCarbonControlWithEdit)
  private
    FIsPassword: Boolean;
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    function GetText(var S: String): Boolean; override;
    function SetText(const S: String): Boolean; override;
  public
    property IsPassword: Boolean read FIsPassword;
  end;

  { TCarbonMemo }

  TCarbonMemo = class(TCarbonControlWithEdit)
  private
    FScrollView: HIViewRef;
    FScrollBars: TScrollStyle;
    procedure SetScrollBars(const AValue: TScrollStyle);
  protected
    function GetFrame: ControlRef; override;
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
  public
    procedure TextDidChange; override;
  public
    procedure SetColor(const AColor: TColor); override;
    procedure SetFont(const AFont: TFont); override;
  public
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars;
  end;

implementation

uses CarbonProc, CarbonConsts, CarbonUtils, CarbonStrings, CarbonWSStdCtrls;

{ TCarbonControlWithEdit }

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

procedure TCarbonControlWithEdit.AdaptCharCase;
begin
  // TODO
end;

class function TCarbonControlWithEdit.GetEditPart: ControlPartCode;
begin
  Result := kControlEntireControl;
end;

class function TCarbonControlWithEdit.GetValidEvents: TCarbonControlEvents;
begin
  Result := [cceTextDidChange];
end;

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
  Method:  TCarbonControlWithEdit.GetSelStart
  Params:  ASelStart - Selection start
  Returns: If the function suceeds

  Gets the selection start from the edit part of control
 ------------------------------------------------------------------------------}
function TCarbonControlWithEdit.GetSelStart(var ASelStart: Integer): Boolean;
var
  SelData: ControlEditTextSelectionRec;
begin
  Result := GetControlData(ControlRef(Widget), GetEditPart,
    kControlEditTextSelectionTag, SizeOf(ControlEditTextSelectionRec),
    @SelData, nil) = noErr;

  if Result then ASelStart := SelData.SelStart;
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
  Result := GetControlData(ControlRef(Widget), GetEditPart,
    kControlEditTextSelectionTag, SizeOf(ControlEditTextSelectionRec),
    @SelData, nil) = noErr;

  if Result then ASelLength := SelData.SelEnd - SelData.SelStart;
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
begin
  Result := GetControlData(ControlRef(Widget), GetEditPart,
    kControlEditTextSelectionTag, SizeOf(ControlEditTextSelectionRec),
    @SelData, nil) = noErr;

  if Result then
  begin
    if SelData.SelStart = ASelStart then Exit;

    SelData.SelEnd := (SelData.SelEnd - SelData.SelStart) + ASelStart;
    SelData.SelStart := ASelStart;
    Result := SetControlData(ControlRef(Widget), GetEditPart,
      kControlEditTextSelectionTag, SizeOf(ControlEditTextSelectionRec),
      @SelData) = noErr;
  end;
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
begin
  Result := GetControlData(ControlRef(Widget), GetEditPart,
    kControlEditTextSelectionTag, SizeOf(ControlEditTextSelectionRec),
    @SelData, nil) = noErr;

  if Result then
  begin
    if SelData.SelEnd = SelData.SelStart + ASelLength then Exit;

    SelData.SelEnd := SelData.SelStart + ASelLength;
    Result := SetControlData(ControlRef(Widget), GetEditPart,
      kControlEditTextSelectionTag, SizeOf(ControlEditTextSelectionRec),
      @SelData) = noErr;
  end;
end;

function TCarbonControlWithEdit.GetText(var S: String): Boolean;
var
  CFString: CFStringRef;
begin
  Result := False;
  if GetControlData(ControlRef(Widget), GetEditPart, kControlEditTextCFStringTag,
    SizeOf(CFStringRef), @CFString, nil) <> noErr then Exit;
  try
    S := CFStringToStr(CFString);
    Result := True;
  finally
    FreeCFString(CFString);
  end;
end;

function TCarbonControlWithEdit.SetText(const S: String): Boolean;
var
  CFString: CFStringRef;
begin
  Result := False;
  CreateCFString(S, CFString);
  try
    Result := SetControlData(ControlRef(Widget), GetEditPart,
      kControlEditTextCFStringTag, SizeOf(CFStringRef), @CFString) = noErr;
  finally
    FreeCFString(CFString);
  end;
end;

{ TCarbonComboBox }

procedure TCarbonComboBox.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  CFString: CFStringRef;
begin
  CreateCFString(AParams.Caption, CFString);
  try
    if HIComboBoxCreate(ParamsToHIRect(AParams), CFString, nil, nil,
      kHIComboBoxAutoSizeListAttribute, Control) = noErr then
    begin
      Widget := Control;

      inherited;
    end
    else RaiseCreateWidgetError(LCLObject);
  finally
    FreeCFString(CFString);
  end;
  
  FItemIndex := -1;
  FMaxLength := 0;
end;

class function TCarbonComboBox.GetEditPart: ControlPartCode;
begin
  Result := kHIComboBoxEditTextPart;
end;

class function TCarbonComboBox.GetValidEvents: TCarbonControlEvents;
begin
  Result := [cceTextDidChange, cceListItemSelected];
end;

procedure TCarbonComboBox.ListItemSelected(AIndex: Integer);
begin
  FItemIndex := AIndex;
  LCLSendSelectionChangedMsg(LCLObject);
end;

function TCarbonComboBox.GetItemIndex: Integer;
begin
  Result := FItemIndex;
end;

function TCarbonComboBox.SetItemIndex(AIndex: Integer): Boolean;
begin
  Result := False;
  if AIndex <> FItemIndex then
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
  end
  else Result := True;
end;

{ TCarbonEdit }

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
    if CreateEditUniCodeTextControl(GetTopParentWindow,
      ParamsToCarbonRect(AParams), CFString, (Edit.PasswordChar <> #0), nil,
      Control) = noErr then
    begin
      Widget := Control;

      inherited;
    end
    else RaiseCreateWidgetError(LCLObject);
  finally
    FreeCFString(CFString);
  end;

  // set edit single line
  SetControlData(Control, kControlEntireControl, kControlEditTextSingleLineTag,
    SizeOf(Boolean), @SingleLine);
  FIsPassword := Edit.PasswordChar <> #0;
  FMaxLength := Edit.MaxLength;
end;

function TCarbonEdit.GetText(var S: String): Boolean;
var
  CFString: CFStringRef;
begin
  if not IsPassword then
    Result := inherited GetText(S)
  else
  begin
    Result := False;

    if GetControlData(ControlRef(Widget), GetEditPart,
      kControlEditTextPasswordCFStringTag, SizeOf(CFStringRef),
      @CFString, nil) <> noErr then Exit;

    try
      S := CFStringToStr(CFString);
      Result := True;
    finally
      FreeCFString(CFString);
    end;
  end;
end;

function TCarbonEdit.SetText(const S: String): Boolean;
var
  CFString: CFStringRef;
begin
  if not IsPassword then
    Result := inherited SetText(S)
  else
  begin
    CreateCFString(S, CFString);
    try
      Result := SetControlData(ControlRef(Widget), GetEditPart,
        kControlEditTextPasswordCFStringTag, SizeOf(CFStringRef), @CFString) = noErr;
    finally
      FreeCFString(CFString);
    end;
  end;
end;

{ TCarbonMemo }

procedure TCarbonMemo.SetScrollBars(const AValue: TScrollStyle);
begin
  if AValue <> FScrollBars then
  begin
    if ((AValue in [ssNone, ssBoth, ssAutoBoth]) and
      (FScrollBars in [ssNone, ssBoth, ssAutoBoth])) or
      ((AValue in [ssVertical, ssAutoVertical]) and
      (FScrollBars in [ssVertical, ssAutoVertical])) or
      ((AValue in [ssHorizontal, ssAutoHorizontal]) and
      (FScrollBars in [ssHorizontal, ssAutoHorizontal])) then
    begin
      FScrollBars := AValue;
      HIScrollViewSetScrollBarAutoHide(FScrollView,
        FScrollBars in [ssNone, ssAutoVertical, ssAutoHorizontal, ssAutoBoth]);
    end
    else
      RecreateWnd(LCLObject);
  end;
end;

function TCarbonMemo.GetFrame: ControlRef;
begin
  Result := FScrollView;
end;

procedure TCarbonMemo.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  Options, ScrollOptions: FPCMacOSAll.OptionBits;
  R: HIRect;
begin
  Options := kTXNMonostyledTextMask or kOutputTextInUnicodeEncodingMask;

  FScrollBars := (LCLObject as TCustomMemo).ScrollBars;
  case FScrollBars of
    ssNone, ssBoth, ssAutoBoth:
      ScrollOptions := kHIScrollViewOptionsVertScroll or
        kHIScrollViewOptionsHorizScroll;
    ssVertical, ssAutoVertical:
      ScrollOptions := kHIScrollViewOptionsVertScroll;
    ssHorizontal, ssAutoHorizontal:
      ScrollOptions := kHIScrollViewOptionsHorizScroll;
  end;

  // ssNone is mapped to ssAutoBoth because HITextView is not scrolling into
  // caret position

  R := ParamsToHIRect(AParams);
  if HITextViewCreate(@R, 0, Options, Control) = noErr then
  begin
    Widget := Control;

    if HIScrollViewCreate(ScrollOptions, FScrollView) <> noErr then
    begin
      DebugLn('TCarbonMemo.CreateWidget Error - unable to create scroll view!');
      Exit;
    end;

    if HIViewAddSubview(FScrollView, Control)<> noErr then
    begin
      DebugLn('TCarbonMemo.CreateWidget Error - unable to embed conrtol in scroll view!');
      Exit;
    end;

    HIViewSetVisible(Control, True);

    inherited;
  end
  else RaiseCreateWidgetError(LCLObject);

  HIScrollViewSetScrollBarAutoHide(FScrollView,
    FScrollBars in [ssNone, ssAutoVertical, ssAutoHorizontal, ssAutoBoth]);

  FMaxLength := 0;
end;

procedure TCarbonMemo.DestroyWidget;
begin
  inherited DestroyWidget;
  
  DisposeControl(FScrollView);
end;

procedure TCarbonMemo.TextDidChange;
var
  MemoStrings: TCarbonMemoStrings;
  Msg: TLMessage;
begin
  // limit the text according to MaxLength
  LimitTextLength;

  AdaptCharCase;

  // update memo strings
  MemoStrings := (LCLObject as TCustomMemo).Lines as TCarbonMemoStrings;
  if MemoStrings <> nil then MemoStrings.ExternalChanged;

  FillChar(Msg, SizeOf(Msg), 0);
  Msg.Msg := CM_TEXTCHANGED;
  DeliverMessage(LCLObject, Msg);
end;

procedure TCarbonMemo.SetColor(const AColor: TColor);
var
  CGColor: CGColorRef;
begin
  CGColor := CreateCGColor(AColor);
  try
    HITextViewSetBackgroundColor(HIViewRef(Widget), CGColor);
  finally
    CGColorRelease(CGColor);
  end;
end;

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

 // font style
 Attrs[3].tag := kTXNATSUIStyle;
 Attrs[3].size := kTXNATSUIStyleSize;
 Attrs[3].data.dataPtr := Pointer(TCarbonFont(AFont.Handle).Style);

 // apply
 TXNSetTypeAttributes(HITextViewGetTXNObject(ControlRef(Widget)), 4, @Attrs[0],
   kTXNStartOffset, kTXNEndOffset);

  // invalidate control
  Invalidate;
end;

end.

