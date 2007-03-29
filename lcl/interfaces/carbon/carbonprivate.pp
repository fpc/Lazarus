{ $Id: $
                  --------------------------------------------
                  carbonprivate.pp  -  Carbon internal classes
                  --------------------------------------------

 This unit contains the private classhierarchy for the Carbon implemetations
 This hierarchy reflects (more or less) the Carbon widget hierarchy

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
unit CarbonPrivate;

{$mode objfpc}{$H+}

interface

uses
 // rtl+ftl
  Types, Classes, SysUtils, Math,
 // carbon bindings
  FPCMacOSAll, CarbonUtils, CarbonExtra,
 // widgetset
  WSControls, WSLCLClasses, WSProc,
 // LCL
  LMessages, LCLMessageGlue, LCLProc, LCLType, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Menus,
 // LCL Carbon
  CarbonDef;
  
type
  TCarbonControlEvent = (cceValueChanged, cceIndicatorMoved,
    cceTextDidChange, cceDoAction, cceDraw, cceHit, cceListItemSelected);
  TCarbonControlEvents = set of TCarbonControlEvent;
  
  { TCarbonControl }
  
  TCarbonControl = class(TCarbonWidget)
  protected
    procedure RegisterEvents; override;
    procedure UnregisterEvents; override;
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
    function GetContent: ControlRef; override;
    function GetFrame: ControlRef; virtual;
  public
    class function GetValidEvents: TCarbonControlEvents; dynamic;
    procedure Hit(AControlPart: ControlPartCode); dynamic;
    procedure Draw; dynamic;
    procedure ValueChanged; dynamic;
    procedure IndicatorMoved; dynamic;
    procedure TextDidChange; dynamic;
    procedure DoAction(AControlPart: ControlPartCode); dynamic;
    procedure ListItemSelected(AIndex: Integer); dynamic;
  public
    function GetTopParentWindow: WindowRef; override;
    function GetThemeDrawState: ThemeDrawState;
    function GetMousePos: TPoint; override;
    function GetClientRect(var ARect: TRect): Boolean; override;
    function GetPreferredSize: TPoint; override;
    procedure Invalidate(Rect: PRect = nil); override;
    function IsEnabled: Boolean; override;
    function IsVisible: Boolean; override;
    function Enable(AEnable: Boolean): Boolean; override;
    
    function GetBounds(var ARect: TRect): Boolean; override;
    function GetScreenBounds(var ARect: TRect): Boolean; override;
    function SetBounds(const ARect: TRect): Boolean; override;

    procedure SetColor(const AColor: TColor); override;
    procedure SetFont(const AFont: TFont); override;
    procedure ShowHide(AVisible: Boolean); override;
    
    function GetText(var S: String): Boolean; override;
    function SetText(const S: String): Boolean; override;
    
    function Update: Boolean; override;
  public
  { Frame:
     = widget in controls without special frame control
     - frame area control of control
     - determines bounds of control
     - processes only bounds changed event            }
    property Frame: ControlRef read GetFrame;
  end;
  
  { TCarbonWindow }

  TCarbonWindow = class(TCarbonWidget)
  protected
    procedure RegisterEvents; override;
    procedure UnregisterEvents; override;
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
    function GetContent: ControlRef; override;
  public
    function GetMousePos: TPoint; override;
    function GetTopParentWindow: WindowRef; override;
    function GetClientRect(var ARect: TRect): Boolean; override;
    procedure Invalidate(Rect: PRect = nil); override;
    function IsEnabled: Boolean; override;
    function IsVisible: Boolean; override;
    function Enable(AEnable: Boolean): boolean; override;
    
    function GetBounds(var ARect: TRect): Boolean; override;
    function GetScreenBounds(var ARect: TRect): Boolean; override;
    function SetBounds(const ARect: TRect): Boolean; override;

    procedure SetColor(const AColor: TColor); override;
    procedure SetFont(const AFont: TFont); override;
    procedure ShowHide(AVisible: Boolean); override;
    
    function GetText(var S: String): Boolean; override;
    function SetText(const S: String): Boolean; override;
    
    function Update: Boolean; override;
  end;
  
  { TCarbonHintWindow }

  TCarbonHintWindow = class(TCarbonWindow)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  end;

  { TCarbonCustomControl }

  TCarbonCustomControl = class(TCarbonControl)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  end;
  
  { TCarbonGroupBox }

  TCarbonGroupBox = class(TCarbonControl)
  private
    FUserPane: ControlRef;
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
    function GetContent: ControlRef; override;
  end;

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
  
  { TCarbonListBox }

  TCarbonListBox = class(TCarbonControl)
  private
    FItemIndex: Integer;
    FList: ListHandle;
    FListDefUPP: ListDefUPP;
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
  public
    function GetItemsCount: Integer;
    function GetItemIndex: Integer;
    procedure SetItemIndex(AIndex: Integer);
    property List: ListHandle read FList;
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

  { TCarbonCustomCheckBox }

  TCarbonCustomCheckBox = class(TCarbonControl)
  public
    class function GetValidEvents: TCarbonControlEvents; override;
    procedure Hit(AControlPart: ControlPartCode); override;
    procedure ValueChanged; override;
  end;
  
  { TCarbonCheckBox }

  TCarbonCheckBox = class(TCarbonCustomCheckBox)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  end;
  
  { TCarbonToggleBox }

  TCarbonToggleBox = class(TCarbonCustomCheckBox)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  end;

  { TCarbonRadioButton }

  TCarbonRadioButton = class(TCarbonCustomCheckBox)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    procedure ValueChanged; override;
  end;
  
  { TCarbonCustomButton }

  TCarbonCustomButton = class(TCarbonControl)
  public
    class function GetValidEvents: TCarbonControlEvents; override;
    procedure Hit(AControlPart: ControlPartCode); override;
  end;
  
  { TCarbonButton }

  TCarbonButton = class(TCarbonCustomButton)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  end;
  
  { TCarbonBitBtn }

  TCarbonBitBtn = class(TCarbonCustomButton)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  end;
  
  { TCarbonStaticText }

  TCarbonStaticText = class(TCarbonControl)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  end;
  
  { TCarbonCustomBar }

  TCarbonCustomBar = class(TCarbonControl)
  public
    procedure SetData(APos: Integer);
    procedure SetData(APos, AMin, AMax: Integer);
    procedure SetData(APos, AMin, AMax, APage: Integer);
    function GetPos: Integer;
  end;
  
  { TCarbonProgressBar }

  TCarbonProgressBar = class(TCarbonCustomBar)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  end;

  { TCarbonTrackBar }

  TCarbonTrackBar = class(TCarbonCustomBar)
  private
    FTicks: LongWord;
  public
    class function GetValidEvents: TCarbonControlEvents; override;
    procedure ValueChanged; override;
    procedure IndicatorMoved; override;
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    function GetTicks: LongWord;
    property Ticks: LongWord read FTicks;
  end;

  { TCarbonScrollBar }

  TCarbonScrollBar = class(TCarbonCustomBar)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    class function GetValidEvents: TCarbonControlEvents; override;
    procedure ValueChanged; override;
    procedure IndicatorMoved; override;
    procedure DoAction(AControlPart: ControlPartCode); override;
  end;

implementation

uses InterfaceBase, CarbonProc, CarbonWSStdCtrls, CarbonStrings,
  CarbonGDIObjects;

procedure RaiseCreateWidgetError(AControl: TWinControl);
begin
  raise Exception.CreateFmt('Unable to create Carbon widget for %s: %s!',
    [AControl.Name, AControl.ClassName]);
end;

// Store state of key modifiers so that we can emulate keyup/keydown
// of keys like control, option, command, caps lock, shift
var PrevKeyModifiers: UInt32 = 0;

// Stores mouse up message to be fired on control hit after value is updated
var SavedMouseUpMsg: TLMMouse;

{$I mackeycodes.inc}

{$I carbonprivatecommon.inc}
{$I carbonprivatecontrol.inc}
{$I carbonprivatewindow.inc}

{ TCarbonHintWindow }

procedure TCarbonHintWindow.CreateWidget(const AParams: TCreateParams);
var
  Window: WindowRef;
  MinSize, MaxSize: HISize;
begin
  if CreateNewWindow(kHelpWindowClass,
    kWindowCompositingAttribute or
    kWindowHideOnSuspendAttribute or kWindowStandardHandlerAttribute,
    ParamsToCarbonRect(AParams), Window) = noErr then
  begin
    Widget := Window;

    SetWindowProperty(Widget, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Self), @Self);
    SetControlProperty(Content, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Self), @Self);
  end
  else RaiseCreateWidgetError(LCLObject);
  
  SetColor(LCLObject.Color);
end;

{ TCarbonCustomControl }

procedure TCarbonCustomControl.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  Attrs: LongWord;
begin
  Attrs := kControlSupportsEmbedding or kControlSupportsFocus or
    kControlWantsActivate or kControlHandlesTracking or
    kControlHasSpecialBackground or kControlGetsFocusOnClick or
    kControlSupportsSetCursor or kControlSupportsContextualMenus or
    kControlSupportsClickActivation;
  
  if CreateUserPaneControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
    Attrs, Control) = noErr then
  begin
    Widget := Control;
    
    inherited;
  end
  else RaiseCreateWidgetError(LCLObject);
end;

{ TCarbonGroupBox }

procedure TCarbonGroupBox.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  CFString: CFStringRef;
  R: TRect;
begin
  CreateCFString(AParams.Caption, CFString);
  try
    if CreateGroupBoxControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      CFString, not (LCLObject.Parent is TCustomGroupBox), Control) = noErr then
    begin
      Widget := Control;
      
      if not GetClientRect(R) then
      begin
        DebugLn('TCarbonGroupBox.CreateWidget Error - no content region!');
        Exit;
      end;
      
      if CreateUserPaneControl(GetTopParentWindow, GetCarbonRect(R),
        kControlSupportsEmbedding or kControlHandlesTracking, FUserPane) <> noErr then
      begin
        DebugLn('TCarbonGroupBox.CreateWidget Error - unable to create content control!');
        Exit;
      end;
      
      if HIViewAddSubview(Control, FUserPane) <> noErr then
      begin
        DebugLn('TCarbonGroupBox.CreateWidget Error - unable to embed conent control!');
        Exit;
      end;
      
      inherited;
    end
    else RaiseCreateWidgetError(LCLObject);
  finally
    FreeCFString(CFString);
  end;
end;

procedure TCarbonGroupBox.DestroyWidget;
begin
  DisposeControl(FUserPane);

  inherited DestroyWidget;
end;

function TCarbonGroupBox.GetContent: ControlRef;
begin
  Result := FUserPane;
end;

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

{ TCarbonListBox }

{------------------------------------------------------------------------------
  Name: CarbonListBoxDefProc
  Carbon interface list box callback
 ------------------------------------------------------------------------------}
procedure CarbonListBoxDefProc(lMessage: SInt16; lSelect: Boolean;
  var lRect: Rect; lCell: Cell; lDataOffset: SInt16; lDataLen: SInt16;
  lHandle: ListHandle); {$IFDEF darwin}mwpascal;{$ENDIF}
  
  function GetItems: TCarbonListBoxStrings;
  begin
    Result := TCarbonListBoxStrings(GetListUserHandle(lHandle));
  end;
  
var
  DrawItem: PDrawListItemStruct;
begin
  case lMessage of
    lDrawMsg: // draw list item
      begin
        DebugLn('CarbonListBoxDefProc Draw Item ' + DbgS(lCell.v) + ' Sel: ' + DbgS(lSelect) + ' Rect: ' + DbgS(CarbonRectToRect(lRect)));
        Exit;
        New(DrawItem);
        try
          DrawItem^.ItemID := lCell.v;
          DrawItem^.Area := CarbonRectToRect(lRect);
          DrawItem^.DC := HDC(GetItems.Owner.Context);
          
          if lSelect then
            DrawItem^.ItemState := [odSelected]
          else DrawItem^.ItemState := [];
          
          LCLSendDrawListItemMsg(GetItems.Owner.LCLObject, DrawItem);
        finally
          Dispose(DrawItem);
        end;
      end;
    lHiliteMsg: // list item selection changed
      begin
        GetItems.Owner.FItemIndex := lCell.v;
        DebugLn('CarbonListBoxDefProc Hilite Item ' + DbgS(lCell.v) + ' Sel: ' + DbgS(lSelect));
        LCLSendSelectionChangedMsg(GetItems.Owner.LCLObject);
      end;
  end;
end;

procedure TCarbonListBox.CreateWidget(const AParams: TCreateParams);
var
  ListBox: TCustomListBox;
  Control: ControlRef;
  ListDef: ListDefSpec;
begin
  ListBox := LCLObject as TCustomListBox;

  FListDefUPP := NewListDefUPP(@CarbonListBoxDefProc);
  ListDef.defType := kListDefStandardTextType;//kListDefUserProcType;
  //ListDef.userProc := FListDefUPP;

  if CreateListBoxControl(GetTopParentWindow, ParamsToCarbonRect(AParams), False,
    ListBox.Items.Count, 1, False, True, ListBox.ItemHeight, 2000, False, ListDef,
    Control) = noErr then
  begin
    Widget := Control;

    inherited;
  end
  else RaiseCreateWidgetError(LCLObject);
  
  FItemIndex := -1;
  GetControlData(Control, kControlEntireControl, kControlListBoxListHandleTag,
    SizeOf(ListHandle), @FList, nil);
end;

procedure TCarbonListBox.DestroyWidget;
begin
   DisposeListDefUPP(FListDefUPP);

  inherited;
end;

function TCarbonListBox.GetItemsCount: Integer;
var
  Bounds: ListBounds;
begin
  if GetListDataBounds(FList, Bounds) = nil then Result := 0
  else Result := Bounds.bottom;
end;

function TCarbonListBox.GetItemIndex: Integer;
begin
  Result := FItemIndex;
end;

procedure TCarbonListBox.SetItemIndex(AIndex: Integer);
var
  Item: Cell;
  I: Integer;
begin
  if AIndex <> FItemIndex then
  begin
    // deselect all except item index
    Item.h := 0;
    Item.v := 0;
    for I := 0 to GetItemsCount - 1 do
    begin
      LSetSelect(I = AIndex, Item, FList);
      Inc(Item.v);
    end;
    
    FItemIndex := AIndex;
  end;
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
  DisposeControl(FScrollView);
  
  inherited DestroyWidget;
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

{ TCarbonCustomCheckBox }

class function TCarbonCustomCheckBox.GetValidEvents: TCarbonControlEvents;
begin
  Result := [cceValueChanged, cceHit];
end;

procedure TCarbonCustomCheckBox.Hit(AControlPart: ControlPartCode);
begin
  // do nothing, because value changed will be fired immediately
end;

procedure TCarbonCustomCheckBox.ValueChanged;
begin
  LCLSendChangedMsg(LCLObject);
end;

{ TCarbonCheckBox }

procedure TCarbonCheckBox.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  CFString: CFStringRef;
  Value: UInt32;
begin
  case (LCLObject as TCustomCheckBox).State of
    cbChecked  : Value := kControlCheckBoxCheckedValue;
    cbUnChecked: Value := kControlCheckBoxUncheckedValue;
    cbGrayed   : Value := kControlCheckBoxMixedValue;
  end;

  CreateCFString(AParams.Caption, CFString);
  try
    if CreateCheckBoxControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      CFString, Value, True, Control) = noErr then
    begin
      Widget := Control;

      inherited;
    end
    else RaisecreateWidgetError(LCLObject);
  finally
    FreeCFString(CFString);
  end;
end;

{ TCarbonToggleBox }

procedure TCarbonToggleBox.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  CFString: CFStringRef;
  Value: UInt32;
begin
  case (LCLObject as TToggleBox).State of
    cbChecked  : Value := kControlCheckBoxCheckedValue;
    cbUnChecked: Value := kControlCheckBoxUncheckedValue;
    cbGrayed   : Value := kControlCheckBoxMixedValue;
  end;

  CreateCFString(AParams.Caption, CFString);
  try
    if CreateBevelButtonControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      CFString, kControlBevelButtonNormalBevel,
      kControlBehaviorToggles, nil, 0, 0, 0, Control) = noErr then
    begin
      Widget := Control;
      
      inherited;
    end
    else RaisecreateWidgetError(LCLObject);
  finally
    FreeCFString(CFString);
  end;
  SetControl32BitValue(Control, Value);
end;

{ TCarbonRadioButton }

procedure TCarbonRadioButton.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  CFString: CFStringRef;
  Value: UInt32;
begin
  case (LCLObject as TRadioButton).State of
    cbChecked  : Value := kControlCheckBoxCheckedValue;
    cbUnChecked: Value := kControlCheckBoxUncheckedValue;
    cbGrayed   : Value := kControlCheckBoxMixedValue;
  end;

  CreateCFString(AParams.Caption, CFString);
  try
    if CreateRadioButtonControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      CFString, Value, True, Control) = noErr then
    begin
      Widget := Control;
      
      inherited;
    end
    else RaisecreateWidgetError(LCLObject);
  finally
    FreeCFString(CFString);
  end;
end;

procedure TCarbonRadioButton.ValueChanged;
var
  RadioButton: TRadioButton;
  Sibling: TControl;
  I: Integer;
begin
  if GetControl32BitValue(ControlRef(Widget)) = kControlCheckBoxCheckedValue then
  begin
    DebugLn('TCarbonRadioButton.ValueChanged Uncheck Sibling');

    // uncheck sibling radio buttons
    RadioButton := (LCLObject as TRadioButton);
    if RadioButton.Parent <> nil then
    begin
      for I := 0 to RadioButton.Parent.ControlCount - 1 do
      begin
        Sibling := RadioButton.Parent.Controls[I];
        if (Sibling is TRadioButton) and (Sibling <> RadioButton) then
          (Sibling as TRadioButton).Checked := False;
      end;
    end;
  end;

  inherited;
end;

{ TCarbonButton }

class function TCarbonCustomButton.GetValidEvents: TCarbonControlEvents;
begin
  Result := [cceHit];
end;

procedure TCarbonCustomButton.Hit(AControlPart: ControlPartCode);
begin
  LCLSendClickedMsg(LCLObject);
end;

{ TCarbonButton }

procedure TCarbonButton.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  CFString: CFStringRef;
begin
  // create the button at bounds with title
  CreateCFString(AParams.Caption, CFString);
  try
    if CreatePushButtonControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      CFString, Control) = noErr then
    begin
      Widget := Control;

      inherited;
    end
    else RaisecreateWidgetError(LCLObject);
  finally
    FreeCFString(CFString);
  end;
end;

{ TCarbonBitBtn }

procedure TCarbonBitBtn.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  CFString: CFStringRef;
begin
  CreateCFString(AParams.Caption, CFString);
  try
    if CreateBevelButtonControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      CFString, kControlBevelButtonNormalBevel, kControlBehaviorPushbutton,
      nil, 0, 0, 0, Control) = noErr then
    begin
      Widget := Control;
      
      inherited;
    end
    else RaisecreateWidgetError(LCLObject);
  finally
    FreeCFString(CFString);
  end;
end;


{ TCarbonStaticText }

procedure TCarbonStaticText.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  CFString: CFStringRef;
  MultiLine: Boolean = True;
  FontStyle: ControlFontStyleRec;
begin
  FontStyle.flags := kControlUseJustMask;
  case (LCLObject as TCustomStaticText).Alignment of
  taLeftJustify:  FontStyle.just := teFlushLeft;
  taRightJustify: FontStyle.just := teFlushRight;
  taCenter:       FontStyle.just := teCenter;
  end;

  CreateCFString(AParams.Caption, CFString);
  try
    if CreateStaticTextControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      CFString, @FontStyle, Control) = noErr then
    begin
      Widget := Control;
      
      inherited;
    end
    else RaisecreateWidgetError(LCLObject);
  finally
    FreeCFString(CFString);
  end;
  
  // switch on multi-line attribute
  SetControlData(Control, kControlEntireControl,
    kControlStaticTextIsMultilineTag, SizeOf(Boolean), @MultiLine);
end;

{ TCarbonCustomBar }

procedure TCarbonCustomBar.SetData(APos: Integer);
begin
  SetControl32BitValue(ControlRef(Widget), APos);
end;

procedure TCarbonCustomBar.SetData(APos, AMin, AMax: Integer);
begin
  SetControl32BitMinimum(ControlRef(Widget), AMin);
  SetControl32BitMaximum(ControlRef(Widget), AMax);
  SetControl32BitValue(ControlRef(Widget), APos);
end;

procedure TCarbonCustomBar.SetData(APos, AMin, AMax, APage: Integer);
begin
  SetControl32BitMinimum(ControlRef(Widget), AMin);
  SetControl32BitMaximum(ControlRef(Widget), AMax);
  SetControl32BitValue(ControlRef(Widget), APos);
  SetControlViewSize(ControlRef(Widget), APage);
end;

function TCarbonCustomBar.GetPos: Integer;
begin
  Result := GetControl32BitValue(ControlRef(Widget));
end;

{ TCarbonProgressBar }

procedure TCarbonProgressBar.CreateWidget(const AParams: TCreateParams);
var
  ProgressBar: TCustomProgressBar;
  Control: ControlRef;
begin
  ProgressBar := LCLObject as TCustomProgressBar;

  // create determinate progress bar
  if CreateProgressBarControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
    ProgressBar.Position, ProgressBar.Min, ProgressBar.Max, False,
    Control) = noErr then
  begin
    Widget := Control;
    
    inherited;
  end
  else RaiseCreateWidgetError(LCLObject);
end;

{ TCarbonTrackBar }

{------------------------------------------------------------------------------
  Method:  TCarbonTrackBar.GetTicks
  Returns: Number of ticks

  Returns the number of ticks for the track bar
 ------------------------------------------------------------------------------}
function TCarbonTrackBar.GetTicks: LongWord;
var
  TrackBar: TCustomTrackBar;
begin
  Result := 0;
  TrackBar := LCLObject as TCustomTrackBar;
  if TrackBar = nil then Exit;
  if TrackBar.TickStyle = tsNone then Exit;

  if TrackBar.Frequency > 0 then
    Result := Math.Ceil(Abs(TrackBar.Max - TrackBar.Min) / TrackBar.Frequency) + 1
  else
    Result := 2;
end;

class function TCarbonTrackBar.GetValidEvents: TCarbonControlEvents;
begin
  Result := [cceValueChanged, cceIndicatorMoved];
end;

procedure TCarbonTrackBar.ValueChanged;
begin
  LCLSendChangedMsg(LCLObject);
end;

procedure TCarbonTrackBar.IndicatorMoved;
begin
  ValueChanged;
end;

procedure TCarbonTrackBar.CreateWidget(const AParams: TCreateParams);
var
  TrackBar: TCustomTrackBar;
  Control: ControlRef;
begin
  TrackBar := LCLObject as TCustomTrackBar;
  
  FTicks := GetTicks;

  if CreateSliderControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
    TrackBar.Position, TrackBar.Min, TrackBar.Max,
    kControlSliderPointsDownOrRight, FTicks, True, nil, Control) = noErr then
  begin
    Widget := Control;
    
    inherited;
  end
  else RaiseCreateWidgetError(LCLObject);
end;

{ TCarbonScrollBar }

procedure TCarbonScrollBar.CreateWidget(const AParams: TCreateParams);
var
  ScrollBar: TCustomScrollBar;
  Control: ControlRef;
begin
  ScrollBar := LCLObject as TCustomScrollBar;

  if CreateScrollBarControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
    ScrollBar.Position, ScrollBar.Min, ScrollBar.Max, ScrollBar.PageSize, True,
    nil, Control) = noErr then
  begin
    Widget := Control;
    
    inherited;
  end
  else RaiseCreateWidgetError(LCLObject);
end;

class function TCarbonScrollBar.GetValidEvents: TCarbonControlEvents;
begin
  Result := [cceValueChanged, cceIndicatorMoved, cceDoAction];
end;

procedure TCarbonScrollBar.ValueChanged;
var
  ScrollMsg: TLMScroll;
begin
  FillChar(ScrollMsg, SizeOf(TLMScroll), 0);

  ScrollMsg.Msg := LM_HSCROLL;
  ScrollMsg.ScrollCode := SB_THUMBTRACK;
  ScrollMsg.Pos := GetControl32BitValue(ControlRef(Widget));
  ScrollMsg.ScrollBar := HWND(Widget);

  DeliverMessage(LCLObject, ScrollMsg);
end;

procedure TCarbonScrollBar.IndicatorMoved;
begin
  ValueChanged;
end;

procedure TCarbonScrollBar.DoAction(AControlPart: ControlPartCode);
var
  ScrollMsg: TLMScroll;
  ScrollCode: SmallInt;
begin
  ScrollCode := -1; // valid scrollcode is >= 0
  case AControlPart of
    kControlUpButtonPart  : ScrollCode := SB_LINEUP;
    kControlDownButtonPart: ScrollCode := SB_LINEDOWN;
    kControlPageUpPart    : ScrollCode := SB_PAGEUP;
    kControlPageDownPart  : ScrollCode := SB_PAGEDOWN;
  end;
  
//DebugLn('TCarbonScrollBar.DoAction ' + IntToStr(Integer(AControlPart)) + ' ' +
//  IntToStr(ScrollCode));

  if ScrollCode >= 0 then
  begin
    FillChar(ScrollMsg, SizeOf(TLMScroll), 0);

    ScrollMsg.Msg := LM_HSCROLL;
    ScrollMsg.ScrollCode := ScrollCode;
    ScrollMsg.Pos := GetControl32BitValue(ControlRef(Widget));
    ScrollMsg.ScrollBar := HWND(Widget);

    DeliverMessage(LCLObject, ScrollMsg);
  end;
end;



end.

