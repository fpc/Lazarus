{ $Id$
                  -------------------------------------------
                  CarbonButtons.pp  -  Carbon buttons classes
                  -------------------------------------------

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
unit CarbonButtons;

{$mode objfpc}{$H+}

interface

// defines
{$I carbondefines.inc}

uses
 // rtl+ftl
  Types, Classes, SysUtils, Math, Contnrs,
 // carbon bindings
  MacOSAll,
 // LCL Carbon
  CarbonDef, CarbonPrivate, CarbonInt, CarbonProc,
  CarbonDbgConsts, CarbonUtils, CarbonStrings, CarbonCanvas, CarbonGDIObjects,
 // LCL
  LCLMessageGlue, LCLType, Graphics;

type
  TUpdateValueEvent = procedure (Sender: TObject; CurrentValue: Integer; var AValue: Integer) of object;

  { TCarbonCustomCheckBox }

  TCarbonCustomCheckBox = class(TCarbonControl)
  private
    fSupressNotify : Boolean;
    LastState: Integer;
    isSetState: Boolean;
  public
    UpdateValue: TUpdateValueEvent;
    class function GetValidEvents: TCarbonControlEvents; override;
    procedure Hit(AControlPart: ControlPartCode); override;
    procedure ValueChanged; override;
    function RetrieveState: Integer; virtual;
    procedure SetState(AState: Integer; NotifyChangeState: Boolean); virtual;
  end;

  { TCarbonCheckBox }

  TCarbonCheckBox = class(TCarbonCustomCheckBox)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    procedure BoundsChanged; override;
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
    procedure BoundsChanged; override;
  end;

  { TCarbonCustomButton }

  TCarbonCustomButton = class(TCarbonControl)
  public
    class function GetValidEvents: TCarbonControlEvents; override;
    procedure Hit(AControlPart: ControlPartCode); override;
  public
    procedure SetDefault(ADefault: Boolean); virtual;
  end;

  { TCarbonButton }

  TCarbonButton = class(TCarbonCustomButton)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    procedure BoundsChanged; override;
  end;

  { TCarbonBitBtn }

  TCarbonBitBtn = class(TCarbonCustomButton)
  private
    fPlacement : ControlButtonTextPlacement;
    fAlignment : ControlButtonTextAlignment;
    CustomFont : Boolean;
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure UpdateButtonStyle;
  public
    function GetPreferredSize: TPoint; override;
    procedure SetFont(const AFont: TFont); override;
    procedure SetGlyph(Glyph: CGImageRef); virtual;
    procedure SetLayout(APlacement: ControlButtonTextPlacement; ATextAlign: ControlButtonTextAlignment); virtual;
    procedure SetDefault(ADefault: Boolean); override;
    function GetBounds(var ARect:TRect):Boolean;override;
    function SetBounds(const ARect: TRect): Boolean; override;
  end;

implementation

{ TCarbonCustomCheckBox }

{------------------------------------------------------------------------------
  Method:  TCarbonCustomCheckBox.GetValidEvents
  Returns: Set of events with installed handlers
 ------------------------------------------------------------------------------}
class function TCarbonCustomCheckBox.GetValidEvents: TCarbonControlEvents;
begin
  Result := [cceValueChanged, cceHit];
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomCheckBox.Hit
  Params:  AControlPart - Hitted control part

  Hit event handler
 ------------------------------------------------------------------------------}
procedure TCarbonCustomCheckBox.Hit(AControlPart: ControlPartCode);
begin
  // do nothing, because value changed will be fired immediately
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomCheckBox.ValueChanged

  Value changed event handler
 ------------------------------------------------------------------------------}
procedure TCarbonCustomCheckBox.ValueChanged;
var
  NewState: Integer;
  RS: Integer;
begin
  if not isSetState and Assigned(UpdateValue) then begin
    RS:=RetrieveState;
    NewState:=RS;
    UpdateValue(Self, LastState, NewState);
    if NewState<>RS then SetValue(NewState);
  end;
  LastState:=RetrieveState;
  if not fSupressNotify then
    LCLSendChangedMsg(LCLObject)
  else
    fSupressNotify := False;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomCheckBox.RetrieveState
  Returns: State of Carbon custom check box
 ------------------------------------------------------------------------------}
function TCarbonCustomCheckBox.RetrieveState: Integer;
begin
  Result := GetControl32BitValue(ControlRef(Widget));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomCheckBox.SetState
  Params:  AState        - New state

  Sets the new state of Carbon custom check box
 ------------------------------------------------------------------------------}
procedure TCarbonCustomCheckBox.SetState(AState: Integer; NotifyChangeState: Boolean);
begin
  if RetrieveState=AState then Exit;
  isSetState:=True;
  fSupressNotify := not NotifyChangeState;
  SetControl32BitValue(ControlRef(Widget), AState);
  LastState:=AState;
  isSetState:=False;
end;

{ TCarbonCheckBox }

{------------------------------------------------------------------------------
  Method:  TCarbonCheckBox.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon check box
 ------------------------------------------------------------------------------}
procedure TCarbonCheckBox.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  Value: UInt32;
begin
  Value := 0;

  if OSError(
    CreateCheckBoxControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      nil, Value, True, Control),
    Self, SCreateWidget, 'CreateCheckBoxControl') then RaiseCreateWidgetError(LCLObject);

  Widget := Control;

  inherited;

  SetText(AParams.Caption);
end;

const
  // values are used from Interface Builder
  StdCheckBoxNormalSize = 18;
  StdCheckBoxSmallSize = 12;
  StdCheckBoxTinySize = 0; // 10

procedure TCarbonCheckBox.BoundsChanged;
begin
  inherited BoundsChanged;
  SetControlViewStyle(Widget, StdCheckBoxTinySize, StdCheckBoxSmallSize, StdCheckBoxNormalSize);
end;

{ TCarbonToggleBox }

{------------------------------------------------------------------------------
  Method:  TCarbonToggleBox.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon toggle box
 ------------------------------------------------------------------------------}
procedure TCarbonToggleBox.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  Value: UInt32;
begin
  Value := 0;
  if OSError(
    CreateBevelButtonControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      nil, kControlBevelButtonNormalBevel,
      kControlBehaviorToggles, nil, 0, 0, 0, Control),
    Self, SCreateWidget, SCreateBevelButton) then RaiseCreateWidgetError(LCLObject);

  Widget := Control;

  inherited;

  SetText(AParams.Caption);
  SetControl32BitValue(Control, Value);
end;

{ TCarbonRadioButton }

{------------------------------------------------------------------------------
  Method:  TCarbonRadioButton.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon radio button
 ------------------------------------------------------------------------------}
procedure TCarbonRadioButton.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  Value: UInt32;
begin
  Value := 0;
  if OSError(
    CreateRadioButtonControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      nil, Value, True, Control),
    Self, SCreateWidget, 'CreateRadioButtonControl') then RaiseCreateWidgetError(LCLObject);

  Widget := Control;

  inherited;

  SetText(AParams.Caption);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonRadioButton.ValueChanged

  Value changed event handler
 ------------------------------------------------------------------------------}
procedure TCarbonRadioButton.ValueChanged;
var
  Parent  : HIViewRef;
  v       : HIViewRef;
  ctrl    : TCarbonControl;
begin
  Parent := HIViewGetSuperview(Widget);
  if not Assigned(Parent) then Exit;


  if RetrieveState<>kControlCheckBoxUncheckedValue then
  begin
    v := HIViewGetFirstSubview(Parent);
    while Assigned(v) do
    begin
      if (v <> Widget) then
      begin
        ctrl := GetCarbonControl(v);
        if ctrl is TCarbonRadioButton then
          TCarbonRadioButton(ctrl).SetState(kControlCheckBoxUncheckedValue, True);
      end;
      v := HIViewGetNextView(v);
    end;
  end;
  inherited;
end;

const
  // values are used from Interface Builder
  StdRadioButtonNormalSize = 16;
  StdRadioButtonSmallSize = 14;
  StdRadioButtonTinySize = 0; // 10

procedure TCarbonRadioButton.BoundsChanged;
begin
  inherited BoundsChanged;
  SetControlViewStyle(Widget, StdRadioButtonTinySize, StdRadioButtonSmallSize, StdRadioButtonNormalSize);
end;

{ TCarbonCustomButton }

{------------------------------------------------------------------------------
  Method:  TCarbonCustomButton.GetValidEvents
  Returns: Set of events with installed handlers
 ------------------------------------------------------------------------------}
class function TCarbonCustomButton.GetValidEvents: TCarbonControlEvents;
begin
  Result := [cceHit];
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomButton.Hit
  Params:  AControlPart - Hitted control part

  Hit event handler
 ------------------------------------------------------------------------------}
procedure TCarbonCustomButton.Hit(AControlPart: ControlPartCode);
begin
  LCLSendClickedMsg(LCLObject);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCustomButton.SetDefault
  Params:  ADefault - Is default

  Sets the default indication
 ------------------------------------------------------------------------------}
procedure TCarbonCustomButton.SetDefault(ADefault: Boolean);
begin
  OSError(
    SetControlData(ControlRef(Widget), kControlEntireControl,
      kControlPushButtonDefaultTag, SizeOf(Boolean), @ADefault),
    Self, 'SetDefault', SSetData);
end;

{ TCarbonButton }

{------------------------------------------------------------------------------
  Method:  TCarbonButton.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon button
 ------------------------------------------------------------------------------}
procedure TCarbonButton.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
begin
  // create the button at bounds
  if OSError(
    CreatePushButtonControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      nil, Control),
    Self, SCreateWidget, 'CreatePushButtonControl') then RaiseCreateWidgetError(LCLObject);

  Widget := Control;

  inherited;

  SetText(AParams.Caption);
end;

const
  // values are used from Interface Builder, should (can?) be evaluated from themes
  MaxPushButtonHeight       = 22;
  StdPushButtonNormalHeight = 20;
  StdPushButtonSmallHeight  = 17;
  StdPushButtonTinyHeight   = 0; // 14

  NormalPushBtnAddV = 2;
  SmallPushBtnAddV  = 1;
  TinyPushBtnAddV   = 0;

function PushBtnAddV(PushBtnStyle: Integer=kThemePushButtonNormal): Integer;
begin
  case PushBtnStyle of
    kThemePushButtonMini: Result:=TinyPushBtnAddV;
    kThemePushButtonSmall: Result:=SmallPushBtnAddV;
    kThemePushButtonNormal: Result:=NormalPushBtnAddV;
  else
    Result:=0;
  end;
end;

procedure GetBevelButtonStyle(const r: TRect; var ButtonStyle: ThemeButtonKind; var ThemeFont: ThemeFontID);
var
  h : Integer;
begin
  h := r.Bottom - r.Top;
  if h < StdPushButtonSmallHeight-1 then
  begin {tiny pushbutton}
    ButtonStyle := kThemePushButtonMini;
    ThemeFont := kThemeMiniSystemFont;
  end
  else if h < StdPushButtonNormalHeight-1 then
  begin {small pushbutton}
    ButtonStyle := kThemePushButtonSmall;
    ThemeFont := kThemeSmallSystemFont;
  end
  else if h <= MaxPushButtonHeight then
  begin {normal pushbutton}
    ButtonStyle := kThemePushButtonNormal;
    ThemeFont := kThemePushButtonFont;
  end
  else begin {bevelbutton}
    ButtonStyle := kThemeRoundedBevelButton;
    ThemeFont := kThemeSystemFont;
  end;
end;

procedure TCarbonButton.BoundsChanged;
begin
  inherited BoundsChanged;
  SetControlViewStyle(Widget, StdPushButtonTinyHeight, StdPushButtonSmallHeight, StdPushButtonNormalHeight);
end;

{ TCarbonBitBtn }

{------------------------------------------------------------------------------
  Method:  TCarbonBitBtn.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon bitmap button
 ------------------------------------------------------------------------------}
procedure TCarbonBitBtn.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
begin
  if OSError(
    CreateBevelButtonControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      nil, kControlBevelButtonLargeBevel, kControlBehaviorPushbutton,
      nil, 0, 0, 0, Control),
    Self, SCreateWidget, SCreateBevelButton) then RaiseCreateWidgetError(LCLObject);

  Widget := Control;

  inherited;

  SetText(AParams.Caption);

  UpdateButtonStyle;
end;

procedure TCarbonBitBtn.UpdateButtonStyle;
var
  bnds        : TRect;
  ButtonKind  : ThemeButtonKind;
  FontStyle   : ControlFontStyleRec;
  themeid     : ThemeFontID;
begin
  GetBounds(bnds);
  GetBevelButtonStyle(bnds, ButtonKind, themeid);

  if not CustomFont then
  begin
    FillChar(FontStyle, sizeof(FontStyle), 0);
    FontStyle.font := themeid;
    FontStyle.flags := kControlUseThemeFontIDMask;
    OSError(SetControlFontStyle(ControlRef(Widget), FontStyle),
      Self, 'UpdateButtonStyle', SSetFontStyle, 'kControlBevelButtonKindTag');
  end;

  OSError(SetControlData(ControlRef(Widget), kControlEntireControl,
      kControlBevelButtonKindTag, SizeOf(ThemeButtonKind), @ButtonKind),
    Self, 'UpdateButtonStyle', SSetData, 'kControlBevelButtonKindTag');
end;

function TCarbonBitBtn.GetPreferredSize: TPoint;
begin
  Result:=inherited GetPreferredSize;
  Result.Y := 20 + PushBtnAddV;
end;

procedure TCarbonBitBtn.SetFont(const AFont: TFont);
begin
  inherited;
  CustomFont:=(AFont.Name<>'default') and (AFont.Name<>'');
  UpdateButtonStyle;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonBitBtn.SetGlyph
  Params:  AGlyph  - New glyph bitmap

  Sets the glyph bitmap
 ------------------------------------------------------------------------------}
procedure TCarbonBitBtn.SetGlyph({const AGlyph: TBitmap} Glyph: CGImageRef);
var
  ContentInfo: ControlButtonContentInfo;
begin
  ContentInfo.imageRef := Glyph;
  if Assigned(Glyph) then
    ContentInfo.contentType := kControlContentCGImageRef
  else
    ContentInfo.contentType := kControlContentTextOnly;

  try
    OSError(SetBevelButtonContentInfo(ControlRef(Widget), @ContentInfo),
      Self, 'SetGlyph', 'SetBevelButtonContentInfo');
  finally
    CGImageRelease(ContentInfo.imageRef);
  end;
  UpdateButtonStyle;
  //SetLayout((LCLObject as TCustomBitBtn).Layout);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonBitBtn.SetLayout
  Params:  ALayout  - Bitmap and caption layout

  Sets the bitmap and caption layout
 ------------------------------------------------------------------------------}
procedure TCarbonBitBtn.SetLayout(APlacement: ControlButtonTextPlacement;
  ATextAlign: ControlButtonTextAlignment);
begin
  OSError(SetBevelButtonTextPlacement(ControlRef(Widget), APlacement),
    Self, 'SetLayout', 'SetBevelButtonTextPlacement');
  OSError(SetBevelButtonTextAlignment(ControlRef(Widget), ATextAlign, 0),
    Self, 'SetLayout', 'SetBevelButtonTextAlignment');
  fPlacement := APlacement;
  fAlignment := ATextAlign;
  UpdateButtonStyle;
  Invalidate;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonBitBtn.SetDefault
  Params:  ADefault - Is default

  Sets the default indication
 ------------------------------------------------------------------------------}
procedure TCarbonBitBtn.SetDefault(ADefault: Boolean);
begin
  // not supported
end;

function TCarbonBitBtn.GetBounds(var ARect:TRect):Boolean;
begin
  Result:=inherited GetBounds(ARect);
  inc(ARect.Bottom, PushBtnAddV);
end;

function TCarbonBitBtn.SetBounds(const ARect: TRect): Boolean;
var
  r : TRect;
begin
  r:=ARect;
  dec(r.Bottom, PushBtnAddV);
  Result:=inherited SetBounds(r);
  UpdateButtonStyle;
end;

end.

