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
  LCLMessageGlue, LCLType;

type

  { TCarbonCustomCheckBox }

  TCarbonCustomCheckBox = class(TCarbonControl)
  public
    class function GetValidEvents: TCarbonControlEvents; override;
    procedure Hit(AControlPart: ControlPartCode); override;
    procedure ValueChanged; override;

    function RetrieveState: Integer; virtual;
    procedure SetState(AState: Integer); virtual;
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
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    procedure SetGlyph(Glyph: CGImageRef); virtual;
    procedure SetLayout(APlacement: ControlButtonTextPlacement; ATextAlign: ControlButtonTextAlignment); virtual;
    procedure SetDefault(ADefault: Boolean); override;
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
begin
  LCLSendChangedMsg(LCLObject);
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
procedure TCarbonCustomCheckBox.SetState(AState: Integer);
begin
  SetControl32BitValue(ControlRef(Widget), AState);
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

  v := HIViewGetFirstSubview(v);
  while Assigned(v) do begin
    if (v <> Widget) then begin
      ctrl := GetCarbonControl(v);
      if ctrl is TCarbonRadioButton then
        TCarbonRadioButton(ctrl).SetState(kControlCheckBoxUncheckedValue);
    end;

    v := HIViewGetNextView(v);
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
var
  value  : Boolean;
begin
  //value := TCustomButton(LCLObject).Default;
  value:=ADefault;
  OSError(
    SetControlData(ControlRef(Widget), kControlEntireControl,
      kControlPushButtonDefaultTag, SizeOf(Boolean), @value),
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
  // values are used from Interface Builder
  StdButtonNormalSize = 20;
  StdButtonSmallSize = 17;
  StdButtonTinySize = 0; // 14

procedure TCarbonButton.BoundsChanged;
begin
  inherited BoundsChanged;
  SetControlViewStyle(Widget, StdButtonTinySize, StdButtonSmallSize, StdButtonNormalSize);
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
  ButtonKind: ThemeButtonKind;
begin
  if OSError(
    CreateBevelButtonControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      nil, kControlBevelButtonLargeBevel, kControlBehaviorPushbutton,
      nil, 0, 0, 0, Control),
    Self, SCreateWidget, SCreateBevelButton) then RaiseCreateWidgetError(LCLObject);

  Widget := Control;

  inherited;

  SetText(AParams.Caption);

  // set round border
  ButtonKind := kThemeRoundedBevelButton;
  OSError(SetControlData(ControlRef(Widget), kControlEntireControl,
      kControlBevelButtonKindTag, SizeOf(ThemeButtonKind), @ButtonKind),
    Self, SCreateWidget, SSetData, 'kControlBevelButtonKindTag');
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

end.

