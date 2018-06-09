{ TCheckBoxThemed

  Copyright (C) 2017 Lazarus team

  This library is free software; you can redistribute it and/or modify it
  under the same terms as the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.

}
unit CheckBoxThemed;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Math,
  // LCL
  Controls, StdCtrls, Graphics, ActnList, Forms,
  LCLIntf, LMessages, LCLProc, LCLType, Themes,
  // LazUtils
  LazMethodList;

type
  TCustomCheckBoxThemed = class;

  { TCheckBoxThemedActionLink }
  TCheckBoxThemedActionLink = class(TWinControlActionLink)
  protected
    FClientCheckBoxThemed: TCustomCheckBoxThemed;
    procedure AssignClient(AClient: TObject); override;
    procedure SetChecked(Value: Boolean); override;
  public
    function IsCheckedLinked: Boolean; override;
  end;

  TCheckBoxThemedActionLinkClass = class of TCheckBoxThemedActionLink;

  { TCustomCheckBoxThemed }
  TCustomCheckBoxThemed = class(TCustomControl)
  private
    FAlignment: TLeftRight;
    FAllowGrayed: Boolean;
    FCheckBoxHovered: Boolean;
    FCheckFromAction: Boolean;
    FOnChange: TNotifyEvent;
    FState: TCheckBoxState;
    function GetChecked: Boolean;
    procedure SetAlignment(AValue: TLeftRight);
    procedure SetCheckBoxHovered(AValue: Boolean);
    procedure SetChecked(AValue: Boolean);
    procedure SetState(AValue: TCheckBoxState);
  private class var
    FThemeCheckBoxSize: TSize;
  protected
    class function GetCheckBoxSize(const PixelsPerInch: Integer): TSize;
  protected
    CheckBoxPressed: Boolean;
    KnobPosUnchecked, KnobPosChecked, KnobPosGrayed: Integer;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer;
                                     {%H-}WithThemeSpace: Boolean); override;
    procedure CMBiDiModeChanged(var {%H-}Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    class procedure InitCheckBoxSize;
    function DialogChar(var Message: TLMKey): Boolean; override;
    procedure DoClick;
    procedure DoEnter; override;
    procedure DoExit; override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure TextChanged; override;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    property CheckBoxHovered: Boolean read FCheckBoxHovered write SetCheckBoxHovered;
    property CheckFromAction: Boolean read FCheckFromAction write FCheckFromAction;
  protected const
    cFocusBorder: SmallInt = 2;
    cIndent: SmallInt = 5;
  public
    class procedure PaintSelf(ACanvas: TCanvas; ACaption: string; ARect: TRect;
      AState: TCheckBoxState; ARightToLeft, AHovered, APressed, AFocused: Boolean;
      AAlignment: TLeftRight; AEnabled: Boolean = True);
    constructor Create(AOwner: TComponent); override;
    property Alignment: TLeftRight read FAlignment write SetAlignment default taRightJustify;
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    property Checked: Boolean read GetChecked write SetChecked default False;
    property State: TCheckBoxState read FState write SetState default cbUnchecked;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TCheckBoxThemed }
  TCheckBoxThemed = class(TCustomCheckBoxThemed)
  published
    property Action;
    property Align;
    property Alignment;
    property AllowGrayed;
    property Anchors;
    property AutoSize default True;
    property BiDiMode;
    property BorderSpacing;
    property Caption;
    property Checked;
    property Color;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property Left;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property State;
    property TabOrder;
    property TabStop default True;
    property Top;
    property Visible;
    property Width;
    property OnChangeBounds;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;

implementation

{ TCheckBoxThemedActionLink }

procedure TCheckBoxThemedActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClientCheckBoxThemed := AClient as TCustomCheckBoxThemed;
end;

function TCheckBoxThemedActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and
            (FClientCheckBoxThemed.Checked = (Action as TCustomAction).Checked);
end;

procedure TCheckBoxThemedActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then begin
    FClientCheckBoxThemed.CheckFromAction := True;
    try
      FClientCheckBoxThemed.Checked := Value;
    finally
      FClientCheckBoxThemed.CheckFromAction := False;
    end;
  end;
end;

{ TCustomCheckBoxThemed }

constructor TCustomCheckBoxThemed.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AccessibleRole := larCheckBox;
  ControlStyle := ControlStyle  + [csParentBackground, csReplicatable] - [csOpaque]
               - csMultiClicks - [csClickEvents, csNoStdEvents];  { inherited Click not used }
  FAlignment := taRightJustify;
  FAllowGrayed := False;
  AutoSize := True;
  TabStop := True;
end;

procedure TCustomCheckBoxThemed.CalculatePreferredSize(var PreferredWidth,
            PreferredHeight: Integer; WithThemeSpace: Boolean);
var aDetails: TThemedElementDetails;
    aFlags: Cardinal;
    aTextSize, CheckBoxSize: TSize;
begin
  CheckBoxSize := GetCheckBoxSize(Font.PixelsPerInch);
  if Caption <> '' then begin
    aDetails := ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal);
    aFlags := DT_CENTER + DT_VCENTER;
    if IsRightToLeft then inc(aFlags, DT_RTLREADING);
    with ThemeServices.GetTextExtent(Canvas.Handle, aDetails, Caption, aFlags, nil) do begin
      aTextSize.cx := Right;
      aTextSize.cy := Bottom;
    end;
    PreferredWidth := CheckBoxSize.cx + cIndent + aTextSize.cx + cFocusBorder;
    PreferredHeight := Math.max(CheckBoxSize.cy, aTextSize.cy + 2 * cFocusBorder);
  end else begin
    PreferredWidth := CheckBoxSize.cx;
    PreferredHeight := CheckBoxSize.cy;
  end;
end;

procedure TCustomCheckBoxThemed.CMBiDiModeChanged(var Message: TLMessage);
begin
  Invalidate;
end;

procedure TCustomCheckBoxThemed.CMEnabledChanged(var Message: TLMessage);
begin
  if IsEnabled then FCheckBoxHovered := False;
  inherited CMEnabledChanged(Message);
end;

class procedure TCustomCheckBoxThemed.InitCheckBoxSize;
begin
  with ThemeServices do
    FThemeCheckBoxSize := GetDetailSize(GetElementDetails(tbCheckBoxCheckedNormal));
end;

function TCustomCheckBoxThemed.DialogChar(var Message: TLMKey): Boolean;
begin
  Result := False;
  if Message.Msg = LM_SYSCHAR then begin
    if IsEnabled and IsVisible then begin
      if IsAccel(Message.CharCode, Caption) then begin
        DoClick;
        SetFocus;
        Result := True;
      end else
      Result := inherited DialogChar(Message);
    end;
  end;
end;

procedure TCustomCheckBoxThemed.DoClick;
begin
  if AllowGrayed then begin
    case FState of
      cbUnchecked: State := cbGrayed;
      cbGrayed: State := cbChecked;
      cbChecked: State := cbUnchecked;
    end;
  end else
    Checked := not Checked;
end;

procedure TCustomCheckBoxThemed.DoEnter;
begin
  inherited DoEnter;
  Invalidate;
end;

procedure TCustomCheckBoxThemed.DoExit;
begin
  inherited DoExit;
  Invalidate;
end;

function TCustomCheckBoxThemed.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TCheckBoxThemedActionLink;
end;

class function TCustomCheckBoxThemed.GetCheckBoxSize(
  const PixelsPerInch: Integer): TSize;
begin
  if FThemeCheckBoxSize.cx<=0 then
    InitCheckBoxSize;
  Result.cx := MulDiv(FThemeCheckBoxSize.cx, PixelsPerInch, Screen.PixelsPerInch);
  Result.cy := MulDiv(FThemeCheckBoxSize.cy, PixelsPerInch, Screen.PixelsPerInch);
end;

procedure TCustomCheckBoxThemed.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key in [VK_RETURN, VK_SPACE]) and not (ssCtrl in Shift) then begin
    CheckBoxPressed := True;
    Invalidate;
  end;
end;

procedure TCustomCheckBoxThemed.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if (Key in [VK_RETURN, VK_SPACE]) and not (ssCtrl in Shift) then begin
    CheckBoxPressed :=  False;
    DoClick;
  end;
end;

procedure TCustomCheckBoxThemed.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and CheckBoxHovered then begin
    CheckBoxPressed := True;
    Invalidate;
  end;
  SetFocus;
end;

procedure TCustomCheckBoxThemed.MouseEnter;
begin
  inherited MouseEnter;
  CheckBoxHovered := True;
end;

procedure TCustomCheckBoxThemed.MouseLeave;
begin
  inherited MouseLeave;
  CheckBoxHovered := False;
end;

procedure TCustomCheckBoxThemed.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then begin
    if PtInRect(ClientRect, Point(X, Y)) then DoClick;
    CheckBoxPressed := False;
  end;
end;

class procedure TCustomCheckBoxThemed.PaintSelf(ACanvas: TCanvas;
  ACaption: string; ARect: TRect; AState: TCheckBoxState; ARightToLeft,
  AHovered, APressed, AFocused: Boolean; AAlignment: TLeftRight;
  AEnabled: Boolean);
var aCaptionPoint, aCheckBoxPoint: TPoint;
    aDetails: TThemedElementDetails;
    aFlags: Cardinal;
    aHelpRect: TRect;
    aTextSize, CheckBoxSize: TSize;         { Hovered,     Pressed,     State }
const caEnabledDetails: array [False..True, False..True, cbUnchecked..cbGrayed] of TThemedButton =
  (((tbCheckBoxUncheckedNormal, tbCheckBoxCheckedNormal, tbCheckBoxMixedNormal),
    (tbCheckBoxUncheckedPressed, tbCheckBoxCheckedPressed, tbCheckBoxMixedPressed)),
   ((tbCheckBoxUncheckedHot, tbCheckBoxCheckedHot, tbCheckBoxMixedHot),
    (tbCheckBoxUncheckedPressed, tbCheckBoxCheckedPressed, tbCheckBoxMixedPressed)));
const caDisabledDetails: array [cbUnchecked..cbGrayed] of TThemedButton =
  (tbCheckBoxUncheckedDisabled, tbCheckBoxCheckedDisabled, tbCheckBoxMixedDisabled);
begin
  CheckBoxSize := GetCheckBoxSize(ACanvas.Font.PixelsPerInch);
  { Calculate }
  if AEnabled then
    aDetails := ThemeServices.GetElementDetails(caEnabledDetails[AHovered, False, AState])
  else
    aDetails := ThemeServices.GetElementDetails(caDisabledDetails[AState]);
  if ACaption <> '' then begin
    aFlags := DT_CENTER + DT_VCENTER;
    if ARightToLeft then inc(aFlags, DT_RTLREADING);
    with ThemeServices.GetTextExtent(ACanvas.Handle, aDetails, ACaption, aFlags, nil) do begin
      aTextSize.cx := Right;
      aTextSize.cy := Bottom;
    end;
    aCaptionPoint.Y := (ARect.Bottom + ARect.Top - aTextSize.cy) div 2;
    aCheckBoxPoint.Y := (ARect.Bottom + ARect.Top - CheckBoxSize.cy) div 2;
    if ARightToLeft xor (AAlignment = taLeftJustify) then begin  { Caption is on the Left }
      aCheckBoxPoint.X := ARect.Right - CheckBoxSize.cx;
      aCaptionPoint.X := ARect.Left;
    end else begin  { Caption is on the Right }
      aCheckBoxPoint.X := ARect.Left;
      aCaptionPoint.X := aCheckBoxPoint.X + cIndent + CheckBoxSize.cx;
    end;
  end else begin
    if not ARightToLeft then
      aCheckBoxPoint.X := ARect.Left
    else
      aCheckBoxPoint.X := ARect.Right - CheckBoxSize.cx;
    aCheckBoxPoint.Y := (ARect.Bottom - CheckBoxSize.cy) div 2;
  end;
  { Paint Caption }
  if ACaption <> '' then begin
    aHelpRect := Rect(aCaptionPoint.X, aCaptionPoint.Y,
      aCaptionPoint.X + aTextSize.cx, aCaptionPoint.Y + aTextSize.cy);
    ThemeServices.DrawText(ACanvas, aDetails, ACaption, aHelpRect, aFlags, 0);
    { Paint FocusRect around Caption }
    if AFocused then begin
      dec(aHelpRect.Left, cFocusBorder);
      inc(aHelpRect.Right, cFocusBorder);
      LCLIntf.SetBkColor(ACanvas.Handle, ColorToRGB(clBtnFace));
      LCLIntf.DrawFocusRect(ACanvas.Handle, aHelpRect);
    end;
  end;
  { Paint CheckBox }
  if AEnabled then
    aDetails := ThemeServices.GetElementDetails(caEnabledDetails[AHovered, APressed, AState])
  else
    aDetails := ThemeServices.GetElementDetails(caDisabledDetails[AState]);
  aHelpRect := Rect(aCheckBoxPoint.X, aCheckBoxPoint.Y,
    aCheckBoxPoint.X + CheckBoxSize.cx, aCheckBoxPoint.Y + CheckBoxSize.cy);
  ThemeServices.DrawElement(ACanvas.Handle, aDetails, aHelpRect);
end;

procedure TCustomCheckBoxThemed.Paint;
begin
  inherited Paint;
  PaintSelf(Canvas, Caption, ClientRect, State, IsRightToLeft, CheckBoxHovered,
    CheckBoxPressed, Focused, Alignment, IsEnabled);
end;

procedure TCustomCheckBoxThemed.TextChanged;
begin
  inherited TextChanged;
  Invalidate;
end;

procedure TCustomCheckBoxThemed.WMSize(var Message: TLMSize);
begin
  inherited WMSize(Message);
  Invalidate;
end;

{ Setters }

function TCustomCheckBoxThemed.GetChecked: Boolean;
begin
  Result := (FState = cbChecked);
end;

procedure TCustomCheckBoxThemed.SetAlignment(AValue: TLeftRight);
begin
  if FAlignment = AValue then exit;
  FAlignment := AValue;
  Invalidate;
end;

procedure TCustomCheckBoxThemed.SetCheckBoxHovered(AValue: Boolean);
begin
  if FCheckBoxHovered = AValue then exit;
  FCheckBoxHovered := AValue;
  Invalidate;
end;

procedure TCustomCheckBoxThemed.SetChecked(AValue: Boolean);
begin
  if AValue then
    State := cbChecked
  else
    State := cbUnChecked;
end;

procedure TCustomCheckBoxThemed.SetState(AValue: TCheckBoxState);
begin
  if FState = AValue then exit;
  FState := AValue;
  if [csLoading, csDestroying, csDesigning]*ComponentState = [] then begin
    if Assigned(OnEditingDone) then OnEditingDone(self);
    if Assigned(OnChange) then OnChange(self);
    { Execute only when Action.Checked is changed }
    if not CheckFromAction then begin
      if Assigned(OnClick) then
        if not (Assigned(Action) and
          CompareMethods(TMethod(Action.OnExecute), TMethod(OnClick)))
          then OnClick(self);
      if (Action is TCustomAction) and
        (TCustomAction(Action).Checked <> (AValue = cbChecked))
        then ActionLink.Execute(self);
    end;
  end;
  Invalidate;
end;

end.


