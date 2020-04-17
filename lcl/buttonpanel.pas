{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit ButtonPanel;

{$mode objfpc}{$h+}

interface

uses
  Math, Types, SysUtils, Classes, LCLProc,Controls, ExtCtrls, StdCtrls, Buttons,
  Forms, GraphType, Graphics, LMessages, Themes, LCLType;

type
  TButtonOrder  = (boDefault, boCloseCancelOK, boCloseOKCancel);
  TPanelButton  = (pbOK, pbCancel, pbClose, pbHelp);
  TPanelButtons = set of TPanelButton;

const
  DefShowButtons = [pbOK, pbCancel, pbClose, pbHelp];
  DefShowGlyphs = [pbOK, pbCancel, pbClose, pbHelp];

type

  { TPanelBitBtn }

  TPanelBitBtn = class(TCustomBitBtn)
  public
    constructor Create(AOwner: TComponent); override;
  published
    // Caption is stored only if DefaultCaption = false
    property Caption stored IsCaptionStored;
    property DefaultCaption stored True;
    property Left stored False;
    property Top stored False;
    property Width stored False;
    property Height stored False;
    property Enabled;
    property Font;
    property Glyph;
    property Name stored True;
    property PopupMenu;
    property ShowHint;
    property OnClick;
  end;

  { TCustomButtonPanel }

  TCustomButtonPanel = class(TCustomPanel)
  private
    FShowBevel: Boolean;
    FShowButtons: TPanelButtons;
    FShowGlyphs: TPanelButtons;
    FBevel: TBevel;
    FGlyphs: array[TPanelButton] of TBitmap;
    FButtons: array[TPanelButton] of TPanelBitBtn;
    FButtonsWidth: Integer;
    FButtonsHeight: Integer;
    FButtonOrder: TButtonOrder;
    FDefaultButton: TPanelButton;
    FSpacing: TSpacingSize;
    procedure CreateButton(AButton: TPanelButton);
    procedure DoDefaultButton;
    procedure DoShowButtons;
    procedure DoShowGlyphs;
    procedure SetButtonOrder(Value: TButtonOrder);
    procedure SetDefaultButton(Value: TPanelButton);
    procedure SetShowBevel(AValue: Boolean);
    procedure SetShowButtons(Value: TPanelButtons);
    procedure SetShowGlyphs(Value: TPanelButtons);
    procedure SetSpacing(AValue: TSpacingSize);
    procedure UpdateBevel;
    procedure UpdateButtonOrder;
    procedure UpdateSizes;
    procedure UpdateButtonLayout;
    procedure UpdateButtonSize;
    function IsLastButton(AControl: TControl): boolean;
  protected
    function CreateControlBorderSpacing: TControlBorderSpacing; override;
    function CustomAlignInsertBefore(AControl1, AControl2: TControl): Boolean; override;
    procedure CustomAlignPosition(AControl: TControl; var ANewLeft, ANewTop,
      ANewWidth, ANewHeight: Integer; var AlignRect: TRect;
      AlignInfo: TAlignInfo); override;
    procedure CalculatePreferredSize(var PreferredWidth,
      PreferredHeight: integer; WithThemeSpace: Boolean); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAlign(Value: TAlign); override;
    procedure CMAppShowBtnGlyphChanged(var Message: TLMessage); message CM_APPSHOWBTNGLYPHCHANGED;
    procedure CMShowingChanged(var Message: TLMessage); message CM_SHOWINGCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Align default alBottom;
    property AutoSize default True;

    property OKButton: TPanelBitBtn read FButtons[pbOK] default nil;
    property HelpButton: TPanelBitBtn read FButtons[pbHelp] default nil;
    property CloseButton: TPanelBitBtn read FButtons[pbClose] default nil;
    property CancelButton: TPanelBitBtn read FButtons[pbCancel] default nil;
    property ButtonOrder: TButtonOrder read FButtonOrder write SetButtonOrder default boDefault;

    property DefaultButton: TPanelButton read FDefaultButton write SetDefaultButton default pbOK;
    property ShowButtons: TPanelButtons read FShowButtons write SetShowButtons default DefShowButtons;
    property ShowGlyphs: TPanelButtons read FShowGlyphs write SetShowGlyphs default DefShowGlyphs;
    property ShowBevel: Boolean read FShowBevel write SetShowBevel default True;
    property Spacing: TSpacingSize read FSpacing write SetSpacing default 6;
  published
  end;

  { TButtonPanel }

  TButtonPanel = class(TCustomButtonPanel)
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Constraints;
    property Enabled;
    property OKButton;
    property HelpButton;
    property CloseButton;
    property CancelButton;
    property Color;
    property ButtonOrder;
    property TabOrder;
    property DefaultButton;
    property Spacing;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
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
    property OnUTF8KeyPress;
    property ShowButtons;
    property ShowGlyphs;
    property ShowBevel;
    property Visible;
  end;

procedure Register;

implementation

const
  DEFAULT_BUTTONPANEL_BORDERSPACING: TControlBorderSpacingDefault = (
    Left:0; Top:0; Right:0; Bottom:0; Around:6;
  );

procedure Register;
begin
  RegisterComponents('Misc', [TButtonPanel]);
end;

{ TPanelBitBtn }

constructor TPanelBitBtn.Create(AOwner: TComponent);
begin
  inherited;

  SetSubComponent(True);
end;

{ TCustomButtonPanel }

procedure TCustomButtonPanel.DoShowButtons;
var
  btn: TPanelButton;
  aButton: TPanelBitBtn;
begin
  DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TCustomButtonPanel.DoShowButtons'){$ENDIF};

  for btn := Low(btn) to High(btn) do
  begin
    if FButtons[btn] = nil
    then CreateButton(btn);
    aButton:=FButtons[btn];

    if btn in FShowButtons
    then begin
      if csDesigning in ComponentState then
        aButton.ControlStyle:=aButton.ControlStyle-[csNoDesignVisible];
      aButton.Visible := True;
    end
    else begin
      if csDesigning in ComponentState then
        aButton.ControlStyle:=aButton.ControlStyle+[csNoDesignVisible];
      aButton.Visible := False;
    end;
  end;

  UpdateButtonOrder;
  UpdateButtonLayout;
  EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TCustomButtonPanel.DoShowButtons'){$ENDIF};
end;

procedure TCustomButtonPanel.SetShowButtons(Value: TPanelButtons);
begin
  if FShowButtons = Value then
    Exit;

  FShowButtons := Value;
  InvalidatePreferredSize;
  DoShowButtons;
end;

procedure TCustomButtonPanel.DoShowGlyphs;
var
  btn: TPanelButton;
begin
  DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TCustomButtonPanel.DoShowGlyphs'){$ENDIF};
  for btn := Low(btn) to High(btn) do
  begin
    if FButtons[btn] = nil then Continue;

    if btn in FShowGlyphs
    then begin
      FButtons[btn].Glyph.Assign(FGlyphs[btn]);
    end
    else begin
      FGlyphs[btn].Assign(FButtons[btn].Glyph);
      FButtons[btn].Glyph.Assign(nil);
    end;
  end;
  EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TCustomButtonPanel.DoShowGlyphs'){$ENDIF};
end;

procedure TCustomButtonPanel.SetShowGlyphs(Value: TPanelButtons);
begin
  if FShowGlyphs = Value then Exit;
  FShowGlyphs := Value;
  InvalidatePreferredSize;
  DoShowGlyphs;
end;

procedure TCustomButtonPanel.SetSpacing(AValue: TSpacingSize);
begin
  if FSpacing = AValue then Exit;
  FSpacing := AValue;
  InvalidatePreferredSize;
  ReAlign;
end;

procedure TCustomButtonPanel.UpdateBevel;
begin
  if FBevel = nil then Exit;

  case Align of
    alTop:
      begin
        FBevel.Shape := bsBottomLine;
        FBevel.Align := alBottom;
      end;
    alLeft:
      begin
        FBevel.Shape := bsRightLine;
        FBevel.Align := alRight;
      end;
    alRight:
      begin
        FBevel.Shape := bsLeftLine;
        FBevel.Align := alLeft;
      end
  else
    // default to bottom
    FBevel.Shape := bsTopLine;
    FBevel.Align := alTop;
  end;

  if Align in [alLeft, alRight]
  then FBevel.Width := 2
  else FBevel.Height := 2;
end;

procedure TCustomButtonPanel.UpdateSizes;
var
  i: Integer;
  BtnWidth, BtnHeight: Integer;
  Details: TThemedElementDetails;
  DefButtonSize: TSize;
begin
  if csDestroying in ComponentState then
    Exit;

  Details := ThemeServices.GetElementDetails(tbPushButtonNormal);
  DefButtonSize := ThemeServices.GetDetailSize(Details);
  FButtonsWidth := DefButtonSize.cx;
  FButtonsHeight := DefButtonSize.cy;

  for i := 0 to ControlCount - 1 do
  begin
    if not (Controls[i] is TCustomButton) then Continue;
    Controls[i].GetPreferredSize(BtnWidth, BtnHeight, True);
    if Align in [alTop, alBottom] then
      Controls[i].Width := BtnWidth;
    if Align in [alLeft, alRight] then
      Controls[i].Height := BtnHeight;
    if BtnWidth > FButtonsWidth then
      FButtonsWidth := BtnWidth;
    if BtnHeight > FButtonsHeight then
      FButtonsHeight := BtnHeight;
  end;
end;

procedure TCustomButtonPanel.UpdateButtonLayout;
var
  aButton: TPanelBitBtn;
  btn: TPanelButton;
begin
  for btn := Low(TPanelButton) to High(TPanelButton) do
  begin
    aButton:=FButtons[btn];
    if aButton = nil then Continue;
    aButton.Align := alCustom;
    aButton.Default := FDefaultButton = btn;
  end;
end;

function TCustomButtonPanel.IsLastButton(AControl: TControl): boolean;
// returns true if AControl is the right/bottommost of the TPanelBitBtn
// Note: pbHelp could be the only button, then it is the last button
var
  i: TPanelButton;
begin
  if not AControl.IsControlVisible then exit(false);
  if not (AControl is TPanelBitBtn) then exit(false);
  for i:=low(FButtons) to pred(high(FButtons)) do
    if (FButtons[i]<>nil) and FButtons[i].IsControlVisible
    and (FButtons[i].TabOrder>TPanelBitBtn(AControl).TabOrder) then
      exit(false); // there is a higher one
  Result:=true;
end;

procedure TCustomButtonPanel.UpdateButtonOrder;
const
  TabOrders: array[TButtonOrder, 0..3] of TPanelButton = (
    {$IFDEF UNIX}
    {boDefault      } (pbOK, pbCancel, pbClose, pbHelp),
    {$ELSE}
    {boDefault      } (pbCancel, pbOK, pbClose, pbHelp),
    {$ENDIF}
    {boCloseCancelOK} (pbOK, pbCancel, pbClose, pbHelp),
    {boCloseOKCancel} (pbCancel, pbOK, pbClose, pbHelp)
  );
var
  i: Integer;
begin
  //set taborder
  for i := Low(TabOrders[FButtonOrder]) to High(TabOrders[FButtonOrder]) do
  begin
    if FButtons[TabOrders[FButtonOrder, i]] = nil then Continue;
    FButtons[TabOrders[FButtonOrder, i]].TabOrder := High(TabOrders[FButtonOrder]) - i;
  end;
  AdjustSize;
end;

procedure TCustomButtonPanel.UpdateButtonSize;
var
  AParent: TCustomDesignControl;
  Details: TThemedElementDetails;
  DefButtonSize: TSize;
  btn: TPanelBitBtn;
begin
  AParent := GetParentDesignControl(Self);
  if AParent=nil then
    Exit;

  Details := ThemeServices.GetElementDetails(tbPushButtonNormal);
  DefButtonSize := ThemeServices.GetDetailSize(Details);

  DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TCustomButtonPanel.UpdateButtonSize'){$ENDIF};
  try
    for btn in FButtons do
    begin
      if btn = nil then Continue;
      if Application.Scaled and AParent.Scaled then
      begin
        btn.Constraints.MinWidth := MulDiv(DefButtonSize.cx, AParent.PixelsPerInch, ScreenInfo.PixelsPerInchX);
        btn.Constraints.MinHeight := MulDiv(DefButtonSize.cy, AParent.PixelsPerInch, ScreenInfo.PixelsPerInchY);
      end else
      begin
        btn.Constraints.MinWidth := DefButtonSize.cx;
        btn.Constraints.MinHeight := DefButtonSize.cy;
      end;
    end;
  finally
    EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TCustomButtonPanel.UpdateButtonSize'){$ENDIF};
  end;
end;

procedure TCustomButtonPanel.SetAlign(Value: TAlign);
begin
  DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TCustomButtonPanel.SetAlign'){$ENDIF};
  try
    inherited SetAlign(Value);
    UpdateButtonLayout;
    UpdateBevel;
    UpdateSizes;
  finally
    EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TCustomButtonPanel.SetAlign'){$ENDIF};
  end;
end;

procedure TCustomButtonPanel.CMAppShowBtnGlyphChanged(var Message: TLMessage);
begin
  NotifyControls(Message.msg);
end;

procedure TCustomButtonPanel.CMShowingChanged(var Message: TLMessage);
begin
  inherited;

  UpdateButtonSize;
end;

procedure TCustomButtonPanel.SetButtonOrder(Value: TButtonOrder);
begin
  if FButtonOrder = Value then Exit;
  FButtonOrder := Value;
  UpdateButtonOrder;
end;

procedure TCustomButtonPanel.DoDefaultButton;
var
  btn: TPanelButton;
begin
  for btn := Low(btn) to High(btn) do
  begin
    if FButtons[btn] = nil then Continue;
    FButtons[btn].Default := FDefaultButton = btn;
  end;
end;

procedure TCustomButtonPanel.SetDefaultButton(Value: TPanelButton);
begin
  if FDefaultButton = Value then
    Exit;

  FDefaultButton := Value;

  DoDefaultButton;
end;

procedure TCustomButtonPanel.SetShowBevel(AValue: Boolean);
begin
  if FShowBevel = AValue then exit;
  FShowBevel := AValue;

  if not FShowBevel
  then begin
    FreeAndNil(FBevel);
    Exit;
  end;

  DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TCustomButtonPanel.SetShowBevel'){$ENDIF};
  try
    FBevel := TBevel.Create(Self);
    FBevel.Parent := Self;
    FBevel.Name   := 'Bevel';

    UpdateBevel;
  finally
    EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TCustomButtonPanel.SetShowBevel'){$ENDIF};
  end;
end;

procedure TCustomButtonPanel.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  btn: TPanelButton;
begin
  if Operation=opRemove then
  begin
    for btn := Low(btn) to High(btn) do
    begin
      if FButtons[btn] <> AComponent then Continue;
      FButtons[btn] := nil;
      Exclude(FShowButtons, btn);
    end;
  end;
  inherited Notification(AComponent, Operation);
  if AComponent is TPanelBitBtn then
    UpdateSizes;
end;

constructor TCustomButtonPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOwnedChildrenNotSelectable];

  Align      := alBottom;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  Caption    := '';
  ControlStyle := ControlStyle - [csSetCaption];
  AutoSize   := True;
  FSpacing   := 6;
  ShowBevel  := True;


  FDefaultButton := pbOK;
  FButtonOrder   := boDefault;
  FShowButtons   := DefShowButtons;
  FShowGlyphs    := DefShowGlyphs;

  // create the buttons
  DoShowButtons;
end;

procedure TCustomButtonPanel.CreateButton(AButton: TPanelButton);
const
  NAMES: array[TPanelButton] of String = (
    'OKButton', 'CancelButton', 'CloseButton', 'HelpButton'
  );
  KINDS: array[TPanelButton] of TBitBtnKind = (
    bkOK, bkCancel, bkClose, bkHelp
  );
begin
  if FButtons[AButton] <> nil then Exit;

  FButtons[AButton] := TPanelBitBtn.Create(Self);
  with FButtons[AButton] do
  begin
    Name     := NAMES[AButton];
    Kind     := KINDS[AButton];
    AutoSize := true;
    TabOrder := Ord(AButton); //initial order
    Align    := alCustom;
    if FGlyphs[AButton] = nil
    then begin
      // first time
      FGlyphs[AButton] := TBitmap.Create;
      FGlyphs[AButton].Assign(Glyph);
    end;
    // (re)set the glyph if needed
    if (AButton in FShowGlyphs)
    then Glyph.Assign(FGlyphs[AButton])
    else Glyph.Assign(nil);
    // set default
    Default  := AButton = FDefaultButton;

    Parent   := Self;
  end;
end;

function TCustomButtonPanel.CreateControlBorderSpacing: TControlBorderSpacing;
begin
  Result := TControlBorderSpacing.Create(Self, @DEFAULT_BUTTONPANEL_BORDERSPACING);
end;

function TCustomButtonPanel.CustomAlignInsertBefore(AControl1, AControl2: TControl): Boolean;
begin
  // bevel is always the very first
  if AControl1 = FBevel then Exit(True);
  if AControl2 = FBevel then Exit(False);
  // the help button is the second
  if AControl1 = FButtons[pbHelp] then Exit(True);
  if AControl2 = FButtons[pbHelp] then Exit(False);
  // user defined controls comes before the normal buttons
  if (not (AControl1 is TPanelBitBtn)) and (AControl2 is TPanelBitBtn) then
    Exit(True)
  else if (AControl1 is TPanelBitBtn) and (not (AControl2 is TPanelBitBtn)) then
    Exit(False);
  // sort for taborder
  Result := TWinControl(AControl2).TabOrder > TWinControl(AControl1).TabOrder;
end;

procedure TCustomButtonPanel.CustomAlignPosition(AControl: TControl;
  var ANewLeft, ANewTop, ANewWidth, ANewHeight: Integer; var AlignRect: TRect;
  AlignInfo: TAlignInfo);
var
  BevelSpacing: TSpacingSize;
begin
  //debugln(['TCustomButtonPanel.CustomAlignPosition ',DbgSName(Self),' AControl=',DbgSName(AControl),' AlignRect=',dbgs(AlignRect),' New=',ANewLeft,',',ANewTop,',',ANewWidth,'x',ANewHeight]);
  inherited CustomAlignPosition(AControl, ANewLeft, ANewTop, ANewWidth,
    ANewHeight, AlignRect, AlignInfo);

  if Assigned(FBevel) and FBevel.IsControlVisible then
    BevelSpacing := Spacing
  else
    BevelSpacing := 0;

  if Align in [alLeft,alRight] then
  begin
    // put top or bottom
    ANewLeft:=AlignRect.Left;
    ANewWidth:=AControl.Constraints.MinMaxWidth(AlignRect.Right-ANewLeft-BevelSpacing);
    if Align=alRight then
      inc(ANewLeft,BevelSpacing);
    if AControl=FButtons[pbHelp] then
    begin
      ANewTop:=AlignRect.Top; // no Spacing in front of the first button
      AlignRect.Top:=Min(AlignRect.Bottom,ANewTop+ANewHeight);
    end else begin
      ANewTop:=AlignRect.Bottom-ANewHeight;
      if not IsLastButton(AControl) then
        dec(ANewTop,Spacing);
      AlignRect.Bottom:=Max(AlignRect.Top,ANewTop);
    end;
  end else
  begin
    // put left or right
    ANewTop:=AlignRect.Top;
    ANewHeight:=AControl.Constraints.MinMaxHeight(AlignRect.Bottom-ANewTop-BevelSpacing);
    if Align=alBottom then
      inc(ANewTop,BevelSpacing);
    if AControl=FButtons[pbHelp] then
    begin
      // put left
      ANewLeft:=AlignRect.Left; // no Spacing in front of the first button
      AlignRect.Left:=Min(AlignRect.Right,ANewLeft+ANewWidth);
    end else begin
      // put right
      ANewLeft:=AlignRect.Right-ANewWidth;
      if not IsLastButton(AControl) then
        dec(ANewLeft,Spacing);
      AlignRect.Right:=Max(AlignRect.Left,ANewLeft);
    end;
  end;
  //debugln(['TCustomButtonPanel.CustomAlignPosition END ',DbgSName(Self),' AControl=',DbgSName(AControl),' AlignRect=',dbgs(AlignRect),' New=',ANewLeft,',',ANewTop,',',ANewWidth,'x',ANewHeight]);
end;

procedure TCustomButtonPanel.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var
  i: Integer;
  AControl: TControl;
  MinWidth: Integer;
  MinHeight: Integer;
  CtrlPrefWidth, CtrlPrefHeight: integer;
begin
  MinWidth:=0;
  MinHeight:=0;
  // add buttons
  for i:=0 to ControlCount-1 do
  begin
    AControl:=Controls[i];
    if (AControl.Align<>alCustom) or (not AControl.IsControlVisible) then continue;
    if AControl=FBevel then continue;
    CtrlPrefWidth:=0;
    CtrlPrefHeight:=0;
    AControl.GetPreferredSize(CtrlPrefWidth,CtrlPrefHeight);
    //debugln(['TCustomButtonPanel.CalculatePreferredSize ',DbgSName(AControl),' ',CtrlPrefHeight]);
    if Align in [alLeft,alRight] then
    begin
      inc(MinHeight,CtrlPrefHeight);
      if not IsLastButton(AControl) then
        inc(MinHeight,Spacing);
      MinWidth:=Max(MinWidth,CtrlPrefWidth);
    end
    else begin
      inc(MinWidth,CtrlPrefWidth);
      if not IsLastButton(AControl) then
        inc(MinWidth,Spacing);
      MinHeight:=Max(MinHeight,CtrlPrefHeight);
    end;
  end;
  // bevel
  if (FBevel<>nil) and FBevel.IsControlVisible then
  begin
    if Align in [alLeft,alRight] then
      inc(MinWidth,FBevel.Width+Spacing)
    else
      inc(MinHeight,FBevel.Height+Spacing);
  end;
  PreferredWidth:=MinWidth;
  PreferredHeight:=MinHeight;
  //debugln(['TCustomButtonPanel.CalculatePreferredSize ',DbgSName(Self),' ',PreferredWidth,'x',PreferredHeight]);
end;

destructor TCustomButtonPanel.Destroy;
var
  btn: TPanelButton;
begin
  for btn := Low(btn) to High(btn) do
    FreeAndNil(FGlyphs[btn]);
  inherited Destroy;
end;

end.
