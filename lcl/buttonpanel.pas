{
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
unit ButtonPanel;

{$mode objfpc}{$h+}

interface

uses
  Types, SysUtils, Classes, Controls, ExtCtrls, Buttons, Forms, GraphType,
  Graphics, LMessages, LCLStrConsts, Themes;

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
    // the translation of the IDE at designtime is used default item
    property Caption stored True;
    property Enabled;
    property Font;
    property Glyph;
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
  protected
    procedure CalculatePreferredSize(
                         var PreferredWidth, PreferredHeight: integer;
                         WithThemeSpace: Boolean); override;
    function CreateControlBorderSpacing: TControlBorderSpacing; override;
    function CustomAlignInsertBefore(AControl1, AControl2: TControl): Boolean; override;
    procedure CustomAlignPosition(AControl: TControl; var ANewLeft, ANewTop, ANewWidth,
                                  ANewHeight: Integer; var AlignRect: TRect;
                                  AlignInfo: TAlignInfo); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAlign(Value: TAlign); override;
    procedure CMAppShowBtnGlyphChanged(var Message: TLMessage); message CM_APPSHOWBTNGLYPHCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Align default alBottom;
    property AutoSize default True;

    property OKButton: TPanelBitBtn read FButtons[pbOK] stored False;
    property HelpButton: TPanelBitBtn read FButtons[pbHelp] stored False;
    property CloseButton: TPanelBitBtn read FButtons[pbClose] stored False;
    property CancelButton: TPanelBitBtn read FButtons[pbCancel] stored False;
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
    property OKButton;
    property HelpButton;
    property CloseButton;
    property CancelButton;
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
    property OnMouseMove;
    property OnMouseUp;
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

constructor TPanelBitBtn.Create(AOwner: TComponent);
begin
  inherited;

  SetSubComponent(True);
end;

procedure TCustomButtonPanel.DoShowButtons;
var
  btn: TPanelButton;
begin
  for btn := Low(btn) to High(btn) do
  begin
    if FButtons[btn] = nil
    then CreateButton(btn);

    if btn in FShowButtons
    then begin
      FButtons[btn].Visible := True;
      FButtons[btn].Enabled := True;
    end
    else begin
      FButtons[btn].Visible := False;
      FButtons[btn].Enabled := False;
    end;
  end;

  UpdateButtonOrder;
end;

procedure TCustomButtonPanel.SetShowButtons(Value: TPanelButtons);
begin
  if FShowButtons = Value then
    Exit;

  FShowButtons := Value;

  DoShowButtons;
end;

procedure TCustomButtonPanel.DoShowGlyphs;
var
  btn: TPanelButton;
begin
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
end;

procedure TCustomButtonPanel.SetShowGlyphs(Value: TPanelButtons);
begin
  if FShowGlyphs = Value then Exit;
  FShowGlyphs := Value;
  DoShowGlyphs;
end;

procedure TCustomButtonPanel.SetSpacing(AValue: TSpacingSize);
begin
  if FSpacing = AValue then Exit;
  FSpacing := AValue;
  ReAlign;
end;

procedure TCustomButtonPanel.UpdateBevel;
begin
  if FBevel = nil then Exit;

  case Align of
    alTop:   FBevel.Shape := bsBottomLine;
    alLeft:  FBevel.Shape := bsRightLine;
    alRight: FBevel.Shape := bsLeftLine;
  else
    // default to bottom
    FBevel.Shape := bsTopLine;
  end;

  if Align in [alLeft, alRight]
  then FBevel.Width := 2
  else FBevel.Height := 2;
end;

procedure TCustomButtonPanel.UpdateSizes;
var
  btn: TPanelButton;
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

  for btn := Low(btn) to High(btn) do
  begin
    if FButtons[btn] = nil then Continue;
    FButtons[btn].CalculatePreferredSize(BtnWidth, BtnHeight, True);
    if Align in [alTop, alBottom] then
      FButtons[btn].Width := BtnWidth;
    if Align in [alLeft, alRight] then
      FButtons[btn].Height := BtnHeight;
    if BtnWidth > FButtonsWidth then
      FButtonsWidth := BtnWidth;
    if BtnHeight > FButtonsHeight then
      FButtonsHeight := BtnHeight;
  end;
end;

procedure TCustomButtonPanel.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  if HandleAllocated then
  begin
    UpdateSizes;
    if Align in [alTop, alBottom] then
    begin
      PreferredHeight := FButtonsHeight;
      if ShowBevel then
        inc(PreferredHeight, Spacing + FBevel.Height);
    end
    else
    if Align in [alLeft, alRight] then
    begin
      PreferredWidth := FButtonsWidth;
      if ShowBevel then
        inc(PreferredWidth, Spacing + FBevel.Width);
    end;
    ReAlign;
  end;
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
  Realign;
end;

procedure TCustomButtonPanel.SetAlign(Value: TAlign);
begin
  inherited SetAlign(Value);
  UpdateBevel;
  UpdateSizes;
  Realign;
end;

procedure TCustomButtonPanel.CMAppShowBtnGlyphChanged(var Message: TLMessage);
begin
  NotifyControls(Message.msg);
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

  FBevel := TBevel.Create(Self);
  FBevel.Parent := Self;
  FBevel.Name   := 'Bevel';
  FBevel.Align := alCustom;

  UpdateBevel;
end;

procedure TCustomButtonPanel.Loaded;
begin
  inherited Loaded;

  Realign;
end;

procedure TCustomButtonPanel.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  btn: TPanelButton;
begin
  if Operation=opRemove
  then begin
    for btn := Low(btn) to High(btn) do
    begin
      if FButtons[btn] <> AComponent then Continue;
      FButtons[btn] := nil;
      Exclude(FShowButtons, btn);
    end;
  end;
  inherited Notification(AComponent, Operation);
  UpdateSizes;
end;

constructor TCustomButtonPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOwnedChildsSelectable];

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

  function GetCaption(Btn: TPanelButton): string;
  begin
    case Btn of
      pbOK: Result:=rsMbOK;
      pbCancel: Result:=rsMbCancel;
      pbClose: Result:=rsMbClose;
      pbHelp: Result:=rsMbHelp;
    else
      Result:='?';
    end;
  end;

var
  Details: TThemedElementDetails;
  DefButtonSize: TSize;
begin
  if FButtons[AButton] <> nil then Exit;

  Details := ThemeServices.GetElementDetails(tbPushButtonNormal);
  DefButtonSize := ThemeServices.GetDetailSize(Details);

  FButtons[AButton] := TPanelBitBtn.Create(Self);
  with FButtons[AButton] do
  begin
    Name     := NAMES[AButton];
    Parent   := Self;
    Kind     := KINDS[AButton];
    Constraints.MinWidth := DefButtonSize.cx;
    Constraints.MinHeight := DefButtonSize.cy;
    Caption  := GetCaption(AButton);
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
    if AButton = FDefaultButton
    then Default := True;
  end;
end;

function TCustomButtonPanel.CreateControlBorderSpacing: TControlBorderSpacing;
begin
  Result := TControlBorderSpacing.Create(Self, @DEFAULT_BUTTONPANEL_BORDERSPACING);
end;

function TCustomButtonPanel.CustomAlignInsertBefore(AControl1, AControl2: TControl): Boolean;
begin
  if AControl1 = FBevel then Exit(True);
  if AControl2 = FBevel then Exit(False);

  Result := TWincontrol(AControl2).TabOrder > TWincontrol(AControl1).TabOrder;
end;

procedure TCustomButtonPanel.CustomAlignPosition(AControl: TControl; var ANewLeft, ANewTop,
  ANewWidth, ANewHeight: Integer; var AlignRect: TRect; AlignInfo: TAlignInfo);

  function GetPrev: TControl;
  var
    Index: Integer;
  begin
    Index := AlignInfo.ControlIndex;
    while Index > 0 do
    begin
       dec(Index);
       Result := TControl(AlignInfo.AlignList[Index]);
       if Result.Visible then
         Exit;
    end;
    Result := nil;
  end;

var
  Prev: TControl;
begin
  if AControl = FBevel
  then begin
    case Align of
      alTop: begin
        ANewTop := AlignRect.Top + FSpacing + FButtonsHeight;
        ANewLeft := AlignRect.Left;
        ANewWidth := AlignRect.Right - AlignRect.Left;
      end;
      alLeft: begin
        ANewTop := AlignRect.Top;
        ANewLeft := AlignRect.Left + FSpacing + FButtonsWidth;
        ANewHeight := AlignRect.Bottom - AlignRect.Top;
      end;
      alRight: begin
        ANewTop := AlignRect.Top;
        ANewLeft := AlignRect.Right - ANewWidth - FSpacing - FButtonsWidth;
        ANewHeight := AlignRect.Bottom - AlignRect.Top;
      end;
    else
      // bottom, none and custom
      ANewTop := AlignRect.Bottom - ANewHeight - FSpacing - FButtonsHeight;
      ANewLeft := AlignRect.Left;
      ANewWidth := AlignRect.Right - AlignRect.Left;
    end;

    Exit;
  end;

  if (csDesigning in ComponentState) and not AControl.Visible
  then begin
    // when designing, hide doesn't work, so position button outside panel
    ANewLeft := -ANewWidth - 100;
    Exit;
  end;

  // make all buttons the same height
  ANewHeight := FButtonsHeight;
  if Align in [alLeft, alRight]
  then begin
    ANewWidth := FButtonsWidth;

    if AControl = FButtons[pbHelp]
    then begin
      ANewTop := AlignRect.Bottom - ANewHeight;
    end
    else begin
      Prev := GetPrev;
      if Prev = nil then
        ANewTop := AlignRect.Top
      else
        ANewTop := Prev.Top + Prev.Height + FSpacing;
    end;
    if Align = alLeft
    then ANewLeft := AlignRect.Left
    else ANewLeft := AlignRect.Right - ANewWidth;
  end
  else begin
    if AControl = FButtons[pbHelp]
    then begin
      ANewLeft := 0
    end
    else begin
      Prev := GetPrev;
      if Prev = nil then
        ANewLeft := AlignRect.Right - ANewWidth
      else
        ANewLeft := Prev.Left - ANewWidth - FSpacing;
    end;
    if Align = alTop
    then ANewTop := AlignRect.Top
    else ANewTop := AlignRect.Bottom - ANewHeight;
  end;
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
