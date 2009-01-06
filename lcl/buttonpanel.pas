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
  Buttons, Classes, LCLProc, Controls, Dialogs, ExtCtrls, Forms, Graphics,
  GraphType, LCLType, SysUtils, LCLStrConsts;

type
  TButtonOrder  = (boDefault, boCloseCancelOK, boCloseOKCancel);
  TPanelButton  = (pbOK, pbCancel, pbClose, pbHelp);
  TPanelButtons = set of TPanelButton;

const
  DefShowButtons = [pbOK, pbCancel, pbClose, pbHelp];
  DefShowGlyphs = [pbOK, pbCancel, pbClose, pbHelp];

type
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
    FButtonOrder: TButtonOrder;
    FDefaultButton: TPanelButton;
    FSpacing: TSpacingSize;
    procedure OrderButtonsRightToLeft(TheButtons: array of TControl);
    procedure ButtonOrderCloseCancelOK;
    procedure ButtonOrderCloseOKCancel;
    procedure CreateButton(AButton: TPanelButton);
    procedure DoButtonOrder;
    procedure DoDefaultButton;
    procedure DoRestoreCancel;
    procedure DoShowButtons;
    procedure DoShowGlyphs;
    procedure SetButtonOrder(Value: TButtonOrder);
    procedure SetDefaultButton(Value: TPanelButton);
    procedure SetShowBevel(AValue: Boolean);
    procedure SetShowButtons(Value: TPanelButtons);
    procedure SetShowGlyphs(Value: TPanelButtons);
    procedure SetSpacing(AValue: TSpacingSize);
    procedure UpdateBevel;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAlign(Value: TAlign); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

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

procedure Register;
begin
  RegisterComponents('Misc', [TButtonPanel]);
end;

constructor TPanelBitBtn.Create(AOwner: TComponent);
begin
  inherited;

  Include(FComponentStyle, csSubComponent);
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
      if btn = pbHelp
      then FButtons[btn].Align := alLeft
      else FButtons[btn].Align := alRight;
    end
    else begin
      FButtons[btn].Visible := False;
      FButtons[btn].Enabled := False;
      FButtons[btn].Align := alNone;
      // when designing, hide doesn't work, so position button outside panel
      if csDesigning in ComponentState
      then begin
        FButtons[btn].Left := -FButtons[btn].Width - 100;
        FButtons[btn].Anchors := [];
      end;
    end;
  end;

  DoButtonOrder;
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
  if FShowGlyphs = Value then
    Exit;

  FShowGlyphs := Value;

  DoShowGlyphs;
end;

procedure TCustomButtonPanel.SetSpacing(AValue: TSpacingSize);
var
  btn: TPanelButton;
begin
  if FSpacing = AValue then Exit;
  FSpacing := AValue;
  for btn := Low(btn) to High(btn) do
  begin
    if FButtons[btn] = nil then Continue;
    FButtons[btn].BorderSpacing.Around := FSpacing;
  end;
  UpdateBevel;
end;

procedure TCustomButtonPanel.UpdateBevel;
begin
  if FBevel = nil then Exit;

  case Align of
    alTop: begin
      FBevel.Shape := bsBottomLine;
      FBevel.Align := alBottom;
    end;
    alLeft: begin
      FBevel.Shape := bsRightLine;
      FBevel.Align := alRight;
    end;
    alRight: begin
      FBevel.Shape := bsLeftLine;
      FBevel.Align := alLeft;
    end;
  else
    // default to bottom
    FBevel.Shape := bsTopLine;
    FBevel.Align := alTop;
  end;


  if Align in [alLeft, alRight]
  then begin
    FBevel.Width := 2;
    FBevel.BorderSpacing.Top := FSpacing;
    FBevel.BorderSpacing.Bottom := FSpacing;
    FBevel.BorderSpacing.Left := 0;
    FBevel.BorderSpacing.Right := 0;
  end
  else begin
    FBevel.Height := 2;
    FBevel.BorderSpacing.Top := 0;
    FBevel.BorderSpacing.Bottom := 0;
    FBevel.BorderSpacing.Left := FSpacing;
    FBevel.BorderSpacing.Right := FSpacing;
  end;
end;

procedure TCustomButtonPanel.DoButtonOrder;
begin
  case FButtonOrder of
    boCloseCancelOK: ButtonOrderCloseCancelOK;
    boCloseOKCancel: ButtonOrderCloseOKCancel;
    else
      //boDefault
      {$IFDEF UNIX}
        ButtonOrderCloseCancelOK;
      {$ELSE}
        ButtonOrderCloseOKCancel;
      {$ENDIF}
  end;
end;

procedure TCustomButtonPanel.OrderButtonsRightToLeft(TheButtons: array of TControl);
 // reorder aligned buttons from left to right.
 // The buttons are Align=alRight. The order is determined by the right edge.
 // Set the Left+Width property to some values in ascending order and the LCL
 // will do the rest.
 function Previous(AIndex: Integer): Integer;
 begin
   Result := AIndex;
   repeat
     Dec(Result)
   until (Result < Low(TheButtons))
      or ((TheButtons[Result] <> nil) and (TheButtons[Result].Align = alRight));
 end;

var
  i, x: integer;
begin
  i := Previous(Length(TheButtons));
  if i < Low(TheButtons) then Exit; // no buttons

  repeat
    x:=TheButtons[i].Left+TheButtons[i].Width;
    i := Previous(i);
    if i < Low(TheButtons) then Exit; // all buttons are already in the correct order
  until TheButtons[i].Left+TheButtons[i].Width >= x;

  DisableAlign;
  try
    x := ClientWidth;
    for i := High(TheButtons) downto Low(TheButtons) do
    begin
      if TheButtons[i]=nil then continue;
      Dec(x, TheButtons[i].Width);
      TheButtons[i].Left := x;
    end;
  finally
    EnableAlign;
  end;
end;

procedure TCustomButtonPanel.SetAlign(Value: TAlign);
begin
  inherited SetAlign(Value);
  UpdateBevel;
end;

procedure TCustomButtonPanel.ButtonOrderCloseCancelOK;
const
  TABORDERS: array[0..3] of TPanelButton = (pbOK, pbCancel, pbClose, pbHelp);
var
  i: Integer;
begin
  OrderButtonsRightToLeft([FButtons[pbClose], FButtons[pbCancel], FButtons[pbOK]]);

  //set taborder
  for i := Low(TABORDERS) to High(TABORDERS) do
  begin
    if FButtons[TABORDERS[i]] = nil then Continue;
    FButtons[TABORDERS[i]].Taborder := i;
  end;
end;

procedure TCustomButtonPanel.ButtonOrderCloseOKCancel;
const
  TABORDERS: array[0..3] of TPanelButton = (pbCancel, pbOK, pbClose, pbHelp);
var
  i: Integer;
begin
  OrderButtonsRightToLeft([FButtons[pbClose], FButtons[pbOK], FButtons[pbCancel]]);

  //set taborder
  for i := Low(TABORDERS) to High(TABORDERS) do
  begin
    if FButtons[TABORDERS[i]] = nil then Continue;
    FButtons[TABORDERS[i]].Taborder := i;
  end;
end;

procedure TCustomButtonPanel.SetButtonOrder(Value: TButtonOrder);
begin
  if FButtonOrder = Value then
    Exit;

  FButtonOrder := Value;

  DoButtonOrder;
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

procedure TCustomButtonPanel.DoRestoreCancel;
begin
  if FButtons[pbCancel] = nil then Exit;

  // to restore cancel button we need to do this hack
  FButtons[pbCancel].Cancel := False;
  FButtons[pbCancel].Cancel := True;
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

  UpdateBevel;
end;

procedure TCustomButtonPanel.Loaded;
begin
  inherited Loaded;

  DoRestoreCancel;
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
  // let the designer decide this
  //BorderSpacing.Left := 6;
  //BorderSpacing.Right := 6;
  FSpacing := 6;
  ShowBevel := True;


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
  CAPTIONS: array[TPanelButton] of String = (
    rsMbOK, rsMbCancel, rsMbClose, rsMbHelp
  );
begin
  if FButtons[AButton] <> nil then Exit;

  FButtons[AButton] := TPanelBitBtn.Create(Self);
  with FButtons[AButton] do
  begin
    Name     := NAMES[AButton];
    Parent   := Self;
    Kind     := KINDS[AButton];
    BorderSpacing.Around := FSpacing;
    AutoSize := True;
    Caption  := CAPTIONS[AButton];
    TabOrder := Ord(AButton); //initial order
    if AButton = pbHelp
    then  Align    := alLeft
    else  Align    := alRight;
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

destructor TCustomButtonPanel.Destroy;
var
  btn: TPanelButton;
begin
  for btn := Low(btn) to High(btn) do
    FreeAndNil(FGlyphs[btn]);
  inherited Destroy;
end;

end.
