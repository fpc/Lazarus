unit ButtonPanel;

{$mode objfpc}{$h+}

interface

uses
  Buttons, Classes, LCLProc, Controls, Dialogs, ExtCtrls, Forms, Graphics,
  GraphType, LCLType, SysUtils;

type
  TButtonOrder  = (boDefault, boCloseCancelOK, boCloseOKCancel);
  TPanelButton  = (pbOK, pbCancel, pbClose, pbHelp);
  TPanelButtons = set of TPanelButton;

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
    FCancelGlyph: TBitmap;
    FCloseGlyph: TBitmap;
    FHelpGlyph:  TBitmap;
    FOKGlyph:    TBitmap;
    FShowButtons: TPanelButtons;
    FShowGlyphs: TPanelButtons;
    FBevel:      TBevel;
    FCancelButton: TPanelBitBtn;
    FCloseButton: TPanelBitBtn;
    FHelpButton: TPanelBitBtn;
    FOKButton:   TPanelBitBtn;
    FButtonOrder: TButtonOrder;
    FDefaultButton: TPanelButton;
    procedure OrderButtonsRightToLeft(TheButtons: array of TControl);
    procedure ButtonOrderCloseCancelOK;
    procedure ButtonOrderCloseOKCancel;
    procedure DoButtonOrder;
    procedure DoDefaultButton;
    procedure DoShowButtons;
    procedure DoShowGlyphs;
    procedure SetButtonOrder(Value: TButtonOrder);
    procedure SetDefaultButton(Value: TPanelButton);
    procedure SetShowButtons(Value: TPanelButtons);
    procedure SetShowGlyphs(Value: TPanelButtons);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property OKButton: TPanelBitBtn Read FOKButton stored False;
    property HelpButton: TPanelBitBtn Read FHelpButton stored False;
    property CloseButton: TPanelBitBtn Read FCloseButton stored False;
    property CancelButton: TPanelBitBtn Read FCancelButton stored False;
    property ButtonOrder: TButtonOrder Read FButtonOrder Write SetButtonOrder;
    property DefaultButton: TPanelButton Read FDefaultButton Write SetDefaultButton;
    property ShowButtons: TPanelButtons Read FShowButtons Write SetShowButtons;
    property ShowGlyphs: TPanelButtons Read FShowGlyphs Write SetShowGlyphs;
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
    property ShowButtons;
    property ShowGlyphs;
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
begin
  FOKButton.Visible     := (pbOK in FShowButtons);
  FCancelButton.Visible := (pbCancel in FShowButtons);
  FCloseButton.Visible  := (pbClose in FShowButtons);
  FHelpButton.Visible   := (pbHelp in FShowButtons);

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
begin
  if not (pbOK in FShowGlyphs) then
  begin
    FOKGlyph.Assign(FOKButton.Glyph);
    FOKButton.Glyph.Assign(nil);
  end
  else
    FOKButton.Glyph.Assign(FOKGlyph);

  if not (pbCancel in FShowGlyphs) then
  begin
    FCancelGlyph.Assign(FCancelButton.Glyph);
    FCancelButton.Glyph.Assign(nil);
  end
  else
    FCancelButton.Glyph.Assign(FCancelGlyph);

  if not (pbClose in FShowGlyphs) then
  begin
    FCloseGlyph.Assign(FCloseButton.Glyph);
    FCloseButton.Glyph.Assign(nil);
  end
  else
    FCloseButton.Glyph.Assign(FCloseGlyph);

  if not (pbHelp in FShowGlyphs) then
  begin
    FHelpGlyph.Assign(FHelpButton.Glyph);
    FHelpButton.Glyph.Assign(nil);
  end
  else
    FHelpButton.Glyph.Assign(FHelpGlyph);
end;

procedure TCustomButtonPanel.SetShowGlyphs(Value: TPanelButtons);
begin
  if FShowGlyphs = Value then
    Exit;

  FShowGlyphs := Value;

  DoShowGlyphs;
end;

procedure TCustomButtonPanel.DoButtonOrder;
begin
  case FButtonOrder of
    boCloseCancelOK: ButtonOrderCloseCancelOK;
    boCloseOKCancel: ButtonOrderCloseOKCancel;
    else
      //boDefault
      {$IFDEF UNIX}
        ButtonOrderCloseOKCancel;
      {$ELSE}
      ButtonOrderCloseCancelOK;
      {$ENDIF}
  end;
end;

procedure TCustomButtonPanel.OrderButtonsRightToLeft(TheButtons: array of TControl);
 // reorder aligned buttons from left to right.
 // The buttons are Align=alRight. The order is determined by the Left property.
 // Set the Left+Wifth property to some values in ascending order and the LCL
 // will do the rest.
var
  i, x: integer;
begin
  i := High(TheButtons);
  while (i > Low(TheButtons)) and (TheButtons[i - 1].Left < TheButtons[i].Left) do
    Dec(i);
  if i = Low(TheButtons) then
    exit;

  DisableAlign;
  try
    x := ClientWidth;
    for i := High(TheButtons) downto Low(TheButtons) do
    begin
      Dec(x, TheButtons[i].Width);
      TheButtons[i].Left := x;
    end;
  finally
    EnableAlign;
  end;
end;

procedure TCustomButtonPanel.ButtonOrderCloseCancelOK;
begin
  OrderButtonsRightToLeft([FCloseButton, FCancelButton, FOKButton]);

  //set taborder
  FOKButton.TabOrder     := 0;
  FCancelButton.TabOrder := 1;
  FCloseButton.TabOrder  := 2;
  FHelpButton.TabOrder   := 3;
end;

procedure TCustomButtonPanel.ButtonOrderCloseOKCancel;
begin
  OrderButtonsRightToLeft([FCloseButton, FOKButton, FCancelButton]);

  //set taborder
  FCancelButton.TabOrder := 0;
  FOKButton.TabOrder     := 1;
  FCloseButton.TabOrder  := 2;
  FHelpButton.TabOrder   := 3;
end;

procedure TCustomButtonPanel.SetButtonOrder(Value: TButtonOrder);
begin
  if FButtonOrder = Value then
    Exit;

  FButtonOrder := Value;

  DoButtonOrder;
end;

procedure TCustomButtonPanel.DoDefaultButton;
begin
  FOKButton.Default     := FDefaultButton = pbOk;
  FCancelButton.Default := FDefaultButton = pbCancel;
  FCloseButton.Default  := FDefaultButton = pbClose;
  FHelpButton.Default   := FDefaultButton = pbHelp;
end;

procedure TCustomButtonPanel.SetDefaultButton(Value: TPanelButton);
begin
  if FDefaultButton = Value then
    Exit;

  FDefaultButton := Value;

  DoDefaultButton;
end;

procedure TCustomButtonPanel.Loaded;
begin
  inherited Loaded;
  DoDefaultButton;
  DoShowGlyphs;
  DoShowButtons;
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
  BorderSpacing.Left := 6;
  BorderSpacing.Right := 6;

  FBevel := TBevel.Create(Self);
  FBevel.Parent := Self;
  with FBevel do
  begin
    Name   := 'Bevel';
    Shape  := bsTopLine;
    Align  := alTop;
    Height := 2;
    BorderSpacing.Left := 6;
    BorderSpacing.Right := 6;
  end;
  FCancelButton := TPanelBitBtn.Create(Self);
  with FCancelButton do
  begin
    Name     := 'CancelButton';
    Parent   := Self;
    Kind     := bkCancel;
    BorderSpacing.Around := 6;
    AutoSize := True;
    Align    := alRight;
  end;
  FCloseButton := TPanelBitBtn.Create(Self);
  with FCloseButton do
  begin
    Name     := 'CloseButton';
    Parent   := Self;
    Kind     := bkClose;
    BorderSpacing.Around := 6;
    AutoSize := True;
    Align    := alRight;
  end;
  FHelpButton := TPanelBitBtn.Create(Self);
  with FHelpButton do
  begin
    Name     := 'HelpButton';
    Parent   := Self;
    Kind     := bkHelp;
    BorderSpacing.Around := 6;
    AutoSize := True;
    Align    := alLeft;
  end;
  FOKButton := TPanelBitBtn.Create(Self);
  with FOKButton do
  begin
    Name     := 'OKButton';
    Parent   := Self;
    Kind     := bkOK;
    BorderSpacing.Around := 6;
    AutoSize := True;
    Align    := alRight;
  end;

  FCancelGlyph := TBitmap.Create;
  FCloseGlyph  := TBitmap.Create;
  FHelpGlyph   := TBitmap.Create;
  FOKGlyph     := TBitmap.Create;

  FOKGlyph.Assign(FOKButton.Glyph);
  FCancelGlyph.Assign(FCancelButton.Glyph);
  FCloseGlyph.Assign(FCloseButton.Glyph);
  FHelpGlyph.Assign(FHelpButton.Glyph);

  FDefaultButton := pbOK;
  FButtonOrder   := boDefault;
  FShowButtons   := [pbOK, pbCancel, pbClose, pbHelp];
  FShowGlyphs    := [pbOK, pbCancel, pbClose, pbHelp];

  if not (csLoading in ComponentState) then
  begin
    DoDefaultButton;
    DoShowButtons;
    DoShowGlyphs;
  end;
end;

destructor TCustomButtonPanel.Destroy;
begin
  FreeAndNil(FCancelGlyph);
  FreeAndNil(FCloseGlyph);
  FreeAndNil(FHelpGlyph);
  FreeAndNil(FOKGlyph);
  inherited Destroy;
end;

end.
