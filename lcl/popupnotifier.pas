{
 popupnotifier.pas

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Authors: A. J. Venter and Felipe Monteiro de Carvalho

 This unit contains the TPopupNotifier visual component.
}
unit PopupNotifier;

interface

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls;
  { Note: Be careful that ExtCtrls depend on popupnotifier, so
    it should have only a minimal amount of dependencies to avoid circular
    references. Preferably only units that ExtCtrls already has }

type
  { TNotifierXButton }

  { To avoid dependency on Buttons }
  TNotifierXButtonButtonState =
  (
    nbsUp,       // button is up
    nbsDown,     // button is down
    nbsHot       // button is under mouse
  );

  TNotifierXButton = class(TCustomControl)
  private
    FState: TNotifierXButtonButtonState;
    procedure HandleMouseDown(Sender: TObject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
    procedure HandleMouseUp(Sender: TObject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  end;

  { TNotifierForm }

  TNotifierForm = class(THintWindow)
  private
    lblTitle: TLabel;
    lblText: TLabel;
    imgIcon: TPicture;
    btnX: TNotifierXButton;
    procedure HideForm(Sender: TObject);
    procedure HandleResize(Sender: TObject);
  protected
    procedure CreateHandle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  end;

  { TPopupNotifier }

  TPopupNotifier = class(TComponent)
  private
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    function GetIcon: TPicture;
    procedure SetIcon(const Value: TPicture);
    function GetText: string;
    procedure SetText(const Value: string);
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
    procedure SetOnClose(const Value: TCloseEvent);
    function  GetOnClose:TCloseEvent;
    function  GetTextFont: TFont;
    procedure SetTextFont(const Value: TFont);
    function GetTitleFont: TFont;
    procedure SetTitleFont(const Value: TFont);
  public
    vNotifierForm: TNotifierForm;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Hide;
    procedure Show;
    procedure ShowAtPos(x: Integer; y: Integer);
  published
    property Color: TColor  read GetColor write SetColor;
    property Icon: TPicture read GetIcon write SetIcon;
    property Text: string read GetText write SetText;
    property TextFont: TFont read GetTextFont write SetTextFont;
    property Title: string read GetTitle write SetTitle;
    property TitleFont: TFont read GetTitleFont write SetTitleFont;
    property Visible: Boolean read GetVisible write SetVisible;
    property OnClose: TCloseEvent  read GetOnClose write SetOnClose;
  end;

const
  BGDrawn: Boolean = False;

procedure Register;

implementation

const
  INT_NOTIFIER_FORM_WIDTH  = 325;
  INT_NOTIFIER_FORM_HEIGHT = 110;
  INT_NOTIFIER_SCREEN_SPACING = 10;
  INT_NOTIFIER_SPACING = 5;
  INT_NOTIFIER_BUTTON_SIZE = 20;


{$ifndef fpc}
  {$R *.DFM}
{$endif}

procedure Register;
begin
  RegisterComponents('Common Controls', [TPopupNotifier]);
end;

{ TNotifierXButton }

procedure TNotifierXButton.HandleMouseDown(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
  begin
    FState := nbsDown;
    Self.Invalidate;
  end;
end;

procedure TNotifierXButton.HandleMouseUp(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FState := nbsUp;
  Self.Invalidate;
end;

constructor TNotifierXButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  FState := nbsUp;
  
  OnMouseUp := HandleMouseUp;
  OnMouseDown := HandleMouseDown;
end;

destructor TNotifierXButton.Destroy;
begin

  inherited Destroy;
end;

procedure TNotifierXButton.Paint;
var
  L: Integer;
begin
  Canvas.Pen.Color := cl3DDKShadow;
  Canvas.Pen.Width := 1;

  Canvas.Brush.Color := Color;
  Canvas.FillRect(0, 0, Width, Height);

  if FState = nbsUp then
    Canvas.Brush.Color := clBtnFace
  else begin
    Canvas.Brush.Color := clHighlight;
    Canvas.Pen.Color := clHighlightText;
  end;

  L := Scale96ToForm(4);
  Canvas.RoundRect(0, 0, Width, Height, L, L);

  Canvas.Pen.EndCap:=pecSquare;
  Canvas.Pen.Width := 2;

  L := Scale96ToForm(7);
  Canvas.MoveTo(L, L);
  Canvas.LineTo(Width - L, Height - L);

  Canvas.MoveTo(Width - L, L);
  Canvas.LineTo(L, Height - L);

  inherited Paint;
end;

{ TNotifierForm }

{*******************************************************************
*  TNotifierForm.Create ()
*
*  Creates the notifier form
*******************************************************************}
constructor TNotifierForm.Create(AOwner: TComponent);
var
  spc: Integer;
begin
  inherited Create(AOwner);

  BorderStyle := bsNone;

  Width := Scale96ToForm(INT_NOTIFIER_FORM_WIDTH);
  Height := Scale96ToForm(INT_NOTIFIER_FORM_HEIGHT);

  // Check for small screens. An extra spacing is necessary
  // in the Windows Mobile 5 emulator
  spc := Scale96ToForm(INT_NOTIFIER_SCREEN_SPACING);
  if Screen.Width - spc < Width then
    Width := Screen.Width - spc;

  ImgIcon := TPicture.Create;

  lblTitle := TLabel.Create(Self);
  lblTitle.Parent := Self;
  lblTitle.AutoSize := False;
  lblTitle.Transparent := True;
  lblTitle.Font.Name := 'default';
  lblTitle.Font.Size := 0;
  lblTitle.Font.Color := clDefault;
  lblTitle.Caption := 'Caption';
  lblTitle.ParentColor := True;
  lblTitle.OnClick := HideForm;

  lblText := TLabel.Create(Self);
  lblText.Parent := Self;
  lblText.AutoSize := False;
  lblText.Transparent := True;
  lblText.Font.Name := 'default';
  lblText.Font.Size := 0;
  lblText.Font.Color := clDefault;
  lblText.Caption := 'Text';
  lblText.WordWrap := True;
  lblText.ParentColor := True;
  lblText.OnClick := HideForm;

  BtnX := TNotifierXButton.Create(Self);
  BtnX.Parent := Self;
  BtnX.Color :=  Color;
  btnX.OnClick := HideForm;

  HandleResize(Self);

  Color := clInfoBk;

  // Connects the methods to events
  OnClick := HideForm;
  OnShow := HandleResize;
end;

{*******************************************************************
*  TNotifierForm.Destroy ()
*
*  Releases associated resources of the notifier form
*******************************************************************}
destructor TNotifierForm.Destroy;

begin
  ImgIcon.Free;
  lblTitle.Free;
  lblText.Free;
  BtnX.Free;
  inherited Destroy;
end;

procedure TNotifierForm.CreateHandle;
begin
  inherited;
  if lblTitle.Font.IsDefault then
    lblTitle.Font.Style := [fsBold];
  if lblText.Font.Color = clDefault then
    lblText.Font.Color := clInfoText;
  if lblTitle.Font.Color = clDefault then
    lblTitle.Font.Color := clInfoText;
end;

procedure TNotifierForm.Paint;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(0,0,width,height));

  { Paints the icon. We can't use a TImage because it's on ExtCtrls }
  if Assigned(imgIcon.Bitmap) then Canvas.Draw(5, 5, imgIcon.Bitmap);
end;

{*******************************************************************
*  TNotifierForm.HideForm ()
*
*  Utilized for events that hide the form, such as clicking on it
*******************************************************************}
procedure TNotifierForm.HideForm(Sender: TObject);
Var NoValue :TCloseAction;
begin
if Assigned(OnClose) then
   OnClose(Self,NoValue);
  Hide;
end;

{*******************************************************************
*  TNotifierForm.HandleResize ()
*
*  Handles OnResize events of the form
*******************************************************************}
procedure TNotifierForm.HandleResize(Sender: TObject);
var
  IconAdjust: Integer;
  spc: Integer;
  btnsize: Integer;
begin
  spc := Scale96ToForm(INT_NOTIFIER_SPACING);
  btnsize := Scale96ToForm(INT_NOTIFIER_BUTTON_SIZE);

  if (ImgIcon.Bitmap <> nil) then
    IconAdjust := spc + imgIcon.Bitmap.Width
  else
    IconAdjust := 0;

  if (lblTitle <> nil) then
  begin
    lblTitle.Left := IconAdjust + spc;
    lblTitle.Top := spc;
    lblTitle.AutoSize := false;
    lblTitle.Constraints.MaxWidth := Width - (lblTitle.Left + spc);
    lblTitle.AutoSize := true;
  end;

  if (lblText <> nil) then
  begin
    lblText.Left := IconAdjust + Scale96ToForm(20);
    lblText.Top := LblTitle.Top + LblTitle.Height + spc;
    lblText.Width := Width - (lblText.Left + spc);
    lblText.Height := Height - (lblText.Top + spc);
  end;

  if (BtnX <> nil) then
  begin
    BtnX.Left := Width - (btnSize + Scale96ToForm(5));
    BtnX.Top := spc;
    BtnX.Width := btnSize;
    BtnX.Height := btnSize;
  end;
end;

{ TPopupNotifier }

{*******************************************************************
*  Methods associated to properties
*******************************************************************}

function TPopupNotifier.GetTitle: string;
begin
  Result := vNotifierForm.lblTitle.Caption;
end;

procedure TPopupNotifier.SetTitle(const Value: string);
begin
  vNotifierForm.lblTitle.Caption := Value;
end;

procedure TPopupNotifier.SetOnClose(const Value: TCloseEvent);
begin
  VNotifierForm.Onclose := Value;
end;

function TPopupNotifier.GetOnClose:TCloseEvent;
begin
  Result := VNotifierForm.Onclose;
end;


function TPopupNotifier.GetVisible: Boolean;
begin
  Result := vNotifierForm.Visible;
end;

procedure TPopupNotifier.SetVisible(const Value: Boolean);
begin
  vNotifierForm.Visible := Value;
end;

function TPopupNotifier.GetText: string;
begin
  Result := vNotifierForm.lblText.Caption;
end;

procedure TPopupNotifier.SetText(const Value: string);
begin
  vNotifierForm.lblText.Caption := Value;
end;

function TPopupNotifier.GetIcon: TPicture;
begin
  Result := vNotifierForm.imgIcon;
end;

procedure TPopupNotifier.SetIcon(const Value: TPicture);
begin
  vNotifierForm.imgIcon.Assign(Value);
end;

function TPopupNotifier.GetColor: TColor;
begin
  Result := vNotifierForm.Color;
end;

procedure TPopupNotifier.SetColor(const Value: TColor);
begin
  vNotifierForm.Color := Value;
end;

function TPopupNotifier.GetTextFont: TFont;
begin
  Result := vNotifierForm.lblText.Font;
end;

procedure TPopupNotifier.SetTextFont(const Value: TFont);
begin
  vNotifierForm.lblText.Font.Assign(Value);
end;

function TPopupNotifier.GetTitleFont: TFont;
begin
  Result := vNotifierForm.lblTitle.Font;
end;

procedure TPopupNotifier.SetTitleFont(const Value: TFont);
begin
  vNotifierForm.lblTitle.Font.Assign(Value);
end;

{*******************************************************************
*  TPopupNotifier.Create ()
*******************************************************************}
constructor TPopupNotifier.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  vNotifierForm := TNotifierForm.Create(nil);
  vNotifierForm.Visible := False;
end;

{*******************************************************************
*  TPopupNotifier.Destroy ()
*******************************************************************}
destructor TPopupNotifier.Destroy;
begin
  vNotifierForm.Hide;
  
  // The following line needs to be removed if we have
  // vNotifierForm := TNotifierForm.Create(Application);
  vNotifierForm.Free;

  inherited Destroy;
end;

{*******************************************************************
*  TPopupNotifier.Hide ()
*******************************************************************}
procedure TPopupNotifier.Hide;
begin
  vNotifierForm.Hide;
end;

{*******************************************************************
*  TPopupNotifier.Show ()
*******************************************************************}
procedure TPopupNotifier.Show;
begin
  vNotifierForm.Show;
end;

{*******************************************************************
*  TPopupNotifier.ShowAtPos ()
*
*  Shows the notifier at a specific position
*
*  The position is corrected to fit the screen, similarly to how
*  a popup menu would have it's position corrected
*
*******************************************************************}
procedure TPopupNotifier.ShowAtPos(x: Integer; y: Integer);
begin
  if x + vNotifierForm.Width > Screen.Width then
  begin
    vNotifierForm.left := x - vNotifierForm.Width;
    if vNotifierForm.Left < 0 then vNotifierForm.Left := 0;
  end
  else
    vNotifierForm.left := x;

  if y + vNotifierForm.Height > Screen.Height then
  begin
    vNotifierForm.top := y - vNotifierForm.Height;
    if vNotifierForm.top < 0 then vNotifierForm.top := 0;
  end
  else
    vNotifierForm.top := y;

  vNotifierForm.Show;
end;

end.
