{
 popupnotifier.pas

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

 Authors: A. J. Venter and Felipe Monteiro de Carvalho

 This unit contains the TPopupNotifier visual component.
}
unit popupnotifier;

interface

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls
  { Note: Be careful that ExtCtrls depend on popupnotifier, so
    it should have only a minimal amount of dependencies to avoid circular
    references. Preferably only units that ExtCtrls already has }
{$ifdef fpc}
  , LResources
{$endif}
;

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
    procedure HandleMouseDown(Sender: TOBject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
    procedure HandleMouseUp(Sender: TOBject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure paint; override;
  end;

  { TNotifierForm }

  TNotifierForm = class(TCustomForm)
  private
    lblTitle: TLabel;
    lblText: TLabel;
    imgIcon: TPicture;
    btnX: TNotifierXButton;
    procedure HideForm(Sender: TObject);
    procedure HandleResize(Sender: TObject);
    procedure HandlePaint(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    property Title: string read GetTitle write SetTitle;
    property Visible: Boolean read GetVisible write SetVisible;
    property OnClose: TCloseEvent  read GetOnClose write SetOnClose;
  end;

const
  BGDrawn: Boolean = False;

procedure Register;

implementation

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

procedure TNotifierXButton.paint;
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(0, 0, Width, Height);

  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Width := 1;
  
  {*******************************************************************
  *  Show a different background color when the button is down
  *******************************************************************}
  if FState = nbsUp then Canvas.Brush.Color := Color
  else Canvas.Brush.Color := clYellow;
  
  Canvas.RoundRect(0, 0, Width, Height, 10, 10);

  Canvas.Brush.Color := clBlack;
  Canvas.MoveTo(5, 5);
  Canvas.LineTo(Width - 5, Height - 5);

  Canvas.MoveTo(Width - 5, 5);
  Canvas.LineTo(5, Height - 5);

  inherited paint;
end;

{ TNotifierForm }

{*******************************************************************
*  TNotifierForm.Create ()
*
*  Creates the notifier form
*******************************************************************}
constructor TNotifierForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  BorderStyle := bsNone;

  Width := 325;
  Height := 110;

  ImgIcon := TPicture.Create;

  lblTitle := TLabel.Create(Self);
  lblTitle.Parent := Self;
  lblTitle.Transparent := True;
  lblTitle.Font.Style := [FsBold];
  lblTitle.Caption := 'Caption';
  lblTitle.ParentColor := True;
  lblTitle.OnClick := HideForm;

  lblText := TLabel.Create(Self);
  lblText.Parent := Self;
  lblText.Transparent := True;
  lblText.Caption := 'Text';
  lblText.ParentColor := True;
  lblText.OnClick := HideForm;

  BtnX := TNotifierXButton.Create(Self);
  BtnX.Parent := Self;
  BtnX.Color :=  $DCFFFF;
  btnX.OnClick := HideForm;

  HandleResize(Self);

  Color := $DCFFFF; // Doesn't work on Gtk

  // Connects the methods to events
  OnClick := HideForm;
  OnShow := HandleResize;
  
{$ifdef Unix}
  OnPaint := HandlePaint; // Fix for TForm.Color not working on gtk
{$endif}
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
begin
  if (ImgIcon.Bitmap <> nil) then IconAdjust := 5 + imgIcon.Bitmap.Width
  else IconAdjust := 0;
  
{  if (ImgIcon.Bitmap <> nil) then
  begin
    ImgIcon.Left := 5;
    ImgIcon.Top := 5;
    
    // Workaround for autosize not working as expected
    ImgIcon.Width := ImgIcon.Picture.Width;
    ImgIcon.Height := ImgIcon.Picture.Height;
  end;}

  if (lblTitle <> nil) then
  begin
    lblTitle.Left := IconAdjust + 5;
    lblTitle.Top := 5;
    lblTitle.Width := Width - 25;
    lblTitle.Height := 20;
  end;

  if (lblText <> nil) then
  begin
    lblText.Left := IconAdjust + 20;
    lblText.Top := LblTitle.Top + LblTitle.Height + 5;
    lblText.Width := Width - (IconAdjust + 5);
    lblText.Height := Height - 10 - LblText.Top;
  end;

  if (BtnX <> nil) then
  begin
    BtnX.Left := Width - 20;
    BtnX.Top := 5;
    BtnX.Width := 20;
    BtnX.Height := 20;
  end;
end;

{*******************************************************************
*  TNotifierForm.HandlePaint ()
*
*******************************************************************}
procedure TNotifierForm.HandlePaint(Sender: TObject);
begin
  { Temporary fix for TForm.Color not working on Gtk
    Remove when the bug is fixed! }
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(0,0,width,height));
  
  { Paints the icon. We can't use a TImage because it's on ExtCtrls }
  if Assigned(imgIcon.Bitmap) then Canvas.Draw(5, 5, imgIcon.Bitmap);
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
    vNotifierForm.left := x - vNotifierForm.Width
  else
    vNotifierForm.left := x;

  if y + vNotifierForm.Height > Screen.Height then
    vNotifierForm.top := y - vNotifierForm.Height
  else
    vNotifierForm.top := y;

  vNotifierForm.Show;
end;

initialization
{$ifdef fpc}
  {$i popupnotifier.lrs}
{$endif}

end.
