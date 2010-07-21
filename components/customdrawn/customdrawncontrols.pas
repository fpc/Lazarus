{
  Copyright (C) 2010 Felipe Monteiro de Carvalho

  License: The same modifying LGPL with static linking exception as the LCL

  This unit should be a repository for various custom drawn components,
  such as a custom drawn version of TButton, of TEdit, of TPageControl, etc,
  eventually forming a full set of custom drawn components.
}
unit customdrawncontrols;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, LCLType, LCLIntf, IntfGraphics;

type

  // commented items are not yet supported
  TBitmappedButtonOption = (bboUseAlphaBlending, bboUseImageForSelection
    {bboUseImageForMouseOver, bboDrawFocusRectangle,}
    (*bboCheckable,*));

  TBitmappedButtonOptions = set of TBitmappedButtonOption;

  // commented items are not yet supported
  TBitmappedButtonState = (bbsNormal, bbsDown, bbsMouseOver, bbsFocused
    (* bbsChecked, bbsCheckedSelected, bbsCheckedDown { is going to be unchecked }*));

  { TCustomBitmappedButton }

  TCustomBitmappedButton = class(TCustomControl)
  private
    FOnChange: TNotifyEvent;
  protected
    FImageBtn: TPicture;
    FImageBtnDown: TPicture;
    FImageBtnMouseOver: TPicture;
    FImageBtnFocused: TPicture;
    FImageBtnChecked: TPicture;
    FImageBtnAlpha: TPicture;
    FOptions: TBitmappedButtonOptions;
    FState: TBitmappedButtonState;
    // keyboard
//    procedure DoEnter; override;
//    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    // mouse
    procedure Click; override;
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
  protected
    // Properties
    property ImageBtn: TPicture read FImageBtn;
    property ImageBtnDown: TPicture read FImageBtnDown;
    property ImageBtnFocused: TPicture read FImageBtnFocused;
    property ImageBtnAlpha: TPicture read FImageBtnAlpha;
    property Options: TBitmappedButtonOptions read FOptions write FOptions;
    // Events
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    function GetStateBitmap(): TBitmap;
  end;

  {@@
    TBitmappedButton is a simple custom drawn button which bases it's drawing
    on provided raster images. Currently the following states are supported:
    normal, down and focused. The button may be drawn flat or alpha blended
    using a separate image for the Alpha channel. While pixels in the alpha
    channel will result in the button pixel being fully drawn, while black
    pixels represent pixels which aren't drawn. grey pixels are alpha blended.
  }

  TBitmappedButton = class(TCustomBitmappedButton)
  published
    property ImageBtn;
    property ImageBtnDown;
    property ImageBtnFocused;
    property ImageBtnAlpha;
    property Options;
    // Events
    property OnChange;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Common Controls', [TBitmappedButton]);
end;

{ TCustomBitmappedButton }

procedure TCustomBitmappedButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
end;

procedure TCustomBitmappedButton.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
end;

procedure TCustomBitmappedButton.Click;
begin
  inherited Click;
end;

procedure TCustomBitmappedButton.DblClick;
begin
  inherited DblClick;
end;

procedure TCustomBitmappedButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  NewState: TBitmappedButtonState;
begin
  case FState of
  bbsNormal, bbsFocused: NewState := bbsDown;
//  bbsChecked, bbsCheckedSelected: NewState := bbsCheckedDown;
  end;

  if NewState <> FState then
  begin
    FState := NewState;
    Invalidate;
  end;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomBitmappedButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  NewState: TBitmappedButtonState;
begin
  case FState of
  bbsDown:
  begin
    if Focused then NewState := bbsFocused
    else NewState := bbsNormal;
  end;
{  bbsCheckedDown:
  begin
    if Focused then NewState := bbsCheckedSelected
    else NewState := bbsChecked;
  end;}
  end;

  if NewState <> FState then
  begin
    FState := NewState;
    Invalidate;
  end;

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomBitmappedButton.MouseEnter;
begin
  inherited MouseEnter;
end;

procedure TCustomBitmappedButton.MouseLeave;
begin
  inherited MouseLeave;
end;

constructor TCustomBitmappedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOptions := [{bboDrawSelectionRectangle}];

  FImageBtn := TPicture.Create;
  FImageBtnDown := TPicture.Create;
  FImageBtnMouseOver := TPicture.Create;
  FImageBtnFocused := TPicture.Create;
  FImageBtnChecked := TPicture.Create;
  FImageBtnAlpha := TPicture.Create;
end;

destructor TCustomBitmappedButton.Destroy;
begin
  if Assigned(FImageBtn) then FImageBtn.Free;
  if Assigned(FImageBtnDown) then FImageBtnDown.Free;
  if Assigned(FImageBtnMouseOver) then FImageBtnMouseOver.Free;
  if Assigned(FImageBtnFocused) then FImageBtnFocused.Free;
  if Assigned(FImageBtnChecked) then FImageBtnChecked.Free;
  if Assigned(FImageBtnAlpha) then FImageBtnAlpha.Free;

  inherited Destroy;
end;

procedure TCustomBitmappedButton.EraseBackground(DC: HDC);
begin
  // The correct implementation is doing nothing
end;

procedure TCustomBitmappedButton.Paint;
{var
  lBitmap: TBitmap;
  lImageIntf, lAlphaIntf: TLazIntfImage;}
begin
{  if [bboUseAlphaBlending] in FOptions then
  begin
    lBitmap := TBitmap.Create;
    lImageIntf := TLazIntfImage.Create(0, 0);
    lAlphaIntf := TLazIntfImage.Create(0, 0);
    try
      lImageIntf.LoadFromBitmap(lBitmap.Handle, lBitmap.MaskHandle);
      lImageIntf
      Canvas.Draw(0, 0, lBitmap);
    finally
      lBitmap.Free;
      lImageIntf.Free;
      lAlphaIntf.Free;
    end;
  end
  else}
    Canvas.Draw(0, 0, GetStateBitmap());
end;

function TCustomBitmappedButton.GetStateBitmap(): TBitmap;
begin
  case FState of
  bbsNormal:    Result := FImageBtn.Bitmap;
  bbsDown:      Result := FImageBtnDown.Bitmap;
//  bbsMouseOver: Result := FImageBtnMouseOver;
  bbsFocused:  Result := FImageBtnFocused.Bitmap;
//  bbsChecked:   Result := FImageBtnChecked;
  end;
end;

end.

