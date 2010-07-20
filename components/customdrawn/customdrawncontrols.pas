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
  Classes, SysUtils, Graphics, Controls, LCLType, LCLIntf;

type

  TBitmappedButtonOption = (bboCheckable, bboUseImageForSelection,
    bboUseImageForMouseOver);

  TBitmappedButtonOptions = set of TBitmappedButtonOption;

  TBitmappedButtonState = (bbsNormal, bbsDown, bbsMouseOver,
    bbsSelected, bbsChecked, bbsCheckedSelected, bbsCheckedDown { is going to be unchecked });

  { TCustomBitmappedButton }

  TCustomBitmappedButton = class(TCustomControl)
  private
    FOnChange: TNotifyEvent;
  protected
    FImageBtn: TBitmap;
    FImageBtnDown: TBitmap;
    FImageBtnMouseOver: TBitmap;
    FImageBtnSelected: TBitmap;
    FImageBtnChecked: TBitmap;
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    function GetStateBitmap(): TBitmap;
    // Properties
    property ImageBtn: TBitmap read FImageBtn;
    property ImageBtnDown: TBitmap read FImageBtnDown;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TBitmappedButton = class(TCustomBitmappedButton)
  published
    property ImageBtn: TBitmap;
    property ImageBtnDown: TBitmap;
{    FImageBtnMouseOver: TBitmap;
    FImageBtnSelected: TBitmap;
    FImageBtnChecked: TBitmap;
    FOptions: TBitmappedButtonOptions;}
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
  bbsNormal, bbsSelected: NewState := bbsDown;
  bbsChecked, bbsCheckedSelected: NewState := bbsCheckedDown;
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
begin
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
end;

destructor TCustomBitmappedButton.Destroy;
begin
  if Assigned(FImageBtn) then FImageBtn.Free;
  if Assigned(FImageBtnDown) then FImageBtnDown.Free;
  if Assigned(FImageBtnMouseOver) then FImageBtnMouseOver.Free;
  if Assigned(FImageBtnSelected) then FImageBtnSelected.Free;
  if Assigned(FImageBtnChecked) then FImageBtnChecked.Free;

  inherited Destroy;
end;

procedure TCustomBitmappedButton.EraseBackground(DC: HDC);
begin
  // The correct implementation is doing nothing
end;

procedure TCustomBitmappedButton.Paint;
begin
  Canvas.Draw(0, 0, GetStateBitmap());
end;

function TCustomBitmappedButton.GetStateBitmap(): TBitmap;
begin
  case FState of
  bbsNormal:    Result := FImageBtn;
  bbsDown:      Result := FImageBtnDown;
  bbsMouseOver: Result := FImageBtnMouseOver;
  bbsSelected:  Result := FImageBtnSelected;
  bbsChecked:   Result := FImageBtnChecked;
  end;
end;

end.

