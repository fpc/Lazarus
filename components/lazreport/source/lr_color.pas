
{*****************************************}
{                                         }
{             FastReport v2.3             }
{             Color selector              }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_Color;

interface

{$I LR_Vers.inc}

uses
  SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, Buttons, ExtCtrls,

  LR_Const;

type
  TColorSelector = class(TPanel)
  private
    FColor: TColor;
    FOtherBtn: TSpeedButton;
    FOnColorSelected: TNotifyEvent;
    procedure ButtonClick(Sender: TObject);
    procedure SetColor(Value: TColor);
  public
    constructor Create(AOwner: TComponent); override;
    property Color: TColor read FColor write SetColor;
    property OnColorSelected: TNotifyEvent read FOnColorSelected write
      FOnColorSelected;
  end;

implementation

uses LR_Class;

constructor TColorSelector.Create(AOwner: TComponent);
var
  b  : TSpeedButton;
  i,j: Integer;
  bmp: TBitmap;
begin
  inherited Create(AOwner);
  Parent := AOwner as TWinControl;
  Width := 96;
  Height := 132;
  bmp := TBitmap.Create;
  bmp.Width := 16;
  bmp.Height := 17;
  with bmp.Canvas do
  begin
    Brush.Color := clBtnFace;
    FillRect(Rect(0, 0, 16, 17));
  end;

  for i := 0 to 3 do
  begin
    for j := 0 to 3 do
    begin
      b := TSpeedButton.Create(Self);
      b.Parent := Self;
      b.SetBounds(j * 22 + 4, i * 22 + 4, 22, 22);
      with bmp.Canvas do
      begin
        Brush.Color := frColors[i * 4 + j];
        Pen.Color := clBtnShadow;
        Rectangle(0, 0, 16, 16);
      end;
      b.Glyph.Assign(bmp);
      b.Tag := i * 4 + j;
      b.OnClick := @ButtonClick;
      b.GroupIndex := 1;
      b.Flat := True;
    end;
  end;
  
  b := TSpeedButton.Create(Self);
  with b do
  begin
    Parent := Self;
    SetBounds(4, 92, 88, 18);
    Tag := 16;
    Caption := sTransparent;
    OnClick := @ButtonClick;
    GroupIndex := 1;
    Flat := True;
  end;

  FOtherBtn := TSpeedButton.Create(Self);
  with FOtherBtn do
  begin
    Parent := Self;
    SetBounds(4, 110, 88, 18);
    Tag := 17;
    Caption := sOther;
    OnClick := @ButtonClick;
    GroupIndex := 1;
    Flat := True;
  end;
  bmp.Free;
end;

procedure TColorSelector.ButtonClick(Sender: TObject);
var
  cd: TColorDialog;
  i: Integer;
begin
  try
    i := (Sender as TSpeedButton).Tag;
    case i of
      0..15: FColor := frColors[i];
      16   : FColor := clNone;
      17   : begin
               cd := TColorDialog.Create(Self);
               //** cd.Options := [cdFullOpen];
               if cd.Execute then
                  FColor := cd.Color
               else
                 Exit;
             end;
    end;
  finally
    if Assigned(fOnColorSelected) then
       FOnColorSelected(Self);
    Hide;
  end;
end;

procedure TColorSelector.SetColor(Value: TColor);
var
  i,j: Integer;
  c  : TSpeedButton;
  bmp: TBitmap;
begin
  for i := 0 to 16 do
  begin
    if ((i=16) and (Value=clNone)) or (frColors[i]=Value) then
    begin
      for j := 0 to ControlCount-1 do
      begin
        c:=Controls[j] as TSpeedButton;
        if c.Tag=i then
        begin
          c.Down:=True;
          FOtherBtn.Glyph.FreeImage;
          break;
        end;
      end;
      Exit;
    end;
  end;
  
  bmp := TBitmap.Create;
  try
    bmp.Width := 12;
    bmp.Height := 13;
    with bmp.Canvas do
    begin
      Brush.Color := clBtnFace;
      FillRect(Rect(0, 0, 12, 13));
      Brush.Color := Value;
      Pen.Color := clBtnShadow;
      Rectangle(0, 0, 12, 12);
    end;
    FOtherBtn.Glyph.Assign(bmp);
  finally
    bmp.Free;
  end;
end;

end.
