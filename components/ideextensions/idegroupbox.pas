{ IdeGroupBox - A specially themed groupbox used inside the IDE

  Copyright (C) 2010 <name of author> <contact>

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit IdeGroupBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLIntf, LCLProc, math;

type

  { TIdeGroupBox }

  TIdeGroupBox = class(TCustomControl)
  private
    FHeaderFont: TFont;
    FHeaderHeight: Integer;
    FBevelTop: Integer;
    FBevelHeight: Integer;
    procedure SetHeaderFont(const AValue: TFont);
  protected
    procedure AdjustClientRect(var ARect: TRect); override;
    procedure Paint; override;
    procedure CalcSize;
    procedure FontChanged(Sender: TObject); override;
    procedure TextChanged; override;
    procedure CreateHandle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Caption;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
  end;

{$R *.rc}

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('IdeExtensions',[TIdeGroupBox]);
end;

{ TIdeGroupBox }
 procedure TIdeGroupBox.SetHeaderFont(const AValue: TFont);
begin
  FHeaderFont.Assign(AValue);
end;

procedure TIdeGroupBox.AdjustClientRect(var ARect: TRect);
begin
debugln([FHeaderHeight]);
  ARect.Top := FHeaderHeight;
end;

procedure TIdeGroupBox.Paint;
var
  PaintRect: TRect;
  TextWidth: Integer;
begin
  Canvas.Font := FHeaderFont;
  PaintRect.Top := FBevelTop;
  PaintRect.Bottom := FBevelTop + FBevelHeight;
  PaintRect.Left := 0;
  if Caption = '' then begin
    PaintRect.Right := Width;
    Frame3D(Handle, PaintRect, 1, bvLowered);
    exit;
  end;
  TextWidth := Canvas.TextWidth(Caption);
  PaintRect.Right := 60;
  Canvas.Frame3D(PaintRect, 1, bvLowered);

  PaintRect.Top := FBevelTop;
  PaintRect.Bottom := FBevelTop + FBevelHeight;
  PaintRect.Left := 80 + TextWidth;
  PaintRect.Right := Width;
  Canvas.Frame3D(PaintRect, 1, bvLowered);

  Canvas.TextOut(70, 0, Caption);
end;

procedure TIdeGroupBox.CalcSize;
begin
  if not HandleAllocated then exit;
  if Caption = '' then
    FHeaderHeight := Canvas.TextHeight(' ')
  else
    FHeaderHeight := Canvas.TextHeight(Caption);
  FBevelHeight := Max(3, FHeaderHeight div 5);
  FHeaderHeight := Max(FHeaderHeight, FBevelHeight + 2);
  FBevelTop := (FHeaderHeight - FBevelHeight) div 2;
end;

procedure TIdeGroupBox.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  CalcSize;
end;

procedure TIdeGroupBox.TextChanged;
begin
  inherited TextChanged;
  if HandleAllocated then begin
    CalcSize;
    Invalidate;
  end;
end;

procedure TIdeGroupBox.CreateHandle;
begin
  inherited CreateHandle;
  CalcSize;
end;

constructor TIdeGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  FHeaderFont := TFont.Create;
  FHeaderFont.Assign(Font);
  FHeaderFont.OnChange := @FontChanged;
  FHeaderFont.Style := FHeaderFont.Style + [fsBold];
end;

destructor TIdeGroupBox.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FHeaderFont);
end;

end.
