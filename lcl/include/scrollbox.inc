{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

Procedure TScrollBox.DoAutoSize;
begin
  Inherited DoAutoSize;
  If AutoSize then begin
    AutoSizing := True;
    Height := ClientHeight + 4;
    AutoSizing := False;
  end;
end;

constructor TScrollBox.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  fCompStyle:= csScrollBox;
  AutoSize := False;
  AutoScroll := True;
  ControlStyle := [csAcceptsControls, csClickEvents, csDoubleClicks];
  Left := 0;
  Top := 0;
  Width := 200;
  Height := 200;
  Visible := True;
  ParentColor := False;
  ParentFont := False;
  Ctl3D := True;
  Color := clWhite;
end;
