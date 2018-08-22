{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

 Author: Balázs Székely
 Abstract: Show hint for meta packages
}
unit opkman_showhintbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Graphics,
  LCLType, LCLIntf;

type

  { TfrShowHint }

  TfrShowHint = class(TFrame)
    lbDescription: TLabel;
    lbLicense: TLabel;
    mDescription: TMemo;
    mLicense: TMemo;
    pnBuffer: TPanel;
    pnPackageName: TPanel;
    pnBase: TPanel;
    pnDescription: TPanel;
    pnLicense: TPanel;
  private
  public
    procedure Init;
    procedure CalcHeight(AMemo: TMemo; AText: String);
  end;

implementation
uses opkman_const, opkman_options;

{$R *.lfm}

{ TfrShowHint }

procedure TfrShowHint.Init;
begin
  lbDescription.Caption := rsMainFrm_VSTText_Description;
  lbLicense.Caption := rsMainFrm_VSTText_License;
  Self.DoubleBuffered := True;
  mDescription.DoubleBuffered := True;
  mLicense.DoubleBuffered := True;
  if Options.HintFormOptionColors.Count = HintColCnt then
  begin
    pnPackageName.Color := StringToColor(Options.HintFormOptionColors[0]);
    pnDescription.Color := StringToColor(Options.HintFormOptionColors[1]);
    mDescription.Color := StringToColor(Options.HintFormOptionColors[1]);
    pnLicense.Color := StringToColor(Options.HintFormOptionColors[2]);
    mLicense.Color := StringToColor(Options.HintFormOptionColors[2]);
  end;
end;

procedure TfrShowHint.CalcHeight(AMemo: TMemo; AText: String);
var
  R: TRect;
  Increase: Integer;
  MH: Integer;
begin
  R := Rect(0, 0, AMemo.Width, 0);
  TPanel(AMemo.Parent).Font.Assign(AMemo.Font);
  DrawText(TPanel(AMemo.Parent).Canvas.Handle, PChar(AText), -1, R, DT_CALCRECT or DT_LEFT or DT_WORDBREAK or DT_NOPREFIX);
  Increase := AMemo.Height;
  MH := R.Bottom - R.Top;
  if MH < 35 then
    MH := 35;
  if MH > 100 then
    MH := 100;
  Increase := MH - Increase;
  AMemo.Height := MH;
  AMemo.Parent.Height := AMemo.Parent.Height + Increase + 2;
  AMemo.Text := AText;
end;

end.

