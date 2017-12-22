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
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Graphics;

type

  { TfrShowHint }

  TfrShowHint = class(TFrame)
    lbDescription: TLabel;
    lbLicense: TLabel;
    mDescription: TMemo;
    mLicense: TMemo;
    pnPackageName: TPanel;
    pnBase: TPanel;
    pnDescription: TPanel;
    pnLicense: TPanel;
  private

  public
    procedure Init;
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
  if Options.HintFormOptionColors.Count = 3 then
  begin
    pnPackageName.Color := StringToColor(Options.HintFormOptionColors[0]);
    pnDescription.Color := StringToColor(Options.HintFormOptionColors[1]);
    mDescription.Color := StringToColor(Options.HintFormOptionColors[1]);
    pnLicense.Color := StringToColor(Options.HintFormOptionColors[2]);
    mLicense.Color := StringToColor(Options.HintFormOptionColors[2]);
  end;
end;

end.

