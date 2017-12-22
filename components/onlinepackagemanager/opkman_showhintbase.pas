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
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls;

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
uses opkman_const;

{$R *.lfm}

{ TfrShowHint }

procedure TfrShowHint.Init;
begin
  lbDescription.Caption := rsMainFrm_VSTText_Description;
  lbLicense.Caption := rsMainFrm_VSTText_License;
  Self.DoubleBuffered := True;
  pnPackageName.Color := $00D9FFFF;
  pnDescription.Color := $00E6FFE6;
  mDescription.Color := $00E6FFE6;
  mDescription.DoubleBuffered := True;
  pnLicense.Color := $00FEEBD3;
  mLicense.Color := $00FEEBD3;
  mLicense.DoubleBuffered := True;
end;

end.

