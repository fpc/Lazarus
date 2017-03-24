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
}

unit opkman_packagedetailsfrm;

{$mode objfpc}{$H+}

interface

uses
  // LCL
  Forms, StdCtrls, ExtCtrls, ButtonPanel;

type

  { TPackageDetailsFrm }

  TPackageDetailsFrm = class(TForm)
    ButtonPanel: TButtonPanel;
    mDetails: TMemo;
    procedure FormKeyPress(Sender: TObject; var Key: char);
  private

  public

  end;

var
  PackageDetailsFrm: TPackageDetailsFrm;

implementation

{$R *.lfm}

{ TPackageDetailsFrm }

procedure TPackageDetailsFrm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
    Close;
end;

end.

