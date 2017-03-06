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

unit opkman_createrepositorypackage;

{$mode objfpc}{$H+}

interface

uses
  // LCL
  Forms,
  // OpkMan
  opkman_const, opkman_createrepositorypackagefr;

type

  { TCreateRepositoryPackagesFrm }

  TCreateRepositoryPackagesFrm = class(TForm)
    frCreateRep: TCreateRepositoryPackagefr;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  CreateRepositoryPackagesFrm: TCreateRepositoryPackagesFrm;

implementation

{$R *.lfm}

{ TCreateRepositoryPackagesFrm }

procedure TCreateRepositoryPackagesFrm.FormCreate(Sender: TObject);
begin
  Caption := rsCreateRepositoryPackageFrm_Caption;
  frCreateRep.InitializeFrame;
end;

procedure TCreateRepositoryPackagesFrm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  frCreateRep.FinalizeFrame;
end;

end.

