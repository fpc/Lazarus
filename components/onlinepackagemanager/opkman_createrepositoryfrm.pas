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

unit opkman_createrepositoryfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  // LCL
  Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls,
  // OpkMan
  opkman_VirtualTrees;

type

  { TCreateRepositoryFrm }

  TCreateRepositoryFrm = class(TForm)
    bAdd: TButton;
    bCancel: TButton;
    bDelete: TButton;
    bOpen: TButton;
    bCreate: TButton;
    pnButtons: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pnButtonsResize(Sender: TObject);
  private
    FVST: TVirtualStringTree;
    FRepository: String;
    procedure EnableDisableButtons(const AEnable: Boolean);
  public

  end;

var
  CreateRepositoryFrm: TCreateRepositoryFrm;

implementation
uses opkman_const;

{$R *.lfm}

{ TCreateRepositoryFrm }

procedure TCreateRepositoryFrm.FormCreate(Sender: TObject);
begin
  Caption := rsCreateRepositoryFrm_Caption;
  bCreate.Caption := rsCreateRepositoryFrm_bCreate_Caption;
  bCreate.Hint := rsCreateRepositoryFrm_bCreate_Hint;
  bOpen.Caption := rsCreateRepositoryFrm_bOpen_Caption;
  bOpen.Hint := rsCreateRepositoryFrm_bOpen_Hint;
  bAdd.Caption := rsCreateRepositoryFrm_bAdd_Caption;
  bAdd.Hint := rsCreateRepositoryFrm_bAdd_Hint;
  bDelete.Caption := rsCreateRepositoryFrm_bDelete_Caption;
  bDelete.Hint := rsCreateRepositoryFrm_bDelete_Hint;
  bCancel.Caption := rsCreateRepositoryFrm_bCancel_Caption;
  bCancel.Hint := rsCreateRepositoryFrm_bCancel_Hint;
  EnableDisableButtons(True);
  FVST := TVirtualStringTree.Create(nil);
end;

procedure TCreateRepositoryFrm.FormDestroy(Sender: TObject);
begin
  FVST.Free;
end;

procedure TCreateRepositoryFrm.pnButtonsResize(Sender: TObject);
begin
  bAdd.Left := (pnButtons.Width - (bAdd.Width + bDelete.Width)) div 2;
  bDelete.Left := bAdd.Left + bAdd.Width + 3;
end;

procedure TCreateRepositoryFrm.EnableDisableButtons(const AEnable: Boolean);
begin
  bOpen.Enabled := AEnable;
  bCreate.Enabled := AEnable;
  bAdd.Enabled := AEnable and FileExists(Trim(FRepository));
  bDelete.Enabled := AEnable and FileExists(Trim(FRepository)) and (FVST.RootNodeCount > 0);
  bCancel.Enabled := AEnable;
end;

end.

