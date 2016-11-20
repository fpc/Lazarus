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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

 Author: Balázs Székely
 Abstract:
   Implementation of the options dialog.
}

unit opkman_optionsfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, opkman_VirtualTrees, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Spin;

type

  { TOptionsFrm }

  TOptionsFrm = class(TForm)
    bCancel: TButton;
    bOk: TButton;
    bSettings: TButton;
    cbProxy: TCheckBox;
    edProxyServer: TEdit;
    edProxyUser: TEdit;
    edProxyPassword: TEdit;
    edRemoteRepository: TEdit;
    gbProxySettings: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    pnBottom: TPanel;
    seProxyPort: TSpinEdit;
    procedure bOkClick(Sender: TObject);
    procedure cbProxyChange(Sender: TObject);
    procedure edRemoteRepositoryKeyPress(Sender: TObject; var Key: char);
  private

  public

  end;

var
  OptionsFrm: TOptionsFrm;

implementation
uses opkman_options, opkman_common, opkman_const;
{$R *.lfm}

{ TOptionsFrm }

procedure TOptionsFrm.bOkClick(Sender: TObject);
begin
  if Trim(edRemoteRepository.Text)  = '' then
  begin
    MessageDlgEx(rsOptionedRemoteRepository, mtInformation, [mbOk], Self);
    edRemoteRepository.SetFocus;
    Exit;
  end;

  if cbProxy.Checked then
  begin
    if Trim(edProxyServer.Text)  = '' then
    begin
      MessageDlgEx(rsOptionedProxyServer, mtInformation, [mbOk], Self);
      edProxyServer.SetFocus;
      Exit;
    end;
    if seProxyPort.Value = 0 then
    begin
      MessageDlgEx(rsOptionedProxyPort, mtInformation, [mbOk], Self);
      seProxyPort.SetFocus;
      Exit;
    end;
  end;
  PackageOptions.RemoteRepository := edRemoteRepository.Text;
  PackageOptions.ProxyEnabled := cbProxy.Checked;
  PackageOptions.ProxyServer := edProxyServer.Text;
  PackageOptions.ProxyPort := seProxyPort.Value;
  PackageOptions.ProxyUser := edProxyUser.Text;
  PackageOptions.ProxyPassword := edProxyPassword.Text;

  PackageOptions.Save;
  ModalResult := mrOk;
end;

procedure TOptionsFrm.cbProxyChange(Sender: TObject);
begin
  gbProxySettings.Enabled:= cbProxy.Checked;
end;

procedure TOptionsFrm.edRemoteRepositoryKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    bOkClick(bOk);
end;

end.

