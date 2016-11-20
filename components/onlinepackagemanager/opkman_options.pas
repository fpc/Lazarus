unit opkman_options;

{$mode objfpc}{$H+}

interface

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
}
uses
  Classes, SysUtils, Laz2_XMLCfg, LazFileUtils;

type
  { TPackageOptions }
  RProxySettings = record
    Enabled: boolean;
    Server: string;
    Port: Word;
    User: string;
    Password: string;
  end;

  TPackageOptions = class
   private
     FProxySettings: RProxySettings;
     FXML: TXMLConfig;
     FRemoteRepository: String;
     FChanged: Boolean;
     FLastDownloadDir: String;
     FLastPackageDirSrc: String;
     FLastPackageDirDst: String;
     procedure LoadDefault;
     procedure SetRemoteRepository(const ARemoteRepository: String);
   public
     constructor Create(const AFileName: String);
     destructor Destroy; override;
   public
     procedure Save;
     procedure Load;
   published
     property Changed: Boolean read FChanged write FChanged;
     property LastDownloadDir: String read FLastDownloadDir write FLastDownloadDir;
     property LastPackagedirSrc: String read FLastPackageDirSrc write FLastPackageDirSrc;
     property LastPackagedirDst: String read FLastPackageDirDst write FLastPackageDirDst;
     property RemoteRepository: string read FRemoteRepository write SetRemoteRepository;
     property ProxyEnabled: Boolean read FProxySettings.Enabled write FProxySettings.Enabled;
     property ProxyServer: String read FProxySettings.Server write FProxySettings.Server;
     property ProxyPort: Word read FProxySettings.Port write FProxySettings.Port;
     property ProxyUser: String read FProxySettings.User write FProxySettings.User;
     property ProxyPassword: String read FProxySettings.Password write FProxySettings.Password;
  end;

var
  PackageOptions: TPackageOptions = nil;

implementation

{ TPackageOptions }

constructor TPackageOptions.Create(const AFileName: String);
begin
  FXML := TXMLConfig.Create(AFileName);
  if FileExists(AFileName) then
    Load
  else
    LoadDefault;
  end;

destructor TPackageOptions.Destroy;
begin
  if FChanged then
    Save;
  FXML.Free;
  inherited Destroy;
end;

procedure TPackageOptions.Load;
begin
  FRemoteRepository := FXML.GetValue('RemoteRepository/Value', '');
  FLastDownloadDir := FXML.GetValue('LastDownloadDir/Value', '');
  FLastPackageDirSrc := FXML.GetValue('LastPackageDirSrc/Value', '');
  FLastPackageDirDst := FXML.GetValue('LastPackageDirDst/Value', '');
  FProxySettings.Enabled := FXML.GetValue('Proxy/Enabled/Value', false);
  FProxySettings.Server := FXML.GetValue('Proxy/Server/Value', '');
  FProxySettings.Port := FXML.GetValue('Proxy/Port/Value', 0);
  FProxySettings.User := FXML.GetValue('Proxy/User/Value', '');
  FProxySettings.Password := FXML.GetValue('Proxy/Password/Value', '');
end;

procedure TPackageOptions.Save;
begin
  FXML.SetDeleteValue('RemoteRepository/Value', FRemoteRepository, '');
  FXML.SetDeleteValue('LastDownloadDir/Value', FLastDownloadDir, '');
  FXML.SetDeleteValue('LastPackageDirSrc/Value', FLastPackageDirSrc, '');
  FXML.SetDeleteValue('LastPackageDirDst/Value', FLastPackageDirDst, '');

  FXML.SetDeleteValue('Proxy/Enabled/Value', FProxySettings.Enabled, false);
  FXML.SetDeleteValue('Proxy/Server/Value', FProxySettings.Server, '');
  FXML.SetDeleteValue('Proxy/Port/Value', FProxySettings.Port, 0);
  FXML.SetDeleteValue('Proxy/User/Value', FProxySettings.User, '');
  FXML.SetDeleteValue('Proxy/Password/Value', FProxySettings.Password, '');

  FXML.Flush;
  FChanged := False;
end;

procedure TPackageOptions.LoadDefault;
begin
  FRemoteRepository := 'http://packages.lazarus-ide.org/';
  Save;
end;

procedure TPackageOptions.SetRemoteRepository(const ARemoteRepository: String);
begin
  if FRemoteRepository <> ARemoteRepository then
  begin
    FRemoteRepository := Trim(ARemoteRepository);
    if FRemoteRepository[Length(FRemoteRepository)] <> '/' then
      FRemoteRepository := FRemoteRepository + '/';
  end;
end;

end.

