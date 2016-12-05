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
unit opkman_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazIDEIntf, Laz2_XMLCfg, LazFileUtils;

const
  OpkVersion = 1;

type
  { TOptions }
  TProxySettings = record
    FEnabled: boolean;
    FServer: string;
    FPort: Word;
    FUser: string;
    FPassword: string;
  end;

  TOptions = class
   private
     FProxySettings: TProxySettings;
     FXML: TXMLConfig;
     FVersion: Integer;
     FRemoteRepository: String;
     FForceDownloadAndExtract: Boolean;
     FDeleteZipAfterInstall: Boolean;
     FChanged: Boolean;
     FLastDownloadDir: String;
     FLastPackageDirSrc: String;
     FLastPackageDirDst: String;
     FRestrictedExtensions: String;
     FRestrictedDirectories: String;
     // Default values for local repositories.
     FLocalPackagesDefault: String;
     FLocalArchiveDefault: String;
     FLocalUpdateDefault: String;
     // Actual local repositories.
     FLocalRepositoryPackages: String;
     FLocalRepositoryArchive: String;
     FLocalRepositoryUpdate: String;
     procedure SetRemoteRepository(const ARemoteRepository: String);
   public
     constructor Create(const AFileName: String);
     destructor Destroy; override;
     procedure Load;
     procedure Save;
     procedure LoadDefault;
     procedure CreateMissingPaths;
   published
     property Changed: Boolean read FChanged write FChanged;
     property RemoteRepository: string read FRemoteRepository write SetRemoteRepository;
     property ForceDownloadAndExtract: Boolean read FForceDownloadAndExtract write FForceDownloadAndExtract;
     property DeleteZipAfterInstall: Boolean read FDeleteZipAfterInstall write FDeleteZipAfterInstall;
     property LastDownloadDir: String read FLastDownloadDir write FLastDownloadDir;
     property LastPackagedirSrc: String read FLastPackageDirSrc write FLastPackageDirSrc;
     property LastPackagedirDst: String read FLastPackageDirDst write FLastPackageDirDst;
     property ProxyEnabled: Boolean read FProxySettings.FEnabled write FProxySettings.FEnabled;
     property ProxyServer: String read FProxySettings.FServer write FProxySettings.FServer;
     property ProxyPort: Word read FProxySettings.FPort write FProxySettings.FPort;
     property ProxyUser: String read FProxySettings.FUser write FProxySettings.FUser;
     property ProxyPassword: String read FProxySettings.FPassword write FProxySettings.FPassword;
     property RestrictedExtension: String read FRestrictedExtensions write FRestrictedExtensions;
     property RestrictedDirectories: String read FRestrictedDirectories write FRestrictedDirectories;
     property LocalRepositoryPackages: String read FLocalRepositoryPackages write FLocalRepositoryPackages;
     property LocalRepositoryArchive: String read FLocalRepositoryArchive write FLocalRepositoryArchive;
     property LocalRepositoryUpdate: String read FLocalRepositoryUpdate write FLocalRepositoryUpdate;
  end;

var
  Options: TOptions = nil;

implementation
uses opkman_const;

{ TOptions }

constructor TOptions.Create(const AFileName: String);
var
  LocalRepo: String;
begin
  LocalRepo := AppendPathDelim(AppendPathDelim(LazarusIDE.GetPrimaryConfigPath) + cLocalRepository);
  FLocalPackagesDefault := LocalRepo + AppendPathDelim(cLocalRepositoryPackages);
  FLocalArchiveDefault := LocalRepo + AppendPathDelim(cLocalRepositoryArchive);
  FLocalUpdateDefault := LocalRepo + AppendPathDelim(cLocalRepositoryUpdate);

  FXML := TXMLConfig.Create(AFileName);
  if FileExists(AFileName) then
  begin
    Load;
    if FLocalRepositoryPackages = '' then
      FLocalRepositoryPackages := FLocalPackagesDefault;
    if FLocalRepositoryArchive = '' then
      FLocalRepositoryArchive := FLocalArchiveDefault;
    if FLocalRepositoryUpdate = '' then
      FLocalRepositoryUpdate := FLocalUpdateDefault;
    if FRestrictedExtensions = '' then
      FRestrictedExtensions := cRestrictedExtensionDef;
    if FRestrictedDirectories = '' then
      FRestrictedDirectories := cRestrictedDirectoryDef;
  end
  else
    LoadDefault;
  CreateMissingPaths;
end;

destructor TOptions.Destroy;
begin
  if FChanged then
    Save;
  FXML.Free;
  inherited Destroy;
end;

procedure TOptions.Load;
begin
  FVersion := FXML.GetValue('Version/Value', 0);
  if FVersion = 0 then
    FRemoteRepository := FXML.GetValue('RemoteRepository/Value', '')
  else
    FRemoteRepository := FXML.GetValue('General/RemoteRepository/Value', '');
  FForceDownloadAndExtract := FXML.GetValue('General/ForceDownloadAndExtract/Value', True);
  FDeleteZipAfterInstall := FXML.GetValue('General/DeleteZipAfterInstall/Value', True);
  FLastDownloadDir := FXML.GetValue('General/LastDownloadDir/Value', '');
  FLastPackageDirSrc := FXML.GetValue('General/LastPackageDirSrc/Value', '');
  FLastPackageDirDst := FXML.GetValue('General/LastPackageDirDst/Value', '');

  FProxySettings.FEnabled := FXML.GetValue('Proxy/Enabled/Value', False);
  FProxySettings.FServer := FXML.GetValue('Proxy/Server/Value', '');
  FProxySettings.FPort := FXML.GetValue('Proxy/Port/Value', 0);
  FProxySettings.FUser := FXML.GetValue('Proxy/User/Value', '');
  FProxySettings.FPassword := FXML.GetValue('Proxy/Password/Value', '');

  FLocalRepositoryPackages := FXML.GetValue('Folders/LocalRepositoryPackages/Value', '');
  FLocalRepositoryArchive := FXML.GetValue('Folders/LocalRepositoryArchive/Value', '');
  FLocalRepositoryUpdate := FXML.GetValue('Folders/LocalRepositoryUpdate/Value', '');
end;

procedure TOptions.Save;
begin
  FXML.SetDeleteValue('Version/Value', OpkVersion, 0);
  FXML.SetDeleteValue('General/RemoteRepository/Value', FRemoteRepository, '');
  FXML.SetDeleteValue('General/ForceDownloadAndExtract/Value', FForceDownloadAndExtract, True);
  FXML.SetDeleteValue('General/DeleteZipAfterInstall/Value', FDeleteZipAfterInstall, True);
  FXML.SetDeleteValue('General/LastDownloadDir/Value', FLastDownloadDir, '');
  FXML.SetDeleteValue('General/LastPackageDirSrc/Value', FLastPackageDirSrc, '');
  FXML.SetDeleteValue('General/LastPackageDirDst/Value', FLastPackageDirDst, '');

  FXML.SetDeleteValue('Proxy/Enabled/Value', FProxySettings.FEnabled, false);
  FXML.SetDeleteValue('Proxy/Server/Value', FProxySettings.FServer, '');
  FXML.SetDeleteValue('Proxy/Port/Value', FProxySettings.FPort, 0);
  FXML.SetDeleteValue('Proxy/User/Value', FProxySettings.FUser, '');
  FXML.SetDeleteValue('Proxy/Password/Value', FProxySettings.FPassword, '');

  FXML.SetDeleteValue('Folders/LocalRepositoryPackages/Value', FLocalRepositoryPackages, '');
  FXML.SetDeleteValue('Folders/LocalRepositoryArchive/Value', FLocalRepositoryArchive, '');
  FXML.SetDeleteValue('Folders/LocalRepositoryUpdate/Value', FLocalRepositoryUpdate, '');

  FXML.Flush;
  FChanged := False;
end;

procedure TOptions.LoadDefault;
begin
  FRemoteRepository := cRemoteRepository;
  FForceDownloadAndExtract := True;
  FDeleteZipAfterInstall := True;

  FProxySettings.FEnabled := False;
  FProxySettings.FServer := '';
  FProxySettings.FPort := 0;
  FProxySettings.FUser := '';
  FProxySettings.FPassword := '';

  FLocalRepositoryPackages := FLocalPackagesDefault;
  FLocalRepositoryArchive := FLocalArchiveDefault;
  FLocalRepositoryUpdate := FLocalUpdateDefault;
  FRestrictedExtensions := cRestrictedExtensionDef;
  FRestrictedDirectories := cRestrictedDirectoryDef;
  Save;
end;

procedure TOptions.CreateMissingPaths;
begin
  if not DirectoryExists(FLocalRepositoryPackages) then
    CreateDirUTF8(FLocalRepositoryPackages);
  if not DirectoryExists(FLocalRepositoryArchive) then
    CreateDirUTF8(FLocalRepositoryArchive);
  if not DirectoryExists(FLocalRepositoryUpdate) then
    CreateDirUTF8(FLocalRepositoryUpdate);
end;

procedure TOptions.SetRemoteRepository(const ARemoteRepository: String);
begin
  if FRemoteRepository <> ARemoteRepository then
  begin
    FRemoteRepository := Trim(ARemoteRepository);
    if FRemoteRepository[Length(FRemoteRepository)] <> '/' then
      FRemoteRepository := FRemoteRepository + '/';
  end;
end;

end.

