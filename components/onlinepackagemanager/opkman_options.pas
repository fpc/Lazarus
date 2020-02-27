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
unit opkman_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  // LazUtils
  Laz2_XMLCfg, LazFileUtils,
  // IdeIntf
  LazIDEIntf,MacroIntf,
  // OpkMan
  opkman_const;

const
  OpkVersion = 1;
  HintColCnt = 3;

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
    FRemoteRepository: TStringList;
    FRemoteRepositoryTmp: TStringList;
    FActiveRepositoryIndex: Integer;
    FLoadJsonLocally: Boolean;
    FLoadJsonLocallyCnt: Integer;
    FForceDownloadAndExtract: Boolean;
    FDeleteZipAfterInstall: Boolean;
    FIncompatiblePackages: Boolean;
    FAlreadyInstalledPackages: Boolean;
    FCheckForUpdates: Integer;
    FLastUpdate: TDateTime;
    FConTimeOut: Integer;
    FDaysToShowNewPackages: Integer;
    FShowRegularIcons: Boolean;
    FUseDefaultTheme: Boolean;
    FHintFormOption: Integer;
    FHintFormOptionColors: TStringList;
    FChanged: Boolean;
    FLastDownloadDir: String;
    FLastPackageDirSrc: String;
    FLastPackageDirDst: String;
    FLastPrivateRepository: String;
    // Default values for local repositories.
    FLocalPackagesDefault: String;
    FLocalArchiveDefault: String;
    FLocalUpdateDefault: String;
    // Actual local repositories in macro format.
    FLocalRepositoryPackages: String;
    FLocalRepositoryArchive: String;
    FLocalRepositoryUpdate: String;
    FUserProfile: Integer;
    FExcludedFiles: String;
    FExcludedFolders: String;
    FOpenSSLDownloadType: Integer;
    procedure CheckColors;
    function GetLocalRepositoryArchiveExpanded:string;
    function GetLocalRepositoryPackagesExpanded:string;
    function GetLocalRepositoryUpdateExpanded:string;
  public
    constructor Create(const AFileName: String);
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    procedure LoadDefault;
    procedure CreateMissingPaths;
    property LocalRepositoryPackagesExpanded:string read GetLocalRepositoryPackagesExpanded;
    property LocalRepositoryArchiveExpanded:string read GetLocalRepositoryArchiveExpanded;
    property LocalRepositoryUpdateExpanded:string read GetLocalRepositoryUpdateExpanded;
  published
    property Changed: Boolean read FChanged write FChanged;
    property RemoteRepository: TStringList read FRemoteRepository write FRemoteRepository;
    property RemoteRepositoryTmp: TStringList read FRemoteRepositoryTmp write FRemoteRepositoryTmp;
    property ActiveRepositoryIndex: Integer read FActiveRepositoryIndex write FActiveRepositoryIndex;
    property LoadJsonLocally: Boolean read FLoadJsonLocally write FLoadJsonLocally;
    property LoadJsonLocallyCnt: Integer read FLoadJsonLocallyCnt write FLoadJsonLocallyCnt;
    property ForceDownloadAndExtract: Boolean read FForceDownloadAndExtract write FForceDownloadAndExtract;
    property DeleteZipAfterInstall: Boolean read FDeleteZipAfterInstall write FDeleteZipAfterInstall;
    property IncompatiblePackages: Boolean read FIncompatiblePackages write FIncompatiblePackages;
    property AlreadyInstalledPackages: Boolean read FAlreadyInstalledPackages write FAlreadyInstalledPackages;
    property CheckForUpdates: Integer read FCheckForUpdates write FCheckForUpdates;
    property LastUpdate: TDateTime read FLastUpdate write FLastUpdate;
    property ConTimeOut: Integer read FConTimeOut write FConTimeOut;
    property DaysToShowNewPackages: Integer read FDaysToShowNewPackages write FDaysToShowNewPackages;
    property ShowRegularIcons: Boolean read FShowRegularIcons write FShowRegularIcons;
    property UseDefaultTheme: Boolean read FUseDefaultTheme write FUseDefaultTheme;
    property HintFormOption: Integer read FHintFormOption write FHintFormOption;
    property HintFormOptionColors: TStringList read FHintFormOptionColors write FHintFormOptionColors;
    property UserProfile: Integer read FUserProfile write FUserProfile;
    property LastDownloadDir: String read FLastDownloadDir write FLastDownloadDir;
    property LastPackagedirSrc: String read FLastPackageDirSrc write FLastPackageDirSrc;
    property LastPackagedirDst: String read FLastPackageDirDst write FLastPackageDirDst;
    property LastPrivateRepository: String read FLastPrivateRepository write FLastPrivateRepository;
    property ProxyEnabled: Boolean read FProxySettings.FEnabled write FProxySettings.FEnabled;
    property ProxyServer: String read FProxySettings.FServer write FProxySettings.FServer;
    property ProxyPort: Word read FProxySettings.FPort write FProxySettings.FPort;
    property ProxyUser: String read FProxySettings.FUser write FProxySettings.FUser;
    property ProxyPassword: String read FProxySettings.FPassword write FProxySettings.FPassword;
    property OpenSSLDownloadType: Integer read FOpenSSLDownloadType write FOpenSSLDownloadType;
    property LocalRepositoryPackages: String read FLocalRepositoryPackages write FLocalRepositoryPackages;
    property LocalRepositoryArchive: String read FLocalRepositoryArchive write FLocalRepositoryArchive;
    property LocalRepositoryUpdate: String read FLocalRepositoryUpdate write FLocalRepositoryUpdate;
    property ExcludedFiles: String read FExcludedFiles write FExcludedFiles;
    property ExcludedFolders: String read FExcludedFolders write FExcludedFolders;
  end;

var
  Options: TOptions = nil;

implementation

{ TOptions }

constructor TOptions.Create(const AFileName: String);
var
  LocalRepo: String;
begin
  FRemoteRepository := TStringList.Create;
  FRemoteRepositoryTmp := TStringList.Create;
  FHintFormOptionColors := TStringList.Create;
  LocalRepo := AppendPathDelim(AppendPathDelim(LazarusIDE.GetPrimaryConfigPath) + cLocalRepository);
  FLocalPackagesDefault := LocalRepo + AppendPathDelim(cLocalRepositoryPackages);
  FLocalArchiveDefault := LocalRepo + AppendPathDelim(cLocalRepositoryArchive);
  FLocalUpdateDefault := LocalRepo + AppendPathDelim(cLocalRepositoryUpdate);

  FXML := TXMLConfig.Create(AFileName);
  if FileExists(AFileName) then
  begin
    Load;
    CheckColors;
    if FLocalRepositoryPackages = '' then
      FLocalRepositoryPackages := FLocalPackagesDefault;
    if FLocalRepositoryArchive = '' then
      FLocalRepositoryArchive := FLocalArchiveDefault;
    if FLocalRepositoryUpdate = '' then
      FLocalRepositoryUpdate := FLocalUpdateDefault;
    if FExcludedFiles = '' then
      FExcludedFiles := cExcludedFilesDef;
    if FExcludedFolders = '' then
      FExcludedFolders := cExcludedFoldersDef;
  end
  else
    LoadDefault;
  CreateMissingPaths;
end;

destructor TOptions.Destroy;
begin
  if FChanged then
    Save;
  FRemoteRepository.Free;
  FRemoteRepositoryTmp.Free;
  FHintFormOptionColors.Free;
  FXML.Free;
  inherited Destroy;
end;

procedure TOptions.Load;
begin
  FVersion := FXML.GetValue('Version/Value', 0);
  if FVersion = 0 then
    FRemoteRepository.Text := FXML.GetValue('RemoteRepository/Value', '')
  else
    FRemoteRepository.Text := FXML.GetValue('General/RemoteRepository/Value', '');
  if Trim(FRemoteRepository.Text) = '' then
    FRemoteRepository.Add(cRemoteRepository);
  FActiveRepositoryIndex := FXML.GetValue('General/ActiveRepositoryIndex/Value', 0);
  FLoadJsonLocally := FXML.GetValue('General/LoadJsonLocally/Value', False);
  FLoadJsonLocallyCnt := FXML.GetValue('General/LoadJsonLocallyCnt/Value', 0);
  FForceDownloadAndExtract := FXML.GetValue('General/ForceDownloadAndExtract/Value', True);
  FDeleteZipAfterInstall := FXML.GetValue('General/DeleteZipAfterInstall/Value', True);
  FIncompatiblePackages := FXML.GetValue('General/IncompatiblePackages/Value', True);
  FAlreadyInstalledPackages := FXML.GetValue('General/AlreadyInstalledPackages/Value', False);
  FLastDownloadDir := FXML.GetValue('General/LastDownloadDir/Value', '');
  FLastPackageDirSrc := FXML.GetValue('General/LastPackageDirSrc/Value', '');
  FLastPackageDirDst := FXML.GetValue('General/LastPackageDirDst/Value', '');
  FLastPrivateRepository := FXML.GetValue('General/LastPrivateRepository/Value', '');
  FCheckForUpdates := FXML.GetValue('General/CheckForUpdates/Value', 0);
  FLastUpdate := FXML.GetExtendedValue('General/LastUpdate/Value', 0.0);
  FConTimeOut := FXML.GetValue('General/ConTimeOut/Value', 10);
  FDaysToShowNewPackages := FXML.GetValue('General/DaysToShowNewPackages/Value', 31);
  FShowRegularIcons := FXML.GetValue('General/ShowRegularIcons/Value', True);
  FUseDefaultTheme := FXML.GetValue('General/UseDefaultTheme/Value', True);
  FHintFormOption := FXML.GetValue('General/HintFormOption/Value', 0);
  FHintFormOptionColors.Text := FXML.GetValue('General/HintFormOptionColors/Value', '');

  FProxySettings.FEnabled := FXML.GetValue('Proxy/Enabled/Value', False);
  FProxySettings.FServer := FXML.GetValue('Proxy/Server/Value', '');
  FProxySettings.FPort := FXML.GetValue('Proxy/Port/Value', 0);
  FProxySettings.FUser := FXML.GetValue('Proxy/User/Value', '');
  FProxySettings.FPassword := FXML.GetValue('Proxy/Password/Value', '');

  FOpenSSLDownloadType := FXML.GetValue('OpenSSL/DownloadType/Value', 1);

  FLocalRepositoryPackages := FXML.GetValue('Folders/LocalRepositoryPackages/Value', '');
  FLocalRepositoryArchive := FXML.GetValue('Folders/LocalRepositoryArchive/Value', '');
  FLocalRepositoryUpdate := FXML.GetValue('Folders/LocalRepositoryUpdate/Value', '');

  FUserProfile := FXML.GetValue('Profiles/UserProfile/Value', 0);
  FExcludedFiles := FXML.GetValue('Profiles/ExcludedFiles/Value', '');
  FExcludedFolders := FXML.GetValue('Profiles/ExcludedFolders/Value', '');
end;

procedure TOptions.Save;
begin
  FXML.SetDeleteValue('Version/Value', OpkVersion, 0);
  FXML.SetDeleteValue('General/RemoteRepository/Value', FRemoteRepository.Text, '');
  FXML.SetDeleteValue('General/ActiveRepositoryIndex/Value', FActiveRepositoryIndex, 0);
  FXML.SetDeleteValue('General/LoadJsonLocally/Value', FLoadJsonLocally, False);
  FXML.SetDeleteValue('General/LoadJsonLocallyCnt/Value', FLoadJsonLocallyCnt, 0);
  FXML.SetDeleteValue('General/ForceDownloadAndExtract/Value', FForceDownloadAndExtract, True);
  FXML.SetDeleteValue('General/DeleteZipAfterInstall/Value', FDeleteZipAfterInstall, True);
  FXML.SetDeleteValue('General/IncompatiblePackages/Value', FIncompatiblePackages, True);
  FXML.SetDeleteValue('General/AlreadyInstalledPackages/Value', FAlreadyInstalledPackages, False);
  FXML.SetDeleteValue('General/LastDownloadDir/Value', FLastDownloadDir, '');
  FXML.SetDeleteValue('General/LastPackageDirSrc/Value', FLastPackageDirSrc, '');
  FXML.SetDeleteValue('General/LastPackageDirDst/Value', FLastPackageDirDst, '');
  FXML.SetDeleteValue('General/LastPrivateRepository/Value', FLastPrivateRepository, '');
  FXML.SetDeleteValue('General/CheckForUpdates/Value', FCheckForUpdates, 0);
  FXML.SetDeleteExtendedValue('General/LastUpdate/Value', FLastUpdate, 0.0);
  FXML.SetDeleteValue('General/ConTimeOut/Value', FConTimeOut, 10);
  FXML.SetDeleteValue('General/DaysToShowNewPackages/Value', FDaysToShowNewPackages, 31);

  FXML.SetDeleteValue('General/ShowRegularIcons/Value', FShowRegularIcons, True);
  FXML.SetDeleteValue('General/UseDefaultTheme/Value', FUseDefaultTheme, True);
  FXML.SetDeleteValue('General/HintFormOption/Value', FHintFormOption, 0);
  FXML.SetDeleteValue('General/HintFormOptionColors/Value', FHintFormOptionColors.Text, '');

  FXML.SetDeleteValue('Proxy/Enabled/Value', FProxySettings.FEnabled, false);
  FXML.SetDeleteValue('Proxy/Server/Value', FProxySettings.FServer, '');
  FXML.SetDeleteValue('Proxy/Port/Value', FProxySettings.FPort, 0);
  FXML.SetDeleteValue('Proxy/User/Value', FProxySettings.FUser, '');
  FXML.SetDeleteValue('Proxy/Password/Value', FProxySettings.FPassword, '');

  FXML.SetDeleteValue('OpenSSL/DownloadType/Value', FOpenSSLDownloadType, 1);

  FXML.SetDeleteValue('Folders/LocalRepositoryPackages/Value', FLocalRepositoryPackages, '');
  FXML.SetDeleteValue('Folders/LocalRepositoryArchive/Value', FLocalRepositoryArchive, '');
  FXML.SetDeleteValue('Folders/LocalRepositoryUpdate/Value', FLocalRepositoryUpdate, '');

  FXML.SetDeleteValue('Profiles/UserProfile/Value', FUserProfile, 0);
  FXML.SetDeleteValue('Profiles/ExcludedFiles/Value', FExcludedFiles, '');
  FXML.SetDeleteValue('Profiles/ExcludedFolders/Value', FExcludedFolders, '');

  FXML.Flush;
  FChanged := False;
end;

procedure TOptions.LoadDefault;
begin
  FRemoteRepository.Clear;
  FRemoteRepositoryTmp.Clear;
  FRemoteRepository.Add(cRemoteRepository);
  FHintFormOptionColors.Clear;
  CheckColors;
  FActiveRepositoryIndex := 0;
  FLoadJsonLocally := False;
  FLoadJsonLocallyCnt := 0;
  FForceDownloadAndExtract := True;
  FDeleteZipAfterInstall := True;
  FIncompatiblePackages := True;
  FAlreadyInstalledPackages := False;
  FCheckForUpdates := 5;
  FLastUpdate := 0.0;
  FConTimeOut := 10;
  FDaysToShowNewPackages := 31;
  FShowRegularIcons := True;
  FUseDefaultTheme := True;
  FHintFormOption := 0;

  FProxySettings.FEnabled := False;
  FProxySettings.FServer := '';
  FProxySettings.FPort := 0;
  FProxySettings.FUser := '';
  FProxySettings.FPassword := '';

  FOpenSSLDownloadType := 1;

  FLocalRepositoryPackages := FLocalPackagesDefault;
  FLocalRepositoryArchive := FLocalArchiveDefault;
  FLocalRepositoryUpdate := FLocalUpdateDefault;

  FUserProfile := 0;
  FExcludedFiles := cExcludedFilesDef;
  FExcludedFolders := cExcludedFoldersDef;
  Save;
end;

procedure TOptions.CreateMissingPaths;
begin
  if not DirectoryExists(LocalRepositoryPackagesExpanded) then
    CreateDir(LocalRepositoryPackagesExpanded);
  if not DirectoryExists(LocalRepositoryArchiveExpanded) then
    CreateDir(LocalRepositoryArchiveExpanded);
  if not DirectoryExists(LocalRepositoryUpdateExpanded) then
    CreateDir(LocalRepositoryUpdateExpanded);
end;

procedure TOptions.CheckColors;
begin
  if FHintFormOptionColors.Count <> HintColCnt then
  begin
    FHintFormOptionColors.Clear;
    FHintFormOptionColors.Add(ColorToString($00D9FFFF));
    FHintFormOptionColors.Add(ColorToString($00E6FFE6));
    FHintFormOptionColors.Add(ColorToString($00FEEBD3));
  end
end;

function TOptions.GetLocalRepositoryArchiveExpanded:string;
begin
  result:=FLocalRepositoryArchive;
  IDEMacros.SubstituteMacros(result);
  Result:=AppendPathDelim(result);
end;

function TOptions.GetLocalRepositoryPackagesExpanded:string;
begin
  result:=FLocalRepositoryPackages;
  IDEMacros.SubstituteMacros(result);
  Result:=AppendPathDelim(result);
end;

function TOptions.GetLocalRepositoryUpdateExpanded:string;
begin
  result:=FLocalRepositoryUpdate;
  IDEMacros.SubstituteMacros(result);
  Result:=AppendPathDelim(result);
end;

end.

