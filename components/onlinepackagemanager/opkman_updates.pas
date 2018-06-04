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

unit opkman_updates;

{$mode objfpc}{$H+}

{$INCLUDE opkman_fpcdef.inc}

interface

uses
  Classes, SysUtils, fpjson, fpjsonrtti, dateutils,
  // LazUtils
  Laz2_XMLCfg,
  // OpkMan
  opkman_serializablepackages, opkman_options, opkman_common,
  {$IFDEF MSWINDOWS}
    opkman_const,
    {$IFDEF FPC311}zipper,{$ELSE}opkman_zip,{$ENDIF}
  {$ENDIF}
  {$IFDEF FPC311}fphttpclient{$ELSE}opkman_httpclient{$ENDIF}
  ;

const
  OpkVersion = 1;

type

  { TUpdateLazPackages }

  TUpdateLazPackages = class(TCollectionItem)
  private
    FName: String;
    FVersion: String;
    FForceNotify: Boolean;
    FInternalVersion: Integer;
  published
    property Name: String read FName write FName;
    property Version: String read FVersion write FVersion;
    property ForceNotify: Boolean read FForceNotify write FForceNotify;
    property InternalVersion: Integer read FInternalVersion write FInternalVersion;
  end;

  { TUpdatePackageData }

  TUpdatePackageData = class(TPersistent)
  private
    FDownloadZipURL: String;
    FDisableInOPM: Boolean;
    FName: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
  published
    property Name: String read FName write FName;
    property DownloadZipURL: String read FDownloadZipURL write FDownloadZipURL;
    property DisableInOPM: Boolean read FDisableInOPM write FDisableInOPM;
  end;

  {TUpdatePackage}

  TUpdatePackage = class(TPersistent)
  private
    FUpdatePackageData: TUpdatePackageData;
    FUpdateLazPackages: TCollection;
    FLastError: String;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;
    function LoadFromJSON(const AJSON: TJSONStringType): Boolean;
    function SaveToJSON(var AJSON: TJSONStringType): Boolean;
    property LastError: String read FLastError;
  published
    property UpdatePackageData: TUpdatePackageData read FUpdatePackageData write FUpdatePackageData;
    property UpdateLazPackages: TCollection read FUpdateLazPackages write FUpdateLazPackages;
  end;

  { TUpdates }
  TUpdates = class(TThread)
  private
    FHTTPClient: TFPHTTPClient;
    FUpdatePackage: TUpdatePackage;
    FVersion: Integer;
    FNeedToBreak: Boolean;
    FBusyUpdating: Boolean;
    FBusySaving: Boolean;
    FOpenSSLAvailable: Boolean;
    FOnUpdate: TNotifyEvent;
    FTime: QWORD;
    FInterval: Cardinal;
    FFileName: String;
    function GetUpdateInfo(const AURL: String; var AJSON: TJSONStringType): Boolean;
    procedure DoOnUpdate;
    procedure Load;
    procedure Save;
    procedure AssignPackageData(AMetaPackage: TMetaPackage);
    procedure ResetPackageData(AMetaPackage: TMetaPackage);
    procedure CheckForOpenSSL;
    procedure CheckForUpdates;
    function IsTimeToUpdate: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(const AFileName: String);
    destructor Destroy; override;
    procedure StartUpdate;
    procedure StopUpdate;
  published
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;

var
  Updates: TUpdates = nil;

implementation

{ TUpdatePackage }

procedure TUpdatePackage.Clear;
var
  I: Integer;
begin
  FUpdatePackageData.Clear;
  for I := FUpdateLazPackages.Count - 1 downto 0 do
    FUpdateLazPackages.Items[I].Free;
  FUpdateLazPackages.Clear;
end;

constructor TUpdatePackage.Create;
begin
  FUpdatePackageData := TUpdatePackageData.Create;
  FUpdateLazPackages := TCollection.Create(TUpdateLazPackages);
end;

destructor TUpdatePackage.Destroy;
var
  I: Integer;
begin
  FUpdatePackageData.Free;
  for I := FUpdateLazPackages.Count - 1 downto 0 do
    FUpdateLazPackages.Items[I].Free;
  FUpdateLazPackages.Free;
  inherited Destroy;
end;

function TUpdatePackage.LoadFromJSON(const AJSON: TJSONStringType): Boolean;
var
  DeStreamer: TJSONDeStreamer;
begin
  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    Clear;
    try
      DeStreamer.JSONToObject(AJSON, Self);
      Result := True;
    except
      on E: Exception do
      begin
        FLastError := E.Message;
        Result := False;
      end;
    end;
  finally
    DeStreamer.Free;
  end;
end;

function TUpdatePackage.SaveToJSON(var AJSON: TJSONStringType): Boolean;
var
  Streamer: TJSONStreamer;
begin
  Result := False;
  Streamer := TJSONStreamer.Create(nil);
  try
    Streamer.Options := Streamer.Options + [jsoUseFormatString];
    try
      AJSON := Streamer.ObjectToJSONString(Self);
      Result := AJSON <> '';
    except
      on E: Exception do
      begin
        FLastError := E.Message;
        Result := False;
      end;
    end;
  finally
    Streamer.Free;
  end;
end;

{ TUpdatePackageData }

constructor TUpdatePackageData.Create;
begin
  Clear;
end;

destructor TUpdatePackageData.Destroy;
begin
  //
  inherited Destroy;
end;

procedure TUpdatePackageData.Clear;
begin
  FName := '';
  FDownloadZipURL := '';
  FDisableInOPM := False;
end;

{ TUpdates }

constructor TUpdates.Create(const AFileName: String);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FFileName := AFileName;
  FHTTPClient := TFPHTTPClient.Create(nil);
  {$IFDEF FPC311}
  FHTTPClient.IOTimeout := Options.ConTimeOut*1000;
  {$ENDIF}
  if Options.ProxyEnabled then
  begin
    FHTTPClient.Proxy.Host:= Options.ProxyServer;
    FHTTPClient.Proxy.Port:= Options.ProxyPort;
    FHTTPClient.Proxy.UserName:= Options.ProxyUser;
    FHTTPClient.Proxy.Password:= Options.ProxyPassword;
  end;
  FUpdatePackage := TUpdatePackage.Create;
end;

destructor TUpdates.Destroy;
begin
  FHTTPClient.Free;
  FUpdatePackage.Free;
  inherited Destroy;
end;

procedure TUpdates.Load;
var
  PackageCount: Integer;
  LazarusPkgCount: Integer;
  I, J: Integer;
  Path, SubPath: String;
  PackageName: String;
  LazarusPkgName: String;
  MetaPkg: TMetaPackage;
  LazarusPkg: TLazarusPackage;
  HasUpdate: Boolean;
  FXML: TXMLConfig;
begin
  if (not Assigned(SerializablePackages)) or (SerializablePackages.Count = 0) then
    Exit;
  FXML := TXMLConfig.Create(FFileName);
  try
    FVersion := FXML.GetValue('Version/Value', 0);
    PackageCount := FXML.GetValue('Count/Value', 0);
    for I := 0 to PackageCount - 1 do
    begin
      Path := 'Package' + IntToStr(I) + '/';
      PackageName := FXML.GetValue(Path + 'Name', '');
      MetaPkg := SerializablePackages.FindMetaPackage(PackageName, fpbPackageName);
      if MetaPkg <> nil then
      begin
        HasUpdate := False;
        MetaPkg.DownloadZipURL := FXML.GetValue(Path + 'DownloadZipURL', '');
        MetaPkg.DisableInOPM := FXML.GetValue(Path + 'DisableInOPM', False);
        MetaPkg.Rating := FXML.GetValue(Path + 'Rating', 0);
        LazarusPkgCount := FXML.GetValue(Path + 'Count', 0);
        for J := 0 to LazarusPkgCount - 1 do
        begin
          SubPath := Path + 'PackageFile' +  IntToStr(J) + '/';
          LazarusPkgName := FXML.GetValue(SubPath + 'Name', '');
          LazarusPkg := MetaPkg.FindLazarusPackage(LazarusPkgName);
          if LazarusPkg <> nil then
          begin
            LazarusPkg.UpdateVersion := FXML.GetValue(SubPath + 'UpdateVersion', '');
            LazarusPkg.ForceNotify := FXML.GetValue(SubPath + 'ForceNotify', False);
            LazarusPkg.InternalVersion := FXML.GetValue(SubPath + 'InternalVersion', 0);;
            LazarusPkg.InternalVersionOld := FXML.GetValue(SubPath + 'InternalVersionOld', 0);
            LazarusPkg.RefreshHasUpdate;
            if not HasUpdate then
              HasUpdate := (LazarusPkg.HasUpdate) and (LazarusPkg.InstalledFileVersion < LazarusPkg.UpdateVersion);
          end;
        end;
        MetaPkg.HasUpdate := HasUpdate;
      end;
    end;
  finally
    FXML.Free;
  end;
  if Assigned(FOnUpdate) and (not FNeedToBreak) then
    Synchronize(@DoOnUpdate);
end;

procedure TUpdates.Save;
var
  I, J: Integer;
  Path, SubPath: String;
  MetaPkg: TMetaPackage;
  LazarusPkg: TLazarusPackage;
  FXML: TXMLConfig;
begin
  if (not Assigned(SerializablePackages)) or (SerializablePackages.Count = 0) or (FBusySaving) then
    Exit;
  FBusySaving := True;
  FXML := TXMLConfig.CreateClean(FFileName);
  try
    FXML.SetDeleteValue('Version/Value', OpkVersion, 0);
    FXML.SetDeleteValue('Count/Value', SerializablePackages.Count, 0);
    for I := 0 to SerializablePackages.Count - 1 do
    begin
      MetaPkg := SerializablePackages.Items[I];
      Path := 'Package' + IntToStr(I) + '/';
      FXML.SetDeleteValue(Path + 'Name', MetaPkg.Name, '');
      FXML.SetDeleteValue(Path + 'DownloadZipURL', MetaPkg.DownloadZipURL, '');
      FXML.SetDeleteValue(Path + 'DisableInOPM', MetaPkg.DisableInOPM, False);
      FXML.SetDeleteValue(Path + 'Rating', MetaPkg.Rating, 0);
      FXML.SetDeleteValue(Path + 'Count', SerializablePackages.Items[I].LazarusPackages.Count, 0);
      for J := 0 to SerializablePackages.Items[I].LazarusPackages.Count - 1 do
      begin
        SubPath := Path + 'PackageFile' +  IntToStr(J) + '/';
        LazarusPkg := TLazarusPackage(SerializablePackages.Items[I].LazarusPackages.Items[J]);
        FXML.SetDeleteValue(SubPath + 'Name', LazarusPkg.Name, '');
        FXML.SetDeleteValue(SubPath + 'UpdateVersion', LazarusPkg.UpdateVersion, '');
        FXML.SetDeleteValue(SubPath + 'ForceNotify', LazarusPkg.ForceNotify, False);
        FXML.SetDeleteValue(SubPath + 'InternalVersion', LazarusPkg.InternalVersion, 0);
        FXML.SetDeleteValue(SubPath + 'InternalVersionOld', LazarusPkg.InternalVersionOld, 0);
      end;
    end;
    FXML.Flush;
  finally
    FXML.Free;
    FBusySaving := False;
  end;
end;

procedure TUpdates.AssignPackageData(AMetaPackage: TMetaPackage);
var
  I: Integer;
  HasUpdate: Boolean;
  LazarusPkg: TLazarusPackage;
  UpdLazPkgs: TUpdateLazPackages;
begin
  if FBusySaving then
    Exit;
  HasUpdate := False;
  AMetaPackage.DownloadZipURL := FUpdatePackage.FUpdatePackageData.DownloadZipURL;
  AMetaPackage.DisableInOPM := FUpdatePackage.FUpdatePackageData.DisableInOPM;
  for I := 0 to FUpdatePackage.FUpdateLazPackages.Count - 1 do
  begin
    UpdLazPkgs := TUpdateLazPackages(FUpdatePackage.FUpdateLazPackages.Items[I]);
    LazarusPkg := AMetaPackage.FindLazarusPackage(UpdLazPkgs.Name);
    if LazarusPkg <> nil then
    begin
      LazarusPkg.UpdateVersion := UpdLazPkgs.Version;
      LazarusPkg.ForceNotify := UpdLazPkgs.ForceNotify;
      LazarusPkg.InternalVersion := UpdLazPkgs.InternalVersion;
      LazarusPkg.RefreshHasUpdate;
      if not HasUpdate then
        HasUpdate := (LazarusPkg.HasUpdate) and (LazarusPkg.InstalledFileVersion < LazarusPkg.UpdateVersion);
    end;
  end;
  AMetaPackage.HasUpdate := HasUpdate;
end;

procedure TUpdates.ResetPackageData(AMetaPackage: TMetaPackage);
var
  I: Integer;
  LazarusPkg: TLazarusPackage;
begin
  if FBusySaving then
    Exit;
  AMetaPackage.DownloadZipURL := '';
  AMetaPackage.DisableInOPM := False;
  AMetaPackage.HasUpdate := False;
  for I := 0 to AMetaPackage.LazarusPackages.Count - 1 do
  begin
    LazarusPkg := AMetaPackage.FindLazarusPackage(TLazarusPackage(AMetaPackage.LazarusPackages.Items[I]).Name);
    if LazarusPkg <> nil then
    begin
      LazarusPkg.HasUpdate := False;
      LazarusPkg.UpdateVersion := '';
      LazarusPkg.ForceNotify := False;
      LazarusPkg.InternalVersion := 0;
      LazarusPkg.InternalVersionOld := 0;
    end;
  end;
end;

procedure TUpdates.CheckForOpenSSL;
{$IFDEF MSWINDOWS}
var
  ParamPath, libeaydll, ssleaydll, ZipFile: String;
  UnZipper: TUnZipper;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  ParamPath := ExtractFilePath(ParamStr(0));
  libeaydll := ParamPath + 'libeay32.dll';
  ssleaydll := ParamPath + 'ssleay32.dll';
  FOpenSSLAvailable := FileExists(libeaydll) and FileExists(ssleaydll);
  if not FOpenSSLAvailable then
  begin
    ZipFile := ParamPath + ExtractFileName(cOpenSSLURL);
    try
      FHTTPClient.Get(cOpenSSLURL, ZipFile);
    except
    end;
    if FileExists(ZipFile) then
    begin
      UnZipper := TUnZipper.Create;
      try
        try
          UnZipper.FileName := ZipFile;
          UnZipper.Examine;
          UnZipper.UnZipAllFiles;
        except
        end;
      finally
        UnZipper.Free;
      end;
      DeleteFile(ZipFile);
      FOpenSSLAvailable := FileExists(libeaydll) and FileExists(ssleaydll);
    end;
  end;
  {$ELSE}
  FOpenSSLAvailable := True;
  {$ENDIF}
end;

function TUpdates.IsTimeToUpdate: Boolean;
begin
  Result := Assigned(SerializablePackages) and (FOpenSSLAvailable) and
            (not FBusyUpdating) and (not FNeedToBreak);
  case Options.CheckForUpdates of
    0: Result := MinutesBetween(Now, Options.LastUpdate) >= 2;
    1: Result := HoursBetween(Now, Options.LastUpdate) >= 1;
    2: Result := DaysBetween(Now, Options.LastUpdate) >= 1;
    3: Result := WeeksBetween(Now, Options.LastUpdate) >= 1;
    4: Result := MonthsBetween(Now, Options.LastUpdate) >= 1;
    5: Result := False;
  end;
end;

function TUpdates.GetUpdateInfo(const AURL: String; var AJSON: TJSONStringType): Boolean;
var
  URL: String;
  Ms: TMemoryStream;
begin
  Result := False;
  if Trim(AURL) = '' then
    Exit;
  if Pos('.json', AURL) = 0 then
    Exit;
  URL := FixProtocol(AURL);
  Ms := TMemoryStream.Create;
  try
    try
      FHTTPClient.AllowRedirect := True;
      FHTTPClient.HTTPMethod('GET', URL, MS, []);
      if FHTTPClient.ResponseStatusCode = 200 then
      begin
        if Ms.Size > 0 then
        begin
          MS.Position := 0;
          SetLength(AJSON, MS.Size);
          MS.Read(Pointer(AJSON)^, Length(AJSON));
          Result := Length(AJSON) > 0;
          {since the class name has changed form "UpdatePackageFiles" to "UpdateLazPackages",
          we have to replace the references in the old JSONs(we don't have access to the files, they are
          located at the developers update page.}
          if Result then
            AJSON := StringReplace(AJSON, 'UpdatePackageFiles', 'UpdateLazPackages', [rfReplaceAll, rfIgnoreCase]);
        end;
      end;
    except
      Result := False;
    end;
  finally
    Ms.Free;
  end;
end;

procedure TUpdates.DoOnUpdate;
begin
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

procedure TUpdates.CheckForUpdates;
var
  I: Integer;
  JSON: TJSONStringType;
begin
  FBusyUpdating := True;
  try
    Options.LastUpdate := Now;
    Options.Changed := True;
    for I := 0 to SerializablePackages.Count - 1  do
    begin
      if FNeedToBreak then
        Break;
      JSON := '';
      if GetUpdateInfo(Trim(SerializablePackages.Items[I].DownloadURL), JSON) then
      begin
        if FUpdatePackage.LoadFromJSON(JSON) then
          AssignPackageData(SerializablePackages.Items[I])
        else
          ResetPackageData(SerializablePackages.Items[I]);
      end
      else
        ResetPackageData(SerializablePackages.Items[I]);
    end;
    if Assigned(FOnUpdate) and (not FNeedToBreak) then
      Synchronize(@DoOnUpdate);
  finally
    FBusyUpdating := False;
  end;
end;

procedure TUpdates.Execute;
begin
  while not Terminated do
  begin
    Sleep(1);
    if (GetTickCount64 - FTime > FInterval) then
    begin
      FTime := GetTickCount64;
      if IsTimeToUpdate then
        CheckForUpdates;
    end;
    if FNeedToBreak then
      Break;
  end;
end;

procedure TUpdates.StartUpdate;
begin
  Load;
  CheckForOpenSSL;
  FTime := GetTickCount64;
  FInterval := 6000;
  Start;
end;

procedure TUpdates.StopUpdate;
begin
  Save;
  FHTTPClient.Terminate;
  FNeedToBreak := True;
end;

end.

