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
  Classes, SysUtils, fpjson, fpjsonrtti, jsonparser, dateutils,
  // LazUtils
  LazIDEIntf,
  // OpkMan
  opkman_serializablepackages, opkman_options, opkman_common, opkman_visualtree,
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
    FSP_Temp: TSerializablePackages;
    FHTTPClient: TFPHTTPClient;
    FUpdatePackage: TUpdatePackage;
    FNeedToBreak: Boolean;
    FBusyUpdating: Boolean;
    FOpenSSLAvailable: Boolean;
    FTime: QWORD;
    FInterval: Cardinal;
    FStarted: Boolean;
    function GetUpdateInfo(const AURL: String; var AJSON: TJSONStringType): Boolean;
    procedure AssignPackageData(AMetaPackage: TMetaPackage);
    procedure ResetPackageData(AMetaPackage: TMetaPackage);
    procedure CheckForOpenSSL;
    procedure CheckForUpdates;
    procedure GetSerializablePackages;
    procedure SetSerializablePackages;
    function IsTimeToUpdate: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartUpdate;
    procedure StopUpdate;
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

function IsValidJSON(const AJSON: TJSONStringType): Boolean;
var
  {%H-}JSONData: TJSONData;
begin
  Result := True;
  try
    JSONData := GetJSON(AJSON);
    JSONData.Free;
  except
    on E: EJSONParser do
      Result := False;
  end;
end;

function TUpdatePackage.LoadFromJSON(const AJSON: TJSONStringType): Boolean;
var
  DeStreamer: TJSONDeStreamer;
begin
  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    Clear;
    try
      if IsValidJSON(AJSON) then
      begin
        DeStreamer.JSONToObject(AJSON, Self);
        Result := True;
      end;
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

constructor TUpdates.Create;
begin
  inherited Create(True);
  FSP_Temp := TSerializablePackages.Create;
  FreeOnTerminate := True;
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
  FSP_Temp.Clear;
  FSP_Temp.Free;
  Updates := nil;
  inherited Destroy;
end;

procedure TUpdates.AssignPackageData(AMetaPackage: TMetaPackage);
var
  I: Integer;
  HasUpdate: Boolean;
  LazarusPkg: TLazarusPackage;
  UpdLazPkgs: TUpdateLazPackages;
begin
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
  Result := (FOpenSSLAvailable) and (not FBusyUpdating) and (not FNeedToBreak);
  case Options.CheckForUpdates of
    0: Result := MinutesBetween(Now, Options.LastUpdate) >= 5;
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

procedure TUpdates.CheckForUpdates;
var
  I: Integer;
  JSON: TJSONStringType;
begin
  if FSP_Temp.Count = 0 then
    Exit;

  FBusyUpdating := True;
  try
    Options.LastUpdate := Now;
    Options.Changed := True;
    for I := 0 to FSP_Temp.Count - 1  do
    begin
      if FNeedToBreak then
        Break;
      JSON := '';
      if (Assigned(LazarusIDE) and LazarusIDE.IDEIsClosing) then
        Break;
      if GetUpdateInfo(Trim(FSP_Temp.Items[I].DownloadURL), JSON) then
      begin
        if FUpdatePackage.LoadFromJSON(JSON) then
          AssignPackageData(FSP_Temp.Items[I])
        else
          ResetPackageData(FSP_Temp.Items[I]);
      end
      else
        ResetPackageData(FSP_Temp.Items[I]);
    end;
  finally
    FBusyUpdating := False;
  end;
end;

procedure TUpdates.GetSerializablePackages;
var
  JSON: TJSONStringType;
begin
  if (FNeedToBreak) or (SerializablePackages.Count = 0) then
    Exit;

  EnterCriticalSection(CriticalSection);
  try
    FSP_Temp.Clear;
    try
      JSON := '';
      SerializablePackages.PackagesToJSON(JSON);
      FSP_Temp.JSONToPackages(JSON);
    except
    end;
  finally
    LeaveCriticalSection(CriticalSection);
  end;
end;

procedure TUpdates.SetSerializablePackages;
var
  I, J: Integer;
  MetaPackage: TMetaPackage;
  HasUpdate: Boolean;
  LazarusPackage: TLazarusPackage;
begin
  if (FNeedToBreak) or (SerializablePackages.Count = 0) or (FSP_Temp.Count = 0) then
    Exit;
  EnterCriticalSection(CriticalSection);
  try
    for I := 0 to FSP_Temp.Count - 1 do
    begin
      MetaPackage := SerializablePackages.FindMetaPackage(FSP_Temp.Items[I].Name, fpbPackageName);
      if MetaPackage <> nil then
      begin
        MetaPackage.DownloadZipURL := FSP_Temp.Items[I].DownloadZipURL;
        MetaPackage.DisableInOPM := FSP_Temp.Items[I].DisableInOPM;
        HasUpdate := False;
        for J := 0 to FSP_Temp.Items[I].LazarusPackages.Count - 1 do
        begin
          LazarusPackage := MetaPackage.FindLazarusPackage(TLazarusPackage(FSP_Temp.Items[I].LazarusPackages.Items[J]).Name);
          if LazarusPackage <> nil then
          begin
            LazarusPackage.UpdateVersion := TLazarusPackage(FSP_Temp.Items[I].LazarusPackages.Items[J]).UpdateVersion;
            LazarusPackage.ForceNotify := TLazarusPackage(FSP_Temp.Items[I].LazarusPackages.Items[J]).ForceNotify;
            LazarusPackage.InternalVersion := TLazarusPackage(FSP_Temp.Items[I].LazarusPackages.Items[J]).InternalVersion;
            LazarusPackage.RefreshHasUpdate;
            if not HasUpdate then
              HasUpdate := (LazarusPackage.HasUpdate) and (LazarusPackage.InstalledFileVersion < LazarusPackage.UpdateVersion);
          end;
        end;
        MetaPackage.HasUpdate := HasUpdate;
      end;
    end;
  finally
    LeaveCriticalSection(CriticalSection);
  end;
end;

procedure TUpdates.Execute;
begin
  while not Terminated do
  begin
    if FNeedToBreak then
      Break;
    Sleep(50);
    if (GetTickCount64 - FTime > FInterval)then
    begin
      FTime := GetTickCount64;
      if (IsTimeToUpdate) then
      begin
        GetSerializablePackages;
        CheckForUpdates;
        SetSerializablePackages;
        if (not FNeedToBreak) and Assigned(VisualTree) then
          Synchronize(@VisualTree.UpdatePackageUStatus);
      end;
    end;
  end;
end;

procedure TUpdates.StartUpdate;
begin
  FStarted := True;
  CheckForOpenSSL;
  FTime := GetTickCount64;
  FInterval := 6000;
  Start;
end;

procedure TUpdates.StopUpdate;
begin
  FStarted := False;
  FHTTPClient.Terminate;
  FNeedToBreak := True;
end;

end.

