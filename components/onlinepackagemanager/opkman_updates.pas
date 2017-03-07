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
  opkman_timer, opkman_serializablepackages,
  opkman_options, opkman_common,
  {$IFDEF MSWINDOWS}
    LazFileUtils, opkman_const,
    {$IFDEF FPC311}zipper{$ELSE}opkman_zip{$ENDIF},
  {$ENDIF}
  {$IFDEF FPC311}fphttpclient{$ELSE}opkman_httpclient{$ENDIF};

const
  OpkVersion = 1;

type

  { TUpdatePackageFiles }

  TUpdatePackageFiles = class(TCollectionItem)
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
    FUpdatePackageFiles: TCollection;
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
    property UpdatePackageFiles: TCollection read FUpdatePackageFiles write FUpdatePackageFiles;
  end;

  { TUpdates }
  TUpdates = class(TThread)
  private
    FXML: TXMLConfig;
    FHTTPClient: TFPHTTPClient;
    FTimer: TThreadTimer;
    FUpdatePackage: TUpdatePackage;
    FStarted: Boolean;
    FVersion: Integer;
    FNeedToBreak: Boolean;
    FNeedToUpdate: Boolean;
    FBusyUpdating: Boolean;
    FOpenSSLAvaialable: Boolean;
    FOnUpdate: TNotifyEvent;
    FPaused: Boolean;
    function GetUpdateInfo(const AURL: String; var AJSON: TJSONStringType): Boolean;
    procedure DoOnTimer(Sender: TObject);
    procedure DoOnUpdate;
    procedure Load;
    procedure Save;
    procedure AssignPackageData(APackage: TPackage);
    procedure ResetPackageData(APackage: TPackage);
    procedure CheckForOpenSSL;
    function IsTimeToUpdate: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(const AFileName: String);
    destructor Destroy; override;
    procedure StartUpdate;
    procedure StopUpdate;
    procedure PauseUpdate;
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
  for I := FUpdatePackageFiles.Count - 1 downto 0 do
    FUpdatePackageFiles.Items[I].Free;
  FUpdatePackageFiles.Clear;
end;

constructor TUpdatePackage.Create;
begin
  FUpdatePackageData := TUpdatePackageData.Create;
  FUpdatePackageFiles := TCollection.Create(TUpdatePackageFiles);
end;

destructor TUpdatePackage.Destroy;
var
  I: Integer;
begin
  FUpdatePackageData.Free;
  for I := FUpdatePackageFiles.Count - 1 downto 0 do
    FUpdatePackageFiles.Items[I].Free;
  FUpdatePackageFiles.Free;
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
  FXML := TXMLConfig.Create(AFileName);
  FHTTPClient := TFPHTTPClient.Create(nil);
  if Options.ProxyEnabled then
  begin
    FHTTPClient.Proxy.Host:= Options.ProxyServer;
    FHTTPClient.Proxy.Port:= Options.ProxyPort;
    FHTTPClient.Proxy.UserName:= Options.ProxyUser;
    FHTTPClient.Proxy.Password:= Options.ProxyPassword;
  end;
  FUpdatePackage := TUpdatePackage.Create;
  FTimer := nil;
end;

destructor TUpdates.Destroy;
begin
  FXML.Free;
  if Assigned(FTimer) then
  begin
    if FTimer.Enabled then
      FTimer.StopTimer;
    FTimer.Terminate;
  end;
  FHTTPClient.Free;
  FUpdatePackage.Free;
  inherited Destroy;
end;

procedure TUpdates.Load;
var
  PackageCount: Integer;
  PackageFileCount: Integer;
  I, J: Integer;
  Path, SubPath: String;
  PackageName: String;
  PackageFileName: String;
  Package: TPackage;
  PackageFile: TPackageFile;
  HasUpdate: Boolean;
begin
  if (not Assigned(SerializablePackages)) or (SerializablePackages.Count = 0) then
    Exit;

  FVersion := FXML.GetValue('Version/Value', 0);
  PackageCount := FXML.GetValue('Count/Value', 0);
  for I := 0 to PackageCount - 1 do
  begin
    Path := 'Package' + IntToStr(I) + '/';
    PackageName := FXML.GetValue(Path + 'Name', '');
    Package := SerializablePackages.FindPackage(PackageName, fpbPackageName);
    if Package <> nil then
    begin
      HasUpdate := False;
      Package.DownloadZipURL := FXML.GetValue(Path + 'DownloadZipURL', '');
      Package.DisableInOPM := FXML.GetValue(Path + 'DisableInOPM', False);
      Package.Rating := FXML.GetValue(Path + 'Rating', 0);
      PackageFileCount := FXML.GetValue(Path + 'Count', 0);
      for J := 0 to PackageFileCount - 1 do
      begin
        SubPath := Path + 'PackageFile' +  IntToStr(J) + '/';
        PackageFileName := FXML.GetValue(SubPath + 'Name', '');
        PackageFile := Package.FindPackageFile(PackageFileName);
        if PackageFile <> nil then
        begin
          PackageFile.UpdateVersion := FXML.GetValue(SubPath + 'UpdateVersion', '');
          PackageFile.ForceNotify := FXML.GetValue(SubPath + 'ForceNotify', False);
          PackageFile.InternalVersion := FXML.GetValue(SubPath + 'InternalVersion', 0);;
          PackageFile.InternalVersionOld := FXML.GetValue(SubPath + 'InternalVersionOld', 0);
          PackageFile.HasUpdate := (PackageFile.UpdateVersion <> '') and (PackageFile.InstalledFileVersion <> '') and
                                   (
                                     ((not PackageFile.ForceNotify) and (PackageFile.UpdateVersion > PackageFile.InstalledFileVersion)) or
                                     ((PackageFile.ForceNotify) and (PackageFile.InternalVersion > PackageFile.InternalVersionOld))
                                   );
          if not HasUpdate then
            HasUpdate := PackageFile.HasUpdate;
        end;
      end;
      Package.HasUpdate := HasUpdate;
    end;
  end;
  Synchronize(@DoOnUpdate);
end;

procedure TUpdates.Save;
var
  I, J: Integer;
  Path, SubPath: String;
  Package: TPackage;
  PackageFile: TPackageFile;
begin
  if (not Assigned(SerializablePackages)) or (SerializablePackages.Count = 0) then
    Exit;
  FXML.Clear;
  FXML.SetDeleteValue('Version/Value', OpkVersion, 0);
  FXML.SetDeleteValue('Count/Value', SerializablePackages.Count, 0);
  for I := 0 to SerializablePackages.Count - 1 do
  begin
    Package := SerializablePackages.Items[I];
    Path := 'Package' + IntToStr(I) + '/';
    FXML.SetDeleteValue(Path + 'Name', Package.Name, '');
    FXML.SetDeleteValue(Path + 'DownloadZipURL', Package.DownloadZipURL, '');
    FXML.SetDeleteValue(Path + 'DisableInOPM', Package.DisableInOPM, False);
    FXML.SetDeleteValue(Path + 'Rating', Package.Rating, 0);
    FXML.SetDeleteValue(Path + 'Count', SerializablePackages.Items[I].PackageFiles.Count, 0);
    for J := 0 to SerializablePackages.Items[I].PackageFiles.Count - 1 do
    begin
      SubPath := Path + 'PackageFile' +  IntToStr(J) + '/';
      PackageFile := TPackageFile(SerializablePackages.Items[I].PackageFiles.Items[J]);
      FXML.SetDeleteValue(SubPath + 'Name', PackageFile.Name, '');
      FXML.SetDeleteValue(SubPath + 'UpdateVersion', PackageFile.UpdateVersion, '');
      FXML.SetDeleteValue(SubPath + 'ForceNotify', PackageFile.ForceNotify, False);
      FXML.SetDeleteValue(SubPath + 'InternalVersion', PackageFile.InternalVersion, 0);
      FXML.SetDeleteValue(SubPath + 'InternalVersionOld', PackageFile.InternalVersionOld, 0);
    end;
  end;
  FXML.Flush;
end;

procedure TUpdates.AssignPackageData(APackage: TPackage);
var
  I: Integer;
  HasUpdate: Boolean;
  PackageFile: TPackageFile;
begin
  HasUpdate := False;
  APackage.DownloadZipURL := FUpdatePackage.FUpdatePackageData.DownloadZipURL;
  APackage.DisableInOPM := FUpdatePackage.FUpdatePackageData.DisableInOPM;
  for I := 0 to FUpdatePackage.FUpdatePackageFiles.Count - 1 do
  begin
    PackageFile := APackage.FindPackageFile(TUpdatePackageFiles(FUpdatePackage.FUpdatePackageFiles.Items[I]).Name);
    if PackageFile <> nil then
    begin
      PackageFile.UpdateVersion := TUpdatePackageFiles(FUpdatePackage.FUpdatePackageFiles.Items[I]).Version;
      PackageFile.ForceNotify := TUpdatePackageFiles(FUpdatePackage.FUpdatePackageFiles.Items[I]).ForceNotify;
      PackageFile.InternalVersion := TUpdatePackageFiles(FUpdatePackage.FUpdatePackageFiles.Items[I]).InternalVersion;
      PackageFile.HasUpdate := (PackageFile.UpdateVersion <> '') and (PackageFile.InstalledFileVersion <> '') and
                               (
                                 ((not PackageFile.ForceNotify) and (PackageFile.UpdateVersion > PackageFile.InstalledFileVersion)) or
                                 ((PackageFile.ForceNotify) and (PackageFile.InternalVersion > PackageFile.InternalVersionOld))
                               );
      if not HasUpdate then
        HasUpdate := PackageFile.HasUpdate;
    end;
  end;
  APackage.HasUpdate := HasUpdate;
end;

procedure TUpdates.ResetPackageData(APackage: TPackage);
var
  I: Integer;
  PackageFile: TPackageFile;
begin
  APackage.DownloadZipURL := '';
  APackage.DisableInOPM := False;
  APackage.HasUpdate := False;
  for I := 0 to APackage.PackageFiles.Count - 1 do
  begin
    PackageFile := APackage.FindPackageFile(TPackageFile(APackage.PackageFiles.Items[I]).Name);
    if PackageFile <> nil then
    begin
      PackageFile.HasUpdate := False;
      PackageFile.UpdateVersion := '';
      PackageFile.ForceNotify := False;
      PackageFile.InternalVersion := 0;
      PackageFile.InternalVersionOld := 0;
    end;
  end;
end;

procedure TUpdates.CheckForOpenSSL;
{$IFDEF MSWINDOWS}
var
  ZipFile: String;
  UnZipper: TUnZipper;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
   FOpenSSLAvaialable := FileExistsUTF8(ExtractFilePath(ParamStr(0)) + 'libeay32.dll') and
                         FileExistsUTF8(ExtractFilePath(ParamStr(0)) + 'ssleay32.dll');
   if not FOpenSSLAvaialable then
   begin
     ZipFile := ExtractFilePath(ParamStr(0)) + ExtractFileName(cOpenSSLURL);
     try
       FHTTPClient.Get(cOpenSSLURL, ZipFile);
     except
     end;
     if FileExistsUTF8(ZipFile) then
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
       DeleteFileUTF8(ZipFile);
       FOpenSSLAvaialable := FileExistsUTF8(ExtractFilePath(ParamStr(0)) + 'libeay32.dll') and
                             FileExistsUTF8(ExtractFilePath(ParamStr(0)) + 'ssleay32.dll');
     end;
  end;
  {$ELSE}
  FOpenSSLAvaialable := True;
  {$ENDIF}
end;

function TUpdates.IsTimeToUpdate: Boolean;
begin
  case Options.CheckForUpdates of
    0: Result := MinutesBetween(Now, Options.LastUpdate) >= 2;
    1: Result := HoursBetween(Now, Options.LastUpdate) >= 1;
    2: Result := DaysBetween(Now, Options.LastUpdate) >= 1;
    3: Result := WeeksBetween(Now, Options.LastUpdate) >= 1;
    4: Result := MonthsBetween(Now, Options.LastUpdate) >= 1;
    5: Result := False;
  end;
end;

procedure TUpdates.DoOnTimer(Sender: TObject);
begin
  if (FTimer.Enabled) and (not FNeedToBreak) and (IsTimeToUpdate) then
    FNeedToUpdate := True;
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

procedure TUpdates.Execute;
var
  I: Integer;
  JSON: TJSONStringType;
begin
  Load;
  CheckForOpenSSL;
  while not Terminated do
  begin
    if Assigned(SerializablePackages) and (FNeedToUpdate) and (not FBusyUpdating) and (not FPaused) and (FOpenSSLAvaialable) then
    begin
      Options.LastUpdate := Now;
      Options.Changed := True;
      FBusyUpdating := True;
      try
        for I := 0 to SerializablePackages.Count - 1  do
        begin
          if FPaused then
            Break;
          if (not FNeedToBreak) then
          begin
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
          end
          else
            FHTTPClient.Terminate;
        end;
        if Assigned(FOnUpdate) and (not FNeedToBreak) and (not FPaused) then
          Synchronize(@DoOnUpdate);
      finally
        FBusyUpdating := False;
        FNeedToUpdate := False;
      end;
    end;
    Sleep(1000);
  end;
end;

procedure TUpdates.StartUpdate;
begin
  Load;
  FPaused := False;
  if FStarted then
    Exit;
  FOpenSSLAvaialable := False;
  FStarted := True;
  FTimer := TThreadTimer.Create;
  FTimer.Interval := 5000;
  FTimer.OnTimer := @DoOnTimer;
  FTimer.StartTimer;
  Start;
end;

procedure TUpdates.StopUpdate;
begin
  FNeedToBreak := True;
  Save;
  FTimer.StopTimer;
  FStarted := False;
  FHTTPClient.Terminate;
end;

procedure TUpdates.PauseUpdate;
begin
  FPaused := True;
  Save;
end;

end.

