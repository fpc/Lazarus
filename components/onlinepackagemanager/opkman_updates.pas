unit opkman_updates;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazIDEIntf, Laz2_XMLCfg, LazFileUtils, fpjson,
  opkman_httpclient, opkman_timer,

  dialogs;

const
  OpkVersion = 1;
  UpdateInterval = 6000;

type
  TUpdateInfo = record
    FPackageName: String;
    FPackageFileName: String;
    FUpdateVersion: String;
    FForceUpdate: Boolean;
  end;

  { TUpdates }

  TUpdates = class(TThread)
  private
    FXML: TXMLConfig;
    FHTTPClient: TFPHTTPClient;
    FTimer: TThreadTimer;
    FStarted: Boolean;
    FVersion: Integer;
    FNeedToBreak: Boolean;
    FNeedToUpdate: Boolean;
    FBusyUpdating: Boolean;
    procedure SetUpdateInfo(const AUpdateInfo: TUpdateInfo);
    function GetUpdateInfo(const AURL: String; var AJSON: TJSONStringType): Boolean;
    function ParseJSON(const AJSON: TJSONStringType; var AUpdateInfo: TUpdateInfo): Boolean;
    procedure DoOnTimer(Sender: TObject);
  protected
    procedure Execute; override;
  public
    procedure Load;
    procedure Save;
    constructor Create(const AFileName: String);
    destructor Destroy; override;
    procedure StartUpdate;
    procedure StopUpdate;
  end;

var
  Updates: TUpdates = nil;

implementation

uses opkman_serializablepackages, opkman_options, opkman_common;

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
  inherited Destroy;
end;

procedure TUpdates.SetUpdateInfo(const AUpdateInfo: TUpdateInfo);
var
  I, J: Integer;
  Package: TPackage;
  PackageFile: TPackageFile;
begin
  for I := 0 to SerializablePackages.Count - 1 do
  begin
    Package := SerializablePackages.Items[I];
    for J := 0 to SerializablePackages.Items[I].PackageFiles.Count - 1 do
    begin
      PackageFile := TPackageFile(SerializablePackages.Items[I].PackageFiles.Items[J]);
      if (UpperCase(Package.Name) = UpperCase(AUpdateInfo.FPackageName)) and
        (UpperCase(PackageFile.Name) = UpperCase(AUpdateInfo.FPackageFileName)) then
      begin
        Package.ForceUpdate := AUpdateInfo.FForceUpdate;
        PackageFile.UpdateVersion := AUpdateInfo.FUpdateVersion;
        Exit;
      end;
    end;
  end;
end;

procedure TUpdates.Load;
var
  Count: Integer;
  I: Integer;
  Path: String;
  UpdateInfo: TUpdateInfo;
begin
  FVersion := FXML.GetValue('Version/Value', 0);
  Count := FXML.GetValue('Count/Value', 0);
  for I := 0 to Count - 1 do
  begin
    Path := 'Item' + IntToStr(I);
    with UpdateInfo do
    begin
      FPackageName := FXML.GetValue('Items/' + Path + '/PackageName', '');
      FForceUpdate :=  FXML.GetValue('Items/' + Path + '/ForceUpdate', False);
      FPackageFileName := FXML.GetValue('Items/' + Path + '/PackageFileName', '');
      FUpdateVersion := FXML.GetValue('Items/' + Path + '/UpdateVersion', '');
    end;
    SetUpdateInfo(UpdateInfo);
  end;
end;

procedure TUpdates.Save;
var
  I, J: Integer;
  Count: Integer;
  Path: String;
  Package: TPackage;
  PackageFile: TPackageFile;
begin
  FNeedToBreak := True;
  FXML.Clear;
  Count := -1;
  FXML.SetDeleteValue('Version/Value', OpkVersion, 0);
  for I := 0 to SerializablePackages.Count - 1 do
  begin
    Package := SerializablePackages.Items[I];
    for J := 0 to SerializablePackages.Items[I].PackageFiles.Count - 1 do
    begin
      Inc(Count);
      Path := 'Item' + IntToStr(Count);
      PackageFile := TPackageFile(SerializablePackages.Items[I].PackageFiles.Items[J]);
      FXML.SetDeleteValue('Items/' + Path + '/PackageName', Package.Name, '');
      FXML.SetDeleteValue('Items/' + Path + '/ForceUpdate', Package.ForceUpdate, False);
      FXML.SetDeleteValue('Items/' + Path + '/PackageFileName', PackageFile.Name, '');
      FXML.SetDeleteValue('Items/' + Path + '/UpdateVersion', PackageFile.UpdateVersion, '');
    end;
  end;
  FXML.SetDeleteExtendedValue('Count/Value', Count, 0);
  FXML.Flush;
end;

procedure TUpdates.DoOnTimer(Sender: TObject);
begin
  if (FTimer.Enabled) and (not FNeedToBreak) then
    FNeedToUpdate := True;
end;

function TUpdates.GetUpdateInfo(const AURL: String; var AJSON: TJSONStringType): Boolean;
var
  URL: string;
  Ms: TMemoryStream;
begin
  Result := False;
  if Trim(AURL) = '' then
    Exit;

  if Pos('update.json', AURL) = 0 then
    Exit;

  URL := FixProtocol(AURL);
  Ms := TMemoryStream.Create;
  try
    try
      FHTTPClient.AllowRedirect := True;
      FHTTPClient.HTTPMethod('GET', URL, MS, []);
      if Ms.Size > 0 then
      begin
        MS.Position := 0;
        SetLength(AJSON, MS.Size);
        MS.Read(Pointer(AJSON)^, Length(AJSON));
        Result := Length(AJSON) > 0;
      end;
    except
      Result := False;
    end;
  finally
    Ms.Free;
  end;
end;

function TUpdates.ParseJSON(const AJSON: TJSONStringType; var AUpdateInfo: TUpdateInfo): Boolean;
begin
  Result := False;

end;

procedure TUpdates.Execute;
var
  I: Integer;
  UpdateInfo: TUpdateInfo;
  JSON: TJSONStringType;
begin
  Load;
  while not Terminated do
  begin
    if (FNeedToUpdate) and (not FBusyUpdating) then
    begin
      FBusyUpdating := True;
      try
        for I := 0 to SerializablePackages.Count - 1  do
        begin
          if not FNeedToBreak then
          begin
            JSON := '';
            if GetUpdateInfo(SerializablePackages.Items[I].DownloadURL, JSON) then
            begin
              if ParseJSON(JSON, UpdateInfo) then
              begin
                SetUpdateInfo(UpdateInfo);
              end;
            end;
          end;
        end;
      finally
        FBusyUpdating := False;
        FNeedToUpdate := False;
      end;
    end;
  end;
end;

procedure TUpdates.StartUpdate;
begin
  if FStarted then
    Exit;
  FStarted := True;
  Load;
  FTimer := TThreadTimer.Create;
  FTimer.Interval := UpdateInterval;
  FTimer.OnTimer := @DoOnTimer;
  FTimer.StartTimer;
  Start;
end;

procedure TUpdates.StopUpdate;
begin
  FNeedToBreak := True;
  FTimer.StopTimer;
  FHTTPClient.NeedToBreak := True;
  Save;
end;

end.

