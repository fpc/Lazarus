unit opkman_updates;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazIDEIntf, Laz2_XMLCfg, LazFileUtils, fpjson, fpjsonrtti,
  opkman_httpclient, opkman_timer, opkman_serializablepackages;

const
  OpkVersion = 1;
  UpdateInterval = 6000;

type

  { TUpdatePackageFiles }

  TUpdatePackageFiles = class(TCollectionItem)
  private
    FName: String;
    FVersion: String;
  published
    property Name: String read FName write FName;
    property Version: String read FVersion write FVersion;
  end;

  { TUpdatePackageData }

  TUpdatePackageData = class(TPersistent)
  private
    FDownloadZipURL: String;
    FForceNotify: boolean;
    FName: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
  published
    property Name: String read FName write FName;
    property ForceNotify: boolean read FForceNotify write FForceNotify;
    property DownloadZipURL: String read FDownloadZipURL write FDownloadZipURL;
  end;

  {TUpdatePackage}

  TUpdatePackage = class(TPersistent)
  private
    FUpdatePackageData: TUpdatePackageData;
    FUpdatePackageFiles: TCollection;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;
    function LoadFromJSON(const AJSON: TJSONStringType): boolean;
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
    FOnUpdate: TNotifyEvent;
    FPaused: Boolean;
    function GetUpdateInfo(const AURL: String; var AJSON: TJSONStringType): Boolean;
    procedure DoOnTimer(Sender: TObject);
    procedure DoOnUpdate;
    procedure Load;
    procedure Save;
    procedure SetPaused(const AValue: Boolean);
    procedure AssignPackageData(APackage: TPackage);
    procedure ResetPackageData(APackage: TPackage);
  protected
    procedure Execute; override;
  public
    constructor Create(const AFileName: String);
    destructor Destroy; override;
    procedure StartUpdate;
    procedure StopUpdate;
  published
    property Paused: Boolean read FPaused write SetPaused;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;

var
  Updates: TUpdates = nil;

implementation

uses opkman_options, opkman_common;

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

function TUpdatePackage.LoadFromJSON(const AJSON: TJSONStringType): boolean;
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
      Result := False;
    end;
  finally
    DeStreamer.Free;
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
  FForceNotify := False;
  FDownloadZipURL := '';
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
  Count: Integer;
  I: Integer;
  Path: String;
  PackageName: String;
  PackageFileName: String;
  Package: TPackage;
  PackageFile: TPackageFile;
begin
  FVersion := FXML.GetValue('Version/Value', 0);
  Count := FXML.GetValue('Count/Value', 0);
  for I := 0 to Count - 1 do
  begin
    Path := 'Item' + IntToStr(I);
    PackageName := FXML.GetValue('Items/' + Path + '/PackageName', '');
    Package := SerializablePackages.FindPackage(PackageName, fpbPackageName);
    if Package <> nil then
    begin
      Package.ForceNotify := FXML.GetValue('Items/' + Path + '/ForceNotify', False);
      Package.DownloadZipURL := FXML.GetValue('Items/' + Path + '/DownloadZipURL', '');
      PackageFileName := FXML.GetValue('Items/' + Path + '/PackageFileName', '');
      PackageFile := Package.FindPackageFile(PackageFileName);
      if PackageFile <> nil then
        PackageFile.UpdateVersion := FXML.GetValue('Items/' + Path + '/UpdateVersion', '');
    end;
  end;
  Synchronize(@DoOnUpdate);
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
      FXML.SetDeleteValue('Items/' + Path + '/ForceNotify', Package.ForceNotify, False);
      FXML.SetDeleteValue('Items/' + Path + '/DownloadZipURL', Package.DownloadZipURL, '');
      FXML.SetDeleteValue('Items/' + Path + '/PackageFileName', PackageFile.Name, '');
      FXML.SetDeleteValue('Items/' + Path + '/UpdateVersion', PackageFile.UpdateVersion, '');
    end;
  end;
  FXML.SetDeleteValue('Count/Value', Count + 1, 0);
  FXML.Flush;
end;

procedure TUpdates.SetPaused(const AValue: Boolean);
begin
  if FPaused <> AValue then
  begin
    FPaused := AValue;
    if FPaused then
      Save;
  end;
end;

procedure TUpdates.AssignPackageData(APackage: TPackage);
var
  I: Integer;
  PackageFile: TPackageFile;
begin
  APackage.DownloadZipURL := FUpdatePackage.FUpdatePackageData.DownloadZipURL;
  APackage.ForceNotify := FUpdatePackage.FUpdatePackageData.ForceNotify;
  for I := 0 to FUpdatePackage.FUpdatePackageFiles.Count - 1 do
  begin
    PackageFile := APackage.FindPackageFile(TUpdatePackageFiles(FUpdatePackage.FUpdatePackageFiles.Items[I]).Name);
    if PackageFile <> nil then
      PackageFile.UpdateVersion := TUpdatePackageFiles(FUpdatePackage.FUpdatePackageFiles.Items[I]).Version;
  end;
end;

procedure TUpdates.ResetPackageData(APackage: TPackage);
var
  I: Integer;
  PackageFile: TPackageFile;
begin
  APackage.DownloadZipURL := '';
  APackage.ForceNotify := False;
  for I := 0 to APackage.PackageFiles.Count - 1 do
  begin
    PackageFile := APackage.FindPackageFile(TPackageFile(APackage.PackageFiles.Items[I]).Name);
    if PackageFile <> nil then
      PackageFile.UpdateVersion := '';
  end;

end;

procedure TUpdates.DoOnTimer(Sender: TObject);
begin
  if (FTimer.Enabled) and (not FNeedToBreak) then
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
  while not Terminated do
  begin
    if (FNeedToUpdate) and (not FBusyUpdating) and (not FPaused) then
    begin
      FBusyUpdating := True;
      try
        for I := 0 to SerializablePackages.Count - 1  do
        begin
          if FPaused then
            Break;
          if (not FNeedToBreak) then
          begin
            JSON := '';
            if GetUpdateInfo(SerializablePackages.Items[I].DownloadURL, JSON) then
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
            FHTTPClient.NeedToBreak := True;
        end;
        if Assigned(FOnUpdate) and (not FNeedToBreak) and (not FPaused) then
          Synchronize(@DoOnUpdate);
      finally
        FBusyUpdating := False;
        FNeedToUpdate := False;
      end;
    end;
  end;
end;

procedure TUpdates.StartUpdate;
begin
  Load;
  FPaused := False;
  if FStarted then
    Exit;
  FStarted := True;
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
  FStarted := False;
  FHTTPClient.NeedToBreak := True;
  Save;
end;

end.

