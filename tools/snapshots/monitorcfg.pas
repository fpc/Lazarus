unit MonitorCfg;

{$mode objfpc}{$H+}

interface

uses
  strutils, contnrs, dom, xmlread;
  
type
  TServerType = (stFtp, stHttp);
  
  TServer = class;
  TFile = class;
  
  TReplaceStringEvent = function (const value: string):string of object;
  
  { TMonitorConfig }

  TMonitorConfig = class
  private
    FFileName: string;
    FFPCDevelVersion: string;
    FFPCFixesVersion: string;
    FFPCReleaseVersion: string;
    FLazVersion: string;
    FServers: TFPObjectList;
    function GetServer(index: integer): TServer;
    function GetServerCount: integer;
    procedure AddServer(const ServerNode: TDOMNode);
    procedure ReadVersions(const VersionNode: TDOMNode);
    function ServerReplaceString(const value: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(const AFileName: string);
    procedure AddServer(AServer: TServer);
    property FileName: string read FFileName write FFileName;
    property LazVersion: string read FLazVersion;
    property FPCReleaseVersion: string read FFPCReleaseVersion;
    property FPCFixesVersion: string read FFPCFixesVersion;
    property FPCDevelVersion: string read FFPCDevelVersion;
    property Servers[index: integer] : TServer read GetServer;
    property ServerCount: integer read GetServerCount;
  end;

  { TServer }

  TServer = class
  private
    FFiles: TFPObjectList;
    FDescription: string;
    FOnReplaceString: TReplaceStringEvent;
    FServerType: TServerType;
    function GetFile(index: integer): TFile;
    function GetFileCount: integer;
    procedure AddFile(const ServerNode: TDOMNode);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddFile(AFile: TFile);
    property Description : string read FDescription write FDescription;
    property ServerType : TServerType read FServerType write FServerType;
    property Files[index: integer] : TFile read GetFile;
    property FileCount: integer read GetFileCount;
    property OnReplaceString: TReplaceStringEvent read FOnReplaceString write FOnReplaceString;
  end;

  { TFile }

  TFile = class
  private
    FDescription: string;
    FMask: string;
  public
    constructor Create;
    property Mask: string read FMask write FMask;
    property Description: string read FDescription write FDescription;
  end;

implementation

function GetAttributeValue(const ANode: TDomNode; const AName: string): string;
var
  Attribute: TDOMNode;
begin
  Attribute := ANode.Attributes.GetNamedItem(AName);
  if assigned(Attribute) then
    Result := Attribute.NodeValue;
end;

{ TServer }

function TServer.GetFile(index: integer): TFile;
begin
  Result := TFile(FFiles[index]);
end;

function TServer.GetFileCount: integer;
begin
  Result := FFiles.Count;
end;

procedure TServer.AddFile(const ServerNode: TDOMNode);
var
  NewFile: TFile;
begin
  NewFile := TFile.Create;
  NewFile.Description := OnReplaceString(GetAttributeValue(ServerNode, 'Description'));
  NewFile.Mask := OnReplaceString(GetAttributeValue(ServerNode, 'Mask'));

  AddFile(NewFile);
end;

constructor TServer.Create;
begin
  FFiles := TFPObjectList.Create;
end;

destructor TServer.Destroy;
begin
  FFiles.Free;
  inherited Destroy;
end;

procedure TServer.AddFile(AFile: TFile);
begin
  FFiles.Add(AFile);
end;

{ TFile }

constructor TFile.Create;
begin
end;

{ TMonitorConfig }

function TMonitorConfig.GetServer(index: integer): TServer;
begin
  Result := TServer(FServers[index]);
end;

function TMonitorConfig.GetServerCount: integer;
begin
  Result := FServers.Count;
end;

procedure TMonitorConfig.AddServer(const ServerNode: TDOMNode);
var
  Server: TServer;
  Attribute: TDOMNode;
  Node: TDomNode;
begin
  Server := TServer.Create;
  Server.OnReplaceString := @ServerReplaceString;
  Attribute := ServerNode.Attributes.GetNamedItem('Name');
  if assigned(Attribute) then
    Server.Description := Attribute.NodeValue;
  Attribute := ServerNode.Attributes.GetNamedItem('Type');
  if assigned(Attribute) then
    if Attribute.NodeValue='ftp' then
      Server.ServerType := stFtp
    else if Attribute.NodeValue='http' then
      Server.ServerType := stHttp;
  Node := ServerNode.FirstChild;
  while Node<>nil do begin
    if Node.NodeName='File' then
      Server.AddFile(Node);
    Node := Node.NextSibling;
  end;
  AddServer(Server);
end;

procedure TMonitorConfig.ReadVersions(const VersionNode: TDOMNode);
begin
  FLazVersion := GetAttributeValue(VersionNode, 'Lazarus');
  FFPCReleaseVersion := GetAttributeValue(VersionNode, 'FPC_Release');
  FFPCFixesVersion := GetAttributeValue(VersionNode, 'FPC_Fixes');
  FFPCDevelVersion := GetAttributeValue(VersionNode, 'FPC_Devel');
end;

function TMonitorConfig.ServerReplaceString(const value: string): string;
begin
  Result := AnsiReplaceStr(Value, '$LAZVER', LazVersion);
  Result := AnsiReplaceStr(Result, '$FPCRELEASEVER', FPCReleaseVersion);
  Result := AnsiReplaceStr(Result, '$FPCFIXESVER', FPCFixesVersion);
  Result := AnsiReplaceStr(Result, '$FPCDEVELVER', FPCDevelVersion);
end;

constructor TMonitorConfig.Create;
begin
  FServers := TFPObjectList.Create;
end;

destructor TMonitorConfig.Destroy;
begin
  FServers.Free;
  inherited Destroy;
end;

procedure TMonitorConfig.Load(const AFileName: string);
var
  XmlDoc: TXMLDocument;
  Node: TDomNode;
begin
  FFileName := AFileName;
  XmlDoc := nil;
  try
    ReadXMLFile(XmlDoc, FileName);
    Node := XmlDoc.DocumentElement.FirstChild;
    while Node<>nil do begin
      if Node.NodeName='Server' then
        AddServer(Node)
      else if Node.NodeName='Version' then
        ReadVersions(Node);
      Node := Node.NextSibling;
    end;
  finally
    XmlDoc.Free;
  end;
end;

procedure TMonitorConfig.AddServer(AServer: TServer);
begin
  FServers.Add(AServer);
end;

end.

