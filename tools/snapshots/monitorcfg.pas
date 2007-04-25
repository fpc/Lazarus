unit MonitorCfg;

{$mode objfpc}{$H+}

interface

uses
  contnrs, dom, xmlread;
  
type
  TServerType = (stFtp, stHttp);
  
  TServer = class;
  TFile = class;
  
  { TMonitorConfig }

  TMonitorConfig = class
  private
    FFileName: string;
    FServers: TFPObjectList;
    function GetServer(index: integer): TServer;
    function GetServerCount: integer;
    procedure AddServer(const ServerNode: TDOMNode);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(const AFileName: string);
    procedure AddServer(AServer: TServer);
    property FileName: string read FFileName write FFileName;
    property Servers[index: integer] : TServer read GetServer;
    property ServerCount: integer read GetServerCount;
  end;

  { TServer }

  TServer = class
  private
    FFiles: TFPObjectList;
    FDescription: string;
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
  Attribute: TDOMNode;
begin
  NewFile := TFile.Create;
  Attribute := ServerNode.Attributes.GetNamedItem('Description');
  if assigned(Attribute) then
    NewFile.Description := Attribute.NodeValue;
  Attribute := ServerNode.Attributes.GetNamedItem('Mask');
  if assigned(Attribute) then
    NewFile.Mask := Attribute.NodeValue;
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
        AddServer(Node);
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

