{
  Author: Mattias Gaertner
}
unit SimpleWebSrvOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LazMethodList, LazConfigStorage, FileUtil, FileProcs,
  BaseIDEIntf;

const
  SWSConfigVersion = 1;
  SWSDefaultServerAddr = '127.0.0.1';
  SWSDefaultServerPort = 7777;
  SWSDefaultAPIPath = '_lazarus_locations';
  SWSOptionsFile = 'simplewebservergui.xml';
  SWSLogMaxLines = 10000;
  SWSCompileServerIni = 'simplewebservergui.ini';
  SWSMainServerPath = 'simplewebserver';
  SWSRecentListCapacity = 30;

type
  TSWSRecentList = (
    swsrlServerAddr,
    swsrlServerExe,
    swsrlServerPort,
    swsrlUserLocation,
    swsrlUserPath,
    swsrlUserParams
    );
  TSWSRecentLists = set of TSWSRecentList;

  { TSimpleWebServerOptions }

  TSimpleWebServerOptions = class(TPersistent)
  private
    fApplyHandlers: TMethodList;
    FBindAny: boolean;
    FChangeStep: integer;
    FPort: word;
    FLastSavedChangeStep: integer;
    FRecentLists: array[TSWSRecentList] of TStringList;
    FServerAddr: string;
    FServerExe: string;
    function GetModified: boolean;
    function GetRecentLists(rl: TSWSRecentList): TStrings;
    procedure SetBindAny(const AValue: boolean);
    procedure SetModified(const AValue: boolean);
    procedure SetPort(const AValue: word);
    procedure SetRecentLists(rl: TSWSRecentList; const AValue: TStrings);
    procedure SetServerAddr(const AValue: string);
    procedure SetServerExe(const AValue: string);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create;
    destructor Destroy; override;
    function Equals(Obj: TObject): boolean; override;
    procedure SaveSafe;
    procedure LoadSafe;
    procedure SaveToFile(Filename: string);
    procedure LoadFromFile(Filename: string);
    procedure Clear;
    function GetDefaultServerExe: string;
    property ChangeStep: integer read FChangeStep;
    procedure IncreaseChangeStep;
    procedure Apply;
    procedure AddHandlerApply(const OnApplyEvent: TNotifyEvent; AsLast: boolean = false);
    procedure RemoveHandlerApply(const OnApplyEvent: TNotifyEvent);
    procedure AddRecent(rl: TSWSRecentList; Value: string);
    property Modified: boolean read GetModified write SetModified;
  public
    property BindAny: boolean read FBindAny write SetBindAny;
    property ServerExe: string read FServerExe write SetServerExe;
    property ServerAddr: string read FServerAddr write SetServerAddr;
    property Port: word read FPort write SetPort;
    property RecentLists[rl: TSWSRecentList]: TStrings read GetRecentLists write SetRecentLists;
  end;

implementation

{ TSimpleWebServerOptions }

function TSimpleWebServerOptions.GetModified: boolean;
begin
  Result:=FLastSavedChangeStep<>FChangeStep;
end;

function TSimpleWebServerOptions.GetRecentLists(rl: TSWSRecentList): TStrings;
begin
  Result:=FRecentLists[rl];
end;

procedure TSimpleWebServerOptions.SetBindAny(const AValue: boolean);
begin
  if FBindAny=AValue then Exit;
  FBindAny:=AValue;
  IncreaseChangeStep;
end;

procedure TSimpleWebServerOptions.SetModified(const AValue: boolean);
begin
  if AValue then
    IncreaseChangeStep
  else
    FLastSavedChangeStep:=FChangeStep;
end;

procedure TSimpleWebServerOptions.SetPort(const AValue: word);
begin
  if FPort=AValue then Exit;
  FPort:=AValue;
  IncreaseChangeStep;
end;

procedure TSimpleWebServerOptions.SetRecentLists(rl: TSWSRecentList;
  const AValue: TStrings);
begin
  if FRecentLists[rl].Equals(AValue) then exit;
  FRecentLists[rl].Assign(AValue);
  IncreaseChangeStep;
end;

procedure TSimpleWebServerOptions.SetServerAddr(const AValue: string);
begin
  if FServerAddr=AValue then Exit;
  FServerAddr:=AValue;
  IncreaseChangeStep;
end;

procedure TSimpleWebServerOptions.SetServerExe(const AValue: string);
begin
  if FServerExe=AValue then Exit;
  FServerExe:=AValue;
  IncreaseChangeStep;
end;

procedure TSimpleWebServerOptions.Assign(Source: TPersistent);
var
  Src: TSimpleWebServerOptions;
  lr: TSWSRecentList;
begin
  if Source is TSimpleWebServerOptions then
  begin
    Src:=TSimpleWebServerOptions(Source);
    FBindAny:=Src.BindAny;
    FServerAddr:=Src.ServerAddr;
    FServerExe:=Src.ServerExe;
    FPort:=Src.Port;
    for lr in TSWSRecentList do
      FRecentLists[lr].Assign(Src.FRecentLists[lr]);
  end else
    inherited Assign(Source);
end;

constructor TSimpleWebServerOptions.Create;
var
  rl: TSWSRecentList;
begin
  inherited Create;
  FChangeStep:=CTInvalidChangeStamp;
  for rl in TSWSRecentList do
    FRecentLists[rl]:=TStringList.Create;
  fApplyHandlers:=TMethodList.Create;
  Clear;
end;

destructor TSimpleWebServerOptions.Destroy;
var
  rl: TSWSRecentList;
begin
  FreeAndNil(fApplyHandlers);
  for rl in TSWSRecentList do
    FreeAndNil(FRecentLists[rl]);
  inherited Destroy;
end;

function TSimpleWebServerOptions.Equals(Obj: TObject): boolean;
var
  Src: TSimpleWebServerOptions;
  lr: TSWSRecentList;
begin
  Result:=false;
  if not (Obj is TSimpleWebServerOptions) then exit;
  Src:=TSimpleWebServerOptions(Obj);
  if (FBindAny<>Src.BindAny)
      or (FServerAddr<>Src.ServerAddr)
      or (FServerExe<>Src.ServerExe)
      or (FPort<>Src.Port) then exit;
  for lr in TSWSRecentList do
    if not FRecentLists[lr].Equals(Src.FRecentLists[lr]) then exit;
  Result:=true;
end;

procedure TSimpleWebServerOptions.SaveSafe;
begin
  try
    SaveToFile(SWSOptionsFile);
  except
    on E: Exception do
      debugln(['TSimpleWebServerOptions.SaveSafe ',E.Message]);
  end;
  Modified:=false;
end;

procedure TSimpleWebServerOptions.LoadSafe;
begin
  try
    LoadFromFile(SWSOptionsFile);
  except
    on E: Exception do
      debugln(['TSimpleWebServerOptions.LoadSafe ',E.Message]);
  end;
  Modified:=false;
end;

procedure TSimpleWebServerOptions.SaveToFile(Filename: string);
var
  Cfg: TConfigStorage;
begin
  Cfg:=GetIDEConfigStorage(Filename,false);
  try
    Cfg.SetDeleteValue('Server/BindAny',BindAny,false);

    Cfg.SetDeleteValue('Server/Addr/Value',ServerAddr,SWSDefaultServerAddr);
    Cfg.SetValue('Server/Addr/Recent',FRecentLists[swsrlServerAddr]);

    Cfg.SetDeleteValue('Server/Exe/Value',ServerExe,GetDefaultServerExe);
    Cfg.SetValue('Server/Exe/Recent',FRecentLists[swsrlServerExe]);

    Cfg.SetDeleteValue('Server/Port',Port,SWSDefaultServerPort);
    Cfg.SetValue('Server/Port/Recent',FRecentLists[swsrlServerPort]);

    Cfg.SetValue('User/Recent/Location',FRecentLists[swsrlUserLocation]);
    Cfg.SetValue('User/Recent/Path',FRecentLists[swsrlUserPath]);
    Cfg.SetValue('User/Recent/Params',FRecentLists[swsrlUserParams]);
  finally
    Cfg.Free;
  end;
end;

procedure TSimpleWebServerOptions.LoadFromFile(Filename: string);
var
  Cfg: TConfigStorage;
  i: integer;
begin
  Clear;
  Cfg:=GetIDEConfigStorage(Filename,true);
  try
    BindAny:=Cfg.GetValue('Server/BindAny',false);

    ServerAddr:=Cfg.GetValue('Server/Addr/Value',SWSDefaultServerAddr);
    Cfg.GetValue('Server/Addr/Recent',FRecentLists[swsrlServerAddr]);

    ServerExe:=Cfg.GetValue('Server/Exe/Value',GetDefaultServerExe);
    Cfg.GetValue('Server/Exe/Recent',FRecentLists[swsrlServerExe]);

    i:=Cfg.GetValue('Server/Port/Value',integer(SWSDefaultServerPort));
    if (i<1) or (i>65535) then i:=SWSDefaultServerPort;
    Port:=i;
    Cfg.GetValue('Server/Port/Recent',FRecentLists[swsrlServerPort]);

    Cfg.GetValue('User/Recent/Location',FRecentLists[swsrlUserLocation]);
    Cfg.GetValue('User/Recent/Path',FRecentLists[swsrlUserPath]);
    Cfg.GetValue('User/Recent/Params',FRecentLists[swsrlUserParams]);
  finally
    Cfg.Free;
  end;
end;

procedure TSimpleWebServerOptions.Clear;
begin
  BindAny:=false;
  ServerAddr:=SWSDefaultServerAddr;
  ServerExe:=GetDefaultServerExe;
  Port:=SWSDefaultServerPort;
end;

function TSimpleWebServerOptions.GetDefaultServerExe: string;
begin
  Result:='compileserver'+GetExeExt;
end;

procedure TSimpleWebServerOptions.IncreaseChangeStep;
begin
  CTIncreaseChangeStamp(FChangeStep);
end;

procedure TSimpleWebServerOptions.Apply;
begin
  fApplyHandlers.CallNotifyEvents(Self);
end;

procedure TSimpleWebServerOptions.AddHandlerApply(
  const OnApplyEvent: TNotifyEvent; AsLast: boolean);
begin
  fApplyHandlers.Add(TMethod(OnApplyEvent),AsLast);
end;

procedure TSimpleWebServerOptions.RemoveHandlerApply(
  const OnApplyEvent: TNotifyEvent);
begin
  fApplyHandlers.Remove(TMethod(OnApplyEvent));
end;

procedure TSimpleWebServerOptions.AddRecent(rl: TSWSRecentList; Value: string);
var
  sl: TStringList;
  i: Integer;
begin
  sl:=FRecentLists[rl];
  i:=sl.IndexOf(Value);
  if i=0 then
    exit
  else if i>=0 then
    sl.Move(i,0)
  else begin
    sl.Insert(0,Value);
    while sl.Count>SWSRecentListCapacity do
      sl.Delete(sl.Count-1);
  end;
  IncreaseChangeStep;
end;

end.

