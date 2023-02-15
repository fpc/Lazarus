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
  SWSDefaultBrowserCmd = 'xdg-open "$(URL)"';
  SWSDefaultServerAddr = '127.0.0.1';
  SWSDefaultServerPort = 7777;
  SWSDefaultAPIPath = '_lazarus_locations';
  SWSOptionsFile = 'simplewebservergui.xml';
  SWSLogMaxLines = 10000;
  SWSCompileServerIni = 'simplewebservergui.ini';
  SWSMainServerPath = 'simplewebserver';
  SWSRecentListCapacity = 30;
  SWSTestprojectOrigin = 'TestProject';

const
  SWSKeyBindAny = 'Server/BindAny';
  SWSKeyBrowserKind = 'Browser/Kind';
  SWSKeyBrowserCmd = 'Browser/Cmd/Value';
  SWSKeyRecentBrowserCmd = 'Browser/Cmd/Recent';
  SWSKeyServerAddr = 'Server/Addr/Value';
  SWSKeyRecentServerAddr = 'Server/Addr/Recent';
  SWSKeyServerExe = 'Server/Exe/Value';
  SWSKeyRecentServerExe = 'Server/Exe/Recent';
  SWSKeyServerOptions = 'Server/Params/Value';
  SWSKeyServerPort = 'Server/Port/Value';
  SWSKeyRecentServerPort = 'Server/Port/Recent';
  SWSKeyRecentUserLocation = 'User/Recent/Location';
  SWSKeyRecentUserPath = 'User/Recent/Path';
  SWSKeyRecentUserParams = 'User/Recent/Params';

type
  TSWSRecentList = (
    swsrlServerAddr,
    swsrlServerExe,
    swsrlServerPort,
    swsrlBrowserCmd,
    swsrlUserLocation,
    swsrlUserPath,
    swsrlUserParams
    );
  TSWSRecentLists = set of TSWSRecentList;

  TSWSBrowserKind = (
    swsbkDefault,
    swsbkFirefox,
    swsbkChrome,
    swsbkOpera,
    swsbkVivaldi,
    {$IFDEF Darwin}
    swsbkSafari,
    {$ENDIF}
    {$IFDEF MSWindows}
    swsbkEdge,
    {$ENDIF}
    swsbkCustom
    );
  TSWSBrowserKinds = set of TSWSBrowserKind;

const
  SWSBrowserKindNames: array[TSWSBrowserKind] of string = (
    'Default',
    'Firefox',
    'Chrome',
    'Opera',
    'Vivaldi',
    {$IFDEF Darwin}
    'Safari',
    {$ENDIF}
    {$IFDEF MSWindows}
    'Edge',
    {$ENDIF}
    'Custom'
    );

type
  { TSimpleWebServerOptions }

  TSimpleWebServerOptions = class(TPersistent)
  private
    fApplyHandlers: TMethodList;
    FBindAny: boolean;
    FBrowserKind: TSWSBrowserKind;
    FBrowserCmd: string;
    FChangeStep: integer;
    FServerPort: word;
    FLastSavedChangeStep: integer;
    FRecentLists: array[TSWSRecentList] of TStringList;
    FServerAddr: string;
    FServerExe: string;
    FServerOpts: TStrings;
    function GetModified: boolean;
    function GetRecentLists(rl: TSWSRecentList): TStrings;
    procedure SetBindAny(const AValue: boolean);
    procedure SetBrowserKind(const AValue: TSWSBrowserKind);
    procedure SetBrowserCmd(const AValue: string);
    procedure SetModified(const AValue: boolean);
    procedure SetServerPort(const AValue: word);
    procedure SetRecentLists(rl: TSWSRecentList; const AValue: TStrings);
    procedure SetServerAddr(const AValue: string);
    procedure SetServerExe(const AValue: string);
    procedure SetServerOpts(const AValue: TStrings);
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
    property BrowserKind: TSWSBrowserKind read FBrowserKind write SetBrowserKind;
    property BrowserCmd: string read FBrowserCmd write SetBrowserCmd;
    property ServerExe: string read FServerExe write SetServerExe;
    property ServerAddr: string read FServerAddr write SetServerAddr;
    property ServerOpts: TStrings read FServerOpts write SetServerOpts; // cmd line options, one per line
    property ServerPort: word read FServerPort write SetServerPort;
    property RecentLists[rl: TSWSRecentList]: TStrings read GetRecentLists write SetRecentLists;
  end;

function StrToBrowserKind(const s: string): TSWSBrowserKind;

implementation

function StrToBrowserKind(const s: string): TSWSBrowserKind;
begin
  for Result:=low(TSWSBrowserKind) to high(TSWSBrowserKind) do
    if SameText(s,SWSBrowserKindNames[Result]) then exit;
  Result:=swsbkDefault;
end;

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

procedure TSimpleWebServerOptions.SetBrowserKind(const AValue: TSWSBrowserKind);
begin
  if FBrowserKind=AValue then Exit;
  FBrowserKind:=AValue;
  IncreaseChangeStep;
end;

procedure TSimpleWebServerOptions.SetBrowserCmd(const AValue: string);
begin
  if FBrowserCmd=AValue then Exit;
  FBrowserCmd:=AValue;
  IncreaseChangeStep;
end;

procedure TSimpleWebServerOptions.SetModified(const AValue: boolean);
begin
  if AValue then
    IncreaseChangeStep
  else
    FLastSavedChangeStep:=FChangeStep;
end;

procedure TSimpleWebServerOptions.SetServerPort(const AValue: word);
begin
  if FServerPort=AValue then Exit;
  FServerPort:=AValue;
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

procedure TSimpleWebServerOptions.SetServerOpts(const AValue: TStrings);
begin
  if (FServerOpts=AValue) or FServerOpts.Equals(AValue) then Exit;
  FServerOpts.Assign(AValue);
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
    FBrowserKind:=Src.FBrowserKind;
    FBrowserCmd:=Src.FBrowserCmd;
    FServerAddr:=Src.ServerAddr;
    FServerExe:=Src.ServerExe;
    FServerOpts.Assign(Src.FServerOpts);
    FServerPort:=Src.ServerPort;
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
  FServerOpts:=TStringList.Create;
  Clear;
end;

destructor TSimpleWebServerOptions.Destroy;
var
  rl: TSWSRecentList;
begin
  FreeAndNil(FServerOpts);
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
      or (FBrowserKind<>Src.BrowserKind)
      or (FBrowserCmd<>Src.BrowserCmd)
      or (FServerAddr<>Src.ServerAddr)
      or (FServerExe<>Src.ServerExe)
      or (not FServerOpts.Equals(Src.ServerOpts))
      or (FServerPort<>Src.ServerPort)
      then exit;
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
    Cfg.SetDeleteValue(SWSKeyBindAny,BindAny,false);

    Cfg.SetDeleteValue(SWSKeyBrowserKind,SWSBrowserKindNames[BrowserKind],SWSBrowserKindNames[swsbkDefault]);

    Cfg.SetDeleteValue(SWSKeyBrowserCmd,BrowserCmd,SWSDefaultBrowserCmd);
    Cfg.SetValue(SWSKeyRecentBrowserCmd,FRecentLists[swsrlBrowserCmd]);

    Cfg.SetDeleteValue(SWSKeyServerAddr,ServerAddr,SWSDefaultServerAddr);
    Cfg.SetValue(SWSKeyRecentServerAddr,FRecentLists[swsrlServerAddr]);

    Cfg.SetDeleteValue(SWSKeyServerExe,ServerExe,GetDefaultServerExe);
    Cfg.SetValue(SWSKeyRecentServerExe,FRecentLists[swsrlServerExe]);

    Cfg.SetValue(SWSKeyServerOptions,FServerOpts);

    Cfg.SetDeleteValue(SWSKeyServerPort,ServerPort,SWSDefaultServerPort);
    Cfg.SetValue(SWSKeyRecentServerPort,FRecentLists[swsrlServerPort]);

    Cfg.SetDeleteValue(SWSKeyServerPort,ServerPort,SWSDefaultServerPort);
    Cfg.SetValue(SWSKeyRecentServerPort,FRecentLists[swsrlServerPort]);

    Cfg.SetValue(SWSKeyRecentUserLocation,FRecentLists[swsrlUserLocation]);
    Cfg.SetValue(SWSKeyRecentUserPath,FRecentLists[swsrlUserPath]);
    Cfg.SetValue(SWSKeyRecentUserParams,FRecentLists[swsrlUserParams]);
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
    BindAny:=Cfg.GetValue(SWSKeyBindAny,false);

    BrowserKind:=StrToBrowserKind(Cfg.GetValue(SWSKeyBrowserKind,SWSBrowserKindNames[swsbkDefault]));

    BrowserCmd:=Cfg.GetValue(SWSKeyBrowserCmd,SWSDefaultBrowserCmd);
    Cfg.GetValue(SWSKeyRecentBrowserCmd,FRecentLists[swsrlBrowserCmd]);

    ServerAddr:=Cfg.GetValue(SWSKeyServerAddr,SWSDefaultServerAddr);
    Cfg.GetValue(SWSKeyRecentServerAddr,FRecentLists[swsrlServerAddr]);

    ServerExe:=Cfg.GetValue(SWSKeyServerExe,GetDefaultServerExe);
    Cfg.GetValue(SWSKeyRecentServerExe,FRecentLists[swsrlServerExe]);

    Cfg.GetValue(SWSKeyServerOptions,FServerOpts);

    i:=Cfg.GetValue(SWSKeyServerPort,integer(SWSDefaultServerPort));
    if (i<1) or (i>65535) then i:=SWSDefaultServerPort;
    ServerPort:=i;
    Cfg.GetValue(SWSKeyRecentServerPort,FRecentLists[swsrlServerPort]);

    Cfg.GetValue(SWSKeyRecentUserLocation,FRecentLists[swsrlUserLocation]);
    Cfg.GetValue(SWSKeyRecentUserPath,FRecentLists[swsrlUserPath]);
    Cfg.GetValue(SWSKeyRecentUserParams,FRecentLists[swsrlUserParams]);
  finally
    Cfg.Free;
  end;
end;

procedure TSimpleWebServerOptions.Clear;
begin
  BindAny:=false;
  ServerAddr:=SWSDefaultServerAddr;
  ServerExe:=GetDefaultServerExe;
  ServerOpts.Clear;
  ServerPort:=SWSDefaultServerPort;
end;

function TSimpleWebServerOptions.GetDefaultServerExe: string;
begin
  Result:='simpleserver'+GetExeExt;
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

