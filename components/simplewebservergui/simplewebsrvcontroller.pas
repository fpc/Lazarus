{
  Author: Mattias Gaertner

Working:
- Start/Stop compileserver
- server log
- on socketerror find conflicting process (Linux, Windows, macos)
- on socketerror find free port (Linux, Windows, macos)
- option: port
- option: interfaceaddress
- change port
- add/delete location
- enable/disable location
- list of locations: location, enabled, filepath, lpi
- floating/dockable window
- IDE menu item: View / Simple Web Server
- options frame
- apply options: restart server
- stop server on close IDE
- options: check and warn about wrong values
- BindAny
- IDE macros SWSPort,SWSAddress, SWSExe
- listview
- delete button
- custom http servers on different ports
  - check port conflict
  - changing main port: check conflict with custom servers
  - list in window
  - stop all on close
- dialog to add user folder

ToDos:
 - log with time and port
 - ide macro SWSExe param: 'resolved', 'base', 'used' and ''
 - Windows: add GetUDPTable2
 - resourcestrings
 - SSL
}
unit SimpleWebSrvController;

{$mode objfpc}{$H+}

interface

uses
  Math, Classes, SysUtils, process, Pipes, Contnrs, fpjson, fphttpclient,
  Sockets,
  // lazutils
  LazLoggerBase, FileUtil, LazUTF8, LazFileUtils, LazMethodList, LazUtilities,
  LazStringUtils,
  // LCL
  Forms, Dialogs, Controls,
  // IDEIntf
  IDEDialogs, IDEMsgIntf, LazIDEIntf, IDEExternToolIntf, MacroIntf,
  MacroDefIntf, SimpleWebSrvUtils, SimpleWebSrvOptions;

type
  ESimpleWebServerException = class(Exception)
  end;

  TSWebServerState = (
    swssNone,
    swssStarting,
    swssRunning,
    swssStopping
    );
  TSWebServerStates = set of TSWebServerState;

  TSWebServerError = (
    swseNone,
    swseBindingOfSocketFailed
    );
  TSWebServerErrors = set of TSWebServerError;

  { TSWSLocation }

  TSWSLocation = class(TPersistent)
  public
    Location: string; // URL subfolder
    Path: string; // path on disk
    Origin: string; // e.g. a lpi file or 'user'
    Enable: boolean;
    ErrorDesc: string;
  end;

  TSWServerThread = class;
  TSimpleWebServerController = class;

  { TSWSInstance }

  TSWSInstance = class(TPersistent)
  public
    Controller: TSimpleWebServerController;

    Exe: string; // may contain macros and not expanded
    ExeUsed: string; // resolved macros, expanded filename
    Params: TStrings;
    Port: word;

    Path: string; // path on disk, can contain macros
    PathUsed: string; // path on disk, resolved macros, expanded filename
    Origin: string; // e.g. a lpi file or 'user'
    ErrorDesc: string;

    State: TSWebServerState;
    Thread: TSWServerThread;
    ExitCode: integer;

    destructor Destroy; override;
  end;

  TSWServerLogEvent = procedure(Sender: TObject; OutLines: TStrings) of object;
  TSWServerStateChangedEvent = procedure(Sender: TObject; Instance: TSWSInstance) of object;

  { TSWServerThread }

  TSWServerThread = class(TThread)
  private
    FOwner: TSWSInstance;
  protected
    fSleepEvent: PRTLEvent;
    FOutLines: TStrings;
    procedure Execute; override; // this thread
    procedure HandleOutput; // this thread
    procedure SynchronizedHandleOuput; // main thread
    procedure QueuedFinished({%H-}Data: PtrInt); // main thread
  public
    TheProcess: TProcess;
    OnOutput: TSWServerLogEvent; // main thread
    OnFinished: TNotifyEvent; // main thread
    constructor Create(TheOwner: TSWSInstance; aProcess: TProcess);
    destructor Destroy; override;
    procedure ShutDown(Gracefully: boolean); // main thread
    procedure TerminateProcess;
    property Owner: TSWSInstance read FOwner;
  end;

  TSWSGetLocationsLocation = record
    Location: string; // http name
    Path: string; // path on disk
  end;
  TSWSGetLocationsLocationArray = array of TSWSGetLocationsLocation;

  TSWSGetLocationsResponse = class
  public
    Locations: TSWSGetLocationsLocationArray;
  end;

  TSWSCHandler = (
    swschStateChanged,  // TSWServerStateChangedEvent
    swschLocationsChanged, // TNotifyEvent
    swschServerLog // TSWServerLogEvent
    );
  TSWSCHandlers = set of TSWSCHandler;

  { TSimpleWebServerController }

  TSimpleWebServerController = class(TComponent)
  private
    FAPIKey: string;
    fAPIPath: string;
    FDestroying: boolean;
    FMainSrvAddr: string;
    FMainSrvBindAny: boolean;
    fHandlers: array[TSWSCHandler] of TMethodList;
    FInstances: TFPList; // list of TSWSInstance
    FLocations: TObjectList; // list of TSWSLocation
    FMainSrvInstance: TSWSInstance;
    FLogLines: TStrings;
    FOptions: TSimpleWebServerOptions;
    FMainSrvError: TSWebServerError;
    FUtility: TSimpleWebServerUtility;
    FViewCaption: string;
    function GetLocationCount: integer;
    function GetLocations(Index: integer): TSWSLocation;
    function GetMainSrvExe: string;
    function GetMainSrvExeUsed: string;
    function GetMainSrvExitCode: integer;
    function GetMainSrvPort: word;
    function GetMainSrvState: TSWebServerState;
    function GetMainSrvThread: TSWServerThread;
    function GetServerCount: integer;
    function GetServers(Index: integer): TSWSInstance;
    function GetSWSAddress(const s: string; const {%H-}Data: PtrInt;
      var Abort: boolean): string;
    function GetSWSExe(const s: string; const {%H-}Data: PtrInt; var Abort: boolean
      ): string;
    function GetSWSPort(const s: string; const {%H-}Data: PtrInt; var Abort: boolean
      ): string;
    procedure OnApplyOptions(Sender: TObject);
    procedure OnIDEClose(Sender: TObject);
    procedure OnServerFinished(Sender: TObject);
    procedure OnServerOutput(Sender: TObject; OutLines: TStrings);
  protected
    IDEMacroSWSAddress: TTransferMacro;
    IDEMacroSWSExe: TTransferMacro;
    IDEMacroSWSPort: TTransferMacro;
    function ParseServerResponse(Response: TStream): TSWSGetLocationsResponse; virtual;
    procedure GetLocationsFromServer; virtual;
    procedure AddServerLocation(Location, Path: string); virtual;
    procedure RemoveServerLocation(Location: string); virtual;
    procedure AddIDEMessageInfo(DbgPrefix, Msg: string);
    procedure SetServerState(Instance: TSWSInstance; NewState: TSWebServerState); virtual;
    procedure StateChanged(Instance: TSWSInstance); virtual;
    procedure LocationsChanged; virtual;
    procedure ServerLog(OutLines: TStrings); virtual;
    function StartServerInstance(Instance: TSWSInstance; ResolveMacros, Interactive: boolean): boolean; virtual;
    function StopServerInstance(Instance: TSWSInstance; Interactive: boolean): boolean; virtual;
    procedure StopAllServers; virtual;
    function GetMainServerExeHint: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure HookMacros; virtual;
    procedure UnhookMacros; virtual;
  public
    // main server
    function StartMainServer(Interactive: boolean): boolean; virtual;
    function StopMainServer(Interactive: boolean): boolean; virtual;
    function AddLocation(Location, Path, Origin: string; Enable: boolean): TSWSLocation; virtual;
    procedure DeleteLocation(Location: string); virtual;
    procedure EnableLocation(Location: string; Enable: boolean); virtual;
    function IndexOfLocation(Location: string): integer; virtual;
    function FindLocation(Location: string): TSWSLocation; virtual;
    procedure AddHandlerLocationsChanged(const OnLocationsChanged: TNotifyEvent; AsLast: boolean = false);
    procedure RemoveHandlerLocationsChanged(const OnLocationsChanged: TNotifyEvent);
    procedure AddHandlerStateChanged(const OnStateChanged: TSWServerStateChangedEvent; AsLast: boolean = false);
    procedure RemoveHandlerStateChanged(const OnStateChanged: TSWServerStateChangedEvent);
    procedure AddHandlerServerLog(const OnServerLog: TSWServerLogEvent; AsLast: boolean = false);
    procedure RemoveHandlerServerLog(const OnServerLog: TSWServerLogEvent);
    procedure RemoveAllHandlersOfObject(AnObject: TObject);
    // custom servers
    function AddServer(Port: word; Exe: string; Params: TStrings;
      Path, Origin: string; ResolveMacros, Interactive: boolean): TSWSInstance; virtual;
    function FindServer(Port: word): TSWSInstance; virtual;
    function FindFreePort(Interactive: boolean; aStartPort: word = 0): word; virtual;
    function StopServer(Instance: TSWSInstance; Interactive: boolean): boolean; virtual;
    function SubstitutePortMacro(aValue, aPort: string): string;
  public
    property Destroying: boolean read FDestroying;
    property LocationCount: integer read GetLocationCount;
    property Locations[Index: integer]: TSWSLocation read GetLocations;
    property LogLines: TStrings read FLogLines;
    property MainSrvAPIKey: string read FAPIKey;
    property MainSrvAPIPath: string read fAPIPath;
    property MainSrvPort: word read GetMainSrvPort;
    property MainSrvBindAny: boolean read FMainSrvBindAny;
    property MainSrvAddr: string read FMainSrvAddr;
    property MainSrvError: TSWebServerError read FMainSrvError;
    property MainSrvExe: string read GetMainSrvExe;
    property MainSrvExeUsed: string read GetMainSrvExeUsed;
    property MainSrvExitCode: integer read GetMainSrvExitCode;
    property MainSrvThread: TSWServerThread read GetMainSrvThread;
    property MainSrvState: TSWebServerState read GetMainSrvState;
    property MainSrvInstance: TSWSInstance read FMainSrvInstance;
    property Options: TSimpleWebServerOptions read FOptions;
    property ServerCount: integer read GetServerCount;
    property Servers[Index: integer]: TSWSInstance read GetServers;
    property Utility: TSimpleWebServerUtility read FUtility;
    property ViewCaption: string read FViewCaption;
  end;

var
  SimpleWebServerController: TSimpleWebServerController; // created by Register

function CheckCompileServerExeQuality(var ServerExe: string; const BaseDir: string;
  aSubtituteMacros: boolean): string; // on fail returns errormessage

implementation

function CheckCompileServerExeQuality(var ServerExe: string;
  const BaseDir: string; aSubtituteMacros: boolean): string;
var
  OutStr, ExpBaseDir: string;
begin
  if aSubtituteMacros then
  begin
    if not IDEMacros.SubstituteMacros(ServerExe) then
      exit('invalid macro');
  end;
  ServerExe:=Trim(ServerExe);
  if ServerExe='' then
    exit('file not found');
  if ExtractFilePath(ServerExe)='' then
  begin
    ExpBaseDir:=BaseDir;
    if aSubtituteMacros and not IDEMacros.SubstituteMacros(ExpBaseDir) then
      exit('invalid macro in BaseDir');
    if ExpBaseDir='' then
    begin
      ExpBaseDir:='$(LazarusDir)';
      IDEMacros.SubstituteMacros(ExpBaseDir);
    end;
    ServerExe:=FindDefaultExecutablePath(ServerExe,ExpBaseDir);
    if ServerExe='' then
      exit('file not found in PATH');
  end else
    ServerExe:=ExpandFileNameUTF8(ServerExe);

  if not FileExistsUTF8(ServerExe) then
    exit('file not found');
  if not FileIsExecutable(ServerExe) then
    exit('file is not executable');

  if not RunCommand(ServerExe,['--version'],OutStr) then
    exit('compileserver does not support --version, maybe this is an old version?');

  Result:='';
end;

{ TSWSInstance }

destructor TSWSInstance.Destroy;
begin
  FreeAndNil(Params);
  inherited Destroy;
end;

{ TSimpleWebServerController }

function TSimpleWebServerController.GetLocationCount: integer;
begin
  Result:=FLocations.Count;
end;

function TSimpleWebServerController.GetLocations(Index: integer): TSWSLocation;
begin
  Result:=TSWSLocation(FLocations[Index]);
end;

function TSimpleWebServerController.GetMainSrvExe: string;
begin
  Result:=FMainSrvInstance.Exe;
end;

function TSimpleWebServerController.GetMainSrvExeUsed: string;
begin
  Result:=FMainSrvInstance.Exe;
end;

function TSimpleWebServerController.GetMainSrvExitCode: integer;
begin
  Result:=FMainSrvInstance.ExitCode;
end;

function TSimpleWebServerController.GetMainSrvPort: word;
begin
  Result:=FMainSrvInstance.Port;
end;

function TSimpleWebServerController.GetMainSrvState: TSWebServerState;
begin
  Result:=FMainSrvInstance.State;
end;

function TSimpleWebServerController.GetMainSrvThread: TSWServerThread;
begin
  Result:=FMainSrvInstance.Thread;
end;

function TSimpleWebServerController.GetServerCount: integer;
begin
  Result:=FInstances.Count;
end;

function TSimpleWebServerController.GetServers(Index: integer): TSWSInstance;
begin
  Result:=TSWSInstance(FInstances[Index]);
end;

function TSimpleWebServerController.GetSWSAddress(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Abort:=False;
  if (s<>'') and (ConsoleVerbosity>=0) then
    debugln(['Hint: (lazarus) [TSimpleWebServerController.GetSWSAddress] ignoring macro SWSAddress parameter "',s,'"']);
  Result:=MainSrvAddr;
  if Result='' then
    Result:='SWSServerAddress'; // always return something to get nicer error messages
end;

function TSimpleWebServerController.GetSWSExe(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Abort:=False;
  if (s<>'') and (ConsoleVerbosity>=0) then
    debugln(['Hint: (lazarus) [TSimpleWebServerController.GetSWSExe] ignoring macro SWSExe parameter "',s,'"']);
  Result:=MainSrvExe;
  if Result='' then
    Result:='SWSServerExe'; // always return something to get nicer error messages
end;

function TSimpleWebServerController.GetSWSPort(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Abort:=False;
  if (s<>'') and (ConsoleVerbosity>=0) then
    debugln(['Hint: (lazarus) [TSimpleWebServerController.GetSWSPort] ignoring macro SWSPort parameter "',s,'"']);
  Result:=IntToStr(MainSrvPort);
  if Result='' then
    Result:='SWSServerPort'; // always return something to get nicer error messages
end;

procedure TSimpleWebServerController.OnApplyOptions(Sender: TObject);
var
  OldRunning: Boolean;
begin
  OldRunning:=MainSrvState=swssRunning;
  StopMainServer(true);
  FMainSrvBindAny:=Options.BindAny;
  FMainSrvAddr:=Options.ServerAddr;
  FMainSrvInstance.Exe:=Options.ServerExe;
  FMainSrvInstance.Port:=Options.Port;
  if OldRunning then
    StartMainServer(true);
end;

procedure TSimpleWebServerController.OnIDEClose(Sender: TObject);
begin
  StopAllServers;
end;

procedure TSimpleWebServerController.OnServerFinished(Sender: TObject);
var
  aProcDescription: string;
  aPID, i: integer;
  r: TModalResult;
  NewPort: Word;
  IPAddr: in_addr;
  Instance: TSWSInstance;
begin
  //debugln(['TSimpleWebServerController.OnServerFinished START']);
  if Destroying then
    exit;

  Instance:=Sender as TSWSInstance;
  if Instance.State=swssRunning then
    StopServerInstance(Instance,false); // notify handlers

  if FMainSrvInstance=Instance then
  begin
    // main server finished

    // error handling
    if (Instance.State<>swssStopping) and (MainSrvError=swseBindingOfSocketFailed) then
    begin
      if MainSrvBindAny then
        IPAddr.s_addr:=0
      else
        IPAddr:=StrToHostAddr(MainSrvAddr);
      if not FUtility.FindProcessListeningOnPort(IPAddr,MainSrvPort,aProcDescription,aPID) then
      begin
        IDEMessageDialog('Error',
           ViewCaption+':'+sLineBreak
           +'Binding of socket failed: '+MainSrvAddr+':'+IntToStr(MainSrvPort),mtError,[mbOk]);
        exit;
      end;

      r:=IDEQuestionDialog('Error',
         ViewCaption+':'+sLineBreak
         +'Binding of socket failed: '+MainSrvAddr+':'+IntToStr(MainSrvPort)+sLineBreak
         +sLineBreak
         +'The following process already listens:'+sLineBreak
         +'PID: '+IntToStr(aPID)+sLineBreak
         +aProcDescription+sLineBreak
         +sLineBreak
         +'Kill process?'
         ,mtError,[mrYes,'Kill PID '+IntToStr(aPID),mrRetry,'Try another port',mrCancel],'');

      case r of
      mrYes:
        if not FUtility.KillProcess(aPID) then
          exit;
      mrRetry:
        begin
          NewPort:=FindFreePort(true);
          if NewPort=0 then
            NewPort:=GetNextIPPort(MainSrvInstance.Port);
          FMainSrvInstance.Port:=NewPort;
        end;
      else
        exit;
      end;

      // try again
      StartMainServer(true);
    end;
  end else begin
    // custom server finished -> delete
    FInstances.Remove(Instance);
    i:=FInstances.IndexOf(Instance);
    if i>=0 then
      FInstances.Delete(i);
    LocationsChanged;
  end;
end;

procedure TSimpleWebServerController.OnServerOutput(Sender: TObject;
  OutLines: TStrings);
const
  BindingOfSocketFailed = 'Binding of socket failed: ';
var
  i: Integer;
  Line: String;
  aThread: TSWServerThread;
begin
  //debugln(['TSimpleWebServerController.OnServerOutput checking StdOut=',dbgs(FMainSrvThread.OutLines.Count),' ...']);
  aThread:=Sender as TSWServerThread;
  if aThread.Owner=FMainSrvInstance then
  begin
    for i:=0 to OutLines.Count-1 do
    begin
      Line:=OutLines[i];
      //debugln(['Hint: TSimpleWebServerController.OnServerOutput {',Line,'}']);
      if LeftStr(Line,length(BindingOfSocketFailed))=BindingOfSocketFailed then
        FMainSrvError:=swseBindingOfSocketFailed;
    end;
  end;

  ServerLog(OutLines);
end;

function TSimpleWebServerController.ParseServerResponse(Response: TStream
  ): TSWSGetLocationsResponse;

  procedure Err(DbgPrefix, Msg: string);
  begin
    AddIDEMessageInfo(DbgPrefix,'get location failed: '+Msg);
  end;

var
  JSON: TJSONData;
  Obj: TJSONObject;
  Arr: TJSONArray;
  i: Integer;
  Location, Path: String;
  ok: Boolean;
begin
  Result:=TSWSGetLocationsResponse.Create;
  ok:=false;
  try
    JSON:=GetJSON(Response);
    if not (JSON is TJSONObject) then
    begin
      Err('20210913154156','response is not a JSON object');
      exit;
    end;
    Obj:=TJSONObject(JSON);
    JSON:=Obj.Find('data');
    if not (JSON is TJSONArray) then
    begin
      Err('20210913154715','response data is not a JSON array');
      exit;
    end;
    Arr:=TJSONArray(JSON);
    SetLength(Result.Locations,Arr.Count);
    for i:=0 to Arr.Count-1 do
    begin
      JSON:=Arr[i];
      if not (JSON is TJSONObject) then
      begin
        Err('20210913154923','response data[i] is not a JSON object');
        exit;
      end;
      Obj:=TJSONObject(JSON);

      JSON:=Obj.Find('location');
      if not (JSON is TJSONString) then
      begin
        Err('20210913155015','response data[i].location is not a JSON string');
        exit;
      end;
      Location:=String(TJSONString(JSON).AsString);

      JSON:=Obj.Find('path');
      if not (JSON is TJSONString) then
      begin
        Err('20210913155016','response data[i].path is not a JSON string');
        exit;
      end;
      Path:=String(TJSONString(JSON).AsString);

      Result.Locations[i].Location:=Location;
      Result.Locations[i].Path:=Path;

      debugln('TSimpleWebServerController.ParseServerResponse ',dbgs(i),' location="',Location,'" path="',Path,'"');
    end;
    ok:=true;
  finally
    if not ok then
      Result.Free;
  end;
end;

procedure TSimpleWebServerController.GetLocationsFromServer;

  procedure Err(Msg: string);
  begin
    AddIDEMessageInfo('[20210913154042]','get location failed: '+Msg);
  end;

var
  Response: TMemoryStream;
  URL: String;
  SrvLocations: TSWSGetLocationsResponse;
  Client: TFPHTTPClient;
  LocArr: TSWSGetLocationsLocationArray;
  i: Integer;
  Loc: TSWSLocation;
  j: SizeInt;
  HasChanged: Boolean;
begin
  URL:='http://'+MainSrvAddr+':'+IntToStr(MainSrvPort)+'/'+MainSrvAPIPath+'/?APIKey='+MainSrvAPIKey+'&fmt=1';
  Response:=TMemoryStream.Create;
  SrvLocations:=nil;
  Client:=TFPHTTPClient.Create(Nil);
  try
    HasChanged:=false;
    Client.Get(URL,Response);
    Response.Position:=0;
    SrvLocations:=ParseServerResponse(Response);
    LocArr:=SrvLocations.Locations;

    for i:=0 to LocationCount-1 do
    begin
      Loc:=Locations[i];
      j:=0;
      while (j<length(LocArr)) and (LocArr[j].Location<>Loc.Location) do
        inc(j);
      if j=length(LocArr) then
      begin
        if Loc.Enable then
          HasChanged:=true;
        Loc.Enable:=false;
      end else
      begin
        if not Loc.Enable then
          HasChanged:=true;
        Loc.Enable:=true;
        if Loc.Path<>LocArr[j].Path then
          AddIDEMessageInfo('20210915144542','path of location "'+Loc.Location+'" differ. IDE="'+Loc.Path+'" Server="'+LocArr[j].Path+'"');
      end;
    end;
  finally
    SrvLocations.Free;
    Response.Free;
    Client.Free;
  end;

  if HasChanged then
    LocationsChanged;
end;

procedure TSimpleWebServerController.AddServerLocation(Location, Path: string);
var
  SrvAddr, URL: String;
  JSON: TStringStream;
  Response: TMemoryStream;
  Client: TFPHTTPClient;
begin
  if MainSrvState<>swssRunning then exit;

  SrvAddr:='http://'+MainSrvAddr+':'+IntToStr(MainSrvPort)+'/';
  URL:=SrvAddr+MainSrvAPIPath+'/?APIKey='+MainSrvAPIKey+'&fmt=1';
  JSON:=TStringStream.Create('{'
    +'"location" : "'+StringToJSONString(Location)+'",'
    +'"path": "'+StringToJSONString(Path)+'"'
    +'}');
  Response:=TMemoryStream.Create;
  Client:=TFPHTTPClient.Create(Nil);
  try
    Client.RequestBody:=JSON;
    Client.AddHeader('Content-Type','application/json');
    AddIDEMessageInfo('20210913104107','adding "'+SrvAddr+Location+'", path="'+Path+'"');
    try
      Client.HTTPMethod('POST',URL,Response,[201,200]);
    except
      on E: Exception do begin
        AddIDEMessageInfo('20220108152508','Failed: '+E.Message);
      end;
    end;
    Response.Position:=0;
    // maybe: check response
  finally
    Response.Free;
    JSON.Free;
    Client.Free;
  end;
end;

procedure TSimpleWebServerController.RemoveServerLocation(Location: string);
var
  URL: String;
  Response: TMemoryStream;
  Client: TFPHTTPClient;
begin
  if MainSrvState<>swssRunning then exit;

  URL:='http://'+MainSrvAddr+':'+IntToStr(MainSrvPort)+'/'+MainSrvAPIPath+'/'+Location+'?APIKey='+MainSrvAPIKey+'&fmt=1';
  Response:=TMemoryStream.Create;
  Client:=TFPHTTPClient.Create(Nil);
  try
    AddIDEMessageInfo('20210915112101','removing location "'+Location+'"');
    try
      Client.HTTPMethod('DELETE',URL,Response,[204,200]);
    except
      on E: EHTTPClient do begin
        if E.StatusCode=404 then
          // already gone
        else
          AddIDEMessageInfo('20210915113102','removing location "'+Location+'" failed: '+E.Message);
      end;
      on E: Exception do begin
        AddIDEMessageInfo('20210915112928','removing location "'+Location+'" failed: '+E.Message);
      end;
    end;
    Response.Position:=0;
    // maybe: check response
  finally
    Response.Free;
    Client.Free;
  end;
end;

procedure TSimpleWebServerController.AddIDEMessageInfo(DbgPrefix,
  Msg: string);
var
  OutLines: TStringListUTF8Fast;
begin
  debugln(['Hint: [',DbgPrefix,'] ',Msg]);
  if ViewCaption='' then ;

  OutLines:=TStringListUTF8Fast.Create;
  try
    OutLines.Add('IDE: '+Msg);
    ServerLog(OutLines);
  finally
    OutLines.Free;
  end;

  if IDEMessagesWindow<>nil then
    IDEMessagesWindow.AddCustomMessage(mluImportant,Msg,'',0,0,ViewCaption);
end;

procedure TSimpleWebServerController.SetServerState(Instance: TSWSInstance;
  NewState: TSWebServerState);
begin
  if Instance.State=NewState then exit;
  Instance.State:=NewState;
  StateChanged(Instance);
end;

procedure TSimpleWebServerController.StateChanged(Instance: TSWSInstance);
var
  h: TMethodList;
  i: Integer;
begin
  h:=fHandlers[swschStateChanged];
  i:=0;
  while (i<h.Count) and not Destroying do
  begin
    TSWServerStateChangedEvent(h[i])(Self,Instance);
    inc(i);
  end;
end;

procedure TSimpleWebServerController.LocationsChanged;
begin
  fHandlers[swschLocationsChanged].CallNotifyEvents(Self);
end;

procedure TSimpleWebServerController.ServerLog(OutLines: TStrings);
var
  h: TMethodList;
  i: Integer;
  sl: TStringListUTF8Fast;
begin
  FLogLines.AddStrings(OutLines);
  if FLogLines.Count>SWSLogMaxLines then
  begin
    // delete old messages
    sl:=TStringListUTF8Fast.Create;
    try
      for i:=FLogLines.Count-SWSLogMaxLines to FLogLines.Count-1 do
        sl.Add(FLogLines[i]);
      FLogLines.Assign(sl);
    finally
      sl.Free;
    end;
  end;

  h:=fHandlers[swschServerLog];
  i:=0;
  while (i<h.Count) and not Destroying do
  begin
    TSWServerLogEvent(h[i])(Self,OutLines);
    inc(i);
  end;
end;

function TSimpleWebServerController.StartServerInstance(Instance: TSWSInstance;
  ResolveMacros, Interactive: boolean): boolean;
var
  PortStr, LinePostfix, TxtPostfix, ServerExeHelp: string;

  function SubstituteMacro(PropName, Value: string; out ParsedValue: string): Boolean;
  begin
    ParsedValue:=SubstitutePortMacro(Value,PortStr);
    if IDEMacros.SubstituteMacros(ParsedValue) then
      exit(true);
    Instance.ErrorDesc:='Failed substituting macros of property "'+PropName+'". '+LinePostfix;
    AddIDEMessageInfo('20220126161036',Instance.ErrorDesc);
    if Interactive then
      IDEMessageDialog('Macro Error',
        ViewCaption+':'+sLineBreak
        +'Failed substituting macros of property "'+PropName+'".'+sLineBreak
        +TxtPostfix,
        mtError,[mbOk]);
    Result:=false;
  end;

  function ErrDlg(aCaption, aMsg: string; IsExe: boolean): TModalResult;
  var
    s: String;
  begin
    s:=ViewCaption+':'+sLineBreak
      +aMsg+sLineBreak
      +TxtPostfix;
    if IsExe then
      s:=s+ServerExeHelp;
    Result:=IDEMessageDialog(aCaption,s,mtError,[mbOk]);
  end;

var
  ExeUsed, PathUsed, Value, s, LazDir: String;
  Params: TStringList;
  i: Integer;
  TheProcess: TProcess;
  aThread: TSWServerThread;
  ProcOpts: TProcessOptions;
begin
  Result:=false;

  case Instance.State of
    swssNone: ;
    swssStopping: exit(false);
    swssStarting: exit(false);
    swssRunning: exit(true);
  end;
  Instance.ExeUsed:='';
  Instance.ErrorDesc:='';
  Instance.ExitCode:=0;
  Instance.PathUsed:='';

  PortStr:=IntToStr(Instance.Port);
  LinePostfix:='Port '+IntToStr(Instance.Port)+', Origin="'+Instance.Origin+'"';
  TxtPostfix:='Port='+IntToStr(Instance.Port)+sLineBreak
              +'Origin="'+Instance.Origin+'"'+sLineBreak;
  if Instance=FMainSrvInstance then
    ServerExeHelp:=GetMainServerExeHint
  else
    ServerExeHelp:='';

  if ConsoleVerbosity>1 then
    debugln(['Hint: TSimpleWebServerController.StartServerInstance START']);

  Params:=TStringList.Create;
  try
    if ResolveMacros then
    begin
      // resolve macros
      if not SubstituteMacro('Executable',Instance.Exe,ExeUsed) then exit;
      if not SubstituteMacro('Local Directory',Instance.Path,PathUsed) then exit;
      if Instance.Params<>nil then
      begin
        for i:=0 to Instance.Params.Count-1 do
        begin
          if not SubstituteMacro('Params['+IntToStr(i)+']',Instance.Params[i],Value) then exit;
          Params.Add(Value);
        end;
      end;
    end else begin
      ExeUsed:=Instance.Exe;
      PathUsed:=Instance.Path;
      Params.AddStrings(Instance.Params);
    end;

    // check for empty values
    if ExeUsed='' then
    begin
      Instance.ErrorDesc:='missing server exe. '+LinePostfix;
      AddIDEMessageInfo('20220127115641',Instance.ErrorDesc);
      if Interactive then
        ErrDlg('Missing Server Exe','Missing server executable',true);
      exit;
    end;
    if PathUsed='' then
    begin
      Instance.ErrorDesc:='missing local directory. '+LinePostfix;
      AddIDEMessageInfo('20220127115738',Instance.ErrorDesc);
      if Interactive then
        ErrDlg('Missing Local Directory','Missing local directory',false);
      exit;
    end;

    // expand local dir
    PathUsed:=ChompPathDelim(ExpandFileNameUTF8(PathUsed));
    if not DirectoryExistsUTF8(PathUsed) then
    begin
      // Note: main server does not need a main dir, the locations have their own dirs
      Instance.ErrorDesc:='server directory not found "'+PathUsed+'". '+LinePostfix;
      AddIDEMessageInfo('20220127122933',Instance.ErrorDesc);
      if Interactive then
        ErrDlg('Missing Server Directory','Server directory "'+PathUsed+'" not found.',false);
      exit;
    end;

    // check exe
    if ExtractFilePath(ExeUsed)='' then
    begin
      LazDir:='$(LazarusDir)';
      IDEMacros.SubstituteMacros(LazDir);
      s:=FindDefaultExecutablePath(ExeUsed,LazDir);
      if s='' then
      begin
        Instance.ErrorDesc:='server exe "'+ExeUsed+'" not found in PATH. '+LinePostfix;
        AddIDEMessageInfo('20220127115917',Instance.ErrorDesc);
        if Interactive then
          ErrDlg('Missing Server Exe','Server executable "'+ExeUsed+'" not found in PATH.',true);
        exit;
      end;
      ExeUsed:=s;
    end else
      ExeUsed:=ExpandFileNameUTF8(ExeUsed);

    debugln(['Hint: [simplewebserver_startserver] ',ViewCaption,' Exe="',ExeUsed,'" LocalDir="',PathUsed,'"',
      ' Params=[',MergeCmdLineParams(Params),'] ',LinePostfix]);
    if not FileExistsUTF8(ExeUsed) then
      begin
      AddIDEMessageInfo('20220127114637','Error: server exe not found "'+ExeUsed+'"');
      if Interactive then
        ErrDlg('Error','File not found: "'+ExeUsed+'"',true);
      exit;
    end;
    if not FileIsExecutable(ExeUsed) then
    begin
      AddIDEMessageInfo('20220127121636','Error: server exe not executable "'+ExeUsed+'"');
      if Interactive then
        ErrDlg('Error','Server exe is not executable: "'+ExeUsed+'"',true);
      exit;
    end;

    // let's get to work
    Instance.ExeUsed:=ExeUsed;
    Instance.PathUsed:=PathUsed;
    SetServerState(Instance,swssStarting);

    // start process
    AddIDEMessageInfo('20210909125756','run: '+MaybeQuote(MainSrvExeUsed)+' '+MergeCmdLineParams(Params));

    TheProcess := TProcess.Create(nil);
    try
      TheProcess.Executable:=ExeUsed;
      TheProcess.Parameters.Assign(Params);
      ProcOpts:=[poUsePipes];
      TheProcess.Options:= ProcOpts;
      TheProcess.ShowWindow := swoHide;   // needed by Windows, ignored by Unix
      TheProcess.StartupOptions:=[suoUseShowWindow]; // needed by Windows, ignored by Unix
      TheProcess.CurrentDirectory:=PathUsed;

      TheProcess.Execute;
    except
      on E: Exception do begin
        AddIDEMessageInfo('20210909125752',
          'unable to run '+MaybeQuote(MainSrvExeUsed)+' '+MergeCmdLineParams(Params)+': '+E.Message);
        TheProcess.Free;
        exit;
      end;
    end;

    Result:=true;
  finally
    Params.Free;
    if not Result then
    begin
      SetServerState(Instance,swssNone);
      if ConsoleVerbosity>1 then
        debugln(['Hint: TSimpleWebServerController.StartServerInstance END']);
    end;
  end;

  try
    // start thread
    aThread:=TSWServerThread.Create(Instance,TheProcess);
    Instance.Thread:=aThread;
    aThread.OnOutput:=@OnServerOutput;
    aThread.OnFinished:=@OnServerFinished;
    aThread.FreeOnTerminate:=false;
    aThread.Start;

    SetServerState(Instance,swssRunning);
  finally
    if ConsoleVerbosity>1 then
      debugln(['Hint: TSimpleWebServerController.StartServerInstance END']);
  end;
end;

function TSimpleWebServerController.StopServerInstance(Instance: TSWSInstance;
  Interactive: boolean): boolean;
var
  aThread: TSWServerThread;
begin
  Result:=false;

  if Interactive then ;

  case Instance.State of
    swssNone: exit(true);
    swssStarting: exit(false);
    swssRunning: ;
    swssStopping: exit(false);
  end;
  if ConsoleVerbosity>1 then
    debugln(['Hint: TSimpleWebServerController.StopServer START Port=',Instance.Port,' Origin=',Instance.Origin,' ',MaybeQuote(Instance.ExeUsed)]);
  SetServerState(Instance,swssStopping);
  try
    aThread:=Instance.Thread;
    if aThread=nil then
    begin
      Result:=true;
      exit;
    end;
    aThread.ShutDown(true);
    if aThread.TheProcess<>nil then
    begin
      Instance.ExitCode:=aThread.TheProcess.ExitCode;
      if (Instance=FMainSrvInstance) and (Instance.ExitCode=1) then
        FMainSrvError:=swseBindingOfSocketFailed;
    end
    else
      Instance.ExitCode:=0;
    FreeAndNil(Instance.Thread);

    Instance.ExeUsed:='';
    Instance.PathUsed:='';
    Result:=true;
  finally
    SetServerState(Instance,swssNone);
  end;
  if ConsoleVerbosity>1 then
    debugln(['Hint: TSimpleWebServerController.StopServer END']);
end;

procedure TSimpleWebServerController.StopAllServers;

  function CleanUp: boolean;
  var
    i: Integer;
    Instance: TSWSInstance;
    aThread: TSWServerThread;
  begin
    for i:=ServerCount-1 downto 0 do
    begin
      Instance:=Servers[i];
      aThread:=Instance.Thread;
      if (Instance.State<>swssRunning) or ((aThread<>nil) and aThread.Finished) then
      begin
        FInstances.Remove(Instance);
        if Instance=FMainSrvInstance then
          FMainSrvInstance:=nil;
        Instance.Free;
      end;
    end;
    Result:=ServerCount=0;
  end;

var
  i: Integer;
  Instance: TSWSInstance;
  aThread: TSWServerThread;
begin
  // terminate all processes and threads
  for i:=0 to ServerCount-1 do
  begin
    Instance:=Servers[i];
    aThread:=Instance.Thread;
    Instance.State:=swssStopping;
    if (Instance.State=swssRunning) and (aThread<>nil) then
    begin
      aThread.TerminateProcess;
      aThread.Terminate;
    end;
  end;

  if CleanUp then exit;

  // wait for all threads (max 2 seconds)
  i:=0;
  repeat
    sleep(20);
    CheckSynchronize(1);
    inc(i);
  until CleanUp or (i=100);

  // free all instances
  FMainSrvInstance:=nil;
  for i:=ServerCount-1 downto 0 do
  begin
    Instance:=Servers[i];
    Instance.Free;
  end;
  FInstances.Clear;
end;

function TSimpleWebServerController.GetMainServerExeHint: string;
begin
  Result:='see Tools / Options / Environment / Simple Web Server / Compileserver';
end;

constructor TSimpleWebServerController.Create(AOwner: TComponent);
var
  h: TSWSCHandler;
begin
  inherited Create(AOwner);
  FUtility:=TSimpleWebServerUtility.Create;
  FViewCaption:='Simple Web Server';
  FUtility.ViewCaption:=FViewCaption;

  FMainSrvBindAny:=false;
  FMainSrvAddr:=SWSDefaultServerAddr;
  fAPIPath:=SWSDefaultAPIPath;
  FLocations:=TObjectList.Create;
  FLogLines:=TStringListUTF8Fast.Create;
  FInstances:=TFPList.Create;
  FMainSrvInstance:=TSWSInstance.Create;
  FMainSrvInstance.Controller:=Self;
  FMainSrvInstance.Port:=SWSDefaultServerPort;
  FMainSrvInstance.Exe:='compileserver'+GetExeExt;

  for h in TSWSCHandler do
    fHandlers[h]:=TMethodList.Create;

  FOptions:=TSimpleWebServerOptions.Create;
  FOptions.AddHandlerApply(@OnApplyOptions);

  LazarusIDE.AddHandlerOnIDEClose(@OnIDEClose);
end;

destructor TSimpleWebServerController.Destroy;
var
  h: TSWSCHandler;
begin
  FDestroying:=true;
  StopAllServers;

  UnhookMacros;
  FreeAndNil(FInstances);
  FreeAndNil(FOptions);
  for h in TSWSCHandler do
    FreeAndNil(fHandlers[h]);
  FreeAndNil(FLogLines);
  FreeAndNil(FLocations);
  FreeAndNil(FUtility);
  inherited Destroy;
end;

function TSimpleWebServerController.StartMainServer(Interactive: boolean
  ): boolean;

  function ErrDlg(aCaption, aMsg: string; IsExe: boolean): TModalResult;
  var
    s: String;
  begin
    s:=ViewCaption+':'+sLineBreak
      +aMsg+sLineBreak;
    if IsExe then
      s:=s+GetMainServerExeHint;
    Result:=IDEMessageDialog(aCaption,s,mtError,[mbOk]);
  end;

var
  LazCfgDir, PathUsed, IniFilename, ErrMsg, ExeUsed: String;
  IniLines: TStringListUTF8Fast;
  MsgResult: TModalResult;
  SecretGUID: TGUID;
  i: Integer;
  Loc: TSWSLocation;
begin
  Result:=false;
  case MainSrvState of
    swssNone: ;
    swssStopping: exit(false);
    swssStarting: exit(false);
    swssRunning: exit(true);
  end;

  if ConsoleVerbosity>1 then
    debugln(['Hint: TSimpleWebServerController.StartMainServer START']);

  LazCfgDir:=AppendPathDelim(LazarusIDE.GetPrimaryConfigPath);
  PathUsed:=LazCfgDir+SWSMainServerPath;
  IniFilename:=LazCfgDir+SWSCompileServerIni;

  CreateGUID(SecretGUID);
  FAPIKey:='';
  for i:=0 to 15 do
    FAPIKey:=FAPIKey+HexStr(PByte(@SecretGUID)[i],2);

  FMainSrvInstance.Params.Free;
  FMainSrvInstance.Params:=TStringListUTF8Fast.Create;
  FMainSrvInstance.Params.Add('-c');
  FMainSrvInstance.Params.Add(IniFilename);

  FMainSrvInstance.Path:=AppendPathDelim(LazarusIDE.GetPrimaryConfigPath)+'simplewebserverdir';

  ExeUsed:=Options.ServerExe;
  ErrMsg:=CheckCompileServerExeQuality(ExeUsed,'',false);
  FMainSrvInstance.Exe:=ExeUsed;
  if ErrMsg<>'' then
  begin
    if Options.ServerExe<>ExeUsed then
      ErrMsg:=ErrMsg+'. Path="'+ExeUsed+'"';
    debugln(['Error: [TSimpleWebServerController.StartServerInstance] invalid ServerExe="',Options.ServerExe,'". ',ErrMsg]);
    AddIDEMessageInfo('20220118164525',ErrMsg);
    if Interactive then
      ErrDlg('Error','Wrong compileserver exe: '+ErrMsg,true);
    exit;
  end;

  // create workdir
  while not DirectoryExistsUTF8(PathUsed) do
  begin
    if CreateDirUTF8(PathUsed) then
      break;
    MsgResult:=IDEMessageDialog('Error',
      ViewCaption+':'+sLineBreak
      +'Error creating directory'+sLineBreak
      +'"'+PathUsed+'"'+sLineBreak,
      mtError,[mbRetry,mbCancel]);
    case MsgResult of
      mrRetry: ;
    else exit(false);
    end;
  end;

  // write main server ini
  IniLines:=TStringListUTF8Fast.Create;
  try
    IniLines.Add('[Server]');
    IniLines.Add('SimpleServer=1');
    IniLines.Add('NoIndexPage=1');
    if MainSrvBindAny then
      IniLines.Add('Interface=0.0.0.0')
    else
      IniLines.Add('Interface='+MainSrvAddr);
    IniLines.Add('Port='+IntToStr(MainSrvPort));
    IniLines.Add('Directory='+PathUsed);
    IniLines.Add('API='+MainSrvAPIPath+','+MainSrvAPIKey);

    IniLines.Add('[Locations]');
    for i:=0 to LocationCount-1 do
    begin
      Loc:=Locations[i];
      if not Loc.Enable then continue;
      if not DirectoryExists(Loc.Path) then
      begin
        AddIDEMessageInfo('20220118165651','Warn: location "'+Loc.Location+'" directory not found: "'+Loc.Path+'"');
        Loc.ErrorDesc:='Directory not found';
        continue;
      end else
        Loc.ErrorDesc:='';
      IniLines.Add(Loc.Location+'='+Loc.Path);
    end;

    repeat
      Result:=false;
      try
        if ConsoleVerbosity>=0 then
          debugln(['Hint: [simplewebserver_startserver] writing ',IniFilename]);
        IniLines.SaveToFile(IniFilename);
        Result:=true;
      except
        on E: Exception do begin
          FMainSrvInstance.ErrorDesc:='Error writing server ini "'+IniFilename+'".';
          AddIDEMessageInfo('20220127123435',FMainSrvInstance.ErrorDesc);
          if Interactive then
          begin
            MsgResult:=IDEMessageDialog('Error',
              ViewCaption+':'+sLineBreak
              +'Error writing "'+IniFilename+'"'+sLineBreak
              +E.Message,
            mtError,[mbRetry,mbCancel]);
            case MsgResult of
              mrRetry: continue;
            else exit(false);
            end;
          end;
        end;
      end;
    until Result;
  finally
    IniLines.Free;
  end;

  Result:=StartServerInstance(FMainSrvInstance,true,Interactive);
end;

function TSimpleWebServerController.StopMainServer(Interactive: boolean
  ): boolean;
begin
  Result:=StopServerInstance(FMainSrvInstance,Interactive);
end;

function TSimpleWebServerController.AddLocation(Location, Path,
  Origin: string; Enable: boolean): TSWSLocation;
var
  i: Integer;
  ExpPath: String;
  DirNotFound: Boolean;
begin
  Result:=nil;
  debugln(['Hint: (TSimpleWebServerController.AddLocation) Location="',Location,'" Path="',Path,'" Origin="',Origin,'" Enable=',Enable]);
  ExpPath:=ExpandFileNameUTF8(Path);

  if Location=SWSDefaultAPIPath then
  begin
    AddIDEMessageInfo('20220119150200','invalid location "'+Location+'"');
    exit;
  end;

  DirNotFound:=not DirectoryExistsUTF8(ExpPath);
  if DirNotFound then
  begin
    AddIDEMessageInfo('20220110182017','Directory not found "'+ExpPath+'"');
    Enable:=false;
  end;

  i:=IndexOfLocation(Location);
  if i<0 then
  begin
    Result:=TSWSLocation.Create;
    Result.Location:=Location;
    Result.Path:=ExpPath;
    Result.Origin:=Origin;
    Result.Enable:=Enable;
    if DirNotFound then
      Result.ErrorDesc:='Directory not found';
    FLocations.Add(Result);
  end else
  begin
    // already exists
    Result:=Locations[i];
    if DirNotFound then
      Result.ErrorDesc:='Directory not found';
    if (Result.Enable=Enable) and (Result.Path=ExpPath) and (Result.Origin=Origin) then
      exit;
    if Result.Enable then
    begin
      if Result.Path=ExpPath then
      begin
        Result.Origin:=Origin;
        LocationsChanged;
        if not Enable then
          RemoveServerLocation(Location);
        exit;
      end else
      begin
        // active location, ExpPath change
      end;
    end else
    begin
      // location was not active
    end;
    Result.Enable:=Enable;
    Result.Path:=ExpPath;
    Result.Origin:=Origin;
  end;
  LocationsChanged;

  if Enable then
    AddServerLocation(Location,ExpPath);
end;

procedure TSimpleWebServerController.DeleteLocation(Location: string);
var
  i: Integer;
  Loc: TSWSLocation;
  WasEnabled: Boolean;
begin
  if Destroying then exit;

  i:=IndexOfLocation(Location);
  if i<0 then
  begin
    debugln(['Warn: (TSimpleWebServerController.DeleteLocation) Location not found "',Location,'" ']);
    exit;
  end;
  Loc:=Locations[i];
  debugln(['Hint: (TSimpleWebServerController.DeleteLocation) Location="',Loc.Location,'" Path="',Loc.Path,'" Origin="',Loc.Origin,'" Enabled=',Loc.Enable]);
  WasEnabled:=Loc.Enable;
  FLocations.Delete(i);
  LocationsChanged;

  if WasEnabled then
    RemoveServerLocation(Location);
end;

procedure TSimpleWebServerController.EnableLocation(Location: string;
  Enable: boolean);
var
  i: Integer;
  Loc: TSWSLocation;
begin
  if Destroying then exit;

  i:=IndexOfLocation(Location);
  if i<0 then
  begin
    debugln(['Warn: (TSimpleWebServerController.EnableLocation) Location not found "',Location,'" ']);
    exit;
  end;
  Loc:=Locations[i];
  if Loc.Enable=Enable then exit;

  debugln(['Hint: (TSimpleWebServerController.EnableLocation) Location="',Loc.Location,'" Path="',Loc.Path,'" Origin="',Loc.Origin,'" Enable=',Enable]);
  Loc.Enable:=Enable;
  LocationsChanged;
  if Enable then
  begin
    AddServerLocation(Loc.Location,Loc.Path);
  end else begin
    RemoveServerLocation(Loc.Location);
  end;
end;

function TSimpleWebServerController.IndexOfLocation(Location: string): integer;
begin
  Result:=0;
  repeat
    if Result=LocationCount then
      exit(-1);
    if Locations[Result].Location=Location then
      exit;
    inc(Result);
  until false;
end;

function TSimpleWebServerController.FindLocation(Location: string
  ): TSWSLocation;
var
  i: Integer;
begin
  i:=IndexOfLocation(Location);
  if i<0 then
    Result:=nil
  else
    Result:=Locations[i];
end;

procedure TSimpleWebServerController.AddHandlerLocationsChanged(
  const OnLocationsChanged: TNotifyEvent; AsLast: boolean);
begin
  fHandlers[swschLocationsChanged].Add(TMethod(OnLocationsChanged),AsLast);
end;

procedure TSimpleWebServerController.RemoveHandlerLocationsChanged(
  const OnLocationsChanged: TNotifyEvent);
begin
  fHandlers[swschLocationsChanged].Remove(TMethod(OnLocationsChanged));
end;

procedure TSimpleWebServerController.AddHandlerStateChanged(
  const OnStateChanged: TSWServerStateChangedEvent; AsLast: boolean);
begin
  fHandlers[swschStateChanged].Add(TMethod(OnStateChanged),AsLast);
end;

procedure TSimpleWebServerController.RemoveHandlerStateChanged(
  const OnStateChanged: TSWServerStateChangedEvent);
begin
  fHandlers[swschStateChanged].Remove(TMethod(OnStateChanged));
end;

procedure TSimpleWebServerController.AddHandlerServerLog(
  const OnServerLog: TSWServerLogEvent; AsLast: boolean);
begin
  fHandlers[swschServerLog].Add(TMethod(OnServerLog),AsLast);
end;

procedure TSimpleWebServerController.RemoveHandlerServerLog(
  const OnServerLog: TSWServerLogEvent);
begin
  fHandlers[swschServerLog].Remove(TMethod(OnServerLog));
end;

procedure TSimpleWebServerController.RemoveAllHandlersOfObject(AnObject: TObject);
var
  h: TSWSCHandler;
begin
  for h in TSWSCHandler do
    fHandlers[h].RemoveAllMethodsOfObject(AnObject);
end;

function TSimpleWebServerController.AddServer(Port: word; Exe: string;
  Params: TStrings; Path, Origin: string; ResolveMacros, Interactive: boolean
  ): TSWSInstance;
begin
  Result:=nil;
  try
    if Port=0 then
      Port:=FindFreePort(Interactive);
    if FindServer(Port)<>nil then
      raise ESimpleWebServerException.Create('port '+IntToStr(Port)+' already in use');

    Result:=TSWSInstance.Create;
    Result.Controller:=Self;
    Result.Port:=Port;
    Result.Exe:=Exe;
    Result.Params:=Params;
    Result.Path:=Path;
    Result.Origin:=Origin;
    FInstances.Add(Result);

  finally
    if Result=nil then
      Params.Free;
  end;
  try
    StartServerInstance(Result,ResolveMacros,Interactive);
  finally
    LocationsChanged;
  end;
end;

function TSimpleWebServerController.FindServer(Port: word): TSWSInstance;
var
  i: Integer;
begin
  for i:=0 to ServerCount-1 do
    if Servers[i].Port=Port then
     exit(Servers[i]);
  Result:=nil;
end;

function TSimpleWebServerController.FindFreePort(Interactive: boolean;
  aStartPort: word): word;
begin
  if aStartPort=0 then
    aStartPort:=MainSrvPort;
  Result:=FUtility.FindFreePort(aStartPort,Interactive);
end;

function TSimpleWebServerController.StopServer(Instance: TSWSInstance;
  Interactive: boolean): boolean;
begin
  if Instance=FMainSrvInstance then
    Result:=StopMainServer(Interactive)
  else
    Result:=StopServerInstance(Instance,Interactive);
end;

function TSimpleWebServerController.SubstitutePortMacro(aValue, aPort: string
  ): string;
var
  l, i: SizeInt;
begin
  Result:=aValue;
  //debugln(['TSimpleWebServerController.SubstitutePortMacro Value="',aValue,'" Port=',aPort]);
  l:=length('$(port)');
  for i:=length(Result)-l+1 downto 1 do
  begin
    if (Result[i]='$') and SameText(copy(Result,i,l),'$(port)') then
      LazStringUtils.ReplaceSubstring(Result,i,l,aPort);
  end;
  //debugln(['TSimpleWebServerController.SubstitutePortMacro Result="',Result,'"']);
end;

procedure TSimpleWebServerController.HookMacros;

  function Add(const AName, ADescription: string;
      AMacroFunction: TMacroFunction): TTransferMacro;
  begin
    Result:=TTransferMacro.Create(AName, '', ADescription, AMacroFunction, []);
    IDEMacros.Add(Result);
  end;

begin
  IDEMacroSWSAddress:=Add('SWSAddress', 'Simple Web Server Address', @GetSWSAddress);
  IDEMacroSWSPort:=Add('SWSPort', 'Simple Web Server Port', @GetSWSPort);
  IDEMacroSWSExe:=Add('SWSExe', 'Simple Web Server Executable', @GetSWSExe);
end;

procedure TSimpleWebServerController.UnhookMacros;
begin
  // nothing at the moment
end;

{ TSWServerThread }

procedure TSWServerThread.Execute;
var
  Buf: string;

  function ReadInputPipe(aStream: TInputPipeStream; var LineBuf: string;
    IsStdErr: boolean): boolean;
  // true if some bytes have been read
  var
    Count: DWord;
    StartPos: Integer;
    i: DWord;
  begin
    Result:=false;
    if Terminated or (aStream=nil) then exit;
    Count:=aStream.NumBytesAvailable;
    if Count=0 then exit;
    Count:=aStream.Read(Buf[1],Min(length(Buf),Count));
    if Count=0 then exit;
    Result:=true;
    StartPos:=1;
    i:=1;
    while i<=Count do begin
      if Buf[i] in [#10,#13] then begin
        LineBuf:=LineBuf+copy(Buf,StartPos,i-StartPos);
        if IsStdErr then ;
        FOutLines.Add(LineBuf);
        LineBuf:='';
        if (i<Count) and (Buf[i+1] in [#10,#13]) and (Buf[i]<>Buf[i+1])
        then
          inc(i);
        StartPos:=i+1;
      end;
      inc(i);
    end;
    LineBuf:=LineBuf+copy(Buf,StartPos,Count-StartPos+1);
  end;

var
  OutputLine, StdErrLine: String;
  HasOutput: Boolean;
begin
  OutputLine:='';
  StdErrLine:='';
  Buf:='';
  SetLength(Buf,4096);
  try
    repeat
      if Terminated then exit;
      HasOutput:=ReadInputPipe(TheProcess.Stderr,StdErrLine,true)
              or ReadInputPipe(TheProcess.Output,OutputLine,false);
      if (not HasOutput) then begin
        // no more pending output
        HandleOutput;
        if Terminated then exit;
        if not TheProcess.Running then break;
        // no more pending output and process is still running -> wait a bit
        RTLEventWaitFor(fSleepEvent,500);
      end;
    until false;
    // add rest of output
    if (OutputLine<>'') then
      FOutLines.Add(OutputLine);
    if (StdErrLine<>'') then
      FOutLines.Add(StdErrLine);
    if not Terminated then
      FOutLines.Add('ExitCode='+IntToStr(TheProcess.ExitCode)+' ExitStatus='+IntToStr(TheProcess.ExitStatus));
    HandleOutput;
    //DebugLn(['Hint: [simplewebserver_serverthread] ExitCode=',TheProcess.ExitCode,' ExitStatus=',TheProcess.ExitStatus]);
  finally
    Application.QueueAsyncCall(@QueuedFinished,0);
  end;
end;

procedure TSWServerThread.HandleOutput;
begin
  if Terminated then exit;
  if (FOutLines.Count=0) then exit;
  Synchronize(@SynchronizedHandleOuput);
end;

procedure TSWServerThread.SynchronizedHandleOuput;
begin
  if Assigned(OnOutput) then
    OnOutput(Self,FOutLines);
  FOutLines.Clear;
end;

procedure TSWServerThread.QueuedFinished(Data: PtrInt);
begin
  if Assigned(OnFinished) then
    OnFinished(Owner);
end;

constructor TSWServerThread.Create(TheOwner: TSWSInstance;
  aProcess: TProcess);
begin
  inherited Create(true);
  FOwner:=TheOwner;
  TheProcess:=aProcess;
  FOutLines:=TStringList.Create;
  fSleepEvent:=RTLEventCreate;
end;

destructor TSWServerThread.Destroy;
begin
  ShutDown(false);
  Application.RemoveAsyncCalls(Self);
  FreeAndNil(TheProcess);
  RTLEventDestroy(fSleepEvent);
  FreeAndNil(FOutLines);
  inherited Destroy;
end;

procedure TSWServerThread.ShutDown(Gracefully: boolean);
begin
  if Finished then exit;
  TerminateProcess;
  if not Gracefully then
    Terminate;
  RTLEventSetEvent(fSleepEvent); // wake up thread
  while not Finished do
  begin
    Application.ProcessMessages;
    if Finished then break;
    sleep(20);
  end;
end;

procedure TSWServerThread.TerminateProcess;
begin
  if TheProcess<>nil then
    TheProcess.Terminate(0);
end;

finalization
  FreeAndNil(SimpleWebServerController);

end.

