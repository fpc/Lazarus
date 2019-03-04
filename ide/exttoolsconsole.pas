unit ExtToolsConsole;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazLogger,
  // IDEIntf
  IDEExternToolIntf,
  // IDE
  ExtTools;

type

  { TLazExtToolConsoleView }

  TLazExtToolConsoleView = class(TLazExtToolView)
  protected
    fWrittenLineCount: integer;
    procedure ToolExited; override; // (main thread)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InputClosed; override; // (main thread)
    procedure ProcessNewMessages({%H-}AThread: TThread); override; // (worker thread, Tool is in Critical section)
    procedure OnNewOutput(Sender: TObject; {%H-}FirstNewMsgLine: integer); // (main thread)
  end;

  { TLazExtToolConsole }

  TLazExtToolConsole = class(TComponent)
  private
    fViews: TFPList; // list of TLazExtToolConsoleView
    function GetViews(Index: integer): TLazExtToolConsoleView;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function FindUnfinishedView: TLazExtToolConsoleView;
    property Views[Index: integer]: TLazExtToolConsoleView read GetViews;
    function Count: integer; inline;
  end;

  { TExternalToolConsole }

  // ToDo: Replace TLazExtToolConsole with this TExternalToolConsole somehow.
  TExternalToolConsole = class(TExternalTool)
  private
  protected
    procedure CreateView; override;
    procedure QueueAsyncAutoFree; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TExternalToolsConsole }

  TExternalToolsConsole = class(TExternalTools)
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function GetIDEObject({%H-}ToolData: TIDEExternalToolData): TObject; override;
    procedure HandleMesages; override;
  end;

var
  ExtToolConsole: TLazExtToolConsole = nil; // set by lazbuild

implementation

{ TLazExtToolConsoleView }

constructor TLazExtToolConsoleView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TLazExtToolConsoleView.Destroy;
begin
  Assert(Owner is TLazExtToolConsole, 'TLazExtToolConsoleView.Destroy: Owner is not TLazExtToolConsole.');
  TLazExtToolConsole(Owner).fViews.Remove(Self);
  inherited Destroy;
end;

procedure TLazExtToolConsoleView.ToolExited;
begin
  inherited ToolExited;
  if Tool.Terminated then begin
    ToolState:=lmvtsFailed;
    debugln('Error: (lazarus) ',Caption,': terminated');
  end else if (ExitCode<>0) then begin
    ToolState:=lmvtsFailed;
    debugln('Error: (lazarus) ',Caption,': stopped with exit code '+IntToStr(ExitCode));
  end else if (ExitStatus<>0) then begin
    ToolState:=lmvtsFailed;
    debugln('Error: (lazarus) ',Caption,': stopped with exit status '+IntToStr(ExitStatus));
  end else if Tool.ErrorMessage<>'' then begin
    ToolState:=lmvtsFailed;
    debugln('Error: (lazarus) ',Caption,': ',Tool.ErrorMessage);
  end else begin
    ToolState:=lmvtsSuccess;
  end;
end;

procedure TLazExtToolConsoleView.ProcessNewMessages(AThread: TThread);
begin

end;

procedure TLazExtToolConsoleView.OnNewOutput(Sender: TObject;
  FirstNewMsgLine: integer);
begin
  while fWrittenLineCount<Tool.WorkerOutput.Count do begin
    debugln(Tool.WorkerOutput[fWrittenLineCount]);
    inc(fWrittenLineCount);
  end;
end;

procedure TLazExtToolConsoleView.InputClosed;
begin
  inherited InputClosed;
  Free;
end;

{ TLazExtToolConsole }

constructor TLazExtToolConsole.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fViews:=TFPList.Create;
  ExtToolConsole:=Self;
end;

destructor TLazExtToolConsole.Destroy;
begin
  Clear;
  FreeAndNil(fViews);
  ExtToolConsole:=nil;
  inherited Destroy;
end;

// inline
function TLazExtToolConsole.Count: integer;
begin
  Result:=fViews.Count;
end;

function TLazExtToolConsole.GetViews(Index: integer): TLazExtToolConsoleView;
begin
  Result:=TLazExtToolConsoleView(fViews[Index]);
end;

procedure TLazExtToolConsole.Clear;
var
  i: Integer;
begin
  while FindUnfinishedView<>nil do begin
    CheckSynchronize;
    Sleep(10);
  end;
  for i:=Count-1 downto 0 do begin
    if i>=Count then continue;
    Views[i].Free;
  end;
  if Count>0 then
    raise Exception.Create('TLazExtToolConsole.Clear: some views failed to free');
end;

function TLazExtToolConsole.FindUnfinishedView: TLazExtToolConsoleView;
var
  i: Integer;
begin
  for i:=0 to fViews.Count-1 do begin
    Result:=Views[i];
    if not Result.HasFinished then exit;
  end;
  Result:=nil;
end;

{ TExternalToolConsole }

constructor TExternalToolConsole.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor TExternalToolConsole.Destroy;
begin
  inherited Destroy;
end;

procedure TExternalToolConsole.CreateView;
// in console mode all output goes unparsed to console
var
  View: TLazExtToolConsoleView;
begin
  if ViewCount>0 then exit;
  ClearParsers;
  //View := ExtToolConsole.CreateView(Self);
  View := TLazExtToolConsoleView.Create(ExtToolConsole);
  View.Caption:=Self.Title;
  AddHandlerOnNewOutput(@View.OnNewOutput);
  ExtToolConsole.fViews.Add(View);     // ToDo: Eliminate ExtToolConsole.
  AddView(View);
end;

procedure TExternalToolConsole.QueueAsyncAutoFree;
begin
  debugln(['WARNING: TExternalTool.SetThread can not call AutoFree from other thread']);
end;

{ TExternalToolsConsole }

constructor TExternalToolsConsole.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FToolClass := TExternalToolConsole;
end;

destructor TExternalToolsConsole.Destroy;
begin
  inherited Destroy;
end;

function TExternalToolsConsole.GetIDEObject(ToolData: TIDEExternalToolData): TObject;
begin
  raise Exception.Create('TExternalToolsConsole.GetIDEObject: Should not happen!');
  Result:=nil;
end;

procedure TExternalToolsConsole.HandleMesages;
begin
  if IsMultiThread then
    CheckSynchronize;
end;

end.

