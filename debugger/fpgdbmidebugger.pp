unit FpGdbmiDebugger;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, FpDbgClasses, GDBMIDebugger, BaseDebugManager, Debugger, GDBMIMiscClasses,
  GDBTypeInfo, maps, FpDbgLoader, FpDbgDwarf, FpDbgDwarfConst, LazLoggerBase,
  LazLoggerProfiling;

type

  { TFpGDBMIDebugger }

  TFpGDBMIDebugger = class(TGDBMIDebugger)
  private
    FImageLoader: TDbgImageLoader;
    FDwarfInfo: TDbgDwarf;
  protected
    function CreateCommandStartDebugging(AContinueCommand: TGDBMIDebuggerCommand): TGDBMIDebuggerCommandStartDebugging; override;
    function CreateLineInfo: TDBGLineInfo; override;
    function  CreateWatches: TWatchesSupplier; override;
    procedure DoState(const OldState: TDBGState); override;
    function  HasDwarf: Boolean;
    procedure LoadDwarf;
    procedure UnLoadDwarf;
    function  RequestCommand(const ACommand: TDBGCommand; const AParams: array of const): Boolean; override;

    procedure GetCurrentContext(out AThreadId, AStackFrame: Integer);
    function  GetLocationForContext(AThreadId, AStackFrame: Integer): TDBGPtr;
    procedure AddToGDBMICache(AThreadId, AStackFrame: Integer; AnIdent: TDbgSymbol);
  public
    class function Caption: String; override;
  public
    destructor Destroy; override;
  end;


implementation

type

  { TFpGDBMIDebuggerCommandStartDebugging }

  TFpGDBMIDebuggerCommandStartDebugging = class(TGDBMIDebuggerCommandStartDebugging)
  protected
    function DoExecute: Boolean; override;
  end;

  { TFPGDBMIWatches }

  TFPGDBMIWatches = class(TGDBMIWatches)
  private
  protected
    //procedure DoStateChange(const AOldState: TDBGState); override;
    procedure InternalRequestData(AWatchValue: TCurrentWatchValue); override;
  public
    //constructor Create(const ADebugger: TDebugger);
    //destructor Destroy; override;
  end;

  { TFpGDBMILineInfo }

  TFpGDBMILineInfo = class(TDBGLineInfo) //class(TGDBMILineInfo)
  private
    FRequestedSources: TStringList;
  protected
    function  FpDebugger: TFpGDBMIDebugger;
    procedure DoStateChange(const AOldState: TDBGState); override;
    procedure ClearSources;
  public
    constructor Create(const ADebugger: TDebugger);
    destructor Destroy; override;
    function Count: Integer; override;
    function GetAddress(const AIndex: Integer; const ALine: Integer): TDbgPtr; override;
    function GetInfo(AAdress: TDbgPtr; out ASource, ALine, AOffset: Integer): Boolean; override;
    function IndexOf(const ASource: String): integer; override;
    procedure Request(const ASource: String); override;
    procedure Cancel(const ASource: String); override;
  end;

{ TFPGDBMIWatches }

procedure TFPGDBMIWatches.InternalRequestData(AWatchValue: TCurrentWatchValue);
var
  Loc: TDBGPtr;
begin
  Loc := TFpGDBMIDebugger(Debugger).GetLocationForContext(AWatchValue.ThreadId, AWatchValue.StackFrame);


  inherited InternalRequestData(AWatchValue);
end;

{ TFpGDBMILineInfo }

function TFpGDBMILineInfo.FpDebugger: TFpGDBMIDebugger;
begin
  Result := TFpGDBMIDebugger(Debugger);
end;

procedure TFpGDBMILineInfo.DoStateChange(const AOldState: TDBGState);
begin
  //inherited DoStateChange(AOldState);
  if not (Debugger.State in [dsPause, dsInternalPause, dsRun]) then
    ClearSources;
end;

procedure TFpGDBMILineInfo.ClearSources;
begin
  FRequestedSources.Clear;
end;

constructor TFpGDBMILineInfo.Create(const ADebugger: TDebugger);
begin
  FRequestedSources := TStringList.Create;
  inherited Create(ADebugger);
end;

destructor TFpGDBMILineInfo.Destroy;
begin
  FreeAndNil(FRequestedSources);
  inherited Destroy;
end;

function TFpGDBMILineInfo.Count: Integer;
begin
  Result := FRequestedSources.Count;
end;

function TFpGDBMILineInfo.GetAddress(const AIndex: Integer; const ALine: Integer): TDbgPtr;
var
  Map: PDWarfLineMap;
begin
  Result := 0;
  if not FpDebugger.HasDwarf then
    exit;
  //Result := FpDebugger.FDwarfInfo.GetLineAddress(FRequestedSources[AIndex], ALine);
  Map := PDWarfLineMap(FRequestedSources.Objects[AIndex]);
  if Map <> nil then
    Result := Map^.GetAddressForLine(ALine);
end;

function TFpGDBMILineInfo.GetInfo(AAdress: TDbgPtr; out ASource, ALine,
  AOffset: Integer): Boolean;
begin
  Result := False;
  //ASource := '';
  //ALine := 0;
  //if not FpDebugger.HasDwarf then
  //  exit(nil);
  //FpDebugger.FDwarfInfo.
end;

function TFpGDBMILineInfo.IndexOf(const ASource: String): integer;
begin
  Result := FRequestedSources.IndexOf(ASource);
end;

procedure TFpGDBMILineInfo.Request(const ASource: String);
begin
  if not FpDebugger.HasDwarf then
    exit;
  FRequestedSources.AddObject(ASource, TObject(FpDebugger.FDwarfInfo.GetLineAddressMap(ASource)));
  DoChange(ASource);
end;

procedure TFpGDBMILineInfo.Cancel(const ASource: String);
begin
  //
end;


{ TFpGDBMIDebuggerCommandStartDebugging }

function TFpGDBMIDebuggerCommandStartDebugging.DoExecute: Boolean;
begin
  TFpGDBMIDebugger(FTheDebugger).LoadDwarf;
  Result := inherited DoExecute;
end;

{ TFpGDBMIDebugger }

procedure TFpGDBMIDebugger.DoState(const OldState: TDBGState);
begin
  inherited DoState(OldState);
  if State in [dsStop, dsError, dsNone] then
    UnLoadDwarf;
end;

function TFpGDBMIDebugger.HasDwarf: Boolean;
begin
  Result := FDwarfInfo <> nil;
end;

procedure TFpGDBMIDebugger.LoadDwarf;
begin
  UnLoadDwarf;
  debugln(['TFpGDBMIDebugger.LoadDwarf ']);
  FImageLoader := TDbgImageLoader.Create(FileName);
  FDwarfInfo := TDbgDwarf.Create(FImageLoader);
  FDwarfInfo.LoadCompilationUnits;
end;

procedure TFpGDBMIDebugger.UnLoadDwarf;
begin
  debugln(['TFpGDBMIDebugger.UnLoadDwarf ']);
  FreeAndNil(FDwarfInfo);
  FreeAndNil(FImageLoader);
end;

function TFpGDBMIDebugger.RequestCommand(const ACommand: TDBGCommand;
  const AParams: array of const): Boolean;
var
  Ident: TDbgSymbol;
  Loc: TDBGPtr;
  CurThread, CurStack: Integer;
begin
  if HasDwarf and (ACommand = dcEvaluate) then begin
     GetCurrentContext(CurThread, CurStack);
     Loc := GetLocationForContext(-1, -1);

     if (Loc <> 0) then begin
       Ident := FDwarfInfo.FindIdentifier(Loc, String(AParams[0].VAnsiString));

       if Ident <> nil then
         AddToGDBMICache(CurThread, CurStack, Ident);

       ReleaseRefAndNil(Ident);
     end;

    //EvalFlags := [];
    //if high(AParams) >= 3 then
    //  EvalFlags := TDBGEvaluateFlags(AParams[3].VInteger);
    //Result := GDBEvaluate(String(AParams[0].VAnsiString),
    //  String(AParams[1].VPointer^), TGDBType(AParams[2].VPointer^),
    //  EvalFlags);
    Result := inherited RequestCommand(ACommand, AParams);
  end
  else
    Result := inherited RequestCommand(ACommand, AParams);
end;

procedure TFpGDBMIDebugger.GetCurrentContext(out AThreadId, AStackFrame: Integer);
begin
  if (AThreadId <= 0) and CurrentThreadIdValid then begin
    AThreadId := CurrentThreadId;
    AStackFrame := 0;
  end
  else
  if (AThreadId <= 0) and (not CurrentThreadIdValid) then begin
    AThreadId := 1;
    AStackFrame := 0;
  end
  else
  if (AStackFrame < 0) and (CurrentStackFrameValid) then begin
    AStackFrame := CurrentStackFrame;
  end
  else
  if (AStackFrame < 0) and (not CurrentStackFrameValid) then begin
    AStackFrame := 0;
  end;
end;

function TFpGDBMIDebugger.GetLocationForContext(AThreadId, AStackFrame: Integer): TDBGPtr;
var
  t: TThreadEntry;
  s: TCallStack;
  f: TCallStackEntry;
begin
  Result := 0;
  if (AThreadId <= 0) then begin
    GetCurrentContext(AThreadId, AStackFrame);
  end
  else
  if (AStackFrame < 0) then begin
    AStackFrame := 0;
  end;

  t := Threads.CurrentThreads.EntryById[AThreadId];
  if t = nil then begin
    DebugLn(['NO Threads']);
    exit;
  end;
  if AStackFrame = 0 then begin
    Result := t.Address;
    DebugLn(['Returning addr from Threads', dbgs(Result)]);
    exit;
  end;

  s := CallStack.CurrentCallStackList.EntriesForThreads[AThreadId];
  if s = nil then begin
    DebugLn(['NO Stackframe list for thread']);
    exit;
  end;
  f := s.Entries[AStackFrame];
  if f = nil then begin
    DebugLn(['NO Stackframe']);
    exit;
  end;

  Result := f.Address;
  DebugLn(['Returning addr from frame', dbgs(Result)]);

end;

type
  TGDBMIDwarfTypeIdentifier = class(TDbgDwarfTypeIdentifier)
  public
    property InformationEntry;
  end;

procedure TFpGDBMIDebugger.AddToGDBMICache(AThreadId, AStackFrame: Integer;
  AnIdent: TDbgSymbol);
const
  GdbCmdPType = 'ptype ';
  GdbCmdWhatIs = 'whatis ';
var
  TypeIdent: TDbgDwarfTypeIdentifier;
  VarName, TypeName: String;
  AReq: TGDBPTypeRequest;
begin
  (* Simulate gdb answers *)
  //TypeRequestCache

  if AnIdent is TDbgDwarfValueIdentifier then begin
    VarName := TDbgDwarfValueIdentifier(AnIdent).IdentifierName;
    TypeIdent := TDbgDwarfValueIdentifier(AnIdent).TypeInfo;
    if TypeIdent = nil then exit;
    TypeName := TypeIdent.IdentifierName;

    if TGDBMIDwarfTypeIdentifier(TypeIdent).InformationEntry.Abbrev.tag = DW_TAG_typedef
    then
      TypeIdent := TDbgDwarfValueIdentifier(TypeIdent).TypeInfo;

    if TGDBMIDwarfTypeIdentifier(TypeIdent).InformationEntry.Abbrev.tag = DW_TAG_base_type
    then begin
      AReq.ReqType := gcrtPType;
      AReq.Request := GdbCmdPType + VarName;
      if TypeRequestCache.IndexOf(AThreadId, AStackFrame, AReq) < 0 then begin
        AReq.Result := ParseTypeFromGdb(Format('type = %s', [TypeName]));
        TypeRequestCache.Add(AThreadId, AStackFrame, AReq)
      end;

      AReq.ReqType := gcrtPType;
      AReq.Request := GdbCmdWhatIs + VarName;
      if TypeRequestCache.IndexOf(AThreadId, AStackFrame, AReq) < 0 then begin
        AReq.Result := ParseTypeFromGdb(Format('type = %s', [TypeName]));
        TypeRequestCache.Add(AThreadId, AStackFrame, AReq)
      end;
    end;

  end;


  (*

      >> TCmdLineDebugger.SendCmdLn "ptype i"
      << TCmdLineDebugger.ReadLn "&"ptype i\n""
      << TCmdLineDebugger.ReadLn "~"type = LONGINT\n""
      << TCmdLineDebugger.ReadLn "^done"
      << TCmdLineDebugger.ReadLn "(gdb) "
      >> TCmdLineDebugger.SendCmdLn "whatis i"
      << TCmdLineDebugger.ReadLn "&"whatis i\n""
      << TCmdLineDebugger.ReadLn "~"type = LONGINT\n""
      << TCmdLineDebugger.ReadLn "^done"
      << TCmdLineDebugger.ReadLn "(gdb) "
      >> TCmdLineDebugger.SendCmdLn "-data-evaluate-expression i"
      << TCmdLineDebugger.ReadLn "^done,value="0""
      << TCmdLineDebugger.ReadLn "(gdb) "

  *)
end;

function TFpGDBMIDebugger.CreateCommandStartDebugging(AContinueCommand: TGDBMIDebuggerCommand): TGDBMIDebuggerCommandStartDebugging;
begin
  Result := TFpGDBMIDebuggerCommandStartDebugging.Create(Self, AContinueCommand);
end;

function TFpGDBMIDebugger.CreateLineInfo: TDBGLineInfo;
begin
  Result := TFpGDBMILineInfo.Create(Self);
end;

function TFpGDBMIDebugger.CreateWatches: TWatchesSupplier;
begin
  Result := TFPGDBMIWatches.Create(Self);
end;

class function TFpGDBMIDebugger.Caption: String;
begin
  Result := 'GNU remote debugger (with fpdebug)';
end;

destructor TFpGDBMIDebugger.Destroy;
begin
  UnLoadDwarf;
  inherited Destroy;
end;

initialization
  RegisterDebugger(TFpGDBMIDebugger);

end.

