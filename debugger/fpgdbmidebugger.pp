unit FpGdbmiDebugger;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, FpDbgClasses, GDBMIDebugger, BaseDebugManager, Debugger, GDBMIMiscClasses,
  GDBTypeInfo, maps, FpDbgLoader, FpDbgDwarf, FpDbgDwarfConst, LazLoggerBase,
  LazLoggerProfiling;

type

  TFpGDBMIDebugger = class;

  { TFpGDBPTypeRequestCache }

  TFpGDBPTypeRequestCache = class(TGDBPTypeRequestCache)
  private
    FDebugger: TFpGDBMIDebugger;
    FInIndexOf: Boolean;
  public
    constructor Create(ADebugger: TFpGDBMIDebugger);
    function IndexOf(AThreadId, AStackFrame: Integer; ARequest: TGDBPTypeRequest): Integer; override;
    property Debugger: TFpGDBMIDebugger read FDebugger;
  end;

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
    function CreateTypeRequestCache: TGDBPTypeRequestCache; override;
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
    function  FpDebugger: TFpGDBMIDebugger;
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

{ TFpGDBPTypeRequestCache }

constructor TFpGDBPTypeRequestCache.Create(ADebugger: TFpGDBMIDebugger);
begin
  FDebugger := ADebugger;
  FInIndexOf := False;
  inherited Create;
end;

function TFpGDBPTypeRequestCache.IndexOf(AThreadId, AStackFrame: Integer;
  ARequest: TGDBPTypeRequest): Integer;
var
  IdentName: String;
  Loc: TDBGPtr;
  Ident: TDbgSymbol;
begin
  Result := inherited IndexOf(AThreadId, AStackFrame, ARequest);

  if (Result > 0) or FInIndexOf then
    exit;

  FInIndexOf := True;
  try
  if FDebugger.HasDwarf and (ARequest.ReqType = gcrtPType) then begin
    if copy(ARequest.Request, 1, 6) = 'ptype ' then
	     IdentName := trim(copy(ARequest.Request, 7, length(ARequest.Request)))
    else
    if copy(ARequest.Request, 1, 7) = 'whatis ' then
      IdentName := trim(copy(ARequest.Request, 8, length(ARequest.Request)));

    if IdentName <> '' then begin
      Loc := FDebugger.GetLocationForContext(AThreadId, AStackFrame);
      if (Loc <> 0) then begin
        Ident := FDebugger.FDwarfInfo.FindIdentifier(Loc, IdentName);
        if Ident <> nil then begin
          FDebugger.AddToGDBMICache(AThreadId, AStackFrame, Ident);
          Result := inherited IndexOf(AThreadId, AStackFrame, ARequest);
        end;
       ReleaseRefAndNil(Ident);
      end;
    end;
  end;
  finally
    FInIndexOf := False;
  end;
end;

{ TFPGDBMIWatches }

function TFPGDBMIWatches.FpDebugger: TFpGDBMIDebugger;
begin
  Result := TFpGDBMIDebugger(Debugger);
end;

procedure TFPGDBMIWatches.InternalRequestData(AWatchValue: TCurrentWatchValue);
var
  Loc: TDBGPtr;
  Ident: TDbgSymbol;
begin
  //if FpDebugger.HasDwarf then begin
  //   Loc := FpDebugger.GetLocationForContext(AWatchValue.ThreadId, AWatchValue.StackFrame);
  //
  //   if (Loc <> 0) then begin
  //     Ident := FpDebugger.FDwarfInfo.FindIdentifier(Loc, AWatchValue.Watch.Expression);
  //
  //     if Ident <> nil then
  //       FpDebugger.AddToGDBMICache(AWatchValue.ThreadId, AWatchValue.StackFrame, Ident);
  //
  //     ReleaseRefAndNil(Ident);
  //   end;
  //end;

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
     //GetCurrentContext(CurThread, CurStack);
     //Loc := GetLocationForContext(CurThread, CurStack);
     //
     //if (Loc <> 0) then begin
     //  Ident := FDwarfInfo.FindIdentifier(Loc, String(AParams[0].VAnsiString));
     //
     //  if Ident <> nil then
     //    AddToGDBMICache(CurThread, CurStack, Ident);
     //
     //  ReleaseRefAndNil(Ident);
     //end;

//    //EvalFlags := [];
//    //if high(AParams) >= 3 then
//    //  EvalFlags := TDBGEvaluateFlags(AParams[3].VInteger);
//    //Result := GDBEvaluate(String(AParams[0].VAnsiString),
//    //  String(AParams[1].VPointer^), TGDBType(AParams[2].VPointer^),
//    //  EvalFlags);
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

  procedure MaybeAdd(AType: TGDBCommandRequestType; AQuery, AAnswer: String);
  var
    AReq: TGDBPTypeRequest;
  begin
    AReq.ReqType := AType;
    AReq.Request := AQuery;
    if TypeRequestCache.IndexOf(AThreadId, AStackFrame, AReq) < 0 then begin
      AReq.Result := ParseTypeFromGdb(AAnswer);
      TypeRequestCache.Add(AThreadId, AStackFrame, AReq);
      debugln(['TFpGDBMIDebugger.AddToGDBMICache ', AReq.Request, ' T:', AThreadId, ' S:',AStackFrame, ' >> ', AAnswer]);
    end;
  end;

var
  TypeIdent: TDbgDwarfTypeIdentifier;
  VarName, TypeName: String;
  AReq: TGDBPTypeRequest;
  IsPointer: Boolean;
begin
  (* Simulate gdb answers *)
  //TypeRequestCache

  if AnIdent is TDbgDwarfValueIdentifier then begin
    VarName := TDbgDwarfValueIdentifier(AnIdent).IdentifierName;
    TypeIdent := TDbgDwarfValueIdentifier(AnIdent).TypeInfo;
    if TypeIdent = nil then exit;
    TypeName := TypeIdent.IdentifierName;
    IsPointer := TypeIdent.IsPointerType;
    while (TypeIdent <> nil) and TypeIdent.IsPointerType do
      TypeIdent := TypeIdent.PointedToType;
    if TypeIdent = nil then exit;


    if TGDBMIDwarfTypeIdentifier(TypeIdent).IsBaseType then begin
      if IsPointer then begin
        MaybeAdd(gcrtPType, GdbCmdPType + VarName, Format('type = ^%s', [TypeName]));
        MaybeAdd(gcrtPType, GdbCmdWhatIs + VarName, Format('type = ^%s', [TypeName]));
        MaybeAdd(gcrtPType, GdbCmdPType + GDBMIMaybeApplyBracketsToExpr(VarName) + '^',
                 Format('type = %s', [TypeName]));
        MaybeAdd(gcrtPType, GdbCmdWhatIs + GDBMIMaybeApplyBracketsToExpr(VarName) + '^',
                 Format('type = %s', [TypeName]));
      end
      else begin
        MaybeAdd(gcrtPType, GdbCmdPType + VarName, Format('type = %s', [TypeName]));
        MaybeAdd(gcrtPType, GdbCmdWhatIs + VarName, Format('type = %s', [TypeName]));
      end;
    end;


  end;
  (*
    ptype i
    ~"type = LONGINT\n"
    whatis i
    ~"type = LONGINT\n"


    ptype @i
    ~"type = ^LONGINT\n"
    ptype (@i)^
    ~"type = LONGINT\n"
    whatis @i
    ~"type = ^LONGINT\n"
  *)

end;

function TFpGDBMIDebugger.CreateTypeRequestCache: TGDBPTypeRequestCache;
begin
  Result := TFpGDBPTypeRequestCache.Create(Self);
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

