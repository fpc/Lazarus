unit FpGdbmiDebugger;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, math, FpDbgClasses, GDBMIDebugger, BaseDebugManager, Debugger,
  GDBMIMiscClasses, GDBTypeInfo, maps, LCLProc, FpDbgLoader, FpDbgDwarf, FpDbgDwarfConst,
  LazLoggerBase, LazLoggerProfiling, FpPascalParser;

type

  TFpGDBMIDebugger = class;

  { TFpGDBMIPascalExpression }

  TFpGDBMIPascalExpression = class(TFpPascalExpression)
  private
    FThreadId: Integer;
    FStackFrame: Integer;
    FDebugger: TFpGDBMIDebugger;
  protected
    function GetDbgTyeForIdentifier(AnIdent: String): TDbgSymbol; override;
  public
    constructor Create(ATextExpression: String; ADebugger: TFpGDBMIDebugger; AThreadId, AStackFrame: Integer);
  end;

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
    //procedure InternalRequestData(AWatchValue: TCurrentWatchValue); override;
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

{ TFpGDBMIPascalExpression }

function TFpGDBMIPascalExpression.GetDbgTyeForIdentifier(AnIdent: String): TDbgSymbol;
var
  Loc: TDBGPtr;
begin
  Result := nil;
  if FDebugger.HasDwarf then begin
    if AnIdent <> '' then begin
      Loc := FDebugger.GetLocationForContext(FThreadId, FStackFrame);
      if (Loc <> 0) then
        Result := FDebugger.FDwarfInfo.FindIdentifier(Loc, AnIdent);
    end;
  end;
end;

constructor TFpGDBMIPascalExpression.Create(ATextExpression: String;
  ADebugger: TFpGDBMIDebugger; AThreadId, AStackFrame: Integer);
begin
  FDebugger := ADebugger;
  FThreadId := AStackFrame;
  FStackFrame := AStackFrame;
  inherited Create(ATextExpression);
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
const
  GdbCmdPType = 'ptype ';
  GdbCmdWhatIs = 'whatis ';

  Function MembersAsGdbText(AStructType: TDbgSymbol; WithVisibilty: Boolean; out AText: String): Boolean;
  var
    CurVis: TDbgSymbolMemberVisibility;

    procedure AddVisibility(AVis: TDbgSymbolMemberVisibility);
    begin
      CurVis := AVis;
      if not WithVisibilty then
        exit;
      case AVis of
        svPrivate:   AText := AText + '  private' + LineEnding;
        svProtected: AText := AText + '  protected' + LineEnding;
        svPublic:    AText := AText + '  public' + LineEnding;
      end;
    end;

    procedure AddMember(AMember: TDbgSymbol);
    var
      ti: TDbgSymbol;
      s: String;
    begin
//todo: functions / virtual / array ...
      if AMember.Kind = FpDbgClasses.skProcedure then begin
        AText := AText + '    procedure ' + AMember.Name + ' ();' + LineEnding;
        exit
      end;

      ti := AMember.TypeInfo;
      if ti = nil then begin
        Result := False;
        exit;
      end;
      s := ti.Name;
      if s = '' then begin
        Result := False;
        exit;
      end;

      if AMember.Kind = FpDbgClasses.skFunction then begin
        AText := AText + '    function  ' + AMember.Name + ' () : '+s+';' + LineEnding;
      end
      else
      begin
        AText := AText + '    ' + AMember.Name + ' : ' + s + ';' + LineEnding;
      end;
    end;

  var
    c: Integer;
    i: Integer;
    m: TDbgSymbol;
  begin
    Result := True;
    AText := '';
    c := AStructType.MemberCount;
    if c = 0 then
      exit;
    i := 0;
    m := AStructType.Member[i];
    AddVisibility(m.MemberVisibility);
    while true do begin
      if m.MemberVisibility <> CurVis then
        AddVisibility(m.MemberVisibility);
      AddMember(m);
      inc(i);
      if (i >= c) or (not Result) then break;
      m := AStructType.Member[i];
    end;
  end;

  procedure MaybeAdd(AType: TGDBCommandRequestType; AQuery, AAnswer: String);
  var
    AReq: TGDBPTypeRequest;
  begin
    AReq.ReqType := AType;
    AReq.Request := AQuery;
    if IndexOf(AThreadId, AStackFrame, AReq) < 0 then begin
      AReq.Result := ParseTypeFromGdb(AAnswer);
      Add(AThreadId, AStackFrame, AReq);
      debugln(['TFpGDBMIDebugger.AddToGDBMICache ', AReq.Request, ' T:', AThreadId, ' S:',AStackFrame, ' >> ', AAnswer]);
    end;
  end;

  procedure AddClassType(ASourceExpr: string; AIsPointerType, AisPointerPointer: Boolean;
    ABaseTypeName, ASrcTypeName, ADeRefTypeName: String;
    ASrcType, ABaseType: TDbgSymbol);
  var
    s, ParentName, RefToken: String;
    s2: String;
  begin
    if not AIsPointerType then begin
      ABaseType := ASrcType;
      ABaseTypeName := ASrcTypeName;
      ADeRefTypeName := ASrcTypeName;
    end;
    if (ABaseType = nil) or (ABaseType.TypeInfo = nil) then
      exit;
    ParentName :=  ABaseType.TypeInfo.Name;
    if not MembersAsGdbText(ABaseType, True, s2) then
      exit;

    s := Format('type = ^%s = class : public %s %s%send%s', [ABaseTypeName, ParentName, LineEnding, s2, LineEnding]);
    MaybeAdd(gcrtPType, GdbCmdPType  + ASourceExpr, s);

    s := Format('type = %s%s', [ASrcTypeName, LineEnding]);
    MaybeAdd(gcrtPType, GdbCmdWhatIs  + ASourceExpr, s);


    ASourceExpr := GDBMIMaybeApplyBracketsToExpr(ASourceExpr)+'^';
    if AIsPointerType
    then RefToken := '^'
    else RefToken := '';
    s := Format('type = %s%s = class : public %s %s%send%s', [RefToken, ABaseTypeName, ParentName, LineEnding, s2, LineEnding]);
    MaybeAdd(gcrtPType, GdbCmdPType  + ASourceExpr, s);

    s := Format('type = %s%s', [ADeRefTypeName, LineEnding]);
    MaybeAdd(gcrtPType, GdbCmdWhatIs  + ASourceExpr, s);
  end;

  procedure AddRecordType(ASourceExpr: string; AIsPointerType, AisPointerPointer: Boolean;
    ABaseTypeName, ASrcTypeName, ADeRefTypeName: String;
    ASrcType, ABaseType: TDbgSymbol);
  var
    s, RefToken: String;
    s2: String;
  begin
    if not AIsPointerType then begin
      ABaseType := ASrcType;
      ABaseTypeName := ASrcTypeName;
      ADeRefTypeName := ASrcTypeName;
    end;
    if (ABaseType = nil) then
      exit;
    if not MembersAsGdbText(ABaseType, False, s2) then
      exit;

    if AIsPointerType
    then RefToken := '^'
    else RefToken := '';
    s := Format('type = %s%s = record %s%send%s', [RefToken, ABaseTypeName, LineEnding, s2, LineEnding]);
    MaybeAdd(gcrtPType, GdbCmdPType  + ASourceExpr, s);

    if AIsPointerType then begin
      s := Format('type = %s%s', [ASrcTypeName, LineEnding]);
      MaybeAdd(gcrtPType, GdbCmdWhatIs  + ASourceExpr, s);

      //ASourceExpr := GDBMIMaybeApplyBracketsToExpr(ASourceExpr)+'^';
      //if AisPointerPointer
      //then RefToken := '^'
      //else RefToken := '';
      //s := Format('type = %s%s = record %s%send%s', [RefToken, ABaseTypeName, LineEnding, s2, LineEnding]);
      //MaybeAdd(gcrtPType, GdbCmdPType  + ASourceExpr, s);

    end;
  end;

  procedure AddBaseType(ASourceExpr: string; AIsPointerType, AisPointerPointer: Boolean;
    ABaseTypeName, ASrcTypeName, ADeRefTypeName: String;
        ASrcType, ABaseType: TDbgSymbol
);
  begin
    if AIsPointerType then begin
      MaybeAdd(gcrtPType, GdbCmdPType  + ASourceExpr, Format('type = ^%s', [ABaseTypeName]));
      MaybeAdd(gcrtPType, GdbCmdWhatIs + ASourceExpr, Format('type = %s', [ASrcTypeName]));
      ASourceExpr := GDBMIMaybeApplyBracketsToExpr(ASourceExpr);
      if AIsPointerPointer then begin
        MaybeAdd(gcrtPType, GdbCmdPType  + ASourceExpr + '^', Format('type = ^%s', [ABaseTypeName]));
        MaybeAdd(gcrtPType, GdbCmdWhatIs + ASourceExpr + '^', Format('type = %s', [ADeRefTypeName]));
      end
      else begin
        MaybeAdd(gcrtPType, GdbCmdPType  + ASourceExpr + '^', Format('type = %s', [ABaseTypeName]));
        MaybeAdd(gcrtPType, GdbCmdWhatIs + ASourceExpr + '^', Format('type = %s', [ABaseTypeName]));
      end;
    end
    else begin
      MaybeAdd(gcrtPType, GdbCmdPType + ASourceExpr, Format('type = %s', [ABaseTypeName]));
      MaybeAdd(gcrtPType, GdbCmdWhatIs + ASourceExpr, Format('type = %s', [ABaseTypeName]));
    end;
  end;

  procedure AddType(ASourceExpr: string; ATypeIdent: TDbgSymbol);
  var
    SrcTypeName,     // The expressions own type name
    DeRefTypeName,   // one levvel of pointer followed
    BaseTypeName: String; // all poiters followed
    IsPointerPointer: Boolean;
    IsPointerType: Boolean;
    SrcType: TDbgSymbol;
  begin
    if (ASourceExpr = '') or (ATypeIdent = nil) then exit;

    IsPointerType := ATypeIdent.Kind = FpDbgClasses.skPointer;
    IsPointerPointer := False;
    SrcTypeName := ATypeIdent.Name;
    SrcType := ATypeIdent;
    if IsPointerType and (ATypeIdent.TypeInfo <> nil) then begin
      ATypeIdent := ATypeIdent.TypeInfo;
      if ATypeIdent = nil then exit;

      // resolved 1st pointer
      if SrcTypeName = '' then
        SrcTypeName := '^'+ATypeIdent.Name;
      IsPointerPointer := ATypeIdent.Kind = FpDbgClasses.skPointer;
      DeRefTypeName := ATypeIdent.Name;

      while (ATypeIdent.Kind = FpDbgClasses.skPointer) and (ATypeIdent.TypeInfo <> nil) do begin
        ATypeIdent := ATypeIdent.TypeInfo;
        if SrcTypeName = ''  then SrcTypeName := '^'+ATypeIdent.Name;
        if DeRefTypeName = '' then DeRefTypeName := '^'+ATypeIdent.Name;
      end;
      if ATypeIdent = nil then exit;
    end;
    BaseTypeName := ATypeIdent.Name;

DebugLn(['--------------'+dbgs(ATypeIdent.Kind), ' ', dbgs(IsPointerType)]);
    if ATypeIdent.Kind in [skInteger, skCardinal, skBoolean, skChar, skFloat]
    then begin
      AddBaseType(ASourceExpr, IsPointerType, IsPointerPointer, BaseTypeName,
                  SrcTypeName, DeRefTypeName, SrcType, ATypeIdent);
    end
    else
    if ATypeIdent.Kind in [FpDbgClasses.skClass]
    then begin
      AddClassType(ASourceExpr, IsPointerType, IsPointerPointer, BaseTypeName,
                  SrcTypeName, DeRefTypeName, SrcType, ATypeIdent);
    end
    else
    if ATypeIdent.Kind in [FpDbgClasses.skRecord]
    then begin
      AddRecordType(ASourceExpr, IsPointerType, IsPointerPointer, BaseTypeName,
                  SrcTypeName, DeRefTypeName, SrcType, ATypeIdent);
    end;
  end;

var
  IdentName: String;
  PasExpr: TFpGDBMIPascalExpression;
begin
  Result := inherited IndexOf(AThreadId, AStackFrame, ARequest);

  if (Result >= 0) or FInIndexOf then
    exit;

  FInIndexOf := True;
  PasExpr := nil;
  try
DebugLn('############### '+ARequest.Request);
    if copy(ARequest.Request, 1, 6) = 'ptype ' then
	     IdentName := trim(copy(ARequest.Request, 7, length(ARequest.Request)))
    else
    if copy(ARequest.Request, 1, 7) = 'whatis ' then
      IdentName := trim(copy(ARequest.Request, 8, length(ARequest.Request)));

    PasExpr := TFpGDBMIPascalExpression.Create(IdentName, FDebugger, AThreadId, AStackFrame);

    AddType(IdentName, PasExpr.ResultType);
    Result := inherited IndexOf(AThreadId, AStackFrame, ARequest);

  finally
    PasExpr.Free;
    FInIndexOf := False;
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

{ TFPGDBMIWatches }

function TFPGDBMIWatches.FpDebugger: TFpGDBMIDebugger;
begin
  Result := TFpGDBMIDebugger(Debugger);
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
  if not FImageLoader.IsValid then begin
    FreeAndNil(FImageLoader);
    exit;
  end;;
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
begin
  if HasDwarf and (ACommand = dcEvaluate) then begin
     //  String(AParams[0].VAnsiString)
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
  if CurrentThreadIdValid then begin
    AThreadId := CurrentThreadId;

    if CurrentStackFrameValid then
      AStackFrame := CurrentStackFrame
    else
      AStackFrame := 0;
  end
  else begin
    AThreadId := 1;
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

