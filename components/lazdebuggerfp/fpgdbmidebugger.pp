unit FpGdbmiDebugger;

{$mode objfpc}{$H+}

{$IFdef MSWindows}
{$DEFINE  WithWinMemReader}
{$ENDIF}

interface

uses
  {$IFdef WithWinMemReader}
  windows,
  {$ENDIF}
  Classes, sysutils, math, FpdMemoryTools, FpDbgInfo, FpDbgClasses, GDBMIDebugger,
  DbgIntfBaseTypes, DbgIntfDebuggerBase, GDBMIMiscClasses,
  GDBTypeInfo, maps, LCLProc, Forms, FpDbgLoader, FpDbgDwarf, FpDbgDwarfConst, LazLoggerBase,
  LazLoggerProfiling, LazClasses, FpPascalParser, FpPascalBuilder;

type

  TFpGDBMIDebugger = class;

  { TFpGDBMIDbgMemReader }

  TFpGDBMIDbgMemReader = class(TFpDbgMemReaderBase)
  private
// TODO
    //FThreadId: Integer;
    //FStackFrame: Integer;
    FDebugger: TFpGDBMIDebugger;
  public
    constructor Create(ADebugger: TFpGDBMIDebugger);
    function ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; override;
    function ReadMemoryEx(AnAddress, AnAddressSpace: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; override;
    function ReadRegister(ARegNum: Cardinal; out AValue: TDbgPtr): Boolean; override;
    function RegisterSize(ARegNum: Cardinal): Integer; override;
  end;

  { TFpGDBMIAndWin32DbgMemReader }

  TFpGDBMIAndWin32DbgMemReader = class(TFpGDBMIDbgMemReader)
  private
    hProcess: THandle;
  public
    destructor Destroy; override;
    function ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal; ADest: Pointer): Boolean; override;
    //function ReadRegister(ARegNum: Integer; out AValue: TDbgPtr): Boolean; override;
    procedure OpenProcess(APid: Cardinal);
    procedure CloseProcess;
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

const
  MAX_CTX_CACHE = 10;

type
  { TFpGDBMIDebugger }

  TFpGDBMIDebugger = class(TGDBMIDebugger)
  private
    FWatchEvalList: TList;
    FImageLoader: TDbgImageLoader;
    FDwarfInfo: TDbgDwarf;
    FMemReader: TFpGDBMIDbgMemReader;
    FMemManager: TFpDbgMemManager;
    // cache last context
    FlastStackFrame, FLastThread: Integer;
    FLastContext: array [0..MAX_CTX_CACHE-1] of TDbgInfoAddressContext;
  protected
    function CreateCommandStartDebugging(AContinueCommand: TGDBMIDebuggerCommand): TGDBMIDebuggerCommandStartDebugging; override;
    function CreateLineInfo: TDBGLineInfo; override;
    function  CreateWatches: TWatchesSupplier; override;
    procedure DoState(const OldState: TDBGState); override;
    function  HasDwarf: Boolean;
    procedure LoadDwarf;
    procedure UnLoadDwarf;
    function  RequestCommand(const ACommand: TDBGCommand; const AParams: array of const): Boolean; override;
    procedure QueueCommand(const ACommand: TGDBMIDebuggerCommand; ForceQueue: Boolean = False);

    procedure GetCurrentContext(out AThreadId, AStackFrame: Integer);
    function  GetLocationForContext(AThreadId, AStackFrame: Integer): TDBGPtr;
    function  GetInfoContextForContext(AThreadId, AStackFrame: Integer): TDbgInfoAddressContext;
    function CreateTypeRequestCache: TGDBPTypeRequestCache; override;
    property CurrentCommand;
    property TargetPID;
  protected
    procedure DoWatchFreed(Sender: TObject);
    function EvaluateExpression(AWatchValue: TWatchValueBase;
                                AExpression: String;
                                var AResText: String;
                                out ATypeInfo: TDBGType;
                                EvalFlags: TDBGEvaluateFlags = []): Boolean;
  public
    class function Caption: String; override;
  public
    constructor Create(const AExternalDebugger: String); override;
    destructor Destroy; override;
  end;

procedure Register;

implementation

type

  { TFpGDBMIDebuggerCommandStartDebugging }

  TFpGDBMIDebuggerCommandStartDebugging = class(TGDBMIDebuggerCommandStartDebugging)
  protected
    function DoExecute: Boolean; override;
  end;

  TFPGDBMIWatches = class;

  { TFpGDBMIDebuggerCommandEvaluate }

  TFpGDBMIDebuggerCommandEvaluate = class(TGDBMIDebuggerCommand)
  private
    FOwner: TFPGDBMIWatches;
  protected
    function DoExecute: Boolean; override;
    procedure DoFree; override;
    procedure DoCancel; override;
    procedure DoLockQueueExecute; override;
    procedure DoUnLockQueueExecute; override;
  public
    constructor Create(AOwner: TFPGDBMIWatches);
  end;

  { TFPGDBMIWatches }

  TFPGDBMIWatches = class(TGDBMIWatches)
  private
    FWatchEvalLock: Integer;
    FNeedRegValues: Boolean;
    FEvaluationCmdObj: TFpGDBMIDebuggerCommandEvaluate;
  protected
    function  FpDebugger: TFpGDBMIDebugger;
    //procedure DoStateChange(const AOldState: TDBGState); override;
    procedure ProcessEvalList;
    procedure QueueCommand;
    procedure InternalRequestData(AWatchValue: TWatchValueBase); override;
  public
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
    constructor Create(const ADebugger: TDebuggerIntf);
    destructor Destroy; override;
    function Count: Integer; override;
    function GetAddress(const AIndex: Integer; const ALine: Integer): TDbgPtr; override;
    function GetInfo(AAdress: TDbgPtr; out ASource, ALine, AOffset: Integer): Boolean; override;
    function IndexOf(const ASource: String): integer; override;
    procedure Request(const ASource: String); override;
    procedure Cancel(const ASource: String); override;
  end;

{ TFpGDBMIDebuggerCommandEvaluate }

function TFpGDBMIDebuggerCommandEvaluate.DoExecute: Boolean;
begin
  FOwner.FEvaluationCmdObj := nil;
  FOwner.ProcessEvalList;
  Result := True;
end;

procedure TFpGDBMIDebuggerCommandEvaluate.DoFree;
begin
  FOwner.FEvaluationCmdObj := nil;
  inherited DoFree;
end;

procedure TFpGDBMIDebuggerCommandEvaluate.DoCancel;
begin
  FOwner.FpDebugger.FWatchEvalList.Clear;
  FOwner.FEvaluationCmdObj := nil;
  inherited DoCancel;
end;

procedure TFpGDBMIDebuggerCommandEvaluate.DoLockQueueExecute;
begin
  //
end;

procedure TFpGDBMIDebuggerCommandEvaluate.DoUnLockQueueExecute;
begin
  //
end;

constructor TFpGDBMIDebuggerCommandEvaluate.Create(AOwner: TFPGDBMIWatches);
begin
  inherited Create(AOwner.FpDebugger);
  FOwner := AOwner;
end;

{ TFpGDBMIAndWin32DbgMemReader }

destructor TFpGDBMIAndWin32DbgMemReader.Destroy;
begin
  CloseProcess;
  inherited Destroy;
end;

function TFpGDBMIAndWin32DbgMemReader.ReadMemory(AnAddress: TDbgPtr;
  ASize: Cardinal; ADest: Pointer): Boolean;
var
  BytesRead: Cardinal;
begin
  {$IFdef MSWindows}
  Result := ReadProcessMemory(
    hProcess,
    Pointer(AnAddress),
    ADest, ASize,
    BytesRead) and
  (BytesRead = ASize);
//DebugLn(['*&*&*&*& ReadMem ', dbgs(Result), '  at ', AnAddress, ' Size ',ASize, ' br=',BytesRead, ' b1',PBYTE(ADest)^]);
  {$ELSE}
  Result := inherited ReadMemory(AnAddress, ASize, ADest);
  {$ENDIF}
end;

procedure TFpGDBMIAndWin32DbgMemReader.OpenProcess(APid: Cardinal);
begin
  {$IFdef MSWindows}
  debugln(['OPEN process ',APid]);
  if APid <> 0 then
    hProcess := windows.OpenProcess(PROCESS_CREATE_THREAD or PROCESS_QUERY_INFORMATION or PROCESS_VM_OPERATION or PROCESS_VM_WRITE or PROCESS_VM_READ, False, APid);
  {$ENDIF}
end;

procedure TFpGDBMIAndWin32DbgMemReader.CloseProcess;
begin
  {$IFdef MSWindows}
  if hProcess <> 0 then
    CloseHandle(hProcess);
  {$ENDIF}
end;

{ TFpGDBMIDbgMemReader }

constructor TFpGDBMIDbgMemReader.Create(ADebugger: TFpGDBMIDebugger);
begin
  FDebugger := ADebugger;
end;

type TGDBMIDebuggerCommandHack = class(TGDBMIDebuggerCommand) end;

function TFpGDBMIDbgMemReader.ReadMemory(AnAddress: TDbgPtr; ASize: Cardinal;
  ADest: Pointer): Boolean;
var
  cmd: TGDBMIDebuggerCommandHack;
  R: TGDBMIExecResult;
  MemDump: TGDBMIMemoryDumpResultList;
  i: Integer;
begin
  Result := False;

  cmd := TGDBMIDebuggerCommandHack(TFpGDBMIDebugger(FDebugger).CurrentCommand);
  if cmd = nil then exit;

  if not cmd.ExecuteCommand('-data-read-memory %u x 1 1 %u', [AnAddress, ASize], R, [cfNoThreadContext, cfNoStackContext])
  then
    exit;
  if R.State = dsError then exit;

  MemDump := TGDBMIMemoryDumpResultList.Create(R);
  if MemDump.Count <> ASize then exit;

  for i := 0 to MemDump.Count - 1 do begin
    PByte(ADest + i)^ := Byte(MemDump.ItemNum[i]);
  end;

  MemDump.Free;
  Result := True;

debugln(['TFpGDBMIDbgMemReader.ReadMemory ', dbgs(AnAddress), '  ', dbgMemRange(ADest, ASize)]);
end;

function TFpGDBMIDbgMemReader.ReadMemoryEx(AnAddress, AnAddressSpace: TDbgPtr;
  ASize: Cardinal; ADest: Pointer): Boolean;
begin
  Result := False;
end;

function TFpGDBMIDbgMemReader.ReadRegister(ARegNum: Cardinal; out
  AValue: TDbgPtr): Boolean;
var
  rname: String;
  v: String;
  i: Integer;
begin
  Result := False;
  // 32 bit gdb dwarf names
  case ARegNum of
    0:  rname := 'EAX'; // RAX
    1:  rname := 'ECX'; // RDX
    2:  rname := 'EDX'; // RCX
    3:  rname := 'EBX'; // RBX
    4:  rname := 'ESP';
    5:  rname := 'EBP';
    6:  rname := 'ESI';
    7:  rname := 'EDI';
    8:  rname := 'EIP';
    else
      exit;
  end;
  for i := 0 to FDebugger.Registers.Count - 1 do
    if UpperCase(FDebugger.Registers.Names[i]) = rname then
      begin
        v := FDebugger.Registers.Values[i];
debugln(['TFpGDBMIDbgMemReader.ReadRegister ',rname, '  ', v]);
        Result := true;
        try
          AValue := StrToQWord(v);
        except
          Result := False;
        end;
        exit;
      end;
end;

function TFpGDBMIDbgMemReader.RegisterSize(ARegNum: Cardinal): Integer;
begin
  Result := 4; // for the very few supported...
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
  GdbCmdEval = '-data-evaluate-expression ';

  procedure AddType(ASourceExpr: string; ATypeIdent: TDbgSymbol; AVal: TDbgSymbolValue = nil); forward;

  procedure FindPointerAndBaseType(ASrcType: TDbgSymbol;
    out APointerLevel: Integer; out ADeRefType, ABaseType: TDbgSymbol;
    out ASrcTypeName, ADeRefTypeName, ABaseTypeName: String);
  begin
    APointerLevel := 0;

    ADeRefType := nil;
    ABaseType  := ASrcType;
    ASrcTypeName   := ASrcType.Name;
    ADeRefTypeName := '';
    ABaseTypeName  := ABaseType.Name;

    while (ABaseType.Kind = skPointer) and (ABaseType.TypeInfo <> nil) do begin
      ABaseType := ABaseType.TypeInfo;
      inc(APointerLevel);

      if ABaseType.Name <> '' then
      begin
        if ASrcTypeName = '' then
          ASrcTypeName := '^' + ABaseType.Name;
        if ADeRefTypeName = '' then begin
          if APointerLevel = 1
          then ADeRefTypeName := ABaseType.Name
          else ADeRefTypeName := '^'+ ABaseType.Name;
        end
      end;

    end;

    ABaseTypeName  := ABaseType.Name;
  end;

  Function MembersAsGdbText(AStructType: TDbgSymbol; WithVisibilty: Boolean; out AText: String): Boolean;
  var
    CurVis: TDbgSymbolMemberVisibility;

    procedure AddVisibility(AVis: TDbgSymbolMemberVisibility);
    begin
      CurVis := AVis;
      if not WithVisibilty then
        exit;
      if AText <> '' then AText := AText + LineEnding;
      case AVis of
        svPrivate:   AText := AText + '  private' + LineEnding;
        svProtected: AText := AText + '  protected' + LineEnding;
        svPublic:    AText := AText + '  public' + LineEnding;
      end;
    end;

    procedure AddMember(AMember: TDbgSymbol);
    var
      ti: TDbgSymbol;
      s, s2: String;
    begin
//todo: functions / virtual / array ...
      s2 := '';
      if AMember.Kind = skProcedure then begin
        if sfVirtual in AMember.Flags then s2 := ' virtual;';
        AText := AText + '    procedure ' + AMember.Name + ' ();' + s2 + LineEnding;
        exit
      end;

      ti := AMember.TypeInfo;
      if ti = nil then begin
        Result := False;
        exit;
      end;

      s := ti.Name;
      if s = '' then begin
        if not( (AMember.Kind = skSet) or (AMember.Kind = skEnum) or
                (AMember.Kind = skArray) or (AMember.Kind = skPointer) or
                (AMember.Kind = skRecord)
              )
        then begin
          Result := False;
          exit;
        end;
        if not GetTypeAsDeclaration(s, ti, [tdfSkipClassBody, tdfSkipRecordBody]) then begin
          Result := False;
          exit;
        end
      end;

      if AMember.Kind = skFunction then begin
        if sfVirtual in AMember.Flags then s2 := ' virtual;';
        AText := AText + '    function  ' + AMember.Name + ' () : '+s+';' + s2 + LineEnding;
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
    if inherited IndexOf(AThreadId, AStackFrame, AReq) < 0 then begin
      if AType = gcrtPType then
        AReq.Result := ParseTypeFromGdb(AAnswer)
      else begin
        AReq.Result.GdbDescription := AAnswer;
        AReq.Result.Kind := ptprkSimple;
      end;
      Add(AThreadId, AStackFrame, AReq);
      debugln(['**** AddToGDBMICache ', AReq.Request, ' T:', AThreadId, ' S:',AStackFrame]);
      //debugln(['**** AddToGDBMICache ', AReq.Request, ' T:', AThreadId, ' S:',AStackFrame, ' >>>> ', AAnswer, ' <<<<']);
    end;
  end;

  procedure AddBaseType(ASourceExpr: string; APointerLevel: Integer;
    ASrcTypeName, ADeRefTypeName, ABaseTypeName: String;
    ASrcType, ABaseType: TDbgSymbol);
  var
    s, s2, RefToken: String;
  begin
    if sfSubRange in ABaseType.Flags then begin
      GetTypeAsDeclaration(s, ABaseType);
      if APointerLevel > 0
      then RefToken := '^'
      else RefToken := '';
      s2 := ASrcType.Name;
      if s2 = '' then s2 := s;

      MaybeAdd(gcrtPType, GdbCmdPType + ASourceExpr, Format('type = %s%s', [RefToken, s]));
      MaybeAdd(gcrtPType, GdbCmdWhatIs + ASourceExpr, Format('type = %s%s', [RefToken, s2]));

      if APointerLevel > 0 then begin
        if APointerLevel > 1
        then RefToken := '^'
        else RefToken := '';
        if (ADeRefTypeName = '') or (ADeRefTypeName[1] = '^') then
          ADeRefTypeName := RefToken + s;

        ASourceExpr := GDBMIMaybeApplyBracketsToExpr(ASourceExpr)+'^';
        MaybeAdd(gcrtPType, GdbCmdPType + ASourceExpr, Format('type = %s%s', [RefToken, s]));
        MaybeAdd(gcrtPType, GdbCmdWhatIs + ASourceExpr, Format('type = %s%s', [ADeRefTypeName]));
      end;

      exit; // subrange
    end;

    if APointerLevel > 0 then begin
      MaybeAdd(gcrtPType, GdbCmdPType  + ASourceExpr, Format('type = ^%s', [ABaseTypeName]));
      MaybeAdd(gcrtPType, GdbCmdWhatIs + ASourceExpr, Format('type = %s', [ASrcTypeName]));
      ASourceExpr := GDBMIMaybeApplyBracketsToExpr(ASourceExpr);
      if APointerLevel > 1 then begin
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

  procedure AddClassType(ASourceExpr: string; APointerLevel: Integer;
    ASrcTypeName, ADeRefTypeName, ABaseTypeName: String;
    ASrcType, ABaseType: TDbgSymbol);
  var
    s, ParentName, RefToken: String;
    s2: String;
  begin
    if APointerLevel = 0 then
      ADeRefTypeName := ASrcTypeName;
    if not MembersAsGdbText(ABaseType, True, s2) then
      exit;

    if (ABaseType.TypeInfo <> nil) then begin
      ParentName :=  ABaseType.TypeInfo.Name;
      if ParentName <> '' then
        ParentName := ' public ' + ParentName;
    end
    else
      ParentName := '';

    s := Format('type = ^%s = class :%s %s%send%s', [ABaseTypeName, ParentName, LineEnding, s2, LineEnding]);
    MaybeAdd(gcrtPType, GdbCmdPType  + ASourceExpr, s);

    s := Format('type = %s%s', [ASrcTypeName, LineEnding]);
    MaybeAdd(gcrtPType, GdbCmdWhatIs  + ASourceExpr, s);


    ASourceExpr := GDBMIMaybeApplyBracketsToExpr(ASourceExpr)+'^';
    if APointerLevel > 0
    then RefToken := '^'
    else RefToken := '';
    s := Format('type = %s%s = class :%s %s%send%s', [RefToken, ABaseTypeName, ParentName, LineEnding, s2, LineEnding]);
    MaybeAdd(gcrtPType, GdbCmdPType  + ASourceExpr, s);

    s := Format('type = %s%s', [ADeRefTypeName, LineEnding]);
    MaybeAdd(gcrtPType, GdbCmdWhatIs  + ASourceExpr, s);
  end;

  procedure AddRecordType(ASourceExpr: string; APointerLevel: Integer;
    ASrcTypeName, ADeRefTypeName, ABaseTypeName: String;
    ASrcType, ABaseType: TDbgSymbol);
  var
    s, RefToken: String;
    s2: String;
  begin
    if not MembersAsGdbText(ABaseType, False, s2) then
      exit;

    if APointerLevel > 0
    then RefToken := '^'
    else RefToken := '';
    s := Format('type = %s%s = record %s%send%s', [RefToken, ABaseTypeName, LineEnding, s2, LineEnding]);
    MaybeAdd(gcrtPType, GdbCmdPType  + ASourceExpr, s);

    if APointerLevel > 0 then begin
      s := Format('type = %s%s', [ASrcTypeName, LineEnding]);
      MaybeAdd(gcrtPType, GdbCmdWhatIs  + ASourceExpr, s);
    end;
  end;

  procedure AddEnumType(ASourceExpr: string; APointerLevel: Integer;
    ASrcTypeName, ADeRefTypeName, ABaseTypeName: String;
    ASrcType, ABaseType: TDbgSymbol);
  var
    s, s2, RefToken: String;
  begin
    if APointerLevel > 0
    then RefToken := '^'
    else RefToken := '';
    if GetTypeAsDeclaration(s2, ABaseType) then begin
      s := Format('type = %s%s = %s%s', [RefToken, ABaseTypeName, s2, LineEnding]);
      MaybeAdd(gcrtPType, GdbCmdPType  + ASourceExpr, s);
      if APointerLevel > 0 then
        MaybeAdd(gcrtPType, GdbCmdWhatIs  + ASourceExpr, 'type = ' + ASrcTypeName);
    end;
  end;

  procedure AddSetType(ASourceExpr: string; APointerLevel: Integer;
    ASrcTypeName, ADeRefTypeName, ABaseTypeName: String;
    ASrcType, ABaseType: TDbgSymbol);
  var
    s, s2, RefToken: String;
  begin
    case APointerLevel of
      0:  RefToken := '';
      1:  RefToken := '^';
      else  RefToken := '^^';
    end;

    if GetTypeAsDeclaration(s2, ABaseType) then begin
      s := Format('type = %s%s%s', [RefToken, s2, LineEnding]);
      MaybeAdd(gcrtPType, GdbCmdPType  + ASourceExpr, s);
      if ASrcTypeName <> ''
      then MaybeAdd(gcrtPType, GdbCmdWhatIs + ASourceExpr, 'type = ' + ASrcTypeName)
      else MaybeAdd(gcrtPType, GdbCmdWhatIs + ASourceExpr, s);
    end;
  end;

  procedure AddArrayType(ASourceExpr: string; APointerLevel: Integer;
    ASrcTypeName, ADeRefTypeName, ABaseTypeName: String;
    ASrcType, ABaseType: TDbgSymbol);
  var
    s: String;
    ElemPointerLevel: Integer;
    ElemDeRefType, ElemBaseType: TDbgSymbol;
    ElemSrcTypeName, ElemDeRefTypeName, ElemBaseTypeName: String;
  begin
    if sfDynArray in ABaseType.Flags then begin
      // dyn
      if ABaseType.TypeInfo = nil then exit;
      FindPointerAndBaseType(ABaseType.TypeInfo, ElemPointerLevel,
                             ElemDeRefType, ElemBaseType,
                             ElemSrcTypeName, ElemDeRefTypeName, ElemBaseTypeName);

      s := ElemSrcTypeName;
      if (s = '') then begin
        if not GetTypeAsDeclaration(s, ABaseType.TypeInfo, [tdfDynArrayWithPointer]) then
          exit;
        s := Format('type = %s%s', [StringOfChar('^', APointerLevel), s]);
      end
      else
        s := Format('type = %s%s', ['^', s]); // ElemSrcTypeName already has ^, if it is pointer
      MaybeAdd(gcrtPType, GdbCmdPType  + ASourceExpr, s + LineEnding);

      s := ASrcTypeName;
      if (s = '') then begin
        if not GetTypeAsDeclaration(s, ASrcType, [tdfDynArrayWithPointer]) then
          exit;
        s := Format('type = %s%s', [StringOfChar('^', APointerLevel), s]);
      end
      else
        s := Format('type = %s', [s]);
      MaybeAdd(gcrtPType, GdbCmdWhatIs + ASourceExpr, s + LineEnding);

      // deref
      ASourceExpr := GDBMIMaybeApplyBracketsToExpr(ASourceExpr)+'^';
      if APointerLevel = 0 then begin
        if not GetTypeAsDeclaration(s, ASrcType, [tdfDynArrayWithPointer]) then
          exit;
        if s[1] = '^' then begin
          Delete(s,1,1);
          if (s <> '') and (s[1] = '(') and (s[Length(s)] = ')') then begin
            Delete(s,Length(s),1);
            Delete(s,1,1);
          end;
        end;
        s := Format('type = %s%s', [s, LineEnding]);
        MaybeAdd(gcrtPType, GdbCmdPType  + ASourceExpr, s);

        AddType(ASourceExpr+'[0]', ABaseType.TypeInfo);
      end
      else begin
        s := ElemSrcTypeName;
        if (s = '') then begin
          if not GetTypeAsDeclaration(s, ABaseType.TypeInfo, [tdfDynArrayWithPointer]) then
            exit;
          s := Format('type = %s%s', [StringOfChar('^', APointerLevel-1), s]);
        end
        else
          s := Format('type = ^%s', [s]);
        MaybeAdd(gcrtPType, GdbCmdPType  + ASourceExpr, s + LineEnding);
      end;

    end
    else begin
      // stat
      if GetTypeAsDeclaration(s, ASrcType, [tdfDynArrayWithPointer]) then begin
        s := Format('type = %s%s', [s, LineEnding]);
        MaybeAdd(gcrtPType, GdbCmdPType  + ASourceExpr, s);
        if ASrcTypeName <> ''
        then MaybeAdd(gcrtPType, GdbCmdWhatIs + ASourceExpr, 'type = ' + ASrcTypeName)
        else MaybeAdd(gcrtPType, GdbCmdWhatIs + ASourceExpr, s);
      end;

      if APointerLevel = 0 then exit;
      ASrcType := ASrcType.TypeInfo;
      if GetTypeAsDeclaration(s, ASrcType, [tdfDynArrayWithPointer]) then begin
        ASourceExpr := GDBMIMaybeApplyBracketsToExpr(ASourceExpr)+'^';
        s := Format('type = %s%s', [s, LineEnding]);
        MaybeAdd(gcrtPType, GdbCmdPType  + ASourceExpr, s);
        if ASrcTypeName <> ''
        then MaybeAdd(gcrtPType, GdbCmdWhatIs + ASourceExpr, 'type = ' + ADeRefTypeName)
        else MaybeAdd(gcrtPType, GdbCmdWhatIs + ASourceExpr, s);
      end;
    end;
  end;

  procedure AddType(ASourceExpr: string; ATypeIdent: TDbgSymbol; AVal: TDbgSymbolValue = nil);
  var
    SrcTypeName,     // The expressions own type name
    DeRefTypeName,   // one levvel of pointer followed
    BaseTypeName: String; // all poiters followed
    DeRefType, BaseType: TDbgSymbol;
    PointerLevel: Integer;
    s: String;
    i: Integer;
  begin
    if (ASourceExpr = '') or (ATypeIdent = nil) then exit;

    FindPointerAndBaseType(ATypeIdent, PointerLevel,
                           DeRefType, BaseType,
                           SrcTypeName, DeRefTypeName, BaseTypeName);

    case BaseType.Kind of
      skInteger, skCardinal, skBoolean: begin
        AddBaseType(ASourceExpr, PointerLevel,
                    SrcTypeName, DeRefTypeName, BaseTypeName,
                    ATypeIdent, BaseType);
        if (AVal <> nil) and (ATypeIdent.Kind = skInteger) then
          MaybeAdd(gcrtEvalExpr, GdbCmdEval + ASourceExpr, Format(',value="%d"', [AVal.AsInteger]))
        else
        if (AVal <> nil) and (ATypeIdent.Kind = skCardinal) then
          MaybeAdd(gcrtEvalExpr, GdbCmdEval + ASourceExpr, Format(',value="%u"', [AVal.AsCardinal]))
        else
        if (AVal <> nil) and (ATypeIdent.Kind = skBoolean) then
          MaybeAdd(gcrtEvalExpr, GdbCmdEval + ASourceExpr, Format(',value="%s"', [dbgs(AVal.AsBool)]))
        else
        if (AVal <> nil) and (ATypeIdent.Kind = skPointer) then
          MaybeAdd(gcrtEvalExpr, GdbCmdEval + ASourceExpr, Format(',value="%u"', [AVal.AsCardinal]))
        ;
      end;
      skChar, skFloat:
        AddBaseType(ASourceExpr, PointerLevel,
                    SrcTypeName, DeRefTypeName, BaseTypeName,
                    ATypeIdent, BaseType);
      skClass:
        AddClassType(ASourceExpr, PointerLevel,
                    SrcTypeName, DeRefTypeName, BaseTypeName,
                    ATypeIdent, BaseType);
      skRecord:
        AddRecordType(ASourceExpr, PointerLevel,
                    SrcTypeName, DeRefTypeName, BaseTypeName,
                    ATypeIdent, BaseType);
      skEnum: begin
        AddEnumType(ASourceExpr, PointerLevel,
                    SrcTypeName, DeRefTypeName, BaseTypeName,
                    ATypeIdent, BaseType);
        if (AVal <> nil) and (ATypeIdent.Kind = skEnum) then
          if AVal.AsString = ''
          then MaybeAdd(gcrtEvalExpr, GdbCmdEval + ASourceExpr, Format(',value="%u"', [AVal.AsCardinal]))
          else MaybeAdd(gcrtEvalExpr, GdbCmdEval + ASourceExpr, Format(',value="%s"', [AVal.AsString]));
      end;
      skSet: begin
        AddSetType(ASourceExpr, PointerLevel,
                    SrcTypeName, DeRefTypeName, BaseTypeName,
                    ATypeIdent, BaseType);
        if (AVal <> nil) and (ATypeIdent.Kind = skSet) then begin
          s := '';
          for i := 0 to AVal.MemberCount-1 do
            if i = 0
            then s := AVal.Member[i].AsString
            else s := s + ', ' + AVal.Member[i].AsString;
          MaybeAdd(gcrtEvalExpr, GdbCmdEval + ASourceExpr, Format(',value="[%s]"', [s]))
        end;
      end;
      skArray:
        AddArrayType(ASourceExpr, PointerLevel,
                    SrcTypeName, DeRefTypeName, BaseTypeName,
                    ATypeIdent, BaseType);
    end;

  end;

var
  IdentName: String;
  PasExpr: TFpPascalExpression;
  rt: TDbgSymbol;
begin
  Result := inherited IndexOf(AThreadId, AStackFrame, ARequest);
DebugLn(['######## '+ARequest.Request, ' ## FOUND: ', dbgs(Result)]);

  if (Result >= 0) or FInIndexOf then
    exit;

  FInIndexOf := True;
  PasExpr := nil;
  try
    if (ARequest.ReqType = gcrtPType) and (length(ARequest.Request) > 0) then begin
      case ARequest.Request[1] of
        'p': if copy(ARequest.Request, 1, 6) = 'ptype ' then
               IdentName := trim(copy(ARequest.Request, 7, length(ARequest.Request)));
        'w': if copy(ARequest.Request, 1, 7) = 'whatis ' then
               IdentName := trim(copy(ARequest.Request, 8, length(ARequest.Request)));
      end;

      if IdentName <> '' then begin
        PasExpr := TFpPascalExpression.Create(IdentName, FDebugger.GetInfoContextForContext(AThreadId, AStackFrame));
        rt := nil;
        if PasExpr.Valid and (PasExpr.ResultValue <> nil) then begin
          rt := PasExpr.ResultValue.DbgSymbol; // value or typecast
if rt <> nil then  debugln(['@@@@@ ',rt.ClassName, '   ADDR=', dbgs(rt.Address)]);
DebugLn(['== VAL === ', PasExpr.ResultValue.AsInteger, '  /  ', PasExpr.ResultValue.AsCardinal,  '  /  ', PasExpr.ResultValue.AsBool,  '  /  ', PasExpr.ResultValue.AsString,  '  /  ', PasExpr.ResultValue.MemberCount,  '  /  ', PasExpr.ResultValue.AsFloat]);

          if (rt <> nil) and (rt is TDbgDwarfValueIdentifier) then begin
            // symbol is value
            rt := rt.TypeInfo;
            AddType(IdentName, rt, PasExpr.ResultValue);
            Result := inherited IndexOf(AThreadId, AStackFrame, ARequest);
          end
          else
          if rt <> nil then begin
            // symbol is type
            AddType(IdentName, rt, nil);
            Result := inherited IndexOf(AThreadId, AStackFrame, ARequest);
          end;
        end
else DebugLn(['NOT VALID ', PasExpr.DebugDump(True)])
        ;
      end;
    end;

  finally
    PasExpr.Free;
    FInIndexOf := False;
  end;

end;

{ TFPGDBMIWatches }

function TFPGDBMIWatches.FpDebugger: TFpGDBMIDebugger;
begin
  Result := TFpGDBMIDebugger(Debugger);
end;

procedure TFPGDBMIWatches.ProcessEvalList;
var
  WatchValue: TWatchValueBase;
  ResTypeInfo: TDBGType;
  ResText: String;

  function IsWatchValueAlive: Boolean;
  begin
    Result := (FpDebugger.FWatchEvalList.Count > 0) and (FpDebugger.FWatchEvalList[0] = Pointer(WatchValue));
  end;
begin
  if FNeedRegValues then begin
    FNeedRegValues := False;
    FpDebugger.Registers.Values[0];
    QueueCommand;
    exit;
  end;

  if FWatchEvalLock > 0 then
    exit;
  inc(FWatchEvalLock);
  try // TODO: if the stack/thread is changed, registers will be wrong
    while (FpDebugger.FWatchEvalList.Count > 0) and (FEvaluationCmdObj = nil) do begin
      try
        WatchValue := TWatchValueBase(FpDebugger.FWatchEvalList[0]);
        ResTypeInfo := nil;
        if not FpDebugger.EvaluateExpression(WatchValue, WatchValue.Expression, ResText, ResTypeInfo)
        then begin
          debugln(['TFPGDBMIWatches.InternalRequestData FAILED']);
          if IsWatchValueAlive then
            inherited InternalRequestData(WatchValue);
        end;
      finally
        if IsWatchValueAlive then begin
          WatchValue.RemoveFreeeNotification(@FpDebugger.DoWatchFreed);
          FpDebugger.FWatchEvalList.Remove(pointer(WatchValue));
        end;
        Application.ProcessMessages;
      end;
    end;
  finally
    dec(FWatchEvalLock);
  end;
end;

procedure TFPGDBMIWatches.QueueCommand;
begin
  FEvaluationCmdObj := TFpGDBMIDebuggerCommandEvaluate.Create(Self);
  FEvaluationCmdObj.Properties := [dcpCancelOnRun];
  // If a ExecCmd is running, then defer exec until the exec cmd is done
  FpDebugger.QueueCommand(FEvaluationCmdObj, ForceQueuing);
end;

procedure TFPGDBMIWatches.InternalRequestData(AWatchValue: TWatchValueBase);
begin
  if (Debugger = nil) or not(Debugger.State in [dsPause, dsInternalPause]) then begin
    AWatchValue.Validity := ddsInvalid;
    Exit;
  end;

  AWatchValue.AddFreeeNotification(@FpDebugger.DoWatchFreed); // we may call gdb
  FpDebugger.FWatchEvalList.Add(pointer(AWatchValue));

  if FEvaluationCmdObj <> nil then exit;

  FpDebugger.Threads.CurrentThreads.Count; // trigger threads, in case
  if FpDebugger.Registers.Count = 0 then   // trigger register, in case
    FNeedRegValues := True
  else
  begin
    FNeedRegValues := False;
    FpDebugger.Registers.Values[0];
  end;

  // Join the queue, registers and threads are needed first
  QueueCommand;
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

constructor TFpGDBMILineInfo.Create(const ADebugger: TDebuggerIntf);
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
{$IFdef WithWinMemReader}
  TFpGDBMIAndWin32DbgMemReader(TFpGDBMIDebugger(FTheDebugger).FMemReader).OpenProcess(
    TFpGDBMIDebugger(FTheDebugger).TargetPid
  );
{$ENDIF}
end;

{ TFpGDBMIDebugger }

procedure TFpGDBMIDebugger.DoState(const OldState: TDBGState);
var
  i: Integer;
begin
  inherited DoState(OldState);
  if State in [dsStop, dsError, dsNone] then
    UnLoadDwarf;

  if OldState in [dsPause, dsInternalPause] then begin
    for i := 0 to MAX_CTX_CACHE-1 do
      ReleaseRefAndNil(FLastContext[i]);
    if not(State in [dsPause, dsInternalPause]) then begin
      for i := 0 to FWatchEvalList.Count - 1 do begin
        TWatchValueBase(FWatchEvalList[i]).RemoveFreeeNotification(@DoWatchFreed);
        //TWatchValueBase(FWatchEvalList[i]).Validity := ddsInvalid;
      end;
      FWatchEvalList.Clear;
    end;
  end;
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
  end;
{$IFdef WithWinMemReader}
  FMemReader := TFpGDBMIAndWin32DbgMemReader.Create(Self);
{$Else}
  FMemReader := TFpGDBMIDbgMemReader.Create(Self);
{$ENDIF}
  FMemManager := TFpDbgMemManager.Create(FMemReader, TFpDbgMemConvertorLittleEndian.Create);

  FDwarfInfo := TDbgDwarf.Create(FImageLoader);
  FDwarfInfo.MemManager := FMemManager;
  FDwarfInfo.LoadCompilationUnits;
end;

procedure TFpGDBMIDebugger.UnLoadDwarf;
begin
  debugln(['TFpGDBMIDebugger.UnLoadDwarf ']);
  FreeAndNil(FDwarfInfo);
  FreeAndNil(FImageLoader);
  FreeAndNil(FMemReader);
  if FMemManager <> nil then
    FMemManager.TargetMemConvertor.Free;
  FreeAndNil(FMemManager);
end;

function TFpGDBMIDebugger.RequestCommand(const ACommand: TDBGCommand;
  const AParams: array of const): Boolean;
var
  EvalFlags: TDBGEvaluateFlags;
begin
  if HasDwarf and (ACommand = dcEvaluate) then begin
    EvalFlags := [];
    EvalFlags := TDBGEvaluateFlags(AParams[3].VInteger);
    Result := EvaluateExpression(nil, String(AParams[0].VAnsiString),
      String(AParams[1].VPointer^), TDBGType(AParams[2].VPointer^),
      EvalFlags);
    if not Result then
      Result := inherited RequestCommand(ACommand, AParams);
  end
  else
    Result := inherited RequestCommand(ACommand, AParams);
end;

procedure TFpGDBMIDebugger.QueueCommand(const ACommand: TGDBMIDebuggerCommand;
  ForceQueue: Boolean);
begin
  inherited QueueCommand(ACommand, ForceQueue);
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
  t: TCallStackEntryBase;
  s: TCallStackBase;
  f: TCallStackEntryBase;
  //Instr: TGDBMIDebuggerInstruction;
begin
(*
  Instr := TGDBMIDebuggerInstruction.Create(Format('-stack-list-frames %d %d', [AStackFrame, AStackFrame]), AThreadId, [], 0);
  Instr.AddReference;
  Instr.Cmd := TGDBMIDebuggerCommand.Create(Self);
  FTheDebugger.FInstructionQueue.RunInstruction(Instr);
  ok := Instr.IsSuccess and Instr.FHasResult;
  AResult := Instr.ResultData;
  Instr.Cmd.ReleaseReference;
  Instr.Cmd := nil;
  Instr.ReleaseReference;

  if ok then begin
    List := TGDBMINameValueList.Create(R, ['stack']);
    Result := List.Values['frame'];
    List.Free;
  end;
*)


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

function TFpGDBMIDebugger.GetInfoContextForContext(AThreadId,
  AStackFrame: Integer): TDbgInfoAddressContext;
var
  Addr: TDBGPtr;
begin
  Result := nil;
  if FDwarfInfo = nil then
    exit;

  if (AThreadId <= 0) then begin
    GetCurrentContext(AThreadId, AStackFrame);
  end;

  Addr := GetLocationForContext(AThreadId, AStackFrame);

  if Addr = 0 then begin
    Result := nil;
    exit;
  end;

  if (AStackFrame >= FlastStackFrame) and
     (AStackFrame - FlastStackFrame < MAX_CTX_CACHE) and
     (FLastContext[AStackFrame - FlastStackFrame] <> nil) and
     (FLastContext[AStackFrame - FlastStackFrame].Address = Addr)
  then begin
DebugLn('******* cached contex <<<<<<<<<<<');
    Result := FLastContext[AStackFrame - FlastStackFrame];
    exit;
  end;

  Result := FDwarfInfo.FindContext(Addr);

  FLastThread := AThreadId;
  FlastStackFrame := AStackFrame;
  FLastContext[0].ReleaseReference;
  FLastContext[0] := Result;
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

procedure TFpGDBMIDebugger.DoWatchFreed(Sender: TObject);
begin
  FWatchEvalList.Remove(pointer(Sender));
end;

function TFpGDBMIDebugger.EvaluateExpression(AWatchValue: TWatchValueBase;
  AExpression: String; var AResText: String; out ATypeInfo: TDBGType;
  EvalFlags: TDBGEvaluateFlags): Boolean;
var
  Ctx: TDbgInfoAddressContext;
  PasExpr: TFpPascalExpression;
  ResValue: TDbgSymbolValue;

  function IsWatchValueAlive: Boolean;
  begin
    Result := (State in [dsPause, dsInternalPause]) and
              (  (AWatchValue = nil) or
                 ( (FWatchEvalList.Count > 0) and (FWatchEvalList[0] = Pointer(AWatchValue)) )
              );
  end;

  function ResTypeName(v: TDbgSymbolValue = nil): String;
  begin
    if v = nil then v := ResValue;
    if not((v.TypeInfo<> nil) and
           GetTypeName(Result, v.TypeInfo, []))
    then
      Result := '';
  end;

  procedure DoPointer;
  begin
    if not PrintPasValue(AResText, ResValue, ctx.SizeOfAddress, []) then
      exit;
    ATypeInfo := TDBGType.Create(skPointer, ResTypeName);
    ATypeInfo.Value.AsPointer := Pointer(ResValue.AsCardinal); // TODO: no cut off
    ATypeInfo.Value.AsString := AResText;
  end;

  procedure DoSimple;
  begin
    if not PrintPasValue(AResText, ResValue, ctx.SizeOfAddress, []) then
      exit;
    ATypeInfo := TDBGType.Create(skSimple, ResTypeName);
    ATypeInfo.Value.AsString := AResText;
  end;

  procedure DoEnum;
  begin
    if not PrintPasValue(AResText, ResValue, ctx.SizeOfAddress, []) then
      exit;
    ATypeInfo := TDBGType.Create(skEnum, ResTypeName);
    ATypeInfo.Value.AsString := AResText;
  end;

  procedure DoSet;
  begin
    if not PrintPasValue(AResText, ResValue, ctx.SizeOfAddress, []) then
      exit;
    ATypeInfo := TDBGType.Create(skSet, ResTypeName);
    ATypeInfo.Value.AsString := AResText;
  end;

  procedure DoRecord;
  var
    s2, n: String;
    m: TDbgSymbolValue;
    i: Integer;
    DBGType: TGDBType;
    f: TDBGField;
  begin
    if not PrintPasValue(AResText, ResValue, ctx.SizeOfAddress, []) then
      exit;
    ATypeInfo := TDBGType.Create(skRecord, ResTypeName);
    ATypeInfo.Value.AsString := AResText;

    if not(defFullTypeInfo in EvalFlags) then exit;
    for i := 0 to ResValue.MemberCount - 1 do begin
      m := ResValue.Member[i];
      if m = nil then Continue; // Todo: procedures.
      case m.Kind of
        skProcedure, skFunction: ; //  DBGType := TGDBType.Create(skProcedure, TGDBTypes.CreateFromCSV(Params))
        else
          begin
            DBGType := TGDBType.Create(skSimple, ResTypeName(m));
            PrintPasValue(s2, m, ctx.SizeOfAddress, []);
            DBGType.Value.AsString := s2;
            n := '';
            if m.DbgSymbol <> nil then n := m.DbgSymbol.Name;
            f := TDBGField.Create(n, DBGType, flPublic);
            ATypeInfo.Fields.Add(f);
          end;
      end;
    end;
  end;

  procedure DoClass;
  var
    m: TDbgSymbolValue;
    s, s2, n, CastName: String;
    DBGType: TGDBType;
    f: TDBGField;
    i: Integer;
    ClassAddr, CNameAddr: TFpDbgMemLocation;
    NameLen: QWord;
    PasExpr2: TFpPascalExpression;
  begin
    if (ResValue.Kind = skClass) and (ResValue.AsCardinal = 0) then begin
      if not PrintPasValue(AResText, ResValue, ctx.SizeOfAddress, []) then
        exit;
      ATypeInfo := TDBGType.Create(skSimple, ResTypeName);
      ATypeInfo.Value.AsString := AResText;
      Result := True;
      exit;
    end;

    CastName := '';
    if (defClassAutoCast in EvalFlags) then begin
      if FMemManager.ReadAddress(ResValue.DataAddress, Ctx.SizeOfAddress, ClassAddr) then begin
        ClassAddr.Address := ClassAddr.Address + 3 * Ctx.SizeOfAddress;
        if FMemManager.ReadAddress(ClassAddr, Ctx.SizeOfAddress, CNameAddr) then begin
          if (FMemManager.ReadUnsignedInt(CNameAddr, 1, NameLen)) then
            if NameLen > 0 then begin
              SetLength(CastName, NameLen);
              CNameAddr.Address := CNameAddr.Address + 1;
              FMemManager.ReadMemory(CNameAddr, NameLen, @CastName[1]);
              PasExpr2 := TFpPascalExpression.Create(CastName+'('+AExpression+')', Ctx);
              if PasExpr2.Valid and (PasExpr2.ResultValue <> nil) then begin
                PasExpr.Free;
                PasExpr := PasExpr2;
                ResValue := PasExpr.ResultValue;
              end
              else
                PasExpr2.Free;
            end;
        end;
      end;
    end;


    if not PrintPasValue(AResText, ResValue, ctx.SizeOfAddress, []) then
      exit;
    if CastName <> '' then AResText := CastName + AResText;
    //if PasExpr.ResultValue.Kind = skObject then
    //  ATypeInfo := TDBGType.Create(skObject, ResTypeName)
    //else
      ATypeInfo := TDBGType.Create(skClass, ResTypeName);
    ATypeInfo.Value.AsString := AResText;

    if not(defFullTypeInfo in EvalFlags) then exit;
    for i := 0 to ResValue.MemberCount - 1 do begin
      m := ResValue.Member[i];
      if m = nil then Continue; // Todo: procedures.
      case m.Kind of
        skProcedure, skFunction: ; //  DBGType := TGDBType.Create(skProcedure, TGDBTypes.CreateFromCSV(Params))
        else
          begin
            DBGType := TGDBType.Create(skSimple, ResTypeName(m));
            PrintPasValue(s2, m, ctx.SizeOfAddress, []);
            DBGType.Value.AsString := s2;
            n := '';
            if m.DbgSymbol <> nil then n := m.DbgSymbol.Name;
            s := '';
            if m.ContextTypeInfo <> nil then s := m.ContextTypeInfo.Name;
// TODO visibility // flags virtual, constructor
            f := TDBGField.Create(n, DBGType, flPublic, [], s);
            ATypeInfo.Fields.Add(f);
          end;
      end;
    end;
  end;

  procedure DoArray;
  begin
    if not PrintPasValue(AResText, ResValue, ctx.SizeOfAddress, []) then
      exit;
    ATypeInfo := TDBGType.Create(skArray, ResTypeName);
    ATypeInfo.Value.AsString := AResText;
    //ATypeInfo.Len;
    //ATypeInfo.BoundLow;
    //ATypeInfo.BoundHigh;
  end;

begin
  Result := False;
  ATypeInfo := nil;
  if AWatchValue <> nil then begin
    EvalFlags := AWatchValue.EvaluateFlags;
    AExpression := AWatchValue.Expression;
  end;
  if AWatchValue <> nil then
    Ctx := GetInfoContextForContext(AWatchValue.ThreadId, AWatchValue.StackFrame)
  else
    Ctx := GetInfoContextForContext(CurrentThreadId, CurrentStackFrame);
  if Ctx = nil then exit;

  PasExpr := TFpPascalExpression.Create(AExpression, Ctx);
  try
    if not IsWatchValueAlive then exit;

    if not (PasExpr.Valid and (PasExpr.ResultValue <> nil)) then
      exit; // TODO handle error
    if not IsWatchValueAlive then exit;

    ResValue := PasExpr.ResultValue;

    case PasExpr.ResultValue.Kind of
      skUnit: ;
      skProcedure: ;
      skFunction: ;
      skPointer:  DoPointer;
      skInteger:  DoSimple;
      skCardinal: DoSimple;
      skBoolean:  DoSimple;
      skChar:     DoSimple;
      skFloat:    DoSimple;
      skString: ;
      skAnsiString: ;
      skCurrency: ;
      skVariant: ;
      skWideString: ;
      skEnum:      DoEnum;
      skEnumValue: DoSimple;
      skSet:       DoSet;
      skRecord:    DoRecord;
      skObject:    DoClass;
      skClass:     DoClass;
      skInterface: ;
      skArray:     DoArray;
    end;
    if not IsWatchValueAlive then exit;

    if ATypeInfo <> nil then begin
      Result := True;
      debugln(['TFPGDBMIWatches.InternalRequestData   GOOOOOOD']);
      if AWatchValue <> nil then begin;
        AWatchValue.Value    := AResText;
        AWatchValue.TypeInfo := ATypeInfo;
        AWatchValue.Validity := ddsValid;
      end;
    end;

  finally
    PasExpr.Free;
  end;
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

constructor TFpGDBMIDebugger.Create(const AExternalDebugger: String);
begin
  FWatchEvalList := TList.Create;
  inherited Create(AExternalDebugger);
end;

destructor TFpGDBMIDebugger.Destroy;
begin
  UnLoadDwarf;
  FWatchEvalList.Free;
  inherited Destroy;
end;

procedure Register;
begin
  RegisterDebugger(TFpGDBMIDebugger);
end;

end.

