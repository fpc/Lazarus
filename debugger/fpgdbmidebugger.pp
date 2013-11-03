unit FpGdbmiDebugger;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, math, FpDbgClasses, GDBMIDebugger, BaseDebugManager, Debugger,
  GDBMIMiscClasses, GDBTypeInfo, maps, LCLProc, FpDbgLoader, FpDbgDwarf, FpDbgDwarfConst,
  LazLoggerBase, LazLoggerProfiling, FpPascalParser, FpPascalBuilder;

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

  procedure AddType(ASourceExpr: string; ATypeIdent: TDbgSymbol); forward;

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

    while (ABaseType.Kind = FpDbgClasses.skPointer) and (ABaseType.TypeInfo <> nil) do begin
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
      if AMember.Kind = FpDbgClasses.skProcedure then begin
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
        if (AMember.Kind = FpDbgClasses.skSet) or (AMember.Kind = FpDbgClasses.skEnum) or
           (AMember.Kind = FpDbgClasses.skArray)
        then
        if not GetTypeAsDeclaration(s, ti, [tdfSkipClassBody, tdfSkipRecordBody]) then
          s := '';
      end;
      if (s = '') and not (AMember.Kind = FpDbgClasses.skRecord)  then begin
        Result := False;
        exit;
      end;

      if AMember.Kind = FpDbgClasses.skFunction then begin
        if sfVirtual in AMember.Flags then s2 := ' virtual;';
        AText := AText + '    function  ' + AMember.Name + ' () : '+s+';' + s2 + LineEnding;
      end
      else
      if AMember.Kind = FpDbgClasses.skRecord then begin
        AText := AText + '    ' + AMember.Name + ' : '+s+' = record ;' + LineEnding;
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
      AReq.Result := ParseTypeFromGdb(AAnswer);
      Add(AThreadId, AStackFrame, AReq);
      debugln(['**** AddToGDBMICache ', AReq.Request, ' T:', AThreadId, ' S:',AStackFrame, ' >>>> ', AAnswer, ' <<<<']);
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
    if (ABaseType.TypeInfo = nil) then
      exit;
    if APointerLevel = 0 then
      ADeRefTypeName := ASrcTypeName;
    ParentName :=  ABaseType.TypeInfo.Name;
    if not MembersAsGdbText(ABaseType, True, s2) then
      exit;

    s := Format('type = ^%s = class : public %s %s%send%s', [ABaseTypeName, ParentName, LineEnding, s2, LineEnding]);
    MaybeAdd(gcrtPType, GdbCmdPType  + ASourceExpr, s);

    s := Format('type = %s%s', [ASrcTypeName, LineEnding]);
    MaybeAdd(gcrtPType, GdbCmdWhatIs  + ASourceExpr, s);


    ASourceExpr := GDBMIMaybeApplyBracketsToExpr(ASourceExpr)+'^';
    if APointerLevel > 0
    then RefToken := '^'
    else RefToken := '';
    s := Format('type = %s%s = class : public %s %s%send%s', [RefToken, ABaseTypeName, ParentName, LineEnding, s2, LineEnding]);
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
    s, s2: String;
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

  procedure AddType(ASourceExpr: string; ATypeIdent: TDbgSymbol);
  var
    SrcTypeName,     // The expressions own type name
    DeRefTypeName,   // one levvel of pointer followed
    BaseTypeName: String; // all poiters followed
    DeRefType, BaseType: TDbgSymbol;
    PointerLevel: Integer;
  begin
    if (ASourceExpr = '') or (ATypeIdent = nil) then exit;

    FindPointerAndBaseType(ATypeIdent, PointerLevel,
                           DeRefType, BaseType,
                           SrcTypeName, DeRefTypeName, BaseTypeName);

    case BaseType.Kind of
      skInteger, skCardinal, skBoolean, skChar, skFloat:
        AddBaseType(ASourceExpr, PointerLevel,
                    SrcTypeName, DeRefTypeName, BaseTypeName,
                    ATypeIdent, BaseType);
      FpDbgClasses.skClass:
        AddClassType(ASourceExpr, PointerLevel,
                    SrcTypeName, DeRefTypeName, BaseTypeName,
                    ATypeIdent, BaseType);
      FpDbgClasses.skRecord:
        AddRecordType(ASourceExpr, PointerLevel,
                    SrcTypeName, DeRefTypeName, BaseTypeName,
                    ATypeIdent, BaseType);
      FpDbgClasses.skEnum:
        AddEnumType(ASourceExpr, PointerLevel,
                    SrcTypeName, DeRefTypeName, BaseTypeName,
                    ATypeIdent, BaseType);
      FpDbgClasses.skSet:
        AddSetType(ASourceExpr, PointerLevel,
                    SrcTypeName, DeRefTypeName, BaseTypeName,
                    ATypeIdent, BaseType);
      FpDbgClasses.skArray:
        AddArrayType(ASourceExpr, PointerLevel,
                    SrcTypeName, DeRefTypeName, BaseTypeName,
                    ATypeIdent, BaseType);
    end;

  end;

var
  IdentName: String;
  PasExpr: TFpGDBMIPascalExpression;
begin
  Result := inherited IndexOf(AThreadId, AStackFrame, ARequest);
DebugLn(['######## '+ARequest.Request, ' ## FOUND: ', dbgs(Result)]);

  if (Result >= 0) or FInIndexOf then
    exit;

  FInIndexOf := True;
  PasExpr := nil;
  try
    if ARequest.ReqType = gcrtPType then begin
//DebugLn('######## '+ARequest.Request);
      if copy(ARequest.Request, 1, 6) = 'ptype ' then
	       IdentName := trim(copy(ARequest.Request, 7, length(ARequest.Request)))
      else
      if copy(ARequest.Request, 1, 7) = 'whatis ' then
        IdentName := trim(copy(ARequest.Request, 8, length(ARequest.Request)));

      PasExpr := TFpGDBMIPascalExpression.Create(IdentName, FDebugger, AThreadId, AStackFrame);

      AddType(IdentName, PasExpr.ResultType);
      Result := inherited IndexOf(AThreadId, AStackFrame, ARequest);
    end;

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

