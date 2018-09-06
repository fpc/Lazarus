unit FpDbgDwarfFreePascal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpDbgDwarfDataClasses, FpDbgDwarf, FpDbgInfo, FpDbgUtil, DbgIntfBaseTypes,
  LazLoggerBase;

type

  { TFpDwarfFreePascalSymbolClassMap }

  TFpDwarfFreePascalSymbolClassMap = class(TFpDwarfDefaultSymbolClassMap)
  public
    class function HandleCompUnit(ACU: TDwarfCompilationUnit): Boolean; override;
    //class function GetDwarfSymbolClass(ATag: Cardinal): TDbgDwarfSymbolBaseClass; override;
    class function CreateContext(AThreadId, AStackFrame: Integer; AnAddress: TDBGPtr; ASymbol: TFpDbgSymbol;
      ADwarf: TFpDwarfInfo): TFpDbgInfoContext; override;
    //class function CreateProcSymbol(ACompilationUnit: TDwarfCompilationUnit;
    //  AInfo: PDwarfAddressInfo; AAddress: TDbgPtr): TDbgDwarfSymbolBase; override;
  end;

  { TFpDwarfFreePascalAddressContext }

  TFpDwarfFreePascalAddressContext = class(TFpDwarfInfoAddressContext)
  private
    FOuterNestContext: TFpDbgInfoContext;
    FOuterNotFound: Boolean;
  protected
    function FindLocalSymbol(const AName: String; PNameUpper, PNameLower: PChar;
      InfoEntry: TDwarfInformationEntry; out ADbgValue: TFpDbgValue): Boolean; override;
  public
    destructor Destroy; override;
  end;

implementation

{ TFpDwarfFreePascalSymbolClassMap }

class function TFpDwarfFreePascalSymbolClassMap.HandleCompUnit(ACU: TDwarfCompilationUnit): Boolean;
var
  s: String;
begin
  s := LowerCase(ACU.Producer);
  Result := pos('free pascal', s) > 0;
end;

class function TFpDwarfFreePascalSymbolClassMap.CreateContext(AThreadId, AStackFrame: Integer;
  AnAddress: TDBGPtr; ASymbol: TFpDbgSymbol; ADwarf: TFpDwarfInfo): TFpDbgInfoContext;
begin
  Result := TFpDwarfFreePascalAddressContext.Create(AThreadId, AStackFrame, AnAddress, ASymbol, ADwarf);
end;

{ TFpDwarfFreePascalAddressContext }

function TFpDwarfFreePascalAddressContext.FindLocalSymbol(const AName: String; PNameUpper,
  PNameLower: PChar; InfoEntry: TDwarfInformationEntry; out ADbgValue: TFpDbgValue): Boolean;
const
  parentfp: string = 'parentfp';
  parentfp2: string = '$parentfp';
  selfname: string = 'self';
  // TODO: get reg num via memreader name-to-num
  RegFp64 = 6;
  RegPc64 = 16;
  RegFp32 = 5;
  RegPc32 = 8;
var
  StartScopeIdx, RegFp, RegPc: Integer;
  ParentFpVal: TFpDbgValue;
  SearchCtx: TFpDwarfFreePascalAddressContext;
  par_fp, cur_fp, prev_fp, pc: TDbgPtr;
  d, i: Integer;
  ParentFpSym: TFpDwarfSymbol;
begin
  if Dwarf.Image64Bit then begin
    RegFP := RegFp64;
    RegPc := RegPc64;
  end
  else begin
    RegFP := RegFp32;
    RegPc := RegPc32;
  end;
  Result := False;
  if (Length(AName) = length(selfname)) and (CompareUtf8BothCase(PNameUpper, PNameLower, @selfname[1])) then begin
    ADbgValue := GetSelfParameter;
    if ADbgValue <> nil then begin
      AddRefToVal(ADbgValue);
      Result := True;
      exit;
    end;
  end;

  StartScopeIdx := InfoEntry.ScopeIndex;
  Result := inherited FindLocalSymbol(AName, PNameUpper, PNameLower, InfoEntry, ADbgValue);
  if Result then
    exit;

  if FOuterNotFound then
    exit;

  if FOuterNestContext <> nil then begin
    ADbgValue := FOuterNestContext.FindSymbol(AName); // TODO: pass upper/lower
    if ADbgValue <> nil then
      AddRefToVal(ADbgValue);
    Result := True; // self, global was done by outer
    exit;
  end;


  InfoEntry.ScopeIndex := StartScopeIdx;
  if not InfoEntry.GoNamedChildEx(@parentfp[1], @parentfp[1]) then begin
    InfoEntry.ScopeIndex := StartScopeIdx;
    if not InfoEntry.GoNamedChildEx(@parentfp2[1], @parentfp2[1]) then begin
      FOuterNotFound := True;
      exit;
    end;
  end;

  ParentFpSym := TFpDwarfSymbol.CreateSubClass(AName, InfoEntry);
  ParentFpVal := ParentFpSym.Value;
  ApplyContext(ParentFpVal);
  //TFpDwarfSymbol(ADbgValue.DbgSymbol).ParentTypeInfo := TFpDwarfSymbolValueProc(FSymbol);
  if not (svfOrdinal in ParentFpVal.FieldFlags) then begin
    DebugLn('no ordinal for parentfp');
    ParentFpSym.ReleaseReference;
    FOuterNotFound := True;
    exit;
  end;

  par_fp := ParentFpVal.AsCardinal;
  ParentFpSym.ReleaseReference;
    DebugLn(['par_fp=',par_fp]);
  if par_fp = 0 then begin
    DebugLn('no ordinal for parentfp');
    FOuterNotFound := True;
    exit;
  end;

  i := StackFrame + 1;
  SearchCtx := TFpDwarfFreePascalAddressContext.Create(ThreadId, i, 0, Symbol, Dwarf);

  cur_fp := 0;
  if MemManager.ReadRegister(RegFp, cur_fp, Self) then begin
    if cur_fp > par_fp then
      d := -1  // cur_fp must go down
    else
      d := 1;  // cur_fp must go up
    while not (cur_fp = par_fp) do begin
      SearchCtx.StackFrame := i;
      // TODO: get reg num via memreader name-to-num
      prev_fp := cur_fp;
      if not MemManager.ReadRegister(RegFp, cur_fp, SearchCtx) then
        break;
      inc(i);
      if (cur_fp = prev_fp) or ((cur_fp < prev_fp) xor (d = -1)) then
        break;  // wrong direction
      if i > StackFrame + 200 then break; // something wrong? // TODO better check
    end;
    dec(i);
  end;

  if (par_fp <> cur_fp) or (cur_fp = 0) or
      not MemManager.ReadRegister(RegPc, pc, SearchCtx)
  then begin
    FOuterNotFound := True;
    SearchCtx.ReleaseReference;
    exit;
  end;

  SearchCtx.ReleaseReference;

  FOuterNestContext := Dwarf.FindContext(ThreadId, i, pc);

  ADbgValue := FOuterNestContext.FindSymbol(AName); // TODO: pass upper/lower
  if ADbgValue <> nil then
    AddRefToVal(ADbgValue);
  Result := True; // self, global was done by outer
end;

destructor TFpDwarfFreePascalAddressContext.Destroy;
begin
  FOuterNestContext.ReleaseReference;
  inherited Destroy;
end;

initialization
  DwarfSymbolClassMapList.AddMap(TFpDwarfFreePascalSymbolClassMap);

end.

