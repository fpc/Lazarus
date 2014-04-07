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
  selfname: string = 'self';
  // TODO: get reg num via memreader name-to-num
  {$IFDEF cpu64}
  RegFp = 6;
  RegPc = 16;
  {$ELSE}
  RegFp = 5;
  RegPc = 8;
  {$ENDIF}
var
  StartScopeIdx: Integer;
  ParentFpVal: TFpDbgValue;
  SearchCtx: TFpDwarfFreePascalAddressContext;
  pfp, fp, pc: TDbgPtr;
  i: Integer;
  ParentFpSym: TFpDwarfSymbol;
begin
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
    Result := True; // self, global was done by outer
    exit;
  end;


  InfoEntry.ScopeIndex := StartScopeIdx;
  // TODO: cache
  if not InfoEntry.GoNamedChildEx(@parentfp[1], @parentfp[1]) then begin
    FOuterNotFound := True;
    exit;
  end;

  ParentFpSym := TFpDwarfSymbol.CreateSubClass(AName, InfoEntry);
  ParentFpVal := ParentFpSym.Value;
  //TFpDwarfSymbol(ADbgValue.DbgSymbol).ParentTypeInfo := TFpDwarfSymbolValueProc(FSymbol);
  if not (svfOrdinal in ParentFpVal.FieldFlags) then begin
    DebugLn('no ordinal for parentfp');
    ParentFpSym.ReleaseReference;
    FOuterNotFound := True;
    exit;
  end;

  pfp := ParentFpVal.AsCardinal;
  ParentFpSym.ReleaseReference;
    DebugLn(['pfp=',pfp]);
  if pfp = 0 then begin
    DebugLn('no ordinal for parentfp');
    FOuterNotFound := True;
    exit;
  end;

  i := StackFrame + 1;
  SearchCtx := TFpDwarfFreePascalAddressContext.Create(ThreadId, i, 0, Symbol, Dwarf);

  fp := 0;
  while not (fp = pfp) do begin
    SearchCtx.StackFrame := i;
    // TODO: get reg num via memreader name-to-num
    if not MemManager.ReadRegister(RegFp, fp, SearchCtx) then
      break;
    inc(i);
    if i > StackFrame + 100 then break; // something wrong? // TODO better check
  end;
  dec(i);

  if (pfp <> fp) or
      not MemManager.ReadRegister(RegPc, pc, SearchCtx)
  then begin
    FOuterNotFound := True;
    SearchCtx.ReleaseReference;
    exit;
  end;

  SearchCtx.ReleaseReference;

  FOuterNestContext := Dwarf.FindContext(ThreadId, i, pc);

  ADbgValue := FOuterNestContext.FindSymbol(AName); // TODO: pass upper/lower
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

