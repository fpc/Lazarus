unit LazLoggerBase;
{$mode objfpc}{$H+}

(*
  - All globas variables, initialization and finalization uses TObject instead
    of TLazLogger.
    This means, usinc the unit, without calling any of the functions, will not
    make any reference to the classes, and they should be smart-linked away.
*)

interface

uses
  Classes, SysUtils, FileUtil, types, math, LazClasses;

type

  TLazLoggerLogGroupFlag =
  ( lgfAddedByParamParser,        // Not added via Register. This is a placeholder for the enabled-state given by the user, via cmd-line
    lgfNoDefaultEnabledSpecified  // Registered without default

  );
  TLazLoggerLogGroupFlags = set of TLazLoggerLogGroupFlag;

  TLazLoggerLogGroup = record
    ConfigName: String;  // case insensitive
    Enabled: Boolean;
    Flags: TLazLoggerLogGroupFlags;
    FOpenedIndents: Integer;
  end;
  PLazLoggerLogGroup = ^TLazLoggerLogGroup;

  TLazLoggerWriteEvent = procedure (Sender: TObject; S: string; var Handled: Boolean) of object;

type

  { TLazLoggerLogGroupList }

  TLazLoggerLogGroupList = class(TRefCountedObject)
  private
    FList: TFPList;
    procedure Clear;
    function GetItem(Index: Integer): PLazLoggerLogGroup;
    function  NewItem(const AConfigName: String; ADefaulEnabled: Boolean = False) : PLazLoggerLogGroup;
  protected
    function  Add(const AConfigName: String; ADefaulEnabled: Boolean = False) : PLazLoggerLogGroup;
    function  FindOrAdd(const AConfigName: String; ADefaulEnabled: Boolean = False) : PLazLoggerLogGroup;
    procedure Remove(const AConfigName: String);
    procedure Remove(const AnEntry: PLazLoggerLogGroup);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Assign(Src: TLazLoggerLogGroupList);
    function  IndexOf(const AConfigName: String): integer;
    function  IndexOf(const AnEntry: PLazLoggerLogGroup): integer;
    function  Find(const AConfigName: String): PLazLoggerLogGroup;
    function  Count: integer;
    property  Item[Index: Integer]: PLazLoggerLogGroup read GetItem; default;
  end;

  { TLazLogger }

  TLazLogger = class(TRefCountedObject)
  private
    FIsInitialized: Boolean;

    FMaxNestPrefixLen: Integer;
    FNestLvlIndent: Integer;

    FLogGroupList: TRefCountedObject; // Using TObject, so if none of the functions is used in the app, then even the rlass should be smart linked
    FUseGlobalLogGroupList: Boolean;

    procedure SetMaxNestPrefixLen(AValue: Integer);
    procedure SetNestLvlIndent(AValue: Integer);

    function  GetLogGroupList: TLazLoggerLogGroupList;
    procedure SetUseGlobalLogGroupList(AValue: Boolean);
  protected
    procedure DoInit; virtual;
    procedure DoFinsh; virtual;

    procedure IncreaseIndent; overload; virtual;
    procedure DecreaseIndent; overload; virtual;
    procedure IncreaseIndent(LogGroup: PLazLoggerLogGroup); overload; virtual;
    procedure DecreaseIndent(LogGroup: PLazLoggerLogGroup); overload; virtual;
    procedure IndentChanged; virtual;

    procedure DoDbgOut(const s: string); virtual;
    procedure DoDebugLn(const s: string); virtual;
    procedure DoDebuglnStack(const s: string); virtual;

    function  ArgsToString(Args: array of const): string;
    property  IsInitialized: Boolean read FIsInitialized;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Assign(Src: TLazLogger); virtual;
    procedure Init;
    procedure Finish;

    property  NestLvlIndent: Integer read FNestLvlIndent write SetNestLvlIndent;
    property  MaxNestPrefixLen: Integer read FMaxNestPrefixLen write SetMaxNestPrefixLen;

  public
    function  RegisterLogGroup(const AConfigName: String; ADefaulEnabled: Boolean) : PLazLoggerLogGroup; virtual;
    function  RegisterLogGroup(const AConfigName: String) : PLazLoggerLogGroup; virtual;
    function  FindOrRegisterLogGroup(const AConfigName: String; ADefaulEnabled: Boolean) : PLazLoggerLogGroup; virtual;
    function  FindOrRegisterLogGroup(const AConfigName: String) : PLazLoggerLogGroup; virtual;
    property  LogGroupList: TLazLoggerLogGroupList read GetLogGroupList;
    property  UseGlobalLogGroupList: Boolean read FUseGlobalLogGroupList write SetUseGlobalLogGroupList;
  public
    procedure DebuglnStack(const s: string = '');

    procedure DbgOut(const s: string = ''); overload;
    procedure DbgOut(Args: array of const); overload;
    procedure DbgOut(const S: String; Args: array of const); overload;// similar to Format(s,Args)
    procedure DbgOut(const s1, s2: string; const s3: string = '';
                     const s4: string = ''; const s5: string = ''; const s6: string = '';
                     const s7: string = ''; const s8: string = ''; const s9: string = '';
                     const s10: string = ''; const s11: string = ''; const s12: string = '';
                     const s13: string = ''; const s14: string = ''; const s15: string = '';
                     const s16: string = ''; const s17: string = ''; const s18: string = ''); overload;

    procedure DebugLn(const s: string = ''); overload;
    procedure DebugLn(Args: array of const); overload;
    procedure DebugLn(const S: String; Args: array of const); overload;// similar to Format(s,Args)
    procedure DebugLn(const s1, s2: string; const s3: string = '';
                      const s4: string = ''; const s5: string = ''; const s6: string = '';
                      const s7: string = ''; const s8: string = ''; const s9: string = '';
                      const s10: string = ''; const s11: string = ''; const s12: string = '';
                      const s13: string = ''; const s14: string = ''; const s15: string = '';
                      const s16: string = ''; const s17: string = ''; const s18: string = ''); overload;

    procedure DebugLnEnter(const s: string = ''); overload;
    procedure DebugLnEnter(Args: array of const); overload;
    procedure DebugLnEnter(s: string; Args: array of const); overload;
    procedure DebugLnEnter(const s1, s2: string; const s3: string = '';
                           const s4: string = ''; const s5: string = ''; const s6: string = '';
                           const s7: string = ''; const s8: string = ''; const s9: string = '';
                           const s10: string = ''; const s11: string = ''; const s12: string = '';
                           const s13: string = ''; const s14: string = ''; const s15: string = '';
                           const s16: string = ''; const s17: string = ''; const s18: string = ''); overload;

    procedure DebugLnExit(const s: string = ''); overload;
    procedure DebugLnExit(Args: array of const); overload;
    procedure DebugLnExit(s: string; Args: array of const); overload;
    procedure DebugLnExit(const s1, s2: string; const s3: string = '';
                          const s4: string = ''; const s5: string = ''; const s6: string = '';
                          const s7: string = ''; const s8: string = ''; const s9: string = '';
                          const s10: string = ''; const s11: string = ''; const s12: string = '';
                          const s13: string = ''; const s14: string = ''; const s15: string = '';
                          const s16: string = ''; const s17: string = ''; const s18: string = ''); overload;


    procedure DebuglnStack(LogGroup: PLazLoggerLogGroup; const s: string = '');

    procedure DbgOut(LogGroup: PLazLoggerLogGroup; const s: string = ''); overload;
    procedure DbgOut(LogGroup: PLazLoggerLogGroup; Args: array of const); overload;
    procedure DbgOut(LogGroup: PLazLoggerLogGroup; const S: String; Args: array of const); overload;// similar to Format(s,Args)
    procedure DbgOut(LogGroup: PLazLoggerLogGroup; const s1, s2: string; const s3: string = '';
                     const s4: string = ''; const s5: string = ''; const s6: string = '';
                     const s7: string = ''; const s8: string = ''; const s9: string = '';
                     const s10: string = ''; const s11: string = ''; const s12: string = '';
                     const s13: string = ''; const s14: string = ''; const s15: string = '';
                     const s16: string = ''; const s17: string = ''; const s18: string = ''); overload;

    procedure DebugLn(LogGroup: PLazLoggerLogGroup; const s: string = ''); overload;
    procedure DebugLn(LogGroup: PLazLoggerLogGroup; Args: array of const); overload;
    procedure DebugLn(LogGroup: PLazLoggerLogGroup; const S: String; Args: array of const); overload;// similar to Format(s,Args)
    procedure DebugLn(LogGroup: PLazLoggerLogGroup; const s1, s2: string; const s3: string = '';
                      const s4: string = ''; const s5: string = ''; const s6: string = '';
                      const s7: string = ''; const s8: string = ''; const s9: string = '';
                      const s10: string = ''; const s11: string = ''; const s12: string = '';
                      const s13: string = ''; const s14: string = ''; const s15: string = '';
                      const s16: string = ''; const s17: string = ''; const s18: string = ''); overload;

    procedure DebugLnEnter(LogGroup: PLazLoggerLogGroup; const s: string = ''); overload;
    procedure DebugLnEnter(LogGroup: PLazLoggerLogGroup; Args: array of const); overload;
    procedure DebugLnEnter(LogGroup: PLazLoggerLogGroup; s: string; Args: array of const); overload;
    procedure DebugLnEnter(LogGroup: PLazLoggerLogGroup; const s1, s2: string; const s3: string = '';
                           const s4: string = ''; const s5: string = ''; const s6: string = '';
                           const s7: string = ''; const s8: string = ''; const s9: string = '';
                           const s10: string = ''; const s11: string = ''; const s12: string = '';
                           const s13: string = ''; const s14: string = ''; const s15: string = '';
                           const s16: string = ''; const s17: string = ''; const s18: string = ''); overload;

    procedure DebugLnExit(LogGroup: PLazLoggerLogGroup; const s: string = ''); overload;
    procedure DebugLnExit(LogGroup: PLazLoggerLogGroup; Args: array of const); overload;
    procedure DebugLnExit(LogGroup: PLazLoggerLogGroup; s: string; Args: array of const); overload;
    procedure DebugLnExit(LogGroup: PLazLoggerLogGroup; const s1, s2: string; const s3: string = '';
                          const s4: string = ''; const s5: string = ''; const s6: string = '';
                          const s7: string = ''; const s8: string = ''; const s9: string = '';
                          const s10: string = ''; const s11: string = ''; const s12: string = '';
                          const s13: string = ''; const s14: string = ''; const s15: string = '';
                          const s16: string = ''; const s17: string = ''; const s18: string = ''); overload;

  end;

  { TLazLoggerWithGroupParam
    - Provides Enabling/disabling groups from commandline
    - TLazLogger provides only storage for LogGroups, it does not need to
      enable/disable them, as it discards all logging anyway
  }

  TLazLoggerWithGroupParam = class(TLazLogger)
  private
    FLogAllDefaultDisabled: Boolean;
    FLogDefaultEnabled: Boolean;
    FLogParamParsed: Boolean;
    FParamForEnabledLogGroups: String;
    procedure SetParamForEnabledLogGroups(AValue: String);
    procedure ParseParamForEnabledLogGroups;
  public
    constructor Create;
    procedure Assign(Src: TLazLogger); override;
    function RegisterLogGroup(const AConfigName: String): PLazLoggerLogGroup; override;
    function RegisterLogGroup(const AConfigName: String; ADefaulEnabled: Boolean): PLazLoggerLogGroup; override;
    function FindOrRegisterLogGroup(const AConfigName: String): PLazLoggerLogGroup; override;
    function FindOrRegisterLogGroup(const AConfigName: String; ADefaulEnabled: Boolean): PLazLoggerLogGroup; override;
    // A param on the commandline, that may contain enabled/disabled LogGroups
    // comma separated list / not present = defaults (none unless emabled in code) / - means none
    property  ParamForEnabledLogGroups: String read FParamForEnabledLogGroups write SetParamForEnabledLogGroups;
  end;

  TLazLoggerNoOutput = class(TLazLogger)
  end;


{$DEFINE USED_BY_LAZLOGGER_BASE}
{$I LazLoggerIntf.inc}

function ConvertLineEndings(const s: string): string;

function GetParamByNameCount(const AName: String): integer;
function GetParamByName(const AName: String; AnIndex: Integer): string;

function GetDebugLoggerGroups: TLazLoggerLogGroupList; inline;
procedure SetDebugLoggerGroups(ALogGroups: TLazLoggerLogGroupList);

function GetDebugLogger: TLazLogger; inline;
function GetExistingDebugLogger: TLazLogger; inline; // No Autocreate
procedure SetDebugLogger(ALogger: TLazLogger);

procedure RecreateDebugLogger;

property DebugLogger: TLazLogger read GetDebugLogger write SetDebugLogger;
property DebugLoggerGroups: TLazLoggerLogGroupList read GetDebugLoggerGroups write SetDebugLoggerGroups;

type
  TLazDebugLoggerCreator = function: TRefCountedObject;

// Using base TRefCountedObject, so if none of the functions is used in the app, then even the class should be smart linked
var
  LazDebugLoggerCreator: TLazDebugLoggerCreator = nil;
  OnWidgetSetDebugLn: TLazLoggerWriteEvent;
  OnWidgetSetDbgOut:  TLazLoggerWriteEvent;

implementation

{$I LazLoggerImpl.inc}

var // Using base TRefCountedObject, so if none of the functions is used in the app, then even the class should be smart linked
  TheLazLogger: TRefCountedObject = nil;
  PrevLazLogger: TRefCountedObject = nil;
  TheLazLoggerGroups: TRefCountedObject = nil;

procedure CreateDebugLogger;
begin
  if (TheLazLogger <> nil) then
    exit;
  if (LazDebugLoggerCreator <> nil) then
    TheLazLogger := LazDebugLoggerCreator();
  if (TheLazLogger = nil) then
    TheLazLogger := TLazLoggerNoOutput.Create;
  TLazLogger(TheLazLogger).UseGlobalLogGroupList := True;
  TheLazLogger.AddReference;
end;

function GetDebugLogger: TLazLogger;
begin
  if (TheLazLogger = nil) then
    CreateDebugLogger;
  Result := TLazLogger(TheLazLogger);
end;

function GetExistingDebugLogger: TLazLogger;
begin
  if TheLazLogger <> nil then
    Result := TLazLogger(TheLazLogger)
  else
    Result := TLazLogger(PrevLazLogger);  // Pretend it still exists
end;

procedure SetDebugLogger(ALogger: TLazLogger);
begin
  ReleaseRefAndNil(TheLazLogger);
  TheLazLogger := ALogger;
  TheLazLogger.AddReference;
end;

procedure RecreateDebugLogger;
begin
  ReleaseRefAndNil(PrevLazLogger);
  PrevLazLogger := TheLazLogger; // Pretend it still exists
  TheLazLogger := nil;           // Force creation
end;

function GetDebugLoggerGroups: TLazLoggerLogGroupList;
begin
  if (TheLazLoggerGroups = nil) then begin
    TheLazLoggerGroups := TLazLoggerLogGroupList.Create;
    TheLazLoggerGroups.AddReference;
  end;
  Result := TLazLoggerLogGroupList(TheLazLoggerGroups);
end;

procedure SetDebugLoggerGroups(ALogGroups: TLazLoggerLogGroupList);
begin
  ReleaseRefAndNil(TheLazLoggerGroups);
  TheLazLoggerGroups := ALogGroups;
  TheLazLoggerGroups.AddReference;
end;

function GetParamByNameCount(const AName: String): integer;
var
  i, l: Integer;
begin
  Result := 0;;
  l := Length(AName);
  for i:= 1 to Paramcount do begin
    if copy(ParamStrUTF8(i),1, l) = AName then
      inc(Result);
  end;
end;

function GetParamByName(const AName: String; AnIndex: Integer): string;
var
  i, l: Integer;
begin
  l := Length(AName);
  for i:= 1 to Paramcount do begin
    if copy(ParamStrUTF8(i),1, l) = AName then begin
      dec(AnIndex);
      if AnIndex < 0 then begin
        Result := copy(ParamStrUTF8(i), l+1, Length(ParamStrUTF8(i))-l);
        break;
      end;
    end;
  end;
end;

{ TLazLoggerLogGroupList }

procedure TLazLoggerLogGroupList.Clear;
begin
  while FList.Count > 0 do begin
    Dispose(Item[0]);
    FList.Delete(0);
  end;
end;

function TLazLoggerLogGroupList.GetItem(Index: Integer): PLazLoggerLogGroup;
begin
  Result := PLazLoggerLogGroup(FList[Index])
end;

function TLazLoggerLogGroupList.NewItem(const AConfigName: String;
  ADefaulEnabled: Boolean): PLazLoggerLogGroup;
begin
  New(Result);
  Result^.ConfigName := UpperCase(AConfigName);
  Result^.Enabled := ADefaulEnabled;
  Result^.Flags := [];
  Result^.FOpenedIndents := 0;
end;

constructor TLazLoggerLogGroupList.Create;
begin
  FList := TFPList.Create;
end;

destructor TLazLoggerLogGroupList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TLazLoggerLogGroupList.Assign(Src: TLazLoggerLogGroupList);
var
  i: Integer;
begin
  Clear;
  if (Src = nil) then
    exit;
  for i := 0 to Src.Count - 1 do
    Add('')^ := Src.Item[i]^;
end;

function TLazLoggerLogGroupList.Add(const AConfigName: String;
  ADefaulEnabled: Boolean): PLazLoggerLogGroup;
begin
  if Find(AConfigName) <> nil then
    raise Exception.Create('Duplicate LogGroup ' + AConfigName);
  Result := NewItem(AConfigName, ADefaulEnabled);
  FList.Add(Result);
end;

function TLazLoggerLogGroupList.FindOrAdd(const AConfigName: String;
  ADefaulEnabled: Boolean): PLazLoggerLogGroup;
begin
  Result := Find(AConfigName);
  if Result <> nil then exit;
  Result := NewItem(AConfigName, ADefaulEnabled);
  FList.Add(Result);
end;

function TLazLoggerLogGroupList.IndexOf(const AConfigName: String): integer;
var
  s: String;
begin
  Result := Count - 1;
  s := UpperCase(AConfigName);
  while (Result >= 0) and (Item[Result]^.ConfigName <> s) do
    dec(Result);
end;

function TLazLoggerLogGroupList.IndexOf(const AnEntry: PLazLoggerLogGroup): integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (Item[Result] <> AnEntry) do
    dec(Result);
end;

function TLazLoggerLogGroupList.Find(const AConfigName: String): PLazLoggerLogGroup;
var
  i: Integer;
begin
  Result := nil;
  i := IndexOf(AConfigName);
  if i >= 0 then
    Result := Item[i];
end;

procedure TLazLoggerLogGroupList.Remove(const AConfigName: String);
var
  i: Integer;
begin
  i := IndexOf(AConfigName);
  if i >= 0 then begin
    Dispose(Item[i]);
    FList.Delete(i);
  end;
end;

procedure TLazLoggerLogGroupList.Remove(const AnEntry: PLazLoggerLogGroup);
var
  i: Integer;
begin
  i := IndexOf(AnEntry);
  if i >= 0 then begin
    Dispose(Item[i]);
    FList.Delete(i);
  end;
end;

function TLazLoggerLogGroupList.Count: integer;
begin
  Result := FList.Count;
end;

{ TLazLogger }

function TLazLogger.GetLogGroupList: TLazLoggerLogGroupList;
begin
  if UseGlobalLogGroupList then begin
    Result := DebugLoggerGroups;
    exit;
  end;

  if FLogGroupList = nil then begin
    FLogGroupList := TLazLoggerLogGroupList.Create;
    FLogGroupList.AddReference;
  end;
  Result := TLazLoggerLogGroupList(FLogGroupList);
end;

procedure TLazLogger.SetUseGlobalLogGroupList(AValue: Boolean);
begin
  if FUseGlobalLogGroupList = AValue then Exit;
  FUseGlobalLogGroupList := AValue;
end;

procedure TLazLogger.SetMaxNestPrefixLen(AValue: Integer);
begin
  if FMaxNestPrefixLen = AValue then Exit;
  FMaxNestPrefixLen := AValue;
  IndentChanged;
end;

procedure TLazLogger.SetNestLvlIndent(AValue: Integer);
begin
  if FNestLvlIndent = AValue then Exit;
  FNestLvlIndent := AValue;
  IndentChanged;
end;

procedure TLazLogger.DoInit;
begin
  //
end;

procedure TLazLogger.DoFinsh;
begin
  //
end;

procedure TLazLogger.DoDebuglnStack(const s: string);
begin
  //
end;

procedure TLazLogger.IncreaseIndent;
begin
  //
end;

procedure TLazLogger.DecreaseIndent;
begin
  //
end;

procedure TLazLogger.IncreaseIndent(LogGroup: PLazLoggerLogGroup);
begin
  //
end;

procedure TLazLogger.DecreaseIndent(LogGroup: PLazLoggerLogGroup);
begin
  //
end;

procedure TLazLogger.IndentChanged;
begin
  //
end;

procedure TLazLogger.DoDbgOut(const s: string);
begin
  //
end;

procedure TLazLogger.DoDebugLn(const s: string);
begin
  //
end;

function TLazLogger.ArgsToString(Args: array of const): string;
var
  i: Integer;
begin
  Result := '';
  for i:=Low(Args) to High(Args) do begin
    case Args[i].VType of
      vtInteger:    Result := Result + dbgs(Args[i].vinteger);
      vtInt64:      Result := Result + dbgs(Args[i].VInt64^);
      vtQWord:      Result := Result + dbgs(Args[i].VQWord^);
      vtBoolean:    Result := Result + dbgs(Args[i].vboolean);
      vtExtended:   Result := Result + dbgs(Args[i].VExtended^);
  {$ifdef FPC_CURRENCY_IS_INT64}
      // MWE:
      // fpc 2.x has troubles in choosing the right dbgs()
      // so we convert here
      vtCurrency:   Result := Result + dbgs(int64(Args[i].vCurrency^)/10000, 4);
  {$else}
      vtCurrency:   Result := Result + dbgs(Args[i].vCurrency^);
  {$endif}
      vtString:     Result := Result + Args[i].VString^;
      vtAnsiString: Result := Result + AnsiString(Args[i].VAnsiString);
      vtChar:       Result := Result + Args[i].VChar;
      vtPChar:      Result := Result + Args[i].VPChar;
      vtPWideChar:  Result := Result + Args[i].VPWideChar;
      vtWideChar:   Result := Result + AnsiString(Args[i].VWideChar);
      vtWidestring: Result := Result + AnsiString(WideString(Args[i].VWideString));
      vtObject:     Result := Result + DbgSName(Args[i].VObject);
      vtClass:      Result := Result + DbgSName(Args[i].VClass);
      vtPointer:    Result := Result + Dbgs(Args[i].VPointer);
      else          Result := Result + '?unknown variant?';
    end;
  end;
end;

constructor TLazLogger.Create;
begin
  FIsInitialized := False;
  FUseGlobalLogGroupList := False;

  FMaxNestPrefixLen := 15;
  FNestLvlIndent := 2;

  FLogGroupList := nil;
end;

destructor TLazLogger.Destroy;
begin
  Finish;
  if TheLazLogger = Self then TheLazLogger := nil;
  ReleaseRefAndNil(FLogGroupList);
  inherited Destroy;
end;

procedure TLazLogger.Assign(Src: TLazLogger);
begin
  if (Src = nil) then
    exit;
  FMaxNestPrefixLen := Src.FMaxNestPrefixLen;
  FNestLvlIndent    := Src.FNestLvlIndent;

  FUseGlobalLogGroupList := Src.FUseGlobalLogGroupList;
  if (not FUseGlobalLogGroupList) and (Src.FLogGroupList <> nil) then
    LogGroupList.Assign(Src.LogGroupList);
end;

procedure TLazLogger.Init;
begin
  if FIsInitialized then exit;
  DoInit;
  FIsInitialized := True;
end;

procedure TLazLogger.Finish;
begin
  if FIsInitialized then
    DoFinsh;
  FIsInitialized := False;
end;

function TLazLogger.RegisterLogGroup(const AConfigName: String;
  ADefaulEnabled: Boolean): PLazLoggerLogGroup;
begin
  // The basic logger does not add entries from parsig cmd-line. So no need to check
  Result := LogGroupList.Add(AConfigName, ADefaulEnabled);
end;

function TLazLogger.RegisterLogGroup(const AConfigName: String): PLazLoggerLogGroup;
begin
  Result := LogGroupList.Add(AConfigName);
  Result^.Flags := Result^.Flags + [lgfNoDefaultEnabledSpecified];
end;

function TLazLogger.FindOrRegisterLogGroup(const AConfigName: String;
  ADefaulEnabled: Boolean): PLazLoggerLogGroup;
begin
  Result := LogGroupList.FindOrAdd(AConfigName, ADefaulEnabled);
end;

function TLazLogger.FindOrRegisterLogGroup(const AConfigName: String): PLazLoggerLogGroup;
begin
  Result := LogGroupList.FindOrAdd(AConfigName);
  Result^.Flags := Result^.Flags + [lgfNoDefaultEnabledSpecified];
end;

procedure TLazLogger.DebuglnStack(const s: string);
begin
  DoDebuglnStack(s);
end;

procedure TLazLogger.DbgOut(const s: string);
begin
  DoDbgOut(s);
end;

procedure TLazLogger.DbgOut(Args: array of const);
begin
  DoDbgOut(ArgsToString(Args));
end;

procedure TLazLogger.DbgOut(const S: String; Args: array of const);
begin
  DoDbgOut(Format(S, Args));
end;

procedure TLazLogger.DbgOut(const s1, s2: string; const s3: string; const s4: string;
  const s5: string; const s6: string; const s7: string; const s8: string; const s9: string;
  const s10: string; const s11: string; const s12: string; const s13: string;
  const s14: string; const s15: string; const s16: string; const s17: string;
  const s18: string);
begin
  DoDbgOut(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18);
end;

procedure TLazLogger.DebugLn(const s: string);
begin
  DoDebugLn(s);
end;

procedure TLazLogger.DebugLn(Args: array of const);
begin
  DoDebugLn(ArgsToString(Args));
end;

procedure TLazLogger.DebugLn(const S: String; Args: array of const);
begin
  DoDebugLn(Format(S, Args));
end;

procedure TLazLogger.DebugLn(const s1, s2: string; const s3: string; const s4: string;
  const s5: string; const s6: string; const s7: string; const s8: string; const s9: string;
  const s10: string; const s11: string; const s12: string; const s13: string;
  const s14: string; const s15: string; const s16: string; const s17: string;
  const s18: string);
begin
  DoDebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18);
end;

procedure TLazLogger.DebugLnEnter(const s: string);
begin
  DoDebugLn(s);
  IncreaseIndent;
end;

procedure TLazLogger.DebugLnEnter(Args: array of const);
begin
  DoDebugLn(ArgsToString(Args));
  IncreaseIndent;
end;

procedure TLazLogger.DebugLnEnter(s: string; Args: array of const);
begin
  DoDebugLn(Format(S, Args));
  IncreaseIndent;
end;

procedure TLazLogger.DebugLnEnter(const s1, s2: string; const s3: string; const s4: string;
  const s5: string; const s6: string; const s7: string; const s8: string; const s9: string;
  const s10: string; const s11: string; const s12: string; const s13: string;
  const s14: string; const s15: string; const s16: string; const s17: string;
  const s18: string);
begin
  DoDebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18);
  IncreaseIndent;
end;

procedure TLazLogger.DebugLnExit(const s: string);
begin
  DecreaseIndent;
  DoDebugLn(s);
end;

procedure TLazLogger.DebugLnExit(Args: array of const);
begin
  DecreaseIndent;
  DoDebugLn(ArgsToString(Args));
end;

procedure TLazLogger.DebugLnExit(s: string; Args: array of const);
begin
  DecreaseIndent;
  DoDebugLn(Format(S, Args));
end;

procedure TLazLogger.DebugLnExit(const s1, s2: string; const s3: string; const s4: string;
  const s5: string; const s6: string; const s7: string; const s8: string; const s9: string;
  const s10: string; const s11: string; const s12: string; const s13: string;
  const s14: string; const s15: string; const s16: string; const s17: string;
  const s18: string);
begin
  DecreaseIndent;
  DoDebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18);
end;

procedure TLazLogger.DebuglnStack(LogGroup: PLazLoggerLogGroup; const s: string);
begin
  if (LogGroup <> nil) and (not LogGroup^.Enabled) then exit;
  DebuglnStack(s);
end;

procedure TLazLogger.DbgOut(LogGroup: PLazLoggerLogGroup; const s: string);
begin
  if (LogGroup <> nil) and (not LogGroup^.Enabled) then exit;
  DoDbgOut(s);
end;

procedure TLazLogger.DbgOut(LogGroup: PLazLoggerLogGroup; Args: array of const);
begin
  if (LogGroup <> nil) and (not LogGroup^.Enabled) then exit;
  DoDbgOut(ArgsToString(Args));
end;

procedure TLazLogger.DbgOut(LogGroup: PLazLoggerLogGroup; const S: String;
  Args: array of const);
begin
  if (LogGroup <> nil) and (not LogGroup^.Enabled) then exit;
  DoDbgOut(Format(S, Args));
end;

procedure TLazLogger.DbgOut(LogGroup: PLazLoggerLogGroup; const s1, s2: string;
  const s3: string; const s4: string; const s5: string; const s6: string; const s7: string;
  const s8: string; const s9: string; const s10: string; const s11: string; const s12: string;
  const s13: string; const s14: string; const s15: string; const s16: string;
  const s17: string; const s18: string);
begin
  if (LogGroup <> nil) and (not LogGroup^.Enabled) then exit;
  DoDbgOut(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18);
end;

procedure TLazLogger.DebugLn(LogGroup: PLazLoggerLogGroup; const s: string);
begin
  if (LogGroup <> nil) and (not LogGroup^.Enabled) then exit;
  DoDebugLn(s);
end;

procedure TLazLogger.DebugLn(LogGroup: PLazLoggerLogGroup; Args: array of const);
begin
  if (LogGroup <> nil) and (not LogGroup^.Enabled) then exit;
  DoDebugLn(ArgsToString(Args));
end;

procedure TLazLogger.DebugLn(LogGroup: PLazLoggerLogGroup; const S: String;
  Args: array of const);
begin
  if (LogGroup <> nil) and (not LogGroup^.Enabled) then exit;
  DoDebugLn(Format(S, Args));
end;

procedure TLazLogger.DebugLn(LogGroup: PLazLoggerLogGroup; const s1, s2: string;
  const s3: string; const s4: string; const s5: string; const s6: string; const s7: string;
  const s8: string; const s9: string; const s10: string; const s11: string; const s12: string;
  const s13: string; const s14: string; const s15: string; const s16: string;
  const s17: string; const s18: string);
begin
  if (LogGroup <> nil) and (not LogGroup^.Enabled) then exit;
  DoDebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18);
end;

procedure TLazLogger.DebugLnEnter(LogGroup: PLazLoggerLogGroup; const s: string);
begin
  if not( (LogGroup <> nil) and (not LogGroup^.Enabled) ) then
    DoDebugLn(s);
  IncreaseIndent(LogGroup);
end;

procedure TLazLogger.DebugLnEnter(LogGroup: PLazLoggerLogGroup; Args: array of const);
begin
  if not( (LogGroup <> nil) and (not LogGroup^.Enabled) ) then
    DoDebugLn(ArgsToString(Args));
  IncreaseIndent(LogGroup);
end;

procedure TLazLogger.DebugLnEnter(LogGroup: PLazLoggerLogGroup; s: string;
  Args: array of const);
begin
  if not( (LogGroup <> nil) and (not LogGroup^.Enabled) ) then
    DoDebugLn(Format(S, Args));
  IncreaseIndent(LogGroup);
end;

procedure TLazLogger.DebugLnEnter(LogGroup: PLazLoggerLogGroup; const s1, s2: string;
  const s3: string; const s4: string; const s5: string; const s6: string; const s7: string;
  const s8: string; const s9: string; const s10: string; const s11: string; const s12: string;
  const s13: string; const s14: string; const s15: string; const s16: string;
  const s17: string; const s18: string);
begin
  if not( (LogGroup <> nil) and (not LogGroup^.Enabled) ) then
    DoDebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18);
  IncreaseIndent(LogGroup);
end;

procedure TLazLogger.DebugLnExit(LogGroup: PLazLoggerLogGroup; const s: string);
begin
  DecreaseIndent(LogGroup);
  if (LogGroup <> nil) and (not LogGroup^.Enabled) then exit;
  DoDebugLn(s);
end;

procedure TLazLogger.DebugLnExit(LogGroup: PLazLoggerLogGroup; Args: array of const);
begin
  DecreaseIndent(LogGroup);
  if (LogGroup <> nil) and (not LogGroup^.Enabled) then exit;
  DoDebugLn(ArgsToString(Args));
end;

procedure TLazLogger.DebugLnExit(LogGroup: PLazLoggerLogGroup; s: string;
  Args: array of const);
begin
  DecreaseIndent(LogGroup);
  if (LogGroup <> nil) and (not LogGroup^.Enabled) then exit;
  DoDebugLn(Format(S, Args));
end;

procedure TLazLogger.DebugLnExit(LogGroup: PLazLoggerLogGroup; const s1, s2: string;
  const s3: string; const s4: string; const s5: string; const s6: string; const s7: string;
  const s8: string; const s9: string; const s10: string; const s11: string; const s12: string;
  const s13: string; const s14: string; const s15: string; const s16: string;
  const s17: string; const s18: string);
begin
  DecreaseIndent(LogGroup);
  if (LogGroup <> nil) and (not LogGroup^.Enabled) then exit;
  DoDebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18);
end;

{ TLazLoggerWithGroupParam }

procedure TLazLoggerWithGroupParam.SetParamForEnabledLogGroups(AValue: String);
begin
  if FParamForEnabledLogGroups = AValue then Exit;
  FParamForEnabledLogGroups := AValue;
  ParseParamForEnabledLogGroups;
end;

procedure TLazLoggerWithGroupParam.ParseParamForEnabledLogGroups;
var
  i, j, c: Integer;
  list: TStringList;
  g: PLazLoggerLogGroup;
  s: String;
  e: Boolean;
begin
  c := GetParamByNameCount(FParamForEnabledLogGroups);
  FLogDefaultEnabled := False;
  FLogAllDefaultDisabled := FAlse;

  list := TStringList.Create;
  for i := 0 to c - 1 do begin
    s := GetParamByName(FParamForEnabledLogGroups, i);

    if s = '-' then begin
      // clear all
      FLogDefaultEnabled := False;
      for j := 0 to LogGroupList.Count - 1 do
        LogGroupList[j]^.Enabled := False;
      FLogAllDefaultDisabled := True;
    end
    else
    begin
      list.CommaText := s;
      for j := 0 to list.Count - 1 do begin
        s := list[j];
        if (s = '-') or (s='') then
          continue; // invalid, within comma list
        if s[1] = '-' then
          e := False
        else
          e := True;
        if s[1] in ['-', '+'] then delete(s,1,1);
        if (s='') then
          continue;

        if e then
          FLogDefaultEnabled := False;

        g := LogGroupList.Find(s);
        if g <> nil then begin
          g^.Enabled := e;
          g^.Flags := g^.Flags - [lgfNoDefaultEnabledSpecified];
        end
        else begin
          g := LogGroupList.Add(s, e);
          g^.Flags := g^.Flags + [lgfAddedByParamParser];
        end;
      end;
    end;
  end;
  list.Free;

  if not FLogParamParsed then begin
    // first parse, reset default unless specified in RegisterLogGroup();
    for i := 0 to LogGroupList.Count - 1 do
      if lgfNoDefaultEnabledSpecified in LogGroupList[i]^.Flags then
        LogGroupList[i]^.Enabled := FLogDefaultEnabled;
  end;

  FLogParamParsed := True;
end;

constructor TLazLoggerWithGroupParam.Create;
begin
  inherited;
  FLogDefaultEnabled := False;
  FLogAllDefaultDisabled := False;
end;

procedure TLazLoggerWithGroupParam.Assign(Src: TLazLogger);
begin
  inherited Assign(Src);
  if (Src <> nil) and (Src is TLazLoggerWithGroupParam) then begin
    FLogParamParsed := False;
    FParamForEnabledLogGroups := TLazLoggerWithGroupParam(Src).FParamForEnabledLogGroups;
  end;
end;

function TLazLoggerWithGroupParam.RegisterLogGroup(const AConfigName: String): PLazLoggerLogGroup;
var
  Default, DefaultFound: Boolean;
begin
  Result := LogGroupList.Find(AConfigName);
  Default := FLogDefaultEnabled;
  DefaultFound := False;
  if Result <> nil then begin
    Default := Result^.Enabled;
    DefaultFound := not(lgfNoDefaultEnabledSpecified in Result^.Flags);
  end;

  Result := RegisterLogGroup(AConfigName, Default);

  if not DefaultFound then
    Result^.Flags := Result^.Flags + [lgfNoDefaultEnabledSpecified];
end;

function TLazLoggerWithGroupParam.RegisterLogGroup(const AConfigName: String;
  ADefaulEnabled: Boolean): PLazLoggerLogGroup;
begin
  if FLogAllDefaultDisabled then
    ADefaulEnabled := False;
  Result := LogGroupList.Find(AConfigName);
  if Result <> nil then begin
    if not(lgfAddedByParamParser in Result^.Flags) then
      raise Exception.Create('Duplicate LogGroup ' + AConfigName);
    if ADefaulEnabled and not(lgfAddedByParamParser in Result^.Flags) then
      Result^.Enabled := True;
    Result^.Flags := Result^.Flags - [lgfAddedByParamParser];
  end
  else
    Result := LogGroupList.Add(AConfigName, ADefaulEnabled);
end;

function TLazLoggerWithGroupParam.FindOrRegisterLogGroup(const AConfigName: String): PLazLoggerLogGroup;
begin
  Result := LogGroupList.Find(AConfigName);
  if Result = nil then
    Result := RegisterLogGroup(AConfigName)
  else
    Result^.Flags := Result^.Flags - [lgfAddedByParamParser];
end;

function TLazLoggerWithGroupParam.FindOrRegisterLogGroup(const AConfigName: String;
  ADefaulEnabled: Boolean): PLazLoggerLogGroup;
begin
  Result := LogGroupList.Find(AConfigName);
  if Result = nil then
    Result := RegisterLogGroup(AConfigName, ADefaulEnabled)
  else
  begin
    if (lgfNoDefaultEnabledSpecified in Result^.Flags) and
       not(lgfAddedByParamParser in Result^.Flags)
    then
      Result^.Enabled := ADefaulEnabled;
    Result^.Flags := Result^.Flags - [lgfNoDefaultEnabledSpecified, lgfAddedByParamParser];
  end;
end;

function ConvertLineEndings(const s: string): string;
var
  i: Integer;
  EndingStart: LongInt;
begin
  Result:=s;
  i:=1;
  while (i<=length(Result)) do begin
    if Result[i] in [#10,#13] then begin
      EndingStart:=i;
      inc(i);
      if (i<=length(Result)) and (Result[i] in [#10,#13])
      and (Result[i]<>Result[i-1]) then begin
        inc(i);
      end;
      if (length(LineEnding)<>i-EndingStart)
      or (LineEnding<>copy(Result,EndingStart,length(LineEnding))) then begin
        // line end differs => replace with current LineEnding
        Result:=
          copy(Result,1,EndingStart-1)+LineEnding+copy(Result,i,length(Result));
        i:=EndingStart+length(LineEnding);
      end;
    end else
      inc(i);
  end;
end;

finalization // Using TObject, so if none of the functions is used in the app, then even the rlass should be smart linked
  ReleaseRefAndNil(TheLazLogger);
  ReleaseRefAndNil(PrevLazLogger);
  ReleaseRefAndNil(TheLazLoggerGroups);

end.

