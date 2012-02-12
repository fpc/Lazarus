unit LazLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, types, math;

type

  TLazLoggerLogGroupFlag = (lgfAddedByParamParser, lgfNoDefaultEnabledSpecified);
  TLazLoggerLogGroupFlags = set of TLazLoggerLogGroupFlag;

  TLazLoggerLogGroup = record
    ConfigName: String;  // case insensitive
    Enabled: Boolean;
    Flags: TLazLoggerLogGroupFlags;
    FOpenedIndents: Integer;
  end;
  PLazLoggerLogGroup = ^TLazLoggerLogGroup;

procedure DebuglnStack(const s: string = '');

procedure DbgOut(const s: string = ''); inline; overload;
procedure DbgOut(Args: array of const); {inline;} overload;
procedure DbgOut(const S: String; Args: array of const); {inline;} overload;// similar to Format(s,Args)
procedure DbgOut(const s1, s2: string; const s3: string = '';
                 const s4: string = ''; const s5: string = ''; const s6: string = '';
                 const s7: string = ''; const s8: string = ''; const s9: string = '';
                 const s10: string = ''; const s11: string = ''; const s12: string = '';
                 const s13: string = ''; const s14: string = ''; const s15: string = '';
                 const s16: string = ''; const s17: string = ''; const s18: string = ''); inline; overload;

procedure DebugLn(const s: string = ''); inline; overload;
procedure DebugLn(Args: array of const); {inline;} overload;
procedure DebugLn(const S: String; Args: array of const); {inline;} overload;// similar to Format(s,Args)
procedure DebugLn(const s1, s2: string; const s3: string = '';
                  const s4: string = ''; const s5: string = ''; const s6: string = '';
                  const s7: string = ''; const s8: string = ''; const s9: string = '';
                  const s10: string = ''; const s11: string = ''; const s12: string = '';
                  const s13: string = ''; const s14: string = ''; const s15: string = '';
                  const s16: string = ''; const s17: string = ''; const s18: string = ''); inline; overload;

procedure DebugLnEnter(const s: string = ''); inline; overload;
procedure DebugLnEnter(Args: array of const); {inline;} overload;
procedure DebugLnEnter(s: string; Args: array of const); {inline;} overload;
procedure DebugLnEnter(const s1, s2: string; const s3: string = '';
                       const s4: string = ''; const s5: string = ''; const s6: string = '';
                       const s7: string = ''; const s8: string = ''; const s9: string = '';
                       const s10: string = ''; const s11: string = ''; const s12: string = '';
                       const s13: string = ''; const s14: string = ''; const s15: string = '';
                       const s16: string = ''; const s17: string = ''; const s18: string = ''); inline; overload;

procedure DebugLnExit(const s: string = ''); inline; overload;
procedure DebugLnExit(Args: array of const); {inline;} overload;
procedure DebugLnExit(s: string; Args: array of const); {inline;} overload;
procedure DebugLnExit (const s1, s2: string; const s3: string = '';
                       const s4: string = ''; const s5: string = ''; const s6: string = '';
                       const s7: string = ''; const s8: string = ''; const s9: string = '';
                       const s10: string = ''; const s11: string = ''; const s12: string = '';
                       const s13: string = ''; const s14: string = ''; const s15: string = '';
                       const s16: string = ''; const s17: string = ''; const s18: string = ''); inline; overload;


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


function DbgS(const c: cardinal): string; overload;
function DbgS(const i: longint): string; overload;
function DbgS(const i: int64): string; overload;
function DbgS(const q: qword): string; overload;
function DbgS(const r: TRect): string; overload;
function DbgS(const p: TPoint): string; overload;
function DbgS(const p: pointer): string; overload;
function DbgS(const e: extended; MaxDecimals: integer = 999): string; overload;
function DbgS(const b: boolean): string; overload;
function DbgS(const s: TComponentState): string; overload;
function DbgS(const m: TMethod): string; overload;
function DbgSName(const p: TObject): string; overload;
function DbgSName(const p: TClass): string; overload;
function DbgStr(const StringWithSpecialChars: string): string; overload;
function DbgWideStr(const StringWithSpecialChars: widestring): string; overload;
function dbgMemRange(P: PByte; Count: integer; Width: integer = 0): string; overload;
function dbgMemStream(MemStream: TCustomMemoryStream; Count: integer): string; overload;
function dbgObjMem(AnObject: TObject): string; overload;
function dbghex(i: Int64): string; overload;

function DbgS(const i1,i2,i3,i4: integer): string; overload;
function DbgS(const Shift: TShiftState): string; overload;

function DbgS(const ASize: TSize): string; overload;

function ConvertLineEndings(const s: string): string;

type

  { TLazLoggerLogGroupList }

  TLazLoggerLogGroupList = class
  private
    FList: TFPList;
    procedure Clear;
    function GetItem(Index: Integer): PLazLoggerLogGroup;
  public
    constructor Create;
    destructor  Destroy; override;
    function  Add(const AConfigName: String; ADefaulEnabled: Boolean = False) : PLazLoggerLogGroup;
    function  IndexOf(const AConfigName: String): integer;
    function  IndexOf(const AnEntry: PLazLoggerLogGroup): integer;
    function  Find(const AConfigName: String): PLazLoggerLogGroup;
    procedure Remove(const AConfigName: String);
    procedure Remove(const AnEntry: PLazLoggerLogGroup);
    function  Count: integer;
    property  Item[Index: Integer]: PLazLoggerLogGroup read GetItem; default;
  end;

  TLazLoggerWriteEvent = procedure (Sender: TObject; S: string; var Handled: Boolean) of object;

  { TLazLogger }

  TLazLogger = class
  private
    FAutoDestroy: Boolean;
    FIsInitialized: Boolean;

    FEnvironmentForLogFileName: String;
    FParamForEnabledLogGroups: String;
    FUseStdOut: Boolean;
    FCloseLogFileBetweenWrites: Boolean;
    FOnDbgOut: TLazLoggerWriteEvent;
    FOnDebugLn: TLazLoggerWriteEvent;
    FMaxNestPrefixLen: Integer;
    FNestLvlIndent: Integer;

    FActiveLogText: PText; // may point to stdout
    FLogText: Text;
    FLogTextInUse, FLogTextFailed: Boolean;
    FLogName: String;
    FGetLogFileNameDone: Boolean;

    FDebugNestLvl: Integer;
    FDebugIndent: String;
    FDebugNestAtBOL: Boolean;

    FLogGroupList: TObject; // Using TObject, so if none of the functions is used in the app, then even the rlass should be smart linked
    FParamForLogFileName: String;
    FLogDefaultEnabled, FLogAllDefaultDisabled, FLogParamParsed: Boolean;

    procedure SetAutoDestroy(AValue: Boolean);
    procedure SetCloseLogFileBetweenWrites(AValue: Boolean);
    procedure SetEnvironmentForLogFileName(AValue: String);
    procedure SetParamForLogFileName(AValue: String);
    procedure SetLogName(AValue: String);
    procedure SetMaxNestPrefixLen(AValue: Integer);
    procedure SetNestLvlIndent(AValue: Integer);
    function  GetParamByNameCount(const AName: String): integer;
    function  GetParamByName(const AName: String; AnIndex: Integer = 0): string;

    procedure SetParamForEnabledLogGroups(AValue: String);
    procedure ParseParamForEnabledLogGroups;
    function  GetLogGroupList: TLazLoggerLogGroupList;
  protected
    procedure WriteToFile(const s: string); inline;
    procedure WriteLnToFile(const s: string); inline;

    procedure DoInit; virtual;
    procedure DoFinsh; virtual;
    procedure DoOpenFile; virtual;
    procedure DoCloseFile; virtual;
    function  GetLogFileName: string; virtual;

    procedure IncreaseIndent; virtual;
    procedure DecreaseIndent; virtual;
    procedure IncreaseIndent(LogGroup: PLazLoggerLogGroup); virtual;
    procedure DecreaseIndent(LogGroup: PLazLoggerLogGroup); virtual;
    procedure CreateIndent; virtual;
    procedure DoDbgOut(const s: string); virtual;
    procedure DoDebugLn(const s: string); virtual;

    function  ArgsToString(Args: array of const): string;
    property  IsInitialized: Boolean read FIsInitialized;
    property  ActiveLogText: PText read FActiveLogText;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Init;
    procedure Finish;
    // AutoDestroy only works for the logger accessble through DebugLogger
    property  AutoDestroy: Boolean read FAutoDestroy write SetAutoDestroy;

    // The Name for the Logfile
    property  LogName: String read FLogName write SetLogName;
    // A param on the commandline, that may contain the name (if not already set)
    // example/default: --debug-log=
    property  ParamForLogFileName: String read FParamForLogFileName write SetParamForLogFileName;
    // Environment to specify log file name (* replaced by param(0))
    // example/default: *_debuglog
    property  EnvironmentForLogFileName: String read FEnvironmentForLogFileName write SetEnvironmentForLogFileName; // "*" will be replaced by appname
    property  UseStdOut: Boolean read FUseStdOut write FUseStdOut;
    property  CloseLogFileBetweenWrites: Boolean read FCloseLogFileBetweenWrites write SetCloseLogFileBetweenWrites;
    property  OnDebugLn: TLazLoggerWriteEvent read FOnDebugLn write FOnDebugLn;
    property  OnDbgOut:  TLazLoggerWriteEvent read FOnDbgOut write FOnDbgOut;

    property  NestLvlIndent: Integer read FNestLvlIndent write SetNestLvlIndent;
    property  MaxNestPrefixLen: Integer read FMaxNestPrefixLen write SetMaxNestPrefixLen;

    function  RegisterLogGroup(const AConfigName: String; ADefaulEnabled: Boolean) : PLazLoggerLogGroup;
    function  RegisterLogGroup(const AConfigName: String) : PLazLoggerLogGroup;
    function  FindOrRegisterLogGroup(const AConfigName: String; ADefaulEnabled: Boolean) : PLazLoggerLogGroup;
    function  FindOrRegisterLogGroup(const AConfigName: String) : PLazLoggerLogGroup;
    // comma separated list / not present = defaults (none unless emabled in code) / - means none
    property  ParamForEnabledLogGroups: String read FParamForEnabledLogGroups write SetParamForEnabledLogGroups;
    property  LogGroupList: TLazLoggerLogGroupList read GetLogGroupList;
  public
    procedure DebuglnStack(const s: string = ''); // TODO:

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

function GetDebugLogger: TLazLogger; inline;
procedure SetDebugLogger(ALogger: TLazLogger);

property DebugLogger: TLazLogger read GetDebugLogger write SetDebugLogger;

implementation

{$ifdef wince}
const
  Str_LCL_Debug_File = 'lcldebug.log';
{$endif}

var // Using TObject, so if none of the functions is used in the app, then even the rlass should be smart linked
  TheLazLogger: TObject = nil;
  TheLazLoggerAutoFree: TObject = nil;

function GetDebugLogger: TLazLogger; inline;
begin
  if TheLazLogger = nil then
    TheLazLogger := TLazLogger.Create;
  Result := TLazLogger(TheLazLogger);
  Result.AutoDestroy := True;
end;

procedure SetDebugLogger(ALogger: TLazLogger);
begin
  if (TheLazLoggerAutoFree <> nil) then
    FreeAndNil(TheLazLoggerAutoFree);
  TheLazLogger := ALogger;
end;

procedure DebuglnStack(const s: string);
begin
  DebugLogger.DebuglnStack(s);
end;

procedure DbgOut(const s: string);
begin
  DebugLogger.DbgOut(s);
end;

procedure DbgOut(Args: array of const);
begin
  DebugLogger.DbgOut(Args);
end;

procedure DbgOut(const S: String; Args: array of const);
begin
  DebugLogger.DbgOut(S, Args);
end;

procedure DbgOut(const s1, s2: string; const s3: string; const s4: string; const s5: string;
  const s6: string; const s7: string; const s8: string; const s9: string; const s10: string;
  const s11: string; const s12: string; const s13: string; const s14: string;
  const s15: string; const s16: string; const s17: string; const s18: string);
begin
  DebugLogger.DbgOut(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18);
end;

procedure DebugLn(const s: string);
begin
  DebugLogger.DebugLn(s);
end;

procedure DebugLn(Args: array of const);
begin
  DebugLogger.DebugLn(Args);
end;

procedure DebugLn(const S: String; Args: array of const);
begin
  DebugLogger.DebugLn(S, Args);
end;

procedure DebugLn(const s1, s2: string; const s3: string; const s4: string; const s5: string;
  const s6: string; const s7: string; const s8: string; const s9: string; const s10: string;
  const s11: string; const s12: string; const s13: string; const s14: string;
  const s15: string; const s16: string; const s17: string; const s18: string);
begin
  DebugLogger.DebugLn(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18);
end;

procedure DebugLnEnter(const s: string);
begin
  DebugLogger.DebugLnEnter(s);
end;

procedure DebugLnEnter(Args: array of const);
begin
  DebugLogger.DebugLnEnter(Args);
end;

procedure DebugLnEnter(s: string; Args: array of const);
begin
  DebugLogger.DebugLnEnter(s, Args);
end;

procedure DebugLnEnter(const s1, s2: string; const s3: string; const s4: string;
  const s5: string; const s6: string; const s7: string; const s8: string; const s9: string;
  const s10: string; const s11: string; const s12: string; const s13: string;
  const s14: string; const s15: string; const s16: string; const s17: string;
  const s18: string);
begin
  DebugLogger.DebugLnEnter(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18);
end;

procedure DebugLnExit(const s: string);
begin
  DebugLogger.DebugLnExit(s);
end;

procedure DebugLnExit(Args: array of const);
begin
  DebugLogger.DebugLnExit(Args);
end;

procedure DebugLnExit(s: string; Args: array of const);
begin
  DebugLogger.DebugLnExit(s, Args);
end;

procedure DebugLnExit(const s1, s2: string; const s3: string; const s4: string;
  const s5: string; const s6: string; const s7: string; const s8: string; const s9: string;
  const s10: string; const s11: string; const s12: string; const s13: string;
  const s14: string; const s15: string; const s16: string; const s17: string;
  const s18: string);
begin
  DebugLogger.DebugLnExit(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18);
end;

procedure DebuglnStack(LogGroup: PLazLoggerLogGroup; const s: string);
begin
  DebugLogger.DebuglnStack(LogGroup, s);
end;

procedure DbgOut(LogGroup: PLazLoggerLogGroup; const s: string);
begin
  DebugLogger.DbgOut(LogGroup, s);
end;

procedure DbgOut(LogGroup: PLazLoggerLogGroup; Args: array of const);
begin
  DebugLogger.DbgOut(LogGroup, Args);
end;

procedure DbgOut(LogGroup: PLazLoggerLogGroup; const S: String; Args: array of const);
begin
  DebugLogger.DbgOut(LogGroup, s, Args);
end;

procedure DbgOut(LogGroup: PLazLoggerLogGroup; const s1, s2: string; const s3: string;
  const s4: string; const s5: string; const s6: string; const s7: string; const s8: string;
  const s9: string; const s10: string; const s11: string; const s12: string;
  const s13: string; const s14: string; const s15: string; const s16: string;
  const s17: string; const s18: string);
begin
  DebugLogger.DbgOut(LogGroup, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18);
end;

procedure DebugLn(LogGroup: PLazLoggerLogGroup; const s: string);
begin
  DebugLogger.DebugLn(LogGroup, s);
end;

procedure DebugLn(LogGroup: PLazLoggerLogGroup; Args: array of const);
begin
  DebugLogger.DebugLn(LogGroup, Args);
end;

procedure DebugLn(LogGroup: PLazLoggerLogGroup; const S: String; Args: array of const);
begin
  DebugLogger.DebugLn(LogGroup, s, Args);
end;

procedure DebugLn(LogGroup: PLazLoggerLogGroup; const s1, s2: string; const s3: string;
  const s4: string; const s5: string; const s6: string; const s7: string; const s8: string;
  const s9: string; const s10: string; const s11: string; const s12: string;
  const s13: string; const s14: string; const s15: string; const s16: string;
  const s17: string; const s18: string);
begin
  DebugLogger.DebugLn(LogGroup, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18);
end;

procedure DebugLnEnter(LogGroup: PLazLoggerLogGroup; const s: string);
begin
  DebugLogger.DebugLnEnter(LogGroup, s);
end;

procedure DebugLnEnter(LogGroup: PLazLoggerLogGroup; Args: array of const);
begin
  DebugLogger.DebugLnEnter(LogGroup, Args);
end;

procedure DebugLnEnter(LogGroup: PLazLoggerLogGroup; s: string; Args: array of const);
begin
  DebugLogger.DebugLnEnter(LogGroup, s, Args);
end;

procedure DebugLnEnter(LogGroup: PLazLoggerLogGroup; const s1, s2: string; const s3: string;
  const s4: string; const s5: string; const s6: string; const s7: string; const s8: string;
  const s9: string; const s10: string; const s11: string; const s12: string;
  const s13: string; const s14: string; const s15: string; const s16: string;
  const s17: string; const s18: string);
begin
  DebugLogger.DebugLnEnter(LogGroup, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18);
end;

procedure DebugLnExit(LogGroup: PLazLoggerLogGroup; const s: string);
begin
  DebugLogger.DebugLnExit(LogGroup, s);
end;

procedure DebugLnExit(LogGroup: PLazLoggerLogGroup; Args: array of const);
begin
  DebugLogger.DebugLnExit(LogGroup, Args);
end;

procedure DebugLnExit(LogGroup: PLazLoggerLogGroup; s: string; Args: array of const);
begin
  DebugLogger.DebugLnExit(LogGroup, s, Args);
end;

procedure DebugLnExit(LogGroup: PLazLoggerLogGroup; const s1, s2: string; const s3: string;
  const s4: string; const s5: string; const s6: string; const s7: string; const s8: string;
  const s9: string; const s10: string; const s11: string; const s12: string;
  const s13: string; const s14: string; const s15: string; const s16: string;
  const s17: string; const s18: string);
begin
  DebugLogger.DebugLnExit(LogGroup, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18);
end;


function DbgS(const c: cardinal): string;
begin
  Result:=IntToStr(c);
end;

function DbgS(const i: longint): string;
begin
  Result:=IntToStr(i);
end;

function DbgS(const i: int64): string;
begin
  Result:=IntToStr(i);
end;

function DbgS(const q: qword): string;
begin
  Result:=IntToStr(q);
end;

function DbgS(const r: TRect): string;
begin
  Result:='l='+IntToStr(r.Left)+',t='+IntToStr(r.Top)
         +',r='+IntToStr(r.Right)+',b='+IntToStr(r.Bottom);
end;

function DbgS(const p: TPoint): string;
begin
  Result:='(x='+IntToStr(p.x)+',y='+IntToStr(p.y)+')';
end;

function DbgS(const p: pointer): string;
begin
  Result:=HexStr(PtrUInt(p),2*sizeof(PtrInt));
end;

function DbgS(const e: extended; MaxDecimals: integer): string;
begin
  Result:=copy(FloatToStr(e),1,MaxDecimals);
end;

function DbgS(const b: boolean): string;
begin
  if b then Result:='True' else Result:='False';
end;

function DbgS(const s: TComponentState): string;

  procedure Add(const a: string);
  begin
    if Result<>'' then
      Result:=Result+',';
    Result:=Result+a;
  end;

begin
  Result:='';
  if csLoading in s then Add('csLoading');
  if csReading in s then Add('csReading');
  if csWriting in s then Add('csWriting');
  if csDestroying in s then Add('csDestroying');
  if csDesigning in s then Add('csDesigning');
  if csAncestor in s then Add('csAncestor');
  if csUpdating in s then Add('csUpdating');
  if csFixups in s then Add('csFixups');
  if csFreeNotification in s then Add('csFreeNotification');
  if csInline in s then Add('csInline');
  if csDesignInstance in s then Add('csDesignInstance');
  Result:='['+Result+']';
end;

function DbgS(const m: TMethod): string;
var
  o: TObject;
  aMethodName: ShortString;
begin
  o:=TObject(m.Data);
  Result:=dbgsname(o)+'.'+dbgs(m.Code);
  if (o<>nil) and (m.Code<>nil) then begin
    aMethodName:=o.MethodName(m.Code);
    Result:=Result+'='''+aMethodName+'''';
  end;
end;

function DbgSName(const p: TObject): string;
begin
  if p=nil then
    Result:='nil'
  else if p is TComponent then
    Result:=TComponent(p).Name+':'+p.ClassName
  else
    Result:=p.ClassName;
end;

function DbgSName(const p: TClass): string;
begin
  if p=nil then
    Result:='nil'
  else
    Result:=p.ClassName;
end;

function DbgStr(const StringWithSpecialChars: string): string;
var
  i: Integer;
  s: String;
begin
  Result:=StringWithSpecialChars;
  i:=1;
  while (i<=length(Result)) do begin
    case Result[i] of
    ' '..#126: inc(i);
    else
      s:='#'+HexStr(ord(Result[i]),2);
      Result:=copy(Result,1,i-1)+s+copy(Result,i+1,length(Result)-i);
      inc(i,length(s));
    end;
  end;
end;

function DbgWideStr(const StringWithSpecialChars: widestring): string;
var
  s: String;
  SrcPos: Integer;
  DestPos: Integer;
  i: Integer;
begin
  SetLength(Result,length(StringWithSpecialChars));
  SrcPos:=1;
  DestPos:=1;
  while SrcPos<=length(StringWithSpecialChars) do begin
    i:=ord(StringWithSpecialChars[SrcPos]);
    case i of
    32..126:
      begin
        Result[DestPos]:=chr(i);
        inc(SrcPos);
        inc(DestPos);
      end;
    else
      s:='#'+HexStr(i,4);
      inc(SrcPos);
      Result:=copy(Result,1,DestPos-1)+s+copy(Result,DestPos+1,length(Result));
      inc(DestPos,length(s));
    end;
  end;
end;

function dbgMemRange(P: PByte; Count: integer; Width: integer): string;
const
  HexChars: array[0..15] of char = '0123456789ABCDEF';
  LineEnd: shortstring = LineEnding;
var
  i: Integer;
  NewLen: Integer;
  Dest: PChar;
  Col: Integer;
  j: Integer;
begin
  Result:='';
  if (p=nil) or (Count<=0) then exit;
  NewLen:=Count*2;
  if Width>0 then begin
    inc(NewLen,(Count div Width)*length(LineEnd));
  end;
  SetLength(Result,NewLen);
  Dest:=PChar(Result);
  Col:=1;
  for i:=0 to Count-1 do begin
    Dest^:=HexChars[PByte(P)[i] shr 4];
    inc(Dest);
    Dest^:=HexChars[PByte(P)[i] and $f];
    inc(Dest);
    inc(Col);
    if (Width>0) and (Col>Width) then begin
      Col:=1;
      for j:=1 to length(LineEnd) do begin
        Dest^:=LineEnd[j];
        inc(Dest);
      end;
    end;
  end;
end;

function dbgMemStream(MemStream: TCustomMemoryStream; Count: integer): string;
var
  s: string;
begin
  Result:='';
  if (MemStream=nil) or (not (MemStream is TCustomMemoryStream)) or (Count<=0)
  then exit;
  Count:=Min(Count,MemStream.Size);
  if Count<=0 then exit;
  SetLength(s,Count);
  Count:=MemStream.Read(s[1],Count);
  Result:=dbgMemRange(PByte(s),Count);
end;

function dbgObjMem(AnObject: TObject): string;
begin
  Result:='';
  if AnObject=nil then exit;
  Result:=dbgMemRange(PByte(AnObject),AnObject.InstanceSize);
end;

function dbghex(i: Int64): string;
const
  Hex = '0123456789ABCDEF';
var
  Negated: Boolean;
begin
  Result:='';
  if i<0 then begin
    Negated:=true;
    i:=-i;
  end else
    Negated:=false;
  repeat
    Result:=Hex[(i mod 16)+1]+Result;
    i:=i div 16;
  until i=0;
  if Negated then
    Result:='-'+Result;
end;

function DbgS(const i1, i2, i3, i4: integer): string;
begin
  Result:=dbgs(i1)+','+dbgs(i2)+','+dbgs(i3)+','+dbgs(i4);
end;

function DbgS(const Shift: TShiftState): string;

  procedure Add(const s: string);
  begin
    if Result<>'' then Result:=Result+',';
    Result:=Result+s;
  end;

begin
  Result:='';
  if ssShift in Shift then Add('ssShift');
  if ssAlt in Shift then Add('ssAlt');
  if ssCtrl in Shift then Add('ssCtrl');
  if ssLeft in Shift then Add('ssLeft');
  if ssRight in Shift then Add('ssRight');
  if ssMiddle in Shift then Add('ssMiddle');
  if ssDouble in Shift then Add('ssDouble');
  if ssMeta in Shift then Add('ssMeta');
  if ssSuper in Shift then Add('ssSuper');
  if ssHyper in Shift then Add('ssHyper');
  if ssAltGr in Shift then Add('ssAltGr');
  if ssCaps in Shift then Add('ssCaps');
  if ssNum in Shift then Add('ssNum');
  if ssScroll in Shift then Add('ssScroll');
  if ssTriple in Shift then Add('ssTriple');
  if ssQuad in Shift then Add('ssQuad');
  Result:='['+Result+']';
end;

function DbgS(const ASize: TSize): string;
begin
   Result := 'cx: ' + DbgS(ASize.cx) + ' cy: ' + DbgS(ASize.cy);
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

function TLazLoggerLogGroupList.Add(const AConfigName: String;
  ADefaulEnabled: Boolean): PLazLoggerLogGroup;
begin
  New(Result);
  FList.Add(Result);
  Result^.ConfigName := UpperCase(AConfigName);
  Result^.Enabled := ADefaulEnabled;
  Result^.Flags := [];
  Result^.FOpenedIndents := 0;
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

procedure TLazLogger.SetEnvironmentForLogFileName(AValue: String);
begin
  if FEnvironmentForLogFileName = AValue then Exit;
  Finish;
  FGetLogFileNameDone := False;
  FEnvironmentForLogFileName := AValue;
end;

procedure TLazLogger.SetCloseLogFileBetweenWrites(AValue: Boolean);
begin
  if FCloseLogFileBetweenWrites = AValue then Exit;
  FCloseLogFileBetweenWrites := AValue;
  if FCloseLogFileBetweenWrites and FLogTextInUse then
    DoCloseFile;
end;

procedure TLazLogger.SetAutoDestroy(AValue: Boolean);
begin
  if (FAutoDestroy = AValue) then Exit;
  FAutoDestroy := AValue and (Self = TheLazLogger);
  if FAutoDestroy then
    TheLazLoggerAutoFree := Self;
end;

function TLazLogger.GetLogGroupList: TLazLoggerLogGroupList;
begin
  if FLogGroupList = nil then
    FLogGroupList := TLazLoggerLogGroupList.Create;
  Result := TLazLoggerLogGroupList(FLogGroupList);
end;

procedure TLazLogger.SetParamForLogFileName(AValue: String);
begin
  if FParamForLogFileName = AValue then Exit;
  Finish;
  FGetLogFileNameDone := False;
  FParamForLogFileName := AValue;
end;

procedure TLazLogger.SetParamForEnabledLogGroups(AValue: String);
begin
  if FParamForEnabledLogGroups = AValue then Exit;
  FParamForEnabledLogGroups := AValue;
  ParseParamForEnabledLogGroups;
end;

procedure TLazLogger.ParseParamForEnabledLogGroups;
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

procedure TLazLogger.SetLogName(AValue: String);
begin
  if FLogName = AValue then Exit;
  Finish;
  FLogName := AValue;
end;

procedure TLazLogger.SetMaxNestPrefixLen(AValue: Integer);
begin
  if FMaxNestPrefixLen = AValue then Exit;
  FMaxNestPrefixLen := AValue;
  CreateIndent;
end;

procedure TLazLogger.SetNestLvlIndent(AValue: Integer);
begin
  if FNestLvlIndent = AValue then Exit;
  FNestLvlIndent := AValue;
  CreateIndent;
end;

function TLazLogger.GetParamByNameCount(const AName: String): integer;
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

function TLazLogger.GetParamByName(const AName: String; AnIndex: Integer): string;
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

procedure TLazLogger.DoInit;
begin
  FActiveLogText := nil;
  FDebugNestLvl := 0;
  FLogTextFailed := False;
  FDebugNestAtBOL := True;
  if (FLogName = '') and not FGetLogFileNameDone then
    FLogName := GetLogFileName;

  if not FCloseLogFileBetweenWrites then
    DoOpenFile;
end;

procedure TLazLogger.DoFinsh;
begin
  DoCloseFile;
  FLogTextFailed := False;
end;

procedure TLazLogger.DoOpenFile;
var
  fm: Byte;
begin
  if FActiveLogText <> nil then exit;

  if (not FLogTextFailed) and (length(FLogName)>0)
     {$ifNdef WinCE}
     and (DirPathExists(ExtractFileDir(FLogName)))
     {$endif}
  then begin
    fm:=Filemode;
    try
      {$ifdef WinCE}
        Assign(FLogText, FLogName);
        {$I-}
        Append(FLogText);
        if IOResult <> 0 then
          Rewrite(FLogText);
        {$I+}
      {$else}
        Filemode:=fmShareDenyNone;
        Assign(FLogText, FLogName);
        if FileExistsUTF8(FLogName) then
          Append(FLogText)
        else
          Rewrite(FLogText);
      {$endif}
      FActiveLogText := @FLogText;
      FLogTextInUse := true;
    except
      FLogTextInUse := false;
      FActiveLogText := nil;
      FLogTextFailed := True;
      // Add extra line ending: a dialog will be shown in windows gui application
      writeln(StdOut, 'Cannot open file: ', FLogName+LineEnding);
    end;
    Filemode:=fm;
  end;

  if (not FLogTextInUse) and (FUseStdOut) then
  begin
    if not(TextRec(Output).Mode=fmClosed) then
      FActiveLogText := @Output;
  end;
end;

procedure TLazLogger.DoCloseFile;
begin
  if FLogTextInUse then begin
    try
      Close(FLogText);
    except
    end;
    FLogTextInUse := false;
  end;
  FActiveLogText := nil;
end;

function TLazLogger.GetLogFileName: string;
var
  EnvVarName: string;
begin
  Result := '';
  FGetLogFileNameDone := True;
  if FParamForLogFileName <> '' then begin
    // first try to find the log file name in the command line parameters
    Result := GetParamByName(FParamForLogFileName);
  end;
  if FEnvironmentForLogFileName <> '' then begin;
    // if not found yet, then try to find in the environment variables
    if (length(result)=0) then begin
      EnvVarName:= ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'') + FEnvironmentForLogFileName;
      Result := GetEnvironmentVariableUTF8(EnvVarName);
    end;
  end;
  if (length(result)>0) then
    Result := ExpandFileNameUTF8(Result);
end;

procedure TLazLogger.WriteToFile(const s: string);
begin
  DoOpenFile;
  if FActiveLogText = nil then exit;

  Write(FActiveLogText^, s);

  if FCloseLogFileBetweenWrites then
    DoCloseFile;
end;

procedure TLazLogger.WriteLnToFile(const s: string);
begin
  DoOpenFile;
  if FActiveLogText = nil then exit;

  WriteLn(FActiveLogText^, s);

  if FCloseLogFileBetweenWrites then
    DoCloseFile;
end;

procedure TLazLogger.IncreaseIndent;
begin
  inc(FDebugNestLvl);
  CreateIndent;
end;

procedure TLazLogger.DecreaseIndent;
begin
  //if not DebugNestAtBOL then DebugLn;

  if FDebugNestLvl > 0 then
    dec(FDebugNestLvl);
  CreateIndent;
end;

procedure TLazLogger.IncreaseIndent(LogGroup: PLazLoggerLogGroup);
begin
  if (LogGroup <> nil) then begin
    if (not LogGroup^.Enabled) then exit;
    inc(LogGroup^.FOpenedIndents);
    IncreaseIndent;
  end
  else
    IncreaseIndent;
end;

procedure TLazLogger.DecreaseIndent(LogGroup: PLazLoggerLogGroup);
begin
  if (LogGroup <> nil) then begin
    // close what was opened, even if now disabled
    // only close, if opened by this group
    if (LogGroup^.FOpenedIndents <= 0) then exit;
    dec(LogGroup^.FOpenedIndents);
    DecreaseIndent;
  end
  else
    DecreaseIndent;
end;

procedure TLazLogger.CreateIndent;
var
  s: String;
  NewLen: Integer;
begin
  NewLen := FDebugNestLvl * FNestLvlIndent;
  if NewLen < 0 then NewLen := 0;
  if (NewLen >= FMaxNestPrefixLen) then begin
    s := IntToStr(FDebugNestLvl);
    NewLen := FMaxNestPrefixLen - Length(s);
    if NewLen < 1 then
      NewLen := 1;
  end else
    s := '';

  if NewLen <> Length(FDebugIndent) then
    FDebugIndent := s + StringOfChar(' ', NewLen);
end;

procedure TLazLogger.DoDbgOut(const s: string);
var
  Handled: Boolean;
begin
  if not IsInitialized then Init;

  if FOnDbgOut <> nil then
  begin
    Handled := False;
    if FDebugNestAtBOL and (s <> '') then
      FOnDbgOut(Self, FDebugIndent + s, Handled)
    else
      FOnDbgOut(Self, s, Handled);
    if Handled then
      Exit;
  end;

  if FDebugNestAtBOL and (s <> '') then
    WriteToFile(FDebugIndent + s)
  else
    WriteToFile(s);
  FDebugNestAtBOL := (s = '') or (s[length(s)] in [#10,#13]);
end;

procedure TLazLogger.DoDebugLn(const s: string);
var
  Handled: Boolean;
begin
  if not IsInitialized then Init;

  if FOnDebugLn <> nil then
  begin
    Handled := False;
    if FDebugNestAtBOL and (s <> '') then
      FOnDebugLn(Self, FDebugIndent + s, Handled)
    else
      FOnDebugLn(Self, s, Handled);
    if Handled then
      Exit;
  end;

  if FDebugNestAtBOL and (s <> '') then
    WriteLnToFile(FDebugIndent + ConvertLineEndings(s))
  else
    WriteLnToFile(ConvertLineEndings(s));
  FDebugNestAtBOL := True;
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
  FAutoDestroy := False;
  FIsInitialized := False;
  FLogTextInUse := False;
  FLogTextFailed := False;
  FDebugNestLvl := 0;
  FMaxNestPrefixLen := 15;
  FNestLvlIndent := 2;
  {$ifdef WinCE}
  FLogFileFromParam := '';
  FLogFileFromEnv   := '';
  FLogName := ExtractFilePath(ParamStr(0)) + Str_LCL_Debug_File;
  FUseStdOut := False;
  FCloseLogFileBetweenWrites := True;
  {$else}
  FParamForLogFileName := '--debug-log=';
  FEnvironmentForLogFileName   := '*_debuglog';
  FLogName := '';
  FUseStdOut := True;
  FCloseLogFileBetweenWrites := False;
  {$endif}
  FLogGroupList := nil;
  FLogDefaultEnabled := False;
  FLogAllDefaultDisabled := FAlse;
end;

destructor TLazLogger.Destroy;
begin
  Finish;
  if TheLazLogger = Self then TheLazLogger := nil;
  FreeAndNil(FLogGroupList);
  inherited Destroy;
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

function TLazLogger.RegisterLogGroup(const AConfigName: String): PLazLoggerLogGroup;
var
  Default, DefaultFound: Boolean;
begin
  if (not FLogParamParsed) and (FParamForEnabledLogGroups <> '') then
    ParseParamForEnabledLogGroups;

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

function TLazLogger.FindOrRegisterLogGroup(const AConfigName: String;
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

function TLazLogger.FindOrRegisterLogGroup(const AConfigName: String): PLazLoggerLogGroup;
begin
  Result := LogGroupList.Find(AConfigName);
  if Result = nil then
    Result := RegisterLogGroup(AConfigName)
  else
    Result^.Flags := Result^.Flags - [lgfAddedByParamParser];
end;

procedure TLazLogger.DebuglnStack(const s: string);
begin
  DebugLn(s);
  DoOpenFile;
  if FActiveLogText = nil then exit;

  Dump_Stack(FActiveLogText^, get_frame);

  if FCloseLogFileBetweenWrites then
    DoCloseFile;
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

finalization // Using TObject, so if none of the functions is used in the app, then even the rlass should be smart linked
  if (TheLazLoggerAutoFree <> nil) then
    FreeAndNil(TheLazLoggerAutoFree);

end.

