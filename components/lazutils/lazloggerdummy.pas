unit LazLoggerDummy;

{$mode objfpc}{$H+}

(*
  Provide an empty re-implementation of LazLoggerBase

  The aim is to allow
  - RegisterLogGroup
  - debugln
  to be called, and translate into empty method, or even inline no code at all

  Any function above that may be limited
*)

interface
uses
  Classes, SysUtils, FileUtil, types, LazClasses;

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


{$DEFINE USED_BY_LAZLOGGER_DUMMY}
{$I LazLoggerIntf.inc}

type
  (* All empty methods *)

  { TLazLoggerLogGroupList }

  TLazLoggerLogGroupList = class(TRefCountedObject)
  private
    function GetItem(Index: Integer): PLazLoggerLogGroup;
  public
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
    FMaxNestPrefixLen: Integer;
    FNestLvlIndent: Integer;
    FUseGlobalLogGroupList: Boolean;
    function GetLogGroupList: TLazLoggerLogGroupList;
    procedure SetMaxNestPrefixLen(AValue: Integer);
    procedure SetNestLvlIndent(AValue: Integer);
    procedure SetUseGlobalLogGroupList(AValue: Boolean);
  public
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

var // Using base TRefCountedObject, so if none of the functions is used in the app, then even the class should be smart linked
  TheLazLogger: TRefCountedObject = nil;
  TheLazLoggerGroups: TRefCountedObject = nil;

{$I LazLoggerImpl.inc}

procedure CreateDebugLogger;
begin
  if (TheLazLogger <> nil) then
    exit;
  if (TheLazLogger = nil) then
    TheLazLogger := TLazLogger.Create;
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
  Result := TLazLogger(TheLazLogger);
end;

procedure SetDebugLogger(ALogger: TLazLogger);
begin
end;

procedure RecreateDebugLogger;
begin
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
end;

function TLazLogger.GetLogGroupList: TLazLoggerLogGroupList;
begin
  Result := DebugLoggerGroups;
end;

procedure TLazLogger.SetMaxNestPrefixLen(AValue: Integer);
begin
  FMaxNestPrefixLen := AValue;
end;

procedure TLazLogger.SetNestLvlIndent(AValue: Integer);
begin
  FNestLvlIndent := AValue;
end;

procedure TLazLogger.SetUseGlobalLogGroupList(AValue: Boolean);
begin
  FUseGlobalLogGroupList := AValue;
end;

procedure TLazLogger.Assign(Src: TLazLogger);
begin

end;

procedure TLazLogger.Init;
begin

end;

procedure TLazLogger.Finish;
begin

end;

function TLazLogger.RegisterLogGroup(const AConfigName: String;
  ADefaulEnabled: Boolean): PLazLoggerLogGroup;
begin
  Result := nil;
end;

function TLazLogger.RegisterLogGroup(const AConfigName: String): PLazLoggerLogGroup;
begin
  Result := nil;
end;

function TLazLogger.FindOrRegisterLogGroup(const AConfigName: String;
  ADefaulEnabled: Boolean): PLazLoggerLogGroup;
begin
  Result := nil;
end;

function TLazLogger.FindOrRegisterLogGroup(const AConfigName: String): PLazLoggerLogGroup;
begin
  Result := nil;
end;

procedure TLazLogger.DebuglnStack(const s: string);
begin

end;

procedure TLazLogger.DbgOut(const s: string);
begin

end;

procedure TLazLogger.DbgOut(Args: array of const);
begin

end;

procedure TLazLogger.DbgOut(const S: String; Args: array of const);
begin

end;

procedure TLazLogger.DbgOut(const s1, s2: string; const s3: string; const s4: string;
  const s5: string; const s6: string; const s7: string; const s8: string; const s9: string;
  const s10: string; const s11: string; const s12: string; const s13: string;
  const s14: string; const s15: string; const s16: string; const s17: string;
  const s18: string);
begin

end;

procedure TLazLogger.DebugLn(const s: string);
begin

end;

procedure TLazLogger.DebugLn(Args: array of const);
begin

end;

procedure TLazLogger.DebugLn(const S: String; Args: array of const);
begin

end;

procedure TLazLogger.DebugLn(const s1, s2: string; const s3: string; const s4: string;
  const s5: string; const s6: string; const s7: string; const s8: string; const s9: string;
  const s10: string; const s11: string; const s12: string; const s13: string;
  const s14: string; const s15: string; const s16: string; const s17: string;
  const s18: string);
begin

end;

procedure TLazLogger.DebugLnEnter(const s: string);
begin

end;

procedure TLazLogger.DebugLnEnter(Args: array of const);
begin

end;

procedure TLazLogger.DebugLnEnter(s: string; Args: array of const);
begin

end;

procedure TLazLogger.DebugLnEnter(const s1, s2: string; const s3: string; const s4: string;
  const s5: string; const s6: string; const s7: string; const s8: string; const s9: string;
  const s10: string; const s11: string; const s12: string; const s13: string;
  const s14: string; const s15: string; const s16: string; const s17: string;
  const s18: string);
begin

end;

procedure TLazLogger.DebugLnExit(const s: string);
begin

end;

procedure TLazLogger.DebugLnExit(Args: array of const);
begin

end;

procedure TLazLogger.DebugLnExit(s: string; Args: array of const);
begin

end;

procedure TLazLogger.DebugLnExit(const s1, s2: string; const s3: string; const s4: string;
  const s5: string; const s6: string; const s7: string; const s8: string; const s9: string;
  const s10: string; const s11: string; const s12: string; const s13: string;
  const s14: string; const s15: string; const s16: string; const s17: string;
  const s18: string);
begin

end;

procedure TLazLogger.DebuglnStack(LogGroup: PLazLoggerLogGroup; const s: string);
begin

end;

procedure TLazLogger.DbgOut(LogGroup: PLazLoggerLogGroup; const s: string);
begin

end;

procedure TLazLogger.DbgOut(LogGroup: PLazLoggerLogGroup; Args: array of const);
begin

end;

procedure TLazLogger.DbgOut(LogGroup: PLazLoggerLogGroup; const S: String;
  Args: array of const);
begin

end;

procedure TLazLogger.DbgOut(LogGroup: PLazLoggerLogGroup; const s1, s2: string;
  const s3: string; const s4: string; const s5: string; const s6: string; const s7: string;
  const s8: string; const s9: string; const s10: string; const s11: string; const s12: string;
  const s13: string; const s14: string; const s15: string; const s16: string;
  const s17: string; const s18: string);
begin

end;

procedure TLazLogger.DebugLn(LogGroup: PLazLoggerLogGroup; const s: string);
begin

end;

procedure TLazLogger.DebugLn(LogGroup: PLazLoggerLogGroup; Args: array of const);
begin

end;

procedure TLazLogger.DebugLn(LogGroup: PLazLoggerLogGroup; const S: String;
  Args: array of const);
begin

end;

procedure TLazLogger.DebugLn(LogGroup: PLazLoggerLogGroup; const s1, s2: string;
  const s3: string; const s4: string; const s5: string; const s6: string; const s7: string;
  const s8: string; const s9: string; const s10: string; const s11: string; const s12: string;
  const s13: string; const s14: string; const s15: string; const s16: string;
  const s17: string; const s18: string);
begin

end;

procedure TLazLogger.DebugLnEnter(LogGroup: PLazLoggerLogGroup; const s: string);
begin

end;

procedure TLazLogger.DebugLnEnter(LogGroup: PLazLoggerLogGroup; Args: array of const);
begin

end;

procedure TLazLogger.DebugLnEnter(LogGroup: PLazLoggerLogGroup; s: string;
  Args: array of const);
begin

end;

procedure TLazLogger.DebugLnEnter(LogGroup: PLazLoggerLogGroup; const s1, s2: string;
  const s3: string; const s4: string; const s5: string; const s6: string; const s7: string;
  const s8: string; const s9: string; const s10: string; const s11: string; const s12: string;
  const s13: string; const s14: string; const s15: string; const s16: string;
  const s17: string; const s18: string);
begin

end;

procedure TLazLogger.DebugLnExit(LogGroup: PLazLoggerLogGroup; const s: string);
begin

end;

procedure TLazLogger.DebugLnExit(LogGroup: PLazLoggerLogGroup; Args: array of const);
begin

end;

procedure TLazLogger.DebugLnExit(LogGroup: PLazLoggerLogGroup; s: string;
  Args: array of const);
begin

end;

procedure TLazLogger.DebugLnExit(LogGroup: PLazLoggerLogGroup; const s1, s2: string;
  const s3: string; const s4: string; const s5: string; const s6: string; const s7: string;
  const s8: string; const s9: string; const s10: string; const s11: string; const s12: string;
  const s13: string; const s14: string; const s15: string; const s16: string;
  const s17: string; const s18: string);
begin

end;

{ TLazLoggerLogGroupList }

function TLazLoggerLogGroupList.GetItem(Index: Integer): PLazLoggerLogGroup;
begin
  Result := nil;
end;

procedure TLazLoggerLogGroupList.Assign(Src: TLazLoggerLogGroupList);
begin
end;

function TLazLoggerLogGroupList.IndexOf(const AConfigName: String): integer;
begin
  Result := -1;
end;

function TLazLoggerLogGroupList.IndexOf(const AnEntry: PLazLoggerLogGroup): integer;
begin
  Result := -1;
end;

function TLazLoggerLogGroupList.Find(const AConfigName: String): PLazLoggerLogGroup;
begin
  Result := nil;
end;

function TLazLoggerLogGroupList.Count: integer;
begin
  Result := 0;
end;

finalization // Using TObject, so if none of the functions is used in the app, then even the rlass should be smart linked
  ReleaseRefAndNil(TheLazLogger);
  ReleaseRefAndNil(TheLazLoggerGroups);

end.

