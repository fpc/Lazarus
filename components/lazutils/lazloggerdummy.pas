{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
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

  TLazLoggerLogEnabled = record end;

type

  TLazLoggerWriteEvent = procedure (Sender: TObject; S: string; var Handled: Boolean) of object;


{$DEFINE USED_BY_LAZLOGGER_DUMMY}
{$push}
{$HINTS off}
{$I LazLoggerIntf.inc}
{$pop}

type
  (* All empty methods *)

  { TLazLoggerBlockHandler
    called for DebuglnEnter / Exit
  }

  TLazLoggerBlockHandler = class
  public
    procedure IncreaseIndent; virtual; abstract;
    procedure DecreaseIndent; virtual; abstract;
  end;

  { TLazLoggerLogGroupList }

  TLazLoggerLogGroupList = class(TRefCountedObject)
  private
    function GetItem({%H-}Index: Integer): PLazLoggerLogGroup;
  public
    procedure Assign({%H-}Src: TLazLoggerLogGroupList);
    function  IndexOf(const {%H-}AConfigName: String): integer;
    function  IndexOf(const {%H-}AnEntry: PLazLoggerLogGroup): integer;
    function  Find(const {%H-}AConfigName: String): PLazLoggerLogGroup;
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
    procedure Assign({%H-}Src: TLazLogger); virtual;
    procedure Init;
    procedure Finish;

    property  NestLvlIndent: Integer read FNestLvlIndent write SetNestLvlIndent;
    property  MaxNestPrefixLen: Integer read FMaxNestPrefixLen write SetMaxNestPrefixLen;

  public
    function  RegisterLogGroup(const {%H-}AConfigName: String; {%H-}ADefaulEnabled: Boolean) : PLazLoggerLogGroup; virtual;
    function  RegisterLogGroup(const {%H-}AConfigName: String) : PLazLoggerLogGroup; virtual;
    function  FindOrRegisterLogGroup(const {%H-}AConfigName: String; {%H-}ADefaulEnabled: Boolean) : PLazLoggerLogGroup; virtual;
    function  FindOrRegisterLogGroup(const {%H-}AConfigName: String) : PLazLoggerLogGroup; virtual;
    property  LogGroupList: TLazLoggerLogGroupList read GetLogGroupList;
    property  UseGlobalLogGroupList: Boolean read FUseGlobalLogGroupList write SetUseGlobalLogGroupList;
  public
    procedure DebuglnStack(const {%H-}s: string = '');

    procedure DbgOut(const {%H-}s: string = ''); overload;
    procedure DbgOut({%H-}Args: array of const); overload;
    procedure DbgOut(const {%H-}S: String; {%H-}Args: array of const); overload;// similar to Format(s,Args)
    procedure DbgOut(const {%H-}s1, {%H-}s2: string; const {%H-}s3: string = '';
                     const {%H-}s4: string = ''; const {%H-}s5: string = ''; const {%H-}s6: string = '';
                     const {%H-}s7: string = ''; const {%H-}s8: string = ''; const {%H-}s9: string = '';
                     const {%H-}s10: string = ''; const {%H-}s11: string = ''; const {%H-}s12: string = '';
                     const {%H-}s13: string = ''; const {%H-}s14: string = ''; const {%H-}s15: string = '';
                     const {%H-}s16: string = ''; const {%H-}s17: string = ''; const {%H-}s18: string = ''); overload;

    procedure DebugLn(const {%H-}s: string = ''); overload;
    procedure DebugLn({%H-}Args: array of const); overload;
    procedure DebugLn(const {%H-}S: String; {%H-}Args: array of const); overload;// similar to Format(s,Args)
    procedure DebugLn(const {%H-}s1, {%H-}s2: string; const {%H-}s3: string = '';
                      const {%H-}s4: string = ''; const {%H-}s5: string = ''; const {%H-}s6: string = '';
                      const {%H-}s7: string = ''; const {%H-}s8: string = ''; const {%H-}s9: string = '';
                      const {%H-}s10: string = ''; const {%H-}s11: string = ''; const {%H-}s12: string = '';
                      const {%H-}s13: string = ''; const {%H-}s14: string = ''; const {%H-}s15: string = '';
                      const {%H-}s16: string = ''; const {%H-}s17: string = ''; const {%H-}s18: string = ''); overload;

    procedure DebugLnEnter(const {%H-}s: string = ''); overload;
    procedure DebugLnEnter({%H-}Args: array of const); overload;
    procedure DebugLnEnter({%H-}s: string; {%H-}Args: array of const); overload;
    procedure DebugLnEnter(const {%H-}s1, {%H-}s2: string; const {%H-}s3: string = '';
                           const {%H-}s4: string = ''; const {%H-}s5: string = ''; const {%H-}s6: string = '';
                           const {%H-}s7: string = ''; const {%H-}s8: string = ''; const {%H-}s9: string = '';
                           const {%H-}s10: string = ''; const {%H-}s11: string = ''; const {%H-}s12: string = '';
                           const {%H-}s13: string = ''; const {%H-}s14: string = ''; const {%H-}s15: string = '';
                           const {%H-}s16: string = ''; const {%H-}s17: string = ''; const {%H-}s18: string = ''); overload;

    procedure DebugLnExit(const {%H-}s: string = ''); overload;
    procedure DebugLnExit({%H-}Args: array of const); overload;
    procedure DebugLnExit({%H-}s: string; {%H-}Args: array of const); overload;
    procedure DebugLnExit(const {%H-}s1, {%H-}s2: string; const {%H-}s3: string = '';
                          const {%H-}s4: string = ''; const {%H-}s5: string = ''; const {%H-}s6: string = '';
                          const {%H-}s7: string = ''; const {%H-}s8: string = ''; const {%H-}s9: string = '';
                          const {%H-}s10: string = ''; const {%H-}s11: string = ''; const {%H-}s12: string = '';
                          const {%H-}s13: string = ''; const {%H-}s14: string = ''; const {%H-}s15: string = '';
                          const {%H-}s16: string = ''; const {%H-}s17: string = ''; const {%H-}s18: string = ''); overload;


    procedure DebuglnStack({%H-}LogEnabled: TLazLoggerLogEnabled; const {%H-}s: string = '');

    procedure DbgOut({%H-}LogEnabled: TLazLoggerLogEnabled; const {%H-}s: string = ''); overload;
    procedure DbgOut({%H-}LogEnabled: TLazLoggerLogEnabled; {%H-}Args: array of const); overload;
    procedure DbgOut({%H-}LogEnabled: TLazLoggerLogEnabled; const {%H-}S: String; {%H-}Args: array of const); overload;// similar to Format(s,Args)
    procedure DbgOut({%H-}LogEnabled: TLazLoggerLogEnabled; const {%H-}s1, {%H-}s2: string; const {%H-}s3: string = '';
                     const {%H-}s4: string = ''; const {%H-}s5: string = ''; const {%H-}s6: string = '';
                     const {%H-}s7: string = ''; const {%H-}s8: string = ''; const {%H-}s9: string = '';
                     const {%H-}s10: string = ''; const {%H-}s11: string = ''; const {%H-}s12: string = '';
                     const {%H-}s13: string = ''; const {%H-}s14: string = ''; const {%H-}s15: string = '';
                     const {%H-}s16: string = ''; const {%H-}s17: string = ''; const {%H-}s18: string = ''); overload;

    procedure DebugLn({%H-}LogEnabled: TLazLoggerLogEnabled; const {%H-}s: string = ''); overload;
    procedure DebugLn({%H-}LogEnabled: TLazLoggerLogEnabled; {%H-}Args: array of const); overload;
    procedure DebugLn({%H-}LogEnabled: TLazLoggerLogEnabled; const {%H-}S: String; {%H-}Args: array of const); overload;// similar to Format(s,Args)
    procedure DebugLn({%H-}LogEnabled: TLazLoggerLogEnabled; const {%H-}s1, {%H-}s2: string; const {%H-}s3: string = '';
                      const {%H-}s4: string = ''; const {%H-}s5: string = ''; const {%H-}s6: string = '';
                      const {%H-}s7: string = ''; const {%H-}s8: string = ''; const {%H-}s9: string = '';
                      const {%H-}s10: string = ''; const {%H-}s11: string = ''; const {%H-}s12: string = '';
                      const {%H-}s13: string = ''; const {%H-}s14: string = ''; const {%H-}s15: string = '';
                      const {%H-}s16: string = ''; const {%H-}s17: string = ''; const {%H-}s18: string = ''); overload;

    procedure DebugLnEnter({%H-}LogEnabled: TLazLoggerLogEnabled; const {%H-}s: string = ''); overload;
    procedure DebugLnEnter({%H-}LogEnabled: TLazLoggerLogEnabled; {%H-}Args: array of const); overload;
    procedure DebugLnEnter({%H-}LogEnabled: TLazLoggerLogEnabled; {%H-}s: string; {%H-}Args: array of const); overload;
    procedure DebugLnEnter({%H-}LogEnabled: TLazLoggerLogEnabled; const {%H-}s1, {%H-}s2: string; const {%H-}s3: string = '';
                           const {%H-}s4: string = ''; const {%H-}s5: string = ''; const {%H-}s6: string = '';
                           const {%H-}s7: string = ''; const {%H-}s8: string = ''; const {%H-}s9: string = '';
                           const {%H-}s10: string = ''; const {%H-}s11: string = ''; const {%H-}s12: string = '';
                           const {%H-}s13: string = ''; const {%H-}s14: string = ''; const {%H-}s15: string = '';
                           const {%H-}s16: string = ''; const {%H-}s17: string = ''; const {%H-}s18: string = ''); overload;

    procedure DebugLnExit({%H-}LogEnabled: TLazLoggerLogEnabled; const {%H-}s: string = ''); overload;
    procedure DebugLnExit({%H-}LogEnabled: TLazLoggerLogEnabled; {%H-}Args: array of const); overload;
    procedure DebugLnExit({%H-}LogEnabled: TLazLoggerLogEnabled; {%H-}s: string; {%H-}Args: array of const); overload;
    procedure DebugLnExit({%H-}LogEnabled: TLazLoggerLogEnabled; const {%H-}s1, {%H-}s2: string; const {%H-}s3: string = '';
                          const {%H-}s4: string = ''; const {%H-}s5: string = ''; const {%H-}s6: string = '';
                          const {%H-}s7: string = ''; const {%H-}s8: string = ''; const {%H-}s9: string = '';
                          const {%H-}s10: string = ''; const {%H-}s11: string = ''; const {%H-}s12: string = '';
                          const {%H-}s13: string = ''; const {%H-}s14: string = ''; const {%H-}s15: string = '';
                          const {%H-}s16: string = ''; const {%H-}s17: string = ''; const {%H-}s18: string = ''); overload;

  end;


function GetDebugLoggerGroups: TLazLoggerLogGroupList; inline;
procedure SetDebugLoggerGroups({%H-}ALogGroups: TLazLoggerLogGroupList);

function GetDebugLogger: TLazLogger; inline;
function GetExistingDebugLogger: TLazLogger; inline; // No Autocreate
procedure SetDebugLogger({%H-}ALogger: TLazLogger);

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

{$push}
{$HINTS off}
{$I LazLoggerImpl.inc}
{$pop}

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

procedure TLazLogger.DebuglnStack(LogEnabled: TLazLoggerLogEnabled; const s: string);
begin

end;

procedure TLazLogger.DbgOut(LogEnabled: TLazLoggerLogEnabled; const s: string);
begin

end;

procedure TLazLogger.DbgOut(LogEnabled: TLazLoggerLogEnabled; Args: array of const);
begin

end;

procedure TLazLogger.DbgOut(LogEnabled: TLazLoggerLogEnabled; const S: String;
  Args: array of const);
begin

end;

procedure TLazLogger.DbgOut(LogEnabled: TLazLoggerLogEnabled; const s1, s2: string;
  const s3: string; const s4: string; const s5: string; const s6: string; const s7: string;
  const s8: string; const s9: string; const s10: string; const s11: string; const s12: string;
  const s13: string; const s14: string; const s15: string; const s16: string;
  const s17: string; const s18: string);
begin

end;

procedure TLazLogger.DebugLn(LogEnabled: TLazLoggerLogEnabled; const s: string);
begin

end;

procedure TLazLogger.DebugLn(LogEnabled: TLazLoggerLogEnabled; Args: array of const);
begin

end;

procedure TLazLogger.DebugLn(LogEnabled: TLazLoggerLogEnabled; const S: String;
  Args: array of const);
begin

end;

procedure TLazLogger.DebugLn(LogEnabled: TLazLoggerLogEnabled; const s1, s2: string;
  const s3: string; const s4: string; const s5: string; const s6: string; const s7: string;
  const s8: string; const s9: string; const s10: string; const s11: string; const s12: string;
  const s13: string; const s14: string; const s15: string; const s16: string;
  const s17: string; const s18: string);
begin

end;

procedure TLazLogger.DebugLnEnter(LogEnabled: TLazLoggerLogEnabled; const s: string);
begin

end;

procedure TLazLogger.DebugLnEnter(LogEnabled: TLazLoggerLogEnabled; Args: array of const);
begin

end;

procedure TLazLogger.DebugLnEnter(LogEnabled: TLazLoggerLogEnabled; s: string;
  Args: array of const);
begin

end;

procedure TLazLogger.DebugLnEnter(LogEnabled: TLazLoggerLogEnabled; const s1, s2: string;
  const s3: string; const s4: string; const s5: string; const s6: string; const s7: string;
  const s8: string; const s9: string; const s10: string; const s11: string; const s12: string;
  const s13: string; const s14: string; const s15: string; const s16: string;
  const s17: string; const s18: string);
begin

end;

procedure TLazLogger.DebugLnExit(LogEnabled: TLazLoggerLogEnabled; const s: string);
begin

end;

procedure TLazLogger.DebugLnExit(LogEnabled: TLazLoggerLogEnabled; Args: array of const);
begin

end;

procedure TLazLogger.DebugLnExit(LogEnabled: TLazLoggerLogEnabled; s: string;
  Args: array of const);
begin

end;

procedure TLazLogger.DebugLnExit(LogEnabled: TLazLoggerLogEnabled; const s1, s2: string;
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

