{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit LazLoggerBase;

{$mode objfpc}{$H+}

(*
  - All global variables, initialization and finalization use TObject instead
    of TLazLogger.
    This means: using the unit (without calling any of the functions) will not
    make any reference to the classes, and they should be smart-linked away.
*)

interface

uses
  Classes, SysUtils, types, math,
  // LazUtils
  LazClasses, LazUTF8;

type

  TLazLoggerLogGroupFlag =
  ( lgfAddedByParamParser,        // Not added via Register. This is a placeholder for the enabled-state given by the user, via command line
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

  TLazLoggerLogEnabled = record
    Enabled: Boolean;
    Group: PLazLoggerLogGroup; // if only one group / remember nestlevel count
  end;

  TLazLoggerWriteTarget = (
    lwtNone,
    lwtStdOut, lwtStdErr,
    lwtTextFile  // Data will be ^Text
  );

  TLazLoggerWriteEvent = procedure(Sender: TObject; S: string; var Handled: Boolean) of object;
  TLazLoggerWidgetSetWriteEvent = procedure(Sender: TObject;
      S: string;
      var Handled: Boolean;
      Target: TLazLoggerWriteTarget;
      Data: Pointer) of object;

type

  TLazLogger = class;

  { TLazLoggerBlockHandler
    called for DebuglnEnter / Exit
  }

  TLazLoggerBlockHandler = class(TRefCountedObject)
  public
    procedure EnterBlock(Sender: TLazLogger; Level: Integer); virtual; abstract;
    procedure ExitBlock(Sender: TLazLogger; Level: Integer); virtual; abstract;
  end;

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
    FLoggerCriticalSection: TRTLCriticalSection;
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
    procedure DoFinish; virtual;
    procedure DoFinsh; deprecated 'Use DoFinish'; // Deprecated in 2.1 / 30.04.2020 / Remove in 2.3

    procedure IncreaseIndent; overload; virtual;
    procedure DecreaseIndent; overload; virtual;
    procedure IncreaseIndent({%H-}LogEnabled: TLazLoggerLogEnabled); overload; virtual;
    procedure DecreaseIndent({%H-}LogEnabled: TLazLoggerLogEnabled); overload; virtual;
    procedure IndentChanged; virtual;
    function  GetBlockHandler({%H-}AIndex: Integer): TLazLoggerBlockHandler; virtual;

    procedure DoDbgOut({%H-}s: string); virtual;
    procedure DoDebugLn({%H-}s: string); virtual;
    procedure DoDebuglnStack(const {%H-}s: string); virtual;

    function  ArgsToString(Args: array of const): string;
    property  IsInitialized: Boolean read FIsInitialized;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Assign(Src: TLazLogger); virtual;
    procedure Init;
    procedure Finish;

    function  CurrentIndentLevel: Integer; virtual;
    property  NestLvlIndent: Integer read FNestLvlIndent write SetNestLvlIndent;
    property  MaxNestPrefixLen: Integer read FMaxNestPrefixLen write SetMaxNestPrefixLen;

  public
    function  RegisterLogGroup(const AConfigName: String; ADefaulEnabled: Boolean) : PLazLoggerLogGroup; virtual;
    function  RegisterLogGroup(const AConfigName: String) : PLazLoggerLogGroup; virtual;
    function  FindOrRegisterLogGroup(const AConfigName: String; ADefaulEnabled: Boolean) : PLazLoggerLogGroup; virtual;
    function  FindOrRegisterLogGroup(const AConfigName: String) : PLazLoggerLogGroup; virtual;
    property  LogGroupList: TLazLoggerLogGroupList read GetLogGroupList;
    property  UseGlobalLogGroupList: Boolean read FUseGlobalLogGroupList write SetUseGlobalLogGroupList;

    procedure AddBlockHandler({%H-}AHandler: TLazLoggerBlockHandler); virtual;
    procedure RemoveBlockHandler({%H-}AHandler: TLazLoggerBlockHandler); virtual;
    function  BlockHandlerCount: Integer; virtual;
    property  BlockHandler[AIndex: Integer]: TLazLoggerBlockHandler read GetBlockHandler;
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

    procedure DebugLnEnter(); overload;
    procedure DebugLnEnter(const s: string); overload;
    procedure DebugLnEnter(Args: array of const); overload;
    procedure DebugLnEnter(s: string; Args: array of const); overload;
    procedure DebugLnEnter(const s1, s2: string; const s3: string = '';
                           const s4: string = ''; const s5: string = ''; const s6: string = '';
                           const s7: string = ''; const s8: string = ''; const s9: string = '';
                           const s10: string = ''; const s11: string = ''; const s12: string = '';
                           const s13: string = ''; const s14: string = ''; const s15: string = '';
                           const s16: string = ''; const s17: string = ''; const s18: string = ''); overload;

    procedure DebugLnExit(); overload;
    procedure DebugLnExit(const s: string); overload;
    procedure DebugLnExit(Args: array of const); overload;
    procedure DebugLnExit(s: string; Args: array of const); overload;
    procedure DebugLnExit(const s1, s2: string; const s3: string = '';
                          const s4: string = ''; const s5: string = ''; const s6: string = '';
                          const s7: string = ''; const s8: string = ''; const s9: string = '';
                          const s10: string = ''; const s11: string = ''; const s12: string = '';
                          const s13: string = ''; const s14: string = ''; const s15: string = '';
                          const s16: string = ''; const s17: string = ''; const s18: string = ''); overload;


    procedure DebuglnStack(LogEnabled: TLazLoggerLogEnabled; const s: string = '');

    procedure DbgOut(LogEnabled: TLazLoggerLogEnabled; const s: string = ''); overload;
    procedure DbgOut(LogEnabled: TLazLoggerLogEnabled; Args: array of const); overload;
    procedure DbgOut(LogEnabled: TLazLoggerLogEnabled; const S: String; Args: array of const); overload;// similar to Format(s,Args)
    procedure DbgOut(LogEnabled: TLazLoggerLogEnabled; const s1, s2: string; const s3: string = '';
                     const s4: string = ''; const s5: string = ''; const s6: string = '';
                     const s7: string = ''; const s8: string = ''; const s9: string = '';
                     const s10: string = ''; const s11: string = ''; const s12: string = '';
                     const s13: string = ''; const s14: string = ''; const s15: string = '';
                     const s16: string = ''; const s17: string = ''; const s18: string = ''); overload;

    procedure DebugLn(LogEnabled: TLazLoggerLogEnabled; const s: string = ''); overload;
    procedure DebugLn(LogEnabled: TLazLoggerLogEnabled; Args: array of const); overload;
    procedure DebugLn(LogEnabled: TLazLoggerLogEnabled; const S: String; Args: array of const); overload;// similar to Format(s,Args)
    procedure DebugLn(LogEnabled: TLazLoggerLogEnabled; const s1, s2: string; const s3: string = '';
                      const s4: string = ''; const s5: string = ''; const s6: string = '';
                      const s7: string = ''; const s8: string = ''; const s9: string = '';
                      const s10: string = ''; const s11: string = ''; const s12: string = '';
                      const s13: string = ''; const s14: string = ''; const s15: string = '';
                      const s16: string = ''; const s17: string = ''; const s18: string = ''); overload;

    procedure DebugLnEnter(LogEnabled: TLazLoggerLogEnabled); overload;
    procedure DebugLnEnter(LogEnabled: TLazLoggerLogEnabled; const s: string); overload;
    procedure DebugLnEnter(LogEnabled: TLazLoggerLogEnabled; Args: array of const); overload;
    procedure DebugLnEnter(LogEnabled: TLazLoggerLogEnabled; s: string; Args: array of const); overload;
    procedure DebugLnEnter(LogEnabled: TLazLoggerLogEnabled; const s1, s2: string; const s3: string = '';
                           const s4: string = ''; const s5: string = ''; const s6: string = '';
                           const s7: string = ''; const s8: string = ''; const s9: string = '';
                           const s10: string = ''; const s11: string = ''; const s12: string = '';
                           const s13: string = ''; const s14: string = ''; const s15: string = '';
                           const s16: string = ''; const s17: string = ''; const s18: string = ''); overload;

    procedure DebugLnExit(LogEnabled: TLazLoggerLogEnabled); overload;
    procedure DebugLnExit(LogEnabled: TLazLoggerLogEnabled; const s: string); overload;
    procedure DebugLnExit(LogEnabled: TLazLoggerLogEnabled; Args: array of const); overload;
    procedure DebugLnExit(LogEnabled: TLazLoggerLogEnabled; s: string; Args: array of const); overload;
    procedure DebugLnExit(LogEnabled: TLazLoggerLogEnabled; const s1, s2: string; const s3: string = '';
                          const s4: string = ''; const s5: string = ''; const s6: string = '';
                          const s7: string = ''; const s8: string = ''; const s9: string = '';
                          const s10: string = ''; const s11: string = ''; const s12: string = '';
                          const s13: string = ''; const s14: string = ''; const s15: string = '';
                          const s16: string = ''; const s17: string = ''; const s18: string = ''); overload;

    procedure DumpExceptionBackTrace;
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

function DbgStr(const StringWithSpecialChars: string): string; overload;
function DbgStr(const StringWithSpecialChars: string; StartPos, Len: PtrInt): string; overload;
function DbgStr(const p: PChar; Len: PtrInt): string; overload;
function DbgWideStr(const StringWithSpecialChars: widestring): string; overload;

procedure DumpStack; inline;

type
  TLazDebugLoggerCreator = function: TRefCountedObject;

// Using base TRefCountedObject, so if none of the functions is used in the app, then even the class should be smart linked
var
  LazDebugLoggerCreator: TLazDebugLoggerCreator = nil;
  OnWidgetSetDebugLn: TLazLoggerWidgetSetWriteEvent;
  OnWidgetSetDbgOut:  TLazLoggerWidgetSetWriteEvent;

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
  if TheLazLogger <> nil then
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
  Result := '';
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

function DbgStr(const StringWithSpecialChars: string): string;
var
  i: Integer;
  s: String;
  l: Integer;
begin
  Result:=StringWithSpecialChars;
  i:=1;
  while (i<=length(Result)) do begin
    case Result[i] of
    ' '..#126: inc(i);
    else
      s:='#'+HexStr(ord(Result[i]),2);
      // Note: do not use copy, fpc might change broken UTF-8 characters to '?'
      l:=length(Result)-i;
      SetLength(Result,length(Result)-1+length(s));
      if l>0 then
        system.Move(Result[i+1],Result[i+length(s)],l);
      system.Move(s[1],Result[i],length(s));
      inc(i,length(s));
    end;
  end;
end;

function DbgStr(const StringWithSpecialChars: string; StartPos, Len: PtrInt
  ): string;
begin
  Result:=dbgstr(copy(StringWithSpecialChars,StartPos,Len));
end;

function DbgStr(const p: PChar; Len: PtrInt): string;
const
  Hex: array[0..15] of char='0123456789ABCDEF';
var
  UsedLen: PtrInt;
  ResultLen: PtrInt;
  Src: PChar;
  Dest: PChar;
  c: Char;
begin
  if (p=nil) or (p^=#0) or (Len<=0) then exit('');
  UsedLen:=0;
  ResultLen:=0;
  Src:=p;
  while Src^<>#0 do begin
    inc(UsedLen);
    if Src^ in [' '..#126] then
      inc(ResultLen)
    else
      inc(ResultLen,3);
    if UsedLen>=Len then break;
    inc(Src);
  end;
  SetLength(Result,ResultLen);
  Src:=p;
  Dest:=PChar(Result);
  while UsedLen>0 do begin
    dec(UsedLen);
    c:=Src^;
    if c in [' '..#126] then begin
      Dest^:=c;
      inc(Dest);
    end else begin
      Dest^:='#';
      inc(Dest);
      Dest^:=Hex[ord(c) shr 4];
      inc(Dest);
      Dest^:=Hex[ord(c) and $f];
      inc(Dest);
    end;
    inc(Src);
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

procedure DumpStack;
begin
  DebuglnStack;
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

function TLazLogger.GetBlockHandler(AIndex: Integer): TLazLoggerBlockHandler;
begin
  Result := nil;;
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

procedure TLazLogger.DumpExceptionBackTrace;
  procedure DumpAddr(Addr: Pointer);
  begin
    // preventing another exception, while dumping stack trace
    try
      DebugLn(BackTraceStrFunc(Addr));
    except
      DebugLn(SysBackTraceStr(Addr));
    end;
  end;
var
  FrameCount: integer;
  Frames: PPointer;
  FrameNumber:Integer;
begin
  DumpAddr(ExceptAddr);
  FrameCount:=ExceptFrameCount;
  Frames:=ExceptFrames;
  for FrameNumber := 0 to FrameCount-1 do
    DumpAddr(Frames[FrameNumber]);
end;

procedure TLazLogger.DoFinish;
begin
  //
end;

procedure TLazLogger.DoFinsh;
begin
  DoFinish;
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

procedure TLazLogger.IncreaseIndent(LogEnabled: TLazLoggerLogEnabled);
begin
  //
end;

procedure TLazLogger.DecreaseIndent(LogEnabled: TLazLoggerLogEnabled);
begin
  //
end;

procedure TLazLogger.IndentChanged;
begin
  //
end;

procedure TLazLogger.DoDbgOut(s: string);
begin
  //
end;

procedure TLazLogger.DoDebugLn(s: string);
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
      vtPWideChar:  Result := {%H-}Result {%H-}+ Args[i].VPWideChar;
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
  InitCriticalSection(FLoggerCriticalSection);
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
  DoneCriticalsection(FLoggerCriticalSection);
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
  EnterCriticalsection(FLoggerCriticalSection);
  try
    if FIsInitialized then exit;
    DoInit;
    FIsInitialized := True;
  finally
    LeaveCriticalsection(FLoggerCriticalSection);
  end;
end;

procedure TLazLogger.Finish;
begin
  if FIsInitialized then
    DoFinish;
  FIsInitialized := False;
end;

function TLazLogger.CurrentIndentLevel: Integer;
begin
  Result := 0;
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

procedure TLazLogger.AddBlockHandler(AHandler: TLazLoggerBlockHandler);
begin
  //
end;

procedure TLazLogger.RemoveBlockHandler(AHandler: TLazLoggerBlockHandler);
begin
  //
end;

function TLazLogger.BlockHandlerCount: Integer;
begin
  Result := 0;
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

procedure TLazLogger.DebugLnEnter();
begin
  IncreaseIndent;
end;

procedure TLazLogger.DebugLnEnter(const s: string);
begin
  DoDebugLn(s);
  IncreaseIndent;
end;

procedure TLazLogger.DebugLnEnter(Args: array of const);
begin
  if high(Args) >= low(Args) then
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

procedure TLazLogger.DebugLnExit();
begin
  DecreaseIndent;
end;

procedure TLazLogger.DebugLnExit(const s: string);
begin
  DecreaseIndent;
  DoDebugLn(s);
end;

procedure TLazLogger.DebugLnExit(Args: array of const);
begin
  DecreaseIndent;
  if high(Args) >= low(Args) then
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

procedure TLazLogger.DebuglnStack(LogEnabled: TLazLoggerLogEnabled; const s: string);
begin
  if not LogEnabled.Enabled then exit;
  DebuglnStack(s);
end;

procedure TLazLogger.DbgOut(LogEnabled: TLazLoggerLogEnabled; const s: string);
begin
  if not LogEnabled.Enabled then exit;
  DoDbgOut(s);
end;

procedure TLazLogger.DbgOut(LogEnabled: TLazLoggerLogEnabled; Args: array of const);
begin
  if not LogEnabled.Enabled then exit;
  DoDbgOut(ArgsToString(Args));
end;

procedure TLazLogger.DbgOut(LogEnabled: TLazLoggerLogEnabled; const S: String;
  Args: array of const);
begin
  if not LogEnabled.Enabled then exit;
  DoDbgOut(Format(S, Args));
end;

procedure TLazLogger.DbgOut(LogEnabled: TLazLoggerLogEnabled; const s1, s2: string;
  const s3: string; const s4: string; const s5: string; const s6: string; const s7: string;
  const s8: string; const s9: string; const s10: string; const s11: string; const s12: string;
  const s13: string; const s14: string; const s15: string; const s16: string;
  const s17: string; const s18: string);
begin
  if not LogEnabled.Enabled then exit;
  DoDbgOut(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18);
end;

procedure TLazLogger.DebugLn(LogEnabled: TLazLoggerLogEnabled; const s: string);
begin
  if not LogEnabled.Enabled then exit;
  DoDebugLn(s);
end;

procedure TLazLogger.DebugLn(LogEnabled: TLazLoggerLogEnabled; Args: array of const);
begin
  if not LogEnabled.Enabled then exit;
  DoDebugLn(ArgsToString(Args));
end;

procedure TLazLogger.DebugLn(LogEnabled: TLazLoggerLogEnabled; const S: String;
  Args: array of const);
begin
  if not LogEnabled.Enabled then exit;
  DoDebugLn(Format(S, Args));
end;

procedure TLazLogger.DebugLn(LogEnabled: TLazLoggerLogEnabled; const s1, s2: string;
  const s3: string; const s4: string; const s5: string; const s6: string; const s7: string;
  const s8: string; const s9: string; const s10: string; const s11: string; const s12: string;
  const s13: string; const s14: string; const s15: string; const s16: string;
  const s17: string; const s18: string);
begin
  if not LogEnabled.Enabled then exit;
  DoDebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18);
end;

procedure TLazLogger.DebugLnEnter(LogEnabled: TLazLoggerLogEnabled);
begin
  IncreaseIndent(LogEnabled);
end;

procedure TLazLogger.DebugLnEnter(LogEnabled: TLazLoggerLogEnabled; const s: string);
begin
  if LogEnabled.Enabled then
    DoDebugLn(s);
  IncreaseIndent(LogEnabled);
end;

procedure TLazLogger.DebugLnEnter(LogEnabled: TLazLoggerLogEnabled; Args: array of const);
begin
  if LogEnabled.Enabled then
    DoDebugLn(ArgsToString(Args));
  IncreaseIndent(LogEnabled);
end;

procedure TLazLogger.DebugLnEnter(LogEnabled: TLazLoggerLogEnabled; s: string;
  Args: array of const);
begin
  if LogEnabled.Enabled then
    DoDebugLn(Format(S, Args));
  IncreaseIndent(LogEnabled);
end;

procedure TLazLogger.DebugLnEnter(LogEnabled: TLazLoggerLogEnabled; const s1, s2: string;
  const s3: string; const s4: string; const s5: string; const s6: string; const s7: string;
  const s8: string; const s9: string; const s10: string; const s11: string; const s12: string;
  const s13: string; const s14: string; const s15: string; const s16: string;
  const s17: string; const s18: string);
begin
  if LogEnabled.Enabled then
    DoDebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18);
  IncreaseIndent(LogEnabled);
end;

procedure TLazLogger.DebugLnExit(LogEnabled: TLazLoggerLogEnabled);
begin
  DecreaseIndent(LogEnabled);
end;

procedure TLazLogger.DebugLnExit(LogEnabled: TLazLoggerLogEnabled; const s: string);
begin
  DecreaseIndent(LogEnabled);
  if not LogEnabled.Enabled then exit;
  DoDebugLn(s);
end;

procedure TLazLogger.DebugLnExit(LogEnabled: TLazLoggerLogEnabled; Args: array of const);
begin
  DecreaseIndent(LogEnabled);
  if not LogEnabled.Enabled then exit;
  DoDebugLn(ArgsToString(Args));
end;

procedure TLazLogger.DebugLnExit(LogEnabled: TLazLoggerLogEnabled; s: string;
  Args: array of const);
begin
  DecreaseIndent(LogEnabled);
  if not LogEnabled.Enabled then exit;
  DoDebugLn(Format(S, Args));
end;

procedure TLazLogger.DebugLnExit(LogEnabled: TLazLoggerLogEnabled; const s1, s2: string;
  const s3: string; const s4: string; const s5: string; const s6: string; const s7: string;
  const s8: string; const s9: string; const s10: string; const s11: string; const s12: string;
  const s13: string; const s14: string; const s15: string; const s16: string;
  const s17: string; const s18: string);
begin
  DecreaseIndent(LogEnabled);
  if not LogEnabled.Enabled then exit;
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
var
  i: Integer;
begin
  inherited Assign(Src);
  if (Src <> nil) and (Src is TLazLoggerWithGroupParam) then begin
    FLogParamParsed := False;
    FParamForEnabledLogGroups := TLazLoggerWithGroupParam(Src).FParamForEnabledLogGroups;
  end;

  if (Src <> nil) then
    for i := 0 to Src.BlockHandlerCount - 1 do
      AddBlockHandler(Src.BlockHandler[i]);
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

finalization // Using TObject, so if none of the functions is used in the app, then even the rlass should be smart linked
  ReleaseRefAndNil(TheLazLogger);
  ReleaseRefAndNil(PrevLazLogger);
  ReleaseRefAndNil(TheLazLoggerGroups);

end.

