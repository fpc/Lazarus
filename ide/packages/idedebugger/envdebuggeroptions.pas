unit EnvDebuggerOptions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo,
  // LCL
  Graphics, Forms,
  // LazUtils
  LazFileUtils, FileUtil, LazFileCache, LazConfigStorage, LazUTF8, LazStringUtils,
  Laz2_XMLCfg, Laz2_DOM,
  // CodeTools
  FileProcs, SourceChanger, CodeCompletionTool,
  // IDEIntf
  ProjectIntf, ObjectInspector, IDEWindowIntf, IDEOptionsIntf, IDEOptEditorIntf,
  ComponentReg, IDEExternToolIntf, SrcEditorIntf,
  // DebuggerIntf
  DbgIntfDebuggerBase,
  // IdeConfig
  EnvironmentOpts, IDEOptionDefs, RecentListProcs, SearchPathProcs, LazConf, TransferMacros,
  ModeMatrixOpts, CoolBarOptions, EditorToolBarOptions;

type
  { Debugging }

  { TDebuggerConfigStore }
  (* TODO: maybe revert relations. Create this in Debugger, and call environmentoptions for the configstore only? *)

  { TDebuggerConfigStoreBase }

  TDebuggerConfigStoreBase = class(TPersistent)
  private
    FConfigStore: TConfigStorage;
  public
    property ConfigStore: TConfigStorage read FConfigStore write FConfigStore;
    procedure Init; virtual;
    procedure Load; virtual;
    procedure Save; virtual;
  end;

  //{ TDebuggerWatchesDlgConfig }
  //
  //TDebuggerWatchesDlgConfig = class(TDebuggerConfigStoreBase)
  //private
  //  FColumnNameWidth: Integer;
  //  FColumnValueWidth: Integer;
  //public
  //  constructor Create;
  //  procedure Init; override;
  //published
  //  property ColumnNameWidth: Integer read FColumnNameWidth write FColumnNameWidth;
  //  property ColumnValueWidth: Integer read FColumnValueWidth write FColumnValueWidth;
  //end;

  { TDebuggerCallStackDlgConfig }

  TDebuggerCallStackDlgConfig = class(TDebuggerConfigStoreBase)
  private
    FViewCount: Integer;
  public
    constructor Create;
    procedure Init; override;
  published
    property ViewCount: Integer read FViewCount write FViewCount;
  end;

  TDebuggerConfigStore = class(TDebuggerConfigStoreBase)
  private
    FDlgCallStackConfig: TDebuggerCallStackDlgConfig;
    //FTDebuggerWatchesDlgConfig: TDebuggerWatchesDlgConfig;
  public
    procedure Load; override;
    procedure Save; override;
  public
    constructor Create;
    destructor Destroy; override;
    //property DlgWatchesConfig: TDebuggerWatchesDlgConfig read FTDebuggerWatchesDlgConfig;
    property DlgCallStackConfig: TDebuggerCallStackDlgConfig read FDlgCallStackConfig write FDlgCallStackConfig;
  published
  end;

  TDebuggerEventLogColor = record
    Foreground: TColor;
    Background: TColor;
  end;

const
  DebuggerDefaultColors: array[TDBGEventType] of TDebuggerEventLogColor = (
{ etDefault              } (Foreground: clWindowText; Background: clWindow),
{ etBreakpointEvaluation } (Foreground: $8080FF;      Background: clWindow),
{ etBreakpointHit        } (Foreground: clRed;        Background: clWindow),
{ etBreakpointMessage    } (Foreground: $0000D9;      Background: clWindow),
{ etBreakpointStackDump  } (Foreground: $2080FF;      Background: clWindow),
{ etExceptionRaised      } (Foreground: clTeal;       Background: clWindow),
{ etModuleLoad           } (Foreground: clBlue;       Background: clWindow),
{ etModuleUnload         } (Foreground: clBlue;       Background: clWindow),
{ etOutputDebugString    } (Foreground: clNavy;       Background: clWindow),
{ etProcessExit          } (Foreground: clGray;       Background: clWindow),
{ etProcessStart         } (Foreground: clGray;       Background: clWindow),
{ etThreadExit           } (Foreground: clMaroon;     Background: clWindow),
{ etThreadStart          } (Foreground: clMaroon;     Background: clWindow),
{ etWindowsMessagePosted } (Foreground: clWhite;      Background: clGray),
{ etWindowsMessageSent   } (Foreground: clSkyBlue;    Background: clWindow)
  );

type

  { TEnvDebuggerOptions }

  TEnvDebuggerOptions = class(TIDESubOptions)
  private
    FDbgConfigStore: TXMLOptionsStorage;
    FDebuggerConfig: TDebuggerConfigStore;
    FDebuggerAllowFunctionCalls: boolean;
    FDebuggerAutoSetInstanceFromClass: boolean;
    FDebuggerShowExitCodeMessage: boolean;
    FDebuggerAutoCloseAsm: boolean;
    // TODO: store per debuggerclass options
    // Maybe these should go to a new TDebuggerOptions class
    FDebuggerResetAfterRun: boolean;
    FDebuggerFileHistory: TStringList; // per debugger class
    FDebuggerShowStopMessage: Boolean;
    FDebuggerEventLogClearOnRun: Boolean;
    FDebuggerEventLogCheckLineLimit: Boolean;
    FDebuggerEventLogLineLimit: Integer;
    FDebuggerEventLogShowBreakpoint: Boolean;
    FDebuggerEventLogShowDebugger: Boolean;
    FDebuggerEventLogShowModule: Boolean;
    FDebuggerEventLogShowOutput: Boolean;
    FDebuggerEventLogShowProcess: Boolean;
    FDebuggerEventLogShowThread: Boolean;
    FDebuggerEventLogShowWindows: Boolean;
    FDebuggerEventLogUseColors: Boolean;
    FDebuggerEventLogColors: array[TDBGEventType] of TDebuggerEventLogColor;
    function GetDebuggerEventLogColors(AIndex: TDBGEventType): TDebuggerEventLogColor;
    function GetNamedDebuggerFileHistory(AnIndex: String): TStringList;
    procedure SetDebuggerEventLogColors(AIndex: TDBGEventType;
      const AValue: TDebuggerEventLogColor);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadFromXml(OnlyDesktop: boolean); override;
    procedure WriteToXml(OnlyDesktop: boolean); override;
    procedure InitConfig; override;
  public
    property DebuggerConfig: TDebuggerConfigStore read FDebuggerConfig;
    property DebuggerFileHistory[AnIndex: String]: TStringList read GetNamedDebuggerFileHistory;
    property DebuggerShowStopMessage: boolean read FDebuggerShowStopMessage write FDebuggerShowStopMessage;
    property DebuggerShowExitCodeMessage: boolean read FDebuggerShowExitCodeMessage write FDebuggerShowExitCodeMessage;
    property DebuggerResetAfterRun: boolean read FDebuggerResetAfterRun write FDebuggerResetAfterRun;
    property DebuggerAutoCloseAsm: boolean read FDebuggerAutoCloseAsm write FDebuggerAutoCloseAsm;
    property DebuggerAutoSetInstanceFromClass: boolean read FDebuggerAutoSetInstanceFromClass write FDebuggerAutoSetInstanceFromClass;
    property DebuggerAllowFunctionCalls: boolean read FDebuggerAllowFunctionCalls write FDebuggerAllowFunctionCalls;
    // Debugger event log
    property DebuggerEventLogClearOnRun: Boolean read FDebuggerEventLogClearOnRun write FDebuggerEventLogClearOnRun;
    property DebuggerEventLogCheckLineLimit: Boolean read FDebuggerEventLogCheckLineLimit write FDebuggerEventLogCheckLineLimit;
    property DebuggerEventLogLineLimit: Integer read FDebuggerEventLogLineLimit write FDebuggerEventLogLineLimit;
    property DebuggerEventLogShowBreakpoint: Boolean read FDebuggerEventLogShowBreakpoint write FDebuggerEventLogShowBreakpoint;
    property DebuggerEventLogShowProcess: Boolean read FDebuggerEventLogShowProcess write FDebuggerEventLogShowProcess;
    property DebuggerEventLogShowThread: Boolean read FDebuggerEventLogShowThread write FDebuggerEventLogShowThread;
    property DebuggerEventLogShowModule: Boolean read FDebuggerEventLogShowModule write FDebuggerEventLogShowModule;
    property DebuggerEventLogShowOutput: Boolean read FDebuggerEventLogShowOutput write FDebuggerEventLogShowOutput;
    property DebuggerEventLogShowWindows: Boolean read FDebuggerEventLogShowWindows write FDebuggerEventLogShowWindows;
    property DebuggerEventLogShowDebugger: Boolean read FDebuggerEventLogShowDebugger write FDebuggerEventLogShowDebugger;
    property DebuggerEventLogUseColors: Boolean read FDebuggerEventLogUseColors write FDebuggerEventLogUseColors;
    property DebuggerEventLogColors[AIndex: TDBGEventType]: TDebuggerEventLogColor read GetDebuggerEventLogColors write SetDebuggerEventLogColors;
  end;

var
  EnvironmentDebugOpts: TEnvDebuggerOptions;


implementation

{ TDebuggerConfigStoreBase }

procedure TDebuggerConfigStoreBase.Init;
begin
  //
end;

procedure TDebuggerConfigStoreBase.Load;
begin
  Init;
  ConfigStore.ReadObject('', self);
end;

procedure TDebuggerConfigStoreBase.Save;
begin
  ConfigStore.WriteObject('', self);
end;

//{ TDebuggerWatchesDlgConfig }
//
//constructor TDebuggerWatchesDlgConfig.Create;
//begin
//  Init;
//end;
//
//procedure TDebuggerWatchesDlgConfig.Init;
//begin
//  FColumnNameWidth := -1;
//  FColumnValueWidth := -1;
//end;

{ TDebuggerCallStackDlgConfig }

constructor TDebuggerCallStackDlgConfig.Create;
begin
  Init;
end;

procedure TDebuggerCallStackDlgConfig.Init;
begin
  inherited Init;
end;

{ TDebuggerConfigStore }

procedure TDebuggerConfigStore.Load;
begin
  inherited;
  //ConfigStore.AppendBasePath('WatchesDlg/');
  //try
  //  FTDebuggerWatchesDlgConfig.ConfigStore := ConfigStore;
  //  FTDebuggerWatchesDlgConfig.Load;
  //finally
  //  ConfigStore.UndoAppendBasePath;
  //end;
  ConfigStore.AppendBasePath('CallStackDlg/');
  try
    FDlgCallStackConfig.ConfigStore := ConfigStore;
    FDlgCallStackConfig.Load;
  finally
    ConfigStore.UndoAppendBasePath;
  end;
end;

procedure TDebuggerConfigStore.Save;
begin
  inherited;
  ConfigStore.DeletePath('Type');
  //ConfigStore.AppendBasePath('WatchesDlg/');
  //try
  //  FTDebuggerWatchesDlgConfig.ConfigStore := ConfigStore;
  //  FTDebuggerWatchesDlgConfig.Save;
  //finally
  //  ConfigStore.UndoAppendBasePath;
  //end;
  ConfigStore.AppendBasePath('CallStackDlg/');
  try
    FDlgCallStackConfig.ConfigStore := ConfigStore;
    FDlgCallStackConfig.Save;
  finally
    ConfigStore.UndoAppendBasePath;
  end;
end;

constructor TDebuggerConfigStore.Create;
begin
  //FTDebuggerWatchesDlgConfig := TDebuggerWatchesDlgConfig.Create;
  FDlgCallStackConfig := TDebuggerCallStackDlgConfig.Create;
end;

destructor TDebuggerConfigStore.Destroy;
begin
  inherited Destroy;
  //FreeAndNil(FTDebuggerWatchesDlgConfig);
  FreeAndNil(FDlgCallStackConfig);
end;

{ TEnvDebuggerOptions }

constructor TEnvDebuggerOptions.Create;
begin
  FDebuggerFileHistory := TStringList.Create;
  FDebuggerFileHistory.OwnsObjects := True;
  FDebuggerEventLogColors := DebuggerDefaultColors;
  (* TODO: maybe revert relations.
    Create this in Debugger, and call environmentoptions for the configstore only? *)
  FDebuggerConfig := TDebuggerConfigStore.Create;
end;

destructor TEnvDebuggerOptions.Destroy;
begin
  FreeAndNil(FDebuggerFileHistory);
  FreeAndNil(FDebuggerConfig);
  FreeAndNil(FDbgConfigStore);
  inherited Destroy;
end;

procedure TEnvDebuggerOptions.ReadFromXml(OnlyDesktop: boolean);
var
  EventType: TDBGEventType;
  i: Integer;
  Path: String;
begin
  if OnlyDesktop then Exit;    // Debugger options are not part of desktop.
  Path:='EnvironmentOptions/';
  // DO not call   LoadDebuggerProperties; => not all debuggers are registered when this is first called
  FDebuggerConfig.Load;
  if XMLCfg.HasPath(Path+'DebuggerFilename/History', False) then begin
    i := FDebuggerFileHistory.AddObject('', TStringList.Create);
    LoadRecentList(XMLCfg,TStrings(FDebuggerFileHistory.Objects[i]),Path+'DebuggerFilename/History/',rltFile);
  end;
  // Debugger General Options
  DebuggerShowStopMessage := XMLCfg.GetValue(Path+'DebuggerOptions/ShowStopMessage/Value', True);
  DebuggerShowExitCodeMessage:=XMLCfg.GetValue(Path+'DebuggerOptions/DebuggerShowExitCodeMessage/Value', True);
  DebuggerResetAfterRun := XMLCfg.GetValue(Path+'DebuggerOptions/DebuggerResetAfterRun/Value', False);
  FDebuggerAutoCloseAsm := XMLCfg.GetValue(Path+'DebuggerOptions/DebuggerAutoCloseAsm/Value', False);
  FDebuggerAutoSetInstanceFromClass := XMLCfg.GetValue(Path+'DebuggerOptions/DebuggerAutoSetInstanceFromClass/Value', False);
  FDebuggerAllowFunctionCalls := XMLCfg.GetValue(Path+'DebuggerOptions/DebuggerAllowFunctionCalls/Value', False);
  FDebuggerEventLogClearOnRun := XMLCfg.GetValue(Path+'Debugger/EventLogClearOnRun', True);
  FDebuggerEventLogCheckLineLimit := XMLCfg.GetValue(Path+'Debugger/EventLogCheckLineLimit', False);
  FDebuggerEventLogLineLimit := XMLCfg.GetValue(Path+'Debugger/EventLogLineLimit', 1000);
  FDebuggerEventLogShowBreakpoint := XMLCfg.GetValue(Path+'Debugger/EventLogShowBreakpoint', False);
  FDebuggerEventLogShowProcess := XMLCfg.GetValue(Path+'Debugger/EventLogShowProcess', True);
  FDebuggerEventLogShowThread := XMLCfg.GetValue(Path+'Debugger/EventLogShowThread', True);
  FDebuggerEventLogShowModule := XMLCfg.GetValue(Path+'Debugger/EventLogShowModule', False);
  FDebuggerEventLogShowOutput := XMLCfg.GetValue(Path+'Debugger/EventLogShowOutput', True);
  FDebuggerEventLogShowWindows := XMLCfg.GetValue(Path+'Debugger/EventLogShowWindows', False);
  FDebuggerEventLogShowDebugger := XMLCfg.GetValue(Path+'Debugger/EventLogShowDebugger', True);
  FDebuggerEventLogUseColors := XMLCfg.GetValue(Path+'Debugger/EventLogUseColors', True);
  for EventType := Low(TDBGEventType) to High(TDBGEventType) do
  begin
    FDebuggerEventLogColors[EventType].Background :=
      XMLCfg.GetValue(Path+'Debugger/EventLogColors/' +
      GetEnumName(TypeInfo(EventType), Ord(EventType)) + '/Background',
      DebuggerDefaultColors[EventType].Background);
    FDebuggerEventLogColors[EventType].Foreground :=
      XMLCfg.GetValue(Path+'Debugger/EventLogColors/' +
      GetEnumName(TypeInfo(EventType), Ord(EventType)) + '/Foreground',
      DebuggerDefaultColors[EventType].Foreground);
  end;
end;

procedure TEnvDebuggerOptions.WriteToXml(OnlyDesktop: boolean);
var
  EventType: TDBGEventType;
  i: Integer;
  Path: String;
begin
  if OnlyDesktop then Exit;
  Path:='EnvironmentOptions/';
  FDebuggerConfig.Save;
  XMLCfg.SetDeleteValue(Path+'DebuggerOptions/ShowStopMessage/Value',
      FDebuggerShowStopMessage, True);
  XMLCfg.SetDeleteValue(Path+'DebuggerOptions/DebuggerShowExitCodeMessage/Value',
      FDebuggerShowExitCodeMessage, True);
  XMLCfg.SetDeleteValue(Path+'DebuggerOptions/DebuggerResetAfterRun/Value',
      FDebuggerResetAfterRun, False);
  XMLCfg.SetDeleteValue(Path+'DebuggerOptions/DebuggerAutoCloseAsm/Value',
      FDebuggerAutoCloseAsm, False);
  XMLCfg.SetDeleteValue(Path+'DebuggerOptions/DebuggerAutoSetInstanceFromClass/Value',
      FDebuggerAutoSetInstanceFromClass, False);
  XMLCfg.SetDeleteValue(Path+'DebuggerOptions/DebuggerAllowFunctionCalls/Value',
      FDebuggerAllowFunctionCalls, False);
  for i := 0 to FDebuggerFileHistory.Count -1 do
    if FDebuggerFileHistory[i] = '' then
      SaveRecentList(XMLCfg,TStrings(FDebuggerFileHistory.Objects[i]),Path+'DebuggerFilename/History/')
    else
      SaveRecentList(XMLCfg,TStrings(FDebuggerFileHistory.Objects[i]),
        Path+'DebuggerFilename/'+FDebuggerFileHistory[i]+'/History/');
  XMLCfg.SetDeleteValue(Path+'Debugger/EventLogClearOnRun',FDebuggerEventLogClearOnRun, True);
  XMLCfg.SetDeleteValue(Path+'Debugger/EventLogCheckLineLimit',FDebuggerEventLogCheckLineLimit, False);
  XMLCfg.SetDeleteValue(Path+'Debugger/EventLogLineLimit',FDebuggerEventLogLineLimit, 1000);
  XMLCfg.SetDeleteValue(Path+'Debugger/EventLogShowBreakpoint',FDebuggerEventLogShowBreakpoint, False);
  XMLCfg.SetDeleteValue(Path+'Debugger/EventLogShowProcess',FDebuggerEventLogShowProcess, True);
  XMLCfg.SetDeleteValue(Path+'Debugger/EventLogShowThread',FDebuggerEventLogShowThread, True);
  XMLCfg.SetDeleteValue(Path+'Debugger/EventLogShowModule',FDebuggerEventLogShowModule, False);
  XMLCfg.SetDeleteValue(Path+'Debugger/EventLogShowOutput',FDebuggerEventLogShowOutput, True);
  XMLCfg.SetDeleteValue(Path+'Debugger/EventLogShowWindows',FDebuggerEventLogShowWindows, False);
  XMLCfg.SetDeleteValue(Path+'Debugger/EventLogShowDebugger',FDebuggerEventLogShowDebugger, True);
  XMLCfg.SetDeleteValue(Path+'Debugger/EventLogUseColors',FDebuggerEventLogUseColors, True);
  for EventType := Low(TDBGEventType) to High(TDBGEventType) do
  begin
    XMLCfg.SetDeleteValue(Path+'Debugger/EventLogColors/' +
      GetEnumName(TypeInfo(EventType), Ord(EventType)) + '/Background',
        FDebuggerEventLogColors[EventType].Background,
        DebuggerDefaultColors[EventType].Background);
    XMLCfg.SetDeleteValue(Path+'Debugger/EventLogColors/' +
      GetEnumName(TypeInfo(EventType), Ord(EventType)) + '/Foreground',
        FDebuggerEventLogColors[EventType].Foreground,
        DebuggerDefaultColors[EventType].Foreground);
  end;
end;

procedure TEnvDebuggerOptions.InitConfig;
begin
  FreeAndNil(FDbgConfigStore);
  FDbgConfigStore := TXMLOptionsStorage.Create(XMLCfg, 'EnvironmentOptions/Debugger/');
  FDebuggerConfig.ConfigStore := FDbgConfigStore;
end;

function TEnvDebuggerOptions.GetDebuggerEventLogColors(AIndex: TDBGEventType): TDebuggerEventLogColor;
begin
  Result := FDebuggerEventLogColors[AIndex];
end;

procedure TEnvDebuggerOptions.SetDebuggerEventLogColors(AIndex: TDBGEventType;
  const AValue: TDebuggerEventLogColor);
begin
  FDebuggerEventLogColors[AIndex] := AValue;
end;

function TEnvDebuggerOptions.GetNamedDebuggerFileHistory(AnIndex: String): TStringList;
var
  i: Integer;
begin
  i := FDebuggerFileHistory.IndexOf(AnIndex);
  if i < 0 then begin
    i := FDebuggerFileHistory.AddObject(AnIndex, TStringList.Create);
    if XMLCfg.HasPath('EnvironmentOptions/DebuggerFilename/'+AnIndex+'/History', False) then
      LoadRecentList(XMLCfg,TStrings(FDebuggerFileHistory.Objects[i]),'EnvironmentOptions/DebuggerFilename/'+AnIndex+'/History/',rltFile)
    else
      TStrings(FDebuggerFileHistory.Objects[i]).Assign(DebuggerFileHistory['']);  // init from old list
  end;
  Result := TStringList(FDebuggerFileHistory.Objects[i]);
end;

end.

