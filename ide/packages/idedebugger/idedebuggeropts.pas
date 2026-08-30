unit IdeDebuggerOpts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  Laz2_XMLCfg, LazFileUtils, LazUTF8, LazLoggerBase,
  Laz2_DOM, Laz2_XMLRead, Laz2_XMLWrite,
  // BuildIntf
  IDEOptionsIntf,
  // IdeIntf
  IdeDebuggerConsolePlugInIntf, IdeDebuggerPlugInIntf,
  // IdeConfig
  EnvironmentOpts,
  // DebuggerIntf
  DbgIntfDebuggerBase,
  // IdeDebugger
  IdeDebuggerStringConstants, IdeDebuggerBackendValueConv,
  IdeDebuggerValueFormatter, IdeDebuggerDisplayFormats, IdeDebuggerExcludedRoutines;

type

  (* TIdeDbgConsoleWindowPlugInList

     One live plug-in instance per registered console window, holding that
     plug-in's settings. The IDE owns them; the options dialog edits a copy and
     assigns it back, which is what makes Cancel work.

     Stored as numbered items with the id as a value, not as a path built from
     the id. Ids are "package/class" and would otherwise become XmlConfig path
     separators, turning one plug-in's settings into two levels of nesting. *)

  TIdeDbgConsoleWindowPlugInList = class
  private type
    TPluginData = record
      RegClass: TLazDbgIdeConsoleWindowPlugInRegistryEntryClass;
      Intf: ILazDbgIdeConsoleWindowPlugIn;
      CopyConf: ILazDbgIdePlugInConfiguration;
      OwningIntf: boolean;
    end;
  private
    FList: array of TPluginData;
    FChanged: Boolean;
    function GetCopiedConf(AIndex: Integer): ILazDbgIdePlugInConfiguration;
    function GetDisplayName(AIndex: Integer): String;
    function GetIds(AIndex: Integer): String;
    function GetPlugIn(AIndex: Integer): ILazDbgIdeConsoleWindowPlugIn;
    function GetCount: Integer;
  public
    constructor Create;
    constructor CreateFrom(ASourceList: TIdeDbgConsoleWindowPlugInList);
    destructor Destroy; override;
    procedure Clear;
    function IndexOf(APlugin: ILazDbgIdeConsoleWindowPlugIn): integer; // only already instantiated
    function IndexOfId(AnID: String): integer;
    procedure AssignConf(ASource: TIdeDbgConsoleWindowPlugInList);
    (* The instance for AId, created from the registry on first ask. Nil if no
       such plug-in is registered in this IDE. *)
    function  PlugInById(const AnId: String): ILazDbgIdeConsoleWindowPlugIn;
    procedure LoadDataFromXMLConfig(const AConfig: TRttiXMLConfig; const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TRttiXMLConfig; const APath: string);
    property Count: Integer read GetCount;
    property Ids[AIndex: Integer]: String read GetIds;
    property PlugIns[AIndex: Integer]: ILazDbgIdeConsoleWindowPlugIn read GetPlugIn;
    property DisplayName[AIndex: Integer]: String read GetDisplayName;
    property CopiedConf[AIndex: Integer]: ILazDbgIdePlugInConfiguration read GetCopiedConf;
    property Changed: Boolean read FChanged write FChanged;
  end;

  { TDebuggerPropertiesConfig }

  TDebuggerPropertiesConfig = class(TPersistent)
  private
    FFLags: set of (dpcLoaded);
    FActive: Boolean;
    FConfigClass: String;
    FConfigClassInOldXml: String; // The ConfigClass in the xml file. In case the class in memory is changed
    FConfigName: String;
    FDebuggerClass: TDebuggerClass;
    FDebuggerFilename: string;
    FUID: String;
    FDebuggerProperties: TDebuggerProperties;
    FDebuggerPropertiesAsXml: String; // In case the DebuggerClass is unknown

    procedure InitUID;
  public
    destructor Destroy; override;
    constructor CreateFromXmlConf(AXMLCfg: TRttiXMLConfig; APath: String; AIndex: Integer; APreventActive: Boolean = False);
    constructor CreateFromOldXmlConf(AXMLCfg: TRttiXMLConfig; APath: String;
      ADebuggerClassName: String; ACreateAsActive: Boolean);
    constructor CreateForDebuggerClass(ADebuggerClass: TDebuggerClass; ACreateAsActive: Boolean);
    constructor CreateForDebuggerClass(ADebuggerClassName: String; ACreateAsActive: Boolean);
    constructor CreateCopy(ASource: TDebuggerPropertiesConfig; ACopyPropValues: Boolean = True; ACopyXmlOrigin: Boolean = False);

    procedure CopyFrom(ASource: TDebuggerPropertiesConfig; ACopyPropValues: Boolean = True);
    procedure AssignTo(Dest: TPersistent); override;
    function DisplayName: String;
    function NeedsExePath: Boolean;
    procedure ChangeDebuggerClass(ADebuggerClass: TDebuggerClass; ACopyPropValues: Boolean = True);
    function IsLoaded: Boolean;  // The class for the debugger was found
    function DebugText: String;

    procedure DeleteFromOldXml(AXMLCfg: TRttiXMLConfig; APath: String);
    procedure SaveToXml(AXMLCfg: TRttiXMLConfig; APath: String; AIndex: Integer);
    //procedure SaveToOldXml(AXMLCfg: TRttiXMLConfig; APath: String);

    property DebuggerClass: TDebuggerClass read FDebuggerClass;
    property DebuggerProperties: TDebuggerProperties read FDebuggerProperties;
  published
    property ConfigName: String read FConfigName write FConfigName;
    property ConfigClass: String read FConfigClass write FConfigClass;
    property ConfigClassInOldXml: String read FConfigClassInOldXml;
    property DebuggerFilename: string read FDebuggerFilename write FDebuggerFilename;
    property Active: Boolean read FActive write FActive;
    property UID: String read FUID write FUID;
  end;

  { TDebuggerPropertiesConfigListBase }

  TDebuggerPropertiesConfigListBase = class(TStringListUTF8Fast)
  private
    FCurrentDebuggerPropertiesConfig: TDebuggerPropertiesConfig; // Active entry, if loaded (if class was found, and is supported)

    function GetOpt(Index: Integer): TDebuggerPropertiesConfig;
  public
    constructor Create;
    procedure Clear; override;

    function EntryByName(AConfName, AConfClass: String): TDebuggerPropertiesConfig;
    function EntryByUid(AnUid: String): TDebuggerPropertiesConfig;
    property Opt[Index: Integer]: TDebuggerPropertiesConfig read GetOpt;
    property CurrentDebuggerPropertiesConfig: TDebuggerPropertiesConfig read FCurrentDebuggerPropertiesConfig;
  end;

  { TDebuggerPropertiesConfigList }

  TDebuggerPropertiesConfigList = class(TDebuggerPropertiesConfigListBase)
  private const
    XML_PATH_DEBUGGER_CONF     = 'Config[%d]/';
    XML_PATH_DEBUGGER_CONF_OLD = 'Class%s/';
  private
    FIsGlobalList: boolean;
    FForcedUnsuitableClass: TDebuggerClass;
    FHasActiveDebuggerEntry: Boolean;
    FKnownDebuggerClassCount: Integer;
    FUnsuitable, FUnloaded: TDebuggerPropertiesConfigListBase;

    function  GetListForEntry(AnEntry: TDebuggerPropertiesConfig): TDebuggerPropertiesConfigListBase;
    procedure SetCurrentDebuggerPropertiesOpt(AValue: TDebuggerPropertiesConfig);
    procedure AddEntry(AnEntry: TDebuggerPropertiesConfig);
  protected
    property HasActiveDebuggerEntry: Boolean read FHasActiveDebuggerEntry write FHasActiveDebuggerEntry; // for the initial setup dialog / entry may be of unknown class
  public
    constructor Create(AnIsGlobalList: boolean = False);
    destructor Destroy; override;
    procedure Clear; override;
    procedure LoadFromXml(AXMLCfg: TRttiXMLConfig; APath: String);
    procedure LoadFromOldXml(AXMLCfg: TRttiXMLConfig; APath: String; AnOldFileNamePath: String = '');
    procedure SaveToXml(AXMLCfg: TRttiXMLConfig; APath: String; AForceSaveEmpty: Boolean = False);

    property CurrentDebuggerPropertiesConfig: TDebuggerPropertiesConfig read FCurrentDebuggerPropertiesConfig write SetCurrentDebuggerPropertiesOpt;

    property Unsuitable: TDebuggerPropertiesConfigListBase read FUnsuitable;
    property Unloaded: TDebuggerPropertiesConfigListBase read FUnloaded;
    property ForcedUnsuitableClass: TDebuggerClass read FForcedUnsuitableClass;
  end;

  TBreakpointsDialogShowTreeType = (bstNone, bstBrkGroup);

  { TDebuggerOptions }

  TDebuggerOptions = class(TAbstractIDEEnvironmentOptions)
  private const
    DebuggerOptsVersion = integer(1);
    (*
      0: Initial version
      1: Upgrade check for GDB to FpDebug done
    *)
    DebuggerOptsVersionFpDebugUpdate = 1;
  private
    FAlwaysBringDbgDialogsToFront: boolean;
    FBreakpointsDialogShowTree: TBreakpointsDialogShowTreeType;
    FDisplayFormatConfigs: TDisplayFormatConfig;
    FEvaluateWordWrap: boolean;
    FFilename: string;
    FFileVersion: integer;

    FBackendConverterConfig: TIdeDbgValueConvertSelectorList;
    FConsoleWindowPlugIns: TIdeDbgConsoleWindowPlugInList;
    FConsoleWindowPlugInId: String;
    FHasActiveDebuggerEntry: Boolean;
    FPrimaryConfigPath: String;
    FSetupCheckIgnoreNoDefault: Boolean;
    FShowHintForWatches: boolean;
    FValueFormatterConfig: TIdeDbgValueFormatterSelectorList;
    FExcludeRoutineEntryConfig: TIdeDebuggerExcludeRoutineConfList;
    FWatchesDetailPaneWordWrap: boolean;
    FXMLCfg: TRttiXMLConfig;

    FDebuggerConfigList: TDebuggerPropertiesConfigList; // named entries

    function GetConsoleWindowPlugIns: TIdeDbgConsoleWindowPlugInList;
    function GetCurrentDebuggerPropertiesConfig: TDebuggerPropertiesConfig;
    procedure SetCurrentDebuggerPropertiesOpt(AValue: TDebuggerPropertiesConfig);
    procedure LoadDebuggerProperties;
  protected
    procedure InitXMLCfg(CleanConfig: boolean);
    property XMLCfg: TRttiXMLConfig read FXMLCfg;
  public
    class function GetGroupCaption:string; override;
    class function GetInstance: TAbstractIDEOptions; override;
  public
    constructor Create;
    constructor CreateDefaultOnly;
    destructor Destroy; override;
    procedure Init;

    procedure Load;
    procedure Save;
    function GetDefaultConfigFilename: string;
    procedure CreateConfig;

    property Filename: string read FFilename;
    property PrimaryConfigPath: String read FPrimaryConfigPath write FPrimaryConfigPath;

    property DisplayFormatConfigs: TDisplayFormatConfig read FDisplayFormatConfigs;
    property BackendConverterConfig: TIdeDbgValueConvertSelectorList read FBackendConverterConfig write FBackendConverterConfig;
    property ValueFormatterConfig: TIdeDbgValueFormatterSelectorList read FValueFormatterConfig write FValueFormatterConfig;
    property ExcludeRoutineEntryConfig: TIdeDebuggerExcludeRoutineConfList read FExcludeRoutineEntryConfig write FExcludeRoutineEntryConfig;
    property ConsoleWindowPlugIns: TIdeDbgConsoleWindowPlugInList read GetConsoleWindowPlugIns;

    function DebuggerFilename: string;
    function GetParsedDebuggerFilename(AProjectDbgFileName: String = ''): string;

    procedure SaveDebuggerPropertiesList;
    function  DebuggerPropertiesConfigList: TDebuggerPropertiesConfigList;
    function  CurrentDebuggerClass: TDebuggerClass;
    function  CurrentDebuggerPropertiesConfigEx(AnUID: String = ''): TDebuggerPropertiesConfig;
    property  CurrentDebuggerPropertiesConfig: TDebuggerPropertiesConfig read GetCurrentDebuggerPropertiesConfig write SetCurrentDebuggerPropertiesOpt;
    // HasActiveDebuggerEntry => marked as active in the xml, even if not IsLoaded
    property  HasActiveDebuggerEntry: Boolean read FHasActiveDebuggerEntry write FHasActiveDebuggerEntry; // for the initial setup dialog / entry may be of unknown class
    //property  DebuggerConfig: TDebuggerConfigStore read FDebuggerConfig;

  published
    property SetupCheckIgnoreNoDefault: Boolean read FSetupCheckIgnoreNoDefault write FSetupCheckIgnoreNoDefault;
    property BreakpointsDialogShowTree: TBreakpointsDialogShowTreeType read FBreakpointsDialogShowTree write FBreakpointsDialogShowTree default bstBrkGroup;
    property AlwaysBringDbgDialogsToFront: boolean read FAlwaysBringDbgDialogsToFront write FAlwaysBringDbgDialogsToFront default true;
    property WatchesDetailPaneWordWrap: boolean read FWatchesDetailPaneWordWrap write FWatchesDetailPaneWordWrap default False;
    property EvaluateWordWrap: boolean read FEvaluateWordWrap write FEvaluateWordWrap default False;
    property ShowHintForWatches: boolean read FShowHintForWatches write FShowHintForWatches default True;
    (* Which registered console window plug-in shows the debuggee's captured
       output. Empty means "whatever the IDE falls back to", which is how an
       existing config file reads and why it is the default: an installation
       that never opens this page must not have a value written for it. *)
    property ConsoleWindowPlugInId: String read FConsoleWindowPlugInId write FConsoleWindowPlugInId;
  end;

  TCurrentDebuggerSetupResult = (
    cdsOk,
    cdsNoActive,      // No Debugger is set as active/current
    cdsNotRegistered, // Active/Current class is not (yet) registered
    cdsNotSupported,  // Active/Current class does not support current OS/Arch

    cdsUpdateToFpDbgNeeded  // Still using GDB - and not yet confirmed as intentional
    // External exe will be checked by caller
  );

function CheckCurrentDebuggerSetup: TCurrentDebuggerSetupResult;

function GetDebuggerOptions: TDebuggerOptions;
property DebuggerOptions: TDebuggerOptions read GetDebuggerOptions;

implementation

const
  DebuggerOptsConfFileName = 'debuggeroptions.xml';
var
  TheDebuggerOptions: TDebuggerOptions = nil;

{ TIdeDbgConsoleWindowPlugInList }

constructor TIdeDbgConsoleWindowPlugInList.Create;
var
  i: Integer;
begin
  inherited Create;
  i := ConsoleWindowPlugInRegistry.Count;
  SetLength(FList, i);
  while i > 0 do begin
    dec(i);
    FList[i].RegClass := ConsoleWindowPlugInRegistry.IdePlugin[i];
  end;
end;

constructor TIdeDbgConsoleWindowPlugInList.CreateFrom(ASourceList: TIdeDbgConsoleWindowPlugInList);
var
  i: Integer;
begin
  inherited Create;
  i := ASourceList.Count;
  SetLength(FList, i);
  while i > 0 do begin
    dec(i);
    FList[i].RegClass := ASourceList.FList[i].RegClass;
    FList[i].Intf := ASourceList.FList[i].Intf;
    FList[i].OwningIntf := False;
  end;
end;

destructor TIdeDbgConsoleWindowPlugInList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TIdeDbgConsoleWindowPlugInList.Clear;
var
  i: Integer;
begin
  for i := 0 to Length(FList) - 1 do begin
    if (FList[i].Intf <> nil) and (FList[i].OwningIntf) then
      FList[i].Intf.Free;

    if (FList[i].CopyConf <> nil) then
      FList[i].CopyConf.FreeCopy;
  end;
  SetLength(FList, 0);
end;

function TIdeDbgConsoleWindowPlugInList.IndexOf(APlugin: ILazDbgIdeConsoleWindowPlugIn): integer;
begin
  Result := Length(FList) - 1;
  while (Result >= 0) and (FList[Result].Intf <> APlugin) do
    dec(Result);
end;

function TIdeDbgConsoleWindowPlugInList.IndexOfId(AnID: String): integer;
begin
  Result := Length(FList) - 1;
  while (Result >= 0) and not SameText(FList[Result].RegClass.GetPlugInId, AnID) do
    dec(Result);
end;

function TIdeDbgConsoleWindowPlugInList.GetCount: Integer;
begin
  Result := Length(FList);
end;

function TIdeDbgConsoleWindowPlugInList.GetPlugIn(AIndex: Integer): ILazDbgIdeConsoleWindowPlugIn;
begin
  Result := FList[AIndex].Intf;
  if Result <> nil then
    exit;
  FList[AIndex].Intf := FList[AIndex].RegClass.CreateIdePlugIn;
  FList[AIndex].OwningIntf := True;
  Result := FList[AIndex].Intf;
end;

function TIdeDbgConsoleWindowPlugInList.GetCopiedConf(AIndex: Integer
  ): ILazDbgIdePlugInConfiguration;
var
  c: ILazDbgIdePlugInConfiguration;
begin
  Result := FList[AIndex].CopyConf;
  if Result <> nil then
    exit;
  c := PlugIns[AIndex].GetConfiguration;
  if c <> nil then
    FList[AIndex].CopyConf := c.CreateCopy;
  Result := FList[AIndex].CopyConf;
end;

function TIdeDbgConsoleWindowPlugInList.GetDisplayName(AIndex: Integer
  ): String;
begin
  Result := FList[AIndex].RegClass.GetDisplayName;
end;

function TIdeDbgConsoleWindowPlugInList.GetIds(AIndex: Integer): String;
begin
  Result := FList[AIndex].RegClass.GetPlugInId;
end;

function TIdeDbgConsoleWindowPlugInList.PlugInById(const AnId: String
  ): ILazDbgIdeConsoleWindowPlugIn;
var
  Entry: TLazDbgIdeConsoleWindowPlugInRegistryEntryClass;
  i: Integer;
begin
  i := IndexOfId(AnId);
  if i >= 0 then
    Result := PlugIns[i]
  else
    Result := nil;
end;

procedure TIdeDbgConsoleWindowPlugInList.AssignConf(ASource: TIdeDbgConsoleWindowPlugInList);
var
  i, j: Integer;
  Src, Dst: ILazDbgIdePlugInConfiguration;
  p: ILazDbgIdeConsoleWindowPlugIn;
begin
  for i := 0 to ASource.Count - 1 do
    if ASource.FList[i].CopyConf <> nil then begin
      j := IndexOfId(ASource.FList[i].RegClass.GetPlugInId);
      assert(j >= 0, 'TIdeDbgConsoleWindowPlugInList.AssignConf: j >= 0');
      if j < 0 then Continue;
      if FList[j].Intf = nil then begin
        FList[j].Intf := ASource.FList[i].Intf;
        FList[j].OwningIntf := ASource.FList[i].OwningIntf;
        ASource.FList[i].OwningIntf := False;
      end;
      p := FList[j].Intf;
      assert(p<>nil, 'TIdeDbgConsoleWindowPlugInList.AssignConf: p<>nil');
      if p = nil then continue;
      Dst := p.GetConfiguration;
      assert(Dst<>nil, 'TIdeDbgConsoleWindowPlugInList.AssignConf: Dst<>nil');
      if Dst = nil then Continue;
      Dst.AssignOptions(ASource.FList[i].CopyConf);
    end;
end;

procedure TIdeDbgConsoleWindowPlugInList.LoadDataFromXMLConfig(
  const AConfig: TRttiXMLConfig; const APath: string);
var
  i, c: Integer;
  Id, p: String;
  Obj: TObject;
  P2: ILazDbgIdeConsoleWindowPlugIn;
begin
  c := AConfig.GetChildCount(APath);
  for i := 1 to c do begin
    p := APath + 'Item[' + IntToStr(i) + ']/';
    Id := AConfig.GetValue(p + 'Id', '');
    if Id = '' then
      Continue;
    (* A plug-in whose package is not installed here simply has no instance to
       read into. Its stored settings stay in the file untouched, so moving a
       config between installations does not quietly empty it. *)
    P2 := PlugInById(Id);
    if P2 = nil then
      Continue;
    Obj := P2.GetConfigObject;
    if Obj <> nil then
      AConfig.ReadObject(p + 'Config/', Obj);
  end;
  FChanged := False;
end;

procedure TIdeDbgConsoleWindowPlugInList.SaveDataToXMLConfig(
  const AConfig: TRttiXMLConfig; const APath: string);
var
  i, n: Integer;
  p: String;
  Obj: TObject;
begin
  AConfig.DeletePath(APath);
  n := 0;
  for i := 0 to Count - 1 do begin
    if FList[i].Intf = nil then
      continue;
    Obj := FList[i].Intf.GetConfigObject;
    if Obj = nil then
      Continue;   // a plug-in with no settings writes no entry at all
    inc(n);
    p := APath + 'Item[' + IntToStr(n) + ']/';
    AConfig.SetValue(p + 'Id', FList[i].RegClass.GetPlugInId);
    AConfig.WriteObject(p + 'Config/', Obj);
  end;
end;

function CheckCurrentDebuggerSetup: TCurrentDebuggerSetupResult;
var
  DbgConf: TDebuggerPropertiesConfig;
begin
  Result := cdsOk;
  DebuggerOptions.LoadDebuggerProperties;

  if not DebuggerOptions.HasActiveDebuggerEntry then
    exit(cdsNoActive);

  DbgConf := DebuggerOptions.DebuggerPropertiesConfigList.CurrentDebuggerPropertiesConfig;
  if (DbgConf = nil) or (DbgConf.DebuggerClass = nil) then
    exit(cdsNotRegistered); // class was not found in registered list

  if dfNotSuitableForOsArch in DbgConf.DebuggerClass.SupportedFeatures then
    exit(cdsNotSupported);

  if (DbgConf.DebuggerClass.ClassName = 'TGDBMIDebugger') and
     (DebuggerOptions.FFileVersion < DebuggerOptions.DebuggerOptsVersionFpDebugUpdate)
  then
    exit(cdsUpdateToFpDbgNeeded);

  assert((DebuggerOptions.CurrentDebuggerClass <> nil), 'CheckCurrentDebuggerSetup: (DebuggerOptions.CurrentDebuggerClass <> nil)');
end;

function GetDebuggerOptions: TDebuggerOptions;
begin
  if TheDebuggerOptions = nil then
    TheDebuggerOptions := TDebuggerOptions.Create;
  Result := TheDebuggerOptions;
end;

{ TDebuggerPropertiesConfig }

procedure TDebuggerPropertiesConfig.InitUID;
var
  g: TGUID;
begin
  if FUID <> '' then
    exit;

  if CreateGUID(g) = 0 then
    FUID := GUIDToString(g)
  else
    FUID := IntToHex(Random($100000000), 8)+'-'+IntToHex(Random($100000000), 8)+'-'+IntToHex(Random($100000000), 8);
end;

destructor TDebuggerPropertiesConfig.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FDebuggerProperties);
end;

constructor TDebuggerPropertiesConfig.CreateFromXmlConf(
  AXMLCfg: TRttiXMLConfig; APath: String; AIndex: Integer;
  APreventActive: Boolean);
var
  n: TDOMNode;
  st: TStringStream;
begin
  Create;
  FFLags := [];

  APath := Format(APath, [AIndex]);
  AXMLCfg.ReadObject(APath, Self);
  if APreventActive then
    FActive := False;

  FDebuggerClass := TBaseDebugManagerIntf.DebuggersByClassName[ConfigClass];
  if (FDebuggerClass <> nil) and
     ( Active or not (dfNotSuitableForOsArch in FDebuggerClass.SupportedFeatures) ) // Active entries will always be loaded for compatibility
  then begin
    FDebuggerProperties := FDebuggerClass.CreateProperties;
    if FDebuggerProperties <> nil then
      AXMLCfg.ReadObject(APath + 'Properties/', FDebuggerProperties);
    FFLags := [dpcLoaded];
  end
  else begin
    n := AXMLCfg.FindNode(APath + 'Properties', False);
    FDebuggerPropertiesAsXml := '';
    if n <> nil then begin
      st := TStringStream.Create('');
      WriteXML(n, st);
      FDebuggerPropertiesAsXml := st.DataString;
      st.Free;
    end;
  end;

  InitUID;
end;

constructor TDebuggerPropertiesConfig.CreateFromOldXmlConf(
  AXMLCfg: TRttiXMLConfig; APath: String; ADebuggerClassName: String;
  ACreateAsActive: Boolean);
var
  p: String;
  n: TDOMNode;
  st: TStringStream;
begin
  Create;
  FFLags := [];

  APath := Format(APath, [ADebuggerClassName]);
  p := APath + 'Config/';
  // Read first, so any (invalid) Class/Name will be cleared after reading
  AXMLCfg.ReadObject(p, Self);  // read FDebuggerFilename;

  FConfigClass := ADebuggerClassName;
  FConfigClassInOldXml := FConfigClass;
//  FConfigName := '';
  FActive := ACreateAsActive;

  FDebuggerClass := TBaseDebugManagerIntf.DebuggersByClassName[ConfigClass];

  p := APath + 'Properties/';
  if (FDebuggerClass <> nil) then begin
    FDebuggerProperties := FDebuggerClass.CreateProperties;
    if FDebuggerProperties <> nil then
      AXMLCfg.ReadObject(p, FDebuggerProperties);
    if Active or not (dfNotSuitableForOsArch in FDebuggerClass.SupportedFeatures) then
      FFLags := [dpcLoaded];
  end
  else begin
    if p[Length(p)] = '/' then
      delete(p, Length(p), 1);
    n := AXMLCfg.FindNode(p, False);
    FDebuggerPropertiesAsXml := '';
    if n <> nil then begin
      st := TStringStream.Create('');
      WriteXML(n, st);
      FDebuggerPropertiesAsXml := st.DataString;
      st.Free;
    end;
  end;

  InitUID;
end;

constructor TDebuggerPropertiesConfig.CreateForDebuggerClass(
  ADebuggerClass: TDebuggerClass; ACreateAsActive: Boolean);
begin
  Create;

  FDebuggerClass := ADebuggerClass;
  FConfigClass := ADebuggerClass.ClassName;
  FConfigName := '';
  FActive := ACreateAsActive;
  FDebuggerProperties := ADebuggerClass.CreateProperties;
  if FActive or not (dfNotSuitableForOsArch in FDebuggerClass.SupportedFeatures) then
    FFLags := [dpcLoaded]; // i.e. treat as loaded, save when saving all

  InitUID;
end;

constructor TDebuggerPropertiesConfig.CreateForDebuggerClass(
  ADebuggerClassName: String; ACreateAsActive: Boolean);
begin
  Create;

  FConfigClass := ADebuggerClassName;
  FConfigName := '';
  FActive := ACreateAsActive;
  FDebuggerClass := TBaseDebugManagerIntf.DebuggersByClassName[ConfigClass];
  if FDebuggerClass <> nil then begin
    FDebuggerProperties := FDebuggerClass.CreateProperties;
    if FActive or not (dfNotSuitableForOsArch in FDebuggerClass.SupportedFeatures) then
      FFLags := [dpcLoaded]; // i.e. treat as loaded, save when saving all
  end;

  InitUID;
end;

constructor TDebuggerPropertiesConfig.CreateCopy(
  ASource: TDebuggerPropertiesConfig; ACopyPropValues: Boolean;
  ACopyXmlOrigin: Boolean);
begin
  Create;
  CopyFrom(ASource, ACopyPropValues);
  if ACopyXmlOrigin then
    FUID          := ASource.FUID;
end;

procedure TDebuggerPropertiesConfig.CopyFrom(
  ASource: TDebuggerPropertiesConfig; ACopyPropValues: Boolean);
begin
  FConfigClass         := ASource.FConfigClass;
  FConfigClassInOldXml := ASource.FConfigClassInOldXml;
  FConfigName       := ASource.FConfigName;
  FDebuggerClass    := ASource.FDebuggerClass;
  FDebuggerFilename := ASource.FDebuggerFilename;
  FFLags            := ASource.FFLags;

  FreeAndNil(FDebuggerProperties);
  if ASource.DebuggerClass <> nil then
    FDebuggerProperties := ASource.DebuggerClass.CreateProperties;
  if ACopyPropValues and (ASource.FDebuggerProperties <> nil) then
    FDebuggerProperties.Assign(ASource.FDebuggerProperties);

  FUID := '';
  InitUID;
end;

procedure TDebuggerPropertiesConfig.AssignTo(Dest: TPersistent);
begin
  TDebuggerPropertiesConfig(Dest).CopyFrom(Self);
end;

function TDebuggerPropertiesConfig.DisplayName: String;
begin
  if FDebuggerClass <> nil then
    Result := FDebuggerClass.Caption
  else
    Result := FConfigClass;
  if FConfigName <> '' then
    Result := FConfigName + ' [' + Result + ']'
  else
    Result := '[' + Result + ']';
end;

function TDebuggerPropertiesConfig.NeedsExePath: Boolean;
begin
  Result := (FDebuggerClass <> nil) and FDebuggerClass.NeedsExePath;
end;

procedure TDebuggerPropertiesConfig.ChangeDebuggerClass(
  ADebuggerClass: TDebuggerClass; ACopyPropValues: Boolean);
var
  p: TDebuggerProperties;
begin
  assert(IsLoaded, 'TDebuggerPropertiesConfig.ChangeDebuggerClass: IsLoaded');
  FDebuggerClass := ADebuggerClass;
  FConfigClass := ADebuggerClass.ClassName;
  p := FDebuggerProperties;
  FDebuggerProperties := ADebuggerClass.CreateProperties;
  if ACopyPropValues and (p <> nil) then
    FDebuggerProperties.Assign(p);
  p.Free;
end;

function TDebuggerPropertiesConfig.IsLoaded: Boolean;
begin
  Result := dpcLoaded in FFLags; // (FDebuggerClass <> nil) and (FDebuggerProperties <> nil);
end;

function TDebuggerPropertiesConfig.DebugText: String;
begin
  if Self = nil then
    exit('NIL');
  Result := Format('C-Name: %s, C-Class: %s, Class %s, Prop %s, Path: %s',
    [FConfigName, FConfigClass, DbgSName(FDebuggerClass), dbgs(FDebuggerProperties),
     FDebuggerFilename]);
end;

procedure TDebuggerPropertiesConfig.DeleteFromOldXml(AXMLCfg: TRttiXMLConfig;
  APath: String);
begin
  if FConfigClassInOldXml = '' then begin
    debugln(['Debugger was loaded, but has no ConfigClass in XML', DebugText]);
    FConfigClassInOldXml := FConfigClass;
  end;

  AXMLCfg.DeletePath(Format(APath, [FConfigClassInOldXml, 'Config']));
  AXMLCfg.DeletePath(Format(APath, [FConfigClassInOldXml, 'Properties']));

  if FConfigClassInOldXml <> FConfigClass then begin
    AXMLCfg.DeletePath(Format(APath, [FConfigClass, 'Config']));
    AXMLCfg.DeletePath(Format(APath, [FConfigClass, 'Properties']));
  end;
  FConfigClassInOldXml := FConfigClass;
end;

procedure TDebuggerPropertiesConfig.SaveToXml(AXMLCfg: TRttiXMLConfig;
  APath: String; AIndex: Integer);
var
  PropDef: TDebuggerProperties;
  OptDef: TDebuggerPropertiesConfig;
  n: TDOMNode;
  st: TStringStream;
begin
  APath := Format(APath, [AIndex]);

  OptDef := TDebuggerPropertiesConfig.Create;
  AXMLCfg.WriteObject(APath, Self, OptDef);
  OptDef.Free;

  if IsLoaded then begin
    if FDebuggerProperties <> nil then begin
      PropDef := FDebuggerClass.CreateProperties;
      AXMLCfg.WriteObject(APath + 'Properties/', FDebuggerProperties, PropDef);
      PropDef.Free;
    end;
  end
  else
  if FDebuggerPropertiesAsXml <> '' then begin
    AXMLCfg.DeletePath(APath + 'Properties');
    n := AXMLCfg.FindNode(APath, False);
    assert(n<>nil, 'TDebuggerPropertiesConfig.SaveToXml: n<>nil');
    if n <> nil then begin
      st := TStringStream.Create(FDebuggerPropertiesAsXml);
      st.Position := 0;
      ReadXMLFragment(n, st, []);
      st.Free;
    end;
  end;
end;

//procedure TDebuggerPropertiesConfig.SaveToOldXml(AXMLCfg: TRttiXMLConfig;
//  APath: String);
//var
//  PropDef: TDebuggerProperties;
//  OptDef: TDebuggerPropertiesConfig;
//  n: TDOMNode;
//  st: TStringStream;
//begin
//  FConfigClassInOldXml := FConfigClass;
//
//  OptDef := TDebuggerPropertiesConfig.Create;
//  OptDef.ConfigName := ConfigName;   // Do not write Name
//  // ConfigClass will differ and be written. This ensures that even an unmodified config is written (to preserve its existence)
//  AXMLCfg.WriteObject(Format(APath, [FConfigClass, 'Config']), Self, OptDef);
//  OptDef.Free;
//
//  if FDebuggerProperties <> nil then begin
//    APath := Format(APath, [FConfigClass, 'Properties']);
//    PropDef := FDebuggerClass.CreateProperties;
//    AXMLCfg.WriteObject(APath, FDebuggerProperties, PropDef);
//    PropDef.Free;
//  end
//  else
//  if FDebuggerPropertiesAsXml <> '' then begin
//    APath := Format(APath, [FConfigClass, '']);
//    while (APath <> '') and (APath[Length(APath)] = '/') do
//      delete(APath, Length(APath), 1);
//    AXMLCfg.DeletePath(APath + '/Properties');
//    n := AXMLCfg.FindNode(APath, False);
//    assert(n<>nil, 'TDebuggerPropertiesConfig.SaveToXml: n<>nil');
//    if n <> nil then begin
//      st := TStringStream.Create(FDebuggerPropertiesAsXml);
//      st.Position := 0;
//      ReadXMLFragment(n, st, []);
//      st.Free;
//    end;
//  end;
//end;

{ TDebuggerPropertiesConfigListBase }

function TDebuggerPropertiesConfigListBase.GetOpt(Index: Integer
  ): TDebuggerPropertiesConfig;
begin
  Result := TDebuggerPropertiesConfig(Objects[Index]);
end;

constructor TDebuggerPropertiesConfigListBase.Create;
begin
  inherited Create;
  OwnsObjects := True;

end;

procedure TDebuggerPropertiesConfigListBase.Clear;
begin
  inherited Clear;
  FCurrentDebuggerPropertiesConfig := nil;
end;

function TDebuggerPropertiesConfigListBase.EntryByName(AConfName,
  AConfClass: String): TDebuggerPropertiesConfig;
var
  i: Integer;
  dpCfg: TDebuggerPropertiesConfig;
begin
  Result := nil;
  i := Count - 1;
  while i >= 0 do begin
    dpCfg := Opt[i];
    if dpCfg.IsLoaded
    and (dpCfg.ConfigName = AConfName)
    and (dpCfg.ConfigClass = AConfClass) then
      Break;
    dec(i);
  end;
  if i >= 0 then
    Result := dpCfg;
end;

function TDebuggerPropertiesConfigListBase.EntryByUid(AnUid: String
  ): TDebuggerPropertiesConfig;
var
  i: Integer;
begin
  Result := nil;
  i := Count - 1;
  while (i >= 0) and (Opt[i].UID <> AnUid) do
    dec(i);
  if i >= 0 then
    Result := Opt[i];
end;

{ TDebuggerPropertiesConfigList }

function TDebuggerPropertiesConfigList.GetListForEntry(
  AnEntry: TDebuggerPropertiesConfig): TDebuggerPropertiesConfigListBase;
begin
  Result := Self;
  if not AnEntry.IsLoaded then begin
    if (AnEntry.DebuggerClass <> nil) and (dfNotSuitableForOsArch in AnEntry.DebuggerClass.SupportedFeatures) then
      Result := FUnsuitable
    else
      Result := FUnloaded;
  end;
end;

procedure TDebuggerPropertiesConfigList.SetCurrentDebuggerPropertiesOpt(
  AValue: TDebuggerPropertiesConfig);
begin
  if FCurrentDebuggerPropertiesConfig = AValue then Exit;
  assert((AValue=nil) or AValue.IsLoaded, 'TDebuggerPropertiesConfigList.SetCurrentDebuggerPropertiesOpt: (AValue.IsLoaded');
  if (AValue <> nil) and (IndexOfObject(AValue) < 0) then
    AddEntry(AValue);
  FCurrentDebuggerPropertiesConfig := AValue;
end;

procedure TDebuggerPropertiesConfigList.AddEntry(
  AnEntry: TDebuggerPropertiesConfig);
begin
  GetListForEntry(AnEntry).AddObject(AnEntry.ConfigName, AnEntry);

  if AnEntry.IsLoaded and (dfNotSuitableForOsArch in AnEntry.DebuggerClass.SupportedFeatures) then
    FForcedUnsuitableClass := AnEntry.DebuggerClass;
end;

constructor TDebuggerPropertiesConfigList.Create(AnIsGlobalList: boolean);
begin
  FIsGlobalList := AnIsGlobalList;
  FUnsuitable := TDebuggerPropertiesConfigListBase.Create;
  FUnloaded := TDebuggerPropertiesConfigListBase.Create;
  inherited Create;
end;

destructor TDebuggerPropertiesConfigList.Destroy;
begin
  inherited Destroy;
  FUnsuitable.Free;
  FUnloaded.Free;
end;

procedure TDebuggerPropertiesConfigList.Clear;
begin
  inherited Clear;
  FUnsuitable.Clear;
  FUnloaded.Clear;
end;

procedure TDebuggerPropertiesConfigList.LoadFromXml(AXMLCfg: TRttiXMLConfig;
  APath: String);
var
  ConfCount, i: Integer;
  Entry: TDebuggerPropertiesConfig;
begin
  // Check if new Debugger-Classes were registered since the last load.
  if FIsGlobalList then begin
    if (Count > 0) and
       (TBaseDebugManagerIntf.DebuggerCount = FKnownDebuggerClassCount)
    then
      exit;
    FKnownDebuggerClassCount := TBaseDebugManagerIntf.DebuggerCount;
  end;
  HasActiveDebuggerEntry := False;


  Clear;
  FCurrentDebuggerPropertiesConfig := nil;
  FForcedUnsuitableClass := nil;

  ConfCount := AXMLCfg.GetListItemCount(APath, 'Config', False);
  for i := 1 to ConfCount do begin
    Entry := TDebuggerPropertiesConfig.CreateFromXmlConf(AXMLCfg, APath + XML_PATH_DEBUGGER_CONF, i, FCurrentDebuggerPropertiesConfig<>nil);
    AddEntry(Entry);
    if Entry.Active then begin
      HasActiveDebuggerEntry := True;
      if GetListForEntry(Entry).FCurrentDebuggerPropertiesConfig = nil then
        GetListForEntry(Entry).FCurrentDebuggerPropertiesConfig := Entry;
    end;
  end;
end;

procedure TDebuggerPropertiesConfigList.LoadFromOldXml(AXMLCfg: TRttiXMLConfig;
  APath: String; AnOldFileNamePath: String);
var
  i: Integer;
  Entry: TDebuggerPropertiesConfig;
  ActiveClassName, CurFilename: String;
  IsEntryForCurrentClass: Boolean;
  nd, nd2: TDOMNode;
  s: String;
begin
  if (Count > 0) and FIsGlobalList and
     (TBaseDebugManagerIntf.DebuggerCount = FKnownDebuggerClassCount)
  then
    exit;

  LoadFromXml(AXMLCfg, APath + 'Configs/');

  // Read old style, per class
  if (AnOldFileNamePath <> '') then begin
    ActiveClassName := AXMLCfg.GetValue(APath + 'Class', '');
    HasActiveDebuggerEntry := HasActiveDebuggerEntry or (ActiveClassName <> '');
    // There is only one filename for all classes
    CurFilename:=AXMLCfg.GetValue(AnOldFileNamePath, '');


    nd := AXMLCfg.FindNode(APath, False);
    if nd <> nil then begin
      for i := 0 to nd.GetChildCount - 1 do begin
        nd2 := nd.ChildNodes[i];
        s :=  nd2.NodeName;
        if (Length(s) < 6) or (CompareText(copy(s,1,5), 'Class') <> 0)
           //or ( (nd2.FindNode('Properties') = nil) and (nd2.FindNode('Config') = nil) )
        then
          Continue;

        s := copy(s, 6, Length(s));
        IsEntryForCurrentClass := (ActiveClassName <> '') and (CompareText(s, ActiveClassName)=0);

        Entry := TDebuggerPropertiesConfig.CreateFromOldXmlConf(AXMLCfg,
          APath + XML_PATH_DEBUGGER_CONF_OLD, s,
          IsEntryForCurrentClass and (FCurrentDebuggerPropertiesConfig = nil)
        );

        if IsEntryForCurrentClass then begin
          ActiveClassName := '';
          if GetListForEntry(Entry).FCurrentDebuggerPropertiesConfig = nil then
            GetListForEntry(Entry).FCurrentDebuggerPropertiesConfig := Entry;
          if (Entry.DebuggerFilename = '') and (Entry.NeedsExePath or (not Entry.IsLoaded)) then
            Entry.DebuggerFilename := CurFilename;
        end;

        AddEntry(Entry);
      end;
    end;

    if ActiveClassName <> '' then begin
      Entry := TDebuggerPropertiesConfig.CreateForDebuggerClass(ActiveClassName, True);
      if (Entry.DebuggerFilename = '') and (Entry.NeedsExePath or (not Entry.IsLoaded)) then
        Entry.DebuggerFilename := CurFilename;

      AddEntry(Entry);
      assert(FCurrentDebuggerPropertiesConfig=nil, 'TDebuggerPropertiesConfigList.LoadFromOldXml: FCurrentDebuggerPropertiesConfig=nil');
      GetListForEntry(Entry).FCurrentDebuggerPropertiesConfig := Entry;
    end;
  end;
end;

procedure TDebuggerPropertiesConfigList.SaveToXml(AXMLCfg: TRttiXMLConfig;
  APath: String; AForceSaveEmpty: Boolean);
var
  i, ConfCount, Idx: Integer;
  Entry: TDebuggerPropertiesConfig;
begin
  ConfCount := AXMLCfg.GetListItemCount(APath, 'Config', False) + 1;
  for i := ConfCount downto 1 do
    AXMLCfg.DeletePath(APath + Format(XML_PATH_DEBUGGER_CONF, [i]));

  Idx := 1;
  for i := 0 to Count - 1 do begin
    Entry := Opt[i];

    Entry.Active := Entry = FCurrentDebuggerPropertiesConfig;
    Entry.SaveToXml(AXMLCfg, APath + XML_PATH_DEBUGGER_CONF, Idx);
    inc(Idx);
  end;

  for i := 0 to FUnloaded.Count - 1 do begin
    Entry := FUnloaded.Opt[i];
    Entry.Active := (FCurrentDebuggerPropertiesConfig = nil) and
                    (Entry = FUnloaded.FCurrentDebuggerPropertiesConfig);
    Entry.SaveToXml(AXMLCfg, APath + XML_PATH_DEBUGGER_CONF, Idx);
    inc(Idx);
  end;
  for i := 0 to FUnsuitable.Count - 1 do begin
    Entry := FUnsuitable.Opt[i];
    Entry.Active := (FCurrentDebuggerPropertiesConfig = nil) and
                    (FUnloaded.FCurrentDebuggerPropertiesConfig = nil) and
                    (Entry = FUnsuitable.FCurrentDebuggerPropertiesConfig);
    Entry.SaveToXml(AXMLCfg, APath + XML_PATH_DEBUGGER_CONF, Idx);
    inc(Idx);
  end;

  if (Count > 0) or AForceSaveEmpty then
    AXMLCfg.SetValue(APath+'Version', 1);
end;

{ TDebuggerOptions }

procedure TDebuggerOptions.InitXMLCfg(CleanConfig: boolean);
begin
  if (FXMLCfg=nil) then begin
    FreeAndNil(FXMLCfg);
    if CleanConfig then
      FXMLCfg:=TRttiXMLConfig.CreateClean(Filename)
    else
      FXMLCfg:=TRttiXMLConfig.Create(Filename);
  end;
end;

class function TDebuggerOptions.GetGroupCaption: string;
begin
  Result := dlgIdeDbgDebugger;
end;

class function TDebuggerOptions.GetInstance: TAbstractIDEOptions;
begin
  Result := DebuggerOptions;
end;

constructor TDebuggerOptions.Create;
begin
  inherited Create;
  FDebuggerConfigList := TDebuggerPropertiesConfigList.Create(True);
  FDisplayFormatConfigs := TDisplayFormatConfig.Create(True);
  BackendConverterConfig := TIdeDbgValueConvertSelectorList.Create;
  FValueFormatterConfig := TIdeDbgValueFormatterSelectorList.Create;
  FExcludeRoutineEntryConfig := TIdeDebuggerExcludeRoutineConfList.Create;
  Init;
end;

constructor TDebuggerOptions.CreateDefaultOnly;
begin
  // Used as default for ReadObject / WriteObject;
  Init;
end;

destructor TDebuggerOptions.Destroy;
begin
  inherited Destroy;
  BackendConverterConfig.Free;
  FDisplayFormatConfigs.Free;
  FValueFormatterConfig.Free;
  FExcludeRoutineEntryConfig.Free;
  FConsoleWindowPlugIns.Free;
  FDebuggerConfigList.Free;

  FXMLCfg.Free;
end;

procedure TDebuggerOptions.Init;
begin
  // Init for all published values
  FBreakpointsDialogShowTree := bstBrkGroup;
  FAlwaysBringDbgDialogsToFront := True;
  FShowHintForWatches := True;
end;

procedure TDebuggerOptions.Load;
var
  Path: String;
  Def: TDebuggerOptions;
begin
  InitXMLCfg(False);

  Path := 'Debugger/';
  FFileVersion:=FXMLCfg.GetValue(Path+'Version', 0);

  Def := TDebuggerOptions.CreateDefaultOnly;
  FXMLCfg.ReadObject(Path + 'Options/', Self, Def);
  FreeAndNil(Def);

  FDisplayFormatConfigs.LoadFromXml(FXMLCfg, Path + 'DisplayFormatConfigs/');
  FBackendConverterConfig.LoadDataFromXMLConfig(FXMLCfg, Path + 'FpDebug/ValueConvert/');
  FValueFormatterConfig.LoadDataFromXMLConfig(FXMLCfg, Path + 'FpDebug/ValueFormatter/');
  FExcludeRoutineEntryConfig.LoadDataFromXMLConfig(FXMLCfg, Path + 'FpDebug/ExcludeRoutineEntries/');
  ConsoleWindowPlugIns.LoadDataFromXMLConfig(FXMLCfg, Path + 'ConsoleWindowPlugIns/');
end;

procedure TDebuggerOptions.Save;
var
  Path: String;
  Def: TDebuggerOptions;
begin
  InitXMLCfg(False); // Dont delete old content
  Path := 'Debugger/';
  FXMLCfg.SetValue(Path+'Version', DebuggerOptsVersion);

  Def := TDebuggerOptions.CreateDefaultOnly;
  FXMLCfg.WriteObject(Path + 'Options/', Self, Def);
  FreeAndNil(Def);

  if FBackendConverterConfig.Changed then
    FBackendConverterConfig.SaveDataToXMLConfig(FXMLCfg, Path + 'FpDebug/ValueConvert/');
  FBackendConverterConfig.Changed := False;
  FDisplayFormatConfigs.SaveToXml(FXMLCfg, Path + 'DisplayFormatConfigs/');
  if FValueFormatterConfig.Changed then
    FValueFormatterConfig.SaveDataToXMLConfig(FXMLCfg, Path + 'FpDebug/ValueFormatter/');
  FValueFormatterConfig.Changed := False;

// TODO: changed since loaded?
  FExcludeRoutineEntryConfig.SaveDataToXMLConfig(FXMLCfg, Path + 'FpDebug/ExcludeRoutineEntries/');

  if ConsoleWindowPlugIns.Changed then
    ConsoleWindowPlugIns.SaveDataToXMLConfig(FXMLCfg, Path + 'ConsoleWindowPlugIns/');
  ConsoleWindowPlugIns.Changed := False;

  SaveDebuggerPropertiesList;

  FXMLCfg.Flush;
end;

function TDebuggerOptions.GetDefaultConfigFilename: string;
begin
  Result:=TrimFilename(AppendPathDelim(PrimaryConfigPath)+DebuggerOptsConfFileName);

end;

procedure TDebuggerOptions.CreateConfig;
begin
  FFilename:=GetDefaultConfigFilename;
end;

function TDebuggerOptions.DebuggerFilename: string;
var
  DbgCfg: TDebuggerPropertiesConfig;
begin
  Result := '';
  DbgCfg := CurrentDebuggerPropertiesConfig;
  if DbgCfg <> nil then
    Result := DbgCfg.DebuggerFilename;
end;

function TDebuggerOptions.GetParsedDebuggerFilename(AProjectDbgFileName: String
  ): string;
begin
  if AProjectDbgFileName = '' then
    AProjectDbgFileName := DebuggerFilename;

  Result:=EnvironmentOptions.GetParsedDebuggerFilename(AProjectDbgFileName);
end;

function TDebuggerOptions.GetCurrentDebuggerPropertiesConfig: TDebuggerPropertiesConfig;
begin
  LoadDebuggerProperties;
  Result := FDebuggerConfigList.CurrentDebuggerPropertiesConfig;
end;

function TDebuggerOptions.GetConsoleWindowPlugIns: TIdeDbgConsoleWindowPlugInList;
begin
  if FConsoleWindowPlugIns = nil then
    FConsoleWindowPlugIns := TIdeDbgConsoleWindowPlugInList.Create;
  Result := FConsoleWindowPlugIns;
end;

procedure TDebuggerOptions.SetCurrentDebuggerPropertiesOpt(
  AValue: TDebuggerPropertiesConfig);
begin
  LoadDebuggerProperties;
  FDebuggerConfigList.CurrentDebuggerPropertiesConfig := AValue;
end;

procedure TDebuggerOptions.LoadDebuggerProperties;
begin
  if XMLCfg.HasPath('Debugger/Backends/', False) then begin
    FDebuggerConfigList.LoadFromXml(XMLCfg, 'Debugger/Backends/');
  end
  else begin
    FDebuggerConfigList.LoadFromOldXml(EnvironmentOptions.XMLCfg, 'EnvironmentOptions/Debugger/', 'EnvironmentOptions/DebuggerFilename/Value');
  end;

  HasActiveDebuggerEntry := FDebuggerConfigList.HasActiveDebuggerEntry;
end;

procedure TDebuggerOptions.SaveDebuggerPropertiesList;
begin
  FDebuggerConfigList.SaveToXml(XMLCfg, 'Debugger/Backends/', True);
  EnvironmentOptions.XMLCfg.SetValue('EnvironmentOptions/Debugger/Deprecated', 'Backends/Class-Config moved to DebuggerOptions.xml');
end;

function TDebuggerOptions.DebuggerPropertiesConfigList: TDebuggerPropertiesConfigList;
begin
  LoadDebuggerProperties;

  Result := FDebuggerConfigList;
end;

function TDebuggerOptions.CurrentDebuggerClass: TDebuggerClass;
var
  Cfg: TDebuggerPropertiesConfig;
begin
  LoadDebuggerProperties;

  Result := nil;
  Cfg := CurrentDebuggerPropertiesConfig;
  if  Cfg<> nil then
    Result := Cfg.DebuggerClass;
end;

function TDebuggerOptions.CurrentDebuggerPropertiesConfigEx(AnUID: String): TDebuggerPropertiesConfig;
begin
  Result := nil;
  if AnUID <> '' then
    Result := FDebuggerConfigList.EntryByUid(AnUID);

  if Result = nil then
    Result := CurrentDebuggerPropertiesConfig;
end;

finalization
  TheDebuggerOptions.Free;

end.

