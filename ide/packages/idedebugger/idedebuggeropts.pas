unit IdeDebuggerOpts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IDEOptionsIntf, Laz2_XMLCfg, LazFileUtils, LazUTF8,
  LazLoggerBase, DbgIntfDebuggerBase, IdeDebuggerStringConstants,
  IdeDebuggerBackendValueConv,
  EnvironmentOpts;

type

  { TDebuggerPropertiesConfig }

  TDebuggerPropertiesConfig = class(TPersistent)
  private
    FFLags: set of (dpcLoaded, dpcDeleted);
    FActive: Boolean;
    FConfigClass: String;
    FConfigClassInOldXml: String; // The ConfigClass in the xml file. In case the class in memory is changed
    FConfigName: String;
    FDebuggerClass: TDebuggerClass;
    FDebuggerFilename: string;
    FIsFromOldXml: Boolean;
    FUID: String;
    FXmlIndex: Integer;
    FDebuggerProperties: TDebuggerProperties;

    procedure InitUID;
  public
    destructor Destroy; override;
    constructor CreateFromXmlConf(AXMLCfg: TRttiXMLConfig; APath: String; AIndex: Integer);
    constructor CreateFromOldXmlConf(AXMLCfg: TRttiXMLConfig; APath: String;
      ADebuggerClass: TDebuggerClass; AForceLoad: Boolean = False);
    constructor CreateFromOldXmlConf(AXMLCfg: TRttiXMLConfig; APath: String;
      ADebuggerClassName: String; AForceLoad: Boolean = False);
    constructor CreateForDebuggerClass(ADebuggerClass: TDebuggerClass);
    constructor CreateCopy(ASource: TDebuggerPropertiesConfig; ACopyPropValues: Boolean = True; ACopyXmlOrigin: Boolean = False);

    procedure CopyFrom(ASource: TDebuggerPropertiesConfig; ACopyPropValues: Boolean = True);
    procedure AssignTo(Dest: TPersistent); override;
    function DisplayName: String;
    function NeedsExePath: Boolean;
    procedure ChangeDebuggerClass(ADebuggerClass: TDebuggerClass; ACopyPropValues: Boolean = True);
    procedure MarkAsDeleted;
    function IsLoaded: Boolean;
    function IsDeleted: Boolean;
    function DebugText: String;

    procedure DeleteFromXml(AXMLCfg: TRttiXMLConfig; APath: String); // uses FXmlIndex from last load/save. No prior sibling must have benn removed or inserted
    procedure DeleteFromOldXml(AXMLCfg: TRttiXMLConfig; APath: String);
    procedure SaveToXml(AXMLCfg: TRttiXMLConfig; APath: String; AIndex: Integer);
    procedure SaveToOldXml(AXMLCfg: TRttiXMLConfig; APath: String);

    property DebuggerClass: TDebuggerClass read FDebuggerClass;
    property DebuggerProperties: TDebuggerProperties read FDebuggerProperties;
    property IsFromOldXml: Boolean read FIsFromOldXml;
  published
    property ConfigName: String read FConfigName write FConfigName;
    property ConfigClass: String read FConfigClass write FConfigClass;
    property ConfigClassInOldXml: String read FConfigClassInOldXml;
    property DebuggerFilename: string read FDebuggerFilename write FDebuggerFilename;
    property Active: Boolean read FActive write FActive;
    property UID: String read FUID write FUID;
  end;

  { TDebuggerPropertiesConfigList }

  TDebuggerPropertiesConfigList = class(TStringListUTF8Fast)
  private const
    XML_PATH_DEBUGGER_CONF     = 'Configs/Config[%d]/';
    XML_PATH_DEBUGGER_CONF_OLD = 'Class%s/%s/';
  private
    FHasActiveDebuggerEntry: Boolean;
    FKnownDebuggerClassCount: Integer;
    FCurrentDebuggerPropertiesConfig: TDebuggerPropertiesConfig;

    function GetOpt(Index: Integer): TDebuggerPropertiesConfig;
    procedure SetCurrentDebuggerPropertiesOpt(AValue: TDebuggerPropertiesConfig
      );
  public
    procedure LoadFromXml(AXMLCfg: TRttiXMLConfig; APath: String; AnOldFileNamePath: String = '');
    procedure SaveToXml(AXMLCfg: TRttiXMLConfig; APath: String; AnOldFileNamePath: String = '');

    procedure ClearAll;
    function CountWithoutDeleted: Integer;
    function EntryByName(AConfName, AConfClass: String): TDebuggerPropertiesConfig;
    function EntryByUid(AnUid: String): TDebuggerPropertiesConfig;
    property Opt[Index: Integer]: TDebuggerPropertiesConfig read GetOpt;
    property HasActiveDebuggerEntry: Boolean read FHasActiveDebuggerEntry write FHasActiveDebuggerEntry; // for the initial setup dialog / entry may be of unknown class
    property CurrentDebuggerPropertiesConfig: TDebuggerPropertiesConfig read FCurrentDebuggerPropertiesConfig write SetCurrentDebuggerPropertiesOpt;
  end;

  { TDebuggerOptions }

  TDebuggerOptions = class(TAbstractIDEEnvironmentOptions)
  private
    FFilename: string;
    FBackendConverterConfig: TIdeDbgValueConvertSelectorList;
    FHasActiveDebuggerEntry: Boolean;
    FPrimaryConfigPath: String;
    FXMLCfg: TRttiXMLConfig;

    FDebuggerConfigList: TDebuggerPropertiesConfigList; // named entries

    function GetCurrentDebuggerPropertiesConfig: TDebuggerPropertiesConfig;
    procedure SetCurrentDebuggerPropertiesOpt(AValue: TDebuggerPropertiesConfig);
    procedure LoadDebuggerProperties;
  protected
    procedure InitXMLCfg(CleanConfig: boolean);
  public
    class function GetGroupCaption:string; override;
    class function GetInstance: TAbstractIDEOptions; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    function GetDefaultConfigFilename: string;
    procedure CreateConfig;

    property Filename: string read FFilename;
    property PrimaryConfigPath: String read FPrimaryConfigPath write FPrimaryConfigPath;

    property BackendConverterConfig: TIdeDbgValueConvertSelectorList read FBackendConverterConfig write FBackendConverterConfig;

    function DebuggerFilename: string;
    function GetParsedDebuggerFilename(AProjectDbgFileName: String = ''): string;

    procedure SaveDebuggerPropertiesList;
    function  DebuggerPropertiesConfigList: TDebuggerPropertiesConfigList;
    function  CurrentDebuggerClass: TDebuggerClass;
    function  CurrentDebuggerPropertiesConfigEx(AnUID: String = ''): TDebuggerPropertiesConfig;
    property  CurrentDebuggerPropertiesConfig: TDebuggerPropertiesConfig read GetCurrentDebuggerPropertiesConfig write SetCurrentDebuggerPropertiesOpt;
    property  HasActiveDebuggerEntry: Boolean read FHasActiveDebuggerEntry write FHasActiveDebuggerEntry; // for the initial setup dialog / entry may be of unknown class
    //property  DebuggerConfig: TDebuggerConfigStore read FDebuggerConfig;
  end;

function GetDebuggerOptions: TDebuggerOptions;
property DebuggerOptions: TDebuggerOptions read GetDebuggerOptions;


implementation

const
  DebuggerOptsConfFileName = 'debuggeroptions.xml';
var
  TheDebuggerOptions: TDebuggerOptions = nil;

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
  AXMLCfg: TRttiXMLConfig; APath: String; AIndex: Integer);
begin
  Create;
  FIsFromOldXml := False;
  FFLags := [];

  APath := Format(APath, [AIndex]);
  AXMLCfg.ReadObject(APath, Self);
  FXmlIndex := AIndex;

  FDebuggerClass := TBaseDebugManagerIntf.DebuggersByClassName[ConfigClass];
  if FDebuggerClass <> nil then begin
    FDebuggerProperties := FDebuggerClass.CreateProperties;
    AXMLCfg.ReadObject(APath + 'Properties/', FDebuggerProperties);
    FFLags := [dpcLoaded];
  end;

  InitUID;
end;

constructor TDebuggerPropertiesConfig.CreateFromOldXmlConf(
  AXMLCfg: TRttiXMLConfig; APath: String; ADebuggerClass: TDebuggerClass;
  AForceLoad: Boolean);
var
  p: String;
begin
  Create;
  FIsFromOldXml := True;
  FFLags := [];

  p := Format(APath, [ADebuggerClass.ClassName, 'Config']);
  if AXMLCfg.HasPath(p, False) then
    AForceLoad := True;
  // Read first, so any (invalid) Class/Name will be cleared after reading
  AXMLCfg.ReadObject(p, Self);  // read FDebuggerFilename;

  FConfigClass := ADebuggerClass.ClassName;
  FConfigClassInOldXml := FConfigClass;
  FConfigName := '';
  FXmlIndex := -1;

  APath := Format(APath, [FConfigClass, 'Properties']);
  if AForceLoad or AXMLCfg.HasPath(APath, False) then begin
    FDebuggerClass := ADebuggerClass;
    FDebuggerProperties := ADebuggerClass.CreateProperties;
    AXMLCfg.ReadObject(APath, FDebuggerProperties);
    FFLags := [dpcLoaded];
  end;

  InitUID;
end;

constructor TDebuggerPropertiesConfig.CreateFromOldXmlConf(
  AXMLCfg: TRttiXMLConfig; APath: String; ADebuggerClassName: String;
  AForceLoad: Boolean);
var
  p: String;
begin
  Create;
  FIsFromOldXml := True;
  FFLags := [];

  p := Format(APath, [ADebuggerClassName, 'Config']);
  if AXMLCfg.HasPath(p, False) then
    AForceLoad := True;
  // Read first, so any (invalid) Class/Name will be cleared after reading
  AXMLCfg.ReadObject(p, Self);  // read FDebuggerFilename;

  FConfigClass := ADebuggerClassName;
  FConfigClassInOldXml := FConfigClass;
  FConfigName := '';
  FXmlIndex := -1;

  FDebuggerClass := TBaseDebugManagerIntf.DebuggersByClassName[ConfigClass];
  APath := Format(APath, [FConfigClass, 'Properties']);
  if (FDebuggerClass <> nil) and (AForceLoad or AXMLCfg.HasPath(APath, False)) then begin
    FDebuggerProperties := FDebuggerClass.CreateProperties;
    AXMLCfg.ReadObject(APath, FDebuggerProperties);
    FFLags := [dpcLoaded];
  end;

  InitUID;
end;

constructor TDebuggerPropertiesConfig.CreateForDebuggerClass(
  ADebuggerClass: TDebuggerClass);
begin
  Create;
  FIsFromOldXml := False;
  FXmlIndex := -1;

  FDebuggerClass := ADebuggerClass;
  FConfigClass := ADebuggerClass.ClassName;
  FConfigName := '';
  FDebuggerProperties := ADebuggerClass.CreateProperties;
  FFLags := [dpcLoaded]; // i.e. treat as loaded, save when saving all

  InitUID;
end;

constructor TDebuggerPropertiesConfig.CreateCopy(
  ASource: TDebuggerPropertiesConfig; ACopyPropValues: Boolean;
  ACopyXmlOrigin: Boolean);
begin
  Create;
  CopyFrom(ASource, ACopyPropValues);
  if ACopyXmlOrigin then begin
    FIsFromOldXml := ASource.FIsFromOldXml;
    FXmlIndex     := ASource.FXmlIndex;
    FUID          := ASource.FUID;
  end
  else begin
    FIsFromOldXml := False;
    FXmlIndex     := -1;
  end;
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

  if not IsDeleted then begin
    FreeAndNil(FDebuggerProperties);
    if ASource.DebuggerClass <> nil then
      FDebuggerProperties := ASource.DebuggerClass.CreateProperties;
    if ACopyPropValues and (ASource.FDebuggerProperties <> nil) then
      FDebuggerProperties.Assign(ASource.FDebuggerProperties);
  end;

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
  assert(IsLoaded and not IsDeleted, 'TDebuggerPropertiesConfig.ChangeDebuggerClass: IsLoaded');
  FDebuggerClass := ADebuggerClass;
  FConfigClass := ADebuggerClass.ClassName;
  p := FDebuggerProperties;
  FDebuggerProperties := ADebuggerClass.CreateProperties;
  if ACopyPropValues and (p <> nil) then
    FDebuggerProperties.Assign(p);
  p.Free;
end;

procedure TDebuggerPropertiesConfig.MarkAsDeleted;
begin
  FreeAndNil(FDebuggerProperties);
  FFLags := FFLags + [dpcDeleted];
end;

function TDebuggerPropertiesConfig.IsLoaded: Boolean;
begin
  Result := dpcLoaded in FFLags; // (FDebuggerClass <> nil) and (FDebuggerProperties <> nil);
end;

function TDebuggerPropertiesConfig.IsDeleted: Boolean;
begin
  Result := dpcDeleted in FFLags; // (FDebuggerClass <> nil) and (FDebuggerProperties = nil);
end;

function TDebuggerPropertiesConfig.DebugText: String;
begin
  if Self = nil then
    exit('NIL');
  Result := Format('C-Name: %s, C-Class: %s, Class %s, Prop %s, Xml: %d %s, Path: %s',
    [FConfigName, FConfigClass, DbgSName(FDebuggerClass), dbgs(FDebuggerProperties),
     FXmlIndex, dbgs(FIsFromOldXml), FDebuggerFilename]);
end;

procedure TDebuggerPropertiesConfig.DeleteFromXml(AXMLCfg: TRttiXMLConfig;
  APath: String);
begin
  if FXmlIndex < 0 then
    exit;
  APath := Format(APath, [FXmlIndex]);
  FXmlIndex := -1;

  AXMLCfg.DeletePath(APath);
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
  FXmlIndex := -1;
  FIsFromOldXml := False;
end;

procedure TDebuggerPropertiesConfig.SaveToXml(AXMLCfg: TRttiXMLConfig;
  APath: String; AIndex: Integer);
var
  PropDef: TDebuggerProperties;
  OptDef: TDebuggerPropertiesConfig;
begin
  APath := Format(APath, [AIndex]);
  FIsFromOldXml := False;
  FXmlIndex := AIndex;

  OptDef := TDebuggerPropertiesConfig.Create;
  AXMLCfg.WriteObject(APath, Self, OptDef);
  OptDef.Free;

  if FDebuggerProperties <> nil then begin
    PropDef := FDebuggerClass.CreateProperties;
    AXMLCfg.WriteObject(APath + 'Properties/', FDebuggerProperties, PropDef);
    PropDef.Free;
  end;
end;

procedure TDebuggerPropertiesConfig.SaveToOldXml(AXMLCfg: TRttiXMLConfig;
  APath: String);
var
  PropDef: TDebuggerProperties;
  OptDef: TDebuggerPropertiesConfig;
begin
  FIsFromOldXml := True;
  FConfigClassInOldXml := FConfigClass;
  FXmlIndex := -1;

  OptDef := TDebuggerPropertiesConfig.Create;
  OptDef.ConfigName := ConfigName;   // Do not write Name
  // ConfigClass will differ and be written. This ensures that even an unmodified config is written (to preserve its existence)
  AXMLCfg.WriteObject(Format(APath, [FConfigClass, 'Config']), Self, OptDef);
  OptDef.Free;

  if FDebuggerProperties <> nil then begin
    APath := Format(APath, [FConfigClass, 'Properties']);
    PropDef := FDebuggerClass.CreateProperties;
    AXMLCfg.WriteObject(APath, FDebuggerProperties, PropDef);
    PropDef.Free;
  end;
end;

{ TDebuggerPropertiesConfigList }

function TDebuggerPropertiesConfigList.GetOpt(Index: Integer): TDebuggerPropertiesConfig;
begin
  Result := TDebuggerPropertiesConfig(Objects[Index]);
end;

procedure TDebuggerPropertiesConfigList.SetCurrentDebuggerPropertiesOpt(
  AValue: TDebuggerPropertiesConfig);
begin
  if FCurrentDebuggerPropertiesConfig = AValue then Exit;

  if (AValue <> nil) and (IndexOfObject(AValue) < 0) then
    AddObject(AValue.ConfigName, AValue);
  FCurrentDebuggerPropertiesConfig := AValue;
end;

procedure TDebuggerPropertiesConfigList.LoadFromXml(AXMLCfg: TRttiXMLConfig;
  APath: String; AnOldFileNamePath: String);
var
  ConfCount, i: Integer;
  DbgClassType: TDebuggerClass;
  Entry, UnloadedCurrent: TDebuggerPropertiesConfig;
  ActiveClassName, CurFilename: String;
  ActiveClassSeen: Boolean;
begin
  if (Count > 0) and
     (TBaseDebugManagerIntf.DebuggerCount = FKnownDebuggerClassCount)
  then
    exit;
  FKnownDebuggerClassCount := TBaseDebugManagerIntf.DebuggerCount;
  HasActiveDebuggerEntry := False;


  ClearAll;
  FCurrentDebuggerPropertiesConfig := nil;
  UnloadedCurrent := nil;

  // Load new style entries
  ConfCount := AXMLCfg.GetListItemCount(APath + 'Configs/', 'Config', False);
  for i := 1 to ConfCount do begin
    Entry := TDebuggerPropertiesConfig.CreateFromXmlConf(AXMLCfg, APath + XML_PATH_DEBUGGER_CONF, i);
    AddObject(Entry.ConfigName, Entry);
    if Entry.Active then
      HasActiveDebuggerEntry := True;
    if Entry.Active and Entry.IsLoaded and (FCurrentDebuggerPropertiesConfig = nil) then
      FCurrentDebuggerPropertiesConfig := Entry;
    if Entry.Active and (UnloadedCurrent = nil) then
      UnloadedCurrent := Entry;
  end;

  if FCurrentDebuggerPropertiesConfig = nil then
    FCurrentDebuggerPropertiesConfig := UnloadedCurrent;

  // Read old style, per class
  if (AnOldFileNamePath <> '') then begin
    ActiveClassName := '';
    ActiveClassSeen := False;
    if FCurrentDebuggerPropertiesConfig = nil then
      ActiveClassName := AXMLCfg.GetValue(APath + 'Class', '');
    HasActiveDebuggerEntry := HasActiveDebuggerEntry or (ActiveClassName <> '');
    // There is only one filename for all classes
    CurFilename:=AXMLCfg.GetValue(AnOldFileNamePath, '');

    for i := 0 to TBaseDebugManagerIntf.DebuggerCount  -1 do begin
      DbgClassType := TBaseDebugManagerIntf.Debuggers[i];
      ActiveClassSeen := ActiveClassSeen or (CompareText(DbgClassType.ClassName, ActiveClassName)=0);
      Entry := TDebuggerPropertiesConfig.CreateFromOldXmlConf(AXMLCfg, APath + XML_PATH_DEBUGGER_CONF_OLD,
        DbgClassType, CompareText(DbgClassType.ClassName, ActiveClassName)=0);
      if not Entry.IsLoaded then begin
        Entry.Free;
        Continue;
      end;
      if (Entry.DebuggerFilename = '') and (Entry.NeedsExePath or (not Entry.IsLoaded)) then
        Entry.DebuggerFilename := CurFilename;
      AddObject(Entry.ConfigName, Entry);
      if (Entry.ConfigClass = ActiveClassName) and (FCurrentDebuggerPropertiesConfig = nil) then
        FCurrentDebuggerPropertiesConfig := Entry;
    end;
  end;
end;

procedure TDebuggerPropertiesConfigList.SaveToXml(AXMLCfg: TRttiXMLConfig;
  APath: String; AnOldFileNamePath: String);
var
  i, ConfCount: Integer;
  Entry: TDebuggerPropertiesConfig;
begin
  (* Delete old entries
     If an entry was loaded for a DebuggerClass that is currently unknown (package not compiled into IDE)
     then the entry did not load its properties. Therefore such entries "not Entry.IsLoaded"
     must not be deleted.
     Loop from the highest Index, so deleting an entry will not change the Xml-Index of
     the Indexes still to loop over.
   *)
  for i := Count - 1 downto 0 do begin
    // Delete last entry first
    Entry := Opt[i];
    if not Entry.IsLoaded then
      Continue;

    if (not Entry.IsFromOldXml) then
      Entry.DeleteFromXml(AXMLCfg, APath + XML_PATH_DEBUGGER_CONF) // will be rewritten
    else
    if Entry.IsDeleted or
       (Entry.ConfigName <> '') or // Moved to named list
       (Entry.ConfigClass <> Entry.ConfigClassInOldXml)
    then
      Entry.DeleteFromOldXml(AXMLCfg, APath + XML_PATH_DEBUGGER_CONF_OLD);

    if Entry.IsDeleted then begin
      Entry.Free;
      Delete(i);
    end;
  end;

  ConfCount := AXMLCfg.GetListItemCount(APath + 'Configs/', 'Config', False) + 1;
  for i := 0 to Count - 1 do begin
    Entry := Opt[i];
    if not Entry.IsLoaded then
      Continue;

    Entry.Active := Entry = FCurrentDebuggerPropertiesConfig;
    if(Entry.ConfigName <> '') then begin
      Entry.SaveToXml(AXMLCfg, APath + XML_PATH_DEBUGGER_CONF, ConfCount);
      inc(ConfCount);
    end
    else begin
      Entry.SaveToOldXml(AXMLCfg, APath + XML_PATH_DEBUGGER_CONF_OLD);
      // For compatibility
      if Entry.Active and (AnOldFileNamePath <> '') then
        AXMLCfg.SetDeleteValue(AnOldFileNamePath, Entry.DebuggerFilename,'');
    end;
  end;

  // compatibility
  if (FCurrentDebuggerPropertiesConfig <> nil) and (FCurrentDebuggerPropertiesConfig.ConfigName = '') then
    AXMLCfg.SetValue(APath + 'Class', FCurrentDebuggerPropertiesConfig.ConfigClass)
  else
    AXMLCfg.DeleteValue(APath + 'Class')
end;

procedure TDebuggerPropertiesConfigList.ClearAll;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Objects[i].Free;
  Clear;
end;

function TDebuggerPropertiesConfigList.CountWithoutDeleted: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if not Opt[i].IsDeleted then
      inc(Result);
end;

function TDebuggerPropertiesConfigList.EntryByName(AConfName, AConfClass: String
  ): TDebuggerPropertiesConfig;
var
  i: Integer;
  dpCfg: TDebuggerPropertiesConfig;
begin
  Result := nil;
  i := Count - 1;
  while i >= 0 do begin
    dpCfg := Opt[i];
    if (not dpCfg.IsDeleted) and dpCfg.IsLoaded
    and (dpCfg.ConfigName = AConfName)
    and (dpCfg.ConfigClass = AConfClass) then
      Break;
    dec(i);
  end;
  if i >= 0 then
    Result := dpCfg;
end;

function TDebuggerPropertiesConfigList.EntryByUid(AnUid: String
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

{ TDebuggerOptions }

procedure TDebuggerOptions.InitXMLCfg(CleanConfig: boolean);
begin
  FreeAndNil(FXMLCfg);
  if CleanConfig then
    FXMLCfg:=TRttiXMLConfig.CreateClean(Filename)
  else
    FXMLCfg:=TRttiXMLConfig.Create(Filename);
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
  FDebuggerConfigList := TDebuggerPropertiesConfigList.Create;
  BackendConverterConfig := TIdeDbgValueConvertSelectorList.Create;
end;

destructor TDebuggerOptions.Destroy;
begin
  inherited Destroy;
  BackendConverterConfig.Free;

  FDebuggerConfigList.ClearAll;
  FreeAndNil(FDebuggerConfigList);

  FXMLCfg.Free;
end;

procedure TDebuggerOptions.Load;
var
  Path: String;
begin
  InitXMLCfg(False);

  Path := 'Debugger/';

  FBackendConverterConfig.LoadDataFromXMLConfig(FXMLCfg, Path + 'FpDebug/ValueConvert/');
end;

procedure TDebuggerOptions.Save;
var
  Path: String;
begin
  InitXMLCfg(False); // Dont delete old content
  Path := 'Debugger/';

  if FBackendConverterConfig.Changed then
    FBackendConverterConfig.SaveDataToXMLConfig(FXMLCfg, Path + 'FpDebug/ValueConvert/');
  FBackendConverterConfig.Changed := False;

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

procedure TDebuggerOptions.SetCurrentDebuggerPropertiesOpt(
  AValue: TDebuggerPropertiesConfig);
begin
  LoadDebuggerProperties;
  FDebuggerConfigList.CurrentDebuggerPropertiesConfig := AValue;
end;

procedure TDebuggerOptions.LoadDebuggerProperties;
begin
  FDebuggerConfigList.LoadFromXml(EnvironmentOptions.XMLCfg, 'EnvironmentOptions/Debugger/', 'EnvironmentOptions/DebuggerFilename/Value');
  HasActiveDebuggerEntry := FDebuggerConfigList.HasActiveDebuggerEntry;
end;

procedure TDebuggerOptions.SaveDebuggerPropertiesList;
begin
  FDebuggerConfigList.SaveToXml(EnvironmentOptions.XMLCfg, 'EnvironmentOptions/Debugger/', 'EnvironmentOptions/DebuggerFilename/Value');
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

