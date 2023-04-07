unit TestXmlOpts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IdeDebuggerOpts, EnvironmentOpts, DbgIntfDebuggerBase,
  Laz2_DOM, Laz2_XMLRead, Laz2_XPath, fpcunit,
  testregistry;

type

  { TTestDebuggerBackend }

  TTestDebuggerBackend = class(TDebuggerIntf)
    class function Caption: String; override;                       // 'My'+Classname
    class function NeedsExePath: boolean; override;                 // True
    class function CreateProperties: TDebuggerProperties; override; // nil
    class function SupportedFeatures: TDBGFeatures; override;       // []
  end;


  { TTestDebuggerBackendAbc }

  TTestDebuggerBackendAbc = class(TTestDebuggerBackend)
  type
    TTestDebuggerPropertiesAbc = class(TDebuggerProperties)
    private
      FAbcProp: boolean;
      FAbcVal: Integer;
    published
      property AbcProp: boolean read FAbcProp write FAbcProp;
      property AbcVal: Integer read FAbcVal write FAbcVal;
    end;
  public
    class function CreateProperties: TDebuggerProperties; override;
  end;

  { TTestDebuggerBackendFoo }

  TTestDebuggerBackendFoo = class(TTestDebuggerBackend)
  type
    TTestDebuggerPropertiesFoo = class(TDebuggerProperties)
    private
      FFooProp: integer;
    published
      property FooProp: integer read FFooProp write FFooProp;
    end;
  public
    class function CreateProperties: TDebuggerProperties; override;
  end;

  { TTestDebuggerBackendBar }

  TTestDebuggerBackendBar = class(TTestDebuggerBackend)
  public
    class function CreateProperties: TDebuggerProperties; override;
  end;

  { TTestDebuggerOptions }

  TTestDebuggerOptions = class(TDebuggerOptions)
  public
    procedure InitXMLCfg(CleanConfig: boolean);
    property XMLCfg;
    function EntryCntByName(AName: String; AnUnloaded: Boolean = False): Integer;
    function EntryCntByClass(AClass: String; AnUnloaded: Boolean = False): Integer;
    function EntryCntByNameAndClass(AName, AClass: String; AnUnloaded: Boolean = False): Integer;
    function EntryByName(AName: String; AnUnloaded: Boolean = False): TDebuggerPropertiesConfig;
    function EntryByClass(AClass: String; AnUnloaded: Boolean = False): TDebuggerPropertiesConfig;
    function EntryByNameAndClass(AName,AClass: String; AnUnloaded: Boolean = False): TDebuggerPropertiesConfig;
  end;

  { TTestXmlOpts }

  TTestXmlOpts = class(TTestCase)
  private
    function  ParseXml(s: String): TXMLDocument;
    procedure CheckXPath(Node: TXMLDocument; XPath: string; ExpCount: integer; ExpFirstTagName: string = '');
    procedure CheckXPath(Node: TDOMElement; XPath: string; ExpCount: integer; ExpFirstTagName: string = '');

    procedure InitEnvOpts(s: String);
    function InitDbgOpts(s: String): TTestDebuggerOptions;
    function EnvOptsToString(e: TEnvironmentOptions = nil): String;
    function SaveEnvOpts(e: TEnvironmentOptions = nil): String;
    function SaveDbgOpts(o: TTestDebuggerOptions): String;
  protected
    procedure DebuglnDbgOptConfigs(opts: TTestDebuggerOptions);
    procedure TearDown; override;
    procedure TestEntryCnt(AnOpts: TTestDebuggerOptions; ExpCount, ExpUnloadedCount: Integer);
    procedure TestHasEntry(AnOpts: TTestDebuggerOptions;
      ExpName, ExpClass: String;
      ExpActive: Boolean = False;
      ExpPath: String = '';
      ExpUUID: String = '';
      ExpLoaded: Boolean = True
      );
    procedure TestHasClassEntry(AnOpts: TTestDebuggerOptions;
      ExpClass: String;
      ExpActive: Boolean = False;
      ExpPath: String = '';
      ExpUUID: String = '';
      ExpLoaded: Boolean = True
      );
    procedure TestEntryCnt(AnXml: TXMLDocument; ExpCount: Integer);
    procedure TestHasEntry(AnXml: TXMLDocument;
      ExpName, ExpClass: String;
      ExpActive: Boolean = False;
      ExpPath: String = '';
      ExpUUID: String = ''
      );
    procedure TestHasEntryWithoutProps(AnXml: TXMLDocument; ExpName: String);
    procedure TestHasEntryProps(AnXml: TXMLDocument; ExpName: String; ExpPropName, ExpPropVal: String);
    procedure TestHasClassOnlyEntryWithoutProps(AnXml: TXMLDocument; ExpClass: String);
    procedure TestHasClassOnlyEntryProps(AnXml: TXMLDocument; ExpClass: String; ExpPropName, ExpPropVal: String);
  published
    procedure TestLoadEnvNew;
    procedure TestLoadEnvOld;
    procedure TestLoadEnvMixed;
    procedure TestLoadEnvNewUnknowActive;
    procedure TestLoadEnvOldUnknowActive;
    procedure TestLoadFromDbg; // Check options from DebuggerOptions are loaded, and not EnvironmentOpts
    procedure TestDeleteSave;
  end;

implementation

function EntryNew(AName, AClassName, APath: string; AnActive: boolean;
  AProps: String = ''; AnUUID: String = ''): String;
begin
  Result :=
    LineEnding +
    '<Config ';
  if AName <> ''  then  Result := Result + '  ConfigName="' + AName + '" ';
                        Result := Result + '  ConfigClass="' + AClassName + '" ';
  if APath <> ''  then  Result := Result + '  DebuggerFilename="' + APath + '" ';
  if AnUUID <> '' then  Result := Result + '  UID ="{' + AnUUID + '}" ';
  if AnActive     then  Result := Result + '  Active="True" ';

  if AProps = ''  then
    Result := Result + ' />'
  else
    Result := Result + ' >' +
      AProps +
      '</Config>';
end;

function EntryOld(AClassName: string = ''; AProps: String = '';
  ANewConf: Boolean = False; APath: String = ''; AnUUID: String = ''): String;
begin
  Result :=
    LineEnding +
    '  <Class' + AClassName;

  if (not ANewConf) and (AProps = '') then begin
    Result := Result + '/>';
    exit;
  end;

  Result := Result + '>';

  // MIXED // 2.2
  //       <Config ConfigClass="TGDBMIDebugger" UID="{8925DE28-B8D5-4D46-B778-3FDA2EE36AAE}" DebuggerFilename="C:\FPC\GDB_x86_64\unicode\9.2\gdb.exe"/>
  if ANewConf then
    Result := Result + EntryNew('', AClassName, APath, False, AProps, AnUUID);

  Result := Result +
    AProps +
    '  </Class' + AClassName + '>';
end;

(* ************************************************************************** *)

const
  PROPS_ABC1 = '<Properties AbcVal="123" AbcProp="True" />';
  PROPS_ABC2 = '<Properties AbcVal="223" AbcProp="True" />';
  PROPS_FOO1 = '<Properties FooProp="987" />';
  PROPS_FOO2 = '<Properties FooProp="887" />';
  PROPS_FOO3 = '<Properties FooProp="787" />';
  PROPS_UNK1 = '<Properties SomeProp="100" SomeVal="False" SomeTxt="" />';
  PROPS_UNK2 = '<Properties AnyProp="abc" />';


(* ************************************************************************** *)

function ConfigEnvNew: String;
begin
  Result :=
    '<?xml version="1.0"?>' +
    '<CONFIG>' +
    '  <EnvironmentOptions>' +
    '    <Version Value="110" Lazarus="2.3.0"/>' +
    '    <Debugger>' +
    '      <Configs>' +
    EntryNew('abc1', 'TTestDebuggerBackendAbc', 'abc.exe', False, '',         'U-1') +
    EntryNew('abc2', 'TTestDebuggerBackendAbc', 'abc.exe', False, PROPS_ABC1, 'U-2') +
    EntryNew('abc3', 'TTestDebuggerBackendAbc', 'abc3.exe', True, '',         'U-3') +
    EntryNew('foo1', 'TTestDebuggerBackendFoo', 'foo.exe', False, PROPS_FOO1, 'U-4') +
    EntryNew('xxx0', 'TUnknownDebuggerBackend', 'any.exe', False, PROPS_UNK1, 'U-5') +
    '      </Configs>' +
    '    </Debugger>' +
    '  </EnvironmentOptions>' +
    '</CONFIG>';
end;

function ConfigEnv1_8(NoDataForDefault: Boolean = False): String;
begin
  Result :=
    '<?xml version="1.0"?>' +
    '<CONFIG>' +
    '  <EnvironmentOptions>' +
    '    <Version Value="110" Lazarus="1.8.5"/>' +
    '    <Debugger Class="TTestDebuggerBackendAbc" EventLogLineLimit="100">' +
    EntryOld('TTestDebuggerBackendBar');
  if not NoDataForDefault
  then Result := Result +
    EntryOld('TTestDebuggerBackendAbc', PROPS_ABC1);
  Result := Result +
    EntryOld('TUnknownDebuggerBackend', PROPS_UNK1)+
    EntryOld('TTestDebuggerBackendFoo', PROPS_FOO1)+
    '    </Debugger>' +
    '    <DebuggerFilename Value="C:\gdb.exe">' +
    '      <History Count="1">' +
    '        <Item1 Value="$Path($(CompPath))\gdb.exe"/>' +
    '      </History>' +
    '    </DebuggerFilename>' +
    '  </EnvironmentOptions>' +
    '</CONFIG>';
end;

function ConfigEnv_Mixed(OldDefault: Boolean = False; NoOldClassAttr: Boolean = False): String;
begin
  Result :=
    '<?xml version="1.0"?>' +
    '<CONFIG>' +
    '  <EnvironmentOptions>' +
    '    <Version Value="110" Lazarus="1.8.5"/>';
  if NoOldClassAttr
  then Result := Result +
    '    <Debugger EventLogLineLimit="100">'
  else Result := Result +
    '    <Debugger Class="TTestDebuggerBackendAbc" EventLogLineLimit="100">';
  Result := Result +
    '      <Configs>' +
    EntryNew('abc1', 'TTestDebuggerBackendAbc', 'abc.exe', False, '',         'U-1') +
    EntryNew('abc2', 'TTestDebuggerBackendAbc', 'abc.exe', not OldDefault,  PROPS_ABC1, 'U-2') +
    EntryNew('abc3', 'TTestDebuggerBackendAbc', 'abc3.exe',False, '',         'U-3') +
    EntryNew('foo1', 'TTestDebuggerBackendFoo', 'foo.exe', False, PROPS_FOO1, 'U-4') +
    EntryNew('xxx0', 'TUnknownDebuggerBackend', 'any.exe', False, PROPS_UNK1, 'U-5') +
    EntryNew('xxx1', 'TNotFoundDebuggerBackend', 'no.exe', False, PROPS_ABC1, 'U-6') +
    '      </Configs>' +
    EntryOld('TTestDebuggerBackendBar', '',         True, 'mixbar', 'M-U1')+
    EntryOld('TTestDebuggerBackendAbc',  PROPS_ABC2)+
    EntryOld('TUnknownDebuggerBackend',  PROPS_UNK1)+
    EntryOld('TOtherDebuggerBackend', PROPS_FOO2)+
    EntryOld('TTestDebuggerBackendFoo',  PROPS_FOO3, True, 'mixfoo', 'M-U2')+
    '    </Debugger>' +
    '    <DebuggerFilename Value="C:\gdb.exe" />' +
    '  </EnvironmentOptions>' +
    '</CONFIG>';
end;

function ConfigEnvNewUnknowActive: String;
begin
  Result :=
    '<?xml version="1.0"?>' +
    '<CONFIG>' +
    '  <EnvironmentOptions>' +
    '    <Version Value="110" Lazarus="2.3.0"/>' +
    '    <Debugger>' +
    '      <Configs>' +
    EntryNew('abc1', 'TTestDebuggerBackendAbc', 'abc.exe', False, '',         'U-1') +
    EntryNew('abc2', 'TTestDebuggerBackendAbc', 'abc.exe', False, PROPS_ABC1, 'U-2') +
    EntryNew('abc3', 'TTestDebuggerBackendAbc', 'abc3.exe', False, '',         'U-3') +
    EntryNew('foo1', 'TTestDebuggerBackendFoo', 'foo.exe', False, PROPS_FOO1, 'U-4') +
    EntryNew('xxx0', 'TUnknownDebuggerBackend', 'any.exe', True, PROPS_UNK1, 'U-5') +
    EntryNew('xxx1', 'TNotFoundDebuggerBackend', 'no.exe', False, PROPS_ABC1, 'U-6') +
    '      </Configs>' +
    '    </Debugger>' +
    '  </EnvironmentOptions>' +
    '</CONFIG>';
end;

function ConfigEnv1_8_UnknownActive: String;
begin
  Result :=
    '<?xml version="1.0"?>' +
    '<CONFIG>' +
    '  <EnvironmentOptions>' +
    '    <Version Value="110" Lazarus="1.8.5"/>' +
    '    <Debugger Class="TUnknownDebuggerBackend" EventLogLineLimit="100">' +
    EntryOld('TTestDebuggerBackendBar') +
    EntryOld('TTestDebuggerBackendAbc', PROPS_ABC1) +
    EntryOld('TUnknownDebuggerBackend', PROPS_UNK1)+
    EntryOld('TTestDebuggerBackendFoo', PROPS_FOO1)+
    '    </Debugger>' +
    '    <DebuggerFilename Value="C:\gdb.exe">' +
    '      <History Count="1">' +
    '        <Item1 Value="$Path($(CompPath))\gdb.exe"/>' +
    '      </History>' +
    '    </DebuggerFilename>' +
    '  </EnvironmentOptions>' +
    '</CONFIG>';
end;
function ConfigDbgNewOne: String;
begin
  Result :=
    '<?xml version="1.0"?>' +
    '<CONFIG>' +
    '<Debugger>' +
    '  <Backends>' +
    EntryNew('new', 'TTestDebuggerBackendAbc', 'new.exe', True, '',         'U-99') +
    '  </Backends>' +
    '</Debugger>' +
    '</CONFIG>';
end;

function ConfigDbgNew: String;
begin
  Result :=
    '<?xml version="1.0"?>' +
    '<CONFIG>' +
    '<Debugger>' +
    '  <Backends>' +
    EntryNew('abc1', 'TTestDebuggerBackendAbc', 'abc.exe', False, '',         'U-1') +
    EntryNew('abc2', 'TTestDebuggerBackendAbc', 'abc.exe', False, PROPS_ABC1, 'U-2') +
    EntryNew('abc3', 'TTestDebuggerBackendAbc', 'abc3.exe', True, '',         'U-3') +
    EntryNew('foo1', 'TTestDebuggerBackendFoo', 'foo.exe', False, PROPS_FOO1, 'U-4') +
    EntryNew('xxx0', 'TUnknownDebuggerBackend', 'any.exe', False, PROPS_UNK1, 'U-5') +
    EntryNew('xxx1', 'TNotFoundDebuggerBackend', 'no.exe', False, PROPS_UNK2, 'U-6') +
    EntryNew('zbar', 'TTestDebuggerBackendBar', 'bar.exe', False, '', 'U-7') +
    '  </Backends>' +
    '</Debugger>' +
    '</CONFIG>';
end;


const

  CONF_EMPTY =
    '<?xml version="1.0"?>' +
    '<CONFIG/>' ;

  CONF_DBG_EMPTY =
    '<?xml version="1.0"?>' +
    '<CONFIG>' +
    '  <Debugger>' +
    '  </Debugger>' +
    '</CONFIG>' ;

{ TTestDebuggerBackend }

class function TTestDebuggerBackend.Caption: String;
begin
  Result := 'My'+ClassName;
end;

class function TTestDebuggerBackend.NeedsExePath: boolean;
begin
  Result := True;
end;

class function TTestDebuggerBackend.CreateProperties: TDebuggerProperties;
begin
  Result := nil;
end;

class function TTestDebuggerBackend.SupportedFeatures: TDBGFeatures;
begin
  Result := inherited SupportedFeatures;
end;

{ TTestDebuggerBackendAbc }

class function TTestDebuggerBackendAbc.CreateProperties: TDebuggerProperties;
begin
  Result := TTestDebuggerPropertiesAbc.Create;
end;

{ TTestDebuggerBackendFoo }

class function TTestDebuggerBackendFoo.CreateProperties: TDebuggerProperties;
begin
  Result := TTestDebuggerPropertiesFoo.Create;
end;

{ TTestDebuggerBackendBar }

class function TTestDebuggerBackendBar.CreateProperties: TDebuggerProperties;
begin
  Result := nil;
end;


{ TTestDebuggerOptions }

procedure TTestDebuggerOptions.InitXMLCfg(CleanConfig: boolean);
begin
  inherited InitXMLCfg(CleanConfig);
end;

function TTestDebuggerOptions.EntryCntByName(AName: String; AnUnloaded: Boolean
  ): Integer;
var
  i: Integer;
  l: TDebuggerPropertiesConfigListBase;
begin
  l := DebuggerPropertiesConfigList;
  if AnUnloaded then l := DebuggerPropertiesConfigList.Unloaded;

  Result := 0;
  for i := 0 to l.Count - 1 do
    if l.Opt[i].ConfigName = AName then
      inc(Result);
end;

function TTestDebuggerOptions.EntryCntByClass(AClass: String;
  AnUnloaded: Boolean): Integer;
var
  i: Integer;
  l: TDebuggerPropertiesConfigListBase;
begin
  l := DebuggerPropertiesConfigList;
  if AnUnloaded then l := DebuggerPropertiesConfigList.Unloaded;

  Result := 0;
  for i := 0 to l.Count - 1 do
    if l.Opt[i].ConfigClass = AClass then
      inc(Result);
end;

function TTestDebuggerOptions.EntryCntByNameAndClass(AName, AClass: String;
  AnUnloaded: Boolean): Integer;
var
  i: Integer;
  l: TDebuggerPropertiesConfigListBase;
begin
  l := DebuggerPropertiesConfigList;
  if AnUnloaded then l := DebuggerPropertiesConfigList.Unloaded;

  Result := 0;
  for i := 0 to l.Count - 1 do
    if (l.Opt[i].ConfigName = AName) and
       (l.Opt[i].ConfigClass = AClass)
    then
      inc(Result);
end;

function TTestDebuggerOptions.EntryByName(AName: String; AnUnloaded: Boolean
  ): TDebuggerPropertiesConfig;
var
  i: Integer;
  l: TDebuggerPropertiesConfigListBase;
begin
  l := DebuggerPropertiesConfigList;
  if AnUnloaded then l := DebuggerPropertiesConfigList.Unloaded;

  Result := nil;
  for i := 0 to l.Count - 1 do
    if l.Opt[i].ConfigName = AName then
      exit(l.Opt[i]);
end;

function TTestDebuggerOptions.EntryByClass(AClass: String; AnUnloaded: Boolean
  ): TDebuggerPropertiesConfig;
var
  i: Integer;
  l: TDebuggerPropertiesConfigListBase;
begin
  l := DebuggerPropertiesConfigList;
  if AnUnloaded then l := DebuggerPropertiesConfigList.Unloaded;

  Result := nil;
  for i := 0 to l.Count - 1 do
    if l.Opt[i].ConfigClass = AClass then
      exit(l.Opt[i]);
end;

function TTestDebuggerOptions.EntryByNameAndClass(AName, AClass: String;
  AnUnloaded: Boolean): TDebuggerPropertiesConfig;
var
  i: Integer;
  l: TDebuggerPropertiesConfigListBase;
begin
  l := DebuggerPropertiesConfigList;
  if AnUnloaded then l := DebuggerPropertiesConfigList.Unloaded;

  Result := nil;
  for i := 0 to l.Count - 1 do
    if (l.Opt[i].ConfigName = AName) and
       (l.Opt[i].ConfigClass = AClass) then
      exit(l.Opt[i]);
end;


{ TTestXmlOpts }

function TTestXmlOpts.ParseXml(s: String): TXMLDocument;
var
  st: TStringStream;
begin
  st := TStringStream.Create(s);
  st.Position := 0;
  ReadXMLFile(Result, st);
  st.Free;
end;

procedure TTestXmlOpts.CheckXPath(Node: TXMLDocument; XPath: string;
  ExpCount: integer; ExpFirstTagName: string);
begin
  CheckXPath(Node.DocumentElement, XPath, ExpCount, ExpFirstTagName);
end;

procedure TTestXmlOpts.CheckXPath(Node: TDOMElement; XPath: string;
  ExpCount: integer; ExpFirstTagName: string);
var
  V: TXPathVariable;
  FirstNode: TDOMElement;
begin
  V:=nil;
  try
    V:=EvaluateXPathExpression(XPath,Node);
    AssertEquals('xpath="'+XPath+'": AsNodeSet',True,V.AsNodeSet<>nil);
    AssertEquals('xpath="'+XPath+'": AsNodeSet.Count',ExpCount,V.ASNodeSet.Count);
    if V.ASNodeSet.Count=0 then exit;
    FirstNode:=TDOMElement(V.AsNodeSet[0]);
    if ExpFirstTagName<>'' then begin
      AssertEquals('xpath="'+XPath+'": node',ExpFirstTagName,FirstNode.TagName);
    end;
  finally
    V.Free;
  end;
end;


procedure TTestXmlOpts.InitEnvOpts(s: String);
var
  st: TStringStream;
begin
  FreeAndNil(EnvironmentOptions);

  EnvironmentOptions := TEnvironmentOptions.Create;
  EnvironmentOptions.Load(True); // InitXmlCfg
  st := TStringStream.Create(s);
  EnvironmentOptions.XMLCfg.ReadFromStream(st);
  st.Free;
end;

function TTestXmlOpts.InitDbgOpts(s: String): TTestDebuggerOptions;
var
  st: TStringStream;
begin
  Result := TTestDebuggerOptions.Create;
  Result.InitXMLCfg(True);
  st := TStringStream.Create(s);
  Result.XMLCfg.ReadFromStream(st);
  st.Free;
end;

function TTestXmlOpts.EnvOptsToString(e: TEnvironmentOptions): String;
var
  st: TStringStream;
begin
  if e = nil then
    e := EnvironmentOptions;
  st := TStringStream.Create('');
  e.XMLCfg.WriteToStream(st);
  Result := st.DataString;
  st.Free;
end;

function TTestXmlOpts.SaveEnvOpts(e: TEnvironmentOptions): String;
begin
  if e = nil then
    e := EnvironmentOptions;
  assert(e.Filename='', 'TTestXmlOpts.SaveEnvOpts: e.Filename=''');
  e.Save(False);
  assert(e.Filename='', 'TTestXmlOpts.SaveEnvOpts: e.Filename=''');
  Result := EnvOptsToString(e);
end;

function TTestXmlOpts.SaveDbgOpts(o: TTestDebuggerOptions): String;
var
  st: TStringStream;
begin
  assert(o.Filename='', 'TTestXmlOpts.SaveDbgOpts: o.Filename=''');
  o.Save;
  assert(o.Filename='', 'TTestXmlOpts.SaveDbgOpts: o.Filename=''');
  st := TStringStream.Create('');
  o.XMLCfg.WriteToStream(st);
  Result := st.DataString;
  st.Free;
end;

procedure TTestXmlOpts.DebuglnDbgOptConfigs(opts: TTestDebuggerOptions);
var
  i: Integer;
begin
  writeln(opts.DebuggerPropertiesConfigList.Count);
  for i := 0 to opts.DebuggerPropertiesConfigList.Count - 1 do
    with opts.DebuggerPropertiesConfigList.Opt[i] do
      WriteLn(' # ', ConfigName, 'C=', ConfigClass, ' , A=', Active, '  , L=',IsLoaded);
end;

procedure TTestXmlOpts.TearDown;
begin
  FreeAndNil(EnvironmentOptions);
  inherited TearDown;
end;

procedure TTestXmlOpts.TestEntryCnt(AnOpts: TTestDebuggerOptions; ExpCount,
  ExpUnloadedCount: Integer);
begin
  AssertEquals('Opts-Cnt', ExpCount, AnOpts.DebuggerPropertiesConfigList.Count);
  AssertEquals('Opts-Cnt', ExpUnloadedCount, AnOpts.DebuggerPropertiesConfigList.Unloaded.Count);
end;

procedure TTestXmlOpts.TestHasEntry(AnOpts: TTestDebuggerOptions; ExpName,
  ExpClass: String; ExpActive: Boolean; ExpPath: String; ExpUUID: String;
  ExpLoaded: Boolean);
var
  e: TDebuggerPropertiesConfig;
begin
  if ExpName = #1 then begin
    AssertEquals('Opt Has Entry', 1, AnOpts.EntryCntByNameAndClass('', ExpClass, not ExpLoaded));
    AssertEquals('Opt Has Entry', 0, AnOpts.EntryCntByNameAndClass('', ExpClass, ExpLoaded));
    e := AnOpts.EntryByNameAndClass('', ExpClass, not ExpLoaded);
  end
  else
  if ExpName = '' then begin
    // Unnamed Entry for class => and NO other entries by that class
    AssertEquals('Opt Has Entry', 1, AnOpts.EntryCntByClass(ExpClass, not ExpLoaded));
    AssertEquals('Opt Has Entry', 0, AnOpts.EntryCntByClass(ExpClass, ExpLoaded));
    e := AnOpts.EntryByClass(ExpClass, not ExpLoaded);
    AssertEquals('name', '', e.ConfigName);
  end
  else begin
    AssertEquals('Opt Has Entry', 1, AnOpts.EntryCntByName(ExpName, not ExpLoaded));
    AssertEquals('Opt Has Entry', 0, AnOpts.EntryCntByName(ExpName, ExpLoaded));
    e := AnOpts.EntryByName(ExpName, not ExpLoaded);
  end;

  AssertEquals('class', ExpClass, e.ConfigClass);
  AssertEquals('active', ExpActive, e.Active);
  if ExpPath <> '' then
    AssertEquals('path', ExpPath, e.DebuggerFilename);
  if ExpUUID <> '' then
    AssertEquals('uuid', '{' + ExpUUID + '}', e.UID)
  else
    AssertTrue('uuid', e.UID <> '');
end;

procedure TTestXmlOpts.TestHasClassEntry(AnOpts: TTestDebuggerOptions;
  ExpClass: String; ExpActive: Boolean; ExpPath: String; ExpUUID: String;
  ExpLoaded: Boolean);
begin
  TestHasEntry(AnOpts, #1, ExpClass, ExpActive, ExpPath, ExpUUID, ExpLoaded);
end;

procedure TTestXmlOpts.TestEntryCnt(AnXml: TXMLDocument; ExpCount: Integer);
begin
  CheckXPath(AnXml, '//Debugger/Backends/Config', ExpCount);
end;

procedure TTestXmlOpts.TestHasEntry(AnXml: TXMLDocument; ExpName,
  ExpClass: String; ExpActive: Boolean; ExpPath: String; ExpUUID: String);
var
  n: String;
begin
  n := '@ConfigName=''' + ExpName + '''';
  if ExpName = '' then
    n := 'not(@ConfigName)';

  if ExpName <> '' then
    CheckXPath(AnXml, '//Debugger/Backends/Config[' + n + ']', 1);

  n := n + ' and @ConfigClass=''' + ExpClass + '''';
  CheckXPath(AnXml, '//Debugger/Backends/Config[' + n + ']', 1);
  if ExpActive then
    CheckXPath(AnXml, '//Debugger/Backends/Config[' + n + ' and @Active=''True'']', 1)
  else
    CheckXPath(AnXml, '//Debugger/Backends/Config[' + n + ' and @Active=''True'']', 0);
  if ExpPath <> '' then
    CheckXPath(AnXml, '//Debugger/Backends/Config[' + n + ' and @DebuggerFilename=''' + ExpPath + ''']', 1);
  if ExpUUID <> '' then
    CheckXPath(AnXml, '//Debugger/Backends/Config[' + n + ' and @UID =''{' + ExpUUID + '}'']', 1)
  else
    CheckXPath(AnXml, '//Debugger/Backends/Config[' + n + ' and @UID]', 1);
end;

procedure TTestXmlOpts.TestHasEntryWithoutProps(AnXml: TXMLDocument;
  ExpName: String);
begin
  CheckXPath(AnXml, '//Debugger/Backends/Config[@ConfigName=''' + ExpName + ''']', 1);
  CheckXPath(AnXml, '//Debugger/Backends/Config[@ConfigName=''' + ExpName + ''']/Properties', 0);
end;

procedure TTestXmlOpts.TestHasEntryProps(AnXml: TXMLDocument; ExpName: String;
  ExpPropName, ExpPropVal: String);
begin
  CheckXPath(AnXml, '//Debugger/Backends/Config[@ConfigName=''' + ExpName + ''']', 1);
  CheckXPath(AnXml, '//Debugger/Backends/Config[@ConfigName=''' + ExpName + ''']/Properties', 1);
  CheckXPath(AnXml, '//Debugger/Backends/Config[@ConfigName=''' + ExpName + ''']/Properties[@' + ExpPropName + '=''' + ExpPropVal + ''']', 1);
end;

procedure TTestXmlOpts.TestHasClassOnlyEntryWithoutProps(AnXml: TXMLDocument;
  ExpClass: String);
begin
  CheckXPath(AnXml, '//Debugger/Backends/Config[not(@ConfigName) and @ConfigClass=''' + ExpClass + ''']', 1);
  CheckXPath(AnXml, '//Debugger/Backends/Config[not(@ConfigName) and @ConfigClass=''' + ExpClass + ''']/Properties', 0);
end;

procedure TTestXmlOpts.TestHasClassOnlyEntryProps(AnXml: TXMLDocument;
  ExpClass: String; ExpPropName, ExpPropVal: String);
begin
  CheckXPath(AnXml, '//Debugger/Backends/Config[not(@ConfigName) and @ConfigClass=''' + ExpClass + ''']', 1);
  CheckXPath(AnXml, '//Debugger/Backends/Config[not(@ConfigName) and @ConfigClass=''' + ExpClass + ''']/Properties', 1);
  CheckXPath(AnXml, '//Debugger/Backends/Config[not(@ConfigName) and @ConfigClass=''' + ExpClass + ''']/Properties[@' + ExpPropName + '=''' + ExpPropVal + ''']', 1);
end;

procedure TTestXmlOpts.TestLoadEnvNew;
var
  opts: TTestDebuggerOptions;
  xml: TXMLDocument;
begin
  InitEnvOpts(ConfigEnvNew);
  opts := InitDbgOpts(CONF_EMPTY);

//DebuglnDbgOptConfigs(opts);
//WriteLn(SaveDbgOpts(opts));
//WriteLn(SaveEnvOpts);

  TestEntryCnt(opts, 4, 1);
  TestHasEntry(opts, 'abc1', 'TTestDebuggerBackendAbc', False, 'abc.exe',  'U-1');
  TestHasEntry(opts, 'abc2', 'TTestDebuggerBackendAbc', False, 'abc.exe',  'U-2');
  TestHasEntry(opts, 'abc3', 'TTestDebuggerBackendAbc', True,  'abc3.exe', 'U-3');
  TestHasEntry(opts, 'foo1', 'TTestDebuggerBackendFoo', False, 'foo.exe',  'U-4');
  TestHasEntry(opts, 'xxx0', 'TUnknownDebuggerBackend', False, 'any.exe',  'U-5', False);


  xml := ParseXml(SaveDbgOpts(opts));
  TestEntryCnt(xml, 5);

  TestHasEntry(xml, 'abc1', 'TTestDebuggerBackendAbc', False, 'abc.exe',  'U-1');
  TestHasEntry(xml, 'abc2', 'TTestDebuggerBackendAbc', False, 'abc.exe',  'U-2');
  TestHasEntry(xml, 'abc3', 'TTestDebuggerBackendAbc', True,  'abc3.exe', 'U-3');
  TestHasEntry(xml, 'foo1', 'TTestDebuggerBackendFoo', False, 'foo.exe',  'U-4');
  TestHasEntry(xml, 'xxx0', 'TUnknownDebuggerBackend', False, 'any.exe',  'U-5');

  TestHasEntryWithoutProps(xml, 'abc1');

  TestHasEntryProps(xml, 'abc2', 'AbcVal',  '123');
  TestHasEntryProps(xml, 'abc2', 'AbcProp',  'True');

  TestHasEntryWithoutProps(xml, 'abc3');

  TestHasEntryProps(xml, 'foo1', 'FooProp',  '987');

  TestHasEntryProps(xml, 'xxx0', 'SomeProp',  '100');
  TestHasEntryProps(xml, 'xxx0', 'SomeVal',  'False');
  TestHasEntryProps(xml, 'xxx0', 'SomeTxt',  '');

  xml.Destroy;
  opts.Free;
end;

procedure TTestXmlOpts.TestLoadEnvOld;
var
  opts: TTestDebuggerOptions;
  xml: TXMLDocument;
  NoDataForDefault: Boolean;
begin
  for NoDataForDefault := False to True do
  begin
  InitEnvOpts(ConfigEnv1_8(NoDataForDefault));
  opts := InitDbgOpts(CONF_EMPTY);

  TestEntryCnt(opts, 3, 1);
  TestHasEntry(opts, '', 'TTestDebuggerBackendBar', False, '');
  TestHasEntry(opts, '', 'TTestDebuggerBackendAbc', True,  'C:\gdb.exe');
  TestHasEntry(opts, '', 'TUnknownDebuggerBackend', False, '', '', False);
  TestHasEntry(opts, '', 'TTestDebuggerBackendFoo', False, '');


  xml := ParseXml(SaveDbgOpts(opts));
  TestEntryCnt(xml, 4);

  TestHasEntry(xml, '', 'TTestDebuggerBackendBar', False, '');
  TestHasEntry(xml, '', 'TTestDebuggerBackendAbc', True,  'C:\gdb.exe');
  TestHasEntry(xml, '', 'TUnknownDebuggerBackend', False, '');
  TestHasEntry(xml, '', 'TTestDebuggerBackendFoo', False, '');

  TestHasClassOnlyEntryWithoutProps(xml, 'TTestDebuggerBackendBar');

  if NoDataForDefault then begin
    TestHasClassOnlyEntryWithoutProps(xml, 'TTestDebuggerBackendAbc');
  end
  else begin
    TestHasClassOnlyEntryProps(xml, 'TTestDebuggerBackendAbc', 'AbcVal',  '123');
    TestHasClassOnlyEntryProps(xml, 'TTestDebuggerBackendAbc', 'AbcProp',  'True');
  end;

  TestHasClassOnlyEntryProps(xml, 'TTestDebuggerBackendFoo', 'FooProp',  '987');

  TestHasClassOnlyEntryProps(xml, 'TUnknownDebuggerBackend', 'SomeProp',  '100');
  TestHasClassOnlyEntryProps(xml, 'TUnknownDebuggerBackend', 'SomeVal',  'False');
  TestHasClassOnlyEntryProps(xml, 'TUnknownDebuggerBackend', 'SomeTxt',  '');

  xml.Destroy;
  opts.Free;
  end;
end;

procedure TTestXmlOpts.TestLoadEnvMixed;
var
  OldDefault, NoOldClassAttr: Boolean;
  opts: TTestDebuggerOptions;
  xml: TXMLDocument;
begin
  for OldDefault := False to True do
  for NoOldClassAttr := False to True do
  begin
  InitEnvOpts(ConfigEnv_Mixed(OldDefault, NoOldClassAttr));
  opts := InitDbgOpts(CONF_EMPTY);

  TestEntryCnt(opts, 7, 4);
  TestHasEntry(opts, 'abc1', 'TTestDebuggerBackendAbc', False, 'abc.exe',  'U-1');
  TestHasEntry(opts, 'abc2', 'TTestDebuggerBackendAbc', not OldDefault,  'abc.exe',  'U-2');
  TestHasEntry(opts, 'abc3', 'TTestDebuggerBackendAbc', False, 'abc3.exe', 'U-3');
  TestHasEntry(opts, 'foo1', 'TTestDebuggerBackendFoo', False, 'foo.exe',  'U-4');
  TestHasEntry(opts, 'xxx0', 'TUnknownDebuggerBackend', False, 'any.exe',  'U-5', False);
  TestHasEntry(opts, 'xxx1', 'TNotFoundDebuggerBackend', False, 'no.exe',  'U-6', False);

  TestHasClassEntry(opts, 'TTestDebuggerBackendBar', False, 'mixbar', 'M-U1');
  if NoOldClassAttr then
    TestHasClassEntry(opts, 'TTestDebuggerBackendAbc', False, '')
  else
    TestHasClassEntry(opts, 'TTestDebuggerBackendAbc', OldDefault,  'C:\gdb.exe');
  TestHasClassEntry(opts, 'TUnknownDebuggerBackend', False, '', '', False);
  TestHasClassEntry(opts, 'TOtherDebuggerBackend', False, '', '', False);
  TestHasClassEntry(opts, 'TTestDebuggerBackendFoo', False, 'mixfoo', 'M-U2');


  xml := ParseXml(SaveDbgOpts(opts));
  TestEntryCnt(xml, 11);

  TestHasEntry(xml, 'abc1', 'TTestDebuggerBackendAbc', False, 'abc.exe',  'U-1');
  TestHasEntry(xml, 'abc2', 'TTestDebuggerBackendAbc', not OldDefault,  'abc.exe',  'U-2');
  TestHasEntry(xml, 'abc3', 'TTestDebuggerBackendAbc', False, 'abc3.exe', 'U-3');
  TestHasEntry(xml, 'foo1', 'TTestDebuggerBackendFoo', False, 'foo.exe',  'U-4');
  TestHasEntry(xml, 'xxx0', 'TUnknownDebuggerBackend', False, 'any.exe',  'U-5');
  TestHasEntry(xml, 'xxx1', 'TNotFoundDebuggerBackend', False, 'no.exe',  'U-6');

  TestHasEntry(xml, '', 'TTestDebuggerBackendBar', False, 'mixbar', 'M-U1');
  if NoOldClassAttr then
    TestHasEntry(xml, '', 'TTestDebuggerBackendAbc', False,  '')
  else
    TestHasEntry(xml, '', 'TTestDebuggerBackendAbc', OldDefault,  'C:\gdb.exe');
  TestHasEntry(xml, '', 'TUnknownDebuggerBackend', False, '');
  TestHasEntry(xml, '', 'TOtherDebuggerBackend', False, '');
  TestHasEntry(xml, '', 'TTestDebuggerBackendFoo', False, 'mixfoo', 'M-U2');

  TestHasEntryWithoutProps(xml, 'abc1');

  TestHasEntryProps(xml, 'abc2', 'AbcVal',  '123');
  TestHasEntryProps(xml, 'abc2', 'AbcProp',  'True');

  TestHasEntryWithoutProps(xml, 'abc3');

  TestHasEntryProps(xml, 'foo1', 'FooProp',  '987');

  TestHasEntryProps(xml, 'xxx0', 'SomeProp',  '100');
  TestHasEntryProps(xml, 'xxx0', 'SomeVal',  'False');
  TestHasEntryProps(xml, 'xxx0', 'SomeTxt',  '');

  TestHasEntryProps(xml, 'xxx1', 'AbcVal',  '123');
  TestHasEntryProps(xml, 'xxx1', 'AbcProp',  'True');


  TestHasClassOnlyEntryWithoutProps(xml, 'TTestDebuggerBackendBar');

  TestHasClassOnlyEntryProps(xml, 'TTestDebuggerBackendAbc', 'AbcVal',  '223');
  TestHasClassOnlyEntryProps(xml, 'TTestDebuggerBackendAbc', 'AbcProp',  'True');

  TestHasClassOnlyEntryProps(xml, 'TUnknownDebuggerBackend', 'SomeProp',  '100');
  TestHasClassOnlyEntryProps(xml, 'TUnknownDebuggerBackend', 'SomeVal',  'False');
  TestHasClassOnlyEntryProps(xml, 'TUnknownDebuggerBackend', 'SomeTxt',  '');

  TestHasClassOnlyEntryProps(xml, 'TOtherDebuggerBackend', 'FooProp',  '887');

  TestHasClassOnlyEntryProps(xml, 'TTestDebuggerBackendFoo', 'FooProp',  '787');

  xml.Destroy;
  opts.Free;
  end;
end;

procedure TTestXmlOpts.TestLoadEnvNewUnknowActive;
var
  opts: TTestDebuggerOptions;
  xml: TXMLDocument;
begin
  InitEnvOpts(ConfigEnvNewUnknowActive);
  opts := InitDbgOpts(CONF_EMPTY);

  TestEntryCnt(opts, 4, 2);
  TestHasEntry(opts, 'abc1', 'TTestDebuggerBackendAbc', False, 'abc.exe',  'U-1');
  TestHasEntry(opts, 'abc2', 'TTestDebuggerBackendAbc', False, 'abc.exe',  'U-2');
  TestHasEntry(opts, 'abc3', 'TTestDebuggerBackendAbc', False,  'abc3.exe', 'U-3');
  TestHasEntry(opts, 'foo1', 'TTestDebuggerBackendFoo', False, 'foo.exe',  'U-4');
  TestHasEntry(opts, 'xxx0', 'TUnknownDebuggerBackend', True, 'any.exe',  'U-5', False);
  TestHasEntry(opts, 'xxx1', 'TNotFoundDebuggerBackend', False, 'no.exe',  'U-6', False);


  xml := ParseXml(SaveDbgOpts(opts));
  TestEntryCnt(xml, 6);

  TestHasEntry(xml, 'abc1', 'TTestDebuggerBackendAbc', False, 'abc.exe',  'U-1');
  TestHasEntry(xml, 'abc2', 'TTestDebuggerBackendAbc', False, 'abc.exe',  'U-2');
  TestHasEntry(xml, 'abc3', 'TTestDebuggerBackendAbc', False,  'abc3.exe', 'U-3');
  TestHasEntry(xml, 'foo1', 'TTestDebuggerBackendFoo', False, 'foo.exe',  'U-4');
  TestHasEntry(xml, 'xxx0', 'TUnknownDebuggerBackend', True, 'any.exe',  'U-5');
  TestHasEntry(xml, 'xxx1', 'TNotFoundDebuggerBackend', False, 'no.exe',  'U-6');

  TestHasEntryWithoutProps(xml, 'abc1');

  TestHasEntryProps(xml, 'abc2', 'AbcVal',  '123');
  TestHasEntryProps(xml, 'abc2', 'AbcProp',  'True');

  TestHasEntryWithoutProps(xml, 'abc3');

  TestHasEntryProps(xml, 'foo1', 'FooProp',  '987');

  TestHasEntryProps(xml, 'xxx0', 'SomeProp',  '100');
  TestHasEntryProps(xml, 'xxx0', 'SomeVal',  'False');
  TestHasEntryProps(xml, 'xxx0', 'SomeTxt',  '');

  xml.Destroy;
  opts.Free;
end;

procedure TTestXmlOpts.TestLoadEnvOldUnknowActive;
var
  opts: TTestDebuggerOptions;
  xml: TXMLDocument;
begin
  InitEnvOpts(ConfigEnv1_8_UnknownActive);
  opts := InitDbgOpts(CONF_EMPTY);

  TestEntryCnt(opts, 3, 1);
  TestHasEntry(opts, '', 'TTestDebuggerBackendBar', False, '');
  TestHasEntry(opts, '', 'TTestDebuggerBackendAbc', False,  '');
  TestHasEntry(opts, '', 'TUnknownDebuggerBackend', True, 'C:\gdb.exe', '', False);
  TestHasEntry(opts, '', 'TTestDebuggerBackendFoo', False, '');


  xml := ParseXml(SaveDbgOpts(opts));
  TestEntryCnt(xml, 4);

  TestHasEntry(xml, '', 'TTestDebuggerBackendBar', False, '');
  TestHasEntry(xml, '', 'TTestDebuggerBackendAbc', False,  '');
  TestHasEntry(xml, '', 'TUnknownDebuggerBackend', True, 'C:\gdb.exe');
  TestHasEntry(xml, '', 'TTestDebuggerBackendFoo', False, '');

  TestHasClassOnlyEntryWithoutProps(xml, 'TTestDebuggerBackendBar');

  TestHasClassOnlyEntryProps(xml, 'TTestDebuggerBackendAbc', 'AbcVal',  '123');
  TestHasClassOnlyEntryProps(xml, 'TTestDebuggerBackendAbc', 'AbcProp',  'True');

  TestHasClassOnlyEntryProps(xml, 'TTestDebuggerBackendFoo', 'FooProp',  '987');

  TestHasClassOnlyEntryProps(xml, 'TUnknownDebuggerBackend', 'SomeProp',  '100');
  TestHasClassOnlyEntryProps(xml, 'TUnknownDebuggerBackend', 'SomeVal',  'False');
  TestHasClassOnlyEntryProps(xml, 'TUnknownDebuggerBackend', 'SomeTxt',  '');

  xml.Destroy;
  opts.Free;
end;

procedure TTestXmlOpts.TestLoadFromDbg;
var
  opts: TTestDebuggerOptions;
  xml: TXMLDocument;
  i: Integer;
begin
  for i := 0 to 2 do begin
    case i of
      0: InitEnvOpts(ConfigEnvNew);
      1: InitEnvOpts(ConfigEnv1_8());
      2: InitEnvOpts(ConfigEnv_Mixed());
    end;
    opts := InitDbgOpts(ConfigDbgNewOne);

    TestEntryCnt(opts, 1, 0);
    TestHasEntry(opts, 'new', 'TTestDebuggerBackendAbc', True, 'new.exe',  'U-99');

    xml := ParseXml(SaveDbgOpts(opts));
    TestEntryCnt(xml, 1);
    TestHasEntry(xml, 'new', 'TTestDebuggerBackendAbc', True, 'new.exe',  'U-99');

    xml.Destroy;
    opts.Free;
  end;
end;

procedure TTestXmlOpts.TestDeleteSave;
var
  opts: TTestDebuggerOptions;
  xml: TXMLDocument;
  i: Integer;
begin
  for i := 0 to 7 do begin
    InitEnvOpts(CONF_EMPTY);
    opts := InitDbgOpts(ConfigDbgNew);

    case i of
      1: opts.DebuggerPropertiesConfigList.Delete(opts.DebuggerPropertiesConfigList.IndexOf('abc1'));
      2: opts.DebuggerPropertiesConfigList.Delete(opts.DebuggerPropertiesConfigList.IndexOf('abc2'));
      3: opts.DebuggerPropertiesConfigList.Delete(opts.DebuggerPropertiesConfigList.IndexOf('abc3'));
      4: opts.DebuggerPropertiesConfigList.Delete(opts.DebuggerPropertiesConfigList.IndexOf('foo1'));
      5: opts.DebuggerPropertiesConfigList.Unloaded.Delete(opts.DebuggerPropertiesConfigList.Unloaded.IndexOf('xxx0'));
      6: opts.DebuggerPropertiesConfigList.Unloaded.Delete(opts.DebuggerPropertiesConfigList.Unloaded.IndexOf('xxx1'));
      7: opts.DebuggerPropertiesConfigList.Delete(opts.DebuggerPropertiesConfigList.IndexOf('zbar'));
    end;

    if i = 0           then TestEntryCnt(opts, 5, 2)
    else if i in [5,6] then TestEntryCnt(opts, 5, 1)
    else                    TestEntryCnt(opts, 4, 2);

    if i <> 1 then TestHasEntry(opts, 'abc1', 'TTestDebuggerBackendAbc', False, 'abc.exe',  'U-1');
    if i <> 2 then TestHasEntry(opts, 'abc2', 'TTestDebuggerBackendAbc', False, 'abc.exe',  'U-2');
    if i <> 3 then TestHasEntry(opts, 'abc3', 'TTestDebuggerBackendAbc', True,  'abc3.exe', 'U-3');
    if i <> 4 then TestHasEntry(opts, 'foo1', 'TTestDebuggerBackendFoo', False, 'foo.exe',  'U-4');
    if i <> 5 then TestHasEntry(opts, 'xxx0', 'TUnknownDebuggerBackend', False, 'any.exe',  'U-5', False);
    if i <> 6 then TestHasEntry(opts, 'xxx1', 'TNotFoundDebuggerBackend', False, 'no.exe',  'U-6', False);
    if i <> 7 then TestHasEntry(opts, 'zbar',  'TTestDebuggerBackendBar', False, 'bar.exe',  'U-7');

    xml := ParseXml(SaveDbgOpts(opts));
    if i = 0 then TestEntryCnt(xml, 7)
    else          TestEntryCnt(xml, 6);

    if i <> 1 then TestHasEntry(xml, 'abc1', 'TTestDebuggerBackendAbc', False, 'abc.exe',  'U-1');
    if i <> 2 then TestHasEntry(xml, 'abc2', 'TTestDebuggerBackendAbc', False, 'abc.exe',  'U-2');
    if i <> 3 then TestHasEntry(xml, 'abc3', 'TTestDebuggerBackendAbc', True,  'abc3.exe', 'U-3');
    if i <> 4 then TestHasEntry(xml, 'foo1', 'TTestDebuggerBackendFoo', False, 'foo.exe',  'U-4');
    if i <> 5 then TestHasEntry(xml, 'xxx0', 'TUnknownDebuggerBackend', False, 'any.exe',  'U-5');
    if i <> 6 then TestHasEntry(xml, 'xxx1', 'TNotFoundDebuggerBackend', False, 'no.exe',  'U-6');
    if i <> 7 then TestHasEntry(xml, 'zbar',  'TTestDebuggerBackendBar', False, 'bar.exe',  'U-7');


    if i <> 1 then TestHasEntryWithoutProps(xml, 'abc1');

    if i <> 2 then TestHasEntryProps(xml, 'abc2', 'AbcVal',  '123');
    if i <> 2 then TestHasEntryProps(xml, 'abc2', 'AbcProp',  'True');

    if i <> 3 then TestHasEntryWithoutProps(xml, 'abc3');

    if i <> 4 then TestHasEntryProps(xml, 'foo1', 'FooProp',  '987');

    if i <> 5 then TestHasEntryProps(xml, 'xxx0', 'SomeProp',  '100');
    if i <> 5 then TestHasEntryProps(xml, 'xxx0', 'SomeVal',  'False');
    if i <> 5 then TestHasEntryProps(xml, 'xxx0', 'SomeTxt',  '');

    if i <> 6 then TestHasEntryProps(xml, 'xxx1', 'AnyProp',  'abc');

    if i <> 7 then TestHasEntryWithoutProps(xml, 'zbar');

    xml.Destroy;
    opts.Free;
  end;
end;



initialization

  RegisterTest(TTestXmlOpts);
  RegisterDebugger(TTestDebuggerBackendAbc);
  RegisterDebugger(TTestDebuggerBackendFoo);
  RegisterDebugger(TTestDebuggerBackendBar);
end.

