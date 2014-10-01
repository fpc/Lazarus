unit PoCheckerSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazConfigStorage,
  {$ifdef POCHECKERSTANDALONE}
  PoCheckerXMLConfig, Forms,
  {$else}
  BaseIDEIntf,
  {$endif}
  PoFamilies, LCLProc, LazFileUtils;

type

  { TPoCheckerSettings }

  TPoCheckerSettings = class
  private
    FConfig: TConfigStorage;
    FExternalEditorName: String;
    FFilename: String;
    FOpenDialogFilename: String;
    FSelectDirectoryFilename: String;
    FTestTypes: TPoTestTypes;
    FTestOptions: TPoTestOptions;
    FLoadSettings: Boolean;
    FSaveSettingsOnExit: Boolean;
    FMasterPoList: TStringList;
    FChildPoList: TStringList;
    FLastSelectedFile: String;
    FMainFormGeometry: TRect;
    FGraphFormGeometry: TRect;
    FResultsFormGeometry: TRect;
    function GetChildPoList: TStrings;
    function GetMasterPoList: TStrings;
    function LoadLastSelectedFile: String;
    function LoadTestTypes: TPoTestTypes;
    function LoadTestOptions: TPoTestOptions;
    procedure LoadWindowsGeometry;
    function LoadExternalEditorName: String;
    function LoadSelectDirectoryFilename: String;
    function LoadOpenDialogFilename: String;
    procedure LoadMasterPoList(List: TStrings);
    procedure LoadChildPoList(List: TStrings);
    procedure SaveLastSelectedFile;
    procedure SaveTestTypes;
    procedure SaveTestOptions;
    procedure SaveWindowsGeometry;
    procedure SaveExternalEditorName;
    procedure SaveSelectDirectoryFilename;
    procedure SaveOpenDialogFilename;
    procedure SaveMasterPoList;
    procedure SaveChildrenPoList;
    procedure SetChildPoList(AValue: TStrings);
    procedure SetMasterPoList(AValue: TStrings);

  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadConfig;
    procedure SaveConfig;

    property Filename: String read FFilename;
    property SaveSettingsOnExit: Boolean read FSaveSettingsOnExit write FSaveSettingsOnExit;
    property TestTypes: TPoTestTypes read FTestTypes write FTestTypes;
    property TestOptions: TPoTestOptions read FTestOptions write FTestOptions;
    property ExternalEditorName: String read FExternalEditorName write FExternalEditorName;
    property MasterPoList: TStrings read GetMasterPoList write SetMasterPoList;
    property ChildPoList: TStrings read GetChildPoList write SetChildPoList;
    property LastSelectedFile: String read FLastSelectedFile write FLastSelectedFile;
    property SelectDirectoryFilename: String read FSelectDirectoryFilename write FSelectDirectoryFilename;
    property OpenDialogFilename: String read FOpenDialogFilename write FOpenDialogFilename;
    property MainFormGeometry: TRect read FMainFormGeometry write FMainFormGeometry;
    property ResultsFormGeometry: TRect read FResultsFormGeometry write FResultsFormGeometry;
    property GraphFormGeometry: TRect read FGraphFormGeometry write FGraphFormGeometry;
  end;

function DbgS(PoTestTypes: TPoTestTypes): String; overload;
function DbgS(PoTestOpts: TPoTestOptions): String; overload;
function FitToRect(const ARect, FitIn: TRect): TRect;
function IsDefaultRect(ARect: TRect): Boolean;
function IsValidRect(ARect: TRect): Boolean;


implementation

function FitToRect(const ARect, FitIn: TRect): TRect;
begin
  Result := ARect;
  if (Result.Right - Result.Left) > (FitIn.Right - FitIn.Left) then
    Result.Right := Result.Left + (FitIn.Right - FitIn.Left);
  if (Result.Bottom - Result.Top) > (FitIn.Bottom - FitIn.Top) then
    Result.Bottom := Result.Top + (FitIn.Bottom - FitIn.Top);
  if Result.Left < FitIn.Left then
  begin
    Result.Right := Result.Right + (FitIn.Left - Result.Left);
    Result.Left := FitIn.Left;
  end;
  if Result.Right > FitIn.Right then
  begin
    Result.Left := Result.Left - (Result.Right - FitIn.Right);
    Result.Right := Result.Right - (Result.Right - FitIn.Right);
  end;
  if Result.Top < FitIn.Top then
  begin
    Result.Bottom := Result.Bottom + (FitIn.Top - Result.Top);
    Result.Top := FitIn.Top;
  end;
  if Result.Bottom > FitIn.Bottom then
  begin
    Result.Top := Result.Top - (Result.Bottom - FitIn.Bottom);
    Result.Bottom := Result.Bottom - (Result.Bottom - FitIn.Bottom);
  end;

  //if Result.Right > FitIn.Right then Result.Right := FitIn.Right;
  //if Result.Bottom > FitIn.Bottom then Result.Bottom := FitIn.Bottom;
end;

function IsDefaultRect(ARect: TRect): Boolean;
begin
  Result := (ARect.Left = -1) and (ARect.Top = -1) and
            (ARect.Right = -1) and (Arect.Bottom = -1);
end;

function IsValidRect(ARect: TRect): Boolean;
begin
  Result := (ARect.Right > ARect.Left) and
            (ARect.Bottom > ARect.Top);
end;

const
  TestTypeNames: array[TPoTestType] of String = (
    'CheckNumberOfItems',
    'CheckForIncompatibleFormatArguments',
    'CheckMissingIdentifiers',
    'CheckForMismatchesInUntranslatedStrings',
    'CheckForDuplicateUntranslatedValues',
    'CheckStatistics'
    );
  TestoptionNames: array[TPoTestOption] of String = (
    'FindAllChildren',
    'IgnoreFuzzyStrings'
    );

  pLoadSettings = 'General/LoadSettings/';
  pLastSelected = 'LastSelected/';
  pSelectDirectoryFilename = 'SelectDirectoryFilename/';
  pOpenDialogFilename = 'OpenDialogFilename/';
  pTestTypes = 'TestTypes/';
  pTestOptions = 'TestOptions/';
  pWindowsGeometry = 'General/WindowsGeometry/';
  pExternalEditor = 'ExternalEditor/';
  pMasterPoFiles = 'MasterPoFiles/';
  pChildPoFiles = 'ChildPoFiles/';

var
  DefaultRect: TRect;

function DbgS(PoTestTypes: TPoTestTypes): String; overload;
var
  Typ: TPoTestType;
begin
  Result := '[';
  for Typ := Low(TPotestType) to High(TPoTesttype) do
  begin
    if (Typ in PoTestTypes) then Result := Result + TestTypeNames[Typ];
  end;
  if (Result[Length(Result)] = ',') then System.Delete(Result,Length(Result),1);
  Result := Result + ']';
end;

function DbgS(PoTestOpts: TPoTestOptions): String; overload;
var
  Opt: TPoTestOption;
begin
  Result := '[';
  for Opt := Low(TPotestOption) to High(TPoTestOption) do
  begin
    if (Opt in PoTestOpts) then Result := Result + TestOptionNames[opt];
  end;
  if (Result[Length(Result)] = ',') then System.Delete(Result,Length(Result),1);
  Result := Result + ']';
end;



{ TPoCheckerSettings }
{$ifdef pocheckerstandalone}
function AppName: String;
begin
  Result := 'pochecker';
end;

function Vendor: String;
begin
  Result := '';
end;

function GetAndCreateConfigPath: String;
var
  OldOnGetApplicationName: TGetAppNameEvent;
  OldOnGetVendorName: TGetVendorNameEvent;
begin
  Result := '';
  if Application.HasOption('primary-config-path') then
    Result := ExpandFileNameUtf8(Application.GetOptionValue('primary-config-path'))
  else if Application.HasOption('pcp') then
    Result := ExpandFileNameUtf8(Application.GetOptionValue('pcp'))
  else
  begin
    OldOnGetApplicationName := OnGetApplicationName;
    OldOnGetVendorName := OnGetVendorName;
    OnGetApplicationName := @AppName;
    OnGetVendorName := @Vendor;
    OnGetApplicationName := OldOnGetApplicationName;
    OnGetVendorName := OldOnGetVendorName;
    Result := GetAppConfigDirUtf8(False);
  end;
  if not ForceDirectoriesUTF8(Result) then
    Debugln('GetAndCreateConfigPath: unable to create "',Result,'"');
end;

{$endif}


function TPoCheckerSettings.LoadLastSelectedFile: String;
begin
  Result := FConfig.GetValue(pLastSelected+'Value','');
end;

function TPoCheckerSettings.GetMasterPoList: TStrings;
begin
  Result := FMasterPoList;
end;

function TPoCheckerSettings.GetChildPoList: TStrings;
begin
  Result := FChildPoList;
end;

function TPoCheckerSettings.LoadTestTypes: TPoTestTypes;
var
  tt: TPoTestType;
  Name: String;
  B: Boolean;
begin
  Result := [];
  for tt := Low(TPoTestType) to High(TPoTestType) do
  begin
    Name := TestTypeNames[tt];
    B := FConfig.GetValue(pTestTypes + Name + '/Value',False);
    if B then Result := Result + [tt];
  end;
end;

function TPoCheckerSettings.LoadTestOptions: TPoTestOptions;
var
  opt: TPoTestOption;
  Name: String;
  B: Boolean;
begin
  Result := [];
  for opt := Low(TPoTestOption) to High(TPoTestOption) do
  begin
    Name := TestOptionNames[opt];
    B := FConfig.GetValue(pTestOptions + Name + '/Value',False);
    if B then Result := Result + [opt];
  end;
end;

procedure TPoCheckerSettings.LoadWindowsGeometry;
begin
  FConfig.GetValue(pWindowsGeometry+'MainForm/Value',FMainFormGeometry,DefaultRect);
  FConfig.GetValue(pWindowsGeometry+'ResultsForm/Value',FResultsFormGeometry,DefaultRect);
  FConfig.GetValue(pWindowsGeometry+'GraphForm/Value',FGraphFormGeometry,DefaultRect);
end;

function TPoCheckerSettings.LoadExternalEditorName: String;
begin
  {$IFDEF POCHECKERSTANDALONE}
  //allow override on commandline
  if Application.HasOption('editor') then
    Result := Application.GetOptionValue('editor')
  else
    Result := FConfig.GetValue(pExternalEditor+'Value','');
  {$ELSE}
  Result := '';
  {$eNDIF}
end;

function TPoCheckerSettings.LoadSelectDirectoryFilename: String;
begin
  Result := FConfig.GetValue(pSelectDirectoryFilename+'Value','');
end;

function TPoCheckerSettings.LoadOpenDialogFilename: String;
begin
  Result := FConfig.GetValue(pOpenDialogFilename+'Value','');
end;


procedure TPoCheckerSettings.LoadMasterPoList(List: TStrings);
var
  Cnt, i: Integer;
  Fn: String;
begin
  List.Clear;
  Cnt := Fconfig.GetValue(pMasterpoFiles+'Count',0);
  //debugln('TPoCheckerSettings.LoadMasterPoList: Cnt = ',DbgS(Cnt));
  for i := 0 to Cnt - 1 do
  begin
    Fn := FConfig.GetValue(pMasterpoFiles+Format('Item_%d/Value',[i]),'');
    if (Fn <> '') then List.Add(Fn);
  end;
end;

procedure TPoCheckerSettings.LoadChildPoList(List: TStrings);
var
  Cnt, i: Integer;
  Fn: String;
begin
  List.Clear;
  Cnt := Fconfig.GetValue(pChildPoFiles+'Count',0);
  //debugln('TPoCheckerSettings.LoadChildPoList: Cnt = ',DbgS(Cnt));
  for i := 0 to Cnt - 1 do
  begin
    Fn := FConfig.GetValue(pChildPoFiles+Format('Item_%d/Value',[i]),'');
    if (Fn <> '') then List.Add(Fn);
  end;
end;

procedure TPoCheckerSettings.SaveLastSelectedFile;
begin
  FConfig.SetDeleteValue(pLastSelected+'Value',FLastSelectedFile,'');
end;

procedure TPoCheckerSettings.SaveTestTypes;
var
  tt: TPoTestType;
  Name: String;
begin
  for tt := Low(TPoTestTypes) to High(TPoTestTypes) do
  begin
    Name := TestTypeNames[tt];
    FConfig.SetDeleteValue(pTestTypes + Name + '/Value',(tt in FTestTypes),False);
  end;
end;

procedure TPoCheckerSettings.SaveTestOptions;
var
  topt: TPoTestOption;
  Name: String;
begin
  for topt := Low(TPoTestOptions) to High(TPoTestoptions) do
  begin
    Name := TestOptionNames[topt];
    FConfig.SetDeleteValue(pTestOptions + Name + '/Value',(topt in FTestOptions),False);
  end;
end;

procedure TPoCheckerSettings.SaveWindowsGeometry;
begin
  FConfig.SetDeleteValue(pWindowsGeometry+'MainForm/Value',FMainFormGeometry,DefaultRect);
  FConfig.SetDeleteValue(pWindowsGeometry+'ResultsForm/Value',FResultsFormGeometry,DefaultRect);
  FConfig.SetDeleteValue(pWindowsGeometry+'GraphForm/Value',FGraphFormGeometry,DefaultRect);
end;

procedure TPoCheckerSettings.SaveExternalEditorName;
begin
  {$IFDEF POCHECKERSTANDALONE}
  FConfig.SetDeleteValue(pExternalEditor+'Value',FExternalEditorName,'');
  {$ENDIF}
end;

procedure TPoCheckerSettings.SaveSelectDirectoryFilename;
begin
  FConfig.SetDeleteValue(pSelectDirectoryFilename+'Value',FSelectDirectoryFilename,'');
end;

procedure TPoCheckerSettings.SaveOpenDialogFilename;
begin
  FConfig.SetDeleteValue(pOpenDialogFilename+'Value',FOpenDialogFilename,'');
end;

procedure TPoCheckerSettings.SaveMasterPoList;
var
  Cnt, i: Integer;
begin
  FConfig.DeletePath(pMasterPoFiles);
  Cnt := FMasterPoList.Count;
  FConfig.SetDeleteValue(pMasterPoFiles+'Count',Cnt,0);
  for i := 0 to Cnt - 1 do
    FConfig.SetDeleteValue(pMasterPoFiles+Format('Item_%d/Value',[i]),FMasterPoList[i],'');
end;

procedure TPoCheckerSettings.SaveChildrenPoList;
var
  Cnt, i: Integer;
begin
  FConfig.DeletePath(pChildPoFiles);
  Cnt := FChildPoList.Count;
  FConfig.SetDeleteValue(pChildPoFiles+'Count',Cnt,0);
  for i := 0 to Cnt - 1 do
    FConfig.SetDeleteValue(pChildPoFiles+Format('Item_%d/Value',[i]),FChildPoList[i],'');
end;

procedure TPoCheckerSettings.SetChildPoList(AValue: TStrings);
begin
  FChildPoList.Assign(AValue);
end;

procedure TPoCheckerSettings.SetMasterPoList(AValue: TStrings);
begin
  FMasterPoList.Assign(AValue);
end;

constructor TPoCheckerSettings.Create;
begin
  try
    FTestTypes := [];
    FTestOptions := [];
    FMainFormGeometry := Rect(-1,-1,-1,-1);
    FMasterPoList := TStringList.Create;
    FMasterPoList.Sorted := True;
    FMasterPoList.Duplicates := dupIgnore;
    FChildPoList := TStringList.Create;
    FChildPoList.Sorted := True;
    FChildPoList.Duplicates := dupIgnore;
    {$ifdef POCHECKERSTANDALONE}
    FFilename := GetAndCreateConfigPath;
    if (FFilename <> '') then FFilename := AppendPathDelim(FFilename);
    FFilename := FFilename + 'pochecker.xml';
    //debugln('TPoCheckerSettings.Create: Filename = ');
    //debugln('"',Filename,'"');

    //FFilename := 'pochecker.xml';

    FConfig := TXMLOptionsStorage.Create(FFilename, True);
    {$else}
    FFilename := 'pochecker.xml';
    FConfig := GetIDEConfigStorage(FFilename, True);
    {$endif}
    //DebugLn('TPoCheckerSettings.Create: FConfig = ',DbgSName(FConfig));
  except
    Debugln('PoCheckerSettings.Create: failed to create ConfigStorage:');
    Debugln(' - Filename = ',FFilename);
    FConfig := nil;
  end;
end;

destructor TPoCheckerSettings.Destroy;
begin
  if Assigned(FConfig) then FConfig.Free;
  FMasterPoList.Free;
  FChildPoList.Free;
  inherited Destroy;
end;

procedure TPoCheckerSettings.LoadConfig;
begin
  try
    FLoadSettings := FConfig.GetValue(pLoadSettings+'Value',False);
    if FLoadSettings then
    begin
      FTestTypes := LoadTestTypes;
      FTestOptions := LoadTestOptions;
      FLastSelectedFile := LoadLastSelectedFile;
      FSelectDirectoryFilename := LoadSelectDirectoryFilename;
      FOpenDialogFilename := LoadOpenDialogFilename;
      FExternalEditorName := LoadExternalEditorName;
      LoadWindowsGeometry;
      LoadMasterPoList(FMasterPoList);
      LoadChildPoList(FChildPoList);
    end;
  except
    FTestTypes := [];
    FTestOptions := [];
    debugln('TPoCheckerSettings.LoadConfig: Error loading config.');
  end;
end;

procedure TPoCheckerSettings.SaveConfig;
begin
  try
    FConfig.SetDeleteValue('Version','1.0','');
    FConfig.SetValue(pLoadSettings+'Value',FSaveSettingsOnExit);
    if FSaveSettingsOnExit then
    begin
      SaveLastSelectedFile;
      SaveTestTypes;
      SaveTestOptions;
      SaveExternalEditorName;
      SaveSelectDirectoryFilename;
      SaveOpenDialogFilename;
      SaveWindowsGeometry;
      SaveMasterPoList;
      SaveChildrenPoList;
    end
    else
    begin
      FConfig.DeletePath(pMasterPoFiles);
      FConfig.DeletePath(pChildPoFiles);
    end;
    FConfig.WriteToDisk;
  except
    debugln('TPoCheckerSettings.SaveConfig: Error saving config.');
  end;
end;

Initialization
  DefaultRect := Rect(-1, -1, -1, -1);
end.

