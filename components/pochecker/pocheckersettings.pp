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
    FFilename: String;
    FTestTypes: TPoTestTypes;
    FTestOptions: TPoTestOptions;
    FLoadSettings: Boolean;
    FSaveSettingsOnExit: Boolean;
    FMasterPoList: TStrings;
    FChildrenPoList: TStrings;
    FLastSelectedFile: String;
    FMainFormGeometry: TRect;
    FGraphFormGeometry: TRect;
    FResultsFormGeometry: TRect;
    function LoadLastSelectedFile: String;
    function LoadTestTypes: TPoTestTypes;
    function LoadTestOptions: TPoTestOptions;
    procedure LoadWindowsGeometry;
    procedure LoadMasterPoList(List: TStrings);
    procedure LoadChildrenPoList(List: TStrings);
    procedure SaveLastSelectedFile;
    procedure SaveTestTypes;
    procedure SaveTestOptions;
    procedure SaveWindowsGeometry;
    procedure SaveMasterPoList;
    procedure SaveChildrenPoList;

  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadConfig;
    procedure SaveConfig;

    property Filename: String read FFilename;
    property SaveSettingsOnExit: Boolean read FSaveSettingsOnExit write FSaveSettingsOnExit;
    property TestTypes: TPoTestTypes read FTestTypes write FTestTypes;
    property TestOptions: TPoTestOptions read FTestOptions write FTestOptions;
    property MasterPoList: TStrings read FMasterPoList write FMasterPoList;
    property ChildrenPoList: TStrings read FChildrenPoList write FChildrenPoList;
    property LastSelectedFile: String read FLastSelectedFile write FLastSelectedFile;
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
  pTestTypes = 'TestTypes/';
  pTestOptions = 'TestOptions/';
  pWindowsGeometry = 'General/WindowsGeometry/';
  pMasterPoFiles = 'MasterPoFiles/';
  pChildrenPoFiles = 'ChildrenPoFiles/';

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

procedure TPoCheckerSettings.LoadMasterPoList(List: TStrings);
begin
  if not Assigned(List) then Exit;
  List.Clear;
end;

procedure TPoCheckerSettings.LoadChildrenPoList(List: TStrings);
begin
  if not Assigned(List) then Exit;
  List.Clear;
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

procedure TPoCheckerSettings.SaveMasterPoList;
begin
  FConfig.DeletePath(pMasterPoFiles);
end;

procedure TPoCheckerSettings.SaveChildrenPoList;
begin
  FConfig.DeletePath(pChildrenPoFiles);
end;

constructor TPoCheckerSettings.Create;
begin
  try
    FTestTypes := [];
    FTestOptions := [];
    FMainFormGeometry := Rect(-1,-1,-1,-1);
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
      LoadWindowsGeometry;
      LoadMasterPoList(FMasterPoList);
      LoadChildrenPoList(FChildrenPoList);
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
      SaveWindowsGeometry;
      SaveMasterPoList;
      SaveChildrenPoList;
    end
    else
    begin
      FConfig.DeletePath(pMasterPoFiles);
      FConfig.DeletePath(pChildrenPoFiles);
    end;
    FConfig.WriteToDisk;
  except
    debugln('TPoCheckerSettings.SaveConfig: Error saving config.');
  end;
end;

Initialization
  DefaultRect := Rect(-1, -1, -1, -1);
end.

