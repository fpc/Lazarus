unit PoCheckerSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazConfigStorage, Forms,
  {$ifdef POCHECKERSTANDALONE}
  PoCheckerXMLConfig,
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
    FGraphFormWindowState: TWindowState;
    FLangFilterLanguageAbbr: String;
    FLangPath: String;
    FMainFormWindowState: TWindowState;
    FResultsFormWindowState: TWindowState;
    FSelectDirectoryFilename: String;
    FTestTypes: TPoTestTypes;
    FMasterPoList: TStringList;
    FMasterPoSelList: TStringList;
    FMainFormGeometry: TRect;
    FGraphFormGeometry: TRect;
    FResultsFormGeometry: TRect;
    FDisableAntialiasing: Boolean;
    function GetDisableAntialiasing: Boolean;
    function GetMasterPoList: TStrings;
    function GetMasterPoSelList: TStrings;
    function LoadTestTypes: TPoTestTypes;
    procedure LoadWindowsGeometry;
    procedure LoadDisableAntiAliasing;
    function LoadExternalEditorName: String;
    function LoadSelectDirectoryFilename: String;
    function LoadLangFilterLanguageAbbr: String;
    function LoadLangPath: String;
    procedure LoadMasterPoList(List: TStrings);
    procedure LoadMasterPoSelList(List: TStrings);
    procedure SaveTestTypes;
    procedure SaveWindowsGeometry;
    procedure SaveDisableAntialiasing;
    procedure SaveExternalEditorName;
    procedure SaveSelectDirectoryFilename;
    procedure SaveLangFilterLanguageAbbr;
    procedure SaveLangPath;
    procedure SaveMasterPoList;
    procedure SaveMasterPoSelList;
    procedure RemoveUnwantedPaths;
    procedure SetMasterPoList(AValue: TStrings);
    procedure SetMasterPoSelList(AValue: TStrings);
    procedure ResetAllProperties;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadConfig;
    procedure SaveConfig;

    property Filename: String read FFilename;
    property TestTypes: TPoTestTypes read FTestTypes write FTestTypes;
    property ExternalEditorName: String read FExternalEditorName write FExternalEditorName;
    property MasterPoList: TStrings read GetMasterPoList write SetMasterPoList;
    property MasterPoSelList: TStrings read GetMasterPoSelList write SetMasterPoSelList;
    property SelectDirectoryFilename: String read FSelectDirectoryFilename write FSelectDirectoryFilename;
    property MainFormGeometry: TRect read FMainFormGeometry write FMainFormGeometry;
    property ResultsFormGeometry: TRect read FResultsFormGeometry write FResultsFormGeometry;
    property DisableAntialiasing: Boolean read GetDisableAntialiasing;
    property GraphFormGeometry: TRect read FGraphFormGeometry write FGraphFormGeometry;
    property MainFormWindowState: TWindowState read FMainFormWindowState write FMainFormWindowState;
    property ResultsFormWindowState: TWindowState read FResultsFormWindowState write FResultsFormWindowState;
    property GraphFormWindowState: TWindowState read FGraphFormWindowState write FGraphFormWindowState;
    property LangFilterLanguageAbbr: String read FLangFilterLanguageAbbr write FLangFilterLanguageAbbr;
    property LangPath: String read FLangPath write FLangPath;
  end;

function DbgS(PoTestTypes: TPoTestTypes): String; overload;
function FitToRect(const ARect, FitIn: TRect): TRect;
function IsDefaultRect(ARect: TRect): Boolean;
function IsValidRect(ARect: TRect): Boolean;
{$ifdef pocheckerstandalone}
function GetGlobalConfigPath: String;
function GetLocalConfigPath: String;
{$endif}

implementation

const
  DEFAULT_DISABLE_ANTIALIASING = true;

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
    'CheckForMismatchesInUntranslatedStrings'
    );

  pSelectDirectoryFilename = 'SelectDirectoryFilename/';
  pLangFilter = 'LanguageFilter/';
  pLangPath = 'LanguageFiles/';
  pTestTypes = 'TestTypes/';
  pWindowsGeometry = 'General/WindowsGeometry/';
  pMasterPoFiles = 'MasterPoFiles/';
  pMasterPoSelection = 'MasterPoSelection/';
  {$IFDEF POCHECKERSTANDALONE}
  pExternalEditor = 'ExternalEditor/';
  {$ENDIF}

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

function GetGlobalConfigPath: String;
var
  OldOnGetApplicationName: TGetAppNameEvent;
  OldOnGetVendorName: TGetVendorNameEvent;
begin
  Result := '';
  OldOnGetApplicationName := OnGetApplicationName;
  OldOnGetVendorName := OnGetVendorName;
  OnGetApplicationName := @AppName;
  OnGetVendorName := @Vendor;
  Result := GetAppConfigDirUtf8(True);
  OnGetApplicationName := OldOnGetApplicationName;
  OnGetVendorName := OldOnGetVendorName;
end;

function GetLocalConfigPath: String;
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
    Result := GetAppConfigDirUtf8(False);
    OnGetApplicationName := OldOnGetApplicationName;
    OnGetVendorName := OldOnGetVendorName;
  end;
end;

function GetAndCreateConfigPath: String;
begin
  Result := GetLocalConfigPath;
  if not ForceDirectoriesUTF8(Result) then
    Debugln('GetAndCreateConfigPath: unable to create "',Result,'"');
end;

{$endif}


function TPoCheckerSettings.GetMasterPoList: TStrings;
begin
  Result := FMasterPoList;
end;

function TPoCheckerSettings.GetMasterPoSelList: TStrings;
begin
  Result := FMasterPoSelList;
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

procedure TPoCheckerSettings.LoadWindowsGeometry;
function IntToWindowState(WSInt: Integer): TWindowState;
begin
  if (WSInt in [Ord(Low(TWindowState))..Ord(High(TWindowState))]) then
    Result := TWindowState(WSInt)
  else
    Result := wsNormal;
end;
begin
  FConfig.GetValue(pWindowsGeometry+'MainForm/Value',FMainFormGeometry,DefaultRect);
  FMainFormWindowState := IntToWindowState(FConfig.GetValue(pWindowsGeometry+'MainForm/WindowState/Value', Ord(wsNormal)));
  FConfig.GetValue(pWindowsGeometry+'ResultsForm/Value',FResultsFormGeometry,DefaultRect);
  FResultsFormWindowState := IntToWindowState(FConfig.GetValue(pWindowsGeometry+'ResultsForm/WindowState/Value', Ord(wsNormal)));
  FConfig.GetValue(pWindowsGeometry+'GraphForm/Value',FGraphFormGeometry,DefaultRect);
  FGraphFormWindowState := IntToWindowState(FConfig.GetValue(pWindowsGeometry+'GraphForm/WindowState/Value', Ord(wsNormal)));
end;

function TPoCheckerSettings.GetDisableAntialiasing: Boolean;
var
  cfg: TConfigStorage;
  ver: Integer;
begin
  {$IFDEF POCHECKERSTANDALONE}
  Result := FDisableAntialiasing;
  {$ELSE}
  cfg := GetIDEConfigStorage('editoroptions.xml', True);
  ver := cfg.GetValue('EditorOptions/Version', 0);
  Result := cfg.GetValue('EditorOptions/Display/DisableAntialiasing', ver < 7);
  FDisableAntiAliasing := Result;
  cfg.Free;
  {$ENDIF}
end;

procedure TPoCheckerSettings.LoadDisableAntialiasing;
begin
  {$IFDEF POCHECKERSTANDALONE}
  FDisableAntialiasing := FConfig.GetValue('General/Display/ResultsForm/DisableAntialiasing',
      DEFAULT_DISABLE_ANTIALIASING);
  {$ENDIF}
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

function TPoCheckerSettings.LoadLangFilterLanguageAbbr: String;
begin
  Result := FConfig.GetValue(pLangFilter + 'Value', '');
end;

function TPoCheckerSettings.LoadLangPath: String;
{$IFDEF POCHECKERSTANDALONE}
var
  SL: TStringList;
  i: Integer;
  S: String;
{$ENDIF}
begin
  {$IFDEF POCHECKERSTANDALONE}
  //allow override on commandline
  if Application.HasOption('langpath') then
  begin
    Result := '';
    SL := TStringList.Create;
    try
      SL.Delimiter := PathSeparator;
      SL.StrictDelimiter := True;
      SL.DelimitedText := Application.GetOptionValue('langpath');
      for i := 0 to SL.Count - 1 do
      begin
        S := SL.Strings[i];
        if (S <> '') then
        begin
          Result := Result + ExpandFileNameUtf8(S) + PathSeparator;
        end;
      end;
      if (Result <> '') and (Result[Length(Result)] = PathSeparator) then
        System.Delete(Result, Length(Result), 1);
    finally
      SL.Free;
    end;
  end
  else
    Result := FConfig.GetValue(pLangPath+'Value','');
  {$ELSE}
  Result := '';
  {$ENDIF}
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

procedure TPoCheckerSettings.LoadMasterPoSelList(List: TStrings);
var
  Cnt, i: Integer;
  Fn: String;
begin
  List.Clear;
  Cnt := Fconfig.GetValue(pMasterpoSelection+'Count',0);
  //debugln('TPoCheckerSettings.LoadMasterPoSelList: Cnt = ',DbgS(Cnt));
  for i := 0 to Cnt - 1 do
  begin
    Fn := FConfig.GetValue(pMasterpoSelection+Format('Item_%d/Value',[i]),'');
    if (Fn <> '') then List.Add(Fn);
  end;
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

procedure TPoCheckerSettings.SaveWindowsGeometry;
begin
  FConfig.SetDeleteValue(pWindowsGeometry+'MainForm/Value',FMainFormGeometry,DefaultRect);
  FConfig.SetDeleteValue(pWindowsGeometry+'MainForm/WindowState/Value',Ord(FMainFormWindowState), Ord(wsNormal));
  FConfig.SetDeleteValue(pWindowsGeometry+'ResultsForm/Value',FResultsFormGeometry,DefaultRect);
  FConfig.SetDeleteValue(pWindowsGeometry+'ResultsForm/WindowState/Value',Ord(FResultsFormWindowState), Ord(wsNormal));
  FConfig.SetDeleteValue(pWindowsGeometry+'GraphForm/Value',FGraphFormGeometry,DefaultRect);
  FConfig.SetDeleteValue(pWindowsGeometry+'GraphForm/WindowState/Value',Ord(FGraphFormWindowState), Ord(wsNormal));
end;

procedure TPoCheckerSettings.SaveDisableAntialiasing;
begin
  {$IFDEF POCHECKERSTANDALONE}
  // Don't use SetDeleteValue to keep the syntax in the file because there is
  // no gui to modify DisableAntialiasing at the moment.
  FConfig.SetValue('General/Display/ResultsForm/DisableAntialiasing', FDisableAntialiasing);
  {$ENDIF}
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

procedure TPoCheckerSettings.SaveMasterPoSelList;
var
  Cnt, i: Integer;
begin
  FConfig.DeletePath(pMasterPoSelection);
  Cnt := FMasterPoSelList.Count;
  FConfig.SetDeleteValue(pMasterPoSelection+'Count',Cnt,0);
  for i := 0 to Cnt - 1 do
    FConfig.SetDeleteValue(pMasterPoSelection+Format('Item_%d/Value',[i]),FMasterPoSelList[i],'');
end;


procedure TPoCheckerSettings.SaveLangFilterLanguageAbbr;
begin
  FConfig.SetDeleteValue(pLangFilter + 'Value', FLangFilterLanguageAbbr, '');
end;

procedure TPoCheckerSettings.SaveLangPath;
begin
  FConfig.SetDeleteValue(pLangPath + 'Value', FLangPath, '');
end;

procedure TPoCheckerSettings.RemoveUnwantedPaths;
const
  pLoadSettings = 'General/LoadSettings/';
  pChildPoFiles = 'ChildPoFiles/';
  pLastSelected = 'LastSelected/';
begin
  FConfig.DeletePath(pLoadSettings);
  FConfig.DeletePath(pChildPoFiles);
  FConfig.DeletePath(pLastSelected);
end;


procedure TPoCheckerSettings.SetMasterPoList(AValue: TStrings);
begin
  FMasterPoList.Assign(AValue);
end;

procedure TPoCheckerSettings.SetMasterPoSelList(AValue: TStrings);
begin
  FMasterPoSelList.Assign(AValue);
end;



procedure TPoCheckerSettings.ResetAllProperties;
begin
  FTestTypes := [];
  FMainFormGeometry := DefaultRect;
  FGraphFormGeometry := DefaultRect;
  FResultsFormGeometry := DefaultRect;
  FMainFormWindowState := wsNormal;
  FResultsFormWindowState := wsNormal;
  FGraphFormWindowState := wsNormal;
  FDisableAntialiasing := DEFAULT_DISABLE_ANTIALIASING;
  FExternalEditorName := '';
  FSelectDirectoryFilename := '';
  FLangFilterLanguageAbbr := '';
  if Assigned(FMasterPoList) then FMasterPoList.Free;
  if Assigned(FMasterPoSelList) then FMasterPoSelList.Free;
  FMasterPoList := TStringList.Create;
  FMasterPoSelList := TStringList.Create;
  FMasterPoList.Sorted := True;
  FMasterPoList.Duplicates := dupIgnore;
end;


constructor TPoCheckerSettings.Create;
begin
  try
    ResetAllProperties;
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
  FMasterPoSelList.Free;
  inherited Destroy;
end;

procedure TPoCheckerSettings.LoadConfig;
begin
  try
    FTestTypes := LoadTestTypes;
    FSelectDirectoryFilename := LoadSelectDirectoryFilename;
    FExternalEditorName := LoadExternalEditorName;
    FLangFilterLanguageAbbr := LoadLangFilterLanguageAbbr;
    FLangPath := LoadLangPath;
    LoadWindowsGeometry;
    LoadDisableAntialiasing;
    LoadMasterPoList(FMasterPoList);
    LoadMasterPoSelList(FMasterPoSelList);
  except
    ResetAllProperties;
    debugln('TPoCheckerSettings.LoadConfig: Error loading config.');
  end;
end;

procedure TPoCheckerSettings.SaveConfig;
begin
  try
    FConfig.SetDeleteValue('Version','1.0','');
    RemoveUnwantedPaths;
    SaveTestTypes;
    SaveExternalEditorName;
    SaveSelectDirectoryFilename;
    SaveLangFilterLanguageAbbr;
    SaveLangPath;
    SaveWindowsGeometry;
    SaveDisableAntialiasing;
    SaveMasterPoList;
    SaveMasterPoSelList;
    FConfig.WriteToDisk;
  except
    debugln('TPoCheckerSettings.SaveConfig: Error saving config.');
  end;
end;

Initialization
  DefaultRect := Rect(-1, -1, -1, -1);
end.

