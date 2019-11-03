unit EMScriptMacro;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SrcEditorIntf, IDEMsgIntf, LazIDEIntf, IDEOptionsIntf,
  IDEExternToolIntf,
  Controls, SynEdit, EMScriptClasses, EMSStrings, Laz2_XMLCfg, uPSRuntime,
  uPSUtils, LazLoggerBase, LazFileUtils;

{$if defined(cpupowerpc)}
  {$ifNdef darwin}  {$DEFINE PasScriptNotAvail } {$ifend}
  {$ifNdef cpu32}  {$DEFINE PasScriptNotAvail } {$ifend}
{$ifend}
{$if defined(cpusparc) }  {$DEFINE PasScriptNotAvail } {$ifend}

const
  EMSSupported = {$IFDEF PasScriptNotAvail} False {$ELSE} True {$ENDIF} ;

type

  { TEMSEditorMacro }

  TEMSEditorMacro = class(TEditorMacro)
  private
    FMacroName: String;
    FSource: String;
    FState: TEditorMacroState;
    FHasError: Boolean;
    FErrorMsg: String;
    FKeyBinding: TEditorMacroKeyBinding;
    FPrivateCompiler: TEMSPSPascalCompiler;
    FPrivateExec: TEMSTPSExec;
    function GetCompiler: TEMSPSPascalCompiler;
    function GetExec: TEMSTPSExec;
    procedure SetCompiler(AValue: TEMSPSPascalCompiler);
    procedure SetExec(AValue: TEMSTPSExec);
  protected
    function  GetMacroName: String; override;
    procedure SetMacroName(AValue: string); override;
    function  GetState: TEditorMacroState; override;
    function  GetErrorMsg: String; override;
    function  GetKeyBinding: TEditorMacroKeyBinding; override;

    procedure DoRecordMacro({%H-}aEditor: TWinControl); override;
    procedure DoPlaybackMacro(aEditor: TWinControl); override;
    procedure DoStop; override;
    procedure DoPause; override;
    procedure DoResume; override;

    procedure Compile;
    procedure FixBeginEnd;
    property Compiler: TEMSPSPascalCompiler read GetCompiler write SetCompiler;
    property Exec: TEMSTPSExec read GetExec write SetExec;
  public
    constructor Create({%H-}aOwner: TComponent); override;
    destructor Destroy; override;
    procedure AssignEventsFrom(AMacroRecorder: TEditorMacro); override;

    procedure Clear; override;

    function  GetAsSource: String; override;
    procedure SetFromSource(const AText: String); override;
    procedure WriteToXmlConf(AConf: TXMLConfig; const APath: String); override;
    procedure ReadFromXmlConf(AConf: TXMLConfig; const APath: String); override;

    function  IsEmpty: Boolean; override;
    function  IsInvalid: Boolean; override;
    function  IsRecording({%H-}AnEditor: TWinControl): Boolean; override;

    property PrivateCompiler: TEMSPSPascalCompiler read FPrivateCompiler write FPrivateCompiler;
    property PrivateExec: TEMSTPSExec read FPrivateExec write FPrivateExec;
  end;

  { TEMSConfig }

  TEMSConfig = class(TAbstractIDEEnvironmentOptions)
  private
    FSelfTestActive: Boolean;
    FSelfTestError: String;
    FSelfTestFailed: Integer; // stores EMSVersion that failed
  protected
    function GetXmlConf(CleanOnError: Boolean = False): TRttiXMLConfig;
  public
    constructor Create;
    procedure Init;
    procedure Load;
    procedure Save;

    class function GetGroupCaption: string; override;
    class function GetInstance: TAbstractIDEOptions; override;
  published
    property SelfTestActive: Boolean read FSelfTestActive write FSelfTestActive;
    property SelfTestFailed: Integer read FSelfTestFailed write FSelfTestFailed;
    property SelfTestError: String read FSelfTestError write FSelfTestError;
  end;

function GetEMSConf: TEMSConfig;

const
  EMSVersion = 2;

implementation

var
  GlobalCompiler: TEMSPSPascalCompiler;
  GlobalExec: TEMSTPSExec;
  ConfFile: TEMSConfig = nil;
  ConfFileName: String = '';

const
  DefaultConfFileName = 'editormacroscript.xml';

function GetConfFileName: String;
begin
  Result := ConfFileName;
  if Result <> '' then
    exit;
  ConfFileName := AppendPathDelim(LazarusIDE.GetPrimaryConfigPath) + DefaultConfFileName;
  LazarusIDE.CopySecondaryConfigFile(DefaultConfFileName);
  Result := ConfFileName;
end;

{ Create global objects }

procedure CreateCompiler;
begin
  GlobalCompiler := TEMSPSPascalCompiler.Create;
end;

procedure CreateExec;
begin
  GlobalExec := TEMSTPSExec.Create;
end;

function GetEMSConf: TEMSConfig;
begin
  Result := ConfFile;
  if Result <> nil then
    exit;

  ConfFile := TEMSConfig.Create;
  Result := ConfFile;
end;

{ TEMSConfig }

function TEMSConfig.GetXmlConf(CleanOnError: Boolean): TRttiXMLConfig;
var
  fn: String;
begin
  fn := GetConfFileName;
  if (not FileExistsUTF8(fn)) then begin
    Result := TRttiXMLConfig.CreateClean(fn)
  end
  else begin
    if CleanOnError then
      try
        Result := TRttiXMLConfig.Create(fn);
      except
        Result := TRttiXMLConfig.CreateClean(fn)
      end
    else
      Result := TRttiXMLConfig.Create(fn);
  end;
end;

constructor TEMSConfig.Create;
begin
  Init;
end;

procedure TEMSConfig.Init;
begin
  FSelfTestActive := False;
  SelfTestFailed := 0;
end;

procedure TEMSConfig.Load;
var
  def: TEMSConfig;
  cfg: TRttiXMLConfig;
begin
  cfg := GetXmlConf;
  def := TEMSConfig.Create;
  try
    cfg.ReadObject('EMS/Settings/', Self, def);
  finally
    cfg.Free;
    def.Free;
  end;
end;

procedure TEMSConfig.Save;
var
  def: TEMSConfig;
  cfg: TRttiXMLConfig;
begin
  cfg := GetXmlConf(True);
  def := TEMSConfig.Create;
  try
    cfg.WriteObject('EMS/Settings/', Self, def);
    cfg.Flush;
  finally
    cfg.Free;
    def.Free;
  end;
end;

class function TEMSConfig.GetGroupCaption: string;
begin
  Result := EMSEditorMacroTitle;
end;

class function TEMSConfig.GetInstance: TAbstractIDEOptions;
begin
  Result := GetEMSConf;
end;

{ TEMSEditorMacro }

function TEMSEditorMacro.GetCompiler: TEMSPSPascalCompiler;
begin
  Result := FPrivateCompiler;
  if Result = nil then
    Result := GlobalCompiler;
end;

function TEMSEditorMacro.GetExec: TEMSTPSExec;
begin
  Result := FPrivateExec;
  if Result = nil then
    Result := GlobalExec;
end;

procedure TEMSEditorMacro.SetCompiler(AValue: TEMSPSPascalCompiler);
begin
  FreeAndNil(FPrivateCompiler);
  FPrivateCompiler := AValue;
end;

procedure TEMSEditorMacro.SetExec(AValue: TEMSTPSExec);
begin
  FreeAndNil(FPrivateExec);
  FPrivateExec := AValue;
end;

function TEMSEditorMacro.GetMacroName: String;
begin
  Result := FMacroName;
end;

procedure TEMSEditorMacro.SetMacroName(AValue: string);
begin
  FMacroName := AValue;
  DoChanged;
end;

function TEMSEditorMacro.GetState: TEditorMacroState;
begin
  Result := FState;
end;

function TEMSEditorMacro.GetErrorMsg: String;
begin
  Result := FErrorMsg;
end;

function TEMSEditorMacro.GetKeyBinding: TEditorMacroKeyBinding;
begin
  if FKeyBinding = nil then
    FKeyBinding := GetDefaultKeyBinding;
  Result := FKeyBinding;
end;

procedure TEMSEditorMacro.DoRecordMacro(aEditor: TWinControl);
begin
  // Not supported
end;

procedure TEMSEditorMacro.DoPlaybackMacro(aEditor: TWinControl);
var
  s, s2: tbtString;
  ExObj: TObject;
  i, x, y: Cardinal;
begin
  if IsEmpty or IsInvalid then exit;

  FState := emPlaying;
  if Assigned(OnStateChange) then
    OnStateChange(Self);

  try
    Compile;
    if IsInvalid then exit;

    Compiler.GetOutput(s{%H-});
    if not Exec.LoadData(s) then // Load the data from the Data string.
      exit;
    Compiler.GetDebugOutput(s2{%H-});
    Exec.LoadDebugData(s2);

    Exec.SynEdit := aEditor as TCustomSynEdit;
    try
      Exec.RunScript;
    except
      on e: Exception do
        IDEMessagesWindow.AddCustomMessage(mluError,Format('%s: %s', [e.ClassName, e.Message]));
    end;
    if Exec.ExceptionCode <> erNoError then begin
      ExObj := Exec.ExceptionObject;
      if ExObj <> nil then
        s := ExObj.ClassName
      else
        s := '<nil>';
      s2 := '';
      i := 0;
      x := 0;
      y := 0;
      Exec.TranslatePositionEx(Exec.ExceptionProcNo, Exec.ExceptionPos, i, x, y, s2);
      if IDEMessagesWindow <> nil then
        IDEMessagesWindow.AddCustomMessage(mluError,Format('%s: "%s" at %d/%d', [s, Exec.ExceptionString, x,y]));
    end;

  finally
    FState := emStopped;
    if Assigned(OnStateChange) then
      OnStateChange(Self);
  end;
end;

procedure TEMSEditorMacro.DoStop;
begin

end;

procedure TEMSEditorMacro.DoPause;
begin
  // Not supported
end;

procedure TEMSEditorMacro.DoResume;
begin
  // Not supported
end;

procedure TEMSEditorMacro.Compile;
var
  i: Integer;
begin
  FHasError := False;
  FErrorMsg := '';
  Compiler.Clear;


  if not Compiler.Compile(FSource) then
  begin
    for i := 0 to Compiler.MsgCount -1 do
      FErrorMsg := FErrorMsg + Compiler.Msg[i].MessageToString + LineEnding;
    FHasError := True;
  end;
end;

procedure TEMSEditorMacro.FixBeginEnd;
begin
  if (not IsEmpty) and (not IsInvalid) then
  begin
    if (pos('begin', FSource) < 1) or (pos('end.', FSource) < 1) then
      FSource := 'begin' + LineEnding + FSource + 'end.'+LineEnding;
  end;
end;

constructor TEMSEditorMacro.Create(aOwner: TComponent);
begin
  FState := emStopped;
end;

destructor TEMSEditorMacro.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FKeyBinding);
  FreeAndNil(FPrivateExec);
  FreeAndNil(FPrivateCompiler);
end;

procedure TEMSEditorMacro.AssignEventsFrom(AMacroRecorder: TEditorMacro);
begin
  FHasError := False;
  if AMacroRecorder = nil then
    Clear
  else
    FSource := AMacroRecorder.GetAsSource;

  if not(AMacroRecorder is TEMSEditorMacro) then
    FixBeginEnd;
  Compile;
  DoChanged;
end;

procedure TEMSEditorMacro.Clear;
begin
  FSource := '';
  FHasError := False;
  DoChanged;
end;

function TEMSEditorMacro.GetAsSource: String;
begin
  Result := FSource;
end;

procedure TEMSEditorMacro.SetFromSource(const AText: String);
begin
  FSource := AText;
  Compile;
  DoChanged;
end;

procedure TEMSEditorMacro.WriteToXmlConf(AConf: TXMLConfig; const APath: String);
begin
  AConf.SetValue(APath + 'Name', MacroName);
  AConf.SetValue(APath + 'Code/Value', GetAsSource);

  if (KeyBinding <> nil) then
    KeyBinding.WriteToXmlConf(AConf, APath);
end;

procedure TEMSEditorMacro.ReadFromXmlConf(AConf: TXMLConfig; const APath: String);
var
  s: String;
begin
  s := AConf.GetValue(APath + 'Code/Value', '');
  FSource := s;
  FixBeginEnd;
  Compile;
  s := AConf.GetValue(APath + 'Name', '');
  if s <> '' then MacroName := s;

  if (not FHasError) and (IsEmpty) then begin
    FHasError := True;
    FErrorMsg := s;
  end;

  if (KeyBinding <> nil) then
    KeyBinding.ReadFromXmlConf(AConf, APath);

  DoChanged;
end;

function TEMSEditorMacro.IsEmpty: Boolean;
begin
  Result := FSource = '';
end;

function TEMSEditorMacro.IsInvalid: Boolean;
begin
  Result := FHasError;
end;

function TEMSEditorMacro.IsRecording(AnEditor: TWinControl): Boolean;
begin
  Result := False;
end;

initialization
  CreateCompiler;
  CreateExec;

finalization
  FreeAndNil(GlobalExec);
  FreeAndNil(GlobalCompiler);
  FreeAndNil(ConfFile);

end.

