unit delphitool;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, System.UITypes, dialogs,
  ProjectIntf, CompOptsIntf, IDEExternToolIntf, MacroDefIntf, MacroIntf, RegExpr,
  strdelphitool;


Const
  SubToolDelphiPriority = SubToolFPCPriority-10;

type

  { TWinePathConverter }

  TWinePathConverter = Class
  Private
    FMap : TStrings;
  Protected
    Procedure FillMap; virtual;
  Public
    Constructor Create;
    Destructor Destroy; override;
    Function UnixToWindows(Const aFileName : String) : String; virtual;
    function WindowsToUnix(Const aFileName : String) : String; virtual;
  end;

  { TDelphiCompilerParser }

  TDelphiCompilerParser = class(TExtToolParser)
  private
  protected
    FRegExprFilenameLineIDMsg: TRegExpr;
    FRegExprFilenameLineUrgencyIDMsg: TRegExpr;
    FConverter : TWinePathConverter;
    Function CreateConverter : TWinePathConverter;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReadLine(Line: string; OutputIndex: integer; IsStdErr: boolean;
      var Handled: boolean); override; // (worker thread)
    class function DefaultSubTool: string; override;
    class function GetParserName: string; override;
    class function GetLocalizedParserName: string; override;
    class function Priority: integer; override;
  end;
  TDelphiCompilerParserClass = class of TDelphiCompilerParser;

  { TDelphiTool }
  TDelphiMacro = (dmCompiler,dmConfigFileName,dmAdditionalArgs,dmCompileCommand);
  TDelphiTool = class
  private
    class var _instance: TDelphiTool;
  Private
    FMacros : Array[TDelphiMacro] of TTransferMacro;
  Protected
    function GetCompileCommand(const s: string; const Data: PtrInt; var Abort: boolean): string; virtual;
    function GetCompilerArgs(const s: string; const Data: PtrInt; var Abort: boolean): string; virtual;
    function GetCompilerPath(const s: string; const Data: PtrInt; var Abort: boolean): string; virtual;
    function GetConfigPath(const s: string; const Data: PtrInt; var Abort: boolean): string; virtual;
    function OnProjectBuilding(Sender: TObject): TModalResult; virtual;
    function FPCToDelphiOpts(Opts: TLazCompilerOptions; aDelphiOpts: TStrings): Integer; virtual;
    function GenerateConfigFilename(const aFilename: String): Boolean; virtual;
  Public
    Destructor Destroy; override;
    Class Constructor Init;
    Class Destructor Done;
    Procedure Hook; virtual;
    Procedure UnHook; virtual;
    Function GetParsedCompilerFilename : String;
    Function GetCurrentConfigFileName(PrependAt: Boolean = true) : String;
    Function GetCompilerArguments: string;
    Function GetCompileCommand: string;
    Class Property Instance: TDelphiTool read _Instance;
  end;

  { TLazProjectDelphiOptions }

  TLazProjectDelphiOptions = class helper for TLazProject
  private
    function GetADO: String;
    function GetDCF: Boolean;
    procedure SetADO(AValue: String);
    procedure SetDCF(AValue: Boolean);
  Public
    Property GenerateDelphiConfigFile : Boolean Read GetDCF Write SetDCF;
    Property AdditionalDelphiOptions : String Read GetADO Write SetADO;
  end;

implementation

uses
  {$IFDEF UNIX} baseunix, {$ENDIF}
  LazIDEIntf, LazLoggerBase, LazUtilities,  delphioptions, fileutil, lazfileutils;

{ TWinePathConverter }

{$IFDEF UNIX}
procedure TWinePathConverter.FillMap;

Var
  DriveLink,DevicesDir : String;
  drive : Char;

begin

  DevicesDir:=GetUserDir+'.wine/dosdevices/';
  if not DirectoryExists(DevicesDir,True) then
    exit;
  for drive:='a' to 'z' do
    begin
    DriveLink:=DevicesDir+Drive+':';
    if DirectoryExists(DriveLink) then
      FMap.Add(Drive+':='+IncludeTrailingPathDelimiter(fpReadLink(DriveLink)));
    end;
end;
{$ELSE}
procedure TWinePathConverter.FillMap;
begin
  // Do nothing
end;
{$ENDIF}

constructor TWinePathConverter.Create;
begin
  FMap:=TStringList.Create;
end;

destructor TWinePathConverter.Destroy;
begin
  FreeAndNil(FMap);
  inherited Destroy;
end;

function TWinePathConverter.UnixToWindows(const aFileName: String): String;

  function SubDirOf(const aBaseDir,aDestDir : string) : Boolean;

  Var
    relPath : String;

  begin
    relPath:=ExtractRelativePath(aBaseDir,aFileName);
    Result:=Copy(relPath,1,3)<>'../';
  end;

  function FileBelowDir(const aBaseDir,aDestFile : String) : Boolean;

  Var
    relPath : String;

  begin
    relPath:=ExtractRelativePath(aBaseDir,aDestFile);
    Result:=Copy(relPath,1,3)<>'../';
  end;


Var
  relPath,Drive,DriveDir,CurDrive,CurDir : String;
  i : Integer;

begin
  if FMap.Count=0 then
    FillMap;
  CurDrive:='';
  CurDir:='';
  for I:=0 to FMap.Count-1 do
    begin
    FMap.GetNameValue(I,Drive,DriveDir);
    if FileBelowDir(DriveDir,aFileName) then
      begin
      if (CurDir='') or (SubDirOf(CurDir,DriveDir)) then
        begin
        CurDrive:=Drive;
        CurDir:=DriveDir
        end;
      end;
    end;
  if CurDrive<>'' then
    begin
    relPath:=ExtractRelativePath(DriveDir,aFileName);
    Result:=UpperCase(CurDrive)+StringReplace(relPath,'/','\',[rfReplaceAll]);
    end
  else
    Result:=StringReplace(relPath,'/','\',[rfReplaceAll]);
end;

function TWinePathConverter.WindowsToUnix(const aFileName: String): String;

var
  S : String;

begin
  Result:='';
  if aFileName='' then exit;
  if (Length(aFileName)<2) or (aFileName[2]<>':') then
    Exit(aFileName);
  if FMap.Count=0 then
    FillMap;
  S:=LowerCase(aFileName[1]+':');
  S:=FMap.Values[S];
  if S<>'' then
    Result:=S+SetDirSeparators(Copy(aFileName,4))
  else
    Result:=SetDirSeparators(aFileName);
  Result:=StringReplace(Result,'\','/',[rfReplaceAll]);
end;

{ TDelphiCompilerParser }

function TDelphiCompilerParser.CreateConverter: TWinePathConverter;
begin
  Result:=TWinePathConverter.Create;
end;

constructor TDelphiCompilerParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConverter:=CreateConverter;

  // filename(linenumber): E2003 Undeclared identifier: 'foo'
  FRegExprFilenameLineIDMsg:=TRegExpr.Create;
  FRegExprFilenameLineIDMsg.ModifierStr:='I';
  FRegExprFilenameLineIDMsg.Expression:='^(.*)\(([0-9]+)\): ([HNWEF])([0-9]+) (.*)$';

  // filename(linenumber): Fatal: F2613 Unit 'Unit3' not found.
  FRegExprFilenameLineUrgencyIDMsg:=TRegExpr.Create;
  FRegExprFilenameLineUrgencyIDMsg.ModifierStr:='I';
  FRegExprFilenameLineUrgencyIDMsg.Expression:='^(.*)\(([0-9]+)\) ([a-zA-Z]+): ([HNWEF])([0-9]+) (.*)$';
end;

destructor TDelphiCompilerParser.Destroy;
begin
  FreeAndNil(FRegExprFilenameLineIDMsg);
  FreeAndNil(FRegExprFilenameLineUrgencyIDMsg);
  FreeAndNil(FConverter);
  inherited Destroy;
end;

procedure TDelphiCompilerParser.ReadLine(Line: string; OutputIndex: integer;
  IsStdErr: boolean; var Handled: boolean);

  procedure Add(const aFilename, LineNoStr, UrgencyLetter, IDStr, MsgStr: String);
  var
    MsgLine: TMessageLine;
  begin
    MsgLine:=CreateMsgLine(OutputIndex);
    case UrgencyLetter of
    'H': MsgLine.Urgency:=mluHint;
    'N': MsgLine.Urgency:=mluNote;
    'W': MsgLine.Urgency:=mluWarning;
    'E': MsgLine.Urgency:=mluError;
    'F': MsgLine.Urgency:=mluFatal;
    else MsgLine.Urgency:=mluImportant;
    end;
    if DelphiToolOptions.ConvertPathsToUnix then
      MsgLine.Filename:=FConverter.WindowsToUnix(aFilename)
    else
      MsgLine.Filename:=aFilename;
    MsgLine.Line:=StrToIntDef(LineNoStr,0);
    MsgLine.MsgID:=StrToIntDef(IDStr,0);
    MsgLine.Msg:=MsgStr;
    if IsStdErr then
      MsgLine.Flags:=MsgLine.Flags+[mlfStdErr];
    AddMsgLine(MsgLine);
  end;

  procedure AddFilenameLineIDMsg;
  var
    RE: TRegExpr;
    aFilename, LineNoStr, UrgencyLetter, IDStr, MsgStr: String;
  begin
    RE:=FRegExprFilenameLineIDMsg;
    aFilename:=RE.Match[1];
    LineNoStr:=RE.Match[2];
    UrgencyLetter:=RE.Match[3];
    IDStr:=RE.Match[4];
    MsgStr:=RE.Match[5];
    Add(aFilename,LineNoStr,UrgencyLetter,IDStr,MsgStr);
  end;

  procedure AddFilenameLineUrgencyIDMsg;
  var
    RE: TRegExpr;
    aFilename, LineNoStr, UrgencyLetter, IDStr, MsgStr: String;
  begin
    RE:=FRegExprFilenameLineUrgencyIDMsg;
    aFilename:=RE.Match[1];
    LineNoStr:=RE.Match[2];
    //UrgencyStr:=RE.Match[3];
    UrgencyLetter:=RE.Match[4];
    IDStr:=RE.Match[5];
    MsgStr:=RE.Match[6];
    Add(aFilename,LineNoStr,UrgencyLetter,IDStr,MsgStr);
  end;

  procedure AddOtherLine;
  var
    MsgLine: TMessageLine;
  begin
    MsgLine:=CreateMsgLine(OutputIndex);
    MsgLine.MsgID:=0;
    MsgLine.SubTool:=SSubToolDelphi;
    if MsgLine.Msg<>'' then
      MsgLine.Urgency:=mluImportant
    else
      MsgLine.Urgency:=mluVerbose2;
    if IsStdErr then
      MsgLine.Flags:=MsgLine.Flags+[mlfStdErr];
    AddMsgLine(MsgLine);
  end;

begin
  FRegExprFilenameLineIDMsg.InputString:=Line;
  if FRegExprFilenameLineIDMsg.ExecPos(1) then
  begin
    AddFilenameLineIDMsg;
    exit;
  end;

  FRegExprFilenameLineUrgencyIDMsg.InputString:=Line;
  if FRegExprFilenameLineUrgencyIDMsg.ExecPos(1) then
  begin
    AddFilenameLineUrgencyIDMsg;
    exit;
  end;

  AddOtherLine;

  Handled:=true;
end;

class function TDelphiCompilerParser.DefaultSubTool: string;
begin
  Result:=SDelphiToolName;
end;

class function TDelphiCompilerParser.GetParserName: string;
begin
  Result:=SDelphiParserName;
end;

class function TDelphiCompilerParser.GetLocalizedParserName: string;
begin
  Result:=SDelphiLocalizedParserName;
end;


class function TDelphiCompilerParser.Priority: integer;
begin
  Result:=SubToolDelphiPriority;
end;

{ TDelphiTool }

function TDelphiTool.GetCompilerPath(const s: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  Abort:=False;
  if (s<>'') and (ConsoleVerbosity>=0) then
    debugln(['Hint: (lazarus) [TDelphiTool.GetCompilerPath] ignoring macro DCC parameter "',s,'"']);
  Result:=GetParsedCompilerFilename;
  if Result='' then
    Result:='dcc32.exe'; // always return something to get nicer error messages
  debugln(['macro DCC parameter: "',Result,'"']);
end;

function TDelphiTool.GetCompileCommand(const s: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  if (s<>'') and (ConsoleVerbosity>=0) then
    debugln(['Hint: (lazarus) [TDelphiTool.GetCompilerPath] ignoring macro DELPHICOMPILE parameter "',s,'"']);
  Result:=GetCompileCommand();
  // debugln(['macro DELPHICOMPILE parameter: "',Result,'"']);
end;

function TDelphiTool.GetCompilerArgs(const s: string; const Data: PtrInt;
  var Abort: boolean): string;


begin

  if (s<>'') and (ConsoleVerbosity>=0) then
    debugln(['Hint: (lazarus) [TDelphiTool.GetCompilerPath] ignoring macro DCCARGS parameter "',s,'"']);
  Result:=GetCompilerArguments;
  // debugln(['macro DCCARGS parameter: "',Result,'"']);
end;


function TDelphiTool.GetConfigPath(const s: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  Abort:=False;
  if (s<>'') and (ConsoleVerbosity>=0) then
    debugln(['Hint: (lazarus) [TDelphiTool.GetConfigPath] ignoring macro DCCCONFIG parameter "',s,'"']);
  Result:=GetCurrentConfigFilename;
  if (Result='') then
    Result:='Project1'+DelphiOptions.DefaultConfigExtension;
  // debugln(['macro DCCCONFIG parameter: "',Result,'"']);
end;

destructor TDelphiTool.Destroy;
begin
  inherited Destroy;
end;

class constructor TDelphiTool.Init;
begin
  _instance:=TDelphiTool.Create;
end;

class destructor TDelphiTool.Done;
begin
  FreeAndNil(_instance);
end;

function TDelphiTool.FPCToDelphiOpts(Opts : TLazCompilerOptions; aDelphiOpts: TStrings) : Integer;

  Procedure AddFlag(SetFlag : Boolean; const FlagName : String);

  begin
    aDelphiOpts.Add('-$'+FlagName+PlusMinus[SetFlag]);
    Inc(Result);
  end;

  procedure AddOption(DoAdd : Boolean; const Option : String);

  begin
    if DoAdd then
      begin
      aDelphiOpts.Add('-'+Option);
      Inc(Result);
      end;
  end;

  procedure AddOption(aValue : String; const FlagName : String);

  begin
    IDEMacros.SubstituteMacros(aValue);
    AddOption(aValue<>'',FlagName+aValue);
  end;

begin
  // Options
  AddOption(Opts.GetUnitPath(False,coptParsed,True),'U');
  AddOPtion(Opts.GetIncludePath(False,coptParsed,True),'I');
  AddOption(Opts.GenerateDebugInfo,'V');
  AddOption(Opts.GetUnitOutputDirectory(False),'NU');
  AddOption(Opts.GetObjectPath(false,coptParsed,True),'O');
  AddOption(Opts.ShowWarn,'W');
  AddOption(Opts.ShowHints,'H');
  AddOption(Opts.DontUseConfigFile,'-no-config');
  AddOption(Opts.TargetFileExt,'TX');
  // Flags
  AddFlag(Opts.RangeChecks,'R');
  AddFlag(Opts.IOChecks,'I');
  AddFlag(Opts.IncludeAssertionCode,'C');
  AddFlag(Opts.UseAnsiStrings,'H');
  AddFlag(Opts.OptimizationLevel>0,'O');
  AddFlag(Opts.OverflowChecks,'Q');
  AddFlag(Opts.GenerateDebugInfo,'L');
  AddFlag(Opts.GenerateDebugInfo,'W');
  AddFlag(Opts.GenerateDebugInfo,'Y');
  // We can maybe check for -Sy in custom options ?
end;

function TDelphiTool.GenerateConfigFilename(const aFilename : String) : Boolean;

var
  Opts : TLazCompilerOptions;
  BuildID: String;
  Idx : Integer;
  L : TStrings;

begin
  debugln(['Generating delphi project configuration file: "',aFileName,'"']);
  BuildID:=LazarusIDE.ActiveProject.ActiveBuildModeID;
  Idx:=LazarusIDE.ActiveProject.LazBuildModes.IndexOf(BuildID);
  if Idx<0 then
    Idx:=0;
  Opts:=LazarusIDE.ActiveProject.LazBuildModes.BuildModes[Idx].LazCompilerOptions;
  L:=TstringList.Create;
  try
    Result:=FPCToDelphiOpts(Opts,L)>0;
    L.SaveToFile(aFileName);
    debugln(['Generated delphi project configuration file: "',aFileName,'"']);
  finally
    L.Free;
  end;
end;

function TDelphiTool.OnProjectBuilding(Sender: TObject): TModalResult;

begin
  Result:=mrOK;
  FMacros[dmCompiler].LazbuildValue:=GetCompileCommand();
  FMacros[dmConfigFileName].LazbuildValue:=GetCurrentConfigFileName;
  FMacros[dmCompileCommand].LazbuildValue:=GetCompileCommand;
  FMacros[dmAdditionalArgs].LazbuildValue:=GetCompilerArguments;
  if Assigned(LazarusIDE.ActiveProject) and LazarusIDE.ActiveProject.GenerateDelphiConfigFile then
    GenerateConfigFilename(GetCurrentConfigFileName(False));
end;

procedure TDelphiTool.Hook;
begin
  FMacros[dmCompiler]:=IDEMacros.Add('DCC', '', SDelphiCompilerFileNameCaption, @GetCompilerPath, [tmfLazbuild]);
  FMacros[dmConfigFileName]:=IDEMacros.Add('DCCCONFIG', '', SDelphiCompilerConfigFileName, @GetConfigPath, [tmfLazbuild]);
  FMacros[dmCompileCommand]:=IDEMacros.Add('DELPHICOMPILE', '', SDelphiCompileCommand, @GetCompileCommand, [tmfLazbuild]);
  FMacros[dmAdditionalArgs]:=IDEMacros.Add('DCCARGS', '', SDelphiCompilerArgs, @GetCompilerArgs, [tmfLazbuild]);
  LazarusIDE.AddHandlerOnProjectBuilding(@OnProjectBuilding);
end;

procedure TDelphiTool.UnHook;
begin
  //
end;

function TDelphiTool.GetParsedCompilerFilename: String;

begin
  Result:=DelphiToolOptions.CompilerFileName;
  IDEMacros.SubstituteMacros(Result);
  if not FilenameIsAbsolute(Result) then
    Result:=FindDefaultExecutablePath(Result);
end;

function TDelphiTool.GetCurrentConfigFileName(PrependAt: Boolean = True): String;
begin
  if Assigned(LazarusIDE.ActiveProject) and LazarusIDE.ActiveProject.GenerateDelphiConfigFile then
    begin
    Result:=ChangeFileExt(LazarusIDE.ActiveProject.ProjectInfoFile,DelphiOptions.DefaultConfigExtension);
    if PrependAt then
      Result:='@'+Result;
    end
  else
    Result:='';
end;

function TDelphiTool.GetCompilerArguments: string;

var
  S : String;

begin
  Result:=DelphiToolOptions.AdditionalOptions;
  IDEMacros.SubstituteMacros(Result);
  if Assigned(LazarusIDE.ActiveProject) then
    begin
    S:=LazarusIDE.ActiveProject.AdditionalDelphiOptions;
    if S<>'' then
      IDEMacros.SubstituteMacros(S);
    if S<>'' then
      Result:=Result+' ';
    end;
end;


function TDelphiTool.GetCompileCommand: string;

begin
  Result:='$(DCC) $(DCCARGS) $(DCCCONFIG)';
  IDEMacros.SubstituteMacros(Result);
end;

{ TLazProjectDelphiOptions }

function TLazProjectDelphiOptions.GetADO: String;
begin
  Result:=CustomData[pKeyAdditionalOptions];
end;

function TLazProjectDelphiOptions.GetDCF: Boolean;
begin
  Result:=CustomData[pKeyGenConfigFile]='1';
end;

procedure TLazProjectDelphiOptions.SetADO(AValue: String);
begin
  CustomData[pKeyAdditionalOptions]:=aValue
end;

procedure TLazProjectDelphiOptions.SetDCF(AValue: Boolean);
begin
  CustomData[pKeyGenConfigFile]:=IntToStr(Ord(aValue));
end;


end.

