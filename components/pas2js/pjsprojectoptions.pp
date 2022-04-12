unit PJSProjectOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  // LCL
  StdCtrls, Spin,
  // LazUtils
  LazLoggerBase, LazFileUtils, LazUTF8,
  // IdeIntf
  LazIDEIntf, ProjectIntf, CompOptsIntf, IDEOptionsIntf, IDEOptEditorIntf,
  // pas2js
  PJSDsgnOptions, PJSController, StrPas2JSDesign;

type

  { TPas2JSProjectOptionsFrame }

  TPas2JSProjectOptionsFrame = class(TAbstractIDEOptionsEditor)
    BMakePas2jsPoject: TButton;
    BResetRunCommand: TButton;
    BResetCompileCommand: TButton;
    CBRunOnReady: TCheckBox;
    CBServerURL: TComboBox;
    CBUseBrowserConsole: TCheckBox;
    CBWebProject: TCheckBox;
    CBHTMLFile: TComboBox;
    CBMaintainHTMLFile: TCheckBox;
    LCBProjectHTMLFile: TLabel;
    RBRunDefault: TRadioButton;
    RBStartServerAt: TRadioButton;
    RBUseURL: TRadioButton;
    RunGroupBox: TGroupBox;
    SEPort: TSpinEdit;
    procedure BMakePas2jsPojectClick(Sender: TObject);
    procedure BResetCompileCommandClick(Sender: TObject);
    procedure BResetRunCommandClick(Sender: TObject);
    procedure CBMaintainHTMLFileChange(Sender: TObject);
    procedure CBWebProjectChange(Sender: TObject);
    procedure RBStartServerAtChange(Sender: TObject);
    procedure RBUseURLChange(Sender: TObject);
  private
    procedure CheckAllControls(aEnabled: Boolean);
    procedure UpdateHTMLControls;
    function FillFilesCombo(PRJ: TLazProject): Integer;
    procedure UpdateRunControls;
  public
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

Procedure SetDefaultWebRunParams(RunParams : TAbstractRunParamsOptionsMode);
Procedure SetDefaultServiceWorkerRunParams(RunParams : TAbstractRunParamsOptionsMode);
Procedure SetDefaultNodeRunParams(RunParams : TAbstractRunParamsOptionsMode);
Procedure SetDefaultWebCompileOptions(CompOpts: TLazCompilerOptions);
Procedure SetDefaultServiceWorkerCompileOptions(CompOpts: TLazCompilerOptions);
Procedure SetDefaultNodeJSCompileOptions(CompOpts: TLazCompilerOptions);
Procedure SetDefaultModuleCompileOptions(CompOpts: TLazCompilerOptions);


implementation

{$R *.lfm}

Procedure ResetRunParams(RunParams : TAbstractRunParamsOptionsMode);

begin
  RunParams.HostApplicationFilename:='';
  RunParams.CmdLineParams:='';
  RunParams.UseLaunchingApplication:=True;
end;

Procedure SetDefaultWebRunParams(RunParams : TAbstractRunParamsOptionsMode);

begin
  ResetRunParams(RunParams);
  RunParams.LaunchingApplicationPathPlusParams:='"$(Pas2JSBrowser)" "$(Pas2JSProjectURL)"';
end;

procedure SetDefaultServiceWorkerRunParams(
  RunParams: TAbstractRunParamsOptionsMode);
begin
  RunParams.HostApplicationFilename:='';
  RunParams.CmdLineParams:='';
  RunParams.UseLaunchingApplication:=False;
end;

Procedure SetDefaultNodeRunParams(RunParams : TAbstractRunParamsOptionsMode);

begin
  ResetRunParams(RunParams);
  RunParams.LaunchingApplicationPathPlusParams:='"$(Pas2JSNodeJS)" "$MakeDir($(ProjPath))$NameOnly($(ProjFile)).js"';
end;

Procedure SetPasJSCompileOptions(CompOpts: TLazCompilerOptions;
  TargetOS, CustomOpts : String);

Var
  Compiler : String;

begin
  //DebugLn(['SetPasJSCompileOptions START']);
  CompOpts.UnitOutputDirectory:='js';

  CompOpts.TargetFileExt:='.js';
  CompOpts.TargetOS:=TargetOS;

  CompOpts.AllowLabel:=false;
  CompOpts.UseAnsiStrings:=false;
  CompOpts.CPPInline:=false;

  CompOpts.IOChecks:=false;
  CompOpts.StackChecks:=false;
  CompOpts.SmartLinkUnit:=false;

  CompOpts.GenerateDebugInfo:=false;
  CompOpts.DebugInfoType:=dsAuto;
  CompOpts.UseLineInfoUnit:=false;
  CompOpts.UseHeaptrc:=false;
  CompOpts.Win32GraphicApp:=false;

  CompOpts.WriteFPCLogo:=true;
  CompOpts.CustomOptions:=CustomOpts;

  Compiler:='$(pas2js)';
  CompOpts.CompilerPath:=Compiler;
  debugln(['Hint: (lazarus) [pjsprojectoptions.SetPasJSCompileOptions] Compiler=',CompOpts.CompilerPath,' TargetOS=',CompOpts.TargetOS,' Custom="',CompOpts.CustomOptions,'"']);
end;

Procedure SetDefaultWebCompileOptions(CompOpts: TLazCompilerOptions);

begin
  SetPasJSCompileOptions(CompOpts,'browser','-Jeutf-8 -Jirtl.js -Jc -Jminclude');
end;

procedure SetDefaultServiceWorkerCompileOptions(CompOpts: TLazCompilerOptions);
begin
  SetPasJSCompileOptions(CompOpts,'browser','-Jeutf-8 -Jirtl.js -Jc -Jminclude');
end;

Procedure SetDefaultNodeJSCompileOptions(CompOpts: TLazCompilerOptions);

begin
  SetPasJSCompileOptions(CompOpts,'nodejs','-Jeutf-8 -Jminclude');
end;

Procedure SetDefaultModuleCompileOptions(CompOpts: TLazCompilerOptions);

begin
  SetPasJSCompileOptions(CompOpts,'module','-Jeutf-8 -Jirtl.js -Jc -Jminclude');
end;

{ TPas2JSProjectOptionsFrame }

function TPas2JSProjectOptionsFrame.GetTitle: string;
begin
  Result:=pjsdWebProjectPas2js;
end;

procedure TPas2JSProjectOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  CBWebProject.Caption:=pjsdProjectIsAWebBrowserPas2jsProject;
  LCBProjectHTMLFile.Caption:=pjsdProjectHTMLPage;
  CBMaintainHTMLFile.Caption:=pjsdMaintainHTMLPage;
  CBUseBrowserConsole.Caption:=pjsdUseBrowserConsoleUnitToDisplayWritelnOutput;
  CBRunOnReady.Caption:=pjsdRunRTLWhenAllPageResourcesAreFullyLoaded;

  RunGroupBox.Caption:=pjsdRun;
  RBStartServerAt.Caption:=pjsdStartHTTPServerOnPort;
  RBUseURL.Caption:=pjsdUseThisURLToStartApplication;
  RBRunDefault.Caption:=pjsExecuteRunParameters;

  BResetRunCommand.Caption:=pjsdResetRunCommand;
  BResetCompileCommand.Caption:=pjsdResetCompileCommand;
  BMakePas2jsPoject.Caption:=pjsMakePas2jsProject;
end;

procedure TPas2JSProjectOptionsFrame.CBWebProjectChange(Sender: TObject);
begin
  CheckAllControls(CBWebProject.Checked);
end;

procedure TPas2JSProjectOptionsFrame.RBStartServerAtChange(Sender: TObject);
begin
  UpdateRunControls;
end;

procedure TPas2JSProjectOptionsFrame.RBUseURLChange(Sender: TObject);
begin
  UpdateRunControls;
end;

procedure TPas2JSProjectOptionsFrame.CBMaintainHTMLFileChange(Sender: TObject);
begin
  UpdateHTMLControls;
end;

procedure TPas2JSProjectOptionsFrame.BResetRunCommandClick(Sender: TObject);

Var
  Prj : TLazProject;

begin
  PRJ:=LazarusIDE.ActiveProject;
  SetDefaultWebRunParams(Prj.RunParameters.GetOrCreate('Default'));
end;

procedure TPas2JSProjectOptionsFrame.BResetCompileCommandClick(Sender: TObject);

Var
  Prj : TLazProject;

begin
  PRJ:=LazarusIDE.ActiveProject;
  SetDefaultWebCompileOptions(PRJ.LazBuildModes.BuildModes[0].LazCompilerOptions);
  SetDefaultWebCompileOptions(PRJ.LazCompilerOptions);
end;

procedure TPas2JSProjectOptionsFrame.BMakePas2jsPojectClick(Sender: TObject);
Var
  Prj : TLazProject;

begin
  PRJ:=LazarusIDE.ActiveProject;
  PRJ.CustomData.Values[PJSProject]:='1';
end;

procedure TPas2JSProjectOptionsFrame.UpdateHTMLControls;
begin
  CBUseBrowserConsole.Enabled:=CBWebProject.Enabled and CBMaintainHTMLFile.Enabled;
  CBRunOnReady.Enabled:=CBUseBrowserConsole.Enabled;
end;

procedure TPas2JSProjectOptionsFrame.CheckAllControls(aEnabled : Boolean);

begin
  CBHTMLFile.Enabled:=aEnabled;

  CBMaintainHTMLFile.Enabled:=aEnabled;
  CBUseBrowserConsole.Enabled:=aEnabled;
  CBRunOnReady.Enabled:=aEnabled;

  RBStartServerAt.Enabled:=aEnabled;
  SEPort.Enabled:=aEnabled and RBStartServerAt.Checked;
  RBUseURL.Enabled:=aEnabled;
  CBServerURL.Enabled:=aEnabled and RBUseURL.Checked;
  RBRunDefault.Enabled:=aEnabled;

  BResetRunCommand.enabled:=aEnabled;
  BResetCompileCommand.Enabled:=aEnabled;
end;

// Fill combo with HTM(L) files.
// Return index of file that has IsProjectHTMLFile set.
function TPas2JSProjectOptionsFrame.FillFilesCombo(PRJ: TLazProject): Integer;

Var
  I : integer;
  HPF,PF : TLazProjectFile;
  L : TStringListUTF8Fast;

begin
  Result:=-1;
  HPF:=Nil;
  L:=TStringListUTF8Fast.Create;
  try
    For I:=0 to PRJ.FileCount-1 do
      begin
      PF:=PRJ.Files[i];
      if FilenameExtIs(PF.Filename,'html') then
        begin
        L.AddObject(PF.FileName,PF);
        If PF.CustomData[PJSIsProjectHTMLFile]='1' then
          HPF:=PF;
        end;
      end;
    L.Sort;
    CBHTMLFile.Items:=L;
  finally
    L.Free;
  end;
  if (HPF<>Nil) then
    Result:=CBHTMLFile.Items.IndexOfObject(HPF);
end;

procedure TPas2JSProjectOptionsFrame.UpdateRunControls;
begin
  SEPort.Enabled:=CBWebProject.Enabled and RBStartServerAt.Enabled and RBStartServerAt.Checked;
  CBServerURL.Enabled:=CBWebProject.Enabled and RBUseURL.Enabled and RBUseURL.Checked;
end;

procedure TPas2JSProjectOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);

Var
  Prj : TLazProject;
  HFN : String;
  HTMLIdx : Integer;
  Port : Word;
  URL : String;

begin
  if AOptions=nil then ;
  Prj:=LazarusIDE.ActiveProject;
  HTMLIdx:=FillFilesCombo(Prj);
  CBWebProject.Checked:=Prj.CustomData[PJSProjectWebBrowser]='1';
  if HTMLIdx=-1 then
    begin
    HFN:=Prj.CustomData[PJSProjectHTMLFile];
    HTMLIdx:=CBHTMLFile.Items.IndexOf(HFN);
    end;
  CBHTMLFile.ItemIndex:=HTMLIdx;

  CBMaintainHTMLFile.Checked:=Prj.CustomData[PJSProjectMaintainHTML]='1';
  CBUseBrowserConsole.Checked:=Prj.CustomData[PJSProjectUseBrowserConsole]='1';
  CBRunOnReady.Checked:=Prj.CustomData[PJSProjectRunAtReady]='1';

  Port:=StrToIntDef(Prj.CustomData[PJSProjectPort],0);
  URL:=Prj.CustomData[PJSProjectURL];
  SEPort.Value:=Min(Max(0,Port),65535);
  CBServerURL.AddHistoryItem(URL,10,True,False);
  if Prj.CustomData.Contains(PJSProjectPort) then
    RBStartServerAt.Checked:=true
  else if Prj.CustomData.Contains(PJSProjectURL) then
    RBUseURL.Checked:=true
  else
    RBRunDefault.Checked:=true;

  UpdateHTMLControls;
  UpdateRunControls;
end;

procedure TPas2JSProjectOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
Var
  Prj : TLazProject;

  Procedure DoBool(N : String; AValue : Boolean);
  begin
    if AValue then
      Prj.CustomData[N]:='1';
  end;

begin
  if AOptions=nil then ;
  Prj:=LazarusIDE.ActiveProject;
  // Clear everything
  With Prj.CustomData do
    begin
    Remove(PJSProject);
    Remove(PJSProjectWebBrowser);
    Remove(PJSProjectHTMLFile);
    Remove(PJSProjectMaintainHTML);
    Remove(PJSProjectUseBrowserConsole);
    Remove(PJSProjectRunAtReady);
    Remove(PJSProjectPort);
    Remove(PJSProjectURL);
    end;
  // Set what is needed
  if CBWebProject.Checked then
    begin
    Prj.CustomData[PJSProject]:='1';
    Prj.CustomData[PJSProjectWebBrowser]:='1';

    With CBHTMLFile do
      if ItemIndex<>-1 then
        (Items.Objects[ItemIndex] as TLazProjectFile).CustomData[PJSIsProjectHTMLFile]:='1';

    DoBool(PJSProjectMaintainHTML,CBMaintainHTMLFile.Checked);
    DoBool(PJSProjectUseBrowserConsole,CBUseBrowserConsole.Checked);
    DoBool(PJSProjectRunAtReady,CBRunOnReady.Checked);

    if RBStartServerAt.Checked and (SEPort.Value>=0) then
      Prj.CustomData[PJSProjectPort]:=IntToStr(SEPort.Value)
    else if RBUseURL.Checked and (CBServerURL.Text<>'') then
      Prj.CustomData[PJSProjectURL]:=CBServerURL.Text;

    end;
end;

class function TPas2JSProjectOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=TAbstractIDEProjectOptions;
end;

end.

